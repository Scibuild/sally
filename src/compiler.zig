const std = @import("std");
const ast = @import("ast.zig");
const VM = @import("interpreter.zig").VM;
const Value = @import("values.zig").Value;
const types = @import("types.zig");
const builtins = @import("builtins.zig");

pub const Opcode = enum(u8) {
    op_return = 0,
    op_call,
    // changes the stack pointer
    op_init_stack_frame,
    // arithmetic operations pop two values off, compute the operation and push the result
    op_add_int,
    op_sub_int,
    op_mul_int,
    op_div_int,
    op_add_float,
    op_sub_float,
    op_mul_float,
    op_div_float,
    // pushes a contant from memory onto the stack
    op_constant,
    op_constant_16,
    // pushes true or false onto the stack
    op_true,
    op_false,
    // negates the top of the stack
    op_negate_int,
    op_negate_float,
    op_not,
    // takes a type and two values from the stack and computes the comparison
    op_cmp_equal,
    op_cmp_greater_int,
    op_cmp_greater_float,
    op_cmp_greater_equal_int,
    op_cmp_greater_equal_float,
    op_str_concat,
    op_get_global,
    // pops value from top of the working stack and stores it in the function stack
    op_set_local,
    // pushes a value from the function stack onto the working stack
    op_get_local,
    // pops a value from the working stack
    op_pop,
    op_swap,

    // subtract the following u16 from the instruction pointer
    op_branch_backward,
    // add the following u16 to the ip
    op_branch,
    // if the top of the stack contains a true, add the following u16 to the ip
    op_branch_if_true,
    // if the top of the stack contains a false, add the following u16 to the ip
    op_branch_if_false,
};

pub const CompiledBytecodeChunk = struct {
    buffer: []const u8,
    constants: []const Value,

    pub fn pprint(self: *@This(), writer: anytype) !void {
        var i: usize = 0;
        while (i < self.buffer.len) {
            const op_code = self.buffer[i];
            try writer.print("{d:0>4}  {}", .{ i, @intToEnum(Opcode, op_code) });
            switch (@intToEnum(Opcode, op_code)) {
                .op_constant => {
                    i += 1;
                    const constant_index = self.buffer[i];
                    try writer.print("             {d: <6} {d}\n", .{ constant_index, self.constants[constant_index] });
                },
                .op_init_stack_frame,
                .op_set_local,
                .op_get_local,
                => {
                    i += 1;
                    const global_index = self.buffer[i];
                    try writer.print("           {d: <6}\n", .{global_index});
                },
                .op_get_global,
                .op_branch,
                .op_branch_if_true,
                .op_branch_if_false,
                .op_branch_backward,
                => {
                    i += 1;
                    const low_byte = @intCast(u16, self.buffer[i]);
                    i += 1;
                    const high_byte = @intCast(u16, self.buffer[i]);
                    try writer.print("           {d: <6}\n", .{(high_byte << 8) | low_byte});
                },
                else => {
                    try writer.writeByte('\n');
                },
            }
            i += 1;
        }
    }
};

pub const BytecodeChunk = struct {
    buffer: std.ArrayListUnmanaged(u8),
    constants: std.ArrayListUnmanaged(Value),
    allocator: std.mem.Allocator,

    pub fn init(self: *@This(), allocator: std.mem.Allocator) !void {
        self.buffer = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 32);
        self.constants = try std.ArrayListUnmanaged(u64).initCapacity(allocator, 16);
        self.allocator = allocator;
    }

    pub fn deinit(self: *@This()) void {
        self.buffer.deinit();
        self.constants.deinit();
    }

    pub fn addOpcode(self: *@This(), code: Opcode) !void {
        try self.buffer.append(self.allocator, @enumToInt(code));
    }
    pub fn addByte(self: *@This(), byte: u8) !void {
        try self.buffer.append(self.allocator, byte);
    }
    pub fn addWord(self: *@This(), word: u16) !void {
        const low: u8 = @intCast(u8, word & 0xff);
        const high: u8 = @intCast(u8, word >> 8);
        try self.buffer.append(self.allocator, low);
        try self.buffer.append(self.allocator, high);
    }

    pub fn addConstant(self: *@This(), constant: u64) !void {
        if (self.constants.items.len > 256) {
            @panic("Unimplemented: more than 256 constants");
        }
        try self.buffer.append(self.allocator, @enumToInt(Opcode.op_constant));
        for (self.constants.items) |c, i| {
            if (c == constant) {
                try self.buffer.append(self.allocator, @intCast(u8, i));
                return;
            }
        }
        try self.constants.append(self.allocator, constant);
        try self.buffer.append(self.allocator, @intCast(u8, self.constants.items.len - 1));
    }

    // returns the location of the two bytes to be overwritten
    pub fn addPlaceholderBranch16(self: *@This()) !usize {
        const location = self.buffer.items.len;
        try self.addByte(undefined);
        try self.addByte(undefined);
        return location;
    }

    // Fills in a placeholder so that it will jump to exactly the instruction after the instruction that was just written
    pub fn fillPlaceholderBranch16(self: *@This(), loc: usize) !void {
        // we subtract the size of a u16 since the place we actually want to jump from will be right after the instruction
        // in the interpreter, it will read the location and put itself two ahead of where it's location is
        //
        // lol no we dont
        const diff = @intCast(u16, self.getLocation() - loc);
        // this is actually not a good idea oops, platform independence is good actually
        // // love this sort of magic, we just cast the location into a 16 bit integer and write straight there
        // @ptrCast(*i16, &self.buffer.items[loc]).* = diff;

        const low_byte = @intCast(u8, diff & 0xff);
        const high_byte = @intCast(u8, (diff >> 8) & 0xff);
        self.buffer.items[loc] = low_byte;
        self.buffer.items[loc + 1] = high_byte;
    }

    pub fn getLocation(self: *@This()) usize {
        return self.buffer.items.len;
    }

    // adds the branch offset to a specific location
    pub fn addBranchBackward16(self: *@This(), loc: usize) !void {
        try self.addOpcode(.op_branch_backward);
        const diff = @intCast(u16, self.getLocation() - loc);
        // this is actually not a good idea oops, platform independence is good actually:
        //
        // // love this sort of magic, we just cast the location into a 16 bit integer and write straight there
        // @ptrCast(*i16, &self.buffer.items[loc]).* = diff;

        const low_byte = @intCast(u8, diff & 0xff);
        const high_byte = @intCast(u8, (diff >> 8) & 0xff);
        try self.addByte(low_byte);
        try self.addByte(high_byte);
    }
};

pub const ProgramCompiler = struct {
    vm: *VM,

    source: [:0]const u8,

    const Self = *@This();

    pub fn compileProgram(self: Self, program: []*ast.StmtNode, chunkAllocator: std.mem.Allocator) VM.AllocError!void {
        for (program) |decl| {
            switch (decl.s) {
                .fn_decl_concrete => |data| {
                    var chunk: BytecodeChunk = undefined;
                    try chunk.init(chunkAllocator);
                    try chunk.addOpcode(.op_init_stack_frame);
                    try chunk.addByte(data.frameSize);
                    var compiler = Compiler{
                        .chunk = &chunk,
                        .vm = self.vm,
                        .source = self.source,
                    };
                    try compiler.compileStmts(data.body);
                    try chunk.addOpcode(.op_return);

                    const compiledChunk = try chunkAllocator.create(CompiledBytecodeChunk);
                    compiledChunk.buffer = chunk.buffer.toOwnedSlice(chunkAllocator);
                    compiledChunk.constants = chunk.constants.toOwnedSlice(chunkAllocator);

                    if (data.id >= self.vm.globals.items.len)
                        try self.vm.globals.resize(self.vm.allocator, data.id + 1);
                    self.vm.globals.items[data.id] = @ptrToInt(compiledChunk);
                },
                else => {},
            }
        }
    }
};

pub const Compiler = struct {
    chunk: *BytecodeChunk,
    vm: *VM,

    source: [:0]const u8,

    const Self = *@This();

    pub fn compileStmts(self: Self, stmts: []*ast.StmtNode) VM.AllocError!void {
        for (stmts) |stmt| {
            try self.compileStmt(stmt);
        }
    }

    pub fn compileStmt(self: Self, stmt: *ast.StmtNode) VM.AllocError!void {
        switch (stmt.s) {
            .semi => {
                try self.chunk.addOpcode(.op_call);
            },
            .local => |id| {
                try self.chunk.addOpcode(.op_get_local);
                try self.chunk.addByte(id);
            },
            .assign_local => |id| {
                try self.chunk.addOpcode(.op_set_local);
                try self.chunk.addByte(id);
            },
            .global => |id| {
                try self.chunk.addOpcode(.op_get_global);
                try self.chunk.addWord(id);
            },
            .global_punct => |id| {
                try self.chunk.addOpcode(.op_get_global);
                try self.chunk.addWord(id);
                try self.chunk.addOpcode(.op_call);
            },
            .builtin => |id| {
                try self.chunk.addConstant(@intCast(u64, @enumToInt(id)) | (1 << 63));
            },
            .builtin_punct => |id| {
                try self.compileBuiltinPunct(id);
            },
            .bool_lit => |val| {
                const op: Opcode = if (val) .op_true else .op_false;
                try self.chunk.addOpcode(op);
            },
            .int_lit => |val| {
                try self.chunk.addConstant(@bitCast(u64, val));
            },
            .float_lit => |val| {
                try self.chunk.addConstant(@bitCast(u64, val));
            },
            .string_lit => |lit| {
                const val = try stringLiteralToValue(lit, self.vm.allocator);
                const vstring = try self.vm.allocZigString(val);
                try self.chunk.addConstant(vstring.toValue());
                self.vm.allocator.free(val);
            },
            .if_stmt => |data| {
                try self.compileStmts(data.condition);
                try self.chunk.addOpcode(.op_branch_if_false);
                const false_branch = try self.chunk.addPlaceholderBranch16();
                try self.compileStmts(data.if_body);

                if (data.else_body.len != 0) {
                    try self.chunk.addOpcode(.op_branch);
                    const true_branch = try self.chunk.addPlaceholderBranch16();
                    try self.chunk.fillPlaceholderBranch16(false_branch);
                    try self.compileStmts(data.else_body);
                    try self.chunk.fillPlaceholderBranch16(true_branch);
                } else {
                    try self.chunk.fillPlaceholderBranch16(false_branch);
                }
            },
            .while_stmt => |data| {
                const condition_location = self.chunk.getLocation();
                try self.compileStmts(data.condition);
                try self.chunk.addOpcode(.op_branch_if_false);
                const false_branch = try self.chunk.addPlaceholderBranch16();
                try self.compileStmts(data.body);
                try self.chunk.addBranchBackward16(condition_location);
                try self.chunk.fillPlaceholderBranch16(false_branch);
            },
            else => {
                std.debug.panic("Cannot compile statment: {}\n", .{stmt});
            },
        }
    }

    pub fn compileBuiltinPunct(self: Self, func: builtins.BuiltinFunctions) VM.AllocError!void {
        switch (func) {
            .bpct_add_int => try self.chunk.addOpcode(.op_add_int),
            .bpct_mul_int => try self.chunk.addOpcode(.op_mul_int),
            .bpct_div_int => try self.chunk.addOpcode(.op_div_int),
            .bpct_sub_int => try self.chunk.addOpcode(.op_sub_int),
            .bpct_add_float => try self.chunk.addOpcode(.op_add_float),
            .bpct_mul_float => try self.chunk.addOpcode(.op_mul_float),
            .bpct_div_float => try self.chunk.addOpcode(.op_div_float),
            .bpct_sub_float => try self.chunk.addOpcode(.op_sub_float),
            .bpct_gt_float => try self.chunk.addOpcode(.op_cmp_greater_float),
            .bpct_gt_int => try self.chunk.addOpcode(.op_cmp_greater_float),
            .bpct_ge_float => try self.chunk.addOpcode(.op_cmp_greater_equal_float),
            .bpct_ge_int => try self.chunk.addOpcode(.op_cmp_greater_equal_int),
            .bpct_lt_float => {
                try self.chunk.addOpcode(.op_swap);
                try self.chunk.addOpcode(.op_cmp_greater_float);
            },
            .bpct_lt_int => {
                try self.chunk.addOpcode(.op_swap);
                try self.chunk.addOpcode(.op_cmp_greater_int);
            },
            .bpct_le_float => {
                try self.chunk.addOpcode(.op_swap);
                try self.chunk.addOpcode(.op_cmp_greater_equal_float);
            },
            .bpct_le_int => {
                try self.chunk.addOpcode(.op_swap);
                try self.chunk.addOpcode(.op_cmp_greater_equal_int);
            },
            .bpct_eq => try self.chunk.addOpcode(.op_cmp_equal),
            else => {
                try self.chunk.addConstant(@intCast(u64, @enumToInt(func)) | (1 << 63));
                try self.chunk.addOpcode(.op_call);
            },
        }
    }

    pub fn stringLiteralToValue(str: []const u8, allocator: std.mem.Allocator) ![]const u8 {
        var i: u32 = 0;
        var strValLen: u32 = 0;
        while (i < str.len) : (i += 1) {
            if (str[i] == '\\')
                i += 1;
            strValLen += 1;
        }

        var strval = try allocator.alloc(u8, strValLen);
        i = 0;
        var j: u32 = 0;
        while (i < str.len) {
            if (str[i] == '\\') {
                i += 1;
                strval[j] = switch (str[i]) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '"' => '\"',
                    '\'' => '\'',
                    else => unreachable,
                };
            } else {
                strval[j] = str[i];
            }
            i += 1;
            j += 1;
        }
        return strval;
    }
};
