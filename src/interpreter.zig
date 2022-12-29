const panic = @import("std").debug.panic;
const std = @import("std");
const comp = @import("compiler.zig");
const values = @import("values.zig");
const builtins = @import("builtins.zig");
const Value = values.Value;
const VString = values.VString;
const DataPtr = values.DataPtr;
const DataSlice = values.DataSlice;
const ConstDataPtr = values.ConstDataPtr;

const libc = @cImport({
    @cInclude("stdlib.h");
});

const STACK_SIZE = 1 << 12; // One page     1 << 10 is 1KB,   1 << 20 is 1MB
var program_stack = [_]u64{0} ** STACK_SIZE;
var working_stack = [_]u64{0} ** STACK_SIZE;

const StringTable = std.StringHashMap(void);
const AllocationTable = std.AutoArrayHashMap(Value, usize); // tracks the size of allocations
const GlobalsList = std.ArrayListUnmanaged(Value);

pub const VM = struct {
    working_stack: []u64 = working_stack[0..],
    program_stack: []u64 = program_stack[0..],
    instr_ptr: usize = 0, // potentially optimise these to be actual pointers
    working_stack_ptr: usize = 0,
    base_ptr: usize = 0, // points to local 0
    stack_ptr: usize = 0, // points to return address of any called function
    bytecode: *comp.CompiledBytecodeChunk = undefined,
    allocations: AllocationTable,
    string_table: StringTable,
    // TODO: custom GC allocator, will let us use all the stdlib stuff
    // that uses allocators if we feel like it, only useful if we dont want our
    // own memory layout tho.
    allocator: std.mem.Allocator,
    memory_usage: isize = 0,

    globals: GlobalsList,

    stdout: std.fs.File,
    stdin: std.fs.File,

    const Self = *@This();

    pub fn init(allocator: std.mem.Allocator) !VM {
        return VM{
            .allocator = allocator,
            .string_table = StringTable.init(allocator),
            .allocations = AllocationTable.init(allocator),
            .globals = try GlobalsList.initCapacity(allocator, 128),
            .stdout = std.io.getStdOut(),
            .stdin = std.io.getStdIn(),
        };
    }

    pub fn deinit(self: Self) void {
        const keys = self.allocations.keys();
        const vals = self.allocations.values();
        for (keys) |key, i| {
            self.allocator.free(@intToPtr(DataPtr, key)[0..vals[i]]);
            self.memory_usage -= @intCast(isize, vals[i]);
        }
        std.debug.assert(self.memory_usage == 0);
        self.allocations.deinit();
        self.string_table.deinit();
        self.globals.deinit(self.allocator);
    }

    // Add 1 to instr_ptr
    inline fn readByte(self: Self, instr_ptr: [*]const u8) u8 {
        _ = self;
        return instr_ptr[0];
    }

    // Add 2 to instr_ptr
    inline fn readShort(self: Self, instr_ptr: [*]const u8) u16 {
        _ = self;
        const low_byte = @intCast(u16, instr_ptr[0]);
        //self.instr_ptr += 1;
        const high_byte = @intCast(u16, instr_ptr[1]);
        //self.instr_ptr += 1;
        return high_byte << 8 | low_byte;
    }

    inline fn stackPush(self: Self, value: u64) void {
        if (self.working_stack_ptr == STACK_SIZE) panic("Stack full, cannot push!", .{});
        self.working_stack[self.working_stack_ptr] = value;
        self.working_stack_ptr += 1;
    }

    inline fn stackPop(self: Self) u64 {
        if (self.working_stack_ptr == 0) panic("Stack empty, cannot pop!", .{});
        self.working_stack_ptr -= 1;
        return self.working_stack[self.working_stack_ptr];
    }

    inline fn stackPeek(self: Self) u64 {
        if (self.working_stack_ptr == 0) panic("Stack empty, cannot peek!", .{});
        return self.working_stack[self.working_stack_ptr - 1];
    }

    pub fn interpret(self: Self, chunk: *comp.CompiledBytecodeChunk) !void {
        // to optimise, get rid of chunks by maybe just having constants straight in the bytecode
        self.bytecode = chunk;
        var instr_ptr: [*]const u8 = @ptrCast([*]const u8, self.bytecode.buffer.ptr);
        while (true) {
            const byte = self.readByte(instr_ptr);
            instr_ptr += 1;
            switch (@intToEnum(comp.Opcode, byte)) {
                .op_return => {
                    if (self.base_ptr == 0) return;

                    const return_ip = self.program_stack[self.base_ptr - 1];
                    const return_chunk = self.program_stack[self.base_ptr - 2];
                    const new_base_ptr = self.program_stack[self.base_ptr - 3];
                    self.stack_ptr = self.base_ptr - 3;
                    self.base_ptr = new_base_ptr;
                    self.bytecode = @intToPtr(*comp.CompiledBytecodeChunk, return_chunk);
                    instr_ptr = @intToPtr([*]u8, return_ip);
                },
                .op_call => {
                    const func_addr = self.stackPop();
                    if ((func_addr >> 63) == 1) {
                        self.callBuiltinFunction(@intToEnum(builtins.BuiltinFunctions, func_addr ^ (1 << 63)));
                    } else {
                        self.program_stack[self.stack_ptr] = @intCast(u64, self.base_ptr);
                        self.program_stack[self.stack_ptr + 1] = @intCast(u64, @ptrToInt(self.bytecode));
                        self.program_stack[self.stack_ptr + 2] = @intCast(u64, @ptrToInt(instr_ptr));
                        self.base_ptr = self.stack_ptr + 3;
                        self.bytecode = @intToPtr(*comp.CompiledBytecodeChunk, func_addr);
                        instr_ptr = @ptrCast([*]const u8, self.bytecode.buffer.ptr);
                    }
                },
                .op_init_stack_frame => {
                    const frameSize = self.readByte(instr_ptr);
                    instr_ptr += 1;
                    self.stack_ptr = self.base_ptr + frameSize;
                },
                .op_pop => {
                    _ = self.stackPop();
                },
                .op_swap => {
                    const a = self.stackPop();
                    const b = self.stackPop();
                    self.stackPush(a);
                    self.stackPush(b);
                },
                .op_add_int => {
                    const b = @bitCast(i64, self.stackPop());
                    const a = @bitCast(i64, self.stackPop());
                    self.stackPush(@bitCast(u64, a + b));
                },
                .op_sub_int => {
                    const b = @bitCast(i64, self.stackPop());
                    const a = @bitCast(i64, self.stackPop());
                    self.stackPush(@bitCast(u64, a - b));
                },
                .op_mul_int => {
                    const b = @bitCast(i64, self.stackPop());
                    const a = @bitCast(i64, self.stackPop());
                    self.stackPush(@bitCast(u64, a * b));
                },
                .op_div_int => {
                    const b = @bitCast(i64, self.stackPop());
                    const a = @bitCast(i64, self.stackPop());
                    self.stackPush(@bitCast(u64, @divFloor(a, b)));
                },
                .op_add_float => {
                    const b = @bitCast(f64, self.stackPop());
                    const a = @bitCast(f64, self.stackPop());
                    self.stackPush(@bitCast(u64, a + b));
                },
                .op_sub_float => {
                    const b = @bitCast(f64, self.stackPop());
                    const a = @bitCast(f64, self.stackPop());
                    self.stackPush(@bitCast(u64, a - b));
                },
                .op_mul_float => {
                    const b = @bitCast(f64, self.stackPop());
                    const a = @bitCast(f64, self.stackPop());
                    self.stackPush(@bitCast(u64, a * b));
                },
                .op_div_float => {
                    const b = @bitCast(f64, self.stackPop());
                    const a = @bitCast(f64, self.stackPop());
                    self.stackPush(@bitCast(u64, a / b));
                },
                .op_constant => {
                    const index = self.readByte(instr_ptr);
                    instr_ptr += 1;
                    self.stackPush(self.bytecode.constants[index]);
                },
                .op_constant_16 => {},
                .op_true => {
                    self.stackPush(@intCast(u64, 1));
                },
                .op_false => {
                    self.stackPush(@intCast(u64, 0));
                },
                .op_negate_int => {
                    const a = @bitCast(i64, self.stackPop());
                    self.stackPush(@bitCast(u64, -a));
                },
                .op_negate_float => {
                    const a = @bitCast(f64, self.stackPop());
                    self.stackPush(@bitCast(u64, -a));
                },
                .op_not => {
                    const a = @bitCast(bool, @intCast(u1, self.stackPop()));
                    self.stackPush(@intCast(u64, @boolToInt(!a)));
                },
                .op_cmp_equal => {
                    const b = self.stackPop();
                    const a = self.stackPop();
                    self.stackPush(@intCast(u64, @bitCast(u1, a == b)));
                },
                .op_cmp_greater_int => {
                    const b = self.stackPop();
                    const a = self.stackPop();
                    self.stackPush(@intCast(u64, @bitCast(u1, @bitCast(i64, a) > @bitCast(i64, b))));
                },
                .op_cmp_greater_float => {
                    const b = self.stackPop();
                    const a = self.stackPop();
                    self.stackPush(@intCast(u64, @bitCast(u1, @bitCast(f64, a) > @bitCast(f64, b))));
                },
                .op_cmp_greater_equal_int => {
                    const b = self.stackPop();
                    const a = self.stackPop();
                    self.stackPush(@intCast(u64, @bitCast(u1, @bitCast(i64, a) >= @bitCast(i64, b))));
                },
                .op_cmp_greater_equal_float => {
                    const b = self.stackPop();
                    const a = self.stackPop();
                    self.stackPush(@intCast(u64, @bitCast(u1, @bitCast(f64, a) == @bitCast(f64, b))));
                },
                .op_str_concat => {
                    const b = VString.fromValue(self.stackPop()).toZigString();
                    const a = VString.fromValue(self.stackPop()).toZigString();
                    const new_str_data = try self.allocate(@sizeOf(u32) + b.len + a.len);
                    const lenptr = @ptrCast(*u32, new_str_data.ptr);
                    lenptr.* = @intCast(u32, a.len + b.len);
                    const a_start = @sizeOf(u32);
                    const b_start = @sizeOf(u32) + a.len;
                    std.mem.copy(u8, new_str_data[a_start..b_start], a);
                    std.mem.copy(u8, new_str_data[b_start..], b);
                    const vstring = try self.takeString(VString{ .ptr = new_str_data.ptr });
                    self.stackPush(vstring.toValue());
                },
                .op_get_global => {
                    const globalid = self.readShort(instr_ptr);
                    instr_ptr += 2;
                    const val = self.globals.items[globalid];
                    self.stackPush(val);
                },
                .op_set_local => {
                    const localid = self.readByte(instr_ptr);
                    instr_ptr += 1;
                    const value = self.stackPop();
                    self.program_stack[self.base_ptr + localid] = value;
                },
                .op_get_local => {
                    const localid = self.readByte(instr_ptr);
                    instr_ptr += 1;
                    self.stackPush(self.program_stack[self.base_ptr + localid]);
                },
                .op_branch => {
                    // we dont add 2 this time, because we are just about
                    // to modify the instruction ptr. this also means
                    // we dont do the weird +/- 2 thing in the compiler
                    const diff = self.readShort(instr_ptr);
                    instr_ptr += diff;
                },
                .op_branch_backward => {
                    const diff = self.readShort(instr_ptr);
                    instr_ptr -= diff;
                },
                .op_branch_if_false => {
                    const diff = self.readShort(instr_ptr);
                    if (self.stackPop() == 0)
                        instr_ptr += diff
                    else
                        instr_ptr += 2;
                },
                .op_branch_if_true => {
                    const diff = self.readShort(instr_ptr);
                    if (self.stackPop() == 1)
                        instr_ptr += diff
                    else
                        instr_ptr += 2;
                },
                .op_inc => {
                    const value = self.stackPop();
                    self.stackPush(value + 1);
                },
                .op_lookup => {
                    const ptr = self.stackPop();
                    self.stackPush(@intToPtr(*u64, ptr).*);
                },
                .op_store => {
                    const ptr = self.stackPop();
                    const val = self.stackPop();
                    @intToPtr(*u64, ptr).* = val;
                },
                .op_ptr_add => {
                    const b = self.stackPop();
                    const a = self.stackPop();
                    self.stackPush(@bitCast(u64, (a << 3) + b));
                },
            }
        }
    }

    pub inline fn callBuiltinFunction(self: Self, func: builtins.BuiltinFunctions) void {
        switch (func) {
            .bfn_print => {
                const str = self.stackPop();
                const zstr = VString.fromValue(str).toZigString();
                self.stdout.writeAll(zstr) catch panic("Failed to write to stdout\n", .{});
            },
            .bfn_print_int => {
                const int = self.stackPop();
                self.stdout.writer().print("{}", .{@bitCast(i64, int)}) catch panic("Failed to write to stdout\n", .{});
            },
            .bfn_print_float => {
                const float = self.stackPop();
                self.stdout.writer().print("{}", .{@bitCast(f64, float)}) catch panic("Failed to write to stdout\n", .{});
            },
            .bfn_print_bool => {
                const boolean = self.stackPop();
                if (boolean == 0) {
                    self.stdout.writeAll("false") catch panic("Failed to write to stdout\n", .{});
                } else {
                    self.stdout.writeAll("true") catch panic("Failed to write to stdout\n", .{});
                }
            },
            .bfn_print_ptr => {
                const ptr = self.stackPop();
                self.stdout.writer().print("0x{x}", .{ptr}) catch panic("Failed to write to stdout\n", .{});
            },
            .bfn_not => {
                const b = self.stackPop();
                self.stackPush(b ^ 1);
            },
            .bfn_malloc => {
                const size = self.stackPop();
                // self.stackPush(@intCast(u64, @ptrToInt((self.allocator.alloc(u64, size) catch panic("Failed to allocate memory.\n", .{})).ptr)));
                self.stackPush(@intCast(u64, @ptrToInt(libc.malloc(@intCast(c_ulong, size << 3)))));
            },
            .bfn_free => {
                const ptr = self.stackPop();
                // std.debug.print("{x}\n", .{ptr});
                //  self.allocator.free(@intToPtr([]u64, ptr));
                libc.free(@intToPtr(?*anyopaque, ptr));
            },
            .bfn_realloc => {
                const ptr = self.stackPop();
                const new_size = self.stackPop();
                // self.stackPush(@intCast(u64, @ptrToInt((self.allocator.realloc(@intToPtr([]u64, ptr), new_size) catch panic("Failed to reallocate memory.\n", .{})).ptr)));
                self.stackPush(@intCast(u64, @ptrToInt(libc.realloc(@intToPtr(?*anyopaque, ptr), @intCast(c_ulong, new_size << 3)))));
            },
            .bfn_mod_int => {
                const b = @bitCast(i64, self.stackPop());
                const a = @bitCast(i64, self.stackPop());
                self.stackPush(@bitCast(u64, @mod(a, b)));
            },
            .bfn_div_mod_int => {
                const b = @bitCast(i64, self.stackPop());
                const a = @bitCast(i64, self.stackPop());
                self.stackPush(@bitCast(u64, @divFloor(a, b)));
                self.stackPush(@bitCast(u64, @mod(a, b)));
            },

            else => panic("Cannot run builtin function '{}'\n", .{func}),
        }
    }

    pub const AllocError = error{
        NotManaged,
    } || std.mem.Allocator.Error;

    pub fn allocate(self: Self, bytes: usize) AllocError!DataSlice {
        var data = try self.allocator.allocAdvanced(u8, @alignOf(u64), bytes, .at_least);
        try self.allocations.put(@ptrToInt(data.ptr), data.len);
        self.memory_usage += @intCast(isize, data.len);
        return data;
    }

    // Perhaps this should be combined into the main reallocate function. Depends on how reallocAdvanced works
    pub fn reallocateRawPtr(self: Self, old_mem: DataPtr, bytes: usize) AllocError!DataSlice {
        const old_len = self.allocations.get(@intCast(u64, @ptrToInt(old_mem))) orelse return error.NotManaged;
        return self.reallocate(old_mem[0..old_len], bytes);
    }

    pub fn reallocate(self: Self, old_mem: DataSlice, bytes: usize) AllocError!DataSlice {
        var new_mem = try self.allocator.reallocAdvanced(old_mem, @alignOf(u64), bytes, .at_least);
        self.allocations.put(@intCast(u64, @ptrToInt(new_mem.ptr)), new_mem.len);

        var old_size = self.allocations.fetchSwapRemove(@intCast(u64, @ptrToInt(old_mem.ptr)));
        if (old_size) |os| {
            self.memory_usage += new_mem.len - os.value;
        } else {
            self.memory_usage += new_mem.len;
        }
        return new_mem;
    }
    // Not zig-like, but helps identify bugs
    pub fn free(self: Self, ptr: ConstDataPtr) AllocError!void {
        const allocSize = self.allocations.get(@intCast(Value, @ptrToInt(ptr))) orelse return error.NotManaged;
        const wasRemoved = self.allocations.swapRemove(@intCast(u64, @ptrToInt(ptr)));
        std.debug.assert(wasRemoved);
        self.memory_usage -= @intCast(isize, allocSize);
        self.allocator.free(ptr[0..allocSize]);
    }

    // If the string already exists, no allocations happen
    // We do not take ownership of the string and its memory
    // should be handled by the calling function.
    pub fn allocZigString(self: Self, string: []const u8) AllocError!VString {
        const maybeKey = self.string_table.getKey(string);
        if (maybeKey) |key| {
            return VString.fromZigString(key);
        }
        const strmem = try self.allocate(string.len + @sizeOf(u32));
        const lenptr = @ptrCast(*u32, strmem.ptr);
        lenptr.* = @intCast(u32, string.len);
        std.mem.copy(u8, strmem[@sizeOf(u32)..], string);
        try self.string_table.put(strmem[@sizeOf(u32)..], {});
        return VString{
            .ptr = strmem.ptr,
        };
    }

    // registers the string in the interpreters intern table and if it already exists, returns the old reference
    // freeing whatever string it was given.
    pub fn takeString(self: Self, string: VString) AllocError!VString {
        const zstr = string.toZigString();
        const maybeKey = self.string_table.getKey(zstr);
        if (maybeKey) |key| {
            try self.free(string.ptr);
            return VString.fromZigString(key);
        } else {
            try self.string_table.put(zstr, {});
            return string;
        }
    }
};
