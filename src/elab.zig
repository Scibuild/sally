const std = @import("std");
const panic = @import("std").debug.panic;
const ast = @import("ast.zig");
const types = @import("types.zig");
const Token = @import("tokenizer.zig").Token;
const errs = @import("errors.zig");
const builtins = @import("builtins.zig");

const Type = types.Type;

pub const TypeStack = struct {
    ty: Type,
    tail: ?*TypeStack,

    pub fn format(self: *const @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        var temp_self: ?*const @This() = self;
        try writer.writeByte('[');
        if (temp_self) |ts| {
            try ts.ty.format(fmt, options, writer);
            temp_self = ts.tail;
        }
        while (temp_self) |ts| {
            try writer.writeByte(',');
            try writer.writeByte(' ');
            try ts.ty.format(fmt, options, writer);
            temp_self = ts.tail;
        }
        try writer.writeByte(']');
    }
};

pub const Elaborator = struct {
    globals: std.StringHashMap(GlobalEntry),
    returns: []const Type,
    source: [:0]const u8,

    locals: [MAX_LOC_VARS]LocalVariable = undefined,
    scopeDepth: u8 = 0,
    localCount: u8 = 0,
    maxLocalCount: u8 = 0,

    ty_stack_alloc: std.heap.ArenaAllocator,
    type_stack: ?*TypeStack,
    metacounter: u32 = 0,

    errors: *errs.Errors,

    const MAX_LOC_VARS = 1 << 8;

    const GlobalEntry = struct { ty: Type, id: u16 };

    pub const LocalVariable = struct {
        ident: []const u8,
        depth: u8,
        ty: Type,
    };

    pub fn lookupOrAddLocal(self: Self, name: []const u8, ty: Type, start: u32, end: u32) TypeError!u8 {
        if (self.findLocal(name)) |local| {
            try self.unifyTypes(self.locals[local].ty, ty, start, end);
            return local;
        }

        try self.addLocal(name, ty, start, end);
        return self.localCount - 1;
    }

    pub fn findLocal(self: Self, name: []const u8) ?u8 {
        for (self.locals[0..self.localCount]) |local, i| {
            if (std.mem.eql(u8, name, local.ident)) {
                return @intCast(u8, i);
            }
        }
        return null;
    }

    pub fn addLocal(self: Self, name: []const u8, ty: Type, start: u32, end: u32) TypeError!void {
        if (self.localCount >= MAX_LOC_VARS) {
            return self.reportError(errs.Error{
                .too_many_variables = errs.Error.range(start, end),
            });
        }
        self.locals[self.localCount] = LocalVariable{
            .ident = name,
            .depth = self.scopeDepth,
            .ty = ty,
        };
        self.localCount += 1;
        if (self.localCount > self.maxLocalCount)
            self.maxLocalCount = self.localCount;
    }

    pub fn enterScope(self: Self) void {
        self.scopeDepth += 1;
    }

    pub fn leaveScope(self: Self) void {
        self.scopeDepth -= 1;
        while (self.localCount > 0 and self.locals[self.localCount - 1].depth > self.scopeDepth) {
            self.localCount -= 1;
        }
    }

    pub const TypeError = error{ ElabFailed, Overflow } || std.mem.Allocator.Error;

    const Self = *@This();

    pub fn init(source: [:0]const u8, allocator: std.mem.Allocator, errors: *errs.Errors) Elaborator {
        var ty_stack_alloc = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        return Elaborator{
            .globals = std.StringHashMap(GlobalEntry).init(allocator),
            .source = source,
            .errors = errors,
            .returns = &.{},
            .ty_stack_alloc = ty_stack_alloc,
            .type_stack = null,
        };
    }

    pub fn deinit(self: Self) void {
        self.globals.deinit();
        self.ty_stack_alloc.deinit();
    }

    pub fn elabProgram(self: Self, program: []*ast.StmtNode) TypeError!void {
        // A program is a collection of definitions of functions, constants and
        // imports, but we dont have constants or imports yet, so its just functions

        // todo, dont allow globals with the same name as other builtins or globals
        for (program) |decl| {
            switch (decl.s) {
                .fn_decl => |data| {
                    const ty = try self.typeNodeToType(data.ty);
                    switch (ty) {
                        .function => {},
                        else => return self.reportError(errs.Error{ .expected_function = .{
                            .got = ty,
                            .start = data.ty.start,
                            .end = data.ty.end,
                        } }),
                    }

                    try self.globals.put(
                        data.name.toString(self.source),
                        GlobalEntry{ .ty = ty, .id = @intCast(u16, self.globals.count()) },
                    );
                },
                else => return self.reportError(errs.Error{ .invalid_stmt = decl }),
            }
        }

        for (program) |decl| {
            switch (decl.s) {
                .fn_decl => |data| {
                    const fn_def = self.globals.getPtr(data.name.toString(self.source)).?;
                    const fn_ty = fn_def.ty.function;
                    self.returns = fn_ty.outputs;

                    self.type_stack = null;
                    self.maxLocalCount = 0;
                    self.localCount = 0;
                    self.scopeDepth = 0;
                    for (fn_ty.inputs) |inp_ty| {
                        try self.pushType(inp_ty);
                    }
                    try self.elabStmts(data.body);

                    for (fn_ty.outputs) |_, i| {
                        const ret_ty = fn_ty.outputs[fn_ty.outputs.len - i - 1];
                        if (self.type_stack == null) {
                            return self.reportError(errs.Error{ .expected_type_on_stack = .{
                                .want = fn_ty.outputs[0..(fn_ty.outputs.len - i)],
                                .start = decl.end - 1,
                                .end = decl.end,
                            } });
                        }
                        const stack_ty = try self.popType(decl.end - 1, decl.end);
                        try self.unifyTypes(ret_ty, stack_ty, decl.end - 1, decl.end);
                    }
                    if (self.type_stack) |ts| {
                        return self.reportError(errs.Error{ .stack_nonempty = .{
                            .got = ts,
                            .start = decl.end - 1,
                            .end = decl.end,
                        } });
                    }
                    decl.s = ast.StmtNode.Stmt{ .fn_decl_concrete = .{
                        .body = data.body,
                        .id = fn_def.id,
                        .frameSize = self.maxLocalCount,
                    } };
                },
                else => return self.reportError(errs.Error{ .invalid_stmt = decl }),
            }
        }
    }

    pub fn elabStmts(self: Self, stmts: []*ast.StmtNode) TypeError!void {
        for (stmts) |stmt| {
            try self.elabStmt(stmt);
        }
    }

    pub fn instantiateFreshMetavars(self: Self, ty: Type) TypeError!Type {
        var metavars: [128]?*Type = [_]?*Type{null} ** 128;
        return self.instantiateFreshMetavarsHelper(ty, &metavars);
    }

    fn instantiateFreshMetavarsHelper(self: Self, ty: Type, metavars: []?*Type) TypeError!Type {
        switch (ty) {
            .fixed_var => |v| {
                if (metavars[v.char]) |mv| {
                    return Type{ .ref = mv };
                }
                var freshMetavar = try self.ty_stack_alloc.allocator().create(Type);
                freshMetavar.* = Type{ .metavar = self.metacounter };
                self.metacounter += 1;
                metavars[v.char] = freshMetavar;
                return Type{ .ref = freshMetavar };
            },
            .function => |data| {
                var tys = try self.ty_stack_alloc.allocator().alloc(Type, data.inputs.len + data.outputs.len);
                for (data.inputs) |inTy, i| {
                    tys[i] = try self.instantiateFreshMetavarsHelper(inTy, metavars);
                }
                for (data.outputs) |outTy, i| {
                    tys[data.inputs.len + i] = try self.instantiateFreshMetavarsHelper(outTy, metavars);
                }
                var inputs = tys[0..data.inputs.len];
                var outputs = tys[data.inputs.len..];
                return Type{ .function = .{ .inputs = inputs, .outputs = outputs } };
            },
            else => return ty,
        }
    }

    pub fn typeNodeToType(self: Self, typenode: *ast.TypeNode) TypeError!Type {
        switch (typenode.ty) {
            .ty_name => {
                const name = self.source[typenode.start..typenode.end];
                // types and variable names exist in completely different worlds with no way to
                // interact, so its totally ok to use the same names if you wanted to.
                if (std.mem.eql(u8, name, "int")) {
                    return Type{ .int = {} };
                } else if (std.mem.eql(u8, name, "float")) {
                    return Type{ .float = {} };
                } else if (std.mem.eql(u8, name, "string")) {
                    return Type{ .string = {} };
                } else if (std.mem.eql(u8, name, "bool")) {
                    return Type{ .boolean = {} };
                } else {
                    panic("Cannot reason about type {s}.", .{name});
                }
            },
            .ty_generic => |c| {
                return Type{ .fixed_var = .{ .char = c, .id = 0 } };
            },
            .ty_func => |data| {
                var tys = try self.ty_stack_alloc.allocator().alloc(Type, data.inputs.len + data.output.len);
                for (data.inputs) |inTy, i| {
                    tys[i] = try self.typeNodeToType(inTy);
                }
                for (data.output) |outTy, i| {
                    tys[data.inputs.len + i] = try self.typeNodeToType(outTy);
                }
                var inputs = tys[0..data.inputs.len];
                var outputs = tys[data.inputs.len..];
                return Type{ .function = .{ .inputs = inputs, .outputs = outputs } };
            },
            else => |t| panic("Cannot reason about type {}.", .{t}),
        }
    }

    pub fn elabStmt(self: Self, stmt: *ast.StmtNode) TypeError!void {
        switch (stmt.s) {
            .int_lit => {
                try self.pushType(Type.t_int);
            },
            .bool_lit => {
                try self.pushType(Type.t_boolean);
            },
            .float_lit => {
                try self.pushType(Type.t_float);
            },
            .string_lit => {
                try self.pushType(Type.t_string);
            },
            .semi => {
                const fn_ty = try self.popType(stmt.start, stmt.end);
                try self.applyFunction(fn_ty, stmt.start, stmt.end);
            },
            .if_stmt => |data| {
                try self.elabStmts(data.condition);

                const cond_ty = try self.popType(stmt.start, stmt.start + 2);
                try self.unifyTypes(cond_ty, Type.t_boolean, stmt.start, stmt.start + 2);

                const initial_stack = self.type_stack;
                self.enterScope();
                try self.elabStmts(data.if_body);
                self.leaveScope();
                const if_branch_stack = self.type_stack;

                self.type_stack = initial_stack;
                self.enterScope();
                try self.elabStmts(data.else_body);
                self.leaveScope();
                const else_branch_stack = self.type_stack;
                try self.unifyStacks(if_branch_stack, else_branch_stack, stmt.start, stmt.end);
            },
            .while_stmt => |data| {
                try self.elabStmts(data.condition);

                const cond_ty = try self.popType(stmt.start, stmt.start + 2);
                try self.unifyTypes(cond_ty, Type.t_boolean, stmt.start, stmt.start + 2);

                const pre_loop_stack = self.type_stack;
                self.enterScope();
                try self.elabStmts(data.body);
                self.leaveScope();
                const post_loop_stack = self.type_stack;

                try self.unifyStacks(pre_loop_stack, post_loop_stack, stmt.start, stmt.end);
            },

            .assign_stmt => |name| {
                const ty = try self.popType(stmt.start, stmt.end);

                const local = try self.lookupOrAddLocal(name.toString(self.source), ty, stmt.start, stmt.end);
                stmt.s = ast.StmtNode.Stmt{ .assign_local = local };
            },
            .punct_ident => {
                const builtin_punct = self.selectBuiltinPunct(stmt);
                const name = self.source[stmt.start..stmt.end];
                if (builtin_punct) |bp| {
                    const ty = try self.instantiateFreshMetavars(bp.getType());
                    try self.applyFunction(ty, stmt.start, stmt.end);
                    stmt.s = ast.StmtNode.Stmt{ .builtin_punct = bp };
                    return;
                } else if (self.globals.get(name)) |global| {
                    const ty = try self.instantiateFreshMetavars(global.ty);
                    try self.applyFunction(ty, stmt.start, stmt.end);
                    stmt.s = ast.StmtNode.Stmt{ .global_punct = global.id };
                    return;
                }
                return self.reportError(errs.Error{ .unknown_variable = errs.Error.range(stmt.start, stmt.end) });
            },
            .ident => {
                const name = self.source[stmt.start..stmt.end];
                if (self.findLocal(name)) |i| {
                    const local = self.locals[i];
                    try self.pushType(try self.instantiateFreshMetavars(local.ty));
                    stmt.s = ast.StmtNode.Stmt{ .local = i };
                    return;
                } else if (self.globals.get(name)) |global| {
                    try self.pushType(try self.instantiateFreshMetavars(global.ty));
                    stmt.s = ast.StmtNode.Stmt{ .global = global.id };
                    return;
                } else if (builtins.BuiltinFunctions.names.get(name)) |builtin| {
                    try self.pushType(try self.instantiateFreshMetavars(builtin.getType()));
                    stmt.s = ast.StmtNode.Stmt{ .builtin = builtin };
                    return;
                }
                return self.reportError(errs.Error{ .unknown_variable = errs.Error.range(stmt.start, stmt.end) });
            },
            else => return self.reportError(errs.Error{ .invalid_stmt = stmt }),
        }
    }

    pub fn selectBuiltinPunct(self: Self, stmt: *ast.StmtNode) ?builtins.BuiltinFunctions {
        const name = self.source[stmt.start..stmt.end];
        if (name.len == 1) {
            const c = name[0];
            switch (c) {
                '+', '*', '-', '/', '<', '>' => {
                    if (self.type_stack == null) return null;
                    const ty1 = self.type_stack.?.*.ty;

                    // TODO: this is really dodgy with how type constraint solving works
                    if (Type.eq(ty1, Type.t_int)) {
                        return switch (c) {
                            '+' => .bpct_add_int,
                            '*' => .bpct_mul_int,
                            '/' => .bpct_div_int,
                            '-' => .bpct_sub_int,
                            '<' => .bpct_lt_int,
                            '>' => .bpct_gt_int,
                            else => null,
                        };
                    }
                    if (Type.eq(ty1, Type.t_float)) {
                        return switch (c) {
                            '+' => .bpct_add_float,
                            '*' => .bpct_mul_float,
                            '/' => .bpct_div_float,
                            '-' => .bpct_sub_float,
                            '<' => .bpct_lt_float,
                            '>' => .bpct_gt_float,
                            else => null,
                        };
                    }
                    return null;
                },
                '=' => return .bpct_eq,
                else => return null,
            }
        } else if (std.mem.eql(u8, ">=", name)) {
            if (self.type_stack == null) return null;
            const ty1 = self.type_stack.?.*.ty;

            if (Type.eq(ty1, Type.t_int)) {
                return .bpct_ge_int;
            }
            if (Type.eq(ty1, Type.t_float)) {
                return .bpct_ge_float;
            }
        } else if (std.mem.eql(u8, "<=", name)) {
            if (self.type_stack == null) return null;
            const ty1 = self.type_stack.?.*.ty;

            if (Type.eq(ty1, Type.t_int)) {
                return .bpct_le_int;
            }
            if (Type.eq(ty1, Type.t_float)) {
                return .bpct_le_float;
            }
        }
        return null;
    }

    pub fn applyFunction(self: Self, fn_ty: Type, start: u32, end: u32) TypeError!void {
        switch (fn_ty) {
            .function => |data| {
                for (data.inputs) |_, i| {
                    const ty = data.inputs[data.inputs.len - i - 1];
                    const stack_ty = try self.popType(start, end);
                    if (!Type.eq(ty, stack_ty)) {
                        return self.reportError(errs.Error{ .unification_error = .{
                            .ty1 = ty,
                            .ty2 = stack_ty,
                            .start = start,
                            .end = end,
                        } });
                    }
                }
                for (data.outputs) |ty| {
                    try self.pushType(ty);
                }
            },
            else => {
                return self.reportError(errs.Error{ .expected_function = .{
                    .got = fn_ty,
                    .start = start,
                    .end = end,
                } });
            },
        }
    }

    pub fn pushType(self: Self, ty: Type) TypeError!void {
        var new_stack_item = try self.ty_stack_alloc.allocator().create(TypeStack);
        new_stack_item.ty = ty;
        new_stack_item.tail = self.type_stack;
        self.type_stack = new_stack_item;
    }

    pub fn popType(self: Self, start: u32, end: u32) TypeError!Type {
        if (self.type_stack) |ts| {
            self.type_stack = ts.tail;
            return ts.ty;
        }
        return self.reportError(errs.Error{ .cannot_pop_empty_stack = errs.Error.Range{
            .start = start,
            .end = end,
        } });
    }

    pub fn reportError(self: Self, err: errs.Error) TypeError {
        self.errors.addError(err);
        return error.ElabFailed;
    }

    pub fn unifyTypes(self: Self, ty1: Type, ty2: Type, start: u32, end: u32) TypeError!void {
        if (!Type.eq(ty1, ty2)) {
            return self.reportError(errs.Error{ .unification_error = .{
                .ty1 = ty1,
                .ty2 = ty2,
                .start = start,
                .end = end,
            } });
        }
    }

    pub fn unifyStacks(self: Self, stack1: ?*TypeStack, stack2: ?*TypeStack, start: u32, end: u32) TypeError!void {
        var ots1 = stack1;
        var ots2 = stack2;
        while (true) {
            if (ots1 == ots2) return;
            if (ots1) |ts1| {
                if (ots2) |ts2| {
                    if (Type.eq(ts1.ty, ts2.ty)) {
                        ots1 = ts1.tail;
                        ots2 = ts2.tail;
                        continue;
                    }
                }
            }
            return self.reportError(errs.Error{ .failed_to_unify_stacks = .{
                .stack1 = stack1,
                .stack2 = stack2,
                .start = start,
                .end = end,
            } });
        }
    }
};
