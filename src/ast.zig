const std = @import("std");
const tk = @import("tokenizer.zig");
const col = @import("colours.zig");
const types = @import("types.zig");
const builtins = @import("builtins.zig");

pub const StmtNode = struct {
    s: Stmt,
    start: u32,
    end: u32,

    pub const Stmt = union(enum) {
        semi: void,
        ident: void,
        punct_ident: void,
        dot_ident: void,
        bool_lit: bool,
        int_lit: i64,
        float_lit: f64,
        string_lit: []const u8,
        if_stmt: struct {
            condition: []*StmtNode,
            if_body: []*StmtNode,
            else_body: []*StmtNode,
        },
        while_stmt: struct {
            condition: []*StmtNode,
            body: []*StmtNode,
        },
        for_stmt: struct {
            var_name: tk.Token,
            body: []*StmtNode,
        },
        assign_stmt: tk.Token,
        fn_decl: struct {
            ty: *TypeNode,
            body: []*StmtNode,
            name: tk.Token,
        },
        type_name: struct {
            name: tk.Token,
            ty: *TypeNode,
        },

        // Bit of a hack, but we modify the ast when resolving variables
        // Once type checked, we use these identifiers

        // index into the locals of this function
        local: u8,
        assign_local: u8,
        // index into the globals
        global: u16,
        // an actual value, not an index
        builtin: builtins.BuiltinFunctions,

        global_punct: u16,
        builtin_punct: builtins.BuiltinFunctions,

        fn_decl_concrete: struct {
            body: []*StmtNode,
            id: u16,
            frameSize: u8,
        },

        for_stmt_concrete: struct {
            local: u8,
            body: []*StmtNode,
        },
    };
};

pub const TypeNode = struct {
    ty: TypeExpr,
    start: u32,
    end: u32,

    pub const TypeExpr = union(enum) {
        ty_name: void,
        ty_func: struct {
            inputs: []*TypeNode,
            output: []*TypeNode,
        },
        ty_op: struct {
            name: []const u8,
            params: []*TypeNode,
        },
        ty_generic: u8,
    };
};
