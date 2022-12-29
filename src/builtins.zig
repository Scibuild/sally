const std = @import("std");
const Type = @import("types.zig").Type;

pub const BuiltinFunctions = enum(u63) {
    bfn_print,
    bfn_print_int,
    bfn_print_float,
    bfn_print_bool,
    bfn_print_ptr,
    bfn_not,
    bfn_malloc,
    bfn_realloc,
    bfn_free,
    bfn_mod_int,
    bfn_div_mod_int,

    bpct_add_int,
    bpct_mul_int,
    bpct_div_int,
    bpct_sub_int,
    bpct_add_float,
    bpct_mul_float,
    bpct_div_float,
    bpct_sub_float,
    bpct_eq,
    bpct_lt_int,
    bpct_lt_float,
    bpct_le_int,
    bpct_le_float,
    bpct_gt_int,
    bpct_gt_float,
    bpct_ge_int,
    bpct_ge_float,
    bpct_ptr_lookup,
    bpct_ptr_store,
    bpct_ptr_add,
    bpct_ptr_sub,

    pub const names = std.ComptimeStringMap(BuiltinFunctions, .{
        .{ "print", .bfn_print },
        .{ "print-int", .bfn_print_int },
        .{ "print-float", .bfn_print_float },
        .{ "print-bool", .bfn_print_bool },
        .{ "print-ptr", .bfn_print_ptr },

        .{ "not", .bfn_not },
        .{ "alloc", .bfn_malloc },
        .{ "realloc", .bfn_realloc },
        .{ "free", .bfn_free },

        .{ "mod", .bfn_mod_int },
        .{ "/mod", .bfn_div_mod_int },
    });

    const ts_bool = ([_]Type{Type.t_boolean})[0..];
    const ts_int = ([_]Type{Type.t_int})[0..];
    const ts_int_int = ([_]Type{ Type.t_int, Type.t_int })[0..];
    const ts_float = ([_]Type{Type.t_float})[0..];
    const ts_float_float = ([_]Type{ Type.t_float, Type.t_float })[0..];
    const ts_string = ([_]Type{Type.t_string})[0..];
    const ts_none = &.{};
    const ts_a = ([_]Type{Type{ .fixed_var = Type.Var{ .char = 'a', .id = 0 } }})[0..];
    const ts_a_a = ([_]Type{
        Type{ .fixed_var = Type.Var{ .char = 'a', .id = 0 } },
        Type{ .fixed_var = Type.Var{ .char = 'a', .id = 0 } },
    })[0..];
    const ts_a_b = ([_]Type{
        Type{ .fixed_var = Type.Var{ .char = 'a', .id = 0 } },
        Type{ .fixed_var = Type.Var{ .char = 'b', .id = 0 } },
    })[0..];
    const ts_b_a = ([_]Type{
        Type{ .fixed_var = Type.Var{ .char = 'b', .id = 0 } },
        Type{ .fixed_var = Type.Var{ .char = 'a', .id = 0 } },
    })[0..];

    const ts_a_ptr = ([_]Type{Type{ .operator = .{ .name = "ptr", .params = ts_a } }})[0..];
    const ts_a_a_ptr = ([_]Type{
        Type{ .fixed_var = Type.Var{ .char = 'a', .id = 0 } },
        Type{ .operator = .{ .name = "ptr", .params = ts_a } },
    })[0..];
    const ts_int_a_ptr = ([_]Type{
        Type.t_int,
        Type{ .operator = .{ .name = "ptr", .params = ts_a } },
    })[0..];
    const ts_a_ptr_a_ptr = ([_]Type{
        Type{ .fixed_var = Type.Var{ .char = 'a', .id = 0 } },
        Type{ .fixed_var = Type.Var{ .char = 'a', .id = 0 } },
    })[0..];

    pub fn getType(self: @This()) Type {
        return Type{ .function = switch (self) {
            .bfn_print => .{ .inputs = ts_string, .outputs = &.{} },
            .bfn_print_int => .{ .inputs = ts_int, .outputs = &.{} },
            .bfn_print_float => .{ .inputs = ts_float, .outputs = &.{} },
            .bfn_print_bool => .{ .inputs = ts_bool, .outputs = &.{} },
            .bfn_print_ptr => .{ .inputs = ts_a_ptr, .outputs = &.{} },
            .bfn_not => .{
                .inputs = ts_bool,
                .outputs = ts_bool,
            },
            .bfn_malloc => .{
                .inputs = ts_int,
                .outputs = ts_a_ptr,
            },
            .bfn_free => .{
                .inputs = ts_a_ptr,
                .outputs = ts_none,
            },
            .bfn_realloc => .{
                .inputs = ts_int_a_ptr,
                .outputs = ts_a_ptr,
            },
            .bfn_div_mod_int => .{
                .inputs = ts_int_int,
                .outputs = ts_int_int,
            },
            .bpct_add_int,
            .bpct_mul_int,
            .bpct_div_int,
            .bpct_sub_int,
            .bfn_mod_int,
            => .{
                .inputs = ts_int_int,
                .outputs = ts_int,
            },
            .bpct_add_float,
            .bpct_mul_float,
            .bpct_div_float,
            .bpct_sub_float,
            => .{
                .inputs = ts_float_float,
                .outputs = ts_float,
            },
            .bpct_eq => .{
                .inputs = ts_a_a,
                .outputs = ts_bool,
            },
            .bpct_lt_float,
            .bpct_le_float,
            .bpct_gt_float,
            .bpct_ge_float,
            => .{
                .inputs = ts_float_float,
                .outputs = ts_bool,
            },
            .bpct_lt_int,
            .bpct_le_int,
            .bpct_gt_int,
            .bpct_ge_int,
            => .{
                .inputs = ts_int_int,
                .outputs = ts_bool,
            },
            .bpct_ptr_lookup => .{
                .inputs = ts_a_ptr,
                .outputs = ts_a,
            },
            .bpct_ptr_store => .{
                .inputs = ts_a_a_ptr,
                .outputs = ts_none,
            },
            .bpct_ptr_add => .{
                .inputs = ts_int_a_ptr,
                .outputs = ts_a_ptr,
            },
            .bpct_ptr_sub => .{
                .inputs = ts_a_ptr_a_ptr,
                .outputs = ts_int,
            },
        } };
    }
};
