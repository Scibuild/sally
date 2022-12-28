const col = @import("colours.zig");
const std = @import("std");

pub const Type = union(enum) {
    int: void,
    float: void,
    string: void,
    boolean: void,
    function: struct {
        inputs: []const Type,
        outputs: []const Type,
    },
    fixed_var: Var,
    ref: *Type,
    metavar: u32,

    pub const t_int = Type{ .int = {} };
    pub const t_float = Type{ .float = {} };
    pub const t_string = Type{ .string = {} };
    pub const t_boolean = Type{ .boolean = {} };

    pub const Var =
        struct { char: u8, id: u16 };

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .int => try writer.writeAll(col.keyword ++ "Int" ++ col.end),
            .float => try writer.writeAll(col.keyword ++ "Float" ++ col.end),
            .string => try writer.writeAll(col.keyword ++ "String" ++ col.end),
            .boolean => try writer.writeAll(col.keyword ++ "Bool" ++ col.end),
            .metavar => |id| {
                try writer.print(col.number ++ "$mv_{}" ++ col.end, .{id});
            },
            .fixed_var => |v| {
                try writer.print(col.number ++ "${c}{}" ++ col.end, .{ v.char, v.id });
            },
            .ref => |ref| {
                try writer.print("{}", .{ref.*});
            },
            .function => |data| {
                try writer.writeByte('(');
                for (data.inputs) |input| {
                    try input.format(fmt, options, writer);
                    try writer.writeByte(' ');
                }
                try writer.writeAll("--");
                for (data.outputs) |output| {
                    try writer.writeByte(' ');
                    try output.format(fmt, options, writer);
                }
                try writer.writeByte(')');
            },
        }
    }

    pub fn unwrapTy(ty: Type) Type {
        switch (ty) {
            .ref => |ref| {
                return unwrapTy(ref.*);
            },
            else => return ty,
        }
    }

    pub fn containsMeta(ty: Type, metaid: u32) bool {
        switch (ty) {
            .function => |data| {
                for (data.inputs) |inty| {
                    if (containsMeta(inty, metaid)) return true;
                }
                for (data.outputs) |outty| {
                    if (containsMeta(outty, metaid)) return true;
                }
                return false;
            },
            .metavar => |id| {
                return id == metaid;
            },
            else => return false,
        }
    }

    // Really this is unification, except it reports success in a bool,, which some might say is a little less useful
    // never call with a straight metavar in either position
    pub fn eq(ty1: @This(), ty2: @This()) bool {
        switch (ty2) {
            .ref => |ref2| {
                switch (ref2.*) {
                    .metavar => |id2| {
                        // before we say that ty2 can be solved to be ty1, we check this
                        // a) isnt circular, that ty1 is secretly relying on a solution to ty2
                        // b) that ty1 doesnt contain anything that relyies on a solution to ty2
                        // note these are subtly different, a) just means we dont resolve the variable yet
                        // but b) is an actual type inequality
                        const ty1unwrapped = unwrapTy(ty1);
                        switch (ty1unwrapped) {
                            .metavar => |id1| if (id1 == id2)
                                return true,

                            else => if (containsMeta(ty1unwrapped, id2))
                                return false,
                        }
                        ref2.* = ty1;
                        return true;
                    },
                    else => return Type.eq(ty1, ref2.*),
                }
            },
            .metavar => |_| {
                std.debug.panic("cannot solve raw (ref-less) metavar", .{});
            },
            else => {},
        }

        switch (ty1) {
            .int => switch (ty2) {
                .int => return true,
                else => {},
            },
            .float => switch (ty2) {
                .float => return true,
                else => {},
            },
            .string => switch (ty2) {
                .string => return true,
                else => {},
            },
            .boolean => switch (ty2) {
                .boolean => return true,
                else => {},
            },
            .fixed_var => |ty1fv| switch (ty2) {
                .fixed_var => |ty2fv| return ty1fv.char == ty2fv.char and ty1fv.id == ty2fv.id,
                else => {},
            },
            .function => |inout1| {
                switch (ty2) {
                    .function => |inout2| {
                        var areEqual = true;
                        for (inout1.inputs) |_, i| {
                            if (!Type.eq(inout1.inputs[i], inout2.inputs[i])) {
                                areEqual = false;
                            }
                        }
                        for (inout1.outputs) |_, i| {
                            if (!Type.eq(inout1.outputs[i], inout2.outputs[i])) {
                                areEqual = false;
                            }
                        }

                        return areEqual;
                    },
                    else => {},
                }
            },
            .ref => |ref| {
                switch (ref.*) {
                    .metavar => |id| {
                        // thanks to earlier work, we know that ty2 cannot be a ref or a metavar,
                        // so we are safe to solve (after checking it doesnt contain id)
                        if (containsMeta(ty2, id)) {
                            return false;
                        }
                        ref.* = ty2;
                        return true;
                    },
                    else => return Type.eq(ref.*, ty2),
                }
            },
            .metavar => |_| {
                std.debug.panic("cannot solve raw (ref-less) metavar", .{});
            },
        }

        return false;
    }
};
