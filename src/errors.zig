const std = @import("std");
const tk = @import("tokenizer.zig");
const types = @import("types.zig");
const cols = @import("colours.zig");
const elab = @import("elab.zig");
const ast = @import("ast.zig");

pub const Error = union(enum) {
    pub const Range = struct {
        start: u32,
        end: u32,

        pub fn toString(self: @This(), source: [:0]const u8) []const u8 {
            return source[self.start..self.end];
        }
    };

    // PARSER ERRORS
    expected_but_found: struct {
        expected: tk.TokenClass,
        found: tk.Token,
    },
    unexpected_token: tk.Token,
    expected_type_operator_name: Range,
    invalid_str: tk.Token,

    // TYPE CHECKING ERRORS

    unknown_variable: Range,
    invalid_stmt: *ast.StmtNode,
    unification_error: struct {
        ty1: types.Type,
        ty2: types.Type,
        start: u32,
        end: u32,
    },
    failed_to_unify_stacks: struct {
        stack1: ?*elab.TypeStack,
        stack2: ?*elab.TypeStack,
        start: u32,
        end: u32,
    },
    too_many_variables: Range,
    too_many_nested_functions: tk.Token,
    variable_already_exists: tk.Token,
    cannot_assign_to_const: tk.Token,

    unreachable_statement: Range,
    cannot_return_from_global_scope: Range,
    cannot_infer_type: Range,

    cannot_pop_empty_stack: Range,
    stack_nonempty: struct {
        got: *elab.TypeStack,
        start: u32,
        end: u32,
    },
    expected_function: struct {
        got: types.Type,
        start: u32,
        end: u32,
    },
    expected_type_on_stack: struct {
        want: []const types.Type,
        start: u32,
        end: u32,
    },

    // OTHER ERRORS
    too_many_errors,

    pub fn range(start: u32, end: u32) Range {
        return Range{ .start = start, .end = end };
    }
};

const MAX_ERRORS = 10;
var ERROR_BUF: [MAX_ERRORS]Error = undefined;

// This same error struct is threaded through everything that might need it
// and then at each stage where errors might occur at the root level,
// the contents are printed to the user.
pub const Errors = struct {
    errors: []Error = ERROR_BUF[0..0],

    pub fn addError(self: *@This(), err: Error) void {
        self.errors.len += 1;
        if (self.errors.len == MAX_ERRORS) {
            self.errors[self.errors.len - 1] = Error{ .too_many_errors = {} };
        } else {
            self.errors[self.errors.len - 1] = err;
        }
    }
};

pub fn reportErrors(errors: *Errors, source: [:0]const u8) !void {
    var bufferedWriter = std.io.bufferedWriter(std.io.getStdErr().writer());
    var writer = bufferedWriter.writer();
    for (errors.errors) |err| {
        var location: ?u32 = null;
        var endlocation: ?u32 = null;
        switch (err) {
            .expected_but_found => |data| {
                try writer.print("Expected '{}' but found '{}'\n", .{ data.expected, data.found.tk });
                location = data.found.start;
                endlocation = data.found.end;
            },
            .unexpected_token => |token| {
                try writer.print("Unexpected token '{}'\n", .{token.tk});
                location = token.start;
                endlocation = token.end;
            },
            .invalid_str => |token| {
                try writer.print("Invalid or unclosed string literal\n", .{});
                location = token.start;
                endlocation = token.end;
            },

            .expected_type_operator_name => |range| {
                try writer.print("Expected a name not type expression.\n", .{});
                location = range.start;
                endlocation = range.end;
            },

            .unknown_variable => |range| {
                try writer.print("Unknown variable '{s}'\n", .{range.toString(source)});
                location = range.start;
                endlocation = range.end;
            },
            .invalid_stmt => |stmt| {
                try writer.print("Invalid statement\n", .{});
                location = stmt.start;
                endlocation = stmt.end;
            },
            .unification_error => |data| {
                try writer.print("Cannot unify '{}' with '{}'\n", .{ data.ty1, data.ty2 });
                location = data.start;
                endlocation = data.end;
            },
            .failed_to_unify_stacks => |data| {
                if (data.stack1) |s1| {
                    if (data.stack2) |s2| {
                        try writer.print("Cannot unify stack {} with {}\n", .{ s1, s2 });
                    } else {
                        try writer.print("Cannot unify stack {} with []\n", .{s1});
                    }
                } else {
                    if (data.stack1) |s2| {
                        try writer.print("Cannot unify stack [] with {}\n", .{s2});
                    } else {
                        try writer.print("Cannot unify stack [] with []\n", .{});
                    }
                }
                location = data.start;
                endlocation = data.start + 1;
            },
            .too_many_variables => |token| {
                try writer.print("Too many variables, cannot declare '{s}'\n", .{token.toString(source)});
                location = token.start;
                endlocation = token.end;
            },
            .variable_already_exists => |token| {
                try writer.print("Variable already exists '{s}'\n", .{token.toString(source)});
                location = token.start;
                endlocation = token.end;
            },
            .too_many_errors => try writer.print("Too many errors, stopping...\n", .{}),
            .cannot_assign_to_const => |token| {
                try writer.print("Cannot assign to constant variable '{s}'\n", .{token.toString(source)});
                location = token.start;
                endlocation = token.end;
            },
            .too_many_nested_functions => |token| {
                try writer.print("Too many nested functions, cannot declare {s}.\n", .{token.toString(source)});
                location = token.start;
                endlocation = token.end;
            },
            .unreachable_statement => |range| {
                try writer.print("Unreachable Statement.\n", .{});
                location = range.start;
                endlocation = range.end;
            },
            .cannot_return_from_global_scope => |range| {
                try writer.print("Cannot return from global scope.\n", .{});
                location = range.start;
                endlocation = range.end;
            },
            .cannot_infer_type => |range| {
                try writer.print("Cannot infer type.\n", .{});
                location = range.start;
                endlocation = range.end;
            },

            .cannot_pop_empty_stack => |range| {
                try writer.print("Attempting to pop empty stack\n", .{});
                location = range.start;
                endlocation = range.end;
            },
            .stack_nonempty => |range| {
                try writer.print("Stack contains more items than expected {}\n", .{range.got});
                location = range.start;
                endlocation = range.end;
            },
            .expected_function => |data| {
                try writer.print("Expected a function type but found '{}'\n", .{data.got});
                location = data.start;
                endlocation = data.end;
            },
            .expected_type_on_stack => |data| {
                try writer.print("Expected {any} on stack but was empty.\n", .{data.want});
                location = data.start;
                endlocation = data.end;
            },
        }

        if (location) |loc| {
            const line = getFullLine(loc, source);
            const position = indexToSourceLoc(loc, source);
            if (position.line > 1) {
                const prevline = getFullLine(loc - position.col - 1, source);
                try writer.print("     |\n{d: >4} | {s}\n{d: >4} | {s}\n     |", .{ position.line - 1, prevline, position.line, line });
            } else {
                try writer.print("     |\n{d: >4} | {s}\n     |", .{ position.line, line });
            }
            try writer.writeByteNTimes(' ', position.col);
            try writer.writeAll(cols.err);
            try writer.writeByteNTimes('^', (endlocation orelse loc + 1) - loc);
            try writer.writeAll(cols.end ++ "\n\n");
        }
    }
    try bufferedWriter.flush();
}

const SoureLoc = struct {
    line: u32,
    col: u32,
};

pub fn indexToSourceLoc(index: u32, source: [:0]const u8) SoureLoc {
    var lines: u32 = 0;
    var line_start_index: u32 = 0;
    for (source[0..index]) |c, i| {
        if (c == '\n') {
            lines += 1;
            line_start_index = @intCast(u32, i) + 1;
        }
    }
    return SoureLoc{
        .line = lines + 1,
        .col = index - line_start_index + 1,
    };
}

pub fn lineStartForIndex(index: u32, source: [:0]const u8) u32 {
    var i = index;
    while (i > 0) {
        if (source[i] == '\n') {
            i += 1;
            break;
        }
        i -= 1;
    }

    return i;
}

// Does not include the newline character
pub fn lineEndForIndex(index: u32, source: [:0]const u8) u32 {
    var i = index;
    while (i < source.len) {
        if (source[i] == '\n') {
            break;
        }
        i += 1;
    }

    return i;
}

pub fn getFullLine(index: u32, source: [:0]const u8) []const u8 {
    const start = lineStartForIndex(index, source);
    const end = lineEndForIndex(index, source);
    if (start >= end) return &.{};
    return source[start..end];
}
