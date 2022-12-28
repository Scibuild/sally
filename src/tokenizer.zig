const std = @import("std");
pub const TokenClass = enum {
    // keywords
    tk_fn,
    tk_return,
    tk_if,
    tk_else,
    tk_while,
    tk_struct,
    tk_import,
    tk_const,
    tk_true,
    tk_false,

    tk_semi,
    tk_colon,
    tk_obrace,
    tk_cbrace,
    tk_oparen,
    tk_cparen,
    tk_equal,
    tk_arrow,
    tk_minus_minus,
    tk_dollar,

    // other
    tk_ident,
    tk_dot_ident,
    tk_punct_ident,
    tk_float_lit,
    tk_int_lit,
    tk_string_lit,

    tk_eof,
    tk_invalid_str,
};

pub const Token = struct {
    tk: TokenClass,
    start: u32,
    end: u32,

    pub fn toString(self: @This(), source: [:0]const u8) []const u8 {
        return source[self.start..self.end];
    }
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: u32,

    const keywordMap = std.ComptimeStringMap(TokenClass, .{
        .{ "fn", .tk_fn },
        .{ "if", .tk_if },
        .{ "else", .tk_else },
        .{ "while", .tk_while },
        .{ "struct", .tk_struct },
        .{ "import", .tk_import },
        .{ "const", .tk_const },
        .{ "true", .tk_true },
        .{ "false", .tk_false },
        .{ "return", .tk_return },
        .{ "--", .tk_minus_minus },
        .{ "->", .tk_arrow },
    });

    pub fn init(file_contents: [:0]const u8) @This() {
        return Tokenizer{
            .buffer = file_contents,
            .index = 0,
        };
    }

    pub fn next(self: *@This()) Token {
        while (true) {
            switch (self.buffer[self.index]) {
                ' ', '\t', '\n' => self.index += 1,
                '#' => {
                    while (self.buffer[self.index] != '\n') {
                        self.index += 1;
                    }
                },
                else => break,
            }
        }
        const tk_start = self.index;
        const token: TokenClass = switch (self.buffer[self.index]) {
            0 => .tk_eof,
            ';' => self.tokenize1(.tk_semi),
            ':' => self.tokenize1(.tk_colon),
            '{' => self.tokenize1(.tk_obrace),
            '}' => self.tokenize1(.tk_cbrace),
            '(' => self.tokenize1(.tk_oparen),
            ')' => self.tokenize1(.tk_cparen),
            '$' => self.tokenize1(.tk_dollar),

            '\'', '"' => brk: {
                const closing = self.buffer[self.index];
                self.index += 1;
                var is_escaping = false;
                var c = self.buffer[self.index];
                while (!(c == closing or c == '\n') or is_escaping) {
                    if (is_escaping) {
                        if (!(c == 'n' or c == 't' or c == '\\' or c == 'r' or c == '\'' or c == '"'))
                            break :brk TokenClass.tk_invalid_str;
                        is_escaping = false;
                    } else if (c == '\\') {
                        is_escaping = true;
                    } else if (c == 0 or c == '\n') {
                        break :brk TokenClass.tk_invalid_str;
                    }
                    self.index += 1;
                    c = self.buffer[self.index];
                }
                self.index += 1;

                break :brk .tk_string_lit;
            },

            '0'...'9' => brk: {
                while (self.buffer[self.index] >= '0' and self.buffer[self.index] <= '9')
                    self.index += 1;
                if (self.buffer[self.index] == '.') {
                    self.index += 1;
                    while (self.buffer[self.index] >= '0' and self.buffer[self.index] <= '9')
                        self.index += 1;
                    break :brk TokenClass.tk_float_lit;
                }
                break :brk TokenClass.tk_int_lit;
            },

            else => brk: {
                const terminating_chars = "\".;(){}\x00 \n\t";
                while (true) {
                    self.index += 1;
                    const c = self.buffer[self.index];
                    // maybe sparse array?
                    for (terminating_chars) |tc| {
                        if (c == tc) {
                            var tok_str = self.buffer[tk_start..self.index];
                            if (tok_str[0] == '.') {
                                break :brk .tk_dot_ident;
                            } else if (keywordMap.get(tok_str)) |kw| {
                                break :brk kw;
                            } else if (is_all_punct(tok_str)) {
                                break :brk .tk_punct_ident;
                            } else {
                                break :brk .tk_ident;
                            }
                        }
                    }
                }
            },
        };

        return Token{ .tk = token, .start = tk_start, .end = self.index };
    }

    fn is_all_punct(string: []const u8) bool {
        const punct = "\'`~!@$%^&*-+=[]|\\:,<.>/?";
        for (string) |c| {
            var is_punct = false;
            for (punct) |p| {
                if (c == p) {
                    is_punct = true;
                }
            }
            if (is_punct == false) return false;
        }
        return true;
    }

    fn tokenize1(self: *@This(), tk: TokenClass) TokenClass {
        self.index += 1;
        return tk;
    }
};
