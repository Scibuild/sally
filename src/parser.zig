const std = @import("std");
const tk = @import("tokenizer.zig");
const ast = @import("ast.zig");
const errs = @import("errors.zig");

const Writer = std.io.Writer;
const S = ast.StmtNode.Stmt;
const T = ast.TypeNode.TypeExpr;

pub const Parser = struct {
    buffer: [:0]const u8,
    arena: std.heap.ArenaAllocator,
    tokenizer: tk.Tokenizer,

    token: tk.Token,

    errors: *errs.Errors,

    const ParseError = error{
        ParseUnsuccessful,
    } || std.mem.Allocator.Error || std.fmt.ParseIntError || std.fmt.ParseFloatError;

    const Self = *@This();

    pub fn init(buffer: [:0]const u8, errors: *errs.Errors) Parser {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        var parser = Parser{
            .buffer = buffer,
            .tokenizer = tk.Tokenizer.init(buffer),
            .arena = arena,
            .token = undefined,
            .errors = errors,
        };
        parser.advance();
        return parser;
    }

    pub fn deinit(self: Self) void {
        self.errors.deinit();
        self.arena.deinit();
    }

    // Simple Utility functions

    pub fn createStmt(self: Self) ParseError!*ast.StmtNode {
        var stmt = try self.arena.allocator().create(ast.StmtNode);
        stmt.start = self.token.start;
        return stmt;
    }

    pub fn createStmtToken(self: Self) ParseError!*ast.StmtNode {
        var stmt = try self.arena.allocator().create(ast.StmtNode);
        stmt.start = self.token.start;
        stmt.end = self.token.end;
        return stmt;
    }

    pub fn createType(self: Self) ParseError!*ast.TypeNode {
        var ty = try self.arena.allocator().create(ast.TypeNode);
        return ty;
    }

    pub fn parseFloatLit(self: Self) ParseError!*ast.StmtNode {
        var stmt = try self.createStmtToken();
        const value = try std.fmt.parseFloat(f64, self.buffer[self.token.start..self.token.end]);
        stmt.s = S{ .float_lit = value };
        self.advance();
        return stmt;
    }

    pub fn parseIntLit(self: Self) ParseError!*ast.StmtNode {
        var stmt = try self.createStmtToken();
        const value = try std.fmt.parseInt(i64, self.buffer[self.token.start..self.token.end], 10);
        stmt.s = S{ .int_lit = value };
        self.advance();
        return stmt;
    }

    pub fn parseStmtsUntil(self: Self, token: tk.TokenClass) ParseError![]*ast.StmtNode {
        var statments = std.ArrayList(*ast.StmtNode).init(self.arena.allocator());

        while (!self.check(token) and !self.check(.tk_eof)) {
            const substmt = try self.parseStmt();
            try statments.append(substmt);
        }
        return statments.toOwnedSlice();
    }

    pub fn parseIfStmt(self: Self) ParseError!*ast.StmtNode {
        const stmt = try self.createStmt();
        try self.expect(.tk_if);

        const cond = try self.parseStmtsUntil(.tk_obrace);
        try self.expect(.tk_obrace);
        const if_branch = try self.parseStmtsUntil(.tk_cbrace);
        const end_of_if_branch = self.token.end;
        try self.expect(.tk_cbrace);
        var else_branch: []*ast.StmtNode = &.{};
        if (self.match(.tk_else)) {
            if (self.check(.tk_if)) {
                const elseif = try self.parseIfStmt();
                else_branch = try self.arena.allocator().alloc(*ast.StmtNode, 1);
                else_branch[0] = elseif;
                stmt.end = else_branch[0].end;
            } else {
                try self.expect(.tk_obrace);
                else_branch = try self.parseStmtsUntil(.tk_cbrace);
                stmt.end = self.token.end;
                try self.expect(.tk_cbrace);
            }
        } else {
            stmt.end = end_of_if_branch;
        }
        stmt.s = S{ .if_stmt = .{
            .condition = cond,
            .if_body = if_branch,
            .else_body = else_branch,
        } };
        return stmt;
    }

    pub fn parseWhileStmt(self: Self) ParseError!*ast.StmtNode {
        var stmt = try self.createStmt();
        try self.expect(.tk_while);

        var cond = try self.parseStmtsUntil(.tk_obrace);
        try self.expect(.tk_obrace);
        var loop_body = try self.parseStmtsUntil(.tk_cbrace);
        stmt.end = self.token.end;
        try self.expect(.tk_cbrace);
        stmt.s = S{ .while_stmt = .{
            .condition = cond,
            .body = loop_body,
        } };
        return stmt;
    }

    pub fn parseStmt(self: Self) ParseError!*ast.StmtNode {
        switch (self.token.tk) {
            .tk_if => {
                return self.parseIfStmt();
            },
            .tk_while => {
                return self.parseWhileStmt();
            },
            .tk_true, .tk_false => {
                var stmt = try self.createStmtToken();
                stmt.s = S{
                    .bool_lit = self.check(.tk_true),
                };
                self.advance();
                return stmt;
            },
            .tk_float_lit => {
                return self.parseFloatLit();
            },
            .tk_int_lit => {
                return self.parseIntLit();
            },
            .tk_string_lit => {
                var stmt = try self.createStmtToken();
                stmt.s = S{ .string_lit = self.buffer[self.token.start + 1 .. self.token.end - 1] };
                self.advance();
                return stmt;
            },
            .tk_ident => {
                const stmt = try self.createStmtToken();
                stmt.s = S{ .ident = {} };
                self.advance();
                return stmt;
            },
            .tk_punct_ident => {
                const stmt = try self.createStmtToken();
                stmt.s = S{ .punct_ident = {} };
                self.advance();
                return stmt;
            },
            .tk_dot_ident => {
                const stmt = try self.createStmtToken();
                stmt.s = S{ .dot_ident = {} };
                self.advance();
                return stmt;
            },
            .tk_arrow => {
                var stmt = try self.createStmt();
                try self.expect(.tk_arrow);
                stmt.s = S{ .assign_stmt = self.token };
                try self.expect(.tk_ident);
                return stmt;
            },
            .tk_fn => {
                var stmt = try self.createStmt();
                self.advance();

                const fn_name = self.token;
                if (!(self.check(.tk_ident) or self.check(.tk_punct_ident)))
                    return self.unexpected();
                self.advance();
                try self.expect(.tk_colon);
                const ty = try self.parseTypeComposite();
                try self.expect(.tk_obrace);
                var body = try self.parseStmtsUntil(.tk_cbrace);
                stmt.end = self.token.end;
                try self.expect(.tk_cbrace);
                stmt.s = S{
                    .fn_decl = .{
                        .ty = ty,
                        .body = body,
                        .name = fn_name,
                    },
                };
                return stmt;
            },
            .tk_semi => {
                var stmt = try self.createStmtToken();
                self.advance();
                stmt.s = S{ .semi = {} };
                return stmt;
            },
            .tk_invalid_str => {
                self.errors.addError(errs.Error{
                    .invalid_str = self.token,
                });
                return error.ParseUnsuccessful;
            },
            else => {
                return self.unexpected();
            },
        }
    }

    pub fn parseTypeComposite(self: Self) ParseError!*ast.TypeNode {
        var ty = try self.createType();
        ty.start = self.token.start;

        if (self.check(.tk_minus_minus)) {
            self.advance();

            var tys = std.ArrayList(*ast.TypeNode).init(self.arena.allocator());
            while (!(self.check(.tk_cparen) or self.check(.tk_obrace) or self.check(.tk_eof))) {
                const nextty = try self.parseTypePrimary();
                try tys.append(nextty);
            }

            var tyslice = tys.toOwnedSlice();
            ty.ty = T{ .ty_func = .{
                .inputs = &.{},
                .output = tyslice,
            } };
            ty.end = self.token.start;
            return ty;
        }

        var first_ty = try self.parseTypePrimary();

        var tys = std.ArrayList(*ast.TypeNode).init(self.arena.allocator());
        var last_ty = first_ty;

        while (!(self.check(.tk_minus_minus) or self.check(.tk_cparen) or self.check(.tk_obrace) or self.check(.tk_eof))) {
            const nextty = try self.parseTypePrimary();
            try tys.append(last_ty);
            last_ty = nextty;
        }

        if (self.check(.tk_minus_minus)) {
            try tys.append(last_ty);
            self.advance();

            const inputs_len = tys.items.len;

            while (!(self.check(.tk_cparen) or self.check(.tk_obrace) or self.check(.tk_eof))) {
                const outty = try self.parseTypePrimary();
                try tys.append(outty);
            }

            var tyslice = tys.toOwnedSlice();

            ty.ty = T{ .ty_func = .{
                .inputs = tyslice[0..inputs_len],
                .output = tyslice[inputs_len..],
            } };

            ty.end = self.token.start;
        } else {
            if (tys.items.len == 0) {
                return first_ty;
            }

            var name: []const u8 = switch (last_ty.ty) {
                .ty_name => self.buffer[last_ty.start..last_ty.end],
                else => {
                    self.errors.addError(errs.Error{
                        .expected_type_operator_name = .{
                            .start = last_ty.start,
                            .end = last_ty.end,
                        },
                    });
                    return error.ParseUnsuccessful;
                },
            };

            ty.ty = T{ .ty_op = .{
                .name = name,
                .params = tys.toOwnedSlice(),
            } };
        }
        return ty;
    }

    pub fn parseTypePrimary(self: Self) ParseError!*ast.TypeNode {
        switch (self.token.tk) {
            .tk_ident => {
                var ty = try self.createType();
                ty.start = self.token.start;
                ty.end = self.token.end;
                ty.ty = T{ .ty_name = {} };
                self.advance();
                return ty;
            },
            .tk_oparen => {
                self.advance();
                var ty = try self.parseTypeComposite();
                try self.expect(.tk_cparen);
                return ty;
            },
            // the generic sort of identifiers, like ocamls 'a 'b
            // ocaml uses these for both unsolved metavars and also
            // generic parameters. What we will use them for is undecided
            .tk_dollar => {
                var ty = try self.createType();
                ty.start = self.token.start;

                self.advance();
                ty.end = self.token.end;
                const ident_tk = self.token;
                if (ident_tk.end - ident_tk.start != 1) {
                    return self.unexpected();
                }
                try self.expect(.tk_ident);
                ty.ty = T{ .ty_generic = self.buffer[ident_tk.start] };
                return ty;
            },
            else => {
                return self.unexpected();
            },
        }
    }

    pub fn parseProgram(self: Self) ParseError![]*ast.StmtNode {
        var statments = std.ArrayList(*ast.StmtNode).init(self.arena.allocator());
        while (!self.check(.tk_eof)) {
            const stmt = self.parseStmt() catch |e| {
                if (e == error.ParseUnsuccessful) {
                    self.synchronise();
                    continue;
                } else {
                    return e;
                }
            };
            try statments.append(stmt);
        }
        if (self.errors.errors.len != 0) {
            return error.ParseUnsuccessful;
        }
        return statments.toOwnedSlice();
    }

    pub inline fn advance(self: Self) void {
        self.token = self.tokenizer.next();
    }

    pub fn check(self: Self, tokenclass: tk.TokenClass) bool {
        return self.token.tk == tokenclass;
    }

    pub fn match(self: Self, tokenclass: tk.TokenClass) bool {
        if (self.token.tk == tokenclass) {
            self.advance();
            return true;
        }
        return false;
    }

    pub fn synchronise(self: Self) void {
        while (!(self.check(.tk_fn) or self.check(.tk_eof)))
            self.advance();
    }

    pub fn unexpected(self: Self) ParseError {
        self.errors.addError(errs.Error{
            .unexpected_token = self.token,
        });
        return error.ParseUnsuccessful;
    }

    pub fn expect(self: Self, tokenclass: tk.TokenClass) ParseError!void {
        if (self.token.tk == tokenclass) {
            self.advance();
        } else {
            self.errors.addError(errs.Error{
                .expected_but_found = .{
                    .expected = tokenclass,
                    .found = self.token,
                },
            });
            return error.ParseUnsuccessful;
        }
    }
};
