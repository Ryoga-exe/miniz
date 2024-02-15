const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const Allocator = std.mem.Allocator;

const Parser = struct {
    const Self = @This();
    allocator: Allocator,
    lexer: *Lexer,
    currentToken: Token,
    peekToken: Token,

    pub fn init(allocator: Allocator, lexer: *Lexer) Self {
        var p = Self{
            .allocator = allocator,
            .lexer = lexer,
            .currentToken = undefined,
            .peekToken = undefined,
        };
        p.nextToken();
        p.nextToken();
        return p;
    }
    pub fn deinit(self: *Self) void {
        _ = self;
    }
    pub fn parseProgram(self: *Self) !*ast.SExpr {
        return self.parseExpr();
    }
    fn parseExpr(self: *Self) !*ast.SExpr {
        switch (self.currentToken.type) {
            .minus => {
                self.nextToken();
                const following = try self.parseExpr();
                return try ast.SExpr.createList(self.allocator, &[_]*ast.SExpr{ try ast.SExpr.createAtom(self.allocator, "-"), following });
            },
            .lparen => {
                self.nextToken();
                const following = try self.parseExpr();
                if (self.currentToken.type != .rparen) {
                    unreachable;
                }
                self.nextToken();

                return try ast.SExpr.createList(self.allocator, &[_]*ast.SExpr{ try ast.SExpr.createAtom(self.allocator, "paren"), following });
            },
            else => {
                return self.parseAtom();
            },
        }
    }
    fn parseAtom(self: *Self) !*ast.SExpr {
        switch (self.currentToken.type) {
            .integer => {
                const result = try ast.SExpr.createAtom(self.allocator, self.currentToken.literal);
                self.nextToken();
                return result;
            },
            else => {
                unreachable;
            },
        }
    }
    fn parseExprBp(self: *Self) ast.SExpr {
        _ = self;
    }
    fn nextToken(self: *Self) void {
        self.currentToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }
};

test "parse: 1234" {
    const alloc = std.testing.allocator;
    const input = "1234";
    var lexer = Lexer.init(input);
    var parser = Parser.init(alloc, &lexer);
    defer parser.deinit();
    const e = try parser.parseProgram();
    defer e.deinit(alloc);
    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "1234", str);
}

test "parse: -1234" {
    const alloc = std.testing.allocator;
    const input = "-1234";
    var lexer = Lexer.init(input);
    var parser = Parser.init(alloc, &lexer);
    defer parser.deinit();
    const e = try parser.parseProgram();
    defer e.deinit(alloc);
    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "(- 1234)", str);
}

test "parse (-1234)" {
    const alloc = std.testing.allocator;
    const input = "(-1234)";
    var lexer = Lexer.init(input);
    var parser = Parser.init(alloc, &lexer);
    defer parser.deinit();
    const e = try parser.parseProgram();
    defer e.deinit(alloc);
    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "(paren (- 1234))", str);
}
