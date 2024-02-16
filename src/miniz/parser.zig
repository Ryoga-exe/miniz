const std = @import("std");
const SExpr = @import("ast.zig").SExpr;
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
    pub fn parseProgram(self: *Self) !*SExpr {
        return self.parseExpr(0);
    }
    fn parseExpr(self: *Self, precedence: u8) !*SExpr {
        var leading = try switch (self.currentToken.type) {
            .plus => blk: {
                const pos_precedence: u8 = 51;
                self.nextToken();
                const following = try self.parseExpr(pos_precedence);
                break :blk try SExpr.createList(self.allocator, &[_]*SExpr{ try SExpr.createAtom(self.allocator, "+"), following });
            },
            .minus => blk: {
                const neg_precedence: u8 = 51;
                self.nextToken();
                const following = try self.parseExpr(neg_precedence);
                break :blk try SExpr.createList(self.allocator, &[_]*SExpr{ try SExpr.createAtom(self.allocator, "-"), following });
            },
            .lparen => blk: {
                self.nextToken();
                const following = try self.parseExpr(0);
                if (self.currentToken.type != .rparen) {
                    unreachable;
                }
                self.nextToken();

                break :blk try SExpr.createList(self.allocator, &[_]*SExpr{ try SExpr.createAtom(self.allocator, "paren"), following });
            },
            else => self.parseAtom(),
        };
        while (true) {
            switch (self.currentToken.type) {
                .eof => return leading,
                else => |c| {
                    const next_precedence: ?u8 = switch (c) {
                        .plus => 50,
                        .minus => 50,
                        .asterisk => 80,
                        else => null,
                    };
                    if (next_precedence) |next| {
                        if (next <= precedence) {
                            return leading;
                        }
                    }
                },
            }
            switch (self.currentToken.type) {
                .plus => {
                    const plus_presedence: u8 = 51;
                    self.nextToken();
                    const following = try self.parseExpr(plus_presedence);
                    leading = try SExpr.createList(self.allocator, &[_]*SExpr{ try SExpr.createAtom(self.allocator, "+"), leading, following });
                },
                .minus => {
                    const minus_presedence: u8 = 51;
                    self.nextToken();
                    const following = try self.parseExpr(minus_presedence);
                    leading = try SExpr.createList(self.allocator, &[_]*SExpr{ try SExpr.createAtom(self.allocator, "-"), leading, following });
                },
                .asterisk => {
                    const asterisk_presedence: u8 = 81;
                    self.nextToken();
                    const following = try self.parseExpr(asterisk_presedence);
                    leading = try SExpr.createList(self.allocator, &[_]*SExpr{ try SExpr.createAtom(self.allocator, "*"), leading, following });
                },
                else => return leading,
            }
        }
    }
    fn parseAtom(self: *Self) !*SExpr {
        switch (self.currentToken.type) {
            .integer => {
                const result = try SExpr.createAtom(self.allocator, self.currentToken.literal);
                self.nextToken();
                return result;
            },
            else => {
                unreachable;
            },
        }
    }
    fn parseExprBp(self: *Self) SExpr {
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

test "parse: +1234" {
    const alloc = std.testing.allocator;
    const input = "+1234";
    var lexer = Lexer.init(input);
    var parser = Parser.init(alloc, &lexer);
    defer parser.deinit();
    const e = try parser.parseProgram();
    defer e.deinit(alloc);
    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "(+ 1234)", str);
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

test "parse: 1234 + 5678" {
    const alloc = std.testing.allocator;
    const input = "1234 + 5678";
    var lexer = Lexer.init(input);
    var parser = Parser.init(alloc, &lexer);
    defer parser.deinit();
    const e = try parser.parseProgram();
    defer e.deinit(alloc);
    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "(+ 1234 5678)", str);
}

test "parse: 1234 + -5678" {
    const alloc = std.testing.allocator;
    const input = "1234 + -5678";
    var lexer = Lexer.init(input);
    var parser = Parser.init(alloc, &lexer);
    defer parser.deinit();
    const e = try parser.parseProgram();
    defer e.deinit(alloc);
    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "(+ 1234 (- 5678))", str);
}

test "parse: 1234--5678" {
    const alloc = std.testing.allocator;
    const input = "1234--5678";
    var lexer = Lexer.init(input);
    var parser = Parser.init(alloc, &lexer);
    defer parser.deinit();
    const e = try parser.parseProgram();
    defer e.deinit(alloc);
    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "(- 1234 (- 5678))", str);
}

test "parse: 1 + 2 + 3" {
    const alloc = std.testing.allocator;
    const input = "1 + 2 + 3";
    var lexer = Lexer.init(input);
    var parser = Parser.init(alloc, &lexer);
    defer parser.deinit();
    const e = try parser.parseProgram();
    defer e.deinit(alloc);
    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "(+ (+ 1 2) 3)", str);
}

test "parse: 1 + 2 * 3 + 4" {
    const alloc = std.testing.allocator;
    const input = "1 + 2 * 3 + 4";
    var lexer = Lexer.init(input);
    var parser = Parser.init(alloc, &lexer);
    defer parser.deinit();
    const e = try parser.parseProgram();
    defer e.deinit(alloc);
    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "(+ (+ 1 (* 2 3)) 4)", str);
}
