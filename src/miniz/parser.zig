const std = @import("std");
const Expression = @import("ast.zig").Expression;
const Operator = @import("operator.zig").Operator;
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const Allocator = std.mem.Allocator;

pub const Parser = struct {
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
    pub fn parseProgram(self: *Self) !*Expression {
        return self.parseExpr(0);
    }
    fn parseExpr(self: *Self, precedence: u8) !*Expression {
        var leading = try switch (self.currentToken.type) {
            .plus => blk: {
                const pos_precedence: u8 = 51;
                self.nextToken();
                const following = try self.parseExpr(pos_precedence);
                break :blk try Expression.createUnaryExpression(self.allocator, .plus, following);
            },
            .minus => blk: {
                const neg_precedence: u8 = 51;
                self.nextToken();
                const following = try self.parseExpr(neg_precedence);
                break :blk try Expression.createUnaryExpression(self.allocator, .minus, following);
            },
            .lparen => blk: {
                self.nextToken();
                const following = try self.parseExpr(0);
                if (self.currentToken.type != .rparen) {
                    unreachable;
                }
                self.nextToken();

                break :blk try Expression.createUnaryExpression(self.allocator, .paren, following);
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
                        .slash => 80,
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
                    leading = try Expression.createBinaryExpression(self.allocator, .plus, leading, following);
                },
                .minus => {
                    const minus_presedence: u8 = 51;
                    self.nextToken();
                    const following = try self.parseExpr(minus_presedence);
                    leading = try Expression.createBinaryExpression(self.allocator, .minus, leading, following);
                },
                .asterisk => {
                    const asterisk_presedence: u8 = 81;
                    self.nextToken();
                    const following = try self.parseExpr(asterisk_presedence);
                    leading = try Expression.createBinaryExpression(self.allocator, .asterisk, leading, following);
                },
                .slash => {
                    const slash_presedence: u8 = 81;
                    self.nextToken();
                    const following = try self.parseExpr(slash_presedence);
                    leading = try Expression.createBinaryExpression(self.allocator, .slash, leading, following);
                },
                else => return leading,
            }
        }
    }
    fn parseAtom(self: *Self) !*Expression {
        switch (self.currentToken.type) {
            .integer => {
                const value = try std.fmt.parseInt(i64, self.currentToken.literal, 10);
                const result = try Expression.createInteger(self.allocator, value);
                defer self.nextToken();
                return result;
            },
            else => {
                unreachable;
            },
        }
    }
    fn nextToken(self: *Self) void {
        self.currentToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }
};

fn ParseTesting(allocator: Allocator, input: []const u8) ![]const u8 {
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();
    const parse_result = try parser.parseProgram();
    defer parse_result.deinit(allocator);
    const str = try parse_result.toString(allocator);
    errdefer allocator.free(str);

    return str;
}

test "parse: 1234" {
    const alloc = std.testing.allocator;
    const result = try ParseTesting(alloc, "1234");
    defer alloc.free(result);
    try std.testing.expectEqualSlices(u8, "1234", result);
}

test "parse: +1234" {
    const alloc = std.testing.allocator;
    const result = try ParseTesting(alloc, "+1234");
    defer alloc.free(result);
    try std.testing.expectEqualSlices(u8, "(+ 1234)", result);
}

test "parse: -1234" {
    const alloc = std.testing.allocator;
    const result = try ParseTesting(alloc, "-1234");
    defer alloc.free(result);
    try std.testing.expectEqualSlices(u8, "(- 1234)", result);
}

test "parse (-1234)" {
    const alloc = std.testing.allocator;
    const result = try ParseTesting(alloc, "(-1234)");
    defer alloc.free(result);
    try std.testing.expectEqualSlices(u8, "(paren (- 1234))", result);
}

test "parse: 1234 + 5678" {
    const alloc = std.testing.allocator;
    const result = try ParseTesting(alloc, "1234 + 5678");
    defer alloc.free(result);
    try std.testing.expectEqualSlices(u8, "(+ 1234 5678)", result);
}

test "parse: 1234 + -5678" {
    const alloc = std.testing.allocator;
    const result = try ParseTesting(alloc, "1234 + -5678");
    defer alloc.free(result);
    try std.testing.expectEqualSlices(u8, "(+ 1234 (- 5678))", result);
}

test "parse: 1234--5678" {
    const alloc = std.testing.allocator;
    const result = try ParseTesting(alloc, "1234--5678");
    defer alloc.free(result);
    try std.testing.expectEqualSlices(u8, "(- 1234 (- 5678))", result);
}

test "parse: 1 + 2 + 3" {
    const alloc = std.testing.allocator;
    const result = try ParseTesting(alloc, "1 + 2 + 3");
    defer alloc.free(result);
    try std.testing.expectEqualSlices(u8, "(+ (+ 1 2) 3)", result);
}

test "parse: 1 + 2 * 3 + 4" {
    const alloc = std.testing.allocator;
    const result = try ParseTesting(alloc, "1 + 2 * 3 + 4");
    defer alloc.free(result);
    try std.testing.expectEqualSlices(u8, "(+ (+ 1 (* 2 3)) 4)", result);
}
