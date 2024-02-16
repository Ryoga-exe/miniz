const std = @import("std");
const Expression = @import("ast.zig").Expression;
const Operator = @import("operator.zig").Operator;
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const Allocator = std.mem.Allocator;

pub const Parser = struct {
    const Self = @This();
    const Error = error{ OutOfMemory, Overflow, InvalidCharacter };
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
    pub fn parseProgram(self: *Self) Error!*Expression {
        return self.parseExpr(0);
    }
    fn parseExpr(self: *Self, precedence: u8) Error!*Expression {
        var leading = try self.parsePrefix();
        while (true) {
            switch (self.currentToken.type) {
                .eof => return leading,
                else => |c| {
                    if (Operator.fromTokenType(c).infixPrecedence()) |next| {
                        if (next[0] <= precedence) {
                            return leading;
                        }
                    }
                },
            }
            switch (self.currentToken.type) {
                .plus => leading = try self.parseInfixOnce(Operator.plus, leading),
                .minus => leading = try self.parseInfixOnce(Operator.minus, leading),
                .asterisk => leading = try self.parseInfixOnce(Operator.asterisk, leading),
                .slash => leading = try self.parseInfixOnce(Operator.slash, leading),
                .percent => leading = try self.parseInfixOnce(Operator.percent, leading),
                .assign => {
                    switch (leading.*) {
                        .identifier => {},
                        else => unreachable, // expected identifier
                    }
                    leading = try self.parseInfixOnce(Operator.assign, leading);
                },
                else => return leading,
            }
        }
    }
    fn parseAtom(self: *Self) Error!*Expression {
        switch (self.currentToken.type) {
            .integer => {
                const value = try std.fmt.parseInt(i64, self.currentToken.literal, 10);
                const result = try Expression.createInteger(self.allocator, value);
                self.nextToken();
                return result;
            },
            .identifier => {
                const result = try Expression.createIdentifier(self.allocator, self.currentToken.literal);
                self.nextToken();
                return result;
            },
            else => {
                unreachable; // expected literal or identifier
            },
        }
    }
    fn parsePrefix(self: *Self) Error!*Expression {
        return switch (self.currentToken.type) {
            .plus => self.parsePrefixOnce(Operator.plus),
            .minus => self.parsePrefixOnce(Operator.minus),
            .lparen => self.parsePrefixParen(),
            else => try self.parseAtom(),
        };
    }
    fn parsePrefixOnce(self: *Self, operator: Operator) Error!*Expression {
        self.nextToken();
        const following = try self.parseExpr(operator.prefixPrecedence().?);
        return try Expression.createUnaryExpression(self.allocator, operator, following);
    }
    fn parsePrefixParen(self: *Self) Error!*Expression {
        self.nextToken();
        const following = try self.parseExpr(Operator.paren.prefixPrecedence().?);
        if (self.currentToken.type != .rparen) {
            unreachable; // expected ')', found self.currentToken.type
        }
        self.nextToken();
        return try Expression.createUnaryExpression(self.allocator, .paren, following);
    }
    fn parseInfixOnce(self: *Self, operator: Operator, leading: *Expression) Error!*Expression {
        self.nextToken();
        const following = try self.parseExpr(operator.infixPrecedence().?[1]);
        return try Expression.createBinaryExpression(self.allocator, operator, leading, following);
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
