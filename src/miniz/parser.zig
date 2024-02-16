const std = @import("std");
const ast = @import("ast.zig");
const Expression = ast.Expression;
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
    pub fn parseProgram(self: *Self) Error!*ast.Program {
        var program = try ast.Program.init(self.allocator);
        while (self.currentToken.type != .eof) {
            const statement = try self.parseStatement();
            try program.statements.append(statement);
            self.nextToken();
        }
        return program;
    }
    fn parseStatement(self: *Self) !*ast.Statement {
        return switch (self.currentToken.type) {
            .keyword_return => self.parseReturnStatement(),
            else => self.parseExpressionStatement(),
        };
    }
    fn parseReturnStatement(self: *Self) !*ast.Statement {
        self.nextToken();
        const expr = try self.parseExpr(0);
        if (self.peekToken.type == .semicolon) {
            self.nextToken();
        }
        return ast.Statement.createReturnStatement(self.allocator, expr);
    }
    fn parseExpressionStatement(self: *Self) !*ast.Statement {
        const expr = try self.parseExpr(0);
        if (self.peekToken.type == .semicolon) {
            self.nextToken();
        }
        return ast.Statement.createExpressionStatement(self.allocator, expr);
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
                .mod => leading = try self.parseInfixOnce(Operator.mod, leading),
                .rem => leading = try self.parseInfixOnce(Operator.rem, leading),
                .lt => leading = try self.parseInfixOnce(Operator.lt, leading),
                .gt => leading = try self.parseInfixOnce(Operator.gt, leading),
                .lt_eq => leading = try self.parseInfixOnce(Operator.lt_eq, leading),
                .gt_eq => leading = try self.parseInfixOnce(Operator.gt_eq, leading),
                .eq => leading = try self.parseInfixOnce(Operator.eq, leading),
                .not_eq => leading = try self.parseInfixOnce(Operator.not_eq, leading),
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
            .keyword_if => self.parsePrefixIf(),
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
            unreachable; // expected ')', found {self.currentToken.type}
        }
        self.nextToken();
        return try Expression.createUnaryExpression(self.allocator, .paren, following);
    }
    fn parsePrefixIf(self: *Self) Error!*Expression {
        self.nextToken();
        if (self.currentToken.type != .lparen) {
            unreachable; // expected '(' after if, found {self.currentToken.type}
        }
        self.nextToken();
        const condition = try self.parseExpr(0);
        if (self.currentToken.type != .rparen) {
            unreachable; // expected ')', found {self.currentToken.type}
        }
        self.nextToken();

        const consequence = try self.parseStatement();

        var alternative: ?*ast.Statement = null;
        if (self.peekToken.type == .keyword_else) {
            self.nextToken();
            self.nextToken();
            alternative = try self.parseStatement();
        }
        return try Expression.createIfExpression(self.allocator, condition, consequence, alternative);
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
    const program = try parser.parseProgram();
    defer program.deinit(allocator);
    const str = try program.toString(allocator);
    errdefer allocator.free(str);

    return str;
}

test "parseProgram" {
    const alloc = std.testing.allocator;
    const tests = [_]struct {
        input: []const u8,
        expect: []const u8,
    }{
        .{ .input = "return 123 + 234;", .expect = "(return (+ 123 234))" },
        .{ .input = "1234;", .expect = "(expr 1234)" },
        .{ .input = "+1234;", .expect = "(expr (+ 1234))" },
        .{ .input = "-1234;", .expect = "(expr (- 1234))" },
        .{ .input = "1234 + 5678;", .expect = "(expr (+ 1234 5678))" },
        .{ .input = "1234 + -5678;", .expect = "(expr (+ 1234 (- 5678)))" },
        .{ .input = "1234--5678;", .expect = "(expr (- 1234 (- 5678)))" },
        .{ .input = "1 + 2 + 3;", .expect = "(expr (+ (+ 1 2) 3))" },
        .{ .input = "1 + 2 * 3 + 4;", .expect = "(expr (+ (+ 1 (* 2 3)) 4))" },
        .{ .input = "-100 %% 100 + - 10 % 10 * 20", .expect = "(expr (+ (%% (- 100) 100) (* (% (- 10) 10) 20)))" },
        .{ .input = "mod = 998244353", .expect = "(expr (= mod 998244353))" },
        .{ .input = "foobar = 1 + 3 + 5", .expect = "(expr (= foobar (+ (+ 1 3) 5)))" },
        .{ .input = "if (x < y) 2; else 3;", .expect = "(expr (if (< x y) (expr 2) (expr 3)))" },
        .{ .input = "if (x + 1 < y + 4) 55;", .expect = "(expr (if (< (+ x 1) (+ y 4)) (expr 55)))" },
    };
    for (tests) |t| {
        const result = try ParseTesting(alloc, t.input);
        defer alloc.free(result);

        try std.testing.expectEqualSlices(u8, t.expect, result);
    }
}
