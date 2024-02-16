const std = @import("std");
const TokenType = @import("token.zig").TokenType;

pub const Operator = enum {
    const Self = @This();

    plus,
    minus,
    asterisk,
    slash,
    paren,
    none,

    pub fn prefixPrecedence(self: Self) ?u8 {
        return switch (self) {
            .plus => 51,
            .minus => 51,
            .paren => 0,
            else => null,
        };
    }
    pub fn infixPrecedence(self: Self) ?std.meta.Tuple(&.{ u8, u8 }) {
        return switch (self) {
            .plus => .{ 50, 51 },
            .minus => .{ 50, 51 },
            .asterisk => .{ 80, 81 },
            else => null,
        };
    }
    pub fn fromTokenType(token_type: TokenType) Self {
        return switch (token_type) {
            .plus => .plus,
            .minus => .minus,
            .asterisk => .asterisk,
            .slash => .slash,
            .lparen => .paren,
            .rparen => .paren,
            else => .none,
        };
    }
    pub fn lookupOperator(operator: []const u8) Self {
        const map = std.ComptimeStringMap(Self, .{
            .{ "+", .plus },
            .{ "-", .minus },
            .{ "*", .asterisk },
            .{ "/", .slash },
            .{ "(", .paren },
            .{ ")", .paren },
        });
        if (map.get(operator)) |ope| {
            return ope;
        }
        return .none;
    }
    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .plus => "+",
            .minus => "-",
            .asterisk => "*",
            .slash => "/",
            .paren => "paren",
            else => "",
        };
    }
};

test "Precedence" {
    try std.testing.expect(Operator.plus.prefixPrecedence().? == 51);
    try std.testing.expect(Operator.asterisk.prefixPrecedence() == null);

    try std.testing.expect(Operator.plus.infixPrecedence().?[0] == 50);
    try std.testing.expect(Operator.paren.infixPrecedence() == null);
}
