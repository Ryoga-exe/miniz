const std = @import("std");

pub const TokenType = enum {
    illegal,
    eof,

    identifier,
    integer,

    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    lt,
    gt,
    eq,
    not_eq,

    comma,
    semicolon,

    lparen,
    rparen,
    lbrace,
    rbrace,

    keyword_function,
    keyword_true,
    keyword_false,
    keyword_if,
    keyword_else,
    keyword_for,
    keyword_while,
    keyword_return,
};

pub const Token = struct {
    const Self = @This();
    type: TokenType = .illegal,
    literal: []const u8 = "",
    pub fn init(token_type: TokenType, literal: []const u8) Self {
        return Self{
            .type = token_type,
            .literal = literal,
        };
    }
    pub fn lookupIdentifier(identifier: []const u8) TokenType {
        const map = std.ComptimeStringMap(TokenType, .{
            .{ "function", .keyword_function },
            .{ "true", .keyword_function },
            .{ "false", .keyword_false },
            .{ "if", .keyword_if },
            .{ "else", .keyword_else },
            .{ "for", .keyword_for },
            .{ "while", .keyword_while },
            .{ "return", .keyword_return },
        });
        if (map.get(identifier)) |key| {
            return key;
        }
        return .identifier;
    }
};

test "lookupIdentifier" {
    try std.testing.expect(.keyword_function == Token.lookupIdentifier("function"));
    try std.testing.expect(.keyword_if == Token.lookupIdentifier("if"));
    try std.testing.expect(.keyword_return == Token.lookupIdentifier("return"));
    try std.testing.expect(.identifier == Token.lookupIdentifier("foo"));
    try std.testing.expect(.identifier == Token.lookupIdentifier("bar"));
    try std.testing.expect(.identifier == Token.lookupIdentifier("foobar"));
}
