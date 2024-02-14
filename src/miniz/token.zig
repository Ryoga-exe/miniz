const eql = @import("std").mem.eql;

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
    type: TokenType,
    literal: []const u8,
};

pub fn lookupIdentifier(literal: []const u8) TokenType {
    if (eql(u8, literal, "function")) {
        return TokenType.keyword_function;
    } else if (eql(u8, literal, "true")) {
        return TokenType.keyword_true;
    } else if (eql(u8, literal, "false")) {
        return TokenType.keyword_false;
    } else if (eql(u8, literal, "if")) {
        return TokenType.keyword_if;
    } else if (eql(u8, literal, "else")) {
        return TokenType.keyword_else;
    } else if (eql(u8, literal, "for")) {
        return TokenType.keyword_for;
    } else if (eql(u8, literal, "while")) {
        return TokenType.keyword_while;
    } else if (eql(u8, literal, "return")) {
        return TokenType.keyword_return;
    }
    return TokenType.identifier;
}
