const std = @import("std");
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;

fn isLetter(char: u8) bool {
    return std.ascii.isAlphabetic(char) or char == '_';
}

fn isDigit(char: u8) bool {
    return std.ascii.isDigit(char);
}

pub const Lexer = struct {
    const Self = @This();

    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,

    pub fn init(input: []const u8) Self {
        var l = Self{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = 0,
        };
        l.readChar();
        return l;
    }
    pub fn nextToken(self: *Self) Token {
        var tok = Token.init(.illegal, "");

        self.skipWhitespace();

        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    tok = Token.init(.eq, "==");
                } else {
                    tok = Token.init(.assign, "=");
                }
            },
            '!' => {
                if (self.peekChar() == '=') {
                    tok = Token.init(.not_eq, "!=");
                } else {
                    tok = Token.init(.bang, "!");
                }
            },
            '+' => tok = Token.init(.plus, "+"),
            '-' => tok = Token.init(.minus, "-"),
            '*' => tok = Token.init(.asterisk, "*"),
            '/' => tok = Token.init(.slash, "/"),
            '<' => tok = Token.init(.lt, "<"),
            '>' => tok = Token.init(.gt, ">"),
            ',' => tok = Token.init(.comma, ","),
            ';' => tok = Token.init(.semicolon, ";"),
            '(' => tok = Token.init(.lparen, "("),
            ')' => tok = Token.init(.rparen, ")"),
            '{' => tok = Token.init(.lbrace, "{"),
            '}' => tok = Token.init(.rbrace, "}"),
            0 => tok = Token.init(.eof, ""),
            else => {
                if (isLetter(self.ch)) {
                    tok.literal = self.readIdentifier();
                    tok.type = Token.lookupIdentifier(tok.literal);
                    return tok;
                } else if (isDigit(self.ch)) {
                    tok.type = .integer;
                    tok.literal = self.readNumber();
                    return tok;
                }
            },
        }
        self.readChar();
        return tok;
    }

    fn readChar(self: *Self) void {
        if (self.read_position < self.input.len) {
            self.ch = self.input[self.read_position];
        } else {
            self.ch = 0;
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
    fn peekChar(self: Self) u8 {
        if (self.read_position < self.input.len) {
            return self.input[self.read_position];
        } else {
            return 0;
        }
    }
    fn skipWhitespace(self: *Self) void {
        while (std.ascii.isWhitespace(self.ch)) {
            self.readChar();
        }
    }
    fn readIdentifier(self: *Self) []const u8 {
        const position = self.position;
        while (isLetter(self.ch)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }
    fn readNumber(self: *Self) []const u8 {
        const position = self.position;
        while (isDigit(self.ch)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }
};

test "Lexer" {
    const input =
        \\five = 5;
        \\ten = 10;
        \\i = 0;
        \\while(i < 0) {
        \\    i = i + 1;
        \\}
    ;
    const tests = [_]Token{
        Token.init(.identifier, "five"),
        Token.init(.assign, "="),
        Token.init(.integer, "5"),
        Token.init(.semicolon, ";"),
        Token.init(.identifier, "ten"),
        Token.init(.assign, "="),
        Token.init(.integer, "10"),
        Token.init(.semicolon, ";"),
        Token.init(.identifier, "i"),
        Token.init(.assign, "="),
        Token.init(.integer, "0"),
        Token.init(.semicolon, ";"),
        Token.init(.keyword_while, "while"),
        Token.init(.lparen, "("),
        Token.init(.identifier, "i"),
        Token.init(.lt, "<"),
        Token.init(.integer, "0"),
        Token.init(.rparen, ")"),
        Token.init(.lbrace, "{"),
        Token.init(.identifier, "i"),
        Token.init(.assign, "="),
        Token.init(.identifier, "i"),
        Token.init(.plus, "+"),
        Token.init(.integer, "1"),
        Token.init(.semicolon, ";"),
        Token.init(.rbrace, "}"),
        Token.init(.eof, ""),
    };
    var lexer = Lexer.init(input);

    for (tests) |expect| {
        const tok = lexer.nextToken();

        try std.testing.expect(expect.type == tok.type);
        try std.testing.expectEqualSlices(u8, expect.literal, tok.literal);
    }
}
