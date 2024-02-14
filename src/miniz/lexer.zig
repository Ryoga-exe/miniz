const std = @import("std");
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const lookupIdentifier = @import("./token.zig").lookupIdentifier;

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
        var tok = Token{ .type = TokenType.illegal, .literal = "" };

        self.skipWhitespace();

        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    tok = Token{ .type = TokenType.eq, .literal = "==" };
                } else {
                    tok = Token{ .type = TokenType.assign, .literal = "=" };
                }
            },
            '!' => {
                if (self.peekChar() == '=') {
                    tok = Token{ .type = TokenType.not_eq, .literal = "!=" };
                } else {
                    tok = Token{ .type = TokenType.bang, .literal = "!" };
                }
            },
            '+' => tok = Token{ .type = TokenType.plus, .literal = "+" },
            '-' => tok = Token{ .type = TokenType.minus, .literal = "-" },
            '*' => tok = Token{ .type = TokenType.asterisk, .literal = "*" },
            '/' => tok = Token{ .type = TokenType.slash, .literal = "/" },
            '<' => tok = Token{ .type = TokenType.lt, .literal = "<" },
            '>' => tok = Token{ .type = TokenType.gt, .literal = ">" },
            ',' => tok = Token{ .type = TokenType.comma, .literal = "," },
            ';' => tok = Token{ .type = TokenType.semicolon, .literal = ";" },
            '(' => tok = Token{ .type = TokenType.lparen, .literal = "(" },
            ')' => tok = Token{ .type = TokenType.rparen, .literal = ")" },
            '{' => tok = Token{ .type = TokenType.lbrace, .literal = "{" },
            '}' => tok = Token{ .type = TokenType.rbrace, .literal = "}" },
            0 => tok = Token{ .type = TokenType.eof, .literal = "" },
            else => {
                if (isLetter(self.ch)) {
                    tok.literal = self.readIdentifier();
                    tok.type = lookupIdentifier(tok.literal);
                    return tok;
                } else if (isDigit(self.ch)) {
                    tok.type = TokenType.integer;
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
        Token{ .type = TokenType.identifier, .literal = "five" },
        Token{ .type = TokenType.assign, .literal = "=" },
        Token{ .type = TokenType.integer, .literal = "5" },
        Token{ .type = TokenType.semicolon, .literal = ";" },
        Token{ .type = TokenType.identifier, .literal = "ten" },
        Token{ .type = TokenType.assign, .literal = "=" },
        Token{ .type = TokenType.integer, .literal = "10" },
        Token{ .type = TokenType.semicolon, .literal = ";" },
        Token{ .type = TokenType.identifier, .literal = "i" },
        Token{ .type = TokenType.assign, .literal = "=" },
        Token{ .type = TokenType.integer, .literal = "0" },
        Token{ .type = TokenType.semicolon, .literal = ";" },
        Token{ .type = TokenType.keyword_while, .literal = "while" },
        Token{ .type = TokenType.lparen, .literal = "(" },
        Token{ .type = TokenType.identifier, .literal = "i" },
        Token{ .type = TokenType.lt, .literal = "<" },
        Token{ .type = TokenType.integer, .literal = "0" },
        Token{ .type = TokenType.rparen, .literal = ")" },
        Token{ .type = TokenType.lbrace, .literal = "{" },
        Token{ .type = TokenType.identifier, .literal = "i" },
        Token{ .type = TokenType.assign, .literal = "=" },
        Token{ .type = TokenType.identifier, .literal = "i" },
        Token{ .type = TokenType.plus, .literal = "+" },
        Token{ .type = TokenType.integer, .literal = "1" },
        Token{ .type = TokenType.semicolon, .literal = ";" },
        Token{ .type = TokenType.rbrace, .literal = "}" },
        Token{ .type = TokenType.eof, .literal = "" },
    };
    var lexer = Lexer.init(input);

    for (tests) |expect| {
        const tok = lexer.nextToken();

        try std.testing.expect(expect.type == tok.type);
        try std.testing.expectEqualSlices(u8, expect.literal, tok.literal);
    }
}
