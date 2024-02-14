const std = @import("std");
const SExpr = @import("./ast.zig").SExpr;
const TokenType = @import("./token.zig").TokenType;
const Lexer = @import("./lexer.zig").Lexer;

pub fn parse_atom(lexer: *Lexer) SExpr {
    const tok = lexer.nextToken();
    switch (tok.type) {
        TokenType.integer => {
            return SExpr{ .atom = tok.literal };
        },
        else => {
            // Todo
            return SExpr{ .atom = tok.literal };
        },
    }
}

pub fn parse_expr(lexer: *Lexer) SExpr {
    // Todo
    return parse_atom(lexer);
}

test "Parse atom" {
    const alloc = std.testing.allocator;
    const input = "7";
    var lexer = Lexer.init(input);
    const e = parse_expr(&lexer);
    const str = try e.toString(alloc);
    defer alloc.free(str);
    try std.testing.expectEqualSlices(u8, "7", str);
}
