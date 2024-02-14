const std = @import("std");
const ast = @import("./ast.zig");
const TokenType = @import("./token.zig").TokenType;
const Lexer = @import("./lexer.zig").Lexer;

pub fn parse_atom(lexer: *Lexer) ast.SExpr {
    const tok = lexer.nextToken();
    switch (tok.type) {
        TokenType.integer => {
            return ast.SExpr{ .atom = tok.literal };
        },
        else => {
            // Todo
            return ast.SExpr{ .atom = tok.literal };
        },
    }
}

pub fn parse_expr(lexer: *Lexer) ast.SExpr {
    // Todo
    return parse_atom(lexer);
}

test "Parse atom" {
    const input = "7";
    var lexer = Lexer.init(input);
    const e = parse_expr(&lexer);

    const str = try e.toString(std.testing.allocator);
    defer std.testing.allocator.free(str);

    try std.testing.expectEqualSlices(u8, "7", str);
}
