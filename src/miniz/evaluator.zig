const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Expression = @import("ast.zig").Expression;

const Object = enum {
    integer,
    string,
    // undefined,
};

pub const Env = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    map: std.StringArrayHashMap(i64),
    // map: std.StringArrayHashMap(*Expression),

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .map = std.StringArrayHashMap(i64).init(allocator),
        };
    }
    pub fn deinit(self: *Self) void {
        self.map.deinit();
    }
    pub fn set(self: *Self, identifier: []const u8, expression: i64) !void {
        try self.map.put(identifier, expression);
    }
    pub fn get(self: *Self, identifier: []const u8) i64 {
        if (self.map.get(identifier)) |expr| {
            return expr;
        } else {
            unreachable; // {identifier} is not defined
        }
    }
};

pub fn eval(allocator: std.mem.Allocator, program: *Expression, env: *Env) !i64 {
    var buffer = std.ArrayList(u8).init(allocator);
    const writer = buffer.writer();
    _ = writer;
    switch (program.*) {
        .integer => |integer| {
            return integer;
        },
        .identifier => |identifier| {
            return env.get(identifier);
        },
        .unary_expression => |uexpr| {
            switch (uexpr.operator) {
                .plus => return try eval(allocator, uexpr.operand, env),
                .minus => return try eval(allocator, uexpr.operand, env) * (-1),
                .paren => return try eval(allocator, uexpr.operand, env),
                else => unreachable,
            }
        },
        .binary_expression => |bexpr| {
            switch (bexpr.operator) {
                .plus => return try eval(allocator, bexpr.lhs, env) + try eval(allocator, bexpr.rhs, env),
                .minus => return try eval(allocator, bexpr.lhs, env) - try eval(allocator, bexpr.rhs, env),
                .asterisk => return try eval(allocator, bexpr.lhs, env) * try eval(allocator, bexpr.rhs, env),
                .slash => return @divFloor(try eval(allocator, bexpr.lhs, env), try eval(allocator, bexpr.rhs, env)),
                .assign => {
                    const result = try eval(allocator, bexpr.rhs, env);
                    try env.set(bexpr.lhs.identifier, result);
                    return result;
                },
                else => unreachable,
            }
        },
    }
    return 0;
}

test "eval: 1 + 2 * 3 + 4" {
    const alloc = std.testing.allocator;
    const input = "1 + 2 * 3 + 4";
    var lexer = Lexer.init(input);
    var parser = Parser.init(alloc, &lexer);
    defer parser.deinit();
    const e = try parser.parseProgram();
    defer e.deinit(alloc);
    const result = try eval(alloc, e);

    try std.testing.expect(result == 11);
}
