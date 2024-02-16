const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Program = @import("ast.zig").Program;
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

pub fn eval(allocator: std.mem.Allocator, program: *Program, env: *Env) !?i64 {
    var buffer = std.ArrayList(u8).init(allocator);
    const writer = buffer.writer();
    _ = writer;

    var ret: ?i64 = null;
    for (program.statements.items) |statement| {
        switch (statement.*) {
            .expression_statement => |es| {
                ret = try evalExpression(allocator, es.expression, env);
            },
            else => {
                ret = null;
            },
        }
    }
    return ret;
}

pub fn evalExpression(allocator: std.mem.Allocator, expression: *Expression, env: *Env) !i64 {
    switch (expression.*) {
        .integer => |integer| {
            return integer;
        },
        .identifier => |identifier| {
            return env.get(identifier);
        },
        .unary_expression => |uexpr| {
            switch (uexpr.operator) {
                .plus => return try evalExpression(allocator, uexpr.operand, env),
                .minus => return try evalExpression(allocator, uexpr.operand, env) * (-1),
                .paren => return try evalExpression(allocator, uexpr.operand, env),
                else => unreachable,
            }
        },
        .binary_expression => |bexpr| {
            switch (bexpr.operator) {
                .plus => return try evalExpression(allocator, bexpr.lhs, env) + try evalExpression(allocator, bexpr.rhs, env),
                .minus => return try evalExpression(allocator, bexpr.lhs, env) - try evalExpression(allocator, bexpr.rhs, env),
                .asterisk => return try evalExpression(allocator, bexpr.lhs, env) * try evalExpression(allocator, bexpr.rhs, env),
                .slash => return @divFloor(try evalExpression(allocator, bexpr.lhs, env), try evalExpression(allocator, bexpr.rhs, env)),
                .mod => return @mod(try evalExpression(allocator, bexpr.lhs, env), try evalExpression(allocator, bexpr.rhs, env)),
                .rem => return @rem(try evalExpression(allocator, bexpr.lhs, env), try evalExpression(allocator, bexpr.rhs, env)),
                .lt => return if (try evalExpression(allocator, bexpr.lhs, env) < try evalExpression(allocator, bexpr.rhs, env)) 1 else 0,
                .gt => return if (try evalExpression(allocator, bexpr.lhs, env) > try evalExpression(allocator, bexpr.rhs, env)) 1 else 0,
                .lt_eq => return if (try evalExpression(allocator, bexpr.lhs, env) <= try evalExpression(allocator, bexpr.rhs, env)) 1 else 0,
                .gt_eq => return if (try evalExpression(allocator, bexpr.lhs, env) >= try evalExpression(allocator, bexpr.rhs, env)) 1 else 0,
                .eq => return if (try evalExpression(allocator, bexpr.lhs, env) == try evalExpression(allocator, bexpr.rhs, env)) 1 else 0,
                .not_eq => return if (try evalExpression(allocator, bexpr.lhs, env) != try evalExpression(allocator, bexpr.rhs, env)) 1 else 0,
                .assign => {
                    const result = try evalExpression(allocator, bexpr.rhs, env);
                    try env.set(bexpr.lhs.identifier, result);
                    return result;
                },
                else => unreachable,
            }
        },
    }
    return 0;
}

test "eval" {
    const alloc = std.testing.allocator;
    const tests = [_]struct {
        input: []const u8,
        expect: ?i64,
    }{
        .{ .input = "return 123 + 234;", .expect = null },
        .{ .input = "1234;", .expect = 1234 },
        .{ .input = "+1234;", .expect = 1234 },
        .{ .input = "-1234;", .expect = -1234 },
        .{ .input = "1234 + 5678;", .expect = 6912 },
        .{ .input = "1234 + -5678;", .expect = -4444 },
        .{ .input = "1234--5678;", .expect = 6912 },
        .{ .input = "1 + 2 + 3;", .expect = 6 },
        .{ .input = "1 + 2 * 3 + 4;", .expect = 11 },
        .{ .input = "-100 %% 100 + - 10 % 10 * 20", .expect = 0 },
        .{ .input = "mod = 998244353", .expect = 998244353 },
        .{ .input = "foobar = 1 + 3 + 5", .expect = 9 },
        .{ .input = "foobar + mod", .expect = 998244362 },
    };

    var env = Env.init(alloc);
    defer env.deinit();
    for (tests) |t| {
        var lexer = Lexer.init(t.input);
        var parser = Parser.init(alloc, &lexer);
        defer parser.deinit();
        const program = try parser.parseProgram();
        defer program.deinit(alloc);
        const result = try eval(alloc, program, &env);

        try std.testing.expect(result == t.expect);
    }
}
