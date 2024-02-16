const std = @import("std");
const Expression = @import("ast.zig").Expression;

const Object = enum {
    integer,
    string,
    // undefined,
};

const env = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    map: std.StringArrayHashMap(*Expression),

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .map = std.StringArrayHashMap(*Expression).init(allocator),
        };
    }
};

// pub fn eval() ?Object {}

pub fn eval(allocator: std.mem.Allocator, program: *Expression) !i64 {
    var buffer = std.ArrayList(u8).init(allocator);
    const writer = buffer.writer();
    _ = writer;
    switch (program.*) {
        .integer => |integer| {
            return integer;
        },
        .unary_expression => |uexpr| {
            if (std.mem.eql(u8, "+", uexpr.operator)) {
                return try eval(allocator, uexpr.operand);
            } else if (std.mem.eql(u8, "-", uexpr.operator)) {
                return try eval(allocator, uexpr.operand) * (-1);
            } else if (std.mem.eql(u8, "paren", uexpr.operator)) {
                return try eval(allocator, uexpr.operand);
            } else {
                unreachable;
            }
        },
        .binary_expression => |bexpr| {
            if (std.mem.eql(u8, "+", bexpr.operator)) {
                return try eval(allocator, bexpr.lhs) + try eval(allocator, bexpr.rhs);
            } else if (std.mem.eql(u8, "-", bexpr.operator)) {
                return try eval(allocator, bexpr.lhs) - try eval(allocator, bexpr.rhs);
            } else if (std.mem.eql(u8, "*", bexpr.operator)) {
                return try eval(allocator, bexpr.lhs) * try eval(allocator, bexpr.rhs);
            } else {
                unreachable;
            }
        },
    }
    return 0;
}
