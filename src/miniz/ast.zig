const std = @import("std");
const Token = @import("token.zig").Token;
const Operator = @import("operator.zig").Operator;
const Allocator = std.mem.Allocator;

const Error = error{OutOfMemory};

pub const Program = struct {
    const Self = @This();

    statements: std.ArrayList(*Statement),

    pub fn init(allocator: Allocator) !*Self {
        const program = try allocator.create(Self);
        program.* = Self{
            .statements = std.ArrayList(*Statement).init(allocator),
        };
        return program;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        for (self.statements.items) |statement| {
            statement.deinit(allocator);
        }
        self.statements.deinit();
        allocator.destroy(self);
    }
    pub fn toString(self: Self, allocator: Allocator) ![]u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        try self.render(&buffer);
        return buffer.toOwnedSlice();
    }
    fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        for (self.statements.items, 0..) |statement, index| {
            if (index > 0) {
                try writer.writeAll("\n");
            }
            try statement.render(buffer);
        }
    }
};

pub const Statement = union(enum) {
    const Self = @This();

    expression_statement: *ExpressionStatement,
    return_statement: *ReturnStatement,

    pub fn init(allocator: Allocator) !*Self {
        return try allocator.create(Self);
    }
    pub fn createExpressionStatement(allocator: Allocator, expression: *Expression) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = Self{
            .expression_statement = try ExpressionStatement.init(allocator, expression),
        };
        return stmt;
    }
    pub fn createReturnStatement(allocator: Allocator, expression: *Expression) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = Self{
            .return_statement = try ReturnStatement.init(allocator, expression),
        };
        return stmt;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        switch (self.*) {
            .expression_statement => |es| es.deinit(allocator),
            .return_statement => |rs| rs.deinit(allocator),
        }
        allocator.destroy(self);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        switch (self) {
            .expression_statement => |es| try es.render(buffer),
            .return_statement => |rs| try rs.render(buffer),
        }
    }
};

const ExpressionStatement = struct {
    const Self = @This();

    expression: *Expression,

    pub fn init(allocator: Allocator, expression: *Expression) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = Self{
            .expression = expression,
        };
        return stmt;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.expression.deinit(allocator);
        allocator.destroy(self);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        try writer.writeAll("(expr ");
        try self.expression.render(buffer);
        try writer.writeAll(")");
    }
};

const ReturnStatement = struct {
    const Self = @This();

    expression: *Expression,

    pub fn init(allocator: Allocator, expression: *Expression) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = Self{
            .expression = expression,
        };
        return stmt;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.expression.deinit(allocator);
        allocator.destroy(self);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        try writer.writeAll("(return ");
        try self.expression.render(buffer);
        try writer.writeAll(")");
    }
};

pub const Expression = union(enum) {
    const Self = @This();

    integer: i64,
    identifier: []const u8,
    binary_expression: *BinaryExpression,
    unary_expression: *UnaryExpression,

    pub fn init(allocator: Allocator) !*Self {
        return try allocator.create(Self);
    }
    pub fn createInteger(allocator: Allocator, value: i64) !*Self {
        const expr = try allocator.create(Self);
        expr.* = Self{
            .integer = value,
        };
        return expr;
    }
    pub fn createIdentifier(allocator: Allocator, identifier: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = Self{
            .identifier = identifier,
        };
        return expr;
    }
    pub fn createUnaryExpression(allocator: Allocator, operator: Operator, operand: *Expression) !*Self {
        const expr = try allocator.create(Self);
        expr.* = Self{
            .unary_expression = try UnaryExpression.init(allocator, operator, operand),
        };
        return expr;
    }
    pub fn createBinaryExpression(allocator: Allocator, operator: Operator, lhs: *Expression, rhs: *Expression) !*Self {
        const expr = try allocator.create(Self);
        expr.* = Self{
            .binary_expression = try BinaryExpression.init(allocator, operator, lhs, rhs),
        };
        return expr;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        switch (self.*) {
            .binary_expression => |bexpr| {
                bexpr.deinit(allocator);
            },
            .unary_expression => |uexpr| {
                uexpr.deinit(allocator);
            },
            else => {},
        }
        allocator.destroy(self);
    }
    pub fn toString(self: Self, allocator: Allocator) ![]u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        try self.render(&buffer);
        return buffer.toOwnedSlice();
    }
    fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        switch (self) {
            .integer => |integer| try writer.print("{d}", .{integer}),
            .identifier => |identifier| try writer.writeAll(identifier),
            .binary_expression => |bexpr| try bexpr.render(buffer),
            .unary_expression => |uexpr| try uexpr.render(buffer),
        }
    }
};

const UnaryExpression = struct {
    const Self = @This();

    operator: Operator,
    operand: *Expression,

    pub fn init(allocator: Allocator, operator: Operator, operand: *Expression) !*Self {
        const expr = try allocator.create(Self);
        expr.* = Self{
            .operator = operator,
            .operand = operand,
        };
        return expr;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.operand.deinit(allocator);
        allocator.destroy(self);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        try writer.print("({s} ", .{self.operator.toString()});
        try self.operand.render(buffer);
        try writer.writeAll(")");
    }
};

const BinaryExpression = struct {
    const Self = @This();

    operator: Operator,
    lhs: *Expression,
    rhs: *Expression,

    pub fn init(allocator: Allocator, operator: Operator, lhs: *Expression, rhs: *Expression) !*Self {
        const expr = try allocator.create(Self);
        expr.* = Self{
            .operator = operator,
            .lhs = lhs,
            .rhs = rhs,
        };
        return expr;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.lhs.deinit(allocator);
        self.rhs.deinit(allocator);
        allocator.destroy(self);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        try writer.print("({s} ", .{self.operator.toString()});
        try self.lhs.render(buffer);
        try writer.writeAll(" ");
        try self.rhs.render(buffer);
        try writer.writeAll(")");
    }
};

test "toString" {
    const alloc = std.testing.allocator;
    const e = try Expression.createBinaryExpression(
        alloc,
        .plus,
        try Expression.createInteger(alloc, 1),
        try Expression.createBinaryExpression(
            alloc,
            .plus,
            try Expression.createInteger(alloc, 2),
            try Expression.createInteger(alloc, 3),
        ),
    );
    defer e.deinit(alloc);

    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "(+ 1 (+ 2 3))", str);
}
