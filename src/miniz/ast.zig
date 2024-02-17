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
    block_statement: *BlockStatement,
    function_statement: *FunctionStatement,
    while_statement: *WhileStatement,

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
    pub fn createBlockStatement(allocator: Allocator) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = Self{
            .block_statement = try BlockStatement.init(allocator),
        };
        return stmt;
    }
    pub fn createFunctionStatement(allocator: Allocator, name: []const u8) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = Self{
            .function_statement = try FunctionStatement.init(allocator, name),
        };
        return stmt;
    }
    pub fn createWhileStatement(allocator: Allocator, condition: *Expression, body: *Statement) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = Self{
            .while_statement = try WhileStatement.init(allocator, condition, body),
        };
        return stmt;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        switch (self.*) {
            .expression_statement => |es| es.deinit(allocator),
            .return_statement => |rs| rs.deinit(allocator),
            .block_statement => |bs| bs.deinit(allocator),
            .function_statement => |fs| fs.deinit(allocator),
            .while_statement => |ws| ws.deinit(allocator),
        }
        allocator.destroy(self);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        switch (self) {
            .expression_statement => |es| try es.render(buffer),
            .return_statement => |rs| try rs.render(buffer),
            .block_statement => |bs| try bs.render(buffer),
            .function_statement => |fs| try fs.render(buffer),
            .while_statement => |ws| try ws.render(buffer),
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

const BlockStatement = struct {
    const Self = @This();

    statements: std.ArrayList(*Statement),

    pub fn init(allocator: Allocator) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = Self{
            .statements = std.ArrayList(*Statement).init(allocator),
        };
        return stmt;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        for (self.statements.items) |statement| {
            statement.deinit(allocator);
        }
        self.statements.deinit();
        allocator.destroy(self);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        try writer.writeAll("(block ");
        for (self.statements.items, 0..) |statement, index| {
            if (index > 0) {
                try writer.writeAll(";");
            }
            try statement.render(buffer);
        }
        try writer.writeAll(")");
    }
};

pub const FunctionStatement = struct {
    const Self = @This();

    name: []const u8,
    params: std.ArrayList([]const u8),
    block: *BlockStatement,
    final: bool,

    pub fn init(allocator: Allocator, name: []const u8) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = Self{
            .name = name,
            .params = std.ArrayList([]const u8).init(allocator),
            .block = try BlockStatement.init(allocator),
            .final = true,
        };
        return stmt;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        for (self.params.items) |param| {
            allocator.free(param);
        }
        self.params.deinit();
        if (self.final) {
            self.block.deinit(allocator);
        } else {
            self.block.statements.deinit();
            allocator.destroy(self.block);
        }

        allocator.destroy(self);
    }
    pub fn addParam(self: *Self, allocator: Allocator, param: []const u8) !void {
        const owned = try allocator.dupe(u8, param);
        try self.params.append(owned);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        try writer.print("(function {s} ", .{self.name});
        try writer.writeAll("(params");
        for (self.params.items) |param| {
            try writer.print(" {s}", .{param});
        }
        try writer.writeAll(") ");
        try self.block.render(buffer);
        try writer.writeAll(")");
    }
};

const WhileStatement = struct {
    const Self = @This();

    condition: *Expression,
    body: *Statement,

    pub fn init(allocator: Allocator, condition: *Expression, body: *Statement) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = Self{
            .condition = condition,
            .body = body,
        };
        return stmt;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.condition.deinit(allocator);
        self.body.deinit(allocator);
        allocator.destroy(self);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        try writer.writeAll("(while ");
        try self.condition.render(buffer);
        try writer.writeAll(" ");
        try self.body.render(buffer);
        try writer.writeAll(")");
    }
};

pub const Expression = union(enum) {
    const Self = @This();

    integer: i64,
    identifier: []const u8,
    binary_expression: *BinaryExpression,
    unary_expression: *UnaryExpression,
    if_expression: *IfExpression,
    call_expression: *CallExpression,

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
    pub fn createIfExpression(allocator: Allocator, condition: *Expression, consequence: *Statement, alternative: ?*Statement) !*Self {
        const expr = try allocator.create(Self);
        expr.* = Self{
            .if_expression = try IfExpression.init(allocator, condition, consequence, alternative),
        };
        return expr;
    }
    pub fn createCallExpression(allocator: Allocator, name: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = Self{
            .call_expression = try CallExpression.init(allocator, name),
        };
        return expr;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        switch (self.*) {
            .integer => {},
            .identifier => {},
            .binary_expression => |bexpr| bexpr.deinit(allocator),
            .unary_expression => |uexpr| uexpr.deinit(allocator),
            .if_expression => |ifexpr| ifexpr.deinit(allocator),
            .call_expression => |callexpr| callexpr.deinit(allocator),
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
            .if_expression => |ifexpr| try ifexpr.render(buffer),
            .call_expression => |callexpr| try callexpr.render(buffer),
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

const IfExpression = struct {
    const Self = @This();

    condition: *Expression,
    consequence: *Statement,
    alternative: ?*Statement,

    pub fn init(allocator: Allocator, condition: *Expression, consequence: *Statement, alternative: ?*Statement) !*Self {
        const expr = try allocator.create(Self);
        expr.* = Self{
            .condition = condition,
            .consequence = consequence,
            .alternative = alternative,
        };
        return expr;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.condition.deinit(allocator);
        self.consequence.deinit(allocator);
        if (self.alternative) |alternative| {
            alternative.deinit(allocator);
        }
        allocator.destroy(self);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        try writer.writeAll("(if ");
        try self.condition.render(buffer);
        try writer.writeAll(" ");
        try self.consequence.render(buffer);
        if (self.alternative) |alternative| {
            try writer.writeAll(" ");
            try alternative.render(buffer);
        }
        try writer.writeAll(")");
    }
};

const CallExpression = struct {
    const Self = @This();

    name: []const u8,
    params: std.ArrayList(*Expression),

    pub fn init(allocator: Allocator, name: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = Self{
            .name = name,
            .params = std.ArrayList(*Expression).init(allocator),
        };
        return expr;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        for (self.params.items) |param| {
            param.deinit(allocator);
        }
        self.params.deinit();
        allocator.destroy(self);
    }
    pub fn render(self: Self, buffer: *std.ArrayList(u8)) Error!void {
        const writer = buffer.writer();
        try writer.print("(call {s} ", .{self.name});
        try writer.writeAll("(params");
        for (self.params.items) |param| {
            try writer.writeAll(" ");
            try param.render(buffer);
        }
        try writer.writeAll(")");
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
