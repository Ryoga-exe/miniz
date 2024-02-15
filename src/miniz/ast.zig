const std = @import("std");
const Token = @import("token").Token;
const Allocator = std.mem.Allocator;

pub const SExpr = union(enum) {
    const Self = @This();

    atom: std.ArrayList(u8),
    // atom_symbol: std.ArrayList(u8),
    // atom_integer: i16,
    // atom_boolean: bool,
    // atom_string: std.ArrayList(u8),
    list: ?std.ArrayList(*SExpr),

    pub fn init(allocator: Allocator) !*Self {
        return try allocator.create(Self);
    }
    pub fn createAtom(allocator: Allocator, str: []const u8) !*Self {
        const sexpr = try allocator.create(Self);
        var buffer = std.ArrayList(u8).init(allocator);
        errdefer buffer.deinit();
        const writer = buffer.writer();
        try writer.writeAll(str);
        sexpr.* = Self{
            .atom = buffer,
        };
        return sexpr;
    }
    pub fn createList(allocator: Allocator, items: []const *SExpr) !*Self {
        const sexpr = try allocator.create(Self);
        var list = std.ArrayList(*SExpr).init(allocator);
        errdefer list.deinit();
        for (items) |item| {
            try list.append(item);
        }
        sexpr.* = Self{
            .list = list,
        };
        return sexpr;
    }
    pub fn deinit(self: *Self, allocator: Allocator) void {
        switch (self.*) {
            SExpr.atom => |atom| {
                atom.deinit();
            },
            SExpr.list => |list| {
                for (list.?.items) |v| {
                    v.deinit(allocator);
                }
                list.?.deinit();
            },
        }
        allocator.destroy(self);
    }
    pub fn toString(self: Self, allocator: Allocator) ![]u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        try self.render(&buffer);
        return buffer.toOwnedSlice();
    }
    fn render(self: Self, buffer: *std.ArrayList(u8)) !void {
        const writer = buffer.writer();
        switch (self) {
            SExpr.atom => |atom| try writer.writeAll(atom.items),
            SExpr.list => |list| {
                try writer.writeAll("(");
                for (list.?.items, 0..) |v, index| {
                    if (index > 0) {
                        try writer.writeAll(" ");
                    }
                    try v.render(buffer);
                }
                try writer.writeAll(")");
            },
        }
    }
};

test "toString()" {
    const alloc = std.testing.allocator;
    const e = try SExpr.createList(alloc, &[_]*SExpr{
        try SExpr.createAtom(alloc, "+"),
        try SExpr.createAtom(alloc, "1"),
        try SExpr.createList(alloc, &[_]*SExpr{
            try SExpr.createAtom(alloc, "+"),
            try SExpr.createAtom(alloc, "2"),
            try SExpr.createAtom(alloc, "3"),
        }),
    });
    defer e.deinit(alloc);
    const str = try e.toString(alloc);
    defer alloc.free(str);

    try std.testing.expectEqualSlices(u8, "(+ 1 (+ 2 3))", str);
}
