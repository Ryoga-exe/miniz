const std = @import("std");

pub const SExpr = union(enum) {
    const Self = @This();

    atom: []const u8,
    list: []const SExpr,

    pub fn toString(self: Self, allocator: std.mem.Allocator) ![]u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();
        const writer = buffer.writer();
        switch (self) {
            SExpr.atom => |atom| try writer.writeAll(atom),
            SExpr.list => |list| {
                try writer.writeAll("(");
                for (list, 0..) |v, index| {
                    const str = try v.toString(allocator);
                    defer allocator.free(str);
                    if (index > 0) {
                        try writer.writeAll(" ");
                    }
                    try writer.writeAll(str);
                }
                try writer.writeAll(")");
            },
        }
        var result = try allocator.dupe(u8, buffer.items);
        return result;
    }
};

test "toString()" {
    const e = SExpr{ .list = &[_]SExpr{ SExpr{ .atom = "+" }, SExpr{ .atom = "1" }, SExpr{ .list = &[_]SExpr{
        SExpr{ .atom = "+" },
        SExpr{ .atom = "2" },
        SExpr{ .atom = "3" },
    } } } };
    const str = try e.toString(std.testing.allocator);
    defer std.testing.allocator.free(str);
    try std.testing.expectEqualSlices(u8, "(+ 1 (+ 2 3))", str);
}
