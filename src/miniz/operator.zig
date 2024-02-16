const std = @import("std");

pub const Operator = enum {
    const Self = @This();

    plus,
    minus,
    asterisk,
    slash,
    paren,
    none,

    pub fn lookupOperator(operator: []const u8) Self {
        const map = std.ComptimeStringMap(Self, .{
            .{ "+", .plus },
            .{ "-", .minus },
            .{ "*", .asterisk },
            .{ "/", .slash },
            .{ "(", .paren },
            .{ ")", .paren },
        });
        if (map.get(operator)) |ope| {
            return ope;
        }
        return .none;
    }

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .plus => "+",
            .minus => "-",
            .asterisk => "*",
            .slash => "/",
            .paren => "paren",
            else => "",
        };
    }
};
