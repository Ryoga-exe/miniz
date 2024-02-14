const std = @import("std");

pub const Input = struct {
    const Self = @This();

    text: []u8,
    position: usize,

    pub fn init() Self {
        return Self{};
    }
    pub fn deinit() void {}
};
