const std = @import("std");
const repl = @import("miniz/repl.zig");

pub fn main() !void {
    const alc = std.heap.page_allocator;
    const args = try std.process.argsAlloc(alc);
    defer std.process.argsFree(alc, args);

    const stdout = std.io.getStdOut().writer();

    if (args.len < 2) {
        try repl.start(alc);
        std.os.exit(0);
    }

    try stdout.print("input file: {s}\n", .{args[1]});

    // if .json
    // if .mnz or .miniz
}
