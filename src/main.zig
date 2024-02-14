const std = @import("std");

pub fn main() !void {
    const alc = std.heap.page_allocator;
    const args = try std.process.argsAlloc(alc);
    defer std.process.argsFree(alc, args);

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    if (args.len < 2) {
        try stdout.print("repl mode...\n", .{});
        try bw.flush();
        std.os.exit(0);
    }

    try stdout.print("input file: {s}\n", .{args[1]});
    try bw.flush();

    // if .json
    // if .mnz or .miniz
}
