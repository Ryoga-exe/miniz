const std = @import("std");
const Prompt = "> ";
const BufferSize = 8192;

pub fn start(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    const welcome_message =
        \\ 
        \\ ███╗░░░███╗██╗███╗░░██╗██╗███████╗
        \\ ████╗░████║██║████╗░██║██║╚════██║
        \\ ██╔████╔██║██║██╔██╗██║██║░░███╔═╝
        \\ ██║╚██╔╝██║██║██║╚████║██║██╔══╝░░
        \\ ██║░╚═╝░██║██║██║░╚███║██║███████╗
        \\ ╚═╝░░░░░╚═╝╚═╝╚═╝░░╚══╝╚═╝╚══════╝
        \\ 
        \\ Minimal scripting language in Zig.
        \\ 
    ;

    try stdout.print("{s}\n", .{welcome_message});
    // var env = ...
    while (true) {
        try stdout.print("{s}", .{Prompt});
        const line = readLine(allocator, stdin) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (line) |ln| {
            if (ln.len == 0) {
                continue;
            }
            try stdout.print("{s}\n", .{ln});
            // const result = eval(...)
            // try stdout.print("{s}\n", result.toString()...)
        }
    }
}

fn readLine(allocator: std.mem.Allocator, reader: anytype) !?[]const u8 {
    var buffer = try allocator.alloc(u8, BufferSize);
    var fbs = std.io.fixedBufferStream(buffer);
    try reader.streamUntilDelimiter(fbs.writer(), '\n', BufferSize);
    return fbs.getWritten();
}
