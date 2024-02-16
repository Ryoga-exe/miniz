const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const evaluator = @import("evaluator.zig");
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
    var env = evaluator.Env.init(allocator);
    defer env.deinit();
    while (true) {
        try stdout.print("{s}", .{Prompt});
        const line = readLine(allocator, stdin) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (line) |input| {
            if (input.len == 0) {
                continue;
            }

            var lexer = Lexer.init(input);
            var parser = Parser.init(allocator, &lexer);
            defer parser.deinit();
            const program = try parser.parseProgram();
            defer program.deinit(allocator);

            const result = try evaluator.eval(allocator, program, &env);

            if (result) |value| {
                try stdout.print("{d}\n\n", .{value});
            } else {
                try stdout.print("{s}\n\n", .{"null"});
            }
        }
    }
}

fn readLine(allocator: std.mem.Allocator, reader: anytype) !?[]const u8 {
    var buffer = try allocator.alloc(u8, BufferSize);
    var fbs = std.io.fixedBufferStream(buffer);
    try reader.streamUntilDelimiter(fbs.writer(), '\n', BufferSize);
    return fbs.getWritten();
}
