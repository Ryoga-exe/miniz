const std = @import("std");
const repl = @import("miniz/repl.zig");
const evaluator = @import("miniz/evaluator.zig");
const Lexer = @import("miniz/lexer.zig").Lexer;
const Parser = @import("miniz/parser.zig").Parser;
const BufferSize = 8192;

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

    var file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    const file_size = try file.getEndPos();
    var reader = std.io.bufferedReader(file.reader());
    var instream = reader.reader();

    const contents = try instream.readAllAlloc(alc, file_size);
    defer alc.free(contents);

    var env = evaluator.Env.init(alc);
    defer env.deinit();

    var lexer = Lexer.init(contents);
    var parser = Parser.init(alc, &lexer);
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit(alc);

    const result = try evaluator.eval(alc, program, &env);

    if (result) |value| {
        try stdout.print("{d}\n\n", .{value});
    } else {
        try stdout.print("{s}\n\n", .{"null"});
    }
}
