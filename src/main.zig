const std = @import("std");
const evaluator = @import("evaluator.zig");

pub fn main() !void {
    var stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const out = bw.writer();

    var line: [256]u8 = undefined;

    while (true) {
        try out.print("> ", .{});
        try bw.flush();

        const read_bytes = try stdin.readUntilDelimiterOrEof(&line, '\n');
        if (read_bytes) |bytes| {
            if (std.mem.eql(u8, bytes, "exit")) break;
            const result = evaluator.run(bytes) catch |err| {
                try out.print("Error: {}\n", .{err});
                continue;
            };
            try out.print("{s}\n", .{result});
            try bw.flush();
        } else {
            break;
        }
    }
}
