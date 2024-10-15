//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.
const std = @import("std");
const evaluator = @import("evaluator.zig");

pub fn main() !void {
    const r = try evaluator.run("'a");
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Result:\n{s}\n", .{r});

    try bw.flush(); // Don't forget to flush!
}
