//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");

const evaluator = @import("evaluator.zig");
const utils = @import("utils.zig");

// cross language call
export fn run(buf: [*:0]const u8, len: c_int) void {
    const source = buf[0..@intCast(len)];
    const result = evaluator.run(source) catch unreachable;
    std.debug.print("{s}\n", .{result});
}

test "all" {
    _ = utils;
    _ = evaluator;
}
