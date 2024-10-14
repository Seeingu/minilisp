const std = @import("std");

pub const String = []const u8;

// Unsafe string builder
pub const StringBuilder = struct {
    allocator: std.mem.Allocator,
    buffer: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) StringBuilder {
        return StringBuilder{
            .allocator = allocator,
            .buffer = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn append(self: *StringBuilder, str: String) void {
        self.buffer.appendSlice(str) catch unreachable;
    }

    pub fn pop(self: *StringBuilder) u8 {
        return self.buffer.pop();
    }

    pub fn toString(self: *StringBuilder) String {
        return self.buffer.toOwnedSlice() catch unreachable;
    }

    pub fn deinit(self: *StringBuilder) void {
        self.buffer.deinit();
    }
};
