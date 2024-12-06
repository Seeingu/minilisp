const std = @import("std");
const StringModule = @import("string").String;
const String = @import("type.zig").String;

pub const StringBuilder = @This();

pub fn init(allocator: std.mem.Allocator) StringBuilder {
    return .{
        .string = StringModule.init(allocator),
    };
}

string: StringModule,

pub fn append(self: *StringBuilder, str: String) void {
    self.string.concat(str) catch unreachable;
}

pub fn pop(self: *StringBuilder) u8 {
    return self.string.pop();
}

pub fn toString(self: *StringBuilder) String {
    return self.string.str();
}

pub fn deinit(self: *StringBuilder) void {
    self.string.deinit();
}
