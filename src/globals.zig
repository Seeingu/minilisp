const Object = @import("object.zig").Object;
const std = @import("std");
const utils = @import("utils.zig");

pub var nilObject: *Object = undefined;
pub var dotObject: *Object = undefined;
pub var parenObject: *Object = undefined;
pub var trueObject: *Object = undefined;

pub fn init(allocator: std.mem.Allocator) !void {
    nilObject = utils.allocObject2(allocator, .{ .token = "()" });
    parenObject = utils.allocObject2(allocator, .{ .token = "paren" });
    dotObject = utils.allocObject2(allocator, .{ .token = "." });
    trueObject = utils.allocObject2(allocator, .{ .token = "t" });
}

pub fn deinit(allocator: std.mem.Allocator) void {
    allocator.destroy(nilObject);
    allocator.destroy(parenObject);
    allocator.destroy(dotObject);
    allocator.destroy(trueObject);
}
