const std = @import("std");
const String = @import("type.zig").String;
const Object = @import("object.zig").Object;
const StringBuilder = @import("StringBuilder.zig").StringBuilder;
const utils = @import("utils.zig");
const Int = @import("type.zig").Int;
const globals = @import("globals.zig");

pub fn intToString(allocator: std.mem.Allocator, i: i32) String {
    return std.fmt.allocPrint(allocator, "{}", .{i}) catch "";
}
pub fn stringEqual(s1: String, s2: String) bool {
    return std.mem.eql(u8, s1, s2);
}

pub fn isSpecialSymbol(s: String) bool {
    return std.mem.indexOf(u8, "+-*/=", s) != null;
}

pub fn objectToString(allocator: std.mem.Allocator, o: *Object) String {
    var sb = StringBuilder.init(allocator);
    switch (o.*) {
        .int => {
            const value = o.int;
            sb.append(utils.intToString(allocator, value));
        },
        .cons => {
            const cell = o.cons;

            sb.append("(");
            var c = cell;
            while (true) {
                sb.append(objectToString(allocator, c.car));
                if (c.cdr == globals.nilObject) {
                    break;
                }
                if (c.cdr.* != .cons) {
                    sb.append(" . ");
                    sb.append(objectToString(allocator, c.cdr));
                    break;
                }
                sb.append(" ");
                c = c.cdr.cons;
            }
            sb.append(")");
        },
        .symbol => {
            const name = o.symbol;
            sb.append(name);
        },
        .function => {
            if (o.function.isMacro) {
                sb.append("<macro>");
            } else {
                sb.append("<function>");
            }
        },
        .token => {
            return o.token;
        },
        else => unreachable,
    }
    return sb.toString();
}

pub fn isdigit(c: u8) bool {
    return std.ascii.isDigit(c);
}

pub fn isalpha(c: u8) bool {
    return std.ascii.isAlphabetic(c);
}

pub fn isalnum(c: u8) bool {
    return std.ascii.isAlphanumeric(c);
}

pub fn iswhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

pub fn charToInt(c: u8) Int {
    return c - '0';
}

pub fn allocObject2(allocator: std.mem.Allocator, value: Object) *Object {
    const o = allocator.create(Object) catch @panic("Out of memory");
    o.* = value;
    return o;
}

const testing = std.testing;

test "utils" {
    try testing.expect(charToInt('6') == 6);
    try testing.expect(charToInt('4') == 4);
}
