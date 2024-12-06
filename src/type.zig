const Interpreter = @import("interpreter.zig").Interpreter;
const Object = @import("object.zig").Object;
const errors = @import("errors.zig");

pub const String = []const u8;

pub const Int = i32;

pub const PrimitiveFn = fn (*Interpreter, *Object) errors.evalError!*Object;
