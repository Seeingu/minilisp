const Int = @import("type.zig").Int;
const types = @import("type.zig");
const String = types.String;
const PrimitiveFn = types.PrimitiveFn;
const Frame = @import("env.zig").Frame;

pub const ObjectType = enum {
    int,
    symbol,
    cons,
    token,
    primitive,
    function,
};

pub const Object = union(ObjectType) {
    int: Int,
    symbol: String,
    cons: Cons,
    token: String,
    primitive: *const PrimitiveFn,
    function: Frame,
};

pub const Cons = struct { car: *Object, cdr: *Object };
