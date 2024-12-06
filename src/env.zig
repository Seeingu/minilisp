const Object = @import("object.zig").Object;

pub const Env = struct {
    vars: *Object,
    outer: ?*Env,
};

pub const Frame = struct {
    params: *Object,
    body: *Object,
    env: *Env,
    isMacro: bool = false,
};
