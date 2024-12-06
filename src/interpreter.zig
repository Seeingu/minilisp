const std = @import("std");
const assert = std.debug.assert;
const StringBuilder = @import("StringBuilder.zig");
const String = @import("type.zig").String;
const Int = @import("type.zig").Int;
const Object = @import("object.zig").Object;
const Env = @import("env.zig").Env;
const utils = @import("utils.zig");
const errors = @import("errors.zig");
const globals = @import("globals.zig");
const PrimitiveFn = @import("type.zig").PrimitiveFn;

pub const Interpreter = struct {
    input: String,
    index: u32,
    symbols: *Object,
    allocator: std.mem.Allocator,
    env: *Env = undefined,

    const Self = @This();

    pub fn init(input: String, allocator: std.mem.Allocator) !*Self {
        const i: *Self = try allocator.create(Interpreter);
        const env = try allocator.create(Env);
        env.* = .{
            .vars = globals.nilObject,
            .outer = null,
        };
        i.* = .{
            .input = input,
            .allocator = allocator,
            .env = env,
            .index = 0,
            .symbols = globals.nilObject,
        };
        i.addConstant("t", globals.trueObject);
        i.addPrimitive("quote", &funQuote);
        i.addPrimitive("+", &funPlus);
        i.addPrimitive("=", &funEq);
        i.addPrimitive("list", &funList);
        i.addPrimitive("define", &funDefine);
        i.addPrimitive("setq", &funSetq);
        i.addPrimitive("if", &funIf);
        i.addPrimitive("lambda", &funLambda);
        i.addPrimitive("defun", &funDefun);
        i.addPrimitive("defmacro", &funDefmacro);
        i.addPrimitive("macroexpand", &funMacroexpand);

        return i;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self.env);
        self.allocator.destroy(self.symbols);
    }

    pub fn run(self: *Self) !String {
        var result: String = "";
        while (!self.isAtEnd()) {
            const parsed = try self.parse();
            result = utils.objectToString(self.allocator, try self.eval(parsed));
        }
        return result;
    }

    fn funQuote(_: *Self, args: *Object) errors.evalError!*Object {
        return args.cons.car;
    }

    fn funSetq(self: *Self, args: *Object) errors.evalError!*Object {
        var bind = self.find(args.cons.car);
        if (bind == globals.nilObject)
            return errors.evalError.SymbolNotFound;
        const value = try self.eval(args.cons.cdr.cons.car);
        bind.cons.cdr = value;
        return value;
    }

    fn funPlus(self: *Self, args: *Object) errors.evalError!*Object {
        var sum: i32 = 0;
        var c = try self.evalList(args);
        while (c != globals.nilObject) {
            const car = c.cons.car;
            if (car.* != .int) {
                return errors.evalError.PlusNonInt;
            }
            sum += car.int;
            c = c.cons.cdr;
        }

        return self.makeNumber(sum);
    }

    fn funEq(self: *Self, args: *Object) errors.evalError!*Object {
        const values = try self.evalList(args);
        const x = values.cons.car;
        const y = values.cons.cdr.cons.car;
        if (x.* != .int or y.* != .int) {
            @panic("funEq: x or y is not int");
        }
        if (x.int == y.int) {
            return globals.trueObject;
        } else {
            return globals.nilObject;
        }
    }

    fn funDefine(self: *Self, obj: *Object) errors.evalError!*Object {
        const sym = obj.cons.car;
        const value = try self.eval(obj.cons.cdr.cons.car);
        self.addVariable(sym, value);
        return value;
    }

    fn funList(self: *Self, args: *Object) errors.evalError!*Object {
        return self.evalList(args);
    }

    fn funIf(self: *Self, args: *Object) errors.evalError!*Object {
        const cond = try self.eval(args.cons.car);
        if (isTruthy(cond)) {
            const then = args.cons.cdr.cons.car;
            return self.eval(then);
        }
        const alternative = args.cons.cdr.cons.cdr;
        if (alternative == globals.nilObject) {
            return globals.nilObject;
        }
        return self.progn(alternative);
    }

    fn funLambda(self: *Self, args: *Object) errors.evalError!*Object {
        return self.handleFunction(args, false);
    }

    fn funDefun(self: *Self, args: *Object) errors.evalError!*Object {
        return self.handleDefun(args, false);
    }

    fn funDefmacro(self: *Self, args: *Object) errors.evalError!*Object {
        return self.handleDefun(args, true);
    }

    fn funMacroexpand(self: *Self, args: *Object) errors.evalError!*Object {
        const body = args.cons.car;
        return self.macroexpand(body);
    }

    fn handleDefun(self: *Self, list: *Object, isMacro: bool) *Object {
        const name = list.cons.car;
        const rest = list.cons.cdr;
        const fun = self.handleFunction(rest, isMacro);
        self.addVariable(name, fun);
        return fun;
    }

    fn handleFunction(self: *Self, list: *Object, isMacro: bool) *Object {
        const car = list.cons.car;
        const cdr = list.cons.cdr;
        return self.makeFunction(isMacro, car, cdr);
    }

    fn allocObject(self: *Self, value: Object) *Object {
        return utils.allocObject2(self.allocator, value);
    }

    fn makeFunction(self: *Self, isMacro: bool, params: *Object, body: *Object) *Object {
        return self.allocObject(
            .{ .function = .{
                .params = params,
                .body = body,
                .env = self.env,
                .isMacro = isMacro,
            } },
        );
    }

    fn isTruthy(obj: *Object) bool {
        if (obj == globals.nilObject) {
            return false;
        }
        switch (obj.*) {
            .int => return obj.int != 0,
            else => return true,
        }
    }

    fn progn(self: *Self, list: *Object) errors.evalError!*Object {
        var c = list;
        var result = globals.nilObject;
        while (c != globals.nilObject) {
            result = try self.eval(c.cons.car);
            c = c.cons.cdr;
        }
        return result;
    }

    fn peek(self: *Self) u8 {
        return self.input[self.index];
    }

    fn getchar(self: *Self) u8 {
        const c = self.input[self.index];
        self.index += 1;
        return c;
    }

    fn readNumber(self: *Self, first: Int) *Object {
        var value = first;
        while (!self.isAtEnd() and utils.isdigit(self.peek())) {
            const v = utils.charToInt(self.getchar());
            value = value * 10 + v;
        }
        return self.makeNumber(value);
    }

    fn makeNumber(self: *Self, v: Int) *Object {
        return self.allocObject(.{ .int = v });
    }

    fn readSymbol(self: *Self) *Object {
        const start = self.index - 1;
        while (!self.isAtEnd() and (utils.isalnum(self.peek()) or self.peek() == '-')) {
            _ = self.getchar();
        }
        const name = self.input[start..self.index];
        return self.intern(name);
    }

    fn makeSymbol(self: *Self, name: String) *Object {
        return self.allocObject(.{ .symbol = name });
    }

    fn intern(self: *Self, name: String) *Object {
        var p = self.symbols;
        while (p != globals.nilObject) {
            const cell = p.cons;
            const carName = cell.car.symbol;
            if (utils.stringEqual(carName, name)) {
                return cell.car;
            }
            p = p.cons.cdr;
        }
        const s = self.makeSymbol(name);
        self.symbols = self.cons(s, self.symbols);

        return s;
    }

    fn acons(self: *Self, x: *Object, y: *Object, a: *Object) *Object {
        return self.cons(self.cons(x, y), a);
    }

    fn cons(self: *Self, car: *Object, cdr: *Object) *Object {
        return self.allocObject(.{ .cons = .{ .car = car, .cdr = cdr } });
    }

    fn readQuote(self: *Self) errors.parseError!*Object {
        const sym = self.intern("quote");
        const name = try self.read();
        return self.cons(sym, self.cons(name, globals.nilObject));
    }

    fn readList(self: *Self) errors.parseError!*Object {
        const obj = try self.read();
        if (obj == globals.nilObject) {
            std.debug.print("readList top: unexpected nil\n", .{});
            return errors.parseError.Syntax;
        }
        if (obj == globals.parenObject) {
            return globals.nilObject;
        }
        const head = self.cons(obj, globals.nilObject);
        var tail: *Object = head;
        while (!self.isAtEnd()) {
            const o = try self.read();
            if (o == globals.parenObject) {
                return head;
            }
            if (o == globals.dotObject) {
                tail.cons.cdr = try self.read();
                if (try self.read() != globals.parenObject) {
                    std.debug.print("readList: expected ')'", .{});
                    return errors.parseError.Syntax;
                }
                return head;
            }
            tail.cons.cdr = self.cons(o, globals.nilObject);
            tail = tail.cons.cdr;
        }
        unreachable;
    }

    fn charToString(self: *Self, c: u8) String {
        return std.fmt.allocPrint(self.allocator, "{c}", .{c}) catch "";
    }

    fn read(self: *Self) errors.parseError!*Object {
        while (!self.isAtEnd()) {
            const c = self.getchar();
            if (utils.iswhitespace(c)) {
                continue;
            }
            if (c == ';') {
                while (!self.isAtEnd() and self.peek() != '\n') {
                    _ = self.getchar();
                }
                continue;
            }
            switch (c) {
                '\'' => return self.readQuote(),
                '(' => return self.readList(),
                ')' => return globals.parenObject,
                '-' => {
                    const cc = self.getchar();
                    if (utils.isdigit(cc)) {
                        return self.readNumber(-utils.charToInt(cc));
                    } else {
                        return errors.parseError.Syntax;
                    }
                },
                '.' => return globals.dotObject,
                else => {
                    if (utils.isSpecialSymbol(self.charToString(c))) {
                        return self.readSymbol();
                    }
                    if (utils.isalpha(c)) {
                        return self.readSymbol();
                    }
                    if (utils.isdigit(c)) {
                        return self.readNumber(utils.charToInt(c));
                    }
                },
            }
        }

        return globals.nilObject;
    }

    fn isAtEnd(self: *Self) bool {
        return self.index >= self.input.len;
    }

    fn makePrimtiive(self: *Self, fun: *const PrimitiveFn) *Object {
        return self.allocObject(.{ .primitive = fun });
    }

    fn addPrimitive(self: *Self, name: String, fun: *const PrimitiveFn) void {
        const sym = self.intern(name);
        const prim = self.makePrimtiive(fun);
        self.addVariable(sym, prim);
    }

    fn addConstant(self: *Self, name: String, value: *Object) void {
        const sym = self.intern(name);
        self.addVariable(sym, value);
    }

    pub fn parse(self: *Self) errors.parseError!*Object {
        return self.read();
    }

    fn apply(self: *Self, fun: *Object, args: *Object) errors.evalError!*Object {
        if (fun.* == .primitive) {
            return fun.primitive(self, args);
        }
        if (fun.* == .function) {
            const body = fun.function.body;
            const params = fun.function.params;
            const eargs = try self.evalList(args);
            self.pushEnv(fun.function.env, params, eargs);
            fun.function.env = self.env;
            const result = try self.progn(body);
            self.popEnv();
            return result;
        }
        @panic("Unknown function");
    }

    fn pushEnv(self: *Self, env: *Env, vars: *Object, values: *Object) void {
        // TODO: List length of vars and values assertion
        var map = globals.nilObject;

        var name = vars;
        var value = values;

        while (name != globals.nilObject) {
            const sym = name.cons.car;
            const v = value.cons.car;
            map = self.acons(sym, v, map);
            name = name.cons.cdr;
            value = value.cons.cdr;
        }
        self.env = self.makeEnv(map, env);
    }
    fn popEnv(self: *Self) void {
        if (self.env.outer) |outer| {
            self.env = outer;
        }
    }

    fn makeEnv(self: *Self, vars: *Object, outer: *Env) *Env {
        const env = self.allocator.create(Env) catch @panic("Out of memory");
        env.* = .{
            .vars = vars,
            .outer = outer,
        };
        return env;
    }

    fn macroexpand(self: *Self, obj: *Object) errors.evalError!*Object {
        if (obj.* != .cons) {
            return obj;
        }
        const car = obj.cons.car;
        if (car.* != .symbol) {
            return obj;
        }
        const bind = self.find(car);
        if (bind == globals.nilObject) {
            return obj;
        }
        if (bind.cons.cdr.* != .function or bind.cons.cdr.*.function.isMacro == false) {
            return obj;
        }
        const args = obj.cons.cdr;
        const body = bind.cons.cdr.function.body;
        const params = bind.cons.cdr.function.params;
        self.pushEnv(self.env, params, args);
        const result = try self.progn(body);
        self.popEnv();
        return result;
    }

    fn eval(self: *Self, obj: *Object) errors.evalError!*Object {
        switch (obj.*) {
            .int, .token => return obj,
            .function => return obj,
            .symbol => {
                const bind = self.find(obj);
                if (bind == globals.nilObject) {
                    return errors.evalError.SymbolNotFound;
                }
                return bind.cons.cdr;
            },
            .cons => {
                const expanded = try self.macroexpand(obj);
                if (expanded != obj) {
                    return self.eval(expanded);
                }
                const cell = obj.cons;
                const fun = try self.eval(cell.car);
                const args = cell.cdr;
                return self.apply(fun, args);
            },
            .primitive => {
                unreachable;
            },
        }
        return "";
    }

    fn evalList(self: *Self, obj: *Object) errors.evalError!*Object {
        var c = obj;

        var head: ?*Object = null;
        var tail: *Object = undefined;
        while (c != globals.nilObject) {
            const o = try self.eval(c.cons.car);
            if (head == null) {
                head = self.cons(o, globals.nilObject);
                tail = head.?;
            } else {
                tail.cons.cdr = self.cons(o, globals.nilObject);
                tail = tail.cons.cdr;
            }
            c = c.cons.cdr;
        }
        if (head == null) {
            return globals.nilObject;
        }
        return head.?;
    }

    fn findVariable(env: ?*Env, sym: *Object) *Object {
        if (env == null) {
            return globals.nilObject;
        }
        var vars = env.?.vars;
        while (vars != globals.nilObject) {
            const bind = vars.cons.car;
            if (utils.stringEqual(bind.cons.car.symbol, sym.symbol)) {
                return bind;
            }
            vars = vars.cons.cdr;
        }
        return globals.nilObject;
    }

    fn find(self: *Self, sym: *Object) *Object {
        var env: ?*Env = self.env;
        while (env != null) {
            const v = findVariable(env, sym);
            if (v != globals.nilObject) {
                return v;
            }
            env = env.?.outer;
        }
        std.debug.print("Variable not found: {s}\n", .{sym.symbol});
        return globals.nilObject;
    }

    fn debugPrintEnv(self: *Self, globalEnv: *Env) void {
        var sb = StringBuilder.init(self.allocator);
        var env: ?*Env = globalEnv;
        var index: i32 = 0;
        while (env != null) {
            sb.append("Env: ");
            sb.append(utils.intToString(self.allocator, index));
            var vars = env.?.vars;
            while (vars != globalEnv.nilObject) {
                const bind = vars.cons.car;
                sb.append("\t");
                sb.append(bind.cons.car.symbol);
                sb.append("\n");
                vars = vars.cons.cdr;
            }
            env = env.?.outer;
            index += 1;
        }
        std.debug.print("{s}\n", .{sb.toString()});
    }

    fn addVariable(self: *Self, sym: *Object, value: *Object) void {
        self.env.vars = self.acons(sym, value, self.env.vars);
    }
};
