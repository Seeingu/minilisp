const std = @import("std");
const assert = std.debug.assert;
const StringBuilder = @import("StringBuilder.zig");
const String = @import("type.zig").String;

const ObjectType = enum {
    int,
    symbol,
    cell,
    token,
    primitive,
    function,
};

const Int = i32;

const Cell = struct { car: *Object, cdr: *Object };

const Env = struct {
    vars: *Object,
    outer: ?*Env,
};

const Frame = struct {
    params: *Object,
    body: *Object,
    env: *Env,
    isMacro: bool = false,
};

const PrimitiveFn = fn (*Interpreter, *Object) *Object;
const Object = union(ObjectType) {
    int: Int,
    symbol: String,
    cell: Cell,
    token: String,
    primitive: *const PrimitiveFn,
    function: Frame,
};

fn intToString(allocator: std.mem.Allocator, i: i32) String {
    return std.fmt.allocPrint(allocator, "{}", .{i}) catch "";
}
fn stringEqual(s1: String, s2: String) bool {
    return std.mem.eql(u8, s1, s2);
}

fn isSpecialSymbol(s: String) bool {
    return std.mem.indexOf(u8, "+-*/=", s) != null;
}

fn objectToString(allocator: std.mem.Allocator, o: *Object) String {
    var sb = StringBuilder.init(allocator);
    switch (o.*) {
        .int => {
            const value = o.int;
            sb.append(intToString(allocator, value));
        },
        .cell => {
            const cell = o.cell;

            sb.append("(");
            var c = cell;
            while (true) {
                sb.append(objectToString(allocator, c.car));
                if (c.cdr == nilObject) {
                    break;
                }
                if (c.cdr.* != .cell) {
                    sb.append(" . ");
                    sb.append(objectToString(allocator, c.cdr));
                    break;
                }
                sb.append(" ");
                c = c.cdr.cell;
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
        else => @panic("Unknown object type"),
    }
    return sb.toString();
}

fn isdigit(c: u8) bool {
    return std.ascii.isDigit(c);
}

fn isalpha(c: u8) bool {
    return std.ascii.isAlphabetic(c);
}

fn isalnum(c: u8) bool {
    return std.ascii.isAlphanumeric(c);
}

fn iswhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

fn charToInt(c: u8) Int {
    return c - '0';
}

var nilObject: *Object = undefined;
var dotObject: *Object = undefined;
var parenObject: *Object = undefined;
var trueObject: *Object = undefined;

fn allocObject2(allocator: std.mem.Allocator, value: Object) *Object {
    const o = allocator.create(Object) catch @panic("Out of memory");
    o.* = value;
    return o;
}

const Interpreter = struct {
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
            .vars = nilObject,
            .outer = null,
        };
        i.* = .{
            .input = input,
            .allocator = allocator,
            .env = env,
            .index = 0,
            .symbols = nilObject,
        };
        i.addConstant("t", trueObject);
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

    fn funQuote(_: *Self, args: *Object) *Object {
        return args.cell.car;
    }

    fn funSetq(self: *Self, args: *Object) *Object {
        var bind = self.find(args.cell.car);
        if (bind == nilObject)
            @panic("setq: symbol not found");
        const value = self.eval(args.cell.cdr.cell.car);
        bind.cell.cdr = value;
        return value;
    }

    fn funPlus(self: *Self, args: *Object) *Object {
        var sum: i32 = 0;
        var c = self.evalList(args);
        while (c != nilObject) {
            const car = c.cell.car;
            if (car.* != .int) {
                @panic("funPlus: car is not int");
            }
            sum += car.int;
            c = c.cell.cdr;
        }

        return self.makeNumber(sum);
    }

    fn funEq(self: *Self, args: *Object) *Object {
        const values = self.evalList(args);
        const x = values.cell.car;
        const y = values.cell.cdr.cell.car;
        if (x.* != .int or y.* != .int) {
            @panic("funEq: x or y is not int");
        }
        if (x.int == y.int) {
            return trueObject;
        } else {
            return nilObject;
        }
    }

    fn funDefine(self: *Self, obj: *Object) *Object {
        const sym = obj.cell.car;
        const value = self.eval(obj.cell.cdr.cell.car);
        self.addVariable(sym, value);
        return value;
    }

    fn funList(self: *Self, args: *Object) *Object {
        return self.evalList(args);
    }

    fn funIf(self: *Self, args: *Object) *Object {
        const cond = self.eval(args.cell.car);
        if (isTruthy(cond)) {
            const then = args.cell.cdr.cell.car;
            return self.eval(then);
        }
        const alternative = args.cell.cdr.cell.cdr;
        if (alternative == nilObject) {
            return nilObject;
        }
        return self.progn(alternative);
    }

    fn funLambda(self: *Self, args: *Object) *Object {
        return self.handleFunction(args, false);
    }

    fn funDefun(self: *Self, args: *Object) *Object {
        return self.handleDefun(args, false);
    }

    fn funDefmacro(self: *Self, args: *Object) *Object {
        return self.handleDefun(args, true);
    }

    fn funMacroexpand(self: *Self, args: *Object) *Object {
        const body = args.cell.car;
        return self.macroexpand(body);
    }

    fn handleDefun(self: *Self, list: *Object, isMacro: bool) *Object {
        const name = list.cell.car;
        const rest = list.cell.cdr;
        const fun = self.handleFunction(rest, isMacro);
        self.addVariable(name, fun);
        return fun;
    }

    fn handleFunction(self: *Self, list: *Object, isMacro: bool) *Object {
        const car = list.cell.car;
        const cdr = list.cell.cdr;
        return self.makeFunction(isMacro, car, cdr);
    }

    fn allocObject(self: *Self, value: Object) *Object {
        return allocObject2(self.allocator, value);
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
        if (obj == nilObject) {
            return false;
        }
        switch (obj.*) {
            .int => return obj.int != 0,
            else => return true,
        }
    }

    fn progn(self: *Self, list: *Object) *Object {
        var c = list;
        var result = nilObject;
        while (c != nilObject) {
            result = self.eval(c.cell.car);
            c = c.cell.cdr;
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
        while (!self.isAtEnd() and isdigit(self.peek())) {
            const v = charToInt(self.getchar());
            value = value * 10 + v;
        }
        return self.makeNumber(value);
    }

    fn makeNumber(self: *Self, v: Int) *Object {
        return self.allocObject(.{ .int = v });
    }

    fn readSymbol(self: *Self) *Object {
        const start = self.index - 1;
        while (!self.isAtEnd() and (isalnum(self.peek()) or self.peek() == '-')) {
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
        while (p != nilObject) {
            const cell = p.cell;
            const carName = cell.car.symbol;
            if (std.mem.eql(u8, carName, name)) {
                return cell.car;
            }
            p = p.cell.cdr;
        }
        const s = self.makeSymbol(name);
        self.symbols = self.cons(s, self.symbols);

        return s;
    }

    fn acons(self: *Self, x: *Object, y: *Object, a: *Object) *Object {
        return self.cons(self.cons(x, y), a);
    }

    fn cons(self: *Self, car: *Object, cdr: *Object) *Object {
        return self.allocObject(.{ .cell = .{ .car = car, .cdr = cdr } });
    }

    fn readQuote(self: *Self) *Object {
        const sym = self.intern("quote");
        const name = self.read();
        return self.cons(sym, self.cons(name, nilObject));
    }

    fn readList(self: *Self) *Object {
        const obj = self.read();
        if (obj == nilObject) {
            @panic("readList top: unexpected nil");
        }
        if (obj == parenObject) {
            return nilObject;
        }
        const head = self.cons(obj, nilObject);
        var tail: *Object = head;
        while (!self.isAtEnd()) {
            const o = self.read();
            if (o == parenObject) {
                return head;
            }
            if (o == dotObject) {
                tail.cell.cdr = self.read();
                if (self.read() != parenObject) {
                    @panic("readList: expected ')'");
                }
                return head;
            }
            tail.cell.cdr = self.cons(o, nilObject);
            tail = tail.cell.cdr;
        }
        @panic("readList: unreachable");
    }

    fn charToString(self: *Self, c: u8) String {
        return std.fmt.allocPrint(self.allocator, "{c}", .{c}) catch "";
    }

    fn read(self: *Self) *Object {
        while (!self.isAtEnd()) {
            const c = self.getchar();
            if (iswhitespace(c)) {
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
                ')' => return parenObject,
                '-' => {
                    const cc = self.getchar();
                    if (isdigit(cc)) {
                        return self.readNumber(-charToInt(cc));
                    } else {
                        @panic("minus");
                    }
                },
                '.' => return dotObject,
                else => {
                    if (isSpecialSymbol(self.charToString(c))) {
                        return self.readSymbol();
                    }
                    if (isalpha(c)) {
                        return self.readSymbol();
                    }
                    if (isdigit(c)) {
                        return self.readNumber(charToInt(c));
                    }
                },
            }
        }

        return nilObject;
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

    pub fn parse(self: *Self) *Object {
        return self.read();
    }

    fn apply(self: *Self, fun: *Object, args: *Object) *Object {
        if (fun.* == .primitive) {
            return fun.primitive(self, args);
        }
        if (fun.* == .function) {
            const body = fun.function.body;
            const params = fun.function.params;
            const eargs = self.evalList(args);
            self.pushEnv(fun.function.env, params, eargs);
            fun.function.env = self.env;
            const result = self.progn(body);
            self.popEnv();
            return result;
        }
        @panic("Unknown function");
    }

    fn pushEnv(self: *Self, env: *Env, vars: *Object, values: *Object) void {
        // TODO: List length of vars and values assertion
        var map = nilObject;

        var name = vars;
        var value = values;

        while (name != nilObject) {
            const sym = name.cell.car;
            const v = value.cell.car;
            map = self.acons(sym, v, map);
            name = name.cell.cdr;
            value = value.cell.cdr;
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

    fn macroexpand(self: *Self, obj: *Object) *Object {
        if (obj.* != .cell) {
            return obj;
        }
        const car = obj.cell.car;
        if (car.* != .symbol) {
            return obj;
        }
        const bind = self.find(car);
        if (bind == nilObject) {
            return obj;
        }
        if (bind.cell.cdr.* != .function or bind.cell.cdr.*.function.isMacro == false) {
            return obj;
        }
        const args = obj.cell.cdr;
        const body = bind.cell.cdr.function.body;
        const params = bind.cell.cdr.function.params;
        self.pushEnv(self.env, params, args);
        const result = self.progn(body);
        self.popEnv();
        return result;
    }

    fn eval(self: *Self, obj: *Object) *Object {
        switch (obj.*) {
            .int, .token => return obj,
            .function => return obj,
            .symbol => {
                const bind = self.find(obj);
                if (bind == nilObject) {
                    @panic("eval: symbol not found");
                }
                return bind.cell.cdr;
            },
            .cell => {
                const expanded = self.macroexpand(obj);
                if (expanded != obj) {
                    return self.eval(expanded);
                }
                const cell = obj.cell;
                const fun = self.eval(cell.car);
                const args = cell.cdr;
                return self.apply(fun, args);
            },
            .primitive => {
                @panic("primitive should not been evaluated");
            },
        }
        return "";
    }

    fn evalList(self: *Self, obj: *Object) *Object {
        var c = obj;

        var head: ?*Object = null;
        var tail: *Object = undefined;
        while (c != nilObject) {
            const o = self.eval(c.cell.car);
            if (head == null) {
                head = self.cons(o, nilObject);
                tail = head.?;
            } else {
                tail.cell.cdr = self.cons(o, nilObject);
                tail = tail.cell.cdr;
            }
            c = c.cell.cdr;
        }
        if (head == null) {
            return nilObject;
        }
        return head.?;
    }

    fn findVariable(env: ?*Env, sym: *Object) *Object {
        if (env == null) {
            return nilObject;
        }
        var vars = env.?.vars;
        while (vars != nilObject) {
            const bind = vars.cell.car;
            if (stringEqual(bind.cell.car.symbol, sym.symbol)) {
                return bind;
            }
            vars = vars.cell.cdr;
        }
        return nilObject;
    }

    fn find(self: *Self, sym: *Object) *Object {
        var env: ?*Env = self.env;
        while (env != null) {
            const v = findVariable(env, sym);
            if (v != nilObject) {
                return v;
            }
            env = env.?.outer;
        }
        std.debug.print("Variable not found: {s}\n", .{sym.symbol});
        return nilObject;
    }

    fn debugPrintEnv(self: *Self, globalEnv: *Env) void {
        var sb = StringBuilder.init(self.allocator);
        var env: ?*Env = globalEnv;
        var index: i32 = 0;
        while (env != null) {
            sb.append("Env: ");
            sb.append(intToString(self.allocator, index));
            var vars = env.?.vars;
            while (vars != nilObject) {
                const bind = vars.cell.car;
                sb.append("\t");
                sb.append(bind.cell.car.symbol);
                sb.append("\n");
                vars = vars.cell.cdr;
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

pub fn run(source: String) !String {
    const allocator = std.heap.page_allocator;
    nilObject = allocObject2(allocator, .{ .token = "()" });
    parenObject = allocObject2(allocator, .{ .token = "paren" });
    dotObject = allocObject2(allocator, .{ .token = "." });
    trueObject = allocObject2(allocator, .{ .token = "t" });

    var e = try Interpreter.init(source, allocator);
    defer e.deinit();
    var result: String = "";
    while (!e.isAtEnd()) {
        const parsed = e.parse();
        std.debug.print("Parsed: {s}\n", .{objectToString(allocator, parsed)});
        result = objectToString(allocator, e.eval(parsed));
    }
    return result;
}

const testing = std.testing;

test "utils" {
    try testing.expect(charToInt('6') == 6);
    try testing.expect(charToInt('4') == 4);
}

test "eval" {
    const TestCase = struct {
        input: String,
        expected: String,
    };
    const testcases = [_]TestCase{
        .{ .input = "42", .expected = "42" },
        .{ .input = "0", .expected = "0" },
        .{ .input = "-1", .expected = "-1" },
        .{ .input = "'a", .expected = "a" },
        .{ .input = "(quote a)", .expected = "a" },
        .{ .input = "'(+ 1 2)", .expected = "(+ 1 2)" },
        .{ .input = "(+ 1 2)", .expected = "3" },
        .{ .input = "(+ 1 -3)", .expected = "-2" },
        .{ .input = "'(a b c)", .expected = "(a b c)" },
        .{ .input = "(list 'a 'b 'c)", .expected = "(a b c)" },
        .{ .input = "'(a b . c)", .expected = "(a b . c)" },
        .{ .input = "; 2\n5", .expected = "5" },
        .{ .input = "(define x 7) x", .expected = "7" },
        .{ .input = "(define x 7) (+ x 3)", .expected = "10" },
        .{ .input = "(define + 7) +", .expected = "7" },
        .{ .input = "(define x 7) (setq x 11) x", .expected = "11" },
        .{ .input = "(if 1 'a)", .expected = "a" },
        .{ .input = "(if () 'a)", .expected = "()" },
        .{ .input = "(if 1 'a 'b)", .expected = "a" },
        .{ .input = "(if 0 'a 'b)", .expected = "b" },
        .{ .input = "(if 'x 'a 'b)", .expected = "a" },
        .{ .input = "(if () 'a 'b)", .expected = "b" },
        .{ .input = "(if () 'a 'b 'c)", .expected = "c" },
        .{ .input = "(= 3 3)", .expected = "t" },
        .{ .input = "(= 3 2)", .expected = "()" },
        .{ .input = "(lambda (x) x)", .expected = "<function>" },
        .{ .input = "((lambda () t))", .expected = "t" },
        .{ .input = "((lambda (x) (+ x x x)) 3)", .expected = "9" },
        .{ .input = "(defun double (x) (+ x x)) (double 6)", .expected = "12" },
        .{
            .input = "(defun call (f) ((lambda (var) (f)) 5)) ((lambda (var) (call (lambda () var))) 3)",
            .expected = "3",
        },
        .{
            .input =
            \\(define counter
            \\((lambda (val)
            \\(lambda () (setq val (+ val 1)) val))
            \\0))
            \\(counter)
            \\(counter)
            \\(counter)
            ,
            .expected = "3",
        },
        .{
            .input = "(defmacro if-zero (x then) (list 'if (list '= x 0) then)) (if-zero 0 42)",
            .expected = "42",
        },
        .{ .input = "(defmacro seven () 7) ((lambda () (seven)))", .expected = "7" },
        .{
            .input = "(defmacro if-zero (x then) (list 'if (list '= x 0) then)) (macroexpand (if-zero x (print x)))",
            .expected = "(if (= x 0) (print x))",
        },
        .{ .input = "(defun f (x) (if (= x 0) 0 (+ (f (+ x -1)) x))) (f 10)", .expected = "55" },
    };
    for (testcases) |tc| {
        std.debug.print("Input: {s}, ", .{tc.input});
        const r = try run(tc.input);
        std.debug.print("Result: {s}\n", .{r});
        try testing.expect(stringEqual(tc.expected, r));
    }
}
