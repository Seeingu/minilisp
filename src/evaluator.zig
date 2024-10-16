const std = @import("std");
const assert = std.debug.assert;
const SB = @import("StringBuilder.zig");
const StringBuilder = SB.StringBuilder;
const String = SB.String;

const ObjectType = enum(u32) {
    int,
    symbol,
    cell,
    token,
    primitive,
    function,
    macro,
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
};

const PrimitiveFn = fn (*Interpreter, *Object) *Object;
const Object = struct {
    type: ObjectType,
    // TODO: Use union
    value: ?Int = undefined,
    cell: ?Cell = undefined,
    name: ?String = undefined,
    symbol: ?String = undefined,
    fun: ?*const PrimitiveFn = undefined,
    frame: ?Frame = undefined,
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

fn objectToString(o: *Object, allocator: std.mem.Allocator) String {
    var sb = StringBuilder.init(allocator);
    switch (o.type) {
        .int => {
            const value = o.value.?;
            sb.append(intToString(allocator, value));
        },
        .cell => {
            const cell = o.cell.?;

            sb.append("(");
            var c = cell;
            while (true) {
                sb.append(objectToString(c.car, allocator));
                if (c.cdr == nilObject) {
                    break;
                }
                if (c.cdr.type != .cell) {
                    sb.append(" . ");
                    sb.append(objectToString(c.cdr, allocator));
                    break;
                }
                sb.append(" ");
                c = c.cdr.cell.?;
            }
            sb.append(")");
        },
        .symbol => {
            const name = o.name.?;
            sb.append(name);
        },
        .function => {
            sb.append("<function>");
        },
        .macro => {
            sb.append("<macro>");
        },
        .token => {
            if (o == trueObject) {
                return "t";
            } else if (o == nilObject) {
                return "()";
            } else {
                @panic("Unknown token");
            }
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

const Interpreter = struct {
    input: String,
    index: u32,
    symbols: *Object,
    allocator: std.mem.Allocator,
    env: *Env = undefined,

    pub fn init(input: String, allocator: std.mem.Allocator) !*Interpreter {
        const i: *Interpreter = try allocator.create(Interpreter);
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

    fn funQuote(_: *Interpreter, args: *Object) *Object {
        return args.cell.?.car;
    }

    fn funSetq(self: *Interpreter, args: *Object) *Object {
        var bind = self.find(args.cell.?.car);
        if (bind == nilObject)
            @panic("setq: symbol not found");
        const value = self.eval(args.cell.?.cdr.cell.?.car);
        bind.cell.?.cdr = value;
        return value;
    }

    fn funPlus(self: *Interpreter, args: *Object) *Object {
        var sum: i32 = 0;
        var c = self.evalList(args);
        while (c != nilObject) {
            const car = c.cell.?.car;
            if (car.type != .int) {
                @panic("funPlus: car is not int");
            }
            sum += car.value.?;
            c = c.cell.?.cdr;
        }
        return self.makeNumber(sum);
    }

    fn funEq(self: *Interpreter, args: *Object) *Object {
        const values = self.evalList(args);
        const x = values.cell.?.car;
        const y = values.cell.?.cdr.cell.?.car;
        if (x.type != .int or y.type != .int) {
            @panic("funEq: x or y is not int");
        }
        if (x.value == y.value) {
            return trueObject;
        } else {
            return nilObject;
        }
    }

    fn funDefine(self: *Interpreter, obj: *Object) *Object {
        const sym = obj.cell.?.car;
        const value = self.eval(obj.cell.?.cdr.cell.?.car);
        self.addVariable(sym, value);
        return value;
    }

    fn funList(self: *Interpreter, args: *Object) *Object {
        return self.evalList(args);
    }

    fn funIf(self: *Interpreter, args: *Object) *Object {
        const cond = self.eval(args.cell.?.car);
        if (isTruthy(cond)) {
            const then = args.cell.?.cdr.cell.?.car;
            return self.eval(then);
        }
        const alternative = args.cell.?.cdr.cell.?.cdr;
        if (alternative == nilObject) {
            return nilObject;
        }
        return self.progn(alternative);
    }

    fn funLambda(self: *Interpreter, args: *Object) *Object {
        return self.handleFunction(args, .function);
    }

    fn funDefun(self: *Interpreter, args: *Object) *Object {
        return self.handleDefun(args, .function);
    }

    fn funDefmacro(self: *Interpreter, args: *Object) *Object {
        return self.handleDefun(args, .macro);
    }

    fn funMacroexpand(self: *Interpreter, args: *Object) *Object {
        const body = args.cell.?.car;
        return self.macroexpand(body);
    }

    fn handleDefun(self: *Interpreter, list: *Object, objectType: ObjectType) *Object {
        const name = list.cell.?.car;
        const rest = list.cell.?.cdr;
        const fun = self.handleFunction(rest, objectType);
        self.addVariable(name, fun);
        return fun;
    }

    fn handleFunction(self: *Interpreter, list: *Object, objectType: ObjectType) *Object {
        const car = list.cell.?.car;
        const cdr = list.cell.?.cdr;
        return self.makeFunction(objectType, car, cdr);
    }

    fn makeFunction(self: *Interpreter, objectType: ObjectType, params: *Object, body: *Object) *Object {
        const object = self.allocator.create(Object) catch @panic("Out of memory");
        object.* = .{
            .type = objectType,
            .frame = .{
                .params = params,
                .body = body,
                .env = self.env,
            },
        };
        return object;
    }

    fn isTruthy(obj: *Object) bool {
        if (obj == nilObject) {
            return false;
        }
        if (obj.type == .int) {
            return obj.value != 0;
        }
        return true;
    }

    fn progn(self: *Interpreter, list: *Object) *Object {
        var c = list;
        var result = nilObject;
        while (c != nilObject) {
            result = self.eval(c.cell.?.car);
            c = c.cell.?.cdr;
        }
        return result;
    }

    fn peek(self: *Interpreter) u8 {
        return self.input[self.index];
    }

    fn getchar(self: *Interpreter) u8 {
        const c = self.input[self.index];
        self.index += 1;
        return c;
    }

    fn readNumber(self: *Interpreter, first: Int) *Object {
        var value = first;
        while (!self.isAtEnd() and isdigit(self.peek())) {
            const v = charToInt(self.getchar());
            value = value * 10 + v;
        }
        return self.makeNumber(value);
    }

    fn makeNumber(self: *Interpreter, v: Int) *Object {
        const obj = self.allocator.create(Object) catch @panic("Out of memory");
        obj.* = .{
            .type = .int,
            .value = v,
        };
        return obj;
    }

    fn readSymbol(self: *Interpreter) *Object {
        const start = self.index - 1;
        while (!self.isAtEnd() and (isalnum(self.peek()) or self.peek() == '-')) {
            _ = self.getchar();
        }
        const name = self.input[start..self.index];
        return self.intern(name);
    }

    fn symbol(self: *Interpreter, name: String) *Object {
        const obj = self.allocator.create(Object) catch @panic("Out of memory");
        obj.type = .symbol;
        obj.name = name;
        return obj;
    }

    fn intern(self: *Interpreter, name: String) *Object {
        var p = self.symbols;
        while (p != nilObject) {
            const cell = p.cell.?;
            const carName = cell.car.name.?;
            if (std.mem.eql(u8, carName, name)) {
                return cell.car;
            }
            p = p.cell.?.cdr;
        }
        const s = self.symbol(name);
        self.symbols = self.cons(s, self.symbols);

        return s;
    }

    fn acons(self: *Interpreter, x: *Object, y: *Object, a: *Object) *Object {
        return self.cons(self.cons(x, y), a);
    }

    fn cons(self: *Interpreter, car: *Object, cdr: *Object) *Object {
        const cell = self.allocator.create(Object) catch @panic("Out of memory");
        cell.type = .cell;
        cell.cell = Cell{ .car = car, .cdr = cdr };
        return cell;
    }

    fn readQuote(self: *Interpreter) *Object {
        const sym = self.intern("quote");
        const name = self.read();
        if (name.type == .symbol) {
            assert(name.name != null);
        }
        if (name.type == .int) {
            assert(name.value != null);
        }
        return self.cons(sym, self.cons(name, nilObject));
    }

    fn readList(self: *Interpreter) *Object {
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
                tail.cell.?.cdr = self.read();
                if (self.read() != parenObject) {
                    @panic("readList: expected ')'");
                }
                return head;
            }
            tail.cell.?.cdr = self.cons(o, nilObject);
            tail = tail.cell.?.cdr;
        }
        @panic("readList: unreachable");
    }

    fn charToString(self: *Interpreter, c: u8) String {
        return std.fmt.allocPrint(self.allocator, "{c}", .{c}) catch "";
    }

    fn read(self: *Interpreter) *Object {
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

    fn isAtEnd(self: *Interpreter) bool {
        return self.index >= self.input.len;
    }

    fn makePrimtiive(self: *Interpreter, fun: *const PrimitiveFn) *Object {
        const obj = self.allocator.create(Object) catch @panic("Out of memory");
        obj.* = .{
            .type = .primitive,
            .fun = fun,
        };
        return obj;
    }

    fn addPrimitive(self: *Interpreter, name: String, fun: *const PrimitiveFn) void {
        const sym = self.intern(name);
        const prim = self.makePrimtiive(fun);
        self.addVariable(sym, prim);
    }

    fn addConstant(self: *Interpreter, name: String, value: *Object) void {
        const sym = self.intern(name);
        self.addVariable(sym, value);
    }

    pub fn parse(self: *Interpreter) *Object {
        return self.read();
    }

    fn apply(self: *Interpreter, fun: *Object, args: *Object) *Object {
        if (fun.type == .primitive) {
            return fun.fun.?(self, args);
        }
        if (fun.type == .function) {
            const body = fun.frame.?.body;
            const params = fun.frame.?.params;
            const eargs = self.evalList(args);
            self.pushEnv(fun.frame.?.env, params, eargs);
            const result = self.progn(body);
            self.popEnv();
            return result;
        }
        @panic("Unknown function");
    }

    fn pushEnv(self: *Interpreter, env: *Env, vars: *Object, values: *Object) void {
        // TODO: List length of vars and values assertion
        var map = nilObject;

        var name = vars;
        var value = values;

        while (name != nilObject) {
            const sym = name.cell.?.car;
            const v = value.cell.?.car;
            map = self.acons(sym, v, map);
            name = name.cell.?.cdr;
            value = value.cell.?.cdr;
        }
        if (env != self.env) {
            env.outer = self.env;
        }
        self.env = self.makeEnv(map, env);
    }
    fn popEnv(self: *Interpreter) void {
        self.env = self.env.outer.?;
    }

    fn makeEnv(self: *Interpreter, vars: *Object, outer: *Env) *Env {
        const env = self.allocator.create(Env) catch @panic("Out of memory");
        env.* = .{
            .vars = vars,
            .outer = outer,
        };
        return env;
    }

    fn macroexpand(self: *Interpreter, obj: *Object) *Object {
        if (obj.type != .cell) {
            return obj;
        }
        const car = obj.cell.?.car;
        if (car.type != .symbol) {
            return obj;
        }
        const bind = self.find(car);
        if (bind == nilObject) {
            return obj;
        }
        if (bind.cell.?.cdr.type != .macro) {
            return obj;
        }
        const args = obj.cell.?.cdr;
        const body = bind.cell.?.cdr.frame.?.body;
        const params = bind.cell.?.cdr.frame.?.params;
        self.pushEnv(self.env, params, args);
        const result = self.progn(body);
        self.popEnv();
        return result;
    }

    fn eval(self: *Interpreter, obj: *Object) *Object {
        switch (obj.type) {
            .int, .token => return obj,
            .function, .macro => return obj,
            .symbol => {
                const bind = self.find(obj);
                if (bind == nilObject) {
                    @panic("eval: symbol not found");
                }
                return bind.cell.?.cdr;
            },
            .cell => {
                const expanded = self.macroexpand(obj);
                if (expanded != obj) {
                    return self.eval(expanded);
                }
                const cell = obj.cell.?;
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

    fn evalList(self: *Interpreter, obj: *Object) *Object {
        var c = obj;

        var head: ?*Object = null;
        var tail: *Object = undefined;
        while (c != nilObject) {
            const o = self.eval(c.cell.?.car);
            if (head == null) {
                head = self.cons(o, nilObject);
                tail = head.?;
            } else {
                tail.cell.?.cdr = self.cons(o, nilObject);
                tail = tail.cell.?.cdr;
            }
            c = c.cell.?.cdr;
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
            const bind = vars.cell.?.car;
            if (stringEqual(bind.cell.?.car.name.?, sym.name.?)) {
                return bind;
            }
            vars = vars.cell.?.cdr;
        }
        return nilObject;
    }

    fn find(self: *Interpreter, sym: *Object) *Object {
        var env: ?*Env = self.env;
        while (env != null) {
            const v = findVariable(env, sym);
            if (v != nilObject) {
                return v;
            }
            env = env.?.outer;
        }
        std.debug.print("Variable not found: {s}\n", .{sym.name.?});
        return nilObject;
    }

    fn debugPrintEnv(self: *Interpreter, globalEnv: *Env) void {
        var sb = StringBuilder.init(self.allocator);
        var env: ?*Env = globalEnv;
        var index: i32 = 0;
        while (env != null) {
            sb.append("Env: ");
            sb.append(intToString(self.allocator, index));
            var vars = env.?.vars;
            while (vars != nilObject) {
                const bind = vars.cell.?.car;
                sb.append("\t");
                sb.append(bind.cell.?.car.name.?);
                sb.append("\n");
                vars = vars.cell.?.cdr;
            }
            env = env.?.outer;
            index += 1;
        }
        std.debug.print("{s}\n", .{sb.toString()});
    }

    fn addVariable(self: *Interpreter, sym: *Object, value: *Object) void {
        self.env.vars = self.acons(sym, value, self.env.vars);
    }
};

pub fn run(source: String) !String {
    const allocator = std.heap.page_allocator;

    nilObject = try allocator.create(Object);
    nilObject.type = .token;
    parenObject = try allocator.create(Object);
    parenObject.type = .token;
    dotObject = try allocator.create(Object);
    dotObject.type = .token;
    trueObject = try allocator.create(Object);
    trueObject.type = .token;

    var e = try Interpreter.init(source, allocator);
    var result: String = "";
    while (!e.isAtEnd()) {
        const parsed = e.parse();
        std.debug.print("Parsed: {s}\n", .{objectToString(parsed, allocator)});
        result = objectToString(e.eval(parsed), allocator);
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
        // TODO: Fix this, return nil currently
        // .{ .input = "; 2\n5 ; 3", .expected = "5" },
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
        // TODO: Fix env issue
        // .{ .input = "(defun f (x) (if (= x 0) 0 (+ (f (+ x -1)) x))) (f 10)", .expected = "55" },
    };
    for (testcases) |tc| {
        std.debug.print("Input: {s}, ", .{tc.input});
        const r = try run(tc.input);
        std.debug.print("Result: {s}\n", .{r});
        try testing.expect(stringEqual(tc.expected, r));
    }
}
