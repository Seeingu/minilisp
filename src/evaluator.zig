const std = @import("std");
const assert = std.debug.assert;
const SB = @import("StringBuilder.zig");
const StringBuilder = SB.StringBuilder;
const String = SB.String;

const ObjectType = enum(u32) {
    nil,
    int,
    symbol,
    cell,
    token,
};

const Int = i32;
const isDebug = false;

const Cell = struct { car: *const Object, cdr: *const Object };

const Object = struct {
    type: ObjectType,
    // TODO: Use union
    value: ?Int = undefined,
    cell: ?Cell = undefined,
    name: ?String = undefined,
    symbol: ?String = undefined,
};

fn intToString(allocator: std.mem.Allocator, i: i32) String {
    return std.fmt.allocPrint(allocator, "{}", .{i}) catch "";
}
fn stringEqual(s1: String, s2: String) bool {
    return std.mem.eql(u8, s1, s2);
}

fn isSpecialSymbol(s: String) bool {
    return std.mem.indexOf(u8, "+-*/", s) != null;
}

fn printObject(o: *const Object, allocator: std.mem.Allocator) String {
    var sb = StringBuilder.init(allocator);
    switch (o.type) {
        .int => {
            const value = o.value.?;
            if (isDebug) {
                sb.append("int: ");
            }
            sb.append(intToString(allocator, value));
        },
        .cell => {
            const cell = o.cell.?;

            if (isDebug) {
                sb.append("cell: {\n");
                sb.append("  car: ");
                sb.append(printObject(cell.car, allocator));

                sb.append("  cdr: ");
                sb.append(printObject(cell.cdr, allocator));
                sb.append("\n}");
            } else {
                sb.append("(");
                var c = cell;
                while (true) {
                    sb.append(printObject(c.car, allocator));
                    if (c.cdr == nilObject) {
                        break;
                    }
                    if (c.cdr.type != .cell) {
                        sb.append(" . ");
                        sb.append(printObject(c.cdr, allocator));
                        break;
                    }
                    sb.append(" ");
                    c = c.cdr.cell.?;
                }
                sb.append(")");
            }
        },
        .nil => {
            sb.append("nil");
        },
        .symbol => {
            const name = o.name.?;
            if (isDebug) {
                sb.append("symbol: ");
                sb.append(name);
                sb.append("\n");
            } else {
                sb.append(name);
            }
        },
        .token => {
            sb.append(o.name.?);
        },
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

const nilObject = &Object{
    .type = .nil,
};
const dotObject = &Object{
    .type = .token,
    .name = ".",
};
const parenObject = &Object{
    .type = .token,
    .name = ")",
};

const Interpreter = struct {
    input: String,
    index: u32 = 0,
    symbols: *const Object = nilObject,
    allocator: std.mem.Allocator,

    fn peek(self: *Interpreter) u8 {
        return self.input[self.index];
    }

    fn getchar(self: *Interpreter) u8 {
        const c = self.input[self.index];
        self.index += 1;
        return c;
    }

    fn readNumber(self: *Interpreter, first: Int) *const Object {
        var value = first;
        while (!self.isAtEnd() and isdigit(self.peek())) {
            const v = charToInt(self.getchar());
            value = value * 10 + v;
        }
        return self.makeNumber(value);
    }

    fn makeNumber(self: *Interpreter, v: Int) *const Object {
        const obj = self.allocator.create(Object) catch @panic("Out of memory");
        obj.type = .int;
        obj.value = v;
        return obj;
    }

    fn readSymbol(self: *Interpreter) *const Object {
        const start = self.index - 1;
        while (!self.isAtEnd() and isalnum(self.peek())) {
            _ = self.getchar();
        }
        const name = self.input[start..self.index];
        return self.intern(name);
    }

    fn symbol(self: *Interpreter, name: String) *const Object {
        const obj = self.allocator.create(Object) catch @panic("Out of memory");
        obj.type = .symbol;
        obj.name = name;
        return obj;
    }

    fn intern(self: *Interpreter, name: String) *const Object {
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
        // std.debug.print("new symbol: {s}\n", .{printObject(s, self.allocator) catch ""});
        self.symbols = self.cons(s, self.symbols);
        // std.debug.print("symbols: {s}\n", .{printObject(self.symbols, self.allocator) catch ""});

        return s;
    }

    fn cons(self: *Interpreter, car: *const Object, cdr: *const Object) *const Object {
        const cell = self.allocator.create(Object) catch @panic("Out of memory");
        cell.type = .cell;
        cell.cell = Cell{ .car = car, .cdr = cdr };
        return cell;
    }

    fn readQuote(self: *Interpreter) *const Object {
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

    fn readList(self: *Interpreter) *const Object {
        const obj = self.read();
        // std.debug.print("list: {s}\n", .{printObject(obj, self.allocator) catch ""});
        if (obj == nilObject) {
            @panic("read list top: unexpected nil");
        }
        if (obj == parenObject) {
            return nilObject;
        }
        const head = self.cons(obj, nilObject);
        var tail: *Object = @constCast(head);
        while (!self.isAtEnd()) {
            const o = self.read();
            if (o == nilObject) {
                @panic("read list unexpected nil");
            }
            if (o == parenObject) {
                return head;
            }
            tail.cell.?.cdr = self.cons(o, nilObject);
            tail = @constCast(tail.cell.?.cdr);
        }
        @panic("readList: unreachable");
    }

    fn charToString(self: *Interpreter, c: u8) String {
        return std.fmt.allocPrint(self.allocator, "{c}", .{c}) catch "";
    }

    fn read(self: *Interpreter) *const Object {
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
                '-' => {
                    const cc = self.getchar();
                    if (isdigit(cc)) {
                        return self.readNumber(-charToInt(cc));
                    } else {
                        @panic("minus");
                    }
                },
                ')' => return parenObject,
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

    pub fn parse(self: *Interpreter) !*const Object {
        return self.read();
    }

    fn eval(self: *Interpreter, obj: *const Object) *const Object {
        // const v = try printObject(obj, allocator);
        // std.debug.print("obj: {s}\n", .{v});
        switch (obj.type) {
            .int, .symbol, .nil => return obj,
            .cell => {
                const cell = obj.cell.?;
                const carName = self.eval(cell.car).name.?;
                if (stringEqual(carName, "quote")) {
                    // quote only have one cdr, so get it directly
                    const cadr = cell.cdr.cell.?.car;
                    return cadr;
                } else if (stringEqual(carName, "+")) {
                    const cadr = cell.cdr;
                    return self.evalPlus(cadr);
                } else if (stringEqual(carName, "list")) {
                    const cadr = cell.cdr;
                    return self.evalList(cadr);
                } else {
                    @panic("Unknown cell");
                }
            },
            .token => {
                @panic("token should not in ast");
            },
        }
        return "";
    }

    fn evalPlus(
        self: *Interpreter,
        obj: *const Object,
    ) *const Object {
        var sum: i32 = 0;
        var c = obj;
        while (c != nilObject) {
            const car = c.cell.?.car;
            if (car.type != .int) {
                @panic("evalPlus: car is not int");
            }
            sum += car.value.?;
            c = c.cell.?.cdr;
        }
        return self.makeNumber(sum);
    }

    fn evalList(self: *Interpreter, obj: *const Object) *const Object {
        if (obj == nilObject) {
            return self.eval(obj);
        }
        var c = obj;

        const head = self.cons(self.eval(c.cell.?.car), nilObject);
        var tail = @constCast(head);
        c = c.cell.?.cdr;
        while (c != nilObject) {
            const o = self.eval(c.cell.?.car);
            tail.cell.?.cdr = @constCast(self.cons(o, nilObject));
            tail = @constCast(tail.cell.?.cdr);
            c = c.cell.?.cdr;
        }
        return head;
    }
};

pub fn run(source: String) !String {
    const allocator = std.heap.page_allocator;
    var e = Interpreter{ .input = source, .allocator = allocator };
    const parsed = try e.parse();
    return printObject(e.eval(parsed), allocator);
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
        .{ .input = "; 2\n5 ; 3", .expected = "5" },
    };
    for (testcases) |tc| {
        const r = try run(tc.input);
        std.debug.print("Result: {s}\n", .{r});
        try testing.expect(stringEqual(tc.expected, r));
    }
}
