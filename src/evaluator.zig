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
const Interpreter = @import("interpreter.zig").Interpreter;

pub fn run(source: String) !String {
    const allocator = std.heap.page_allocator;
    try globals.init(allocator);
    defer globals.deinit(allocator);

    var e = try Interpreter.init(source, allocator);
    defer e.deinit();
    return e.run();
}

const testing = std.testing;

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
        try testing.expect(utils.stringEqual(tc.expected, r));
    }
}
