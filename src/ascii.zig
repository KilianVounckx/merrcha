const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const merrcha = @import("main.zig");
const Parser = merrcha.Parser;
const ParserResult = merrcha.ParserResult;

const meta = @import("meta.zig");

/// Constructs a parser that parses a single ascii bytes based on a
/// `predicate`. If the `predicate` returns true, the parser will return the
/// byte parsed and the rest of the string. Otherwise the parser will fail.
/// `err` is the error the parser returns if the parser fails.
pub fn wrap(
    comptime predicate: *const fn (u8) bool,
    comptime err: anytype,
) Parser(u8, @TypeOf(err)) {
    if (@typeInfo(@TypeOf(err)) != .error_set)
        @compileError("wrap 'err' type must be error_set");
    return .{ .parse = struct {
        fn parse(_: Allocator, str: []const u8) !merrcha.Result(u8) {
            if (str.len == 0 or !predicate(str[0]))
                return err;
            return .{ .value = str[0], .rest = str[1..] };
        }
    }.parse };
}

/// Parser that only succeeds if the string starts with `expected`.
pub fn char(
    comptime expected: u8,
) Parser(
    u8,
    meta.ErrorFromName(std.fmt.comptimePrint("Expected '{c}'", .{expected})),
) {
    const err = std.fmt.comptimePrint("Expected '{c}'", .{expected});
    return wrap(charPredicate(expected), meta.errorFromName(err));
}

fn charPredicate(comptime expected: u8) *const fn (u8) bool {
    return struct {
        fn predicate(c: u8) bool {
            return c == expected;
        }
    }.predicate;
}

test "char" {
    const allocator = testing.failing_allocator;
    inline for (0..256) |i| {
        const c: u8 = @intCast(i);
        try testWithPredicate(
            allocator,
            char(c),
            rangePredicate(c, c),
            meta.errorFromName(std.fmt.comptimePrint("Expected '{c}'", .{c})),
        );
    }
}

/// Parser that only succeeds if the string starts with a codepoint that is in
/// between `start` and `end` inclusively. The parser's result will be the
/// byte parsed.
pub fn range(
    comptime start: u8,
    comptime end: u8,
) Parser(
    u8,
    meta.ErrorFromName(std.fmt.comptimePrint("Expected '{c}'...'{c}'", .{ start, end })),
) {
    const err = std.fmt.comptimePrint("Expected '{c}'...'{c}'", .{ start, end });
    return wrap(rangePredicate(start, end), meta.errorFromName(err));
}

fn rangePredicate(comptime start: u8, comptime end: u8) *const fn (u8) bool {
    return struct {
        fn predicate(c: u8) bool {
            return switch (c) {
                start...end => true,
                else => false,
            };
        }
    }.predicate;
}

fn DigitError(comptime base: u8) type {
    if (base == 0)
        @compileError("'digit' base value cannot be 0.");
    if (base > 36)
        @compileError("'digit' base value cannot be more than 36.");
    if (base <= 10) {
        const err = std.fmt.comptimePrint("Expected '0'...'{c}'", .{'0' + base - 1});
        return meta.ErrorFromName(err);
    }
    const err = std.fmt.comptimePrint("Expected 'A'...'{c}'", .{'A' + base - 11});
    return meta.ErrorFromName(err);
}

/// Parser that succeeds if the string starts with a character that is a digit
/// in `base`. The parser's result will be the character parsed.
pub fn digit(comptime base: u8) Parser(u8, DigitError(base)) {
    if (base == 0)
        @compileError("'digit' base value cannot be 0.");
    if (base > 36)
        @compileError("'digit' base value cannot be more than 36.");
    if (base <= 10)
        return range('0', '0' + base - 1);
    return merrcha.oneOf(.{
        range('0', '9'),
        range('a', 'a' + base - 11),
        range('A', 'A' + base - 11),
    });
}

test "digit" {
    const allocator = testing.failing_allocator;

    try testWithPredicate(allocator, digit(2), struct {
        fn predicate(c: u8) bool {
            return switch (c) {
                '0'...'1' => true,
                else => false,
            };
        }
    }.predicate, error.@"Expected '0'...'1'");

    try testWithPredicate(allocator, digit(10), struct {
        fn predicate(c: u8) bool {
            return switch (c) {
                '0'...'9' => true,
                else => false,
            };
        }
    }.predicate, error.@"Expected '0'...'9'");

    try testWithPredicate(allocator, digit(16), struct {
        fn predicate(c: u8) bool {
            return switch (c) {
                '0'...'9', 'a'...'f', 'A'...'F' => true,
                else => false,
            };
        }
    }.predicate, error.@"Expected 'A'...'F'");
}

/// Parser that accepts a single ascii alphabetic character.
pub const alphabetic = wrap(std.ascii.isAlphabetic, error.@"Expected ascii alphabetic");
/// Parser that accepts a single ascii alphanumeric character.
pub const alphanumeric = wrap(std.ascii.isAlphanumeric, error.@"Expected ascii alphanumeric");
/// Parser that accepts a single ascii character.
pub const ascii = wrap(std.ascii.isAscii, error.@"Expected ascii");
/// Parser that accepts a single ascii printable (non-control) character.
pub const control = wrap(std.ascii.isControl, error.@"Expected ascii control");
/// Parser that accepts a single ascii control character.
pub const print = wrap(std.ascii.isPrint, error.@"Expected ascii print");
/// Parser that accepts a single ascii lowercase character.
pub const lower = wrap(std.ascii.isLower, error.@"Expected ascii lowercase");
/// Parser that accepts a single ascii uppercase character.
pub const upper = wrap(std.ascii.isUpper, error.@"Expected ascii uppercase");
/// Parser that accepts a single ascii whitespace character.
pub const whitespace = wrap(std.ascii.isWhitespace, error.@"Expected ascii whitespacecase");

test "predicate" {
    const allocator = testing.failing_allocator;
    try testWithPredicate(allocator, alphabetic, std.ascii.isAlphabetic, error.@"Expected ascii alphabetic");
    try testWithPredicate(allocator, alphanumeric, std.ascii.isAlphanumeric, error.@"Expected ascii alphanumeric");
    try testWithPredicate(allocator, ascii, std.ascii.isAscii, error.@"Expected ascii");
    try testWithPredicate(allocator, control, std.ascii.isControl, error.@"Expected ascii control");
    try testWithPredicate(allocator, print, std.ascii.isPrint, error.@"Expected ascii print");
    try testWithPredicate(allocator, lower, std.ascii.isLower, error.@"Expected ascii lowercase");
    try testWithPredicate(allocator, upper, std.ascii.isUpper, error.@"Expected ascii uppercase");
    try testWithPredicate(allocator, whitespace, std.ascii.isWhitespace, error.@"Expected ascii whitespacecase");
}

fn testWithPredicate(
    allocator: Allocator,
    p: anytype,
    predicate: *const fn (u8) bool,
    err: anytype,
) !void {
    for (0..256) |i| {
        const c: u8 = @intCast(i);
        if (predicate(c)) switch (ParserResult(@TypeOf(p))) {
            u8 => try merrcha.expectResult(allocator, p, &.{c}, .{ .value = c, .rest = "" }),
            void => try merrcha.expectResult(allocator, p, &.{c}, .{ .value = {}, .rest = "" }),
            else => |t| @compileError("Invalid testWithPredicate type: " ++ @typeName(t)),
        } else switch (ParserResult(@TypeOf(p))) {
            u8 => try merrcha.expectResult(allocator, p, &.{c}, err),
            void => try merrcha.expectResult(allocator, p, &.{c}, err),
            else => |t| @compileError("Invalid testWithPredicate type: " ++ @typeName(t)),
        }
    }
}
