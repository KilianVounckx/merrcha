const std = @import("std");
const ArrayList = std.ArrayList;
const fmt = std.fmt;
const maxInt = std.math.maxInt;
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;

pub const ascii = @import("ascii.zig");
const meta = @import("meta.zig");

test {
    _ = ascii;
    _ = meta;
}

const merrcha = @This();

/// Main Parser type for this package. `T` is the type of what is parsed.
/// `Error` is an error set of all possible errors for the parser (without
/// `error.OutOfMemory`).
pub fn Parser(comptime _T: type, comptime _Error: type) type {
    return struct {
        pub const T = _T;
        pub const Error = _Error;

        parse: *const fn (Allocator, []const u8) (Allocator.Error || Error)!Result(T),

        pub const asStr = merrcha.asStr;
        pub const discard = merrcha.discard;
        pub const many = merrcha.many;
        pub const manyN = merrcha.manyN;
        pub const map = merrcha.map;
        pub const mapConst = merrcha.mapConst;
        pub const opt = merrcha.opt;
        pub const sepBy = merrcha.sepBy;
        pub const sepByN = merrcha.sepByN;
    };
}

/// Basic tuple type of a parsed value and the remaining input to be parsed.
pub fn Result(comptime T: type) type {
    return struct {
        pub const Value = T;

        value: Value,
        rest: []const u8,
    };
}

/// Get the result type of a parser.
pub fn ParserResult(comptime parser: anytype) type {
    const P = if (@TypeOf(parser) == type) parser else @TypeOf(parser);
    switch (@typeInfo(P)) {
        .pointer => |p| {
            typeCheckParser(p.child);
            return p.child.T;
        },
        else => {
            typeCheckParser(P);
            return P.T;
        },
    }
}

/// Get the error set type of a parser.
pub fn ParserError(comptime parser: anytype) type {
    const P = if (@TypeOf(parser) == type) parser else @TypeOf(parser);
    switch (@typeInfo(P)) {
        .pointer => |p| {
            typeCheckParser(p.child);
            return p.child.Error;
        },
        else => {
            typeCheckParser(P);
            return P.Error;
        },
    }
}

/// Parser that always succeeds and parses nothing.
///
/// This parser is only really useful for generic code.
pub const noop = Parser(void, error{}){
    .parse = struct {
        fn parse(_: Allocator, str: []const u8) !Result(void) {
            return .{ .value = {}, .rest = str };
        }
    }.parse,
};

test "noop" {
    const allocator = testing.failing_allocator;
    try expectResult(allocator, noop, "", .{ .value = {}, .rest = "" });
    try expectResult(allocator, noop, "abc", .{ .value = {}, .rest = "abc" });
}

/// Parser that only succeeds at the end of a string.
pub const eos = Parser(void, error{@"Expected EOS"}){
    .parse = struct {
        fn parse(_: Allocator, str: []const u8) !Result(void) {
            if (str.len != 0) {
                return error.@"Expected EOS";
            }
            return .{ .value = {}, .rest = str };
        }
    }.parse,
};

test "eos" {
    const allocator = testing.failing_allocator;
    try expectResult(allocator, eos, "", .{ .value = {}, .rest = "" });
    try expectResult(allocator, eos, "abc", error.@"Expected EOS");
}

/// Parser that always succeeds with the result being the entire string.
pub const rest = Parser([]const u8, error{}){
    .parse = struct {
        fn parse(_: Allocator, str: []const u8) !Result([]const u8) {
            return .{ .value = str, .rest = str[str.len..] };
        }
    }.parse,
};

test "rest" {
    const allocator = testing.failing_allocator;
    try expectResult(allocator, rest, "", .{ .value = "", .rest = "" });
    try expectResult(allocator, rest, "abc", .{ .value = "abc", .rest = "" });
}

/// Constructs a parser that discards the result returned from the parser
/// it wraps.
pub fn discard(
    comptime parser: anytype,
) Parser(void, ParserError(parser)) {
    typeCheckParser(parser);
    return .{
        .parse = struct {
            fn parse(allocator: Allocator, str: []const u8) !Result(void) {
                const result = try parser.parse(allocator, str);
                return .{ .value = {}, .rest = result.rest };
            }
        }.parse,
    };
}

test "discard" {
    const allocator = testing.failing_allocator;
    const parser = comptime string("abc").discard();
    try expectResult(allocator, parser, "abc", .{ .value = {}, .rest = "" });
    try expectResult(allocator, parser, "abcde", .{ .value = {}, .rest = "de" });
    try expectResult(allocator, parser, "zz", error.@"Expected 'abc'");
}

/// Error type of `string(expected)` parser.
pub fn StringError(comptime expected: []const u8) type {
    return meta.ErrorFromName("Expected '" ++ expected ++ "'");
}

fn stringError(comptime expected: []const u8) StringError(expected) {
    return meta.errorFromName("Expected '" ++ expected ++ "'");
}

/// Parser which succeeds if the string starts with `expected`.
pub fn string(
    comptime expected: []const u8,
) Parser([]const u8, StringError(expected)) {
    return .{
        .parse = struct {
            fn parse(_: Allocator, str: []const u8) !Result([]const u8) {
                if (!mem.startsWith(u8, str, expected)) {
                    return stringError(expected);
                }
                return .{
                    .value = str[0..expected.len],
                    .rest = str[expected.len..],
                };
            }
        }.parse,
    };
}

test "string" {
    const allocator = testing.failing_allocator;
    try expectResult(allocator, string("aa"), "aa", .{ .value = "aa", .rest = "" });
    try expectResult(allocator, string("aa"), "aaa", .{ .value = "aa", .rest = "a" });
    try expectResult(allocator, string("aa"), "ba", error.@"Expected 'aa'");
    try expectResult(allocator, string("aa"), "", error.@"Expected 'aa'");
}

/// Parser that repeatedly uses `parser` until `n` iterations are reached.
/// The parser's result will be an array of the results from the repeated parser.
pub fn manyN(
    comptime parser: anytype,
    comptime n: usize,
) Parser([n]ParserResult(parser), ParserError(parser)) {
    typeCheckParser(parser);
    const Array = [n]ParserResult(parser);
    return .{
        .parse = struct {
            fn parse(allocator: Allocator, str: []const u8) !Result(Array) {
                var remainder = str;
                var result: Array = undefined;
                for (&result) |*value| {
                    const res = try parser.parse(allocator, remainder);
                    remainder = res.rest;
                    value.* = res.value;
                }
                return .{ .value = result, .rest = remainder };
            }
        }.parse,
    };
}

test "manyN" {
    const allocator = testing.failing_allocator;
    const parser = comptime ascii.range('a', 'b').manyN(3);
    try expectResult(allocator, parser, "ababab", .{ .value = "aba".*, .rest = "bab" });
}

/// Parser that repeatedly uses `parser` until `n` iterations are reached,
/// separated by `separator`.
/// The parser's result will be an array of the results from the repeated parser.
pub fn sepByN(
    comptime parser: anytype,
    comptime n: usize,
    comptime separator: anytype,
) Parser(
    [n]ParserResult(parser),
    ParserError(parser) || ParserError(separator),
) {
    typeCheckParser(parser);
    typeCheckParser(separator);
    const Array = [n]ParserResult(parser);
    return .{
        .parse = struct {
            fn parse(allocator: Allocator, str: []const u8) !Result(Array) {
                var remainder = str;
                var result: Array = undefined;
                for (&result, 0..) |*value, index| {
                    if (index != 0) {
                        remainder = (try separator.parse(allocator, remainder)).rest;
                    }
                    const res = try parser.parse(allocator, remainder);
                    remainder = res.rest;
                    value.* = res.value;
                }
                return .{ .value = result, .rest = remainder };
            }
        }.parse,
    };
}

test "sepByN" {
    const allocator = testing.failing_allocator;
    const parser = comptime ascii.range('a', 'b').sepByN(3, string(","));
    try expectResult(allocator, parser, "a,b,a,b,a,b", .{ .value = "aba".*, .rest = ",b,a,b" });
}

/// Options passed to `many`.
pub const ManyOptions = struct {
    /// Minimal number of elements parsed (inclusive).
    ///
    /// If not enough elements can be parsed, the parser will return an error.
    min: usize = 0,
    /// Maximal number of elements parsed (inclusive).
    ///
    /// If more elements can be parsed, the parser will not parse these. They
    /// are still in `rest`.
    max: usize = maxInt(usize),
    /// Collect the elements in an allocated slice. If false, the parser will
    /// return the parsed string without allocation.
    collect: bool = true,
};

fn Many(comptime parser: anytype, comptime options: ManyOptions) type {
    if (!options.collect) {
        return []const u8;
    }
    return []ParserResult(parser);
}

/// Parser that repeatedly uses `parser` as long as it succeeds or until
/// `opt.max` is reached. See `ManyOptions` for options this function exposes.
pub fn many(
    comptime parser: anytype,
    comptime options: ManyOptions,
) Parser(Many(parser, options), ParserError(parser) || error{@"Too few"}) {
    typeCheckParser(parser);
    const Element = ParserResult(parser);
    const Slice = Many(parser, options);
    return .{
        .parse = struct {
            fn parse(allocator: Allocator, str: []const u8) !Result(Slice) {
                var result = if (options.collect)
                    try ArrayList(Element).initCapacity(allocator, options.min)
                else {};
                errdefer if (options.collect) result.deinit();

                var remainder = str;
                var num_parsed: usize = 0;
                while (num_parsed < options.max) : (num_parsed += 1) {
                    const res = parser.parse(
                        allocator,
                        remainder,
                    ) catch |err| switch (err) {
                        error.OutOfMemory => return error.OutOfMemory,
                        else => break,
                    };
                    remainder = res.rest;
                    if (options.collect) {
                        try result.append(res.value);
                    }
                }
                if (num_parsed < options.min)
                    return error.@"Too few";

                return .{
                    .value = if (options.collect)
                        try result.toOwnedSlice()
                    else
                        str[0 .. str.len - remainder.len],
                    .rest = remainder,
                };
            }
        }.parse,
    };
}

test "many" {
    const allocator = testing.failing_allocator;

    const parser1 = comptime string("ab").many(.{ .collect = false });
    try expectResult(allocator, parser1, "", .{ .value = "", .rest = "" });
    try expectResult(allocator, parser1, "a", .{ .value = "", .rest = "a" });
    try expectResult(allocator, parser1, "ab", .{ .value = "ab", .rest = "" });
    try expectResult(allocator, parser1, "aba", .{ .value = "ab", .rest = "a" });
    try expectResult(allocator, parser1, "abab", .{ .value = "abab", .rest = "" });
    try expectResult(allocator, parser1, "ababa", .{ .value = "abab", .rest = "a" });
    try expectResult(allocator, parser1, "ababab", .{ .value = "ababab", .rest = "" });

    const parser2 = comptime string("ab").many(.{ .collect = false, .min = 1, .max = 2 });
    try expectResult(allocator, parser2, "", error.@"Too few");
    try expectResult(allocator, parser2, "a", error.@"Too few");
    try expectResult(allocator, parser2, "ab", .{ .value = "ab", .rest = "" });
    try expectResult(allocator, parser2, "aba", .{ .value = "ab", .rest = "a" });
    try expectResult(allocator, parser2, "abab", .{ .value = "abab", .rest = "" });
    try expectResult(allocator, parser2, "ababa", .{ .value = "abab", .rest = "a" });
    try expectResult(allocator, parser2, "ababab", .{ .value = "abab", .rest = "ab" });
}

/// Parser that repeatedly uses `parser` as long as it succeeds or until
/// `opt.max` is reached. Each two elements parsed are separated by other
/// elements parsed by `separator`. See `ManyOptions` for options this
/// function exposes.
pub fn sepBy(
    comptime parser: anytype,
    comptime separator: anytype,
    comptime options: ManyOptions,
) Parser(
    Many(parser, options),
    ParserError(parser) || ParserError(separator) || error{@"Too few"},
) {
    typeCheckParser(parser);
    typeCheckParser(separator);
    const Element = ParserResult(parser);
    const Slice = Many(parser, options);
    return .{
        .parse = struct {
            fn parse(allocator: Allocator, str: []const u8) !Result(Slice) {
                var result = if (options.collect)
                    try ArrayList(Element).initCapacity(allocator, options.min)
                else {};
                errdefer if (options.collect) result.deinit();

                var remainder = str;
                var num_parsed: usize = 0;
                while (num_parsed < options.max) : (num_parsed += 1) {
                    if (num_parsed != 0) {
                        remainder = (separator.parse(
                            allocator,
                            remainder,
                        ) catch break).rest;
                    }

                    const res = parser.parse(
                        allocator,
                        remainder,
                    ) catch |err| switch (err) {
                        error.OutOfMemory => return error.OutOfMemory,
                        else => break,
                    };
                    remainder = res.rest;
                    if (options.collect) {
                        try result.append(res.value);
                    }
                }
                if (num_parsed < options.min)
                    return error.@"Too few";

                return .{
                    .value = if (options.collect)
                        try result.toOwnedSlice()
                    else
                        str[0 .. str.len - remainder.len],
                    .rest = remainder,
                };
            }
        }.parse,
    };
}

test "sebBy" {
    const allocator = testing.failing_allocator;

    const parser1 = comptime string("ab").sepBy(string(","), .{ .collect = false });
    try expectResult(allocator, parser1, "", .{ .value = "", .rest = "" });
    try expectResult(allocator, parser1, "a", .{ .value = "", .rest = "a" });
    try expectResult(allocator, parser1, "ab", .{ .value = "ab", .rest = "" });
    try expectResult(allocator, parser1, "aba", .{ .value = "ab", .rest = "a" });
    try expectResult(allocator, parser1, "ab,ab", .{ .value = "ab,ab", .rest = "" });
    try expectResult(allocator, parser1, "ab,aba", .{ .value = "ab,ab", .rest = "a" });
    try expectResult(allocator, parser1, "ab,ab,ab", .{ .value = "ab,ab,ab", .rest = "" });

    const parser2 = comptime string("ab").sepBy(string(","), .{ .collect = false, .min = 1, .max = 2 });
    try expectResult(allocator, parser2, "", error.@"Too few");
    try expectResult(allocator, parser2, "a", error.@"Too few");
    try expectResult(allocator, parser2, "ab", .{ .value = "ab", .rest = "" });
    try expectResult(allocator, parser2, "aba", .{ .value = "ab", .rest = "a" });
    try expectResult(allocator, parser2, "ab,ab", .{ .value = "ab,ab", .rest = "" });
    try expectResult(allocator, parser2, "ab,aba", .{ .value = "ab,ab", .rest = "a" });
    try expectResult(allocator, parser2, "ab,ab,ab", .{ .value = "ab,ab", .rest = ",ab" });
}

/// Parser that will call `parser` on the string but never fails to parse. The
/// parser's result will be the result of `parser` on success and `null` on
/// failure.
pub fn opt(
    comptime parser: anytype,
) Parser(?ParserResult(parser), error{}) {
    typeCheckParser(parser);
    const T = ?ParserResult(parser);
    return .{
        .parse = struct {
            fn parse(allocator: Allocator, str: []const u8) !Result(T) {
                const result = parser.parse(allocator, str) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => return .{ .value = null, .rest = str },
                };
                return .{ .value = result.value, .rest = result.rest };
            }
        }.parse,
    };
}

test "opt" {
    const allocator = testing.failing_allocator;
    const parser = comptime ascii.range('a', 'z').opt();
    try expectResult(allocator, parser, "a", .{ .value = 'a', .rest = "" });
    try expectResult(allocator, parser, "aa", .{ .value = 'a', .rest = "a" });
    try expectResult(allocator, parser, "1", .{ .value = null, .rest = "1" });
}

fn parsersTypes(comptime parsers: anytype) []const type {
    var types: []const type = &[_]type{};
    for (parsers) |parser| {
        typeCheckParser(parser);
        const T = ParserResult(parser);
        if (T != void) {
            types = types ++ [_]type{T};
        }
    }
    return types;
}

fn Tuple(comptime n: usize, comptime types: [n]type) type {
    return std.meta.Tuple(&types);
}

fn Combine(comptime parsers: anytype) type {
    const types = parsersTypes(parsers);
    if (types.len == 0)
        return void;
    if (types.len == 1)
        return types[0];
    return Tuple(types.len, types[0..types.len].*);
}

fn CombineError(comptime parsers: anytype) type {
    var result = error{};
    for (parsers) |parser| {
        typeCheckParser(parser);
        const Err = ParserError(parser);
        result = result || Err;
    }
    return result;
}

/// Takes a tuple of `Parser(any, anyerr)` and constructs a parser that only
/// succeeds if all parsers succeed to parse. The parsers will be called in
/// order and parser `N` will use the `rest` from parser `N-1`. The parsers
/// result will be a `Tuple` of all parser not of type `Parser(void, anyerr)`.
/// If only one parser is not of type `Parser(void, anyerr)` then this parser's
/// result is returned instead of a tuple.
pub fn combine(
    comptime parsers: anytype,
) Parser(Combine(parsers), CombineError(parsers)) {
    const types = parsersTypes(parsers);
    const T = Combine(parsers);
    return .{
        .parse = struct {
            fn parse(allocator: Allocator, str: []const u8) !Result(Combine(parsers)) {
                var remainder = str;
                var result: T = undefined;

                comptime var index = 0;
                inline for (parsers) |parser| {
                    typeCheckParser(parser);
                    const res = try parser.parse(allocator, remainder);
                    remainder = res.rest;
                    if (@TypeOf(res.value) == void)
                        continue;
                    if (types.len == 1) {
                        result = res.value;
                    } else {
                        result[index] = res.value;
                    }
                    index += 1;
                }
                return .{ .value = result, .rest = remainder };
            }
        }.parse,
    };
}

test "combine" {
    const allocator = testing.failing_allocator;

    const parser1 = combine(.{
        opt(ascii.range('a', 'b')),
        opt(ascii.range('d', 'e')),
    });
    try expectResult(
        allocator,
        parser1,
        "ad",
        .{ .value = .{ .@"0" = 'a', .@"1" = 'd' }, .rest = "" },
    );
    try expectResult(
        allocator,
        parser1,
        "aa",
        .{ .value = .{ .@"0" = 'a', .@"1" = null }, .rest = "a" },
    );
    try expectResult(
        allocator,
        parser1,
        "da",
        .{ .value = .{ .@"0" = null, .@"1" = 'd' }, .rest = "a" },
    );
    try expectResult(
        allocator,
        parser1,
        "qa",
        .{ .value = .{ .@"0" = null, .@"1" = null }, .rest = "qa" },
    );

    const parser2 = combine(.{
        opt(ascii.range('a', 'b')),
        ascii.char('d'),
    });
    try expectResult(
        allocator,
        parser2,
        "ad",
        .{ .value = .{ .@"0" = 'a', .@"1" = 'd' }, .rest = "" },
    );
    try expectResult(
        allocator,
        parser2,
        "ada",
        .{ .value = .{ .@"0" = 'a', .@"1" = 'd' }, .rest = "a" },
    );
    try expectResult(
        allocator,
        parser2,
        "da",
        .{ .value = .{ .@"0" = null, .@"1" = 'd' }, .rest = "a" },
    );
    try expectResult(allocator, parser2, "qa", error.@"Expected 'd'");
}

fn OneOf(comptime parsers: anytype) type {
    var maybeT: ?type = null;
    inline for (parsers) |parser| {
        typeCheckParser(parser);
        const TP = ParserResult(parser);
        if (maybeT) |T| {
            if (TP != T) {
                @compileError(
                    "All parsers in 'oneOf' must have the same result type",
                );
            }
        } else {
            maybeT = TP;
        }
    }
    if (maybeT) |T| {
        return T;
    }
    @compileError("'oneOf' parsers list must not be empty");
}

fn OneOfError(comptime parsers: anytype) type {
    if (parsers.len == 0)
        @compileError("'oneOf' parsers list must not be empty");
    const parser = parsers[parsers.len - 1];
    typeCheckParser(parser);
    return ParserError(parser);
}

/// Takes a tuple of `Parser(T, Error)` and constructs a parser that only
/// succeeds if one of the parsers succeed to parse. The parsers will be called
/// in order all with `str` as input. The parser will return with the result of
/// the first parser that succeeded. The parsers result will be `Result(T)`
pub fn oneOf(
    comptime parsers: anytype,
) Parser(OneOf(parsers), OneOfError(parsers)) {
    return .{
        .parse = struct {
            fn parse(
                allocator: Allocator,
                str: []const u8,
            ) !Result(OneOf(parsers)) {
                inline for (parsers, 0..) |parser, index| {
                    typeCheckParser(parser);
                    if (parser.parse(allocator, str)) |result| {
                        return result;
                    } else |err| switch (err) {
                        error.OutOfMemory => return error.OutOfMemory,
                        else => |e| if (index == parsers.len - 1) {
                            return e;
                        },
                    }
                }
            }
        }.parse,
    };
}

test "oneOf" {
    const allocator = testing.failing_allocator;
    const parser = oneOf(.{ string("ab"), string("cd") });
    try expectResult(allocator, parser, "abzz", .{ .value = "ab", .rest = "zz" });
    try expectResult(allocator, parser, "cdzz", .{ .value = "cd", .rest = "zz" });
    try expectResult(allocator, parser, "zz", error.@"Expected 'cd'");
    try expectResult(allocator, parser, "", error.@"Expected 'cd'");
}

/// Takes any parser and converts it to a parser where the result is a string
/// that contains all characters parsed by `parser`.
fn asStr(comptime parser: anytype) Parser([]const u8, ParserError(parser)) {
    typeCheckParser(parser);
    return .{
        .parse = struct {
            fn parse(allocator: Allocator, str: []const u8) !Result([]const u8) {
                const result = try parser.parse(allocator, str);
                return .{ .value = str[0 .. str.len - result.rest.len], .rest = result.rest };
            }
        }.parse,
    };
}

test "asStr" {
    const allocator = testing.failing_allocator;
    const parser1 = comptime combine(.{ string("a"), string("b") }).asStr();
    try expectResult(allocator, parser1, "abc", .{ .value = "ab", .rest = "c" });
    try expectResult(allocator, parser1, "zzc", error.@"Expected 'a'");
}

const mapFuncInfo = struct {
    input: ?type,
    output: type,
    err: ?type,
    allocator: bool,
};

fn MapFuncInfo(comptime func: anytype) mapFuncInfo {
    const Func = if (@TypeOf(func) == type) func else @TypeOf(func);
    const err = "expected 'map' function, found '" ++ @typeName(Func) ++ "'";

    var info: mapFuncInfo = undefined;

    const F = switch (@typeInfo(Func)) {
        .pointer => |ptr| ptr.child,
        .@"fn" => Func,
        else => @compileError(err),
    };

    const function_info = switch (@typeInfo(F)) {
        .@"fn" => |funct| funct,
        else => @compileError(err),
    };

    if (function_info.params.len == 1) {
        info.allocator = false;
        info.input = function_info.params[0].type;
    } else if (function_info.params.len == 2) {
        if ((function_info.params[1].type orelse @compileError(err)) != Allocator)
            @compileError(err);
        info.allocator = true;
        info.input = function_info.params[1].type orelse @compileError(err);
    } else {
        @compileError(err);
    }

    switch (@typeInfo(function_info.return_type orelse @compileError(err))) {
        .error_union => |eu| {
            info.output = eu.payload;
            info.err = eu.error_set;
        },
        else => {
            info.output = function_info.return_type.?;
            info.err = null;
        },
    }

    return info;
}

/// Parser that has its result converted with the `func` function.
///
/// The `func` function's signature can be one of
///   - `*const fn (ParserResult(parser)) T`
///   - `*const fn (ParserResult(parser)) !T`
///   - `*const fn (Allocator, ParserResult(parser)) T`
///   - `*const fn (Allocator, ParserResult(parser)) !T`
///
/// If `func` returns an error union, the resulting parsers `Error` type will
/// be extended with `func`'s error set. Otherwise, the `Error` type will be
/// the same as `parsers`'s.
pub fn map(
    comptime parser: anytype,
    comptime func: anytype,
) Parser(
    MapFuncInfo(func).output,
    ParserError(parser) || (MapFuncInfo(func).err orelse error{}),
) {
    typeCheckParser(parser);
    const info = MapFuncInfo(func);
    return .{ .parse = struct {
        fn parse(allocator: Allocator, str: []const u8) !Result(info.output) {
            const result = try parser.parse(allocator, str);
            const output = if (comptime info.allocator)
                if (comptime info.err == null)
                    func(allocator, result.value)
                else
                    try func(allocator, result.value)
            else if (comptime info.err == null)
                func(result.value)
            else
                try func(result.value);
            return .{ .value = output, .rest = result.rest };
        }
    }.parse };
}

/// Constructs a function for `map` that takes a string and parses it to an int
/// of type `T`.
pub fn toInt(
    comptime T: type,
    comptime base: u8,
) *const fn ([]const u8) fmt.ParseIntError!T {
    if (base == 0)
        @compileError("'intToken' base value cannot be 0.");
    return struct {
        fn func(str: []const u8) !T {
            return fmt.parseInt(T, str, base);
        }
    }.func;
}

/// Constructs a function for `map` that takes a string and parses it to an
/// float of type `T`.
pub fn toFloat(
    comptime T: type,
) *const fn ([]const u8) fmt.ParseFloatError!T {
    return struct {
        fn func(str: []const u8) !T {
            return fmt.parseFloat(T, str);
        }
    }.func;
}

fn EnumError(comptime T: type) type {
    return meta.ErrorFromName(
        fmt.comptimePrint("Expected enum '{s}'", .{@typeName(T)}),
    );
}

fn enumError(comptime T: type) EnumError(T) {
    return meta.errorFromName(
        fmt.comptimePrint("Expected enum '{s}'", .{@typeName(T)}),
    );
}

/// Constructs a function for `map` that takes a string and parses it to an
/// enum of type `T` with `std.meta.stringToEnum`.
pub fn toEnum(
    comptime T: type,
) *const fn ([]const u8) EnumError(T)!T {
    return struct {
        fn func(str: []const u8) !T {
            return std.meta.stringToEnum(T, str) orelse enumError(T);
        }
    }.func;
}

/// Constructs a function for `map` that takes a tuple, array or single value
/// and converts it into the struct `T`. Fields will be assigned in order, so
/// `tuple[i]` will be assigned to the ith field of `T`. This function will
/// give a compile error if `T` and the tuple does not have the same number of
/// fields, or if the items of the tuple cannot be coerced into the fields of
/// the struct.
pub fn toStruct(comptime T: type) *const fn (anytype) T {
    const struct_info = switch (@typeInfo(T)) {
        .@"struct" => |info| info,
        else => @compileError("`toStruct` type must be a struct"),
    };

    return struct {
        fn func(value: anytype) T {
            const struct_fields = struct_info.fields;
            const copy_many = switch (@typeInfo(@TypeOf(value))) {
                .@"struct" => |info| info.is_tuple and info.fields.len == struct_fields.len,
                .array => |info| info.len == struct_fields.len,
                else => false,
            };
            var result: T = undefined;
            if (copy_many) {
                inline for (struct_fields, 0..) |field, index| {
                    @field(result, field.name) = value[index];
                }
            } else {
                if (struct_fields.len != 1) {
                    @compileError("Cannot map " ++
                        @typeName(@TypeOf(value)) ++
                        " to " ++
                        @typeName(T));
                }
                @field(result, struct_fields[0].name) = value;
            }
            return result;
        }
    }.func;
}

/// Constructs a function for `map` that initializes a union of type `T` with
/// the value passed to it, using `@unionInit` with the tag `tag`.
pub fn toUnion(
    comptime T: type,
    comptime tag: std.meta.Tag(T),
) *const fn (anytype) T {
    return struct {
        fn func(value: anytype) T {
            return @unionInit(T, @tagName(tag), value);
        }
    }.func;
}

test "map" {
    const allocator = testing.failing_allocator;

    const parser1 = comptime string("123").map(toInt(u8, 10));
    try expectResult(allocator, parser1, "123", .{ .value = 123, .rest = "" });
    try expectResult(allocator, parser1, "12", error.@"Expected '123'");

    const parser2 = comptime string("1.23").map(toFloat(f32));
    try expectResult(allocator, parser2, "1.23", .{ .value = 1.23, .rest = "" });
    try expectResult(allocator, parser2, "1.23a", .{ .value = 1.23, .rest = "a" });
    try expectResult(allocator, parser2, "1.2", error.@"Expected '1.23'");

    const E = enum { a, b };
    const parser3 = comptime rest.map(toEnum(E));
    try expectResult(allocator, parser3, "a", .{ .value = .a, .rest = "" });
    try expectResult(allocator, parser3, "b", .{ .value = .b, .rest = "" });
    try expectResult(allocator, parser3, "z", error.@"Expected enum 'main.test.map.E'");

    const int_parser = comptime many(
        ascii.range('0', '9'),
        .{ .collect = false, .min = 1 },
    ).map(toInt(usize, 10));

    const Point = struct {
        x: usize,
        y: usize,
    };
    const parser4 = comptime combine(.{
        int_parser,
        discard(ascii.char(' ')),
        int_parser,
    }).map(toStruct(Point));
    try expectResult(
        allocator,
        parser4,
        "10 10",
        .{ .value = .{ .x = 10, .y = 10 }, .rest = "" },
    );
    try expectResult(
        allocator,
        parser4,
        "20 20aa",
        .{ .value = .{ .x = 20, .y = 20 }, .rest = "aa" },
    );
    try expectResult(
        allocator,
        parser4,
        "12",
        error.@"Expected ' '",
    );

    const parser5 = comptime manyN(combine(.{
        int_parser,
        discard(ascii.char(' ')),
    }), 2).map(toStruct(Point));
    try expectResult(
        allocator,
        parser5,
        "10 10 ",
        .{ .value = .{ .x = 10, .y = 10 }, .rest = "" },
    );
    try expectResult(
        allocator,
        parser5,
        "20 20 aa",
        .{ .value = .{ .x = 20, .y = 20 }, .rest = "aa" },
    );
    try expectResult(
        allocator,
        parser5,
        "12 13",
        error.@"Expected ' '",
    );

    const Person = struct {
        name: []const u8,
        age: usize,
    };
    const Message = union(enum) {
        point: Point,
        person: Person,
    };
    const parser6 = comptime combine(.{
        int_parser,
        discard(ascii.char(' ')),
        int_parser,
    }).map(toStruct(Point)).map(toUnion(Message, .point));
    try expectResult(
        allocator,
        parser6,
        "10 10",
        .{ .value = .{ .point = .{ .x = 10, .y = 10 } }, .rest = "" },
    );

    const parser7 = comptime combine(.{
        many(ascii.alphabetic, .{ .min = 1, .collect = false }),
        discard(ascii.char(' ')),
        int_parser,
    }).map(toStruct(Person)).map(toUnion(Message, .person));
    const person_result = try parser7.parse(allocator, "Bob 24");
    try testing.expectEqualStrings("Bob", person_result.value.person.name);
    try testing.expectEqual(24, person_result.value.person.age);

    const Wrapper = struct {
        value: []const u8,
    };
    const parser8 = comptime string("foo").map(toStruct(Wrapper));
    const wrapper_result = try parser8.parse(allocator, "foo");
    try testing.expectEqualStrings("foo", wrapper_result.value.value);
}

/// Parser that consumes the input with `parser` and places `value` into it's
/// result. Discarding `parser`'s result value, but keeping it's rest. This can
/// be used to map parsers to static values, for example `\n` to the newline
/// character.
pub fn mapConst(
    comptime parser: anytype,
    comptime value: anytype,
) Parser(@TypeOf(value), ParserError(parser)) {
    typeCheckParser(parser);
    return .{ .parse = struct {
        fn parse(allocator: Allocator, str: []const u8) !Result(@TypeOf(value)) {
            const result = try parser.parse(allocator, str);
            return .{ .value = value, .rest = result.rest };
        }
    }.parse };
}

test "mapConst" {
    const allocator = testing.failing_allocator;
    const parser = mapConst(string("123"), @as(u8, 3));
    try expectResult(allocator, parser, "123", .{ .value = 3, .rest = "" });
    try expectResult(allocator, parser, "123a", .{ .value = 3, .rest = "a" });
    try expectResult(allocator, parser, "12", error.@"Expected '123'");
}

/// Options used in `intToken` and `int`.
pub const IntOptions = struct {
    /// Parse '+' or '-' in the int.
    parse_sign: bool = true,
    /// Base of integer to parse.
    base: u8 = 10,
    /// Maximum numbers of digits to parse in the number.
    max_digits: usize = maxInt(usize),
};

fn IntTokenError(comptime options: IntOptions) type {
    if (options.base == 0)
        @compileError("'intToken' base value cannot be 0.");
    const sign_parser = comptime if (options.parse_sign)
        oneOf(.{ ascii.char('-'), ascii.char('+') })
    else
        noop;
    const parser = comptime combine(.{
        sign_parser,
        ascii.digit(options.base).many(.{
            .collect = false,
            .min = 1,
            .max = options.max_digits,
        }),
    }).asStr();
    return ParserError(parser);
}

/// Parser that succeeds if it parser an integer of `options.base`. This parser
/// will stop parsing digits after `options.max_digits` after the leading zeros
/// have been reached. The result of this parser will be the string containing
/// the match.
pub fn intToken(
    comptime options: IntOptions,
) Parser([]const u8, IntTokenError(options)) {
    if (options.base == 0)
        @compileError("'intToken' base value cannot be 0.");
    const sign_parser = comptime if (options.parse_sign)
        oneOf(.{ ascii.char('-'), ascii.char('+') })
    else
        noop;
    return comptime combine(.{
        sign_parser,
        ascii.digit(options.base).many(.{
            .collect = false,
            .min = 1,
            .max = options.max_digits,
        }),
    }).asStr();
}

fn IntError(comptime options: IntOptions) type {
    return meta.ErrorFromName(fmt.comptimePrint(
        "Expected digit of base {d}",
        .{options.base},
    )) ||
        error{@"Expected unsigned number"};
}

/// Same as `intToken` but also converts the parsed string to an integer of
/// type `T`.
pub fn int(
    comptime T: type,
    comptime options: IntOptions,
) Parser(T, IntError(options)) {
    if (options.base == 0)
        @compileError("'intToken' base value cannot be 0.");

    return .{ .parse = struct {
        fn parseAfterSign(
            str: []const u8,
            add_sub: *const fn (T, T) error{Overflow}!T,
        ) IntError(options)!Result(T) {
            const expected_err =
                meta.errorFromName(fmt.comptimePrint(
                "Expected digit of base {d}",
                .{options.base},
            ));
            const expected_pos_err = error.@"Expected unsigned number";
            if (str.len == 0)
                return expected_err;
            const max_digits = @min(str.len, options.max_digits);
            const first = fmt.charToDigit(str[0], options.base) catch |err| switch (err) {
                error.InvalidCharacter => return expected_err,
            };
            const first_casted: T = std.math.cast(T, first) orelse return expected_err;

            var result = add_sub(0, first_casted) catch return expected_pos_err;
            const end = for (str[1..max_digits], 0..) |char, index| {
                const digit = std.fmt.charToDigit(char, options.base) catch break index;
                const casted_base = std.math.cast(T, options.base) orelse break index;
                const casted_digit = std.math.cast(T, digit) orelse break index;
                const next = std.math.mul(T, result, casted_base) catch break index;
                result = add_sub(next, casted_digit) catch break index;
            } else max_digits - 1;
            return .{ .value = result, .rest = str[end + 1 ..] };
        }

        fn parse(_: Allocator, str: []const u8) !Result(T) {
            if (options.parse_sign and str.len > 0) {
                switch (str[0]) {
                    '+' => return parseAfterSign(str[1..], add),
                    '-' => return parseAfterSign(str[1..], sub),
                    else => {},
                }
            }
            return parseAfterSign(str, add);
        }

        fn add(a: T, b: T) !T {
            return std.math.add(T, a, b);
        }

        fn sub(a: T, b: T) !T {
            return std.math.sub(T, a, b);
        }
    }.parse };
}

test "int" {
    const allocator = testing.failing_allocator;
    const parser1 = int(u8, .{});
    try expectResult(allocator, parser1, "0", .{ .value = 0, .rest = "" });
    try expectResult(allocator, parser1, "1", .{ .value = 1, .rest = "" });
    try expectResult(allocator, parser1, "1a", .{ .value = 1, .rest = "a" });
    try expectResult(allocator, parser1, "255", .{ .value = 255, .rest = "" });
    try expectResult(allocator, parser1, "2555", .{ .value = 255, .rest = "5" });
    try expectResult(allocator, parser1, "256", .{ .value = 25, .rest = "6" });
    try expectResult(allocator, parser1, "+255", .{ .value = 255, .rest = "" });
    try expectResult(allocator, parser1, "-255", error.@"Expected unsigned number");

    const parser2 = int(u8, .{ .base = 16 });
    try expectResult(allocator, parser2, "0", .{ .value = 0x00, .rest = "" });
    try expectResult(allocator, parser2, "1", .{ .value = 0x01, .rest = "" });
    try expectResult(allocator, parser2, "1a", .{ .value = 0x1a, .rest = "" });
    try expectResult(allocator, parser2, "1g", .{ .value = 0x01, .rest = "g" });
    try expectResult(allocator, parser2, "ff", .{ .value = 0xff, .rest = "" });
    try expectResult(allocator, parser2, "FF", .{ .value = 0xff, .rest = "" });
    try expectResult(allocator, parser2, "00FF", .{ .value = 0xff, .rest = "" });
    try expectResult(allocator, parser2, "100", .{ .value = 0x10, .rest = "0" });
    try expectResult(allocator, parser2, "fg", .{ .value = 0xf, .rest = "g" });
    try expectResult(allocator, parser2, "+ff", .{ .value = 0xff, .rest = "" });
    try expectResult(allocator, parser2, "-ff", error.@"Expected unsigned number");

    const parser3 = int(u8, .{ .base = 16, .max_digits = 2 });
    try expectResult(allocator, parser3, "FF", .{ .value = 0xff, .rest = "" });
    try expectResult(allocator, parser3, "00FF", .{ .value = 0x00, .rest = "FF" });

    const parser4 = int(isize, .{});
    try expectResult(allocator, parser4, "+255", .{ .value = 255, .rest = "" });
    try expectResult(allocator, parser4, "-255", .{ .value = -255, .rest = "" });

    const parser5 = int(isize, .{ .parse_sign = false });
    try expectResult(allocator, parser5, "255", .{ .value = 255, .rest = "" });
    try expectResult(allocator, parser5, "+255", error.@"Expected digit of base 10");
    try expectResult(allocator, parser5, "-255", error.@"Expected digit of base 10");
}

/// Parser that succeeds if it parses any tag from `Enum` as a string. The
/// longest match is always chosen, so for `enum{a,aa}` the "aa" string will
/// succeed parsing and have the result of `.aa` and not `.a`.
pub fn enumeration(comptime T: type) Parser(T, EnumError(T)) {
    return .{ .parse = struct {
        fn parse(allocator: Allocator, str: []const u8) !Result(T) {
            var result: EnumError(T)!Result(T) = enumError(T);
            inline for (@typeInfo(T).@"enum".fields) |field| next: {
                const string_parser = comptime string(field.name);
                const match = string_parser.parse(allocator, str) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => break :next,
                };
                const old = result catch Result(T){ .value = undefined, .rest = str };
                if (match.rest.len < old.rest.len) {
                    result = .{ .value = @field(T, field.name), .rest = match.rest };
                }
            }
            return result;
        }
    }.parse };
}

test "enumeration" {
    const allocator = testing.failing_allocator;
    const E = enum { a, b, aa };
    const parser = enumeration(E);
    try expectResult(allocator, parser, "a", .{ .value = .a, .rest = "" });
    try expectResult(allocator, parser, "aa", .{ .value = .aa, .rest = "" });
    try expectResult(allocator, parser, "b", .{ .value = .b, .rest = "" });
    try expectResult(allocator, parser, "ab", .{ .value = .a, .rest = "b" });
    try expectResult(allocator, parser, "bb", .{ .value = .b, .rest = "b" });
    try expectResult(allocator, parser, "256", enumError(E));
}

fn RefType(comptime func: anytype) type {
    const Func = if (@TypeOf(func) == type) func else @TypeOf(func);
    const err = "expected 'ref' function, found '" ++ @typeName(Func) ++ "'";

    const F = switch (@typeInfo(Func)) {
        .pointer => |ptr| blk: {
            if (@typeInfo(ptr.child) != .@"fn")
                @compileError(err);
            break :blk ptr.child;
        },
        .@"fn" => Func,
        else => @compileError(err),
    };

    const R = @typeInfo(F).@"fn".return_type orelse @compileError(err);
    typeCheckParser(R);
    return R;
}

/// Parser that calls a function to obtain its underlying parser. This function
/// introduces the indirection required for recursive grammars.
/// ```
/// const digit_10 = ascii.digit(10).discard();
/// const digits = oneOf(.{ combine(.{ digit_10, ref(digitsRef) }), digit_10 });
/// fn digitsRef() Parser(void) {
///     return digits;
/// };
/// ```
pub fn ref(
    comptime func: anytype,
) RefType(func) {
    return .{ .parse = struct {
        fn parse(
            allocator: std.mem.Allocator,
            str: []const u8,
        ) !Result(ParserResult(RefType(func))) {
            return func().parse(allocator, str);
        }
    }.parse };
}

test "ref" {
    const allocator = testing.failing_allocator;
    const parser = struct {
        const digit = ascii.digit(10).discard();
        const digits = oneOf(.{
            combine(.{ digit, ref(digitsRef) }),
            digit,
        });
        fn digitsRef() Parser(void, ParserError(digit)) {
            return digits;
        }
    }.digits;
    try expectResult(allocator, parser, "0", .{ .value = {}, .rest = "" });
    try expectResult(allocator, parser, "0123", .{ .value = {}, .rest = "" });
    try expectResult(allocator, parser, "0123a", .{ .value = {}, .rest = "a" });
    try expectResult(allocator, parser, "a", error.@"Expected '0'...'9'");
}

fn typeCheckParser(comptime parser: anytype) void {
    const P = if (@TypeOf(parser) == type) parser else @TypeOf(parser);
    const err =
        "expected 'parser.Parser(T, Error)', found '" ++ @typeName(P) ++ "'";
    const PInner = switch (@typeInfo(P)) {
        .pointer => |ptr| ptr.child,
        else => P,
    };

    if (@typeInfo(PInner) != .@"struct") @compileError(err);
    if (!@hasDecl(PInner, "T")) @compileError(err);
    if (@TypeOf(PInner.T) != type) @compileError(err);
    if (!@hasDecl(PInner, "Error")) @compileError(err);
    if (@TypeOf(PInner.Error) != type) @compileError(err);
    if (@typeInfo(PInner.Error) != .error_set) @compileError(err);
    if (PInner != Parser(PInner.T, PInner.Error)) @compileError(err);
}

/// Helper function to test parsers.
pub fn expectResult(
    allocator: Allocator,
    parser: anytype,
    input: []const u8,
    expected_error_union: (Allocator.Error || @TypeOf(parser).Error)!Result(@TypeOf(parser).T),
) !void {
    const P = @TypeOf(parser);
    typeCheckParser(P);

    const actual_error_union = parser.parse(allocator, input);

    const expected = expected_error_union catch |err| {
        try testing.expectError(err, actual_error_union);
        return;
    };
    const actual = try actual_error_union;

    try testing.expectEqualStrings(expected.rest, actual.rest);
    switch (P.T) {
        []const u8 => try testing.expectEqualStrings(expected.value, actual.value),
        else => switch (@typeInfo(P.T)) {
            .pointer => |ptr| try testing.expectEqualSlices(ptr.child, expected.value, actual.value),
            else => try testing.expectEqual(expected.value, actual.value),
        },
    }
}
