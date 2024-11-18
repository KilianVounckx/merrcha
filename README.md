# Merrcha

A parser combinator package for the Zig programming language.

A lot is taken from [mecha](https://github.com/Hejsil/mecha). The main
difference is that merrcha adds an error type to the parsers. This means that
there is more error information than just `error.ParserFailed`. You mostly don't
have to worry about these errors when writing parsers, as they get accumulated
automatically in combinators like `combine`. Only when using `ref`, you have
to be more explicit and write down some of the errors. Beginning with writing
the empty error set (`error{}`), and then looking at compile errors to fix it
is a relatively simple way to know the exact errors needed.

A lot of code can be ported from mecha almost directly. There are two main
differences in the api (apart from the obvious errors). One is that this
package's `Result` type does not contain an index and value union. Instead it
contains a value type and a string slice containing the part of the string
after the parsed part. This was mainly done because it was simpler to write.
The second difference is that `many` and `manyN` have counterparts `sepBy` and
`sepByN`. This was done because having a default separator was harder to
implement and having the types be correct. I'm sure it could be done, but this
solution works and is not harder to use.

To show how easy it is to port, here is the rgb example from mecha ported to
merrcha:

```zig
const std = @import("std");
const testing = std.testing;

const merrcha = @import("merrcha");

const Rgb = struct {
    r: u8,
    g: u8,
    b: u8,
};

const rgb = merrcha.combine(.{
    merrcha.ascii.char('#').discard(),
    merrcha.oneOf(.{ rgb2, rgb1 }),
    merrcha.eos,
});

const rgb1 = merrcha.manyN(hex1, 3).map(merrcha.toStruct(Rgb));
const rgb2 = merrcha.manyN(hex2, 3).map(merrcha.toStruct(Rgb));

const hex1 = merrcha.int(u4, .{
    .parse_sign = false,
    .base = 16,
    .max_digits = 1,
}).map(toByte);

const hex2 = merrcha.int(u8, .{
    .parse_sign = false,
    .base = 16,
    .max_digits = 2,
});

fn toByte(v: u4) u8 {
    return @as(u8, v) * 0x10 + v;
}

test "rgb" {
    const allocator = testing.allocator;

    const a = (try rgb.parse(allocator, "#aabbcc")).value;
    try testing.expectEqual(0xaa, a.r);
    try testing.expectEqual(0xbb, a.g);
    try testing.expectEqual(0xcc, a.b);

    const b = (try rgb.parse(allocator, "#abc")).value;
    try testing.expectEqual(0xaa, b.r);
    try testing.expectEqual(0xbb, b.g);
    try testing.expectEqual(0xcc, b.b);

    const c = (try rgb.parse(allocator, "#000000")).value;
    try testing.expectEqual(0x0, c.r);
    try testing.expectEqual(0x0, c.g);
    try testing.expectEqual(0x0, c.b);

    const d = (try rgb.parse(allocator, "#000")).value;
    try testing.expectEqual(0x0, d.r);
    try testing.expectEqual(0x0, d.g);
    try testing.expectEqual(0x0, d.b);
}
```

I mainly wrote this as an exercise in metaprogramming in zig, but I believe
it might actually be useful as well.

