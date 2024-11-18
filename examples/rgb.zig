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
