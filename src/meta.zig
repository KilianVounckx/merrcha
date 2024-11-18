const std = @import("std");
const testing = std.testing;

pub fn ErrorFromName(comptime name: []const u8) type {
    const name_zero = (name[0..] ++ .{0})[0..name.len :0];
    return @Type(.{ .error_set = &.{.{ .name = name_zero }} });
}

pub fn errorFromName(comptime name: []const u8) ErrorFromName(name) {
    return @field(ErrorFromName(name), name);
}

test "errorFromName" {
    try testing.expectError(
        error.Foobar,
        @as(error{Foobar}!void, errorFromName("Foobar")),
    );
}
