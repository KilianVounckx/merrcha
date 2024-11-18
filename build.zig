const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    try b.modules.put(b.dupe("merrcha"), b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
    }));

    const tests = b.addTest(.{
        .name = "test_merrcha",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);

    try buildExamples(b, test_step, target, optimize);
}

fn buildExamples(
    b: *Build,
    test_step: *Build.Step,
    target: Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) !void {
    const dirname = "examples";
    var dir = try std.fs.cwd().openDir(dirname, .{ .iterate = true });
    defer dir.close();
    var walker = try dir.walk(b.allocator);
    defer walker.deinit();
    while (try walker.next()) |entry| {
        if (entry.kind != .file)
            continue;
        if (!std.mem.eql(u8, std.fs.path.extension(entry.basename), ".zig"))
            continue;
        const filename = b.pathJoin(&.{ dirname, entry.path });
        const testname = b.fmt("test_{s}", .{entry.path[0 .. entry.path.len - 4]});
        for (testname) |*c| {
            if (c.* == '/') {
                c.* = '-';
            }
        }
        const tests = b.addTest(.{
            .name = testname,
            .root_source_file = b.path(filename),
            .target = target,
            .optimize = optimize,
        });
        tests.root_module.addImport("merrcha", b.modules.get("merrcha").?);
        const run_test = b.addRunArtifact(tests);
        test_step.dependOn(&run_test.step);
    }
}
