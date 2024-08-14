const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "calico",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const llvm = b.dependency("llvm-zig", .{});
    _ = try b.modules.put("llvm", llvm.module("llvm"));
    exe.root_module.addImport("llvm", b.modules.get("llvm").?);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the compiler");
    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run unit tests");
    for ([_][]const u8{
        "src/main.zig",
        "src/tokenize.zig",
        "src/parser.zig",
        "src/codegen.zig",
    }) |file|
        unit_test(b, target, optimize, test_step, file);
}

fn unit_test(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    test_step: *std.Build.Step,
    fname: []const u8,
) void {
    const unit = b.addTest(.{
        .root_source_file = b.path(fname),
        .target = target,
        .optimize = optimize,
    });
    const unit_tests = b.addRunArtifact(unit);
    test_step.dependOn(&unit_tests.step);
    unit.root_module.addImport("llvm", b.modules.get("llvm").?);
}
