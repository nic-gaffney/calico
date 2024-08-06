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

    const lib = b.addStaticLibrary(.{
        .name = "llvm",
        .root_source_file = b.path("lib/llvm-zig/src/llvm.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib.defineCMacro("_FILE_OFFSET_BITS", "64");
    lib.defineCMacro("__STDC_CONSTANT_MACROS", null);
    lib.defineCMacro("__STDC_FORMAT_MACROS", null);
    lib.defineCMacro("__STDC_LIMIT_MACROS", null);
    lib.linkSystemLibrary("z");
    lib.linkLibC();
    lib.linkSystemLibrary("LLVM-17");

    b.installArtifact(exe);

    b.installArtifact(lib);

    _ = try b.modules.put("llvm", &lib.root_module);
    exe.root_module.addImport("llvm", b.modules.get("llvm").?);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the compiler");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const token_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/tokenize.zig"),
        .target = target,
        .optimize = optimize,
    });

    const parse_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/parser.zig"),
        .target = target,
        .optimize = optimize,
    });

    const codegen_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/codegen.zig"),
        .target = target,
        .optimize = optimize,
    });

    codegen_unit_tests.root_module.addImport("llvm", &lib.root_module);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const run_token_unit_tests = b.addRunArtifact(token_unit_tests);
    const run_parse_unit_tests = b.addRunArtifact(parse_unit_tests);
    const run_codegen_unit_tests = b.addRunArtifact(codegen_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
    test_step.dependOn(&run_token_unit_tests.step);
    test_step.dependOn(&run_parse_unit_tests.step);
    test_step.dependOn(&run_codegen_unit_tests.step);
}
