const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const gl_bindings = @import("zigglgen").generateBindingsModule(b, .{
        .api = .gl,
        .version = .@"1.0",
        .extensions = &.{ .ARB_clip_control, .NV_scissor_exclusive },
    });

    const zglfw = b.dependency("zglfw", .{
        .target = target,
        .optimize = optimize,
    });

    exe_mod.addImport("gl", gl_bindings);
    exe_mod.addImport("glfw", zglfw.module("glfw"));

    const exe = b.addExecutable(.{
        .name = "lab4",
        .root_module = exe_mod,
    });

    exe.linkLibC();
    exe.linkSystemLibrary("glfw");

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
