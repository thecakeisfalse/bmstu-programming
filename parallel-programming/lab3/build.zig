const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{
        .preferred_optimize_mode = .ReleaseFast,
    });

    const cpp_flags = [_][]const u8{
        "-std=c++23",
        "-openmp",
        "-Ofast",
    };

    const module = b.createModule(.{
        .link_libc = true,
        .link_libcpp = true,
        .optimize = optimize,
        .target = target,
    });

    module.addCSourceFile(.{
        .file = b.path("src/main.cc"),
        .flags = &cpp_flags,
    });

    const exe = b.addExecutable(.{
        .name = "lab3",
        .root_module = module,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
