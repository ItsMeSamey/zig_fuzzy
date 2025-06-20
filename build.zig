const std = @import("std");

pub fn build(b: *std.Build) void {
  const target = b.standardTargetOptions(.{});
  const optimize = b.standardOptimizeOption(.{});

  const fuzzy_module = b.addModule("fuzzy", .{
    .root_source_file = b.path("src/root.zig"),
    .single_threaded = true,
  });

  const options = b.addOptions();
  fuzzy_module.addOptions("build", options);

  const lib_unit_tests = b.addTest(.{
    .root_module = b.addModule(.{
      .root_source_file = b.path("src/root.zig"),
      .target = target,
      .optimize = optimize,
    }),
  });

  const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

  const test_step = b.step("test", "Run unit tests");
  test_step.dependOn(&run_lib_unit_tests.step);
}

