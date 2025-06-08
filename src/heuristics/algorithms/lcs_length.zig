const std = @import("std");

/// Calculates the Levenshtein(/edit) distance between two strings using a space-optimized approach.
/// Implementation from https://wikipedia.org/wiki/Levenshtein_distance
///
/// Time Complexity = O(a * b)
/// Space Complexity = O(2 * min(a, b))
pub fn LCSLength(comptime I: type, a_: []const u8, b_: []const u8, allocator: std.mem.Allocator) !I {
  var a = a_;
  var b = b_;

  if (a.len < b.len) {
    a = b_;
    b = a_;
  }

  if (b.len == 0) return 0;

  var buf = try allocator.alloc(I, 2 * (b.len + 1));
  defer allocator.free(buf);

  var v0: []I = buf[0 .. b.len + 1];
  var v1: []I = buf[b.len + 1 .. 2 * (b.len + 1)];

  // Initialize v0
  @memset(v0, 0);
  v1[0] = 0;

  // Main loop
  for (0..a.len) |i| {
    for (0..b.len) |j| {
      v1[j + 1] = if (a[i] == b[j]) v0[j] + 1 else @max(v0[j + 1], v1[j]);
    }

    // Swap v0 and v1 slices by pointer update
    const temp = v0;
    v0 = v1;
    v1 = temp;
  }

  // After the last swap, the results are in v0 (because they were swapped back).
  return v0[b.len];
}

