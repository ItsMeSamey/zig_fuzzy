const std = @import("std");

/// Calculates the Levenshtein(/edit) distance between two strings using a space-optimized approach.
/// Implementation from https://wikipedia.org/wiki/Levenshtein_distance
///
/// Time Complexity: O(a * b)
/// Space Complexity: O(2 * min(a, b))
pub fn LevenshteinDistance(comptime I: type, a_: []const u8, b_: []const u8, allocator: std.mem.Allocator) !I {
  var a = a_;
  var b = b_;

  if (a.len < b.len) {
    a = b_;
    b = a_;
  }

  if (b.len == 0) return a.len;

  const buffer_size_in_usize = 2 * (b.len + 1);
  var buf = try allocator.alloc(I, buffer_size_in_usize);
  defer allocator.free(buf);

  var v0: []I = buf[0 .. b.len + 1];
  var v1: []I = buf[b.len + 1 .. 2 * (b.len + 1)];

  // initialize v0 (the previous row of distances)
  for (0..b.len + 1) |i| v0[i] = @intCast(i);

  for (0..a.len) |i| {
    // calculate v1 (current row distances) from the previous row v0
    v1[0] = @intCast(i + 1);

    for (0..b.len) |j| {
      var increment: usize = 0;
      if (a[i] != b[j]) { increment = 1; }

      v1[j+1] = @min(
        @min(
          v0[j+1] + 1,     // deletion cost
          v1[j] + 1,       // insertion cost
        ),
        v0[j] + increment, // substitution cost
      );
    }

    const temp = v0;
    v0 = v1;
    v1 = temp;
  }

  return v0[b.len];
}


