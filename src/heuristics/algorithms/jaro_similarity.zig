const std = @import("std");

/// JaroDistance calculates the Jaro distance between two strings.
///
/// The Jaro distance is a measure of similarity between two strings using the following formula:
/// jaro_distance = 1/3 * (m/|s1| + m/|s2| + (m - t)/m)
///
/// Time Complexity = O(a * b)
/// Space Complexity = O(a + b)
pub fn JaroSimilarity(comptime I: type, comptime F: type, a_: []const u8, b_: []const u8, allocator: std.mem.Allocator) !F {
  var a = a_;
  var b = b_;

  if (a.len < b.len) {
    a = b_;
    b = a_;
  }

  if (b.len == 0) {
    return if (a.len == 0) 1.0 else 0.0;
  }

  // This is done to reduce the number of allocation done down to 1
  const MaskInt = std.bit_set.DynamicBitSetUnmanaged.MaskInt;
  const maskIntSize = @bitSizeOf(MaskInt);

  const a_count = (a.len + maskIntSize - 1) / maskIntSize;
  const b_count = (b.len + maskIntSize - 1) / maskIntSize;

  const allocation = try allocator.alloc(MaskInt, a_count + b_count);
  defer allocator.free(allocation);
  @memset(allocation[0..a_count + b_count], 0);

  var a_matches: std.bit_set.DynamicBitSetUnmanaged = .{ .bit_length = a.len, .masks = allocation[0..].ptr};
  var b_matches: std.bit_set.DynamicBitSetUnmanaged = .{ .bit_length = b.len, .masks = allocation[a_count..].ptr};

  const match_distance = a.len / 2 - 1;
  var matches: I = 0;

  // Find the number of matching characters.
  for (0..a.len) |i| {
    const start_idx = if (i >= match_distance) i - match_distance else 0;
    const end_idx = @min(b.len - 1, i + match_distance);

    for (start_idx..end_idx + 1) |j| {
      if (a[i] == b[j] and !b_matches.isSet(j)) {
        a_matches.set(i);
        b_matches.set(j);
        matches += 1;
        break;
      }
    }
  }

  if (matches == 0) return 0.0;

  // Calculate the number of transpositions.
  var transpositions: I = 0;
  var k: I = 0;
  for (0..a.len) |i| {
    if (a_matches.isSet(i)) {
      while (k < b.len and !b_matches.isSet(k)) {
        k += 1;
      }
      if (k < b.len and a[i] != b[k]) {
        transpositions += 1;
      }
      k += 1;
    }
  }
  transpositions /= 2;

  // Calculate the Jaro distance.
  return (
    @as(F, @floatFromInt(matches)) / @as(F, @floatFromInt(a.len)) +
    @as(F, @floatFromInt(matches)) / @as(F, @floatFromInt(b.len)) +
    (@as(F, @floatFromInt(matches)) - @as(F, @floatFromInt(transpositions))) / @as(F, @floatFromInt(matches))
  ) / 3.0;
}

test JaroSimilarity {
  const Test = struct {
    name: []const u8,
    a: []const u8,
    b: []const u8,
    expected: f64,
  };

  const tests = [_]Test{
    .{ .name = "Empty strings", .a = "", .b = "", .expected = 1.0 },
    .{ .name = "One empty string", .a = "kitten", .b = "", .expected = 0.0 },
    .{ .name = "Another empty string", .a = "", .b = "sitting", .expected = 0.0 },
    .{ .name = "Identical strings", .a = "kitten", .b = "kitten", .expected = 1.0 },
    .{ .name = "Simple substitution", .a = "kitten", .b = "sitten", .expected = 0.888888888888889 },
    .{ .name = "Transposition", .a = "MARTHA", .b = "MARHTA", .expected = 0.9444444444444445 },
    .{ .name = "Different lengths", .a = "CRATE", .b = "TRACE", .expected = 0.7333333333333333 },
    .{ .name = "No match", .a = "foo", .b = "bar", .expected = 0.0 },
    .{ .name = "Partial match 1", .a = "aaa", .b = "aab", .expected = 0.777777777777778 },
    .{ .name = "Partial match 2", .a = "very", .b = "vary", .expected = 0.8333333333333333 },
  };

  inline for (tests) |tt| {
    const actual = try JaroSimilarity(u32, f64, tt.a, tt.b, std.testing.allocator);
    try std.testing.expectApproxEqAbs(tt.expected, actual, 1e-9);
  }
}

// NOTE: Winkler variants can be constructed by using wrapper functions present in heuristics/main.zig

