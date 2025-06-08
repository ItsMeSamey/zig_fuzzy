const std = @import("std");

/// JaroDistance calculates the Jaro distance between two strings.
///
/// The Jaro distance is a measure of similarity between two strings using the following formula:
/// jaro_distance = 1/3 * (m/|s1| + m/|s2| + (m - t)/m)
///
/// Time Complexity = O(a * b)
/// Space Complexity = O(a + b)
pub fn JaroSimilarity(comptime F: type, a_: []const u8, b_: []const u8, allocator: std.mem.Allocator) F {
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

  const allocation = try allocator.alloc(MaskInt, a_count + 1 + b_count + 1);
  defer allocator.free(allocation);
  allocation[0] = a_count;
  allocation[a_count + 1] = b_count;
  @memset(allocation[1..a_count + 1], 0);
  @memset(allocation[a_count + 1 + 1..], 0);

  std.debug.assert(allocation[0] == a_count);
  std.debug.assert(allocation[a_count + 1] == b_count);

  var a_matches: std.bit_set.DynamicBitSetUnmanaged = .{ .bit_length = a.len, .masks = allocation[1..]};
  var b_matches: std.bit_set.DynamicBitSetUnmanaged = .{ .bit_length = a.len, .masks = allocation[a_count + 1 + 1..]};

  const match_distance = a.len / 2 - 1;
  var matches: usize = 0;

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
  var transpositions: usize = 0;
  var k: usize = 0;
  for (0..a.len) |i| {
    if (a_matches[i]) {
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

/// Calculates the Jaro-Winkler distance between two strings by
/// giving more favorable ratings to strings that match from the beginning.
///
/// Time Complexity = O(a * b)
/// Space Complexity = O(a + b)
pub fn JaroWinklerSimilarity(comptime F: type, a: []const u8, b: []const u8, prefix_l: F, prefix_limit: usize) F {
  const jaro = JaroSimilarity(F, a, b);

  // Calculate the length of the matching prefix.
  var prefix: usize = 0;
  const common_len = @min(a.len, b.len);
  
  while (prefix < prefix_limit and prefix < common_len and a[prefix] == b[prefix]) {
    prefix += 1;
  }

  return jaro + @as(F, @floatFromInt(prefix)) * prefix_l * (1.0 - jaro);
}

/// Calculates the Jaro-Winkler distance between two strings by
/// giving more favorable ratings to strings that match from the beginning And the end.
///
/// Time Complexity = O(a * b)
/// Space Complexity = O(a + b)
pub fn JaroWinklerSimilarityBidirectional(comptime F: type, a: []const u8, b: []const u8, prefix_l: F, prefix_limit: usize, suffix_l: F, suffix_limit: usize) F {
  const jaro_dist = JaroSimilarity(F, a, b);

  // Calculate the length of the matching prefix.
  var prefix: usize = 0;
  const common_len_prefix = @min(a.len, b.len);

  while (prefix < prefix_limit and prefix < common_len_prefix and a[prefix] == b[prefix]) {
    prefix += 1;
  }

  // Calculate the length of the matching suffix.
  var suffix: usize = 0;
  const common_len_suffix = common_len_prefix - prefix;

  while (suffix < suffix_limit and suffix < common_len_suffix and a[a.len - 1 - suffix] == b[b.len - 1 - suffix]) {
    suffix += 1;
  }

  return jaro_dist + @as(F, @floatFromInt(prefix)) * prefix_l * (1.0 - jaro_dist) + @as(F, @floatFromInt(suffix)) * suffix_l * (1.0 - jaro_dist);
}

