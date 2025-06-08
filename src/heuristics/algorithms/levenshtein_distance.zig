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

  if (b.len == 0) return @intCast(a.len);

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

test LevenshteinDistance {
  const Test = struct {
    name: []const u8,
    a: []const u8,
    b: []const u8,
    expected: usize,
  };

  const tests = [_]Test{
    .{ .name = "Empty strings", .a = "", .b = "", .expected = 0 },
    .{ .name = "One empty string", .a = "kitten", .b = "", .expected = 6 },
    .{ .name = "Another empty string", .a = "", .b = "sitting", .expected = 7 },
    .{ .name = "Identical strings", .a = "kitten", .b = "kitten", .expected = 0 },
    .{ .name = "Simple substitution", .a = "kitten", .b = "sitten", .expected = 1 },
    .{ .name = "Simple insertion", .a = "kitten", .b = "kittens", .expected = 1 },
    .{ .name = "Simple deletion", .a = "kitten", .b = "kitte", .expected = 1 },
    .{ .name = "Substitution at the beginning", .a = "kitten", .b = "vitten", .expected = 1 },
    .{ .name = "Substitution at the end", .a = "kitten", .b = "kitteo", .expected = 1 },
    .{ .name = "Two substitutions and addition to end", .a = "kitten", .b = "sitting", .expected = 3 },
    .{ .name = "Longer strings", .a = "intention", .b = "execution", .expected = 5 },
    .{ .name = "Different lengths", .a = "abcdef", .b = "azced", .expected = 3 },
    .{ .name = "Transposition (should be 2 with simple implementation)", .a = "ca", .b = "ac", .expected = 2 },
    .{ .name = "Another transposition", .a = "abcd", .b = "badc", .expected = 3 },
  };

  inline for (tests) |tt| {
    const actual = try LevenshteinDistance(usize, tt.a, tt.b, std.testing.allocator);
    try std.testing.expectEqual(tt.expected, actual);
  }
}

/// Calculates the Optimal String Alignment (OSA) distance between two strings using a space-optimized approach.
/// Implementation adapted from https://wikipedia.org/wiki/Damerau-Levenshtein_distance
///
/// Time Complexity: O(n*m)
/// Space Complexity: O(3 * min(n,m))
pub fn LevenshteinOSADistance(comptime I: type, a_: []const u8, b_: []const u8, allocator: std.mem.Allocator) !I {
  var a = a_;
  var b = b_;

  if (a.len < b.len) {
    a = b_;
    b = a_;
  }

  // No exchanges if smaller str is shorter than 2
  if (b.len < 2) return LevenshteinDistance(I, a, b, allocator);

  const row_len = b.len + 1;
  var buf = try allocator.alloc(I, 3 * row_len);
  defer allocator.free(buf);

  var v0: []I = buf[0 * row_len .. 1 * row_len];
  var v1: []I = buf[1 * row_len .. 2 * row_len];
  var v2: []I = buf[2 * row_len .. 3 * row_len];

  // Initialize v0 same as in LevenshteinDistance (d[0][j] = j)
  for (0..row_len) |k| v0[k] = @intCast(k);

  // Fill v1 with first pass (for a[0]) - calculates d[1][j] for all j
  v1[0] = 1;
  for (0..b.len) |j| {
    var increment: I = 0;
    if (a[0] != b[j]) { increment = 1; }

    v1[j+1] = @min(
      @min(
        v0[j+1] + 1,     // deletion cost (from d[0][j+1])
        v1[j] + 1,       // insertion cost (from d[1][j])
      ),
      v0[j] + increment, // substitution cost (from d[0][j])
    );
  }

  // Main loop for remaining rows of `a`
  for (1..a.len) |i| {
    v2[0] = @intCast(i + 1);

    const increment_for_v2_1: I = if (a[i] != b[0]) 1 else 0;

    v2[1] = @min(
      @min(
        v1[1] + 1,                // deletion cost (from d[i][1])
        v2[0] + 1,                // insertion cost (from d[i+1][0])
      ),
      v1[0] + increment_for_v2_1, // substitution cost (from d[i][0])
    );

    // Inner loop for remaining columns of `b` (j from 1 to b.len-1)
    for (1..b.len) |j| {
      const increment: I = if (a[i] != b[j]) 1 else 0;

      v2[j+1] = @min(
        @min(
          v1[j+1] + 1,     // deletion cost (from d[i][j+1])
          v2[j] + 1,       // insertion cost (from d[i+1][j])
        ),
        v1[j] + increment, // substitution cost (from d[i][j])
      );

      if (a[i] == b[j-1] and a[i-1] == b[j]) {
        v2[j+1] = @min(v2[j+1], v0[j-1] + 1);
      }
    }

    // Rotate slices: v0 becomes old v1, v1 becomes old v2, v2 becomes old v0
    const temp = v0;
    v0 = v1;
    v1 = v2;
    v2 = temp;
  }

  // After the last swap, the results for the full distance are in v1.
  return v1[b.len];
}

test LevenshteinOSADistance {
  const Test = struct {
    name: []const u8,
    a: []const u8,
    b: []const u8,
    expected: usize,
  };

  const tests = [_]Test{
    .{ .name = "Empty strings", .a = "", .b = "", .expected = 0 },
    .{ .name = "One empty string", .a = "kitten", .b = "", .expected = 6 },
    .{ .name = "Another empty string", .a = "", .b = "sitting", .expected = 7 },
    .{ .name = "Identical strings", .a = "kitten", .b = "kitten", .expected = 0 },
    .{ .name = "Simple substitution", .a = "kitten", .b = "sitten", .expected = 1 },
    .{ .name = "Simple insertion", .a = "kitten", .b = "kittens", .expected = 1 },
    .{ .name = "Simple deletion", .a = "kitten", .b = "kitte", .expected = 1 },
    .{ .name = "Substitution at the beginning", .a = "kitten", .b = "vitten", .expected = 1 },
    .{ .name = "Substitution at the end", .a = "kitten", .b = "kitteo", .expected = 1 },
    .{ .name = "Two substitutions", .a = "kitten", .b = "sitting", .expected = 3 },
    .{ .name = "Longer strings", .a = "intention", .b = "execution", .expected = 5 },
    .{ .name = "Different lengths", .a = "abcdef", .b = "azced", .expected = 3 },
    .{ .name = "Simple transposition", .a = "ca", .b = "ac", .expected = 1 },
    .{ .name = "Another transposition", .a = "abcd", .b = "badc", .expected = 2 },
    .{ .name = "Transposition with other edits", .a = "kitten", .b = "sitting", .expected = 3 },
    .{ .name = "More complex transposition", .a = "mart", .b = "tram", .expected = 3 },
    .{ .name = "Consecutive transpositions (OSA restriction)", .a = "abdc", .b = "acbd", .expected = 2 },
    .{ .name = "Consecutive transpositions (OSA restriction) - 2", .a = "abcd", .b = "cadb", .expected = 4 },
  };

  inline for (tests) |tt| {
    const actual = try LevenshteinOSADistance(usize, tt.a, tt.b, std.testing.allocator);
    try std.testing.expectEqual(tt.expected, actual);
  }
}

/// Calculates the Damerau-Levenshtein distance between two strings.
/// Implementation from https://wikipedia.org/wiki/Damerau-Levenshtein_distance
///
/// Time Complexity: O(n*m)
/// Space Complexity: O(n*m)
pub fn DamerauLevenshteinDistance(comptime I: type, a_: []const u8, b_: []const u8, allocator: std.mem.Allocator) !I {
  var a = a_;
  var b = b_;

  if (a.len < b.len) {
    a = b_;
    b = a_;
  }

  // for string is less 3 characters, only short transpositions are possible
  if (b.len < 3) return LevenshteinOSADistance(I, a, b, allocator);

  // Using a fixed-size array for 'da' since ASCII has 256 characters.
  var da: [256]I = .{0} ** 256;

  const num_rows = a.len + 2;
  const num_cols = b.len + 2;
  var d: struct {
    d: []I,
    num_cols: usize,

    fn init(allocator_: std.mem.Allocator, n_r: usize, n_c: usize) !@This() {
      return @This(){.d = try allocator_.alloc(I, n_r * n_c), .num_cols = n_c}; // Store num_cols during init
    }

    fn deinit(self: *@This(), allocator_: std.mem.Allocator) void {
      allocator_.free(self.d);
    }

    fn get(self: *const @This(), row_idx: usize, col_idx: usize) I {
      return self.d[row_idx * self.num_cols + col_idx]; // Use the stored num_cols
    }

    fn set(self: *@This(), row_idx: usize, col_idx: usize, val: I) void {
      self.d[row_idx * self.num_cols + col_idx] = val; // Use the stored num_cols
    }
  } = try .init(allocator, num_rows, num_cols);
  defer d.deinit(allocator);

  const max_dist: I = @intCast(a.len + b.len + 1);

  d.set(0, 0, max_dist);

  for (1..num_rows) |i| {
    d.set(i, 0, max_dist);
    d.set(i, 1, @intCast(i - 1));
  }

  for (1..num_cols) |j| {
    d.set(0, j, max_dist);
    d.set(1, j, @intCast(j - 1));
  }

  for (1..a.len + 1) |i| {
    var db: I = 0;
    for (1..b.len + 1) |j| {
      const k = da[b[j - 1]];
      const l = db;
      var cost: I = 0;
      if (a[i - 1] == b[j - 1]) {
        cost = 0;
        db = @intCast(j);
      } else {
        cost = 1;
      }

      const substitution_cost = d.get(i - 1 + 1, j - 1 + 1) + cost;
      const insertion_cost = d.get(i + 1, j - 1 + 1) + 1;
      const deletion_cost = d.get(i - 1 + 1, j + 1) + 1;

      var transposition_cost: I = 0;
      if (k > 0 and l > 0) {
        transposition_cost = d.get(k - 1 + 1, l - 1 + 1) + @as(I, @intCast(i - k - 1)) + 1 + @as(I, @intCast(j - l - 1));
      } else {
        transposition_cost = max_dist;
      }

      d.set(i + 1, j + 1, @min(
        @min(
          substitution_cost,
          insertion_cost,
        ),
        @min(
          deletion_cost,
          transposition_cost,
        ),
      ));
    }
    da[a[i - 1]] = @intCast(i);
  }

  return d.get(a.len + 1, b.len + 1);
}

test DamerauLevenshteinDistance {
  const Test = struct {
    name: []const u8,
    a: []const u8,
    b: []const u8,
    expected: usize,
  };

  const tests = [_]Test{
    .{ .name = "Empty strings", .a = "", .b = "", .expected = 0 },
    .{ .name = "One empty string", .a = "kitten", .b = "", .expected = 6 },
    .{ .name = "Another empty string", .a = "", .b = "sitting", .expected = 7 },
    .{ .name = "Identical strings", .a = "kitten", .b = "kitten", .expected = 0 },
    .{ .name = "Simple substitution", .a = "kitten", .b = "sitten", .expected = 1 },
    .{ .name = "Simple insertion", .a = "kitten", .b = "kittens", .expected = 1 },
    .{ .name = "Simple deletion", .a = "kitten", .b = "kitte", .expected = 1 },
    .{ .name = "Substitution at the beginning", .a = "kitten", .b = "vitten", .expected = 1 },
    .{ .name = "Substitution at the end", .a = "kitten", .b = "kitteo", .expected = 1 },
    .{ .name = "Two substitutions and addition to end", .a = "kitten", .b = "sitting", .expected = 3 },
    .{ .name = "Longer strings", .a = "intention", .b = "execution", .expected = 5 },
    .{ .name = "Different lengths", .a = "abcdef", .b = "azced", .expected = 3 },
    .{ .name = "Transposition", .a = "ca", .b = "ac", .expected = 1 },
    .{ .name = "Longer Transposition", .a = "cxa", .b = "axc", .expected = 2 },
    .{ .name = "Another transposition", .a = "abcd", .b = "badc", .expected = 2 },
    .{ .name = "Transposition at start", .a = "abcde", .b = "bacde", .expected = 1 },
    .{ .name = "Two transpositions", .a = "abdcfe", .b = "adbcef", .expected = 2 },
    .{ .name = "Transposition and deletion", .a = "abcd", .b = "ac", .expected = 2 },
  };

  inline for (tests) |tt| {
    const actual = try DamerauLevenshteinDistance(usize, tt.a, tt.b, std.testing.allocator);
    try std.testing.expectEqual(tt.expected, actual);
  }
}

