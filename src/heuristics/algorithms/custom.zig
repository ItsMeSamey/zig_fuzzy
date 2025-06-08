const std = @import("std");

fn diff(comptime T: type, a: anytype, b: @TypeOf(a)) T {
  const result = if (a > b) a - b else b - a;
  if (@typeInfo(T) == .float) return @floatFromInt(result);
  return @intCast(result);
}

/// A dissimilarity measure that calculates a "Frequency Distance" between two strings.
/// It attempts to quantify how "far apart" characters and their positions are across strings,
/// especially when character frequencies differ.
///
/// C=256 (alphabet size)
/// Time complexity: O(a + b + C * log2(max(a, b)))
/// Space complexity: O(a + b + C)
pub fn FrequencyDistance(comptime I: type, comptime F: type, a_: []const u8, b_: []const u8, allocator: std.mem.Allocator) !F {
  @setEvalBranchQuota(10_000);
  var a = a_;
  var b = b_;

  if (a.len < b.len) {
    a = b_;
    b = a_;
  }

  if (b.len == 0) return @as(F, @floatFromInt(a.len));
  if (a.len == 1) return if (a[0] == b[0]) 0.0 else 1.0;

  // Initialize frequency maps to store indices of character occurrences.
  var fa_array: [256]std.ArrayListUnmanaged(I) = [_]std.ArrayListUnmanaged(I){.{}} ** 256;
  var fb_array: [256]std.ArrayListUnmanaged(I) = [_]std.ArrayListUnmanaged(I){.{}} ** 256;

  defer {
    inline for (0..256) |i| {
      fa_array[i].deinit(allocator);
      fb_array[i].deinit(allocator);
    }
  }

  for (0..a.len) |i| try fa_array[a[i]].append(allocator, @intCast(i));
  for (0..b.len) |i| try fb_array[b[i]].append(allocator, @intCast(i));

  var distance_no_div: F = 0.0;
  var distance: F = 0.0;
  const norm_factor_inner: F = @floatFromInt(b.len - 1);

  const lessThanFn = struct {
    fn inner(v_a: I, v_b: I) bool {
      return v_a < v_b;
    }
  }.inner;

  inline for (0..256) |char_code| {
    var ia = fa_array[char_code].items;
    var ib = fb_array[char_code].items;

    if (ia.len < ib.len) {
      ia = fb_array[char_code].items;
      ib = fa_array[char_code].items;
    }

    // If character counts are the same, sum absolute differences of corresponding indices
    if (ia.len == ib.len) {
      for (0..ia.len) |j| distance += diff(F, ia[j], ib[j]);
    } else if (ib.len == 0) { // character counts are different
      // All are unmatched
      distance_no_div += @floatFromInt(ia.len);
    } else if (ib.len == 1) { // Special matching for 1 occurrence
      if (ia.len == 1) {
        distance += diff(F, ia[0], ib[0]);
      } else {
        distance_no_div += @floatFromInt(ia.len - 1);

        var idx = std.sort.partitionPoint(I, ia, ib[0], lessThanFn);
        if (idx == ia.len) {
          idx = ia.len - 1;
        } else if (idx == 0) {
          idx = 1;
        }
        
        // Compare with element at idx and idx-1 (for closest match)
        distance += @floatFromInt(@min(diff(I, ia[idx], ib[0]), diff(I, ia[idx-1], ib[0])));
      }
    } else {
      distance_no_div += @as(F, @floatFromInt(ia.len - ib.len));

      var start_idx = std.sort.partitionPoint(I, ia, ib[0], lessThanFn);
      var end_idx = std.sort.partitionPoint(I, ia[start_idx..], ib[ib.len - 1], lessThanFn) + start_idx;

      if (end_idx == ia.len) {
        end_idx = ia.len - 1;
      }
      if (start_idx > end_idx) {
        start_idx = end_idx;
      }

      const mid = start_idx + (end_idx - start_idx) / 2;
      const is_start_longer: I = if (mid - start_idx > end_idx - mid) 1 else 0;
      if (mid < ib.len/2 + is_start_longer) {
        start_idx = 0;
        end_idx = ib.len - 1;
      } else if (mid + (ib.len/2 + (1^is_start_longer)) >= ia.len) {
        end_idx = ia.len - 1;
        start_idx = ia.len - ib.len;
      } else {
        start_idx = mid - (ib.len/2 + is_start_longer);
        end_idx = mid + (ib.len/2 + (1^is_start_longer));
      }

      ia = ia[start_idx..end_idx];

      const c_start = diff(I, ia[0], ib[0]);
      const c_end = diff(I, ia[ia.len - 1], ib[ib.len - 1]);
      distance += 2 * @as(F, @floatFromInt(@min(c_start, c_end)));

      for (1..ib.len - 1) |i| {
        const dis = diff(I, ia[i], ib[i]);
        distance += @floatFromInt(@min(@min(diff(I, dis, c_start), diff(I, dis, c_end)), dis));
      }
    }
  }

  return (distance_no_div + distance / norm_factor_inner) / @as(F, @floatFromInt(a.len + b.len));
}

test FrequencyDistance {
  const TestCase = struct {
    a: []const u8,
    b: []const u8,
  };

  const test_cases = [_]TestCase{
    .{ .a = "", .b = "" },
    .{ .a = "a", .b = "a" },
    .{ .a = "abb", .b = "bba" },
    .{ .a = "abc", .b = "acb" },
    .{ .a = "abc", .b = "bac" },
    .{ .a = "aabb", .b = "abab" },
    .{ .a = "aaaa", .b = "bbbb" },
    .{ .a = "abc", .b = "abcd" },
    .{ .a = "abcd", .b = "abc" },
    .{ .a = "apple", .b = "apxle" },
    .{ .a = "apple", .b = "apxpl" },
    .{ .a = "apple", .b = "axple" },
    .{ .a = "apple", .b = "bpple" },
    .{ .a = "hello", .b = "world" },
    .{ .a = "testing", .b = "test" },
    .{ .a = "test", .b = "testing" },
    .{ .a = "aaaaa", .b = "aaaba" },
    .{ .a = "aaaba", .b = "aaaaa" },
    .{ .a = "aaaaa", .b = "aabba" },
    .{ .a = "aabba", .b = "aaaaa" },
    .{ .a = "abcde", .b = "edcba" },
    .{ .a = "microsoft", .b = "mitsubishi" },
    .{ .a = "intention", .b = "execution" },
    .{ .a = "aaaa", .b = "aaa" },
    .{ .a = "aaa", .b = "aaaa" },
    .{ .a = "cat", .b = "act" },
    .{ .a = "dog", .b = "god" },
    .{ .a = "listen", .b = "silent" },
  };

  inline for (test_cases) |tc| {
    const value = try FrequencyDistance(u32, f32, tc.a, tc.b, std.testing.allocator);
    try std.testing.expect(value >= 0);
    try std.testing.expect(value <= 1);
  }
}

