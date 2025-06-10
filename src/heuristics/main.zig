const std = @import("std");
const custom = @import("algorithms/custom.zig");
const jaro_similarity = @import("algorithms/jaro_similarity.zig");
const lcs_length = @import("algorithms/lcs_length.zig");
const levenshtein_distance = @import("algorithms/levenshtein_distance.zig");
const n_gram = @import("algorithms/n_gram.zig");

pub const SimilarityMeasure = @TypeOf(struct {
  fn inner(comptime I: type, comptime F: type, a: []const u8, b: []const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error!F {
    _ = .{ I, F, a, b, allocator };
    return undefined;
  }
}.inner);

/// A similarity measure that calculates a "Frequency Distance" between two strings.
/// It attempts to quantify how "far apart" characters and their positions are across strings,
/// especially when character frequencies differ.
///
/// C=256 (alphabet size)
/// Time complexity: O(a + b + C * log2(max(a, b)))
/// Space complexity: O(a + b + C)
pub fn FrequencySimilarity(comptime I: type, comptime F: type, a: []const u8, b: []const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error!F {
  return 1.0 - (try custom.FrequencyDistance(I, F, a, b, allocator));
}

test {
  std.testing.refAllDeclsRecursive(@This());
}

