const std = @import("std");
const heuristics = @import("heuristics/main.zig");

/// I is the integer type used for indexing, asserts that every string is less than std.math.maxInt(I), strs.len (length of strs array) can however be larger
/// F is the float type used for scores, sorting uses an array of F (same size as strs)
/// score_fn is a function that takes in 2 strings and returns a score as a result
/// optional_sort_fn is a function that takes in 2 uints and a context with `swap` and `lessThan` methods
pub fn GetSorter(I: type, F: type, score_fn: heuristics.SimilarityMeasure, optional_sort_fn: ?fn (usize, usize, anytype) void) type {
  const sort_fn = optional_sort_fn orelse std.sort.pdqContext;

  const ContextUtils = struct {
    fn ArrayContextType() type {
      return struct {
        strs: [][]const u8,

        fn len(self: *const @This()) usize {
          return self.strs.len;
        }

        fn get(self: *const @This(), index: usize) []const u8 {
          return self.strs[index];
        }

        fn swap(self: *const @This(), a: usize, b: usize) void {
          std.mem.swap([]const u8, &self.strs[a], &self.strs[b]);
        }
      };
    }
    pub fn arrayContext(strs: [][]const u8) ArrayContextType() {
      return .{ .strs = strs };
    }

    fn SupercontextType(comptime T: type) type {
      return struct {
        strs: T,
        scores: [*]F,

        fn len(self: *const @This()) usize {
          return self.strs.len();
        }

        fn getScore(self: *const @This(), index: usize) F {
          return self.scores[index];
        }

        fn swap(self: *const @This(), a: usize, b: usize) void {
          self.strs.swap(a, b);
        }

        fn lessThan(self: *const @This(), a: usize, b: usize) bool {
          return self.scores[a] < self.scores[b];
        }
      };
    }

    fn superContext(strs: anytype, costs: []F) SupercontextType(@TypeOf(strs)) {
      std.debug.assert(strs.len() == costs.len);
      return .{ .strs = strs, .costs = undefined };
    }
  };

  return struct {
    /// Takes in 2 strings and returns the score as a result
    pub fn score(target: []const u8, str: []const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error!F {
      return score_fn(I, F, target, str, allocator);
    }

    /// Sorts all the strings in the given array with respect to the `target` string
    pub fn sort(target: []const u8, strs: [][]const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
      return sortContext(target, ContextUtils.arrayContext(strs), allocator);
    }

    /// Sort all `strs` in the given context with respect to the `target` string
    /// `strs` must have the following functions available:
    ///   - `len() usize`
    ///   - `get(index: usize) []const u8`
    ///   - `swap(a: usize, b: usize) void`
    pub fn sortContext(target: []const u8, strs: anytype, allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
      const list = try allocator.alloc(F, strs.len());
      defer allocator.free(list);

      for (0..strs.len()) |i| list[i] = try score(target, strs.get(i), allocator);

      sort_fn(0, list.len, ContextUtils.superContext(strs, list));
    }

    pub const Options = struct {
      /// Strings that have similarity scores less than this value will be ignored
      /// NOTE: Null threshold is not the same as 0, null means no threshold because it may be negative for some custom heuristics
      /// NOTE: If bounds are specified, they are given priority over this, this is then just a hint in that case
      ///   This falls back to 0.5 if not specified in that case
      threshold: ?F = null,

      /// Minimum number of strings that must be sorted,
      /// Min: Minimum number of strings that must be sorted, set it to 0 to allow undershooting
      /// Max: Set this to any value >= strs.len() to allow overshooting (usually set to std.math.maxInt(usize))
      /// NOTE: bounds are inclusive => min <= return_value <= max
      target_len: ?Bounds = null,

      pub const Bounds = struct {
        min: usize,
        max: usize,
      };
    };

    /// Sorts all the strings in the given array with respect to the `target` string
    /// Returns the number of strings that were sorted, which will equal the length of `strs` if every string was sorted
    /// only strs[0..return_value] should be used
    ///
    /// NOTE: This function may return 0 if no strings were above the threshold
    /// WARNING: This function may NOT respect the upper bound in all cases, but will always respect lower bound
    pub fn sortOptions(target: []const u8, strs: [][]const u8, allocator: std.mem.Allocator, op: Options) std.mem.Allocator.Error!usize {
      return sortContextOptions(target, ContextUtils.arrayContext(strs), allocator, op);
    }

    /// Sort all `strs` in the given context with respect to the `target` string
    /// returns the number of strings that were sorted, which will equal the length of `strs` if every string was sorted
    /// only strs[0..return_value] should be used
    /// `strs` must have the following functions available:
    ///   - `len() usize`
    ///   - `get(index: usize) []const u8`
    ///   - `swap(a: usize, b: usize) void`
    ///
    /// this function asserts (op.target_len.?.min <= op.target_len.?.max) if op.target_len != null
    /// NOTE: This function may return 0 if no strings were above the threshold
    /// WARNING: This function may NOT respect the upper bound in all cases, but will always respect lower bound
    pub fn sortContextOptions(target: []const u8, strs: anytype, allocator: std.mem.Allocator, op: Options) std.mem.Allocator.Error!usize {
      std.debug.assert(op.target_len != null and op.target_len.?.min <= op.target_len.?.max);

      if (strs.len() == 0) return 0;

      if (
        (op.threshold == null and (op.target_len == null or op.target_len.?.max >= strs.len())) or
        (op.target_len != null and (op.target_len.?.min >= strs.len() or (op.target_len.?.max >= strs.len() and op.target_len.?.min == 0)))
      ) {
        sortContext(target, strs, allocator);
        return strs.len();
      }

      const list = try allocator.alloc(F, strs.len());
      defer allocator.free(list);

      for (0..strs.len()) |i| {
        list[i] = try score(target, strs.get(i), allocator);
      }

      const result = getBounds(ContextUtils.superContext(strs, list), op);

      if (op.target_len) |bounds| {
        // This is an implementation error, if this happens please report it
        std.debug.assert(bounds.min <= result);
        // This is not guaranteed
        // std.debug.assert(result <= bounds.max);
        
      }

      return result;
    }

    /// Returns the bounds that need to be sorted
    fn getBounds(ctx: anytype, op: Options) usize {
      std.debug.assert(op.threshold != null or op.target_len != null);

      const bounds = if (op.target_len) |bounds| bounds else .{ .min = 0, .max = ctx.len() };
      const min = bounds.min;
      const max = @min(bounds.max, ctx.len());

      var i: usize = 0;
      var i_old: usize = 0;
      var j: usize = ctx.len() - 1;

      var threshold: F = if (op.threshold) |t| t
        else @as(F, @floatFromInt(op.target_len.?.min + (@min(op.target_len.?.max, ctx.len()) - op.target_len.?.min) / 2)) / @as(F, @floatFromInt(ctx.len()));
      // Note: threshold_min and threshold_max are just suggestions here, it may actually be lower or higher
      var threshold_min: F = 0;
      var threshold_min_strong = false;
      var threshold_max: F = 1;
      var threshold_max_strong = false;

      var num_iters = std.math.log2(ctx.len()) * 2;
      // This is done to minimize the number of swaps
      while (num_iters > 0): (num_iters -= 1) {
        while (j > i) {
          if (ctx.getScore(j) >= threshold) {
            ctx.swap(i, j);
            i += 1; j -= 1;
            break;
          } else {
            j -= 1;
          }
        }

        while (j > i): (i += 1) {
          if (ctx.getScore(i) < threshold) { break; }
        }

        if (i == j) {
          if (ctx.getScore(i) >= threshold) i += 1;
          i += 1;

          if (i <= max) {
            sort_fn(i_old, i, ctx);
            if (i >= min) return i;
            // Need to lower threshold
            threshold_max = threshold;
            threshold_max_strong = true;
            const old = threshold;
            threshold = (threshold + threshold) / 2;
            if (ctx.getScore(i) < threshold) {
              threshold = (ctx.getScore(i) + threshold) / 2;
            }
            if (!threshold_max_strong) {
              threshold_min = @min(ctx.getScore(i), threshold_min);
              if (old < threshold) threshold -= 2 * (threshold - old);
            }
          } else {
            // Need to raise threshold
            const old = threshold;
            threshold_min = threshold;
            threshold_min_strong = true;
            threshold = (threshold + threshold_max) / 2;
            if (ctx.getScore(i) > threshold) {
              threshold = (ctx.getScore(i) + threshold) / 2;
            }
            if (!threshold_min_strong) {
              threshold_max = @max(ctx.getScore(i), threshold_max);
              if (old > threshold) threshold += 2 * (old - threshold);
            }
          }

          i_old = i;
        }
      }

      sort_fn(i_old, ctx.len(), ctx);
      return ctx.len();
    }
  };
}

