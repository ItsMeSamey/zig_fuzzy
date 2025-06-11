const std = @import("std");
const heuristics = @import("heuristics/main.zig");

/// I is the integer type used for indexing, asserts that every string is less than std.math.maxInt(I), strs.len (length of strs array) can however be larger
/// F is the float type used for scores, sorting uses an array of F (same size as strs)
/// score_fn is a function that takes in 2 strings and returns a score as a result
/// optional_sort_fn is a function that takes in 2 uints and a context with `swap` and `lessThan` methods
pub fn GetSorter(I: type, F: type, score_fn: heuristics.SimilarityMeasure, optional_sort_fn: ?fn (usize, usize, anytype) void) type {
  const sort_fn = optional_sort_fn orelse std.sort.pdqContext;
  return struct {
    /// Takes in 2 strings and returns the score as a result
    pub fn score(target: []const u8, str: []const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error!F {
      return score_fn(I, F, target, str, allocator);
    }

    /// Sorts all the strings in the given array with respect to the `target` string
    pub fn sort(target: []const u8, strs: [][]const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
      return sortContext(target, struct {
        strs: [][]const u8,

        fn len(context: void) usize {
          return context.strs.len;
        }

        fn get(context: void, index: usize) []const u8 {
          return context.strs[index];
        }

        fn swap(context: void, a: usize, b: usize) void {
          std.mem.swap([]const u8, &context.strs[a], &context.strs[b]);
        }
      }{ .strs = strs }, allocator);
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

      sort_fn(0, list.len, struct {
        list: []F,
        subcontext: @TypeOf(strs),

        fn swap(self: *@This(), a: usize, b: usize) void {
          self.subcontext.swap(a, b);
          std.mem.swap(F, &self.list[a], &self.list[b]);
        }

        fn lessThan(self: *@This(), a: usize, b: usize) bool {
          return self.list[a] < self.list[b];
        }
      }{ .list = list, .subcontext = strs });
    }

    pub const Options = struct {
      /// Strings that have similarity scores less than this value will be ignored
      threshold: F,

      // /// Minimum number of strings that must be sorted,
      // min_sort_size: usize,
    };

    /// Sorts all the strings in the given array with respect to the `target` string
    /// Returns the number of strings that were sorted, which will equal the length of `strs` if every string was sorted
    /// only strs[0..return_value] should be used
    ///
    /// Note: This function may return 0 if no strings were above the threshold
    pub fn sortOptions(target: []const u8, strs: [][]const u8, allocator: std.mem.Allocator, options: Options) std.mem.Allocator.Error!usize {
      return sortContextOptions(target, struct {
        strs: [][]const u8,

        fn len(context: void) usize {
          return context.strs.len;
        }

        fn get(context: void, index: usize) []const u8 {
          return context.strs[index];
        }

        fn swap(context: void, a: usize, b: usize) void {
          std.mem.swap([]const u8, &context.strs[a], &context.strs[b]);
        }
      }{ .strs = strs }, allocator, options);
    }

    /// Sort all `strs` in the given context with respect to the `target` string
    /// returns the number of strings that were sorted, which will equal the length of `strs` if every string was sorted
    /// only strs[0..return_value] should be used
    /// `strs` must have the following functions available:
    ///   - `len() usize`
    ///   - `get(index: usize) []const u8`
    ///   - `swap(a: usize, b: usize) void`
    ///
    /// Note: This function may return 0 if no strings were above the threshold
    pub fn sortContextOptions(target: []const u8, strs: anytype, allocator: std.mem.Allocator, options: Options) std.mem.Allocator.Error!usize {
      const list = try allocator.alloc(F, strs.len());
      defer allocator.free(list);

      var i: usize = 0;
      var j: usize = strs.len() - 1;

      // This is done to minimize the number of swaps
      while (true) {
        while (j > i): (j -= 1) {
          list[j] = try score(target, strs.get(j), allocator);
          if (list[j] >= options.threshold) {
            std.mem.swap(F, &list[i], &list[j]);
            strs.swap(i, j);
            i += 1;
            break;
          }
        }

        while (j > i): (i += 1) {
          list[i] = try score(target, strs.get(i), allocator);
          if (list[i] < options.threshold) { break; }
        }

        if (i == j) {
          list[i] = try score(target, strs.get(i), allocator);
          if (list[i] >= options.threshold) i += 1;
          break;
        }
      }

      sort_fn(0, i, struct {
        list: []F,
        subcontext: @TypeOf(strs),

        fn swap(self: *@This(), a: usize, b: usize) void {
          self.subcontext.swap(a, b);
          std.mem.swap(F, &self.list[a], &self.list[b]);
        }

        fn lessThan(self: *@This(), a: usize, b: usize) bool {
          return self.list[a] < self.list[b];
        }
      }{ .list = list, .subcontext = strs });

      return i;
    }
  };
}

