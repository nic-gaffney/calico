const std = @import("std");
const m = @import("main.zig");

pub fn comptimeError(err: anytype) void {
    // const stderr = std.fs.File.stderr();
    // var buff: [256]u8 = undefined;
    // const errWriter = stderr.writer(&buff);
    // var errWriterInterface = errWriter.interface;
    std.debug.print("{s}:{d}: {any}: Expected '{any}' got '{any}'\n", .{ m.publicFileName, err.line, err.err, err.exp, err.got });
}

pub fn map(comptime T: type, comptime F: type, slice: []const F, func: fn (F) T) []const T {
    var list: [64]T = undefined;
    var max: usize = 0;
    for (slice, 0..) |item, i| {
        list[i] = func(item);
        max = i + 1;
    }
    return list[0..max];
}
