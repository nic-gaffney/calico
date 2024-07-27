const std = @import("std");
const parse = @import("parser.zig");

pub const Generator = struct {
    root: parse.NodeExit,
    allocator: std.mem.Allocator,
    code: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator, root: parse.NodeExit) Generator {
        return .{
            .root = root,
            .allocator = allocator,
            .code = std.ArrayList(u8).init(allocator),
        };
    }

    fn genExit(self: *Generator) ![]const u8 {
        return try std.fmt.allocPrint(self.allocator,
            \\  mov rax, 60
            \\  mov rdi, {}
            \\  syscall
            \\
        , .{self.root.expr.intLit.intLit});
    }

    pub fn generate(self: *Generator) ![]const u8 {
        try self.code.appendSlice(
            \\global _start:
            \\
        );
        const exitStmt = try self.genExit();
        defer self.allocator.free(exitStmt);
        try self.code.appendSlice(exitStmt);
        return try self.code.toOwnedSlice();
    }
};
