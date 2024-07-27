const std = @import("std");
const parse = @import("parser.zig");

pub const Generator = struct {
    root: []const parse.NodeStmt,
    allocator: std.mem.Allocator,
    code: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator, stmts: []const parse.NodeStmt) Generator {
        return .{
            .root = stmts,
            .allocator = allocator,
            .code = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Generator) void {
        self.code.deinit();
    }

    fn genExit(self: *Generator, expr: parse.NodeExpr) ![]const u8 {
        return try std.fmt.allocPrint(self.allocator,
            \\  mov rax, 60
            \\  mov rdi, {d}
            \\  syscall
            \\
        , .{switch (expr) {
            .intLit => expr.intLit.intlit.intLit,
            else => return error.NotImplemented,
        }});
    }

    fn genValue(self: *Generator) ![]const u8 {
        _ = self;
        return error.NotImplemented;
    }

    pub fn generate(self: *Generator) ![]const u8 {
        try self.code.appendSlice(
            \\global _start:
            \\
        );
        for (self.root) |stmt| {
            const code = switch (stmt) {
                .exit => try self.genExit(stmt.exit.expr),
                .value => try self.genValue(),
            };
            try self.code.appendSlice(code);
        }
        return self.code.items;
    }
};
