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

    fn genExit(self: *Generator, exit: parse.NodeExit) !void {
        const expr = exit.expr;
        const newCode =
            switch (expr.kind) {
            .intLit => |intlit| try std.fmt.allocPrint(self.allocator,
                \\  mov rax, 60
                \\  mov rdi, {d}
                \\  syscall
                \\
            , .{
                intlit.intlit.intLit,
            }),
            .ident => |ident| try std.fmt.allocPrint(self.allocator,
                \\  mov rax, 60
                \\  mov rdi, [{s}]
                \\  syscall
                \\
            , .{
                ident.ident.ident,
            }),
        };
        try self.code.appendSlice(newCode);
        self.allocator.free(newCode);
    }

    fn genVar(self: *Generator, value: parse.NodeVar) !void {
        const str = try std.fmt.allocPrint(self.allocator,
            \\section .data
            \\  {s}: dw {d}
            \\
        , .{ value.ident.ident, switch (value.expr.kind) {
            .intLit => |intlit| intlit.intlit.intLit,
            else => return error.NotImplemented,
        } });
        defer self.allocator.free(str);
        try self.code.insertSlice(0, str);
    }

    fn genValue(self: *Generator, value: parse.NodeValue) !void {
        const str = try std.fmt.allocPrint(self.allocator,
            \\section .data
            \\  {s}: dw {d}
            \\
        , .{ value.ident.ident, switch (value.expr.kind) {
            .intLit => |intlit| intlit.intlit.intLit,
            else => return error.NotImplemented,
        } });
        defer self.allocator.free(str);
        try self.code.insertSlice(0, str);
    }

    fn genAssign(self: *Generator, assign: parse.NodeAssign) !void {
        const newCode =
            switch (assign.expr.kind) {
            .intLit => |intlit| try std.fmt.allocPrint(self.allocator,
                \\  mov rax, {d}
                \\  mov [{s}], rax
                \\
            , .{
                intlit.intlit.intLit,
                assign.ident.ident,
            }),
            .ident => |ident| try std.fmt.allocPrint(self.allocator,
                \\  mov rax, [{s}]
                \\  mov [{s}], rax
                \\
            , .{
                ident.ident.ident,
                assign.ident.ident,
            }),
        };
        try self.code.appendSlice(newCode);
        self.allocator.free(newCode);
    }

    pub fn generate(self: *Generator) ![]const u8 {
        try self.code.appendSlice(
            \\section .text
            \\  global _start
            \\_start:
            \\
        );
        for (self.root) |stmt| {
            switch (stmt.kind) {
                .exit => |exit| try self.genExit(exit),
                .defValue => |defValue| try self.genValue(defValue),
                .defVar => |defVar| try self.genVar(defVar),
                .assignVar => |assign| try self.genAssign(assign),
            }
        }
        return self.code.items;
    }
};

test "Codegen exit" {
    const tok = @import("tokenize.zig");
    const expect = std.testing.expect;
    const src = "exit 120;";
    var tokenizer = tok.Tokenizer.init(std.testing.allocator, src);
    defer tokenizer.deinit();
    const toks = try tokenizer.tokenize();
    var parser = parse.Parser.init(std.testing.allocator, toks);
    defer parser.deinit();
    const parseTree = try parser.parse();
    var gen = Generator.init(std.testing.allocator, parseTree);
    defer gen.deinit();
    const actual = try gen.generate();
    const expected =
        \\section .text
        \\  global _start
        \\_start:
        \\  mov rax, 60
        \\  mov rdi, 120
        \\  syscall
        \\
    ;
    try expect(std.mem.eql(u8, actual, expected));
}
