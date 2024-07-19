const std = @import("std");
const tok = @import("tokenize.zig");

const SyntaxError = error{SyntaxError};
const expectedToken = error{ExpectedToken};

pub const BinOp = enum(u32) {
    Add = 0b000,
    Sub = 0b001,
    Mul = 0b100,
    Div = 0b101,
    Mod = 0b110,

    fn init(typ: tok.TokenType) ?BinOp {
        return switch (typ) {
            .plus => .Add,
            .minus => .Sub,
            .star => .Mul,
            .slash => .Div,
            else => null,
        };
    }

    fn greater(self: BinOp, other: ?BinOp) bool {
        if (other == null) return true;
        return (@intFromEnum(self) >> 2) > (@intFromEnum(other.?) >> 2);
    }
};

pub const Literal = union(enum) {
    Int: i32,
};

pub const AstExpr = struct {
    kind: ExprKind,
    allocator: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator) !*AstExpr {
        const value = try allocator.create(AstExpr);
        value.* = .{
            .kind = .Void,
            .allocator = allocator,
        };
        return value;
    }
    pub fn deinit(self: *AstExpr) void {
        self.allocator.free(self);
    }
};

pub const AstStmt = struct {
    kind: StmtKind,
    allocator: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator) !*AstStmt {
        const value = try allocator.create(AstStmt);
        value.* = .{
            .kind = .Void,
            .allocator = allocator,
        };
        return value;
    }
    pub fn deinit(self: *AstStmt) void {
        self.allocator.free(self);
    }
};

pub const Ast = union(enum) {
    Expr: AstExpr,
    Stmt: AstStmt,
};

const StmtKind = union(enum) {
    exit: AstExpr,
    Void,
};

const ExprKind = union(enum) {
    Literal: Literal,
    BinaryOp: struct {
        op: BinOp,
        left: *AstExpr,
        right: *AstExpr,
    },
    Void,
};

const AstParser = struct {
    tokens: tok.Iterator(tok.Token),
    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,

    fn init(allocator: *std.heap.ArenaAllocator, tokens: tok.Iterator(tok.Token)) AstParser {
        return AstParser{
            .tokens = tokens,
            .arena = allocator.*,
            .allocator = allocator.allocator(),
        };
    }

    fn deinit(self: *AstParser) void {
        self.arena.deinit();
    }

    fn parseStmt(self: *AstParser) !AstStmt {
        return switch (self.tokens.peek().?) {
            .ret => self.exitStmt(),
            else => error.SyntaxError,
        };
    }

    fn parseExpr(self: *AstParser, lastOp: ?BinOp) !*AstExpr {
        if (!tok.checkType(self.tokens.peek().?, tok.TokenType.intLit)) return error.ExpectedToken;
        const kind = ExprKind{ .Literal = .{ .Int = self.tokens.consume().?.intLit } };
        var lhs = try AstExpr.init(self.allocator);
        lhs.*.kind = kind;
        while (self.tokens.peek()) |tokn| {
            const op = BinOp.init(tokn);
            if (op != null and op.?.greater(lastOp)) {
                self.tokens.skip();
                const rhs = try self.parseExpr(op);
                const newkind = ExprKind{ .BinaryOp = .{
                    .op = op.?,
                    .left = lhs,
                    .right = rhs,
                } };
                lhs = try AstExpr.init(self.allocator);
                lhs.*.kind = newkind;
            }
            return lhs;
        }
        return lhs;
    }

    fn exitStmt(self: *AstParser) !AstStmt {
        if (!tok.checkType(self.tokens.consume().?, tok.TokenType.ret))
            return error.ExpectedToken;
        const value = try self.parseExpr(null);

        if (!tok.checkType(self.tokens.consume().?, tok.TokenType.semiCol)) return error.ExpectedToken;
        const kind = StmtKind{ .exit = value.* };
        const stmt = try AstStmt.init(self.allocator);
        stmt.kind = kind;
        return stmt.*;
    }
};

test "AstParse" {
    std.testing.log_level = std.log.Level.info;
    const expect = std.testing.expect;
    const testSource: []const u8 = "exit 120 + 150;";
    var toks = tok.Tokenizer.init(std.testing.allocator, testSource);
    defer toks.deinit();
    var arrtoks = try toks.tokenize();
    const slice = try arrtoks.toOwnedSlice();
    const iter = tok.Iterator(tok.Token).init(slice);
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    var parser = AstParser.init(&arena, iter);
    defer parser.deinit();
    const stmt = try parser.parseStmt();
    _ = stmt;
    _ = expect;
}
