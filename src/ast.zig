const std = @import("std");
const tok = @import("tokenize.zig");

const SyntaxError = error{SyntaxError};
const expectedToken = error{ExpectedToken};

pub const BinOp = enum {
    Add,
    Sub,
    Mul,
    Div,
};

pub const Literal = union(enum) {
    Int: i32,
};

pub const Ast = union(enum) {
    Expr: struct {
        kind: ExprKind,
    },
    Stmt: struct {
        kind: StmtKind,
    },
};

const StmtKind = union(enum) {
    exit: Ast.Expr,
};

const ExprKind = union(enum) {
    Literal: Literal,
    BinaryOp: struct {
        op: BinOp,
        left: Ast.Expr,
        right: Ast.Expr,
    },
};

fn checkType(token: tok.Token, typ: tok.TokenType) bool {
    return switch (token) {
        typ => true,
        else => false,
    };
}

const AstParser = struct {
    tokens: tok.Iterator(tok.Token),
    fn parseStmt(self: *AstParser) !Ast.Stmt {
        return switch (self.tokens.peek().?) {
            .ret => try self.exitStmt(),
        };
    }

    fn parseExpr(self: *AstParser) !Ast.Expr {

    }

    fn exitStmt(self: *AstParser) !Ast.Stmt {
        if (!checkType(self.tokens.consume().?, tok.TokenType.ret)) return expectedToken;
        const value = self.parseExpr();

        if (!checkType(self.tokens.consume().?, tok.TokenType.semiCol)) return expectedToken;
        const kind = StmtKind{ .exit = value };
        return Ast.Stmt{ .kind = kind };
    }
};
