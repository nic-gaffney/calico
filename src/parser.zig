const std = @import("std");
const tok = @import("tokenize.zig");
const symb = @import("symtable.zig");
const Iterator = tok.Iterator;
const Token = tok.Token;

const ParsingError = error{
    InvalidExpression,
    ExpectedExit,
    ExpectedSemicolon,
    ExpectedEqual,
    ExpectedIdentifier,
    InvalidStatement,
};

pub const Node = union(enum) {
    Expr: NodeExpr,
    Stmt: NodeStmt,

    pub fn children(self: Node, allocator: std.mem.Allocator) ![]Node {
        var childrenArray = std.ArrayList(Node).init(allocator);
        defer childrenArray.deinit();
        switch (self) {
            .Expr => |expr| try childrenArray.appendSlice(try expr.children()),
            .Stmt => |stmt| try childrenArray.appendSlice(try stmt.children()),
        }
        return try childrenArray.toOwnedSlice();
    }
};

pub const NodeExpr = struct {
    id: u32,
    kind: ExprKind,
    symtable: *symb.SymbolTable,
    typ: ?symb.SymbType,
    isConst: bool,

    pub fn asNode(self: NodeExpr) Node {
        return Node{ .Expr = self };
    }

    pub fn children(self: NodeExpr, allocator: std.mem.Allocator) ![]Node {
        var childrenArray = std.ArrayList(Node).init(allocator);
        defer childrenArray.deinit();
        switch (self.kind) {
            else => {},
        }
        return try childrenArray.toOwnedSlice();
    }
};

pub const NodeStmt = struct {
    id: u32,
    kind: StmtKind,
    symtable: *symb.SymbolTable,

    pub fn asNode(self: NodeStmt) Node {
        return Node{ .Stmt = self };
    }

    pub fn children(self: NodeStmt, allocator: std.mem.Allocator) ![]Node {
        var childrenArray = std.ArrayList(Node).init(allocator);
        defer childrenArray.deinit();
        switch (self.kind) {
            .exit => |exit| try childrenArray.append(exit.expr.asNode()),
            .defValue => |value| try childrenArray.append(value.expr.asNode()),
            .defVar => |variable| try childrenArray.append(variable.expr.asNode()),
            .assignVar => |assign| try childrenArray.append(assign.expr.asNode()),
            else => {},
        }
        return try childrenArray.toOwnedSlice();
    }
};

pub const Parser = struct {
    top: *symb.SymbolTable,
    id: u32,
    tokens: Iterator(Token),
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(NodeStmt),
    nextId: u32 = 1,

    fn reserveId(self: *Parser) u32 {
        defer self.nextId += 1;
        return self.nextId;
    }

    pub fn init(allocator: std.mem.Allocator, tokens: []Token, symbolTable: *symb.SymbolTable) Parser {
        return .{
            .top = symbolTable,
            .allocator = allocator,
            .tokens = Iterator(Token).init(tokens),
            .nodes = std.ArrayList(NodeStmt).init(allocator),
            .id = 0,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.nodes.deinit();
    }

    fn parseExpr(self: *Parser) !NodeExpr {
        const kind = try switch (self.tokens.peek().?) {
            .intLit => ExprKind{
                .intLit = NodeIntlit{
                    .intlit = (try self.tokens.consume(.intLit)).?,
                },
            },
            .ident => ExprKind{
                .ident = NodeIdent{
                    .ident = (try self.tokens.consume(.ident)).?,
                },
            },
            else => ParsingError.InvalidExpression,
        };
        return NodeExpr{
            .id = self.reserveId(),
            .kind = kind,
            .isConst = kind.isConstant(),
            .typ = null,
            .symtable = self.top,
        };
    }

    fn parseStmt(self: *Parser) !NodeStmt {
        return switch (self.tokens.peek().?) {
            .exit => try self.parseExit(),
            .constant => try self.parseConstant(),
            .variable => try self.parseVariable(),
            .ident => try self.parseAssign(),
            else => ParsingError.InvalidStatement,
        };
    }

    fn parseAssign(self: *Parser) !NodeStmt {
        const ident = (try self.tokens.consume(.ident)).?;
        _ = try self.tokens.consume(.equal);
        const expr = try self.parseExpr();
        _ = try self.tokens.consume(.semiCol);
        const kind = StmtKind{
            .assignVar = NodeAssign{
                .ident = ident,
                .expr = expr,
            },
        };
        return NodeStmt{
            .id = self.reserveId(),
            .kind = kind,
            .symtable = self.top,
        };
    }

    fn parseExit(self: *Parser) !NodeStmt {
        _ = try self.tokens.consume(.exit);
        const expr = try self.parseExpr();
        _ = try self.tokens.consume(.semiCol);
        const kind = StmtKind{ .exit = NodeExit{ .expr = expr } };
        return NodeStmt{
            .symtable = self.top,
            .kind = kind,
            .id = self.reserveId(),
        };
    }

    fn parseVariable(self: *Parser) !NodeStmt {
        _ = try self.tokens.consume(.variable);
        const ident = (try self.tokens.consume(.ident)).?;
        _ = try self.tokens.consume(.equal);
        const expr = try self.parseExpr();
        _ = try self.tokens.consume(.semiCol);
        const kind = StmtKind{
            .defVar = NodeVar{
                .ident = ident,
                .expr = expr,
            },
        };
        return NodeStmt{
            .id = self.reserveId(),
            .kind = kind,
            .symtable = self.top,
        };
    }

    fn parseConstant(self: *Parser) !NodeStmt {
        _ = try self.tokens.consume(.constant);
        const ident = (try self.tokens.consume(.ident)).?;
        _ = try self.tokens.consume(.equal);
        const expr = try self.parseExpr();
        _ = try self.tokens.consume(.semiCol);
        const kind = StmtKind{
            .defValue = NodeValue{
                .ident = ident,
                .expr = expr,
            },
        };
        return NodeStmt{
            .id = self.reserveId(),
            .kind = kind,
            .symtable = self.top,
        };
    }

    pub fn parse(self: *Parser) ![]const NodeStmt {
        while (self.tokens.peek()) |_|
            try self.nodes.append(try self.parseStmt());

        return self.nodes.items;
    }
};

pub const NodeAssign = struct {
    ident: Token,
    expr: NodeExpr,
};

pub const NodeValue = struct {
    ident: Token,
    expr: NodeExpr,
};

pub const NodeVar = struct {
    ident: Token,
    expr: NodeExpr,
};

pub const NodeExit = struct {
    expr: NodeExpr,
};

pub const NodeIntlit = struct {
    intlit: Token,
};

pub const NodeIdent = struct {
    ident: Token,
};

pub const StmtKind = union(enum) {
    exit: NodeExit,
    defValue: NodeValue,
    defVar: NodeVar,
    assignVar: NodeAssign,
};

pub const ExprKind = union(enum) {
    intLit: NodeIntlit,
    ident: NodeIdent,

    pub fn isConstant(self: ExprKind) bool {
        return switch (self) {
            .intLit => true,
            .ident => false,
        };
    }
};

test "Parser" {
    const expect = std.testing.expect;
    const src = "exit 120;";
    var tokenizer = tok.Tokenizer.init(std.testing.allocator, src);
    defer tokenizer.deinit();
    const toks = try tokenizer.tokenize();
    var parser = Parser.init(std.testing.allocator, toks);
    defer parser.deinit();
    const parseTree = try parser.parse();
    const exp: []const NodeStmt = &[_]NodeStmt{NodeStmt{
        .exit = NodeExit{
            .expr = NodeExpr{
                .intLit = NodeIntlit{
                    .intlit = Token{
                        .intLit = 120,
                    },
                },
            },
        },
    }};
    for (parseTree, exp) |stmt, expStmt|
        try expect(std.meta.eql(stmt, expStmt));
}
