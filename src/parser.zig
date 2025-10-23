const std = @import("std");
const tok = @import("tokenize.zig");
const symb = @import("symtable.zig");
const util = @import("utils.zig");
const Iterator = tok.Iterator;
const Token = tok.Token;

const ParsingError = error{
    InvalidExpression,
    ExpectedExit,
    ExpectedSemicolon,
    ExpectedEqual,
    ExpectedIdentifier,
    InvalidStatement,
    UnknownIdentifier,
    UnknownToken,
    UnexpectedEOF,
    ExpectedToken,
    OutOfMemory,
    TokenIteratorOnly,
    TypeRequiredForVarbl,
};

const TypeError = error{
    OutOfMemory,
    IncorrectType,
    UnknownIdentifier,
};

fn errcast(err: anytype) ParsingError {
    return err[0];
}

pub const Node = union(enum) {
    Expr: NodeExpr,
    Stmt: NodeStmt,
    pub fn children(self: Node, allocator: std.mem.Allocator) ![]Node {
        var childrenArray = std.ArrayList(Node){};
        defer childrenArray.deinit(allocator);
        switch (self) {
            .Expr => |expr| try childrenArray.appendSlice(allocator, try expr.children(allocator)),
            .Stmt => |stmt| try childrenArray.appendSlice(allocator, try stmt.children(allocator)),
        }
        return try childrenArray.toOwnedSlice(allocator);
    }
};

pub const NodeExpr = struct {
    id: u32,
    line: u32,
    kind: ExprKind,
    symtable: *symb.SymbolTable,
    typ: ?TypeIdent,
    isConst: bool,

    pub fn asNode(self: NodeExpr) Node {
        return Node{ .Expr = self };
    }

    pub fn children(self: NodeExpr, allocator: std.mem.Allocator) ![]Node {
        var childrenArray = std.ArrayList(Node){};
        defer childrenArray.deinit(allocator);
        switch (self.kind) {
            else => {},
        }
        return try childrenArray.toOwnedSlice(allocator);
    }

    pub fn inferType(self: NodeExpr, allocator: std.mem.Allocator, table: *symb.SymbolTable) TypeError!TypeIdent {
        const expectedType = try switch (self.kind) {
            .call => |call| if (table.getValue(call.ident.ident)) |symbol| try symbol.typ.Function.output.toTypeIdent(allocator) else TypeError.UnknownIdentifier,
            .ident => |id| if (table.getValue(id.ident)) |symbol| try symbol.typ.toTypeIdent(allocator) else TypeError.UnknownIdentifier,
            .intLit => TypeIdent{ .ident = "i32", .list = false },
            .binaryOp => TypeIdent{ .ident = "i32", .list = false },
            .stringLit => TypeIdent{ .ident = "[u8]", .list = true },
        };
        if (self.typ) |typ| {
            return if (std.mem.eql(u8, typ.ident, expectedType.ident)) expectedType else blk: {
                util.comptimeError(.{
                    .line = self.line,
                    .err = TypeError.IncorrectType,
                    .exp = expectedType.ident,
                    .got = typ.ident,
                });
                break :blk TypeError.IncorrectType;
            };
        } else return expectedType;
    }
};

pub const NodeStmt = struct {
    id: u32,
    line: u32,
    kind: StmtKind,
    symtable: *symb.SymbolTable,

    pub fn asNode(self: NodeStmt) Node {
        return Node{ .Stmt = self };
    }

    pub fn children(self: NodeStmt, allocator: std.mem.Allocator) ![]Node {
        var childrenArray = std.ArrayList(Node){};
        defer childrenArray.deinit(allocator);
        switch (self.kind) {
            .exit => |exit| try childrenArray.append(allocator, exit.asNode()),
            .defValue => |value| try childrenArray.append(allocator, value.expr.asNode()),
            .defVar => |variable| try childrenArray.append(allocator, variable.expr.asNode()),
            .assignVar => |assign| try childrenArray.append(allocator, assign.expr.asNode()),
            .block => |block| {
                const blockChildren = util.map(Node, NodeStmt, block, NodeStmt.asNode);
                for (blockChildren) |child| try childrenArray.append(allocator, child);
            },
            .ifstmt => |ifstmt| try childrenArray.append(allocator, ifstmt.body.*.asNode()),
            .whileStmt => |whilestmt| try childrenArray.append(allocator, whilestmt.body.*.asNode()),
            .function => |fun| if (fun.block == null) {} else try childrenArray.append(allocator, fun.block.?.asNode()),
            .expr => |expr| try childrenArray.append(allocator, expr.asNode()),
        }
        return try childrenArray.toOwnedSlice(allocator);
    }
};

pub const Parser = struct {
    top: *symb.SymbolTable,
    id: u32,
    tokens: Iterator(Token),
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(NodeStmt),
    nextId: u32 = 1,
    line: u32 = 1,

    fn reserveId(self: *Parser) u32 {
        defer self.nextId += 1;
        return self.nextId;
    }

    pub fn init(allocator: std.mem.Allocator, tokens: []Token, symbolTable: *symb.SymbolTable) Parser {
        return .{
            .top = symbolTable,
            .allocator = allocator,
            .tokens = Iterator(Token).init(tokens),
            .nodes = std.ArrayList(NodeStmt){},
            .id = 0,
        };
    }

    fn dinitHelper(self: *Parser, node: NodeStmt) !void {
        switch (node.kind) {
            .block => |blk| {
                const children = try node.children(self.allocator);
                defer self.allocator.free(children);
                for (children) |child| try self.dinitHelper(child.Stmt);
                self.allocator.free(blk);
                node.symtable.deinit();
            },
            .function => |fun| {
                const children = try node.children(self.allocator);
                defer self.allocator.free(children);
                for (children) |child| try self.dinitHelper(child.Stmt);
                self.allocator.free(fun.args);
                if (fun.block != null)
                    self.allocator.destroy(fun.block.?);
            },
            else => {},
        }
    }

    pub fn deinit(self: *Parser) void {
        for (self.nodes.items) |node| {
            self.dinitHelper(node) catch |err| {
                if (err == error.OutOfMemory) {}
            };
        }
        self.nodes.deinit();
    }

    fn parseCall(self: *Parser, typ: ?TypeIdent, expr: NodeExpr) ParsingError!NodeExpr {
        if (self.tokens.peek().? != .openParen) return expr;
        _ = try self.tokens.consume(.openParen);
        var args = std.ArrayList(NodeExpr){};
        while (self.tokens.peek().? != .closeParen) {
            try args.append(self.allocator, try self.parseExpr());
            if (self.tokens.peek().? != .comma) break;
        }
        _ = try self.tokens.consume(.closeParen);

        const kind = ExprKind{ .call = .{ .ident = expr.kind.ident, .args = args } };
        return NodeExpr{
            .kind = kind,
            .isConst = false,
            .line = self.tokens.line,
            .typ = typ,
            .id = self.reserveId(),
            .symtable = self.top,
        };
    }

    fn parseExpr(self: *Parser) !NodeExpr {
        var typ: ?TypeIdent = null;
        var isIdent = false;
        const kind: ExprKind = try blk: {
            try switch (self.tokens.peek().?) {
                .intLit => {
                    typ = TypeIdent{
                        .ident = "i32",
                        .list = false,
                    };
                    break :blk ExprKind{ .intLit = (try self.tokens.consume(.intLit)).? };
                },
                .ident => {
                    isIdent = true;
                    const ident = (try self.tokens.consume(.ident)).?;
                    if (tok.checkType(self.tokens.peek().?, .openParen)) {
                        _ = try self.tokens.consume(.openParen);

                        break :blk ExprKind{
                            .call = .{
                                .ident = ident,
                                .args = innerblk: {
                                    var argExprs = std.ArrayList(NodeExpr){};
                                    while (!tok.checkType(self.tokens.peek().?, .closeParen)) {
                                        try argExprs.append(self.allocator, try self.parseExpr());
                                        if (tok.checkType(self.tokens.peek().?, .closeParen)) break;
                                        _ = try self.tokens.consume(.comma);
                                    }
                                    _ = try self.tokens.consume(.closeParen);

                                    break :innerblk try argExprs.clone(self.allocator);
                                },
                            },
                        };
                    }
                    typ = null;
                    break :blk ExprKind{ .ident = ident };
                },
                .stringLit => {
                    typ = TypeIdent{
                        .ident = "[u8]",
                        .list = true,
                    };
                    break :blk ExprKind{ .stringLit = (try self.tokens.consume(.stringLit)).? };
                },
                else => break :blk errorblk: {
                    break :errorblk ParsingError.InvalidExpression;
                },
            };
        };

        const lhs = NodeExpr{
            .id = self.reserveId(),
            .line = self.tokens.line,
            .isConst = false,
            .kind = kind,
            .symtable = self.top,
            .typ = typ,
        };

        const lhsptr = try self.allocator.create(NodeExpr);
        lhsptr.* = lhs;

        const precTable: []const []const tok.TokenType = &.{
            &.{ .plus, .minus },
            &.{ .star, .slash },
            &.{ .eqleql, .lessthan, .greaterthan },
        };

        return try self.genBinOp(precTable[0], typ, lhsptr) //.
        orelse try self.genBinOp(precTable[1], typ, lhsptr) //.
        orelse try self.genBinOp(precTable[2], typ, lhsptr) //.
        orelse if (isIdent) blk: {
            break :blk try self.parseCall(typ, lhsptr.*);
        } //.
        else lhs;
    }

    fn genBinOp(self: *Parser, comptime ops: []const tok.TokenType, typ: ?TypeIdent, lhs: *NodeExpr) ParsingError!?NodeExpr {
        for (ops) |op|
            if (self.tokens.peek().? == op) {
                const oper = self.tokens.next();
                const rhsptr = try self.allocator.create(NodeExpr);
                rhsptr.* = try self.parseExpr();
                const kind = ExprKind{
                    .binaryOp = .{
                        .lhs = lhs,
                        .rhs = rhsptr,
                        .op = oper.?,
                    },
                };
                return NodeExpr{
                    .id = self.reserveId(),
                    .line = self.tokens.line,
                    .isConst = false,
                    .kind = kind,
                    .symtable = self.top,
                    .typ = typ,
                };
            };
        return null;
    }

    fn parseStmt(self: *Parser) ParsingError!NodeStmt {
        return switch (self.tokens.peek().?) {
            .ifstmt => try self.parseIf(),
            .whilestmt => try self.parseWhile(),
            .exit => try self.parseExit(),
            .constant => try self.parseConstant(),
            .variable => try self.parseVariable(),
            .ident => blk: {
                if (!tok.checkType(self.tokens.peekAhead(1).?, .openParen))
                    break :blk try self.parseAssign();
                break :blk try self.parseExprStmt();
            },
            .openBrace => self.parseBlock(false),
            .fun => try self.parseFunc(false),
            .import => try self.parseFunc(true),
            else => self.parseExprStmt(),
        };
    }

    fn parseExprStmt(self: *Parser) ParsingError!NodeStmt {
        const kind: StmtKind = StmtKind{ .expr = try self.parseExpr() };
        _ = try self.tokens.consume(.semiCol);
        return NodeStmt{
            .id = self.reserveId(),
            .line = self.tokens.line,
            .symtable = self.top,
            .kind = kind,
        };
    }

    fn parseWhile(self: *Parser) ParsingError!NodeStmt {
        _ = try self.tokens.consume(.whilestmt); // if

        _ = try self.tokens.consume(.openParen); // (
        const expr = try self.parseExpr(); // EXPR
        _ = try self.tokens.consume(.closeParen); // )

        const body = try self.allocator.create(NodeStmt);
        body.* = try self.parseStmt();

        const kind = StmtKind{
            .whileStmt = .{
                .expr = expr,
                .body = body,
            },
        };
        return NodeStmt{
            .line = self.tokens.line,
            .id = self.reserveId(),
            .kind = kind,
            .symtable = self.top,
        };
    }

    fn parseIf(self: *Parser) ParsingError!NodeStmt {
        _ = try self.tokens.consume(.ifstmt); // if

        _ = try self.tokens.consume(.openParen); // (
        const expr = try self.parseExpr(); // EXPR
        _ = try self.tokens.consume(.closeParen); // )

        const body = try self.allocator.create(NodeStmt);
        body.* = try self.parseStmt();

        const kind = StmtKind{
            .ifstmt = .{
                .expr = expr,
                .body = body,
            },
        };
        return NodeStmt{
            .line = self.tokens.line,
            .id = self.reserveId(),
            .kind = kind,
            .symtable = self.top,
        };
    }

    fn parseFunc(self: *Parser, external: bool) ParsingError!NodeStmt {
        if (external) _ = try self.tokens.consume(.import);
        var returns = false;
        var typ: ?TypeIdent = null;
        _ = try self.tokens.consume(.fun);
        const ident = (try self.tokens.consume(.ident)).?;
        _ = try self.tokens.consume(.openParen);
        var args = std.ArrayList(FunctionArg){};
        defer args.deinit(self.allocator);
        while (!tok.checkType(self.tokens.peek().?, .closeParen)) {
            const argIdent: Token = (try self.tokens.consume(.ident)).?;
            _ = try self.tokens.consume(.colon);
            const argTypIdent: TypeIdent = try self.parseType();
            const funcArg: FunctionArg = .{
                .ident = argIdent.ident,
                .typ = argTypIdent,
            };
            try args.append(self.allocator, funcArg);
            if (!tok.checkType(self.tokens.peek().?, .comma)) break;
            _ = try self.tokens.consume(.comma);
        }
        _ = try self.tokens.consume(.closeParen);
        if (tok.checkType(self.tokens.peek().?, .arrow)) {
            returns = true;
            self.tokens.skip();
            typ = try self.parseType();
        }

        if (external) {
            _ = try self.tokens.consume(.semiCol);
            return NodeStmt{
                .line = self.tokens.line,
                .id = self.reserveId(),
                .kind = StmtKind{
                    .function = .{
                        .ident = ident,
                        .args = try args.toOwnedSlice(self.allocator),
                        .retType = typ,
                        .block = null,
                    },
                },
                .symtable = self.top,
            };
        }

        const block = try self.allocator.create(NodeStmt);
        block.* = try self.parseBlock(returns);

        const kind = StmtKind{
            .function = .{
                .ident = ident,
                .args = try args.toOwnedSlice(self.allocator),
                .retType = typ,
                .block = block,
            },
        };

        return NodeStmt{
            .id = self.reserveId(),
            .line = self.tokens.line,
            .kind = kind,
            .symtable = self.top,
        };
    }

    fn parseBlock(self: *Parser, returns: bool) ParsingError!NodeStmt {
        _ = try self.tokens.consume(.openBrace);
        var stmtArr = std.ArrayList(NodeStmt){};
        const child = try self.top.makeChild();
        self.top = child;
        while (!tok.checkType(self.tokens.peek().?, .closeBrace))
            try stmtArr.append(self.allocator, try self.parseStmt());
        self.top = self.top.parent().?;
        _ = try self.tokens.consume(.closeBrace);
        if (returns) {
            if (blk: {
                for (stmtArr.items) |it| {
                    if (switch (it.kind) {
                        .exit => true,
                        else => false,
                    }) break :blk true;
                }
                break :blk false;
            }) {} else return ParsingError.ExpectedExit;
        }
        const kind = StmtKind{
            .block = try stmtArr.toOwnedSlice(self.allocator),
        };

        return NodeStmt{
            .line = self.tokens.line,
            .id = self.reserveId(),
            .kind = kind,
            .symtable = child,
        };
    }

    fn parseAssign(self: *Parser) ParsingError!NodeStmt {
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
            .line = self.tokens.line,
            .id = self.reserveId(),
            .kind = kind,
            .symtable = self.top,
        };
    }

    fn parseExit(self: *Parser) ParsingError!NodeStmt {
        _ = self.tokens.consume(.exit) catch |err| return errcast(.{err});
        const expr = try self.parseExpr();
        _ = try self.tokens.consume(.semiCol);
        const kind = StmtKind{ .exit = expr };
        return NodeStmt{
            .line = self.tokens.line,
            .symtable = self.top,
            .kind = kind,
            .id = self.reserveId(),
        };
    }

    fn parseVariable(self: *Parser) ParsingError!NodeStmt {
        _ = try self.tokens.consume(.variable);
        var typ: ?TypeIdent = null;
        _ = self.tokens.consume(.colon) catch {
            return ParsingError.TypeRequiredForVarbl;
        };
        typ = .{
            .ident = (try self.tokens.consume(.ident)).?.ident,
            .list = false,
        };
        const ident = (try self.tokens.consume(.ident)).?;
        _ = try self.tokens.consume(.equal);
        const expr = try self.parseExpr();
        _ = try self.tokens.consume(.semiCol);
        const kind = StmtKind{
            .defVar = NodeVar{
                .ident = ident,
                .expr = NodeExpr{
                    .line = self.tokens.line,
                    .typ = typ,
                    .id = expr.id,
                    .kind = expr.kind,
                    .isConst = expr.isConst,
                    .symtable = expr.symtable,
                },
            },
        };
        return NodeStmt{
            .line = self.tokens.line,
            .id = self.reserveId(),
            .kind = kind,
            .symtable = self.top,
        };
    }

    fn parseType(self: *Parser) ParsingError!TypeIdent {
        const list = tok.checkType(self.tokens.peekAhead(0).?, .openBracket);
        if (list) {
            _ = try self.tokens.consume(.openBracket);
            const typ = (try self.parseType()).ident;
            _ = try self.tokens.consume(.closeBracket);
            return .{
                .ident = try std.fmt.allocPrint(self.allocator, "[{s}]", .{typ}),
                .list = true,
            };
        }
        return .{
            .ident = (try self.tokens.consume(.ident)).?.ident,
            .list = false,
        };
    }

    fn parseConstant(self: *Parser) ParsingError!NodeStmt {
        _ = try self.tokens.consume(.constant);
        var typ: ?TypeIdent = null;
        std.debug.print("{any}\n", .{self.tokens.peek().?});
        _ = if (tok.checkType(self.tokens.peek().?, .colon)) {
            _ = try self.tokens.consume(.colon);
            typ = try self.parseType();
        };
        //     else {
        //     // return;
        //     // if (err != tok.TokenizeError.ExpectedToken) return err;
        // };

        const ident = (try self.tokens.consume(.ident)).?;
        _ = try self.tokens.consume(.equal);
        const expr: NodeExpr = try self.parseExpr();
        _ = try self.tokens.consume(.semiCol);
        const kind = StmtKind{
            .defValue = NodeValue{
                .ident = ident,
                .expr = NodeExpr{
                    .line = self.tokens.line,
                    .typ = typ orelse expr.typ,
                    .id = expr.id,
                    .kind = expr.kind,
                    .isConst = expr.isConst,
                    .symtable = expr.symtable,
                },
            },
        };
        return NodeStmt{
            .line = self.tokens.line,
            .id = self.reserveId(),
            .kind = kind,
            .symtable = self.top,
        };
    }

    pub fn parse(self: *Parser) !NodeStmt {
        while (self.tokens.peek()) |_|
            try self.nodes.append(self.allocator, try self.parseStmt());

        return NodeStmt{
            .line = self.tokens.line,
            .id = self.reserveId(),
            .kind = StmtKind{ .block = self.nodes.items },
            .symtable = self.top,
        };
    }
};

pub const TypeIdent = struct {
    ident: []const u8,
    list: bool,
};

pub const FunctionArg = struct {
    ident: []const u8,
    typ: TypeIdent,
};

pub const NodeFunction = struct {
    ident: Token,
    args: []const FunctionArg,
    retType: ?TypeIdent,
    block: ?*NodeStmt,
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

pub const NodeIf = struct {
    expr: NodeExpr,
    body: *NodeStmt,
};

pub const NodeWhile = struct {
    expr: NodeExpr,
    body: *NodeStmt,
};

pub const NodeExit = NodeExpr;
pub const NodeIntlit = Token;
pub const NodeStringlit = Token;
pub const NodeIdent = Token;
pub const NodeBlock = []const NodeStmt;
pub const NodeBinOp = Token;

pub const NodeCall = struct {
    ident: Token,
    args: std.ArrayList(NodeExpr),
};

pub const StmtKind = union(enum) {
    ifstmt: NodeIf,
    whileStmt: NodeWhile,
    function: NodeFunction,
    exit: NodeExit,
    defValue: NodeValue,
    defVar: NodeVar,
    assignVar: NodeAssign,
    block: NodeBlock,
    expr: NodeExpr,
};

pub const ExprKind = union(enum) {
    intLit: NodeIntlit,
    stringLit: NodeStringlit,
    ident: NodeIdent,
    call: NodeCall,
    binaryOp: struct {
        lhs: *NodeExpr,
        rhs: *NodeExpr,
        op: NodeBinOp,
    },

    pub fn isConstant(self: ExprKind) bool {
        return switch (self) {
            .intLit => true,
            .stringLit => true,
            .ident => false,
            .call => false,
        };
    }
};

test "Parser" {
    const main = @import("main.zig");
    const expect = std.testing.expect;
    const src = "return 120;";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tokenizer = tok.Tokenizer.init(allocator, src);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();
    const symbTable = try main.initSymbolTable(arena.allocator());
    var parser = Parser.init(arena.allocator(), tokens, symbTable);
    const tree = try parser.parse();
    var treeNode = tree.asNode();
    var pop = symb.Populator.init(arena.allocator());
    try pop.populateSymtable(&treeNode);
    const children = try treeNode.children(allocator);
    const exp: []const Node = &[_]Node{Node{
        .Stmt = NodeStmt{
            .id = 2,
            .symtable = symbTable,
            .line = 0,
            .kind = StmtKind{
                .exit = NodeExpr{
                    .line = 0,
                    .id = 1,
                    .kind = ExprKind{
                        .intLit = Token{ .intLit = 120 },
                    },
                    .symtable = symbTable,
                    .typ = TypeIdent{ .list = false, .ident = "i32" },
                    .isConst = false,
                },
            },
        },
    }};
    for (children, exp) |stmt, expStmt|
        try expect(std.meta.eql(stmt, expStmt));
}
