const std = @import("std");
const tok = @import("tokenize.zig");
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

pub const NodeExpr = union(enum) {
    intLit: NodeIntlit,
    ident: NodeIdent,
};

pub const NodeStmt = union(enum) {
    exit: NodeExit,
    value: NodeValue,
    assign: NodeAssign,
};

pub const NodeAssign = struct {
    ident: Token,
    value: NodeExpr,
};

pub const NodeValue = struct {
    ident: Token,
    value: NodeExpr,
    isConst: bool,
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

pub const Parser = struct {
    tokens: Iterator(Token),
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(NodeStmt),

    pub fn init(allocator: std.mem.Allocator, tokens: []Token) Parser {
        return .{
            .allocator = allocator,
            .tokens = Iterator(Token).init(tokens),
            .nodes = std.ArrayList(NodeStmt).init(allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.nodes.deinit();
    }

    fn parseExpr(self: *Parser) !NodeExpr {
        return switch (self.tokens.peek().?) {
            .intLit => NodeExpr{
                .intLit = NodeIntlit{
                    .intlit = (try self.tokens.consume(.intLit)).?,
                },
            },
            .ident => NodeExpr{
                .ident = NodeIdent{
                    .ident = (try self.tokens.consume(.ident)).?,
                },
            },
            else => ParsingError.InvalidExpression,
        };
    }

    fn parseStmt(self: *Parser) !NodeStmt {
        return switch (self.tokens.peek().?) {
            .exit => NodeStmt{ .exit = try self.parseExit() },
            .constant => NodeStmt{ .value = try self.parseValue(true) },
            .variable => NodeStmt{ .value = try self.parseValue(false) },
            .ident => NodeStmt{ .assign = try self.parseAssign() },
            else => ParsingError.InvalidStatement,
        };
    }

    fn parseAssign(self: *Parser) !NodeAssign {
        const ident = try self.tokens.consume(.ident);
        var isMutable = false;
        var exists = false;
        for (self.nodes.items) |item| {
            switch (item) {
                .value => |v| {
                    if (std.mem.eql(u8, v.ident.ident, ident.?.ident)) {
                        isMutable = !v.isConst;
                        exists = true;
                    }
                },
                else => {},
            }
        }
        if (!exists) return error.UnknownIdentifier;
        if (!isMutable) return error.ImmutableValue;
        _ = try self.tokens.consume(.equal);
        const expr = try self.parseExpr();
        _ = try self.tokens.consume(.semiCol);
        return NodeAssign{
            .ident = ident.?,
            .value = expr,
        };
    }

    fn parseExit(self: *Parser) !NodeExit {
        _ = try self.tokens.consume(.exit);
        const expr = try self.parseExpr();
        _ = try self.tokens.consume(.semiCol);
        return NodeExit{
            .expr = expr,
        };
    }

    fn parseValue(self: *Parser, isConst: bool) !NodeValue {
        self.tokens.skip();
        const ident = (try self.tokens.consume(.ident)).?;
        _ = try self.tokens.consume(.equal);
        const expr = try self.parseExpr();
        _ = try self.tokens.consume(.semiCol);
        return NodeValue{
            .ident = ident,
            .value = expr,
            .isConst = isConst,
        };
    }

    pub fn parse(self: *Parser) ![]const NodeStmt {
        while (self.tokens.peek()) |_|
            try self.nodes.append(try self.parseStmt());

        return self.nodes.items;
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
