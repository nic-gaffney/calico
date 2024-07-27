const std = @import("std");
const tok = @import("tokenize.zig");
const Iterator = tok.Iterator;
const Token = tok.Token;

const ParsingError = error{ InvalidExpression, ExpectedExit, ExpectedSemicolon };

pub const NodeExpr = struct {
    intLit: Token,
};

pub const NodeExit = struct {
    expr: NodeExpr,
};

pub const Parser = struct {
    tokens: Iterator(Token),

    pub fn init(tokens: []Token) Parser {
        return .{
            .tokens = Iterator(Token).init(tokens),
        };
    }

    fn parseExpr(self: *Parser) !NodeExpr {
        if (tok.checkType(self.tokens.peek().?, tok.TokenType.intLit))
            return NodeExpr{ .intLit = self.tokens.consume().? };
        return ParsingError.InvalidExpression;
    }

    pub fn parse(self: *Parser) !NodeExit {
        var root: NodeExit = undefined;
        while (self.tokens.peek()) |token| {
            switch (token) {
                .exit => {
                    self.tokens.skip();
                    root.expr = try self.parseExpr();
                    if (!tok.checkType(self.tokens.peek().?, tok.TokenType.semiCol))
                        return ParsingError.ExpectedSemicolon;
                    self.tokens.skip();
                },
                else => return ParsingError.ExpectedExit,
            }
        }
        return root;
    }
};

test "Parser" {
    const expect = std.testing.expect;
    const src = "exit 120;";
    var tokenizer = tok.Tokenizer.init(std.testing.allocator, src);
    defer tokenizer.deinit();
    const toks = try tokenizer.tokenize();
    var parser = Parser.init(toks);
    const parseTree = try parser.parse();
    const exp = NodeExit{ .expr = NodeExpr{ .intLit = Token{ .intLit = 120 } } };
    try expect(std.meta.eql(parseTree, exp));
}
