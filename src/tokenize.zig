const std = @import("std");

pub const TokenizeError = error{
    UnknownToken,
    UnexpectedEOF,
    ExpectedToken,
};

pub const TokenType = enum {
    // Runtime Values
    ident,
    intLit,
    // Keywords
    constant,
    variable,
    exit,
    fun,
    // Operators
    plus,
    minus,
    star,
    slash,
    semiCol,
    equal,
    // Symbols
    openBrace,
    closeBrace,
    openParen,
    closeParen,
    arrow,
};

pub const Token = union(TokenType) {
    //RuntimeVar
    ident: []const u8,
    intLit: i32,
    // Keywords
    constant,
    variable,
    exit,
    fun,
    // Operators
    plus,
    minus,
    star,
    slash,
    semiCol,
    equal,
    // Symbols
    openBrace,
    closeBrace,
    openParen,
    closeParen,
    arrow,

    pub fn fromChar(char: u8) !Token {
        return switch (char) {
            '+' => .plus,
            '-' => .minus,
            '*' => .star,
            '/' => .slash,
            ';' => .semiCol,
            '=' => .equal,
            '{' => .openBrace,
            '}' => .closeBrace,
            '(' => .openParen,
            ')' => .closeParen,
            else => TokenizeError.UnknownToken,
        };
    }

    pub fn fromStr(str: []const u8) Token {
        const eql = std.mem.eql;
        if (eql(u8, str, "return")) return .exit;
        if (eql(u8, str, "const")) return .constant;
        if (eql(u8, str, "var")) return .variable;
        if (eql(u8, str, "fn")) return .fun;
        return Token{ .ident = str };
    }
};

pub fn checkType(tok: Token, comptime typ: TokenType) bool {
    return switch (tok) {
        typ => true,
        else => false,
    };
}

/// Creates a tokenizer over a slice of typ
pub fn Iterator(comptime typ: type) type {
    return struct {
        items: []const typ,
        index: usize = 0,

        /// Initialize tokenizer with a slice
        pub fn init(items: []const typ) Iterator(typ) {
            return Iterator(typ){ .items = items };
        }

        /// Get current item
        pub fn peekAhead(self: Iterator(typ), ahead: u32) ?typ {
            if (self.index + ahead >= self.items.len) return null;
            return self.items[self.index + ahead];
        }

        pub fn peek(self: Iterator(typ)) ?typ {
            return peekAhead(self, 0);
        }

        /// Get current item and iterate index
        pub fn next(self: *Iterator(typ)) ?typ {
            const ret = self.peek();
            self.index += 1;
            return ret;
        }

        pub fn consume(self: *Iterator(typ), comptime expected: TokenType) !?typ {
            if (typ != Token) return error.TokenIteratorOnly;
            if (!checkType(self.peek().?, expected))
                return TokenizeError.ExpectedToken;
            return self.next();
        }

        /// Skip over current item
        pub fn skip(self: *Iterator(typ)) void {
            self.index += 1;
        }
    };
}

/// Tokenizes a string of source code
pub const Tokenizer = struct {
    src: Iterator(u8),
    allocator: std.mem.Allocator,
    toks: std.ArrayList(Token),

    /// Initializes a string of source code
    /// Deinitialize with Tokenizer.deinit()
    pub fn init(allocator: std.mem.Allocator, src: []const u8) Tokenizer {
        return Tokenizer{
            .src = Iterator(u8).init(src),
            .allocator = allocator,
            .toks = std.ArrayList(Token).init(allocator),
        };
    }

    /// Releases allocated memory
    pub fn deinit(self: *Tokenizer) void {
        for (self.toks.items) |token| {
            if (checkType(token, TokenType.ident))
                self.allocator.free(token.ident);
        }
        self.toks.deinit();
    }

    /// Returns an ArrayList of tokens
    pub fn tokenize(self: *Tokenizer) ![]Token {
        var buff = std.ArrayList(u8).init(self.allocator);
        defer buff.deinit();

        while (self.src.peek()) |char| {
            try switch (char) {
                '-' => {
                    self.src.skip();
                    if (self.src.peek().? != '>') {
                        try self.toks.append(.minus);
                        continue;
                    }
                    self.src.skip();
                    try self.toks.append(.arrow);
                },
                ' ', '\n', '\t' => self.src.skip(),
                '0'...'9' => {
                    while (std.ascii.isDigit(self.src.peek().?))
                        try buff.append(self.src.next().?);

                    const num: i32 = try std.fmt.parseInt(i32, buff.items, 10);
                    try self.toks.append(.{ .intLit = num });
                    buff.clearAndFree();
                },
                'a'...'z', 'A'...'Z' => {
                    while (std.ascii.isAlphanumeric(self.src.peek().?))
                        try buff.append(self.src.next().?);
                    const str = try buff.toOwnedSlice();
                    const token = Token.fromStr(str);
                    try self.toks.append(token);
                    if (!checkType(token, TokenType.ident)) self.allocator.free(str);
                    buff.clearAndFree();
                },
                else => self.toks.append(try Token.fromChar(self.src.next().?)),
            };
        }
        return self.toks.items;
    }
};

test "Tokenize Expression" {
    const expect = std.testing.expect;
    const testSource: []const u8 = "return 120 + 150 - 260 * 12 / 5 + variable;";
    var tokenizer = Tokenizer.init(std.testing.allocator, testSource);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();
    const expected = &[_]Token{
        .exit,
        .{ .intLit = 120 },
        .plus,
        .{ .intLit = 150 },
        .minus,
        .{ .intLit = 260 },
        .star,
        .{ .intLit = 12 },
        .slash,
        .{ .intLit = 5 },
        .plus,
        .{ .ident = "variable" },
        .semiCol,
    };
    for (tokens, expected) |act, exp| {
        switch (act) {
            .exit => |v| try expect(v == exp.exit),
            .intLit => |v| try expect(v == exp.intLit),
            .semiCol => |v| try expect(v == exp.semiCol),
            .plus => |v| try expect(v == exp.plus),
            .minus => |v| try expect(v == exp.minus),
            .star => |v| try expect(v == exp.star),
            .slash => |v| try expect(v == exp.slash),
            .ident => |v| try expect(std.mem.eql(u8, v, exp.ident)),
            else => try expect(1 == 0),
        }
    }
}

test "Tokenize variable" {
    const expect = std.testing.expect;
    const testSource: []const u8 = "var five = 5;";
    var tokenizer = Tokenizer.init(std.testing.allocator, testSource);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();
    const expected = &[_]Token{
        .variable,
        .{ .ident = "five" },
        .equal,
        .{ .intLit = 5 },
        .semiCol,
    };
    for (tokens, expected) |act, exp| {
        switch (act) {
            .variable => |v| try expect(v == exp.variable),
            .ident => |v| try expect(std.mem.eql(u8, exp.ident, v)),
            .equal => |v| try expect(v == exp.equal),
            .intLit => |v| try expect(v == exp.intLit),
            .semiCol => |v| try expect(v == exp.semiCol),
            else => try expect(1 == 0),
        }
    }
}

test "Tokenize constant" {
    const expect = std.testing.expect;
    const testSource: []const u8 = "const five = 5;";
    var tokenizer = Tokenizer.init(std.testing.allocator, testSource);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();
    const expected = &[_]Token{
        .constant,
        .{ .ident = "five" },
        .equal,
        .{ .intLit = 5 },
        .semiCol,
    };
    for (tokens, expected) |act, exp| {
        switch (act) {
            .constant => |v| try expect(v == exp.constant),
            .ident => |v| try expect(std.mem.eql(u8, exp.ident, v)),
            .equal => |v| try expect(v == exp.equal),
            .intLit => |v| try expect(v == exp.intLit),
            .semiCol => |v| try expect(v == exp.semiCol),
            else => try expect(1 == 0),
        }
    }
}

test "Tokenize Function" {
    const expect = std.testing.expect;
    const testSource: []const u8 =
        \\fn main() -> i32 {
        \\  return 7;
        \\}
    ;
    var tokenizer = Tokenizer.init(std.testing.allocator, testSource);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();
    const expected = &[_]Token{
        .fun,
        .{ .ident = "main" },
        .openParen,
        .closeParen,
        .arrow,
        .{ .ident = "i32" },
        .openBrace,
        .exit,
        .{ .intLit = 7 },
        .semiCol,
        .closeBrace,
    };
    for (tokens, expected) |act, exp| {
        switch (act) {
            .ident => |v| try expect(std.mem.eql(u8, exp.ident, v)),
            .fun => |v| try expect(v == exp.fun),
            .arrow => |v| try expect(v == exp.arrow),
            .intLit => |v| try expect(v == exp.intLit),
            .exit => |v| try expect(v == exp.exit),
            .closeParen => |v| try expect(v == exp.closeParen),
            .openParen => |v| try expect(v == exp.openParen),
            .openBrace => |v| try expect(v == exp.openBrace),
            .closeBrace => |v| try expect(v == exp.closeBrace),
            .semiCol => |v| try expect(v == exp.semiCol),
            else => try expect(1 == 0),
        }
    }
}
