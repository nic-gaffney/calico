const std = @import("std");

const TokenError = error{UnknownToken};

const Token = union(enum) {
    ret: []const u8,
    intLit: i32,
    semiCol,
    nil,
};

pub const TokenIterator = struct {
    tokens: std.ArrayList(Token),
    index: usize = 0,

    pub fn next(self: *TokenIterator) ?Token {
        defer self.index = self.index + 1;
        if (self.index >= self.tokens.items.len) return null;
        return self.tokens.items[self.index];
    }
};

pub const StringIterator = struct {
    string: []const u8,
    index: usize = 0,

    pub fn init(string: []const u8) StringIterator {
        return StringIterator{ .string = string };
    }

    pub fn peek(self: StringIterator) ?u8 {
        if (self.index >= self.string.len) return null;
        return self.string[self.index];
    }

    pub fn consume(self: *StringIterator) ?u8 {
        defer self.index += 1;
        return self.peek();
    }

    pub fn skip(self: *StringIterator) void {
        self.index += 1;
    }
};

pub const Tokenizer = struct {
    src: StringIterator,
    allocator: std.mem.Allocator,
    toks: std.ArrayList(Token),

    pub fn init(allocator: std.mem.Allocator, src: []const u8) Tokenizer {
        return Tokenizer{
            .src = StringIterator.init(src),
            .allocator = allocator,
            .toks = std.ArrayList(Token).init(allocator),
        };
    }

    pub fn deinit(self: *Tokenizer) void {
        self.toks.deinit();
    }

    pub fn tokenize(self: *Tokenizer) !std.ArrayList(Token) {
        var str = std.ArrayList(u8).init(self.allocator);
        defer str.deinit();

        while (self.src.peek()) |char| {
            switch (char) {
                ' ', '\n', '\t' => {
                    self.src.skip();
                    continue;
                },
                '0'...'9' => {
                    while (std.ascii.isDigit(self.src.peek().?))
                        try str.append(self.src.consume().?);

                    const num: i32 = try std.fmt.parseInt(i32, str.items, 10);
                    try self.toks.append(.{ .intLit = num });
                    str.deinit();
                    str = std.ArrayList(u8).init(self.allocator);
                },
                'a'...'z', 'A'...'Z' => {
                    while (std.ascii.isAlphanumeric(self.src.peek().?))
                        try str.append(self.src.consume().?);

                    try self.toks.append(.{ .ret = try str.toOwnedSlice() });
                    str.deinit();
                    str = std.ArrayList(u8).init(self.allocator);
                },
                ';' => {
                    self.src.skip();
                    try self.toks.append(.semiCol);
                },
                '+', '-', '*', '/' => {
                    // Process operator
                },
                else => {},
            }
        }
        return self.toks;
    }
};
