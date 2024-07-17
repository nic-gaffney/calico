const std = @import("std");

const TokenError = error{UnknownToken};
const TokenType = enum {
    ret,
    intLit,
    binaryOp,
    semiCol,
    nil,
};

pub const Token = union(TokenType) {
    ret: []const u8,
    intLit: i32,
    binaryOp: u8,
    semiCol,
    nil,
};

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
        pub fn peek(self: Iterator(typ)) ?typ {
            if (self.index >= self.items.len) return null;
            return self.items[self.index];
        }

        /// Get current item and iterate index
        pub fn consume(self: *Iterator(typ)) ?typ {
            defer self.index += 1;
            return self.peek();
        }
        /// Get current item and iterate index
        pub const next = consume;

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
        self.toks.deinit();
    }

    /// Returns an ArrayList of tokens
    pub fn tokenize(self: *Tokenizer) !std.ArrayList(Token) {
        var str = std.ArrayList(u8).init(self.allocator);
        defer str.deinit();

        while (self.src.peek()) |char| {
            switch (char) {
                ' ', '\n', '\t' => self.src.skip(),
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
                '+', '-', '*', '/' => try self.toks.append(.{ .binaryOp = self.src.consume().? }),
                else => {},
            }
        }
        return self.toks;
    }
};

test "Tokenize" {
    std.testing.log_level = std.log.Level.info;
    const expect = std.testing.expect;
    const testSource: []const u8 = "exit 120 + 150 - 260 * 12 / 5;";
    var toks = Tokenizer.init(std.testing.allocator, testSource);
    defer toks.deinit();
    const arrtoks = try toks.tokenize();
    const expected = &[_]Token{
        .{ .ret = "exit" },
        .{ .intLit = 120 },
        .{ .binaryOp = '+' },
        .{ .intLit = 150 },
        .{ .binaryOp = '-' },
        .{ .intLit = 260 },
        .{ .binaryOp = '*' },
        .{ .intLit = 12 },
        .{ .binaryOp = '/' },
        .{ .intLit = 5 },
        .semiCol,
    };
    for (arrtoks.items, expected) |act, exp| {
        switch (act) {
            .ret => |v| {
                try expect(std.mem.eql(u8, v, exp.ret));
                std.testing.allocator.free(v);
            },
            .intLit => |v| try expect(v == exp.intLit),
            .semiCol => |v| try expect(v == exp.semiCol),
            .binaryOp => |v| try expect(v == exp.binaryOp),
            else => {},
        }
    }
}
