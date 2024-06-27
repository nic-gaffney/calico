const std = @import("std");

const TokenError = error{UnknownToken};

const Token = union(enum) {
    ret: []const u8,
    intLit: i32,
    semiCol: u8,
    nil: void,
};

pub const TokenIterator = struct {
    tokens: []const Token,
    index: usize = 0,

    pub fn next(self: *TokenIterator) ?Token {
        defer self.*.index = self.*.index + 1;
        if (self.*.index >= self.*.tokens.len) return null;
        return self.*.tokens[self.*.index];
    }
};

pub fn tokenize(allocator: std.mem.Allocator, buff: []const u8) ![]const Token {
    var toks = std.ArrayList(Token).init(allocator);
    defer toks.deinit();
    var str = std.ArrayList(u8).init(allocator);
    defer str.deinit();

    var i: u32 = 0;
    while (i < buff.len) {
        switch (buff[i]) {
            ' ', '\n', '\t' => {
                i = i + 1;
                continue;
            },
            '0'...'9' => {
                while (std.ascii.isDigit(buff[i])) {
                    try str.append(buff[i]);
                    i = i + 1;
                }
                const num: i32 = try std.fmt.parseInt(i32, str.items, 10);
                try toks.append(.{ .intLit = num });
                str.deinit();
                str = std.ArrayList(u8).init(allocator);
            },
            'a'...'z', 'A'...'Z' => {
                while (std.ascii.isAlphanumeric(buff[i])) {
                    try str.append(buff[i]);
                    i = i + 1;
                }
                try toks.append(.{ .ret = try str.toOwnedSlice() });
                str.deinit();
                str = std.ArrayList(u8).init(allocator);
            },
            ';' => {
                i = i + 1;
                try toks.append(.{ .semiCol = ';' });
            },
            '+', '-', '*', '/' => {
                // Process operator
            },
            else => {},
        }
    }
    return toks.toOwnedSlice();
}
