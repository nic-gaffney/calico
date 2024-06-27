const std = @import("std");
const tok = @import("tokenize.zig");

const gftCompilerError = error{NoInputFile};

pub fn main() !void {
    if (std.os.argv.len != 2) return gftCompilerError.NoInputFile;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var args = std.process.args();
    _ = args.skip();
    const inputFileName = args.next();
    const inputFile = try std.fs.cwd().openFile(inputFileName.?, .{});
    defer inputFile.close();

    std.fs.cwd().makeDir("out") catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };
    const outfile = try std.fs.cwd().createFile("out/out.asm", .{});
    const outWriter = outfile.writer();
    defer outfile.close();

    // Logic here to compile language
    const all = try inputFile.readToEndAlloc(gpa.allocator(), 2048);
    defer gpa.allocator().free(all);

    const toks = try tok.tokenize(gpa.allocator(), all);
    defer gpa.allocator().free(toks);
    var tokIter = tok.TokenIterator{ .tokens = toks };
    try outWriter.print("global _start:\n", .{});
    while (tokIter.next()) |t| {
        switch (t) {
            .ret => {
                const num = tokIter.next();
                switch (num.?) {
                    .intLit => {},
                    else => break,
                }
                switch (tokIter.next().?) {
                    .semiCol => {},
                    else => break,
                }
                try outWriter.print(
                    \\  mov rax, 60
                    \\  mov rdi, {}
                    \\  syscall
                    \\
                , .{num.?.intLit});
                gpa.allocator().free(t.ret);
            },
            else => {},
        }
    }

    const nasmargv = [_][]const u8{ "nasm", "-felf64", "out/out.asm" };
    const nasmproc = try std.ChildProcess.run(.{ .argv = &nasmargv, .allocator = gpa.allocator() });
    defer gpa.allocator().free(nasmproc.stdout);
    defer gpa.allocator().free(nasmproc.stderr);

    const ldargv = [_][]const u8{ "ld", "-o", "out/out", "out/out.o" };
    const ldproc = try std.ChildProcess.run(.{ .argv = &ldargv, .allocator = gpa.allocator() });
    defer gpa.allocator().free(ldproc.stdout);
    defer gpa.allocator().free(ldproc.stderr);
}
