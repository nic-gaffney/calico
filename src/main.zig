const std = @import("std");
const tok = @import("tokenize.zig");

const gftCompilerError = error{NoInputFile};

pub fn main() !void {
    if (std.os.argv.len < 2) return gftCompilerError.NoInputFile;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var args = std.process.args();
    _ = args.skip();
    const inputFileName = args.next();

    var out_name: []const u8 = "out";
    if (std.os.argv.len == 3) out_name = args.next().?;

    const inputFile = try std.fs.cwd().openFile(inputFileName.?, .{});
    defer inputFile.close();

    std.fs.cwd().makeDir("calico-out") catch |err|
        if (err != error.PathAlreadyExists) return err;

    // Setup native code writer
    const outFileName = try getFileName(gpa.allocator(), out_name, "asm");
    defer gpa.allocator().free(outFileName);
    const outfile = try std.fs.cwd().createFile(outFileName, .{});
    const outWriter = outfile.writer();
    defer outfile.close();

    // Turn the input file into a string
    const all = try inputFile.readToEndAlloc(gpa.allocator(), 2048);
    defer gpa.allocator().free(all);

    // Tokenize
    var tokenizer = tok.Tokenizer.init(gpa.allocator(), all);
    defer tokenizer.deinit();
    var tokIter = tok.Iterator(tok.Token).init((try tokenizer.tokenize()).items);

    // Parse tokens
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
            // No other commands
            else => {},
        }
    }

    // Run nasm and ld to build the executable
    // TODO: switch to qbe or llvm (preferabbly qbe)
    const nasmargv = [_][]const u8{ "nasm", "-felf64", outFileName };
    const nasmproc = try std.process.Child.run(.{ .argv = &nasmargv, .allocator = gpa.allocator() });
    defer gpa.allocator().free(nasmproc.stdout);
    defer gpa.allocator().free(nasmproc.stderr);

    const ldFile = try getFileName(gpa.allocator(), out_name, "o");
    defer gpa.allocator().free(ldFile);
    const binFile = try getFileName(gpa.allocator(), out_name, "");
    defer gpa.allocator().free(binFile);
    const ldargv = [_][]const u8{ "ld", "-o", binFile, ldFile };
    const ldproc = try std.process.Child.run(.{ .argv = &ldargv, .allocator = gpa.allocator() });
    defer gpa.allocator().free(ldproc.stdout);
    defer gpa.allocator().free(ldproc.stderr);
}

/// Get file extension based on filename
inline fn getFileName(allocator: std.mem.Allocator, out_name: []const u8, fileType: []const u8) ![]const u8 {
    var hasDot: []const u8 = ".";
    if (fileType.len == 0) hasDot = "";
    return try std.fmt.allocPrint(allocator, "calico-out/{s}{s}{s}", .{ out_name, hasDot, fileType });
}
