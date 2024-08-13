const std = @import("std");
const tok = @import("tokenize.zig");
const parse = @import("parser.zig");
const gen = @import("codegen.zig");
const symb = @import("symtable.zig");

pub fn main() !void {
    if (std.os.argv.len < 2) {
        std.debug.print(
            \\info: Usage: calico [input file]
            \\
        , .{});
        return;
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
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
    const outFileName = try getFileName(allocator, out_name, "ll");
    defer allocator.free(outFileName);
    const outfile = try std.fs.cwd().createFile(outFileName, .{});
    const outWriter = outfile.writer();
    defer outfile.close();

    // Turn the input file into a string
    const all = try inputFile.readToEndAlloc(allocator, 2048);
    defer allocator.free(all);

    // Tokenize
    var tokenizer = tok.Tokenizer.init(allocator, all);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();

    // Parse
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const symbTable = try initSymbolTable(arena.allocator());

    var parser = parse.Parser.init(arena.allocator(), tokens, symbTable);
    const tree = try parser.parse();
    var treeNode = tree.asNode();
    var pop = symb.Populator.init(arena.allocator());
    try pop.populateSymtable(&treeNode);
    // var iter = symbTable.scope.?.symbs.iterator();
    // while (iter.next()) |entry| {
    // std.debug.print("{s} -> {any}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    // }

    // Codegen
    const fname = try allocator.dupeZ(u8, inputFileName.?);
    defer allocator.free(fname);
    var generator = gen.Generator.init(arena.allocator(), tree, @ptrCast(fname));
    defer generator.deinit();
    const code = try generator.generate();
    // std.debug.print("{s}\n", .{code});
    try outWriter.writeAll(code);

    const binFile = try getFileName(allocator, out_name, "");
    defer allocator.free(binFile);
    const ldargv = [_][]const u8{ "clang", "-o", binFile, outFileName };
    const ldproc = try std.process.Child.run(.{ .argv = &ldargv, .allocator = allocator });
    defer allocator.free(ldproc.stdout);
    defer allocator.free(ldproc.stderr);
}

/// Get file extension based on filename
inline fn getFileName(allocator: std.mem.Allocator, out_name: []const u8, fileType: []const u8) ![]const u8 {
    var hasDot: []const u8 = ".";
    if (fileType.len == 0) hasDot = "";
    return try std.fmt.allocPrint(allocator, "calico-out/{s}{s}{s}", .{ out_name, hasDot, fileType });
}

pub fn initSymbolTable(allocator: std.mem.Allocator) !*symb.SymbolTable {
    var table = try symb.SymbolTable.init(allocator);
    const intSymb: symb.SymbType = symb.SymbType.Integer;
    const charSymb: symb.SymbType = symb.SymbType.Character;
    const strSymb: symb.SymbType = symb.SymbType.String;
    if (!try table.insert("i32", intSymb.toSymb())) return error.FailedToInsert;
    if (!try table.insert("u8", charSymb.toSymb())) return error.FailedToInsert;
    if (!try table.insert("[u8]", strSymb.toSymb())) return error.FailedToInsert;
    return table;
}
