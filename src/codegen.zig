const std = @import("std");
const parse = @import("parser.zig");
const symb = @import("symtable.zig");
const llvm = @import("llvm");
const analysis = llvm.analysis;
const core = llvm.core;
const target = llvm.target;
const types = llvm.types;

const CodegenError = error{
    OutOfMemory,
};

fn toLLVMtype(typ: parse.TypeIdent, sym: *symb.SymbolTable) types.LLVMTypeRef {
    if (sym.getType(typ)) |t| {
        return switch (t) {
            .Integer => core.LLVMInt32Type(),
            .Void => core.LLVMVoidType(),
            else => core.LLVMVoidType(),
        };
    }
    return core.LLVMVoidType();
}

pub const Generator = struct {
    root: parse.NodeStmt,
    allocator: std.mem.Allocator,
    builder: types.LLVMBuilderRef,
    context: types.LLVMContextRef,
    module: types.LLVMModuleRef,
    currentFunc: ?types.LLVMValueRef,
    currentFuncIsVoid: bool,

    pub fn init(allocator: std.mem.Allocator, root: parse.NodeStmt) Generator {
        _ = target.LLVMInitializeNativeTarget();
        _ = target.LLVMInitializeNativeAsmPrinter();
        _ = target.LLVMInitializeNativeAsmParser();

        const context = core.LLVMContextCreate();
        const builder = core.LLVMCreateBuilderInContext(context);
        const module = core.LLVMModuleCreateWithNameInContext("_calico_start", context);

        return .{
            .root = root,
            .allocator = allocator,
            .builder = builder,
            .context = context,
            .module = module,
            .currentFunc = null,
            .currentFuncIsVoid = false,
        };
    }

    pub fn deinit(self: *Generator) void {
        // Shutdown LLVM
        defer core.LLVMShutdown();
        defer core.LLVMDisposeModule(self.module);
        defer core.LLVMDisposeBuilder(self.builder);

        //self.code.deinit();
    }

    fn genExit(self: *Generator, exit: parse.NodeExit) !void {
        const expr = exit;
        const val = core.LLVMConstInt(core.LLVMInt32Type(), switch (expr.kind) {
            .intLit => |i| @intCast(i.intLit),
            .ident => unreachable,
        }, 0);
        _ = core.LLVMBuildRet(self.builder, val);
    }

    fn genVar(self: *Generator, value: parse.NodeVar) !void {
        const str = try std.fmt.allocPrint(self.allocator,
            \\section .data
            \\  {s}: dw {d}
            \\
        , .{ value.ident.ident, switch (value.expr.kind) {
            .intLit => |intlit| intlit.intLit,
            else => return error.NotImplemented,
        } });
        defer self.allocator.free(str);
        try self.code.insertSlice(0, str);
    }

    fn genValue(self: *Generator, value: parse.NodeValue) !void {
        const str = try std.fmt.allocPrint(self.allocator,
            \\section .data
            \\  {s}: dw {d}
            \\
        , .{ value.ident.ident, switch (value.expr.kind) {
            .intLit => |intlit| intlit.intLit,
            else => return error.NotImplemented,
        } });
        defer self.allocator.free(str);
        try self.code.insertSlice(0, str);
    }

    fn genAssign(self: *Generator, assign: parse.NodeAssign) !void {
        const newCode =
            switch (assign.expr.kind) {
            .intLit => |intlit| try std.fmt.allocPrint(self.allocator,
                \\  mov rax, {d}
                \\  mov [{s}], rax
                \\
            , .{
                intlit.intLit,
                assign.ident.ident,
            }),
            .ident => |ident| try std.fmt.allocPrint(self.allocator,
                \\  mov rax, [{s}]
                \\  mov [{s}], rax
                \\
            , .{
                ident.ident,
                assign.ident.ident,
            }),
        };
        try self.code.appendSlice(newCode);
        self.allocator.free(newCode);
    }

    fn genBlock(self: *Generator, block: []const parse.NodeStmt) CodegenError!void {
        for (block) |stmt| try self.genStmt(stmt);
    }

    fn genFunc(self: *Generator, stmt: parse.NodeStmt) !void {
        const fun = stmt.kind.function;
        const table = stmt.symtable;
        const block = fun.block;
        const codeSlice = block.kind.block;
        const funcName: [*:0]const u8 = try self.allocator.dupeZ(u8, fun.ident.ident);

        const retType = toLLVMtype(fun.retType.?, table);
        var params = [0]types.LLVMTypeRef{};
        const funcType = core.LLVMFunctionType(retType, @ptrCast(&params), 0, 0);
        const func = core.LLVMAddFunction(self.module, funcName, funcType);
        self.currentFunc = func;
        self.currentFuncIsVoid = switch (table.getType(fun.retType.?).?) {
            .Void => true,
            else => false,
        };

        const function: types.LLVMValueRef = self.currentFunc.?;
        const codeBlock = core.LLVMAppendBasicBlockInContext(self.context, function, "entry");
        core.LLVMPositionBuilderAtEnd(self.builder, codeBlock);
        const bodyTable = block.symtable;
        _ = bodyTable;
        //TODO: codegen for args

        try self.genBlock(codeSlice);
        _ = if (self.currentFuncIsVoid) core.LLVMBuildRetVoid(self.builder);
    }

    fn genStmt(self: *Generator, stmt: parse.NodeStmt) !void {
        try switch (stmt.kind) {
            .exit => |expr| self.genExit(expr),
            .function => self.genFunc(stmt),
            else => {},
        };
    }

    pub fn generate(self: *Generator) ![]const u8 {
        try switch (self.root.kind) {
            .block => |b| {
                for (b) |stmt|
                    try self.genStmt(stmt);
            },
            else => error.InvalidTop,
        };
        const string: []const u8 = std.mem.span(core.LLVMPrintModuleToString(self.module));
        return string;
    }
};

test "Codegen exit" {
    const tok = @import("tokenize.zig");
    const expect = std.testing.expect;
    const main = @import("main.zig");

    const src =
        \\fn main() -> i32 {
        \\    return 7;
        \\}
    ;
    const expected =
        \\; ModuleID = '_calico_start'
        \\source_filename = "_calico_start"
        \\
        \\define i32 @main() {
        \\entry:
        \\  ret i32 7
        \\}
        \\
    ;
    var tokenizer = tok.Tokenizer.init(std.testing.allocator, src);
    defer tokenizer.deinit();
    const toks = try tokenizer.tokenize();
    var symbTable: *symb.SymbolTable = try main.initSymbolTable(std.testing.allocator);
    defer symbTable.deinit();
    var parser = parse.Parser.init(std.testing.allocator, toks, symbTable);
    defer parser.deinit();
    const parseTree = try parser.parse();
    var pop = symb.Populator.init(std.testing.allocator);
    var treeNode = parseTree.asNode();
    try pop.populateSymtable(&treeNode);
    var gen = Generator.init(std.testing.allocator, parseTree);
    defer gen.deinit();
    const actual = try gen.generate();
    try expect(std.mem.eql(u8, actual, expected));
}
