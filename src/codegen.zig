const std = @import("std");
const parse = @import("parser.zig");
const symb = @import("symtable.zig");
const llvm = @import("llvm");
const analysis = llvm.analysis;
const core = llvm.core;
const target = llvm.target;
const types = llvm.types;

const CodegenError = error{
    Immutable,
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
    references: std.AutoHashMap(u32, types.LLVMValueRef),

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
            .references = std.AutoHashMap(u32, types.LLVMValueRef).init(allocator),
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
        const val = self.genExpr(expr);
        _ = core.LLVMBuildRet(self.builder, val);
    }

    fn genVar(self: *Generator, stmt: parse.NodeStmt) !void {
        const nodeVar = stmt.kind.defVar;

        const table = stmt.symtable;
        const symbol = table.getValue(nodeVar.ident.ident).?;
        const value = self.genExpr(nodeVar.expr);
        const ptr = try self.genAlloc(toLLVMtype(nodeVar.expr.typ.?, table).?, nodeVar.ident.ident);
        _ = core.LLVMBuildStore(self.builder, value, ptr);
        try self.references.put(symbol.id, ptr);
    }

    fn genValue(self: *Generator, stmt: parse.NodeStmt) !void {
        const nodeVar = stmt.kind.defValue;

        const table = stmt.symtable;
        const symbol = table.getValue(nodeVar.ident.ident).?;
        const ptr = try self.genAlloc(toLLVMtype(nodeVar.expr.typ.?, table), nodeVar.ident.ident);
        const value = self.genExpr(nodeVar.expr);
        _ = core.LLVMBuildStore(self.builder, value, ptr);
        try self.references.put(symbol.id, ptr);
    }

    fn genAlloc(self: *Generator, typ: types.LLVMTypeRef, ident: []const u8) !types.LLVMValueRef {
        const builder = core.LLVMCreateBuilderInContext(self.context);

        const entryFunc = self.currentFunc.?;
        const entry = core.LLVMGetFirstBasicBlock(entryFunc).?;

        if (core.LLVMGetFirstInstruction(entry)) |first| {
            core.LLVMPositionBuilderBefore(builder, first);
        } else {
            core.LLVMPositionBuilderAtEnd(builder, entry);
        }

        const str: [*:0]const u8 = try self.allocator.dupeZ(u8, ident);
        return core.LLVMBuildAlloca(builder, typ, str);
    }

    fn asBasicType(typ: symb.SymbType) ?types.LLVMTypeKind {
        return switch (typ) {
            .Integer => types.LLVMTypeKind.LLVMIntegerTypeKind,
            else => null,
        };
    }

    fn genAssign(self: *Generator, stmt: parse.NodeStmt) !void {
        std.debug.print("assign\n", .{});
        const table = stmt.symtable;
        const symbol = table.get(stmt.kind.assignVar.ident.ident).?;
        if (!symbol.Value.mut) return CodegenError.Immutable;
        const ptr = self.references.get(symbol.Value.id).?;
        const value = self.genExpr(stmt.kind.assignVar.expr);
        _ = core.LLVMBuildStore(self.builder, value, ptr);
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
            .defValue => self.genValue(stmt),
            .defVar => self.genVar(stmt),
            .assignVar => self.genAssign(stmt),
            else => {},
        };
    }

    fn genExpr(self: *Generator, expr: parse.NodeExpr) types.LLVMValueRef {
        return switch (expr.kind) {
            .ident => blk: {
                const table = expr.symtable;
                const symbol = table.getValue(expr.kind.ident.ident).?;
                const ptr = self.references.get(symbol.id).?;
                break :blk core.LLVMBuildLoad2(self.builder, toLLVMtype(expr.typ.?, table), ptr, "");
            },
            .intLit => |int| core.LLVMConstInt(core.LLVMInt32TypeInContext(self.context), @intCast(int.intLit), 1),
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

test "Codegen assign" {
    const tok = @import("tokenize.zig");
    const expect = std.testing.expect;
    const main = @import("main.zig");

    const src =
        \\fn main() -> i32 {
        \\    const testval = 6;
        \\    var testvar = testval;
        \\    testvar = 5;
        \\    return testvar;
        \\}
    ;
    const expected =
        \\; ModuleID = '_calico_start'
        \\source_filename = "_calico_start"
        \\
        \\define i32 @main() {
        \\entry:
        \\  %testvar = alloca i32, align 4
        \\  %testval = alloca i32, align 4
        \\  store i32 6, ptr %testval, align 4
        \\  %0 = load i32, ptr %testval, align 4
        \\  store i32 %0, ptr %testvar, align 4
        \\  store i32 5, ptr %testvar, align 4
        \\  %1 = load i32, ptr %testvar, align 4
        \\  ret i32 %1
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

test "Codegen assign constant" {
    const tok = @import("tokenize.zig");
    const main = @import("main.zig");

    const src =
        \\fn main() -> i32 {
        \\    const testval = 6;
        \\    const testvar = testval;
        \\    testvar = 5;
        \\    return testvar;
        \\}
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
    const actual = gen.generate();
    try std.testing.expectError(CodegenError.Immutable, actual);
}
