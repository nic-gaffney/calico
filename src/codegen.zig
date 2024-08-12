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

fn toLLVMtype(typ: parse.TypeIdent, sym: *symb.SymbolTable, expr: ?parse.NodeExpr) types.LLVMTypeRef {
    _ = expr;
    if (sym.getType(typ)) |t| {
        return switch (t) {
            .Integer => core.LLVMInt32Type(),
            .String => core.LLVMPointerType(core.LLVMInt8Type(), 0),
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
    stringId: u32,

    pub fn init(allocator: std.mem.Allocator, root: parse.NodeStmt, filename: [*:0]const u8) Generator {
        _ = target.LLVMInitializeNativeTarget();
        _ = target.LLVMInitializeNativeAsmPrinter();
        _ = target.LLVMInitializeNativeAsmParser();

        const context = core.LLVMContextCreate();
        const builder = core.LLVMCreateBuilderInContext(context);
        const module = core.LLVMModuleCreateWithNameInContext(filename, context);

        return .{
            .root = root,
            .allocator = allocator,
            .builder = builder,
            .context = context,
            .module = module,
            .currentFunc = null,
            .currentFuncIsVoid = false,
            .references = std.AutoHashMap(u32, types.LLVMValueRef).init(allocator),
            .stringId = 0,
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
        const val = try self.genExpr(expr);
        _ = core.LLVMBuildRet(self.builder, val);
    }

    fn genVar(self: *Generator, stmt: parse.NodeStmt) !void {
        const nodeVar = stmt.kind.defVar;

        const table = stmt.symtable;
        const symbol = table.getValue(nodeVar.ident.ident).?;
        const value = try self.genExpr(nodeVar.expr);
        const ptr = try self.genAlloc(toLLVMtype(nodeVar.expr.typ.?, table, nodeVar.expr).?, nodeVar.ident.ident);
        _ = core.LLVMBuildStore(self.builder, value, ptr);
        try self.references.put(symbol.id, ptr);
    }

    fn genValue(self: *Generator, stmt: parse.NodeStmt) !void {
        const nodeVar = stmt.kind.defValue;

        const table = stmt.symtable;
        const symbol = table.getValue(nodeVar.ident.ident).?;
        const ptr = try self.genAlloc(toLLVMtype(nodeVar.expr.typ.?, table, nodeVar.expr), nodeVar.ident.ident);
        const value = try self.genExpr(nodeVar.expr);
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
        // std.debug.print("assign\n", .{});
        const table = stmt.symtable;
        const symbol = table.get(stmt.kind.assignVar.ident.ident).?;
        if (!symbol.Value.mut) return CodegenError.Immutable;
        const ptr = self.references.get(symbol.Value.id).?;
        const value = try self.genExpr(stmt.kind.assignVar.expr);
        _ = core.LLVMBuildStore(self.builder, value, ptr);
    }

    fn genBlock(self: *Generator, block: []const parse.NodeStmt) CodegenError!void {
        for (block) |stmt| try self.genStmt(stmt);
    }

    fn genFunc(self: *Generator, stmt: parse.NodeStmt) !void {
        self.references.clearAndFree();
        const fun = stmt.kind.function;
        var table: *symb.SymbolTable = stmt.symtable;
        var block: *parse.NodeStmt = undefined;
        var codeSlice: []const parse.NodeStmt = undefined;
        if (fun.block != null) {
            table = fun.block.?.symtable;
            block = fun.block.?;
            codeSlice = block.kind.block;
        }
        const funcName: [*:0]const u8 = try self.allocator.dupeZ(u8, fun.ident.ident);

        const retType = toLLVMtype(fun.retType.?, table, null);
        var params = std.ArrayList(types.LLVMTypeRef).init(self.allocator);
        for (fun.args) |arg| {
            try params.append(toLLVMtype(arg.typ, table, null));
        }

        const funcType = core.LLVMFunctionType(retType, @ptrCast(params.items), @intCast(params.items.len), 0);
        const func = core.LLVMAddFunction(self.module, funcName, funcType);
        for (fun.args, 0..) |arg, i| {
            const symbol = table.get(arg.ident).?;
            const ptr: types.LLVMValueRef = core.LLVMGetParam(func, @intCast(i));
            try self.references.put(symbol.Value.id, ptr);
        }

        if (fun.block != null) {
            self.currentFunc = func;
            self.currentFuncIsVoid = switch (table.getType(fun.retType.?).?) {
                .Void => true,
                else => false,
            };

            const function: types.LLVMValueRef = func;
            const codeBlock = core.LLVMAppendBasicBlockInContext(self.context, function, "entry");
            core.LLVMPositionBuilderAtEnd(self.builder, codeBlock);
            const bodyTable = block.symtable;
            _ = bodyTable;

            try self.genBlock(codeSlice);
            _ = if (self.currentFuncIsVoid) core.LLVMBuildRetVoid(self.builder);
        }
    }

    fn genStmt(self: *Generator, stmt: parse.NodeStmt) !void {
        try switch (stmt.kind) {
            .exit => |expr| self.genExit(expr),
            .function => self.genFunc(stmt),
            .defValue => self.genValue(stmt),
            .defVar => self.genVar(stmt),
            .assignVar => self.genAssign(stmt),
            .expr => |expression| {
                _ = try self.genExpr(expression);
            },

            else => {},
        };
    }

    fn genExpr(self: *Generator, expr: parse.NodeExpr) !types.LLVMValueRef {
        return switch (expr.kind) {
            .ident => |id| blk: {
                // std.debug.print("getValue({s})\n", .{id.ident});
                const table = expr.symtable;

                // std.debug.print("\n\nEXPERTABLE\n\n", .{});
                // var iterTable = table.scope.?.symbs.iterator();
                // while (iterTable.next()) |entry| {
                //     // std.debug.print("{s} -> {any}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
                // }
                // std.debug.print("\n\nEXPERTABLE\n\n", .{});
                const symbol = table.getValue(id.ident).?;
                const ptr = self.references.get(symbol.id).?;
                if (core.LLVMIsAArgument(ptr)) |_|
                    break :blk ptr;

                break :blk core.LLVMBuildLoad2(self.builder, toLLVMtype(expr.typ.?, table, expr), ptr, "");
            },
            .intLit => |int| core.LLVMConstInt(core.LLVMInt32TypeInContext(self.context), @intCast(int.intLit), 1),
            .stringLit => |str| blk: {
                const vref = core.LLVMAddGlobal(
                    self.module,
                    core.LLVMArrayType(core.LLVMInt8Type(), @intCast(str.stringLit.len + 1)),
                    try self.allocator.dupeZ(u8, try std.fmt.allocPrint(
                        self.allocator,
                        ".str.{d}",
                        .{self.stringId},
                    )),
                );
                self.stringId += 1;
                const sref = core.LLVMConstString(try self.allocator.dupeZ(u8, str.stringLit), @intCast(str.stringLit.len), 0);
                core.LLVMSetInitializer(vref, sref);
                core.LLVMSetGlobalConstant(vref, 1);
                core.LLVMSetLinkage(vref, .LLVMPrivateLinkage);
                core.LLVMSetUnnamedAddr(vref, 1);
                break :blk vref;
            },

            .call => |call| blk: {
                const ident = try self.allocator.dupeZ(u8, call.ident.ident);
                const function = core.LLVMGetNamedFunction(self.module, ident);
                var args = std.ArrayList(types.LLVMValueRef).init(self.allocator);
                for (call.args) |arg|
                    try args.append(try self.genExpr(arg));
                const funcType = core.LLVMGlobalGetValueType(function);
                // std.debug.print("FUNCTYPE: {s}\n", .{call.ident.ident});

                const llvmCall = core.LLVMBuildCall2(
                    self.builder,
                    funcType,
                    function,
                    @ptrCast(args.items),
                    @intCast(call.args.len),
                    ident,
                );
                // std.debug.print("CALL\n", .{});

                break :blk llvmCall;
            },
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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var gen = Generator.init(arena.allocator(), parseTree);
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
