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
    IncorrectType,
    UnknownIdentifier,
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
        // const ptr = try self.genAlloc(toLLVMtype(nodeVar.expr.typ.?, table).?, nodeVar.ident.ident);
        const ptr = try self.genAlloc(
            toLLVMtype(
                nodeVar.expr.typ orelse try nodeVar.expr.symtable.getValue(nodeVar.ident.ident).?.typ.toTypeIdent(self.allocator),
                table,
                nodeVar.expr,
            ).?,
            nodeVar.ident.ident,
        );
        _ = core.LLVMBuildStore(self.builder, value, ptr);
        try self.references.put(symbol.id, ptr);
    }

    fn genValue(self: *Generator, stmt: parse.NodeStmt) !void {
        const nodeVar = stmt.kind.defValue;

        const table = stmt.symtable;
        const symbol = table.getValue(nodeVar.ident.ident).?;
        // const ptr = try self.genAlloc(toLLVMtype(nodeVar.expr.typ.?, table), nodeVar.ident.ident);
        const ptr = try self.genAlloc(toLLVMtype(nodeVar.expr.typ orelse try nodeVar.expr.inferType(self.allocator, table), table, nodeVar.expr), nodeVar.ident.ident);
        const value = try self.genExpr(nodeVar.expr);
        _ = core.LLVMBuildStore(self.builder, value, ptr);
        std.debug.print("\t\t\tGenerated Value {s}\n", .{nodeVar.ident.ident});
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

    fn asBasicType(typ: symb.SymbType) ?types.LLVMTypeRef {
        return switch (typ) {
            .Integer => core.LLVMInt32Type(),
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

    fn genIf(self: *Generator, stmt: parse.NodeStmt) CodegenError!void {
        const ifkind = stmt.kind.ifstmt;
        const block = ifkind.body;
        const expr = ifkind.expr;

        const condition = try self.genExpr(expr);
        const func = self.currentFunc.?;

        const then = core.LLVMAppendBasicBlock(func, "then");
        const elsebb = core.LLVMAppendBasicBlock(func, "elsebb");
        const cont = core.LLVMAppendBasicBlock(func, "continue");

        _ = core.LLVMBuildCondBr(self.builder, condition, then, elsebb);

        _ = core.LLVMPositionBuilderAtEnd(self.builder, then);
        try self.genStmt(block.*);
        _ = core.LLVMBuildBr(self.builder, cont);

        _ = core.LLVMPositionBuilderAtEnd(self.builder, elsebb);
        _ = core.LLVMBuildBr(self.builder, cont);

        _ = core.LLVMPositionBuilderAtEnd(self.builder, cont);
    }

    fn genStmt(self: *Generator, stmt: parse.NodeStmt) !void {
        std.debug.print("======\n\tStmt: {any}\n======\n", .{stmt.kind});
        try switch (stmt.kind) {
            .exit => |expr| self.genExit(expr),
            .function => self.genFunc(stmt),
            .defValue => self.genValue(stmt),
            .defVar => self.genVar(stmt),
            .assignVar => self.genAssign(stmt),
            .ifstmt => self.genIf(stmt),
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

                break :blk core.LLVMBuildLoad2(
                    self.builder,
                    toLLVMtype(
                        expr.typ orelse try table.getValue(id.ident).?.typ.toTypeIdent(self.allocator),
                        table,
                        expr,
                    ),
                    ptr,
                    "",
                );
            },
            .intLit => |int| core.LLVMConstInt(core.LLVMInt32TypeInContext(self.context), @intCast(int.intLit), 1),
            .binaryOp => |exp| blk: {
                const lhs = try self.genExpr(exp.lhs.*);
                const rhs = try self.genExpr(exp.rhs.*);
                std.debug.print("\n\n\tLHS: {any}\n\tRHS: {any}\n\tOP: {any}\n\n", .{ lhs, rhs, exp.op });

                break :blk switch (exp.op) {
                    .plus => core.LLVMBuildAdd(self.builder, lhs, rhs, "add"),
                    .minus => core.LLVMBuildSub(self.builder, lhs, rhs, "sub"),
                    .star => core.LLVMBuildMul(self.builder, lhs, rhs, "mul"),
                    .slash => core.LLVMBuildSDiv(self.builder, lhs, rhs, "div"),
                    .eqleql => core.LLVMBuildICmp(self.builder, types.LLVMIntPredicate.LLVMIntEQ, lhs, rhs, "eql"),
                    else => core.LLVMBuildICmp(self.builder, types.LLVMIntPredicate.LLVMIntEQ, lhs, rhs, "eql"),
                };
            },
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
                const functype = expr.symtable.getValue(call.ident.ident).?.typ.Function;
                const ident = try self.allocator.dupeZ(u8, call.ident.ident);
                const function = core.LLVMGetNamedFunction(self.module, ident);
                var args = std.ArrayList(types.LLVMValueRef).init(self.allocator);
                for (call.args.items, functype.input) |arg, intype| {
                    if (!std.meta.eql(expr.symtable.getType(arg.typ.?).?, intype)) return CodegenError.IncorrectType;
                    try args.append(try self.genExpr(arg));
                }
                const funcType = core.LLVMGlobalGetValueType(function);
                // std.debug.print("FUNCTYPE: {s}\n", .{call.ident.ident});

                const llvmCall = core.LLVMBuildCall2(
                    self.builder,
                    funcType,
                    function,
                    @ptrCast(args.items),
                    @intCast(call.args.items.len),
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
                for (b) |stmt| {
                    try self.genStmt(stmt);
                }
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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tokenizer = tok.Tokenizer.init(allocator, src);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();
    const symbTable = try main.initSymbolTable(arena.allocator());
    var parser = parse.Parser.init(arena.allocator(), tokens, symbTable);
    const tree = try parser.parse();
    var treeNode = tree.asNode();
    var pop = symb.Populator.init(arena.allocator());
    try pop.populateSymtable(&treeNode);
    var generator = Generator.init(arena.allocator(), tree, "_calico_start");
    defer generator.deinit();
    const code = try generator.generate();
    try expect(std.mem.eql(u8, code, expected));
}

test "Codegen assign" {
    const tok = @import("tokenize.zig");
    const expect = std.testing.expect;
    const main = @import("main.zig");

    const src =
        \\fn main() -> i32 {
        \\    const testval = 6;
        \\    varbl: i32 testvar = testval;
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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tokenizer = tok.Tokenizer.init(allocator, src);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();
    const symbTable = try main.initSymbolTable(arena.allocator());
    var parser = parse.Parser.init(arena.allocator(), tokens, symbTable);
    const tree = try parser.parse();
    var treeNode = tree.asNode();
    var pop = symb.Populator.init(arena.allocator());
    try pop.populateSymtable(&treeNode);
    var generator = Generator.init(arena.allocator(), tree, "_calico_start");
    defer generator.deinit();
    const code = try generator.generate();
    try expect(std.mem.eql(u8, code, expected));
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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tokenizer = tok.Tokenizer.init(allocator, src);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();
    const symbTable = try main.initSymbolTable(arena.allocator());
    var parser = parse.Parser.init(arena.allocator(), tokens, symbTable);
    const tree = try parser.parse();
    var treeNode = tree.asNode();
    var pop = symb.Populator.init(arena.allocator());
    try pop.populateSymtable(&treeNode);
    var generator = Generator.init(arena.allocator(), tree, "_calico_start");
    defer generator.deinit();
    const code = generator.generate();
    try std.testing.expectError(CodegenError.Immutable, code);
}
test "Codegen extern fn string" {
    const tok = @import("tokenize.zig");
    const expect = std.testing.expect;
    const main = @import("main.zig");

    const src =
        \\import fn puts(str: [u8]) -> i32;
        \\fn main() -> i32 {
        \\    puts("Hello World!");
        \\}
    ;
    const expected =
        \\; ModuleID = '_calico_start'
        \\source_filename = "_calico_start"
        \\
        \\@.str.0 = private unnamed_addr constant [13 x i8] c"Hello World!\00"
        \\
        \\declare i32 @puts(ptr)
        \\
        \\define i32 @main() {
        \\entry:
        \\  %puts = call i32 @puts(ptr @.str.0)
        \\}
        \\
    ;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tokenizer = tok.Tokenizer.init(allocator, src);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();
    const symbTable = try main.initSymbolTable(arena.allocator());
    var parser = parse.Parser.init(arena.allocator(), tokens, symbTable);
    const tree = try parser.parse();
    var treeNode = tree.asNode();
    var pop = symb.Populator.init(arena.allocator());
    try pop.populateSymtable(&treeNode);
    var generator = Generator.init(arena.allocator(), tree, "_calico_start");
    defer generator.deinit();
    const code = try generator.generate();
    try expect(std.mem.eql(u8, code, expected));
}
