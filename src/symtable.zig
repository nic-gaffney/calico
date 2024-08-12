const std = @import("std");
const pars = @import("parser.zig");

const Scope = struct {
    par: ?*Scope,
    symbs: std.StringHashMap(Symbol),
};

pub const Symbol = union(enum) {
    Type: SymbType,
    Value: SymbValue,
};

pub const SymbType = union(enum) {
    Void,
    Integer,
    Character,
    String,
    Function: struct {
        input: []const SymbType,
        output: *SymbType,
    },
    pub fn toSymb(self: SymbType) Symbol {
        return Symbol{ .Type = self };
    }
    pub fn toString(self: SymbType) []const u8 {
        return switch (self) {
            .Integer => "i32",
            .Character => "u8",
            else => "void",
        };
    }
};

pub const SymbValue = struct {
    typ: SymbType,
    id: u32,
    mut: bool,
    pub fn toSymb(self: SymbValue) Symbol {
        return Symbol{ .Value = self };
    }
};

pub const SymbolTable = struct {
    par: ?*SymbolTable,
    scope: ?*Scope = null,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*SymbolTable {
        const scope = try allocator.create(Scope);
        scope.par = null;
        scope.symbs = std.StringHashMap(Symbol).init(allocator);
        const table = try allocator.create(SymbolTable);
        table.* = SymbolTable{
            .par = null,
            .scope = scope,
            .allocator = allocator,
        };
        return table;
    }

    pub fn deinit(self: *SymbolTable) void {
        if (self.scope) |scope| {
            var iter = scope.symbs.iterator();
            while (iter.next()) |entry| {
                switch (entry.value_ptr.*) {
                    .Type => |t| switch (t) {
                        .Function => |f| {
                            self.allocator.destroy(f.output);
                            self.allocator.free(f.input);
                        },
                        else => {},
                    },
                    else => {},
                }
            }
            scope.symbs.deinit();
            self.allocator.destroy(scope);
        }
        self.allocator.destroy(self);
    }

    pub fn makeChild(self: *SymbolTable) !*SymbolTable {
        const scope = try self.allocator.create(Scope);
        scope.par = self.scope;
        scope.symbs = try self.scope.?.symbs.clone();
        const stable: *SymbolTable = try self.allocator.create(SymbolTable);
        stable.* = .{
            .par = self,
            .scope = scope,
            .allocator = self.allocator,
        };
        return stable;
    }

    pub fn parent(self: SymbolTable) ?*SymbolTable {
        if (self.par) |par|
            return par;
        return null;
    }

    pub fn contains(self: SymbolTable, ident: []const u8) bool {
        if (self.scope) |scope| return scope.symbs.contains(ident);
        return false;
    }

    pub fn get(self: SymbolTable, ident: []const u8) ?Symbol {
        if (self.scope) |scope| return scope.symbs.get(ident);
        return null;
    }

    pub fn getValue(self: SymbolTable, ident: []const u8) ?SymbValue {
        if (self.get(ident)) |symbol|
            return switch (symbol) {
                .Value => |value| value,
                else => null,
            };

        return null;
    }

    pub fn getMut(self: *SymbolTable, ident: []const u8) ?Symbol {
        if (self.get(ident)) |symbol|
            return switch (symbol) {
                .Value => null,
                else => symbol,
            };
        return null;
    }

    pub fn getType(self: *SymbolTable, typ: pars.TypeIdent) ?SymbType {
        if (self.get(typ.ident)) |symb| return symb.Type;
        return null;
    }

    pub fn insert(self: *SymbolTable, ident: []const u8, symbol: Symbol) !bool {
        // std.debug.print("Inserted {s} as {any}\n", .{ ident, symbol });
        if (self.scope) |scope| {
            if (scope.symbs.getEntry(ident)) |_| return false;
            try scope.symbs.put(ident, symbol);
            return true;
        }
        return false;
    }
};

pub const Populator = struct {
    id: u32,
    allocator: std.mem.Allocator,

    fn reserveId(self: *Populator) u32 {
        defer self.id += 1;
        return self.id;
    }

    pub fn init(allocator: std.mem.Allocator) Populator {
        return .{
            .id = 1,
            .allocator = allocator,
        };
    }

    pub fn populateSymtable(self: *Populator, node: *const pars.Node) !void {
        switch (node.*) {
            .Stmt => |stmt| {
                const table: *SymbolTable = stmt.symtable;
                switch (stmt.kind) {
                    .defVar => |variable| {
                        const symbol: Symbol = try self.buildValueSymb(
                            table,
                            if (variable.expr.typ) |typ| typ else pars.TypeIdent{ .ident = "i32", .list = false },
                            true,
                        );
                        if (!try table.insert(variable.ident.ident, symbol)) return error.FailedToInsert;
                    },
                    .defValue => |value| {
                        const symbol: Symbol = try self.buildValueSymb(
                            table,
                            if (value.expr.typ) |typ| typ else pars.TypeIdent{ .ident = "i32", .list = false },
                            false,
                        );
                        if (!try table.insert(value.ident.ident, symbol)) return error.FailedToInsert;
                    },
                    .block => {
                        const children = try stmt.children(self.allocator);
                        defer self.allocator.free(children);
                        for (children) |child| try self.populateSymtable(&child);
                    },
                    .function => |fun| {
                        const bodyTable = if (fun.block == null) stmt.symtable else fun.block.?.symtable;
                        const symbol: Symbol = try self.buildFunctionSymb(
                            bodyTable,
                            fun.args,
                            fun.retType,
                        );
                        if (!try table.insert(fun.ident.ident, symbol)) return error.FailedToInsert;
                        if (fun.block == null) return;
                        // var iter = fun.block.?.symtable.scope.?.symbs.iterator();
                        // while (iter.next()) |val| {
                        //     // std.debug.print("{s}\n", .{val.key_ptr.*});
                        // }

                        const block = fun.block.?.asNode();
                        try self.populateSymtable(&block);

                        // var iterTable = bodyTable.scope.?.symbs.iterator();
                        // while (iterTable.next()) |entry| {
                        //     // std.debug.print("{s} -> {any}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
                        // }
                    },

                    else => {},
                }
            },
            else => {
                for (try node.children(self.allocator)) |child|
                    try self.populateSymtable(&child);
            },
        }
    }

    fn buildFunctionSymb(
        self: *Populator,
        table: *SymbolTable,
        args: []const pars.FunctionArg,
        retType: ?pars.TypeIdent,
    ) !Symbol {
        var inputArr = std.ArrayList(SymbType).init(self.allocator);
        for (args) |arg| {
            // std.debug.print("{s}: {s}\n", .{ arg.ident, arg.typ.ident });
            const argSymb = try self.buildValueSymb(table, arg.typ, false);
            if (!try table.insert(arg.ident, argSymb)) return error.FailedToInsert;
            try inputArr.append(table.getType(arg.typ) orelse SymbType.Void);
        }
        const input = try inputArr.toOwnedSlice();

        const output = try self.allocator.create(SymbType);
        output.* = if (retType) |typ| table.getType(typ).? else SymbType.Void;
        const typ = SymbType{
            .Function = .{
                .output = output,
                .input = input,
            },
        };
        const id = self.reserveId();

        const name = try std.fmt.allocPrint(self.allocator, "func_{d}", .{id});
        _ = try table.insert(name, typ.toSymb());

        return Symbol{
            .Value = SymbValue{
                .mut = true,
                .id = id,
                .typ = typ,
            },
        };
    }

    fn buildValueSymb(self: *Populator, table: *SymbolTable, typ: pars.TypeIdent, mutable: bool) !Symbol {
        if (table.getType(typ)) |newTyp| {
            const value = SymbValue{
                .typ = newTyp,
                .id = self.reserveId(),
                .mut = mutable,
            };
            return value.toSymb();
        }
        // std.debug.print("{s}: ", .{typ.ident});
        return error.UnknownType;
    }
};
