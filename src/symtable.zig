const std = @import("std");
const pars = @import("parser.zig");

const Scope = struct {
    par: ?*Scope,
    symbs: std.StringHashMap(Symbol),
};

const Symbol = union(enum) {
    Type: SymbType,
    Value: SymbValue,
};

pub const SymbType = union(enum) {
    Void,
    Integer,
    String,
};

const SymbValue = struct {
    typ: SymbType,
    id: u32,
    mut: bool,
};

pub const SymbolTable = struct {
    scope: ?*Scope = null,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*SymbolTable {
        const scope = try allocator.create(Scope);
        scope.par = null;
        scope.symbs = std.StringHashMap(Symbol).init(allocator);
        const table = try allocator.create(SymbolTable);
        table.* = SymbolTable{
            .scope = scope,
            .allocator = allocator,
        };
        return table;
    }

    pub fn deinit(self: *SymbolTable) void {
        if (self.scope) |scope| {
            scope.symbs.deinit();
            self.allocator.destroy(scope);
        }
    }

    pub fn makeChild(self: *SymbolTable) SymbolTable {
        const scope = try self.allocator.create(Scope);
        scope.par = self;
        scope.symbs = std.StringHashMap(Symbol).init(self.allocator);
        return SymbolTable{
            .scope = scope,
            .allocator = self.allocator,
        };
    }

    pub fn parent(self: SymbolTable) ?*SymbolTable {
        if (self.scope) |scope|
            if (scope.par) |par|
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

    pub fn insert(self: *SymbolTable, ident: []const u8, symbol: Symbol) bool {
        if (self.scope) |scope| {
            if (scope.symbs.getEntry(ident)) return false;
            scope.symbs.put(ident, symbol);
            return true;
        }
        return false;
    }
};

const Populator = struct {
    id: u32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Populator {
        return .{
            .id = 0,
            .allocator = allocator,
        };
    }

    fn populateSymtable(self: *Populator, node: *pars.Node) void {
        switch (node) {
            .Stmt => |stmt| {
                const table: *SymbolTable = stmt.symtable;
                switch (stmt.kind) {
                    .defVar => |variable| {
                        const symbol = self.buildValueSymb(
                            table,
                            if (variable.expr.typ) |typ| typ else .Integer,
                            true,
                        );
                        table.insert(variable.ident, symbol);
                    },
                    .defValue => |value| {
                        const symbol = self.buildValueSymb(
                            table,
                            if (value.expr.typ) |typ| typ else .Integer,
                            true,
                        );
                        table.insert(value.ident, symbol);
                    },
                }
            },
            else => {
                for (node.children(self.allocator)) |child|
                    populateSymtable(&child);
            },
        }
    }

    fn buildValueSymb(self: *Populator, table: *SymbolTable, typ: SymbType, mutable: bool) Symbol {
        const newTyp = table.getType(typ);
    }
};
