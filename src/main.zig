const std = @import("std");

const Allocator = std.mem.Allocator;
const AL = std.ArrayList;
const p = @import("parser.zig");
const elab = @import("elab.zig");
const types = @import("types.zig");
const comp = @import("compiler.zig");
const interp = @import("interpreter.zig");
const values = @import("values.zig");
const errs = @import("errors.zig");

const debug_mode = false;

pub fn interpret(source: [:0]const u8, allocator: Allocator, vm: *interp.VM) !void {
    var errors = errs.Errors{};

    std.debug.print("Parsing..\n", .{});
    var parser = p.Parser.init(source, &errors);

    const stmts = parser.parseProgram() catch |e| {
        if (e != error.ParseUnsuccessful) {
            return e;
        } else {
            try errs.reportErrors(&errors, source);
            return;
        }
    };
    if (errors.errors.len > 0) {
        try errs.reportErrors(&errors, source);
        return;
    }

    var buffered_writer = std.io.bufferedWriter(std.io.getStdOut().writer());
    var writer = buffered_writer.writer();

    var elaborator = elab.Elaborator.init(source, allocator, &errors);
    defer elaborator.deinit();

    std.debug.print("Type checking..\n", .{});
    elaborator.elabProgram(stmts) catch |e| {
        if (e != error.ElabFailed) {
            return e;
        } else {
            try errs.reportErrors(&errors, source);
            return;
        }
    };
    if (errors.errors.len > 0) {
        try errs.reportErrors(&errors, source);
        return;
    }

    const mainfn = elaborator.globals.get("main");
    const mainfn_id = if (mainfn) |mfid| mfid.id else {
        std.debug.print("No main function found\n", .{});
        return;
    };

    var chunkArena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer chunkArena.deinit();
    var compiler = comp.ProgramCompiler{
        .vm = vm,
        .source = source,
    };
    std.debug.print("Compiling code..\n", .{});
    try compiler.compileProgram(stmts, chunkArena.allocator());

    const mainChunk = @intToPtr(*comp.CompiledBytecodeChunk, vm.globals.items[mainfn_id]);
    if (debug_mode) {
        try mainChunk.pprint(writer);
        try buffered_writer.flush();
    }

    std.debug.print("Running code..\n==========\n", .{});

    try vm.interpret(mainChunk);
}

const maxFileSize = 1 << 32;
pub fn runFile(path: []const u8, allocator: Allocator) !void {
    const file = std.fs.cwd().openFile(path, .{}) catch |e| {
        if (e == error.FileNotFound) {
            std.log.err("File not found: {s}\n", .{path});
            return;
        } else return e;
    };
    var stat = try file.stat();
    const fileContents = try file.readToEndAllocOptions(allocator, maxFileSize, stat.size, @alignOf(u8), 0);
    defer allocator.free(fileContents);

    var vm = try interp.VM.init(allocator);
    defer vm.deinit();
    try interpret(fileContents, allocator, &vm);
}

pub fn readLine(allocator: Allocator) !?[:0]u8 {
    const stdin = std.io.getStdIn().reader();
    var array_list = std.ArrayList(u8).init(allocator);
    defer array_list.deinit();
    stdin.readUntilDelimiterArrayList(&array_list, '\n', 1 << 10) catch |err| switch (err) {
        error.EndOfStream => if (array_list.items.len == 0) {
            return null;
        } else {
            try array_list.append(0);
            var buff = array_list.toOwnedSlice();
            return buff[0 .. buff.len - 1 :0];
        },
        else => |e| return e,
    };
    try array_list.append(0);
    var buff = array_list.toOwnedSlice();
    return buff[0 .. buff.len - 1 :0];
}

// Not supported at this time
pub fn runRepl(allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    var vm = interp.VM.init(allocator);
    defer vm.deinit();
    while (true) {
        _ = try stdout.write("> ");
        const line = try readLine(allocator);
        defer if (line) |l| allocator.free(l);
        if (line) |l| {
            if (std.mem.startsWith(u8, l, ":q")) {
                return;
            }
            try interpret(l, allocator, &vm);
        } else {
            _ = try stdout.write("\n");
            return;
        }
    }
}

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    defer {
        _ = gpa.detectLeaks();
        _ = gpa.deinit();
    }
    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    switch (args.len) {
        1 => {
            std.log.err("Error: REPL mode not supported at this time.", .{});
            // try runRepl(allocator);
        },
        2 => {
            try runFile(args[1], allocator);
        },
        else => {
            std.log.err("Error: too many arguments, try:\n{s} [filename] ", .{args[0]});
        },
    }
}
