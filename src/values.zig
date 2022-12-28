const assert = @import("std").debug.assert;
const std = @import("std");
pub const Value = u64;

pub const DataPtr = [*]align(@alignOf(u64)) u8;
pub const ConstDataPtr = [*]align(@alignOf(u64)) const u8;
pub const DataSlice = []align(@alignOf(u64)) u8;

// The first 4 bytes indicate the length of the string, which is the remaining part. Zig doesn't let us represent this as a struct (and probably for good reason too, this is not a good design pattern anywhere else)
pub const VString = struct {
    ptr: ConstDataPtr,
    pub fn fromValue(value: Value) VString {
        return VString{
            .ptr = @intToPtr(ConstDataPtr, value),
        };
    }

    pub inline fn toValue(self: @This()) Value {
        return @intCast(u64, @ptrToInt(self.ptr));
    }

    pub inline fn len(self: @This()) u32 {
        return @ptrCast(*const u32, self.ptr).*;
    }

    pub inline fn toZigString(self: @This()) []const u8 {
        const length = self.len();
        return self.ptr[4 .. 4 + length];
    }

    // Assumes the slice given has an underlying VString
    pub inline fn fromZigString(slice: []const u8) VString {
        return VString{ .ptr = @alignCast(@alignOf(u64), slice.ptr - @sizeOf(u32)) }; // offset from the header
    }

    comptime {
        assert(@sizeOf(VString) == @sizeOf(Value));
    }
};
