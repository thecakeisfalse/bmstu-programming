const std = @import("std");
const glfw = @import("zglfw");
const gl = @import("gl");

const string = []const u8;

// based on https://bsouthga.dev/posts/color-gradients-with-python

fn hex_to_digit(hex: u8) i16 {
    if ('0' <= hex and hex <= '9') {
        return hex - '0';
    }
    if ('A' <= hex and hex <= 'F') {
        return hex - 'A' + 10;
    }
    return hex - 'a' + 10;
}

fn hex_to_RGB(hex: string) [3]i16 {
    return .{
        hex_to_digit(hex[1]) * 16 + hex_to_digit(hex[2]),
        hex_to_digit(hex[3]) * 16 + hex_to_digit(hex[4]),
        hex_to_digit(hex[5]) * 16 + hex_to_digit(hex[6]),
    };
}

fn intToFloat(comptime T: type, value: T) f16 {
    return @as(f16, @floatFromInt(value));
}

fn linear_gradient(comptime start: string, comptime finish: string, comptime count: usize) [count][3]f16 {
    var color_list: [count][3]f16 = undefined;

    const s = hex_to_RGB(start);
    const f = hex_to_RGB(finish);

    for (0..count) |t| {
        for (&color_list[t], 0..) |*value, i| {
            value.* = intToFloat(i16, s[i]);
            value.* += intToFloat(i16, (f[i] - s[i]) * @as(i16, @intCast(t))) / count;
            value.* /= 256;
        }
    }

    return color_list;
}

var proc_table: gl.ProcTable = undefined;

const n = 3;

const default_rotation: f16 = -90 * (n - 2) / n;

var rotation: f16 = default_rotation;
var rotation_delta: f16 = 0.0;

var scale: f16 = 2.0;

fn display(_: *glfw.Window) void {
    gl.Clear(gl.COLOR_BUFFER_BIT);

    gl.LoadIdentity();
    gl.ClearColor(1.0, 1.0, 1.0, 1.0);

    gl.PushMatrix();
    gl.Rotatef(rotation, 0, 0, 1.0);

    gl.Begin(gl.POLYGON);

    const colors = linear_gradient("#eece13", "#b210ff", n);

    for (0..n, colors) |k, color| {
        // coordinates ~ complex roots of \sqrt[n]{1}
        // ~> (x', y') := C * (cos(2*pi*k/n), sin(2*pi*k/n))

        const angle = (2 * std.math.pi * intToFloat(usize, k)) / n;

        const x = @cos(angle) / scale;
        const y = @sin(angle) / scale;

        gl.Color3f(color[0], color[1], color[2]);
        gl.Vertex2f(x, y);
    }

    gl.End();

    gl.PopMatrix();

    rotation += rotation_delta;

    if (rotation >= 360) {
        rotation -= 360;
    } else if (rotation <= -360) {
        rotation += 360;
    }
}

fn framebufferResize(_: *glfw.Window, width: c_int, height: c_int) callconv(.C) void {
    gl.Viewport(0, 0, width, height);
}

fn keyCallback(_: *glfw.Window, key: c_int, _: c_int, action: c_int, _: c_int) callconv(.C) void {
    if (action == glfw.Press) {
        if (key == glfw.KeyRight) {
            rotation_delta = -0.4;
        } else if (key == glfw.KeyLeft) {
            rotation_delta = 0.4;
        }
    }
}

fn scrollCallback(_: *glfw.Window, xoffset: f64, yoffset: f64) callconv(.C) void {
    const delta = @as(f16, @floatCast(yoffset)) / 10;
    scale += if (xoffset > 0) delta else -delta;
    scale = @max(scale, 0.1);
}

pub fn main() !void {
    try glfw.init();
    defer glfw.terminate();

    const window: *glfw.Window = try glfw.createWindow(
        640,
        640,
        "glfw",
        null,
        null,
    );
    defer glfw.destroyWindow(window);

    glfw.makeContextCurrent(window);
    defer glfw.makeContextCurrent(null);

    _ = glfw.setKeyCallback(window, keyCallback);
    _ = glfw.setScrollCallback(window, scrollCallback);

    _ = glfw.setFramebufferSizeCallback(
        window,
        framebufferResize,
    );

    if (!proc_table.init(glfw.getProcAddress)) {
        return error.NotInitialized;
    }

    std.debug.print("{}\n", .{default_rotation});

    gl.makeProcTableCurrent(&proc_table);
    defer gl.makeProcTableCurrent(null);

    while (!glfw.windowShouldClose(window)) {
        display(window);
        glfw.swapBuffers(window);
        glfw.pollEvents();
    }
}
