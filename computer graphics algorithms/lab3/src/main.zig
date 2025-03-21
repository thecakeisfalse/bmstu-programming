const std = @import("std");
const glfw = @import("zglfw");
const gl = @import("gl");

var proc_table: gl.ProcTable = undefined;

const string = []const u8;

// based on https://bsouthga.dev/posts/color-gradients-with-python

fn intToFloat(comptime T: type, value: T) f16 {
    return @as(f16, @floatFromInt(value));
}

fn hex_to_digit(hex: u8) f16 {
    const v = switch (hex) {
        '0'...'9' => hex - '0',
        'A'...'F' => hex - 'A' + 10,
        'a'...'f' => hex - 'a' + 10,
        else => unreachable,
    };
    return intToFloat(@TypeOf(v), v);
}

fn hex_to_RGB(hex: string) [3]f16 {
    return .{
        hex_to_digit(hex[1]) * 16 + hex_to_digit(hex[2]),
        hex_to_digit(hex[3]) * 16 + hex_to_digit(hex[4]),
        hex_to_digit(hex[5]) * 16 + hex_to_digit(hex[6]),
    };
}

fn linear_gradient(comptime start: string, comptime finish: string, comptime count: usize) [count][3]f16 {
    var color_list: [count][3]f16 = undefined;

    const s = hex_to_RGB(start);
    const f = hex_to_RGB(finish);

    for (0..count) |t| {
        for (&color_list[t], 0..) |*value, i| {
            value.* = s[i];
            value.* += (f[i] - s[i]) / intToFloat(usize, count) * intToFloat(usize, t);
            value.* /= 256;
        }
    }

    return color_list;
}

fn framebufferResize(_: *glfw.Window, width: c_int, height: c_int) callconv(.C) void {
    gl.Viewport(0, 0, width, height);
}

fn degree2radian(angle: f32) f32 {
    const fixed_angle = @mod(angle, 360.0);
    return fixed_angle / 180.0 * std.math.pi;
}

const theta0 = 35.26;
const phi0 = -45.0;

var theta: f32 = 0;
var phi: f32 = 0;
var scale: f32 = 0.7;

const step = 100;

var switch_mode: bool = false;

fn torus_func_generator(a: f32, b: f32, c: f32) fn (f32, f32) [3]f32 {
    return struct {
        fn func(u: f32, v: f32) [3]f32 {
            return [_]f32{
                (c + a * @cos(v)) * @cos(u),
                (c + a * @cos(v)) * @sin(u),
                b * @sin(v),
            };
        }
    }.func;
}

fn elliptic_torus(comptime a: f32, comptime b: f32, comptime c: f32) void {
    gl.Begin(gl.QUADS);
    defer gl.End();

    const f = torus_func_generator(a, b, c);

    const eps = std.math.pi / intToFloat(usize, step);

    const colors = linear_gradient(
        "#eece13",
        "#b210ff",
        2 * step * step,
    );

    var i: f32 = 0;
    while (i < 2 * step) : (i += 1) {
        var j: f32 = 0;

        while (j < 2 * step) : (j += 1) {
            const A = f(eps * i, eps * j);
            const B = f(eps * (i + 1), eps * j);
            const C = f(eps * (i + 1), eps * (j + 1));
            const D = f(eps * i, eps * (j + 1));

            const part = if (i < step) i else (2 * step - i - 1);
            const index = part * 2 * step + j;

            const color = colors[@as(usize, @intFromFloat(index))];

            const brightness =
                if (j < step) 1 else @max(@abs(3 * step - 2 * j) / step, 0.4);

            gl.Color3f(
                brightness * color[0],
                brightness * color[1],
                brightness * color[2],
            );

            gl.Vertex3f(A[0], A[1], A[2]);
            gl.Vertex3f(B[0], B[1], B[2]);
            gl.Vertex3f(C[0], C[1], C[2]);
            gl.Vertex3f(D[0], D[1], D[2]);
        }
    }
}

fn glMultMatrixf(mat: []const f32) void {
    gl.MultMatrixf(@as([*c]const f32, @ptrCast(mat)));
}

fn display() void {
    gl.LoadIdentity();
    gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    gl.MatrixMode(gl.PROJECTION);

    gl.Translated(0.75, 0.75, 0);

    gl.Scaled(0.3, 0.3, 0.3);

    glMultMatrixf(&.{
        1, 0, 0,  0,
        0, 1, 0,  0,
        0, 0, -1, 0,
        0, 0, 0,  1,
    });

    gl.Rotatef(phi0, 1, 0, 0);
    gl.Rotatef(theta0, 0, 0, 1);

    elliptic_torus(0.2, 0.2, 0.5);

    gl.LoadIdentity();

    gl.Scaled(scale, scale, scale);

    glMultMatrixf(&.{
        1, 0, 0,  0,
        0, 1, 0,  0,
        0, 0, -1, 0,
        0, 0, 0,  1,
    });

    gl.Rotatef(theta, 1, 0, 0);
    gl.Rotatef(phi, 0, 0, 1);

    elliptic_torus(0.2, 0.2, 0.5);
}

fn keyCallback(_: *glfw.Window, key: glfw.Key, _: c_int, action: c_int, _: c_int) callconv(.C) void {
    if (action != glfw.Release) {
        switch (key) {
            glfw.KeyW => theta += -2,
            glfw.KeyS => theta += 2,
            glfw.KeyA => phi += -2,
            glfw.KeyD => phi += 2,
            glfw.KeyUp => theta += -1,
            glfw.KeyDown => theta += 1,
            glfw.KeyLeft => phi += -1,
            glfw.KeyRight => phi += 1,
            else => return,
        }

        phi = @mod(phi, 360);
        theta = @mod(theta, 360);
    }

    if (action == glfw.Release and key == glfw.KeyC) {
        switch_mode = !switch_mode;
        gl.PolygonMode(gl.FRONT_AND_BACK, if (switch_mode) gl.LINE else gl.FILL);
    }
}

fn scrollCallback(_: *glfw.Window, xoffset: f64, yoffset: f64) callconv(.C) void {
    const delta = @as(f16, @floatCast(yoffset)) / 10;
    scale += if (xoffset > 0) delta else -delta;
    scale = @min(@max(scale, 0.1), 1);
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

    _ = glfw.setFramebufferSizeCallback(window, framebufferResize);
    _ = glfw.setKeyCallback(window, keyCallback);
    _ = glfw.setScrollCallback(window, scrollCallback);

    if (!proc_table.init(glfw.getProcAddress)) {
        return error.NotInitialized;
    }

    gl.makeProcTableCurrent(&proc_table);
    defer gl.makeProcTableCurrent(null);

    gl.Enable(gl.DEPTH_TEST);
    gl.DepthFunc(gl.LESS);

    while (!glfw.windowShouldClose(window)) {
        display();
        glfw.swapBuffers(window);
        glfw.pollEvents();
    }
}
