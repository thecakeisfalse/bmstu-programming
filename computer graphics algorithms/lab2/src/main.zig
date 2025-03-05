const std = @import("std");
const glfw = @import("zglfw");
const gl = @import("gl");

var proc_table: gl.ProcTable = undefined;

fn framebufferResize(_: *glfw.Window, width: c_int, height: c_int) callconv(.C) void {
    gl.Viewport(0, 0, width, height);
}

fn degree2radian(angle: f32) f32 {
    const fixed_angle = @mod(angle, 360.0);
    return fixed_angle / 180.0 * std.math.pi;
}

const theta0 = 35.26;
const phi0 = 45.0;

var theta: f32 = 0;
var phi: f32 = 0;
var scale: f32 = 0.7;

var switch_mode: bool = false;

fn cube(size: f32) void {
    gl.Begin(gl.QUADS);
    defer gl.End();

    gl.Color3f(0.0, 0.0, 1.0);
    gl.Vertex3f(-size / 2, -size / 2, -size / 2);
    gl.Vertex3f(-size / 2, size / 2, -size / 2);
    gl.Vertex3f(-size / 2, size / 2, size / 2);
    gl.Vertex3f(-size / 2, -size / 2, size / 2);

    gl.Color3f(1.0, 0.0, 0.0);
    gl.Vertex3f(size / 2, -size / 2, -size / 2);
    gl.Vertex3f(size / 2, -size / 2, size / 2);
    gl.Vertex3f(size / 2, size / 2, size / 2);
    gl.Vertex3f(size / 2, size / 2, -size / 2);

    gl.Color3f(0.0, 1.0, 0.0);
    gl.Vertex3f(-size / 2, -size / 2, -size / 2);
    gl.Vertex3f(-size / 2, -size / 2, size / 2);
    gl.Vertex3f(size / 2, -size / 2, size / 2);
    gl.Vertex3f(size / 2, -size / 2, -size / 2);

    gl.Color3f(1.0, 1.0, 0.0);
    gl.Vertex3f(-size / 2, size / 2, -size / 2);
    gl.Vertex3f(-size / 2, size / 2, size / 2);
    gl.Vertex3f(size / 2, size / 2, size / 2);
    gl.Vertex3f(size / 2, size / 2, -size / 2);

    gl.Color3f(0.0, 1.0, 1.0);
    gl.Vertex3f(-size / 2, -size / 2, -size / 2);
    gl.Vertex3f(size / 2, -size / 2, -size / 2);
    gl.Vertex3f(size / 2, size / 2, -size / 2);
    gl.Vertex3f(-size / 2, size / 2, -size / 2);

    gl.Color3f(1.0, 0.0, 1.0);
    gl.Vertex3f(-size / 2, -size / 2, size / 2);
    gl.Vertex3f(size / 2, -size / 2, size / 2);
    gl.Vertex3f(size / 2, size / 2, size / 2);
    gl.Vertex3f(-size / 2, size / 2, size / 2);
}

fn glMultMatrixf(mat: []const f32) void {
    gl.MultMatrixf(@as([*c]const f32, @ptrCast(mat)));
}

fn display() void {
    gl.LoadIdentity();
    gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    gl.MatrixMode(gl.PROJECTION);

    gl.Translated(0.75, 0.75, 0);

    glMultMatrixf(&.{
        1, 0, 0,  0,
        0, 1, 0,  0,
        0, 0, -1, 0,
        0, 0, 0,  1,
    });

    gl.Rotatef(theta0, 1, 0, 0);
    gl.Rotatef(phi0, 0, 1, 0);

    cube(0.2);

    gl.LoadIdentity();
    gl.Translated(0, 0, 0);

    glMultMatrixf(&.{
        1, 0, 0,  0,
        0, 1, 0,  0,
        0, 0, -1, 0,
        0, 0, 0,  1,
    });

    gl.Rotatef(theta, 1, 0, 0);
    gl.Rotatef(phi, 0, 1, 0);

    cube(scale);
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
        if (switch_mode) {
            gl.PolygonMode(gl.FRONT_AND_BACK, gl.LINE);
        } else {
            gl.PolygonMode(gl.FRONT_AND_BACK, gl.FILL);
        }
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
