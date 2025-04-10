const std = @import("std");

const Point = struct {
    x: f64,
    y: f64,

    const Self = @This();

    fn init(x: f64, y: f64) Self {
        return .{ .x = x, .y = y };
    }

    fn round(self: Self) Self {
        if (self.size() == 0) {
            return self;
        }

        if (self.x == 0 or self.y == 0) {
            return self.multiply(1 / self.size());
        }

        var X = @round(self.x);
        var Y = @round(self.y);
        const k = self.y / self.x;

        if (@abs(Y - k * X) > 0.5) {
            const Y_opt = @round(k * X);
            const X_opt = @round(Y / k);

            if (@abs(Y_opt - k * X) < @abs(Y - k * X_opt)) {
                Y = Y_opt;
            } else {
                X = X_opt;
            }
        }

        return .init(X, Y);
    }

    fn multiply(self: Self, k: f64) Self {
        return .{ .x = self.x * k, .y = self.y * k };
    }

    fn norm(self: Self) Self {
        const sz = self.size();

        if (sz == 0) {
            return self;
        }

        return self.multiply(sz);
    }

    fn dist(s: Self, o: Self) f64 {
        const dx = s.x - o.x;
        const dy = s.y - o.y;

        return @sqrt(dx * dx + dy * dy);
    }

    fn mdist(s: Self, o: Self) f64 {
        return @abs(s.x - o.x) + @abs(s.y - o.y);
    }

    fn cross(s: Self, o: Self) f64 {
        return s.x * o.y - s.y * o.x;
    }

    fn dot(s: Self, o: Self) f64 {
        return s.x * o.x + s.y * o.y;
    }

    fn sum(s: Self, o: Self) Self {
        return .{
            .x = s.x + o.x,
            .y = s.y + o.y,
        };
    }

    fn diff(s: Self, o: Self) Self {
        return .{
            .x = s.x - o.x,
            .y = s.y - o.y,
        };
    }

    fn isZero(s: Self) bool {
        return s.x == 0 and s.y == 0;
    }

    fn size(s: Self) f64 {
        return @sqrt(dot(s, s));
    }

    fn angle(s: Self, o: Self) f64 {
        if (s.isZero() or o.isZero()) {
            return 0;
        }

        return std.math.acos(
            dot(s, o) / (s.size() * o.size()),
        );
    }

    const Comparator = struct {
        fn locLess(_: void, a: Self, b: Self) bool {
            return a.y < b.y or (a.y == b.y and a.x < b.x);
        }

        fn polarAngleLess(_: void, a: Self, b: Self) bool {
            const a1 = Self.angle(a, Ox);
            const a2 = Self.angle(b, Ox);
            return a1 < a2 or (a1 == a2 and a.size() < b.size());
        }
    };
};

const Vec2 = Point;

const Segment = struct {
    s: Point,
    e: Point,

    const Self = @This();

    fn init(start: Point, end: Point) Self {
        return .{ .s = start, .e = end };
    }
};

const Segments = std.ArrayList(Segment);
const Points = std.ArrayList(Point);

const PointWithDirections = struct {
    p: Point,
    v1: Vec2,
    v2: Vec2,
};

fn getSegmentsIntersectionsWithDirections(
    segments: Segments,
    allocator: std.mem.Allocator,
) !std.ArrayList(PointWithDirections) {
    const size = segments.items.len;
    var result = std.ArrayList(PointWithDirections).init(allocator);

    for (0..size) |i| {
        for (i + 1..size) |j| {
            const u = segments.items[i];
            const v = segments.items[j];

            const det = (v.e.x - v.s.x) * (u.e.y - u.s.y) - (v.e.y - v.s.y) * (u.e.x - u.s.x);

            if (det == 0) {
                continue;
            }

            const u_t = ((v.e.x - v.s.x) * (v.s.y - u.s.y) - (v.e.y - v.s.y) * (v.s.x - u.s.x)) / det;
            const v_t = ((u.e.x - u.s.x) * (v.s.y - u.s.y) - (u.e.y - u.s.y) * (v.s.x - u.s.x)) / det;

            if ((0 < u_t and u_t < 1) and (0 < v_t and v_t < 1)) {
                const x = u.s.x + u_t * (u.e.x - u.s.x);
                const y = u.s.y + u_t * (u.e.y - u.s.y);

                try result.append(.{
                    .p = .init(x, y),
                    .v1 = .diff(u.e, u.s),
                    .v2 = .diff(v.e, v.s),
                });
            }
        }
    }

    return result;
}

fn getPolarOrder(points: Points, base: Point) !Points {
    const result = try points.clone();

    for (points.items, 0..) |p, i| {
        result.items[i] = p.diff(base);
    }

    std.mem.sort(Point, result.items, {}, Point.Comparator.polarAngleLess);

    for (points.items, 0..) |p, i| {
        result.items[i] = p.sum(base);
    }

    return points;
}

fn getTheLeftestAmongTheLowests(points: Points) Point {
    var p0 = points.items[0];

    for (points.items) |p| {
        if (Point.Comparator.locLess({}, p, p0)) {
            p0 = p;
        }
    }

    return p0;
}

const Polygon = struct {
    points: Points,
    segments: Segments,
    updated: bool,

    const Self = @This();
    const accuracy = 1e-2;

    fn init(allocator: std.mem.Allocator) Self {
        return .{
            .points = .init(allocator),
            .segments = .init(allocator),
            .updated = false,
        };
    }

    fn deinit(self: *Self) void {
        self.points.deinit();
        self.segments.deinit();
    }

    fn getSegments(self: *Self) !Segments {
        if (!self.updated) {
            return self.segments;
        }

        self.updated = false;
        self.segments.clearAndFree();

        const points = self.points.items;
        const size = points.len;

        for (0..size) |i| {
            try self.segments.append(.init(
                points[i],
                points[(i + 1) % size],
            ));
        }

        return self.segments;
    }

    fn addPoint(self: *Self, p: Point) !void {
        for (self.points.items) |q| {
            if (p.mdist(q) < accuracy) {
                return;
            }
        }

        self.updated = true;
        try self.points.append(p);
    }

    fn removePoint(self: *Self, p: Point) void {
        var removed: usize = 0;

        for (self.points.items, 0..) |q, i| {
            if (p.mdist(q) < accuracy) {
                self.updated = true;
                _ = self.points.swapRemove(i - removed);
                removed += 1;
            }
        }
    }

    fn display(self: Self) void {
        gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        for (self.points.items) |q| {
            gl.Color3f(1, 1, 1);
            gl.Rectd(
                q.x - accuracy,
                q.y - accuracy,
                q.x + accuracy,
                q.y + accuracy,
            );
        }
    }
};

const Ox = Point.init(1, 0);
const Oy = Point.init(0, 1);

const gl = @import("gl");
const glfw = @import("glfw");

fn sign(x: f64) f64 {
    return if (x == 0) 0 else x / @abs(x);
}

const Rasterization = struct {
    pixels: Pixels,
    scale: Vec2,

    const Self = @This();
    const Pixels = std.ArrayList(u8);

    fn init() Self {
        return .{ .pixels = undefined, .scale = undefined };
    }

    fn deinit(self: *Self) void {
        self.pixels.deinit();
    }

    fn isValid(self: Self, x: f64, y: f64) bool {
        return 0 <= x and x < self.scale.x and 0 <= y and y < self.scale.y;
    }

    fn set(self: Self, x: f64, y: f64, value: f64) void {
        if (!self.isValid(x, y)) {
            return;
        }

        const index: usize = @intFromFloat(y * self.scale.x + x);
        if (index >= self.pixels.capacity) {
            return;
        }

        self.pixels.items[index] = @intFromFloat(value);
    }

    fn get(self: Self, x: f64, y: f64) u8 {
        if (!self.isValid(x, y)) {
            return 0;
        }

        const index: usize = @intFromFloat(y * self.scale.x + x);
        if (index >= self.pixels.capacity) {
            return 0;
        }

        return self.pixels.items[index];
    }

    fn fromPolygon(
        self: *Self,
        polygon: *Polygon,
        scale: Vec2,
        use_antialias: bool,
        allocator: std.mem.Allocator,
    ) !void {
        self.scale = scale;

        self.pixels.clearRetainingCapacity();
        self.pixels = try Pixels.initCapacity(
            allocator,
            @intFromFloat(scale.x * scale.y),
        );

        for (0..self.pixels.capacity) |_| {
            try self.pixels.append(0);
        }

        const points_count = polygon.points.items.len;

        if (points_count == 0) {
            return;
        }

        const segments = try polygon.getSegments();

        for (segments.items) |segment| {
            if (use_antialias) {
                try self.segmentBresenhamWithAntialiasing(segment);
            } else {
                try self.segmentBresenham(segment);
            }
        }

        var intersect = try getSegmentsIntersectionsWithDirections(
            segments,
            allocator,
        );
        defer intersect.deinit();

        if (intersect.items.len == 0) {
            var centroid = Point.init(0, 0);

            for (polygon.points.items) |point| {
                centroid = centroid.sum(self.canonizePoint(point));
            }

            centroid = centroid.multiply(1 / @as(f64, @floatFromInt(points_count))).round();

            try self.fillWithSeed(centroid, allocator);

            return;
        }

        const eps = 1e-2;

        for (intersect.items) |point| {
            const base = point.p;
            const shift = point.v1.diff(point.v2).norm().multiply(eps);

            const p1 = self.canonizePoint(base.sum(shift));
            const p2 = self.canonizePoint(base.diff(shift));

            try self.fillWithSeed(p1, allocator);
            try self.fillWithSeed(p2, allocator);
        }
    }

    fn canonizePoint(self: Self, p: Point) Point {
        return .{
            .x = @round((p.x + 1) / 2 * self.scale.x),
            .y = @round((p.y + 1) / 2 * self.scale.y),
        };
    }

    fn normalizePoint(self: Self, p: Point) Point {
        return .{
            .x = @round(p.x) * 2 / self.scale.x - 1,
            .y = 1 - @round(p.y) * 2 / self.scale.y,
        };
    }

    fn segmentBresenhamWithAntialiasing(self: *Self, segment: Segment) !void {
        const start = self.canonizePoint(segment.s);
        const end = self.canonizePoint(segment.e);

        if (start.x == end.x and start.y == end.y) {
            self.set(start.x, start.y, 255);
            return;
        }

        var dx = @abs(end.x - start.x);
        var dy = @abs(end.y - start.y);

        const sx: f64 = sign(end.x - start.x);
        const sy: f64 = sign(end.y - start.y);

        var x = start.x;
        var y = start.y;

        var h: f64 = if (dx == 0) 0 else dy / dx;

        const fl = dy > dx;

        if (dy > dx) {
            std.mem.swap(f64, &dx, &dy);
            h = if (h != 0) 1 / h else h;
        }

        const i_max: f64 = 256;

        h *= i_max;
        var e: f64 = 0.5;
        const w = i_max - h;

        var i: f64 = 1;

        self.set(x, y, h / 2);

        while (i <= dx) : (i += 1) {
            if (e <= w) {
                if (fl) {
                    y += sy;
                } else {
                    x += sx;
                }
                e += h;
            } else {
                e -= w;
                x += sx;
                y += sy;
            }

            self.set(x, y, @max(255 * (1 - (e / 255)), 1));
        }
    }

    fn segmentBresenham(self: *Self, segment: Segment) !void {
        const start = self.canonizePoint(segment.s);
        const end = self.canonizePoint(segment.e);

        const dx = @abs(end.x - start.x);
        const dy = -@abs(end.y - start.y);

        const sx: f64 = if (start.x < end.x) 1 else -1;
        const sy: f64 = if (start.y < end.y) 1 else -1;

        var err = dx + dy;
        var x = start.x;
        var y = start.y;

        while (true) {
            self.set(x, y, 255);

            const e2 = 2 * err;
            if (e2 >= dy) {
                if (x == end.x) {
                    break;
                }

                err += dy;
                x += sx;
            }

            if (e2 <= dx) {
                if (y == end.y) {
                    break;
                }

                err += dx;
                y += sy;
            }
        }
    }

    fn fillWithSeed(
        self: *Self,
        seed: Point,
        allocator: std.mem.Allocator,
    ) !void {
        var stack = Points.init(allocator);
        try stack.append(seed);

        while (stack.items.len > 0) {
            const cur = stack.pop() orelse seed;

            if (self.get(cur.x, cur.y) != 0) {
                continue;
            }

            var x_left = cur.x;
            var x_right = cur.x;

            while (true) {
                if (!self.isValid(x_left - 1, cur.y)) {
                    break;
                }

                if (self.get(x_left - 1, cur.y) != 0) {
                    break;
                }

                x_left -= 1;
            }

            while (true) {
                if (!self.isValid(x_right + 1, cur.y)) {
                    break;
                }

                if (self.get(x_right + 1, cur.y) != 0) {
                    break;
                }

                x_right += 1;
            }

            if (x_right + 1 == self.scale.x or x_left == 0) {
                continue;
            }

            var x = x_left;
            while (x <= x_right) : (x += 1) {
                const p = Point.init(x, cur.y);
                self.set(p.x, p.y, 255);

                for (directions) |d| {
                    const q = p.sum(d);

                    if (!self.isValid(q.x, q.y)) {
                        continue;
                    }

                    if (self.get(q.x, q.y) == 0) {
                        try stack.append(q);
                    }
                }
            }
        }
    }

    fn display(
        self: Self,
    ) !void {
        gl.DrawPixels(
            @intFromFloat(self.scale.x),
            @intFromFloat(self.scale.y),
            gl.BLUE,
            gl.UNSIGNED_BYTE,
            @ptrCast(self.pixels.items),
        );
    }
};

const directions = [_]Vec2{
    .{ .x = 1, .y = 0 },
    .{ .x = -1, .y = 0 },
    .{ .x = 0, .y = 1 },
    .{ .x = 0, .y = -1 },
};

fn getWindowSize(window: *glfw.Window) Vec2 {
    var width: c_int = undefined;
    var height: c_int = undefined;

    glfw.getWindowSize(window, &width, &height);

    return .{
        .x = @floatFromInt(width),
        .y = @floatFromInt(height),
    };
}

fn getFramebufferSize(window: *glfw.Window) Vec2 {
    var width: c_int = undefined;
    var height: c_int = undefined;

    glfw.getFramebufferSize(window, &width, &height);

    return .{
        .x = @floatFromInt(width),
        .y = @floatFromInt(height),
    };
}

const ViewMode = enum { Points, Raster, Filter };

const Render = struct {
    polygon: Polygon,
    raster: Rasterization,
    allocator: std.mem.Allocator,
    mode: ViewMode,

    const Self = @This();

    fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .polygon = Polygon.init(allocator),
            .raster = Rasterization.init(),
            .mode = .Points,
        };
    }

    fn deinit(self: *Self) void {
        self.polygon.deinit();
        self.raster.deinit();
    }

    fn display(self: *Self, window: *glfw.Window) !void {
        const scale = getFramebufferSize(window);

        if (self.mode == .Points) {
            self.polygon.display();
            return;
        }

        try self.raster.fromPolygon(
            &self.polygon,
            scale,
            self.mode == .Filter,
            self.allocator,
        );

        try self.raster.display();
    }
};

var render: Render = undefined;

var proc_table: gl.ProcTable = undefined;

fn framebufferResize(_: *glfw.Window, width: c_int, height: c_int) callconv(.C) void {
    gl.Viewport(0, 0, width, height);
}

fn keyCallback(
    _: *glfw.Window,
    key: glfw.Key,
    _: c_int,
    action: c_int,
    _: c_int,
) callconv(.C) void {
    if (action != glfw.Release) {
        return;
    }

    switch (key) {
        glfw.KeyG => render.mode = .Raster,
        glfw.KeyF => render.mode = .Filter,
        else => return,
    }
}

fn getNormalizedCursor(window: *glfw.Window) Vec2 {
    var x: f64 = undefined;
    var y: f64 = undefined;

    glfw.getCursorPos(window, &x, &y);

    const win_size = getWindowSize(window);

    return .{
        .x = @round(x) * 2 / win_size.x - 1,
        .y = 1 - @round(y) * 2 / win_size.y,
    };
}

fn mouseButtonCallback(
    window: *glfw.Window,
    button: c_int,
    action: c_int,
    _: c_int,
) callconv(.C) void {
    if (action != glfw.Press) {
        return;
    }

    switch (button) {
        glfw.MouseButtonLeft => {
            const p = getNormalizedCursor(window);
            render.polygon.addPoint(p) catch {};
            render.mode = .Points;
        },
        glfw.MouseButtonRight => {
            const p = getNormalizedCursor(window);
            render.polygon.removePoint(p);
            render.mode = .Points;
        },
        else => {},
    }
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
    _ = glfw.setMouseButtonCallback(window, mouseButtonCallback);

    if (!proc_table.init(glfw.getProcAddress)) {
        return error.NotInitialized;
    }

    gl.makeProcTableCurrent(&proc_table);
    defer gl.makeProcTableCurrent(null);

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    render = .init(allocator);
    defer render.deinit();

    while (!glfw.windowShouldClose(window)) {
        try render.display(window);
        glfw.swapBuffers(window);
        glfw.pollEvents();
    }
}
