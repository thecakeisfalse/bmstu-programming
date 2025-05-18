#include <GL/gl.h>
#include <GLFW/glfw3.h>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <ctime>
#include <vector>
#include <array>
#include <random>

struct Vec3 {
    float x, y, z;

    Vec3(float x = 0.0f, float y = 0.0f, float z = 0.0f) : x(x), y(y), z(z) {}

    Vec3 operator*(float k) const { return { x * k, y * k, z * k }; }

    Vec3 operator+(const Vec3& other) const { return { x + other.x, y + other.y, z + other.z }; }
    Vec3 operator-(const Vec3& other) const { return { x - other.x, y - other.y, z - other.z }; }

    float dot(const Vec3& other) const { return x * other.x + y * other.y + z * other.z; }

    Vec3 cross(const Vec3& other) const {
        return { y * other.z - z * other.y, z * other.x - x * other.z, x * other.y - y * other.x };
    }

    Vec3 normalized() const {
        const float len = std::sqrt(x * x + y * y + z * z);
        return len < 1e-8f ? Vec3 {} : *this * (1.0f / len);
    }
};

namespace config {
    constexpr int TEXTURE_SIZE = 256;
    constexpr int TORUS_STEPS = 100;

    constexpr float PHYSICS_THRESHOLD = 0.5f;

    constexpr float TORUS_MAJOR_RADIUS = 0.5f;
    constexpr float TORUS_MINOR_RADIUS_A = 0.2f;
    constexpr float TORUS_MINOR_RADIUS_B = 0.2f;

    constexpr float MOVEMENT_SPEED = 0.003f;
    constexpr float ROTATION_SPEED = 0.7f;

    const Vec3 LIGHT_POSITION = { 1.0f, 1.0f, -1.0f };

    constexpr std::array<float, 4> LIGHT_COLOR = { 1.0f, 1.0f, 1.0f, 1.0f };
    constexpr std::array<float, 4> MATERIAL_AMBIENT = { 0.2f, 0.2f, 0.2f, 1.0f };
    constexpr std::array<float, 4> MATERIAL_DIFFUSE = { 0.8f, 0.8f, 0.8f, 1.0f };
}  // namespace config

class Texture {
    const GLuint MAX_TEXTURE_ID = std::numeric_limits<GLuint>::max();

    GLuint id = MAX_TEXTURE_ID;
    std::vector<uint8_t> pixels;
    int size;

  public:
    Texture() : size(0) {}

    Texture(int size) : size(size) {
        pixels.resize(size * size * 3);
        generateNoise();
        createGLTexture();
    }

    ~Texture() {
        if (id != MAX_TEXTURE_ID) {
            glDeleteTextures(1, &id);
        }
    }

    Texture(const Texture&) = delete;
    Texture& operator=(const Texture&) = delete;

    Texture(Texture&& other) : id(other.id), size(other.size) {
        if (&other == this) {
            return;
        }

        other.id = 0;
    }
    Texture& operator=(Texture&& other) {
        if (&other == this) {
            return *this;
        }

        id = other.id;
        size = other.size;

        other.id = MAX_TEXTURE_ID;

        return *this;
    }

    GLuint get() const { return id; }

  private:
    void generateNoise() {
        std::mt19937 gen(std::random_device {}());
        std::uniform_real_distribution<float> dist(0.0f, 1.0f);

        for (int i = 0; i < size * size * 3; i += 3) {
            const float noise = dist(gen);
            pixels[i] = static_cast<uint8_t>(noise * 100);
            pixels[i + 1] = 0;
            pixels[i + 2] = static_cast<uint8_t>(noise * 200 + 55);
        }
    }

    void createGLTexture() {
        glGenTextures(1, &id);
        glBindTexture(GL_TEXTURE_2D, id);

        glTexImage2D(
            GL_TEXTURE_2D, 0, GL_RGB, size, size, 0, GL_RGB, GL_UNSIGNED_BYTE, pixels.data()
        );

        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    }
};

class Torus {
    const float a, b, c;

  public:
    Torus(float a, float b, float c) : a(a), b(b), c(c) {}

    Vec3 operator()(float u, float v) const {
        return {
            static_cast<float>((c + a * cos(v)) * cos(u)),
            static_cast<float>((c + a * cos(v)) * sin(u)),
            static_cast<float>(b * sin(v)),
        };
    }
};

class Application {
    float theta = 0.0f;
    float phi = 0.0f;
    float center = 0.0f;
    float direction = 1.0f;
    bool use_texture = false;

    Texture texture;
    Torus torus;
    GLFWwindow* window = nullptr;

    const std::array<float, 16> mirror_matrix = {
        1, 0, 0,  0,  //
        0, 1, 0,  0,  //
        0, 0, -1, 0,  //
        0, 0, 0,  1,
    };

  public:
    Application()
        : torus(
              config::TORUS_MINOR_RADIUS_A, config::TORUS_MINOR_RADIUS_B, config::TORUS_MAJOR_RADIUS
          ) {}

    ~Application() {
        if (window) {
            glfwTerminate();
        }
    }

    bool init() {
        if (!glfwInit()) {
            return false;
        }

        window = glfwCreateWindow(640, 640, "glfw", nullptr, nullptr);
        if (!window) {
            return false;
        }

        glfwMakeContextCurrent(window);
        glfwSetWindowUserPointer(window, this);

        glfwSetFramebufferSizeCallback(window, [](GLFWwindow*, int w, int h) {
            glViewport(0, 0, w, h);
        });

        glfwSetKeyCallback(window, [](GLFWwindow* window, int key, int, int action, int) {
            if (action == GLFW_PRESS && key == GLFW_KEY_C) {
                auto* app = static_cast<Application*>(glfwGetWindowUserPointer(window));
                app->use_texture = !app->use_texture;
            }
        });

        setupTextures();
        setupLighting();

        return true;
    }

    void run() {
        while (!glfwWindowShouldClose(window)) {
            update();
            render();

            glfwSwapBuffers(window);
            glfwPollEvents();
        }
    }

  private:
    void setupTextures() {
        glEnable(GL_TEXTURE_2D);

        texture = std::move(Texture(config::TEXTURE_SIZE));

        glDisable(GL_TEXTURE_2D);
    }

    void setupLighting() {
        glEnable(GL_DEPTH_TEST);
        glDepthFunc(GL_LESS);
        glEnable(GL_LIGHTING);
        glEnable(GL_LIGHT0);

        const std::array lightPos = {
            config::LIGHT_POSITION.x,
            config::LIGHT_POSITION.y,
            config::LIGHT_POSITION.z,
            1.0f,
        };

        glLightfv(GL_LIGHT0, GL_POSITION, lightPos.data());
        glLightfv(GL_LIGHT0, GL_DIFFUSE, config::LIGHT_COLOR.data());
        glLightfv(GL_LIGHT0, GL_SPECULAR, config::LIGHT_COLOR.data());

        glMaterialfv(GL_FRONT, GL_AMBIENT, config::MATERIAL_AMBIENT.data());
        glMaterialfv(GL_FRONT, GL_DIFFUSE, config::MATERIAL_DIFFUSE.data());

        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    }

    void update() {
        center += direction * config::MOVEMENT_SPEED;

        if (std::abs(center) >=
            config::TORUS_MINOR_RADIUS_A + config::TORUS_MAJOR_RADIUS + config::PHYSICS_THRESHOLD) {
            direction = -direction;
        }

        phi = std::fmod(phi + config::ROTATION_SPEED, 360.0f);
        theta = std::fmod(theta + config::ROTATION_SPEED, 360.0f);
    }

    void render() {
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();

        glTranslatef(center, 0, 0);

        glMultMatrixf(mirror_matrix.data());
        glRotatef(theta, 1, 0, 0);
        glRotatef(phi, 0, 1, 0);

        renderTorus();
    }

    void renderTorus() {
        if (use_texture) {
            glEnable(GL_TEXTURE_2D);
            glBindTexture(GL_TEXTURE_2D, texture.get());
        }

        glBegin(GL_QUADS);
        const float step = M_PI / config::TORUS_STEPS;

        for (int i = 0; i <= 2 * config::TORUS_STEPS; ++i) {
            for (int j = 0; j <= 2 * config::TORUS_STEPS; ++j) {
                const float u = step * i;
                const float v = step * j;

                const Vec3 A = torus(u, v);
                const Vec3 B = torus(u + step, v);
                const Vec3 C = torus(u + step, v + step);
                const Vec3 D = torus(u, v + step);

                const Vec3 normal = (B - A).cross(D - A).normalized();
                glNormal3f(normal.x, normal.y, normal.z);

                if (use_texture) {
                    const float s = i / float(2 * config::TORUS_STEPS);
                    const float t = j / float(2 * config::TORUS_STEPS);
                    const float ds = 1.0f / config::TORUS_STEPS;

                    glTexCoord2f(s, t);
                    glVertex3f(A.x, A.y, A.z);
                    glTexCoord2f(s + ds, t);
                    glVertex3f(B.x, B.y, B.z);
                    glTexCoord2f(s + ds, t + ds);
                    glVertex3f(C.x, C.y, C.z);
                    glTexCoord2f(s, t + ds);
                    glVertex3f(D.x, D.y, D.z);
                } else {
                    glVertex3f(A.x, A.y, A.z);
                    glVertex3f(B.x, B.y, B.z);
                    glVertex3f(C.x, C.y, C.z);
                    glVertex3f(D.x, D.y, D.z);
                }
            }
        }

        glEnd();

        if (use_texture) {
            glDisable(GL_TEXTURE_2D);
        }
    }
};

int main() {
    Application app;
    if (!app.init()) {
        return -1;
    }
    app.run();
    return 0;
}
