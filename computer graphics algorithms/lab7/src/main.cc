#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <ctime>
#include <vector>
#include <array>
#include <random>
#include <iostream>

#define USE_IBO 1
#define USE_DISPLAY_LISTS 1
#define USE_VBO 1
#define USE_MIPMAPS 1

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
    constexpr int TORUS_STEPS = 1000;

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
    GLuint id = 0;
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
        if (id != 0) {
            glDeleteTextures(1, &id);
        }
    }

    Texture(const Texture&) = delete;
    Texture& operator=(const Texture&) = delete;

    Texture(Texture&& other) noexcept
        : id(other.id), pixels(std::move(other.pixels)), size(other.size) {
        other.id = 0;
    }

    Texture& operator=(Texture&& other) noexcept {
        if (this != &other) {
            if (id != 0) {
                glDeleteTextures(1, &id);
            }
            id = other.id;
            pixels = std::move(other.pixels);
            size = other.size;
            other.id = 0;
        }
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

#if USE_MIPMAPS
        glGenerateMipmap(GL_TEXTURE_2D);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
#else
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
#endif

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
            (c + a * cosf(v)) * cosf(u),
            (c + a * cosf(v)) * sinf(u),
            b * sinf(v),
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

    GLuint torusDisplayList = 0;

    GLuint vbo_vertices = 0;
    GLuint vbo_normals = 0;
    GLuint vbo_texcoords = 0;
    GLuint ibo_indices = 0;

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

#if USE_VBO
        if (vbo_vertices) {
            glDeleteBuffers(1, &vbo_vertices);
        }
        if (vbo_normals) {
            glDeleteBuffers(1, &vbo_normals);
        }
        if (vbo_texcoords) {
            glDeleteBuffers(1, &vbo_texcoords);
        }
#endif

#if USE_IBO
        if (ibo_indices) {
            glDeleteBuffers(1, &ibo_indices);
        }
#endif

#if USE_DISPLAY_LISTS
        if (torusDisplayList != 0) {
            glDeleteLists(torusDisplayList, 1);
        }
#endif
    }

    bool init() {
        if (!glfwInit()) {
            std::cerr << "Failed to initialize GLFW\n";
            return false;
        }

        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
        glfwWindowHint(GLFW_SAMPLES, 4);

        window = glfwCreateWindow(640, 640, "glfw", nullptr, nullptr);
        if (!window) {
            std::cerr << "Failed to create GLFW window\n";
            glfwTerminate();
            return false;
        }

        glfwMakeContextCurrent(window);
        glfwSetWindowUserPointer(window, this);

        GLenum err = glewInit();
        if (err != GLEW_OK) {
            std::cerr << "Failed to initialize GLEW: " << glewGetErrorString(err) << "\n";
            glfwTerminate();
            return false;
        }

        glfwSetFramebufferSizeCallback(window, [](GLFWwindow*, int w, int h) {
            glViewport(0, 0, w, h);
        });

        glfwSetKeyCallback(window, [](GLFWwindow* window, int key, int, int action, int) {
            if (action == GLFW_PRESS && key == GLFW_KEY_C) {
                auto* app = static_cast<Application*>(glfwGetWindowUserPointer(window));
                app->use_texture = !app->use_texture;
            }
        });

        if (!setupGL()) {
            return false;
        }

        return true;
    }

    void run() {
        while (!glfwWindowShouldClose(window)) {
            const double start_time = glfwGetTime();

            update();
            render();
            glfwSwapBuffers(window);

            glfwPollEvents();

            const double end_time = glfwGetTime();
            const double delta_time = (end_time - start_time) * 1000;

            std::cout << delta_time << std::endl;
        }
    }

  private:
    bool setupGL() {
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_MULTISAMPLE);

        try {
            setupTextures();
            setupLighting();

#if USE_VBO
            setupVBO();
#else
#if USE_DISPLAY_LISTS
            createDisplayList();
#endif
#endif
            return true;
        } catch (const std::exception& e) {
            std::cerr << "Initialization error: " << e.what() << "\n";
            return false;
        }
    }

    void setupTextures() {
        glEnable(GL_TEXTURE_2D);
        texture = Texture(config::TEXTURE_SIZE);
        glDisable(GL_TEXTURE_2D);
    }

    void setupLighting() {
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

#if USE_VBO
        renderTorusWithVBO();
#else
#if USE_DISPLAY_LISTS
        renderTorusWithDisplayList();
#else
        renderTorus();
#endif
#endif
    }

    void renderTorus() {
        if (use_texture) {
            glEnable(GL_TEXTURE_2D);
            glBindTexture(GL_TEXTURE_2D, texture.get());
        }

        glBegin(GL_QUADS);
        const float step = static_cast<float>(M_PI) / config::TORUS_STEPS;

        for (int i = 0; i < 2 * config::TORUS_STEPS; ++i) {
            for (int j = 0; j < 2 * config::TORUS_STEPS; ++j) {
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

    void createDisplayList() {
        torusDisplayList = glGenLists(1);
        glNewList(torusDisplayList, GL_COMPILE);

        renderTorus();

        glEndList();
    }

    void renderTorusWithDisplayList() {
        if (use_texture) {
            glEnable(GL_TEXTURE_2D);
            glBindTexture(GL_TEXTURE_2D, texture.get());
        }

        glCallList(torusDisplayList);

        if (use_texture) {
            glDisable(GL_TEXTURE_2D);
        }
    }

    void setupVBO() {
        std::vector<float> vertices, normals, texcoords;
        std::vector<unsigned int> indices;

        const float u_step = 2.0f * static_cast<float>(M_PI) / config::TORUS_STEPS;
        const float v_step = 2.0f * static_cast<float>(M_PI) / config::TORUS_STEPS;

        for (int i = 0; i <= config::TORUS_STEPS; ++i) {
            for (int j = 0; j <= config::TORUS_STEPS; ++j) {
                const float u = u_step * i;
                const float v = v_step * j;

                const Vec3 pos = torus(u, v);
                const Vec3 next_u = torus(u + u_step, v);
                const Vec3 next_v = torus(u, v + v_step);

                const Vec3 tangent_u = next_u - pos;
                const Vec3 tangent_v = next_v - pos;
                const Vec3 normal = tangent_u.cross(tangent_v).normalized();

                vertices.insert(vertices.end(), { pos.x, pos.y, pos.z });
                normals.insert(normals.end(), { normal.x, normal.y, normal.z });

                float s = u / (2.0f * static_cast<float>(M_PI));
                float t = v / (2.0f * static_cast<float>(M_PI));
                texcoords.insert(texcoords.end(), { s, t });
            }
        }

        for (int i = 0; i < config::TORUS_STEPS; ++i) {
            for (int j = 0; j < config::TORUS_STEPS; ++j) {
                unsigned int first = i * (config::TORUS_STEPS + 1) + j;
                unsigned int second = first + 1;
                unsigned int third = (i + 1) * (config::TORUS_STEPS + 1) + j + 1;
                unsigned int fourth = (i + 1) * (config::TORUS_STEPS + 1) + j;

                indices.insert(indices.end(), { first, second, third, fourth });
            }
        }

        glGenBuffers(1, &vbo_vertices);
        glBindBuffer(GL_ARRAY_BUFFER, vbo_vertices);
        glBufferData(
            GL_ARRAY_BUFFER, vertices.size() * sizeof(float), vertices.data(), GL_STATIC_DRAW
        );

        glGenBuffers(1, &vbo_normals);
        glBindBuffer(GL_ARRAY_BUFFER, vbo_normals);
        glBufferData(
            GL_ARRAY_BUFFER, normals.size() * sizeof(float), normals.data(), GL_STATIC_DRAW
        );

        glGenBuffers(1, &vbo_texcoords);
        glBindBuffer(GL_ARRAY_BUFFER, vbo_texcoords);
        glBufferData(
            GL_ARRAY_BUFFER, texcoords.size() * sizeof(float), texcoords.data(), GL_STATIC_DRAW
        );

#if USE_IBO
        glGenBuffers(1, &ibo_indices);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo_indices);
        glBufferData(
            GL_ELEMENT_ARRAY_BUFFER, indices.size() * sizeof(unsigned int), indices.data(),
            GL_STATIC_DRAW
        );
#endif
    }

    void renderTorusWithVBO() {
        if (use_texture) {
            glEnable(GL_TEXTURE_2D);
            glBindTexture(GL_TEXTURE_2D, texture.get());
        }

        glEnableClientState(GL_VERTEX_ARRAY);
        glEnableClientState(GL_NORMAL_ARRAY);

        if (use_texture) {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        }

        glBindBuffer(GL_ARRAY_BUFFER, vbo_vertices);
        glVertexPointer(3, GL_FLOAT, 0, nullptr);

        glBindBuffer(GL_ARRAY_BUFFER, vbo_normals);
        glNormalPointer(GL_FLOAT, 0, nullptr);

        if (use_texture) {
            glBindBuffer(GL_ARRAY_BUFFER, vbo_texcoords);
            glTexCoordPointer(2, GL_FLOAT, 0, nullptr);
        }

#if USE_IBO
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo_indices);
        glDrawElements(
            GL_QUADS, 4 * config::TORUS_STEPS * config::TORUS_STEPS, GL_UNSIGNED_INT, nullptr
        );
#else

        for (int i = 0; i < config::TORUS_STEPS; ++i) {
            glDrawArrays(
                GL_QUAD_STRIP, i * (config::TORUS_STEPS + 1), 2 * (config::TORUS_STEPS + 1)
            );
        }
#endif

        glDisableClientState(GL_VERTEX_ARRAY);
        glDisableClientState(GL_NORMAL_ARRAY);

        if (use_texture) {
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);
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
