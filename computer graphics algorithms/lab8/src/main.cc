#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <ctime>
#include <vector>
#include <array>
#include <random>
#include <string>
#include <iostream>

struct Vec3 {
    float x, y, z;
    Vec3(float x = 0.0f, float y = 0.0f, float z = 0.0f) : x(x), y(y), z(z) {}
    Vec3 operator*(float k) const { return { x * k, y * k, z * k }; }
    Vec3 operator+(const Vec3& other) const { return { x + other.x, y + other.y, z + other.z }; }
    Vec3 operator-(const Vec3& other) const { return { x - other.x, y - other.y, z - other.z }; }
    float dot(const Vec3& other) const { return x * other.x + y * other.y + z * z; }
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

class ShaderProgram {
    GLuint programId = 0;
    GLuint vertexShaderId = 0;
    GLuint fragmentShaderId = 0;

  public:
    ShaderProgram(const std::string& vertexShader, const std::string& fragmentShader) {
        vertexShaderId = glCreateShader(GL_VERTEX_SHADER);
        const char* vertexShaderSrc = vertexShader.c_str();
        glShaderSource(vertexShaderId, 1, &vertexShaderSrc, nullptr);
        glCompileShader(vertexShaderId);

        fragmentShaderId = glCreateShader(GL_FRAGMENT_SHADER);
        const char* fragmentShaderSrc = fragmentShader.c_str();
        glShaderSource(fragmentShaderId, 1, &fragmentShaderSrc, nullptr);
        glCompileShader(fragmentShaderId);

        programId = glCreateProgram();
        glAttachShader(programId, vertexShaderId);
        glAttachShader(programId, fragmentShaderId);
        glLinkProgram(programId);
    }
    ~ShaderProgram() {
        if (vertexShaderId) {
            glDeleteShader(vertexShaderId);
        }
        if (fragmentShaderId) {
            glDeleteShader(fragmentShaderId);
        }
        if (programId) {
            glDeleteProgram(programId);
        }
    }
    void use() const { glUseProgram(programId); }
    GLuint get() const { return programId; }
};

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
    ShaderProgram* shaderProgram = nullptr;

    const std::array<float, 16> mirror_matrix = { 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1 };

    const std::string vertexShaderSource = R"(
      #version 330 core
      layout(location = 0) in vec3 aPos;
      layout(location = 1) in vec3 aNormal;
      layout(location = 2) in vec2 aTexCoord;
      out vec3 FragPos;
      out vec3 Normal;
      out vec2 TexCoord;
      uniform mat4 model;
      uniform mat4 view;
      uniform mat4 projection;
      void main() {
          FragPos = vec3(model * vec4(aPos, 1.0));
          Normal = mat3(transpose(inverse(model))) * aNormal;
          TexCoord = aTexCoord;
          gl_Position = projection * view * vec4(FragPos, 1.0);
      }
    )";

    const std::string fragmentShaderSource = R"(
      #version 330 core
      in vec3 FragPos;
      in vec3 Normal;
      in vec2 TexCoord;
      out vec4 FragColor;
      uniform vec3 lightPos;
      uniform vec3 viewPos;
      uniform vec3 lightColor;
      uniform vec3 materialAmbient;
      uniform vec3 materialDiffuse;
      uniform vec3 materialSpecular;
      uniform float materialShininess;
      uniform bool useTexture;
      uniform sampler2D tex;

      void main() {
          vec3 ambient = materialAmbient * lightColor;

          vec3 norm = normalize(Normal);
          vec3 lightDir = normalize(lightPos - FragPos);
          float diff = max(dot(norm, lightDir), 0.0);
          vec3 diffuse = materialDiffuse * diff * lightColor;

          vec3 viewDir = normalize(viewPos - FragPos);
          vec3 reflectDir = reflect(-lightDir, norm);
          float spec = pow(max(dot(viewDir, reflectDir), 0.0), materialShininess);
          vec3 specular = materialSpecular * spec * lightColor;

          vec4 texColor = texture(tex, TexCoord);

          vec3 textureFactor = useTexture ? texColor.rgb : vec3(1.0);
          vec3 result = (ambient + diffuse + specular) * textureFactor;

          FragColor = vec4(result, 1.0);
      }
    )";

  public:
    Application()
        : torus(
              config::TORUS_MINOR_RADIUS_A, config::TORUS_MINOR_RADIUS_B, config::TORUS_MAJOR_RADIUS
          ) {}

    ~Application() {
        if (window) {
            glfwTerminate();
        }

        if (shaderProgram) {
            delete shaderProgram;
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

        if (glewExperimental = GL_TRUE; glewInit() != GLEW_OK) {
            return false;
        }

        shaderProgram = new ShaderProgram(vertexShaderSource, fragmentShaderSource);
        setupTextures();

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

        texture = Texture(config::TEXTURE_SIZE);

        glDisable(GL_TEXTURE_2D);
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
        glClearColor(0.1f, 0.1f, 0.1f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glEnable(GL_DEPTH_TEST);

        shaderProgram->use();

        GLfloat modelViewMatrix[16];
        glGetFloatv(GL_MODELVIEW_MATRIX, modelViewMatrix);
        GLfloat projectionMatrix[16];
        glGetFloatv(GL_PROJECTION_MATRIX, projectionMatrix);

        GLint modelLoc = glGetUniformLocation(shaderProgram->get(), "model");
        glUniformMatrix4fv(modelLoc, 1, GL_FALSE, modelViewMatrix);

        GLint viewLoc = glGetUniformLocation(shaderProgram->get(), "view");
        glUniformMatrix4fv(viewLoc, 1, GL_FALSE, modelViewMatrix);

        GLint projLoc = glGetUniformLocation(shaderProgram->get(), "projection");
        glUniformMatrix4fv(projLoc, 1, GL_FALSE, projectionMatrix);

        GLint lightPosLoc = glGetUniformLocation(shaderProgram->get(), "lightPos");
        glUniform3f(
            lightPosLoc, config::LIGHT_POSITION.x, config::LIGHT_POSITION.y,
            config::LIGHT_POSITION.z
        );

        GLint viewPosLoc = glGetUniformLocation(shaderProgram->get(), "viewPos");
        glUniform3f(viewPosLoc, 0.0f, 0.0f, 3.0f);

        GLint lightColorLoc = glGetUniformLocation(shaderProgram->get(), "lightColor");
        glUniform3f(
            lightColorLoc, config::LIGHT_COLOR[0], config::LIGHT_COLOR[1], config::LIGHT_COLOR[2]
        );

        GLint matAmbientLoc = glGetUniformLocation(shaderProgram->get(), "materialAmbient");
        glUniform3f(
            matAmbientLoc, config::MATERIAL_AMBIENT[0], config::MATERIAL_AMBIENT[1],
            config::MATERIAL_AMBIENT[2]
        );

        GLint matDiffuseLoc = glGetUniformLocation(shaderProgram->get(), "materialDiffuse");
        glUniform3f(
            matDiffuseLoc, config::MATERIAL_DIFFUSE[0], config::MATERIAL_DIFFUSE[1],
            config::MATERIAL_DIFFUSE[2]
        );

        GLint matSpecularLoc = glGetUniformLocation(shaderProgram->get(), "materialSpecular");
        glUniform3f(matSpecularLoc, 0.8f, 0.8f, 0.8f);

        GLint matShineLoc = glGetUniformLocation(shaderProgram->get(), "materialShininess");
        glUniform1f(matShineLoc, 32.0f);

        GLint useTexLoc = glGetUniformLocation(shaderProgram->get(), "useTexture");
        glUniform1i(useTexLoc, use_texture ? 1 : 0);

        if (use_texture) {
            glActiveTexture(GL_TEXTURE0);
            glBindTexture(GL_TEXTURE_2D, texture.get());
            GLint texLoc = glGetUniformLocation(shaderProgram->get(), "tex");
            glUniform1i(texLoc, 0);
        }

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

                glVertexAttrib3f(1, normal.x, normal.y, normal.z);

                const float s = i / float(2 * config::TORUS_STEPS);
                const float t = j / float(2 * config::TORUS_STEPS);
                const float ds = 1.0f / (2 * config::TORUS_STEPS);

                glTexCoord2f(s, t);
                glVertex3f(A.x, A.y, A.z);

                glTexCoord2f(s + ds, t);
                glVertex3f(B.x, B.y, B.z);

                glTexCoord2f(s + ds, t + ds);
                glVertex3f(C.x, C.y, C.z);

                glTexCoord2f(s, t + ds);
                glVertex3f(D.x, D.y, D.z);
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
