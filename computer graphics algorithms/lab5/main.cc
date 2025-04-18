#include <boost/geometry.hpp>
#include <boost/geometry/geometries/linestring.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/polygon.hpp>
#include <cmath>
#include <GLFW/glfw3.h>
#include <vector>

namespace bg = boost::geometry;

using Point = bg::model::d2::point_xy<double>;
using Linestring = bg::model::linestring<Point>;
using Polygon = bg::model::polygon<Point>;

const int WIDTH = 800;
const int HEIGHT = 600;
const double EPSILON = 1e-6;

bool enter_pressed = false;
bool done_first = false;
bool done_second = false;
bool space_pressed = false;

struct PolygonData {
    std::vector<Point> subject;
    std::vector<Point> clipper;
    std::vector<Point> result;
};

PolygonData data;

bool pointsEqual(const Point& a, const Point& b) {
    return bg::distance(a, b) < EPSILON;
}

void findAllIntersections(
    const std::vector<Point>& poly1, const std::vector<Point>& poly2,
    std::vector<Point>& intersection_points
) {
    for (size_t i = 0; i < poly1.size(); ++i) {
        size_t next_i = (i + 1) % poly1.size();
        const Point& a1 = poly1[i];
        const Point& a2 = poly1[next_i];

        for (size_t j = 0; j < poly2.size(); ++j) {
            size_t next_j = (j + 1) % poly2.size();
            const Point& b1 = poly2[j];
            const Point& b2 = poly2[next_j];

            Linestring line1 { a1, a2 };
            Linestring line2 { b1, b2 };

            std::vector<Point> output;
            bg::intersection(line1, line2, output);

            if (!output.empty()) {
                bool alreadyExists = false;
                for (const auto& existing : intersection_points) {
                    if (pointsEqual(existing, output[0])) {
                        alreadyExists = true;
                        break;
                    }
                }

                if (!pointsEqual(output[0], a1) &&
                    !pointsEqual(output[0], a2) &&
                    !pointsEqual(output[0], b1) &&
                    !pointsEqual(output[0], b2) && !alreadyExists) {
                    intersection_points.push_back(output[0]);
                }
            }
        }
    }
}

std::vector<Point> weilerAtherton(
    const std::vector<Point>& subject, const std::vector<Point>& clipper
) {
    if (subject.size() < 3 || clipper.size() < 3) {
        return {};
    }

    Polygon subj_poly, clip_poly;
    for (const auto& pt : subject) {
        bg::append(subj_poly.outer(), pt);
    }

    for (const auto& pt : clipper) {
        bg::append(clip_poly.outer(), pt);
    }

    bg::correct(subj_poly);
    bg::correct(clip_poly);

    if (bg::within(subj_poly, clip_poly)) {
        return subject;
    }

    if (bg::within(clip_poly, subj_poly)) {
        return clipper;
    }

    if (!bg::intersects(subj_poly, clip_poly)) {
        return {};
    }

    std::vector<Point> intersection_points;
    findAllIntersections(subject, clipper, intersection_points);

    if (intersection_points.empty()) {
        return {};
    }

    std::vector<Polygon> output;
    bg::intersection(subj_poly, clip_poly, output);

    if (!output.empty()) {
        return output[0].outer();
    }

    return {};
}

void renderPolygon(const std::vector<Point>& points) {
    if (points.size() < 2) {
        return;
    }

    glBegin(GL_POLYGON);
    for (const auto& p : points) {
        glVertex2d(p.x(), p.y());
    }
    glEnd();
}

void display() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, WIDTH, HEIGHT, 0, -1, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    if (done_first) {
        glColor3f(1.0f, 0.0f, 0.0f);
        renderPolygon(data.subject);
    }

    if (done_second) {
        glColor3f(0.0f, 1.0f, 0.0f);
        renderPolygon(data.clipper);
    }

    if (space_pressed) {
        glColor3f(0.0f, 0.0f, 1.0f);
        renderPolygon(data.result);
    }
}

void mouseButtonCallback(GLFWwindow* window, int button, int action, int mods) {
    if (button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS) {
        double x, y;
        glfwGetCursorPos(window, &x, &y);

        if (!enter_pressed) {
            data.subject.emplace_back(x, y);
        } else if (!done_second) {
            data.clipper.emplace_back(x, y);
        }
    }
}

void keyCallback(
    GLFWwindow* window, int key, int scancode, int action, int mods
) {
    if (action != GLFW_PRESS) {
        return;
    }

    if (key == GLFW_KEY_ENTER) {
        if (!enter_pressed) {
            enter_pressed = true;
            done_first = true;
        } else if (!done_second) {
            done_second = true;
        }
    } else if (key == GLFW_KEY_SPACE && done_first && done_second) {
        space_pressed = !space_pressed;
        if (space_pressed) {
            data.result = weilerAtherton(data.subject, data.clipper);
        }
    }
}

int main() {
    if (!glfwInit()) {
        return -1;
    }

    GLFWwindow* window = glfwCreateWindow(WIDTH, HEIGHT, "lab5", NULL, NULL);
    if (!window) {
        glfwTerminate();
        return -1;
    }

    glfwMakeContextCurrent(window);
    glfwSetMouseButtonCallback(window, mouseButtonCallback);
    glfwSetKeyCallback(window, keyCallback);

    while (!glfwWindowShouldClose(window)) {
        display();
        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    glfwDestroyWindow(window);
    glfwTerminate();
}
