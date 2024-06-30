#include <iostream>
#include <vector>

using namespace std;

class Particle {
private:
    int mass;
public:
    Particle(int mass) {
        cout << "Particle created\n";
        setMass(mass);
        cout << "Current mass is " << mass << "\n";
    }

    int getMass() { return mass; }

    void setMass(int mass) {
        if (mass < 0) {
            cout << "Invalid particle mass (" << mass << ")\n";
            return;
        }

        if (mass == 0) {
            cout << "Someone is trying to break physics??\n";
        }

        this->mass = mass;
    }
};

class Universe {
private:
    static int count;
    vector<Particle> particles;
public:
    Universe() {
        cout << "Universe created\n";
        cout << "Currently multiverse has " << count << " particles\n";
    }

    void addParticle(Particle &p) {
        particles.push_back(p);
        ++count;
    }

    int getParticlesCount() { return count; }
        double computeAverageMass() {
        if (this->particles.size() == 0) {
            cout << "Oops, your universe has no particles, so average mass can't be calculated\n";
            return -1e10;
        }

        double totalMass = 0;
        for (int i = 0; i < this->particles.size(); i++) {
            totalMass += this->particles[i].getMass();
        }

        return totalMass / particles.size();
    }
};

int Universe::count = 0;

int main() {
    Universe u;

    srand(time(NULL));

    int n = rand() % 20;
    for (int i = 0; i < n; ++i) {
        int mass = 5 + rand() % 100;
        Particle p(mass);
        u.addParticle(p);
        cout << "Current average mass: " << u.computeAverageMass() << " ("
             << u.getParticlesCount() << " particle)\n";
    }
}
