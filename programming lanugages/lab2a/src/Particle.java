public class Particle {
    private int mass;
    public Particle(int mass) {
        System.out.println("Particle created");
        setMass(mass);
        System.out.printf("Current mass is %d\n", this.mass);
    }

    int getMass() {
        return this.mass;
    }

    void setMass(int mass) {
        if (mass < 0) {
            System.out.printf("Invalid particle mass (%d)\n", mass);
            return;
        }

        if (mass == 0) {
            System.out.println("Someone is trying to break physics??");
        }

        this.mass = mass;
    }
}

