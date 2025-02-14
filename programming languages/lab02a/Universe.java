import java.util.ArrayList;

public class Universe {
    private ArrayList<Particle> particles;
    private static int count;

    public Universe() {
        System.out.println("Universe created");
        System.out.printf("Currently multiverse has %d particles\n", count);
        this.particles = new ArrayList<Particle>();
    }

    void addParticle(Particle particle) {
        this.particles.add(particle);
        increaseParticlesCount();
    }

    static void increaseParticlesCount() {
        count += 1;
    }

    int getParticlesCount() {
        return count;
    }

    double computeAverageMass() {
        if (this.particles.size() == 0) {
            System.out.println("Oops, your universe has no particles, so average mass can't be calculated");
            return -Integer.MAX_VALUE;
        }

        double totalMass = 0;
        for (int i = 0; i < this.particles.size(); i++) {
            totalMass += this.particles.get(i).getMass();
        }

        return totalMass / this.particles.size();
    }
}
