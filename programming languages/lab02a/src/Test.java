import java.util.Random;

public class Test {
    public static void main(String[] args) {
        Random rand = new Random();
        Universe uni = new Universe();
        int n = rand.nextInt(20);
        for (int i = 0; i < n; i++) {
            int mass = 5 + rand.nextInt(100);
            uni.addParticle(new Particle(mass));
            System.out.printf("Current average mass: %f (%d points)\n",
                    uni.computeAverageMass(), uni.getParticlesCount());
        }
    }
}

