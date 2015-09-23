public class MainBB {
	public static void main(String[] argv) {
		final int regulPriority = 8;
		
		BallAndBeam bb = new BallAndBeam();
		
		ReferenceGenerator refgen = new ReferenceGenerator(20.0, 4.0);
		BallAndBeamRegul regul = new BallAndBeamRegul(refgen, bb, regulPriority);
		
		refgen.start();
		regul.start();
	}
}
