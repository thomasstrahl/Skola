public class PIDParameters implements Cloneable {
	public double K;
	public double Ti;
	public double Tr;
	public double Td;
	public double N;
	public double Beta;
	public double H;
	public boolean integratorOn;
	
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException x) {
			return null;
		}
	}
}
