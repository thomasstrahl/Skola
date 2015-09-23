package mountain;

public class Side {
	private Point p1;
	private Point p2;
	private Point m;
	
	
	public Side (Point p1, Point p2, Point m) {
		this.p1= p1;
		this.p2= p2;
		this.m= m;
	}
	
	public Point getP1 () {
		return p1;
	}
	
	public Point getP2 () {
		return p2;
	}
	
	public Point getM () {
		return m;
	}

}
