package mountain;

import fractal.*;
import java.util.LinkedList;

public class RealMountain extends Fractal {
	private Point a;
	private Point b;
	private Point c;
	private double dev;
	private LinkedList<Side> side;

	/**
	 * Creates an object that handles Mountain fractal.
	 * 
	 * @param length
	 *            the length of the triangle side
	 */
	public RealMountain(Point a, Point b, Point c) {

		super();
		this.a = a;
		this.b = b;
		this.c = c;
		dev = 50;
		side = new LinkedList<Side>();

	}

	/**
	 * Returns the title.
	 * 
	 * @return the title
	 */
	public String getTitle() {
		return "Verklig Bergsfraktal";
	}

	/**
	 * Draws the fractal.
	 * 
	 * @param turtle
	 *            the turtle graphic object
	 */
	public void draw(TurtleGraphics turtle) {

		fractalLine(turtle, order, a, b, c, dev);

	}

	/*
	 * Reursive method: Draws a recursive line of the triangle.
	 */
	private void fractalLine(TurtleGraphics turtle, int order, Point a,
			Point b, Point c, double dev) {

		if (order == 0) {
			turtle.moveTo(a.getX(), a.getY());
			turtle.forwardTo(b.getX(), b.getY());
			turtle.forwardTo(c.getX(), c.getY());
			turtle.forwardTo(a.getX(), a.getY());

		} else {

//			Point d = new Point((a.getX() + b.getX()) / 2, ((a.getY() + b
//					.getY()) + RandomUtilities.randFunc(dev)) / 2);
//
//			Point e = new Point((b.getX() + c.getX()) / 2, ((b.getY() + c
//					.getY()) + RandomUtilities.randFunc(dev)) / 2);
//
//			Point f = new Point((c.getX() + a.getX()) / 2, ((c.getY() + a
//					.getY()) + RandomUtilities.randFunc(dev)) / 2);

//			side.add(new Side(a, b, d));
//			side.add(new Side(b, c, e));
//			side.add(new Side(c, a, f));
			
			Point d = getMiddle(a, b, dev);
			Point e = getMiddle(b, c, dev);
			Point f = getMiddle(a, c, dev);
			

			fractalLine(turtle, order - 1, d, e, f, dev / 2);
			fractalLine(turtle, order - 1, a, d, f, dev / 2);
			fractalLine(turtle, order - 1, d, b, e,	dev / 2);
			fractalLine(turtle, order - 1, f, e, c,	dev / 2);
			
			

		}
	}

	private Point getMiddle(Point a, Point b, double dev) {

		Point temp;
		for (int i = 0; i < side.size(); i++) {
			if (side.get(i).getP1() == a && side.get(i).getP2() == b || side.get(i).getP1() == b && side.get(i).getP2() == a) {
				temp = side.get(i).getM();
				side.remove(i);
				return temp;

			}

		}
		
		temp = new Point((a.getX() + b.getX()) / 2,
				((a.getY() + b.getY()) + RandomUtilities.randFunc(dev)) / 2);
		
		side.add(new Side(a, b, temp));
		
		return temp; 

	}
}
