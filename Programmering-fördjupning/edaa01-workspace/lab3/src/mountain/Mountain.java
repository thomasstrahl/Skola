package mountain;

import fractal.*;

public class Mountain extends Fractal {
	private Point a;
	private Point b;
	private Point c;
	

	/**
	 * Creates an object that handles Mountain fractal.
	 * 
	 * @param length
	 *            the length of the triangle side
	 */
	public Mountain(Point a, Point b, Point c) {
		
		super();
		this.a = a;
		this.b = b;
		this.c = c;
	
	}

	/**
	 * Returns the title.
	 * 
	 * @return the title
	 */
	public String getTitle() {
		return "Bergsfraktal";
	}

	/**
	 * Draws the fractal.
	 * 
	 * @param turtle
	 *            the turtle graphic object
	 */
	public void draw(TurtleGraphics turtle) {

		fractalLine(turtle, order, a, b, c);
		

	}

	/*
	 * Reursive method: Draws a recursive line of the triangle.
	 */
	private void fractalLine(TurtleGraphics turtle, int order, Point a,
			Point b, Point c) {

		if (order == 0) {
			turtle.moveTo(a.getX(), a.getY());
			turtle.forwardTo(b.getX(), b.getY());
			turtle.forwardTo(c.getX(), c.getY());
			turtle.forwardTo(a.getX(), a.getY());
		} else {
			Point d = new Point((a.getX() + b.getX()) / 2,
					(a.getY() + b.getY()) / 2);
			Point e = new Point((b.getX() + c.getX()) / 2,
					(b.getY() + c.getY()) / 2);
			Point f = new Point((c.getX() + a.getX()) / 2,
					(c.getY() + a.getY()) / 2);

			fractalLine(turtle, order - 1, d, e, f);
			fractalLine(turtle, order - 1, a, d, f);
			fractalLine(turtle, order - 1, d, b, e);
			fractalLine(turtle, order - 1, f, e, c);

		}

	}

}
