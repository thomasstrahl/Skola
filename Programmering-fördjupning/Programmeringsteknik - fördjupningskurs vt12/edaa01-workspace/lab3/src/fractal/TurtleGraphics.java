package fractal;
import java.awt.Graphics;

public class TurtleGraphics {
	private Graphics g;	
	private double x, y;
	private double alpha;
	private boolean isPenDown;

	/** Creates a turtle graphics object that can draw lines in a drawing area. 
	 * At start the turtle location is (0,0), the direction is north and he drawing pen is lowered.
	 * @param g the graphic object
	 */
	public TurtleGraphics (Graphics g) {
		this.g = g;
		x = 0;
		y = 0;
		alpha = 90;
		isPenDown = true;
	}

	/** The turtle moves to the location (newX,newY) without drawing.
	 * @param  newX the x coordinate of the new location
	 * @param  newY the y coordinate of the new location
	 */
	public void moveTo(double newX, double newY) {
		x = newX;
		y = newY;
	}

	/** The turtle moves to the location (newX,newY) and draws if the pen i lowered.
	 * @param  newX the x coordinate of the new location
	 * @param  newY the y coordinate of the new location
	 */
	public void forwardTo(double newX, double newY) {
		if (isPenDown) {
			g.drawLine((int) x, (int) y, (int) newX, (int) newY);
		}
		x = newX;
		y = newY;
	}

	/** The turtle moves n pixels forward and draws if the pen i lowered.
	 * @param  n the nbr of pixels to go forward
	 */
	public void forward(double n) {
		double oldX = x;
		double oldY = y;
		x = x + n * Math.cos(alpha * Math.PI / 180);
		y = y - n * Math.sin(alpha * Math.PI / 180);
		if (isPenDown) {
			g.drawLine((int) oldX,(int) oldY,(int) x,(int) y);
		}
	}

	/** 
	 * Returns the x coordinate. 
	 * @return the x coordinate
	 */
	public double getX() {
		return x;
	}

	/** 
	 * Returns the y coordinate. 
	 * @return the y coordinate
	 */
	public double getY() {
		return y;
	}

	/** 
	 * Sets the direction to alpha degrees.
	 * @param alpha the new direction
	 */
	public void setDirection(double alpha) {
		this.alpha = alpha;
	}

	/** 
	 * Turns the turtles direction beta degrees to the left.
	 * @param beta the number of degrees to turn left
	 */
	public void left(double beta) {
		alpha = alpha + beta;
	}

	/** 
	 * Turns the turtles direction beta degrees to the right.
	 * @param beta the number of degrees to turn right
	 */
	public void right(double beta) {
		alpha = alpha - beta;
	}

	/** 
	 * Returns the direction of the turtle.
	 * @return the direction of the turtle
	 */
	public double getDirection() {
		return alpha;
	}

	/** 
	 * Lifts the turtles pen.
	 * /
	public void penUp() {
		isPenDown = false;
	}

	/** Lowers the turtles pen. 
	 */
	public void penDown() {
		isPenDown = true;
	}

	/** 
	 * Returns the width of the turtles drawing area. 
	 * @return the width of the turtles drawing area.
	 */
	public int getWidth() {
		return (int) g.getClipBounds().getWidth();
	}

	/** 
	 * Returns the height of the turtles drawing area. 
	 * @return the height of the turtles drawing area.
	 */
	public int getHeight() {
		return (int) g.getClipBounds().getHeight();
	}

}
