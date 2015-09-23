package fractal;

import mountain.*;
import koch.Koch;

public class FractalApplication {
	public static void main(String[] args) {
		Fractal[] fractals = new Fractal[3];
		fractals[0] = new Koch(300);
		Point a = new Point (100,500);
		Point b = new Point (200,300);
		Point c = new Point (400,400);
		fractals[1] = new Mountain (a,b,c);
		fractals[2] = new RealMountain (a,b,c);
	    new FractalView(fractals, "Fraktaler");
	}

}
