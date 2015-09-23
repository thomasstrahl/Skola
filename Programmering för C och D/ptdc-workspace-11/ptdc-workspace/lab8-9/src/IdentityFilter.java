import java.awt.Color;
import se.lth.cs.ptdc.images.ImageFilter;

/** IdentityFilter beskriver en identitetstransformation */
public class IdentityFilter extends ImageFilter {
	/** skapa ett filterobjekt med namnet name */
	public IdentityFilter(String name) {
		super(name);
	}
	
	/** filtrera bilden i matrisen inPixels och returnera resultatet i
		en ny matris. Utnyttja eventuellt värdet av parameter */
	public Color[][] apply(Color[][] inPixels, double paramValue) {
		int height = inPixels.length;
		int width = inPixels[0].length;
		Color[][] outPixels = new Color[height][width];
		for (int i = 0; i < height; ++i) {
			for (int j = 0; j < width; ++j) {
				Color pixel = inPixels[i][j];			
				outPixels[i][j] = new Color(pixel.getRed(), 
										    pixel.getGreen(),
										    pixel.getBlue());
			}
		}
		return outPixels;
	}
}
