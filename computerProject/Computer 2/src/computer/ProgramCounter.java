/**
 * 
 */
package computer;

/**
 * @author dat11gma
 *
 */
public class ProgramCounter {

	/**
	 * 
	 */
	private int counter;

	/**
	 * 
	 */
	public ProgramCounter() {
		counter = 0;
	}

	/**
	 * 
	 */
	public void increase() {
		counter++;
	}

	/**
	 * 
	 */
	public void halt() {
		counter = -1;
	}

	/**
	 * 
	 */
	public void jump(int i) {
		counter = i;
	}
	public boolean ishalted(){
		if(counter == -1){
			return true;
		}
		return false;
	}
	public int getCount(){
		return counter;
	}
}
