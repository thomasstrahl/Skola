/**
 * 
 */
package computer;

/**
 * @author dat11gma
 *
 */
public class Address implements Operand{

	private int memoryslot;
	/**
	 * 
	 */
	public Address(int slot) {
		memoryslot = slot;
	}
	/**
	 * 
	 */
	public computer.Word getData(Memory memory) {
		return memory.getData(memoryslot);
	}
	@Override
	public String print() {
		return " ["+memoryslot+"] ";
		
	}

}
