/**
 * 
 */
package computer;


/**
 * @author dat11gma
 *
 */
public class LongMemory implements Memory{

	/**
	 * 
	 */
	private Word[] memory;

	/**
	 * 
	 */
	public LongMemory(int size) {
		memory = new Word[size];
	}

	@Override
	public void add(int saveSlot, Word data) {
		memory[saveSlot] = data;
		
	}

	@Override
	public Word getData(int i) {
		if(memory[i] == null){
			add(i, new LongWord(0));
		}
		return memory[i];
	}

}
