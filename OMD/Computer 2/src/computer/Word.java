/**
 * 
 */
package computer;


/**
 * @author dat11gma
 *
 */
public abstract class Word implements Operand{

	/**
	 * 
	 */
	public Word() {
	}

	/**
	 * 
	 */
	public abstract void add(Word word1, Word word2);
	
	public abstract void mul(Word word1, Word word2);
	
	public abstract boolean equals(Word word);
	
	public abstract void copy(Word word);
	
	public abstract long value();
}
