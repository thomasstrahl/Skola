/**
 * 
 */
package computer;

/**
 * @author dat11gma
 *
 */
public class LongWord extends Word{

	private long data;
	/**
	 * 
	 */
	public LongWord(long data) {
		this.data = data;
	}
	public boolean equals(Word word){
		return data == word.value();
	}
	@Override
	public void add(Word word1, Word word2) {
		data = word1.value() + word2.value();
		
	}
	@Override
	public void mul(Word word1, Word word2) {
		data = word1.value() * word2.value();
		
	}
	@Override
	public void copy(Word word) {
		data = word.value();
		
	}
	@Override
	public Word getData(Memory memory) {
		return this;
	}
	public String toString(){
		return ""+data;
	}
	public long value(){
		return data;
	}
	@Override
	public String print() {
		return toString();
		
	}
}
