/**
 * 
 */
package command;
import computer.*;

/**
 * @author dat11gma
 *
 */
public class Copy implements Command{

	/**
	 * 
	 */
	private computer.Operand data;
	/**
	 * 
	 */
	private computer.Operand copyTo;
	/**
	 * 
	 */
	public Copy(Operand data, Operand copyTo){
		this.data = data;
		this.copyTo = copyTo;
	}
	/**
	 * 
	 */
	public void execute(Memory memory, ProgramCounter counter) {
		Word word = data.getData(memory);
		Word to = copyTo.getData(memory);
		to.copy(word);
		counter.increase();
	}
	@Override
	public void print(int index) {
		System.out.println(index +" CPY "+ data.print() + copyTo.print());		
	}

}
