/**
 * 
 */
package command;
import computer.*;
/**
 * @author dat11gma
 *
 */
public class Print implements Command{

	/**
	 * 
	 */
	private Operand input;

	/**
	 * 
	 */
	public Print(Operand word){
		input = word;
	}

	/* (non-Javadoc)
	 * @see command.Command#execute()
	 */
	@Override
	public void execute(Memory memory, ProgramCounter counter) {
		Word word = input.getData(memory);
		System.out.print(word);
		counter.increase();
	}

	@Override
	public void print(int index) {
		System.out.println(index +" PRT"+input.print());
		
	}

}
