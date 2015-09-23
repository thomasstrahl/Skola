/**
 * 
 */
package command;

import computer.Memory;
import computer.ProgramCounter;

/**
 * @author dat11gma
 *
 */
public class Jump implements Command{

	/**
	 * 
	 */
	private int jumpTo;

	/**
	 * 
	 */
	public Jump(int jumpTo) {
		this.jumpTo = jumpTo;
	}

	/* (non-Javadoc)
	 * @see command.Command#execute()
	 */
	@Override
	public void execute(Memory memory, ProgramCounter counter) {
		counter.jump(jumpTo);

	}

	@Override
	public void print(int index) {
		System.out.println(index +" JMP "+jumpTo);
		
		
	}

}
