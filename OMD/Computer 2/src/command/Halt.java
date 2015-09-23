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
public class Halt implements Command{

	/**
	 * 
	 */
	public Halt() {
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see command.Command#execute()
	 */
	@Override
	public void execute(Memory memory, ProgramCounter counter) {
		counter.halt();

	}

	@Override
	public void print(int index) {
		System.out.println(index +" HLT"+ '\n');
		
	}

}
