package command;

import computer.*;

public interface Command {

	/**
	 * 
	 */
	public void execute(Memory memory, ProgramCounter counter);
	public void print(int index);

}
