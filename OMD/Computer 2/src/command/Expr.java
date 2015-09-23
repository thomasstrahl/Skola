/**
 * 
 */
package command;
import computer.*;

/**
 * @author dat11gma
 *
 */
public abstract class Expr implements Command {

	/**
	 * 
	 */
	protected Operand input1;
	/**
	 * 
	 */
	protected Operand input2;
	/**
	 * 
	 */
	protected Operand saveSlot;

	/**
	 * 
	 */
	public Expr(Operand input1, Operand input2, Operand saveSlot){
		this.saveSlot = saveSlot;
		this.input1 = input1;
		this.input2 = input2;
	}
	/* (non-Javadoc)
	 * @see command.Command#execute()
	 */
	@Override
	public void execute(Memory memory, ProgramCounter counter) {
		value(memory);
		counter.increase();

	}

	/**
	 * 
	 */
	public abstract void value(Memory memory);

}
