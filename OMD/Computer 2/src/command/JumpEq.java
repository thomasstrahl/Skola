/**
 * 
 */
package command;

import computer.*;

/**
 * @author dat11gma
 *
 */
public class JumpEq implements Command{

	/**
	 * 
	 */
	private int jumpTo;
	/**
	 * 
	 */
	private computer.Operand input1;

	/**
	 * 
	 */
	private computer.Operand input2;

	/**
	 * 
	 */
	public JumpEq(int jumpTo, Operand input1, Operand input2) {
		this.jumpTo = jumpTo;
		this.input1 = input1;
		this.input2 = input2;
	}

	/* (non-Javadoc)
	 * @see command.Command#execute()
	 */
	@Override
	public void execute(Memory memory, ProgramCounter counter) {
		Word word1 = input1.getData(memory);
		Word word2= input2.getData(memory);
		if(word1.equals(word2)){
			counter.jump(jumpTo);
		}
		else{
			counter.increase();
		}
	}

	@Override
	public void print(int index) {
		System.out.println(index +" JEQ "+ jumpTo +input1.print()+ input2.print());
		
	}

}
