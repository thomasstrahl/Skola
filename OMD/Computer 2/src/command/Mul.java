/**
 * 
 */
package command;
import computer.*;
/**
 * @author dat11gma
 *
 */
public class Mul extends Expr {
	


	public Mul(Operand input1, Operand input2, Operand saveSlot) {
		super(input1, input2, saveSlot);
		// TODO Auto-generated constructor stub
	}

	/**
	 * 
	 */
	public void value(Memory memory) {
		Word save = saveSlot.getData(memory);
		Word word1 = input1.getData(memory);
		Word word2 = input2.getData(memory);
		save.mul(word1, word2);
	}

	@Override
	public void print(int index) {
		System.out.println(index +" MUL"+input1.print() + input2.print() + saveSlot.print());
		
	}

}
