/**
 * 
 */
package computer;
import command.*;

/**
 * @author dat11gma
 *
 */
@SuppressWarnings("serial")
public class factorial extends Program {
	public factorial() {
		Address n = new Address(0),
		fac = new Address(1);
		add(new Copy(new LongWord(5), n));
		add(new Copy(new LongWord(1), fac));
		add(new JumpEq(6, n, new LongWord(1)));
		add(new Mul(fac, n, fac));
		add(new Add(n, new LongWord(-1), n));
		add(new Jump(2));
		add(new Print(fac));
		add(new Halt());
	}
}
