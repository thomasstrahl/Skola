/**
 * 
 */
package computer;

import command.Command;

/**
 * @author dat11gma
 *
 */
public abstract class Program extends java.util.ArrayList<Command>{

	/**
	 * 
	 */

	/**
	 * 
	 */
	public Program() {
	}

	/**
	 * 
	 */
	public void run(Memory memory, ProgramCounter counter) {
		while(!counter.ishalted()){
			this.get(counter.getCount()).execute(memory, counter);
		}
	}
	public void print(){
		for(Object c : this.toArray()){
			((Command)c).print(this.indexOf(c));
		}
	}


}
