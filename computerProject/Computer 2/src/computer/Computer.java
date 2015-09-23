/**
 * 
 */
package computer;


/**
 * @author dat11gma
 *
 */
public class Computer {


	private Program program;
	private ProgramCounter counter;
	private Memory memory;


	
	public Computer(Memory memory) {
		this.memory = memory;
		counter = new ProgramCounter();
	}

	/**
	 * 
	 */
	public void load(Program program) {
		this.program = program;
	}

	/**
	 * 
	 */
	public void run() {
		program.run(memory, counter);
	}

}
