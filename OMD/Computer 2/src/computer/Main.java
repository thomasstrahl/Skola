package computer;

public class Main {

	public static void main(String[] args) {
		Program factorial = new factorial();
		factorial.print();
		Computer computer = new Computer(new LongMemory(64));
		computer.load(factorial);
		computer.run();
	}
}