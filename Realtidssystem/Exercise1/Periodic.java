public class Periodic extends Thread {
	private int period;
	

	public Periodic(int period){
		this.period = period;
	}
	public static void main(String [] args){
		for(String arg:args){
			Periodic p = new Periodic(Integer.parseInt(arg));
			p.start();
		}
	}

	public void run(){

	System.out.println("Priority = " + getPriority());
	try {
		while(!Thread.interrupted()){
		
			System.out.print(period);
			System.out.print(", ");
	
			Thread.sleep(period);
	
		}
	} catch (InterruptedException e){
	
	}
	System.out.println("Thread stopped");

}
}
