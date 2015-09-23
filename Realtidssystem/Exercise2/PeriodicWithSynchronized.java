public class PeriodicWithSynchronized extends Thread {
	private int period;
	

	public PeriodicWithSynchronized(int period){
		this.period = period;
	}
	public static void main(String [] args){
		for(String arg:args){
			PeriodicWithSynchronized p = new PeriodicWithSynchronized(Integer.parseInt(arg));
			p.start();
		}
	}

	public void run(){

	System.out.println("Priority = " + getPriority());
	try {
		while(!Thread.interrupted()){
			synchronized(this){ 
				System.out.print(period);
				System.out.print(", ");
			}
			Thread.sleep(period);
	
		}
	} catch (InterruptedException e){
	
	}
	System.out.println("Thread stopped");

}
}
