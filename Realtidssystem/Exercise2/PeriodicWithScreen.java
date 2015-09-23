public class PeriodicWithScreen extends Thread {
	private int period;
	private Screen s;

	public PeriodicWithScreen(int period, Screen s){
		this.s = s;
		this.period = period;
	}
	public static void main(String [] args){
		Screen s = new Screen();
		for(String arg:args){
			PeriodicWithScreen p = new PeriodicWithScreen(Integer.parseInt(arg),s);
			p.start();
		}
	}

	public void run(){

	System.out.println("Priority = " + getPriority());
	try {
		while(!Thread.interrupted()){
			
				s.writePeriod(period);
			
			Thread.sleep(period);
	
		}
	} catch (InterruptedException e){
	
	}
	System.out.println("Thread stopped");

}
}
