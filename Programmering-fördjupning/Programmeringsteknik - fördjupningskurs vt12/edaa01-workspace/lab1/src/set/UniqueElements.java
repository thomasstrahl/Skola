package set;


public class UniqueElements {

	
	public static int[] uniqueElements(int [] ints) {
		
		MaxSet<Integer> ms= new MaxSet<Integer>();
		for(int i=0; i < ints.length; i++) {
			ms.add(ints[i]);
			
		}
		
		int [] v = new int [ms.size()];
		
		for (int i = v.length-1; i >= 0 ; i--) {
		
			v[i] = ms.getMax();
			ms.remove(v[i]);
			
		}
		return v;
	}
	

}
