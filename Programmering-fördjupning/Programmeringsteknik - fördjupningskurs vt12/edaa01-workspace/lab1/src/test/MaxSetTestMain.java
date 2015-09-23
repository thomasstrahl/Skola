package test;

import set.ArraySet;
import set.MaxSet;
import set.UniqueElements;

public class MaxSetTestMain {


	public static void main(String[] args) {
		
		MaxSet<Integer> s = new MaxSet<Integer>();
		
		
		for (int i = 1; i <= 1000; i++) {
			s.add(i);
		}
		ArraySet<Integer> s2 = new ArraySet<Integer>();
		for (int i = 1000; i <= 2000; i++) {
			s2.add(i);
		}
		s.addAll(s2);
		
		System.out.println(s.getMax());

	
	
	ArraySet<Integer> a = new ArraySet<Integer>();
	
	
	for (int i = 1; i <= 1000; i++) {
		a.add(i);
	}
	ArraySet<Integer> a2 = new ArraySet<Integer>();
	for (int i = 1000; i <= 2000; i++) {
		a2.add(i);
	}
	a.addAll(a2);
	
	System.out.println(a.size());

	
	int [] v = {7, 5, 3, 5, 2, 2, 7};
	
	int [] q = UniqueElements.uniqueElements(v);
	
	
	for(int i=0; i<q.length;i++){
		System.out.println(q[i]);
	}
	}
}


