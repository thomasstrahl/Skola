package set;
import java.util.ArrayList;
import java.util.Iterator;


public class ArraySet<E> implements SimpleSet<E>  {
	private ArrayList<E> set;
	
	/**
	 * Constructs a new empty set.
	 */
	public ArraySet() {
		set= new ArrayList<E>();

	}

	/** 
	 * Adds the specified element to this set, if it is not already present.
	 * post: x is added to the set if it is not already present
	 * @param  x the element to be added
	 * @return true if the specified element was added
	 */
	public boolean add(E x) {
		
		int n=0;
		while(n<set.size()){
			
			if(set.get(n).equals(x)) {
				
				return false;				
				
			}
			n++;
		}
		
		set.add(x);
		return true;
		
		
	}

	/** 
	 * Removes the specified element from this set if it is present. 
	 * post: 	x is removed if it was present
	 * @param 	x the element to remove - if present
	 * @return true if the set contained the specified element
	 */
	public boolean remove(Object x) {
		
		int n=0;
		while(n<set.size()){
			
			if(set.get(n).equals(x)) {
				
				set.remove(x);
				return true;
			}
			n++;
		}
		
		return false;
	}

	/** 
	 * Returns true if this set contains the specified element.
	 * @param 	x the element whose presence is to be tested
	 * @return	true if this set contains the specified element
	 */
	public boolean contains(Object x) {	
		
		int n=0;
		while(n<set.size()){
			
			if(set.get(n).equals(x)) {
				
				return true;
			}
			n++;
		}
		
		return false;
		
	}


	/** 
	 * Returns true if this set contains no elements.
	 * @return true if this set contains no elements
	 */
	public boolean isEmpty() {
		
		if(set.size()==0){
			return true;
		}
		
		return false;
	}

	/** 
	 * Returns the number of elements in this set.
	 * @return the number of elements in this set
	 */
	public int size() {
		return set.size();
	}

	/** 
	 * Returns an iterator over the elements in this set.
	 * @return an iterator over the elements in this set
	 */
	public Iterator<E> iterator() {
		
		Iterator<E> iterator = set.iterator();
		return iterator;
	}

	public boolean addAll(SimpleSet<? extends E> s){
		
		boolean change = false;
		
		Iterator<? extends E> iterator = s.iterator();
			
		while(iterator.hasNext()) {
				E temp= iterator.next();
				change |= add(temp);
			}
		
		return change;
		

	}
	
}
