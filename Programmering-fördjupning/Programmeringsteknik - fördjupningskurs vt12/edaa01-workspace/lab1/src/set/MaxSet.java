package set;

import java.util.NoSuchElementException;
import java.util.Iterator;

public class MaxSet<E extends Comparable<E>> extends ArraySet<E> {
	private E maxElement;

	/**
	 * Constructs a new empty set.
	 */
	public MaxSet() {
		super();
	}

	/**
	 * Returns the currently largest element in this set. pre: the set is not
	 * empty post: the set is unchanged
	 * 
	 * @return the currently largest element in this set
	 * @throws NoSuchElementException
	 *             if this set is empty
	 */
	public E getMax() {
		if (super.isEmpty()) {
			throw new NoSuchElementException();
		}
		return maxElement;
	}

	/**
	 * Adds the specified element to this set, if it is not already present.
	 * post: x is added to the set if it is not already present
	 * 
	 * @param x
	 *            the element to be added
	 * @return true if the specified element was added
	 */
	public boolean add(E x) {
		if (isEmpty()) {
			maxElement = x;

		} else {

			int compare = maxElement.compareTo(x);
			if (compare < 0) {

				maxElement = x;

			}

		}

		return super.add(x);
	}

	/**
	 * Removes the specified element from this set if it is present. post: x is
	 * removed if it was present
	 * 
	 * @param x
	 *            the element to remove - if present
	 * @return true if the set contained the specified element
	 */
	public boolean remove(Object x) {
		
		
		E tempMax = maxElement;
		super.remove(x);
		
		if(super.size() != 0){
		E temp = super.iterator().next();
		if (tempMax.equals(x)) {
			
			Iterator <E> iterator= super.iterator();
			
			while (iterator.hasNext()) {
				
				E temp2 = iterator.next();

				int compare = temp.compareTo(temp2);
				if (compare < 0) {
					temp = temp2;

				}
			}

		}
		maxElement = temp;

		}
		return true;
	}

//	/**
//	 * Adds all of the elements in the specified set, for which it is possible,
//	 * to this set. post: all elements, for which it is possible, in the
//	 * specified set are added to this set.
//	 * 
//	 * @return true if this set changed as a result of the call
//	 */
//	public boolean addAll(SimpleSet<? extends E> c) {
//boolean change = false;
//		
//		Iterator<? extends E> iterator = c.iterator();
//			
//		while(iterator.hasNext()) {
//				E temp= iterator.next();
//				change |= add(temp);
//			}
//		
//		return change; 
//	}

}