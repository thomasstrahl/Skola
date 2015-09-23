package set;

public interface SimpleSet<E> extends Iterable<E> {
				
	/** 
	 * Adds the specified element to this set, if it is not already present.
	 * post: x is added to the set if it is not already present
	 * @param  x the element to be added
	 * @return true if the specified element was added
	 */
	boolean add(E x);

	/** 
	 * Removes the specified element from this set if it is present. 
	 * post: 	x is removed if it was present
	 * @param 	x the element to remove - if present
	 * @return true if the set contained the specified element
	 */
	boolean remove(Object x);

	/** 
	 * Returns true if this set contains the specified element.
	 * @param 	x the element whose presence is to be tested
	 * @return	true if this set contains the specified element
	 */
	boolean contains(Object x);

	/** 
	 * Returns true if this set contains no elements.
	 * @return true if this set contains no elements
	 */
	boolean isEmpty();

	/** 
	 * Returns the number of elements in this set.
	 * @return the number of elements in this set
	 */
	int size();
}

