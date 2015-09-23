package queue;
import java.util.*;

public class FifoQueue<E> extends AbstractQueue<E> 
implements Queue<E> {
	private QueueNode<E> last;
	private int size;

	public FifoQueue() {
		size=0;
		last = null;
	}

	/**	
	 * Returns an iterator over the elements in this queue
	 * @return an iterator over the elements in this queue
	 */	
	public Iterator<E> iterator() {
		
		return new QueueIterator();
	}
	private class QueueIterator implements Iterator<E>{
		private QueueNode<E> pos;
		private int n;
		
		private QueueIterator(){
			n = 0;
			if(size != 0){
				
				pos= last.next;	
			}
		}
		public boolean hasNext(){
			if(size == 0 || n >= size){
				return false;
			}
			return true;
		}
		public E next(){
			if(!hasNext()){
				throw new NoSuchElementException();
				
			}
			n++;
			QueueNode<E> temp = pos;
			pos = pos.next;
			return temp.element;
			
		}
		public void remove(){
			throw new UnsupportedOperationException();
			
		}
	}

	
	/**	
	 * Returns the number of elements in this queue
	 * @return the number of elements in this queue
	 */
	public int size() {		
		return size;
	}

	/**	
	 * Inserts the specified element into this queue, if possible
	 * post:	The specified element is added to the rear of this queue
	 * @param	x the element to insert
	 * @return	true if it was possible to add the element 
	 * 			to this queue, else false
	 */
	public boolean offer(E x) {
		QueueNode<E> node = new QueueNode <E> (x);
		if(size==0){
			last = node;
			last.next = node;
			
			size++;
			return true;
		}
		else{
		node.next = last.next;
		last.next = node;	
		last = node;
		size++;
		return true;
		}
	}

	/**	
	 * Retrieves and removes the head of this queue, 
	 * or null if this queue is empty.
	 * post:	the head of the queue is removed if it was not empty
	 * @return 	the head of this queue, or null if the queue is empty 
	 */
	public E poll() {
		
		if(size==0){
			return null;
			}
		QueueNode<E> temp = last.next;
		if(size == 1){
			size --;
			last = null;
			
			return temp.element;
		}
		last.next = temp.next;
		size--;
		
		
		
		
		return temp.element;
	}

	/**	
	 * Retrieves, but does not remove, the head of this queue, 
	 * returning null if this queue is empty
	 * @return 	the head element of this queue, or null 
	 * 			if this queue is empty
	 */
	public E peek() {
		if(size==0){
		return null;
		}
		
		return last.next.element;
	}
	
	public void append(FifoQueue<E> q) {
		
		
		
		if(size==0) {
			last=q.last;
			
			}
		
		else if(size!=0 && q.size!=0) {
		
		QueueNode<E> temp = last.next;
		
		last.next = q.last.next;
		q.last.next = temp;
		
		last=q.last;
		
		}
		size+= q.size();
		q.last = null;
		q.size = 0;
		
	}


	private static class QueueNode<E> {
		E element;
		QueueNode<E> next;

		private QueueNode(E x) {
			element = x;
			next = null;
		}

	}
	
	

}
