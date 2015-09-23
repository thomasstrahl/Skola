package testqueue;

import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.Iterator;

import queue.FifoQueue;


public class TestAppendFifoQueue {
	private FifoQueue<Integer> myQueue;
	private FifoQueue<Integer> myQueue2;
	
	@Before
	public void setUp() throws Exception {
		myQueue = new FifoQueue<Integer>();
		myQueue2 = new FifoQueue<Integer>();
	}

	@After
	public void tearDown() throws Exception {
		myQueue= null;
		myQueue2 = null;
	}
	
	@Test
	public final void testTwoEmptyQueues() {
		myQueue.append(myQueue2);
		
		assertTrue("Wrong size after append", myQueue.size() == 0);
		
		assertTrue("Wrong size after append", myQueue2.size() == 0);
		
		
		
	}
	
	@Test
	public final void testMyQueueEmpty() {
		myQueue2.offer(1);
		myQueue2.offer(2);
		myQueue2.offer(3);
		
		myQueue.append(myQueue2);
		
		assertTrue("Wrong size after append", myQueue.size() == 3);
		
		assertTrue("Wrong size after append", myQueue2.size() == 0);
		
	
	}
	
	@Test
	public final void testMyQueue2Empty() {
		myQueue.offer(1);
		myQueue.offer(2);
		myQueue.offer(3);
		
		
		myQueue.append(myQueue2);
		
		assertTrue("Wrong size after append", myQueue.size() == 3);
		
		assertTrue("Wrong size after append", myQueue2.size() == 0);
	}
	
	@Test
	public final void testTwoQueues() {
		myQueue.offer(1);
		myQueue.offer(2);
		myQueue.offer(3);
		
		myQueue2.offer(4);
		myQueue2.offer(5);
		myQueue2.offer(6);
		
		
		myQueue.append(myQueue2);
		
		assertTrue("Wrong size after append", myQueue.size() == 6);
		
		assertTrue("Wrong size after append", myQueue2.size() == 0);
				
		
		for(int i=1; i<7; i++) {
			
			assertTrue("Wrong size after append", myQueue.poll() == i);
			
		}
		
		
		
		
	}
	

	
}

	
