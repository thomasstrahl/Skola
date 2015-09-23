package test;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import set.ArraySet;

public class TestArraySet {
	ArraySet<Integer> s;

	@Before
	public void setUp() throws Exception {
		s = new ArraySet<Integer>();
	}

	@After
	public void tearDown() throws Exception {
		s = null;
	}

	@Test
	public final void testEmpty() {
		assertTrue("isEmpty false for empty set", s.isEmpty());
		s.add(1);
		assertFalse("isEmpty true for non-empty set", s.isEmpty());
	}

	@Test
	public final void testOneElementSet() {
		s.add(1);
		assertEquals("Wrong size():", 1, s.size());
		assertTrue("Element not found in set: 1", s.contains(1));
	}

	@Test
	public final void testTwoDuplicatesWhereNotAllowed() {
		s.add(1);
		s.add(1);
		assertEquals("Wrong size() after duplicate add:", 1, s.size());
		assertTrue("Element not found in set: 1", s.contains(1));
	}

	@Test
	public final void testTwoDifferent() {
		s.add(1);
		s.add(2);
		assertEquals("Wrong size():", 2, s.size());
		assertTrue("Element not found in set: 1", s.contains(1));
		assertTrue("Element not found in set: 2", s.contains(2));
	}

	@Test
	public final void testManyDifferent() {
		for (int i = 1; i <= 1000; i++) {
			s.add(i);
		}
		assertEquals("Wrong size():", 1000, s.size());
		for (int i = 1; i <= 1000; i++) {
			assertTrue("Element not found in set:" + i, s.contains(i));
		}
	}

	@Test
	public final void testManyDuplicates() {
		for (int j = 1; j <= 10; j++) {
			for (int i = 1; i <= 1000; i++) {
				s.add(i);
			}
		}
		assertEquals("Wrong size():", 1000, s.size());
		for (int i = 1; i <= 1000; i++) {
			assertTrue("Element not found in set:" + i, s.contains(i));
		}
	}

	@Test
	public final void testRemoveFromEmpty() {
		assertFalse("remove for empty set returns true", s.remove(1));
	}

	@Test
	public final void testRemoveOneElement() {
		s.add(1);
		assertTrue("remove for one element set returns false", s.remove(1));
		assertTrue("isEmpty false for empty set after removal", s.isEmpty());
	}

	@Test
	public final void testRemoveSeveral() {
		for (int j = 1; j <= 10; j++) {
			for (int i = 1; i <= 1000; i++) {
				s.add(i);
			}
		}
		// remove 10 elements 10 times
		for (int i = 1; i <= 10; i++) {
			for (int j = 1; j <= 10; j++) {
				s.remove(j * 10);
			}
		}
		assertEquals("Wrong size():", 990, s.size());
	}

	@Test
	public final void testIterator() {
		for (int i = 1; i <= 10; i++) {
			s.add(i);
		}
		int j = 1;
		for (int i : s) {
			assertEquals("Wrong value from iterator:", j, i);
			j++;
		}
	}
	
	@Test
	public final void testAddAll() {
		ArraySet<Integer> set= new ArraySet<Integer>();
		s.add(1);
		assertFalse("AddAll returns true, false was expected", s.addAll(set));
		set.add(2);
		assertTrue("AddAll returns false, true was expected", s.addAll(set));
	}

}
