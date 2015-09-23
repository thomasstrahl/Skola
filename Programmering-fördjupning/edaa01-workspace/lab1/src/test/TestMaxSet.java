package test;

import static org.junit.Assert.*;

import java.util.NoSuchElementException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import set.ArraySet;
import set.MaxSet;

public class TestMaxSet {
	private MaxSet<Integer> s;

	@Before
	public void setUp() throws Exception {
		s = new MaxSet<Integer>();
	}

	@After
	public void tearDown() throws Exception {
		s = null;
	}

	@Test
	public final void testMaxOfEmpty() {
		try {
			int i = s.getMax();
			fail("getMax() of empty set should throw exception");
		} catch (NoSuchElementException e) {
			// success
		}
	}

	@Test
	public final void testMaxOfOne() {
		s.add(1);
		assertEquals("Wrong value of max: ", new Integer(1), s.getMax());
	}

	@Test
	public final void testMaxOfTwo() {
		s.add(1);
		s.add(2);
		assertEquals("Wrong value of max: ", new Integer(2), s.getMax());
	}

	@Test
	public final void testMaxOfSeveral() {
		for (int i = 1; i <= 1000; i++) {
			s.add(i);
		}
		assertEquals("Wrong value of max: ", new Integer(1000), s.getMax());
	}

	@Test
	public final void testMaxAfterMaxremove() {
		for (int i = 1; i <= 1000; i++) {
			s.add(i);
		}
		s.remove(1000);
		assertEquals("Wrong value of max: ", new Integer(999), s.getMax());
	}

	@Test
	public final void testMaxAfterManyMaxremoves() {
		for (int i = 1; i <= 1000; i++) {
			s.add(i);
		}
		for (int i = 1000; i >= 500; i--) {
			s.remove(i);
		}
		assertEquals("Wrong value of max: ", new Integer(499), s.getMax());
	}

	@Test
	public final void testMaxAfterMakeEmpty() {
		s.add(3);
		s.add(5);
		s.remove(5);
		s.remove(3);
		s.add(2);
		assertEquals("Wrong value of max: ", new Integer(2), s.getMax());
	}

	@Test
	public final void testMaxAfterAddAll() {
		for (int i = 1; i <= 1000; i++) {
			s.add(i);
		}
		ArraySet<Integer> s2 = new ArraySet<Integer>();
		for (int i = 1000; i <= 2000; i++) {
			s2.add(i);
		}
		s.addAll(s2);
		assertEquals("Wrong value of max: ", new Integer(2000), s.getMax());
	}
}
