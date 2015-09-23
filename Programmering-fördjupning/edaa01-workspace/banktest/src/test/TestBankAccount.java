package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import bank.BankAccount;

public class TestBankAccount {
	private BankAccount theAccount;

	@Before
	public void setUp() throws Exception {
		theAccount = new BankAccount();
	}

	@After
	public void tearDown() throws Exception {
		theAccount = null;
	}

	@Test
	public final void testGetBalance() {
		assertEquals("New account, balance not 0", 0, theAccount.getBalance());

	}

	@Test
	public final void testDeposit() {
		theAccount.deposit(100);
		assertEquals("Wrong balance after deposit", 100, theAccount
				.getBalance());
	}

	@Test
	public final void testWithdraw() {
		theAccount.deposit(100);
		theAccount.withdraw(20);
		assertEquals("Wrong balance after withdraw", 80, theAccount
				.getBalance());
	}

	@Test
	public final void testOverDraft() {
		theAccount.deposit(100);
		try {
			theAccount.withdraw(200);
			fail("Should raise IllegalArgumentException");
		} catch (IllegalArgumentException e) {
			// successful test
		}
	}

}
