/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda 2008   Turin (ITALY)
 */
package junit.test;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import analyzer.ExecutionStarter;

/**
 * @author amrita
 *
 */
public class ProcessPgmLevelUXTest {
	 protected ExecutionStarter aps = null;
	 protected String arParm[] = null;

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}


	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void testFunctionSourcesDetected() throws Exception {
		aps = new ExecutionStarter();
		arParm = new String[2];
		arParm[0] = "";     // File di configurazione di default
		arParm[1] = "PilotProcessPgmlevelUX.pilot";   // File pilota per unit test processi a livello programma
		aps.main(arParm);
		assertTrue(true);
	}
	
}
