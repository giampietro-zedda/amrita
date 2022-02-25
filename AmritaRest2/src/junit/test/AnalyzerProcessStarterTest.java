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
public class AnalyzerProcessStarterTest {
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
	public final void testAllInputDefault() throws Exception {
		aps = new ExecutionStarter();
		arParm = new String[4];
		arParm[0] = "";     // File di configurazione di default
		arParm[1] = "";     // File pilota processi di default
		arParm[2] = "";     // File pilota sorgenti di default
		arParm[3] = "";     // File pilota filtro sorgenti di default
		aps.main(arParm);
		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void testAllInputCoded() throws Exception {
		aps = new ExecutionStarter();
		arParm = new String[4];
		arParm[0] = "";     // File di configurazione di default
		arParm[1] = "PilotDefaultProcess.pilot";     		// File pilota processi di default
		arParm[2] = "PilotDefaultSource.pilot";     		// File pilota sorgenti di default
		arParm[3] = "PilotDefaultSourceFilter.flter";     	// File pilota filtro sorgenti di default
		aps.main(arParm);
		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void testFunctionSourcesDetected() throws Exception {
		aps = new ExecutionStarter();
		arParm = new String[4];
		arParm[0] = "";     // File di configurazione di default
		arParm[1] = "PilotTestAssegniFunctionSourcesDetected.pilot";   // File pilota processi di default
		arParm[2] = "PilotTestAssegniSources.pilot";       			   // File pilota sorgenti di default
		arParm[3] = "PilotTestAssegniSourceFilter.flter"; 			   // File pilota filtro sorgenti di default
		aps.main(arParm);
		assertTrue(true);
	}
	
}
