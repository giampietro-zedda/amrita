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
import analyzer.AmritaConstants;
import analyzer.AnalyzerCobol;
import analyzer.ExecutionDirectives;
import analyzer.ExecutionStarter;
import analyzer.InstructionCobolProcedure;
import analyzer.InstructionCics;
import analyzer.LoggerFacade;
import analyzer.MessagesManager;
import analyzer.ProgramCobol;
import analyzer.UserConfiguration;

/**
 * @author amrita
 *
 */
public class UnitTestProcessPgmLevelTest implements AmritaConstants {
	 protected ExecutionStarter aps = null;
	 protected String arParm[] = null;
     protected ProgramCobol programCobol = null;
	 protected UserConfiguration sd = null;
	 protected MessagesManager mm = null;
     protected LoggerFacade lf = null;
     protected AnalyzerCobol acp = null;
     protected ExecutionDirectives di = null;
     protected InstructionCobolProcedure instrCobol= null;
     protected InstructionCics instrPrecompiler= null;
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
	public final void testProcessPgmlevel() throws Exception {
		aps = new ExecutionStarter();
		arParm = new String[2];
		arParm[0] = "";     // File di configurazione di default
		arParm[1] = "PilotUnitTestProcessPgmlevel.pilot";   // File pilota per unit test processi a livello programma (COBTST06)
		aps.main(arParm);
		assertTrue(true);
		
	}


}
