/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda 2008   Turin (ITALY)
 */
package analyzer.test;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import enums.EnumLogicSetMode;
import enums.EnumLogicSetPointerArea;
import exception.ExceptionAmrita;

import analyzer.AmritaConstants;
import analyzer.AnalyzerCobol;
import analyzer.ExecutionDirectives;
import analyzer.ExecutionStarter;
import analyzer.InstructionCobolProcedure;
import analyzer.InstructionCics;
import analyzer.LoggerFacade;
import analyzer.LogicDynamicFieldSub;
import analyzer.MessagesManager;
import analyzer.ProgramCobol;
import analyzer.UserConfiguration;
import analyzer.LogicDynamicFieldSubSetting;

/**
 * @author amrita
 *
 */
public class UnitTestLogicManagerTest implements AmritaConstants {
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
	public final void test_COBTST02_ProcessPgmlevel() throws Exception {
		aps = new ExecutionStarter();
		arParm = new String[2];
		arParm[0] = "";     // File di configurazione di default
		arParm[1] = "PilotUnitTestLogicManager.pilot";   // File pilota per unit test processi a livello programma (COBTST02)
		aps.main(arParm);
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T001() throws Exception {
		
		String ar_valueOperand [] = {"T001DYN1"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T001 istruzione CALL T001-FLD01
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(69);
        ArrayList<String> al_valueOperand = instrCobol.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T002() throws Exception {
		
		String ar_valueOperand [] = {"T002DYN1"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T001 istruzione CALL T002-FLD-SINGLE
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(75);
        ArrayList<String> al_valueOperand = instrCobol.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T003() throws Exception {
		
		String ar_valueOperand [] = {"T003DYN ", "T003DYN1"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T003 istruzione EXEC CICS READ
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(84);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("FILE");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T004() throws Exception {
		
		String ar_valueOperand [] = {"        ", "00000000", ";;;;;;;;", "::::::::" };
		List<String> ls_valueOperand = null;
		
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T004 istruzione EXEC CICS READ
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(100);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("FILE");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T005() throws Exception {
		
		String ar_valueOperand [] = {"T005DYN1" };
		List<String> ls_valueOperand = null;
		
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T005 istruzione EXEC CICS READNEXT
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(105);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("FILE");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T006() throws Exception {
		
		String ar_valueOperand [] = {"T006DYN1", "T006DYN ", "T006D   "};
		List<String> ls_valueOperand = null;
		
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T006 istruzione EXEC CICS READPREV
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(121);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("FILE");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}


	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T007() throws Exception {
		
		String ar_valueOperand [] = {"T00700ZZ", "T00700AA", "T00700BB", "T00701ZZ", "T00701AA", "T00701BB", "T00702ZZ", "T00702BB", "T00702BB"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T007 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(142);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T008() throws Exception {
		
		String ar_valueOperand [] = {"T008DYN1", "T008DYN2", "T008DYN3"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T008 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(156);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T009() throws Exception {
		
		String ar_valueOperand [] = {"T009DYN1", "T009DYN2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T009 istruzione EXEC CICS XCTL
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(171);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T010() throws Exception {
		
		String ar_valueOperand [] = {"T010DY  "};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T010 istruzione EXEC CICS XCTL
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(178);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T011() throws Exception {
		
		String ar_valueOperand [] = {"T011DYN1", "T011DYN2", "T011DYN3"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T011 istruzione EXEC CICS XCTL
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(191);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi (Restituisce il default T011DFLT e non dovrebbe
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T012() throws Exception {
		
		String ar_valueOperand [] = {"T012DFLT", "T012DYN", "T012DYN1", "T012DYN2", "T012DYN3", "T012DYN4"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T012 istruzione EXEC CICS XCTL
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(212);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi 
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T013() throws Exception {
		
		String ar_valueOperand [] = {"T013DYN1"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T013 istruzione EXEC CICS XCTL
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(225);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T014() throws Exception {
		
		String ar_valueOperand [] = {"T014DYN1", "T014DYN2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T014 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(237);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T015() throws Exception {
		
		String ar_valueOperand [] = {"T015DFLT", "T015DYN", "T015DYN1", "T015DYN2", "T015DYN3"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T015 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(255);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T016() throws Exception {
		
		String ar_valueOperand [] = {"T016DFLT", "T016DYN1", "T016DYN2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T016 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(266);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T017() throws Exception {
		
		String ar_valueOperand [] = {"T017MAP1", "T017MAP2"};
		String ar_valueOperand2[] = {"T017MPS1", "T017MPS2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T017 istruzione EXEC CICS SEND
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(278);
        
        // MAP
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("MAP");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
        // MAPSET
        al_valueOperand = instrPrecompiler.getDynamicOperandValues("MAPSET");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand2);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T018() throws Exception {
		
		String ar_valueOperand [] = {"T018DFLT", "T018TBD2", "T018TBD3"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T018 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(288);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T019() throws Exception {
		
		String ar_valueOperand [] = {"T019DFLT", "T019TBD7"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T019 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(295);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T020() throws Exception {
		
		String ar_valueOperand [] = {"T020DFLT", "T020DYN1", "T020TBD1", "T020TBD2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T020 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(305);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T021() throws Exception {
		
		String ar_valueOperand [] = {"T021DFLT", "T021TBD1", "T021TBD2", "T021TBD3"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T021 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(318);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T022() throws Exception {
		
		String ar_valueOperand [] = {"T022DFLT", "T022DYN1", "T022DYN2", "T022DYN3"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T022 istruzione EXEC CICS XCTL
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(332);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T023() throws Exception {
		
		String ar_valueOperand [] = {"T023DYN1", "T023DYN2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T023 istruzione EXEC CICS XCTL
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(342);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T024() throws Exception {
		
		String ar_valueOperand [] = {"T024DYN1", "T024DYN2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T024 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(353);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T025() throws Exception {
		
		String ar_valueOperand [] = {"T025DFLT", "T025DYN1", "T025DYN2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T024 istruzione EXEC CICS XCTL
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(365);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T029() throws Exception {
		
		String ar_valueOperand [] = {"T029TSD1", "T029TSD2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T029 istruzione EXEC CICS READQ TS
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(374);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("QUEUE");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
		
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T030() throws Exception {
		
		String ar_valueOperand [] = {"T030TSD1", "T030TSD2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T030 istruzione EXEC CICS READQ TS
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(380);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("QUEUE");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T031() throws Exception {
		
		String ar_valueOperandFile [] = {"T031VSM1"};
		String ar_valueOperandQueue [] = {"T031TSD1", "T031TSD2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T031 istruzione EXEC CICS READ 
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(387);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("FILE");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperandFile.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperandFile);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
        // Recupero istruzione e valori test T031 istruzione EXEC CICS READQ TD 
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(390);
        al_valueOperand = instrPrecompiler.getDynamicOperandValues("QUEUE");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperandQueue.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperandQueue);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T032() throws Exception {
		
		String ar_valueOperand [] = {"T032TSD1", "T032TSD2", "T032TSD3"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T030 istruzione EXEC CICS READQ TD
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(398);
        ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("QUEUE");
        
        // Numero valori diversi
        if (al_valueOperand.size() != ar_valueOperand.length) {
			assertTrue(false);
		}
        ls_valueOperand = Arrays.asList(ar_valueOperand);
        
        // Valori diversi
        if (!al_valueOperand.containsAll(ls_valueOperand)) {
        	assertTrue(false);
		}
        
		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T033() throws Exception {
		
		String ar_valueOperandQueue [] = {"T033TSQ1"};
		String ar_valueOperandFile [] = {"T033VSM1", "T033VSM2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T033 istruzione EXEC CICS READQ TD
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(404);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("QUEUE");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperandQueue.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperandQueue);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
         // Recupero istruzione e valori test T033 istruzione EXEC CICS READ 
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(406);
         al_valueOperand = instrPrecompiler.getDynamicOperandValues("FILE");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperandFile.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperandFile);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T034() throws Exception {
		
		String ar_valueOperand [] = {"TRM1    ", "TRM2    "};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T034 istruzione EXEC CICS RETURN
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(413);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("TRANSID");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T035() throws Exception {
		
		String ar_valueOperand [] = {"TRN1    ", "TRN2    "};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T035 istruzione EXEC CICS RETURN
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(420);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("TRANSID");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}


	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T036_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<analyzer.LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T036 istruzione EXEC CICS RETURN
         

		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
        al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(426, "T036-FLD-SINGLE");

		// Sottocampo T036-FLD-SINGLE
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
// 		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T036-FLD-SINGLE")
		||	 numInstrOrigin != 426
		||   numInstrSet    != 424
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  !setField.equals("LK-SPREADED-PGM-NAME")
		||   posInField != 1
		||   lngInField != 8
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 8
		||   dspFieldInLinkageArea != 0
		||   numUsingParm != 2
		||   numUsingParmPointer != 0
		||   dspFieldInUsingParm != 32
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}
  		
 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T037_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T036 istruzione EXEC CICS RETURN
         

		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(434, "T037-FLD-SINGLE");

		// Sottocampo T037-FLD-SINGLE
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
		// lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T037-FLD-SINGLE")
		||	 numInstrOrigin != 434
		||   numInstrSet    != 430
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  !setField.equals("LK-SPREADED-PGM-PREFIX")
		||   posInField != 1
		||   lngInField != 4
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 4
		||   dspFieldInLinkageArea != 0
		||   numUsingParm != 2
		||   dspFieldInUsingParm != 40
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(1); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T037-FLD-SINGLE")
		||	 numInstrOrigin != 434
		||   numInstrSet    != 431
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  !setField.equals("LK-SPREADED-PGM-ID")
		||   posInField != 1
		||   lngInField != 3
		||   posMappedInSubFieldOrigin != 5
		||   lngMappedInSubFieldOrigin != 3
		||   dspFieldInLinkageArea != 0
		||   numUsingParm != 2
		||   dspFieldInUsingParm != 44
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(2); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T037-FLD-SINGLE")
		||	 numInstrOrigin != 434
		||   numInstrSet    != 432
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  !setField.equals("LK-SPREADED-PGM-SUFFIX")
		||   posInField != 1
		||   lngInField != 1
		||   posMappedInSubFieldOrigin != 8
		||   lngMappedInSubFieldOrigin != 1
		||   dspFieldInLinkageArea != 0
		||   numUsingParm != 2
		||   dspFieldInUsingParm != 47
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

		
		
 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T038_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(442, "T038-FLD-SINGLE");

		// Sottocampo T037-FLD-SINGLE
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T038-FLD-SINGLE")
		||	 numInstrOrigin != 442
		||   numInstrSet    != 438
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  !setField.equals("LK-SPREADED-PGM-PREFIX")
		||   posInField != 1
		||   lngInField != 4
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 4
		||   dspFieldInLinkageArea != 0
		||   numUsingParm != 2
		||   dspFieldInUsingParm != 40
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(1); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T038-FLD-SINGLE")
		||	 numInstrOrigin != 442
		||   numInstrSet    != 439
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  !setField.equals("LK-SPREADED-PGM-ID")
		||   posInField != 1
		||   lngInField != 3
		||   posMappedInSubFieldOrigin != 5
		||   lngMappedInSubFieldOrigin != 3
		||   dspFieldInLinkageArea != 0
		||   numUsingParm != 2
		||   dspFieldInUsingParm != 44
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(2); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T038-FLD-SINGLE")
		||	 numInstrOrigin != 442
		||   numInstrSet    != 440
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  !setField.equals("LK-SPREADED-PGM-SUFFIX-0")
		||   posInField != 1
		||   lngInField != 1
		||   posMappedInSubFieldOrigin != 8
		||   lngMappedInSubFieldOrigin != 1
		||   dspFieldInLinkageArea != 0
		||   numUsingParm != 1
		||   dspFieldInUsingParm != 15
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

		
		
 		assertTrue(true);
	}

	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T039_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(449, "T039-FLD-SINGLE");

		// Sottocampo T039-FLD-SINGLE
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T039-FLD-SINGLE")
		||	 numInstrOrigin != 449
		||   numInstrSet    != 448
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_CICS_TWA
		||  !setField.equals("T039-TWA-PGM-NAME")
		||   posInField != 21
		||   lngInField != 8
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 8
		||   dspFieldInLinkageArea != 20
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

		
 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T040_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(456, "T040-FLD-SINGLE");

		// Sottocampo T039-FLD-SINGLE
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T040-FLD-SINGLE")
		||	 numInstrOrigin != 456
		||   numInstrSet    != 455
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_CICS_TWA
		||  !setField.equals("T040-TWA-FILE-NAME")
		||   posInField != 29
		||   lngInField != 8
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 8
		||   dspFieldInLinkageArea != 28
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

		
 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T041_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(464, "T041-FLD-SINGLE");

		// Sottocampo T039-FLD-SINGLE
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T041-FLD-SINGLE")
		||	 numInstrOrigin != 464
		||   numInstrSet    != 462
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_CICS_CSA
		||  !setField.equals("T041-CSA")
		||   posInField != 2
		||   lngInField != 8
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 8
		||   dspFieldInLinkageArea != 1
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

		
 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T042_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(471, "T042-PGM-NAME");

		// Sottocampo T042-PGM-NAME
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T042-PGM-NAME")
		||	 numInstrOrigin != 471
		||   numInstrSet    != 468
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_CICS_DFHCOMMAREA
		||  !setField.equals("DFHCOMMAREA")
		||   posInField != 25
		||   lngInField != 8
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 8
		||   dspFieldInLinkageArea != 24
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T043_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(478, "T043-PGM-NAME");

		// Sottocampo T043-PGM-NAME
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T043-PGM-NAME")
		||	 numInstrOrigin != 478
		||   numInstrSet    != 475
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_CICS_DFHCOMMAREA
		||  !setField.equals("DFHCOMMAREA")
		||   posInField != 9
		||   lngInField != 8
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 8
		||   dspFieldInLinkageArea != 8
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T044_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(485, "T044-PGM-NAME-GROUP");

		// Sottocampo T044-PGM-PREFIX
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T044-PGM-PREFIX")
		||	 numInstrOrigin != 485
		||   numInstrSet    != 482
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  !setField.equals("LK-SPREADED-PGM-PREFIX-T044")
		||   posInField != 1
		||   lngInField != 4
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 4
		||   dspFieldInLinkageArea != 0
		||   numUsingParm != 2
		||   dspFieldInUsingParm != 68
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

		// Sottocampo T044-PGM-PREFIX
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(1);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();

	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T044-PGM-ID")
		||	 numInstrOrigin != 485
		||   numInstrSet    != 483
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  !setField.equals("LK-SPREADED-PGM-ID-T044")
		||   posInField != 1
		||   lngInField != 3
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 3
		||   dspFieldInLinkageArea != 0
		||   numUsingParm != 2
		||   dspFieldInUsingParm != 72
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

		// Sottocampo T044-PGM-PREFIX
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(2);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
		// Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T044-PGM-SUFFIX")
		||	 numInstrOrigin != 485
		||   numInstrSet    != 484
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  !setField.equals("LK-SPREADED-PGM-SUFFIX-T044")
		||   posInField != 1
		||   lngInField != 1
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 1
		||   dspFieldInLinkageArea != 0
		||   numUsingParm != 2
		||   dspFieldInUsingParm != 75
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}


		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T045_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(499, "T045-PGM-NAME");

		// Sottocampo T045-PGM-NAME
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T045-PGM-NAME")
		||	 numInstrOrigin != 499
		||   numInstrSet    != 498
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE
		||  !setField.equals("LK-SPREADED-PGM-NAME-T045")
		||   posInField != 1
		||   lngInField != 8
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 8
		||   dspFieldInLinkageArea != 16
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 1
		||   pointerTypeArea != EnumLogicSetPointerArea.POINTER_INSIDE_USING_PARM
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 11
		   ) {
			assertTrue(false);
		}


		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T046_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(512, "T046-PGM-NAME");

		// Sottocampo T046-PGM-NAME
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T046-PGM-NAME")
		||	 numInstrOrigin != 512
		||   numInstrSet    != 511
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE
		||  !setField.equals("LK-SPREADED-PGM-NAME-T046")
		||   posInField != 1
		||   lngInField != 8
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 8
		||   dspFieldInLinkageArea != 16
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.POINTER_INSIDE_CICS_DFHCOMMAREA
		||   dspPointerInLinkageArea != 16
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}


		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T047_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(525, "T047-PGM-NAME");

		// Sottocampo T047-PGM-NAME
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T047-PGM-NAME")
		||	 numInstrOrigin != 525
		||   numInstrSet    != 524
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE
		||  !setField.equals("LK-SPREADED-PGM-NAME-T047")
		||   posInField != 1
		||   lngInField != 8
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 8
		||   dspFieldInLinkageArea != 16
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.POINTER_INSIDE_CICS_TWA
		||   dspPointerInLinkageArea != 16
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}


		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T048_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(540, "T048-PGM-NAME");

		// Sottocampo T048-PGM-NAME
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T048-PGM-NAME")
		||	 numInstrOrigin != 540
		||   numInstrSet    != 539
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE
		||  !setField.equals("LK-SPREADED-PGM-NAME-T048")
		||   posInField != 1
		||   lngInField != 8
		||   posMappedInSubFieldOrigin != 1
		||   lngMappedInSubFieldOrigin != 8
		||   dspFieldInLinkageArea != 16
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.POINTER_INSIDE_CICS_CSA
		||   dspPointerInLinkageArea != 33
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}


		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T049_spreaded() throws Exception {
		
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		ArrayList<LogicDynamicFieldSub> al_logicDynamicFieldSub = null;
		LogicDynamicFieldSubSetting lastSetSpreaded = null;
		String subFieldName = "";
		EnumLogicSetMode setMode = null;;
		String setField = "";
		int numInstrOrigin = 0;
		int numInstrSet = 0;
		int posInField = 0;
		int lngInField = 0;
		int posMappedInSubFieldOrigin = 0;
		int lngMappedInSubFieldOrigin = 0;
		int dspFieldInLinkageArea = 0;
		int numUsingParm = 0;
		int numUsingParmPointer = 0;
		int dspFieldInUsingParm = 0;
		EnumLogicSetPointerArea pointerTypeArea = null;
		int dspPointerInLinkageArea = 0;
		int dspPointerInUsingParm = 0;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
 
		// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
		al_logicDynamicFieldSub = programCobol.getLogicInfoDynamic().getDynamicFieldsSub(556, "T049-FLDX8");

		// Sottocampo T049-FLDX8
		logicDynamicFieldSub = al_logicDynamicFieldSub.get(0);
	    subFieldName = logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier();
	    
	    // Ultima assegnazione 1
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(0); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T049-FLDX8")
		||	 numInstrOrigin != 556
		||   numInstrSet    != 549
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_CICS_DFHCOMMAREA
		||  !setField.equals("DFHCOMMAREA")
		||   posInField != 62
		||   lngInField != 3
		||   posMappedInSubFieldOrigin != 2
		||   lngMappedInSubFieldOrigin != 3
		||   dspFieldInLinkageArea != 61
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

	    // Ultima assegnazione 2
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(1); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T049-FLDX8")
		||	 numInstrOrigin != 556
		||   numInstrSet    != 553
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_CICS_TWA
		||  !setField.equals("T049-TWA-SUBFLD3")
		||   posInField != 77
		||   lngInField != 2
		||   posMappedInSubFieldOrigin != 5
		||   lngMappedInSubFieldOrigin != 2
		||   dspFieldInLinkageArea != 76
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}

	    // Ultima assegnazione 3
//		lastSetSpreaded = logicDynamicFieldSub.al_lastSetSpreaded.get(2); 

		// Valori ultima assegnazione
		numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded();
		numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet();
		setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode();
		setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
		posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos();
		lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng();
		posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();
		lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		dspFieldInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		numUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		numUsingParmPointer = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer();
	    dspFieldInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
	    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea();
	    dspPointerInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea();
		dspPointerInUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm();
  		
		// Verifica valori
		if (!subFieldName.equals("T049-FLDX8")
		||	 numInstrOrigin != 556
		||   numInstrSet    != 554
		||   setMode        != EnumLogicSetMode.LAST_SET_BY_CICS_CSA
		||  !setField.equals("T049-CSA-SUBFLD4")
		||   posInField != 52
		||   lngInField != 2
		||   posMappedInSubFieldOrigin != 7
		||   lngMappedInSubFieldOrigin != 2
		||   dspFieldInLinkageArea != 51
		||   numUsingParm != 0
		||   dspFieldInUsingParm != 0
		||   numUsingParmPointer != 0
		||   pointerTypeArea != EnumLogicSetPointerArea.NOT_ASSIGNED
		||   dspPointerInLinkageArea != 0
		||   dspPointerInUsingParm != 0
		   ) {
			assertTrue(false);
		}


		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T050() throws Exception {
		
		String ar_valueOperand [] = {"T050DYN1"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T050 istruzione EXEC CICS LINK
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(560);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}

	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T051() throws Exception {
		
		String ar_valueOperand [] = {"T051DYN1"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T051 istruzione EXEC CICS LINK
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(564);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T052() throws Exception {
		
		String ar_valueOperand [] = {"T05201AA", "T05202AA", "T05203AA", "T05201BB", 
									 "T05202BB", "T05203BB", "T05201DF", "T05202DF", 
									 "T05203DF", "T052DFAA", "T052DFBB", "T052DFDF"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T052 istruzione CALL
         instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(580);
         ArrayList<String> al_valueOperand = instrCobol.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T053() throws Exception {
		
		String ar_valueOperand [] = {"T053KK01", "T053KK02", "T053KK05", "T053KK99", "T053KK9Z", "T053KKMM"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T053 istruzione LINK
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(606);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}
	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T055() throws Exception {
		
		String ar_valueOperand [] = {"T055AABB", "T055AA04", "T055A0BB", "T055A004", 
									 "T055BBBB", "T055BB04", "T055AADF", "T055A0DF", 
									 "T055BBDF", "T055DFBB", "T055DF04", "T055DFDF"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T055 istruzione LINK
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(624);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}
	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T056() throws Exception {
		
		String ar_valueOperand [] = {"T056T2D1", "T056T2D5", "T056T2D9", "T056T2D2", "T056T2D6", "T056T2DA",  
									 "T056T2D3", "T056T2D7", "T056T2DB", "T056T2D4", "T056T2D8", "T056T2DC", 
									 "T056TBD1", "T056TBD5", "T056TBD9", "T056TBD2", "T056TBD6", "T056TBDA",  
									 "T056TBD3", "T056TBD7", "T056TBDB", "T056TBD4", "T056TBD8", "T056TBDC"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T056 istruzione LINK
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(634);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}
	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T057() throws Exception {
		
		String ar_valueOperand [] = {"T057AABB", "T057CCBB", "T057AADD", "T057CCDD"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T057 istruzione CALL
         instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(650);
         ArrayList<String> al_valueOperand = instrCobol.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}
	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T058() throws Exception {
		
		String ar_valueOperand [] = {"T058AABB", "T058AADD", "T058AA02", "T058CCBB", "T058CCDD", "T058CC02", "T05801BB", "T05801DD", "T0580102"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T058 istruzione CALL
         instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(667);
         ArrayList<String> al_valueOperand = instrCobol.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}
	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T059() throws Exception {
		
		String ar_valueOperand [] = {"T059TTUU"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T059 istruzione CALL
         instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(679);
         ArrayList<String> al_valueOperand = instrCobol.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T060() throws Exception {
		
		String ar_valueOperand [] = {"T060A1B1", "T060A1B2", "T060A1B3", "T060A2B1", "T060A2B2", "T060A2B3"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T060 istruzione 593  EXEC CICS LINK
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(694);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}
	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T061() throws Exception {
		
		String ar_valueOperand [] = {"T061M1N1", "T061M1N2", "T061M2N1",  "T061M2N2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T061 istruzione EXEC CICS LINK
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(702);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}
	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T062() throws Exception {
		
		String ar_valueOperand [] = {"T621", "T622"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T062 istruzione 608  EXEC CICS ABEND
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(709);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("ABCODE");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}
	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T063() throws Exception {
		
		String ar_valueOperand [] = {"T063P1M1", "T063P1M2", "T063P2M1", "T063P2M2"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T063 istruzione EXEC CICS LINK
         instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(717);
         ArrayList<String> al_valueOperand = instrPrecompiler.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}
	

	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T065() throws Exception {
		
		String ar_valueOperand [] = {"T065C1ZZ", "T065F1ZZ", "T065P1ZZ", "T065P2ZZ","T065P3ZZ"};
		List<String> ls_valueOperand = null;
		 
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives(sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
         // Recupero istruzione e valori test T065 istruzione CALL
         instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(734);
         ArrayList<String> al_valueOperand = instrCobol.getDynamicOperandValues("PROGRAM");
         
         // Numero valori diversi
         if (al_valueOperand.size() != ar_valueOperand.length) {
 			assertTrue(false);
 		}
         ls_valueOperand = Arrays.asList(ar_valueOperand);
         
         // Valori diversi
         if (!al_valueOperand.containsAll(ls_valueOperand)) {
         	assertTrue(false);
 		}
         
 		assertTrue(true);
	}
	

}
