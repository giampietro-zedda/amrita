/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda 2008   Turin (ITALY)
 */
package junit.test;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import exception.ExceptionAmrita;

import analyzer.AmritaConstants;
import analyzer.AnalyzerCobol;
import analyzer.ExecutionDirectives ;
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
public class UnitTestProcessSystemLevelLogicDynamicTest implements AmritaConstants {
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
	public final void test_COBTST02_ProcessSystemLevel() throws Exception {
		aps = new ExecutionStarter();
		arParm = new String[2];
		arParm[0] = "";     // File di configurazione di default
		arParm[1] = "PilotUnitTestProcessSystemLevelLogicDynamic.pilot";   // File pilota per unit test processi a livello programma
		aps.main(arParm);
		assertTrue(true);
	}
	
	
	/**
	 * Test method for {@link analyzer.ExecutionStarter#main(java.lang.String[])}.
	 * @throws Exception 
	 */
	@Test
	public final void test_COBTST02_T036() throws Exception {
		
		String ar_valueOperand [] = {"T036PGM1", "T036PGM2", "T036PGM3"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T036 istruzione CALL T036-FLD-SINGLE
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(426);
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
	public final void test_COBTST02_T037() throws Exception {
		
		String ar_valueOperand [] = {"T037P371", "T037P372", "T037P373", "T037P374"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T037 istruzione CALL T037-FLD-SINGLE
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(434);
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
	public final void test_COBTST02_T038() throws Exception {
		
		String ar_valueOperand [] = {"T038P38A", "T038P38B", "T038P38C", "T038P38D"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T038 istruzione CALL T038-FLD-SINGLE
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(442);
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
	public final void test_COBTST02_T039() throws Exception {
		
		String ar_valueOperand [] = {"T0390001", "T0390002"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T039 istruzione CALL T039-FLD-SINGLE
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(449);
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
	public final void test_COBTST02_T040() throws Exception {
		
		String ar_valueOperand [] = {"T0400001", "T0400002"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T040 istruzione EXEC CICS READ FILE(T040-FLD-SINGLE)
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(456);
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
	public final void test_COBTST02_T041() throws Exception {
		
		String ar_valueOperand [] = {"T0410001", "T0410002"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T041 istruzione  EXEC CICS READ FILE(T041-FLD-SINGLE)
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(464);
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
	public final void test_COBTST02_T042() throws Exception {
		
		String ar_valueOperand [] = {"T042PGM1", "T042PGM2", "T042PGM3"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T042 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(471);
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
	public final void test_COBTST02_T043() throws Exception {
		
		String ar_valueOperand [] = {"T043P431", "T043P432"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T043 istruzione CALL
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(478);
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
	public final void test_COBTST02_T044() throws Exception {
		
		String ar_valueOperand [] = {"T044P44A", "T044P44B", "T044P44C",
				                     "T044Q44A", "T044Q44B", "T044Q44C",
				                     "T044R44A", "T044R44B", "T044R44C",
				                    };
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T044 istruzione CALL
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(485);
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
	public final void test_COBTST02_T045() throws Exception {
		
		String ar_valueOperand [] = {"T0450001", "T0450002", "T0450003"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T045 istruzione CALL
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(499);
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
	public final void test_COBTST02_T046() throws Exception {
		
		String ar_valueOperand [] = {"T046DFLT", "T0460001", "T0460002"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T046 istruzione CALL
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(512);
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
	public final void test_COBTST02_T047() throws Exception {
		
		String ar_valueOperand [] = {"T047DFLT", "T0470001", "T0470002"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T047 istruzione CALL
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(525);
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
	public final void test_COBTST02_T048() throws Exception {
		
		String ar_valueOperand [] = {"T048DFLT", "T0480001", "T0480002"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T048 istruzione CALL
        instrCobol = (InstructionCobolProcedure) programCobol.instructionProcedure(540);
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
	public final void test_COBTST02_T049() throws Exception {
		
		String ar_valueOperand [] = {"T0493141", "T0493142", "T0493241", "T0493242"};
		List<String> ls_valueOperand = null;
		
		// Inizializzazioni
		sd = new UserConfiguration();
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        di = new ExecutionDirectives (sd);
        
		// Recupero descrittore di COBTST02
		acp = new AnalyzerCobol(this.sd, di, programCobol);
		
		// Deserializzazione oggetto descrittore programma
        try {
  	        programCobol = (ProgramCobol) acp.getSerialized(this.sd.getDirCobolObjPgm(), "COBTST02", SUFFIX_SERIALIZED_PGM);
         } catch (ExceptionAmrita e) {
	        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
        }
		
        // Recupero istruzione e valori test T049 istruzione EXEC CICS LINK
        instrPrecompiler = (InstructionCics) programCobol.instructionProcedure(556);
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

}
