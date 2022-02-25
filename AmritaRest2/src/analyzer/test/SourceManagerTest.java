package analyzer.test;

import static org.junit.Assert.*;

import java.util.ArrayList;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import analyzer.ExecutionDirectives;
import analyzer.LoggerFacade;
import analyzer.MessagesManager;
import analyzer.SourceInput;
import analyzer.SourceManager;
import analyzer.UserConfiguration;
import exception.ExceptionAmrita;

public class SourceManagerTest {
    // Variabili fixture
	 protected UserConfiguration UserConfiguration;
	 protected LoggerFacade loggerFacade;
	 protected MessagesManager messagesManager;
	 protected SourceManager sourceManager;
	 protected SourceInput sourceInput;
	 protected SourceInput sourceInput2;
	 protected SourceInput sourceInput3;
	 protected SourceInput arSourceInput[];
	 protected String arFilter[];

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public final void testGetSourceAndInfo() {
		initDefaultsAndLog();
		sourceManager = new SourceManager(UserConfiguration);
		
		////////////////////////////////////////////////////////////////////////////
		// (1) Recupero contenuto  file specifico (pilota di analisi con 5 entries)
		////////////////////////////////////////////////////////////////////////////
		
		sourceInput = sourceManager.getSource("I:/Amrita/Amrita-Java/AmritaFramework/work/", "iptAnalyzer", "txt", false, false);
		assertNotNull(sourceInput);
		assertTrue(sourceInput.getIdSource().equals("iptAnalyzer"));
		assertTrue(sourceInput.getSourceSuffix().equals("txt"));
		assertTrue(sourceInput.getIdSourceComplete().equals("iptAnalyzer.txt"));
		assertTrue(sourceInput.getDirInput().equals("I:/Amrita/Amrita-Java/AmritaFramework/work/"));
		assertTrue(sourceInput.getPathComplete().equals("I:\\Amrita\\Amrita-Java\\AmritaFramework\\work\\iptAnalyzer.txt"));
		assertTrue(sourceInput.getSize() > 0);
		assertFalse(sourceInput.isDirectory());
		assertFalse(sourceInput.isHidden());
		assertTrue(sourceInput.getArrayRowSource().length == 8);
	}

	@Test
	public final void testFileInfoStringStringString() {
		initDefaultsAndLog();
		
		////////////////////////////////////////////////////////////////////////////
		// (1) Recupero informazioni file specifico (directory con i sorgenti)
		////////////////////////////////////////////////////////////////////////////
		
		sourceManager = new SourceManager(UserConfiguration);
 		sourceInput = sourceManager.fileInfo("G:\\SPaolo\\Sources\\LTM0H.SAE.SOURCE\\", "TestDir", "");
 		assertNotNull(sourceInput);
		assertTrue(sourceInput.getIdSource().equals("TestDir"));
		assertTrue(sourceInput.getSourceSuffix().equals(""));
		assertTrue(sourceInput.getIdSourceComplete().equals("TestDir"));
		assertTrue(sourceInput.getDirInput().equals("G:\\SPaolo\\Sources\\LTM0H.SAE.SOURCE\\"));
		assertTrue(sourceInput.getPathComplete().equals("G:\\SPaolo\\Sources\\LTM0H.SAE.SOURCE\\TestDir"));
 		assertTrue(sourceInput.isDirectory());
 		assertFalse(sourceInput.isHidden());
 		
 		////////////////////////////////////////////////////////////////////////////
 		// (2) Ripeto il test
 		////////////////////////////////////////////////////////////////////////////
 		
 		sourceManager = new SourceManager(UserConfiguration);
		sourceInput = sourceManager.fileInfo("G:/SPaolo/Sources/LTM0H.SAE.SOURCE/", "TestDir", "");
		assertTrue(sourceInput.getIdSource().equals("TestDir"));
		assertTrue(sourceInput.getSourceSuffix().equals(""));
		assertTrue(sourceInput.getIdSourceComplete().equals("TestDir"));
		assertTrue(sourceInput.getDirInput().equals("G:/SPaolo/Sources/LTM0H.SAE.SOURCE/"));
		assertTrue(sourceInput.getPathComplete().equals("G:\\SPaolo\\Sources\\LTM0H.SAE.SOURCE\\TestDir"));
 		assertTrue(sourceInput.isDirectory());
 		assertFalse(sourceInput.isHidden());

		////////////////////////////////////////////////////////////////////////////
 		// (3) Recupero informazioni file specifico sorgente
 		////////////////////////////////////////////////////////////////////////////
 		
 		sourceManager = new SourceManager(UserConfiguration);
		sourceInput = sourceManager.fileInfo("G:/SPaolo/Sources/LTM0H.SAE.SOURCE/", "LFA1100", "");
		assertNotNull(sourceInput);
		assertTrue(sourceInput.getIdSource().equals("LFA1100"));
		assertTrue(sourceInput.getSourceSuffix().equals(""));
		assertTrue(sourceInput.getIdSourceComplete().equals("LFA1100"));
		assertTrue(sourceInput.getDirInput().equals("G:/SPaolo/Sources/LTM0H.SAE.SOURCE/"));
		assertTrue(sourceInput.getPathComplete().equals("G:\\SPaolo\\Sources\\LTM0H.SAE.SOURCE\\LFA1100"));
 		assertFalse(sourceInput.isDirectory());
 		assertFalse(sourceInput.isHidden());
  		
 		
		////////////////////////////////////////////////////////////////////////////
 		// (4) Recupero informazioni file specifico sorgente non presente
 		////////////////////////////////////////////////////////////////////////////
 		
 		sourceManager = new SourceManager(UserConfiguration);
 		sourceInput = null;
		sourceInput = sourceManager.fileInfo("G:/SPaolo/Sources/LTM0H.SAE.SOURCE/", "LFA1100", "FILE");
		assertNull(sourceInput);
 		
	}

	@Test
	public final void testFileInfoString() {
		initDefaultsAndLog();

		
		////////////////////////////////////////////////////////////////////////////
 		// (1) Recupero informazioni file specifico sorgente con path completo
 		////////////////////////////////////////////////////////////////////////////

		sourceManager = new SourceManager(UserConfiguration);
 		sourceInput = sourceManager.fileInfo("G:/SPaolo/Sources/LTM0H.SAE.SOURCE/" + "TestDir");
 		assertNotNull(sourceInput);
		assertTrue(sourceInput.getIdSource().equals("TestDir"));
		assertTrue(sourceInput.getSourceSuffix().equals(""));
		assertTrue(sourceInput.getIdSourceComplete().equals("TestDir"));
		assertTrue(sourceInput.getDirInput().equals("G:/SPaolo/Sources/LTM0H.SAE.SOURCE/"));
		assertTrue(sourceInput.getPathComplete().equals("G:\\SPaolo\\Sources\\LTM0H.SAE.SOURCE\\TestDir"));
 		assertTrue(sourceInput.isDirectory());
 		assertFalse(sourceInput.isHidden());

		////////////////////////////////////////////////////////////////////////////
 		// (2) Recupero informazioni file specifico sorgente con path completo NON ESISTENTE
 		////////////////////////////////////////////////////////////////////////////

		sourceManager = new SourceManager(UserConfiguration);
 		sourceInput = sourceManager.fileInfo("G:/SPaolo/Sources/LTM0H.SAE.SOURCE/" + "RRRRR");
 		assertNull(sourceInput);
	
	}

	@Test
	public final void testListFiles() throws Exception {
		initDefaultsAndLog();

		////////////////////////////////////////////////////////////////////////////
 		// (1) Recupero elenco files in una directory senza filtri
 		////////////////////////////////////////////////////////////////////////////

		sourceManager = new SourceManager(UserConfiguration);
		arSourceInput = sourceManager.listFiles(UserConfiguration.getDirWork(), false, false);
		assertTrue(arSourceInput.length > 2);	
		arSourceInput = sourceManager.listFiles(UserConfiguration.getPathPilot(), false, false);
		assertTrue(arSourceInput.length == 0);
		
		// Simulazione recupero sorgenti da analizzare
		sourceInput = sourceManager.getSource(UserConfiguration.getPathPilot(), false, false);
		assertTrue(sourceInput.getArrayRowSource().length == 8);

		// Simulazione recupero nomi sorgenti da analizzare
		sourceInput = sourceManager.getSource(UserConfiguration.getPathPilot(), false, false);
		assertTrue(sourceInput.getArrayRowSource().length == 8);
		// Recupero sorgente per sorgente
		for (int i = 0; i < sourceInput.getArrayRowSource().length; i++) {
			String strPathFileComplete = sourceInput.getArrayRowSource()[i];
			if (strPathFileComplete.startsWith("#")) {
				continue;
			}
			sourceInput2  = sourceManager.fileInfo(strPathFileComplete);
			// Recupero il sorgente e  tutte le informazioni del file
			if (!sourceInput2.isDirectory()) {
				sourceInput3 = sourceManager.getSource(strPathFileComplete,	false, false);
			}
			
		}
		

		////////////////////////////////////////////////////////////////////////////
 		// (2) Recupero elenco files in una directory senza filtri
 		////////////////////////////////////////////////////////////////////////////

		sourceManager = new SourceManager(UserConfiguration);
		arSourceInput = sourceManager.listFiles(UserConfiguration.getDirWork(), false, false);
		assertTrue(arSourceInput.length > 2);		
		arSourceInput = sourceManager.listFiles(UserConfiguration.getPathPilot(), false, false);
		assertTrue(arSourceInput.length == 0);	
		
		// Simulazione recupero sorgenti da analizzare
		sourceInput = sourceManager.getSource(UserConfiguration.getPathPilot(), false, false);
		assertTrue(sourceInput.getArrayRowSource().length == 8);

		// Simulazione recupero nomi sorgenti da analizzare
		sourceInput = sourceManager.getSource(UserConfiguration.getPathPilot(), false, false);
		assertTrue(sourceInput.getArrayRowSource().length == 8);
		// Recupero sorgente per sorgente
		for (int i = 0; i < sourceInput.getArrayRowSource().length; i++) {
			String strPathFileComplete = sourceInput.getArrayRowSource()[i];
			if (strPathFileComplete.startsWith("#")) {
				continue;
			}
			sourceInput2  = sourceManager.fileInfo(strPathFileComplete);
			// Recupero il sorgente e  tutte le informazioni del file
			if (!sourceInput2.isDirectory()) {
				sourceInput3 = sourceManager.getSource(strPathFileComplete,	false, false);
			}
			
		}

		////////////////////////////////////////////////////////////////////////////
 		// (3) Recupero elenco files in una directory CON filtri
 		////////////////////////////////////////////////////////////////////////////

		sourceManager = new SourceManager(UserConfiguration);
		arFilter = new String[]{new String("ip")};
		arSourceInput = sourceManager.listFiles(UserConfiguration.getDirWork(), false, false);
		assertNotNull(arSourceInput);	
		assertTrue(arSourceInput.length == 1);	
		arSourceInput = sourceManager.listFiles(UserConfiguration.getPathPilot(), false, false);
		assertTrue(arSourceInput.length == 0);	
		
		// Simulazione recupero sorgenti da analizzare elencati in un file pilota
		sourceInput = sourceManager.getSource(UserConfiguration.getPathPilot(), false, false);
		assertTrue(sourceInput.getArrayRowSource().length == 8);

		// Simulazione recupero nomi sorgenti da analizzare
		sourceInput = sourceManager.getSource(UserConfiguration.getPathPilot(), false, false);
		assertTrue(sourceInput.getArrayRowSource().length == 8);
		// Recupero sorgente per sorgente
		for (int i = 0; i < sourceInput.getArrayRowSource().length; i++) {
			String strPathFileComplete = sourceInput.getArrayRowSource()[i];
			if (strPathFileComplete.startsWith("#")) {
				continue;
			}
			sourceInput2  = sourceManager.fileInfo(strPathFileComplete);
			// Recupero il sorgente e  tutte le informazioni del file
			if (!sourceInput2.isDirectory()) {
				sourceInput3 = sourceManager.getSource(strPathFileComplete,	false, false);
				assertNotNull(sourceInput3);
			}
			
		}
		
		
	}

	@Test
	public final void testListFilesRecursive() throws Exception {
		initDefaultsAndLog();

		////////////////////////////////////////////////////////////////////////////
 		// (1) Recupero elenco files a partire da un file pilota con elenco di files e directorie
		//     ricorsivamente CON filtri
 		////////////////////////////////////////////////////////////////////////////

		sourceManager = new SourceManager(UserConfiguration);
		arFilter = new String[]{new String("Pr"), new String("lv2")};
		arSourceInput = sourceManager.listFiles("G:/SPaolo/Sources/LTM0H.SAE.SOURCE/", true, true);
		assertTrue(arSourceInput.length == 6);	
		
	}

	@Test
	public final void testListFilesFromPilot() throws Exception {
		initDefaultsAndLog();

		ArrayList<ExecutionDirectives> al_di = null;
		
        // Pilota inesistente
		sourceManager = new SourceManager(UserConfiguration);
//		arSourceInput = sourceManager.listFilesFromPilot("G:/SPaolo/Sources/LTM0H.SAE.SOURCE/",true, true);
 		arSourceInput = sourceManager.listFilesFromPilot(al_di,true, true);
		assertNull(arSourceInput);	
		
        // Pilota corretto
		sourceManager = new SourceManager(UserConfiguration);
		arFilter = new String[]{new String("Pr"), new String("lv2")};
//		arSourceInput = sourceManager.listFilesFromPilot(UserConfiguration.getPathIptPilotSourcesToAnalyze(), true, true);
		arSourceInput = sourceManager.listFilesFromPilot(al_di, true, true);
		assertNotNull(arSourceInput);	
		assertTrue(arSourceInput.length == 10);	

	}
	
	/**
	 * Test method for {@link analyzer.GraphManager#optimizeGraph()}.
	 */
	private  void initDefaultsAndLog() {
		// Defaults
		try {
			UserConfiguration = new UserConfiguration();
		} catch (ExceptionAmrita e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        messagesManager = new MessagesManager(UserConfiguration);
        UserConfiguration.setMessagesManager(messagesManager);
        loggerFacade = new LoggerFacade(UserConfiguration);
        UserConfiguration.setLoggerFacade(loggerFacade);
	}

}
