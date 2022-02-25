package analyzer;

import java.io.File;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;


import dao.DAOImplDynamicField;
import dao.DAOImplDynamicFieldSubValue;
import dao.DAOImplDynamicFieldSubWaitExt;
import dao.DAOImplObjectOption;
import dao.DAOImplRelation;
import dao.DAOImplRelationOrigin;
import dao.IDAODynamicField;
import dao.IDAODynamicFieldSubValue;
import dao.IDAODynamicFieldSubWaitExt;
import dao.IDAOObjectOption;
import dao.IDAORelation;
import dao.IDAORelationOrigin;
import utilities.DateTimeService;
import utilities.StringService;
import utilities.SystemService;

import entities.EntityDynamicField;
import entities.EntityDynamicFieldSubValue;
import entities.EntityDynamicFieldSubWaitExt;
import entities.EntityObjectOption;
import entities.EntityRelation;
import entities.EntityRelationOrigin;
import enums.EnumLogicSetMode;
import enums.EnumLogicSetPointerArea;
import enums.EnumMessageType;
import enums.EnumModule;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import enums.EnumRelation;
import enums.EnumSymbolType;
import exception.ExceptionAmrita;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * FunctionPgmSummary
 * </h1>
 * <p>
 * La funzione produce una documentazione completa e scalabile di un programma analizzato.<br>
 * <p>
 * Vengono letti su Object gli oggetti programma i cui tipi sono specificati nella richiesta e filtrati dalle direttive.
 * Per ogni programma viene quindi letto il descrittore serializzato che contiene tutte le informazioni
 * da esporre (tranne quelle dedotte dal data base come relazioni e opzioni)<br>
 * Il programma deve essere nello stato di oggetto analizzato senza errori.<br>
 * Per ogni programma vengono prodotti, in base all'abilitazione nelle direttive di esecuzione,
 * i seguenti gruppi di informazione.
 * <p>
 * <Ul>
 * <Li> <b>01</b> Informazioni sorgente, data e ora di analisi, status, path, etc.
 * <Li> <b>02</b> Sorgente programma codificato con numeri istruzione e copy esplosi 
 * <Li> <b>03</b> Xref Labels definite e referenziate
 * <Li> <b>04</b> Xref Internal procedures (Section) definite e referenziate
 * <Li> <b>05</b> Xref Simboli definiti e referenziati in ordine alfabetico
 * <Li> <b>06</b> Lista I-O su file system, con external file, phisical file e accesso CRUD
 * <Li> <b>07</b> Lista I-O su tabelle  SQL con accesso CRUD
 * <Li> <b>08</b> Lista I-O su segmenti DL1 con accesso CRUD
 * <Li> <b>09</b> Matrice CRUD complessiva I-O programma
 * <Li> <b>10</b> Lista relazioni, con nomi programmi origine e relative istruzioni
 * <Li> <b>11</b> Lista opzioni di programma rilevati dal processo di analisi sorgente
 * <Li> <b>12</b> Codice morto come label e section non referenziate o codice non raggiungibile
 * <Li> <b>14</b> Lista istruzioni dinamiche, parametri risolti e valori individuati 
 * <Li> <b>15</b> Lista metriche di programma 
 * </Ul>
 * <p>
 * Sono disponibili metodi pubblici per ottenere, fornito il nome del programma da deserializzare,
 * le informazioni delle singole sezioni in forma codificata e non in in un file di testo.<br>
 * E' quindi possibile utilizzare questa classe come input per le applicazioni di visualizzazione. 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
  * @since 10/04/2010
 * @see ExecutionStarter
 * @see ExecutionShared
 * @see ExecutionDispatcher
*/
public class FunctionPgmSummary extends ExecutionShared implements Runnable, AmritaConstants{

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza specifiche                                                                    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
	
    // Attenzione !! 
	// le direttive attive sono disponibili nella variabile di istanza DirectivesInfo di, della superclasse
	// valorizzata dal costruttore, che contiene anche tutte le informazioni di esecuzione.
		
	
	// Programma serializzato
	private ProgramCobol programCobol = null;
	

	// Varie
	private boolean isPgmWithDynamicCodeSpreaded = false;
	
	
	/**
	 * 
	 * Costruttore 
	 * 
	 * I file di pilot con i sources/directories da trattare e con i processi da attivare, 
	 * si trovano nella directory DirPilotAndFilter che permettono di personalizzare i processi di analisi. 
	 * Nei files di pilot sono presenti paths compleeti di sorgenti e directories da analizzare, 
	 * con eventuali commenti e informazioni di filtro.
	 * 
	 * 
	 */
	public FunctionPgmSummary(UserConfiguration sd, ArrayList<ExecutionDirectives> al_di) {
		super(sd, al_di.get(al_di.size()-1));	
		this.al_di = al_di;
 	}
	
	
   /**
    * 
    * Attivato a fronte dell'esecuzione di start() sul thread  da {@link _ExecutionLauncherFunction} <br>
    * 
    */
	public void run() {
		
		this.di.excpOccurred = null;
		this.di.isExecutionWithErrors = false;

		try {
			
			exec();
			
		} catch (Exception e) {
			// L'esecuzione è in un thread separato, l'ora di fine elaborazione è già stata impostata da execFunction.
			// Nel caso di eccezione di tipo AmritaException, è già stata loggata e lo stack trace prodotto,
			// altrimenti tutto è a cura del chiamante AnalyzeManagerFunction.
			logMessage(EnumMessageType.ERROR_FATAL, "EF0021", e);
			di.excpOccurred = e;
		}
		
	}
	

	/**
	 * 
	 * Funzione eseguita in modo sincrono o come thread separato.<br>
	 * <p>
	 * Questo metodo di ingresso permette di effettuare operazioni iniziali
	 * e finali relativi alla funzione da eseguire.<br>
	 * Per esempio vengono impostati l'ora di inzio, fine ed elapsed di elaborazione.<br>
	 * In caso di eccezione viene impostato il flag di funzione eseguita con errori
	 * nell'oggetto di direttiva di esecuzione DirectivesInfo.<br>
	 * <p>
	 *  
	 * @throws Exception 
	 * 
	 */
	public void exec() throws Exception  {

		this.di.execMsAllStart = System.currentTimeMillis();
		this.di.isExecutionWithErrors = false;
		
		execPgmSummary();
		
		this.di.execMsAllEnd = System.currentTimeMillis();
		this.di.execMsAllElapsed = (int) (this.di.execMsAllEnd - this.di.execMsAllStart);
		this.ucfg.setExecMsAllElapsed(this.di.execMsAllElapsed);
	}
	
	
 
	/**
	 * 
	 * Funzione di documentazione programma eseguito in modo sincrono o come thread separato.<br>
	 * <p>
	 * Dopo aver recuperato i nomi dei programmi da documentare, viene prodotta la documentazione sul file di output specificato.
	 * <p>
	 * @throws Exception 
	 * 
	 */
	public void execPgmSummary() throws Exception {
		
		ExecutionDirectives diCur = null;                        		// Direttiva per il programma corrente
		SystemService sys = null;                           		// Gestore generalizzato servizi di sistema
		SourceManager sm = null;                    				// Gestore generalizzato recupero sorgenti
        Object objUnserialized = null;                      		// Contiene il programma deserializzato
 		String dirOutput = null;									// Directory di output
        String fileSourcePgmSummary = null;     					// Include eventuale prefissso finale .xxx
        String pathSourcePgmSummary = null;	 						// Path completo file con nomi sorgenti in output
        String ar_RowPgmSummary[] = null;							// Righe file di output programma
        ArrayList<String> al_RowPgmSummary = null;					// Righe file di output programma
		ArrayList<InnerDescriptorSource> al_PgmToDoSummary = null; 	// Programmi da documentare
		
			
	    // Allocazione Entity e gestore sources 
		sm = new SourceManager(ucfg);
		
		
		// Estrazione centralizzata oggetti pgm da trattare
		al_PgmToDoSummary = this.getObjectsToProcess(new EnumObject[]{EnumObject.OBJECT_PGM_COBOL}
		                                           , new EnumObjectStatus[]{EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS, EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS}
		                                           , new EnumObjectOption[] {}                                          
		);

		
		// Programmi da documentare individuati.
		// Produzione file di Summary per ogni programma con le informazioni
		// abilitate nelle direttive
		    
	    // Sorgenti da analizzare individuati per la direttiva OBJECT_IDENTIFICATION_UNIT corrente
	    this.di.execTotObjectToProcess = al_PgmToDoSummary.size();
	    logMessage(EnumMessageType.INFORMATION, "MI0130", al_PgmToDoSummary.size()+"");
	    

	    // Scan programmi individuati da documentare
	    for (InnerDescriptorSource infoPgmToDoSummary : al_PgmToDoSummary) {

	    	diCur = infoPgmToDoSummary.di;
	    	
	    	// Recupero il programma deserializzato
	    	sys = new SystemService(this.ucfg, diCur);   
			sys.setSourceObjectContainer(infoPgmToDoSummary.sourceFileName);
			objUnserialized = sys.getSerialized(ucfg.getDirCobolObjPgm(), infoPgmToDoSummary.sourceFileName, SUFFIX_SERIALIZED_PGM);
	    	
			// Programma serializzato non trovato, impossibile produrre documentazione: messaggio già fornito
			if (objUnserialized == null || !(objUnserialized instanceof ProgramCobol)) {
				continue;
			}
			
			// Oggetto ProgramCobol
			this.programCobol = (ProgramCobol) objUnserialized;

			// Alloco array list con le righe del file di summary per il pgm corrente
	        al_RowPgmSummary = new ArrayList<String>();
			
			// Pgm deserializzato disponibile: carico informazioni identificative per logging in caso di abend
	        this.di.curObjectId = infoPgmToDoSummary.idObject;
	        this.di.fileNameCurObj = infoPgmToDoSummary.sourceFileName;
	        this.di.filePathCurObj = infoPgmToDoSummary.sourcePath;
	        this.di.libraryCodeCurObj = infoPgmToDoSummary.libraryCode;
	        this.di.libraryPathCurObj = infoPgmToDoSummary.libraryPath;
			
			try {
				
				this.di.execCntObjectProcessed++;
				putPgmSummarySections(infoPgmToDoSummary, programCobol, al_RowPgmSummary);   // -> alRowPgmSummary
			
			} catch (Exception e) {
				this.di.excpOccurred = e;
				this.di.isExecutionWithErrors = true;
				this.di.execCntObjectProcessedExcp++;
				this.di.exec_alObjectNameExcp.add(this.di.curObjectId);
				this.logApplicationInfoException();				// Informazioni applicative
                this.logSystemInfoException();           		// Informazioni di debug se presenti
                continue;
			}
			
			// Recupero file dir e name  di output
			dirOutput = ucfg.getDirOutput();
			fileSourcePgmSummary = infoPgmToDoSummary.idObject + "." + "summary";
			pathSourcePgmSummary = dirOutput + File.separatorChar + fileSourcePgmSummary;
			
			// Produzione file di output
			ar_RowPgmSummary = new String[al_RowPgmSummary.size()];
			ar_RowPgmSummary = al_RowPgmSummary.toArray(ar_RowPgmSummary);
	        sm.writeSource(pathSourcePgmSummary, ar_RowPgmSummary);
	    
	        // Messaggio 
	        this.logMessage(EnumMessageType.INFORMATION, "MI0115", infoPgmToDoSummary.idObject, pathSourcePgmSummary);
	        
	    }
		
		this.logFinalExecutionStatistics();    // Conteggi, elenco oggetti con errori e in exception
		
  		return;

 	}

	

    /*
     * Produzione su array list di righe, 
     * delle varie sezioni del Summary di documentazione
     * 
     * 
     */
    private void putPgmSummarySections(InnerDescriptorSource innerInfoSummary
    							  , ProgramCobol programCobol
    							  , ArrayList<String> alRowPgmSummary
    							   ) throws ExceptionAmrita, SQLException {

    	
 		putHeader(innerInfoSummary, programCobol, alRowPgmSummary);     			// Copyrigth, nome funzione, data e ora elaborazione
		putInfoSource(innerInfoSummary, programCobol, alRowPgmSummary);  			// Informazioni sorgente, data e ora di analisi, status, path, etc.
		putListOptions(innerInfoSummary, programCobol, alRowPgmSummary);			// Lista opzioni di programma rilevati dal processo di analisi sorgente
		putListRelations(innerInfoSummary, programCobol, alRowPgmSummary);			// Lista relazioni, con nomi programmi origine e relative istruzioni
		putMatrixCrud(innerInfoSummary, programCobol, alRowPgmSummary);				// Matrice CRUD complessiva I-O programma VSAM/Sql/DL1/Adabas
		putListIOFileSystem(innerInfoSummary, programCobol, alRowPgmSummary);  		// Lista I-O su file system, con external file, phisical file e accesso CRUD
		putListDynamicCode(innerInfoSummary, programCobol, alRowPgmSummary);		// Lista istruzioni dinamiche, parametri risolti e valori individuati 
		putSourceCoded(innerInfoSummary, programCobol, alRowPgmSummary);  			// Sorgente programma codificato con numeri istruzione e copy esplosi 
		putXrefLabels(innerInfoSummary, programCobol, alRowPgmSummary);  			// Xref Labels definite e referenziate
		putXrefSections(innerInfoSummary, programCobol, alRowPgmSummary);  		    // Xref Internal procedures (Section) definite e referenziate
		putXrefSymbols(innerInfoSummary, programCobol, alRowPgmSummary);  			// Xref Simboli definiti e referenziati in ordine alfabetico
		putDeadCode(innerInfoSummary, programCobol, alRowPgmSummary);				// Codice morto come label e section non referenziate o codice non raggiungibile
		putProgramMetrics(innerInfoSummary, programCobol, alRowPgmSummary);			// Metriche di programma 
    	
	}




	/*
     * Produzione header Summary
     * 
     */
	@SuppressWarnings("static-access")
	private void putHeader(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol, ArrayList<String> alRowPgmSummary) {
		
		String row = "";
		
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0001", ucfg.getAmritaActiveVersion(), ucfg.getAmritaLastModDate());
		alRowPgmSummary.add(row);
		
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0002");
		alRowPgmSummary.add(row);
		
		alRowPgmSummary.add("");
		
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0047", DateTimeService.getDateFormatted(new Date(), "dd-MM-yyyy", ucfg.getCurrentLocale()), DateTimeService.getDateFormatted(new Date(), "hh:mm:ss", ucfg.getCurrentLocale()));
		alRowPgmSummary.add(row);

		alRowPgmSummary.add("");
		alRowPgmSummary.add("");

		
		// Sezioni attive
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0074");
		alRowPgmSummary.add(row);
		alRowPgmSummary.add("");

		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0048");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0083");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0086");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0071");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0080");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0089");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0056");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0062");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0065");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0068");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0092");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0097");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0107");
		alRowPgmSummary.add(row);
	
		alRowPgmSummary.add("");
		
		isPgmWithDynamicCodeSpreaded = false;

	}

	/*
	 * 
	 * Produzione informazioni su sorgente,
	 * 
	 */
    private void putInfoSource(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol, ArrayList<String> alRowPgmSummary) {
    	String row = "";
    	
    	String programName = "";
    	String programType = "";
    	String libraryCode = "";
    	String libraryPath = "";
    	String sourcePath = "";
    	String sourceFileName = "";
    	EnumObjectStatus status = null;
    	String dtFirstAnalysis = "";
    	String tmFirstAnalysis = "";
    	String dtLastAnalysis = "";
    	String tmLastAnalysis = "";
    	
		alRowPgmSummary.add("");

		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0048");
		alRowPgmSummary.add(row);

		programName = innerPgmToDoSummary.idObject;
		programName = innerPgmToDoSummary.objectType.toString();
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0049", programName, programType);
		alRowPgmSummary.add(row);
		
		libraryCode = innerPgmToDoSummary.libraryCode;
		libraryPath = innerPgmToDoSummary.libraryPath;
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0050", libraryCode, libraryPath);
		alRowPgmSummary.add(row);

		sourcePath = innerPgmToDoSummary.sourcePath;
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0051", sourcePath);
		alRowPgmSummary.add(row);

		sourceFileName = innerPgmToDoSummary.sourceFileName;
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0052", sourceFileName);
		alRowPgmSummary.add(row);
    	
		status = innerPgmToDoSummary.objectStatus;
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0053", status.toString());
		alRowPgmSummary.add(row);
		
		dtFirstAnalysis = innerPgmToDoSummary.dtFirstAnalysis;
		tmFirstAnalysis = innerPgmToDoSummary.tmFirstAnalysis;
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0054", dtFirstAnalysis, tmFirstAnalysis);
		alRowPgmSummary.add(row);
		
		dtLastAnalysis = innerPgmToDoSummary.dtLastAnalysis;
		tmLastAnalysis = innerPgmToDoSummary.tmLastAnalysis;
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0055", dtLastAnalysis, tmLastAnalysis);
		alRowPgmSummary.add(row);
		
		alRowPgmSummary.add("");
		
	}

     /*
     * Produzione Lista opzioni di programma rilevati dal processo di analisi sorgente
     * 
     */
 	private void putListOptions(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol, ArrayList<String> alRowPgmSummary) throws ExceptionAmrita, SQLException {

 		ExecutionDirectives diCur = innerPgmToDoSummary.di;
 		String row = "";
 		String optionProgram = "";
        String whereCondition = "";
        List<EntityObjectOption> ar_objEntityObjectOption = null;
  		EntityObjectOption entityObjectOption = null;
  
        if (!diCur.optListOptions) {
 			return;
 		}

	    Connection conn = DataBaseConnections.getConnection();
		IDAOObjectOption eoDAO = (DAOImplObjectOption) AmritaStartup.sqlFactory.getDAOObjectOption(conn, false,false, ucfg);

 		alRowPgmSummary.add("");
 		alRowPgmSummary.add("");

 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0083");
 		alRowPgmSummary.add(row);
 		alRowPgmSummary.add("");
 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0084");
 		alRowPgmSummary.add(row);

         
         // Lettura opzioni di programma
 		whereCondition = whereCondition +      "OBJOSYST = '" + diCur.systemInput + "'";
 		whereCondition = whereCondition + " AND OBJOSUBS = '" + diCur.subSystemInput + "'";
 		whereCondition = whereCondition + " AND OBJOIDOB = '" + programCobol.programName + "'";
 		whereCondition = whereCondition + " AND OBJOTYPO = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
 		ar_objEntityObjectOption = eoDAO.readSetEntityWhere(whereCondition, "");
 		
 		// Scan opzioni trovate
 		for (Object object : ar_objEntityObjectOption) {
 			entityObjectOption = (EntityObjectOption) object;
 			optionProgram = entityObjectOption.getOption().toString();
 			optionProgram = StringService._pad(optionProgram, ' ', 50, StringService.PAD_RIGHT);
 			
 			row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0085", optionProgram);
 			alRowPgmSummary.add(row);
            
 			// Flag di istanza utilizzato nella sezione delle istruzioni dinamiche
  			if (entityObjectOption.getOption() == EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SPREAD) {
 				isPgmWithDynamicCodeSpreaded = true;
			}
 			
 		}
 		DataBaseConnections.releaseConnection(conn);
 		eoDAO.setConn(null);
 	}
 	
    /*
     * Produzione Lista relazioni, con nomi programmi origine e relative istruzioni
     * 
     */
 	private void putListRelations(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol, ArrayList<String> alRowPgmSummary) throws ExceptionAmrita, SQLException {

 		ExecutionDirectives diCur = innerPgmToDoSummary.di;
		String row = "";
 		String relation = "";
 		String withObjectName = "";
 		String withObjectType = "";
		String relationType = "";
		String process = "";
		String numInstrOrigin = "";
		String pgmAreaOrigin = "";
		String instrCategory = "";
		String instrPrecompiler = "";
        String whereCondition = "";
        List<EntityRelation> ar_objEntityRelation = null;
        List<EntityRelationOrigin> ar_objEntityRelationOrigin = null;
  		EntityRelation entityRelation = null;
  		EntityRelationOrigin entityRelationOrigin = null;

 		if (!diCur.optListRelationships) {
 			return;
 		}

	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAORelation = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);
		IDAORelationOrigin eoDAORelationOrigin = (DAOImplRelationOrigin) AmritaStartup.sqlFactory.getDAORelationOrigin(conn, false,false, ucfg);
 		
		alRowPgmSummary.add("");
 		alRowPgmSummary.add("");

 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0086");
 		alRowPgmSummary.add(row);
 		alRowPgmSummary.add("");
 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0087");
 		alRowPgmSummary.add(row);

         
         // Lettura relazioni di programma
 		whereCondition = whereCondition +      "sys = '" + diCur.systemInput + "'";
 		whereCondition = whereCondition + " AND subSys = '" + diCur.subSystemInput + "'";
 		whereCondition = whereCondition + " AND idObjectA = '" + this.programCobol.programName + "'";
		whereCondition = whereCondition + " AND typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";

		entityRelation = new EntityRelation();
 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, " ORDER BY relation, idObjectB");
 
 		// Scan relazioni trovate
 		for (Object objectRelations : ar_objEntityRelation) {
 			entityRelation = (EntityRelation) objectRelations;
 			
 			// Relazione
			relation = entityRelation.getRelation().toString();
 			relation = StringService._pad(relation, ' ', 30, StringService.PAD_RIGHT);
			withObjectName = entityRelation.getIdObjectB();
			withObjectName = StringService._pad(withObjectName, ' ', 10, StringService.PAD_RIGHT);
			withObjectType = entityRelation.getTypeObjectB().toString();
			withObjectType = StringService._pad(withObjectType, ' ', 20, StringService.PAD_RIGHT);
			
  			
	        // Lettura origine relazione nel programma
			whereCondition = "";
	 		whereCondition = whereCondition +      "sys = '" + diCur.systemInput + "'";
	 		whereCondition = whereCondition + " AND subSys = '" + diCur.subSystemInput + "'";
	 		whereCondition = whereCondition + " AND idObjectA = '" + this.programCobol.programName + "'";
			whereCondition = whereCondition + " AND typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
	 		whereCondition = whereCondition + " AND idObjectB = '" + entityRelation.getIdObjectB() + "'";
			whereCondition = whereCondition + " AND typeObjectB = " + entityRelation.getTypeObjectB().ordinal()+"";
			whereCondition = whereCondition + " AND relation = " + entityRelation.getRelation().ordinal()+"";

			entityRelationOrigin = new EntityRelationOrigin();
	 		ar_objEntityRelationOrigin = eoDAORelationOrigin.readSetEntityWhere(whereCondition, "");

            // Scan origine relazioni trovate
	 		for (Object objectRelationsOrigin : ar_objEntityRelationOrigin) {
	 			entityRelationOrigin = (EntityRelationOrigin) objectRelationsOrigin;
	 			
				// Origine  relazione
				relationType = entityRelationOrigin.getRelationType().toString();
				relationType = StringService._pad(relationType, ' ', 20, StringService.PAD_RIGHT);
				process = entityRelationOrigin.getRelationSource().toString();
				process = StringService._pad(process, ' ', 20, StringService.PAD_RIGHT);
				numInstrOrigin = entityRelationOrigin.getNumInstrOrigin()+"";
				numInstrOrigin = StringService._pad(numInstrOrigin, ' ', 6, StringService.PAD_RIGHT);
				pgmAreaOrigin = entityRelationOrigin.getInstrProgramArea().getValueText1();
				pgmAreaOrigin = StringService._pad(pgmAreaOrigin, ' ', 11, StringService.PAD_RIGHT);
				instrCategory = entityRelationOrigin.getInstrCategory().toString();
				instrCategory = StringService._pad(instrCategory, ' ', 25, StringService.PAD_RIGHT);
				instrPrecompiler = entityRelationOrigin.getInstrTypePrecompiler().toString();
				instrPrecompiler = StringService._pad(instrPrecompiler, ' ', 25, StringService.PAD_RIGHT);

	 			row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0088", relation,withObjectName,withObjectType,relationType,process,numInstrOrigin,pgmAreaOrigin,instrCategory,instrPrecompiler);
	 			alRowPgmSummary.add(row);

	 			// Pulizia campi relazione per origini multiple nello stesso programma
	 			relation = StringService._pad("", ' ', 30, StringService.PAD_RIGHT);
				withObjectName = StringService._pad("", ' ', 10, StringService.PAD_RIGHT);
				withObjectType = StringService._pad("", ' ', 20, StringService.PAD_RIGHT);
				relationType = StringService._pad("", ' ', 20, StringService.PAD_RIGHT);

	 		}
	 		DataBaseConnections.releaseConnection(conn);
	 		eoDAORelation.setConn(null);
	 		eoDAORelationOrigin.setConn(null);
 		}
 	}



  /*
    * Produzione Matrice CRUD complessiva I-O programma
    * 
    */
	private void putMatrixCrud(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol2, ArrayList<String> alRowPgmSummary) throws ExceptionAmrita, SQLException {

 		ExecutionDirectives diCur = innerPgmToDoSummary.di;
 		ResultSet rs = null;
 		EnumObject en_internalNameType = null;
        List<EntityRelation> ar_objEntityRelation = null;

 		String row = "";
  		String internalName = "";
 		String internalNameType = "";
 		String crudCreate = "";
 		String crudRead = "";
 		String crudUpdate = "";
 		String crudDelete = "";
  		String strSql = "";
        String whereCondition = "";
		int internalNameTypeNum = 0;
  
        if (!diCur.optListCrudMatrix) {
 			return;
 		}
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAORelation = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);
		
 		alRowPgmSummary.add("");
 		alRowPgmSummary.add("");

 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0071");
 		alRowPgmSummary.add(row);
 		alRowPgmSummary.add("");
 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0072");
 		alRowPgmSummary.add(row);

 		// Recupero entity relazionate al programma 
		strSql = "SELECT typeObjectB, idObjectB FROM Relation"; 
		strSql = strSql + " WHERE  sys = '" + diCur.systemInput + "'";
 		strSql = strSql +  " AND   subSys = '" + diCur.subSystemInput + "'";
 		strSql = strSql +  " AND   idObjectA = '" + programCobol.programName + "'";
 		strSql = strSql +  " AND   typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
 		strSql = strSql +  " AND  (relation = " + EnumRelation.PGM_ENTITY.ordinal()+"";
 		strSql = strSql +  "      ) ";
 		strSql = strSql +  " GROUP BY typeObjectB, idObjectB";
 		strSql = strSql +  " ORDER BY typeObjectB, idObjectB";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAORelation.execSqlGeneric(strSql);

		// Scan Entity trovati ordinati per tipo
		while (rs.next()) {
			
			internalNameTypeNum = rs.getInt(1);
			en_internalNameType = getDescEntityType(internalNameTypeNum);
			internalNameType = en_internalNameType.toString();
			internalName = rs.getString(2);
			internalName = StringService._pad(internalName, ' ', 35, StringService.PAD_RIGHT);
			internalNameType = StringService._pad(internalNameType, ' ', 20, StringService.PAD_RIGHT);
			
	        // Lettura relazioni di programma con entity in insert
			whereCondition = "";
	 		whereCondition = whereCondition +      "sys = '" + diCur.systemInput + "'";
	 		whereCondition = whereCondition + " AND   subSys = '" + diCur.subSystemInput + "'";
	 		whereCondition = whereCondition + " AND   idObjectA = '" + programCobol.programName + "'";
	 		whereCondition = whereCondition + " AND   typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
	 		whereCondition = whereCondition + " AND   idObjectB = '" + internalName + "'";
	 		whereCondition = whereCondition + " AND  (relation = " + EnumRelation.PGM_ENTITY_INSERT.ordinal()+"";
	 		whereCondition = whereCondition + "      )";

	 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
	 		crudCreate = " N ";
	 		if (ar_objEntityRelation.size() > 0) {
	 			crudCreate = " Y ";
			}
			
	        // Lettura relazioni di programma con entity in lettura
			whereCondition = "";
	 		whereCondition = whereCondition +      "sys = '" + diCur.systemInput + "'";
	 		whereCondition = whereCondition + " AND   subSys = '" + diCur.subSystemInput + "'";
	 		whereCondition = whereCondition + " AND   idObjectA = '" + programCobol.programName + "'";
	 		whereCondition = whereCondition + " AND   typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
	 		whereCondition = whereCondition + " AND   idObjectB = '" + internalName + "'";
	 		whereCondition = whereCondition + " AND  (relation = " + EnumRelation.PGM_ENTITY_READ.ordinal()+"";
	 		whereCondition = whereCondition + "    OR relation = " + EnumRelation.PGM_ENTITY_READPREV.ordinal()+"";
	 		whereCondition = whereCondition + "    OR relation = " + EnumRelation.PGM_ENTITY_READNEXT.ordinal()+"";
	 		whereCondition = whereCondition + "      )";

	 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
	 		crudRead = " N ";
	 		if (ar_objEntityRelation.size() > 0) {
	 			crudRead = " Y ";
			}

	        // Lettura relazioni di programma con entity in update
			whereCondition = "";
	 		whereCondition = whereCondition +      "sys = '" + diCur.systemInput + "'";
	 		whereCondition = whereCondition + " AND   subSys = '" + diCur.subSystemInput + "'";
	 		whereCondition = whereCondition + " AND   idObjectA = '" + programCobol.programName + "'";
	 		whereCondition = whereCondition + " AND   typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
	 		whereCondition = whereCondition + " AND   idObjectB = '" + internalName + "'";
	 		whereCondition = whereCondition + " AND  (relation = " + EnumRelation.PGM_ENTITY_UPDATE.ordinal()+"";
	 		whereCondition = whereCondition + "      )";

	 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
	 		crudUpdate = " N ";
	 		if (ar_objEntityRelation.size() > 0) {
	 			crudUpdate = " Y ";
			}

	        // Lettura relazioni di programma con entity in Delete
			whereCondition = "";
	 		whereCondition = whereCondition +      "sys = '" + diCur.systemInput + "'";
	 		whereCondition = whereCondition + " AND   subSys = '" + diCur.subSystemInput + "'";
	 		whereCondition = whereCondition + " AND   idObjectA = '" + programCobol.programName + "'";
	 		whereCondition = whereCondition + " AND   typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
	 		whereCondition = whereCondition + " AND   idObjectB = '" + internalName + "'";
	 		whereCondition = whereCondition + " AND  (relation = " + EnumRelation.PGM_ENTITY_DELETE.ordinal()+"";
	 		whereCondition = whereCondition + "      )";
	 		
	 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
	 		crudDelete = " N ";
	 		if (ar_objEntityRelation.size() > 0) {
	 			crudDelete = " Y ";
			}

 			row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0073", internalName,internalNameType,crudCreate,crudRead,crudUpdate,crudDelete);
 			alRowPgmSummary.add(row);
			
		} // end-for
		
		DataBaseConnections.releaseConnection(conn);
 		eoDAORelation.setConn(null);
		
	}

   /*
    * Produzione Lista I-O su file system, con external file, phisical file e accesso CRUD
    * 
    */
	private void putListIOFileSystem(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol2, ArrayList<String> alRowPgmSummary) throws ExceptionAmrita, SQLException {

 		ExecutionDirectives diCur = innerPgmToDoSummary.di;
 		ResultSet rs = null;
 		EnumObject en_internalNameType = null;
        List<EntityRelation> ar_objEntityRelation = null;
        List<EntityRelationOrigin> ar_objEntityRelationOrigin = null;

  		EntityRelationOrigin entityRelationOrigin = null;
  		
  		String row = "";
 		String internalName = "";
 		String internalNameType = "";
 		String crudCreate = "";
 		String crudRead = "";
 		String crudUpdate = "";
 		String crudDelete = "";
 		String externalName = "";
 		String phisicalName = "";
 		String jclName = "";
		String jclStep = "";
		String jclProc = "";
 		String jclLibrary = "";
 		String jclLibraryPath = "";
  		String strSql = "";
        String whereCondition = "";
		int internalNameTypeNum = 0;
  
        if (!diCur.optListIOFileSystem) {
 			return;
 		}

	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAORelation = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);
		IDAORelationOrigin eoDAORelationOrigin = (DAOImplRelationOrigin) AmritaStartup.sqlFactory.getDAORelationOrigin(conn, false,false, ucfg);

 		alRowPgmSummary.add("");
 		alRowPgmSummary.add("");

 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0080");
 		alRowPgmSummary.add(row);
 		alRowPgmSummary.add("");
 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0081");
 		alRowPgmSummary.add(row);

 		// Recupero internal files relazionati al programma 
		strSql = "SELECT idObjectB, typeObjectB FROM Relation"; 
		strSql = strSql + " WHERE  sys = '" + diCur.systemInput + "'";
 		strSql = strSql +  " AND   subSys = '" + diCur.subSystemInput + "'";
 		strSql = strSql +  " AND   idObjectA = '" + programCobol.programName + "'";
 		strSql = strSql +  " AND   typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
 		strSql = strSql +  " AND  (relation = " + EnumRelation.PGM_INTERNAL_FILE.ordinal()+"";
 		strSql = strSql +  "      ) ";
 		strSql = strSql +  " GROUP BY idObjectB, typeObjectB";
 		strSql = strSql +  " ORDER BY idObjectB";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAORelation.execSqlGeneric(strSql);
         
		// Scan Internal files trovati ordinati per nome
		while (rs.next()) {
			
			internalName = rs.getString(1);
			internalName = StringService._pad(internalName, ' ', 20, StringService.PAD_RIGHT);

			internalNameTypeNum = rs.getInt(2);
			en_internalNameType = getDescEntityType(internalNameTypeNum);
			internalNameType = en_internalNameType.toString();
			internalNameType = StringService._pad(internalNameType, ' ', 20, StringService.PAD_RIGHT);
			
	        // Lettura relazioni di programma con internal file in insert
			whereCondition = getWhereIOFileSystem(diCur, internalName, EnumRelation.PGM_INTERNAL_FILE_INSERT.ordinal());

	 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
	 		crudCreate = " N ";
	 		if (ar_objEntityRelation.size() > 0) {
	 			crudCreate = " Y ";
			}
	 		
	        // Lettura relazioni di programma con internal file in lettura
			whereCondition = getWhereIOFileSystemRead(diCur, internalName);
 
	 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
	 		crudRead = " N ";
	 		if (ar_objEntityRelation.size() > 0) {
	 			crudRead = " Y ";
			}

	        // Lettura relazioni di programma con internal file in update
			whereCondition = getWhereIOFileSystem(diCur, internalName, EnumRelation.PGM_INTERNAL_FILE_UPDATE.ordinal());
 
	 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
	 		crudUpdate = " N ";
	 		if (ar_objEntityRelation.size() > 0) {
	 			crudUpdate = " Y ";
			}

	        // Lettura relazioni di programma con internal file in Delete
			whereCondition = getWhereIOFileSystem(diCur, internalName, EnumRelation.PGM_INTERNAL_FILE_DELETE.ordinal());
	 		 
	 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
	 		crudDelete = " N ";
	 		if (ar_objEntityRelation.size() > 0) {
	 			crudDelete = " Y ";
	 		}
	 		
	 		// Individuazione external file a partire dall'internal file.
	 		// Si legge l'origine della relazione INTERNAL_FILE_EXTERNAL_FILE originata dal programma corrente
	 		// Questa relazione e la sua origine viene generata in fase di analisi del programma
	 		// Possono esserci più external file per un internal file.
			whereCondition = "";
	 		whereCondition = whereCondition + "       sys = '" + diCur.systemInput + "'";
	 		whereCondition = whereCondition + " AND   subSys = '" + diCur.subSystemInput + "'";
	 		whereCondition = whereCondition + " AND   idObjectA = '" + internalName + "'";
	 		whereCondition = whereCondition + " AND   typeObjectA =  " + EnumObject.OBJECT_INTERNAL_FILE.ordinal()+"";
	 		whereCondition = whereCondition + " AND   typeObjectB =  " + EnumObject.OBJECT_EXTERNAL_FILE.ordinal()+"";
	 		whereCondition = whereCondition + " AND   relation =  " + EnumRelation.INTERNAL_FILE_EXTERNAL_FILE.ordinal()+"";
	 		whereCondition = whereCondition + " AND   idObjectOrigin = '" + programCobol.programName + "'";
	 		whereCondition = whereCondition + " AND   typeObjectOrigin =  " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
	 		 
	 		ar_objEntityRelationOrigin = eoDAORelationOrigin.readSetEntityWhere(whereCondition, "");
	 		externalName = StringService._pad("", ' ', 15, StringService.PAD_RIGHT);
	 		
	 		// Presenti internal file nel programma
	 		if (ar_objEntityRelationOrigin.size() > 0) {
	 			entityRelationOrigin = ar_objEntityRelationOrigin.get(0);
				externalName = entityRelationOrigin.getIdObjectRelB();
	 		
		 		// Individuazione jcl in cui l'external file è utilizzato dal programma.
		 		// L'external file, per il programma che lo utilizza, può trovarsi in + di un jcl .
				// Si utilizza una sola relazione generata nell'analisi del jcl.
				// E' sufficiente la relazione EXTERNAL_FILE_JCL_SOURCE con tutte le info di incrocio
				//       RELOTYPC = OBJECT-PHISICAL-FILE 
				//       RELOIDOC = phisical file name 
				//       info1Cross = step
				//       info2Cross = Proc
				//       info3Cross = exec pgm
				// Composizione condizione Where 
				whereCondition =                      "      sys  = '" + diCur.systemInput + "'";
				whereCondition = whereCondition +	  " AND  subSys  = '" + diCur.subSystemInput + "'";
				whereCondition = whereCondition +     " AND  relation  =  " + EnumRelation.EXTERNAL_FILE_JCL_JOB.ordinal();
				whereCondition = whereCondition +     " AND  typeObjectA  =  " + EnumObject.OBJECT_EXTERNAL_FILE.ordinal();
				whereCondition = whereCondition +     " AND  idObjectA  = '" + externalName + "'";
				whereCondition = whereCondition +     " AND  info3Cross  = '" + programCobol.programName + "'";
		 		 
		 		ar_objEntityRelationOrigin = eoDAORelationOrigin.readSetEntityWhere(whereCondition, "");
		 		
		 		// External file utilizzato dal programma in qualche jcl
		 		if (ar_objEntityRelationOrigin.size() > 0) {
		 			
		            // Scan origine relazioni con jcl in cui il file esterno è dichiarato per il programma
		 		    for (int i = 0; i < ar_objEntityRelationOrigin.size(); i++) {
			 			
		 		    	entityRelationOrigin = ar_objEntityRelationOrigin.get(i);
			 			externalName = StringService._pad(externalName, ' ', 8, StringService.PAD_RIGHT);
				 		phisicalName = StringService._pad("", ' ', 40, StringService.PAD_RIGHT);
				 	 	phisicalName = entityRelationOrigin.getIdObjectCross();
				 	 	phisicalName = StringService._pad(phisicalName, ' ', 40, StringService.PAD_RIGHT);
			 	 		jclName = entityRelationOrigin.getIdObjectOrigin();
			 	 		jclName = StringService._pad(jclName, ' ', 8, StringService.PAD_RIGHT);
			 	 		jclStep = entityRelationOrigin.getInfo1Cross();
			 	 		jclStep = StringService._pad(jclStep, ' ', 8, StringService.PAD_RIGHT);
			 	 		jclProc = entityRelationOrigin.getInfo2Cross();
			 	 		jclProc = StringService._pad(jclProc, ' ', 8, StringService.PAD_RIGHT);
			 	 		jclLibrary = entityRelationOrigin.getLibrarySourceObject();
			 	 		jclLibrary = StringService._pad(jclLibrary, ' ', 10, StringService.PAD_RIGHT);
			 	 		jclLibraryPath = entityRelationOrigin.getLibrarySourcePath();
			 	 		jclLibraryPath = StringService._pad(jclLibraryPath, ' ', 40, StringService.PAD_RIGHT);
			 			
			 	 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0082"
			 					, internalName,internalNameType,crudCreate,crudRead,crudUpdate,crudDelete
			 					, externalName,phisicalName,jclName,jclStep,jclProc, jclLibrary,jclLibraryPath);
			 			alRowPgmSummary.add(row);
					
		 		    }  // end-for phisical file  

		 		} // end-if
		 		
			} // end-if

	 		
	 		// Informazioni fisiche di dettaglio da jcl non disponibili
	 		externalName = StringService._pad(externalName, ' ', 8, StringService.PAD_RIGHT);
	 		phisicalName = StringService._pad("", ' ', 40, StringService.PAD_RIGHT);
	 		jclName = StringService._pad("", ' ', 8, StringService.PAD_RIGHT);
	 		jclStep = StringService._pad("", ' ', 8, StringService.PAD_RIGHT);
	 		jclProc = StringService._pad("", ' ', 8, StringService.PAD_RIGHT);
 	 		jclLibrary = StringService._pad("", ' ', 10, StringService.PAD_RIGHT);
 	 		jclLibraryPath = StringService._pad("", ' ', 40, StringService.PAD_RIGHT);
	 		
 			row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0082"
 					, internalName,internalNameType,crudCreate,crudRead,crudUpdate,crudDelete
 					, externalName,phisicalName,jclName,jclStep,jclProc, jclLibrary,jclLibraryPath);
 			alRowPgmSummary.add(row);
 			
		} // end-for internal file

		DataBaseConnections.releaseConnection(conn);
		eoDAORelation.setConn(null);
		eoDAORelationOrigin.setConn(null);
		
	}

 
	/*
	 * Costruzione where per informazioni CRUD
	 * 
	 */
	private String getWhereIOFileSystem(ExecutionDirectives diCur, String internalName, int ordinal) {
		String whereCondition = "";

		whereCondition = whereCondition +      "sys = '" + diCur.systemInput + "'";
		whereCondition = whereCondition + " AND   subSys = '" + diCur.subSystemInput + "'";
		whereCondition = whereCondition + " AND   idObjectA = '" + programCobol.programName + "'";
		whereCondition = whereCondition + " AND   idObjectB = '" + internalName + "'";
		whereCondition = whereCondition + " AND   typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
		whereCondition = whereCondition + " AND   relation = " + ordinal+"";		
		return whereCondition;
	}

	/*
	 * Costruzione where per informazioni CRUD di letttura
	 * 
	 */
	private String getWhereIOFileSystemRead(ExecutionDirectives diCur, String internalName) {
		String whereCondition = "";

		whereCondition = whereCondition +        "sys = '" + diCur.systemInput + "'";
 		whereCondition = whereCondition + " AND   subSys = '" + diCur.subSystemInput + "'";
 		whereCondition = whereCondition + " AND   idObjectA = '" + programCobol.programName + "'";
 		whereCondition = whereCondition + " AND   idObjectB = '" + internalName + "'";
 		whereCondition = whereCondition + " AND   typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
 		whereCondition = whereCondition + " AND  (relation = " + EnumRelation.PGM_INTERNAL_FILE_READ.ordinal()+"";
 		whereCondition = whereCondition + "    OR relation = " + EnumRelation.PGM_INTERNAL_FILE_READNEXT.ordinal()+"";
		whereCondition = whereCondition + "    OR relation = " + EnumRelation.PGM_INTERNAL_FILE_READPREV.ordinal()+"";
		whereCondition = whereCondition + "    OR relation = " + EnumRelation.PGM_INTERNAL_FILE_SORT.ordinal()+"";
		whereCondition = whereCondition + "    OR relation = " + EnumRelation.PGM_INTERNAL_FILE_MERGE.ordinal()+"";
 		whereCondition = whereCondition + "      )";
		return whereCondition;
	}

   /*
    * Produzione Lista istruzioni dinamiche, parametri risolti e valori individuati 
    * 
    */
	private void putListDynamicCode(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol2, ArrayList<String> alRowPgmSummary) throws ExceptionAmrita, SQLException {

		ExecutionDirectives diCur = innerPgmToDoSummary.di;
        List<EntityDynamicField> ar_objEntityDynamicField = null;
        List<EntityDynamicFieldSubValue> ar_objEntityDynamicValue = null;
        List<EntityDynamicFieldSubWaitExt> ar_objEntityDynamicWaitingExternal = null;
 		EntityDynamicField entityDynamicField = null;
  		EntityDynamicFieldSubValue entityDynamicValue = null;
 		EntityDynamicFieldSubWaitExt entityDynamicWaitingExternal = null;
 		Instruction instruction = null;
 		ArrayList<Integer> al_numInstrSpreaded = null;
   	    
	    /////////////////////////////////////////////////////////////////////////
	    // (1) Produzione istruzioni dinamiche 
	    /////////////////////////////////////////////////////////////////////////
	

  		String row = "";
  		String whereCondition = "";
		String numInstrProc = "";
		String dynamicInstr = "";
  		String status = "";
 		String dynamicField = "";
 		String numDefField = "";
 		String precompInstr = "";
 		String precompOperand = "";
 		String dynamicValue = "";
 		
 		// Campi specifici per dati esterni
		String typeObjectExternal = "";
		String idObjectExternal	= "";
		String idFieldExternal = "";  
		String posColumnExternal = "";     
		String lengthColumnExternal	= "";
		String typeSystemFieldExternal = "";
		String cicsNameExternal	= "";
 		
 		// Campi specifici per istruzioni spreaded
        String subFieldName = "";
        String idPgmSet = "";
        String numInstrOrigin = "";
        String numInstrSet = "";
        String setMode = ""; 
        String setField = "";
        String posInField = "";
        String lngInField = "";
        String posMappedInSubFieldOrigin = "";
        String lngMappedInSubFieldOrigin = "";
        String dspInLinkageAreaOrUsingParm = "";
        String usingParm = "";
        String pointerTypeArea = "";
        String pointerDisplInLinkageArea = "";
        String setStatus = "";
		
 		
        int numInstr = 0;
        
        if (!diCur.optListDynamicCodeInfo) {
 			return;
 		}
 
	    Connection conn = DataBaseConnections.getConnection();
		IDAODynamicField eoDAODynamicField = (DAOImplDynamicField) AmritaStartup.sqlFactory.getDAODynamicField(conn, false,false, ucfg);
		IDAODynamicFieldSubValue eoDAODynamicFieldSubValue = (DAOImplDynamicFieldSubValue) AmritaStartup.sqlFactory.getDAODynamicFieldSubValue(conn, false,false, ucfg);
		IDAODynamicFieldSubWaitExt eoDAODynamicFieldSubWaitExt = (DAOImplDynamicFieldSubWaitExt) AmritaStartup.sqlFactory.getDAODynamicFieldSubWaitExt(conn, false,false, ucfg);
        
        al_numInstrSpreaded = new  ArrayList<Integer> ();
        
 		alRowPgmSummary.add("");
 		alRowPgmSummary.add("");

 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0089");
 		alRowPgmSummary.add(row);
 		alRowPgmSummary.add("");
 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0090");
 		alRowPgmSummary.add(row);

 		// Lettura istruzioni dinamiche programma da DFLD (EntityDynamicField) 
		whereCondition = "";
 		whereCondition = whereCondition +        "sys = '" + diCur.systemInput + "'";
 		whereCondition = whereCondition + " AND   subSys = '" + diCur.subSystemInput + "'";
 		whereCondition = whereCondition + " AND   idObject = '" + programCobol.programName + "'";
 		whereCondition = whereCondition + " AND   typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";

 		ar_objEntityDynamicField = eoDAODynamicField.readSetEntityWhere(whereCondition, "");
 		
 		// Programma senza istruzioni dinamiche: exit
 		if (ar_objEntityDynamicField.size() ==  0) {
 	 		DataBaseConnections.releaseConnection(conn);
 	 		eoDAODynamicField.setConn(null);
 			eoDAODynamicFieldSubValue.setConn(null);
 			eoDAODynamicFieldSubWaitExt.setConn(null);
			return;
		}
 		
        // Scan istruzioni dinamiche definite nel programma
	    for (int i = 0; i < ar_objEntityDynamicField.size(); i++) {
 	 		
	    	entityDynamicField = ar_objEntityDynamicField.get(i);
	    	
	    	numInstr = entityDynamicField.getNumInstr();
	    	instruction = (Instruction) this.programCobol.instructionProcedure(numInstr);
	    	
	    	if (instruction.isDynamicSpreaded()) {
				al_numInstrSpreaded.add(numInstr);
			}
	    	
	 		numInstrProc = numInstr+"";
	 		numInstrProc = StringService._pad(numInstrProc, ' ', 5, StringService.PAD_RIGHT);
	    	dynamicInstr = entityDynamicField.getInstrCobolType().toString();
	    	dynamicInstr = StringService._pad(dynamicInstr, ' ', 30, StringService.PAD_RIGHT);
	 		status = AmritaStartup.mm.getMessage(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "TC0004");           // Risolta
	 		if (!entityDynamicField.getSolved()) {
	 			status = AmritaStartup.mm.getMessage(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "TC0005");       // Da risolvere
	 			if (isInstrWaitingForExternalData(instruction, entityDynamicField)) {
	 				status = AmritaStartup.mm.getMessage(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "TC0006");   // In attesa di dati esterni
				}
			}
	 		status = StringService._pad(status, ' ', 25, StringService.PAD_RIGHT);
	 		dynamicField = entityDynamicField.getIdField();
	 		dynamicField = StringService._pad(dynamicField, ' ', 32, StringService.PAD_RIGHT);
	 		numDefField = entityDynamicField.getNumField()+"";
	 		numDefField = StringService._pad(numDefField, ' ', 5, StringService.PAD_RIGHT);
	 		precompInstr = entityDynamicField.getInstrPrecompType().toString();
	 		precompInstr = StringService._pad(precompInstr, ' ', 20, StringService.PAD_RIGHT);
	 		precompOperand = entityDynamicField.getInstrPrecompOprndType().toString();
	 		precompOperand = StringService._pad(precompOperand, ' ', 15, StringService.PAD_RIGHT);
	 		dynamicValue = StringService._pad("", ' ', 15, StringService.PAD_RIGHT);
	    	
	 		// Lettura valori
			whereCondition = "";
	 		whereCondition = whereCondition +        "sys = '" + diCur.systemInput + "'";
	 		whereCondition = whereCondition + " AND   subSys = '" + diCur.subSystemInput + "'";
	 		whereCondition = whereCondition + " AND   idObject = '" + programCobol.programName + "'";
	 		whereCondition = whereCondition + " AND   typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
	 		whereCondition = whereCondition + " AND   numInstr = " + numInstr;
	 		whereCondition = whereCondition + " AND   idField = '" + entityDynamicField.getIdField() + "'";
	 		whereCondition = whereCondition + " AND   idSubField = ' '";
	 
	 		ar_objEntityDynamicValue = eoDAODynamicFieldSubValue.readSetEntityWhere(whereCondition, "");
	 		
	 		// Valori trovati per il campo dell'istruzione dinamica: output 
	 		if (ar_objEntityDynamicValue.size() > 0) {
				for (Object obj : ar_objEntityDynamicValue) {
					entityDynamicValue = (EntityDynamicFieldSubValue) obj;
					dynamicValue = entityDynamicValue.getValue();
					dynamicValue = StringService._pad(dynamicValue, ' ', 15, StringService.PAD_RIGHT);
		 			row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0091", numInstrProc,dynamicInstr,status,dynamicField,numDefField,precompInstr,precompOperand,dynamicValue);
		  			alRowPgmSummary.add(row);
		  			
		  			// Dalla riga successiva si visualizzano solo i valori
			 		numInstrProc = StringService._pad("", ' ', 5, StringService.PAD_RIGHT);
			    	dynamicInstr = StringService._pad("", ' ', 30, StringService.PAD_RIGHT);
			 		status = StringService._pad("", ' ', 25, StringService.PAD_RIGHT);
			 		dynamicField = StringService._pad("", ' ', 32, StringService.PAD_RIGHT);
			 		numDefField = StringService._pad("", ' ', 5, StringService.PAD_RIGHT);
			 		precompInstr = StringService._pad("", ' ', 20, StringService.PAD_RIGHT);
			 		precompOperand = StringService._pad("", ' ', 15, StringService.PAD_RIGHT);
				}
				continue;   // Next dynamic field
			}
	 		
	 		// Nessun valore trovato per il campo dell'istruzione dinamica
 			row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0091", numInstrProc,dynamicInstr,status,dynamicField,numDefField,precompInstr,precompOperand,dynamicValue);
  			alRowPgmSummary.add(row);
		}    

	    ////////////////////////////////////////////////////////////////////////////
	    // (2) Produzione dettaglio media esterni istruzioni dinamiche, se presente
	    ////////////////////////////////////////////////////////////////////////////
	
 		// Lettura valori
		whereCondition = "";
 		whereCondition = whereCondition +        "sys = '" + diCur.systemInput + "'";
 		whereCondition = whereCondition + " AND   subSys = '" + diCur.subSystemInput + "'";
 		whereCondition = whereCondition + " AND   idObject = '" + programCobol.programName + "'";
 		whereCondition = whereCondition + " AND   typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
 		 
 		ar_objEntityDynamicWaitingExternal = eoDAODynamicFieldSubWaitExt.readSetEntityWhere(whereCondition, "");

 		// Nessuna istruzione necessita di dati da media esterni
		if (ar_objEntityDynamicWaitingExternal.size() == 0) {
	 		DataBaseConnections.releaseConnection(conn);
	 		eoDAODynamicField.setConn(null);
			eoDAODynamicFieldSubValue.setConn(null);
			eoDAODynamicFieldSubWaitExt.setConn(null);
			return;
		}
 		
 		// Intestazioni
 		alRowPgmSummary.add("");
 		alRowPgmSummary.add("");

 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0137");
 		alRowPgmSummary.add(row);
 		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0138");
		alRowPgmSummary.add(row);
		
		
        // Scan istruzioni dinamiche risolte/risolvibili con contributi da media esterni
	    for (int i = 0; i < ar_objEntityDynamicWaitingExternal.size(); i++) {
 	 		
	    	entityDynamicWaitingExternal = ar_objEntityDynamicWaitingExternal.get(i);
	    	
	    	numInstr = entityDynamicWaitingExternal.getNumInstr();
	 		numInstrProc = numInstr+"";
	 		numInstrProc = StringService._pad(numInstrProc, ' ', 5, StringService.PAD_RIGHT);
	 		dynamicField = entityDynamicWaitingExternal.getIdField();
	 		dynamicField = StringService._pad(dynamicField, ' ', 25, StringService.PAD_RIGHT);
			typeObjectExternal = entityDynamicWaitingExternal.getTypeObjectExternal().toString();
			typeObjectExternal = StringService._pad(typeObjectExternal, ' ', 25, StringService.PAD_RIGHT);
			idObjectExternal = entityDynamicWaitingExternal.getIdObjectExternal();
			idObjectExternal = StringService._pad(idObjectExternal, ' ', 35, StringService.PAD_RIGHT);
			idFieldExternal = entityDynamicWaitingExternal.getIdFieldExternal();
			idFieldExternal = StringService._pad(idFieldExternal, ' ', 30, StringService.PAD_RIGHT);
			posColumnExternal = entityDynamicWaitingExternal.getPosColumnExternal()+"";
			posColumnExternal = StringService._pad(posColumnExternal, ' ', 8, StringService.PAD_RIGHT);
			lengthColumnExternal = entityDynamicWaitingExternal.getLengthColumnExternal()+"";
			lengthColumnExternal = StringService._pad(lengthColumnExternal, ' ', 8, StringService.PAD_RIGHT);
			typeSystemFieldExternal = entityDynamicWaitingExternal.getTypeSystemFieldExternal().toString();
			typeSystemFieldExternal = StringService._pad(typeSystemFieldExternal, ' ', 25, StringService.PAD_RIGHT);
			cicsNameExternal = entityDynamicWaitingExternal.getCicsNameExternal();
			cicsNameExternal = StringService._pad(cicsNameExternal, ' ', 8, StringService.PAD_RIGHT);
			
 			row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0139" ,numInstrProc
																				 					,dynamicField
																				 					,typeObjectExternal
																				 					,idObjectExternal
																				 					,idFieldExternal
																				 					,posColumnExternal
																				 					,lengthColumnExternal
																									,typeSystemFieldExternal
																									,cicsNameExternal
																				 					);
  			alRowPgmSummary.add(row);

	    }
 		
 		alRowPgmSummary.add("");
 		alRowPgmSummary.add("");

        if (!isPgmWithDynamicCodeSpreaded) {
     		DataBaseConnections.releaseConnection(conn);
     		eoDAODynamicField.setConn(null);
    		eoDAODynamicFieldSubValue.setConn(null);
    		eoDAODynamicFieldSubWaitExt.setConn(null);
			return;
		}
 		
        
	    ////////////////////////////////////////////////////////////////////////////
	    // (3) Produzione dettaglio assegnazioni spreaded
	    ////////////////////////////////////////////////////////////////////////////
	
 		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0140");
 		alRowPgmSummary.add(row);
 		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0141");
		alRowPgmSummary.add(row);
		
		// Scan istruzioni spreaded
		for (Integer numInstrSpreaded : al_numInstrSpreaded) {

			instruction = this.programCobol.entryProcedure(numInstrSpreaded).getInstruction();
			numInstrProc = numInstrSpreaded.toString();

			// Scan campi dinamici
			for (String dynamicOperandName : instruction.getDynamicOperandNames()) {
				// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
	 			ArrayList<LogicDynamicFieldSub> al_innerSubField = this.programCobol.getLogicInfoDynamic().getDynamicFieldsSub(numInstrSpreaded, dynamicOperandName);
				dynamicField = dynamicOperandName;

				// Scan SOTTOCAMPI campo in programma origine (presente anche in caso di campo non di gruppo)
				for (LogicDynamicFieldSub innerSubField : al_innerSubField) {

					subFieldName = innerSubField.dataItemIdentifierSubField.getNameIdentifier();

					// Scan assegnazioni sottocampo spreaded ancora da risolvere generate nel programma ORIGINE e aggiornate
					for (LogicDynamicFieldSubSetting lastSetSpreaded : innerSubField.al_lastSetTotal) {

						// Informazioni
						idPgmSet = lastSetSpreaded.entityDynamicFieldSetting.getIdPgmSet();
						numInstrOrigin = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrOriginSpreaded()+"";
						numInstrSet = lastSetSpreaded.entityDynamicFieldSetting.getNumInstrSet()+"";
						setMode = lastSetSpreaded.entityDynamicFieldSetting.getSetMode().toString().substring(9);
						setField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverId();
						posInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos()+"";
						lngInField = lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng()+"";
						posMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField()+"";
						lngMappedInSubFieldOrigin = lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField()+"";
						dspInLinkageAreaOrUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea()+"";
						usingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm()+"";
						if (lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea() == EnumLogicSetPointerArea.POINTER_INSIDE_USING_PARM) {
							usingParm = lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer()+"";
						}
						if (usingParm.equals("0")) {
							usingParm = "";
						}
						if (lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM) {
							dspInLinkageAreaOrUsingParm = lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm()+"";
						}
					    pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea().toString();
					    if (lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea() == EnumLogicSetPointerArea.NOT_ASSIGNED) {
					    	pointerTypeArea = "";
						} else {
							pointerTypeArea = lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea().toString().substring(15);
						}
					    pointerDisplInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea()+"";
						if (pointerDisplInLinkageArea.equals("0")) {
							pointerDisplInLinkageArea = "";
						}
						if (lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea() == EnumLogicSetPointerArea.POINTER_INSIDE_USING_PARM) {
							pointerDisplInLinkageArea = lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm()+"";
						}
					    setStatus = "";
						if (lastSetSpreaded.lastSetSolved) {
							setStatus = AmritaStartup.mm.getMessage(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "TC0004");
						}
						if (!lastSetSpreaded.lastSetSolved) {
							setStatus = AmritaStartup.mm.getMessage(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "TC0005");
							// Ultima assegnazione indica waiting di dati esterni
							if (lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM
							||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_READ           	 		    	 
							||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_RECEIVE_MAP     
							||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM          	 	
							||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TS_QUEUE          	
							||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TD_QUEUE             
							||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRMID             		
							||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRNID             		
							||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_SQL_SELECT                 	
							||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_FILE_GET) {
								setStatus = AmritaStartup.mm.getMessage(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "TC0006");
							}
						}
						
						// Formattazione
				 		numInstrProc = StringService._pad(numInstrProc, ' ', 5, StringService.PAD_RIGHT);
				 		dynamicField = StringService._pad(dynamicField, ' ', 25, StringService.PAD_RIGHT);
				 		subFieldName = StringService._pad(subFieldName, ' ', 25, StringService.PAD_RIGHT);
				 		idPgmSet = StringService._pad(idPgmSet, ' ', 8, StringService.PAD_RIGHT);
				 		numInstrOrigin = StringService._pad(numInstrOrigin, ' ', 5, StringService.PAD_RIGHT);
				 		numInstrSet = StringService._pad(numInstrSet, ' ', 5, StringService.PAD_RIGHT);
				 		setMode = StringService._pad(setMode, ' ', 21, StringService.PAD_RIGHT);
				 		setField = StringService._pad(setField, ' ', 25, StringService.PAD_RIGHT);
				 		posInField = StringService._pad(posInField, ' ', 5, StringService.PAD_RIGHT);
				 		lngInField = StringService._pad(lngInField, ' ', 5, StringService.PAD_RIGHT);
				 		posMappedInSubFieldOrigin = StringService._pad(posMappedInSubFieldOrigin, ' ', 5, StringService.PAD_RIGHT);
				 		lngMappedInSubFieldOrigin = StringService._pad(lngMappedInSubFieldOrigin, ' ', 5, StringService.PAD_RIGHT);
						dspInLinkageAreaOrUsingParm = StringService._pad(dspInLinkageAreaOrUsingParm, ' ', 5, StringService.PAD_RIGHT);
						usingParm = StringService._pad(usingParm, ' ', 5, StringService.PAD_RIGHT);
				        pointerTypeArea = StringService._pad(pointerTypeArea, ' ', 16, StringService.PAD_RIGHT);
				        pointerDisplInLinkageArea = StringService._pad(pointerDisplInLinkageArea, ' ', 5, StringService.PAD_RIGHT);
				 		setStatus = StringService._pad(setStatus, ' ', 25, StringService.PAD_RIGHT);
						
			 			row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0142" 
																							 					,numInstrProc
																							 					,dynamicField
																							 					,subFieldName
																							 					,idPgmSet
																							 					,numInstrOrigin
																							 					,numInstrSet
																							 					,setMode
																							 					,setField
																							 					,posInField
																							 					,lngInField
																							 					,posMappedInSubFieldOrigin
																							 					,lngMappedInSubFieldOrigin
																							 					,dspInLinkageAreaOrUsingParm
																							 					,usingParm
																							 					,pointerTypeArea
																							 					,pointerDisplInLinkageArea
																							 					,setStatus
																							 					);
			 			alRowPgmSummary.add(row);
						
			 			numInstrProc = "";
			 			dynamicField = "";
			 			subFieldName = "";
			 			
		 			} // end-for last-set

				} // end-for sottocampi
				
			} // end-for campi dinamici
			
		} // end-for istruzioni spreaded
		
 		alRowPgmSummary.add("");
 		alRowPgmSummary.add("");

 		DataBaseConnections.releaseConnection(conn);
 		eoDAODynamicField.setConn(null);
		eoDAODynamicFieldSubValue.setConn(null);
		eoDAODynamicFieldSubWaitExt.setConn(null);
		
 		
	}
			
    /*
     * Restituisce true se qualche sottocampo ha una ultima assegnazione da media esterni
     * 
     */
	private boolean isInstrWaitingForExternalData(Instruction instruction, EntityDynamicField entityDynamicField) {

 		ArrayList<LogicDynamicFieldSub> al_innerSubField = null;
   		
		// Scan campi dinamici
		for (String dynamicOperandName : instruction.getDynamicOperandNames()) {
			
			// Estrazione sottocampi elementari disponibili in LogicInfoDynamic del programma ORIGINE
//			al_innerSubField = this.programCobol.getLogicInfoDynamic().getDynamicFieldsSub(instruction.getNumInstr(), dynamicOperandName);

			// Scan SOTTOCAMPI campo in programma origine (presente anche in caso di campo non di gruppo)
			for (LogicDynamicFieldSub innerSubField : al_innerSubField) {

				// Scan assegnazioni sottocampo spreaded ancora da risolvere generate nel programma ORIGINE e aggiornate
				for (LogicDynamicFieldSubSetting lastSetSpreaded : innerSubField.al_lastSetTotal) {
                    
					// Ultima assegnazione risolta
					if (lastSetSpreaded.lastSetSolved) {
						continue;
					}
					
					// Ultima assegnazione indica waiting di dati esterni
					if (!lastSetSpreaded.lastSetSolved) {
						if (lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM
						||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_READ           	 		    	 
						||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_RECEIVE_MAP     
						||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM          	 	
						||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TS_QUEUE          	
						||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TD_QUEUE             
						||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRMID             		
						||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRNID             		
						||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_SQL_SELECT                 	
						||	lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_FILE_GET) {
							return true;
						}
					} 
						
	 			} // end-for last-set-spreaded

				// Scan assegnazioni sottocampo spreaded ancora da risolvere generate nel programma ORIGINE e aggiornate
				for (LogicDynamicFieldSubSetting lastSetLocal : innerSubField.al_lastSetTotal) {
                    
					// Ultima assegnazione risolta
					if (lastSetLocal.lastSetSolved) {
						continue;
					}
					
					// Ultima assegnazione indica waiting di dati esterni
					if (!lastSetLocal.lastSetSolved) {
						if (lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM
						||	lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_READ           	 		    	 
						||	lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_RECEIVE_MAP     
						||	lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM          	 	
						||	lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TS_QUEUE          	
						||	lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TD_QUEUE             
						||	lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRMID             		
						||	lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRNID             		
						||	lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_SQL_SELECT                 	
						||	lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_FILE_GET) {
							return true;
						}
					} 
						
	 			} // end-for last-set-spreaded

			
				
				
			} // end-for sottocampi
			
		} // end-for campi dinamici

	return false;
}


	/*
     * Produzione sorgente codificato.
     * Viene prima prodotta una legenda.
     * 
     */
	private void putSourceCoded(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol, ArrayList<String> alRowPgmSummary) {
		
		String row = "";
		
		alRowPgmSummary.add("");

		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0056");
		alRowPgmSummary.add(row);
		alRowPgmSummary.add("");

		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0057");
		alRowPgmSummary.add(row);
		alRowPgmSummary.add("");

		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0134");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0135");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0136");
		alRowPgmSummary.add(row);
		alRowPgmSummary.add("");

		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0060");
		alRowPgmSummary.add(row);

		// Estrazione e output delle istruzioni codificate nelle varie divisioni
		putSourceCodedByCobolDivision(this.programCobol.entriesIdentification(), alRowPgmSummary);
		putSourceCodedByCobolDivision(this.programCobol.entriesEnvironment(), alRowPgmSummary);
		putSourceCodedByCobolDivision(this.programCobol.entriesData(), alRowPgmSummary);
		putSourceCodedByCobolDivision(this.programCobol.entriesProcedure(), alRowPgmSummary);

	}

	
   /*
    * Output delle istruzioni della divisione Cobol specifica.
    * 	
    */
   private void putSourceCodedByCobolDivision(ProgramCobolEntry<? extends Instruction>[] ar_entryCobolDivision, ArrayList<String> alRowPgmSummary) {


		ProgramCobolEntry<? extends Instruction> entryGeneric  = null;
		Instruction instructionGeneric = null;
		String ar_rowInstruction[] = null;
		String row = "";

		String instr = "";
		String instrRelated = "";
		String underCopyName = "";
		String replacedBy = "";
		String dynamic = "";
		String dynamicSolved = "";
		String dynamicSolvedFull = "";
		String dynamicSpreaded = "";
		String dynamicWaitingForData = "";
		String rowSource = "";

		int instrPrecRowNumber = -1;
		
		
		// Scan istruzioni divisione
		for (int i = 0; i < ar_entryCobolDivision.length; i++) {
			
			entryGeneric = ar_entryCobolDivision[i];
			instructionGeneric = entryGeneric.getInstruction();

			// Istruzione su stessa riga istruzione precedente: skip
			if (instrPrecRowNumber == instructionGeneric.getRowStartSource()) {
				continue;
			}
			
			
			// Righe commento prima di istruzione
			ar_rowInstruction = instructionGeneric.getCommentsBeforeNotEmpty();
			for (int j = 0; j < ar_rowInstruction.length; j++) {
				instr = "     ";					// 5
				instrRelated = "     ";				// 5
				underCopyName = "          ";		// 10
				replacedBy = "   ";		            // 3
				dynamic = "   ";				    // 3
				dynamicSolved = "   ";		        // 3
				dynamicSolvedFull = "   ";	        // 3
				dynamicSpreaded = "   ";	        // 3
				dynamicWaitingForData = "   ";      // 3
				rowSource = ar_rowInstruction[j];
				underCopyName = entryGeneric.getUnderCopyName();
				underCopyName = StringService._pad(underCopyName, ' ', 10, StringService.PAD_RIGHT);
				
				try {
					row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0061",instr,instrRelated,underCopyName,replacedBy,dynamic,dynamicSolved,dynamicSolvedFull,dynamicSpreaded,dynamicWaitingForData,rowSource);
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				alRowPgmSummary.add(row);
			}

			// Informazioni istruzione 
			instr = StringService._pad(instructionGeneric.getNumInstr()+"", '0', 5, StringService.PAD_LEFT);
			instrRelated = StringService._pad(entryGeneric.getNumInstrRelated()+"", '0', 5, StringService.PAD_LEFT);
			if (instrRelated.equals("00000")) {
				instrRelated = "     ";
			}
			underCopyName = entryGeneric.getUnderCopyName();
			underCopyName = StringService._pad(underCopyName, ' ', 10, StringService.PAD_RIGHT);
			replacedBy = "No ";                 // 3
			dynamic = "No ";                    // 3
			dynamicSolved = "   ";		        // 3
			dynamicSolvedFull = "   ";	        // 3
			dynamicSpreaded = "   ";	        // 3
			dynamicWaitingForData = "   ";	    // 3
			if (entryGeneric.isReplacedBy()) {
				replacedBy = "Yes";
			}
			if (instructionGeneric.isDynamic()) {
				dynamic = "Yes";
				if (instructionGeneric.isDynamicSolvedFull()) {
					dynamicSolvedFull = "Yes";
				} else {
					dynamicSolvedFull = "No ";
				}
				if (instructionGeneric.isDynamicSolved()) {
					dynamicSolved = "Yes";
				} else {
					dynamicSolved = "No ";
				}
				if (instructionGeneric.isDynamicSolvedFull()) {
					dynamicSolvedFull = "Yes";
				} else {
					dynamicSolvedFull =  "No ";
				}
				if (instructionGeneric.isDynamicSpreaded()) {
					dynamicSpreaded = "Yes";
				} else {
					dynamicSpreaded = "No ";
				}
				if (instructionGeneric.isDynamicWaitingForData()) {
					dynamicWaitingForData= "Yes";
				} else {
					dynamicWaitingForData = "No ";
				}
			}

			// Righe istruzione
			ar_rowInstruction = instructionGeneric.getRowsSource();
			for (int j = 0; j < ar_rowInstruction.length; j++) {
				
				rowSource = ar_rowInstruction[j];
				
				row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0061",instr,instrRelated,underCopyName,replacedBy,dynamic,dynamicSolved,dynamicSolvedFull,dynamicSpreaded,dynamicWaitingForData,rowSource);
				alRowPgmSummary.add(row);
				
			} // end-for

			// Impostazione riga istruzione precedente
			instrPrecRowNumber = instructionGeneric.getRowStartSource();
			
		} // end-for

	}


/*
    * Produzione cross reference labels
    * 
    */
	private void putXrefLabels(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol, ArrayList<String> alRowPgmSummary) {

		ExecutionDirectives diCur = innerPgmToDoSummary.di;
		String ar_labelName[] = null;
		ArrayList<String> al_labelReferences = null;
	   	String row = "";
	   	String labelName = "";
    	String labelDefInstr = "";
    	String labelReferences = "";
    	int ar_XrefLabel[] = null;
        int numDefInstr = 0;
    	
		if (!diCur.optListXrefLabel) {
			return;
		}
       	
		alRowPgmSummary.add("");
		alRowPgmSummary.add("");

		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0062");
		alRowPgmSummary.add(row);
		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0063");
		alRowPgmSummary.add(row);
		
		ar_labelName = this.programCobol.labelNames();
		Arrays.sort(ar_labelName);
		al_labelReferences = new ArrayList<String> ();
		
		// Scan label
		for (String label : ar_labelName) {
			labelName = StringService._pad(label, ' ', 32, StringService.PAD_RIGHT);
			numDefInstr = this.programCobol.labelPointer(label);
			labelDefInstr = "*****";
			labelReferences = StringService._pad("", '*', 20, StringService.PAD_RIGHT);
			al_labelReferences.clear();
			
			// Formattazione informazioni
			if (numDefInstr > 0) {
				
				labelDefInstr = StringService._pad(numDefInstr+"", ' ', 5, StringService.PAD_RIGHT);
				ar_XrefLabel = this.programCobol.xrefToLabel(label);
				labelReferences = "";
				
				// Scompattamento xref in righe di lunghezza massima
				for (int i = 0; i < ar_XrefLabel.length; i++) {
					labelReferences = labelReferences + ar_XrefLabel[i]+" ";
					if (labelReferences.length() > 85) {
						al_labelReferences.add(labelReferences);
						labelReferences = "";
					}
				} // end-for
				
				al_labelReferences.add(labelReferences);
				
				// Scan righe di xref per la label e output delle stesse
				for (String labelXref : al_labelReferences) {
					labelReferences = StringService._pad(labelXref, ' ', 90, StringService.PAD_RIGHT);
					row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0064", labelName, labelDefInstr, labelReferences);
					alRowPgmSummary.add(row);
					
					// Nelle righe successive non vengono ripetute label e numeri di idefinizione
					labelName = StringService._pad(label, ' ', 32, StringService.PAD_RIGHT);
					labelDefInstr = "     ";
				} // end-for
				
			} // end-if
			
		} // end-for
	}


   /*
    * Produzione cross reference sections
    * 
    */
	private void putXrefSections(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol2, ArrayList<String> alRowPgmSummary) {

		ExecutionDirectives diCur = innerPgmToDoSummary.di;
		String ar_sectionName[] = null;
		ArrayList<String> al_sectionReferences = null;
	   	String row = "";
	   	String sectionName = "";
    	String sectionDefInstr = "";
    	String sectionReferences = "";
    	int ar_XrefSection[] = null;
        int numDefInstr = 0;
    	
		if (!diCur.optListXrefSection) {
			return;
		}
       	
		alRowPgmSummary.add("");
		alRowPgmSummary.add("");

		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0065");
		alRowPgmSummary.add(row);
		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0066");
		alRowPgmSummary.add(row);
		
		ar_sectionName = this.programCobol.procInternalNames();
		Arrays.sort(ar_sectionName);
		al_sectionReferences = new ArrayList<String> ();
		
		// Scan section
		for (String section : ar_sectionName) {
			sectionName = StringService._pad(section, ' ', 32, StringService.PAD_RIGHT);
			numDefInstr = this.programCobol.sectionPointer(section);
			sectionDefInstr = "*****";
			sectionReferences = StringService._pad("", '*', 20, StringService.PAD_RIGHT);
			al_sectionReferences.clear();
			
			// Formattazione informazioni
			if (numDefInstr > 0) {
				
				sectionDefInstr = StringService._pad(numDefInstr+"", ' ', 5, StringService.PAD_RIGHT);
				ar_XrefSection = this.programCobol.xrefToSection(section);
				sectionReferences = "";
				
				// Scompattamento xref in righe di lunghezza massima
				for (int i = 0; i < ar_XrefSection.length; i++) {
					sectionReferences = sectionReferences + ar_XrefSection[i]+" ";
					if (sectionReferences.length() > 85) {
						al_sectionReferences.add(sectionReferences);
						sectionReferences = "";
					}
				} // end-for
				
				al_sectionReferences.add(sectionReferences);
				
				// Scan righe di xref per la section e output delle stesse
				for (String sectionXref : al_sectionReferences) {
					sectionReferences = StringService._pad(sectionXref, ' ', 90, StringService.PAD_RIGHT);
					row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0067", sectionName, sectionDefInstr, sectionReferences);
					alRowPgmSummary.add(row);
					
					// Nelle righe successive non vengono ripetute section e numeri di idefinizione
					sectionName = StringService._pad(section, ' ', 32, StringService.PAD_RIGHT);
					sectionDefInstr = "     ";
				} // end-for
				
			} // end-if
			
		} // end-for
	}


   /*
    * Produzione cross reference simboli (costanti, campi)
    * 
    */
	private void putXrefSymbols(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol2, ArrayList<String> alRowPgmSummary) {

		ExecutionDirectives diCur = innerPgmToDoSummary.di;
		ProgramCobolEntry<? extends Instruction> entryDataDivision = null;
		String ar_symbolName[] = null;
		ArrayList<String> al_symbolReferences = null;
		int ar_pointerToDefInEnv[] = null;
		int ar_pointerToDefInData[] = null;
		int ar_pointerToDefInProc[] = null;
		int ar_xrefToSymbolInEnvInp[] = null;
		int ar_xrefToSymbolInDataInp[] = null;
		int ar_xrefToSymbolInProcInp[] = null;
		int ar_xrefToSymbolInProcOut[] = null;
	   	String row = "";
	   	String symbolName = "";
	   	String symbolNameBlank = "";
    	String symbolDefSection = "";
    	String symbolDefSectionBlank = "";
    	String symbolReferences = "";
    	String symbolTypeDesc = "";
    	String symbolTypeDescBlank = "";
    	EnumSymbolType symbolType = null;
    	
        int numDefInstr = 0;
    	
		if (!diCur.optListXrefSymbols) {
			return;
		}
       	
		alRowPgmSummary.add("");
		alRowPgmSummary.add("");

		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0068");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0057");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0058");
		alRowPgmSummary.add(row);
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0059");
		alRowPgmSummary.add(row);
		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0069");
		alRowPgmSummary.add(row);
		
		ar_symbolName = this.programCobol.symbolNames();
		Arrays.sort(ar_symbolName);
		al_symbolReferences = new ArrayList<String> ();
		
		symbolNameBlank = StringService._pad("", ' ', 32, StringService.PAD_RIGHT);
		symbolDefSectionBlank = StringService._pad("", ' ', 15, StringService.PAD_RIGHT);
		symbolTypeDescBlank = StringService._pad("", ' ', 20, StringService.PAD_RIGHT);
		
		// Scan symbol
		for (String symbol : ar_symbolName) {
			
			// Tipo simbolo e clear struttura cumulativa references per riga
			symbolType = this.programCobol.symbolType(symbol);
			al_symbolReferences.clear();
			symbolReferences = "";
			
			// Estrazione di tutti i possibili cross references per il simbolo valorizzati in analisi
			ar_pointerToDefInEnv = this.programCobol.getXrefSymbolDefEnv(symbol);
			ar_pointerToDefInData = this.programCobol.getXrefSymbolDefData(symbol);
			ar_pointerToDefInProc = this.programCobol.getXrefSymbolDefProc(symbol);
			ar_xrefToSymbolInEnvInp = this.programCobol.xrefToSymbolInEnvDivision(symbol);
			ar_xrefToSymbolInDataInp = this.programCobol.xrefToSymbolInDataDivision(symbol);
			ar_xrefToSymbolInProcInp = this.programCobol.xrefToSymbolInProcedure(symbol, INSTR_USE_DATA_ITEM_INPUT);
			ar_xrefToSymbolInProcOut = this.programCobol.xrefToSymbolInProcedure(symbol, INSTR_USE_DATA_ITEM_OUTPUT);
			
			// Estrazione numero definizione e impostazione
			symbolName = StringService._pad(symbol, ' ', 32, StringService.PAD_RIGHT);
			symbolTypeDesc = symbolType.toStringShort();
			symbolTypeDesc = StringService._pad(symbolTypeDesc, ' ', 20, StringService.PAD_RIGHT);
			
			// Definizione simbolo, tipo e sezione di appartenenza
			symbolDefSection = StringService._pad("", ' ', 15, StringService.PAD_RIGHT);
			if (ar_pointerToDefInData != null) {
				numDefInstr = ar_pointerToDefInData[0];
				entryDataDivision = this.programCobol.entryDataDivision(numDefInstr);
				symbolDefSection = entryDataDivision.getProgramSection().toString();
				symbolDefSection = StringService._pad(symbolDefSection, ' ', 15, StringService.PAD_RIGHT);
			} // end-if

			///////////////////////////////////////
			// Xref a definizioni in data division
            ///////////////////////////////////////
			if (ar_pointerToDefInData != null) {
				// Scompattamento xref in righe di lunghezza massima
				for (int i = 0; i < ar_pointerToDefInData.length; i++) {
					symbolReferences = symbolReferences + ar_pointerToDefInData[i]+"DD"+" ";
					if (symbolReferences.length() > 85) {
						al_symbolReferences.add(symbolReferences);
						symbolReferences = "";
					}
				} // end-for
			} // end-if
			
			///////////////////////////////////////////////
			// Xref a definizioni in environment division
			///////////////////////////////////////////////
			if (ar_pointerToDefInEnv != null) {
				// Scompattamento xref in righe di lunghezza massima
				for (int i = 0; i < ar_pointerToDefInEnv.length; i++) {
					symbolReferences = symbolReferences + ar_pointerToDefInEnv[i]+"DE"+" ";
					if (symbolReferences.length() > 85) {
						al_symbolReferences.add(symbolReferences);
						symbolReferences = "";
					}
				} // end-for
			} // end-if

			///////////////////////////////////////////////
			// Xref a definizioni in procedure division
			///////////////////////////////////////////////
			if (ar_pointerToDefInProc != null) {
				// Scompattamento xref in righe di lunghezza massima
				for (int i = 0; i < ar_pointerToDefInProc.length; i++) {
					symbolReferences = symbolReferences + ar_pointerToDefInProc[i]+"DP"+" ";
					if (symbolReferences.length() > 85) {
						al_symbolReferences.add(symbolReferences);
						symbolReferences = "";
					}
				} // end-for
			} // end-if


			////////////////////////////////////////////////////
			// Xref a utilizzi in input in environment division
			////////////////////////////////////////////////////
			if (ar_xrefToSymbolInEnvInp != null) {
				// Scompattamento xref in righe di lunghezza massima
				for (int i = 0; i < ar_xrefToSymbolInEnvInp.length; i++) {
					symbolReferences = symbolReferences + ar_xrefToSymbolInEnvInp[i]+"DP"+" ";
					if (symbolReferences.length() > 85) {
						al_symbolReferences.add(symbolReferences);
						symbolReferences = "";
					}
				} // end-for
			} // end-if

			
			
			//////////////////////////////////////////////
			// Xref a utilizzi in input in data division
			//////////////////////////////////////////////
			if (ar_xrefToSymbolInDataInp != null) {
				// Scompattamento xref in righe di lunghezza massima
				for (int i = 0; i < ar_xrefToSymbolInDataInp.length; i++) {
					symbolReferences = symbolReferences + ar_xrefToSymbolInDataInp[i]+"ID"+" ";
					if (symbolReferences.length() > 85) {
						al_symbolReferences.add(symbolReferences);
						symbolReferences = "";
					}
				} // end-for
			} // end-if
			
			
			///////////////////////////////////////////////////
			// Xref a utilizzi in input in procedure division
			///////////////////////////////////////////////////
			if (ar_xrefToSymbolInProcInp != null) {
				// Scompattamento xref in righe di lunghezza massima
				for (int i = 0; i < ar_xrefToSymbolInProcInp.length; i++) {
					symbolReferences = symbolReferences + ar_xrefToSymbolInProcInp[i]+"IP"+" ";
					if (symbolReferences.length() > 85) {
						al_symbolReferences.add(symbolReferences);
						symbolReferences = "";
					}
				} // end-for
			} // end-if
			
			
			///////////////////////////////////////////////////
			// Xref a utilizzi in output in procedure division
			///////////////////////////////////////////////////
			
			if (ar_xrefToSymbolInProcOut != null) {
				// Scompattamento xref in righe di lunghezza massima
				for (int i = 0; i < ar_xrefToSymbolInProcOut.length; i++) {
					symbolReferences = symbolReferences + ar_xrefToSymbolInProcOut[i]+"OP"+" ";
					if (symbolReferences.length() > 85) {
						al_symbolReferences.add(symbolReferences);
						symbolReferences = "";
					}
				} // end-for
			} // end-if
			
			// Ultima riga ancora da accodare
			if (!symbolReferences.equals("")) {
				al_symbolReferences.add(symbolReferences);
			}
			
			
			
			// Produzione righe di xref per il simbolo
			
			// Scan righe di xref e output delle stesse
			for (String symbolXref : al_symbolReferences) {
				symbolReferences = StringService._pad(symbolXref, ' ', 90, StringService.PAD_RIGHT);
				row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0070", symbolName, symbolTypeDesc, symbolDefSection, symbolReferences);
				alRowPgmSummary.add(row);
				
				// Nelle righe successive non vengono ripetute symbol e sezione di definizione del dato
				symbolName = symbolNameBlank;
				symbolDefSection = symbolDefSectionBlank;
				symbolTypeDesc = symbolTypeDescBlank;
			} // end-for
			
		} // end-for
	}


 	
 
   /*
    * Produzione Codice morto come label e section non referenziate o codice non raggiungibile
    * 
    */
	private void putDeadCode(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol2, ArrayList<String> alRowPgmSummary) {

		ExecutionDirectives diCur = innerPgmToDoSummary.di;

		if (!diCur.optListDeadCode) {
			return;
		}

		ProgramCobolEntry<? extends Instruction>[] ar_entryDataDivision= null;
		ProgramCobolEntry<? extends Instruction>[] ar_entryProcDivision= null;
		ProgramCobolEntry<? extends Instruction> entryProcDivision = null;
		ProgramCobolEntry<? extends Instruction> entryDataDivision = null;
		InstructionCobolDataItem dataItem = null;
		String ar_sectionName[] = null;
		String ar_labelName[] = null;
		ArrayList<String> al_copyName = null;
		ArrayList<String> al_deadCodeData = null;
	   	String row = "";
	   	String deadCodeValueOld = "";
	   	String deadCodeInstruction = "";
	   	String deadCodeSection = "";
	   	String deadCodeLabel = "";
	   	String deadCodeCopy = "";
	   	String deadCodeFieldNames = "";
	   	String deadCodeFieldName = "";
    	boolean isDeadCodeFound = true;
       	
		alRowPgmSummary.add("");
		alRowPgmSummary.add("");

		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0092");
		alRowPgmSummary.add(row);
	

		////////////////////////////////////////////////////////
		// Definizione dati dead code non utilizzate
		////////////////////////////////////////////////////////
		
		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0075");
		alRowPgmSummary.add(row);
		isDeadCodeFound = false;
		al_deadCodeData = new ArrayList<String> ();
		
		// Estrazione campi dead code per ordinamento successivo
		ar_entryDataDivision = this.programCobol.entriesData();
		for (int i = 0; i < ar_entryDataDivision.length; i++) {
			entryDataDivision = ar_entryDataDivision[i];
			
			// Interessano solo le definizioni dati
			if (!(entryDataDivision.getInstruction() instanceof InstructionCobolDataItem)) {
				continue;
			}
			// Interessa solo il dead code
			if (!entryDataDivision.isDeadCode()) {
				continue;
			}
			
			dataItem = (InstructionCobolDataItem) entryDataDivision.getInstruction();
			deadCodeFieldName = dataItem.getDataName();
			
			// Non interessano i filler
			if (deadCodeFieldName.equals("FILLER")) {
				continue;
			}
			
			isDeadCodeFound = true;
			al_deadCodeData.add(deadCodeFieldName);
		}
		
		Collections.sort(al_deadCodeData);
		
		ar_entryDataDivision = this.programCobol.entriesData();
		for (int i = 0; i < al_deadCodeData.size(); i++) {
			
			deadCodeFieldName = al_deadCodeData.get(i);
			
			deadCodeValueOld = deadCodeFieldNames;
			deadCodeFieldNames = deadCodeFieldNames + " " + deadCodeFieldName +  " ";
			deadCodeFieldNames = deadCodeFieldNames.trim();
			
			// Riga piena
			if (deadCodeFieldNames.length() > 110) {
				deadCodeValueOld = StringService._pad(deadCodeValueOld, ' ', 110, StringService.PAD_RIGHT);
				row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0076", deadCodeValueOld);
				alRowPgmSummary.add(row);
				deadCodeFieldNames = deadCodeFieldName;
			}
		}

		if (!isDeadCodeFound) {
			deadCodeFieldNames = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0077");
		} 
		deadCodeFieldNames = StringService._pad(deadCodeFieldNames, ' ', 110, StringService.PAD_RIGHT);
		
		
		// Ultima riga
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0076", deadCodeFieldNames);
		alRowPgmSummary.add(row);		
		
		
		
		////////////////////////////////////////////////////////
		// Istruzioni dead code non raggiungibili
		////////////////////////////////////////////////////////
		
		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0093");
		alRowPgmSummary.add(row);
		isDeadCodeFound = false;
		
		ar_entryProcDivision = this.programCobol.entriesProcedure();
		for (int i = 0; i < ar_entryProcDivision.length; i++) {
			
			entryProcDivision = ar_entryProcDivision[i];
			
			// Istruzione raggiungibile, non dead code: skip
			if (!entryProcDivision.isDeadCode()) {
				continue;
			}
			deadCodeInstruction = deadCodeInstruction + i +  " ";
			isDeadCodeFound = true;
			
			// Riga piena
			if (deadCodeInstruction.length() > 100) {
				deadCodeInstruction = StringService._pad(deadCodeInstruction, ' ', 110, StringService.PAD_RIGHT);
				row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0096", deadCodeInstruction);
				alRowPgmSummary.add(row);
				deadCodeInstruction = "";
			}
		}
		
		if (!isDeadCodeFound) {
			deadCodeInstruction = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0077");
		}
		deadCodeInstruction = StringService._pad(deadCodeInstruction, ' ', 110, StringService.PAD_RIGHT);
	
		
		// Ultima riga
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0096", deadCodeInstruction);
		alRowPgmSummary.add(row);
		
		
		////////////////////////////////////////////////////////
		// Section non referenziate
		////////////////////////////////////////////////////////

		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0094");
		alRowPgmSummary.add(row);
		isDeadCodeFound = false;
		
		ar_sectionName = this.programCobol.procInternalNames();
		for (String sectionName : ar_sectionName) {
			// Section referenziata: skip
			if (this.programCobol.isSectionReferenced(sectionName)) {
				continue;
			}
			deadCodeValueOld = deadCodeSection;
			deadCodeSection = deadCodeSection + sectionName +  " ";
			isDeadCodeFound = true;
			
			// Riga piena
			if (deadCodeSection.length() > 110) {
				deadCodeValueOld = StringService._pad(deadCodeValueOld, ' ', 110, StringService.PAD_RIGHT);
				row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0096", deadCodeValueOld);
				alRowPgmSummary.add(row);
				deadCodeSection = sectionName + " ";
			}
		}

		if (!isDeadCodeFound) {
			deadCodeSection = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0077");
		}
		deadCodeSection = StringService._pad(deadCodeSection, ' ', 110, StringService.PAD_RIGHT);
		 

		// Ultima riga
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0096", deadCodeSection);
		alRowPgmSummary.add(row);


		////////////////////////////////////////////////////////
		// Label non referenziate
		////////////////////////////////////////////////////////

		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0095");
		alRowPgmSummary.add(row);
		isDeadCodeFound = false;
		
		ar_labelName = this.programCobol.labelNames();
		Arrays.sort(ar_labelName);
		
		for (String labelName : ar_labelName) {
			// Label referenziata: skip
			if (this.programCobol.isLabelReferenced(labelName)) {
				continue;
			}
			deadCodeValueOld = deadCodeLabel;
			deadCodeLabel = deadCodeLabel + labelName +  " ";
			isDeadCodeFound = true;
			
			// Riga piena
			if (deadCodeLabel.length() > 110) {
				deadCodeValueOld = StringService._pad(deadCodeValueOld, ' ', 110, StringService.PAD_RIGHT);
				row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0096", deadCodeValueOld);
				alRowPgmSummary.add(row);
				deadCodeLabel = labelName + " ";
			}
		}

		if (!isDeadCodeFound) {
			deadCodeLabel = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0077");
		} 
		deadCodeLabel = StringService._pad(deadCodeLabel, ' ', 110, StringService.PAD_RIGHT);
		 

		// Ultima riga
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0096", deadCodeLabel);
		alRowPgmSummary.add(row);

		
		
		////////////////////////////////////////////////////////
		// Copy Data Division definiti e non utilizzati
		////////////////////////////////////////////////////////

		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0078");
		alRowPgmSummary.add(row);

		al_copyName = this.programCobol.deadCodeCopyData();
		Collections.sort(al_copyName);
		
		for (String copyName : al_copyName) {

			deadCodeValueOld = deadCodeCopy;
			deadCodeCopy = deadCodeCopy + copyName +  " ";
			
			// Riga piena
			if (deadCodeCopy.length() > 110) {
				deadCodeValueOld = StringService._pad(deadCodeValueOld, ' ', 110, StringService.PAD_RIGHT);
				row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0096", deadCodeValueOld);
				alRowPgmSummary.add(row);
				deadCodeCopy = copyName + " ";
			}
		}

		if (al_copyName.size() == 0) {
			deadCodeCopy = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0077");
		} 
		deadCodeCopy = StringService._pad(deadCodeCopy, ' ', 110, StringService.PAD_RIGHT);	

		
		// Ultima riga
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0096", deadCodeCopy);
		alRowPgmSummary.add(row);
	
		
		////////////////////////////////////////////////////////
		// Copy Procedure Division definiti e non utilizzati
		////////////////////////////////////////////////////////

		alRowPgmSummary.add("");
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0079");
		alRowPgmSummary.add(row);
		
		al_copyName = this.programCobol.deadCodeCopyProc();
		Collections.sort(al_copyName);
		
		for (String copyName : al_copyName) {

			deadCodeValueOld = deadCodeCopy;
			deadCodeCopy = deadCodeCopy + copyName +  " ";
			
			// Riga piena
			if (deadCodeCopy.length() > 110) {
				deadCodeValueOld = StringService._pad(deadCodeValueOld, ' ', 110, StringService.PAD_RIGHT);
				row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0096", deadCodeValueOld);
				alRowPgmSummary.add(row);
				deadCodeCopy = copyName + " ";
			}
		}

		if (al_copyName.size() == 0) {
			deadCodeCopy = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0077");
		} 
		deadCodeCopy = StringService._pad(deadCodeCopy, ' ', 110, StringService.PAD_RIGHT);
		

		// Ultima riga
		row = AmritaStartup.mm.getMessageActualized(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "MI0096", deadCodeCopy);
		alRowPgmSummary.add(row);
		
	}
	
	
 




	/*
    * Produzione Metriche di programma 
    * 
    */
	private void putProgramMetrics(InnerDescriptorSource innerPgmToDoSummary, ProgramCobol programCobol2, ArrayList<String> alRowPgmSummary) {

		ExecutionDirectives diCur = innerPgmToDoSummary.di;
		
		if (!diCur.optListProgramGraph) {
			return;
		}

	}

	

	////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////                      Metodi privati                   /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	

 

	/*
	 * Restituisce l'oggetto EnumObject a fronte del suo numero ordinale
	 * 
	 */
    private EnumObject getDescEntityType(int internalNameTypeNum) {
		for (EnumObject en_object : EnumObject.values()) {
			
			if (en_object.ordinal() == internalNameTypeNum) {
				return en_object;
			}
			
		}
		return EnumObject.NOT_ASSIGNED;
	}





	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	
}

