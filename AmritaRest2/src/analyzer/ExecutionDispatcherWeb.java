package analyzer;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Scanner;
import utilities.StringService;
import enums.EnumAmritaExceptionError;
import enums.EnumDataBase;
import enums.EnumDirectivesExecution;
import enums.EnumMessageType;
import exception.ExceptionAmrita;
/**
 * Copyright (c) 2009-2021 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * === VERSIONE ATTIVA =====
 * <h1>
 * ExecutionDispatcher
 * </h1>
 * <p>
 * Questa classe gestisce l'attivazione generalizzata dello specifico gestore di processi e funzioni 
 * {@link ProcessAnalyzeSource}, {@link ProcessPgmLevel}, {@link ProcessSystemLevel} etc. 
 * versione per attivazione web da web service<br>
 * <p>
 * Tutte le directories, i path e i file pilota da utilizzare per individuare i sorgenti e il tipo
 * di analisi da effettuare, sono pilotati attraverso l'oggetto UserConfigurator e le costanti statiche in AmritaStartup.
 * <p>
 * Il processo di analisi viene completamente automatizzato attraverso il file pilota con le direttive
 * di esecuzione, passato come parametro all'interfaccia con l'esterno {@link ExecutionStarterWeb} e localizzato
 * nella directory <b>DirPilotAndFilter</b>
 * <p>
 * Le direttive vengono analizzate sequenzialmente e i valori incontrati di identificazione
 * oggetti, di filtro o altro, vengono aggiornati non appena incontrati.<br>
 * A fronte della direttiva OBJECTS_IDENTIFICATION_UNIT, tutti i valori incontrati precedentemente
 * di identificazione oggetti e di controllo, vengono memorizzati nella direttiva stessa.<br>
 * La stessa operazione viene effettuata per successive sequenze di direttive di identificazione
 * e controllo seguite da una ulteriore direttiva OBJECTS_IDENTIFICATION_UNIT.<br>
 * Ogni direttiva OBJECTS_IDENTIFICATION_UNIT identifica quindi un insieme di oggetti, filtri,
 * opzioni di esecuzione, sistemi/sorttosistemi diversi.<br>
 * In sintesi una direttiva OBJECTS_IDENTIFICATION_UNIT rappresenta un insieme di oggetti da processare
 * con le relative opzioni.<br>
 * <p>
 * Segue un esempio di pilota con direttive per analisin programmi Cobol:<br>
 * <p>
 * <tt>
 * 
#--------------------------------------------------------------------------<br>
# Direttive sempre valide<br>
#--------------------------------------------------------------------------<br>

FILE_OUTPUT FileSourcesDetected2.txt<br>
DATA_BASE_LOG_ANY_SQL_DISABLED<br>
USER_EXIT_CLASS UserExit<br>
OPT_STACK_TRACE_ON_PARSING_ERROR_DISABLED<br>
OPT_VERBOSE_MESSAGES_DISABLED<br>
THREAD_NAME T1<br>
THREAD_GROUP G1   <br>                      
THREAD_PRTY 6   <br>                       
THREAD_MAX_SUB_THREADS 5  <br>              
EXECUTION_UNDER_THREAD_ENABLED<br>

#--------------------------------------------------------------------------<br>
# Librerie sorgenti programmi e copy e search copy included<br>
#--------------------------------------------------------------------------<br>

LIBRARY PGMTEST  I:\Amrita\Amrita-Java\AmritaFramework\UnitTestPgm\<br>
LIBRARY COPYTEST I:\Amrita\Amrita-Java\AmritaFramework\UnitTestCopy\<br>
LIBRARY PGMUX G:\SPaolo\Sources\LTM0T.SAE.SOURCE\<br>
LIBRARY COPYUX G:\SPaolo\Sources\X400DB2.TS000.COPYCOB\<br>
LIBRARY_SOURCE_SEARCH COPYTEST<br>
LIBRARY_SOURCE_SEARCH COPYUX<br>

#--------------------------------------------------------------------------<br>
# Opzioni e direttive specifiche di processo<br>
#--------------------------------------------------------------------------<br>

OPT_DETECT_DEAD_CODE_DISABLED<br>
OPT_PGM_GRAPH_CREATION_DISABLED<br>
OPT_SOLVE_DYNAMIC_LOCAL_DISABLED<br>
OPT_RE_ANALYSIS_COPY_DISABLED<br>
OPT_RE_BUILD_COPY_DEF_ON_DB_DISABLED<br>
OPT_UPDATE_DB_ENABLED<br>
OPT_WHERE_USED_COPY_ITEMS_ON_DB_DISABLED<br>
OPT_COPY_DEF_ON_DB_ENABLED<br>

#--------------------------------------------------------------------------<br>
# Identificazione oggetti da analizzare<br>
#--------------------------------------------------------------------------<br>
#COPY_TO_FORCE_RE_BUILD_ON_DB UX00000<br>
#OBJECT_TYPE_COPY_COBOL_DATA *ALL* <br> 
#OBJECT_TYPE_COPY_COBOL_DATA *SINGLE* UX42895<br>

SYSTEM_VALUE S<br>
SUB_SYSTEM_VALUE UX<br>

OBJECT_TYPE_PGM_COBOL *ALL* <br>
#OBJECT_TYPE_PGM_COBOL *ALL* UX51890 UX51990 <br>

# Elenco programmi analizzati con errori<br>
#OBJECT_TYPE_PGM_COBOL *SINGLE* UX2E110<br>
#OBJECT_TYPE_PGM_COBOL *SINGLE* UX39505 <br> 

#Elenco programmi analizzati con errori<br>
#OBJECT_TYPE_PGM_COBOL *SINGLE* UX81090<br>

# Elenco programmi analizzati terminati con exception<br>
#OBJECT_TYPE_PGM_COBOL *SINGLE* UX81100<br>
#OBJECT_TYPE_PGM_COBOL *SINGLE* UX81920<br>

FILTER_ON_OBJECT_NAME 1 UX0<br>

#FILTER_ON_OBJECT_NAME 1 UX52O50<br>
#FILTER_ON_OBJECT_NAME_RANGE UX51890 UX51990<br>

#EXCLUDE_OBJECT_NAME UX51890<br>
#EXCLUDE_OBJECT_NAME_RANGE UX51890 UX51890<br>

OBJECTS_IDENTIFICATION_UNIT<br>


#--------------------------------------------------------------------------<br>
# Attivazione processo<br>
#--------------------------------------------------------------------------<br>

PROCESS_ANALYZE_SOURCES	 <br>

START<br>
</tt>
 * <p>
 * 
 * 
 * 
 * <p>
 * Sono gestite le seguenti funzionalità:
 * <Ul>
 * <Li> Gestione multithreading
 * <Li> Caricamento sorgente 
 * <Li> Gestione processi di analisi
 * <Li> Gestione Inserimento oggetti/relazioni su data base
 * <Li> Gestione logging informazioni iniziali
 * <Li> Gestione intercettazione e logging exception classi figlie speifiche
 * <Li> Metodi di interrogazione istruzioni generalizzate, risolte, da risolvere
 * </Ul>
 * 
 * <p>
 * Sono gestiti i seguenti tipi di processo di analisi attraverso la/le classi specializzate:
 * 
 * <Ul>
 * <Li> PROCESS_ANALYZE_SOURCE <br>						Analisi preliminare sorgente, codifica e serializzazione
 * <Li> PROCESS_PGM_LEVEL <br>						    Elaborazione informazioni a livello di programma
 * <Li> PROCESS_SYSTEM_LEVEL  <br>					    Elaborazione informazioni a livello di sistema/sottosistema
 * <p>
 * Sono gestiti i seguenti tipi di funzione attraverso la/le classi specializzate:
 * 
 * <Li> FUNCTION_LIBRARY_SCAN  <br>					    Individuazione tipologia sorgenti
 * <Li> FUNCTION_PGM_SUMMARY  <br>					    Esposizione informazioni a livello di programma
 * <Li> FUNCTION_SYSTEM_SUMMARY  <br>					Esposizione informazioni a livello di Sistema/sottosistema
 * </Ul>
 * 
 * <p>
 * Le funzioni e i processi vengono attivati con la direttiva:
 *  <Ul>
 * <Li> START  <br>										Exec in modo sincrono o con thread separato
 * </Ul>
 * 

 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/04/2010
 * @see ExecutionStarter
 * @see ProcessAnalyzeSource
 * @see ProcessPgmLevel
 * @see ProcessSystemLevel
 */
public class ExecutionDispatcherWeb implements AmritaConstants{


	///////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	///////////////////////////////////////////////////////////////////////////////////

	// User configuration (da login)
	private UserConfiguration ucfg = null;  

	// gestore generalizzato sources
	private SourceManager sm = null;                              

	// Files pilota impostati da ExecutionStarter e forniti dall'esterno o da configurazione di default
	private SourceInput siExecutionPilot = null;				// File di pilot di esecuzione

	// Informazioni e opzioni da passare ai gestori di processi e funzioni recuperate dal pilota processi
	// Il pilota processi è analizzato sequenzialmente e le informazioni contenute vengono aggiornate
	// prima di attivare il processo/funzione. 
	// Questo descrittore di esecuzione è di servizio.
	private ExecutionDirectives di = null;

	// Contiene sequenze di direttive OBJECTS_IDENTIFICATION_UNIT 
	// seguite dalla direttiva PROCESS o FUNCTION da eseguire.
	private ArrayList<ExecutionDirectives> al_directivesInfoToExec = null;   	

	// Presente direttiva START: processi e funzioni da attivare
	private boolean isThereDirectiveoStart = false;



	/**
	 * 
	 * Costruttore 
	 * 
	 * I file di pilot con i sources/directories da trattare e con i processi da attivare, 
	 * si trovano nella directory DirPilotAndFilter che permettono di personalizzare i processi di analisi. 
	 * Nei files di pilot sono presenti paths compleeti di sorgenti e directories da analizzare, 
	 * con eventuali commenti e informazioni di filtro.
	 * 
	 */
	public ExecutionDispatcherWeb(UserConfiguration ucfg ,SourceInput siExecutionPilot) throws ExceptionAmrita {

		this.ucfg = ucfg;  // Defaults reference ai servizi condivisi
		this.siExecutionPilot = siExecutionPilot;  // Da parametri di lancio esterni
		this.sm = new SourceManager(ucfg);   // Source manager
		al_directivesInfoToExec = new ArrayList<ExecutionDirectives>();  // Allocazioni varie 

	}


	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////      Dispatcher processi e funzioni da eseguire      /////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////



	/**
	 * 
	 * Viene letto il file di processo impostato dal costruttore
	 * con le elaborazioni e i processi da effettuare.
	 * Vengono quindi attivate le elaborazioni schedulate.<br>
	 * Prima vengono eseguite le funzioni e i processi sincroni, poi i processi
	 * lanciati con thread e infine le funzioni lanciate con thread.
	 * 
	 * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * @throws Exception 
	 * 
	 */
	public void processMain() throws ExceptionAmrita, SQLException  {

		EnumDirectivesExecution en_processOrFunction = null;
		String arRowPilotExecution[] = null;     				      // Righe sorgente pilota processi/funzioni
		String[] arParm = null;										  // Parametri per loggging

		arRowPilotExecution = siExecutionPilot.getArrayRowSource();   // Recupero righe direttive pilota di esecuzione

		// Prima istanza direttive di funzione/processo, verrà clonata a ogni nuova funzione o processo
		di = new ExecutionDirectives(ucfg);

		///////////////////////////////////////////////////////////////////////////////////////////////////
		// (1) Controllo, intabellamento e classificazione informazioni direttive di processo e funzione //
		///////////////////////////////////////////////////////////////////////////////////////////////////

		for (int i = 0; i < arRowPilotExecution.length; i++) {

			// Commento o riga vuota: Skip
			if (arRowPilotExecution[i].startsWith(TAG_COMMENT )   
					||  arRowPilotExecution[i].trim().equals("")) {	
				continue;
			}

			en_processOrFunction = detectProcessOrFunctionType(arRowPilotExecution[i]);

			// Non è una direttiva valida: skip
			if (en_processOrFunction == EnumDirectivesExecution.NOT_ASSIGNED) {
				arParm = new String[1];
				arParm[0] = arRowPilotExecution[i].trim();
				// Direttiva di processo/funzione non prevista
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0020", arParm, null); 
				continue;
			}

			// Inserimento informazioni di filtro, selezione  oggetti, thread, modalità di esecuzione ..
			extractInfoDirective(en_processOrFunction, arRowPilotExecution[i]);  // -> al_directivesInfoToExec

		} // End-for

		// Nessuna direttiva di START: segnalazione warning
		if (!isThereDirectiveoStart) {
			AmritaStartup.lf.writeRow(EnumMessageType.WARNING, "MW0004", null, null); 
			return;
		}


		///////////////////////////////////////////////////////////////////////////////////////////////
		// (3) Esecuzione di processi e funzioni                                                     //
		///////////////////////////////////////////////////////////////////////////////////////////////

		// Attivazione gestore di processi o di funzioni
		try {

			execFunctionsAndProcesses();

			// Se c'è stata qualche exception è gia stata gestita 

			// Exception già gestita da processo/funzione sincrono: nessuna operazione
		} catch (Exception e) {

		} // End-try
	} 




	////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////                      Metodi privati                  /////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////


	/**
	 * 
	 * Viene fornito il tipo di processo da eseguire. Tutti i parametri, i filtri e le opzioni
	 * codificate nel file pilota delle direttive di processo, prima della direttiva di esecuzione
	 * del processo corrente, sono disponibili in questa classe attraverso i metodi della super classe 
	 * {@link _ExecutionLauncher}.
	 * Viene quindi attivato il metodo di esecuzione della funzione o del processo, nella classe di
	 * gestione.
	 * 
	 * @throws Exception 
	 * 
	 */
	private void execFunctionsAndProcesses() throws Exception {
		ExecutionDirectives diFunctionProcess = null;

		// Ogni elemento è un ArrayList di direttive di una funzione/processo
		ArrayList<ArrayList<ExecutionDirectives>> al_splittedExecInfo = null;

		// Recupero descrittore funzione da eseguire
		al_splittedExecInfo = splitInfoExecutionUntit();


		///////////////////////////////////////////////////////////////////////////////////////////
		// (1) Esecuzione funzioni/processi che devono essere eseguite ognuna un thread separato 
		///////////////////////////////////////////////////////////////////////////////////////////

		for (ArrayList<ExecutionDirectives> al_execUnitDirectives : al_splittedExecInfo) {
			execUnit(true, al_execUnitDirectives);
		}

		///////////////////////////////////////////////////////////////////////////////////////////
		// (2) Esecuzione funzioni/processi che devono essere eseguite ognuna in modo sincrono
		///////////////////////////////////////////////////////////////////////////////////////////

		for (ArrayList<ExecutionDirectives> al_execUnitDirectives : al_splittedExecInfo) {
			execUnit(false, al_execUnitDirectives);
		}


		///////////////////////////////////////////////////////////////////////////////////////////
		// (3) Attesa che tutti i thread lanciati siano terminati                                //
		///////////////////////////////////////////////////////////////////////////////////////////

		for (ArrayList<ExecutionDirectives> al_execUnit : al_splittedExecInfo) {

			diFunctionProcess = al_execUnit.get(al_execUnit.size() - 1);

			// Solo se era stato eseguito in un thread separato
			if (diFunctionProcess.isToExecAsThread) {

				diFunctionProcess.thread.join();							// Attende che il thread sia terminato

				loggingEndProcessFunction(al_execUnit, diFunctionProcess); 	// Elapset totale e medio per oggetto

			} // end.if

		} // end-for


		// Elaborazioni sincrone ed eseguite in thread separati terminate.
		// Nel caso di esecuzione come thread lo stato il tempo di elaborazione si trova nello specifico oggetto
		// DirectivesInfo allocato dallo scheduler ExecutionDispatcher, arrivato fino alla specifica funzione
		// applicativa, e da questa aggiornato con l'ora di fine elaborazione ed eventuale exception generata


		return;
	}


	/*
	 * 
	 * Gestione esecuzione generalizzata funzione/processo, in modo sincrono o in un thread separato.
	 * 
	 * A tale scopo viene usata Reflection per istanziare la classe e invocare il metodo di esecuzione.
	 * 
	 * Viene fornito un ArrayList con le direttive OBJECTS_IDENTIFICATION_UNIT seguite da quella di
	 * esecuzione del processo/funzione, come ProcessAnalyzeSource.
	 * Al gestore del processo vengono passate tutte le direttive  OBJECTS_IDENTIFICATION_UNIT in quanto
	 * ognuna identifica oggetti, filtri, parametri, opzioni, sistema e sottosistema eventualmente diversi.
	 * Inoltre eventuali informazioni di ritorno, come errori di parsing o exception,
	 * vengono restituiti sempre nel corrispondente oggetto DirectivesInfo.
	 * 
	 */
	private void execUnit(boolean execAsThread, ArrayList<ExecutionDirectives> al_execUnitDirectives) throws Exception {

		ExecutionDirectives diFunctionProcess = null;
		ExecutionDirectives directiveExcp = null;
		Exception diExcpOccurred = null;

		diFunctionProcess = al_execUnitDirectives.get(al_execUnitDirectives.size() - 1);
		Object ObjFunctionToExec = null;						// Oggetto che contiene la classe applicativa di gestione della funzione
		ExceptionAmrita excp = null;							// Exception generata
		String execUnitClassNameComplete = "";					// Nome completo della classe applicativa di gestione della funzione
		String classModel = "" ;                                // Modello completo classe incluso package
		String execUnitClassName = "";                          // Classe di gestione della funzione/processo


		// Determino il nome completo della classe di esecuzione, prendendo a modello FunctionLibraryScan
		execUnitClassName = diFunctionProcess.en_CurProcessFunction.getClassName();
		classModel = FunctionLibraryScan.class.getName();
		int iStart = classModel.indexOf("FunctionLibraryScan");
		execUnitClassNameComplete = classModel.substring(0, iStart) + execUnitClassName;


		///////////////////////////////////////////////////////////////////////
		// Esecuzione funzione in modo sincrono, via Invoke di reflection    
		///////////////////////////////////////////////////////////////////////

		if (!execAsThread  && !diFunctionProcess.isToExecAsThread) {


			ObjFunctionToExec = newFunctionInstance(execUnitClassNameComplete, al_execUnitDirectives);   // Creo dinamicamente un'istanza della classe

			try {
				loggingStartProcessFunction(al_execUnitDirectives, diFunctionProcess);

				try {

					diFunctionProcess.launchMethodMode = AmritaConstants.LAUNCH_METHOD_INVOKE;
					AmritaStartup.rm.invokeMethod(ObjFunctionToExec, "exec", null, null);


				} catch (Exception e) {
					excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, null);
					String strMsg[] = new String[2];
					strMsg[0] = execUnitClassNameComplete;
					strMsg[1] = "exec";
					AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INTERNAL, "ET0006", strMsg, excp);
					throw excp;
				}

				// Dal momento che la funzione è stata lanciata con Invoke, il controllo torna  in ogni caso all'istruzione dopo la Invoke, 
				// anche se nella funzione applicativa si era verificata una eccezione gestita, oppura non prevista.
				// L'oggetto DirectivesInfo di è qui disponibile ed è stato eventualmente aggiornato con l'exception avvenuta.
				// Quindi se nella funzione, attivata con invoke, c'è stata una exception, è stata intercettata e il controllo torna in questo punto
				directiveExcp = getDirectiveWithException(al_execUnitDirectives);
				diExcpOccurred = directiveExcp.excpOccurred;

				if (diFunctionProcess.launchMethodMode == AmritaConstants.LAUNCH_METHOD_INVOKE && diExcpOccurred != null) {
					// Faccio in modo di attivare il blocco catch (*) più giù nel codice, che sarebbe stato attivato normalmente in caso
					// di exception e lancio del metodo applicativo statico
					throw diExcpOccurred;
				}  

				// Nessuna eccezione generata dalla funzione richiamata 

				// logging di normale fine elaborazione
				loggingEndProcessFunction(al_execUnitDirectives, diFunctionProcess);
				return;



				// (*) A causa di reflection non si possono trappare e rilanciare direttamente le eccezioni avvenute nei metodi invocati
			} catch (Exception e) {

				loggingEndProcessFunction(al_execUnitDirectives, diFunctionProcess);
				return;

			} // end-try

		} // end-if



		///////////////////////////////////////////////////////////////////////
		// Esecuzione funzione come thread separato                          //
		///////////////////////////////////////////////////////////////////////

		if (execAsThread && diFunctionProcess.isToExecAsThread) {
			ObjFunctionToExec = newFunctionInstance(execUnitClassNameComplete, al_execUnitDirectives);   // Creo dinamicamente un'istanza della classe
			diFunctionProcess.thread = new Thread(diFunctionProcess.threadGroup, (Runnable) ObjFunctionToExec, diFunctionProcess.threadName);
			//			diFunctionProcess.thread.setPriority(diFunctionProcess.threadPrty);

			diFunctionProcess.thread.start();

			this.loggingStartProcessFunction(al_execUnitDirectives, diFunctionProcess);
		}

	}

	/*
	 * 
	 * Estrazione informazioni di filtro, selezione oggetti e altro
	 * e aggiornamento variabili di istanza.
	 * 
	 */
	public void extractInfoDirective(EnumDirectivesExecution enProcessOrFunction, String rowPilot) {

		FilterEntry ief = null;						// Desrittore singolo filtro
		StringService ss = null;        			// Per gestore stringhe
		String[] arParm = null;		    			// Parametri per loggging
		SourceInput sourceInputInfo = null;			// Descrittore file 

		////////////////////////////////////////////////////////////////	
		// E' una direttiva valida: estraggo e memorizzo i valori     //
		////////////////////////////////////////////////////////////////

		switch (enProcessOrFunction){

		/////////////////////////////////////////////////////////////////////////////////////////////////
		// Direttive di impostazione directories default per sources/log/oggetti serializzati                          
		/////////////////////////////////////////////////////////////////////////////////////////////////	 


		case DIR_PILOT_AND_FILTER:              			// Pilot  sources e processi e filtri     (.pilot)	    	 
			if (isThereDirectiveoStart) {
				ucfg.setDirPilot(getDirFromRowpilot(EnumDirectivesExecution.DIR_PILOT_AND_FILTER, rowPilot));
			}
			return;

		case DIR_WORK:                                		// Working e temporaneo		    
			if (isThereDirectiveoStart) {
				ucfg.setDirWork(getDirFromRowpilot(EnumDirectivesExecution.DIR_WORK, rowPilot));
			}
			return;

		case DIR_LOG:                                 		// Log
			if (isThereDirectiveoStart) {
				ucfg.setDirLog(getDirFromRowpilot(EnumDirectivesExecution.DIR_LOG, rowPilot));
			}
			return;

		case DIR_OUTPUT:                              		// Output per funzioni 
			if (isThereDirectiveoStart) {
				ucfg.setDirOutput(getDirFromRowpilot(EnumDirectivesExecution.DIR_OUTPUT, rowPilot));
			}
			return;

		case DIR_RESOURCES:                        			// Resource
			if (isThereDirectiveoStart) {
				ucfg.setDirResources(getDirFromRowpilot(EnumDirectivesExecution.DIR_RESOURCES, rowPilot));
			}
			return;

		case DIR_DATABASE:                          		// Database		              
			if (isThereDirectiveoStart) {
				ucfg.setDirDatabase(getDirFromRowpilot(EnumDirectivesExecution.DIR_DATABASE, rowPilot));
			}
			return;

		case DIR_COBOL_SRC_PGM_INPUT:          				// Pgm    Cobol sorgenti in analisi		  (.*) 
			if (isThereDirectiveoStart) {
				ucfg.setDirCobolSrcPgm(getDirFromRowpilot(EnumDirectivesExecution.DIR_COBOL_SRC_PGM_INPUT, rowPilot));
			}
			return;

		case DIR_COBOL_SRC_COPY_INPUT:        				// Copy   Cobol sorgenti in analisi		  (.*)
			if (isThereDirectiveoStart) {
				ucfg.setDirCobolSrcCopy(getDirFromRowpilot(EnumDirectivesExecution.DIR_COBOL_SRC_COPY_INPUT, rowPilot));
			}
			return;

		case DIR_COBOL_PGM:                  				// Pgm    Cobol codificati e serializzati (.program)			 
			if (isThereDirectiveoStart) {
				ucfg.setDirCobolObjPgm(getDirFromRowpilot(EnumDirectivesExecution.DIR_COBOL_PGM, rowPilot));
			}
			return;

		case DIR_COBOL_COPY:                        		// Copy   Cobol codificati e serializzati (.copy)	
			if (isThereDirectiveoStart) {
				ucfg.setDirCobolObjCopy(getDirFromRowpilot(EnumDirectivesExecution.DIR_COBOL_COPY, rowPilot));
			}
			return;

		case DIR_COBOL_GRAPH:                				// Grafi  Cobol codificati e serializzati (.graph)	
			if (isThereDirectiveoStart) {
				ucfg.setDirCobolGraph(getDirFromRowpilot(EnumDirectivesExecution.DIR_COBOL_GRAPH, rowPilot));
			}
			return;


		case DIR_SQL_SCRIPT:                  				// Script Sql codificati e serializzati   (.scriptSql)			 
			if (isThereDirectiveoStart) {
				ucfg.setDirSqlSrcScript(getDirFromRowpilot(EnumDirectivesExecution.DIR_SQL_SCRIPT, rowPilot));
			}
			return;

		case DIR_JCL_INPUT:                          		// Jcl    in input al processo di analisi (.*)        
			if (isThereDirectiveoStart) {
				ucfg.setDirJclSrc(getDirFromRowpilot(EnumDirectivesExecution.DIR_JCL_INPUT, rowPilot));
			}
			return;

		case DIR_JCL:                 			        	// Jcl    codificati e serializzati (.jclSource, .jclInclude, .jclProc)		 
			if (isThereDirectiveoStart) {
				ucfg.setDirJclObj(getDirFromRowpilot(EnumDirectivesExecution.DIR_JCL, rowPilot));
			}
			return;


		/////////////////////////////////////////////////////////////////////////////////////////////////
		// Direttive di accesso al database                         
		/////////////////////////////////////////////////////////////////////////////////////////////////	 

		// Nome data base
		case DATABASE_NAME:  
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_NAME, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			this.ucfg.setDataBaseName(ss._word(1));
			this.di.dataBaseName = ss._word(1);
			return;

			// Username server (GZEDDA)	 
		case  DATABASE_USER: 
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_USER, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			this.ucfg.setUser(ss._word(1));
			this.di.dataBaseUser = ss._word(1);
			return;

			// Password (giampietro4)
		case  DATABASE_PWD: 
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_PWD, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			this.ucfg.setPwd(ss._word(1));
			this.di.dataBasePwd = ss._word(1);
			return;									

			// MYSQL, ORACLE, MSACCESS		 
		case DATABASE_TYPE: 
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_TYPE, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			String dataBaseType = ss._word(1);
			if (dataBaseType.contentEquals("MYSQL")) {
				this.di.dataBaseType = EnumDataBase.DB_MYSQL;
			} else if (dataBaseType.contentEquals("ORACLE")) {
				this.di.dataBaseType = EnumDataBase.DB_ORACLE;
			} else {
				this.di.dataBaseType = EnumDataBase.NOT_ASSIGNED;
			}
			return;

			// MYSQL=com.mysql.cj.jdbc.Driver, MSACCESS	= sun.jdbc.odbc.JdbcOdbcDriver
		case DATABASE_DRIVER:
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_DRIVER, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			this.di.dataBaseDriver = ss._word(1);
			return;

			// LOCAL, REMOTE
		case  DATABASE_ACCESS_TYPE:
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_ACCESS_TYPE, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			this.di.dataBaseAccessType = ss._word(1);
			return;

			// MYSQL   =jdbc:mysql://localhost:3306/Amrita?autoReconnect=true&useSSL=false&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC
			// MSACCESS=jdbc:odbc:DbAmrita
		case  DATABASE_URL:
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_URL, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			this.di.dataBaseUrl = ss._word(1);
			return;

			// Numero massimo connessioni permesse	
		case DATABASE_MAX_CONN: 
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_MAX_CONN, rowPilot, new String[]{PARM_NUMERIC});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			this.ucfg.setDataBaseMaxConn(Integer.parseInt(ss._word(1)));
			return;

			// Log istruzioni sql abilitato
		case DATABASE_LOG_ANY_SQL_ENABLED:  
			di.optDataBaseLogAnySql = true;
			return;

			// Log istruzioni sql disabilitato
		case DATABASE_LOG_ANY_SQL_DISABLED:  
			di.optDataBaseLogAnySql = false;
			return;

			// Numero blocco istruzioni primas di commit automatico
		case DATABASE_COMMIT_BLOCK_UPDATE:  
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_COMMIT_BLOCK_UPDATE, rowPilot, new String[]{PARM_NUMERIC});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			di.dataBaseCommitBlockUpdates = Integer.parseInt(ss._word(1));
			ucfg.setDataBaseCommitBlockUpdates(di.dataBaseCommitBlockUpdates);
			return;



		/////////////////////////////////////////////////////////////////////////////////////////////////
		// Direttive di impostazione sistema/sottosistema di appartenenza oggetto/sorgente da analizzare                          
		/////////////////////////////////////////////////////////////////////////////////////////////////

		case SYSTEM_INPUT:
			ss = extractValueFromDirective(EnumDirectivesExecution.SYSTEM_INPUT, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			di.systemInput = ss._word(1);
			di.en_activeObjectSystem = EnumDirectivesExecution.SYSTEM_INPUT;
			return;

		case SYSTEM_ON_NAME:
			ss = extractValueFromDirective(EnumDirectivesExecution.SYSTEM_ON_NAME, rowPilot, new String[]{PARM_NUMERIC, PARM_NUMERIC});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			di.en_activeObjectSystem = EnumDirectivesExecution.SYSTEM_ON_NAME;
			di.curSystemPos = Integer.parseInt(ss._word(1));
			di.curSystemLng = Integer.parseInt(ss._word(2));
			return;

			// Direttive sistema di appartenenza
		case SYSTEM_BY_EXIT:
			di.en_activeObjectSystem = EnumDirectivesExecution.SYSTEM_BY_EXIT;
			// Gestito al momento del reperimento del sorgente
			return;
			// Direttive sotto sistema di appartenenza

		case SUB_SYSTEM_INPUT:
			ss = extractValueFromDirective(EnumDirectivesExecution.SUB_SYSTEM_INPUT, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			di.en_activeObjectSubSystem = EnumDirectivesExecution.SUB_SYSTEM_INPUT;
			di.subSystemInput = ss._word(1);
			di.ar_subSystemInput = ss._words();
			return;

		case SUB_SYSTEM_ON_NAME:
			ss = extractValueFromDirective(EnumDirectivesExecution.SUB_SYSTEM_ON_NAME, rowPilot, new String[]{PARM_NUMERIC, PARM_NUMERIC});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			di.en_activeObjectSubSystem = EnumDirectivesExecution.SUB_SYSTEM_ON_NAME;
			di.curSubSystemPos = Integer.parseInt(ss._word(1));
			di.curSubSystemLng = Integer.parseInt(ss._word(2));
			return;
			
		case SUB_SYSTEM_BY_EXIT:
			di.en_activeObjectSubSystem = EnumDirectivesExecution.SUB_SYSTEM_BY_EXIT;
			return;

		case CUSTOMER_CODE:
			ss = extractValueFromDirective(EnumDirectivesExecution.CUSTOMER_CODE, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			di.curCustomerCode = ss._word(1);
			return;

		case CUSTOMER_INFO:
			ss = extractValueFromDirective(EnumDirectivesExecution.CUSTOMER_INFO, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			di.curCustomerInfo = ss._word(1);
			return;


		/////////////////////////////////////////////////////////////////////////////////////
		// Direttive per SQUALE e violazioni alle metriche
		/////////////////////////////////////////////////////////////////////////////////////

		// Valutazione metriche SQUALE 
		case OPT_METRICS_SQUALE_ENABLED:
			di.optMetricsSquale = true;
			return;
			// Valutazione metriche SQUALE disabilitata
		case OPT_METRICS_SQUALE_DISABLED:
			di.optMetricsSquale = false;
			return;

		case METRICS_SQUALE_VIOLATIONS_SCENARIO:
			ss = extractValueFromDirective(EnumDirectivesExecution.METRICS_SQUALE_VIOLATIONS_SCENARIO, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.curMetricsViolationsScenario = ss._word(1);
			di.optMetricsViolationsScenario = true;
			return;
		case OPT_METRICS_SQUALE_VIOLATIONS_SCENARIO_ENABLED:
			di.optMetricsViolationsScenario = true;
			return;
		case OPT_METRICS_SQUALE_VIOLATIONS_SCENARIO_DISABLED:
			di.optMetricsViolationsScenario = false;
			return;

		/////////////////////////////////////////////////////////////////////////////////////
		// Direttive per librerie, files da trattare in input, copy da includere    
		/////////////////////////////////////////////////////////////////////////////////////

		case LIBRARY:
			ss = extractValueFromDirective(EnumDirectivesExecution.LIBRARY, rowPilot, new String[]{PARM_TEXT, PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			// Libreria inesistente: logging e analisi riga successiva
			sourceInputInfo = sm.fileInfo(ss._word(2));
			if (!sourceInputInfo.isExists()) {
				arParm = new String[1];
				arParm[0] = ss._word(2);
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0002", arParm, null); 
				return;
			}
			di.al_libraryCode.add(ss._word(1));
			di.al_libraryPath.add(ss._word(2));
			return;

		case FILE_PATH:
			ss = extractValueFromDirective(EnumDirectivesExecution.FILE_PATH, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			// File inesistente: logging e analisi riga successiva
			sourceInputInfo = sm.fileInfo(ss._word(1));
			if (!sourceInputInfo.isExists()) {
				arParm = new String[1];
				arParm[0] = ss._word(1);
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0002", arParm, null); 
				return;
			}
			di.al_filePath.add(ss._word(1));
			return;

		case FILE_SUFFIX_COPY:
			ss = extractValueFromDirective(EnumDirectivesExecution.FILE_SUFFIX_COPY, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.al_fileSuffixCopy.add(ss._word(1));
			return;

			// Direttive di Library search per copy/programmi analizzati contestualmente
		case LIBRARY_SOURCE_SEARCH:
			ss = extractValueFromDirective(EnumDirectivesExecution.LIBRARY_SOURCE_SEARCH, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.al_librarySourceSearchCode.add(ss._word(1));
			return;
		case CICS:
			ss = extractValueFromDirective(EnumDirectivesExecution.CICS, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.al_cicsName.add(ss._word(1));
			return;
		case LIBRARY_CLEAR:
			di.al_libraryCode.clear();
			di.al_libraryPath.clear();
			return;
		case FILE_PATH_CLEAR:
			di.al_filePath.clear();
			return;
		case FILE_SUFFIX_COPY_CLEAR:
			di.al_filePath.clear();
			return;
		case LIBRARY_SOURCE_SEARCH_CLEAR:
			di.al_librarySourceSearchCode.clear();
			return;
		case CICS_CLEAR:
			di.al_cicsName.clear();
			return;
		case COPY_PRECOMPILER:
			ss = extractValueFromDirective(EnumDirectivesExecution.COPY_PRECOMPILER, rowPilot, new String[]{PARM_TEXT, PARM_TEXT, PARM_TEXT, PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.al_copyPrecompiler.add(ss._word(1) + " " + ss._word(3) + " " + ss._word(4));
			return;
		case COPY_PRECOMPILER_CLEAR:
			di.al_copyPrecompiler.clear();
			return;


		///////////////////////////////////////////////////////////////////////////////////////
		// Direttive di filtro sull stato degli oggetti identificati in input
		///////////////////////////////////////////////////////////////////////////////////////

		case INPUT_OBJECTS_STATUS_ALL:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_STATUS_ALL);
			return;
		case INPUT_OBJECTS_ANALYZED_WITH_NO_ERRORS:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_ANALYZED_WITH_NO_ERRORS);
			return;
		case INPUT_OBJECTS_ANALYZED_WITH_ERRORS:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_ANALYZED_WITH_ERRORS);
			return;
		case INPUT_OBJECTS_ANALYZED_WITH_EXCEPTION:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_ANALYZED_WITH_EXCEPTION);
			return;
		case INPUT_OBJECTS_PROCESSED_WITH_NO_ERRORS:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_PROCESSED_WITH_NO_ERRORS);
			return;
		case INPUT_OBJECTS_ANALYZED_DYNAMIC:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_ANALYZED_DYNAMIC);
			return;			
		case INPUT_OBJECTS_ANALYZED_DYNAMIC_SPREADED_TO_SOLVE:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_ANALYZED_DYNAMIC_SPREADED_TO_SOLVE);
			return;			
		case INPUT_OBJECTS_ANALYZED_DYNAMIC_SOLVED:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_ANALYZED_DYNAMIC_SOLVED);
			return;			
		case INPUT_OBJECTS_ANALYZED_WAITING_FOR_DATA:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_ANALYZED_WAITING_FOR_DATA);
			return;							
		case INPUT_OBJECTS_PROCESSED_WITH_ERRORS:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_PROCESSED_WITH_ERRORS);
			return;
		case INPUT_OBJECTS_PROCESSED_WITH_EXCEPTION:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_PROCESSED_WITH_EXCEPTION);
			return;
		case INPUT_OBJECTS_TO_BE_ANALYZED:
			di.al_objectInputStatus.add(EnumDirectivesExecution.INPUT_OBJECTS_TO_BE_ANALYZED);
			return;
		case INPUT_OBJECTS_STATUS_CLEAR:
			di.al_objectInputStatus.clear();
			return;


		///////////////////////////////////////////////////////////////////////////////////////
		// Direttive di filtro sugli oggetti identificati in input per il sistema/sottosistema
		///////////////////////////////////////////////////////////////////////////////////////

		// Direttive di filtro (3 gruppi di posizione e valore)
		case FILTER_ON_OBJECT_NAME_POS_VALUE:
			ss = extractValueFromDirective(EnumDirectivesExecution.FILTER_ON_OBJECT_NAME_POS_VALUE, rowPilot, new String[]{PARM_NUMERIC, PARM_TEXT, PARM_NUMERIC, PARM_TEXT, PARM_NUMERIC, PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.en_filterType = EnumDirectivesExecution.FILTER_ON_OBJECT_NAME_POS_VALUE;
			ief = new FilterEntry(EnumDirectivesExecution.FILTER_ON_OBJECT_NAME_POS_VALUE, ss._words());
			di.al_filterEntry.add(ief);
			return;
			// Direttive di range (2 valori alfanumerici)
		case FILTER_ON_OBJECT_NAME_RANGE:
			ss = extractValueFromDirective(EnumDirectivesExecution.FILTER_ON_OBJECT_NAME_RANGE, rowPilot, new String[]{PARM_TEXT, PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.en_filterType = EnumDirectivesExecution.FILTER_ON_OBJECT_NAME_RANGE;
			di.filterRangeObjectNameFrom = ss._word(1);
			di.filterRangeObjectNameTo = ss._word(2);
			return;
		case FILTER_BY_EXIT:
			di.en_filterType = EnumDirectivesExecution.FILTER_BY_EXIT;
			return;
		case FILTER_DISABLED:
			di.en_filterType = EnumDirectivesExecution.FILTER_DISABLED;
			return;
		case FILTER_CLEAR:
			di.al_filterEntry.clear();
			di.al_filterObjectName.clear();
			di.filterRangeObjectNameFrom = "";
			di.filterRangeObjectNameTo = "";
			return;


		///////////////////////////////////////////////////////////////////////////////////////
		// Direttive di exclude sugli oggetti identificati in input per il sistema/sottosistema
		///////////////////////////////////////////////////////////////////////////////////////

		// Direttive di esclusione (nomi oggetti da escludere da quelli selezionati e filtrati))
		case EXCLUDE_ON_OBJECT_NAME:
			ss = extractValueFromDirective(EnumDirectivesExecution.EXCLUDE_ON_OBJECT_NAME, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.al_excludeObjectName.add(ss._word(1));
			return;
			// Direttive di esclusione (3 gruppi di posizione e valore)
		case EXCLUDE_ON_OBJECT_NAME_POS_VALUE:
			ss = extractValueFromDirective(EnumDirectivesExecution.FILTER_ON_OBJECT_NAME_POS_VALUE, rowPilot, new String[]{PARM_NUMERIC, PARM_TEXT, PARM_NUMERIC, PARM_TEXT, PARM_NUMERIC, PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.en_filterType = EnumDirectivesExecution.FILTER_ON_OBJECT_NAME_POS_VALUE;
			ief = new FilterEntry(EnumDirectivesExecution.FILTER_ON_OBJECT_NAME_POS_VALUE, ss._words());
			di.al_filterEntry.add(ief);
			return;

		case EXCLUDE_ON_OBJECT_NAME_RANGE:
			ss = extractValueFromDirective(EnumDirectivesExecution.EXCLUDE_ON_OBJECT_NAME_RANGE, rowPilot, new String[]{PARM_TEXT, PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.excludeRangeObjectNameFrom = ss._word(1);
			di.excludeRangeObjectNameTo = ss._word(2);
			return;
		case EXCLUDE_CLEAR:
			di.al_excludeObjectName.clear();
			di.excludeRangeObjectNameFrom = "";
			di.excludeRangeObjectNameTo = "";
			return;


		///////////////////////////////////////////////////////////////////////////////
		// Direttive di compliance per le versioni analizzate
		///////////////////////////////////////////////////////////////////////////////

		// Accettazione sintassi Db2 Ibm fino a version 10
		case OPT_IBM_DB2_MAINFRAME_V10_COMPLIANCE_ENABLED:
			di.optDb2MainframeV10Compliance = true;
			return;
		case OPT_IBM_DB2_MAINFRAME_V10_COMPLIANCE_DISABLED:
			di.optDb2MainframeV10Compliance = false;
			return;
		case OPT_IBM_DB2_AIX_V9_COMPLIANCE_ENABLED:
			di.optDb2AixV19Compliance = true;
			return;
		case OPT_IBM_DB2_AIX_V9_COMPLIANCE_DISABLED:
			di.optDb2AixV19Compliance = false;
			return;


		///////////////////////////////////////////////////////////////////////////////
		// Direttive generali e di opzione per processi e funzioni   
		///////////////////////////////////////////////////////////////////////////////

		// Generiche e/o comune a processi/funzioni diversi


		// Messasggi verbosi
		case OPT_VERBOSE_MESSAGES_ENABLED:
			di.optVerboseMessages = true;
			return;
		case OPT_VERBOSE_MESSAGES_DISABLED:
			di.optVerboseMessages = false;
			return;
			// Singoli errori di analisi su db
		case OPT_TRACE_ANY_ANALYSIS_ERROR_ON_DB_ENABLED:
			di.optTraceAnyAnalysisErrorOnDb = true;
			return;
		case OPT_TRACE_ANY_ANALYSIS_ERROR_ON_DB_DISABLED:
			di.optTraceAnyAnalysisErrorOnDb = false;
			return;

		// METRICS

		// Valutazione metriche complete 
		case OPT_METRICS_ALL_ENABLED:
			di.optMetricsAll = true;
			return;
			// Valutazione metriche di programma disabilitata
		case OPT_METRICS_ALL_DISABLED:
			di.optMetricsAll = false;
			return;
			// Valutazione metriche di base dimensionali abilitata 
		case OPT_METRICS_BASIC_ENABLED:
			di.optMetricsBasic = true;
			return;
			// Valutazione metriche di base dimensionali disabilitata
		case OPT_METRICS_BASIC_DISABLED:
			di.optMetricsBasic = false;
			return;
			// Valutazione metriche di dettaglio mainline/section/pgm abilitata 
		case OPT_METRICS_DETAIL_ENABLED:
			di.optMetricsDetail = true;
			return;
			// Valutazione metriche di dettaglio mainline/section/pgm disabilitata
		case OPT_METRICS_DETAIL_DISABLED:
			di.optMetricsDetail = false;
			return;
			// Valutazione metriche di McCabe abilitata 
		case OPT_METRICS_MCCABE_ENABLED:
			di.optMetricsMcCabe = true;
			return;
			// Valutazione metriche di McCabe e di Halstead disabilitata
		case OPT_METRICS_MCCABE_DISABLED:
			di.optMetricsMcCabe = false;
			return;
			// Valutazione metriche di Halstead abilitata 
		case OPT_METRICS_HALSTEAD_ENABLED:
			di.optMetricsHalstead = true;
			return;
			// Valutazione metriche di Halstead disabilitata
		case OPT_METRICS_HALSTEAD_DISABLED:
			di.optMetricsHalstead = false;
			return;
			// Valutazione metriche di complessità strutturale fan-in e fan-out abilitata
		case OPT_METRICS_COMPLEXITY_STRUCTURE_ENABLED:
			di.optMetricsComplexityStructure = true;
			return;
			// Valutazione metriche di complessità strutturale fan-in e fan-out disabilitata
		case OPT_METRICS_COMPLEXITY_STRUCTURE_DISABLED:
			di.optMetricsComplexityStructure = false;
			return;
			// Valutazione metriche di complessità funzionale abilitata
		case OPT_METRICS_COMPLEXITY_FUNCTION_ENABLED: 
			di.optMetricsComplexityFunction = true;
			return;
			// Valutazione metriche di complessità funzionale disabilitata
		case OPT_METRICS_COMPLEXITY_FUNCTION_DISABLED:
			di.optMetricsComplexityFunction = false;
			return;
			// Valutazione metriche di function point abilitata
		case OPT_METRICS_FUNCTION_POINT_ENABLED:
			di.optMetricsFunctionPoint = true;
			return;
			// Valutazione metriche di function point disabilitata
		case OPT_METRICS_FUNCTION_POINT_DISABLED:
			di.optMetricsFunctionPoint = false;
			return;
			// Valutazione metriche di rehosting abilitata
		case OPT_METRICS_REHOSTING_ENABLED:
			di.optMetricsRehosting = true;
			return;
			//Valutazione metriche di rehosting abilitata
		case OPT_METRICS_REHOSTING_DISABLED:
			di.optMetricsRehosting = false;
			return;
			// Valutazione metriche di violazione abilitata
		case OPT_METRICS_SQUALE_DETECT_VIOLATIONS_ENABLED:
			di.optMetricsSqualeDetectViolation = true;
			return;
			//Valutazione metriche di violazione abilitata
		case OPT_METRICS_SQUALE_DETECT_VIOLATIONS_DISABLED:
			di.optMetricsSqualeDetectViolation = false;
			return;

			// Metriche di dettaglio per ogni section/paragrafo su database abilitate
		case OPT_METRICS_PGM_DETAIL_ON_DB_ENABLED:
			di.optMetricsPgmDetailOnDb = true;
			return;
			//Valutazione metriche di rehosting abilitata
		case OPT_METRICS_PGM_DETAIL_ON_DB_DISABLED:
			di.optMetricsPgmDetailOnDb = false;
			return;
		// Controllo via exit validità nome programma abilitato


		// PROCESS_ANALYZE_SOURCES

		// Forzatura analisi copy
		case COPY_TO_FORCE_RE_ANALYSIS:
			ss = extractValueFromDirective(EnumDirectivesExecution.COPY_TO_FORCE_RE_ANALYSIS, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.al_copyToForceReAnalysis.add(ss._word(1));
			return;
		// Nome copy di cui forzare la ricostruzione della definizione su db (tracciato)
		case COPY_TO_FORCE_RE_BUILD_DEF_ON_DB:
			ss = extractValueFromDirective(EnumDirectivesExecution.COPY_TO_FORCE_RE_BUILD_DEF_ON_DB, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
				return;
			}
			di.al_copyToForceReBuildOndb.add(ss._word(1));
			return;

			// Logging stack error per ogni istruzione errata abilitata
		case OPT_STACK_TRACE_ON_PARSING_ERROR_ENABLED:
			di.optStackTraceOnParsingError = true;
			return;
			// Logging stack error per ogni istruzione errata disabilitata
		case OPT_STACK_TRACE_ON_PARSING_ERROR_DISABLED:
			di.optStackTraceOnParsingError = false;
			return;
			// Inserimento where used items copy su data base a fine programma
		case OPT_DB_CREATE_WHERE_USED_COPY_ITEMS_ENABLED:
			di.optWhereUsedCopyItemsOnDbToInsert = true;
			return;
			// Inserimento where used items copy su data base a fine programma disabilitato
		case OPT_DB_CREATE_WHERE_USED_COPY_ITEMS_DISABLED:
			di.optWhereUsedCopyItemsOnDbToInsert = false;
			return;
		case OPT_DB_CREATE_WHERE_USED_ENTITY_COLUMNS_ENABLED:
			di.optWhereUsedEntityColumnsOnDbToInsert = true;
			return;
		// Inserimento where used colonne entity su data base a fine programma disabilitato
		case OPT_DB_CREATE_WHERE_USED_ENTITY_COLUMNS_DISABLED:
			di.optWhereUsedEntityColumnsOnDbToInsert = false;
			return;
		case OPT_DB_CREATE_COPY_DEF_ENABLED: 
			di.optCopyDefOnDbToInsert = true;
			return;
			// Creazione definizione copy su data base a fine programma
		case OPT_DB_CREATE_COPY_DEF_DISABLED:
			di.optCopyDefOnDbToInsert = false;
			return;
			// Soluzione istruzioni dinamiche a fine programma
		case OPT_FORCE_RE_ANALYSIS_COPY_ENABLED:
			di.optReAnalysisCopy = true;
			return;
			// Soluzione istruzioni dinamiche a fine programma
		case OPT_FORCE_RE_ANALYSIS_COPY_DISABLED:
			di.optReAnalysisCopy = false;
			return;
		// Creazione definizione copy su data base a fine programma
		// Inserimento where used colonne entity su data base a fine programma
		// Ricostruzione definizione tracciato copy su db 
		case OPT_FORCE_RE_BUILD_COPY_DEF_ON_DB_ENABLED:
			di.optReBuildCopyDefOnDb = true;
			return;
		// Ricostruzione definizione tracciato copy su db disabilitata
		case OPT_FORCE_RE_BUILD_COPY_DEF_ON_DB_DISABLED:
			di.optReBuildCopyDefOnDb = false;
			return;


		// PROCESS_ANALYZE_SOURCES & PROCESS_PGM_LEVEL

			// Individuazione codice morto a fine analisi programma
		case OPT_DETECT_DEAD_CODE_ENABLED:
			di.optDetectDeadCode = true;
			return;
			// Individuazione codice morto a fine analisi programma disbilitata
		case OPT_DETECT_DEAD_CODE_DISABLED:
			di.optDetectDeadCode = false;
			return;
			// Soluzione istruzioni dinamiche a fine programma
		case OPT_SOLVE_DYNAMIC_LOCAL_ENABLED:
			di.optSolveDynamicLocal = true;
			return;
			// Soluzione istruzioni dinamiche a fine programma
		case OPT_SOLVE_DYNAMIC_LOCAL_DISABLED:
			di.optSolveDynamicLocal = false;
			return;
			// Soluzione istruzioni dinamiche a fine programma
		case OPT_SOLVE_DYNAMIC_LOCAL_LIGHT_ENABLED:
			di.optSolveDynamicLocalLight = true;
			return;
			// Soluzione istruzioni dinamiche a fine programma
		case OPT_SOLVE_DYNAMIC_LOCAL_LIGHT_DISABLED:
			di.optSolveDynamicLocalLight = false;
			return;
			// Soluzione istruzioni Cics
		case OPT_SOLVE_CODE_CICS_ENABLED:
			di.optSolveCodeCics = true;
			return;
			// Soluzione istruzioni Cics disabilitata
		case OPT_SOLVE_CODE_CICS_DISABLED:
			di.optSolveCodeCics = false;
			return;
			// Soluzione istruzioni Sql
		case OPT_SOLVE_DML_SQL_ENABLED:
			di.optSolveDmlSql = true;
			return;
			// Soluzione istruzioni Sql disabilitata
		case OPT_SOLVE_DML_SQL_DISABLED:
			di.optSolveDmlSql = false;
			return;


		// PROCESS_SYSTEM_LEVEL

		// Soluzione istruzioni dinamiche spreaded
		case OPT_SOLVE_DYNAMIC_SPREADED_ENABLED:
			di.optSolveDynamicSpreaded = true;
			return;
			// Soluzione istruzioni dinamiche spreaded disabilitata
		case OPT_SOLVE_DYNAMIC_SPREADED_DISABLED:
			di.optSolveDynamicSpreaded = false;
			return;
			// Ricostruzione modello dati da oggetti e relazioni
		case OPT_DETECT_DATA_MODEL_ENABLED:
			di.optDetectDataModel = true;
			return;
			// Ricostruzione modello dati da oggetti e relazioni disabilitata
		case OPT_DETECT_DATA_MODEL_DISABLED:
			di.optDetectDataModel = false;
			return;
			// Individuazione relazioni indirette 
		case OPT_DETECT_INDIRECT_RELATIONSHIPS_ENABLED:
			di.optDetectIndirectRelationships = true;
			return;
			// Individuazione relazioni indirette disabilitata 
		case OPT_DETECT_INDIRECT_RELATIONSHIPS_DISABLED:
			di.optDetectIndirectRelationships = false;
			return;
			// Assegnazione file fisico in relazioni con logic file
		case OPT_ASSIGN_PHISICAL_TO_LOGICAL_FILE_ENABLED:
			di.optAssignPhisicalToLogicalFiles = true;
			return;
			// Assegnazione file fisico in relazioni con logic file disabilitata
		case OPT_ASSIGN_PHISICAL_TO_LOGICAL_FILE_DISABLED:
			di.optAssignPhisicalToLogicalFiles = false;
			return;
			// Agrregazione metriche di sistema/sottosistema  (a partire da quelle di programma)
		case OPT_EVALUATE_METRICS_SYSTEM_ENABLED:
			di.optMetricsSystem = true;
			return;
			// Valutazione metriche di sistema  disabilitata
		case OPT_EVALUATE_METRICS_SYSTEM_DISABLED:
			di.optMetricsSystem = false;
			return;


		// FUNCTION_LIBRARY_SCAN

		// Attiva riconoscimento tipologia sorgente
		case OPT_DETECT_SOURCE_TYPE_ENABLED:
			di.optDetectSourceType = true;
			return;
			// Attiva riconoscimento tipologia sorgente disabilitata
		case OPT_DETECT_SOURCE_TYPE_DISABLED:
			di.optDetectSourceType = false;
			return;

			// Ricerca sorgenti ricorsiva nelle directories del file pilota sorgenti
		case OPT_SEARCH_SOURCES_FILE_RECURSIVE_ENABLED:
			di.optSearchFilesRecursive = true;
			return;
			// Ricerca sorgenti ricorsiva disabilitata
		case OPT_SEARCH_SOURCES_FILE_RECURSIVE_DISABLED:
			di.optSearchFilesRecursive = false;
			return;
			// Informazioni complete di file system su files source
		case OPT_FULL_INFO_FILE_SYSTEM_ENABLED:
			di.optFullInfoFileSystem = true;
			return;
			// Informazioni complete di file system su files source disabilitata
		case OPT_FULL_INFO_FILE_SYSTEM_DISABLED:
			di.optFullInfoFileSystem = true;
			return;


		// FUNCTION_PGM_SUMMARY 


		// Sommario programma con tutte le opzioni previste
		case OPT_PGM_SUMMARY_ALL_ENABLED:
			di.optPgmSummaryAll = true;
			return;
			// Sommario programma con tutte le opzioni previste disabilitato
		case OPT_PGM_SUMMARY_ALL_DISABLED:
			di.optPgmSummaryAll = true;
			return;
			// Lista programma codificato
		case OPT_LIST_SOURCE_CODED_ENABLED:
			di.optListSourceCoded = true;
			return;
			// Lista programma codificato disabilitata
		case OPT_LIST_SOURCE_CODED_DISABLED:
			di.optListSourceCoded = true;
			return;
			// Lista Xref label
		case OPT_LIST_XREF_LABEL_ENABLED:
			di.optListXrefLabel = true;
			return;
			//Lista Xref label disabilitata
		case OPT_LIST_XREF_LABEL_DISABLED:
			di.optListXrefLabel = true;
			return;
			// Lista Xref section
		case OPT_LIST_XREF_SECTION_ENABLED:
			di.optListXrefSection = true;
			return;
			//Lista Xref section disabilitata
		case OPT_LIST_XREF_SECTION_DISABLED:
			di.optListXrefSection = true;
			return;
			// Lista Xref simboli (literal, data items)
		case OPT_LIST_XREF_SYMBOLS_ENABLED:
			di.optListXrefSection = true;
			return;
			//Lista Xref simboli (literal, data items) disabilitata
		case OPT_LIST_XREF_SYMBOLS_DISABLED:
			di.optListXrefSection = true;
			return;
			//	Lista I-O file system effettuato dal pgm
		case OPT_LIST_IO_FILE_SYSTEM_ENABLED:
			di.optListIOFileSystem = true;
			return;
			//Lista I-O file system effettuato dal pgm disabilitata
		case OPT_LIST_IO_FILE_SYSTEM_DISABLED:
			di.optListIOFileSystem = true;
			return;
			//	Lista I-O Sql effettuato dal pgm
		case OPT_LIST_IO_SQL_ENABLED:
			di.optListIOSql = true;
			return;
			//Lista I-O Sql effettuato dal pgm disabilitata
		case OPT_LIST_IO_SQL_DISABLED:
			di.optListIOSql = true;
			return;
			//	Lista I-O Dl1 effettuato dal pgm
		case OPT_LIST_IO_DL1_ENABLED:
			di.optListIODl1 = true;
			return;
			//Lista I-O Dl1 effettuato dal pgm disabilitata
		case OPT_LIST_IO_DL1_DISABLED:
			di.optListIODl1 = true;
			return;
			//	Lista I-O Adabas effettuato dal pgm
		case OPT_LIST_IO_ADABAS_ENABLED:
			di.optListIOAdabas = true;
			return;
			//Lista I-O Adabas effettuato dal pgm disabilitata
		case OPT_LIST_IO_ADABAS_DISABLED:
			di.optListIOAdabas = true;
			return;
			//	Lista Relazioni selettive fra oggetti
		case OPT_LIST_RELATIONSHIPS_ENABLED:
			di.optListRelationships = true;
			return;
			// Lista Relazioni selettive fra oggetti disabilitata
		case OPT_LIST_RELATIONSHIPS_DISABLED:
			di.optListRelationships = true;
			return;
			//	Lista origine Relazioni selettive fra oggetti
		case OPT_LIST_RELATIONSHIPS_ORIGIN_ENABLED:
			di.optListRelationshipsOrigin = true;
			return;
			// Lista origine Relazioni selettive fra oggetti disabilitata
		case OPT_LIST_RELATIONSHIPS_ORIGIN_DISABLED:
			di.optListRelationshipsOrigin = true;
			return;
			//	Lista opzioni di programma
		case OPT_LIST_OPTIONS_ENABLED:
			di.optListOptions = true;
			return;
			// Lista opzioni di programma disabilitata
		case OPT_LIST_OPTIONS_DISABLED:
			di.optListOptions = true;
			return;
			// Lista Info di dettaglio istruzioni dinamiche (pasths campi, sottocampi,..)
		case OPT_LIST_DYNAMIC_CODE_INFO_ENABLED:
			di.optListDynamicCodeInfo = true;
			return;
			// Lista Info di dettaglio istruzioni dinamiche (pasths campi, sottocampi,..) disabilitata
		case OPT_LIST_DYNAMIC_CODE_INFO_DISABLED:
			di.optListDynamicCodeInfo = true;
			return;
			//	Lista dead code programma
		case OPT_LIST_DEAD_CODE_ENABLED:
			di.optListDeadCode = true;
			return;
			// Lista dead code programma disabilitata
		case OPT_LIST_DEAD_CODE_DISABLED:
			di.optListDeadCode = true;
			return;
			//	Lista metriche di programma
		case OPT_LIST_METRICS_ENABLED:
			di.optListMetrics = true;
			return;
			// Lista metriche di programma disabilitata
		case OPT_LIST_METRICS_DISABLED:
			di.optListMetrics = true;
			return;
			// Lista matrice CRUD a livello di programma
		case OPT_LIST_CRUD_MATRIX_ENABLED:
			di.optListMetrics = true;
			return;
			// Lista matrice CRUD a livello di programma disabilitata
		case OPT_LIST_CRUD_MATRIX_DISABLED:
			di.optListMetrics = true;
			return;
			// Lista grafo di programma
		case OPT_LIST_PROGRAM_GRAPH_ENABLED:
			di.optListProgramGraph = true;
			return;
			// Lista grafo di programma disabilitata
		case OPT_LIST_PROGRAM_GRAPH_DISABLED:
			di.optListProgramGraph = true;
			return;


		// FUNCTION_SYSTEM_SUMMARY 


		// Lista sorgenti mancanti per completare analisi
		case OPT_LIST_SOURCES_MISSING_ENABLED:
			di.optListSourcesMissing = true;
			return;
			// Lista sorgenti mancanti per completare analisi disabilitata
		case OPT_LIST_SOURCES_MISSING_DISABLED:
			di.optListSourcesMissing = true;
			return;
			// Lista sorgenti non identificati da LibraryScan
		case OPT_LIST_SOURCES_NOT_IDENTIFIED_ENABLED:
			di.optListSourcesNotIdentified = true;
			return;
			// Lista sorgenti non identificati da LibraryScan disabilitata
		case OPT_LIST_SOURCES_NOT_IDENTIFIED_DISABLED:
			di.optListSourcesNotIdentified = true;
			return;
			// Lista oggetti (pgm/files/..) con nomi, stato e opzioni specificate
		case OPT_LIST_OBJECTS_SYSTEM_ENABLED:
			di.optListObjectsSystem = true;
			return;
			// Lista oggetti (pgm/files/..) con nomi, stato e opzioni specificate disabilitata
		case OPT_LIST_OBJECTS_SYSTEM_DISABLED:
			di.optListObjectsSystem = true;
			return;
			// Lista relazioni selettive per tipo, tipi oggetto, nomi ..
		case OPT_LIST_RELATIONSHIPS_SYSTEM_ENABLED:
			di.optListRelationshipsSystem = true;
			return;
			// Lista relazioni selettive per tipo, tipi oggetto, nomi .. disabilitata
		case OPT_LIST_RELATIONSHIPS_SYSTEM_DISABLED:
			di.optListRelationshipsSystem = true;
			return;
			// Lista origine Relazioni
		case OPT_LIST_RELATIONSHIPS_ORIGIN_SYSTEM_ENABLED:
			di.optListRelationshipsOriginSystem = true;
			return;
			// Lista origine Relazioni disabilitata  
		case OPT_LIST_RELATIONSHIPS_ORIGIN_SYSTEM_DISABLED:
			di.optListRelationshipsOriginSystem = true;
			return;
			// Lista matrice CRUD a livello di sistema
		case OPT_LIST_CRUD_MATRIX_SYSTEM_ENABLED:
			di.optListCrudMatrixSystem = true;
			return;
			// Lista matrice CRUD a livello di sistema disabilitata
		case OPT_LIST_CRUD_MATRIX_SYSTEM_DISABLED:
			di.optListCrudMatrixSystem = true;
			return;
			// Lista metriche di sistema
		case OPT_LIST_METRICS_SYSTEM_ENABLED:
			di.optListMetricsSystem = true;
			return;
			// Lista metriche di sistema disabilitata
		case OPT_LIST_METRICS_SYSTEM_DISABLED:
			di.optListMetricsSystem = true;
			return;



		///////////////////////////////////////////////////////////////////////////
		// Direttive di limitazione sui sources/oggetti da trattare                    
		///////////////////////////////////////////////////////////////////////////

		// Direttive di limitazione a oggetti 
		case LIMIT_MAX_OBJECTS_ENABLED:
			di.limitMaxObjects = true;
			return;
		case LIMIT_MAX_OBJECTS_DISABLED:
			di.limitMaxObjects = false;
			return;
		case LIMIT_MAX_OBJECTS_INPUT:
			ss = extractValueFromDirective(EnumDirectivesExecution.LIMIT_MAX_OBJECTS_INPUT, rowPilot, new String[]{PARM_NUMERIC});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			di.limitMaxObjectsInput = Integer.parseInt(ss._word(1));
			return;
		case LIMIT_MAX_OBJECTS_TO_PROCESS:
			ss = extractValueFromDirective(EnumDirectivesExecution.LIMIT_MAX_OBJECTS_TO_PROCESS, rowPilot, new String[]{PARM_NUMERIC});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			di.limitMaxObjectsToProcess = Integer.parseInt(ss._word(1));
			return;


		///////////////////////////////////////////////////////////////////////////
		// Direttive generali di controllo e governo esecuzione processi/funzioni		 
		///////////////////////////////////////////////////////////////////////////

		// Nome file di output da produrre, senza directory e completo di eventuale suffisso
		case FILE_NAME_OUTPUT :  
			ss = extractValueFromDirective(EnumDirectivesExecution.FILE_NAME_OUTPUT, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			di.fileNameOutput = ss._word(1);
			return;
			// Aggiornamento database a fine processo/funzione abilitata
		case OPT_UPDATE_DB_ENABLED : 
			di.optUpdateDb = true;
			return;
			// Aggiornamento database a fine processo/funzione disaabilitata
		case OPT_UPDATE_DB_DISABLED : 
			di.optUpdateDb = false;
			return;
		case USER_EXIT_CLASS:  
			ss = extractValueFromDirective(EnumDirectivesExecution.USER_EXIT_CLASS, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			di.userExitClass = ss._word(1);
			return;
			// Nome thread di esecuzione
		case THREAD_NAME:  
			ss = extractValueFromDirective(EnumDirectivesExecution.THREAD_NAME, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			di.threadName = ss._word(1);
			return;
			// Gruppo thread di appartenenza
		case THREAD_GROUP:  
			ss = extractValueFromDirective(EnumDirectivesExecution.THREAD_GROUP, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			di.threadGroupName = ss._word(1);
			return;
			// Gruppo thread di appartenenza
		case THREAD_PRTY:  
			ss = extractValueFromDirective(EnumDirectivesExecution.THREAD_PRTY, rowPilot, new String[]{PARM_NUMERIC});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			di.threadPrty = Integer.parseInt(ss._word(1));
			return;
			// Numero massimi subthreads da attivare (es. analisi parallele sorgenti)
		case THREAD_MAX_SUB_THREADS:  
			ss = extractValueFromDirective(EnumDirectivesExecution.THREAD_MAX_SUB_THREADS, rowPilot, new String[]{PARM_NUMERIC});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			di.threadMaxSubThreads = Integer.parseInt(ss._word(1));
			return;
			// Esecuzione sotto thread separato abilitata
		case EXECUTION_UNDER_THREAD_ENABLED:
			di.isToExecAsThread = true;
			return;
			// Esecuzione sotto thread separato disaabilitata
		case EXECUTION_UNDER_THREAD_DISABLED:
			di.isToExecAsThread = false;
			return;


		///////////////////////////////////////////////////////////////////////////
		// Direttive di selezione tipologie oggetti in modo massivo              //
		///////////////////////////////////////////////////////////////////////////

		// Tratta 	tutti i tipi di oggetti
		case OBJECT_TYPE_INCLUDE_ALL:   
			di.al_objectTypeToProcess.clear();
			di.al_objectNameToProcessFrom.clear();
			di.al_objectNameToProcessTo.clear();
			di.al_objectTypeToProcess.add(EnumDirectivesExecution.OBJECT_TYPE_PGM_COBOL);
			di.al_objectModeToProcess.add(AmritaConstants.TYPE_OBJECT_ALL);
			di.al_objectNameToProcess.add("");

			di.al_objectTypeToProcess.add(EnumDirectivesExecution.OBJECT_TYPE_COPY_COBOL_ID);
			di.al_objectModeToProcess.add(AmritaConstants.TYPE_OBJECT_ALL);
			di.al_objectNameToProcess.add("");
			di.al_objectNameToProcessFrom.add("");
			di.al_objectNameToProcessTo.add("");

			di.al_objectTypeToProcess.add(EnumDirectivesExecution.OBJECT_TYPE_COPY_COBOL_ENV);
			di.al_objectModeToProcess.add(AmritaConstants.TYPE_OBJECT_ALL);
			di.al_objectNameToProcess.add("");
			di.al_objectNameToProcessFrom.add("");
			di.al_objectNameToProcessTo.add("");

			di.al_objectTypeToProcess.add(EnumDirectivesExecution.OBJECT_TYPE_COPY_COBOL_DATA);
			di.al_objectModeToProcess.add(AmritaConstants.TYPE_OBJECT_ALL);
			di.al_objectNameToProcess.add("");
			di.al_objectNameToProcessFrom.add("");
			di.al_objectNameToProcessTo.add("");

			di.al_objectTypeToProcess.add(EnumDirectivesExecution.OBJECT_TYPE_COPY_COBOL_PROC);
			di.al_objectModeToProcess.add(AmritaConstants.TYPE_OBJECT_ALL);
			di.al_objectNameToProcess.add("");
			di.al_objectNameToProcessFrom.add("");
			di.al_objectNameToProcessTo.add("");

			di.al_objectTypeToProcess.add(EnumDirectivesExecution.OBJECT_TYPE_SQL_SCRIPT);
			di.al_objectModeToProcess.add(AmritaConstants.TYPE_OBJECT_ALL);
			di.al_objectNameToProcess.add("");
			di.al_objectNameToProcessFrom.add("");
			di.al_objectNameToProcessTo.add("");

			di.al_objectTypeToProcess.add(EnumDirectivesExecution.OBJECT_TYPE_JCL_MVS_JOB);
			di.al_objectModeToProcess.add(AmritaConstants.TYPE_OBJECT_ALL);
			di.al_objectNameToProcess.add("");
			di.al_objectNameToProcessFrom.add("");
			di.al_objectNameToProcessTo.add("");

			di.al_objectTypeToProcess.add(EnumDirectivesExecution.OBJECT_TYPE_JCL_MVS_INCLUDE);
			di.al_objectModeToProcess.add(AmritaConstants.TYPE_OBJECT_ALL);
			di.al_objectNameToProcess.add("");
			di.al_objectNameToProcessFrom.add("");
			di.al_objectNameToProcessTo.add("");

			di.al_objectTypeToProcess.add(EnumDirectivesExecution.OBJECT_TYPE_JCL_MVS_PROC);
			di.al_objectModeToProcess.add(AmritaConstants.TYPE_OBJECT_ALL);
			di.al_objectNameToProcess.add("");
			di.al_objectNameToProcessFrom.add("");
			di.al_objectNameToProcessTo.add("");

			di.al_objectTypeToProcess.add(EnumDirectivesExecution.OBJECT_TYPE_CICS_BMS);
			di.al_objectModeToProcess.add(AmritaConstants.TYPE_OBJECT_ALL);
			di.al_objectNameToProcess.add("");
			di.al_objectNameToProcessFrom.add("");
			di.al_objectNameToProcessTo.add("");
			return;

			// Escludi  tutti i tipi di oggetti
		case OBJECT_TYPE_EXCLUDE_ALL:  
			di.al_objectTypeToProcess.clear();
			di.al_objectModeToProcess.clear();
			di.al_objectNameToProcess.clear();
			di.al_objectNameToProcessFrom.clear();
			di.al_objectNameToProcessTo.clear();
			return;
			// Tratta il singolo programma COBOL (*SINGLE* name o tutti *ALL*)
		case OBJECT_TYPE_PGM_COBOL:
			manageObjectType(EnumDirectivesExecution.OBJECT_TYPE_PGM_COBOL, rowPilot, di);
			return;

			// Tratta il singolo modulo copy Cobol identification (*SINGLE* name o tutti *ALL*)
		case OBJECT_TYPE_COPY_COBOL_ID:
			manageObjectType(EnumDirectivesExecution.OBJECT_TYPE_COPY_COBOL_ID, rowPilot, di);
			return;
			// Tratta il singolo modulo copy Cobol environment (*SINGLE* name o tutti *ALL*)
		case OBJECT_TYPE_COPY_COBOL_ENV:
			manageObjectType(EnumDirectivesExecution.OBJECT_TYPE_COPY_COBOL_ENV, rowPilot, di);
			return;
			// Tratta il singolo modulo copy Cobol Procedure (*SINGLE* name o tutti *ALL*)
		case OBJECT_TYPE_COPY_COBOL_PROC:
			manageObjectType(EnumDirectivesExecution.OBJECT_TYPE_COPY_COBOL_PROC, rowPilot, di);
			return;
			// Tratta il singolo modulo copy Data  (*SINGLE* name o tutti *ALL*)
		case OBJECT_TYPE_COPY_COBOL_DATA:
			manageObjectType(EnumDirectivesExecution.OBJECT_TYPE_COPY_COBOL_DATA, rowPilot, di);
			return;
			// Tratta il singolo modulo copy Pl1 (*SINGLE* name o tutti *ALL*) 
		case OBJECT_TYPE_SQL_SCRIPT:
			manageObjectType(EnumDirectivesExecution.OBJECT_TYPE_SQL_SCRIPT, rowPilot, di);
			return;
		case OBJECT_TYPE_JCL_MVS_JOB:
			manageObjectType(EnumDirectivesExecution.OBJECT_TYPE_JCL_MVS_JOB, rowPilot, di);
			return;
		case OBJECT_TYPE_JCL_MVS_INCLUDE:
			manageObjectType(EnumDirectivesExecution.OBJECT_TYPE_JCL_MVS_INCLUDE, rowPilot, di);
			return;
		case OBJECT_TYPE_JCL_MVS_PROC:
			manageObjectType(EnumDirectivesExecution.OBJECT_TYPE_JCL_MVS_PROC, rowPilot, di);
			return;
		case OBJECT_TYPE_CICS_BMS:
			manageObjectType(EnumDirectivesExecution.OBJECT_TYPE_CICS_BMS, rowPilot, di);
			return;


		///////////////////////////////////////////////////////////////////////////////////////
		// Direttiva di accodamento direttive precedenti in unico descrittore DirectivesInfo
		///////////////////////////////////////////////////////////////////////////////////////

		case OBJECTS_IDENTIFICATION_UNIT:
			// Descrittore di servizio e inserimento reference in ArrayList per i launcher
			di.en_CurProcessFunction = enProcessOrFunction;
			al_directivesInfoToExec.add(di);

			// Clonazione direttive correnti in un nuovo oggetto descrittore, per l'unità di identificazione successiva
			di = (ExecutionDirectives) di.clone();
			return;


		///////////////////////////////////////////////////////////////////////////
		// Direttiva di esecuzione processi/funzioni dichiarati precedentemente
		///////////////////////////////////////////////////////////////////////////

		case START:
			isThereDirectiveoStart = true;     // Abilitazione all'esecuzione
			return;
		}



		///////////////////////////////////////////////////////////////////////////
		// Può essere solo una direttiva PROCESS_..  o FUNCTION_..           
		///////////////////////////////////////////////////////////////////////////

		di.en_CurProcessFunction = enProcessOrFunction;
		al_directivesInfoToExec.add(di);


		// Clonazione descrittore direttive correnti in un nuovo oggetto descrittore, per la funzione successiva
		di = (ExecutionDirectives) di.clone();
	}

	/*
	 * Estrazione directory e controllo esistenza
	 */
	private String getDirFromRowpilot(EnumDirectivesExecution dirPilotAndFilter, String rowPilot) {
		StringService ss = null;        			// Per gestore stringhe
		SourceInput sourceInputInfo = null;

		ss = extractValueFromDirective(EnumDirectivesExecution.DIR_PILOT_AND_FILTER, rowPilot, new String[]{PARM_TEXT});
		String[] arParm;
		// Se direttiva errata exit
		if (ss == null) {
			arParm = new String[1];
			arParm[0] = rowPilot.trim();
			AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0004", arParm, null); 
			return "";
		}
		// Dir inesistente: logging e analisi riga successiva
		sourceInputInfo = sm.fileInfo(ss._word(1));
		if (!sourceInputInfo.isExists()) {
			arParm = new String[1];
			arParm[0] = ss._word(1);
			AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0002", arParm, null); 
			return "";
		}
		return ss._word(1);
	}


	/*
	 * 
	 * Gestione direttiva di tipologia oggetto da gestire.
	 * 
	 * Il primo parametro indica la modalità di trattamento: 
	 *     *SINGLE* per singolo oggetto o 
	 *     *ALL     per tutti gli oggetti del tipo specificato
	 * Il secondo parametro vale se *SINGLE* e contiene il nome dell'oggetto
	 * 
	 * 
	 */
	private void manageObjectType(EnumDirectivesExecution object_type, String rowPilot, ExecutionDirectives di2) {
		StringService ss = null;        // Per gestore stringhe
		String[] arParm = null;		    // Parametri per loggging
		String objectNameToProcess = "";
		String objectNameToProcessFrom = "";
		String objectNameToProcessTo = "";

		ss = extractValueFromDirective(object_type, rowPilot, new String[]{PARM_TEXT, PARM_TEXT, PARM_TEXT});
		// Se direttiva errata exit
		if (ss == null) {
			arParm = new String[1];
			arParm[0] = rowPilot.trim();
			AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
			return;
		}
		di.al_objectTypeToProcess.add(object_type);
		di.al_objectModeToProcess.add(ss._word(1));

		if (ss._word(1).equals("*ALL*")) {
			objectNameToProcess = "";
			objectNameToProcessFrom = ss._word(2);		// Range
			objectNameToProcessTo = ss._word(3);		// Range
		} else {
			objectNameToProcess = ss._word(2);
			objectNameToProcessFrom = "";
			objectNameToProcessTo = "";
		}

		di.al_objectNameToProcess.add(objectNameToProcess);
		di.al_objectNameToProcessFrom.add(objectNameToProcessFrom);		// Range
		di.al_objectNameToProcessTo.add(objectNameToProcessTo);		    // Range
		return;

	}


	/*
	 * Restituisce la direttiva di processo codificata nella riga sorgente
	 * 
	 */
	public EnumDirectivesExecution detectProcessOrFunctionType(String rowPilot)  {

		Scanner scn = null;
		EnumDirectivesExecution edpMatchOutput = EnumDirectivesExecution.NOT_ASSIGNED; 
		String directiveOnRowPilot = "";

		scn = new Scanner(rowPilot);
		directiveOnRowPilot = scn.next();

		//  Scan direttive processi definite
		for (EnumDirectivesExecution enumDirective : EnumDirectivesExecution.values()) {
			String directiveValue = enumDirective.name();

			// Direttiva di processo/funzione senza parametri
			if (directiveOnRowPilot.equals(directiveValue)) {
				edpMatchOutput = EnumDirectivesExecution.valueOf(directiveValue);
				break;
			}
		}
		scn.close();
		return edpMatchOutput;
	}


	/*
	 * 
	 * Estrazione e controllo parametri direttiva.
	 * Controllo numericità se parametri numerici.
	 * Se errori restituisce null.
	 * 
	 */
	private StringService extractValueFromDirective(
			EnumDirectivesExecution filter_pos_start_value
			,String rowPilot
			,String arInputParmType[]
			) {
		StringService ss = null;
		int iStartParms;
		@SuppressWarnings("unused")
		int valParmNumeric = 0;

		// Estrazione posizione e valore separate da space
		iStartParms = filter_pos_start_value.name().length();
		ss = new StringService(rowPilot.substring(iStartParms));
		ss._words();  

		// Scan parametri in input, se ci sono, devono essere del type fornito 
		for (int i = 0; i < arInputParmType.length; i++) {

			// Ci sono	meno token dei parametri massimi previsti		
			if ((i + 1) > ss._wordsCount() ) {
				break;
			}

			// Inizio commento
			if (ss._word(i + 1).startsWith("#")) {
				break;
			}

			// Controllo solo parametri numerici
			if (arInputParmType[i] != PARM_NUMERIC) {
				continue;
			}

			// Controllo valore numerico
			try {
				valParmNumeric = Integer.parseInt(ss._word(i + 1));
			} catch (NumberFormatException e) {
				String strMsg[] = new String[1];
				strMsg[1] = rowPilot;
				// Direttiva di filtro errata
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INTERNAL, "ET0004", strMsg, null);
				return null;
			}
		}
		return ss;
	}


	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
	////////////////////////////////////////////////////////////////////////////////////////////




	/*
	 * 
	 * Separa le direttive di esecuzione in tante unita separate, una per ogni processo/funzione
	 * 
	 */
	public ArrayList<ArrayList<ExecutionDirectives>> splitInfoExecutionUntit() {

		ArrayList<ArrayList<ExecutionDirectives>> al_allExecInfo = null;
		ArrayList<ExecutionDirectives> al_singleExecInfo = null;    // Ogni elemento è una direttiva FUNCTION/PROCESS o OBJECTS_IDENTIFICATION_UNIT
		ExecutionDirectives functionDetectedInfo = null;
		int i = 0;
		int j = 0;
		int iFirst = 0;
		int iLast = 0;

		al_allExecInfo = new ArrayList<ArrayList<ExecutionDirectives>> ();  // Ogni elemento è una unità di esecuzione separata


		// Scan direttive PROCESS/FUNCTION e OBJECTS_IDENTIFICATION_UNIT estratte e aggregate dal file pilota di esecuzione
		for (i = 0; i < al_directivesInfoToExec.size(); i++) {

			iLast = i;

			functionDetectedInfo = al_directivesInfoToExec.get(i);

			// Direttiva non di esecuzione: skip
			if (functionDetectedInfo.en_CurProcessFunction == EnumDirectivesExecution.OBJECTS_IDENTIFICATION_UNIT) {
				continue;
			}

			// Direttiva di process o funzione: estraggo direttive e carico elemento di array in output

			// Creo nuova ArrayList con i soli descrittori della funzione/processo in esame
			al_singleExecInfo = new ArrayList<ExecutionDirectives> ();    
			for (j = iFirst; j <= iLast; j++) {
				al_singleExecInfo.add(al_directivesInfoToExec.get(j));
			} // end-for

			// Accodo ArrayList singolo in ArrayList con tutte le esecuzioni da effettuare
			al_allExecInfo.add(al_singleExecInfo);

			// Per successivo processo/funzione
			iFirst = i + 1;
		}

		return al_allExecInfo;
	}

	/*
	 * 
	 * Restituisce l'oggetto istanza della classe di gestione della funzione
	 * 
	 */
	private Object newFunctionInstance(String functionClassNameComplete
			, ArrayList<ExecutionDirectives> al_functionToExecInfo
			) throws ExceptionAmrita {

		Object ObjFunctionToExec = null;						// Oggetto che contiene la classe applicativa di gestione della funzione
		ExceptionAmrita excp = null;							// Exception generata

		/////////////////////////////////////////////
		// Istanzio classe di gestione             //
		/////////////////////////////////////////////

		// Alloco arrays di classi e oggetti costruttore, per reflection
		Object[] ar_Object = null;
		Class[] ar_Class = null;

		// Classi parametri costruttore
		ar_Class = new Class[2];
		ar_Class[0] = UserConfiguration.class;
		ar_Class[1] = ArrayList.class;

		// Oggetti classi parametri costruttore
		ar_Object = new Object[2];
		ar_Object[0] = ucfg;
		ar_Object[1] = al_functionToExecInfo;	

		// Generazione istanza classe di gestione funzione
		// TODO			ObjFunctionToExec = AmritaStartup.rm.newInstance(functionClassNameComplete, ar_Class, ar_Object);
		switch (functionClassNameComplete) {
		case "analyzer.ProcessAnalyzeSource":
			ObjFunctionToExec = new ProcessAnalyzeSource(ucfg, al_functionToExecInfo);
			break;
		case "analyzer.ProcessPgmLevel":
			ObjFunctionToExec = new ProcessPgmLevel(ucfg, al_functionToExecInfo);
			break;
		case "analyzer.ProcessSystemLevel":
			ObjFunctionToExec = new ProcessSystemLevel(ucfg, al_functionToExecInfo);
			break;
		case "analyzer.FunctionClearDb":
			ObjFunctionToExec = new FunctionClearDb(ucfg, al_functionToExecInfo);
			break;
		case "analyzer.FunctionLibraryScan":
			try {
				ObjFunctionToExec = new FunctionLibraryScan(ucfg, al_functionToExecInfo);
			} catch (SQLException e) {
				// TODO  Gestire
				e.printStackTrace();
			}
			break;

		default:
			break;
		}

		// Gestione errore di istanziazione
		if (ObjFunctionToExec == null) {
			excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_NEW_INSTANCE, null);
			String strMsg[] = new String[1];
			strMsg[0] = functionClassNameComplete;
			AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INTERNAL, "ET0005", strMsg, excp);
			throw excp;
		}

		return ObjFunctionToExec;
	}


	/*
	 *  Logging messaggio di inizio elaborazione di processo / funzione corrente
	 * 		
	 */
	private void loggingStartProcessFunction(ArrayList<ExecutionDirectives> al_execUnitDirectives, ExecutionDirectives curProcessFunctionInfo) {

		String[] arParm = new String[4];
		arParm[0] = curProcessFunctionInfo.en_CurProcessFunction.toString();
		arParm[1] = curProcessFunctionInfo.threadName;
		if (curProcessFunctionInfo.isToExecAsThread) {
			arParm[1] = curProcessFunctionInfo.threadName;
			arParm[2] = curProcessFunctionInfo.threadGroupName;
			if (!curProcessFunctionInfo.threadSubThreadName.equals("")) {
				arParm[3] = curProcessFunctionInfo.threadSubThreadName;
			} else {
				arParm[3] = "No";
			}
		} else {
			arParm[1] = "No";
			arParm[2] = "No";
			arParm[3] = "No";
		}

		// Processo/funzione started
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0016", arParm, null); 
	}

	/*
	 *  Logging messaggio di fine elaborazione di processo e funzione
	 * 		
	 */
	private void loggingEndProcessFunction(ArrayList<ExecutionDirectives> al_execUnitInfo, ExecutionDirectives curProcessFunctionInfo) {

		String excecutionState = "";
		String[] arParm = new String[4];
		int timeMsMean = 0;

		arParm[0] = curProcessFunctionInfo.en_CurProcessFunction.toString();
		arParm[1] = new Long(curProcessFunctionInfo.execMsAllElapsed).toString();
		timeMsMean = (int) (curProcessFunctionInfo.execMsAllElapsed / curProcessFunctionInfo.execTotObjectToProcess);
		arParm[2] = timeMsMean + "";

		// Con o senza errori
		if (isExecutionWithErrors(al_execUnitInfo)) {
			excecutionState = AmritaStartup.mm.getMessage(ucfg.getCurModule(), EnumMessageType.INFORMATION, "TC0002");
		} else {
			excecutionState = AmritaStartup.mm.getMessage(ucfg.getCurModule(), EnumMessageType.INFORMATION, "TC0001");
		}
		arParm[3] = excecutionState;

		// Processo/funzione terminato in x ms
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0017", arParm, null); 
	}

	/*
	 *  Restituisce la direttiva di esecuzione che ha generato l'exception
	 *  Se non ci sono exception restituisce -1
	 */
	private ExecutionDirectives getDirectiveWithException(ArrayList<ExecutionDirectives> al_directivesInput) {
		ExecutionDirectives di = null;

		// Scan gruppi di esecuzione separata
		for (int i = 0; i < al_directivesInput.size(); i++) {
			di = al_directivesInput.get(i);
			if (di.excpOccurred != null) {
				break;
			}

		}
		return di;
	}


	/*
	 *  Restituisce true se almeno una unità di esecuzione ha avuto l'elaborazione
	 *  terminata con exception
	 * 		
	 */
	@SuppressWarnings("unused")
	private boolean isExcpOccured(ArrayList<ExecutionDirectives> al_directivesInput) {
		boolean bRet = false;

		for (ExecutionDirectives directivesInfo : al_directivesInput) {
			if (directivesInfo.excpOccurred != null) {
				bRet = true;
				break;
			}
		}
		return bRet;
	}

	/*
	 *  Restituisce true se almeno una unità di esecuzione ha avuto l'elaborazione
	 *  terminata con errori
	 * 		
	 */
	private boolean isExecutionWithErrors(ArrayList<ExecutionDirectives> al_directivesInput) {
		boolean bRet = false;

		for (ExecutionDirectives directivesInfo : al_directivesInput) {
			if (directivesInfo.isExecutionWithErrors) {
				bRet = true;
				break;
			}
		}
		return bRet;
	}


	/**
	 * @return the current ExecutionDirective
	 */
	public ExecutionDirectives getDi() {
		return di;
	}


	/**
	 * @param di the current ExecutionDirective
	 */
	public void setDi(ExecutionDirectives di) {
		this.di = di;
	}

	
}
