package analyzer;

import java.sql.Connection;
import java.util.ArrayList;
import java.util.Locale;

import enums.EnumCobolReservedWords;
import enums.EnumDataBase;
import enums.EnumDirectivesExecution;
import enums.EnumInstrDataCategory;
import enums.EnumObject;
import enums.EnumRelation;
import enums.EnumSourceType;
import enums.EnumTypeProcessAnalysis;
import forward.ForwardFunction;
import forward.ForwardLogicalDataView;
//import forward.ForwardFunction;
//import forward.ForwardLogicalDataView;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ExecutionDirectives(DirectivesInfo)
 * </h1>
 * <p>
 * Questa classe descrive tutte le informazioni relative alle direttive di processo correnti, durante l'esecuzione
 * di un processo o di una funzione come elencato in {@link EnumDirectivesExecution}. <br>
 * <p>
 * Si tratta di informazioni di filtro, di reperimento di sistema e sottosistema e in generale  di tutte le informazioni 
 * presenti nel file pilota di esecuzione processi, prima della direttiva di esecuzione di un 
 * processo/funzione (es. FUNCTION_LIBRARY_SCAN).<br>
 * Oggetti di questa classe vengono istanziati da {@link ExecutionDispatcher} e passati alle classi specializzate
 * di gestione di processi e funzioni. Ciò garantisce l'autonomia completa delle classi specializzate e la
 * possibilità di eseguirle in thread completamente indipendenti separati.<b>
 * Questa classe rappresenta solo un contenitore e pertanto, per comodità di utilizzo, gli item vengono
 * dichiarati pubblici per essere referenziati direttamente senza metodi getter e setter.
 * le informazioni e opzioni modellate da questa classe vengono passate ai gestori di processi e funzioni,
 * e recuperate dai files pilota dei sorgenti, e da quello dei processi.
 * I file pilota vengono analizzati sequenzialmente e le variabili di istanza di questa classe vengono aggiornate<br>
 * prima di attivare il processo/funzione indicato.<br>
 * I processi e le funzioni applicative aggiornano la loro istanza corrente di questa classe per inserire stato di
 * esecuzione, come un'eccezione rilevata, o qualsiasi altra informazione che deve essere recuperata e gestita a monte.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/04/2010
 * @see ExecutionStarter
 * @see ExecutionDispatcher
 * @see _ExecutionLauncherFunction
 * @see _ExecutionLauncherProcess
*/
@SuppressWarnings("unused")
public class ExecutionDirectives implements Cloneable{

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza di reference a servizi generalizzati e gestori                                //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    
	private UserConfiguration sd = null;          					  			// Defaults e references globali come gestore messaggi, log etc	

	
	///////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	///////////////////////////////////////////////////////////////////////////////////
	
	// Processo/funzione da eseguire, identifica una istanza di questa classe
	public EnumDirectivesExecution en_CurProcessFunction = null;	// Processo/funzione corrente  
	
	// Input: Sistemi/sottosistemi da trattare, anche a range in PROCESS_LIBRARY_SCAN / PROCESS_ANALYZE_SOURCES / *
 	public String systemInput = "";                                 // Sistema      di cui considerare gli oggetti (* tutti)
 	public String subSystemInput = "";                              // SottoSistema di cui considerare gli oggetti (* tutti)
 	public String[] ar_subSystemInput = null;                       // SottoSistemi di cui considerare gli oggetti (* tutti)
	public String curCustomerCode = "";                             // Codice  cliente utilizzato in external UserExit da direttive di esecuzione
	public String curCustomerInfo = "";                             // Info specifiche utilizzate in external UserExit da direttive di esecuzione
	public int curSystemPos = 0;		      						// Posizione corrente partenza sistema nel nome 
	public int curSystemLng = 0;		      						// Lunghezza corrente sistema nel nome 
	public int curSubSystemPos = 0;		  							// Posizione corrente partenza sistema nel nome 
	public int curSubSystemLng = 0;		  							// Lunghezza corrente sistema nel nome 

	// Input: librerie in cui si trovano i sorgenti, librerie di ricerca sorgenti, path files specifici, suffissi con cui cercare moduli copy
	public ArrayList<String> al_libraryCode = null;		        	// Nomi librerie con sorgenti da analizzare o altro
	public ArrayList<String> al_libraryPath = null;		        	// Path librerie con sorgenti da analizzare o altro
	public ArrayList<String> al_librarySourceSearchCode = null;		// Nomi librerie di search in cui cercare i sorgenti in analisi
    public ArrayList<String> al_filePath = null;                    // Path completi files     in direttiva FILE    (parm 1)
	public ArrayList<String> al_fileSuffixCopy = null;	            // Es. cpy, cpp, ..

	// Input: tipologie oggetti da eleborare ammesse, elenco oggetti/tipo/range 
	public ArrayList<EnumDirectivesExecution> al_objectTypeToProcess = null;// Tipologia oggetti da elaborare in modo singolo indicati nelle direttive di processo
	public ArrayList<String> al_objectModeToProcess = null;		    // Corrispondente modalità *ALL* o *SINGLE*     
	public ArrayList<String> al_objectNameToProcess = null;		    // Corrispondente nome oggetto se *SINGLE*, "" altrimenti
	public ArrayList<String> al_objectNameToProcessFrom = null;	    // Corrispondente nome oggetto From se *ALL* FromName ToName
	public ArrayList<String> al_objectNameToProcessTo = null;	    // Corrispondente nome oggetto To   se *ALL* FromName ToName

	// Input: copy inseriti dai precompilatori di cui simulare l'inclusione
	public ArrayList<String> al_copyPrecompiler = null;				// Ogni entry contiene, separati da spazi CICS|SQL|DL1 LINKAGE|WORKING lib-ccopy-name 

	// Info: Library/patn/name oggetto correntemente sotto analisi/elaborazione
	public String libraryCodeCurObj = "";	               		    // (Analyzer     ) Nome liberia con sorgente corrente in analisi
	public String libraryPathCurObj = "";	               		    // (Analyzer     ) Path liberia con sorgente corrente in analisi
	public String filePathCurObj = "";	               		        // (Analyzer     ) File path sorgente corrente in analisi se non disponibile nome libreria
	public String fileNameCurObj = "";	               		        // (Analyzer     ) File name sorgente corrente in analisi completo di postfisso
    
	// Info: Sistema/sottosistema sorgenti/Oggetti e modalità di reperimento informazioni dinamiche  ????????
	public EnumDirectivesExecution en_activeObjectSystem = null;	// Modalità reperimento sistema oggetti
	public EnumDirectivesExecution en_activeObjectSubSystem = null; // Modalità reperimento sottosistema oggetti
	public String curMetricsViolationsScenario = "";                // Valido se optMetricsViolationsScenario
 
	// Filtri: oggetti/sorgenti da trattare fra quelli individuati in input
	public EnumDirectivesExecution en_filterType = null;			// Modalità reperimento criteri di filtro  
 	public ArrayList<FilterEntry> al_filterEntry = null;       	    // Filtri attivi per nomi sorgente/oggetti
 	public ArrayList<String> al_filterObjectName = null;       	    // Nomi sorgenti/oggetti da filtrare  fra quelli in input 
 	public String filterRangeObjectNameFrom = "";       	        // Range nome inizio sorgente/oggetto da filtrare  fra quelli in input 
 	public String filterRangeObjectNameTo = "";       	            // Range nome fine   sorgente/oggetto da filtrare  fra quelli in input 
    
 	// Filtri: stati oggetto da considerare
 	public ArrayList<EnumDirectivesExecution> al_objectInputStatus = null;	// Stati considerati processabili

	// Filtri: Esclusioni/oggetti/sorgenti da NON trattare fra quelli individuati in input e filtrati 
	public EnumDirectivesExecution en_excludeType = null;			// Modalità reperimento criteri di esclusione 
	public ArrayList<FilterEntry> al_excludeEntry = null;       	// exclude attivi per nomi sorgente/oggetti
 	public ArrayList<String> al_excludeObjectName = null;       	// Nomi sorgenti/oggetti da escludere da quelli individuati e filtrati
 	public String excludeRangeObjectNameFrom = "";       	        // Range nome inizio sorgente/oggetto da escludere da quelli individuati e filtrati
 	public String excludeRangeObjectNameTo = "";       	            // Range nome fine   sorgente/oggetto da escludere da quelli individuati e filtrati

	// Database
 	public EnumDataBase dataBaseType = null;                    	// (database     ) Tipologia database MSACCESS/MYSQL
 	public String dataBaseName = "";                             	// (database     ) Nome database/schema
 	public String dataBaseUser = "";                      	        // (database     ) User server
 	public String dataBasePwd = "";                                 // (database     ) Pwd
 	public String dataBaseDriver = "com.mysql.cj.jdbc.Driver";      // (database     ) Driver
	public String dataBaseAccessType = "";                 	        // (database     ) Accesso LOCAL/REMOTE
	public String dataBaseUrl = "";			 	                    // (database     ) Default Url Access, include database name
	public int dataBaseMaxConn = 1;                             	// (database     ) Numero massimo connessioni attive
	public int dataBaseCommitBlockUpdates = 100;                    // (database     ) Soglia istruzione di update da committare
	public boolean dataBaseLogAnySql = true;                        // (database     ) Log istruzioni Sql come messaggi informativi

 		
	// Opzioni:valori generici relativi ai vari processi/funzioni o comuni a più processi/funzioni 
	public String pgmName = "";                                     // (generic      ) Nome programma da elaborare
	public String fileNameOutput = "";                              // (generic      ) File di output per le funzioni che lo prevedono
	public String userExitClass = "UserExit";                		// (generic      ) Classe di exit utente 
	public boolean optDirectoryOnOutput = false;			        // (generic      ) La directory sorgente viene scritta nel file di output
	public boolean optDataBaseLogAnySql = false;	               	// (generic      ) Log di qualsiasi istruzione sql
	public boolean optUpdateDb = true;	               			 	// (generic      ) True = Aggiornamento db a fine processo/funzione
	public boolean optVerboseMessages = true;	               		// (generic      ) True = Livello verboso messaggi attivo
	public boolean optTraceAnyAnalysisErrorOnDb = true;	            // (generic      ) True = insert ogni errore di analisi su OBJE
	public boolean optMetricsAll = false;	               	        // (generic      ) Valutazione metriche complete 
	public boolean optMetricsBasic = true;	            			// (generic      ) Valutazione metriche di base dimensionali  
	public boolean optMetricsDetail = true;	            		    // (generic      ) Valutazione metriche di dettaglio mainline/section/programma
	public boolean optMetricsPgmDetailOnDb = false;	               	// (generic      ) Produzione dettaglio metriche di programma
	public boolean optMetricsMcCabe = false;	            		// (generic      ) Valutazione metriche di McCabe  
	public boolean optMetricsHalstead = false;	            		// (generic      ) Valutazione metriche di di Halstead
	public boolean optMetricsComplexityStructure = false;	        // (generic      ) Valutazione metriche di complessità strutturale fan-in e fan-out
 	public boolean optMetricsComplexityFunction = false;	        // (generic      ) Valutazione metriche di di complessità funzionale generica
	public boolean optMetricsFunctionPoint = false;	                // (generic      ) Valutazione metriche di function point
	public boolean optMetricsRehosting = false;	                    // (generic      ) Valutazione metriche di re-hosting
	public boolean optMetricsSquale = false;	                    // (generic      ) Valutazione metriche di SQUALE
	public boolean optMetricsSqualeDetectViolation = false;	        // (generic      ) Valutazione metriche di SQUALE. violazione a regole, standard etc.
	public boolean optMetricsViolationsScenario = false;	        // (generic      ) Utilizzo scenario di personalizzazione violazioni su EntityMetricScenario  
	 
	// Opzioni: Compatibilità di analisi cobol
	public boolean optDb2MainframeV10Compliance = false;	        // (compliance   ) Riconoscimento sintassi DB2 V10 Mainframe
	public boolean optDb2AixV19Compliance = false;	                // (compliance   ) Riconoscimento sintassi DB2 V10 Aix/Linux/Window
	
	// PROCESS_LIBRARY_SCAN
	public boolean optSearchFilesRecursive = false;			       	// (Library scan ) Indicas Se la ricerca viene effettuata ricorsivamente sui sources in input
	public boolean optDetectSourceType = true;	                    // (Library scan ) Individuazione tipo sorgente  
	public boolean optFullInfoFileSystem = false;	               	// (Library scan ) Informazioni complete di file system su files source
	
	// PROCESS_ANALYZE_SOURCES
	public boolean optStackTraceOnParsingError = false;	            // (Analyzer     ) Stack trace completo per ogni istruzione con errori di parsing
	public boolean optWhereUsedCopyItemsOnDbToInsert = true;	    // (Analyzer     ) Inserimento where used di ogni campo del copy
	public boolean optWhereUsedEntityColumnsOnDbToInsert = true;	// (Analyzer     ) Inserimento where used di ogni colonna di entity (Sql, Dl1, Vsam ...)
	public boolean optCopyDefOnDbToInsert = true;	                // (Analyzer     ) Creazione definizione items copy su data base
	public boolean optReAnalysisCopy = false;	               		// (Analyzer     ) Ri-Analisi copy anche se presenti in forma serializzata (refresh)
	public boolean optReBuildCopyDefOnDb = false;	                // (Analyzer     ) Ri-Creazione definizione items copy su data base
	public boolean optNormalizeSourceCode = false;	                // (Analyzer     ) Normalizzazione sorgente prima dell'analaisi

	public ArrayList<String> al_copyToForceReAnalysis = null;       // (Analyzer     ) Elenco copy dei quali forzare in ogni caso l'analisi sorgente
    public ArrayList<String> al_copyToForceReBuildOndb = null;      // (Analyzer     ) Elenco copy dei quali forzare in ogni caso la ricostruzione del tracciato su db
    
    // PROCESS_ANALYZE_SOURCES & PROCESS_PGM_LEVEL
	public boolean optDetectDeadCode = true;	               		// (Analyzer/pgm ) Intercettazione dead code a fine analisi programma
	public boolean optSolveDynamicLocal = true;	               	    // (Analyzer/pgm ) Attivazione LogicManager per soluzione istruzioni dinamiche locali
	public boolean optSolveDynamicLocalLight = true;	            // (Analyzer/pgm ) Attivazione LogicManager per soluzione istruzioni dinamiche locali (solo campi con value)
	public boolean optSolveCodeCics = true;	               	        // (Analyzer/pgm ) Soluzione istruzioni Cics
	public boolean optSolveDmlSql = true;	               	    	// (Analyzer/pgm ) Soluzione istruzioni Sql
	public boolean optSolveDmlDl1 = true;	               	    	// (Analyzer/pgm ) Soluzione istruzioni Dl1
	public boolean optSolveDmlAdabas = true;	               		// (Analyzer/pgm ) Soluzione istruzioni Adabas
    public ArrayList<String> al_cicsName = null;                    // (Analyzer/pgm ) Elenco Cics per estrazione valori esterni da tabelle DVAE/DCXM
    
	// PROCESS_SYSTEM_LEVEL
    public boolean optSolveDynamicSpreaded = false;	                // (SystemLevel	 ) Soluzione istruzioni dinamiche spreaded
	public boolean optDetectDataModel = true;	               	    // (SystemLevel	 ) Ricostruzione modello dati da oggetti e relazioni
	public boolean optDetectIndirectRelationships = true;	        // (SystemLevel	 ) Individuazione relazioni indirette 
	public boolean optAssignPhisicalToLogicalFiles = true;	        // (SystemLevel	 ) Assegnazione file fisico in relazioni con logic file
	public boolean optMetricsSystem = true;	               	        // (SystemLevel	 ) Valutazione metriche di sistema  (elaborazione di quelle di programma)
	
	// FUNCTION_PGM_SUMMARY
	public boolean optPgmSummaryAll = false;	                    // (PgmSummary	 ) Sommario programma con tutte le opzioni previste
	public boolean optListSourceCoded = true;	                    // (PgmSummary	 ) Lista programma codificato
	public boolean optListXrefLabel = true;	                        // (PgmSummary	 ) Lista Xref label
	public boolean optListXrefSection = true;	                    // (PgmSummary	 ) Lista Xref Sections
	public boolean optListXrefSymbols = true;	                    // (PgmSummary	 ) Lista Xref simboli (literal, data items)
	public boolean optListIOFileSystem = true;	                    // (PgmSummary	 ) Lista I-O file system effettuato dal pgm
	public boolean optListIOSql = true;	                            // (PgmSummary	 ) Lista I-O tabelle sql effettuato dal pgm
	public boolean optListIODl1 = true;	                            // (PgmSummary	 ) Lista I-O segmenti Dl1 effettuato dal pgm
	public boolean optListIOAdabas = true;	                        // (PgmSummary	 ) Lista I-O files Adabas effettuato dal pgm
	public boolean optListRelationships = true;	                    // (PgmSummary	 ) Lista Relazioni selettive fra oggetti
	public boolean optListRelationshipsOrigin = true;	            // (PgmSummary	 ) Lista origine Relazioni  
	public boolean optListOptions = true;	                        // (PgmSummary	 ) Lista opzioni di programma
	public boolean optListDynamicCodeInfo = true;	                // (PgmSummary	 ) Lista Info di dettaglio istruzioni dinamiche (pasths campi, sottocampi,..)
	public boolean optListDeadCode = true;	                        // (PgmSummary	 ) Lista dead code programma
	public boolean optListMetrics = true;	                		// (PgmSummary	 ) Lista metriche di programma
	public boolean optListCrudMatrix = true;	                	// (PgmSummary   ) Lista matrice CRUD a livello di programma
	public boolean optListProgramGraph = true;	                    // (PgmSummary	 ) Lista grafo di programma
	
	// FUNCTION_SYSTEM_SUMMARY
	public boolean optListSourcesMissing = true;	                // (SystemSummary) Lista sorgenti mancanti per completare analisi
	public boolean optListSourcesNotIdentified = true;	            // (SystemSummary) Lista sorgenti non identificati da LibraryScan
	public boolean optListObjectsSystem = true;	        			// (SystemSummary) Lista oggetti (pgm/files/..) con nomi, stato e opzioni specificate
	public boolean optListRelationshipsSystem = true;	            // (SystemSummary) Lista relazioni selettive per tipo, tipi oggetto, nomi ..
	public boolean optListRelationshipsOriginSystem = true;	        // (SystemSummary) Lista origine Relazioni  
	public boolean optListCrudMatrixSystem = true;	                // (SystemSummary) Lista matrice CRUD a livello di sistema
	public boolean optListMetricsSystem = true;	                    // (SystemSummary) Lista metriche di sistema

	// Limitazioni sul numero di sorgenti/oggetti processati
	public boolean limitMaxObjects = false;		    		        // Abilitazione controllo numero oggetti/sorgenti  processati
	public int limitMaxObjectsInput = 0;                   	  		// Numero massimo di oggetti filtrati da considerare 0 = NO limit
    public int limitMaxObjectsToProcess = 0;           				// Numero massimo di oggetti filtrati da processare  0 = NO limit

	// Inizio e termine elaborazione, modalità di lancio, Oggetto restituito, exception rilevate, oggetto corrente, ..
    public boolean isExecutionWithException = false;			    // True indica esecuzione terminata a causa di exception
    public boolean isExecutionWithErrors = false;					// True indica un'errore di esecuzione (eccezione gestita o meno)
    public int launchMethodMode = 0;								// Lancio statico metodo o con Invoke reflection. (vedi AmritaConstants)
    public Exception excpOccurred = null;                           // Eccezione provocata da processo/funzione applicativa
    public Object excpInfo = null;									// Oggetto Instruction/String/.. utile per il debug	a fronte di Excp non pilotata
    public Object objectInput = null;                               // Oggetto input  a  processo/funzione
    public Object objectOutput = null;                              // Oggetto output da processo/funzione
    public Connection dbConn = null;								// Connessione attiva con il database	

    // Dati esecuzione processi e funzioni Runtime: conteggi e durata visibili al client via web service
	public EnumDirectivesExecution execProcess = null;              // Processo/funzione in esecuzione
	public boolean execProcessRunning = false;                      // Il processo di elaborazione/analisi è in esecuzione
	public boolean execStopRequired = false;                        // Richiesto stop al processo dal Web
	public String execCurDtStartProcess = "";              			// AAAAMMGG di inizio elaborazione oggetto corrente
	public String execCurTmStartProcess = "";              			// HHMMSS di inizio elaborazione oggetto corrente
	public EnumObject execCurTypeObject = null;             	    // Tipo Oggetto in analisi/elaborazione (PGM/JCL, etc)
	public String execCurIdObject = "";             			    // Oggento corrente in elaborazione
	public int execMsAvg = 0;             			                // Millisecondi medio elaborazione oggetti
	public int execMsMax = 0;             			                // Millisecondi massimo elaborazione oggetti
	public int execMsMin = 0;             			                // Millisecondi massimo elaborazione oggetti
	public int execMsMaxIdObject = 0;             			        // Oggetto con Millisecondi massimo elaborazione 
	public int execMsMinIdObject = 0;             			        // Oggetto con Millisecondi massimo elaborazione 
	public int execMsCurExpectedEnd = 0;             			    // Millisecondi attesi alla fine corrente elaborazione
	public int execMsAllExpectedEnd = 0;             			    // Millisecondi attesi alla fine tutte le elaborazione
 	public long execMsAllStart = 0;             			        // Millisecondi iniziali elaborazione complessiva
    public long execMsAllEnd = 0;                                   // Millisecondi finali elaborazione complessiva
    public int execMsAllElapsed = 0;                                // Millisecondi Elapsed totale  
	public long execMsCurStart = 0;             			        // Millisecondi iniziali elaborazione oggetto corrente
	public long execMsCurEnd = 0;             			            // Millisecondi finali   elaborazione oggetto corrente
	public int execMsCurElapsed= 0;             			        // Millisecondi elaborazione oggetto corrente
    public String execCurObjectInfo = null;							// Info testuali formattate stato elaborazione oggetto (impostato dagli analizzatori)
    public int execTotObjectToProcess = 0;                          // Totale  oggetti da processare
    public int execCntObjectProcessed = 0;                          // Counter oggetti processati
    public int execCntObjectProcessedNoError = 0;                   // Counter oggetti processati senza errori
    public int execCntObjectProcessedError = 0;                     // Counter oggetti processati con errori
    public int execCntObjectProcessedExcp = 0;                      // Counter oggetti processati terminati da exception
    public ArrayList<String> exec_alObjectNameError = null;         // Elenco oggetti processati con errori
    public ArrayList<String> exec_alObjectNameExcp = null;          // Elenco oggetti processati terminati da exception
    

    // Informazioni su oggetto corrente sotto analisi (pgm, jcl, ddl, ..) anche a fronte di exception
    public boolean isRecursiveException = false;                    // Exception avvenuto a fronte di trattamento exception    
    public boolean curObjectWithErrors = false;				        // True indica un'errore di analisi/processo singolo oggetto
    public String curObjectId = "";									// Oggetto corrente sotto analisi/elaborazione 
    public UserExitInfo userExitInfoPgm = new UserExitInfo();		// Contiene info specifiche per il programma sotto analisi/elaborazione come sys/subsys
    public EnumObject curObjectType = null;							// Tipo oggetto corrente sotto analisi/elaborazione 
    public AnalyzerDbInfo curAnalyzerDbInfo = null;                 // Container di gestione db con oggetti info/errori pendenti
    public CopyCobol curCopyCobol = null;                           // Descrittore corrente copy      sotto analisi (con istruzioni analizzate)
    public ProgramCobol curProgramCobol = null;                     // Descrittore corrente programma sotto analisi (con istruzioni analizzate)
    public String curCopyUnderAnalysis = "";                        // Nome copy sotto analisi a fronte dell'analisi stmt copy del programma
    public EnumObject curTypeCopyUnderAnalysis = null;              // Tipo copy sotto analisi a fronte dell'analisi stmt copy del programma
    public ProgramCobolEntry<? extends Instruction> curEntryCobol = null;// Container istruzione corrente sotto analisi
    public Instruction curInstr = null;					    		// Istruzione corrente sotto analisi
    public Instruction lastInstrGood = null;					    // Istruzione precedente analizzata correttamente (impostato dagli analizzatori)
    public EnumTypeProcessAnalysis curTypeProcessAnalysis = null;   // Tipo processo corrente al momento dell'errore/abend
    public EnumInstrDataCategory curTypeInstr = null;        		// Tipo istruzione (Cobol nativa, Precompiler Sql, Cics, ..)
    public EnumCobolReservedWords curCobolDivision = null;			// Divisione cobol corrente attiva (impostato dagli analizzatori)
    public EnumCobolReservedWords curCobolSection = null;			// Sezione   cobol corrente attiva (impostato dagli analizzatori)
    public long curTimeMsStart = 0;                                 // Ora inizio esecuzione processo   singolo oggetto
    public long curTimeMsEnd = 0;                                   // Ora fine   esecuzione processo   singolo oggetto
    public long curTimeMsElapsed = 0;                               // Elapsed totale di elaborazione   singolo oggetto
    public int curNumRowSource = 0;                                 // Numero riga        sorgente corrente sotto analisi
    public int curNumInstr = 0;                                     // Numero istruzione  sorgente corrente sotto analisi di Id/Env/Data/Proc

    // Informazioni di internazionalizzazione
    public Locale curLocale = null;                                 // Locale corrente
    public String curLocaleLanguage = "";                           // Linguaggio come "it", "de", etc.
    public String curLocaleCountry = "";                            // Paese come "IT", "DE", etc.
    
	// Informazioni sulle modalità e lo stato di esecuzione di processo/funzione come thread separato
    public boolean isForwardClosing = false;                        // True indica richiesta di chiusura forward in corso
    public boolean isToExecAsThread = false;                        // True indica esecuzione di processo/funzione come thread
    public Thread thread = null;                                    // Reference a thread di esecuzione
    public ThreadGroup threadGroup = null;                          // Oggetto gruppo di appartenenza
    public int threadPrty = 0;                                      // Priorità thread
    public String threadName = "";                                  // Nome thread
    public String threadGroupName = "";                             // Nome gruppo di appartenenza
    public String threadSubThreadName = "";                         // Nome sub thread (Es. un programma source da analizzare)
    public int threadMaxSubThreads = 1;                             // Numero massimo sub threads (Es. per analyzer sources)

    /////// TODO Rimuovere e riferirsi a UserConfiguration //////////////////////////
	// Directories complete da file di configurazione generale clonate da UserConfiguration
    // Possono essere modificate per lo specifico processo/funzione
    public String dirPilot = "";                                	// Pilot with execution directives
    public String dirResources = "";                            	// Resource
    public String dirWork = "";                                 	// Working e temporaneo		    
	public String dirDatabase  = "";                            	// Database		              
	public String dirJclInput = "";                             	// Jcl    in input al processo di analisi (.*)        
	public String dirCobolSrcPgm  = "";                    			// Pgm    Cobol sorgenti in analisi		  (.*) 
	public String dirCobolSrcCopy = "";                    			// Copy   Cobol sorgenti in analisi		  (.*)
	public String dirCobolObjPgm = "";                  			// Pgm    Cobol codificati e serializzati (.program)			 
	public String dirCobolObjCopy = "";                            	// Copy   Cobol codificati e serializzati (.copy)	
	public String dirJclObj = "";                  			        // Jcl    codificati e serializzati (.jclSource, .jclInclude, .jclProc)		 
	public String dirSqlScript = "";                  			 	// Script Sql codificati e serializzati   (.scriptSql)			 
	public String dirCobolGraph = "";                			 	// Grafi  Cobol codificati e serializzati (.graph)	
	public String dirLog = "";                                 	    // Log
	public String dirOutput = "";                                 	// Output per funzioni 

    
    
    // Informazioni specifiche per forward
    public String fwdFunctionClassName = "";                        // Nome classe dichiarazione funzione
    public ForwardFunction fwdFunctionClass = null;                 // Descrittore dichiarazione funzione
    public String fwdLdvClassName = "";                        		// Nome classe dichiarazione logical data viw
    public ForwardLogicalDataView fwdLdvClass = null;               // Descrittore logical data view


    
    /*
	 *  
	 * Costruttore  
	 * 
	 */
	public ExecutionDirectives(UserConfiguration sd)  {
		this.sd = sd;

		al_objectTypeToProcess = new ArrayList<EnumDirectivesExecution>();
		al_objectModeToProcess = new ArrayList<String>();	     
		al_objectNameToProcess = new ArrayList<String>();		    
		al_objectNameToProcessFrom = new  ArrayList<String> ();  
		al_objectNameToProcessTo = new ArrayList<String> ();

		al_filterEntry = new ArrayList<FilterEntry>();
		al_excludeEntry = new ArrayList<FilterEntry>();
		al_libraryCode = new ArrayList<String>();		     
		al_libraryPath = new ArrayList<String>();		      
		al_filePath = new ArrayList<String>();		      
		al_fileSuffixCopy = new ArrayList<String> ();
		al_copyPrecompiler = new ArrayList<String> ();
		al_librarySourceSearchCode = new ArrayList<String>();	
		al_copyToForceReAnalysis = new ArrayList<String> ();
		al_copyToForceReBuildOndb = new ArrayList<String> ();
		al_cicsName = new ArrayList<String> ();
		al_filterObjectName = new ArrayList<String> ();
		al_excludeObjectName = new ArrayList<String> ();
	    exec_alObjectNameError = new ArrayList<String> ();
	    exec_alObjectNameExcp = new ArrayList<String> ();
	    al_objectInputStatus = new ArrayList<EnumDirectivesExecution> ();
	    
		en_filterType = EnumDirectivesExecution.FILTER_DISABLED;
    	en_excludeType = EnumDirectivesExecution.FILTER_DISABLED;

     	en_activeObjectSystem = EnumDirectivesExecution.NOT_ASSIGNED;
    	en_activeObjectSubSystem = EnumDirectivesExecution.NOT_ASSIGNED;
    	    	
        limitMaxObjectsInput = sd.getLimitMaxSourcesInput();
       	limitMaxObjectsToProcess = sd.getLimitMaxObjectsToProcess();
       	
        optDataBaseLogAnySql = sd.isDataBaseLogAnySql();
        fileNameOutput = sd.getFileOutput();
        dataBaseCommitBlockUpdates = sd.getDataBaseCommitBlockUpdates();
        userExitClass = sd.getUserExitClass();
        launchMethodMode = AmritaConstants.LAUNCH_METHOD_INVOKE;
        curObjectType = EnumObject.NOT_ASSIGNED;
        curCobolDivision = EnumCobolReservedWords .NOT_ASSIGNED;
        curCobolSection = EnumCobolReservedWords .NOT_ASSIGNED;
        curTypeInstr = EnumInstrDataCategory.NOT_ASSIGNED;
        curTypeProcessAnalysis = EnumTypeProcessAnalysis.NOT_ASSIGNED;

	}


	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected Object clone()  {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return this.en_CurProcessFunction.toString();
	}
	
	
}
