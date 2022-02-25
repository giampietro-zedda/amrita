package analyzer;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.sql.Connection;
import java.util.Locale;
import java.util.Properties;
import enums.EnumAmritaExceptionError;
import enums.EnumDataBase;
import enums.EnumDirectivesExecution;
import enums.EnumLanguage;
import enums.EnumMessageType;
import enums.EnumModule;
import enums.EnumObject;
import enums.EnumProcessStatus;
import enums.EnumUserStatus;
import enums.EnumUserType;
import exception.ExceptionAmrita;

/** 
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * UserConfiguration 
 * </h1> 
 * <p>
 * 
 * Questa classe descrive tutte i paramatri  a livello di utente dal file config.properties <br>
 * Un oggetto di questa classe viene istanziato in fase di login ed è quindi disponibile
 * a tutti gli altri oggetti.<br>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 2/11/2009
 *
*/
public class UserConfiguration implements java.io.Serializable{
	
	///////////////////////////////////////////////////////////////////////////////////
	// Informazioni di versione
	///////////////////////////////////////////////////////////////////////////////////
	
	private static final String amritaActiveVersion = "3.0.10";
	private static final String amritaLastModDate = "13/03/2010";
	private static final String amritaLastIssue = "Implementazione WebApp";
	private static final long serialVersionUID = 1L;
	
	///////////////////////////////////////////////////////////////////////////////////
	// identificazione e Esecuzione
	///////////////////////////////////////////////////////////////////////////////////
    
	private String user="";
	private String pwd  = null;
    private boolean userDefinedOnDb = false;
	private String baseUrl = "";
	private String companyCode = "";
	private String company = "";
	private EnumUserType userType  = null;
	private EnumUserStatus userStatus  = null;
	private String mail  = null;
	private String mailInfo  = null;
	private String referManager  = null;
	private String referTech  = null;
	private boolean analyzerEnabled = false;
	private boolean viewerEnabled = false;
	private boolean inspectorEnabled = false;
	private boolean assesmentEnabled = false;
	
	///////////////////////////////////////////////////////////////////////////////////
	// Reference a oggetti di default di uso comune RUNTIME 
	///////////////////////////////////////////////////////////////////////////////////
	
	// Dati generali, oggetti comuni, db
	private boolean execProcessRunning = false;                     // Il processo di elaborazione/analisi è in esecuzione
	private boolean execStopRequired = false;                       // Richiesto stop al processo dal Web
	private boolean excpOccurred = false;                           // Esecuzione terminata a causa di exception
	private Exception execExcp = null;                              // Exception che ha terminato l'esecuzione
	private Properties prConfig = null;	                      		// Properties di configurazione
	private MessagesManager messagesManager = null;           		// Bundle di messaggi
	private LoggerFacade lf = null;                   		  		// Gestore logging
	private DataBaseStatusDetailed dbsd = null;                     // Db status attivoper l'utente (gestita per exception)
	private Connection dbConn = null;                               // Ultima Connessione attiva per l'utente (gestita per exception)
	// Dati esecuzione processi (sono qui perchè devono essere accessibili a livello di utente)
	private EnumDirectivesExecution execProcess = null;             // processo/funzione in esecuzione
	private EnumProcessStatus execProcessStatus = null;             // Stato processo in esecuzione
	private int execTotObjectToProcess = 0;             			// Numero totale oggetti in analisi/elaborazione
	private int execCntObjectProcessed = 0;             			// Counter oggetti elaborati (include quello corrente)
    private int execCntObjectProcessedNoError = 0;                  // Counter oggetti processati senza errori
    private int execCntObjectProcessedError = 0;                    // Counter oggetti processati con errori
    private int execCntObjectProcessedExcp = 0;                     // Counter oggetti processati terminati da exception
	private EnumObject execCurTypeObject = null;                    // Tipo Oggetto in analisi/elaborazione (PGM/JCL, etc)
	private int execMsAvg = 0;             			                // Millisecondi medio elaborazione oggetti
	private int execMsMax = 0;             			                // Millisecondi massimo elaborazione oggetti
	private int execMsMin = 0;             			                // Millisecondi massimo elaborazione oggetti
	private String execMsMaxIdObject = "";             			    // Oggetto con Millisecondi massimo elaborazione 
	private String execMsMinIdObject = "";             			    // Oggetto con Millisecondi massimo elaborazione 
	private int execMsCurExpectedEnd = 0;             			    // Millisecondi attesi alla fine corrente elaborazione
	private int execMsAllExpectedEnd = 0;             			    // Millisecondi attesi alla fine tutte le elaborazione
	private long execMsAllStart = 0;             			        // Millisecondi iniziali elaborazione complessiva
	private long execMsAllEnd = 0;                                  // Millisecondi finali elaborazione complessiva
	private long execMsAllElapsed = 0;                              // Millisecondi Elapsed totale  
	private String execCurIdObject = "";             			    // Oggento corrente in elaborazione
	private long execMsCurStart = 0;             			        // Millisecondi iniziali elaborazione oggetto corrente
	private long execMsCurEnd = 0;             			            // Millisecondi finali   elaborazione oggetto corrente
	private long execMsCurElapsed= 0;             			        // Millisecondi elaborazione oggetto corrente
	
	///////////////////////////////////////////////////////////////////////////////////
	// Parametri di default centralizzati
	///////////////////////////////////////////////////////////////////////////////////
	
	// Internazionalizzazione
	private Locale currentLocale = null;                        	// Oggetto Locale corrente
	private EnumLanguage language = EnumLanguage.ITALIAN;   		// Linguaggio corrente
	private String strLanguage = "it";   							// Linguaggio corrente
	private String country = "IT";   			                	// Paese corrente
	
	// Identificazione modulo
	private EnumModule curModule = EnumModule.ANALYZER;        	 	// Modulo corrente
	private String strCurModule = EnumModule.ANALYZER.toString();	// Modulo corrente

	// Path completi
    private String pathRoot = "";                                   // Phisical path to WEB-INF 0r Installation application
    private String pathConfigFile = null;              				// Path completo file di configurazione (default properties iniziale allo startup)
    private String pathUser = "";                                   // Path completo folder delle dir utente (pilot, source, etc)
    private String pathPilot = "";                                  // Path completo folder path di default
    
	// Directories relative a pathUser
	private String dirResources = "";                            	// Resource
	private String dirWork = "";                                 	// Working e temporaneo		    
	private String dirDatabase  = "";                            	// Database		              
	private String dirJclSrc = "";                             		// Jcl    in input al processo di analisi (.*)        
	private String dirCobolSrcPgm  = "";                    		// Pgm    Cobol sorgenti in analisi		  (.*) 
	private String dirCobolSrcCopy = "";                    		// Copy   Cobol sorgenti in analisi		  (.*)
	private String dirBmsSrc = "";                    		        // Cics   BMS definition             	  (.*)
	private String dirCobolObjPgm = "";                  			// Pgm    Cobol codificati e serializzati (.program)			 
	private String dirCobolObjCopy = "";                            // Copy   Cobol codificati e serializzati (.copy)	
	private String dirJclObj = "";                  			    // Jcl    codificati e serializzati (.jclSource, .jclInclude, .jclProc)		 
	private String dirSqlSrcScript = "";                  			// Script Sql codificati e serializzati   (.scriptSql)			 
	private String dirCobolGraph = "";                			 	// Grafi  Cobol codificati e serializzati (.graph)	
	private String dirPilot = "";                                   // Pilot  sources e processi e filtri     (.pilot)
	private String dirLog = "";                                 	// Log
	private String dirOutput = "";                                 	// Output per funzioni che producono text

	// Ottimizzazione processi ed elaborazioni, allocazione di arrays, collections, map
	private int maxThreadsAnalisys = 10;                           	// Numero massimo threads concorrenti
	private int sizeAvgSource = 1000;                               // Numero medio righe sorgenti
	private int limitMaxLinesScanFindingSourceType = 200;           // Numero massimo righe da analizzare per individuare il tipo sorgente
	private boolean limitMaxSources = false;                        // Abilitazione limitazione ai sources trattati
	private int limitMaxSourcesInput = 0;                           // Numero massimo sorgenti da considerare in input
	private int limitMaxSourcesToProcess = 0;                       // Numero massimo sorgenti in input dei quali è stato intercettato il tipo
	private boolean limitMaxObjects = false;                        // Abilitazione limitazione agli oggetti processati
	private int limitMaxObjectsInput = 0;                           // Numero massimo oggetti da considerare in input ai processi (filtrati)
	private int limitMaxObjectsToProcess = 0;                       // Numero massimo oggetti in input da processare
	private int countAvgSource = 1000;                              // Numero indicativo sorgenti da trattare
	private long debugThresholdMemoryGarbage = 50000000;            // Attivazione gc() se memoria disponibile <
	private int debugSourcesDetectedFreqGarbage = 200;              // gc() attivata ogni 200 sorgenti
	private boolean debugActive = false;                        	// Attivazione debug dove previsto (messaggi log di debug)
	private boolean logVerbose = false;                           	// Dettaglio log operazioni Sql e informative
	private String preferredVisitMethod = "BACKWARD";            	// Metodo di visita predefinito
	private String preferredCachingLevel = "CACHING_PATH_ALL";   	// Livello di caching predefinito
	private String preferredCachingSupport = "CACHING_ON_HASH_MAP"; // Tipo supporto java di cache (.., CACHING_ON_TREE_MAP)
	
	// Database
	private String dataBaseType = "MYSQL";                    	    // Tipologia database MSACCESS/MYSQL
	private String dataBaseName = "DbAmrita";                      	// Nome database
	private String dataBaseUser = "GZEDDA";                      	// User
	private String dataBasePwd = "giampietro4";                     // Pwd
	private String dataBaseDriver = "com.mysql.cj.jdbc.Driver";     // Driver
	private String dataBaseAccessType = "LOCAL";                 	// Accesso LOCAL/REMOTE
	private String dataBaseUrl = "jdbc:mysql://localhost:3306/Amrita?autoReconnect=true&useSSL=false&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC";			 	
	                                                                // Default Url Access, include database name
	private int dataBaseMaxConn = 1;                             	// Numero massimo connessioni attive
	private int dataBaseCommitBlockUpdates = 100;                   // Commit automatica a fine gruppo aggiornamenti
	private boolean dataBaseLogAnySql = true;                       // Log istruzioni Sql come messaggi informativi
    	
	// Controllo analisi, identificazione oggetti analizzati/processati, piloti sources, filtri e processi
	private String pilotDefaultSource = "PilotDefaultSource.pilot";	 // Pilota        sorgenti
	private String pilotDefaultProcess = "PilotDefaultProcess.pilot";// Pilota        processi
	private String systemOwner = "A";								// Sistema 		proprietario degli oggetti
	private String subSystemOwner = "A";							// Sottosistema proprietario degli oggetti
	private String userExitClass = "UserExit"; 	                    // Classe con exit applicative codificate
	private String fileOutput = "";                                 // Id. file prodotto su DirOutput da FUNCTION_LIBRARY_SCAN
   
	/**
     *  Costruttore di default
     * @throws ExceptionAmrita 
     */
	public UserConfiguration() throws ExceptionAmrita {  
		super();
		this.userType=EnumUserType.NOT_ASSIGNED;
		this.userStatus=EnumUserStatus.NOT_ASSIGNED;
		this.execProcess=EnumDirectivesExecution.NOT_ASSIGNED;
		this.execProcessStatus=EnumProcessStatus.NOT_ASSIGNED;
		this.pathRoot=getPathRootWebApp();
	
	     // Get properties file configuration for the user
		this.pathConfigFile  = pathRoot
			                 + "resources" 
			                 + File.separator 
			                 + "config.properties";
		 
		loadFromPropertiesFile();
	}
	
	/**
     *  Costruttore con user
     * @throws ExceptionAmrita 
     */
	public UserConfiguration(String user) throws ExceptionAmrita {  
		super();
		this.userType=EnumUserType.NOT_ASSIGNED;
		this.userStatus=EnumUserStatus.NOT_ASSIGNED;
		this.user = user;		
		this.pathRoot=getPathRootWebApp();
		
	     // Get properties file configuration for the user
		this.pathConfigFile  = pathRoot
			                 + "resources" 
			                 + File.separator 
			                 + "config.properties";
		 
		loadFromPropertiesFile();
	}

    public String getPathRootWebApp() {
        String path = this.getClass().getClassLoader().getResource("").getPath();
        String pathRoot = "";
        try {
			String fullPath = URLDecoder.decode(path,"UTF-8");
			String pathArr[] = fullPath.split("classes/");
			pathRoot=pathArr[0];
			pathRoot=pathRoot.substring(1);
		} catch (UnsupportedEncodingException e) {
			ExceptionAmrita excp = new  ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_CONFIG_FILE_NOT_FOUND, null );
			excp.setMsg(this.pathConfigFile);
		}
        return pathRoot;
  	}

	/**
	 * 
	 * Caricamento coppie nomi/valore da Config.properties
	 * @throws ExceptionAmrita 
	 * 
	 */
	private void loadFromPropertiesFile() throws ExceptionAmrita {

		InputStream is = null;
		
		//////////////////////////////////////////////////////////////////////////////////////////// 
		// Individuazione file di properties
		//////////////////////////////////////////////////////////////////////////////////////////// 

		try {
			prConfig = new Properties();
			is = new FileInputStream(pathConfigFile);
			try {
				prConfig.load(is);
			} catch (IOException e) {
				if (lf == null) {throw new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_LOGGER_NOT_SET, e);}
				lf.writeRow(EnumMessageType.ERROR_FATAL, "0001", null, e);
				return;
			}
		} catch (FileNotFoundException e) {
			if (lf == null) {throw new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_LOGGER_NOT_SET, e);}
			lf.writeRow(EnumMessageType.ERROR_FATAL, "0002", null, e);
			return;
		}    	
		
		
		//////////////////////////////////////////////////////////////////////////////////////////// 
		// Caricamento valori da file .properties a  variabili di istanza                         //
		////////////////////////////////////////////////////////////////////////////////////////////
		
		// Internazionalizzazione
		strLanguage = prConfig.getProperty("Language", "it");
		country = "IT";
		if (strLanguage.equals(EnumLanguage.ITALIAN.toString())) {
			language = EnumLanguage.ITALIAN;
			country = prConfig.getProperty("Country", "IT");
		}  
		if (strLanguage.equals(EnumLanguage.ENGLISH.toString())) {
			language = EnumLanguage.ENGLISH;
			country = prConfig.getProperty("Country", "EN");
		}  
		if (strLanguage.equals(EnumLanguage.DEUTCH.toString())) {
			language = EnumLanguage.DEUTCH;
			country = prConfig.getProperty("Country", "DE");
		}  
		if (strLanguage.equals(EnumLanguage.FRENCH.toString())) {
			language = EnumLanguage.FRENCH;
		}  
		if (strLanguage.equals(EnumLanguage.SPANISH.toString())) {
			language = EnumLanguage.SPANISH;
			country = prConfig.getProperty("Country", "ES");
		} 
		currentLocale = new Locale(strLanguage, country);
		
		// Path
		pathRoot = prConfig.getProperty("PathRoot");
		pathConfigFile = prConfig.getProperty("PathConfigFile");
		pathPilot = prConfig.getProperty("PathPilot");
		pathUser = prConfig.getProperty("PathUser");
		
		// Directories
		dirPilot = prConfig.getProperty("DirPilot");
	    dirResources = prConfig.getProperty("DirResources");
		dirWork = prConfig.getProperty("DirWork");           
		dirCobolSrcPgm  = prConfig.getProperty("DirCobolSrcPgm");
		dirCobolSrcCopy = prConfig.getProperty("DirCobolSrcCopy");
		dirJclSrc = prConfig.getProperty("DirJclSrcInput");  
		dirSqlSrcScript = prConfig.getProperty("DirSqlSrcScript");		 
		dirBmsSrc = prConfig.getProperty("DirBmsSrc");		 
		dirCobolObjPgm = prConfig.getProperty("DirCobolObjPgm");		 
		dirCobolObjCopy = prConfig.getProperty("DirCobolObjCopy");
		dirJclObj = prConfig.getProperty("DirJclObj");
		dirCobolGraph = prConfig.getProperty("DirCobolGraph");
		dirLog = prConfig.getProperty("DirLog");
		dirOutput = prConfig.getProperty("DirOutput");
		
		// Ottimizzazione processi ed elaborazioni, allocazione di arrays e collections
		debugActive = Boolean.parseBoolean(prConfig.getProperty("DebugActive")); 
		logVerbose = Boolean.parseBoolean(prConfig.getProperty("LogVerbose")); 
		debugThresholdMemoryGarbage = Long.parseLong(prConfig.getProperty("DebugThresholdMemoryGarbage"));
		debugSourcesDetectedFreqGarbage = Integer.parseInt(prConfig.getProperty("DebugSourcesDetectedFreqGarbage"));
		maxThreadsAnalisys  = Integer.parseInt(prConfig.getProperty("MaxThreadsAnalisys"));  
		limitMaxLinesScanFindingSourceType  = Integer.parseInt(prConfig.getProperty("LimitMaxLinesScanFindingSourceType"));   
		limitMaxSources = Boolean.parseBoolean(prConfig.getProperty("LimitMaxSources")); 
		limitMaxSourcesInput  = Integer.parseInt(prConfig.getProperty("LimitMaxSourcesInput"));   
		limitMaxSourcesToProcess  = Integer.parseInt(prConfig.getProperty("LimitMaxSourcesToProcess"));
		limitMaxObjects = Boolean.parseBoolean(prConfig.getProperty("LimitMaxObjects")); 
		limitMaxObjectsInput  = Integer.parseInt(prConfig.getProperty("LimitMaxObjectsInput"));   
		limitMaxObjectsToProcess  = Integer.parseInt(prConfig.getProperty("LimitMaxObjectsToProcess"));   
		countAvgSource  = Integer.parseInt(prConfig.getProperty("CountAvgSource"));   
		sizeAvgSource  = Integer.parseInt(prConfig.getProperty("SizeAvgSource"));   
		preferredVisitMethod = prConfig.getProperty("PreferredVisitMethod");                
		preferredCachingLevel = prConfig.getProperty("PreferredCachingLevel");     
		preferredCachingSupport = prConfig.getProperty("PreferredCachingSupport");      

		// Analisi, identificazione e controllo oggetti analizzati/processati
		pilotDefaultSource = prConfig.getProperty("PilotSource");    
		pilotDefaultProcess = prConfig.getProperty("PilotProcess");    
		systemOwner = prConfig.getProperty("SystemOwner");      
		subSystemOwner = prConfig.getProperty("SubSystemOwner");      
		fileOutput = prConfig.getProperty("FileOutput");      
		
		// Database
		dataBaseType = prConfig.getProperty("DataBaseType");                    	 
		dataBaseName = prConfig.getProperty("DataBaseName");                      	 
		dataBaseUser = prConfig.getProperty("DataBaseUser");                      	 
		dataBasePwd = prConfig.getProperty("DataBasePwd");                       	 
		dataBaseDriver = prConfig.getProperty("DataBaseDriver");                    
		dataBaseAccessType = prConfig.getProperty("DataBaseAccessType");                 
		dataBaseUrl = prConfig.getProperty("DataBaseUrl");							  
		dataBaseMaxConn = Integer.parseInt(prConfig.getProperty("DataBaseMaxConn"));                             
		dataBaseCommitBlockUpdates = Integer.parseInt(prConfig.getProperty("DataBaseCommitBlockUpdates"));        
		dataBaseLogAnySql = Boolean.parseBoolean(prConfig.getProperty("DataBaseLogAnySql")); 
		
		// Logic User exit classes
		userExitClass = prConfig.getProperty("UserExitClass");     		
		
		return;
	}

	
	
	/**
	 * @return the amritaActiveVersion
	 */
	public static String getAmritaActiveVersion() {
		return amritaActiveVersion;
	}

	/**
	 * @return the amritaLastModDate
	 */
	public static String getAmritaLastModDate() {
		return amritaLastModDate;
	}

	/**
	 * @return the amritaLastIssue
	 */
	public static String getAmritaLastIssue() {
		return amritaLastIssue;
	}

	/**
	 * @return the module
	 */
	public EnumModule getCurModule() {
		return curModule;
	}

	/**
	 * @param module the module to set
	 */
	public void setCurModule(EnumModule curModule) {
		this.curModule = curModule;
		this.strCurModule = curModule.toString();
	}

	/**
	 * @return the strCurModule
	 */
	public String getStrCurModule() {
		return strCurModule;
	}

	/**
	 * @return the language
	 */
	public EnumLanguage getLanguage() {
		return language;
	}

	/**
	 * @param language the language to set
	 */
	public void setLanguage(EnumLanguage language) {
		this.language = language;
	}

	/**
	 * @return the strLanguage
	 */
	public String getStrLanguage() {
		return strLanguage;
	}

	/**
	 * @param strLanguage the strLanguage to set
	 */
	public void setStrLanguage(String strLanguage) {
		this.strLanguage = strLanguage;
	}

	/**
	 * @return the locale
	 */
	public Locale getCurrentLocale() {
		return currentLocale;
	}

	/**
	 * @param locale the locale to set
	 */
	public void setCurrentLocale(Locale currentLocale) {
		this.currentLocale = currentLocale;
	}

	/**
	 * @return the country
	 */
	public String getCountry() {
		return country;
	}

	/**
	 * @param country the country to set
	 */
	public void setCountry(String country) {
		this.country = country;
	}

	/**
	 * @return the pathResources
	 */
	public String getDirResources() {
		return dirResources;
	}

	/**
	 * @param pathResources the pathResources to set
	 */
	public void setDirResources(String dirResources) {
		this.dirResources = dirResources;
	}

	/**
	 * @return the dirPilotAndFilter
	 */
	public String getDirPilot() {
		return dirPilot;
	}

	/**
	 * @param dirPilotAndFilter the dirPilotAndFilter to set
	 */
	public void setDirPilot(String dirPilotAndFilter) {
		this.dirPilot = dirPilotAndFilter;
	}

	/**
	 * @return the dirLog
	 */
	public String getDirLog() {
		return dirLog;
	}

	/**
	 * @param dirLog the dirLog to set
	 */
	public void setDirLog(String dirLog) {
		this.dirLog = dirLog;
	}

	/**
	 * @return the dirOutput
	 */
	public String getDirOutput() {
		return dirOutput;
	}

	/**
	 * @param dirOutput the dirOutput to set
	 */
	public void setDirOutput(String dirOutput) {
		this.dirOutput = dirOutput;
	}

	/**
	 * @return the dirWork
	 */
	public String getDirWork() {
		return dirWork;
	}

	/**
	 * @param dirWork the dirWork to set
	 */
	public void setDirWork(String dirWork) {
		this.dirWork = dirWork;
	}

	/**
	 * @return the dirDatabase
	 */
	public String getDirDatabase() {
		return dirDatabase;
	}

	/**
	 * @param dirDatabase the dirDatabase to set
	 */
	public void setDirDatabase(String dirDatabase) {
		this.dirDatabase = dirDatabase;
	}

	/**
	 * @return the dirJclInput
	 */
	public String getDirJclSrc() {
		return dirJclSrc;
	}

	/**
	 * @param dirJclInput the dirJclInput to set
	 */
	public void setDirJclSrc(String dirJclSrc) {
		this.dirJclSrc = dirJclSrc;
	}

	/**
	 * @return the dirCobolSrcPgmInput
	 */
	public String getDirCobolSrcPgm() {
		return dirCobolSrcPgm;
	}

	/**
	 * @param dirCobolSrcPgmInput the dirCobolSrcPgmInput to set
	 */
	public void setDirCobolSrcPgm(String dirCobolSrcPgm) {
		this.dirCobolSrcPgm = dirCobolSrcPgm;
	}

	/**
	 * @return the dirCobolSrcCopyInput
	 */
	public String getDirCobolSrcCopy() {
		return dirCobolSrcCopy;
	}

	/**
	 * @param dirCobolSrcCopyInput the dirCobolSrcCopyInput to set
	 */
	public void setDirCobolSrcCopy(String dirCobolSrcCopy) {
		this.dirCobolSrcCopy = dirCobolSrcCopy;
	}

	/**
	 * @return the dirCobolPgm
	 */
	public String getDirCobolObjPgm() {
		return dirCobolObjPgm;
	}

	/**
	 * @param dirCobolPgm the dirCobolPgm to set
	 */
	public void setDirCobolObjPgm(String dirCobolPgm) {
		this.dirCobolObjPgm = dirCobolPgm;
	}

	/**
	 * @return the dirSqlScript
	 */
	public String getDirSqlScript() {
		return dirSqlSrcScript;
	}

	/**
	 * @param dirSqlScript the dirSqlScript to set
	 */
	public void setDirSqlScript(String dirSqlScript) {
		this.dirSqlSrcScript = dirSqlScript;
	}

	/**
	 * @return the dirJcl
	 */
	public String getDirJclObj() {
		return dirJclObj;
	}

	/**
	 * @param dirJcl the dirJcl to set
	 */
	public void setDirJclObj(String dirJcl) {
		this.dirJclObj = dirJcl;
	}

	/**
	 * @return the dirCobolGraph
	 */
	public String getDirCobolGraph() {
		return dirCobolGraph;
	}

	/**
	 * @param dirCobolGraph the dirCobolGraph to set
	 */
	public void setDirCobolGraph(String dirCobolGraph) {
		this.dirCobolGraph = dirCobolGraph;
	}

	/**
	 * @return the prConfig
	 */
	public Properties getPrConfig() {
		return prConfig;
	}

	/**
	 * @param prConfig the prConfig to set
	 */
	public void setPrConfig(Properties prConfig) {
		this.prConfig = prConfig;
	}

	/**
	 * @return the messagesManager
	 */
	public MessagesManager getMessagesManager() {
		return messagesManager;
	}

	/**
	 * @param messageStore the messageStore to set
	 */
	public void setMessagesManager(MessagesManager messageStore) {
		this.messagesManager = messageStore;
	}

	/**
	 * @return the lf
	 */
	public LoggerFacade getLoggerFacade() {
		return lf;
	}

	/**
	 * @param loggerFacade the loggerFacade to set
	 */
	public void setLoggerFacade(LoggerFacade lf) {
		this.lf = lf;
	}

	/**
	 * @return the maxThreadsAnalisys
	 */
	public int getMaxThreadsAnalisys() {
		return maxThreadsAnalisys;
	}

	/**
	 * @param maxThreadsAnalisys the maxThreadsAnalisys to set
	 */
	public void setMaxThreadsAnalisys(int maxThreadsAnalisys) {
		this.maxThreadsAnalisys = maxThreadsAnalisys;
	}

	/**
	 * @return the LimitMaxLinesScanFindingSourceType
	 */
	public int getLimitMaxLinesScanFindingSourceType() {
		return limitMaxLinesScanFindingSourceType;
	}
     
	/**
	 * @param LimitMaxLinesScanFindingSourceType the LimitMaxLinesScanFindingSourceType to set
	 */
	public void setLimitMaxLinesScanFindingSourceType(int limitMaxLinesScanFindingSourceType) {
		this.limitMaxLinesScanFindingSourceType = limitMaxLinesScanFindingSourceType;
	}

	
	/**
	 * @return the limitMaxSourcesInput
	 */
	public int getLimitMaxSourcesInput() {
		return limitMaxSourcesInput;
	}

	/**
	 * @param limitMaxSourcesInput the limitMaxSourcesInput to set
	 */
	public void setLimitMaxSourcesInput(int limitMaxSourcesInput) {
		this.limitMaxSourcesInput = limitMaxSourcesInput;
	}


	/**
	 * @return the limitMaxSourcesInputToProcess
	 */
	public int getLimitMaxSourcesToProcess() {
		return limitMaxSourcesToProcess;
	}

	/**
	 * @param limitMaxSourcesInputToProcess the limitMaxSourcesInputToProcess to set
	 */
	public void setLimitMaxSourcesToProcess(int limitMaxSourcesToProcess) {
		this.limitMaxSourcesToProcess = limitMaxSourcesToProcess;
	}

	/**
	 * @return the limitMaxSources
	 */
	public boolean isLimitMaxSources() {
		return limitMaxSources;
	}

	/**
	 * @param limitMaxSources the limitMaxSources to set
	 */
	public void setLimitMaxSources(boolean limitMaxSources) {
		this.limitMaxSources = limitMaxSources;
	}

	/**
	 * @return the limitMaxObjects
	 */
	public boolean isLimitMaxObjects() {
		return limitMaxObjects;
	}

	/**
	 * @param limitMaxObjects the limitMaxObjects to set
	 */
	public void setLimitMaxObjects(boolean limitMaxObjects) {
		this.limitMaxObjects = limitMaxObjects;
	}

	/**
	 * @param limitMaxObjectsToProcess the limitMaxObjectsToProcess to set
	 */
	public void setLimitMaxObjectsToProcess(int limitMaxObjectsToProcess) {
		this.limitMaxObjectsToProcess = limitMaxObjectsToProcess;
	}

	/**
	 * @return the limitMaxObjectsInput
	 */
	public int getLimitMaxObjectsInput() {
		return limitMaxObjectsInput;
	}

	/**
	 * @param limitMaxObjectsInput the limitMaxObjectsInput to set
	 */
	public void setLimitMaxObjectsInput(int limitMaxObjectsInput) {
		this.limitMaxObjectsInput = limitMaxObjectsInput;
	}

	/**
	 * @return the limitMaxObjectsInputToProcess
	 */
	public int getLimitMaxObjectsToProcess() {
		return limitMaxObjectsToProcess;
	}

	/**
	 * @param limitMaxObjectsInputToProcess the limitMaxObjectsInputToProcess to set
	 */
	public void setLimitMaxObjectsInputToProcess(int limitMaxObjectsToProcess) {
		this.limitMaxObjectsToProcess = limitMaxObjectsToProcess;
	}

	/**
	 * @return the countAvgSource
	 */
	public int getCountAvgSource() {
		return countAvgSource;
	}

	/**
	 * @param countAvgSource the countAvgSource to set
	 */
	public void setCountAvgSource(int countAvgSource) {
		this.countAvgSource = countAvgSource;
	}

	/**
	 * @return the sizeAvgSource
	 */
	public int getSizeAvgSource() {
		return sizeAvgSource;
	}

	/**
	 * @param sizeAvgSource the sizeAvgSource to set
	 */
	public void setSizeAvgSource(int sizeAvgSource) {
		this.sizeAvgSource = sizeAvgSource;
	}

	/**
	 * @return the preferredVisitMethod
	 */
	public String getPreferredVisitMethod() {
		return preferredVisitMethod;
	}

	/**
	 * @param preferredVisitMethod the preferredVisitMethod to set
	 */
	public void setPreferredVisitMethod(String preferredVisitMethod) {
		this.preferredVisitMethod = preferredVisitMethod;
	}

	/**
	 * @return the preferredCachingLevel
	 */
	public String getPreferredCachingLevel() {
		return preferredCachingLevel;
	}

	/**
	 * @return the preferredCachingSupport
	 */
	public String getPreferredCachingSupport() {
		return preferredCachingSupport;
	}

	/**
	 * @param preferredCachingSupport the preferredCachingSupport to set
	 */
	public void setPreferredCachingSupport(String preferredCachingSupport) {
		this.preferredCachingSupport = preferredCachingSupport;
	}

	/**
	 * @return the debugActive
	 */
	public boolean isDebugActive() {
		return debugActive;
	}

	/**
	 * @param debugActive the debugActive to set
	 */
	public void setDebugActive(boolean bDebugActive) {
		this.debugActive = bDebugActive;
	}

	/**
	 * @return the logVerbose
	 */
	public boolean isLogVerbose() {
		return logVerbose;
	}

	/**
	 * @param logVerbose the logVerbose to set
	 */
	public void setLogVerbose(boolean logVerbose) {
		this.logVerbose = logVerbose;
	}

	/**
	 * @return the debugThresholdMemoryGarbage
	 */
	public long getDebugThresholdMemoryGarbage() {
		return debugThresholdMemoryGarbage;
	}

	/**
	 * @param debugThresholdMemoryGarbage the debugThresholdGarbageMemory to set
	 */
	public void setDebugThresholdMemoryGarbage(long debugThresholdMemoryGarbage) {
		this.debugThresholdMemoryGarbage = debugThresholdMemoryGarbage;
	}

	/**
	 * @return the debugSourcesDetectedFreqGarbage
	 */
	public int getDebugSourcesDetectedFreqGarbage() {
		return debugSourcesDetectedFreqGarbage;
	}

	/**
	 * @param debugSourcesDetectedFreqGarbage the debugSourcesDetectedFreqGarbage to set
	 */
	public void setDebugSourcesDetectedFreqGarbage(
			int debugSourcesDetectedFreqGarbage) {
		this.debugSourcesDetectedFreqGarbage = debugSourcesDetectedFreqGarbage;
	}

	/**
	 * @param preferredCachingLevel the preferredCachingLevel to set
	 */
	public void setPreferredCachingLevel(String preferredCachingLevel) {
		this.preferredCachingLevel = preferredCachingLevel;
	}

	/**
	 * @return the dataBaseType
	 */
	public EnumDataBase getDataBaseType() {
		EnumDataBase en_db = null;
		
		for (EnumDataBase e : EnumDataBase.values()) {
			
			if (e.toString().equals(dataBaseType)) {
				en_db = e;
				break;
			}
		}
		
		return en_db;
	}

	/**
	 * @param dataBaseType the dataBaseType to set
	 */
	public void setDataBaseType(String dataBaseType) {
		this.dataBaseType = dataBaseType;
	}

	/**
	 * @return the dataBaseName
	 */
	public String getDataBaseName() {
		return dataBaseName;
	}

	/**
	 * @param dataBaseName the dataBaseName to set
	 */
	public void setDataBaseName(String dataBaseName) {
		this.dataBaseName = dataBaseName;
	}

	/**
	 * @return the dataBaseUser
	 */
	public String getDataBaseUser() {
		return dataBaseUser;
	}

	/**
	 * @param dataBaseUser the dataBaseUser to set
	 */
	public void setDataBaseUser(String dataBaseUser) {
		this.dataBaseUser = dataBaseUser;
	}

	/**
	 * @return the dataBasePwd
	 */
	public String getDataBasePwd() {
		return dataBasePwd;
	}

	/**
	 * @param dataBasePwd the dataBasePwd to set
	 */
	public void setDataBasePwd(String dataBasePwd) {
		this.dataBasePwd = dataBasePwd;
	}

	/**
	 * @return the dataBaseDriver
	 */
	public String getDataBaseDriver() {
		return dataBaseDriver;
	}

	/**
	 * @param dataBaseDriver the dataBaseDriver to set
	 */
	public void setDataBaseDriver(String dataBaseDriver) {
		this.dataBaseDriver = dataBaseDriver;
	}

	/**
	 * @return the dataBaseAccessType
	 */
	public String getDataBaseAccessType() {
		return dataBaseAccessType;
	}

	/**
	 * @param dataBaseAccessType the dataBaseAccessType to set
	 */
	public void setDataBaseAccessType(String dataBaseAccessType) {
		this.dataBaseAccessType = dataBaseAccessType;
	}

	/**
	 * @return the dataBaseUrl
	 */
	public String getDataBaseUrl() {
		return dataBaseUrl;
	}

	/**
	 * @param dataBaseUrl the dataBaseUrl to set
	 */
	public void setDataBaseUrl(String dataBaseUrl) {
		this.dataBaseUrl = dataBaseUrl;
	}

	/**
	 * @return the dataBaseMaxConn
	 */
	public int getDataBaseMaxConn() {
		return dataBaseMaxConn;
	}

	/**
	 * @param dataBaseMaxConn the dataBaseMaxConn to set
	 */
	public void setDataBaseMaxConn(int dataBaseMaxConn) {
		this.dataBaseMaxConn = dataBaseMaxConn;
	}

	

	/**
	 * @return the dataBaseCommitBlockUpdates
	 */
	public int getDataBaseCommitBlockUpdates() {
		return this.dataBaseCommitBlockUpdates;
	}

	/**
	 * @param dataBaseCommitBlockUpdates the dataBaseCommitBlockUpdates to set
	 */
	public void setDataBaseCommitBlockUpdates(int dataBaseCommitBlockUpdates) {
		this.dataBaseCommitBlockUpdates = dataBaseCommitBlockUpdates;
	}

	/**
	 * @return the optDataBaseLogAnySql
	 */
	public boolean isDataBaseLogAnySql() {
		return dataBaseLogAnySql;
	}

	/**
	 * @param optDataBaseLogAnySql the optDataBaseLogAnySql to set
	 */
	public void setDataBaseLogAnySql(boolean dataBaseLogAnySql) {
		this.dataBaseLogAnySql = dataBaseLogAnySql;
	}

	/**
	 * @return the pilotDefaultSource
	 */
	public String getPilotDefaultSource() {
		return pilotDefaultSource;
	}

	/**
	 * @param pilotDefaultSource the pilotDefaultSource to set
	 */
	public void setPilotDefaultSource(String pilotDefaultSource) {
		this.pilotDefaultSource = pilotDefaultSource;
	}

	/**
	 * @return the pilotDefaultProcess
	 */
	public String getPilotDefaultProcess() {
		return pilotDefaultProcess;
	}

	/**
	 * @param pilotDefaultProcess the pilotDefaultProcess to set
	 */
	public void setPilotDefaultProcess(String pilotDefaultProcess) {
		this.pilotDefaultProcess = pilotDefaultProcess;
	}

	/**
	 * @return the system
	 */
	public String getSystemOwner() {
		return systemOwner;
	}

	/**
	 * @param system the system to set
	 */
	public void setSystemOwner(String systemOwner) {
		this.systemOwner = systemOwner;
	}

	/**
	 * @return the subSystem
	 */
	public String getSubSystemOwner() {
		return subSystemOwner;
	}

	/**
	 * @param subSystem the subSystem to set
	 */
	public void setSubSystemOwner(String subSystemOwner) {
		this.subSystemOwner = subSystemOwner;
	}

	/**
	 * @return the fileOutput da produrre
	 */
	public String getFileOutput() {
		return fileOutput;
	}

	
	/**
	 * @param fileOutput da produrre
	 */
	public void setFileOutput(String fileOutput) {
		this.fileOutput = fileOutput;
	}


	/**
	 * @return the exitClass 
	 */
	public String getUserExitClass() {
		return userExitClass;
	}

	/**
	 * @param exitClassCopyCobol the exitClassCopyCobol to set
	 */
	public void setUserExitClass(String userExitClass) {
		this.userExitClass = userExitClass;
	}

	/**
	 * @param strCurModule the strCurModule to set
	 */
	public void setStrCurModule(String strCurModule) {
		this.strCurModule = strCurModule;
	}

	/**
	 * @return the pathConfigFile
	 */
	public String getPathConfigFile() {
		return pathConfigFile;
	}

	/**
	 * 
	 * @return the pathRoot
	 */
	public String getPathRoot() {
		return pathRoot;
	}

	/**
	 * @return the pathUser
	 */
	public String getPathUser() {
		return pathUser;
	}

	/**
	 * @return the user
	 */
	public String getUser() {
		return user;
	}

	/**
	 * @param user the user to set
	 */
	public void setUser(String user) {
		this.user = user;
	}

	/**
	 * @return the baseUrl
	 */
	public String getBaseUrl() {
		return baseUrl;
	}

	/**
	 * @param baseUrl the baseUrl to set
	 */
	public void setBaseUrl(String baseUrl) {
		this.baseUrl = baseUrl;
	}

	/**
	 * @return the companyCode
	 */
	public String getCompanyCode() {
		return companyCode;
	}

	/**
	 * @param companyCode the companyCode to set
	 */
	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	/**
	 * @return the pwd
	 */
	public String getPwd() {
		return pwd;
	}

	/**
	 * @param pwd the pwd to set
	 */
	public void setPwd(String pwd) {
		this.pwd = pwd;
	}

	/**
	 * @return the mail
	 */
	public String getMail() {
		return mail;
	}

	/**
	 * @param mail the mail to set
	 */
	public void setMail(String mail) {
		this.mail = mail;
	}

	/**
	 * @return the mailInfo
	 */
	public String getMailInfo() {
		return mailInfo;
	}

	/**
	 * @param mailInfo the mailInfo to set
	 */
	public void setMailInfo(String mailInfo) {
		this.mailInfo = mailInfo;
	}

	/**
	 * @return the referManager
	 */
	public String getReferManager() {
		return referManager;
	}

	/**
	 * @param referManager the referManager to set
	 */
	public void setReferManager(String referManager) {
		this.referManager = referManager;
	}

	/**
	 * @return the referTech
	 */
	public String getReferTech() {
		return referTech;
	}

	/**
	 * @param referTech the referTech to set
	 */
	public void setReferTech(String referTech) {
		this.referTech = referTech;
	}

	/**
	 * @param pathRoot the pathRoot to set
	 */
	public void setPathRoot(String pathRoot) {
		this.pathRoot = pathRoot;
	}

	/**
	 * @param pathConfigFile the pathConfigFile to set
	 */
	public void setPathConfigFile(String pathConfigFile) {
		this.pathConfigFile = pathConfigFile;
	}

	/**
	 * @param pathUser the pathUser to set
	 */
	public void setPathUser(String pathUser) {
		this.pathUser = pathUser;
	}

	/**
	 * @return the userType
	 */
	public EnumUserType getUserType() {
		return userType;
	}

	/**
	 * @param userType the userType to set
	 */
	public void setUserType(EnumUserType userType) {
		this.userType = userType;
	}

	/**
	 * @return the userStatus
	 */
	public EnumUserStatus getUserStatus() {
		return userStatus;
	}

	/**
	 * @param userStatus the userStatus to set
	 */
	public void setUserStatus(EnumUserStatus userStatus) {
		this.userStatus = userStatus;
	}

	/**
	 * @return the company
	 */
	public String getCompany() {
		return company;
	}

	/**
	 * @param company the company to set
	 */
	public void setCompany(String company) {
		this.company = company;
	}

	/**
	 * @return the analyzerEnabled
	 */
	public boolean isAnalyzerEnabled() {
		return analyzerEnabled;
	}

	/**
	 * @param analyzerEnabled the analyzerEnabled to set
	 */
	public void setAnalyzerEnabled(boolean analyzerEnabled) {
		this.analyzerEnabled = analyzerEnabled;
	}

	/**
	 * @return the viewerEnabled
	 */
	public boolean isViewerEnabled() {
		return viewerEnabled;
	}

	/**
	 * @param viewerEnabled the viewerEnabled to set
	 */
	public void setViewerEnabled(boolean viewerEnabled) {
		this.viewerEnabled = viewerEnabled;
	}

	/**
	 * @return the inspectorEnabled
	 */
	public boolean isInspectorEnabled() {
		return inspectorEnabled;
	}

	/**
	 * @param inspectorEnabled the inspectorEnabled to set
	 */
	public void setInspectorEnabled(boolean inspectorEnabled) {
		this.inspectorEnabled = inspectorEnabled;
	}

	/**
	 * @return the assesmentEnabled
	 */
	public boolean isAssesmentEnabled() {
		return assesmentEnabled;
	}

	/**
	 * @param assesmentEnabled the assesmentEnabled to set
	 */
	public void setAssesmentEnabled(boolean assesmentEnabled) {
		this.assesmentEnabled = assesmentEnabled;
	}

	/**
	 * @return the userDefinedOnDb
	 */
	public boolean isUserDefinedOnDb() {
		return userDefinedOnDb;
	}

	/**
	 * @param userDefinedOnDb the userDefinedOnDb to set
	 */
	public void setUserDefinedOnDb(boolean userDefinedOnDb) {
		this.userDefinedOnDb = userDefinedOnDb;
	}

	/**
	 * @return the pathPilot
	 */
	public String getPathPilot() {
		return pathPilot;
	}

	/**
	 * @param pathPilot the pathPilot to set
	 */
	public void setPathPilot(String pathPilot) {
		this.pathPilot = pathPilot;
	}

	/**
	 * @return the dirBmsSrc
	 */
	public String getDirBmsSrc() {
		return dirBmsSrc;
	}

	/**
	 * @param dirBmsSrc the dirBmsSrc to set
	 */
	public void setDirBmsSrc(String dirBmsSrc) {
		this.dirBmsSrc = dirBmsSrc;
	}

	/**
	 * @return the dirCobolObjCopy
	 */
	public String getDirCobolObjCopy() {
		return dirCobolObjCopy;
	}

	/**
	 * @param dirCobolObjCopy the dirCobolObjCopy to set
	 */
	public void setDirCobolObjCopy(String dirCobolObjCopy) {
		this.dirCobolObjCopy = dirCobolObjCopy;
	}

	/**
	 * @return the dirSqlSrcScript
	 */
	public String getDirSqlSrcScript() {
		return dirSqlSrcScript;
	}

	/**
	 * @param dirSqlSrcScript the dirSqlSrcScript to set
	 */
	public void setDirSqlSrcScript(String dirSqlSrcScript) {
		this.dirSqlSrcScript = dirSqlSrcScript;
	}

	/**
	 * @return the dbConn Data Base Connection
	 */
	public Connection getDbConn() {
		return dbConn;
	}

	/**
	 * @param dbConn the database connection to set
	 */
	public void setDbConn(Connection dbConn) {
		this.dbConn = dbConn;
	}

	/**
	 * @return the dbsd
	 */
	public DataBaseStatusDetailed getDbsd() {
		return dbsd;
	}

	/**
	 * @param dbsd the dbsd to set
	 */
	public void setDbsd(DataBaseStatusDetailed dbsd) {
		this.dbsd = dbsd;
	}

	/**
	 * @return the execProcess
	 */
	public EnumDirectivesExecution getExecProcess() {
		return execProcess;
	}

	/**
	 * @param execProcess the execProcess to set
	 */
	public void setExecProcess(EnumDirectivesExecution execProcess) {
		this.execProcess = execProcess;
	}

	/**
	 * @return the execCurTypeObject
	 */
	public EnumObject getExecCurTypeObject() {
		return execCurTypeObject;
	}

	/**
	 * @param execTypeObject the execTypeObject to set
	 */
	public void setExecCurTypeObject(EnumObject execCurTypeObject) {
		this.execCurTypeObject = execCurTypeObject;
	}

	/**
	 * @return the execTotObjects
	 */
	public int getExecTotObjectToProcess() {
		return execTotObjectToProcess;
	}

	/**
	 * @param execTotObjects the execTotObjects to set
	 */
	public void setExecTotObjectToProcess(int execTotObjects) {
		this.execTotObjectToProcess = execTotObjects;
	}

	/**
	 * @return the execCntObjects
	 */
	public int getExecCntObjectProcessed() {
		return execCntObjectProcessed;
	}

	/**
	 * @param execCntObjects the execCntObjects to set
	 */
	public void setExecCntObjectProcessed(int execCntObjects) {
		this.execCntObjectProcessed = execCntObjects;
	}

	/**
	 * @return the execCurIdObject
	 */
	public String getExecCurIdObject() {
		return execCurIdObject;
	}

	/**
	 * @param execCurIdObject the execCurIdObject to set
	 */
	public void setExecCurIdObject(String execCurIdObject) {
		this.execCurIdObject = execCurIdObject;
	}

	/**
	 * @return the execMsAvg
	 */
	public int getExecMsAvg() {
		return execMsAvg;
	}

	/**
	 * @param execMsAvg the execMsAvg to set
	 */
	public void setExecMsAvg(int execMsAvg) {
		this.execMsAvg = execMsAvg;
	}

	/**
	 * @return the execMsMax
	 */
	public int getExecMsMax() {
		return execMsMax;
	}

	/**
	 * @param execMsMax the execMsMax to set
	 */
	public void setExecMsMax(int execMsMax) {
		this.execMsMax = execMsMax;
	}

	/**
	 * @return the execMsMin
	 */
	public int getExecMsMin() {
		return execMsMin;
	}

	/**
	 * @param execMsMin the execMsMin to set
	 */
	public void setExecMsMin(int execMsMin) {
		this.execMsMin = execMsMin;
	}

	/**
	 * @return the execMsMaxIdObject
	 */
	public String getExecMsMaxIdObject() {
		return execMsMaxIdObject;
	}

	/**
	 * @param execMsMaxIdObject the execMsMaxIdObject to set
	 */
	public void setExecMsMaxIdObject(String execMsMaxIdObject) {
		this.execMsMaxIdObject = execMsMaxIdObject;
	}

	/**
	 * @return the execMsMinIdObject
	 */
	public String getExecMsMinIdObject() {
		return execMsMinIdObject;
	}

	/**
	 * @param execMsMinIdObject the execMsMinIdObject to set
	 */
	public void setExecMsMinIdObject(String execMsMinIdObject) {
		this.execMsMinIdObject = execMsMinIdObject;
	}

	/**
	 * @return the execMsCurExpectedEnd
	 */
	public int getExecMsCurExpectedEnd() {
		return execMsCurExpectedEnd;
	}

	/**
	 * @param execMsCurExpectedEnd the execMsCurExpectedEnd to set
	 */
	public void setExecMsCurExpectedEnd(int execMsCurExpectedEnd) {
		this.execMsCurExpectedEnd = execMsCurExpectedEnd;
	}

	/**
	 * @return the execMsAllExpectedEnd
	 */
	public int getExecMsAllExpectedEnd() {
		return execMsAllExpectedEnd;
	}

	/**
	 * @param execMsAllExpectedEnd the execMsAllExpectedEnd to set
	 */
	public void setExecMsAllExpectedEnd(int execMsAllExpectedEnd) {
		this.execMsAllExpectedEnd = execMsAllExpectedEnd;
	}

	/**
	 * @return the execMsAllStart
	 */
	public long getExecMsAllStart() {
		return execMsAllStart;
	}

	/**
	 * @param execMsAllStart the execMsAllStart to set
	 */
	public void setExecMsAllStart(long execMsAllStart) {
		this.execMsAllStart = execMsAllStart;
	}

	/**
	 * @return the execMsCurStart
	 */
	public long getExecMsCurStart() {
		return execMsCurStart;
	}

	/**
	 * @param execMsCurStart the execMsCurStart to set
	 */
	public void setExecMsCurStart(long execMsCurStart) {
		this.execMsCurStart = execMsCurStart;
	}

	/**
	 * @return the execMsAllEnd
	 */
	public long getExecMsAllEnd() {
		return execMsAllEnd;
	}

	/**
	 * @param execMsAllEnd the execMsAllEnd to set
	 */
	public void setExecMsAllEnd(long execMsAllEnd) {
		this.execMsAllEnd = execMsAllEnd;
	}

	/**
	 * @return the execMsAllElapsed
	 */
	public long getExecMsAllElapsed() {
		return execMsAllElapsed;
	}

	/**
	 * @param execMsAllElapsed the execMsAllElapsed to set
	 */
	public void setExecMsAllElapsed(long execMsAllElapsed) {
		this.execMsAllElapsed = execMsAllElapsed;
	}

	/**
	 * @return the execCntObjectProcessedError
	 */
	public int getExecCntObjectProcessedError() {
		return execCntObjectProcessedError;
	}

	/**
	 * @param execCntObjectProcessedError the execCntObjectProcessedError to set
	 */
	public void setExecCntObjectProcessedError(int execCntObjectProcessedError) {
		this.execCntObjectProcessedError = execCntObjectProcessedError;
	}

	/**
	 * @return the execCntObjectProcessedExcp
	 */
	public int getExecCntObjectProcessedExcp() {
		return execCntObjectProcessedExcp;
	}

	/**
	 * @param execCntObjectProcessedExcp the execCntObjectProcessedExcp to set
	 */
	public void setExecCntObjectProcessedExcp(int execCntObjectProcessedExcp) {
		this.execCntObjectProcessedExcp = execCntObjectProcessedExcp;
	}

	/**
	 * @return the execMsCurEnd
	 */
	public long getExecMsCurEnd() {
		return execMsCurEnd;
	}

	/**
	 * @param execMsCurEnd the execMsCurEnd to set
	 */
	public void setExecMsCurEnd(long execMsCurEnd) {
		this.execMsCurEnd = execMsCurEnd;
	}

	/**
	 * @return the execMsCurElapsed
	 */
	public long getExecMsCurElapsed() {
		return execMsCurElapsed;
	}

	/**
	 * @param execMsCurElapsed the execMsCurElapsed to set
	 */
	public void setExecMsCurElapsed(long execMsCurElapsed) {
		this.execMsCurElapsed = execMsCurElapsed;
	}

	/**
	 * @return the execProcessStatus
	 */
	public EnumProcessStatus getExecProcessStatus() {
		return execProcessStatus;
	}

	/**
	 * @param execProcessStatus the execProcessStatus to set
	 */
	public void setExecProcessStatus(EnumProcessStatus execProcessStatus) {
		this.execProcessStatus = execProcessStatus;
	}

	/**
	 * True when the Stop to the process has been required by Analyzer
	 * 
	 * @return the isStopRequired
	 */
	public boolean getExecStopRequired() {
		return execStopRequired;
	}

	/**
	 * @param isStopRequired the isStopRequired to set
	 */
	public void setExecStopRequired(boolean isStopRequired) {
		this.execStopRequired = isStopRequired;
	}
	
	
	/**
	 * @return the excpOccurred
	 */
	public boolean isExcpOccurred() {
		return excpOccurred;
	}

	/**
	 * @param excpOccurred the excpOccurred to set
	 */
	public void setExcpOccurred(boolean excpOccurred) {
		this.excpOccurred = excpOccurred;
	}

	/**
	 * @return the execExcp
	 */
	public Exception getExecExcp() {
		return execExcp;
	}

	/**
	 * @param execExcp the execExcp to set
	 */
	public void setExecExcp(Exception execExcp) {
		this.execExcp = execExcp;
	}

	/**
	 * @return the execCntObjectProcessedNoError
	 */
	public int getExecCntObjectProcessedNoError() {
		return execCntObjectProcessedNoError;
	}

	/**
	 * @param execCntObjectProcessedNoError the execCntObjectProcessedNoError to set
	 */
	public void setExecCntObjectProcessedNoError(int execCntObjectProcessedNoError) {
		this.execCntObjectProcessedNoError = execCntObjectProcessedNoError;
	}

	/**
	 * @return the isProcessRunning
	 */
	public boolean getExecProcessRunning() {
		return execProcessRunning;
	}

	/**
	 * @param isProcessRunning the isProcessRunning to set
	 */
	public void setExecProcessRunning(boolean isProcessRunning) {
		this.execProcessRunning = isProcessRunning;
	}

	
	
}
   