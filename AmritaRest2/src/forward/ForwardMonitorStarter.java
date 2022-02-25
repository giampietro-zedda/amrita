package forward;

import java.sql.SQLException;
import java.util.Date;
import java.util.Enumeration;
import java.util.Locale;
import java.util.Properties;
import java.util.Scanner;

import analyzer.AmritaConstants;
import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.ExecutionDirectives;
import analyzer.LoggerFacade;
import analyzer.MessagesManager;
import analyzer.SourceInput;
import analyzer.SourceManager;
import analyzer.UserConfiguration;

import utilities.DateTimeService;
import utilities.ReflectionManager;
import utilities.StringService;

import enums.EnumAmritaExceptionError;
import enums.EnumDataBase;
import enums.EnumDirectivesExecution;
import enums.EnumMessageType;
import enums.EnumModule;
import exception.ExceptionAmrita;

/**
 * copyright (c) 2009-2012 e-Amrita - Giampietro Zedda  Turin (ITALY)
 * 
 * <h1>
 * ForwardMonitorStarter
 * </h1>
 * <p>
 * This is the line command program to start an application using forward monitor.
 * <p>
 * The application is defined by function declaration defined as a Java class and the name .<br>
 * of the application function class is a mandatory input parameter.<br>
 * Command line to execute an application by forward monitor is like to:<br>
 * <p>
 * <b>javax ExecutionMonitorStarter configFile, pathPilotToStart</b>
 * <p>
 * 
 * Monitor execution is driven by system defaults defined by configuration file <code>configDefault.properties</code>
 * in resource directory <code>".../resources"</code>.<br>
 * It's possible specify a specialized configuration, otherwise, if it is an empty string, it will be used the default <br>
 * <code>configDefault.properties</code> in the folder <code>resource</code><br>
 * The <br>pathPilot<br> contains all execution directives, as the function class to start and any property in override.<br>
 * <p>
 * 
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 12/01/2012
 * @see ForwardMonitorDesktop
 * 
*/
@SuppressWarnings("unchecked")
public class ForwardMonitorStarter implements AmritaConstants {

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza di reference a servizi generalizzati e gestori                                //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    
	private UserConfiguration sd = null;          					// Defaults e references globali come gestore messaggi, log etc	
 	private MessagesManager mm = null;         					// Gestore messaggi
    private LoggerFacade lf = null;            					// Gestore logging
    private SourceManager sm = null;                            // gestore generalizzato sources
    private ReflectionManager rm = null;
	
    // Informazioni e opzioni da passare ai gestori di processi e funzioni recuperate dal pilota processi
    // Il pilota processi è analizzato sequenzialmente e le informazioni contenute vengono aggiornate
    // prima di attivare il processo/funzione. 
	// Questo descrittore di esecuzione è di servizio.
	private ExecutionDirectives di = null;
	private SourceInput siPilotExecution = null;				// Container righe file pilot processi 

	// Campi per connessione e accesso ai dati
    private String url = "";							     	// Url di connessione
    private String user = "";								 	// Utente 
    private String pwd = "";								 	// Password
    private String dbname = "";							 		// Database name
    private String driver = "";								 	// driver jdbc
    private EnumDataBase database = null;					 	// Tipologie di database gestite
    private int dbMaxConn = 3;						         	// Numero massimo connessioni
    public DataBaseManager dbm = null;							 // Gestore database
    public DataBaseEntityInterface dbei = null;				 	// Interfaccia per operazioni CRUD su entities			     
		
    // Properties
	private Properties systemProperties = null;					// Properties di sistema direttamente da java
	private Properties configProperties = null;					// Properties applicative da cartella resources
	@SuppressWarnings("rawtypes")
	private Enumeration enumProperties = null;					// Per estrarre le properties
	
	// Oggetti di gestione
	private ForwardMonitorDesktop fm = null;					// Forward monitor
	private ForwardFunction functionClass = null;				// Funzione applicativa da attivare
	private String functionClassName = "";						// Nome funzione applicativa da attivare
	
	// Tempi di esecuzione
	long timeMsStart = 0;										// Inizio elaborazione
	long timeMsEnd = 0;											// Fine elaborazione
	long timeMsTotal = 0;										// Durata in millisecondi
	
	// Di servizio
	String configFile = null;				    				// Config file nella cartella resources (senza .properties finale) 
 	String pilotexecution = null;								// Path completo file pilota processi/funzioni da eseguire
	String msg = null;											// Messaggio loggato
	String[] arParm = null;										// Parametri per errori
	String[] arRowSourcePilot = null;							// Righe sorgente file pilota
	boolean isThereDirectiveoStart = false;

	
	/**
	 * @param args (1) name sources (2) function class name
	 * @throws Exception 
	 * 
	 */
	public  void main(String[] args) throws Exception {

	    // Meno di 2 parametri: lancio exception
		if (args.length != 2) {
			throw new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_TOO_FEW_PARAMETERS, null);
		}
	    // Qualche parametro a null: lancio exception
		for (int i = 0; i < args.length; i++) {
			if (args[i] == null) {
				throw new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_NULL_PARAMETER, null);
			}
		}

		//////////////////////////////////////////////////////////////////////////
        // Gestione impostazione file di configurazione di default o da parametro
		//////////////////////////////////////////////////////////////////////////
		
		if (!args[0].trim().equals("")) {
			configFile = args[0];
			sd = new UserConfiguration(configFile);
			// File di configurazione non trovato
			if (sd == null) {
				ExceptionAmrita excp = new  ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_CONFIG_FILE_NOT_FOUND, null );
				excp.setMsg(configFile);
				throw excp;
			}
		} else {
			sd = new UserConfiguration();
			// File di configurazione di default non trovato
			if (sd == null) {
				ExceptionAmrita excp = new  ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_CONFIG_FILE_NOT_FOUND, null );
				excp.setMsg("resources/ConfigDefault.properties");
				throw excp;
			}
		}

		//////////////////////////////////////////////////////////////////////////
		// Oggetti shared e reference in SystemDefault
		//////////////////////////////////////////////////////////////////////////
		
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        sm = new SourceManager(sd);
        rm = new ReflectionManager();
        sd.setCurModule(EnumModule.FORWARD);
        
		//////////////////////////////////////////////////////////////////////////
        // Gestione pilot esecuzione
		//////////////////////////////////////////////////////////////////////////
        
		if (!args[1].equals("")) {
			pilotexecution = args[1];
		} else {
			pilotexecution = sd.getPilotDefaultSource();
		}
		siPilotExecution = sm.getSource(sd.getDirPilot() + pilotexecution, false, true);
		// File di pilot di esecuzione da eseguire inesistente
		if (siPilotExecution == null) {
			arParm = new String[1];
			arParm[0] = args[1];
			lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0005", arParm, null);
			throw new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_PILOT_PROCESS_FILE_NOT_FOUND, null);
		}

        

		//////////////////////////////////////////////////////////////////////////
        // Scrittura su log informazioni di ambiente e di inizio
		//////////////////////////////////////////////////////////////////////////
        
		// Logo e copyright
		// Data e ora
		String[] arParm = new String[2];  
		arParm[0] = DateTimeService.getDateFormatted(new Date(), "dd-MM-yyyy", sd.getCurrentLocale());
		arParm[1] = DateTimeService.getTimeFormatted(new Date(), "hh:mm:ss", sd.getCurrentLocale());
		lf.writeRow(EnumMessageType.INFORMATION, "MI0001", arParm, null);
		lf.writeRow(EnumMessageType.INFORMATION, "MI0002", null, null);
		
		// Versione, data ultima modifica e motivo
		arParm = new String[3];
		arParm[0] = UserConfiguration.getAmritaActiveVersion();
		arParm[1] = UserConfiguration.getAmritaLastModDate();
		arParm[2] = UserConfiguration.getAmritaLastIssue();
		lf.writeRow(EnumMessageType.INFORMATION, "MI0023", arParm, null);
		
		// Inizializzazione in corso
		lf.writeRow(EnumMessageType.INFORMATION, "MI0003", null, null);
		
		// Proprietà di sistema Java
		lf.writeRow(EnumMessageType.INFORMATION, "MI0012", "");
		lf.writeRow(EnumMessageType.INFORMATION, "MI0004", null, null);
		systemProperties = System.getProperties();
		enumProperties = (Enumeration<Properties>) systemProperties.propertyNames();
		while (enumProperties.hasMoreElements()){
			String key =  enumProperties.nextElement().toString();
			String keyValue =  System.getProperty(key);
			arParm = new String[2];
			arParm[0] = key;
			arParm[1] = keyValue;
			lf.writeRow(EnumMessageType.INFORMATION, "MI0005", arParm, null);
	    }	
		
		// File di configurazione
		arParm = new String[1];
		arParm[0] = sd.getPathConfigFile();
		lf.writeRow(EnumMessageType.INFORMATION, "MI0012", "");
		lf.writeRow(EnumMessageType.INFORMATION, "MI0007", arParm, null);
		// Proprietà di configurazione
		configProperties = sd.getPrConfig();
		enumProperties =configProperties.propertyNames();
		// Scan proprietà di configurazione e invio su log
		while (enumProperties.hasMoreElements()){
			String key =  enumProperties.nextElement().toString();
			String keyValue =  configProperties.getProperty(key);
			arParm = new String[2];
			arParm[0] = key;
			arParm[1] = keyValue;
			lf.writeRow(EnumMessageType.INFORMATION, "MI0005", arParm, null);
	    }	

		// File pilota di esecuzione da eseguire
		arParm = new String[1];
		arParm[0] = siPilotExecution.getPathComplete();
		lf.writeRow(EnumMessageType.INFORMATION, "MI0012", "");
		lf.writeRow(EnumMessageType.INFORMATION, "MI0008", arParm, null);
		// Scan rughe file pilota di esecuzione
		arRowSourcePilot = siPilotExecution.getArrayRowSource();
		for (int i = 0; i < arRowSourcePilot.length; i++) {
			arParm = new String[1];
			arParm[0] = arRowSourcePilot[i];
			lf.writeRow(EnumMessageType.INFORMATION, "MI0011", arParm, null);
		}

		
		// Inizializzazione terminata
		lf.writeRow(EnumMessageType.INFORMATION, "MI0006", null, null);
		lf.writeRow(EnumMessageType.INFORMATION, "MI0012", "");
		
		//////////////////////////////////////////////////////////////////////////
        // Attivazione gestore generalizzato processi e funzioni di Analyzer
		//////////////////////////////////////////////////////////////////////////
		
		timeMsStart = System.currentTimeMillis();
		lf.writeRow(EnumMessageType.INFORMATION, "MI0013", null, null); // Inizio elaborazione processi ...
		
		startMonitor();
		
		timeMsEnd = System.currentTimeMillis();
		timeMsTotal = timeMsEnd - timeMsStart;
		//////////////////////////////////////////////////////////////////////////
        // Scrittura su log informazioni di chiusura e consuntivo
		//////////////////////////////////////////////////////////////////////////
        
		lf.writeRow(EnumMessageType.INFORMATION, "MI0014", null, null); // Fine elaborazione processi ...
		Long timeElapsed = new Long(timeMsTotal);
		arParm = new String[1];
		arParm[0] = timeElapsed.toString();
		lf.writeRow(EnumMessageType.INFORMATION, "MI0015", arParm, null); // Elapsed totale
	}


    /*
     * Validazione parametri di pilot
     * Istanziazione  classe dichiarazione funzione
     * Attivazione monitor
     * 
     */
	private void startMonitor() throws Exception {

		ExceptionAmrita excp = null;									// Exception generata
		
		String functionClassNameComplete = "";							// Nome completo della classe applicativa di gestione della funzione
		String classModel = "" ;                               	 		// Modello completo classe incluso package
        int i = 0;
        int j = 0;
		
		EnumDirectivesExecution en_processOrFunction = null;
		String arRowPilotExecution[] = null;     				    	// Righe sorgente pilota processi/funzioni
		String[] arParm = null;											// Parametri per loggging
		
		arRowPilotExecution = siPilotExecution.getArrayRowSource();   	// Recupero righe direttive pilota di esecuzione
     	
		// Prima istanza direttive di funzione/processo, verrà clonata a ogni nuova funzione o processo
     	di = new ExecutionDirectives(sd);


		// Controllo, intabellamento e classificazione informazioni direttive di processo e funzione 
		for (i = 0; i < arRowPilotExecution.length; i++) {
			
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
				lf.writeRow(EnumMessageType.INFORMATION, "MI0020", arParm, null); 
				continue;
			}
			
			extractInfoDirective(en_processOrFunction, arRowPilotExecution[i]);  // -> this.di
			
		} // End-for
	
		// Valorizzazione locale
		this.di.curLocale = this.sd.getCurrentLocale();
		if (!this.di.curLocaleLanguage.equals("") 
		&&  !this.di.curLocaleCountry.equals("")) {
			this.di.curLocale = new Locale(this.di.curLocaleLanguage, this.di.curLocaleCountry);
		} else if (!this.di.curLocaleLanguage.equals("")) {
			this.di.curLocaleCountry = this.sd.getCountry();
			this.di.curLocale = new Locale(this.di.curLocaleLanguage, this.di.curLocaleCountry);
		} else {
			this.di.curLocaleLanguage = this.sd.getLanguage().getLocalValue();
			this.di.curLocale = new Locale(this.di.curLocaleLanguage, this.di.curLocaleCountry);
		}
		this.functionClassName = this.di.fwdFunctionClassName;
		
		// Inizializzazione base dati   
		dataBaseInitial();			  
		

		// Esecuzione monitor sulla funzione                                               		 
		try {
	     	classModel = this.getClass().getName();
			j = classModel.indexOf(this.getClass().getSimpleName());
			functionClassNameComplete = classModel.substring(0, j) + this.functionClassName;
			lf.writeRow(EnumMessageType.INFORMATION, "MI0201", new String[]{functionClassNameComplete}, null); 		// Start ..
			// Generazione istanza classe di gestione funzione
			Object objFunctionToStart = rm.newInstance(functionClassNameComplete, null, null);
			
			// Gestione errore di istanziazione
			if (objFunctionToStart == null) {
		       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_NEW_INSTANCE, rm.getException());
			    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0101", new String[]{functionClassNameComplete}, excp);
			    return;
			}
            
			// Esecuzione dichiarazione funzione e valorizzazione strutture interne
			this.functionClass = (ForwardFunction) objFunctionToStart;
			this.di.fwdFunctionClass = this.functionClass;
			this.functionClass.runDeclare(di);
			
			// Exception in dichiarazione funzione, errore interno
			if (di.excpOccurred != null) {
				lf.writeRow(EnumMessageType.ERROR_INPUT, "EI0101", new String[]{this.functionClass.getFunctionName(), functionClassNameComplete}, di.excpOccurred);
				return;
			}
			
			// Attivazione monitor con la funzione da startare
			this.fm = new ForwardMonitorDesktop(sd, di, functionClass);
			this.fm.setSd(this.sd);
			this.fm.setDbm(this.dbm);
			this.fm.setDbei(new DataBaseEntityInterface(sd, dbm));
			di.thread = new Thread((Runnable) this.fm, "T01");
			di.thread.start();
			di.thread.join();
			
			// Per evitare la chiusura del thread con il frame di forward in azione.
			// Loop infinito fino a richiesta di chiusura o exception in GUI
			while (!di.isForwardClosing && di.excpOccurred == null) {
				Thread.sleep(300);
			}
			
			// Exception in creazione GUI funzione, errore interno
			if (di.excpOccurred != null) {
				lf.writeRow(EnumMessageType.ERROR_INPUT, "EI0101", new String[]{this.functionClass.getFunctionName(), functionClassNameComplete}, di.excpOccurred);
				return;
			}
			

		} catch (Exception e) {
	       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_FORWARD_STARTING, e);
		    lf.writeRow(EnumMessageType.ERROR_INTERNAL, "ET0102", new String[]{functionClassNameComplete}, excp);
		    return;
		}
		
		// Il controllo torna a questo punto anche se nella funzione applicativa si era verificata una eccezione gestita, oppura non prevista
		// L'oggetto DirectivesInfo di è qui disponibile ed è stato eventualmente aggiornato con l'exception avvenuta.
		if (di.excpOccurred != null) {
			lf.writeRow(EnumMessageType.INFORMATION, "MI0203", new String[]{this.functionClass.getFunctionName(), functionClassNameComplete}, di.excpOccurred);   // End con exception
			return;
		}  

		lf.writeRow(EnumMessageType.INFORMATION, "MI0202", new String[]{this.functionClass.getFunctionName(), functionClassNameComplete}, null);  // End ..
		return;
	}
	
	
	

	/*
	 *  
	 * Viene istanziato, se non lo è già, il gestore centralizzato e generalizzato del database 
	 * {@link DataBaseManager}, il cui reference viene memorizzato nei default di sistema.<br>
	 * L'istanziazione provoca l'inizializzazione del database, con la corretta stringa di connessione,
	 * e la creazione di un pool di connessioni predefinito, disponibile per tutti i processi/funzioni.
	 * Il pool viene creato del numero di entrate specificate in configurazione.<br>
	 * Successivamente viene istanziato il gestore delle entities gestite come CRUD {@link DataBaseEntityInterface}.
	 *
	 * @throws ExceptionAmrita 
	 * @throws SQLException 
	 * 	
     */
	private void dataBaseInitial() throws ExceptionAmrita, SQLException {

  		// Parametri di connessione database di default
    	this.url = this.sd.getDataBaseUrl();
    	this.user = this.sd.getDataBaseUser();
    	this.pwd = this.sd.getDataBasePwd();
    	this.dbname = this.sd.getDataBaseName();
    	this.driver = this.sd.getDataBaseDriver(); 
    	this.database = this.sd.getDataBaseType();
        this.dbMaxConn = this.sd.getDataBaseMaxConn();

        /*  OBSOLETO
		// Connessione al db e inizializzazione interfacce
		if (this.sd.getDataBaseManager() == null) {
			dbm = new DataBaseManager(sd, database, driver, url, dbname, user, pwd, dbMaxConn);
			this.sd.setDataBaseManager(dbm);
		}
		*/
	}
	
	/*
	 * Restituisce la direttiva di processo codificata nella riga sorgente
	 * 
	 */
	private EnumDirectivesExecution detectProcessOrFunctionType(String rowPilot)  {
		
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
		return edpMatchOutput;
	}
 
	/*
	 * 
	 * Estrazione informazioni di filtro, selezione oggetti e altro
	 * e aggiornamento variabili di istanza.
	 * 
	 */
	private void extractInfoDirective(EnumDirectivesExecution enDirectiveExecution, String rowPilot) {
		
		StringService ss = null;        			// Per gestore stringhe
		String[] arParm = null;		    			// Parametri per loggging

		
		switch (enDirectiveExecution){


		/////////////////////////////////////////////////////////////////////////////////////////////////
		// Direttive di impostazione sistema/sottosistema di appartenenza funzione                     
		/////////////////////////////////////////////////////////////////////////////////////////////////

		case SYSTEM_INPUT:
			ss = extractValueFromDirective(EnumDirectivesExecution.SYSTEM_INPUT, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			di.en_activeObjectSystem = EnumDirectivesExecution.SYSTEM_INPUT;
			di.subSystemInput = ss._word(1);
			return;
			
		case SUB_SYSTEM_INPUT:
			ss = extractValueFromDirective(EnumDirectivesExecution.SUB_SYSTEM_INPUT, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			di.en_activeObjectSubSystem = EnumDirectivesExecution.SUB_SYSTEM_INPUT;
			di.subSystemInput = ss._word(1);
			return;


			/////////////////////////////////////////////////////////////////////////////////////////////////
			// Direttive di impostazione sistema/sottosistema da trattare o di appartenenza oggetto/sorgente                           
			/////////////////////////////////////////////////////////////////////////////////////////////////

			case CUSTOMER_CODE:
				ss = extractValueFromDirective(EnumDirectivesExecution.CUSTOMER_CODE, rowPilot, new String[]{PARM_TEXT});
				// Se direttiva errata exit
				if (ss == null) {
					arParm = new String[1];
					arParm[0] = rowPilot.trim();
					lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
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
					lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
					return;
				}
				// Carico valori globali
				di.curCustomerInfo = ss._word(1);
				return;

			/////////////////////////////////////////////////////////////////////////////////////////////////
			// Direttive di impostazione sistema/sottosistema da trattare o di appartenenza oggetto/sorgente                           
			/////////////////////////////////////////////////////////////////////////////////////////////////

			case LOCALE_LANGUAGE:
				ss = extractValueFromDirective(EnumDirectivesExecution.LOCALE_LANGUAGE, rowPilot, new String[]{PARM_TEXT});
				// Se direttiva errata exit
				if (ss == null) {
					arParm = new String[1];
					arParm[0] = rowPilot.trim();
					lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
					return;
				}
				// Carico valori globali
				di.curLocaleLanguage = ss._word(1);
				return;

			case LOCALE_COUNTRY:
				ss = extractValueFromDirective(EnumDirectivesExecution.LOCALE_COUNTRY, rowPilot, new String[]{PARM_TEXT});
				// Se direttiva errata exit
				if (ss == null) {
					arParm = new String[1];
					arParm[0] = rowPilot.trim();
					lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
					return;
				}
				// Carico valori globali
				di.curLocaleCountry = ss._word(1);
				return;

			
		///////////////////////////////////////////////////////////////////////////
		// Direttive funzione applicative                                   		
		///////////////////////////////////////////////////////////////////////////

		case FWD_FUNCTION_START:
			ss = extractValueFromDirective(EnumDirectivesExecution.FWD_FUNCTION_START, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			// Carico valori globali
			di.fwdFunctionClassName = ss._word(1);
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
			

		///////////////////////////////////////////////////////////////////////////
		// Direttive gestione database                                    		 //
		///////////////////////////////////////////////////////////////////////////

		// Nome data base
		case DATABASE_NAME:  
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_NAME, rowPilot, new String[]{PARM_TEXT});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			this.dbname = ss._word(1);
			return;
		// numero massimo connessioni permesse
		case DATABASE_MAX_CONN: 
			ss = extractValueFromDirective(EnumDirectivesExecution.DATABASE_MAX_CONN, rowPilot, new String[]{PARM_NUMERIC});
			// Se direttiva errata exit
			if (ss == null) {
				arParm = new String[1];
				arParm[0] = rowPilot.trim();
				lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			this.dbMaxConn = Integer.parseInt(ss._word(1));
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
				lf.writeRow(EnumMessageType.INFORMATION, "ET0004", arParm, null); 
				return;
			}
			di.dataBaseCommitBlockUpdates = Integer.parseInt(ss._word(1));
			sd.setDataBaseCommitBlockUpdates(di.dataBaseCommitBlockUpdates);
			return;



		///////////////////////////////////////////////////////////////////////////
		// Direttiva di esecuzione processi/funzioni dichiarati precedentemente
		///////////////////////////////////////////////////////////////////////////

		case START:
			isThereDirectiveoStart = true;     		// Abilitazione all'esecuzione
			return;
		}

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
			     lf.writeRow(EnumMessageType.ERROR_INTERNAL, "ET0004", strMsg, null);
			     return null;
		     }
		}
		return ss;
	}


}
