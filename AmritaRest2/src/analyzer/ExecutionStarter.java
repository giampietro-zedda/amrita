package analyzer;

import java.io.File;
import java.util.Date;
import java.util.Enumeration;
import java.util.Properties;

import utilities.DateTimeService;

import enums.EnumAmritaExceptionError;
import enums.EnumMessageType;
import exception.ExceptionAmrita;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda  Turin (ITALY)
 * 
 * <h1>
 * ExecutionStarter
 * </h1>
 * <p>
 * Questa classe rappresenta il programma di partenza per i servizi di
 * analisi richiamati dall'esterno, richiamabile da linea comandi. 
 * Tutti i parametri sono opzionali e se non specificati vengono utilizzati i valori di default.
 * Tuttavia, se non specificati, i parametri devono essre espressi come stringhe vuote "".<br>
 * <p>
 * <b>javax ExecutionStarter configFile, pathPilotProcesses, pathPilotSources</b>
 * <p>
 * 
 * I processi/funzioni di analisi sono governati dai defaults di sistema definiti nel file
 * di configurazione <b>config.properties</b> nella directory di risorce <b>".../resources"</b>.
 * In config.properties sono codificate tutte le directories necessarie per i processi di analisi.
 * E' possibile specificare un file di configurazione specializzato, altrimenti, se non fornito,
 * verrà utilizzato quello di default <b>config.properties</b> nella cartella resources
 * i valori di ottimizzsazione, le informmazioni per accedere al data base. <br>
 * Nel file di configurazione e' codificata anche la directory dove vengono cercati i files pilota 
 * che governano l'esecuzione i processi di analisi, con il parametro <b>DirPilotAndFilter</b> <br>
 * <p>
 * I files pilota contengono informazioni su:
 * 
 * <Ul>
 * <Li> Nomi completi dei sorgenti da analizzare e/o directories in cui cercarli
 * <Li> Nomi processi e/o funzioni da attivare
 * </Ul>

 * <p>
 * Se i path dei files pilota, completi di prefisso, vengono passati come parametri, vengono inoltrati al gestore dei
 * processi {@link ExecutionDispatcher} direttamene, altrimenti vengono letti i valori di default attraverso
 * SystemDefaults.
 * Se forniti i files vengono validati, ovvero si verifica la loro appartenenza alla directory DirPilotAndFilter.
 * 
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 20/02/2010
 * @see Analyzer
 * @see ExecutionDispatcher
 * 
*/
@SuppressWarnings("unchecked")
public class ExecutionStarter {
	
    // Properties
	static Properties systemProperties = null;			// Properties di sistema direttamente da java
	static Properties configProperties = null;			// Properties applicative da cartella resources
	@SuppressWarnings("rawtypes")
	static Enumeration enumProperties = null;			// Per estrarre le properties
	
	// Oggetti di gestione
	static ExecutionDispatcher ed = null;				// Gestore attivazione process e funzioni
	static UserConfiguration ucfg = null;					// Default applicativi da configProperties
	static MessagesManager mm = null;					// Gestore messaggi
	static LoggerFacade lf = null;						// Gestore logging messaggi
	static SourceManager sm = null;						// Gestore lettura/scrittura files sources 
	static SourceInput si = null;
	static SourceInput siPilotExecution = null;			// Container righe file pilot processi 
	
	// Tempi di esecuzione
	static long timeMsStart = 0;						// Inizio elaborazione
	static long timeMsEnd = 0;							// Fine elaborazione
	static long timeMsTotal = 0;						// Durata in millisecondi
	
	// Di servizio
	static String configFile = null;				    // Config file nella cartella resources (senza .properties finale) 
	static String pilotSources = null;					// Path completo file pilota sorgenti con filtri
 	static String pilotexecution = null;					// Path completo file pilota processi/funzioni da eseguire
	static String msg = null;							// Messaggio loggato
	static String[] arParm = null;						// Parametri per errori
	static String[] arRowSourcePilot = null;			// Righe sorgente file pilota
	

	
	/**
	 * @param arg1 Pilot name sources
	 * @param arg2 Pilot name sources filter
	 * @param arg3 Pilot name processes
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
			ucfg = new UserConfiguration(configFile);
			// File di configurazione non trovato
			if (ucfg == null) {
				ExceptionAmrita excp = new  ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_CONFIG_FILE_NOT_FOUND, null );
				excp.setMsg(configFile);
				throw excp;
			}
		} else {
			ucfg = new UserConfiguration();
			// File di configurazione di default non trovato
			if (ucfg == null) {
				ExceptionAmrita excp = new  ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_CONFIG_FILE_NOT_FOUND, null );
				excp.setMsg("resources/ConfigDefault.properties");
				throw excp;
			}
		}

		//////////////////////////////////////////////////////////////////////////
		// Oggetti shared e reference in SystemDefault
		//////////////////////////////////////////////////////////////////////////
		
		mm = new MessagesManager(ucfg);
        lf = new LoggerFacade(ucfg);
        ucfg.setMessagesManager(mm);
        ucfg.setLoggerFacade(lf);
        sm = new SourceManager(ucfg);
        
		//////////////////////////////////////////////////////////////////////////
        // Gestione pilot esecuzione
		//////////////////////////////////////////////////////////////////////////
        
		if (!args[1].equals("")) {
			pilotexecution = args[1];
		} else {
			pilotexecution = ucfg.getPilotDefaultSource();
		}
		siPilotExecution = sm.getSource(ucfg.getDirPilot() + File.separator + pilotexecution, false, true);
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
		Date date = new Date();
		String[] arParm = new String[2];  
		arParm[0] = DateTimeService.getDateFormatted(date, "dd-MM-yyyy", ucfg.getCurrentLocale());
		arParm[1] = DateTimeService.getDateFormatted(date, "hh:mm:ss", ucfg.getCurrentLocale());
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
		arParm[0] = ucfg.getPathConfigFile();
		lf.writeRow(EnumMessageType.INFORMATION, "MI0012", "");
		lf.writeRow(EnumMessageType.INFORMATION, "MI0007", arParm, null);
		// Proprietà di configurazione
		configProperties = ucfg.getPrConfig();
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
		ed = new ExecutionDispatcher(ucfg, siPilotExecution);
		lf.writeRow(EnumMessageType.INFORMATION, "MI0013", null, null); // Inizio elaborazione processi ...
		
		ed.processMain();
		
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
	
}
