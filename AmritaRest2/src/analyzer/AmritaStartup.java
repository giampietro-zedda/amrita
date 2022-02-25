package analyzer;
import java.util.Date;
import java.util.Enumeration;
import java.util.Properties;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import dao.DAOFactory;
import dao.MySQLDAOFactory;
import enums.EnumMessageType;
import exception.ExceptionAmrita;
import utilities.AmritaCache;
import utilities.DateTimeService;
import utilities.ReflectionManager;
public class AmritaStartup implements ServletContextListener{
	// test branch
    static final int TIME_TO_LIVE = 200;      // Secondi
    static final int TIMER_INTERVAL = 500;    // Secondi
    static final int MAX_ITEMS = 20;          // 

	// Oggetti di gestione accessibili a tutta l'applicazione
    public static UserConfiguration ucfg = null;                    // Parameters from config.properties
    public static LoggerFacade lf = null;							// Gestore logging messaggi
    public static MessagesManager mm = null;						// Gestore messaggi
    public static ReflectionManager rm = null;					    // Gestore reflection
    public static SourceManager sm = null;					        // Gestore sorgenti
    public static MySQLDAOFactory sqlFactory = null;                // Factory per sql DAO
    
    // Cache
    public static AmritaCache<String, Object> cache = null;         // Gestore Cache con Apache LRUMap
	
	// Pool di connessioni predefinite allo startup sul db iniziale di configurazione (user=amrita)
	static DataBaseConnection[]  arDbConnection = null;    

	public AmritaStartup() {
		super();		
	}
	
	@Override
	public void contextInitialized(ServletContextEvent arg0)  {
		
		/////////// TODO ///////////////////////
		// Set path per log4J
		// System.setProperty("logPath", myPath); // where myPath is either C:\... or D:\... based on your logic
		/////////////////////////////////////////////////////
				
		try {  
			// Get Configuration from config.properties under \WEB-INF\resources
			// This default is tailored for amrita user
 			ucfg = new UserConfiguration();
		} catch (ExceptionAmrita e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}
		
		mm = new MessagesManager(ucfg);
        ucfg.setMessagesManager(mm);
        lf = new LoggerFacade(ucfg); 
        rm = new ReflectionManager();
        ucfg.setLoggerFacade(lf);
        sm = new SourceManager();
        
        // SQL
		sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		arDbConnection = new DataBaseConnection[ucfg.getDataBaseMaxConn()];       
	    DataBaseConnections.createConnectionsPool(ucfg);  // Static method to create all allowed connections
 
	    // Cache manager 
        cache = new AmritaCache<String, Object>(TIME_TO_LIVE, TIMER_INTERVAL, MAX_ITEMS);
	    
        // Initializaion messages
        logStartupMessages();
        
	}

	/*
	 * Log Initial Messages
	 */
	private void logStartupMessages() {
	    // Properties
		Properties systemProperties = null;			// Properties di sistema direttamente da java
		Properties configProperties = null;			// Properties applicative da cartella resources
		@SuppressWarnings("rawtypes")
		Enumeration enumProperties = null;			// Per estrarre le properties
		
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
		
		// File di configurazione di default
		arParm = new String[1];
		arParm[0] = ucfg.getPathConfigFile();
		lf.writeRow(EnumMessageType.INFORMATION, "MI0012", "");
		lf.writeRow(EnumMessageType.INFORMATION, "MI0007", arParm, null);
		
		// Proprietà di configurazione di default
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
		
		// Inizializzazione terminata
		lf.writeRow(EnumMessageType.INFORMATION, "MI0006", null, null);
		lf.writeRow(EnumMessageType.INFORMATION, "MI0012", "");
				
	}

	@Override
	public void contextDestroyed(ServletContextEvent arg0) {
	   // Static Method to release all allowed connections
       DataBaseConnections.releaseAllConnection();   
	}

}