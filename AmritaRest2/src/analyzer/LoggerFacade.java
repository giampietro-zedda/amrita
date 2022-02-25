package analyzer;
import org.apache.log4j.*;

import enums.EnumMessageType;

/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda Turin (ITALY)
  * <h1>
  * LoggerFacade
  * </h1>
  * <p>
  * Questa classe funge da interfaccia al sistema di log effettivamente usato. Viene utilizzato il prodotto standard
  * Apache <b>Log4J</b>. Nella diretory dove sono posizionate le classi eseguibili deve essere posto<br>
  * il file di configurazione <b>log4j.properties</b>. Viene utilizzata una sola ategoria, nel linguaggio Log4J, di ROOT. <br>
  * I messaggi di log possono essere dei tipi seguenti e vengono mappati nei tipo standard di Log4J: <br>
  * <p>
  * <Ul>
  * <Li> Information
  * <Li> Warning  
  * <Li> Debug
  * <Li> Internal Error 
  * <Li> Input Error 
  * <Li> Data base Error 
  * <Li> Fatal Error 
  * </Ul>
  * <p>
  * Tutti i messaggi di log sono codificati in lingua e permettono di avere un numero
  * arbitrario di paramentri posizionali, attualizzati al momento dell'inserimento
  * della registrazione di log.<br>
  * <p>
  * I messaggi di log codificati per numero sono memorizzati nella classe {@link MessagesManager}, che
  * viene inizializzata, con input da database, nelle prime operazioni di analisi.
  * In ogni caso @link LoggerMessages contiene tutti i messaggi in lingua italiana.<br>
  * <p>
  * Il metodo di scrittura di una riga richiede il numero di messaggio, il tipo, la lingua, e gli eventuali
  * parametri posizionali da attualizzare.
  * 
  *
  *
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 2/11/2009
  * @see GraphManager
  *
*/

public class LoggerFacade implements java.io.Serializable{
	
	private static final long serialVersionUID = 1L;


	// Categori Log4J
	static Category catLog;  
	
	// Contenitore valori di defaults
    transient private UserConfiguration ucfg = null;
    
    // Gestione messaggi
	transient private MessagesManager messagesManager = null; // Contenitore messaggi per tutti i moduli, di tutti i tipi, in tutte le lingue


	/**
	 * Costruttore di default
	 */
	public LoggerFacade(UserConfiguration ucfg ) {     // Default di tutti i valori e reference di sistema
			           
		super();
		this.ucfg = ucfg;
		this.messagesManager = ucfg.getMessagesManager();
	}

	/**
	 * Impostazione references a systemDefaults e messagesManager persi a fronte della serializzazione diretta o indiretta
	 */
	public void setReferences(UserConfiguration ucfg) {      
		this.ucfg = ucfg;
		this.messagesManager = ucfg.getMessagesManager();
	}

	/**
	 * Apertura log
	 */
	public void open() {
	}

	/**
	 * Chiusura log
	 */
	public void close() {
	}


	/**
	 * Inserimento riga di messaggio CODIFICATO nel log.
	 * Se il parametro exception è valorizzato, viene passato a Log4J, che porta sul log
	 * anche il printStackTrae().
	 * 
	 * @param EnumMessageType messageType
	 * @param String keyMessage
	 * @param String[] arParm
	 * @param Throwable exception
	 * 
	 * 
	 */
	public void writeRow(
			             EnumMessageType messageType            // Debug, Warning, ..
						,String keyMessage                      // Key messaggio per modulo e per tipo messaggio
						,String[] arParm                        // Parametri da attualizzare (null = nessun parametro)
						,Throwable exception                    // Exception (X printStackTrace()
				    	) {
		
        String messageTextToWrite = null;                       // Testo messaggio internazionalizzato
        String module = null;      								// Modulo attivo corrente
        
		// Recupero testo messaggio dal gestore messaggi e aggiungo modulo corrente
        messageTextToWrite = messagesManager.getMessageActualized(ucfg.getCurModule(), messageType, keyMessage, arParm);
        module = ucfg.getCurModule().toString();
        messageTextToWrite = module + " " + keyMessage + ": " + messageTextToWrite;

		// Scrittura effettiva messaggio
		writeRowEffective(messageType, messageTextToWrite, exception);
		
	}
	
	/**
	 * Restituisvìce la riga di messaggio che verrebbe scritta nel log.
	 * 
	 * @param EnumMessageType messageType
	 * @param String keyMessage
	 * @param String[] arParm
	 * 
	 * 
	 */
	public String getMsgActualized(
			             EnumMessageType messageType            // Debug, Warning, ..
						,String keyMessage                      // Key messaggio per modulo e per tipo messaggio
						,String[] arParm                        // Parametri da attualizzare (null = nessun parametro)
				    	) {
		
        String messageTextToWrite = null;                       // Testo messaggio internazionalizzato
        String module = null;      								// Modulo attivo corrente
        
		// Recupero testo messaggio dal gestore messaggi e aggiungo modulo corrente
        messageTextToWrite = messagesManager.getMessageActualized(ucfg.getCurModule(), messageType, keyMessage, arParm);
        module = ucfg.getCurModule().toString();
        messageTextToWrite = module + " " + keyMessage + ": " + messageTextToWrite;

		return messageTextToWrite;
	}
	
	
	
	/**
	 * Inserimento riga di messaggio NON codificata nel log.
	 * Metodo sovraccaricato per utilizzare il testo messaggio fornito esternamente.
	 * Usato per esempio nell'elenco delle proprietà di sistema.
	 * 
	 * @param EnumMessageType messageType
	 * @param String keyMessage
	 * @param String textMessage
	 */
	public void writeRow(
			             EnumMessageType messageType            // Debug, Warning, ..
						,String keyMessage                      // Key messaggio per modulo e per tipo messaggio
						,String textMessage                     // Testo messaggio
				    ) {
		
        String module = null;      			   // Modulo attivo corrente
        String textMessageToWrite = null;      // Testo messaggio completo
        module = ucfg.getCurModule().toString();
        
        // composizine messaggio
        textMessageToWrite = module + " " + keyMessage + ": " + textMessage;

		// Scrittura effettiva messaggio
		writeRowEffective(messageType, textMessageToWrite, null);
		
	}

	
	/**
	 * Inserimento riga di messaggio nel log.
	 * Se il parametro exception è valorizzato, viene passato a Log4J, che porta sul log
	 * anche il printStackTrae().
	 * 
	 */
	@SuppressWarnings("deprecation")
	private void writeRowEffective(
						             EnumMessageType messageType            // Debug, Warning, ..
									,String textMessageToWrite              // Testo messaggio
									,Throwable exception                    // Exception (X printStackTrace()
								  ) {
		
                 
		catLog = Category.getInstance(LoggerFacade.class.getName());  
	 	Logger logger = Logger.getLogger(LoggerFacade.class.getName());

		
		// BasicConfigurator.configure();
		// Il layout della riga di Log è:
		// Modulo gg-mm-aa hh:mm:ss ---------- testo messaggio -----------------
	    switch (messageType) {
				case DEBUG:
					if (exception == null) {
						logger.debug(textMessageToWrite);
					} else {
						logger.debug(textMessageToWrite, exception); 
					}
					break;
				case WARNING:
					if (exception == null) {
						logger.warn(textMessageToWrite);
					} else {
						logger.warn(textMessageToWrite, exception); 
					}
					break;
				case INFORMATION:
					if (exception == null) {
						logger.info(textMessageToWrite);
					} else {
						logger.info(textMessageToWrite, exception); 
					}
					break;
				case ERROR_DATABASE:
					if (exception == null) {
						logger.error(textMessageToWrite);
					} else {
						logger.error(textMessageToWrite, exception); 
					}
					break;
				case ERROR_INTERNAL:
					if (exception == null) {
						logger.error(textMessageToWrite);
					} else {
						logger.error(textMessageToWrite, exception); 
					}
					break;
				case ERROR_INPUT:
					if (exception == null) {
						logger.error(textMessageToWrite);
					} else {
						logger.error(textMessageToWrite, exception); 
					}
					break;
					
				case ERROR_FATAL:
					if (exception == null) {
						logger.fatal(textMessageToWrite);
					} else {
						logger.fatal(textMessageToWrite, exception); 
					}
					break;
					
		        // Non possibile
				default:
					break;
		}
	}

	/**
	 * 
	 * Restituisce l'oggetto MessagesManager caricato dal costruttore
	 * 
	 * @return the messageManager
	 */
	public MessagesManager getMessageManager() {
		return messagesManager;
	}

	
}