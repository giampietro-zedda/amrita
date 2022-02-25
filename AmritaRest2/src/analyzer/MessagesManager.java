package analyzer;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import enums.EnumMessageType;
import enums.EnumModule;
/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda Turin (ITALY)
  *
  * <h1>
  * MessagesManager
  * </h1>
  * <p>
  * Questa classe gestisce tutti i messaggi internazionalizzati attraverso la classe ResourceBundle.
  * Tali messaggi includono quelli di log e quelli di tutti i moduli che compongono e-Amrita e
  * sono caricati in fase di inizializzazione del sistema.<br>
  * I messaggi sono caricati in files di properties specifici per modulo e tipologia, per gestire
  * automaticamente l'internazionalizzazione tramite i ResouresBoundle.
  * Le tipologie dei messaggi rispeccchiano le tipologie di Log4J, con una specializzazione
  * per i messaggi di errore.<br>
  * Sono disponibili metodi per recuperare i messaggi, che vengono restituiti già
  * nella lingua corrispondente al locale attivo.
  * I messaggi sono individuati da una chiave alfanumerica che parte da "0001", per
  * ogni tipologia di messaggio.
  * 
  *
  *
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 2/11/2009
  * @see Message
  *
*/

public class MessagesManager implements java.io.Serializable{
	
	private static final long serialVersionUID = 1L;


	
	
	/////////////////////////////////////////////////////////////////////////////////////
    //// Variabili di istanza                                                          //
    /////////////////////////////////////////////////////////////////////////////////////
	
	// Dichiarazione Locale
	@SuppressWarnings("unused")
	private UserConfiguration systemDefaults;
	
	// Resources Boundle Analyszer per ogni tipologia di messaggio
/*	private ResourceBundle rbMessageAnalyzer_Warning = null; 			
	private ResourceBundle rbMessageAnalyzer_Information = null; 			
	private ResourceBundle rbMessageAnalyzer_Debug = null; 			
	private ResourceBundle rbMessageAnalyzer_ErrorDatabase = null; 			
	private ResourceBundle rbMessageAnalyzer_ErrorInternal = null; 			
	private ResourceBundle rbMessageAnalyzer_ErrorInput = null; 
*/		
	private transient ResourceBundle rbMessageAnalyzer = null; 
    private Locale locale;
	/**
	 * Costruttore  
	 */
	public MessagesManager(UserConfiguration ucfg) {
		this.systemDefaults = ucfg;

	
        // Analyzer: Costruzione Boundles 
		try {
/*			rbMessageAnalyzer_Warning = ResourceBundle.getBundle("AnalyzerMsgWarning", systemDefaults.getCurrentLocale());			
			rbMessageAnalyzer_Information = ResourceBundle.getBundle("AnalyzerMsgInformation", systemDefaults.getCurrentLocale());						
			rbMessageAnalyzer_Debug = ResourceBundle.getBundle("AnalyzerMsgDebug", systemDefaults.getCurrentLocale());					
			rbMessageAnalyzer_ErrorDatabase = ResourceBundle.getBundle("AnalyzerMsgErrorDatabase", systemDefaults.getCurrentLocale());					
			rbMessageAnalyzer_ErrorInternal = ResourceBundle.getBundle("AnalyzerMsgErrorInternal", systemDefaults.getCurrentLocale());						
			rbMessageAnalyzer_ErrorInput = ResourceBundle.getBundle("AnalyzerMsgErrorInput", systemDefaults.getCurrentLocale());} 
*/
			locale = ucfg.getCurrentLocale();
			rbMessageAnalyzer = ResourceBundle.getBundle("resourcesBundle.AnalyzerMsg", ucfg.getCurrentLocale());} 
		catch (MissingResourceException e) 
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}					
		catch (Exception e) 
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}					
	
	}
	

	/**
	 * Restituzione stringa messaggio internazionalizzato senza attualizzazione parametri
	*/
	public String getMessage(
						     EnumModule module, 
							 EnumMessageType messageType, 
							 String keyMessage
						    ) {
		
		String textMessageOutput = null;
		
		switch (module) {
				case ANALYZER:
					switch (messageType) {
							case WARNING:
							//	textMessageOutput = rbMessageAnalyzer_Warning.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case INFORMATION:
							//	textMessageOutput = rbMessageAnalyzer_Information.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case DEBUG:
							//	textMessageOutput = rbMessageAnalyzer_Debug.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case ERROR_DATABASE:
							//	textMessageOutput = rbMessageAnalyzer_ErrorDatabase.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case ERROR_INPUT:
							//	textMessageOutput = rbMessageAnalyzer_ErrorInput.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case ERROR_INTERNAL:
							//	textMessageOutput = rbMessageAnalyzer_ErrorInternal.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case ERROR_FATAL:
								//	textMessageOutput = rbMessageAnalyzer_ErrorFatal.getString(keyMessage);
									textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
									break;
							default:
								textMessageOutput = "**** Internal MessageManager error ****";
								break;
			        }
					break;
			    // TODO
				case VIEWER:
					break;
				case FORWARD:
					switch (messageType) {
							case WARNING:
							//	textMessageOutput = rbMessageAnalyzer_Warning.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case INFORMATION:
							//	textMessageOutput = rbMessageAnalyzer_Information.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case DEBUG:
							//	textMessageOutput = rbMessageAnalyzer_Debug.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case ERROR_DATABASE:
							//	textMessageOutput = rbMessageAnalyzer_ErrorDatabase.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case ERROR_INPUT:
							//	textMessageOutput = rbMessageAnalyzer_ErrorInput.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case ERROR_INTERNAL:
							//	textMessageOutput = rbMessageAnalyzer_ErrorInternal.getString(keyMessage);
								textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
								break;
							case ERROR_FATAL:
								//	textMessageOutput = rbMessageAnalyzer_ErrorFatal.getString(keyMessage);
									textMessageOutput = rbMessageAnalyzer.getString(keyMessage);
									break;
							default:
								textMessageOutput = "**** Internal MessageManager error ****";
								break;
			        }
					break;
				case INSPECTOR:
					break;
				case METRICS:
					break;
				default:
					break;
		}
		return textMessageOutput;
	}

	/**
	 * Restituzione stringa messaggio internazionalizzato con attualizzazione parametri
	*/
	public String getMessageActualized(
									  EnumModule module 
							 		 ,EnumMessageType messageType
							 		 ,String keyMessage
							 		 ,String ... arParm
						    		 ) {
		
		String textMessageOutput = getMessage(module, messageType, keyMessage);
		String parmTotReplace = "";
		
		// Solo se sono parametri da attualizzare
		if (arParm != null) {
			// Scan parametri da sostituire
			for (int i = 0; i < arParm.length; i++) {
				parmTotReplace = "#" + i + "#";
				textMessageOutput = textMessageOutput.replace(parmTotReplace, arParm[i] );
			}
		}
		return textMessageOutput;
	}
}
	
	
	
