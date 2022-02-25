package utilities;

import java.io.File;
import java.io.FileInputStream;
import java.io.ObjectInputStream;

import analyzer.AmritaConstants;
import analyzer.CopyCobol;
import analyzer.ExecutionDirectives;
import analyzer.ExecutionShared;
import analyzer.ProgramCobol;
import analyzer.UserConfiguration;
import enums.EnumAmritaExceptionError;
import enums.EnumMessageType;
import exception.ExceptionAmrita;

/**
	* <h1>
	* SystemService  
	* </h1>
	*  <p>
	* Servizi generali di sistema quali serializzazione, deserializzazione etc.  <br>
	* 
	* @author Giampietro Zedda
	* @version 1.0.0
	* @since 2/03/2010
*/

public class SystemService extends ExecutionShared implements AmritaConstants  {
  
	private String sourceObjectContainer = "";			// Nome oggetto (programma) contenente l'oggetto da deseriaslizzare

	/**
	 * Costruttore 
	 * 
	 */
	public SystemService(UserConfiguration sd, ExecutionDirectives di) {
		super(sd, di);
	}
	
	/**
	 * 
	 * Restituisce loggetto serializzato con il path fornito.<br>
	 * <p>
	 * In caso di oggetto (programma o copy) non trovato o errore di deserializzazione, restituisce
	 * l'exception già pronta per essere trattata.
	 * 
	 * 
	 * @param String dirFileSerialized 		Directory file di output 
	 * @param String suffixFile 			Suffisso file di output (SUFFIX_SERIALIZED_COPY, SUFFIX_SERIALIZED_PGM, ..)
	 * @param String idObject 				Nome file di output 
	 * @return Object object 				Oggetto deserializzato di cui effettuare il casting
	 * @throws ExceptionAmrita 
	 */
	public Object getSerialized(String dirFileSerialized         // Recuperata dal chiamante da SystemDefaults
			                  , String idObject                  // Nome Copy, programma, ..
			                  , String suffixFile                // Passata dal chiamante da AmritaConstants
		                  ) throws ExceptionAmrita {
		
		Object objectUnserialized = null;
		ExceptionAmrita excp = null;
		ObjectInputStream ois = null;
		String fileName = null;
		
		fileName = dirFileSerialized + File.separator + idObject + "." + suffixFile;

		try {
			// Open del file
			ois = new ObjectInputStream(new FileInputStream(fileName));
			objectUnserialized = ois.readObject();
			ois.close ();
 		} catch (Exception e) {
 			if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_COPY)) {
				logMessage(EnumMessageType.ERROR_INTERNAL, "EI0009", e, idObject, CopyCobol.class.getName(), fileName);
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_UNSERIALIZE_COPY, e);
			} else {
				logMessage(EnumMessageType.ERROR_INTERNAL, "EI0009", e, idObject, ProgramCobol.class.getName(), fileName);
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_UNSERIALIZE_PGM, e);
			}
			// Il main gestisce l'eccezione, fornisce info supplementari e decide se continuare l'elaborazione
            return excp;
		}

		// De-serializzazione completata: log messaggio informativo
 		if (this.di != null && this.di.optVerboseMessages) {
 			if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_COPY)) {
 				logMessage(EnumMessageType.ERROR_INTERNAL, "MI0033", this.sourceObjectContainer, idObject, CopyCobol.class.getName(), fileName);
 			} else {
 				logMessage(EnumMessageType.ERROR_INTERNAL, "MI0033", this.sourceObjectContainer, idObject, CopyCobol.class.getName(), fileName);
 			}
		}
		return objectUnserialized;
	}

	/**
	 * Restituisce il nome dell'oggetto contenitore (copy o pgm) da visualizzare nel log.
	 * 
	 * @return the sourceObjectContainer
	 */
	public String getSourceObjectContainer() {
		return sourceObjectContainer;
	}

	/**
	 * 
	 * Imposta il nome dell'oggetto contenitore (copy o pgm) da visualizzare nel log.
	 * 
	 * @param sourceObjectContainer the sourceObjectContainer to set
	 */
	public void setSourceObjectContainer(String sourceObjectContainer) {
		this.sourceObjectContainer = sourceObjectContainer;
	}
	


}
