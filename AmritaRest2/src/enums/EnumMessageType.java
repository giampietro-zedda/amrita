/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda Turin (ITALY)
  * 
  *  Questa enumerazione elena tutti i possibili tipi di messaggi che è possibile scrivere. 
  * 
 */
package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda Turin (ITALY)
  * 
  * Questa enumerazione elenca le tipologie di messaggi. 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 12/01/2010
  * @see Message
  * 
 */
@DataBaseMappedEnumeration
public enum EnumMessageType {
	
	NOT_ASSIGNED,       	// 00 Di servizio 
	
    WARNING, 			    // 01 Solo warning, nesssun problema
    INFORMATION, 			// 02 Solo informativo, nessun problema
    DEBUG, 			        // 03 Solo per debug
    TEXT, 			        // 04 Testo generico
    ERROR_DATABASE, 		// 05 Errore su accesso a database, interno a e-Amrita o nell'input trattato: continua
    ERROR_INTERNAL, 		// 06 Errore interno a e-Amrita: continua
    ERROR_INPUT,            // 07 Errore nell'input trattato: continua
    ERROR_FATAL             // 08 Errore non recuperabile: stop
}
