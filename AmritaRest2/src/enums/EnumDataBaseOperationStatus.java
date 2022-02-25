package enums;

import analyzer.DataBaseStatusDetailed;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumDataBaseOperationStatus
 * </h1>
 *  <p>
 * Vengono elencati i possibili stati di ritorno di un'operazione su Db.
 * Gli stati hanno caratteristiche generiche e sono proncipalmente utilizzati
 * nell operazioni CRUD. Nel caso di erroi fisici la natura e il tipo
 * viene dettagliato ulteriormente dalla classe {@link DataBaseStatusDetailed}.
 * Gli elementi di questa enum vengono riportati sul log in caso di eccezione.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 26/03/2010
 * @see DataBaseManager
 * @see DataBaseFacade
 * @see EnumDataBaseOperation
*/
public enum EnumDataBaseOperationStatus {

	 NOT_ASSIGNED,                  // 00 Di servizio 
	
	 DB_OPERATION_OK, 			    // 01 Operazione senza errori: Riga trovata in read, aggiornata in update etc    			  
	 DB_NOTFOUND, 					// 02 Riga non trovata a fronte di funzione readCrud o readDelete o readUpdate  			  
	 DB_DUPLICATE, 					// 03 Riga già trovata a fronte di funzione readUpdate   			  
	 DB_ERROR_SQL, 					// 04 Errore sql generico dettagliato ulteriormente			  
	 DB_ERROR_SESSION, 			    // 05 Errore nell'acquisizione della sessione con il database		  
	 DB_ERROR_CONNECTION_OPEN, 	    // 06 Errore nell'acquisizione di una connessione dal database  
	 DB_ERROR_CONNECTION_CLOSE,     // 07 Errore nel rilascio di una connessione
	 DB_ERROR_PREP_STMT__CLOSE,     // 08 Errore nella close della prepared statement
	 DB_ERROR_COMMIT,               // 09 Errore in commit
	 DB_ERROR_MAX_CONN_REACHED; 	// 10 Errore nell'acquisizione di una connessione dal pool, che è tutto busy  
	 
}
