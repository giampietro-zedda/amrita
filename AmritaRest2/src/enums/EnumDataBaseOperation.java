package enums;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumDataBaseOperation  
 * </h1>
 *  <p>
 * Vengono elencate tutte le operazioni previste dalla classe di gestione generalizzata
 * del database {@link DataBaseManager}.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/03/2010
 * @see DataBaseManager
 * @see DataBaseFacade
 *  
*/
public enum EnumDataBaseOperation {

	 NOT_ASSIGNED,              		// 00 Di servizio            

	 DB_OPEN_SESSION,  					// 01   			  
	 DB_CLOSE_SESSION,     			    // 02
	 DB_GET_CREATE_CONNECTIONS,    	    // 03 Ottiene connessione dal dbms 			  
	 DB_ACQUIRE_CONNECTION,    			// 04 Acquisisce connessione dal pool generato con n DB_GET_CONNECTION			  
	 DB_CLOSE_CONNECTION,     			// 05   
	 DB_CLOSE_CONNECTIONS,   			// 06 
	 DB_BEGIN_TRANSACTION, 				// 07
	 DB_COMMIT, 						// 08
	 DB_ROLLBACK, 						// 09
	 DB_SET_COMMIT_POINT, 				// 10
	 DB_CRUD_CREATE,     			    // 11
	 DB_CRUD_CREATE_BULK,     			// 12
	 DB_CRUD_READ,     			        // 13
	 DB_CRUD_UPDATE,     			    // 14
	 DB_CRUD_DELETE,   					// 15
	 DB_READ_SET_ENTITY,		        // 16 Lettura righe singola entity con where fornita
	 DB_READ_VIEW,     			        // 17 Lettura righe singola/multiple entity con sql select generica fornita
	 DB_PREPARE,  						// 18
	 DB_PREPARED_EXEC, 					// 19
	 DB_CLOSE_RESULTSET,     			// 20
	 DB_GENERIC_SQL,					// 21
	 DB_BATCH_CREATE,					// 22
	 DB_BATCH_CLEAR,					// 23
	 DB_BATCH_EXEC,						// 24
	 DB_TABLE_CREATE, 					// 25
	 DB_TABLE_DROP, 					// 26
	 DB_TABLE_INFO,						// 27
	 DB_TABLE_INFO_COLUMN,				// 28
	 DB_TABLE_INDEX_CREATE,				// 29
	 DB_TABLE_INDEX_DROP,				// 30
	 DB_TABLE_INDEX_INFO_COLUMN,		// 31 
	 DB_SQL_GENERIC;		    		// 32
}
