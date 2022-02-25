package enums;

import forward.ForwardFunction;
import forward.ForwardLogicalDataView;

/**
  * Copyright (c) 2010-2012 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardLogicalDataViewControl
  * </h1>
  *  <p>
  * Describes all coded information for a {@link ForwardLogicalDataView}<br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see ForwardLogicalDataView
  */
public enum EnumForwardLogicalDataViewControl {
	
	LDV_OK,    									// 00 The logical data view has been succesfully validated 
	                                            // Errori e fine righe
	LDV_ERROR_NO_PK,  							// 01 The logical data view has the first FOR_ANY() with a related bean class that doesn't define a primary key
	LDV_ERROR_COLUMN_MATCHING,  				// 02 Any column of a FOR_ANY() or FOR_EACH() doesn't match and it's, so impossible to establish a relationship between two entities
	LDV_ERROR_NO_RELATIONSHIP,           		// 03 No previuos entity relationship found
	LDV_ERROR_RULE_TABLE_KEYS_MATCH,     		// 04 No rule table keys match in previously declared entity
	LDV_ERROR_NOTFOUND,                 	 	// 05 Notfound entity in update & delete
	LDV_ERROR_DUPLICATE,                 		// 06 Duplicate entity in insert
	LDV_ERROR_DB_ACCESS,                 		// 07 Error executing sql select or getting data from resultset
	LDV_ERROR_SET_ENTIITY_FIELD,        	 	// 08 Error updating via reflection an entity field for an entity update
	LDV_ERROR_FOR_EACH_RULE_TABLE_NOT_UNIQUE,	// 09 FOR_EACh() of rule table not the unique declaration in the logical data view
	LDV_ERROR_FOR_SQL_VALIDATE,              	// 10 FOR_SQL() multiple o malformate
	LDV_ERROR_ORDER_BY,							// 11 ORDER_BY errata formalmente o colonna inesistente
	LDV_ERROR_VALIDATION_TO_DO,              	// 12 Vista logica ancora da validare
	LDV_ERROR_INTERNAL,         				// 13 Errore interno ldv
	LDV_ERROR_EXCEPTION,         				// 14 Errore interno ldv con exception
	LDV_ERROR_DB_CONNECTION,         			// 15 Errore in acquisizione della connessione, con exception
	LDV_ERROR_DB_COMMIT,       		       	 	// 16 Errore nella commit al db
	LDV_ERROR_DB_ROLLBACK,       		   		// 17 Errore nella commit al db
	LDV_ERROR_NOT_DECLARED,              		// 18 Logical data view NON presente
	LDV_ERROR_INDEX_OUT_OF_BOUND,              	// 19 Richiesta errata
	LDV_EOF,                					// 20 Fine logical data view
	LDV_FOR_ANY, 								// 21 FOR_ANY() of entity descriptor
	LDV_FOR_EACH,								// 22 FOR_EACH() of entity descriptor 
	LDV_FOR_ANY_RULE_TABLE, 					// 23 FOR_ANY() of a rule table descriptor
	LDV_FOR_EACH_RULE_TABLE, 					// 24 FOR_EACH() of a rule table descriptor
	LDV_FOR_SQL,								// 25 FOR_SQL() descriptor of free sql select statement
	LDV_UPDATE,									// 26 ALLOW_UPDATE() of logical data view entity
	LDV_INSERT,									// 27 ALLOW_INSERT() of logical data view entity
	LDV_DELETE 							    	// 28 ALLOW_DELETE() of logical data view entity
}
