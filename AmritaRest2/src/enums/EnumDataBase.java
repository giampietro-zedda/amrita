package enums;
/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumDataBase  
 * </h1>
 *  <p>
 * Questa enum elenca i possibili tipi di database gestiti.
 * Vengono memorizzate eventuali informazioni specifiche di ogni database.
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/03/2010
 * @see DataBaseManager
 * @see DataBaseFacade
 *  
*/
public enum EnumDataBase {

	 ///////////////////////////////////////////////////////////////////////////
     // Database gestiti                        							  //
	 ///////////////////////////////////////////////////////////////////////////
	 
	 NOT_ASSIGNED, 				 	   // (00) Di servizio   
	
	 DB_MS_ACCESS,   				   // (01)  			  
	 DB_SQLSERVER,    			       // (02) 
	 DB_MYSQL,    			           // (03) 
	 DB_POSTGRE,    			       // (04) 
	 DB_ORACLE;    			       	   // (05) 
}
