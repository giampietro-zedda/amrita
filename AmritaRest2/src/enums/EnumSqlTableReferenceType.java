package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumSqlTableReferenceType
  * </h1>
  *  <p>
  * Questa enum elenca i possibili tipi di reference a tabelle, nella clausola <tt>FROM</tt>.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 19/07/2011
   *  
*/
public enum EnumSqlTableReferenceType {
	
	NOT_ASSIGNED, 						   		// (00) Di servizio  
	
    SINGLE_TABLE,                               // (01)
    NESTED_TABLE_EXPRESSION,                    // (01)
    TABLE_FUNCTION_REFERENCE,                   // (02)
    DATA_CHANGE_TABLE_REFERENCE,                // (03)
    JOINED_TABLE,                               // (04)
    TABLE_LOCATOR_REFERENCE,                    // (05)
    XMLTABLE_EXPRESSION,                        // (06)
}