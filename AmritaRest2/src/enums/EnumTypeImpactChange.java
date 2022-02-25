package enums;

/**
  * copyright (c) 2021 e-Amrita - Giampietro Zedda Turin (ITALY)
  * 
  *  Questa enumerazione elenca i possibili linguaggi con la convenzione della classe Locale.
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 20/05/2021
  * 
 */

public enum EnumTypeImpactChange {
	
	NOT_ASSIGNED,        				// 00 Di servizio 
	
    COPY_FIELD_CHANGE,    				// 01	 
    COPY_FIELD_DELETE,     				// 02	 
    COPY_FIELD_NEW,     				// 03	 
	COPY_DELETE,                    	// 04
    TABLE_COLUMN_CHANGE,    			// 05	 
    TABLE_COLUMN_DELETE,     			// 06	 
    TABLE_COLUMN_NEW,     				// 07	 
    TABLE_DELETE,                    	// 08
    MAP_FIELD_CHANGE,    				// 09	 
    MAP_FIELD_DELETE,     				// 10	 
    MAP_FIELD_NEW,     					// 11	 
    MAP_DELETE,                    		// 12
    JCL_JOB_DELETE,                     // 13
    JCL_DDNAME_CHANGE,					// 14
    JCL_DDNAME_DELETE,					// 15
    JCL_PROC_DELETE,					// 16
    JCL_PROC_CHANGE,					// 17
    JCL_INCLUDE_DELETE,					// 18
    JCL_INCLUDE_CHANGE;					// 19
}