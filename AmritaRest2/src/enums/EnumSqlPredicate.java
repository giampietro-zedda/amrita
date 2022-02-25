package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumCobolPredicate	
  * </h1>
  *  <p>
  * Questa enum elenca i possibili tipi di predicati Sql presenti nella clausola <tt>WHERE</tt>.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 03/05/2010
  * @see EnumSymbolType
  *  
*/
public enum EnumSqlPredicate {
	
	NOT_ASSIGNED, 						   	// (00) Di servizio             

	BASIC(),                   				// (01) ...
	QUANTIFIED(),                   		// (02) ...
	BETWEEN(),                   			// (03) BETWEEN val1 AND val2 
	DISTINCT(),                   			// (04) DISTINCT full-select
	EXISTS(),                   			// (05) EXISTS full-select
	IN(),                                   // (16) IN full-select
	LIKE(),                               	// (07) LIKE
	NULL(),                               	// (08) NULL
	XMLEXISTS();                           	// (09) XMLEXISTS ...
}