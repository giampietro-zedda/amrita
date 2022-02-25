package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumCobolOperatorPredicate	
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie di elementi componenti una expression Sql.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 19/07/2010
  * @see EnumSymbolType
  *  
*/
public enum EnumSqlExpressionElementType {
	
	NOT_ASSIGNED, 						   		// (00) Di servizio             

	
	// Operatori matematici e di concatenazione
	ADD("+"),                                   // (01) ) +
	SUB("-"),                                   // (02) ) -
	MULT("*"),                                  // (03) ) *
	DIV("/"),                                   // (04) ) /
	CONCAT("||"),                               // (05) ) || o CONCAT
	
	// Separator
	PAR_OPEN("("),                              // (06) (
	PAR_CLOSE(")"),                             // (07) )
	COMMA(","),                                 // (08) ,
	
	// Elementi
	CONSTANT_NUMERIC(),                     	// (09)  
	CONSTANT_ALPHANUMERIC(),                    // (10)  
	COLUMN_NAME(),                     			// (11) 
	HOST_VAR(),                    			    // (12)  
	VARIABILE(),                    			// (13)  
    SPECIAL_REGISTER(),                     	// (14)  
    FUNCTION_INVOCATION(),                      // (15)  
    SCALAR_FULL_SELECT(),                     	// (16)  
    EXPRESSION(),                     			// (17)  
    CASE_EXPRESSION(),                     		// (18)  
    ROW_RANGE_EXPRESSION(),                     // (19)  
    TIMEZONE_EXPRESSION(),                     	// (20)  
    LABELED_DURATION(),                     	// (21)  
    SEQUENCE_REFERENCE(),                     	// (22)  
    CAST_SPECIFICATION(),                     	// (23)  
    XML_CAST_SPECIFICATION(),                   // (24)  
    OLAP_SPECIFICATION(),                    	// (25)  

	// Figurativi
	DEFAULT(),                     	            // (26)  
	NULL();                     	            // (27)  
	
	
    String value = "";
    
	/*
	 * Costruttore vuoto
	 * 
	 */
	private EnumSqlExpressionElementType() {
	}
	
	/*
	 * Costruttore per valore
	 * 
	 */
	private EnumSqlExpressionElementType(String value) {
		this.value = value;
	}
	
	
	
	
}