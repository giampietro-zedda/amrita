package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumCobolOperator
  * </h1>
  *  <p>
  * Questa enum elenca i possibili operatori logici e matematici Sql.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 03/05/2010
  * @see EnumSymbolType
  *  
*/
public enum EnumSqlOperator {
	
	NOT_ASSIGNED, 						   		// (00) Di servizio             

	// Operatori utilizzati nei predicati
	AND("AND"),                           		// (01) AND
	OR("OR"),                             		// (02) OR
	NOT("NOT"),                           		// (03) NOT
	
	// Operatori logici di confronto in espressioni 
	EQ("="),                       				// (04) EQUAL
	GE(">="),                  					// (05) GREATER OR EQUAL
	GT(">"),                         			// (06) GREATER("x"), >
	LT("<"),                            		// (07) LESS("x"), <
	LE("<="),                     				// (08) LESS OR EQUAL
	NE("<>"),                      				// (09) NOT EQUAL
	
	// Operatori matematici
	ADD("+"),                                   // (10) ) +
	SUB("-"),                                   // (11) ) -
	MULT("*"),                                  // (12) ) *
	DIV("/"),                                   // (13) ) /
	
	// Separazione 
	PAR_OPEN("("),                              // (14) (
	PAR_CLOSE(")"),                             // (15) )
	COMMA(",");                                 // (16) ,
	
	
	String value = "";
    
	/*
	 * Costruttore vuoto
	 * 
	 */
	private EnumSqlOperator() {
	}
	
	/*
	 * Costruttore per valore
	 * 
	 */
	private EnumSqlOperator(String value) {
		this.value = value;
	}
	
	
	
	
}