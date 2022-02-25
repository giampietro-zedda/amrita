package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumSqlOperator	
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
public enum EnumCobolOperator {
	
	NOT_ASSIGNED, 						   		// (00) Di servizio             

	LOGIC_AND("AND"),                           // (01) AND
	LOGIC_OR("OR"),                             // (02) OR
	LOGIC_NOT("NOT"),                           // (03) NOT
	LOGIC_EQUAL("="),                       	// (04) EQUAL
	LOGIC_UNEQUAL("<>"),                       	// (05) UNEQUAL
	LOGIC_GREATER_EQUAL(">="),                  // (06) GREATER OR EQUAL
	LOGIC_GREATER(">"),                         // (07) GREATER("x"), >
	LOGIC_LESS("<"),                            // (08) LESS("x"), <
	LOGIC_LESS_EQUAL("<="),                     // (09) LESS OR EQUAL
	LOGIC_ANY("ANY"),                           // (10) ANY in condizione When Evaluate
	LOGIC_THRU("THRU"),                         // (11) THRU/THROUGH in condizione When Evaluate
	THEN("THEN"),                               // (12) THEN
	ALL("ALL"),                                 // (13) ALL in Move ALL o in IF campo = ALL .. o in WHEN
	IS("IS"),                                   // (14) IS
	OF("OF"),                                   // (15) OF
	IN("IN"),                                   // (16) IN
	PAR_OPEN("("),                              // (17) (
	PAR_CLOSE(")"),                             // (18) )
	ADD("+"),                                   // (19) ) +
	SUB("-"),                                   // (20) ) -
	MULT("*"),                                  // (21) ) *
	DIV("/"),                                   // (22) ) /
	POWER("/"),                                 // (23) ) **
	REF_MOD_SEPARATOR(":"),                     // (24) ) :
	COMMA_SEPARATOR(",");                       // (25) ) ,
	
	
	String value = "";
    
	/*
	 * Costruttore vuoto
	 * 
	 */
	private EnumCobolOperator() {
	}
	
	/*
	 * Costruttore per valore
	 * 
	 */
	private EnumCobolOperator(String value) {
		this.value = value;
	}
	
	
	
	
}