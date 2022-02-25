package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumExpressionItem
  * </h1>
  *  <p>
  * Questa enum i possibili operandi e operatori di una espressione/Condizione per il
  * linguaggio Cobol e Sql.<br>
  * 
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 12/03/2010
  * @see Analyzer
  * @see EnumExpressionItemSql
*/
public enum EnumExpressionItem {
	
	NOT_ASSIGNED,        				// 00 Di servizio 
	
	//////////////////////////////////////////////////////////////////////////////
	// Cobol Expression item
	//////////////////////////////////////////////////////////////////////////////
	
	// Operatori algebrici
	COBOL_OPRT_ALGEBRIC_ADD,           		// 01 Operatore +
	COBOL_OPRT_ALGEBRIC_SUBTRACT,      		// 02 Operatore -
	COBOL_OPRT_ALGEBRIC_MULTIPLY,      		// 03 Operatore *
	COBOL_OPRT_ALGEBRIC_DIVIDE,        		// 04 Operatore /
	COBOL_OPRT_ALGEBRIC_POWER,         		// 05 Operatore **

	// Operatori Logici
	COBOL_OPRT_LOGIC_AND,           		// 06 AND
	COBOL_OPRT_LOGIC_OR,           			// 07 OR
	COBOL_OPRT_LOGIC_NOT,          			// 08 NOT

	// Operatori Di confronto
	COBOL_OPRT_COMPARE_GREATER,      		// 09 >
	COBOL_OPRT_COMPARE_LESS,         		// 10 <
	COBOL_OPRT_COMPARE_EQUAL,        		// 11 =
	COBOL_OPRT_COMPARE_LESS_EQUAL,   		// 12 <= (LESS OR EQUAL)
	COBOL_OPRT_COMPARE_GREATER_EQUAL,		// 13 >= (GREATER OR EQUAL)
	COBOL_OPRT_COMPARE_NOT_EQUAL,    		// 14 <>
	COBOL_OPRT_COMPARE_NOT_LESS,     		// 15 NOT <
	COBOL_OPRT_COMPARE_NOT_GREATER,  		// 16 NOT >
	COBOL_OPRT_COMPARE_NOT_LESS_EQUAL,    	// 17 NOT <= (NOT LESS OR EQUAL)
	COBOL_OPRT_COMPARE_NOT_GREATER_EQUAL, 	// 18 NOT >= (NOT GREATER OR EQUAL)

	// Operatori Di Classe e di Segno
	COBOL_OPRT_VALUE_NUMERIC,      			// 19 IS NUMERIC
	COBOL_OPRT_VALUE_ALPHABETIC,   			// 20 IS ALPHABETIC
	COBOL_OPRT_VALUE_ALPHABETIC_LOWER,  	// 21 IS ALPHABETIC-LOWER
	COBOL_OPRT_VALUE_ALPHABETIC_UPPER,  	// 22 IS ALPHABETIC-UPPER
	COBOL_OPRT_VALUE_POSITIVE,     			// 23 IS POSITIVE
	COBOL_OPRT_VALUE_NEGATIVE,     			// 24 IS NEGATIVE
	COBOL_OPRT_VALUE_ZERO,         			// 25 IS ZERO/ZEROS/ZEROES
	COBOL_OPRT_VALUE_NOT_NUMERIC,  			// 26 IS NOT NUMERIC
	COBOL_OPRT_VALUE_NOT_ALPHABETIC,    	// 27 IS NOT ALPHABETIC
	COBOL_OPRT_VALUE_NOT_ALPHABETIC_LOWER, 	// 28 IS NOT ALPHABETIC-LOWER
	COBOL_OPRT_VALUE_NOT_ALPHABETIC_UPPER, 	// 29 IS NOT ALPHABETIC-UPPER
	COBOL_OPRT_VALUE_NOT_POSITIVE, 			// 30 IS NOT POSITIVE
	COBOL_OPRT_VALUE_NOT_NEGATIVE, 			// 31 IS NOT NEGATIVE
	COBOL_OPRT_VALUE_NOT_ZERO,     			// 32 IS NOT ZERO/ZEROS/ZEROES

	// Parentesi in espressione originale
	COBOL_BRACKET_OPEN,     				// 33 (
	COBOL_BRACKET_CLOSE,    				// 34 )
	COBOL_END_EXPRESSION,          			// 35 Fine descrittore polacca inversa
	
	//////////////////////////////////////////////////////////////////////////////
	// Sql Expression item
	//////////////////////////////////////////////////////////////////////////////
	// Operatori SQL (SELECT .......    )
	SQL_OPRT_SQL_SQL_EXISTS,   				// 36 SQL EXISTS
	SQL_OPRT_SQL_SQL_NOT_EXISTS,       		// 37 Cls  SQL NOT EXISTS
	SQL_OPRT_SQL_SQL_IN,       				// 38 SQL IN
	SQL_OPRT_SQL_SQL_BETWEEN,  				// 39 SQL BETWEEN

	// Parentesi in espressione originale
	SQL_BRACKET_OPEN,     					// 40 (
	SQL_BRACKET_CLOSE,    					// 41 )
	SQL_END_EXPRESSION;          			// 42 Fine descrittore polacca inversa


}
