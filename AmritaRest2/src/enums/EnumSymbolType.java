package enums;

/**
  * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumSymbolType
  * </h1>
  *  <p>
  * Questa enum elenca la tipologia generale delle classi dei simboli di un programma, durante l'analisi del  sorgente.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/02/2010
  * @see Analyzer
  * @see EnumDataItemGeneric
  * @see EnumDataItemType
  * @see EnumDataItemGeneric
  * @see EnumDataItemFigurative
  * @see EnumDataItemType 
  * @see EnumDataItemTypeSql
*/
public enum EnumSymbolType {
	
	NOT_ASSIGNED, 				 			// (00) Di servizio             

	// Definizioni specifiche per Cobol
	COBOL_SYMBOL_DATA_ITEM,      			// (01) Campo in Data Division descritto da EnumLanguageItemInstr... 	(operando in ExpressionCobolElement)
	COBOL_SYMBOL_LITERAL_NUM,    			// (02) Literal in Data/Procedure 										(operando in ExpressionCobolElement)
	COBOL_SYMBOL_LITERAL_ALPHA,  			// (03) Literal in Data/Procedure 										(operando in ExpressionCobolElement)
	COBOL_SYMBOL_FIGURATIVE,     			// (04) Costante Figurativa 											(operando in ExpressionCobolElement)
	COBOL_SYMBOL_LABEL,          			// (05) Label di Procedure division
	COBOL_SYMBOL_PROC_INTERNAL,        		// (06) Section di Procedure division o paragrafo
	COBOL_SYMBOL_SPECIAL_REGISTER,       	// (07) Registro speciale come TALLY, DATE, LENGTH OF                   (operando in ExpressionCobolElement)
	COBOL_SYMBOL_ENVIRONMENT_NAME,       	// (08) Variabile di ambiente                                           (Sysin, Sysipt,... in Accept ...)
	COBOL_SYMBOL_MNEMONIC_NAME,       	    // (09) Nome mnemonico assegnato a variabile di ambiente                (Come C01 IS NEW-PAGE)
	COBOL_SYMBOL_COPY_NAME_DATA,  			// (10  Nome copy di data  
	COBOL_SYMBOL_COPY_NAME_PROC,  			// (11) Nome copy di procedure  
	COBOL_SYMBOL_OPERATOR,       			// (12) Operatore logico AND, OR, NOT, +. -, *, /						(operator in ExpressionCobolElement)
	COBOL_SYMBOL_INTERNAL_FILE,       		// (13) Nome file interno al programma, come Fd/Select Cobol
	COBOL_SYMBOL_EXTERNAL_FILE,             // (14) Nome file esterno al programma, come in Select Cobol

	// Definizioni specifiche per Sql in script o programmi
	SQL_SYMBOL_LABEL,           			// (15) Label  
	SQL_SYMBOL_PROCEDURE,           		// (16) Procedure name  
	SQL_SYMBOL_FUNCTION,           			// (16) Function name  
	SQL_SYMBOL_LITERAL_NUM,           		// (17) Literal numeric  
	SQL_SYMBOL_LITERAL_ALPHA,           	// (18) Literal alphanumeric  
	SQL_SYMBOL_OPERATOR,           			// (19) Operatori matematici +, -, * /, <, >, <>, =
	SQL_SYMBOL_PREDICATE,           		// (20) Predicati BETWEEN, IN, LIKE, EXISTS, NOT ..
	SQL_SYMBOL_HOST_VAR,           			// (21) Host variable  
	SQL_SYMBOL_DEFINED_VAR,           		// (22) Defined variable  
	SQL_SYMBOL_TABLE_NAME,           		// (23) Table name  
	SQL_SYMBOL_COLUMN_NAME,           		// (24) Column name  
	SQL_SYMBOL_SPECIAL_REGISTER,           	// (25) Special register come DATE, SERVER, SQLID 
	SQL_SYMBOL_LABELED_DURATION,           	// (26) labeled Duration come MONTH, DAYS etc
	SQL_SYMBOL_FULL_SELECT,           	    // (27) Full-select dopo EXISTS, IN, ANY etc
	SQL_SYMBOL_SEPARATOR,           	    // (28) ( ) ,
	SQL_SYMBOL_CURSOR_NAME;           	    // (29) Nome cursore
	
	
	
	/* (non-Javadoc)
	 * @see java.lang.Enum#toString()
	 */
	public String toStringShort() {
		String toStringShort = "";
		toStringShort = super.toString();
		if (toStringShort.equals("NOT_ASSIGNED")) {
			return super.toString();
		}
		return toStringShort.substring(13);
	}       		
	
}
