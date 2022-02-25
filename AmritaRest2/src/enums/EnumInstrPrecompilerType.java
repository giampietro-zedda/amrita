package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumInstrPrecompilerType
  * </h1>
  *  <p>
  * Identifica le tipologie di istruzioni di precompilatore gestite.<br>
   * <p>
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/01/2011
*/
@DataBaseMappedEnumeration
public enum EnumInstrPrecompilerType {
    
    NOT_ASSIGNED,		   															// 000 Non assegnato 																															// 000
	
	// Tipologie operandi/opzioni/.. istruzioni precompilatore Cics
	EXEC_CICS_INSTRUCTION,	    		// L'entry è di una istruzione Exec Cics
	EXEC_CICS_OPERAND,	    			// L'entry è di un operando
	EXEC_CICS_OPTION,	    		    // L'entry è di una opzione
	EXEC_CICS_EXCEPTION,	    		// L'entry è di una exception e indica una label
	
	// Tipologie operandi/opzioni/.. istruzioni precompilatore Sql
	EXEC_SQL_INSTRUCTION_DDL,	    	// L'entry è di una istruzione Exec Sql DDL
	EXEC_SQL_INSTRUCTION_DML,	    	// L'entry è di una istruzione Exec Sql DML
	EXEC_SQL_INSTRUCTION_PROCEDURE,	    // L'entry è di una istruzione di una procedure tipo Call, Case, GoTo, If, Loop,....
	EXEC_SQL_INSTRUCTION_COMMAND,	    // L'entry è di una istruzione command come BIND etc
	EXEC_SQL_OPERAND,	    		    // L'entry è di un operando ovvero una colonna o una host var
	EXEC_SQL_OPTION,	    		    // L'entry è di una opzione 
	EXEC_SQL_SPECIAL_REGISTER,	    	// L'entry è di uno special register
	EXEC_SQL_LABELED_DURATION,			// L'entry è del tipo YEAR, MONTH etc.
	EXEC_SQL_FUNCTION,            		// L'entry è relativo a una function Sql
	EXEC_SQL_PREDICATE,	            	// L'entry è relativo a un predicato SQL e correlati
	EXEC_SQL_OPERATOR,	            	// L'entry è relativo a uno peratore +,-,<>,..
	EXEC_SQL_KEYWORD;	            	// L'entry è relativo a uan generica parola chiave Sql
	
	
	//////////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanza per ogni entry
	//////////////////////////////////////////////////////////////////////////////////////
	
	
	String valueText1 = "";							// Valore testuale per lo statement
	EnumInstrDataCategory instrCategory = null;     // Categoria istruzione jcl
	
	/*
     * Costruttore vuoto
     */
	EnumInstrPrecompilerType(){
	}

	
	/*
     * Costruttore con identificativo istruzione jcl
     */
	EnumInstrPrecompilerType(String  valueText1){
		this.valueText1 = valueText1;		
	}
	/*
     * Costruttore con identificativo istruzione jcl
     */
	EnumInstrPrecompilerType(String  valueText1, EnumInstrDataCategory instrCategory){
		this.valueText1 = valueText1;	
		this.instrCategory = instrCategory;
	}


	/**
	 * Restituisce la stringa identificativa dell'istruzione jcl.<br>
	 * 
	 * @return the valueKey
	 */
	public String getInstrKey() {
		return valueText1;
	}


	/**
	 * Restituisce la categoria dell'istruzione jcl.<br>
	 * 
	 * @return the instrCategory
	 */
	public EnumInstrDataCategory getInstrCategory() {
		return instrCategory;
	}

}
