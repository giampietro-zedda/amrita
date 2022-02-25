package enums;

import analyzer.AmritaConstants;
import analyzer.DataBaseMappedEnumeration;
import analyzer.ScriptSql;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1> 
  * EnumPrecompilerOptionsInstruction
  * </h1>
  *  <p>
  * Questa enum viene utilizzata insieme a {@link EnumPrecompilerReservedWords} ed elenca tutte le istruzioni 
  * di un precompilatore che hanno delle opzioni che sono a loro volta delle istruzioni di precompilatore.<br>
  * <p>
  * Si utilizza nell'analisi di {@link ScriptSql} per individuare la fine di un'istruzione valida.<br>
  * Ogni entry di questa enum contiene il riferimento a una istruzione di precomplilatore e a un
  * array con il riferimento a tutte le opzioni di una istruzione che sono a loro volta anche istruzioni valide.<br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 11/07/2011
  * @see AnalyzerSql
  * 
*/
@DataBaseMappedEnumeration
public enum EnumPrecompilerOptionsInstruction implements AmritaConstants{

			
	NOT_ASSIGNED,															// 000 
	
	/////////////////////////////////////////////////////////////
    // Tipologie Istruzioni Sql                                      
    /////////////////////////////////////////////////////////////
	

	// Istruzioni Sql DDL embedded in programmi o codificate in script
	SQL_CREATE_TABLESPACE(EnumPrecompilerReservedWords.SQL_CREATE_TABLESPACE,	new EnumPrecompilerReservedWords[]{EnumPrecompilerReservedWords.SQL_CLOSE} ), 		// 000
	SQL_CREATE_VIEW(EnumPrecompilerReservedWords.SQL_CREATE_VIEW,				new EnumPrecompilerReservedWords[]{EnumPrecompilerReservedWords.SQL_SELECT} ), 		// 000
	SQL_CREATE_INDEX(EnumPrecompilerReservedWords.SQL_CREATE_INDEX,           	new EnumPrecompilerReservedWords[]{EnumPrecompilerReservedWords.SQL_CLOSE} ), 		// 000
	SQL_CREATE_TRIGGER(EnumPrecompilerReservedWords.SQL_CREATE_TRIGGER,       	new EnumPrecompilerReservedWords[]{EnumPrecompilerReservedWords.SQL_UPDATE,
																												   EnumPrecompilerReservedWords.SQL_DELETE, 		// 000
																												   EnumPrecompilerReservedWords.SQL_SELECT, 		// 000
																												   EnumPrecompilerReservedWords.SQL_INSERT}),		// 000
	SQL_CREATE_TABLE(EnumPrecompilerReservedWords.SQL_CREATE_TABLE,           	new EnumPrecompilerReservedWords[]{EnumPrecompilerReservedWords.SQL_UPDATE,
																											 	   EnumPrecompilerReservedWords.SQL_SELECT,
																											 	   EnumPrecompilerReservedWords.SQL_PROCEDURE_SET,
																											 	   EnumPrecompilerReservedWords.SQL_PROCEDURE_BEGIN,
																											 	   EnumPrecompilerReservedWords.SQL_DROP,
																											 	   EnumPrecompilerReservedWords.SQL_PROCEDURE_FOR_LOOP,
																											 	   EnumPrecompilerReservedWords.SQL_PROCEDURE_END}), 		// 000
	SQL_CREATE_SYNONYM(EnumPrecompilerReservedWords.SQL_CREATE_SYNONIM,	 		new EnumPrecompilerReservedWords[]{EnumPrecompilerReservedWords.SQL_PROCEDURE_FOR_LOOP} ), 		// 000
	SQL_CREATE_ALIAS(EnumPrecompilerReservedWords.SQL_CREATE_ALIAS,	 			new EnumPrecompilerReservedWords[]{EnumPrecompilerReservedWords.SQL_PROCEDURE_FOR_LOOP} ), 		// 000
	SQL_PROCEDURE_CASE(EnumPrecompilerReservedWords.SQL_PROCEDURE_CASE,	 		new EnumPrecompilerReservedWords[]{EnumPrecompilerReservedWords.SQL_PROCEDURE_SET} ), 		// 000
	SQL_PROCEDURE_FOR_LOOP(EnumPrecompilerReservedWords.SQL_PROCEDURE_FOR_LOOP,	new EnumPrecompilerReservedWords[]{EnumPrecompilerReservedWords.SQL_PROCEDURE_FOR_LOOP} ); 		// 000
	
		
	//////////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanza per ogni entry
	//////////////////////////////////////////////////////////////////////////////////////
	
	EnumPrecompilerReservedWords typeInstruction = null;
	EnumPrecompilerReservedWords ar_optionInstruction[] = null ; // Entry che sono opzioni per l'istruzione ma identificano specifiche istruzioni
	
	
	/*
     * 
     * Costruttore vuoto
     * 
     */
	EnumPrecompilerOptionsInstruction(){
	}

	
	/*
     * 
     * Costruttore per entries Sql
     * 
     */
	EnumPrecompilerOptionsInstruction(EnumPrecompilerReservedWords typeInstruction, EnumPrecompilerReservedWords ar_optionInstruction[]){
		this.typeInstruction = typeInstruction;
		this.ar_optionInstruction = ar_optionInstruction;
	}
	


	/**
	 * Restituisce un array con le enumerazioni {@link EnumPrecompilerReservedWords} che
	 * sono delle opzioni per l'enumerazione corrente ma rappresentano a loro volta
	 * anche una istruzione valida.<br>
	 * Questa informazione si utilizza nel parsing di ScriptSql a istruzioni quali,<br>
	 * per esempio, <tt>CREATE INDEX <\tt> e <tt>CREATE TSBLESPACE <\tt> che hanno <br>
	 * come opzione <tt>CLOSE <\tt>, che identifica l'istruzione <tt>CLOSE CURSOR<\tt> <br>
	 * <p>
	 * 
	 * @return the ar_optionInstruction
	 */
	public EnumPrecompilerReservedWords[] getOptionsAsInstruction() {
		return ar_optionInstruction;
	}



	/**
	 * Restituisce l'enumerazione relativa all'istruzione codificata
	 * dall'entry<br>
	 * 
	 * @return the typeInstruction
	 */
	public EnumPrecompilerReservedWords getTypeInstruction() {
		return typeInstruction;
	}


	/* (non-Javadoc)
	 * @see java.lang.Enum#toString()
	 */
	@Override
	public String toString() {
		return super.toString() + ( (ar_optionInstruction == null) ? (""): (ar_optionInstruction.toString()));
	}

}
