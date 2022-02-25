package enums;
/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumInstrStatus
  * </h1>
  *  <p>
  * Questa enum elenca gli stati di trattamento di ogni istruzione
  * individuata nel processo di analisi.
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/02/2010
  * @see Analyzer
  * @see EnumPrecompilerReservedWords
  * @see EnumLanguageInstrItem
  * 
*/

public enum EnumInstrStatus {
	
	NOT_ASSIGNED,        		// 00 Di servizio 
	
	INSTR_NOT_TO_MANAGE,     	// 01 Istruzione da non gestire
	INSTR_STATIC_TO_SOLVE,      // 02 Istruzione Statica da risolvere
	INSTR_STATIC_SOLVED,        // 03 Istruzione Statica risolta
	INSTR_DYNAMIC_TO_SOLVE,     // 04 Istruzione Dinamica da risolvere
	INSTR_DYNAMIC_SOLVED        // 05 Istruzione Dinamica risolta
}
