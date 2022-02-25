package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumInstrUnresolved	
  * </h1>
  *  <p>
  * Questa enum elenca le opzioni delle istruzioni Sql gestite.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 13/03/2010
  * @see Analyzer
  * @see EnumInstrStatus
  * @see EnumLanguageInstrItem
  * @see EnumInstrTypeCics
  * @see EnumPrecompilerReservedWords
*/
public enum EnumInstrUnresolved {
	
	NOT_ASSIGNED,        		// 00 Di servizio 
	
	COBOL_SECTION_A,           // 01
	COBOL_SECTION_THRU_B,      // 02 
	COBOL_LABEL,               // 03 
	COBOL_LIKE,                // 04 
	COBOL_REDEFINES,           // 05 
	COBOL_RENAMES,             // 06 
	COBOL_RENAMES_THRU,        // 07 
	COBOL_OCC_DEPENDING_ON     // 08 
}
