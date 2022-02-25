package enums;

/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumUnresolvedPgm
  * </h1>
  *  <p>
  * Questa enum elenca i possibili unresolved di simboli nel processo preliminare di
  * analisi di un programma generico programma.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 12/03/2010
  * @see Analyzer
*/
public enum EnumUnresolvedPgm {
	
	NOT_ASSIGNED,			  				// 00 
	
	COBOL_UNRESOLVED_SECTION,    			// 01
	COBOL_UNRESOLVED_LABEL,            		// 02
	COBOL_UNRESOLVED_LIKE,             		// 03
	COBOL_UNRESOLVED_REDEFINES,         	// 04
	COBOL_UNRESOLVED_RENAMES,          		// 05
	COBOL_UNRESOLVED_RENAMES_THRU,       	// 06
	COBOL_UNRESOLVED_OCC_DEPENDING_ON;   	// 07
}
