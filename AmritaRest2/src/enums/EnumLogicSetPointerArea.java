package enums;

/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumLogicSetPointerArea (28)
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie di aree dove è situato il pointer dell'area di Linkage,
  * dove risiede il campo assegnato. In pratica l'ultima assegnazione è avvenuta con
  * un campo di Linkage di un'area indirizzata da un Pointer.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 14/03/2010
  * @see Analyzer
  * @see EnumLogicSetType
  * @see EnumLogicSetMode
*/
public enum EnumLogicSetPointerArea {

	NOT_ASSIGNED,        				// 00 Di servizio 
	
	// Specifici per Cobol MOVE da campo in Linkage indirizzato da Pointer
	POINTER_INSIDE_USING_PARM,       	// 01 Pointer in un parametro di Using 
	POINTER_INSIDE_CICS_DFHCOMMAREA,    // 02 Pointer in un campo di COMMAREA
	POINTER_INSIDE_CICS_TWA,        	// 03 Pointer in un campo di TWA
	POINTER_INSIDE_CICS_CSA;        	// 04 Pointer in un campo di TWA

}
