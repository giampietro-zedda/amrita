package enums;

/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumCobolUsage
 * </h1>
 *  <p>
 * Questa enum elenca i possibili valori della clausola Usage dei un data item Cobol. <br>
 * E' inoltre disponibile il metodo <b>getLengthBytes(EnumCobolUsage en_CobolUsage, int numDigits)</b> , che
 * fornisce lo spazio in bytes allocato per il tipo di campo in memoria.</br> 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 12/04/2010
 * @see Analyzer
 * @see InstructionCobol
 * @see DateItemCobol
*/
public enum EnumCobolUsage {
	
	NOT_ASSIGNED, 				 	   // (00) Di servizio   
	
	USAGE_DISPLAY,                     // (01) Alfanumerico X o zoned
	USAGE_DISPLAY_1,                   // (02) Alfanumerico X o zoned
	USAGE_INDEX(4),                    // (03) Numerico indice 4 bytes length
	USAGE_POINTER(4),                  // (04) Numerico Puntatore 4 bytes length
	USAGE_FUNCTION_POINTER(4),		   // (05) Numerico puntatore 4 bytes length
	USAGE_PROCEDURE_POINTER(8),		   // (06) Numerico puntatore 8 bytes length
	USAGE_BINARY,                      // (07) Numerico binario 2/4/8 bytes length se numero digits <= 4/9/18
	USAGE_COMPUTATIONAL,               // (08) Equivalente a bynary
	USAGE_COMP,                        // (09) Equivalente a bynary
	USAGE_COMPUTATIONAL_1(4),          // (10) Numerico floating point singola precisione 4 bytes long
	USAGE_COMP_1(4),                   // (11) Numerico floating point singola precisione 4 bytes long
	USAGE_COMPUTATIONAL_2(8),          // (12) Numerico floating point doppia precisione 8 bytes long
	USAGE_COMP_2(8),                   // (13) Numerico floating point doppia precisione 8 bytes long
	USAGE_COMPUTATIONAL_3,             // (14) Numerico Packed decimal
	USAGE_COMP_3,                      // (15) Numerico packed decimal
	USAGE_COMPUTATIONAL_4,             // (16) Equivalente a bynary
	USAGE_COMP_4,                      // (17) Equivalente a bynary
	USAGE_COMPUTATIONAL_5,             // (18) Numerico binario eventuale little endian
	USAGE_COMP_5,                      // (19) Numerico binario eventuale little endian
	USAGE_COMP_X,                      // (20) Cobol MF indica o numerico zoned o alfanumerico X
	USAGE_PACKED_DECIMAL;              // (21) Numerico packed decimal

	
	/////////////////////////////////////////////////////////////////////////////////////
	 //  Campi istanza enumerazione                                                     //
	 /////////////////////////////////////////////////////////////////////////////////////

	EnumCobolUsage en_alias = null;
	int lengthBytes = 0;
	 
	 /**
	  * Costruttore per enumerazioni senza informazioni
	 */
	 EnumCobolUsage() {
	 }               			
	
	 /**
	  * Costruttore con lunghezza in bytes
	 */
	 EnumCobolUsage(int lengthBytes) {
		 this.lengthBytes = lengthBytes;
	 }               			
     /**
      * 
      * Restituisce la lunghezza del tipo di campo o zero, se non è direttamente attribuibile.
      * 
      * @return
      */
	 public int getLengthBytes(){
		return this.lengthBytes;
	 }
	 
	 /**
	  * 
	  * Restituisce la lunghezza in bytes allocata, fornito il tipo di enumerazione (usage) e il numero di digits.
	  * 
	  * 
	  * @param numDigits
	  * @return
	  */
	 public int getLengthBytes(int numDigits){
		int lengthBytes = 0;
		 
		lengthBytes = this.getLengthBytes();
		 
		// Se la lunghezza del tipo di dato è staticamente definita la restituisco
		if (lengthBytes > 0) {
			return lengthBytes;
		}
		
		// Campo binario
		if (this == EnumCobolUsage.USAGE_BINARY
		||  this == EnumCobolUsage.USAGE_BINARY
		||  this == EnumCobolUsage.USAGE_COMP
		||  this == EnumCobolUsage.USAGE_COMPUTATIONAL
		||  this == EnumCobolUsage.USAGE_COMP_4
		||  this == EnumCobolUsage.USAGE_COMPUTATIONAL_4   
		||  this == EnumCobolUsage.USAGE_COMP_5
		||  this == EnumCobolUsage.USAGE_COMPUTATIONAL_5  ) {
			if (numDigits <= 4) {
				return 2;
			}
			if (numDigits <= 9) {
				return 4;
			}
			if (numDigits <= 18) {
				return 8;
			}
		}

		// Campo packed decimal
		if (this == EnumCobolUsage.USAGE_PACKED_DECIMAL
		||  this == EnumCobolUsage.USAGE_COMP_3
		||  this == EnumCobolUsage.USAGE_COMPUTATIONAL_3 ) {
			
			// Numero pari di digit
			if ((numDigits % 2) == 0) {
				return numDigits / 2 + 1;
			}
			// Numero dispari di digit
			return (numDigits + 1) / 2;
		}
		return numDigits;
	 }

}
