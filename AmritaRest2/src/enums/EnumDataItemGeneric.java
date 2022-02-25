package enums;

import analyzer.DataBaseMappedEnumeration;
/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumDataItemGeneric
  * </h1>
  *  <p>
  * Questa enum elenca le possibili tipologie astratte di dato elementare, tipicamente
  * dati numerici, alfanumerici e di condizione e di data.
  * Ogni linguaggio specifico avrà pertanto la propria rappresentazione.
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 12/03/2010
  * @see Analyzer
  * @see EnumObject
   * 
*/
@DataBaseMappedEnumeration
public enum EnumDataItemGeneric {

	NOT_ASSIGNED,               // 00 Di servizio 
	
    DATA_ITEM_NUMERIC,          // 01 Campo numerico per esempio PIC 9 Cobol
    DATA_ITEM_TEXT,             // 02 Campo alfanumerico per esempio PIC X Cobol
    DATA_ITEM_CONDITION,        // 03 Campo di condizione per esempio liv 88 Cobol o campo booleano
    DATA_ITEM_STRUCTURE,        // 04 Campo struttura, per esempio gruppo Cobol
    DATA_ITEM_DATE,             // 05 Campo numerico rappresentante una data
    DATA_ITEM_REGISTER;         // 06 Campo rappresentato da un registro del linguaggio (es. TALLY Cobol etc.)
}



















