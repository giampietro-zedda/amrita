package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumDataItemLevel
  * </h1>
  *  <p>
  * Questa enum elenca le possibili tipologie dei numeri di livello o di gruppo di un data item.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 12/03/2010
  * @see Analyzer
  * @see EnumDataItemGeneric
  * @see EnumDataItemFigurative
  * @see EnumDataItemType 
  * @see EnumDataItemTypeSql
*/
public enum EnumDataItemLevel {
	
	NOT_ASSIGNED,               		// 00 Di servizio 
	
	COBOL_DATA_ITEM_LEVEL_77,           // 01 Campo elementare a livello 77
	COBOL_DATA_ITEM_LEVEL_66,           // 02 Campo elementare a livello 66
	COBOL_DATA_ITEM_LEVEL_88,           // 03 Campo elementare a livello 88 (condizione)
	COBOL_DATA_ITEM_LEVEL_OTHER;        // 04 Campo elementare no livello 77 66 88
}
