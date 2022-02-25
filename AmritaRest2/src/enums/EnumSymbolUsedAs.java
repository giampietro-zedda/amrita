package enums;

/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumSymbolUsedAs	
  * </h1>
  *  <p>
  * Modalità di utilizzo del symbolo, campo di programma
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/02/2010
  * @see EnumDataItemGeneric
  * @see EnumDataItemType
  * @see EnumDataItemTypeSql
  * @see EnumSymbolType
  *  
*/
public enum EnumSymbolUsedAs {
	
	NOT_ASSIGNED,			  // 00 
	
	USED_AS_DATE,             // 01 Campo gestito come data
	USED_AS_WORK_FIELD,       // 02 Campo gestito come work
	USED_AS_COUNTER,          // 03 Campo gestito come contatore
	USED_AS_INDEX,            // 04 Campo gestito come indice di tabella
	USED_AS_MIXED;            // 05 Campo gestito in modi diversi
}
