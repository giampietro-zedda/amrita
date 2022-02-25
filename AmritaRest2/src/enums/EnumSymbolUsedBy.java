package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumSymbolUsedBy	
  * </h1>
  *  <p>
  * Tipologie aree nelle quali il simbolo (dato elementare) è utilizzato
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
public enum EnumSymbolUsedBy {
	
	NOT_ASSIGNED,			  	// 00 
	
	SYMBOL_USED_BY_DB,      	// 01 Field di Ioarea DB  (data base)
	SYMBOL_USED_BY_FD,      	// 02 Field di Ioarea FD  (File definition)
	SYMBOL_USED_BY_TS,      	// 03 Field di Ioarea TS  (Temporary Storage)
	SYMBOL_USED_BY_TD,      	// 04 Field di Ioarea TD  (Transient Data)
	SYMBOL_USED_BY_MAP;      	// 05 Field di Ioarea Map (BMS)
}
