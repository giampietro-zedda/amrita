package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumMapTypeField	
  * </h1>
  *  <p>
  * Tipologie campi mappa video
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
public enum EnumMapTypeField {
	
	NOT_ASSIGNED,       	// 00 Di servizio 
	
	CICS_BMS_HEADER,      	// 01 Campo di testata
	CICS_BMS_FIELD;      	// 02 Campo digitabile
}
