package enums;

import analyzer.DataBaseMappedEnumeration;
/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumLanguageItemInstr
  * </h1>
  *  <p>
  * Questa enum elenca i possibili tipi di linguaggio per data item e istruzioni.
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 12/03/2010
  * @see Analyzer
  * @see EnumDataItemGeneric
  * @see EnumDataItemFigurative
  * @see EnumDataItemLevel
  * @see EnumDataItemTypeSql
  * @see EnumDataItemType
  *    
*/
@DataBaseMappedEnumeration
public enum EnumLanguageItemInstr {

	NOT_ASSIGNED,        		// 00 Di servizio 
	
    ITEM_COBOL,          		// 01 Istruzione o Data item definito in programma Cobol
    ITEM_PL1,           		// 02 Istruzione o Data item definito in programma Pl1
    ITEM_JAVA,           		// 03 Istruzione o Data item definito in programma Java
    ITEM_ASM,            		// 04 Istruzione o Data item definito in programma Asm
    ITEM_CICS,           		// 05 Istruzione o Data item Cics       
    ITEM_SQL,            		// 06 Istruzione o Data item Sql
    ITEM_DL1,            		// 07 Istruzione o Data item Dl1
    ITEM_ADABAS,         		// 08 Istruzione o Data item Adabas                
    STMT_JCL;         		    // 09 Statement jcl              
}
