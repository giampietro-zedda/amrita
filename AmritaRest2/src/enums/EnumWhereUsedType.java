package enums;

import analyzer.DataBaseMappedEnumeration;

/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumWhereUsedType 
 * </h1>
 *  <p>
 * Questa enum elenca le possibili tipologie di WhereUsed, tipicamente input o output.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 12/03/2010
 * @see Analyzer
 * @see EnumWhereUsedOrigin
*/
@DataBaseMappedEnumeration
public enum EnumWhereUsedType{
	
	NOT_ASSIGNED,			  // 00 
	
	WHERE_USED_INPUT,         // 01 Item solo in input
	WHERE_USED_OUTPUT;        // 02 Item in output

}
