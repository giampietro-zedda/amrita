package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardFunctionModel
  * </h1>
  *  <p>
  * Questa enum elenca i possibili modelli applicativi predefiniti o customdi una funzione Forward.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  */
@DataBaseMappedEnumeration
public enum EnumForwardFunctionModel {
	
	NOT_ASSIGNED,           									// 00 Di servizio 
	
	SINGLE_RECORD,           									// 01 
	MASTER_DETAIL,           									// 02 
	LIST_DRIVEN,           										// 03 
	GRID_DRIVEN,           										// 04 
	TABBED_FORM,           										// 05 
	SEQUENCE_FORM,           									// 06 
	CUSTOM;           											// 07 
}












