package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing Giampietro Zedda    Turin (Italy)
  *
  * <h1>
  * EnumForwardBorderType
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie generali di una funzione.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardBorderType {
	
	NOT_ASSIGNED,           									// 00 Di servizio 
	
	EMPTY,           											// 01 
	LINE,           											// 02 
	RAISED_ETCHED,           									// 03 
	LOWERED_ETCHED,           									// 04 
	LOWERED_BEVEL,           									// 04 
	RAISED_BEVEL,           									// 05 
	RAISED_LOWERED,           									// 06 
	COMPOUND,           										// 08   Two bevels
	TITLED_DEFAULT,     										// 09 
	TITLED_LINE,      											// 10
	TITLED_LOWERED_ETCHED,           							// 11 
	TITLED_LOWERED_BEVEL,										// 12
	MATTE,           											// 13 
}












