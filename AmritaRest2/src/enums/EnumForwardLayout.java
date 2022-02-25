package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardLayout
  * </h1>
  *  <p>
  * Questa enum elenca i gestori di layout utilizzati da Forward.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardLayout {
	
	NOT_ASSIGNED,           									// 00 Di servizio 
	
	BORDER_LAYOUT,           									// 01 Per disporre pannelli
	CARD_LAYOUT,           										// 02 Per disporre pannelli
	BOX_LAYOUT,           										// 03 Per disporre pannelli e componenti in pannelli
	FLOW_LAYOUT,           										// 04 Per disporre componenti  
	GRID_LAYOUT,           										// 05 Per disporre componenti in pannelli
	GRID_BAG_LAYOUT,           									// 06 Per disporre componenti in pannelli
	PREDEFINED_LAYOUT;           							    // 09 Definito dai pannelli specializzati
}












