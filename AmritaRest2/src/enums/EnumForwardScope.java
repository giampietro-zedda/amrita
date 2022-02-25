package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (Italy)
  *
  * <h1>
  * EnumForwardScope
  * </h1>
  *  <p>
  * This enum lists the scope of a vriable in the runtime forward monitor.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardScope {
	
	NOT_ASSIGNED,           	// 00 Di servizio 
	
	SCOPE_SERVER,           	// 01 Visibility for all functions running on server
	SCOPE_SESSION,          	// 02 Visibility for the function and any function started, for all session
	SCOPE_FUNCTION,           	// 03 Visibility for the current function from any panel declared and any logic executed from 
	SCOPE_REQUEST,           	// 04 Visibility for the current panel and any logic executed from
}












