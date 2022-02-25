package enums;

import analyzer.DataBaseMappedEnumeration;
import forward.ForwardFunction;

/**
  * Copyright (c) 2010-2012 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardUserType (59)
  * </h1>
  *  <p>
  * They are all classified forward user types. <br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/03/2012
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumUserType {
	
	NOT_ASSIGNED,       						// 000 OK
	USER_SUPER,                               	// 001 Fa tutto, incluso reset sistema e altre operazioni
	USER_MASTER,             					// 002 Crea altri user e assegna permission
	USER_NORMAL,             					// 003 User normale  
	USER_GUEST,             					// 003 User associato con diritti specifici assegnati
}
