package enums;

import analyzer.DataBaseMappedEnumeration;
import forward.ForwardFunction;

/**
  * Copyright (c) 2010-2012 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardUserStatus (60)
  * </h1>
  *  <p>
  * They are all classified forward user status. <br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/03/2012
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumUserStatus {
	
	NOT_ASSIGNED,       						// 000 OK
	USER_ACTIVE,                               	// 001 Utente attivo
	USER_INACTIVE,             					// 002 Utente non attivo
	USER_SUSPENDED,             				// 003 Utente sospeso, temporaneamente non attivo
}
