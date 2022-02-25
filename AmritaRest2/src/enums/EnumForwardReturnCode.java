package enums;

import forward.ForwardFunction;

/**
  * Copyright (c) 2010-2012 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardReturnCode
  * </h1>
  *  <p>
  * They are all classified general return codes <br>
  * a livello di dichiarazione della funzione applicativa.<br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/03/2012
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
public enum EnumForwardReturnCode {
	
	RC_OK,       								// 000 OK
	RC_ERROR_LDV,                               // 001 Errore in accesso ai dati logical data view
	RC_ERROR_FUNCTION_NEW_INSTANCE,             // 002 Errore in new instance di funzione
}
