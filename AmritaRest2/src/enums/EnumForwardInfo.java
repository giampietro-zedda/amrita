package enums;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardInfo
  * </h1>
  *  <p>
  * Questa enum elenca le informazioni che possono essere rese disponibili all'applicazione<br>
  * senza scrivere codice aggiuntivo, ma interrogando a livello applicativo il monitor di forward<br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardInfo {
	
	NOT_ASSIGNED,       // 00 Di servizio 
	
	USER_LOGIN, 		// 01  							 
	ACTIVE_FORM, 		// 01  							 
	ACTIVE_PANEL, 		// 01  							 
	ACTIVE_FIELD, 		// 01  							 
}












