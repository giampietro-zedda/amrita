package enums;

import analyzer.DataBaseMappedEnumeration;
/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumMetricsRuleType	
  * </h1>
  *  <p>
  * Questa enum elenca le modalità di applicazione di una regola.<br>
  * <p>
  * La refola può essere applicata in modo lineare, e in questo caso, vengono<br>
  * conteggiate tutte le violazioni alla regola individuate.<br>
  * Oppure la regola può essere applicata in modo fisso e in questo caso tutte<br>
  * le violazioni individuate contano per una sola occorrenza.<br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 02/03/2011
  * @see Metrics
  *  
*/
@DataBaseMappedEnumeration
public enum EnumMetricsRuleType {
	
	NOT_ASSIGNED, 				// (00) Di servizio             
	LINEAR,         			// (01) Ogni violazione alla regola viene conteggiata
	FIXED,        				// (02) Tutte le violazioni alla regola contano come una violazione
}