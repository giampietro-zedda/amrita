package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumMetricsViolationFixUnit	
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie di unità di remediation delle violazionio alle metriche
  * in miniti, ore, giorni, mesi.<br>
  * <p>
   * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/09/2011
  * @see Metrics
  *  
*/
@DataBaseMappedEnumeration
public enum EnumMetricsViolationFixUnit {
	
	NOT_ASSIGNED, 					// 0 Di servizio    
	
	MINUTE,							// 1 Minuti
	HOUR,							// 2 Ore
	DAY,							// 3 Giorni
	MONTH,							// 4 Mesi
}