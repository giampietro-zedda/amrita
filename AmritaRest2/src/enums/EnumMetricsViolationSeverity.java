package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumMetricsViolationSeverity	
  * </h1>
  *  <p>
  * Questa enum elenca i livelli di severità di  ogni violazione.<br>
  * <p>
   * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 02/09/2011
  * @see Metrics
  *  
*/
@DataBaseMappedEnumeration
public enum EnumMetricsViolationSeverity {
	
	NOT_ASSIGNED, 					// 0 Di servizio    
	
	A_INFO,							// 1 Violazione di livello informativo
	B_MINOR,						// 2 Violazione di livello lieve
	C_MAJOR,						// 3 Violazione di livello grave
	D_CRITICAL,						// 4 Violazione di livello critico
	E_BLOCKING; 					// 5 Violazione di livello bloccante	

	
	private int totRemediationCost = 0;
    
	/* Costruttore vuoto */
	private EnumMetricsViolationSeverity() {
	}

	/**
	 * Restituisce il totale in giorni del remediation cost <br>
	 * della caratteristica di qualità.<br>
	 * <p>
	 * @return the totRemediationCost
	 */
	public int getTotRemediationCost() {
		return totRemediationCost;
	}

	/**
	 * Imposta il totale in giorni del remediation cost <br>
	 * della caratteristica di qualità.<br>
	 * <p>
	 * @param totRemediationCost the totRemediationCost to set
	 */
	public void setTotRemediationCost(int totRemediationCost) {
		this.totRemediationCost = totRemediationCost;
	}
	
	
}