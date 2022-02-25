package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2021 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumMetricsQualityFactors
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie di fattori di qualità.<br>
  * Ogni violazione alle metriche {@link EnumMetricsViolation} viene associata a una caratteristica di qualità<br>
  * e a un fattore di qualità<br>
  * Le caratteristiche di qualità possono essere aggregate a vari livelli per fornire indici di qualità<br>
  * dei vari fattori da punti di vista differenti.<br>
   * <p>
   * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/09/2011
  * @see Metrics
  *  
*/
@DataBaseMappedEnumeration
public enum EnumMetricsQualityFactors {
	
	NOT_ASSIGNED, 																						// 00 Di servizio   
	
	///////////////////////////////////////////////////////////////////////
	// Fattori di qualita generali comuni ai vari modelli (SQALE, CAST,..)
	///////////////////////////////////////////////////////////////////////
	
	PORTABILITY(true, false), 							// 01 SQALE
	MAINTENABILITY(true, false),						// 02 SQALE
	SECURITY(true, false),      						// 03 SQALE
	EFFICIENCY(true, false),    						// 04 SQALE
	CHANGEABILITY(true, false), 						// 05 SQALE
	RELIABILITY(true, false),   						// 06 SQALE
	TESTABILITY(true, false);   						// 07 SQALE
	

	// Variabili di istanza
	boolean isSqaleQualityFactor = false;
	boolean isCastHealthFactor = false;
	
	/*
	 * Costruttore vuoto
	 */
	private EnumMetricsQualityFactors() {
	}					
	
	/*
	 * Costruttore vuoto
	 */
	private EnumMetricsQualityFactors(boolean isSqaleQualityFactor, boolean isCastHealthFactor ) {
		this.isSqaleQualityFactor = isSqaleQualityFactor;
		this.isCastHealthFactor = isCastHealthFactor;
	}

	/**
	 * Restituisce se è un fattore di qualità Squale.<br>
	 * <p>
	 * @return the isSqaleQualityFactor
	 */
	public boolean isSqaleQualityFactor() {
		return isSqaleQualityFactor;
	}

	/**
	 * Imposta se è un fattore di qualità Squale.<br>
	 * <p>
	 * @param isSqaleQualityFactor the isSqaleQualityFactor to set
	 */
	public void setSqaleQualityFactor(boolean isSqaleQualityFactor) {
		this.isSqaleQualityFactor = isSqaleQualityFactor;
	}

	/**
	 * Restituisce se è un fattore di salute CAST.<br>
	 * <p>
	 * @return the isCastHealthFactor
	 */
	public boolean isCastHealthFactor() {
		return isCastHealthFactor;
	}

	/**
	 * Imposta se è un fattore di salute CAST.<br>
	 * <p>
	 * @param isCastHealthFactor the isCastHealthFactor to set
	 */
	public void setCastHealthFactor(boolean isCastHealthFactor) {
		this.isCastHealthFactor = isCastHealthFactor;
	}					
	
}