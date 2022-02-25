package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumMetricsQualityCharacteristics
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie di caratteristiche di qualità.<br>
  * Ogni violazione alle metriche {@link EnumMetricsViolation} viene associata a una caratteristica di qualità<br>
  * Le caratteristiche di qualità possono essere aggregate a vari livelli per fornire indici di qualità<br>
  * da punti di vista differenti.<br>
  * Le caratteristiche sono strutturate su due livelli e implementano la metodologia SQALE.
  * <p>
   * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/09/2011
  * @see Metrics
  *  
*/
@DataBaseMappedEnumeration
public enum EnumMetricsQualityCharacteristics {
	
	NOT_ASSIGNED, 																						// 00 Di servizio   
	
	///////////////////////////////////////////////////////////////////////
	// Fattori di qualita generali comuni ai vari modelli (SQALE, CAST,..)
	// OBSOLETO
	///////////////////////////////////////////////////////////////////////
	
	FREE_1, 					    // 01 SQALE
	FREE_2,						    // 02 SQALE
	FREE_3,      					// 03 SQALE
	FREE_4,    						// 04 SQALE
	FREE_5, 						// 05 SQALE
	FREE_6,   						// 06 SQALE
	FREE_7,   						// 07 SQALE
		
	////////////////////////////////////////////////////////////////////////////////////
	// Caratteristiche di qualità misurabili.
	// Categorie regole di cui si verificano le violazioni.
	////////////////////////////////////////////////////////////////////////////////////
	
	COMPILER(EnumMetricsQualityFactors.PORTABILITY),								// 08  SQALE Compiler related
	HARDWARE(EnumMetricsQualityFactors.PORTABILITY),								// 09  SQALE Hardware related
	SOFTWARE(EnumMetricsQualityFactors.PORTABILITY),								// 10  SQALE Software related
	LANGUAGE(EnumMetricsQualityFactors.PORTABILITY),								// 11  SQALE Language related
	OPERATING_SYSTEM(EnumMetricsQualityFactors.PORTABILITY),						// 12  SQALE Operating system related
	TIME_ZONE(EnumMetricsQualityFactors.PORTABILITY),								// 13  SQALE Time zone related
	READABILTY(EnumMetricsQualityFactors.MAINTENABILITY),							// 14  SQALE Readibility
	UNDERSTANDABILITY(EnumMetricsQualityFactors.MAINTENABILITY),					// 15  SQALE Understandability
	API_ABUSE(EnumMetricsQualityFactors.SECURITY),								// 16  SQALE API abuse
	ERRORS(EnumMetricsQualityFactors.SECURITY),									// 17  SQALE Errors
	INPUT_VALIDATION(EnumMetricsQualityFactors.SECURITY),							// 18  SQALE Input validation and rappresentation
	FEATURES(EnumMetricsQualityFactors.SECURITY),									// 19  SQALE Security features
	MEMORY_USE(EnumMetricsQualityFactors.EFFICIENCY),								// 20  SQALE Memory use
	PROCESSOR_USE(EnumMetricsQualityFactors.EFFICIENCY),							// 21  SQALE Memory use
	ARCHITECTURE_CHANGEABILITY(EnumMetricsQualityFactors.CHANGEABILITY),			// 22  SQALE Architecture  
	DATA_CHANGEABILITY(EnumMetricsQualityFactors.CHANGEABILITY),					// 23  SQALE Data
	LOGIC_CHANGEABILITY(EnumMetricsQualityFactors.CHANGEABILITY),					// 24  SQALE Logic related changeability
	ARCHITECTURE_RELIABILITY(EnumMetricsQualityFactors.RELIABILITY),				// 25  SQALE Architecture related reliability
	DATA_RELIABILITY(EnumMetricsQualityFactors.RELIABILITY),						// 26  SQALE Data related reliability
	EXCEPTION_HANDLING(EnumMetricsQualityFactors.RELIABILITY),					// 27  SQALE Exception handling
	FAULT_TOLERANCE(EnumMetricsQualityFactors.RELIABILITY),						// 28  SQALE Fault tolerance
	INSTRUCTION(EnumMetricsQualityFactors.RELIABILITY),							// 29  SQALE Instruction related
	LOGIC_RELIABILITY(EnumMetricsQualityFactors.RELIABILITY),						// 30  SQALE Logic related reliability
	SYNCRONIZATION(EnumMetricsQualityFactors.RELIABILITY),						// 31  SQALE Syncronization related
	UNIT_TESTS(EnumMetricsQualityFactors.RELIABILITY),							// 32  SQALE Unit tests
	INTEGRATION_LEVEL(EnumMetricsQualityFactors.TESTABILITY),						// 33  SQALE Integration level testability
	UNIT_LEVEL(EnumMetricsQualityFactors.TESTABILITY);							// 34  SQALE Unit level

	
	// Variabili di istanza
	EnumMetricsQualityFactors relatedSqaleQualityFactor = null;;
	boolean isSqaleQualityFactor = false;
	boolean isCastHealthFactor = false;
	
	/*
	 * Costruttore vuoto
	 */
	private EnumMetricsQualityCharacteristics() {
	}					
	
	/*
	 * Costruttore vuoto
	 */
	private EnumMetricsQualityCharacteristics(EnumMetricsQualityFactors relatedSqaleQualityFactor, boolean isSqaleQualityFactor, boolean isCastHealthFactor ) {
		this.relatedSqaleQualityFactor = relatedSqaleQualityFactor;
		this.isSqaleQualityFactor = isSqaleQualityFactor;
		this.isCastHealthFactor = isCastHealthFactor;
	}

	/*
	 * Costruttore vuoto
	 */
	private EnumMetricsQualityCharacteristics(EnumMetricsQualityFactors relatedSqaleQualityFactor) {
		this.relatedSqaleQualityFactor = relatedSqaleQualityFactor;
	}


	/**
	 * Restituisce il fattore di qualità di pprimo livello.<br>
	 * <p>
	 * @return the relatedSqaleQualityFactor
	 */
	public EnumMetricsQualityFactors getRelatedSqaleQualityFactor() {
		return relatedSqaleQualityFactor;
	}

	/**
	 * Imposta il fattore di qualità di pprimo livello.<br>
	 * <p>
	 * @param relatedSqaleQualityFactor the relatedSqaleQualityFactor to set
	 */
	public void setRelatedSqaleQualityFactor(
			EnumMetricsQualityFactors relatedSqaleQualityFactor) {
		this.relatedSqaleQualityFactor = relatedSqaleQualityFactor;
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