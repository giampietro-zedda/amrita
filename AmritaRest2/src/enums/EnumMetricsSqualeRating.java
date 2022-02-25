package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumMetricsSqualeRating	
  * </h1>
  *  <p>
  * Questa enum elenca i livelli di rating del modello SQUALE.<br>
  * <p>
  * Si tratta del rapporto fra il costo stimato di remediation e il costo stimato di sviluppo.<br>
  * Sono cinque livelli classificati da A a E, dal verde al rosso.<br>
  * <p>
  * Viene memorizzata la soglia di default per ogni valore:<br>
  * <p>
  * A 0%   - 10%  verde scuro<br>
  * B 10%  - 20%  verde chiaro<br>
  * C 20%  - 50%  giallo<br>
  * D 50%  - 100% Arancione<br>
  * E 100% - >    Rosso<br>
  * <p>
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 02/09/2011
  * @see Metrics
  *  
*/
@DataBaseMappedEnumeration
public enum EnumMetricsSqualeRating {
	
	NOT_ASSIGNED, 					// 00 Di servizio    
	
	SQALE_RATING_A(0,10),			// 01 Verde scuro
	SQALE_RATING_B(10,20),			// 02 Verde chiaro
	SQALE_RATING_C(20,50),			// 03 Giallo
	SQALE_RATING_D(50,100),			// 04 Arancione
	SQALE_RATING_E(100,0);			// 05 Rosso
	
	private int thresholdLow = 0;	
	private int thresholdHigh = 0;
	
	/*
	 * Costruttore vuoto
	 */
	private EnumMetricsSqualeRating() {
	}	
	
	/*
	 * Costruttore
	 */
	private EnumMetricsSqualeRating(int thresholdLow, int thresholdHigh) {
		this.thresholdLow = thresholdLow;
		this.thresholdHigh = thresholdHigh;
	}

	/**
	 * Restituisce la soglia inferiore del livello di rating.<br>
	 * <p>
	 * @return the thresholdLow
	 */
	public int getThresholdLow() {
		return thresholdLow;
	}

	/**
	 * Imposta la soglia inferiore del livello di rating.<br>
	 * <p>
	 * @param thresholdLow the thresholdLow to set
	 */
	public void setThresholdLow(int thresholdLow) {
		this.thresholdLow = thresholdLow;
	}

	/**
	 * Restituisce la soglia superiore del livello di rating.<br>
	 * <p>
	 * @return the thresholdHigh
	 */
	public int getThresholdHigh() {
		return thresholdHigh;
	}

	/**
	 * Imposta la soglia superiore del livello di rating.<br>
	 * <p>
	 * @param thresholdHigh the thresholdHigh to set
	 */
	public void setThresholdHigh(int thresholdHigh) {
		this.thresholdHigh = thresholdHigh;
	}	
	
}