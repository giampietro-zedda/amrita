package enums;

import analyzer.DataBaseMappedEnumeration;
/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumThresholds	
  * </h1>
  *  <p>
  * Questa enum elenca i valori di soglia o di range previsti in fase di analisi, metriche ed elaborazioni.<br>
  * Questi valori possono essere aggiornati all'occorrenza dai paramentri di esecuzione o in altro modo.<br>
  * Ogni elaborazione fa riferimento a questa enumerazione.
  * <p>
  * Per ogni entry sono definiti due valori di soglia, uno minimo e uno massimo.<br>
  * Si intende che la misura è accettabile se il range di valori rispetta l'indicazione successiva di BT, GT, LT.<br>
  * Se i due valori sono uguali la misura, per essere accettabile, deve essere o maggiore o minore.<br>
  * Casi tipici i commenti, che devono essere maggiori di un certo numero o il numero delle section
  * di programma, che devono essere minori di un certo numero.<br>
  * <p>
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 20/09/2011
  * @see EnumMetricsViolation
  *  
*/
@DataBaseMappedEnumeration
public enum EnumThresholds {
	
	NOT_ASSIGNED(), 														// 000 Di servizio    
	
	COB_PGM_SMALL(1, 200, "BT"),											// 001 Programma piccolo in istruzioni
	COB_PGM_MEDIUM(201, 1000, "BT"),										// 002 Programma medio in istruzioni
	COB_PGM_LARGE(1001, 4000, "BT"),										// 003 Programma grande in istruzioni
	COB_PGM_HUGE(4001, 100000, "BT"),										// 004 Programma enorme in istruzioni
	COB_SIZE_FIELD_NAME(3, 25, "BT"),									    // 005 Lunghezza nome campo
	COB_SIZE_LABEL(3, 20, "BT"),											// 006 Lunghezza nome label
	COB_SIZE_PARAGRAPH_NAME(3, 25, "BT"),									// 007 Lunghezza nome paragrafo
	COB_SIZE_SECTION_NAME(3, 25, "BT"),										// 008 Lunghezza nome section
	COB_NESTING_IF(3, 3, "LT"),												// 009 Livello di annidamento if
	COB_SIZE_SOURCE_ROWS(1500, 1500, "LT"),									// 010 Numero righe sorgente
    COB_SIZE_INSTR(1000, 1000, "LT"),								        // 011 Numero istruzioni di procedure
	COB_SIZE_SECTION_INSTR(40, 40, "LT"),								    // 012 Numero istruzioni in section
	COB_SIZE_PARAGRAPH_INSTR(40, 40, "LT"),									// 013 Numero istruzioni in paragrafo
	COB_SIZE_PERFORM_INNER_INSTR(25, 25, "LT"),								// 014 Numero istruzioni in inner perform
	COB_SIZE_BLOCK_DUPLICATED(15, 15, "LT"),								// 015 Numero istruzioni duplicate minime per blocco
	COB_SIZE_BLOCK_COMMENTED(15, 15, "LT"),								    // 016 Numero istruzioni commentate minime per blocco
	COB_CONDITIONS(5, 5, "LT"),												// 017 Numero condizioni in if o evaluate when
   	COB_SECTIONS(30, 30, "LT"),												// 018 Numero section definite
  	COB_PARAGRAPHS(30, 30, "LT"),										    // 019 Numero paragrafi definiti
	COB_COMM_PGM(5, 5, "GT"),												// 020 Numero righe commento di inizio programma
	COB_COMM_SECTION(5, 5, "GT"),										    // 021 Numero righe commento di inizio section
	COB_COMM_PARAGRAPH(5, 5, "GT"),											// 022 Numero righe commento di inizio paragrafo
	COB_PERC_COMM_PROC_BY_ROW(20, 20, "GT"),								// 023 Percentuale commenti istruzioni procedure per riga sorgente
	COB_PERC_COMM_PROC_BY_INSTR(25, 25, "GT"),								// 024 Percentuale commenti istruzioni procedure per istruzione
	COB_PERC_COMM_SECTION_BY_ROW(20, 20, "GT"),								// 025 Percentuale commenti section per riga sorgente
	COB_PERC_COMM_SECTION_BY_INSTR(15, 15, "GT"),							// 026 Percentuale commenti section procedure per istruzione
	COB_PERC_COMM_PARAGRAPH_BY_ROW(10, 10, "GT"),							// 027 Percentuale commenti paragrafo per riga sorgente
	COB_PERC_COMM_PARAGRAPH_BY_INSTR(15, 15, "GT"),							// 028 Percentuale commenti paragrafo per istruzione
	COB_PERC_COMM_DATA_BY_ROW(10, 10, "GT"),								// 029 Percentuale commenti data division per riga sorgente
	COB_PERC_COMM_DATA_BY_INSTR(15, 15, "GT"),								// 030 Percentuale commenti data division per istruzione
	COB_MCBE_SECTION_PARAGRAPH(20, 20, "LT"),								// 031 Indice di McCabe section/paragrafo 
	COB_MCBE_TOT_PGM(400, 400, "LT"),										// 032 Indice di McCabe totale  di programma (somma di tutte le section/paragrafi)
	SQL_TABLES_ACCESSED(1, 5, "LT"),										// 033 Numero tabelle accedute in sql select
	SQL_WHERE_CONDITIONS(10, 10, "LT"),								    	// 034 Numero di condizioni singole in Where
	SQL_TABLES_JOINED(5, 5, "LT"),										    // 035 Numero tabelle accedute in sql in Join a quella principale
	SQL_SELECT_NESTED(3, 3, "LT"),										    // 036 Numero select annidate (subselect)
	FAN_IN(20, 20, "LT"),													// 037 Numero programmi chiamanti
	FAN_OUT(20, 20, "LT");													// 038 Numero programmi chiamati
	
	
	String condGood = "";                                   // LT, GT, BT (between)
    int thresholdLow = 0;									// Valore di soglia inferiore
    int thresholdHigh = 0;									// Valore di soglia superiore
	
    
	// Costruttore vuoto
	private EnumThresholds() {
	}
	
	// Costruttore con soglie
	private EnumThresholds(int thresholdLow, int thresholdHigh, String condGood) {
		this.thresholdLow = thresholdLow;
		this.thresholdHigh = thresholdHigh;
		this.condGood = condGood;
	}

	/**
	 * Restituisce la condizione di accettazione.<br>
	 * LT se misura minore valori di range (uguali)<br>
	 * GT se misura maggiore valori di range (uguali)<br>
	 * BT se misura nei valori di range (diversi)<br>
	 * <p>
	 * @return the condGood
	 */
	public String getCondGood() {
		return condGood;
	}

	/**
	 * Imposta la condizione di accettazione.<br>
	 * LT se misura minore valori di range (uguali)<br>
	 * GT se misura maggiore valori di range (uguali)<br>
	 * BT se misura nei valori di range (diversi)<br>
	 * <p>
	 * @param condGood the condGood to set
	 */
	public void setCondGood(String condGood) {
		this.condGood = condGood;
	}

	/**
	 * Restituisce il valore della soglia inferiore.<br>
	 * <p>
	 * @return the thresholdLow
	 */
	public int getThresholdLow() {
		return thresholdLow;
	}

	/**
	 * Imposta il valore della soglia inferiore.<br>
	 * <p>
	 * @param thresholdLow the thresholdLow to set
	 */
	public void setThresholdLow(int thresholdLow) {
		this.thresholdLow = thresholdLow;
	}

	/**
	 * Restituisce il valore della soglia superiore.<br>
	 * <p>
	 * @return the thresholdHigh
	 */
	public int getThresholdHigh() {
		return thresholdHigh;
	}

	/**
	 * Imposta il valore della soglia superiore.<br>
	 * <p>
	 * @param thresholdHigh the thresholdHigh to set
	 */
	public void setThresholdHigh(int thresholdHigh) {
		this.thresholdHigh = thresholdHigh;
	}
	
	
	
}