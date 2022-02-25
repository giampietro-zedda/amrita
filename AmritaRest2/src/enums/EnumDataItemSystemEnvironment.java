package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumDataItemSystemEnvironment (21)
  * </h1>
  *  <p>
  * Questa enum elenca i valori dei possibili data item predefiniti dalle varie aree di sistema.
  * Ogni valore è prefissato dall'ambiente specifico (Cics, sql, ... ).<br>
  * Questi data item possono essere referenziati in qualsiasi istruzione cobol  e non sono
  * definiti nelle strutture deol programma.<br>
  * Per esempio nel caso del Cics, data item come EIBTRMID sono definiti nella struttura
  * EIBxxxx inserita dal precompilatore Cics.<br>
  * In generale questi data item rappresentano dei campi, anche se non definiti, <br>
  * e pertanto possono essere seguiti dalla notazione reference modification, di<br>
  * di posizione e lunghezza.<br>
  * Ci sono eccezioni come DFHRESP che si può trovare nei pgm come DFHRESP(NORMAL) o altro<br>
  * e che rappresenta una condizione. In questi casi non viene verificata l'esistenza della<br>
  * parte indexata, attraverso la parametrizzazione dell'enumerazione <tt>indexVerify</tt>
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 14/03/2010
  * @see Analyzer
  * @see EnumInstrTypeCics
  * @see EnumInstrTypeCicsOption
*/
@DataBaseMappedEnumeration
public enum EnumDataItemSystemEnvironment {
	
	NOT_ASSIGNED,           		        							     	  // Di servizio 
	
    // Cics
	CICS_EIBAID(EnumInstrDataCategory.CICS_PRECOMPILER,      "EIBAID",     1, true),    // 01 Aid  
	CICS_EIBATT(EnumInstrDataCategory.CICS_PRECOMPILER,      "EIBATT",     1, true),    // 02 Attention  
	CICS_EIBCALEN(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBCALEN",   2, true),    // 03 lunghezza commarea
	CICS_EIBCOMPL(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBCOMPL",   1, true),    // 04   
	CICS_EIBCONF(EnumInstrDataCategory.CICS_PRECOMPILER,     "EIBCONF",    1, true),    // 05    
	CICS_EIBCPOSN(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBCPOSN",   2, true),    // 06 Colonna  
	CICS_EIBDATE(EnumInstrDataCategory.CICS_PRECOMPILER,     "EIBDATE",    2, true),    // 07 Data  
	CICS_EIBDS(EnumInstrDataCategory.CICS_PRECOMPILER,       "EIBDS",      1, true),    // 08    
	CICS_EIBEOC(EnumInstrDataCategory.CICS_PRECOMPILER,      "EIBEOC",     1, true),    // 09    
	CICS_EIBERR(EnumInstrDataCategory.CICS_PRECOMPILER,      "EIBERR",     2, true),    // 10    
	CICS_EIBERRCD(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBERRCD",   2, true),    // 11    
	CICS_EIBFMH(EnumInstrDataCategory.CICS_PRECOMPILER,      "EIBFMH",     2, true),    // 12    
	CICS_EIBFN(EnumInstrDataCategory.CICS_PRECOMPILER,       "EIBFN",      1, true),    // 13    
	CICS_EIBFREE(EnumInstrDataCategory.CICS_PRECOMPILER,     "EIBFREE",    1, true),    // 14    
	CICS_EIBNODAT(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBNODAT",   1, true),    // 15    
	CICS_EIBRCODE(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBRCODE",   1, true),    // 16    
	CICS_EIBRECV(EnumInstrDataCategory.CICS_PRECOMPILER,     "EIBRECV",    1, true),    // 17    
	CICS_EIBREQID(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBREQID",   1, true),    // 18    
	CICS_EIBRESP(EnumInstrDataCategory.CICS_PRECOMPILER,     "EIBRESP",    1, true),    // 19    
	CICS_EIBRESP2(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBRESP2",   1, true),    // 20    
	CICS_EIBRLDBK(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBRLDBK",   1, true),    // 21    
	CICS_EIBRSRCE(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBRSRCE",   1, true),    // 22    
	CICS_EIBSIG(EnumInstrDataCategory.CICS_PRECOMPILER,      "EIBSIG",     1, true),    // 23    
	CICS_EIBSYNC(EnumInstrDataCategory.CICS_PRECOMPILER,     "EIBSYNC",    1, true),    // 24    
	CICS_EIBSYNRB(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBSYNRB",   1, true),    // 25    
	CICS_EIBTASKN(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBTASKN",   8, true),    // 26    
	CICS_EIBTIME(EnumInstrDataCategory.CICS_PRECOMPILER,     "EIBTIME",    8, true),    // 27 Ora  
	CICS_EIBTRMID(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBTRMID",   4, true),    // 28 Terminale
	CICS_EIBTRNID(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBTRNID",   4, true),    // 29 Transazione
	CICS_EIBLABEL(EnumInstrDataCategory.CICS_PRECOMPILER,    "EIBLABEL",   0, true),    // 30 Eiblabel
	CICS_DFHRESP(EnumInstrDataCategory.CICS_PRECOMPILER,     "DFHRESP",    4, false),   // 31 Response
	CICS_DFHCOMMAREA(EnumInstrDataCategory.CICS_PRECOMPILER, "DFHCOMMAREA",0, true),    // 32 Commarea Cics passata a programmi e transazioni
	CICS_DFHEIBLK(EnumInstrDataCategory.CICS_PRECOMPILER,    "DFHEIBLK",   0, true),    // 33 Exec Interface Block
	CICS_DFHENTER(EnumInstrDataCategory.CICS_PRECOMPILER,    "DFHENTER",   0, true),    // 34
	CICS_DFHCLEAR(EnumInstrDataCategory.CICS_PRECOMPILER,    "DFHCLEAR",   0, true),    // 35
	CICS_DFHPA1(EnumInstrDataCategory.CICS_PRECOMPILER,    	 "DFHPA1",     0, true),    // 36
	CICS_DFHPA2(EnumInstrDataCategory.CICS_PRECOMPILER,      "DFHPA2",     0, true),    // 37
	CICS_DFHPA3(EnumInstrDataCategory.CICS_PRECOMPILER,      "DFHPA3",     0, true),    // 38
	CICS_TWA(EnumInstrDataCategory.CICS_PRECOMPILER,         "TWA",        0, true),    // 39
	CICS_CWA(EnumInstrDataCategory.CICS_PRECOMPILER,         "CWA",        0, true),    // 40
	CICS_CSA(EnumInstrDataCategory.CICS_PRECOMPILER,         "CSA",        0, true),    // 41
	CICS_DFHPF1(EnumInstrDataCategory.CICS_PRECOMPILER,      "DFHPF1",       0, true),    // XX
	CICS_DFHPF2(EnumInstrDataCategory.CICS_PRECOMPILER,      "DFHPF2",       0, true),    // XX
	CICS_DFHPF3(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF3",       0, true),    // XX
	CICS_DFHPF4(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF4",       0, true),    // XX
	CICS_DFHPF5(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF5",       0, true),    // XX
	CICS_DFHPF6(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF6",       0, true),    // XX
	CICS_DFHPF7(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF7",       0, true),    // XX
	CICS_DFHPF8(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF8",       0, true),    // XX
	CICS_DFHPF9(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF9",       0, true),    // XX
	CICS_DFHPF10(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF10",       0, true),    // XX
	CICS_DFHPF11(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF11",       0, true),    // XX
	CICS_DFHPF12(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF12",       0, true),    // XX
	CICS_DFHPF13(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF13",       0, true),    // XX
	CICS_DFHPF14(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF14",       0, true),    // XX
	CICS_DFHPF15(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF15",       0, true),    // XX
	CICS_DFHPF16(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF16",       0, true),    // XX
	CICS_DFHPF17(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF17",       0, true),    // XX
	CICS_DFHPF18(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF18",       0, true),    // XX
	CICS_DFHPF19(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF19",       0, true),    // XX
	CICS_DFHPF20(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF20",       0, true),    // XX
	CICS_DFHPF21(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF21",       0, true),    // XX
	CICS_DFHPF22(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF22",       0, true),    // XX
	CICS_DFHPF23(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF23",       0, true),    // XX
	CICS_DFHPF24(EnumInstrDataCategory.CICS_PRECOMPILER,        "DFHPF24",       0, true),    // XX

	
	SQL_CODE(EnumInstrDataCategory.SQL_PRECOMPILER,          "SQLCODE",    0, true);    // 36 Status sql


	//////////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanza per ogni entry
	//////////////////////////////////////////////////////////////////////////////////////
	
	
	String fieldName = "";							// Nome campo utilizzato nel programma
	int sizeBytes = 0;							    // Lunghezza in bytes campo cics, 0 indica indeterminata
	EnumInstrDataCategory typePrecompiler = EnumInstrDataCategory.NOT_ASSIGNED;
    boolean indexVerify = true;
	
	/*
	 * Costruttore vuoto
	 */
	private EnumDataItemSystemEnvironment() {
	}
	

	/*
	 * Costruttore 
	 */
	private EnumDataItemSystemEnvironment(EnumInstrDataCategory typePrecompiler, String fieldName, int sizeBytes, boolean indexVerify) {
		this.typePrecompiler = typePrecompiler;
		this.fieldName = fieldName;
		this.sizeBytes = sizeBytes;
		this.indexVerify = indexVerify;
	}

	/**
	 * Restituisce il nome del campo
	 * 
	 * @return the fieldName
	 */
	public String getFieldName() {
		return fieldName;
	}

	/**
	 * Restituisce il tipo di precompilatore di appartenenza
	 * del data item.
	 * 
	 * @return the en_InstrCategory
	 */
	public EnumInstrDataCategory getTypePrecompiler() {
		return this.typePrecompiler;
	}

	/**
	 * @return the sizeBytes
	 */
	public int getSizeBytes() {
		return sizeBytes;
	}

	/**
	 * @param sizeBytes the sizeBytes to set
	 */
	public void setSizeBytes(int sizeBytes) {
		this.sizeBytes = sizeBytes;
	}


	/**
	 * Restituisce true se dopo il campo deve essere verificato
	 * indexamento e reference modification.<br>
	 * <p>
	 * @return the indexVerify
	 */
	public boolean isIndexVerify() {
		return indexVerify;
	}

}

