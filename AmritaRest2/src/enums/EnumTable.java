package enums;
/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumTable	
 * </h1>
 *  <p>
 * Lists all system tables managed by means of the forward generalized rule table system.<br>
 * <br><pre>
 * 0000 - 9999 system reserved.<br>
 * 			 	00000 - 00999 - System tables/enumerations common to all modules 
 * 			 	01000 - 09999 - Module dependent tables
 * <br>
 * 10000 - ????? Application dependent grouped by function
 *            	10011 - Function 1 Table function captions 
 *             	10012 - Function 1 Table function technical help 
 *          	10013 - Function 1 Table function user help 
 *    ...        
 *            	100n1 - Function n Table function captions 
 *             	100n2 - Function n Table function technical help 
 *          	100n3 - Function n Table function user help </pre>
 *            
 *            
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 12/03/2010
 * @see EntityTableHeader
 * @see EntityTableData
 * @see EntityTableStructure
*/

public enum EnumTable {
	
	EnumTable("T0000", 0),                     	    // Master index - Elenco generale tabelle definite nel sistema
	EnumObject("T0001", 1),                    	    // Tipo oggetto 
	EnumSourceType("T0002", 2),                 	// Tipo sorgente oggetto
	EnumObjectStatus("T0003", 3),                	// Stato oggetto 		
	EnumObjectOption("T0004", 4),              	    // Opzione oggetto 	
	EnumRelationType("T0005", 5),                   // Tipo relazione (Diretta/indiretta)
	EnumRelationSourceProcess("T0006", 6),          // Processo sorgente relazione	
	EnumLanguageItemInstr("T0007", 7),              // Linguaggio item/istruzione (Cobol, Pl1, Sql, ..)	
	EnumDataItemType("T0008", 8),                   // Tipo data item  
	EnumPrecompilerReservedWords("T0009", 9),       // Tipo istruzione per tutti i linguaggi/operandi/opzioni
	EnumWhereUsedType("T0010", 10),                 // Tipo where used (input/output)
	EnumWhereUsedTypeAlias("T0011", 11),            // Tipo alias where used (FULL_MATCH, LOWER_MATCH, etc.)
	EnumIndexOrder("T0012", 12),                    // Ordinamento indice
	EnumProcessStatus("T0013", 13),                 // Stato processi e funzioni in esecuzione
	EnumDataItemGeneric("T0014", 14),               // Generico data item senza implementazione specifica (numerico, text, condizione)
	EnumPrecompilerOptionsInstruction("T0015", 15), // Opzioni istruzioni precompiler, come cics ????? inserire su db
	EnumCobolUsage("T0016", 16),             	    // Usage cobol ????? inserire su db
	EnumExpressionItem("T0017", 17),            	// Operandi e opratori in espressione Cobol
	EnumInstrDataCategory("T0018", 18),				// Categoria istruzione (procedure o non)
	EnumDataItemOption("T0019", 19),                // Opzioni di un data item  
	EnumCobolSymbolType("T0020", 20),               // Categoria simboli data item Cobol
	EnumDataItemSystemEnvironment("T0021", 21),     // Data item dipendenti dall'ambiente operativo (Cics", 0), ..)
	EnumDateFormat("T0022", 22),                    // Formati data
	EnumInstrStatus("T0023", 23),                   // Stato istruzione analizzata
	EnumMessageType("T0024", 24),                   // Tipo messaggio (Info, Error, ...)
	EnumTypeProcessAnalysis("T0025", 25),           // Tipo processo di analisi attivo
	free26("T0026", 26),                   			// Free
	EnumLogicSetMode("T0027", 27),                  // Tipologia valorizzazioni in trasformazioni campi elementari
	EnumLogicSetPointerArea("T0028", 28),           // Area di definizione del pointer che indirizza l'area di definizione del campo impostato
	EnumCobolReservedWords("T0029", 29),            // Tipologie istruzioni in programmi Cobol
	TableFieldDescLong("T0030", 30),                // Descrizione lunga campo di tabella forward
	TableFieldDescShort("T0031", 31),               // Descrizione breve campo di tabella forward
	free32("T0032", 32),                    		// Free
	EnumDirectivesExecution("T0033", 33),           // Direttive di esecuzione processi e funzioni
	EnumRelation("T0034", 34),                    	// Relazione definita fra due oggetti
	EnumRelationOriginInstr("T0035", 35),           // Istruzione origine della relazione
	free36("T0036", 36),                   			// Free
	EnumSymbolType("T0037", 37),                    // Tipologia simbolo di programma
	EnumSymbolUsedAs("T0038", 38),                  // Tipologia utilizzo simbolo di programma
	EnumSymbolUsedBy("T0039", 39),                  // Tipologia utilizzatore simbolo di programma	
	EnumLanguage("T0040", 40),                      // Linguaggio stessa codifica locale (it minuscolo)
	EnumMetricsSqualeRating("T0041", 41),           // Squale livelli di rating A-E
	EnumMetricsQualitySections("T0042", 42),        // Sezioni qualità da tabella METR
	EnumMapTypeField("T0043", 43),                  // Tipologia campo mappa bms
	EnumScopeSection("T0044", 44),                  // Tipologia scope (campo di validita)  
	EnumMetricsScope("T0045", 45),              	// Tipologia scope per metriche
	EnumMetricsViolation("T0046", 46),               // Violazioni metriche
	EnumMetricsViolationSeverity("T0047", 47),      // Severità violazioni metriche
	EnumMetricsViolationFixUnit("T0048", 48),		// Costo remediation violazioni
	EnumMetricsQualityCharacteristics("T0049", 49), // Caratteristiche qualità metriche
	EnumUserExit("T0050", 50),						// Possibili operazioni in User Exit
	EnumUserStatus("T0051", 51),					// Stato utente
	EnumUserType("T0052", 52);						// Tipo utente
	

	// Variabili di istanza valide per ogni elemento dell'enumerazione
	private String codeTable = "";
	private int numTable = 0;
	
    /*
     * 
     * Costruttore con codice tabella text e numero tabellas
     * 	
     */
	private EnumTable(String codeTable, int numTable) {
		this.codeTable = codeTable;
		this.numTable = numTable;
	}

	/**
	 * @return the codeTable
	 */
	public String getCodeTable() {
		return codeTable;
	}

	/**
	 * @return the numTable
	 */
	public int getNumTable() {
		return numTable;
	}



}
