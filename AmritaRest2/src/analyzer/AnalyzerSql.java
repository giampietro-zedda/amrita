package analyzer;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import dao.DAOImplCopyEntityDefinition;
import dao.DAOImplObject;
import dao.DAOImplObjectOption;
import dao.IDAOCopyEntityDefinition;
import dao.IDAOObject;
import dao.IDAOObjectOption;
import utilities.DateTimeService;
import utilities.ReflectionManager;
import utilities.StringService;
import entities.EntityCopyEntityDefinition;
import entities.EntityIndexItem;
import entities.EntityMetricValue;
import entities.EntityObject;
import entities.EntityObjectOption;
import entities.EntityRelation;
import entities.EntityRelationOrigin;
import enums.EnumAmritaExceptionError;
import enums.EnumCobolReservedWords;
import enums.EnumDataItemType;
import enums.EnumIndexOrder;
import enums.EnumInstrPrecompilerType;
import enums.EnumLanguageItemInstr;
import enums.EnumMessageType;
import enums.EnumMetricsScope;
import enums.EnumMetricsViolation;
import enums.EnumModule;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import enums.EnumPrecompilerReservedWords;
import enums.EnumRelation;
import enums.EnumSourceType;
import enums.EnumSqlExpressionElementType;
import enums.EnumSqlOperator;
import enums.EnumSqlPredicate;
import enums.EnumSqlTableReferenceType;
import enums.EnumSymbolType;
import exception.ExceptionAmrita;



/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * AnalyzerSql
 * </h1>
 * <p>
 * Questa classe modella tutte le funzionalità relative all'analisi di sorgenti Sql, DDL e DML. <br>
 * <p>
 * Come in {@link AnalyzerCobol}, viene gestito l'esame del sorgente e l'isolamento delle varie istruzioni
 * con un parser parzialmente pilotato dall'enumerazione per Sql {@link EnumPrecompilerReservedWords}.<br>
 * Di ogni istruzione analizzata viene prodotto un oggetto istruzione per DDL e DML  {@linkInstructionSql}<br>
 * <p>
 * A fronte dell'analisi di un sorgente DDL viene prodotto e serializzato un oggetto SqlDDL.<br>
 * <br>
 * Il metodo di analisi di una specifica istruzione può essere richiamato esternamente per effettuare l'analisi di
 * uno statement Sql DDL o DML, restituendo l'oggetto istruzione corrispondente.<br>
 *  <p>
 * Questa classe eredita da {@link ExecutionShared} e pertanto ha tutte le capacità operative di
 * esecuzione, come l'accesso alla base dati.
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 28/06/2011
 * 
*/

public class AnalyzerSql extends Analyzer implements AmritaConstants {
	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza generali e usate nei processi ricorsivi                                       //                                                        //
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private AnalyzerCobol acp = null;											  // Analizzatore cobol attivo con embedded Sql
	private AnalyzerDbInfo analyzerDbInfo = null;							      // Contenitore/gestore aggiornamenti		
	private String scriptSqlName = "";											  // Stringa vuota significa attivazione da analyzer Cobol
	private ScriptSql scriptSql = null;											  // Descrittore script sql	
	
	// Informazioni contesto di esecuzione
	private InnerContextAnalysis ictx = null;
	private int idxScriptInDbObject = 0;                                          // Indice oggetto pgm in struttura db
	private int curNumDefScript = 0;                           				      // Numero di definizione corrente procedure division
    private String nextToken = "";                                                //
	private String strProgramScript = "";										  //	
	private String strProgramScriptObj = "";									  //	
	private Scanner scn = null;													  // Scanner corrente	
	private Scanner scnNoMemory = null;											  // Scanner copia di corrente senza memoria token precedenti
    private String strNoMemoryToParse = "";                                       // Per scnNoMemory
    
    // Metriche per il programma sotto analisi.
    // Saranno serialiizzate insieme all'ogggetto ProgramCobol.
    private Metrics metricsScriptSql = null;
 	 
    // Parole riservate per identificazione istruzione.
    // Key  = parola valida di inizio istruzione o operando o operatore etc.
    // Dati = ArrayList di insiemi di sequenze di parole identificanti una istruzione valida
    // Con questa Map si gestisce l'individuazione di qualsiasi tipo di istruzione.
    // La Map viene caricata dal costruttore a partire da EnumCobolReservedWords
    private Map<String, ArrayList<InnerInstructionWordsEntry>> map_SqlReservedWords;	
    
    // Per ottimizzazione recupero enumerazione a fronte di ordinale
    private EnumPrecompilerReservedWords ar_enumPrecompilerReservedWords[] = null;
	
    // Sistema/sottosistema del programma sotto analisi
    private UserExitInfo userExitInfoPgm = null;                                  

	
	/**
	 * Costruttore per attivazione a fronte di analisi pgm cobol con istruzioni sql embedded
	 */
	public AnalyzerSql(UserConfiguration sd, ExecutionDirectives di, AnalyzerCobol acp) {
		super(sd, di);
		
        this.acp = acp;
		this.analyzerDbInfo = acp.getDbInfo();
		
		// Origine processo di analisi e inizializzazione
		this.ictx = new InnerContextAnalysis ();
		this.ictx.activeTypeSource = EnumSourceType.COBOL_PROGRAM;
		this.ictx.activeSourceName = this.acp.getProgramName();
		this.ictx.typeObjectOrigin = EnumObject.OBJECT_PGM_COBOL;
		this.ictx.idObjectOrigin = this.acp.getProgramName();
		initialFromConstructor();
	} 
	

	/**
	 * Costruttore per attivazione a fronte di analisi di script sql
	 */
	public AnalyzerSql(UserConfiguration sd, ExecutionDirectives di, String scriptSqlName) {
		super(sd, di);
		
		this.analyzerDbInfo = new AnalyzerDbInfo(sd, di, scriptSqlName);
		this.scriptSqlName = scriptSqlName;
		
		// Origine processo di analisi e inizializzazione
		this.ictx = new InnerContextAnalysis ();
		this.ictx.activeTypeSource = EnumSourceType.SQL_SCRIPT;
		this.ictx.activeSourceName = this.scriptSqlName;
		this.ictx.typeObjectOrigin = EnumObject.OBJECT_SQL_SCRIPT;
		this.ictx.idObjectOrigin = this.scriptSqlName;
		initialFromConstructor();
	} 
	
    /*
     * Operazioni iniziali comuni ai due costruttori
     * 
     */
	private void initialFromConstructor() {
		
		AmritaStartup.rm = new ReflectionManager();
		
	    // Grammatica parole riservate per parser generalizzato istruzione.
		Map<String, ArrayList<ArrayList<String>>> map_SqlSequenceKeys = null;	
		ArrayList<ArrayList<String>> al_al_sequenceKeysDescriptor = null;
		ArrayList<ArrayList<String>> al_al_sequenceKeys = null;
		ArrayList<String> al_sequenceKeysDescriptor = null;
		ArrayList<Integer> al_ordinalEnumSequenceKeys = null;
		ArrayList<InnerInstructionWordsEntry> al_sqlReservedWords = null;
		InnerInstructionWordsEntry sqlReservedWords = null;
		EnumPrecompilerReservedWords typeInstr = null;
		int ordinalEnumSequenceKeys = 0;
		
		// Creazione Array con enumerazioni X accesso veloce
		ar_enumPrecompilerReservedWords = new EnumPrecompilerReservedWords[EnumPrecompilerReservedWords.values().length];
		for (int i = 0; i < EnumPrecompilerReservedWords.values().length; i++) {
			EnumPrecompilerReservedWords enumIstr = EnumPrecompilerReservedWords.values()[i];
			ar_enumPrecompilerReservedWords[i] = enumIstr;
		}
		
		
		//////////////////////////////////////////////////////////////////////
		// Creazione possibili sequenze di chiave a cura della classe madre
		//////////////////////////////////////////////////////////////////////
		
		al_al_sequenceKeysDescriptor = new ArrayList<ArrayList<String>> ();
		al_ordinalEnumSequenceKeys = new ArrayList<Integer> ();;

		// Scan enumerazione con parole riservate Sql gestite
		for (EnumPrecompilerReservedWords en_reservedWord : EnumPrecompilerReservedWords.values()) {
			
			// Interessano solo le Exec Sql
			if (!en_reservedWord.name().startsWith("SQL")) {
				continue;
			}
			// Valori chiave
			al_sequenceKeysDescriptor = new ArrayList<String> ();
			if (!en_reservedWord.getValueText1().equals("")) {al_sequenceKeysDescriptor.add(en_reservedWord.getValueText1());}
			if (!en_reservedWord.getValueText2().equals("")) {al_sequenceKeysDescriptor.add(en_reservedWord.getValueText2());}
			if (!en_reservedWord.getValueText3().equals("")) {al_sequenceKeysDescriptor.add(en_reservedWord.getValueText3());}
			if (!en_reservedWord.getValueText4().equals("")) {al_sequenceKeysDescriptor.add(en_reservedWord.getValueText4());}
			al_al_sequenceKeysDescriptor.add(al_sequenceKeysDescriptor);
			
			// Enumerazione origine
			al_ordinalEnumSequenceKeys.add(en_reservedWord.ordinal());
			
		}
		map_SqlSequenceKeys = getParserSequenceKeysList(al_al_sequenceKeysDescriptor, al_ordinalEnumSequenceKeys);
		
		//////////////////////////////////////////////////////////////////////
		// Caricamento valori generati in map_SqlReservedWords
		//////////////////////////////////////////////////////////////////////
		
		this.map_SqlReservedWords = new HashMap<String, ArrayList<InnerInstructionWordsEntry>>(ar_enumPrecompilerReservedWords.length * 2);
		
		// Scan keys di inizio sequenze parole chiave
        for (String keyMap : map_SqlSequenceKeys.keySet()) {
			
        	// Scan insiemi di sequenze di parole chiave
        	al_al_sequenceKeys = map_SqlSequenceKeys.get(keyMap);
        	for (ArrayList<String> al_sequenceKeys : al_al_sequenceKeys) {
        		
        		// Tipo istruzione
        		ordinalEnumSequenceKeys = Integer.parseInt(al_sequenceKeys.get(0));
        		typeInstr = ar_enumPrecompilerReservedWords[ordinalEnumSequenceKeys];
        		
        		// Recupero/inizializzazione ArrayList di sequenze di chiavi da mappare 
        		al_sqlReservedWords = this.map_SqlReservedWords.get(keyMap);
        		if (al_sqlReservedWords == null) {
        			al_sqlReservedWords = new ArrayList<InnerInstructionWordsEntry>();
        			this.map_SqlReservedWords.put(keyMap, al_sqlReservedWords);
				}
        		
        		// Caricamento tipo istruzione mappata
        		sqlReservedWords = new InnerInstructionWordsEntry();
           		sqlReservedWords.en_WordReservedOwner = typeInstr;
           		sqlReservedWords.typeEntry = typeInstr.getTypeEntry();
           		
           		// Caricamento Sequence keys mappate
           		for (int i = 1; i < al_sequenceKeys.size(); i++) {
           			sqlReservedWords.al_wordKey.add(al_sequenceKeys.get(i));
				}
           		al_sqlReservedWords.add(sqlReservedWords);
           		
 			} // end-for insiemi di sequenze di parole chiave
        	
 		} // end-for keys di inizio sequenze parole chiave
		
	}

	


	/**
	 * Analisi istruzione precompilatore Sql. <br>
	 * <p>
	 * Questo metodo viene richiamato dall'analizzatore Cobol {@link AnalyzerCobol} a fronte di 
	 * una istruzione di precompilatore Sql.<br>
	 * Viene effettuata l'analisi standard, comune anche agli script sql, e viene aggiornata
	 * l'istruzione con i simboli in input e i output, per un completo cross reference
	 * a livello di programma Cobol.<br>
	 * Il source dell'istruzione fornito non è epurato dell' eventuale EXEC SQL ... END-EXEC,
	 * se proveniente da program embedded e dal ; finale se proveniente da uno script Sql.<br>
	 * Vengono inoltre aggiornati i simboli in input all'istruzione, in output
	 * e definiti dall'istruzione stessa (literal).<br>
	 * <p>
	 * Vengono aggiornate le strutture dinamiche dell'istruzione, disponibili con
	 * metodi specifici per ogni tipologia di istruzione, talvolta direttamente e<br>
	 * talvolta attraverso strutture specifiche.<br>
	 * <p>
	 * @throws ExceptionAmrita 
	 * @throws SQLException 
	 * 
	 */
	public void analyzeSqlInstruction(InstructionSql instruction) throws ExceptionAmrita, SQLException {
		
		Scanner scn = null;
		InnerContextAnalysis ictx = null;
		ArrayList<InnerTokenInstr> al_tokenNormalized = null;
		StringBuffer sb_instrNormalized = null;
		String str = "";
		String sourceNormalized = "";
        String token1 = "";
        String token3 = "";
		
		// Di servizio per share analisi script sql
		ictx = new InnerContextAnalysis();
		ictx.isAnalysisOfScript = false;
		ictx.typeObjectOrigin = EnumObject.OBJECT_PGM_COBOL;
		ictx.idObjectOrigin = this.acp.getProgramName();
		 
		// Eliminazione EXEC SQL ... END-EXEC se presente
		str = instruction.getSourceInstr();
		if (instruction.getSourceInstr().startsWith("EXEC ")) {
			str = str.substring(5).trim();									// Elimino EXEC
			str = str.substring(4).trim();									// Elimino SQL
			str = deleteTrailingCharIfAny(str, ".").trim();					// Elimino .
			str = str.substring(0, str.length() - 8);						// Elimino END-EXEC
		}
		
		// Gestione speciale per DECLARE nome-tb TABLE: nessuna operazione
		scn = new Scanner(str);
		token1 = nextTokenNoMemory(scn);
		nextTokenNoMemory(scn);
		token3 = nextTokenNoMemory(scn);
		if (token1.equals("DECLARE")
		&&  token3.equals("TABLE")	) {
			instruction.setTypeInstrPrecompiler(EnumPrecompilerReservedWords.SQL_DECLARE_TABLE);
			return;
		}
		
		// Replace space con underscore dentro literal, segnalazione se non chiuse correttamente 
		sourceNormalized = replaceSpaceInLiteralWithUnderscore(str);						// Trasformazione literal alfanumeriche con spaces in token pieni
		sourceNormalized = normalizePreliminaryInstr(instruction, sourceNormalized);		// Normalizzazione preliminare istruzione
		if (sourceNormalized.equals("")) {
			instruction.setParsingError(true);
	       	instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0036", "", null, this.strProgramScript, this.strProgramScriptObj);
	       	logMessage(EnumMessageType.INFORMATION, "MI0035", this.acp.pgmNameUnderParsing, sourceNormalized);
			return;
		}
		
		// Normalizzazione source istruzione, con gli stessi metodi di analisi del file di script sql
		ictx.ar_RowsSource = new String[1];
		ictx.ar_RowsSource[0] = sourceNormalized;
		al_tokenNormalized = new ArrayList<InnerTokenInstr> ();
		getTokensSpreaded(ictx, al_tokenNormalized, ictx.rowStartSource, 0, SQL_SCRIPT_PREFETCH_PARSING_ROWS, true);
        
		// Causa sources ORACLE VALBRUNA inserisco FROM in DELETE se assente
		if (al_tokenNormalized.get(0).token.equals("DELETE") && !(al_tokenNormalized.get(1).token.equals("FROM"))) {
			InnerTokenInstr itin = new InnerTokenInstr();
			itin.token = "FROM";
			al_tokenNormalized.add(1, itin);
		}
		
		
		
        // Individua il tipo di istruzione o NOT_ASSIGNED se non riconosciuta.
		// Viene identificata qualsiasi tipo di istruzione, command sql
		// Split token in + token se necessario per corretta identificazione istruzione
		ictx.activeTypeInstr = getInstructionType(ictx, al_tokenNormalized, 0, true);     
		
		// Tipo istruzione non individuata: potrebbe essere una Select che inizia con WITH
		if (ictx.activeTypeInstr == EnumPrecompilerReservedWords.NOT_ASSIGNED
		&&  al_tokenNormalized.get(0).token.equals("WITH")
		&&  str.indexOf("SELECT ") > 0) {
			ictx.activeTypeInstr = EnumPrecompilerReservedWords.SQL_SELECT;
		}
		
		// Tipo istruzione non individuata: errore interno o source errato
		if (ictx.activeTypeInstr == EnumPrecompilerReservedWords.NOT_ASSIGNED) {
			instruction.setParsingError(true);
	       	logMessage(EnumMessageType.ERROR_INTERNAL, "EI0028", this.acp.pgmNameUnderParsing, instruction.getRowStartSource()+"", instruction.getPosStartInstr()+"", al_tokenNormalized.get(0).token);
	       	logMessage(EnumMessageType.INFORMATION, "MI0035", this.acp.pgmNameUnderParsing, sourceNormalized);
			return;
		}
		
		// Ricomposizione stringa source istruzione normalizzata
		sb_instrNormalized = new StringBuffer();
		for (InnerTokenInstr innerTokenInstr : al_tokenNormalized) {
			sb_instrNormalized.append(innerTokenInstr.token + " ");
		}
		sourceNormalized = sb_instrNormalized.toString();
		
		// Update istruzione da analizzare con input normalizzato
		instruction.setSourceInstr(sourceNormalized);
		instruction.setTypeInstrPrecompiler(ictx.activeTypeInstr);
		
		// Analisi generalizzata istruzioni comune a script sql e dml attivato da analizzatore Cobol
		analyzeSqlStmt(ictx, instruction);									
		
		// La segnalzione di errori/warning di parsing sulle istruzioni riconosciute è a cura del chiamante
	} 

	 
	/*
	 * Normalizzazione preliminare istruzione a fronte di attivazione
	 * da analizzatore Cobol, embedded, dove non serve mantenere la posizione
	 * di oghi singolo token come negli script.
	 * 
	 */
	private String normalizePreliminaryInstr(InstructionSql instruction, String sourceToNormalize) {
		String sourceNormalized = "";
		
		sourceNormalized = sourceToNormalize;
		sourceNormalized = sourceNormalized.replace("(", " ( ");
		sourceNormalized = sourceNormalized.replace(")", " ) ");
		sourceNormalized = sourceNormalized.replace("||", " || ");
		sourceNormalized = sourceNormalized.replace(";--", "; --");
		
		// Bad sysntax sql: qualifier 
		if (sourceNormalized.indexOf(". ") > 0) {
			sourceNormalized = sourceNormalized.replace(". ", ".");
			instruction.getMetricViolations().add(EnumMetricsViolation.R0158_AVOID_SQL_WITH_DEPRECATED_SYNTAX);
		}
		
		return sourceNormalized;
	}


	/**
	 * <h1>
	 * Analizza il sorgente dello script Sql memorizzato nell'istanza di SourceInput.<br>
	 * </h1>
	 * Nome Script e istanza di SourceInput sono stati valorizzati dal costruttore di questa classe.
	 * Le operazioni di parsing comuni sono effettuate in modo centralizzato e generalizzato nella classe
	 * madre AnalyzerCobol. Qui vengono solo sviluppate le logiche specifiche.<br>
	 * <p>
	 * @throws Exception 
	 */
	public void analyzeSqlScript(SourceInput si, String programName) throws Exception {
		
		InstructionSql instructionSql = null;
		ScriptSqlEntry scriptSqlEntry = null;
		
		initialOperationsScript(si);   			// Operazioni iniziali per oggetto ScriptSql da analizzare
		
		// Lettura ahead istruzione nell'oggetto Instruction... corretto
		instructionSql = getNextObjectInstruction(this.ictx);
        		
		// Scan istruzioni del programma da analizzare come oggetto Instruction completo di info sorgente
		while (instructionSql != null) {
			
			// Analisi istruzione con aggiornamento variabili di controllo
			analyzeSqlStmt(ictx, instructionSql);  

			// Update strutture generali script
			updateScriptSqlXrefFromInstruction(this.ictx, instructionSql);
			scriptSqlEntry = new ScriptSqlEntry();
			scriptSqlEntry.setInstruction(instructionSql);
			this.curNumDefScript = this.scriptSql.addEntryScript(scriptSqlEntry);
			instructionSql.setNumInstr(this.curNumDefScript);
			this.curNumDefScript++;

			
			// Se parsing error impostazione flag generale di errore
			if (instructionSql.isParsingError() 
			||  instructionSql.isSemanticError()) {
				this.ictx.isAnyInstructionErrorDetected = true;
			}
			// Se warning impostazione flag generale di warning
			if (instructionSql.isWarning()) {
				this.ictx.isAnyInstructionWarningDetected = true;
			}

			// Lettura in ciclo oggetto istruzione successiva
			instructionSql = getNextObjectInstruction(this.ictx);
	        
		} // end-while

		
		finalOperationsScript();  // Operazioni finali su istruzioni di procedure division e generali di programma
	}

	


	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Metodi privati                                        /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

	


	/*
     * 
     * Analizza l'istruzione sql, se fra quelle da analizzare.
     * Si aggiornano le strutture interne, nell'istruzione, dei simboli utilizzati.
     * 
     */
	private void analyzeSqlStmt(InnerContextAnalysis ictx, InstructionSql instruction) throws ExceptionAmrita, SQLException {
	    
		ictx.numInstrOrigin = this.curNumDefScript;
		
		switch (ictx.activeTypeInstr) {
                
		        // DDL statements gestite
		
				case SQL_CREATE_DATABASE:  
					analyzeSqlCreateDatabase(ictx, instruction);
					break;

				case SQL_CREATE_TABLESPACE:  
					analyzeSqlCreateTablespace(ictx, instruction);
					break;

				case SQL_CREATE_STOGROUP:  
					analyzeSqlCreateStogroup(ictx, instruction);
					break;

				case SQL_CREATE_TABLE:  
					analyzeSqlCreateTable(ictx, instruction);
					break;
		
				case SQL_CREATE_VIEW:  
					analyzeSqlCreateView(ictx, instruction);
					break;
		
				case SQL_CREATE_INDEX:  
					analyzeSqlCreateIndex(ictx, instruction);
					break;
		
				case SQL_CREATE_SYNONIM:  
					analyzeSqlCreateSynonym(ictx, instruction);
					break;
		
				case SQL_CREATE_ALIAS:  
					analyzeSqlCreateAlias(ictx, instruction);
					break;

			    // DML statements gestite
					
				case SQL_SELECT:  
					analyzeSqlSelect(ictx, instruction);  
					break;

				case SQL_UPDATE: 
					analyzeSqlUpdate(ictx, instruction);
					break;  

				case SQL_INSERT:
					analyzeSqlInsert(ictx, instruction);
					break;

				case SQL_DELETE:
					analyzeSqlDelete(ictx, instruction);
					break;

				case SQL_MERGE: 
					analyzeSqlMerge(ictx, instruction);
					break;

				case SQL_DECLARE_CURSOR:  
					analyzeSqDeclareCursor(ictx, instruction);
					break;

				case SQL_OPEN: 
					analyzeSqOpen(ictx, instruction);
					break;

				case SQL_CLOSE:  
					analyzeSqClose(ictx, instruction);
					break;

				case SQL_FETCH:
					analyzeSqFetch(ictx, instruction);
					break;

				case SQL_LOCK_TABLE: 
					analyzeSqLockTable(ictx, instruction);
					break;

				case SQL_PREPARE: 
					break;

				case SQL_EXECUTE:
					analyzeSqPrepare(ictx, instruction);
					break;

				case SQL_BEGIN_DECLARE_SECTION: 
					analyzeSqDeclareSection(ictx, instruction);
					break;

				case SQL_END_DECLARE_SECTION: 
					analyzeSqEndDeclareSection(ictx, instruction);
					break;

				default:
					break;
				}

	}
	    

	 
	/*
     * ----------------------------------
     * Analisi statement Create Table.
     * ----------------------------------
     * 
     * 
     */
	private void analyzeSqlCreateTable(InnerContextAnalysis ictx, InstructionSql instruction) throws ExceptionAmrita {

		EntityObject eo = null;
		EntityRelation er = null;
		EntityRelationOrigin ero = null;
		UserExitInfo userExitInfoEntity = null;
		Scanner scn = null;
		EntityCopyEntityDefinition ecd = null;
		String str = "";
		String strSql = "";
		String token = "";
		int i1 = -1;
		int i2 = -1;
 		int numSeq = 0;
		
        SqlTable sqlTable = null;
 		String tableFullName = "";
 		String tableFullNameNormalized = "";
		String db2LocationName = "";
		String tableOwner = "";;
		String tableName = "";
		String strOccursed = "";
		String strOptions = "";
		String ar_strColumn[] = null;
		
		// Allocazione strutture di lavoro
		sqlTable = new SqlTable();
		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		////////////////////////////////////////////////////////////////////
		// Estrazione e analisi fino a nome tabella
		///////////////////////////////////////////////////////////////////
		
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		scn = new Scanner(str);									        //  
		
		token = nextToken(scn);                                         // CREATE
		token = nextToken(scn);                                         // TABLE
		
		tableFullName = nextToken(scn);                                 // table-name
		i1 = tableFullName.indexOf(".");
		if (i1 > 0) {
			i2 = tableFullName.indexOf(".", i1 + 1);
		}
		// Nessuna qualificazione
		if (i1 < 0 && i2 < 0) {
			tableName = tableFullName;
			tableOwner = "";
		// Completa qualificazione
		} else if (i1 > 0 && i2 > 0) {
			db2LocationName = tableFullName.substring(0, i1);
			tableOwner = tableFullName.substring(i1 + 1, i2);
			tableName = tableFullName.substring(i2);
			db2LocationName = deleteTrailingLeadingApiceIfAny(db2LocationName).trim();
			tableOwner = deleteTrailingLeadingApiceIfAny(tableOwner).trim();
			tableName = deleteTrailingLeadingApiceIfAny(tableName).trim();
			tableFullNameNormalized = db2LocationName + "." + tableOwner + "." + tableName;
		// Solo qualificazione owner
		} else {
			db2LocationName = "";
			tableOwner = tableFullName.substring(0, i1);
			tableName = tableFullName.substring(i1 + 1);
			tableOwner = deleteTrailingLeadingApiceIfAny(tableOwner).trim();
			tableName = deleteTrailingLeadingApiceIfAny(tableName).trim();
			tableFullNameNormalized = tableOwner + "." +tableName;

		}

		sqlTable.setTableFullName(tableFullNameNormalized);
		sqlTable.setDb2LocationName(db2LocationName);
		sqlTable.setTableOwner(tableOwner);
		sqlTable.setTableName(tableName);
   
		

		///////////////////////////////////////////////////////////////
		// Estrazione sezioni da analizzare 
		///////////////////////////////////////////////////////////////
		
		token = nextToken(scn);											// ( ?
		
		// Presente definizione di parametri, vincoli, periodo fra parentesi e separati da virgole
        if (token.equals("(")) {
        	strOccursed = extractStringBetweenPar(scn).trim();
			token = nextToken(scn);										// Primo token dopo )
		}
        strOptions = token + " " + this.strNoMemoryToParse.trim();
	
        
        //////////////////////////////////////////////////////////
        // Parsing sezioni estratte
        //////////////////////////////////////////////////////////
        
        // Constraint-unique/primary/referential/check/columns definition
        if (!strOccursed.equals("")) {
        	ar_strColumn = splitParms(strOccursed);
			// Scan singole definizioni colonne/period/Unique/constraints/..
			for (String strColumn : ar_strColumn) {
				createTableParseParmsOccursed(instruction, strColumn, sqlTable);
				// Istruzione incompleta o errata
				if (instruction.isParsingError() || instruction.isSemanticError()) {
					return;
				}
			}
		}
        
		// Definizioni/Opzioni generali a livello di tabella
		parseParmsTable(instruction, strOptions, sqlTable);     // -> this.nextToken

		
		// Istruzione incompleta o errata
		if (instruction.isParsingError() || instruction.isSemanticError()) {
			return;
		}
		

		 
		////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////

		// Identificazione sistema e sottosistema
	    userExitInfoEntity = userExitGetSystemSubsystem(this.di, "", tableOwner, tableName);

    	// Descriptor istruzione con opzioni e strutture dati
  		instruction.sqlCreateTableSetTableDescriptor(sqlTable);
   		
 		// Sql delete preliminare colonne tabella
 		strSql = "DELETE FROM copyEntityDefinition  WHERE sys = '" + di.systemInput + "'" +
								    " AND subSys = '" + di.subSystemInput + "'" +
							        " AND idObject = '" + sqlTable.getTableName() + "'" +
							        " AND typeObject =  " + EnumObject.OBJECT_ENTITY_SQL.ordinal();
 		this.analyzerDbInfo.addSqlDeleteStatement(strSql);
 	  	
 		
		// Oggetto database da inserire a fine analisi
 		if (!sqlTable.getDatabaseName().equals("")) {
 			eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_DBNAME, sqlTable.getDatabaseName(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
            eo.setSystem(di.systemInput);
            eo.setSubSystem(di.subSystemInput);
            eo.setSystemOwner(di.systemInput);
            eo.setSubSystemOwner(di.subSystemInput);
		}
 	    // Oggetto tablespace da inserire a fine analisi
 		if (!sqlTable.getTablespaceName().equals("")) {
 	 		eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_TABLESPACE, sqlTable.getTablespaceName(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
            eo.setSystem(di.systemInput);
            eo.setSubSystem(di.subSystemInput);
            eo.setSystemOwner(di.systemInput);
            eo.setSubSystemOwner(di.subSystemInput);
		}
 	    // Oggetto entity da inserire a fine analisi nel suo sistema/sottosistema di appartenenza
  		eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, sqlTable.getTableName(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
        eo.setSystem(di.systemInput);
        eo.setSubSystem(di.subSystemInput);
        eo.setSystemOwner(di.systemInput);
        eo.setSubSystemOwner(di.subSystemInput);
        // Oggetto db2-location da inserire a fine analisi  
        if (!sqlTable.getDb2LocationName().equals("")) {
        	eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_DB2_LOCATION, sqlTable.getDb2LocationName(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
            eo.setSystem(di.systemInput);
            eo.setSubSystem(di.subSystemInput);
            eo.setSystemOwner(di.systemInput);
            eo.setSubSystemOwner(di.subSystemInput);
		}
 	    // Oggetto owner da inserire a fine analisi nel suo sistema/sottosistema di appartenenza
        if (!sqlTable.getTableOwner().equals("")) {
        	eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER, sqlTable.getTableOwner(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
            eo.setSystem(di.systemInput);
            eo.setSubSystem(di.subSystemInput);
            eo.setSystemOwner(di.systemInput);
            eo.setSubSystemOwner(di.subSystemInput);
        } 	
		
 		// DBNAME_ENTITY
        if (!sqlTable.getDatabaseName().equals("")) {
    		er  = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.DBNAME_ENTITY, EnumObject.OBJECT_SQL_DBNAME, sqlTable.getDatabaseName(), EnumObject.OBJECT_ENTITY_SQL, sqlTable.getTableName(), userExitInfoEntity);
            er.setSystem(di.systemInput);
            er.setSubSystem(di.subSystemInput);
    		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.DBNAME_ENTITY, EnumObject.OBJECT_SQL_DBNAME, sqlTable.getDatabaseName(), EnumObject.OBJECT_ENTITY_SQL, sqlTable.getTableName(), instruction, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoEntity, instruction);
            ero.setSystem(di.systemInput);
            ero.setSubSystem(di.subSystemInput);
            
    		// DBNAME_SQL_OWNER
            if (!sqlTable.getTableOwner().equals("")) {
        		er =  this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.DBNAME_SQL_OWNER, EnumObject.OBJECT_SQL_DBNAME, sqlTable.getDatabaseName(), EnumObject.OBJECT_SQL_OWNER, sqlTable.getTableOwner(), userExitInfoEntity);
                er.setSystem(di.systemInput);
                er.setSubSystem(di.subSystemInput);
        		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.DBNAME_SQL_OWNER, EnumObject.OBJECT_SQL_DBNAME, sqlTable.getDatabaseName(), EnumObject.OBJECT_SQL_OWNER, sqlTable.getTableOwner(), instruction, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoEntity, instruction);
                ero.setSystem(di.systemInput);
                ero.setSubSystem(di.subSystemInput);
              }
    		// DBNAME_SQL_TABLESPACE
            if (!sqlTable.getTablespaceName().equals("")) {
          		er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_TABLESPACE, EnumObject.OBJECT_SQL_DBNAME, sqlTable.getDatabaseName(), EnumObject.OBJECT_SQL_TABLESPACE, sqlTable.getTablespaceName(), this.userExitInfoPgm);
                er.setSystem(di.systemInput);
                er.setSubSystem(di.subSystemInput);
        		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.DBNAME_SQL_TABLESPACE, EnumObject.OBJECT_SQL_DBNAME, sqlTable.getDatabaseName(), EnumObject.OBJECT_SQL_TABLESPACE, sqlTable.getTablespaceName(), instruction, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoEntity, instruction);
                ero.setSystem(di.systemInput);
                ero.setSubSystem(di.subSystemInput);
            }
        }
		
		// ENTITY_SQL_TABLESPACE
        if (!sqlTable.getTablespaceName().equals("")) {
       		er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_TABLESPACE, EnumObject.OBJECT_ENTITY_SQL, sqlTable.getTableName(), EnumObject.OBJECT_SQL_TABLESPACE, sqlTable.getTablespaceName(), userExitInfoEntity);
            er.setSystem(di.systemInput);
            er.setSubSystem(di.subSystemInput);
    		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_TABLESPACE, EnumObject.OBJECT_ENTITY_SQL, sqlTable.getTableName(), EnumObject.OBJECT_SQL_TABLESPACE, sqlTable.getTablespaceName(), instruction, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoEntity, instruction);
            ero.setSystem(di.systemInput);
            ero.setSubSystem(di.subSystemInput);
        }
		
		// ENTITY_SQL_OWNER
        if (!sqlTable.getTableOwner().equals("")) {
    		er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, sqlTable.getTableName(), EnumObject.OBJECT_SQL_OWNER, sqlTable.getTableOwner(), userExitInfoEntity);
            er.setSystem(di.systemInput);
            er.setSubSystem(di.subSystemInput);
     		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, sqlTable.getTableName(), EnumObject.OBJECT_SQL_OWNER, sqlTable.getTableOwner(), instruction, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoEntity, instruction);
            ero.setSystem(di.systemInput);
            ero.setSubSystem(di.subSystemInput);
 		}
		
		// ENTITY_SQL_SCRIPT
		er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, sqlTable.getTableName(), EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, userExitInfoEntity);
        er.setSystem(di.systemInput);
        er.setSubSystem(di.subSystemInput);
        ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, sqlTable.getTableName(), EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, instruction, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoEntity, instruction);
        ero.setSystem(di.systemInput);
        ero.setSubSystem(di.subSystemInput);
	
		numSeq = 0;
		
		// Caricamento colonne in descrittore campi CPYE, comune ai moduli copy
        for (SqlTableColumn tableColumn : sqlTable.getColumns()) {
			
        	// Creazione oggetto e inserimento in struttura
        	ecd = this.analyzerDbInfo.prepareForDbCopyEntityDefinition(EnumObject.OBJECT_ENTITY_SQL, sqlTable.getTableName(), numSeq, tableColumn.getColumnName(), EnumLanguageItemInstr.ITEM_SQL, tableColumn.getDataType() );
            ecd.setSystem(di.systemInput);
            ecd.setSubSystem(di.subSystemInput);
			
			// Completamento oggetto già inserito in struttura
        	ecd.setLngBytes(tableColumn.getNumChar());
        	ecd.setNumDigit(tableColumn.getNumDigit());
        	ecd.setScale(tableColumn.getScale());
        	ecd.setNotNull(tableColumn.isNotNull());
        	ecd.setWithDefault(tableColumn.isWithDefault());
        	ecd.setDefaultValue(tableColumn.getWithDefaultValue());    // Valorizza se default constant
			
			numSeq++;
		}

	}

	/*
     * ----------------------------------
     * Analisi statement Create View.
     * ----------------------------------
     * 
     * 
     */
	private void analyzeSqlCreateView(InnerContextAnalysis ictx, InstructionSql instruction) throws ExceptionAmrita, SQLException {

		EntityObject eo = null;
		EntityRelation er = null;
		EntityRelationOrigin ero = null;
		EntityObjectOption eoo = null;
		UserExitInfo userExitInfoEntity = null;
		@SuppressWarnings("unused")
		EntityCopyEntityDefinition copyItemDefinition = null;
		ArrayList<SqlCommonTableExpression> al_commonTableExpression = null;
		InstructionSql instructionFullSelect = null;
		Scanner scnOptions = null;
		String str = "";
		String strFullSelect = "";
		String strFullSelectAndOptions = "";
		String strOptions = "";
		String strCommonTableExpressions = "";
		String strSql = "";
		String token = "";
		String viewFullName = "";
		String viewFullNameNormalized = "";
		String viewOwner = "";;
		String viewName = "";
		int i1 = -1;
		int iOptions = 0;
 		int numSeq = 0;
		
        SqlView sqlView = null;
		
		// Allocazione strutture di lavoro
		sqlView = new SqlView();
		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		////////////////////////////////////////////////////////////////////
		// Parser
		///////////////////////////////////////////////////////////////////
		
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		this.scn = new Scanner(str);									//  
		token = nextToken(scn);                                         // CREATE
		token = nextToken(scn);                                         // VIEW
		
		viewFullName = nextToken(scn);                                 	// 
		i1 = viewFullName.indexOf(".");
		// Nessuna qualificazione
		if (i1 < 0) {
			viewName = viewFullName;
			viewOwner = "";
		// Completa qualificazione
		} else {
			viewOwner = viewFullName.substring(0, i1);
			viewName = viewFullName.substring(i1 + 1);
		}
		sqlView.setViewOwner(deleteTrailingLeadingApiceIfAny(viewOwner).trim());
		sqlView.setViewName(deleteTrailingLeadingApiceIfAny(viewName).trim());
		viewFullNameNormalized = viewOwner;
		if (i1 > 0) {
			viewFullNameNormalized = viewOwner + "." + viewName;
		} else {
			viewFullNameNormalized = viewOwner + "." + viewName;
		}
		sqlView.setViewFullName(viewFullNameNormalized);
 
		token = nextToken(scn);					// (|AS ?
		
        // Sono specificate le colonne della view
		if (token.equals("(")) {
			token = nextToken(scn);	
			// Estrazione colonne
			while (!token.equals("") && !token.equals(")")) {
				if (token.equals(",")) {
					token = nextToken(scn);	
					continue;
				}
				sqlView.getColumns().add(token);
				token = nextToken(scn);	
			}
		}
		
		// Inizio clausola AS
		token = nextToken(scn);					// AS
		token = nextToken(scn);					// WITH ?
		
		// WITH common-table-expression: estrazione completa
		if (token.equals("WITH")) {
			strCommonTableExpressions = token;
			while (!token.equals("") && !token.equals("SELECT")) {
				token = nextToken(scn);			// table-identifier
				strCommonTableExpressions = strCommonTableExpressions + " " + token;     // 
				token = nextToken(scn);	       
			}
		}
		
		// Può essere solo full-select
		if (!token.equals("SELECT")) {
			instruction.setParsingError(true);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
			return;
		}
		
		// Full select oggetto della view
		strFullSelectAndOptions = this.strNoMemoryToParse;
		
		// Estrazione opzioni
		iOptions = strFullSelectAndOptions.indexOf("WITH");
		if (iOptions > 0) {
			strOptions = strFullSelectAndOptions.substring(iOptions).trim();
			strFullSelect = strFullSelectAndOptions.substring(0, iOptions).trim();
		} else {
			strFullSelect = strFullSelectAndOptions;
		}
		
		// Analisi opzioni, se presenti
		if (!strOptions.equals("")) {
			scnOptions = new Scanner(strOptions);
			token = nextToken(scnOptions);				// WITH
			token = nextToken(scnOptions);				// CASCADE|LOCAL| |
			if (token.equals("CASCADED")) {
				sqlView.setWithCascadeCheckOption(true);
			} else if (token.equals("LOCAL")) {
				sqlView.setWithLocalCheckOption(true);
			} else {
				sqlView.setWithCheckOption(true);
			}
		}
		
		// Analisi ricorrenze multiple di common-table-expression
		if (!strCommonTableExpressions.equals("")) {
			al_commonTableExpression = analyzeSqCommonTableExpressions(ictx, instruction, strCommonTableExpressions);
		} else {
			al_commonTableExpression = new ArrayList<SqlCommonTableExpression> ();
		}
		
		// Analisi istruzione Sql Select
		instructionFullSelect = new InstructionSql();
		instructionFullSelect.setSourceInstr(strFullSelect);
		instructionFullSelect.setTypeInstr(EnumCobolReservedWords.PRECOMPILER_SQL);
		instructionFullSelect.setTypeInstrPrecompiler(EnumPrecompilerReservedWords.SQL_SELECT);
		analyzeSqlSelect(ictx, instructionFullSelect);
		
		// Analysys error: trasferisco l'errore all'istruzione in input
		if (instructionFullSelect.isParsingError() || instructionFullSelect.isSemanticError()) {
			instruction.setParsingError(instructionFullSelect.isParsingError());
			instruction.setSemanticError(instructionFullSelect.isSemanticError());
			instruction.setMsgCode(instructionFullSelect.getMsgCode());
			instruction.setMsgType(instructionFullSelect.getMsgType());
			instruction.setMsgParm(instructionFullSelect.getMsgParm());
			return;
		}
		
		// Impostazione strutture analizzate
		sqlView.setFullSelect(instructionFullSelect);
		sqlView.setCommonTableExpressions(al_commonTableExpression);
		

		////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////

		// Identificazione sistema e sottosistema
	    userExitInfoEntity = userExitGetSystemSubsystem(this.di, "", viewOwner, viewName);

		
    	// Descriptor istruzione con opzioni e strutture dati
  		instruction.sqlCreateViewSetViewDescriptor(sqlView);
   		
 		// Sql delete preliminare colonne tabella
 		strSql = "DELETE FROM copyEntityDefinition  WHERE sys = '" + di.systemInput + "'" +
								    " AND subSys = '" + di.systemInput + "'" +
							        " AND idObject = '" + sqlView.getViewName() + "'" +
							        " AND typeObject =  " + EnumObject.OBJECT_ENTITY_SQL.ordinal();
 		this.analyzerDbInfo.addSqlDeleteStatement(strSql);
 	  	
 		
		// Oggetti da inserire a fine analisi
		eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, sqlView.getViewName(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
        eo.setSystem(di.systemInput);
        eo.setSubSystem(di.subSystemInput);
        eo.setSystemOwner(di.systemInput);
        eo.setSubSystemOwner(di.subSystemInput);
        if (!sqlView.getViewOwner().equals("")) {
        	eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER, sqlView.getViewOwner(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
            eo.setSystem(di.systemInput);
            eo.setSubSystem(di.subSystemInput);
            eo.setSystemOwner(di.systemInput);
            eo.setSubSystemOwner(di.subSystemInput);
        } 	
		
 		// ENTITY_SQL_OWNER
        if (!sqlView.getViewOwner().equals("")) {
    		er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, sqlView.getViewName(), EnumObject.OBJECT_SQL_OWNER, sqlView.getViewOwner(), userExitInfoEntity);
            eo.setSystem(di.systemInput);
            eo.setSubSystem(di.subSystemInput);
   		    
            ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, sqlView.getViewName(), EnumObject.OBJECT_SQL_OWNER, sqlView.getViewOwner(), instruction, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoEntity, instruction);
            ero.setSystem(di.systemInput);
            ero.setSubSystem(di.subSystemInput);
		}
		
		// ENTITY_SQL_SCRIPT
        er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, sqlView.getViewName(), EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, userExitInfoEntity);
        er.setSystem(di.systemInput);
        er.setSubSystem(di.subSystemInput);
        
        ero= this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, sqlView.getViewName(), EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, instruction, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoEntity, instruction);
        ero.setSystem(di.systemInput);
        ero.setSubSystem(di.subSystemInput);

		// Option ENTITY_AS_VIEW
		eoo = this.analyzerDbInfo.prepareForDbObjectOption(sqlView.getViewName(), EnumObject.OBJECT_ENTITY_SQL, EnumObjectOption.ENTITY_SQL_AS_VIEW);
	    eoo.setSystem(di.systemInput);
	    eoo.setSubSystem(di.subSystemInput);
		
		
		numSeq = 0;
		
		// Caricamento colonne in descrittore campi CPYE, comune ai moduli copy
        for (String columnName : sqlView.getColumns()) {
			
        	// Creazione oggetto e inserimento in struttura
			copyItemDefinition = this.analyzerDbInfo.prepareForDbCopyEntityDefinition(EnumObject.OBJECT_ENTITY_SQL, sqlView.getViewName(), numSeq, columnName, EnumLanguageItemInstr.ITEM_SQL, EnumDataItemType.NOT_ASSIGNED );
			
			// Non sono disponibili altre informazioni a livello di colonna
			
			numSeq++;
		}

	}

	/* -------------------------------------------------------------------
	 * Analisi insiemi di sql common-table-expression separate da virgola
	 * -------------------------------------------------------------------
	 * 
	 * Si tratta di elementi ripetuti composti da table-identifier, colonne e full-select
     *
	 * WITH   tb1-identifier |(col1, coln) AS (full-select1)
	 *        ...
	 *      , tbn-identifier |(col1, coln) AS (full-selectn) 
	 * 
	 * Restituzione arrayList con i singoli descrittori.
	 * 
	 * Viene richiesta un'istruzione, in cui eventualmente codificare l'errore
	 * e la stringa da analizzare.
	 * 
	 */
	private ArrayList<SqlCommonTableExpression> analyzeSqCommonTableExpressions(InnerContextAnalysis ictx, InstructionSql instruction, String strCommonTableExpressions) throws SQLException, ExceptionAmrita  {
		
		Scanner scn = null;
		SqlCommonTableExpression commonTableExpression = null;
		ArrayList<SqlCommonTableExpression> al_commonTableExpression = null;
		SqlFullSelect fullSelect = null;
		String ar_strCommonTableExpressionSingle[] = null;
		String ar_columnName[] = null;
		String strFullSelect = "";
		String str = "";
		String token = "";
		String strBetweenPar = "";
 		
        // Errori precedenti
        if (instruction.isParsingError()) {
			return new ArrayList<SqlCommonTableExpression> ();
		}
        
        // Input vuoto: restituisco arrayList vuoto
        if (strCommonTableExpressions.equals("")) {
        	return new ArrayList<SqlCommonTableExpression> ();
		}
        
		al_commonTableExpression = new ArrayList<SqlCommonTableExpression> ();
		
		
		str = strCommonTableExpressions.substring(4).trim();			// Eliminazione WITH
		ar_strCommonTableExpressionSingle = splitParms(str);            // Estrazione elementi singoli
		
		// Scan elementi estratti
		for (String strCommonTableExpressionSingle : ar_strCommonTableExpressionSingle) {
			
			commonTableExpression = new SqlCommonTableExpression ();
			scn = new Scanner(strCommonTableExpressionSingle);

			token = nextToken(scn);											// table-identifier
			commonTableExpression.setTableIdentifier(token);
			token = nextToken(scn);											// (|AS
			
			// (col1, ... )
			if (token.equals("(")) {
				strBetweenPar = extractStringBetweenPar(scn);
				ar_columnName = splitParms(strBetweenPar);
				// Scan nomi colonne
				for (String columnName : ar_columnName) {
					commonTableExpression.getColumns().add(columnName);
				}
				token = nextToken(scn);										// AS
			}
			
			// Deve essere AS
			if (!token.equals("AS")) {
				instruction.setParsingError(true);
				instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
	            return al_commonTableExpression;
			}
			
			token = nextToken(scn);										   // ( fullselect )
			strFullSelect = extractStringBetweenPar(scn);
			
			// Analisi standard full Select
			fullSelect = analyzeSqlFullselect(ictx, instruction, strFullSelect);
			
			// Analysys error: trasferisco l'errore all'istruzione in input
			if (instruction.isParsingError() || instruction.isSemanticError()) {
				return al_commonTableExpression;
			}
			
			// Update struttura e new token
			commonTableExpression.setFullSelect(fullSelect);
			
			// Porto in output
			al_commonTableExpression.add(commonTableExpression);

		}
		
		return al_commonTableExpression;
	}


	/*
     * ----------------------------------
     * Analisi statement Create Database.
     * ----------------------------------
     * 
     * 
     */
	private void analyzeSqlCreateDatabase(InnerContextAnalysis ictx, InstructionSql instruction) throws ExceptionAmrita {
		
	   	EntityObject eo = null;
		EntityRelation er = null;
		EntityRelationOrigin ero = null;
		UserExitInfo userExitInfo = null;
		String token = "";
		String str = "";
		String databaseName = "";
		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		     
		////////////////////////////////////////////////////////////////////
		// Parser
		///////////////////////////////////////////////////////////////////
		
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		this.scn = new Scanner(str);									//  
		token = nextToken(scn);                                         // CREATE
		token = nextToken(scn);                                         // DATABASE
		token = nextToken(scn); 										// database-name
		databaseName = token;                                   
		

		////////////////////////////////////////////////////////////////////
		// Identificazione sistema e sottosistema 
		///////////////////////////////////////////////////////////////////
		
		// Identificazione sistema e sottosistema
	    userExitInfo = userExitGetSystemSubsystem(this.di, "", "", "");

    	// Descriptor istruzione con opzioni e strutture dati
  		instruction.sqlCreateDatabaseSetDatabaseName(databaseName);
   		
 		// Oggetti da inserire a fine analisi
		eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_DBNAME, databaseName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfo);
        eo.setSystem(userExitInfo.getSystem());
        eo.setSubSystem(userExitInfo.getSubSystem());
        eo.setSystemOwner(userExitInfo.getSystem());
        eo.setSubSystemOwner(userExitInfo.getSubSystem());
 		
		// DBNAME_SQL_SCRIPT
		if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
			er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.DBNAME_SQL_SCRIPT, EnumObject.OBJECT_SQL_DBNAME, databaseName, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, userExitInfo);
	        er.setSystem(userExitInfo.getSystem());
	        er.setSubSystem(userExitInfo.getSubSystem());
			ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.DBNAME_SQL_SCRIPT, EnumObject.OBJECT_SQL_DBNAME, databaseName, EnumObject.OBJECT_SQL_SCRIPT, ictx.idObjectOrigin, instruction, ictx.typeObjectOrigin, ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfo, instruction);
	        ero.setSystem(userExitInfo.getSystem());
	        ero.setSubSystem(userExitInfo.getSubSystem());
		}
	}


	/*
     * -------------------------------------
     * Analisi statement Create Tablespace.
     * -------------------------------------
     * 
     * 
     */
	private void analyzeSqlCreateTablespace(InnerContextAnalysis ictx, InstructionSql instruction) throws ExceptionAmrita {
		
	   	EntityObject eo = null;
		EntityRelation er = null;
		EntityRelationOrigin ero = null;
		UserExitInfo userExitInfo = null;
		String token = "";
		String str = "";
		
 		String databaseName = "";
 		String tablespaceName = "";
		boolean isLobTablespace = false;
		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		////////////////////////////////////////////////////////////////////
		// Parser
		///////////////////////////////////////////////////////////////////
		
		str = instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		this.scn = new Scanner(str);									//  
		token = nextToken(scn);                                         // CREATE
		token = nextToken(scn); 										// LOB|TABLESPACE ?
		if (token.equals("LOB")) {
			isLobTablespace = true;
			token = nextToken(scn); 									// TABLESPACE 
		}
		token = nextToken(scn); 							 			// tablespace-name
		tablespaceName = token;
		tablespaceName = deleteTrailingLeadingApiceIfAny(tablespaceName).trim();
		token = nextToken(scn);                                         // IN ?
		if (token.equals("IN")) {
			databaseName = nextToken(scn); 								// database-name
			databaseName = deleteTrailingLeadingApiceIfAny(databaseName).trim();
		}
		    
		// Gli altri parametri non sono trattati
		

		
    	// Descriptor istruzione con opzioni e strutture dati
  		instruction.sqlCreateTablespaceSetDatabaseName(databaseName);
  		instruction.sqlCreateTablespaceSetLob(isLobTablespace);
   		
		
		////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////
  		
		// Identificazione sistema e sottosistema
	    userExitInfo = userExitGetSystemSubsystem(this.di, tablespaceName, "", "");

		
		// Oggetti da inserire a fine analisi
		eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_TABLESPACE, tablespaceName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfo);
        eo.setSystem(userExitInfo.getSystem());
        eo.setSubSystem(userExitInfo.getSubSystem());
        eo.setSystemOwner(userExitInfo.getSystem());
        eo.setSubSystemOwner(userExitInfo.getSubSystem());
		if (!databaseName.equals("")) {
			eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_DBNAME, databaseName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfo);
	        eo.setSystem(userExitInfo.getSystem());
	        eo.setSubSystem(userExitInfo.getSubSystem());
	        eo.setSystemOwner(userExitInfo.getSystem());
	        eo.setSubSystemOwner(userExitInfo.getSubSystem());
		}
		 		
		// TABLESPACE_SQL_SCRIPT
		if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
			er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.TABLESPACE_SQL_SCRIPT, EnumObject.OBJECT_SQL_TABLESPACE, tablespaceName, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, userExitInfo);
	        er.setSystem(userExitInfo.getSystem());
	        er.setSubSystem(userExitInfo.getSubSystem());
	        ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.TABLESPACE_SQL_SCRIPT, EnumObject.OBJECT_SQL_TABLESPACE, tablespaceName, EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin,  EnumCobolReservedWords.NOT_ASSIGNED, userExitInfo, instruction);
	        ero.setSystem(userExitInfo.getSystem());
	        ero.setSubSystem(userExitInfo.getSubSystem());
		}

		// PGM_SQL_TABLESPACE
		if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
			er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_TABLESPACE, EnumObject.OBJECT_PGM_COBOL, tablespaceName, EnumObject.OBJECT_SQL_SCRIPT, tablespaceName, userExitInfo);
	        er.setSystem(userExitInfo.getSystem());
	        er.setSubSystem(userExitInfo.getSubSystem());
			ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_TABLESPACE, EnumObject.OBJECT_PGM_COBOL, tablespaceName, EnumObject.OBJECT_SQL_SCRIPT, tablespaceName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfo, instruction);
	        ero.setSystem(userExitInfo.getSystem());
	        ero.setSubSystem(userExitInfo.getSubSystem());
		}

		// DBNAME_SQL_TABLESPACE
		if (!databaseName.equals("")) {
			er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.DBNAME_SQL_TABLESPACE, EnumObject.OBJECT_SQL_DBNAME, databaseName, EnumObject.OBJECT_SQL_TABLESPACE, tablespaceName, userExitInfo);
	        er.setSystem(userExitInfo.getSystem());
	        er.setSubSystem(userExitInfo.getSubSystem());
			ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.DBNAME_SQL_TABLESPACE, EnumObject.OBJECT_SQL_DBNAME, databaseName, EnumObject.OBJECT_SQL_TABLESPACE, tablespaceName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfo, instruction);
	        ero.setSystem(userExitInfo.getSystem());
	        ero.setSubSystem(userExitInfo.getSubSystem());
		}

	}

	/*
     * ----------------------------------------
     * Analisi statement Create Storage group.
     * ----------------------------------------
     * 
     * 
     */
	private void analyzeSqlCreateStogroup(InnerContextAnalysis ictx, InstructionSql instruction) throws ExceptionAmrita {
 
	   	EntityObject eo = null;
		UserExitInfo userExitInfo = null;
		String token = "";
		String str = "";
 		String stogroupName = "";
 		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		////////////////////////////////////////////////////////////////////
		// Parser
		///////////////////////////////////////////////////////////////////
		
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		this.scn = new Scanner(str);									//  
		token = nextToken(scn);                                         // CREATE
		token = nextToken(scn); 										// STOGROUP
		token = nextToken(scn); 							 	        // stogroup-name
		stogroupName = token;                                           //  
		
		
		// Gli altri parametri non sono trattati
		

		
		////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////
		
		// Identificazione sistema e sottosistema
	    userExitInfo = userExitGetSystemSubsystem(this.di, "", "", "");

	    // Descriptor istruzione con opzioni e strutture dati
  		instruction.sqlCreateStogroupSetName(stogroupName);
    		
 	  	
		// Oggetti da inserire a fine analisi
		eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_STOGROUP, stogroupName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfo);
        eo.setSystem(userExitInfo.getSystem());
        eo.setSubSystem(userExitInfo.getSubSystem());
        eo.setSystemOwner(userExitInfo.getSystem());
        eo.setSubSystemOwner(userExitInfo.getSubSystem());
		
  	}


	/*
     * ----------------------------------------
     * Analisi statement Create Index.
     * ----------------------------------------
     * 
     * 
     */
	private void analyzeSqlCreateIndex(InnerContextAnalysis ictx, InstructionSql instruction) throws ExceptionAmrita {

	   	EntityObject eo = null;
		EntityRelation er = null;
		EntityRelationOrigin ero = null;
		EntityIndexItem eii = null;
		UserExitInfo userExitInfo = null;
			
		String str = "";
		String strColumns = "";
		String strOtherOptions = "";
		String strSql = "";
		String token = "";
		String indexFullName = "";
		String indexFullNameNormalized = "";
		String indexOwner = "";;
		String indexName = "";
		String tableFullName = "";
		String tableFullNameNormalized = "";
		String tableOwner = "";;
		String tableName = "";
		int i1 = -1;
 		int numSeq = 0;
		
        Sqlndex sqlIndex = null;
		
		// Allocazione strutture di lavoro
		sqlIndex = new Sqlndex();
		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		////////////////////////////////////////////////////////////////////
		// Parser
		///////////////////////////////////////////////////////////////////
		
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		this.scn = new Scanner(str);									//  
		token = nextToken(scn);                                         // CREATE
		token = nextToken(scn);                                         // INDEX|UNIQUE
        if (token.equals("UNIQUE")) {
        	sqlIndex.setUnique(true);
        	token = nextToken(scn); 
        	if (token.equals("WHERE")) {
           		token = nextToken(scn); 								// NOT
           		token = nextToken(scn); 								// NULL
           		sqlIndex.setUniqueWhereNotNull(true);
           		token = nextToken(scn); 								// INDEX
			}
		}
        token = nextToken(scn);                                         // index-name
		
        // Estrazione owner e nome indice
        indexFullName = token;                                          //
		i1 = indexFullName.indexOf(".");
		// Nessuna qualificazione: owner non presente
		if (i1 < 0 ) {
			indexOwner = "";
			indexName = indexFullName;
			indexName = deleteTrailingLeadingApiceIfAny(indexName).trim();
			indexFullNameNormalized = indexName;
		// Owner presente
		} else {
			indexOwner = indexFullName.substring(0, i1);
			indexName = indexFullName.substring(i1 + 1);
			indexOwner = deleteTrailingLeadingApiceIfAny(indexOwner).trim();
			indexName = deleteTrailingLeadingApiceIfAny(indexName).trim();
			indexFullNameNormalized = indexOwner +"." + indexName;
		}
		
		
		token = nextToken(scn);                                         // ON
		token = nextToken(scn);                                         // table-name|aux-yable-name

		// Estrazione owner e nome tabella
		tableFullName = token;                                 			// 
		i1 = tableFullName.indexOf(".");

		// Nessuna qualificazione: owner non presente
		if (i1 < 0 ) {
			tableOwner = "";
			tableName = tableFullName;
			tableName = deleteTrailingLeadingApiceIfAny(tableName).trim();
			tableFullNameNormalized = tableName;
		// Owner presente
		} else {
			tableOwner = tableFullName.substring(0, i1);
			tableName = tableFullName.substring(i1 + 1);
			tableOwner = deleteTrailingLeadingApiceIfAny(tableOwner).trim();
			tableName = deleteTrailingLeadingApiceIfAny(tableName).trim();
			tableFullNameNormalized = tableOwner + "." + tableName;
		}
		
		// Update strutture di controllo
		sqlIndex.setIndexFullName(indexFullNameNormalized);
		sqlIndex.setIndexOwner(indexOwner);
		sqlIndex.setIndexName(indexName);
		sqlIndex.setTableFullName(tableFullNameNormalized);
		sqlIndex.setTableOwner(tableOwner);
		sqlIndex.setTableName(tableName);
  
		token = nextToken(scn);											// (
		token = nextToken(scn);											// First token column
		
		// Estrazione stringa con descrittori completi colonne
		while (!token.equals("") && !token.equals(")")) {
			strColumns = strColumns + " " + token;
			token = nextToken(scn);	
		}

		// Estrazione stringa con altre opzioni
		if (token.equals(")")) {
			token = nextToken(scn);
			while (!token.equals("")) {
				strOtherOptions = strOtherOptions + " " + token;
				token = nextToken(scn);	
			}
		}
		
		// Parsing specifico colonne indice e altre opzioni
		parseIndexColumns(instruction, strColumns, sqlIndex);
		parseIndexOtherParms(instruction, strOtherOptions, sqlIndex);
		
		// Istruzione incompleta o errata
		if (instruction.isParsingError() || instruction.isSemanticError()) {
			return;
		}
		
		
		////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////
		
		// Identificazione sistema e sottosistema
	    userExitInfo = userExitGetSystemSubsystem(this.di, "", tableOwner, tableName);

	    // Descriptor istruzione con colonne, opzioni e strutture dati
  		instruction.sqlCreateIndexSetIndexDescriptor(sqlIndex);
   		
 		// Sql delete preliminare colonne indice
 		strSql = "DELETE FROM IndexItem  WHERE sys = '" + userExitInfo.getSystem() + "'" 					+
								    " AND subSys = '" + userExitInfo.getSubSystem() + "'" 				+
							        " AND idObject = '" + sqlIndex.getIndexName() + "'" 					+
							        " AND typeObject =  " + EnumObject.OBJECT_SQL_INDEX.ordinal() 			+
							        " AND idObjectOwner = '" + sqlIndex.getTableName() + "'" 					+
							        " AND typeObjectOwner =  " + EnumObject.OBJECT_ENTITY_SQL.ordinal();
 		this.analyzerDbInfo.addSqlDeleteStatement(strSql);
 	  	
  		// Sql delete preliminare delle origini relazioni che verranno inserite
		strSql = "DELETE FROM RelationOrigin WHERE   sys = '" + userExitInfo.getSystem() + "'" 					+
								    " AND   subSys = '" + userExitInfo.getSubSystem() + "'" 				+
							        " AND   relation =  " + EnumRelation.ENTITY_INDEX.ordinal() 			+
							        " AND   idObjectA = '" + sqlIndex.getTableName() + "'" 					+
							        " AND   typeObjectA =  " + EnumObject.OBJECT_ENTITY_SQL.ordinal()          +
							        " AND   idObjectB = '" + sqlIndex.getIndexName() + "'" 					+
							        " AND   typeObjectB =  " + EnumObject.OBJECT_SQL_INDEX.ordinal();
		this.analyzerDbInfo.addSqlDeleteStatement(strSql);
 	  	

		// Oggetti da inserire a fine analisi
		eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_INDEX, sqlIndex.getIndexName(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfo);
        eo.setSystem(userExitInfo.getSystem());
        eo.setSubSystem(userExitInfo.getSubSystem());
        eo.setSystemOwner(userExitInfo.getSystem());
        eo.setSubSystemOwner(userExitInfo.getSubSystem());

        eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, sqlIndex.getTableName(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfo);
        if (!sqlIndex.getIndexOwner().equals("")) {
        	eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER, sqlIndex.getIndexOwner(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfo);
            eo.setSystemOwner(userExitInfo.getSystem());
            eo.setSubSystemOwner(userExitInfo.getSubSystem());
        } 	
        if (!sqlIndex.getTableOwner().equals("")) {
        	eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER, sqlIndex.getTableOwner(), EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfo);
            eo.setSystemOwner(userExitInfo.getSystem());
            eo.setSubSystemOwner(userExitInfo.getSubSystem());
      } 	
		
 		// ENTITY_INDEX
		er =  this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_INDEX, EnumObject.OBJECT_ENTITY_SQL, sqlIndex.getTableName(), EnumObject.OBJECT_SQL_INDEX, sqlIndex.getIndexName(), userExitInfo);
	    er.setSystem(userExitInfo.getSystem());
	    er.setSubSystem(userExitInfo.getSubSystem());
		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_INDEX, EnumObject.OBJECT_ENTITY_SQL, sqlIndex.getTableName(), EnumObject.OBJECT_SQL_INDEX, sqlIndex.getIndexName(), instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfo, instruction);
	    ero.setSystem(userExitInfo.getSystem());
	    ero.setSubSystem(userExitInfo.getSubSystem());
        
		// INDEX_SQL_OWNER
        if (!sqlIndex.getTableOwner().equals("")) {
        	er =  this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.INDEX_SQL_OWNER, EnumObject.OBJECT_SQL_INDEX, sqlIndex.getIndexName(), EnumObject.OBJECT_SQL_OWNER, sqlIndex.getTableOwner(), userExitInfo);
    	    er.setSystem(userExitInfo.getSystem());
    	    er.setSubSystem(userExitInfo.getSubSystem());
        	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.INDEX_SQL_OWNER, EnumObject.OBJECT_SQL_INDEX, sqlIndex.getIndexName(), EnumObject.OBJECT_SQL_OWNER, sqlIndex.getIndexOwner(), instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfo, instruction);
    	    ero.setSystem(userExitInfo.getSystem());
    	    ero.setSubSystem(userExitInfo.getSubSystem());
		}
		
		// ENTITY_SQL_OWNER
        if (!sqlIndex.getTableOwner().equals("")) {
    		er =  this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, sqlIndex.getTableName(), EnumObject.OBJECT_SQL_OWNER, sqlIndex.getTableOwner(), userExitInfo);
    	    er.setSystem(userExitInfo.getSystem());
    	    er.setSubSystem(userExitInfo.getSubSystem());
    		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, sqlIndex.getTableName(), EnumObject.OBJECT_SQL_OWNER, sqlIndex.getTableOwner(), instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfo, instruction);
    	    ero.setSystem(userExitInfo.getSystem());
     	    ero.setSubSystem(userExitInfo.getSubSystem());
  		}
		
		// INDEX_SQL_SCRIPT
        if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
        	er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.INDEX_SQL_SCRIPT, EnumObject.OBJECT_SQL_INDEX, sqlIndex.getIndexName(), EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName, userExitInfo);
    	    er.setSystem(userExitInfo.getSystem());
    	    er.setSubSystem(userExitInfo.getSubSystem());
        	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.INDEX_SQL_SCRIPT, EnumObject.OBJECT_SQL_INDEX, sqlIndex.getIndexName(), EnumObject.OBJECT_SQL_SCRIPT, this.scriptSqlName,  instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfo, instruction);
    	    ero.setSystem(userExitInfo.getSystem());
     	    ero.setSubSystem(userExitInfo.getSubSystem());
        }
		
		numSeq = 0;
		
		// Caricamento colonne indice in descrittore colonne indici copyEntityDefinition 
        for (SqlIndexColumn indexColumn : sqlIndex.getColumns()) {
			
        	// Creazione oggetto e inserimento in struttura
			eii = this.analyzerDbInfo.prepareForDbIndexItem(EnumObject.OBJECT_SQL_INDEX
														  , sqlIndex.getIndexName()
														  , EnumObject.OBJECT_ENTITY_SQL
														  , sqlIndex.getTableName()
														  , indexColumn.getColumnName()
													      , indexColumn.getOrder()
														  , numSeq
															);
			eii.setSystem(userExitInfo.getSystem());
			eii.setSubSystem(userExitInfo.getSubSystem());
			
			numSeq++;

        }
 

	}


    /*
     * ----------------------------
     * Parsing colonne indice.
     * ----------------------------
     * 
     * 
     * In input una stringa con sequenze di colonna, tipo ordinamento  
     * 
     */
	private void parseIndexColumns(InstructionSql instruction, String strColumns, Sqlndex sqlIndex) {
        
		SqlIndexColumn sqlIndexColumn = null;
        ArrayList<SqlIndexColumn> al_sqlIndexColumn = null;
        
		String columnName = "";
		String token = "";
		
		al_sqlIndexColumn = new ArrayList<SqlIndexColumn> ();
		sqlIndexColumn = new SqlIndexColumn ();
		
 		////////////////////////////////////////////////////////////////////
		// Parser
		////////////////////////////////////////////////////////////////////
		
		this.scn = new Scanner(strColumns);								//  
		
		token = nextToken(scn);                                         //  
		while (!token.equals("")) {
			
			// Separatore fra colonne
			if (token.equals(",")) {
				token = nextToken(scn); 
				continue;
			}
			if (token.equals("BUSINESS_TIME")) {
				sqlIndex.setBusinessTimeWithoutOverlaps(true);
				break;
			} 
			
			// Colonna
			columnName = token;
			sqlIndexColumn.setColumnName(deleteTrailingLeadingApiceIfAny(columnName));
			
			token = nextToken(scn);										// ASC|DESC|RANDOM| |
			if (token.equals("ASC")) {
				sqlIndexColumn.setOrderType(EnumIndexOrder.ORDER_ASCENDING);
			} else if (token.equals("DESC")) {
				sqlIndexColumn.setOrderType(EnumIndexOrder.ORDER_DESCENDING);
			} else if (token.equals("RANDOM")) {
				sqlIndexColumn.setOrderType(EnumIndexOrder.ORDER_RANDOM);
			} else if (token.equals(",")) {
				sqlIndexColumn.setOrderType(EnumIndexOrder.ORDER_ASCENDING);
			} 
			al_sqlIndexColumn.add(sqlIndexColumn);
			sqlIndexColumn = new SqlIndexColumn ();
			
			token = nextToken(scn); 								   // ,	
		}
		
		// Update colonne indice
		sqlIndex.setColumns(al_sqlIndexColumn);
		
	}

	
	/* -----------------------------------------
	 * Parsing altri parametri/opzioni indice
	 * -----------------------------------------
	 * 
	 * 
	 */
	private void parseIndexOtherParms(InstructionSql instruction, String strOtherOptions, Sqlndex sqlIndex) {

		SqlPartitions sqlPartitions = null;
		String token = "";
		String strPartitions = "";
		int cntParOpen = 0;
		int numericParm = 0;
		
 		////////////////////////////////////////////////////////////////////
		// Parser
		////////////////////////////////////////////////////////////////////
		
		this.scn = new Scanner(strOtherOptions);					    //  
		
		token = nextToken(scn);                                         //  
		
		while (!token.equals("")) {
			
			// CLUSTER e PADDED
			if (token.equals("NOT")) {
				token = nextToken(scn); 								// CLUSTER|PADDED
				if (token.equals("CLUSTER")) {
					sqlIndex.setCluster(false);
					token = nextToken(scn); 
					continue;
				}
				if (token.equals("PADDED")) {
					sqlIndex.setPadded(false);
					token = nextToken(scn); 
					continue;
				}
			}
			if (token.equals("CLUSTER")) {
				sqlIndex.setCluster(true);
				token = nextToken(scn); 
				continue;
			}
			if (token.equals("PADDED")) {
				sqlIndex.setPadded(true);
				token = nextToken(scn); 
				continue;
			}
			
			// DEFINE
			if (token.equals("DEFINE")) {
				token = nextToken(scn); 
				if (token.equals("YES")) {
					sqlIndex.setDefine(true);
					token = nextToken(scn); 
				}
				if (token.equals("NO")) {
					sqlIndex.setDefine(false);
					token = nextToken(scn); 
				}
				continue;
			}
			
			// COMPRESS
			if (token.equals("COMPRESS")) {
				sqlIndex.setPartitioned(true);
				token = nextToken(scn); 
				if (token.equals("YES")) {
					sqlIndex.setCompress(true);
					token = nextToken(scn); 
				}
				if (token.equals("NO")) {
					sqlIndex.setCompress(false);
					token = nextToken(scn); 
				}
				continue;
			}

			// CLOSE
			if (token.equals("CLOSE")) {
				token = nextToken(scn); 
				if (token.equals("YES")) {
					sqlIndex.setClose(true);
					token = nextToken(scn); 
				}
				if (token.equals("NO")) {
					sqlIndex.setClose(false);
					token = nextToken(scn); 
				}
				continue;
			}

			// DEFER
			if (token.equals("DEFER")) {
				token = nextToken(scn); 
				if (token.equals("YES")) {
					sqlIndex.setDefer(true);
					token = nextToken(scn); 
				}
				if (token.equals("NO")) {
					sqlIndex.setDefer(false);
					token = nextToken(scn); 
				}
				continue;
			}

			// COPY
			if (token.equals("COPY")) {
				token = nextToken(scn); 
				if (token.equals("YES")) {
					sqlIndex.setCopy(true);
					token = nextToken(scn); 
				}
				if (token.equals("NO")) {
					sqlIndex.setCopy(false);
					token = nextToken(scn); 
				}
				continue;
			}

			// BUFFERPOOL
			if (token.equals("BUFFERPOOL")) {
				token = nextToken(scn);								// bp-name
				sqlIndex.setBufferPool(token);
				token = nextToken(scn); 
				continue;
			}
			
			// PIECESIZE
			if (token.equals("PIECESIZE")) {
				token = nextToken(scn);								// integer
				numericParm = StringService._getNumericInt(token);
				sqlIndex.setPieceSizeValue(numericParm);
				token = nextToken(scn);								// K|M|G
				sqlIndex.setPieceSizeType(token);
				token = nextToken(scn); 
				continue;
			}
			 
			// USING VCAT|STOGROUP
			if (token.equals("USING")) {
				token = nextToken(scn); 
				if (token.equals("VCAT")) {
					token = nextToken(scn);							// Catalog-name
					sqlIndex.setUsingVcat(token);
					token = nextToken(scn); 
					continue;
				}
				if (token.equals("STOGROUP")) {
					token = nextToken(scn);							// Stogroup-name
					sqlIndex.setUsingStogroup(token);
					token = nextToken(scn); 
					continue;
				}
			}
			
			// GBPCACHE (di partition)
			if (token.equals("GBPCACHE")) {
				token = nextToken(scn); 							// CHANGED|ALL							
				sqlIndex.setGbpCache(token);
				token = nextToken(scn); 
				continue;
			}

			// PRIQTY (di STOGROUP)
			if (token.equals("PRIQTY")) {
				token = nextToken(scn); 							
				numericParm = StringService._getNumericInt(token);
				sqlIndex.setPriQty(numericParm);
				token = nextToken(scn); 
				continue;
			}
			
			// SECQTY (di STOGROUP)
			if (token.equals("SECQTY")) {
				token = nextToken(scn); 							
				numericParm = StringService._getNumericInt(token);
				sqlIndex.setSecQty(numericParm);
				token = nextToken(scn); 
				continue;
			}
			
			// ERASE (di STOGROUP)
			if (token.equals("ERASE")) {
				token = nextToken(scn); 							// YES|NO
				if (token.equals("YES")) {
					sqlIndex.setErase(true);
					token = nextToken(scn); 
					continue;
				}
				if (token.equals("NO")) {
					sqlIndex.setErase(false);
					token = nextToken(scn); 
					continue;
				}
			}
			
			// FREEPAGE
			if (token.equals("FREEPAGE")) {
				token = nextToken(scn); 							
				numericParm = StringService._getNumericInt(token);
				sqlIndex.setFreePage(numericParm);
				token = nextToken(scn); 
				continue;
			}
			
			// PCTFREE
			if (token.equals("PCTFREE")) {
				token = nextToken(scn); 							
				numericParm = StringService._getNumericInt(token);
				sqlIndex.setPctFree(numericParm);
				token = nextToken(scn); 
				continue;
			}
			
			// PARTITIONED
			if (token.equals("PARTITIONED") && !sqlIndex.isPartitioned()) {
				sqlIndex.setPartitioned(true);
				token = nextToken(scn); 
				continue;
			}
			
			// PARTITIONED BY RANGE (....
			if (token.equals("PARTITION")) {
				sqlPartitions = new SqlPartitions ();
				sqlIndex.setPartitionsDescriptor(sqlPartitions);
				sqlPartitions.setPartitionByRange(true);
				token = nextToken(scn); 								// BY
				token = nextToken(scn); 								// RANGE ?
				if (token.equals("RANGE")) {
					token = nextToken(scn); 							// (
				}
				cntParOpen = 1;
				token = nextToken(scn); 							    // Ahead
				while (!token.equals("")  && cntParOpen > 0) {
					
					if (token.equals("(")) {cntParOpen++;}
					if (token.equals(")")) {cntParOpen--;}
					
					strPartitions = strPartitions + " " + token;
					token = nextToken(scn);
				}
				parseIndexPartitions(instruction, strPartitions, sqlIndex);
				token = nextToken(scn);
				continue;
			}
			
			token = nextToken(scn); 								   // ,	
		}
		

	}



	/* -----------------------------------------
	 * Parsing definizione partizioni
	 * -----------------------------------------
	 * 
	 * 
	 */
	private void parseIndexPartitions(InstructionSql instruction, String strPartitions, Sqlndex sqlIndex) {
		
		SqlPartitionValues partitionValue = null;
		String token = "";
		int numValue = 0;
		
		
 		////////////////////////////////////////////////////////////////////
		// Parser
		////////////////////////////////////////////////////////////////////
		
		this.scn = new Scanner(strPartitions);					    		//  
		
		token = nextToken(scn);                                     		//  
		 
		while (!token.equals("")) {
			
			// Separator fra due definizioni di partizioni
			if (token.equals(",")) {
				sqlIndex.getPartitionsDescriptor().getValues().add(partitionValue);
				token = nextToken(scn);  
				continue;
			}
			
			// PARTITION
			if (token.equals("PARTITION")) {
				partitionValue = new SqlPartitionValues ();
				token = nextToken(scn);             // Partition number
				numValue = StringService._getNumericInt(token);
				partitionValue.setPartitionNumber(numValue);
				token = nextToken(scn);
				continue;
			}	
			
			// ENDING AT
			if (token.equals("ENDING")) {
				token = nextToken(scn);							// AT ?
				if (token.equals("AT")) {
					token = nextToken(scn);						// (
				}
				token = nextToken(scn);							// Ahead
				while (!token.equals("") && !token.equals(")")) {
					if (token.equals(",")) {
						token = nextToken(scn);
						continue;
					}
					partitionValue.getEndingAtValues().add(token);
					token = nextToken(scn);	
				}
				token = nextToken(scn);
				continue;
			}

			// INCLUSIVE
			if (token.equals("INCLUSIVE")) {
				token = nextToken(scn);
				partitionValue.setInclusive(true);
				token = nextToken(scn);
				continue;
			}
			
			// USING VCAT|STOGROUP
			if (token.equals("USING")) {
				token = nextToken(scn); 
				if (token.equals("VCAT")) {
					token = nextToken(scn);							// Catalog-name
					partitionValue.setUsingVcat(token);
				}
				if (token.equals("STOGROUP")) {
					token = nextToken(scn);							// Stogroup-name
					partitionValue.setUsingStogroup(token);
				}
				token = nextToken(scn); 
				continue;
			}
			
			// GBPCACHE 
			if (token.equals("GBPCACHE")) {
				token = nextToken(scn); 							// CHANGED|ALL							
				partitionValue.setGbpCache(token);
				token = nextToken(scn); 
				continue;
			}
			
			// PRIQTY  
			if (token.equals("PRIQTY")) {
				token = nextToken(scn); 							
				numValue = StringService._getNumericInt(token);
				partitionValue.setPriQty(numValue);
				token = nextToken(scn); 
				continue;
			}
			
			// SECQTY  
			if (token.equals("SECQTY")) {
				token = nextToken(scn); 							
				numValue = StringService._getNumericInt(token);
				partitionValue.setSeqQty(numValue);
				token = nextToken(scn); 
				continue;
			}
			
			// ERASE  
			if (token.equals("ERASE")) {
				token = nextToken(scn); 							// YES|NO
				if (token.equals("YES")) {
					partitionValue.setErase(true);
				}
				if (token.equals("NO")) {
					partitionValue.setErase(false);
				}
				token = nextToken(scn); 
				continue;
			}
			
			// FREEPAGE
			if (token.equals("FREEPAGE")) {
				token = nextToken(scn); 							
				numValue = StringService._getNumericInt(token);
				partitionValue.setFreePage(numValue);
				token = nextToken(scn); 
				continue;
			}
			
			// PCTFREE
			if (token.equals("PCTFREE")) {
				token = nextToken(scn); 							
				numValue = StringService._getNumericInt(token);
				partitionValue.setPctFree(numValue);
				token = nextToken(scn); 
				continue;
			}
			
			// Parametro non gestito
			token = nextToken(scn); 								   //  
		}
		 
		// Partizione pendente
		sqlIndex.getPartitionsDescriptor().getValues().add(partitionValue);
		
	}


	/*
     * ----------------------------------------
     * Analisi statement Create Synonym.
     * ----------------------------------------
     * 
     * 
     */
	private void analyzeSqlCreateSynonym(InnerContextAnalysis ictx, InstructionSql instruction) throws ExceptionAmrita {

		EntityObject eo = null;
		EntityRelation er = null;
		EntityRelationOrigin ero = null;
		UserExitInfo userExitInfoEntity = null;
		String token = "";
		String str = "";
		String synonymName = "";
		String entityOwner = "";
		String entityName = "";
		int iPoint = 0;
		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		////////////////////////////////////////////////////////////////////
		// Parser
		///////////////////////////////////////////////////////////////////
		
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		this.scn = new Scanner(str);									//  
		token = nextToken(scn);                                         // CREATE
		token = nextToken(scn); 										// SYNONYM
		token = nextToken(scn); 							 	        // synonymName
		iPoint = token.indexOf(".");
  		synonymName = deleteTrailingLeadingApiceIfAny(token);                  
		
		token = nextToken(scn); 										// FOR
		token = nextToken(scn); 										// owner.table|view
		iPoint = token.indexOf(".");
		if (iPoint > 0) {
			entityOwner = token.substring(0, iPoint);	
			entityName = token.substring(iPoint + 1).trim();	
			entityName = deleteTrailingLeadingApiceIfAny(entityName).trim();
			entityOwner = deleteTrailingLeadingApiceIfAny(entityOwner).trim();
		} else {
			entityName = token;
			entityName = deleteTrailingLeadingApiceIfAny(entityName).trim();
		}
		
		
		////////////////////////////////////////////////////////////////////
		// Identificazione sistema e sottosistema 
		///////////////////////////////////////////////////////////////////

		userExitInfoEntity = userExitGetSystemSubsystem(this.di, "", entityOwner, entityName);
 
		
		////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////
		
    	// Descriptor istruzione con opzioni e strutture dati
 		instruction.sqlCreateSynonymSetName(synonymName);
 		instruction.sqlCreateSynonymSetOwner(entityOwner);
 		instruction.sqlCreateSynonymSetTableView(entityName);
   		
 		// Oggetti da inserire a fine analisi
 		if (!entityOwner.equals("")) {
   		   eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,  entityOwner,  EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
   	       eo.setSystem(di.systemInput);
   	       eo.setSubSystem(di.subSystemInput);
  		}
 		
	    eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_ALIAS,  synonymName,  EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
        eo.setSystem(di.systemInput);
        eo.setSubSystem(di.subSystemInput);
        
	    eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL,  entityName,  EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
        eo.setSystem(di.systemInput);
        eo.setSubSystem(di.subSystemInput);
 		
		
		// ENTITY_SQL_SYNONYM
		er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SYNONYM, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_SYNONYM, synonymName, userExitInfoEntity);
		er.setSystem(di.systemInput);
		er.setSubSystem(di.subSystemInput);
		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SYNONYM, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_SYNONYM, synonymName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoEntity, instruction);
		ero.setSystem(di.systemInput);
		ero.setSubSystem(di.subSystemInput);
		
		// ENTITY_SQL_OWNER
		er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, userExitInfoEntity);
		er.setSystem(di.systemInput);
		er.setSubSystem(di.subSystemInput);
		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoEntity, instruction);
		ero.setSystem(di.systemInput);
		ero.setSubSystem(di.subSystemInput);
		
	}

	/*
     * ----------------------------------------
     * Analisi statement Create Alias.
     * ----------------------------------------
     * 
     * 
     */
	private void analyzeSqlCreateAlias(InnerContextAnalysis ictx, InstructionSql instruction) throws ExceptionAmrita {

		EntityObject eo = null;
		EntityRelation er = null;
		EntityRelationOrigin ero = null;
		UserExitInfo userExitInfoEntity = null;
		UserExitInfo userExitInfoAlias = null;
		String token = "";
		String str = "";
		String aliasDb2Location = "";
		String aliasOwner = "";
		String aliasName = "";
		String entityOwner = "";
		String entityName = "";
		int iPoint = 0;
		int iPoint2 = -1;
		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		////////////////////////////////////////////////////////////////////
		// Parser
		///////////////////////////////////////////////////////////////////
		
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		this.scn = new Scanner(str);									//  
		token = nextToken(scn);                                         // CREATE
		token = nextToken(scn); 										// ALIAS
		token = nextToken(scn); 							 	        // aliasName|owner.aliasName|db2Location.owner.aliasName
		iPoint = token.indexOf(".");
        if (iPoint > 0) {
        	iPoint2 = token.indexOf(".", iPoint + 1);
		}
        // Nessuna qualificazione
		if (iPoint < 0 && iPoint2 < 0) {
			aliasName = deleteTrailingLeadingApiceIfAny(token);                  
		}
        // Solo owner di qualificazione
		if (iPoint > 0 && iPoint2 <= 0) {
			aliasName = token.substring(iPoint + 1);
			aliasName = deleteTrailingLeadingApiceIfAny(aliasName).trim();  
			aliasOwner = token.substring(0, iPoint);
			aliasOwner = deleteTrailingLeadingApiceIfAny(aliasOwner).trim();  
		}
        // Db2 location e owner di qualificazione
		if (iPoint > 0 && iPoint2 > 0) {
			aliasName = token.substring(iPoint2 + 1);
			aliasName = deleteTrailingLeadingApiceIfAny(aliasName).trim();  
			aliasOwner = token.substring(iPoint, iPoint2);
			aliasOwner = deleteTrailingLeadingApiceIfAny(aliasOwner).trim();
			aliasDb2Location = token.substring(0, iPoint);
			aliasDb2Location = deleteTrailingLeadingApiceIfAny(aliasDb2Location).trim();
		}
		
		token = nextToken(scn); 										// FOR
		token = nextToken(scn); 										// owner.table|view
		iPoint = token.indexOf(".");
		if (iPoint > 0) {
			entityOwner = token.substring(0, iPoint);	
			entityName = token.substring(iPoint + 1).trim();	
			entityName = deleteTrailingLeadingApiceIfAny(entityName); 
			entityOwner = deleteTrailingLeadingApiceIfAny(entityOwner); 
		} else {
			entityName = token;
			entityName = deleteTrailingLeadingApiceIfAny(entityName); 
		}
		
		
		////////////////////////////////////////////////////////////////////
		// Identificazione sistema e sottosistema 
		///////////////////////////////////////////////////////////////////

		userExitInfoEntity = userExitGetSystemSubsystem(this.di, "", entityOwner, entityName);
		userExitInfoAlias  = userExitGetSystemSubsystem(this.di, "", aliasOwner, aliasName);
 
		
		////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////
		
    	// Descriptor istruzione con opzioni e strutture dati
 		instruction.sqlCreateAliasSetName(aliasName);
 		instruction.sqlCreateAliasSetOwner(aliasOwner);
		instruction.sqlCreateAliasSetTableView(entityName);
		instruction.sqlCreateAliasSetTableViewOwner(entityOwner);
   		
 		// Oggetti da inserire a fine analisi
		
		// Owner alias
 		if (!aliasOwner.equals("")) {
  		   eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,  aliasOwner,  EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoAlias);
  	       eo.setSystem(userExitInfoAlias.getSystem());
  	       eo.setSubSystem(userExitInfoAlias.getSubSystem());
           eo.setSystemOwner(userExitInfoAlias.getSystem());
           eo.setSubSystemOwner(userExitInfoAlias.getSubSystem());
		}
 		
 		// Owner entity
 		if (!entityOwner.equals("")) {
   		   eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,  entityOwner,  EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
   	       eo.setSystem(di.systemInput);
   	       eo.setSubSystem(di.subSystemInput);
           eo.setSystemOwner(di.systemInput);
           eo.setSubSystemOwner(di.subSystemInput);
 		}
 		
 		// Alias sul suo sistema/sottosistema proprietario
	    eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_ALIAS,  aliasName,  EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoAlias);
        eo.setSystem(userExitInfoAlias.getSystem());
        eo.setSubSystem(userExitInfoAlias.getSubSystem());
        eo.setSystemOwner(userExitInfoAlias.getSystem());
        eo.setSubSystemOwner(userExitInfoAlias.getSubSystem());
	
        // Alias sul sistema/sottosistema dell'entity che lo utilizza
	    eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_ALIAS,  aliasName,  EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
        eo.setSystem(di.systemInput);
        eo.setSubSystem(di.subSystemInput);
        eo.setSystemOwner(userExitInfoAlias.getSystem());
        eo.setSubSystemOwner(userExitInfoAlias.getSubSystem());
        
        // Entity
        eo = this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL,  entityName,  EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, userExitInfoEntity);
        eo.setSystem(di.systemInput);
        eo.setSubSystem(di.subSystemInput);
  		
		
		// ENTITY_SQL_ALIAS
		er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_ALIAS, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_ALIAS, aliasName, userExitInfoAlias);
		er.setSystem(di.systemInput);
		er.setSubSystem(di.subSystemInput);
		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_ALIAS, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_ALIAS, aliasName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoAlias, instruction);
		ero.setSystem(di.systemInput);
		ero.setSubSystem(di.subSystemInput);
		
		// ENTITY_SQL_OWNER
		er = this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, userExitInfoAlias);
		er.setSystem(di.systemInput);
		er.setSubSystem(di.subSystemInput);
		ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, userExitInfoAlias, instruction);
		ero.setSystem(di.systemInput);
		ero.setSubSystem(di.subSystemInput);
		
	}


	/* ------------------------------------------
	 * Parsing parametri a livello di tabella
	 * ------------------------------------------
	 * 
	 * Se token in input riconosciuti restituisce aggiorna sqlTable
	 * 
	 */
    private void parseParmsTable(InstructionSql instruction, String strToParse, SqlTable sqlTable) {
    	

    	Scanner scn = null;
       	SqlPartitionColumn partitionColumn = null;
       	SqlPartitionValues partitionValue = null;
    	String databaseName = "";
		String tablespaceName = "";
		String token = "";
		String tokenNext = "";
		int numValue = 0;
        int obid = 0;
        int i = 0;
        
        scn = new Scanner(strToParse);
        this.strNoMemoryToParse = strToParse;
        
        token = nextToken(scn);
        
        // Scan token
        while (!token.equals("")) {
		
            // LIKE clause
    		if (token.equals("LIKE")) {
    			token = nextToken(scn);				// tablename/view
    			sqlTable.setLikeTableView(token);	// Può essere table full name (owner.tableName)
    			token = nextToken(scn);
      			continue;
    		}
    		
    		// IN clause
    		if (token.equals("IN")) {
    			token = nextToken(scn);				
    			if (token.equals("DATABASE")) {
    				databaseName = nextToken(scn);
    				sqlTable.setDatabaseName(deleteTrailingLeadingApiceIfAny(databaseName).trim());
    				token = nextToken(scn);
         			continue;
    			}
    			i = token.indexOf(".");
                if (i > 0) {
                	databaseName = token.substring(0, i);
                	tablespaceName = token.substring(i + 1);
                	sqlTable.setDatabaseName(deleteTrailingLeadingApiceIfAny(databaseName).trim());
                	sqlTable.setTablespaceName(deleteTrailingLeadingApiceIfAny(tablespaceName).trim());
    				token = nextToken(scn);
         			continue;
    			}
               	sqlTable.setTablespaceName(deleteTrailingLeadingApiceIfAny(token).trim());
    			token = nextToken(scn);
     			continue;
    		}
    		
    		// EDITPROC clause
    		if (token.equals("EDITPROC")) {
    			token = nextToken(scn);
              	sqlTable.setEditprocProgramName(token);
    			token = nextToken(scn);
     			continue;
    		}
    		
    		// VALIDPROC clause
    		if (token.equals("VALIDPROC")) {
    			token = nextToken(scn);
              	sqlTable.setValidprocProgramName(token);
    			token = nextToken(scn);
     			continue;
    		}
    		
    		// AUDIT clause
    		if (token.equals("AUDIT")) {
    			token = nextToken(scn);
              	sqlTable.setAudit(token);  				// NONE|CHANGES|ALL
    			token = nextToken(scn);
     			continue;
    		}
    		
    		// OBID clause
    		if (token.equals("OBID")) {
    			token = nextToken(scn);
    			if (!StringService._isNumeric(token)) {
    				instruction.setParsingError(true);
    				instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
    				return;
    			}
    			obid = StringService._getNumericInt(token);
              	sqlTable.setObid(obid);
    			token = nextToken(scn);
     			continue;
    		}
    		
    		// DATA CAPTURE clause
    		if (token.equals("DATA")) {
    			token = nextToken(scn);  				// CAPTURE
    			token = nextToken(scn);  				// NONE|CHANGES
              	sqlTable.setDataCapture(token);
    			token = nextToken(scn);
     			continue;
    		}
    		
    		// WITH RESTRICT ON DROP clause
    		if (token.equals("WITH")) {
    			token = nextToken(scn);  				// RESTRICT
    			token = nextToken(scn);  				// ON
    			token = nextToken(scn);  				// DROP
    			sqlTable.setWithRestrictOnDrop(true);
    			token = nextToken(scn);
     			continue;
    		}
    		
    		// CCSID clause
    		if (token.equals("CCSID")) {
    			token = nextToken(scn);  				// ASCII|EBCDIC|UNICODE
    			sqlTable.setCcsId(token);
    			token = nextToken(scn);
     			continue;
    		}
    		
    		// Copy-options, dopo LIKE e (..) AS result-table
    		// INCLUDING
    		if (token.equals("INCLUDING")) {
    			token = nextToken(scn);				 
    			// INCLUDING IDENTITY |COLUMN ATTRIBUTES
    			if (token.equals("IDENTITY")) {
    				token = nextToken(scn);			// COLUMN ? 
    				this.scnNoMemory = new Scanner(this.strNoMemoryToParse);
    				tokenNext = nextTokenNoMemory(this.scnNoMemory);      
    				if (token.equals("COLUMN") && tokenNext.equals("ATTRIBUTES")) {
    					sqlTable.setCopyIncludingIdentityColumnAttributes(true);
    					token = nextToken(scn);		// ATTRIBUTES
    					token = nextToken(scn);
    	     			continue;
    				}
    			}
    			// INCLUDING |COLUMN DEFAULTS
    			if (token.equals("COLUMN")) {
    				token = nextToken(scn);			// COLUMN|DEFAULTS ? 
    				if (token.equals("COLUMN")) {
    					token = nextToken(scn);		// DEFAULTS
    				}
    				sqlTable.setCopyIncludingColumnDefaults(true);
    				token = nextToken(scn);
         			continue;
    			}
    			// INCLUDING ROW CHANGE TIMESTAMP
    			if (token.equals("ROW")) {
    				token = nextToken(scn);			// CHANGE 
    				token = nextToken(scn);			// TIMESTAMP 
    				sqlTable.setCopyIncludingRowChangeTimestamp(true);
    				token = nextToken(scn);
         			continue;
    			}
    		}
    		
    		// Copy-options, dopo LIKE e (..) AS result-table
    		// EXCLUDING
    		if (token.equals("EXCLUDING")) {
    			token = nextToken(scn);				 
    			// EXCLUDING IDENTITY |COLUMN ATTRIBUTES
    			if (token.equals("IDENTITY")) {
    				token = nextToken(scn);			// COLUMN ? 
    				this.scnNoMemory = new Scanner(this.strNoMemoryToParse);
    				tokenNext = nextTokenNoMemory(this.scnNoMemory);      
    				if (token.equals("COLUMN") && tokenNext.equals("ATTRIBUTES")) {
    					token = nextToken(scn);		// ATTRIBUTES
    					sqlTable.setCopyExcludingIdentityColumnAttributes(true);
    					token = nextToken(scn);
    	     			continue;
    				}
    			}
    			// EXCLUDING |COLUMN DEFAULTS
    			if (token.equals("COLUMN")) {
    				token = nextToken(scn);			// COLUMN|DEFAULTS ? 
    				if (token.equals("COLUMN")) {
    					token = nextToken(scn);		// DEFAULTS
    				}
    				sqlTable.setCopyExcludingColumnDefaults(true);
    				token = nextToken(scn);
         			continue;
    			}
    			// EXCLUDING ROW CHANGE TIMESTAMP
    			if (token.equals("ROW")) {
    				token = nextToken(scn);			// CHANGE 
    				token = nextToken(scn);			// TIMESTAMP 
    				sqlTable.setCopyExcludingRowChangeTimestamp(true);
    				token = nextToken(scn);
         			continue;
    			}
    			// EXCLUDING XML TYPE MODIFIERS
    			if (token.equals("XML")) {
    				token = nextToken(scn);			// TYPE 
    				token = nextToken(scn);			// MODIFIERS 
    				sqlTable.setCopyExcludingXmlTypeModifiers(true);
    				token = nextToken(scn);
         			continue;
    			}
    		}
    		// Copy-options, dopo LIKE e (..) AS result-table
    		// USING TYPE DEFAULTS
    		if (token.equals("USING")) {
    			token = nextToken(scn);			// TYPE			 
    			token = nextToken(scn);			// DEFAULTS
    			sqlTable.setCopyUsingTypeDefaults(true);
    			token = nextToken(scn);
     			continue;
    		}
    		
    		// PARTITION
    		if (token.equals("PARTITION")) {
    			sqlTable.setPartitionsDescriptor(new SqlPartitions());
    			token = nextToken(scn);			// BY
    			token = nextToken(scn);     	// SIZE ?
    			// PARTITION BY SIZE
    			if (token.equals("SIZE")) {
    				sqlTable.getPartitionsDescriptor().setPartitionBySize(true);
    				token = nextToken(scn);		// EVERY ?
    				if (token.equals("EVERY")) {
    					token = nextToken(scn);	// integer-constant
    					numValue = StringService._getNumericInt(token);
    					sqlTable.getPartitionsDescriptor().setPartitionBySizeEveryValue(numValue);
    					token = nextToken(scn);	// G
    					token = nextToken(scn);
    				}
         			continue;
    			}
    			// PARTITION BY |RANGE
    			sqlTable.getPartitionsDescriptor().setPartitionByRange(true);
    			if (token.equals("RANGE")) {
    				token = nextToken(scn);		// (
    			}
    			token = nextToken(scn);
    			// Colonne
    			partitionColumn = new SqlPartitionColumn();
    			while (!token.equals("") && !token.equals(")")) {
    				if (token.equals(",")) {
    					token = nextToken(scn);
    					sqlTable.getPartitionsDescriptor().getColumns().add(partitionColumn);
    					partitionColumn = new SqlPartitionColumn();
    					continue;
    				}
    				if (token.equals("NULLS")) {
    					partitionColumn.setNullLast(true);
    					token = nextToken(scn);		// LAST
    					token = nextToken(scn);
    					continue;
    				}
    				if (token.equals("ASC")) {
    					partitionColumn.setOrder(EnumIndexOrder.ORDER_ASCENDING);
    					token = nextToken(scn);
    					continue;
    				}
    				if (token.equals("DESC")) {
    					partitionColumn.setOrder(EnumIndexOrder.ORDER_DESCENDING);
    					token = nextToken(scn);
    					continue;
    				}
    				if (token.equals("RANDOM")) {
    					partitionColumn.setOrder(EnumIndexOrder.ORDER_RANDOM);
    					token = nextToken(scn);
    					continue;
    				}
    				// Può essere solo il nome della colonna
    				partitionColumn.setColumn(token);
    				token = nextToken(scn);
    			}
    			// Ultima colonna da inserire
    			sqlTable.getPartitionsDescriptor().getColumns().add(partitionColumn);
    			token = nextToken(scn);				// (
    			token = nextToken(scn);				// PARTITION
    			partitionValue = new SqlPartitionValues();
    			// Valori
    			while (!token.equals("") && !token.equals(")")) {
    				if (token.equals(",")) {
    					token = nextToken(scn);
    					sqlTable.getPartitionsDescriptor().getValues().add(partitionValue);
    					partitionValue = new SqlPartitionValues();
    					continue;
    				}
    				token = nextToken(scn);             // Partition number
    				numValue = StringService._getNumericInt(token);
    				partitionValue.setPartitionNumber(numValue);
    				token = nextToken(scn);				// ENDING
    				token = nextToken(scn);				// AT ?
    				if (token.equals("AT")) {
    					token = nextToken(scn);			// (
    				}
    				token = nextToken(scn);	
    				while (!token.equals("") && !token.equals(")")) {
    					if (token.equals(",")) {
    						token = nextToken(scn);
    						continue;
    					}
    					partitionValue.getEndingAtValues().add(token);
    					token = nextToken(scn);	
    				}
    				token = nextToken(scn);				// HASH ? INCLUSIVE ?
    				// HASH SPACE
    				if (token.equals("HASH")) {
    					token = nextToken(scn);			// SPACE
    					token = nextToken(scn);			// integer
    					numValue = StringService._getNumericInt(token);
    					partitionValue.setHashSpaceValue(numValue);
    					token = nextToken(scn);			// K|M|G
    					partitionValue.setHashSpaceType(token);
    					token = nextToken(scn);
    				}
    				// INCLUSIVE
    				if (token.equals("INCLUSIVE")) {
    					token = nextToken(scn);
    					partitionValue.setInclusive(true);
    				}
    			} // end-while valori
    			sqlTable.getPartitionsDescriptor().getValues().add(partitionValue);
    			token = nextToken(scn);
				continue;
    		}
    			
    		// ORGANIZE BY HASH UNIQUE
    		if (token.equals("ORGANIZE")) {
    			token = nextToken(scn);			// BY
    			token = nextToken(scn);     	// HASH
    			token = nextToken(scn);     	// UNIQUE
    			token = nextToken(scn);			// (
    			token = nextToken(scn);
    			while (!token.equals("") && !token.equals(")")) {
    				if (token.equals(",")) {
    					token = nextToken(scn);
    					continue;
    				}
    				sqlTable.getColumnsOrganizeByHashUnique().add(token);
    				token = nextToken(scn);
    			}
    			token = nextToken(scn);
    			if (token.equals("HASH")) {
    				token = nextToken(scn);     	// SPACE
    				token = nextToken(scn);     	// integer
    				numValue = StringService._getNumericInt(token);
    				sqlTable.setOrganizeByHashUniqueSpaceValue(numValue);
    				token = nextToken(scn);			// K|M|G
    				sqlTable.setOrganizeByHashUniqueSpaceType(token);
    				token = nextToken(scn);
    			}
					continue;
    		}

    		//////////////////////////////////////////////////////////////////
    		// Opzioni di tabella specifiche per DB2/AIX64 Version 9.1.9
    		//////////////////////////////////////////////////////////////////
    		
    		// COMPRESS 
    		if (token.equals("COMPRESS")) {
    			token = nextToken(scn);      // YES|NO
    			if (token.equals("YES") || token.equals("NO")) {
    				token = nextToken(scn);
    			}
				continue;
    		}
    		
    		// VALUE COMPRESSION 
    		if (token.equals("VALUE") && nextTokenWithoutScannerMod(scn).equals("COMPRESSION")) {
    			token = nextToken(scn);      // COMPRESSION
    			token = nextToken(scn);
				continue;
    		}
    		
    		// INDEX 
    		if (token.equals("INDEX") ) {
    			token = nextToken(scn);      // IN
    			token = nextToken(scn);	     // tablespace
    			token = nextToken(scn);
				continue;
    		}
    		
    		
    		// Parametro non gestito (come NOT VOLATILE)
    		token = nextToken(scn);
        	
		}
 
		return;
	}

	/* -----------------------------------------------------------------------------
	 * Attivazione parsing parametri di vincolo, generici di colonna e di data type
	 * -----------------------------------------------------------------------------
	 * Analizza una singola occorrenza fra parentesi CREATE TABLE tableName (occ1, occn) ..
	 * Se tipo clausola occorrenza riconosciuta aggiorna sqlTable/sqlTableColumn
	 * Altrimenti imposta errore in istruzione imposta errore in istruzione
	 * 
	 */ 
	private void createTableParseParmsOccursed(InstructionSql instruction
				                             , String strToParse
				                             , SqlTable sqlTable 					// Per valori non specifici di colonna
				                              ) {

        Scanner scn = null;
        String token = "";
		boolean isParsingGood = false;
        
 		// Definizine di colonna
		isParsingGood = createTableParseColumnDefinition(instruction, strToParse, sqlTable);
		if (isParsingGood) {return;}

		// Constraint (comune a column definition)
		scn = new Scanner(strToParse);
		this.strNoMemoryToParse = strToParse;
		token = nextToken(scn);
		isParsingGood = createTableParseConstraint(instruction, scn, token, sqlTable, null);
		if (isParsingGood) {return;}

		// Parametro generic, no data type e no constraint 
		isParsingGood = createTableParseParmsOccurredGeneric(instruction, strToParse, sqlTable);
		if (isParsingGood) {return;}
		
        // Clausola non riconosciuta
		instruction.setParsingError(true);
		instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
		
		return;
	}


	/*
	 * -------------------------------------------
     * Parsing definizioni di constraints.
     * -------------------------------------------
     * 
     * Se ci sono errori di parsing si aggiornano i flag nell'istruzione
     * Se non riconosciuto restituisce false.
     * 
     */
	private boolean createTableParseConstraint (InstructionSql instruction
											  , Scanner scn
											  , String tokenStart
											  , SqlTable sqlTable
											  , SqlTableColumn sqlTableColumn) {

		
		ArrayList<String> al_foreignKeyCol = null;
		String constraintName = "";
		String tableFullName = "";
		String token = "";
		String strCheckCondition = "";
		String strBetweenPar = "";
		String ar_strParm[] = null;
		int iLastReferenceTable = 0;
		
		al_foreignKeyCol = new ArrayList<String>();
		
		token = tokenStart;
		
		
		/////////////////////////////////////////////////////////
		// Clausola constraint comune a unique/referential/check
		/////////////////////////////////////////////////////////
		
		// CONSTRAINT clause
		if (token.equals("CONSTRAINT")) {
			constraintName = nextToken(scn);
			token = nextToken(scn);  
		}

		//////////////////////////////////////////////////////////////////////////
		// Unique constraint
		//////////////////////////////////////////////////////////////////////////
		
        // UNIQUE|PRIMARY
		if (token.equals("UNIQUE") || token.equals("PRIMARY")) {
			if (!constraintName.equals("")) {
				sqlTable.setConstraintUniquePrimaryKeyName(constraintName);
			    if (sqlTableColumn != null) {sqlTableColumn.setConstraintName(constraintName);}
			}
			if (token.equals("PRIMARY")) {
				token = nextToken(scn);      				// KEY
				sqlTable.setConstraintPrimaryKey(true);
				if (sqlTableColumn != null) {sqlTableColumn.setConstraintPrimaryKey(true);}
			} else {
				sqlTable.setConstraintUnique(true);
				if (sqlTableColumn != null) {sqlTableColumn.setConstraintUnique(true);}
			}
			token = nextToken(scn); 		 				// ( ?
			if (token.equals("(")) {
				strBetweenPar = extractStringBetweenPar(scn);
			    ar_strParm = splitParms(strBetweenPar);
				// Scan colonne primary key
				for (String primaryKeyCol : ar_strParm) {
					sqlTable.getConstraintUniquePrimaryKeyCols().add(primaryKeyCol);
				}
				token = nextToken(scn); 
			}
			// BUSINESS_TIME WITHOUT OVERLAP (non memorizzato nella struttura)
			if (token.equals("BUSINESS_TIME")) {
				token = nextToken(scn); 					// WITHOUT
				token = nextToken(scn); 					// OVERLAP
				sqlTable.getConstraintUniquePrimaryKeyCols().clear();
				token = nextToken(scn);
				this.nextToken = token;
			}
			return true;
		}


		//////////////////////////////////////////////////////////////////////////
		// Referential constraint (include reference-clause)
		//////////////////////////////////////////////////////////////////////////
		
		// FOREIGN KEY
		if (token.equals("FOREIGN")) {
			sqlTable.setConstraintForeignKey(true);
			if (constraintName.equals("")) {
				constraintName = "*";
			} else {
				sqlTable.setConstraintForeignKey(true);
			}
			// Attivazione a fronte di column-constraint
			if (sqlTableColumn != null) {
				sqlTableColumn.setConstraintName(constraintName);
				if (!constraintName.equals("*")) {sqlTableColumn.setConstraintForeignKey(true);}
			}
			sqlTable.getConstraintForeignKeyNames().add(constraintName);
			sqlTable.getConstraintForeignKeyCols().add(new ArrayList<String> ());
			token = nextToken(scn);      				// KEY
			token = nextToken(scn);      				// (
			strBetweenPar = extractStringBetweenPar(scn);
		    ar_strParm = splitParms(strBetweenPar);
			// Scan colonne foreign key
			for (String foreignKeyCol : ar_strParm) {
				al_foreignKeyCol.add(foreignKeyCol);
			}
			token = nextToken(scn);  
			this.nextToken = token;
		}

		
		//////////////////////////////////////////////////////////////////////////
		// reference-clause
		//////////////////////////////////////////////////////////////////////////
		
		// REFERENCES
		if (token.equals("REFERENCES")) {
			tableFullName = nextToken(scn); 
			sqlTable.getConstraintForeignKeyReferenceTables().add(tableFullName);
			iLastReferenceTable = sqlTable.getConstraintForeignKeyReferenceTables().size() - 1;
			// Inserimento ArrayList per foreign keys e references keys cols
			sqlTable.getConstraintForeignKeyNames().add(constraintName);
		    sqlTable.getConstraintForeignKeyCols().add(new ArrayList<String>());
			sqlTable.getConstraintsForeignKeyReferenceCols().add(new ArrayList<String>());
			sqlTable.getConstraintsForeignOnDelete().add("");
			// Attivazione a fronte di column-constraint
			if (sqlTableColumn != null) {
				sqlTableColumn.setConstraintReferenceTable(tableFullName);
				sqlTableColumn.setConstraintReferenceCol(sqlTableColumn.getColumnName());
				sqlTableColumn.setConstraintName(constraintName);
			}
			// Inserimento foreignKey cols e foreignKeyReference cols
			sqlTable.getConstraintForeignKeyCols().get(iLastReferenceTable).addAll(al_foreignKeyCol);
			token = nextToken(scn); 
			if (token.equals("(")) {
				strBetweenPar = extractStringBetweenPar(scn);
			    ar_strParm = splitParms(strBetweenPar);
				// Scan colonne reference key
				for (String referenceKeyCol : ar_strParm) {
					sqlTable.getConstraintsForeignKeyReferenceCols().get(iLastReferenceTable).add(referenceKeyCol);
				}
				token = nextToken(scn);   
			}
			// ON DELETE
			if (token.equals("ON")) {
				token = nextToken(scn);         // DELETE
				token = nextToken(scn); 		// NO ACTION, SET NULL, RESTRICT, CASCADE  
				if (token.equals("NO")) {
					sqlTable.getConstraintsForeignOnDelete().set(iLastReferenceTable, "NO ACTION");
					token = nextToken(scn);     // ACTION
					token = nextToken(scn); 
					return true;
				} else if (token.equals("SET")) {
					sqlTable.getConstraintsForeignOnDelete().set(iLastReferenceTable, "SET NULL");
					token = nextToken(scn);     // NULL
					token = nextToken(scn); 
					return true;
				} else if (token.equals("RESTRICT")) {
					sqlTable.getConstraintsForeignOnDelete().set(iLastReferenceTable, "RESTRICT");
					token = nextToken(scn);
					this.nextToken = token;
					return true;
				} else if (token.equals("CASCADE")) {
					sqlTable.getConstraintsForeignOnDelete().set(iLastReferenceTable, "CASCADE");
					token = nextToken(scn); 
					this.nextToken = token;
					return true;
				} else {
					instruction.setParsingError(true);
					instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
					return false;
				}
			}
			// ENFORCED|NOT ENFORCED (non memorizzata in struttura colonna)
			if (token.equals("ENFORCED") || (token.equals("NOT") && nextTokenWithoutScannerMod(scn).equals("ENFORCED"))) {
				if (token.equals("NOT")) {
					token = nextToken(scn);     		// ENFORCED
				} 
				token = nextToken(scn);
				this.nextToken = token;
				return true;
			}
			// ENABLE QUERY OPTIMIZATION (non memorizzata in struttura colonna)
			if (token.equals("ENABLE") && nextTokenWithoutScannerMod(scn).equals("QUERY")) {
				token = nextToken(scn);     		// QUERY
				token = nextToken(scn);     		// OPTIMIZATION
				token = nextToken(scn);
				this.nextToken = token;
				return true;
			}
            // Nessun parametro dopo REFERENCES o ON DELETE			
			return true;
		}
		
		//////////////////////////////////////////////////////////////////////////
		// Referential constraint (include reference-clause)
		//////////////////////////////////////////////////////////////////////////
		
		// CHECK( ... )
		if (token.equals("CHECK")) {
			if (constraintName.equals("")) {
				constraintName = "*";
			} else {
				sqlTable.setConstraintCheck(true);
			}
			// Attivazione a fronte di column-constraint
			if (sqlTableColumn != null) {
				sqlTableColumn.setConstraintName(constraintName);
				if (!constraintName.equals("*")) {sqlTableColumn.setConstraintForeignKey(true);}
			}
			sqlTable.getConstraintCheckNames().add(constraintName);
			token = nextToken(scn);      			// ( 
			strCheckCondition = extractStringBetweenPar(scn);
			sqlTable.getConstraintCheckCondition().add(strCheckCondition);
			token = nextToken(scn);
			this.nextToken = token;
			return true;
		}
		
		// Parametro di constraint non riconosciuto
		
		return false;
	}


	/*
     * Parsing definizioni occursate generiche no data type e no constraints.
     * Si tratta di parametri non legati a una singola colonna ma a 
     * tutta la tabella, specìficati nelle definizioni ripetitive
     */
    private boolean createTableParseParmsOccurredGeneric(InstructionSql instruction, String strToParse, SqlTable sqlTable) {

    	Scanner scn = null;
		String token = "";
        String strBetweenPar = "";
        String ar_parm [] = null;
		
		
        scn = new Scanner(strToParse);
        this.strNoMemoryToParse = strToParse;
        
        token = nextToken(scn);
        
        // Scan token
        while (!token.equals("")) {
        	
    		// PERIOD
    		if (token.equals("PERIOD")) {
    			token = nextToken(scn);      					// SYSTEM_TIME|BUSINESS_TIME
    			if (token.equals("SYSTEM_TIME")) {
    				sqlTable.setPeriodSystemTime(true);
    			} else {
    				sqlTable.setPeriodBusinessTime(true);
    			}
    			token = nextToken(scn); 						// (
    			strBetweenPar = extractStringBetweenPar(scn);
    			ar_parm = splitParms(strBetweenPar);
    			for (String periodColumn : ar_parm) {
    				sqlTable.getPeriodColumns().add(periodColumn);
				}
     			token = nextToken(scn);
      			return true;
    		}
		}
        
		return false;
	}

    /* ----------------------------------------------------------------------
     * Normalizzazione stringa con numero decimale seza numeri dopo il punto
     * ----------------------------------------------------------------------
     * 
     * Si tratta di numeri tipo 4. 0. +5.
     * 
     * 
     */
	private String normalizeNumIfNeeds(String token) {
		// Non è da normalizzare
		if (!token.endsWith(".")) {
			return token;
		}
		
		// Aggiungo uno 0 dopo il punto
		
		return token + "0";
	}


	/*
     * Parsing definizioni colonna  
     * 
     * Se non riconosciuto restituisce false.
     */
	private boolean createTableParseColumnDefinition(InstructionSql instruction
												   , String strToParse
												   , SqlTable sqlTable
												   ) {

		
		Scanner scn = null;
		SqlTableColumn sqlTableColumn = null;      // Struttura colonna corrente
		String columnName = "";
		String token = "";
		String strNumberDigits = "";
		String strNumberScale = "";
		String strNumberChar = "";
		String strIntegerCharLargeObject = "";
		String strTypeUnitSize = "";
		String strPrecisionFloat = "";
		String strXmlTypeModifier = "";
		String castFunctionName = "";
		String castFunctionParm = "";
		String fieldProcProgramName = "";
		String identityClauseValue = "";
        String tokenNumericNormalized = "";
        boolean isConstraint = false;
        int numericConstant = 0;
        long numericConstantLong = 0;

	    
	    scn = new Scanner(strToParse);
	    this.strNoMemoryToParse = strToParse;
	    
	    token = nextToken(scn);
	    
	    // Non si tratta della definizione di una colonna
	    if (!isSqlDataType(nextTokenWithoutScannerMod(scn))) {
			return false;
		}
	    
	    
		//////////////////////////////////////////////////////////////////////////////////
		// Column definition
        //////////////////////////////////////////////////////////////////////////////////

	    
	    sqlTableColumn = new SqlTableColumn();
  		columnName = token;					
		sqlTableColumn.setColumnName(deleteTrailingLeadingApiceIfAny(columnName));
		sqlTable.getColumns().add(sqlTableColumn);
		
		token = nextToken(scn);							// Data-Type
		

		//////////////////////////////////////////////////////////////////////////////////
		// (1) Analisi data type sql
        //////////////////////////////////////////////////////////////////////////////////
		
		// SMALLINT
		if (token.equals("SMALLINT")) {
			token = nextToken(scn);  
			sqlTableColumn.setDataType(EnumDataItemType.SQL_SMALLINT);
		
		// INTEGER	
		} else if (token.equals("INTEGER") || token.equals("INT")) {
			token = nextToken(scn);  
			sqlTableColumn.setDataType(EnumDataItemType.SQL_INTEGER);
		
		// BIGINT
		} else if (token.equals("BIGINT")) {
			token = nextToken(scn);  
			sqlTableColumn.setDataType(EnumDataItemType.SQL_BIGINT);
		
		// DECIMAL
		} else if (token.equals("DECIMAL") || token.equals("DEC") || token.equals("NUMERIC")) {
			token = nextToken(scn);						// ( ?
			strNumberDigits = "0";
			strNumberScale = "0";
			if (token.equals("(")) {
				token = nextToken(scn);					// digits
				strNumberDigits = token;
				token = nextToken(scn);					// , ) ?		
				if (token.equals(",")) {
					token = nextToken(scn);				// scale
					strNumberScale = token;
					token = nextToken(scn);    			// )
				}
				token = nextToken(scn); 
			}
			sqlTableColumn.setDataType(EnumDataItemType.SQL_DECIMAL);
			sqlTableColumn.setNumDigit(StringService._getNumericInt(strNumberDigits));
			sqlTableColumn.setScale(StringService._getNumericInt(strNumberScale));

		// FLOAT
		} else if (token.equals("FLOAT")) {
			token = nextToken(scn);						// ( ?
			strPrecisionFloat = "0";
			if (token.equals("(")) {
				token = nextToken(scn);					// integer
				strPrecisionFloat = token;
				token = nextToken(scn); 				// )
				token = nextToken(scn);
			}
			sqlTableColumn.setDataType(EnumDataItemType.SQL_FLOAT);
			sqlTableColumn.setPrecision(StringService._getNumericInt(strPrecisionFloat));
		
		// REAL
		} else if (token.equals("REAL")) {
			token = nextToken(scn);						//  
			strPrecisionFloat = "0";
			sqlTableColumn.setDataType(EnumDataItemType.SQL_REAL);
			sqlTableColumn.setPrecision(StringService._getNumericInt(strPrecisionFloat));
			
		// DOUBLE
		} else if (token.equals("DOUBLE")) { 
			token = nextToken(scn);						// (|PRECISION ?
			if (token.equals("PRECISION")) {
				token = nextToken(scn);
			}
			strPrecisionFloat = "0";
			sqlTableColumn.setDataType(EnumDataItemType.SQL_DOUBLE);
			sqlTableColumn.setPrecision(StringService._getNumericInt(strPrecisionFloat));
			
		// DECFLOAT
		} else if (token.equals("DECFLOAT")) {
			token = nextToken(scn);						// (
			strPrecisionFloat = "0";
			if (token.equals("(")) {
				token = nextToken(scn);					// integer
				strPrecisionFloat = token;
				token = nextToken(scn); 				// )
				token = nextToken(scn);
			}
			sqlTableColumn.setDataType(EnumDataItemType.SQL_DECFLOAT);
			sqlTableColumn.setPrecision(StringService._getNumericInt(strPrecisionFloat));
		
		// BYNARY VARYING o VARBINARY
		} else if (token.equals("VARBINARY") || (token.equals("BINARY") && nextTokenWithoutScannerMod(scn).equals("VARYING"))) {
			if (token.equals("BINARY")) {
				token = nextToken(scn);             	// VARYING
			}
			token = nextToken(scn);						// (
			token = nextToken(scn);             		// integer
			strNumberDigits = token;
			token = nextToken(scn); 					// )
			token = nextToken(scn);
			sqlTableColumn.setDataType(EnumDataItemType.SQL_VARBINARY);
			sqlTableColumn.setNumDigit(StringService._getNumericInt(strNumberDigits));
		
		// BYNARY
		} else if (token.equals("BINARY") && !nextTokenWithoutScannerMod(scn).equals("VARYING") && !nextTokenWithoutScannerMod(scn).equals("LARGE") ) {
			token = nextToken(scn);					// ( ?
			strNumberDigits = "0";
			if (token.equals("(")) {
				token = nextToken(scn);				// integer
				strNumberChar = token;
				token = nextToken(scn); 			// )
				token = nextToken(scn);             // FOR ?
			}
			sqlTableColumn.setDataType(EnumDataItemType.SQL_BINARY);
			sqlTableColumn.setNumDigit(StringService._getNumericInt(strNumberDigits));
			
		// BLOB	
		} else if ( token.equals("BLOB") || (token.equals("BINARY"))) {
			if (token.equals("BINARY")) {
				token = nextToken(scn);				// LARGE
				token = nextToken(scn);				// OBJECT
			}
			token = nextToken(scn);           		// (
			strIntegerCharLargeObject = "0";
			strTypeUnitSize = "";
			if (token.equals("(")) {
				token = nextToken(scn);             // integer
				strIntegerCharLargeObject = token;
				token = nextToken(scn); 			// K|M|G| o )
				if (!token.equals(")")) {
					strTypeUnitSize = token;
					token = nextToken(scn);     	// )
					token = nextToken(scn);         //  
				} else {
					token = nextToken(scn); 
				}
			} 
			sqlTableColumn.setDataType(EnumDataItemType.SQL_BLOB);
			sqlTableColumn.setNumChar(StringService._getNumericInt(strIntegerCharLargeObject));
			sqlTableColumn.setTypeUnitSize(strTypeUnitSize);
		
		// CHARACTER
		} else if ((token.equals("CHARACTER") || token.equals("CHAR")) && (!nextTokenWithoutScannerMod(scn).equals("VARYING") && !nextTokenWithoutScannerMod(scn).equals("LARGE"))) {
			token = nextToken(scn);
			strNumberChar = "1";
			if (token.equals("(")) {
				token = nextToken(scn);
				strNumberChar = token;
				token = nextToken(scn); 			// )
				token = nextToken(scn);             // FOR ?
			}
			sqlTableColumn.setDataType(EnumDataItemType.SQL_CHAR);
			sqlTableColumn.setNumChar(StringService._getNumericInt(strNumberChar));
			
		// LONG VARCHAR (old syntax for VARCHAR)	
		} else if ((token.equals("LONG") && nextTokenWithoutScannerMod(scn).equals("VARCHAR"))) {
			token = nextToken(scn);			 		// VARCHAR
			token = nextToken(scn);                 // FOR ?
			sqlTableColumn.setDataType(EnumDataItemType.SQL_VARCHAR);
			sqlTableColumn.setNumChar(0);
		
		// VARCHAR
		} else if  (token.equals("VARCHAR") ) {
			token = nextToken(scn);			 		// (
			token = nextToken(scn);          		// integer
			strNumberChar = token;				    //
			token = nextToken(scn); 				// )
			token = nextToken(scn);                 // FOR ?
			sqlTableColumn.setDataType(EnumDataItemType.SQL_VARCHAR);
			sqlTableColumn.setNumChar(StringService._getNumericInt(strNumberChar));
		
		// VARCHAR
		} else if  ((token.equals("CHAR") || token.equals("CHARACTER")) && nextTokenWithoutScannerMod(scn).equals("VARYING")) {
			token = nextToken(scn);      			// VARYING
			token = nextToken(scn);      			// (
			strNumberChar = nextToken(scn);   		// integer
			token = nextToken(scn);                 // )
			token = nextToken(scn);                 // FOR ?
			sqlTableColumn.setDataType(EnumDataItemType.SQL_VARCHAR);
			sqlTableColumn.setNumChar(StringService._getNumericInt(strNumberChar));
		
		// CLOB
		} else if ( token.equals("CLOB") || (token.equals("CHAR") || token.equals("CHARACTER"))) {
			if (token.equals("CHAR") || token.equals("CHARACTER")) {
				token = nextToken(scn);				// LARGE
				token = nextToken(scn);				// OBJECT
			}
			token = nextToken(scn);					// ( ?
			strIntegerCharLargeObject = "0";
			strTypeUnitSize = "";
			if (token.equals("(")) {
				token = nextToken(scn);             // integer
				strIntegerCharLargeObject = token;
				token = nextToken(scn); 			// K|M|G| o )
				if (!token.equals(")")) {
					strTypeUnitSize = token;
					token = nextToken(scn);     	// )
					token = nextToken(scn);         // FOR ?
				} else {
					token = nextToken(scn); 
				}
				
			} 
			sqlTableColumn.setDataType(EnumDataItemType.SQL_CLOB);
			sqlTableColumn.setNumChar(StringService._getNumericInt(strIntegerCharLargeObject));
			sqlTableColumn.setTypeUnitSize(strTypeUnitSize);
			
		// GRAPHIC
		} else if (token.equals("GRAPHIC")) {
			token = nextToken(scn);					// ( ?
			strNumberChar = "0";
			if (token.equals("(")) {
				token = nextToken(scn);				// integer
				strNumberChar = token;
				token = nextToken(scn); 			// )
				token = nextToken(scn);
			} 
			sqlTableColumn.setDataType(EnumDataItemType.SQL_GRAPHIC);
			sqlTableColumn.setNumChar(StringService._getNumericInt(strNumberChar));
		
		// LONG VARGRAPHIC (old syntax for VARCHAR)
		} else if (token.equals("LONG") && nextTokenWithoutScannerMod(scn).equals("VARGRAPHIC")) {
			token = nextToken(scn);			 		// VARGRAPHIC
			token = nextToken(scn);                 // FOR ?
			sqlTableColumn.setDataType(EnumDataItemType.SQL_VARGRAPHIC);
			sqlTableColumn.setNumChar(0);
		
		// VARGRAPHIC
		} else if (token.equals("VARGRAPHIC")) {
			token = nextToken(scn); 				// (
			strNumberChar = "0";
			token = nextToken(scn);					// integer
			strNumberChar = token;
			token = nextToken(scn); 				// )
			token = nextToken(scn);                 // FOR ?
			sqlTableColumn.setDataType(EnumDataItemType.SQL_VARGRAPHIC);
			sqlTableColumn.setNumChar(StringService._getNumericInt(strNumberChar));
		
		// DBCLOB
		} else if (token.equals("DBCLOB") ) {
			token = nextToken(scn);					// ( ?
			strNumberChar = "0";
			strTypeUnitSize = "";
			if (token.equals("(")) {
				token = nextToken(scn);				// integer
				strNumberChar = token;
				token = nextToken(scn); 			// )
				if (!token.equals(")")) {
					strTypeUnitSize = token;
					token = nextToken(scn);     	// )
					token = nextToken(scn);
				}
			} 
			sqlTableColumn.setDataType(EnumDataItemType.SQL_DBCLOB);
			sqlTableColumn.setNumChar(StringService._getNumericInt(strNumberChar));
			sqlTableColumn.setTypeUnitSize(strTypeUnitSize);
			
		// DATE
		} else if (token.equals("DATE") ) {
			token = nextToken(scn);
			sqlTableColumn.setDataType(EnumDataItemType.SQL_DATE);
		
		// TIME
		} else if (token.equals("TIME") ) {
			token = nextToken(scn);
			sqlTableColumn.setDataType(EnumDataItemType.SQL_TIME);
		
		// TIMESTAMP
		} else if (token.equals("TIMESTAMP") ) {
			token = nextToken(scn);					// ( ?
			strNumberChar = "0";
			if (token.equals("(")) {
				token = nextToken(scn);				// integer
				strNumberChar = token;
				token = nextToken(scn); 			// )
				token = nextToken(scn);
			} 
			sqlTableColumn.setNumChar(StringService._getNumericInt(strNumberChar));
			sqlTableColumn.setDataType(EnumDataItemType.SQL_TIMESTAMP);
		
		// ROWID
		} else if (token.equals("ROWID") ) {
			token = nextToken(scn);
			sqlTableColumn.setDataType(EnumDataItemType.SQL_ROWID);
			
		// XML
		} else if (token.equals("XML") ) {
			token = nextToken(scn);					// (
			token = nextToken(scn);
			while (!token.equals("") && !token.equals(")")) {
				strXmlTypeModifier = strXmlTypeModifier + " " + token;
				token = nextToken(scn);
			}
			sqlTableColumn.setXmlTypeModifier(strXmlTypeModifier);
			sqlTableColumn.setDataType(EnumDataItemType.SQL_XML);
			token = nextToken(scn);
		} else {
			
			// Errore di programma
			instruction.setParsingError(true);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
		} 
		

		//////////////////////////////////////////////////////////////////////////////////
		// (2) Analisi opzioni definizione colonna dopo nome e data type
        //////////////////////////////////////////////////////////////////////////////////
		

        // Scan token
		while (!token.equals("")) {
			
	        // FOR SBCS|MIXED|BIT DATA
			if (token.equals("FOR")) {  			// 
				token = nextToken(scn);             // SBCS|MIXED|BIT 
				sqlTableColumn.setForData(token);
				token = nextToken(scn);       		// DATA
				token = nextToken(scn);
				continue;
			}

			// NOT NULL
			if (token.equals("NOT") && nextTokenWithoutScannerMod(scn).equals("NULL")) {
				token = nextToken(scn);      // NULL
				token = nextToken(scn);
				sqlTableColumn.setNotNull(true);
				continue;
			}

			// WITH DEFAULT
			if ((token.equals("WITH") && nextTokenWithoutScannerMod(scn).equals("DEFAULT")) || token.equals("DEFAULT") ) {
				if (token.equals("WITH")) {
					token = nextToken(scn);      // DEFAULT
					sqlTableColumn.setWithDefault(true);
				}
				castFunctionName = "";
				castFunctionParm = "";
				token = nextToken(scn);          // nothing / Constant / USER / CURRENT SQLID / NULL / cast-function-name( ... )
				if (token.equals("")) {continue;}
				if (nextTokenWithoutScannerMod(scn).equals("(")) {
					castFunctionName = token;
					token = nextToken(scn);	                           // (
					token = nextToken(scn);
					while (!token.equals("") && !token.equals(")")) {
						castFunctionParm = castFunctionParm + " " + token;
						token = nextToken(scn);
					}
					sqlTableColumn.setWithDefaultCastFunctionName(castFunctionName);
					sqlTableColumn.setWithDefaultCastFunctionValue(castFunctionParm);
					token = nextToken(scn);
					continue;

				} else if (token.equals("USER") || token.equals("SESSION_USER")) {
					sqlTableColumn.setWithDefaultUser(true);
					sqlTableColumn.setWithDefaultValue(token);
					token = nextToken(scn);
				} else if (token.equals("NULL")) {
					sqlTableColumn.setWithDefaultNull(true);
					sqlTableColumn.setWithDefaultValue(token);
					token = nextToken(scn);
				} else if (token.equals("CURRENT")) {
					token = nextToken(scn); 						// SQLID    
					sqlTableColumn.setWithDefaultCurrentSqlId(true);
					sqlTableColumn.setWithDefaultValue("CURRENT SQLID");
					token = nextToken(scn);
				} else if (!token.equals(",")){
					tokenNumericNormalized = normalizeNumIfNeeds(token);
					if (StringService._isLiteral(token) || StringService._isNumeric(tokenNumericNormalized)) {
						sqlTableColumn.setWithDefaultConstant(true);
						sqlTableColumn.setWithDefaultValue(deleteTrailingLeadingApiceIfAny(token));
						token = nextToken(scn);
					}
				} else {
					// Default non gestito o inizio di nuova clausola
					token = nextToken(scn);
				}
				continue;
			}
			
			// GENERATED
			if (token.equals("GENERATED")) {
				token = nextToken(scn);      							// ALWAYS|BY DEFAULT
				// BY DEFAULT 
				if (token.equals("BY")) {
					token = nextToken(scn);  							// DEFAULT
					token = nextToken(scn);
					sqlTableColumn.setGeneratedByDefault(true);
				// ALWAYS
				} else if (token.equals("ALWAYS")) {
					token = nextToken(scn);
					sqlTableColumn.setGeneratedAlwais(true);
				}
				// FOR EACH ROW ON UPDATE AS ROW CHANGE TIMESTAMP
				if (token.equals("FOR") && nextTokenWithoutScannerMod(scn).equals("EACH")) {
					token = nextToken(scn);								// EACH
					token = nextToken(scn);								// ROW
					token = nextToken(scn);								// ON
					token = nextToken(scn);								// UPDATE
					token = nextToken(scn);								// AS
					token = nextToken(scn);								// ROW
					token = nextToken(scn);								// CHANGE
					token = nextToken(scn);								// TIMESTAMP
					sqlTableColumn.setGeneratedForEachRowOnUpdate(true);
					token = nextToken(scn);
					continue;
				}
				// AS IDENTITY
				if (token.equals("AS") && nextTokenWithoutScannerMod(scn).equals("IDENTITY")) {
					token = nextToken(scn);  							// IDENTITY
					token = nextToken(scn);  							// ( ?
					identityClauseValue = "";
					sqlTableColumn.setGeneratedAsIdentity(true);
				    if (token.equals("(")) {
						token = nextToken(scn);
						while (!token.equals("") && !token.equals(")")) {
							identityClauseValue = identityClauseValue + " " + token;
							
							if (token.equals("START")) {
								token = nextToken(scn);    				 // WITH
								token = nextToken(scn);    				 // start-value
								numericConstant = StringService._getNumericInt(token);
								sqlTableColumn.setGeneratedAsIdentityStartWith(numericConstant);
							} else if (token.equals("INCREMENT")) {
								token = nextToken(scn);     			 // BY
								token = nextToken(scn);    				 // increment
								numericConstant = StringService._getNumericInt(token);
								sqlTableColumn.setGeneratedAsIdentityIncrBy(numericConstant);
							} else if (token.equals("MAXVALUE")) {
								token = nextToken(scn);    				 // max-value
								numericConstantLong = StringService._getNumericLong(token);
								sqlTableColumn.setGeneratedAsIdentityIncrMaxValue(numericConstantLong);
							} else if (token.equals("MINVALUE")) {
								token = nextToken(scn);    				 // min-value
								numericConstantLong = StringService._getNumericInt(token);
								sqlTableColumn.setGeneratedAsIdentityIncrMinValue(numericConstantLong);
							} else if (token.equals("CYCLE")) {
								sqlTableColumn.setGeneratedAsIdentityCycle(true);
							} else if (token.equals("NO") && nextTokenWithoutScannerMod(scn).equals("CYCLE")) {
								token = nextToken(scn);     			// CYCLE
								sqlTableColumn.setGeneratedAsIdentityNoCycle(true);
							} else if (token.equals("NO") && nextTokenWithoutScannerMod(scn).equals("CACHE")) {
								token = nextToken(scn);     			// CACHE
								sqlTableColumn.setGeneratedAsIdentityNoCache(true);
							} else if (token.equals("CACHE")) {
								token = nextToken(scn);    				 // cache
								numericConstant = StringService._getNumericInt(token);
								sqlTableColumn.setGeneratedAsIdentityCache(numericConstant);
							} else {
								// parametro non gestito 
							}
								
							token = nextToken(scn);
							if (token.equals(",")) {
								token = nextToken(scn);
							}
						}
					    sqlTableColumn.setGeneratedAsIdentityValue(identityClauseValue);
					    token = nextToken(scn);
					}
					continue;
				}
				// AS ROW BEGIN|END
				if (token.equals("AS") && nextTokenWithoutScannerMod(scn).equals("ROW")) {
					token = nextToken(scn);								// ROW
					token = nextToken(scn);								// BEGIN|END
					if (token.equals("BEGIN")) {
						sqlTableColumn.setGeneratedAsRowBegin(true);
					} else {
						sqlTableColumn.setGeneratedAsRowEnd(true);
					}
					token = nextToken(scn);
					continue;
				}
				// AS TRANSACTION START ID
				if (token.equals("AS") && nextTokenWithoutScannerMod(scn).equals("TRANSACTION")) {
					token = nextToken(scn);								// TRANSACTION
					token = nextToken(scn);								// START
					token = nextToken(scn);								// ID
					sqlTableColumn.setGeneratedAsTransactionStartId(true);
					token = nextToken(scn);
					continue;
				}
				// FOR EACH ROW ON UPDATE AS ROW CHANGE TIMESTAMP
				if (token.equals("FOR") && nextTokenWithoutScannerMod(scn).equals("EACH")) {
					token = nextToken(scn);								// EACH
					token = nextToken(scn);								// ROW
					token = nextToken(scn);								// ON
					token = nextToken(scn);								// UPDATE
					token = nextToken(scn);								// AS
					token = nextToken(scn);								// ROW
					token = nextToken(scn);								// CHANGE
					token = nextToken(scn);								// TIMESTAMP
					sqlTableColumn.setGeneratedForEachRowOnUpdate(true);
					token = nextToken(scn);
					continue;
				}
				// Nessun parametro dopy ALWAYS|BY DEFAULT
				continue;
			}

			// FIELDPROC
			if (token.equals("FIELDPROC")) {
				token = nextToken(scn);      					// program-name
				fieldProcProgramName = token;
				sqlTableColumn.setFieldProcProgramName(fieldProcProgramName);
				token = nextToken(scn);							// ( ?
				if (token.equals("(")) {
					token = nextToken(scn);
					while (!token.equals("") && !token.equals(")")) {
						if (token.equals(",")) {
							token = nextToken(scn);
						    continue;
						}
						sqlTableColumn.getFieldProcProgramParms().add(token);
						token = nextToken(scn);
					}
					
					token = nextToken(scn);
				}
				continue;
			}

			// AS SECURITY LABEL
			if (token.equals("AS")) {
				token = nextToken(scn);      					// SECURITY
				token = nextToken(scn);      					// LABEL
				token = nextToken(scn); 
				sqlTableColumn.setAsSecurityLabel(true);
				continue;
			}

			// IMPLICITLY HIDDEN
			if (token.equals("IMPLICITLY")) {
				token = nextToken(scn);      					// HIDDEN
				token = nextToken(scn); 
				sqlTableColumn.setImplicitlyHidden(true);
				continue;
			}
			// INLINE LENGTH
			if (token.equals("INLINE")) {
				token = nextToken(scn);      					// LENGTH
				token = nextToken(scn);                         // integer
				sqlTableColumn.setInlineLength(StringService._getNumericInt(token));
				token = nextToken(scn); 
				continue;
			}

			// WITH TIME ZONE
			if (token.equals("WITH") && nextTokenWithoutScannerMod(scn).equals("TIME")) {
				token = nextToken(scn);      					// TIME
				token = nextToken(scn); 						// ZONE
				token = nextToken(scn);
				sqlTableColumn.setWithTimeZone(true);
				continue;
			}

			// WITHOUT TIME ZONE
			if (token.equals("WITHOUT") && nextTokenWithoutScannerMod(scn).equals("TIME")) {
				token = nextToken(scn);      					// TIME
				token = nextToken(scn); 						// ZONE
				token = nextToken(scn);
				sqlTableColumn.setWithTimeZone(false);
				continue;
			}

			//////////////////////////////////////////////////////////////////////////
			// Parsing constraints unificato
			//////////////////////////////////////////////////////////////////////////
			
			isConstraint = createTableParseConstraint(instruction, scn, token, sqlTable, sqlTableColumn);
			if (isConstraint) {
				token = this.nextToken;
				continue;
			}
			
			//////////////////////////////////////////////////////////////////////////
			// Opzioni specifiche per DB2 su AIX (DB2/AIX64 Version 9.1.9)
			//////////////////////////////////////////////////////////////////////////
			
			// NOT LOGGED  
			if (token.equals("NOT") && nextTokenWithoutScannerMod(scn).equals("LOGGED")) {
				token = nextToken(scn);      // LOGGED
				token = nextToken(scn);
				continue;
			}
			 
			// LOGGED  
			if (token.equals("LOGGED")) {
				token = nextToken(scn);
				continue;
			}

			// NOT COMPACT  
			if (token.equals("NOT") && nextTokenWithoutScannerMod(scn).equals("COMPACT")) {
				token = nextToken(scn);      // COMPACT
				token = nextToken(scn);
				continue;
			}

			// COMPACT  
			if (token.equals("COMPACT")) {
				token = nextToken(scn);
				continue;
			}

            // Clausola non gestita
			
			
		}
		
 		return true;
	}

	
	/*
	 * Restituisce se inizia con un data type sql
	 * 
	 */
	private boolean isSqlDataType(String token) {
		if (token.equals("SMALLINT")
		||  token.equals("INTEGER")		
		||  token.equals("INT")		
		||  token.equals("BIGINT")		
		||  token.equals("DECIMAL")		
		||  token.equals("DEC")		
		||  token.equals("NUMERIC")		
		||  token.equals("FLOAT")		
		||  token.equals("REAL")		
		||  token.equals("DOUBLE")		
		||  token.equals("DECFLOAT")		
		||  token.equals("BLOB")		
		||  token.equals("BINARY")		
		||  token.equals("VARBINARY")		
		||  token.equals("CHARACTER")		
		||  token.equals("CHAR")		
		||  token.equals("LONG")		
		||  token.equals("VARCHAR")		
		||  token.equals("CLOB")		
		||  token.equals("GRAPHIC")		
		||  token.equals("VARGRAPHIC")		
		||  token.equals("DBCLOB")		
		||  token.equals("DATE")		
		||  token.equals("TIME")		
		||  token.equals("TIMESTAMP")		
		||  token.equals("ROWID")		
		||  token.equals("XML")	) {
			return true;
		}
		return false;
	}

	/* -------------------------------------------------
	 * Operazioni iniziali sul programma da analizzare
	 * -------------------------------------------------
	 * 
	 */
	private void initialOperationsScript(SourceInput si) throws Exception {
		
		EntityObject entityObject = null;
        String strSql = "";
        
		// Initial ora di inizio e reset errori
		this.di.curTimeMsStart = System.currentTimeMillis();
		this.di.curObjectWithErrors = false;
		
		// Descrittore sorgente e nome sorgente
		this.si = si;
		this.ictx.si = si;
		this.ictx.ar_RowsSource = this.si.getArrayRowSource();
		this.di.curObjectId = scriptSqlName;
		this.di.curObjectType = EnumObject.OBJECT_SQL_SCRIPT;
		this.scriptSql = new ScriptSql(ucfg, scriptSqlName);
		
		// Metriche sript Sql
		this.metricsScriptSql = new Metrics(ucfg, di);
		this.metricsScriptSql.setSystem(di.systemInput);
		this.metricsScriptSql.setSubSystem(di.systemInput);
		this.metricsScriptSql.setScope(EnumMetricsScope.SCOPE_LEVEL_OBJECT);
		this.metricsScriptSql.setTypeObject(EnumObject.OBJECT_SQL_SCRIPT);
    	this.metricsScriptSql.setIdObject(scriptSqlName);
		this.metricsScriptSql.setSection("*");
		
		// Impostazioni per oggetti di aggregazione da inserire a fine elaborazione, solo se dichiarati esplicitamente
		if (!di.systemInput.equals("")) {
			entityObject = new EntityObject();
			entityObject.setSystem("*");   						 
			entityObject.setSubSystem("*");    
			entityObject.setTypeObject(EnumObject.OBJECT_SYS);
			entityObject.setIdObject(di.systemInput);
			this.analyzerDbInfo.addObjEntity(entityObject);
		}
		
		if (!di.systemInput.equals("")) {
			entityObject = new EntityObject();
			entityObject.setSystem("*");   						 
			entityObject.setSubSystem("*");    
			entityObject.setTypeObject(EnumObject.OBJECT_SUBSYS);
			entityObject.setIdObject(di.systemInput);
			this.analyzerDbInfo.addObjEntity(entityObject);
		}
		
		if (!di.systemInput.equals("") && !di.systemInput.equals("")) {
			entityObject = new EntityObject();
			entityObject.setSystem("*");   						 
			entityObject.setSubSystem("*");    
			entityObject.setTypeObject(EnumObject.OBJECT_SYS_SUBSYS);
			entityObject.setIdObject(di.systemInput + "-" + di.systemInput);
			this.analyzerDbInfo.addObjEntity(entityObject);
		}
		
        // Impostazioni per oggetto script sql da inserire/aggiornare a fine elaborazione
	
		entityObject = new EntityObject();
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem(di.subSystemInput);    
		entityObject.setTypeObject(EnumObject.OBJECT_SQL_SCRIPT);
		entityObject.setIdObject(this.scriptSqlName);
		
        // Impostazioni come se il programma fosse alla sua prima analisi
		entityObject.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);  // Status di default
		entityObject.setFileSource(di.filePathCurObj);   	   			          // Nome file
		entityObject.setLibrarySourceObject(di.libraryCodeCurObj);   	   		  // Nome oggetto LIBRARY libreria sorgente di analisi
		entityObject.setLibrarySource(di.libraryPathCurObj);         			  // Libreria sorgente di analisi (Per oggetti direttamente dedotti da un file sorgente)
   		entityObject.setDtFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
		entityObject.setTmFirstAnalysis(DateTimeService.getTimeFormatted(new Date(), "hhmmss") + "00");
   		entityObject.setDtLastAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
		entityObject.setTmLastAnalysis(DateTimeService.getTimeFormatted(new Date(), "hhmmss") + "00");

		// Inserito/aggiornato a fine elaborazione
		idxScriptInDbObject = this.analyzerDbInfo.addObjEntity(entityObject);
 		
		// Sql delete preliminare delle origini relazioni originate da questo script
 		strSql = "DELETE FROMRelationOrigin WHERE   sys = '" + di.systemInput + "'" 					 +
								    " AND   subSys = '" + di.subSystemInput + "'" 					 +
							        " AND   RELOIDOO = '" + this.scriptSqlName + "'" +
							        " AND   RELOTYOO =  " + EnumObject.OBJECT_SQL_SCRIPT.ordinal();
 		this.analyzerDbInfo.addSqlDeleteStatement(strSql);
 	}

	
	/*
     * ----------------------------------
     * Analisi statement Select.
     * ----------------------------------
     * 
     * 1) WITH common-table-identifiers
     * 2) full-select
     * 3) options
     * 
     * 
     */
	private SqlSelectStatement analyzeSqlSelect(InnerContextAnalysis ictx, InstructionSql instruction) throws SQLException, ExceptionAmrita  {
		
		Scanner scn = null;
		ArrayList<SqlCommonTableExpression> al_commonTableExpression = null;
        SqlSelectStatement sqlSelect = null;
		SqlFullSelect fullselect = null;
		StringBuffer sbCommonTableExpressions = null;
		String strBetweenPar = "";
		String str = "";
		String strFullSelect = "";
		String strFullSelectAndOptions = "";
		String strOptions = "";
		String strCommonTableExpressions = "";
		String token = "";
		int iOptions = 0;
     		
		// Allocazione strutture di lavoro
        sqlSelect = new SqlSelectStatement();
        fullselect = new SqlFullSelect();
        sbCommonTableExpressions = new StringBuffer();
        
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		////////////////////////////////////////////////////////////////////
		// Estrazione sezioni istruzione per parser
		//  1) WITH common-table-descriptor
		//  2) full-select
		//  3) Opzioni select
		///////////////////////////////////////////////////////////////////
		
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		scn = new Scanner(str);											//  
 
		token = nextToken(scn);											// WITH ?
		
 		// WITH common-table-expression: estrazione completa
		if (token.equals("WITH")) {
			token = nextToken(scn);										// table-identifier
			while (!token.equals("")) {
				sbCommonTableExpressions.append(" " + token);
				if (token.equals("AS")) {
					token = nextToken(scn);          					// (
					strBetweenPar = extractStringBetweenPar(scn);
					sbCommonTableExpressions.append(" (" + strBetweenPar + " )");
					token = nextToken(scn);          					// ,|table-identifier|SELECT|(
					// Inizio full-select: fine common-table-expression
					if (token.equals("SELECT") || token.equals("(")) {
						break;
					}
					continue;
				}  
				token = nextToken(scn);									 
			} // end-while common-table-expression
		}
		
		// Può essere solo full-select
		if (!token.equals("SELECT") && !token.equals("(")) {
			instruction.setParsingError(true);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
			return sqlSelect;
		}
		
		// Full select, corpo dell'istruzione select, pointer a inizio opzioni select 
		strFullSelectAndOptions = token + " " + this.strNoMemoryToParse;
		iOptions = analyzeSqlSelectGetStartOptions(strFullSelectAndOptions);
		
		// Estrazione opzioni
		if (iOptions > 0) {
			strOptions = strFullSelectAndOptions.substring(iOptions).trim();
			strFullSelect = strFullSelectAndOptions.substring(0, iOptions).trim();
		} else {
			strFullSelect = strFullSelectAndOptions.trim();
		}

		////////////////////////////////////////////////////////////////////
		// Parser
		///////////////////////////////////////////////////////////////////
		
		al_commonTableExpression = analyzeSqCommonTableExpressions(ictx, instruction, strCommonTableExpressions);
		fullselect = analyzeSqlFullselect(ictx, instruction, strFullSelect);
		analyzeSqlSelectParseOptions(ictx, instruction, sqlSelect, strOptions);
				
		// Parsing error
		if (instruction.isParsingError()) {
			return sqlSelect;
		}
		
		////////////////////////////////////////////////////////////////////////////////////
		// Impostazione sotto strutture analizzate e caricamento descriptor in istruzione
		////////////////////////////////////////////////////////////////////////////////////
		
		sqlSelect.setFullSelect(fullselect);
		sqlSelect.setCommonTableExpressions(al_commonTableExpression);
    	instruction.sqlSelectSetDescriptor(sqlSelect);	

    	
		///////////////////////////////////////////////////////////////////////////
		// Caricamento simboli nell'istruzione e predisposizione per Updates finali
		////////////////////////////////////////////////////////////////////////////	
		
       	analyzeSqlSelectStoreSymbolsOnInstruction(instruction, sqlSelect);
       	analyzeSqlSelectDbUpdates(instruction, sqlSelect);
  		
 		return sqlSelect;
	}
	
	
    /* ------------------------------------------------------------------
     * Caricamento simboli in istruzione a fronte di SqlSelect statement
     * ------------------------------------------------------------------
     * 
     */
	private void analyzeSqlSelectStoreSymbolsOnInstruction(InstructionSql instruction, SqlSelectStatement sqlSelect) {

 	   	ArrayList<String> al_literalAlpha = null;
  	   	ArrayList<String> al_literalNum = null;
  	   	ArrayList<ArrayList<String>> al_al_entity = null;
   	   	ArrayList<String> al_hostVar = null;

		// Simboli literal alfanumeriche
   		al_literalAlpha = sqlSelect.getConstantsAlphanumeric();
		for (String literalAlpha : al_literalAlpha) {
			instruction.addSymbolInput(literalAlpha, EnumSymbolType.SQL_SYMBOL_LITERAL_ALPHA);
		}

   		// Simboli literal numeriche
   		al_literalNum = sqlSelect.getConstantsNumeric();
		for (String literalNum : al_literalNum) {
			instruction.addSymbolInput(literalNum.toString(), EnumSymbolType.SQL_SYMBOL_LITERAL_NUM);
		}
   		
		// Symboli nomi tabelle/view in input per l'istruzione
		al_al_entity = sqlSelect.getEntities();
		for (ArrayList<String> al_entity : al_al_entity) {
			instruction.addSymbolInput(al_entity.get(0), EnumSymbolType.SQL_SYMBOL_TABLE_NAME);
		}
		
   		// Simboli host var in input per l'istruzione, : gia eliminati
		al_hostVar = sqlSelect.getHostVarsWhere();
  	   	for (String hostVarWhere : al_hostVar) {
 	   		instruction.addSymbolInput(hostVarWhere, EnumSymbolType.SQL_SYMBOL_HOST_VAR);
 		}

  		// Simboli host var in output per l'istruzione, : gia eliminati 
  	    al_hostVar = sqlSelect.getHostVarsInto();
  		for (String hostVarWhere : al_hostVar) {
	   		instruction.addSymbolOutput(hostVarWhere, EnumSymbolType.SQL_SYMBOL_HOST_VAR);
 		}
	}

    /* ----------------------------------------------------------------------
     * inserimento oggetti e relazione su db a fronte di SqlSelect statement
     * ----------------------------------------------------------------------
     * 
     */
	private void analyzeSqlSelectDbUpdates(InstructionSql instruction, SqlSelectStatement sqlSelect) {

   	   	ArrayList<ArrayList<String>> al_al_entity = null;
 		String entityName = "";
		String entityOwner = "";

        if (instruction.getNumInstr() == 0) {
        	entityOwner="";
		}	
		
    	// Symboli nomi tabelle/view in input per l'istruzione
		al_al_entity = sqlSelect.getEntities();
    	
		// Scan tabelle dichiarate direttamente, in subselect e fullselect
   	   	for (ArrayList<String> al_entity : al_al_entity) {
   	   		
 	   		entityName  = al_entity.get(0);
 	   		entityOwner  = al_entity.get(1);
   	   		
   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   	   		if (!entityOwner.equals("")) {
   	   		   this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER, entityOwner, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
			}
   	   		
   	   	    // PGM_SQL_OWNER
   	   		if (!entityOwner.equals("")) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
   	  		}
   			// PGM_ENTITY 
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
   				this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
   				this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_INQUIRY);
   	   	    }
   			// PGM_ENTITY_READ
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY_READ, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY_READ, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
   	   	    }
   	        // ENTITY_SQL_OWNER
   	   	    if (!entityOwner.equals("")) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
   	   	    }
   	   	    // ENTITY_SQL_SCRIPT
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
   				this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_WITH_I_O_SQL);
   				this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_INQUIRY);
   	   	    }
		}
   	   	
   	   	// N.B. I Where-used delle colonne vengono inseriti nel programma chiamante
        // dove sono presenti tutte le informazioni di where used che dipendono
   	   	// dall'analisi e dalle variabili del programma.
	}


	/* ---------------------------------------
	 * Parsing opzioni istruzione sql select
	 * ---------------------------------------
	 * 
	 */
	private void analyzeSqlSelectParseOptions(InnerContextAnalysis ictx, InstructionSql instruction, SqlSelectStatement sqlSelect, String strOptions) {
        
		Scanner scnOptions = null;
		String token = "";
		
		// Errori di parsing precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Nessuna opzione da analizzare
		if (strOptions.equals("")) {
			return;
		}
		  
		scnOptions = new Scanner(strOptions);
		token = nextToken(scnOptions);					//  
		
		// Scan tokens
		while (!token.equals("")) {
			
			// OPTIMIZE FOR
			if (token.equals("OPTIMIZE")) {
				token = nextToken(scnOptions);			// FOR
				token = nextToken(scnOptions);          // n
				sqlSelect.setOptimizeForRowsNumber(StringService._getNumericInt(token));
				sqlSelect.setOptimizeForRows(true);
				token = nextToken(scnOptions);			// ROW|ROWS
				token = nextToken(scnOptions);
				continue;
			}

			// QUERYNO
			if (token.equals("QUERYNO")) {
				token = nextToken(scnOptions);			// n
				sqlSelect.setQueryno(StringService._getNumericInt(token));
				sqlSelect.setQueryno(true);
				token = nextToken(scnOptions);
				continue;
			}

			// SKIP LOCK DATA
			if (token.equals("SKIP")) {
				token = nextToken(scnOptions);			// LOCK
				token = nextToken(scnOptions);			// DATA
				sqlSelect.setSkipLockData(true);
				token = nextToken(scnOptions);
				continue;
			}

			// WITH .. USE
			if (token.equals("WITH")) {
				token = nextToken(scnOptions);			// RR|RS|CS|UR
				sqlSelect.setIsolation(token);
				sqlSelect.setIsolation(true);
				token = nextToken(scnOptions);			// USE ?
				if (!token.equals("USE")) {
					continue;
				}
				token = nextToken(scnOptions);			// AND
				token = nextToken(scnOptions);			// KEEP
				token = nextToken(scnOptions);			// EXCLUSIVE|UPDATE|SHARE
				if (token.equals("EXCLUSIVE")) {
					sqlSelect.setIsolationUseAndKeepExclusiveLocks(true);
					token = nextToken(scnOptions);		// LOCKS
					token = nextToken(scnOptions);
					continue;
				}
				if (token.equals("UPDATE")) {
					sqlSelect.setIsolationUseAndKeepUpdateLocks(true);
					token = nextToken(scnOptions);
					continue;
				}
				if (token.equals("SHARE")) {
					sqlSelect.setIsolationUseAndKeepShareLocks(true);
					token = nextToken(scnOptions);
					continue;
				}

				// parametro non gestito: skip
				token = nextToken(scnOptions);
				continue;
			}

			// FOR
			if (token.equals("FOR")) {
				token = nextToken(scnOptions);			// UPDATE|READ
				if (token.equals("READ")) {
					sqlSelect.setForReadOnly(true);
					token = nextToken(scnOptions);		// ONLY
					token = nextToken(scnOptions);
					continue;
				}
				if (token.equals("UPDATE")) {
					sqlSelect.setForUpdate(true);
					token = nextToken(scnOptions);		// OF ?
					if (!token.equals("OF")) {
						continue;
					}
					token = nextToken(scnOptions);
					while (!token.equals("") 
						&& !token.equals("FOR") 
						&& !token.equals("OPTIMIZE") 
						&& !token.equals("WITH") 
						&& !token.equals("QUERYNO") 
					    && !token.equals("SKIP")) {
						
						if (token.equals(",")) {
							token = nextToken(scnOptions);
							continue;
						}
						sqlSelect.getForUpdateColumns().add(token);
						token = nextToken(scnOptions);
					}
					continue;
				}
			}
			
		    // parametro non gestito: skip
			token = nextToken(scnOptions);
		}
	}

    /* ------------------------------------------------------------
     * Individuazione posizione inizio parametri istruzione select
     * ------------------------------------------------------------
     * 
     *  I parametri possono essere codificati in qualsiasi ordine
     *  Viene restituito l'indice del primo parametro
     *  Se non ci sono parametri restituisce -1
     */
	private int analyzeSqlSelectGetStartOptions(String strFullSelectAndOptions) {
		
		Scanner scn = null;
		String token = "";
        ArrayList<Integer> al_iOption = null;
		int i = 0;
		
		al_iOption = new ArrayList<Integer> ();
		
		i = strFullSelectAndOptions.indexOf(" FOR ");
		if (i > 0) {
			scn = new Scanner(strFullSelectAndOptions.substring(i + 5).trim());
			token = nextToken(scn);
			if (token.equals("UPDATE") || token.equals("READ")) {
				al_iOption.add(i);
			}
		}
		
		i = strFullSelectAndOptions.indexOf(" OPTIMIZE ");
		if (i > 0) {
			scn = new Scanner(strFullSelectAndOptions.substring(i + 10).trim());
			token = nextToken(scn);
			if (token.equals("FOR")) {
				al_iOption.add(i);
			}
		}
		
		i = strFullSelectAndOptions.indexOf(" WITH ");
		if (i > 0) {
			scn = new Scanner(strFullSelectAndOptions.substring(i + 6).trim());
			token = nextToken(scn);
			if (token.equals("RR") || token.equals("RS") || token.equals("CS") || token.equals("UR")) {
				al_iOption.add(i);
			}
		}
		
		i = strFullSelectAndOptions.indexOf(" SKIP ");
		if (i > 0) {
			scn = new Scanner(strFullSelectAndOptions.substring(i + 6));
			token = nextToken(scn);
			if (token.equals("LOCK")) {
				if (i > 0) {al_iOption.add(i);}
			}
		}
		
		i = strFullSelectAndOptions.indexOf(" QUERYNO ");
		if (i > 0) {al_iOption.add(i);}

		// Nessuna opzione presente
		if (al_iOption.size() == 0) {
			return -1;
		}
		
		// Ordinamento in ordine ascendente
		Collections.sort(al_iOption);
		
		// Restituisco il primo, con indice <
		return al_iOption.get(0);
	}

	
	/* -------------------------
	 * Analisi statement Update
	 * -------------------------
	 * 
	 * UPDATE tableName 
	 *      |period-clause 			FOR PORTION OF BUSINESS_TIME FROM value1 TO value2
	 *      |correlation-name 		name
	 *      |include-clause			INCLUDE ( col dataType, ..)
	 *       SET assignment-clause 
	 *      |WHERE
	 *      |isolation-clause, SKIP
	 *      |QUERYNO
	 *      
	 *  oppure
	 * 
	 * UPDATE tableName |correlation-name 
	 *       SET assignment-clause 
	 *       WHERE CURRENT OF cursor-name
	 *         FOR ROW hostVar|number OF ROWSET
	 * 
	 */
	private void analyzeSqlUpdate(InnerContextAnalysis ictx, InstructionSql instruction) throws SQLException, ExceptionAmrita{
	
  	   	ArrayList<String> al_literalAlpha = null;
  	   	ArrayList<String> al_literalNum = null;
  	   	ArrayList<ArrayList<String>> al_al_entity = null;
   	   	ArrayList<String> al_hostVar = null;
        SqlUpdateStatement sqlUpdate = null;
 		String str = "";
 		String entityName = "";
 		String entityOwner = "";
 		
 		int i = 0;
 		
 		String strBeforeSet = "";
 		String strAfterSet = "";
 		 
    		
		// Allocazione strutture di lavoro
        sqlUpdate = new SqlUpdateStatement();
 		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		// Estrazione sezioni a sinistra e a destra di SET
		str =instruction.getSourceInstr(); 
		i = str.indexOf(" SET ");
		strBeforeSet = str.substring(0, i).trim();
		strAfterSet = str.substring(i + 5).trim();
		

		////////////////////////////////////////////////////////////////////
		// Parser
		///////////////////////////////////////////////////////////////////
		  
		analyzeSqlUpdateBeforeSet(ictx, instruction, sqlUpdate, strBeforeSet);
		analyzeSqlUpdateAfterSet(ictx, instruction, sqlUpdate, strAfterSet);

		// Parsing error
		if (instruction.isParsingError()) {
			return;
		}
		
		
		////////////////////////////////////////////////////////////////////
		// Caricamento simboli nell'istruzione e descrittore
		///////////////////////////////////////////////////////////////////		
		
   		// Simboli literal alfanumeriche
   		al_literalAlpha = sqlUpdate.getConstantsAlphanumeric();
		for (String literalAlpha : al_literalAlpha) {
			instruction.addSymbolInput(literalAlpha, EnumSymbolType.SQL_SYMBOL_LITERAL_ALPHA);
		}

   		// Simboli literal numeriche
   		al_literalNum = sqlUpdate.getConstantsNumeric();
		for (String literalNum : al_literalNum) {
			instruction.addSymbolInput(literalNum.toString(), EnumSymbolType.SQL_SYMBOL_LITERAL_NUM);
		}
   		
		// Symboli nomi tabelle/view in input per l'istruzione
		al_al_entity = sqlUpdate.getEntities();
		for (ArrayList<String> al_entity : al_al_entity) {
			instruction.addSymbolInput(al_entity.get(0), EnumSymbolType.SQL_SYMBOL_TABLE_NAME);
		}
		
   		// Simboli host var in input per l'istruzione, elimino :
  	   	al_hostVar = sqlUpdate.getHostVars();
  	   	for (String hostVar : al_hostVar) {
	   		instruction.addSymbolInput(hostVar.substring(1), EnumSymbolType.SQL_SYMBOL_HOST_VAR);
 		}
        
  	   	// Descrittore istruzione UPDATE
    	instruction.sqlUpdateSetDescriptor(sqlUpdate);


    	////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////

  	    entityName = sqlUpdate.getEntityName();
  	    entityOwner = sqlUpdate.getEntityOwner();
  	    
  	   	// Oggetto tabella in update e owner
   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   		if (!entityOwner.equals("")) {
   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,entityOwner, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
		}
   		
   	    // PGM_SQL_OWNER
   		if (!entityOwner.equals("")) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION,  this.userExitInfoPgm, instruction);
  		}
		// PGM_ENTITY 
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
	   	    this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
   	    }
		// PGM_ENTITY_UPDATE
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY_UPDATE, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY_UPDATE, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_UPDATE);
   	    }
        // ENTITY_SQL_OWNER
   	    if (!entityOwner.equals("")) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
   	    }
   	    // ENTITY_SQL_SCRIPT
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_WITH_I_O_SQL);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_UPDATE);
   	    }

  	   	
		// Scan tabelle dichiarate direttamente, in subselect e fullselect
   	   	for (ArrayList<String> al_entity : al_al_entity) {
   	   		
   	   		entityName = al_entity.get(0);
   	   	    entityOwner = al_entity.get(1);
   	   		
   	   	    // Oggetto tabella e owner
   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   	  		if (!entityOwner.equals("")) {
   	   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,entityOwner, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   			}
  	   		
   	   	    // PGM_SQL_OWNER
   	   		if (!entityOwner.equals("")) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
   	  		}
   		    // PGM_ENTITY
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
   	   	    }
   		    // PGM_ENTITY_UPDATE
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY_UPDATE, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY_UPDATE, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
     			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_UPDATE);
   	   	    }
   	        // ENTITY_SQL_OWNER
   	   	    if (!entityOwner.equals("")) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
   	   	    }
   		    // ENTITY_SQL_SCRIPT
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_WITH_I_O_SQL);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_INQUIRY);
   	   	    }
		}
   	   	
   	   	// N.B. I Where-used delle colonne vengono inseriti nel programma chiamante
        // dove sono presenti tutte le informazioni di where used che dipendono
   	   	// dall'analisi e dalle variabili del programma.
	}
	
	/*
	 * --------------------------------------------------------
	 * Analisi istruzione Update, da inizio fino a SET escluso 
	 * --------------------------------------------------------
	 * 
	 * UPDATE tableName 
	 *      |period-clause 			FOR PORTION OF BUSINESS_TIME FROM value1 TO value2
	 *      |correlation-name 		name
	 *      |include-clause			INCLUDE ( col dataType, ..)
	 *       SET assignment-clause 
	 *      |WHERE search-conditions
	 *      |isolation-clause, SKIP
	 *      |QUERYNO
	 *      
	 *  oppure
	 * 
	 * UPDATE tableName |correlation-name 
	 *       SET assignment-clause 
	 *       WHERE CURRENT OF cursor-name
	 *         FOR ROW hostVar|number OF ROWSET
	 * 
	 * Si analizzano:
	 * 
	 *  1) table name
	 *  2) period-clause
	 *  3) correlationName
	 *  4) include-clause
	 * 
	 */
	private void analyzeSqlUpdateBeforeSet(InnerContextAnalysis ictx, InstructionSql instruction, SqlUpdateStatement sqlUpdate, String strToParse) {
		
		Scanner scn = null;
		String token = "";
		String strBetweenPar = "";
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		scn = new Scanner(strToParse);
		this.strNoMemoryToParse = strToParse;
		token = nextToken(scn);							// UPDATE
		token = nextToken(scn);							// table-view-name
		sqlUpdate.setEntityNameQualified(token);
		
		token = nextToken(scn);							// FOR ?
		
		// period-clause
		if (token.equals("FOR")) {
			token = nextToken(scn);						// PORTION
			token = nextToken(scn);						// OF
			token = nextToken(scn);						// BUSINESS_TIME
			token = nextToken(scn);						// FROM
			token = nextToken(scn);						// value-lower
			sqlUpdate.setPeriodForValueLower(token);
			token = nextToken(scn);						// FROM
			token = nextToken(scn);						// value-higher
			sqlUpdate.setPeriodForValueHigher(token);
			sqlUpdate.setPeriod(true);
			token = nextToken(scn);
		}
		
		// fine clausole codificate
		if (token.equals("")) {
			return;
		}
		
		// correlation-name
		if (!token.equals("INCLUDE")) {
			sqlUpdate.setCorrelationName(token);
			token = nextToken(scn);
		}
		
		// include-clause
		if (token.equals("INCLUDE")) {
			token = nextToken(scn);						// (
			strBetweenPar = extractStringBetweenPar(scn);
			sqlUpdate.setIncludeColumnsValue(strBetweenPar);
			sqlUpdate.setIncludeColumns(true);
		}
	}

	 
   
	/*
	 * ----------------------------------------------------------------
	 * Analisi istruzione Update, da inizio SET fino a fine istruzione
	 * ----------------------------------------------------------------
	 * 
	 * 
	 * UPDATE tableName 
	 *      |period-clause 			FOR PORTION OF BUSINESS_TIME FROM value1 TO value2
	 *      |correlation-name 		name
	 *      |include-clause			INCLUDE ( col dataType, ..)
	 *       SET assignment-clause 
	 *      |WHERE seatch-conditions
	 *      |isolation-clause, SKIP
	 *      |QUERYNO
	 *      
	 *  oppure
	 * 
	 * UPDATE tableName |correlation-name 
	 *       SET assignment-clause 
	 *       WHERE CURRENT OF cursor-name
	 *         FOR ROW hostVar|number OF ROWSET
	 * 
	 * 
	 * Si analizza:
	 * 
	 *  1) assignment-clause
	 *  2) where-current-clause
	 *  3) where-search-conditions
	 *  4) isolation-clause
	 *  5) queryno
	 *
	 *  2) è mutuamente esclusiva con 3) 4) 5)		
	 */
	private void analyzeSqlUpdateAfterSet(InnerContextAnalysis ictx, InstructionSql instruction, SqlUpdateStatement sqlUpdate, String strToParse) throws SQLException, ExceptionAmrita {

		Scanner scn = null;
		SqlExpression sqlExpression = null;
		SqlSearchConditions sqlWhere = null;
		StringBuffer sbAssignmentClause = null;
		StringBuffer sbWhereSearchConditionClause = null;
		String[] ar_assignment = null;
		String[] ar_valueAssigned = null;
		String[] ar_columnLeft = null;
		String strWhereClause = null;
		String assignmentLeft = "";
		String assignmentRight = "";
		String strBetweenPar = "";
		String token = "";
		String strOptions = "";
		String strBeteeenPar = "";
 		int i = 0;
 		
 		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		///////////////////////////////////////////////////////////////////////////////////
		// Individuazione sezioni e clausole da analizzare
		///////////////////////////////////////////////////////////////////////////////////
		
		scn = new Scanner(strToParse);
		this.strNoMemoryToParse = strToParse;
		
		token = nextToken(scn);
		sbAssignmentClause = new StringBuffer ();
		
		// Estrazione token fino a fine SET clause
		while (!token.equals("") 
			&& !token.equals("WHERE") 
			&& !token.equals("WITH") 
			&& !token.equals("SKIP") 
			&& !token.equals("QUERYNO")) {
			
			sbAssignmentClause.append(" " + token);
			if (token.equals("(")) {
				strBeteeenPar = extractStringBetweenPar(scn);
				sbAssignmentClause.append(" " + strBeteeenPar + " )");
			}
			token = nextToken(scn);
			continue;
		}
		
		// Where-clause
		sbWhereSearchConditionClause = new StringBuffer ();
		if (token.equals("WHERE")) {
			token = nextToken(scn);								// bypass WHERE
			// Estrazione token fino a fine WHERE clause
			while (!token.equals("") 
				&& !token.equals("WITH") 
				&& !token.equals("SKIP") 
				&& !token.equals("QUERYNO")) {
				
				sbWhereSearchConditionClause.append(" " + token);
				if (token.equals("(")) {
					strBeteeenPar = extractStringBetweenPar(scn);
					sbWhereSearchConditionClause.append(" " + strBeteeenPar + " )");
				}
				token = nextToken(scn);
				continue;
			}
		}
		
		// Options, se presenti
		strOptions = token + " " + this.strNoMemoryToParse;
		strOptions = strOptions.trim();
		
		/////////////////////////////////////////////////////////////////
		// Analisi assignement clause, assegnazioni separate da virgole
		/////////////////////////////////////////////////////////////////
		
		ar_assignment = splitParms(sbAssignmentClause.toString().trim());
		for (String assignment : ar_assignment) {
			
			i = assignment.indexOf(" = ");
			assignmentLeft = assignment.substring(0, i).trim();
			assignmentRight = assignment.substring(i + 3).trim();
			
			// Colonne fra parentesi ( ... )
			if (assignmentLeft.startsWith("(")) {
				// Elimino parentesi ()
				strBetweenPar = assignmentLeft.substring(1);			 
			    strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1).trim();	
			    ar_columnLeft = splitParms(strBetweenPar);
			    // Scan nomi colonna
			    for (String columnName : ar_columnLeft) {
				    sqlUpdate.getSetColumnsName().add(columnName);
				}
			    
			// C'è un solo nome di colonna 
			} else {
				sqlUpdate.getSetColumnsName().add(assignmentLeft);
			}
	        
			// Analisi valori assegnati a destra del segno di uguale
			
			// Valori fra parentesi ( ... )
			if (assignmentRight.startsWith("(")) {
				// Elimino parentesi ( )
				strBetweenPar = assignmentRight.substring(1);			 
			    strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1).trim();	
			    // ( full-select ) la tratto come scalar-fullselect elemento di espression 
			    if (strBetweenPar.startsWith("SELECT")) {
				   sqlExpression = analyzeSqlExpression(ictx, instruction, "( " + strBetweenPar + " )");
				   if (instruction.isParsingError()) {return;}
				   sqlUpdate.getSetExpressionsRight().add(sqlExpression);
				   continue;
				} 
			    
				// (expr|DEFAULT|NULL, ...,)
			    ar_valueAssigned = splitParms(strBetweenPar);
			    for (String valueAssigned : ar_valueAssigned) {
				   sqlExpression = analyzeSqlExpression(ictx, instruction, valueAssigned);
				   if (instruction.isParsingError()) {return;}
				   sqlUpdate.getSetExpressionsRight().add(sqlExpression);
			    }
			    continue;
			}
			
			// Analisi unico valore o espressione assegnata a destra del segno di uguale
			sqlExpression = analyzeSqlExpression(ictx, instruction, assignmentRight); 
			if (instruction.isParsingError()) {return;}
			sqlUpdate.getSetExpressionsRight().add(sqlExpression);
	
		} // end-for assignment
		
			
		/////////////////////////////////////////////////////////////////
		// Analisi where-search-conditions
		//         where-current-clause
		/////////////////////////////////////////////////////////////////
		
		strWhereClause = sbWhereSearchConditionClause.toString().trim();
		if (!strWhereClause.equals("")) {
			
			// where-search-conditions
			if (!strWhereClause.startsWith("CURRENT")) {
				sqlWhere = analyzeSqlSearchConditions(ictx, instruction, sbWhereSearchConditionClause.toString().trim());
				if (instruction.isParsingError()) {return;}
				sqlUpdate.setWhere(sqlWhere);
				sqlUpdate.setSearchedUpdate(true);
				
		    // where-current-clause				
			} else {
				scn = new Scanner(strWhereClause);
				this.strNoMemoryToParse = strWhereClause;
				token = nextToken(scn);							// WHERE
				token = nextToken(scn);							// CURRENT
				token = nextToken(scn);							// OF
				token = nextToken(scn);							// cursor-name
				sqlUpdate.setCursorName(token);
				token = nextToken(scn);							// FOR ?
				
				// FOR ROW ... OF ROWSET
			    if (token.equals("FOR")) {
			    	token = nextToken(scn);						// ROW
			    	token = nextToken(scn);						// host-variable|integer-constant
			    	if (StringService._isNumericInt(token)) {
			    		sqlUpdate.setForRowNumber(StringService._getNumericInt(token));
			    		sqlUpdate.setForRowNumber(true);
					} else {
			    		sqlUpdate.setForRowHostVar(token);
			    		sqlUpdate.setForRowHostVar(true);
					}
				}
			}
		}
			
		

		/////////////////////////////////////////////////////////////////
		// Analisi options skip, isolation clause e queryno
		/////////////////////////////////////////////////////////////////
		
		if (!strOptions.equals("")) {
			scn = new Scanner(strOptions);
			this.strNoMemoryToParse = strOptions;
			token = nextToken(scn);							 
			
			// Scan tokens
            while (!token.equals("")) {
				
            	// SKIP LOCKED DATA
            	if (token.equals("SKIP")) {
            		token = nextToken(scn);					// LOCKED
               		token = nextToken(scn);					// DATA
               		sqlUpdate.setSkipLockData(true);
               		token = nextToken(scn);
               		continue;
				}
            	// WITH RR|RS|CS
            	if (token.equals("WITH")) {
            		token = nextToken(scn);					// RR|RS|CS	
            		sqlUpdate.setIsolation(token);
            		sqlUpdate.setIsolation(true);
            		token = nextToken(scn);
            		continue;
            	}
            	
            	// QUERYNO
            	if (token.equals("QUERYNO")) {
            		token = nextToken(scn);					// num
                 	sqlUpdate.setQueryno(StringService._getNumericInt(token));
                	sqlUpdate.setQueryno(true);
                	token = nextToken(scn);
                	continue;
            	}
 			}
		}

	}




	/* -------------------------
	 * Analisi statement Delete
	 * -------------------------
	 * 
	 * DELETE FROM tbName
	 *        |period-clause
	 *        |corr-name
	 *        |include-columns
	 *        |SET assignment-clause
	 *        |WHERE search-conditions
	 *        !SKIP, QUERYNO, isolation clause
	 *        
	 * oppure
	 * 
	 * DELETE FROM tbName
	 *        |corr-name
	 *        |WHERE CURRENT OF
	 *        !FOR ROW ... OF ROWSET
	 *        
	 * 
	 */
	private void analyzeSqlDelete(InnerContextAnalysis ictx, InstructionSql instruction) throws SQLException, ExceptionAmrita{
		
  	   	ArrayList<String> al_literalAlpha = null;
  	   	ArrayList<String> al_literalNum = null;
  	   	ArrayList<ArrayList<String>> al_al_entity = null;
   	   	ArrayList<String> al_hostVar = null;
        SqlDeleteStatement sqlDelete = null;
        StringBuffer sbAssignmentClause = null;
        StringBuffer sbWhereClause = null;
 		String str = "";
 		String entityName = "";
		String entityOwner = "";
		String strBetweenPar = "";
		String strOptions = "";
 		String token = "";
 		 		 
    		
		// Allocazione strutture di lavoro
 		sqlDelete = new SqlDeleteStatement();
        sbAssignmentClause = new StringBuffer();
        sbWhereClause =new StringBuffer();
 		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		     

		///////////////////////////////////////////////////////////////////////////////////
		// Individuazione sezioni e clausole da analizzare
		///////////////////////////////////////////////////////////////////////////////////

		str =instruction.getSourceInstr(); 
		scn = new Scanner(str);
		this.strNoMemoryToParse = str;

		
		token = nextToken(scn);							// DELETE
		token = nextToken(scn);							// FROM
		token = nextToken(scn);							// entityName
		sqlDelete.setEntityNameQualified(token);
		token = nextToken(scn);	
		
		// period-clause
		if (token.equals("FOR")) {
			token = nextToken(scn);						// PORTION
			token = nextToken(scn);						// OF
			token = nextToken(scn);						// BUSINESS_TIME
			token = nextToken(scn);						// FROM
			token = nextToken(scn);						// value-lower
			sqlDelete.setPeriodForValueLower(token);
			token = nextToken(scn);						// FROM
			token = nextToken(scn);						// value-higher
			sqlDelete.setPeriodForValueHigher(token);
			sqlDelete.setPeriod(true);
			token = nextToken(scn);
		}
		
		// Correlation-name
		if (!token.equals("")
		&&	!token.equals("INCLUDE")
	    &&  !token.equals("SET")	
	    &&  !token.equals("WHERE")	
	    &&  !token.equals("WITH")	
	    &&  !token.equals("SKIP")	
	    &&  !token.equals("QUERYNO")) {
			sqlDelete.setCorrelationName(token);
			token = nextToken(scn);	
		}
		
		// include-clause
		if (token.equals("INCLUDE")) {
			token = nextToken(scn);									// (
			strBetweenPar = extractStringBetweenPar(scn);
			sqlDelete.setIncludeColumnsValue(strBetweenPar);
			sqlDelete.setIncludeColumns(true);
			token = nextToken(scn);	
		}
		
		// set-assignment-clause
		if (token.equals("SET")) {
			token = nextToken(scn);	
		    while (!token.equals("")
		       &&  !token.equals("WHERE")	
		       &&  !token.equals("WITH")	
		       &&  !token.equals("SKIP")	
		       &&  !token.equals("QUERYNO")) {
		    	
		    	sbAssignmentClause.append(" " + token);
		    	if (token.equals("(")) {
		    		strBetweenPar = extractStringBetweenPar(scn);
		    		sbAssignmentClause.append(" " + strBetweenPar + " )");
				}
		    	token = nextToken(scn);	
		    }
		}
		
		
		// WHERE search-coditions
		// WHERE CURRENT OF cursor-name
		if (token.equals("WHERE")) {
			sbWhereClause.append(token);
			token = nextToken(scn);
			// Estrazione token fino a fine WHERE clause
			while (!token.equals("") 
				&& !token.equals("FOR") 
				&& !token.equals("WITH") 
				&& !token.equals("SKIP") 
				&& !token.equals("QUERYNO")) {
				
				sbWhereClause.append(" " + token);
				if (token.equals("(")) {
					strBetweenPar = extractStringBetweenPar(scn);
					sbWhereClause.append(" " + strBetweenPar + " )");
				}
				token = nextToken(scn);
				continue;
			}
		}

		// FOR ROW ... OF ROWSET
	    if (token.equals("FOR")) {
	    	token = nextToken(scn);						// ROW
	    	token = nextToken(scn);						// host-variable|integer-constant
	    	if (StringService._isNumericInt(token)) {
	    		sqlDelete.setForRowNumber(StringService._getNumericInt(token));
	    		sqlDelete.setForRowNumber(true);
			} else {
	    		sqlDelete.setForRowHostVar(token);
	    		sqlDelete.setForRowHostVar(true);
			}
			token = nextToken(scn);						// OF
			token = nextToken(scn);						// ROWSET
			token = nextToken(scn);
		}

	    // Options
	    strOptions = token + " " + this.strNoMemoryToParse;
		

		////////////////////////////////////////////////////////////////////
		// Parser sezioni complesse
		////////////////////////////////////////////////////////////////////
		
		analyzeSqlDeleteSet(ictx, instruction, sqlDelete, sbAssignmentClause.toString().trim());
		analyzeSqlDeleteWhere(ictx, instruction, sqlDelete, sbWhereClause.toString().trim());
		analyzeSqlDeleteOptions(ictx, instruction, sqlDelete, strOptions.toString().trim());

		// Parsing error
		if (instruction.isParsingError()) {
			return;
		}
		
		
		////////////////////////////////////////////////////////////////////
		// Caricamento simboli nell'istruzione e descrittore
		///////////////////////////////////////////////////////////////////		
		
   		// Simboli literal alfanumeriche
   		al_literalAlpha = sqlDelete.getConstantsAlphanumeric();
		for (String literalAlpha : al_literalAlpha) {
			instruction.addSymbolInput(literalAlpha, EnumSymbolType.SQL_SYMBOL_LITERAL_ALPHA);
		}

   		// Simboli literal numeriche
   		al_literalNum = sqlDelete.getConstantsNumeric();
		for (String literalNum : al_literalNum) {
			instruction.addSymbolInput(literalNum.toString(), EnumSymbolType.SQL_SYMBOL_LITERAL_NUM);
		}
   		
		// Symboli nomi tabelle/view in input per l'istruzione
		al_al_entity = sqlDelete.getEntities();
		for (ArrayList<String> al_entity : al_al_entity) {
			instruction.addSymbolInput(al_entity.get(0), EnumSymbolType.SQL_SYMBOL_TABLE_NAME);
		}
		
   		// Simboli host var in input per l'istruzione, elimino i : 
  	   	al_hostVar = sqlDelete.getHostVars();
  	   	for (String hostVar : al_hostVar) {
	   		instruction.addSymbolInput(hostVar.substring(1), EnumSymbolType.SQL_SYMBOL_HOST_VAR);
 		}
        
  	   	// Descrittore istruzione DELETE
    	instruction.sqlDeleteSetDescriptor(sqlDelete);


    	////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////

  	    entityName = sqlDelete.getEntityName();
  	    entityOwner = sqlDelete.getEntityOwner();
  	    
  	   	// Oggetto tabella in update e owner
   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   		if (!entityOwner.equals("")) {
   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,entityOwner, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
		}
   		
   	    // PGM_SQL_OWNER
   		if (!entityOwner.equals("")) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
  		}
		// PGM_ENTITY
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
   	    }
		// PGM_ENTITY_DELETE
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY_DELETE, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY_DELETE, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_UPDATE);
   	    }
        // ENTITY_SQL_OWNER
   	    if (!entityOwner.equals("")) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
   	    }
   	    // ENTITY_SQL_SCRIPT
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_WITH_I_O_SQL);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_UPDATE);
   	    }

  	   	
		// Scan tabelle dichiarate direttamente, in subselect e fullselect
   	   	for (ArrayList<String> al_entity : al_al_entity) {
   	   		
   	   		entityName = al_entity.get(0);
   	   	    entityOwner = al_entity.get(1);
   	   		
   	   	    // Oggetto tabella e owner
   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   	  		if (!entityOwner.equals("")) {
   	   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,entityOwner, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   			}
  	   		
   	   	    // PGM_SQL_OWNER
   	   		if (!entityOwner.equals("")) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
   	  		}
   		    // PGM_ENTITY
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
    	   	    }
   		    // PGM_ENTITY_DELETE
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY_DELETE, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY_DELETE, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_UPDATE);
   	   	    }
   	        // ENTITY_SQL_OWNER
   	   	    if (!entityOwner.equals("")) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
   	   	    }
   		    // ENTITY_SQL_SCRIPT
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_WITH_I_O_SQL);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_INQUIRY);
   	   	    }
		}
   	   	
   	   	// N.B. I Where-used delle colonne vengono inseriti nel programma chiamante
        // dove sono presenti tutte le informazioni di where used che dipendono
   	   	// dall'analisi e dalle variabilòi del programma.
	}
	
	
	
    /* -------------------------------
     * Analisi clausola SET di delete
     * -------------------------------
     * 
     * 
     */
	private void analyzeSqlDeleteSet(InnerContextAnalysis ictx, InstructionSql instruction, SqlDeleteStatement sqlDelete, String strAssignmentClause) throws SQLException, ExceptionAmrita {
		
		SqlExpression sqlExpression = null;
		String[] ar_assignment = null;
		String[] ar_valueAssigned = null;
		String[] ar_columnLeft = null;
		String assignmentLeft = "";
		String assignmentRight = "";
		String strBetweenPar = "";
 		int i = 0;
 		
 		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}

		// Input vuoto
		if (strAssignmentClause.equals("")) {
			return;
		}
		
		///////////////////////////////////////////////////////////////////
 		// Analisi assegnazioni separate da virgole
 		/////////////////////////////////////////////////////////////////
 		
		ar_assignment = splitParms(strAssignmentClause.toString().trim());
		for (String assignment : ar_assignment) {
			
			i = assignment.indexOf(" = ");
			assignmentLeft = assignment.substring(0, i).trim();
			assignmentRight = assignment.substring(i + 3).trim();
			
			// Colonne fra parentesi ( ... )
			if (assignmentLeft.startsWith("(")) {
				// Elimino parentesi ()
				strBetweenPar = assignmentLeft.substring(1);			 
			    strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1).trim();	
			    ar_columnLeft = splitParms(strBetweenPar);
			    // Scan nomi colonna
			    for (String columnName : ar_columnLeft) {
				    sqlDelete.getSetColumnsName().add(columnName);
				}
			    
			// C'è un solo nome di colonna 
			} else {
				sqlDelete.getSetColumnsName().add(assignmentLeft);
			}
	        
			// Analisi valori assegnati a destra del segno di uguale
			
			// Valori fra parentesi ( ... )
			if (assignmentRight.startsWith("(")) {
				// Elimino parentesi ( )
				strBetweenPar = assignmentRight.substring(1);			 
			    strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1).trim();	
			    // ( full-select ) la tratto come scalar-fullselect elemento di espression 
			    if (strBetweenPar.startsWith("SELECT")) {
				   sqlExpression = analyzeSqlExpression(ictx, instruction, "( " + strBetweenPar + " )");
				   if (instruction.isParsingError()) {return;}
				   sqlDelete.getSetExpressionsRight().add(sqlExpression);
				   continue;
				} 
			    
				// (expr|NULL, ...,)
			    ar_valueAssigned = splitParms(strBetweenPar);
			    for (String valueAssigned : ar_valueAssigned) {
				   sqlExpression = analyzeSqlExpression(ictx, instruction, valueAssigned);
				   if (instruction.isParsingError()) {return;}
				   sqlDelete.getSetExpressionsRight().add(sqlExpression);
			    }
			    continue;
			}
			
			// Analisi unico valore o espressione assegnata a destra del segno di uguale
			sqlExpression = analyzeSqlExpression(ictx, instruction, assignmentRight); 
			if (instruction.isParsingError()) {return;}
			sqlDelete.getSetExpressionsRight().add(sqlExpression);
	
		} // end-for assignment
		
	}


    /* -------------------------------
     * Analisi clausola WHERE di delete
     * -------------------------------
     * 
     * 
     */
	private void analyzeSqlDeleteWhere(InnerContextAnalysis ictx, InstructionSql instruction, SqlDeleteStatement sqlDelete, String strWhereClause) throws SQLException, ExceptionAmrita {

		Scanner scn = null;
		SqlSearchConditions sqlWhere = null;
		String strSearchConditions = "";
		String token = "";
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}

		// Input vuoto
		if (strWhereClause.equals("")) {
			return;
		}
		
		
		/////////////////////////////////////////////////////////////////
		// Analisi where-search-conditions
		//         where-current-clause
		/////////////////////////////////////////////////////////////////
		
		// where-search-conditions
		if (!strWhereClause.substring(5).trim().startsWith("CURRENT")) {
			strSearchConditions = strWhereClause.substring(5).trim();  // Elimina WHERE
			sqlWhere = analyzeSqlSearchConditions(ictx, instruction, strSearchConditions);
			if (instruction.isParsingError()) {return;}
			sqlDelete.setWhere(sqlWhere);
			sqlDelete.setSearchedDelete(true);
			
	    // where-current-clause				
		} else {
			scn = new Scanner(strWhereClause);
			this.strNoMemoryToParse = strWhereClause;
			token = nextToken(scn);							// WHERE
			token = nextToken(scn);							// CURRENT
			token = nextToken(scn);							// OF
			token = nextToken(scn);							// cursor-name
			sqlDelete.setCursorName(token);
			sqlDelete.setPositionedDelete(true);
		}
	}


    /* -------------------------------
     * Analisi options di delete
     * -------------------------------
     * 
     * 
     */
	private void analyzeSqlDeleteOptions(InnerContextAnalysis ictx, InstructionSql instruction, SqlDeleteStatement sqlDelete, String strOptions) {

		Scanner scn = null;
		String token = "";
 		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Input vuoto
		if (strOptions.equals("")) {
			return;
		}
		
		
		scn = new Scanner(strOptions);
		this.strNoMemoryToParse = strOptions;
		token = nextToken(scn);							 
		
		// Scan tokens
        while (!token.equals("")) {
			
        	// SKIP LOCKED DATA
        	if (token.equals("SKIP")) {
        		token = nextToken(scn);					// LOCKED
           		token = nextToken(scn);					// DATA
           		sqlDelete.setSkipLockData(true);
           		token = nextToken(scn);
           		continue;
			}
        	// WITH RR|RS|CS
        	if (token.equals("WITH")) {
        		token = nextToken(scn);					// RR|RS|CS	
        		sqlDelete.setIsolation(token);
        		sqlDelete.setIsolation(true);
        		token = nextToken(scn);
        		continue;
        	}
        	
        	// QUERYNO
        	if (token.equals("QUERYNO")) {
        		token = nextToken(scn);					// num
        		sqlDelete.setQueryno(StringService._getNumericInt(token));
        		sqlDelete.setQueryno(true);
            	token = nextToken(scn);
            	continue;
        	}
        	
        	// Opzione non gestita
        	token = nextToken(scn);
		}

	}



	
	/* -------------------------
	 * Analisi statement Insert
	 * -------------------------
	 * 
	 * 
	 */
	private void analyzeSqlInsert(InnerContextAnalysis ictx, InstructionSql instruction) throws SQLException, ExceptionAmrita{
		
  	   	Scanner scn = null;
        SqlInsertStatement sqlInsert = null;
		ArrayList<String> al_literalAlpha = null;
  	   	ArrayList<String> al_literalNum = null;
  	   	ArrayList<ArrayList<String>> al_al_entity = null;
   	   	ArrayList<String> al_hostVar = null;
  	   	String ar_columnName[] = null;
  	   	String strColumnNames = null;
   	    String[] ar_parmInclude = null;
   		String str = "";
        String strBetweenPar = "";
 		String token = "";
 		String entityName = "";
 		String entityOwner = "";
 		
    		
		// Allocazione strutture di lavoro
 		sqlInsert = new SqlInsertStatement();
 		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		str = instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		scn = new Scanner(str);											//  
		
		token = nextToken(scn);											// INSERT
		token = nextToken(scn);											// INTO
		token = nextToken(scn);											// tbName/viewName
		sqlInsert.setEntityNameQualified(token);						// Con eventuale owner di qualificazione
		
		// ( Columns )
		token = nextToken(scn);											// ( ?
		if (token.equals("(") && !nextTokenWithoutScannerMod(scn).equals("SELECT")) {
			strColumnNames = extractStringBetweenPar(scn);              //
			ar_columnName = splitParms(strColumnNames);
			// Scan colonne
			for (String columnName : ar_columnName) {
				sqlInsert.getColumnsName().add(columnName);
			}
			token = nextToken(scn);										// INCLUDE ?

			// INCLUDE (col data-type, ... )
			if (token.equals("INCLUDE")) {
				sqlInsert.setIncludeColumns(true);
				token = nextToken(scn);									// ( 
				strBetweenPar = extractStringBetweenPar(scn);			// ( content )
				ar_parmInclude = splitParms(strBetweenPar);
				for (String parmInclude : ar_parmInclude) {
					sqlInsert.getColumnsInclude().add(parmInclude);
				}
				token = nextToken(scn);									// OVERRIDING|VALUES|WITH|SELECT ?
			}
			
			// OVERRIDING USER VALUE
	        if (token.equals("OVERRIDING")) {
	        	sqlInsert.setOverridingUserValue(true);
	           	token = nextToken(scn);									// USER
	           	token = nextToken(scn);									// VALUES|(SELECT ...
	           	token = nextToken(scn);										 
			}
		}
		
		
		////////////////////////////////////////////////////////////////////
		// Parser sezioni insert
		///////////////////////////////////////////////////////////////////
		
       	// VALUES
        if (token.equals("VALUES")) {
        	// expr|DEFAULT|NULL | (expr1, DEFAULT, NULL, ...)
        	analyzeSqlInsertValues(ictx, instruction, scn, sqlInsert);
         } else {
            // fullselect
        	analyzeSqlInsertFullselect(ictx, instruction, scn, sqlInsert, token);
 		}

		// Parsing error
		if (instruction.isParsingError()) {
			return;
		}
		
		
		////////////////////////////////////////////////////////////////////
		// Caricamento simboli nell'istruzione e descrittore
		///////////////////////////////////////////////////////////////////		
		
   		// Simboli literal alfanumeriche
   		al_literalAlpha = sqlInsert.getConstantsAlphanumeric();
		for (String literalAlpha : al_literalAlpha) {
			instruction.addSymbolInput(literalAlpha, EnumSymbolType.SQL_SYMBOL_LITERAL_ALPHA);
		}

   		// Simboli literal numeriche
   		al_literalNum = sqlInsert.getConstantsNumeric();
		for (String literalNum : al_literalNum) {
			instruction.addSymbolInput(literalNum.toString(), EnumSymbolType.SQL_SYMBOL_LITERAL_NUM);
		}
   		
		// Symboli nomi tabelle/view in input per l'istruzione
		al_al_entity = sqlInsert.getEntities();
		for (ArrayList<String> al_entity : al_al_entity) {
			instruction.addSymbolInput(al_entity.get(0), EnumSymbolType.SQL_SYMBOL_TABLE_NAME);
		}
		
   		// Simboli host var in input per l'istruzione  
  	   	al_hostVar = sqlInsert.getHostVars();
  	   	for (String hostVar : al_hostVar) {
	   		instruction.addSymbolInput(hostVar, EnumSymbolType.SQL_SYMBOL_HOST_VAR);
 		}
        
  	   	// Descrittore istruzione INSERT
    	instruction.sqlInsertSetDescriptor(sqlInsert);


    	////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////

  	    entityName = sqlInsert.getEntityName();
  	    entityOwner = sqlInsert.getEntityOwner();
  	    
  	   	// Oggetto tabella in update e owner
   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   		if (!entityOwner.equals("")) {
   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,entityOwner, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
		}
   		
   	    // PGM_SQL_OWNER
   		if (!entityOwner.equals("")) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
  		}
		// PGM_ENTITY
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_INQUIRY);
   	    }
		// PGM_ENTITY_INSERT
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY_INSERT, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY_INSERT, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_UPDATE);
   	    }
        // ENTITY_SQL_OWNER
   	    if (!entityOwner.equals("")) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
   	    }
   	    // ENTITY_SQL_SCRIPT
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_WITH_I_O_SQL);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_INQUIRY);
   	    }

  	   	
		// Scan tabelle dichiarate direttamente, in subselect e fullselect
   	   	for (ArrayList<String> al_entity : al_al_entity) {
   	   		
   	   		entityName = al_entity.get(0);
   	   	    entityOwner = al_entity.get(1);
   	   		
   	   	    // Oggetto tabella e owner
   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   	  		if (!entityOwner.equals("")) {
   	   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,entityOwner, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   			}
  	   		
   	   	    // PGM_SQL_OWNER
   	   		if (!entityOwner.equals("")) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
   	  		}
   		    // PGM_ENTITY
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_INQUIRY);
   	   	    }
   		    // PGM_ENTITY_READ
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY_READ, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY_READ, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
   	   	    }
   	        // ENTITY_SQL_OWNER
   	   	    if (!entityOwner.equals("")) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
   	   	    }
   		    // ENTITY_SQL_SCRIPT
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_WITH_I_O_SQL);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_INQUIRY);
   	   	    }
		}
   	   	
   	   	// N.B. I Where-used delle colonne vengono inseriti nel programma chiamante
        // dove sono presenti tutte le informazioni di where used che dipendono
   	   	// dall'analisi e dalle variabili del programma.
		
	}
	
	
	

	/*
	 * -------------------------------------------
	 * Analisi costrutto VALUES di insert 
	 * -------------------------------------------
	 * 
	 * VALUES   expression|DEFAULT|NULL
	 * VALUES ( expression|DEFAULT|NULL, ...)
	 * VALUES ( expression|DEFAULT|NULL, ...) |FOR host-variable ROWS
	 * VALUES ( expression|DEFAULT|NULL, ...) |FOR num ROWS  |ATOMIC|NOT ATOMIC CONTINUE ON SQLEXCEPTION
	 * 
	 */
	private void analyzeSqlInsertValues(InnerContextAnalysis ictx, InstructionSql instruction, Scanner scn, SqlInsertStatement sqlInsert) throws SQLException, ExceptionAmrita {
		
        SqlExpression sqlExpression = null;
   	    String[] ar_parmExpression = null;
        String strBetweenPar = "";
        String strExpression = "";
        StringBuffer sbExpression = null;
 		String token = "";
 		
  		token = nextToken(scn);										// ( ?
  		
    	// Occorrenze di espressioni fra parentesi separate da virgola 
 		if (token.equals("(")) {
 			sqlInsert.setValuesByList(true);
    		strBetweenPar = extractStringBetweenPar(scn);
			ar_parmExpression = splitParms(strBetweenPar);
			for (String parmExpression : ar_parmExpression) {
				sqlExpression = analyzeSqlExpression(ictx, instruction, parmExpression);
				sqlInsert.getValueExpressions().add(sqlExpression);
			}
			token = nextToken(scn);									// FOR|ATOMIC|NOT ATOMIC|
		}
 	    // Una sola espressione 
    	else {
    		sbExpression = new StringBuffer();
			while (!token.equals("") && !token.equals("FOR") && !token.equals("ATOMIC") && !token.equals("NOT")) {
				sbExpression.append(" " + token);
				strExpression = strExpression + token;
				token = nextToken(scn);
			}
			sqlExpression = analyzeSqlExpression(ictx, instruction, sbExpression.toString().trim());
			if (sqlExpression.getElements().get(0).getTypeElement() == EnumSqlExpressionElementType.DEFAULT ) {
				sqlInsert.setValuesByDefault(true);
			} else if (sqlExpression.getElements().get(0).getTypeElement() == EnumSqlExpressionElementType.NULL ) {
				sqlInsert.setValuesByNull(true);
			} else if (sqlExpression.getElements().get(0).getTypeElement() == EnumSqlExpressionElementType.HOST_VAR ) {
				sqlInsert.setValuesByHostVarArray(true);
			} else {
				sqlInsert.setValuesByExpression(true);
			}
		}
 		
 		// FOR num|host-var ROWS
 		if (token.equals("FOR")) {
 			token = nextToken(scn);									// host-var|num
 			if (StringService._isNumeric(token)) {
 				sqlInsert.setForRowNumber(StringService._getNumericInt(token));
				sqlInsert.setForRowsNumber(true);

			} else {
				sqlInsert.setForRowHostVar(token);
				sqlInsert.setForRowsHostVar(true);
			}
 			token = nextToken(scn);									// ROWS
 			token = nextToken(scn);									
		}
 		
 		// ATOMIC
		if (token.equals("ATOMIC")) {
 			sqlInsert.setAtomic(true);
 			token = nextToken(scn);	
 			return;
		}
		
		// NOT ATOMIC CONTINUE ON SQLEXCEPTION
		if (token.equals("NOT")) {
 			sqlInsert.setNotAtomicContinue(true);
   			token = nextToken(scn);									// ATOMIC
   			token = nextToken(scn);									// CONTINUE
  			token = nextToken(scn);									// ON
  			token = nextToken(scn);									// SQLEXCEPTION
  			return;
		}
		
	}

	/* -------------------------------------------
	 * Analisi fullselect di insert 
	 * -------------------------------------------
	 * 
	 * Si tratta della sequenza dei costrutti:
	 * 
	 *  1) WITH common-table-descriptor (opzionale)
	 *  2) full-select
	 *  3) isolation clause (opzionale)
	 *  4) queryno clause   (opzionale)
	 *  
	 * Si utilizza lo scanner fornito, posizionato sul primo token da trattare
	 * 
	 */
	private void analyzeSqlInsertFullselect(InnerContextAnalysis ictx, InstructionSql instruction, Scanner scn, SqlInsertStatement sqlInsert, String tokenFirst) throws SQLException, ExceptionAmrita {

		Scanner scnOptions = null;
		SqlFullSelect fullselect = null;
		ArrayList<SqlCommonTableExpression> al_commonTableExpression = null;
		StringBuffer sbCommonTableExpressions = null;
		String strFullSelect= "";
		String strFullSelectAndOptions = "";
		String strOptions = "";
		String svStrNoMemoryToParse = "";
		String token = "";
		int iOptions = 0;
		int iWith = 0;
		int iQueryno = 0;
		
		token = tokenFirst;
		
 		// WITH common-table-expression: estrazione completa
		if (token.equals("WITH")) {
			sqlInsert.setWithCommonTableExpression(true);
			sbCommonTableExpressions = new StringBuffer ();
			sbCommonTableExpressions.append(token);
			while (!token.equals("")) {
				token = nextToken(scn);									// table-identifier
				sbCommonTableExpressions.append(" " + token);
				if (token.equals("AS")) {
					token = nextToken(scn);          					// (
					strFullSelect = extractStringBetweenPar(scn);
					sbCommonTableExpressions.append(" ( " + strFullSelect + " )");      
					token = nextToken(scn);								// ,|SELECT ?
					// Fine common-table-expression
					if (token.equals("SELECT")) {
						break;
					}
				}       
			} // end-while common-table-expression
			svStrNoMemoryToParse = this.strNoMemoryToParse;
			al_commonTableExpression = analyzeSqCommonTableExpressions(ictx, instruction, sbCommonTableExpressions.toString().trim());
			this.strNoMemoryToParse = svStrNoMemoryToParse;
			if (instruction.isParsingError()) {return;}
			sqlInsert.setCommonTableExpressions(al_commonTableExpression);
		}
		
		// Può essere solo full-select o (full-select)
		if (!token.equals("SELECT") && !token.equals("(")) {
			instruction.setParsingError(true);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
			return;
		}
		
		// Full select + eventuali opzioni, corpo dell'istruzione insert con input da tabella
		strFullSelectAndOptions = token + " " + this.strNoMemoryToParse;
		
		iWith = strFullSelectAndOptions.indexOf(" WITH ");
		iQueryno = strFullSelectAndOptions.indexOf(" QUERYNO ");
		if (iWith > 0) {
			iOptions = iWith;
		} else if (iQueryno > 0) {
			iOptions = iQueryno;
		}
		
		// Estrazione fullselect opzioni
		if (iOptions > 0) {
			strOptions = strFullSelectAndOptions.substring(iOptions).trim();
			strFullSelect = strFullSelectAndOptions.substring(0, iOptions).trim();
		} else {
			strFullSelect = strFullSelectAndOptions.trim();
			// Eliminazione eventuali parentesi ((.. fullselect ..))
            while (strFullSelect.startsWith("(")) {
            	strFullSelect = strFullSelect.substring(1).trim();
            	if (!strFullSelect.endsWith(")")) {
        			instruction.setParsingError(true);
        			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0041", token, null, this.strProgramScript, this.strProgramScriptObj);
        			return;
				}
            	strFullSelect = strFullSelect.substring(0, strFullSelect.length() - 1).trim();
			}
		}

		////////////////////////////////////////////////////////////////////
		// Parser 
		///////////////////////////////////////////////////////////////////

		// fullselect
		fullselect = analyzeSqlFullselect(ictx, instruction, strFullSelect);
		if (instruction.isParsingError()) {return;}
		sqlInsert.setFullSelect(fullselect);
		sqlInsert.setValuesByFullselect(true);
		
		// Opzioni
		scnOptions = new Scanner(strOptions);
		this.strNoMemoryToParse = strOptions;
		token = nextToken(scnOptions);
		
		// Isolation
		if (token.equals("WITH")) {
			token = nextToken(scnOptions);  				// RR|RS|CS
			sqlInsert.setIsolation(true);
			sqlInsert.setIsolation(token);
			token = nextToken(scnOptions);
		}
		
		// Queryno
		if (token.equals("QUERYNO")) {
			token = nextToken(scnOptions);  				// integer
			sqlInsert.setQueryno(true);
			sqlInsert.setQueryno(StringService._getNumericInt(token));
			token = nextToken(scnOptions);
		}
		
	}


	/* -------------------------
	 * Analisi statement Merge
	 * -------------------------
	 * 
	 * 
	 */
	private void analyzeSqlMerge(InnerContextAnalysis ictx, InstructionSql instruction) throws SQLException, ExceptionAmrita{
		
        SqlMergeStatement sqlMerge = null;
		ArrayList<String> al_literalAlpha = null;
  	   	ArrayList<String> al_literalNum = null;
  	   	ArrayList<ArrayList<String>> al_al_entity = null;
   	   	ArrayList<String> al_hostVar = null;
    	String str = "";
        String strInto = "";										// INTO table|view |AS corr-name INCLUDE include-columns
        String strUsing = "";										// USING source-table ON search-condition
        String strUsingSourceTable = "";							// source table
        String strUsingOnSearchCondition = "";						// search condition
        String strWhenThen = "";									// WHEN ... THEN ... WHEN ... THEN ...
        String strWhenThenSingle = "";								// WHEN ... THEN  
        ArrayList<String> al_strWhenThenSingle = null;				// WHEN ... THEN
        String strOptions = "";										// NOT ATOMIC ... QUERYNO ..
        String strWork = "";										 
  		String entityName = "";
 		String entityOwner = "";
		int i = 0;
		int iStart = 0;
		int iEnd = 0;
		int iInto = 0;
		int iIntoEnd = 0;
		int iUsing = 0;
		int iUsingEnd = 0;
		int iUsingOn = 0;
		int iWhenThen = 0;
		int iWhenThenEnd = 0;
		int iOptions = 0;
		int iOptionsEnd = 0;
    		
		// Allocazione strutture di lavoro
 		sqlMerge = new SqlMergeStatement();
 		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		str =instruction.getSourceInstr(); 
		str = str.substring(6).trim();								// Eliminazione MERGE
		
		
		////////////////////////////////////////////////////////////
		// Individuazione sezioni per PARSE
		///////////////////////////////////////////////////////////
		
		iUsing = str.indexOf(" USING ");
		iIntoEnd = iUsing;
		iUsingOn = str.indexOf(" ON ", iUsing + 7);
		iWhenThen = str.indexOf(" WHEN ", iUsingOn + 5);
		iUsingEnd = iWhenThen;
		i =str.indexOf(" NOT ", iWhenThen + 6);
		while (i > 0) {
			strWork = str.substring(i + 5).trim();
			if (strWork.startsWith("ATOMIC ")) {
				iOptions = i;
				iOptionsEnd = str.length();
				break;
			}
			i =str.indexOf(" NOT ", i + 5);
		}
		// NOT ATOMIC CONTINUE ON SQLEXCEPTION non codificata: verifica esistenza QUERYNO
		if (iOptions <= 0) {
			iOptions = str.indexOf(" QUERYNO ");
			if (iOptions > 0) {
				iOptionsEnd = str.length();
			}
		}

		
		////////////////////////////////////////////////////////////
		// Estrazione sezioni per PARSE
		///////////////////////////////////////////////////////////
		
        strInto = str.substring(iInto, iIntoEnd).trim();
        strUsing = str.substring(iUsing, iUsingEnd).trim();
        strWhenThen = str.substring(iWhenThen, iWhenThenEnd).trim();
		
        // USING source-table ON search-condition
        iUsingOn = strUsing.indexOf(" ON ");
        strUsingSourceTable = strUsing.substring(iUsing + 7, iUsingOn).trim();
        strUsingOnSearchCondition = strUsing.substring(iUsingOn + 4).trim();
        
        // WHEN ... THEN ...
        al_strWhenThenSingle = new ArrayList<String> ();
        iStart = 0;
        iEnd = strWhenThen.indexOf(" WHEN ", iStart + 5);
        
        // Scan occorrenze WHEN ... THEN ...
        while (iStart >= 0 ) {
        	iEnd = strWhenThen.indexOf(" WHEN ", iStart + 5);
        	// Ultimo elemento When ... THEN ...
        	if (iEnd < 0) {
				iEnd = strWhenThen.length();
			}
			strWhenThenSingle = strWhenThen.substring(iStart, iEnd).trim();
			al_strWhenThenSingle.add(strWhenThenSingle);
			iStart = strWhenThen.indexOf(" WHEN ", iStart + 5);
		}
 
        // options
        if (iOptions > 0) {
			strOptions = str.substring(iOptions, iOptionsEnd);
		}

        
        ////////////////////////////////////////////////////////////
		// PARSE sezioni estratte
		///////////////////////////////////////////////////////////
		
        analyzeSqlMergeInto(ictx, instruction, sqlMerge, strInto);
        analyzeSqlMergeUsingSourceTable(ictx, instruction, sqlMerge, strUsingSourceTable);
        analyzeSqlMergeUsingOnSearchCondition(ictx, instruction, sqlMerge, strUsingOnSearchCondition);
        analyzeSqlMergeWhenThenOccurs(ictx, instruction, sqlMerge, al_strWhenThenSingle);
        analyzeSqlMergeOptions(ictx, instruction, sqlMerge, strOptions);
		
		// Parsing error
		if (instruction.isParsingError()) {
			return;
		}
		
		
		////////////////////////////////////////////////////////////////////
		// Caricamento simboli nell'istruzione e descrittore
		///////////////////////////////////////////////////////////////////		
		
   		// Simboli literal alfanumeriche
   		al_literalAlpha = sqlMerge.getConstantsAlphanumeric();
		for (String literalAlpha : al_literalAlpha) {
			instruction.addSymbolInput(literalAlpha, EnumSymbolType.SQL_SYMBOL_LITERAL_ALPHA);
		}

   		// Simboli literal numeriche
   		al_literalNum = sqlMerge.getConstantsNumeric();
		for (String literalNum : al_literalNum) {
			instruction.addSymbolInput(literalNum.toString(), EnumSymbolType.SQL_SYMBOL_LITERAL_NUM);
		}
   		
		// Symboli nomi tabelle/view in input per l'istruzione
		al_al_entity = sqlMerge.getEntities();
		for (ArrayList<String> al_entity : al_al_entity) {
			instruction.addSymbolInput(al_entity.get(0), EnumSymbolType.SQL_SYMBOL_TABLE_NAME);
		}
		
   		// Simboli host var in input per l'istruzione  
  	   	al_hostVar = sqlMerge.getHostVars();
  	   	for (String hostVar : al_hostVar) {
	   		instruction.addSymbolInput(hostVar, EnumSymbolType.SQL_SYMBOL_HOST_VAR);
 		}
        
  	   	// Descrittore istruzione MERGE
    	instruction.sqlMergeSetDescriptor(sqlMerge);


    	////////////////////////////////////////////////////////////////////
		// Predisposizione per Updates finali
		///////////////////////////////////////////////////////////////////

  	    entityName = sqlMerge.getEntityName();
  	    entityOwner = sqlMerge.getEntityOwner();
  	    
  	   	// Oggetto tabella in update e owner
   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   		if (!entityOwner.equals("")) {
   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,entityOwner, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
		}
   		
   	    // PGM_SQL_OWNER
   		if (!entityOwner.equals("")) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
  		}
		// PGM_ENTITY
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_INQUIRY);
   	    }
		// PGM_ENTITY_INSERT
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY_INSERT, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY_INSERT, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_UPDATE);
   	    }
        // ENTITY_SQL_OWNER
   	    if (!entityOwner.equals("")) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
   	    }
   	    // ENTITY_SQL_SCRIPT
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_WITH_I_O_SQL);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_INQUIRY);
   	    }

  	   	
		// Scan tabelle dichiarate direttamente, in subselect e fullselect
   	   	for (ArrayList<String> al_entity : al_al_entity) {
   	   		
   	   		entityName = al_entity.get(0);
   	   	    entityOwner = al_entity.get(1);
   	   		
   	   	    // Oggetto tabella e owner
   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   	  		if (!entityOwner.equals("")) {
   	   	   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER,entityOwner, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   			}
  	   		
   	   	    // PGM_SQL_OWNER
   	   		if (!entityOwner.equals("")) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
   	  		}
   		    // PGM_ENTITY
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_INQUIRY);
   	   	    }
   		    // PGM_ENTITY_READ
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY_READ, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY_READ, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
   	   	    }
   	        // ENTITY_SQL_OWNER
   	   	    if (!entityOwner.equals("")) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
   				this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
   	   	    }
   		    // ENTITY_SQL_SCRIPT
   	   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
   	   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, this.userExitInfoPgm);
    			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_WITH_I_O_SQL);
    			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_INQUIRY);
   	   	    }
		}
   	   	
   	   	// N.B. I Where-used delle colonne vengono inseriti nel programma chiamante
        // dove sono presenti tutte le informazioni di where used che dipendono
   	   	// dall'analisi e dalle variabilòi del programma.
		
	}
	
	
	/* -----------------------------------
	 * Analisi clausola INTO di sql merge
	 * -----------------------------------
	 * 
	 */
	private void analyzeSqlMergeInto(InnerContextAnalysis ictx, InstructionSql instruction, SqlMergeStatement sqlMerge, String strInto) {

        Scanner scn = null;
		String token = "";
		String strBetweenPar = "";
		String ar_parmInclude[] = null;
		
        scn = new Scanner(strInto);
        this.strNoMemoryToParse = strInto;
		
		token = nextToken(scn);											// INTO
		token = nextToken(scn);											// tbName/viewName
		sqlMerge.setEntityNameQualified(token);							// Con eventuale owner di qualificazione
		
		token = nextToken(scn);											// AS|INCLUDE ?
		if (token.equals("AS")) {
			token = nextToken(scn);										 
		}
		
		// correlation-name
		if (!token.equals("INCLUDE")) {
			sqlMerge.setCorrelationName(token);
			token = nextToken(scn);										// INCLUDE ?									 
		}

		// INCLUDE (col data-type, ... )
		if (token.equals("INCLUDE")) {
			token = nextToken(scn);										// ( 
			strBetweenPar = extractStringBetweenPar(scn);				// ( col1 data-type1 , col2 data-type2 , ... )
			ar_parmInclude = splitParms(strBetweenPar);
			for (String parmInclude : ar_parmInclude) {
				sqlMerge.getColumnsInclude().add(parmInclude);
			}
		}
	}


	/* ----------------------------------------------------
	 * Analisi source-table di clausola USING di sql merge
	 * ----------------------------------------------------
	 * 
	 * USING source-table ON search-condition
	 * 
	 * Dove source-table è nella forma:
	 * 
	 * ( VALUES   expr|NULL|hostVarArray                                  )
	 * ( VALUES   expr|NULL|hostVarArray                                  ) FOR hostVar|number ROWS
	 * ( VALUES ( expr1|NULL|hostVarArray1, expr2|NULL|hostVarArray1, ... ) FOR hostVar|number ROWS
	 * 
	 * Seguito obbbligatoriamente da:
	 * 
	 * |AS correlation-name ( colName1 , ..., colNamen)
	 * 
	 */
	private void analyzeSqlMergeUsingSourceTable(InnerContextAnalysis ictx, InstructionSql instruction, SqlMergeStatement sqlMerge, String strUsingSourceTable) throws SQLException, ExceptionAmrita {
		
        Scanner scn = null;
        SqlExpression sqlExpression = null;
		String token = "";
		String strSourceTableValues = "";
		String strSourceTableCorrelation = "";
		String strForRows = "";
		String strExpressions = "";
		String[] ar_expression = null;
		int iForRows = 0;
		
        scn = new Scanner(strUsingSourceTable);
        this.strNoMemoryToParse = strUsingSourceTable;
		
		token = nextToken(scn);											// (
		token = nextToken(scn);											// VALUES ( tableValues ) AS corr-name (col1 , coln)
		token = nextToken(scn);											// (  
		strSourceTableValues = extractStringBetweenPar(scn);			//  tableValues  )
		strSourceTableCorrelation = this.strNoMemoryToParse;   			// AS corr-name (col1 , coln)

		
		
		///////////////////////////////////////////////
		// Analisi  val di VALUES ( tableValue  )
		///////////////////////////////////////////////
		
		// Estrazione eventuale FOR ... ROWS
		iForRows = strSourceTableValues.indexOf(" FOR ");
		if (iForRows > 0) {
			strForRows = strSourceTableValues.substring(iForRows).trim();
			strSourceTableValues = strSourceTableValues.substring(0, iForRows).trim();
		}
		
        scn = new Scanner(strSourceTableValues);
        this.strNoMemoryToParse = strSourceTableValues;
			
        token = nextToken(scn);										  

	    // espressioni multiple fra parntesi
        if (token.equals("(")) {
        	strExpressions = extractStringBetweenPar(scn);
        	ar_expression = splitParms(strExpressions);
        	
        // Espressione singola
 		} else {
 			ar_expression = new String[1];
  			strExpressions = token + " " + this.strNoMemoryToParse;
  			ar_expression[0] = strExpressions;
		}

        // Scan e analisi espressioni a fronte di modalità singola e multipla
        for (String strExpression : ar_expression) {
			sqlExpression = analyzeSqlExpression(ictx, instruction, strExpression);
			if (instruction.isParsingError()) {return;}
			sqlMerge.getUsingSourceValueExpressions().add(sqlExpression);
		}
        
        // Analisi FOR ... ROWS
        if (!strForRows.equals("")) {
            scn = new Scanner(strForRows);
            this.strNoMemoryToParse = strForRows;
            token = nextToken(scn);									// FOR
            token = nextToken(scn);									// hostVar|Number
            if (StringService._isNumericInt(token)) {
            	sqlMerge.setForRowNumber(StringService._getNumericInt(token));
            	sqlMerge.setUsingForRowNumber(true);
			} else {
	           	sqlMerge.setForRowHostVar(token);
            	sqlMerge.setUsingForRowHostVar(true);
			}
		}
		
		
		///////////////////////////////////////////////
		// Analisi  AS corr-name (col1 , coln)
		///////////////////////////////////////////////
		
        scn = new Scanner(strSourceTableCorrelation);
        this.strNoMemoryToParse = strUsingSourceTable;
		
		token = nextToken(scn);											// AS ?
		if (token.equals("AS")) {
			token = nextToken(scn);										 
		}
		
		// correlation-name
		sqlMerge.setUsingSourceValueAsCorrName(token);
		token = nextToken(scn);											// (									 
        
		// correlation-columns
		while (!token.equals("") && !token.equals(")")) {
			if (token.equals(",")) {
				token = nextToken(scn);		
				continue;
			}
			sqlMerge.getUsingSourceValueAsCorrNameColumns().add(token);
			token = nextToken(scn);										 
		}

        
	}


	/* --------------------------------------------------------
	 * Analisi search-condition di clausola USING di sql merge
	 * --------------------------------------------------------
	 * 
	 * USING source-table ON search-condition
	 * 
	 * 
	 */
	private void analyzeSqlMergeUsingOnSearchCondition(InnerContextAnalysis ictx, InstructionSql instruction, SqlMergeStatement sqlMerge, String strUsingOnSearchCondition) throws SQLException, ExceptionAmrita {
		
		SqlSearchConditions sqlSearchConditions = null;
		
		// Errore di parsing precedenti
		if (instruction.isParsingError()) {
			return;
		}
				
		sqlSearchConditions = analyzeSqlSearchConditions(ictx, instruction, strUsingOnSearchCondition);
		if (instruction.isParsingError()) {return;}

		sqlMerge.setUsingSearchConditions(sqlSearchConditions);
		
		
	}

	/* --------------------------------------------------------
	 * Analisi clausole WHEN ... THEN di sql merge
	 * --------------------------------------------------------
	 * 
	 * WHEN matching-condition1 THEN modification-operation1 
	 * WHEN matching-condition2 THEN modification-operation2
	 * 
	 * 
	 */
	private void analyzeSqlMergeWhenThenOccurs(InnerContextAnalysis ictx
											 , InstructionSql instruction
											 , SqlMergeStatement sqlMerge
											 , ArrayList<String> al_strWhenThen 
								              ) throws SQLException, ExceptionAmrita {

		Scanner scn = null;
		SqlMergeWhenThen sqlMergeWhenThen = null;
		SqlExpression sqlExpression = null;
		String[] ar_strExpression = null;
		String[] ar_strAssignment = null;
		String[] ar_expressionAssigned = null;
		String strExpressionAssigned = "";
		String strAssignments = "";
		String strColumnsToAssign = "";
		String strExpressionsAssigned = "";
		String strExpressions = "";
		String columnName = "";
		String token = "";
        int iEqual = 0;
		int iParClose = 0;
        
		
		// Errore di parsing precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Scan occorrenze WHEN ... THEN
		for (String strWhenThen : al_strWhenThen) {
			
			sqlMergeWhenThen = new SqlMergeWhenThen();
			
			scn = new Scanner(strWhenThen);
			this.strNoMemoryToParse = strWhenThen;

			// WHEN ... THEN ...
			token = nextToken(scn);							// WHEN
			token = nextToken(scn);							// |NOT|MATCH ?
			if (token.equals("NOT")) {
				sqlMergeWhenThen.setNotMatched(true);
				token = nextToken(scn);						// MATCH
			} else {
				sqlMergeWhenThen.setMatched(true);
			}
			token = nextToken(scn);							// THEN
			token = nextToken(scn);							// UPDATE|INSERT
			
			
			///////////////////////////////////////////////////////////////////
			// UPDATE SET
			///////////////////////////////////////////////////////////////////
			
			if (token.equals("UPDATE")) {
				token = nextToken(scn);						// SET
				strAssignments = this.strNoMemoryToParse;
				ar_strAssignment = splitParms(strAssignments);	// include assegnazioni a colonne o a espressioni fra parentesi
                // scan assegnazioni
				for (String strAssegnment : ar_strAssignment) {
					// (col1, .. coln) = ( expr1, .. exprn)
					if (strAssegnment.startsWith("(")) {
						iParClose = strAssegnment.indexOf(")");
						strColumnsToAssign = strAssegnment.substring(1, iParClose).trim();
						scn = new Scanner(strColumnsToAssign);
						this.strNoMemoryToParse = strColumnsToAssign;
						token = nextToken(scn);
						while (!token.equals("") && !token.equals(")")) {
							if (token.equals(",")) {
								token = nextToken(scn);
								continue;
							}
							columnName = token;
							sqlMergeWhenThen.getUpdateSetColumnNames().add(columnName);
							token = nextToken(scn);
						}
						
						// 
						iEqual = strColumnsToAssign.indexOf(" = ", iParClose + 1);
						strExpressionsAssigned = strAssegnment.substring(iEqual + 3).trim();
						ar_expressionAssigned = splitParms(strExpressionsAssigned);
						// Scan espressioni assegnate
						for (String strExpressionAssigned2 : ar_expressionAssigned) {
							sqlExpression = analyzeSqlExpression(ictx, instruction, strExpressionAssigned2);
							sqlMergeWhenThen.getInsertValuesExpressions().add(sqlExpression);
						}
						
						sqlMerge.getWhenThens().add(sqlMergeWhenThen);
						continue;
					}
                    
					// col = expr
					iEqual = strAssegnment.indexOf(" = ");
					columnName = strAssegnment.substring(0, iEqual).trim();
					sqlMergeWhenThen.getUpdateSetColumnNames().add(columnName);
					strExpressionAssigned = strAssegnment.substring(iEqual + 3).trim();
					sqlExpression = analyzeSqlExpression(ictx, instruction, strExpressionAssigned);
					if (instruction.isParsingError()) {return;}
					sqlMergeWhenThen.getInsertValuesExpressions().add(sqlExpression);
				}
				sqlMerge.getWhenThens().add(sqlMergeWhenThen);
				continue;
			}
			
			
			////////////////////////////////////////////////////////////////////
			// INSERT ... VALUES ...
			////////////////////////////////////////////////////////////////////
			
			token = nextToken(scn);							// ( cols )
			while (!token.equals("") && !token.equals(")")) {
				if (token.equals(",")) {
					token = nextToken(scn);	
					continue;
				}
				sqlMergeWhenThen.getInsertColumnNames().add(token);
				token = nextToken(scn);	
			}
			
			token = nextToken(scn);							// VALUES
			
			token = nextToken(scn);							// ( ?
			if (token.equals("(")) {
				strExpressions = extractStringBetweenPar(scn);
				ar_strExpression = splitParms(strExpressions);
			} else {
				ar_strExpression = new String[1];
				ar_strExpression[0] = this.strNoMemoryToParse;
			}
			
			// Scan espressioni multiple o singola
			for (String strExpression : ar_strExpression) {
				sqlExpression = analyzeSqlExpression(ictx, instruction, strExpression);
				if (instruction.isParsingError()) {return;}
				sqlMergeWhenThen.getInsertValuesExpressions().add(sqlExpression);
			}
			
			sqlMerge.getWhenThens().add(sqlMergeWhenThen);
			continue;
			
		} // end-for when then

		
	}


	/* --------------------------------------------------------
	 * Analisi opzioni Merge
	 * --------------------------------------------------------
	 * 
	 * NOT ATOMIC CONTINUE ON SQLEXCEPTION QUERYNO n
	 * 
	 */
	private void analyzeSqlMergeOptions(InnerContextAnalysis ictx, InstructionSql instruction, SqlMergeStatement sqlMerge, String strOptions) {

		Scanner scn = null;
        String token = "";
		
		// Errore di parsing precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		scn = new Scanner(strOptions);
		this.strNoMemoryToParse = strOptions;
		
		token = nextToken(scn);
		
		// NOT ATOMIC CONTINUE ON SQLEXCEPTION
		if (token.equals("NOT")) {
			token = nextToken(scn);  						// ATOMIC
			token = nextToken(scn);  						// CONTINUE
			token = nextToken(scn);  						// ON
			token = nextToken(scn);  						// SQLEXCEPTION
			token = nextToken(scn);  						// QUERYNO ?
			sqlMerge.setNotAtomicContinue(true);
		}
		
		// QUERYNO
		if (token.equals("QUERYNO")) {
			token = nextToken(scn); 
			sqlMerge.setQueryno(true);
			sqlMerge.setQueryno(StringService._getNumericInt(token));
		}
	}


	/* ----------------------------------
	 * Analisi statement DeclareCursor
	 * ----------------------------------
	 * 
	 * 
	 */
	private void analyzeSqDeclareCursor(InnerContextAnalysis ictx, InstructionSql instruction) throws SQLException, ExceptionAmrita{
		
  	   	Scanner scn = null;
        SqlDeclareCursorStatement sqlDeclareCursor = null;
        SqlSelectStatement sqlSelect = null;
        InstructionSql instructionSelect = null;
   		String str = "";
        String strSelectStatement = "";
 		String token = "";
 		
    		
		// Allocazione strutture di lavoro
 		sqlDeclareCursor = new SqlDeclareCursorStatement();
 		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		scn = new Scanner(str);											//  
		
		token = nextToken(scn);											// DECLARE
		token = nextToken(scn);											// cursor-name
		sqlDeclareCursor.setCursorName(token);							 
		token = nextToken(scn);										    // NO|ASENSITIVE|SENSITIVE|CURSOR										 

		// NO SCROLL
		if (token.equals("NO")) {
			token = nextToken(scn);										// SCROLL
			sqlDeclareCursor.setNoScrollCursor(true);
			token = nextToken(scn);										// CURSOR
		// ASENSITIVE
		} else if (token.equals("ASENSITIVE")) {
			token = nextToken(scn);										// SCROLL
			sqlDeclareCursor.setAsensitiveScrollCursor(true);
			token = nextToken(scn);										// CURSOR
		} else if (token.equals("INSENSITIVE")) {
			token = nextToken(scn);										// SCROLL
			sqlDeclareCursor.setInsensitiveScrollCursor(true);
			token = nextToken(scn);										// CURSOR
		} else if (token.equals("SENSITIVE")) {
			token = nextToken(scn);										// |DYNAMIC|STATIC|SCROLL
			sqlDeclareCursor.setSensitiveScrollCursor(true);
			if (token.equals("DYNAMIC")) {
				sqlDeclareCursor.setSensitiveDynamicScrollCursor(true);
				token = nextToken(scn);									// SCROLL
				token = nextToken(scn);									// CURSOR
			} else if (token.equals("STATIC")) {
				sqlDeclareCursor.setSensitiveStaticScrollCursor(true);
				token = nextToken(scn);									// SCROLL
				token = nextToken(scn);									// CURSOR
			}
		}   

		token = nextToken(scn);											// |WITH|WITHOUT
		
        // holdability, returnability e rowset-positioning
		while (!token.equals("") && !token.equals("FOR")) {
			
			// WITHOUT  
			if (token.equals("WITHOUT")) {
				token = nextToken(scn);									// HOLD|RETURN|ROWSET POSITIONING
				// WITHOUT  HOLD
				if (token.equals("HOLD")) {
			    	sqlDeclareCursor.setWithoutHold(true);
			    	token = nextToken(scn);	
			    	continue;
				}
				// WITHOUT RETURN
				if (token.equals("RETURN")) {
			    	sqlDeclareCursor.setWithoutReturn(true);
			    	token = nextToken(scn);	
			    	continue;
				}
				// WITHOUT ROWSET POSITIONING
				if (token.equals("ROWSET")) {
			    	sqlDeclareCursor.setWithoutRowsetPositioning(true);
			    	token = nextToken(scn);								// POSITIONING
			    	token = nextToken(scn);	
			    	continue;
				}
			}

			// WITH  
			if (token.equals("WITH")) {
				token = nextToken(scn);									// HOLD|RETURN|ROWSET POSITIONING
				// WITH  HOLD
				if (token.equals("HOLD")) {
			    	sqlDeclareCursor.setWithHold(true);
			    	token = nextToken(scn);	
			    	continue;
				}
				// WITH RETURN
				if (token.equals("RETURN")) {
			    	sqlDeclareCursor.setWithReturn(true);
			    	token = nextToken(scn);								// |TO CALLER ?
			    	if (token.equals("TO")) {
			    		token = nextToken(scn);							// CALLER|CLIENT
			    		if (token.equals("CALLER")) {
				    		sqlDeclareCursor.setWithReturnToCaller(true);
					    	token = nextToken(scn);	
					    	continue;
						}
			    		if (token.equals("CLIENT")) {
				    		sqlDeclareCursor.setWithReturnToClient(true);
					    	token = nextToken(scn);	
					    	continue;
						}
			    		// Non previsto
				    	token = nextToken(scn);	
			    	}
			    	continue;
				}

			    // WITH ROWSET POSITIONING
				if (token.equals("ROWSET")) {
			    	sqlDeclareCursor.setWithRowsetPositioning(true);
			    	token = nextToken(scn);								// POSITIONING
			    	token = nextToken(scn);	
			    	continue;
				}
			}
			
    		// Non previsto
	    	token = nextToken(scn);	
	    	continue;

		}
		
		// Può essere solo FOR
		if (!token.equals("FOR")) {
			instruction.setParsingError(true);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", "", null, this.strProgramScript, this.strProgramScriptObj);
			return;
		}
		
		token = nextToken(scn);										// SELECT|Statement-name
		
		// Parsing select-statement
		if (token.equals("WITH") || token.equals("SELECT") || token.equals("(")) {
			strSelectStatement = token + " " + this.strNoMemoryToParse;
			instructionSelect = new InstructionSql();
			instructionSelect.setSourceInstr(strSelectStatement);
			instructionSelect.setTypeInstr(instruction.getTypeInstr());
			instructionSelect.setTypeInstrPrecompiler(instruction.getTypeInstrPrecompiler());
			instructionSelect.setTypePrecompiler(instruction.getTypePrecompiler());
			instructionSelect.setNumInstr(instruction.getNumInstr());
			instructionSelect.setRowStartSource(instruction.getRowStartSource());
			instructionSelect.setRowEndSource(instruction.getRowEndSource());
			sqlSelect = analyzeSqlSelect(ictx, instructionSelect);
			// Parsing error
			if (instructionSelect.isParsingError() || instructionSelect.isSemanticError() ) {
				instruction.setParsingError(instructionSelect.isParsingError());
				instruction.setSemanticError(instructionSelect.isSemanticError());
				instruction.setTokenInError(instructionSelect.getTokenInError());
				instruction.setMsgCode(instructionSelect.getMsgCode());
				instruction.setMsgType(instructionSelect.getMsgType());
				instruction.setMsgParm(instructionSelect.getMsgParm());
				return;
			}
			sqlDeclareCursor.setSelectStatement(sqlSelect);
			sqlDeclareCursor.setBySelectStatement(true);
		
		// statement-name
		} else {
			sqlDeclareCursor.setStatementName(token);
			sqlDeclareCursor.setByStatementName(true);
		}
		
  	   	// Descrittore istruzione DECLARE CURSOR
    	instruction.sqlDeclareCursorSetDescriptor(sqlDeclareCursor);

		// Il cursore non è a fronte di una select-statement: nessuna ulteriore operazione
		if (!sqlDeclareCursor.isBySelectStatement()) {
			return;
		}
		

		///////////////////////////////////////////////////////////////////////////
		// Caricamento simboli nell'istruzione e predisposizione per Updates finali
		////////////////////////////////////////////////////////////////////////////	
		
       	analyzeSqlSelectStoreSymbolsOnInstruction(instruction, sqlSelect);
       	analyzeSqlSelectDbUpdates(instruction, sqlSelect);
  		
	}
	
	
	/* ------------------------------
	 * Analisi statement Open cursor
	 * ------------------------------
	 * 
	 * 
	 */
	private void analyzeSqOpen(InnerContextAnalysis ictx, InstructionSql instruction){
		
 	   	Scanner scn = null;
   		String str = "";
  		String token = "";
		String cursorName = "";
		String usingDescriptorName = "";
		ArrayList<String> al_usingHostVar = null;
		
		// Allocazione strutture di lavoro
		al_usingHostVar = new ArrayList<String> ();
 		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		     
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		scn = new Scanner(str);											//  
		
		token = nextToken(scn);											// OPEN
		token = nextToken(scn);											// cursor-name
		cursorName = token;
		
		token = nextToken(scn);
		if (token.equals("USING")) {
			token = nextToken(scn);										// DESCRIPTOR ?
			
			// USING DESCRIPTOR
			if (token.equals("DESCRIPTOR")) {
				token = nextToken(scn);									// descriptor-name
				usingDescriptorName = token;
			
			// USING host-vars
			} else {
				while (!token.equals("") ) {
					if (token.equals(",")) {
						token = nextToken(scn);	
						continue;
					}
					al_usingHostVar.add(token);
					token = nextToken(scn);	
				}
			}
		}
 
 	   	// Descrittore istruzione OPEN CURSOR
	   	instruction.sqlOpenCursorSetCursorName(cursorName);
	   	if (!usingDescriptorName.equals("")) {
		   	instruction.sqlOpenCursorSetUsingDescriptorName(usingDescriptorName);
		   	instruction.sqlOpenCursorSetUsingDescriptor(true);
		} else if (al_usingHostVar.size() > 0) {
		   	instruction.sqlOpenCursorSetUsingHostVars(al_usingHostVar);
		   	instruction.sqlOpenCursorSetUsingHostVars(true);
		}

		// Symbolo sql cursor name
		instruction.addSymbolInput(cursorName, EnumSymbolType.SQL_SYMBOL_CURSOR_NAME);
		
   		// Simboli host var in input per l'istruzione  
  	   	for (String hostVar : al_usingHostVar) {
	   		instruction.addSymbolInput(hostVar, EnumSymbolType.SQL_SYMBOL_HOST_VAR);
 		}
		
	}
	
	
	/* -------------------------------
	 * Analisi statement Close cursor
	 * -------------------------------
	 * 
	 * 
	 */
	private void analyzeSqClose(InnerContextAnalysis ictx, InstructionSql instruction){
	   	
		Scanner scn = null;
   		String str = "";
  		String token = "";
		String cursorName = "";
 		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		     
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		scn = new Scanner(str);											//  
		
		token = nextToken(scn);											// CURSOR
		token = nextToken(scn);											// cursor-name
		cursorName = token;
		
 
 	   	// Descrittore istruzione OPEN CURSOR
	   	instruction.sqlCloseCursorSetCursorName(cursorName);

		// Symbolo sql cursor name
		instruction.addSymbolInput(cursorName, EnumSymbolType.SQL_SYMBOL_CURSOR_NAME);
		
 	}
	
	
	/* -------------------------
	 * Analisi statement Fetch
	 * -------------------------
	 * 
	 * 
	 */
	private void analyzeSqFetch(InnerContextAnalysis ictx, InstructionSql instruction){

  	   	Scanner scn = null;
        SqlFetchStatement sqlFetch = null;
    	String str = "";
   		String token = "";
 		
    		
		// Allocazione strutture di lavoro
 		sqlFetch = new SqlFetchStatement();
 		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		     
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		scn = new Scanner(str);											//  
		
		token = nextToken(scn);											// FETCH
		token = nextToken(scn);
		
		// INSENSITIVE
		if (token.equals("INSENSITIVE")) {
			sqlFetch.setInsensitive(true);
			token = nextToken(scn);	
		} else if (token.equals("SENSITIVE")) {
			sqlFetch.setSensitive(true);
			token = nextToken(scn);	
		}
		// WITH CONTINUE 
		if (token.equals("WITH")) {
			token = nextToken(scn);										// WITH
			sqlFetch.setWithContinue(true);
			token = nextToken(scn);	
		}
		
		// BEFORE
		if (token.equals("BEFORE")) {
			sqlFetch.setBefore(true);
			token = nextToken(scn);	
		}
		
		// AFTER
		if (token.equals("AFTER")) {
			sqlFetch.setAfter(true);
			token = nextToken(scn);	
		}
		
		// NEXT
		if (token.equals("NEXT") && !this.strNoMemoryToParse.startsWith("ROWSET")) {
			sqlFetch.setAfterNext(true);
			token = nextToken(scn);	
			
		// PRIOR
		} else if (token.equals("PRIOR") && !this.strNoMemoryToParse.startsWith("ROWSET")) {
			sqlFetch.setAfterPrior(true);
			token = nextToken(scn);	
			
		// FIRST
		} else if (token.equals("FIRST") && !this.strNoMemoryToParse.startsWith("ROWSET")) {
			sqlFetch.setAfterFirst(true);
			token = nextToken(scn);	
			
		// LAST
		} else if (token.equals("LAST") && !this.strNoMemoryToParse.startsWith("ROWSET")) {
			sqlFetch.setAfterLast(true);
			token = nextToken(scn);	
			
		// CURRENT
     	} else if (token.equals("CURRENT") && !this.strNoMemoryToParse.startsWith("ROWSET")) {
			sqlFetch.setAfterCurrent(true);
			token = nextToken(scn);
			// CONTINUE
			if (token.equals("CONTINUE")) {
				sqlFetch.setAfterCurrentContinue(true);
				token = nextToken(scn);	
			}
			
		// ABSOLUTE 
		} else if (token.equals("ABSOLUTE")) {
			sqlFetch.setAfterAbsolute(true);
			token = nextToken(scn);	
			if (StringService._isNumericInt(token)) {
				sqlFetch.setAfterNumber(true);
				sqlFetch.setAfterAbsoluteNumber(StringService._getNumericInt(token));
			} else {
				sqlFetch.setAfterHostVar(true);
				sqlFetch.setAfterAbsoluteHostVar(token);
			}
			token = nextToken(scn);	
		
		// RELATIVE 
		} else if (token.equals("RELATIVE")) {
			sqlFetch.setAfterRelative(true);
			token = nextToken(scn);	
			if (StringService._isNumericInt(token)) {
				sqlFetch.setAfterNumber(true);
				sqlFetch.setAfterRelativeNumber(StringService._getNumericInt(token));
			} else {
				sqlFetch.setAfterHostVar(true);
				sqlFetch.setAfterRelativeHostVar(token);
			}
			token = nextToken(scn);	
			
		// NEXT ROWSET
		} else if (token.equals("NEXT") && this.strNoMemoryToParse.startsWith("ROWSET")) {
			sqlFetch.setAfterNextRowSet(true);
			token = nextToken(scn);						// ROWSET
			token = nextToken(scn);
			
		// PRIOR ROWSET
		} else if (token.equals("PRIOR") && this.strNoMemoryToParse.startsWith("ROWSET")) {
			sqlFetch.setAfterPriorRowSet(true);
			token = nextToken(scn);						// ROWSET
			token = nextToken(scn);	
			
		// FIRST ROWSET
		} else if (token.equals("FIRST") && this.strNoMemoryToParse.startsWith("ROWSET")) {
			sqlFetch.setAfterFirstRowSet(true);
			token = nextToken(scn);						// ROWSET
			token = nextToken(scn);	
			
		// LAST ROWSET
		} else if (token.equals("LAST") && this.strNoMemoryToParse.startsWith("ROWSET")) {
			sqlFetch.setAfterLastRowSet(true);
			token = nextToken(scn);						// ROWSET
			token = nextToken(scn);	
			
		// CURRENT ROWSET
		} else if (token.equals("CURRENT") && this.strNoMemoryToParse.startsWith("ROWSET")) {
			sqlFetch.setAfterCurrentRowSet(true);
			token = nextToken(scn);						// ROWSET
			token = nextToken(scn);	
		
		// ROWSET STARTING AT
		} else if (token.equals("ROWSET")) {
			token = nextToken(scn);						// STARTING
			token = nextToken(scn);						// AT
			token = nextToken(scn);						// ABSOLUTE|RELATIVE
			if (token.equals("ABSOLUTE")) {
				sqlFetch.setAfterRowSetAtAbsolute(true);
			} else {
				sqlFetch.setAfterRowSetAtRelative(true);
			};
			token = nextToken(scn);						// host-var|integer-constant
			if (StringService._isNumericInt(token)) {
				sqlFetch.setAfterRowSetAtNumber(true);
				sqlFetch.setAfterRowSetNumber(StringService._getNumericInt(token));
			} else {
				sqlFetch.setAfterRowSetAtHostVar(true);
				sqlFetch.setAfterRowSetHostVar(token);
			}
			token = nextToken(scn);  					// FROM ?
		}

		// FROM
		if (token.equals("FROM")) {
			token = nextToken(scn); 					// cursor-name
		};

		sqlFetch.setCursorName(token);
		token = nextToken(scn); 						// INTO|FOR
		
		// FOR ... ROWS
		if (token.equals("FOR")) {
			sqlFetch.setForRows(true);
			token = nextToken(scn); 					// host-var|integer-constant
			if (StringService._isNumericInt(token)) {
				sqlFetch.setForRowsNumber(true);
				sqlFetch.setForRowsNumber(StringService._getNumericInt(token));
			} else {
				sqlFetch.setForRowsHostVar(true);
				sqlFetch.setForRowsHostVar(token);
			}
			token = nextToken(scn); 					// ROWS
			token = nextToken(scn); 
		}
		
		
		// INTO
		if (token.equals("INTO")) {
			token = nextToken(scn); 					// DESCRIPTOR|host-var
			// DESCRIPTOR
			if (token.equals("DESCRIPTOR")) {
				token = nextToken(scn); 				// descriptor-name
				sqlFetch.setIntoDescriptor(true);
				sqlFetch.setIntoDescriptorName(token);
				token = nextToken(scn);
			// Host-vars
			} else {
				while (!token.equals("") ) {
					if (token.equals(",")) {
						token = nextToken(scn);
						continue;
					}
					sqlFetch.getIntoHostVars().add(token);
					token = nextToken(scn);
				}
			}
		}
		
 
		
		
  	   	// Descrittore istruzione DECLARE CURSOR
    	instruction.sqlFetchSetDescriptor(sqlFetch);

    	
		///////////////////////////////////////////////////////////////////////////
		// Caricamento simboli nell'istruzione e predisposizione per Updates finali
		////////////////////////////////////////////////////////////////////////////	
		
   		// Simboli host var in output per l'istruzione, si eliminano i : iniziali
   	   	for (String hostVar : sqlFetch.getIntoHostVars()) {
	   		instruction.addSymbolOutput(hostVar.substring(1), EnumSymbolType.SQL_SYMBOL_HOST_VAR);
 		}
 		
   	   	// N.B. La fetch fa riferimento a un cursore dichiarato con declare cursor
   	   	// al quale corrispondono una o più tabelle o view.
   	   	// Le relazioni vengono inserite a fine elaborazione in quanto la declare cursor
   	   	// potrebbe non essere ancora stata analizzata
   	   	
	}
	
	
	/* ----------------------------
	 * Analisi statement LockTable
	 * ----------------------------
	 * 
	 * 
	 */
	private void analyzeSqLockTable(InnerContextAnalysis ictx, InstructionSql instruction){
		
		Scanner scn = null;
		String str = "";
		String tableName = "";
		String entityName = "";
	    String entityOwner = "";
		String token = "";
		boolean isShareMode = false;
		boolean isExclusiveMode = false;
 		int partitionNumber = -1;
		
		// Origine analisi per messaggi di errore
		adjustMsgForScriptOrPgmAnalysys();
		
		
		str =instruction.getSourceInstr(); 
		this.strNoMemoryToParse = str;									// Initial 
		scn = new Scanner(str);									        //  
		
		token = nextToken(scn);											// LOCK
		token = nextToken(scn);											// TABLE
		token = nextToken(scn);											// table-name
		tableName = token;
		token = nextToken(scn);											// PARTITION|IN ?
		if (token.equals("PARTITION")) {
			token = nextToken(scn);										// integer
			if (StringService._isNumericInt(token)) {
				partitionNumber = StringService._getNumericInt(token);
			}
			token = nextToken(scn);										//  								
		}
		token = nextToken(scn);											// SHARE|EXCLUSIVE
		if (token.equals("EXCLUSIVE")) {
			isExclusiveMode = true;
		} else {
			isShareMode = true;
		}

		
		////////////////////////////////////////////////////////////////////////////////////
		// Impostazione sotto strutture analizzate e caricamento descriptor in istruzione
		////////////////////////////////////////////////////////////////////////////////////
		
	   	instruction.sqlLockTableSetTableNameQualified(tableName);
	   	if (partitionNumber >= 0) {
		   	instruction.sqlLockTableSetPartitionNumber(partitionNumber);	
		}
	   	if (isExclusiveMode) {
		   	instruction.sqlLockTableSetExclusiveMode(true);	
		}
	   	if (isShareMode) {
		   	instruction.sqlLockTableSetShareMode(true);	
		}

    	
		///////////////////////////////////////////////////////////////////////////
		// Caricamento simboli nell'istruzione e predisposizione per Updates finali
		////////////////////////////////////////////////////////////////////////////	
		
	   	if (partitionNumber >= 0) {
	  		instruction.addSymbolOutput(partitionNumber+"", EnumSymbolType.SQL_SYMBOL_LITERAL_NUM);
		}
 

   		entityName  = instruction.sqlLockTableGetTableName();
	   	entityOwner  = instruction.sqlLockTableGetTableOwner();
	   		
   		this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
   		if (!entityOwner.equals("")) {
   		   this.analyzerDbInfo.prepareForDbObject(EnumObject.OBJECT_SQL_OWNER, entityOwner, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
	    }
	   		
   	    // PGM_SQL_OWNER
   		if (!entityOwner.equals("")) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_SQL_OWNER, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION, this.userExitInfoPgm, instruction);
  		}
		// PGM_ENTITY 
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_PGM_COBOL) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_ENTITY, EnumObject.OBJECT_PGM_COBOL, ictx.idObjectOrigin, EnumObject.OBJECT_ENTITY_SQL, entityName, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION,this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SQL);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_UPDATE);
   	    }
        // ENTITY_SQL_OWNER
   	    if (!entityOwner.equals("")) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_OWNER, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_SQL_OWNER, entityOwner, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.PROC_DIVISION,  this.userExitInfoPgm, instruction);
   	    }
   	    // ENTITY_SQL_SCRIPT
   	    if (ictx.typeObjectOrigin == EnumObject.OBJECT_SQL_SCRIPT) {
   	    	this.analyzerDbInfo.prepareForDbObjectRelation      (EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, this.userExitInfoPgm);
			this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.ENTITY_SQL_SCRIPT, EnumObject.OBJECT_ENTITY_SQL, entityName, EnumObject.OBJECT_ENTITY_SQL, ictx.idObjectOrigin, instruction, this.ictx.typeObjectOrigin, this.ictx.idObjectOrigin, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, instruction);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_WITH_I_O_SQL);
			this.analyzerDbInfo.prepareForDbObjectOption(ictx.idObjectOrigin, EnumObject.OBJECT_SQL_SCRIPT, EnumObjectOption.SQL_SCRIPT_UPDATE);
   	    }

	}
	/* -------------------------
	 * Analisi statement Prepare
	 * -------------------------
	 * 
	 * 
	 */
	private void analyzeSqPrepare(InnerContextAnalysis ictx, InstructionSql instruction){
		// TODO
	}
	/* ---------------------------------
	 * Analisi statement DeclareSection
	 * ---------------------------------
	 * 
	 * 
	 */
	private void analyzeSqDeclareSection(InnerContextAnalysis ictx, InstructionSql instruction){
		// Nessuna elaborazione
	}
	
	/* ------------------------------------
	 * Analisi statement EndDeclareSection
	 * ------------------------------------
	 * 
	 * 
	 */
	private void analyzeSqEndDeclareSection(InnerContextAnalysis ictx, InstructionSql instruction){
		// Nessuna elaborazione
	}

	
	
	

	/*
     * ----------------------------------
     * Analisi fullselect.
     * ----------------------------------
     * 
     * L'analisi è di tipo ricorsivo.
     * 
     * 1) subselect1 | (fullselect1)
     * 2) | UNION|EXCEPT|INTERSECT linked to subselect2 | (fullselect2)
     * 3) | ORDER BY option
     * 4) | FETCH FIRST n ROWS option
     * 
     * subselect1 e (fullselect1) potrebbero avere al loro interno 
     * clausole ORDER BY e FETCH FIRST pertanto vengono estratte le
     * sezioni del costrutto full-select analizzando token per token
     * e bypasssando i contenuti fra parentesi
     * 
     * 
     */
	private SqlFullSelect analyzeSqlFullselect(InnerContextAnalysis ictx, InstructionSql instruction, String strToParse) throws SQLException, ExceptionAmrita  {
       
 		Scanner scn = null;
		SqlFullSelect fullselect = null;
		SqlFullSelect fullselectRecursive = null;
		SqlSubselectSelectInto subselect = null;
		StringBuffer sbFullSubSelect = null;
		StringBuffer sbFullSelectLinkedTo = null;
		String strFullSubSelect = "";
		String strFullSelectLinkedTo = "";
		String strFetchOrOrderBy = "";
		String strBetweenPar = "";
		String token = "";
		
 		if (instruction.isParsingError()) {
		  return null;
	    }
       
		// Allocazione strutture di lavoro
 		subselect = new SqlSubselectSelectInto();
        fullselect = new SqlFullSelect();
		sbFullSubSelect = new StringBuffer();
		sbFullSelectLinkedTo = new StringBuffer();
     
        
		////////////////////////////////////////////////////////////////////
		// Estrazione sezioni fullselect per parser
		//  1) subselect o (fullselect)
		//  2) Insiemi di UNION/EXCEPT/INTERSECT a subselect o (fullselect)
		//  3) Opzioni  order by e fetch
		///////////////////////////////////////////////////////////////////
		
		scn = new Scanner(strToParse);									//  
        this.strNoMemoryToParse = strToParse;							// Initial 

        token = nextToken(scn);											// ( | SELECT 
        
        // full-select inizia con una subselect
        if (token.equals("SELECT")) {
        	sbFullSubSelect.append("SELECT");
        	token = nextToken(scn);
         	// Scan token subselect fino a gruppi linkedTo UNION|EXCEPT|INTERSECT o fine stringa
 			while (!token.equals("") && !token.equals("UNION") && !token.equals("EXCEPT") && !token.equals("INTERSECT")) {

		       	// I gruppi UNION|EXCEPT|INTERSECT possono essere dentro clausole FROM
	        	// Se sono all'interno di parentesi NON devo considerarli, verranno trattati ricorsivamente
	        	// come nested-table expression della FROM
 				if (token.equals("(")) {
					strBetweenPar = extractStringBetweenPar(scn);
					sbFullSubSelect.append(" ( " + strBetweenPar + " ) ");
					token = nextToken(scn);
					continue;
				}
				
				sbFullSubSelect.append(" ");
				sbFullSubSelect.append(token);
				token = nextToken(scn);
 			}
			
	    // full-select inizia con una attivazione ricorsiva fra parentesi (full-select)
        } else if (token.equals("(")) {
        	strBetweenPar = extractStringBetweenPar(scn);
        	sbFullSubSelect.append(" ( ");
         	sbFullSubSelect.append(strBetweenPar);
           	sbFullSubSelect.append(" )");
         	token = nextToken(scn);
        	
        // Istruzione malformata: non inizia con select o (fullselect)
		} else {
			instruction.setParsingError(true);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
			return fullselect;
		} // End-while 			
			
		// Gruppo linkedTo UNION
		if (token.equals("UNION") || token.equals("EXCEPT") || token.equals("INTERSECT") ) {
			sbFullSelectLinkedTo.append(token);
			token = nextToken(scn);
        	// Scan Gruppo linkedTo UNION fino a opzioni full-select o fine stringa
			while (!token.equals("") &&  !token.equals("ORDER") && !token.equals("FETCH")) {
				sbFullSelectLinkedTo.append(" " + token);
				// Estrazione contenuto fra parentesi 
				if (token.equals("(")) {
					strBetweenPar = extractStringBetweenPar(scn);
					sbFullSelectLinkedTo.append(" " + strBetweenPar);
					sbFullSelectLinkedTo.append(" )");
				}
				token = nextToken(scn);
			}
		 }
		 
	    // ORDER BY e/o FETCH FIRST
	    if (token.equals("ORDER") || token.equals("FETCH")) {
		    strFetchOrOrderBy = token + " " + this.strNoMemoryToParse.trim();
	    }

        
         
 		////////////////////////////////////////////////////////////////////
		// Parser
		///////////////////////////////////////////////////////////////////
	     
	    
		// Full select: attivazione analisi ricorsiva contenuto all'interno delle parentesi
		if (sbFullSubSelect.toString().trim().startsWith("(")) {
			strFullSubSelect = sbFullSubSelect.toString().trim();
			strFullSubSelect = strFullSubSelect.substring(1).trim();
			strFullSubSelect = strFullSubSelect.substring(0, strFullSubSelect.length() - 1);
			fullselectRecursive = analyzeSqlFullselect(ictx, instruction, strFullSubSelect);
		    fullselect.setFullSelect(fullselectRecursive);
		    fullselect.setFullSelect(true);
			if (instruction.isParsingError()) {
				return fullselect;
			}
		}
		   
		// Subselect o select into: attivazione analisi standard 
		if (sbFullSubSelect.toString().startsWith("SELECT")) {
			subselect = analyzeSqlSubselectSelectInto(instruction, sbFullSubSelect.toString());
			fullselect.setSubselectInto(subselect);
			fullselect.setSubSelect(true);
			if (instruction.isParsingError()) {
				return fullselect;
			}
		}
		
		// Gruppi di union/except/intersect
		strFullSelectLinkedTo = sbFullSelectLinkedTo.toString();
		if (!strFullSelectLinkedTo.equals("")) {
			analyzeSqlFullselectLinkedTo(ictx, instruction, fullselect, strFullSelectLinkedTo);
			if (instruction.isParsingError()) {
				return fullselect;
			}
		}
		
		// Opzioni order by e fetch
		if (!strFetchOrOrderBy.equals("")) {
			analyzeSqlFullselectOptions(ictx, instruction, fullselect, strFetchOrOrderBy);
			if (instruction.isParsingError()) {
				return fullselect;
			}
		}
       
		return fullselect;
	}

	/*
     * ----------------------------------
     * Analisi Subselect e Select Into.
     * ----------------------------------
     * 
     * Si tratta di costrutti di tipo SELECT e SELECT INTO
     * 
     * Vengono gestitite, un sequenza, tutte le clausole:
     * 
     * SELECT		(obbligatoria)
     * INTO 
     * FROM			(obbligatoria)
     * WHERE
     * GROUP BY
     * HAVING
     * ORDER BY
     * QUERYNO
     * SKIP|ISOLATION
     * FETCH FIRST
    * 
     * 
     */
	private SqlSubselectSelectInto analyzeSqlSubselectSelectInto(InstructionSql instruction, String strToParse) throws SQLException, ExceptionAmrita {
 
		Scanner scn = null;
  		SqlSubselectSelectInto subselectSelectInto = null;
 		StringBuffer sbSelect = null;
		StringBuffer sbInto = null;
		StringBuffer sbFrom = null;
		StringBuffer sbWhere = null;
		StringBuffer sbGroupBy = null;
		StringBuffer sbHaving = null;
		StringBuffer sbOrderBy = null;
		StringBuffer sbQueryno = null;
		StringBuffer sbSkipIsolation = null;
		StringBuffer sbFetch = null;
		String token = "";
		String strBetweenPar = "";
		
 		if (instruction.isParsingError()) {
		  return null;
	    }
       
		// Allocazione strutture di lavoro
		sbSelect = new StringBuffer ();
		sbInto = new StringBuffer ();
		sbFrom = new StringBuffer ();
		sbWhere = new StringBuffer ();
		sbGroupBy= new StringBuffer ();
		sbHaving = new StringBuffer ();
		sbOrderBy = new StringBuffer ();
		sbQueryno = new StringBuffer ();
		sbSkipIsolation = new StringBuffer ();
		sbFetch = new StringBuffer ();

 		
		////////////////////////////////////////////////////////////////////
		// Estrazione in sequenza sezioni istruzione presenti per parser
 		// 00) Select 
 		// 02) Into 
 		// 03) From 
 		// 04) Where 
 		// 05) GroupBy 
 		// 06) Having  
 		// 07) OrderBy  
 		// 08) Queryno  
 		// 09) SkipIsolation 
 		// 10) Fetch  
		///////////////////////////////////////////////////////////////////
				
 		subselectSelectInto = new SqlSubselectSelectInto();    
  		scn = new Scanner(strToParse);
  		this.strNoMemoryToParse = strToParse;
  		
  		// Estrazione Select col1, col2,
		token = nextToken(scn);
		while (!token.equals("") && !token.equals("FROM") && !token.equals("INTO")) {
 			sbSelect.append(" " + token);
 			if (token.equals("(")) {
 				strBetweenPar = extractStringBetweenPar(scn);
 				sbSelect.append(" " + strBetweenPar + " )");
			}
 			token = nextToken(scn);
		}
        
 		// Estrazione into hostvar1, hostvarn se presente
		if (token.equals("INTO")) {
			sbInto.append(" " + token);
			token = nextToken(scn);
			while (!token.equals("") && !token.equals("FROM")) {
	 			sbInto.append(" " + token);
	 			if (token.equals("(")) {
	 				strBetweenPar = extractStringBetweenPar(scn);
	 				sbInto.append(" " + strBetweenPar + " )");
				}
	 			token = nextToken(scn);
			}
		}
		
		// FROM non 
		
  		// Estrazione From tb1, tbn
		if (token.equals("FROM")) {
			sbFrom.append(" " + token);
			token = nextToken(scn);
			while (!token.equals("") 
					&& !token.equals("WHERE") 
					&& !token.equals("GROUP") 
					&& !token.equals("HAVING") 
					&& !token.equals("ORDER")
					&& !token.equals("QUERYNO")
					&& !token.equals("FETCH")
					&& !token.equals("WITH")
					&& !token.equals("SKIP")
					) {
	 			sbFrom.append(" " + token);
	 			if (token.equals("(")) {
	 				strBetweenPar = extractStringBetweenPar(scn);
	 				sbFrom.append(" " + strBetweenPar + " )");
				}
	 			token = nextToken(scn);
			}
		}
        
 		// Estrazione where search-conditions
		if (token.equals("WHERE")) {
			sbWhere.append(" " + token);
			token = nextToken(scn);
			while (!token.equals("") 
					&& !token.equals("GROUP") 
					&& !token.equals("HAVING") 
					&& !token.equals("ORDER")
					&& !token.equals("QUERYNO")
					&& !token.equals("FETCH")
					&& !token.equals("WITH")
					&& !token.equals("SKIP")
					) {
				sbWhere.append(" " + token);
	 			if (token.equals("(")) {
	 				strBetweenPar = extractStringBetweenPar(scn);
	 				sbWhere.append(" " + strBetweenPar + " )");
				}
	 			token = nextToken(scn);
			}
		}
		
		// Estrazione group by
		if (token.equals("GROUP")) {
			sbGroupBy.append(" " + token);
			token = nextToken(scn);
			while (!token.equals("") 
					&& !token.equals("HAVING") 
					&& !token.equals("ORDER")
					&& !token.equals("QUERYNO")
					&& !token.equals("FETCH")
					&& !token.equals("WITH")
					&& !token.equals("SKIP")
					) {
				sbGroupBy.append(" " + token);
	 			if (token.equals("(")) {
	 				strBetweenPar = extractStringBetweenPar(scn);
	 				sbGroupBy.append(" " + strBetweenPar + " )");
				}
	 			token = nextToken(scn);
			}
		}
		
		// Estrazione having
		if (token.equals("HAVING")) {
			sbHaving.append(" " + token);
			token = nextToken(scn);
			while (!token.equals("") 
					&& !token.equals("ORDER")
					&& !token.equals("QUERYNO")
					&& !token.equals("FETCH")
					&& !token.equals("WITH")
					&& !token.equals("SKIP")
					) {
				sbHaving.append(" " + token);
	 			if (token.equals("(")) {
	 				strBetweenPar = extractStringBetweenPar(scn);
	 				sbHaving.append(" " + strBetweenPar + " )");
				}
	 			token = nextToken(scn);
			}
		}
		
		// Estrazione order by
		if (token.equals("ORDER")) {
			sbOrderBy.append(" " + token);
			token = nextToken(scn);
			while (!token.equals("") 
					&& !token.equals("QUERYNO")
					&& !token.equals("FETCH")
					&& !token.equals("WITH")
					&& !token.equals("SKIP")
					) {
				sbOrderBy.append(" " + token);
	 			if (token.equals("(")) {
	 				strBetweenPar = extractStringBetweenPar(scn);
	 				sbOrderBy.append(" " + strBetweenPar + " )");
				}
	 			token = nextToken(scn);
			}
		}
		
		// Estrazione isolation 
		if (token.equals("WITH") || token.equals("SKIP")) {
			sbSkipIsolation.append(" " + token);
			token = nextToken(scn);
			while (!token.equals("") 
					&& !token.equals("QUERYNO")
					&& !token.equals("FETCH")
					) {
				sbSkipIsolation.append(" " + token);
	 			if (token.equals("(")) {
	 				strBetweenPar = extractStringBetweenPar(scn);
	 				sbSkipIsolation.append(" " + strBetweenPar + " )");
				}
	 			token = nextToken(scn);
			}
		}
		
		// Estrazione queryno 
		if (token.equals("QUERYNO")) {
			sbQueryno.append(" " + token);
			token = nextToken(scn);
			while (!token.equals("") 
					&& !token.equals("FETCH")
					) {
				sbQueryno.append(" " + token);
	 			token = nextToken(scn);
			}
		}
		
		// Estrazione fetch
		if (token.equals("FETCH")) {
			sbFetch.append(" " + token);
			token = nextToken(scn);
			while (!token.equals("")) {
				sbFetch.append(" " + token);
	 			token = nextToken(scn);
			}
		}
		
		// Clausola FROM non codificata, anche a causa di parentesi chiuse non bilanciate
		if (sbFrom.toString().trim().equals("")) {
			instruction.setParsingError(true);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0045", "", null, this.strProgramScript, this.strProgramScriptObj);
			return subselectSelectInto;
		}
		
		
        //////////////////////////////////////////////////////////////////////////
        // Parser singole clausole, se codificate
        /////////////////////////////////////////////////////////////////////////
        
        analyzeSqlClauseFrom     (instruction, subselectSelectInto, sbFrom.toString().trim());
        analyzeSqlClauseSelect   (instruction, subselectSelectInto, sbSelect.toString().trim());
        analyzeSqlClauseInto     (instruction, subselectSelectInto, sbInto.toString().trim());
        analyzeSqlClauseWhere    (instruction, subselectSelectInto, sbWhere.toString().trim());
        analyzeSqlClauseGroupBy  (instruction, subselectSelectInto, sbGroupBy.toString().trim());
        analyzeSqlClauseHaving   (instruction, subselectSelectInto, sbHaving.toString().trim());
        analyzeSqlClauseOrderBy  (instruction, subselectSelectInto, sbOrderBy.toString().trim());
       	analyzeSqlClauseIsolation(instruction, subselectSelectInto, sbSkipIsolation.toString().trim());
       	analyzeSqlClauseQueryno  (instruction, subselectSelectInto, sbQueryno.toString().trim());
        analyzeSqlClauseFetch    (instruction, subselectSelectInto, sbFetch.toString().trim());
        
		// Select o subselect codificata dalle analisi precedenti
        // Errori eventuali di parsing sono indicati nell'oggetto instruction, in input al metodo
        
		return subselectSelectInto;
	}

    
	/* ------------------------
	 * Analisi clausola SELECT
	 * ------------------------
	 * 
	 * Vengono estratte e analizzate le singole colonne, comunque codificate.
	 * Update oggetto descrittore SqlSubselectSelectInto
	 * 
	 */
	private void analyzeSqlClauseSelect(InstructionSql instruction, SqlSubselectSelectInto subselectSelectInto, String strSelect) throws SQLException, ExceptionAmrita {
		
		Scanner scn = null;
		SqlColumnInSelect sqlColumn = null;
		SqlExpression expression = null;
		ArrayList<ArrayList<String>> al_al_columnImplicit = null;
		String ar_strColumn[] = null;
		StringBuffer sbExpression = null;
		String token = "";
		String strBeforePoint = "";
		String tableViewName = "";
		String correlationName = "";
		int iPoint = 0;
		int cntPar = 0;
		boolean isFirstColumn = true;
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		sbExpression = new StringBuffer();
		scn = new Scanner(strSelect);
		this.strNoMemoryToParse = strSelect;
		token = nextToken(scn);										// SELECT
		
		// ALL
		if (nextTokenWithoutScannerMod(scn).equals("ALL")) {
			subselectSelectInto.setSelectAll(true);
			token = nextToken(scn);
		} else if (nextTokenWithoutScannerMod(scn).equals("DISTINCT")) {
			subselectSelectInto.setSelectDistinct(true);
			token = nextToken(scn);
		} 
		
		ar_strColumn = splitParms(this.strNoMemoryToParse);			// Da prima colonna utile dopo SELECT|ALL|DISTINCT
		
		
		// Scan singole colonne estratte che erano separate da ,
		for (String strColumn : ar_strColumn) {
			
			scn = new Scanner(strColumn);
			this.strNoMemoryToParse = strColumn;
			token = nextToken(scn);							 

			sqlColumn = new SqlColumnInSelect();
			
			// SELECT *
			if (token.equals("*") && isFirstColumn) {
				subselectSelectInto.setSelectColsAsStar(true);
				sqlColumn = new SqlColumnInSelect();
				sqlColumn.setByStar(true);
				al_al_columnImplicit = getColumnsImplicit(instruction, sqlColumn, subselectSelectInto, token);
				sqlColumn.setColumnsImplicit(al_al_columnImplicit);
				subselectSelectInto.getColumns().add(sqlColumn);
				return;
			}

			// x.*
			iPoint = token.indexOf(".");
			if (isFirstColumn && iPoint > 0 && token.charAt(iPoint + 1) == '*') {
				strBeforePoint = token.substring(0, iPoint);
				// Verifica se table
				if (isSqlEntity(subselectSelectInto, strBeforePoint, false)) {
					tableViewName = strBeforePoint;
					sqlColumn = new SqlColumnInSelect();
					sqlColumn.setTableViewName(tableViewName);
					sqlColumn.setByTableView(true);
					subselectSelectInto.getColumns().add(sqlColumn);
					isFirstColumn = false;
					continue;
				}
				// Verifica se View
				if (isSqlEntity(subselectSelectInto, strBeforePoint, false)) {
					tableViewName = strBeforePoint;
					sqlColumn = new SqlColumnInSelect();
					sqlColumn.setTableViewName(tableViewName);
					sqlColumn.setByTableView(true);
					subselectSelectInto.getColumns().add(sqlColumn);
					subselectSelectInto.getColumns().add(sqlColumn);
					isFirstColumn = false;
					continue;
				}
				if (isSqlCorrelationName(subselectSelectInto, strBeforePoint)) {
					correlationName = strBeforePoint;
					sqlColumn = new SqlColumnInSelect();
					sqlColumn.setColumnQualifier(correlationName);
					sqlColumn.setByCorrelationName(true);
					subselectSelectInto.getColumns().add(sqlColumn);
					token = nextToken(scn);	
					isFirstColumn = false;
					continue;
				}
				instruction.setParsingError(true);
				instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
			    return;
			}

			// Colonna codificate come nomi colonne, espressioni, etc., tecnicamente è sempre una espressione

			sbExpression = new StringBuffer();
			
			// Scan fino a fine token o AS (che non deve essere dentro parentesi)
			while (!token.equals("")) {
				if (token.equals("(")) {cntPar++;}
				if (token.equals(")")) {cntPar--;}

				sbExpression.append(" " + token);
				token = nextToken(scn);
				
				// Correlation-name fuori da eventiaìuali parentesi precedenti
				if (token.equals("AS") && cntPar == 0) {
					break;
				}
			}
			
			// Analisi espressione, comunque complessa o al minimo una colonna
			expression = analyzeSqlExpression(ictx, instruction, sbExpression.toString().trim());
			if (instruction.isParsingError()) {
				return;}

			// AS corr-name
			if (token.equals("AS")) {
				token = nextToken(scn);
				sqlColumn.setAsNewColumnName(token);
			}
		
			sqlColumn.setExpression(expression);
			sqlColumn.setByExpression(true);
			subselectSelectInto.getColumns().add(sqlColumn);
		}
	}

	/*
	 * Restituisce le colonne della/e tabelle implicitamente richieste a fronte di notazioni di colonna quali:
	 * 
	 * 1) *
	 * 2) table.*
	 * 3) view.*
	 * 4) corrName.*
	 * 
	 * E' disponibile l'oggetto SqlSubselectSelectInto, dove sono già presenti tutte le
	 * informazioni della clausola FROM, già analizzata.
	 * 
	 * Vengono individuati nella clausola FROM i nomi delle entities interessate (table o view)
	 * e recuperate le colonne dal database.
	 * 
	 * Si suppone che le tabelle siano disponibili e già trattate analizzando qualche script.
	 * Se così non fosse, viene segnalato un warning sull'istruzione.
	 * 
	 * Viene restituito un ArrayList di colonne.<br>
	 * Ogni elemento, indipendentemente dalla notazione implicita adottata,<br>
	 * è a sua volta un ArrayList di stringhe di 4 elementi che sono, nell'ordine:
	 * <ul>
	 *  <li> <tt>column-name</tt></li>
	 *  <li> <tt>table-name</tt></li>
	 *  <li> <tt>owner</tt></li>
	 *  <li> <tt>correlation-name</tt></li>
	 * </ul>
	 * <p>
	 * L'owner e il correlation name possono essere valorizzati come stringhe vuote,<br>
	 * se non sono codificati nell'istruzione.
	 * <p>
	 */
	private ArrayList<ArrayList<String>> getColumnsImplicit(InstructionSql instruction
														  , SqlColumnInSelect sqlColumn
														  , SqlSubselectSelectInto subselectSelectInto
														  , String implicitNotation
														   ) throws SQLException, ExceptionAmrita {
		
		
		ArrayList<ArrayList<String>> al_al_columnImplicit = null;		// Un elemento per ogni table-reference in FROM
		ArrayList<String> al_columnImplicit = null;		    			// Per ogni elemento un elemento con  column, table, owner, corr-name
		ArrayList<ArrayList<String>> al_al_entityInfo = null;			// Per ogni elemento un elemento con  table, owner, corr-name
		EntityCopyEntityDefinition[] ar_entityColumn = null;			// Colonne da database
		String entityName = "";
	    String owner = "";
	    String corrName = "";
	    String qualifier = "";
	    int i = 0;
	    
	    i = implicitNotation.indexOf(".");
	    if (i > 0) {
	    	qualifier = implicitNotation.substring(0, i);
		}
	    
		al_al_columnImplicit = new ArrayList<ArrayList<String>> ();
		al_al_entityInfo = new ArrayList<ArrayList<String>> ();
		
		// Scan reference-table di FROM e append informazioni
		for (SqlFromTableReference sqlFrom : subselectSelectInto.getFromTableReferences()) {
			
			// Select * indica tutte le colonne di tutte le tabella
			if (implicitNotation.equals("*")) {
				al_al_entityInfo.addAll(sqlFrom.getEntities());
				continue;
			}
			
			// Si tratta di qualificazione come table.*, view.*, alias.* e corrName.*
			
			// Il qualifier è il correlation name
			if (qualifier.equals(sqlFrom.getCorrelationClause().getCorrName())) {
				al_al_entityInfo.addAll(sqlFrom.getEntities());
				continue;
			}
			
			// Il tipo di reference-table è single-table e Il qualifier è l'owner
			if (sqlFrom.getTypeTableReference() == EnumSqlTableReferenceType.SINGLE_TABLE
			&&  qualifier.equals(sqlFrom.getSingleTableEntityOwner())) {
				al_al_entityInfo.addAll(sqlFrom.getEntities());
				continue;
			}
			
			// Il tipo di reference-table è single-table e Il qualifier è il nome della tabella/view/
			if (sqlFrom.getTypeTableReference() == EnumSqlTableReferenceType.SINGLE_TABLE
			&&  qualifier.equals(sqlFrom.getSingleTableEntityName())) {
				al_al_entityInfo.addAll(sqlFrom.getEntities());
				continue;
			}
			
		} // end-for reference-tables
		
		
		// Scan entities estratti
		for (ArrayList<String> al_entityInfo : al_al_entityInfo) {
			
			// Info generali entity  
			entityName = al_entityInfo.get(0);
			owner = al_entityInfo.get(1);
			corrName = al_entityInfo.get(2);
			
			// Colonne entity
			ar_entityColumn = getEntityColumnsDefined(instruction, entityName);
			
			// Colonne non disponibili
			if (ar_entityColumn.length == 0) {
				continue;
			}
			
			// Scan colonne definite
			for (EntityCopyEntityDefinition entityColumn : ar_entityColumn) {
				
				// Info complessive colonna
				al_columnImplicit = new ArrayList<String> ();
				al_columnImplicit.add(entityColumn.getIdField());
				al_columnImplicit.add(entityName);
				al_columnImplicit.add(owner);
				al_columnImplicit.add(corrName);
				
				// Porto in output
				al_al_columnImplicit.add(al_columnImplicit);
				
			} //end-for colonne
			
		} // end-for entities
		
		return al_al_columnImplicit;
	}


	/* ---------------------------------------------------------------
     * Restituisce true se il nome fornito è quello di una entity sql
     * ---------------------------------------------------------------
     * 
     * Il metodo viene richiamato a fronte dell'analisi delle colonne della clausola SELECT.
     * Serve a determinare se il nome fornito identifica una TABLE sql o una VIEW.
     * Se il parametro searchAsView è true si verifica se il nome è una VIEW, altrimenti si
     * verifica se è una table.
     * 
     * Il nome fornito in input può identificare il tableName o il corr-name e può essere relativo a:
     * 
     * 1) Un correlation name                 SELECT corr-name.col ... FROM tableName AS corr-name
     *                                        SELECT corr-name.*   ... FROM tableName AS corr-name
     * 2) Il nome qualificato di una tabella  SELECT tableName.col ... FROM owner.tableName 
     *                                        SELECT tableName.*   ... FROM owner.tableName 
     * 3) Il nome semplice di una tabella     SELECT tableName.col ... FROM tableName
     *                                        SELECT tableName.*   ... FROM tableName
     * 
      * Si verifica se il nome è presente nel database come entity e non esiste l'opzione ENTITY_SQL_AS_VIEW
     * In questo caso si restituisce subuto true.
     * 
     * Se non presente si verifica se il nome è un correlation-name per qualche tabella 
     * nella FROM clause della subselect fornita in input o in qualche subselect nested.
     * 
     */
	private boolean isSqlEntity(SqlSubselectSelectInto subselectSelectInto
							  , String name
							  , boolean isToSearchAsView
							   ) throws SQLException, ExceptionAmrita {
		
		EntityObject entityObject = null;
		EntityObjectOption entityObjectOption = null;

		List<EntityObjectOption> ar_objOption = null;
		String whereCondition = "";
		
		
		///////////////////////////////////////////////////////////////////
		// Verifica se tabella già definita nel database da Create Table
		///////////////////////////////////////////////////////////////////
		
		// Operazioni per accesso al database
	    Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false,false, ucfg);
		IDAOObjectOption eoDAOO = (DAOImplObjectOption) AmritaStartup.sqlFactory.getDAOObjectOption(conn, false,false, ucfg);
		 
		// Lettura oggetto su OBJT per recupero path e nome sorgente
    	entityObject = new EntityObject();	
    	entityObjectOption = new EntityObjectOption();	
    	
		// Primary key
		entityObject.setSystem(this.di.systemInput);   						 
		entityObject.setSubSystem(this.di.subSystemInput);    						 
		entityObject.setIdObject(name);    						 
		entityObject.setTypeObject(EnumObject.OBJECT_ENTITY_SQL);				 
		
		// Tabella definita come Entity: verifica che NON sia una view
        if (eoDAO.read(entityObject)) {
      		whereCondition =                      "      sys  =  '"  + di.systemInput 		+ "'";
    		whereCondition = whereCondition +	  " AND  subSys  =  '"  + di.systemInput 	+ "'";
     		whereCondition = whereCondition +     " AND  idObject  =  '"  + name 				    + "'";
     		whereCondition = whereCondition +     " AND  typeObject  =   "  + EnumObject.OBJECT_ENTITY_SQL.ordinal();
     		whereCondition = whereCondition +     " AND  typeSource  =   "  + entityObject.getTypeSource().ordinal();
    		whereCondition = whereCondition +     " AND  option  =   "  + EnumObjectOption.ENTITY_SQL_AS_VIEW.ordinal();
    		ar_objOption = eoDAOO.readSetEntityWhere(whereCondition, "");
    		DataBaseConnections.releaseConnection(conn);
    		eoDAOO.setConn(null);
    		// Richiesta verifica esistenza come view
    		if (isToSearchAsView) {
    	        // E' una table
             	if (ar_objOption.size() == 0) {
    				return false;
    			}
             	// E' una view
             	return true;
			}
    		
    		// Richiesta verifica esistenza come table
    		
	        // E' una table
         	if (ar_objOption.size() == 0) {
				return true;
			}
         	// E' una view
         	return false;
   		
 		}

		/////////////////////////////////////////////////////////////////////////////////
        // Verifica se nome presente come tabella presente in clausola FROM di subselect
		/////////////////////////////////////////////////////////////////////////////////
        
        // TODO

		return false;
	}
	
	/* ------------------------------------------------------------------
     * Restituisce le colonne definite sul database per l'entity fornita
     * ------------------------------------------------------------------
     * 
     * Si può trattare di una tabella, view, alias, o sinonimo
     * 
     * Viene restituito una array di descrittoti di colonna EntityCopyEntityDefinition,
     * con tutte le informazioni a livello di colonna
     * 
     * 
     */
	private EntityCopyEntityDefinition[] getEntityColumnsDefined(InstructionSql instruction, String entityName) throws SQLException, ExceptionAmrita {
		
		EntityObject entityObject = null;
		EntityCopyEntityDefinition ar_entityCopyEntityDefinition[] = null;
		List<EntityCopyEntityDefinition> ar_obj = null;										// Output generico da accesso ai dati
		String whereCondition = "";
		
		
		///////////////////////////////////////////////////////////////////
		// Verifica se tabella già definita nel database da Create Table
		///////////////////////////////////////////////////////////////////
		
		// Operazioni per accesso al database
	    Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false,false, ucfg);
		IDAOCopyEntityDefinition eoDAOO = (DAOImplCopyEntityDefinition) AmritaStartup.sqlFactory.getDAOCopyEntityDefinition(conn, false,false, ucfg);
		 
		// Lettura oggetto su OBJT per recupero path e nome sorgente
    	entityObject = new EntityObject();	
    	
		// Primary key
		entityObject.setSystem(this.di.systemInput);   						 
		entityObject.setSubSystem(this.di.systemInput);    						 
		entityObject.setIdObject(entityName);    						 
		entityObject.setTypeObject(EnumObject.OBJECT_ENTITY_SQL);				 
		
		// Entity non ancora analizzata: colonne non disponibili
		if (!eoDAO.read(entityObject)) {
			DataBaseConnections.releaseConnection(conn);
			eoDAO.setConn(null);
			instruction.setWarning(true);
			instruction.setInfoError(EnumMessageType.WARNING, "MW0015", entityName, null, this.strProgramScript, this.strProgramScriptObj, entityName);
			return new EntityCopyEntityDefinition[0];
		}
		 
		// Entity definita come Entity: estrazione colonne
  		whereCondition =                      "      sys  =  '"  + di.systemInput 		+ "'";
		whereCondition = whereCondition +	  " AND  subSys  =  '"  + di.subSystemInput 	+ "'";
 		whereCondition = whereCondition +     " AND  idObject  =  '"  + entityName 				    + "'";
 		whereCondition = whereCondition +     " AND  typeObject  =   "  + EnumObject.OBJECT_ENTITY_SQL.ordinal();
 		ar_obj = eoDAOO.readSetEntityWhere(whereCondition, "");
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		// Entity non ancora analizzata: colonne non disponibili
        if (ar_obj.size() == 0) { 
			instruction.setWarning(true);
			instruction.setInfoError(EnumMessageType.WARNING, "MW0015", entityName, null, this.strProgramScript, this.strProgramScriptObj, entityName);
			return new EntityCopyEntityDefinition[0];
		}
 
        // Necessario effettuare il cast di ogni object in ar_obj
        ar_entityCopyEntityDefinition = new EntityCopyEntityDefinition[ar_obj.size()];     
        for (int i = 0; i < ar_obj.size(); i++) {
        	ar_entityCopyEntityDefinition[i] = (EntityCopyEntityDefinition) ar_obj.get(i);
		}
        
 		return ar_entityCopyEntityDefinition;
	}

	
	 /*
     * Restituisce true se il nome fornito è quello di un correlation
     * name codificato nella clausola FROM in una correlation-clause
     * 
     */
	private boolean isSqlCorrelationName(SqlSubselectSelectInto subselectSelectInto, String name) {
		
		ArrayList<SqlFromTableReference> al_sqlFrom = null;
		SqlCorrelationClause sqlCorrelationClause = null;
		
		al_sqlFrom = subselectSelectInto.getFromTableReferences();
		
		// Scan table references
		for (SqlFromTableReference sqlFromTableReference : al_sqlFrom) {
			
			// Il reference table non ha la correlation clause
			if (!sqlFromTableReference.isThereCorrelationClause()) {
				continue;
			}
			
			sqlCorrelationClause = sqlFromTableReference.getCorrelationClause();
			
			// Il nome fornito è un correlation name
			if (name.equals(sqlCorrelationClause.getCorrName())) {
				return true;
			}
		}
		
		return false;
	}


	 /*
     * Restituisce true se il nome fornito è quello di una colonna dichiarata
     * nella clausola SELECT.
     * 
     * Il nome può essere quello effettivo della colonna, anche qualificato o il nuovo nome dichiarato
     * con AS new-col-name
     * 
     * Oppure il nome può essere dichiarato implicitamente da notazioni quali;
     * 
     * SELECT * FROM TB1, TBn
     * SELECT tb1.* FROM TB1 
     * SELECT corr.* FROM TB1 AS corr 
     * 
     * 
     */
	private boolean isSqlColumnName(SqlSubselectSelectInto subselectSelectInto,String name) {
		
		 ArrayList<SqlColumnInSelect> al_sqlColumn = null;
		 ArrayList<ArrayList<String>> al_al_columnImplicit = null;			// Un elemento per ogni tabella
		 String columnQualifier = "";
		 String columnName = "";
		 int iPoint;
		 
		 // Estrazione eventuale qualificatore dal nome
		 // Si può trattare di correlation-name o table-name.
		 iPoint = name.indexOf(".");
		 if (iPoint > 0) {
			 columnQualifier = name.substring(0, iPoint);
			 columnName = name.substring(iPoint + 1);
		 } else {
			 columnName = name;
		 }
		 
		 // Colonne 
		 al_sqlColumn = subselectSelectInto.getColumns();
		 
		 // Scan colonne in select
		 for (SqlColumnInSelect sqlColumn : al_sqlColumn) {
	
			 ////////////////////////////////////////////////////
			 // Colonna espressa con notazione implicita *
			 ////////////////////////////////////////////////////

			 // Non è un nome semplice di colonna
			 if (!sqlColumn.isColumnName()) {
				// Colonna espressa con * o qualifier.* a indicare implicitamente tutte le colonee di tutte le tabelle o di una specifica
				if (sqlColumn.isByStar() || sqlColumn.isByTableView() || sqlColumn.isByCorrelationName()) {
					al_al_columnImplicit = sqlColumn.getColumnsImplicit();
					for (ArrayList<String> al_columnImplicit : al_al_columnImplicit) {
						if (columnName.equals(al_columnImplicit.get(0))) {
							if (columnQualifier.equals("")) {
								return true;
							}
							if (columnQualifier.equals(al_columnImplicit.get(3))) {
								return true;
							}
						}
					} // end-for 
				} // end-if
				continue;
			 }

			 
			 ////////////////////////////////////////////////////////
			 // Colonna semplice espressa con il suo nome in Select
			 ////////////////////////////////////////////////////////
			 
			 // Nessun Match di nome
			 if (!columnName.equals(sqlColumn.getColumnName())) {
				continue;
 			 }
			 
			 // Il nome richiesto era senza qualificatore: match pieno
			 if (columnQualifier.equals("")) {
				return true;
			 } 
			 
			 // Verifico se il qualifier è lo stesso
			 if (columnQualifier.equals(sqlColumn.getColumnQualifier())) {
				return true;
			 }
		 }
		 return false;
	}


	
	/* ---------------------------------------------------------------------------------------
     * Restituisce il descrittore della map dello special register, se il token lo identifica
     * ---------------------------------------------------------------------------------------
     * 
     * Se non è uno special register restituisce null,
     * Token è il primo token eventuale dello special register
     * strNextTokens contiene tutti i successivi token 
     * 
     */
	private InnerInstructionWordsEntry getSqlSpecialRegisterKeyMap(String tokenStart, String strNextTokens) {
		
		Scanner scn = null;
		EnumPrecompilerReservedWords typeInstr = null;
		ArrayList<InnerTokenInstr> al_token = null;
		InnerTokenInstr innerTokenInstr = null;
		String token = "";
		
		scn = new Scanner(strNextTokens);
		al_token = new ArrayList<InnerTokenInstr> ();
		
		// Primi token chiave
		innerTokenInstr = new InnerTokenInstr();
		innerTokenInstr.token = tokenStart;
		al_token.add(innerTokenInstr);
		
		// Max altri 4 token individuano uno special register
		for (int i = 0; i < 3; i++) {
			token = nextTokenNoMemory(scn);
			if (token.equals("")) {
				break;
			}
			innerTokenInstr = new InnerTokenInstr();
			innerTokenInstr.token = token;
			al_token.add(innerTokenInstr);
		}
		

	    // Individua il tipo di parola chiave o NOT_ASSIGNED se non riconosciuta.
		typeInstr = getInstructionType(ictx, al_token, 0, false);     
		
		// Sicuramente non è uno special register
		if (typeInstr == EnumPrecompilerReservedWords.NOT_ASSIGNED) {
			return null;
		}
		 
		// Non e' una sequenza riconosciuta di keys che individua uno special register
		if (ictx.activeInstrKeyWordEntry.typeEntry != EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER) {
			return null;
		}
		
		// Special register
		
		return ictx.activeInstrKeyWordEntry;
	}


	
	/* ------------------------------------------------------------------------------
     * Restituisce true se il nome fornito è una function Sql 
     * ------------------------------------------------------------------------------
     * 
     * Una function può essere:
     * 
     * 1) Scalare predefinita 
     * 2) Table function predefinita 
     * 3) User-defined
     *    Si tratta di nomi di funzioni, tabellate in EnumPrecompilerReservedWords
     *    quali ALTDATE, ALTTIME etc., che possono essere qualificate da un dsname come
     *    per esempio dsn9.ALTDATE( ... )
     * 4) Definite con CREATE FUNCTION
     * 
     * I nomi delle function possono essere non qualificati o qualificati come:
     * 
     * owner.functionName
     * idServer.owner.functionName
     * 
     * E' possibile che il nome della function sia definito in qualche tabella
     * come una normale colonna. 
     * In questo caso viene restituito sempre true, in modo grossolano, in quanto
     * potrebbe NON essere la tabella sotto esame quella con la colonna definita
     * con lo stesso nome dell function. Verrà affinato successivamente.
     * 
     */
	private boolean isSqlFunction(String name) {
		
		EnumPrecompilerReservedWords functionNameCoded = null;
		
		functionNameCoded = getSqlFunction(name);
		if (functionNameCoded == EnumPrecompilerReservedWords.NOT_ASSIGNED) {
			return false;
		}
		
		return true;
	}

	 /*
     * Restituisce true se il nome fornito è una function aggregata.
     * 
     * Una function aggregata può essere:
     * 
     * AVG 
     * MAX 
     * MIN 
     * SUM 
     * COUNT 
     * COUNT_BIG 
     * STDDEV 
     * VARIANCE 
     * COVARIANCE 
     */
	private boolean isSqlFunctionAggregate(String name) {
		
		EnumPrecompilerReservedWords functionNameCoded = null;
		
		functionNameCoded = getSqlFunction(name);
		
		// Non è una function: NOT_ASSIGNED
		if (functionNameCoded == EnumPrecompilerReservedWords.NOT_ASSIGNED) {
			return false;
		}
		
		// NON E' una function aggregate
		if (!functionNameCoded.isAggregateFunction()) {
			return false;
		}
		
		// E' una function aggregate

		return true;
	}

	/* -----------------------------------------------------------------------------------------
     * Restituisce l'enumerazione che codifica il nome della function di cui è fornito il nome.
     * -----------------------------------------------------------------------------------------
     * Se non si tratta di una function codificata restituisce NOT_ASSIGNED
     * 
     * Una function può essere:
     * 
     * 1) Scalare predefinita 
     * 2) Table function predefinita 
     * 3) User-defined
     *    Si tratta di nomi di funzioni, tabellate in EnumPrecompilerReservedWords
     *    quali ALTDATE, ALTTIME etc., che possono essere qualificate da un dsname come
     *    per esempio dsn9.ALTDATE( ... )
     * 4) Definite con CREATE FUNCTION
     * 
     * I nomi delle function possono essere non qualificati o qualificati come:
     * Se il nome è qualificato e la funzione è una funzione standard senza qualificazione,
     * significa che si tratta di un campo di una SELECT come per esempio B.WEEK.
     * In questo caso NON viene riconosciuta come function.
     * 
     * owner.functionName
     * idServer.owner.functionName
     */
	@SuppressWarnings("unused")
	private EnumPrecompilerReservedWords getSqlFunction(String name) {
		
		EnumPrecompilerReservedWords functionNameCoded = null;
		
		functionNameCoded = EnumPrecompilerReservedWords.NOT_ASSIGNED;
		
		ArrayList<InnerInstructionWordsEntry> al_keyWord = null;
		String functionName = "";
		String owner = "";
		String idServer = "";
		int iPoint1 = 0;
		int iPoint2 = 0;
		
		iPoint1 = name.indexOf(".");
		if (iPoint1 > 0) {
			iPoint2 = name.indexOf(".", iPoint1 + 1);
		}
		
		// functionName
		if (iPoint1 < 0) {
			functionName = name;
		} else {
			// owner.functionName
			if (iPoint2 < 0) {
				owner = name.substring(0, iPoint1);
				functionName =  name.substring(iPoint1 + 1);
			// idServer.owner.functionName
			} else {
				owner = name.substring(0, iPoint1);
				idServer = name.substring(iPoint1 + 1, iPoint2);
				functionName =  name.substring(iPoint2 + 1);
			}
		}
		
		// Verifica se è il nome di una funzione
		al_keyWord = this.map_SqlReservedWords.get(functionName);
		
		// Non è sicuramente una function: NOT_ASSIGNED
		if (al_keyWord == null) {
			return functionNameCoded;
		}
		
		// E' il nome di una function ma l'owner è codificato
		// Si tratta di un campo qualificato di una Select con lo
		// stesso nome di una function
		if (al_keyWord.get(0).typeEntry == EnumInstrPrecompilerType.EXEC_SQL_FUNCTION 
		&& !owner.equals("")) {			
			return functionNameCoded;
		}

		
		// E' una function
		if (al_keyWord.get(0).typeEntry == EnumInstrPrecompilerType.EXEC_SQL_FUNCTION ) {
			return al_keyWord.get(0).en_WordReservedOwner;
		}
		
		// Sicuramente non è una function: NOT_ASSIGNED
		if (al_keyWord.size() == 1) {
			return functionNameCoded;
		}
		
		// Può essere una function codificata anche come labeled duration (es.DAY)
		for (int i = 0; i < al_keyWord.size(); i++) {
			if (al_keyWord.get(i).typeEntry == EnumInstrPrecompilerType.EXEC_SQL_FUNCTION ) {
				return al_keyWord.get(i).en_WordReservedOwner;
			}
		}
		
		// Non è una function: NOT_ASSIGNED
		
		return functionNameCoded;
	}

	/* -----------------------------------------------------------
     * Restituisce true se il nome fornito è una labeled duration.
     * -----------------------------------------------------------
     * 
     *  Si tratta di :
     *  
     *  YEAR
     *  YEARS
     *  MONTH
     *  MONTHS
     *  DAY
     *  DAYS
     *  HOUR
     *  HOURS
     *  MINUTE
     *  MINUTES
     *  SECOND
     *  SECONDS
     *  MICROSECOND
     *  MICROSECONDS
      * 
     */
	private boolean isLabeledDuration(String name) {
		EnumPrecompilerReservedWords labeledDurationCoded = null;

		labeledDurationCoded = getLabeledDuration(name);
		
		// E' labeled duration
		if (labeledDurationCoded != EnumPrecompilerReservedWords.NOT_ASSIGNED) {
			return true;
		}
		
		// Non e' labeled duration
		
		return false;
	}

	/* ------------------------------------------------------------------------------------------------
     * Restituisce l'enumerazione che codifica il nome della labeled duration di cui è fornito il nome.
     * ------------------------------------------------------------------------------------------------
     * 
     *  Si tratta di :
     *  
     *  YEAR
     *  YEARS
     *  MONTH
     *  MONTHS
     *  DAY
     *  DAYS
     *  HOUR
     *  HOURS
     *  MINUTE
     *  MINUTES
     *  SECOND
     *  SECONDS
     *  MICROSECOND
     *  MICROSECONDS
    * 
     */
	private EnumPrecompilerReservedWords getLabeledDuration(String name) {
		EnumPrecompilerReservedWords labeledDurationCoded = null;
		
		labeledDurationCoded = EnumPrecompilerReservedWords.NOT_ASSIGNED;
		
		ArrayList<InnerInstructionWordsEntry> al_keyWord = null;
		
		// Verifica se è il nome di una labeled duration
		al_keyWord = this.map_SqlReservedWords.get(name);
		
		// Non è sicuramente una labeled duration: NOT_ASSIGNED
		if (al_keyWord == null) {
			return labeledDurationCoded;
		}
		
		// Non è una labeled duration: NOT_ASSIGNED
		if (al_keyWord.get(0).typeEntry != EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION) {
			return labeledDurationCoded;
		}		
		
		// E' una labeled duration
		labeledDurationCoded = al_keyWord.get(0).en_WordReservedOwner;

		return labeledDurationCoded;
	}


	
	/* ----------------------
	 * Analisi clausola INTO
	 * ----------------------
	 * 
	 * Vengono estratte le variabili host che assumono la forma di:
	 * 
	 * :HostVar 
	 * :HostGrp    (implica tutti campi della struttura)
	 * :HostGrp.HostVar
	 * 
	 * Seguite da INDICATOR opzionale
	 * 
	 * Seguite da:
	 * 
	 * :HostVarIndicator
	 * :HostGrpIndicator    (implica tutti campi della struttura)
	 * :HostGrpIndicator.HostVarIndicator
	 * 
	 * 
	 * Update oggetto descrittore SqlSubselectSelectInto
	 * 
	 */
	private void analyzeSqlClauseInto(InstructionSql instruction, SqlSubselectSelectInto subselectSelectInto, String strInto) {
		
		Scanner scn = null;
		String token = "";
		String hostVar = "";
		String hostVarIndicator = "";
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Input vuoto
		if (strInto.equals("")) {
			return;
		}
		
		subselectSelectInto.setInto(true);
		
		scn = new Scanner(strInto);
		this.strNoMemoryToParse = strInto;
		token = nextToken(scn);							// INTO
		
		
		// Elenco di variabili host separate da virgola
		token = nextToken(scn);							// Prima host-var fuori ciclo
		
		// Scan generale token  
		while (!token.equals("")) {
			
			// Delimiter fine host-var
			if (token.equals(",")) {
				token = nextToken(scn);	
				continue;
			}
			
			hostVar = token;
			token = nextToken(scn);						// INDICATOR ?
			
			// Indicator per colonne a null
			if (token.equals("INDICATOR")) {
				token = nextToken(scn);					// host-var indicator
				hostVarIndicator = token;
			}

			// Caricamento ultima variabile host pendente 
			subselectSelectInto.getHostVarsIntoPure().add(hostVar);
			subselectSelectInto.getHostVarIndicatorIntoMap().put(hostVar, hostVarIndicator);
			hostVar = "";
			hostVarIndicator = "";

			token = nextToken(scn);
			
		} // end-while generale token clausola select
		
		// Caricamento ultima variabile host pendente 
		subselectSelectInto.getHostVarsIntoPure().add(hostVar);
		subselectSelectInto.getHostVarIndicatorIntoMap().put(hostVar, hostVarIndicator);
	}


	/*
	 * ----------------------
	 * Analisi clausola FROM
	 * ----------------------
	 * 
	 * Si analizzano i vari table-reference nella clausola FROM.
	 * Un table-reference identifica il nome di una tabella,  una view, un join o 
	 * una tabella risultato (espressa da Select) o una tabella intermedia
	 * e può assumere le seguenti forme:
	 * 
	 * 1) Single-table
	 * 2) Nested-table-expression
	 * 3) Data-change-table-reference
	 * 4) Table-function-reference
	 * 5) Joined-table
	 * 6) Table-locator-reference
	 * 7) xml-table-expression
	 * 
	 * Per semplicità di analisi si estraggono le singole table-reference
	 * delimitate da , e si analizzano separatamente, per evitare ambiguità.
	 * 
	 * Update oggetto descrittore SqlSubselectSelectInto
	 * 
	 */
	private void analyzeSqlClauseFrom(InstructionSql instruction, SqlSubselectSelectInto subselectSelectInto, String strFrom) throws SQLException, ExceptionAmrita {
		
		SqlFromTableReference sqlFromTableReference = null;
		String ar_strTableReference[] = null;
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		ar_strTableReference = splitParms(strFrom.substring(5).trim());		// FROM iniziale non viene considerato
		
		// Scan table-references che erano separati da virgola
		for (String strTableReference : ar_strTableReference) {
			
			sqlFromTableReference = analyzeSqlClauseFromTableReference(instruction, strTableReference.trim());
			if (instruction.isParsingError()) {
				return;
			}
			subselectSelectInto.getFromTableReferences().add(sqlFromTableReference);
		}
	}

    /* ---------------------------------------------------
     * Analisi singolo table-reference di clausola FROM
     * ---------------------------------------------------
     * 
     * Il singolo table-reference è già stato estratto dal chiamante e viene fornito in input
     *  
     * Si gestiscono tutti i tipi di table-reference:
     * 
 	 * 1) Single-table
	 * 2) Nested-table-expression
	 * 3) Data-change-table-reference
	 * 4) Table-function-reference
	 * 5) Joined-table
	 * 6) Table-locator-reference
	 * 7) xml-table-expression
	 * 
     */
	private SqlFromTableReference analyzeSqlClauseFromTableReference(InstructionSql instruction, String strTableReference) throws SQLException, ExceptionAmrita {
		
		Scanner scn = null;
		Scanner scnNext = null;
		Scanner scnWork = null;
		Scanner scnBetweenPar = null;
		SqlFromTableReference sqlFromTableReference = null;
		SqlFromTableReferenceTableFunction sqlFromTableReferenceTableFunction = null;
		SqlFromTableReferenceJoinedTable sqlFromTableReferenceJoinedTable = null;
		SqlCorrelationClause sqlCorrelationClause  = null;
		SqlExpression sqlExpression = null;
		SqlFullSelect sqlFullselect = null;
		SqlPeriodSpecification sqlPeriodSpecification = null;
		InstructionSql instructionGenericSql = null;
		String[] ar_parm = null;
		String strBetweenPar = null;
		String token = "";
		String tokenNext1 = "";
		String tokenNext2 = "";
		boolean isCorrelationClauseToSearch = false;
		int numConstant = 0;
		  
		sqlFromTableReference = new SqlFromTableReference();
		
		scn = new Scanner(strTableReference);
		this.strNoMemoryToParse = strTableReference;
		
		token = nextToken(scn);							//  
		scnNext = new Scanner(this.strNoMemoryToParse);	// A partire dal token successivo a token

		tokenNext1 = nextTokenNoMemory(scnNext);		//  
		tokenNext2 = nextTokenNoMemory(scnNext);		//  
   
		
		///////////////////////////////////////////////////
		// (6) Table-locator-reference  
		///////////////////////////////////////////////////
		
		if (token.equals("TABLE") && tokenNext1.equals("(") && tokenNext2.startsWith(":")) {
			sqlFromTableReference.setTypeTableReference(EnumSqlTableReferenceType.TABLE_LOCATOR_REFERENCE);
			token = nextToken(scn);						// (
			token = nextToken(scn);						// host-var
			sqlFromTableReference.setTableLocatorHostVar(token);
			token = nextToken(scn);						// LIKE
			token = nextToken(scn);						// table-name
			sqlFromTableReference.setTableLocatorEntityNameLike(token);
			token = nextToken(scn);						// )
			token = nextToken(scn);                     // |AS correlation-name ?
			isCorrelationClauseToSearch = true;;

			
		///////////////////////////////////////////////////
		// (4) Table-function-reference
		///////////////////////////////////////////////////
			
		} else if (token.equals("TABLE") && tokenNext1.equals("(") && isSqlFunction(tokenNext2)) {
			sqlFromTableReference.setTypeTableReference(EnumSqlTableReferenceType.TABLE_FUNCTION_REFERENCE);
			token = nextToken(scn);                     		// ( 
			strBetweenPar = extractStringBetweenPar(scn);		// Estrazione contenuto fra parentesi  TABLE ( ... )
			scnWork = new Scanner(strBetweenPar);				//
			token = nextToken(scnWork);               			// function-name 
			sqlFromTableReference.setTableFunctionName(token);	//
			token = nextToken(scnWork);                 		// ( 
			strBetweenPar = extractStringBetweenPar(scnWork);  	// Estrazione contenuto fra parentesi function-name(op1,.., opn)
			ar_parm = splitParms(strBetweenPar);				// Singoli operandi TABLE (function-name(op1,.., opn) ... )
			
			// Scan parametri dentro parentesi
			for (String strParm : ar_parm) {
				
				scnWork = new Scanner(strParm);
				token = nextToken(scnWork);                    // TABLE|expression
 	
				sqlFromTableReferenceTableFunction = new SqlFromTableReferenceTableFunction();

				// TABLE transition-table-name
				if (token.equals("TABLE")) {
					token = nextToken(scnBetweenPar);          // transition-table-name
					sqlFromTableReferenceTableFunction.setTransitionTableName(token);
					sqlFromTableReferenceTableFunction.setTransitionTable(true);
				// Expression
				} else {
					sqlExpression = analyzeSqlExpression(ictx, instruction, strParm);
					sqlFromTableReferenceTableFunction.setExpression(sqlExpression);
					sqlFromTableReferenceTableFunction.setExpression(true);
				}
				
				// Caricamento elemento in truttura
				sqlFromTableReference.getTableFunctionElements().add(sqlFromTableReferenceTableFunction);
			}
			// Lo scanner scnWork è posizionato sull'ultima parentesi a destra di function(...)
			token = nextToken(scnWork);							// |CARDINALITY ?
			if (token.equals("CARDINALITY")) {
				token = nextToken(scn); 						// MULTIPLIER ?
				if (token.equals("MULTIPLIER")) {
					token = nextToken(scn); 					// numeric-constant  
					numConstant = StringService._getNumericInt(token);
					sqlFromTableReference.setTableFunctionCardinalityMultiplierNumber(numConstant);
					sqlFromTableReference.setThereTableFunctionCardinality(true);
				} else {
					numConstant = StringService._getNumericInt(token);
					sqlFromTableReference.setTableFunctionCardinalityNumber(numConstant);
					sqlFromTableReference.setThereTableFunctionCardinality(true);
				}
				token = nextToken(scn); 						// correlation-clause ?
			}
			isCorrelationClauseToSearch = true;;

			
		///////////////////////////////////////////////////
		// (2) Nested-table-expression
		///////////////////////////////////////////////////
			
		} else if ((token.equals("TABLE") && tokenNext1.equals("(")) || (token.equals("(") && isFullSelectInsidePar(strTableReference)) ) {
			if (token.equals("TABLE")) {
				token = nextToken(scn); 				// (
			}
			// Estrazione contenuto fra parentesi
			strBetweenPar = extractStringBetweenPar(scn);
			sqlFullselect = analyzeSqlFullselect(ictx, instruction, strBetweenPar);
			if (instruction.isParsingError()) {return sqlFromTableReference;}
			sqlFromTableReference.setNestedTableFullSelect(sqlFullselect);
			sqlFromTableReference.setTypeTableReference(EnumSqlTableReferenceType.NESTED_TABLE_EXPRESSION);
			token = nextToken(scn); 
			isCorrelationClauseToSearch = true;

			
		///////////////////////////////////////////////////
		// (7) xml-table-expression
		///////////////////////////////////////////////////
				
		} else if (token.equals("XMLTABLE")) {
			token = nextToken(scn); 				// (
			// Estrazione contenuto fra parentesi
			strBetweenPar = extractStringBetweenPar(scn);
			sqlFromTableReference.setXmltableExpressionValue(strBetweenPar);
			sqlFromTableReference.setTypeTableReference(EnumSqlTableReferenceType.XMLTABLE_EXPRESSION);
			token = nextToken(scn);

			
		///////////////////////////////////////////////////
		// (5) Joined-table
		///////////////////////////////////////////////////
					
		} else if (strTableReference.indexOf(" JOIN ") > 0) {
			// Analisi ricorsiva 
			sqlFromTableReferenceJoinedTable = analyzeSqlClauseFromTableReferenceJoinedTable(ictx, instruction, strTableReference, true);
			if (instruction.isParsingError()) {
				return sqlFromTableReference;
			}
			sqlFromTableReference.setTypeTableReference(EnumSqlTableReferenceType.JOINED_TABLE);
			sqlFromTableReference.setJoinedTable(sqlFromTableReferenceJoinedTable);
			
			
		///////////////////////////////////////////////////
		// (3) Data-change-table-reference
		///////////////////////////////////////////////////
						
		} else if (token.equals("FINAL") || token.equals("OLD")) {
			if (token.equals("FINAL")) {
				sqlFromTableReference.setDataChangeFinal(true);
				token = nextToken(scn);					// TABLE
			} else if (token.equals("FINAL")) {
				sqlFromTableReference.setDataChangeOld(true);
				token = nextToken(scn);					// TABLE
			}
			token = nextToken(scn);						// (
			// Estrazione contenuto fra parentesi
			strBetweenPar = extractStringBetweenPar(scn);
			instructionGenericSql = new InstructionSql();
			instructionGenericSql.setSourceInstr(strBetweenPar);
			if (strBetweenPar.indexOf("INSERT") >= 0) {
				analyzeSqlInsert(ictx, instructionGenericSql);
  			} else if (strBetweenPar.indexOf("UPDATE") >= 0) {
 				analyzeSqlUpdate(ictx, instructionGenericSql);
 			} else if (strBetweenPar.indexOf("DELETE") >= 0) {
				analyzeSqlDelete(ictx, instructionGenericSql);
			} else if (strBetweenPar.indexOf("MERGE") >= 0) {
				analyzeSqlMerge(ictx, instructionGenericSql);
			} else {
				instruction.setParsingError(true);
				instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", "", null, this.strProgramScript, this.strProgramScriptObj);
				return sqlFromTableReference;
			}  
			if (instructionGenericSql.isParsingError()) {
				instruction.setParsingError(true);
				instruction.setSourceInstr(strBetweenPar);
				instruction.setTokenInError(instructionGenericSql.getTokenInError());
				instruction.setMsgParm(instructionGenericSql.getMsgParm());
				instruction.setExcpError(instructionGenericSql.getExcpError());
				return sqlFromTableReference;
			}
			sqlFromTableReference.setTypeTableReference(EnumSqlTableReferenceType.DATA_CHANGE_TABLE_REFERENCE);
			sqlFromTableReference.setDataChangeStmt(instructionGenericSql);
			token = nextToken(scn);	
			isCorrelationClauseToSearch = true;
			
			
		///////////////////////////////////////////////////
		// (1) Single-table
		///////////////////////////////////////////////////
			
		} else {
			sqlFromTableReference.setTypeTableReference(EnumSqlTableReferenceType.SINGLE_TABLE);
			sqlFromTableReference.setSingleTableEntityNameQualified(token);;	//
			token = nextToken(scn);								// 	FOR|AS|corr-name ?
			// Period-specification valida solo per DB2 Ibm Mainframe
			if (this.di.optDb2MainframeV10Compliance && !this.di.optDb2AixV19Compliance) {
				// Scan period-specification 
				while (token.equals("FOR")) {
					sqlPeriodSpecification = new SqlPeriodSpecification ();
					token = nextToken(scn);							// SYSTEM_TIME|BUSINESS_TIME
					if (token.equals("SYSTEM_TIME")) {
						sqlPeriodSpecification.setTypePeriod(EnumPrecompilerReservedWords.SQL_SYSTEM_TIME);
					} else if (token.equals("BUSINESS_TIME")) {
						sqlPeriodSpecification.setTypePeriod(EnumPrecompilerReservedWords.SQL_BUSINESS_TIME);
					} else {
						instruction.setParsingError(true);
						instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
	                    return sqlFromTableReference;
					}
					token = nextToken(scn);							// FROM|BETWEEN|AS OF
					if (token.equals("AS")) {
						sqlPeriodSpecification.setAsOf(true);
						token = nextToken(scn);						// OF
						token = nextToken(scn);						// value 
						sqlPeriodSpecification.setValue(token);
						token = nextToken(scn);						 
					} else if (token.equals("FROM")) {
						sqlPeriodSpecification.setFrom(true);
						token = nextToken(scn);						// value1
						sqlPeriodSpecification.setValue1(token);
						token = nextToken(scn);						// TO
						token = nextToken(scn);						// value2
						sqlPeriodSpecification.setValue2(token);
						token = nextToken(scn);	
					} else if (token.equals("BETWEEN")) {
						sqlPeriodSpecification.setBetween(true);
						token = nextToken(scn);						// value1
						sqlPeriodSpecification.setValue1(token);
						token = nextToken(scn);						// AND
						token = nextToken(scn);						// value2
						sqlPeriodSpecification.setValue2(token);
						token = nextToken(scn);	
					} else {
						instruction.setParsingError(true);
						instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
	                    return sqlFromTableReference;
					}
					sqlFromTableReference.getSingleTablePeriod().add(sqlPeriodSpecification);
				} // end-for period-specification
			}
			sqlFromTableReference.setTypeTableReference(EnumSqlTableReferenceType.SINGLE_TABLE);
			isCorrelationClauseToSearch = true;
		}

		
		///////////////////////////////////////////////////
		// Correlation-clause comune a + clausole
		///////////////////////////////////////////////////
	
		// Correlation-clause 
		if (!token.equals("") && isCorrelationClauseToSearch) {
			sqlFromTableReference.setCorrelationClause(true);
			if (token.equals("AS")) {
				token = nextToken(scn);                 // Correlation-name
			}
			// Correlation clause
			sqlCorrelationClause = new SqlCorrelationClause();
			sqlCorrelationClause.setCorrName(token);
			token = nextToken(scn);                     // ( ?
			
			// Colonne fra parentesi non presenti
			if (token.equals("")) {
				sqlFromTableReference.setCorrelationClause(sqlCorrelationClause);
				return sqlFromTableReference;
			}
			
			token = nextToken(scn); 					// Prima colonna
			
			// new-cols name
			while (!token.equals("") && !token.equals(")")) {
				if (token.equals(",")) {
					token = nextToken(scn);   
					continue;
				}
				sqlCorrelationClause.getNewColNames().add(token);
				token = nextToken(scn);
			}
			sqlFromTableReference.setCorrelationClause(sqlCorrelationClause);
		}
		
		return sqlFromTableReference;
	}

    /* ----------------------------------------------------------------------
     * Restituisce true se la stringa contiene fra parentesi una full-select
     * ----------------------------------------------------------------------
     * 
     */
	private boolean isFullSelectInsidePar(String strTableReference) {
		
		Scanner scn = null;
		String strBetweenPar = "";
		String svStrNoMemoryToParse = "";
		String token = "";
			
		svStrNoMemoryToParse = this.strNoMemoryToParse;
		scn = new Scanner(strTableReference);
		strBetweenPar = extractStringBetweenPar(scn);
		scn = new Scanner(strBetweenPar);
		token = nextToken(scn);									// (
		
		// Non era fra parentesi
		if (!token.equals("(")) {
			this.strNoMemoryToParse = svStrNoMemoryToParse;
			return false;
		}
		
		token = nextToken(scn);
		
		// Select
		if (token.equals("SELECT")) {
			this.strNoMemoryToParse = svStrNoMemoryToParse;
            return true;
		}
		
		this.strNoMemoryToParse = svStrNoMemoryToParse;
		return false;
	}


	/* ------------------------------------------------------
     * Analisi table-reference joined-table di clausola FROM
     * ------------------------------------------------------
     *  
     *  Il metodo è ricorsivo in quanto il costrutto joined-table è descritto ricorsivamente.
     *  
     *  Situazioni gestite joined-table:
     *  
     *  1) (joined-table)
     *  2) table-reference INNER|LEFT|RIGHT|FULL|OUTER JOIN table-reference1 ON condition1
     *                     ...
     *                     INNER|LEFT|RIGHT|FULL|OUTER JOIN table-referencen ON conditionN
     *  3) table-reference INNER|LEFT|RIGHT|FULL|OUTER JOIN (joined-table)   ON condition
     *  
 	 */
	private SqlFromTableReferenceJoinedTable analyzeSqlClauseFromTableReferenceJoinedTable(InnerContextAnalysis ictx, InstructionSql instruction, String strToParse, boolean fullAnalisysToDo) throws SQLException, ExceptionAmrita {
		
		Scanner scn = null;
		SqlFromTableReferenceJoinedTable sqlFromTableReferenceJoinedTable = null;
		SqlFromTableReferenceJoinedTable sqlFromTableReferenceJoinedTableWork = null;
		SqlFromTableReferenceJoinedTable sqlFromTableReferenceJoinedTableRecursive = null;
		SqlFromTableReference sqlFromTableReference = null;
 		SqlFromTableReferenceJoinedTableCondition sqlJoinCondition = null;
		SqlSearchConditions sqlSearchConditions = null;
		ArrayList<SqlFromTableReferenceJoinedTableCondition> al_sqlJoinedTableCondition = null;
		ArrayList<String> al_strJoinedTableToParse = null;
		String strToParseWip = "";
		String strJoinCondition = "";
		String strTableReference = "";
		String strBetweenPar = "";
		String token = "";
		String svStrNoMemoryToParse = "";
		StringBuffer sbJoin = null;
		boolean isFirstJoinedTable = true;
		int iOn = 0;
		
		sqlFromTableReferenceJoinedTable = new SqlFromTableReferenceJoinedTable();
        sqlFromTableReferenceJoinedTableWork = new SqlFromTableReferenceJoinedTable();
        strToParseWip = strToParse;
        
		scn = new Scanner(strToParseWip);
		this.strNoMemoryToParse = strToParseWip;

		token = nextToken(scn);								// (|table-reference
		 
		// joined-table: attivazione ricorsiva
		if (token.equals("(")) {
			strBetweenPar = extractStringBetweenPar(scn);
			// Attivazione ricorsiva
			sqlFromTableReferenceJoinedTableWork = analyzeSqlClauseFromTableReferenceJoinedTable(ictx, instruction, strBetweenPar, true);
			sqlFromTableReferenceJoinedTable.setJoinedTableInvoked(true);
			sqlFromTableReferenceJoinedTable.setJoinedTableInvoked(sqlFromTableReferenceJoinedTableWork);
			return sqlFromTableReferenceJoinedTable;
		}
		
		// Estrazione primo table reference se richiesto
		if (fullAnalisysToDo) {
			sqlFromTableReferenceJoinedTable.getTableReference().setSingleTableEntityNameQualified(token);
			
			token = nextToken(scn);								// |AS|Corr-name|INNER|LEFT|RIGHT|FULL|JOIN
			if (token.equals("AS")) {
				token = nextToken(scn);							// Corr-name
				sqlFromTableReferenceJoinedTableWork.getTableReference().getCorrelationClause().setCorrName(token);
			} else {
				// Corr-name|INNER|LEFT|RIGHT|FULL|JOIN
				if (!token.equals("JOIN") 
				&&  !token.equals("LEFT")
				&&  !token.equals("RIGHT")
				&&  !token.equals("FULL")
				&&  !token.equals("INNER")) {
					sqlFromTableReferenceJoinedTableWork.getTableReference().getCorrelationClause().setCorrName(token);
				}
			}
			// Nuova stringa da parsificare
			strToParseWip = this.strNoMemoryToParse;
		}
		
		// Estrazione insiemi completi di joined-table, fino a ON condition di join inclusa

		scn = new Scanner(strToParseWip);
		this.strNoMemoryToParse = strToParseWip;
		al_strJoinedTableToParse = new ArrayList<String> ();
		
		
		// Ciclo generale di estrazione join
		token = nextToken(scn);
		while (!token.equals("")) {
			
			sbJoin = new StringBuffer();

			// Accodo primi token di identificazione join
			while (!token.equals("") && token.equals("JOIN") || token.equals("INNER") || token.equals("LEFT") || token.equals("RIGHT") || token.equals("FULL") || token.equals("OUTER")) {
				sbJoin.append(" " + token);
				token = nextToken(scn);
			}
			
			// Accodo fino a successiva definizione di join
			while (!token.equals("") && (!token.equals("JOIN") && !token.equals("INNER") && !token.equals("LEFT") && !token.equals("RIGHT") && !token.equals("FULL"))) {
				// Definizione ricorsiva
				if (token.equals("(")) {
					strBetweenPar = extractStringBetweenPar(scn);
					sbJoin.append(" ( " + strBetweenPar + " ) ");
					token = nextToken(scn);
					continue;
				}
				
				// Corpo definizione join
				sbJoin.append(" " + token);
				token = nextToken(scn);
			}
			
			al_strJoinedTableToParse.add(sbJoin.toString().trim());
		}
		
		
		// Analisi singole join estratte
		
		// Scan e parsing joined table estratte 
		for (String strJoinedTableToParse : al_strJoinedTableToParse) {
			
			scn = new Scanner(strJoinedTableToParse);
			this.strNoMemoryToParse = strJoinedTableToParse;
            token = nextToken(scn);
            
             
    		if (token.equals("LEFT")) {
    			sqlFromTableReferenceJoinedTableWork.setLeftJoin(true);
    			token = nextToken(scn);							// !OUTER|JOIN
    		} else if (token.equals("RIGHT")) {
    			sqlFromTableReferenceJoinedTableWork.setRightJoin(true);
    			token = nextToken(scn);							// |OUTER|JOIN
    		} else if (token.equals("FULL")) {
    			sqlFromTableReferenceJoinedTableWork.setFullJoin(true);
    			token = nextToken(scn);							// |OUTER|JOIN
    		} else if (token.equals("INNER")) {
    			sqlFromTableReferenceJoinedTableWork.setFullJoin(true);
    			token = nextToken(scn);							// JOIN
    		}
    		if (token.equals("OUTER")) {
    			sqlFromTableReferenceJoinedTableWork.setOuterJoin(true);
    			token = nextToken(scn);							// JOIN
    		}
      		// Può essere solo JOIN
    		if (!token.equals("JOIN")) {
    			instruction.setParsingError(true);
    			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
    			return sqlFromTableReferenceJoinedTable;
    		}

       		strTableReference = this.strNoMemoryToParse.trim();

       		// Joined-table:attivazione ricorsiva
    		if (strTableReference.startsWith("(") && !isTableReference(strTableReference)) {
    			token = nextToken(scn);							// (
				strBetweenPar = extractStringBetweenPar(scn);
				svStrNoMemoryToParse = this.strNoMemoryToParse;
				sqlFromTableReferenceJoinedTableRecursive = analyzeSqlClauseFromTableReferenceJoinedTable(ictx, instruction, strBetweenPar, true);
				this.strNoMemoryToParse = svStrNoMemoryToParse;
				sqlFromTableReferenceJoinedTableWork.setJoinedTableInvoked(sqlFromTableReferenceJoinedTableRecursive);
				token = nextToken(scn);								// ON
				strJoinCondition = this.strNoMemoryToParse.trim();
			} else {
	    		iOn = strTableReference.indexOf(" ON ");
	    		if (iOn < 0) {
	    			instruction.setParsingError(true);
	    			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
	    			return sqlFromTableReferenceJoinedTable;
	    		}
				sqlFromTableReference = analyzeSqlClauseFromTableReference(instruction, strTableReference.substring(0, iOn).trim());
				sqlFromTableReferenceJoinedTableWork.setTableReferenceJoin(sqlFromTableReference);
	 		    strJoinCondition = strTableReference.substring(iOn + 4).trim();
			}
    	  
    		// Normalizzazione join condition (search o expression) dopo ON
     		if (strJoinCondition.startsWith("(")) {
    			strJoinCondition = strJoinCondition.substring(1);
    			strJoinCondition = strJoinCondition.substring(0, strJoinCondition.length() - 1).trim();
    		}

    		// Analisi join-condition se full-outer-join
    		if (sqlFromTableReferenceJoinedTableWork.isFullJoin()) {
    			al_sqlJoinedTableCondition = analyzeSqlClauseFromTableReferenceJoinedTableConditions(ictx, instruction, strJoinCondition);
    			if (instruction.isParsingError()) {return sqlFromTableReferenceJoinedTableWork;}
    			sqlFromTableReferenceJoinedTableWork.setJoinConditions(al_sqlJoinedTableCondition);
    		}
    		
    		// Analisi join-condition se left/right/inner join
    		if (!sqlFromTableReferenceJoinedTableWork.isFullJoin()) {
    			sqlSearchConditions = analyzeSqlSearchConditions(ictx, instruction, strJoinCondition.toString().trim());
    			if (instruction.isParsingError()) {return sqlFromTableReferenceJoinedTableWork;}
    			sqlJoinCondition = new SqlFromTableReferenceJoinedTableCondition();
    			sqlJoinCondition.setSearchCondition(true);
    			sqlJoinCondition.setSearchCondition(sqlSearchConditions);
    			sqlFromTableReferenceJoinedTableWork.getJoinConditions().add(sqlJoinCondition);
    		}
 			
			// Prima joined table: le informazioni vanno su questo oggetto corrente
			if (isFirstJoinedTable) {
				sqlFromTableReferenceJoinedTable.setTableReference(sqlFromTableReferenceJoinedTableWork.getTableReference());
				sqlFromTableReferenceJoinedTable.setInnerJoin(sqlFromTableReferenceJoinedTableWork.isInnerJoin());
				sqlFromTableReferenceJoinedTable.setLeftJoin(sqlFromTableReferenceJoinedTableWork.isLeftJoin());
				sqlFromTableReferenceJoinedTable.setRightJoin(sqlFromTableReferenceJoinedTableWork.isRightJoin());
				sqlFromTableReferenceJoinedTable.setFullJoin(sqlFromTableReferenceJoinedTableWork.isFullJoin());
				sqlFromTableReferenceJoinedTable.setOuterJoin(sqlFromTableReferenceJoinedTableWork.isOuterJoin());
				sqlFromTableReferenceJoinedTable.setTableReferenceJoin(sqlFromTableReferenceJoinedTableWork.getTableReferenceJoin());
				sqlFromTableReferenceJoinedTable.setJoinConditions(sqlFromTableReferenceJoinedTableWork.getJoinConditions());
				sqlFromTableReferenceJoinedTableWork = new SqlFromTableReferenceJoinedTable();
				isFirstJoinedTable = false;
				continue;
			}
			
			// Joined table successive: le accodo come joined-table extra
			sqlFromTableReferenceJoinedTable.getJoinedTablesExtra().add(sqlFromTableReferenceJoinedTableWork);
			sqlFromTableReferenceJoinedTableWork = new SqlFromTableReferenceJoinedTable();

		}
		
		return sqlFromTableReferenceJoinedTable;
	}

	
    /* --------------------------------------------------
     * Restituisce true se la string è un table-reference
     * --------------------------------------------------
     * 
     * Si tratta dei table reference presenti nella clausola FROM e JOIN
     * 
     * 
     */
	private boolean isTableReference(String str) {
		
		Scanner scn = null;
		String token = "";
		String tokenNext1 = "";
		String tokenNext2 = "";
		
		scn = new Scanner(str);
		token = nextTokenNoMemory(scn);					//  
		tokenNext1 = nextTokenNoMemory(scn);			//  
		tokenNext2 = nextTokenNoMemory(scn);			//  
   
		
		// Table-locator-reference  
		if (token.equals("TABLE") && tokenNext1.equals("(") && tokenNext2.startsWith(":")) {
            return true;
		}   
			
		// Table-function-reference
		if (token.equals("TABLE") && tokenNext1.equals("(") && isSqlFunction(tokenNext2)) {
			return true;
		}
			
		// Nested-table-expression
		if ((token.equals("TABLE") && tokenNext1.equals("(")) || (token.equals("(") && isFullSelectInsidePar(str)) ) {
			return true;
		}

		// xml-table-expression
		if (token.equals("XMLTABLE")) {
			return true;
		}

		// Data-change-table-reference
		if (token.equals("FINAL") || token.equals("OLD")) {
			return true;
		}
		
		// NO Joined-table
		if (str.indexOf(" JOIN ") > 0) {
			return false;
		}	

	    // Single-table
		return true;
	}


	/*
     * ---------------------------------------------------------------------------
     * Analisi join-condition di espressioni per FULL OUTER JOIN di clausola FROM 
     * ---------------------------------------------------------------------------
     * 
     * Si tratta di:
     * 
     *  AND
     *  full-join-expression = full-join-expression
     * 
     */
	private ArrayList<SqlFromTableReferenceJoinedTableCondition> analyzeSqlClauseFromTableReferenceJoinedTableConditions(	
									InnerContextAnalysis ictx
								  , InstructionSql instruction
								  , String strJoinCondition) {

		Scanner scn = null;
		ArrayList<SqlFromTableReferenceJoinedTableCondition> al_sqlJoinedTableCondition = null;
		SqlFromTableReferenceJoinedTableCondition sqlJoinedTableCondition = null;
		String token = "";
		String strBetweenPar = "";
        int cntParOpen = 0;		
		
		al_sqlJoinedTableCondition = new ArrayList<SqlFromTableReferenceJoinedTableCondition> ();
		
		scn = new Scanner(strJoinCondition);
		this.strNoMemoryToParse = strJoinCondition;
		token = nextToken(scn);
		
		// scan token join condition
		while (!token.equals("")) {
			if (token.equals("AND")) {
				sqlJoinedTableCondition = new SqlFromTableReferenceJoinedTableCondition();
				sqlJoinedTableCondition.setFullOuterJoinAnd(true);
				al_sqlJoinedTableCondition.add(sqlJoinedTableCondition);
				token = nextToken(scn);
				continue;
			}
			
			// --- Analisi a sinistra segno di uguale ---
			sqlJoinedTableCondition = new SqlFromTableReferenceJoinedTableCondition();
			
			// CAST
			if (token.equals("CAST")) {
				token = nextToken(scn);				// (
				cntParOpen = 1;	
				while (!token.equals("") && cntParOpen > 0) {
					strBetweenPar = strBetweenPar + " " + token;
					if (token.equals("(")) {cntParOpen++;}
					if (token.equals(")")) {cntParOpen--;}
					token = nextToken(scn);
				}
				sqlJoinedTableCondition.setCastFunctionLeft(true);
				sqlJoinedTableCondition.setCastFunctionLeftValue(strBetweenPar);
				al_sqlJoinedTableCondition.add(sqlJoinedTableCondition);
				
			// COALESCE
			} else if (token.equals("COALESCE")) {
				cntParOpen = 1;	
				while (!token.equals("") && cntParOpen > 0) {
					strBetweenPar = strBetweenPar + " " + token;
					if (token.equals("(")) {cntParOpen++;}
					if (token.equals(")")) {cntParOpen--;}
					token = nextToken(scn);
				}
				sqlJoinedTableCondition.setCoalesceLeft(true);
				sqlJoinedTableCondition.setCoalesceLeftValue(strBetweenPar);
				al_sqlJoinedTableCondition.add(sqlJoinedTableCondition);
			
			// column-name
			} else {
				sqlJoinedTableCondition.setColumnNameLeft(true);
				sqlJoinedTableCondition.setColumnNameLeft(token);
				al_sqlJoinedTableCondition.add(sqlJoinedTableCondition);
			}
			
			token = nextToken(scn);				// =
			
			 
			// --- Analisi a destra segno di uguale ---
			sqlJoinedTableCondition = new SqlFromTableReferenceJoinedTableCondition();

			// CAST
			if (token.equals("CAST")) {
				token = nextToken(scn);				// (
				cntParOpen = 1;	
				while (!token.equals("") && cntParOpen > 0) {
					strBetweenPar = strBetweenPar + " " + token;
					if (token.equals("(")) {cntParOpen++;}
					if (token.equals(")")) {cntParOpen--;}
					token = nextToken(scn);
				}
				sqlJoinedTableCondition.setCastFunctionRight(true);
				sqlJoinedTableCondition.setCastFunctionRightValue(strBetweenPar);
				al_sqlJoinedTableCondition.add(sqlJoinedTableCondition);
				
			// COALESCE
			} else if (token.equals("COALESCE")) {
				cntParOpen = 1;	
				while (!token.equals("") && cntParOpen > 0) {
					strBetweenPar = strBetweenPar + " " + token;
					if (token.equals("(")) {cntParOpen++;}
					if (token.equals(")")) {cntParOpen--;}
					token = nextToken(scn);
				}
				sqlJoinedTableCondition.setCoalesceRight(true);
				sqlJoinedTableCondition.setCoalesceRightValue(strBetweenPar);
				al_sqlJoinedTableCondition.add(sqlJoinedTableCondition);
				
			// column-name
			} else {
				sqlJoinedTableCondition.setColumnNameRight(true);
				sqlJoinedTableCondition.setColumnNameRight(token);
				al_sqlJoinedTableCondition.add(sqlJoinedTableCondition);
			}
			
			token = nextToken(scn);						// AND|CAST|COALESCE|column-name
		}
		return al_sqlJoinedTableCondition;
	}


	/* ----------------------
	 * Analisi clausola WHERE
	 * ----------------------
	 * 
	 * 
	 * Update oggetto descrittore SqlSubselectSelectInto
	 * 
	 */
	private void analyzeSqlClauseWhere(InstructionSql instruction, SqlSubselectSelectInto subselectSelectInto, String strWhere) throws SQLException, ExceptionAmrita {
		
		SqlSearchConditions searchConditions = null;
		String strSearchConditions = "";
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Input vuoto
		if (strWhere.equals("")) {
			return;
		}
		
		subselectSelectInto.setWhere(true);
		strSearchConditions = strWhere.substring(5).trim();  // Elimina WHERE
		
		
		// Parentesi NON bilanciate
		if (!isBalancedPar(strSearchConditions)) {
			instruction.setParsingError(true);
	       	instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0037", "(", null, this.strProgramScript, this.strProgramScriptObj);
	       	return;
		}
        
		// Parsing condizioni di search
		searchConditions = analyzeSqlSearchConditions(ictx, instruction, strSearchConditions.toString().trim());
		subselectSelectInto.setWhere(searchConditions);
	}


	/* --------------------------
	 * Analisi clausola GROUP BY
	 * --------------------------
	 * 
	 * 
	 * Update oggetto descrittore SqlSubselectSelectInto
	 * 
	 */
	private void analyzeSqlClauseGroupBy(InstructionSql instruction, SqlSubselectSelectInto subselectSelectInto, String strGroupBy) throws SQLException, ExceptionAmrita {
		
		Scanner scn = null;
		SqlExpression expression = null;
		String token = "";
		StringBuffer sbExpression = null;
		int cntParOpen = 0;
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Input vuoto
		if (strGroupBy.equals("")) {
			return;
		}
		
		subselectSelectInto.setGroupBy(true);
		
		sbExpression = new StringBuffer();
		scn = new Scanner(strGroupBy);
		this.strNoMemoryToParse = strGroupBy;
		token = nextToken(scn);							// GROUP
		token = nextToken(scn);							// BY
		token = nextToken(scn);
		
		
		// Elenco espressioni di grouping
		
		// Scan generale token  
		while (!token.equals("")) {
			
			// Fine espressione grouping by
			if (token.equals(",")) {
				expression = analyzeSqlExpression(ictx, instruction, sbExpression.toString());
				if (instruction.isParsingError()) {
					return;
				}
				subselectSelectInto.getGroupByExpressions().add(expression);
				sbExpression = new StringBuffer();
				token = nextToken(scn);
				continue;
			}
			
			sbExpression.append(" " + token);
			
			// Estrazione contenuto fra parentesi, che potrebbe avere anche delle virgole
			if (token.equals("(")) {
				cntParOpen = 1;
				token = nextToken(scn);	
				while (!token.equals("") && cntParOpen > 0) {
					sbExpression.append(" " + token);
					if (token.equals("(")) {cntParOpen++;}
					if (token.equals(")")) {cntParOpen--;}
					token = nextToken(scn);	
				}
				continue;
			}
			
			token = nextToken(scn);	
				
				
		} // end-while generale token clausola select
		
		// Ultima espressione grouping by pendente
		if (!sbExpression.toString().trim().equals("")) {
			expression = analyzeSqlExpression(ictx, instruction, sbExpression.toString().trim());
			subselectSelectInto.getGroupByExpressions().add(expression);
		}		
	}

	/* -----------------------
	 * Analisi clausola HAVING
	 * -----------------------
	 * 
	 * 
	 * Update oggetto descrittore SqlSubselectSelectInto
	 * 
	 */
	private void analyzeSqlClauseHaving(InstructionSql instruction, SqlSubselectSelectInto subselectSelectInto, String strHaving) throws SQLException, ExceptionAmrita {
		
		SqlSearchConditions searchConditions = null;
		String strSearchConditions = "";
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Input vuoto
		if (strHaving.equals("")) {
			return;
		}
		
		subselectSelectInto.setHaving(true);
		
		strSearchConditions = strHaving.substring(7).trim();  // Elimina HAVING  
		searchConditions = analyzeSqlSearchConditions(ictx, instruction, strSearchConditions);
		subselectSelectInto.setHaving(searchConditions);
		
	}



	/*
	 * --------------------------
	 * Analisi clausola ORDER BY
	 * --------------------------
	 * 
	 * 
	 * Update oggetto descrittore SqlSubselectSelectInto
	 * 
	 */
	private void analyzeSqlClauseOrderBy(InstructionSql instruction, SqlSubselectSelectInto subselectSelectInto, String strOrderBy) throws SQLException, ExceptionAmrita {
		
		Scanner scn = null;
		Scanner scnNext = null;
		SqlExpression sortKeyExpression = null;
		SqlOrderBy sqlOrderBy = null;
		String[] ar_sortKey = null;
		String token = "";
		String tokenNext = "";
		String strSortKeys = "";
		StringBuffer sbSortKeyExpression = null;
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Input vuoto
		if (strOrderBy.equals("")) {
			return;
		}
		
		subselectSelectInto.setOrderBy(true);
		
		scn = new Scanner(strOrderBy);
		this.strNoMemoryToParse = strOrderBy;
		token = nextToken(scn);							// ORDER
		token = nextToken(scn);							// BY
		token = nextToken(scn);							// INPUT|ORDER|Col-name|col-num|sort-key-expression
		
		// INPUT SEQUENCE
		if (token.equals("INPUT")) {
			token = nextToken(scn);						// SEQUENCE
			subselectSelectInto.setSelectAll(true);
			token = nextToken(scn);	
			sqlOrderBy = new SqlOrderBy();
			sqlOrderBy.setOrderByInputSequence(true);
			subselectSelectInto.getOrderByElements().add(sqlOrderBy);
			return;
		} 
		 
		// ORDER OF table-designator
		if (token.equals("ORDER")) {
			token = nextToken(scn);						// OF
			token = nextToken(scn);						// table-designator
			sqlOrderBy = new SqlOrderBy();
			sqlOrderBy.setTableDesignator(token);
			sqlOrderBy.setOrderByOfTableDesignator(true);
			subselectSelectInto.getOrderByElements().add(sqlOrderBy);
			return;
		} 
		
		// Elenco di colonne, numeri colonne e sort-key expression separati da ,
		strSortKeys = token + " " + this.strNoMemoryToParse;
		ar_sortKey = splitParms(strSortKeys);
		
		// Scan sort keys
		for (String sortKey : ar_sortKey) {
			
			sqlOrderBy = new SqlOrderBy();
			scn = new Scanner(sortKey);
			this.strNoMemoryToParse = sortKey;
			token = nextToken(scn);						// columnName|integer|sort-key-expr
			scnNext = new Scanner(this.strNoMemoryToParse);
			tokenNext = nextTokenNoMemory(scnNext);
			
			// Nome colonna
			if (isSqlColumnName(subselectSelectInto, token) && (tokenNext.equals("") || tokenNext.equals("ASC") || tokenNext.equals("DESC"))) {
				sqlOrderBy = new SqlOrderBy();
				sqlOrderBy.setColumnName(token);
				sqlOrderBy.setOrderByColumnName(true);
				token = nextToken(scn);					// |ASC|DESC
				subselectSelectInto.getOrderByElements().add(sqlOrderBy);
			
			// Numero colonna
			} else if (StringService._isNumericInt(token) && (tokenNext.equals("") || tokenNext.equals("ASC") || tokenNext.equals("DESC"))) {
				sqlOrderBy.setColumnNumber(StringService._getNumericInt(token));
				sqlOrderBy.setOrderByColumnNumber(true);
				token = nextToken(scn);					// |ASC|DESC
				subselectSelectInto.getOrderByElements().add(sqlOrderBy);

			// Sort-key expression
			} else {
				sbSortKeyExpression = new StringBuffer ();
				// Estrazione espressione
				while (!token.equals("") && token.equals("ASC") && token.equals("DESC")) {
					sbSortKeyExpression.append(" " + token);
					token = nextToken(scn);
				}
				sortKeyExpression = analyzeSqlExpression(ictx, instruction, sbSortKeyExpression.toString().trim());
				if (instruction.isParsingError()) {return;}
			    sqlOrderBy.setOrderBySortKeyExpression(true);
				sqlOrderBy.setSortKeyExpression(sortKeyExpression);
				subselectSelectInto.getOrderByElements().add(sqlOrderBy);
			}
			
			if (token.equals("ASC")) {
				sqlOrderBy.setOrderAscending(true);
			}
			if (token.equals("DESC")) {
				sqlOrderBy.setOrderDescending(true);
			}
		}
				
	}


	/* ---------------------------
	 * Analisi clausola isolation
	 * ---------------------------
	 * 
	 * 
	 * Update oggetto descrittore SqlSubselectSelectInto
	 * 
	 */
	private void analyzeSqlClauseIsolation(InstructionSql instruction, SqlSubselectSelectInto subselectSelectInto, String strSkipIsolation) {

		Scanner scn = null;
		String token = "";
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Input vuoto
		if (strSkipIsolation.equals("")) {
			return;
		}
		
		subselectSelectInto.setIsolation(true);
		
		scn = new Scanner(strSkipIsolation);
		this.strNoMemoryToParse = strSkipIsolation;
		token = nextToken(scn);	
		
		// Scan generale token  
		while (!token.equals("")) {
			
			// SKIP LOCK DATA
			if (token.equals("SKIP")) {
				token = nextToken(scn);					// LOCK
				token = nextToken(scn);					// DATA
				subselectSelectInto.setSkipLockData(true);
				token = nextToken(scn);
				continue;
			}

			// WITH .. USE
			if (token.equals("WITH")) {
				token = nextToken(scn);					// RR|RS|CS|UR
				subselectSelectInto.setIsolation(token);
				token = nextToken(scn);					// USE ?
				if (!token.equals("USE")) {
					continue;
				}
				token = nextToken(scn);					// EXCLUSIVE|UPDATE|SHARE
				if (token.equals("EXCLUSIVE")) {
					subselectSelectInto.setIsolationUseAndKeepExclusiveLocks(true);
					token = nextToken(scn);
					continue;
				}
				if (token.equals("UPDATE")) {
					subselectSelectInto.setIsolationUseAndKeepUpdateLocks(true);
					token = nextToken(scn);
					continue;
				}
				if (token.equals("SHARE")) {
					subselectSelectInto.setIsolationUseAndKeepShareLocks(true);
					token = nextToken(scn);
					continue;
				}

				// parametro non gestito: skip
				token = nextToken(scn);
				continue;
			}
				
			token = nextToken(scn);				
			
		} // end-while generale token clausola isolation
		
	}


	/*
	 * -------------------------
	 * Analisi clausola QUERYNO
	 * -------------------------
	 * 
	 * 
	 * Update oggetto descrittore SqlSubselectSelectInto
	 * 
	 */
	private void analyzeSqlClauseQueryno(InstructionSql instruction, SqlSubselectSelectInto subselectSelectInto, String strQueryno) {

		Scanner scn = null;
		String token = "";
		int numQueryno = 0;
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Input vuoto
		if (strQueryno.equals("")) {
			return;
		}
		
       	subselectSelectInto.setQueryno(true);

		scn = new Scanner(strQueryno);
		this.strNoMemoryToParse = strQueryno;
		
		token = nextToken(scn);					// QUERYNO
		token = nextToken(scn);					// integer
		numQueryno = StringService._getNumericInt(token);
		subselectSelectInto.setQueryno(true);
		subselectSelectInto.setQueryno(numQueryno);
	}


	/*
	 * Analisi clausola FETCH FIRST
	 * 
	 * Update oggetto descrittore SqlSubselectSelectInto
	 * 
	 */
	private void analyzeSqlClauseFetch(InstructionSql instruction, SqlSubselectSelectInto subselectSelectInto, String strFetch) {

		Scanner scn = null;
		String token = "";
		int numRows = 1;
		
		// Errori precedenti
		if (instruction.isParsingError()) {
			return;
		}
		
		// Input vuoto
		if (strFetch.equals("")) {
			return;
		}
		
        subselectSelectInto.setFetchFirstNRows(true);

		scn = new Scanner(strFetch);
		this.strNoMemoryToParse = strFetch;
		
		token = nextToken(scn);						// FETCH
		token = nextToken(scn);						// FIRST
		token = nextToken(scn);						// integer ?
		if (StringService._isNumericInt(token)) {
			numRows = StringService._getNumericInt(token);
			token = nextToken(scn);
		}
		token = nextToken(scn);						// ROW|ROWS
		token = nextToken(scn);						// ONLY
		
		subselectSelectInto.setFetchFirstNRows(true);
		subselectSelectInto.setFetchFirstRowsNumber(numRows);
		
	}


	/*
     * -----------------------------------------------------------------
     * Analisi insiemi di clausole UNION/EXCEPT/INTERSECT di fullselect
     * -----------------------------------------------------------------
     * 
     * Si tratta di occorrenze di:
     * 
     * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect|(full-select) ....
     * 
     */
	private void analyzeSqlFullselectLinkedTo(InnerContextAnalysis ictx, InstructionSql instruction, SqlFullSelect fullselect, String strFullSelectLinkedTo) throws SQLException, ExceptionAmrita {
		
		Scanner scn = null;
		SqlFullSelectLinkedTo fullSubSelectLinkedTo = null;
		SqlFullSelect subQuery = null;
		SqlSubselectSelectInto subselect = null;
        StringBuffer sbSubselect = null;
		String strSubQuery = "";
		String token = "";
        
		fullselect.setFullSelectWithLinkedTo(true);
		
		scn = new Scanner(strFullSelectLinkedTo);	
		token = nextToken(scn);					// UNION|EXCEPT|INTERSECT 
		
		// Scan generale occorrenze linked to
		while (!token.equals("")) {
			
			fullSubSelectLinkedTo = new SqlFullSelectLinkedTo();
			
			// UNION
			if (token.equals("UNION")) {
				fullSubSelectLinkedTo.setTypeLink(EnumPrecompilerReservedWords.SQL_UNION);
				token = nextToken(scn);
			// EXCEPT
			} else if (token.equals("EXCEPT")) {
				fullSubSelectLinkedTo.setTypeLink(EnumPrecompilerReservedWords.SQL_EXCEPT);
				token = nextToken(scn);
			// INTERSECT
			} else if (token.equals("INTERSECT")) {
				fullSubSelectLinkedTo.setTypeLink(EnumPrecompilerReservedWords.SQL_INTERSECT);
				token = nextToken(scn);
			}
				
			// DISTINCT
			if (token.equals("DISTINCT")) {
				fullSubSelectLinkedTo.setDistinct(true);
				token = nextToken(scn);
			} else if (token.equals("ALL")) {
				fullSubSelectLinkedTo.setAll(true);
				token = nextToken(scn);
			}
				
			// (full-select)
			if (token.equals("(")) {
				strSubQuery = extractStringBetweenPar(scn);								// Scanner posizionato su parentesi )
				// Analisi fullselect
				subQuery = analyzeSqlFullselect(ictx, instruction, strSubQuery);
				fullSubSelectLinkedTo.setFullSelect(subQuery);
				fullSubSelectLinkedTo.setLinkedToFullselect(true);
				fullselect.getLinkedSubFullSelects().add(fullSubSelectLinkedTo);		// Append Union linked to
				
				// Next UNION|EXCEPT|INTERSECT 
				token = nextToken(scn);		
				continue;
			}
			 
			// Subselect
			if (token.equals("SELECT")) {
				sbSubselect = new StringBuffer();;
				
				// Scan tokens fino a prossima union o fine stringa
				while (!token.equals("")  && !token.equals("UNION") && !token.equals("EXCEPT") && !token.equals("INTERSECT")) {
					sbSubselect.append( " " + token);
					token = nextToken(scn);
				} // end-while parms fra parentesi
				
				// Analisi subselect
				subselect = analyzeSqlSubselectSelectInto(instruction, sbSubselect.toString());
				fullSubSelectLinkedTo.setSubselect(subselect);
				fullSubSelectLinkedTo.setLinkedToSubselect(true);
				fullselect.getLinkedSubFullSelects().add(fullSubSelectLinkedTo);		// Append Union linked to
				
				// Next UNION|EXCEPT|INTERSECT 
				continue;
			}
			 
		} // end-while generale occorrenze linked to
		 
	}


	/*
     * --------------------------------------------------
     * Analisi opzioni di order by e fetch di fullselect
     * --------------------------------------------------
     * 
     * Si tratta delle opzioni, opzionali, seguenti:
     * 
     * |ORDER BY ...
     * |FETCH FITST |n |ROW|ROWS ONLY
     * 
     */
	private void analyzeSqlFullselectOptions(InnerContextAnalysis ictx, InstructionSql instruction, SqlFullSelect fullselect, String strFetchOrOrderBy) throws SQLException, ExceptionAmrita {
		
		String strOrderBy = "";
		String strFetchFirst = "";
        int iOrderBy = 0;
        int iFetchFirst = 0;
        
        // Estrazione sezioni da analizzare
        iOrderBy = strFetchOrOrderBy.indexOf("ORDER ");
        iFetchFirst = strFetchOrOrderBy.indexOf("FETCH ");
        
        // Presenti tutte le opzioni
        if (iOrderBy >= 0 && iFetchFirst >= 0) {
           	strOrderBy = strFetchOrOrderBy.substring(0, iFetchFirst).trim();
           	strFetchFirst = strFetchOrOrderBy.substring(iFetchFirst).trim();
		}
        // Presente solo order by
        if (iOrderBy >= 0 && iFetchFirst < 0) {
           	strOrderBy = strFetchOrOrderBy.trim();
 		}
        // Presente solo fetch first
        if (iOrderBy < 0 && iFetchFirst >= 0) {
        	strFetchFirst = strFetchOrOrderBy.trim();
 		}
        
        
        // Analisi order by
        if (!strOrderBy.equals("")) {
        	analyzeSqlFullselectOptionsOrderBy(ictx, instruction, fullselect, strOrderBy);
		}
        
        // Analisi fetch first
        if (!strFetchFirst.equals("")) {
        	analyzeSqlFullselectOptionsFetchFirst(ictx, instruction, fullselect, strFetchFirst);
		}      
	}

	
	/*
     * --------------------------------------------------
     * Analisi opzione di order by di fullselect
     * --------------------------------------------------
     * 
     * Si tratta delle opzioni, opzionali, seguenti:
     * 
     * |ORDER BY ...
     * 
	 */
	private void analyzeSqlFullselectOptionsOrderBy(InnerContextAnalysis ictx, InstructionSql instruction, SqlFullSelect fullselect, String strOrderBy) throws SQLException, ExceptionAmrita {

		Scanner scnOptions = null;
		SqlOrderBy orderByElement = null;
		SqlExpression sortKeyExpression = null;
		String token = "";
		String strSortKeyExpression = "";
	    int columnNumber = 0;
        int cntParOpen = 0;
        
		fullselect.setOrderBy(true);
		orderByElement = new SqlOrderBy ();

		scnOptions = new Scanner(strOrderBy);	
		token = nextToken(scnOptions);					// ORDER 
		token = nextToken(scnOptions);					// BY 
		token = nextToken(scnOptions);					// INPUT|ORDER|col-name|col-num|expr

		// ORDER BY INPUT SEQUENCE
		if (token.equals("INPUT")) {
			token = nextToken(scnOptions);				// SEQUENCE
			orderByElement.setOrderByInputSequence(true);
			fullselect.getOrderByElements().add(orderByElement);
			return;
		}
		
		// ORDER BY ORDER OF table-designator
		if (token.equals("ORDER")) {
			orderByElement.setOrderByOfTableDesignator(true);
			token = nextToken(scnOptions);				// OF
			token = nextToken(scnOptions);
			orderByElement.setTableDesignator(token);
			fullselect.getOrderByElements().add(orderByElement);
			return;
		}

		// Elenco di colonne, numeri di colonna o espressioni
		
		// Scan tokens
		while (!token.equals("")) {
			
			// Fine elemento precedente
			if (token.equals(",")) {
				fullselect.getOrderByElements().add(orderByElement);
				orderByElement = new SqlOrderBy ();
				token = nextToken(scnOptions);
				continue;
			}
					
			// Numero colonna
			if (StringService._isNumericInt(token)) {
				columnNumber = StringService._getNumericInt(token);
				orderByElement.setColumnNumber(columnNumber);
				token = nextToken(scnOptions);
				continue;
			}
			
			// ASC
			if (token.equals("ASC")) {
				orderByElement.setOrderAscending(true);
				token = nextToken(scnOptions);
				continue;
			}
			
			// DESC
			if (token.equals("DESC")) {
				orderByElement.setOrderDescending(true);
				token = nextToken(scnOptions);
				continue;
			}
			
            // Nome colonna dichiarata in select col1, col, col2, .. di qualche subselect della fullselect
			if (isSqlColumnNameInFullSelect(fullselect, token)) {
				orderByElement.setOrderByColumnName(true);
				orderByElement.setColumnName(token);
				token = nextToken(scnOptions);
				continue;
			}
			
			// Può essere solo una sort-key-expression
			sortKeyExpression = new SqlExpression ();
			
			// Scan tokens sort-key-expression
			while (!token.equals("")  && !token.equals(",") && !token.equals("ASC")  && !token.equals("DESC")) {
				strSortKeyExpression = strSortKeyExpression + " " + token;
				// sub-expression o parms di una funzione
				if (token.equals("(")) {
					cntParOpen = 1;
					token = nextToken(scnOptions);
					// Scan tokens  parms fra parentesi
					while (!token.equals("")  && cntParOpen > 0) {
						strSortKeyExpression = strSortKeyExpression + " " + token;
						token = nextToken(scnOptions);
						if (token.equals("(")) {cntParOpen++;}
						if (token.equals(")")) {cntParOpen--;}
					} // end-while parms fra parentesi
					continue;
				}
				token = nextToken(scnOptions);
			} // end-while tokens sort-key-expression
			
			sortKeyExpression = analyzeSqlExpression(ictx, instruction, strSortKeyExpression);
			orderByElement.setSortKeyExpression(sortKeyExpression);
			
		} // end-while tokens  
		 
		// Accodamento elemento pendente
		fullselect.getOrderByElements().add(orderByElement);

	}


	/*
     * --------------------------------------------------
     * Analisi opzione fetch first di fullselect
     * --------------------------------------------------
     * 
     * Si tratta di:
     * 
     * FETCH FIRST n ROW|ROWS ONLY
     * 
	 */
	private void analyzeSqlFullselectOptionsFetchFirst(InnerContextAnalysis ictx, InstructionSql instruction, SqlFullSelect fullselect, String strFetchFirst) {
		
		Scanner scnOptions = null;
		String token = "";
        
		fullselect.setFetchFirstNRows(true);
	
		scnOptions = new Scanner(strFetchFirst);
		token = nextToken(scnOptions);					// FETCH 
		token = nextToken(scnOptions);					// FIRST 
		token = nextToken(scnOptions);					// integer|ROW|ROWS
		
		// presente il numero di righe
		if (StringService._isNumericInt(token)) {
			fullselect.setFetchFirstRowsNumber(StringService._getNumericInt(token));
			token = nextToken(scnOptions);				// ROW|ROWS
		}
		token = nextToken(scnOptions);					// ONLY
		
	}


	/* ----------------------------------------------------------------------------
	 * Restituisce true se la colonna è elencata in qualche select della fullselect
	 * ----------------------------------------------------------------------------
	 * 
	 * Le colonne elencate possono essere qualificate dal correlation name della tabella
	 * 
	 * SELECT A.COL1 A.COL2 FROM OWNER.TAB1 AS A
	 *     ORDER BY A.COL1 COL2
	 *     
	 * Il nome di colonna fornito può essere qualificato, e il match è quindi completo
	 * oppure non qualificato dove univoco. 
	 * 
	 */
	private boolean isSqlColumnNameInFullSelect(SqlFullSelect fullselect, String name) {
		
		ArrayList<SqlSubselectSelectInto> al_subselect = null;
		
		// Scan tutte le subselect definite dalla full-select
		al_subselect = fullselect.getAllSubselect();
		for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
			if (isSqlColumnName(sqlSubselect, name)) {
				return true;
			}
		}
		
		return false;
	}

	/* -----------------------
	 * Analisi expression sql
	 * -----------------------
	 * 
	 * Viene analizzata una espressione sql con restituzione di un oggetto SqlExpression.
	 * 
	 * Una espressione è costituita da una espressione matematica di operandi concatenati,
	 * sommati, sottratti, divisi e moltiplicati.
	 * 
	 * L'analisi è completamente indipendente dal resto dell'istruzione.
	 * 
	 * Ogni operando può essere:
	 * 
	 * 01) function-invocation
	 * 02) (expression)
	 * 03) constant
	 * 04) column-name
	 * 05) variabile
	 * 06) special-register
	 * 07) (scalar-fullselect)
	 * 08) time-zone-specific-expression
	 * 09) labeled-duration
	 * 10) case-expression
	 * 11) cast-specification
	 * 12) XMLCAST-specification
	 * 13) OLAP specification
	 * 14) row-change-expression
	 * 15) sequence-reference
	 * 
	 */
	private SqlExpression analyzeSqlExpression(InnerContextAnalysis ictx, InstructionSql instruction, String strExpression) throws SQLException, ExceptionAmrita  {

		Scanner scn = null;
		SqlExpression sqlExpression = null;
		SqlExpression sqlExpressionRecursive = null;
		SqlExpressionElement sqlExpressionElement = null;
		SqlFullSelect sqlFullSelect = null;
		EnumPrecompilerReservedWords functionInvoked = null;
		InnerInstructionWordsEntry wordKey = null;
		List<String> al_functionParm = null;
		StringBuffer sbCaseExpression = null;
		String[] ar_functionParm = null;
		String token = "";;
		String strBetweenPar = "";
		String strOlapSpecification = "";
		String strOlapFunctionAggregateName = "";
		String strOlapFunctionAggregateBody = "";
		String svNoMemoryToParse = "";
        int numTokensSpecialRegister = 0;
        int cntCase = 0;
		boolean isDurationAndTimeZone = false;
        
		sqlExpression = new SqlExpression ();
      
		scn = new Scanner(strExpression);
		this.strNoMemoryToParse = strExpression;
		token = nextToken(scn);					 

		// scan generale operandi espressione
		while (!token.equals("")) {
			
			///////////////////////////////////////////////////
			// Operatore +,-,*,/,||,CONCAT
			///////////////////////////////////////////////////
			
			if (token.equals("+")) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.ADD);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyAdd(true);
				token = nextToken(scn);
				continue;
			}
			if (token.equals("-")) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.SUB);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnySubtract(true);
				token = nextToken(scn);
				continue;
			}
			if (token.equals("/")) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.DIV);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyDivide(true);
				token = nextToken(scn);
				continue;
			}
			if (token.equals("*")) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.MULT);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyMultiply(true);
				token = nextToken(scn);
				continue;
			}
			if (token.equals("||") || token.equals("CONCAT")) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.CONCAT);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyConcat(true);
				token = nextToken(scn);
				continue;
			}

			///////////////////////////////////////////////////
			// Costante alfanumerica o numerica
			///////////////////////////////////////////////////
			
			if (StringService._isLiteral(token)) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.CONSTANT_ALPHANUMERIC);
				sqlExpressionElement.setConstant(token);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyConstant(true);
				token = nextToken(scn);				// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
				isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
				if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
				if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
					sqlExpression.setAsNewColumnName(token);
					break;
				}
     			continue;
			}
			if (StringService._isNumeric(token)) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.CONSTANT_NUMERIC);
				sqlExpressionElement.setConstant(token);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyConstant(true);
				token = nextToken(scn);				// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
				isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
				if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
				if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
					sqlExpression.setAsNewColumnName(token);
					break;
				}
     			continue;
			}

			///////////////////////////////////////////////////
			// Variabile host
			///////////////////////////////////////////////////
			
			if (token.startsWith(":")) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.HOST_VAR);
				sqlExpressionElement.setHostVar(token);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyHostVar(true);
				token = nextToken(scn);				// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
				isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
				if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
				if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
					sqlExpression.setAsNewColumnName(token);
					break;
				}
     			continue;
			}

			///////////////////////////////////////////////////////
			// Special-register, possono essere necessari + token
			///////////////////////////////////////////////////////
			
			wordKey = getSqlSpecialRegisterKeyMap(token, this.strNoMemoryToParse);
			if (wordKey != null) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.SPECIAL_REGISTER);
				sqlExpressionElement.setSpecialRegister(wordKey.en_WordReservedOwner);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnySpecialRegister(true);
                numTokensSpecialRegister = wordKey.al_wordKey.size();
                for (int i = 1; i < numTokensSpecialRegister; i++) {token = nextToken(scn);}
				token = nextToken(scn);										// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
				isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
				if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
				if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
					sqlExpression.setAsNewColumnName(token);
					break;
				}
     			continue;
			}
			
			///////////////////////////////////////////////////////
			// Figurative constants
			///////////////////////////////////////////////////////
			
			if (token.equals("DEFAULT") ) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.DEFAULT);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyDefault(true);
				token = nextToken(scn);
     			continue;
			}
			if (token.equals("NULL") ) {
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.NULL);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyNull(true);
				token = nextToken(scn);
     			continue;
			}
			
			
			///////////////////////////////////////////////////////
			// Function invocation no CAST no XMLCAST no OLAP
			///////////////////////////////////////////////////////
			
			if (isSqlFunction(token) && nextTokenWithoutScannerMod(scn).equals("(") && !token.equals("CAST") && !token.equals("XMLCAST") && !isSqlOlapSpecification(scn, token)) {
				sqlExpressionElement = new SqlExpressionElement();
				functionInvoked = getSqlFunction(token);
				token = nextToken(scn);				// (
				// Deve essere una parentesi aperta
				if (!token.equals("(")) { 
					instruction.setParsingError(true);
					instruction.setTokenInError(token);
					instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", functionInvoked.toString(), null, this.strProgramScript, this.strProgramScriptObj);
				    return sqlExpression;
				}
				strBetweenPar = extractStringBetweenPar(scn);
				ar_functionParm = splitParms(strBetweenPar);
				al_functionParm = Arrays.asList(ar_functionParm);
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.FUNCTION_INVOCATION);
				sqlExpressionElement.setFunctionNameCoded(functionInvoked);
				sqlExpressionElement.setFunctionParms(al_functionParm);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyFunctionInvocation(true);
				token = nextToken(scn);				// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
				isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
				if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
				if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
					sqlExpression.setAsNewColumnName(token);
					break;
				}
     			continue;
			}

			///////////////////////////////////////////////////////
			// (expression) o (full-select)  
			///////////////////////////////////////////////////////
			
			if (token.equals("(") ) {
				sqlExpressionElement = new SqlExpressionElement();
				strBetweenPar = extractStringBetweenPar(scn);
				// (expression)
				if (!strBetweenPar.startsWith("SELECT ")) {
					svNoMemoryToParse = this.strNoMemoryToParse;
					sqlExpressionRecursive = analyzeSqlExpression(ictx, instruction, strBetweenPar);
	                if (instruction.isParsingError()) {
						return sqlExpressionRecursive;
					}
	                this.strNoMemoryToParse = svNoMemoryToParse;
	                sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.EXPRESSION);
	                sqlExpressionElement.setExpression(sqlExpressionRecursive);
	                sqlExpression.getElements().add(sqlExpressionElement);
	                sqlExpression.setThereAnyExpression(true);
					token = nextToken(scn);				// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
					isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
					if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
					if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
						sqlExpression.setAsNewColumnName(token);
						break;
					}
	     			continue;
				}
				// (scalar-full-select) o full-select
				sqlFullSelect = analyzeSqlFullselect(ictx, instruction, strBetweenPar);
                if (instruction.isParsingError()) {
					return sqlExpressionRecursive;
				}
                sqlExpressionElement.setFullSelect(sqlFullSelect);
                sqlExpression.getElements().add(sqlExpressionElement);
                sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.SCALAR_FULL_SELECT);
                sqlExpression.setThereAnyScalarFullSelect(true);
				token = nextToken(scn);				// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
				isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
				if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
				if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
					sqlExpression.setAsNewColumnName(token);
					break;
				}
     			continue;
			}
			
			///////////////////////////////////////////////////////
			// Cast-specification
			///////////////////////////////////////////////////////
			
			if (token.equals("CAST")) {
				token = nextToken(scn);				// (
				// Deve essere una parentesi aperta
				if (!token.equals("(")) {
					instruction.setParsingError(true);
					instruction.setTokenInError(token);
					instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
				    return sqlExpression;
				}
				strBetweenPar = extractStringBetweenPar(scn);
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.CAST_SPECIFICATION);
				sqlExpressionElement.setCastSpecification(strBetweenPar);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyCastSpecification(true);
				token = nextToken(scn);				// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
				isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
				if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
				if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
					sqlExpression.setAsNewColumnName(token);
					break;
				}
     			continue;
			}
	
			///////////////////////////////////////////////////////
			// XMLCAST-specification
			///////////////////////////////////////////////////////
			
			if (token.equals("XMLCAST")) {
				token = nextToken(scn);				// (
				// Deve essere una parentesi aperta
				if (!token.equals("(")) {
					instruction.setParsingError(true);
					instruction.setTokenInError(token);
					instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
				    return sqlExpression;
				}
				strBetweenPar = extractStringBetweenPar(scn);
				sqlExpressionElement = new SqlExpressionElement();
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.XML_CAST_SPECIFICATION);
				sqlExpressionElement.setCastSpecification(strBetweenPar);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyXMLCastSpecification(true);
				token = nextToken(scn);				// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
				isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
				if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
				if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
					sqlExpression.setAsNewColumnName(token);
					break;
				}
     			continue;
			}
			
			///////////////////////////////////////////////////////
			// ROW CHANGE
			///////////////////////////////////////////////////////
			
			if (token.equals("ROW")) {
				sqlExpressionElement = new SqlExpressionElement();
				token = nextToken(scn);					// CHANGE
				token = nextToken(scn);					// TIMESTAMP|TOKEN
				if (token.equals("TIMESTAMP")) {
					sqlExpressionElement.setRowChangeTimestamp(true);
				} else {
					sqlExpressionElement.setRowChangeToken(true);
				}
				token = nextToken(scn);					// FOR
				token = nextToken(scn);					// table-designator
				sqlExpressionElement.setRowChangeTableDesignator(token);
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.ROW_RANGE_EXPRESSION);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyRowChangeExpression(true);
	            token = nextToken(scn);
				if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
					sqlExpression.setAsNewColumnName(token);
					break;
				}
	            continue;
			}
				
			///////////////////////////////////////////////////////
			// sequence-reference
			///////////////////////////////////////////////////////
			
			if (token.equals("NEXT")) {
				sqlExpressionElement = new SqlExpressionElement();
				token = nextToken(scn);					// VALUE
				token = nextToken(scn);					// FOR
				token = nextToken(scn);					// sequence-name
				sqlExpressionElement.setSequenceReferenceName(token);
				sqlExpressionElement.setSequenceReferenceNext(true);
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.SEQUENCE_REFERENCE);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnySequenceReference(true);
	            token = nextToken(scn);
	            continue;
			}
			if (token.equals("PREVIOUS")) {
				sqlExpressionElement = new SqlExpressionElement();
				token = nextToken(scn);					// VALUE
				token = nextToken(scn);					// FOR
				token = nextToken(scn);					// sequence-name
				sqlExpressionElement.setSequenceReferenceName(token);
				sqlExpressionElement.setSequenceReferencePrevious(true);
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.SEQUENCE_REFERENCE);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnySequenceReference(true);
	            token = nextToken(scn);
	            continue;
			}	
			
			///////////////////////////////////////////////////////
			// CASE expressions
			///////////////////////////////////////////////////////
			
			if (token.equals("CASE")) {
				sqlExpressionElement = new SqlExpressionElement();
				sbCaseExpression = new StringBuffer();
	     		// Estrazione fino a END inclusa
				sbCaseExpression.append(token);
				cntCase = 1;
				while (!token.equals("") && !token.equals("END") && cntCase >= 1) {
					token = nextToken(scn);
					if (token.equals("CASE")) {cntCase++;}
					if (token.equals("END"))  {cntCase--;}
					sbCaseExpression.append(" " + token);
				}
				sqlExpressionElement.setCaseExpression(sbCaseExpression.toString());
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.CASE_EXPRESSION);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyCaseExpression(true);
				token = nextToken(scn);				// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
				isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
				if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
				if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
					sqlExpression.setAsNewColumnName(token);
					break;
				}
     			continue;
			}
			
			///////////////////////////////////////////////////////
			// OLAP specification
			///////////////////////////////////////////////////////
			
			// Inizio operando OLAP
			if (isSqlOlapSpecification(scn, token)) {
				sqlExpressionElement = new SqlExpressionElement();
				// Function aggregate
				if (isSqlFunctionAggregate(token)) {
					strOlapFunctionAggregateName = token;
					token = nextToken(scn);					// (
					strOlapFunctionAggregateBody = extractStringBetweenPar(scn);
					sqlExpressionElement.setOLAPAggregateFunctionName(strOlapFunctionAggregateName);
					sqlExpressionElement.setOLAPAggregateFunctionBody(strOlapFunctionAggregateBody);
				} else {
					sqlExpressionElement.setOLAPType(token);
					token = nextToken(scn);						// (
					token = nextToken(scn);						// )
				}
				token = nextToken(scn);							// OVER
				// Deve essere OVER
				if (!token.equals("OVER")) {
					instruction.setParsingError(true);
					instruction.setTokenInError(token);
					instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
				    return sqlExpression;
				}
				token = nextToken(scn);						// (
				strBetweenPar = extractStringBetweenPar(scn);
				strOlapSpecification = strBetweenPar;
				sqlExpressionElement.setOLAPSpecification(strOlapSpecification);
				sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.OLAP_SPECIFICATION);
				sqlExpression.getElements().add(sqlExpressionElement);
				sqlExpression.setThereAnyOlapSpecification(true);
	            token = nextToken(scn);
	            continue;
			}

			////////////////////////////////////////////////////////
			// Colonna (a questo punto può solo essere una colonna)
			////////////////////////////////////////////////////////
			
			sqlExpressionElement = new SqlExpressionElement();
			sqlExpressionElement.setColumnNameQualified(token);
			sqlExpressionElement.setTypeElement(EnumSqlExpressionElementType.COLUMN_NAME);
			sqlExpression.getElements().add(sqlExpressionElement);
			sqlExpression.setThereAnyColumnName(true);
			token = nextToken(scn);				// YEAR|YEARS|....|AT LOCAL|TIME ZONE ?
			isDurationAndTimeZone = analyzeSqlExpressionDurationsAndTimeZone(scn, sqlExpressionElement, token);
			if (isDurationAndTimeZone) {token = nextToken(scn);}		// column-name ?
			if (!token.equals("") && !isThereAnyFurtherToken(scn)) {
				sqlExpression.setAsNewColumnName(token);
				break;
			}
 			continue;
		}
				
		return sqlExpression;
	}

	/* -----------------------------------------------------------
	 * Verifica se sono presenti altri token dopo quello corrente
	 * -----------------------------------------------------------
	 * 
	 * Senza alterare lo scanner corrente fornito, restituisce true
	 * se sono presenti altri token nello scanner.
	 * 
	 */
	private boolean isThereAnyFurtherToken(Scanner scn) {
		
		Scanner scnNext = null;
		String strNext = "";
		String tokenNext = "";
		
		// Verifica se token corrente ultimo token: significa nome colonna a fine espressione
		strNext = this.strNoMemoryToParse;
		scnNext = new Scanner(strNext);
		tokenNext = nextTokenNoMemory(scnNext);
		if (!tokenNext.equals("")) {
			return true;
		}
		
		// Lo scanner fornito è posizionato sull'ultimo token
		
		return false;
	}


	/* ----------------------------------------------------------------
	 * Restituisce true se l'indice è posizionato dentro una parentesi
	 * ----------------------------------------------------------------
	 * 
	 * Si verifica se la posizione è dentro una parentesi
	 * 
	 */
	private boolean isInsidePar(String str, int index) {
		
		int cntPar = 0;
		
		for (int i = index - 1; i >= 0; i--) {
			if (str.charAt(i) == '(') {cntPar++;}
			if (str.charAt(i) == ')') {cntPar--;}
		}
		
		// Dentro parentesi
		if (cntPar != 0) {
			return true;
		}
		
		// NON è dentro parentesi
		
		return false;
	}

	/* ------------------------------------------------------------------------
	 * Elimina le parentesi wrapper ininfluenti (((...)))
	 * ------------------------------------------------------------------------
	 * 
	 * La stringa fornita inizia con una parentesi aperta
	 * Vengono eliminate solo le parentesi wrapper superflue, oltre la prima
	 * Dopo la parentesi di chiusura NON deve esserci un'altra parentesi aperta
	 * L'operazione inizia solo se la stringa inizia co (
	 * 
	 * 1) ( ... ) ...  non viene modificato
	 * 2) ( (...) ) diventa ( ... )
	 * 3) ( ( ( A + B) + ( C + D ) ) ) diventa ( ( A + B) + ( C + D ) )
	 * 
	 * 
	 */
	private String normalizeBalancedPar(String str) {
		
		Scanner scn = null;
		String strPrec = "";
		String strNext = "";
		String strBetweenPar = "";
		String token = "";
		
		scn = new Scanner(str);
		this.strNoMemoryToParse = str;
		
		token = nextToken(scn);
		
	    // Nessuna operazione
		if (!token.equals("(")) {
			return str;
		}

		strPrec = str;

		// Ciclo generale di normalizzazione
		while (token.equals("(")) {
			
			strBetweenPar = extractStringBetweenPar(scn);
			strNext = this.strNoMemoryToParse.trim();
			
			// Situazione tipo: ( (A + B) + (C + D) )
			// strBetweenPar  : A + B
			// strNext        : + (C + D)
			// strPrec        : ( (A + B) + (C + D) )
			if (strNext.indexOf("(") >= 0) {
				return strPrec;
			}

			// Situazione tipo: ( ( (A + B) ) )
			// strBetweenPar  : (A + B)
			// strNext        : ")"
			if (strNext.startsWith(")")) {
				scn = new Scanner(strBetweenPar);
				this.strNoMemoryToParse = strBetweenPar;
				strPrec = strBetweenPar;
				token = nextToken(scn);
				continue;
			}

			// Situazione tipo: (A + B) 
			// strBetweenPar  :  A + B
			// strNext        : ""
			if (strNext.equals("")) {
				return strBetweenPar;
			}

			
			// Situazione tipo: ( A + B) <= D  
			// strBetweenPar  : A + B
			// strNext        : <= D
			// strPrec        : ( A + B) <= D  
			if (!strNext.equals("")) {
				return strPrec;
			}

		}
		
		return strPrec;
	}



	
	/* ------------------------------------------------------------------------
	 * Restituisce true se la stringa non contiene parentesi o sono bilanciate
	 * ------------------------------------------------------------------------
	 * 
	 */
	private boolean isBalancedPar(String str) {
		
		int cntPar = 0;
		
		for (int i = 0; i < str.length(); i++) {
			if (str.charAt(i) == '(') {cntPar++;}
			if (str.charAt(i) == ')') {cntPar--;}
		}
		
		// Parentesi bilanciate
		if (cntPar == 0) {
			return true;
		}
		
		// Parentesi aperte non chiuse
		
		return false;
	}


	/* -----------------------------------------------------------------------------------------------
	 * Verifica se lo scanner è posizionato sulla clausola di labeled-duration o time-zone specific
	 * ------------------------------------------------------------------------------------------------
	 * 
	 * Se labeled duration o time zone aggiorna l'elemento dell'espressione
	 * Lo scanner non subisce avanzamenti
	 * 
	 */
    private boolean analyzeSqlExpressionDurationsAndTimeZone(Scanner scnExpression, SqlExpressionElement sqlExpressionElement, String token) {

    	EnumPrecompilerReservedWords typeLabeledDuration = null;
        
    	// Nessuna operazione da fare
    	if (token.equals("")) {
			return false;
		}
    	
    	// Labeled_duration
    	if (isLabeledDuration(token)) {
			typeLabeledDuration = getLabeledDuration(token);
			sqlExpressionElement.setLabeledDuration(true);
			sqlExpressionElement.setTypeLabeledDuration(typeLabeledDuration);
			return true;
		}
		
    	// Time zone specific expression
    	if (token.equals("AT")) {
    		token = nextToken(scnExpression);				// LOCAL|TIME|TIMEZONE
    		if (token.equals("TIME")) {
    			token = nextToken(scnExpression);			// ZONE
    			token = nextToken(scnExpression);			// '...'
			}
     		sqlExpressionElement.setLabeledDuration(true);
    		return true;
		}
    	
    	return false;
	}


	/* -----------------------------------------------------------------------------------
     * Restituisce un array di stringhe con i parametri estratti dalla stringa di input
     * -----------------------------------------------------------------------------------
     * 
     * I parametri sono separati da virgole.
     * Stringhe fra parentesi aperte e chiuse vengono bypassate
     * La stringa è già suddivisa in token elementari
     * Ogni parametro può contenere al suo interno espressioni fra parentesi con delle virgole
     * 
     */
	private String[] splitParms(String strToSplit) {
		
		Scanner scn = null;
		String token = "";
		String strBetweenPar = "";
		StringBuffer sbParm = null;
		String ar_parm[] = null;
		ArrayList<String> al_parm = null;
		
		// Strutture di lavoro
		al_parm = new ArrayList<String> ();
		sbParm = new StringBuffer ();
		
		scn = new Scanner(strToSplit);
		token = nextTokenNoMemory(scn);
		
		// Scan generale
		while (!token.equals("")) {
			
			// Fine parametro precedente
			if (token.equals(",")) {
				al_parm.add(sbParm.toString().trim());
				sbParm = new StringBuffer ();
				token = nextTokenNoMemory(scn);
				continue;
			}
			
			// Parentesi aperta: inglobo con tutto il contenuto
			if (token.equals("(")) {
				strBetweenPar = extractStringBetweenPar(scn);
				sbParm.append(" ( " + strBetweenPar + " ) ");
				token = nextTokenNoMemory(scn);
				continue;
			}
			
			// Accodamento token e next
			sbParm.append(" " + token);
			token = nextTokenNoMemory(scn);
		}
		
		// Ultimo parametro pendente
		al_parm.add(sbParm.toString().trim());
		
		// Conversione in array di stringhe
		ar_parm = new String[al_parm.size()];
		ar_parm = al_parm.toArray(ar_parm);
		
		return ar_parm;
	}


	/* ------------------------------
	 * Analisi search conditions sql
	 * ------------------------------
	 * 
	 * Viene analizzata una search conditions sql di una clausola WHERE, HAVING etc.
	 * Viene fornita la search conditione epurata dalla WHERE, HAVING etc. iniziale
	 * Viene effettuta una analisi ricorsiva delle search conditions
	 * 
	 */
	private SqlSearchConditions analyzeSqlSearchConditions(InnerContextAnalysis ictx, InstructionSql instruction, String strSearchConditions) throws SQLException, ExceptionAmrita  {
		
		SqlSearchConditions sqlSearchConditions = null;
		SqlSearchConditions sqlSearchConditionRecursive = null;
		SqlSearchConditions sqlSearchConditionOccurs = null;
		SqlPredicate sqlPredicate = null;
		Scanner scn = null;
		ArrayList<String> al_searchConditionOccursed = null;
		String token = "";
		String strSearchCondition = "";
		String strPredicate = "";
		String strSearchConditionsOccursed = "";
		String strPredicateNormalized = "";					// Senza NOT iniziale
		int iAndOrConditionOccursed = 0;
		int iNext = 0;
		
 		// Allocazione strutture di lavoro
		sqlSearchConditions = new SqlSearchConditions ();
		al_searchConditionOccursed = new ArrayList<String> ();
        
		
 		////////////////////////////////////////////////////////////////////
		// Parser
		////////////////////////////////////////////////////////////////////
		
        this.strNoMemoryToParse = strSearchConditions;					// Initial 
		scn = new Scanner(strSearchConditions);							//  
		strPredicateNormalized = strSearchConditions;
		
		token = nextToken(scn);											// |NOT
 		
		
		/////////////////////////////////////////////////////////////////////
		// Analisi parte iniziale fissa di search-conditions
		/////////////////////////////////////////////////////////////////////
		
		// NOT predicate|(search-condition)
		if (token.equals("NOT")) {
			strPredicateNormalized = this.strNoMemoryToParse.trim();
			sqlSearchConditions.setStartingOperator(EnumSqlOperator.NOT);
			token = nextToken(scn);										// predicate|(search-condition)
		}
		
		// Estrazione eventuale gruppo ripetitivo DOPO primo predicato/search-condition
		iAndOrConditionOccursed = indexOfFirstTokenNotInParOfBetween(strPredicateNormalized, 0, " AND ", " OR ");
		if (iAndOrConditionOccursed > 0) {
			strSearchConditionsOccursed = strPredicateNormalized.substring(iAndOrConditionOccursed).trim();
		} else {
			strSearchConditionsOccursed = "";
		}
		
		// (search-condition): attivazione analisi ricorsiva
		if (strPredicateNormalized.startsWith("(") 
		&& !isPredicate(ictx, strPredicateNormalized ) ) {
			strSearchCondition = extractStringBetweenPar(scn);
			sqlSearchConditionRecursive = analyzeSqlSearchConditions(ictx, instruction, strSearchCondition);
			if (instruction.isParsingError()) {return sqlSearchConditions;}
			sqlSearchConditions.setSearchCondition(sqlSearchConditionRecursive);
			sqlSearchConditions.setSearchCondition(sqlSearchConditionRecursive);
			sqlSearchConditions.setSearchCondition(true);
			strSearchConditionsOccursed = this.strNoMemoryToParse.trim();;
			
		// Predicate, anche del tipo (row-value-expressions) oprt .... (row-value-expressions) o (expr) AND|OR ...
		} else {
			if (iAndOrConditionOccursed > 0) {
				strPredicate = strPredicateNormalized.substring(0, iAndOrConditionOccursed).trim();
			} else {
				strPredicate = strPredicateNormalized.trim();
			}
			sqlPredicate = analyzeSqlSearchConditionsPredicate(ictx, instruction, strPredicate);
			if (instruction.isParsingError()) {return sqlSearchConditions;}
			sqlSearchConditions.setPredicate(true);
			sqlSearchConditions.setPredicate(sqlPredicate);
		}
	
		
		// Nessuna ulteriore condizione ripetitiva in AND o OR 
		if (strSearchConditionsOccursed.equals("")) {
			return sqlSearchConditions;
		}
		scn = new Scanner(strSearchConditionsOccursed);	
		token = nextToken(scn);
		
		
		/////////////////////////////////////////////////////////////////////
		// Estrazione search conditions ripetitive (iniziano per AND o OR)
		/////////////////////////////////////////////////////////////////////
		
        iNext = indexOfFirstTokenNotInParOfBetween(strSearchConditionsOccursed, token.length(), " AND ", " OR ");
        
        // Singola search condition
		if (iNext < 0) {
			iNext = strSearchConditionsOccursed.length();
		    strSearchCondition = strSearchConditionsOccursed;
		    al_searchConditionOccursed.add(strSearchCondition.trim());
		} else {
			// Multipla search conditions
			while (!strSearchConditionsOccursed.trim().equals("")) {
				
			   iNext = indexOfFirstTokenNotInParOfBetween(strSearchConditionsOccursed,  5, " AND ", " OR ");
			   if (iNext < 0) {iNext = strSearchConditionsOccursed.length();}   // Ultima search condition
			   
			   strSearchCondition = strSearchConditionsOccursed.substring(0, iNext);
			   al_searchConditionOccursed.add(strSearchCondition.trim());
			   strSearchConditionsOccursed = strSearchConditionsOccursed.substring(iNext).trim();
			}
		}

		
		/////////////////////////////////////////////////////////////////////
		// Analisi search conditions estratte
		/////////////////////////////////////////////////////////////////////
		
		// Scan condizioni di search ripetitive
	    for (String strSearchConditionOccursed : al_searchConditionOccursed) {
			sqlSearchConditionOccurs = new SqlSearchConditions ();

			scn = new Scanner(strSearchConditionOccursed);
			this.strNoMemoryToParse = strSearchConditionOccursed;
			token = nextToken(scn);
			strPredicateNormalized = this.strNoMemoryToParse.trim();

			if (token.equals("AND")) {
				sqlSearchConditionOccurs.setStartingOperator(EnumSqlOperator.AND);
			}
			if (token.equals("OR")) {
				sqlSearchConditionOccurs.setStartingOperator(EnumSqlOperator.OR);
			}
			token = nextToken(scn);						// |NOT|predicate|(search-condition)
			if (token.equals("NOT")) {
				strPredicateNormalized = this.strNoMemoryToParse.trim();
				sqlSearchConditionOccurs.setNotAfterStartingOperator(true);
				token = nextToken(scn);						// predicate|(search-condition)
			}
			
			// (search-condition): attivazione analisi ricorsiva
			if (strPredicateNormalized.startsWith("(") 
 			&& !isPredicateRowValueExpression(ictx, strPredicateNormalized)
 			&& !isExpressionBetweenPar(ictx, strPredicateNormalized)) {
				strSearchConditionOccursed = extractStringBetweenPar(scn);
				sqlSearchConditionOccurs = analyzeSqlSearchConditions(ictx, instruction, strSearchConditionOccursed);
				if (instruction.isParsingError()) {return sqlSearchConditions;}
				sqlSearchConditions.getSearchConditions().add(sqlSearchConditionOccurs);		// Accodamento condizione ripetitiva
			
			// Predicate, anche del tipo (row-value-expressions) oprt .... (row-value-expressions) o (expr) AND|OR ...
			} else {
				sqlPredicate = analyzeSqlSearchConditionsPredicate(ictx, instruction, strPredicateNormalized);
				if (instruction.isParsingError()) {return sqlSearchConditions;}
				sqlSearchConditionOccurs.setPredicate(true);
				sqlSearchConditionOccurs.setPredicate(sqlPredicate);
				sqlSearchConditions.getSearchConditions().add(sqlSearchConditionOccurs);		// Accodamento condizione ripetitiva
			}
			
		}
		
		return sqlSearchConditions;
	}


	/* ---------------------------------------------------------------------------------
     * Restituisce la posizione del primo token fra quelli forniti, nella stringa, 
     * all'esterno di parentesi
     * ---------------------------------------------------------------------------------
     * 
     * Se nessun token soddisfa si restituisce -1
     * 
     */
	@SuppressWarnings("unused")
	private int indexOfFirstTokenNotInPar(String str, int iBegin, String ... ar_tokenToFind) {
		
		int iToken = 0;
		int iStart = 0;
		int cntPar = 0;
		
		// Scan token da verificare
		for (String token : ar_tokenToFind) {
			iStart = iBegin;
			iToken = str.indexOf(token, iStart);
			while (iToken >= 0) {
				cntPar = 0;
				// Cerco parentesi ( backward
				for (int i = iToken - 1; i >= 0; i--) {
					if (str.charAt(i) == '(') {cntPar++;}
					if (str.charAt(i) == ')') {cntPar--;}
				}
				// Token trovato fuori parentesi: Ok
				if (cntPar == 0) {
					return iToken;
				}
				// Token trovato dentro parentesi: cerca successivo
				iStart = iToken + token.length();
				iToken = str.indexOf(token, iStart);
			}
		}
		return -1;
	}


	/* ---------------------------------------------------------------------------------------------
     * Restituisce la posizione del primo token nella stringa non appartenente al predicato BETWEEN
     * ---------------------------------------------------------------------------------------------
     * 
     * Si scartano token (AND/OR) dentro parentesi e appartenenti a clausola BETWEEN
     * 
     * Se nessun token soddisfa si restituisce -1
     * 
     */
	private int indexOfFirstTokenNotInParOfBetween(String str, int iBegin, String ... ar_tokenToFind) {
		
		int iToken = 0;
		int iStart = 0;
		
		// Scan token da verificare
		for (String token : ar_tokenToFind) {
			iStart = iBegin;
			iToken = str.indexOf(token, iStart);
			while (iToken >= 0) {
				
				// OR fuori parentesi
				if (token.trim().equals("OR") 
				&& !isInsidePar(str, iToken)) {
					return iToken;
				}
				
				// AND fuori parentesi NON di BETWEEN
				if (token.trim().equals("AND") 
				&& !isInsidePar(str, iToken)
				&& !isTokenAndOfBetweenClause(str, iToken)) {
					return iToken;
				}
                
				// AND/OR dentro parentesi: ricerca successivo
				iStart = iToken + token.length();
				iToken = str.indexOf(token, iStart);
			}
		}
		return -1;
	}

    /*
     * Restituisce true se il token è una AND di Between
     * 
     * A BETWEEN B AND C
     * 
     * Index è posizionato prima di AND 
     * AND NON è mai dentro parentesi 
     */
    private boolean isTokenAndOfBetweenClause(String str, int iBegin) {
    	
    	String strWork = "";
    	int iBetween = 0;
    	
    	// Prima eventuale between
    	iBetween = str.indexOf(" BETWEEN ");
    	
      	// Scan BETWEEN presenti nella stringa
    	while (iBetween > 0) {
    		
    		// Between dopo AND: sicuramente AND non di BETWEEN
    		if (iBetween > iBegin) {
				return false;
			}
    		
			// Between dentro parentesi: skip
    		if (isInsidePar(str, iBetween)) {
    			iBetween = str.indexOf(" BETWEEN ", iBetween + 9);
				continue;
			}
    		
    		// Between fuori parentesi: verifica se AND è di between
    		break;
		}
    	
    	// Nessuna BETWEEN o nessuna BETWEEN fuori parentesi: sicuramente AND non di BETWEEN
       	if (iBetween < 0) {
			return false;
		}

       	// E' una AND di BETWEEN
      	strWork = str.substring(iBetween, iBegin);
     	if (strWork.indexOf(" AND ") < 0) {
			return true;
		}
    	
    	// NON è una AND di between ma un separatore di predicati di Search-condition
    	
		return false;
	} 	


	/* -----------------------------------------------------------------------------------
	 * Restituisce true se la stringa inizia con una row-value-expression  
	 * -----------------------------------------------------------------------------------
	 * 
	 * Si tratta di stringhe della forma:
	 * 
	 * (expr1, expr2, exprn) oprt (expr1, expr2, exprn)
	 * 
	 * dove oprt vale;
	 * 
	 * =, <>, !<, !> 
	 * 
	 */
    private boolean isPredicateRowValueExpression(InnerContextAnalysis ictx, String str) throws SQLException, ExceptionAmrita  {
    	
		String strOprtLeft = "";
		String strOprtRight = "";
	  	String ar_expressionLeft[] = null;
	  	String ar_expressionRight[] = null;
		int iOprt = -1;
		

		// Individuo operatore
		if (iOprt < 0 ) {iOprt = str.indexOf("=");}
		if (iOprt < 0 ) {iOprt = str.indexOf("<>");}
		if (iOprt < 0 ) {iOprt = str.indexOf("!>");}
		if (iOprt < 0 ) {iOprt = str.indexOf("!<");}
		
		// Non c'è un operatore previsto
		if (iOprt < 0 ) {
			return false;
		}
		
		// Estrazione espressioni fra parentesi a sinistra e a destra dell'operatore
		strOprtLeft = str.substring(0, iOprt).trim();
		if (str.charAt(iOprt) != '=') {iOprt++;}
		strOprtRight = str.substring(iOprt + 1).trim();

		// Le espressioni devono iniziare con una parentesi aperta
		if (!strOprtLeft.startsWith("(")) {
			return false;
		}
		if (!strOprtRight.startsWith("(")) {
			return false;
		}
		
		// Eliminazione parentesi in espressione di destra
		deleteLeadingCharIfAny(strOprtRight, "(");
		deleteTrailingCharIfAny(strOprtRight, ")");
		
		// Estrazione singli elementi espressioni (expr1, expr2, ... exprn)
		ar_expressionLeft = splitParms(strOprtLeft);
		ar_expressionRight = splitParms(strOprtRight);

		// Le espressioni a destra e sinistra devono avere lo stesso numero di parametri
		if (ar_expressionLeft.length != ar_expressionRight.length) {
			return false;
		}
		
		// Non vengono analizzate le singole espressioni
  		
		// Si tratta sicuramente di row-value-expressions
		
		return true;
	}


	/* -----------------------------------------------------------------------------------
	 * Restituisce true se la stringa inizia con una espressione fra parentesi  
	 * -----------------------------------------------------------------------------------
	 * 
	 * Si tratta di strunghe della forma:
	 * La stringa deve iniziare con (
	 * 
	 * (expr) oprt valore
	 * 
	 * dove oprt vale;
	 * 
	 * =, <, >, <>, !=, !<, !> 
	 * 
	 */
	private boolean isExpressionBetweenPar(InnerContextAnalysis ictx, String str) throws SQLException, ExceptionAmrita  {
    	
    	Scanner scn = null;
    	String token = "";
		InstructionSql instruction = null;
		String strExpr = "";
 		
		scn = new Scanner(str);
		token = nextToken(scn); 					// (
		if (!token.equals("(")) {
			return false;
		}
		strExpr = extractStringBetweenPar(scn);
		token = nextToken(scn);						// oprt ?
		
		// Verifica se espressione valida
    	instruction = new InstructionSql();
		analyzeSqlExpression(ictx, instruction, strExpr);
		
		// Non è una espressione valida
		if (instruction.isParsingError()) {
			return false;					 
		}
		
		return true;
	}

	/* -----------------------------------------------------------------------------------
	 * Restituisce true se la stringa inizia con un predicato 
	 * -----------------------------------------------------------------------------------
	 * 
	 * Predicato basic: si tratta di stringhe della forma:
	 * 
	 * (full-select) oprt (value)
	 * (full-select) oprt  value
	 * (expr1) oprt (expr2)
	 *  expr1  oprt (full-select)
	 *  expr1  oprt expr2
	 *  
	 * dove:
	 * 
	 * expr può essere una espressione algebtica o una full-select
	 * 
	 * oprt vale =, <>, !<, !> 
	 * 
	 */
	@SuppressWarnings("unused")
    private boolean isPredicate(InnerContextAnalysis ictx, String str) throws SQLException, ExceptionAmrita  {
    	
		ArrayList<Integer> al_iOprt = null;
		String strLeftOprt = "";
		String strRightOprt = "";
		String oprt = "";
     	int iBetween = 0;
      	int iDistinct = 0;
    	int iFrom = 0;
       	int iIn = 0;
       	int iLike = 0;
      	int i = 0;
      	int iOprt = 0;
      	
      	al_iOprt = new ArrayList<Integer> ();
      	
       	if (isPredicateBasic(ictx, str)) 				{return true;};
       	if (isPredicateRowValueExpression(ictx, str))	{return true;};
		if (str.startsWith("EXISTS")) {return true;};
		iBetween = str.indexOf(" BETWEEN ");
		if (iBetween > 0 && !isInsidePar(str, iBetween)){return true;};
		iDistinct = str.indexOf(" DISTINCT ");
		if (iDistinct > 0 && !isInsidePar(str, iDistinct)) {
			iFrom = str.indexOf(" FROM ", iDistinct + 9);
			if (iFrom > 0 && str.substring(iDistinct + 9, iFrom + 1).trim().equals("")) {return true;};
		}
		iIn = str.indexOf(" IN ");
		if (iIn > 0 && !isInsidePar(str, iIn)) {return true;};
		iLike = str.indexOf(" LIKE ");
		if (iLike > 0) {return true;};
		if (str.endsWith(" NULL"))  {return true;};
		if (str.startsWith("XMLEXIST")) {return true;};

		// Potrebbero esserci operatori all'interno di scalar-fullselect
		// Si deve prendere il primo operatore expr oprt expr
		i = str.indexOf("<>");
		if (i > 0) {al_iOprt.add(i);}
		i = str.indexOf("!=");
		if (i > 0) {al_iOprt.add(i);}
		i = str.indexOf("<=");
		if (i > 0) {al_iOprt.add(i);}
		i = str.indexOf(">=");
		if (i > 0) {al_iOprt.add(i);}
		i = str.indexOf("=");
		if (i > 0) {al_iOprt.add(i);}
		i = str.indexOf("<");
		if (i > 0) {al_iOprt.add(i);}
		i = str.indexOf(">");
		if (i > 0) {al_iOprt.add(i);}
		if (al_iOprt.size() > 0) {
			Collections.sort(al_iOprt);
			iOprt = al_iOprt.get(0);
		}
		

		// Predicato basic o quantified
		if (iOprt > 0) {
			strLeftOprt = str.substring(0, iOprt).trim();
			strRightOprt = str.substring(iOprt + 2).trim();
			oprt = str.substring(iOprt, iOprt + 2).trim();
			
			// Predicato quantified
			if (strRightOprt.startsWith("SOME") 
			|| strRightOprt.startsWith("ANY")
			|| strRightOprt.startsWith("ALL")) {return true;};
			
			// Predicato basic
			return true;
		}

    	return false;
    }
    	

	/* -----------------------------------------------------------------------------------
	 * Restituisce true se la stringa inizia con un predicato basic 
	 * -----------------------------------------------------------------------------------
	 * 
	 * Si tratta di stringhe della forma:
	 * 
	 * (full-select) oprt (value)
	 * (full-select) oprt  value
	 * (expr1) oprt (expr2)
	 *  expr1  oprt (full-select)
	 *  expr1  oprt expr2
	 *  
	 * dove:
	 * 
	 * expr può essere una espressione algebtica o una full-select
	 * 
	 * oprt vale =, <>, !<, !> 
	 * 
	 */
    private boolean isPredicateBasic(InnerContextAnalysis ictx, String str) throws SQLException, ExceptionAmrita  {
    	
   		InstructionSql instruction = null;
		String strExprLeft = "";
		int iOprt = -1;

		// Individuo operatore
		if (iOprt < 0 ) {iOprt = str.indexOf("=");}
		if (iOprt < 0 ) {iOprt = str.indexOf("<");}
		if (iOprt < 0 ) {iOprt = str.indexOf(">");}
		if (iOprt < 0 ) {iOprt = str.indexOf("<>");}
		if (iOprt < 0 ) {iOprt = str.indexOf("!>");}
		if (iOprt < 0 ) {iOprt = str.indexOf("!<");}
		
		// Non è sicuramente un predicato basic
		if (iOprt < 0 ) {
			return false;
		}
		
		// Operatore dentro parentesi: non è un predicato basic
		if (isInsidePar(str, iOprt)) {
			return false;
		}
		
		strExprLeft = str.substring(0, iOprt).trim();
			
		// Verifica se espressione valida, anche se fra parentesi o con full-select
    	instruction = new InstructionSql();
		analyzeSqlExpression(ictx, instruction, strExprLeft);
		
		// Non è una espressione valida
		if (instruction.isParsingError()) {
			return false;					 
		}
		
		return true;
	}


	/* ---------------------
     * Analisi predicato Sql
     * ---------------------
     * 
     * Viene fornita la stringa con un singolo predicato da analizzare in modo sconnesso dal resto dell'istruzione.
     * Viene restituito un oggetto SqlPredicate con il predicato analizzato.
     * Il predicato può essere fornito da parentesi di wrapper in numero illimitato (((... predicate ...)))
     * che vengono eliminate prima dei ogni operazione
     * 
     */
	private SqlPredicate analyzeSqlSearchConditionsPredicate(InnerContextAnalysis ictx, InstructionSql instruction, String strPredicate) throws SQLException, ExceptionAmrita {
		
		SqlPredicate sqlPredicate = null;

		String strLeftOprt = "";
		String strRightOprt = "";
		String oprt = "";
		ArrayList<Integer> al_iOprt = null;
		int i = 0;
		int iOprt = 0;
		int iBetween = 0;
		int iLike = 0;
		int iIn = 0;
		int iDistinct = 0;
		int iXmlExists = 0;
 		int iFrom = 0;
 		
 		// Allocazione strutture di lavoro
		sqlPredicate = new SqlPredicate ();
		al_iOprt = new ArrayList<Integer> ();
		
		// Eliminazione eventuali parentesi wrapper ridondanti
		if (strPredicate.startsWith("(")) {
			strPredicate = normalizeBalancedPar(strPredicate);
		}
		
		// Predicato EXISTS
		if (strPredicate.startsWith("EXISTS")) {
			analyzeSqlSearchConditionsPredicateExists(ictx, instruction, sqlPredicate, strPredicate);
			return sqlPredicate;
		}
		
		// Predicato BETWEEN
		iBetween = strPredicate.indexOf(" BETWEEN ");
		if (iBetween > 0 && !isInsidePar(strPredicate, iBetween)) {
			analyzeSqlSearchConditionsPredicateBetween(ictx, instruction, sqlPredicate, strPredicate, iBetween);
			return sqlPredicate;
		}
		
		// Predicato DISTINCT
		iDistinct = strPredicate.indexOf(" DISTINCT ");
		if (iDistinct > 0 && !isInsidePar(strPredicate, iDistinct)) {
			iFrom = strPredicate.indexOf(" FROM ", iDistinct + 9);
			// DISTINCT FROM
			if (iFrom > 0 && strPredicate.substring(iDistinct + 9, iFrom + 1).trim().equals("")) {
				analyzeSqlSearchConditionsPredicateDistinct(ictx, instruction, sqlPredicate, strPredicate, iDistinct);
				return sqlPredicate;
			}
		}
		
		// Predicato IN 
		iIn = strPredicate.indexOf(" IN ");
		if (iIn > 0 && !isInsidePar(strPredicate, iIn)) {
			analyzeSqlSearchConditionsPredicateIn(ictx, instruction, sqlPredicate, strPredicate, iIn);
			return sqlPredicate;
		}
		
		// Predicato LIKE
		iLike = strPredicate.indexOf(" LIKE ");
		if (iLike > 0) {
			analyzeSqlSearchConditionsPredicateLike(ictx, instruction, sqlPredicate, strPredicate, iLike);
			return sqlPredicate;
		}
		
		// Predicato NULL
		if (strPredicate.endsWith(" NULL")) {
			analyzeSqlSearchConditionsPredicateNull(ictx, instruction, sqlPredicate, strPredicate);
			return sqlPredicate;
		}
		
		// Predicato XMLEXIST
		if (strPredicate.startsWith("XMLEXIST")) {
			analyzeSqlSearchConditionsPredicateXmlExists(ictx, instruction, sqlPredicate, strPredicate, iXmlExists);
			return sqlPredicate;
		}

		// Potrebbero esserci operatori all'interno di scalar-fullselect
		// Si deve prendere il primo operatore expr oprt expr
		i = strPredicate.indexOf("<>");
		if (i > 0) {al_iOprt.add(i);}
		i = strPredicate.indexOf("!=");
		if (i > 0) {al_iOprt.add(i);}
		i = strPredicate.indexOf("<=");
		if (i > 0) {al_iOprt.add(i);}
		i = strPredicate.indexOf(">=");
		if (i > 0) {al_iOprt.add(i);}
		i = strPredicate.indexOf("=");
		if (i > 0) {al_iOprt.add(i);}
		i = strPredicate.indexOf("<");
		if (i > 0) {al_iOprt.add(i);}
		i = strPredicate.indexOf(">");
		if (i > 0) {al_iOprt.add(i);}
		if (al_iOprt.size() > 0) {
			Collections.sort(al_iOprt);
			iOprt = al_iOprt.get(0);
		}
		

		// Predicato basic o quantified
		if (iOprt > 0) {
			strLeftOprt = strPredicate.substring(0, iOprt).trim();
			strRightOprt = strPredicate.substring(iOprt + 2).trim();
			oprt = strPredicate.substring(iOprt, iOprt + 2).trim();
			
			// Predicato quantified
			if (strRightOprt.startsWith("SOME") 
			|| strRightOprt.startsWith("ANY")
			|| strRightOprt.startsWith("ALL")) {
				analyzeSqlSearchConditionsPredicateQuantified(ictx, instruction, sqlPredicate, strLeftOprt, oprt, strRightOprt);
				return sqlPredicate;
			}
			
			// Predicato basic
			analyzeSqlSearchConditionsPredicateBasic(ictx, instruction, sqlPredicate, strLeftOprt, oprt, strRightOprt);
			return sqlPredicate;
			
		}
		

		// Errore di parsing
		instruction.setParsingError(true);
		instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", "", null, this.strProgramScript, this.strProgramScriptObj);

		return sqlPredicate;
	}



	/*
	 * ---------------------------
	 * Analisi predicato sql basic
	 * ---------------------------
	 * 
	 *  expression oprt expression
	 * (row-value-expression =|<> row-value-expression
	 */
	private void analyzeSqlSearchConditionsPredicateBasic(InnerContextAnalysis ictx
														, InstructionSql instruction
														, SqlPredicate sqlPredicate
														, String strLeftOprt
														, String oprt
														, String strRightOprt
														 ) throws SQLException, ExceptionAmrita {


		ArrayList<InnerInstructionWordsEntry> al_sqlKeyWord = null;
		SqlExpression sqlExpression = null;
		String[] ar_strExpressionLeft = null;
		String[] ar_strExpressionRight = null;
		String strBetweenPar = "";
		
		sqlPredicate.setTypePredicate(EnumSqlPredicate.BASIC);
		
		// Operatore
		al_sqlKeyWord = this.map_SqlReservedWords.get(oprt.equals("!=") ? "<>" : oprt);
        if (al_sqlKeyWord == null) {
        	instruction.setParsingError(true);
        	instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", oprt, null, this.strProgramScript, this.strProgramScriptObj);
		    return;
        }
        sqlPredicate.setBasicOperator(al_sqlKeyWord.get(0).en_WordReservedOwner.getTypeOperatorPredicate());
        
        // (Row-value-expression-left) oprt (Row-value-expression-right)
		if (strLeftOprt.startsWith("(") && strLeftOprt.endsWith(")") && strRightOprt.startsWith("(") && strRightOprt.endsWith(")")) {
			
			sqlPredicate.setBasicRowValueExpressions(true);
			
			// Estrazione espressioni parte left
			strBetweenPar = strLeftOprt.substring(1);
			strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1);
			ar_strExpressionLeft = splitParms(strBetweenPar);
			
			// Estrazione espressioni parte right
			strBetweenPar = strRightOprt.substring(1);
			strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1);
			ar_strExpressionRight = splitParms(strBetweenPar);

			// Scan espressioni row-value-expression left
			for (String strExpression : ar_strExpressionLeft) {
				sqlExpression = analyzeSqlExpression(ictx, instruction, strExpression);
				if (instruction.isParsingError()) {return;}
				sqlPredicate.getBasicRowValueExpressionsLeft().add(sqlExpression);
			}
			
			// Scan espressioni row-value-expression right
			for (String strExpression : ar_strExpressionRight) {
				sqlExpression = analyzeSqlExpression(ictx, instruction, strExpression);
				if (instruction.isParsingError()) {return;}
				sqlPredicate.getBasicRowValueExpressionsRight().add(sqlExpression);
			}
			return;
		}
	
		// expression-left oprt expression-right
		
		sqlPredicate.setBasicRowValueExpressions(false);

		// Analisi espressione parte left
		sqlExpression = analyzeSqlExpression(ictx, instruction, strLeftOprt);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setBasicExpressionLeft(sqlExpression);
		
		// Analisi espressione parte tight
		sqlExpression = analyzeSqlExpression(ictx, instruction, strRightOprt);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setBasicExpressionRight(sqlExpression);
		
	}


	/*
	 * ---------------------------------
	 * Analisi predicato sql quantified
	 * ---------------------------------
	 * 
	 * 
	 * 
	 */
	private void analyzeSqlSearchConditionsPredicateQuantified(InnerContextAnalysis ictx
															 , InstructionSql instruction
															 , SqlPredicate sqlPredicate
															 , String strLeftOprt 
															 , String oprt
															 , String strRightOprt
															  ) throws SQLException, ExceptionAmrita {

		Scanner scn = null;
		SqlFullSelect sqlFullselect = null;
		String strFullSelect = "";
		String token = "";

		ArrayList<InnerInstructionWordsEntry> al_sqlKeyWord = null;
		SqlExpression sqlExpression = null;
		String[] ar_strExpression = null;
		String strBetweenPar = "";
		 
		sqlPredicate.setTypePredicate(EnumSqlPredicate.QUANTIFIED);
		
		// Operatore
		al_sqlKeyWord = this.map_SqlReservedWords.get(oprt);
        if (al_sqlKeyWord == null) {
        	instruction.setParsingError(true);
        	instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", oprt, null, this.strProgramScript, this.strProgramScriptObj);
		    return;
        }
        sqlPredicate.setQuantifiedOperator(al_sqlKeyWord.get(0).en_WordReservedOwner.getTypeOperatorPredicate());
        
        
        //////////////////////////////////////////////////
        // Analisi parte a sinistra dell'operatore
        //////////////////////////////////////////////////
        
        // (Row-value-expression-left) oprt ....
		if (strLeftOprt.startsWith("(") && strLeftOprt.endsWith(")")) {
			
			sqlPredicate.setQuantifiedRowValueExpression(true);
			
			// Estrazione e analisi espressioni parte left
			strBetweenPar = strLeftOprt.substring(1);
			strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1);
			ar_strExpression = splitParms(strBetweenPar);
			
			// Scan espressioni sql
			for (String strExpression : ar_strExpression) {
				sqlExpression = analyzeSqlExpression(ictx, instruction, strExpression);
				if (instruction.isParsingError()) {return;}
				sqlPredicate.getQuantifiedRowValueExpressionsLeft().add(sqlExpression);
			}
		
		// expression oprt ....
		} else {
			
			sqlPredicate.setQuantifiedRowValueExpression(false);
			
			// Analisi espressione parte left
			sqlExpression = analyzeSqlExpression(ictx, instruction, strLeftOprt);
			if (instruction.isParsingError()) {return;}
			sqlPredicate.setQuantifiedExpressionLeft(sqlExpression);
		}
	
		
        //////////////////////////////////////////////////
        // Analisi parte a destra dell'operatore
        //////////////////////////////////////////////////
        
		this.strNoMemoryToParse = strRightOprt;					// Initial 
		scn = new Scanner(strRightOprt);						//  
		token = nextToken(scn);									// SOME|ANY|ALL

		// qualifier SOME|ANY|ALL
		al_sqlKeyWord = this.map_SqlReservedWords.get(token);
        if (al_sqlKeyWord == null) {
        	instruction.setParsingError(true);
        	instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", token, null, this.strProgramScript, this.strProgramScriptObj);
		    return;
        }
        sqlPredicate.setQuantifiedQualifier(al_sqlKeyWord.get(0).en_WordReservedOwner);

		// operator
		al_sqlKeyWord = this.map_SqlReservedWords.get(oprt);
        if (al_sqlKeyWord == null) {
        	instruction.setParsingError(true);
        	instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0035", oprt, null, this.strProgramScript, this.strProgramScriptObj);
		    return;
        }
        sqlPredicate.setQuantifiedOperator(al_sqlKeyWord.get(0).en_WordReservedOwner.getTypeOperatorPredicate());

        // (full-select)
        token = nextToken(scn);									// (
        strFullSelect = extractStringBetweenPar(scn);
        sqlFullselect = analyzeSqlFullselect(ictx, instruction, strFullSelect);
        sqlPredicate.setQuantifiedFullSelect(sqlFullselect);
	}


	/*
	 * ---------------------------------
	 * Analisi predicato sql BETWEEN
	 * ---------------------------------
	 * 
	 * expression |NOT BETWEEN  expression-left AND expression-left
	 * 
	 */
	private void analyzeSqlSearchConditionsPredicateBetween(InnerContextAnalysis ictx
														  , InstructionSql instruction
														  , SqlPredicate sqlPredicate
														  , String strPredicate
														  , int iBetween
														   ) throws SQLException, ExceptionAmrita {
		SqlExpression sqlExpression = null;
		String strExpressionleft = ""; 
		String strExpressionLower = ""; 
		String strExpressionHigher = "";
		String strBetweenRight = "";
		boolean isNotBetween = false;				// expression NOT BETWEEN  expression-left AND expression-left
		int iAnd = 0;
		
		sqlPredicate.setTypePredicate(EnumSqlPredicate.BETWEEN);
		
		/////////////////////////////////////////////////////////
		// Estrazione espressioni contenute nel predicato
		/////////////////////////////////////////////////////////
		
		iBetween = strPredicate.indexOf(" BETWEEN ");
		strExpressionleft = strPredicate.substring(0, iBetween).trim();
		if (strExpressionleft.endsWith(" NOT")) {
			isNotBetween = true;
			sqlPredicate.setBetweenNot(isNotBetween);
			strExpressionleft = strExpressionleft.substring(0, strExpressionleft.length() - 4).trim();
		}
		strBetweenRight = strPredicate.substring(iBetween + 8).trim();		// expression-left AND expression-left
		iAnd = strBetweenRight.indexOf(" AND ");
		strExpressionLower = strBetweenRight.substring(0, iAnd).trim();
		strExpressionHigher = strBetweenRight.substring(iAnd + 5).trim();
		
		/////////////////////////////////////////////////////////
		// Analisi espressioni  
		/////////////////////////////////////////////////////////
		
		// Espression di confronto
		sqlExpression = analyzeSqlExpression(ictx, instruction, strExpressionleft);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setBetweenExpression(sqlExpression);
		
		// Expression lower
		sqlExpression = analyzeSqlExpression(ictx, instruction, strExpressionLower);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setBetweenExpressionLower(sqlExpression);
		
		// Expression higher
		sqlExpression = analyzeSqlExpression(ictx, instruction, strExpressionHigher);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setBetweenExpressionHigher(sqlExpression);
		
	}


	/*
	 * ---------------------------------
	 * Analisi predicato sql DISTINCT
	 * ---------------------------------
	 * 
	 * 
	 * 
	 */
	private void analyzeSqlSearchConditionsPredicateDistinct(InnerContextAnalysis ictx
														   , InstructionSql instruction
														   , SqlPredicate sqlPredicate
														   , String strPredicate
														   , int iDistinct
														    ) throws SQLException, ExceptionAmrita {
		SqlExpression sqlExpression = null;
		String[] ar_strExpression = null;
		String strDistinctLeft = ""; 	// expression-left|(row-value-expresion-left) IS |NOT DISTINCT FROM  expression-right|(row-value-expresion-right)
		String strDistinctRight = "";
		String strBetweenPar = "";
		
		sqlPredicate.setTypePredicate(EnumSqlPredicate.DISTINCT);
		
		/////////////////////////////////////////////////////////
		// Estrazione espressioni contenute nel predicato
		/////////////////////////////////////////////////////////
		
		// Sinistra
		strDistinctLeft = strPredicate.substring(0, iDistinct).trim();
		if (strDistinctLeft.endsWith(" NOT")) {
			sqlPredicate.setDistinctNot(true);
			strDistinctLeft = strDistinctLeft.substring(0, strDistinctLeft.length() - 3).trim();	// Eliminazione NOT
		}
		strDistinctLeft = strDistinctLeft.substring(0, strDistinctLeft.length() - 2).trim();		// Eliminazione IS finale
		
		// Destra
		strDistinctRight = strPredicate.substring(iDistinct + 10).trim();						    // Eliminazione DISTINCT		 
		strDistinctRight = strDistinctRight.substring(5).trim();								    // Eliminazione FROM

		
		/////////////////////////////////////////////////////////
		// Analisi DISTINCT FROM fra  (Row-value-expression) 
		/////////////////////////////////////////////////////////
		
	    // (Row-value-expression-left) IS |NOT DISTINCT FROM (Row-value-expression-right)
		if (strDistinctLeft.startsWith("(")) {
			
			sqlPredicate.setDistinctRowValueExpression(true);
			
			// Estrazione e analisi espressioni parte left
			strBetweenPar = strDistinctLeft.substring(1);
			strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1);
			ar_strExpression = splitParms(strBetweenPar);
			
			// Scan espressioni sql
			for (String strExpression : ar_strExpression) {
				sqlExpression = analyzeSqlExpression(ictx, instruction, strExpression);
				if (instruction.isParsingError()) {return;}
				sqlPredicate.getDistinctRowValueExpressionsLeft().add(sqlExpression);
			}
		
			// Estrazione e analisi espressioni parte right
			strBetweenPar = strDistinctRight.substring(1);
			strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1);
			ar_strExpression = splitParms(strBetweenPar);
			
			// Scan espressioni sql
			for (String strExpression : ar_strExpression) {
				sqlExpression = analyzeSqlExpression(ictx, instruction, strExpression);
				if (instruction.isParsingError()) {return;}
				sqlPredicate.getDistinctRowValueExpressionsRight().add(sqlExpression);
			}
		    return;
		}
	
		
		/////////////////////////////////////////////////////////
		// Analisi DISTINCT FROM fra expression
		/////////////////////////////////////////////////////////
		
		sqlPredicate.setDistinctRowValueExpression(false);
		
		// Espressione parte left
		sqlExpression = analyzeSqlExpression(ictx, instruction, strDistinctLeft);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setDistinctExpressionLeft(sqlExpression);

		// Espressione parte right
		sqlExpression = analyzeSqlExpression(ictx, instruction, strDistinctRight);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setDistinctExpressionRight(sqlExpression);

	}

	/*
	 * ---------------------------------
	 * Analisi predicato sql EXISTS
	 * ---------------------------------
	 * 
	 * 
	 * 
	 */
	private void analyzeSqlSearchConditionsPredicateExists(InnerContextAnalysis ictx
														 , InstructionSql instruction
														 , SqlPredicate sqlPredicate
														 , String strPredicate
														  ) throws SQLException, ExceptionAmrita {
		SqlFullSelect sqlFullselect = null;
		String strFullSelect = "";
		
		strFullSelect = strPredicate.substring(7).trim();						// Eliminazione EXISTS
		strFullSelect = strFullSelect.substring(1);								// Eliminazione (
		strFullSelect = strFullSelect.substring(0, strFullSelect.length() - 1);	// Eliminazione )

		sqlPredicate.setTypePredicate(EnumSqlPredicate.EXISTS);

		sqlFullselect = analyzeSqlFullselect(ictx, instruction, strFullSelect);
		sqlPredicate.setExistsFullSelect(sqlFullselect);
	}

	/*
	 * ---------------------------------
	 * Analisi predicato sql IN
	 * ---------------------------------
	 * 
	 *  1) expression-left        |NOT IN (full-select)|(expr1,expr2,..,exprn)
	 *  2) (row-value-expresion)  |NOT IN (full-select)
	 */
	private void analyzeSqlSearchConditionsPredicateIn(InnerContextAnalysis ictx
													 , InstructionSql instruction
													 , SqlPredicate sqlPredicate
													 , String strPredicate
													 , int iIn
													  ) throws SQLException, ExceptionAmrita {

		InstructionSql instructionService = null;
		SqlExpression sqlExpression = null;
		String[] ar_strExpression = null;
		SqlFullSelect sqlFullselect = null;
		String strInLeft = ""; 	 
		String strInRight = "";
		String strBetweenPar = "";
		
		sqlPredicate.setTypePredicate(EnumSqlPredicate.IN);
		
		/////////////////////////////////////////////////////////
		// Estrazione espressioni contenute nel predicato
		/////////////////////////////////////////////////////////
		
		// Sinistra
		strInLeft = strPredicate.substring(0, iIn).trim();
		if (strInLeft.endsWith(" NOT")) {
			sqlPredicate.setInNot(true);
			strInLeft = strInLeft.substring(0, strInLeft.length() - 3).trim();						// Eliminazione NOT
		}
		// Destra
		strInRight = strPredicate.substring(iIn + 4).trim();									// Eliminazione IN		 

		
		/////////////////////////////////////////////////////////
		// Analisi parte sinistra predicato IN 
		/////////////////////////////////////////////////////////
		
	    // (Row-value-expression-left) |NOT IN (full-select)|(expr1,expr2,..,exprn)
		if (strInLeft.startsWith("(")) {
			
			sqlPredicate.setInRowValueExpressionLeft(true);
			
			// Estrazione e analisi espressioni parte left
			strBetweenPar = strInLeft.substring(1);
			strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1);
			ar_strExpression = splitParms(strBetweenPar);
			
			// Scan espressioni sql
			for (String strExpression : ar_strExpression) {
				sqlExpression = analyzeSqlExpression(ictx, instruction, strExpression);
				if (instruction.isParsingError()) {return;}
				sqlPredicate.getInRowValueExpressionsLeft().add(sqlExpression);
			}
		
		// expression |NOT IN (full-select)|(expr1,expr2,..,exprn)
		} else {
			sqlPredicate.setInRowValueExpressionLeft(false);
			sqlExpression = analyzeSqlExpression(ictx, instruction, strInLeft);
			sqlPredicate.setInExpressionLeft(sqlExpression);
		}
	

		/////////////////////////////////////////////////////////
		// Analisi parte destra predicato IN 
		/////////////////////////////////////////////////////////
		
		strBetweenPar = strInRight.substring(1);
		strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1).trim();
		
		// Si può trattare di fullselect singola, l'analisi non deve dare parsing error
		if (strBetweenPar.startsWith("SELECT")) {
			instructionService = new InstructionSql();
			analyzeSqlFullselect(ictx, instructionService, strBetweenPar);
			// fullselect singola
			if (!instructionService.isParsingError()) {
				sqlFullselect = analyzeSqlFullselect(ictx, instruction, strBetweenPar);
				sqlPredicate.setInFullSelect(true);
				sqlPredicate.setInFullSelect(sqlFullselect);
				return;
			}
		}

        // Si tratta di un elenco di espressioni separate da virgole (potrebbero essere anche + fullselect)
		ar_strExpression = splitParms(strBetweenPar);
		
		// Scan espressioni sql
		for (String strExpression : ar_strExpression) {
			sqlExpression = analyzeSqlExpression(ictx, instruction, strExpression);
			if (instruction.isParsingError()) {return;}
			sqlPredicate.setInFullSelect(false);
			sqlPredicate.getInExpressionsRight().add(sqlExpression);
		}

	}

	/*
	 * ---------------------------------
	 * Analisi predicato sql LIKE
	 * ---------------------------------
	 * 
	 */
	private void analyzeSqlSearchConditionsPredicateLike(InnerContextAnalysis ictx
													   , InstructionSql instruction
													   , SqlPredicate sqlPredicate
													   , String strPredicate
													   , int iLike
													    ) throws SQLException, ExceptionAmrita {
		SqlExpression sqlExpression = null;
		String strLikeMatchExpression = ""; 	 
		String strLikePatternExpression = ""; 	 
		String strLikeEscapeExpression = ""; 	 
		int iEscape = 0;
		
		sqlPredicate.setTypePredicate(EnumSqlPredicate.LIKE);
		
		
		/////////////////////////////////////////////////////////
		// Estrazione espressioni contenute nel predicato
		/////////////////////////////////////////////////////////
		
		// Match-expression
		strLikeMatchExpression = strPredicate.substring(0, iLike).trim();
		if (strLikeMatchExpression.endsWith(" NOT")) {
			sqlPredicate.setLikeNot(true);
			strLikeMatchExpression = strLikeMatchExpression.substring(strLikeMatchExpression.length() - 3).trim();	// Eliminazione NOT
		}
		
		// Pattern-expression, escape-expression
		strLikePatternExpression = strPredicate.substring(iLike + 5).trim();
		iEscape = strLikePatternExpression.indexOf(" ESCAPE ");
		if (iEscape > 0) {
			strLikeEscapeExpression = strLikePatternExpression.substring(iEscape + 8).trim();
			strLikePatternExpression = strLikePatternExpression.substring(0, iEscape).trim();	// Eliminazione ESCAPE
		}

		
		
		/////////////////////////////////////////////////////////
		// Analisi espressioni LIKE
		/////////////////////////////////////////////////////////
		
		sqlExpression = analyzeSqlExpression(ictx, instruction, strLikeMatchExpression);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setLikeExpressionMatch(sqlExpression);
	
		sqlExpression = analyzeSqlExpression(ictx, instruction, strLikePatternExpression);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setLikeExpressionMatch(sqlExpression);
	
		sqlExpression = analyzeSqlExpression(ictx, instruction, strLikeEscapeExpression);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setLikeExpressionMatch(sqlExpression);
	
	}


	/*
	 * ---------------------------------
	 * Analisi predicato sql NULL
	 * ---------------------------------
	 * 
	 */
	private void analyzeSqlSearchConditionsPredicateNull(InnerContextAnalysis ictx
													   , InstructionSql instruction
													   , SqlPredicate sqlPredicate
													   , String strPredicate
													    ) throws SQLException, ExceptionAmrita {
		
		SqlExpression sqlExpression = null;
		String strExpression = ""; 	 
		
		sqlPredicate.setTypePredicate(EnumSqlPredicate.NULL);
		
		
		/////////////////////////////////////////////////////////
		// Estrazione espressione di NULL
		/////////////////////////////////////////////////////////
		
		// null-expression
		strExpression = strPredicate.trim();
		strExpression = strExpression.substring(0, strExpression.length() - 4).trim();		// Eliminazione NULL
		if (strExpression.endsWith(" NOT")) {
			sqlPredicate.setNullNot(true);
			strExpression = strExpression.substring(0, strExpression.length() - 3).trim();	// Eliminazione NOT
		}
		strExpression = strExpression.substring(0, strExpression.length() - 2).trim();		// Eliminazione IS
		
		
		/////////////////////////////////////////////////////////
		// Analisi espressioni NULL
		/////////////////////////////////////////////////////////
		
		sqlExpression = analyzeSqlExpression(ictx, instruction, strExpression);
		if (instruction.isParsingError()) {return;}
		sqlPredicate.setNullExpression(sqlExpression);
	}




	/*
	 * ---------------------------------
	 * Analisi predicato sql XMLEXISTS
	 * ---------------------------------
	 * 
	 */
	private void analyzeSqlSearchConditionsPredicateXmlExists(InnerContextAnalysis ictx
															, InstructionSql instruction
															, SqlPredicate sqlPredicate
															, String strPredicate
															, int iXmlExists
															 ) {

		String strBetweenPar = ""; 	 
		
		sqlPredicate.setTypePredicate(EnumSqlPredicate.XMLEXISTS);
		
		
		/////////////////////////////////////////////////////////
		// Estrazione espressione valore dentro parentesi
		/////////////////////////////////////////////////////////
		
		strBetweenPar = strPredicate.substring(9).trim();
		strBetweenPar = strBetweenPar.substring(1);										// Eliminazione (
		strBetweenPar = strBetweenPar.substring(0, strBetweenPar.length() - 1).trim();	// Eliminazione )
		
        // Viene solo memorizzato il contenuto fra parentesi		
		sqlPredicate.setXmlexistsValue(strBetweenPar);
		
	}

	
	
	
	/*-----------------------------------------------
	 * Operazioni finali di analisi dello script Sql
	 * ----------------------------------------------
	 * 
	 * Gestione segnalazione errori 
	 * Completamento analisi post parsing
	 * 
	 * 
	 */
	private void finalOperationsScript() throws Exception {
        
		EntityMetricValue entityMetric = null;
		
		
		///////////////////////////////////////////////////////////////////////////////////
        // (1) Errori/warning di parsing: logging istruzioni in errore e fine elaborazione
        ///////////////////////////////////////////////////////////////////////////////////

		if (this.ictx.isAnyInstructionErrorDetected
		||  this.ictx.isAnyInstructionWarningDetected) {
			loggingParsingErrorsWarning();				// Si trattano le istruzioni di tutte le divisioni Cobol
			if (this.ictx.isAnyInstructionErrorDetected) {
				this.di.isExecutionWithErrors = true;
				this.di.curObjectWithErrors = true;
			}
		}
		
		
		/////////////////////////////////////////////////////////////////////////
        // (2) Completamento analisi istruzioni
        /////////////////////////////////////////////////////////////////////////

		if (!this.ictx.isAnyInstructionErrorDetected) {
	        finalSetInstructionInfo();        				// Completamento informazioni su istruzioni analizzate
 	        this.scriptSql.optimize();                      // Ottimizzazione oggetto scriptSql
		}
		
        
  
		/////////////////////////////////////////////////////////////////////////////
        // (3) Marcatura codice morto e metriche di base programma (input da source)
        /////////////////////////////////////////////////////////////////////////////

		if (!this.ictx.isAnyInstructionErrorDetected) {
	        detectMetricsMeasureBasic(); 			  			// Impostazione misure metriche di base, calcolate in ogni caso
	        this.metricsScriptSql.computeIdxDoc();                // Calcolo indici di documentazione
	        // Metriche in base al solo sorgente
	        if (di.optMetricsAll) {
		    	entityMetric = new EntityMetricValue();          	// Oggetto per inserimento su db
		    	this.metricsScriptSql.dbPopulateMetric(entityMetric);
		    	this.analyzerDbInfo.addObjMetric(entityMetric); // Inserimento in tabella metriche programma per update db finale
			}
		}  

		
        //////////////////////////////////////////////////////////////////////////////////////
        // (4) Persistenza su file system, script sql con istruzioni codificate
        //////////////////////////////////////////////////////////////////////////////////////
  
 		putSerialized(ucfg.getDirCobolObjPgm()	     
				, SUFFIX_SERIALIZED_SCRIPT_SQL
				, this.scriptSqlName
				, this.scriptSql
				, EnumAmritaExceptionError.ERROR_SERIALIZE_SQL_SCRIPT
				, "EI0008"
				, "MI0143");
    
    
        /////////////////////////////////////////////////////////////////////////
        // (5) Persistenza su db
        /////////////////////////////////////////////////////////////////////////

		// Aggiornamento stato analisi programma 
        if (this.ictx.isAnyInstructionErrorDetected) {
        	analyzerDbInfo.al_DbObject.get(idxScriptInDbObject).setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS);      
		} else {
			analyzerDbInfo.al_DbObject.get(idxScriptInDbObject).setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);   
		}
         
        // Aggiornamenti complessivi effettivi base dati
        if (this.di.optUpdateDb) {
        	analyzerDbInfo.sqlDeleteGenericLevel();
        	analyzerDbInfo.sqlDeleteScriptSqlLevel(this.scriptSqlName);
            analyzerDbInfo.update(true, false, this.scriptSqlName, null);
		}
 
		/////////////////////////////////////////////////////////////////////////
        // (6) Statistiche finali
        /////////////////////////////////////////////////////////////////////////

		// Calcolo tempo di elapsed complessivo per il programma
		this.di.curTimeMsEnd = System.currentTimeMillis();
		this.di.curTimeMsElapsed = this.di.curTimeMsEnd - this.di.curTimeMsStart;
		Long timeElapsed = new Long(this.di.curTimeMsElapsed);
		
		// Informazioni applicative utilizzabili anche dal dispatcher chiamante
//		this.di.curNumRowsSource = this.si.getArrayRowSource().length;
//		this.di.curNumTotInstrProc = scriptSql.entriesScript().length;

		// Messaggio di fine elaborazione
        if (this.ictx.isAnyInstructionErrorDetected) {
        	logMessage(EnumMessageType.INFORMATION, "MI0144", this.scriptSqlName, timeElapsed.toString(), this.si.getArrayRowSource().length+"", scriptSql.entriesScript().length+"");    
		} else {
			logMessage(EnumMessageType.INFORMATION, "MI0145", this.scriptSqlName, timeElapsed.toString(), this.si.getArrayRowSource().length+"", scriptSql.entriesScript().length+""); 
		}
 	}

	 
	/**
	 * 
	 *  Restituisce il successivo oggetto Instruction a partire dalle righe sorgente, in modo trasparente. 
	 *  <p>
	 *  Se non ci sono più istruzioni da restituire, restituisce null.
	 *  
	 *  
	 * @param InnerContextAnalysis 
	 * @return Object con oggetto Instruction specifico
	 * 
	 */
	private InstructionSql getNextObjectInstruction(InnerContextAnalysis ictx) {
		
		Object objectInstr = null;		// Oggetto istruzione di output
		InstructionSql instructionSql = null;
		String sourceInstr = "";  		// Stringa istruzione completa, senza eventuale ; finale
		                                // eventualmente derivata da source splittato su + righe
		
		
		sourceInstr = getNextSourceInstruction(ictx);  				// -> this.sourceInstr this.activeTypeInstr
		
		// Fine istruzioni o errore sorgente in input: restituisco oggetto istruzione null
		if (sourceInstr.equals("")) {
			ictx.objectInstr = null;
			return null;
		}

		// Istruzione non riconosciuta: restituisco oggetto istruzione null
		if (ictx.activeTypeInstr == EnumPrecompilerReservedWords.NOT_ASSIGNED) {
			return null;
		}
		
		// Composizione oggetto istruzione con le informazioni accuisite 
		ictx.nameInstr = ictx.activeTypeInstr.toString();
		
		instructionSql = new InstructionSql();
		storeSourceInstructionInfo(ictx.curInstrInfo, instructionSql);
		instructionSql.setTypeInstrPrecompiler(ictx.activeTypeInstr);
		instructionSql.setName(ictx.activeTypeInstr.toString());
		ictx.objectInstr = objectInstr;
		
		this.di.excpInfo = ictx;
		
		return instructionSql;
	}



	
	
	/*
	 * -------------------------------------------------------------------------------------
	 *  Restituisce la successiva istruzione Sql pacchettizzata e senza commenti 
	 *  ------------------------------------------------------------------------------------
	 *  
	 *  In Scrip sql db2 ogni statement è terminato con un punto e virgola ";".
	 *  Tuttavia è possibile trovare statement com END-IF etc che non terminano con ";".
	 *  Pertanto, anche se a  differenza del Cobol l'istruzione è quasi sempre terminata da ";"
	 *  può essere necessario cercare una nuova istruzione per acquisire i limiti di quella corrente.
	 *  
	 *  <p>
	 *  Una riga può contenere anche più di una istruzione e quindi viene anche gestita la posizione di
	 *  inizio e di fine di ogni definizione.<br>
	 *  <p>
	 *  Viene restituito il codice dell'istruzione e la stringa completa dell'istruzione, anche se era spalmata
	 *  su più righe, oppure iniziava alla fine di altre istruzioni, in mezzo alla riga sorgente.
	 *  
	 * @param InnerContextAnalysis ictx 
	 * @return String stringa con l'istruzione completa
	 * 
	 */
	private String getNextSourceInstruction(InnerContextAnalysis ictx) {
		
		InnerTokenInstr tokenLastInstrCur = null;
		ArrayList<InnerTokenInstr> al_tokenSpreadedNormalized = null;
		String sourceInstrPackaged = "";        	// Stringa con istruzione completa packagizzata
		int j = 0;
		int k = 0;
		
		// Fine sorgente
		if (ictx.rowStartSource >= ictx.ar_RowsSource.length) {
			ictx.sourceInstr = "";
			return "";
		}
		
		
        ///////////////////////////////////////////////////////////////////
 		// (1) Individuazione ed estrazione completa istruzione
		///////////////////////////////////////////////////////////////////
		  
		// Estrazione token già normalizzati accodando n righe successive, per gestire parole chiave istruzioni splittate
		al_tokenSpreadedNormalized = new ArrayList<InnerTokenInstr> ();
		ictx.instructionTerminatorDetected = "";
		getTokensSpreaded(ictx, al_tokenSpreadedNormalized, ictx.rowStartSource, ictx.posStartInstr, SQL_SCRIPT_PREFETCH_PARSING_ROWS, true);
		
		// Fine sorgente senza ulteriori istruzioni
		if (al_tokenSpreadedNormalized.size() == 0) {
			ictx.sourceInstr = "";
			return "";
		}
		
        // Individua il tipo di istruzione o NOT_ASSIGNED se non riconosciuta.
		// Viene identificata qualsiasi tipo di istruzione, command sql
		// Split token in + token se necessario per corretta identificazione istruzione
		ictx.activeTypeInstr = getInstructionType(ictx, al_tokenSpreadedNormalized, 0, true);     
		
		// Tipo istruzione non individuata: errore interno o source errato
		if (ictx.activeTypeInstr == EnumPrecompilerReservedWords.NOT_ASSIGNED) {
	      	logMessage(EnumMessageType.ERROR_INTERNAL, "EI0036", this.scriptSqlName, al_tokenSpreadedNormalized.get(0).numRowSource+"", al_tokenSpreadedNormalized.get(0).posInRow+"", al_tokenSpreadedNormalized.get(0).token);
	       	logMessage(EnumMessageType.INFORMATION, "MI0146", this.scriptSqlName, ictx.ar_RowsSource[al_tokenSpreadedNormalized.get(0).numRowSource]);
	       	ictx.isAnyInstructionErrorDetected = true;
	       	ictx.sourceInstr = "";
			return "";
		}

		// Nei commenti prima dell'istruzione c'era #SET TERMINATOR char
		if (!ictx.instructionTerminatorDetected.equals("") && ictx.numRowInstructionTerminatorDetected < al_tokenSpreadedNormalized.get(0).numRowSource) {
			ictx.instructionTerminatorActive = ictx.instructionTerminatorDetected;
		}

		// Individuazione ultimo token istruzione corrente (potrebbe far accodare ulteriori token spreaded)
		// Split token in + token normalizzati se necessario per corretta gestione parsing successivi
		ictx.rowStartSource = al_tokenSpreadedNormalized.get(0).numRowSource;
		ictx.numLastTokenCurInstr = getLastTokenInstr(ictx, al_tokenSpreadedNormalized, ictx.activeInstrKeyWordsSize);	
		tokenLastInstrCur = al_tokenSpreadedNormalized.get(ictx.numLastTokenCurInstr);			 
		
		// Completamento informazioni istruzione corrente
		ictx.rowEndSource = tokenLastInstrCur.numRowSource;
		ictx.posEndInstr = tokenLastInstrCur.posInRow + tokenLastInstrCur.token.length();
//GPZ	// Impostazione ultimo token istruzione precedente
//		if (ictx.numLastTokenPrecInstr == 0) {
//			tokenLastInstrPrec = null;
//		} else {
//			tokenLastInstrPrec = al_tokenSpreadedNormalized.get(ictx.numLastTokenPrecInstr);
//		}
//     	extractInfoComments(ictx, tokenLastInstrPrec, al_tokenSpreadedNormalized.get(0), tokenLastInstrCur);   // -> ictx.curInstInfo
		ictx.curInstrInfo.setRowStartSource(al_tokenSpreadedNormalized.get(0).numRowSource);
		ictx.curInstrInfo.setPosStartInstr(al_tokenSpreadedNormalized.get(0).posInRow);
		ictx.curInstrInfo.setRowEndSource(ictx.rowEndSource);
		ictx.curInstrInfo.setPosEndInstr(ictx.posEndInstr);
		
		
		//////////////////////////////////////////////////////////////////////////////////////
		// (2) Packaging istruzione eventualmente spalmata su più righe in unica stringa
		//////////////////////////////////////////////////////////////////////////////////////
		
		// Scan token componenti l'istruzione
		sourceInstrPackaged = "";
		for (int i = 0; i <= ictx.numLastTokenCurInstr; i++) {
			sourceInstrPackaged = sourceInstrPackaged + al_tokenSpreadedNormalized.get(i).token + " ";
		}
		sourceInstrPackaged = sourceInstrPackaged.trim();
		sourceInstrPackaged = deleteTrailingCharIfAny(sourceInstrPackaged, ictx.instructionTerminatorActive);
		
        // Caricamento informazioni sorgente nell' oggetto istruzione
        ictx.sourceInstr = sourceInstrPackaged;
        ictx.curInstrInfo.setSourceInstr(sourceInstrPackaged);
		ictx.ar_RowsSourceInstruction = new String[ictx.rowEndSource - ictx.rowStartSource + 1];
		j = 0;
        for (k = ictx.rowStartSource; k <= ictx.rowEndSource; k++) {
        	ictx.ar_RowsSourceInstruction[j] = ictx.ar_RowsSource[k];
        	j++;
		}
        ictx.curInstrInfo.setNumInstr(ictx.numInstr);
     	ictx.curInstrInfo.setRowsSource(ictx.ar_RowsSourceInstruction);
    	ictx.curInstrInfo.setSourceInstr(ictx.sourceInstr);

    	
    	///////////////////////////////////////////////////////////////////////////////////////////
        // (3) Impostazione per reperimento successiva istruzione (sulla stessa riga o successive)
    	///////////////////////////////////////////////////////////////////////////////////////////
    	
    	ictx.numLastTokenPrecInstr = ictx.numLastTokenCurInstr;
    	
        // Istruzione estratta ultima del sorgente
        if (ictx.numLastTokenCurInstr >= al_tokenSpreadedNormalized.size() - 1 
        &&  tokenLastInstrCur.numRowSource >= ictx.ar_RowsSource.length) {
        	ictx.rowStartSource = ictx.ar_RowsSource.length + 1;
        	ictx.posStartInstr = 0;
            return sourceInstrPackaged;
		}
         
        // Ulteriori istruzioni presenti
        ictx.rowStartSource = tokenLastInstrCur.numRowSource;
        ictx.posStartInstr = tokenLastInstrCur.posInRow + tokenLastInstrCur.token.length() + 1;
		
		return sourceInstrPackaged;
	}


	
	/* -------------------------------------------------------------------------
	 * Normalizza un singolo token e restitusce un ArrayList con tutti i valori
	 * -------------------------------------------------------------------------
	 * 
	 * Divisione token con all'interno "=", ";" ",", +, - .....
	 * In caso di literal alfanumerica o numeri non si effettuano operazioni.
	 * 
	 * Si dividono token del tipo:
	 * 
	 * Es. token
	 * 
	 * AA;
	 * (AA
	 * ("AA
	 * A);
	 * A");
	 * ;A
	 * AA;AA
	 * A=B
	 * C*D
	 * ...
	 * ...
	 * 
	 * Es. istruzione
	 * 
	 *     SET CURRENT SQLID=USER;SET .... 
	 *       diventa
	 *     SET CURRENT SQLID = USER ; SET 
	 * 
	 */
    private ArrayList<InnerTokenInstr> getTokensNormalizedFromSingleToken(InnerTokenInstr tokenInput) {
    	
    	ArrayList<InnerTokenInstr> al_tokenNormalized = null;
    	InnerTokenInstr innerTokenInstrNew = null;		//
    	String token = "";								//
    	String tokenLeft = "";							//
 		String tokenToIns = "";							//
		char singleApice = 39;							// Apice singolo '
		char doubleApice = 34;							// Doppio apice ""
		char curApice = ' ';							// Apice corrente
		int i = 0;										//
		int j = 0;										//
		int k = 0;                                      //
		int posInToken = 0;								//
        boolean isTokenSplitted = false;                //
 		
		al_tokenNormalized = new ArrayList<InnerTokenInstr> ();
		token = tokenInput.token;
	   	tokenLeft = "";
 		posInToken = 0;
 		
		// il token è un numero valido: nessun intervento
		if (StringService._isNumeric(token)) {
			al_tokenNormalized.add(tokenInput);
			return al_tokenNormalized;
		}

		
		// Scan fino a completo trattamento
		while (!token.trim().equals("")) {
			
			// Scan crt token
			for (i = 0; i < token.length(); i++) {
				
				// Tipologia caratteri che generano split del token
				if (token.charAt(i) == '=' 
				||  token.charAt(i) == '>'
				||  token.charAt(i) == '<'
				||  token.charAt(i) == ';'
				||  token.charAt(i) == ','
				||  token.charAt(i) == '('
				||  token.charAt(i) == ')'
				||  token.charAt(i) == '+'
				||  token.charAt(i) == '/'
				||  token.charAt(i) == '!'
				||  token.charAt(i) == '*' && i > 0 && token.charAt(i - 1) != '.') {
					 
					// Estrazione e inserimento token a sinistra (se c'è)
					if (i > 0) {
						tokenLeft = token.substring(0, i);
						innerTokenInstrNew = new InnerTokenInstr();
						innerTokenInstrNew.token = tokenLeft;
						innerTokenInstrNew.numRowSource = tokenInput.numRowSource;
						innerTokenInstrNew.posInRow = tokenInput.posInRow + posInToken;
						al_tokenNormalized.add(innerTokenInstrNew);
						posInToken = posInToken + i;
					}
					
					// Inserimento token atomico
					
					// Casi >= <= <> !=
					k = i + 1;
					if (k < token.length() && (token.charAt(k) == '=' || token.charAt(k) == '>')) {k++;}
					
					// token di 1 o 2 crt
					tokenToIns = token.substring(i, k);
					innerTokenInstrNew = new InnerTokenInstr();
					innerTokenInstrNew.token = tokenToIns;
					innerTokenInstrNew.numRowSource = tokenInput.numRowSource;
					innerTokenInstrNew.posInRow = tokenInput.posInRow + posInToken;
					al_tokenNormalized.add(innerTokenInstrNew);
					posInToken = posInToken + k - i;
                    
					// Tratto crt rimanenti a destra di quello appena trattato
					
					token = token.substring(k);
					isTokenSplitted = true;
					i = 0;
					break;
					
				} // end-if crt singoli	
					
					
				// Literal alfanumerica
				if (token.charAt(i) == singleApice || token.charAt(i) == doubleApice) {
					curApice = token.charAt(i);
					// individuo fine literal
					for (j = i + 1; j < token.length(); j++) {
						if (token.charAt(j) == curApice) {
							break;
						}
					}
					
					// Estrazione e inserimento token a sinistra della literal (se c'è)
					if (i > 0) {
						tokenToIns = token.substring(0, i + 1);
						innerTokenInstrNew = new InnerTokenInstr();
						innerTokenInstrNew.token = tokenToIns;
						innerTokenInstrNew.numRowSource = tokenInput.numRowSource;
						innerTokenInstrNew.posInRow = tokenInput.posInRow + posInToken;
						al_tokenNormalized.add(innerTokenInstrNew);
						posInToken = posInToken + i;
					}

					// Caso in script Sql di unico token con due literal divise da un punto: deve restare una literal unica
					// Es. CREATE ALIAS "DBDTB001"."DESCRIZIONI" FOR "DJAP0009"."TATB0PK1";
					// Sposto j, ora sull'apice di fine della prima literal, sulla fine della seconda literal
					if (token.length() > j + 1 && token.charAt(j + 1) == '.' && (token.charAt(j + 2) == singleApice || token.charAt(j + 2) == doubleApice)) {
						curApice = token.charAt(j + 2);
						// individuo fine literal
						for (j = j + 3; j < token.length(); j++) {
							if (token.charAt(j) == curApice) {
								break;
							}
						}
					}
					
					// Inserimento token literal
					tokenToIns = token.substring(i, j + 1);
					innerTokenInstrNew = new InnerTokenInstr();
					innerTokenInstrNew.token = tokenToIns;
					innerTokenInstrNew.numRowSource = tokenInput.numRowSource;
					innerTokenInstrNew.posInRow = tokenInput.posInRow + posInToken;
					al_tokenNormalized.add(innerTokenInstrNew);
					posInToken = posInToken + j - i + 1;
	                
					// Tratto crt rimanenti a destra di quello appena trattato
					token = token.substring(j + 1);
					isTokenSplitted = true;
					i = 0;
					break;
					
				} // end-if literal
				
			} // end-for crt in token
		    
			if (i >= token.length()) {
				break;
			}
			
		} // end-while token ancora da trattare
		
		// Ultimo token parziale da inserire integro
		if (isTokenSplitted && i >= token.length() && !token.trim().equals("")) {
			innerTokenInstrNew = new InnerTokenInstr();
			innerTokenInstrNew.token = token;
			innerTokenInstrNew.numRowSource = tokenInput.numRowSource;
			innerTokenInstrNew.posInRow = tokenInput.posInRow + posInToken;
			al_tokenNormalized.add(innerTokenInstrNew);
		}
		
		// Il token origine non ha subuto split: lo porto in output
		if (!isTokenSplitted) {
			al_tokenNormalized.add(tokenInput);
		}

		return al_tokenNormalized;
	}


	/* -----------------------------------------------------------------
     * Rimpiazza gli spazi dentro le literal con _
     * ------------------------------------------------------------------
     *
     * Se literal non chiusa restituisce  una stringa vuota
     * 
     */
	private String replaceSpaceInLiteralWithUnderscore(String strInput) {

		StringBuffer sb = null;
		char singleApice = 39;					// Apice singol '
		char doubleApice = 34;					// Doppio apice ""
		char curApice = 34;						// Apice corrente utilizzato dalla literal (singolo o doppio)
        int i = 0;
        boolean isLiteral = false;
        boolean isLiteralWellClosed = true;
        
        
        sb = new StringBuffer(strInput);
        
        // Scan caratteri stringa
        for (i = 0; i < strInput.length(); i++) {
        	
            // Cerca primo apice
            for (; i < strInput.length(); i++) {
             	// Non apice singolo e non apice doppio: skip
            	isLiteral = false;
       			if (strInput.charAt(i) == singleApice) {
       				isLiteral = true;
    			}
	   			if (strInput.charAt(i) == doubleApice) {
	   				isLiteral = true;
    			}
	   			// Inizio literal: trattala
	   			if (isLiteral) {
					break;
				}
       		}

            // Fine stringa
            if (i >= strInput.length()) {
				break;
			}

 			curApice = strInput.charAt(i);
 			isLiteralWellClosed = false;
 			
            // replace space in _
            for (i = i + 1; i < strInput.length(); i++) {
            	// Replace space con underscore
    			if (strInput.charAt(i) == ' ') {
    				sb.setCharAt(i, '_');
					continue;
				}
    			
    			// Bypass doppio apice
    			if (curApice == singleApice 
    			&& strInput.charAt(i) == curApice
    			&& i < (strInput.length() -1)
    			&& strInput.charAt(i + 1) == curApice) {
					i = i + 1;
					continue;
				}
    			
    			// Fine literal
       			if (strInput.charAt(i) == curApice) {
       				isLiteralWellClosed = true;
 					break;
				}
       			
       		} // end-for
            
		} // end-for
		
        
        // Literal Non chiusa
        if (!isLiteralWellClosed) {
			return "";
		}
        
        return sb.toString();
        
	}


	/* -----------------------
	 * bypass righe commento
	 * -----------------------
	 * 
	 * Restituisce l'indice della prima riga non commento e non vuota
	 * 
	 */
	private int bypassRowsComment(InnerContextAnalysis ictx, int numRowStart) {

		Scanner scn = null;
		String token = "";
		int iComment = 0;
		int i = 0;
		
		// Ricerca prima riga dell'istruzione da trattare da cui estrarre i token dell'istruzione      
        for (i = numRowStart; i < ictx.ar_RowsSource.length; i++) {
         	ictx.rowAll = ictx.ar_RowsSource[i];
  
           	// E' una riga vuota: skip
        	if (ictx.rowAll.trim().length() == 0) {
         		continue;
			}

    		iComment = ictx.rowAll.indexOf("--");
    		
    		// Riga senza nessun commento 
    		if (iComment < 0) {
       			break;
     	       	
    		// Riga di solo commento 
     		} else if ((iComment == 0 && ictx.rowAll.substring(iComment).startsWith("--"))
     			   ||  (iComment > 0  && ictx.rowAll.substring(iComment).startsWith("--") && ictx.rowAll.substring(0, iComment).trim().equals(""))  ) {
     			
     			// Verifica se #SET TERMINATOR
     			scn = new Scanner(ictx.rowAll);
     			token = scn.next();
  				if (ictx.instructionTerminatorDetected.equals("") && token.equals("--#SET")) {
  					token = scn.next();					// TERMINATOR
  					token = scn.next();					// value
  					ictx.instructionTerminatorDetected = token;
  					ictx.numRowInstructionTerminatorDetected = i;
 				}
     			continue;
     			
       		// Riga con valore seguito da commento
			} else {
				break;
			}
  		}
		
        return i;
	}



	/*
	 * -------------------------------------------------------------------------------------------------------
	 * Individua il tipo di istruzione, come sequenza di parole riservate, nell'elenco di token disponibili
	 * -------------------------------------------------------------------------------------------------------
	 * 
	 * Le parole riservate possono essere anche '*' oppure '*C', con C qualsiasi carattere.
	 * "*" indica che qualsiasi valore è valido per la sequenza di chiavi.
	 * "*C" indica che qualsiasi valore che termina con il carattere C è valido per la sequenza di chiavi.
	 * 
	 * Tutte le possibili sequenze di chiavi che iniziano con la stessa parola, sono state caricate in 
	 * inizializzazione nella map map_SqlReservedWords in modo generalizzato.
	 *  
	 * Se non viene individuata un'istruzione valida restituisce null.
	 * 
	 */
	private EnumPrecompilerReservedWords getInstructionType(InnerContextAnalysis ictx						// Informazioni di contesto
														  , ArrayList<InnerTokenInstr> al_tokenSpreaded		// Token istruzione
														  , int numTokenStart								// primo token dopo key word istruzione
	                                                      , boolean isToDetectInstruction					// true individua istruzione, false solo parole chiave										      
															) {
		 

		EnumPrecompilerReservedWords detectedTypeInstr = null;				//
		EnumPrecompilerReservedWords detectedTypeInstrMax = null;			//
		EnumPrecompilerReservedWords detectedTypeInstrWork = null;			//
		ArrayList<InnerInstructionWordsEntry> al_innerReservedWord = null;	//
		String curWordKey = "";												//
		String curTokenKey = "";											//
        int cntKeysDetectedMax = 0;  										// Numero riga dalla quale iniziare la ricerca
        int cntKeysDetectedWork = 0;
 		
  		// Sono disponibili le singole parole separate da spazi della riga corrente e delle successive 
 		// e il numero di parola dal quale iniziare la ricerca
 
		// Verifica se parola riservata di inizio istruzione
   
 		ictx.token = al_tokenSpreaded.get(numTokenStart).token;			// Prima parola chiave identificativa istruzione
 		ictx.curToken = ictx.token;
 		detectedTypeInstrWork = EnumPrecompilerReservedWords.NOT_ASSIGNED;
 		
		// Cerco istruzione che inizia con il token come parola riservata
 		al_innerReservedWord = this.map_SqlReservedWords.get(ictx.token);
		
		// Parola chiave non censita: puo essere una label.
		if (al_innerReservedWord == null) {
			
    		// Label: il token termina con :
			if (ictx.token.endsWith(":")) {
				detectedTypeInstr = EnumPrecompilerReservedWords.SQL_PROCEDURE_LABEL;
				ictx.activeInstrKeyWordsSize = 1;
				return detectedTypeInstr;
			}
	
	        // Istruzione non identificabile a partire dalla parola richiesta
			return EnumPrecompilerReservedWords.NOT_ASSIGNED;
		}

		// Non può essere l'inizio di una istruzione
		if (isToDetectInstruction && al_innerReservedWord.size() > 0) {
			detectedTypeInstrWork = al_innerReservedWord.get(0).en_WordReservedOwner;
			if (detectedTypeInstrWork.getTypeEntry() != EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL
			&&  detectedTypeInstrWork.getTypeEntry() != EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML		
			&&  detectedTypeInstrWork.getTypeEntry() != EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE		
			&&  detectedTypeInstrWork.getTypeEntry() != EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_COMMAND) {
				return EnumPrecompilerReservedWords.NOT_ASSIGNED;
			}
		}
		
		
		
		// Il token identifica l'inizio di una possibile istruzione valida, che può essere identificata
		// da 1 o + sequenze di parole chiave che iniziano con lo stesso token
		
		detectedTypeInstrMax = EnumPrecompilerReservedWords.NOT_ASSIGNED; 
		cntKeysDetectedMax = 0;
		

		// Scan possibili sequenze di parole chiave.
		// ogni sequenza può essere una variante della stessa istruzione con valori opzionali o meno
		// oppure una possibile istruzione diversa
		for (InnerInstructionWordsEntry innerInstructionWordsEntry : al_innerReservedWord) {
			
			cntKeysDetectedWork = 0;

			// Scan parole chiave dell' insieme corrente.
			// Si deve considerare l'insieme con il match del numero massimo di parole chiave
			for (int i = 0; i < innerInstructionWordsEntry.al_wordKey.size(); i++) {
				
				// Numero token dispnibili inferiore al numero di keys da verificare
				// Es. Caso Sql COMMIT 
				if (numTokenStart + i >= al_tokenSpreaded.size()) {
					break;
				}
				
				// Viaggiano in parallelo
				curWordKey = innerInstructionWordsEntry.al_wordKey.get(i);
				curTokenKey = al_tokenSpreaded.get(numTokenStart + i).token;
				
				// Token non sufficienti a individuare l'istruzione: il chiamante accoderà altri token
				if (numTokenStart + i >= al_tokenSpreaded.size()) {
					return EnumPrecompilerReservedWords.NOT_ASSIGNED;
				}
				
				// No match parola chiave: next insieme di parole chiave
				if (!curWordKey.equals("*" ) && !curWordKey.equals(curTokenKey)) {
					break;
				}
				
				// Match parola chiave: verifico se ultima dell'insieme
				cntKeysDetectedWork++;
				
				// Ultima parola chiave: insieme valido di parole chiave individuata
				if (i == innerInstructionWordsEntry.al_wordKey.size() - 1) {

					detectedTypeInstr = innerInstructionWordsEntry.en_WordReservedOwner;
				    // Richiesto riconoscimento di sole istruzioni
					if (isToDetectInstruction) {
						if (detectedTypeInstr.getTypeEntry() == EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL
						||  detectedTypeInstr.getTypeEntry() == EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML		
						||  detectedTypeInstr.getTypeEntry() == EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE		
						||  detectedTypeInstr.getTypeEntry() == EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_COMMAND) {
							cntKeysDetectedWork = innerInstructionWordsEntry.al_wordKey.size();
							detectedTypeInstrWork = detectedTypeInstr;
						}
						
					// Richiesto riconoscimento di qualunque sequenza di parole chiave valide
					} else {
						cntKeysDetectedWork = innerInstructionWordsEntry.al_wordKey.size();
						detectedTypeInstrWork = detectedTypeInstr;
					}
					
				} // end-if
				
			} // end-for parole chiave dell'insieme corrente
			
			// Considero l'istruzione con il numero maggiore di parole chiave
			// se c'è match nella sequenza di parole chiave
			if (cntKeysDetectedWork == innerInstructionWordsEntry.al_wordKey.size()
			&&  cntKeysDetectedWork > cntKeysDetectedMax) {
				cntKeysDetectedMax = cntKeysDetectedWork;
				ictx.activeInstrKeyWordEntry = innerInstructionWordsEntry;
				detectedTypeInstrMax = detectedTypeInstrWork;
			}
				
		} // end-for insiemi di parole chiave
		
		// Individuato l'insieme di key word con il massimo numero di match
		if (cntKeysDetectedMax > 0) {
			ictx.activeInstrKeyWordsSize = cntKeysDetectedMax;
			return detectedTypeInstrMax;
		}
		
		// Non è stata individuata nessuna istruzione in nessuna sequenza prevista
		
		// Istruzione non identificabile a partire dalla parola richiesta
		return EnumPrecompilerReservedWords.NOT_ASSIGNED;
	}

	

	
	/* -----------------------------------------------------------------------
	 * Individua l'istruzione successiva e pertanto la fine dell'istruzione 
	 * corrente in termini di riga e posizione.
	 * -----------------------------------------------------------------------
	 * 
	 * 
	 * Viene fornita in input una ArrayList con le parole chiave che hanno identificato
	 * l'istruzione, per esempio "SET CURRENT SQLID"
	 * 
	 * Viene cercata la fine dell'istruzione analizzando le parole chiave successive a
	 * quelle identificanti e cercando il primo carattere terminatore ; di default. 
	 * L'ultimo elemento identifica la riga e la posizione dell'ultimo token utile.
	 * 
	 *    
	 */
	private int getLastTokenInstr(InnerContextAnalysis ictx							// Informazioni di contesto
							    , ArrayList<InnerTokenInstr> al_tokenSpreaded		// Tokens istruzione corrente
						        , int iFirstTokenNoKey								// Primo token dopo keys identificative istruzione
						         ) {

		InnerTokenInstr innerLastTokenInstr = null;				    // Descrittore ultimo token identificativo parola chiave istruzione
		String tokenCur = "";                                       // token corrente istruzione sotto esame
		int posStart = 0;											// Posizione un riga dalla quale iniziare la ricerca
		int iTokenStart = 0;										// Numero token dal quale iniziare la ricerca
		int rowCur = 0;                                             // Riga corrente da cui cercare la fine istruzioni
		int iTokenCur = 0;                                          // Indice corrente token
		
		
		
		// La ricerca inizia dal primo token successivo all'ultimo token identificativo dell'istruzione
		iTokenStart = iFirstTokenNoKey;

        
		// Scan righe sorgente a partire da quella corrente, posizione successiva a inizio istruzione
		while (rowCur < ictx.ar_RowsSource.length) {
						
			// Scan token su righe splittate sotto esame 
			// a partire dal token successivo a quello identificativo dell'istruzione corrente
			for (iTokenCur = iTokenStart; iTokenCur < al_tokenSpreaded.size(); iTokenCur++) {
				      
				// Estraggo token corrente e precedente  
				tokenCur = al_tokenSpreaded.get(iTokenCur).token;
 				
				// Fine istruzione con punto e virgola o altro impostato da --#SET TERMINATOR  
				if (tokenCur.equals(ictx.instructionTerminatorActive)) {
					return iTokenCur;
				}

			} // end-for

			// Sono stati trattati tutti i token disponibili senza individuare la fine
			// dell'istruzione corrente o l'inizio di una nuova istruzione.
			// Vengono accodati nuovi token dalle righe successive e si riprende la ricerca
			
			// La ricerca continua a partire dalla fine dell'ultimo token dell'ultima riga considerata. 
			iTokenStart = al_tokenSpreaded.size();	
			
			// Recupero riga e posizione ultimo token accodato
			innerLastTokenInstr = al_tokenSpreaded.get(al_tokenSpreaded.size() - 1);
			rowCur = innerLastTokenInstr.numRowSource;
			posStart = innerLastTokenInstr.posInRow + innerLastTokenInstr.token.length();;	 
			
			// Accodo i token già normalizzati resenti nelle successive SQL_SCRIPT_PREFETCH_PARSING_ROWS righe utili
			getTokensSpreaded(ictx, al_tokenSpreaded, rowCur, posStart, SQL_SCRIPT_PREFETCH_PARSING_ROWS, true);
			
			// Nessun ulteriore token accodato: fine sorgente
			if (al_tokenSpreaded.size() == iTokenStart) {
				break;
			}
			
		} // end-while

		
		return iTokenCur;
	}



	/*
	 * --------------------------------------------------------
	 * Accoda token istruzione a partire da riga e posizione.
	 * --------------------------------------------------------
	 * 
     * A partire dalla riga corrente, posizione corrente, restituisce una 
     * ArrayList con tutti i token estratti, indicando riga e posizione nella riga.
     * singola stringa con le successive numRowsDeep righe source accodate.
     * 
     * Viene così composta una unica stringa con l'istruzione da valutare
     * 
     */
	private int getTokensSpreaded(InnerContextAnalysis ictx						// Informazioni di contesto
								, ArrayList<InnerTokenInstr> al_tokenSpreaded	// Tokens estratti con info di origine
								, int numRowStart 								// Numero riga di inizio
								, int posStart									// Posizione di inizio in riga di inizio
								, int numRowsDeep								// Numero righe utili da considerare dopo quella di inizio
								, boolean areToGetNormalized                    // Se true i token vengono accodati già normalizzati
				  				 ) {
		
		InnerTokenInstr innerTokenInstr = null;					// Singolo token con parola chiave, istruzione, posizione, ...
		ArrayList<InnerTokenInstr> al_tokenNormalized = null;   //
		StringService ss = null;								// Gestore stringhe
		String row = "";										// Singola riga sorgente col 1-80
		String strToSplit = "";									// Porzione utile riga sorgente col 8-72
		int cntRows = 0;
        int iFirstRowNoComment = 0;
        int i = 0;
		
 
 		
 		//////////////////////////////////////////////////////////////////////////////
 		// (1) Gestione token inizio istruzione su ultima riga istruzione precedente
 		//////////////////////////////////////////////////////////////////////////////
 		
 		if (posStart > 0) {
 			row = ictx.ar_RowsSource[numRowStart];
 			if (posStart < row.length() 
 			&& !row.substring(posStart).trim().equals("")
 			&& !row.startsWith("--", posStart)) {
 				
 				strToSplit = row.substring(posStart);
 				// Estrazione di tutte le parole presenti nella riga
 				ss = new StringService(strToSplit.toUpperCase());  
 				ss._words();												// Estrazione parole
 				ss._wordsPos();												// Calcolo posizione in strToSplit
 	
 				// Scan token righe splittate sotto esame
 				for (int j = 1; j <= ss._wordsCount(); j++) {
 					
 					// Creazione descrittore token con nome row e pos
  					innerTokenInstr = new InnerTokenInstr();
 					innerTokenInstr.token = ss._word(j);
 					innerTokenInstr.numRowSource = numRowStart;
 					innerTokenInstr.posInRow = ss._wordPos(j) + posStart;
 					
					// Normalizzazione token in + token eventuali e accodamento in output
 					al_tokenNormalized = getTokensNormalizedFromSingleToken(innerTokenInstr);
   					al_tokenSpreaded.addAll(al_tokenNormalized);
 				}
  				numRowStart++;
			} else {
				numRowStart++;
			}
		}
 		
 		// Bypass commento ahead
 		iFirstRowNoComment = bypassRowsComment(ictx, numRowStart);
 		
 	  	
		////////////////////////////////////////////////////
		// (2) Scan righe e intabellamento token
		////////////////////////////////////////////////////
	    
		// Scan righe sorgente 
		for (i = iFirstRowNoComment; i < ictx.ar_RowsSource.length; i++) {
			
			row = ictx.ar_RowsSource[i];
			// Istruzioni embedded possono avere campi con -- e si possono scambiare per commenti
			if (ictx.isAnalysisOfScript) {
				row = eraseCommentOnRowIfAny(row);
			}
    		strToSplit = replaceSpaceInLiteralWithUnderscore(row);
    		
			// Estrazione di tutte le parole presenti nella riga
			ss = new StringService(strToSplit.toUpperCase());  
			ss._words();			// Estrazione parole
			ss._wordsPos();			// Calcolo posizione in strToSplit
			
			// Scan token righe splittate sotto esame
			for (int j = 1; j <= ss._wordsCount(); j++) {
				
				// Creazione descrittore token con nome row e pos
				innerTokenInstr = new InnerTokenInstr();
				innerTokenInstr.token = ss._word(j);
				innerTokenInstr.numRowSource = i;
				innerTokenInstr.posInRow = ss._wordPos(j);
				
				// Normalizzazione token in + token eventuali e accodamento in output
				al_tokenNormalized = getTokensNormalizedFromSingleToken(innerTokenInstr);
				al_tokenSpreaded.addAll(al_tokenNormalized);
			}
  
			cntRows++;
			
			// Accodati i token delle righe successive richieste
			if (cntRows > numRowsDeep) {
				break;
			}
			
			// Bypass righe commento in ciclo			
			iFirstRowNoComment = bypassRowsComment(ictx, i + 1);				 
            i = iFirstRowNoComment - 1;

		} // end-for righe sorgente

		
		// Si restituisce l'indice dell'ultima riga tokenizzata
		
		return i;
	}


	/*
	 * Eliminazione commento di riga
	 * 
	 * 
	 */
	private String eraseCommentOnRowIfAny(String row) {
		int iComment = 0;
	
		iComment = row.indexOf("--");
		
		// Riga senza nessun commento 
		if (iComment < 0) {
			return row;
		}	

		return row.substring(0, iComment);
	}


	/*
	 * Da implemenatre
	 * 
	 */
	private void finalSetInstructionInfo() {
	}



	/*
	 * 
	 *  Memorizza le informazioni di origine sorgente dell'istruzione
	 *  nell'istruzione generata
	 *  
	 */
	private void storeSourceInstructionInfo(Instruction instrSaved, InstructionSql instrToUpdate) {
	    
		// Caricamento informazioni sorgente nell' oggetto istruzione
		instrToUpdate.setNumInstr(instrSaved.getNumInstr());
     	instrToUpdate.setRowStartSource(instrSaved.getRowStartSource());
     	instrToUpdate.setRowEndSource(instrSaved.getRowEndSource()); 
    	instrToUpdate.setPosStartInstr(instrSaved.getPosStartInstr()); 
     	instrToUpdate.setPosEndInstr(instrSaved.getPosEndInstr()); 
    	instrToUpdate.setName(instrSaved.getName());   
    	instrToUpdate.setCommentsBefore(instrSaved.getCommentsBefore());
//    	instrToUpdate.setCommentsLeft(instrSaved.getCommentsLeft());
//    	instrToUpdate.setCommentsRight(instrSaved.getCommentsRight());
    	instrToUpdate.setRowsSource(instrSaved.getRowsSource());
    	instrToUpdate.setSourceInstr(instrSaved.getSourceInstr());
    	instrToUpdate.setTerminatedWithPoint(instrSaved.isTerminatedWithPoint());
 	}


	/*
	 * ---------------------------------
     * Update symbol table dello script
     * ---------------------------------
     * 
     * 
     * Aggiornamento delle Symbol table dell'oggetto ScriptSql, sulla base
     * degli utilizzi dei simboli usati nell'istruzione, che è stata
     * analizzata e che è pronta per l'uso.
     * 
     */
	private void updateScriptSqlXrefFromInstruction(
													  InnerContextAnalysis ictx				// Contesto corrente
													, Object instructionGeneric				// Istruzione analizzata
													  ) {
		
		Instruction instruction = null;
		Instruction.InnerSymbolEntry ar_symbolsDefinedInside[] = null;
		Instruction.InnerSymbolEntry ar_symbolsInput[] = null;
		Instruction.InnerSymbolEntry ar_symbolsOutput[] = null;
		String symbolName = null;
		DataItemQualifier qualifier = null;
		EnumSymbolType symbolType = null;
		
		
		// Cast a Instruction, da cui tutte le istruzioni ereditano
		instruction = (Instruction) instructionGeneric;

		// Recupero simboli istruzione definiti, in input e output
		ar_symbolsDefinedInside = instruction.symbolsDefinedInside();
		ar_symbolsInput = instruction.symbolsUsedInput();
		ar_symbolsOutput = instruction.symbolsUsedOutput();

		// Scan simboli definiti all'interno dell'istruzione (Section o label)
		for (Instruction.InnerSymbolEntry entry : ar_symbolsDefinedInside) {
			this.scriptSql.symbolDefinitionAddProc(entry.symbolName, this.curNumDefScript, entry.qualifier.getSymbolType());
		}

		// Scan simboli in input.
		for (Instruction.InnerSymbolEntry entry : ar_symbolsInput) {
			symbolName =  entry.symbolName;
			qualifier = entry.qualifier;
			symbolType = qualifier.getSymbolType();
			// Label, procedure, Columns name ....
			this.scriptSql.symbolAddXrefProcInput(symbolName, this.curNumDefScript, symbolType);
		}
		
		// Scan simboli in output.
		for (Instruction.InnerSymbolEntry entry : ar_symbolsOutput) {
			symbolName =  entry.symbolName;
			qualifier = entry.qualifier;
			symbolType = qualifier.getSymbolType();
			// Label, proc_internal, Data_item,....
			this.scriptSql.symbolAddXrefProcOutput(symbolName, this.curNumDefScript, symbolType);
		}
	}


	
	/* -------------------------------------
	 * Impostazione misure metriche di base.
	 * -------------------------------------
	 * 
	 * Le metriche dimensionali sui commenti e linee di codice
	 * logiche, sono gà state caricate al momento della lettura 
	 * del sorgente.
	 * 
	 * Questo metodo EFFETTUA il conteggio delle misure dimensionali
	 * dei sorgenti per quanto riguarda:
	 * 
	 * 1) Righe commeno
	 * 1) Righe a blank
	 * 1) Campi definiti
	 * 2) Literal 
	 * 
	 * 
	 */
	private void detectMetricsMeasureBasic() {
		
		ScriptSqlEntry[] ar_entryScript = null;
		ScriptSqlEntry entryScript = null;
		Instruction instruction = null;
		
	    // Contatori misure
	    long cntLinesBlank = 0;
	    long cntLinesComment = 0;
        int numLastRowInstrPrec = 0;	    

	    ar_entryScript = this.scriptSql.entriesScript();
		
		// Scan istruzionio definite
		for (int i = 0; i < ar_entryScript.length; i++) {
			
			entryScript = ar_entryScript[i];
			instruction = entryScript.getInstruction();
			cntLinesComment = cntLinesComment + instruction.getCommentsBefore().length;
			
			// Scan righe commento o a blank prima dell'istruzione
			for (int j = numLastRowInstrPrec; j < instruction.getRowStartSource(); j++) {
				
				// Riga vuota: conteggio misura
				if (this.ictx.ar_RowsSource[j].trim().equals("")) {
					cntLinesBlank++;
					continue;
				}
			
			}
			 
			numLastRowInstrPrec = instruction.getRowEndSource() + 1;		
		}

	
		// Update misure dimensionali (non sono rilevanti altre misure dimensionali)
		this.metricsScriptSql.setSizeInstr(this.scriptSql.entriesScript().length);  			// # Istruzioni
		this.metricsScriptSql.setSizeLinesBlank(cntLinesBlank);									//
		this.metricsScriptSql.setSizeLinesComment(cntLinesComment);								//			
		
	}

	/*
	 * -------------------------------------------------------------------------
	 * Adjust messaggistica in base ad analisi attiva di script o di programma
	 * -------------------------------------------------------------------------
	 * 
	 * 
	 */
	private void adjustMsgForScriptOrPgmAnalysys() {
		
		// Origine analisi per messaggi di errore
		if (this.scriptSqlName.equals("")) {
			this.strProgramScript = getMessage(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "TC0008");  // Program
			this.strProgramScriptObj = this.acp.getProgramName();
		} else {
			this.strProgramScript = getMessage(EnumModule.ANALYZER, EnumMessageType.INFORMATION, "TC0007");  // Sql Script
			this.strProgramScriptObj = this.scriptSqlName;
		}

	}
	
	


	/**
	 * 
	 * Restituisce l'oggetto che aggrega tutti gli aggiornamenti da fare
	 * sul db per l'analisi del programma in corso.
	 * 
	 * @return AnalyzerDbInfo updates db container
	 */
	public AnalyzerDbInfo getDbInfo() {
		return this.analyzerDbInfo;
	}

	
	
	
	/**
	 * Restituisce le informazioni di sistema/sottosistema<br>
	 * <p>
	 * @return the userExitInfoPgm
	 */
	public UserExitInfo getUserExitInfoPgm() {
		return userExitInfoPgm;
	}


	/**
	 * Imposta le informazioni di sistema/sottosistema<br>
	 * <p>
	 * @param userExitInfoPgm the userExitInfoPgm to set
	 */
	public void setUserExitInfoPgm(UserExitInfo userExitInfoPgm) {
		this.userExitInfoPgm = userExitInfoPgm;
	}

	/*
	 * Logging istruzioni errate con tutte le informazioni disponibili
	 * al momento del parsing
	 * 
	 * 
	 */
	private void loggingParsingErrorsWarning() {
		
		ScriptSqlEntry[] ar_entries = null;
		
		Instruction instrGeneric = null;

		ar_entries = this.scriptSql.entriesScript();
		
		// Scan Istruzioni  
		for (ScriptSqlEntry entryInstrucion : ar_entries) {
			
			instrGeneric = entryInstrucion.getInstruction();
			
			// Istruzione parsata e analizzata senza errori: continue
			if (!instrGeneric.isParsingError() 
			&&  !instrGeneric.isSemanticError()
			&&  !instrGeneric.isWarning()) {
				continue;
			}

			// Errori di parsing o di analisi semantica 
				
			// Messagio e informazioni generati al momento dell'intercettamento dell'errore
			if (di.optStackTraceOnParsingError) {
				logMessage(instrGeneric.getMsgType(), instrGeneric.getMsgCode(), instrGeneric.getExcpError(),  instrGeneric.getMsgParm());
			} else {
				logMessage(instrGeneric.getMsgType(), instrGeneric.getMsgCode(), instrGeneric.getMsgParm());
			}
			
			// Logging dettaglio istruzione in errore
			logMessage(EnumMessageType.INFORMATION
				 	 , "MI0044" 
					 , this.scriptSqlName
					 , instrGeneric.getSourceInstr()
					 , instrGeneric.getTokenInError()
					 , instrGeneric.getRowStartSource()+""
					 , instrGeneric.getPosStartInstr()+"" 
					 , instrGeneric.getRowEndSource()+""
					 , instrGeneric.getPosEndInstr() +""
			           );
			
		} // end-for
		
	}
  
		

	/*
     * 
     * 
     *  Restituisce il token successivo dopo aver verificato che esiste.
     *  Se non esiste restituisce ""
     * 
     */
	private String nextToken(Scanner scn)  {
		
		String nextToken = "";
		
		// Non ci sono altri token: return stringa vuota
		if (!scn.hasNext()) {
			return nextToken;
		} 
			
		nextToken = scn.next();
		
		// Situazione inconsistente
		if (this.strNoMemoryToParse.length() < nextToken.length()) {
			return nextToken;
		}
		
		// Stringa senza i token precedenti già trattati
		this.strNoMemoryToParse = this.strNoMemoryToParse.trim().substring(nextToken.length()).trim();
		return nextToken;
	}
	
	/*
     * 
     * 
     *  Restituisce il token successivo dopo aver verificato che esiste.
     *  Se non esiste restituisce ""
     * 
     */
	private String nextTokenNoMemory(Scanner scn)  {
		
		String newToken = "";
		
		// Non ci sono altri token: return stringa vuota
		if (!scn.hasNext()) {
			return newToken;
		} 
			
		newToken = scn.next();
		
		return newToken;
	}

	/*
     *  Restituisce il token successivo sullo scanner corrente
     *  senza alterarne il posizionamento
     * 
     */
	private String nextTokenWithoutScannerMod(Scanner scn)  {
		
		Scanner scnNext = null;
		String newToken = "";
		
		scnNext = new Scanner(this.strNoMemoryToParse);
		if (!scnNext.hasNext()) {
			scnNext.close();
			return "";
		}
		newToken = scnNext.next();
		scnNext.close();
		return newToken;
	}
	
	/*
     *  Restituisce true se il token corrente identifica una OLAP specification.
     *  Lo scanner fornito non viene alterato
     *  
     *  Si tratta di aggregate-function OVER ( ... )
     *  o RANK()       OVER ( ... )
     *  o DENSE_RANK() OVER ( ... )
     *  o ROW_NUMBER() OVER ( ... )
     * 
     */
	private boolean isSqlOlapSpecification(Scanner scn, String tokenCur)  {
		
		Scanner scnNext = null;
		String nextToken = "";
		
		// Sicuramente OLAP specification
		if (tokenCur.equals("RANK") 
		||  tokenCur.equals("DENSE_RANK") 		
		||  tokenCur.equals("ROW_NUMBER")) {
			return true;
		}

		// Sicuramente non OLAP specification
		if (!isSqlFunctionAggregate(tokenCur)) {
			return false;
		}
		
		// Se OLAP c'è il token OVER dopo la funzione aggregata
		
		scnNext = new Scanner(this.strNoMemoryToParse);
		nextToken = nextToken(scnNext);						// (
		extractStringBetweenPar(scnNext);		            //  ... )
		nextToken = nextToken(scnNext);						// OVER ?
		
		// OLAP specification
		if (nextToken.equals("OVER")) {
			return true;
		}
		
		return false;
	}
	
	

	/* ---------------------------------
     * Estrazione stringa fra parentesi
     * ---------------------------------
     * 
     * Viene restituita la stringa fra parentesi ( value-string )
     * 
     * Lo scanner è posizionato inizialmente sulla parentesi aperta,
     * Vengono bypassate le parentesi interne
     * Lo scanner viene restituito posizionato sul token dell'ultima parentesi chiusa
      * 
     */
	private String extractStringBetweenPar(Scanner scn) {
		
		StringBuffer sbBetweenPar = null;
		String token = "";
		int cntParOpen = 1;
		sbBetweenPar = new StringBuffer();
		
		token = nextToken(scn);					// Primo token dentro parentesi
		
		// caso (), niente fra parentesi
		if (token.equals(")")) {
			return "";
		}
		
		// caso ((, parentesi interna, incremento counter
		if (token.equals("(")) {
			cntParOpen++;
		}
		
		while (!token.equals("") && cntParOpen > 0) {
			sbBetweenPar.append(" " + token);
			token = nextToken(scn);	
			if (token.equals("(")) {cntParOpen++;}
			if (token.equals(")")) {cntParOpen--;}
		}
		return sbBetweenPar.toString().trim();
	}


	/*
	 * 
	 *  Eliminazione carattere finale se presente
	 * 
	 */
	private String deleteTrailingCharIfAny(String token, String charToDelete) {
		String tokenOut = "";
		
		// Non termina con crt fornito : Restituisco stringa originale
		if (!token.endsWith(charToDelete)) {
			return token;
		}
		
		// Eliminazione crt finale
		tokenOut = token.substring(0, token.length() - 1);
		return tokenOut;
	}

	/*
	 * 
	 *  Eliminazione carattere iniziale se presente
	 * 
	 */
	private String deleteLeadingCharIfAny(String token, String charToDelete) {
		String tokenOut = "";
		
		// Non termina con crt fornito : Restituisco stringa originale
		if (!token.startsWith(charToDelete)) {
			return token;
		}
		
		// Eliminazione crt finale
		tokenOut = token.substring(1);
		return tokenOut;
	}

	/*
	 * 
	 *  Eliminazione apici di inizio fine  strina
	 * 
	 */
	private String deleteTrailingLeadingApiceIfAny(String token) {
		String tokenOut = "";
		char singleApice = 39;					// Apice singolo '
		char doubleApice = 34;					// Doppio apice ""
		
		if (token.length() < 3) {
			return token;
		}

		tokenOut = token;

		// Literal
		if (token.charAt(0) == singleApice ||token.charAt(0) == doubleApice) {
			tokenOut = token.substring(1, token.length() - 1);
			tokenOut = tokenOut.substring(0, tokenOut.length());
		}
		
		return tokenOut;
	}

	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

	
	/*
	 *   Classe contenitore di servizio associato alla prima parola riservata di istruzioni Sql.
	 *   
	 *   Ogni entry contiene la sequenza di parole chiave che identificano un'istruzione Cics.
	 *   Più istruzioni Cics possono iniziare con la stessa parola, come:
	 *   handle Aid, Handle Abend, Handle condition
	 *   
	 *   
	 */
	private class InnerInstructionWordsEntry {
		
		public EnumPrecompilerReservedWords en_WordReservedOwner = null; // Enum parola riservata indicante l'istruzione
		public EnumInstrPrecompilerType typeEntry = EnumInstrPrecompilerType.NOT_ASSIGNED;  // Constant: istruzione, operando opzione o exception (EXEC_CICS_INSTRUCTION, ..)
		public ArrayList<String> al_wordKey = null;                		 // Sequenza completa di parole chiave valide identificanti l'istruzione 
		
        /*
         * Costruttore
         */
        private InnerInstructionWordsEntry() {
        	en_WordReservedOwner = EnumPrecompilerReservedWords.NOT_ASSIGNED;
        	al_wordKey = new ArrayList<String> ();
		}
	}

	/*
	 * 
	 * Classe contenitore con le informazioni correnti sull'analisi del sorgente corrente.
	 * 
	 * Le informazioni sono relative al contesto di uno specifico livello ricorsivo.
	 * Una istanza viene istanziata relativamente al programma sotto analisi. 
	 * Per ogni modulo copy viene creata una nuova istanza e attivata l'analisi, ricorsivamente. 
	 * Così a qualsiasi livello di ricorsività dei moduli copy.
	 * 
	 */
	public class InnerContextAnalysis implements Cloneable {
        
		// Identificazione origine analisi
		boolean isAnalysisOfScript = true;              // True indica elaborazione a fronte di analisi complessiva script sql
        EnumObject typeObjectOrigin = null;             // PGM_COBOL o SCRIPT_SQL
		String idObjectOrigin = "";                     // pgmName o scriptSql
        int numInstrOrigin = 0;                         // Numero istruzione origine in pgm cobol o script sql
		
		// Identificazione sorgente e istruzione
		SourceInput si = null;							// Descrittore completo sorgente sotto analisi
		EnumPrecompilerReservedWords activeTypeInstr = null;  // Tipologia istruzione codificata, come parola riservata di inizio
		EnumSourceType activeTypeSource = null;         // Tipologia sorgente sotto analisi
		int numRowInstructionTerminatorDetected = 0;    // Numero riga --#SET TERMINATOR  (a fronte di trigger)
		String instructionTerminatorDetected = "";      // Delimitatore di fine istruzione, impostato da --#SET TERMINATOR  (a fronte di trigger)
		String instructionTerminatorActive = ";";       // Delimitatore di fine istruzione attivo
		String[] ar_RowsSource = null;            		// Array righe sorgente  complessive
		String[] ar_RowsSourceInstruction = null;       // Array righe sorgente  istruzione corrente pacchettizzata. Include ebentuali altre istruzioni stessa riga
		String activeSourceName = "";                   // Nome sorgente in analisi, programma o copy al livello nesting corrente
		int activeInstrKeyWordsSize = 0;  	      		// Numero parole chiave che hanno identificato l'istruzione corrente
		InnerInstructionWordsEntry activeInstrKeyWordEntry = null; // Entry che identifica l'istruzione corrente
	    int numLastTokenPrecInstr = 0;					// Ultima parola istruzione precedente
	    int numLastTokenCurInstr = 0;					// Ultima parola istruzione corrente
	    int numFirstTokenNextInstr = 0;					// Prima parola istruzione successiva
		int numInstr = 0;                     	 		// Numero sequenza entry (0-based)
		int rowStartSource = 0;	                 		// Numero riga sorgente di analisi di inizio
		int rowEndSource = 0;	                 		// Numero riga sorgente di analisi di fine
		int posStartInstr = 0;	             			// Pos 0-based inizio istruzione in riga sorgente  
		int posEndInstr = 79;	                 		// Pos 0-based fine istruzione in riga sorgente   (include col 1-7)
		String nameInstr = null;                  		// Nome istruzione (es. IF)
		boolean isLiteralInProgress = false;            // True indica token dentro literal in identificazione istruzione successiva
		boolean isLiteralDoubleDelimited = false;       // Nome istruzione (es. IF)
	    String sourceInstr = "";			 		 	// Istruzione source estratta completa, senza punto finale
	    Object objectInstr = "";			 		 	// Oggetto Instruction codificato da sourceInstr
	    InstructionSql curInstrInfo = null;             // Contiene numeri riga e posizione istruzione corrente  
	    
		// Commenti istruzione corrente
	    ArrayList<String> al_CommentsBeforeInstr = null;// ArrayList righe commento precedenti l'istruzione  
	    ArrayList<String> al_RowsInstr = null;          // ArrayList con righe istruzione        
	    ArrayList<String> al_CommentsRightInstr = null; // ArrayList righe commento dopo di ogni istruzione         
		String[] ar_CommentsBeforeInstr = null;   		// Array righe commento precedenti l'istruzione
		String[] ar_CommentsRightInstr = null;    		// Array righe commento dopo di ogni istruzione

		// Campi e variabili di servizio per estrazione istruzione da righe sorgente
	    String token = "";						  		// Token di servizio
	    String curToken = "";							// Token corrente istruzione sotto analisi
	    String rowAll = "";								// Pos 1-80
	    String rowComm = "";							// Pos 1-80
	    String rowLeft = "";							// Pos 1-6
	    String rowRight = "";							// Pos 73-80
	    String rowInstr = "";               			// Pos 8-72
		String rowCont = "";                			// Pos 7		(continuazione)

	    // Stato analisi istruzione
	    boolean isAnyInstructionErrorDetected = false;  // True indica un errore di parsing durante l'analisi sorgenti o semantico o di ceazione grafo (semantico) 
	    boolean isAnyInstructionWarningDetected = false;// True indica un warning da segnalare a fonte dell'analisi istruzione
	    
	    // Metriche 
	    Metrics metrics = null;                         // Classe contenitore mtriche
    
	    
	    /*
	     * 
	     * Costruttore
	     * 
	     */
		public InnerContextAnalysis() {
			al_RowsInstr = new ArrayList<String>();       
			al_CommentsBeforeInstr = new ArrayList<String>();       
			al_CommentsRightInstr = new ArrayList<String>();   
			activeTypeInstr = EnumPrecompilerReservedWords.NOT_ASSIGNED; 
		    curInstrInfo = new InstructionSql();
		    metrics = new Metrics(ucfg, di);
		}


		/* (non-Javadoc)
		 * @see java.lang.Object#clone()
		 */
		@Override
		protected InnerContextAnalysis clone()  {
			InnerContextAnalysis ictxCloned = null;

			try {
				ictxCloned =   (InnerContextAnalysis) super.clone();
			} catch (CloneNotSupportedException e) {
				// Exception ne chiamanti
				ictxCloned = null;
			}
			
			// Inizializzazione variabili per nuovo contesto
			ictxCloned.activeInstrKeyWordsSize = 0;  	    
			ictxCloned.numLastTokenPrecInstr = 0;				 
			ictxCloned.numLastTokenCurInstr = 0;				 
			ictxCloned.numFirstTokenNextInstr = 0;					 
			ictxCloned.numInstr = 0;                     	 		 
			ictxCloned.rowStartSource = 0;	                 		 
			ictxCloned.rowEndSource = 0;	                 		 
			ictxCloned.posStartInstr = 7;	             			 
			ictxCloned.posEndInstr = 71;	                 		 
			return ictxCloned;
		}
	}

	
	/*
	 *   Classe contenitore di servizio per i singoli token estratti
	 *   dalle righe sorgente da valutare
	 *   
	 */
	private class InnerTokenInstr implements Cloneable{
		
		// Valori di identificazione token  
		String token = "";                           		  // Parola corrente estratta dalla riga sorgente
		int numRowSource = 0;                                 // Numero corrente riga sorgente di appartenenza
		int posInRow = 0;                                     // Posizione corrente in riga 0-based da colonna 1
		/**
		 * @return
//		 * @see java.lang.String#toString()
		 */
		public String toString() {
			return token.toString();
		}
		/* (non-Javadoc)
		 * @see java.lang.Object#clone()
		 */
		@Override
		protected Object clone()  {
			try {
				return super.clone();
			} catch (Exception e) {
				return null;
			}
		}      
	}	
}
