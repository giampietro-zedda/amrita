package analyzer;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;

import utilities.DateTimeService;
import utilities.ReflectionManager;
import enums.EnumAmritaExceptionError;
import enums.EnumCobolReservedWords;
import enums.EnumDirectivesExecution;
import enums.EnumInstrDataCategory;
import enums.EnumInstrPrecompilerType;
import enums.EnumMessageType;
import enums.EnumPrecompilerReservedWords;
import enums.EnumSourceType;
import enums.EnumUserExit;
import exception.ExceptionAmrita;

/**
  * Copyright (c) 2009-2010 e-Amrita - Ing. Giampietro Zedda Turin (ITALY)
  * 
 * <h1>
 * SourceManager  
 * </h1>
 *  <p>
 * Questa classe gestisce tutte le operazioni di scrittura, recupero e classificazione
 * dei sorgenti, in un formato pronto per l'analisi.
 * <p>
 * Viene restituito un sorgente di una specifica directory, oppure
 * un elenco di sorgenti, codificato da  oggetti della classe {@link SourceInput}.<br>
 * Il metodo <b>listFilesFromPilot</b> restituisce sempre un array di oggetti {@link SourceInput}<br>
 * ma utilizza in input il file di pilota sorgenti descritto dall'enumerazione
 * {@link EnumDirectivesExecution}<br>
 * Attraverso questa enumerazione è possibile programmare estrazioni di sorgenti complesse,
 * su directory diverse, di files specifici e con la contestuale classificazione del
 * tipo sorgente. Vengo rstituiti solo i sorgenti del tipo richiesto<br>
 * E' inoltre possibile specificare criteri di filtro multipli che devono essere soddisfatti
 * in AND sullo stesso nome sorgente e inserirne altri , che vengono soddisfatti in OR.
 * Sul nome sorgente è possibile quindi specificare criteri, per esempio indicando che il
 * primo carattere deve valer "A" e contestualmente gli ultimi due "FG" oppure una qualsiasi
 * altra combinazione.<br>
 * Sempre dal nome sorgente, o in modo esplicito,  è possibile estrarre il codice del sistema
 * e del sottosistema del quale il sorgente in oggetto fa parte<br>
 * Nel caso le operazioni suddette non fossero sufficienti è possibile richiamare una exit esterna,
 * codificata come un metodo di una clase dichiarata a livello di configurazione,
 * per ottenere, a partire dal nome sorgente, i criteri di filtro, il sistema di appartenenza
 * e il sottosistema.<br>
 * <p>
 * Segue un esempio di utilizzo delle direttive nel file pilota di esecuzione:
 * 
				{@code}
				<p>
				SEARCH_RECURSIVE_ENABLED   <br>
				OPT_DETECT_SOURCE_TYPE_ENABLED <br>
				
				
				#--------------------------------------------------------------------------<br>
				#--------------------------------------------------------------------------<br>
				# Estrazione Copy Cobol<br>
				#--------------------------------------------------------------------------<br>
				#--------------------------------------------------------------------------<br>
				
				# G/GA<br>
				SYSTEM_VALUE G<br>
				SUB_SYSTEM_VALUE GA<br>
				FILTER_ON_SOURCE_NAME 1 GA<br>
				SOURCE_TYPE_COPY_COBOL_PROC <br>
				SOURCE_TYPE_COPY_COBOL_DATA 
				LIBRARY LIB1 G:\SPaolo\Sources\X400DB2.TS000.COPYCOB\ <br>
				OBJECTS_IDENTIFICATION_UNIT
				
				# G/HG<br>
				SUB_SYSTEM_VALUE HG<br>
				FILTER_CLEAR<br>
				FILTER_ON_SOURCE_NAME 1 HG<br>
				LIBRARY LIB2 G:\SPaolo\Sources\X400DB2.TS000.COPYCOB\<br>
				OBJECTS_IDENTIFICATION_UNIT
				
				# G/GY <br>
				SUB_SYSTEM_VALUE GY <br>
				FILTER_CLEAR <br>
				FILTER_ON_SOURCE_NAME 1 GY <br>
				LIBRARY LIB3 G:\SPaolo\Sources\X400DB2.TS000.COPYCOB\ <br>
				OBJECTS_IDENTIFICATION_UNIT
				
				
				#--------------------------------------------------------------------------<br>
				#--------------------------------------------------------------------------<br>
				# Estrazione programmi<br>
				#--------------------------------------------------------------------------<br>
				#--------------------------------------------------------------------------<br>
				
				# G/GA <br>
				SYSTEM_VALUE G <br>
				SUB_SYSTEM_VALUE GA <br>
				FILTER_ON_SOURCE_NAME 1 GA <br>
				SOURCE_TYPE_EXCLUDE_ALL <br>
				SOURCE_TYPE_INCLUDE_ALL <br>
				LIBRARY LIB4 G:\SPaolo\Sources\LTM0G.SAE.SOURCE\ <br>
				OBJECTS_IDENTIFICATION_UNIT
				
				# G/HG <br>
				SUB_SYSTEM_VALUE HG <br>
				FILTER_CLEAR <br>
				FILTER_ON_SOURCE_NAME 1 HG <br>
				LIBRARY LIB4 G:\SPaolo\Sources\LTM0G.SAE.SOURCE\ <br>
				OBJECTS_IDENTIFICATION_UNIT
				
				# G/GY <br>
				SUB_SYSTEM_VALUE GY <br>
				FILTER_CLEAR <br>
				FILTER_ON_SOURCE_NAME 1 GY <br>
				LIBRARY LIB4 G:\SPaolo\Sources\LTM0G.SAE.SOURCE\ <br>
				OBJECTS_IDENTIFICATION_UNIT
				
			    FUNCTION_DETECT_SOURCE_TYPE
			    
			    START
				
 *  
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 15/02/2010
 * @see Analyzer
 * @see SourceInput
 *   
 */
 
public class SourceManager implements AmritaConstants{
	
	///////////////////////////////////////////////////////////////////////////////////
	// Reference a oggetti di default di uso comune
	///////////////////////////////////////////////////////////////////////////////////
	
    private UserConfiguration ucfg = null;          // Defaults e references globali come gestore messaggi, log etc	
	private LoggerFacade lf = null;  		        // Gestore scrittura su log
	private Object ue = null;                       // User exit il cui nome è specificato in configurazione
	private ReflectionManager rm = null;            // Gestore generalizzato operazioni di reflection
	
	
	///////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanzs generali
	///////////////////////////////////////////////////////////////////////////////////
	 
	// Valori correnti generali e di direttiva
    private ExecutionDirectives di = null;          		// Informazioni direttive correnti (FUNCTION_....o APPEND_DIRECTIVES_..)
	

	///////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanzs di servizio
	///////////////////////////////////////////////////////////////////////////////////
	
    // directories - Files input: #1# Files output: #2# Files scartati: #3#
	// Contatori generali
	private int cntDirTot = 0;             			 // Numero directories esaminate
	private int cntSourcesInput = 0;                 // Numero sorgenti input totali
	private int cntSourcesOutput = 0;                // Numero sorgenti identificati totali
	private int cntSourcesDiscarded = 0;             // Numero sorgenti scartati, che non hanno superato i criteri di filtro

	// Varie
	private UserExitInfo userExitInfo = null;				// Informazione da/per exit applicativa

	// Parole riservate per identificazione istruzione Sql.
    // Key  = parola valida di inizio istruzione.
    // Dati = ArrayList di insiemi di sequenze di parole identificanti una istruzione valida
    // La Map viene caricata dal costruttore a partire da EnumCobolReservedWords
    public Map<String, ArrayList<InnerInstructionWordsEntrySql>> map_SqlReservedWords;	

    // Parole riservate per identificazione istruzione.
    // Key  = parola valida di inizio istruzione o operando o operatore etc.
    // Dati = ArrayList di insiemi di sequenze di parole identificanti una istruzione valida
    // Con questa Map si gestisce l'individuazione di qualsiasi tipo di istruzione.
    // La Map viene caricata dal costruttore a partire da EnumCobolReservedWords
    public Map<String, ArrayList<InnerInstructionWordsEntryCobol>> map_ReservedWords;	


    // Flag individuazione pgm/copy Cobol
    private boolean isThereIdDivision = false;
    private boolean isThereEnvDivision = false;
    private boolean isThereDataDivision = false;
    private boolean isThereProcDivision = false;
    private boolean isThereFileSection = false;
    private boolean isThereWsSection = false;
    private boolean isThereLinkageSection = false;
	private boolean areThereDataDefinitions = false;
	private boolean areThereProcInstructions = false;

	
	/**
	 * Costruttore vuoto
	 */	
	public SourceManager() {
		super();
	}


	/**
	 * Costruttore 
	 */
	public SourceManager(UserConfiguration sd) {
		super();
		this.ucfg = sd;
		this.lf = sd.getLoggerFacade();
		this.rm = new ReflectionManager();
		
		// Caricamento map per riconoscimento script sql con ddl statements
		this.map_SqlReservedWords = new HashMap<String, ArrayList<InnerInstructionWordsEntrySql>>(500);
		 	
		// Scan enumerazione con parole riservate Sql gestite
		for (EnumPrecompilerReservedWords en_reservedWord : EnumPrecompilerReservedWords.values()) {
			
			// Interessaasno solo le Sql DDL
			if (en_reservedWord.getTypeEntry() != EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL) {
				continue;
			}
			putMapReservedWordsSql(en_reservedWord); 				// -> map_ReservedWords
		}		

		this.map_ReservedWords = new HashMap<String, ArrayList<InnerInstructionWordsEntryCobol>>(1000);

		// Scan enumerazione con parole riservate Cobol gestite
		for (EnumCobolReservedWords en_reservedWord : EnumCobolReservedWords.values()) {
			if (en_reservedWord == EnumCobolReservedWords.NOT_ASSIGNED) {
				continue;
			}
			putMapReservedWordsCobol(en_reservedWord); 				// -> map_ReservedWords
		}
	}

	
	/*
	 * 
	 * Inserimento in map parole riservate Cobol, con collegamento alla EnumCobolResevedWords
	 * principale che identifica l'istruzione o direttiva o parola chiave figurativa etc.
	 * 
	 */
	private void putMapReservedWordsCobol(EnumCobolReservedWords en_reservedWord) {

		ArrayList<String> al_wordKeyDefinition = null;					// Parole chiave valide come inizio di istruzione
		ArrayList<String> al_wordKeySequence = null;					// Sequenza di parole chiave valide identificanti un0istruzione  
		
		
		al_wordKeyDefinition = new ArrayList<String> ();
		al_wordKeySequence = new ArrayList<String> ();
				
		// Caricamento valori singole parole, che possono essere multiple e opzionali (se precedute da "|")
		
		// Key word Label e Section NON possono essere riconosciute da una prima parola chiave
		if (en_reservedWord.getValueText1().equals("")) {
			return;		 
		}
		al_wordKeyDefinition.add(en_reservedWord.getValueText1());		// Per esmpio IF 
		if (!en_reservedWord.getValueText2().equals("")) {
			al_wordKeyDefinition.add(en_reservedWord.getValueText2());		// Per esmpio DIVISION di ID DIVISION
		}
		if (!en_reservedWord.getValueText3().equals("")) {
			al_wordKeyDefinition.add(en_reservedWord.getValueText3());		// Per esmpio SIZE di NOT ON SIZE ERROR
		}
		if (!en_reservedWord.getValueText4().equals("")) {
			al_wordKeyDefinition.add(en_reservedWord.getValueText4());		// Per esmpio ERROR di NOT ON SIZE ERROR
		}
		
		// Estrazione ricorsiva sequenze corrette di parole chiave
		
	    extractKeyRecursive(en_reservedWord, al_wordKeyDefinition, 0, al_wordKeySequence);		// -> map_ReservedWords
		
	}

    /*
     * Analisi ricorsiva singola parola chiave e inserimento in map
     * di tutte le possibili sequenze di chiave corrette per l'istruzione
     * 
     */
	@SuppressWarnings("unchecked")
	private void extractKeyRecursive(
									  EnumCobolReservedWords en_reservedWord
									, ArrayList<String> al_wordKey
									, int iStart
									, ArrayList<String> al_wordKeySequence
									) {
		
		InnerInstructionWordsEntryCobol reservedSubWordEntry = null;
		ArrayList<InnerInstructionWordsEntryCobol> al_reservedSubWordEntry = null;
		ArrayList<String> al_wordKeySequenceRecursive = null;
		
		String ar_valueKeyOptional[] = null;							// Valori chiave opzionali di ogni parola chiave
		String wordKey = ""; 
		String wordKeyFirst = ""; 
	
		// Scan stringhe descrittori parole chiave
		
		for (int i = iStart; i < al_wordKey.size(); i++) {
			
			wordKey = al_wordKey.get(i);
			
			// Non definita: skip
			if (wordKey.equals("")) {
				break;
			}
			
			// Ci sono valori opzionali nella posizione: attivazione ricorsiva
			if (wordKey.indexOf("|") > 0) {
				wordKey = wordKey.replace('|', ':');
				ar_valueKeyOptional = wordKey.split(":");
				// Scan valori opzionali
				for (String valueKeyOptional : ar_valueKeyOptional) {
					al_wordKeySequenceRecursive = (ArrayList<String>) al_wordKeySequence.clone();
					// Space indica opzionale: non deve essere inserito
					if (!valueKeyOptional.trim().equals("")) {
						al_wordKeySequenceRecursive.add(valueKeyOptional);
					}
					extractKeyRecursive(en_reservedWord, al_wordKey, i+1, (ArrayList<String>) al_wordKeySequenceRecursive);
				}
				return;
			}
		
			// C'è un valore chiave singolo nella posizione: accodo in ArrayList di output
			al_wordKeySequence.add(wordKey);
		}
		
		// Inserimento Sequenza valori chiave validi in map
		// La chiave è il primo valore della sequenza di chiavi
		
		wordKeyFirst = al_wordKeySequence.get(0);
		al_reservedSubWordEntry = map_ReservedWords.get(wordKeyFirst);
		if (al_reservedSubWordEntry == null) {
			al_reservedSubWordEntry = new ArrayList<InnerInstructionWordsEntryCobol> ();
			map_ReservedWords.put(wordKeyFirst, al_reservedSubWordEntry);
		}
		
		// Creazione entry per nuova sequenza di valori chiave
		reservedSubWordEntry =  new InnerInstructionWordsEntryCobol();
		reservedSubWordEntry.en_WordReservedOwner = en_reservedWord;
		reservedSubWordEntry.en_InstrCategory = en_reservedWord.getCobolInstrCategory();
		reservedSubWordEntry.wordKeyFirst = wordKeyFirst;
		reservedSubWordEntry.al_wordKey = al_wordKeySequence;
		
		// Accodamento a ArrayList associata al primo valore chiave
		al_reservedSubWordEntry.add(reservedSubWordEntry);
	}


	
	
	/*
	 * 
	 * Inserimento in map parole riservate Sql, con collegamento alla EnumSqlResevedWords
	 * principale che identifica l'istruzione o parola chiave 
	 * 
	 */
	private void putMapReservedWordsSql(EnumPrecompilerReservedWords en_reservedWord) {

		ArrayList<String> al_wordKey = null;											// Parole chiave valide come inizio di istruzione
		ArrayList<InnerInstructionWordsEntrySql> al_instructionsWordsEntry = null;     	// Mapping Insieme di sequenze valide di chiavi
		InnerInstructionWordsEntrySql instructionsWordsEntrySql = null;      			// Singola sequenza
		String keyMap = "";                                                         	//
		
		al_wordKey = new ArrayList<String> ();
				
		keyMap = en_reservedWord.getValueText1();
		al_wordKey.add(keyMap);		    							// Per esempio CREATE 
		
		if (!en_reservedWord.getValueText2().equals("")) {
			al_wordKey.add(en_reservedWord.getValueText2());		// Per esempio GLOBAL 
		}
		
		if (!en_reservedWord.getValueText3().equals("")) {
			al_wordKey.add(en_reservedWord.getValueText3());		// Per esempio TEMPORARY 
		}
		
		if (!en_reservedWord.getValueText4().equals("")) {
			al_wordKey.add(en_reservedWord.getValueText4());		// Per esempio TABLE 
		}
		
		// Sequenza di parole individuante l'istruzione sql DDL
		instructionsWordsEntrySql = new InnerInstructionWordsEntrySql();
		instructionsWordsEntrySql.en_WordReservedOwner = en_reservedWord;
		instructionsWordsEntrySql.typeEntry = en_reservedWord.getTypeEntry();
		instructionsWordsEntrySql.al_wordKey = al_wordKey;
		
		// Recupero insieme di sequenze valide per la parola chiave di inizio istruzione
		al_instructionsWordsEntry = this.map_SqlReservedWords.get(en_reservedWord.getValueText1());
		if (al_instructionsWordsEntry == null) {
			al_instructionsWordsEntry = new ArrayList<InnerInstructionWordsEntrySql> ();
		}
		
		// Aggiorno la map con le sequenze valide aggiornate
		al_instructionsWordsEntry.add(instructionsWordsEntrySql);
		this.map_SqlReservedWords.put(en_reservedWord.getValueText1(), al_instructionsWordsEntry);
	}


	/**
	 * Restituisce oggetto SourceInput con il sorgente specifico richiesto
	 * se viene restituito null significa file non trovato o errore di accesso.
	 * 
	 * 
	 * @param String dirSource
	 * @param String idSource
	 * @param String prefixSource
	 * @param boolean bDetectSourceType
	 */
	public SourceInput getSource(String dirSource
								,String idSource
								,String suffixSource
								,boolean bDetectSourceType
								,boolean bInfoFileCompleteToGet
								)  {
		
		FileReader fr = null;                 // Reader
		BufferedReader br = null;             // Buffered reader
		SourceInput sourceInput = null;       // Descrive il sorgente
		String rowSource = "";                // 
		String pathComplete = "";             // Path completo
		ArrayList<String> alRowSource = null; // 
		String arRowSource[] =null;           //
		
		// Composizione nome file completo e allocazioni
		pathComplete = dirSource;
		pathComplete = pathComplete + File.separator + idSource;
		if (!suffixSource.equals("")) {
			pathComplete = pathComplete + "." + suffixSource;
		}
		alRowSource = new ArrayList<String>(ucfg.getSizeAvgSource());

		try {
				fr = new FileReader(pathComplete);
				br = new BufferedReader(fr);
				alRowSource = new ArrayList<String>();
			
				// Leggo il sorgente e popolo array list
				while ((rowSource = br.readLine()) != null) {
					alRowSource.add(rowSource);
				}
			
		} catch (FileNotFoundException e) {	
	        	String strMsg[] = new String[1];
	        	strMsg[0] = pathComplete;
//				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0002", strMsg, null);
	 			return null;
	 			
		} catch (IOException e) {
	        	String strMsg[] = new String[1];
	        	strMsg[0] = pathComplete;
	        	AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0003", strMsg, null);
				return null;
		}
		
		
		// Creo Array righe sorgente
		arRowSource = new String[alRowSource.size()];
		arRowSource = alRowSource.toArray(arRowSource);
		sourceInput = new SourceInput(null);
		
		// Informazioni base
		sourceInput.setIdSource(idSource);
		sourceInput.setSourceSuffix(suffixSource);
		sourceInput.setDirInput(dirSource);
		sourceInput.setPathComplete(pathComplete);
		
		// Informazioni complete file solo se richiesto (size, data etc.)
		if (bInfoFileCompleteToGet) {
			sourceInput = fileInfo(dirSource, idSource, suffixSource);
		}
		
		// Valorizzo tipologia sorgente e array di stringhe
		sourceInput.setArrayRowSource(arRowSource);
		if (bDetectSourceType) {
			detectSourceType(sourceInput);
		}

		return sourceInput;
	}

	
	/**
	 * Restituisce oggetto SourceInput con il sorgente specifico richiesto
	 * se viene restituito null significa file non trovato o errore di accesso.
	 * Nell'oggetto SourceInput vengono anche restituite tutte le informazioni 
	 * sul sorgente
	 * 
	 * @param String pathSourceComplete
     * @param boolean bDetectSourceType
	 */
	 public SourceInput getSource(String pathSourceComplete
								,boolean bDetectSourceType
								,boolean bInfoFileCompleteToGet
								)  {
		
		int iPoint = 0;
		int iPathSep = 0;
		String dirSource = "";
		String idSource = "";
		String prefixSource = "";
		SourceInput sourceInput = null;
		
		////////////////////////////////////////////////////////////////////////////////////////////////////
		// (0) Individuazione inizio nome file e inizio prefisso (-1)
		////////////////////////////////////////////////////////////////////////////////////////////////////

		iPoint = pathSourceComplete.lastIndexOf(".");
		iPathSep = pathSourceComplete.lastIndexOf("/");
        if (iPathSep < 0) {
        	iPathSep = pathSourceComplete.lastIndexOf("\\");
		}
        if (iPathSep < 0) {
        	iPathSep = pathSourceComplete.lastIndexOf(":");
		}
        
		////////////////////////////////////////////////////////////////////////////////////////////////////
		// (1) Path normale CON prefisso finale preceduto da '.'
		////////////////////////////////////////////////////////////////////////////////////////////////////
		
		if ((iPoint >= 0) && (iPathSep >= 0) && (iPoint > iPathSep) ) {
			prefixSource = pathSourceComplete.substring(iPoint+1);
			dirSource = pathSourceComplete.substring(0, iPathSep);			
			idSource = pathSourceComplete.substring(iPathSep+1, iPoint);
			sourceInput = getSource(dirSource, idSource, prefixSource, bDetectSourceType, bInfoFileCompleteToGet);
			return sourceInput;
		}
	
		////////////////////////////////////////////////////////////////////////////////////////////////////
		// (2) Path normale SENZA pefisso finale
		////////////////////////////////////////////////////////////////////////////////////////////////////
		
		if ( (iPoint < 0) && (iPathSep >=0) 
		|| ((iPoint > 0) && (iPathSep >=0) && iPathSep > iPoint  ) ) {
			prefixSource = "";
			dirSource = pathSourceComplete.substring(0, iPathSep+1);  //GPZ
			dirSource = pathSourceComplete.substring(0, iPathSep);
			idSource = pathSourceComplete.substring(iPathSep+1);
			sourceInput = getSource(dirSource, idSource, prefixSource, bDetectSourceType, bInfoFileCompleteToGet);
			return sourceInput;
		}
		
		return null;
	}
	
	/**
	 * Produce un file di output identificato dal parametro pathOutput
	 * con le righe memorizzate nel parametro descrittore file sourceInputDescriptor.
	 * 
	 * @param String pathOutput
     * @param String arRows[]  con righe da mandare sul file
	 * @throws ExceptionAmrita 
	 */
	public boolean writeSource(String pathOutput, String arRows[]) throws ExceptionAmrita  {
		boolean bWriteStatus = true;
		FileWriter fileOut = null;
		PrintWriter pw = null;  
        ExceptionAmrita excp = null;
		String row = "";
		
		try {
			
			// Creo un oggetto FileWriter e Lo incapsulo in un BufferedWriter 
			fileOut = new FileWriter(pathOutput);
			BufferedWriter out = new BufferedWriter(fileOut);
			pw = new PrintWriter(out);
		
			// Scan righe da scrivere
			for (int i = 0; i < arRows.length; i++) {
				
				row = arRows[i] == null ? "" : arRows[i];
				pw.println(row);
				
				// Scarto righe a null (lascio solo la prima)
				/*
				if (row.equals("")) {
					for (; i < arRows.length; i++) {
						if (arRows[i] != null) {
							break;
						}
					}
					i--;
				}
				*/
			}
            
			// Chiusura file
			pw.close(); 
			
		} catch (IOException e) {
			bWriteStatus = false;
        	String strMsg[] = new String[1];
        	strMsg[0] = pathOutput;
			excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_I_O_FILE_SYSTEM, e);
			AmritaStartup.lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0006", strMsg, excp);
		    throw excp;
			
		}
		return bWriteStatus;
	}

	
	/**
	 * Restituisce informazioni complete sul file.
	 * Vengono usati le informazioni separate di directory, nome file e prefisso.
	 * se il file non esiste restituisce null
	 * 
	 * @param String dir 
	 * @param String idFile 
	 * @param String prefix
	 */
	public SourceInput fileInfo(String dir, String idFile, String prefix) {
		SourceInput sourceInfo = null;                // Source  Input Information
		String pathComplete = null;                   // Path composto completo
		File file = null;
		
		// Composizione nome completo file da interrogare e accesso
		pathComplete = dir;
		pathComplete = pathComplete + "/" + idFile;		
		if (!prefix.equals("")) {
			pathComplete = pathComplete + "." + prefix;
		}
		file = new File(pathComplete);

		// Il file non esiste
		if (!file.exists()) {
			return null;
		}
		 
		sourceInfo = new SourceInput(null);
		sourceInfo.setDirInput(dir);
		populateSourceInfoFromObjFile(sourceInfo, file);

		return sourceInfo;
	}



	/**
	 * Restituisce informazioni complete sul file.
	 * Viene fornito in input il path completo del file.
	 * se il file non esiste restituisce null
	 * 
	 * @param String fileInput 
	 */
	public SourceInput fileInfo(String fileInput) {
		SourceInput sourceInfo = null;                // Source  Input Information
		File file = null;
		int iPoint = 0;
		int iPathSep = 0;
		String dirSource = "";
		
		// Oggetti di input/output
		file = new File(fileInput);
		sourceInfo = new SourceInput(null);
		
		// Il file non esiste
		if (!file.exists()) {
			sourceInfo.setExists(false);
			return sourceInfo;
		}
 		
		// Carico le informazioni da oggetto File(non c'è la directory)
		populateSourceInfoFromObjFile(sourceInfo, file);

		
		// Estrazione directory dal nome file completo

		iPoint = fileInput.lastIndexOf(".");
		iPathSep = fileInput.lastIndexOf("/");
        if (iPathSep < 0) {
        	iPathSep = fileInput.lastIndexOf("\\");
		}
        if (iPathSep < 0) {
        	iPathSep = fileInput.lastIndexOf(":");
		}
        
		if ((iPoint >= 0) && (iPathSep >= 0) && (iPoint > iPathSep) ) {
			dirSource = fileInput.substring(0, iPathSep+1);
			if (sourceInfo.isDirectory()) {  //GPZ
				dirSource = fileInput;        //GPZ
			}                                 //GPZ
		}
	
		if ( (iPoint < 0) && (iPathSep >=0) 
		|| ((iPoint > 0) && (iPathSep >=0) && iPathSep > iPoint  ) ) {
			dirSource = fileInput.substring(0, iPathSep+1);
			if (sourceInfo.isDirectory()) {   //GPZ
				dirSource = fileInput;        //GPZ
			}                                 //GPZ
		}
		
		sourceInfo.setDirInput(dirSource);
		return sourceInfo;
	}
	
	/**
	 * Restituisce l'elenco dei file della directory come array di oggetti String.
	 * 
	 * 
	 * @param String pathDir 
	 * @throws Exception  
	 */
	public String[] listFiles(String pathDir)  {
		
		String[] arFiles = null;
		File file = null;
		
		file = new File(pathDir);
		
		// Il file non esiste
		if (!file.exists()) {
			return arFiles;
		}
		// Deve essere una directory
		if (!file.isDirectory()) {
			return arFiles;
		}
		
		// Leggo tutti i files della directory 
		arFiles = file.list();
		return arFiles;
	}
	
	/**
	 * Restituisce un arrayList con il contenuto del fle specificato
	 * 
	 * 
	 * @param String path
	 * @throws Exception  
	 */
	static public List<String> getFile(String path)  {
		
		List<String> listFiles = new ArrayList<String>();
				
		try {
			listFiles = Files.readAllLines(Paths.get(path),StandardCharsets.ISO_8859_1);
		} catch (IOException e) {
			// Empty list
			if (!(e instanceof NoSuchFileException) ){
				e.printStackTrace();
			}
		}
		return listFiles;
	}
	
	
	/**
	 * Scrive un file al path specificato
	 * 
	 * 
	 * @param String path
	 * @param List<String> rows
	 * @throws IOException 
	 */
	public String putFile(String pathInput, List<String> rows)  {		
		StringBuilder sb = new StringBuilder();
		String retCode="OK";
				
 	
  		Path path = Paths.get(pathInput);	
		try(BufferedWriter writer = Files.newBufferedWriter(path, Charset.forName("UTF-8"))){
			for (String row : rows) {
				sb.append(row);
				sb.append(System.lineSeparator());
			}	
			writer.write(sb.toString());
			writer.flush();
			writer.close();

						
		}catch(IOException ex){
			ex.printStackTrace();
			retCode="KO"+ex.getMessage();
			
		}		
 
		return retCode;
	}
	
	/**
	 * Scrive un file al path specificato.
	 * Il file viene chiuso con il line feed di sistema
	 * 
	 * 
	 * @param String path
	 * @param String strng
	 * @throws IOException 
	 */
	public String putFile(String dirOutput, String fileName, String suffix, String strng)  {		
		String retCode="OK";
					
  		Path path = Paths.get(dirOutput + File.separator + fileName + "." + suffix);	
		try(BufferedWriter writer = Files.newBufferedWriter(path, Charset.forName("UTF-8")))
		{
			writer.write(strng);
			writer.flush();
			writer.close();
		} catch(IOException ex){
			ex.printStackTrace();
			retCode="KO"+ex.getMessage();
			
		}		
 		
		return retCode;			
	}		
 	

	/**
	 * Deleta un file al path specificato
	 * 
	 * 
	 * @param String path
	 */
	public String deleteFile(String pathInput)  {		
		Path path = Paths.get(pathInput);
		String retCode="OK";
		
		try {
			if (!Files.deleteIfExists(path)) {
				retCode+="NOTFOUND";
			}
		} catch (IOException e) {
			retCode="KO"+e.getMessage();
			e.printStackTrace();
		}		
		return retCode;
	}

	/**
	 * Restituisce l'elenco dei file della directory come array di oggetti SourceInput.
	 * Necessita in input del path completo della directory.
	 * Non viene fatto nessun controllo sui files estratti e non viene attivato nessun filtro.
	 * Restituisce null se il file non esiste o non è una directory o è una directory vuota.
	 * I due parametri booleani pilotano l'attivazione ricorsiva di directories e il porting o
	 * meno in output del file di directory.
	 * 
	 * 
	 * @param String pathDir 
	 * @param boolean bSearchRecursive
	 * @param boolean bDirOnOutput
	 * @throws Exception  
	 */
	public SourceInput[] listFiles(
								   String pathDir
                                  ,boolean bSearchRecursive
                                  ,boolean bDirOnOutput
                                  ) throws Exception  {
		
		SourceInput arSourceInput[] = null;
		Set<SourceInput> set_SourceInput = null;
		set_SourceInput = new HashSet<SourceInput>();
		
		// Attivazione ricorsiva
		listFilesOnDirRecursive(pathDir, "", "", set_SourceInput, bSearchRecursive, bDirOnOutput);
		
		// Converto in array e porto in output
		arSourceInput = new SourceInput[set_SourceInput.size()];
		arSourceInput = set_SourceInput.toArray(arSourceInput);
		return arSourceInput;
	}
	



	
	/**
	 * Restituisce tutti i file a partire da un file pilot di cui deve essere fornito il path completo. 
	 * Il file pilota contiene direttive per il recupero dei sorgenti e di filtro degli stessi. <br>
	 * <p>
	 * Il file pilota dei sorgenti può contenere direttive per la valorizzazione del sistema e del
	 * sottosistema. Tali assegnazioni vengono caricate nell'oggetto InfoFile di ogni file
	 * portatto in output e sono valide fino alla successiva direttiva.
	 * Se non sono presenti direttive di valorizzazione di sistema e sottosistema, vengono
	 * caricati i valori di default del file di configurazione attivo per l'elaborazione.
	 * In questo modo a ogni sorgente viene assegnato un sistema e un sottosistema dall'esterno. <br>
	 * <p>
	 * Per ogni file individuato attraverso il file pilota dei sorgenti, vengono applicati
	 * dei criteri di filtro specificati sempre nel file pilota dei sorgetnti.
	 * Le direttive di filtro rimangono valide fino alla successiva direttiva o alla direttiva di
	 * disabilitazione filtri.
	 * Ogni riga del file pilota può contenere solo una direttiva oppure una stringa con il path del file
	 * o della directory.
	 * La stringa indica che il suo valore deve trovarsi all'interno del nome del sorgente.
	 * Le direttive di filtro possono specificare un insieme di filtri (pos nel nome + valore) da soddisfare in
	 * AND sullo stesso nome di sorgente. Ulteriori direttive sono invece verificate in OR.
	 * Nelle direttive di filtro la posizione sul nomne sorgente è specificata 1 based, oppure può essere
	 * indicato di fare riferimento, per l'applicazione del filtro, alla classe di exit esterna.<br>
	 * <p>
	 * Operativamente viene letto il file pilota dei sorgenti e il file pilota di filtro.
	 * Ogni file individuato nel file system che soddisfa i criteri di filtro viene portato in output.
	 * Nel caso il file sia una directory e sia previsto dal parametro in input, viene attivata la
	 * ricerca ricorsiva standard.
	 * <p>
	 * E' possibile specificare se la ricerca deve essere effettuata ricorsivamente sulle directories
	 * trovate o restare al primo livello. Inoltre si può richiedere che le le directorries intercettate
	 * siamo portate in output o meno.
	 * 
	 * @param String pathSourcesPilot 
	 * @param String arFilter[] filtro nomi files
	 * @param boolean bSearchRecursive
	 * @param boolean bDirOnOutput
	 * @throws Exception  
	 */
	public SourceInput[] listFilesFromPilot(
			                                ArrayList<ExecutionDirectives> al_di
		                                   ,boolean bSearchRecursive
		                                   ,boolean bDirOnOutput
										   ) throws Exception  {
		
		SourceInput arSourceInput[] = null;								// Array con descrittori file sorgenti in input 
		SourceInput sourceInputInfo = null;								// Descrittore file 
		Set<SourceInput> set_sourceInput = null; 					// Contiene i descrittori dei sorgenti individuati
		String arParm[] = null;                                         // Messagi
		String pathFile = "";
		String libraryPath = "";
		String libraryCode = "";
		int posSubSys = 0;
		int lngSubSys = 0;
		
		// Allocazione array list
		set_sourceInput = new HashSet<SourceInput>();				 	// Contiene i descrittori dei sorgenti individuati
		
   
		// Scan descrittori direttive di esecuzione con le informazioni su librerie, file, filtri etc.
		for (int i = 0; i < al_di.size(); i++) {
			
			di = al_di.get(i);
			
			// Direttiva diversa da quella contenente informazioni su librerie e files da estrarre
			// e presenti più direttive: skip
			if (di.en_CurProcessFunction != EnumDirectivesExecution.OBJECTS_IDENTIFICATION_UNIT && al_di.size() > 1) {
				continue;
			}

			// FILE_PATH: Scan files specifici da trattare presenti nel descrittore
			for (int j = 0; j < di.al_filePath.size(); j++) {
				
				pathFile = di.al_filePath.get(j);
				sourceInputInfo = fileInfo(pathFile);		// Informazioni complete file 
				
				// Recupero via exit sistema/sottosistema e info filtro se richiesto da direttive
				userExitInfo = userExitCallIfNeeds(di, sourceInputInfo.getIdSource(), "");    

				// Sistema valorizzato da exit applicativa
				if (di.en_activeObjectSystem == EnumDirectivesExecution.SYSTEM_BY_EXIT) {
					di.systemInput = userExitInfo.getSystem();
				} 
				
				// Sottosistema valorizzato da exit applicativa
				if (di.en_activeObjectSubSystem == EnumDirectivesExecution.SUB_SYSTEM_BY_EXIT) {
					di.subSystemInput = userExitInfo.getSubSystem();
				
				// Sottosistema valorizzato da nome file da posizione/lng
				} else if (di.en_activeObjectSubSystem == EnumDirectivesExecution.SUB_SYSTEM_ON_NAME) {
					if (di.curSubSystemPos > 0 && di.curSubSystemLng >= 1) {
						posSubSys = di.curSubSystemPos - 1;
						lngSubSys = posSubSys + di.curSubSystemLng;
						di.subSystemInput = sourceInputInfo.getIdSource().substring(posSubSys, lngSubSys).toUpperCase();
					}
					
				} else {
					// Filtro non gestito: si è richiesto esplicitamente il file
				}
				
				// Identificazione sorgente 
				sourceInputInfo.setSystemOwner(di.systemInput);
				sourceInputInfo.setSubSystemOwner(di.subSystemInput);
				sourceInputInfo.setDi((di));
				sourceInputInfo.setLibraryCode("");
				if (!set_sourceInput.contains(sourceInputInfo)) {
					set_sourceInput.add(sourceInputInfo);	
					cntSourcesInput++;                      // Sorgenti input totali
					cntSourcesOutput++;                     // Sorgenti estratti totali
				} 				

			} // end-for
			

			// LIBRARY: Scan librerie specifiche e attivazione ricerca ricorsiva files
			for (int j = 0; j < di.al_libraryCode.size(); j++) {
				
				libraryPath = di.al_libraryPath.get(j);
				libraryCode = di.al_libraryCode.get(j);
				sourceInputInfo = fileInfo(libraryPath);		// Informazioni complete file directory
					
				// Attivazione ricorsiva ricerca files (se richiamo con abilitazione ricorsiva)
				if (sourceInputInfo.isDirectory()) {
					if (di.optDirectoryOnOutput) {
						sourceInputInfo.setDi((di));
						set_sourceInput.add(sourceInputInfo);
					}  
					
					// Messaggio di inizio scansione libreria, eventualmente ricorsivamente
					arParm = new String[2];
					arParm[0] = libraryCode;
					arParm[1] = libraryPath;
					AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0030", arParm, null); 
					
					listFilesOnDirRecursive( sourceInputInfo.getDirInput()
							               , libraryCode
							               , libraryPath
										   , set_sourceInput
										   , bSearchRecursive
										   , bDirOnOutput
									      );
					   
					// Messaggio di fine scansione libreria 
					arParm = new String[2];
					arParm[0] = libraryCode;
					arParm[1] = libraryPath;
					AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0031", arParm, null); 
					
				    // Test di raggiungimento limite massimo sorgenti
					if (di.limitMaxObjectsInput > 0 && cntSourcesInput > di.limitMaxObjectsInput) {
						break;
					} // end-if

				} // end-if 
				
			} // end-for
		} 

		// Messaggio finale di consuntivo scansione
		arParm = new String[4];
		arParm[0] = cntDirTot + "";
		arParm[1] = cntSourcesInput + "";
		arParm[2] = cntSourcesOutput + "";
		arParm[3] = cntSourcesDiscarded + "";
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0040", arParm, null); 

		// Converto in array e porto in output
		List<SourceInput> l =  new ArrayList<SourceInput>(); 
		l.addAll(set_sourceInput);
		Collections.sort(l);
		arSourceInput = new SourceInput[set_sourceInput.size()];
		arSourceInput = l.toArray(arSourceInput);
		return arSourceInput;
	}
	


	/**
	 * Imposta la tipologia del sorgente nel suo descrittore.
	 * Viene analizzato il sorgente
	 * 
	 * @return EnumSourceType of source
	 */
	public void detectSourceType(SourceInput si)  {
		
		// File vuoto
		if (si.getArrayRowSource().length == 0) {
			return;
		}
		
		// COBOL_PROGRAM
		// COBOL_COPY_PROC
		// COBOL_COPY_DATA
		// COBOL_COPY_ID_DIV
		// COBOL_COPY_ENV
		// COBOL_COPY_ENV
		// DDL_SQL
		// CICS_PCT
		// CICS_PPT
		// CICS_FCT
		// CICS_TCT
 		detectSourceTypeCobolProgram(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// SOURCE_PL1_PROGRAM
		detectSourceTypePl1Program(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// SOURCE_PL1_COPY
		detectSourceTypePl1Copy(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// SOURCE_ASM_PROGRAM
		detectSourceTypeAsmProgram(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// SOURCE_ASM_COPY
		detectSourceTypeAsmCopy(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// SOURCE_JAVA_PROGRAM
		detectSourceTypeJavaProgram(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// SOURCE_DDL_SQL
		detectSourceTypeScriptSql(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// SOURCE_DDL_DL1
		detectSourceTypeDdlDl1(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// SOURCE_JCL_MVS_JOB
		detectSourceTypeJclMvsJob(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// SOURCE_JCL_MVS_PROC
		detectSourceTypeJclMvsJobProc(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// SOURCE_JCL_MVS_INCLUDE
		detectSourceTypeJclMvsJobInclude(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}

		// CICS_BMS|PCT|PPT|FCT|TCT
		detectSourceTypeCicsBmsPctPptFctTct(si);
		if (si.getSourceType() != EnumSourceType.NOT_ASSIGNED) {
			return;
		}
		
		// Sorgente non identificato

		return;
	}

	
	
	
	/**
	 * Verifica se sorgente Cobol, programma o copy
	 * 
	 * @return EnumSourceType of source
	 */
	private void detectSourceTypeCobolProgram(SourceInput si)  {
		
		ArrayList<InnerInstructionWordsEntryCobol> al_reservedWordEntry = null;
		EnumSourceType enSourceType = EnumSourceType.NOT_ASSIGNED; 
		Scanner scn = null;
		String token = "";
		String tokenNext = "";
		String row = "";
		int level = 0;
		int lengthRow = 0;

		// E' un jcl, non può essere Cobol
		if (si.getArrayRowSource()[0].startsWith("//")) {
			enSourceType = EnumSourceType.NOT_ASSIGNED;
			return;
		}
		   
	    // Reset flag
	    isThereIdDivision = false;
	    isThereEnvDivision = false;
	    isThereDataDivision = false;
	    isThereProcDivision = false;
	    isThereFileSection = false;
	    isThereWsSection = false;
	    isThereLinkageSection = false;
	    areThereDataDefinitions = false;
	    areThereProcInstructions = false;

	    
		// Scan righe sorgente
		for (int i = 0; i < si.getArrayRowSource().length; i++) {
			
			row = si.getArrayRowSource()[i].toUpperCase();
			lengthRow = row.length();
			
			// Riga vuota: skip
			if (row.trim().equals("")) {
				continue;
			}

			// Commento Cobol:Skip
			if (lengthRow > 7){
				if (row.charAt(6) == '*' ) {
					continue;
				}
			} else {
				continue;
			}
			
			// Si considera da area A cobol
			scn = new Scanner(row.substring(7));
			
			token = nextToken(scn);
			tokenNext = nextToken(scn);
			
			// Da scartare
			if (token.trim().contentEquals("") && tokenNext.trim().contentEquals("")) {
				continue;
			}
			
			/////////////////////////////////////////////////////////////
			// Identificazione divisioni e section cobol
			/////////////////////////////////////////////////////////////
			
			if ((token.equals("IDENTIFICATION") || token.equals("ID")) && (tokenNext.equals("DIVISION."))) {
				isThereIdDivision = true;
				continue;
			} else if ((token.equals("ENVIRONMENT") || token.equals("ENV")) && (token.equals("DIVISION."))) {
				isThereEnvDivision = true;
				continue;
			} else if (token.equals("DATA") && tokenNext.equals("DIVISION.")) {
				isThereDataDivision = true;
				continue;
			} else if (token.equals("FILE") && tokenNext.equals("SECTION.")) {
				isThereFileSection = true;
				continue;
			} else if ((token.equals("LINKAGE") && tokenNext.equals("SECTION."))) {
				isThereLinkageSection = true;
				continue;
			} else if (token.equals("WORKING-STORAGE") && tokenNext.equals("SECTION.")) {
				isThereWsSection = true;
				continue;
			} else if ((token.equals("PROCEDURE") && tokenNext.equals("DIVISION."))) {
				isThereProcDivision = true;
				continue;
			}

			/////////////////////////////////////////////////////////////
			// Identificazione definizioni dati cobol
			/////////////////////////////////////////////////////////////
			
			// Definizione dati non ancora incontrata
			if (!areThereDataDefinitions) {
				scn = new Scanner(row.substring(7));
				// Pic / Picture
				if (scn.hasNextInt()) {
					level = scn.nextInt();
					if (level < 100) {
						while (scn.hasNext()) {
							token = scn.next();
							if (token .equals("PIC") 
							||  token .equals("PICTURE")
							||  token .equals("VALUE")
							||  token .equals("OCCURS")) {
								areThereDataDefinitions = true;
								break;
							}
						}
						if (areThereDataDefinitions) {
							continue;
						}
					}
				}
			}

			
			///////////////////////////////////////////////////////////////////
			// Identificazione istruzione di procedure division, fine ricerca
			///////////////////////// //////////////////////////////////////////
			
			// Verifica se label
			if (tokenNext.trim().contentEquals("") 
					&& !(token.substring(0, 1).equals("'"))
					&& token.charAt(token.length() - 1) == '.'  
					&& !(token.toUpperCase().equals("SPECIAL-NAMES.") 
							|| token.toUpperCase().equals("PROGRAM-ID.") 
							|| token.toUpperCase().equals("REPOSITORY.")  
							|| token.toUpperCase().equals("FILE-CONTROL.")

							)
					) {
				areThereProcInstructions = true;
				continue;				
			}		

			// Verifica se parola riservata Cobol di procedure dicvision
            al_reservedWordEntry = this.map_ReservedWords.get(token);
            if (al_reservedWordEntry != null && !tokenNext.trim().contentEquals("")
            && al_reservedWordEntry.get(0).en_WordReservedOwner.getCobolInstrCategory() == EnumInstrDataCategory.COBOL_PROC_INSTRUCTION) {
            	areThereProcInstructions = true;
            	break;
            }

		}

		/////////////////////////////////////////////////////////////
		// Identificazione tipo sorgente Cobol
		/////////////////////////////////////////////////////////////
		
		// Programma cobol con almeno una divisione/section in chiaro, delle definizioni dati e delle istruzioni
		if ((isThereIdDivision 		|| 
		     isThereEnvDivision		||	
		     isThereDataDivision 	||
		     isThereFileSection 	||
		     isThereWsSection 		||		
		     isThereLinkageSection 	||
		     isThereProcDivision
		    )
	    && areThereDataDefinitions
	    && areThereProcInstructions
		) {
			enSourceType = EnumSourceType.COBOL_PROGRAM;
			si.setSourceType(enSourceType);
			return;
		}
		
		// Copy di data division, con sole definizioni dati e nessuna divisione/section
		if (!isThereIdDivision   
		&&  !isThereEnvDivision	 
		&&  !isThereDataDivision  
		&&  !isThereFileSection  
		&&  !isThereWsSection  		
		&&  !isThereLinkageSection
		&&  !isThereProcDivision
	    &&  !areThereProcInstructions
	    &&   areThereDataDefinitions
		) {
			enSourceType = EnumSourceType.COBOL_COPY_DATA;
			si.setSourceType(enSourceType);
			return;
		}
		
		// Copy di proc division, con sole definizioni dati e nessuna divisione/section
		if (!isThereIdDivision   
		&&  !isThereEnvDivision	 
		&&  !isThereDataDivision  
		&&  !isThereFileSection  
		&&  !isThereWsSection  		
		&&  !isThereLinkageSection
	    &&  !areThereDataDefinitions
	    &&   areThereProcInstructions
		) {
			enSourceType = EnumSourceType.COBOL_COPY_PROC;
			si.setSourceType(enSourceType);
			return;
		}
		
		// Copy di identification division, contiene da identification in avanti
		if (isThereIdDivision) {
			enSourceType = EnumSourceType.COBOL_COPY_ID;
			si.setSourceType(enSourceType);
			return;
		}
		
		// Copy di environment division, contiene da environment division in avanti
		if (isThereEnvDivision) {
			enSourceType = EnumSourceType.COBOL_COPY_ENV;
			si.setSourceType(enSourceType);
			return;
		}

		// Tipo sorgente non individuato
		si.setSourceType(EnumSourceType.NOT_ASSIGNED);
		return;
	}


	/**
	 * Verifica se sorgente programma Pl1
	 * 
	 * @return EnumSourceType of source
	 */
	private void detectSourceTypePl1Program(SourceInput si) {
		
		EnumSourceType enSourceType = EnumSourceType.NOT_ASSIGNED; 
		String row = "";
		int limitMaxLinesScanFindingSourceType = ucfg.getLimitMaxLinesScanFindingSourceType();

		// E' un jcl
		if (si.getArrayRowSource()[0].startsWith("//")) {
			enSourceType = EnumSourceType.NOT_ASSIGNED;
			return;
		}

		
		// Scan righe lette
		for (int i = 0; i < si.getArrayRowSource().length; i++) {
			
			// Superato max previsto: sorgente non identificato
			if (i > limitMaxLinesScanFindingSourceType && limitMaxLinesScanFindingSourceType > 0) {
				break;
			}
			
			row = si.getArrayRowSource()[i].toUpperCase();
			
			// Riga vuota: skip
			if (row.trim().equals("")) {
				continue;
			}

			// Commento Pl1: skip
			if (row.startsWith("/* ") || row.startsWith("/**")){
				continue;
			}
		}
		
		// Valorizzo tipo sorgente nel descrittore
		si.setSourceType(enSourceType);
		
		
	}


	/**
	 * Verifica se sorgente copy PL1
	 * 
	 * @return EnumSourceType of source
	 */

	private void detectSourceTypePl1Copy(SourceInput si) {
		// TODO Auto-generated method stub
		
	}



	/**
	 * Verifica se sorgente Cics Bms Asm
	 * 
	 * @return EnumSourceType of source
	 */

	private void detectSourceTypeCicsBmsPctPptFctTct(SourceInput si) {
		EnumSourceType enSourceType = EnumSourceType.NOT_ASSIGNED; 
		String row = "";

		int limitMaxLinesScanFindingSourceType = ucfg.getLimitMaxLinesScanFindingSourceType();

		// E' un jcl
		if (si.getArrayRowSource()[0].startsWith("//")) {
			enSourceType = EnumSourceType.NOT_ASSIGNED;
			return;
		}

		// Scan righe lette
		for (int i = 0; i < si.getArrayRowSource().length; i++) {
			
			// Superato max previsto: sorgente non identificato
			if (i > limitMaxLinesScanFindingSourceType && limitMaxLinesScanFindingSourceType > 0) {
				break;
			}
			
			row = si.getArrayRowSource()[i].toUpperCase();

			// Riga vuota: skip
			if (row.trim().equals("")) {
				continue;
			}

			// Commento Asm: skip
			if (row.charAt(0) == '*' ) {
				continue;
			}
			
			// DFHMSD
			if ( row.indexOf(" DFHMSD ") > 0) {
				enSourceType = EnumSourceType.CICS_BMS;
				break;
			}

			// DFHMSD
			if ( row.indexOf(" DFHMDI ") > 0) {
				enSourceType = EnumSourceType.CICS_BMS;
				break;
			}
			
			// DFHPCT
			if ( row.indexOf(" DFHPCT ") > 0) {
				enSourceType = EnumSourceType.CICS_PCT;
				break;
			}
			
			// DFHPPT
			if ( row.indexOf(" DFHPPT ") > 0) {
				enSourceType = EnumSourceType.CICS_PPT;
				break;
			}			
			
			// DFHTCT
			if ( row.indexOf(" DFHTCT ") > 0) {
				enSourceType = EnumSourceType.CICS_TCT;
				break;
			}
			
			// DFHFCT
			if ( row.indexOf(" DFHFCT ") > 0) {
				enSourceType = EnumSourceType.CICS_FCT;
				break;
			}
		}
		
		// Valorizzo tipo sorgente nel descrittore
		si.setSourceType(enSourceType);
		
	}


	/**
	 * Verifica se sorgente Jcl Job
	 * 
	 * Il source deve essere un jcl e contenere la scheda job.
	 * Può contenere anche proc embedded
	 * 
	 * 
	 * @return EnumSourceType of source
	 */

	private void detectSourceTypeJclMvsJob(SourceInput si) {
		
		Scanner scn = null;
		EnumSourceType enSourceType = EnumSourceType.NOT_ASSIGNED; 
		String row = "";
        String token = "";
		
		int limitMaxLinesScanFindingSourceType = ucfg.getLimitMaxLinesScanFindingSourceType();
		
		// Non è un jcl
		row = si.getArrayRowSource()[0];
		if (!row.startsWith("//")) {
			si.setSourceType(EnumSourceType.NOT_ASSIGNED);
			return;
		}
		
		// Scan righe lette
		for (int i = 0; i < si.getArrayRowSource().length; i++) {
			
			// Superato max previsto: sorgente non identificato
			if (i > limitMaxLinesScanFindingSourceType && limitMaxLinesScanFindingSourceType > 0) {
				break;
			}
			
			row = si.getArrayRowSource()[i].toUpperCase();
			scn = new Scanner(row);
			token = scn.next();
			
			
			
			if (scn.hasNext()) {
				token = scn.next();
				if (token.equals("JOB")) {
					enSourceType = EnumSourceType.JCL_MVS_JOB;
					break;
				}
			}
		}
		
		// Valorizzo tipo sorgente nel descrittore
		si.setSourceType(enSourceType);
	}


	/**
	 * Verifica se sorgente Jcl definisce una Proc catalogata
	 * 
	 * Il source deve essere un jcl e contenere gli statement PROC/PEND

	 * @return EnumSourceType of source
	 */

	private void detectSourceTypeJclMvsJobProc(SourceInput si) {

		Scanner scn = null;
		EnumSourceType enSourceType = EnumSourceType.NOT_ASSIGNED; 
		String row = "";
        String token = "";
		
		int limitMaxLinesScanFindingSourceType = ucfg.getLimitMaxLinesScanFindingSourceType();

		// Non è un jcl
		row = si.getArrayRowSource()[0];
		if (!row.startsWith("//")) {
			si.setSourceType(EnumSourceType.NOT_ASSIGNED);
			return;
		}

		// Scan righe lette
		for (int i = 0; i < si.getArrayRowSource().length; i++) {
			
			// Superato max previsto: sorgente non identificato
			if (i > limitMaxLinesScanFindingSourceType && limitMaxLinesScanFindingSourceType > 0) {
				break;
			}
			
			row = si.getArrayRowSource()[i].toUpperCase();
			scn = new Scanner(row);
			token = scn.next();
			
			if (scn.hasNext()) {
				token = scn.next();
				if (token.equals("PROC")) {
					enSourceType = EnumSourceType.JCL_MVS_PROC;
					break;
				}
			}
		}
		
		// Valorizzo tipo sorgente nel descrittore
		si.setSourceType(enSourceType);
		
		
	}

   /**
	 * Verifica se sorgente Jcl definisce una include
	 * 
	 * Il source deve essere un jcl e non contenere schede job
	 * o definizioni di PROC.
	 * Deve essere un jcl puro.
     *
	 * @return EnumSourceType of source
	 */
	private void detectSourceTypeJclMvsJobInclude(SourceInput si) {

		Scanner scn = null;
		EnumSourceType enSourceType = EnumSourceType.NOT_ASSIGNED; 
		String row = "";
        String token = "";
		boolean isJclJob = false;
		boolean isJclProc = false;
        
        
		int limitMaxLinesScanFindingSourceType = ucfg.getLimitMaxLinesScanFindingSourceType();
		
		// Non è un jcl
		row = si.getArrayRowSource()[0];
		if (!row.startsWith("//")) {
			si.setSourceType(EnumSourceType.NOT_ASSIGNED);
			return;
		}
		
		// Scan righe lette
		for (int i = 0; i < si.getArrayRowSource().length; i++) {
			
			// Superato max previsto: sorgente non identificato
			if (i > limitMaxLinesScanFindingSourceType && limitMaxLinesScanFindingSourceType > 0) {
				break;
			}
			
			row = si.getArrayRowSource()[i].toUpperCase();
			scn = new Scanner(row);
			token = scn.next();
			
			if (scn.hasNext()) {
				token = scn.next();
				if (token.equals("JOB")) {
					isJclJob = true;
					break;
				}
				if (token.equals("PROC")) {
					isJclProc = true;
					break;
				}
			}
		}
		
		// Una include non è un jcl Job e nemmeno una definizione di Proc
		if (!isJclJob && !isJclProc) {
			enSourceType = EnumSourceType.JCL_MVS_INCLUDE;
		}
		
		// Valorizzo tipo sorgente nel descrittore
		si.setSourceType(enSourceType);

	}


	private void detectSourceTypeDdlDl1(SourceInput si) {
		// TODO Auto-generated method stub
		
	}


	/*
	 * Verifica se sorgente script Sql
	 * 
	 * @return EnumSourceType of source
	 */
	private void detectSourceTypeScriptSql(SourceInput si) {

		Scanner scn = null;
		EnumSourceType enSourceType = EnumSourceType.NOT_ASSIGNED; 
		String row = "";
        String token = "";
        ArrayList<String> al_tokenRow = null;
        ArrayList<InnerInstructionWordsEntrySql> al_sequenceKey = null;
        boolean isSequenceMatch = false;
        int i = 0;
        int j = 0;
        int k = 0;
        int l = 0;
		int limitMaxLinesScanFindingSourceType = ucfg.getLimitMaxLinesScanFindingSourceType();

		al_tokenRow = new ArrayList<String> ();
		
		enSourceType = EnumSourceType.NOT_ASSIGNED;
		si.setSourceType(enSourceType);
		
		// Scan righe lette
		for (i = 0; i < si.getArrayRowSource().length; i++) {
			
			// Superato max previsto: sorgente non identificato
			if (i > limitMaxLinesScanFindingSourceType && limitMaxLinesScanFindingSourceType > 0) {
				break;
			}
			
			row = si.getArrayRowSource()[i].toUpperCase();

			// Riga vuota: skip
			if (row.trim().equals("")) {
				continue;
			}
			
			// Estrazione token dalla riga
			scn = new Scanner(row);
			al_tokenRow.clear();
			token = nextToken(scn);
			
			// Riga di commento: skip riga
			if (token.startsWith("--")) {
				continue;
			}

			// Scan token in riga
			while (!token.equals("")) {
				al_tokenRow.add(token);
				token = nextToken(scn);
				
				// Inizio commento in riga: fine token validi
				if (token.equals("--")) {
					break;
				}
			}
			
			// Scan token estratti
			for (j = 0; j < al_tokenRow.size() - 1; j++) {
				
				token = al_tokenRow.get(j);
				
				// Token non identifica inizio istruzione
				if (this.map_SqlReservedWords.get(token) == null) {continue;}
				
				// Sequenza di parole chiavi da verificare
				al_sequenceKey = this.map_SqlReservedWords.get(token);
				isSequenceMatch = false;
				
				// Scan possibile sequenza valida di key chiave
				for (InnerInstructionWordsEntrySql sequenceKeys : al_sequenceKey) {
					
					ArrayList<String> al_wordKey = sequenceKeys.al_wordKey;
					
					// Sicuramente sequence keys No Match, token insufficienti nella riga
					if (al_wordKey.size() > al_tokenRow.size() - j) {
						continue;
					}

					// Scan keywords in sequence key
					for (k = 0; k < al_wordKey.size(); k++) {
						
						l = j + k;			// Parallel token index  on row
						
						// Sequenza fino alla key precedente individuata
						if (al_wordKey.get(k).equals("")){
							enSourceType = EnumSourceType.SQL_SCRIPT;
							si.setSourceType(enSourceType);				// Valorizzo tipo sorgente nel descrittore
							return;
						}
						
						// Sequenza per tutte le key individuate
						if (al_wordKey.get(k).equals(al_tokenRow.get(l)) && k == (al_wordKey.size() - 1)){
							enSourceType = EnumSourceType.SQL_SCRIPT;
							si.setSourceType(enSourceType);				// Valorizzo tipo sorgente nel descrittore
							return;
						}
							
						//  Match: compare next couple
						if (al_wordKey.get(k).equals(al_tokenRow.get(l))) {
							continue;
						}
						
						// No Match: analisi next sequence key
						if (!al_wordKey.get(k).equals(al_tokenRow.get(l))) {
							break;
						}
					
					} // end-for key word in sequence

				} // end-for sequence key
				
				
				// Sequenza completa individuata del numero massimo di occorrenze
				if (isSequenceMatch) {
					enSourceType = EnumSourceType.SQL_SCRIPT;
					si.setSourceType(enSourceType);				// Valorizzo tipo sorgente nel descrittore
					return;
				}
				
			} // end-for token riga
			
		} // end-for righe source
	}

    /*
     * Restituisce, se disponibile, il token successivo
     * 
     */
	private String nextToken(Scanner scn) {
		if (scn.hasNext()) {
			return scn.next();
		}
		return "";
	}

	private void detectSourceTypeJavaProgram(SourceInput si) {
		// TODO Auto-generated method stub
		
	}


	private void detectSourceTypeAsmCopy(SourceInput si) {
		// TODO Auto-generated method stub
		
	}

	/*
	 * Verifica se sorgente Assembler
	 * 
	 * @return EnumSourceType of source
	 */
	private void detectSourceTypeAsmProgram(SourceInput si) {
		EnumSourceType enSourceType = EnumSourceType.NOT_ASSIGNED; 
		String row = "";
        
		
		int limitMaxLinesScanFindingSourceType = ucfg.getLimitMaxLinesScanFindingSourceType();

		// E' un jcl
		if (si.getArrayRowSource()[0].startsWith("//")) {
			enSourceType = EnumSourceType.NOT_ASSIGNED;
			return;
		}

		
		// Scan righe lette
		for (int i = 0; i < si.getArrayRowSource().length; i++) {
			
			// Superato max previsto: sorgente non identificato
			if (i > limitMaxLinesScanFindingSourceType && limitMaxLinesScanFindingSourceType > 0) {
				break;
			}
			
			row = si.getArrayRowSource()[i].toUpperCase();

			// Riga vuota: skip
			if (row.trim().equals("")) {
				continue;
			}

			// Commento Asm: skip
			if (row.charAt(0) == '*' ) {
				continue;
			}
			

		}
		
		// Valorizzo tipo sorgente nel descrittore
		si.setSourceType(enSourceType);
		
	}



	

	/**
	 * Restituisce l'elenco dei file della directory come array di oggetti SourceInput.
	 * Necessita in input del path completo della directory.
	 * Viene fornito come parametro opzionale, un'array di stringhe di filtro. Vengono
	 * restituiti solo i file il cui nome inizia per quei valori.
	 * Restituisce null se il file non esiste o non è una directory o è una directory vuota
	 * 
	 * 
	 * @param String pathDir 
	 * @param ArrayList<SourceInput> alSourceInput
	 * @param boolean bSearchRecursive
	 * @param  boolean bDirOnOutput
	 * @throws Exception  
	 */
	private void listFilesOnDirRecursive(  
			                              String pathDir
			                            , String libraryCode
			                            , String libraryPath
							            , Set<SourceInput> set_SourceInput
							            , boolean bSearchRecursive
							            , boolean bDirOnOutput
							            ) throws Exception  {
		
		String[] arFiles = null;
		String[] arParm = null;
		SourceInput sourceInput = null;
		File file = null;
		int posSubSys = 0;
		int lngSubSys = 0;
		
		file = new File(libraryPath);
		
		// Il file non esiste
		if (!file.exists()) {
			return;
		}
		// Deve essere una directory
		if (!file.isDirectory()) {
			return;
		}
		
		// Leggo tutti i files della directory 
		arFiles = file.list();
		if (arFiles.length == 0) {
			return;
		}
		
		cntDirTot++;                     // Contatore directories trattate

		// Scan nomi files letti
		for (int i = 0; i < arFiles.length; i++) {
           	
			// Informazioni complete file (richiede molto tempo)
			sourceInput = fileInfo(libraryPath + File.separator + arFiles[i]);
         	
            // E' una directory: attivo estrazione ricorsiva nomi files
           	if (sourceInput.isDirectory()) {
           		
             	if (bDirOnOutput) {
     				sourceInput.setDi((di));
     				set_SourceInput.add(sourceInput);
				}
              	if (bSearchRecursive) {
             		// Messaggio di inizio scansione
            		arParm = new String[1];
            		arParm[0] = pathDir + arFiles[i];
            		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0030", arParm, null); 

            		listFilesOnDirRecursive(pathDir + sourceInput.getIdSource() + File.separator
            				              , libraryCode
            				              , libraryPath
          				                  , set_SourceInput
          				                  , bSearchRecursive
          				                  , bDirOnOutput
          				                   );
              		
					// Messaggio di fine scansione directory 
					arParm = new String[1];
					arParm[0] = pathDir;
					AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0031", arParm, null); 
 
              	}
              	continue;     // Next entry
            } 
           
           	cntSourcesInput++;                     // Contatore generale sorgenti in input
            	
            // E' un file: verifico matching filtro utilizzando le direttive di filtro inontrate
            if (isFileToManage(arFiles[i])) {
            	
            	// File già presente da precedente libreria in search: scarto
            	
				// Recupero via exit sistema/sottosistema e info filtro se richiesto da direttive
				userExitInfo = userExitCallIfNeeds(di, sourceInput.getIdSource(), libraryCode);    
				
				// Sistema valorizzato da exit applicativa
				if (di.en_activeObjectSystem == EnumDirectivesExecution.SYSTEM_BY_EXIT) {
					di.systemInput = userExitInfo.getSystem();
				}
				
				// Sottosistema valorizzato da exit applicativa
				if (di.en_activeObjectSubSystem == EnumDirectivesExecution.SUB_SYSTEM_BY_EXIT) {
					
					// Filtro valorizzato da exit applicativa
					if (di.en_filterType == EnumDirectivesExecution.FILTER_BY_EXIT) {
						// Sorgente non ha superato i criteri di filtro: scarta
						if (userExitInfo.isMatchingFilter() == false) {
							continue;
						}
					}
					di.subSystemInput = userExitInfo.getSubSystem();
					
				// Sottosistema valorizzato da nome file da posizione/lng
				} else if (di.en_activeObjectSubSystem == EnumDirectivesExecution.SUB_SYSTEM_ON_NAME) {
					if (di.curSubSystemPos > 0 && di.curSubSystemLng >= 1) {
						posSubSys = di.curSubSystemPos - 1;
						lngSubSys = posSubSys + di.curSubSystemLng;
						di.subSystemInput = arFiles[i].substring(posSubSys, lngSubSys).toUpperCase();
					}
				} else {
					// Subsystem fisso impostato nella direttiva
				}				

				// Identificazione sorgente
				sourceInput.setSystemOwner(di.systemInput);
				sourceInput.setSubSystemOwner(di.subSystemInput);
           	    sourceInput.setDirInput(pathDir);
 				sourceInput.setDi((di));
				sourceInput.setLibraryCode(libraryCode);
				sourceInput.setLibraryPath(libraryPath);
				
				// Se trovato significa già trattato in precedente libreria in search
				if (!set_SourceInput.contains(sourceInput)) {
					set_SourceInput.add(sourceInput);
					cntSourcesOutput++;                     // Contatore generale sorgenti estratti
				} else {
					cntSourcesDiscarded++;					
				}				
				
            } else {
            	cntSourcesDiscarded++;
			}
            
            // Test che il sorgente non sia già presente nel sistema
            
            
		    // Test di raggiungimento limite massimo sorgenti da prendere in considerazione
			if (di.limitMaxObjectsInput > 0 && cntSourcesInput >= di.limitMaxObjectsInput) {
				break;
			}
       
		} // end-for
		
		return;
	}

	
	/**
	 * Popolamento informazioni file a partire dall'oggetto File	 
	 */
	private void populateSourceInfoFromObjFile(SourceInput sourceInfo, File file) {
		int i = 0;
		Date dateLastMod = null;		
		
		// Informazioni file
		sourceInfo.setIdSourceComplete(file.getName());
		sourceInfo.setIdSource(file.getName());
		sourceInfo.setPathComplete(file.getPath());
		i = file.getName().indexOf(".");
		if (i >= 0) {
		   sourceInfo.setIdSource(file.getName().substring(0, i));
		   sourceInfo.setSourceSuffix(file.getName().substring(i + 1));
		} else {
		   sourceInfo.setIdSource(file.getName());
		   sourceInfo.setSourceSuffix("");
		}
		sourceInfo.setSize(file.length());
		sourceInfo.setDateLastMod(file.lastModified());
		dateLastMod = new Date(file.lastModified());
		
		// Data ultima modifica in formato AAAA/MM/GG
		sourceInfo.setDateLastModFormatted(DateTimeService.getDateFormatted(dateLastMod, "yyyy-MM-dd", ucfg.getCurrentLocale()));
		if (file.isDirectory()) {
			sourceInfo.setDirectory(true);
		}
		
		// Hidden
		if (file.isHidden()) {
			sourceInfo.setHidden(true);
		}		
	}

	/**
	 * Restituisce true se il source soddisfa i criteri di filtro.
	 * Il nome del file deve iniziare per una delle stringhe in array di input
	 * @throws Exception  
	 */
	private boolean isFileToManage(String idSource) throws Exception  {
		
		boolean bRet = false;                  // Default NO match e file da NON considerare
		boolean bFilterAnd = true;             // True indica filtri superati: sorgente da considerare
		int posFilter = 0;
		String valueFilterToMath = "";
		String arParm[] = null;				   // Parametri messaggio
		
		
		// Nessun filtro: file da gestire
		
		if (di.en_filterType == EnumDirectivesExecution.FILTER_DISABLED) {
			return true;
		}
		// Nessun filtro impostato
		if (di.al_filterEntry.size() == 0) {
			return true;
		}

		// Posizione e valore Filtro nel nome file  gestito da exit parametrica esterna
		if (di.en_filterType == EnumDirectivesExecution.FILTER_BY_EXIT) {
			// Gestito al momento del reperimento del sorgente
			return true;
		}
		
		// Filtri gestiti da posizione e valore.
		// Gli entry dello stesso filtro sono in AND mentre gli entry sono in OR
        // Verifico se il nome sorgente passa i criteri di filtro
		
		// Scan filtri
		for (FilterEntry ife : di.al_filterEntry) {
			
			bFilterAnd = true; 
			
			// Scan entry filtro corrente
			for (int i = 0; i < ife.arPosFilter.length; i++) {
				
				posFilter = ife.arPosFilter[i];
				valueFilterToMath = ife.arValueFilterToMath[i].trim();
				
				// Test validità dati di filtro in input
			    if ((valueFilterToMath.length() + posFilter - 1) > idSource.length()) {
			       // Logging  
				   arParm = new String[3];
				   arParm[0] = idSource;
				   arParm[1] = "" + posFilter;
				   arParm[2] = valueFilterToMath;
				   AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MW0003", arParm, null); 
			       // Applicazione filtro successivo
				   bFilterAnd = false; 
			       break;
			    }
			    
		    	// Test applicazione filtro su id source
		    	if (!idSource.startsWith(valueFilterToMath, posFilter - 1)) {
		    		bFilterAnd = false; 
		    		break;
				}
		    	
		    	// Analizzo successivo filtro in AND
		    	
			} // end-for
			
			// Tutti i filtri in AND sul nome sono stati superati: sorgente da considerare
			if (bFilterAnd) {
				bRet = true;
				break;
			}
			
			// Analizzo successivo filtro in OR
			
		} // end-for

		return bRet;
	}



	/*
	 * 
	 * Attivazione user exit  per recupero sistema sottosistema e filtro sul nome sorgente,
	 * se richiesto dalle direttive
	 * 
	 */

	private UserExitInfo userExitCallIfNeeds(ExecutionDirectives di, String idSource, String libraryCode) throws ExceptionAmrita {
		
		String classModel = "";
		String classExitNameComplete = "";
		Object[] ar_Object = null;
		Class[] ar_Class = null;
		ExceptionAmrita excp = null;
		UserExitInfo userExitInfo = null;
		
		userExitInfo = new UserExitInfo();
		
		// Recupero oggetto istanza di classe con user exit
		if (ue == null) {
			ReflectionManager rm = new ReflectionManager();
			// Determino il nome completo della classe, prendendo a modello questa corrente
			classModel = this.getClass().getName();
			int iStart = classModel.indexOf(this.getClass().getSimpleName());
			classExitNameComplete = classModel.substring(0, iStart) + di.userExitClass;

			ue = rm.newInstance(classExitNameComplete, ar_Class, ar_Object);
			ue = new UserExitDefault();
			// Gestione errore di istanziazione
			if (ue == null) {
		       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_NEW_INSTANCE, null);
        	    String strMsg[] = new String[1];
        	    strMsg[0] = classExitNameComplete;
        	    AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INTERNAL, "ET0005", strMsg, excp);
			    throw excp;
			}
		}
				
		// Recupero sistema
		if (di.en_activeObjectSystem == EnumDirectivesExecution.SYSTEM_BY_EXIT) {
			userExitInfo.setRunningProcessFunction(di.en_CurProcessFunction);
			userExitInfo.setNameToEvaluate(idSource);
			userExitInfo.setUserExitOperation(EnumUserExit.GET_SYSTEM_OWNER);
			userExitInfo.setCustomerCode(di.curCustomerCode);
			userExitInfo.setCustomerInfo(di.curCustomerInfo);
			userExitInfo.setLibraryCode(libraryCode);
			ar_Class = new Class[1];
			ar_Class[0] = userExitInfo.getClass();
			ar_Object = new Object[1];
			ar_Object[0] = userExitInfo;
			// Invocazione metodo di exit
			rm.invokeMethod(ue, "executeExit", ar_Class, ar_Object);
			// Gestione errore di invocazione
			if (ue == null) {
		       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, null);
        	    String strMsg[] = new String[2];
        	    strMsg[0] = ucfg.getUserExitClass();
        	    strMsg[1] = "executeExit";
        	    AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INTERNAL, "ET0006", strMsg, excp);
			    throw excp;
			}
			this.di.systemInput = userExitInfo.getSystem();
		}

		// Recupero sottosistema
		if (di.en_activeObjectSubSystem == EnumDirectivesExecution.SUB_SYSTEM_BY_EXIT) {
			userExitInfo.setRunningProcessFunction(di.en_CurProcessFunction);
			userExitInfo.setNameToEvaluate(idSource);
			userExitInfo.setUserExitOperation(EnumUserExit.GET_SUB_SYSTEM_OWNER);
			userExitInfo.setCustomerCode(di.curCustomerCode);
			userExitInfo.setCustomerInfo(di.curCustomerInfo);
			ar_Class = new Class[1];
			ar_Class[0] = userExitInfo.getClass();
			ar_Object = new Object[1];
			ar_Object[0] = userExitInfo;
			rm.invokeMethod(ue, "executeExit", ar_Class, ar_Object);
			// Gestione errore di invocazione
			if (ue == null) {
		       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, null);
        	    String strMsg[] = new String[2];
        	    strMsg[0] = ucfg.getUserExitClass();
        	    strMsg[1] = "executeExit";
        	    AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INTERNAL, "ET0006", strMsg, excp);
			    throw excp;
			}
			this.di.subSystemInput = userExitInfo.getSubSystem();
		}

		// Verifica matching filtro
		if (di.en_filterType == EnumDirectivesExecution.FILTER_BY_EXIT) {
			userExitInfo.setRunningProcessFunction(di.en_CurProcessFunction);
			userExitInfo.setNameToEvaluate(idSource);
			userExitInfo.setUserExitOperation(EnumUserExit.FILTER_ON_OBJECT_NAME_EVALUATE);
			userExitInfo.setCustomerCode(di.curCustomerCode);
			userExitInfo.setCustomerInfo(di.curCustomerInfo);
			ar_Class = new Class[1];
			ar_Class[0] = UserConfiguration.class;
			ar_Object = new Object[1];
			ar_Object[0] = userExitInfo;
			rm.invokeMethod(ue, "executeExit", ar_Class, ar_Object);
			// Gestione errore di invocazione
			if (ue == null) {
		       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, null);
        	    String strMsg[] = new String[1];
        	    strMsg[0] = ucfg.getUserExitClass();
        	    strMsg[1] = "executeExit";
        	    AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INTERNAL, "ET0006", strMsg, excp);
			    throw excp;
			}
			
		}
		return userExitInfo;
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
	@SuppressWarnings("unused")
	private class InnerInstructionWordsEntrySql {
		
		public EnumPrecompilerReservedWords en_WordReservedOwner = null; // Enum parola riservata indicante l'istruzione
		public EnumInstrPrecompilerType typeEntry = EnumInstrPrecompilerType.NOT_ASSIGNED;  // Constant: istruzione, operando opzione o exception (EXEC_CICS_INSTRUCTION, ..)
		public ArrayList<String> al_wordKey = null;                		 // Sequenza completa di parole chiave valide identificanti l'istruzione 
		
        /*
         * Costruttore
         */
        private InnerInstructionWordsEntrySql() {
        	en_WordReservedOwner = EnumPrecompilerReservedWords.NOT_ASSIGNED;
        	al_wordKey = new ArrayList<String> ();
		}
	}

	
	/*
	 *   Classe contenitore di servizio associato alla prima parola riservata di istruzioni Cobol.
	 *   
	 *   Tale parola può essere per esempio "NOT" di NOT AT END o di NOT INVALID KEY etc.,
	 *   che rappresentano istruzioni Cobol diverse.
	 *   Ogni entry contiene, se esistono, tutte parole che identificano l'istruzione 
	 *   
	 *   
	 */
	@SuppressWarnings("unused")
	private class InnerInstructionWordsEntryCobol {
		
		// Per riconoscimento istruzioni o parte di istruzioni
		public EnumCobolReservedWords en_WordReservedOwner = null; // Enum parola riservata indicante l'istruzione
		public EnumInstrDataCategory en_InstrCategory = null;      // Categoria word: istruzione, costante figurativa etc.
		public ArrayList<String> al_wordKey = null;                // Sequenza completa di parole chiave valide identificanti l'istruzione 
		public String wordKeyFirst = "";                           // Prima parola chiave identificante l'istruzione
		
		
        /*
         * Costruttore
         */
        private InnerInstructionWordsEntryCobol() {
        	en_WordReservedOwner = EnumCobolReservedWords.NOT_ASSIGNED;
        	en_InstrCategory = EnumInstrDataCategory.NOT_ASSIGNED;
        	al_wordKey = new ArrayList<String> ();
		}
	}


	/**
	 * @return the ucfg
	 */
	public UserConfiguration getUcfg() {
		return ucfg;
	}


	/**
	 * @param ucfg the ucfg to set
	 */
	public void setUcfg(UserConfiguration ucfg) {
		this.ucfg = ucfg;
	}

	
}




