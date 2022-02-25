package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import enums.EnumCobolReservedWords;
import enums.EnumSymbolType;
import enums.EnumInstrDataCategory;
import enums.EnumObject;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
  * 
 * <h1>
 * ProgramCobol 
 * </h1>
 *  <p>
 * Questa classe  modella un programma del linguaggio Cobol.<br>
 * Tutte le caratteristiche e le gestioni comuni a tutti i linguaggi, sono modellate dalla classe madre Program.<br>
 * 
 *
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/04/2010
 * @see Program
 * @see DataItem
 * @see Analyzer
 * @see AnalyzerCobol
 * @see AnalyzerCobolCopyDataDivision
 * @see AnalyzerCobolCopyProcedure
 * @see AnalyzerCobolProgram
 *   
 */

public class ProgramCobol extends Program implements Serializable, AmritaConstants {

	private static final long serialVersionUID = 1L;

  	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza specifiche per programmi Cobol                                                                            //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

	// Struttura con tutte le istruzioni di identification division del programma. 
    // Viene gestita l'array list di servizio e poi fa fede l'array.
	private ArrayList<ProgramCobolEntry<? extends Instruction>> al_IdentificationEntry = null;			 

	
	// Struttura con tutte le istruzioni di environment division del programma. 
    // Viene gestita l'array list di servizio e poi fa fede l'array.
	private ArrayList<ProgramCobolEntry<? extends Instruction>> al_EnvironmentEntry = null;			 

	
	// Struttura con tutte le istruzioni di procedure division del programma. 
 	private ArrayList<ProgramCobolEntry<? extends Instruction>> al_ProcedureEntry = null;			 
	

	// Struttura con tutte le istruzioni di data division del programma (definizioni dati, Fd, ..)
 	private ArrayList<ProgramCobolEntry<? extends Instruction>> al_DataEntry = null;			 

 	// Struttura con tutti i moduli copy di procedure/Data dichiarati nel programma e non utilizzati
 	// Strutture con section/paragrafi/label dichiarati e non utilizzati
 	// Nel caso di section e paragrafi sono dead code anche quelli richiamati da section/paragrafi
 	// a loro volta anche indirettamente richiamati da dead code
	private ArrayList<String> al_CopyProcDeadCode = null;			 
 	private ArrayList<String> al_CopyDataDeadCode = null;			 
 	private ArrayList<Integer> al_numDataItemDeadCode = null;			 
	private ArrayList<String> al_sectionDeadCode = null;			 
 	private ArrayList<String> al_paragraphDeadCode = null;			 
	private ArrayList<String> al_labelDeadCode = null;			 
	private ArrayList<Integer> al_unreachableDeadCode = null;			 
	private ArrayList<Integer> al_numInstrDeadCode = null;			 

 	
	// Struttura con tutti i moduli copy di procedure/Data/Env/Id dichiarati nel programma 
 	private Set<String> set_CopyId = null;			 
 	private Set<String> set_CopyEnv = null;			 
 	private Set<String> set_CopyProc = null;			 
	private Set<String> set_CopyData = null;			 
	private Set<String> set_CopyNested = null;			 
 
	// Oggetto con le informazioni sul codice dinamico di ogni istruzione.
 	// Viene serializzato contestualmente a questo descrittore di programma.
	private LogicInfoDynamic logicInfoDynamic = null;
	
	// Opzioni e informazioni di programma rilevanti
	private String programId = "";                                          // Valore di PROGRAM-ID in identification division
	private InstructionCobolEnvironment specialNames = null;				// Istruzione special names
 	private int numLastInstrMainline = 0;                                   // Numero ultima istruzione della mainline del programma
	private int numInstrLinkageSection = 0;                                 // Numero istruzione in Data Division Linkage Section
	private int numInstrWsStorageSection = 0;                               // Numero istruzione in Data Ws-Storage Section
	private boolean isThereLinkageSection = false;                          // Presente Linkage Section
	private boolean isThereWsStorageSection = false;                        // Presente Ws-Storage Section
	private boolean isCicsProgram = false;									// Programma con istruzioni Cics
	private boolean isSqlProgram = false;									// Programma con istruzioni Sql
	private boolean isDl1Program = false;									// Programma con istruzioni Dl1
 	private boolean isDecimalPointComma = false;							// Decimal Point is Comma codificato
	private boolean isAlphabetEbcdic = false;								// Alphabet Ebcdic codificato
	
    // Metriche associate al programma nel suo complesso, alla mainline e alle section/paragrafi richiamati
    private Metrics metricsProgram = null;						            // Programma nel suo complesso
    private Metrics metricsProgramMainline = null;				            // Mainline
 	private ArrayList<Metrics> al_metricsProgramSection = null;	            // Metriche per ogni Section/paragrafo richiamati
	
 	// Procedure interne richiamate
 	private Map<String, ArrayList<String>> map_procInternal = null;   		// Key =Nome section/paragrafo
 	                                                                        // Data=Nomi section/paragrafi richiamati con perform (a qualsiasi livello)
    // Info varie dimensionali sul programma
    private int sizeInstrProc = 0;                                          // Numero istruzioni di procedure division
    private int sizeInstrData = 0;                                          // Numero istruzioni di definizione dati in data division
    private int sizeInstrDataFile = 0;                                      // Numero istruzioni di definizione dati in data division file section
    private int sizeInstrDataWorking = 0;                                   // Numero istruzioni di definizione dati in data division file working
    private int sizeInstrDataLinkage = 0;                                   // Numero istruzioni di definizione dati in data division file linkage
    private int sizeSource = 0;                                             // Numero righe sorgente complessive
    private int sizeSourceProc = 0;                                         // Numero righe sorgente di procedure division
    private int sizeSourceData = 0;                                         // Numero righe sorgente di definizione dati, NON di procedure division
    private int sizeSourceBlank = 0;                                        // Numero righe sorgente complessive vuote (da colonna 7 a 72)
    private int sizeSourceProcBlank = 0;                                    // Numero righe sorgente di procedure division vuote (da colonna 7 a 72)
    private int sizeSourceDataBlank = 0;                                    // Numero righe sorgente di definizione dati, NON di procedure division, vuote (da colonna 7 a 72)
    private int sizeComm = 0;                                      		    // Numero righe commento di tutto il programma
    private int sizeCommProc = 0;                                      		// Numero righe commento di procedure division 
    private int sizeCommData = 0;                                      		// Numero righe commento di definizione dati
 	
    // Info su numeri riga sorgente di inizio/fine per divisione 
    private int numRowStartIdentification = 0;                              // Prima riga identification division
    private int numRowEndIdentification = 0;                                // Ultima riga identification division
    private int numRowStartEnvironment = 0;                              	// Prima riga environment division
    private int numRowEndEnvironment = 0;                                	// Ultima riga environment division
    private int numRowStartData = 0;                              			// Prima riga data division
    private int numRowEndData = 0;                                			// Ultima riga data division
    private int numRowStartProc = 0;                              			// Prima riga procedure division
    private int numRowEndProc = 0;                                			// Ultima riga procedure division
    
    // Info su numeri righe sorgente con anomalie recoverate
    private ArrayList<Integer> al_numRowShiftedRight = null;				// Righe shiftate causa anomalia Cobol MF che accetta istruzioni prima di col 8
    private ArrayList<Integer> al_numRowShiftedLeft = null;					// Righe shiftate causa anomalia Cobol MF che accetta label dopo col 8
    private ArrayList<Integer> al_numRowWithBadChar = null;	    			// Righe con caratteri speciali, tabulazioni, eliminati prima dell'analisi
    private ArrayList<Integer> al_numRowWithDataUnclosed = null;			// Righe di definizione dati senza punto finale di chiusura
    private ArrayList<Integer> al_numRowWithBadCoding = null;			    // Righe di proc/definizione dati con codifiche deprecate come " ..."" ....""
    private ArrayList<Integer> al_numRowWithBadCodingSql = null;			// Righe di stmt Sql con codifiche deprecate come INTO:var o : var invece di :var

    
    // Strutture generate al momento dell'analisi necessarie per calcolo grafo post analisi
    
	// GraphManager: map per connessione finale nodi BRANCH_INTERNAL a nodi JOIN (GoTo a Label)
    private Map<Integer, Object[]> map_GoTo = null;        		             // Coppia nodo GoTo   /   Object[2]
																			 //                        Object[0] = GraphNode 
																			 //                        Object[1] = String[] labels|sections
																			                                                                                 
    private Map<String, Object[]> map_LabelSection = null;				     // Coppia label|section / Object[6]
																			 //                        Object[0] = GraphNode         
																			 //                        Object[1] = ArrayList<int> goTo instructions  
    																		 //                        Object[2] = ArrayList<int> perform instructions  
	 																		 //                        Object[3] = Int numInstr (prima definizione)       
    																		 //                        Object[4] = ArrayList<int> def instructions   
                                                                             //                        Object[5] = Boolean true se section
    
    // Di servizio per i vari metodi
    private int ifLvlNestingMax = 0;                                         // Livello massimo condizioni annidate If
     
	/**
	 *  Costruttore 1
	 */
	public ProgramCobol(UserConfiguration sd) {
		super(sd, "");
		this.programType = EnumObject.OBJECT_PGM_COBOL;
		al_ProcedureEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();
		al_DataEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();
		al_IdentificationEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();
		al_EnvironmentEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();
	 	al_CopyProcDeadCode = new ArrayList<String> ();			 
	 	al_CopyDataDeadCode = new ArrayList<String> ();	
	 	al_numDataItemDeadCode = new ArrayList<Integer> ();
	 	al_sectionDeadCode = new ArrayList<String> ();			 
	 	al_paragraphDeadCode = new ArrayList<String> ();			 
	 	al_labelDeadCode = new ArrayList<String> ();
	 	al_unreachableDeadCode = new ArrayList<Integer> ();
	 	al_numInstrDeadCode = new ArrayList<Integer> ();
	 	set_CopyId = new HashSet<String>();			 
	 	set_CopyEnv = new HashSet<String>();		 
	 	set_CopyProc = new HashSet<String>();		 
	 	set_CopyData = new HashSet<String>();		 
	 	set_CopyNested = new HashSet<String>();		 
	 	al_metricsProgramSection = new ArrayList<Metrics> ();
	    al_numRowShiftedRight = new ArrayList<Integer> ();
	    al_numRowShiftedLeft  = new ArrayList<Integer> ();
	    al_numRowWithBadChar  = new ArrayList<Integer> ();
	    al_numRowWithBadCoding  = new ArrayList<Integer> ();
	    al_numRowWithBadCodingSql  = new ArrayList<Integer> ();
	    al_numRowWithDataUnclosed  = new ArrayList<Integer> ();
	    map_procInternal = new HashMap<String, ArrayList<String>> ();
	}

	
    /**
	 *  Costruttore 2
	 */
	public ProgramCobol(UserConfiguration sd, String programName) {
		super(sd, programName);
		this.programType = EnumObject.OBJECT_PGM_COBOL;
		al_ProcedureEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();
		al_DataEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();
		al_IdentificationEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();
		al_EnvironmentEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();
	 	al_CopyProcDeadCode = new ArrayList<String> ();		  	 
	 	al_CopyDataDeadCode = new ArrayList<String> ();
	 	al_numDataItemDeadCode = new ArrayList<Integer> ();
	 	al_sectionDeadCode = new ArrayList<String> ();			 
	 	al_paragraphDeadCode = new ArrayList<String> ();			 
	 	al_labelDeadCode = new ArrayList<String> ();
	 	al_unreachableDeadCode = new ArrayList<Integer> ();
	 	al_numInstrDeadCode = new ArrayList<Integer> ();
	 	set_CopyId = new HashSet<String>();			 
	 	set_CopyEnv = new HashSet<String>();		 
	 	set_CopyProc = new HashSet<String>();		 
	 	set_CopyData = new HashSet<String>();		 
	 	set_CopyNested = new HashSet<String>();		 
	 	al_metricsProgramSection = new ArrayList<Metrics> ();
	    al_numRowShiftedRight = new ArrayList<Integer> ();
	    al_numRowShiftedLeft  = new ArrayList<Integer> ();
	    al_numRowWithBadChar  = new ArrayList<Integer> ();
	    al_numRowWithBadCoding = new ArrayList<Integer> ();
	    al_numRowWithBadCodingSql = new ArrayList<Integer> ();
	    al_numRowWithDataUnclosed  = new ArrayList<Integer> ();
	    map_procInternal = new HashMap<String, ArrayList<String>> ();
	}
 	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////                     Metodi pubblici                   /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////



	/**
	 * 
	 * Restituisce l'oggetto LogicInfoDynamic con le informazioni 
	 * sulle istruzioni dinamiche, le trasformazioni dei campi, i valori rilevati
	 * e le eventuali ultime assegnazioni ancora da risolvere.
	 * 
	 * @return the logicInfoDynamic
	 */
	public LogicInfoDynamic getLogicInfoDynamic() {
		return logicInfoDynamic;
	}


	/**
	 * 
	 * Imposta l'oggetto LogicInfoDynamic con le informazioni 
	 * sulle istruzioni dinamiche, le trasformazioni dei campi, i valori rilevati
	 * e le eventuali ultime assegnazioni ancora da risolvere.
	 * 
	 * 
	 * @param logicInfoDynamic the logicInfoDynamic to set
	 */
	public void setLogicInfoDynamic(LogicInfoDynamic logicInfoDynamic) {
		this.logicInfoDynamic = logicInfoDynamic;
	}


	/**
	 * 
	 * Inserisce un entry di identificastion division del programma<br> 
	 * <p>
	 * @param ProgramCobolEntry dataInstructionEntry
     * @return Int pointer a elemento inserito
	 * 
	 */
 	public int addEntryIdentification (ProgramCobolEntry<? extends Instruction> identificationEntry){
 		al_IdentificationEntry.add(identificationEntry);
    	return al_IdentificationEntry.size()- 1;
    }

	/**
	 * 
	 * Inserisce un entry di environment division del proframma<br> 
	 * <p>
	 * @param ProgramCobolEntry dataInstructionEntry
     * @return Int pointer a elemento inserito
	 * 
	 */
 	public int addEntryEnvironment (ProgramCobolEntry<? extends Instruction> environmentEntry){
 		al_EnvironmentEntry.add(environmentEntry);
    	return al_EnvironmentEntry.size()- 1;
    }

	
	/**
	 * 
	 * Inserisce un entry di definizione dati del programma.<br> 
	 * <p>
	 * @param ProgramCobolEntry dataDefinitionEntry
	 * @return Int pointer a elemento inserito
	 * 
	 */
 	public int addEntryData (ProgramCobolEntry<? extends Instruction> dataDefinitionEntry){
 		al_DataEntry.add(dataDefinitionEntry);
    	return al_DataEntry.size()- 1;
 	}


	/**
	 * 
	 * Inserisce un entry di definizione istruzione del programma<br> 
	 * <p>
	 * @param ProgramCobolEntry dataInstructionEntry
     * @return Int pointer a elemento inserito
	 * 
	 */
 	public int addEntryProcedure (ProgramCobolEntry<? extends Instruction> instructionEntry){
 		al_ProcedureEntry.add(instructionEntry);
    	return al_ProcedureEntry.size()- 1;
    }

 	
 	
	/**
	 * Restituisce i copy nested presenti nel programma.<br>
	 * <p>
	 * Si tratta sia dei copy chiamanti sia dei copy chiamati.<br>
	 * <br>
	 * @return the set_CopyNested
	 */
	public Set<String> getCopyNested() {
		return set_CopyNested;
	}


	/**
	 * Restituisce i copy nested presenti nel programma.<br>
	 * <p>
	 * Si tratta sia dei copy chiamanti sia dei copy chiamati.<br>
	 * <br>
	 * @param set_CopyNested the set_CopyNested to set
	 */
	public void setCopyNested(Set<String> set_CopyNested) {
		this.set_CopyNested = set_CopyNested;
	}
	

 	
	/**
	 * 
	 * Restituisce l'elenco dei copy non utilizzati in data Division.<br> 
	 * <p>
	 * @return ArrayList<String> con i nomi dei copy
 	 * 
	 */
 	public ArrayList<String> deadCodeCopyData (){
    	return al_CopyDataDeadCode;
    }

	/**
	 * 
	 * Restituisce l'elenco dei copy non utilizzati in Procedure Division.<br> 
	 * <p>
	 * @return ArrayList<String> con i nomi dei copy
 	 * 
	 */
 	public ArrayList<String> deadCodeCopyProc (){
    	return al_CopyProcDeadCode;
    }

	/**
	 * 
	 * Restituisce l'elenco delle section di Procedure Division definite e non richiamate,<br>
	 * oppure richiamate da anche indirettamente da una section dead code.<br> 
	 * <p>
	 * 
	 * @return ArrayList<String> con i nomi delle section
 	 */
 	public ArrayList<String> deadCodeSections (){
     	return this.al_sectionDeadCode;
    }

	/**
	 * 
	 * Restituisce l'elenco dei paragraphi di Procedure Division definiti e non richiamati.<br>
	 * <p>
	 * Amrita identifica un paragrafo come tale a fronte di una perform o di una perform thru.<br>
	 * La fine del paragrafo viene delimitata dalla label di <tt>thru</tt> esplicita o dalla <br>
	 * prima label dopo quella oggetto di perform.<br>
	 * Formalmente la label di uscita dovrebbe essere seguita dallo statement <tt>EXIT</tt><br>
	 * <p>
	 * Pertanto NON dovrebbero esserci paragrafi dead code in quanto rientrano nella categoria 
	 * di label non referenziate.<br>
	 * <p>
	 * Tuttavia paragrafi formalmente definiti e terminati da <tt>EXIT</t> possono essere definiti
	 * e non più richiamati a fronte di successive manutenzioni.<br>
	 * <p>
	 * Questo metodo individua i paragrafi formali terminati da <tt>EXIT</tt> e verifica se vengono
	 * richiamati da qualche perform. Se non sono richiamati li restituisce come dead paragraphs.<br>
	 * <p>
	 * Vengono presi in considerazione solo i paragrafi definiti fuori dalle section.
	 * 
	 * 
	 * @return ArrayList<String> con i nomi dei paragrafi
 	 */
 	public ArrayList<String> deadCodeParagraphs (){
     	return this.al_paragraphDeadCode;
    }

	/**
	 * 
	 * Restituisce l'elenco dei numeri di definizione dati dead code in Data Division.<br> 
	 * <p>
	 * Si tratta di definizioni dati non referenziate nemmeno indirettamente, attraverso campi<br>
	 * redefines o di gruppo.<br>
	 * <p>
	 * @return ArrayList<Integer> con i numeri delle definizioni dei campi dead code in data division
 	 */
 	public ArrayList<Integer> deadCodeDataItemNumbers (){
    	return this.al_numDataItemDeadCode;
    }

	/**
	 * 
	 * Restituisce l'elenco delle label di Procedure Division definite e non referenziate.<br> 
	 * <p>
	 * @return ArrayList<String> con i nomi delle section
 	 */
 	public ArrayList<String> deadCodeLabels (){
    	return this.al_labelDeadCode;
    }

	/**
	 * 
	 * Restituisce i numeri di istruzione di codice morto, non raggiungibile fisicamente.<br> 
	 * <p>
	 * Non vengono restituite le istruzioni dentro section che sono già dead code<br>
	 * ma solo quelle non raggiungibili, collocate dopo istruzioni goTo o Xctl o comunque<br>
	 * istruzioni di interruzione del flusso di esecuzione, non precedute da label referenziate<br>
	 * <p>
	 * L'individuazione del codice morto e del codice morto non raggiungibile viene effettuata<br>
	 * nelle operazioni finali di analisi del programma.<br>
	 * <p>
	 * @return ArrayList<Integer> con i numeri delle istruzioni dead code NON raggiumgibili
 	 */
 	public ArrayList<Integer> deadCodeUnreachable(){
 		return this.al_unreachableDeadCode;
    }

	/**
	 * 
	 * Restituisce i numeri di istruzione di codice morto.<br> 
	 * <p>
	 * Vengono restituite anche le istruzioni dead code non raggiungibili ottenibili con il metodo <br>
	 * <tt>deadCodeUnreachable()</tt><br>
	 * <p>
	 * L'individuazione del codice morto e del codice morto non raggiungibile viene effettuata<br>
	 * nelle operazioni finali di analisi del programma.<br>
	 * <p>
	 * @return ArrayList<Integer> con i numeri delle istruzioni dead code
 	 */
 	public ArrayList<Integer> deadCodeInstrNumbers(){
 		return this.al_numInstrDeadCode;
    }


	/**
	 * 
	 * Restituisce tutte le istruzioni di identification division del programma.<br> 
	 * <p>
	 * @return ProgramCobolEntry<?>[]
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public ProgramCobolEntry<? extends Instruction>[] entriesIdentification (){
 		ProgramCobolEntry<? extends Instruction> ar_IdentificationEntry[] = null;
 		ar_IdentificationEntry = new ProgramCobolEntry[al_IdentificationEntry.size()];
 		ar_IdentificationEntry = al_IdentificationEntry.toArray(ar_IdentificationEntry);
    	return ar_IdentificationEntry;
 	}

	/**
	 * 
	 * Restituisce tutte le istruzioni di environment division del programma.<br> 
	 * <p>
	 * @return ProgramCobolEntry<?>[]
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public ProgramCobolEntry<? extends Instruction>[] entriesEnvironment (){
 		ProgramCobolEntry<? extends Instruction> ar_EnvironmentEntry[] = null;
 		ar_EnvironmentEntry = new ProgramCobolEntry[al_EnvironmentEntry.size()];
 		ar_EnvironmentEntry = al_EnvironmentEntry.toArray(ar_EnvironmentEntry);
    	return ar_EnvironmentEntry;
 	}



	/**
	 * 
	 * Restituisce tutte le definizioni di data division del programma.<br> 
	 * <p>
	 * @return ProgramCobolEntry<?>[]
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public ProgramCobolEntry<? extends Instruction>[] entriesData (){
 		ProgramCobolEntry<Instruction> ar_DefinitionEntry[] = null;
 		ar_DefinitionEntry = new ProgramCobolEntry[al_DataEntry.size()];
 		ar_DefinitionEntry = al_DataEntry.toArray(ar_DefinitionEntry);
    	return ar_DefinitionEntry;
 	}

	/**
	 * 
	 * Restituisce tutte le istruzioni di procedure division del programma.<br> 
	 * <p>
	 * @return ProgramCobolEntry<?>[]
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public ProgramCobolEntry<? extends Instruction>[] entriesProcedure (){
 		ProgramCobolEntry<? extends Instruction> ar_InstructionEntry[] = null;
 		ar_InstructionEntry = new ProgramCobolEntry[al_ProcedureEntry.size()];
 		ar_InstructionEntry = al_ProcedureEntry.toArray(ar_InstructionEntry);
    	return ar_InstructionEntry;
 	}

 	/**
	 * 
	 * Operazioni finali di consolidamento delle strutture interne.<br> 
	 * Gli ArrayList con le definizioni dati e istruzioni vengono trimmati alle dimensioni effettive<br>
	 * <p>
	 * 
	 */
 	public void optimize(){
 		
  		al_IdentificationEntry.trimToSize();
  		al_EnvironmentEntry.trimToSize();
    	al_ProcedureEntry.trimToSize();
  		al_DataEntry.trimToSize();
    	al_ProcedureEntry.trimToSize();
 		
 		return;
    }



	/**
	 * Restituisce il primo numero di istruzione della procedura interna.<br>
	 * <p>
	 * Viene fornito il nome di una procedura interna, ovvero una section o un paragrafo.<br>
	 * Si restituisce il numero di istruzione di definizione della section o del paragrafo.<br>
	 * Se la procedura interna non è definita restituisce -1.<br>
	 * <p>
	 * @param nome section/paragrafo  
	 * 
	 */
	public int procInternalNumInstrFirst(String procInternalName)  {
		int numInstr = 0;
		numInstr = this.procInternalPointer(procInternalName);
		if (numInstr > 0) {
			return numInstr;
		}
		return numInstr;
	}
	
	/**
	 * Restituisce i numeri di istruzione chiamanti, anche indirettamente, la procedura fornita.<br>
	 * <p>
	 * Viene fornito il nome di una procedura interna, ovvero di una section o un paragrafo.<br>
	 * Se la procedura interna non è definita restituisce null.<br>
	 * Si individuano e si accumulano i numeri delle istruzioni chiamanti (perform/goTo).<br>
	 * Se dentro section/paragrafo si ripete ricorsivamente il processo sulla procedura chiamante.<br>
	 * Il processo di accumulo termina quando le perform/goTo sono nella mainline.<br>
	 * <p>
	 * Al termine del processo sono disponibili un insieme di numeri di istruzione,<br>
	 * tipicamente perform o goTo, in section o paragrafi anche diversi, che direttamente o <br>
	 * indirettamente richiamano la procedura interna fornita in input<br>
	 * <br>
	 * @param nome section/paragrafo  
	 * @return ArrayList<Integer> instruction caller
	 */
	public ArrayList<Integer> procInternalInstructionsCaller(String procInternalName)  {
		ArrayList<Integer> al_numInstrCaller = null;
		ArrayList<String> al_procInternalManaged = null;
		int numInstr = 0;
		
		numInstr = this.procInternalPointer(procInternalName);
		if (numInstr < 0) {
			return null;
		}

		al_numInstrCaller = new ArrayList<Integer> ();
		al_procInternalManaged = new ArrayList<String> ();
		procInternalInstructionsCallerrRecursive(al_numInstrCaller, al_procInternalManaged, procInternalName);	// Attivazione ricorsiva su chiamanti	
		return al_numInstrCaller;
	}
	
	/* ------------------------------------------------------
	 * Individuazione ricorsiva numeri istruzioni chiamanti
	 * ------------------------------------------------------
	 * 
	 * Si individuano gli Xref alla procedura in input
	 * Si inseriscono i numeri istruzione chiamante in output
	 * Si inserisce la procedura di apparteneza nel set
	 * Si analizzano le procedure chiamanti
	 * Per ogni procedura chiamante si riattiva il processo ricorsivamente
	 * Per loop prevention le procedure già gestite vengono memorizzate
	 * 
	 * Il processo si ferma quando le istruzioni chiamanti sono nella mainline.
	 * 
	 */
	private void procInternalInstructionsCallerrRecursive(ArrayList<Integer> al_numInstrCaller, ArrayList<String> al_procInternalManaged , String procInternalName) {
		
		Set<String> set_procInternalCaller = null;
		String procInternalCaller = "";
		int[] ar_numInstrCaller = null;
		
		set_procInternalCaller = new HashSet<String> ();
		ar_numInstrCaller = this.xrefToProcedureInternal(procInternalName);
		
		// Scan istruzioni chiamanti la procedura interna
		for (int numInstrCaller : ar_numInstrCaller) {
			procInternalCaller = this.procInternalNameOwner(numInstrCaller);
			set_procInternalCaller.add(procInternalCaller);
			al_numInstrCaller.add(numInstrCaller);
		}
		
		// Scan procedure interne chiamanti X attivazione ricorsiva
		for (String procCaller : set_procInternalCaller) {
			
			// Procedura già trattata
			if (al_procInternalManaged.contains(procCaller)) {
				continue;
			}
			
			// Mainline: nessuna operazione
			if (procCaller.equals("== MAINLINE ==")) {
				continue;
			}
			
			// Attivazione ricorsiva
			al_procInternalManaged.add(procCaller);
			procInternalInstructionsCallerrRecursive(al_numInstrCaller, al_procInternalManaged, procCaller); 
			
		}
	}


	/**
	 * Restituisce i nomi delle procedure interne chiamanti, anche indirettamente, la procedura fornita.<br>
	 * <p>
	 * Si tratta di tutte le section o paragrafi che direttamente o indirettamene, a qualsiasi livllo,<br>
	 * richiamano la procedura fornita in input.<br>
	 * <p>
	 * Viene fornito il nome di una procedura interna, ovvero di una section o un paragrafo.<br>
	 * Se la procedura interna non è definita restituisce null.<br>
	 * Si individuano i numeri delle istruzioni chiamanti (perform/goTo).<br>
	 * Se in section/paragrafo di accumulano i nomi e si ripete ricorsivamente il processo sulla procedura chiamante.<br>
	 * Il processo di accumulo termina quando le perform/goTo sono nella mainline.<br>
	 * <p>
	 * Al termine del processo sono disponibili un insieme di nomi di section e/o paragrafi,<br>
	 * che direttamente o indirettamente richiamano la procedura interna fornita in input<br>
	 * <br>
	 * Se richiamo dalla mainline viene restituito come nome procedura == MAINLINE ==<br>
	 * <p>
	 * @param nome section/paragrafo  
	 * @return ArrayList<String> proc internal caller
	 */
	public ArrayList<String> procInternalNamesCaller(String procInternalName)  {
		
		Set<String> set_ProcInternalName = null;
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		ArrayList<String> al_procInternalName = null;
		ArrayList<Integer> al_numInstrCaller = null;
		int numInstr = 0;
		
		// Pointer a definizione section o paragrafo 
		numInstr = this.procInternalPointer(procInternalName);
		if (numInstr > 0) {
			return null;
		}
		
		// Allocazione strutture e recupero istruzioni chiamanti
		set_ProcInternalName = new HashSet<String> ();								// Conterrà i nomi delle procedure chiamanti
		al_procInternalName = new ArrayList<String> ();
		al_numInstrCaller = this.procInternalInstructionsCaller(procInternalName); 	// Individuo numeri istruzione chiamanti, anche indiretti

		// Scan istruzioni chiamanti
		for (Integer numInstrCaller : al_numInstrCaller) {
			entryProc = this.entryProcedure(numInstrCaller);
			set_ProcInternalName.add(this.procInternalNameOwner(entryProc.getInstruction().getNumInstr()));
		}
		
		// Arraylist di output
		al_procInternalName.addAll(set_ProcInternalName);
 		return al_procInternalName;
	}
	
	/**
	 * Restituisce i nomi di tutte le procedure interne richiamate dalla procedura fornita direttamente.<br>
	 * <p>
	 * Viene fornito il nome di una procedura interna, ovvero una section o un paragrafo.<br>
	 * Vengono esaminate tutte le istruzioni e per ogni perform si intabella la procedura interna richiamata.<br>
	 * <p>
	 * @param section/paragrafo di partenza
	 * 
	 */
	public ArrayList<String> procInternalNamesCalledDirect(String procInternalName)  {
		ArrayList<String> al_procNameCalled = null;
		al_procNameCalled = this.map_procInternal.get(procInternalName);
        return al_procNameCalled;
	}

	/**
	 * Restituisce i nomi di tutte le procedure interne richiamate dalla procedura fornita direttamente o ricorsivamente.<br>
	 * <p>
	 * Viene fornito il nome di una procedura interna, ovvero una section o un paragrafo.<br>
	 * Vengono esaminate tutte le istruzioni e per ogni perform si intabella la procedura interna richiamata e
	 * si procede ricorsivamente.<br>
	 * Nel caso di richiamo ricorsivo della stessa procedura interna, si intabella se la procedura interna
	 * è la stessa di quella di partenza e si torna backward nel processo ricorsivo.<br>
	 * <p>
	 * @param section/paragrafo di partenza
	 * 
	 */
	public ArrayList<String> procInternalNamesCalledAll(String procInternalName)  {
		ArrayList<String> al_procNameCalledAll = null;
		
		al_procNameCalledAll = new ArrayList<String> ();
		procInternalNamesCalledAllRecursive(procInternalName, procInternalName, al_procNameCalledAll);
        return al_procNameCalledAll;
	}

	/*
	 * Gestione ricorsiva.<br>
	 */
	private void procInternalNamesCalledAllRecursive(String procNameOrigin, String procNameCur, ArrayList<String> al_procNameCalledAll)  {
		
		ArrayList<String> al_procNameCalledCur = null;
		
		al_procNameCalledCur = this.map_procInternal.get(procNameCur);
		
		// Sicuramente
		if (al_procNameCalledCur == null) {
			return;
		}
		
		// Scan procedure chiamate a livello ricorsivo corrente
		for (String procCalled : al_procNameCalledCur) {
			
			// Loop prevention
            if (al_procNameCalledAll.contains(procCalled) && !procCalled.equals(procNameOrigin)) {
				continue;
			}
            
			// Richiamata ricorsivamente procedura origine: accodo 
            if (al_procNameCalledAll.contains(procCalled) && procCalled.equals(procNameOrigin)) {
            	al_procNameCalledAll.add(procCalled);
				continue;
			}
             
            // Accodo procedura e tratto ricorsivamente
            if (!al_procNameCalledAll.contains(procCalled)) {
            	al_procNameCalledAll.add(procCalled);
            	procInternalNamesCalledAllRecursive(procNameOrigin, procCalled, al_procNameCalledAll);
				continue;
			}
		}
		
       return;
	}
	
	/**
	 * Restituisce se la procedura interna fornita richiama ricorsivamente se stessa.<br>
	 * <p>
	 * Viene fornito il nome di una procedura interna, ovvero una section o un paragrafo.<br>
	 * Si verifica se la procedura interna richiama ricorsivamente se stessa.<br>
	 * Si tratta di una anomalia<br>
	 * <p>
	 * @param section/paragrafo di partenza
	 * 
	 */
	public boolean isProcInternalNameRecursiveCalled(String procInternalName)  {
		ArrayList<String> al_procNameCalled = null;
		
		// Procedura interna dead code: nessuna operazione
		if (this.isProcInternalDeadCode(procInternalName)) {
			return false;
		}
		
		// Recupero ricorsivo di tutte le procedure richiamate dalla procedura interna fornita
		// Se la procedura corrente viene richiamata ricorsivamente allora viene inclusa.
		// Qualsiasi altra procedura richiamata ricorsivamente (loop) non viene inclusa
		al_procNameCalled = this.procInternalNamesCalledAll(procInternalName);
		
		// Richiamo diretto o ricorsivo di se stessa
		if (al_procNameCalled.contains(procInternalName)) {
			return true;
		}
        return false;
	}

	/**
	 * Restituisce se la procedura interna è richiamata a fronte di un loop.<br>
	 * <p>
	 * Viene fornito il nome di una procedura interna, ovvero una section o un paragrafo.<br>
	 * Se la procedura interna non è definita restituisce false<br>
	 * Si verifica se la procedura interna viene richiamata esplicitamente con perform varying until<br>
	 * Si verifica se la procedura interna viene richiamata indirettamente con perform varying until,<br>
	 * -  per esempio da section o paragrafi a loro volta richiamati con perform varying until<br>
	 * Si verifica se la procedura è richiamata, anche indirettamente, da una perform inner<br>
	 * <p>
	 * In caso affermativo si restituisce true<br>
	 * <p>
	 * @param section/paragrafo di partenza
	 * @return Boolean loopPerformed
	 */
	public boolean isProcInternalLoopPerformed(String procInternalName)  {
		
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		InstructionCobolProcedure instrPerform = null;
		Instruction instrProc = null;
		ArrayList<Integer> al_numInstrCaller = null;
		boolean isEndPerform = false;
        int i = 0;
		
		al_numInstrCaller = this.procInternalInstructionsCaller(procInternalName);
		if (al_numInstrCaller == null) {
			return false;
		}
		
		// Scan istruzioni chiamanti
		for (Integer numInstrCaller : al_numInstrCaller) {
			
			entryProc = this.entryProcedure(numInstrCaller);
			
			// Interessano solo le perform, eventuali goTo a paragrafi non sono considerati
			if (entryProc.getTypeInstr() != EnumCobolReservedWords.PROC_PERFORM) {
				continue;
			}
			
			// Perform dentro loop di inner perform: si deve trovare una successiva END-PERFORM
			instrPerform = (InstructionCobolProcedure) entryProc.getInstruction();
			i = instrPerform.getNumInstr() + 1;
			for (; i < this.entriesProcedure().length; i++) {
				entryProc = this.entryProcedure(i);
				instrProc = entryProc.getInstruction();
				if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_END_PERFORM) {
					isEndPerform = true;
					break;
				}
				// Sicuramente NON dentro inner perform
				if (instrProc.isTerminatedWithPoint() 
				||  entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_SECTION
				||  entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_LABEL) {
					break;
				}
			}
			// Perform dentro un loop
			if (isEndPerform) {
				return true;
			}
			
			// Perform con opzione until
			if (instrPerform.performIsWithUntil()) {
				return true;
			}
		}
		return false;
	}


	
	/**
	 * 
	 * Restiuisce true se l'entry rappresenta una definizione dati<br> 
	 * <p>
	 * @param String int entryPointer
	 * @return boolean true se il simbolo esiste ed è una definizione dati
	 * 
	 */
 	public boolean isEntryDataItem (int entryPointer){

 		ProgramCobolEntry<? extends Instruction> dataEntry = null;
  		
		// Verifica se il pointer è corretto
 		if (entryPointer >= al_DataEntry.size()) {
			return false;
		}
 		
 		dataEntry = al_DataEntry.get(entryPointer);
 		
  		// Verifica se l'entry contiene una istruzione di ddefinizione dati
 		if (dataEntry.getInstruction() instanceof InstructionCobolDataItem) {
 			return true;
		}
     	return false;
    }

 	/**
	 * 
	 * Restiuisce true se l'entry rappresenta una label di procedure division<br> 
	 * <p>
	 * @param String int symbolPointer
	 * @return boolean true se il simbolo esiste ed è una label
	 * 
	 */
 	public boolean isEntryLabel (int entryPointer){

 		ProgramCobolEntry<? extends Instruction> procedureEntry = null;
  		InstructionCobolProcedure instr = null;
 		
		// Verifica se il pointer è corretto
 		if (entryPointer >= al_ProcedureEntry.size()) {
			return false;
		}

 		procedureEntry = al_ProcedureEntry.get(entryPointer); 
 		
 		// Verifica se l'entry contiene una istruzione 
		if (procedureEntry.getInstruction() instanceof InstructionCobolProcedure) {
			instr = (InstructionCobolProcedure) procedureEntry.getInstruction();
			// Verifica se l'istruzione è una label
	 		if (instr.getTypeInstr() == EnumCobolReservedWords.PROC_LABEL) {
				return true;
			}
		}
     	return false;
    }


 
	/**
	 * 
	 * Restiuisce true se l'entry rappresenta una Section di procedure division<br> 
	 * <p>
	 * @param int entryPointer
	 * @return boolean true se il simbolo esiste ed è una label
	 * 
	 */
 	public boolean isEntrySection (int entryPointer){

		ProgramCobolEntry<? extends Instruction> procedureEntry = null;
  		InstructionCobolProcedure instr = null;
 		
		// Verifica se il pointer è corretto
 		if (entryPointer >= al_ProcedureEntry.size()) {
			return false;
		}

 		procedureEntry = al_ProcedureEntry.get(entryPointer); 
 		
 		// Verifica se l'entry contiene una istruzione 
		if (procedureEntry.getInstruction() instanceof InstructionCobolProcedure) {
			instr = (InstructionCobolProcedure) procedureEntry.getInstruction();
			// Verifica se ls'istruzione è una section cobol
	 		if (instr.getTypeInstr() == EnumCobolReservedWords.PROC_SECTION) {
				return true;
			}
		}
    	return false;
    }


	/**
	 * 
	 * Restituisce true se l'istruzione è definita all'interno della Section fornita in input<br> 
	 * <p>
	 * Se il pointer all'istruzione è errato o la Section non è definita oppure l'istruzione non è
	 * definita al suo interno, si restituisce false.<br>
	 * Il pointer alla definizione della section nel programma si può ottenere con il metodo<b> int sectionDefinition()</b>
	 * <p>
	 * @param int instructionPointer
	 * @param int underSectionPointer
	 * @return boolean true se l'istruzione è definita all'interno della section
	 * 
	 */
 	public boolean isInstructionUnderSection(int instructionPointer, int underSectionPointer){
 		
		ProgramCobolEntry<? extends Instruction> programEntry = null;          // Singolo entry di programma

 
		// Controllo se out of range l'istruzione
 		if (instructionPointer >= al_ProcedureEntry.size()) {
			return false;
		}

		// Controllo se out of range la Section
 		if (underSectionPointer >= al_ProcedureEntry.size()) {
			return false;
		}
 		
 	    // Estraggo entry con definizione di procedure interna (Section)
		programEntry = al_ProcedureEntry.get(underSectionPointer);

 		// Verifica se è una definizione di una procedure interna (Section)
 		if (programEntry.getEntryType() != EnumInstrDataCategory.COBOL_PROC_PROC_INTERNAL) {
			return false;
		}

 		// Estraggo entry con istruzione
 		programEntry = al_ProcedureEntry.get(instructionPointer);
        
 		// Verifica se definita dentro una Section
 		if (!programEntry.isUnderProcInternal()) {
			return false;
		}
 		
		// Verifica se è definita dentro la Section fornita in input
 		if (programEntry.getUnderProcInternalPointer() != underSectionPointer) {
			return false;
		}

 		// Istruzione definita dentro la Section fornita in input
 		
 		return true;
    }

	/**
	 * 
	 * Restituisce true se l'istruzione è definita in un ramo di una istruzione condizionale<br> 
	 * <p>
	 * Se il pointer all'istruzione è errato oppure l'istruzione non è
	 * definita al suo interno, si restituisce false.<br>
	 * <p>
	 * @param int instructionPointer
	 * @return boolean true se l'istruzione è definita sotto condizione
	 * 
	 */
 	public boolean isInstructionUnderCondition(int instructionPointer){
 		
		ProgramCobolEntry<? extends Instruction> programEntry = null;          // Singolo entry di programma

 
		// Controllo se out of range l'istruzione
 		if (instructionPointer >= al_ProcedureEntry.size()) {
			return false;
		}
 		
 		// Estraggo entry con istruzione
 		programEntry = al_ProcedureEntry.get(instructionPointer);
        
 		return programEntry.isUnderCondition();
    }
	
	/**
	 * 
	 * Restiuisce true se la Section è in input in qualche istruzione di procedure division<br> 
	 * <p>
	 * @param String symbol name
	 * @return boolean true se il simbolo esiste ed è in input a qualche istruzione
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean isSectionReferenced (String sectionName){
 		
 		Object ar_o[] = null;
 		EnumSymbolType symbolType = null;
 		ArrayList<Integer> al_XrefInput = null;
 	
 		ar_o = map_Symbol.get(sectionName); 
 		
		// Verifica se la section è definita
 		if (ar_o == null) {
			return false;
		}
 
 		symbolType = (EnumSymbolType) ar_o[0];
 		
 		// Verifica se il simbolo è una Section
 		if (symbolType != EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL) {
			return false;
		}

 		// Verifica se la section è richiamata da qualche istruzione
 		if (ar_o[3] == null) {
			return false;
		}
 		
 		al_XrefInput = (ArrayList<Integer>) ar_o[3];
 		if (al_XrefInput.size() == 0) {
			return false;
		}
		
    	return true;
    }
 	
	/**
	 * 
	 * Restiuisce true se il nome fornito in input è quello di una label<br> 
	 * <p>
	 * @param String labelname
	 * @return boolean true se il simbolo esiste ed è in input a qualche istruzione
	 * 
	 */
	public boolean isLabel (String name){
		
		// E' una label
 		if (this.labelPointer(name) > 0) {
			return true;
		}
 		return false;
 	}

	/**
	 * 
	 * Restituisce true se il nome fornito in input è quello di un paragrafo cobol<br> 
	 * <p>
	 * Un paragrafo è una label oggetto di uno statement perform<br>
	 * oppure dichuarata nella sezione DECLARATIVES oppure semplicemente<br>
	 * seguita da altri statement, una label di chiusura e terminato con exit<br>
	 * <p>
	 * L'indicazione se una label sia o meno un paragrafo viene imnposta a fine<br>
	 * analisi sorgente e viene aggiornata l'istruzione label con le informazioni<br>
	 * del paragrafo.<br>
	 * <p>
	 * @param String paragraphName
	 * @return boolean true se il simbolo è il nome di un paragrafo
	 * 
	 */
	public boolean isParagraph (String idParagraph){
		
		ProgramCobolEntry<? extends Instruction> entryLabel = null;
		InstructionCobolProcedure instrLabel = null;
		int numInstrLabel = 0;
		
		numInstrLabel = this.labelPointer(idParagraph);
		entryLabel = this.entryProcedure(numInstrLabel);
		instrLabel = (InstructionCobolProcedure) entryLabel.getInstruction();
		
		// E' un paragrafo
		if (instrLabel.labelIsParagraph()) {
			return true;
		}
		
 		return false;
 	}

	/**
	 * 
	 * Restiuisce true se il nome fornito in input è quello di una section cobol<br> 
	 * <p>
	 * @param String sectionName
	 * @return boolean true se il simbolo è il nome di una section
	 * 
	 */
	public boolean isSection (String name){
		int pointerProc = 0;
		
		pointerProc = this.sectionPointer(name);
		
		// E' una section
 		if (pointerProc > 0) {
 			if (this.isEntrySection(pointerProc)) {
				return true;
			}
		}
 		return false;
 	}

	/**
	 * 
	 * Restiuisce true se la label è referenziata in qualche istruzione di procedure division<br> 
	 * <p>
	 * @param String symbol name
	 * @return boolean true se il simbolo esiste ed è in input a qualche istruzione
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean isLabelReferenced (String labelName){
 		
 		Object ar_o[] = null;
 		ArrayList<Integer> al_XrefInput = null;
 	
 		ar_o = map_Symbol.get(labelName); 
 		
		// Verifica se la section è definita
 		if (ar_o == null) {
			return false;
		}
 
  		// Verifica se il simbolo è una label o un paragrafo
		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL)
		&&  !al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_LABEL)) {
			return false;
		}

 		// Verifica se la section/label è richiamata da qualche istruzione
 		if (ar_o[3] == null) {
			return false;
		}
 		
 		al_XrefInput = (ArrayList<Integer>) ar_o[3];
 		if (al_XrefInput.size() == 0) {
			return false;
		}
		
    	return true;
    }

	/**
	 * 
	 * Restituisce true se il data item è referenziato da qualche altro data item.<br> 
	 * <p>
	 * @param String symbol name
	 * @return boolean true se il simbolo è referenziato da qualche altro data item
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean isDataItemInputForAnyData (String dataName){
 		
		Object ar_o[] = null;										// Array di oggetti da Map simboli
		EnumSymbolType symbolType = null;                           // Tipologia simbolo
 		ArrayList<Integer> al_XrefInputData = null;                 // Di servizio
 		
 		ar_o = map_Symbol.get(dataName); 
		 		
		// Verifica se il data item è definito come simbolo
 		if (ar_o == null) {
			return false;
		}
        
 		// Tipologia simbolo
 		symbolType = (EnumSymbolType) ar_o[0];
 		
 		// Verifica se il simbolo è un data item
 		if (symbolType != EnumSymbolType.COBOL_SYMBOL_DATA_ITEM) {
			return false;
		}
 
 		// Recupero tutti i riferimenti alle definizioni che referenziano il data item
		al_XrefInputData =  (ArrayList<Integer>) ar_o[4];
 		
		// Nessun riferimento al data item da parte di altri data item
		if (al_XrefInputData == null) {
			return false;
		}
		if (al_XrefInputData.size() == 0) {
			return false;
		}
		
		// Esistono dei riferimenti al data item
		
	 	return true;
    }


	/**
	 * 
	 * Restituisce true se il data item è referenziato in input o in output da qualche istruzione<br> 
	 * <p>
	 * @param String symbol name
	 * @param String typeUseDataItem che può valere INSTR_USE_DATA_ITEM_INPUT/INSTR_USE_DATA_ITEM_OUTPUT
	 * @return boolean true se il simbolo è referenziato in input da qualche istruzione
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean isDataItemUsedByAnyProcInstr (String dataName, String typeUseDataItem){
		
 		Object ar_o[] = null;										// Array di oggetti da Map simboli
 		EnumSymbolType symbolType = null;                           // Tipologia simbolo
 		ArrayList<Integer> al_XrefProcedure = null;                 // Di servizio
 		
 		ar_o = map_Symbol.get(dataName); 
		 		
		// Verifica se il data item è definito come simbolo
 		if (ar_o == null) {
			return false;
		}
        
 		// Tipologia simbolo
 		symbolType = (EnumSymbolType) ar_o[0];
 		
 		// Verifica se il simbolo è un data item
 		if (symbolType != EnumSymbolType.COBOL_SYMBOL_DATA_ITEM) {
			return false;
		}

 		// Recupero tutti i riferimenti alle istruzioni che referenziano il data item in input o in output
		if (typeUseDataItem == INSTR_USE_DATA_ITEM_INPUT) {
			al_XrefProcedure =  (ArrayList<Integer>) ar_o[3];
		} else {
			al_XrefProcedure =  (ArrayList<Integer>) ar_o[5];
		}
		
		
		// Nessun riferimento al data item da parte di altri data item
		if (al_XrefProcedure == null) {
			return false;
		}
		if (al_XrefProcedure.size() == 0) {
			return false;
		}
		
		// Esistono dei riferimenti al data item da parte di istruzioni di procedure division

		return true;
    }

	/**
	 * 
	 * Restituisce true se il data item è referenziato direttamente o indirettamente 
	 * in input o in output da qualche istruzione<br> 
	 * <p>
	 * Si considerano i riferimenti al data item, alle sue ridefinizioni ai suoi campi
	 * rinominati e a tutti i gruppi sotto il quale il data item è definito.<br>
	 * Nel caso dei gruppi si considerano tutte le assegnazioni di move con reference
	 * modification alla posizione che aggiorna il data item.<br>
	 * <p>
	 * 
	 * @param int pointer data name
	 * @param String typeUseDataItem che può valere INSTR_USE_DATA_ITEM_INPUT/INSTR_USE_DATA_ITEM_OUTPUT
	 * @return boolean true se il simbolo è referenziato in input da qualche istruzione
	 * 
	 */
 	public boolean isDataItemUsedByAnyProcInstrImplicit (int pointerDataName, String typeUseDataItem){
		
        int ar_xrefPointer[] = null; 
  		
        ar_xrefPointer = xrefToDataItemInProcedureImplicit(pointerDataName, typeUseDataItem);
 		
        // Pointer out of range o  data item non referenziato implicitamente
        if (ar_xrefPointer == null
        ||  ar_xrefPointer.length == 0) {
			return false;
		}
 		
 		return true;
    }


	/**
	 * 
	 * Restituisce true se il data item è definito immediatamente sotto il campo di gruppo fornito.<br> 
	 * In pratica il gruppo fornito è owner diretto per il data item.
	 * <p>
	 * @param int definitionPointer
	 * @param String group name
	 * @return boolean true se il data item è definito sotto il gruppo
	 * 
	 */
 	public boolean isDataItemUnderGroupName (int definitionPointer, String underGroup){
  
 		String groupOwnerName = "";
 		
 		groupOwnerName = this.groupOwnerName(definitionPointer);
 		
 		// Nessun gruppo sotto il quale il data item è definito
 		if (groupOwnerName == "") {
			return false;
		}
 		
 		// Il gruppo sotto il quale il data item è immediatamente definito non è quello fornito
 		if (!groupOwnerName.equalsIgnoreCase(underGroup)) {
			return false;
		}
 		
  	   	return true;
    }

	/**
	 * 
	 * Restituisce true se il data item è definito sotto il campo di gruppo fornito.<br> 
	 * <p>
	 * Il gruppo fornito potrebbe essere non già quello sotto il quale il data item è definito,<br>
	 * ma anche a un livello più alto, con numero di livello inferiore, sotto il quale
	 * il campoo è definito.
	 * <p>
	 * @param int definitionPointer
	 * @param String group name
	 * @return boolean true se il data item è definito sotto il gruppo
	 * 
	 */
 	public boolean isDataItemUnderGroupNameAnyLevel (int definitionPointer, String underGroup){
 		
  		ProgramCobolEntry<? extends Instruction> entryDataDiv = null;
 		InstructionCobolDataItem dataItem = null;

		int ar_groupOwnerDefinition[] = null;
 		
		// Gruppi sotto il quale il campo è definito, nella struttura annidata
 		ar_groupOwnerDefinition = this.groupOwnerPointers(definitionPointer);
 		
 		// Scan pointer a campi di gruppo sotto cui il data item è definito (uno dentro l'altro) 
 		for (int pointerDef : ar_groupOwnerDefinition) {
 			
			entryDataDiv = al_DataEntry.get(pointerDef);
 			dataItem = (InstructionCobolDataItem) entryDataDiv.getInstruction();
 			
			if (dataItem.getDataName().equalsIgnoreCase(underGroup)) {
				return true;
			}
		}
 		return false;
    }

 	
	/**
	 * 
	 * Restituisce true se il data item, di gruppo, contiene dei sottocampi binari.<br> 
	 * <p>
	 * Questa funzione è utile in particolare nei servizi di re-hosting per individuare
	 * i campi binari dei files utilizzati.<br>
	 * Per binario si intende un campo <b>binario</b>, <b>packed</b> o <b>floating point</b>.
	 * <p>
	 * Se il pointer del campo fornito è out of range , restituisce false.<br>
	 * Se il pointer del campo fornito non è di un campo cobol, restituisce false.<br>
	 * Se il pointer del campo fornito è di un campo cobol elementare, restituisce false.<br>
	 * <p>
	 * @param the definitionPointer
	 * @param the group name<br>
	 * @return true se il data item è definito sotto il gruppo
	 * 
	 */
 	public boolean isDataItemGroupWithBynaryFields (int definitionPointer){
 		
  		ProgramCobolEntry<? extends Instruction> entryDataDiv = null;
 		InstructionCobolDataItem dataItem = null;
        int lvlGroup = 0;
 		boolean isThereAnyBinaryField = false;
        
        // Pointer out of range
		if (definitionPointer >= al_DataEntry.size()) {
			return false;
		}
		
		// Pointer a definizione NON di campo, come COPY, FD etc.
		if (!(al_DataEntry.get(definitionPointer).getInstruction() instanceof InstructionCobolDataItem)) {
			return false;
		}
		
		dataItem = (InstructionCobolDataItem) al_DataEntry.get(definitionPointer).getInstruction();
		
		// E' un campo elementare
		if (!dataItem.isGroupField()) {
			return false;
		}
		
		lvlGroup = dataItem.getLevelNumber();
		
		// Scan 
		for (int i = definitionPointer + 1; i < al_DataEntry.size(); i++) {

			entryDataDiv = al_DataEntry.get(i);
			
			// Non è un campo: presumibilmente è uno stmt Copy o FD etc.
			if (!(entryDataDiv.getInstruction() instanceof InstructionCobolDataItem)) {
				continue;
			}
			
			dataItem = (InstructionCobolDataItem) entryDataDiv.getInstruction();
			
			// Fine gruppo
			if (dataItem.getLevelNumber() <= lvlGroup) {
				break;
			}
 			
			// E' un campo display
			if (dataItem.isDataItemDisplay()) {
				continue;      
			}
			
			// E' un campo binario, packed o floating point
			isThereAnyBinaryField = true;
			break;
		}
		
 		return isThereAnyBinaryField;
    }

 	
 	
 	
 	
 	
	/**
	 * Restituisce true se il data item è un elemento di una tabella definita con occurs.<br> 
	 * <p>
	 * Il data item potrebbe essere un campo elementare con picture e occurs dichiarate,<br>
	 * oppure puo' essere un campo di un elemento di gruppo occursato.<br>
	 * L'elemento di gruppo può essere direttamente occursato o fare parte di un gruppo di
	 * <p>
	 * @param int definitionPointer
	 * @return boolean true se il data item appartiene a un elemento di tabella.
	 * 
	 */
 	public boolean isDataItemTableElement (int definitionPointer){
 		
  		ProgramCobolEntry<? extends Instruction> entryDataDiv = null;
 		InstructionCobolDataItem dataItem = null;

		int ar_groupOwnerDefinition[] = null;
 		
		// Recupero definizione in oggetto
		entryDataDiv = al_DataEntry.get(definitionPointer);
		if (!(entryDataDiv.getInstruction() instanceof InstructionCobolDataItem)) {
			return false;
		}
		dataItem = (InstructionCobolDataItem) entryDataDiv.getInstruction();
		
		// Campo occursato: sicuramente elemento di tabella
		if (dataItem.isOccursClause()) {
			return true;
		}
		
		// Gruppi sotto il quale il campo è definito, nella struttura annidata
 		ar_groupOwnerDefinition = this.groupOwnerPointers(definitionPointer);
 		
 		// Scan pointer a campi di gruppo sotto cui il data item è definito (uno dentro l'altro) 
 		for (int pointerDef : ar_groupOwnerDefinition) {
 			
			entryDataDiv = al_DataEntry.get(pointerDef);
 			dataItem = (InstructionCobolDataItem) entryDataDiv.getInstruction();
 			
 			// Campo di gruppo con occurs: il campo in input è sicuramente elemento di tabella
 			if (dataItem.isOccursClause()) {
 				return true;
 			}
		}
 		return false;
    }
 	
 	
	/**
	 * Restituisce true se il programma, negli Special-Names,
	 * ha la clausola Decimal Point Is Comma.<br>
	 * In questo caso i valori numerici utilizzano la virgola
	 * per indicare il punto decimale e il punto per le migliaia.<br>
	 * 
	 * @return the isDecimalPointComma
	 */
	public boolean isDecimalPointComma() {
		return isDecimalPointComma;
	}


	/**
	 * Imposta se se il programma, negli Special-Names,
	 * ha la clausola Decimal Point Is Comma.<br>
	 * In questo caso i valori numerici utilizzano la virgola
	 * per indicare il punto decimale e il punto per le migliaia.<br>
	 * 
	 * @param isDecimalPointComma the isDecimalPointComma to set
	 */
	public void setDecimalPointComma(boolean isDecimalPointComma) {
		this.isDecimalPointComma = isDecimalPointComma;
	}


	/**
	 * Restituisce true se se il programma, negli Special-Names,
	 * ha la clausola ALPHABET diversa da EBCDIC.<br>
	 * 
	 * @return the isAlphabetEbcdic
	 */
	public boolean isAlphabetEbcdic() {
		return isAlphabetEbcdic;
	}


	/**
	 * Imposta se il programma, negli Special-Names,
	 * ha la clausola ALPHABET diversa da EBCDIC.<br>
	 * @param isAlphabetEbcdic the isAlphabetEbcdic to set
	 */
	public void setAlphabetEbcdic(boolean isAlphabetEbcdic) {
		this.isAlphabetEbcdic = isAlphabetEbcdic;
	}


	/**
	 * Restituisce il nome del programma codificato in PROGRAM-ID.<br>
	 * Può essere diverso dal nome del programma.<br>
	 * <p>
	 * @return the programId
	 */
	public String getProgramId() {
		return programId;
	}


	/**
	 * Imposta il nome del programma codificato in PROGRAM-ID.<br>
	 * Può essere diverso dal nome del programma.<br>
	 * <p>
	 * @param programId the programId to set
	 */
	public void setProgramId(String programId) {
		this.programId = programId;
	}


	/**
	 * Restituisce se il programma contiene istruzioni Cics.<br>
	 * <p>
	 * @return the isCicsProgram
	 */
	public boolean isCicsProgram() {
		return isCicsProgram;
	}


	/**
	 * Imposta se il programma contiene istruzioni Cics.<br>
	 * <p>
	 * @param isCicsProgram the isCicsProgram to set
	 */
	public void setCicsProgram(boolean isCicsProgram) {
		this.isCicsProgram = isCicsProgram;
	}


	/**
	 * Restituisce se il programma contiene istruzioni Sql.<br>
	 * <p>
	 * @return the isSqlProgram
	 */
	public boolean isSqlProgram() {
		return isSqlProgram;
	}


	/**
	 * Imèpsta se il programma contiene istruzioni Sql.<br>
	 * <p>
	 * @param isSqlProgram the isSqlProgram to set
	 */
	public void setSqlProgram(boolean isSqlProgram) {
		this.isSqlProgram = isSqlProgram;
	}


	/**
	 * Restituisce se il programma contiene istruzioni Dl1.<br>
	 * <p>
	 * @return the isDl1Program
	 */
	public boolean isDl1Program() {
		return isDl1Program;
	}


	/**
	 * Imposta se il programma contiene istruzioni Dl1.<br>
	 * <p>
	 * @param isDl1Program the isDl1Program to set
	 */
	public void setDl1Program(boolean isDl1Program) {
		this.isDl1Program = isDl1Program;
	}


	/**
	 * Restituisce l'istruzione completa di environment division che descrive
	 * il paragrafo special-names, con informazioni punto decimale, alfabeto,
	 * currency etc.<br>
	 * Se il paragrafo non è presente resrtituisce null.<br>
	 * L'istruzione fornisce tutti i metodi per recuperare le informazioni.<br>
	 * <p>
	 * 
	 * 
	 * @return InstructionCobolEnvironment the specialNames
	 */
	public InstructionCobolEnvironment getSpecialNamesInfo() {
		return specialNames;
	}


	/**
	 * Imposta l'istruzione completa di environment division che descrive
	 * il paragrafo special-names, con informazioni punto decimale, alfabeto,
	 * currency etc.<br>
	 * L'istruzione fornisce tutti i metodi per recuperare le informazioni.<br>
	 * <p>
	 * 
	 * @param InstructionCobolEnvironment specialNames the specialNames to set
	 */
	public void setSpecialNamesInfo(InstructionCobolEnvironment specialNames) {
		this.specialNames = specialNames;
	}

	/**
	 * Restituisce l'istruzione Select di environment division relativa <br>
	 * al nome interno del file fornito in input;
	 * <p>
	 * Se non viene trovata una select per il file fornito restituisce null.<br>
	 * <p>
	 * @return InstructionCobolEnvironment the specialNames
	 */
	public InstructionCobolEnvironment getSelectStatement(String fileName) {
		InstructionCobolEnvironment instrEnv = null;
		String fileNameSelect = "";
		
		for (ProgramCobolEntry<? extends Instruction> entryEnv : this.al_EnvironmentEntry) {
			
			// Interessano solo le Select
			if (entryEnv.getTypeInstr() != EnumCobolReservedWords.ENV_DIV_SELECT) {
				continue;
			}
			
			// E' la select del fileName fornito
			instrEnv = (InstructionCobolEnvironment) entryEnv.getInstruction();
			fileNameSelect = instrEnv.selectGetFileNameInternal();
			if (fileNameSelect.equals(fileName)) {
				return instrEnv;
			}
		}
		return null;
	}


	/**
	 * 
	 * Restituisce il puntatore alla definizione della section<br> 
	 * <p>
	 * Coincide coincide con il numero dell'istruzione.
	 * Se la Section non è definita restituisce -1.
	 * <p>
	 * @param String Section name
	 * @return int con la posizione della section nelle istruzioni del programma
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int sectionPointer (String sectionName){
 		
 		Object ar_o[] = null;
  	    ArrayList<Integer> al_Instruction = null;
 	
 		ar_o = map_Symbol.get(sectionName); 
 		
		// Verifica se la section è definita come simbolo
 		if (ar_o == null) {
			return -1;
		}
 
 		// Verifica se il simbolo è una Section
		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL)) {
			return -1;
		}

 		al_Instruction = (ArrayList<Integer>) ar_o[2];
 		
 		// Verifica se la section è definita come istruzione  
  		if (al_Instruction.size() == 0) {
			return -1;
		}
 		
 		// Restituisce il pointer alla definizione dell'istruzione
    	return al_Instruction.get(0);
    }


	/**
	 * 
	 * Restituisce il puntatore alla definizione della label<br> 
	 * <p>
	 * Coincide coincide con il numero dell'istruzione.
	 * Se la Label non è definita restituisce -1.
	 * <p>
	 * @param String Label name
	 * @return int con la posizione della Label nelle istruzioni del programma
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int labelPointer (String labelName){
 		
 		Object ar_o[] = null;
  	    ArrayList<Integer> al_Instruction = null;
 		
 		
 		ar_o = map_Symbol.get(labelName); 
 		
		// Verifica se la label è definita come simbolo
 		if (ar_o == null) {
			return -1;
		}
 
  		// Verifica se il simbolo è stato definito come label semplice o di inizio/fine procedura interna
 		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_LABEL)
 		&&  !al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL)) {
 			return -1;
		}
 		
 		// Verifica se la label è definita come istruzione  
 		al_Instruction =  (ArrayList<Integer>) ar_o[2];
 		if (al_Instruction.size() == 0) {
			return -1;
		}
 		
		// Restituisce il pointer alla definizione della label
    	return al_Instruction.get(0);
    }
	/**
	 * USARE labelPointer()
	 * Restituisce il puntatore alla definizione della section o del paragrafo<br> 
	 * <p>
	 * Coincide coincide con il numero dell'istruzione.
	 * Se la procedura interna  non è definita restituisce -1.
	 * <p>
	 * @param String proc internal name
	 * @return int con il numero di definizione della procedura interna
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int procInternalPointer (String idProcInternal){
 		
 		Object ar_o[] = null;
  	    ArrayList<Integer> al_Instruction = null;
 		
 		
 		ar_o = map_Symbol.get(idProcInternal); 
 		
		// Verifica se la label è definita come simbolo
 		if (ar_o == null) {
			return -1;
		}
 
  		// Verifica se il simbolo è una label
		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL)
 		&&  !al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_LABEL)) {
			return -1;
		}

 		al_Instruction =  (ArrayList<Integer>) ar_o[2];
 		
 		// Verifica se la section/paragrafo è definita come istruzione  
 		if (al_Instruction.size() == 0) {
			return -1;
		}
 		
		// Restituisce il pointer alla definizione della section/paragrafo
    	return al_Instruction.get(0);
    }

	/**
	 * 
	 * Restituisce il nome della section o del paragrafo sotto la quale è collocato il numero di istruzione<br> 
	 * <p>
	 * In caso di istruzione nella mainline del programma restituisce <br>
	 * la stringa "== MAINLINE =="<br>.
	 * <p>
	 * @param int numInstr
	 * @return String con nome section o paragrafo
	 * 
	 */
	public String procInternalNameOwner (int numInstr){
		
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		InstructionCobolProcedure instrProc = null;
		
		
		entryProc = this.entryProcedure(numInstr);
		
		// Istruzione nella mainline
		if (!entryProc.isUnderProcInternal()) {
			return "== MAINLINE ==";
		}
		
		// Istruzione dentro una section o un paragrafo
		entryProc = this.entryProcedure(entryProc.getUnderProcInternalPointer());
		instrProc = (InstructionCobolProcedure) entryProc.getInstruction();
		if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_SECTION) {
			return instrProc.sectionGetName();
		} else  {
			return instrProc.labelGetName();
		}
    }

 	
 	
	/**
	 * 
	 * Restituisce i puntatori alle definizione della label<br> 
	 * <p>
	 * Normalmente una label è definito una sola volta ma<br> 
	 * può essere definita con lo stesso nome in section Cobol<br> 
	 * differenti.<br> 
	 * Si restituiscono i numeri delle istruzioni di definizione<br> 
	 * <p>
	 * Se la Label non è definita restituisce un array list vuoto.
	 * <p>
	 * @param String Label name
	 * @return int con la posizione della Label nelle istruzioni del programma
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public ArrayList<Integer> labelPointers (String labelName){
 		
 		Object ar_o[] = null;
  	    ArrayList<Integer> al_Instruction = null;
 		
 		
 		ar_o = map_Symbol.get(labelName); 
 		
		// Verifica se la label è definita come simbolo
 		if (ar_o == null) {
			return new ArrayList<Integer> ();
		}
 
   		// Verifica se il simbolo è stato definito come label semplice o di inizio/fine procedura interna
 		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_LABEL)
 		&&  !al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL)) {
 			return new ArrayList<Integer> ();
		}

 		al_Instruction =  (ArrayList<Integer>) ar_o[2];
 		
 		// Verifica se la label è definita come istruzione  
 		if (al_Instruction.size() == 0) {
			return new ArrayList<Integer> ();
		}
 		
		// Restituisce i pointer alle definizioni della label (possono essere in section diverse)
    	return al_Instruction;
    }

	/**
	 * 
	 * Restituisce l'istruzione di definizione del data item.<br> 
	 * <p>
	 * Viene fornito il numero di entry di programma di Data Division.<br>
	 * Viene recuperato l'entry di programma con tale numero e restituita
	 * l'istruzione di definizione il cui reference è memorizzato all'interno.<br>
	 * Il numero dell'entry coincide con il numero dell'istruzione.<br>
	 * In caso di chiamata errata, a causa di numero istruzione out of range o
	 * entry di programma NON contenente una istruzione di definizione dati,
	 * viene restituito null.
	 * <p>
	 * @param int numInstrDef
	 * @return InstructionCobolDataItem con istruzione di definizione dati
	 * 
	 */
 	public InstructionCobolDataItem dataItemDefinition (int numInstrDef){
 	
		ProgramCobolEntry<? extends Instruction> cobolEntryDataDiv = null;		// Entry definizione dati
        Object objInstr = null;
	    
        // Out of range
        if (numInstrDef >= this.al_DataEntry.size()) {
			return null;
		}
        
      	cobolEntryDataDiv =   (ProgramCobolEntry<? extends Instruction>) this.entryDataDivision(numInstrDef);
      	objInstr = cobolEntryDataDiv.getInstruction();
        
      	// L'entry NON contiene una istruzione di definizione dati
      	if (!(objInstr instanceof InstructionCobolDataItem)) {
			return null;
		}
   		
   		return (InstructionCobolDataItem) objInstr;
 	 }

	/**
	 * 
	 * Restituisce il puntatore alla definizione dati del data item qualificato<br> 
	 * <p>
	 * Viene cercato il data item che è definito sotto il campo di gruppo fornito.<br> 
	 * Se non esiste il gruppo viene restituito null.<br> 
	 * Se il campo di gruppo fornito non è un gruppo viene restituito -1.<br> 
	 * Se non esiste la definizione sotto il gruppo viene restituito -1.<br> 
	 * <p>
	 * @param String data name
	 * @param String group name
	 * @return int con il numeri di definizione del data itemqualificato
	 * 
	 */
 	public Integer dataItemPointer (String dataName, String underGroup){
 		
 		int ar_definitionPointer[]  = null;									// Array di puntatori a data item con il nome del gruppo
		ProgramCobolEntry<? extends Instruction> definitionEntry = null; 	// Singolo entry di data division
 		InstructionCobolDataItem dataItemCobol = null;             			// Singola definizione di data item
 		int lvlGroup = 0;                               					// Numero livello data item di gruppo
		int lvlDataItem = 0;                            					// Numero livello data item 
        int i = 0;
		
 		// Recupero la definizioni del campo di gruppo fornito
 		ar_definitionPointer = dataItemPointers(underGroup);
 		
 		// Campo non definito
 		if (ar_definitionPointer == null) {
			return null;
		}
 		
  		i = ar_definitionPointer[0];
		definitionEntry = al_DataEntry.get(i);
		
		// Estrazione istruzione e numero di livello
		dataItemCobol = (InstructionCobolDataItem) definitionEntry.getInstruction();
		
		// Non è un campo di gruppo: skip
		if (!dataItemCobol.isGroupField()) {
			return -1;  
		}
		
		lvlGroup = dataItemCobol.getLevelNumber();

		// Campo di gruppo: scan fino a gruppo successivo con livello <= (inizio nuovo gruppo)
		for (int j = i + 1; j < al_DataEntry.size(); j++) {
			
			definitionEntry = al_DataEntry.get(j);
			
			// Non è una istruzione di definizione dati: skip
 			if (!(definitionEntry.getInstruction() instanceof InstructionCobolDataItem)) {
				continue;
			}

 			// Estrazione istruzione e numero di livello
 			dataItemCobol = (InstructionCobolDataItem) definitionEntry.getInstruction();
 			lvlDataItem = dataItemCobol.getLevelNumber();
 			
 			// Data item individuato sotto il gruppo richiesto: return pointer
 			if (dataItemCobol.getDataName().equalsIgnoreCase(dataName)) {
				return j;
			}
 			
 			// Condizioni di fine gruppo corrente: data item non trovato nel gruppo
 			if (lvlDataItem == 77 || lvlDataItem <= lvlGroup ) {
				break;
			}
		} // end-for
 			
  		
  		return -1;
 	 }

	/**
	 * 
	 * Restituisce i puntatori alla definizione dati nel programma, di data item con il nome fornito in input.<br> 
	 * <p>
	 * Un data item con lo stesso nome potrebbe essere definito in strutture Cobol
	 * diverse. Per questo motivo viene restituito un array di puntatori.
	 * I pointer coincidono  con i numeri di definizione dei data item nel programma.
	 * Se il data item non è definito come simbolo restituisce null.
	 * <p>
	 * @param String data name
	 * @return int[] con i numeri di definizione del data item nel programma
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] dataItemPointers (String dataName){
 		
 		Object ar_o[] = null;
 		EnumSymbolType symbolType = null; 
 	    ArrayList<Integer> al_Definition = null;
		int ar_Definition[] = null;
		 	    
 	    
 		ar_o = map_Symbol.get(dataName); 
 		
		// Verifica se il data item è definito come simbolo
 		if (ar_o == null) {
			return null;
		}
 
 		symbolType = (EnumSymbolType) ar_o[0];
 		
 		// Verifica se il simbolo è un dataName
 		if (symbolType != EnumSymbolType.COBOL_SYMBOL_DATA_ITEM) {
			return null;
		}
        
 		// Pointer alle definizioni del data item
 		al_Definition = (ArrayList<Integer>) ar_o[1];
 		
 		// Verifica se il data item è definito come definizione
		if (al_Definition.size() == 0) {
			return null;
		}
		
		// Allocazione array puntatori di output
		ar_Definition = new int[al_Definition.size()];
 		
 		// Trasformo array di integer in array di int
  		for (int i = 0; i < ar_Definition.length; i++) {
 			Integer objInteger = (Integer) al_Definition.get(i);
 			ar_Definition[i] = objInteger.intValue();
		}

 		// Restituisce i pointer alle definizioni, in quanto il data item potrebbe
 		// essere definito in sezioni diverse Cobol con lo stesso nome
    	return ar_Definition;
    }

	/**
	 * 
	 * Restituisce il primo data item definito con il nome fornito.<br> 
	 * <p>
	 * Potrebbero essere definiti più campi con lo stesso nome in strutture diverse,<b>
	 * referenziati normalminte utilizzando l'attributo OF.<br>.
	 * Nella maggior parte dei casi la definizione è univoca e, in ogni caso, questo <br>
	 * metodo restituisce il primo data item definito.<br>
	 * <p>
	 * Se nessun data item è definito con il nome fornito, viene restituito null.<br>
	 * <p>
	 * @param String data item name
	 * @return InstructionCobolDataItem with the name supplied
	 */
	public InstructionCobolDataItem dataItemDefinition(String dataItemName){
 		InstructionCobolDataItem dataItemFirstDefined = null;
 		int[] ar_pointers = null;
 		int numInstrDef = 0;
 		
 		ar_pointers = this.dataItemPointers(dataItemName);
 		
 		// Campo non definito
 		if (ar_pointers == null) {
			return null;
		}
 		numInstrDef = ar_pointers[0];
 		dataItemFirstDefined = this.dataItemDefinition(numInstrDef);
 		
     	return dataItemFirstDefined;
    }
	/**
	 * 
	 * Restituisce i puntatori alla definizione dati nel programma, per il data item fornito in input,
	 * di tutti i data item redefines, renames e di gruppo che possono riferirsi implicitamente.<br>
	 * <p>
	 * Viene restituita una ArrayList di ArrayList di interi.<br>
	 * La prima ArrayList contiene i numeri di definizione dei data item impliciti.<br>
	 * La seconda ArrayList contiene la posizione utile di inizio dei data item impliciti.<br>
	 * La terza ArrayList contiene la lunghezza utile a copertura di tutto il data item fornito.<br>
	 * <p>
	 * La posizione è 1-based e relativa al livello del data item implicito, se un gruppo, oppure
	 * relativa al livello 01 di appartenenza, negli altri casi (redefines e renames.<br>
	 * <p>
	 * Tutti i data item impliciti restituiti sono definiti sotto lo stesso livello 01.<br>
	 * <p>
	 * Applicativamente è possibile trovare nel programma assegnazioni valide con esplicitazione
	 * di reference modification (pos e lunghezza), che modificano implicitamente il data item
	 * in oggetto.<br>
	 * <p>
	 * In caso di pointer fornito errato, si restituisce null.
	 * 
	 * 
	 * @param int pointerDataItem
	 * @return ArrayList bidimensionale con i numeri di definizione dei data item, posizione e lunghezza.
	 * 
	 */
 	public ArrayList<ArrayList<Integer>> dataItemsImplicit(int pointerDataItem){
 		
		ProgramCobolEntry<? extends Instruction> entryDataItem = null;          // Contenitore istruzione di definizione data item
		ProgramCobolEntry<? extends Instruction> entryDataItemWork = null;      // Contenitore istruzione di definizione data item
		InstructionCobolDataItem dataItem = null;				                // Istruzione di definizione dati data item
		InstructionCobolDataItem dataItemWork = null;				            // Istruzione di definizione dati di lavoro
		
		ArrayList<ArrayList<Integer>> al_implicitDataItemOutput = null;         // ArrayList bidimensionale restituito in output
		ArrayList<Integer> al_implicitDataItemPointer = null;   				// Pointers a  data item impliciti                      
		ArrayList<Integer> al_implicitDataItemPos = null;   					// Posizione inizio in data item impliciti                      
		ArrayList<Integer> al_implicitDataItemLng = null;   					// Lunghezza utile in data item impliciti                      
		
		// Gruppi e campi impliciti
		int[] ar_dataItemGroupOwner = null;                                     // Gruppi sotto il quale è definito il campo Receiver
		int[] ar_dataItemGroupOwnerRedefines = null;                            // Campi che ridefiniscono i gruppi sotto il quale è definito il campo Receiver
		int[] ar_dataItemGroupDataItem = null;                                  // Campi di gruppo o elementari definiti sotto il campo Receiver, se gruppo
		int[] ar_dataItemRedefines = null;                                      // Campi che ridefinizcono il campo Receiver
		int[] ar_dataItemRedefiningGroup = null;                                // Campi di gruppo o elementari definiti sotto il campo ridefinente il gruppo owner
		
		// Posizione data item fornito 
		int posDataItem = 0;                                                    // 0-based a partire dal livello 01
		int lngDataItem = 0;                                                    // Lunghezza in bytes data item fornito in input

		// Posizioni per data item impliciti
		int posInImplicitDataItem = 0;											// 0-based a partire dal livello 01
		int posImplicitDataItem = 0;											// 0-based a partire dal livello 01
		int lngImplicitDataItem = 0;											// Da pos a copertura della lunghezza del data item fornito
        
		
		al_implicitDataItemOutput = new ArrayList<ArrayList<Integer>> ();
		al_implicitDataItemPointer = new ArrayList<Integer> ();    				// (0) ArrayList con pointers ai data item impliciti     
		al_implicitDataItemPos = new ArrayList<Integer> ();    					// (1) ArrayList con posizione implicita nel campo       
		al_implicitDataItemLng = new ArrayList<Integer> ();    					// (2) ArrayList con lunghezza implicita nel campo       
		
		// Store references ArrayList per output
		al_implicitDataItemOutput.add(al_implicitDataItemPointer);    						      
		al_implicitDataItemOutput.add(al_implicitDataItemPos);    							      
		al_implicitDataItemOutput.add(al_implicitDataItemLng);    							        
		
    
 		// Pointer out of range
 		if (pointerDataItem >= al_DataEntry.size()) {
 			return null;
		}
		
   		// Data item elemento di tabella, sotto occurs oppure campo occursato: si considera solo in input e si scarta
        if (this.isDataItemTableElement(pointerDataItem)) {
			return al_implicitDataItemOutput;
		}   		
 
        entryDataItem = this.entryDataDivision(pointerDataItem);
        dataItem = (InstructionCobolDataItem) entryDataItem.getInstruction();
   		posDataItem = entryDataItem.getPos();
        lngDataItem = dataItem.getSizeBytes();

        
 		//////////////////////////////////////////////////////////////////////////////////////////
		// Estrazione di tutti i possibili Data items impliciti
 		//////////////////////////////////////////////////////////////////////////////////////////

  		// Campi elementari e/o di gruppo definiti sotto il data item di gruppo (array null se data item elementare).
        // Interessa solo il primo campo
		ar_dataItemGroupDataItem = dataItemsUnderGroupPointers(pointerDataItem, DATA_ITEMS_ALL);
   	    for (int pointer : ar_dataItemGroupDataItem) {
   	    	
   			// Estremi data item implicito
  			entryDataItemWork = this.entryDataDivision(pointer);
 			dataItemWork = this.instructionDataItem(pointer);

			// Calcolo posizione e lunghezza 
 			posInImplicitDataItem = 1;
 			if (dataItemWork.getSizeBytes() > lngDataItem) {
 				lngImplicitDataItem = lngDataItem;
			} else {
				lngImplicitDataItem = dataItemWork.getSizeBytes();
			}
 			
 			// Inserimento data item implicito
  			al_implicitDataItemPointer.add(pointer);			    		// Pointer definizione
			al_implicitDataItemPos.add(posInImplicitDataItem);			    //  Da posizione
			al_implicitDataItemLng.add(lngImplicitDataItem);	    		//    per lunghezza
   			break;
		}
   	    
		// Campi di gruppo owner contenenti il data item candidato esplicito.
		ar_dataItemGroupOwner = this.groupOwnerPointers(pointerDataItem);
   		for (int pointer : ar_dataItemGroupOwner) {
   			
   			// Estremi data item implicito
  			entryDataItemWork = this.entryDataDivision(pointer);
 			dataItemWork = this.instructionDataItem(pointer);
 			
  			// Campo di gruppo FILLER: non possono essere referenziati
  			if (dataItemWork.getDataName().equals("FILLER")) {continue;}
   			
   			// Calcolo posizione e lunghezza nel gruppo
   			posImplicitDataItem = entryDataItemWork.getPos();
   			posInImplicitDataItem = posDataItem - posImplicitDataItem + 1;
   			lngImplicitDataItem = lngDataItem;
   			
   		    // Inserimento data item implicito
   			al_implicitDataItemPointer.add(pointer);								// Pointer definizione
			al_implicitDataItemPos.add(posInImplicitDataItem);			    		//  Da posizione
			al_implicitDataItemLng.add(lngImplicitDataItem);	    				//    per lunghezza
 			
   			// Campi che ridefiniscono i campi di gruppo owner contenenti il campo Receiver candidato esplicito.
   			ar_dataItemGroupOwnerRedefines = this.dataItemsRedefinePointers(pointer);
            for (int pointerGroupRed : ar_dataItemGroupOwnerRedefines) {
            	
            	// Estremi data item implicito
    			entryDataItemWork = this.entryDataDivision(pointerGroupRed);
     			dataItemWork = this.instructionDataItem(pointerGroupRed);
     			
     			// Calcolo posizione e lunghezza nel gruppo
      			posImplicitDataItem = entryDataItemWork.getPos();
       			posInImplicitDataItem = posDataItem - posImplicitDataItem + 1;
       			lngImplicitDataItem = lngDataItem;

       		    // Inserimento data item implicito
            	al_implicitDataItemPointer.add(pointerGroupRed);					// Pointer definizione
    			al_implicitDataItemPos.add(posInImplicitDataItem);			    	//  Da posizione
    			al_implicitDataItemLng.add(lngImplicitDataItem);	    			//    per lunghezza

            	// Campi sotto i campi che ridefiniscono i campi di gruppo owner contenenti il data item.
        		ar_dataItemGroupDataItem = this.dataItemsUnderGroupPointers(pointerGroupRed, DATA_ITEMS_ALL);
           	    for (int pointerUnderGroupRed : ar_dataItemGroupDataItem) {
           	    	
                	// Estremi data item implicito candidato
        			entryDataItemWork = this.entryDataDivision(pointerUnderGroupRed);
         			dataItemWork = this.instructionDataItem(pointerUnderGroupRed);

           	    	// Il data item inizia dopo quello fornito
         			if (entryDataItemWork.getPos() >= posDataItem + lngDataItem ) {break;}
         			
         	    	// Il data item inizia e finisce prima di quello fornito
         			if (entryDataItemWork.getPos() + dataItemWork.getSizeBytes() < posDataItem) {continue;}

         			// Il data item coincide o si sovrappone con quello fornito
         			
         			// Calcolo posizione e lunghezza nel campo
         			posImplicitDataItem = entryDataItemWork.getPos() - posDataItem + 1 ;
         			lngImplicitDataItem = dataItemWork.getSizeBytes() - posImplicitDataItem;
         			if (lngImplicitDataItem > lngDataItem - posImplicitDataItem) {
         				lngImplicitDataItem = lngDataItem - posImplicitDataItem;
					}
         			
         		    // Inserimento data item implicito
           			al_implicitDataItemPointer.add(pointerUnderGroupRed);      		// Pointer definizione
        			al_implicitDataItemPos.add(posInImplicitDataItem);			    //  Da posizione
        			al_implicitDataItemLng.add(lngImplicitDataItem);	    		//    per lunghezza
       		}
			}
		}
   		
	    // Campi elementari e/o di gruppo che ridefiniscono esplicitamente il data item fornito.
		ar_dataItemRedefines = this.dataItemsRedefinePointers(pointerDataItem);
  		for (int pointerRedefines : ar_dataItemRedefines) {
  			
        	// Estremi data item implicito
			entryDataItemWork = this.entryDataDivision(pointerRedefines);
 			dataItemWork = this.instructionDataItem(pointerRedefines);
            
 			// Calcolo posizione e lunghezza 
 			posInImplicitDataItem = 1;
 			if (dataItemWork.getSizeBytes() > lngDataItem) {
 				lngImplicitDataItem = lngDataItem;
			} else {
				lngImplicitDataItem = dataItemWork.getSizeBytes();
			}
 			
 			// Inserimento data item implicito
  			al_implicitDataItemPointer.add(pointerRedefines);			    // Pointer definizione
			al_implicitDataItemPos.add(posInImplicitDataItem);			    //  Da posizione
			al_implicitDataItemLng.add(lngImplicitDataItem);	    		//    per lunghezza

   			// Campi definiti sotto il campo ridefinente
			ar_dataItemRedefiningGroup = this.dataItemsUnderGroupPointers(pointerRedefines, DATA_ITEMS_ALL);
            for (int pointerRed : ar_dataItemRedefiningGroup) {
            	
      			// Estremi data item implicito
      			entryDataItemWork = this.entryDataDivision(pointerRed);
     			dataItemWork = this.instructionDataItem(pointerRed);

     			// Calcolo posizione e lunghezza nel campo
     			posImplicitDataItem = entryDataItemWork.getPos() - posDataItem + 1 ;
     			lngImplicitDataItem = dataItemWork.getSizeBytes();
     			if (lngImplicitDataItem > lngDataItem - posImplicitDataItem) {
     				lngImplicitDataItem = lngDataItem - posImplicitDataItem;
				}
     			
     			// Inserimento data item implicito
      			al_implicitDataItemPointer.add(pointerRed);			    		// Pointer definizione
    			al_implicitDataItemPos.add(posInImplicitDataItem);			    //  Da posizione
    			al_implicitDataItemLng.add(lngImplicitDataItem);	    		//    per lunghezza
			}
 		}

    	return al_implicitDataItemOutput;
    }




	/**
	 * 
	 * Restituisce i puntatori alle definizioni dei data item elementari e/o di gruppo, 
	 * definiti sotto  il gruppo fornito in input.<br> 
	 * <p>
	 * Se il campo non è di gruppo o la richiesta è errata restituisce un array vuoto.<br>
	 * <p>
	 * Il primo parametro è il numero di definizione del data item di gruppo da esaminare.<br>
	 * <p>
	 * Il secondo parametro indica il tipo di data items definiti sotto il gruppo da restituire:<br>
	 * <p>
	 * <b>DATA_ITEMS_GROUP_FIELDS </b> <br>  		 
	 *    Restituiti solo i campi di gruppo del gruppo<br>
	 * <p>
	 * <b>DATA_ITEMS_ELEMENTARY_FIELDS</b> <br>  
	 *    Restituiti solo i campi elementari del gruppo 
	 * <p> 
	 * <b>DATA_ITEMS_ALL</b> <br>		     
	 *    Restituiti tutti i campi del gruppo (altri gruppi e dati elementari)
	 * 
	 * @parm int dataNameGroupPointer
	 * @parm int typeDataItemsToExtract da AmritaConstants
	 * @return int[] con i numeri di definizione dei data item elementari defini nel gruppo
	 * 
	 */
 	public int[] dataItemsUnderGroupPointers(int dataNameGroupPointer, int typeDataItemsToExtract){
 		
		ProgramCobolEntry<? extends Instruction> dataEntry = null;  // Singolo entry di data division
 		InstructionCobolDataItem dataItemCobol = null;             	// Singola definizione di data item
  		ArrayList<Integer> al_dataItemElementary = null;			// ArrayList di servizio
		int ar_dataItemElementary[] = null;							// Array con nomi campi elementari del gruppo da portare in output
		int lvlDataItemGroup = 0;                       			// Numero livello data item di gruppo
 		int lvlDataItemLoop = 0;                        			// Numero livello data item in ciclo
 		
 		al_dataItemElementary = new ArrayList<Integer>();
 		
 		// Pointer out of range
 		if (dataNameGroupPointer >= al_DataEntry.size()) {
			return null;
		}
 		
 		dataEntry = al_DataEntry.get(dataNameGroupPointer);
 		
 		// Verifico che l'entry contenga una istruzione di definizione dati
 		if (!(dataEntry.getInstruction() instanceof InstructionCobolDataItem)) {
 			return new int[] {};
		}
 		
 		// Estraggo entry gruppo di data division e definizione data item
		dataItemCobol = (InstructionCobolDataItem) dataEntry.getInstruction();
		lvlDataItemGroup = dataItemCobol.getLevelNumber();
        
		// Non è un campo di gruppo
		if (!dataItemCobol.isGroupField()) {
			return new int[] {};
		} 		
		
		
		// Scan definizioni definite sotto il gruppo
 		for (int i = dataNameGroupPointer + 1; i < this.al_DataEntry.size(); i++) {

 			dataEntry = this.al_DataEntry.get(i);
 			
 	 		// L'entry non contiene un'istruzione di definizione dati: skip
 	 		if (!(dataEntry.getInstruction() instanceof InstructionCobolDataItem)) {
 	 			continue;
 			}
			
 			// Estraggo istruzione e livello
 			dataItemCobol = (InstructionCobolDataItem) dataEntry.getInstruction();
 			lvlDataItemLoop = dataItemCobol.getLevelNumber();
 			
 			// Livello 88: skip
 			if (lvlDataItemLoop == 77) {
				continue;
			}

 			// Livello 77: exit
 			if (lvlDataItemLoop == 77) {
				break;
			}

			// Fine campo di gruppo in input: exit
 			if (lvlDataItemLoop <= lvlDataItemGroup) {
				break;
			}

			// Campo di gruppo e richiesti solo campi elementari: skip
 			if (dataItemCobol.isGroupField() && typeDataItemsToExtract == DATA_ITEMS_ELEMENTARY_FIELDS) {
 				continue;
			}

			// Campo elementare e richiesti solo campi di gruppo: skip
 			if (!dataItemCobol.isGroupField() && typeDataItemsToExtract == DATA_ITEMS_GROUP_FIELDS) {
 				continue;
			}

			// Richiesti tutti i tipi di campi o match specifico: porto in output
 			al_dataItemElementary.add(i);
 		}
 		
 		// Converto ArrayList in Array di interi di output
 		ar_dataItemElementary = new int[al_dataItemElementary.size()];
 		for (int i = 0; i < ar_dataItemElementary.length; i++) {
 			ar_dataItemElementary[i] = al_dataItemElementary.get(i).intValue();
		}
 
 		// Restituisce i pointer alle definizioni di campi elementari dentro il gruppo

 		return ar_dataItemElementary;
    }

 	
	/**
	 * 
	 * Restituisce le definizioni dei data item elementari e/o di gruppo, 
	 * definiti sotto  il gruppo fornito in input.<br> 
	 * <p>
	 * Se il campo non è di gruppo o la richiesta è errata restituisce un array vuoto.<br>
	 * <p>
	 * Il primo parametro è il numero di definizione del data item di gruppo da esaminare.<br>
	 * <p>
	 * Il secondo parametro indica il tipo di data items definiti sotto il gruppo da restituire:<br>
	 * <p>
	 * <b>DATA_ITEMS_GROUP_FIELDS </b> <br>  		 
	 *    Restituiti solo i campi di gruppo del gruppo<br>
	 * <p>
	 * <b>DATA_ITEMS_ELEMENTARY_FIELDS</b> <br>  
	 *    Restituiti solo i campi elementari del gruppo 
	 * <p> 
	 * <b>DATA_ITEMS_ALL</b> <br>		     
	 *    Restituiti tutti i campi del gruppo (altri gruppi e dati elementari)
	 * 
	 * @parm int dataNameGroupPointer
	 * @parm int typeDataItemsToExtract da AmritaConstants
	 * @return ArrayList<InstructionCobolDataItem> con i data item defini nel gruppo
	 * 
	 */
 	public ArrayList<InstructionCobolDataItem> dataItemsUnderGroup(int dataNameGroupPointer, int typeDataItemsToExtract){
 		
		ProgramCobolEntry<? extends Instruction> entryData = null;  		// Singolo entry di data division
 		InstructionCobolDataItem dataItemCobol = null;             			// Singola definizione di data item
  		ArrayList<InstructionCobolDataItem> al_dataItemUnderGroup = null;	// ArrayList di di output
		int ar_dataItemPointer[] = null;									// Pointers data item da portare in output
 		int i = 0;
 		al_dataItemUnderGroup = new ArrayList<InstructionCobolDataItem>();
 		
 		ar_dataItemPointer = dataItemsUnderGroupPointers(dataNameGroupPointer, typeDataItemsToExtract);
 		
 		// Scan data items estratti
 		for (i = 0; i < ar_dataItemPointer.length; i++) {
 			entryData = this.entryDataDivision(i);
 			dataItemCobol = (InstructionCobolDataItem) entryData.getInstruction();
 			al_dataItemUnderGroup.add(dataItemCobol);
		}
   		return al_dataItemUnderGroup;
    }

 	
	/**
	 * 
	 * Restituisce i nomi dei data item elementari, non di gruppo, definiti sotto il campo gruppo fornito in input.
	 * <p>
	 * Il primo parametro è il numero di definizione del data item di gruppo da esaminare.<br>
	 * Il secondo parametro indica il tipo di data items definiti sotto il gruppo da restituire:<br>
	 * <p>
	 * <b>DATA_ITEMS_GROUP_FIELDS </b> <br>  		 
	 *    Restituiti solo i campi di gruppo del gruppo<br>
	 * <p>
	 * <b>DATA_ITEMS_ELEMENTARY_FIELDS</b> <br>  
	 *    Restituiti solo i campi elementari del gruppo 
	 * <p> 
	 * <b>DATA_ITEMS_ALL</b> <br>		     
	 *    Restituiti tutti i campi del gruppo (altri gruppi e dati elementari)
	 * 
	 * @parm int dataNameGroupPointer
	 * @parm int typeDataItemsToExtract da AmritaConstants
	 * @return String[] con i nomi dei data item elementari defini nel gruppo
	 * 
	 */
 	public String[] dataItemsUnderGroupNames(int dataNameGroupPointer, int typeDataItemsToExtract){
 		
		ProgramCobolEntry<? extends Instruction> dataEntry = null;	// Singolo entry di data division
 		InstructionCobolDataItem dataItemCobol = null;             	// Singola definizione di data item
 		String ar_dataItemElementaryName[] = null;					// Array con nomi campi elementari del gruppo da portare in output
 		int ar_dataItemElementaryPointer[] = null;      			// Array con nomi campi elementari del gruppo da portare in output
   		int dataItemPointer = 0;                        			// Pointer a definizijne dato elementare
 		
 		
 		ar_dataItemElementaryPointer = dataItemsUnderGroupPointers(dataNameGroupPointer, typeDataItemsToExtract);
 		
 		// Pointer out of range o campo non di gruppo
 		if (ar_dataItemElementaryPointer == null) {
			return null;
		}

 		
 		ar_dataItemElementaryName = new String[ar_dataItemElementaryPointer.length];
 		
 		// Scan pointers ai campi elementari del gruppo
 		for (int i = 0; i < ar_dataItemElementaryPointer.length; i++) {
 			
 			dataItemPointer = ar_dataItemElementaryPointer[i];
 			dataEntry = al_DataEntry.get(dataItemPointer);
 			dataItemCobol = (InstructionCobolDataItem) dataEntry.getInstruction();
 			
 			ar_dataItemElementaryName[i] = dataItemCobol.getDataName();
		}
 		
 	 
 		// Restituisce i nomi alle definizioni di campi elementari dentro il gruppo

 		return ar_dataItemElementaryName;
    }


	/**
	 * 
	 * Restituisce i puntatori alle definizioni dei data item, che ridefiniscono quello fornito in input.<br> 
	 * <p>
	 * In caso di richiesta non valida o nessun valore in output, viene restituito un array vuoto.<br>
	 * 
	 * @parm int dataNameNumber redefined
	 * @return int[] con i numeri di definizione dei data item elementari defini nel gruppo
	 * 
	 */
 	public int[] dataItemsRedefinePointers(int numInstrDataNameRedefined){
 		
		ProgramCobolEntry<? extends Instruction> dataEntry = null;  // Singolo entry di data division
 		InstructionCobolDataItem dataItemCobol = null;             	// Singola definizione di data item
  		ArrayList<Integer> al_dataItemRedefines = null;				// ArrayList di servizio
		String idDataNameRedefined = null;                          // Nome campo di cui si cercano le ridefinizioni
  		int ar_dataItemRedefines[] = null;							// Array con numeri istruzione di definizione da portare in output
		int lvlDataItemRedefined = 0;                       		// Numero livello data item ridefinito
 		int lvlDataItemLoop = 0;                        			// Numero livello data item in ciclo
 		
 		al_dataItemRedefines = new ArrayList<Integer>();
 		
 		// Pointer out of range
 		if (numInstrDataNameRedefined >= al_DataEntry.size()) {
 			return new int[] {};
		}
 		
 		// Estraggo entry definizione data item da condiderare
		dataEntry = al_DataEntry.get(numInstrDataNameRedefined);
		
		// L'entry non contiene una istruzione di definizione dati: return
		if (!(dataEntry.getInstruction() instanceof InstructionCobolDataItem)) {
			return new int[] {};
		}
		
		dataItemCobol = (InstructionCobolDataItem) dataEntry.getInstruction();
		lvlDataItemRedefined = dataItemCobol.getLevelNumber();
		idDataNameRedefined = dataItemCobol.getDataName();

		// Scan definizioni definite sotto il campo ridefinito successivamente
 		for (int i = numInstrDataNameRedefined + 1; i < al_DataEntry.size(); i++) {

 			dataEntry = al_DataEntry.get(i);
 			
 			// L'entry non contiene una istruzione di definizione dati: skip
 			if (!(dataEntry.getInstruction() instanceof InstructionCobolDataItem)) {
  				continue;
 			}
			
 			// Estraggo istruzione e numero di livello
 			dataItemCobol = (InstructionCobolDataItem) dataEntry.getInstruction();
 			lvlDataItemLoop = dataItemCobol.getLevelNumber();
 			
 			// Livello 77: stop
 			if (lvlDataItemLoop == 77) {
				break;
			}
 			
 			// Livello 88: continue
 			if (lvlDataItemLoop == 88) {
 				continue;
			}
 			
 			// Livello 66: continue
 			if (lvlDataItemLoop == 66) {
 				continue;
			}

			// Livello - annidato: fine
 			if (lvlDataItemLoop < lvlDataItemRedefined) {
				break;
			}

			// Livello + annidato: continuo la ricerca dello stesso livello o <, per uscire
 			if (lvlDataItemLoop > lvlDataItemRedefined) {
				continue;
			}

 			// Stesso livello: verifico se redefines
 			
  			// Non è redefines: cerca il prossimo
 			if (!dataItemCobol.isRedefinesClause()) {
				continue;
			}

 			// Non ridefinisce il campo in input: skip 
 			if (!dataItemCobol.getRedefinesDataName().equals(idDataNameRedefined)) {
				continue;
			}
  			
			// Il campo ridefinisce quello in input: accodo numero istruzione di definizione
 			al_dataItemRedefines.add(i);
 		}
 		
 		// Converto ArrayList in Array di interi di output
 		ar_dataItemRedefines = new int[al_dataItemRedefines.size()];
 		for (int i = 0; i < ar_dataItemRedefines.length; i++) {
 			ar_dataItemRedefines[i] = al_dataItemRedefines.get(i).intValue();
		}
 
 		// Restituisce i pointer alle definizioni di campi he ridefiniscono il campo in input

 		return ar_dataItemRedefines;
    }


	/**
	 * 
	 * Restituisce il nome del data item, fornito il suo numero di definizione.<br> 
	 * <p>
	 * E' una scorciatoia per non richiamare <b>entryDataDivision(int)</b> e
	 * successivamente recuperare il nome del campo dall'istruzione DataItem.
	 * Se non si riesce a determinare il nome viene restituito null.
	 * <p>
	 * @param int numDefinition
	 * @return String con il nome del campo
	 * 
	 */
 	public String dataItemName (int numDefinition){
 		
 		ProgramCobolEntry<? extends Instruction> innerDefinition = null;
 		InstructionCobolDataItem dataItem = null;
 
 		// Richiesta definizione fuori range
 		if (numDefinition >= al_DataEntry.size()) {
			return null;
		}
 		
 		innerDefinition = al_DataEntry.get(numDefinition);
 		
 		// Non è la definizione di un data name
 		if (innerDefinition.getEntryType() != EnumInstrDataCategory.COBOL_DATA_ITEM) {
			return null;
		}
 		
 		// estraggo la definizione
 		dataItem = (InstructionCobolDataItem) innerDefinition.getInstruction();
 		
      	return dataItem.getDataName();
    }


 
	/**
	 * 
	 * Restituisce il qualificatote di utilizzo in input o in output del data item utilizzato dall'istruzione<br> 
	 * <p>
	 * Se il numero di istruzione è errato viene restituito null.<br>
	 * Se il numero di definizione è errato viene restituito null.<br>
	 * Se il data item non ha utilizzi in input restituisce null<br>
	 * <p>
	 * @param int numInstruction
	 * @param String dataName
	 * @param String typeUseDataItem che può valere INSTR_USE_DATA_ITEM_INPUT/INSTR_USE_DATA_ITEM_OUTPUT
	 * @return DataItemQualifier con le qualificazioni di utilizzo
	 * 
	 */
 	public DataItemQualifier dataItemQualifier (int numInstruction, String dataName, String typeUseDataItem){
 		
 		ProgramCobolEntry<? extends Instruction> entryProcedure = null;
 		InstructionCobolProcedure instruction = null;
 		DataItemQualifier dataItemQualifier = null;
 		
 		// Controllo sia richiesta un'istruzione corretta
 		if (numInstruction >= al_ProcedureEntry.size()) {
			return null;
		}
  		
 		// Recupero entry di procedure division
 		entryProcedure = al_ProcedureEntry.get(numInstruction);
 		
 		// L'entry non contiene un'istruzione di procedure division
 		if (!(entryProcedure.getInstruction() instanceof InstructionCobolProcedure)) {
 			return null;
		}
 		
 		// Recuperoistruzione contenuta
 		instruction = (InstructionCobolProcedure) entryProcedure.getInstruction();
 		
 		// Recupero qualificazione di utilizzo in input del data item, nell'istruzione
 		if (typeUseDataItem == INSTR_USE_DATA_ITEM_INPUT) {
 			dataItemQualifier = instruction.symbolUsedInput(dataName);
		} else {
			dataItemQualifier = instruction.symbolUsedOutput(dataName);
		}

 		return dataItemQualifier;
    }

	/**
	 * 
	 * Restituisce array con le coppie nome item/qualificatori dei data item utilizzati dall'istruzione<br> 
	 * <p>
	 * Se il numero di istruzione è errato viene restituito null.
	 * <p>
	 * @param int numDefinition
	 * @param String typeUseDataItem che può valere INSTR_USE_DATA_ITEM_INPUT/INSTR_USE_DATA_ITEM_OUTPUT
	 * @return Set<Entry<String, DataItemQualifier>> con i data items in input
	 * 
	 */
 	public Instruction.InnerSymbolEntry[] dataItemsUsed (int numInstruction, String typeUseDataItem){
 		
 		ProgramCobolEntry<? extends Instruction> entryProcedure = null;
 		InstructionCobol instructionCobol = null;
 		
 		// Controllo sia richiesta un'istruzione corretta
 		if (numInstruction >= al_ProcedureEntry.size()) {
			return null;
		}
 		
 		// recupero entry di procedure e istruzione
 		entryProcedure = al_ProcedureEntry.get(numInstruction);
 		instructionCobol = (InstructionCobol) entryProcedure.getInstruction();
 		
 		// Restituisco i data items utilizzati in input o in output
		if (typeUseDataItem == INSTR_USE_DATA_ITEM_INPUT) {
			return instructionCobol.symbolsUsedInput();
		} else {
			return instructionCobol.symbolsUsedOutput();
		}
    }
    /**
     * Restituisce le definizioni dati a livello 88 codificate dopo il data item fornito.<br>
     * <p>
     * Se il data item non ha livelli 88 viene restituito un insieme vuoto.
     * <p>
     * @param InstructionCobolDataItem dataItem to ebvaluate
     * @return ArrayList<InstructionCobolDataItem> 88 level data item
     */
	public ArrayList<InstructionCobolDataItem> dataItemsLvl88(InstructionCobolDataItem dataItem) {
		
		ProgramCobolEntry<? extends Instruction>[] ar_entryData = null;
		ProgramCobolEntry<? extends Instruction> entryData = null;
		ArrayList<InstructionCobolDataItem> al_dataItemsLvl88 = null;
		InstructionCobolDataItem dataItemLvl88 = null;
		int i = 0;
		
		al_dataItemsLvl88 = new ArrayList<InstructionCobolDataItem> ();
		ar_entryData = this.entriesData();
		
		// Scan definizioni dati a partire dalla sucesiva a quella fornita
		for (i = dataItem.getNumInstr() + 1; i < ar_entryData.length; i++) {
			entryData = ar_entryData[i];
			
			// Interessano solo le istruzioni di definizione dati di livello 88
			if (!(entryData.getInstruction() instanceof InstructionCobolDataItem)) {continue;} 
			dataItemLvl88 = (InstructionCobolDataItem) entryData.getInstruction();
			if (dataItemLvl88.getLevelNumber() != 88) {break;}
			
			// Intabello la definizione di livello 88
			al_dataItemsLvl88.add(dataItemLvl88);
			
		}
		
		return al_dataItemsLvl88;
	}


 	/**
	 * 
	 * Restituisce i nomi dei data item utilizzati in input o in output dall'istruzione<br> 
	 * <p>
	 * Se il numero di istruzione è errato viene restituito null.
	 * <p>
	 * @param int numDefinition
	 * @param String useDataItem INSTR_USE_DATA_ITEM_INPUT o INSTR_USE_DATA_ITEM_OUTPUT
	 * @return String[] con il nome dei campi
	 * 
	 */
 	public String[] dataItemsNamesUsed (int numInstruction, String useDataItem){

 		String dataName = "";											// Nome data item
 		ArrayList<String> al_dataName = null;                  			// Elenco nomi data item
 		String ar_dataName[] = null;                  					// Elenco nomi data item
 		Instruction.InnerSymbolEntry[] ar_EntryDataItems = null;    			// Simboli restituiti dall'istruzione
 		
 		al_dataName = new ArrayList<String>();

 		// Estrazione di tutti i simboli in input o in output per l'istruzione
		if (useDataItem.equals(INSTR_USE_DATA_ITEM_INPUT)) {
			ar_EntryDataItems = dataItemsUsed(numInstruction, INSTR_USE_DATA_ITEM_INPUT);
		} else {
			ar_EntryDataItems = dataItemsUsed(numInstruction, INSTR_USE_DATA_ITEM_OUTPUT);
		}
 		
		// Numero di istruzione errata
		if (ar_EntryDataItems == null) {
			return null;
		}
		
 		// Scan simboli utilizzati dall'istruzione: interessa solo il nome
 		for (Instruction.InnerSymbolEntry entrySymbol : ar_EntryDataItems) {
 			dataName = entrySymbol.symbolName;
 			al_dataName.add(dataName);
 		}
   		
 		// Conversione ArrayList in Array
 		ar_dataName = new String[al_dataName.size()];
 		ar_dataName = al_dataName.toArray(ar_dataName);
  		
 		return ar_dataName;
    }

	/**
	 * 
	 * Restituisce i nomi dei data item utilizzati sia in input sia in output dall'istruzione<br> 
	 * <p>
	 * Se il numero di istruzione è errato viene restituito null.
	 * <p>
	 * @param int numDefinition
	 * @return String[] con il nome dei campi
	 * 
	 */
 	public String[] dataItemsNamesUsedBoth (int numInstruction){

 		String ar_dataNameInput[] = null;            // Elenco nomi data item
 		String ar_dataNameOutput[] = null;           // Elenco nomi data item
 		String ar_dataNameInputOutput[] = null;      // Elenco nomi data item
        int lengthInputOutput = 0;                   // Dimensioni arrau di output
 		int iOutput = 0;
        
		// Estraggo nomi di data itemutilizzati in input e in output
        ar_dataNameInput = dataItemsNamesUsed(numInstruction, INSTR_USE_DATA_ITEM_INPUT);
        ar_dataNameOutput = dataItemsNamesUsed(numInstruction, INSTR_USE_DATA_ITEM_OUTPUT);
 		
 		// Alloco il nuovo Array risultato
        if (ar_dataNameInput != null) {
        	lengthInputOutput = ar_dataNameInput.length;
		}
        if (ar_dataNameOutput != null) {
        	lengthInputOutput = lengthInputOutput + ar_dataNameOutput.length;
		}
        ar_dataNameInputOutput = new String[lengthInputOutput];
		
        // Copio array data items in input
		if (ar_dataNameInput != null) {
			for (int i = 0; i < ar_dataNameInput.length; i++) {
				ar_dataNameInputOutput[iOutput] = ar_dataNameInput[i];
				iOutput++;
			}
		}
		
	    // Copio array data items in Output
		if (ar_dataNameOutput != null) {
			for (int i = 0; i < ar_dataNameInput.length; i++) {
				ar_dataNameInputOutput[iOutput] = ar_dataNameOutput[i];
				iOutput++;
			}
		}
   		
 		return ar_dataNameInputOutput;
    }


 
 
 	/**
	 * 
	 * Restituisce i numeri delle definizioni dei data item utilizzati in input o in output dall'istruzione<br> 
	 * <p>
	 * Per ogni simbolo qualificato viene cercato il numero di definizione dove questo è definito.
	 * Se il numero di istruzione è errato viene restituito null.
	 * <p>
	 * @param int numInstruction
	 * @param Strin useDataItem (INSTR_USE_DATA_ITEM_INPUT o INSTR_USE_DATA_ITEM_OUTPUT)
	 * @return int[] con i pointers alle definizioni dei campi
	 * 
	 */
 	public int[] xrefToDataItemsUsedByInstr (int numInstruction, String useDataItem){
 
 		String dataName = "";									// Nome simbolo
 		DataItemQualifier qualifier = null;						// Mappato dal nome del simbolo
 		ArrayList<Integer> al_dataItemInputPointer = null;      // Per eliminare doppioni 
 		int dataItemPointer = 0;                                // Singolo pointer a definizione
  		int[] ar_DataItemInputPointer = null;					// Pointers da portare in output
  		
  		// Simboli in input restituiti dall'istruzione
  		Instruction.InnerSymbolEntry ar_EntryDataItemsInput[] = null;
 		
 		al_dataItemInputPointer = new ArrayList<Integer>();

 		// Estrazione di tutte le coppie simbolo/qualificazioni in input per l'istruzione
		if (useDataItem.equals(INSTR_USE_DATA_ITEM_INPUT)) {
			ar_EntryDataItemsInput = dataItemsUsed(numInstruction, INSTR_USE_DATA_ITEM_INPUT);
		} else {
			ar_EntryDataItemsInput = dataItemsUsed(numInstruction, INSTR_USE_DATA_ITEM_OUTPUT);
		}
 				
 		// Scan simboli utilizzati in input o in output dall'istruzione
		for (Instruction.InnerSymbolEntry entrySymbol : ar_EntryDataItemsInput) {
 			dataName = entrySymbol.symbolName;
 			qualifier = entrySymbol.qualifier;
  			// Utilizzo semplice non qualificato
 			if (qualifier == null) {
 				dataItemPointer = dataItemPointer(dataName, "");
 				al_dataItemInputPointer.add(dataItemPointer);
 				continue;
 			}
 			// Al data item non è richiesto di essere definito sotto un group
			if (qualifier.isUnderGroupDeclared()) {
				al_dataItemInputPointer.add(dataItemPointer);
  				continue;
 			}
			// Utilizzo qualificato dal campo di gruppo (dichiarazione nome-campo OF nome-gruppo
			dataItemPointer = dataItemPointer(dataName, qualifier.getGroupNameField());
			al_dataItemInputPointer.add(dataItemPointer);

 		}
   		
 		// Allocazione array puntatori di output
 		ar_DataItemInputPointer = new int[al_dataItemInputPointer.size()];
 		
 		// Trasformo array di integer in array di int
  		for (int i = 0; i < ar_DataItemInputPointer.length; i++) {
 			Integer objInteger = (Integer) al_dataItemInputPointer.get(i);
 		 	ar_DataItemInputPointer[i] = objInteger.intValue();
		}
       	return ar_DataItemInputPointer;
    }


	/**
	 * 
	 * Restituisce i pointer alle definizioni dei campi utilizzati in input insieme a quelli utilizzati in output<br> 
	 * dell'istruzione il cui numero è fornito in input.
	 * 
	 * <p>
	 * @param int numInstruction
	 * @return int[] con  pointer alle definizioni
	 * 
	 */
 	public int[] xrefToDataItemsUsedByInstrBoth(int numInstruction){
 		
 		int[] ar_DataItemInputPointer = null;					// Pointers da portare in output
		int[] ar_DataItemOutputPointer = null;					// Pointers da portare in output
		int[] ar_DataItemInputOutputPointer = null;			    // Unione dei precedenti
        int iOutput = 0;
        
		// estraggo pointers a campi utilizzati in input e in output
		ar_DataItemInputPointer = xrefToDataItemsUsedByInstr(numInstruction, INSTR_USE_DATA_ITEM_INPUT);
		ar_DataItemOutputPointer = xrefToDataItemsUsedByInstr(numInstruction, INSTR_USE_DATA_ITEM_OUTPUT);
 		
		// Alloco il nuovo Arrauìy risultato
		ar_DataItemInputOutputPointer = new int[ar_DataItemInputPointer.length + ar_DataItemOutputPointer.length];
		
		// Copio i due array in quello di output
		for (int i = 0; i < ar_DataItemInputPointer.length; i++) {
			ar_DataItemInputOutputPointer[iOutput] = ar_DataItemInputPointer[i];
			iOutput++;
		}
		for (int i = 0; i < ar_DataItemOutputPointer.length; i++) {
			ar_DataItemInputOutputPointer[iOutput] = ar_DataItemOutputPointer[i];
			iOutput++;
		}
		
     	return ar_DataItemInputOutputPointer;
    }

 	

 
	/**
	 * 
	 * Restituisce i puntatori alle istruzioni che referenziano una Section Cobol<br> 
	 * <p>
	 * Coincide con i numeri delle istruzioni del programma.<br>
	 * Se la section non è referenziata restituisce array vuoto.<br>
	 * <p>
	 * @param String section name
	 * @return int[] con il numero di istruzione che richiama la Section
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] xrefToSection(String sectionName){
 		
 		Object ar_o[] = null;
 		int ar_XrefInputProcedure[] = null;
 		ArrayList<Integer> al_XrefInputProcedure = null;
 		ArrayList<Integer> al_InstructionEntry = null;
 	
 		ar_o = map_Symbol.get(sectionName); 
 		
		// Verifica se la section è definito come simbolo
 		if (ar_o == null) {
 			return new int[0];
		}
 
 		// Verifica se il simbolo è una Section
		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL)) {
 			return new int[0];
		}

 		al_InstructionEntry = (ArrayList<Integer>) ar_o[2];
 		
 		// Verifica se la section è definita come definizione
		if (al_InstructionEntry.size() == 0) {
			return new int[0];
		}
	 
		al_XrefInputProcedure =  (ArrayList<Integer>) ar_o[3];
        
		// Converto in array semplice di interi
		ar_XrefInputProcedure = new int[al_XrefInputProcedure.size()];
		for (int i = 0; i < ar_XrefInputProcedure.length; i++) {
			ar_XrefInputProcedure[i] = al_XrefInputProcedure.get(i);
		}
     	return ar_XrefInputProcedure;
    }

	/**
	 * 
	 * Restituisce i puntatori alle istruzioni che referenziano una label Cobol<br> 
	 * <p>
	 * Coincide con il numeri delle istruzioni di programma.<br>
	 * Se la section non è referenziata restituisce array vuoto.<br>
	 * <p>
	 * @param String label name
	 * @return int[] con il numero di istruzione che richiama la label
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] xrefToLabel(String labelName){
 		
		Object ar_o[] = null;
 		int ar_XrefInputProcedure[] = null;
 		ArrayList<Integer> al_XrefInputProcedure = null;
 	    ArrayList<Integer> al_InstructionEntry = null;
 		
 		ar_o = map_Symbol.get(labelName); 
 		
		// Verifica se la label è definito come simbolo
 		if (ar_o == null) {
			return new int[0];
		}
 
 		// Verifica se il simbolo è una label o un paragrafo
		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_LABEL)
 		&&  !al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL)) {
 			return new int[0];
		}

 		al_InstructionEntry = (ArrayList<Integer>) ar_o[2];
 		
 		// Verifica se la label è definita come definizione
		if (al_InstructionEntry.size() == 0) {
			return new int[0];
		}
	 
		al_XrefInputProcedure =  (ArrayList<Integer>) ar_o[3];
        
		// Converto in array semplice di interi
		ar_XrefInputProcedure = new int[al_XrefInputProcedure.size()];
		for (int i = 0; i < ar_XrefInputProcedure.length; i++) {
			ar_XrefInputProcedure[i] = al_XrefInputProcedure.get(i);
		}
 		
     	return ar_XrefInputProcedure;
    }

	/**
	 * 
	 * Restituisce i puntatori alle istruzioni Cobol Perform a section o procedure<br> 
	 * <p>
	 * Coincide con il numeri delle istruzioni di programma.<br>
	 * La procedura può essere una section Cobol oppure un paragrafo.
	 * In entrambi i casi la procedura è richiamata con Perform o Perform Thru.<br>
	 * Si tenta inizialmente considerando come paragrafo.<br>
	 * Se non si trovano riferimenti si tenta allora come section.<br>
	 * Se la procedura non è referenziata restituisce un array vuoto.<br>
	 * <p>
	 * @param String procedure name
	 * @return int[] con il numero di istruzione che richiama la procedure
	 * 
	 */
 	public int[] xrefToProcedureInternal(String procedureName){
 		
 		ArrayList<Integer> al_XrefInputProcedure = null;
 		int ar_XrefInputProcedure[] = null;
  		
 		al_XrefInputProcedure = new ArrayList<Integer> ();
 		
 		// Procedure richiamata con Perform o Perform Thru o dichiarata in sort/merge
		ar_XrefInputProcedure = xrefToLabel(procedureName);
		if (ar_XrefInputProcedure != null) {
			
			// Scan references
			for (int i : ar_XrefInputProcedure) {
				// E' una perform o una dichiarazione sort/merge
				if (this.entryProcedure(i).getTypeInstr() == EnumCobolReservedWords.PROC_PERFORM
				||  this.entryProcedure(i).getTypeInstr() == EnumCobolReservedWords.PROC_SORT	
				||  this.entryProcedure(i).getTypeInstr() == EnumCobolReservedWords.PROC_MERGE) {
					al_XrefInputProcedure.add(i);
				}
			}
			ar_XrefInputProcedure = new int[al_XrefInputProcedure.size()];
			for (int i = 0; i < al_XrefInputProcedure.size(); i++) {
				ar_XrefInputProcedure[i] = al_XrefInputProcedure.get(i);
			}
			return ar_XrefInputProcedure;
		}
  		 
		
 	    // Procedure richiamata con Perform o Perform Thru
		ar_XrefInputProcedure = xrefToSection(procedureName);
		
		if (ar_XrefInputProcedure != null) {
			
			// Scan references
			for (int i : ar_XrefInputProcedure) {
				// E' una perform
				if (this.entryProcedure(i).getTypeInstr() == EnumCobolReservedWords.PROC_PERFORM) {
					al_XrefInputProcedure.add(i);
				}
			}
			ar_XrefInputProcedure = new int[al_XrefInputProcedure.size()];
			for (int i = 0; i < al_XrefInputProcedure.size(); i++) {
				ar_XrefInputProcedure[i] = al_XrefInputProcedure.get(i);
			}
	     	return ar_XrefInputProcedure;
		}
		
		return new int[0];
    }

 
	/**
	 * 
	 * Restituisce i puntatori alle istruzioni che referenziano il data item in procedure division<br> 
	 * <p>
	 * Se il data item non è definito restituisce null.
	 * Se il data item non è referenziato restituisce null.
	 * Viene fornito un parametro per restituire i riferimenti in input o in output
	 * <p>
	 * @param int pointer definizione data item
	 * @param String INSTR_USE_DATA_ITEM_INPUT o INSTR_USE_DATA_ITEM_OUTPUT
	 * @return int[] con il numero di istruzione che referenziano il data name
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] xrefToDataItemInProcedure(int pointerDataItem, String typeXref){
 		
 		InstructionCobolDataItem dataItem = null;				                // Istruzione di definizione dati data item
 		ArrayList<Integer> al_XrefProcedure = null;
 		ArrayList<Integer> al_PointerToDefinitionEntry = null;					// Da elemento (1) mappato in Map simboli
		Object ar_o[] = null;
  		String dataName = "";                                                  // 
 		int ar_XrefProcedure[] = null;
       
        dataItem = this.dataItemDefinition(pointerDataItem);
        dataName = dataItem.getDataName();
 		
 		
 		ar_o = map_Symbol.get(dataName); 
		 		
		// Verifica se il data item è definito come simbolo
 		if (ar_o == null) {
			return null;
		}
 
 		// Verifica se il simbolo è un data item
		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_DATA_ITEM)) {
			return null;
		}

		// Pointers alle definizioni del data item
 		al_PointerToDefinitionEntry = (ArrayList<Integer>) ar_o[1];
 		
 		// Verifica se il data item esiste come definizione 
		if (al_PointerToDefinitionEntry.size() == 0) {
			return null;
		}
        
		// Xref in input o in output
		if (typeXref.equals(INSTR_USE_DATA_ITEM_INPUT)) {
			al_XrefProcedure =  (ArrayList<Integer>) ar_o[3];
		} else {
			al_XrefProcedure =  (ArrayList<Integer>) ar_o[5];
		}
 		
		// Converto in array semplice di interi
		ar_XrefProcedure = new int[al_XrefProcedure.size()];
		for (int i = 0; i < ar_XrefProcedure.length; i++) {
			ar_XrefProcedure[i] = al_XrefProcedure.get(i);
		}
	 	
		// Null se no references
		if (ar_XrefProcedure.length == 0) {
			ar_XrefProcedure = null;
		}
		
    	return ar_XrefProcedure;
    }

	/**
	 * 
	 * Restituisce i puntatori alle istruzioni che implicitamente referenziano 
	 * il data item in procedure division, attraverso assegnazioni a campi redefines,
	 * renames e di gruppo.<br> 
	 * <p>
	 * Se il data item non è definito restituisce null.
	 * Se il data item non è referenziato implicitamente restituisce un insieme vuoto.
	 * Viene fornito un parametro per restituire i riferimenti in input o in output
	 * <p>
	 * @param String data name
	 * @param String INSTR_USE_DATA_ITEM_INPUT o INSTR_USE_DATA_ITEM_OUTPUT
	 * @return int[] con il numero di istruzione che referenziano il data name
	 * 
	 */
 	public int[] xrefToDataItemInProcedureImplicit(int pointerDataItemOrigin, String typeXref){
 
 		ProgramCobolEntry<? extends Instruction> entryInstr = null;         // Entry con istruzione
 		InstructionCobolProcedure instrMove = null;                         // Move di xref
		ArrayList<ArrayList<Integer>> al_implicitDataItem = null;         	// ArrayList bidimensionale restituito in output
		ArrayList<Integer> al_implicitDataItemPointer = null;   			// Pointers a  data item impliciti                      
		ArrayList<Integer> al_implicitDataItemPos = null;   				// Posizione inizio in data item impliciti                      
		ArrayList<Integer> al_implicitDataItemLng = null;   				// Lunghezza utile in data item impliciti                      
		ArrayList<Integer> al_implicitXref = null;   				        // Xref di tutti i data item implicit   
		ArrayList<DataItemCobolIdentifier> al_identifierMoveTo = null;      // Identificatori completi output in Move
		DataItemCobolIdentifier identifierMoveFrom = null;                  // Identificatore completo input in Move
		int implicitDataItemPointer = 0;                                    // Pointer a definizione data item implicito
		int[] ar_xrefDataItemInProcedure = null;							// Pointers xref data item in procedure
        int[] ar_implicitXref = null;										// Pointers xref data item impliciti
        int implicitPos = 0;                                                // Posizione implicita 
        int implicitLng = 0;                                                // Lunghezza utile implicita
       
        al_implicitXref = new ArrayList<Integer> ();
  		
		
		// Recupero tutti i data item impliciti, con relativa posizione e lunghezza
 		al_implicitDataItem = this.dataItemsImplicit(pointerDataItemOrigin);
 		
 		// Data item forntito errato 
 		if (al_implicitDataItem == null) {
			return null;
		}
 		
 		// Data item definito, pointers, posizione e lunghezza
 		al_implicitDataItemPointer = al_implicitDataItem.get(0);
 		al_implicitDataItemPos = al_implicitDataItem.get(1);
 		al_implicitDataItemLng = al_implicitDataItem.get(2);
 		
		// Scan data item impliciti potenzialmente utilizzati nel programma
 		for (int i = 0; i < al_implicitDataItemPointer.size(); i++) {
			
 			implicitDataItemPointer = al_implicitDataItemPointer.get(i);
 			implicitPos = al_implicitDataItemPos.get(i);
 			implicitLng = al_implicitDataItemLng.get(i);
 			
			// Xref data item implicito in tutti i punti del programma
			ar_xrefDataItemInProcedure = this.xrefToDataItemInProcedure(implicitDataItemPointer, typeXref);
			
			// Nessuna movimentazione
			if (ar_xrefDataItemInProcedure == null) {
				continue;
			}
			
			// Scan xref a utilizzi impliciti
			for (int xrefPointer : ar_xrefDataItemInProcedure) {
				
				entryInstr = this.entryProcedure(xrefPointer);
                
				// Non è una move: porto in output
				if (entryInstr.getTypeInstr() != EnumCobolReservedWords.PROC_MOVE) {
					al_implicitXref.add(Integer.valueOf(xrefPointer));
					continue;
				}
				
				// E' una move: verifica pos/lng di reference modification
				instrMove = (InstructionCobolProcedure) entryInstr.getInstruction();
				
				identifierMoveFrom = instrMove.moveGetIdentifierFrom();
				al_identifierMoveTo = instrMove.moveGetIdentifiersTo();
				
				// Richiesti xref impliciti di input
				if (typeXref.equals(INSTR_USE_DATA_ITEM_INPUT)) {
					
					// E' il campo in input alla move che interessa
					if (identifierMoveFrom.getNumInstr() == implicitDataItemPointer) {
						addXrefPointerImplicit(xrefPointer, identifierMoveFrom, al_implicitXref, implicitPos, implicitLng);
					}
					
				} // end-if xref in input
				
				
				
				// Richiesti xref impliciti di output
				
				// Scan identificatori in output
				for (DataItemCobolIdentifier identifierMoveTo : al_identifierMoveTo) {

					// Non è il campo in output alla move che interessa
					if (identifierMoveTo.getNumInstr() != implicitDataItemPointer) {continue;}
					
					addXrefPointerImplicit(xrefPointer, identifierMoveTo, al_implicitXref, implicitPos, implicitLng);

				} // end-for identificatori di output
				
			} // end-for xref data item implicito
			 
		} // end-for data item impliciti
		
 		
		// Converto in array semplice di interi
		ar_implicitXref = new int[al_implicitXref.size()];
		for (int i = 0; i < ar_implicitXref.length; i++) {
			ar_implicitXref[i] = al_implicitXref.get(i);
		}
		
    	return ar_implicitXref;
    }


 	/*
 	 * Inserisce i pointer di xref impliciti
 	 * 
 	 * 
 	 */
	private void addXrefPointerImplicit(int xrefPointer								 // Pointer specifo xref (Move)
			                          , DataItemCobolIdentifier identifierMove       // Identificatore Move From/To
			                          , ArrayList<Integer> al_implicitXref			 // Xref di tutti i data item implicit  
			                          , int implicitPos								 // Posizion implicita
			                          , int implicitLng								 // Lunghezza utile implicita
			                          ) {

 		DataItemQualifier qualifierMoveTo = null;                           // Qualificatore completo output in Move
		ExpressionCobol exprPos = null;										// Espressione pos di reference modification
		ExpressionCobol exprLng = null;										// Espressione lng di reference modification
		ExpressionCobolElement[] ar_exprPosElement = null;					// Elementi di espressione pos di reference modification
		@SuppressWarnings("unused")
		ExpressionCobolElement[] ar_exprLngElement = null;					// Elementi di espressione lng di reference modification
		ExpressionCobolElement exprPosElement = null;					    // Elemento di espressione pos di reference modification
        int posRefMod = 0;                                                  // Posizione reference modification
 
		qualifierMoveTo = identifierMove.getQualifier();
		
		// Niente reference modification: campo modificato completamente
		if (!qualifierMoveTo.isThereRefModification()) {
			al_implicitXref.add(Integer.valueOf(xrefPointer));
			return;
		}
		
		// Estrazione espressioni reference modification
		exprPos = qualifierMoveTo.getPos();
		exprLng = qualifierMoveTo.getLength();
		
		// Estrazione elementi espressioni reference modification
		ar_exprPosElement = exprPos.getElements();
		ar_exprLngElement = exprLng.getElements();
		
		// Posizione espressa da + di un operando: campo potenzialmente modificato 
		if (ar_exprPosElement.length > 1) {
			al_implicitXref.add(Integer.valueOf(xrefPointer));
			return;
		}
		
		exprPosElement = ar_exprPosElement[0];
		
		// Posizione espressa da un campo: campo potenzialmente modificato 
		if (exprPosElement.getReservedWordCobol() == EnumCobolReservedWords.DATA_DIV_DATA_ITEM) {
			al_implicitXref.add(Integer.valueOf(xrefPointer));
			return;
		}
		
		// Posizione espressa da literal: verifica se dentro il campo
		if (exprPosElement.getReservedWordCobol() == EnumCobolReservedWords.OPERAND_LITERAL_NUM) {
			posRefMod = exprPosElement.getValueNumeric();
			
			// Pos in reference modification dentro il campo: campo sicuramente modificato  
			if (posRefMod >= implicitPos 
			&& posRefMod <= implicitPos + implicitLng - 1) {
				al_implicitXref.add(Integer.valueOf(xrefPointer));
				}
		}
		
        return;
		
		
	}


	/**
	 * 
	 * Restituisce i puntatori alle istruzioni di procedure che referenziano il data item
	 * qualificato dalla clausola OF.<br> 
	 * <p>
	 * Viene fornito in input anche il nome del data item di gruppo sotto il quale è definito il data item.
	 * Campi definiti con lo stesso nome in gruppi diversi sono a tutti gli effetti campi diversi.
	 * Se il data item non è referenziato restituisce null.
	 * <p>
	 * @param String data name
	 * @param String underGroupName
	 * @param String INSTR_USE_DATA_ITEM_INPUT o INSTR_USE_DATA_ITEM_OUTPUT
	 * @return int[] con il numero di istruzione che referenziano il data name
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] xrefToDataItemInProcedureOf(String dataName, String underGroupName, String typeXref){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
  		int cntUnderGroup = 0;                                  // Di servizio
		ArrayList<Integer> al_XrefProcedure = null;				// ArrayList Xref in input o in output
		int ar_XrefProcedureOf[] = null;						// Array di output con le istruzioni che referenziano il data item
		ArrayList<Integer> al_XrefProcedureOf = null;		    // ArrayList Xref in input o in output
 		ArrayList<Integer> al_PointerToDefinitionEntry = null;	// Da elemento (1) mappato in Map simboli
 	
 		ar_o = map_Symbol.get(dataName); 
		 		
		// Verifica se il data item è definito come simbolo
 		if (ar_o == null) {
			return null;
		}
        
  		// Verifica se il simbolo è un data item
		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_DATA_ITEM)) {
			return null;
		}
        
 		// Pointers alle definizioni del data item
 		al_PointerToDefinitionEntry = (ArrayList<Integer>) ar_o[1];
 		
 		// Verifica se il data item esiste come definizione 
 		if (al_PointerToDefinitionEntry == null) {
			return null;
		}
		if (al_PointerToDefinitionEntry.size() == 0) {
			return null;
		}

		// Verifica se il data item, per qualche sua definizione, esiste sotto il gruppo fornito in input
		for (Integer pointerToDef : al_PointerToDefinitionEntry) {
			if (!isDataItemUnderGroupNameAnyLevel(pointerToDef, underGroupName))  {
				continue;
			} 
			cntUnderGroup++;								// Conter definizioni sotto gruppi diversi
		}
		
		// Data item non definito sotto il gruppo fornito
		if (cntUnderGroup == 0) {
			return null;
		}


		// Mi interessa solo il data item definito sotto il gruppo indicato
		// Devo recuperare tutte le istruzioni che referenziano il data item
		// Per ogni istruzione devo considerare solo quelle nei cui simboli utilizzati
		// è presente lo stesso data item, qualificato dal gruppo fornito in ingresso (OF del Cobol)
		
		// Array list di istruzioni referenziate e di output
		al_XrefProcedure = new ArrayList<Integer>();
		al_XrefProcedureOf = new ArrayList<Integer>();
		
 		
		// Xref in input o in output
		if (typeXref.equals(INSTR_USE_DATA_ITEM_INPUT)) {
			al_XrefProcedure =  (ArrayList<Integer>) ar_o[3];
		} else {
			al_XrefProcedure =  (ArrayList<Integer>) ar_o[5];
		}
 

		// Scan istruzioni che referenziano il data item in input o in output
		for (Integer pointerToInstr : al_XrefProcedure) {
			
			ProgramCobolEntry<?> defInstr= al_ProcedureEntry.get(pointerToInstr);
			InstructionCobol instr = (InstructionCobol) defInstr.getInstruction();
			
			// Estrazione simboli utilizzati dall'istruzione sia in input sia in output
			Instruction.InnerSymbolEntry ar_dataItemUsed[] = instr.symbolsUsed( );
			
			// Scan set con coppie simbolo/qualificatore
			for (Instruction.InnerSymbolEntry entry : ar_dataItemUsed) {
				
				// Utilizzi di data item diversi: skip
				if (!entry.symbolName.equalsIgnoreCase(dataName)) {
					continue;
				}
				
				// Verifico utilizzo del data item qualificato dal gruppo
				DataItemQualifier qualifier = entry.qualifier;
				
				// Utilizzo di Data Item non qualificato da un gruppo: skip
				if (!qualifier.isUnderGroupDeclared()) {
					continue;
				}
				
				// Utilizzo di Data Item qualificato da un gruppo diverso da quello richiesto: skip
				if (!qualifier.getGroupNameField().equalsIgnoreCase(underGroupName)) {
					continue;
				}
				
				// L'istruzione utilizza il campo qualificato dal gruppo: la accodo a struttura xref
				al_XrefProcedureOf.add(pointerToInstr);
			}
		}
		
		
		// Converto in array semplice di interi
		ar_XrefProcedureOf = new int[al_XrefProcedureOf.size()];
		for (int i = 0; i < ar_XrefProcedureOf.length; i++) {
			ar_XrefProcedureOf[i] = al_XrefProcedureOf.get(i);
		}

     	return ar_XrefProcedureOf;
    }

 
 

	/**
	 * 
	 * Restituisce i puntatori alle definizioni che referenziano il data item, in data division<br> 
	 * <p>
	 * Per esempio vengono restituiti tutti i data item che ridefiniscono un campo.
	 * Se il data item non è referenziata restituisce null.
	 * <p>
	 * @param String data name
	 * @return int[] con il numero di istruzione che referenziano il data name
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] xrefToDataItemInDataDivision(String dataName){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
 		int ar_XrefDataDefOut[] = null;							// Array di output con le definizioni che referenziano il data item
		ArrayList<Integer> al_XrefDataDefOut = null;			// Da elemento (1) mappato in Map simboli
 		ArrayList<Integer> al_PointerToDefinitionEntry = null;	// Di servizio
  		
 		ar_o = map_Symbol.get(dataName); 
 		
		// Verifica se il data item è definito come simbolo
 		if (ar_o == null) {
			return null;
		}
        
 		// Verifica se il simbolo è un data item
		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_DATA_ITEM)) {
			return null;
		}
        
 		// Pointers alle definizioni del data item
 		al_PointerToDefinitionEntry = (ArrayList<Integer>) ar_o[1];
 		
 		// Verifica se il data item esiste come definizione 
		if (al_PointerToDefinitionEntry.size() == 0) {
			return null;
		}
		 
		al_XrefDataDefOut = (ArrayList<Integer>) ar_o[4];
 		
		// Converto in array semplice di interi
		ar_XrefDataDefOut = new int[al_XrefDataDefOut.size()];
		for (int i = 0; i < ar_XrefDataDefOut.length; i++) {
			ar_XrefDataDefOut[i] = al_XrefDataDefOut.get(i);
		}

		return ar_XrefDataDefOut;
    }

 	/**
	 * 
	 * Restituisce i puntatori alle definizioni che referenziano il data item, in data division 
	 * con la clausola OF group-name<br>
	 * <p>
	 * Viene fornito anche il nome del data item di gruppo sotto il quale è definito il data item
	 * Se il data item non è referenziata restituisce null.
	 * <p>
	 * @param String data name
	 * @return int[] con il numero di istruzione che referenziano il data name
	 * 
	 */
 	public int[] xrefToDataItemInDataDivisionOf(String dataName, String underGroupName){
 		
		int ar_XrefDataDef[] = null;					// Array di output con le definizioni che referenziano il data item
		int ar_XrefDataDefOut[] = null;					// Array di output con le definizioni che referenziano il data item
		ArrayList<Integer> al_XrefDataDefOut = null;	// Di servizio
        
		al_XrefDataDefOut = new ArrayList<Integer>();
		
		// Estrazione di tutti i riferimenti di definizione, al data item specificato
		ar_XrefDataDef = xrefToDataItemInDataDivision(underGroupName);
 		
		// Scan data item che refeferenziano quello fornito in input
		for (int pointerDef : ar_XrefDataDef) {
			// Data item non definito nel gruppo fornito: skip
			if (groupOwnerDefinition(pointerDef) == -1) {
				continue;
			}
			// Pointer da portare in output
			al_XrefDataDefOut.add(pointerDef);
		}
		
		// Converto in array semplice di interi
		ar_XrefDataDefOut = new int[al_XrefDataDefOut.size()];
		for (int i = 0; i < ar_XrefDataDefOut.length; i++) {
			ar_XrefDataDefOut[i] = al_XrefDataDefOut.get(i);
		}
		
     	return ar_XrefDataDefOut;
    }

	/**
	 * 
	 * Restituisce i puntatori alle definizioni che referenziano il simbolo, in environment division<br> 
	 * <p>
	 * Per esempio vengono restituiti tutti gli statement di environment dove una literal è utilizzata
	 * o un data  item referenziato.
	 * Se il simbolo non è referenziata restituisce null.
	 * <p>
	 * @param String symbol name
	 * @return int[] con il numero di istruzione che referenziano il simbolo
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] xrefToSymbolInEnvDivision(String symbolName){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
 		int ar_XrefEnvDefOut[] = null;							// Array di output con le definizioni che referenziano il data item
		ArrayList<Integer> al_XrefEnvDefOut = null;			    // Da elemento (7) mappato in Map simboli
  		
 		ar_o = map_Symbol.get(symbolName); 
 		
		// Verifica se il simbolo è definito 
 		if (ar_o == null) {
			return null;
		}
 		// Riferimenti in data division
 		al_XrefEnvDefOut = (ArrayList<Integer>) ar_o[7];
 		
		// Converto in array semplice di interi
 		ar_XrefEnvDefOut = new int[al_XrefEnvDefOut.size()];
		for (int i = 0; i < ar_XrefEnvDefOut.length; i++) {
			ar_XrefEnvDefOut[i] = al_XrefEnvDefOut.get(i);
		}

		// Null se no references
		if (ar_XrefEnvDefOut.length == 0) {
			ar_XrefEnvDefOut = null;
		}

		return ar_XrefEnvDefOut;
    }


	/**
	 * 
	 * Restituisce i puntatori alle definizioni che referenziano il simbolo, in data division<br> 
	 * <p>
	 * Per esempio vengono restituiti tutti i data item che referenziano una costante figurativa.
	 * Se il simbolo non è referenziata restituisce null.
	 * <p>
	 * @param String symbol name
	 * @return int[] con il numero di istruzione che referenziano il simbolo
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] xrefToSymbolInDataDivision(String symbolName){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
 		int ar_XrefDataDefOut[] = null;							// Array di output con le definizioni che referenziano il data item
		ArrayList<Integer> al_XrefDataDefOut = null;			// Da elemento (1) mappato in Map simboli
  		
 		ar_o = map_Symbol.get(symbolName); 
 		
		// Verifica se il simbolo è definito 
 		if (ar_o == null) {
			return null;
		}
 		// Riferimenti in data division
		al_XrefDataDefOut = (ArrayList<Integer>) ar_o[4];
 		
		// Converto in array semplice di interi
		ar_XrefDataDefOut = new int[al_XrefDataDefOut.size()];
		for (int i = 0; i < ar_XrefDataDefOut.length; i++) {
			ar_XrefDataDefOut[i] = al_XrefDataDefOut.get(i);
		}

		// Null se no references
		if (ar_XrefDataDefOut.length == 0) {
			ar_XrefDataDefOut = null;
		}

		return ar_XrefDataDefOut;
    }


	/**
	 * 
	 * Restituisce i puntatori alle istruzioni che referenziano il simbolo in procedure division<br> 
	 * <p>
	 * Se il simbolo non è definito restituisce null.<br>
	 * Se il simbolo non è referenziato restituisce null.<br>
	 * Viene fornito un parametro per restituire i riferimenti in input o in output<br>
	 * <p>
	 * @param String symbol
	 * @param String INSTR_USE_DATA_ITEM_INPUT o INSTR_USE_DATA_ITEM_OUTPUT
	 * @return int[] con il numero di istruzione che referenziano il data name
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] xrefToSymbolInProcedure(String symbolName, String typeXref){
 		
 		Object ar_o[] = null;
 		int ar_XrefProcedure[] = null;
 		ArrayList<Integer> al_XrefProcedure = null;
 	
 		ar_o = map_Symbol.get(symbolName); 
		 		
		// Verifica se il simbolo è definito come simbolo
 		if (ar_o == null) {
			return null;
		}
 
		// Xref in input o in output in procedure
		if (typeXref.equals(INSTR_USE_DATA_ITEM_INPUT)) {
			al_XrefProcedure =  (ArrayList<Integer>) ar_o[3];
		} else {
			al_XrefProcedure =  (ArrayList<Integer>) ar_o[5];
		}
 		
		// Converto in array semplice di interi
		ar_XrefProcedure = new int[al_XrefProcedure.size()];
		for (int i = 0; i < ar_XrefProcedure.length; i++) {
			ar_XrefProcedure[i] = al_XrefProcedure.get(i);
		}
	 	
		// Null se no references
		if (ar_XrefProcedure.length == 0) {
			ar_XrefProcedure = null;
		}
		
    	return ar_XrefProcedure;
    }



	/**
	 * 
	 * Restituisce i nomi di tutti i simboli di un certo tipo non referenziati da nessuna istruzione di procedure division<br> 
	 * <p>
	 * Il tipo di simbolo può essere Label, Section o Data Item come speccificato in {@link AmritaConstants}
	 * <p>
	 * @return String[] con i nomi dei data item non referenziate
	 * 
	 */
 	public String[] unXrefSymbols(EnumSymbolType symbolTypeParm){
 		
		Object ar_o[] = null;                                   // Mappato da ogni simbolo
		EnumSymbolType symbolType = null; 						// Da elemento (0) mappato in Map simboli
 		Set<Entry<String, Object[]>> set_EntrySymbol = null;	// Entry elemento map simboli
		String ar_SymbolOut[] = null;						    // Array di output con label non referenziate
		ArrayList<String> al_SymbolOut = null;					// Di servizio
        
		al_SymbolOut = new ArrayList<String>();
		
		// Set di coppie simbolo/Object[]
 		set_EntrySymbol = map_Symbol.entrySet();
 		
 		// Scan simboli definiti
 		for (Entry<String, Object[]> entry : set_EntrySymbol) {
			
 			// Array di oggetti associati al simbolo
 			ar_o = entry.getValue();
 			
			// Tipologia simbolo nel primo elemento
 			symbolType = (EnumSymbolType) ar_o[0];
	 		
	 		// Non il tipo di simbolo richiesto: skip
	 		if (symbolType != symbolTypeParm) {
				continue;
			}
	        
	 		// Simbolo non referenziato in procedure division: porto in output
	 		if (ar_o[2] == null) {
	 			al_SymbolOut.add(entry.getKey());
			}
		}
 		
		// Converto in array semplice di Stringhe
 		ar_SymbolOut = new String[al_SymbolOut.size()];
 		ar_SymbolOut = al_SymbolOut.toArray(ar_SymbolOut);
     	
      	return null;
    }

 	/**
	 * 
	 * Restituisce tutte le label definite nel programma<br> 
	 * <p>
	 * @return String[] con i nomi delle label definite
	 * 
	 */
 	public String[] labelNames(){
 		
		Object ar_o[] = null;                                   // Mappato da ogni simbolo
		EnumSymbolType symbolType = null;    					// Da elemento (0) mappato in Map simboli
 		Set<Entry<String, Object[]>> set_EntrySymbol = null;	// Entry elemento map simboli
		String ar_LabelOut[] = null;							// Array di output con label non referenziate
		ArrayList<String> al_LabelOut = null;					// Di servizio
        
		al_LabelOut = new ArrayList<String>();
		
		// Set di coppie simbolo/Object[]
 		set_EntrySymbol = map_Symbol.entrySet();
 		
 		// Scan simboli definiti
 		for (Entry<String, Object[]> entry : set_EntrySymbol) {
 			// Array di oggetti associati al simbolo
 			ar_o = entry.getValue();
 			
			// Tipologia simbolo nel primo elemento
 			symbolType = (EnumSymbolType) ar_o[0];
	 		
	 		// Label: porto in outout
	 		if (symbolType == EnumSymbolType.COBOL_SYMBOL_LABEL
	 		||  symbolType == EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL) {
	 			al_LabelOut.add(entry.getKey().trim());
				continue;
			}
		}
 		
		// Converto in array semplice di Stringhe
 		ar_LabelOut = new String[al_LabelOut.size()];
 		ar_LabelOut = al_LabelOut.toArray(ar_LabelOut);
 		
     	return ar_LabelOut;
    }

	/**
	 * 
	 * Restituisce tutte le label definite in una section del programma <br> 
	 * <p>
	 * Se la section non è definita restituisce null
	 * 
	 * @param String sectionName
	 * @return String[] con i nomi delle label definite
	 * 
	 */
 	public String[] labelNames(String sectionName){
 		
 		int pointerToSection = 0;
		String ar_LabelOut[] = null;									// Array di output con label non referenziate
		ArrayList<String> al_LabelOut = null;							// Di servizio
 		ProgramCobolEntry<? extends Instruction> entry = null;			// Descrittore entry di procedure division
 		InstructionCobolProcedure instrCobol = null;					// Istruzione cobol di procedure

 		// Recupero pointer a istruzione descrivente la Section
 		pointerToSection = sectionPointer(sectionName);
 		if (pointerToSection == -1) {
			return null;
		}

		al_LabelOut = new ArrayList<String>();

		// Scan istruzioni a partire da quella successiva alla section
 		// fino alla successiva section o all fine del programma
 		for (int i = pointerToSection + 1; i < al_ProcedureEntry.size(); i++) {
 			
 			entry = entryProcedure(i);
 			
 			// Non è una istruzione di procedure: skip
 			if (!(entry.getInstruction() instanceof InstructionCobolProcedure)) {
				continue;
			}
 			
 			// Estraggo istruzione
			instrCobol = (InstructionCobolProcedure) entry.getInstruction();
 			
 			// Nuova Section: exit
  			if (instrCobol.getTypeInstr() == EnumCobolReservedWords.PROC_SECTION) {
				break;
			}
  			
  			// Non è una label: skip
			if (instrCobol.getTypeInstr() != EnumCobolReservedWords.PROC_LABEL) {
				continue;
			}
            
 		    // Label: la porto in output
			al_LabelOut.add(instrCobol.labelGetName());
 		}
 		
		// Converto in array semplice di Stringhe
 		ar_LabelOut = new String[al_LabelOut.size()];
 		ar_LabelOut = al_LabelOut.toArray(ar_LabelOut);

 		return null;
    }

	/**
	 * 
	 * Restituisce tutte le label definite fra due label di un programma <br> 
	 * <p>
	 * Se labelFrom o labelTo non è definita restituisce null
	 * 
	 * @return String[] con i nomi delle label definite
	 * 
	 */
 	public String[] labelNames(String labelFrom, String labelTo){
 		
		int pointerToLabelFrom = 0;								// Pinter a istruzione Label from
		int pointerToLabelTo = 0;								// Pinter a istruzione Label to
		String ar_LabelOut[] = null;							// Array di output con label non referenziate
		ArrayList<String> al_LabelOut = null;					// Di servizio
 		ProgramCobolEntry<? extends Instruction> entry = null;		// Descrittore entry di procedure division
 		InstructionCobolProcedure instrCobol = null;					// Istruzione cobol

		al_LabelOut = new ArrayList<String>();

 		// Recupero pointer a istruzione descrivente la Section
		pointerToLabelFrom = labelPointer(labelFrom);
		pointerToLabelTo = labelPointer(labelTo);
 		if (pointerToLabelFrom == -1 || pointerToLabelTo == -1) {
			return null;
		}
		
		// Scan istruzioni a partire da quella successiva alla label di partenza
 		// fino alla successiva section o all fine del programma
 		for (int i = pointerToLabelFrom + 1; i < al_ProcedureEntry.size(); i++) {
 			
			entry = entryProcedure(i);
 			
 			// Non è una istruzione di procedure: skip
 			if (!(entry.getInstruction() instanceof InstructionCobolProcedure)) {
				continue;
			}
 			
 			// Estraggo istruzione
			instrCobol = (InstructionCobolProcedure) entry.getInstruction();
 			
 			// Inizio di una Section: exit
  			if (instrCobol.getTypeInstr() == EnumCobolReservedWords.PROC_SECTION) {
				break;
			}
  			
			// Non è una label: skip
			if (instrCobol.getTypeInstr() != EnumCobolReservedWords.PROC_LABEL) {
				continue;
			}
  			
 			// Label di fine: exit
			if (instrCobol.labelGetName().equalsIgnoreCase(labelTo)) {
				break;
			}

  		    // Label: la porto in output
			al_LabelOut.add(instrCobol.labelGetName());
 		}
 		
		// Converto in array semplice di Stringhe
 		ar_LabelOut = new String[al_LabelOut.size()];
 		ar_LabelOut = al_LabelOut.toArray(ar_LabelOut);
 		
     	return null;
    }

	/**
	 * 
	 * Restituisce i nomi di tutte le procedure interne definite nel programma <br> 
	 * <p>
	 * Si tratta di Section e paragrafi richiamati con perform o eventualmente solo definiti.
	 * <p>
	 * @return String[] con i nomi delle section/paragrafi definite nel programma
	 * 
	 */
	public String[] procInternalNames(){
 		
 		InstructionCobolProcedure instrProc = null;
		String ar_procInternallOut[] = null;					// Array di output con procedure interne
		ArrayList<String> al_procInternallOut = null;			// Di servizio
        
		al_procInternallOut = new ArrayList<String>();
		
		// Scan entries procedure
		for (ProgramCobolEntry<? extends Instruction> entryProc : this.entriesProcedure()) {
			
			// Section: sicuramente procedura interna, anche se dead code
			if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_SECTION) {
				instrProc = (InstructionCobolProcedure) entryProc.getInstruction();
				al_procInternallOut.add(instrProc.sectionGetName());
				continue;
			}
			
			// Label  
			if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_LABEL) {
				instrProc = (InstructionCobolProcedure) entryProc.getInstruction();
				// Paragrafo: è una procedura interna, anche se dead code
				if (instrProc.labelIsParagraph()) {
					al_procInternallOut.add(instrProc.labelGetName());
				}
				continue;
			}
		}
 		
		// Converto in array semplice di Stringhe
		ar_procInternallOut = new String[al_procInternallOut.size()];
		ar_procInternallOut = al_procInternallOut.toArray(ar_procInternallOut);
 		
     	return ar_procInternallOut;
    }

	/**
	 * 
	 * Restituisce i nomi di tutte le section definite nel programma. <br> 
	 * <p>
	 * @return String[] con i nomi delle section definite nel programma
	 * 
	 */
 	public String[] sectionNames(){
 		
		InstructionCobolProcedure instrSection = null;
		String ar_SectionlOut[] = null;							// Array di output con label non referenziate
		ArrayList<String> al_SectionlOut = null;				// Di servizio
		
		al_SectionlOut = new ArrayList<String>();
		
		// Scan istruzioni di programma
        for (ProgramCobolEntry<? extends Instruction> entryProc : this.entriesProcedure()) {
			if (entryProc.getTypeInstr() != EnumCobolReservedWords.PROC_SECTION) {continue;}
			instrSection = (InstructionCobolProcedure) entryProc.getInstruction();
			al_SectionlOut.add(instrSection.sectionGetName());
  		}
 		
		// Converto in array semplice di Stringhe
 		ar_SectionlOut = new String[al_SectionlOut.size()];
 		ar_SectionlOut = al_SectionlOut.toArray(ar_SectionlOut);
 		
     	return ar_SectionlOut;
    }

	/**
	 * Restituisce i nomi di tutti i paragrafi definiti nel programma<br> 
	 * <p>
	 * Si tratta di paragrafi dichiarati nella sezione DECLARATIVES, <br>
	 * eseguiti con perform o codificati e chiusi formalmente da EXIT e <br>
	 * non richiamati.<br>
	 * <p>
	 * @return String[] con i nomi dei paragrafi definiti nel programma
	 * 
	 */
	public String[] paragraphNames(){
 		
		InstructionCobolProcedure instrLabel = null;
		String ar_paragraphOut[] = null;						// Array di output con paragrafi
		ArrayList<String> al_paragraphOut = null;				// Di servizio
		
		al_paragraphOut = new ArrayList<String>();
		
		// Scan istruzioni di programma
        for (ProgramCobolEntry<? extends Instruction> entryProc : this.entriesProcedure()) {
			if (entryProc.getTypeInstr() != EnumCobolReservedWords.PROC_LABEL) {continue;}
    		instrLabel = (InstructionCobolProcedure) entryProc.getInstruction();
    		
    		// E' un paragrafo
    		if (instrLabel.labelIsParagraph()) {
    			al_paragraphOut.add(instrLabel.labelGetName());
    		}

		}
 		
		// Converto in array semplice di Stringhe
 		ar_paragraphOut = new String[al_paragraphOut.size()];
 		ar_paragraphOut = al_paragraphOut.toArray(ar_paragraphOut);
 		
     	return ar_paragraphOut;
    }

	/**  
	 * Restituisce tutte le section chiamanti dead code, senza ulteriori chiamanti.<br>
	 * <p>
	 * Viene analizzato qualsiasi livello di annidamento delle section.<br>
	 * Della section fornita si individuano i punti dove è performata.<br>
	 * Di ogni punto si individua la section di appartenenza e se si trova una section
	 * non + performata si restituisce in output
	 * 
	 * @param String sectionName
	 * @return ArrayList<String> al_numSectionDeadCodeCaller
	 * 
	 */
	public ArrayList<String> sectionCallerDeadCode(String sectionName) {
		
		ArrayList<String> al_sectionDeadCodeCaller = null;
		ArrayList<String> al_sectionAlreadyManaged = null;
		al_sectionDeadCodeCaller = new ArrayList<String> ();
		al_sectionAlreadyManaged = new ArrayList<String> ();
		sectionCallerDeadCodeRecursive(sectionName, al_sectionDeadCodeCaller, al_sectionAlreadyManaged);

		return al_sectionDeadCodeCaller;
	}

	/**  
	 * Restituisce tutte le section chiamanti no dead code, richiamate direttamente dalla mainline<br>
	 * <p>
	 * Viene analizzato qualsiasi livello di annidamento delle section.<br>
	 * Della section fornita si individuano i punti dove è performata.<br>
	 * Di ogni punto si individua la section di appartenenza e se si prosegue
	 * fino a trivare una attivazione dalla mainline del programma.
	 * 
	 * @param String sectionName
	 * @return ArrayList<String> al_numSectionDeadCodeCaller
	 * 
	 */
	public ArrayList<String> sectionCallerNoDeadCode(String sectionName) {
		
		ArrayList<String> al_sectionNoDeadCodeCaller = null;
		ArrayList<String> al_sectionAlreadyManaged = null;
		al_sectionNoDeadCodeCaller = new ArrayList<String> ();
		al_sectionAlreadyManaged = new ArrayList<String> ();
		sectionCallerNoDeadCodeRecursive(sectionName, al_sectionNoDeadCodeCaller, al_sectionAlreadyManaged);
			
		return al_sectionNoDeadCodeCaller;
	}

	/**  
	 * Restituisce tutte le section chiamanti, senza discriminare se dead code o meno<br>
	 * <p>
	 * Viene analizzato qualsiasi livello di annidamento delle section.<br>
	 * Della section fornita si individuano i punti dove è performata.<br>
	 * Di ogni punto si individua la section di appartenenza e se si trova una section
	 * non + performata si restituisce in output
	 * 
	 * @param String sectionName
	 * @return ArrayList<String> al_numSectionDeadCodeCaller
	 * 
	 */
	public ArrayList<String> sectionCaller(String sectionName) {
		
		ArrayList<String> al_sectionNoDeadCodeCaller = null;
		ArrayList<String> al_sectionAlreadyManaged = null;
		al_sectionNoDeadCodeCaller = new ArrayList<String> ();
		al_sectionAlreadyManaged = new ArrayList<String> ();
		sectionCallerNoDeadCodeRecursive(sectionName, al_sectionNoDeadCodeCaller, al_sectionAlreadyManaged);
		return al_sectionNoDeadCodeCaller;
	}

	/**  
	 * Restituisce i nomi di tutte le label presenti nella section fornita<br>
	 * <p>
	 * Se il nome della section è errato restituisce un array vuoto<br>
	 * 
	 * @param String sectionName
	 * @return ArrayList<String> al_label
	 * 
	 */
	public ArrayList<String> sectionLabels(String sectionName) {
		
		ProgramCobolEntry<? extends Instruction> entrySection = null;
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		InstructionCobolProcedure instrSection = null;
		InstructionCobolProcedure instrProc = null;
		ArrayList<String> al_label = null;
        int numInstrSection = 0;
        int numInstrSectionEnd = 0;
        int i = 0;
        
        al_label = new ArrayList<String> ();
        
        numInstrSection = this.sectionPointer(sectionName);
        if (numInstrSection < 0) {
        	return al_label;
		}
		
        // Info section fine
        entrySection = this.entryProcedure(numInstrSection);
        instrSection = (InstructionCobolProcedure) entrySection.getInstruction();
        numInstrSectionEnd = instrSection.sectionGetLastNumInstr();
        
		// Scan istruzioni dentro section per estrazione label
		for (i = numInstrSection + 1;  i <= numInstrSectionEnd; i++) {
			
			entryProc = this.entryProcedure(i);
			if (!(entryProc.getInstruction() instanceof InstructionCobolProcedure)) {continue;}

			// l'ultima label della section, anche se formalmente un paragrafo
			// NON si considera in quanto considerata label di chiusura, di solito
			// succeduta da uno statement EXIT
			instrProc = (InstructionCobolProcedure) entryProc.getInstruction();
			if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_LABEL
			&&  instrProc.getNumInstr() == numInstrSectionEnd) {
				break;
			}
			
			// Label dentro la section, NON di chiusura
			if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_LABEL) {
				al_label.add(instrProc.labelGetName());
			}
		} 
        
		return al_label;
	}

	/**  
	 * Restituisce il numero di istruzioni non LABEL presenti nella section fornita<br>
	 * <p>
	 * Se il nome della section è errato restituisce -1<br>
	 * 
	 * @param String sectionName
	 * @return int count instructions no label
	 * 
	 */
	public int sectionCountNoLabelInstr(String sectionName) {
		
		ProgramCobolEntry<? extends Instruction> entrySection = null;
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		InstructionCobolProcedure instrSection = null;
        int numInstrSection = 0;
        int numInstrSectionEnd = 0;
        int i = 0;
        int cntInstrNoLabel = 0;
        
        numInstrSection = this.sectionPointer(sectionName);
        if (numInstrSection < 0) {
        	return -1;
		}
		
        // Info section fine
        entrySection = this.entryProcedure(numInstrSection);
        instrSection = (InstructionCobolProcedure) entrySection.getInstruction();
        numInstrSectionEnd = instrSection.sectionGetLastNumInstr();
        
		// Scan istruzioni dentro section per estrazione label
		for (i = numInstrSection + 1;  i <= numInstrSectionEnd; i++) {
			
			entryProc = this.entryProcedure(i);
			
			// Label: skip
			if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_LABEL) {
				continue;
			}
			
			cntInstrNoLabel++;
		} 
        
		return cntInstrNoLabel;
	}

	
	/**  
	 * Restituisce true se la section fornita è dead code<br>
	 * <p>
	 * La section è dead code se non è mai referenziata o se seguendo la
	 * catena di section chiamanti si arriva a una non più referenziata.<br>
	 * Se tutte le section chiamanti di primo livello non sono referenziate
	 * viene restituito true<br>
	 * 
	 * @param String sectionName
	 * @return boolean true se dead code
	 * 
	 */
	public boolean isSectionDeadCode(String sectionName) {
		
		ArrayList<String> al_sectionDeadCodeCaller = null;
		ArrayList<String> al_sectionNoDeadCodeCaller = null;
		ArrayList<String> al_sectionAlreadyManaged = null;
		
		// Sicuramente dead code
		if (this.isSymbolUnXref(sectionName)) {
			return true;
		}
		
		al_sectionDeadCodeCaller = new ArrayList<String> ();
		al_sectionNoDeadCodeCaller = new ArrayList<String> ();
		al_sectionAlreadyManaged = new ArrayList<String> ();
		
		// Section da cui è partita la prima perform, dead code o n
		sectionCallerDeadCodeRecursive(sectionName, al_sectionDeadCodeCaller, al_sectionAlreadyManaged);
		al_sectionAlreadyManaged.clear();
		sectionCallerNoDeadCodeRecursive(sectionName, al_sectionNoDeadCodeCaller, al_sectionAlreadyManaged);
		
		// La section è proprio dead code
		if (al_sectionDeadCodeCaller.size() > 0
	    &&  al_sectionNoDeadCodeCaller.size() == 0) {
			return true;
		}
		
		return false;
	}


	
	/**  
	 * Restituisce true se il paragrafo fornito è dead code<br>
	 * <p>
	 * Si verifica semplicemente se la label del paragrafo è referenziata.<br>
	 * 
	 * @param String paragraphName
	 * @return boolean true se dead code
	 * 
	 */
	public boolean isParagraphDeadCode(String paragraphName) {
		
		// Non è un paragrafo
		if (!this.isParagraph(paragraphName)) {
			return false;
		}
		
		// Sicuramente dead code
		if (this.isSymbolUnXref(paragraphName)) {
			return true;
		}
		
		// TODO specifico per paraggrafo, come per section
		
		
		return false;
	}

	/**  
	 * Restituisce true se la procedura interna fornito è dead code<br>
	 * <p>
	 * La procedura interna è dead code se non è mai referenziata o se seguendo la
	 * catena di section o paragrafi chiamanti si arriva a una non più referenziata.<br>
	 * Se tutte le section o paragrafi chiamanti di primo livello non sono referenziati
	 * viene restituito true<br>
	 * <p>
	 * Se il nome fornito non è quello di una section o di un paragrafo,
	 * restituisce false.<br>
	 * <p>
	 * 
	 * @param String procInternalName
	 * @return boolean true se dead code
	 */
	public boolean isProcInternalDeadCode(String procInternalName) {
		
		// Paragrafo
		if (this.isParagraph(procInternalName)) {
			return isParagraphDeadCode(procInternalName);
		}
		
		// Section
		if (this.isSection(procInternalName)) {
			return isSectionDeadCode(procInternalName);
		}
		
		return false;
	}


	/**  
	 * Restituisce true se il paragrafo fornito è referenziato<br>
	 * <p>
	 * Il paragrafo può essere referenziato con perform o con goto.<br>
	 * 
	 * @param String paragraphName
	 * @return boolean true se dead code
	 * 
	 */
	public boolean isParagraphReferenced(String paragraphName) {
		
		// Non è un paragrafo
		if (!this.isParagraph(paragraphName)) {
			return false;
		}
		
		// Sicuramente dead code
		if (this.isSymbolUnXref(paragraphName)) {
			return false;
		}
		
		return true;
	}




	/**
	 * 
	 * Restituisce i nomi dei simboli non section e non label definiti nel programma <br> 
	 * <p>
	 * Si tratta di nomi di campi, literal, costanti figurative etc.
	 * @return String[] con i nomi delle section definite nel programma
	 * 
	 */
 	public String[] symbolNames(){
 		
		Object ar_o[] = null;                                   // Mappato da ogni simbolo
		EnumSymbolType symbolType = null;    					// Da elemento (0) mappato in Map simboli
 		Set<Entry<String, Object[]>> set_EntrySymbol = null;	// Entry elemento map simboli
		String ar_SymbolOut[] = null;							// Array di output con label non referenziate
		ArrayList<String> al_SymbolOut = null;				// Di servizio
        
		al_SymbolOut = new ArrayList<String>();
		
		// Set di coppie simbolo/Object[]
 		set_EntrySymbol = map_Symbol.entrySet();
 		  
 		// Scan simboli definiti
 		for (Entry<String, Object[]> entry : set_EntrySymbol) {
			
 			// Array di oggetti associati al simbolo
 			ar_o = entry.getValue();
 			
			// Tipologia simbolo nel primo elemento
 			symbolType = (EnumSymbolType) ar_o[0];
	 		
	 		// Section: porto in outout
	 		if (symbolType != EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL 
	 		&& symbolType != EnumSymbolType.COBOL_SYMBOL_LABEL) {
	 			al_SymbolOut.add(entry.getKey());
				continue;
			}
		}
 		
		// Converto in array semplice di Stringhe
 		ar_SymbolOut = new String[al_SymbolOut.size()];
 		ar_SymbolOut = al_SymbolOut.toArray(ar_SymbolOut);
 		
     	return ar_SymbolOut;
    }


 	/**
	 * 
	 * Restituisce la definizione completa dell' entry di procedure division<br> 
	 * <p>
	 * Coincide on il numero dell'istruzione.<br>
	 * Se il pointer fornito è errato restituisce null.<br>
	 * <p>
	 * @param int instructionPointer
	 * @return ProgramCobolEntry con la definizione completa dell'entry
	 * 
	 */
 	public ProgramCobolEntry<? extends Instruction> entryProcedure (int instructionPointer){
 		
 		ProgramCobolEntry<? extends Instruction> entryProcedure = null;
 		
  		// Istruzione richiesta fuori range 
 		if (instructionPointer >= al_ProcedureEntry.size()) {
			return null;
		}
 		
 		entryProcedure = al_ProcedureEntry.get(instructionPointer);
 		 		
 	   	return entryProcedure;
    }

	/**
	 * 
	 * Restituisce la definizione completa dell' entry di data division<br>
	 * <p> 
	 * Coincide coincide con il numero della definizione.<br>
	 * Se il pointer fornito è errato restituisce null.<br>
	 * <p>
	 * @param int definitionPointer
	 * @return ProgramCobolEntry con la definizione completa dell'entry
	 * 
	 */
 	public ProgramCobolEntry<? extends Instruction> entryDataDivision (int definitionPointer){
 		
 		ProgramCobolEntry<? extends Instruction> entryDataDivision = null;
 		
  		// Istruzione richiesta fuori range 
 		if (definitionPointer >= al_DataEntry.size()) {
			return null;
		}
 		
 		entryDataDivision = al_DataEntry.get(definitionPointer);
 		
	   	return entryDataDivision;
    }

	/**
	 * 
	 * Restituisce la definizione completa dell' entry di identification division<br> 
	 * <p> 
	 * Coincide coincide con il numero della definizione.<br>
	 * Se il pointer fornito è errato restituisce null.<br>
	 * <p>
	 * @param int definitionPointer
	 * @return ProgramCobolEntry con la definizione completa dell'entry
	 * 
	 */
 	public ProgramCobolEntry<? extends Instruction> entryIdentificationDivision (int definitionPointer){
 		
 		ProgramCobolEntry<? extends Instruction> entryIdentification = null;
 		
  		// Istruzione richiesta fuori range 
 		if (definitionPointer >= al_IdentificationEntry.size()) {
			return null;
		}
 		
 		entryIdentification = al_IdentificationEntry.get(definitionPointer);
 		
	   	return entryIdentification;
    }
 	
	/**
	 * 
	 * Restituisce la definizione completa dell' entry di environment division<br> 
	 * <p>
	 * Coincide coincide con il numero della definizione.<br>
	 * Se il pointer fornito è errato restituisce null.<br>
	 * <p>
	 * @param int definitionPointer
	 * @return ProgramCobolEntry con la definizione completa dell'entry
	 * 
	 */
 	public ProgramCobolEntry<? extends Instruction> entryEnvironmentDivision (int definitionPointer){
 		
 		ProgramCobolEntry<? extends Instruction> entryEnvironment = null;
 		
  		// Istruzione richiesta fuori range 
 		if (definitionPointer >= al_EnvironmentEntry.size()) {
			return null;
		}
 		
 		entryEnvironment = al_EnvironmentEntry.get(definitionPointer);
 		
	   	return entryEnvironment;
    }


	/**
	 * 
	 * Restituisce un oggetto InstructionCobolDataItem, memorizzato nell'entry del quale è fornito il numero<br> 
	 * <p>
	 * Se il pointer fornito è errato restituisce null.
	 * <p>
	 * @param int definitionPointer
	 * @return InstructionCobolDataItem con la definizione completa del data item
	 * 
	 */
 	public InstructionCobolDataItem instructionDataItem (int definitionPointer){
 		ProgramCobolEntry<? extends Instruction> entryDataItem = null;
 		
 		// Richiesta out of range
 		if (definitionPointer >= al_DataEntry.size()) {
			return null;
		}
 		
 		entryDataItem = al_DataEntry.get(definitionPointer);
 		
 		// L'entry non contiene un'istruzione di definizione dati: return null
 		if (!(entryDataItem.getInstruction() instanceof InstructionCobolDataItem)) {
			return null;
		}
 		
 		// Non è un entry di data item: restituisco null
 		if (!(entryDataItem.getEntryType() == EnumInstrDataCategory.COBOL_DATA_ITEM)) {
			return null;
		}
 		
 	   	return (InstructionCobolDataItem) entryDataItem.getInstruction();
    }

	/**
	 * 
	 * Restituisce l'struzione statement Cobol di data division, memorizzata nell'entry del quale è fornito il numero<br> 
	 * <p>
	 * Se il pointer fornito è errato restituisce null.
	 * <p>
	 * @param int definitionPointer
	 * @return InstructionCobolCopy con la definizione completa dello statement copy
	 * 
	 */
 	public InstructionCobol instructionCopyData (int definitionPointer){
 		ProgramCobolEntry<? extends Instruction> entryCopyCobol = null;
 		
 		// Richista out of rangeCopyStmt
 		
 		if (definitionPointer >= al_DataEntry.size()) {
			return null;
		}
 		
 		entryCopyCobol = al_DataEntry.get(definitionPointer);
 

 		// Non è un entry di copy stmt: restituisco null
 		if (!(entryCopyCobol.getEntryType() == EnumInstrDataCategory.COBOL_COPY_INSTRUCTION)) {
			return null;
		}

	   	return (InstructionCobol) entryCopyCobol.getInstruction();
    }

	/**
	 * 
	 * Restituisce l'struzione statement Cobol di procedure division, memorizzata nell'entry del quale è fornito il numero<br>
	 * <p> 
	 * Se il pointer fornito è errato restituisce null.
	 * <p>
	 * @param int definitionPointer
	 * @return InstructionCobolCopy con la definizione completa dello statement copy
	 * 
	 */
 	public InstructionCobol instructionCopyProc (int definitionPointer){
 		
 		ProgramCobolEntry<? extends Instruction> entryCopyCobol = null;
 		
 		// Richista out of range
 		if (definitionPointer >= al_DataEntry.size()) {
			return null;
		}
 		
 		entryCopyCobol = al_ProcedureEntry.get(definitionPointer);
 		
  
 		// Non è un entry di copy stmt: restituisco null
 		if (!(entryCopyCobol.getEntryType() == EnumInstrDataCategory.COBOL_COPY_INSTRUCTION)) {
			return null;
		}

	   	return (InstructionCobol) entryCopyCobol.getInstruction();
    }

 	
	/**
	 * 
	 * Restituisce l'istruzione in procedure division, memorizzata nell'entry del quale è fornito il numero<br>
	 * <p> 
	 * Il chiamante deve effettuare il casting all'oggetto istruzione corretto
	 * Se il pointer fornito è errato restituisce null.
	 * <p>
	 * @param int definitionPointer
	 * @return Object con istruzione di procedure division memorizzata nell'entry
	 * 
	 */
	public Object instructionProcedure (int instructionPointer){
 		
		ProgramCobolEntry<? extends Instruction> entryInstrCobol = null;
		Object instrObj = null;
		
 		// Richista out of ranged
 		if (instructionPointer >= al_ProcedureEntry.size()) {
			return null; 
		}
 		
 		entryInstrCobol = al_ProcedureEntry.get(instructionPointer);
		instrObj = (Object) entryInstrCobol.getInstruction();
	
 	   	return instrObj;
    }
 	
	/**
	 * 
	 * Restituisce l'struzione di precompilatore, memorizzata nell'entry del quale è fornito il numero<br> 
	 * <p>
	 * Se il pointer fornito è errato restituisce null.
	 * <p>
	 * @param int definitionPointer
	 * @return InstructionPrecompiler con l'istruzione diretta al precompilatore
	 * 
	 */
 	public InstructionCics instructionPrecompilerData (int definitionPointer){
		ProgramCobolEntry<? extends Instruction> entryPrecompliler = null;
 		
 		// Richista out of range
 		if (definitionPointer >= al_DataEntry.size()) {
			return null;
		}
 		
 		entryPrecompliler = al_DataEntry.get(definitionPointer);

		// L'entry non contiene un'istruzione di copy: return null
 		if (!(entryPrecompliler.getInstruction() instanceof InstructionCics)) {
			return null;
		}

		// E' un entry di precompiler 
 		if (entryPrecompliler.getEntryType() == EnumInstrDataCategory.CICS_PRECOMPILER
 		||  entryPrecompliler.getEntryType() == EnumInstrDataCategory.SQL_PRECOMPILER
 		||  entryPrecompliler.getEntryType() == EnumInstrDataCategory.DL1_PRECOMPILER) {
 			return (InstructionCics) entryPrecompliler.getInstruction();
 		}
 
 	   	return null;
    }
 	
 	
 	
	/**
	 * 
	 * Restituisce l'struzione di precompilatore, memorizzata nell'entry del quale è fornito il numero<br> 
	 * <p>
	 * Se il pointer fornito è errato restituisce null.
	 * <p>
	 * @param int definitionPointer
	 * @return InstructionPrecompiler con l'istruzione diretta al precompilatore
	 * 
	 */
 	public InstructionCics instructionPrecompilerProc (int definitionPointer){
		ProgramCobolEntry<? extends Instruction> entryPrecompliler = null;
 		
 		// Richiesta out of range
 		if (definitionPointer >= al_ProcedureEntry.size()) {
			return null;
		}
 		
 		entryPrecompliler = al_ProcedureEntry.get(definitionPointer);

		// L'entry non contiene un'istruzione di copy: return null
 		if (!(entryPrecompliler.getInstruction() instanceof InstructionCics)) {
			return null;
		}

		// E' un entry di precompiler 
 		if (entryPrecompliler.getEntryType() == EnumInstrDataCategory.CICS_PRECOMPILER
 		||  entryPrecompliler.getEntryType() == EnumInstrDataCategory.SQL_PRECOMPILER
 		||  entryPrecompliler.getEntryType() == EnumInstrDataCategory.DL1_PRECOMPILER) {
 			return (InstructionCics) entryPrecompliler.getInstruction();
 		}
 
 	   	return null;   
 	 }
 
	/**
	 * 
	 * Restituisce il numero dell'istruzione condizionale di più alto livello
	 * sotto la quale l'istruzione fornita in input è definita.<br> 
	 * <p>
	 * Se il pointer all'istruzione è errato si restituisce -1.<br>
	 * Se l'istruzione fornita non è sotto condizione si restituisce lo stesso numero di
	 * istruzione fornito in input.<br>
	 * <p>
	 * 
	 * @param int instructionPointer
	 * @return int con numero istruzione condizionale proprietaria
	 * 
	 */
 	public int instructionConditionOwnerMain(int instructionPointer){
 		
		ProgramCobolEntry<? extends Instruction> programEntry = null;          // Singolo entry di programma
        int numInstr = 0;
        int numInstrOwner = 0;
 
		// Controllo se out of range l'istruzione
 		if (instructionPointer >= al_ProcedureEntry.size()) {
			return -1;
		}
 		
 		// Estraggo entry con istruzione
 		programEntry = al_ProcedureEntry.get(instructionPointer);

 		// Controllo se l'istruzione è sotto condizione
        if (!programEntry.isUnderCondition()) {
			return instructionPointer;
		}
 		
		// Ahead setting
		numInstr = instructionPointer;
 		
		while (numInstr > 0) {
			programEntry = this.entryProcedure(numInstr);
			
			// Istruzione condizionale sicuramente terminata
			if (programEntry.getTypeInstr() == EnumCobolReservedWords.PROC_DIVISION
			||  programEntry.getTypeInstr() == EnumCobolReservedWords.PROC_LABEL
			||  programEntry.getTypeInstr() == EnumCobolReservedWords.PROC_SECTION) {
				break;
			}
			
			// If di più alto livello sotto la quale è definita l'assegnazione
			if (programEntry.getNumInstrRelated() == 0) {
				break;
			}
			numInstrOwner = numInstr;
 			numInstr = programEntry.getNumEntryOwnerConditional();			// In loop setting
  			
		} // end-while
 		
 		return numInstrOwner;
    }

	/**
	 * 
	 * Restituisce il nomi dei campi di gruppo sotto i quali il data item è definito<br> 
	 * <p>
	 * La funzione restituisce tutti i campi di gruppo sotto il quale il data item fornito è definito<br>
	 * L'array restituito ha come ultimo elemento il campo di gruppo più generale, con il numero di livello<br>
	 * più basso, come il livello 01. Il primo elemento contiene invece il campo di gruppo immediatamente <br>
	 * sotto il quale il data item è definito.<br>
	 * In Cobol, di fatto, le strutture devono iniziare con livelli 01 e, pertanto, l'ultimo numero di <br>
	 * livello nell'array dovrebbe essere 01.<br>
	 * Se il data item fornito è errato restituisce un array vuoto<br>
	 * Se il data item è a livello 1 o 77, restituisce un array vuoto<br>
	 * <p>
     * @param int dataNamePointer
	 * @return String dataName owner
	 * 
	 */
 	public String[] groupOwnerNames (int dataNamePointer){

 		ProgramCobolEntry<? extends Instruction> dataEntry = null;	// Singolo entry di data division
 		InstructionCobolDataItem dataItemCobol = null; 				// Singola istruzione di definizione di data item
 		String ar_groupOwnerNames[] = null;							// Array con nomi campi di gruppo da portare in output
 		ArrayList<String> al_groupOwnerNames = null;				// ArrayList di servizio
 		int lvlDataItem = 0;                           				// Numero livello data item
 		
 		al_groupOwnerNames = new ArrayList<String>();
 		
 		// Pointer out of range
 		if (dataNamePointer >= al_DataEntry.size()) {
			return new String[0];
		}
 		
 		dataEntry = al_DataEntry.get(dataNamePointer) ;
 		
 		// L'entry non contiene un'istruzioone di definizione dati: return array vuoto
 		if (!(dataEntry.getInstruction() instanceof InstructionCobolDataItem)) {
 			return new String[0];
		}
 		
 		// Estraggo istruzione di definizione data item e livello
		dataItemCobol = (InstructionCobolDataItem) dataEntry.getInstruction();
		lvlDataItem = dataItemCobol.getLevelNumber();
        
		// Livello 01 o 77: non può essere sotto un gruppo
		if (lvlDataItem == 01
		||  lvlDataItem == 77) {
			return new String[0];
		}
 		
		// Scan definizioni all'indietro 
 		for (int i = dataNamePointer - 1; i >= 0; i--) {

 			dataEntry = al_DataEntry.get(i);
 			
 	 		// L'entry vpn contiene un'istruzioone di definizione dati: skip
 	 		if (!(dataEntry.getInstruction() instanceof InstructionCobolDataItem)) {
 	 			continue;
 			}

 			// Estraggo istruzione di definizione data item e livello
 			dataItemCobol = (InstructionCobolDataItem) dataEntry.getInstruction();
 			lvlDataItem = dataItemCobol.getLevelNumber();
 			
 			// Livello 77: exit
 			if (lvlDataItem == 77) {
				break;
			}
 			
 			// Campo di gruppo
 			if (dataItemCobol.isGroupField()) {
 				al_groupOwnerNames.add(dataItemCobol.getDataName());
			}
 			
			// Livello 01: exit
 			if (lvlDataItem == 1) {
				break;
			}
		}
 		
 		// Converto ArrayList in Array di output
 		ar_groupOwnerNames = new String[al_groupOwnerNames.size()];
 		ar_groupOwnerNames = al_groupOwnerNames.toArray(ar_groupOwnerNames);
 		
  	   	return ar_groupOwnerNames;
    }
 	
 	
	/**
	 * 
	 * Restituisce il nome del campo di gruppo sotto il quale il data item è immediatamente definito<br> 
	 * <p>
	 * Il numero di livello del gruppo deve essere inferiore a quello del data item
	 * Se il data item fornito è errato restituisce null <br>
	 * Se il data item non è definito sotto un gruppo, in quanto livello 01, restituisce stringa vuota<br>
	 * <p>
	 * @param int dataNamePointer
	 * @return String dataName owner
	 * 
	 */
 	public String groupOwnerName (int dataNamePointer){

 		String ar_groupOwnerNames[] = null;				// Array con nomi campi di gruppo da portare in output
  		
  		ar_groupOwnerNames = groupOwnerNames(dataNamePointer);
 		
 		// Nessuna definizione di gruppo possibile
 		if (ar_groupOwnerNames == null || ar_groupOwnerNames.length == 0) {
			return "";
		}
 		
 	  	return ar_groupOwnerNames[0];
    }

	/**
	 * 
	 * Restituisce il puntatore al campo di gruppo sotto il quale il data item è immediatamente definito.<br> 
	 * <p>
	 * Vengono analizzate all'indietro le definizioni prima del data item e, se sono relative a una definizione
	 * di gruppo che include il data item fornito, vengono restituite in output. <br>
	 * Il numero di livello del gruppo deve essere inferiore a quello del data item.
	 * Se il data item fornito è errato restituisce -1
	 * <p>
	 * @param int dataNamePointer
	 * @return int dataName owner pointer
	 * 
	 */
 	public int groupOwnerDefinition (int dataNamePointer){
 		
 		int ar_groupOwnerDefinition[] = null;				// Array con pointer campi di gruppo del data item
  		
  		ar_groupOwnerDefinition = groupOwnerPointers(dataNamePointer);
 		
  		// Il data itemm non è sotto un gruppo oppure il pointer e out of range
 		if (ar_groupOwnerDefinition == null) {
			return -1;
		}

   	   	return ar_groupOwnerDefinition[0];
    }


	/**
	 * 
	 * Restituisce i puntatori ai campi di gruppo 
	 * sotto il quale il data item fornito è definito, fino al livello 01.<br> 
	 * <p>
	 * La funzione restituisce tutti i pointer campi di gruppo sotto il quale il data item fornito è definito<br>
	 * L'array restituito ha come ultimo elemento il campo di gruppo più generale, con il numero di livello<br>
	 * più basso, come il livello 01. Il primo elemento contiene invece il campo di gruppo immediatamente <br>
	 * sotto il quale il data item è definito.<br>
	 * In Cobol, di fatto, le strutture devono iniziare con livelli 01 e, pertanto, l'ultimo numero di <br>
	 * livello nell'array dovrebbe essere 01.<br>
	 * Se il data item fornito è errato restituisce un array vuoto<br>
	 * Se il data item è a livello 1 o 77, restituisce un array vuoto<br>
	 * <p>
	 * @param int dataNamePointer
	 * @return int dataName owner pointer
	 * 
	 */
 	public int[] groupOwnerPointers(int dataNamePointer){
 		
		ProgramCobolEntry<? extends Instruction> dataEntry = null;  // Singolo entry di data division
 		InstructionCobolDataItem dataItemCobol = null;             	// Singola definizione di data item
 		ArrayList<Integer> al_groupOwnerDefinition = null;			// ArrayList di servizio
 		int ar_groupOwnerDefinition[] = null;						// Array con pointer campi di gruppo da portare in output
 		int lvlDataItemOrigin = 0;                            		// Numero livello data item origine
 		int lvlDataItemCur = 0;                            		    // Numero livello data item corrente precedente
 		int lvlGroupGreater = 0;                                    // Livello gruppo da cercare backward >
 		int i = 0;
 		int j = 0;
 		
 		
 		// Pointer out of range
 		if (dataNamePointer >= al_DataEntry.size()) {
			return new int[] {};
		}

 		// Estraggo entry di data division e definizione data item
		dataEntry = al_DataEntry.get(dataNamePointer);
		
		// L'entry non contiene una istruzione di definizione dati: return null
		if (!(dataEntry.getInstruction() instanceof InstructionCobolDataItem)) {
			return new int[] {};
		}
		
		al_groupOwnerDefinition = new ArrayList<Integer>();
		 
        // Estraggo istruzione e livello
		dataItemCobol = (InstructionCobolDataItem) dataEntry.getInstruction();
		lvlDataItemOrigin = dataItemCobol.getLevelNumber();
		lvlGroupGreater = lvlDataItemOrigin;
		
		// Livello 77 0 01: non può essere sotto un gruppo
		if (lvlGroupGreater == 77 || lvlGroupGreater == 1) {
			return new int[] {};
		}
 		
		// Scan definizioni all'indietro, loop generale
 		for (i = dataNamePointer - 1; i >= 0; i--) {

			// Cerco primo gruppo utile precedente all'entry corrente 
 			for (j = i; j >= 0; j--) {
 			
 				dataEntry = al_DataEntry.get(j);

	 			// L'entry non contiene una istruzione di definizione dati: skip
	 			if (!(dataEntry.getInstruction() instanceof InstructionCobolDataItem)) {continue;}
				
	 			dataItemCobol = (InstructionCobolDataItem) dataEntry.getInstruction();
	 			lvlDataItemCur = dataItemCobol.getLevelNumber();

	 			// Livello 77: fine
	 			if (lvlDataItemCur == 77) {
	 				i = - 1;
					break;
				}
	 			
	 			// E' un campo elementare o 88: skip
	 			if (!dataItemCobol.isGroupField()) 	{continue;}
	 			if (lvlDataItemCur == 88) 			{continue;}

	 			// E' un campo di gruppo
	 			
				// Livello campo di gruppo < di quello del campo corrente (definito dopo): skip
	 			if (lvlDataItemCur < lvlGroupGreater) {break;}
	
 			}

 			// Fine scansione anomala: non era presente un livello 01 iniziale
 			if (j < 0) {break;}

 			// Fine scansione regolare
 			if (i < 0) {break;}
 			
 			// Campo di gruppo valido: lo porto in output
 			al_groupOwnerDefinition.add(j);
 			
 			// Appena accodato livello 01: fine
 			if (dataItemCobol.getLevelNumber() == 1) {
				break;
			}
 			
 			// Inizializzazione in loop per ricerca nuovo gruppo
 			i = j;
 			dataEntry = al_DataEntry.get(i);
 			dataItemCobol = (InstructionCobolDataItem) dataEntry.getInstruction();
 			lvlGroupGreater = dataItemCobol.getLevelNumber();
		}
 			
 		
 		// Converto ArrayList in Array di output
 		ar_groupOwnerDefinition = new int[al_groupOwnerDefinition.size()];
 		for (i = 0; i < ar_groupOwnerDefinition.length; i++) {
 			ar_groupOwnerDefinition[i] = al_groupOwnerDefinition.get(i).intValue();
		}
 		
  	   	return ar_groupOwnerDefinition;
    }


 	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Metodi privati                                        /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

	/*
	 *  
	 * Accumula ricorsivamente tutte le section chiamanti dead code, senza ulteriori chiamanti<br>
	 * <p>
	 * Viene analizzato qualsiasi livello di annidamento delle section.<br>
	 * Della section fornita si individuano i punti dove è performata.<br>
	 * Di ogni punto si individua la section di appartenenza e se si trova una section
	 * non + performata si restituisce in output
	 * 
	 */
	private void sectionCallerDeadCodeRecursive(String sectionName, ArrayList<String> al_sectionDeadCodeCaller, ArrayList<String> al_sectionAlreadyManaged) {
		
		ProgramCobolEntry<? extends Instruction> entryXref = null;
		ProgramCobolEntry<? extends Instruction> entrySection = null;
		InstructionCobolProcedure instrCobolProc = null;
		String sectionRecursive = "";
		int ar_xref[] = null;
		
		ar_xref = this.xrefToSection(sectionName);
		
		// Section non referenziata: accodamento e return
        if (ar_xref == null) {
        	al_sectionDeadCodeCaller.add(sectionName);
        	return;
		}
		
		// Section referenziata: per ogni xref recupero la section in cui è collocato il richiamo
        al_sectionAlreadyManaged.add(sectionName);
        
		// Scan xref (perform, perform thru)
		for (int numInstrXref : ar_xref) {
			entryXref = this.al_ProcedureEntry.get(numInstrXref);
			
			// Perform non all'interno di section: skip
			if (entryXref.getUnderProcInternalPointer() == 0) {
				continue;
			}
			
			// La Perform è dentro una section: recupero il nome
			entrySection = this.al_ProcedureEntry.get(entryXref.getUnderProcInternalPointer());
			instrCobolProc = (InstructionCobolProcedure) entrySection.getInstruction();
			sectionRecursive = instrCobolProc.sectionGetName();
			
			// Section già trattata: si evita il loop
			if (al_sectionAlreadyManaged.contains(sectionRecursive)) {
				continue;
			}
			
			// Attivazione ricorsiva
			sectionCallerDeadCodeRecursive(sectionRecursive, al_sectionDeadCodeCaller, al_sectionAlreadyManaged);

		} // end-for
			
		return;
	}
 	

	/*
	 *  
	 * Accumula ricorsivamente tutte le section chiamanti Non dead code, richiamate 
	 * direttamente dalla mainline del programma<br>
	 * <p>
	 * Viene analizzato qualsiasi livello di annidamento delle section.<br>
	 * Della section fornita si individuano i punti dove è performata.<br>
	 * Di ogni punto si individua la section di appartenenza e se si trova una section
	 * non + performata si restituisce in output
	 * 
	 */
	private void sectionCallerNoDeadCodeRecursive(String sectionName, ArrayList<String> al_sectionNotDeadCodeCaller, ArrayList<String> al_sectionAlreadyManaged) {
		
		ProgramCobolEntry<? extends Instruction> entryXref = null;
		ProgramCobolEntry<? extends Instruction> entrySection = null;
		InstructionCobolProcedure instrCobolProc = null;
		String sectionRecursive = "";
		int ar_xref[] = null;
		
		ar_xref = this.xrefToSection(sectionName);
		
		// Section non referenziata: return
        if (ar_xref == null) {
         	return;
		}
		
		// Section referenziata: per ogni xref recupero la section in cui è collocato il richiamo
        al_sectionAlreadyManaged.add(sectionName);
        
		// Scan xref (perform, perform thru)
		for (int numInstrXref : ar_xref) {
			entryXref = this.al_ProcedureEntry.get(numInstrXref);
			
			// Perform a section dalla mailine: accoda e continue
			if (entryXref.getUnderProcInternalPointer() == 0) {
				al_sectionNotDeadCodeCaller.add(sectionName);
				continue;
			}
			
			// La Perform è dentro una section: recupero il nome
			entrySection = this.al_ProcedureEntry.get(entryXref.getUnderProcInternalPointer());
			instrCobolProc = (InstructionCobolProcedure) entrySection.getInstruction();
			sectionRecursive = instrCobolProc.sectionGetName();
			
			// Section già trattata: si evita il loop
			if (al_sectionAlreadyManaged.contains(sectionRecursive)) {
				continue;
			}
			
			// Attivazione ricorsiva
			sectionCallerNoDeadCodeRecursive(sectionRecursive, al_sectionNotDeadCodeCaller, al_sectionAlreadyManaged);

		} // end-for
			
		return;
	}


	/**
	 * Restituisce le metriche associate  al programma completo
	 * come istanza dell'oggetto {@link Metrics}
	 * 
	 * @return the metricsProgram
	 */
	public Metrics getMetricsProgram() {
		return metricsProgram;
	}


	/**
     * Imposta le metriche associate  al programma completo
	 * come istanza dell'oggetto {@link Metrics}
	 * 
      * @param metricsProgram the metricsProgram to set
	 */
	public void setMetricsProgram(Metrics metricsProgram) {
		this.metricsProgram = metricsProgram;
	}


	
	
	
	/**
	 * Restituisce le metriche associate  alla mainline del programma
	 * come istanza dell'oggetto {@link Metrics}
	 * @return the metricsProgramMainline
	 */
	public Metrics getMetricsProgramMainline() {
		return metricsProgramMainline;
	}


	/**
	 * Imposta le metriche associate  alla mainline del programma
	 * come istanza dell'oggetto {@link Metrics}
	 * @param metricsProgramMainline the metricsProgramMainline to set
	 */
	public void setMetricsProgramMainline(Metrics metricsProgramMainline) {
		this.metricsProgramMainline = metricsProgramMainline;
	}


	/**
	 * Restituisce le metriche associate  alle section/paragrafi del programma come istanza dell'oggetto {@link Metrics}<br>
	 * <p>
	 * @return the al_metricsProgramSection
	 */
	public ArrayList<Metrics> getMetricsProgramSections() {
		return al_metricsProgramSection;
	}

	/**
	 * Restituisce le metriche associate  a una specifica section/paragrafo del programma come istanza dell'oggetto {@link Metrics}<br>
	 * <p>
	 * Se sectio/paragrafo non trovato restituisce null.<br>
	 * <p>
	 * @return the metricsProgramSection
	 */
	public Metrics getMetricsProgramSectionParagraph(String idSectionParagraph) {
		
		Metrics metricsSectionParagraphOut = null;
		
		// Scan metriche calcolate per procedure interne richiamate
		for (Metrics metricsSectionParagraph : al_metricsProgramSection) {
			// E' la procedura interna richiesta
			if (metricsSectionParagraph.getSection().startsWith(idSectionParagraph)) {
				metricsSectionParagraphOut = metricsSectionParagraph;
				break;
			}
		}
		
		return (metricsSectionParagraphOut == null ? new Metrics(): metricsSectionParagraphOut);
	}


	/**
	 * Imposta le metriche associate  alle section/paragrafi del programma
	 * come istanza dell'oggetto {@link Metrics}
	 * <p>
	 * @param al_metricsProgramSection the al_metricsProgramSection to set
	 */
	public void setMetricsProgramSection(ArrayList<Metrics> al_metricsProgramSection) {
		this.al_metricsProgramSection = al_metricsProgramSection;
	}

	
	

	/**
	 * Restituisce l'ultimo numero di istruzione della mainline.<br>
	 * <p>
	 * Dopo questa istruzione c'è la fine del programma o iniziano
	 * le section o i paragrafi richiamati.<br>
	 * <p>
	 * @return the numInstrEndMainline
	 */
	public int getNumLastInstrMainline() {
		return numLastInstrMainline;
	}


	/**
	 * Imposta l'ultimo numero di istruzione della mainline.<br>
	 * <p>
	 * Dopo questa istruzione c'è la fine del programma o iniziano
	 * le section o i paragrafi richiamati.<br>
	 * <p>
	 * @param numLastInstrMainline the numLastInstrMainline to set
	 */
	public void setNumLastInstrMainline(int numLastInstrMainline) {
		this.numLastInstrMainline = numLastInstrMainline;
	}


	/**
	 * Restituisce il numero di section definite nel programma;
	 * <p>
	 * @return the countSections
	 */
	public int getCountSections() {
		return this.sectionNames().length;
	}


	/**
	 * Restituisce il numero di paragrafi (richiamati con perform) definiti nel programma;
	 * <p>
	 * @return the countParagraphs
	 */
	public int getCountParagraphs() {
		return this.paragraphNames().length;
	}



	/**
	 * Restituisce se presente almeno una section cobol nel programma
	 * <p>
	 * @return the isAnySectionCobol
	 */
	public boolean isAnySectionCobol() {
		if (this.getCountSections() > 0) {
			return true;
		}
		return false;
	}


	

	/**
	 * Restituisce i nomi dei copy definiti in identification division.<br>
	 * <p>
	 * @return the set_CopyId
	 */
	public Set<String> getCopyNamesId() {
		return set_CopyId;
	}


	/**
	 * Imposta i nomi dei copy definiti in identification division.<br>
	 * <p>
	 * @param set_CopyId the set_CopyId to set
	 */
	public void setCopyNamesId(Set<String> set_CopyId) {
		this.set_CopyId = set_CopyId;
	}


	/**
	 * Restituisce i nomi dei copy definiti in environment division.<br>
	 * <p>
	 * @return the set_CopyEnv
	 */
	public Set<String> getCopyNamesEnv() {
		return set_CopyEnv;
	}


	/**
	 * Imposta i nomi dei copy definiti in environment division.<br>
	 * <p>
	 * @param set_CopyEnv the set_CopyEnv to set
	 */
	public void setCopyNamesEnv(Set<String> set_CopyEnv) {
		this.set_CopyEnv = set_CopyEnv;
	}


	/**
	 * Restituisce i nomi dei copy definiti in procedure division.<br>
	 * <p>
	 * @return the set_CopyProc
	 */
	public Set<String> getCopyNamesProc() {
		return set_CopyProc;
	}


	/**
	 * Imposta i nomi dei copy definiti in procedure division.<br>
	 * <p>
	 * @param set_CopyProc the set_CopyProc to set
	 */
	public void setCopyNamesProc(Set<String> set_CopyProc) {
		this.set_CopyProc = set_CopyProc;
	}


	/**
	 * Restituisce i nomi dei copy definiti in data division.<br>
	 * <p>
	 * @return the set_CopyData
	 */
	public Set<String> getCopyNamesData() {
		return set_CopyData;
	}


	/**
	 * Imposta i nomi dei copy definiti in data division.<br>
	 * <p>
	 * @param set_CopyData the set_CopyData to set
	 */
	public void setCopyNamesData(Set<String> set_CopyData) {
		this.set_CopyData = set_CopyData;
	}

	

	/**
	 * Restituisce il numero di istruzioni in procedure division<br>
	 * <p>
	 * @return the sizeInstrProc
	 */
	public int getSizeInstrProc() {
		return sizeInstrProc;
	}


	/**
	 * Imposta il numero di istruzioni in procedure division<br>
	 * <p>
	 * @param sizeInstrProc the sizeInstrProc to set
	 */
	public void setSizeInstrProc(int sizeInstrProc) {
		this.sizeInstrProc = sizeInstrProc;
	}


	/**
	 * Restituisce il numero di istruzioni di definizione dati in data division<br>
	 * <p>
	 * @return the sizeInstrData
	 */
	public int getSizeInstrData() {
		return sizeInstrData;
	}


	/**
	 * Imposta il numero di istruzioni di definizione dati in data division<br>
	 * <p>
	 * @param sizeInstrData the sizeInstrData to set
	 */
	public void setSizeInstrData(int sizeInstrData) {
		this.sizeInstrData = sizeInstrData;
	}


	/**
	 * Restituisce il numero di istruzioni di definizione dati in data division, File section<br>
	 * <p>
	 * @return the sizeInstrDataFile
	 */
	public int getSizeInstrDataFile() {
		return sizeInstrDataFile;
	}


	/**
	 * Imposta il numero di istruzioni di definizione dati in data division, File section<br>
	 * <p>
	 * @param sizeInstrDataFile the sizeInstrDataFile to set
	 */
	public void setSizeInstrDataFile(int sizeInstrDataFile) {
		this.sizeInstrDataFile = sizeInstrDataFile;
	}


	/**
	 * Restituisce il numero di istruzioni di definizione dati in data division, Working-storage section<br>
	 * <p>
	 * @return the sizeInstrDataWorking
	 */
	public int getSizeInstrDataWorking() {
		return sizeInstrDataWorking;
	}


	/**
	 * Imposta il numero di istruzioni di definizione dati in data division, Working-storage section<br>
	 * <p>
	 * @param sizeInstrDataWorking the sizeInstrDataWorking to set
	 */
	public void setSizeInstrDataWorking(int sizeInstrDataWorking) {
		this.sizeInstrDataWorking = sizeInstrDataWorking;
	}


	/**
	 * Restituisce il numero di istruzioni di definizione dati in data division, Linkage section<br>
	 * <p>
	 * @return the sizeInstrDataLinkage
	 */
	public int getSizeInstrDataLinkage() {
		return sizeInstrDataLinkage;
	}


	/**
	 * Imposta il numero di istruzioni di definizione dati in data division, Linkage section<br>
	 * <p>
	 * @param sizeInstrDataLinkage the sizeInstrDataLinkage to set
	 */
	public void setSizeInstrDataLinkage(int sizeInstrDataLinkage) {
		this.sizeInstrDataLinkage = sizeInstrDataLinkage;
	}


	/**
	 * Restituisce il numero totale di righe sorgente, con i copy non esplosi<br>
	 * <p>
	 * @return the sizeSource
	 */
	public int getSizeSource() {
		return sizeSource;
	}


	/**
	 * Imposta il numero totale di righe sorgente, con i copy non esplosi<br>
	 * <p>
	 * @param sizeSource the sizeSource to set
	 */
	public void setSizeSource(int sizeSource) {
		this.sizeSource = sizeSource;
	}


	/**
	 * Restituisce il numero totale di righe sorgente di procedure division<br>
	 * <p>
	 * @return the sizeSourceProc
	 */
	public int getSizeSourceProc() {
		return sizeSourceProc;
	}


	/**
	 * Imposta il numero totale di righe sorgente di procedure division<br>
	 * <p>
	 * @param sizeSourceProc the sizeSourceProc to set
	 */
	public void setSizeSourceProc(int sizeSourceProc) {
		this.sizeSourceProc = sizeSourceProc;
	}


	/**
	 * Restituisce il numero totale di righe sorgente di di data division<br>
	 * <p>
	 * @return the sizeSourceData
	 */
	public int getSizeSourceData() {
		return sizeSourceData;
	}


	/**
	 * Imposta il numero totale di righe sorgente di data division<br>
	 * <p>
	 * @param sizeSourceData the sizeSourceData to set
	 */
	public void setSizeSourceData(int sizeSourceData) {
		this.sizeSourceData = sizeSourceData;
	}
	

	/**
	 * Restituisce il numero totale di righe sorgente vuote<br>
	 * <p>
	 * Si considerano le righe nulle o quelle con spazi da colonna 7 a 72-<br>
	 * <p>
	 * @return the sizeSourceBlank
	 */
	public int getSizeSourceBlank() {
		return sizeSourceBlank;
	}


	/**
	 * Imposta il numero totale di righe sorgente vuote<br>
	 * <p>
	 * Si considerano le righe nulle o quelle con spazi da colonna 7 a 72-<br>
	 * <p>
	 * @param sizeSourceBlank the sizeSourceBlank to set
	 */
	public void setSizeSourceBlank(int sizeSourceBlank) {
		this.sizeSourceBlank = sizeSourceBlank;
	}


	/**
	 * Restituisce il numero totale di righe sorgente vuote di procedure division<br>
	 * <p>
	 * Si considerano le righe nulle o quelle con spazi da colonna 7 a 72-<br>
	 * <p>
	 * @return the sizeSourceProcBlank
	 */
	public int getSizeSourceProcBlank() {
		return sizeSourceProcBlank;
	}


	/**
	 * Imposta il numero totale di righe sorgente vuote di procedure division<br>
	 * <p>
	 * Si considerano le righe nulle o quelle con spazi da colonna 7 a 72-<br>
	 * <p>
	 * @param sizeSourceProcBlank the sizeSourceProcBlank to set
	 */
	public void setSizeSourceProcBlank(int sizeSourceProcBlank) {
		this.sizeSourceProcBlank = sizeSourceProcBlank;
	}


	/**
	 * Restituisce il numero totale di righe sorgente vuote di data division<br>
	 * <p>
	 * Si considerano le righe nulle o quelle con spazi da colonna 7 a 72-<br>
	 * <p>
	 * @return the sizeSourceDataBlank
	 */
	public int getSizeSourceDataBlank() {
		return sizeSourceDataBlank;
	}


	/**
	 * Imposta il numero totale di righe sorgente vuote di data division<br>
	 * <p>
	 * Si considerano le righe nulle o quelle con spazi da colonna 7 a 72-<br>
	 * <p>
	 * @param sizeSourceDataBlank the sizeSourceDataBlank to set
	 */
	public void setSizeSourceDataBlank(int sizeSourceDataBlank) {
		this.sizeSourceDataBlank = sizeSourceDataBlank;
	}


	
	
	/**
	 * Restituisce il numero di righe commento di procedure division.<br>
	 * <p>
	 * @return the sizeCommProc
	 */
	public int getSizeCommProc() {
		return sizeCommProc;
	}


	/**
	 * Imposta il numero di righe commento di procedure division.<br>
	 * <p>
	 * @param sizeCommProc the sizeCommProc to set
	 */
	public void setSizeCommProc(int sizeCommProc) {
		this.sizeCommProc = sizeCommProc;
	}


	/**
	 * Restituisce il numero di righe commento di data division.<br>
	 * <p>
	 * @return the sizeCommData
	 */
	public int getSizeCommData() {
		return sizeCommData;
	}


	/**
	 * Imposta il numero di righe commento di data division.<br>
	 * <p>
	 * @param sizeCommData the sizeCommData to set
	 */
	public void setSizeCommData(int sizeCommData) {
		this.sizeCommData = sizeCommData;
	}


	/**
	 * Restituisce il numero di righe commento di tutto il programma.<br>
	 * <p>
	 * @return the sizeComm
	 */
	public int getSizeComm() {
		return sizeComm;
	}


	/**
	 * Imposta il numero di righe commento di tutto il programma.<br>
	 * <p>
	 * @param sizeComm the sizeComm to set
	 */
	public void setSizeComm(int sizeComm) {
		this.sizeComm = sizeComm;
	}

	
	

	/**
	 * Restituisce i numeri di riga sorgente shiftate a destra.<br>
	 * <p>
	 * Ciò a causa anomalia Cobol MF che accetta istruzioni prima di col 8<br>
	 * <p>
	 * @return the al_numRowShiftedRight
	 */
	public ArrayList<Integer> getNumRowsShiftedRight() {
		return al_numRowShiftedRight;
	}


	/**
	 * Imposta i numeri di riga sorgente shiftate a destra.<br>
	 * <p>
	 * Ciò a causa anomalia Cobol MF che accetta istruzioni prima di col 8<br>
	 * <p>
	 * @param al_numRowShiftedRight the al_numRowShiftedRight to set
	 */
	public void setNumRowsShiftedRight(ArrayList<Integer> al_numRowShiftedRight) {
		this.al_numRowShiftedRight = al_numRowShiftedRight;
	}


	/**
	 * Restituisce i numeri di riga sorgente shiftate a sinistra.<br>
	 * <p>
	 * Ciò a causa anomalia Cobol MF che accetta label dopo col 8<br>
	 * <p>
	 * @return the al_numRowShiftedLeft
	 */
	public ArrayList<Integer> getNumRowsShiftedLeft() {
		return al_numRowShiftedLeft;
	}
	

	/**
	 * Imposta i numeri di riga sorgente shiftate a sinistra.<br>
	 * <p>
	 * Ciò a causa anomalia Cobol MF che accetta label dopo col 8<br>
	 * <p>
	 * @param al_numRowShiftedLeft the al_numRowShiftedLeft to set
	 */
	public void setNumRowsShiftedLeft(ArrayList<Integer> al_numRowShiftedLeft) {
		this.al_numRowShiftedLeft = al_numRowShiftedLeft;
	}


	/**
	 * Restituisce i numeri di riga sorgente con caratteri speciali e tabulazioni.<br>
	 * <p>
	 * Ciò a causa anomalia Cobol MF che accetta sorgenti di questo tipo<br>
	 * <p>
	 * @return the al_numRowWithBadChar
	 */
	public ArrayList<Integer> getNumRowsWithBadChar() {
		return al_numRowWithBadChar;
	}

	/**
	 * Imposta i numeri di riga sorgente con caratteri speciali e tabulazioni.<br>
	 * <p>
	 * Ciò a causa anomalia Cobol MF che accetta sorgenti di questo tipo<br>
	 * <p>
	 * @param al_numRowWithBadChar the al_numRowWithBadChar to set
	 */
	public void setNumRowsWithBadChar(ArrayList<Integer> al_numRowWithBadChar) {
		this.al_numRowWithBadChar = al_numRowWithBadChar;
	}


	/**
	 * Restituisce i numeri di riga sorgente di definizione dati non chiusi da un punto finale.<br>
	 * <p>
	 * Ciò a causa anomalia Cobol MF che accetta definizioni dati non chiuse da un punto.<br>
	 * <p>
	 * @return the al_numRowWithDataUnclosed
	 */
	public ArrayList<Integer> getNumRowsWithDataUnclosed() {
		return al_numRowWithDataUnclosed;
	}

	/**
	 * Imposta i numeri di riga sorgente di definizione dati non chiusi da un punto finale.<br>
	 * <p>
	 * Ciò a causa anomalia Cobol MF che accetta definizioni dati non chiuse da un punto.<br>
	 * <p>
	 * @param al_numRowWithDataUnclosed the al_numRowWithDataUnclosed to set
	 */
	public void setNumRowsWithDataUnclosed(ArrayList<Integer> al_numRowWithDataUnclosed) {
		this.al_numRowWithDataUnclosed = al_numRowWithDataUnclosed;
	}

	
	
	/**
	 * Restituisce i numeri di riga sorgente con codifiche deprecate.<br>
	 * <p>
	 * Per esempio literal con dentro gli stessi delimiter come "AA""BBB"<br>
	 * <p>
	 * @return the al_numRowWithBadCoding
	 */
	public ArrayList<Integer> getNumRowsWithBadCoding() {
		return al_numRowWithBadCoding;
	}


	/**
	 * Imposta i numeri di riga sorgente con codifiche deprecate.<br>
	 * <p>
	 * Per esempio literal con dentro gli stessi delimiter come "AA""BBB"<br>
	 * <p>
	 * @param al_numRowWithBadCoding the al_numRowWithBadCoding to set
	 */
	public void setNumRowsWithBadCoding(ArrayList<Integer> al_numRowWithBadCoding) {
		this.al_numRowWithBadCoding = al_numRowWithBadCoding;
	}


	/**
	 * Restituisce i numeri di riga sorgente con codifiche deprecate in statements Sql.<br>
	 * <p>
	 * Per esempio stmt sql con INTO:var o INTO : var<br>
	 * <p>
	 * @return the al_numRowWithBadCodingSql
	 */
	public ArrayList<Integer> getNumRowsWithBadCodingSql() {
		return al_numRowWithBadCodingSql;
	}


	/**
	 * Imposta i numeri di riga sorgente con codifiche deprecate in statements Sql.<br>
	 * <p>
	 * Per esempio stmt sql con INTO:var o INTO : var<br>
	 * <p>
	 * @param al_numRowWithBadCodingSql the al_numRowWithBadCodingSql to set
	 */
	public void setNumRowsWithBadCodingSql(ArrayList<Integer> al_numRowWithBadCodingSql) {
		this.al_numRowWithBadCodingSql = al_numRowWithBadCodingSql;
	}


	/**
	 * Restituisce il numero riga sorgente di inizio di identification division.<br>
	 * <p>
	 * @return the numRowStartIdentification
	 */
	public int getNumRowStartIdentification() {
		return numRowStartIdentification;
	}


	/**
	 * Imposta il numero riga sorgente di inizio di identification division.<br>
	 * <p>
	 * @param numRowStartIdentification the numRowStartIdentification to set
	 */
	public void setNumRowStartIdentification(int numRowStartIdentification) {
		this.numRowStartIdentification = numRowStartIdentification;
	}


	/**
	 * Restituisce il numero riga sorgente di fine di identification division.<br>
	 * <p>
	 * @return the numRowEndIdentification
	 */
	public int getNumRowEndIdentification() {
		return numRowEndIdentification;
	}


	/**
	 * Imposta il numero riga sorgente di fine di identification division.<br>
	 * <p>
	 * @param numRowEndIdentification the numRowEndIdentification to set
	 */
	public void setNumRowEndIdentification(int numRowEndIdentification) {
		this.numRowEndIdentification = numRowEndIdentification;
	}


	/**
	 * Restituisce il numero riga sorgente di inizio di environment division.<br>
	 * <p>
	 * @return the numRowStartEnvironment
	 */
	public int getNumRowStartEnvironment() {
		return numRowStartEnvironment;
	}


	/**
	 * Imposta il numero riga sorgente di inizio di environment division.<br>
	 * <p>
	 * @param numRowStartEnvironment the numRowStartEnvironment to set
	 */
	public void setNumRowStartEnvironment(int numRowStartEnvironment) {
		this.numRowStartEnvironment = numRowStartEnvironment;
	}


	/**
	 * Restituisce il numero riga sorgente di fine di environment division.<br>
	 * <p>
	 * @return the numRowEndEnvironment
	 */
	public int getNumRowEndEnvironment() {
		return numRowEndEnvironment;
	}


	/**
	 * Imposta il numero riga sorgente di fine di environment division.<br>
	 * <p>
	 * @param numRowEndEnvironment the numRowEndEnvironment to set
	 */
	public void setNumRowEndEnvironment(int numRowEndEnvironment) {
		this.numRowEndEnvironment = numRowEndEnvironment;
	}


	/**
	 * Restituisce il numero riga sorgente di inizio di data division.<br>
	 * <p>
	 * @return the numRowStartData
	 */
	public int getNumRowStartData() {
		return numRowStartData;
	}


	/**
	 * Imposta il numero riga sorgente di inizio di data division.<br>
	 * <p>
	 * @param numRowStartData the numRowStartData to set
	 */
	public void setNumRowStartData(int numRowStartData) {
		this.numRowStartData = numRowStartData;
	}


	/**
	 * Restituisce il numero riga sorgente di fine di data division.<br>
	 * <p>
	 * @return the numRowEndData
	 */
	public int getNumRowEndData() {
		return numRowEndData;
	}


	/**
	 * Imposta il numero riga sorgente di fine di data division.<br>
	 * <p>
	 * @param numRowEndData the numRowEndData to set
	 */
	public void setNumRowEndData(int numRowEndData) {
		this.numRowEndData = numRowEndData;
	}


	/**
	 * Restituisce il numero riga sorgente di inizio di procedure division.<br>
	 * <p>
	 * @return the numRowStartProc
	 */
	public int getNumRowStartProc() {
		return numRowStartProc;
	}


	/**
	 * Imposta il numero riga sorgente di inizio di procedure division.<br>
	 * <p>
	 * @param numRowStartProc the numRowStartProc to set
	 */
	public void setNumRowStartProc(int numRowStartProc) {
		this.numRowStartProc = numRowStartProc;
	}


	/**
	 * Restituisce il numero riga sorgente di fine di procedure division.<br>
	 * <p>
	 * @return the numRowEndProc
	 */
	public int getNumRowEndProc() {
		return numRowEndProc;
	}


	/**
	 * Imòposta il numero riga sorgente di fine di procedure division.<br>
	 * <p>
	 * @param numRowEndProc the numRowEndProc to set
	 */
	public void setNumRowEndProc(int numRowEndProc) {
		this.numRowEndProc = numRowEndProc;
	}

	/**
	 * Restituisce la divisione cobol del numero di riga sorgente fornito.<br>
	 * <p>
	 * @return the EnumCobolReservedWords division
	 */
	public EnumCobolReservedWords getRowDivision(int numRow) {
		
		// Identification division
		if (numRow >= this.numRowStartIdentification && numRow <= this.numRowEndIdentification) {
			return EnumCobolReservedWords.ID_DIVISION;
		}
		
		// Environment division
		if (numRow >= this.numRowStartEnvironment && numRow <= this.numRowEndEnvironment) {
			return EnumCobolReservedWords.ENV_DIVISION;
		}
		
		// Data division
		if (numRow >= this.numRowStartData && numRow <= this.numRowEndData) {
			return EnumCobolReservedWords.DATA_DIVISION;
		}
		
		// Proc division
		if (numRow >= this.numRowStartProc && numRow <= this.numRowEndProc) {
			return EnumCobolReservedWords.PROC_DIVISION;
		}
			
		return EnumCobolReservedWords.NOT_ASSIGNED;
	}

	/**
	 * Restituisce il numero di istruzione, nella divisione cobol di appartenenza,
	 * del numero di riga sorgente fornito.<br>
	 * <p>
	 * @return the EnumCobolReservedWords division
	 */
	public int getNumInstrInDivision(int numRow) {
		
		ProgramCobolEntry<? extends Instruction>[] ar_entries = null;
		Instruction instructionGeneric = null;
		EnumCobolReservedWords division = null;
		int numInstr = 0;

		division = getRowDivision(numRow);

		if (division == EnumCobolReservedWords.ID_DIVISION) {
			ar_entries = this.entriesIdentification();
		} else if (division == EnumCobolReservedWords.ENV_DIVISION) {
			ar_entries = this.entriesEnvironment();
		} else if (division == EnumCobolReservedWords.DATA_DIVISION) {
			ar_entries = this.entriesData();
		}  else if (division == EnumCobolReservedWords.PROC_DIVISION) {
			ar_entries = this.entriesProcedure();
		} 
		
		// Scan entries
		for (ProgramCobolEntry<? extends Instruction> entry : ar_entries) {
			instructionGeneric = entry.getInstruction();
			if (numRow >= instructionGeneric.getRowStartSource() && numRow <= instructionGeneric.getRowEndSource()) {
				numInstr = instructionGeneric.getNumInstr();
				break;
			}
		}
		
		return numInstr;
	}


	/**
	 * Restituisce il numero di IF annidate, a partire dall'istruzione IF origine<br>
	 * <p>
	 * Le ELSE interrompono il livello di annidamento.<br>
	 * <p>
	 * Per esempio la seguente struttura restituisce 2<br>
	 * <p>
	 * IF A<br>
	 *   IF B<br>
	 *      IF C<br>
	 *      ..<br>
	 *      ..<br>
	 * <p>
	 * La seguente struttura restituisce invece 1<br>
	 * <p>
	 * IF A<br>
	 *   IF B<br>
	 *   ...
	 *   ELSE
	 *      IF C<br>
	 *        IF D<br>
	 *          ..<br>
	 *           ..<br>
	 * <p>
	 * Se il numero istruzione fornito non corrisponde a una istruzione IF<br>
	 * restituisce 0.<br>
	 * <p>
	 * Se non ci sono IF annidate restituisce 0.<br>
	 * <p>
	 * 
	 * @return the nesting level number
	 */
	public int getIfNestingLevel(int numInstrIf) {
		
		ProgramCobolEntry<? extends Instruction>[] ar_entryProc = null;
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		int numInstrEnd = 0;
		int lvlNestingMax = 0;
		
		ar_entryProc = this.entriesProcedure();
		
		// Controlli di sicurezza
		if (numInstrIf >= ar_entryProc.length) {
			return 0;
		}
		
		// Verifica se if
		entryProc = ar_entryProc[numInstrIf];
		if (entryProc.getTypeInstr() != EnumCobolReservedWords.PROC_IF) {
			return 0;
		}
		numInstrEnd = entryProc.getNumInstrRelated();
		
		// Attivazione recorsiva
		this.ifLvlNestingMax = 0;
		lvlNestingMax = getIfNestingLevelRecursive(ar_entryProc, numInstrIf, numInstrEnd, 0);
		
		return lvlNestingMax;
	}

    /*
     * Verifica ricorsiva livello massimo annidamento if.
     * Si aggiorna la variabile di istanza con il valore massimo raggiunto
     */
	private int getIfNestingLevelRecursive(ProgramCobolEntry<? extends Instruction>[] ar_entryProc
			 							 , int numInstrIfStart
			 							 , int numInstrIfEnd
										 , int lvlNestingCurrent) {
		
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		ProgramCobolEntry<? extends Instruction> entryProcIfEnd = null;
		int numInstrIfStartRecursive = 0;
		int numInstrIfEndRecursive = 0;
		int cntIf = 0;
		int cntElse = 0;
		int i = 0;
			
		
		// Scan istruzioni IF corrente, fino a fine if
		for (i = numInstrIfStart + 1; i < numInstrIfEnd; i++) {
			
			entryProc = ar_entryProc[i];
			
			// If: incremento nesting corrente e aggiorno valore massimo
			if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_IF) {
				lvlNestingCurrent++;
				this.ifLvlNestingMax = (lvlNestingCurrent > this.ifLvlNestingMax) ? lvlNestingCurrent : this.ifLvlNestingMax;
				continue;
			}
			
			// end-If: decremento nesting corrente  
			if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_END_IF) {
				lvlNestingCurrent--;
				continue;
			}
			
			// Interessano solo le Else
			if (entryProc.getTypeInstr() != EnumCobolReservedWords.PROC_ELSE) {continue;}
			
			// Else: individuo ultima istruzione blocco ramo false
			cntIf = 0;
			cntElse = 0;
			numInstrIfStartRecursive = 0;
			numInstrIfEndRecursive = numInstrIfEnd;
			
			// Scan eventuali if/else nel ramo else corrente, da verificare ricorsivamente
			for (i =  i + 1; i < numInstrIfEnd; i++) {
				entryProc = ar_entryProc[i];
				// Successiva Else e nessuna if trovata o if/else trovate bilanciate
				if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_ELSE) {
					if (cntIf == cntElse) {
						numInstrIfEndRecursive = i - 1;
						break;
					}
					cntElse++;
				}
				
				// End-if: decremento nesting levele, if aperta e chiusa
				if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_END_IF) {
					lvlNestingCurrent--;
					this.ifLvlNestingMax--;
					break;
				}
				
				// If: incremento counter
				if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_IF) {
					cntIf++;
					if (numInstrIfStartRecursive == 0) {
						numInstrIfStartRecursive = i - 1;
						entryProcIfEnd = this.entryProcedure(entryProc.getNumInstrRelated());
						// End-If: si evita la ricerca
						if (entryProcIfEnd.getTypeInstr() == EnumCobolReservedWords.PROC_END_IF) {
							numInstrIfEndRecursive = entryProc.getNumInstrRelated();
							i = numInstrIfEndRecursive;
							break;
						}
					}
				}
			} // for-ramo else 
			
			// if/else nel ramo else corrente: attivazione ricorsiva
			if (cntIf > 0) {
				getIfNestingLevelRecursive(ar_entryProc, numInstrIfStartRecursive, numInstrIfEndRecursive, lvlNestingCurrent);
			}
			
		} // for-istruzioni if corrente
		
		return this.ifLvlNestingMax;
	}

	/**
	 * Restituisce le istruzioni del ramo TRUE di una istruzione IF.<br>
	 * <p>
	 * Nel caso di una IF senzza ELSE e IF nidificate restituisce semplicemente
	 * tutte le istruzioni sotto condizione.<br>
	 * Nel caso di IF con ELSE restituisce tutte le istruzioni fino a ELSE esclusa.<br>
	 * <br>
	 * Le istruzioni restituite possono contenere a loro volta altre strutture IF/THEN/ELSE.<br>
	 * In caso di nessuna istruzione presente, ovvero di ELSE immediatamente dopo la IF,<br>
	 * restituisce un array vuoto.<br>
	 * <p>
	 * Per esempio la seguente struttura restituisce IF A, IF B e le istruzioni successive<br>
	 * <p>
	 * IF A<br>
	 *   IF B<br>
	 *      IF C<br>
	 *      ..<br>
	 *      ..<br>
	 * <p>
	 * La seguente struttura restituisce invece MOVE ..<br>
	 * <p>
	 * IF A<br>
	 *   MOVE <br>
	 *   ...
	 * ELSE
	 *      IF C<br>
	 *        IF D<br>
	 *          ..<br>
	 *           ..<br>
	 * <p>
	 * 
	 * @return al instructions in TRUE branch
	 */
	public ArrayList<ProgramCobolEntry<? extends Instruction>> getIfTrueEntries(int numInstrIf) {
		
		ArrayList<ProgramCobolEntry<? extends Instruction>> al_ifTrueEntry = null;
		ProgramCobolEntry<? extends Instruction>[] ar_entryProc = null;
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		int numInstrIfEnd = 0;
		int numInstrIfDelimiter = 0;
		int i = 0;
		
		ar_entryProc = this.entriesProcedure();
		al_ifTrueEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();

		// Controlli di sicurezza
		if (numInstrIf >= ar_entryProc.length) {
			return al_ifTrueEntry;
		}
		
		// Verifica se if
		entryProc = ar_entryProc[numInstrIf];
		if (entryProc.getTypeInstr() != EnumCobolReservedWords.PROC_IF) {
			return al_ifTrueEntry;
		}
		
		// END-IF o ultima istruzione IF terminata con punto
		numInstrIfEnd = entryProc.getNumInstrRelated();
		numInstrIfDelimiter = numInstrIfEnd;
		if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_END_IF) {
			numInstrIfDelimiter--;
		}
		
		
		// Scan istruzioni if
		for (i = numInstrIf + 1; i < numInstrIfEnd; i++) {
			entryProc = ar_entryProc[i];
			if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_ELSE
			&& 	entryProc.getNumInstrRelated() == numInstrIf) {
				numInstrIfDelimiter = i - 1;
			}
		}
		
		// Empty true branch
		if (numInstrIfDelimiter <= numInstrIf) {
			return al_ifTrueEntry;
		}
		
		// Porto in output
		for (i = numInstrIf + 1; i <= numInstrIfDelimiter; i++) {
			entryProc = ar_entryProc[i];
			al_ifTrueEntry.add(entryProc);
		}

		return al_ifTrueEntry;
	}
	
	
	/**
	 * Restituisce le istruzioni del ramo ELSE di una istruzione IF ovvero per condizione FALSE.<br>
	 * <p>
	 * Nel caso di una IF senza ELSE restituisce un insieme vuoto.<br>
	 * Nel caso di END-IF immediatamente dopo la ELSE restituisce un insieme vuoto.<br>
	 * Nel caso di IF con ELSE restituisce tutte le istruzioni successive alla ELSE fino<br>
	 * all'istruzione precedente alla END-IF o all'ultima istruzione, se chiusa da un puno.<br>
	 * Le istruzioni restituite possono contenere a loro volta altre strutture IF/THEN/ELSE.<br>
	 * <p>
	 * Per esempio la seguente struttura restituisce IF A, IF B e le istruzioni successive<br>
	 * restituiscono MOVE ... <br>
	 * <p>
	 * IF A<br>
	 *   IF B<br>
	 *      IF C<br>
	 *      ..<br>
	 *      ELSE<br>
	 *      ... <br>
	 *      END-IF<br>
	 *   END-IF<br>
	 * ELSE<br>
	 *   MOVE ..<br>
	 *   ....<br>
	 * END-IF<br>
	 * <p>
	 * Consideruiamo La seguente struttura:<br>
	 * <p>
	 * IF A<br>
	 *   IF B<br>
	 *     IF C<br>
	 *       ,,,<br>
	 *     ELSE<br>
	 *       MOVE D<br>
	 *   ELSE<br>
	 * ELSE<br>
	 *   MOVE E.<br>
	 * <p>
	 * Applicata a IF A restituisce MOVE E<br>
	 * Applicata a IF B restituisce insieme vuoto<br>
	 * Applicata a IF C restituisce MOVE D<br>
	 * 
	 * 
	 * 
	 * @return al instructions in TRUE branch
	 */
	public ArrayList<ProgramCobolEntry<? extends Instruction>> getIfFalseEntries(int numInstrIf) {
		
		ArrayList<ProgramCobolEntry<? extends Instruction>> al_ifFalseEntry = null;
		ProgramCobolEntry<? extends Instruction>[] ar_entryProc = null;
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		int numInstrIfEnd = 0;
		int numInstrIfElse = 0;
		int i = 0;
		
		ar_entryProc = this.entriesProcedure();
		al_ifFalseEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();

		// Controlli di sicurezza
		if (numInstrIf >= ar_entryProc.length) {
			return al_ifFalseEntry;
		}
		
		// Verifica se if
		entryProc = ar_entryProc[numInstrIf];
		if (entryProc.getTypeInstr() != EnumCobolReservedWords.PROC_IF) {
			return al_ifFalseEntry;
		}
		
		// END-IF o ultima istruzione IF terminata con punto
		numInstrIfEnd = entryProc.getNumInstrRelated();		
		
		// Scan istruzioni if
		for (i = numInstrIf + 1; i < numInstrIfEnd; i++) {
			entryProc = ar_entryProc[i];
			if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_ELSE
			&& 	entryProc.getNumInstrRelated() == numInstrIf) {
				numInstrIfElse = i;
				break;
			}
		}
		
		// Else non presente: return insieme vuoto
		if (numInstrIfElse == 0) {
			return al_ifFalseEntry;
		}
		
		// Else presente e istruzione successiva END-IF: Empty false branch
		if (numInstrIfElse > 0 
		&&  this.entryProcedure(numInstrIfElse + 1).getTypeInstr() == EnumCobolReservedWords.PROC_END_IF) {
			return al_ifFalseEntry;
		}
		
		// Porto in output le istruzioni fra ELSE e END-IF o successiva ELSE
		for (i = numInstrIfElse + 1; i <= numInstrIfEnd; i++) {
			
			// Fine istruzioni ramo false
			if (this.entryProcedure(numInstrIfElse + 1).getTypeInstr() == EnumCobolReservedWords.PROC_END_IF
			||  this.entryProcedure(numInstrIfElse + 1).getTypeInstr() == EnumCobolReservedWords.PROC_ELSE) {
				break;
			}
			
			// Accodo
			al_ifFalseEntry.add(this.entryProcedure(i));
		}

		return al_ifFalseEntry;
	}

	/**
	 * Restituisce se presente il ramo ELSE di una istruzione IF, ovvero per condizione FALSE.<br>
	 * <p>
	 * 
	 * @return if is there Else statement
	 */
	public boolean isIfWithElse(int numInstrIf) {
		
		ProgramCobolEntry<? extends Instruction>[] ar_entryProc = null;
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		int numInstrIfEnd = 0;
		int i = 0;
		
		ar_entryProc = this.entriesProcedure();

		// Controlli di sicurezza
		if (numInstrIf >= ar_entryProc.length) {
			return false;
		}
		
		// Verifica se if
		entryProc = ar_entryProc[numInstrIf];
		if (entryProc.getTypeInstr() != EnumCobolReservedWords.PROC_IF) {
			return false;
		}
		
		// END-IF o ultima istruzione IF terminata con punto
		numInstrIfEnd = entryProc.getNumInstrRelated();
		
		
		// Scan istruzioni if
		for (i = numInstrIf + 1; i < numInstrIfEnd; i++) {
			entryProc = ar_entryProc[i];
			if (entryProc.getTypeInstr() == EnumCobolReservedWords.PROC_ELSE
			&& 	entryProc.getNumInstrRelated() == numInstrIf) {
				return true;
			}
		}
		
		// Else non presente: return insieme vuoto
		return false;
	}


	
	/**
	 * Restituisce il numero di istruzione, in Data Division, <br>
	 * dello statement Linkage Section.<br>
	 * <p>
	 * @return the numInstrLinkageSection
	 */
	public int getNumInstrLinkageSection() {
		return numInstrLinkageSection;
	}


	/**
	 * Imposta il numero di istruzione, in Data Division, <br>
	 * dello statement Linkage Section.<br>
	 * <p>
	 * @param numInstrLinkageSection the numInstrLinkageSection to set
	 */
	public void setNumInstrLinkageSection(int numInstrLinkageSection) {
		this.numInstrLinkageSection = numInstrLinkageSection;
	}


	/**
	 * Restituisce il numero di istruzione, in Data Division, <br>
	 * dello statement Working-Storage Section.<br>
	 * <p>
	 * @return the numInstrWsStorageSection
	 */
	public int getNumInstrWsStorageSection() {
		return numInstrWsStorageSection;
	}


	/**
	 * Imposta il numero di istruzione, in Data Division, <br>
	 * dello statement Working-Storage Section.<br>
	 * <p>
	 * @param numInstrWsStorageSection the numInstrWsStorageSection to set
	 */
	public void setNumInstrWsStorageSection(int numInstrWsStorageSection) {
		this.numInstrWsStorageSection = numInstrWsStorageSection;
	}


	/**
	 * Restituisce se presente, in Data Division, <br>
	 * lo statement Working-Storage Section.<br>
	 * <p>
	 * @return the isThereLinkageSection
	 */
	public boolean isThereLinkageSection() {
		return isThereLinkageSection;
	}


	/**
	 * Imposta se presente, in Data Division, <br>
	 * lo statement Working-Storage Section.<br>
	 * <p>
	 * @param isThereLinkageSection the isThereLinkageSection to set
	 */
	public void setLinkageSection(boolean isThereLinkageSection) {
		this.isThereLinkageSection = isThereLinkageSection;
	}


	/**
	 * Restituisce se presente, in Data Division, <br>
	 * lo statement Linkage Section.<br>
	 * <p>
	 * @return the isThereWsStorageSection
	 */
	public boolean isThereWsStorageSection() {
		return isThereWsStorageSection;
	}

	

	/**
	 * Restituisce la map di servizio GoTo generata in fase di analisi.<br>
	 * <p>
	 * Necessaria per la generazione del grafo di programma come processo<br>
	 * a livello di programma post analisi.<br>
	 * <p>
	 * @return the map_GoTo
	 */
	public Map<Integer, Object[]> getAnalysisMapGoTo() {
		return map_GoTo;
	}


	/**
	 * Imposta la map di servizio GoTo generata in fase di analisi.<br>
	 * <p>
	 * Necessaria per la generazione del grafo di programma come processo<br>
	 * a livello di programma post analisi.<br>
	 * <p>
	 * @param mapGoTo the map_GoTo to set
	 */
	public void setAnalysisMapGoTo(Map<Integer, Object[]> mapGoTo) {
		map_GoTo = mapGoTo;
	}


	/**
	 * Restituisce la map di servizio Label/Section generata in fase di analisi.<br>
	 * <p>
	 * Necessaria per la generazione del grafo di programma come processo<br>
	 * a livello di programma post analisi.<br>
	 * <p>
	 * @return the map_LabelSection
	 */
	public Map<String, Object[]> getAnalysisMapLabelSection() {
		return map_LabelSection;
	}


	/**
	 * Imposta la map di servizio Label/Section generata in fase di analisi.<br>
	 * <p>
	 * Necessaria per la generazione del grafo di programma come processo<br>
	 * a livello di programma post analisi.<br>
	 * <p>
	 * @param mapLabelSection the map_LabelSection to set
	 */
	public void setAnalysisMapLabelSection(Map<String, Object[]> mapLabelSection) {
		map_LabelSection = mapLabelSection;
	}


	/**
	 * Imposta se presente, in Data Division, <br>
	 * lo statement Linkage Section.<br>
	 * <p>
	 * @param isThereWsStorageSection the isThereWsStorageSection to set
	 */
	public void setWsStorageSection(boolean isThereWsStorageSection) {
		this.isThereWsStorageSection = isThereWsStorageSection;
	}


	@Override
	public String toString() {
		return "Program:" + this.programName;
	}



	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

}
