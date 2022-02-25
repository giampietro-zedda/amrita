package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import enums.EnumCobolReservedWords;
import enums.EnumInstrDataCategory;
import enums.EnumMessageType;
import enums.EnumMetricsViolation;
import enums.EnumPrecompilerReservedWords;
import enums.EnumSymbolType;
import enums.EnumTypeProcessAnalysis;
import exception.ExceptionAmrita;


/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
 * 
 * <h1>
 * Instruction  
 * </h1>
 *  <p>
 * Questa classe modella una generica istruzione di un programma sorgente, senza considerazioni sul linguaggio utilizzato.<br>
 * Vengono memorizzate tutte le informazioni di collegamento con il sorgente intercettate in fase di analisi preliminare.<br>
 * Queste informazioni vengono utilizzate per poter ricomporre nelle fasi successive <br>
 * il sorgente e per visualizzarlo con il posizionamento sulle istruzioni corrette. <br>
 * Vengono memorizzate le righe sorgente componenti l'istruzione e gli eventuali commenti <br>
 * che la precedono.
 * <p>
 * Vengono modellate inoltre le informazioni che descrivono nel dettaglio gli elementi dell'istruzione
 * senza alcuna struttura specifica predefinita.<br>
 * Ciò viene realizzato con una Map<String, Object> che mappa i valori degli operandi rilevanti dell'istruzione.<br>
 * Object può contenere qualsiasi tipo di oggetto, da una stringa a un array, in base alle esigenze della particolare <br>
 * istruzione. Eventuali opzioni dell'istruzione che possono esserci o non esserci, vengono semplicemente<br>
 * caricati in Map con un Object a null.<br>
 * <p>
 * Sono presenti poi tre struttura ArrayList di simboli:<br>
 * <p>
 * 1) Una ArrayList con i symboli definiti direttamente dentro l'istruzione<br>
 * 2) Una ArrayList con i symboli utilizzati in input,  con relativa modalità di utilizzo (qualificatore)<br>
 * 3) Una ArrayList con i symboli utilizzati in output, con relativa modalità di utilizzo (qualificatore)<br>
 * <p>
 * Queste ArrayList vengono utilizzati a livello generale di programma per costruire le Map dei simboli e degli
 * unresolved.<br>
 * <p>
 * Inoltre è presente una struttura Map map_Descriptor che mappa una stringa su un generico oggetto dipendente
 * dalla stringa. Con questa Map vengono codificate opzioni, operandi, valori di operandi e qualsiasi struttura
 * complessa associata a una specifica istruzione.
 * <p>
 * Un' istruzione potrebbe referenziare, in input o in output, lo stesso simbolo definito in strutture di <br>
 * definizione diverse, per esempio in diversi gruppi o livelli 01 Cobol. In questo caso, come per il Cobol,<br>
 * le istruzioni riferiscono al simbolo qualificandolo con l'opzione OF groupName. <br>
 * Oppure il simbolo potrebbe essere referenziato per posizione e lunghezzo, con degli indici o altro<br>
 * Per questo motivo le ArrayList contengono oggetti conetenti il nome del simbolo e un oggetto della classe {@link DataItemQualifier}, <br>
 * per memorizzare tutte le specifiche informazioni di qualificazione nel riferimento al simbolo, da parte dell'istruzione.
 * <p>
 * Se l'istrruzione è di tipo procedurale e non dichiarativo (non è un {@link DataItem}), allora il processo di
 * analisi preliminare del sorgente collocherà, in fase di costruzione del grafo di programma, l'istruzione o meglio
 * il suo numero, in un preciso arco del grafo.
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/04/2010
 * @see InstructionCobol
 * @see ProgramCobol
 * @see AnalyzerCobol
 * 
*/

public class Instruction implements Serializable {
    
	private static final long serialVersionUID = 1L;

	////////////////////////////////////////////////////////////////////////////////////
    // Informazioni sullo stato dell'analisi e/o dell'elaborazione dell'istruzione
	////////////////////////////////////////////////////////////////////////////////////

	private EnumMessageType msgType = null;					// Tipo messaggio 
	private String tokenInError = "";				  		// Token in errore
	private String msgCode = "";                      		// Codice messaggio di errore o informativo o di warning
	private String ar_msgParm[] = null;				  		// parametri messaggio di errore
    private ExceptionAmrita excpError = null; 				// Exception al momento dell'errore con relativo stack trace 
	private boolean parsingError = false;          		    // True indica un errore di parsing o un sorgente malformato
	private boolean semanticError = false;          		// True indica un errore di semantica, come un campo non definito
	private boolean warning = false;          		        // True indica un warning nell'analisi dell'istruzione (es. operando Cics non codificato)

	  
	/////////////////////////////////////////////////////////////////////////
    // Informazioni sul num. istruzione, sulle righe sorgente origine, ...
	/////////////////////////////////////////////////////////////////////////
	
	private int numInstr = 0;                     	  		// Numero sequenza istruzione (0-based) nella sezione di programma
	private int numInstrOwner = 0;                     	    // Numero sequenza istruzione proprietaria  (es. X istruzioni dentro IF)
	private int rowStartSource = 0;	                  		// Numero riga sorgente di analisi di inizio
	private int rowEndSource = 0;	                  		// Numero riga sorgente di analisi di fine
	private int posStartInstr = 0;	          		  		// Posizione inizio istruzione in riga sorgente
	private int posEndInstr = 0;	                  		// Posizione fine istruzione in riga sorgente	
	private String[] ar_RowsSource;                   		// Array righe sorgente con l'istruzione originaria (estratto da righe sorgente programma)
	private String[] ar_CommentsBeforeInstr;          		// Array righe commento precedenti l'istruzione
	private String name = "";                       		// Nome istruzione (es. IF)
	private String sourceInstr = "";                		// Istruzione source completa e pacchettizzata 
	private boolean terminatedWithPoint = false;	  		// True indica istruzione terminata con un punto

	
	/////////////////////////////////////////////////////////////////////////
    // Informazioni sulla tipologia di istruzioni
	/////////////////////////////////////////////////////////////////////////

	private EnumInstrDataCategory typeInstrJcl = null;            			// Tipo istruzione codificata JCL
	private EnumCobolReservedWords typeInstr = null;            			// Tipo istruzione codificata (MOVE, EXEC SQL)
	private EnumInstrDataCategory typePrecompiler = null; 					// Tipo precompilatore (SQL PRECOMPILER)
	private EnumPrecompilerReservedWords typeInstrPrecompiler = null;		// Codice istruzione specifico sql (SQL SELECT)

	
	/////////////////////////////////////////////////////////////////////////
    // Informazioni ottenute a fronte di analisi sorgente e successive
	/////////////////////////////////////////////////////////////////////////

	// Struttura principale di descrizione valori campi e opzioni dell'istruzione.
	// Ogni entry ha come chiave il nome di un operando/opzione/key di struttura specifica con lo stesso
	// nome della sintassi dell'istruzione (per esempio MAP o MAPSET etc.), preceduto e succeduto da "$".
	// La key viene mappata con un oggetto che può contenere il valore di un operando, la presenza di una opzione
	// oppure una struttura Array o ArrayList o ancora, nel caso più generale, il riferimento all'istanza
	// di una classe specifica che descrive l'istruzione (generalmente si cerca di evitare tale dettaglio).
    // Le tre ArrayList successive di simboli contengono i nomi dei campi rispettivamente definiti dall'istruzione, 
	// in input o in output e non obbligatoriamente sono descritti anche in questa Map.
	private HashMap<String, Object> map_Descriptor = null;		// Descrittore operandi/operatori/Opzioni/strutture istruzione

	// Simboli definiti dentro l'istruzione.
	// Vengono inseriti in questa struttura i simboli quali definizione di label o procedure
	// interne (section se Cobol) se l'istruzione è di procedure.
	// Se l'istruzione è un data item vengono inseriti il nome del data item definito e gli eventuali
	// condition-name se livello 88.
	private ArrayList<InnerSymbolEntry> al_SymbolDefinedInside = null;  
	
	// Simboli in input all'istruzione.
	// Vengono inseriti in questa struttura i nomi dei campi di input che non vengono modificati 
	// dall'istruzione stessa, oppure le label o le procedure interne richiamate. 
	// Per esempio viene caricato il campo A dell'istruzione Move A to B
    // Il simbolo viene mappato su un oggetto DataItemQualifier che descrive come il dato è qualificato
	// per esempio perchè si fa riferimento a un data item sotto un gruppo oppure con degli indici (es. Move A OF group To ..),
	private ArrayList<InnerSymbolEntry> al_SymbolInput = null;  	
	
	// Simboli in output all'istruzione.
	// Vengono inseriti in questa struttura i nomi dei campi di output che vengono modificati 
	// dall'istruzione stessa.  
	// Per esempio vengono caricati i campi B, C e D dell'istruzione Move A To B C D
    // Il simbolo viene mappato su un oggetto DataItemQualifier che descrive come il dato è qualificato
	// per esempio perchè si fa riferimento a un data item sotto un gruppo oppure con degli indici (es. Move A OF group To ..),
	private ArrayList<InnerSymbolEntry> al_SymbolOutput = null;  
	
	// Indica se l'istruzione è dinamica o statica.
	// Se l'istruzione è statica allora contiene in chiaro in map_Descriptor il valore di tutti
	// i suoi operandi e quindi possono essere inserite tutte le relazioni sul db.
	// Se è dinamica deve essere risolta dai processi ricorsivi di analisi dinamica.
	private boolean dynamic = false;

	// Indica se l'istruzione è dinamica senza necessità di elaborazioni.
	// Si tratta di istruzioni dinamiche con tutti i campi da risolvere non referenziati
	// e con un valore iniziale. Si attiva comunque il gestore generalizzato di gestione 
	// delle logiche dinamiche per tenere traccia dell'istruzione dinamica.
	private boolean dynamicLight = false;

	// Indica se l'istruzione dinamica è stata risolta producendo dei valori validi per i campi dinamici.
	// La soluzione ha prodotto dei valori validi ma potrebbero NON essere tutti quelli possibili.
	// Ci possono essere presenti delle ultime assegnazioni, anche parziali, ancora da risolvere.
	private boolean dynamicSolved = false;

	// Indica se l'istruzione dinamica è stata risolta completamente.
	// Quando operandi di una istruzione dinamica vengono risolti individuando 
	// dei valori validi, allora vengono inserite le relazioni e aggiornato il flag dynamicSolved.
	// Tuttavia possono esistere ancora dei path per i quali è necessario risolvere
	// l'ultima assegnazione, nei programmi chiamanti oppure con dati da media esterni.
	// Quando l'istruzione dinamica è completamente risolta questo flag viene impostato a true.
	private boolean dynamicSolvedFull = false;

	// Indica se l'istruzione dinamica e stata risolta in programma diversi da quello di analisi.
	// In questo caso le trasformazioni dei campi elementari hanno portato a campi che possono
	// essere risolti solo in programmi chiamanti o chiamati, a qualsiasi livello di richiamo.
	private boolean dynamicSpreaded = false;

	// Indica se l'istruzione dinamica necessita di dati esterni, non disponibili, per essere risolta.
	// Si tratta di colonne di tabelle, files Vsam, code Cics TS/TD, nomi terminali etc.
	// I valori di tali colonne devono essere inseriti nell'apposita tabella di interfaccia.
	private boolean dynamicWaitingForData = false;

	// Indica se l'istruzione statica è stata risolta.
	// Può essere utile in caso di riesecuzioni dello stesso processo.
	private boolean staticSolved = false;

	// Elenco violazioni.
	// Si tratta di violazioni codificate dell'istruzione rilevate in fase di analisi o
	// in elaborazioni successive. Sono informazioni che vengono collezionate a livello
	// di metriche e aggregate a livello di programma e sezione di programma.
	// Non sono veri e propri errori di programma ma codifiche e situazioni sconsigliate.
	ArrayList<EnumMetricsViolation> al_metricViolation = null;
	
	
	/*  
	 * Costruttore senza parametri
	 */
	public Instruction() {
		super();
		this.typeInstrJcl = EnumInstrDataCategory.NOT_ASSIGNED;
		this.typeInstr = EnumCobolReservedWords.NOT_ASSIGNED;
		this.typePrecompiler = EnumInstrDataCategory.NOT_ASSIGNED;
		this.typeInstrPrecompiler = EnumPrecompilerReservedWords.NOT_ASSIGNED;

		this.msgType = EnumMessageType.NOT_ASSIGNED;
		this.map_Descriptor = new HashMap<String, Object> ();
		this.al_SymbolDefinedInside = new  ArrayList<InnerSymbolEntry>();
		this.al_SymbolInput = new  ArrayList<InnerSymbolEntry>();
		this.al_SymbolOutput = new  ArrayList<InnerSymbolEntry>();
		this.ar_msgParm = new String[0];
		this.al_metricViolation = new ArrayList<EnumMetricsViolation> ();
	}

	/**  
	 * Costruttore utilizzato per istanziare un oggetto Instruction
	 * con tutti i parametri richiesti
	 *  
	 *  @param numInstr 				Numero sequenza istruzione 0-based
	 *  @param RowStartSource 			Numero riga sorgente di analisi di inizio
	 *  @param RowEndSource 			Numero riga sorgente di analisi di fine
	 *  @param PosStart 		    	Posizione inizio istruzione in riga sorgente
	 *  @param PosEnd    		    	Posizione fine istruzione in riga sorgente	
	 *  @param ar_RowsSource          	Array righe sorgente con l'istruzione
	 *  @param ar_RowsSourceComments  	Array righe sorgente con i commenti precedenti l'istruzione
	 *  @param name  	                Nome istruzione
	 *  @param ar_RowsSourceComments  	Istruzione completa estratta
	 *  
	 */
	public Instruction(int numInstr
					  ,int rowStartSource
					  ,int rowEndSource
					  ,int posStartInstr
					  ,int PosEndInstruction
				 	  ,String ar_RowsSource[]
			          ,String ar_CommentsBeforeInstr[]
			          ,String ar_CommentsLeftInstr[]                        
			          ,String ar_CommentsRightInstr[]                        
			          ,String name
			          ,String sourceInstr
			          ) {
		super();
		
		this.numInstr = numInstr;
		this.rowStartSource = rowStartSource;
		this.rowEndSource = rowEndSource;
		this.posStartInstr = posStartInstr;
		this.posEndInstr = PosEndInstruction;	
		this.ar_RowsSource = ar_RowsSource;	
		this.ar_CommentsBeforeInstr = ar_CommentsBeforeInstr;	
//		this.ar_CommentsLeftInstr = ar_CommentsLeftInstr;	
//		this.ar_CommentsRightInstr = ar_CommentsRightInstr;	
		this.name = name;	
		this.sourceInstr = sourceInstr;		
		
		// Strutture per controllo simboli e descrittori
		this.map_Descriptor = new HashMap<String, Object> ();
		this.al_SymbolDefinedInside = new  ArrayList<InnerSymbolEntry>();
		this.al_SymbolInput = new  ArrayList<InnerSymbolEntry>();
		this.al_SymbolOutput = new  ArrayList<InnerSymbolEntry>();
	}

	
	/**
	 * @return the numInstr
	 */
	public int getNumInstr() {
		return this.numInstr;
	}

	/**
	 * @param numInstr the numInstr to set
	 */
	public  void setNumInstr(int numInstr) {
		this.numInstr = numInstr;
	}

	
	/**
	 * @return the numInstrOwner with the block instruction owner
	 */
	public int getNumInstrOwner() {
		return numInstrOwner;
	}

	/**
	 * @param numInstrOwner the numInstrOwner to set
	 */
	public void setNumInstrOwner(int numInstrOwner) {
		this.numInstrOwner = numInstrOwner;
	}

	/**
	 * @return the rowStartSource
	 */
	public int getRowStartSource() {
		return rowStartSource;
	}

	/**
	 * @param rowStartSource the rowStartSource to set
	 */
	public void setRowStartSource(int rowStartSource) {
		this.rowStartSource = rowStartSource;
	}

	/**
	 * @return the rowEndSource
	 */
	public int getRowEndSource() {
		return rowEndSource;
	}

	/**
	 * @param rowEndSource the rowEndSource to set
	 */
	public void setRowEndSource(int rowEndSource) {
		this.rowEndSource = rowEndSource;
	}

	/**
	 * @return the posStart
	 */
	public int getPosStartInstr() {
		return this.posStartInstr;
	}

	/**
	 * @param posStart the posStart to set
	 */
	public void setPosStartInstr(int posStartInstr) {
		this.posStartInstr = posStartInstr;
	}

	/**
	 * @return the posEnd
	 */
	public int getPosEndInstr() {
		return this.posEndInstr;
	}

	/**
	 * @param posEnd the posEnd to set
	 */
	public void setPosEndInstr(int posEndInstr) {
		this.posEndInstr = posEndInstr;
	}


	/**
	 * @return the ar_RowsSource
	 */
	public String[] getRowsSource() {
		return ar_RowsSource;
	}

	/**
	 * @param ar_RowsSource the ar_RowsSource to set
	 */
	public void setRowsSource(String[] ar_RowsSource) {
		this.ar_RowsSource = ar_RowsSource;
	}

	/**
	 * @return the ar_CommentsBeforeInstr
	 */
	public String[] getCommentsBefore() {
		return (ar_CommentsBeforeInstr == null) ? new String[0] : ar_CommentsBeforeInstr ;
	}

	
	/**
	 * Restituisce le righe di commento con * a colonna 7 prima dell'istruzione.<br>
	 * <p>
	 * @return the ar_CommentsBeforeInstr
	 */
	public String[] getCommentsBeforeNotEmpty() {
		String ar_commentsBeforeInstr[] = null;
		ar_commentsBeforeInstr = getCommentsBefore();
		// Non ci sono commenti prima dell'istruzione
		if (ar_commentsBeforeInstr == null) {
			return new String[0];
		}
		// Anomalia di analisi
		if (ar_commentsBeforeInstr.length == 1 && ar_commentsBeforeInstr[0] == null) {
			return new String[0];
		}
		
		ArrayList<String> al_commentsBeforeInstr = null;
		al_commentsBeforeInstr = new ArrayList<String> ();
		for (String strRowComment : ar_commentsBeforeInstr) {
			if (strRowComment == null) {continue;}
			if (strRowComment.trim().equals("")) {continue;}
			if (strRowComment.trim().length() < 7) {continue;}
			if (strRowComment.charAt(6) != '*') {continue;}
			al_commentsBeforeInstr.add(strRowComment);
		}
		ar_commentsBeforeInstr = new String[al_commentsBeforeInstr.size()];
		ar_commentsBeforeInstr =  al_commentsBeforeInstr.toArray(ar_commentsBeforeInstr);
		return ar_commentsBeforeInstr ;
	}

	
	
	/**
	 * @param ar_CommentsBeforeInstr the ar_CommentsBeforeInstr to set
	 */
	public void setCommentsBefore(String[] ar_CommentsBeforeInstr) {
		this.ar_CommentsBeforeInstr = ar_CommentsBeforeInstr;
	}
 

	/**
	 * @return the name of instruction coded. Es. IF
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the sourceInstr
	 */
	public String getSourceInstr() {
		return sourceInstr;
	}

	/**
	 * @param sourceInstr the sourceInstr to set
	 */
	public void setSourceInstr(String sourceInstr) {
		this.sourceInstr = sourceInstr;
	}

	/**
	 * 
	 * Restituisce la Map con le strutture specifiche codificate dell'istruzione.<br>
	 * Il contenuto dipende dall'istruzione. A fronte di un nome viene mappato un oggetto<br>
	 * che può esssere di volta in volta il valore di un parametro, la presenza di un opzione <br>
	 * o il riferimento a una struttura o un oggetto specifico.
	 * 
	 * @return Map descriptor
	 */
	public Map<String, Object> getMapDescriptor() {
		return map_Descriptor;
	}

	/**
	 * 
     * Imposta Map completa con tutti i descrittori dell'istruzione
	 * 
	 * @param Map Descriptor to set
	 */
	public void setMapDescriptor(Map<String, Object> map_Descriptor) {
		this.map_Descriptor = (HashMap<String, Object>) map_Descriptor;
	}

	/**
	 * 
	 * Inserisce l'oggetto mappato dal descrittore chiave<br>
	 * Si può trattare di un oggetto String con il valore di una Label oppure o di un parametro<br>
	 * oppure di una struttura comunque complessa <br>
	 * 
	 * @param String key
	 * @return Map descriptor
	 */
	public Object addMapDescriptorObject(String objectKey, Object object) {
		return map_Descriptor.put(objectKey, object);
	}

	/**
	 * 
	 * Restituisce l'oggetto mappato dal escrittore chiave<br>
	 * Si può trattare di un oggetto String con il valore di una Label oppure o di un parametro<br>
	 * oppure di una struttura comunque complessa <br>
	 * 
	 * @param String key
	 * @return Map descriptor
	 */
	public Object getMapDescriptorObject(String objectKey) {
		return map_Descriptor.get(objectKey);
	}


	/**
	 * 
	 * Imposta ArrayList completo con i simboli definiti direttamente dall'istruzione.<br>
	 * Si tratta di simboli come nomi di label, di procedure interne (Section).<br>
	 * Nel caso di istruzioni di definizione di dati si tratta invece del nome del<br>
	 * del data item, dgli eventuali nomi condizione Cobol di livello 88, etc.<br>
	 * Il caso più normale per un data item è che definisca un solo simbolo con il suo nome.
	 * 
	 * @param map_SymbolInput the map_SymbolInput to set
	 */
	public void setSymbolsDefinedInside(ArrayList<InnerSymbolEntry> al_SymbolInput) {
		this.al_SymbolInput = al_SymbolInput;
	}

	/**
	 * 
	 * Imposta ArrayList completo con i simboli utilizzati in input 
	 * 
	 * @param map_SymbolInput the map_SymbolInput to set
	 */
	public void setSymbolsInput(ArrayList<InnerSymbolEntry> al_SymbolInput) {
		this.al_SymbolInput = al_SymbolInput;
	}


	
	/**
	 * 
	 * Imposta ArrayList completo con i simboli utilizzati in output
	 * 
	 * @param map_SymbolOutput the map_SymbolOutput to set
	 */
	public void setSymbolsOutput(ArrayList<InnerSymbolEntry> al_SymbolOutput) {
		this.al_SymbolOutput = al_SymbolOutput;
	}


	/**
	 * 
	 * Restituisce  Array di InnerEntry con i simboli definiti all'interno dell'istruzione stessa.<br>
	 * 
	 * @return String[] ar_SymbolDefined
	 */
	public InnerSymbolEntry[] symbolsDefinedInside() {
	
		InnerSymbolEntry[] ar_SymbolDefinedInside = null;
		
		// Converto in array
		ar_SymbolDefinedInside = new InnerSymbolEntry[al_SymbolDefinedInside.size()];
		ar_SymbolDefinedInside = al_SymbolDefinedInside.toArray(ar_SymbolDefinedInside);
		
		return ar_SymbolDefinedInside;
	}

	
	/**
	 * 
	 * Restituisce  Array di InnerEntry con simboli utilizzati in input, uniti a quelli utilizzati <br>
	 * in output in un unico Array risultato.<br>
	 * 
	 * @return InnerEntry[] ar_SymbolsUsed
	 */
	public InnerSymbolEntry[] symbolsUsed() {
		
		// Union simboli in input e in output
		ArrayList<InnerSymbolEntry> al_SymbolsUsed = null;
		InnerSymbolEntry[] ar_SymbolsUsed = null;
		
		// ArrayList simboli di output
		al_SymbolsUsed = new ArrayList<InnerSymbolEntry>();
		
		// Inserisco in output i set con i simboli in input e in output
		al_SymbolsUsed.addAll(al_SymbolInput);
		al_SymbolsUsed.addAll(al_SymbolOutput);
		
		// Alloco Array di output
		ar_SymbolsUsed = new InnerSymbolEntry[al_SymbolsUsed.size()];
		ar_SymbolsUsed = al_SymbolsUsed.toArray(ar_SymbolsUsed);
		
		return ar_SymbolsUsed;
	}


	
	/**
	 * 
	 * Restituisce Array con InnerEntry dei simboli in input
	 * 
	 * @return array  Entry<String, DataItemQualifier>[] con nome simbolo e qualificatore utilizzo
	 */
	public InnerSymbolEntry[] symbolsUsedInput() {
		InnerSymbolEntry ar_SymbolInput[] = null;
		
		ar_SymbolInput = new InnerSymbolEntry[al_SymbolInput.size()];
		ar_SymbolInput = al_SymbolInput.toArray(ar_SymbolInput);
		
		return  ar_SymbolInput;
	}

	/**
	 * 
	 * Restituisce Array con Entry<String, DataItemQualifier> dei simboli in output
	 * 
	 * @return array  Entry<String, DataItemQualifier>[] con nome simbolo e qualificatore utilizzo
	 */
	public InnerSymbolEntry[] symbolsUsedOutput() {
		InnerSymbolEntry ar_SymbolOutput[] = null;
		
		ar_SymbolOutput = new InnerSymbolEntry[al_SymbolOutput.size()];
		ar_SymbolOutput = al_SymbolOutput.toArray(ar_SymbolOutput);
		
		return  ar_SymbolOutput;
	}


	/**
	 * 
	 * Restituisce il qualificatore di utilizzo del simbolo in input all'istruzione, <br>
	 * come oggetto di {@link DataItemQualifier}.<br>
	 * Tale oggetto specifica posizione inizio, lunghezza, indici utilizzati per referenziare il data item etc.<br>
	 * Se il simbolo non è utilizzato in input restituisce null.
	 * 
	 * @return DataItemQualifier object
	 */
	public DataItemQualifier symbolUsedInput(String symbolName) {
		
		DataItemQualifier qualifier = null; 

		for (InnerSymbolEntry innerEntry : al_SymbolInput) {
			if (innerEntry.symbolName.equals(symbolName)) {
				qualifier = innerEntry.qualifier;
				break;
			}
		}
		
		return qualifier;
	}

	/**
	 * 
	 * Restituisce il qualificatore di utilizzo del simbolo in output all'istruzione, <br>
	 * come oggetto di {@link DataItemQualifier}.<br>
     * Tale oggetto specifica posizione inizio, lunghezza, indici utilizzati per referenziare il data item etc.<br>
	 * Se il simbolo non è utilizzato in input restituisce null.
	 * 
	 * @return DataItemQualifier object
	 */
	public DataItemQualifier symbolUsedOutput(String symbolName) {
		
		DataItemQualifier qualifier = null; 

		for (InnerSymbolEntry innerEntry : al_SymbolOutput) {
			if (innerEntry.symbolName.equals(symbolName)) {
				qualifier = innerEntry.qualifier;
				break;
			}
		}
		
		return qualifier;
	}

	/**
	 * 
	 * Inserisce il simbolo nella struttura dei simboli definiti all'interno dell'istruzione<br>
	 * come per esempio il nome di un campo in data division o il nome di una label o di una literal.
	 * 
	 * @param String simbolo definito dall'istruzione da inserire
	 * @param EnumSymbolType symbolType
	 */
	public void addSymbolDefinedInside(String symbolName, EnumSymbolType symbolType) {
		
		DataItemQualifier qualifier = null;
		InnerSymbolEntry innerEntry = null;
		
		qualifier = new DataItemQualifier();
		qualifier.setSymbolType(symbolType);
		innerEntry = new InnerSymbolEntry(symbolName, qualifier);
		
		// Inserisco simbolo definito dall'istruzione
		al_SymbolDefinedInside.add(innerEntry);
		
		return;
	}

	/**
	 * 
	 * Inserisce il simbolo non qualificato nella struttura di input
	 * 
	 * @param String simbolo da inserire
	 */
	public void addSymbolInput(String symbolName) {
		
		// Inserisco simbolo senza array di qualificatori
		al_SymbolInput.add(new InnerSymbolEntry(symbolName, null));
		
		return;
	}

	/**
	 * 	  
	 * Inserisce il simbolo qualificato nella struttura di input.
	 * 
	 * @param symbolName
	 * @param qualifier
	 */
	public void addSymbolInput(String symbolName, DataItemQualifier qualifier) {
				
		// Inserisco simbolo con qualificatore
		al_SymbolInput.add(new InnerSymbolEntry(symbolName, qualifier));
		
		return;
	}

	/**
	 * 	  
	 * Inserisce il simbolo nella struttura dei simboli di input dell'istruzione.
	 * Viene generato automaticamente un qualificatore dove viene memorizzato il tipo
	 * di simbolo in input all'istruzione.
	 * 
	 * @param symbolName
	 * @param EnumSymbolType symbolType
	 */
	public void addSymbolInput(String symbolName, EnumSymbolType symbolType) {
		
		DataItemQualifier qualifier;

		qualifier = new DataItemQualifier();
		qualifier.setSymbolType(symbolType);
		
		// Inserisco simbolo con qualificatore
		al_SymbolInput.add(new InnerSymbolEntry(symbolName, qualifier));
		
		return;
	}

	
	
	/**
	 * 
	 * Inserisce il simbolo non qualificato nella struttura di output
	 * 
	 * @param String simbolo da inserire
	 */
	public void addSymbolOutput(String symbolName) {
		
		// Inserisco simbolo senza array di qualificatori
		al_SymbolOutput.add(new InnerSymbolEntry(symbolName, null));
		
		return;
	}

	/**
	 * 	  
	 * Inserisce il simbolo qualificato nella struttura di output
	 * 
	 * @param symbolName
	 * @param qualifier
	 */
	public void addSymbolOutput(String symbolName, DataItemQualifier qualifier) {
		
		// Inserisco simbolo con qualificatore
		al_SymbolOutput.add(new InnerSymbolEntry(symbolName, qualifier));
		
		return;
	}


	/**
	 * 	  
	 * Inserisce il simbolo nella struttura dei simboli di output dell'istruzione.
	 * Viene generato automaticamente un qualificatore dove viene memorizzato il tipo
	 * di simbolo in output all'istruzione.
	 * 
	 * @param symbolName
	 * @param EnumSymbolType symbolType
	 */
	public void addSymbolOutput(String symbolName, EnumSymbolType symbolType) {
		
		DataItemQualifier qualifier;
		
		
		qualifier = new DataItemQualifier();
		qualifier.setSymbolType(symbolType);
		
		// Inserisco simbolo con qualificatore
		al_SymbolOutput.add(new InnerSymbolEntry(symbolName, qualifier));
		
		return;
	}

	
	/**
	 * 	  
	 * Azzera le strutture dei simboli presenti nell'istruzione.<br>
	 * <p>
	 * Si tratta delle strutture che contengono:<br>
	 * - Simboli definiti dentro l'istruzione stessa
	 * - Simboli in input all'istruzsione
	 * - Simboli in output all'istruzione<br>
	 * <p>
	 * Questo metodo viene utilizzato a fronte di inclusione copy con replacing,<br>
	 * nel caso di istruzione modificata dal replacing stesso e quindi rianalizzata.<br>
	 * In questo modo vengono indicizzati a livello di programma i simboli eventualmente <br>
	 * inclusi con copy ma modificati dal replacing.
	 * 
	 * @param symbolName
	 * @param EnumSymbolType symbolType
	 */
	public void clearXrefStruct() {
		al_SymbolDefinedInside.clear();
		al_SymbolInput.clear();  
		al_SymbolOutput.clear();  
		return;
	}

	/**
	 * 
	 * Restituisce true se il simbol è definito all'inetrno dell'istruzione
	 * 
	 * @return boolean True se in input
	 */
	public boolean isSymbolDefinedInside(String symbolName) {
		
		for (InnerSymbolEntry innerEntry : al_SymbolDefinedInside) {
			
			if (innerEntry.symbolName.equals(symbolName)) {
				return true;
			}
		}
		
		return false;
	}

	
	/**
	 * 
	 * Restituisce true se il simbol è di input per l'istruzione
	 * 
	 * @return boolean True se in input
	 */
	public boolean isSymbolInput(String symbolName) {
		
		for (InnerSymbolEntry innerEntry : al_SymbolInput) {
			
			if (innerEntry.symbolName.equals(symbolName)) {
				return true;
			}
		}
		
		return false;
	}

	/**
	 * 
	 * Restituisce true se il simbol qualificato è di input per l'istruzione
	 * 
	 * @return boolean True se in input
	 */
	public boolean isSymbolInput(String symbolName, String ofField) {
		
		DataItemQualifier qualifier = null;
	
		for (InnerSymbolEntry innerEntry : al_SymbolInput) {
			
			if (!innerEntry.symbolName.equals(symbolName)) {
				continue;
			}
			
			qualifier = innerEntry.qualifier;
			
			// Simbolo utilizzato in input con la qualidìficazione
			if (qualifier.getGroupNameField().equalsIgnoreCase(ofField)) {
				return true;
			}
            return false;
		}
		
		return false;
	}


	
	/**
	 * 
	 * Restituisce true se il simbol è di output per l'istruzione
	 * 
	 * @return boolean True se in output
	 */
	public boolean isSymbolOutput(String symbolName) {
		
		for (InnerSymbolEntry innerEntry : al_SymbolOutput) {
			
			if (innerEntry.symbolName.equals(symbolName)) {
				return true;
			}
		}
		
		return false;
	}

	/**
	 * 
	 * Restituisce true se il simbol qualificato è di output per l'istruzione
	 * 
	 * @return boolean True se in output
	 */
	public boolean isSymbolOutput(String symbolName, String ofField) {
		
		DataItemQualifier qualifier = null;
		
		for (InnerSymbolEntry innerEntry : al_SymbolOutput) {
			
			if (!innerEntry.symbolName.equals(symbolName)) {
				continue;
			}
			
			qualifier = innerEntry.qualifier;
			
			// Simbolo utilizzato in input con la qualidìficazione
			if (qualifier.getGroupNameField().equalsIgnoreCase(ofField)) {
				return true;
			}
            return false;
		}
		
		
		return false;
	}

	/**
	 * 
	 * Restituisce true se il simbol è sia di input sia di output per l'istruzione.
	 * 
	 * @return boolean True se in input/iìoutput
	 */
	public boolean isSymbolInputOutput(String symbolName) {
		
		if (isSymbolInput(symbolName) && isSymbolOutput(symbolName) ) {
			return true;
		}
		
		return false;
	}

	/**
	 * 
	 * Restituisce true se il simbol è di input/output per l'istruzione
	 * 
	 * @return boolean True se in input/output
	 */
	public boolean isSymbolInputOutput(String symbolName, String ofField) {
		
		if (isSymbolInput(symbolName, ofField) && isSymbolOutput(symbolName, ofField) ) {
			return true;
		}
		
		return false;
	}

	
	
	/**
	 * Restituisce true se si tratta di una istruzione dinamica.
	 * 
	 * @return the dynamic
	 */
	public boolean isDynamic() {
		return dynamic;
	}

	/**
	 * Restituisce true se l'istruzione dinamica è stata risolta completamente.<br>
	 * <p>
	 * Quando operandi di una istruzione dinamica vengono risolti individuando 
	 * dei valori validi, allora vengono inserite le relazioni e aggiornato il flag dynamicSolved.<br>
	 * Tuttavia possono esistere ancora dei path per i quali è necessario risolvere
	 * l'ultima assegnazione, nei programmi chiamanti oppure con dati da media esterni.<br>
	 * Quando l'istruzione dinamica è completamente risolta questo flag viene impostato a true.<br>
	 * <p>
	 * 
	 * @return the dynamicSolvedFull
	 */
	public boolean isDynamicSolvedFull() {
		return dynamicSolvedFull;
	}
	
	
	
	
	/**
	 * Restituisce se l'istruzione è dinamica <tt>leggera</tt><br>
	 * <p>
	 * Si tratta di istruzioni dinamiche con tutti i campi da rislovere non
	 * referenziati e con un valore iniziale.<br>
	 * Per la soluzione non si attiva il gestore generalizzato di gestione
	 * logiche {@link LogicManager} ma si prende il valore iniziale del campo.<br>
	 * <p>
	 * @return the dynamicLight
	 */
	public boolean isDynamicLight() {
		return dynamicLight;
	}

	/**
	 * Imposta se l'istruzione è dinamica <tt>leggera</tt><br>
	 * <p>
	 * Si tratta di istruzioni dinamiche con tutti i campi da rislovere non
	 * referenziati e con un valore iniziale.<br>
	 * Per la soluzione non si attiva il gestore generalizzato di gestione
	 * logiche {@link LogicManager} ma si prende il valore iniziale del campo.<br>
	 * <p>
	 * @param dynamicLight the dynamicLight to set
	 */
	public void setDynamicLight(boolean dynamicLight) {
		this.dynamicLight = dynamicLight;
	}

	/**
	 * Restituisce true se l'istruzione dinamica e stata risolta in programma diversi da quello di analisi.<br>
	 * <p>
	 * In questo caso le trasformazioni dei campi elementari hanno portato a campi che possono
	 * essere risolti solo in programmi chiamanti o chiamati, a qualsiasi livello di richiamo.
	 * <p>
	 * 
	 * @return the dynamicSpreaded
	 */
	public boolean isDynamicSpreaded() {
		return dynamicSpreaded;
	}
	
	/**
	 * Restituisce true se l'istruzione dinamica necessita di dati esterni, non disponibili, per essere risolta.<br>
	 * <p>
	 * Si tratta di colonne di tabelle, files Vsam, code Cics TS/TD, nomi terminali etc.
	 * I valori di tali colonne devono essere inseriti nell'apposita tabella di interfaccia.
	 * <p>
	 * 
	 * @return the dynamicWaitingForData
	 */
	public boolean isDynamicWaitingForData() {
		return dynamicWaitingForData;
	}

	/**
	 * 
	 * Restituisce true se l'istruzione dinamica è stata risolta producendo dei valori validi per i campi dinamici.<br>
	 * <p>
	 * La soluzione ha prodotto dei valori validi ma potrebbero NON essere tutti quelli possibili.<br>
	 * Ci possono essere presenti delle ultime assegnazioni, anche parziali, ancora da risolvere.<br>
	 * 
	 * @return the dynamicSolved
	 */
	public boolean isDynamicSolved() {
		return dynamicSolved;
	}


	/**
	 * 
	 * Restituisce true se l'operando in input è dinamico.<br>
	 * <p>
	 * Si verifica nella struttura interna di elenco operandi
	 * dinamici nell'istruzione
	 * 
	 * @return true se dinamico
	 */
	@SuppressWarnings("unchecked")
	public boolean isOperandDynamic(String dynamicOperandName) {
		
		ArrayList<String> al_dynamicOperand = null;
		
		al_dynamicOperand = (ArrayList<String>) this.getMapDescriptorObject("$OPRND-DYN-NAME-LIST$"); 
		
		// Nessun operando dinamico nell'istruzione
		if (al_dynamicOperand == null) {
			return false;
		}

		// Campo dinamico non presente nell'istruzione
		if (!(al_dynamicOperand.contains(dynamicOperandName))) {
			return false;
		}

		// campo dinamico presente nell'istruzione
		
		return true;
	}



	/**
	 * Imposta l'istruzione come dinamica
	 * 
	 * @param dynamic the dynamic to set
	 */
	public void setDynamic(boolean dynamic) {
		this.dynamic = dynamic;
	}

	/**
	 * Imposta se l'istruzione dinamica e stata risolta in programmi diversi da quello di analisi.<br>
	 * <p>
	 * In questo caso le trasformazioni dei campi elementari hanno portato a campi che possono
	 * essere risolti solo in programmi chiamanti o chiamati, a qualsiasi livello di richiamo.
	 * <p>
	 * 
	 * @param dynamic spreaded the dynamic to set
	 */
	public void setDynamicSpreaded(boolean dynamicSpreaded) {
		this.dynamicSpreaded = dynamicSpreaded;
	}

	/**
	 * Imposta se l'istruzione dinamica necessita di dati esterni, non disponibili, per essere risolta.<br>
	 * <p>
	 * Si tratta di colonne di tabelle, files Vsam, code Cics TS/TD, nomi terminali etc.
	 * I valori di tali colonne devono essere inseriti nell'apposita tabella di interfaccia.
	 * <p>
	 * 
	 * @param dynamic waiting fro data to set
	 */
	public void setDynamicWaitingForData(boolean dynamicWaitingForData) {
		this.dynamicWaitingForData = dynamicWaitingForData;
	}

	/**
	 * 
	 * Imposta se l'istruzione dinamica è stata risolta producendo dei valori validi per i campi dinamici.<br>
	 * <p>
	 * La soluzione ha prodotto dei valori validi ma potrebbero NON essere tutti quelli possibili.<br>
	 * Ci possono essere presenti delle ultime assegnazioni, anche parziali, ancora da risolvere.<br>
	 * 
	 * @param solved the solved to set
	 */
	public void setDynamicSolved(boolean dynamicSolved) {
		this.dynamicSolved = dynamicSolved;
	}


	/**
	 * Imposta se l'istruzione dinamica è stata risolta completamente.<br>
	 * <p>
	 * Quando operandi di una istruzione dinamica vengono risolti individuando 
	 * dei valori validi, allora vengono inserite le relazioni e aggiornato il flag dynamicSolved.<br>
	 * Tuttavia possono esistere ancora dei path per i quali è necessario risolvere
	 * l'ultima assegnazione, nei programmi chiamanti oppure con dati da media esterni.<br>
	 * Quando l'istruzione dinamica è completamente risolta questo flag viene impostato a true.<br>
	 * <p>
	 * 
	 * 
	 * @param dynamicSolvedFull the dynamicSolvedFull to set
	 */
	public void setDynamicSolvedFull(boolean dynamicSolvedFull) {
		this.dynamicSolvedFull = dynamicSolvedFull;
	}


	/**
	 * @return the staticSolved
	 */
	public boolean isStaticSolved() {
		return staticSolved;
	}

	/**
	 * @param staticSolved the staticSolved to set
	 */
	public void setStaticSolved(boolean staticSolved) {
		this.staticSolved = staticSolved;
	}

	/**
	 * @return the terminatedWithPoint
	 */
	public boolean isTerminatedWithPoint() {
		return terminatedWithPoint;
	}

	/**
	 * @param terminatedWithPoint the terminatedWithPoint to set
	 */
	public void setTerminatedWithPoint(boolean terminatedWithPoint) {
		this.terminatedWithPoint = terminatedWithPoint;
	}
	/**
	 * @return the msgCode
	 */
	public String getMsgCode() {
		return msgCode;
	}

	/**
	 * @param msgCode the msgCode to set
	 */
	public void setMsgCode(String msgCode) {
		this.msgCode = msgCode;
	}

	/**
	 * Restituisce i parametri del messaggio di errore
	 * 
	 * @return the ar_msgParm
	 */
	public String[] getMsgParm() {
		return ar_msgParm;
	}

	/**
	 * Imposta i parametri del messaggio di errore
	 * 
	 * @param ar_msgParm the ar_msgParm to set
	 */
	public void setMsgParm(String ... ar_msgParm) {
		this.ar_msgParm = ar_msgParm;
	}

	/**
	 * @return the msgType
	 */
	public EnumMessageType getMsgType() {
		return msgType;
	}

	/**
	 * @param msgType the msgType to set
	 */
	public void setMsgType(EnumMessageType msgType) {
		this.msgType = msgType;
	}

	/**
	 * 
	 * Impostazione di tutti i parametri di errore per l'istruzione.
	 * 
	 * @param msgType the msgType to set
	 */
	public void setInfoError(EnumMessageType msgType
						   , String msgCode
						   , String tokenInError
						   , ExceptionAmrita excpError
						   , String ... ar_msgParm
							) {
		
		this.msgType = msgType;
		this.msgCode = msgCode;
		this.tokenInError = tokenInError;
		this.excpError = excpError;
		this.ar_msgParm = ar_msgParm;
	}

	
	/**
	 * @return the parsingError
	 */
	public boolean isParsingError() {
		return parsingError;
	}

	/**
	 * @param parsingError the parsingError to set
	 */
	public void setParsingError(boolean parsingError) {
		this.parsingError = parsingError;
	}

	
	/**
	 * @return the semanticError
	 */
	public boolean isSemanticError() {
		return semanticError;
	}

	/**
	 * @param semanticError the semanticError to set
	 */
	public void setSemanticError(boolean semanticError) {
		this.semanticError = semanticError;
	}

	/**
	 * @return the warning
	 */
	public boolean isWarning() {
		return warning;
	}

	/**
	 * @param warning the warning to set
	 */
	public void setWarning(boolean warning) {
		this.warning = warning;
	}

	/**
	 * @return the excpError
	 */
	public ExceptionAmrita getExcpError() {
		return excpError;
	}

	/**
	 * @param excpError the excpError to set
	 */
	public void setExcpError(ExceptionAmrita excpError) {
		this.excpError = excpError;
	}

	/**
	 * @return the tokenInError
	 */
	public String getTokenInError() {
		return tokenInError;
	}

	/**
	 * Restituisce un array con i nomi dei campi dinamici dell'istruzione
	 * da risolvere o risolti.<br>
	 * <p>
	 * Se non ci sono campi dinamici restituisce un array vuoto.
	 * 
	 * @return String ar_dynamicOperand[]
	 */
	@SuppressWarnings("unchecked")
	public String[] getDynamicOperandNames() {
		
		String ar_dynamicOperand[] = null;
		ArrayList<String> al_dynamicOperand = null;
		
		// Estrazione nomi identificatori dinamici
		al_dynamicOperand =  (ArrayList<String>) this.getMapDescriptorObject("$OPRND-DYN-NAME-LIST$");
		if (al_dynamicOperand == null) {
			return new String[]{};
		}
		
		// Trasformo in array
		ar_dynamicOperand = new String[al_dynamicOperand.size()];
		ar_dynamicOperand = al_dynamicOperand.toArray(ar_dynamicOperand);
		
		return ar_dynamicOperand;
	}

	/**
	 * Restituisce un array con i nomi delle opzioni presenti nell'istruzione.<br>
	 * <p>
	 * Nel casoo di istruzione Cics si può trattare di ERASEAUP, FREEKB e in
	 * generale di qualsiasi opzione presente in una istruzione Cics.<br>
	 * 
	 * @return String ar_optionName[]
	 */
	@SuppressWarnings("unchecked")
	public String[] getOptionNames() {
		
		String ar_optionName[] = null;
		ArrayList<String> al_optionName = null;
		
		// Estrazione opzioni istruzione
		al_optionName =  (ArrayList<String>) this.getMapDescriptorObject("$OPT-LIST$");
		if (al_optionName == null) {
			return new String[]{};
		}
		
		// Trasformo in array
		ar_optionName = new String[al_optionName.size()];
		ar_optionName = al_optionName.toArray(ar_optionName);
		
		return ar_optionName;
	}


	/**
	 * Restituisce un array con i nomi degli operandi in input nell'istruzione.<br>
	 * <p>
	 * Nel casoo di istruzione Cics si può trattare di MAP, PROGRAM,FILE e in
	 * generale di qualsiasi operando presente in una istruzione Cics.<br>
	 * 
	 * @return String ar_optionName[]
	 */
	@SuppressWarnings("unchecked")
	public String[] getOperandInputNames() {
		
		String ar_operandInputName[] = null;
		ArrayList<String> al_operandInputName = null;
		
		// Estrazione opzioni istruzione
		al_operandInputName =  (ArrayList<String>) this.getMapDescriptorObject("$OPRND-INP-LIST$");
		if (al_operandInputName == null) {
			return new String[]{};
		}
		
		// Trasformo in array
		ar_operandInputName = new String[al_operandInputName.size()];
		ar_operandInputName = al_operandInputName.toArray(ar_operandInputName);
		
		return ar_operandInputName;
	}

	/**
	 * Restituisce un array con gli identificatori Cobol completi degli operandi in input nell'istruzione.<br>
	 * <p>
	 * Nel caso di istruzione Cics si può trattare di MAP, PROGRAM,FILE e in
	 * generale di qualsiasi operando presente in una istruzione Cics.<br>
	 * 
	 * @return DataItemCobolIdentifier ar_optionName[]
	 */
	public DataItemCobolIdentifier[] getOperandsInput() {
		
		ArrayList<DataItemCobolIdentifier> al_operandInput = null;
		DataItemCobolIdentifier ar_operandInput[] = null;
		DataItemCobolIdentifier operandInput = null;
		String ar_operandInputName[] = null;
	
		al_operandInput = new ArrayList<DataItemCobolIdentifier>();

		ar_operandInputName = this.getOperandInputNames();

		// Scan nomi identificatori in input
		for (String operandInputName : ar_operandInputName) {
			operandInput = this.getOperand(operandInputName);
			al_operandInput.add(operandInput);
		}
		
		// Trasform0 in array
		ar_operandInput = new DataItemCobolIdentifier[al_operandInput.size()];
		ar_operandInput = al_operandInput.toArray(ar_operandInput);
		
		return ar_operandInput;
	}

	/**
	 * Restituisce un array con gli identificatori Cobol completi degli operandi in output nell'istruzione.<br>
	 * <p>
	 * Nel caso di istruzione Cics si può trattare di INTO e in
	 * generale di qualsiasi operando presente in una istruzione Cics.<br>
	 * 
	 * @return DataItemCobolIdentifier ar_optionName[]
	 */
	public DataItemCobolIdentifier[] getOperandsOutput() {
		
		ArrayList<DataItemCobolIdentifier> al_operandOutput = null;
		DataItemCobolIdentifier ar_operandOutput[] = null;
		DataItemCobolIdentifier operandOutput = null;
		String ar_operandOutputName[] = null;

		al_operandOutput = new ArrayList<DataItemCobolIdentifier>();

		ar_operandOutputName = this.getOperandOutputNames();
		
		// Scan nomi identificatori in input
		for (String operandOutputName : ar_operandOutputName) {
			operandOutput = this.getOperand(operandOutputName);
			al_operandOutput.add(operandOutput);
		}
		
		// Trasform0 in array
		ar_operandOutput = new DataItemCobolIdentifier[al_operandOutput.size()];
		ar_operandOutput = al_operandOutput.toArray(ar_operandOutput);
		
		return ar_operandOutput;
	}

	/**
	 * Restituisce un array con i nomi degli operandi in output nell'istruzione.<br>
	 * <p>
	 * Nel casoo di istruzione Cics si può trattare di INTO e in generale
	 * di qualsiasi operando in output presente in una istruzione Cics.<br>
	 * 
	 * @return String ar_optionName[]
	 */
	@SuppressWarnings("unchecked")
	public String[] getOperandOutputNames() {
		
		String ar_operandOutputName[] = null;
		ArrayList<String> al_operandOutputName = null;
		
		// Estrazione opzioni istruzione
		al_operandOutputName =  (ArrayList<String>) this.getMapDescriptorObject("$OPRND-OUT-LIST$");
		if (al_operandOutputName == null) {
			return new String[]{};
		}
		
		// Trasformo in array
		ar_operandOutputName = new String[al_operandOutputName.size()];
		ar_operandOutputName = al_operandOutputName.toArray(ar_operandOutputName);
		
		return ar_operandOutputName;
	}

	/**
	 * Restituisce un array con i nomi degli operandi dell'istruzione.<br>
	 * <p>
	 * Vengono restituiti sia gli operandi in input che quelli in output.
	 * 
	 * @return String ar_optionName[]
	 */
	public String[] getOperandNames() {
		
		String ar_operandInputName[] = null;
		String ar_operandOutputName[] = null;
		String ar_operandName[] = null;
		List<String> al_operandInputName = null;
		List<String> al_operandOutputName = null;
		List<String> al_operandName = null;
		
		ar_operandInputName = this.getOperandInputNames();
		ar_operandOutputName = this.getOperandOutputNames();

		al_operandInputName = Arrays.asList(ar_operandInputName);
		al_operandOutputName = Arrays.asList(ar_operandOutputName);
		al_operandName = new ArrayList<String>();
		al_operandName.addAll(al_operandInputName);
		al_operandName.addAll(al_operandOutputName);
		
		// Trasformo in array
		ar_operandName = new String[al_operandName.size()];
		ar_operandName = al_operandName.toArray(ar_operandName);
		
		return ar_operandName;
	}
	
	
	/**
	 * Restituisce un array con gli identificatori Cobol dei campi dinamici dell'istruzione
	 * da risolvere o risolti.<br>
	 * <p>
	 * Se non ci sono campi dinamici restituisce un array vuoto.
	 * 
	 * @param DataItemCobolIdentifier ar_dynamicOperand[]
	 */
	@SuppressWarnings("unchecked")
	public DataItemCobolIdentifier[] getDynamicOperandIdentifiers() {
		
		DataItemCobolIdentifier dynamicOperandIdentifier;
		DataItemCobolIdentifier ar_dynamicOperandIdentifier[] = null;
		ArrayList<String> al_dynamicOperandName = null;
		ArrayList<DataItemCobolIdentifier> al_dynamicOperandIdentifier = null;

		al_dynamicOperandIdentifier = new ArrayList<DataItemCobolIdentifier>();
		

		// Estrazione nomi identificatori dinamici
		al_dynamicOperandName = (ArrayList<String>) this.getMapDescriptorObject("$OPRND-DYN-NAME-LIST$");
		if (al_dynamicOperandName == null) {
			return new DataItemCobolIdentifier[]{};
		}
		
		// Scan nomi operandi dinamici e recupero identificatore Cobol
		for (String dynamicOperandName : al_dynamicOperandName) {
			dynamicOperandIdentifier = this.getOperand(dynamicOperandName);
			al_dynamicOperandIdentifier.add(dynamicOperandIdentifier);
		}
		
		// Trasformo in array
		ar_dynamicOperandIdentifier = new DataItemCobolIdentifier[al_dynamicOperandIdentifier.size()];
		ar_dynamicOperandIdentifier = al_dynamicOperandIdentifier.toArray(ar_dynamicOperandIdentifier);
		
		return ar_dynamicOperandIdentifier;
	}
	


	/**
	 * Inserisce un operando dinamico nell'istruzione.<br>
	 * <p>
	 * L'operando è identificato da un id chiave (es. FILE o PROGRAM)
	 * e qualificato da un identificatore Cobol completo {@link DataItemCobolIdentifier}.<br>
	 * Viene inserito il normale operando identificabile dal nome del campo.<br>
	 * Viene inserito l'operando come dinamico identificabile dalll'id chiave.<br>
	 * Esecuzioni multiple di questo metodo vengono ignorate.
	 * 
	 * @param String idOperandkey
	 * @param DataItemCobolIdentifier identifierOperand
	 */
	@SuppressWarnings("unchecked")
	public void addOperandDynamic(String idOperandkey, DataItemCobolIdentifier identifierOperand) {
		
		ArrayList<String> al_dynamicOperand = null;
	

		//////////////////////////////////////////////////////////////////////// 
		// (1) Inserimento implicito operando, identificato dal nome del campo
		//////////////////////////////////////////////////////////////////////// 
		
		this.addOperand(identifierOperand.getNameIdentifier(), identifierOperand);
		

		/////////////////////////////////////////////////////////////////////
		// (2) Inserimento operando dinamico identificato dal nome del campo
		/////////////////////////////////////////////////////////////////////
		
		// Inserimento in struttura elenco identificatori operandi dinamici identificati dal nome
		if (this.getMapDescriptorObject("$OPRND-DYN-NAME-LIST$") == null) {
			al_dynamicOperand = new ArrayList<String>();
			this.addMapDescriptorObject("$OPRND-DYN-NAME-LIST$", al_dynamicOperand);
		}
		al_dynamicOperand = (ArrayList<String>) this.getMapDescriptorObject("$OPRND-DYN-NAME-LIST$");
		// Campo non presente: inserisco
		if (!al_dynamicOperand.contains(identifierOperand.getNameIdentifier())) {
			al_dynamicOperand.add(identifierOperand.getNameIdentifier());
		}

		return;
	}


	/**
	 * Restituisce l'identificatore Cobol completo di un operando dinamico.<br>
	 * <p>
	 * Viene richiesto l'id chiave dell'operando dinamico come , per esempio,
	 * MAP, MAPSET, PROGRAM etc. 
	 * 
	 * @return String idOperandKey
	 */
	public DataItemCobolIdentifier getDynamicOperandIdentifier(String idOperandKey) {
		DataItemCobolIdentifier dynamicOperandIdentifier = null;
		
		dynamicOperandIdentifier = (DataItemCobolIdentifier) this.getMapDescriptorObject("$OPRND-DYN-KEY$" + idOperandKey);
		
		return dynamicOperandIdentifier;
	}


	/**
	 * Restituisce un array list con i valori individuati per il campo.<br>
	 * <p>
	 * Viene richiesto l'id chiave dell'operando dinamico come , per esempio,
	 * MAP, MAPSET, PROGRAM etc. 
	 * 
	 * @return String dynamicOperand
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getDynamicOperandValues(String idOperandKey) {
		ArrayList<String> al_dynamicOperandValue = null;
		al_dynamicOperandValue = (ArrayList<String>) this.getMapDescriptorObject("$OPRND-DYN-VALUES$" + idOperandKey);
		return al_dynamicOperandValue;
	}

	/**
	 * Elimina i valori dinamici di un campo memorizzati nell'istruzione.
	 * <p>
	 * Viene richiesto l'id chiave dell'operando dinamico come , per esempio,
	 * MAP, MAPSET, PROGRAM etc. <br>
	 * Successive richieste di valori restituiranno un array vuoto.<br>
	 * <p>
	 * @param String idOperandKey
	 * 
	 */
	public void delDynamicOperandValues(String idOperandKey) {
		map_Descriptor.put("$OPRND-DYN-VALUES$" + idOperandKey, new ArrayList<String>() );
		return;
	}
 
	/**
	 * Inserisce un array list con i valori di un campo dinamico dell'istruzione<br>
	 * <p>
	 * Viene richiesto l'id chiave dell'operando dinamico come , per esempio,
	 * MAP, MAPSET, PROGRAM etc. 
	 * 
	 * @param String dynamicOperand
	 * @param String ar_dynamicOperandValue[]
	 */
	public void setDynamicOperandValues(String idOperandKey, ArrayList<String> al_dynamicOperandValue) {
		this.addMapDescriptorObject("$OPRND-DYN-VALUES$" + idOperandKey, al_dynamicOperandValue);
		return;
	}

	/**
	 * Restituisce l'identificatore dell'operando dell'istruzione.<br>
	 * <p>
	 * Se l'operando è inesistente restituisce null, se l'dentificatore è una costante,
	 * il nome dell'identificatore è la costante stessa fra apici o doppi apici, così
	 * come trovata nel programma.
	 * 
	 * @return DataItemCobolIdentifier identificatore Cobol operando
	 */
	public DataItemCobolIdentifier getOperand(String idOperand) {
		DataItemCobolIdentifier identifierOperand = null;
		identifierOperand =  (DataItemCobolIdentifier) this.getMapDescriptorObject("$OPRND$" + idOperand);
		return identifierOperand;
	}

	/**
	 * Restituisce true se operando presente nell'istruzione.<br>
	 * <p>
	 * Se l'operando è inesistente restituisce false.
	 * 
	 * @return boolean true if operand name defined
	 */
	public boolean isThereOperand(String idOperand) {
		DataItemCobolIdentifier identifierOperand = null;
		identifierOperand =  (DataItemCobolIdentifier) this.getMapDescriptorObject("$OPRND$" + idOperand);
		if (identifierOperand == null) {
			return false;
		}
		return true;
	}

	
	
	
	
	/**
	 * Inserisce l'identificatore dell'operando dell'istruzione.<br>
	 * Inserisce l'operando nell'elenco operandi di input<br>
	 * <p>
	 * @param String idOperand
	 * @param DataItemCobolIdentifier identifierOperand
	 * 
	 */
	@SuppressWarnings("unchecked")
	public void addOperand(String idOperand, DataItemCobolIdentifier identifierOperand) {
		ArrayList<String> al_inputOperand = null;
		
		this.addMapDescriptorObject("$OPRND$" + idOperand, identifierOperand);
		
		// Inserimento in struttura elenco operandi in input
		if (this.getMapDescriptorObject("$OPRND-INP-LIST$") == null) {
			al_inputOperand = new ArrayList<String>();
			this.addMapDescriptorObject("$OPRND-INP-LIST$", al_inputOperand);
		}
		
		al_inputOperand = (ArrayList<String>) this.getMapDescriptorObject("$OPRND-INP-LIST$");
		
		// Campo già presente: probabile riesecuzione 
		if (al_inputOperand.contains(idOperand)) {
			return;
		}
		al_inputOperand.add(idOperand);
		return;
	}

	/**
	 * Inserisce l'identificatore dell'operando dell'istruzione.<br>
	 * Inserisce l'operando nell'elenco operandi di input<br>
	 * Equivalente a addOperand()
	 * <p>
	 * @param String idOperand
	 * @param DataItemCobolIdentifier identifierOperand
	 */
	public void addOperandInput(String idOperand, DataItemCobolIdentifier identifierOperand) {
		this.addOperand(idOperand, identifierOperand);
		return;
	}

	/**
	 * Inserisce l'identificatore dell'operando dell'istruzione.<br>
	 * Inserisce l'operando nell'elenco operandi di output<br>
	 * <p>
	 * @param String idOperand
	 * @param DataItemCobolIdentifier identifierOperand
	 * 
	 */
	@SuppressWarnings("unchecked")
	public void addOperandOutput(String idOperand, DataItemCobolIdentifier identifierOperand) {
		ArrayList<String> al_outputOperand = null;
		
		this.addMapDescriptorObject("$OPRND$" + idOperand, identifierOperand);
		
		// Inserimento in struttura elenco operandi in input
		if (this.getMapDescriptorObject("$OPRND-OUT-LIST$") == null) {
			al_outputOperand = new ArrayList<String>();
			this.addMapDescriptorObject("$OPRND-OUT-LIST$", al_outputOperand);
		}
		
		al_outputOperand = (ArrayList<String>) this.getMapDescriptorObject("$OPRND-OUT-LIST$");
		
		// Campo già presente: probabile riesecuzione 
		if (al_outputOperand.contains(idOperand)) {
			return;
		}
		al_outputOperand.add(idOperand);
		return;
	}


	/**
	 * Inserisce l'opzione dell'istruzione.<br>
	 * <p>
	 * 
	 * @param String option
	 */
	@SuppressWarnings("unchecked")
	public void addOption(String option) {
		
		ArrayList<String> al_option = null;

		// Inserimento operando normalmente
		this.addMapDescriptorObject("$OPT$" + option, "");
		
		// Inserimento in struttura elenco opzioni istruzione
		if (this.getMapDescriptorObject("$OPT-LIST$") == null) {
			al_option = new ArrayList<String>();
			this.addMapDescriptorObject("$OPT-LIST$", al_option);
		}
		al_option = (ArrayList<String>) this.getMapDescriptorObject("$OPT-LIST$");
		
		// Opzione già presente: skip
		if (al_option.contains(option)) {
			return;
		}
		al_option.add(option);
		return;
	}

	/**
	 * Restituisce true se l'opzione è presente nell'istruzione.<br>
	 * <p>
	 * 
	 * @param String option
	 * @return Boolean true se l'opzione è presente
	 */
	public boolean isThereOption(String option) {
		if (this.getMapDescriptorObject("$OPT$" + option) == null) {
			return false;
		}
		return true;
	}
	

	/**
	 * Imposta il token in errore.<br>
	 * <p>
	 * @param tokenInError the tokenInError to set
	 */
	public void setTokenInError(String tokenInError) {
		this.tokenInError = tokenInError;
	}

	
	
	
	/**
	 * Restituisce le violazioni effettuate dall'istruzione.<br>
	 * <p>
	 * @return the al_metricViolation
	 */
	public ArrayList<EnumMetricsViolation> getMetricViolations() {
		return al_metricViolation;
	}

	/**
	 * Imposta le violazioni effettuate dall'istruzione.<br>
	 * <p>
	 * @param al_metricViolation the al_metricViolation to set
	 */
	public void setMetricViolations(ArrayList<EnumMetricsViolation> al_metricViolation) {
		this.al_metricViolation = al_metricViolation;
	}

	
	/**
	 * Imposta il tipo di processo attivo al momento dell'errore sull'istruzione.<br>
	 * <p>
	 * @param EnumTypeProcessAnalysis typeProcessAnalysis
	 */
	public void setTypeProcessAnalysis(EnumTypeProcessAnalysis typeProcessAnalysis) {
		this.addMapDescriptorObject("$TYPE-PROCESS$", typeProcessAnalysis);
		return;
	}

	/**
	 * Restituisce il tipo di processo attivo al momento dell'errore sull'istruzione.<br>
	 * <p>
	 * @return EnumTypeProcessAnalysis typeProcessAnalysis
	 */
	public EnumTypeProcessAnalysis getTypeProcessAnalysis() {
		EnumTypeProcessAnalysis typeTypeProcessAnalysis = null;
		
		typeTypeProcessAnalysis = (EnumTypeProcessAnalysis) this.getMapDescriptorObject("$TYPE-PROCESS$");
		if (typeTypeProcessAnalysis == null) {
			typeTypeProcessAnalysis = EnumTypeProcessAnalysis.NOT_ASSIGNED;
		}
		return typeTypeProcessAnalysis;
	}

	
	/**
	 * Imposta il tipo istruzione ovvero Cobol nativa, Sql precompiler, etc..<br>
	 * <p>
	 * @param EnumInstrDataCategory typeInstr
	 */
	public void setTypeInstrCategory(EnumInstrDataCategory typeInstr) {
		this.addMapDescriptorObject("$TYPE-INSTR$", typeInstr);
		return;
	}

	/**
	 * Restituisce il tipo istruzione ovvero Cobol nativa, Sql precompiler, etc..<br>
	 * <p>
	 * @return EnumInstrDataCategory typeInstr
	 */
	public EnumInstrDataCategory getTypeInstrCategory() {
		EnumInstrDataCategory typeInstr = null;
		
		typeInstr = (EnumInstrDataCategory) this.getMapDescriptorObject("$TYPE-INSTR$");
		if (typeInstr == null) {
			typeInstr = EnumInstrDataCategory.NOT_ASSIGNED;
		}
		return typeInstr;
	}

	
	/**
	 * Imposta il numero di riga sorgente in errore.<br>
	 * <p>
	 * @param int rowNumError
	 */
	public void setRowNumError(int rowNumError) {
		this.addMapDescriptorObject("$ROW-NUM$", rowNumError);
		return;
	}

	/**
	 * Restituisce il numero di riga sorgente in errore dell'istruzione.<br>
	 * <p>
	 * @return int numeRow
	 */
	public int getRowNumError() {
		int rowNumError = 0;
		
		if (this.getMapDescriptorObject("$ROW-NUM$") == null) {
			return 0;
		}
		rowNumError = (Integer) this.getMapDescriptorObject("$ROW-NUM$");
		return rowNumError;
	}

	/**
	 * Imposta il numero codificato di istruzione in errore<br>
	 * <p>
	 * @param int instrNumError
	 */
	public void setInstrNumError(int instrNumError) {
		this.addMapDescriptorObject("$INSTR-NUM$", instrNumError);
		return;
	}

	/**
	 * Restituisce il numero codificato di istruzione in errore<br>
	 * <p>
	 * @return int instrNumError
	 */
	public int getInstrNumError() {
		int instrNumError = 0;
		
		if (this.getMapDescriptorObject("$INSTR-NUM$") == null) {
			return 0;
		}
		instrNumError = (Integer) this.getMapDescriptorObject("$INSTR-NUM$");
		return instrNumError;
	}

	/**
	 * 
	 * Restituisce il codice dell'istruzione
	 * 
	 * @return the typeInstr
	 */
	public EnumCobolReservedWords getTypeInstr() {
		return typeInstr;
	}

	/**
	 * @param typeInstr the typeInstr to set
	 */
	public void setTypeInstr(EnumCobolReservedWords typeInstr) {
		this.typeInstr = typeInstr;
	}


	/**
	 * @return the typeInstrPrecompiler
	 */
	public EnumPrecompilerReservedWords getTypeInstrPrecompiler() {
		return typeInstrPrecompiler;
	}


	/**
	 * @return the typeInstrJcl
	 */
	public EnumInstrDataCategory getTypeInstrJcl() {
		return typeInstrJcl;
	}

	/**
	 * @param typeInstrJcl the typeInstrJcl to set
	 */
	public void setTypeInstrJcl(EnumInstrDataCategory typeInstrJcl) {
		this.typeInstrJcl = typeInstrJcl;
	}

	/**
	 * @param typeInstrPrecompiler the typeInstrPrecompiler to set
	 */
	public void setTypeInstrPrecompiler(
			EnumPrecompilerReservedWords typeInstrPrecompiler) {
		this.typeInstrPrecompiler = typeInstrPrecompiler;
	}


	
	
	/**
	 * Restituisce il tipo di precompilatore.<btìr>
	 * <p>
	 * @return the typePrecompiler
	 */
	public EnumInstrDataCategory getTypePrecompiler() {
		return typePrecompiler;
	}


	/**
	 * Imposta il tipo di istruzione.<br>
	 * <p>
	 * @param typePrecompiler the typePrecompiler to set
	 */
	public void setTypePrecompiler(EnumInstrDataCategory typePrecompiler) {
		this.typePrecompiler = typePrecompiler;
	}

	/**
	 * Restituisce il tipo di istruzione.<br>
	 * <p>
	 * @param typeInstr the typeInstr to set
	 */
	public void setTypeInstr(EnumPrecompilerReservedWords typeInstrPrecompiler) {
		this.typeInstrPrecompiler = typeInstrPrecompiler;
	}


	
	
	
	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected Object clone() {
		try {
			return super.clone();
		} catch (Exception e) {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return    " Source:"    + sourceInstr 
				+ " Num:"       + numInstr       + " Name:"   + name 
				+ " RowStart:"  + rowStartSource + " RowEnd:" + rowEndSource 
				+ " PosStart:"  + posStartInstr  + " PosEnd:" + posEndInstr ;
	}

	


	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	

	/*
	 *   Classe contenitore di servizio con la descrizione di una coppia di elementi  
	 *   simbolo/qualificatore di utilizzo. se il simbolo è utilizzato direttamente
	 *   senza qualificazione, il qualifier rimane impostato a null;
	 *   
	 */
	public class InnerSymbolEntry implements Serializable {
		
		private static final long serialVersionUID = 1L;

		String symbolName = "";                         // Nome simbolo
		DataItemQualifier qualifier = null;             // Qualificazione di utilizzo
	
		/*
		 * 
		 * Costruttore
		 * 
		 */
		private InnerSymbolEntry(String symbolName, DataItemQualifier qualifier) {
			super();
			this.symbolName = symbolName;
			this.qualifier = qualifier;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return this.symbolName.toString();
		}
		
		
	}


	
	
}
