package analyzer;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

import utilities.StringService;
import analyzer.LogicDynamicField;
import analyzer.LogicDynamicFieldSub;
import analyzer.LogicDynamicFieldSubSetting;
import analyzer.LogicDynamicValue;
import entities.EntityDynamicField;
import entities.EntityDynamicFieldSub;
import enums.EnumCobolReservedWords;
import enums.EnumDataItemType;
import enums.EnumLogicSetMode;
import enums.EnumMessageType;
import enums.EnumObject;
import enums.EnumPrecompilerReservedWords;
import exception.ExceptionAmrita;


/**
 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * LogicSamePgm
 * </h1>
 * <p>
 * Vengono gestite tutte le operazioni relative alle logiche dinamiche di programma. <br>
 * <p>
 * Viene fornito l'oggetto {@link ProgramCobol} su cui operare, il numero di istruzione dove si trovano
 * i campi dei quali si vuole conoscere il valore dinamico e i nomi del campo o dei campi dei quali
 * si vuole conoscere i valori dinamici.<br>
 * In {@link ProgramCobol} è gestito il reference all'oggetto {@link _LogicInfoDynamic}. Tale oggetto contiene
 * tutte le informaszioni, in modo esaustivo, delle istruzioni dinamiche del programma, dei campi da risolvere,
 * dello stato di soluzione oltre al dettaglio delle catene di trasformazione, che portano all'ultima (la prima)
 * assegnazione attraverso la quale si determinano i valori dinamici del sottocampo. <br>
 * Attraverso la rappresentazione del grafo di programma, gestita dall'oggetto {@link GraphManager}, vengono
 * generati tutti i path fra l'istruzione fornita e l'inizio del programma.
 * Quindi composti tutti i valori possibili, eventualmente attivando ricorsivamente la ricerca a programmi
 * chiamati con Call.<br>
 * Vengono aggiornate le strutture di aggiornamento db {@link AnalyzerDbInfo} e il descrittore delle istruzioni
 * dinamiche {@link EntityDynamicField}, che viene serializzato insieme al programma.<br> 
 * Ciò viene fatto nel caso di un campo, o di un suo sottocampo, non si sia riusciti a trovare i valori, 
 * rimanendo unresolved. Nella stessa struttura vengono inseriti le righe per resolved.<br>
 * <p>
 * Un campo può restare unresolved se l'ultima assegnazione è :<br>
 * 1) Un campo di Using<br>
 * 2) Un campo di Linkage<br>
 * 3) Un campo di sistema Cics com EIBTRMID o altror><br>
 * 4) Una lettura da un archivio esterno come un file, tabella db, coda di temporary Storage<br>
 *<p>
 * L'ultima assegnazione, che in pratica è la prima assegnazione 
 * utile, individua i possibili valori del sottocampo nella catena
 * di assegnazioni.<br>
 * <p>
 * Possiamo avere i seguenti casi in base all'ultima istruzione:
 * <p>
 * <Ul>
 * <Li> <b>MOVE campo di Working Storage</B> <br>
 * Se il campo di partenza ha un valore iniziale, il valore espresso dalla
 * clausola value viene memorizzato come valore possibile.
 * 
 * <Li> <b>MOVE literal</B> <br>
 * Se il valore di partenza è una literal alfanumerica, il valore
 * viene memorizzato come valore possibile.
 * 
 * <Li> <b>MOVE elemento di tabella</B> <br>
 * Il campo di partenza è un campo di un elemento di tabella dichiarata
 * con occurs anche a più livelli. Il qualificatore del campo di partenza 
 * indica quindi uno o più indici. Viene analizzata la tabella per individuare
 * l'area che viene ridefinita contenente gli elementi di tabella.<br>
 * Viene quindi ricostruita una unica stringa con il contenuto completo della tabella.<br>
 * Vengono quindi estratti i valori dell'elemento di ogni riga della tabella
 * e memorizzati come valori possibili.
 * 
 * <Li> <b>MOVE campo di Procedure Division Using</B> <br>
 * Il processo si ferma nel caso di ricerca dei valori nello stesso programma.<br>
 * In caso di ricerca di valori <b>spreaded</b> fra diversi programmi, la ricerca continua
 * nei programmi chiamanti, ricorsivamente, memorizzando i valori individuati.
 * 
 * <Li> <b>MOVE campo di Linkage Section</B> <br>
 * Non essendo un campo di Using, sempre definito in Linkage Section, in questo caso l'area
 * di Linkage, di livello 01, dove si trova il campo, deve essere indirizzata con una
 * istruzione Set Address Of Area To Pointer. <br>
 * Una volta individuato il pointer si cerca di individuare la catena di programmi chiamanti 
 * che hanno valorizzato l'area di Linkage  attraverso il pointer.<br>
 * Successivamente il processo di valorizzazione è sempre lo stesso. 
 * 
 * <Li> <b>MOVE campo Cics EIBTRMID</B> <br>
 * Il campo è di sistema Cics e non può che essere valorizzato esternamente.<br>
 * La Move può fare riferimento a EIBTRMID oppure a una sua parte, via reference modification,
 * con posizione e lunghezza. Vengono cercati i valori nella tabella esterna DVAE (DynamicValueExternal)
 * e se si trovano vengono memorizzati, altrimenti viene inserita la richiesta di valori esterni. 
 * 
 * <Li> <b>MOVE campo Cics EIBTRNID</B> <br>
 * Il campo è di sistema Cics e non può che essere valorizzato esternamente.<br>
 * La Move può fare riferimento a EIBTRNID oppure a una sua parte, via reference modification,
 * con posizione e lunghezza. Vengono cercati i valori nella tabella esterna DVAE (DynamicValueExternal)
 * e se si trovano vengono memorizzati, altrimenti viene inserita la richiesta di valori esterni. 
 
 * <Li> <b>MOVE campo Cics di Twa</B> <br>
 * Da definire la gestione.
 * 
 * <Li> <b>MOVE campo Cics di Csa</B> <br>
 * Da definire la gestione.
 * 
 * <Li> <b>Read File</B> <br>
 * L'ultima assegnazione ha portato a una istruzione di lettura che aggiorna il
 * contenuto dell'ultimo campo della catena di trasformazione del dato elementare.<br>
 * Viene calcolata la posizione del campo nel tracciato del file.
 * Vengono cercati i valori nella tabella esterna DVAE (DynamicValueExternal)
 * e se si trovano vengono memorizzati, altrimenti viene inserita la richiesta di valori esterni. 
 * 
 * <Li> <b>Sql Select/Fetch</B> <br>
 * L'ultima assegnazione ha portato a una istruzione Sql di lettura che aggiorna il
 * contenuto dell'ultimo campo della catena di trasformazione del dato elementare.<br>
 * Viene individuato il nome della tabella Sql e della colonna.
 * Vengono cercati i valori nella tabella esterna DVAE (DynamicValueExternal)
 * e se si trovano vengono memorizzati, altrimenti viene inserita la richiesta di valori esterni. 
 * 
 * <Li> <b>Exec Cics Read/ReadNext/ReadPrev</B> <br>
 * L'ultima assegnazione ha portato a una istruzione di lettura Cics di un Vsam che aggiorna il
 * contenuto dell'ultimo campo della catena di trasformazione del dato elementare.<br>
 * Viene calcolata la posizione del campo nel tracciato del file.
 * Vengono cercati i valori nella tabella esterna DVAE (DynamicValueExternal)
 * e se si trovano vengono memorizzati, altrimenti viene inserita la richiesta di valori esterni. 
 * 
 * <Li> <b>Exec Cics Readq Ts/Td</B> <br>
 * L'ultima assegnazione ha portato a una istruzione di lettura Cics di una coda che aggiorna il
 * contenuto dell'ultimo campo della catena di trasformazione del dato elementare.<br>
 * Viene calcolata la posizione del campo nel tracciato del file.
 * Vengono cercati i valori nella tabella esterna DVAE (DynamicValueExternal)
 * e se si trovano vengono memorizzati, altrimenti viene inserita la richiesta di valori esterni. 
 * 
 * <Li> <b>Exec Cics Receive Map</B> <br>
 * L'ultima assegnazione ha portato a una istruzione di lettura Cics da terminale.
 * Individuare i valori possibili diventa una complessa funzione in grado di seguire
 * dove il dato letto viene memorizzato e recuperato da media esterni.
 * 
 * </Ul>
 *
 * <h2>
 * Creazione catene di trasformazioni elementari con attivazione ricorsiva.<br>
 * </h2>
 * <p>
 * Riassumendo la trasformazione può individuare, come ultima assegnazione:<br>
 * <p>
 * 1) una literal<br>
 * 2) Un campo che non è referenziato in output e ha un valore iniziale<br>
 * 3) Un campo indicizzato che rappresenta un elemento di tabella Occurs<br>
 * 4) Un campo non risolvibile nello stesso programma in quanto campo di:<br>
 * 4.1) Using parm di una Call<br>
 * 4.2) Using parm di programma<br>
 * 4.3) Linkage Section<br>
 * 4.4) Valorizzato da istruzione di Read Cobol<br>
 * 4.5) Sistema Cics come EIBTRMID, EIBTRNID<br>
 * 4.6) Valorizzato da istruzione di Read/Readnext/.. Cics (Vsam, Ts, Td)<br> 
 * 4.7) Valorizzato da istruzione di Sql Select<br> 
 * <p>
 * <h2>
 * Assegnazioni multiple stesso campo
 * </h2>
 * <p>
 * La prima operazione effettuata, è quella di individuare tutte le assegnazioni
 * che valorizzano in output il campo receiver fornito, per ottenere il campo sender.<br>
 * Ogni assegnazione sullo stesso campo, genera ricorsivamente una catena di 
 * trasformazioni diversa. <br>
 * Dal momento che in questa fase non si tiene conto dei path di esecuzione effettivi, 
 * si possono generare delle catene di trasformazione  inconsistenti, 
 * ovvero è possibile che nei path di esecuzione, validi per la catena di trasformazione, 
 * ci siano assegnazioni successive dello stesso campo. <br>
 * Quindi, nel caso di assegnazioni multiple, vengono memorizzati nella struttura
 * di trasformazione subFieldSetting, tutti gli archi relativi alle altre istruzioni
 * di assegnazione, in una ArrayList.<br>
 * Al momento della verifica della copertura delle catene di esecuzione nei paths,
 * si dovrà verificare, per ogni trasformazione, che in nessun path, ci siano istruzioni 
 * di assegnazione SUCCESSIVE nei path in esame.
 * 
 * <p>
 * <h2>
 * Call fra due assegnazioni
 * </h2>
 * <p>
 * E' possibile che fra due assegnazioni, per esempio due Move, dove la prima aggiorna
 * un campo e la seconda utilizza quel campo in input, ci siano una o più statemets Call,
 * che utilizzano direttamente o indirettamente quel campo, che potrebbe pertanto essere
 * potenzialmente aggiornato dalla catena di programmi chiamati.<br>
 * Il campo viene dichiarato esplicitamente come parametro nella using oppure è definito 
 * sotto qualche gruppo di uno dei parametri. <br>
 * E' necessario seguire le trasformazioni nella catena di programmi chiamati. 
 * Tale catena potrebbe contenere valorizzazioni via Read, Exec Cics Read ... o altro.<br>
 * L'informazione di Call potenzialmente influente ai fini dell'ultima impostazione,
 * viene memorizzata nella struttura di ultima assegnazione successiva alla Call.<br>
 * Viene quindi generata una catena di trasformazione che inizia con la Call in
 * questione, come lastSet. <br>
 * L'attivazione di LogicManager in modalità "spreaded"
 * tenterà di risolvere nel programma richiasmato con Call, e nella sua catena di
 * chiamati. Se non ci sono valorizzazioni valide nella catena dei programma
 * chiamati, non è da considerarsi un errore: semplicemente il programmi chiamati
 * non aggiornano il campo in questione.
 * 
 * 
 * <p>
 * <h2>
 * Ricorsività
 * </h2>
 * <p>
 * Questo metodo viene attivato ricorsivamente solo nel caso di assegnazioni multiple
 * (xref) del campo receiver. <br>
 * In questo caso viene creata una nuova catena di trasformazioni indipendente, 
 * per ogni assegnazione individuata. <br>
 *
 *
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 31/05/2010
 * @see AnalyzerCobolProgram
 * @see Instruction
 * 
*/

public class LogicSamePgm extends ExecutionShared implements AmritaConstants {
	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza generali                                  //                                                        //
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// Struttura di controllo che può essere restituita al chiamante
	LogicWorkProcess ilpw = null;
	
	// E' il descrittore del programma nel quale le logiche devono essere risolte
	private ProgramCobol program = null;

	// Descrittore campo originario di cui trovare i valori
	// Contiene tutte le informazioni su db/sottocampi/assegnazioni/etc
 	private LogicDynamicField dynamicFieldToSolve = null;  									
		
	// Classe di supporto alle logiche, paths di esecuzione etc
	private LogicTools logicTools = null;

	// Reference al contenitore con info dinamiche.
	private LogicInfoDynamic logicInfoDynamic = null;

    // Gestore centralizzato aggiornamenti db cumulativi.
    // Impostato da ProcessSystemLevel all'attivazione.
    private AnalyzerDbInfo analyzerDbInfo = null;
   
	
	// Programma origine delle logiche da risolvere 
	@SuppressWarnings("unused")
	private String programName = "";
	@SuppressWarnings("unused")
	private EnumObject typeProgram = null;
	
	
	/**
	 * Costruttore  
	 */
	public LogicSamePgm(UserConfiguration sd, ExecutionDirectives di, ProgramCobol program) {
		super(sd, di);
		this.program = program;
		this.logicInfoDynamic = program.getLogicInfoDynamic();
		this.logicTools = new LogicTools(program, true, ucfg);
	    this.typeProgram = program.programType;
	    this.programName = program.programName;
	} 
	
	
	
	
	/**
	 * 
	 * Restituisce il descrittore del programma contenente le istruzioni da risolvere.
	 * 
	 * @return the program
	 */
	public ProgramCobol getProgram() {
		return program;
	}




	/**
	 * 
	 * Imposta il descrittore del programma contenente le istruzioni da risolvere.
	 * 
	 * 
	 * @param program the program to set
	 */
	public void setProgram(ProgramCobol program) {
		this.program = program;
	}





	/**
	 * 
	 * Imposta il nome del programma origine con le istruzioni
	 * dinamiche da risolvere.
	 * 
	 * @param programName the programName to set
	 */
	public void setProgramName(String programName) {
		this.programName = programName;
	}



	/**
	 * 
	 * Imposta il tipo di oggetto programma origine con le istruzioni
	 * dinamiche da risolvere.
	 * 
	 * @param typeProgram the typeProgram to set
	 */
	public void setTypeProgram(EnumObject typeProgram) {
		this.typeProgram = typeProgram;
	}


	/**
	 * Restituisce oggetto con informazioni dinamiche associate al programma
	 * 
	 * @return the logicInfoDynamic
	 */
	public LogicInfoDynamic getLogicInfoDynamic() {
		return logicInfoDynamic;
	}




	/**
	 * Imposta oggetto con informazioni dinamiche associate al programma

	 * @param logicInfoDynamic the logicInfoDynamic to set
	 */
	public void setLogicInfoDynamic(LogicInfoDynamic logicInfoDynamic) {
		this.logicInfoDynamic = logicInfoDynamic;
	}

	/**
	 * Restituisce oggetto con informazioni X aggiornamenti finali su db
	 * 
	 * @return the analyzerDbInfo
	 */
	public AnalyzerDbInfo getAnalyzerDbInfo() {
		return analyzerDbInfo;
	}

	/**
	 * 	Imposta oggetto con informazioni X aggiornamenti finali su db
	 * 
	 * @param analyzerDbInfo the analyzerDbInfo to set
	 */
	public void setAnalyzerDbInfo(AnalyzerDbInfo analyzerDbInfo) {
		this.analyzerDbInfo = analyzerDbInfo;
	}



	/**
	 * -----------------------------------------------------------------------------------  <br>
	 * Restituisce i valori dinamici di un campo, generati nel solo programma corrente o  <br>
	 * attraverso valori esterni caricati nella tabella DynamicValueExternal. <br>
	 * -----------------------------------------------------------------------------------  <br>
	 *  <p>
	 * Viene effettuata l'analisi dinamica del programma fornito in input, senza estendere
	 * la ricerca ricorsivamente ai programmi chiamanti e chiamati con Call nei path individuati.<br>
	 * <p>
	 * La ricerca viene effettuata a partire dall'istruzione corrente tornando indietro fino alla prima
	 * istruzione del programma (Procedure Division), attraverso tutti i possibili path di esecuzione.<br>
	 * vIENE fornitO in input 1 identificatori Cobol (campo) di cui si individuano i valori
	 * assunti nello STESSO path di esecuzione.<br>
	 * l'identificatore, istanza di DataItemCobolIdentifier modella l'istruzione di definizione del dato 
	 * in Data Division e l'eventuale identificatore con reference modification, pos, index etc.
	 * <p>
	 * Si restituisce in output un ArrayList di stringhe con i valori individuati.<br>
	 * <p>
	 * I valori restituiti sono ordinati e già epurati dai duplicati.<br>
	 * A cura del chiamante il trattamento di questi valori che, se programmi, possono essere già stati censiti o meno.
	 * In caso non siano stati censiti è possibile che i valori individuati rappresentino una combinazione di valori
	 * impossibile e vadano pertanto scartati (caso di campi di gruppo con singoli valori di sottocampi individuati).
	 * 
	 * 
	 * @param ProgramCobol programOrigin	 Descrittore programma origine
	 * @param ProgramCobol programCur	 Descrittore programma corrente
	 * @param Instruction instr 	Istruzione dinamica da risolvere	
	 * @param DataItemCobolIdentifier DataItem  di cui trovare i valori
	 * @return ArrayList<String> valori trovati per il campo da inizio programma fino a istruzione target
	 * @throws ExceptionAmrita 
	 * @throws SQLException 
	 */
	public ArrayList<String> dynamicValues(
										   ProgramCobol programOrigin				 // Descrittore programma dove risolvere il data item dell'istruzione			
									     , Instruction instr 						 // Istruzione dinamica da risolvere		
									     , DataItemCobolIdentifier dataItemToSolve   // Operando di cui trovare i valori (campo elementare o di gruppo)
										  ) throws ExceptionAmrita, SQLException {
				
		
		LogicWorkProcess ilpw = null;				 					// Informazioni e reference per il completo controllo del processo normale e ricorsivo
		int posInSubField  = 0;                      					// Posizione in sottocampo origine di cui trovare i valori
		int lngInSubField  = 0;                     					// Lunghezza in sottocampo origine di cui trovare i valori
		
		posInSubField = 1;	                         					// TODO verificare utilizzo in caso di sottocampo di gruppo	
		lngInSubField = dataItemToSolve.getDataItem().getSizeBytes();	// TODO verificare utilizzo in caso di sottocampo di gruppo
		ilpw = LogicTools.dynamicValuesInitial(programOrigin, instr, dataItemToSolve);
		ilpw.ucfg = this.ucfg;                                          // Per accesso dati in metodi statici di LogicTools
		ilpw.isDynamicInstrSolving = true;           					// Attivazione per soluzione istruzione dinamica target
		ilpw.isExecLogicSpreaded = false;            					// Attivazione a fronte di istruzione dinamica in programma origine
		this.ilpw = ilpw;                                               // 
		
		// Valori in ilpw.al_valuesField;
		dynamicValuesExtractor(ilpw, programOrigin, instr, dataItemToSolve, posInSubField, lngInSubField);
				
		return ilpw.al_valuesField;
	} 	
	
	/* ---------------------------------------------------------------
	 * Individuazione valori data item fornito
	 * ---------------------------------------------------------------
	 * 
	 * ilpw.al_valuesField viene valorizzato con gli eventuali valori trovati
	 */
	public ArrayList<String> dynamicValuesExtractor(
												  LogicWorkProcess ilpw                      // Area di controllo già inizializzata
												, ProgramCobol program				 		 // Descrittore programma dove risolvere il data item dell'istruzione			
												, Instruction instr 						 // Istruzione dinamica da risolvere		
												, DataItemCobolIdentifier dataItemToSolve    // Operando di cui trovare i valori (campo elementare o di gruppo)
												, int posInSubField                          // Posizione in sottocampo origine di cui trovare i valori
												, int lngInSubField                          // Lunghezza in sottocampo origine di cui trovare i valori
												) throws ExceptionAmrita, SQLException {


		// Individuazione di tutti i possibili path di esecuzione 
		// I path partono da istruzione 0 Procedure a istruzione dinamica target
		// Le perform nei path sono esplose ricorsivamente 
		ilpw.arPaths = logicTools.paths(program, 0, ilpw.instrDynamicOrigin.getNumInstr(), LogicTools.EXPAND_PERFORM_DETAIL_ANY_LEVEL);

		// Espansione di dettaglio paths
		// Solo Se istruzione di partenza NON dentro section/paragrafo dead code
		if (!(ilpw.arPaths.length == 1 && ilpw.arPaths[0].getEntries().length == 1)) {
			ilpw.arPaths = logicTools.pathsExpand(program, ilpw.arPaths, LogicTools.EXPAND_PERFORM_DETAIL_ANY_LEVEL);
		}

		// Codifica campo di cui trovare i valori
		ilpw.dynamicFieldToSolve = setDynamicFieldToSolve(ilpw);
		ilpw.dataItemFieldToSolve = dataItemToSolve;
		this.dynamicFieldToSolve = ilpw.dynamicFieldToSolve;    // TODO forse inutile

		// Individuazione sottocampi del campo di cui trovare i valori.
		// La ricerca valori è sempre fatta sui sottocampi.
		// A fine processo i valori vengono ricomposti.
		// Aggiornamento sottocampi in classe campo da risolvere
		ilpw.arDataItemFieldSubToSolve = LogicTools.getSubFields(program, dataItemToSolve);	
		ilpw.dynamicFieldToSolve.al_FieldSub = setDynamicFieldSubToSolve(ilpw);

		// Istruzione di partenza dentro section/paragrafo dead code
		// Struttura subfield comunque generata
		// Si restituisce array list di valori vuoto.
		if (ilpw.arPaths.length == 1 && ilpw.arPaths[0].getEntries().length == 1) {
			return ilpw.al_valuesField;
		}

		// Individuazione esaustiva di tutte le catene di trasformazione per tutti i sottocampi
		// Le catene di trasformazione sono memorizzate a livello di sottocampo in LogicDynamicFieldSub
		ilpw.dynamicFieldName = ilpw.dataItemFieldToSolve.getDataItem().getDataName();

		// Scan SOTTOCAMPI elementari campo (presente anche in caso di campo non di gruppo)
		// Individuazione catene di trasformazione con risultato già in ilpw.dynamicFieldToSolve.al_SubField(n)
		for (LogicDynamicFieldSub dynamicFieldSub : ilpw.dynamicFieldToSolve.al_FieldSub) {

			ilpw.dynamicFieldSubToSolve = dynamicFieldSub;          								     // Per comodità dynamicFieldSubOrigin punta al sottocampo origine dentro il campo origine
			ilpw.posRcv = 1;																			 // A partire dalla prima posizione nel campo									
			ilpw.lngRcv = dynamicFieldSub.dataItemIdentifierSubField.getDataItem().getSizeBytes();		 //   Per la lunghezza massima del campo in bytes
			ilpw.posInSubField = 1;                                                             		 // Mappato da posizione iniziale interessata nel sottocampo
			ilpw.lngInSubField = dynamicFieldSub.dataItemIdentifierSubField.getDataItem().getSizeBytes();//   Per lunghezza iniziale interessata nel sottocampo

			// Catene individuate in dynamicFieldSub.al_al_chainSetSubField 
			logicTools.chainSet(ilpw, dynamicFieldSub, 0, instr.getNumInstr());			

		} // end-for sottocampi

		// Controlli validità per estrazione valori di tutti i SOTTOCAMPI campo in input
		// Supportati da un path da inizio pgm a istruzione target
		checkChainsSetCoveredByPaths(ilpw); 					

		// Generazione valori campp in input come Composizione valori sottocampi 
		LogicTools.generateFieldValuesBySubFieldsComposition(ilpw);
		
		// Inserimento valori completi relativi al campo in struttura dinamica se necessario
		LogicTools.insertFullFieldValues(ilpw, ilpw.al_valuesField);
		
		// I flag istruzione sono aggiornati dal chiamante attraverso LogicInfoDynamic.putDatabaseInfoAll()
		return ilpw.al_valuesField;
	} 
	
	
	
    /*
     *  Impostazione struttura sottocampoi dinamici da risolvere, nella struttura del campo dinamico 
     */
	private ArrayList<LogicDynamicFieldSub> setDynamicFieldSubToSolve(LogicWorkProcess ilpw) {
		EntityDynamicFieldSub entityDynamicFieldSub = null;
		LogicDynamicFieldSub logicDynamicFieldSub = null;
		
		// Scan sottocampi da risolvere
		for (DataItemCobolIdentifier dataItemFieldSubToSolve : ilpw.arDataItemFieldSubToSolve) {
			logicDynamicFieldSub = new LogicDynamicFieldSub(ilpw.programCur.getSysOwner(), ilpw.programCur.getSubSysOwner());
			
			// Struttura db
			entityDynamicFieldSub = logicDynamicFieldSub.entityDynamicFieldSub;
			entityDynamicFieldSub.setSystem(ilpw.programCur.getSysOwner());
			entityDynamicFieldSub.setSubSystem(ilpw.programCur.getSubSysOwner());
			entityDynamicFieldSub.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
			entityDynamicFieldSub.setIdObject(ilpw.programCur.getProgramName());
			entityDynamicFieldSub.setIdField(ilpw.dataItemFieldToSolve.getDataItem().getDataName());
			entityDynamicFieldSub.setNumField(ilpw.dataItemFieldToSolve.getDataItem().getNumInstr());  
			entityDynamicFieldSub.setNumInstr(ilpw.instrDynamicOrigin.getNumInstr()); 
			entityDynamicFieldSub.setIdSubField("");
			if (!ilpw.dataItemFieldToSolve.getDataItem().getDataName().equals(dataItemFieldSubToSolve.getDataItem().getDataName())) {
				entityDynamicFieldSub.setIdSubField(dataItemFieldSubToSolve.getDataItem().getDataName());        
			}
			entityDynamicFieldSub.setNumSubField(dataItemFieldSubToSolve.getDataItem().getNumInstr());
			entityDynamicFieldSub.setPosSubField(1);
			entityDynamicFieldSub.setSizeSubField(dataItemFieldSubToSolve.getDataItem().getSizeBytes());
			entityDynamicFieldSub.setTypeSubField(EnumDataItemType.COBOL_DISPLAY);			
			entityDynamicFieldSub.setLight(false);
			entityDynamicFieldSub.setSpreaded(false);
			entityDynamicFieldSub.setWaitingForData(false);
			entityDynamicFieldSub.setSolved(false);
			entityDynamicFieldSub.setSpreaded(false);	
			
			// Completamento informazioni sottocampo dinamico
			logicDynamicFieldSub.entityDynamicFieldSub = entityDynamicFieldSub;			
			logicDynamicFieldSub.dataItemIdentifierSubField = dataItemFieldSubToSolve;
			logicDynamicFieldSub.valueDefault = "";
			
			// Accodamento a sottocampi da trattare
			ilpw.al_DynamicFieldSubWrk.add(logicDynamicFieldSub);
		}
		
		return ilpw.al_DynamicFieldSubWrk;    // Solo per chiarezza
	}



    /*
     *  Impostazione struttura campo dinamico da risolvere.
     *  Valorizzzione EntityDynamicField per eventuali aggiornamenti finali su db.
     */
	private LogicDynamicField setDynamicFieldToSolve(LogicWorkProcess ilpw) {
		LogicDynamicField logicDynamicField = null;
		
		logicDynamicField = new LogicDynamicField(ilpw.programCur.getSysOwner(), program.getSubSysOwner());
		 
		logicDynamicField.entityDynamicField.setSystem(ilpw.programCur.getSysOwner());
		logicDynamicField.entityDynamicField.setSubSystem(ilpw.programCur.getSubSysOwner());
		logicDynamicField.entityDynamicField.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
		logicDynamicField.entityDynamicField.setIdObject(ilpw.programCur.getProgramName());
		logicDynamicField.entityDynamicField.setIdField(ilpw.dataItemFieldToSolve.getDataItem().getDataName());
		logicDynamicField.entityDynamicField.setNumField(ilpw.dataItemFieldToSolve.getDataItem().getNumInstr());
		logicDynamicField.entityDynamicField.setNumInstr(ilpw.instrDynamicOrigin.getNumInstr());
		logicDynamicField.entityDynamicField.setInstrCobolType(ilpw.instrDynamicOrigin.getTypeInstr());
		logicDynamicField.entityDynamicField.setInstrPrecompOprndType(EnumPrecompilerReservedWords.NOT_ASSIGNED);
		logicDynamicField.entityDynamicField.setInstrPrecompType(ilpw.instrDynamicOrigin.getTypeInstrPrecompiler());		
		logicDynamicField.entityDynamicField.setLight(false);
		logicDynamicField.entityDynamicField.setSpreaded(false);
		logicDynamicField.entityDynamicField.setWaitingForData(false);
		logicDynamicField.entityDynamicField.setSolvedFull(false);
		logicDynamicField.entityDynamicField.setSolved(false);
		logicDynamicField.entityDynamicField.setSpreaded(false);
		
		logicDynamicField.dataItemCobolIdentifier = ilpw.dataItemFieldToSolve;	
		
		return logicDynamicField;
	}



	/* --------------------------------------------------------------------------------
	 * Controlli validità catene di trasformazione per estrazione valori
	 * --------------------------------------------------------------------------------
	 * 
	 * Vengono disabilitate le catene di trasformazione non valide.
	 * Si considerano le catene di trasformazione che generano valori, solo se
	 * compatibili con i possibili path di esecuzione, fino all'istruzione dinamica
	 */
	private void checkChainsSetCoveredByPaths(LogicWorkProcess ilpw) {
		
		// Scan sottocampi da risolvere già processati
		for (LogicDynamicFieldSub logicDynamicFieldSub : ilpw.dynamicFieldToSolve.al_FieldSub) {
             
			// Campo literal alfanumerica: skip
			if (logicDynamicFieldSub.dataItemIdentifierSubField.getIdentifierType() == EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
				continue;
			}

			// Individuazione catene trasformazioni valide da prendere in considerazione
			// Verifica se catene coperte da qualche path
			// Store numeri paths supportati per ogni catena di trasformazione
			// Impostazione flag di copertura/disabilitazione per ogni catena
			markChainSetCoveredByAnyPath(ilpw, logicDynamicFieldSub);  				
			
			ilpw.al_al_chainSetSubField = logicDynamicFieldSub.al_al_chainSetSubField;    					// Catene di trasformazioni elementari valide per il sottocampo 

		    // Campo FILLER, deve avere un value: skip
			if (logicDynamicFieldSub.dataItemIdentifierSubField.getNameIdentifier().toUpperCase().equals("FILLER") 
			&&  logicDynamicFieldSub.dataItemIdentifierSubField.getIdentifierType() == EnumCobolReservedWords.DATA_DIV_DATA_ITEM) {
				continue;
			}			
			
			// Individuazione catene di trasformazione, com relativi valori generati, da disabilitare
		    // A fronte di ulteriori controlli di validita
			
			// Scan CATENE possibili di trasformazione associate al sottocampo.
			for (int j = 0; j < logicDynamicFieldSub.al_al_chainSetSubField.size(); j++) {
				
				ArrayList<LogicDynamicFieldSubSetting> al_chainSet = ilpw.al_al_chainSetSubField.get(j);							// Catena trasformazioni
				
				// Scan catena di trasformazione
				for (LogicDynamicFieldSubSetting set : al_chainSet) {
					set.entityDynamicFieldSetting = set.entityDynamicFieldSetting.clone();
					set.entityDynamicFieldSetting.setNumChain(j);
					set.numChainSet = j;
				}				
				
				// Catena di trasformazioni non coperta da path valido o disabilitata: skip
				if (logicDynamicFieldSub.ar_chainSetFlagPathCovering[j] == false
				|| 	logicDynamicFieldSub.ar_chainSetFlagDisabled[j] == true	) {
					continue;
				}
				
	        	// La catena di trasformazione è valida e stata individuata come supportata da uno o più path.
				
	        	// Verifica che il valore NON sia sovrascritto da assegnazioni SUCCESSIVE incondizionate
				// Flag catena come non valida se necessario  
	        	checkChainSetMarked(ilpw, logicDynamicFieldSub, j);
	        	
			} // end-for catene
			
		}	

	}

	/*
     * Debug current chains
     */
	@SuppressWarnings("unused")
	private void debug(LogicWorkProcess ilpw ) {
		String idFieldSub;
		int numChain = 0;
		int numChains = 0;
		int numChainSets = 0;
		EnumLogicSetMode setMode = null;
		String sender = "";
		String receiver = "";
		int numInstrSet = 0;

		System.out.println("");
		System.out.println("");
		
		// for fieldSub
		for (LogicDynamicFieldSub FieldSubField : ilpw.dynamicFieldToSolve.al_FieldSub) {
			idFieldSub = FieldSubField.entityDynamicFieldSub.getIdSubField();
			numChains = FieldSubField.al_al_chainSetSubField.size();
			// for chain subfield
		    for (ArrayList<LogicDynamicFieldSubSetting> fieldSubChain : FieldSubField.al_al_chainSetSubField) {
		    	numChainSets = fieldSubChain.size();
		    	System.out.println("START CHAIN");
				// chain set
		    	for (LogicDynamicFieldSubSetting fieldSubChainSet : fieldSubChain) {
		    		numChain = fieldSubChainSet.entityDynamicFieldSetting.getNumChain();
					setMode = fieldSubChainSet.entityDynamicFieldSetting.getSetMode();
					sender = fieldSubChainSet.entityDynamicFieldSetting.getFieldSenderId();
					receiver = fieldSubChainSet.entityDynamicFieldSetting.getFieldReceiverId();
					numInstrSet = fieldSubChainSet.entityDynamicFieldSetting.getNumInstrSet();
					System.out.println("Chain:"+numChain+" "+numInstrSet+" "+setMode+" "+sender+" "+receiver+" ");
				}
		    	System.out.println("END CHAIN");
			}
		
		}		
	}
	
	/* --------------------------------------------------------------------------
	 * Generazione valori definitivi CAMPI come moltiplicazione valori sottocampi
	 * --------------------------------------------------------------------------
	 * 
	 * Prima vengono composti i valori assunti da ogni sottocampo.
	 * Poi vengono moltiplicati i valori di tutti i sottocampi.
	 * Vengono eliminati i duplicati e i valori vengono ordinati.
	 * Vengono inoltre aggiornate le strutture del sottocampo con le ultime
	 *   assegnazioni local e spreaded
	 *   
	*/
	private void generateFieldValuesBySubFieldsComposition(LogicWorkProcess ilpw) {

		// Ipotizzo che non tutti i sottocampi siano stati risolti con dei valori
		ilpw.isInstrDynamicSolvedFull = false;
		
		// Allocazione ArrayList di valori per il campo
		ilpw.al_valuesField = new ArrayList<String> ();
		
		//--------------------------------------------------------------------------------
		// 1) Normalizzazione valori di ogni sottocampo
		//--------------------------------------------------------------------------------

		// L'operando è un campo di cui comporre i valori
		// Verifica se qualche sottocampo non è stato risolto
		ilpw.al_DynamicFieldSubWrk = ilpw.dynamicFieldToSolve.al_FieldSub;
		ilpw.allSubFieldsWithValues = true;

		// Scan sottocampi campo dinamico in input
		for (LogicDynamicFieldSub dynamicFieldSub : ilpw.dynamicFieldToSolve.al_FieldSub) {

			ilpw.dynamicFieldSubToSolve = dynamicFieldSub;

			// Scan catene trasformazione sottocampo
			for (int k = 0; k < dynamicFieldSub.al_al_chainSetSubField.size(); k++) {
				// Catena di trasformazione non coperta da path o invalidata: skip
				if (dynamicFieldSub.ar_chainSetFlagPathCovering[k] == false) {continue;}
				if (dynamicFieldSub.ar_chainSetFlagDisabled[k]     == true)  {continue;}

				// Catena di trasformazione valida ma non risolta: specifica ultima assegnazione sottocampo non risolta
				if (dynamicFieldSub.ar_chainSetFlagValues[k] == false) {
					ilpw.isInstrDynamicSolvedFull = false;
				} // end-if

			} // end-for catene trasformazione

			// Valori non generati o generati solo valori parziali 
			// Update dynamicSubField con last set spreaded
			if (!areSubFieldValuesFullCovering(ilpw, dynamicFieldSub)) {
				ilpw.allSubFieldsWithValues = false;
				continue;
			}

			// Valori sottocampo generati: produzione valori stringa effettivi per il sottocampo ordinati e senza duplicati
			ilpw.al_valueSubField = valuesSubFieldBinded(ilpw, dynamicFieldSub);
			dynamicFieldSub.al_valueComplete = ilpw.al_valueSubField;

		} // end-for sottocampi

		//--------------------------------------------------------------------------------
		// 2) Combinazione valori sottocampi per composizione valori campo
		//--------------------------------------------------------------------------------
		
		// Tutti i sottocampi sono almeno parzialmente risolti: moltiplicazione valori 
		if (ilpw.allSubFieldsWithValues) {
			ilpw.isInstrDynamicSolvedFull = true;
			ilpw.isInstrDynamicToSolve = false;
			ilpw.al_ValuesWrk = new ArrayList<String> ();
			ilpw.dynamicFieldWrk =ilpw.dynamicFieldToSolve;
			generateFieldValuesRecursive(ilpw.dynamicFieldWrk, 0,ilpw.al_ValuesWrk, "");  // Moltiplicazione singoli valori
			// Scan valori campi individuati
			for (String valueField : ilpw.al_ValuesWrk) {
				ilpw.al_valuesField.add(valueField);
			}
			// per logiche local, update istruzione
			if (!ilpw.isExecLogicSpreaded) {
				ilpw.dynamicFieldToSolve.entityDynamicField.setSolved(true);
			}
		}

		//--------------------------------------------------------------------------------
		// 3) Eliminazione duplicati e ordinamento valori
		//--------------------------------------------------------------------------------
				
		Set<String> set_valuesField = null;
		set_valuesField = new HashSet<String>();
		set_valuesField.addAll(ilpw.al_valuesField);
		ilpw.al_valuesField.clear();
		ilpw.al_valuesField.addAll(set_valuesField);
		Collections.sort(ilpw.al_valuesField);

		//--------------------------------------------------------------------------------
		// 4) Update valori in struttura campo dinamico.
		//    Il dettaglio dei valori, con la loro origine e la fb entity è a livello di sottocampo
		//    In caso di campo elementare la riga di EntityDynamicFieldSub con sottocampo a space
		//    di servizio, è già caricata a livello di sorttocampo
		//--------------------------------------------------------------------------------
				
		ilpw.dynamicFieldToSolve.al_valuesField = ilpw.al_valuesField;
	}
    

	/* -------------------------------------------------------------
	 * Aggiornamento istruzione origine con flag stato soluzione
	 * -------------------------------------------------------------
	 * 
	 * La risoluzione di una istruzione dinamica è fatta da LogicSamePgm sempre un operando per volta.
	 * 
	 * Se sono presenti ultime assegnazioni spreaded 					si accende il flag instrDynamicSpreaded.
     * Se sono stati individuati dei valori per TUTTI i campi 			si accende il flag instrDynamicSolved.
     * Se sono state risolte TUTTE le ultime assegnazioni 				si accende il flag instrDynamicSolvedFull.
     * Se sono presenti ultime assegnazioni in attesa di dati esterni 	si accende il flag instrDynamicWaitingForData.
     * 
	 */
	/*
	public void updateInstr(LogicWorkProcess ilpw) {

		ilpw.instrDynamicSolved = true;
		ilpw.isInstrDynamicSolvedFull = true;
		ilpw.isInstrDynamicSpreaded = false;
		ilpw.instrDynamicWaitingForData = false;
		
		// Operando di cui non sono stati individuati valori validi
		if (ilpw.al_valuesField.size() == 0) {
			ilpw.instrDynamicSolved = false;
			ilpw.isInstrDynamicSolvedFull = false;
		}
			
		
		// Verifica se ci sono ancora ultime assegnazioni da risolvere o in attesa di dati esterni.
		
		// Scan CAMPI in programma origine 
		for (int i = 0; i < ilpw.arDataItemFieldSubToSolve.length; i++) {

			// Campo literal alfanumerica: skip
			if (ilpw.arDataItemFieldSubToSolve[i].getIdentifierType() == EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {continue;}
			ilpw.dataItemFieldToSolve = ilpw.arDataItemFieldSubToSolve[i];
			ilpw.dynamicFieldName = ilpw.arDataItemFieldSubToSolve[i].getNameIdentifier();
			ilpw.dynamicFieldToSolve.entityDynamicField.setWaitingForData(false);
			ilpw.dynamicFieldToSolve.entityDynamicField.setSolvedFull(true);
 
			// Estrazione sottocampi elementari disponibili in LogicInfoDynamic2  del programma ORIGINE
			ilpw.al_DynamicFieldSubWrk = logicInfoDynamic.getDynamicFieldsSub(ilpw.instrDynamicOrigin.getNumInstr(), ilpw.dynamicFieldName);
			ilpw.al_DynamicFieldSubWrk = ilpw.dynamicFieldToSolve.al_FieldSub;

			// Scan SOTTOCAMPI campo in programma origine (presente anche in caso di campo non di gruppo)
			for (LogicDynamicFieldSub dynamicFieldSubWrk : ilpw.al_DynamicFieldSubWrk) {

				ilpw.dynamicFieldSubToSolve = dynamicFieldSubWrk;
                ilpw.sizeSubFieldOrigin = dynamicFieldSubWrk.dataItemIdentifierSubField.getDataItem().getSizeBytes();
				
   				// Scan ultime assegnazioni sottocampo local generate nel programma origine 
				for (LogicDynamicFieldSubSetting lastSetLocal : dynamicFieldSubWrk.al_lastSetLocal) {

					if (lastSetLocal.lastSetSolved) {continue;}           	 // Ultima assegnazione risolta: skip
 					
					ilpw.isInstrDynamicSolvedFull = false;                     // Ultima assegnazione da risolvere
					ilpw.dynamicFieldToSolve.entityDynamicField.setSolvedFull(true);
					if (isLastSetWaitingForData(lastSetLocal)) {
 						ilpw.instrDynamicWaitingForData = true;				 // Da file/cics/...
						ilpw.dynamicFieldToSolve.entityDynamicField.setWaitingForData(true);

					}
					
					if (lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_DFHCOMMAREA
 					||  lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_TWA		
 					||  lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_CSA		
 					||  lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE		
 					||  lastSetLocal.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM) {
 						ilpw.isInstrDynamicSpreaded = true;
					}
					
				} // end-for last-set
                
                // Scan ultime assegnazioni sottocampo spreaded generate nel programma origine e/o chiamanti
				for (LogicDynamicFieldSubSetting lastSetSpreaded : dynamicFieldSubWrk.al_lastSetSpreaded) {

					ilpw.isInstrDynamicSpreaded = true;                      // Ultima assegnazione da risolvere/risolta nei programmi chiamanti
					if (lastSetSpreaded.lastSetSolved) {continue;}           // Ultima assegnazione risolta: skip
					
					ilpw.isInstrDynamicSolvedFull = false;                     // Ultima assegnazione da risolvere
					ilpw.dynamicFieldToSolve.entityDynamicField.setSolvedFull(true);
 					if (isLastSetWaitingForData(lastSetSpreaded)) {
 						ilpw.instrDynamicWaitingForData = true;				 // Da file/cics/...
 						ilpw.dynamicFieldToSolve.entityDynamicField.setWaitingForData(true);
					}
					
 					if (lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_DFHCOMMAREA
 					||  lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_TWA		
 					||  lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_CSA		
 					||  lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE		
 					||  lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM) {
 						ilpw.isInstrDynamicSpreaded = true;
					}
 					
				} // end-for last-set

			} // end-for sottocampi

		} // end-for campi
		

        // Flags stato soluzione 
		ilpw.instrDynamicOrigin.setDynamicSpreaded(ilpw.isInstrDynamicSpreaded);
		ilpw.instrDynamicOrigin.setDynamicSolved(ilpw.instrDynamicSolved);
		ilpw.instrDynamicOrigin.setDynamicSolvedFull(ilpw.isInstrDynamicSolvedFull);
		ilpw.instrDynamicOrigin.setDynamicWaitingForData(ilpw.instrDynamicWaitingForData);

	}
*/

	/* --------------------------------------------------------------------
     * Restituisce true se ultima assegnazione in attesa di dati esterni
     * --------------------------------------------------------------------
     * 
     * L0ultima assegnazione fornita in input NON è ancora stata risolta.
     * 
     */
	private boolean isLastSetWaitingForData(LogicDynamicFieldSubSetting lastSet) {

		if (lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_READ
		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_SQL_SELECT	
		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_FILE_GET
		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRMID
		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRNID		
		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM	
		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TS_QUEUE	
		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TD_QUEUE	
		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_RECEIVE_MAP) {
			return true;
		}

		return false;
	}

	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Metodi privati                                        /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

	/* ------------------------------------------------------------------------------------
     * Restituisce true se i valori generati per il sottocampo coprono la dimensione completa
     * ------------------------------------------------------------------------------------
     * 
     * Per esempio potrebbero esserci nel programma assegnazioni
     * parziali di literal e assegnazioni sempre parziali da campi
     * da risolvere nei pgm chiamanti, come DFHCOMMAREA oppure
     * Using Parm.
     * 
     */
	private boolean areSubFieldValuesFullCovering(LogicWorkProcess ilpw, LogicDynamicFieldSub logicDynamicFieldSub) {
		
		int sizeSubField = 0;
		int posStart = 0;								// Posizione inizio valore
		int posEnd = 0;									// Posizione fine valore 
		boolean ar_posSubField[] = null;				// Ogni posizione indica se coperta da valori assunti dal sottocampo		
		
		// Dimensioni sottocampo in programma origine
		sizeSubField = logicDynamicFieldSub.entityDynamicFieldSub.getSizeSubField();
		
		// In caso di logiche spreaded, la ricerca dei valori sottocampo potrebbe
		// iniziare NON da un sottocampo completo ma già da una posizione di un campo,
		// come nel caso di chiamante Cics Link Commarea(WS-COMMAREA).
		// Ciò significa che è stato attivato il processo ricorsivo in modo
		// artificioso, da una condizione che non si riscontra nella soluzione delle
		// logiche stesso programma, partendo da un sottocampo di lavoro che NON
		// è da considerarsi atomico, ai fini della ricomposizione dei valori.
		// Quindi, in questi casi, il size da considerare per il sottocampo, è
		// dato dalla lunghezza nel sottocampo originario da cercare, individuata
		// nel programma origine e impostata nell'ultima assegnazione spreaded.
// 		if (ilpw.isExecLogicSpreaded && ilpw.posInSubField > 1) {
		if (ilpw.isExecLogicSpreaded ) {
			sizeSubField = ilpw.lngInSubField;
		}
		
		ar_posSubField = new boolean[sizeSubField];
		
		// Default tutte le posizioni non coperte da valori
		for (int i = 0; i < ar_posSubField.length; i++) {
			ar_posSubField[i] = false;
		}
		
		// Scan valori individuati per il sottocampo origine , da posizione per lunghezza
		for (LogicDynamicValue logicDynamicValue : logicDynamicFieldSub.al_value) {
			
			// posizione di inizio e lunghezza nel sottocampo originario impostato dall'ultima assegnazione
			posStart = logicDynamicValue.entityDynamicValue.getPosInSubField() - 1;					// 0-based
			posEnd = posStart + logicDynamicValue.entityDynamicValue.getLngInSubField() - 1;		// 0-based
			
			// In caso di receiver impliciti la catena di trasformazione può fare riferimento a un valore
			// parziale che NON copre tutto il sottocampo origine. 
			// In quel caso la catena di trasformazione NON deve essere considerata per la composizione dei valori
			// Update posizioni sottocampo come coperte da valori
			for (int i = posStart; i <= posEnd; i++) {
				ar_posSubField[i] = true;
			}
			
		}

        // Scan posizioni sottocampo
		for (int i = 0; i < ar_posSubField.length; i++) {
			
			// Posizione non coperta da valore
			if (ar_posSubField[i] == false) {
				return false;
			}
		}
		
		return true;
	}


	/*
     * ------------------------------------------------------
     * Marcatura catene di trasformazione formalmente valide
     * ------------------------------------------------------
     * 
     * Si tratta di catene di trasformazione coperte da qualche path.
     * Tutte le istruzioni di trasformazione devono trovarsi nello stesso path
     * e rispettare la stessa sequenza di esecuzione.
     * Non si tiene conto di catene invalidate da assegnazioni successive
     * incondizionate.
     * 
     */
	private void markChainSetCoveredByAnyPath(LogicWorkProcess ilpw
			                                , LogicDynamicFieldSub dynamicFieldSub) {
		
		ArrayList<LogicDynamicFieldSubSetting> al_specificChainSetSubField = null;	              // Catena trasformazioni per un sottocampo specifica
		LogicDynamicFieldSubSetting lastSetSubField = null;                                       // Ultima assegnazione
		Integer[] arPathNumCovering = null;
		EnumLogicSetMode setMode = null;
        int i = 0; 
		
        // Allocazione arrays per flags copertura in paths
        // Suppongo nessuna catena di trasformazione supportata da almeno un path valido
        dynamicFieldSub.ar_chainSetFlagDisabled = new Boolean[dynamicFieldSub.al_al_chainSetSubField.size()];
        dynamicFieldSub.ar_chainSetFlagPathCovering = new Boolean[dynamicFieldSub.al_al_chainSetSubField.size()];
        dynamicFieldSub.ar_chainSetFlagValues = new Boolean[dynamicFieldSub.al_al_chainSetSubField.size()];
        dynamicFieldSub.anyChainSetFlagPathCovering = false;
        
		// Scan CATENE di trasformazione individuate associate al sottocampo.
		// Verifica se valori individuati da ultima assegnazione, statici o dinamici o da media esterno
		// Individuazione paths che supportano le catene di trasformazione
		for (i = 0; i < dynamicFieldSub.al_al_chainSetSubField.size(); i++) {

			al_specificChainSetSubField = dynamicFieldSub.al_al_chainSetSubField.get(i);
			lastSetSubField = al_specificChainSetSubField.get(al_specificChainSetSubField.size() - 1);
			
			// Ultima assegnazione sicuramente con valori
			if (lastSetSubField.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_LITERAL_ALPHA ||
				lastSetSubField.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_LITERAL_NUM ||
				lastSetSubField.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_LITERAL_ALPHA ||
				lastSetSubField.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_LITERAL_NUM) {
        		dynamicFieldSub.ar_chainSetFlagDisabled[i] = false;    	  			// Catena trasformazione abilitata per il sottocampo
        		dynamicFieldSub.ar_chainSetFlagPathCovering[i] = true;   			// Catena trasformazione coperta da path per il sottocampo
        		dynamicFieldSub.ar_chainSetFlagValues[i] = true;  		  			// Catena trasformazione CON values per il sottocampo
        		dynamicFieldSub.anyChainSetFlagPathCovering = false;      			// Catena trasformazione coperta da paths sottocampo (di servizio)
				continue;
			}
			
			// Individuazione paths a copertura della catena di trasformazione
			// Inserimento num istruzioni path in elemento array corrispondente (può avere 0 entries)
			arPathNumCovering = getAllPathsCovering(ilpw, al_specificChainSetSubField);
			dynamicFieldSub.al_chainSetPathNumCovering.add(arPathNumCovering);			
			
			// Ultima assegnazione NON ha individuato dei valori
			if (!lastSetSubField.lastSetSolved || lastSetSubField.al_Value.size() == 0) {
        		dynamicFieldSub.ar_chainSetFlagDisabled[i] = true;    				// Catena trasformazione abilitata per il sottocampo
        		dynamicFieldSub.ar_chainSetFlagPathCovering[i] = true;  			// Catena trasformazione coperta da path per il sottocampo
        		dynamicFieldSub.ar_chainSetFlagValues[i] = false;	                // Catena trasformazione SENZA values per il sottocampo
        		dynamicFieldSub.anyChainSetFlagPathCovering = true;     			// Catena trasformazione coperta da paths sottocampo 
        		continue;
			}
			 
			// Catena di trasformazione sottocampo supportata da qualche path
			if (arPathNumCovering.length > 0) {
        		dynamicFieldSub.ar_chainSetFlagDisabled[i] = false;    				// Catena trasformazione abilitata per il sottocampo
        		dynamicFieldSub.ar_chainSetFlagPathCovering[i] = true;  			// Catena trasformazione coperta da path per il sottocampo
        		dynamicFieldSub.ar_chainSetFlagValues[i] = true;	                // Catena trasformazione CON values per il sottocampo
        		dynamicFieldSub.anyChainSetFlagPathCovering = true;     			// Catena trasformazione coperta da paths sottocampo 
        		continue;
			}
			
			// Non ci sono path a supporto della catena di trasformazione
			
			// Sottocampo NON movimentato con default value: ok se value dichiarato 
			setMode = al_specificChainSetSubField.get(al_specificChainSetSubField.size() - 1).entityDynamicFieldSetting.getSetMode();
			if (setMode == EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_ZEROES
			||  setMode == EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_LITERAL_ALPHA	
			||  setMode == EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_LITERAL_NUM) {
        		dynamicFieldSub.ar_chainSetFlagDisabled[i] = false;    				// Catena trasformazione abilitata per il sottocampo
        		dynamicFieldSub.ar_chainSetFlagPathCovering[i] = false; 			// Catena trasformazione NON coperta da path per il sottocampo
        		dynamicFieldSub.ar_chainSetFlagValues[i] = false;					// Catena trasformazione CON values per il sottocampo
        		continue;
			}
			
			// Nessun path di esecuzione supporta la catena di trasformazione
			
    		dynamicFieldSub.ar_chainSetFlagDisabled[i] = true;    					// Catena trasformazione disabilitata per il sottocampo
      		dynamicFieldSub.ar_chainSetFlagPathCovering[i] = false; 				// Catena trasformazione NON coperta da paths sottocampo        		
      		dynamicFieldSub.ar_chainSetFlagValues[i] = false;  						// Catena trasformazione SENZA values per il sottocampo
      		
      		// Rimozione valore individuato per il sottocampo nella catena di trasformazione corrente
      		// per il programma corrente
    		for (int j = 0; j < dynamicFieldSub.al_value.size(); j++) {       			
				// Valore non impostato a fronte di programma e catena corrente: skip
    			if (dynamicFieldSub.al_value.get(j).numChainSet != i
    			|| !dynamicFieldSub.al_value.get(j).pgmSet.equals(this.program.programName)	) {
    				continue;
    			}
    			
    			// Eliminazione valore
    			dynamicFieldSub.al_value.remove(j);
    			j--;
			}

			       	   
		} // end-for catene
		
	}


    /*
     * --------------------------------------------------------
     * Verifiche supplementari su catena trasformazioni valida
     * --------------------------------------------------------
     * 
     * La catena di trasformazioni, pur risultando formalmente valida
     * e supportata da almeno un path, potrebbe comunque essere scartata
     * e non risultare valida.
     * 
     * Sono gestite le seguenti tipologie di casi:
     * 
     * 
     * (1) VALORE SOTTOCAMPO DI DEFAULT SENZA MOVE ASSEGNAZIONI
     * 
     * Si tratta di catene di trasformazione di un solo elemento,
     * dovuto al valore iniziale di un campo, parte di un gruppo, in assenza di trasformazioni
     * di assegnazione. 
     * Capita per esempio che, un sottocampo di un campo origine di una 
     * istruzione dinamica, non è mai referenziato, pur producendo un valore 
     * di default direttamente o attraverso un receiver implicito. 
     * Sono comunque presenti, in qualche catena, assegnazioni esplicite 
     * o implicite incondizionate, del sottocampo origine, attraverso assegnazioni
     * implicite dei campi di gruppo sotto i quali il sottocampo è definito.
     * In questo caso la catena di trasformazione deve essere scartata
     * in quanto il valore di default è sicuramente sovrascritto.
     * 
     * (2) VALORE SOTTOCAMPO DI DEFAULT CON MOVE ASSEGNAZIONI
     * 
     * Si tratta di catene di trasformazione di più elementi.
     * Capita per esempio quando un campo, parte di un gruppo, viene
     * assegnato al sottocampo di cui trovare il valore o a una sua 
     * trasformazione. In questo caso il processo genera una catena
     * con il valore di default value del campo.
     * Tuttavia il campo è parte di un gruppo e il gruppo può essere
     * assegnato precedentemente, generando altre catene. 
     * In questo caso la catena di trasformazione con il value di
     * default deve essere scartata, in quanto il valore di default 
     * è sicuramente sovrascritto.
     * Si risolve cercando nelle catene assegnazioni precedenti, per
     * il campo che ha generato il value di default, o per un suo
     * receiver implicito.
     * 
     * 
     */
    private void checkChainSetMarked(LogicWorkProcess ilpw
    							   , LogicDynamicFieldSub innerSubField
								   , int curChainSetSubField) {

 		
    	
    	// (1) Default prodotti senza assegnazioni 
    	checkChainSetMarkedValuesNoAssign(ilpw, innerSubField, curChainSetSubField);
		  
	  	// (2) Default prodotti indebitamente con assegnazioni successive
    	checkChainSetMarkedWithAssign(ilpw, innerSubField, curChainSetSubField);
    	
    }

 
    /*
     * --------------------------------------------------------
     * Verifiche supplementari su catena trasformazioni valida
     * --------------------------------------------------------
     * 
     * La catena di trasformazioni, pur risultando formalmente valida
     * e supportata da almeno un path, potrebbe comunque essere scartata
     * e non risultare valida.
     * 
     * (1) VALORE SOTTOCAMPO DI DEFAULT SENZA MOVE ASSEGNAZIONI
     * 
     * Si tratta di catene di trasformazione di un solo elemento,
     * dovuto al valore iniziale di un campo, in assenza di trasformazioni
     * di assegnazione. 
     * Capita per esempio che, un sottocampo di un campo origine di una 
     * istruzione dinamica, non è mai referenziato, pur producendo un valore 
     * di default direttamente o attraverso un receiver implicito. 
     * Sono comunque presenti, in qualche catena, assegnazioni esplicite 
     * o implicite incondizionate, del sottocampo origine.
     * Si tratta del caso che il gruppo sotto cui è definito il sottocampo
     * abbia generato ricorsivamente catene di trasformazione che aggiornano 
     * indirettamente il sottocampo.
     * In questo caso, se la la catena di trasformazione è supportata dallo stesso path
     * deve essere scartata, in quanto il valore di default è sicuramente sovrascritto.
     * 
     */
    private void checkChainSetMarkedValuesNoAssign(LogicWorkProcess ilpw
			    							     , LogicDynamicFieldSub dynamicSubField
												 , int inpChainSetSubField) {

 		ArrayList<ArrayList<LogicDynamicFieldSubSetting>> al_chainSetSubField = null; 	// Catene trasformazioni per un sottocampo
		ArrayList<LogicDynamicFieldSubSetting> al_SetSubField = null;	               	// Catena trasformazioni per un sottocampo specifica
		LogicDynamicFieldSubSetting lastSetSubField = null;                             // Trasformazione per un sottocampo singola di assegnazione valori
		LogicDynamicFieldSubSetting lastSetSubFieldDefault = null;                      // Trasformazione di cui verificare la validita
		ProgramCobolEntry<? extends Instruction> entryInstructionLastSet = null;        // Entry di programma con istruzione
		int numInstrLastSet = 0;                                                        // Numero istruzione di assegnazione (Move, Read, ...)
		int i = 0;
		
		
		// Tutte le catene di trasformazione individuate per il sottocampo
    	al_chainSetSubField = dynamicSubField.al_al_chainSetSubField;
    	
    	// Catena specifica oggetto di analisi
    	al_SetSubField = al_chainSetSubField.get(inpChainSetSubField);
    	
      	// Catena da trattare ha piu di un elemento: skip
		if (al_SetSubField.size() > 1) {
			return;
		}  

		// Trasformazione singola oggetto di analisi
		lastSetSubFieldDefault = al_SetSubField.get(0);
        
		// Il sottocampo non deve essere movimentato e avere un value
		if (lastSetSubFieldDefault.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_LITERAL_ALPHA) {
			return;
		}
		
    	// (1) VALORE SOTTOCAMPO DI DEFAULT SENZA MOVE ASSEGNAZIONI E CON VALUE
		
		// Scan catene trasformazione individuate per il sottocampo o indirettamente
		// attraverso i campi di gruppo sotto cui è definito
        for (i = 0; i < al_chainSetSubField.size(); i++) {
        	
          	// La catena coincide con quella da trattare o catena non valida: skip
        	if (i == inpChainSetSubField) {continue;}
         	if (dynamicSubField.ar_chainSetFlagPathCovering[i] == false) {continue;}
         	if (dynamicSubField.ar_chainSetFlagDisabled[i] == true)      {continue;}
        	
        	// Catena da verificare diversa da quella in oggetto: si può procedere
        	al_SetSubField = al_chainSetSubField.get(i);
        	
        	// Ultima trasformazione con impostazione valore/i sottocampo
        	lastSetSubField = al_SetSubField.get(al_SetSubField.size() - 1);
        	numInstrLastSet = lastSetSubField.entityDynamicFieldSetting.getNumInstrSet();
        	
        	// Entry istruzione di ultima assegnazione
        	entryInstructionLastSet = ilpw.programCur.entryProcedure(numInstrLastSet);
        	
        	// Istruzione incondizionata: la catena in esame è da scartare
        	if (!entryInstructionLastSet.isUnderCondition()) {
                
        		// Rimozione valore assegnato precedentemente in processo ricorsivo
         		removeValues(lastSetSubField.entityDynamicFieldSetting.getIdPgmSet(), dynamicSubField, inpChainSetSubField);
        		
    			// Catena con una sola assegnazione dovuta a default value NON è da considerarsi valida 
         		dynamicSubField.ar_chainSetFlagDisabled[inpChainSetSubField] = Boolean.parseBoolean("false");	// Flag su specifica catena
         		
         		return;
			
        	} // end-if
		
        } // end-for catene 
    }
    
    // Rimozione valori sottocampo assegnati in specifica catea di trasformazione e programma specificato
    private void removeValues(String pgmSet, LogicDynamicFieldSub dynamicSubField, int numChainSet) {
		ArrayList<LogicDynamicValue> al_DynamicFieldSubValue = null;
		al_DynamicFieldSubValue = new ArrayList<LogicDynamicValue>();
		
    	// scan valori
    	for (LogicDynamicValue dynamicValue : dynamicSubField.al_value) {
			if (!dynamicValue.pgmSet.equals(pgmSet) || !(dynamicValue.numChainSet == numChainSet)) {continue;}
			al_DynamicFieldSubValue.add(dynamicValue);
		}
    	dynamicSubField.al_value = al_DynamicFieldSubValue;
	}




	/*
     * --------------------------------------------------------
     * Verifiche supplementari su catena trasformazioni valida
     * --------------------------------------------------------
     * 
     * La catena di trasformazioni, pur risultando formalmente valida
     * e supportata da almeno un path, potrebbe comunque essere scartata
     * e non risultare valida.
     * 
     * (2) VALORE SOTTOCAMPO DI DEFAULT CON MOVE ASSEGNAZIONI IMPLICITE
     * 
     * Si tratta di catene di trasformazione di più elementi.
     * Capita per esempio quando un campo, parte di un gruppo, viene
     * assegnato al sottocampo di cui trovare il valore o a una sua 
     * trasformazione. In questo caso il processo genera una catena
     * con il valore di default value del campo.
     * Tuttavia il campo è parte di un gruppo e il gruppo può essere
     * assegnato precedentemente, generando altre catene. 
     * Stessa situazione se il campo è elementare ma assegnato
     * precedentemente.
     * In questo caso la catena di trasformazione con il value di
     * default deve essere scartata se l'assegnazione implicita è incondizionata, 
     * in quanto il valore di default è sicuramente sovrascritto.
     * Si risolve cercando nelle altre catene assegnazioni, se un suo
     * receiver implicito ha una assegnazione incondizionata.
     * Le due catene di trasformazione, quella iniziale da verificare e quella
     * con assegnazioni implicite incondizionate, dovrebbero inoltre risiedere 
     * nello stesso path che le supporta.
     * 
     */
 
    private void checkChainSetMarkedWithAssign(LogicWorkProcess ilpw
										     , LogicDynamicFieldSub dynamicSubField
										     , int inpNumChainSetSubField) {

    	ArrayList<ArrayList<LogicDynamicFieldSubSetting>> al_chainSetSubField = null; 	// Catene trasformazioni per un sottocampo
    	ArrayList<LogicDynamicFieldSubSetting> al_SetSubField = null;	               	// Catena trasformazioni per il sottocampo specifico
    	LogicDynamicFieldSubSetting lastSetSubField = null;                             // Ultima Trasformazione di cui verificare la validita
    	LogicDynamicFieldSubSetting wrkLastSetSubField = null;                              // Trasformazione di servizio
     	ProgramCobolEntry<? extends Instruction> entryInstructionLastSet = null;        // Entry di programma con istruzione
    	ProgramPath programPathLastSet = null;                                          // Path che copre la catena trasformazioni corrente
    	ProgramPathEntry pathEntry = null;                                              // Singolo elemento del path , indica una istruzione
     	int numPathLastSet = 0;                                                         // Numero path ultima assegnazionein input
     	int numPathLastSetWrk = 0;                                                      // Numero path ultima assegnazione di servizio
      	int numInstrLastSetWrk = 0;                                                     // Numero istruzione di assegnazione (Move, Read, ...) di servizio
    	int i = 0;																		// Di servizio
       	int j = 0;																		// Di servizio
 

    	// Tutte le catene
    	al_chainSetSubField = dynamicSubField.al_al_chainSetSubField;

    	// Catena oggetto di analisi
    	al_SetSubField = al_chainSetSubField.get(inpNumChainSetSubField);

    	// Catena da trattare ha un solo un elemento: exit
    	if (al_SetSubField.size() == 1) {
    		return;
    	}  

    	// Ultima Trasformazione singola catena trasformazione di input
    	lastSetSubField = al_SetSubField.get(al_SetSubField.size() - 1);
 
    	// Interessano solo le ultime assegnazioni di default value
    	if (lastSetSubField.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_SPACES
    	&&  lastSetSubField.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_LITERAL_ALPHA) {
    		return;
    	} // end-if
   

    	// Numero path catena di trasformazione in analisi
     	
    	// Scan tutte le catene trasformazione individuate per il sottocampo
    	for (i = 0; i < al_chainSetSubField.size(); i++) {

    		// La catena coincide con quella da trattare o catena non valida: skip
    		if (i == inpNumChainSetSubField) {continue;}
       		if (dynamicSubField.ar_chainSetFlagPathCovering[i] == false) {continue;}
       		if (dynamicSubField.ar_chainSetFlagDisabled[i] == true)      {continue;}

       		
    		// Catena da verificare diversa da quella in oggetto: si può procedere
       	    // Interessa solo l'ultima assegnazione 
    		al_SetSubField = al_chainSetSubField.get(i);
    		wrkLastSetSubField = al_SetSubField.get(al_SetSubField.size() - 1);
    		
   			// Interessano solo le ultime assegnazioni di valori NON dovute a default values
			if (wrkLastSetSubField.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_SPACES
			&&  wrkLastSetSubField.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_LITERAL_ALPHA) {
				continue;
			}
    		
			// === ATTENZIONE ====
			// Al momento si considera solo il primo path indicato a copertura delle catene di trasformazione
			// Successive elaborazioni più sofisticate devono considerare tutti i path generati per questa analisi
			// L'ultima assegnazione incondizionata di receiver implicito deve trovarsi prima dell'ultima
			// assegnazione del sottocampo in esame per non invalidare la catena di trasformazioni
		   	numPathLastSet = dynamicSubField.al_chainSetPathNumCovering.get(inpNumChainSetSubField)[0];
      		numPathLastSetWrk = dynamicSubField.al_chainSetPathNumCovering.get(i)[0];
			if (numPathLastSet != numPathLastSetWrk) {continue;}
			
			// Descrittore Path di esecuzione che supporta la catena di trasformazione
			programPathLastSet = ilpw.arPaths[numPathLastSet];
			
			// Mi posiziono nel path sull'istruzione di last set della catena in INPUT 
			for (j = programPathLastSet.getEntries().length - 1; j > 0; j--) {
				 pathEntry = programPathLastSet.getEntries()[j];
				 if (pathEntry.getNumInstr() == lastSetSubField.entityDynamicFieldSetting.getNumInstrSet()) {
					break;
				 }
			}
			
			// Istruzione non trovata: skip catena
			if (j == 0) {continue;}
			
			// Verifico se successivamente (tornando indietro nel path)
			// Si trova l'istruzione dell'ultima assegnazione della catena corrente in ciclo
			for (j = j - 1; j > 0; j--) {
				 pathEntry = programPathLastSet.getEntries()[j];
				 
				 // Assegnazione successiva individuata.
				 // Nello stesso path il sottocampo ha subito una assegnazione successiva.
				 if (pathEntry.getNumInstr() == wrkLastSetSubField.entityDynamicFieldSetting.getNumInstrSet()) {
					numInstrLastSetWrk = wrkLastSetSubField.entityDynamicFieldSetting.getNumInstrSet();
			        entryInstructionLastSet = ilpw.programCur.entryProcedure(numInstrLastSetWrk);

		       		// E' una istruzione incondizionata di assegnazione successiva: la catena corrente fornita in input è da scartare
		       		if (!entryInstructionLastSet.isUnderCondition()) {
						dynamicSubField.ar_chainSetFlagDisabled[inpNumChainSetSubField] = Boolean.parseBoolean("true");	// Flag disabilitazione su specifica catena
		     			// Rimozione valore assegnato in processo ricorsivo
		        		removeValues(lastSetSubField.entityDynamicFieldSetting.getIdPgmSet(), dynamicSubField, inpNumChainSetSubField);
		       		}
				}
			}
			
    	} // end-for catene 

    }




	/* --------------------------------------------------------------------------------
	 * Restituisce i path che includono le trasformazioni del sottocampo elementare. 
	 * --------------------------------------------------------------------------------
	 * 
	 * In caso di catena di trasformazione di un solo elemento di impostazione da 
	 * literal, significa caso di sottocampo con value non modificato nel programma.
	 * 
	 * In questo caso NON si tiene conto dei path di esecuzione e si restituisce un array vuoto.
	 * Altrimenti il path deve includere le istruzioni specificate nelle trasformazioni
	 * esattamente nello stesso ordine, per garantire la corretta valorizzazione
	 * dei sottocampo. 
	 * 
	 * L'ArrayList di set contiene le trasformazioni e i relativi numeri di istruzione.
	 * I possibili path di esecuzione sono stati calcolati a inizio processo e sono in ordine crescente
	 * e sono già nel corretto ordine di esecuzione, ovvero l'ultima istruzione di ogni path è l'istruzione
	 * dinamica target.
	 * 
	 * Se non sono trovati path di esecuzione validi restituisce null
	 * 
	 */
	private Integer[] getAllPathsCovering(
			                               LogicWorkProcess ilpw									// Dati comuni
										 , ArrayList<LogicDynamicFieldSubSetting> al_SubFieldSetting	// Catena di trasformazioni da validare
										   ) {
	
		ProgramPath path = null;
		ProgramPathEntry pathEntry = null;
		ProgramPathEntry[] ar_pathEntry = null;		
		LogicDynamicFieldSubSetting dynamicFieldSubSetting = null;	// Struttura con informazioni di set del sottocampo
		ArrayList<LogicDynamicFieldSubSetting> al_SubFieldSettingW = null;
		ArrayList<Integer> al_pathOk = null;
		Integer[] ar_pathOk = null;
		
		al_SubFieldSettingW = new ArrayList<LogicDynamicFieldSubSetting>();
		al_pathOk = new ArrayList<Integer>();
		
		// Indici e numeri di servizio
		int numInstrSet = 0;										// Numero istruzione di set
		int numInstrPath = 0;										// Numero istruzione in path
		int i = 0;													// Di servizio
		int j = 0;													// Di servizio
		int k = 0;													// Di servizio
		  
		boolean isPathMatch = true;                                 // Path supporta la catena di trasformazioni correttamente

		
        //////////////////////////////////////////////////////////////////////////////////////
		// (1) Reverse array list di setting per agevolare la comparazione
        //////////////////////////////////////////////////////////////////////////////////////
		
		// Scan istruzioni di trasformazione del sottocampo.
		// Si considerano solo le trasformazioni di assegnazione.
		for (i = al_SubFieldSetting.size() - 1; i >= 0; i--) {
			
			dynamicFieldSubSetting = al_SubFieldSetting.get(i);
			
			// Interessano solo le trasformazioni di assegnazione con MOVE
			if (dynamicFieldSubSetting.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_FIELD
			&&  dynamicFieldSubSetting.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_GROUP
			&&  dynamicFieldSubSetting.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_LITERAL_ALPHA
			&&  dynamicFieldSubSetting.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_LITERAL_NUM
			&&  dynamicFieldSubSetting.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_ZEROES
			&&  dynamicFieldSubSetting.entityDynamicFieldSetting.getSetMode() != EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_TBITEM_OF_OCCURS) {
				continue;
			}
			
			al_SubFieldSettingW.add(dynamicFieldSubSetting);
										
		} // end-for

		
        ///////////////////////////////////////////////////////////////////////////////////////////
		// (2) Non è presente nessuna trasformazione del campo elementare.
		//     Situazione in cui il campo dinamico è direttamente nell'istruzione oppure
		//     in caso di LAST-SET-LINKAGE con pointer in DFHCOMMAREA o in using parm
		//     NON movimentato
        ///////////////////////////////////////////////////////////////////////////////////////////
		if (al_SubFieldSettingW.size() == 0) {
			return new Integer[0];
		}
		
		
		
        ///////////////////////////////////////////////////////////////////////////////////////////
		// (3) Verifica i path che includono tutte le trasformazioni.
		//     Le istruzioni devono essere presenti esattamente nella stessa sequenza
        ///////////////////////////////////////////////////////////////////////////////////////////
		
		// Scan Paths di esecuzione fra Procedure Division e istruzione dinamica inclusa
		for (i = 0; i < ilpw.arPaths.length; i++) {
			
			path = ilpw.arPaths[i];
			ar_pathEntry = path.getEntries();
			
			isPathMatch = false;                                     // Suppongo il path NON valido

			// Ahead transformation						
			dynamicFieldSubSetting = al_SubFieldSettingW.get(0);
			numInstrSet = dynamicFieldSubSetting.entityDynamicFieldSetting.getNumInstrSet(); 

			// Scan instructions in path						
			for (j = 0; j < ar_pathEntry.length; j++) {
				pathEntry = ar_pathEntry[j];
				numInstrPath = pathEntry.getNumInstr();
				if (numInstrPath == numInstrSet) {
					k++;
					if (k >= al_SubFieldSettingW.size()) {
						isPathMatch = true; 
						break;
					}
					dynamicFieldSubSetting = al_SubFieldSettingW.get(k);
					numInstrSet = dynamicFieldSubSetting.entityDynamicFieldSetting.getNumInstrSet(); 
					continue;
				}
			}
						
		    // Il path sotto esame supporta le trasformazioni:. accodo numero path
	        if (isPathMatch) {
	        	al_pathOk.add(i);
			}
	        
	        // Next path
	        
		} // end-for path

		// Convert to array
        ar_pathOk = new Integer[al_pathOk.size()];
        ar_pathOk = al_pathOk.toArray(ar_pathOk);
        
		return ar_pathOk;
	}




	
	/*
	 * ------------------------------------------------------------------------------
	 * Generazione valori effettivi assunti dal sottocampo nelle varie trasformazioni
	 * ------------------------------------------------------------------------------
	 * 
	 * E' disponibile il descrittore del sottocampo con i valori parziali individuati
	 * e l'indice del valore parziale da trattare.
	 * Viene aggiornata sempre nel descrittore del sottocampo l'array list con i valori
	 * definitivi.
	 * 
	 * I valori assunti dal sottocampo si trovano sull'ultima assegnazione di ogni
	 * catena di trasformazioni valida, come elemento di LogicDynamicSubField.al_value.
	 * Tuttavia l'assegnazione può essere relativa solo a una porzione di sottocampo.
	 * Può quindi essere necessario comporre i valori definitivi assunti dal sottocampo
	 * sulla base dei valori assunti da porzioni del sottocampo stesso.
	 * Un caso comune è quello di un sottocampo assegnato a un campo di gruppo con
	 * assegnazioni successive ai singoli campi componenti il gruppo. 
	 * 
	 * Casi gestiti di pos-lng, a cui corrispondono assegnazioni multiple di valori
	 * 
	 * 1) Porzioni di sottocampo NON sovrapposte 
	 *    1-2
	 *    3-3
	 *    4-1
	 *    5-3 
	 * 2) Porzioni di campo sovrapposte
	 *    1-2
	 *    1-5
	 *    1-7
	 *    3-2
	 *    3-5
	 *    5-3
	 * 3) Porzioni di campo con dei buchi
	 *    1-2
	 *    5-3 
	 */
	@SuppressWarnings("unchecked")
	private ArrayList<String> valuesSubFieldBinded(LogicWorkProcess ilpw, LogicDynamicFieldSub dynamicFieldSub) {
		
		TreeMap<String,ArrayList<String>> tmap_values = null;       			// Key=pos"-"lng, Data=value
		TreeMap<String,ArrayList<String>> tmap_valuesWork = null;       		// Key=pos"-"lng, Data=value
		Entry<String, ArrayList<String>> entry_tmap_valuesPrec = null;			// Entry gruppo pos-lng valori precedenti in loop
		Entry<String, ArrayList<String>> entry_tmap_valuesNextGreater = null;   // Entry gruppo pos-lng valori successivi in loop
		Entry<String, ArrayList<String>> entry_tmap_values = null;				// Entry gruppo pos-lng valori in loop
		Entry<String, ArrayList<String>> ar_entry_tmap_values[] = null;         // Array Entry gruppo pos-lng 
		ArrayList<String> al_valueComplete = null;								// Valori definitivi sottocampo
		ArrayList<String> al_valueCompleteWork = null;							// Valori definitivi sottocampo in progress
		ArrayList<String> al_valuePartial = null;								// Valori parziali sottocampo
		String ar_tmapKey[] = null;												// Chiavi pos-lng gruppi valori
		String keyValue = "";													// Chiave pos-lng gruppi valori 
		String valueDefault = "";												// Valore di default sottocampo
		String valuePartial = "";												// Valore parziale sottocampo
		String keyValuePartialHole = "";										// Chiave pos-lng gruppo valori fra i due forniti
		String keyValueLast = "";												// Chiave pos-lng gruppo valori ultimo
		String keyValueFirst = "";												// Chiave pos-lng gruppo valori primo
		boolean isFragmentationToDo = true;										// True indica valori parziali ancora da frammentare
		boolean areAllValuesFromPos1 = true;								    // True indica tutti valori parziali da pos 1 e NON necessità di frammentazione
		int numChainSet = 0;                                                    //
		int posValueNextGreater = 0;											// Pos successiva valori parziali da elaborare
		int posValuePartialHole = 0;											// Pos successiva valori per buco da riempire fra due gruppi di valori forniti
		int posValuePartialCur = 0;												// Pos corrente valori 									
		int posValuePartialPrec = 0;                                            // Pos precedente valori
		int posValuePartial = 0;                                                // Pos parziale valori
		int lngValuePartialHole = 0;                                            // Lng successiva valori per buco da riempire fra due gruppi di valori forniti
		int lngValuePartialPrec = 0;                                            // Lng precedente valori
		int lngValuePartial = 0;                                                // Lng parziale valori
		int posValue = 0;														// Pos valori																
		int lngValue = 0;														// Lng valori
		int iValue = 0;															// Indice di servizio
		int i = 0;																// Indice di servizio
		int j = 0;																// Indice di servizio

		
		// Default sottocampo definito da value
		if (dynamicFieldSub.dataItemIdentifierSubField.getDataItem().isValueClause()) {
			valueDefault = dynamicFieldSub.dataItemIdentifierSubField.getDataItem().getValueStringFormattedToSize();
		
		// Default sottocampo a space
		} else {
			valueDefault = StringService._pad("", ' ', dynamicFieldSub.dataItemIdentifierSubField.getDataItem().getSizeBytes(), StringService.PAD_RIGHT);
		}
 
		
		// Initial
		tmap_values = new TreeMap<String, ArrayList<String>>();
		tmap_valuesWork = new TreeMap<String, ArrayList<String>>();  
	
		
		//////////////////////////////////////////////////////////////////////////
		// (1) Popolamento preliminare tree map con i valori parziali disponibili
		//     I valori sono ordinati per chiave pos-lng
		//     I valori possono essere multipli per pos-lng
		//////////////////////////////////////////////////////////////////////////
		
		// Scan valori parziali individuati per il sottocampo
		for (iValue = 0; iValue < dynamicFieldSub.al_value.size(); iValue++) {
			
            // Info valore parziale sottocampo
			valuePartial = dynamicFieldSub.al_value.get(iValue).entityDynamicValue.getValue();
			posValuePartial = dynamicFieldSub.al_value.get(iValue).entityDynamicValue.getPosInSubField();
			if (ilpw.isSpreadedSubFielValuePartial) {
				posValuePartial = ilpw.posRcvInSubFieldSpreaded;
			}
			lngValuePartial = dynamicFieldSub.al_value.get(iValue).entityDynamicValue.getLngInSubField();
			numChainSet = dynamicFieldSub.al_value.get(iValue).numChainSet;			// Catena di trasformazione che ha originato il valore
			
			// Il valore è relativo a una catena senza path a copertura o disabilitata: skip
			if (dynamicFieldSub.ar_chainSetFlagPathCovering[numChainSet] == false
		    ||  dynamicFieldSub.ar_chainSetFlagDisabled[numChainSet] == true) {
				continue;
			}
			
			// Recupero o creazione array list valori
			keyValue = posValuePartial + "-" + lngValuePartial;
			al_valuePartial = tmap_values.get(keyValue);
			if (al_valuePartial == null) {
				al_valuePartial = new ArrayList<String>();
			}

			// Formattazione alla lunghezza corretta e accodamento valore a key pos-lng
			valuePartial = StringService._pad(valuePartial, ' ', lngValuePartial, StringService.PAD_RIGHT);
			al_valuePartial.add(valuePartial);
			tmap_values.put(keyValue, al_valuePartial);
			
		}

		//////////////////////////////////////////////////////////////////////////
		// (2) Gestione semplificata se tutti i valori iniziano a colonna 1
		//     I valori vengono portati direttamente in output.
		//////////////////////////////////////////////////////////////////////////
		

		ar_entry_tmap_values = new Entry[tmap_values.entrySet().size()];
		ar_entry_tmap_values = tmap_values.entrySet().toArray(ar_entry_tmap_values);
		al_valueComplete = new ArrayList<String> ();
		
		// Se tutti i valori sottocampo iniziano da pos 1 li porto direttamente in output  
		for (i = 0; i < ar_entry_tmap_values.length; i++) {
			entry_tmap_values = ar_entry_tmap_values[i];
			ar_tmapKey = entry_tmap_values.getKey().split("-");
			posValuePartial = Integer.parseInt(ar_tmapKey[0]);
			if (posValuePartial > 1) {
				areAllValuesFromPos1 = false;
			}
			for (String value : entry_tmap_values.getValue()) {
				al_valueComplete.add(value);
			}
		}
		// Tutti i valori del sottocampo iniziano da pos 1: non si effettua la frammentazione
		if (areAllValuesFromPos1) {
			// Inserisco il valore di default non ancora gestito solo se diverso da space
			if (!valueDefault.isBlank()) {
				al_valueComplete.add(valueDefault);
			}
			// Elimino duplicati e sorto
			Set<String> set_valueComplete = null;
			set_valueComplete = new HashSet<String>();
			set_valueComplete.addAll(al_valueComplete);
			al_valueComplete.clear();
			al_valueComplete.addAll(set_valueComplete);
			Collections.sort(al_valueComplete);
			return al_valueComplete;
		}
		
		
		//////////////////////////////////////////////////////////////////////////
	    // (3) Gestione riduzione a valori parziali elementari NON sovrapposti.
		//     Sono a fronte di Move (pos:lng) per qualunque porzione sottocampo
		//     per valori parziali alla stessa posizione, di lunghezza maggiore
		//////////////////////////////////////////////////////////////////////////
		
		// Iterazione processo fino a completa deframmentazione elementare
		while (isFragmentationToDo) {
  
			tmap_valuesWork = new TreeMap<String, ArrayList<String>>();
			
			entry_tmap_valuesPrec = null;
			ar_entry_tmap_values = null;
			ar_entry_tmap_values = new Entry[tmap_values.entrySet().size()];
			ar_entry_tmap_values = tmap_values.entrySet().toArray(ar_entry_tmap_values);
			
			// Scan partial key/values
			for (i = 0; i < ar_entry_tmap_values.length; i++) {
				entry_tmap_values = ar_entry_tmap_values[i];
				
				// Initial
				if (entry_tmap_valuesPrec == null) {
					entry_tmap_valuesPrec = entry_tmap_values;
					ar_tmapKey = entry_tmap_values.getKey().split("-");
					posValuePartialPrec = Integer.parseInt(ar_tmapKey[0]);
					lngValuePartialPrec = Integer.parseInt(ar_tmapKey[1]);
					tmap_valuesWork.put(entry_tmap_values.getKey(), entry_tmap_values.getValue());
					continue;
				}
				
				// Pos/lng correnti
				ar_tmapKey = entry_tmap_values.getKey().split("-");
				posValuePartial = Integer.parseInt(ar_tmapKey[0]);
				lngValuePartial = Integer.parseInt(ar_tmapKey[1]);

				// Ricerca pos successiva maggiore di quella corrente a partire dal valore parziale successivo
				posValueNextGreater = dynamicFieldSub.dataItemIdentifierSubField.getDataItem().getSizeBytes();
				for (j = i + 1; j < ar_entry_tmap_values.length; j++) {
					entry_tmap_valuesNextGreater = ar_entry_tmap_values[j];
					ar_tmapKey = entry_tmap_valuesNextGreater.getKey().split("-");
					posValuePartialCur = Integer.parseInt(ar_tmapKey[0]);
	                // Valore parziale inizia a posizione maggiore di quella corrente
					if (posValuePartialCur > posValuePartial) {
						posValueNextGreater = posValuePartialCur;
					}
				}

				// Pos inizio diverse con valori NON sovrapposti: porto in output di work
				if (posValuePartialPrec != posValuePartial 
				&&  posValuePartialPrec + lngValuePartialPrec <= posValuePartial) {
					tmap_valuesWork.put(entry_tmap_values.getKey(), entry_tmap_values.getValue());
					posValuePartialPrec = posValuePartial;
					lngValuePartialPrec = lngValuePartial;
					continue;
				}
					
				// Valori sovrapposti: frammentazione
				
				// Inserimento valori troncati alla lunghezza minore.
				// Inserimento valori oltre posizione troncata minore
	            fragmentPartialValues(tmap_values, tmap_valuesWork, entry_tmap_values.getKey(), posValuePartial, lngValuePartial, posValueNextGreater);
				 
	            
	            lngValuePartialPrec = lngValuePartial;
	            
			} // end-for

			// Tree map di lavoro diventa quella corrente
			tmap_values = (TreeMap<String, ArrayList<String>>) tmap_valuesWork.clone();
			
            // Verifica se ancora necessaria deframmentazione
			isFragmentationToDo = false;
			posValuePartialPrec = 0;
			lngValuePartialPrec = 0;
			
			// Scan partial key
			for (String keyPartialValue : tmap_values.keySet()) {
				
				ar_tmapKey = keyPartialValue.split("-");
				posValuePartial = Integer.parseInt(ar_tmapKey[0]);
				lngValuePartial = Integer.parseInt(ar_tmapKey[1]);
				
				// Initial
				if (posValuePartialPrec == 0) {
					posValuePartialPrec = posValuePartial;
					lngValuePartialPrec = lngValuePartial;
					continue;
				}

				// Valori parziali sovrapposti: ancora necessaria deframmentazione
				if ((posValuePartialPrec + lngValuePartialPrec) > posValuePartial) {
					isFragmentationToDo = true;
					break;
				}
				
				// In loop impostazioni
				posValuePartialPrec = posValuePartial;
				lngValuePartialPrec = lngValuePartial;

			} // end-for
			
		} // end-for
		
		
		///////////////////////////////////////////////////////////////////////////////////////
	    // (4) Gestione inserimento valori fra valori parziali forniti, se cè un buco
		//     Il valore da inserire dovrebbe essere estratto dal valore di default
		///////////////////////////////////////////////////////////////////////////////////////

		entry_tmap_valuesPrec = null;
		tmap_valuesWork = new TreeMap<String, ArrayList<String>>();

		// I valori parziali sono ordinati per chiave pos-lng
		for (Entry<String, ArrayList<String>> entry_tmap_values3 : tmap_values.entrySet()) {

			// Initial
			if (entry_tmap_valuesPrec == null) {
				entry_tmap_valuesPrec = entry_tmap_values3;
				ar_tmapKey = entry_tmap_valuesPrec.getKey().split("-");
				posValuePartialPrec = Integer.parseInt(ar_tmapKey[0]);
				lngValuePartialPrec = Integer.parseInt(ar_tmapKey[1]);
			}
			

			// Pos/lng correnti
			ar_tmapKey = entry_tmap_values3.getKey().split("-");
			posValuePartial = Integer.parseInt(ar_tmapKey[0]);
			lngValuePartial = Integer.parseInt(ar_tmapKey[1]);
			
			// Pos/lng corrente non indica un buco: porto in output
			if (posValuePartial <= posValuePartialPrec + lngValuePartialPrec) {
				tmap_valuesWork.put(entry_tmap_values3.getKey(), entry_tmap_values3.getValue());
				posValuePartialPrec = posValuePartial;
				lngValuePartialPrec = lngValuePartial;
				continue;
			}

			// Pos/lng corrente indica un buco: inserisco un valore di riempimento al valore di default
			if (posValuePartial > posValuePartialPrec + lngValuePartialPrec) {
				posValuePartialHole = posValuePartialPrec + lngValuePartialPrec;
				lngValuePartialHole = posValuePartial - posValuePartialPrec - lngValuePartialPrec;
				keyValuePartialHole = posValuePartialHole + "-" + lngValuePartialHole;
				// Recupero o creazione array list valori
				al_valuePartial = tmap_values.get(keyValuePartialHole);
				if (al_valuePartial == null) {
					al_valuePartial = new ArrayList<String>();
				}
				// Accodamento valore a key pos-lng
				al_valuePartial.add(valueDefault.substring(posValuePartialHole - 1, posValuePartialHole - 1 + lngValuePartialHole));
				tmap_valuesWork.put(keyValuePartialHole, al_valuePartial);
			}
			
			entry_tmap_valuesPrec = entry_tmap_values;
		}
		
		// Tree map di lavoro diventa quella corrente
		tmap_values = (TreeMap<String, ArrayList<String>>) tmap_valuesWork.clone();
		

		///////////////////////////////////////////////////////////////////////////////////////
	    // (5) Gestione inserimento eventuale valore a copertura da prima posizione
		//     Gestione inserimento eventuale valore a copertura ultime posizioni
		///////////////////////////////////////////////////////////////////////////////////////

		// Ultimo valore chiave in ultima posizione
		keyValueLast = tmap_values.lastKey();
		ar_tmapKey = keyValueLast.split("-");
		posValuePartial = Integer.parseInt(ar_tmapKey[0]);
		lngValuePartial = Integer.parseInt(ar_tmapKey[1]);
		
		// Valore da inserire per completamento ultime posizioni
        if ((posValuePartial + lngValuePartial - 1) < dynamicFieldSub.dataItemIdentifierSubField.getDataItem().getSizeBytes()) {
        	posValue = posValuePartial + lngValuePartial;
        	lngValue = dynamicFieldSub.dataItemIdentifierSubField.getDataItem().getSizeBytes() - posValue - 1;
        	keyValue = posValue + "-" + lngValue;
			// Recupero o creazione array list valori
			al_valuePartial = tmap_values.get(keyValue);
			if (al_valuePartial == null) {
				al_valuePartial = new ArrayList<String>();
			}
			// Accodamento valore a key pos-lng
			al_valuePartial.add(valueDefault.substring(posValue - 1, posValue - 1 + lngValuePartial));
        	tmap_values.put(keyValue, al_valuePartial);
		}
		
        // Primo valore chiave in prima posizione
		keyValueFirst = tmap_values.firstKey();
		ar_tmapKey = keyValueFirst.split("-");
		posValuePartial = Integer.parseInt(ar_tmapKey[0]);
		lngValuePartial = Integer.parseInt(ar_tmapKey[1]);
		
		// Valore da inserire da prima posizione
        if (posValuePartial > 1) {
        	posValue = 1;
        	lngValue = posValuePartial - 1;
        	keyValue = posValue + "-" + lngValue;
        	// Accodamento valore a key pos-lng
        	al_valuePartial.add(valueDefault.substring(0, posValue));
        	tmap_values.put(keyValue, al_valuePartial);
		}

        
        //////////////////////////////////////////////////////////////////////////////////////
        // (6) Moltiplicazione valori parziali.
        //     Vengono semplicemente accodati progressivamente i valori a posizione crescente
        //     Valori restituiti in al_valueComplete
        //////////////////////////////////////////////////////////////////////////////////////
        
		al_valueComplete = new ArrayList<String>();
		
		// Scan partial key/values
        for (Entry<String, ArrayList<String>> entryPartial : tmap_values.entrySet()) {
			
        	al_valuePartial = entryPartial.getValue();
        	
        	// Sono i primi valori parziali: vanno inseriti e non moltiplicati
        	if (al_valueComplete.size() == 0) {
        	   for (String value : al_valuePartial) {
					al_valueComplete.add(value);
					continue;			// Next valore parziale stessa pos-lng
			   }
        	   continue;                // next valori parziali successiva pos-lng
			}
        	
        	// E' già stato inserito il primo gruppo di valori parziali: ora si devono moltiplicare
        	al_valueCompleteWork = new ArrayList<String>();
        	
        	// Scan valori completi progressivi fino a questo momento
        	for (String valueComplete2 : al_valueComplete) {
        		// Scan valori parziali da moltiplicare
				for (String valuePartial2 : al_valuePartial) {
					al_valueCompleteWork.add(valueComplete2 + valuePartial2);
				}
			}
        	
        	// L'array list di lavoro diventa quello aggiornato
        	al_valueComplete = al_valueCompleteWork;
  		}
        

		///////////////////////////////////////////////////////////////////////////////////////
	    // (7) Eliminazione duplicati e ordinamento valori
		///////////////////////////////////////////////////////////////////////////////////////
        
		Set<String> set_valueComplete = null;
		set_valueComplete = new HashSet<String>();
		set_valueComplete.addAll(al_valueComplete);
		al_valueComplete.clear();
		al_valueComplete.addAll(set_valueComplete);
		Collections.sort(al_valueComplete);
		
		return al_valueComplete;
	}

	
	
	/*
	 * --------------------------------
	 * Frammentazione valori parziali
	 * --------------------------------
	 * 
	 * Viene fornita la pos lng dei valori da frammentare e la pos successiva 
	 * di valori sovrascritta, che causa la necessità di frammentazione.
	 * Vengono inseriti i valori troncati alla lunghezza minore.
	 * Vengono inseriti i valori oltre posizione troncata minore, per la lunghezza
	 * rimanenete.
	 * 
	 * Vengono gestite le due situazioni, ad esempio:
	 * 
	 * 1) 1-7  frammenta valori precedenti in 1-2 3-5
	 *    3-2
	 * 2) 1-7
	 *    1-8  frammenta valori successivi in 1-7 8-1   
	 * 
	 */
	private void fragmentPartialValues(TreeMap<String, ArrayList<String>> tmapValues			//
									 , TreeMap<String, ArrayList<String>> tmapValuesWork		//
									 , String keyValuePartial									// Key balues to fragment
									 , int posValuePartial										// Pos 1-based
									 , int lngValuePartial										// lng
									 , int PosValuePartialNextGreater							// Pos next 1-based
									 ) {
										
										
        ArrayList<String> al_values = null;
        ArrayList<String> al_valuesTruncated = null;
        ArrayList<String> al_valuesBeyond = null;
		String valuePartialTruncated = "";
		String valuePartialBeyond = "";
        String keyValuePartialTruncated = "";
        String keyValuePartialBeyond = "";
		
		// Inserimento valori troncati
		al_values = tmapValues.get(keyValuePartial);
		al_valuesTruncated = new ArrayList<String> ();
		al_valuesBeyond = new ArrayList<String> ();
		
		// Scan valori da troncare
		for (String valuePartial : al_values) {
			// Valore troncato
			valuePartialTruncated = valuePartial.substring(posValuePartial - 1, PosValuePartialNextGreater - 1);
			al_valuesTruncated.add(valuePartialTruncated);
			// Valore rimanente
			valuePartialBeyond = valuePartial.substring(PosValuePartialNextGreater - 1);
			al_valuesBeyond.add(valuePartialBeyond);
		}
		
		// Chiavi gruppi di valori troncati e rimanenti
		keyValuePartialTruncated = posValuePartial + "-" + (PosValuePartialNextGreater - posValuePartial);
		keyValuePartialBeyond = PosValuePartialNextGreater + "-" + (lngValuePartial - PosValuePartialNextGreater + 1);
		
		// Inserimento nuovo gruppo di valori troncati 
		al_values = tmapValuesWork.get(keyValuePartialTruncated);
		al_values.addAll(al_valuesTruncated);
		tmapValuesWork.put(keyValuePartialTruncated, al_values);
		
		// Inserimento nuovo gruppo di valori rimanenti 
		al_values = tmapValuesWork.get(keyValuePartialBeyond);
		if (al_values == null) {
			al_values = new ArrayList<String>();
		}
		al_values.addAll(al_valuesBeyond);
		tmapValuesWork.put(keyValuePartialBeyond, al_values);
	}


	/*
	 * ---------------------------------------------------------------------
     * Moltiplicazione valori sottocampi per generazione valori finali campi
     * ---------------------------------------------------------------------
     * 
     * Ogni sottocampo del campo in input ha generato dei valori nei path di esecuzione possibili.
     * Ora bisogna ricomporre i valori del campo come moltiplicazione dei valori dei singoli sottocampi.
     * Tutti i valori dei sottocampi sono già della lunghezzsa corretta e la loro somma coincide 
     * esattamente con le dimensioni del campo.
     * 
     * Viene valorizzato ricorsivamente l'ArrayList<String> al_Values con i valori individuati e ricomposti
     * per il campo, come somma dei singoli valori dei sottocampi componenti il campo.
     * 
     */
	private void generateFieldValuesRecursive(LogicDynamicField dynamicField
											, int iSubField
											, ArrayList<String> al_Values
											, String valueParzial
											) {
		
		LogicDynamicFieldSub logicSubField = null;
		String valueField = "";
		String newValueParzial = "";
		
		logicSubField = dynamicField.al_FieldSub.get(iSubField);
		
		// Ultimo subfield: generazione valori complessivi
		if (iSubField == dynamicField.al_FieldSub.size() - 1) {
			// Scan valori ultimo sottocampo e caricamento valori composti complessivi
			for (String valueComplete : logicSubField.al_valueComplete) {
				valueField = valueParzial + valueComplete;
				al_Values.add(valueField);
			}
			return;
		}
		
		// Scan valori sottocampo
		for (int i = 0; i < logicSubField.al_valueComplete.size(); i++) {
			
			newValueParzial = valueParzial + logicSubField.al_valueComplete.get(i);
			
            // Attivazione ricorsiva
            generateFieldValuesRecursive(dynamicField, iSubField + 1, al_Values, newValueParzial);
		}
		
		return;
		
	}


	/* ------------------------------------------------------------------------------------
     * Aggiornamento elenco ultime assegnazioni locali o spreaded in sottocampo.<br>
     * ------------------------------------------------------------------------------------
     * 
     * Nella catena di trasformazione, per le ultime assegnazioni spreaded si tratta di 
     * LAST_SET_BY_COBOL_USING_PARM, LAST_SET_BY_COBOL_LINKAGE, LAST_SET_B_CICS_DFHCOMMAREA
     * LAST_SET_BY_CICS_TWA.
     */
	private boolean updateSubFieldWithLastSetIfAny(LogicWorkProcess ilpw
			                                     , ArrayList<LogicDynamicFieldSubSetting> al_lastSet
			                                      ) {
		
		LogicDynamicFieldSubSetting lastSet = null;

		lastSet = al_lastSet.get(al_lastSet.size() - 1);

		// Ultima assegnazione spreaded da risolvere nei pgm chiamanti
		if (lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE
	    ||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
	    ||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_DFHCOMMAREA
	    ||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_TWA
	    ||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_CSA) {
			// L'ultima assegnazione potrebbe essere duplicata dai processi ricorsivi
			if (!existLastSetTotal(ilpw, lastSet)) {
				ilpw.dynamicFieldSubToSolve.al_lastSetTotal.add(lastSet);
			}
			return true;
		}
		


		return true;
	}

	/* ---------------------------------------------------------------------------
	 * Restituisce true se esiste già una assegnazione local(spreaded equivalente
	 * ---------------------------------------------------------------------------
	 * 
	 * Si tratta di assegnazione local/spreaded relativa allo stesso campo, da pos/lng
	 * mappati nel sottocampo alla stessa pos/lng e dovuti alla stessa
	 * istruzione.
	 * 
	 */
	private boolean existLastSetTotal(LogicWorkProcess ilpw, LogicDynamicFieldSubSetting lastSet) {
		
		// Scan ultime assegnazioni local per il sottocampo
		for (LogicDynamicFieldSubSetting dsfs : ilpw.dynamicFieldSubToSolve.al_lastSetTotal) {
			
			if (dsfs.entityDynamicFieldSetting.getFieldReceiverId().equals(lastSet.entityDynamicFieldSetting.getFieldReceiverId())
			&& 	dsfs.entityDynamicFieldSetting.getFieldReceiverPos() == lastSet.entityDynamicFieldSetting.getFieldReceiverPos()
			&&  dsfs.entityDynamicFieldSetting.getPosInSubField() == lastSet.entityDynamicFieldSetting.getPosInSubField()
			&&  dsfs.entityDynamicFieldSetting.getLngInSubField() == lastSet.entityDynamicFieldSetting.getLngInSubField()
			&& 	dsfs.entityDynamicFieldSetting.getNumInstrSet() == lastSet.entityDynamicFieldSetting.getNumInstrSet()) {
				return true;
			}
		}
		
		return false;
	}


	/*
     * Logging informazioni sull'istruzione dinamica origine
     * 
     * Sono utilizzate le informazioni di ultima assegnazione in ilpw
     * 
      */
	private void logMessageDynamicInstrOrigin(LogicWorkProcess ilpw) {
		
		Instruction instr = null;
		int numInstr = 0;
		String strNumInstr = "";
		String txtInstr = "";
		String field = "";
		String subField = "";
		String txtLngSubField = "";
		int numDefSubField = 0;
		int lngSubField = 0;
		
		// Informazioni su istruzione origine, programma e campo origine
		numInstr = ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getNumInstr();
		strNumInstr = numInstr + "";
		instr = (Instruction) ilpw.programOrigin.instructionProcedure(numInstr);
		txtInstr = instr.getName();
		field = ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdField();
		subField = ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdSubField();
		numDefSubField = ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getNumField();
		lngSubField = ilpw.programOrigin.instructionDataItem(numDefSubField).getSizeBytes();
		txtLngSubField = lngSubField + "";
		logMessage(EnumMessageType.INFORMATION, "MI0036", ilpw.programOrigin.programName, strNumInstr, txtInstr, field, subField, txtLngSubField);

	}


	/**
	 * @return the dynamicFieldToSolve
	 */
	public LogicDynamicField getDynamicFieldToSolve() {
		return dynamicFieldToSolve;
	}

	/**
	 * @param dynamicFieldToSolve the dynamicFieldToSolve to set
	 */
	public void setDynamicFieldToSolve(LogicDynamicField dynamicFieldToSolve) {
		this.dynamicFieldToSolve = dynamicFieldToSolve;
	}




	/**
	 * Restituisce il toolkit X logiche
	 * 
	 * @return the logicTools
	 */
	public LogicTools getLogicTools() {
		return logicTools;
	}




	/**
	 * Imposta il toolkit X logiche
	 * 
	 * @param logicTools the logicTools to set
	 */
	public void setLogicTools(LogicTools logicTools) {
		this.logicTools = logicTools;
	}




	/**
	 * Return the logic process work area
	 * 
	 * @return the ilpw
	 */
	public LogicWorkProcess getIlpw() {
		return ilpw;
	}

	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
}
