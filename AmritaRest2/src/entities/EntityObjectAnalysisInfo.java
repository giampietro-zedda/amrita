package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedTable;
import enums.EnumObject;
import enums.EnumSourceType;


/**
	 * Copyright (c) 2009-2011 e-Amrita - ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityObjectAnalysysInfo (OBJI) 
	 * </h1>
	 *  <p>
	 * Questa classe descrive le informazioni dimensionali e di consuntivo analisi a fronte dell'analisi di un oggetto.<br>
	 * Si tratta di informazioni relative al numero di righe sorgente, commenti, righe vuote etc.<br>
	 * oltre a informazioni sulla durata dei vari aspetti dell'analisi.<br>
	 * In particolare viene memorizzato il tempo in millisecondi di parsing puro, dei vari processi post parsing,<br>
	 * di indviduazione metriche e violazione alle stesse oltre ai tempi di aggiornamento su db.<br>
	 * Viene inoltre memorizzato il tempo di scrittura su disco dell'oggetto programma serializzato.<br>
	 * Con queste informazione è possibuile effettuare un tuning specifico di ofni aspetto del processo di<br>
	 * analisi del sorgente.<br>
	 * <p>
	 * In caso di fine anomala con exception, sono disponibili tutte le informazioni di contesto al<br>
	 * momento dell'exception, incluso stack trace, istruzione, in errore, riga e istruzione attiva etc.
	 * <p>
	 * Queste informazioni rappresentano un'estensione di un oggetto analizzato e vengono sempre memorizzate<br>
	 * anche a fronte di exception o in assenza di calcolo delle metriche, che includono anche le informazioni <br>
	 * dimensionali<br>
	 * <p>
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 08/10/2011
	 * @see Analyzer
	 * @see EntityObjectAnalysisInfo 
	 * @see EntityObjectOption
	 * @see EntityIndexItem
	 * @see EntityRelation
	 * @see EntityRelationOrigin
	 * @see EntityCopyEntityDefinition
	 * @see EntityWhereUsedItem
	 * @see EntityMapDescriptor
	 * @see EntityMapItem
	 * @see EntityTagValue
	 * @see EntityDynamicValueExt
	 * @see EntityScopeHeader
	 * @see EntityScopeSection
	 * @see EntityScopeItem
	 * @see EntityScopeChild
	 * @see EntityScopeProgram
	 * @see EntityScopeObject
	 * @see EntityScopeRelation
	 * @see EntityProcessLog
	 * @see EntityMetric
	 * @see EntityTableHeader    
	 * @see EntityTableStructure   
	 * @see EntityTableData 
	 * @see EntityDynamicField
	 * @see EntityDynamicFieldSub
	 * @see EntityDynamicValue
	 * @see EntityDynamicFieldSubSetting
*/

@Entity(name="ObjectAnalysisInfo")
public class EntityObjectAnalysisInfo {
	
	///////////////////////////////////////////////////////////////////////
    // Data Items Object                                                 //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            						// OBJISYST(PK) Sistema applicativo
	@Id
    @Column(name="subSys")
	private String subSystem = "";         						// OBJISUBS(PK) Sotto sistema applicativo
	@Id
    @Column(name="idObject")
	private String idObject = "";          						// OBJIIDOB(PK) Nome oggetto (es. programma, copy,..)
	@Id
    @Column(name="typeObject")
	private EnumObject typeObject = null;       				// OBJITYPO(PK) Tipologia oggetto (T0001)

    @Column(name="typeSource")
	private EnumSourceType typeSource; 				            // OBJTTYPS     Tipologia sorgente (T0002)			
	
	// Data 
	                                                            // ** Info dimensionali ***
    @Column(name="sizeBytes")
	private long sizeBytes = 0; 								// OBJISIZE     Dimensioni in bytes del file (esclusi caratteri di controllo)
    @Column(name="numRowsTot")
	private int numRowsTot = 0; 								// OBJISLOT     Numero totale righe totali incluse quelle vuote
    @Column(name="numRowsCommTot")
	private int numRowsCommTot = 0; 							// OBJICLOT     Numero totale righe commento
    @Column(name="numRowsEmptyTot")
	private int numRowsEmptyTot = 0; 							// OBJIELOT     Numero totale righe vuote (carriage return o tutti spaces)
    @Column(name="numRowsBlankTot")
	private int numRowsBlankTot = 0; 							// OBJIBLOT     Numero totale righe vuote (space a 7-72 X Cobol)
    @Column(name="numRowsCommData")
	private int numRowsCommData = 0; 							// OBJICLOD     Numero commenti in Data Division Cobol
    @Column(name="numRowsData")
	private int numRowsData = 0; 							    // OBJISLOD     Numero righe sorgente in procedure division Cobol
    @Column(name="numRowsEmptyData")
	private int numRowsEmptyData = 0; 							// OBJIELOD     Numero totale righe vuote in Data Division Cobol (carriage return)
    @Column(name="numRowsBlankData")
	private int numRowsBlankData = 0; 							// OBJIBLOD     Numero totale righe vuote in Data Division Cobol (space a 7-72)
    @Column(name="numRowsCommProc")
	private int numRowsCommProc = 0; 							// OBJICLOP     Numero commenti in Procedure Division Cobol
    @Column(name="numRowsProc")
	private int numRowsProc = 0; 							    // OBJISLOP     Numero righe sorgente in procedure division
    @Column(name="numRowsEmptyProc")
	private int numRowsEmptyProc = 0; 							// OBJIELOP     Numero totale righe vuote in Procedure Division Cobol (carriage return)
    @Column(name="numRowsBlankProc")
	private int numRowsBlankProc = 0; 							// OBJIBLOP     Numero totale righe vuote in Procedure Division Cobol(space a 7-72)
    @Column(name="numStmtProc")
	private int numStmtProc = 0; 								// OBJISTMP     Numero totale statements in Procedure Division
    @Column(name="numDefData")
	private int numDefData = 0; 								// OBJIDEFD     Numero totale definizioni in Data Division, incluse quelle nei copy
    @Column(name="numDefDataProgram")
	private int numDefDataProgram = 0; 							// OBJIDEFP     Numero totale definizioni in Data Division, nel source del programma
    @Column(name="numDefDataCopy")
	private int numDefDataCopy = 0; 							// OBJIDEFC     Numero totale definizioni in Data Division, nei copy richiamati
	
	                                                            // ** Info data, ora conteggi updates e tempi di esecuzione **
    @Column(name="dtAnalysis")
	private String dtAnalysis = "";	    						// OBJIDTAN     Data analisi
    @Column(name="tmStartAnalysis")
	private String tmStartAnalysis = "";	    				// OBJITMSA     Ora inizio analisi HHMMSSCC	
    @Column(name="tmEndAnalysis")
	private String tmEndAnalysis = "";	    					// OBJITMEA     Ora Fine analisi HHMMSSCC	
    @Column(name="msElapsedTot")
    private long msElapsedTot = 0;                              // OBJIMSTO     ms elapsed di analisi totali
    @Column(name="msParsing")
    private long msParsing = 0;                               	// OBJIMSPA     ms di parsing sorgente
    @Column(name="msPostParsingOperations")
    private long msPostParsingOperations = 0;                   // OBJIMSPP     ms di operazioni finali post parsing sorgente
    @Column(name="msGraphCreation")
    private long msGraphCreation = 0;                    		// OBJIMSGH     ms di creazione grafi e sottografi di programma
    @Column(name="msDynamicCodeSolving")
    private long msDynamicCodeSolving = 0;                    	// OBJIMSDY     ms di soluzione codice dinamico del programma
    @Column(name="msMetricComputing")
    private long msMetricComputing = 0;                         // OBJIMSME     ms elapsed di calcolo metriche
    @Column(name="msViolationDetecting")
    private long msViolationDetecting = 0;                      // OBJIMSVI     ms di individuazione violazioni alle metriche
    @Column(name="msSerializationPgm")
    private long msSerializationPgm = 0;                       	// OBJIMSSE     ms di serializzazione pgm su disco
    @Column(name="msDbDelete")
    private long msDbDelete = 0;                       			// OBJIMSDD     ms totale di delete data base
    @Column(name="msDbUpdate")
    private long msDbUpdate = 0;                       			// OBJIMSDB     ms totale di update data base
    @Column(name="msUpdObject")
    private long msUpdObject = 0;                               //              ms totale Object aggiornati
    @Column(name="msInsRelation")
    private long msInsRelation = 0;                             //              ms totale relazioni inserite
    @Column(name="msInsRelationOrigin")
    private long msInsRelationOrigin = 0;                       //              ms totale relazioni inserite
    @Column(name="msInsWhereUsed")
    private long msInsWhereUsed = 0;                            //              ms totale wehereUsed inserite
    @Column(name="msInsCopyEntity")
    private long msInsCopyEntity = 0;                           //              ms totale copyEntityDefinition inserite
    @Column(name="msInsMetric")
    private long msInsMetric = 0;                               //              ms totale Metric inserite
    @Column(name="msInsMetricViolation")
    private long msInsMetricViolation = 0;                      //              ms totale MetricViolation inserite
    @Column(name="msInsDynamicField")
    private long msInsDynamicField = 0;                         //              ms totale DynamicField inserite
    @Column(name="cntInsObject")
    private long cntInsObject = 0;                              //              Counter Object inseriti
    @Column(name="cntUpdObject")
    private long cntUpdObject = 0;                              //              Counter Object aggiornati
    @Column(name="cntInsRelation")
    private long cntInsRelation = 0;                            //              Counter relazioni inserite
    @Column(name="cntInsRelationOrigin")
    private long cntInsRelationOrigin = 0;                      //              Counter relazioni origine inserite
    @Column(name="cntInsWhereUsed")
    private long cntInsWhereUsed = 0;                           //              Counter wehereUsed inserite
    @Column(name="cntInsCopyEntity")
    private long cntInsCopyEntity = 0;                          //              Counter copyEntityDefinition inserite
    @Column(name="cntInsMetric")
    private long cntInsMetric = 0;                              //              Counter Metric inserite
    @Column(name="cntInsMetricViolation")
    private long cntInsMetricViolation = 0;                     //              Counter MetricViolation inserite
    @Column(name="cntInsDynamicField")
    private long cntInsDynamicField = 0;                        //              Counter DynamicField inserite
	
																// ** Info tipologie errori di parsing presenti **
    @Column(name="withException")
    private boolean withException = false;                      //              Analisi terminata con exception
    @Column(name="withAnyCobolParsingErrors")
    private boolean withAnyCobolParsingErrors = false;          // OBJICOBE     True indica qualche istruzione Cobol con errori di parsing
    @Column(name="withAnySqlParsingErrors")
    private boolean withAnySqlParsingErrors = false;            // OBJISQLE     True indica qualche istruzione Sql   con errori di parsing
    @Column(name="withAnyCicsParsingErrors")
    private boolean withAnyCicsParsingErrors = false;           // OBJICICE     True indica qualche istruzione Cics  con errori di parsing
    @Column(name="withAnyDL1ParsingErrors")
    private boolean withAnyDL1ParsingErrors = false;            // OBJIDL1E     True indica qualche istruzione DL1   con errori di parsing
    
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityObjectAnalysisInfo() {
		super();
		
		typeObject = EnumObject.NOT_ASSIGNED;
		typeSource = EnumSourceType.NOT_ASSIGNED;
	}
	
	
	/**
	 * @return the system
	 */
	public String getSystem() {
		return system;
	}
	/**
	 * @param system the system to set
	 */
	public void setSystem(String system) {
		this.system = system;
	}
	/**
	 * @return the subSystem
	 */
	public String getSubSystem() {
		return subSystem;
	}
	/**
	 * @param subSystem the subSystem to set
	 */
	public void setSubSystem(String subSystem) {
		this.subSystem = subSystem;
	}
	/**
	 * @return the idObject
	 */
	public String getIdObject() {
		return idObject;
	}
	/**
	 * @param idObject the idObject to set
	 */
	public void setIdObject(String idObject) {
		this.idObject = idObject;
	}
	/**
	 * @return the typeObject
	 */
	public EnumObject getTypeObject() {
		return typeObject;
	}
	/**
	 * @param typeObject the typeObject to set
	 */
	public void setTypeObject(EnumObject typeObject) {
		this.typeObject = typeObject;
		if (typeObject == EnumObject.OBJECT_COPY_COBOL_DATA ) {
			this.typeSource = EnumSourceType.COBOL_COPY_DATA;
		} else if (typeObject == EnumObject.OBJECT_COPY_COBOL_PROC) {
			this.typeSource = EnumSourceType.COBOL_COPY_PROC;
		} else if (typeObject == EnumObject.OBJECT_COPY_COBOL_ENV) {
			this.typeSource = EnumSourceType.COBOL_COPY_ENV;
		} else if (typeObject == EnumObject.OBJECT_PGM_COBOL) {
			this.typeSource = EnumSourceType.COBOL_PROGRAM;
		} else if (typeObject == EnumObject.OBJECT_CICS_BMS_SOURCE) {
			this.typeSource = EnumSourceType.CICS_BMS;
		} else if (typeObject == EnumObject.OBJECT_SQL_SCRIPT
				|| typeObject == EnumObject.OBJECT_ENTITY_SQL) {
			this.typeSource = EnumSourceType.SQL_SCRIPT;
		} else {
			this.typeSource = EnumSourceType.NOT_ASSIGNED;
		}	
	}
	
	
	/**
	 * @return the typeSource
	 */
	public EnumSourceType getTypeSource() {
		return typeSource;
	}
	/**
	 * @param typeSource the typeSource to set
	 */
	public void setTypeSource(EnumSourceType typeSource) {
		this.typeSource = typeSource;
	}

	
	
	
	/**
	 * @return the sizeBytes
	 */
	public long getSizeBytes() {
		return sizeBytes;
	}


	/**
	 * @param sizeBytes the sizeBytes to set
	 */
	public void setSizeBytes(long sizeBytes) {
		this.sizeBytes = sizeBytes;
	}


	/**
	 * @return the numRowsCommTot
	 */
	public int getNumRowsCommTot() {
		return numRowsCommTot;
	}


	/**
	 * @param numRowsCommTot the numRowsCommTot to set
	 */
	public void setNumRowsCommTot(int numRowsCommTot) {
		this.numRowsCommTot = numRowsCommTot;
	}


	/**
	 * @return the numRowsTot
	 */
	public int getNumRowsTot() {
		return numRowsTot;
	}


	/**
	 * @param numRowsTot the numRowsTot to set
	 */
	public void setNumRowsTot(int numRowsTot) {
		this.numRowsTot = numRowsTot;
	}


	/**
	 * @return the numRowsEmptyTot
	 */
	public int getNumRowsEmptyTot() {
		return numRowsEmptyTot;
	}


	/**
	 * @param numRowsEmptyTot the numRowsEmptyTot to set
	 */
	public void setNumRowsEmptyTot(int numRowsEmptyTot) {
		this.numRowsEmptyTot = numRowsEmptyTot;
	}


	/**
	 * @return the numRowsBlankTot
	 */
	public int getNumRowsBlankTot() {
		return numRowsBlankTot;
	}


	/**
	 * @param numRowsBlankTot the numRowsBlankTot to set
	 */
	public void setNumRowsBlankTot(int numRowsBlankTot) {
		this.numRowsBlankTot = numRowsBlankTot;
	}


	/**
	 * @return the numRowsCommData
	 */
	public int getNumRowsCommData() {
		return numRowsCommData;
	}


	/**
	 * @param numRowsCommData the numRowsCommData to set
	 */
	public void setNumRowsCommData(int numRowsCommData) {
		this.numRowsCommData = numRowsCommData;
	}


	/**
	 * @return the numRowsData
	 */
	public int getNumRowsData() {
		return numRowsData;
	}


	/**
	 * @param numRowsData the numRowsData to set
	 */
	public void setNumRowsData(int numRowsData) {
		this.numRowsData = numRowsData;
	}


	/**
	 * @return the numRowsEmptyData
	 */
	public int getNumRowsEmptyData() {
		return numRowsEmptyData;
	}


	/**
	 * @param numRowsEmptyData the numRowsEmptyData to set
	 */
	public void setNumRowsEmptyData(int numRowsEmptyData) {
		this.numRowsEmptyData = numRowsEmptyData;
	}


	/**
	 * @return the numRowsBlankData
	 */
	public int getNumRowsBlankData() {
		return numRowsBlankData;
	}


	/**
	 * @param numRowsBlankData the numRowsBlankData to set
	 */
	public void setNumRowsBlankData(int numRowsBlankData) {
		this.numRowsBlankData = numRowsBlankData;
	}


	/**
	 * @return the numRowsCommProc
	 */
	public int getNumRowsCommProc() {
		return numRowsCommProc;
	}


	/**
	 * @param numRowsCommProc the numRowsCommProc to set
	 */
	public void setNumRowsCommProc(int numRowsCommProc) {
		this.numRowsCommProc = numRowsCommProc;
	}


	/**
	 * @return the numRowsProc
	 */
	public int getNumRowsProc() {
		return numRowsProc;
	}


	/**
	 * @param numRowsProc the numRowsProc to set
	 */
	public void setNumRowsProc(int numRowsProc) {
		this.numRowsProc = numRowsProc;
	}


	/**
	 * @return the numRowsEmptyProc
	 */
	public int getNumRowsEmptyProc() {
		return numRowsEmptyProc;
	}


	/**
	 * @param numRowsEmptyProc the numRowsEmptyProc to set
	 */
	public void setNumRowsEmptyProc(int numRowsEmptyProc) {
		this.numRowsEmptyProc = numRowsEmptyProc;
	}


	/**
	 * @return the numRowsBlankProc
	 */
	public int getNumRowsBlankProc() {
		return numRowsBlankProc;
	}


	/**
	 * @param numRowsBlankProc the numRowsBlankProc to set
	 */
	public void setNumRowsBlankProc(int numRowsBlankProc) {
		this.numRowsBlankProc = numRowsBlankProc;
	}


	/**
	 * @return the numStmtProc
	 */
	public int getNumStmtProc() {
		return numStmtProc;
	}


	/**
	 * @param numStmtProc the numStmtProc to set
	 */
	public void setNumStmtProc(int numStmtProc) {
		this.numStmtProc = numStmtProc;
	}




	/**
	 * @return the numDefData
	 */
	public int getNumDefData() {
		return numDefData;
	}


	/**
	 * @param numDefData the numDefData to set
	 */
	public void setNumDefData(int numDefData) {
		this.numDefData = numDefData;
	}


	/**
	 * @return the numDefDataProgram
	 */
	public int getNumDefDataProgram() {
		return numDefDataProgram;
	}


	/**
	 * @param numDefDataProgram the numDefDataProgram to set
	 */
	public void setNumDefDataProgram(int numDefDataProgram) {
		this.numDefDataProgram = numDefDataProgram;
	}


	/**
	 * @return the numDefDataCopy
	 */
	public int getNumDefDataCopy() {
		return numDefDataCopy;
	}


	/**
	 * @param numDefDataCopy the numDefDataCopy to set
	 */
	public void setNumDefDataCopy(int numDefDataCopy) {
		this.numDefDataCopy = numDefDataCopy;
	}


	/**
	 * @return the dtAnalysis
	 */
	public String getDtAnalysis() {
		return dtAnalysis;
	}


	/**
	 * @param dtAnalysis the dtAnalysis to set
	 */
	public void setDtAnalysis(String dtAnalysis) {
		this.dtAnalysis = dtAnalysis;
	}


	/**
	 * @return the tmStartAnalysis
	 */
	public String getTmStartAnalysis() {
		return tmStartAnalysis;
	}


	/**
	 * @param tmStartAnalysis the tmStartAnalysis to set
	 */
	public void setTmStartAnalysis(String tmStartAnalysis) {
		this.tmStartAnalysis = tmStartAnalysis;
	}


	/**
	 * @return the tmEndAnalysis
	 */
	public String getTmEndAnalysis() {
		return tmEndAnalysis;
	}


	/**
	 * @param tmEndAnalysis the tmEndAnalysis to set
	 */
	public void setTmEndAnalysis(String tmEndAnalysis) {
		this.tmEndAnalysis = tmEndAnalysis;
	}


	/**
	 * @return the msElapsedTot
	 */
	public long getMsElapsedTot() {
		return msElapsedTot;
	}


	/**
	 * @param msElapsedTot the msElapsedTot to set
	 */
	public void setMsElapsedTot(int msElapsedTot) {
		this.msElapsedTot = msElapsedTot;
	}


	/**
	 * @return the msParsing
	 */
	public long getMsParsing() {
		return msParsing;
	}


	/**
	 * @param msParsing the msParsing to set
	 */
	public void setMsParsing(int msParsing) {
		this.msParsing = msParsing;
	}


	/**
	 * @return the msPostParsingOperations
	 */
	public long getMsPostParsingOperations() {
		return msPostParsingOperations;
	}


	/**
	 * @param msPostParsingOperations the msPostParsingOperations to set
	 */
	public void setMsPostParsingOperations(int msPostParsingOperations) {
		this.msPostParsingOperations = msPostParsingOperations;
	}


	/**
	 * @return the msGraphCreation
	 */
	public long getMsGraphCreation() {
		return msGraphCreation;
	}


	/**
	 * @param msGraphCreation the msGraphCreation to set
	 */
	public void setMsGraphCreation(int msGraphCreation) {
		this.msGraphCreation = msGraphCreation;
	}


	/**
	 * @return the msDynamicCodeSolving
	 */
	public long getMsDynamicCodeSolving() {
		return msDynamicCodeSolving;
	}


	/**
	 * @param msDynamicCodeSolving the msDynamicCodeSolving to set
	 */
	public void setMsDynamicCodeSolving(int msDynamicCodeSolving) {
		this.msDynamicCodeSolving = msDynamicCodeSolving;
	}


	/**
	 * @return the msMetricComputing
	 */
	public long getMsMetricComputing() {
		return msMetricComputing;
	}


	/**
	 * @param msMetricComputing the msMetricComputing to set
	 */
	public void setMsMetricComputing(int msMetricComputing) {
		this.msMetricComputing = msMetricComputing;
	}


	/**
	 * @return the msViolationDetecting
	 */
	public long getMsViolationDetecting() {
		return msViolationDetecting;
	}


	/**
	 * @param msViolationDetecting the msViolationDetecting to set
	 */
	public void setMsViolationDetecting(int msViolationDetecting) {
		this.msViolationDetecting = msViolationDetecting;
	}


	/**
	 * @return the msDbDelete
	 */
	public long getMsDbDelete() {
		return msDbDelete;
	}


	/**
	 * @param msDbDelete the msDbDelete to set
	 */
	public void setMsDbDelete(int msDbDelete) {
		this.msDbDelete = msDbDelete;
	}


	/**
	 * @return the msDbUpdate
	 */
	public long getMsDbUpdate() {
		return msDbUpdate;
	}


	/**
	 * @param msDbUpdate the msDbUpdate to set
	 */
	public void setMsDbUpdate(int msDbUpdate) {
		this.msDbUpdate = msDbUpdate;
	}


	/**
	 * @return the msSerializationPgm
	 */
	public long getMsSerializationPgm() {
		return msSerializationPgm;
	}


	/**
	 * @param msSerializationPgm the msSerializationPgm to set
	 */
	public void setMsSerializationPgm(int msSerializationPgm) {
		this.msSerializationPgm = msSerializationPgm;
	}


	
	
	/**
	 * @return the withAnyCobolParsingErrors
	 */
	public boolean getWithAnyCobolParsingErrors() {
		return withAnyCobolParsingErrors;
	}


	/**
	 * @param withAnyCobolParsingErrors the withAnyCobolParsingErrors to set
	 */
	public void setWithAnyCobolParsingErrors(boolean withAnyCobolParsingErrors) {
		this.withAnyCobolParsingErrors = withAnyCobolParsingErrors;
	}


	/**
	 * @return the withAnySqlParsingErrors
	 */
	public boolean getWithAnySqlParsingErrors() {
		return withAnySqlParsingErrors;
	}


	/**
	 * @param withAnySqlParsingErrors the withAnySqlParsingErrors to set
	 */
	public void setWithAnySqlParsingErrors(boolean withAnySqlParsingErrors) {
		this.withAnySqlParsingErrors = withAnySqlParsingErrors;
	}


	/**
	 * @return the withAnyCicsParsingErrors
	 */
	public boolean getWithAnyCicsParsingErrors() {
		return withAnyCicsParsingErrors;
	}


	/**
	 * @param withAnyCicsParsingErrors the withAnyCicsParsingErrors to set
	 */
	public void setWithAnyCicsParsingErrors(boolean withAnyCicsParsingErrors) {
		this.withAnyCicsParsingErrors = withAnyCicsParsingErrors;
	}


	/**
	 * @return the withAnyDL1ParsingErrors
	 */
	public boolean getWithAnyDL1ParsingErrors() {
		return withAnyDL1ParsingErrors;
	}


	/**
	 * @param withAnyDL1ParsingErrors the withAnyDL1ParsingErrors to set
	 */
	public void setWithAnyDL1ParsingErrors(boolean withAnyDL1ParsingErrors) {
		this.withAnyDL1ParsingErrors = withAnyDL1ParsingErrors;
	}




	public long getCntInsObject() {
		return cntInsObject;
	}


	public void setCntInsObject(long cntInsObject) {
		this.cntInsObject = cntInsObject;
	}


	public long getCntUpdObject() {
		return cntUpdObject;
	}


	public void setCntUpdObject(long cntUpdObject) {
		this.cntUpdObject = cntUpdObject;
	}


	public long getCntInsRelation() {
		return cntInsRelation;
	}


	public void setCntInsRelation(long cntInsRelation) {
		this.cntInsRelation = cntInsRelation;
	}


	public long getCntInsWhereUsed() {
		return cntInsWhereUsed;
	}


	public void setCntInsWhereUsed(long cntInsWhereUsed) {
		this.cntInsWhereUsed = cntInsWhereUsed;
	}


	public long getCntInsCopyEntity() {
		return cntInsCopyEntity;
	}


	public void setCntInsCopyEntity(long cntInsCopyEntity) {
		this.cntInsCopyEntity = cntInsCopyEntity;
	}


	public long getCntInsMetric() {
		return cntInsMetric;
	}


	public void setCntInsMetric(long cntInsMetric) {
		this.cntInsMetric = cntInsMetric;
	}


	public long getCntInsMetricViolation() {
		return cntInsMetricViolation;
	}


	public void setCntInsMetricViolation(long cntInsMetricViolation) {
		this.cntInsMetricViolation = cntInsMetricViolation;
	}


	public long getCntInsDynamicField() {
		return cntInsDynamicField;
	}


	public void setCntInsDynamicField(long cntInsDynamicField) {
		this.cntInsDynamicField = cntInsDynamicField;
	}


	public long getMsUpdObject() {
		return msUpdObject;
	}


	public void setMsUpdObject(long msUpdObject) {
		this.msUpdObject = msUpdObject;
	}


	public long getMsInsRelation() {
		return msInsRelation;
	}


	public void setMsInsRelation(long msInsRelation) {
		this.msInsRelation = msInsRelation;
	}


	public long getMsInsWhereUsed() {
		return msInsWhereUsed;
	}


	public void setMsInsWhereUsed(long msInsWhereUsed) {
		this.msInsWhereUsed = msInsWhereUsed;
	}


	public long getMsInsCopyEntity() {
		return msInsCopyEntity;
	}


	public void setMsInsCopyEntity(long msInsCopyEntity) {
		this.msInsCopyEntity = msInsCopyEntity;
	}


	public long getMsInsMetric() {
		return msInsMetric;
	}


	public void setMsInsMetric(long msInsMetric) {
		this.msInsMetric = msInsMetric;
	}


	public long getMsInsMetricViolation() {
		return msInsMetricViolation;
	}


	public void setMsInsMetricViolation(long msInsMetricViolation) {
		this.msInsMetricViolation = msInsMetricViolation;
	}


	public long getMsInsDynamicField() {
		return msInsDynamicField;
	}


	public void setMsInsDynamicField(long msInsDynamicField) {
		this.msInsDynamicField = msInsDynamicField;
	}


	public boolean getWithException() {
		return withException;
	}


	public void setWithException(boolean withException) {
		this.withException = withException;
	}


	public void setMsElapsedTot(long msElapsedTot) {
		this.msElapsedTot = msElapsedTot;
	}


	public void setMsParsing(long msParsing) {
		this.msParsing = msParsing;
	}


	public void setMsPostParsingOperations(long msPostParsingOperations) {
		this.msPostParsingOperations = msPostParsingOperations;
	}


	public void setMsGraphCreation(long msGraphCreation) {
		this.msGraphCreation = msGraphCreation;
	}


	public void setMsDynamicCodeSolving(long msDynamicCodeSolving) {
		this.msDynamicCodeSolving = msDynamicCodeSolving;
	}


	public void setMsMetricComputing(long msMetricComputing) {
		this.msMetricComputing = msMetricComputing;
	}


	public void setMsViolationDetecting(long msViolationDetecting) {
		this.msViolationDetecting = msViolationDetecting;
	}


	public void setMsSerializationPgm(long msSerializationPgm) {
		this.msSerializationPgm = msSerializationPgm;
	}


	public void setMsDbDelete(long msDbDelete) {
		this.msDbDelete = msDbDelete;
	}


	public void setMsDbUpdate(long msDbUpdate) {
		this.msDbUpdate = msDbUpdate;
	}


	public long getMsInsRelationOrigin() {
		return msInsRelationOrigin;
	}


	public void setMsInsRelationOrigin(long msInsRelationOrigin) {
		this.msInsRelationOrigin = msInsRelationOrigin;
	}


	public long getCntInsRelationOrigin() {
		return cntInsRelationOrigin;
	}


	public void setCntInsRelationOrigin(long cntInsRelationOrigin) {
		this.cntInsRelationOrigin = cntInsRelationOrigin;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return  typeObject.toString() + " IdObject:" + idObject;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityObjectAnalysisInfo objEntityObject = null;
		
		objEntityObject = (EntityObjectAnalysisInfo) obj;
		 
		return this.system.equals(objEntityObject.system)
		 &&    this.subSystem.equals(objEntityObject.subSystem)
		 &&    this.idObject.equals(objEntityObject.idObject)
		 &&    this.typeObject == objEntityObject.typeObject;
	}





}
