package entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import enums.EnumDataItemSystemEnvironment;
import enums.EnumLogicSetMode;
import enums.EnumLogicSetPointerArea;
import enums.EnumObject;

/**
	* Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityDynamicSubFieldSetting (DynamicSubFieldSetting, DSFS)
	* </h1>
	*  <p>
	* Questa classe mappa la tabella FieldSubSetting e descrive una singola trasformazione di un sottocampo,
	* di un campo di un'istruzione dinamica.<br>
	* Il processo di soluzione delle istruzioni dinamiche, a partire dall'istruzione da risolvere,
	* per ogni sottocampo di ogni operando, traccia le trasformazioni nello stesso  programma, 
	* dove il campo di partenza può assumere nomi e formati diversi.
	* E' possibile che il processo si arresti nello stesso programma, in quanto l'ultima assegnazione
	* risulta essere una valore in chiaro esplicito, oppure un campo inizializzato a programma.<br>
	* In alcuni casi l'ultima assegnazione non è risolvibile nello stesso programma, perchè fa
	* riferimento a campi impostati da un programma chiamante oppure memorizzati in un un archivio
	* esterno o altre situazioni similari.<br>
	* In questi casi il processo si arresta e continua sui programmi chiamanti/chiamati.
	* Questa classe descrive pertanto nel dettaglio ogni singola trasformazione del sottocampo originale e di 
	* tutti i suoi derivati. 
	* 
	* 
	* @author Giampietro Zedda
	* @version 1.0.0
	* @since 12/03/2010
	* @see Analyzer
	* @see EntityObject 
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

@Entity(name="DynamicFieldSubSetting")
public class EntityDynamicFieldSubSetting implements Serializable, Cloneable {

	private static final long serialVersionUID = 1L;
	
	///////////////////////////////////////////////////////////////////////
    // Data Items DynamicSubFieldSetting                              
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
	@Column(name="sys")
	private String system = "";            							// Sistema applicativo
	@Id
	@Column(name="subSys")
	private String subSystem = "";         							// Sotto sistema applicativo
	@Id
	@Column(name="idObject")
	private String idObject = "";          							// Nome programma origine
	@Id
	@Column(name="typeObject")
	private EnumObject typeObject = null;       					// Tipologia oggetto programma origine (T0001)
	@Id
	@Column(name="numInstr")
	private int numInstr = 0; 										// Numero istruzione in programma origine
	@Id
	@Column(name="idField")
	private String idField = "";									// Nome campo origine operando da risolvere/risolto
	@Id
	@Column(name="idSubField")
	private String idSubField = "";									// Nome sottocampo origine campo operando  
	@Id
	@Column(name="idPgmSet")
	private String idPgmSet = "";          							// Nome programma dove è avvenuta l'assegnazione
	@Id
	@Column(name="numChain")
	private int numChain = 0; 									    // Numero catena trasformazione (nel programma di set)
	@Id
	@Column(name="progr")
	private int progr = 0; 									        // Progressivo per mantenere l'ordinamento
	@Id
	@Column(name="numInstrSet")
	private int numInstrSet = 0; 									// Numero istruzione di set

	// Data

	@Column(name="numField")
	private int numField = 0;           					        // Numero campo codificato nella struttura di programma
	@Column(name="numSubField")
	private int numSubField = 0;           					        // Numero sottocampo codificato nella struttura di programma

	// Porzione di sottocampo oggetto dell'impostazione 
	@Column(name="posInSubField")
	private int posInSubField = 0;									// Posizione in sottocampo (1-based) inizio valore di questa assegnazione
	@Column(name="lngInSubField")
	private int lngInSubField = 0;									// Lunghezza in sottocampo valorizzata da questa assegnazione
	
	// Informazioni di impostazione, programma, path e modalità di assegnazione e valore se disponibile (literal, campo di tabella)
	@Column(name="numInstrOriginSpreaded")
	private int numInstrOriginSpreaded = 0;       				    // Numero istruzione origine in chiamante (Call/Xctl/Link)
	@Column(name="typeObjectPgmSet")
	private EnumObject typeObjectPgmSet = null;       				// Tipologia oggetto programma dove è avvenuta l'assegnazione (T0001)
	@Column(name="numPath")
	private int numPath = 0;									    // Numero path valido per la catena di assegnazioni
	@Column(name="setMode")
	private EnumLogicSetMode setMode = null;						// Modalita ultima assegnazione campo (T0044)	
	@Column(name="numUsingParm")
	private int numUsingParm = 0;						            // Numero parametro Using a fronte di LAST_SET_BY_COBOL_USING_PARM
	@Column(name="dspFieldInUsingParm")
	private int dspFieldInUsingParm = 0;						    // Displacement campo in parametro Using a fronte di LAST_SET_BY_COBOL_USING_PARM
	@Column(name="dspFieldInLinkageArea")
	private int dspFieldInLinkageArea  = 0;                         // Displacement campo in area di linkage a fronte di LAST_SET_BY_COBOL_LINKAGE/TWA/CSA
	@Column(name="typePointerArea")
	private EnumLogicSetPointerArea typePointerArea = null;			// Tipo area contenente il pointer  a fronte di LAST_SET_BY_COBOL_LINKAGE (T0028)
	@Column(name="dspPointerInLinkageArea")
	private int dspPointerInLinkageArea = 0;						// Displacement pointer in tipo area indicata da typePointerArea
	@Column(name="numUsingParmPointer")
	private int numUsingParmPointer = 0;						    // Numero parametro Using con pointer a fronte di LAST_SET_BY_COBOL_LINKAGE e POINTER_INSIDE_USING_PARM
	@Column(name="dspPointerInUsingParm")
	private int dspPointerInUsingParm = 0;						    // Displacement pointer in parametro Using a fronte di LAST_SET_BY_COBOL_LINKAGE e POINTER_INSIDE_USING_PARM	
	@Column(name="value")
	private String value = "";						                // Valore assegnazione per literal e casi espliciti formattato in lunghezza
	
	// Campo input in assegnazione di trasformazione o campo di ultima assegnazione (di Linkage, ioarea file etc.)
	// Posizione e lunghezza per il sender sono quelli espressi da reference modification (posSnd:lngSnd) se indicato.
    // Posizione e lunghezza sono sempre valorizzati e se non presenti sono inizializzati (1:size(campo sender))
	@Column(name="fieldSenderId")
    private String fieldSenderId = ""; 								// Campo sender, nome				   
	@Column(name="fieldSenderNum")
    private int fieldSenderNum = 0; 							    // Campo sender, numero istruzione di definizione		   
	@Column(name="fieldSenderPos")
    private int fieldSenderPos = 0; 								// Campo sender, da posizione			   
	@Column(name="fieldSenderLng")
    private int fieldSenderLng = 0; 								// Campo sender, per lunghezza	
    
    // Campo output in assegnazione di trasformazione o campo receiver senza trasformazioni. 
    // La posizione è quella iniziale interessata alla trasformazione.
    // La lunghezza è quella del sottocampo origine di cui trovare i valori.
    // Posizione e lunghezza sono sempre valorizzati ed inizializzati nel processo a 1, size(campo receiver)
    // Se l'istruzione Move che ha generato l'assegnazione contiene anche reference modification (pos:lng), l'informazione
    // è utilizzata solo per determinare se il receiver è influenzato dalla trasformazione, ma NON viene memorizzata.
	@Column(name="fieldReceiverId")
    private String fieldReceiverId = ""; 							// Campo receiver, nome			   
	@Column(name="fieldReceiverNum")
    private int fieldReceiverNum = 0; 							    // Campo receiver, numero istruzione di definizione		   
	@Column(name="fieldReceiverPos")
    private int fieldReceiverPos = 0; 								// Campo receiver, da posizione				   
	@Column(name="fieldReceiverLng")
    private int fieldReceiverLng = 0; 								// Campo receiver, per lunghezza	
    
	// Oggetto alla cui ioarea appartiene il campo ultima trasformazione, di cui trovare i valori esternamente (prima assegnazione nella catena)
	@Column(name="idObjExt")
    private String idObjExt = "";          							// Nome oggetto esterno, Ts, Entity, File, Sql table dove trovare i valori
	@Column(name="typeObjExt")
	private EnumObject typeObjExt = null;       					// Tipologia oggetto sender (ENTITY, PHISICAL_FILE, OBJECT_CICS_.., OBJECT_CICS_SYSTEM_FIELD) (T0001)
	@Column(name="objExtSqlTableColumn")
	private String objExtSqlTableColumn = "";						// Sql Table Column name a fronte di LAST_SET_BY_SQL_SELECT
	@Column(name="objExtSystem")
	private EnumDataItemSystemEnvironment objExtSystem = null;   	// Campo di sistema, se OBJECT_CICS_SYSTEM_FIELD, come CICS_EIBTRMID (T0021)
	@Column(name="objExtColField")
	private String objExtColField = "";                	        	// Nome campo/colonna come definito in CopyEntityDefinition 
	@Column(name="objExtIdCopy")
	private String objExtIdCopy = "";                               // Nome copy dove objExtColField è definito, se presente 
	@Column(name="objExtPosCol")
	private int objExtPosCol = 0;							    	// Posizione colonna in record area/copy/TS/TD (se non definito tracciato)
	@Column(name="objExtLengthCol")
	private int objExtLengthCol = 0;							    // Lunghezza colonna in record area/copy/TS/TD (se non definito tracciato)
	
	// Indicatori di soluzione e di valori disponibili
	@Column(name="solvedObjExt")
	private boolean solvedObjExt = false;       					// True = Ts, File etc. risolto per object esterno
	@Column(name="WaitingForExternalData")
	private boolean WaitingForExternalData = false;              	// False = Dati disponibili in DynamicValueExt per object esterno

	
	public EntityDynamicFieldSubSetting() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		typeObjectPgmSet = EnumObject.NOT_ASSIGNED;
		setMode = EnumLogicSetMode.NOT_ASSIGNED;
		typeObjExt = EnumObject.NOT_ASSIGNED;
		typePointerArea = EnumLogicSetPointerArea.NOT_ASSIGNED;
		  			 
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
	}





	/**
	 * @return the numInstr
	 */
	public int getNumInstr() {
		return numInstr;
	}





	/**
	 * @param numInstr the numInstr to set
	 */
	public void setNumInstr(int numInstr) {
		this.numInstr = numInstr;
	}








	/**
	 * @return the idPgmSet
	 */
	public String getIdPgmSet() {
		return idPgmSet;
	}





	/**
	 * @param idPgmSet the idPgmSet to set
	 */
	public void setIdPgmSet(String idPgmSet) {
		this.idPgmSet = idPgmSet;
	}





	/**
	 * @return the posInSubField
	 */
	public int getPosInSubField() {
		return posInSubField;
	}





	/**
	 * @param posInSubField the posInSubField to set
	 */
	public void setPosInSubField(int posInSubField) {
		this.posInSubField = posInSubField;
	}





	/**
	 * @return the lngInSubField
	 */
	public int getLngInSubField() {
		return lngInSubField;
	}





	/**
	 * @param lngInSubField the lngInSubField to set
	 */
	public void setLngInSubField(int lngInSubField) {
		this.lngInSubField = lngInSubField;
	}





	/**
	 * Restituisce il numero istruzione origine in programma chiamante.<br>
	 * <p>
	 * Si tratta del numero istruzione, in un programma chiamante, che
	 * richiama un programma con eventuale passaggio di parametri.<br>
	 * Tipicamente sono istruzioni Call/Cics Xctl/Cics Link)<br>
	 * <p>
	 * @return the numInstrOriginSpreaded
	 */
	public int getNumInstrOriginSpreaded() {
		return numInstrOriginSpreaded;
	}


	/**
	 * Imposta il numero istruzione origine in programma chiamante.<br>
	 * <p>
	 * Si tratta del numero istruzione, in un programma chiamante, che
	 * richiama un programma con eventuale passaggio di parametri.<br>
	 * Tipicamente sono istruzioni Call/Cics Xctl/Cics Link)<br>
	 * <p>
	 * @param numInstrOriginSpreaded the numInstrOriginSpreaded to set
	 */
	public void setNumInstrOriginSpreaded(int numInstrOriginSpreaded) {
		this.numInstrOriginSpreaded = numInstrOriginSpreaded;
	}





	/**
	 * @return the numChain
	 */
	public int getNumChain() {
		return numChain;
	}


	/**
	 * @param numChain the numChain to set
	 */
	public void setNumChain(int numChain) {
		this.numChain = numChain;
	}





	/**
	 * @return the progr
	 */
	public int getProgr() {
		return progr;
	}





	/**
	 * @param progr the progr to set
	 */
	public void setProgr(int progr) {
		this.progr = progr;
	}





	/**
	 * @return the numInstrSet
	 */
	public int getNumInstrSet() {
		return numInstrSet;
	}





	/**
	 * @param numInstrSet the numInstrSet to set
	 */
	public void setNumInstrSet(int numInstrSet) {
		this.numInstrSet = numInstrSet;
	}





	/**
	 * @return the numField
	 */
	public int getNumField() {
		return numField;
	}





	/**
	 * @param numField the numField to set
	 */
	public void setNumField(int numField) {
		this.numField = numField;
	}





	/**
	 * @return the numSubField
	 */
	public int getNumSubField() {
		return numSubField;
	}





	/**
	 * @param numSubField the numSubField to set
	 */
	public void setNumSubField(int numSubField) {
		this.numSubField = numSubField;
	}





	/**
	 * @return the idField
	 */
	public String getIdField() {
		return idField;
	}





	/**
	 * @param idField the idField to set
	 */
	public void setIdField(String idField) {
		this.idField = idField;
	}





	/**
	 * @return the idSubField
	 */
	public String getIdSubField() {
		return idSubField;
	}





	/**
	 * @param idSubField the idSubField to set
	 */
	public void setIdSubField(String idSubField) {
		this.idSubField = idSubField;
	}





	/**
	 * @return the typeObjectPgmSet
	 */
	public EnumObject getTypeObjectPgmSet() {
		return typeObjectPgmSet;
	}





	/**
	 * @param typeObjectPgmSet the typeObjectPgmSet to set
	 */
	public void setTypeObjectPgmSet(EnumObject typeObjectPgmSet) {
		this.typeObjectPgmSet = typeObjectPgmSet;
	}





	/**
	 * @return the numPath
	 */
	public int getNumPath() {
		return numPath;
	}





	/**
	 * @param numPath the numPath to set
	 */
	public void setNumPath(int numPath) {
		this.numPath = numPath;
	}





	/**
	 * @return the setMode
	 */
	public EnumLogicSetMode getSetMode() {
		return setMode;
	}





	/**
	 * @param setMode the setMode to set
	 */
	public void setSetMode(EnumLogicSetMode setMode) {
		this.setMode = setMode;
	}





	/**
	 * Restituisce il numero parametro di procedure using dove è definito
	 * il campo di ultima trasformazione da risolvere esternamente 
	 * nei programmi chiamanti.<br>
	 * Oppure il numero di parametro di call using dove è definito
	 * il campo di ultima trasformazione da risolvere esternamente 
	 * nel programma chiamanto.<br>
	 * Il numero parametro è 0-based.<br>
	 * 
	 * 
	 * @return the numUsingParm
	 */
	public int getNumUsingParm() {
		return numUsingParm;
	}





	/**
	 * Imposta il numero parametro di procedure using dove è definito
	 * il campo di ultima trasformazione da risolvere esternamente 
	 * nei programmi chiamanti.<br>
	 * oppure il numero di parametro di call using dove è definito
	 * il campo di ultima trasformazione da risolvere esternamente 
	 * nel programma chiamanto.
     *
	 * @param numUsingParm the numUsingParm to set
	 */
	public void setNumUsingParm(int numUsingParm) {
		this.numUsingParm = numUsingParm;
	}





	/**
	 * Restituisce il displacement, nel parametro using,
	 * del campo di ultima assegnazione.<br>
	 * <p>
	 * 
	 * @return the dspFieldInUsingParm
	 */
	public int getDspFieldInUsingParm() {
		return dspFieldInUsingParm;
	}





	/**
	 * Imposta il displacement, nel parametro using,
	 * del campo di ultima assegnazione.<br>
	 * <p>
	 * @param dspFieldInUsingParm the dspFieldInUsingParm to set
	 */
	public void setDspFieldInUsingParm(int dspFieldInUsingParm) {
		this.dspFieldInUsingParm = dspFieldInUsingParm;
	}





	/**
	 * Restituisce il numero parametro di using (1-based)
	 * dove è definito il pointer con l'indirizzo dell'area di linkage.<br>
	 * L'area di linkage contiene il campo di ultima assegnazione, di cui
	 * trovare i valori.
	 * 
	 * @return the numUsingParmPointer
	 */
	public int getNumUsingParmPointer() {
		return numUsingParmPointer;
	}





	/**
	 * Imposta il numero parametro di using (1-based)
	 * dove è definito il pointer con l'indirizzo dell'area di linkage.<br>
	 * L'area di linkage contiene il campo di ultima assegnazione, di cui
	 * trovare i valori.
	 * 
	 * @param numUsingParmPointer the numUsingParmPointer to set
	 */
	public void setNumUsingParmPointer(int numUsingParmPointer) {
		this.numUsingParmPointer = numUsingParmPointer;
	}



	/**
	 * Restituisce il displacement del pointer con l'indirizzo dell'area di linkage,
	 * nel parametro di using ottenibile con getNumUsingParmPointer().<br>
	 * L'area di linkage contiene il campo di ultima assegnazione, di cui
	 * trovare i valori.
	 * 
	 * @return the dspPointerInUsingParm
	 */
	public int getDspPointerInUsingParm() {
		return dspPointerInUsingParm;
	}





	/**
	 * Imposta il displacement del pointer con l'indirizzo dell'area di linkage,
	 * nel parametro di using ottenibile con getNumUsingParmPointer().<br>
	 * L'area di linkage contiene il campo di ultima assegnazione, di cui
	 * trovare i valori.
	 * 
	 * @param dspUsingParmPointer the dspUsingParmPointer to set
	 */
	public void setDspPointerInUsingParm(int dspPointerInUsingParm) {
		this.dspPointerInUsingParm = dspPointerInUsingParm;
	}





	/**
	 * Restituisce il displacement nell'area di linkage section,
	 * che contiene il campo di ultima assegnazione, di cui trovare i valori.<br>
	 * L'area di linkage può essere un'area applicativa, la Cics TWA o la Cics CSA.<br>
	 * <p>
	 * L'area di linkage pò essere indirizzata con pointer passati in un parametro using,
	 * in Cics TWA o in Cics CSA.
	 * 
	 * @return the dspFieldInLinkageArea
	 */
	public int getDspFieldInLinkageArea() {
		return dspFieldInLinkageArea;
	}





	/**
	 * Imposta il displacement nell'area di linkage section,
	 * che contiene il campo di ultima assegnazione, di cui trovare i valori.<br>
	 * L'area di linkage può essere un'area applicativa, la Cics TWA o la Cics CSA.<br>
	 * <p>
	 * L'area di linkage pò essere indirizzata con pointer passati in un parametro using,
	 * in Cics TWA o in Cics CSA.
	 * 
	 * @param dspFieldInLinkageArea the dspFieldInLinkageArea to set
	 */
	public void setDspFieldInLinkageArea(int dspFieldInLinkageArea) {
		this.dspFieldInLinkageArea = dspFieldInLinkageArea;
	}





	/**
	 * Restituisce il tipo di area dove è memorizzato il pointer che indirizza
	 * l'area di linkage section, che contiene il campo di ultima assegnazione, 
	 * di cui trovare i valori.<br>
	 * L'area di linkage può essere un'area applicativa, la Cics TWA o la Cics CSA.<br>
	 * 
	 * @return the typePointerArea
	 */
	public EnumLogicSetPointerArea getTypePointerArea() {
		return typePointerArea;
	}





	/**
	 * Imposta il tipo di area dove è memorizzato il pointer che indirizza
	 * l'area di linkage section, che contiene il campo di ultima assegnazione, 
	 * di cui trovare i valori.<br>
	 * L'area di linkage può essere un'area applicativa, la Cics TWA o la Cics CSA.<br>
	 * 
	 * @param typePointerArea the typePointerArea to set
	 */
	public void setTypePointerArea(EnumLogicSetPointerArea typePointerArea) {
		this.typePointerArea = typePointerArea;
	}





	/**
	 * Restituisce il displacemente del pointer che indirizza l'area di linkage section, 
	 * che contiene il campo di ultima assegnazione.
	 * Il displacemeent è relativo al tipo di area ottenibile con  getTypePointerArea()
	 * e può essere una Cics commarea, la Cics TWA o la Cics CSA.<br>
	 * 
	 * @return the dspPointerInLinkageArea
	 */
	public int getDspPointerInLinkageArea() {
		return this.dspPointerInLinkageArea;
	}





	/**
	 * Imposta il displacemente del pointer che indirizza l'area di linkage section, 
	 * che contiene il campo di ultima assegnazione.
	 * Il displacemeent è relativo al tipo di area ottenibile con  getTypePointerArea()
	 * e può essere una Cics commarea, la Cics TWA o la Cics CSA.<br>
	 * 
	 * @param dspPointerInLinkageArea the dspPointerInLinkageArea to set
	 */
	public void setDspPointerInLinkageArea(int dspPointerInLinkageArea) {
		this.dspPointerInLinkageArea = dspPointerInLinkageArea;
	}





	/**
	 * 
	 * Restituisce il valore dell trasformazione.<br>
	 * <p>
	 * Si tratta del valore iniziale di un campo espresso con
	 * literal o costante figurativa oppure di una literal di un campo
	 * sender in una istruzione di assegnazione Move.
	 * 
	 * @return the value
	 */
	public String getValue() {
		return value;
	}





	/**
	 * @param value the value to set
	 */
	public void setValue(String value) {
		this.value = value;
	}





	/**
	 * @return the fieldSenderId
	 */
	public String getFieldSenderId() {
		return fieldSenderId;
	}





	/**
	 * @param fieldSenderId the fieldSenderId to set
	 */
	public void setFieldSenderId(String fieldSenderId) {
		this.fieldSenderId = fieldSenderId;
	}





	/**
	 * @return the fieldSenderNum
	 */
	public int getFieldSenderNum() {
		return fieldSenderNum;
	}





	/**
	 * @param fieldSenderNum the fieldSenderNum to set
	 */
	public void setFieldSenderNum(int fieldSenderNum) {
		this.fieldSenderNum = fieldSenderNum;
	}





	/**
	 * @return the fieldSenderPos
	 */
	public int getFieldSenderPos() {
		return fieldSenderPos;
	}





	/**
	 * @param fieldSenderPos the fieldSenderPos to set
	 */
	public void setFieldSenderPos(int fieldSenderPos) {
		this.fieldSenderPos = fieldSenderPos;
	}





	/**
	 * @return the fieldSenderLng
	 */
	public int getFieldSenderLng() {
		return fieldSenderLng;
	}





	/**
	 * @param fieldSenderLng the fieldSenderLng to set
	 */
	public void setFieldSenderLng(int fieldSenderLng) {
		this.fieldSenderLng = fieldSenderLng;
	}





	/**
	 * @return the fieldReceiverId
	 */
	public String getFieldReceiverId() {
		return fieldReceiverId;
	}





	/**
	 * @param fieldReceiverId the fieldReceiverId to set
	 */
	public void setFieldReceiverId(String fieldReceiverId) {
		this.fieldReceiverId = fieldReceiverId;
	}





	/**
	 * @return the fieldReceiverNum
	 */
	public int getFieldReceiverNum() {
		return fieldReceiverNum;
	}





	/**
	 * @param fieldReceiverNum the fieldReceiverNum to set
	 */
	public void setFieldReceiverNum(int fieldReceiverNum) {
		this.fieldReceiverNum = fieldReceiverNum;
	}





	/**
	 * @return the fieldReceiverPos
	 */
	public int getFieldReceiverPos() {
		return fieldReceiverPos;
	}





	/**
	 * @param fieldReceiverPos the fieldReceiverPos to set
	 */
	public void setFieldReceiverPos(int fieldReceiverPos) {
		this.fieldReceiverPos = fieldReceiverPos;
	}





	/**
	 * @return the fieldReceiverLng
	 */
	public int getFieldReceiverLng() {
		return fieldReceiverLng;
	}





	/**
	 * @param fieldReceiverLng the fieldReceiverLng to set
	 */
	public void setFieldReceiverLng(int fieldReceiverLng) {
		this.fieldReceiverLng = fieldReceiverLng;
	}





	/**
	 * @return the idObjExt
	 */
	public String getIdObjExt() {
		return idObjExt;
	}





	/**
	 * @param idObjExt the idObjExt to set
	 */
	public void setIdObjExt(String idObjExt) {
		this.idObjExt = idObjExt;
	}





	/**
	 * @return the typeObjExt
	 */
	public EnumObject getTypeObjExt() {
		return typeObjExt;
	}





	/**
	 * @param typeObjExt the typeObjExt to set
	 */
	public void setTypeObjExt(EnumObject typeObjExt) {
		this.typeObjExt = typeObjExt;
	}





	/**
	 * @return the objExtSystem
	 */
	public EnumDataItemSystemEnvironment getObjExtSystem() {
		return objExtSystem;
	}





	/**
	 * @param objExtSystem the objExtSystem to set
	 */
	public void setObjExtSystem(EnumDataItemSystemEnvironment objExtSystem) {
		this.objExtSystem = objExtSystem;
	}





	/**
	 * @return the objExtColField
	 */
	public String getObjExtColField() {
		return objExtColField;
	}





	/**
	 * @param objExtColField the objExtColField to set
	 */
	public void setObjExtColField(String objExtColField) {
		this.objExtColField = objExtColField;
	}





	/**
	 * @return the objExtPosCol
	 */
	public int getObjExtPosCol() {
		return objExtPosCol;
	}





	/**
	 * @param objExtPosCol the objExtPosCol to set
	 */
	public void setObjExtPosCol(int objExtPosCol) {
		this.objExtPosCol = objExtPosCol;
	}





	/**
	 * @return the objExtLengthCol
	 */
	public int getObjExtLengthCol() {
		return objExtLengthCol;
	}





	/**
	 * @param objExtLengthCol the objExtLengthCol to set
	 */
	public void setObjExtLengthCol(int objExtLengthCol) {
		this.objExtLengthCol = objExtLengthCol;
	}





	/**
	 * @return the solvedObj
	 */
	public boolean isSolvedObjExt() {
		return solvedObjExt;
	}
	public boolean getSolvedObjExt() {
		return solvedObjExt;
	}





	/**
	 * @param solvedObj the solvedObj to set
	 */
	public void setSolvedObjExt(boolean solvedObjExt) {
		this.solvedObjExt = solvedObjExt;
	}





	/**
	 * @return the WaitingForExternalData
	 */
	public boolean isWaitingForExternalData() {
		return WaitingForExternalData;
	}
	public boolean getWaitingForExternalData() {
		return WaitingForExternalData;
	}


	/**
	 * @param WaitingForExternalData the WaitingForExternalData to set
	 */
	public void setWaitingForExternalData(boolean WaitingForExternalData) {
		this.WaitingForExternalData = WaitingForExternalData;
	}


	/**
	 * @return the objExtIdCopy
	 */
	public String getObjExtIdCopy() {
		return objExtIdCopy;
	}

	/**
	 * @param objExtIdCopy the objExtIdCopy to set
	 */
	public void setObjExtIdCopy(String objExtIdCopy) {
		this.objExtIdCopy = objExtIdCopy;
	}

	/**
	 * @return the objExtSqlTableColumn
	 */
	public String getObjExtSqlTableColumn() {
		return objExtSqlTableColumn;
	}

	/**
	 * @param objExtSqlTableColumn the objExtSqlTableColumn to set
	 */
	public void setObjExtSqlTableColumn(String objExtSqlTableColumn) {
		this.objExtSqlTableColumn = objExtSqlTableColumn;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	public EntityDynamicFieldSubSetting clone() {
		EntityDynamicFieldSubSetting dynamicSubFieldSettingCloned = null;
		try {
			dynamicSubFieldSettingCloned = (EntityDynamicFieldSubSetting) super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
		return dynamicSubFieldSettingCloned;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "EntityDynamicFieldSubSetting [idObject=" + idObject + ", numInstr=" + numInstr + ", idField=" + idField
				+ ", idSubField=" + idSubField + ", idPgmSet=" + idPgmSet + ", numChain=" + numChain + ", numInstrSet="
				+ numInstrSet + ", setMode=" + setMode + ", value=" + value + "]";
	}




	
}
