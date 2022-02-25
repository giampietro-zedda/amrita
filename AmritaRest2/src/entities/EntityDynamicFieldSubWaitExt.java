package entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumDataItemSystemEnvironment;
import enums.EnumObject;

/**
	* Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityDynamicWaitingExternal (DynamicWaitingExternal, DWTE)
	* </h1>
	*  <p>
	* Questa classe mappa la tabella <tt>DynamicWaitingExternal</tt> e descrive una risorsa esterna necessaria 
	* al completamento della soluzione di un'istruzione dinamica.<br>
	* <p>
	* La tabella <tt>DynamicWaitingExternal</tt> è collegata alla tabella <tt>EntityDynamicField</tt>. A fronte di una riga 
	* descrivinte un'istruzione dinamica e un campo, possono essere presenti righe multiple in
	* DynamicWaitingExternal a indicare nomi di sorgenti fisiche esterne di dati.<br>
	* <p>
	* Si tratta del nome di un file fisico, di un oggetto Cics come terminale o transazione,
	* una tabella db2, una coda Ts etc.<br>
	* Il processo di soluzione delle istruzioni dinamiche, quando individua la sorgente ultima di
	* un campo o una posizione in un media esterno al programma, individua il nome fisico della risorsa 
	* (come il nome fisico del del file, dal jcl) e memorizza le informazioni in questa tabella.<br>
	* <p>
	* Il tipo di media esterno e il nome fisico della risorsa esterna sono utilizzati come chiave
	* per accedere agli effettivi valori della colonna esterna, nella tabella <tt>DynamicValueExternal</tt>.<br>
	* <p>
	* Per completare il processo di soluzione dell'istruzione dinamica è necessario inserire, nella
	* tabella <tt>DynamicValueExternal</tt>, i valori della colonna esterna descritti da <tt>DynamicWaitingExternal</tt>.<br>
	* <p>
	* 
	* 
	* @author Giampietro Zedda
	* @version 1.0.0
	* @since 05/06/2011
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
	 * @see EntityDynamicFieldSubWaitExt
	 * @see EntityDynamicFieldSub
	 * @see EntityDynamicValue
	 * @see EntityDynamicFieldSubSetting
*/

@Entity(name="DynamicFieldSubWaitExt")
public class EntityDynamicFieldSubWaitExt implements Serializable {

	private static final long serialVersionUID = 1L;
	
	///////////////////////////////////////////////////////////////////////
    // Data Items DynamicWaitingExternal                                                         
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
	@Column(name="sys")
	private String system = "";            							     // Sistema applicativo
	@Id
	@Column(name="subSys")
	private String subSystem = "";         							 	 // Sotto sistema applicativo
	@Id
	@Column(name="idObject")
	private String idObject = "";          							 	 // Nome programma  
	@Id
	@Column(name="typeObject")
	private EnumObject typeObject = null;       					 	 // Tipologia oggetto programma (T0001)
	@Id
	@Column(name="numInstr")
	private int numInstr = 0; 										 	 // Numero istruzione dinamica
	@Id
	@Column(name="idField")
	private String idField = "";									 	 // Nome operando/campo da risolvere/risolto
	@Id
	@Column(name="idSubField")
	private String idSubField = "";									 	 // Nome sottocampo elementare operando/campo
	@Id
	@Column(name="numProgr")
	private int numProgr = 0;                     			         	 // Numero sequenza progressivo media esterno in waiting (per EIBTRMID e EIBTRNID su cics diversi
	
	// Data
	@Column(name="typeObjectExternal")
	private EnumObject typeObjectExternal  = null;       			 	 // Tipologia oggetto waiting T001 (ENTITY, PHISICAL_FILE, OBJECT_CICS_.., OBJECT_CICS_SYSTEM_FIELD) (T0001)
	@Column(name="idObjectExternal")
	private String idObjectExternal = "";          			         	 // Nome tabella/Coda Ts/DDName esterno/ ...
	@Column(name="dsnameExternal")
	private String dsnameExternal = "";          			             // Nome fisico esterno, rappresenta il Dsname trovato nel JCL	
	@Column(name="typeSystemFieldExternal")
	private EnumDataItemSystemEnvironment typeSystemFieldExternal = null;// Tipo campo di sistema waiting, se OBJECT_CICS_SYSTEM_FIELD, come CICS_EIBTRMID (T0021)
	@Column(name="cicsNameExternal")
	private String cicsNameExternal = "";          					 	 // Nome Cics waiting valido se CICS_EIBTRMID e CICS_EIBTRNID
	@Column(name="idFieldExternal")
	private String idFieldExternal = "";                			 	 // Nome campo/colonna waiting come definito in CopyEntityDefinition o space
	@Column(name="posColumnExternal")
	private int posColumnExternal = 0;							     	 // Posizione colonna waiting in record area/copy/TS/TD (se non definito tracciato)
	@Column(name="lengthColumnExternal")
	private int lengthColumnExternal = 0;							 	 // Lunghezza colonna waiting in record area/copy/TS/TD (se non definito tracciato)
	
	/*
	 * 
	 * Costruttore 
	 * 
	 */
	public EntityDynamicFieldSubWaitExt() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		typeObject  = EnumObject.NOT_ASSIGNED;
		typeSystemFieldExternal = EnumDataItemSystemEnvironment.NOT_ASSIGNED;
		
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
	 * Restituisce il tipo di oggetto con i dati necessari alla soluzione 
	 * dell'istruzione,<br>
	 * <p>
	 * 
	 * @return the idObject
	 */
	public String getIdObjectExternal() {
		return idObjectExternal;
	}

	/**
	 * Imposta il tipo di oggetto con i dati necessari alla soluzione 
	 * dell'istruzione,<br>
	 * <p>
	 * @param idObject the idObject to set
	 */
	public void setIdObjectExternal(String idObjectExternal) {
		this.idObjectExternal = idObjectExternal;
	}

	

	/**
	 * Restituisce il nome fisico (Dsname) associato al nome esterno (DDname)
	 * 
	 * @return the idObjectExternalPhis
	 */
	public String getDsnameExternal() {
		return dsnameExternal;
	}


	/**
	 * Imposta il nome fisico (Dsname) associato al nome esterno (DDname)
	 * 
	 * @param idObjectExternalPhis the idObjectExternalPhis to set
	 */
	public void setDsnameExternal(String dsnameExternal) {
		this.dsnameExternal = dsnameExternal;
	}


	/**
	 * Restituisce il tipo di oggetto con i dati necessari alla soluzione 
	 * dell'istruzione,<br>
	 * <p>
	 * 
	 * @return the typeObject 
	 */
	public EnumObject getTypeObjectExternal () {
		return typeObjectExternal ;
	}


	/**
	 * Imposta il tipo di oggetto con i dati necessari alla soluzione 
	 * dell'istruzione,<br>
	 * <p>
	 * 
	 * @param typeObject  the typeObject  to set
	 */
	public void setTypeObjectExternal (EnumObject typeObject ) {
		this.typeObjectExternal  = typeObject ;
	}


	/**
	 * Restituisce il tipo campo di sistema con i dati necessari alla soluzione 
	 * dell'istruzione,<br>
	 * <p>
	 * 
	 * @return the typeSystemFieldExternal
	 */
	public EnumDataItemSystemEnvironment getTypeSystemFieldExternal() {
		return typeSystemFieldExternal;
	}


	/**
	 * Imposta il tipo campo di sistema con i dati necessari alla soluzione 
	 * dell'istruzione,<br>
	 * <p>
	 * 
	 * @param typeSystemFieldExternal the typeSystemFieldExternal to set
	 */
	public void setTypeSystemFieldExternal(	EnumDataItemSystemEnvironment typeSystemFieldExternal) {
		this.typeSystemFieldExternal = typeSystemFieldExternal;
	}


	/**
	 * Restituisce il nome del cics in cui cercare il campo di sistema con i dati necessari alla soluzione 
	 * dell'istruzione,<br>
	 * <p>
	 * 
	 * @return the cicsNameExternal
	 */
	public String getCicsNameExternal() {
		return cicsNameExternal;
	}


	/**
	 * @param cicsNameExternal the cicsNameExternal to set
	 */
	public void setCicsNameExternal(String cicsNameExternal) {
		this.cicsNameExternal = cicsNameExternal;
	}


	/**
	 * @return the idFieldExternal
	 */
	public String getIdFieldExternal() {
		return idFieldExternal;
	}


	/**
	 * @param idFieldExternal the idFieldExternal to set
	 */
	public void setIdFieldExternal(String idFieldExternal) {
		this.idFieldExternal = idFieldExternal;
	}


	/**
	 * @return the posColumnExternal
	 */
	public int getPosColumnExternal() {
		return posColumnExternal;
	}


	/**
	 * @param posColumnExternal the posColumnExternal to set
	 */
	public void setPosColumnExternal(int posColumnExternal) {
		this.posColumnExternal = posColumnExternal;
	}


	/**
	 * Imposta il nome del cics in cui cercare il campo di sistema con i dati necessari alla soluzione 
	 * dell'istruzione,<br>
	 * <p>
	 * 
	 * @return the lengthColumnExternal
	 */
	public int getLengthColumnExternal() {
		return lengthColumnExternal;
	}


	/**
	 * Restituisce la lunghezza della colonna o del campo con i dati necessari alla soluzione 
	 * dell'istruzione,<br>
	 * <p>
	 * 
	 * @param lengthColumnExternal the lengthColumnExternal to set
	 */
	public void setLengthColumnExternal(int lengthColumnExternal) {
		this.lengthColumnExternal = lengthColumnExternal;
	}



	/**
	 * @return the numProgr
	 */
	public int getNumProgr() {
		return numProgr;
	}

	/**
	 * @param numProgr the numProgr to set
	 */
	public void setNumProgr(int numProgr) {
		this.numProgr = numProgr;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityDynamicFieldSubWaitExt objEntityDynamicWaitingExternal = null;
		
		objEntityDynamicWaitingExternal = (EntityDynamicFieldSubWaitExt) obj;
		 
		return this.system.equals(objEntityDynamicWaitingExternal.system)
		 &&    this.subSystem.equals(objEntityDynamicWaitingExternal.subSystem)
		 &&    this.idObject.equals(objEntityDynamicWaitingExternal.idObject)
		 &&    this.typeObject == objEntityDynamicWaitingExternal.typeObject
		 &&    this.numInstr == objEntityDynamicWaitingExternal.numInstr
		 &&    this.idField == objEntityDynamicWaitingExternal.idField;
	}




	
}
