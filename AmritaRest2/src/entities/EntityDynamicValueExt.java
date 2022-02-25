package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumDataItemSystemEnvironment;
import enums.EnumObject;

/**
	 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityDynamicValueExternal (DynamicValueExternal, DVAE)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella DynamicValueExternal e descrive i valori delle colonne di archivi esterni,
	 * necessari a risolvere le istruzioni dinamiche incontrate nei processi di analisi. <br>
	 * In questo caso, le logiche di trasformazione dei dati elementari, terminano con un accesso a un archivio esterno.
	 * L'archivio esterno può essere una tabella Db2, un segmento Dl1, un file sequenziale o Vsam.<br>
	 * Inoltre, in luogo di un archivio esterno è possibile avere dei campi di sistema indicanti,
	 * per esempio, il campo terminale del Cics oppure una coda Cics di temporary storage o altro.<br>
	 * Senza il caricamento esterno manuale di questa tabella, il processo di soluzione delle istruzioni
	 * dinamiche non può essere portato a termine.<br>
	 * Viene descritta la colonna della tabella da cui recuperare i valori esterni in termini di nome colonna
	 * oppure posizione e lunghezza. Il processo di soluzione delle istruzioni dinamiche memorizza quale porzione,
	 * o tutta la colonna, è significativa. <br>
	 * Talvolta i valori di un campo di una istruzione dinamica non possono essere trovati attraverso i processi di
	 * individuazione delle logiche applicative, propagate nei programmi chiamati/chiamanti.
	 * Semplicemente un dato elementare viene infine letto da un archivio esterno, o da una tabella Sql o anche
	 * solo da una interfaccia esterna.<br>
	 * In questi casi i valori devono essere forniti da una struttura esterna. 
	 * Questa entity descrive proprio questo valore.
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

@Entity(name="DynamicValueExt")
public class EntityDynamicValueExt {

	///////////////////////////////////////////////////////////////////////
    // Data Items ExternalValue                                          //                                                         
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
	@Column(name="sys")
	private String system = "";            					  			// Sistema applicativo
	@Id
	@Column(name="subSys")
	private String subSystem = "";         					  			// Sotto sistema applicativo
	@Id
	@Column(name="typeObjectExternal")
	private EnumObject typeObjectExternal = null;       	  			// Tipologia oggetto (ENTITY, PHISICAL_FILE, OBJECT_CICS_.., OBJECT_CICS_SYSTEM_FIELD) (T0001)
	@Id
	@Column(name="idObjectExternal")
	private String idObjectExternal = "";          			  			// DDname file fisico/nome tabella/Segmento/Coda Ts ...space se oggetto CICS
	@Id
	@Column(name="dsnameExternal")
	private String dsnameExternal = "";          			            // Nome fisico esterno, rappresenta il Dsname trovato nel JCL		
	@Id
	@Column(name="idFieldColumnExternal")
	private String idFieldColumnExternal = "";                			// Nome campo/colonna come definito in CopyEntityDefinition o space
	@Id
	@Column(name="typeSystemFieldExternal")
	private EnumDataItemSystemEnvironment typeSystemFieldExternal = null; 	// Tipologia campo di sistema, se OBJECT_CICS_SYSTEM_FIELD, come CICS_EIBTRMID (T0021)
	@Id
	@Column(name="cicsNameExternal")
	private String cicsNameExternal = "";          					  	// Nome Cics valido se CICS_EIBTRMID e CICS_EIBTRNID. Space riferito a tutti i cics.
	@Id
	@Column(name="numProgr")
	private int numProgr = 0;                     			  			// Numero sequenza progressivo valore
    
	// Data
	@Column(name="posColumnExternal")
	private int posColumnExternal = 0;							      	// Posizione colonna in record area/copy/TS/TD (se non definito tracciato)
	@Column(name="lengthColumnExternal")
	private int lengthColumnExternal = 0;							  	// Lunghezza colonna in record area/copy/TS/TD (se non definito tracciato)
	@Column(name="value")
	private String value = "";                     			  			// Valore in formato carattere no packed del fromato definito


	/*
	 * Costruttore
	 */
	public EntityDynamicValueExt() {
		super();
		typeObjectExternal = EnumObject.NOT_ASSIGNED;
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
	public String getIdObjectExternal() {
		return idObjectExternal;
	}

	/**
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
	 * @return the typeObject
	 */
	public EnumObject getTypeObjectExternal() {
		return typeObjectExternal;
	}

	/**
	 * @param typeObject the typeObject to set
	 */
	public void setTypeObjectExternal(EnumObject typeObject) {
		this.typeObjectExternal = typeObject;
	}

	/**
	 * @return the typeSystemFieldExternal
	 */
	public EnumDataItemSystemEnvironment getTypeSystemFieldExternal() {
		return typeSystemFieldExternal;
	}

	/**
	 * @param  the typeSystemFieldExternal to set
	 */
	public void setTypeSystemFieldExternal(EnumDataItemSystemEnvironment typeSystemFieldExternal) {
		this.typeSystemFieldExternal = typeSystemFieldExternal;
	}

	/**
	 * @return the cicsName
	 */
	public String getCicsNameExternal() {
		return cicsNameExternal;
	}

	/**
	 * @param cicsName the cicsName to set
	 */
	public void setCicsNameExternal(String cicsNameExternal) {
		this.cicsNameExternal = cicsNameExternal;
	}

	/**
	 * @return the idFieldColumnExternal
	 */
	public String getIdFieldColumnExternal() {
		return idFieldColumnExternal;
	}

	/**
	 * @param idFieldColumnExternal the idFieldColumnExternal to set
	 */
	public void setIdFieldColumnExternal(String idFieldColumnExternal) {
		this.idFieldColumnExternal = idFieldColumnExternal;
	}

	/**
	 * @return the posColumn
	 */
	public int getPosColumnExternal() {
		return posColumnExternal;
	}

	/**
	 * @param posColumn the posColumn to set
	 */
	public void setPosColumnExternal(int posColumnExternal) {
		this.posColumnExternal = posColumnExternal;
	}

	/**
	 * @return the lengthColumn
	 */
	public int getLengthColumnExternal() {
		return lengthColumnExternal;
	}

	/**
	 * @param lengthColumn the lengthColumn to set
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

	/**
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

}
