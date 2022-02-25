package entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import enums.EnumDataItemType;
import enums.EnumObject;

/**
	* copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityDynamicSubField (DynamicSubField, DSFD)
	* </h1>
	*  <p>
	* Questa classe mappa la tabella DSFD e descrive un sottocampo di un campo di una istruzione dinamica.
	* Per il Cobol, come per il PL1, è possibile che un campo sia strutturato con dei sottocampi, a livelli
	* successivi non predefinibili. Il processo di individuazione dei possibili valori di un campo,
	* implica pertanto l'individuazione e la successiva ricomposizione, dei valori di tutti i suoi sottocampi.
	* Viene memorizzato il nome del sottocampo e lo stato, ovvero se il sottocampo è stato risolto o meno.<br>
	* Nel caso siano stati individuati i valori del sottocampo, questi sono memorizzati nell'entity 
	* {@link EntityDynamicValue} (DVAL) <br>
	* Nel caso lo stato del sottocampo sia ancora non risolto, l'entity {@link EntityDynamicFieldSubSetting}
	* contiene tutte le trasformazioni e impostazioni del sottocampo, a partire dall'istruzione dinamica
	* origine fino all'ultima assegnazione anche in programmi diversi.<br>
	* E' possibile che dopo una catena di numerosi programmi dove il sottocampo viene valorizzato, trasformato,
	* chiamato con nomi e formati diversi, l'ultima impostazione necessiti di un archivio esterno, di un 
	* sorgente non disponibile etc.
	* 
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

@Entity(name="DynamicFieldSub")
public class EntityDynamicFieldSub implements Serializable {

	private static final long serialVersionUID = 1L;
	
	///////////////////////////////////////////////////////////////////////
    // Data Items DynamicSubFieldDefinition                              //   
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
	private String idObject = "";          							// Nome programma
	@Id
	@Column(name="typeObject")
	private EnumObject typeObject = null;       					// Tipologia oggetto programma (T0001)
	@Id
	@Column(name="numInstr")
	private int numInstr = 0; 										// Numero istruzione
	@Id
	@Column(name="idField")
	private String idField = "";									// Nome campo operando da risolvere/risolto
	@Id
	@Column(name="idSubField")
	private String idSubField = "";									// Nome sottocampo campo operando 
	                                                                // Spaces in caso di campo elementare senza sottocampi
	@Id
	@Column(name="numField")
	private int numField = 0;           					        // Numero campo codificato nella struttura di programma

	// Data
	@Column(name="numSubField")
	private int numSubField = 0;           					        // Numero sottocampo codificato nella struttura di programma
	@Column(name="sizeSubField")
	private int sizeSubField = 0;           					    // Size in bytes sottocampo codificato nella struttura di programma
 	@Column(name="posSubField")
	private int posSubField = 0;           					        // pos (0-based) sottocampo codificato nella struttura di programma
	@Column(name="typeSubField")
    private EnumDataItemType typeSubField = null;  	    			// Tipologia specifica sottocampo  (T0008)
	
    // Sub field values resolution
	@Column(name="light")
	private boolean light = false;									// Indicazione di campo dinamico light (campo valorizzato da value nel programma)
	@Column(name="solved")
	private boolean solved = false;									// Indicazione di campo risolto, con valori validi recuperati
	@Column(name="waitingForData")
	private boolean waitingForData = false;						    // Indicazione di campo in attesa di dati esterni per essere risolto
	@Column(name="spreaded")
	private boolean spreaded = false;								// Indicazione di campo con assegnazioni spreaded, nei pgm chiamanti/chiamati

    
    
	public EntityDynamicFieldSub() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		typeSubField = EnumDataItemType.NOT_ASSIGNED;

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
	 * @return the sizeSubField
	 */
	public int getSizeSubField() {
		return sizeSubField;
	}




	/**
	 * @param sizeSubField the sizeSubField to set
	 */
	public void setSizeSubField(int sizeSubField) {
		this.sizeSubField = sizeSubField;
	}




	/**
	 * @return the typeSubField
	 */
	public EnumDataItemType getTypeSubField() {
		return typeSubField;
	}




	/**
	 * @param typeSubField the typeSubField to set
	 */
	public void setTypeSubField(EnumDataItemType typeSubField) {
		this.typeSubField = typeSubField;
	}




	/**
	 * @return the light
	 */
	public boolean isLight() {
		return light;
	}
	/**
	 * @return the light
	 */
	public boolean getLight() {
		return light;
	}




	/**
	 * @param light the light to set
	 */
	public void setLight(boolean light) {
		this.light = light;
	}




	/**
	 * @return the solved
	 */
	public boolean isSolved() {
		return solved;
	}
	/**
	 * @return the solved
	 */
	public boolean getSolved() {
		return solved;
	}




	/**
	 * @param solved the solved to set
	 */
	public void setSolved(boolean solved) {
		this.solved = solved;
	}




	/**
	 * @return the waitingForData
	 */
	public boolean isWaitingForData() {
		return waitingForData;
	}
	/**
	 * @return the waitingForData
	 */
	public boolean getWaitingForData() {
		return waitingForData;
	}




	/**
	 * @param waitingForData the waitingForData to set
	 */
	public void setWaitingForData(boolean waitingForData) {
		this.waitingForData = waitingForData;
	}




	/**
	 * @return the spreaded
	 */
	public boolean isSpreaded() {
		return spreaded;
	}
	/**
	 * @return the spreaded
	 */
	public boolean getSpreaded() {
		return spreaded;
	}




	/**
	 * @param spreaded the spreaded to set
	 */
	public void setSpreaded(boolean spreaded) {
		this.spreaded = spreaded;
	}




	/**
	 * @return the posSubField
	 */
	public int getPosSubField() {
		return posSubField;
	}




	/**
	 * @param posSubField the posSubField to set
	 */
	public void setPosSubField(int posSubField) {
		this.posSubField = posSubField;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityDynamicFieldSub objEntityDynamicFieldSub = null;
		
		objEntityDynamicFieldSub = (EntityDynamicFieldSub) obj;
		 
		return this.system.equals(objEntityDynamicFieldSub.system)
		 &&    this.subSystem.equals(objEntityDynamicFieldSub.subSystem)
		 &&    this.idObject.equals(objEntityDynamicFieldSub.idObject)
		 &&    this.typeObject == objEntityDynamicFieldSub.typeObject
		 &&    this.numInstr == objEntityDynamicFieldSub.numInstr
		 &&    this.idField.equals(objEntityDynamicFieldSub.idField)
		 &&    this.numField == objEntityDynamicFieldSub.numField
		 &&    this.idSubField.equals(objEntityDynamicFieldSub.idSubField);		 
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "EntityDynamicFieldSub [idObject=" + idObject + ", numInstr=" + numInstr + ", idField=" + idField
				+ ", numField=" + numField + ", idSubField=" + idSubField + ", sizeSubField=" + sizeSubField
				+ ", posSubField=" + posSubField + "]";
	}

	

}
