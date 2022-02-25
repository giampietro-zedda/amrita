package entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import enums.EnumObject;

/**
	* Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityDynamicValueField (DynamicValueField, DVAL)
	* </h1>
	*  <p>
	* Questa classe mappa la tabella EntityDynamicValueField e descrive un valore individuato per un sottocampo
	* o per un campo di una istruzione dinamica. <br>
	* Tale valore viene individuato nei processi di soluzione delle istruzioni dinamiche, 
	* che analizzano la propagazione delle trasformazioni dei campi elementari nello stesso programma 
	* e propagate in programmi diversi.
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
	 * @see EntityDynamicFieldSubValue
	 * @see EntityDynamicFieldSubSetting
*/

@Entity(name="DynamicFieldSubValue")
public class EntityDynamicFieldSubValue implements Serializable {

	private static final long serialVersionUID = 1L;
	
	///////////////////////////////////////////////////////////////////////
    // Data Items DynamicFieldValue                                  
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
	private EnumObject typeObject = null;       					// Tipologia oggetto programma (T0001)
	@Id
	@Column(name="numInstr")
	private int numInstr = 0; 										// Numero istruzione
	@Id
	@Column(name="idField")
	private String idField = "";									// Nome campo operando da risolvere/risolto
	@Id
	@Column(name="idSubField")
	private String idSubField = "";									// Spaces indica valore per campo elementare o campo di gruppo
	@Id
	@Column(name="progr")
	private int progr = 0;           					            // Progressivo valore 

	// Data
	
	@Column(name="defaultValue")
	private boolean defaultValue = false;							// True indica valore di default non dovuto a catene di trasformazione
	
	@Column(name="fieldValueFull")
	private boolean fieldValueFull = false;							// True indica valore completo riferito al campo nel suo complesso quando idSubfield = space
	                                                                // in questi casi posInSubField = 1 e lngInSubField = lunghezza in bytes del campo	
	// Porzione di sottocampo oggetto dell'impostazione 
	@Column(name="posInSubField")
	private int posInSubField = 0;									// Posizione in sottocampo (1-based) inizio valore di questa assegnazione
	@Column(name="lngInSubField")
	private int lngInSubField = 0;									// Lunghezza in sottocampo valorizzata da questa assegnazione
	@Column(name="value")
    private String value = "";										// Valore sottocampo o campo 

	// Origine valore sottocampo
	@Column(name="typeObjectFrom")
    private EnumObject typeObjectFrom = null;                        // Tipo oggetto (pgm, file, etc.) dove il valore è stato impostato
	@Column(name="idObjectFrom")
    private String idObjectFrom = "";                                // Nome oggetto (pgm, file, etc.) dove il valore è stato impostato		
	@Column(name="numInstrFrom")
    private int numInstrFrom = 0;                                    // Numero istruzione di assegnazione
	@Column(name="idPgmFrom")
    private String idPgmFrom = "";                                   // Pgm di assegnazione

	
	public EntityDynamicFieldSubValue() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		typeObjectFrom = EnumObject.NOT_ASSIGNED;
		
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
	 * @return the typeObjectFrom
	 */
	public EnumObject getTypeObjectFrom() {
		return typeObjectFrom;
	}



	/**
	 * @param typeObjectFrom the typeObjectFrom to set
	 */
	public void setTypeObjectFrom(EnumObject typeObjectFrom) {
		this.typeObjectFrom = typeObjectFrom;
	}



	/**
	 * @return the idObjectFrom
	 */
	public String getIdObjectFrom() {
		return idObjectFrom;
	}



	/**
	 * @param idObjectFrom the idObjectFrom to set
	 */
	public void setIdObjectFrom(String idObjectFrom) {
		this.idObjectFrom = idObjectFrom;
	}



	/**
	 * @return the numInstrFrom
	 */
	public int getNumInstrFrom() {
		return numInstrFrom;
	}



	/**
	 * @param numInstrFrom the numInstrFrom to set
	 */
	public void setNumInstrFrom(int numInstrFrom) {
		this.numInstrFrom = numInstrFrom;
	}



	/**
	 * @return the idPgmFrom
	 */
	public String getIdPgmFrom() {
		return idPgmFrom;
	}



	/**
	 * @param idPgmFrom the idPgmFrom to set
	 */
	public void setIdPgmFrom(String idPgmFrom) {
		this.idPgmFrom = idPgmFrom;
	}



	/**
	 * @return the defaultValue
	 */
	public boolean isDefaultValue() {
		return defaultValue;
	}

	/**
	 * @return the defaultValue
	 */
	public boolean getDefaultValue() {
		return defaultValue;
	}


	/**
	 * @param defaultValue the defaultValue to set
	 */
	public void setDefaultValue(boolean defaultValue) {
		this.defaultValue = defaultValue;
	}



	/**
	 * @return the fieldValueFull
	 */
	public boolean isFieldValueFull() {
		return fieldValueFull;
	}

	/**
	 * @return the fieldValueFull
	 */
	public boolean getFieldValueFull() {
		return fieldValueFull;
	}



	/**
	 * @param fieldValueFull the fieldValueFull to set
	 */
	public void setFieldValueFull(boolean fieldValueFull) {
		this.fieldValueFull = fieldValueFull;
	}



	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "EntityDynamicFieldSubValue [numInstr=" + numInstr + ", idField=" + idField + ", idSubField="
				+ idSubField + ", progr=" + progr + ", value=" + value + "]";
	}



}
