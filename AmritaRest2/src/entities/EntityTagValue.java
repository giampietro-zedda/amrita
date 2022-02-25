package entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedTable;
import enums.EnumObject;

/**
	 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityTagValue (TagValue, TAGV)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella TagValue e descrive un tag, ovvero una stringa, valorizzata
	 * a cura dell'utente, da ricercare nei sorgenti in fase di analisi.
	 * A livello di oggetti è previsto l'oggetto USER_TAG. Tale oggetto aggrega tutti i possibili valori
	 * che si desidera intercettare nei sorgenti con un significato compiuto.
	 * In caso il tag (stringa) sia individuato nei sorgenti, viene inserita sul data base una relazione
	 * fra l'gggetto USER_TAG e quello dove è stata individuata la stringa (programma, ddl, etc.).
	 * Vengono pertanto sfruttate le possibilità di memorizzazione standard dell'origine delle relazioni,
	 * nella tabella RELO, per indicare l'esatta riga sorgente origine.
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

@Entity(name="EntityTagValue")
public class EntityTagValue {

	///////////////////////////////////////////////////////////////////////
    // Data Items TagValue                                               //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
	@Column(name="sys")
	private String system = "";            		// TAGVSYST(PK) Sistema applicativo
	@Id
	@Column(name="subSys")
	private String subSystem = "";         		// TAGVSUBS(PK) Sotto sistema applicativo
	@Id
	@Column(name="idObject")
	private String idObject = "";          		// TAGVIDOB(PK) Identificativo categoria tag utente
	@Id
	@Column(name="typeObject")
	private EnumObject typeObject = null;       // TAGVTYPO(PK) Tipologia oggetto USER_TAG (T0001)
	@Id
	@Column(name="progr")
	private int progr = 0;                      // TAGVPROG(PK) Progressivo valori tag

	// Data
	@Column(name="tagValue")
	private String tagValue = "";				// TAGVVALU     Valore tag da cercare nei sorgenti
	@Column(name="tagLength")
	private int tagLength = 0;				    // TAGVLNGH     Lunghezza tag
    
	
	/*
	 * 
	 * Costruttore
	 * 
	 */
	public EntityTagValue() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		
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
	 * @return the tagValue
	 */
	public String getTagValue() {
		return tagValue;
	}


	/**
	 * @param tagValue the tagValue to set
	 */
	public void setTagValue(String tagValue) {
		this.tagValue = tagValue;
	}


	/**
	 * @return the tagLength
	 */
	public int getTagLength() {
		return tagLength;
	}


	/**
	 * @param tagLength the tagLength to set
	 */
	public void setTagLength(int tagLength) {
		this.tagLength = tagLength;
	}



}
