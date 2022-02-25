package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import enums.EnumLanguageItemInstr;
import enums.EnumDataItemType;
import enums.EnumObject;
/**
	* Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityCopyEntityDefinition (CopyEntityDefinition, CPYE)
	* </h1>
	*  <p>
	* Questa classe mappa la tabella CPYE e descrive il tracciato record di un modulo copy o le colonne di una tabella.
	* la stessa relazione può essere generata a fronte di situazioni molto diverse, in differenti 
	* istruzioni dello stesso programma.
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

@Entity(name="CopyEntityDefinition")
public class EntityCopyEntityDefinition {

	///////////////////////////////////////////////////////////////////////
    // Data Items RecordEntityCopyItem                                   //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
	@Column(name="sys")
	private String system = "";            			// CPYESYST(PK) Sistema applicativo
	@Id
	@Column(name="subSys")
	private String subSystem = "";         			// CPYESUBS(PK) Sotto sistema applicativo
	@Id
	@Column(name="idObject")
	private String idObject = "";          			// CPYEIDOB(PK) Nome oggetto (Entity, Copy)
	@Id
	@Column(name="typeObject")
	private EnumObject typeObject = null;       	// CPYETYPO(PK) Tipologia oggetto Entity/Copy/Internal/Phisical Vsam (T0001)
	@Id
	@Column(name="numSeq")
	private int numSeq = 0;                     	// CPYENSEQ(PK) Numero definizione campo da inizio copy/definizione ddl (0-based step 5)
                                                    //              La numerazione può avere dei buchi valorizzati in fase di analisi
													//              con copy stmt o altre istruzioni
	@Column(name="idField")
	private String idField = "";                	// CPYEFNAM     Nome campo/colonna

	// Data
	@Column(name="level")
	private int level = 0;                      	// CPYELEVL     Livello 	(X Copy e strutture Adabas)
	@Column(name="occurs")
	private int occurs = 0;                         // CPYEOCCR     Occorrenze 	(X Copy e strutture Adabas)
	@Column(name="lngBytes")
	private int lngBytes = 0;                   	// CPYESIZE     Lunghezza campo in bytes
	@Column(name="pos")
	private int pos = 0;                        	// CPYEPOSI     Posizione campo (0-based)
	@Column(name="numInt")
	private int numInt = 0;                     	// CPYENINT     Numero interi campo
	@Column(name="numDec")
	private int numDec = 0;                        	// CPYENDEC     Numero decimali campo
	@Column(name="signed")
	private boolean signed = false;             	// CPYESIGN     Campo definito con segno
	@Column(name="groupField")
	private boolean groupField = false;         	// CPYEFGRP     Campo di gruppo
	@Column(name="itemLang")
    private EnumLanguageItemInstr itemLang = null; 	// CPYELANG     Linguaggio item (Cobol, Sql, ..)  (T0007)
	@Column(name="itemType")
    private EnumDataItemType itemType = null;  	    // CPYEDTYP     Tipologia specifica item  (T0008)
    
                                                    // * Valori specifici per tabelle sql *
	@Column(name="numDigit")
	private int numDigit = 0;                     	// CPYEDIGT     Numero digit complessivi colonna sql
	@Column(name="scale")
	private int scale = 0;                     	    // CPYESCAL     Numero digit decimali 
	@Column(name="notNull")
	private boolean notNull = false;             	// CPYENNUL     Colonna definita not null
	@Column(name="withDefault")
	private boolean withDefault = false;            // CPYEWDFL     Colonna definita with default
	@Column(name="defaultValue")
	private String defaultValue = "";               // CPYEDFLV     Valore default
                                                    //              La lunghezza della colonna è data da lngBytes
        
	/*
	 * Costruttore
	 */
	public EntityCopyEntityDefinition() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		itemLang = EnumLanguageItemInstr.NOT_ASSIGNED;
		itemType = EnumDataItemType.NOT_ASSIGNED;
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
	 * @return the numSeq
	 */
	public int getNumSeq() {
		return numSeq;
	}


	/**
	 * @param numSeq the numSeq to set
	 */
	public void setNumSeq(int numSeq) {
		this.numSeq = numSeq;
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
	 * @return the level
	 */
	public int getLevel() {
		return level;
	}


	/**
	 * @param level the level to set
	 */
	public void setLevel(int level) {
		this.level = level;
	}


	/**
	 * @return the occurs
	 */
	public int getOccurs() {
		return occurs;
	}


	/**
	 * @param occurs the occurs to set
	 */
	public void setOccurs(int occurs) {
		this.occurs = occurs;
	}


	/**
	 * @return the lngBytes
	 */
	public int getLngBytes() {
		return lngBytes;
	}


	/**
	 * @param lngBytes the lngBytes to set
	 */
	public void setLngBytes(int lngBytes) {
		this.lngBytes = lngBytes;
	}

	/**
	 * @return the pos
	 */
	public int getPos() {
		return pos;
	}




	/**
	 * @param pos the pos to set
	 */
	public void setPos(int pos) {
		this.pos = pos;
	}


	/**
	 * @return the numInt
	 */
	public int getNumInt() {
		return numInt;
	}


	/**
	 * @param numInt the numInt to set
	 */
	public void setnumInt(int numInt) {
		this.numInt = numInt;
	}


	/**
	 * @return the dec
	 */
	public int getNumDec() {
		return this.numDec;
	}


	/**
	 * @param dec the dec to set
	 */
	public void setNumDec(int numDec) {
		this.numDec = numDec;
	}


	/**
	 * @return the fieldSigned
	 */
	public boolean getSigned() {
		return signed;
	}


	/**
	 * @param fieldSigned the fieldSigned to set
	 */
	public void setSigned(boolean signed) {
		this.signed = signed;
	}


	/**
	 * @return the fieldGroup
	 */
	public boolean getGroupField() {
		return this.groupField;
	}


	/**
	 * @param fieldGroup the fieldGroup to set
	 */
	public void setGroupField(boolean groupField) {
		this.groupField = groupField;
	}



	/**
	 * @return the typeItem
	 */
	public EnumLanguageItemInstr getItemLang() {
		return this.itemLang;
	}


	/**
	 * @param typeItem the typeItem to set
	 */
	public void setTypeItem(EnumLanguageItemInstr itemLang) {
		this.itemLang = itemLang;
	}


	/**
	 * @return the itemType
	 */
	public EnumDataItemType getItemType() {
		return itemType;
	}


	/**
	 * @param itemType the itemType to set
	 */
	public void setItemType(EnumDataItemType itemType) {
		this.itemType = itemType;
	}


	/**
	 * @return the signed
	 */
	public boolean isSigned() {
		return signed;
	}


	/**
	 * @param numInt the numInt to set
	 */
	public void setNumInt(int numInt) {
		this.numInt = numInt;
	}


	/**
	 * @param itemLang the itemLang to set
	 */
	public void setItemLang(EnumLanguageItemInstr itemLang) {
		this.itemLang = itemLang;
	}


	/**
	 * @return the numDigit
	 */
	public int getNumDigit() {
		return numDigit;
	}


	/**
	 * @param numDigit the numDigit to set
	 */
	public void setNumDigit(int numDigit) {
		this.numDigit = numDigit;
	}


	/**
	 * @return the scale
	 */
	public int getScale() {
		return scale;
	}


	/**
	 * @param scale the scale to set
	 */
	public void setScale(int scale) {
		this.scale = scale;
	}


	/**
	 * @return the notNull
	 */
	public boolean getNotNull() {
		return notNull;
	}


	/**
	 * @param notNull the notNull to set
	 */
	public void setNotNull(boolean notNull) {
		this.notNull = notNull;
	}


	/**
	 * @return the withDefault
	 */
	public boolean getWithDefault() {
		return withDefault;
	}


	/**
	 * @param withDefault the withDefault to set
	 */
	public void setWithDefault(boolean withDefault) {
		this.withDefault = withDefault;
	}


	/**
	 * @return the defaultValue
	 */
	public String getDefaultValue() {
		return defaultValue;
	}


	/**
	 * @param defaultValue the defaultValue to set
	 */
	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}


}
