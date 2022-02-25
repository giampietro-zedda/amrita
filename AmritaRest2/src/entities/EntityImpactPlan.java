package entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import enums.EnumDataItemType;
import enums.EnumTypeImpactChange;
import enums.EnumObject;


/**
	 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntitytImpactPlan (ImpactPlan)
	 * </h1>
	 *  <p>
	 * Questa classe elenca le operazioni da effettuare per un piano di impatto.
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0
	 * @since 25/05/20
	 * @see Analyzer
	 * @see EntityObject 
	 * @see EntityObjectOption
	 * @see EntityIndexItem
	 * @see EntityImpactPlan
	 * @see EntitytRelationOrigin
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


@Entity(name="ImpactPlan")
public class EntityImpactPlan  { 

	///////////////////////////////////////////////////////////////////////
    // Data Items Relation                                               //                                                     
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            				// Sistema applicativo
	@Id
    @Column(name="idPlan")
	private String idPlan = "";          			    // Identificativo piano di impatto
	@Id
    @Column(name="numOp")
	private int numOp;             						// Numero operazione
	@Id
    @Column(name="typeObjectOrigin")
	private EnumObject typeObjectOrigin;                // Tipo oggetto origine di(COPY|ENTITY|MAP|JCL|..)
	@Id
    @Column(name="idObjectOrigin")
	private String idObjectOrigin = "";          	    // Nome oggetto di cui verificare l'impatto  
	
	// Data
	@Id
    @Column(name="subSys")
	private String subSystem = "";         				// Sotto sistema applicativo (volutamente NON in chiave)
    @Column(name="fieldColumn")
	private String fieldColumn = "";       	            // Nome campo/Colonna
    @Column(name="typeImpactChange")
	private EnumTypeImpactChange typeImpactChange;      // Tipologia impatto
    @Column(name="fromLength")
	private int fromLength = 0;             			// Da lunghezza
    @Column(name="toLength")
	private int toLength = 0;          					// A lunghezza
    @Column(name="fromInt")
	private int fromInt = 0;          				    // Da intero
    @Column(name="toInt")
	private int toInt = 0;         						// A intero
    @Column(name="fromDec")
	private int fromDec = 0;          				    // Da decimali
    @Column(name="toDec")
	private int toDec = 0;         						// A decimali
    @Column(name="fromDataType")
	private EnumDataItemType fromDataType;             	// Da formato (Packed/Display/Comp
    @Column(name="toDataType")
	private EnumDataItemType toDataType;             	// A formato (Packed/Display/Comp
    @Column(name="fromDataTypePic") 
	private String fromDataTypePic= "";         	    // Da formato (Packed/Display/Comp
    @Column(name="toDataTypePic")
	private String toDataTypePic= "";       	        // A formato (Packed/Display/Comp   
    @Column(name="fromDefaultValue")
	private String fromDefaultValue = "";             	// Da  Default 
    @Column(name="toDefaultValue")
	private String toDefaultValue = "";             	// A  Default 
    @Column(name="fromSign")
	private boolean fromSign = false;             	    // Da signed
    @Column(name="toSign")
	private boolean toSign = false;             	    // A unsigned
	

	/*
	 * Costruttore
	 */
	public EntityImpactPlan() {
		super();
		typeObjectOrigin = EnumObject.NOT_ASSIGNED;
		typeImpactChange = EnumTypeImpactChange.NOT_ASSIGNED;
		fromDataType = EnumDataItemType.NOT_ASSIGNED;
		toDataType = EnumDataItemType.NOT_ASSIGNED;
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
	 * @return the fieldColumn
	 */
	public String getFieldColumn() {
		return fieldColumn;
	}

	/**
	 * @param fieldColumn the fieldColumn to set
	 */
	public void setFieldColumn(String fieldColumn) {
		this.fieldColumn = fieldColumn;
	}

	/**
	 * @return the typeObjectOrigin
	 */
	public EnumObject getTypeObjectOrigin() {
		return typeObjectOrigin;
	}


	/**
	 * @param typeObjectOrigin the typeObjectOrigin to set
	 */
	public void setTypeObjectOrigin(EnumObject typeObjectOrigin) {
		this.typeObjectOrigin = typeObjectOrigin;
	}

	/**
	 * @param idObjectOrigin the idObjectOrigin to set
	 */
	public void setIdObjectOrigin(String idObjectOrigin) {
		this.idObjectOrigin = idObjectOrigin;
	}


	/**
	 * @return the idObjectOrigin
	 */
	public String getIdObjectOrigin() {
		return idObjectOrigin;
	}

	

	/**
	 * @return the idPlan
	 */
	public String getIdPlan() {
		return idPlan;
	}

	/**
	 * @param idPlan the idPlan to set
	 */
	public void setIdPlan(String idPlan) {
		this.idPlan = idPlan;
	}

	/**
	 * @return the impactType
	 */
	public EnumTypeImpactChange getTypeImpactChange() {
		return typeImpactChange;
	}

	/**
	 * @param impactType the impactType to set
	 */
	public void setTypeImpactChange(EnumTypeImpactChange typeImpactChange) {
		this.typeImpactChange = typeImpactChange;
	}

	/**
	 * @return the numOp
	 */
	public int getNumOp() {
		return numOp;
	}

	/**
	 * @param numOp the numOp to set
	 */
	public void setNumOp(int numOp) {
		this.numOp = numOp;
	}

	/**
	 * @return the fromLength
	 */
	public int getFromLength() {
		return fromLength;
	}

	/**
	 * @param fromLength the fromLength to set
	 */
	public void setFromLength(int fromLength) {
		this.fromLength = fromLength;
	}

	/**
	 * @return the toLength
	 */
	public int getToLength() {
		return toLength;
	}

	/**
	 * @param toLength the toLength to set
	 */
	public void setToLength(int toLength) {
		this.toLength = toLength;
	}

	/**
	 * @return the fromInt
	 */
	public int getFromInt() {
		return fromInt;
	}

	/**
	 * @param fromInt the fromInt to set
	 */
	public void setFromInt(int fromInt) {
		this.fromInt = fromInt;
	}

	/**
	 * @return the toInt
	 */
	public int getToInt() {
		return toInt;
	}

	/**
	 * @param toInt the toInt to set
	 */
	public void setToInt(int toInt) {
		this.toInt = toInt;
	}

	/**
	 * @return the fromDec
	 */
	public int getFromDec() {
		return fromDec;
	}

	/**
	 * @param fromDec the fromDec to set
	 */
	public void setFromDec(int fromDec) {
		this.fromDec = fromDec;
	}

	/**
	 * @return the toDec
	 */
	public int getToDec() {
		return toDec;
	}

	/**
	 * @param toDec the toDec to set
	 */
	public void setToDec(int toDec) {
		this.toDec = toDec;
	}

	/**
	 * @return the fromDataType
	 */
	public EnumDataItemType getFromDataType() {
		return fromDataType;
	}

	/**
	 * @param fromDataType the fromDataType to set
	 */
	public void setFromDataType(EnumDataItemType fromDataType) {
		this.fromDataType = fromDataType;
	}

	/**
	 * @return the toDataType
	 */
	public EnumDataItemType getToDataType() {
		return toDataType;
	}

	/**
	 * @param toDataType the toDataType to set
	 */
	public void setToDataType(EnumDataItemType toDataType) {
		this.toDataType = toDataType;
	}

	/**
	 * @return the fromDataTypePic
	 */
	public String getFromDataTypePic() {
		return fromDataTypePic;
	}

	/**
	 * @param fromDataTypePic the fromDataTypePic to set
	 */
	public void setFromDataTypePic(String fromDataTypePic) {
		this.fromDataTypePic = fromDataTypePic;
	}

	/**
	 * @return the toDataTypePic
	 */
	public String getToDataTypePic() {
		return toDataTypePic;
	}

	/**
	 * @param toDataTypePic the toDataTypePic to set
	 */
	public void setToDataTypePic(String toDataTypePic) {
		this.toDataTypePic = toDataTypePic;
	}

	/**
	 * @return the fromSign
	 */
	public boolean getFromSign() {
		return fromSign;
	}

	/**
	 * @return the fromSign
	 */
	public boolean isFromSign() {
		return fromSign;
	}

	/**
	 * @param fromSign the fromSign to set
	 */
	public void setFromSign(boolean fromSign) {
		this.fromSign = fromSign;
	}

	/**
	 * @return the toSign
	 */
	public boolean getToSign() {
		return toSign;
	}

	/**
	 * @return the toSign
	 */
	public boolean isToSign() {
		return toSign;
	}

	/**
	 * @param toSign the toSign to set
	 */
	public void setToSign(boolean toSign) {
		this.toSign = toSign;
	}

	
	/**
	 * @return the fromDefaultValue
	 */
	public String getFromDefaultValue() {
		return fromDefaultValue;
	}

	/**
	 * @param fromDefaultValue the fromDefaultValue to set
	 */
	public void setFromDefaultValue(String fromDefaultValue) {
		this.fromDefaultValue = fromDefaultValue;
	}

	/**
	 * @return the toDefaultValue
	 */
	public String getToDefaultValue() {
		return toDefaultValue;
	}

	/**
	 * @param toDefaultValue the toDefaultValue to set
	 */
	public void setToDefaultValue(String toDefaultValue) {
		this.toDefaultValue = toDefaultValue;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "EntityImpactPlan [idPlan=" + idPlan + ", idObject=" + idObjectOrigin + ", typeObject=" + typeObjectOrigin
				+ ", typeImpactChange=" + typeImpactChange + ", numOp=" + numOp + ", fromLength=" + fromLength
				+ ", toLength=" + toLength + ", fromInt=" + fromInt + ", toInt=" + toInt + ", fromDec=" + fromDec
				+ ", toDec=" + toDec + ", fromDataType=" + fromDataType + ", toDataType=" + toDataType
				+ ", fromDataTypePic=" + fromDataTypePic + ", toDataTypePic=" + toDataTypePic + "]";
	}

}
