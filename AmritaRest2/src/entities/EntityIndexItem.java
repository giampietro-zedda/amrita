package entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumIndexOrder;
import enums.EnumObject;

/**
	 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityIndexItem (IndexItem, IDXI)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella IndexItem e descrive gli indici di un oggetto INDEX.
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

@Entity(name="IndexItem")
public class EntityIndexItem {

	///////////////////////////////////////////////////////////////////////
    // Data Items IndexItem                                              //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key  
	@Id
	@Column(name="sys")
	private String system = "";            		// IDXISYST(PK) Sistema applicativo
	@Id
	@Column(name="subSys")
	private String subSystem = "";         		// IDXISUBS(PK) Sotto sistema applicativo
	@Id
	@Column(name="idObject")
	private String idObject = "";          		// IDXIIDOB(PK) Nome oggetto Index
	@Id
	@Column(name="typeObject")
	private EnumObject typeObject = null;       // IDXITYPO(PK) Tipologia oggetto index (T0001)
	@Id
	@Column(name="idObjectOwner")
	private String idObjectOwner = "";          // IDXIIOWN(PK) Nome oggetto proprietario dell'indice Entity, Logical/Phisical File Vsam
	@Id
	@Column(name="typeObjectOwner")
	private EnumObject typeObjectOwner = null;  // IDXITOWN(PK) Tipologia oggetto proprietario (T0001)
	@Id
	@Column(name="numSeq")
	private int numSeq = 0;                     // IDXINSEQ(PK) Numero sequenza definizione idObjectOwner (CPYE EntityCopyEntityDefinition)
	
	// Data
	@Column(name="orderType")
    private EnumIndexOrder orderType = null;        // IDXIORDR     Ordinamento Ascending/Descending/.. (T0041)
	@Column(name="idColumnName")
    private String idColumnName = null;         // IDXIIDCO     Nome campo indice come definito in CPYE (ridondante)
		
	
	/*
     *
	 *  Costruttore
	 * 
	 */
	public EntityIndexItem() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		typeObjectOwner = EnumObject.NOT_ASSIGNED;
		orderType = EnumIndexOrder.NOT_ASSIGNED;
		   			 
		
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
	 * @return the idObjectOwner
	 */
	public String getIdObjectOwner() {
		return idObjectOwner;
	}



	/**
	 * @param idObjectOwner the idObjectOwner to set
	 */
	public void setIdObjectOwner(String idObjectOwner) {
		this.idObjectOwner = idObjectOwner;
	}



	/**
	 * @return the typeObjectOwner
	 */
	public EnumObject getTypeObjectOwner() {
		return typeObjectOwner;
	}



	/**
	 * @param typeObjectOwner the typeObjectOwner to set
	 */
	public void setTypeObjectOwner(EnumObject typeObjectOwner) {
		this.typeObjectOwner = typeObjectOwner;
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
	 * @return the orderType
	 */
	public EnumIndexOrder getOrderType() {
		return orderType;
	}



	/**
	 * @param orderType the order to set
	 */
	public void setOrderType(EnumIndexOrder orderType) {
		this.orderType = orderType;
	}



	/**
	 * @return the idColumnName
	 */
	public String getIdColumnName() {
		return idColumnName;
	}



	/**
	 * @param idColumnName the idColumnName to set
	 */
	public void setIdColumnName(String idColumnName) {
		this.idColumnName = idColumnName;
	}


		
}
