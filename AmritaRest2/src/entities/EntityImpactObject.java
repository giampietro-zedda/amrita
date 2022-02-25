package entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumCobolReservedWords;
import enums.EnumObject;


/**
	 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntitytImpactObject (ImpactObject)
	 * </h1>
	 *  <p>
	 * Questa classe dettaglia gli oggetti relativi a una operazione del piano di impatti,
	 * in termini di oggetto (pgm, source bms, etc.), numero riga e numero istruzione
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0
	 * @since 25/05/20
	 * @see Analyzer
	 * @see EntityObject 
	 * @see EntityObjectOption
	 * @see EntityIndexItem
	 * @see EntityImpactObject
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
	 * @see EntityImpactPlan
*/


@Entity(name="ImpactObject")
public class EntityImpactObject  { 

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
	@Id
	@Column(name="typeObjectTarget")
	private EnumObject typeObjectTarget;                // Tipo oggetto modificato di(COPY|ENTITY|MAP|JCL|..)
	@Id
	@Column(name="idObjectTarget")
	private String idObjectTarget = "";          	    // Nome oggetto modificato  
	@Id
    @Column(name="numInstr")
	private int numInstr;             					// Numero istruzione da modificare 
	@Id
	@Column(name="rowStart")
	private int rowStart;             					// Numero riga inizio modifica nel sorgente
		
	// Data
    @Column(name="rowEnd")
	private int rowEnd;             					// Numero riga fine modifica nek sorgente
    @Column(name="subSys")
	private String subSystem = "";         				// Sotto sistema applicativo (ridondante)
    @Column(name="cobolDivision")
	private EnumCobolReservedWords cobolDivision;       // Divisione Cobol impattata (DATA o PROC solo per programmi)
    @Column(name="fieldColumn")
	private int fieldColumn;             				// Nome campo/colonna
	

	/*
	 * Costruttore
	 */
	public EntityImpactObject() {
		super();
		typeObjectOrigin = EnumObject.NOT_ASSIGNED;
		typeObjectTarget = EnumObject.NOT_ASSIGNED;
		cobolDivision = EnumCobolReservedWords.NOT_ASSIGNED;

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
	 * @return the typeObjectTarget
	 */
	public EnumObject getTypeObjectTarget() {
		return typeObjectTarget;
	}

	/**
	 * @param typeObjectTarget the typeObjectTarget to set
	 */
	public void setTypeObjectTarget(EnumObject typeObjectTarget) {
		this.typeObjectTarget = typeObjectTarget;
	}

	/**
	 * @return the idObjectTarget
	 */
	public String getIdObjectTarget() {
		return idObjectTarget;
	}

	/**
	 * @param idObjectTarget the idObjectTarget to set
	 */
	public void setIdObjectTarget(String idObjectTarget) {
		this.idObjectTarget = idObjectTarget;
	}

	/**
	 * @return the cobolDivision
	 */
	public EnumCobolReservedWords getCobolDivision() {
		return cobolDivision;
	}

	/**
	 * @param cobolDivision the cobolDivision to set
	 */
	public void setCobolDivision(EnumCobolReservedWords cobolDivision) {
		this.cobolDivision = cobolDivision;
	}

	/**
	 * @return the rowStart
	 */
	public int getRowStart() {
		return rowStart;
	}

	/**
	 * @param rowStart the rowStart to set
	 */
	public void setRowStart(int rowStart) {
		this.rowStart = rowStart;
	}

	/**
	 * @return the rowEnd
	 */
	public int getRowEnd() {
		return rowEnd;
	}

	/**
	 * @param rowEnd the rowEnd to set
	 */
	public void setRowEnd(int rowEnd) {
		this.rowEnd = rowEnd;
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
	 * @return the fieldColumn
	 */
	public int getFieldColumn() {
		return fieldColumn;
	}

	/**
	 * @param fieldColumn the fieldColumn to set
	 */
	public void setFieldColumn(int fieldColumn) {
		this.fieldColumn = fieldColumn;
	}



}
