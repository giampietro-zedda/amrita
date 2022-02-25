package entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumObject;


/**
	 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntitytImpactRecompile (ImpactRecompile)
	 * </h1>
	 *  <p>
	 * Questa classe dettaglia i programmi da ricompilare a fronte di un impact plan.
	 * Per esempio allargando un campo di un copy tutti i programmi che lo includono devono essere ricompilati.
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0
	 * @since 25/05/20
	 * @see Analyzer
	 * @see EntityObject 
	 * @see EntityObjectOption
	 * @see EntityIndexItem
	 * @see EntityImpactCompile
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
	 * @see EntityImpactObject
*/


@Entity(name="ImpactCompile")
public class EntityImpactCompile  { 

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
    @Column(name="typeObjectCompile")
	private EnumObject typeObjectCompile;                // Tipo oggetto da ricompilare (PGM|ENTITY|MAP|..)
	@Id
    @Column(name="idObjectCompile")
	private String idObjectCompile = "";          	    // Nome oggetto da ricompilare
		
	// Data
    @Column(name="subSys")
	private String subSystem = "";         				// Sotto sistema applicativo (ridondante)
	

	/*
	 * Costruttore
	 */
	public EntityImpactCompile() {
		super();
		typeObjectCompile = EnumObject.NOT_ASSIGNED;

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
	 * @param typeObjectCompile the typeObjectOrigin to set
	 */
	public void setTypeObjectCompile(EnumObject typeObjectCompile) {
		this.typeObjectCompile = typeObjectCompile;
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
	 * @return the typeObjectCompile
	 */
	public EnumObject getTypeObjectCompile() {
		return typeObjectCompile;
	}

	/**
	 * @return the idObjectCompile
	 */
	public String getIdObjectCompile() {
		return idObjectCompile;
	}

	/**
	 * @param idObjectCompile the idObjectCompile to set
	 */
	public void setIdObjectCompile(String idObjectCompile) {
		this.idObjectCompile = idObjectCompile;
	}

}
