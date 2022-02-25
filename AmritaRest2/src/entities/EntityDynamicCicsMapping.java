package entities;
import enums.EnumObject;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

/**
	 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityDynamicCicsMapping (DynamicCicsMapping, DCXM)
	 * </h1>
	 *  <p>
	 * Questa classe descrive la tabella EntityDynamicCicsMapping e mappa i dsname esterni con le ddname, per ogni Cics definito.
	 * La tabella EntityDynamicCicsMapping è usata in congiunzione con la tabella DynamicValueExternal per determinare i valori esterni di files
	 * Vsam acceduti dal Cics con Exec Cics Read ... oppure di code Cics di Temporary Storage o Transient Data.
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

@Entity(name="DynamicCicsMapping")       			  
public class EntityDynamicCicsMapping {

	///////////////////////////////////////////////////////////////////////
    // Data Items ExternalValue                                          //                                                         
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
	@Column(name="sys")
	private String system = "";            					  // Sistema applicativo
	@Id
	@Column(name="subSys")
	private String subSystem = "";         					  // Sotto sistema applicativo
	@Id
	@Column(name="cicsName")
	private String cicsName = "";          					  // Nome Cics
	@Id
	@Column(name="typeObject")
	private EnumObject typeObject = null;       			  // Tipologia oggetto (ENTITY_VSAM, ENTITY_SQL, ... CICS_SYSTEM_FIELD) (T0001)
	@Id
	@Column(name="externalName")
	private String externalName = "";                		  // DDname in jcl Cics nome file Vsam/coda/ o nome tab Sql, seg DL1,...
    
	// Data
	@Column(name="dsname")
	private String dsname = "";                			      // Dsname file fisico/nome tabella/Segmento/Coda Ts ...
	
	
	/*
	 * Costruttore
	 */
	public EntityDynamicCicsMapping() {
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
	 * @return the cicsName
	 */
	public String getCicsName() {
		return cicsName;
	}

	/**
	 * @param cicsName the cicsName to set
	 */
	public void setCicsName(String cicsName) {
		this.cicsName = cicsName;
	}

	/**
	 * @return the typeObject
	 */
	public EnumObject getTypeObject() {
		return typeObject;
	}

	/**
	 * @param typeEntry the typeEntry to set
	 */
	public void setTypeObject(EnumObject typeObject) {
		this.typeObject = typeObject;
	}

	/**
	 * @return the externalName
	 */
	public String getExternalName() {
		return externalName;
	}

	/**
	 * @param externalName the externalName to set
	 */
	public void setExternalName(String externalName) {
		this.externalName = externalName;
	}

	/**
	 * @return the dsname
	 */
	public String getDsname() {
		return dsname;
	}

	/**
	 * @param dsname the dsname to set
	 */
	public void setDsname(String dsname) {
		this.dsname = dsname;
	}


}
