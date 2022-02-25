package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import enums.EnumObject;
import enums.EnumObjectStatus;
import enums.EnumSourceType;

/**
	 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityObject (Object, OBJT) 
	 * </h1>
	 *  <p>
	 * Questa classe descrive l'entity EntityObject, ovvero la tabella oggetti OBJT.
	 * Ogni oggetto analizzato viene inserito in questa tabella.
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
	 * @see Analyzer
	 * @see EntityObjectShort 
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

@Entity(name="Object")
public class EntityObjectShort implements Cloneable {

	///////////////////////////////////////////////////////////////////////
    // Data Items Object                                                 //                                                        
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            			// OBJTSYST(PK) Sistema applicativo
	@Id	
    @Column(name="subSys")
	private String subSystem = "";         			// OBJTSUBS(PK) Sotto sistema applicativo
	@Id
    @Column(name="idObject")
	private String idObject = "";          			// OBJTIDOB(PK) Nome oggetto (es. programma, file fisico,..)
	@Id
    @Column(name="typeObject")
	private EnumObject typeObject = null;       	// OBJTTYPO(PK) Tipologia oggetto (T0001)
	@Id
    @Column(name="typeSource")
	private EnumSourceType typeSource; 				// OBJTTYPS     Tipologia sorgente (T0002)			
	
	// Data 
    @Column(name="subSysOwner")
	private String subSystemOwner = "";    			// OBJTSSOW  	Sotto sistema applicativo proprietario
    @Column(name="statusObject")
	private EnumObjectStatus status;            	// OBJTSTAT     Stato oggetto (T0003)
	
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityObjectShort() {
		super();
		
		typeObject = EnumObject.NOT_ASSIGNED;
		typeSource = EnumSourceType.NOT_ASSIGNED;
		status = EnumObjectStatus.OBJECT_TO_BE_ANALYZED;
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
		if (typeObject == EnumObject.OBJECT_COPY_COBOL_DATA ) {
			this.typeSource = EnumSourceType.COBOL_COPY_DATA;
		} else if (typeObject == EnumObject.OBJECT_COPY_COBOL_PROC) {
			this.typeSource = EnumSourceType.COBOL_COPY_PROC;
		} else if (typeObject == EnumObject.OBJECT_COPY_COBOL_ENV) {
			this.typeSource = EnumSourceType.COBOL_COPY_ENV;
		} else if (typeObject == EnumObject.OBJECT_PGM_COBOL) {
			this.typeSource = EnumSourceType.COBOL_PROGRAM;
		} else if (typeObject == EnumObject.OBJECT_CICS_BMS_SOURCE) {
			this.typeSource = EnumSourceType.CICS_BMS;
		} else if (typeObject == EnumObject.OBJECT_SQL_SCRIPT
				|| typeObject == EnumObject.OBJECT_ENTITY_SQL) {
			this.typeSource = EnumSourceType.SQL_SCRIPT;
		} else {
			this.typeSource = EnumSourceType.NOT_ASSIGNED;
		}	
	}
	
	/**
	 * @return the typeSource
	 */
	public EnumSourceType getTypeSource() {
		return typeSource;
	}
	/**
	 * @param typeSource the typeSource to set
	 */
	public void setTypeSource(EnumSourceType typeSource) {
		this.typeSource = typeSource;
	}
	
	/**
	 * @return the status
	 */
	public EnumObjectStatus getStatus() {
		return status;
	}
	/**
	 * @param status the status to set
	 */
	public void setStatus(EnumObjectStatus status) {
		this.status = status;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return  typeObject.toString() + " IdObject:" + idObject;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityObjectShort objEntityObject = null;
		
		objEntityObject = (EntityObjectShort) obj;
		 
		return this.system.equals(objEntityObject.system)
		 &&    this.subSystem.equals(objEntityObject.subSystem)
		 &&    this.idObject.equals(objEntityObject.idObject)
		 &&    this.typeObject == objEntityObject.typeObject;
		 
	}

	/**
	 * @return the subSystemOwner
	 */
	public String getSubSystemOwner() {
		return subSystemOwner;
	}
	/**
	 * @param subSystemOwner the subSystemOwner to set
	 */
	public void setSubSystemOwner(String subSystemOwner) {
		this.subSystemOwner = subSystemOwner;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	public EntityObjectShort clone()  {
		EntityObjectShort o = null;
		 try {
			o = (EntityObjectShort) super.clone();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return o;
	}





}
