package entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import enums.EnumObject;
import enums.EnumRelation;
import enums.EnumSourceType;

/**
	 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntitytRelation (Relation, RELA)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella Relation e descrive una relazione fra due oggetti in Object table.
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


@Entity(name="Relation")
public class EntityRelation  { 

	///////////////////////////////////////////////////////////////////////
    // Data Items Relation                                               //                                                     
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            				// Sistema applicativo
	@Id
    @Column(name="subSys")
	private String subSystem = "";         				// Sotto sistema applicativo
	@Id
    @Column(name="relation")
	private EnumRelation relation = null;   			// Relazione codificata fra oggetti A e B (T0034)										
	@Id
    @Column(name="idObjectA")
	private String idObjectA = "";          			// Nome oggetto A (es. programma, file fisico,..)
	@Id
    @Column(name="typeObjectA")
	private EnumObject typeObjectA;             		// Tipo oggetto A  (T0001)
	@Id
    @Column(name="typeSourceA")
	private EnumSourceType typeSourceA; 				// Tipologia sorgente (T0002)	
	@Id
    @Column(name="idObjectB")
	private String idObjectB = "";          			// Nome oggetto B (es. programma, file fisico,..)
	@Id
    @Column(name="typeObjectB")
	private EnumObject typeObjectB;             		// Tipo oggetto B  (T0001)
	@Id
    @Column(name="typeSourceB")
	private EnumSourceType typeSourceB; 				// Tipologia sorgente (T0002)	
	
	
	/*
	 * Costruttore
	 */
	public EntityRelation() {
		super();
		relation = EnumRelation.NOT_ASSIGNED;
		typeObjectA = EnumObject.NOT_ASSIGNED;
		typeSourceA = EnumSourceType.NOT_ASSIGNED;
		typeObjectB = EnumObject.NOT_ASSIGNED;
		typeSourceB = EnumSourceType.NOT_ASSIGNED;

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
	 * @return the relation
	 */
	public EnumRelation getRelation() {
		return relation;
	}


	/**
	 * @param relation the relation to set
	 */
	public void setRelation(EnumRelation relation) {
		this.relation = relation;
	}


	/**
	 * @return the typeObjectA
	 */
	public EnumObject getTypeObjectA() {
		return typeObjectA;
	}


	/**
	 * @param typeObjectA the typeObjectA to set
	 */
	public void setTypeObjectA(EnumObject typeObjectA) {
		this.typeObjectA = typeObjectA;
		if (typeObjectA == EnumObject.OBJECT_COPY_COBOL_DATA ) {
			this.typeSourceA = EnumSourceType.COBOL_COPY_DATA;
		} else if (typeObjectA == EnumObject.OBJECT_COPY_COBOL_PROC) {
			this.typeSourceA = EnumSourceType.COBOL_COPY_PROC;
		} else if (typeObjectA == EnumObject.OBJECT_COPY_COBOL_ENV) {
			this.typeSourceA = EnumSourceType.COBOL_COPY_ENV;
		} else if (typeObjectA == EnumObject.OBJECT_COPY_COBOL_ID) {
			this.typeSourceA = EnumSourceType.COBOL_COPY_ID;
		} else if (typeObjectA == EnumObject.OBJECT_PGM_COBOL) {
			this.typeSourceA = EnumSourceType.COBOL_PROGRAM;
		} else if (typeObjectA == EnumObject.OBJECT_CICS_BMS_SOURCE) {
			this.typeSourceA = EnumSourceType.CICS_BMS;
		} else if (typeObjectA == EnumObject.OBJECT_SQL_SCRIPT
				|| typeObjectA == EnumObject.OBJECT_ENTITY_SQL) {
			this.typeSourceA = EnumSourceType.SQL_SCRIPT;
		} else {
			this.typeSourceA = EnumSourceType.NOT_ASSIGNED;
		}
	}


	/**
	 * @return the typeObjectB
	 */
	public EnumObject getTypeObjectB() {
		return typeObjectB;
	}


	/**
	 * @param typeObjectB the typeObjectB to set
	 */
	public void setTypeObjectB(EnumObject typeObjectB) {
		this.typeObjectB = typeObjectB;
		if (typeObjectB == EnumObject.OBJECT_COPY_COBOL_DATA ) {
			this.typeSourceB = EnumSourceType.COBOL_COPY_DATA;
		} else if (typeObjectB == EnumObject.OBJECT_COPY_COBOL_PROC) {
			this.typeSourceB = EnumSourceType.COBOL_COPY_PROC;
		} else if (typeObjectB == EnumObject.OBJECT_COPY_COBOL_ENV) {
			this.typeSourceB = EnumSourceType.COBOL_COPY_ENV;
		} else if (typeObjectB == EnumObject.OBJECT_COPY_COBOL_ID) {
			this.typeSourceB = EnumSourceType.COBOL_COPY_ID;
		} else if (typeObjectB == EnumObject.OBJECT_PGM_COBOL) {
			this.typeSourceB = EnumSourceType.COBOL_PROGRAM;
		} else if (typeObjectB == EnumObject.OBJECT_CICS_BMS_SOURCE) {
			this.typeSourceB = EnumSourceType.CICS_BMS;
		} else if (typeObjectB == EnumObject.OBJECT_SQL_SCRIPT
				|| typeObjectB == EnumObject.OBJECT_ENTITY_SQL) {
			this.typeSourceB = EnumSourceType.SQL_SCRIPT;
		} else {
			this.typeSourceB = EnumSourceType.NOT_ASSIGNED;
		}	
	}

	/**
	 * @return the typeSource A
	 */
	public EnumSourceType getTypeSourceA() {
		return typeSourceA;
	}
	/**
	 * @param typeSourceA the typeSource to set
	 */
	public void setTypeSourceA(EnumSourceType typeSourceA) {
		this.typeSourceA = typeSourceA;
	}
	
	
	/**
	 * @return the typeSource A
	 */
	public EnumSourceType getTypeSourceB() {
		return typeSourceB;
	}
	/**
	 * @param typeSourceA the typeSource to set
	 */
	public void setTypeSourceB(EnumSourceType typeSourceB) {
		this.typeSourceB = typeSourceB;
	}
	
	

	/**
	 * @return the idObjectA
	 */
	public String getIdObjectA() {
		return idObjectA;
	}


	/**
	 * @param idObjectRelA the idObjectA to set
	 */
	public void setIdObjectA(String idObjectA) {
		this.idObjectA = idObjectA;
	}


	/**
	 * @return the idObjectB
	 */
	public String getIdObjectB() {
		return idObjectB;
	}


	/**
	 * @param idObjectRelB the idObjectRelB to set
	 */
	public void setIdObjectB(String idObjectB) {
		this.idObjectB = idObjectB;
	}





	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityRelation objEntitytRelation = null;
		
		objEntitytRelation = (EntityRelation) obj;
		 
		return this.system.equals(objEntitytRelation.system)
		 &&    this.subSystem.equals(objEntitytRelation.subSystem)
		 &&    this.relation.equals(objEntitytRelation.relation)
		 &&    this.idObjectA.equals(objEntitytRelation.idObjectA)
		 &&    this.typeObjectA.equals(objEntitytRelation.typeObjectA)
		 &&    this.idObjectB.equals(objEntitytRelation.idObjectB)
		 &&    this.typeObjectB.equals(objEntitytRelation.typeObjectB);
		 
	}


	public int compareTo(Object obj) {
		EntityRelation objToCompare = null;
		objToCompare = (EntityRelation) obj;
		
		if (this.system.compareTo(objToCompare.getSystem()) != 0) {
			return this.system.compareTo(objToCompare.getSystem());
		}
		if (this.subSystem.compareTo(objToCompare.getSubSystem()) != 0) {
			return this.subSystem.compareTo(objToCompare.getSubSystem());
		}
		if (this.relation.compareTo(objToCompare.getRelation()) != 0) {
			return this.relation.compareTo(objToCompare.getRelation());
		}
		if (this.idObjectA.compareTo(objToCompare.getIdObjectA()) != 0) {
			return this.idObjectA.compareTo(objToCompare.getIdObjectA());
		}
		if (this.typeObjectA.compareTo(objToCompare.getTypeObjectA()) != 0) {
			return this.typeObjectA.compareTo(objToCompare.getTypeObjectA());
		}
		if (this.idObjectB.compareTo(objToCompare.getIdObjectB()) != 0) {
			return this.idObjectB.compareTo(objToCompare.getIdObjectB());
		}
		if (this.typeObjectB.compareTo(objToCompare.getTypeObjectB()) != 0) {
			return this.typeObjectB.compareTo(objToCompare.getTypeObjectB());
		}
		return 0;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "EntityRelation [relation=" + relation + ", idObjectA=" + idObjectA + ", typeObjectA=" + typeObjectA
				+ ", typeSourceA=" + typeSourceA + ", idObjectB=" + idObjectB + ", typeObjectB=" + typeObjectB
				+ ", typeSourceB=" + typeSourceB + "]";
	}


}
