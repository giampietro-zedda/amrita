package entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumSourceType;

/**
	 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityObjectOption (ObjectOption, OBJO)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella OBJO e descrive le possibili optioni di un oggetto
	 * descritto da EntityObject (Object)
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

@Entity(name="ObjectOption")
public class EntityObjectOption {

	///////////////////////////////////////////////////////////////////////
    // Data Items ObjectOption                                           //                                                     
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            		// Sistema applicativo
	@Id
    @Column(name="subSys")
	private String subSystem = "";         		// Sotto sistema applicativo
	@Id
    @Column(name="idObject")
	private String idObject = "";          		// Nome oggetto (es. programma, file fisico,..)
	@Id
    @Column(name="typeObject")
	private EnumObject typeObject = null;       // Tipologia oggetto  (T0001)
	@Id
    @Column(name="typeSource")
	private EnumSourceType typeSource = null;   // Tipologia sorgente (T0002)			
	@Id
    @Column(name="optionObject")
	private EnumObjectOption option = null;     // Opzione codificata (T0004)										
	
	// Data
    @Column(name="textValue")
	private String textValue = "";              // Info specifiche acquisite in analisi (nick text X visualizzazione) o User Tag											

	
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityObjectOption() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		typeSource = EnumSourceType.NOT_ASSIGNED;
		option = EnumObjectOption.NOT_ASSIGNED;
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
	 * @return the option
	 */
	public EnumObjectOption getOption() {
		return option;
	}


	/**
	 * @param option the option to set
	 */
	public void setOption(EnumObjectOption option) {
		this.option = option;
	}


	/**
	 * @return the textValue
	 */
	public String getTextValue() {
		return textValue;
	}


	/**
	 * @param textValue the textValue to set
	 */
	public void setTextValue(String textValue) {
		this.textValue = textValue;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityObjectOption objEntityObjectOption = null;
		
		objEntityObjectOption = (EntityObjectOption) obj;
		 
		return this.system.equals(objEntityObjectOption.system)
		 &&    this.subSystem.equals(objEntityObjectOption.subSystem)
		 &&    this.idObject.equals(objEntityObjectOption.idObject)
		 &&    this.typeObject == objEntityObjectOption.typeObject
		 &&    this.option == objEntityObjectOption.option;
	}

	
	public int compareTo(Object obj) {
		EntityObjectOption objToCompare = null;
		objToCompare = (EntityObjectOption) obj;
		
		if (this.system.compareTo(objToCompare.getSystem()) != 0) {
			return this.system.compareTo(objToCompare.getSystem());
		}
		if (this.subSystem.compareTo(objToCompare.getSubSystem()) != 0) {
			return this.subSystem.compareTo(objToCompare.getSubSystem());
		}
		if (this.idObject.compareTo(objToCompare.getIdObject()) != 0) {
			return this.idObject.compareTo(objToCompare.getIdObject());
		}
		if (this.typeObject.compareTo(objToCompare.getTypeObject()) != 0) {
			return this.typeObject.compareTo(objToCompare.getTypeObject());
		}
		if (this.option.compareTo(objToCompare.getOption()) != 0) {
			return this.option.compareTo(objToCompare.getOption());
		}

		return 0;
	}	

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return  option.toString() + " " +  typeObject.toString() + " IdObject:" + idObject;

	}





	
}
