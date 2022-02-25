package entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumObject;

/**
	* copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityMapDescriptor  
	* </h1>
	*  <p>
	* Questa classe mappa la tabella MapDescriptor  e descrive le caratteristiche generali di una mappa video.
	* Il descrittore ha carattere generale ma sono presenti tutte le informazioni specifiche relative
	* alle mappe Cics BMS.
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

@Entity(name="MapDescriptor")
public class EntityMapDescriptor {

	///////////////////////////////////////////////////////////////////////
    // Data Items MapDescriptor                                          //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            		// MAPDSYST(PK) Sistema applicativo
	@Id
    @Column(name="subSys")
	private String subSystem = "";         		// MAPDSUBS(PK) Sotto sistema applicativo
	@Id
    @Column(name="idObject")
	private String idObject = "";          		// MAPDIDOB(PK) Nome oggetto map
	@Id
    @Column(name="typeObject")
	private EnumObject typeObject = null;       // MAPDTYPO(PK) Tipologia oggetto (T0001)
	
	// Data
    @Column(name="idObjectMapset")
	private String idObjectMapset = "";         // MAPDOMPS     Nome oggetto Cics Mapset EnumObject.CICS_MAPSET
    @Column(name="typeObjectMap")
 	private EnumObject typeObjectMap = null;       // MAPDTYPO(PK) Tipologia oggetto (T0001)
    @Column(name="idObjectMap")
	private String idObjectMap = "";            // MAPDOMAP     Nome oggetto Cics Map    EnumObject.CICS_MAP 
    @Column(name="typeObjectMapset")
	private EnumObject typeObjectMapset = null;       // MAPDTYPO(PK) Tipologia oggetto (T0001)
    @Column(name="typeObjectBmsSource")
	private EnumObject typeObjectBmsSource = null;  //Tipologia CICS_BMS (T0001)
    @Column(name="idObjectBmsSource")  
	private String idObjectBmsSource = "";      //  

    @Column(name="fieldCursor")
	private String fieldCursor = "";			// MAPDFCUR     Nome campo cursore
    @Column(name="rowsSize")
	private int rowsSize = 24;					// MAPDSROW     Righe mappa 
    @Column(name="colsSize")
	private int colsSize = 80;					// MAPDSCOL     Colonne mappa 
    @Column(name="rowBegin")
	private int rowBegin = 0;					// MAPDBROW     Riga inizio nello schermo
    @Column(name="colBegin")
	private int colBegin = 0;					// MAPDBCOL     Colonna inizio nello schermo
	
	/*
	 * 
	 * Costruttore 
	 * 
	 */
	public EntityMapDescriptor() {
		super();
		typeObject = EnumObject.OBJECT_CICS_MAP;		
		typeObjectMap = EnumObject.OBJECT_CICS_MAP;		
		typeObjectMapset = EnumObject.OBJECT_CICS_MAPSET;		
		typeObjectBmsSource = EnumObject.OBJECT_CICS_BMS_SOURCE;		
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
	 * @return the idObjectMapset
	 */
	public String getIdObjectMapset() {
		return idObjectMapset;
	}


	/**
	 * @param idObjectMapset the idObjectMapset to set
	 */
	public void setIdObjectMapset(String idObjectMapset) {
		this.idObjectMapset = idObjectMapset;
	}


	/**
	 * @return the idObjectMap
	 */
	public String getIdObjectMap() {
		return idObjectMap;
	}


	/**
	 * @param idObjectMap the idObjectMap to set
	 */
	public void setIdObjectMap(String idObjectMap) {
		this.idObjectMap = idObjectMap;
	}


	/**
	 * @return the fieldCursor
	 */
	public String getFieldCursor() {
		return fieldCursor;
	}


	/**
	 * @param fieldCursor the fieldCursor to set
	 */
	public void setFieldCursor(String fieldCursor) {
		this.fieldCursor = fieldCursor;
	}


	/**
	 * @return the rowsSize
	 */
	public int getRowsSize() {
		return rowsSize;
	}


	/**
	 * @param rowsSize the rowsSize to set
	 */
	public void setRowsSize(int rowsSize) {
		this.rowsSize = rowsSize;
	}


	/**
	 * @return the colsSize
	 */
	public int getColsSize() {
		return colsSize;
	}


	/**
	 * @param colsSize the colsSize to set
	 */
	public void setColsSize(int colsSize) {
		this.colsSize = colsSize;
	}


	/**
	 * @return the rowBegin
	 */
	public int getRowBegin() {
		return rowBegin;
	}


	/**
	 * @param rowBegin the rowBegin to set
	 */
	public void setRowBegin(int rowBegin) {
		this.rowBegin = rowBegin;
	}


	/**
	 * @return the colBegin
	 */
	public int getColBegin() {
		return colBegin;
	}


	/**
	 * @param colBegin the colBegin to set
	 */
	public void setColBegin(int colBegin) {
		this.colBegin = colBegin;
	}
	
	/**
	 * @return the typeObjectMap
	 */
	public EnumObject getTypeObjectMap() {
		return typeObjectMap;
	}


	/**
	 * @param typeObjectMap the typeObjectMap to set
	 */
	public void setTypeObjectMap(EnumObject typeObjectMap) {
		this.typeObjectMap = typeObjectMap;
	}


	/**
	 * @return the typeObjectMapset
	 */
	public EnumObject getTypeObjectMapset() {
		return typeObjectMapset;
	}


	/**
	 * @param typeObjectMapset the typeObjectMapset to set
	 */
	public void setTypeObjectMapset(EnumObject typeObjectMapset) {
		this.typeObjectMapset = typeObjectMapset;
	}


	/**
	 * @return the typeObjectBmsSource
	 */
	public EnumObject getTypeObjectBmsSource() {
		return typeObjectBmsSource;
	}


	/**
	 * @param typeObjectBmsSource the typeObjectBmsSource to set
	 */
	public void setTypeObjectBmsSource(EnumObject typeObjectBmsSource) {
		this.typeObjectBmsSource = typeObjectBmsSource;
	}


	/**
	 * @return the idObjectBmsSource
	 */
	public String getIdObjectBmsSource() {
		return idObjectBmsSource;
	}


	/**
	 * @param idObjectBmsSource the idObjectBmsSource to set
	 */
	public void setIdObjectBmsSource(String idObjectBmsSource) {
		this.idObjectBmsSource = idObjectBmsSource;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityMapDescriptor objD = null;
		
		objD = (EntityMapDescriptor) obj;
		 
		return this.system.equals(objD.system)
		 &&    this.subSystem.equals(objD.subSystem)
		 &&    this.idObject.equals(objD.idObject)
		 &&    this.typeObject == objD.typeObject;
		 
	}
	
}
