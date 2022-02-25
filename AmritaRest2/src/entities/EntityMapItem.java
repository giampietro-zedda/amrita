package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumObject;

/**
	* copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityMapItem (MAPI)
	* </h1>
	*  <p>
	* Questa classe mappa la tabella MAPI e descrive un singolo campo di una mappa video.
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

@Entity(name="MapItem")
public class EntityMapItem {

	///////////////////////////////////////////////////////////////////////
    // Data Items MapItem                                         		 //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            			// MAPISYST(PK) Sistema applicativo
	@Id
    @Column(name="subSys")
	private String subSystem = "";         			// MAPISUBS(PK) Sotto sistema applicativo
	@Id
    @Column(name="idObject")
	private String idObject = "";          			// MAPIIDOB(PK) Nome oggetto FORM (map+mapset)
	@Id
    @Column(name="typeObject")
	private EnumObject typeObject = null;       	// MAPITYPO(PK) Tipologia oggetto (T0001)
	@Id
    @Column(name="rowNum")
	private int row = 0;                        	// MAPIFROW(PK) Riga campo
	@Id
    @Column(name="col")
	private int col = 0;                        	// MAPIFCOL(PK) Colonna campo
	
	// Data
	
	// Campi definizioni BMS 
    @Column(name="bmsIdField")
	private String bmsIdField = "";					// MAPIFNAM     Nome campo 
    @Column(name="bmsDescField")
	private String bmsDescField = "";				// MAPIFDSC     Descrizione campo 
    @Column(name="bmsFlgData")
	private boolean bmsFlgData = false;   			// MAPIFDAT     True=Campo Variabile digitabile, False=Campo testata protetto
    @Column(name="bmsLength")
	private int bmsLength = 0;						// MAPILENG     Lunghezza header o campo digitabile  
    @Column(name="bmsNumDec")
	private int bmsNumDec = 0;						// MAPINDEC     Numero decimali 
    @Column(name="bmsInitial")
	private String bmsInitial = "";					// MAPIINIT     Valore iniziale 
    @Column(name="bmsNumGrp")
	private int bmsNumGrp =0;						// MAPINGRP     Numero gruppo
    @Column(name="bmsNumGrpOccurs")
	private int bmsNumGrpOccurs =0;					// MAPINGRO     Numero occorrenze gruppo
    @Column(name="bmsNumericSeparator")
	private boolean bmsNumericSeparator = false;	// MAPINSEP     True=Separatore numerico
    @Column(name="bmsCtrlMinMax")
	private boolean bmsCtrlMinMax = false;		    // MAPICTRM     True Controllo Min/Max 
    @Column(name="bmsFlgIc")
	private boolean bmsFlgIc = false;   			// MAPIFLIC     True=BMS attribute IC attivo
    @Column(name="bmsFlgProt")
	private boolean bmsFlgProt = false;   			// MAPIFLPR     True=BMS attribute PROT attivo
    @Column(name="bmsFlgUnprot")
	private boolean bmsFlgUnprot = false;   		// MAPIFLUN     True=BMS attribute UNPROT attivo
    @Column(name="bmsFlgAskip")
	private boolean bmsFlgAskip = false;   			// MAPIFLAS     True=BMS attribute ASKIP attivo
    @Column(name="bmsFlgNorm")
	private boolean bmsFlgNorm = false;   			// MAPIFLNO     True=BMS attribute NORM attivo
    @Column(name="bmsFlgBrt")
	private boolean bmsFlgBrt = false;   			// MAPIFLBR     True=BMS attribute BRIGHT attivo
    @Column(name="bmsFlgDark")
	private boolean bmsFlgDark = false;   			// MAPIFLDK     True=BMS attribute DARK attivo
    @Column(name="bmsFlgFset")
	private boolean bmsFlgFset = false;   			// MAPIFLFS     True=BMS attribute FSET attivo
	

	
	/*
	 * 
	 * Costruttore 
	 * 
	 */
	public EntityMapItem() {
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
	 * @return the row
	 */
	public int getRow() {
		return row;
	}



	/**
	 * @param row the row to set
	 */
	public void setRow(int row) {
		this.row = row;
	}



	/**
	 * @return the col
	 */
	public int getCol() {
		return col;
	}



	/**
	 * @param col the col to set
	 */
	public void setCol(int col) {
		this.col = col;
	}



	/**
	 * @return the bmsIdField
	 */
	public String getBmsIdField() {
		return bmsIdField;
	}



	/**
	 * @param bmsIdField the bmsIdField to set
	 */
	public void setBmsIdField(String bmsIdField) {
		this.bmsIdField = bmsIdField;
	}



	/**
	 * @return the bmsDescField
	 */
	public String getBmsDescField() {
		return bmsDescField;
	}



	/**
	 * @param bmsDescField the bmsDescField to set
	 */
	public void setBmsDescField(String bmsDescField) {
		this.bmsDescField = bmsDescField;
	}



	/**
	 * @return the bmsFlgData
	 */
	public boolean isBmsFlgData() {
		return bmsFlgData;
	}

	/**
	 * @return the bmsFlgData
	 */
	public boolean getBmsFlgData() {
		return bmsFlgData;
	}



	/**
	 * @param bmsFlgData the bmsFlgData to set
	 */
	public void setBmsFlgData(boolean bmsFlgData) {
		this.bmsFlgData = bmsFlgData;
	}



	/**
	 * @return the bmsLength
	 */
	public int getBmsLength() {
		return bmsLength;
	}



	/**
	 * @param bmsLength the bmsLength to set
	 */
	public void setBmsLength(int bmsLength) {
		this.bmsLength = bmsLength;
	}



	/**
	 * @return the bmsNumDec
	 */
	public int getBmsNumDec() {
		return bmsNumDec;
	}



	/**
	 * @param bmsNumDec the bmsNumDec to set
	 */
	public void setBmsNumDec(int bmsNumDec) {
		this.bmsNumDec = bmsNumDec;
	}



	/**
	 * @return the bmsInitial
	 */
	public String getBmsInitial() {
		return bmsInitial;
	}



	/**
	 * @param bmsInitial the bmsInitial to set
	 */
	public void setBmsInitial(String bmsInitial) {
		this.bmsInitial = bmsInitial;
	}



	/**
	 * @return the bmsNumGrp
	 */
	public int getBmsNumGrp() {
		return bmsNumGrp;
	}



	/**
	 * @param bmsNumGrp the bmsNumGrp to set
	 */
	public void setBmsNumGrp(int bmsNumGrp) {
		this.bmsNumGrp = bmsNumGrp;
	}



	/**
	 * @return the bmsNumGrpOccurs
	 */
	public int getBmsNumGrpOccurs() {
		return bmsNumGrpOccurs;
	}



	/**
	 * @param bmsNumGrpOccurs the bmsNumGrpOccurs to set
	 */
	public void setBmsNumGrpOccurs(int bmsNumGrpOccurs) {
		this.bmsNumGrpOccurs = bmsNumGrpOccurs;
	}



	/**
	 * @return the bmsNumericSeparator
	 */
	public boolean getBmsNumericSeparator() {
		return bmsNumericSeparator;
	}



	/**
	 * @param bmsNumericSeparator the bmsNumericSeparator to set
	 */
	public void setBmsNumericSeparator(boolean bmsNumericSeparator) {
		this.bmsNumericSeparator = bmsNumericSeparator;
	}



	/**
	 * @return the bmsCtrlMinMax
	 */
	public boolean getBmsCtrlMinMax() {
		return bmsCtrlMinMax;
	}



	/**
	 * @param bmsCtrlMinMax the bmsCtrlMinMax to set
	 */
	public void setBmsCtrlMinMax(boolean bmsCtrlMinMax) {
		this.bmsCtrlMinMax = bmsCtrlMinMax;
	}



	/**
	 * @return the bmsFlgIc
	 */
	public boolean isBmsFlgIc() {
		return bmsFlgIc;
	}

	/**
	 * @return the bmsFlgIc
	 */
	public boolean getBmsFlgIc() {
		return bmsFlgIc;
	}



	/**
	 * @param bmsFlgIc the bmsFlgIc to set
	 */
	public void setBmsFlgIc(boolean bmsFlgIc) {
		this.bmsFlgIc = bmsFlgIc;
	}


	/**
	 * @return the bmsFlgProt
	 */
	public boolean isBmsFlgProt() {
		return bmsFlgProt;
	}

	/**
	 * @return the bmsFlgProt
	 */
	public boolean getBmsFlgProt() {
		return bmsFlgProt;
	}



	/**
	 * @param bmsFlgProt the bmsFlgProt to set
	 */
	public void setBmsFlgProt(boolean bmsFlgProt) {
		this.bmsFlgProt = bmsFlgProt;
	}



	/**
	 * @return the bmsFlgUnprot
	 */
	public boolean isBmsFlgUnprot() {
		return bmsFlgUnprot;
	}

	/**
	 * @return the bmsFlgUnprot
	 */
	public boolean getBmsFlgUnprot() {
		return bmsFlgUnprot;
	}



	/**
	 * @param bmsFlgUnprot the bmsFlgUnprot to set
	 */
	public void setBmsFlgUnprot(boolean bmsFlgUnprot) {
		this.bmsFlgUnprot = bmsFlgUnprot;
	}



	/**
	 * @return the bmsFlgAskip
	 */
	public boolean isBmsFlgAskip() {
		return bmsFlgAskip;
	}

	/**
	 * @return the bmsFlgAskip
	 */
	public boolean getBmsFlgAskip() {
		return bmsFlgAskip;
	}



	/**
	 * @param bmsFlgAskip the bmsFlgAskip to set
	 */
	public void setBmsFlgAskip(boolean bmsFlgAskip) {
		this.bmsFlgAskip = bmsFlgAskip;
	}



	/**
	 * @return the bmsFlgNorm
	 */
	public boolean isBmsFlgNorm() {
		return bmsFlgNorm;
	}

	/**
	 * @return the bmsFlgNorm
	 */
	public boolean getBmsFlgNorm() {
		return bmsFlgNorm;
	}



	/**
	 * @param bmsFlgNorm the bmsFlgNorm to set
	 */
	public void setBmsFlgNorm(boolean bmsFlgNorm) {
		this.bmsFlgNorm = bmsFlgNorm;
	}



	/**
	 * @return the bmsFlgBrt
	 */
	public boolean isBmsFlgBrt() {
		return bmsFlgBrt;
	}

	/**
	 * @return the bmsFlgBrt
	 */
	public boolean getBmsFlgBrt() {
		return bmsFlgBrt;
	}



	/**
	 * @param bmsFlgBrt the bmsFlgBrt to set
	 */
	public void setBmsFlgBrt(boolean bmsFlgBrt) {
		this.bmsFlgBrt = bmsFlgBrt;
	}



	/**
	 * @return the bmsFlgDark
	 */
	public boolean isBmsFlgDark() {
		return bmsFlgDark;
	}

	/**
	 * @return the bmsFlgDark
	 */
	public boolean getBmsFlgDark() {
		return bmsFlgDark;
	}



	/**
	 * @param bmsFlgDark the bmsFlgDark to set
	 */
	public void setBmsFlgDark(boolean bmsFlgDark) {
		this.bmsFlgDark = bmsFlgDark;
	}



	/**
	 * @return the bmsFlgFset
	 */
	public boolean isBmsFlgFset() {
		return bmsFlgFset;
	}

	/**
	 * @return the bmsFlgFset
	 */
	public boolean getBmsFlgFset() {
		return bmsFlgFset;
	}



	/**
	 * @param bmsFlgFset the bmsFlgFset to set
	 */
	public void setBmsFlgFset(boolean bmsFlgFset) {
		this.bmsFlgFset = bmsFlgFset;
	}




	/**
	 * @param system the system to set
	 */
	public void setSystem(String system) {
		this.system = system;
	}

	
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityMapItem objD = null;
		
		objD = (EntityMapItem) obj;
		 
		return this.system.equals(objD.system)
		 &&    this.subSystem.equals(objD.subSystem)
		 &&    this.idObject.equals(objD.idObject)
		 &&    this.typeObject == objD.typeObject
		 &&    this.row == objD.row
		 &&    this.col == objD.col;	 
	}
}
