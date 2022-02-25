package entities;
import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedForAny;
import analyzer.DataBaseMappedForAnys;
import analyzer.DataBaseMappedRuleTable;
import analyzer.DataBaseMappedRuleTables;
import analyzer.DataBaseMappedTable;
import enums.EnumDataItemGeneric;
import enums.EnumDateFormat;
import enums.EnumTable;

/**
	 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityTableStructure (TableStructure, TBST)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella TableStructure che descrive un singolo item (campo) della tabella.
	 * E' possibile definire nome, posizione, formato, lunghezza, obbligatorietà e altro ancora.
	 * Il nome di ogni elemento è definito obbligatoriamente di 8 caratteri di cui i primi 4 sono
	 * il nome della tabella.
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

@DataBaseMappedTable("TableStructure")
@DataBaseMappedColumns(java_db_col = {  
		                   // Colonne primary key
			                "system 			sys     	PK"
						   ,"subSystem 			subSys   	PK"    
						   ,"numTable 			numTable 	PK"  
						   ,"posItem 			posItem 	PK"    
						   ,"idItem             idItem 	    PK"  
						   // Colonne senza vincoli di chiave (No key)
						   ,"typeItem           typeItem 	NK"    
						   ,"dateFormat 		dateFormat 	NK"    
						   ,"obbl 				obbl    	NK"    
						   ,"valDefault 		valDefault 	NK"    
						   ,"lngBytes 			lngBytes 	NK"    
						   ,"numInt 			numInt   	NK"    
						   ,"numDec 			numDec  	NK"     
						}
         )

@DataBaseMappedForAnys(forAny = {@DataBaseMappedForAny(entity = "EntityTableHeader", colsBound = {"sys sys", "subSys subSys", "numTable numTable"})
                                } 
                      )

@DataBaseMappedRuleTables(tablesRule = { @DataBaseMappedRuleTable(tabRef = "A", tabId = "30", tabSystem = true, tabNumCol = "numTableT030", tabKeyColsBound={"idItem"})
                                       , @DataBaseMappedRuleTable(tabRef = "B", tabId = "31", tabSystem = true, tabNumCol = "numTableT031", tabKeyColsBound={"idItem"})
                                       , @DataBaseMappedRuleTable(tabRef = "C", tabId = "14", tabSystem = true, tabNumCol = "numTableT014", tabKeyColsBound={"typeItem"})
                                       , @DataBaseMappedRuleTable(tabRef = "D", tabId = "22", tabSystem = true, tabNumCol = "numTableT022", tabKeyColsBound={"dateFormat"})
                                       }
                         ) 

public class EntityTableStructure {

	///////////////////////////////////////////////////////////////////////
    // Data Items TableItemDefinition                                    //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	private String system = "";            		// TBSTSYST(PK) Sistema applicativo
	private String subSystem = "";         		// TBSTSUBS(PK) Sotto sistema applicativo
	private int numTable = 0;       			// TBSTNTAB(PK) Tipo (numero) tabella (T0000)
	private int posItem = 0;                    // TBSTPOSI(PK) Posizione item nell'area dati 0-based
	private String idItem = "";       	        // TBSTITEM(PK) Nome item (campo) (T0030, T0031, descrizione lunga e breve)
	                                            //              Key=idItem, key1N=numTable
	
	// Data
	private EnumDataItemGeneric typeItem = null;// TBSTTYPI     Tipo item numerico, alfanumerico e data (T0014)
	private EnumDateFormat dateFormat= null;    // TBSTDTFO     Formato data se typeItem = DATA_ITEM_DATE (T0022)
	private boolean obbl = false;		        // TBSTOBBL     Obbligatorietà
	private String valDefault = "";		        // TBSTDFLT     Valore di default in accordo a formato/interi/decimali
	private int lngBytes = 0;					// TBSTSIZE     Lunghezza complessiva in bytes, -1 indica tutta l'area disponibile
	private int numInt = 0;					    // TBSTNINT     Numero interi
	private int numDec = 0;					    // TBSTNDEC     Numero decimali 

	
	
	/*
	 * 
	 * Costruttore
	 * 
	 */
	public EntityTableStructure() {
		super();
		typeItem = EnumDataItemGeneric.NOT_ASSIGNED;
		dateFormat = EnumDateFormat.NOT_ASSIGNED;

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
	 * @return the numTable
	 */
	public int getNumTable() {
		return numTable;
	}


	/**
	 * @param numTable the numTable to set
	 */
	public void setNumTable(int numTable) {
		this.numTable = numTable;
	}


	/**
	 * @return the idItem
	 */
	public String getIdItem() {
		return idItem;
	}


	/**
	 * @param idItem the idItem to set
	 */
	public void setIdItem(String idItem) {
		this.idItem = idItem;
	}



	/**
	 * @return the typeItem
	 */
	public EnumDataItemGeneric getTypeItem() {
		return typeItem;
	}


	/**
	 * @param typeItem the typeItem to set
	 */
	public void setTypeItem(EnumDataItemGeneric typeItem) {
		this.typeItem = typeItem;
	}


	/**
	 * @return the dateFormat
	 */
	public EnumDateFormat getDateFormat() {
		return dateFormat;
	}


	/**
	 * @param dateFormat the dateFormat to set
	 */
	public void setDateFormat(EnumDateFormat dateFormat) {
		this.dateFormat = dateFormat;
	}


	/**
	 * @return the obbl
	 */
	public boolean getObbl() {
		return obbl;
	}


	/**
	 * @param obbl the obbl to set
	 */
	public void setObbl(boolean obbl) {
		this.obbl = obbl;
	}


	/**
	 * @return the posItem
	 */
	public int getPosItem() {
		return posItem;
	}


	/**
	 * @param posItem the posItem to set
	 */
	public void setPosItem(int posItem) {
		this.posItem = posItem;
	}


	/**
	 * @return the valDefault
	 */
	public String getValDefault() {
		return valDefault;
	}


	/**
	 * @param valDefault the valDefault to set
	 */
	public void setValDefault(String valDefault) {
		this.valDefault = valDefault;
	}


	/**
	 * @return the lngBytes
	 */
	public int getLngBytes() {
		return lngBytes;
	}


	/**
	 * @param lngBytes the lngBytes to set
	 */
	public void setLngBytes(int lngBytes) {
		this.lngBytes = lngBytes;
	}


	/**
	 * @return the numInt
	 */
	public int getNumInt() {
		return numInt;
	}


	/**
	 * @param numInt the numInt to set
	 */
	public void setNumInt(int numInt) {
		this.numInt = numInt;
	}


	/**
	 * @return the numDec
	 */
	public int getNumDec() {
		return numDec;
	}


	/**
	 * @param numDec the numDec to set
	 */
	public void setNumDec(int numDec) {
		this.numDec = numDec;
	}
	
}
