package entities;
import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedForEachs;
import analyzer.DataBaseMappedRuleTable;
import analyzer.DataBaseMappedRuleTables;
import analyzer.DataBaseMappedTable;
import enums.EnumTable;

/**
	 * Copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityTableHeader (TableHeader, TBHD)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella TableHeader che descrive una tabella del sistema generalizzato di
	 * gestione tabelle. Tale sistema permette di definire le tabelle NON di database, di trascodifica
	 * in lingua e/o di configurazione, attraverso la definizione del formato generale della tabella
	 * e dei campi che la compongono. <br>
	 * Il sistema di gestione tabelle è gestito dalle entities EntityTableHeader, EntityTableStructure e EntityTableData.
	 * Il nome della tabella è definito di 4 caratteri. E' prevista una tabella particolare con nome
	 * '000' che contiene i nomi di tutte le tabelle definite.
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

@DataBaseMappedTable("TableHeader")
@DataBaseMappedColumns(java_db_col = {  
		                   // Colonne primary key
			                "system 			sys             PK"
						   ,"subSystem 			subSys   	    PK"    
						   ,"numTable 			numTable 	    PK"  
						   // Colonne senza vincoli di chiave (No key)
						   ,"descTb				descTb          NK"    
						   ,"tableSystem 		tableSystem     NK"    
						   ,"tableStructured 	tableStructured NK"    
						   ,"enumJava 	        enumJava        NK"    
						   ,"dtCreation 		dtCreation      NK"    
						   ,"dtUpdate 			dtUpdate        NK"    
						   ,"tmCreation 		tmCreation      NK"    
						   ,"tmUpdate 			tmUpdate        NK"     
	                       }
         			 )
public class EntityTableHeader {
	
	///////////////////////////////////////////////////////////////////////
    // Data Items TableHeader                                            //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	private String system = "";            		// TBHDSYST(PK) Sistema applicativo
	private String subSystem = "";         		// TBHDSUBS(PK) Sotto sistema applicativo
	private int numTable = 0;       			// TBHDTTAB(PK) Tipo (numero) tabella (T0000)
	
	// Data
	private String descTb = "";					// TBHDDESC     Descrizione tabella iniziale inglese
	private boolean tableSystem = false;		// TBHDTBSY     True indica tabella di sistema
	private boolean tableStructured = false;	// TBHDTBST     True indica tabella strutturata in campi
	private String enumJava = "";	            // TBHDENUM     Enumerazione java che descrive il dominio
	private String dtCreation = "";				// TBHDDCRE     Data creazione 		(AAAAMMGG)
	private String dtUpdate = "";				// TBHDDUPD     Data aggiornamento  (AAAAMMGG)
	private String tmCreation = "";				// TBHDTCRE     Ora creazione 		(HHMMSSCC)
	private String tmUpdate = "";				// TBHDTUPD     Ora aggiornamento  (HHMMSSCC)
    	
	/*
	 * 
	 * Costruttore
	 * 
	 */
	public EntityTableHeader() {
		super();
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
	 * @return the descTb
	 */
	public String getDescTb() {
		return descTb;
	}


	/**
	 * @param descTb the descTb to set
	 */
	public void setDescTb(String descTb) {
		this.descTb = descTb;
	}


	/**
	 * @return the tableSystem
	 */
	public boolean getTableSystem() {
		return tableSystem;
	}


	/**
	 * @param tableSystem the tableSystem to set
	 */
	public void setTableSystem(boolean tableSystem) {
		this.tableSystem = tableSystem;
	}


	/**
	 * @return the tableStructured
	 */
	public boolean getTableStructured() {
		return tableStructured;
	}


	/**
	 * @param tableStructured the tableStructured to set
	 */
	public void setTableStructured(boolean tableStructured) {
		this.tableStructured = tableStructured;
	}


	/**
	 * @return the enumJava
	 */
	public String getEnumJava() {
		return enumJava;
	}


	/**
	 * @param enumJava the enumJava to set
	 */
	public void setEnumJava(String enumJava) {
		this.enumJava = enumJava;
	}


	/**
	 * @return the dtCreation
	 */
	public String getDtCreation() {
		return dtCreation;
	}


	/**
	 * @param dtCreation the dtCreation to set
	 */
	public void setDtCreation(String dtCreation) {
		this.dtCreation = dtCreation;
	}


	/**
	 * @return the dtUpdate
	 */
	public String getDtUpdate() {
		return dtUpdate;
	}


	/**
	 * @param dtUpdate the dtUpdate to set
	 */
	public void setDtUpdate(String dtUpdate) {
		this.dtUpdate = dtUpdate;
	}


	/**
	 * @return the tmCreation
	 */
	public String getTmCreation() {
		return tmCreation;
	}


	/**
	 * @param tmCreation the tmCreation to set
	 */
	public void setTmCreation(String tmCreation) {
		this.tmCreation = tmCreation;
	}


	/**
	 * @return the tmUpdate
	 */
	public String getTmUpdate() {
		return tmUpdate;
	}


	/**
	 * @param tmUpdate the tmUpdate to set
	 */
	public void setTmUpdate(String tmUpdate) {
		this.tmUpdate = tmUpdate;
	}


}
