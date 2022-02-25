package entities;

import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedForAny;
import analyzer.DataBaseMappedForAnys;
import analyzer.DataBaseMappedTable;
import enums.EnumLanguage;

/**
	 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityTableData (TableData, TBDT)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella TBDT che descrive una riga con i dati della tabella.
	 * La riga contiene i dati secondo il tracciato definito dall'entity {@link EntityTableStructure}.
	 * I dati, anche quelli numerici, sono memorizzati in formato carattere.
	 * L'interfaccia con i programmi applicativi ha il compito di restituire i dati formattandoli
	 * in modo completamente indipendete per l'applicazione. 
	 * E' prevista un'interfaccia in gradi di fornire i soli campi richiesti.
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

@DataBaseMappedTable("TableData")
@DataBaseMappedColumns(java_db_col = {  
		                   // Colonne primary key
			                "system 			sys  	     PK"
						   ,"subSystem 			subSys  	 PK"    
						   ,"numTable 			numTable 	 PK"  
						   ,"language 			language 	 PK"    
						   ,"keySeq 			keySeq 	     PK"  
						   ,"keyVal 			keyVal 	     PK"  
						   // Colonne senza vincoli di chiave (No key)
						   ,"rowData 			rowData 	 NK"    
	                       }
         )

@DataBaseMappedForAnys(forAny = {@DataBaseMappedForAny(entity = "EntityTableHeader", colsBound = {"system system", "subSystem subSystem", "numTable numTable"})
                                } 
                      )

public class EntityTableData {

	///////////////////////////////////////////////////////////////////////
    // Data Items TableData                                              //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	private String system = "";            		// TBDTSYST(PK) Sistema applicativo
	private String subSystem = "";         		// TBDTSUBS(PK) Sotto sistema applicativo
	private int numTable = 0;       			// TBDTTTAB(PK) Tipo (numero) tabella  
	private EnumLanguage language = null;       // TBDTLANG(PK) Linguaggio (T0040)
	private String keySeq = "";       			// TBDTKSEQ(PK) Sequenza chiave per evenruale ordinamento valori (4 char)
	private String keyVal = "";       			// TBDTKVAL(PK) Chiave elemento di tabella
	
	// Data
	private String rowData = "";				// TBDTRDAT 	Riga dati tabella con elementi (items) descritti da TBIT
    
	
	/*
	 * 
	 * Costruttore
	 * 
	 */
	public EntityTableData() {
		super();
		language = EnumLanguage.NOT_ASSIGNED; 

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
	 * @return the language
	 */
	public EnumLanguage getLanguage() {
		return language;
	}



	/**
	 * @param language the language to set
	 */
	public void setLanguage(EnumLanguage language) {
		this.language = language;
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
	 * @return the keySeq
	 */
	public String getKeySeq() {
		return keySeq;
	}



	/**
	 * @param keySeq the keySeq to set
	 */
	public void setKeySeq(String keySeq) {
		this.keySeq = keySeq;
	}



	/**
	 * @return the keyVal
	 */
	public String getKeyVal() {
		return keyVal;
	}



	/**
	 * @param keyVal the keyVal to set
	 */
	public void setKeyVal(String keyVal) {
		this.keyVal = keyVal;
	}


	/**
	 * @return the rowData
	 */
	public String getRowData() {
		return rowData;
	}



	/**
	 * @param rowData the rowData to set
	 */
	public void setRowData(String rowData) {
		this.rowData = rowData;
	}


}
