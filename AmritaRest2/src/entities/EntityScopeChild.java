package entities;

import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedForAny;
import analyzer.DataBaseMappedForAnys;
import analyzer.DataBaseMappedTable;

/**
	* copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityScopeChild (ScopeChild, SCPC)
	* </h1>
	*  <p>
	* Questa classe mappa la tabella ScopeChild e permette di associare a un campo di validità (ovvero a un
	* insieme di programmi), uno o più sottoinsiemi degli stessi, descritti da uno scope specifico su 
	* {@link EntityScopeSection}. Ogni sottoinsieme, indicato in questa tabella, viene poi sempre descritto
	* nell tabella SCPH ScopeHeader con tutte le informazioni necessarie quali numero di oggetti e programmi.
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

@DataBaseMappedTable("ScopeChild")
@DataBaseMappedColumns(java_db_col = {  
		                   // Colonne primary key
			                "system 			sys  	     PK"
						   ,"subSystem 			subSys  	 PK"    
						   ,"idScope 			idScope 	 PK"    
						   ,"idScopeChild 		idScopeChild PK"  
						   // Colonne senza vincoli di chiave (No key)
                       }
         )

		 /*
@DataBaseMappedForAnys(forAny = {@DataBaseMappedForAny(entity = "EntityScopeHeader",
                                                       colsBound = {"system      system"
                                                                   ,"subSystem   subSystem"    
                                                                   ,"idScope 	 idScope"    
                                                                   }
                                                      )
                                } 
                      )
*/

public class EntityScopeChild {

	///////////////////////////////////////////////////////////////////////
    // Data Items ScopeChild                                             //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	private String system = "";            		// SCPCSYST(PK) Sistema applicativo
	private String subSystem = "";         		// SCPCSUBS(PK) Sotto sistema applicativo
	private String idScope = "";          	    // SCPCSCOP(PK) Identificativo scope
	private String idScopeChild = "";           // SCPCSCPC(PK) Identificativo scope figlio, definito in SCPH
	
	// Data

    
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityScopeChild() {
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
	 * @return the idScope
	 */
	public String getIdScope() {
		return idScope;
	}

	/**
	 * @param idScope the idScope to set
	 */
	public void setIdScope(String idScope) {
		this.idScope = idScope;
	}

	/**
	 * @return the idScopeChild
	 */
	public String getIdScopeChild() {
		return idScopeChild;
	}

	/**
	 * @param idScopeChild the idScopeChild to set
	 */
	public void setIdScopeChild(String idScopeChild) {
		this.idScopeChild = idScopeChild;
	}


}
