package entities;
import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedForAny;
import analyzer.DataBaseMappedForAnys;
import analyzer.DataBaseMappedTable;
import enums.EnumObject;
import enums.EnumTable;

/**
	 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityScopeObject (ScopeObject, SCPO)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella ScopeObject che codifica un oggetto appartenente allo scope.
	 * Le righe di questa tabella vengono generate a partire dai programmi in scope memorizzati 
	 * su ScopeProgram. Di ogni programma vengono recuperate tutte le relazioni con tutti gli altri oggetti,
	 * confezionando un work set esaustivo e congruente di oggetti collegati a un insieme di programmi.
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
	 * @see EntityDynamicFieldSubValue
	 * @see EntityDynamicFieldSubSetting
*/

@DataBaseMappedTable("ScopeObject")
@DataBaseMappedColumns(java_db_col = {  
		                   // Colonne primary key
			                "system 			sys  	     PK"
						   ,"subSystem 			subSys  	 PK"    
						   ,"idScope 			idScope 	 PK"    
						   ,"idObject 			idObject 	 PK"    
						   ,"typeObject 		typeObject 	 PK"    
                       }
         )

		 /*
@DataBaseMappedForAnys(forAny = {@DataBaseMappedForAny(entity = "EntityScopeHeader",
                                                       colsBound = {"system      system"
                                                                   ,"subSystem   subSystem"    
                                                                   ,"idScope 	  idScope"    
                                                                   }
                                                      )
                                } 
                      )

*/

public class EntityScopeObject {

	///////////////////////////////////////////////////////////////////////
    // Data Items ScopeObhect                                            //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	private String system = "";            		// SCPOSYST(PK) Sistema applicativo
	private String subSystem = "";         		// SCPOSUBS(PK) Sotto sistema applicativo
	private String idScope = "";          	    // SCPOSCOP(PK) Identificativo scope  
	private String idObject = "";          	    // SCPONOBJ(PK) Nome oggetto correlato ai programmi in scope
	private EnumObject typeObject = null;       // SCPOTYPO(PK) Tipo oggetto correlato ai programmi in scope  (T0001)   
	
	// Data
	
    
	/*
	 * 
	 * Costruttore
	 * 
	 */
	
	public EntityScopeObject() {
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

	
}
