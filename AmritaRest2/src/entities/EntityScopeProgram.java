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
	 * EntityScopeProgram (ScopeProgram, SCPP)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella ScopeProgram e descrive un programma appartenente al campo di validità.
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
	 * @see EntityDynamicFieldSubValue
	 * @see EntityDynamicFieldSubSetting
*/

@DataBaseMappedTable("ScopeProgram")
@DataBaseMappedColumns(java_db_col = {  
		                   // Colonne primary key
			                "system 			sys  	      PK"
						   ,"subSystem 			subSys  	  PK"    
						   ,"idScope 			idScope 	  PK"    
						   ,"idProgram 			idProgram 	  PK"    
						   ,"typeObjectPgm 		typeObjectPgm PK"    
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
public class EntityScopeProgram {

	///////////////////////////////////////////////////////////////////////
    // Data Items ScopeProgram                                           //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	private String system = "";            		// SCPPSYST(PK) Sistema applicativo
	private String subSystem = "";         		// SCPPSUBS(PK) Sotto sistema applicativo
	private String idScope = "";          	    // SCPPSCOP(PK) Identificativo scope  
	private String idProgram = "";          	// SCPPPNAM(PK) Nome programma 
	private EnumObject typeObjectPgm = null;    // SCPPTOBP(PK) Tipo oggetto programma (T0001) 
	
	
	// Data
	
	
	/*	
	 * 
	 * Costruttore 
	 *
	 */
	public EntityScopeProgram() {
		super();
		typeObjectPgm = EnumObject.NOT_ASSIGNED;
		
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
	 * @return the idProgram
	 */
	public String getIdProgram() {
		return idProgram;
	}

	/**
	 * @param idProgram the idProgram to set
	 */
	public void setIdProgram(String idProgram) {
		this.idProgram = idProgram;
	}

	/**
	 * @return the typeObjectPgm
	 */
	public EnumObject getTypeObjectPgm() {
		return typeObjectPgm;
	}

	/**
	 * @param typeObjectPgm the typeObjectPgm to set
	 */
	public void setTypeObjectPgm(EnumObject typeObjectPgm) {
		this.typeObjectPgm = typeObjectPgm;
	}

}
