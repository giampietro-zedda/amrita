package entities;
import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedTable;
import enums.EnumObject;
import enums.EnumRelation;
import enums.EnumTable;

/**
	 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityScopeRelation (ScopeRelation, SCPR)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella ScopeRelation che elenca le possibili relazioni associate ai programmi.
	 * Insieme alla tabella ScopeObject, questa tabella concorre a ridurre la complessità dell'intero
	 * sistema/sottosistema, attraverso i soli oggetti che interessano, raggruppati in uno scope.
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

@DataBaseMappedTable("ScopeRelation")
@DataBaseMappedColumns(java_db_col = {  
		                   // Colonne primary key
			                "system 			system 	     PK"
						   ,"subSystem 			subSystem 	 PK"    
						   ,"idScope 			idScope 	 PK"    
						   ,"idObject 			idObject 	 PK"    
						   ,"typeObject 		typeObject 	 PK"    
						   ,"relation 			relation 	 PK"  
						   ,"idObject 			idObject 	 PK"    
						   ,"typeObject 		typeObject 	 PK" 						   
						   // Colonne senza vincoli di chiave (No key)
                       }
         )

         public class EntityScopeRelation {

	///////////////////////////////////////////////////////////////////////
    // Data Items ScopeRelation                                          //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	private String system = "";            		// SCPRSYST(PK) Sistema applicativo
	private String subSystem = "";         		// SCPRSUBS(PK) Sotto sistema applicativo
	private String idScope = "";          	    // SCPRSCOP(PK) Identificativo scope  
	private String idObject = "";          	    // SCPRNOBJ(PK) Nome oggetto correlato ai programmi in scope
	private EnumObject typeObject = null;       // SCPRTYPO(PK) Tipo oggetto correlato (T0001)   
	private EnumRelation relation = null;       // SCPRRELA(PK) Relazione codificata programma/oggetto (T0034)										
	private String idObjectRelated = "";        // SCPRNOBJ(PK) Nome oggetto correlato ai programmi in scope
	private EnumObject typeObjectRelated = null;// SCPRTYPO(PK) Tipo oggetto correlato (T0001)   
	
	// Data
	
	/*
	 * 
	 * Costruttore
	 * 
	 */
	
	public EntityScopeRelation() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		relation = EnumRelation.NOT_ASSIGNED;
		 

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
	 * @return the idObjectRelated
	 */
	public String getIdObjectRelated() {
		return idObjectRelated;
	}

	/**
	 * @param idObject the idObject to set
	 */
	public void setIdObjectRelated(String idObjectRelated) {
		this.idObjectRelated = idObjectRelated;
	}

	/**
	 * @return the typeObjectRelated
	 */
	public EnumObject getTypeObjectRelated() {
		return typeObjectRelated;
	}

	/**
	 * @param typeObject the typeObjectRelated to set
	 */
	public void setTypeObjectRelated(EnumObject typeObjectRelated) {
		this.typeObject = typeObjectRelated;
	}



}
