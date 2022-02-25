package entities;

import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedForEachs;
import analyzer.DataBaseMappedTable;

/**
	* copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityScopeHeader (ScopeHeader, SCPH)
	* </h1>
	*  <p>
	* Questa classe mappa la tabella ScopeHeader con informazioni identificative di testata dello scope.
	* Un campo di validità o scope identifica un insieme di programmi, individuati a fronte di determinate
	* regole di estrazione, che vanno dal contenuto di particolari istruzioni nel sorgente o user tag, fino 
	* alla presenza di specifiche relazioni del programma con altri oggetti oppure a specifici I-O
	* effettuati dai programmi. <br>
	* Ciò permette di limitare la complessità di oggetti e relazioni, ai soli direttamente o 
	* indirettamente correlati all'insieme di programmi in scope.
	* Per esempio un scope può individuare una manutenzione su una specifica area che lavora su un insieme
	* ben determinato di oggetti.<br>
	* Lo scope può essere concepito come una struttura ad albero di livello indefinito che qualifica
	* con dettaglio sempre maggiore insiemi di programmi. Ogni nodo di questa struttura identifica uno
	* specifico insieme di programmi, idintificato da un nome. Ogni nodo rappresenta pertanto un
	* ulteriore scope.<br>
	* Per realizzare questa struttura ad albero viene utilizzata anche l'entity {@link EntityScopeChild} 
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
public class EntityScopeHeader {

	///////////////////////////////////////////////////////////////////////
    // Data Items ScopeHeader                                            //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	private String system = "";            		// SCPHSYST(PK) Sistema applicativo
	private String subSystem = "";         		// SCPHSUBS(PK) Sotto sistema applicativo
	private String idScope = "";          	    // SCPHSCOP(PK) Identificativo scope  
	
	// Data
	private String descScope = "";		        // SCPHDESC     Descrizione scope
	private int numPrograms = 0;          		// SCPSNUMP     Numero programmi in scope 
	private int numObjects = 0;          		// SCPSNUMO     Numero oggetti correlati ai programmi (anche altri programmi)
	private String dateCreation = "";			// SCPHDTCR     Data creazione Scope AAAAMMGG
	private String dateUpdate = "";			    // SCPHDTUP     Data update AAAAMMGG
	

	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityScopeHeader() {
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
	 * @return the descScope
	 */
	public String getDescScope() {
		return descScope;
	}


	/**
	 * @param descScope the descScope to set
	 */
	public void setDescScope(String descScope) {
		this.descScope = descScope;
	}


	/**
	 * @return the numPrograms
	 */
	public int getNumPrograms() {
		return numPrograms;
	}


	/**
	 * @param numPrograms the numPrograms to set
	 */
	public void setNumPrograms(int numPrograms) {
		this.numPrograms = numPrograms;
	}


	/**
	 * @return the numObjects
	 */
	public int getNumObjects() {
		return numObjects;
	}


	/**
	 * @param numObjects the numObjects to set
	 */
	public void setNumObjects(int numObjects) {
		this.numObjects = numObjects;
	}


	/**
	 * @return the dateCreation
	 */
	public String getDateCreation() {
		return dateCreation;
	}


	/**
	 * @param dateCreation the dateCreation to set
	 */
	public void setDateCreation(String dateCreation) {
		this.dateCreation = dateCreation;
	}


	/**
	 * @return the dateUpdate
	 */
	public String getDateUpdate() {
		return dateUpdate;
	}


	/**
	 * @param dateUpdate the dateUpdate to set
	 */
	public void setDateUpdate(String dateUpdate) {
		this.dateUpdate = dateUpdate;
	}



}
