package entities;
import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedForAny;
import analyzer.DataBaseMappedForAnys;
import analyzer.DataBaseMappedForEachs;
import analyzer.DataBaseMappedRuleTable;
import analyzer.DataBaseMappedRuleTables;
import analyzer.DataBaseMappedTable;
import enums.EnumForwardCaptionCustomization;
import enums.EnumTable;

/**
	 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityCustomization (CUST) 
	 * </h1>
	 *  <p>
	 * Questa classe descrive l'entity EntityCustomization, ovvero la tabella con le personalizzazioni delle funzioni di forward.
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
	 * @see Analyzer
	 * @see EntityCustomization 
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

@DataBaseMappedTable("Customization")
@DataBaseMappedColumns(java_db_col = {  
		                   // Colonne primary key
						    "userName 			  		userName 	      PK"    
						   ,"function 			  		function 	      PK"    
						   ,"language 			  		language 	      PK"
						   ,"typeCustomization  		typeCustomization PK"
						   ,"componentName 		  		componentName 	  PK"    
						   ,"subComponentName 		  	subComponentName  PK"    
						   ,"propertyName 		  		propertyName PK" 
						   // Colonne dati
						   ,"value	  					value 	          NK"    
						   ,"valueClass	  				valueClass 	      NK"    
						   ,"numTableT058      			numTableT058      NK" 
                       }
         )

@DataBaseMappedForAnys(forAny = {@DataBaseMappedForAny(entity = "EntityUser",
                                                       colsBound = {"userName userName"}
                                                      )
                                  } 
                       )


@DataBaseMappedRuleTables(tablesRule = { @DataBaseMappedRuleTable(tabRef = "A", tabId = "58", tabSystem = true, tabNumCol = "numTableT058", tabKeyColsBound={"typeCustomization"})
  									   }
                         ) 

public class EntityCustomization {

	///////////////////////////////////////////////////////////////////////
    // Data Items Customization                                                                                                
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	private String userName = "";         							    // CUSTUSER(PK) Codice utente
	private String function = "";          								// CUSTFUNC(PK) Nome funzione (classe che la implementa)
	private String language = "";            							// CUSTLANG(PK) Liguaggio in formato Locale ("en", "it", ... )
	private EnumForwardCaptionCustomization typeCustomization = null; 	// CUSTCUST(PK) Tipologia customizzazione (T058)
	private String componentName = "";       							// CUSTCOMP(PK) Nome componente, come una tabella JTable
	private String subComponentName = "";       						// CUSTSUBC(PK) Nome subcomponente, come la colonna di un componente JTable
	private String propertyName = "";       							// CUSTPRPT(PK) Nome proprietà java da cistomizzare sul componennte
	
	// Data 
	private String value = "";       									// CUSTVALU     Valore customizzato (o più valori)
	private String valueClass = "";       								// CUSTVALC     Classe valore (String, Integer, Boolean, Color, Font)
	
	// Numeri tabelle
	private int numTableT058 = 0;    			    					// CUSTT058     Numero Tabella 
	
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityCustomization() {
		super();
		
		typeCustomization = EnumForwardCaptionCustomization.NOT_ASSIGNED;
		
		// Numeri tabelle
//		numTableT058 = EnumTable.EnumForwardCaptionCustomization.getNumTable();   

	}

	/**
	 * @return the language
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * @param language the language to set
	 */
	public void setLanguage(String language) {
		this.language = language;
	}

	/**
	 * @return the userName
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * @param user the userName to set
	 */
	public void setUserName(String userName) {
		this.userName = userName;
	}

	/**
	 * @return the function
	 */
	public String getFunction() {
		return function;
	}

	/**
	 * @param function the function to set
	 */
	public void setFunction(String function) {
		this.function = function;
	}

	/**
	 * @return the typeCustomization
	 */
	public EnumForwardCaptionCustomization getTypeCustomization() {
		return typeCustomization;
	}

	/**
	 * @param typeCustomization the typeCustomization to set
	 */
	public void setTypeCustomization(
			EnumForwardCaptionCustomization typeCustomization) {
		this.typeCustomization = typeCustomization;
	}

	/**
	 * @return the componentName
	 */
	public String getComponentName() {
		return componentName;
	}

	/**
	 * @param componentName the componentName to set
	 */
	public void setComponentName(String componentName) {
		this.componentName = componentName;
	}

	/**
	 * @return the subComponentName
	 */
	public String getSubComponentName() {
		return subComponentName;
	}

	/**
	 * @param subComponentName the subComponentName to set
	 */
	public void setSubComponentName(String subComponentName) {
		this.subComponentName = subComponentName;
	}

	/**
	 * @return the propertyName
	 */
	public String getPropertyName() {
		return propertyName;
	}

	/**
	 * @param propertyName the propertyName to set
	 */
	public void setPropertyName(String propertyName) {
		this.propertyName = propertyName;
	}

	/**
	 * @return the value
	 */
	public String getValue() {
		return value;
	}

	/**
	 * @param value the value to set
	 */
	public void setValue(String value) {
		this.value = value;
	}

	/**
	 * @return the valueClass
	 */
	public String getValueClass() {
		return valueClass;
	}

	/**
	 * @param valueClass the valueClass to set
	 */
	public void setValueClass(String valueClass) {
		this.valueClass = valueClass;
	}

	/**
	 * @return the numTableT057
	 */
	public int getNumTableT058() {
		return numTableT058;
	}

	/**
	 * @param numTableT058 the numTableT057 to set
	 */
	public void setNumTableT058(int numTableT058) {
		this.numTableT058 = numTableT058;
	}
	


}
