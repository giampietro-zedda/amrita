
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDescriptorView
 * </h1>
 * <p>
 * Descrive tutte le informazioni di una View Sql, a fronte di uno statement CREATE VIEW>
 * <p>
 * 
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlView 
*/

public class SqlView implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////////
    // Variabili di istanza
    //////////////////////////////////////////////////////////////////////////
    
	private String viewFullName = "";                           							// Nome completo qualificato
	private String viewOwner = "";                           		  						// Nome owner se qualificazione su 3 o 2 campi
	private String viewName = "";                           		  						// Nome tabella applicativo
	private boolean isWithCascadeCheckOption = false;                						// WITH CASCADE CHECK OPTION
	private boolean isWithLocalCheckOption = false;                							// WITH LOCAL CHECK OPTION
	private boolean isWithCheckOption = false;                								// WITH CHECK OPTION
	private ArrayList<String> al_column = null;                   							// CREATE VIEW name (cols) 
    private InstructionSql fullSelect = null;                                     			// CREATE VIEW name (cols) AS full-select
    private ArrayList<SqlCommonTableExpression> al_commonTableExpression = null;	// CREATE VIEW name (cols) AS WITH common-table-expressions  
     
    
	/**
	 * Costruttore vuoto
	 */
	public SqlView() {
		super();
		al_column = new  ArrayList<String> ();
		al_commonTableExpression = new  ArrayList<SqlCommonTableExpression> ();
	}


	/**
	 * Restituisce il nome qualificato della view,
	 * come <tt>Owner.viewName</tt>
	 * 
	 * @return the viewFullName
	 */
	public String getViewFullName() {
		return viewFullName;
	}


	/**
	 * Imposta il nome qualificato della view,
	 * come <tt>Owner.viewName</tt>
	 * 
	 * @param viewFullName the viewFullName to set
	 */
	public void setViewFullName(String viewFullName) {
		this.viewFullName = viewFullName;
	}



	/**
	 * Restituisce l'owner della view, a fronte
	 * di <tt>sOwner.viewName</tt> <br>
	 * <p>
	 * 
	 * @return the viewOwner
	 */
	public String getViewOwner() {
		return viewOwner;
	}


	/**
	 * Imposta l'owner della view, a fronte
	 * di <tt>sOwner.viewName</tt> <br>
	 * <p>
	 * 
	 * @param viewOwner the viewOwner to set
	 */
	public void setViewOwner(String viewOwner) {
		this.viewOwner = viewOwner;
	}


	/**
	 * Restituisce il nome  della view, a fronte
	 * di <tt>Owner.viewName</tt>  oppure di
	 * viewName per nome non qualificato.
	 * <p>
	 * 
	 * @return the viewName
	 */
	public String getViewName() {
		return viewName;
	}


	/**
	 * Imposta il nome  della view, a fronte
	 * di <tt>Owner.viewName</tt>  oppure di
	 * viewName per nome non qualificato.
	 * <p>
	 * 
	 * @param viewName the viewName to set
	 */
	public void setViewName(String viewName) {
		this.viewName = viewName;
	}


	/**
	 * Restituisce se presente l'opzione <tt>WITH CASCADE CHECK OPTION</tt> 
	 * <p>
	 * @return the isWithCascadeCheckOption
	 */
	public boolean isWithCascadeCheckOption() {
		return isWithCascadeCheckOption;
	}


	/**
	 * Imposta se presente l'opzione <tt>WITH CASCADE CHECK OPTION</tt> 
	 * <p>
	 * @param isWithCascadeCheckOption the isWithCascadeCheckOption to set
	 */
	public void setWithCascadeCheckOption(boolean isWithCascadeCheckOption) {
		this.isWithCascadeCheckOption = isWithCascadeCheckOption;
	}


	/**
	 * Restituisce se presente l'opzione <tt>WITH LOCAL CHECK OPTION</tt> 
	 * <p>
	 * @return the isWithLocalCheckOption
	 */
	public boolean isWithLocalCheckOption() {
		return isWithLocalCheckOption;
	}


	/**
	 * Imposta se presente l'opzione <tt>WITH LOCAL CHECK OPTION</tt> 
	 * <p>
	 * @param isWithLocalCheckOption the isWithLocalCheckOption to set
	 */
	public void setWithLocalCheckOption(boolean isWithLocalCheckOption) {
		this.isWithLocalCheckOption = isWithLocalCheckOption;
	}


	/**
	 * Restituisce se presente l'opzione <tt>WITH CHECK OPTION</tt> 
	 * <p>
	 * @return the isWithCheckOption
	 */
	public boolean isWithCheckOption() {
		return isWithCheckOption;
	}


	/**
	 * Imposta se presente l'opzione <tt>WITH CHECK OPTION</tt> 
	 * <p>
	 * @param isWithCheckOption the isWithCheckOption to set
	 */
	public void setWithCheckOption(boolean isWithCheckOption) {
		this.isWithCheckOption = isWithCheckOption;
	}


	/**
	 * Restituisce le colonne della view.<br>
	 * <p>
	 * Se no specificate restituisce una struttura vuota.
	 * <p>
	 * @return the al_column
	 */
	public ArrayList<String> getColumns() {
		return al_column;
	}


	/**
	 * Imposta le colonne della view.<br>
	 * <p>
	 * <p>
	 * @param alColumn the al_column to set
	 */
	public void setColumns(ArrayList<String> alColumn) {
		al_column = alColumn;
	}


	/**
	 * Restituisce l'istruzione full-select che descrive la view.<br>
	 * <p>
	 * @return the fullSelect
	 */
	public InstructionSql getFullSelect() {
		return fullSelect;
	}


	/**
	 * Imposta l'istruzione full-select che descrive la view.<br>
	 * <p>
	 * @param fullSelect the fullSelect to set
	 */
	public void setFullSelect(InstructionSql fullSelect) {
		this.fullSelect = fullSelect;
	}


	/**
	 * Restituisce i table-common-expression definiti dall' opzione <tt>WITH common-table-expressions</tt> <br>
	 * <p>
	 * @return the al_commonTableExpression
	 */
	public ArrayList<SqlCommonTableExpression> getCommonTableExpressions() {
		return this.al_commonTableExpression;
	}


	/**
	 * Imposta i table-common-expression definiti dall' opzione <tt>WITH common-table-expressions</tt> <br>
	 * <p>
	 * @param al_commonTableExpression the al_commonTableExpression to set
	 */
	public void setCommonTableExpressions(ArrayList<SqlCommonTableExpression> al_commonTableExpression) {
		this.al_commonTableExpression = al_commonTableExpression;
	}



	
}
