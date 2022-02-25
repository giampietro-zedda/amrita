
package analyzer;

import java.io.Serializable;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDeclareCursorStatement
 * </h1>
 * <p>
 * Descrive una DECLARE CURSOR statement che dichiara un cursore su una select-statement o una statement-name.<br> 
 * <p>
 * {@link SqlSelectStatement}<br>
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 04/Ago/2011 
 * @see SqlDeclareCursorStatement 
*/

public class SqlDeclareCursorStatement implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	// Descrizione corpo istruzione
	private String cursorName = "";                    			// DECLARE cursor-name ....
	private String statementName = "";                    		// DECLARE cursor-name .... FOR statement
    private SqlSelectStatement selectStatement = null;          // DECLARE cursor-name .... FOR select-statement
	
    // Opzioni presenti
    private boolean isByStatementName = false;                   //
    private boolean isBySelectStatement = false;                //
    private boolean isNoScrollCursor = false;                   //    			 
    private boolean isAsensitiveScrollCursor = false;           //    			 
    private boolean isInsensitiveScrollCursor = false;          //    			 
    private boolean isSensitiveScrollCursor = false;     //    			 
    private boolean isSensitiveDynamicScrollCursor = false;     //    			 
    private boolean isSensitiveStaticScrollCursor = false;      //    			 
    private boolean isScrollCursor = false;                     //    			 
   
    private boolean isWithHold = false;                         //    			 
    private boolean isWithReturn = false;                       //    			 
    private boolean isWithReturnToCaller = false;               //    			 
    private boolean isWithReturnToClient = false;               //    			 
    private boolean isWithRowsetPositioning = false;            //    			 
    private boolean isWithoutHold = false;                      //    			 
    private boolean isWithoutReturn = false;         //    			 
    private boolean isWithoutReturnToCaller = false;         //    			 
    private boolean isWithoutReturnToClient = false;         //    			 
    private boolean isWithoutRowsetPositioning = false;         //    			 
  
    
	/**
	 * Costruttore 
	 */
	public SqlDeclareCursorStatement() {
		super();

	}


	/**
	 * Restituisce il nome del cursore dichiarato<br>
	 * <p>
	 * @return the cursorName
	 */
	public String getCursorName() {
		return cursorName;
	}


	/**
	 * Imposta il nome del cursore dichiarato<br>
	 * <p>
	 * @param cursorName the cursorName to set
	 */
	public void setCursorName(String cursorName) {
		this.cursorName = cursorName;
	}


	/**
	 * Restituisce il nome dello statement preparato su cui utilizzare il cursore<br>
	 * <p>
	 * @return the statementName
	 */
	public String getStatementName() {
		return statementName;
	}


	/**
	 * Imposta il nome dello statement preparato su cui utilizzare il cursore<br>
	 * <p>
	 * @param statementName the statementName to set
	 */
	public void setStatementName(String statementName) {
		this.statementName = statementName;
	}


	/**
	 * Restituisce l'istruzione delect definita dal cursore<br>
	 * <p>
	 * @return the selectStatement
	 */
	public SqlSelectStatement getSelectStatement() {
		return selectStatement;
	}


	/**
	 * Imposta l'istruzione delect definita dal cursore<br>
	 * <p>
	 * @param selectStatement the selectStatement to set
	 */
	public void setSelectStatement(SqlSelectStatement selectStatement) {
		this.selectStatement = selectStatement;
	}


	/**
	 * @return the isByStatementName
	 */
	public boolean isByStatementName() {
		return isByStatementName;
	}


	/**
	 * @param isByStatementName the isByStatementName to set
	 */
	public void setByStatementName(boolean isByStatementName) {
		this.isByStatementName = isByStatementName;
	}


	/**
	 * @return the isBySelectStatement
	 */
	public boolean isBySelectStatement() {
		return isBySelectStatement;
	}


	/**
	 * @param isBySelectStatement the isBySelectStatement to set
	 */
	public void setBySelectStatement(boolean isBySelectStatement) {
		this.isBySelectStatement = isBySelectStatement;
	}


	/**
	 * @return the isNoScrollCursor
	 */
	public boolean isNoScrollCursor() {
		return isNoScrollCursor;
	}


	/**
	 * @param isNoScrollCursor the isNoScrollCursor to set
	 */
	public void setNoScrollCursor(boolean isNoScrollCursor) {
		this.isNoScrollCursor = isNoScrollCursor;
	}


	/**
	 * @return the isAsensitiveScrollCursor
	 */
	public boolean isAsensitiveScrollCursor() {
		return isAsensitiveScrollCursor;
	}


	/**
	 * @param isAsensitiveScrollCursor the isAsensitiveScrollCursor to set
	 */
	public void setAsensitiveScrollCursor(boolean isAsensitiveScrollCursor) {
		this.isAsensitiveScrollCursor = isAsensitiveScrollCursor;
	}


	/**
	 * @return the isInsensitiveScrollCursor
	 */
	public boolean isInsensitiveScrollCursor() {
		return isInsensitiveScrollCursor;
	}


	/**
	 * @param isInsensitiveScrollCursor the isInsensitiveScrollCursor to set
	 */
	public void setInsensitiveScrollCursor(boolean isInsensitiveScrollCursor) {
		this.isInsensitiveScrollCursor = isInsensitiveScrollCursor;
	}


	/**
	 * @return the isSensitiveDynamicScrollCursor
	 */
	public boolean isSensitiveDynamicScrollCursor() {
		return isSensitiveDynamicScrollCursor;
	}


	/**
	 * @param isSensitiveDynamicScrollCursor the isSensitiveDynamicScrollCursor to set
	 */
	public void setSensitiveDynamicScrollCursor(
			boolean isSensitiveDynamicScrollCursor) {
		this.isSensitiveDynamicScrollCursor = isSensitiveDynamicScrollCursor;
	}


	/**
	 * @return the isSensitiveScrollCursor
	 */
	public boolean isSensitiveScrollCursor() {
		return isSensitiveScrollCursor;
	}


	/**
	 * @param isSensitiveScrollCursor the isSensitiveScrollCursor to set
	 */
	public void setSensitiveScrollCursor(boolean isSensitiveScrollCursor) {
		this.isSensitiveScrollCursor = isSensitiveScrollCursor;
	}


	/**
	 * @return the isSensitiveStaticScrollCursor
	 */
	public boolean isSensitiveStaticScrollCursor() {
		return isSensitiveStaticScrollCursor;
	}


	/**
	 * @param isSensitiveStaticScrollCursor the isSensitiveStaticScrollCursor to set
	 */
	public void setSensitiveStaticScrollCursor(boolean isSensitiveStaticScrollCursor) {
		this.isSensitiveStaticScrollCursor = isSensitiveStaticScrollCursor;
	}


	/**
	 * @return the isScrollCursor
	 */
	public boolean isScrollCursor() {
		return isScrollCursor;
	}


	/**
	 * @param isScrollCursor the isScrollCursor to set
	 */
	public void setScrollCursor(boolean isScrollCursor) {
		this.isScrollCursor = isScrollCursor;
	}


	/**
	 * @return the isWithoutHold
	 */
	public boolean isWithoutHold() {
		return isWithoutHold;
	}


	/**
	 * @param isWithoutHold the isWithoutHold to set
	 */
	public void setWithoutHold(boolean isWithoutHold) {
		this.isWithoutHold = isWithoutHold;
	}


	/**
	 * @return the isWithoutReturn
	 */
	public boolean isWithoutReturn() {
		return isWithoutReturn;
	}


	/**
	 * @param isWithoutReturn the isWithoutReturn to set
	 */
	public void setWithoutReturn(boolean isWithoutReturn) {
		this.isWithoutReturn = isWithoutReturn;
	}


	/**
	 * @return the isWithoutReturnToCaller
	 */
	public boolean isWithoutReturnToCaller() {
		return isWithoutReturnToCaller;
	}


	/**
	 * @param isWithoutReturnToCaller the isWithoutReturnToCaller to set
	 */
	public void setWithoutReturnToCaller(boolean isWithoutReturnToCaller) {
		this.isWithoutReturnToCaller = isWithoutReturnToCaller;
	}


	/**
	 * @return the isWithoutReturnToClient
	 */
	public boolean isWithoutReturnToClient() {
		return isWithoutReturnToClient;
	}


	/**
	 * @param isWithoutReturnToClient the isWithoutReturnToClient to set
	 */
	public void setWithoutReturnToClient(boolean isWithoutReturnToClient) {
		this.isWithoutReturnToClient = isWithoutReturnToClient;
	}


	/**
	 * @return the isWithHold
	 */
	public boolean isWithHold() {
		return isWithHold;
	}


	/**
	 * @param isWithHold the isWithHold to set
	 */
	public void setWithHold(boolean isWithHold) {
		this.isWithHold = isWithHold;
	}


	/**
	 * @return the isWithReturn
	 */
	public boolean isWithReturn() {
		return isWithReturn;
	}


	/**
	 * @param isWithReturn the isWithReturn to set
	 */
	public void setWithReturn(boolean isWithReturn) {
		this.isWithReturn = isWithReturn;
	}


	/**
	 * @return the isWithReturnToCaller
	 */
	public boolean isWithReturnToCaller() {
		return isWithReturnToCaller;
	}


	/**
	 * @param isWithReturnToCaller the isWithReturnToCaller to set
	 */
	public void setWithReturnToCaller(boolean isWithReturnToCaller) {
		this.isWithReturnToCaller = isWithReturnToCaller;
	}


	/**
	 * @return the isWithReturnToClient
	 */
	public boolean isWithReturnToClient() {
		return isWithReturnToClient;
	}


	/**
	 * @param isWithReturnToClient the isWithReturnToClient to set
	 */
	public void setWithReturnToClient(boolean isWithReturnToClient) {
		this.isWithReturnToClient = isWithReturnToClient;
	}


	/**
	 * @return the isWithoutRowsetPositioning
	 */
	public boolean isWithoutRowsetPositioning() {
		return isWithoutRowsetPositioning;
	}


	/**
	 * @param isWithoutRowsetPositioning the isWithoutRowsetPositioning to set
	 */
	public void setWithoutRowsetPositioning(boolean isWithoutRowsetPositioning) {
		this.isWithoutRowsetPositioning = isWithoutRowsetPositioning;
	}


	/**
	 * @return the isWithRowsetPositioning
	 */
	public boolean isWithRowsetPositioning() {
		return isWithRowsetPositioning;
	}


	/**
	 * @param isWithRowsetPositioning the isWithRowsetPositioning to set
	 */
	public void setWithRowsetPositioning(boolean isWithRowsetPositioning) {
		this.isWithRowsetPositioning = isWithRowsetPositioning;
	}

	/**
	 * Restituisce il numero massimo di subselect nested<br>
	 * <br>
	 * <p>
	 * @return the subselect nesting max
	 */
	public  int getMaxSubselectNestedNumber() {
		int maxSubselectNestedNumber = 0;
		maxSubselectNestedNumber = this.selectStatement.getMaxSubselectNestedNumber();
		return maxSubselectNestedNumber;
	}



}
