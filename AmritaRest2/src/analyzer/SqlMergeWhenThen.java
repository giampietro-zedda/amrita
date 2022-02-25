
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlMergeWhen
 * </h1>
 * <p>
 * Descrive una singola occorrenza del costrutto WHEN matching-condition THEN modification-operation di un MERGE statement.<br> 
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlMerge
 * @see SqlExpression
*/

public class SqlMergeWhenThen implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	// Descrizione corpo istruzione
    private ArrayList<String> al_updateSetColumnName = null;      		        // MERGE ... THEN UPDATE SET (col1, coln) = (expr1, exprn)
    private ArrayList<SqlExpression> al_updateSetExpression = null;             // MERGE ... THEN UPDATE SET (col1, coln) = (expr1, exprn)
    private ArrayList<String> al_insertColumnName = null;                       // MERGE ... THEN INSERT  (col1, coln) VALUES expr|(expr1, exprn)
    private ArrayList<SqlExpression> al_insertValuesExpression = null;          // MERGE ... THEN INSERT  (col1, coln) VALUES expr|(expr1, exprn)
    
    // Opzioni presenti
    private boolean isMatched = false;                     				        // WHEN MATCHED
    private boolean isNotMatched = false;                     				    // WHEN NOT MATCHED
    private boolean isUpdateSet = false;                     				    // WHEN ... UPDATE SET ...
    private boolean isInsert = false;                     				        // WHEN ... INSERT ( cols ) VALUES ( expr )
    
    
	/**
	 * Costruttore 
	 */
	public SqlMergeWhenThen() {
		super();

		al_updateSetColumnName = new ArrayList<String> (); 		     
	    al_updateSetExpression = new ArrayList<SqlExpression> ();          
	    al_insertColumnName = new ArrayList<String> ();                     
	    al_insertValuesExpression = new ArrayList<SqlExpression> ();         
	}


	/**
	 * Restituisce le colonne specificate dal costrutto MERGE ... THEN UPDATE SET (col1, coln) = (expr1, exprn)<br>
	 * <p>
	 * @return the al_updateSetColumnName
	 */
	public ArrayList<String> getUpdateSetColumnNames() {
		return al_updateSetColumnName;
	}


	/**
	 * Imposta le colonne specificate dal costrutto MERGE ... THEN UPDATE SET (col1, coln) = (expr1, exprn)<br>
	 * <p>
	 * @param al_updateSetColumnName the al_updateSetColumnName to set
	 */
	public void setUpdateSetColumnNames(ArrayList<String> al_updateSetColumnName) {
		this.al_updateSetColumnName = al_updateSetColumnName;
	}


	/**
	 * Restituisce le espressioni specificate dai costrutti:<br>
	 * <p>
	 *  MERGE ... THEN UPDATE SET (col1, coln) = (expr1, exprn)<br>
     *  MERGE ... THEN UPDATE SET col1 = expr1, coln = exprn<br>
	 * <p>
	 * @return the al_updateSetExpression
	 */
	public ArrayList<SqlExpression> getUpdateSetExpressions() {
		return al_updateSetExpression;
	}


	/**
	 * Imposta le espressioni specificate dai costrutti:<br>
	 * <p>
	 *  MERGE ... THEN UPDATE SET (col1, coln) = (expr1, exprn)<br>
     *  MERGE ... THEN UPDATE SET col1 = expr1, coln = exprn<br>
	 * <p>
	 * @param al_updateSetExpression the al_updateSetExpression to set
	 */
	public void setUpdateSetExpressions(ArrayList<SqlExpression> al_updateSetExpression) {
		this.al_updateSetExpression = al_updateSetExpression;
	}


	/**
	 * Restituisce le colonne specificate dal costrutto<br>
	 * <p>
	 *  MERGE ... THEN INSERT  (col1, coln) VALUES wxpr|(expr1, exprn)<br>
 	 * <p>
	 * @return the al_insertColumnName
	 */
	public ArrayList<String> getInsertColumnNames() {
		return al_insertColumnName;
	}


	/**
	 * Imposta le colonne specificate dal costrutto<br>
	 * <p>
	 *  MERGE ... THEN INSERT  (col1, coln) VALUES wxpr|(expr1, exprn)<br>
 	 * <p>
	 * @param al_insertColumnName the al_insertColumnName to set
	 */
	public void setAl_insertColumnName(ArrayList<String> al_insertColumnName) {
		this.al_insertColumnName = al_insertColumnName;
	}


	/**
	 * Restituisce le espressioni specificate dal costrutto<br>
	 * <p>
	 *  MERGE ... THEN INSERT  (col1, coln) VALUES wxpr|(expr1, exprn)<br>
 	 * <p>
	 * @return the al_insertValuesExpression
	 */
	public ArrayList<SqlExpression> getInsertValuesExpressions() {
		return al_insertValuesExpression;
	}


	/**
	 * Imposta le espressioni specificate dal costrutto<br>
	 * <p>
	 *  MERGE ... THEN INSERT  (col1, coln) VALUES wxpr|(expr1, exprn)<br>
 	 * <p>
	 * @param al_insertValuesExpression the al_insertValuesExpression to set
	 */
	public void setInsertValuesExpressions(ArrayList<SqlExpression> al_insertValuesExpression) {
		this.al_insertValuesExpression = al_insertValuesExpression;
	}


	/**
	 * Restituisce se presente l'opzione nella WHEN<br>
	 * <p>
	 *  WHEN MATCHED ...<br>
 	 * <p>
	 * @return the isMatched
	 */
	public boolean isMatched() {
		return isMatched;
	}


	/**
	 * Imposta se presente l'opzione nella WHEN<br>
	 * <p>
	 *  WHEN MATCHED ...<br>
 	 * <p>
	 * @param isMatched the isMatched to set
	 */
	public void setMatched(boolean isMatched) {
		this.isMatched = isMatched;
	}


	/**
	 * Restituisce se presente l'opzione nella WHEN<br>
	 * <p>
	 *  WHEN NOT MATCHED ...<br>
 	 * <p>
	 * @return the isNotMatched
	 */
	public boolean isNotMatched() {
		return isNotMatched;
	}


	/**
	 * Imposta se presente l'opzione nella WHEN<br>
	 * <p>
	 *  WHEN NOT MATCHED ...<br>
 	 * <p>
	 * @param isNotMatched the isNotMatched to set
	 */
	public void setNotMatched(boolean isNotMatched) {
		this.isNotMatched = isNotMatched;
	}


	/**
	 * Restituisce se a foronte della WHEN si effettua uina operazione di update<br>
	 * <p>
	 *  WHEN ... UPDATE SET ...<br>
 	 * <p>
	 * @return the isUpdateSet
	 */
	public boolean isUpdateSet() {
		return isUpdateSet;
	}


	/**
	 * Imposta se a foronte della WHEN si effettua uina operazione di update<br>
	 * <p>
	 *  WHEN ... UPDATE SET ...<br>
 	 * <p>
	 * @param isUpdateSet the isUpdateSet to set
	 */
	public void setUpdateSet(boolean isUpdateSet) {
		this.isUpdateSet = isUpdateSet;
	}


	/**
	 * Restituisce se a foronte della WHEN si effettua uina operazione di insert<br>
	 * <p>
	 *  WHEN ... INSERT ( cols ) VALUES ( expr )<br>
 	 * <p>
	 * @return the isInsert
	 */
	public boolean isInsert() {
		return isInsert;
	}


	/**
	 * Imposta se a foronte della WHEN si effettua uina operazione di insert<br>
	 * <p>
	 *  WHEN ... INSERT ( cols ) VALUES ( expr )<br>
 	 * <p>
	 * @param isInsert the isInsert to set
	 */
	public void setInsert(boolean isInsert) {
		this.isInsert = isInsert;
	}



}
