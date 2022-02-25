
package analyzer;

import java.io.Serializable;

import enums.EnumPrecompilerReservedWords;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlFullSelectLinkedTo
 * </h1>
 * <p>
 * Descrive una singola <tt>UNION</tt>, <tt>EXCEPT</tt> o <tt>INTERSECT</tt> a una full-select o subselect.<br> 
 * <p>
 * Completa la modellazione di una full-select {@link SqlFullSelect}<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlSelectStatement
 * @see SqlSubselectSelectInto 
 * @see SqlFullSelect 
*/

public class SqlFullSelectLinkedTo implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	private SqlSubselectSelectInto subselect = null;		// SELECT * ....
	private SqlFullSelect fullSelect = null;				// (full-select) ...
    private EnumPrecompilerReservedWords typeLink = null;   // UNION|EXCEPT|INTERSECT
	private boolean isLinkedToSubselect = false;            // UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect
	private boolean isLinkedToFullselect = false;           // UNION|EXCEPT|INTERSECT |DISTINCT|ALL (full-select)
	
    // Opzioni presenti
	private boolean isDistinct = false;                     // UNION|EXCEPT|INTERSECT DISTINCT
	private boolean isAll = false;                          // UNION|EXCEPT|INTERSECT ALL
    
    
	/**
	 * Costruttore vuoto
	 */
	public SqlFullSelectLinkedTo() {
		super();

	}


	/**
	 * Restituisce la subselect connessa in <tt>UNION EXCEPT o INTERSECT</tt><br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @return the subselect
	 */
	public SqlSubselectSelectInto getSubselect() {
		return subselect;
	}


	/**
	 * Imposta la subselect connessa in <tt>UNION EXCEPT o INTERSECT</tt><br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @param subselect the subselect to set
	 */
	public void setSubselect(SqlSubselectSelectInto subselect) {
		this.subselect = subselect;
	}


	/**
	 * Restituisce la full-select connessa in <tt>UNION EXCEPT o INTERSECT</tt><br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @return the fullSelect
	 */
	public SqlFullSelect getFullSelect() {
		return fullSelect;
	}


	/**
	 * Imposta la full-select connessa in <tt>UNION EXCEPT o INTERSECT</tt><br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @param fullSelect the fullSelect to set
	 */
	public void setFullSelect(SqlFullSelect fullSelect) {
		this.fullSelect = fullSelect;
	}


	/**
	 * Restituisce il tipo di connessione che può essere <tt>UNION EXCEPT o INTERSECT</tt><br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @return the typeLink
	 */
	public EnumPrecompilerReservedWords getTypeLink() {
		return typeLink;
	}


	/**
	 * Imposta il tipo di connessione che può essere <tt>UNION EXCEPT o INTERSECT</tt><br>
	 * <p>
	 * @param typeLink the typeLink to set
	 */
	public void setTypeLink(EnumPrecompilerReservedWords typeLink) {
		this.typeLink = typeLink;
	}


	/**
	 * Restituisce se la connessione è con una subselect<br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @return the isSubselect
	 */
	public boolean isLinkedToSubselect() {
		return isLinkedToSubselect;
	}


	/**
	 * Imposta se la connessione è con una subselect<br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @param isLinkedToSubselect the isLinkedToSubselect to set
	 */
	public void setLinkedToSubselect(boolean isLinkedToSubselect) {
		this.isLinkedToSubselect = isLinkedToSubselect;
	}


	/**
	 * Restituisce se la connessione è con una full-select<br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @return the isLinkedToFullselect
	 */
	public boolean isLinkedToFullselect() {
		return isLinkedToFullselect;
	}


	/**
	 * Imposta se la connessione è con una full-select<br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @param isLinkedToFullselect the isLinkedToFullselect to set
	 */
	public void setLinkedToFullselect(boolean isLinkedToFullselect) {
		this.isLinkedToFullselect = isLinkedToFullselect;
	}


	/**
	 * Restituisce se codificata la clausola <tt>DISTINCT</tt><br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @return the isDistinct
	 */
	public boolean isDistinct() {
		return isDistinct;
	}


	/**
	 * Imposta se codificata la clausola <tt>DISTINCT</tt><br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @param isDistinct the isDistinct to set
	 */
	public void setDistinct(boolean isDistinct) {
		this.isDistinct = isDistinct;
	}


	/**
	 * Restituisce se codificata la clausola <tt>ALL</tt><br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @return the isAll
	 */
	public boolean isAll() {
		return isAll;
	}


	/**
	 * Imposta se codificata la clausola <tt>ALL</tt><br>
	 * <p>
	 * si tratta di:<br>
	 * <p>
	 * UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect(full-select)
	 * <p>
	 * @param isAll the isAll to set
	 */
	public void setAll(boolean isAll) {
		this.isAll = isAll;
	}


	
}
