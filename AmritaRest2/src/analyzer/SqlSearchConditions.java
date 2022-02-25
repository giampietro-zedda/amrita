
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumSqlExpressionElementType;
import enums.EnumSqlOperator;
import enums.EnumSqlPredicate;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlSearchConditions
 * </h1>
 * <p>
 * Descrive le condizioni di ricerca a fronte della clausola <tt>WHERE</tt>.<br> 
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 19/lug/2011 
 * @see SqlSubselectSelectInto
 * @see SqlFullSelect
*/

public class SqlSearchConditions implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
 	private EnumSqlOperator startingOperator = null;					// |NOT predicate|(search-condition) .....
    private boolean isNotAfterStartingOperator = false;         		// AND|OR |NOT predicate|(search-conditions)
    private boolean isPredicate = false;           		 				// ... predicate 
    private boolean isSearchCondition = false;           				// ... (search-conditions)
    private SqlPredicate predicate = null;								// .. predicate 
    private SqlSearchConditions searchCondition = null;					// ... (search-conditions)
    private ArrayList<SqlSearchConditions> al_searchConditions = null;	// Eventuale elenco di condizioni ripetitive
    
    
	/**
	 * Costruttore vuoto
	 */
	public SqlSearchConditions() {
		super();
		startingOperator = EnumSqlOperator.NOT_ASSIGNED;
	    al_searchConditions = new ArrayList<SqlSearchConditions> ();
	}


	/**
	 * Restituisce l'operatore iniziale della condizione di search.<br>
	 * <p>
	 * Se si tratta dell'operatore <tt>NOT</tt> allora è l'inizio della condizione
	 * di search, non ripetitiva e non ricorsiva.<br>
	 * <p>
	 * @return the startingOperator
	 */
	public EnumSqlOperator getStartingOperator() {
		return startingOperator;
	}


	/**
	 * Imposta l'operatore iniziale della condizione di search.<br>
	 * <p>
	 * Se si tratta dell'operatore <tt>NOT</tt> allora è l'inizio della condizione
	 * di search, non ripetitiva e non ricorsiva.<br>
	 * <p>
	 * @param startingOperator the startingOperator to set
	 */
	public void setStartingOperator(EnumSqlOperator startingOperator) {
		this.startingOperator = startingOperator;
	}


	/**
	 * Restituisce se presente l'operatore <tt>NOT</tt> dopo l'operatore iniziale.<br>
	 * <p>
	 * L'operatore iniziale può essere solo <tt>AND</tt> oppure <tt>OR</tt><br>
	 * <p>
	 * @return the isNotAfterStartingOperator
	 */
	public boolean isNotAfterStartingOperator() {
		return isNotAfterStartingOperator;
	}


	/**
	 * Imposta se presente l'operatore <tt>NOT</tt> dopo l'operatore iniziale.<br>
	 * <p>
	 * L'operatore iniziale può essere solo <tt>AND</tt> oppure <tt>OR</tt><br>
	 * <p>
	 * @param isNotAfterStartingOperator the isNotAfterStartingOperator to set
	 */
	public void setNotAfterStartingOperator(boolean isNotAfterStartingOperator) {
		this.isNotAfterStartingOperator = isNotAfterStartingOperator;
	}


	/**
	 * Restituisce se il predicato è codificato dopo l'operatore.<br>
	 * <p>
	 * @return the isPredicate
	 */
	public boolean isPredicate() {
		return isPredicate;
	}


	/**
	 * Imposta se il predicato è codificato dopo l'operatore.<br>
	 * <p>
	 * @param isPredicate the isPredicate to set
	 */
	public void setPredicate(boolean isPredicate) {
		this.isPredicate = isPredicate;
	}


	/**
	 * Restituisce se la condizione di search è codificata dopo l'operatore.<br>
	 * <p>
	 * @return the isSearchCondition
	 */
	public boolean isSearchCondition() {
		return isSearchCondition;
	}


	/**
	 * Imposta se la condizione di search è codificata dopo l'operatore.<br>
	 * <p>
	 * @param isSearchCondition the isSearchCondition to set
	 */
	public void setSearchCondition(boolean isSearchCondition) {
		this.isSearchCondition = isSearchCondition;
	}

	/**
	 * Restituisce tutte le variabili host presenti in tutte le espressioni presenti 
	 * in tutti i predicati nella seach condition.<br>
	 * <p>
	 * Le variabili host vengono restituite senza i due punti iniziali.<br>
	 * <p>
	 * @return all host variables
	 */
	public ArrayList<String> getHostVars() {
		ArrayList<String> al_hostVarAll = null;
		ArrayList<String> al_hostVarSinglePredicate = null;
        ArrayList<SqlPredicate> al_sqlPredicate = null;
		
		al_hostVarAll = new ArrayList<String> ();
		al_sqlPredicate = new ArrayList<SqlPredicate> ();
		
		// Recupero ricorsivamente tutti i predicati presenti
		getHostVarsPredicatesRecursive(this, al_sqlPredicate);
		
		// Scan predicati individuati
		for (SqlPredicate sqlPredicate : al_sqlPredicate) {
			al_hostVarSinglePredicate = sqlPredicate.getHostVars();
			al_hostVarAll.addAll(al_hostVarSinglePredicate);
		}
		
		return al_hostVarAll;
	}



	/*
	 * Accoda ricorsivamente i predicati della search condition
	 */
	private void getHostVarsPredicatesRecursive(SqlSearchConditions sqlSearchConditions, ArrayList<SqlPredicate> al_sqlPredicate) {
		
		if (sqlSearchConditions.isPredicate()) {
			al_sqlPredicate.add(sqlSearchConditions.getPredicate());
		} else {
			getHostVarsPredicatesRecursive(sqlSearchConditions.getSearchCondition(), al_sqlPredicate);
		}
		
		// Scan search conditions in AND OR
		for (SqlSearchConditions searchConditions : sqlSearchConditions.getSearchConditions()) {
			getHostVarsPredicatesRecursive(searchConditions, al_sqlPredicate);
		}
	}


	/**
	 * Restituisce il primo predicato codificato dopo l'operatore.<br>
	 * <p>
	 * 
	 * @return the predicate
	 */
	public SqlPredicate getPredicate() {
		return predicate;
	}

	/**
	 * Restituisce tutti i predicati presenti nella search condition<br>
	 * <p>
	 * @return the predicate
	 */
	public ArrayList<SqlPredicate> getPredicateAll() {
		ArrayList<SqlPredicate> al_predicate = null;
		ArrayList<SqlSearchConditions> al_searchConditions = null;
		
		al_predicate = new ArrayList<SqlPredicate> ();
		
		// Primo predicato
		if (this.isPredicate) {
			al_predicate.add(this.predicate);
		}
		
		// Ulteriori condizioni di search
		al_searchConditions = getSearchConditions();
		for (SqlSearchConditions sqlSearchConditions : al_searchConditions) {
			if (!sqlSearchConditions.isPredicate) {continue;}			// Di sicurezza
			al_predicate.add(sqlSearchConditions.getPredicate());
		}
		return al_predicate;
	}

	/**
	 * Restituisce tutti i predicati presenti nella search condition<br>
	 * contenenti delle subselect<br>
	 * <p>
	 * @return the predicate
	 */
	public ArrayList<SqlPredicate> getPredicateWithSubselect() {
		ArrayList<SqlPredicate> al_predicate = null;
		ArrayList<SqlSearchConditions> al_searchConditions = null;
		
		al_predicate = new ArrayList<SqlPredicate> ();
		
		// Primo predicato
		if (this.isPredicate) {
			al_predicate.add(this.predicate);
		}
		
		// Ulteriori predicati
		al_searchConditions = getSearchConditionsElementary();
		for (SqlSearchConditions sqlSearchConditions : al_searchConditions) {
			if (sqlSearchConditions.isPredicate) {continue;}			// Di sicurezza
			al_predicate.add(sqlSearchConditions.getPredicate());
		}
		return al_predicate;
	}


	/**
	 * Imposta il predicato codificato dopo l'operatore.<br>
	 * <p>
	 * @param predicate the predicate to set
	 */
	public void setPredicate(SqlPredicate predicate) {
		this.predicate = predicate;
	}


	/**
	 * Restituisce la condizione di search codificata dopo l'operatore.<br>
	 * <p>
	 * @return the searchCondition
	 */
	public SqlSearchConditions getSearchCondition() {
		return searchCondition;
	}


	/**
	 * Imposta la condizione di search codificata dopo l'operatore.<br>
	 * <p>
	 * @param searchCondition the searchCondition to set
	 */
	public void setSearchCondition(SqlSearchConditions searchCondition) {
		this.searchCondition = searchCondition;
	}


	/**
	 * Restituisce le ulteriori condizioni di search codificate dopo il primo predicato o condizione di search.<br>
	 * <p>
	 * Possono essere presento 0, 1 o qualunque numero di ulteriori condizioni di ricerca.<br>
	 * In caso di condizione di search fra parentesi viene restituito un elemento da trattare ricorsivamente<br>
	 * <p>
	 * @return the al_searchConditions
	 */
	public ArrayList<SqlSearchConditions> getSearchConditions() {
		return al_searchConditions;
	}


	/**
	 * Restituisce le ulteriori condizioni di search codificate dopo il primo predicato o condizione di search.<br>
	 * <p>
	 * Vengono restituite solo le condizioni di search elementari, indipendentemente dal fatto<br>
	 * che tali condizioni siano fra parentesi, ovvero i successivi predicati.<br>
	 * Questo metodo evita di effettuare una ulteriore analisi ricorsiva sugli elementi restituiti da getSearchConditions()<br>
	 * <p>
	 * @return the al_searchConditions
	 */
	public ArrayList<SqlSearchConditions> getSearchConditionsElementary() {
		ArrayList<SqlSearchConditions> al_searchConditionsRecursive = null;
		
		al_searchConditionsRecursive = new ArrayList<SqlSearchConditions> ();
		getSearchConditionsElementaryRecursive(this, al_searchConditionsRecursive);
		
		return al_searchConditionsRecursive;
	}

	/*
	 * Restituisce le ulteriori condizioni di search codificate dopo il primo predicato o condizione di search.<br>
	 * <p>
	 * Vengono restituite solo le condizioni di search elementari, indipendentemente dal fatto<br>
	 * che tali condizioni siano fra parentesi.<br>
	 * Questo metodo evita di effettuare una ulteriore analisi ricorsiva sugli elementi restituiti da getSearchConditions()<br>
	 * <p>
	 */
	private void getSearchConditionsElementaryRecursive(SqlSearchConditions searchConditions, ArrayList<SqlSearchConditions> al_searchConditionsRecursive) {
		
		// (search condition)
		if (searchConditions.isSearchCondition()) {
			getSearchConditionsElementaryRecursive(searchConditions.searchCondition, al_searchConditionsRecursive);
			return;
		}

		// Predicato elementare: porto in output
		if (searchConditions.isPredicate()) {
			al_searchConditionsRecursive.add(searchConditions);
		}

        // Condizioni di search ripetitive: tratto ricorsivamente
		for (SqlSearchConditions sqlSearchConditions : searchConditions.al_searchConditions) {
			getSearchConditionsElementaryRecursive(sqlSearchConditions, al_searchConditionsRecursive);
		}
		 
		return;
	}


	/**
	 * Imposta le ulteriori condizioni di search codificate dopo il primo predicato o condizione di search.<br>
	 * <p>
	 * Possono essere presento 0, 1 o qualunque numero di ulteriori condizioni di ricerca.<br>
	 * <p>
	 * @param al_searchConditions the al_searchConditions to set
	 */
	public void setSearchConditions(ArrayList<SqlSearchConditions> al_searchConditions) {
		this.al_searchConditions = al_searchConditions;
	}


	/**
	 * Restituisce tutte le costanti alfanumeriche, literal, presenti nelle espressioni di tutti i predicati.<br>
	 * <p>
     * @return all constants alpha
	 */
	public ArrayList<String> getConstantsAlphanumeric() {
		
		ArrayList<String> al_constantAlpha = null;
		ArrayList<String> al_constantAlphaSinglePredicate = null;
        ArrayList<SqlPredicate> al_sqlPredicate = null;

        al_constantAlpha = new ArrayList<String> ();
		al_sqlPredicate = new ArrayList<SqlPredicate> ();
		
		// Recupero ricorsivamente tutti i predicati presenti
		getHostVarsPredicatesRecursive(this, al_sqlPredicate);
		
		// Scan predicati individuati
		for (SqlPredicate sqlPredicate : al_sqlPredicate) {
			al_constantAlphaSinglePredicate = sqlPredicate.getConstantsAlphanumeric();
			al_constantAlpha.addAll(al_constantAlphaSinglePredicate);
		}
		
		return al_constantAlpha;
	}

	/**
	 * Restituisce tutte le costanti numeriche, presenti nelle espressioni di tutti i predicati.<br>
	 * <p>
	 * Vengono restituite stringhe contenenti numeri validi.<br>
	 * <p>
     * @return all constants numeric
	 */
	public ArrayList<String> getConstantsNumeric() {
		
		ArrayList<String> al_constantNumeric = null;
		ArrayList<String> al_constantNumericSinglePredicate = null;
        ArrayList<SqlPredicate> al_sqlPredicate = null;

        al_constantNumeric = new ArrayList<String> ();
		al_sqlPredicate = new ArrayList<SqlPredicate> ();
		
		// Recupero ricorsivamente tutti i predicati presenti
		getHostVarsPredicatesRecursive(this, al_sqlPredicate);
		
		// Scan predicati individuati
		for (SqlPredicate sqlPredicate : al_sqlPredicate) {
			al_constantNumericSinglePredicate = sqlPredicate.getConstantsNumeric();
			al_constantNumeric.addAll(al_constantNumericSinglePredicate);
		}
		
		return al_constantNumeric;
	}


	/**
	 * Restituisce il numero massimo di subselect nested<br>
	 * <br>
	 * Si analizzano tutti i predicati codificati nella search condition.<br>
	 * Si trattano solo i predicati in cui è presente una full-select.<br>
	 * Si individua il numero di subselect nested per ogni predicato trattato.<br>
	 * Si restituisce il valore massimo raggiunto.<br>
	 * <p>
	 * @return the subselect nesting level max
	 */
	public  int getMaxSubselectNestedNumber() {
		
        ArrayList<SqlPredicate> al_predicate = null;
		Integer maxNested = new Integer(0);
		Integer maxNestedWrk = null;
        Integer[] ar_maxNested = null;
        Integer[] ar_maxNestedWrk = null;
		
		al_predicate = this.getPredicateAll();
		
		// Scan predicati di primo livello
		for (SqlPredicate sqlPredicate : al_predicate) {
			ar_maxNested = new Integer[1];
			ar_maxNestedWrk = new Integer[1];
			maxNestedWrk = new Integer(0);
			ar_maxNested[0] = maxNestedWrk;
			ar_maxNestedWrk[0] = new Integer(0);
			getSubselectCountRecursive(sqlPredicate, ar_maxNested, ar_maxNestedWrk);
			if (ar_maxNested[0].intValue() > maxNested.intValue()) {
				maxNested = new Integer(ar_maxNested[0].intValue());
			}
		}
		return maxNested;
	}

	
	/* ---------------------------------------------------------------------------------
	 * Analisi ricorsiva predicati e aggiornamento livello massimo di nesting raggiunto
	 * ---------------------------------------------------------------------------------
	 * 
	 */
	private void getSubselectCountRecursive(SqlPredicate sqlPredicate, Integer[] ar_maxNested, Integer[] ar_maxNestedWrk) {
		
		Integer maxNestedStart = null;
		ArrayList<SqlFullSelect> al_sqlFullSelect = null;
		ArrayList<SqlSubselectSelectInto> al_subselectRecursive = null;
		ArrayList<SqlPredicate> al_predicateRecursive = null;
		SqlFullSelect sqlFullSelect = null;
		SqlSearchConditions searchConditionsRecursive = null;
		
		al_sqlFullSelect = new ArrayList<SqlFullSelect> ();
		maxNestedStart = ar_maxNested[0];
		
		// Estrazione full-select implicate nel predicato
		
		// Predicato Quantified
		if (sqlPredicate.getTypePredicate() == EnumSqlPredicate.QUANTIFIED) {
			sqlFullSelect = sqlPredicate.getQuantifiedFullSelect();
			al_sqlFullSelect.add(sqlFullSelect);
			
		// Predicato Exists	
		} else if (sqlPredicate.getTypePredicate() == EnumSqlPredicate.EXISTS) {
			sqlFullSelect = sqlPredicate.getExistsFullSelect();
			al_sqlFullSelect.add(sqlFullSelect);
			
	    // Predicato In
		} else if (sqlPredicate.getTypePredicate() == EnumSqlPredicate.IN) {
			sqlFullSelect = sqlPredicate.getInFullSelect();
			al_sqlFullSelect.add(sqlFullSelect);
			
	    // Predicato Basic
		}  else if (sqlPredicate.getTypePredicate() == EnumSqlPredicate.BASIC) {
			if (!sqlPredicate.isBasicRowValueExpressions()) {
				for (SqlExpressionElement exprElement : sqlPredicate.getBasicExpressionLeft().getElements()) {
					if (exprElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
						al_sqlFullSelect.add(exprElement.getFullSelect());
					}
				}
				for (SqlExpressionElement exprElement : sqlPredicate.getBasicExpressionRight().getElements()) {
					if (exprElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
						al_sqlFullSelect.add(exprElement.getFullSelect());
					}
				}
			}

		// Predicato Between
		} else if (sqlPredicate.getTypePredicate() == EnumSqlPredicate.BETWEEN) {
			for (SqlExpressionElement exprElement : sqlPredicate.getBetweenExpression().getElements()) {
				if (exprElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
					al_sqlFullSelect.add(exprElement.getFullSelect());
				}
			}
			for (SqlExpressionElement exprElement : sqlPredicate.getBetweenExpressionLower().getElements()) {
				if (exprElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
					al_sqlFullSelect.add(exprElement.getFullSelect());
				}
			}
			for (SqlExpressionElement exprElement : sqlPredicate.getBetweenExpressionHigher().getElements()) {
				if (exprElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
					al_sqlFullSelect.add(exprElement.getFullSelect());
				}
			}
			
		// Predicato Distinct
		} else if (sqlPredicate.getTypePredicate() == EnumSqlPredicate.DISTINCT) {
			if (!sqlPredicate.isDistinctRowValueExpression()) {
				for (SqlExpressionElement exprElement : sqlPredicate.getDistinctExpressionLeft().getElements()) {
					if (exprElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
						al_sqlFullSelect.add(exprElement.getFullSelect());
					}
				}
				for (SqlExpressionElement exprElement : sqlPredicate.getDistinctExpressionRight().getElements()) {
					if (exprElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
						al_sqlFullSelect.add(exprElement.getFullSelect());
					}
				}
			}
		}
		
		// Scan fullselect allo stesso livello da esaminare ricorsivamente
		for (SqlFullSelect sqlFullSelectRecursive : al_sqlFullSelect) {
			al_subselectRecursive = sqlFullSelectRecursive.getSubselectIntoAll();
			
			// Scan subselect di livello successivo al primo
			for (SqlSubselectSelectInto sqlSubselectRecursive : al_subselectRecursive) {
			   
				if (!sqlSubselectRecursive.isWhere()) {continue;}			// Interessano solo le subselect con where
				searchConditionsRecursive = sqlSubselectRecursive.getWhere(); 
				al_predicateRecursive = searchConditionsRecursive.getPredicateAll();
				
				// Attivazione ricorsiva sui predicati
				for (SqlPredicate sqlPredicateRecursive : al_predicateRecursive) {
					ar_maxNestedWrk[0] = new Integer(maxNestedStart.intValue() + 1);
					getSubselectCountRecursive(sqlPredicateRecursive, ar_maxNested, ar_maxNestedWrk);
					// Update valore massimo nesting level
					if (ar_maxNestedWrk[0].intValue() > ar_maxNested[0].intValue()) {
						ar_maxNested[0] = ar_maxNestedWrk[0];
					}
				}
			}
		}
	}

	
}
