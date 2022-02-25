
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumSqlExpressionElementType;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlExpression
 * </h1>
 * <p>
 * Descrive una generica expression Sql, che specifica un valore che può prendere diverse forme.<br> 
 * <p>
 * Viene memorizzato anche il nome di una colonna, per espressione in clausola <br>
 * <tt>SELECT</tt> con opzione <tt>AS new-col-name</tt><br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlExpressionElement
 * @see SqlSubselectSelectInto
*/

public class SqlExpression implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
    private ArrayList<SqlExpressionElement> al_element = null;   			// Elementi espressione
    private String asNewColumnName = "";                                    // Nuovo nome di colonna associato all'espressione in SELECT clause
    
    // Tipi di elementi presenti nell'espressione
    private boolean isThereAnyAdd = false;                     				//  
    private boolean isThereAnySubtract = false;                     			//  
    private boolean isThereAnyMultiply = false;                     			//  
    private boolean isThereAnyDivide = false;                     			    //  
    private boolean isThereAnyDefault = false;                     			    //  
    private boolean isThereAnyNull = false;                     			    //  
    private boolean isThereAnyConcat = false;                     			    //  
    private boolean isThereAnyFunctionInvocation = false;                     	//  
    private boolean isThereAnyExpression = false;                     			//  
    private boolean isThereAnyConstant = false;                     			//  
    private boolean isThereAnyColumnName = false;                     			//  
    private boolean isThereAnyVariabile = false;                    			//  
    private boolean isThereAnyHostVar = false;                    			    //  
    private boolean isThereAnySpecialRegister = false;                     	    //  
    private boolean isThereAnyScalarFullSelect = false;                     	//  
    private boolean isThereAnyTimezoneExpression = false;                     	//  
    private boolean isThereAnyLabeledDuration = false;                     	    //  
    private boolean isThereAnyCaseExpression = false;                     		//  
    private boolean isThereAnyCastSpecification = false;                     	//  
    private boolean isThereAnyXMLCastSpecification = false;                   	//  
    private boolean isThereAnyOlapSpecification = false;                     	//  
    private boolean isThereAnyRowChangeExpression = false;                      //  
    private boolean isThereAnySequenceReference = false;                     	//  
       
    
	/**
	 * Costruttore vuoto
	 */
	public SqlExpression() {
		super();
		al_element = new ArrayList<SqlExpressionElement> ();
	}



	/**
	 * Restituisce gli elementi dell'espressione.<bt>
	 * <p>
	 * @return the al_element
	 */
	public ArrayList<SqlExpressionElement> getElements() {
		return al_element;
	}

	
	
	
	/**
	 * Restituisce il nome di colonna codificato dopo l'epressione,
	 * nell'elenco di colonne dello statement Select.<br>
	 * <p>
	 * Il new name della colonna è stabilmente memorizzato nella struttura
	 * preposta a fronte dello statement Select, in oggetti {@link SqlColumnInSelect}.<br>
	 * <p>
	 * Questa e' una informazione di servizio utilizzata al momento del<br>
	 * parsing delle colonne in Select, ognuna rappresentata da una espressione. <br>
	 * Ciò avviene solo quando il nuovo nome della colonna non è preceduto da AS <br>
	 * ma è codificato direttamente dopo l'espressione.<br>
	 * <p>
	 * @return the asNewColumnName
	 */
	public String getAsNewColumnName() {
		return asNewColumnName;
	}



	/**
	 * Imposta il nome di colonna codificato dopo l'epressione,
	 * nell'elenco di colonne dello statement Select.<br>
	 * <p>
	 * Il new name della colonna è stabilmente memorizzato nella struttura
	 * preposta a fronte dello statement Select, in oggetti {@link SqlColumnInSelect}.<br>
	 * <p>
	 * Questa e' una informazione di servizio utilizzata al momento del<br>
	 * parsing delle colonne in Select, ognuna rappresentata da una espressione. <br>
	 * Ciò avviene solo quando il nuovo nome della colonna non è preceduto da AS <br>
	 * ma è codificato direttamente dopo l'espressione.<br>
	 * <p>
	 * @param asNewColumnName the asNewColumnName to set
	 */
	public void setAsNewColumnName(String asNewColumnName) {
		this.asNewColumnName = asNewColumnName;
	}



	/**
	 * Restituisce tutte le variabili host presenti nell'espressione.<br>
	 * <p>
	 * Le variabili host vengono restituite senza i due punti iniziali e senza l'eventuale qualificazione.<br>
	 * La qualificazione è data dal compo di gruppo sotto il quale la variabile è definita.<br>
	 * <p>
	 * In presenza di qualificazione la codifica è la seguente:<br>
	 * <p>
	 * <tt>:grpHostVar.hostVar</tt>
	 * <p>
	 * In assenza di qualificazione la codifica è la seguente:<br>
	 * <p>
	 * <tt>:hostVar</tt>
	 * <p>
	 * <p>
	 * Le variabili host vengono restituite senza i due punti iniziali.<br>
	 * <p>
     * @return all host variables
	 */
	public ArrayList<String> getHostVars() {
		
		ArrayList<String> al_hostVar = null;
		al_hostVar = new ArrayList<String> ();
		String hostVar = "";
		int i = 0;
		
		// Scan elementi espressione
		for (SqlExpressionElement expressionElement : this.getElements()) {
			
			// Interessano solo le variabili host
			if (expressionElement.getTypeElement() != EnumSqlExpressionElementType.HOST_VAR) {
				continue;
			}
			hostVar = expressionElement.getHostVar().substring(1);
			i = hostVar.indexOf(".");
			if (i > 0) {
				hostVar = hostVar.substring(i + 1);
			}
			al_hostVar.add(hostVar);
		}
		
		return al_hostVar;
	}

	/**
	 * Restituisce tutte le variabili host presenti nell'espressione.<br>
	 * <p>
	 * Le variabili host vengono restituite senza i due punti iniziali con l'eventuale qualificazione.<br>
	 * La qualificazione è data dal compo di gruppo sotto il quale la variabile è definita.<br>
	 * <p>
	 * In presenza di qualificazione la codifica è la seguente:<br>
	 * <p>
	 * <tt>:grpHostVar.hostVar</tt>
	 * 
	 * Può essere presente una variabile host per indicare il null come:
	 * <p>
	 * <tt>:grpHostVar.hostVar INDICATOR >:grpHostVar.hostVar</tt><br>
	 * <tt>:hostVar</tt> INDICATOR >:grpHostVar.hostVar</tt><br>
	 * <p>
	 * Vengono restituite anche le variabili host di indicator.<br>
	 * <p>
	 * <p>
     * @return all host variables
	 */
	public ArrayList<String> getHostVarsQualified() {
		
		ArrayList<String> al_hostVar = null;
		al_hostVar = new ArrayList<String> ();
		
		// Scan elementi espressione
		for (SqlExpressionElement expressionElement : this.getElements()) {
			
			// Interessano solo le variabili host
			if (expressionElement.getTypeElement() != EnumSqlExpressionElementType.HOST_VAR) {
				continue;
			}
			
			al_hostVar.add(expressionElement.getHostVar().substring(1));
		}
		
		return al_hostVar;
	}


	/**
	 * Restituisce tutte le costanti alfanumeriche, literal, presenti nell'espressione.<br>
	 * <p>
     * @return all constants alpha
	 */
	public ArrayList<String> getConstantsAlphanumeric() {
		
		ArrayList<String> al_constantAlpha = null;
		al_constantAlpha = new ArrayList<String> ();
		
		// Scan elementi espressione
		for (SqlExpressionElement expressionElement : this.getElements()) {
			
			// Interessano le costanti definite nele scalar-fullselect
			if (expressionElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
				al_constantAlpha.addAll(expressionElement.getFullSelect().getConstantsAlphanumeric());
				continue;
			}
			
			// Interessano le costanti alfanumeriche
			if (expressionElement.getTypeElement() == EnumSqlExpressionElementType.CONSTANT_ALPHANUMERIC) {
				al_constantAlpha.add(expressionElement.getConstant());
				continue;
			}
			
		}
		
		return al_constantAlpha;
	}

	/**
	 * Restituisce tutte le costanti numeriche, presenti nell'espressione.<br>
	 * <p>
	 * Vengono restituite stringhe contenenti numeri validi.<br>
	 * <p>
     * @return all constants numeric
	 */
	public ArrayList<String> getConstantsNumeric() {
		
		ArrayList<String> al_constantNumeric = null;
		al_constantNumeric = new ArrayList<String> ();
		
		// Scan elementi espressione
		for (SqlExpressionElement expressionElement : this.getElements()) {
			
			// Interessano le costanti definite nele scalar-fullselect
			if (expressionElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
				al_constantNumeric.addAll(expressionElement.getFullSelect().getConstantsNumeric());
				continue;
			}
			
			// Interessano le costanti numeriche
			if (expressionElement.getTypeElement() == EnumSqlExpressionElementType.CONSTANT_NUMERIC) {
				al_constantNumeric.add(expressionElement.getConstant());
				continue;
			}
		}
		
		return al_constantNumeric;
	}


	
	
	/**
	 * Imposta gli elementi dell'espressione.<bt>
	 * <p>
	 * @param alElement the al_element to set
	 */
	public void setElements(ArrayList<SqlExpressionElement> al_element) {
		this.al_element = al_element;
	}


	/**
	 * Restituisce se presente un operatore aritmetico di somma nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyAdd
	 */
	public boolean isThereAnyAdd() {
		return isThereAnyAdd;
	}


	/**
	 * Imposta se presente un operatore aritmetico di somma nell'espressione.<br>
	 * <p>
	 * @param isThereAnyAdd the isThereAnyAdd to set
	 */
	public void setThereAnyAdd(boolean isThereAnyAdd) {
		this.isThereAnyAdd = isThereAnyAdd;
	}


	/**
	 * Restituisce se presente un operatore aritmetico di sottrazione nell'espressione.<br>
	 * <p>
	 * @return the isThereAnySubtract
	 */
	public boolean isThereAnySubtract() {
		return isThereAnySubtract;
	}


	/**
	 * Imposta se presente un operatore aritmetico di sottrazione nell'espressione.<br>
	 * <p>
	 * @param isThereAnySubtract the isThereAnySubtract to set
	 */
	public void setThereAnySubtract(boolean isThereAnySubtract) {
		this.isThereAnySubtract = isThereAnySubtract;
	}


	/**
	 * Restituisce se presente un operatore aritmetico di moltiplicazione nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyMultiply
	 */
	public boolean isThereAnyMultiply() {
		return isThereAnyMultiply;
	}


	/**
	 * Imposta se presente un operatore aritmetico di moltiplicazione nell'espressione.<br>
	 * <p>isThereAnyMultiply
	 * @param isThereAnyDivide the isThereAnyDivide to set
	 */
	public void setThereAnyDivide(boolean isThereAnyDivide) {
		this.isThereAnyDivide = isThereAnyDivide;
	}

	/**
	 * Restituisce se presente un operatore aritmetico di divisione nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyDivide
	 */
	public boolean isThereAnyDivide() {
		return isThereAnyDivide;
	}


	/**
	 * Restituisce se presente un operatore DEFAULT nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyDefault
	 */
	public boolean isThereAnyDefault() {
		return isThereAnyDefault;
	}



	/**
	 * Imposta se presente un operatore DEFAULT nell'espressione.<br>
	 * <p>
	 * @param isThereAnyDefault the isThereAnyDefault to set
	 */
	public void setThereAnyDefault(boolean isThereAnyDefault) {
		this.isThereAnyDefault = isThereAnyDefault;
	}



	/**
	 * Restituisce se presente un operatore NULL nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyNull
	 */
	public boolean isThereAnyNull() {
		return isThereAnyNull;
	}



	/**
	 * Imposta se presente un operatore NULL nell'espressione.<br>
	 * <p>
	 * @param isThereAnyNull the isThereAnyNull to set
	 */
	public void setThereAnyNull(boolean isThereAnyNull) {
		this.isThereAnyNull = isThereAnyNull;
	}



	/**
	 * Imposta se presente un operatore aritmetico di concatenazione nell'espressione.<br>
	 * <p>isThereAnyMultiply
	 * @param isThereAnyConcat the isThereAnyConcat to set
	 */
	public void setThereAnyConcat(boolean isThereAnyConcat) {
		this.isThereAnyConcat = isThereAnyConcat;
	}

	/**
	 * Restituisce se presente un operatore aritmetico di concatenazione nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyConcat
	 */
	public boolean isThereAnyConcat() {
		return isThereAnyConcat;
	}


	/**
	 * Imposta se presente un operatore aritmetico di divisione nell'espressione.<br>
	 * <p>isThereAnyMultiply
	 * @param isThereAnySubtract the isThereAnyMultiply to set
	 */
	public void setThereAnyMultiply(boolean isThereAnyMultiply) {
		this.isThereAnyMultiply = isThereAnyMultiply;
	}



	/**
	 * Restituisce se presente l'invocaziopne di una function nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyFunctionInvocation
	 */
	public boolean isThereAnyFunctionInvocation() {
		return isThereAnyFunctionInvocation;
	}


	/**
	 * Imposta se presente l'invocaziopne di una function nell'espressione.<br>
	 * <p>
	 * @param isThereAnyFunctionInvocation the isThereAnyFunctionInvocation to set
	 */
	public void setThereAnyFunctionInvocation(boolean isThereAnyFunctionInvocation) {
		this.isThereAnyFunctionInvocation = isThereAnyFunctionInvocation;
	}


	/**
	 * Restituisce se presente l'invocaziopne ricorsiva di un'altra espressione, nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyExpression
	 */
	public boolean isThereAnyExpression() {
		return isThereAnyExpression;
	}


	/**
	 * Imposta se presente l'invocaziopne ricorsiva di un'altra espressione, nell'espressione.<br>
	 * <p>
	 * @param isThereAnyExpression the isThereAnyExpression to set
	 */
	public void setThereAnyExpression(boolean isThereAnyExpression) {
		this.isThereAnyExpression = isThereAnyExpression;
	}


	/**
	 * Restituisce se presente qualche costante nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyConstant
	 */
	public boolean isThereAnyConstant() {
		return isThereAnyConstant;
	}


	/**
	 * Imposta se presente qualche costante nell'espressione.<br>
	 * <p>
	 * @param isThereAnyConstant the isThereAnyConstant to set
	 */
	public void setThereAnyConstant(boolean isThereAnyConstant) {
		this.isThereAnyConstant = isThereAnyConstant;
	}


	/**
	 * Restituisce se presente qualche nome di colonna nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyColumnName
	 */
	public boolean isThereAnyColumnName() {
		return isThereAnyColumnName;
	}


	/**
	 * Imposta se presente qualche nome di colonna nell'espressione.<br>
	 * <p>
	 * @param isThereAnyColumnName the isThereAnyColumnName to set
	 */
	public void setThereAnyColumnName(boolean isThereAnyColumnName) {
		this.isThereAnyColumnName = isThereAnyColumnName;
	}


	/**
	 * Restituisce se presente qualche variabile nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyVariabile
	 */
	public boolean isThereAnyVariabile() {
		return isThereAnyVariabile;
	}


	/**
	 * Imposta se presente qualche variabile nell'espressione.<br>
	 * <p>
	 * @param isThereAnyVariabile the isThereAnyVariabile to set
	 */
	public void setThereAnyVariabile(boolean isThereAnyVariabile) {
		this.isThereAnyVariabile = isThereAnyVariabile;
	}

	/**
	 * Restituisce se presente qualche variabile host, che inizia con :, nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyVariabile
	 */
	public boolean isThereAnyHostVar() {
		return isThereAnyHostVar;
	}


	/**
	 * Imposta se presente qualche variabile host, che inizia con :, nell'espressione.<br>
	 * <p>
	 * @param isThereAnyVariabile the isThereAnyVariabile to set
	 */
	public void setThereAnyHostVar(boolean isThereAnyHostVar) {
		this.isThereAnyHostVar = isThereAnyHostVar;
	}


	/**
	 * Restituisce se presente registro speciale nell'espressione.<br>
	 * <p>
	 * @return the isThereAnySpecialRegister
	 */
	public boolean isThereAnySpecialRegister() {
		return isThereAnySpecialRegister;
	}


	/**
	 * Imposta se presente registro speciale nell'espressione.<br>
	 * <p>
	 * @param isThereAnySpecialRegister the isThereAnySpecialRegister to set
	 */
	public void setThereAnySpecialRegister(boolean isThereAnySpecialRegister) {
		this.isThereAnySpecialRegister = isThereAnySpecialRegister;
	}


	/**
	 * Restituisce se presente l'invocazione di una scalar-full-select nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyScalarFullSelect
	 */
	public boolean isThereAnyScalarFullSelect() {
		return isThereAnyScalarFullSelect;
	}


	/**
	 * Imposta se presente l'invocazione di una scalar-full-select nell'espressione.<br>
	 * <p>
	 * @param isThereAnyScalarFullSelect the isThereAnyScalarFullSelect to set
	 */
	public void setThereAnyScalarFullSelect(boolean isThereAnyScalarFullSelect) {
		this.isThereAnyScalarFullSelect = isThereAnyScalarFullSelect;
	}




	/**
	 * Restituisce se presente l'espressione di una timezone nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyTimezoneExpression
	 */
	public boolean isThereAnyTimezoneExpression() {
		return isThereAnyTimezoneExpression;
	}


	/**
	 * Imposta se presente l'espressione di una timezone nell'espressione.<br>
	 * <p>
	 * @param isThereAnyTimezoneExpression the isThereAnyTimezoneExpression to set
	 */
	public void setThereAnyTimezoneExpression(boolean isThereAnyTimezoneExpression) {
		this.isThereAnyTimezoneExpression = isThereAnyTimezoneExpression;
	}


	/**
	 * Restituisce se presente una duration label nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyLabeledDuration
	 */
	public boolean isThereAnyLabeledDuration() {
		return isThereAnyLabeledDuration;
	}


	/**
	 * Imposta se presente una duration label nell'espressione.<br>
	 * <p>
	 * @param isThereAnyLabeledDuration the isThereAnyLabeledDuration to set
	 */
	public void setThereAnyLabeledDuration(boolean isThereAnyLabeledDuration) {
		this.isThereAnyLabeledDuration = isThereAnyLabeledDuration;
	}


	/**
	 * Restituisce se presente una CASE expression nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyCaseExpression
	 */
	public boolean isThereAnyCaseExpression() {
		return isThereAnyCaseExpression;
	}


	/**
	 * Imposta se presente una CASE expression nell'espressione.<br>
	 * <p>
	 * @param isThereAnyCaseExpression the isThereAnyCaseExpression to set
	 */
	public void setThereAnyCaseExpression(boolean isThereAnyCaseExpression) {
		this.isThereAnyCaseExpression = isThereAnyCaseExpression;
	}


	/**
	 * Restituisce se presente una cast specification nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyCastSpecification
	 */
	public boolean isThereAnyCastSpecification() {
		return isThereAnyCastSpecification;
	}


	/**
	 * Imposta se presente una cast specification nell'espressione.<br>
	 * <p>
	 * @param isThereAnyCastSpecification the isThereAnyCastSpecification to set
	 */
	public void setThereAnyCastSpecification(boolean isThereAnyCastSpecification) {
		this.isThereAnyCastSpecification = isThereAnyCastSpecification;
	}


	/**
	 * Restituisce se presente una XML cast specification nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyXMLCastSpecification
	 */
	public boolean isThereAnyXMLCastSpecification() {
		return isThereAnyXMLCastSpecification;
	}


	/**
	 * Imposta se presente una XML cast specification nell'espressione.<br>
	 * <p>
	 * @param isThereAnyXMLCastSpecification the isThereAnyXMLCastSpecification to set
	 */
	public void setThereAnyXMLCastSpecification(boolean isThereAnyXMLCastSpecification) {
		this.isThereAnyXMLCastSpecification = isThereAnyXMLCastSpecification;
	}


	/**
	 * Restituisce se presente una OLAP specification nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyOlapSpecification
	 */
	public boolean isThereAnyOlapSpecification() {
		return isThereAnyOlapSpecification;
	}


	/**
	 * Imposta se presente una OLAP specification nell'espressione.<br>
	 * <p>
	 * @param isThereAnyOlapSpecification the isThereAnyOlapSpecification to set
	 */
	public void setThereAnyOlapSpecification(boolean isThereAnyOlapSpecification) {
		this.isThereAnyOlapSpecification = isThereAnyOlapSpecification;
	}


	/**
	 * Restituisce se presente una row change specification nell'espressione.<br>
	 * <p>
	 * @return the isThereAnyRowChangeExpression
	 */
	public boolean isThereAnyRowChangeExpression() {
		return isThereAnyRowChangeExpression;
	}


	/**
	 * Imposta se presente una row change specification nell'espressione.<br>
	 * <p>
	 * @param isThereAnyRowChangeExpression the isThereAnyRowChangeExpression to set
	 */
	public void setThereAnyRowChangeExpression(boolean isThereAnyRowChangeExpression) {
		this.isThereAnyRowChangeExpression = isThereAnyRowChangeExpression;
	}


	/**
	 * Restituisce se presente un sequence reference nell'espressione.<br>
	 * <p>
	 * @return the isThereAnySequenceReference
	 */
	public boolean isThereAnySequenceReference() {
		return isThereAnySequenceReference;
	}


	/**
	 * Imposta se presente un sequence reference nell'espressione.<br>
	 * <p>
	 * @param isThereAnySequenceReference the isThereAnySequenceReference to set
	 */
	public void setThereAnySequenceReference(boolean isThereAnySequenceReference) {
		this.isThereAnySequenceReference = isThereAnySequenceReference;
	}



	
}
