
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumSqlExpressionElementType;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlColumnInSelect
 * </h1>
 * <p>
 * Definisce le informazioni di una specifica colonna dichiarata in una <tt>SELECT</tt>.<br> 
 * <p>
 * Vengono memorizzato informazioni aggiuntive quali nuovo nome di una colonna, per espressione in clausola AS<br>
 * <p>
 * <tt>SELECT</tt> column <tt>AS new-col-name</tt><br>
 * <p>
 * Il nome della tabella o della view e il correlation name per gestire casi del tipo:<br>
 * <p>
 * <tt>SELECT table|view.*, col2, corrName.* FROM table1, table2 AS corrName </tt><br>
 * <p>
 * La codifica della colonna come expression sql per gestire casi del tipo:<br>
 * <p>
 * <tt>SELECT expression AS new-col-name FROM table1, table2 AS corrName </tt><br>
 * <p>
 * La codifica di tutte le colonne di tutte le tabelle dichiarate in FROM  per gestire casi del tipo:<br>
 * <p>
 * <tt>SELECT * FROM table1, table2 AS corrName </tt><br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/lug/2011 
  * @see SqlSubselectSelectInto
*/

public class SqlColumnInSelect implements Serializable {

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	private String tableViewName = "";           					 // SELECT table|view.*
	private String tableViewOwner = "";          					 // SELECT owner.table|owner.view.*
	private String columnQualifier = "";         					 // SELECT correlationName.*, tb.col1, view.col2, alias.col3, synonym.col4, 
 	private SqlExpression expression = null;    					 // SELECT expression |AS newColName, ... 
 																	 //        expression può essere, nella forma più semplice, colName
    private String asNewColumnName = "";         					 // SELECT expression AS newColName
    private ArrayList<ArrayList<String>> al_al_columnImplicit = null;// Elenco colonne definite per table/view (da database)
    																 //  Ogni elemento contiene tableName, owner, correlationName e column name
                                                                     //  Per gestire * e table.*
    // Opzioni
    private boolean isByStar = false;        	 					 // True indica SELECT * ovvero tutte le colonne di tutte le tabella in FROM
	private boolean isByExpression = false;      					 // True indica che la colonna è una espressione, al limite di una colonna
	private boolean isByTableView = false;       					 // True indica SELECT table|view .* ovvero tutte le colonne della tabella
	private boolean isByCorrelationName = false; 					 // True indica SELECT correlationName.* ovvero tutte le colonne della tabella correlata 
 
	
	/**
	 * Costruttore
	 */
	public SqlColumnInSelect() {
		super();
		al_al_columnImplicit = new ArrayList<ArrayList<String>> ();
	}




	/**
	 * Restituisce il nuovo nome della colonna associata all'espressione.<br>
	 * <p>
	 * Si tratta del nome colonna associato all'espressione e specificato in clausole del tipo<br>
	 * <tt>SELECT expression AS new-col-name ...</tt><br>
	 * <p>
	 * @return the asNewColumnName
	 */
	public String getAsNewColumnName() {
		return asNewColumnName;
	}



	/**
	 * Imposta il nuovo nome della colonna associata all'espressione.<br>
	 * <p>
	 * Si tratta del nome colonna associato all'espressione e specificato in clausole del tipo<br>
	 * <tt>SELECT expression AS new-col-name ...</tt><br>
	 * <p>
	 * @param asNewColumnName the asNewColumnName to set
	 */
	public void setAsNewColumnName(String asNewColumnName) {
		this.asNewColumnName = asNewColumnName;
	}

	
	/**
	 * Restituisce le colonne implicite definite per tutte le tabella o view,<br>
	 * caricate su database con <tt>CREATE TABLE</tt> a fronte di statement tipo:<br>
	 * <p>
	 * <ul>
	 * 	<li>SELECT * </li>
	 * 	<li>SELECT table1-name.*, table2-name.*, ...</li>	 
	 * 	<li>SELECT view-name.*</li>	 
	 * 	<li>SELECT corr-name.*</li>	 
	 * </ul>
	 * <p>
	 * Viene restituito un ArrayList di colonne.<br>
	 * Ogni elemento, indipendentemente dalla notazione implicita adottata,<br>
	 * è a sua volta un ArrayList di stringhe di 4 elementi che sono, nell'ordine:
	 * <ul>
	 *  <li> <tt>column-name</tt></li>
	 *  <li> <tt>table-name</tt></li>
	 *  <li> <tt>owner</tt></li>
	 *  <li> <tt>correlation-name</tt></li>
	 * </ul>
	 * <p>
	 * L'owner e il correlation name possono essere valorizzati come stringhe vuote,<br>
	 * se non sono codificati nell'istruzione.
	 * <p>
	 * @return the al_al_columnImplicit
	 */
	public ArrayList<ArrayList<String>> getColumnsImplicit() {
		return al_al_columnImplicit;
	}

	/**
	 * Restituisce le colonne implicite definite per una specifica tabella o view,<br>
	 * caricate su database con <tt>CREATE TABLE</tt> a fronte di statement tipo:<br>
	 * <p>
	 * <ul>
	 * 	<li>SELECT * FROM table</li>
	 * 	<li>SELECT table1-name.*, table2-name.*, ...</li>	 
	 * 	<li>SELECT view-name.*</li>	 
	 * 	<li>SELECT corr-name.*</li>	 
	 * </ul>
	 * <p>
	 * Viene restituito un ArrayList di colonne.<br>
	 * Ogni elemento, indipendentemente dalla notazione implicita adottata,<br>
	 * è a sua volta un ArrayList di stringhe di 4 elementi che sono, nell'ordine:
	 * <ul>
	 *  <li> <tt>column-name</tt></li>
	 *  <li> <tt>table-name</tt></li>
	 *  <li> <tt>owner</tt></li>
	 *  <li> <tt>correlation-name</tt></li>
	 * </ul>
	 * <p>
	 * L'owner e il correlation name possono essere valorizzati come stringhe vuote,<br>
	 * se non sono codificati nell'istruzione.
	 * <p>
	 * @param String tableName
	 * @return the al_al_columnImplicit
	 */
	public ArrayList<ArrayList<String>> getColumnsImplicit(String tableName) {
		ArrayList<ArrayList<String>> al_al_columnImplicitOutput = null;
		
		al_al_columnImplicitOutput = new ArrayList<ArrayList<String>> ();
		for (ArrayList<String> al_columnImplicit : this.al_al_columnImplicit) {
			
			// Non è la tabella che interessa
			if (!al_columnImplicit.get(1).equals(tableName)) {
				continue;
			}
			
			al_al_columnImplicitOutput.add(al_columnImplicit);
		}
		
		return al_al_columnImplicitOutput;
	}

	/**
	 * Imposta le colonne implicite definite per tutte le tabella o view,<br>
	 * caricate su database con <tt>CREATE TABLE</tt> a fronte di statement tipo:<br>
	 * <p>
	 * <ul>
	 * 	<li>SELECT * </li>
	 * 	<li>SELECT table1-name.*, table2-name.*, ...</li>	 
	 * 	<li>SELECT view-name.*</li>	 
	 * 	<li>SELECT corr-name.*</li>	 
	 * </ul>
	 * <p>
	 * Viene restituito un ArrayList di colonne.<br>
	 * Ogni elemento, indipendentemente dalla notazione implicita adottata,<br>
	 * è a sua volta un ArrayList di stringhe di 4 elementi che sono, nell'ordine:
	 * <ul>
	 *  <li> <tt>column-name</tt></li>
	 *  <li> <tt>table-name</tt></li>
	 *  <li> <tt>owner</tt></li>
	 *  <li> <tt>correlation-name</tt></li>
	 * </ul>
	 * <p>
	 * L'owner e il correlation name possono essere valorizzati come stringhe vuote,<br>
	 * se non sono codificati nell'istruzione.
	 * <p>
	 * @param the al_al_columnImplicit to set
	 */
	public void setColumnsImplicit(ArrayList<ArrayList<String>> al_al_columnImplicit) {
		this.al_al_columnImplicit = al_al_columnImplicit;
		return;
	}


	/**
	 * Imposta le colonne implicite definite per la tabella o view,<br>
	 * caricate su database a fronte di <tt>CREATE TABLE</tt> a fronte di:<br>
	 * <ul>
	 * 	<li>SELECT * </li>
	 * 	<li>SELECT table-name.*</li>	 
	 * 	<li>SELECT view-name.*</li>	 
	 * 	<li>SELECT corr-name.*</li>	 
	 * </ul>
	 * <p>
	 * Ogni colonna, indipendentemente dalla notazione implicita adottata<br>
	 * viene restituita, in ogni elemento dell'array, come <tt>tableName.columnName</tt>
	 * <p>
	 * 
	 * @param al_al_columnImplicit the al_al_columnImplicit to set
	 */
	public void setColumnsDefined(ArrayList<ArrayList<String>> al_al_columnImplicit) {
		this.al_al_columnImplicit = al_al_columnImplicit;
	}



	/**
	 * Restituisce il nome della tabella o della view di cui
	 * sono richieste tutte le colonne, a fronte di <tt>SELECT table|view.*</tt><br>
	 * <p>
	 * @return the tableViewName
	 */
	public String getSelectTableViewName() {
		return tableViewName;
	}





	/**
	 * Imposta il nome della tabella o della view di cui
	 * sono richieste tutte le colonne, a fronte di <tt>SELECT table|view.*</tt><br>
	 * <p>
	 * @param tableViewName the tableViewName to set
	 */
	public void setSelectTableViewName(String tableViewName) {
		this.tableViewName = tableViewName;
	}





	/**
	 * Restituisce l'owner della tabella o della view di cui
	 * sono richieste tutte le colonne, a fronte di <tt>SELECT owner.table|owner.view.*</tt><br>
	 * <p>
	 * @return the tableViewOwner
	 */
	public String getTableViewOwner() {
		return tableViewOwner;
	}




	/**
	 * Imposta l'owner della tabella o della view di cui
	 * sono richieste tutte le colonne, a fronte di <tt>SELECT owner.table|owner.view.*</tt><br>
	 * <p>
	 * @param tableViewOwner the tableViewOwner to set
	 */
	public void setTableViewOwner(String tableViewOwner) {
		this.tableViewOwner = tableViewOwner;
	}




	/**
	 * Restituisce il qualifier della colonna.<br>
	 * <p>
	 * Si tratta di istruzioni del tipo:<br>
	 * <p>
	 * <tt>SELECT corrName.* FROM table AS corrName</tt><br>
	 * <tt>SELECT corrName.colName FROM table AS corrName</tt><br>
	 * <tt>SELECT table.colName FROM table</tt><br>
	 * <tt>SELECT view.colName  FROM view</tt><br>
	 * <tt>SELECT alias.colName  FROM alias</tt><br>
	 * <tt>SELECT synonym.colName  FROM suìynonym</tt><br>
	 * <p>
	 * @return the columnQualifier
	 */
	public String getColumnQualifier() {
		
		SqlExpressionElement espressionElement = null;
		
		// Se la colonna è espressa da un columnName, il qualifier è memorizzato
		// nell'unico elemento dell'espressione
		if (!this.isByExpression) {
			return this.columnQualifier;
		}
		
		// Può esssere una una colonna di database
		if (this.expression.getElements().size() == 1) {
			espressionElement = this.expression.getElements().get(0); 
			// E' una colonna di database
			if (espressionElement.getTypeElement() == EnumSqlExpressionElementType.COLUMN_NAME) {
				return espressionElement.getColumnQualifier();
			}
		}
		
		// La colonna non è descritta da una espressione
		
		return this.columnQualifier;
	}





	/**
	 * Imposta il qualifier della colonna.<br>
	 * <p>
	 * Si tratta di istruzioni del tipo:<br>
	 * <p>
	 * <tt>SELECT corrName.* FROM table AS corrName</tt><br>
	 * <tt>SELECT corrName.colName FROM table AS corrName</tt><br>
	 * <tt>SELECT table.colName FROM table</tt><br>
	 * <tt>SELECT view.colName  FROM view</tt><br>
	 * <tt>SELECT alias.colName  FROM alias</tt><br>
	 * <tt>SELECT synonym.colName  FROM suìynonym</tt><br>
	 * <p>
	 * @param columnQualifier the columnQualifier to set
	 */
	public void setColumnQualifier(String columnQualifier) {
		this.columnQualifier = columnQualifier;
	}




	/**
	 * Restituisce il nome della tabella o della view del quale sono espresse tutte le colonne<br>
	 * nella forma <tt>SELECT table|view.*</tt><br>
	 * <p>
	 * @return the tableViewName
	 */
	public String getTableViewName() {
		return tableViewName;
	}




	/**
	 * Imposta il nome della tabella o della view del quale sono espresse tutte le colonne<br>
	 * nella forma <tt>SELECT table|view.*</tt><br>
	 * <p>
	 * @param tableViewName the tableViewName to set
	 */
	public void setTableViewName(String tableViewName) {
		this.tableViewName = tableViewName;
	}




	/**
	 * Restituisce l'espressione che codifica la colonna<br>
	 * nella forma <tt>SELECT expression, ... FROM ...</tt><br>
	 * <p>
	 * @return the expression
	 */
	public SqlExpression getExpression() {
		return expression;
	}




	/**
	 * Imposta l'espressione che codifica la colonna<br>
	 * nella forma <tt>SELECT expression, ... FROM ...</tt><br>
	 * <p>
	 * @param expression the expression to set
	 */
	public void setExpression(SqlExpression expression) {
		this.expression = expression;
	}


	/**
	 * Restituisce il nome sql di definizione della colonna<br>
	 * se la colonna è memorizzata come espressione di un singolo elemento
	 * contenente una colonna.<br>
	 * <p>
	 * Si tratta di statement del tipo: <br>
	 * <p>
	 * <tt>SELECT colName, ... FROM ...</tt><br>
	 * <tt>SELECT colName AS newColName, ... FROM ...</tt><br>
	 * <p>
	 * Se non c'è una associazione con una colonna effettiva di database
	 * viene restituita una stringa vuota.<br>
	 * <p>
	 * @return the column name
	 */
	public String getColumnName() {
		
		SqlExpressionElement espressionElement = null;
		String columnName = "";
		
		// Non è sicuramente una colonna di database
		if (this.expression == null) {
			return "";
		}
		if (this.expression.getElements().size() > 1) {
			return "";
		}
		
		espressionElement = this.expression.getElements().get(0);
		
		// Non è una colonna di database
		if (espressionElement.getTypeElement() != EnumSqlExpressionElementType.COLUMN_NAME) {
			return "";
		}
		
		columnName = espressionElement.getColumnName();
		
		return columnName;
	}

	/**
	 * Restituisce true se la colonna è espressa semplicemente con il nome di una colonna<br>
	 * di una tabella di database, memorizzata come espressione di un singolo elemento<br>
	 * contenente una colonna.<br>
	 * <p>
	 * Si tratta di statement del tipo: <br>
	 * <p>
	 * <tt>SELECT colName, ... FROM ...</tt><br>
	 * <tt>SELECT colName AS newColName, ... FROM ...</tt><br>
	 * <p>
	 * Se non c'è una associazione con una colonna effettiva di database
	 * viene restituita una stringa vuota.<br>
	 * <p>
	 * @return true if column name
	 */
	public boolean isColumnName() {
		
		String columnName = "";
		columnName = getColumnName();
		if (columnName.equals("")) {
			return false;
		}
		return true;
	}



	/**
	 * Restituisce se vengono espresse tutte le colonne definite,<br>
	 * nella forma <tt>SELECT * FROM ...</tt><br>
	 * <p>
	 * @return the isByStar
	 */
	public boolean isByStar() {
		return isByStar;
	}




	/**
	 * Imposta se vengono espresse tutte le colonne definite,<br>
	 * nella forma <tt>SELECT * FROM ...</tt><br>
	 * <p>
	 * @param isByStar the isByStar to set
	 */
	public void setByStar(boolean isByStar) {
		this.isByStar = isByStar;
	}




	/**
	 * Restituisce se la colonna è codificata da una espressione<br>
	 * nella forma <tt>SELECT expression, ... FROM ...</tt><br>
	 * <p>
	 * @return the isByExpression
	 */
	public boolean isByExpression() {
		return isByExpression;
	}




	/**
	 * Imposta se la colonna è codificata da una espressione<br>
	 * nella forma <tt>SELECT expression, ... FROM ...</tt><br>
	 * <p>
	 * @param isExpression the isExpression to set
	 */
	public void setByExpression(boolean isByExpression) {
		this.isByExpression = isByExpression;
	}




	/**
	 * Restituisce se presente il nome della tabella o della view per esprimere tutte le colonne<br>
	 * nella forma <tt>SELECT table|view.*</tt><br>
	 * <p>
	 * @return the isByTableView
	 */
	public boolean isByTableView() {
		return isByTableView;
	}




	/**
	 * Imposta se presente il nome della tabella o della view per esprimere tutte le colonne<br>
	 * nella forma <tt>SELECT table|view.*</tt><br>
	 * <p>
	 * @param isByTableView the isByTableView to set
	 */
	public void setByTableView(boolean isByTableView) {
		this.isByTableView = isByTableView;
	}




	/**
	 * Restituisce se presente il correlation name per esprimere tutte le colonne<br>
	 * nella forma <tt>SELECT corr-name.*</tt><br>
	 * <p>
	 * @return the isByCorrelationName
	 */
	public boolean isByCorrelationName() {
		return isByCorrelationName;
	}




	/**
	 * Imposta se presente il correlation name per esprimere tutte le colonne<br>
	 * nella forma <tt>SELECT corr-name.*</tt><br>
	 * <p>
	 * @param isByCorrelationName the isByCorrelationName to set
	 */
	public void setByCorrelationName(boolean isByCorrelationName) {
		this.isByCorrelationName = isByCorrelationName;
	}



	
}
