
package analyzer;
import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDescriptorFromTableReferenceJoinedTable
 * </h1>
 * <p>
 * Descrive il table-reference joined-table nella clausola FROM.<br> 
 * <p>
 * Viene gestita la sintassi, non documentata in DB2 IBM V10, di n <br>
 * joined-table in INNER, LEFT, RIGHT o FULL, in sequenza.<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlFromTableReference
 * @see SqlSelectStatement
 * @see SqlFullSelect
*/

public class SqlFromTableReferenceJoinedTable implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	private SqlFromTableReference tableReference = null;                            // table/View to join with 
	private SqlFromTableReference tableReferenceJoin = null;                        // table/View joined
	private boolean isInnerJoin = false;                  							// table-reference INNER JOIN table-reference ON join-condition
	private boolean isLeftJoin = false;                  							// table-reference LEFT  |OUTER JOIN table-reference ON join-condition
	private boolean isRightJoin = false;                  							// table-reference RIGHT |OUTER JOIN table-reference ON join-condition
    private boolean isFullJoin = false;                  							// table-reference FULL  |OUTER JOIN table-reference ON join-condition
    private boolean isOuterJoin = false;                  							// table-reference FULL   OUTER JOIN table-reference ON join-condition
    private boolean isJoinedTableInvoked = false;                  					// (joined-table)
    private SqlFromTableReferenceJoinedTable joinedTableInvoked = null;				// (joined-table)
    private ArrayList<SqlFromTableReferenceJoinedTableCondition> al_joinCondition = null;
    private ArrayList<SqlFromTableReferenceJoinedTable> al_joinedTableExtra = null;	// INNER JOIN tb1 ON con1 LEFT JOIN tb2 ON cond2 ....
     
    
	/**
	 * Costruttore vuoto
	 */
	public SqlFromTableReferenceJoinedTable() {
		super();
		
		tableReference = new SqlFromTableReference ();
		tableReferenceJoin = new SqlFromTableReference ();
		al_joinCondition = new ArrayList<SqlFromTableReferenceJoinedTableCondition> ();
		al_joinedTableExtra = new ArrayList<SqlFromTableReferenceJoinedTable> ();
	}



	/**
	 * Restituisce il table-reference completo della da mettere in join con.<br>
	 * <p>
	 * Include la correlation clause limitata al correlation name.<b>
	 * Si utilizza lo stesso descrittore taable.reference della clausola FROM.<br>
	 * <p>
	 * @return the tableReference
	 */
	public SqlFromTableReference getTableReference() {
		return tableReference;
	}


	/**
	 * Imposta il table-reference completo della da mettere in join con.<br>
	 * <p>
	 * Include la correlation clause limitata al correlation name.<b>
	 * Si utilizza lo stesso descrittore taable.reference della clausola FROM.<br>
	 * <p>
	 * @param tableReference the tableReference to set
	 */
	public void setTableReference(SqlFromTableReference tableReference) {
		this.tableReference = tableReference;
	}


	/**
	 * Restituisce il table-reference completo della tabella relazionata in join.<br>
	 * <p>
	 * Include la correlation clause limitata al correlation name.<b>
	 * Si utilizza lo stesso descrittore taable.reference della clausola FROM.<br>
	 * <p>
	 * @return the tableReferenceJoin
	 */
	public SqlFromTableReference getTableReferenceJoin() {
		return tableReferenceJoin;
	}


	/**
	 * Imposta il table-reference completo della tabella relazionata in join.<br>
	 * <p>
	 * Include la correlation clause limitata al correlation name.<b>
	 * Si utilizza lo stesso descrittore taable.reference della clausola FROM.<br>
	 * <p>
	 * @param tableReferenceJoin the tableReferenceJoin to set
	 */
	public void setTableReferenceJoin(SqlFromTableReference tableReferenceJoin) {
		this.tableReferenceJoin = tableReferenceJoin;
	}



	/**
	 * Restituisce se il join è del tipo <tt>INNER JOIN</tt>.<br>
	 * <p>
	 * <tt>table-reference INNER JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @return the isInnerJoin
	 */
	public boolean isInnerJoin() {
		return isInnerJoin;
	}


	/**
	 * Imposta se il join è del tipo <tt>INNER JOIN</tt>.<br>
	 * <p>
	 * <tt>table-reference INNER JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @param isInnerJoin the isInnerJoin to set
	 */
	public void setInnerJoin(boolean isInnerJoin) {
		this.isInnerJoin = isInnerJoin;
	}


	/**
	 * Restituisce se il join è del tipo <tt>LEFT JOIN</tt>.<br>
	 * <p>
	 * <tt>table-reference LEFT  |OUTER JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @return the isLeftJoin
	 */
	public boolean isLeftJoin() {
		return isLeftJoin;
	}


	/**
	 * Imposta se il join è del tipo <tt>LEFT JOIN</tt>.<br>
	 * <p>
	 * <tt>table-reference LEFT  |OUTER JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @param isLeftJoin the isLeftJoin to set
	 */
	public void setLeftJoin(boolean isLeftJoin) {
		this.isLeftJoin = isLeftJoin;
	}


	/**
	 * Restituisce se il join è del tipo <tt>RIGHT JOIN</tt>.<br>
	 * <p>
	 * <tt>table-reference LEFT  |OUTER JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @return the isRightJoin
	 */
	public boolean isRightJoin() {
		return isRightJoin;
	}


	/**
	 * Imposta se il join è del tipo <tt>RIGHT JOIN</tt>.<br>
	 * <p>
	 * <tt>table-reference LEFT  |OUTER JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @param isRightJoin the isRightJoin to set
	 */
	public void setRightJoin(boolean isRightJoin) {
		this.isRightJoin = isRightJoin;
	}


	/**
	 * Restituisce se il join è del tipo <tt>FULL JOIN</tt>.<br>
	 * <p>
	 * <tt>table-reference LEFT  |OUTER JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @return the isFullJoin
	 */
	public boolean isFullJoin() {
		return isFullJoin;
	}


	/**
	 * Imposta se il join è del tipo <tt>FULL JOIN</tt>.<br>
	 * <p>
	 * <tt>table-reference LEFT  |OUTER JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @param isFullJoin the isFullJoin to set
	 */
	public void setFullJoin(boolean isFullJoin) {
		this.isFullJoin = isFullJoin;
	}


	/**
	 * Restituisce se il join è del tipo <tt>OUTER JOIN</tt>.<br>
	 * <p>
	 * <tt>table-reference LEFT|RIGHT|FULL  OUTER JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @return the isOuterJoin
	 */
	public boolean isOuterJoin() {
		return isOuterJoin;
	}


	/**
	 * Imposta se il join è del tipo <tt>OUTER JOIN</tt>.<br>
	 * <p>
	 * <tt>table-reference LEFT|RIGHT|FULL  OUTER JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @param isOuterJoin the isOuterJoin to set
	 */
	public void setOuterJoin(boolean isOuterJoin) {
		this.isOuterJoin = isOuterJoin;
	}


	/**
	 * Restituisce se il descrittore joined-table codificato ricorsivamente</tt>.<br>
	 * <p>
	 * <tt>( joined-table )</tt>
	 * <p>
	 * @return the isJoinedTableInvoked
	 */
	public boolean isJoinedTableInvoked() {
		return isJoinedTableInvoked;
	}


	/**
	 * Imposta se il descrittore joined-table codificato ricorsivamente</tt>.<br>
	 * <p>
	 * <tt>( joined-table )</tt>
	 * <p>
	 * @param isJoinedTableInvoked the isJoinedTableInvoked to set
	 */
	public void setJoinedTableInvoked(boolean isJoinedTableInvoked) {
		this.isJoinedTableInvoked = isJoinedTableInvoked;
	}


	/**
	 * Restituisce il descrittore joined-table codificato ricorsivamente</tt>.<br>
	 * <p>
	 * <tt>( joined-table )</tt>
	 * <p>
	 * @return the joinedTableInvoked
	 */
	public SqlFromTableReferenceJoinedTable getJoinedTableInvoked() {
		return joinedTableInvoked;
	}

	
	
	/**
	 * Restituisce i descrittori delle tabelle in join</tt>.<br>
	 * <p>
	 * Vengono restituiti 
	 * <p>
	 * @return the joinedTableInvoked
	 */
	public ArrayList<SqlFromTableReferenceJoinedTable> getJoinedTables() {
		ArrayList<SqlFromTableReferenceJoinedTable> al_fromTableReferenceJoinedTable = null;
		
		al_fromTableReferenceJoinedTable = new ArrayList<SqlFromTableReferenceJoinedTable> ();
		
		// Porto in output la prima tabella in Join
		al_fromTableReferenceJoinedTable.add(this);

	    // Estraggo ricorsivamente le tabelle in join dentro parentesi
		if (this.isJoinedTableInvoked) {
			getJoinedTablesRecursive(this.joinedTableInvoked, al_fromTableReferenceJoinedTable);
		}
		
		// Porto in output tutte le tabelle in join extra successive eventuali
		for (SqlFromTableReferenceJoinedTable sqlFromTableReferenceJoinedTable : this.al_joinedTableExtra) {
			getJoinedTablesRecursive(sqlFromTableReferenceJoinedTable, al_fromTableReferenceJoinedTable);
		}
		
		return al_fromTableReferenceJoinedTable;
	}

	/*
	 * Accoda ricorsivamente i descrittori completi delle joined-tables.
	 * 
	 * Il primo elemento restituito contiene la prima tabella da mettere in join con.
	 * I successivi elementi contengono le altre tabelle in join.
	 * Ogni elemento contiene tutte le informazioni del table.reference, delle condizioni e del tipo di join
	 * 
	 */
	public void getJoinedTablesRecursive(SqlFromTableReferenceJoinedTable fromTableReferenceJoinedTable, ArrayList<SqlFromTableReferenceJoinedTable> al_fromTableReferenceJoinedTable) {
		
	    // Estraggo ricorsivamente le tabelle in join dentro parentesi
		if (fromTableReferenceJoinedTable.isJoinedTableInvoked) {
			getJoinedTablesRecursive(this.joinedTableInvoked, al_fromTableReferenceJoinedTable);
		}

	    // Porto in output la tabella in join
		if (!fromTableReferenceJoinedTable.isJoinedTableInvoked) {
			al_fromTableReferenceJoinedTable.add(fromTableReferenceJoinedTable);
		}
		
		// Porto in output tutte le tabelle in join extra successive eventuali
		for (SqlFromTableReferenceJoinedTable sqlFromTableReferenceJoinedTable : fromTableReferenceJoinedTable.al_joinedTableExtra) {
			al_fromTableReferenceJoinedTable.add(sqlFromTableReferenceJoinedTable);
		}
		
		return ;
	}

	

	/**
	 * Imposta il descrittore joined-table codificato ricorsivamente</tt>.<br>
	 * <p>
	 * <tt>( joined-table )</tt>
	 * <p>
	 * @param joinedTableInvoked the joinedTableInvoked to set
	 */
	public void setJoinedTableInvoked(SqlFromTableReferenceJoinedTable joinedTableInvoked) {
		this.joinedTableInvoked = joinedTableInvoked;
	}


	/**
	 * Restituisce le codizioni di join</tt>.<br>
	 * <p>
	 * <tt>table-reference LEFT|RIGHT|FULL  OUTER JOIN table-reference ON join-conditions</tt>
	 * <p>
	 * <b>INNER, LEFT OUTER, and RIGHT OUTER joins:</b>
	 * <p>
	 * Viene resituito un solo elemento con una <tt>search-condition</tt><br>
	 * <p>
	 * <b>FULL OUTER joins:</b><br>
	 * <p>
	 * Viene restituito un numero di elementi >= 1 in <tt>AND</tt> del tipo<br>
	 * <p>
	 * <tt>full-join-expression = full-join-expression</tt>
	 * <p>
	 * <b>full-join-expression:</b><br>
	 * <p>
	 * Viene restituito un elemento che descrive:<br>
	 * <p>
	 * <ul>
	 * <li>column-name</li>
	 * <li>cast-function</li>
	 * <li>COALESCE( .. )</li>
	 * </ul>
	 * <p>
	 * @return the al_joinCondition
	 */
	public ArrayList<SqlFromTableReferenceJoinedTableCondition> getJoinConditions() {
		return al_joinCondition;
	}


	/**
	 * Imposta le codizioni di join</tt>.<br>
	 * <p>
	 * <tt>table-reference LEFT|RIGHT|FULL  OUTER JOIN table-reference ON join-conditions</tt>
	 * <p>
	 * <b>INNER, LEFT OUTER, and RIGHT OUTER joins:</b>
	 * <p>
	 * Viene resituito un solo elemento con una <tt>search-condition</tt><br>
	 * <p>
	 * <b>FULL OUTER joins:</b><br>
	 * <p>
	 * Viene restituito un numero di elementi >= 1 in <tt>AND</tt> del tipo<br>
	 * <p>
	 * <tt>full-join-expression = full-join-expression</tt>
	 * <p>
	 * <b>full-join-expression:</b><br>
	 * <p>
	 * può assumere le seguenti forme:<br>
	 * <p>
	 * <ul>
	 * <li>column-name</li>
	 * <li>cast-function</li>
	 * <li>COALESCE( .. )</li>
	 * </ul>
	 * <p>
	 * @param al_joinCondition the al_joinCondition to set
	 */
	public void setJoinConditions(ArrayList<SqlFromTableReferenceJoinedTableCondition> al_joinCondition) {
		this.al_joinCondition = al_joinCondition;
	}


	/**
	 * Restituisce i descrittori di joined-table extra successivi al primo.<br>
	 * <p>
	 * Viene così gestita la sintassi del DB2 su dipartimentale.<br>
	 * In DB2 V10 su mainframe la sintassi delle joined-table prevede una sola coppia
	 * di tabelle in join.<br>
	 * Questa struttura permette di codificare in JOIN con la prima tabella,<br>
	 * un numero illimitato di tabelle.<br>
	 * <p>
	 * Viene gestita la seguente situazione:<br>
	 * <p>
	 *  ... <br>
	 *  ... <br>
	 *  <tt>
	 *            FROM<br>
     *               tbName1       AS corrName1,<br>
     *                 .....
     *               tbName4 AS corrName4  <br>
     *               tbName5 LEFT  OUTER JOIN tbName5  AS corrName5  ON (cond5)<br>
     *               tbName6 INNER       JOIN tbName6  AS corrName6  ON (cond6)<br>
     *               tbName7 RIGHT OUTER JOIN VENDOR AS VE ON<br>
     * <p>
	 * @return the al_joinedTableExtra
	 */
	public ArrayList<SqlFromTableReferenceJoinedTable> getJoinedTablesExtra() {
		return al_joinedTableExtra;
	}


	/**
	 * Imposta i descrittori di joined-table extra successivi al primo.<br>
	 * <p>
	 * Viene così gestita la sintassi del DB2 su dipartimentale.<br>
	 * In DB2 V10 su mainframe la sintassi delle joined-table prevede una sola coppia
	 * di tabelle in join.<br>
	 * Questa struttura permette di codificare in JOIN con la prima tabella,<br>
	 * un numero illimitato di tabelle.<br>
	 * <p>
	 * Viene gestita la seguente situazione:<br>
	 * <p>
	 *  ... <br>
	 *  ... <br>
	 *  <tt>
	 *            FROM<br>
     *               tbName1       AS corrName1,<br>
     *                 .....
     *               tbName4 AS corrName4  <br>
     *               tbName5 LEFT  OUTER JOIN tbName5  AS corrName5  ON (cond5)<br>
     *               tbName6 INNER       JOIN tbName6  AS corrName6  ON (cond6)<br>
     *               tbName7 RIGHT OUTER JOIN VENDOR AS VE ON<br>
     * <p>
	 * @param alJoinedTableExtra the al_joinedTableExtra to set
	 */
	public void setJoinedTablesExtra(ArrayList<SqlFromTableReferenceJoinedTable> al_joinedTableExtra) {
		this.al_joinedTableExtra = al_joinedTableExtra;
	}

	
	
}
