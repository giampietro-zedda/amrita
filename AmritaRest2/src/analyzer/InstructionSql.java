package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumInstrDataCategory;;


/**
 * 
 * Copyright (c) 2009-2011 e-Amrita - ing. Giampietro Zedda    Turin (ITALY)
 * 
 * <h1>
 * InstructionSql
 * </h1>
 *  <p>
 * Questa classe modella uno specifico statement sorgente di un precompilatore Sql. <br>
 * Si tratta du una generica Exec Cics. Questa classe
 * eredida da Instruction, che gestisce tutte le informazioni sorgente dell'istruzione stessa,
 * i commenti e il collegamento con il numero di riga e la posizione nel sorgente originale.
 *  
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/04/2010
 * @see Instruction
 * 
 * 
*/
public class InstructionSql extends Instruction implements Serializable, Cloneable {
	
	private static final long serialVersionUID = 1L;

		
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza                                             								  //                                                        //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**  
	 * Costruttore vuoto
	 *  
	 */
	public InstructionSql() {
		super();	
	}

	
	/**  
	 * Costruttore utilizzato per istanziare un oggetto Instruction
	 * con tutti i parametri richiesti
	 *  
	 *  @param numInstr 				Numero sequenza istruzione 0-based
	 *  @param RowStartSource 			Numero riga sorgente di analisi di inizio
	 *  @param RowEndSource 			Numero riga sorgente di analisi di fine
	 *  @param PosStartInstruction 		Posizione inizio istruzione in riga sorgente
	 *  @param PosEndInstruction        Posizione fine istruzione in riga sorgente	
	 *  @param ar_RowsSource          	Array righe sorgente con l'istruzione
	 *  @param ar_RowsSourceComments  	Array righe sorgente con i commenti precedenti l'istruzione
	 *  @param name  					Nome istruzione
	 *  @param sourceInstruction  		Istruzione completa
	 *  
	 */
	public InstructionSql(EnumInstrDataCategory typePrecompiler
			                     ,int numInstr
								 ,int RowStartSource
								 ,int RowEndSource
								 ,int PosStartInstruction
								 ,int PosEndInstruction
							 	 ,String ar_RowsSource[]
						         ,String ar_CommentsBeforeInstr[]
						         ,String ar_CommentsLeftInstr[]                        
						         ,String ar_CommentsRightInstr[]                        
						         ,String Name
						         ,String sourceInstruction
					             ) {
		
		super(numInstr
		 	 ,RowStartSource
			 ,RowEndSource
			 ,PosStartInstruction
			 ,PosEndInstruction
			 ,ar_RowsSource
			 ,ar_CommentsBeforeInstr
			 ,ar_CommentsLeftInstr
			 ,ar_CommentsRightInstr
			 ,Name
			 ,sourceInstruction
			 );	
		
		}


	/**
	 * Restituisce il nome della include
	 * dello statement Exec Sql Include name End-Exec.<br>
	 * <p>
	 * Se il parametro è inesistente restituisce stringa vuota.
	 * <p>
	 * 
	 * @return String includeName
	 */
	public String sqlIncludeGetName() {
		String includeName = "";
		includeName = (String) this.getMapDescriptorObject("$SQL-INCLUDE$NAME");
		if (includeName == null) {
			includeName = "";
		}
		return includeName;
	}

	/**
	 * Imposta il nome della include
	 * dello statement Exec Sql Include name End-Exec.<br>
	 * <p>
	 * <p>
	 * 
	 * @param String includeName
	 */
	public void sqlIncludeSetName(String includeName) {
		this.addMapDescriptorObject("$SQL-INCLUDE$NAME", includeName);
		return;
	}

	
	/**
	 * Restituisce il descrittore completo della tabella sql creata..<br>
	 * <p>
	 * Se inesistente restituisce null.
	 * <p>
	 * 
	 * @return SqlDescriptorTable sqlCreateTableGetTable
	 */
	public SqlTable sqlCreateTableGetTableDescriptor() {
		SqlTable tableDescriptor = null;
		tableDescriptor = (SqlTable) this.getMapDescriptorObject("$SQL-CREATE-TABLE$TABLE");
		return tableDescriptor;
	}

	/**
	 * Imposta il descrittore completo della tabella sql creata..<br>
	 * <p>
	 * 
	 * @param SqlTable tableDescriptor
	 */
	public void sqlCreateTableSetTableDescriptor(SqlTable tableDescriptor) {
		this.addMapDescriptorObject("$SQL-CREATE-TABLE$TABLE", tableDescriptor);
		return;
	}

	
	/**
	 * Restituisce il descrittore completo della view sql creata..<br>
	 * <p>
	 * Se inesistente restituisce null.
	 * <p>
	 * 
	 * @return viewDescriptor viewDescriptor
	 */
	public SqlView sqlCreateViewGetViewDescriptor() {
		SqlView viewDescriptor = null;
		viewDescriptor = (SqlView) this.getMapDescriptorObject("$SQL-CREATE-VIEW$VIEW");
		return viewDescriptor;
	}

	/**
	 * Imposta il descrittore completo della view sql creata..<br>
	 * <p>
	 * 
	 * @param SqlTable tableDescriptor
	 */
	public void sqlCreateViewSetViewDescriptor(SqlView viewDescriptor) {
		this.addMapDescriptorObject("$SQL-CREATE-VIEW$VIEW", viewDescriptor);
		return;
	}

	
	/**
	 * Restituisce l'elenco dei descrittori completi delle colonne  della tabella sql creata..<br>
	 * <p>
	 * Se tabella inesistente restituisce null.
	 * <p>
	 * 
	 * @return SqlDescriptorTable sqlCreateTableGetTable
	 */
	public ArrayList<SqlTableColumn> sqlCreateTableGetColsDescriptor() {
		SqlTable tableDescriptor = null;
		tableDescriptor = (SqlTable) this.getMapDescriptorObject("$SQL-CREATE-TABLE$TABLE");
		if (tableDescriptor == null) {
			return null;
		}
		return tableDescriptor.getColumns();
	}

	/**
	 * Restituisce il descrittore completo dell'indice sql creato.<br>
	 * <p>
	 * Se inesistente restituisce null.
	 * <p>
	 * 
	 * @return SqlDescriptorIndex sqlCreateIndexGetIndex
	 */
	public Sqlndex sqlCreateIndexGetIndexDescriptor() {
		Sqlndex indexDescriptor = null;
		indexDescriptor = (Sqlndex) this.getMapDescriptorObject("$SQL-CREATE-INDEX$INDEX");
		return indexDescriptor;
	}

	/**
	 * Imposta il descrittore completo dell'indice sql creato.<br>
	 * <p>
	 * 
	 * @param Sqlndex indexDescriptor
	 */
	public void sqlCreateIndexSetIndexDescriptor(Sqlndex indexDescriptor) {
		this.addMapDescriptorObject("$SQL-CREATE-INDEX$INDEX", indexDescriptor);
		return;
	}

	
	
	/**
	 * Restituisce l'elenco dei descrittori completi delle colonne dell'indice sql creato<br>
	 * <p>
	 * Se indice inesistente restituisce null.
	 * <p>
	 * 
	 * @return ArrayList<SqlDescriptorIndexColumn> sqlDescriptorIndexColumns
	 */
	public ArrayList<SqlIndexColumn> sqlCreateIndexGetColsDescriptor() {
		Sqlndex indexDescriptor = null;
		indexDescriptor = (Sqlndex) this.getMapDescriptorObject("$SQL-CREATE-INDEX$INDEX");
		if (indexDescriptor == null) {
			return null;
		}
		return indexDescriptor.getColumns();
	}


	/**
	 * Imposta il nome del data base.<br>
	 * <p>
	 * 
	 * @param String databaseName
	 */
	public void sqlCreateDatabaseSetDatabaseName(String databaseName) {
		this.addMapDescriptorObject("$SQL-DATABASE", databaseName);
		return;
	}

	
	
	/**
	 * Restituisce il nome del data base.<br>
	 * <p>
	 * Se database non codificato restituisce null.
	 * <p>
	 * 
	 * @return String databaseName
	 */
	public String sqlCreateDatabaseGetDatabaseName() {
		String databaseName = null;
		databaseName = (String) this.getMapDescriptorObject("$SQL-DATABASE");
		if (databaseName == null) {
			return null;
		}
		return databaseName;
	}

	/**
	 * Imposta il nome del data base.<br>
	 * <p>
	 * 
	 * @param String databaseName
	 */
	public void sqlCreateTablespaceSetDatabaseName(String databaseName) {
		this.addMapDescriptorObject("$SQL-DATABASE", databaseName);
		return;
	}

	
	
	/**
	 * Restituisce il nome del data base.<br>
	 * <p>
	 * Se database non codificato restituisce null.
	 * <p>
	 * 
	 * @return String databaseName
	 */
	public String sqlCreateTablespaceGetDatabaseName() {
		String databaseName = null;
		databaseName = (String) this.getMapDescriptorObject("$SQL-DATABASE");
		return databaseName;
	}

	/**
	 * Imposta il nome dell'owner della tabella o view.<br>
	 * <p>
	 * 
	 * @param String owner
	 */
	public void sqlCreateSynonymSetOwner(String owner) {
		this.addMapDescriptorObject("$SQL-OWNER", owner);
		return;
	}

	
	
	/**
	 * Restituisce il nome dell'owner della tabella o view.<br>
	 * <p>
	 * Se owner non valorizzato restituisce null.
	 * <p>
	 * 
	 * @return String owner
	 */
	public String sqlCreateSynonymGetOwner() {
		String owner = null;
		owner = (String) this.getMapDescriptorObject("$SQL-OWNER");
		return owner;
	}

	/**
	 * Imposta il nome dell'owner della tabella o view.<br>
	 * <p>
	 * 
	 * @param String owner
	 */
	public void sqlCreateAliasSetOwner(String owner) {
		this.addMapDescriptorObject("$SQL-OWNER", owner);
		return;
	}

	
	
	/**
	 * Restituisce il nome dell'owner della tabella o view.<br>
	 * <p>
	 * Se owner non valorizzato restituisce null.
	 * <p>
	 * 
	 * @return String owner
	 */
	public String sqlCreateAliasGetOwner() {
		String owner = null;
		owner = (String) this.getMapDescriptorObject("$SQL-OWNER");
		return owner;
	}

	/**
	 * Imposta il nome del sinonimo della tabella o view.<br>
	 * <p>
	 * 
	 * @param String synonymName
	 */
	public void sqlCreateSynonymSetName(String synonymName) {
		this.addMapDescriptorObject("$SQL-SYNONYM", synonymName);
		return;
	}

	
	
	/**
	 * Restituisce il nome del sinonimo della tabella o view.<br>
	 * <p>
	 * Se owner non valorizzato restituisce null.
	 * <p>
	 * 
	 * @return String synonymName
	 */
	public String sqlCreateSynonymGetName() {
		String synonymName = null;
		synonymName = (String) this.getMapDescriptorObject("$SQL-SYNONYM");
		return synonymName;
	}

	/**
	 * Imposta il nome dell'alias della tabella o view.<br>
	 * <p>
	 * 
	 * @param String aliasName
	 */
	public void sqlCreateAliasSetName(String aliasName) {
		this.addMapDescriptorObject("$SQL-ALIAS", aliasName);
		return;
	}

	
	
	/**
	 * Restituisce il nome del sinonimo della tabella o view.<br>
	 * <p>
	 * Se alias non valorizzato restituisce null.
	 * <p>
	 * 
	 * @return String aliasName
	 */
	public String sqlCreateAliasGetName() {
		String aliasName = null;
		aliasName = (String) this.getMapDescriptorObject("$SQL-ALIAS");
		return aliasName;
	}

	/**
	 * Imposta il nome della tabella o view.<br>
	 * <p>
	 * 
	 * @param String tableView
	 */
	public void sqlCreateSynonymSetTableView(String tableView) {
		this.addMapDescriptorObject("$SQL-TABLE", tableView);
		return;
	}

	
	
	/**
	 * Restituisce il nome della tabella o view.<br>
	 * <p>
	 * Se tabella o view non valorizzata restituisce null.
	 * <p>
	 * 
	 * @return String tableView
	 */
	public String sqlCreateSynonymGetTableView() {
		String tableView = null;
		tableView = (String) this.getMapDescriptorObject("$SQL-TABLE");
		return tableView;
	}

	/**
	 * Imposta il nome della tabella o view.<br>
	 * <p>
	 * 
	 * @param String tableView
	 */
	public void sqlCreateAliasSetTableView(String tableView) {
		this.addMapDescriptorObject("$SQL-TABLE", tableView);
		return;
	}

	
	/**
	 * Restituisce il proprietario della tabella o view.<br>
	 * <p>
	 * Se tabella o view non valorizzata restituisce null.
	 * <p>
	 * 
	 * @return String tableViewOwner
	 */
	public String sqlCreateSynonymGetTableViewOwner() {
		String tableViewOwner = null;
		tableViewOwner = (String) this.getMapDescriptorObject("$SQL-TABLE-OWNER");
		return tableViewOwner;
	}

	/**
	 * Imposta il proprietario della tabella o view.<br>
	 * <p>
	 * 
	 * @param String tableViewOwner
	 */
	public void sqlCreateAliasSetTableViewOwner(String tableViewOwner) {
		this.addMapDescriptorObject("$SQL-TABLE-OWNER", tableViewOwner);
		return;
	}

	
	
	/**
	 * Restituisce il nome della tabella o view.<br>
	 * <p>
	 * Se tabella o view non valorizzata restituisce null.
	 * <p>
	 * 
	 * @return String tableView
	 */
	public String sqlCreateAliasGetTableView() {
		String tableView = null;
		tableView = (String) this.getMapDescriptorObject("$SQL-TABLE");
		return tableView;
	}

	/**
	 * Imposta il nome dello storage group.<br>
	 * <p>
	 * 
	 * @param String stogroupName
	 */
	public void sqlCreateStogroupSetName(String stogroupName) {
		this.addMapDescriptorObject("$SQL-STOGROUP", stogroupName);
		return;
	}

	
	
	/**
	 * Restituisce il nome dello storage group.<br>
	 * <p>
	 * Se stogroupName non codificato restituisce null.
	 * <p>
	 * 
	 * @return String stogroupName
	 */
	public String sqlCreateStogroupGetName() {
		String stogroupName = null;
		stogroupName = (String) this.getMapDescriptorObject("$SQL-STOGROUP");
		return stogroupName;
	}

	/**
	 * Imposta se il tablespace è di tipo LOB.<br>
	 * <p>
	 * 
	 * @param boolean lobTablespace
	 */
	public void sqlCreateTablespaceSetLob(boolean lobTablespace) {
		this.addMapDescriptorObject("$SQL-TABLESPACE-LOB", lobTablespace);
		return;
	}

	
	
	/**
	 * Restituisce se il tablespace è di tipo LOB.<br>
	 * <p>
	 * 
	 * @return String databaseName
	 */
	public boolean sqlCreateTablespaceIsLob() {
		Boolean lobTablespace = null;
		lobTablespace = (Boolean) this.getMapDescriptorObject("$SQL-TABLESPACE-LOB");
		if (lobTablespace == null) {
			return false;
		}
		return lobTablespace;
	}


	/**
	 * Restituisce il descrittore dell'istruzione sql Insert<br>
	 * <p>
	 * Se non valorizzato resatituisce null.<br>
	 * <p>
	 * @return SqlInsertStatement statement coded
	 */
	public SqlInsertStatement sqlInsertGetDescriptor() {
        SqlInsertStatement sqlInserStatement = null;
        sqlInserStatement = (SqlInsertStatement) this.getMapDescriptorObject("$SQL-INSERT$DESCRIPTOR");
		return sqlInserStatement;
	}

	/**
	 * Imposta il descrittore dell'istruzione sql Insert<br>
	 * <p>
	 * Se non valorizzato resatituisce null.<br>
	 * <p>
	 * @return SqlInsertStatement statement coded
	 */
	public void sqlInsertSetDescriptor(SqlInsertStatement sqlInserStatement) {
		this.addMapDescriptorObject("$SQL-INSERT$DESCRIPTOR", sqlInserStatement);
		return;
	}
	
	
	/**
	 * Restituisce il descrittore dell'istruzione sql Declare cursor<br>
	 * <p>
	 * Se non valorizzato restituisce null.<br>
	 * <p>
	 * @return SqlDeclareCursorStatement statement coded
	 */
	public SqlDeclareCursorStatement sqlDeclareCursorGetDescriptor() {
		SqlDeclareCursorStatement sqlDeclareCursorStatement = null;
		sqlDeclareCursorStatement = (SqlDeclareCursorStatement) this.getMapDescriptorObject("$SQL-DECLARE_CURSOR$DESCRIPTOR");
		return sqlDeclareCursorStatement;
	}

	/**
	 * Imposta il descrittore dell'istruzione sql Declare Cursor<br>
	 * <p>
	 * Se non valorizzato resatituisce null.<br>
	 * <p>
	 * @return SqlDeclareCursorStatement statement coded
	 */
	public void sqlDeclareCursorSetDescriptor(SqlDeclareCursorStatement sqlDeclareCursorStatement) {
		this.addMapDescriptorObject("$SQL-DECLARE_CURSOR$DESCRIPTOR", sqlDeclareCursorStatement);
		return;
	}
	
	/**
	 * Restituisce il descrittore dell'istruzione sql Fetch<br>
	 * <p>
	 * Se non valorizzato restituisce null.<br>
	 * <p>
	 * @return SqlFetchStatement statement coded
	 */
	public SqlFetchStatement sqlFetchGetDescriptor() {
		SqlFetchStatement sqlFetchStatement = null;
		sqlFetchStatement = (SqlFetchStatement) this.getMapDescriptorObject("$SQL-FETCH$DESCRIPTOR");
		return sqlFetchStatement;
	}

	/**
	 * Imposta il descrittore dell'istruzione sql Declare Cursor<br>
	 * <p>
	 * Se non valorizzato resatituisce null.<br>
	 * <p>
	 * @return SqlFetchStatement statement coded
	 */
	public void sqlFetchSetDescriptor(SqlFetchStatement sqlFetchStatement) {
		this.addMapDescriptorObject("$SQL-FETCH$DESCRIPTOR", sqlFetchStatement);
		return;
	}
	
	/**
	 * Restituisce il descrittore dell'istruzione sql Delete<br>
	 * <p>
	 * Se non valorizzato resatituisce null.<br>
	 * <p>
	 * @return SqlDeleteStatement statement coded
	 */
	public SqlDeleteStatement sqlDeleteGetDescriptor() {
		SqlDeleteStatement sqlDeleteStatement = null;
		sqlDeleteStatement = (SqlDeleteStatement) this.getMapDescriptorObject("$SQL-DELETE$DESCRIPTOR");
		return sqlDeleteStatement;
	}

	/**
	 * Imposta il descrittore dell'istruzione sql Insert<br>
	 * <p>
	 * Se non valorizzato resatituisce null.<br>
	 * <p>
	 * @return SqlDeleteStatement statement coded
	 */
	public void sqlDeleteSetDescriptor(SqlDeleteStatement SqlDeleteStatement) {
		this.addMapDescriptorObject("$SQL-DELETE$DESCRIPTOR", SqlDeleteStatement);
		return;
	}
	
	/**
	 * Restituisce il descrittore dell'istruzione sql Update<br>
	 * <p>
	 * Se non valorizzato resatituisce null.<br>
	 * <p>
	 * @return SqlUpdateStatement statement coded
	 */
	public SqlUpdateStatement sqlUpdateGetDescriptor() {
		SqlUpdateStatement sqlUpdateStatement = null;
		sqlUpdateStatement = (SqlUpdateStatement) this.getMapDescriptorObject("$SQL-UPDATE$DESCRIPTOR");
		return sqlUpdateStatement;
	}

	/**
	 * Imposta il descrittore dell'istruzione sql Update<br>
	 * <p>
	 * Se non valorizzato resatituisce null.<br>
	 * <p>
	 * @return SqlUpdateStatement statement coded
	 */
	public void sqlUpdateSetDescriptor(SqlUpdateStatement SqlUpdateStatement) {
		this.addMapDescriptorObject("$SQL-UPDATE$DESCRIPTOR", SqlUpdateStatement);
		return;
	}
	
	/**
	 * Restituisce il descrittore dell'istruzione sql Merge<br>
	 * <p>
	 * Se non valorizzato resatituisce null.<br>
	 * <p>
	 * @return SqlMergeStatement statement coded
	 */
	public SqlMergeStatement sqlMergeGetDescriptor() {
		SqlMergeStatement sqlMergeStatement = null;
		sqlMergeStatement = (SqlMergeStatement) this.getMapDescriptorObject("$SQL-MERGE$DESCRIPTOR");
		return sqlMergeStatement;
	}

	/**
	 * Imposta il descrittore dell'istruzione sql Merge<br>
	 * <p>
	 * Se non valorizzato resatituisce null.<br>
	 * <p>
	 * @return SqlMergeStatement statement coded
	 */
	public void sqlMergeSetDescriptor(SqlMergeStatement SqlMergeStatement) {
		this.addMapDescriptorObject("$SQL-MERGE$DESCRIPTOR", SqlMergeStatement);
		return;
	}
	
	/**
	 * Restituisce il descrittore completo dello statement sql select<br>
	 * <p>
	 * Ogni informazione, anche ricorsiva su select, subselect e fullselect,
	 * si ottiene interrogando l'oggetto {@link SqlSelectStatement}<br>
	 * <p>
	 * Se inesistente restituisce null.
	 * <p>
	 * 
	 * @return selectStatement
	 */
	public SqlSelectStatement sqlSelectGetDescriptor() {
		SqlSelectStatement selectStatement = null;
		selectStatement = (SqlSelectStatement) this.getMapDescriptorObject("$SQL-SELECT$DESCRIPTOR");
		return selectStatement;
	}

	/**
	 * Imposta il descrittore completo dello statement sql select<br>
	 * <p>
	 * 
	 * @param SqlSelectStatement SqlSelectStatement
	 */
	public void sqlSelectSetDescriptor(SqlSelectStatement selectStatement) {
		this.addMapDescriptorObject("$SQL-SELECT$DESCRIPTOR", selectStatement);
		return;
	}


	/**
	 * Restituisce il nome del cursore dello statement sql Open Cursor<br>
	 * <p>
	 * @return selectStatement
	 */
	public String sqlOpenCursorGetCursorName() {
		String cursorName = "";
		cursorName = (String) this.getMapDescriptorObject("$SQL-OPEN$CURSOR");
		return cursorName;
	}

	/**
	 * Imposta il nome del cursore dello statement sql Open Cursor<br>
	 * <p>
	 * 
	 * @param String cursorName
	 */
	public void sqlOpenCursorSetCursorName(String cursorName) {
		this.addMapDescriptorObject("$SQL-OPEN$CURSOR", cursorName);
	}

	/**
	 * Restituisce le variabili host specificate nella clausiola USING dello statement sql Open Cursor<br>
	 * <p>
	 * @return ArrayList<String> hostVars
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> sqlOpenCursorGetUsingHostVars() {
		ArrayList<String> al_usingHostVar = null;
		al_usingHostVar =  (ArrayList<String>) this.getMapDescriptorObject("$SQL-OPEN$HOST-VARS");
		return al_usingHostVar;
	}

	/**
	 * Imposta le variabili host specificate nella clausiola USING dello statement sql Open Cursor<br>
	 * <p>
	 * @param String cursorName
	 */
	public void sqlOpenCursorSetUsingHostVars(ArrayList<String> al_usingHostVar) {
		this.addMapDescriptorObject("$SQL-OPEN$HOST-VARS", al_usingHostVar);
	}

	/**
	 * Restituisce il nome del descrittore dello statement sql Open Cursor<br>
	 * <p>
	 * @return descriptorName
	 */
	public String sqlOpenCursorGetUsingDescriptorName() {
		String descriptorName = "";
		descriptorName = (String) this.getMapDescriptorObject("$SQL-OPEN$DESCRIPTOR");
		return descriptorName;
	}

	/**
	 * Imposta il nome del descrittore dello statement sql Open Cursor<br>
	 * <p>
	 * 
	 * @param String usingDescriptor
	 */
	public void sqlOpenCursorSetUsingDescriptorName(String usingDescriptor) {
		this.addMapDescriptorObject("$SQL-OPEN$DESCRIPTOR", usingDescriptor);
	}

	
	
	
	/**
	 * Restituisce se specificato il descriptor-name nella clausola USING dello statement sql Open Cursor<br>
	 * <p>
	 * @return Boolean isUsingDescriptor
	 */
	public boolean sqlOpenCursorIsUsingDescriptor() {
		Boolean isUsingDescriptor = false;
		isUsingDescriptor = (Boolean) this.getMapDescriptorObject("$SQL-OPEN$IS-DESCRIPTOR");
		if (isUsingDescriptor == null) {
			return false;
		}
		return isUsingDescriptor;
	}

	/**
	 * Imposta se specificato il descriptor-name nella clausola USING dello statement sql Open Cursor<br>
	 * <p>
	 * @param boolean isUsingDescriptor
	 */
	public void sqlOpenCursorSetUsingDescriptor(boolean isUsingDescriptor) {
		this.addMapDescriptorObject("$SQL-OPEN$IS-DESCRIPTOR", isUsingDescriptor);
	}


	
	
	
	
	
	

	/**
	 * Restituisce se sono specificate le variabili host nella clausiola USING dello statement sql Open Cursor<br>
	 * <p>
	 * @return Boolean isHostVars
	 */
	public boolean sqlOpenCursorIsUsingHostVars() {
		Boolean isUsingHostVar = false;
		isUsingHostVar = (Boolean) this.getMapDescriptorObject("$SQL-OPEN$IS-HOST-VARS");
		if (isUsingHostVar == null) {
			return false;
		}
		return isUsingHostVar;
	}

	/**
	 * Imposta se sono specificate le variabili host nella clausiola USING dello statement sql Open Cursor<br>
	 * <p>
	 * @param String cursorName
	 */
	public void sqlOpenCursorSetUsingHostVars(boolean isUsingHostVar) {
		this.addMapDescriptorObject("$SQL-OPEN$IS-HOST-VARS", isUsingHostVar);
	}


	
	/**
	 * Restituisce il nome del cursore dello statement sql Close Cursor<br>
	 * <p>
	 * @return selectStatement
	 */
	public String sqlCloseCursorGetCursorName() {
		String cursorName = "";
		cursorName = (String) this.getMapDescriptorObject("$SQL-CLOSE$CURSOR");
		return cursorName;
	}

	/**
	 * Imposta il nome del cursore dello statement sql Close Cursor<br>
	 * <p>
	 * 
	 * @param String cursorName
	 */
	public void sqlCloseCursorSetCursorName(String cursorName) {
		this.addMapDescriptorObject("$SQL-CLOSE$CURSOR", cursorName);
	}


	/**
	 * Imposta il nome della tabella dello statement Lock Table<br>
	 * <p>
	 * Il nome della tabella può essere qualificato dall'owner.
	 * <p>
	 * @param String tableName
	 */
	public void sqlLockTableSetTableNameQualified(String tableName) {
		this.addMapDescriptorObject("$SQL-LOCK$TABLE", tableName);		
	}

	/**
	 * Restituisce il nome della tabella qualificato dello statement Lock Table<br>
	 * <p>
	 * Il nome della tabella può essere qualificato dall'owner.
	 * 
	 * @return tableNameQualified
	 */
	public String sqlLockTableGetTableNameQualified() {
		String tableName = "";
		tableName = (String) this.getMapDescriptorObject("$SQL-LOCK$TABLE");
		return tableName;
	}

	/**
	 * Restituisce il nome della tabella senza qualificazione, se presente, dello statement Lock Table<br>
	 * <p>
	 * Il nome della tabella può essere qualificato dall'owner o meno.
	 * 
	 * @return tableName
	 */
	public String sqlLockTableGetTableName() {
		String tableNameQualified = "";
		String tableName = "";
		int i = 0;
		tableNameQualified = (String) this.getMapDescriptorObject("$SQL-LOCK$TABLE");
        if (tableNameQualified == null) {
			return "";
		}
		
		
		i = tableNameQualified.indexOf(".");
		if (i < 0) {
			tableName = tableNameQualified;
		} else {
			tableName = tableNameQualified.substring(i + 1);
		}
		return tableName;
	}
	
	/**
	 * Restituisce l'owner di qualificazionedella tabella, se presente, dello statement Lock Table<br>
	 * <p>
	 * Il nome della tabella può essere qualificato dall'owner o meno.
	 * 
	 * @return tableOwner
	 */
	public String sqlLockTableGetTableOwner() {
		
		String tableNameQualified = "";
		String tableOwner = "";
		int i = 0;
		tableNameQualified = (String) this.getMapDescriptorObject("$SQL-LOCK$TABLE");
        if (tableNameQualified == null) {
			return "";
		}
		
		
		i = tableNameQualified.indexOf(".");
		if (i < 0) {
			tableOwner = "";
		} else {
			tableOwner = tableNameQualified.substring(0, i);
		}
	
		return tableOwner;
	}


	/**
	 * Imposta il numero della partizione specificato dalla clausola PARTITION dello statement Lock Table<br>
	 * <p>
	 * @param int partitionNumber
	 */
	public void sqlLockTableSetPartitionNumber(int partitionNumber) {
		this.addMapDescriptorObject("$SQL-LOCK$PARTITION", partitionNumber);		
	}

	/**
	 * Imposta il numero della partizione specificato dalla clausola PARTITION dello statement Lock Table<br>
	 * <p>
	 * Se non presente restituisce -1
	 * <p>
	 * @return tableOwner
	 */
	public int sqlLockTableGetPartitionNumber() {
		
		Integer partitionNumber = 0;
		partitionNumber = (Integer) this.getMapDescriptorObject("$SQL-LOCK$PARTITION");
        if (partitionNumber == null) {
			return -1;
		}
		return partitionNumber;
	}


	/**
	 * Imposta se specificato EXCLUSIVE MODE nello statement Lock Table<br>
	 * <p>
	 * @param boolean isExclusiveMode
	 */
	public void  sqlLockTableSetExclusiveMode(boolean isExclusiveMode) {
		this.addMapDescriptorObject("$SQL-LOCK$EXCLUSIVE", isExclusiveMode);	
	}

	/**
	 * Imposta se specificato SHARE MODE nello statement Lock Table<br>
	 * <p>
	 * @param boolean isShareMode
	 */
	public void  sqlLockTableSetShareMode(boolean isShareMode) {
		this.addMapDescriptorObject("$SQL-LOCK$SHARE", isShareMode);	
	}

	/**
	 *  Restituisce se specificato EXCLUSIVE MODE nello statement Lock Table<br>
	 * <p>
	 * @return boolean isExclusiveMode
	 */
	public boolean sqlLockTableIsExclusiveMode() {
		boolean isExclusiveMode = false;
		isExclusiveMode = (Boolean) this.getMapDescriptorObject("$SQL-LOCK$EXCLUSIVE"); 	
		return isExclusiveMode;
	}
	
	/**
	 *  Restituisce se specificato SHARE MODE nello statement Lock Table<br>
	 * <p>
	 * @return boolean isShareMode
	 */
	public boolean sqlLockTableIsShareMode() {
		boolean isShareMode = false;
		isShareMode = (Boolean) this.getMapDescriptorObject("$SQL-LOCK$SHARE"); 	
		return isShareMode;
	}
	

}
