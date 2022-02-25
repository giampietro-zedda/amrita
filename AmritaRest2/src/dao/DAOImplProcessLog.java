package dao;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import entities.EntityProcessLog;
import enums.EnumDirectivesExecution;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;

/**
 * Implementazione dell'interfaccia ObjectDAO per il database MySQL.
 */
public class DAOImplProcessLog extends DAOAbstract<EntityProcessLog> implements IDAOProcessLog {
	/*
	 * Constructor 
	 */
	public DAOImplProcessLog(boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(isConnToClose, isAutocommit);
		super.typeClass = EntityProcessLog.class;
		super.ucfg = ucfg;
	}

	/*
	 * Constructor 
	 */
	public DAOImplProcessLog(Connection conn, boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(conn, isConnToClose, isAutocommit);
		super.typeClass = EntityProcessLog.class;
		super.ucfg = ucfg;
		super.ucfg.setDbConn(conn);
	}

	/*
	 * =============================================================================
	 * ================ Specific methods implemented for DAO  ======================
	 * =============================================================================
	 */

	public List<EntityProcessLog> findAllByUser(String user, String sys, String dtStartProcess) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "WHERE" 
				          + "      user = ? "
				          + "  AND sys = ? " 
				          + "  AND dtStartProcess <= ? "
				+ "ORDER BY dtStartProcess DESC, tmStartProcess DESC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, user, sys, dtStartProcess);
	}

	@Override
	public List<EntityProcessLog> findAllByUserDateTime(String user, String sys, String dtStartProcess, String tmStartProcess) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "WHERE" 
		          + "      sys = ? "
		          + "  AND dtStartProcess <= ? "
		          + "  AND tmStartProcess <= ? "
		+ "ORDER BY dtStartProcess DESC, tmStartProcess DESC";
       return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, dtStartProcess, tmStartProcess);
    }



	public List<EntityProcessLog> findAllByDate(String sys, String dtStartProcess) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "WHERE" 
				          + "      sys = ? "
				          + "  AND dtStartProcess <= ? "
				+ "ORDER BY dtStartProcess DESC, tmStartProcess DESC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, dtStartProcess);
	}

	public List<EntityProcessLog> findAllByIdObject(String sys, String idObject, String dtStartProcess) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "WHERE" 
				          + "      sys = ? "
				          + "  AND idObject = ? "
				          + "  AND dtStartProcess <= ? "
				+ "ORDER BY dtStartProcess DESC, tmStartProcess DESC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idObject, dtStartProcess);
	}

	public List<EntityProcessLog> findAllByTypeProcess(String sys, EnumDirectivesExecution typeProcess, String dtStartProcess) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "WHERE" 
				          + "      sys = ? "
				          + "  AND typeProcess = ? "
				          + "  AND dtStartProcess <= ? "
				+ "ORDER BY idObject ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, typeProcess, dtStartProcess);
	}
	
	
	///////////////////////////////////////////////////////////////////////////////////////
	//////////////////// DAO STANDARD METHODS INHERITATED ///////////////////////////////// 
	///////////////////////////////////////////////////////////////////////////////////////
    

	/*
	 * Equivalent to readSetEntityParm.
	 * 
	 * Get all objects by a Select instruction, with specific where condition supplied, with result a List<EntityObject>
	 * Parameters can be supplied and are actualized before the execution.
	 * The where string supplied must start with the word WHERE but can be put before any further 
	 * instruction qualification as JOIN whit the current table, named T1.
	 * The orderBy by string is not supplied and eventually has to be enqueued to the where string.
	 * 
	 * <strong>Example </strong>
	 * 
	 * IDAOObject eoDAO = (MySQLDAOImplObject) AmritaStartup.sqlFactory.getDAOObject(true,true, ucfg);
     * resultset = eoDAO.readSetEntityParmRS(where, "Order By idObject");
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 */
	public List<EntityProcessLog> findAllWhere(String whereClause, String orderBy) throws ExceptionAmritaSqlError, SQLException {
		return super.readSetEntityWhereAbstract(whereClause, orderBy);
	}

	
	/*
	 * Get all objects by a Select instruction, with specific where condition supplied, with result a List<EntityObject>
	 * Parameters can be supplied and are actualized before the execution.
	 * The where string supplied must start with the word WHERE but can be put before any further 
	 * instruction qualification as JOIN whit the current table, named T1.
	 * The orderBy by string is not supplied and eventually has to be enqueued to the where string.
	 * 
	 * <strong>Example </strong>
	 * 
	 * IDAOObject eoDAO = (MySQLDAOImplObject) AmritaStartup.sqlFactory.getDAOObject(true,true, ucfg);
     * resultset = eoDAO.readSetEntityParmRS(where, "Order By idObject");
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 */

	public List<EntityProcessLog> readSetEntityParm(String where, Object... parms) throws SQLException, ExceptionAmritaSqlError {
		return super.readSetEntityParmAbstract(where, parms);
	}

	/*
	 * Get all objects by a Select instruction, with specific where condition supplied, with result a resultset
	 * Parameters can be supplied and are actualized before the execution.
	 * The where string supplied must start with the word WHERE but can be put before any further 
	 * instruction qualification as JOIN whit the current table, named T1.
	 * The orderBy by string will be appended to the instruction.
	 * 
	 * <strong>Example </strong>
	 * 
	 * IDAOObject eoDAO = (MySQLDAOImplObject) AmritaStartup.sqlFactory.getDAOObject(true,true, ucfg);
     * resultset = eoDAO.readSetEntityParmRS(where, "Order By idObject");
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 */

	public ResultSet readSetEntityParmRS(String where, String orderBy, Object... parms) throws SQLException, ExceptionAmritaSqlError {
		return super.readSetEntityParmRSAbstract(where, orderBy, parms);
	}

	/*
	 * Get all objects by a Select instruction, with specific where condition supplied, with result an array list of entity objects
	 * No parameters are supplied.
	 * The where string supplied must start with the word WHERE but can be put before any further 
	 * instruction qualification as JOIN whit the current table, named T1.
	 * The orderBy by string will be appended to the instruction.
	 * 
	 * <strong>Example </strong>
	 * 
	 * IDAOObject eoDAO = (MySQLDAOImplObject) AmritaStartup.sqlFactory.getDAOObject(true,true, ucfg);
     * List<EntitySqlGeneric> = eoDAO.readSetEntityWhere(sqlString, parm1, parm2);
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 */

	public List<EntityProcessLog> readSetEntityWhere(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError {
		return super.readSetEntityWhereAbstract(where, orderBy);
	}

	/*
	 * Exec a complete generic Sql Statement, with the List<Entity> in output
	 * Can be any type of SQL statement, DML and DDL
	 * Parameters are allowed and will be actualized in the sql statement before execution.
	 * Can be used any type of DAO available, better to use MySQLDAOImplSqlGeneric
	 * If is an update instruction dbsd can be queried to get the updateCount
	 * 
	 * <strong>Example </strong>
	 * 
	 * IDAOObject eoDAO = (MySQLDAOImplObject) AmritaStartup.sqlFactory.getDAOObject(true,true, ucfg);
     * List<EntitySqlGeneric> = eoDAO.execSqlGenericEntity(sqlString, parm1, parm2);
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 */
	

	public List<EntityProcessLog> execSqlGenericEntity(String strSql, Object... parms) throws SQLException, ExceptionAmritaSqlError {
		return super.execSqlGenericEntityAbstract(strSql,  parms);
	}

	/*
	 * Exec a complete generic Sql Statement, with the resultset in output
	 * Can be any type of SQL statement, DML and DDL
	 * No parameters are allowed.
	 * Can be used any type of DAO available, better to use MySQLDAOImplSqlGeneric
	 * If is an update instruction dbsd can be queried to get the updateCount
	 * 
	 * <strong>Example </strong>
	 * 
	 * IDAOObject eoDAO = (MySQLDAOImplObject) AmritaStartup.sqlFactory.getDAOObject(true,true, ucfg);
     * Resultset rs = eoDAO.execSqlGeneric(sqlString);
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 */

	public ResultSet execSqlGeneric(String sqlString) throws SQLException, ExceptionAmrita {
		return super.execSqlGenericRSAbstract(sqlString);
	}

	
	/*
	 * Data Base Status dettagliato ultima operazione effettuata
	 * 
	 * @see dao.IDAO#getDataBaseStatusOperation()
	 */

	public DataBaseStatusDetailed getDataBaseStatusOperation() {
		return this.dbsd;
	}

	/*
	 * Set Data Base Status dettagliato ultima operazione effettuata
	 * 
	 * @see dao.IDAO#getDataBaseStatusOperation()
	 */

	public void setDataBaseStatusOperation(DataBaseStatusDetailed dbsd) {
		this.dbsd = dbsd;	
	}

	/*
	 * Set User Configuration
	 * 
	 * @see dao.IDAO#getDataBaseStatusOperation()
	 */

	public void setUserConfiguration(UserConfiguration ucfg) {
		this.ucfg = ucfg;		
	}


}