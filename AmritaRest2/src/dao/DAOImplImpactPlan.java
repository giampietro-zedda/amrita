package dao;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import entities.EntityImpactPlan;
import enums.EnumObject;
import enums.EnumTypeImpactChange;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;

/**
 * Implementazione dell'interfaccia ObjectDAO per il database MySQL.
 */
public class DAOImplImpactPlan extends DAOAbstract<EntityImpactPlan> implements IDAOImpactPlan {
	/*
	 * Constructor 
	 */
	public DAOImplImpactPlan(boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(isConnToClose, isAutocommit);
		super.typeClass = EntityImpactPlan.class;
		super.ucfg = ucfg;
	}

	/*
	 * Constructor 
	 */
	public DAOImplImpactPlan(Connection conn, boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(conn, isConnToClose, isAutocommit);
		super.typeClass = EntityImpactPlan.class;
		super.ucfg = ucfg;
		super.ucfg.setDbConn(conn);
	}


	/*
	 * 1) Get all operations of a specific impact plan
	 */
	@Override
	public List<EntityImpactPlan> findAll(String sys, String idPlan) throws SQLException, ExceptionAmritaSqlError {		
		String FIND_ALL_WHERE = "" 
				+ "WHERE " 
				+ "     sys = ? "  
				+ " AND idPlan = ? "
				+ "ORDER BY numOp ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idPlan);

	}

	/*
	 * 2) Get a specific  operation of a specific impact plan
	 */
	@Override
	public List<EntityImpactPlan> findAll(String sys, String idPlan, int numOp) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "" 
				+ "WHERE " 
				+ "     sys = ? " 
				+ " AND idPlan = ? "
				+ " AND numOp = ? "
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idPlan, numOp);
	}

	/*
	 * 3) Get all operations related to an object (map, pgm, copy, ...)
	 */
	@Override
	public List<EntityImpactPlan> findAll(String sys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "" 
				+ "WHERE " 
				+ "     sys = ? " 
				+ " AND idObject = ? "
				+ " AND typeObject = ? "
				+ "ORDER BY numOp ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idObject, typeObject);
	}

	/*
	 * 4)  4) Get all operations related to a change type in a specific subSys
	 */
	@Override
	public List<EntityImpactPlan> findAll(String sys, String subSys, EnumTypeImpactChange typeImpactChange) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "" 
				+ "WHERE " 
				+ "     sys = ? " 
				+ " AND subSys = ? " 
				+ " AND typeImpactChange = ? "
				+ "ORDER BY idPlan ASC "
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, typeImpactChange);
	}

	/*
	 * 5) Get all operations related to a change type for a specific object
	 */
	@Override
	public List<EntityImpactPlan> findAll(String sys, String idObject, EnumObject typeObject, EnumTypeImpactChange typeImpactChange) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "" 
				+ "WHERE " 
				+ "     sys = ? " 
				+ " AND idObject = ? " 
				+ " AND typeObject = ? " 
				+ " AND typeImpactChange = ? "
				+ "ORDER BY idPlan ASC "
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idObject, typeObject, typeImpactChange);
	}

	/*
	 * Equivalent to readSetEntityParm.
	 * 
	 * Get all objects by a Select instruction, with specific where condition supplied, with result a List<EntityImpactPlan>
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
	public List<EntityImpactPlan> findAllWhere(String whereClause, String orderBy) throws ExceptionAmritaSqlError, SQLException {
		return super.readSetEntityWhereAbstract(whereClause, orderBy);
	}

	
	/*
	 * Get all objects by a Select instruction, with specific where condition supplied, with result a List<EntityImpactPlan>
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
	public List<EntityImpactPlan> readSetEntityParm(String where, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntityImpactPlan> readSetEntityWhere(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError {
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
	
	public List<EntityImpactPlan> execSqlGenericEntity(String strSql, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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
	@Override
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