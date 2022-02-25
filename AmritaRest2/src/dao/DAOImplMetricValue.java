package dao;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import entities.EntityMetricValue;
import enums.EnumMetricsScope;
import enums.EnumObject;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;

/**
 * Implementazione dell'interfaccia ObjectDAO per il database MySQL.
 */
public class DAOImplMetricValue extends DAOAbstract<EntityMetricValue> implements IDAOMetricValue{

	/*
	 * Constructor 
	 */
	public DAOImplMetricValue(boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(isConnToClose, isAutocommit);
		super.typeClass = EntityMetricValue.class;
		super.ucfg = ucfg;
	}

	/*
	 * Constructor 
	 */
	public DAOImplMetricValue(Connection conn, boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(conn, isConnToClose, isAutocommit);
		super.typeClass = EntityMetricValue.class;
		super.ucfg = ucfg;
	}

	/*
	 * ===============================================================================================================
	 * ================ Specific methods implemented for DAO                                    ======================
	 * ===============================================================================================================
	 */

	/* (non-Javadoc) 1
	 * @see dao.IDAOMetricValue#findAll(java.lang.String, java.lang.String, enums.EnumMetricsScope, java.lang.String, enums.EnumObject, java.lang.String)
	 */
	@Override
	public List<EntityMetricValue> findAll(String sys, String subSys, EnumMetricsScope scope, String idObject, EnumObject typeObject, String section) throws SQLException, ExceptionAmritaSqlError {
	    String FIND_ALL_WHERE = ""
				+"WHERE"	
				+"      sys = ? "
				+"  AND subSys = ? "
				+"  AND scope = ? "  
				+"  AND idObject = ? "  
				+"  AND typeObject = ? "  				
				+"  AND section = ? "  
				;
	   return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, scope, idObject, typeObject, section);	
	}

	/* (non-Javadoc) 2
	 * @see dao.IDAOMetricValue#findAll(java.lang.String, java.lang.String, enums.EnumMetricsScope, java.lang.String, enums.EnumObject)
	 */
	@Override
	public List<EntityMetricValue> findAll(String sys, String subSys, EnumMetricsScope scope, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
	    String FIND_ALL_WHERE = ""
	    +"WHERE"	
		+"      sys = ? "
		+"  AND subSys = ? "
		+"  AND scope = ? "  
		+"  AND idObject = ? "  
		+"  AND typeObject = ? "  				
		+"ORDER BY section ASC"
		;
        return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, scope, idObject, typeObject);	
	}

	/* (non-Javadoc 3
	 * @see dao.IDAOMetricValue#findAll(java.lang.String, java.lang.String, enums.EnumMetricsScope)
	 */
	@Override
	public List<EntityMetricValue> findAll(String sys, String subSys, EnumMetricsScope scope) throws SQLException, ExceptionAmritaSqlError {
	    String FIND_ALL_WHERE = ""
	    +"WHERE"	
		+"      sys = ? "
		+"  AND subSys = ? "
		+"  AND scope = ? "  
		+"ORDER BY idObject, typeObject, section ASC"
		;
        return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, scope);	
	}

	/* (non-Javadoc) 4
	 * @see dao.IDAOMetricValue#findAll(java.lang.String, enums.EnumMetricsScope)
	 */
	@Override
	public List<EntityMetricValue> findAll(String sys, EnumMetricsScope scope) throws SQLException, ExceptionAmritaSqlError {
	    String FIND_ALL_WHERE = ""
	    +"WHERE"	
		+"      sys = ? "
		+"  AND scope = ? "  
		+"ORDER BY subSys ASC, idObject ASC, typeObject ASC, section ASC"
		;
        return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, scope);	
	}

	@Override
	public List<EntityMetricValue> findAll(String sys, String subSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
	    String FIND_ALL_WHERE = ""
	    +"WHERE"	
		+"      sys = ? "
		+"  AND subSys = ? "
		+"  AND idObject = ? "  
		+"  AND typeObject = ? "  
		+"ORDER BY scope, section ASC"
		;
        return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObject, typeObject);	
	}

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
	public List<EntityMetricValue> findAllWhere(String whereClause, String orderBy) throws ExceptionAmritaSqlError, SQLException {
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
	@Override
	public List<EntityMetricValue> readSetEntityParm(String where, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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
	@Override
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
	@Override
	public List<EntityMetricValue> readSetEntityWhere(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError {
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
	
	@Override
	public List<EntityMetricValue> execSqlGenericEntity(String strSql, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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
	@Override
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
	@Override
	public void setDataBaseStatusOperation(DataBaseStatusDetailed dbsd) {
		this.dbsd = dbsd;	
	}

	/*
	 * Set User Configuration
	 * 
	 * @see dao.IDAO#getDataBaseStatusOperation()
	 */
	@Override
	public void setUserConfiguration(UserConfiguration ucfg) {
		this.ucfg = ucfg;		
	}

	
}