package dao;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import entities.EntityDynamicFieldSub;
import enums.EnumObject;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;


/**
 * Implementazione dell'interfaccia ObjectDAO per il database MySQL.
 */
public class DAOImplDynamicFieldSub extends DAOAbstract<EntityDynamicFieldSub> implements IDAODynamicFieldSub{
	
	/*
	 * Constructor  
	 */
	public DAOImplDynamicFieldSub(boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(isConnToClose, isAutocommit);
		super.typeClass = EntityDynamicFieldSub.class;
		super.ucfg = ucfg;
	}

	/*
	 * Constructor  
	 */
	public DAOImplDynamicFieldSub(Connection conn, boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(conn, isConnToClose, isAutocommit);
		super.typeClass = EntityDynamicFieldSub.class;
		super.ucfg = ucfg;
	}

	/*
	 * ===============================================================================================================
	 * ================ Specific methods implemented described by the interface IDAODynamicFieldSub  =================
	 * ===============================================================================================================
	 */

	/* (non-Javadoc)
	 * 1 Get all Dynamic sub fields defined for the dynamic field, at a specific instruction
	 * @see dao.IDAODynamicFieldSub#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int)
	 */
	@Override
	public List<EntityDynamicFieldSub> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr) throws SQLException, ExceptionAmritaSqlError {
		//  Find sub fields list for the object at the instruction
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObject = ? "  
				+"  AND typeObject = ? "  
				+"  AND numInstr = ? "  
				+"ORDER BY idField ASC, numSubField ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObject,  typeObject, numInstr);	
	}

	/* (non-Javadoc)
	 * 2 Get all Dynamic sub fields defined for the specific dynamic field, by name 
	 * @see dao.IDAODynamicFieldSub#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, java.lang.String)
	 */
	@Override
	public List<EntityDynamicFieldSub> findAll(String sys, String subSys, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError {
		//  Find sub fields list for the object and the dynamic field
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObject = ? "  
				+"  AND typeObject = ? "  
				+"  AND idField = ? "  
				+"ORDER BY numInstr ASC, numSubField ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObject,  typeObject, idField);	
	}

	/* (non-Javadoc)
	 * 3 Get all Dynamic sub fields defined for the specific dynamic field, by numInstr, idField
	 * @see dao.IDAODynamicFieldSub#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicFieldSub> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField) throws SQLException, ExceptionAmritaSqlError {
		//  Find sub fields list for the object and the dynamic field at a specific instruction
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObject = ? "  
				+"  AND typeObject = ? " 
				+"  AND numInstr = ? "  
				+"  AND idField = ? "  
				+"ORDER BY numSubField ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObject,  typeObject, numInstr, idField);	
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
	public List<EntityDynamicFieldSub> findAllWhere(String whereClause, String orderBy) throws ExceptionAmritaSqlError, SQLException {
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
	public List<EntityDynamicFieldSub> readSetEntityParm(String where, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntityDynamicFieldSub> readSetEntityWhere(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntityDynamicFieldSub> execSqlGenericEntity(String strSql, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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


