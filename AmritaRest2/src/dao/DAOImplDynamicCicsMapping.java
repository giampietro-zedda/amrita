package dao;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import entities.EntityDynamicCicsMapping;
import entities.EntityDynamicValueExt;
import enums.EnumObject;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;


/**
 * Implementazione dell'interfaccia ObjectDAO per il database MySQL.
 */
public class DAOImplDynamicCicsMapping extends DAOAbstract<EntityDynamicCicsMapping> implements IDAODynamicCicsMapping{
	
	/*
	 * Constructor  
	 */
	public DAOImplDynamicCicsMapping(boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(isConnToClose, isAutocommit);
		super.typeClass = EntityDynamicCicsMapping.class;
		super.ucfg = ucfg;
	}

	/*
	 * Constructor  
	 */
	public DAOImplDynamicCicsMapping(Connection conn, boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(conn, isConnToClose, isAutocommit);
		super.typeClass = EntityDynamicCicsMapping.class;
		super.ucfg = ucfg;
	}

	

	/* (non-Javadoc)
	 * 1 Get all mappings available
	 * @see dao.IDAODynamicField#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicCicsMapping> findAll(String sys) throws SQLException, ExceptionAmritaSqlError {

		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"ORDER BY cicsName ASC, typeObject ASC, externalName ASC, dsname ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys);	
	}

	/* (non-Javadoc)
	 * 2 Get dsname mapping for specific subSys
	 * @see dao.IDAODynamicField#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicCicsMapping> findAllBySubSys(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError {

		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"      subSys = ? "
				+"ORDER BY cicsName ASC, typeObject ASC, externalName ASC, dsname ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys);	
	}

	/* (non-Javadoc)
	 * 3 Get dsname mapping for specific Cics 
	 * @see dao.IDAODynamicField#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicCicsMapping> findAllByCics(String sys, String cicsName) throws SQLException, ExceptionAmritaSqlError {

		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"      cicsName = ? "
				+"ORDER BY subSys ASC, typeObject ASC, externalName ASC, dsname ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, cicsName);	
	}

	
	/* (non-Javadoc)
	 * 4 Get dsname mapping for specific externalName
	 * @see dao.IDAODynamicField#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicCicsMapping> findAll(String sys, String subSys, EnumObject typeObject, String externalName) throws SQLException, ExceptionAmritaSqlError {

		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"      subSys = ? "
				+"      typeObject = ? "
				+"      externalName = ? "
				+"ORDER BY cicsName ASC, dsname ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, typeObject, externalName);	
	}

	/* (non-Javadoc)
	 * 5 Get dsname mapping for specific externalName and cicsName
	 * @see dao.IDAODynamicField#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicCicsMapping> findAll(String sys, String subSys, String cicsName, EnumObject typeObject, String externalName) throws SQLException, ExceptionAmritaSqlError {

		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"      subSys = ? "
				+"      cicsName = ? "
				+"      typeObject = ? "
				+"      externalName = ? "
				+"ORDER BY dsname ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, cicsName, typeObject, externalName);	
	}

	

	/*
	 * ===============================================================================================================
	 * ================ Specific methods implemented described by the interface IDAOSubFieldValue  ===================
	 * ===============================================================================================================
	 */

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
	public List<EntityDynamicCicsMapping> findAllWhere(String whereClause, String orderBy) throws ExceptionAmritaSqlError, SQLException {
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
	public List<EntityDynamicCicsMapping> readSetEntityParm(String where, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntityDynamicCicsMapping> readSetEntityWhere(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntityDynamicCicsMapping> execSqlGenericEntity(String strSql, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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


