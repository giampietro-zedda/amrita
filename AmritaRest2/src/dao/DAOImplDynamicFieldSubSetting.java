package dao;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import entities.EntityDynamicFieldSubSetting;
import enums.EnumObject;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;


/**
 * Implementazione dell'interfaccia ObjectDAO per il database MySQL.
 */
public class DAOImplDynamicFieldSubSetting extends DAOAbstract<EntityDynamicFieldSubSetting> implements IDAODynamicFieldSubSetting{
	
	/*
	 * Constructor  
	 */
	public DAOImplDynamicFieldSubSetting(boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(isConnToClose, isAutocommit);
		super.typeClass = EntityDynamicFieldSubSetting.class;
		super.ucfg = ucfg;
	}

	/*
	 * Constructor  
	 */
	public DAOImplDynamicFieldSubSetting(Connection conn, boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(conn, isConnToClose, isAutocommit);
		super.typeClass = EntityDynamicFieldSubSetting.class;
		super.ucfg = ucfg;
	}

	/*
	 * ===============================================================================================================
	 * ================ Specific methods implemented described by the interface IDAODynamicFieldSub  =================
	 * ===============================================================================================================
	 */



	/* (non-Javadoc)
	 * 1 Get all Dynamic sub field setting for a specific subField of a dynamic instruction
	 * @see dao.IDAODynamicFieldSub#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicFieldSubSetting> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField, String idSubField) throws SQLException, ExceptionAmritaSqlError {
		//  Find sub fields list for the object and the dynamic field at a specific instruction
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObject = ? "  
				+"  AND typeObject = ? " 
				+"  AND numInstr = ? "  
				+"  AND idField = ? "  
				+"  AND idSubField = ? "  
				+"ORDER BY idPgmSet ASC, numChain ASC, progr ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObject,  typeObject, numInstr, idField, idSubField);	
	}

	/* (non-Javadoc)
	 * 2 Get all Dynamic sub field setting for a specific subField of a dynamic instruction
	 * @see dao.IDAODynamicFieldSub#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicFieldSubSetting> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField, String idSubField, String idPgmSet) throws SQLException, ExceptionAmritaSqlError {
		//  Find sub fields list for the object and the dynamic field at a specific instruction
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObject = ? "  
				+"  AND typeObject = ? " 
				+"  AND numInstr = ? "  
				+"  AND idField = ? "  
				+"  AND idSubField = ? "  
				+"  AND idPgmSet = ? " 
				+"ORDER BY idPgmSet ASC, numChain ASC, progr ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObject,  typeObject, numInstr, idField, idSubField, idPgmSet);	
	}

	/* (non-Javadoc)
	 * 3 Get all Dynamic sub field setting for a specific subField of a dynamic instruction
	 * @see dao.IDAODynamicFieldSub#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicFieldSubSetting> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField, String idSubField, String idPgmSet, int numChain) throws SQLException, ExceptionAmritaSqlError {
		//  Find sub fields list for the object and the dynamic field at a specific instruction
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObject = ? "  
				+"  AND typeObject = ? " 
				+"  AND numInstr = ? "  
				+"  AND idField = ? "    
				+"  AND idSubField = ? "  
				+"  AND idPgmSet = ? " 
				+"  AND numChain = ? "  
				+"ORDER BY idPgmSet ASC, numChain ASC, progr ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObject,  typeObject, numInstr, idField, idSubField, idPgmSet, numChain);	
	}


	/* (non-Javadoc)
	 * 4 Get all Dynamic sub fields settings only LAST_SET_BY_COBOL_USING_PARM | LAST_SET_BY_COBOL_LINKAGE 
	 *   for logic spread in callers/called
	 * @see dao.IDAODynamicFieldSub#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicFieldSubSetting> findAll(String sys, String subSys, boolean onlyUnresolved) throws SQLException, ExceptionAmritaSqlError {
		String andOnlyUnresolved = "";
		
		if (onlyUnresolved) {
			andOnlyUnresolved = " AND T2.solved = false ";
		}
		
		//  Find sub fields list for the object and the dynamic field at a specific instruction
		String FIND_ALL_WHERE = ""
				+ "INNER JOIN dynamicfieldsub T2  "	
				+ "   ON T1.sys = T2.sys "	
				+ "  AND T1.subSys = T2.subSys "	
				+ "  AND T1.idObject = T2.idObject "	
				+ "  AND T1.typeObject = T2.typeObject "	
				+ "  AND T1.numInstr = T2.numInstr "	
				+ "  AND T1.idField = T2.idField  "	
				+ "  AND T1.idSubField = T2.idSubField "	
				+ "WHERE "	
				+"       T1.sys = ? "
				+ "  AND T1.setMode IN (6, 7)  "    // LAST_SET_BY_COBOL_USING_PARM | LAST_SET_BY_COBOL_LINKAGE
				+ "  AND T2.spreaded = true "	
				+ andOnlyUnresolved	
				+ "ORDER BY T1.idObject ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys);	
	}

	/* (non-Javadoc)
	 * 5 Get all Dynamic sub fields settings only LAST_SET_BY_COBOL_USING_PARM | LAST_SET_BY_COBOL_LINKAGE 
	 *   for logic spread in callers/called in all subSys
	 * @see dao.IDAODynamicFieldSub#findAll(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, int, java.lang.String)
	 */
	@Override
	public List<EntityDynamicFieldSubSetting> findAll(String sys, boolean onlyUnresolved) throws SQLException, ExceptionAmritaSqlError {
		String andOnlyUnresolved = "";
		
		if (onlyUnresolved) {
			andOnlyUnresolved = " AND T2.solved = false ";
		}
		
		//  Find sub fields list for the object and the dynamic field at a specific instruction
		String FIND_ALL_WHERE = ""
				+ "INNER JOIN dynamicfieldsub T2  "	
				+ "   ON T1.sys = T2.sys "	
				+ "  AND T1.idObject = T2.idObject "	
				+ "  AND T1.typeObject = T2.typeObject "	
				+ "  AND T1.numInstr = T2.numInstr "	
				+ "  AND T1.idField = T2.idField  "	
				+ "  AND T1.idSubField = T2.idSubField "	
				+ "WHERE "	
				+ "      T1.sys = ? "
				+ "  AND T1.setMode IN (6, 7, 25)  "    // LAST_SET_BY_COBOL_USING_PARM | LAST_SET_BY_COBOL_LINKAGE | LAST_SET_BY_CICS_DFHCOMMAREA
				+ "  AND T2.spreaded = true "	
				+ andOnlyUnresolved	
				+ "ORDER BY T1.idObject ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys);	
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
	public List<EntityDynamicFieldSubSetting> findAllWhere(String whereClause, String orderBy) throws ExceptionAmritaSqlError, SQLException {
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
	public List<EntityDynamicFieldSubSetting> readSetEntityParm(String where, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntityDynamicFieldSubSetting> readSetEntityWhere(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntityDynamicFieldSubSetting> execSqlGenericEntity(String strSql, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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


