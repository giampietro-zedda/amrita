package dao;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import entities.EntityObject;
import enums.EnumObject;
import enums.EnumObjectStatus;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;

/**
 * Implementazione dell'interfaccia ObjectDAO per il database MySQL.
 */
public class DAOImplObject extends DAOAbstract<EntityObject> implements IDAOObject {
	/*
	 * Constructor 
	 */
	public DAOImplObject(boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(isConnToClose, isAutocommit);
		super.typeClass = EntityObject.class;
		super.ucfg = ucfg;
	}

	/*
	 * Constructor 
	 */
	public DAOImplObject(Connection conn, boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(conn, isConnToClose, isAutocommit);
		super.typeClass = EntityObject.class;
		super.ucfg = ucfg;
		super.ucfg.setDbConn(conn);
	}

	/*
	 * =============================================================================
	 * ================ Specific methods implemented for DAO  ======================
	 * =============================================================================
	 */

	/*
	 * 1 Get all Object of the type specified (i.e. COBOL_PROGRAMS)
	 */
	public List<EntityObject> findAll(String sys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
		// Find object list by type in all subsys
		String FIND_ALL_WHERE = "" 
				+ "WHERE" + "      sys = ? " + "  AND typeObject = ? "
				+ "ORDER BY subSys, typeObject ASC, idObject ASC, subSys ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, typeObject);
	}

	/*
	 * 2 Get all Object of the type specified
	 */
	public List<EntityObject> findAll(String sys, String subSys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
		// Find object list by type
		String FIND_ALL_WHERE = "" 
				+ "WHERE " + "      sys = ? " + "  AND subSys = ? " + "  AND typeObject = ? "
				+ "ORDER BY typeObject ASC, idObject ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, typeObject);
	}

	/*
	 * 3 Get all Object with status (i.e. ANALYZED_WITH_NO_ERRORS)
	 */
	public List<EntityObject> findAll(String sys, EnumObjectStatus status) throws SQLException, ExceptionAmritaSqlError {
		// Find object list by status
		String FIND_ALL_WHERE = "" 
				+ "WHERE " + "     sys = ? " + " AND statusObject = ? "
				+ "ORDER BY subSys ASC, typeObject ASC, idObject ASC";

		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, status);
	}

	/*
	 * 4 Get all Object with status (i.e. ANALYZED_WITH_NO_ERRORS)
	 */
	public List<EntityObject> findAll(String sys, String subSys, EnumObjectStatus status) throws SQLException, ExceptionAmritaSqlError {
		// Find object list by status
		String FIND_ALL_WHERE = "" 
				+ "WHERE " + "      sys = ? " + " AND subSys = ? " + " AND statusObject = ? "
				+ "ORDER BY typeObject ASC, idObject ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, status);
	}

	/*
	 * 5 Get all Object of the type and status specified (i.e. COBOL_PROGRAM &
	 * ANALYZED_WITH_NO_ERRORS)
	 */
	public List<EntityObject> findAll(String sys, String subSys, EnumObject typeObject, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError {
		// Find object list by type and status
		String FIND_ALL_WHERE = "" 
				+ "WHERE" + "     sys = ? " + " AND subSys = ? " + " AND typeObject = ? "
				+ " AND statusObject = ? " 
				+ "ORDER BY typeObject ASC, idObject ASC";

		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, typeObject, statusObject);
	}

	/*
	 * 6 Get all Object of the type and status specified (i.e. COBOL_PROGRAM &
	 * ANALYZED_WITH_NO_ERRORS)
	 */
	public List<EntityObject> findAll(String sys, EnumObject typeObject, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError {
		// Find object list by type and status
		String FIND_ALL_WHERE = "" 
		        + "WHERE" + "     sys = ? " + " AND typeObject = ? " + " AND statusObject = ? "
				+ "ORDER BY subSys, typeObject ASC, idObject ASC";

		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, typeObject, statusObject);
	}

	/*
	 * 7 Get all Object of the sys & subsys specified (i.e. COBOL_PROGRAM &
	 * ANALYZED_WITH_NO_ERRORS)
	 * 
	 * @see dao.IDAOObject#findAllBySysSubSys(java.lang.String, java.lang.String)
	 */
	@Override
	public List<EntityObject> findAll(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError {
		// Find object list by sys and subSys
		String FIND_ALL_WHERE = "" 
				+ "WHERE" + "     sys = ? " + " AND subSys = ? "
				+ "ORDER BY typeObject ASC,idObject ASC";

		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys);
	}

	/*
	 * 8 Get all Objects with a given type
	 * 
	 * @see dao.IDAOObject#findAllSys()
	 */
	@Override
	public List<EntityObject> findAll(EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
		// Find object list by type
		String FIND_ALL_WHERE = "" 
				+ "WHERE" + "     typeObject = ? " 
				+ "ORDER BY sys, SubSys, typeObject ASC, idObject ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, typeObject);
	}

	/*
	 * 9 Get all Objects with a given system regardless type, status etc
	 * 
	 * @see dao.IDAOObject#findAllSys()
	 */
	@Override
	public List<EntityObject> findAll(String sys) throws SQLException, ExceptionAmritaSqlError {
		// Find object list by system
		String FIND_ALL_WHERE = "" 
				+ "WHERE" + "     sys = ? " 
				+ "ORDER BY subSys, TypeObject, idObject ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys);
	}

	/*
	 * 10 Get a specific Object 
	 */
	public List<EntityObject> findAll(String sys, String subSys, EnumObject typeObject, String idObject) throws SQLException, ExceptionAmritaSqlError {
		// Find object  
		String FIND_ALL_WHERE = "" 
				+ "WHERE " + "      sys = ? " + "  AND subSys = ? " + "  AND typeObject = ? " + "  AND idObject = ? ";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, typeObject);
	}

	
	/////////////////////////////////////////////////////////
	// Methods with additional options and LIKE of idObject
	/////////////////////////////////////////////////////////
		
	/* (non-Javadoc) 1
	 * @see dao.IDAOObject#findAll(java.lang.String[], java.lang.String, enums.EnumObject)
	 */
	@Override
	public List<EntityObject> findAll(String[] options, String likeIdObject, String sys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "";
		
		// Find object list by type in all subsys
		FIND_ALL_WHERE = FIND_ALL_WHERE
				+ "WHERE" + "      sys = ? " + "  AND typeObject = ? "
				+ addSqlIN_LIKE(options, likeIdObject)
				+ "ORDER BY subSys ASC, typeObject ASC, idObject ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, typeObject);
	}

	/* (non-Javadoc) 2
	 * @see dao.IDAOObject#findAll(java.lang.String[], java.lang.String, java.lang.String, enums.EnumObject)
	 */
	@Override
	public List<EntityObject> findAll(String[] options, String likeIdObject, String sys, String subSys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "";
		
	    FIND_ALL_WHERE = FIND_ALL_WHERE
	    		       + "WHERE " + "      sys = ? " + "  AND subSys = ? " + "  AND typeObject = ? "
	    	           + addSqlIN_LIKE(options, likeIdObject)
			           + "ORDER BY typeObject ASC, idObject ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, typeObject);
	}

	/* (non-Javadoc) 3
	 * @see dao.IDAOObject#findAll(java.lang.String[], java.lang.String, enums.EnumObjectStatus)
	 */
	@Override
	public List<EntityObject> findAll(String[] options, String likeIdObject, String sys, EnumObjectStatus status) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "";
		
		// Find object list by status
		FIND_ALL_WHERE = FIND_ALL_WHERE
				+ "WHERE " + "     sys = ? " + " AND statusObject = ? "
				+ addSqlIN_LIKE(options, likeIdObject)
				+ "ORDER BY subSys ASC, typeObject ASC, idObject ASC";

		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, status);
	}

	/* (non-Javadoc) 4
	 * @see dao.IDAOObject#findAll(java.lang.String[], java.lang.String, java.lang.String, java.lang.String, enums.EnumObjectStatus)
	 */
	@Override
	public List<EntityObject> findAll(String[] options, String likeIdObject, String sys, String subSys, EnumObjectStatus status) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "";
		
		// Find object list by status
		FIND_ALL_WHERE = FIND_ALL_WHERE
				+ "WHERE " + "      sys = ? " + " AND subSys = ? " + " AND statusObject = ? "
				+ addSqlIN_LIKE(options, likeIdObject)
				+ "ORDER BY typeObject ASC, idObject ASC";

		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, status);
		
	}

	/* (non-Javadoc) 5
	 * @see dao.IDAOObject#findAll(java.lang.String[], java.lang.String, java.lang.String, enums.EnumObject, enums.EnumObjectStatus)
	 */
	@Override
	public List<EntityObject> findAll(String[] options, String likeIdObject, String sys, EnumObject typeObject, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "";

		// Find object list by type and status
		FIND_ALL_WHERE = FIND_ALL_WHERE
				+ "WHERE" + "     sys = ? " + " AND typeObject = ? "
				+ " AND statusObject = ? " 
				+ addSqlIN_LIKE(options, likeIdObject)
				+ "ORDER BY subSys ASC, idObject ASC, subSys ASC";

		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, typeObject, statusObject);
	}

	/* (non-Javadoc) 6
	 * @see dao.IDAOObject#findAll(java.lang.String[], java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumObjectStatus)
	 */
	@Override
	public List<EntityObject> findAll(String[] options, String likeIdObject, String sys, String subSys, EnumObject typeObject, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "";
		
	    FIND_ALL_WHERE = FIND_ALL_WHERE 
	    		+ "WHERE" + "     sys = ? " + " AND typeObject = ? " + " AND statusObject = ? "
	    		+ addSqlIN_LIKE(options, likeIdObject)
				+ "ORDER BY subSys ASC, idObject ASC";

		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, typeObject, statusObject);
	}

	/* (non-Javadoc) 7
	 * @see dao.IDAOObject#findAll(java.lang.String[], java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public List<EntityObject> findAll(String[] options, String likeIdObject, String sys, String subSys) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "";
		
		// Find object list by sys and subSys
		FIND_ALL_WHERE = FIND_ALL_WHERE 
				+ "WHERE" + "     sys = ? " + " AND subSys = ? "
				+ addSqlIN_LIKE(options, likeIdObject)
				+ "ORDER BY typeObject ASC, idObject ASC";

		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys);
	}

	/* (non-Javadoc) 8
	 * @see dao.IDAOObject#findAll(java.lang.String, java.lang.String[], enums.EnumObject)
	 */
	@Override
	public List<EntityObject> findAll(String likeIdObject, String[] options, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "";
		
		// Find object list by type
		FIND_ALL_WHERE = FIND_ALL_WHERE
				+ "WHERE" + "     typeObject = ? " 
				+ addSqlIN_LIKE(options, likeIdObject)
				+ "ORDER BY sys ASC, subSys ASC, idObject ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, typeObject);
	}

	/* (non-Javadoc) 9
	 * @see dao.IDAOObject#findAll(java.lang.String[], java.lang.String, java.lang.String)
	 */
	@Override
	public List<EntityObject> findAll(String[] options, String likeIdObject, String sys) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "";
		
		// Find object list by system
		FIND_ALL_WHERE = FIND_ALL_WHERE
				+ "WHERE" + "     sys = ? "
				+ addSqlIN_LIKE(options, likeIdObject)
		        + "ORDER BY subSys ASC, typeObject ASC, idObject ASC";
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys);
	}

	
	///////////////////////////////////////////////////////////////////////////////////////
	//////////////////// FURTHER NAMED METHODS ////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////
    
	/* Returns programs with names regardless subsystem
	 * @see dao.IDAOObject#findAll(java.lang.String[], java.lang.String, java.lang.String)
	 */
	public List<EntityObject> findProgramsByName(String sys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
		String FIND_ALL_WHERE = "";
	    FIND_ALL_WHERE = FIND_ALL_WHERE 
	    		+ "WHERE" + "     sys = ? " + " AND idObject = ? " + " AND typeObject = ? " 
				+ "ORDER BY subSys ASC";

		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idObject, typeObject);
	}
	
	
	
	///////////////////////////////////////////////////////////////////////////////////////
	//////////////////// SERVICE METHODS //////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////
    
	/* Attach INNER JOIN with ObjectOption */
	private String addSqlIN_LIKE (String[] options, String likeIdObject) {
		String s = "";
		String c = "";  // Comma
		
		// AND LIKE
		if (!likeIdObject.isBlank() && !likeIdObject.equals("%") && !likeIdObject.equals("*")) {
			s = s + " AND T1.idObject LIKE '" + likeIdObject.trim() + "' " ;
		}
		
		// AND EXISTS
		if (options != null && options.length > 0) {
			s = " AND EXISTS (SELECT * FROM objectOption T2"
				       + " WHERE T1.sys = T2.sys"
				       + " AND T1.subSys = T2.subSys"
				       + " AND T1.idObject = T2.idObject"
				       + " AND T1.typeObject = T2.typeObject"
				       + " AND T2.optionObject IN (";
			for (int i = 0; i < options.length; i++) {
				s = s + c + options[i];
				c=",";
			}
			s = s + " ) ) ";
		}		
		return s;
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
	public List<EntityObject> findAllWhere(String whereClause, String orderBy) throws ExceptionAmritaSqlError, SQLException {
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
	public List<EntityObject> readSetEntityParm(String where, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntityObject> readSetEntityWhere(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError {
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
	
	public List<EntityObject> execSqlGenericEntity(String strSql, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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