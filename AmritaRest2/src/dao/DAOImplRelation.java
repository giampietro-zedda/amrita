package dao;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import entities.EntityRelation;
import enums.EnumObject;
import enums.EnumRelation;
import enums.EnumSourceType;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;


/**
 * Implementazione dell'interfaccia ObjectDAO per il database MySQL.
 */
public class DAOImplRelation extends DAOAbstract<EntityRelation> implements IDAORelation{
	
	/*
	 * Constructor  
	 */
	public DAOImplRelation(boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(isConnToClose, isAutocommit);
		super.typeClass = EntityRelation.class;
		super.ucfg = ucfg;
	}

	/*
	 * Constructor  
	 */
	public DAOImplRelation(Connection conn, boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(conn, isConnToClose, isAutocommit);
		super.typeClass = EntityRelation.class;
		super.ucfg = ucfg;
	}

 
	/*
	 * ===============================================================================================================
	 * ================ Specific methods implemented described by the interface IDAORelation    ======================
	 * ===============================================================================================================
	 */


	/* (non-Javadoc) 1
	 * @see dao.IDAORelation#findAllByIdARel(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, enums.EnumRelation)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdARel(String sys, String subSys, String idObjectA, EnumObject typeObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError {

		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObjectA = ? "  
				+"  AND typeObjectA = ? "  				
				+"  AND relation = ? "  
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectA,  typeObjectA, relation);		
	}

	/* (non-Javadoc) 2
	 * @see dao.IDAORelation#findAllByIdBRel(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, enums.EnumRelation)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdBRel(String sys, String subSys, String idObjectB, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObjectB = ? "    				
				+"  AND relation = ? "  
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectA ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectB, relation);		
	}

	/* (non-Javadoc) 3
	 * @see dao.IDAORelation#findAllByIdAIdBRel(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, java.lang.String, enums.EnumObject, enums.EnumSourceType, enums.EnumRelation)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdAIdBRel(String sys, String subSys, String idObjectA, EnumObject typeObjectA,
			EnumSourceType typeSourceA, String idObjectB, EnumObject typeObjectB, EnumSourceType typeSourceB,
			EnumRelation relation) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObjectA = ? "  
				+"  AND typeObjectA = ? "  				
				+"  AND idObjectB = ? "  
				+"  AND typeObjectB = ? "  				
				+"  AND relation = ? "  
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectA,  typeObjectA, idObjectB,  typeObjectB, relation);		
	}

	/* (non-Javadoc) 4
	 * @see dao.IDAORelation#findAllByIdA(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdA(String sys, String subSys, String idObjectA, EnumObject typeObjectA,
			EnumSourceType typeSourceA) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObjectA = ? "  
				+"  AND typeObjectA = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY relation ASC, idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectA,  typeObjectA);		
	}

	/* (non-Javadoc) 5
	 * @see dao.IDAORelation#findAllByIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdB(String sys, String subSys, String idObjectB, EnumObject typeObjectB,
			EnumSourceType typeSourceB) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObjectB = ? "  
				+"  AND typeObjectB = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY relation ASC, idObjectA ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectB,  typeObjectB);			
	}

	/* (non-Javadoc) 6
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdAIdB(String sys, String subSys, String idObjectA, EnumObject typeObjectA,
			EnumSourceType typeSourceA, String idObjectB, EnumObject typeObjectB, EnumSourceType typeSourceB)
			throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObjectA = ? "  
				+"  AND typeObjectA = ? "  				
				+"  AND idObjectB = ? "  
				+"  AND typeObjectB = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY relation ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectA,  typeObjectA, idObjectB,  typeObjectB);			
	}
	
	/* (non-Javadoc) 7
	 * @see dao.IDAORelation#findAllByIdA(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllByIdA(String sys, String idObjectA, EnumObject typeObjectA) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND idObjectA = ? "  
				+"  AND typeObjectA = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY relation ASC, idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idObjectA,  typeObjectA);		
	}

	/* (non-Javadoc) 8
	 * @see dao.IDAORelation#findAllByIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllByIdB(String sys, String idObjectB, EnumObject typeObjectB,
			EnumSourceType typeSourceB) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND idObjectB = ? "  
				+"  AND typeObjectB = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY relation ASC, idObjectA ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idObjectB,  typeObjectB);			
	}

	/* (non-Javadoc) 9
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumRelation, java.lang.String)
	 */
	@Override
	public List<EntityRelation> findAllByIdAIdB(String sys, String subSys, String idObjectA, EnumObject typeObjectA,
			EnumRelation relation, String idObjectB) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "  
				+"  AND idObjectA = ? "  
				+"  AND typeObjectA = ? "  				
				+"  AND relation = ? "  
				+"  AND idObjectB = ? "  
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectA,  typeObjectA, relation, idObjectB );			
	}

	/* (non-Javadoc) 10
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumRelation, java.lang.String, enums.EnumObject)
	 */
	@Override
	public List<EntityRelation> findAllByIdAIdB(String sys, String subSys, EnumRelation relation, EnumObject typeObjectB)
			throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "  
				+"  AND relation = ? "  
				+"  AND typeObjectB = ? "  				
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, relation, typeObjectB );			
	}

	/* (non-Javadoc) 11
	 * @see dao.IDAORelation#findAllByIdA(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumRelation, enums.EnumObject)
	 */
	@Override
	public List<EntityRelation> findAllByIdA(String sys, String subSys, String idObjectA, EnumObject typeObjectA,
			EnumRelation relation, EnumObject typeObjectB) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "  
				+"  AND idObjectA = ? "  
				+"  AND typeObjectA = ? "  				
				+"  AND relation = ? "  
				+"  AND typeObjectB = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectA,  typeObjectA, relation, typeObjectB );			
	}


	/* (non-Javadoc) 12) Get all relations of a specific type regardless subSys, id and type
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumRelation, java.lang.String, enums.EnumObject)
	 */
	@Override
	public List<EntityRelation> findAll(String sys, EnumRelation relation)
			throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND relation = ? "  
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, relation );			
	}

	/* (non-Javadoc) 13
	 * @see dao.IDAORelation#findAllByIdBRel(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, enums.EnumRelation)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdBRel(String sys, String idObjectB, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND idObjectB = ? "    				
				+"  AND relation = ? "  
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectA ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idObjectB, relation);		
	}

	/* (non-Javadoc) 14
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdAIdB(String sys, String idObjectA, EnumObject typeObjectA,
			EnumSourceType typeSourceA, String idObjectB, EnumObject typeObjectB, EnumSourceType typeSourceB)
			throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND idObjectA = ? "  
				+"  AND typeObjectA = ? "  				
				+"  AND idObjectB = ? "  
				+"  AND typeObjectB = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY relation ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idObjectA,  typeObjectA, idObjectB,  typeObjectB);			
	}

	/* (non-Javadoc) 15
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllByIdARel(String sys, String idObjectA, EnumObject typeObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND idObjectA = ? "  
				+"  AND typeObjectA = ? "  				  
				+"  AND relation = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idObjectA,  typeObjectA, relation);			
	}
	
	/* (non-Javadoc) 16
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllByIdARel(String sys, String idObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND idObjectA = ? "    				
				+"  AND relation = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, idObjectA, relation);			
	}

	/* (non-Javadoc) 17
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdA(String sys, String subSys, String idObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "  
				+"  AND idObjectA = ? "    				
				+"  AND relation = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectA, relation);			
	}

	/* (non-Javadoc) 18
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysRel(String sys, String subSys, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "   				
				+"  AND relation = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, relation);			
	}

	/* (non-Javadoc) 19
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllByRel(String sys, EnumObject typeObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND typeObjectA = ? "   				
				+"  AND relation = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, typeObjectA, relation);			
	}

	/* (non-Javadoc) 20
	 * @see dao.IDAORelation#findAllByIdAIdB(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllByRel(String sys, String subSys, EnumObject typeObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "   	
				+"  AND typeObjectA = ? "   				
				+"  AND relation = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, typeObjectA, relation);			
	}

	/* (non-Javadoc) 21
	 * @see dao.IDAORelation#findAllByIdA(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysTypeA(String sys, String subSys, EnumObject typeObjectA) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND typeObjectA = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY relation ASC, idObjectA ASC, idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, typeObjectA);		
	}

	/* (non-Javadoc) 22
	 * @see dao.IDAORelation#findAllByIdA(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdA(String sys, String subSys, String idObjectA) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObjectA = ? "  				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY relation ASC, idObjectB ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectA);		
	}

	/* (non-Javadoc) 23
	 * @see dao.IDAORelation#findAllByIdBRel(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, enums.EnumRelation)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdARelIdB(String sys, String subSys, String idObjectA, EnumRelation relation, String idObjectB) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObjectA = ? "    				
				+"  AND relation = ? "  
				+"  AND idObjectB = ? "    				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectA ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectA, relation, idObjectB);		
	}
	
	/* (non-Javadoc) 24
	 * @see dao.IDAORelation#findAllByIdBRel(java.lang.String, java.lang.String, java.lang.String, enums.EnumObject, enums.EnumSourceType, enums.EnumRelation)
	 */
	@Override
	public List<EntityRelation> findAllBySubSysIdATypeATypeB(String sys, String subSys, String idObjectA, EnumObject typeObjectA, EnumObject typeObjectB) throws SQLException, ExceptionAmritaSqlError {
		
		String FIND_ALL_WHERE = ""
				+"WHERE "	
				+"      sys = ? "
				+"  AND subSys = ? "	
				+"  AND idObjectA = ? "    				
				+"  AND typeObjectA = ? "  
				+"  AND typeObjectB = ? "    				
				;
	     FIND_ALL_WHERE = FIND_ALL_WHERE
				+"ORDER BY idObjectA ASC, relation ASC"
				;
		return super.readSetEntityParmAbstract(FIND_ALL_WHERE, sys, subSys, idObjectA, typeObjectA, typeObjectB);		
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
	public List<EntityRelation> findAllWhere(String whereClause, String orderBy) throws ExceptionAmritaSqlError, SQLException {
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
	public List<EntityRelation> readSetEntityParm(String where, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntityRelation> readSetEntityWhere(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntityRelation> execSqlGenericEntity(String strSql, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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

/*
 * 
 * Specific methods
 * 
 */



