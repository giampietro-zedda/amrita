package dao;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import entities.EntitySqlGeneric;
import enums.EnumObject;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;


/**
 * Implementazione dell'interfaccia x gwnwrico SQL per il database MySQL.
 * @param <T>
 */
public class DAOImplSqlGeneric extends DAOAbstract<EntitySqlGeneric> implements IDAOSqlGeneric<EntitySqlGeneric>{
	
	/*
	 * Constructor 
	 */
	public DAOImplSqlGeneric(boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(isConnToClose, isAutocommit);
		super.typeClass = EntitySqlGeneric.class;
		super.ucfg = ucfg;
	}

	/*
	 * Constructor 
	 */
	public DAOImplSqlGeneric(Connection conn, boolean isConnToClose, boolean isAutocommit, UserConfiguration ucfg) {
		super(conn, isConnToClose, isAutocommit);
		super.typeClass = EntitySqlGeneric.class;
		super.ucfg = ucfg;
	}

	
	 /* 1 Select for CRUD Matrix to get all Entities/Progrrams/CRUD */
	@Override
	public List<EntitySqlGeneric> sqlCrudMatrix(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError {
	   
		String SELECT  = "SELECT sys, subSys, idObjectA AS pgm, relation, idObjectB AS entity  "	
				+" FROM  relation "
				+" WHERE "
				+"       sys ='"     + sys + "'" 
				+"   AND subSys = '" + subSys + "'"
				+ "  AND typeObjectA = '1' "             // OBJECT_PGM_COBOL
				+"   AND typeObjectB = '15'"             // OBJECT_ENTITY_SQL
				+"   AND relation  IN (11, 14, 15, 16)"  // PGM_ENTITY_READ, PGM_ENTITY_UPDATE, PGM_ENTITY_DELETE, PGM_ENTITY_INSERT
				+" GROUP BY idObjectA, relation, idObjectB"
				+" ORDER BY idObjectA, idObjectB, relation"
				;
	
		return super.execSqlGenericEntityAbstract(SELECT);
	}
	

	 /** 2 Select for CRUD Matrix to get all Entities/Progrrams/CRUD 
	 * @throws ExceptionAmritaSqlError */	
	@Override
	public List<EntitySqlGeneric> sqlCrudMatrix(String sys, String subSys, String idPgmFrom, String idPgmTo, String idEntityFrom, String idEntityTo) throws SQLException, ExceptionAmritaSqlError {
 
		String SELECT  = "SELECT sys, subSys, idObjectA AS pgm, relation, idObjectB AS entity  "	
				+" FROM  relation "
				+" WHERE "
				+"       sys ='"     + sys + "'" 
				+"   AND subSys = '" + subSys + "'"
				+ "  AND typeObjectA = '1' "             // OBJECT_PGM_COBOL
				+"   AND typeObjectB = '15'"             // OBJECT_ENTITY_SQL
				+"   AND relation  IN (11, 14, 15, 16)"  // PGM_ENTITY_READ, PGM_ENTITY_UPDATE, PGM_ENTITY_DELETE, PGM_ENTITY_INSERT
				+"   AND idObjectA BETWEEN '" + idPgmFrom    + "' AND '" + idPgmTo    + "'"
				+"   AND idObjectB BETWEEN '" + idEntityFrom + "' AND '" + idEntityTo + "'"
				+" GROUP BY idObjectA, relation, idObjectB"
				+" ORDER BY idObjectA, idObjectB, relation"
				;
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	 /** 3 Select for CRUD Matrix to get all Entities/Progrrams/CRUD 
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlCrudMatrixByPgm(String sys, String subSys, String idPgmFrom, String idPgmTo) throws SQLException, ExceptionAmritaSqlError {
 
		String SELECT  = "SELECT sys, subSys, idObjectA AS pgm, relation, idObjectB AS entity  "	
				+" FROM  relation "
				+" WHERE "
				+"       sys ='"     + sys + "'" 
				+"   AND subSys = '" + subSys + "'"
				+ "  AND typeObjectA = '1' "             // OBJECT_PGM_COBOL
				+"   AND typeObjectB = '15'"             // OBJECT_ENTITY_SQL
				+"   AND relation  IN (11, 14, 15, 16)"  // PGM_ENTITY_READ, PGM_ENTITY_UPDATE, PGM_ENTITY_DELETE, PGM_ENTITY_INSERT
				+"   AND idObjectA BETWEEN '" + idPgmFrom    + "' AND '" + idPgmTo    + "'"
				+" GROUP BY idObjectA, relation, idObjectB"
				+" ORDER BY idObjectA, idObjectB, relation"
				;
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	 /** 4 Select for CRUD Matrix to get all Entities/Progrrams/CRUD 
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlCrudMatrixByEntity(String sys, String subSys, String idEntityFrom, String idEntityTo) throws SQLException, ExceptionAmritaSqlError {
 
		String SELECT  = "SELECT sys, subSys, idObjectA AS pgm, relation, idObjectB AS entity  "	
				+" FROM  relation "
				+" WHERE "
				+"       sys ='"     + sys + "'" 
				+"   AND subSys = '" + subSys + "'"
				+ "  AND typeObjectA = '1' "             // OBJECT_PGM_COBOL
				+"   AND typeObjectB = '15'"             // OBJECT_ENTITY_SQL
				+"   AND relation  IN (11, 14, 15, 16)"  // PGM_ENTITY_READ, PGM_ENTITY_UPDATE, PGM_ENTITY_DELETE, PGM_ENTITY_INSERT
				+"   AND idObjectB BETWEEN '" + idEntityFrom + "' AND '" + idEntityTo + "'"
				+" GROUP BY idObjectA, relation, idObjectB"
				+" ORDER BY idObjectA, idObjectB, relation"
				;
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	 /** 5 Select for TreeView to get all object relations grouped for an object
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlTreeViewRelations(String sys, String subSys, int typeObject, String idObject, boolean isDirect) throws SQLException, ExceptionAmritaSqlError {
		String SELECT = "";
		
		if (isDirect) {
		   SELECT   = "SELECT relation, typeObjectA, typeObjectB "	
					+ " FROM  relation "
					+ " WHERE "
					+ "       sys ='"     + sys + "'" 
					+ "   AND subSys = '" + subSys + "'"
					+ "   AND typeObjectA = "  +       typeObject             
					+ "   AND idObjectA   = "  + "'" + idObject  + "'"           
					+ " GROUP BY relation, typeObjectA, typeObjectB"
					+ " ORDER BY relation"
					;
		} else {
			   SELECT   = "SELECT relation, typeObjectB AS typeObjectA, typeObjectA AS typeObjectB "	
						+ " FROM  relation "
						+ " WHERE "
						+ "       sys ='"     + sys + "'" 
						+ "   AND subSys = '" + subSys + "'"
						+ "   AND typeObjectB = "  +       typeObject             
						+ "   AND idObjectB   = "  + "'" + idObject  + "'"           
						+ " GROUP BY relation, typeObjectA, typeObjectB"
						+ " ORDER BY relation"
						;
		}
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	 /** 6 Select for getting subSysOwner
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlSubSysOwner(String sys, String subSys, int typeObject, String idObject) throws SQLException, ExceptionAmritaSqlError {

		String SELECT   = "SELECT subSysOwner, statusObject "	
						+ " FROM  object "
						+ " WHERE "
						+ "       sys ='"     + sys + "'" 
						+ "   AND subSys = '" + subSys + "'"
						+ "   AND typeObject = "  +       typeObject             
						+ "   AND idObject   = "  + "'" + idObject  + "'"           
						;
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	
	 /** 7 Select for getting sub systems shared
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlSubSysOwnerUsed(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError {
       
		String SELECT   = "SELECT subSys AS subSystem, idObjectDescriptor "	
						+ " FROM  object "
						+ " WHERE "
						+ "       sys ='"     + sys + "'" 
						+ "   AND subSysOwner = '" + subSys + "'"
						+ "   AND subSys <> subSysOwner "
						+ "   AND typeObject <> 88 "                     // OBJECT_LIBRARY
						+ " GROUP BY subSys"
						;
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	 /** 8 Select for getting sub systems shared
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlTypeObjectsShared(String sys, String subSysOwner, String subSys) throws SQLException, ExceptionAmritaSqlError {

		String SELECT   = "SELECT typeObject AS typeObjectA"	
						+ " FROM  object "
						+ " WHERE "
						+ "       sys ='"     + sys + "'" 
						+ "   AND subSys = '" + subSys + "'"
						+ "   AND subSysOwner = '" + subSysOwner + "'"
						+ " GROUP BY typeObjectA"
						;
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	 /** 9 Select for getting objects shared between the subsystem owner and another subsystem
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlObjectsShared(String sys, String subSysOwner, String subSys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {
 
		String SELECT   = "SELECT idObject, statusObject, typeObject AS typeObjectA"	
				+ " FROM  object "
				+ " WHERE "
				+ "       sys ='"     + sys + "'" 
				+ "   AND subSys = '" + subSys + "'"
				+ "   AND subSysOwner = '" + subSysOwner + "'"
				+ "   AND typeObject = '" + typeObject.ordinal() + "'"
				;
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	 /** 10 Select for getting objects shared between the subsystem owner and another subsystem
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlImpactPlan(String sys) throws SQLException, ExceptionAmritaSqlError {

		String SELECT   = "SELECT idPlan AS idObject"	
				+ " FROM  impactPlan "
				+ " WHERE "
				+ "       sys ='"     + sys + "'" 
				+ "   GROUP BY idPlan"
				+ "   ORDER BY idPlan"
				;
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	 /** 11 Select count dei programmi interessati da uno specifico where used (idPlan NON utilizzato al momento)
	  *     Si leggono i where used di uno specifico campo di un determinato copy/entity 
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlImpacPlanPgmCount(String sys, String idPlan, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError {

		String SELECT   = "SELECT COUNT(*) AS cnt FROM ("	
													   + " SELECT DISTINCT idObjectRefer "
													   + "   FROM whereused "
													   + "  WHERE sys        = '"  + sys + "'"
													   + "    AND idObject   = '"  + idObject + "'" 
													   + "    AND typeObject =  "  + typeObject.ordinal() 
													   + "    AND idField    = '"  + idField + "'"
											        + ") AS B " 
		;
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	
	 /** 12 Select l'elenco dei programmi interessati da uno specifico campo, senza il dettaglio di riga/istruzione
	  *     Si leggono i where used di uno specifico campo di un determinato copy/entity 
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlImpactPlanPgmList(String sys, String idPlan, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError {

		String SELECT   = "SELECT DISTINCT subSys AS subSystem, idObjectRefer AS pgm, typeObjectRefer AS typeObjectA"	
					    + "  FROM whereUsed "
					    + " WHERE sys        = '"  + sys + "'"
					    + "   AND idObject   = '"  + idObject + "'" 
					    + "   AND typeObject =  "  + typeObject.ordinal()  
					    + "   AND idField    = '"  + idField + "'"
					    + " ORDER BY idObjectRefer "
		;
		return super.execSqlGenericEntityAbstract(SELECT);
	}

	 /** 13 Select count del dettaglio di where used di un campo di un copy/entity
	  *     Si leggono i where used di uno specifico campo di un determinato copy/entity 
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlImpacPlanWhereUsedCount(String sys, String idPlan, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError {

		String SELECT   = "SELECT COUNT(*) AS cnt FROM ("	
													   + " SELECT DISTINCT * "
													   + "   FROM whereUsed "
													   + "  WHERE sys        = '"  + sys + "'"
													   + "    AND idObject   = '"  + idObject + "'" 
													   + "    AND typeObject =  "  + typeObject.ordinal()  
													   + "    AND idField    = '"  + idField + "'"
											        + ") AS B " 
		;
		return super.execSqlGenericEntityAbstract(SELECT);
	}


	 /** 14) Select objects owned by a subsystem of a specific type
	  *  Si leggono i where used di uno specifico campo di un determinato copy/entity 
	  *  
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlObjectsOwned(String sys, String subSys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError {

		String SELECT   = "SELECT idObject, statusObject"	
						+ " FROM  object "
						+ " WHERE "
						+ "       sys ='"     + sys + "'" 
						+ "   AND subSys = '" + subSys + "'"
						+ "   AND typeObject = " + typeObject.ordinal() 
						+ "   AND subSysOwner = '" + subSys + "'"
						+ " ORDER BY idObject"
						;
		return super.execSqlGenericEntityAbstract(SELECT);
		}

	/** Get all configuration id from MetricViolationConfig
	 * @throws ExceptionAmritaSqlError */
	@Override
	public List<EntitySqlGeneric> sqlQualityConfig(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError {

		String SELECT  = "SELECT configName AS idObject  "	
				+" FROM  metricViolationConfig "
				+" WHERE "
				+"       sys ='"     + sys + "'" 
				+"   AND subSys = '" + subSys + "'"
				+" GROUP BY configName "
				+" ORDER BY configName "
				;
		return super.execSqlGenericEntityAbstract(SELECT);
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
	public List<EntitySqlGeneric> findAllWhere(String whereClause, String orderBy) throws ExceptionAmritaSqlError, SQLException {
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
	public List<EntitySqlGeneric> readSetEntityParm(String where, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntitySqlGeneric> readSetEntityWhere(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError {
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
	public List<EntitySqlGeneric> execSqlGenericEntity(String strSql, Object... parms) throws SQLException, ExceptionAmritaSqlError {
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