/**
 * 
 */
package dao;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import entities.EntitySqlGeneric;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per generiche operazioni non CRUD.
 * Inserire qui tutte le ulteriori operazioni comuni a tutte le tabelle.
 */
public interface IDAOSqlGeneric<T> extends IDAO<EntitySqlGeneric> {
 		
	 /** Get the current connection 
	 * @throws SQLException */	 
	 public Connection getConn() throws SQLException;
		
	 /** Close the current connection 
	 * @throws SQLException */	 
	 public void releaseConn() throws SQLException;
		

	 /* 1 Select for CRUD Matrix to get all Entities/Progrrams/CRUD
	 * @throws SQLException 
	 * @throws ExceptionAmritaSqlError */	 
	 public List<EntitySqlGeneric> sqlCrudMatrix(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError;
		
	 
	 /* 2 Select for CRUD Matrix to get all Entities/Progrrams/CRUD
	 * @throws SQLException */	 
	 public List<EntitySqlGeneric> sqlCrudMatrix(String sys, String subSys, String idPgmFrom, String idPgmTo, String idEntityFrom, String idEntityTo) throws SQLException , ExceptionAmritaSqlError;
		
	 /* 3 Select for CRUD Matrix to get all Entities/Progrrams/CRUD
	 * @throws SQLException */	 
	 public List<EntitySqlGeneric> sqlCrudMatrixByPgm(String sys, String subSys, String idPgmFrom, String idPgmTo) throws SQLException, ExceptionAmritaSqlError;

	 /* 4 Select for CRUD Matrix to get all Entities/Progrrams/CRUD
	 * @throws SQLException */	 
	 public List<EntitySqlGeneric> sqlCrudMatrixByEntity(String sys, String subSys, String idEntityFrom, String idEntityTo) throws SQLException, ExceptionAmritaSqlError;
	 
	 /* 5 Select for TreeView to get all object relations grouped for an object
	 * @throws SQLException */	 
	 public List<EntitySqlGeneric> sqlTreeViewRelations(String sys, String subSys, int typeObject, String idObject, boolean isDirect) throws SQLException, ExceptionAmritaSqlError;
	 
	 /* 6 Select for to get object owner
	 * @throws SQLException */	 
	 public List<EntitySqlGeneric> sqlSubSysOwner(String sys, String subSys, int typeObject, String idObject) throws SQLException, ExceptionAmritaSqlError;
	 
	 /* 7 Select for getting sub systems owner used by a subsystem
	 * @throws SQLException */	 
	 public List<EntitySqlGeneric> sqlSubSysOwnerUsed(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError;
	 
	 /* 8 Select for getting sub systems shared
	  * 
	 * @throws SQLException */	 
	 public List<EntitySqlGeneric> sqlTypeObjectsShared(String sys, String subSysOwner, String subSys) throws SQLException, ExceptionAmritaSqlError;
	 
	 /* 9 Select for getting objects shared between the subsystem owner and another subsystem
	 * @throws SQLException */	 
	 public List<EntitySqlGeneric> sqlObjectsShared(String sys, String subSysOwner, String subSys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError;

	 /* 10 Select for getting all ImpactPlan codes
	 * @throws SQLException */	 
	 public List<EntitySqlGeneric> sqlImpactPlan(String sys) throws SQLException, ExceptionAmritaSqlError;
	 
	 /* 11) Select count dei programmi interessati da un impact plan
	  *  Si leggono i where used di uno specifico campo di un determinato copy/entity 
	  * @throws SQLException 
	  * */	 
	 public List<EntitySqlGeneric> sqlImpacPlanPgmCount(String sys, String idPlan, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError;
	 
	 /* 12) Select l'elenco dei programmi interessati da una operazione di un impact plan
	  *  Si leggono i where used di uno specifico campo di un determinato copy/entity 
	  * @throws SQLException 
	  * */	 
	 public List<EntitySqlGeneric> sqlImpactPlanPgmList(String sys, String idPlan, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError;
	 
	 /* 13) Select Count Detail all where used o a specific field
	  *  Si leggono i where used di uno specifico campo di un determinato copy/entity 
	  * @throws SQLException 
	  * */	 
	 public List<EntitySqlGeneric> sqlImpacPlanWhereUsedCount(String sys, String idPlan, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError;
	 
	 /* 14) Select objects owned by a subsystem of a specific type
	  *  Si leggono i where used di uno specifico campo di un determinato copy/entity 
	  * @throws SQLException 
	  * */	 
	 public List<EntitySqlGeneric> sqlObjectsOwned(String sys, String subSys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError;
	 
	 /* 15) Get all configuration id from MetricViolationConfig
	  *  
	  * @throws SQLException 
	  * */	 
	 public List<EntitySqlGeneric> sqlQualityConfig(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError;
 
	 

}