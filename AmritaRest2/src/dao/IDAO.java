/**
 * 
 */
package dao;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per generiche operazioni non CRUD.
 * Inserire qui tutte le ulteriori operazioni comuni a tutte le tabelle.
 */
public interface IDAO<T> extends IDAOCRUD<T> {
 		
	 /** Get the current connection 
	 * @throws SQLException */	 
	 public Connection getConn() throws SQLException;

	 /**
	 * @param conn the conn to set
	 */
	 public void setConn(Connection conn);
	 
	 /** Close the current connection 
	 * @throws SQLException */	 
	 public void releaseConn() throws SQLException;
	 
	 /** Get database status operation **/
	 public DataBaseStatusDetailed getDataBaseStatusOperation() ;
	 
	 /** set database status operation **/
	 public void setDataBaseStatusOperation(DataBaseStatusDetailed dbsd) ;
	 
	 /** set User Configuration **/
	 public void setUserConfiguration(UserConfiguration ucfg) ;
	 
	 /* Commit operations */
	 public void commit() throws SQLException;
	 
	 /** Get a set of entities with where clause and order by supplied (readSetEntityWhere) **/
	 public List<T> findAllWhere(String whereClause, String orderBy) throws SQLException, ExceptionAmritaSqlError;  ;
	 	 
	 /*
	 * Get all objects with 0 or more parameters for specific conditions with result an array list of objects
	 * Are supplied parameters that are actualized before execution
	 * The DAO implementor has specific methods with explicit parameters
	 * The Order by eventually is set by the DAO implementor
	 */
	 public List<T> readSetEntityParm(String where, Object ... parms) throws SQLException, ExceptionAmritaSqlError;  
	 
	/*
	 * Get all objects with 0 or more parameters for specific conditions with result the resultset
	 * Are supplied parameters that are actualized before execution
	 * The DAO implementor has specific methods with explicit parameters
	 * The Order by eventually is set by the DAO implementor
	 */
	public ResultSet readSetEntityParmRS(String where, String orderBy, Object ... parms) throws SQLException, ExceptionAmritaSqlError;
	 
	 
	/*
	 * Get all objects with specific where supplied with result an array list of objects
	 * The DAO implementor has specific methods preparing the where and calling this super method
	 * No replacing of parameters will be done
	 */
	public List<T> readSetEntityWhere(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError;

	/*
	 * Exec a complete generic Sql Statement, with a list of Entity Objects in output
	 * Can be any type of statement
	 * Can be used any type of DAO available, better to use MySQLDAOImplSqlGeneric
	 */
	public List<T> execSqlGenericEntity(String select, Object ... parms) throws SQLException, ExceptionAmritaSqlError;

	/*
	 * Exec a complete generic Sql Statement, with the resultset in output
	 * Can be any type of statement
	 * Can be used any type of DAO available, better to use MySQLDAOImplSqlGeneric
	 * No exception is produced and has to be managed by the caller querying the dbsd object
	 */
	public ResultSet execSqlGeneric(String sqlString) throws SQLException, ExceptionAmrita;

}