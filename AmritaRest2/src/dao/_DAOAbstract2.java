package dao;
import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import analyzer.AmritaStartup;
import analyzer.DataBaseConnections;
import analyzer.DataBaseItemDescriptor;
import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;
import enums.EnumAmritaExceptionError;
import enums.EnumDataBaseOperation;
import enums.EnumDataBaseOperationStatus;
import enums.EnumMessageType;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;
import utilities.ReflectionManager;


/**
 * Generic abstract class for DAO implementation with common code
 * for CRUD operations. 
 * Concrete DAO classes must implement and override all public variables and methods
 */
public abstract class _DAOAbstract2<T> implements IDAO<T> {

	Class<T> typeClass = null;                 // To get instance using T generics    
	Connection conn = null;                    // Connection from Factory
	boolean isConnToRelease = true;            // Connection closed after operation
	boolean isAutocommit = true;               // Autocommit enabled for the connection 	
	DataBaseStatusDetailed dbsd = null;        // Descrittore operazione
    UserConfiguration ucfg = null;             //	
	
	/*
	 * Constructor  no conn
	 */
	public _DAOAbstract2(boolean isConnToRelease, boolean isAutocommit) {
		this.isConnToRelease = isConnToRelease;
		this.isAutocommit = isAutocommit;
		this.dbsd = new DataBaseStatusDetailed();
	}

	/*
	 * Constructor  yes conn
	 */
	public _DAOAbstract2(Connection conn, boolean isConnToRelease, boolean isAutocommit) {
		this.conn = conn;
		this.isConnToRelease = isConnToRelease;
		this.isAutocommit = isAutocommit;
		this.dbsd = new DataBaseStatusDetailed();
	}


	/*
	 *  Create object
	 */
	public final boolean create(T object) throws SQLException, ExceptionAmrita {
		PreparedStatement preparedStatement = null;
		List<DataBaseItemDescriptor> lsf = null;
		String CREATE_QUERY = "";
		String entityName = "";
		boolean ret = false;
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_CRUD_CREATE);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();
		}
		this.conn.setAutoCommit(false);		
		
		try {
			entityName=this.typeClass.getSimpleName().substring(6);
			lsf = getEntityFields(object);
			CREATE_QUERY=createInsertForPrepare(lsf);
 			preparedStatement = conn.prepareStatement(CREATE_QUERY);
            setPreparedStatement(1, false, false, lsf, preparedStatement);  // All columns Same order as defined
            this.dbsd.setSqlString(preparedStatement.toString());
            preparedStatement.executeUpdate();
			ret = true;
			if (preparedStatement.getWarnings() != null) {
				dbsd.setWarningMessage(preparedStatement.getWarnings().toString());
			}
			logSqlInstructionInfo(dbsd);

		} catch (SQLException e) {
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			setErrorDetails(dbsd, e, entityName);
			logSqlInstructionInfo(dbsd);
			// Sql Error
			if (e.getErrorCode() != 1062) {    // MYSQL duplicate code
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
				setErrorDetails(this.dbsd, e, entityName);
				logSqlInstructionInfo(this.dbsd);
			    throw e;
			}
			// Duplicate primary key
			this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_DUPLICATE);
			
		} finally {
			try {
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);
				preparedStatement.close();
				if (ret == true && isAutocommit) {
					this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_COMMIT);
					conn.commit();
				}	
				DataBaseConnections.releaseConnection(conn);
			} catch (SQLException e) {
				setErrorDetails(this.dbsd, e, entityName);
				logSqlInstructionInfo(this.dbsd);
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
				throw e;

			}
		}	
		if (isConnToRelease) {
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
		}
		return ret;
	}

	/*  
	 * Create multiple inserts general implementation with single execute
	 */
	public final int[] createBulk2(List<T> lo) throws ExceptionAmrita, SQLException {
		PreparedStatement preparedStatement = null;
		List<DataBaseItemDescriptor> lsf = null;
		int[] arRetCount = new int[0];
		String CREATE_QUERY = "";
		String entityName = "";
		String values = "";
		int i = 0;
		int iParm = 0;
		boolean ret = false;
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_CRUD_CREATE_BULK);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
		
        if (lo.size() == 0) {
			return arRetCount;
		}
        
		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();
		}
		this.conn.setAutoCommit(false);		
	    
	    try {
			entityName=this.typeClass.getSimpleName().substring(6);
			lsf = getEntityFields(lo.get(0));                                   // To get Insert to prepare        
			CREATE_QUERY=createInsertForPrepare(lsf);   						// INSERT INTO table field1,.. VALUES (?, ?...)
			i = CREATE_QUERY.indexOf("VALUES");
			values=CREATE_QUERY.substring(i+6);
			StringBuffer sql = new StringBuffer(CREATE_QUERY);
			for(i=1;i<lo.size();i++) {
			    sql.append("," + values);   									
			}
			preparedStatement = conn.prepareStatement(sql.toString());          // INSERT INTO table field1,.. VALUES (?, ?...),(?, ?...),(...
			iParm = 1;
			for (T objToCreate : lo) {
    			updEntityFieldsValue(objToCreate, lsf);                         			// Update column value by getField.. via reflection
	 			iParm=setPreparedStatement(iParm, false, false, lsf, preparedStatement); 	 // All columns Same order as defined
			}
			preparedStatement.execute();
			if (preparedStatement.getWarnings() != null) {
				dbsd.setWarningMessage(preparedStatement.getWarnings().toString());
			}
			logSqlInstructionInfo(dbsd);

		} catch (SQLException e) {
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			setErrorDetails(dbsd, e, entityName);
			logSqlInstructionInfo(dbsd);
			// Sql Error, no duplicate allowed (Sql vendor Code 1062)
			this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			if (e.getErrorCode() == 1062) {    // MYSQL duplicate code
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_DUPLICATE);
			}
			setErrorDetails(this.dbsd, e, entityName);
			logSqlInstructionInfo(this.dbsd);
		    throw e;			
			
		} finally {
			try {
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);
				if (preparedStatement != null) {
					preparedStatement.close();
				}
				if (ret == true && isAutocommit) {
					this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_COMMIT);
					conn.commit();
				}	
				DataBaseConnections.releaseConnection(conn);
			} catch (SQLException e) {
				setErrorDetails(this.dbsd, e, entityName);
				logSqlInstructionInfo(this.dbsd);
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
				throw e;

			}
		}	
		if (isConnToRelease) {
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
		}
		return arRetCount;	
	}

	/*  
	 * Create multiple inserts general implementation (original version with addBatch()
	 */
	public final int[] createBulk(List<T> lo) throws ExceptionAmrita, SQLException {
		PreparedStatement preparedStatement = null;
		List<DataBaseItemDescriptor> lsf = null;
		int[] arRetCount = new int[0];
		String CREATE_QUERY = "";
		String entityName = "";
		boolean ret = false;
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_CRUD_CREATE_BULK);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
		
        if (lo.size() == 0) {
			return arRetCount;
		}
        
		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();
		}
		this.conn.setAutoCommit(false);		
	
		try {
			entityName=this.typeClass.getSimpleName().substring(6);
			lsf = getEntityFields(lo.get(0));                                   // To get Insert to prepare        
			CREATE_QUERY=createInsertForPrepare(lsf);   						// INSERT INTO table field1,.. VALUES (?, ?...)
 			preparedStatement = conn.prepareStatement(CREATE_QUERY);
			for (T objToCreate : lo) {
    			updEntityFieldsValue(objToCreate, lsf);                         // Update column value by getField.. via reflection
	 			setPreparedStatement(1, false, false, lsf, preparedStatement);  // All columns Same order as defined
	            this.dbsd.setSqlString(preparedStatement.toString());
	            preparedStatement.addBatch();
			}
		    arRetCount = preparedStatement.executeBatch();
			if (preparedStatement.getWarnings() != null) {
				dbsd.setWarningMessage(preparedStatement.getWarnings().toString());
			}
			logSqlInstructionInfo(dbsd);

		} catch (SQLException e) {
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			setErrorDetails(dbsd, e, entityName);
			logSqlInstructionInfo(dbsd);
			// Sql Error, no duplicate allowed (Sql vendor Code 1062)
			this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			if (e.getErrorCode() == 1062) {    // MYSQL duplicate code
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_DUPLICATE);
			}
			setErrorDetails(this.dbsd, e, entityName);
			logSqlInstructionInfo(this.dbsd);
		    throw e;			
			
		} finally {
			try {
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);
				if (preparedStatement != null) {
					preparedStatement.close();
				}
				if (ret == true && isAutocommit) {
					this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_COMMIT);
					conn.commit();
				}	
				DataBaseConnections.releaseConnection(conn);
			} catch (SQLException e) {
				setErrorDetails(this.dbsd, e, entityName);
				logSqlInstructionInfo(this.dbsd);
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
				throw e;

			}
		}	
		if (isConnToRelease) {
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
		}
		return arRetCount;	
	}
	/*
	 *  Read Object by key
	 */
	public final boolean read(T object) throws SQLException, ExceptionAmrita {

		PreparedStatement preparedStatement = null;
		List<DataBaseItemDescriptor> lsf = null;
		ResultSet result = null;
		String READ_QUERY = "";
		String entityName = "";
		boolean ret = false;
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_CRUD_READ);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();
		}
		
		try {
			entityName=this.typeClass.getSimpleName().substring(6);
			lsf = getEntityFields(object);
			READ_QUERY=createSelectForPrepare(lsf);
			preparedStatement = conn.prepareStatement(READ_QUERY);
			setPreparedStatement(1, true, false, lsf, preparedStatement);  // All PK columns Same order as defined
			this.dbsd.setSqlString(preparedStatement.toString());
			preparedStatement.execute();
			result = preparedStatement.getResultSet();
			
			if (preparedStatement.getWarnings() != null) {
				dbsd.setWarningMessage(preparedStatement.getWarnings().toString());
			}
			logSqlInstructionInfo(dbsd);

			if (result.next() && result != null) {
				setObject(object, result, lsf); 
				DataBaseConnections.releaseConnection(conn);
				return true;
			} 
			
		} catch (SQLException e) {
			this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			setErrorDetails(this.dbsd, e, entityName);
			logSqlInstructionInfo(this.dbsd);
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			throw e;
			
		} finally {
			try {
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);
				result.close();
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);	
				preparedStatement.close();
			} catch (SQLException e) {
				setErrorDetails(this.dbsd, e, entityName);
				logSqlInstructionInfo(this.dbsd);
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
				throw e;
			}
		}
		if (isConnToRelease) {
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
		}
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_NOTFOUND);
		return ret;
	}

 	/*
	 * Update the object
	 */
	public final boolean update(T object) throws SQLException, ExceptionAmrita {

		PreparedStatement preparedStatement = null;
		List<DataBaseItemDescriptor> lsf = null;
		String UPDATE_QUERY = "";
		String entityName = "";
		boolean ret = false;
		int iStartPrep=0;
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_CRUD_UPDATE);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();	
		}
		this.conn.setAutoCommit(false);		

		try {
			entityName=this.typeClass.getSimpleName().substring(6);
			lsf = getEntityFields(object);
			UPDATE_QUERY=createUpdateForPrepare(lsf);
			preparedStatement = conn.prepareStatement(UPDATE_QUERY);
			            
			iStartPrep=setPreparedStatement(1, false, true, lsf, preparedStatement);  // All DT columns Same order as defined
			setPreparedStatement(iStartPrep, true, false, lsf, preparedStatement);    // All PK columns Same order as defined
			this.dbsd.setSqlString(preparedStatement.toString());
			preparedStatement.execute();
			if (preparedStatement.getUpdateCount() <= 0) {
				// Notfound primary key
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_NOTFOUND);
				ret = false;
			} else {
				// Found primary key, update done
				ret = true;
			}

			if (preparedStatement.getWarnings() != null) {
				dbsd.setWarningMessage(preparedStatement.getWarnings().toString());
			}
			logSqlInstructionInfo(this.dbsd);


		} catch (SQLException e) {
			this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			setErrorDetails(this.dbsd, e, entityName);
			logSqlInstructionInfo(this.dbsd);
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			throw e;
			
		} finally {
			try {
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);
				preparedStatement.close();
				if (ret == true && isAutocommit) {
					this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_COMMIT);
					conn.commit();
				}	
			} catch (SQLException e) {
				setErrorDetails(this.dbsd, e, entityName);
				logSqlInstructionInfo(this.dbsd);
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
				throw e;
			}
		}
		if (isConnToRelease) {
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
		}
		return ret;
	}


	/*
	 * Delete the object
	 */
	public final boolean delete(T object) throws SQLException, ExceptionAmrita {

		PreparedStatement preparedStatement = null;
		List<DataBaseItemDescriptor> lsf = null;
		String DELETE_QUERY = "";
		String entityName = "";
		boolean ret = false;
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_CRUD_DELETE);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
		
		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();
		}
		this.conn.setAutoCommit(false);		

		try {
			entityName=this.typeClass.getSimpleName().substring(6);
			lsf = getEntityFields(object);
			DELETE_QUERY=createDeleteForPrepare(lsf);
			preparedStatement = conn.prepareStatement(DELETE_QUERY);
			setPreparedStatement(1, true, false, lsf, preparedStatement);  // All PK columns Same order as defined
			this.dbsd.setSqlString(preparedStatement.toString());
			preparedStatement.execute();
			if (preparedStatement.getUpdateCount() <= 0) {
				// Notfound primary key
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_NOTFOUND);
				ret = false;
			} else {
				ret = true;
			}

			if (preparedStatement.getWarnings() != null) {
				dbsd.setWarningMessage(preparedStatement.getWarnings().toString());
			}
			logSqlInstructionInfo(dbsd);

		} catch (SQLException e) {
			this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			setErrorDetails(this.dbsd, e, entityName);
			logSqlInstructionInfo(this.dbsd);
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			throw e;
			   
		} finally {
			try {
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);
				preparedStatement.close();
				if (ret == true && isAutocommit) {
					this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_COMMIT);
					conn.commit();
				}
			} catch (SQLException e) {
				setErrorDetails(this.dbsd, e, entityName);
				logSqlInstructionInfo(this.dbsd);
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
				throw e;
			}
		}
		if (isConnToRelease) {
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
		}
		return ret;

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
	public final List<T> readSetEntityParmAbstract(String where, Object ... parms) throws SQLException, ExceptionAmritaSqlError {

		PreparedStatement preparedStatement = null;
		List<DataBaseItemDescriptor> lsf = null;
		List<T> ar_Objects = new ArrayList<T>();
		ResultSet result = null;
		String SELECT_FROM = "";
		String entityName = "";
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_READ_SET_ENTITY);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();
		}
		
		try {
			entityName=this.typeClass.getSimpleName().substring(6);
			T objectT = getEntityInstance();
			lsf = getEntityFields(objectT);
			SELECT_FROM=createFindForPrepare(lsf)  + where ;         // Select ... From ... Where ... Order By
			preparedStatement = conn.prepareStatement(SELECT_FROM);
			setPreparedStatementFindAllKey(preparedStatement, parms);
			this.dbsd.setSqlString(preparedStatement.toString());
			preparedStatement.execute();
			result = preparedStatement.getResultSet();

			if (preparedStatement.getWarnings() != null) {
				dbsd.setWarningMessage(preparedStatement.getWarnings().toString());
				logSqlInstructionInfo(dbsd);
			}

			while (result.next()) { 
				T object2 = getEntityInstance();
				setObject(object2, result, lsf); 
				ar_Objects.add(object2);
			}  

		} catch (SQLException e) {
			this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			setErrorDetails(this.dbsd, e, entityName);
			logSqlInstructionInfo(this.dbsd);
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			throw e;
			
		} finally {
			try {
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);
				if (result != null) {
					result.close();
				}
				if (preparedStatement != null) {
				   preparedStatement.close();
				}
			} catch (SQLException e) {
				DataBaseConnections.releaseConnection(conn);
				setErrorDetails(this.dbsd, e, entityName);
				logSqlInstructionInfo(this.dbsd);
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
				throw e;
			}
			if (isConnToRelease) {
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
			}
		}
		return ar_Objects;
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
	public final ResultSet readSetEntityParmRSAbstract(String where, String orderBy, Object ... parms) throws SQLException, ExceptionAmritaSqlError {

		PreparedStatement preparedStatement = null;
		List<DataBaseItemDescriptor> lsf = null;
		List<T> ar_Objects = new ArrayList<T>();
		ResultSet result = null;
		String SELECT_FROM = "";
		String entityName = "";
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_READ_SET_ENTITY);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();
		}
		
		try {
			entityName=this.typeClass.getSimpleName().substring(6);
			T objectT = getEntityInstance();
			lsf = getEntityFields(objectT);
			SELECT_FROM=createFindForPrepare(lsf)  + where + " " + orderBy ;         // Select ... From ... Where ... Order By
			preparedStatement = conn.prepareStatement(SELECT_FROM);
			setPreparedStatementFindAllKey(preparedStatement, parms);
			this.dbsd.setSqlString(preparedStatement.toString());
			preparedStatement.execute();
			result = preparedStatement.getResultSet();

			if (preparedStatement.getWarnings() != null) {
				dbsd.setWarningMessage(preparedStatement.getWarnings().toString());
			}
			logSqlInstructionInfo(dbsd);

			while (result.next()) { 
				T object2 = getEntityInstance();
				setObject(object2, result, lsf); 
				ar_Objects.add(object2);
			}  

		} catch (SQLException e) {
			this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			setErrorDetails(this.dbsd, e, entityName);
			logSqlInstructionInfo(this.dbsd);
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			throw e;
			
		} finally {
			try {
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);
				if (result != null) {
					result.close();
				}
				if (preparedStatement != null) {
				   preparedStatement.close();
				}
			} catch (SQLException e) {
				setErrorDetails(this.dbsd, e, entityName);
				logSqlInstructionInfo(this.dbsd);
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
				throw e;
			}
			if (isConnToRelease) {
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
			}
			
		}
		return result;
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
	public final List<T> readSetEntityWhereAbstract(String where, String orderBy) throws SQLException, ExceptionAmritaSqlError {

		Statement statement = null;
		List<DataBaseItemDescriptor> lsf = null;
		List<T> al_Objects = new ArrayList<T>();
		ResultSet result = null;
		boolean sqlSelectOk = false;
		String SELECT_FROM = "";
		String entityName = "";
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_READ_SET_ENTITY);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();
		}
		
		try {
			entityName=this.typeClass.getSimpleName().substring(6);
			T objectT = getEntityInstance();
			lsf = getEntityFields(objectT);
			SELECT_FROM=createFindForPrepare(lsf)  + " WHERE " + where + " " + orderBy ;         // Select ... From ... Where ... Order By
			this.dbsd.setSqlString(SELECT_FROM);
			
			// esecuzione istruzione
			statement = this.conn.createStatement();
			sqlSelectOk = statement.execute(SELECT_FROM);

			// Se non è un'istruzione di aggiornamento è stato prodotto un ResultSet
			if (sqlSelectOk && statement.getUpdateCount() == -1) {
				result = statement.getResultSet();
				logSqlInstructionInfo(this.dbsd);
			}
	
			if (statement.getWarnings() != null) {
				dbsd.setWarningMessage(statement.getWarnings().toString());
			}
			logSqlInstructionInfo(dbsd);

			while (result.next()) { 
				T object2 = getEntityInstance();
				setObject(object2, result, lsf); 
				al_Objects.add(object2);
			}  

		} catch (SQLException e) {
			this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			setErrorDetails(this.dbsd, e, entityName);
			logSqlInstructionInfo(this.dbsd);
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			throw e;
			
		} finally {
			try {
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);
				if (result != null) {
					result.close();
				}
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				setErrorDetails(this.dbsd, e, entityName);
				logSqlInstructionInfo(this.dbsd);
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
				throw e;
			}
			if (isConnToRelease) {
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
			}
		}
		return al_Objects;
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
	public final List<T> execSqlGenericEntityAbstract(String select, Object ... parms) throws SQLException, ExceptionAmritaSqlError {

		PreparedStatement preparedStatement = null;
		List<DataBaseItemDescriptor> lsf = null;
		List<T> ar_Objects = new ArrayList<T>();
		String entityName = "";
		ResultSet result = null;
		String SELECT_FROM = select;
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_SQL_GENERIC);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();
		}

		try {
			this.conn.setAutoCommit(false);
			T objectT = getEntityInstance();
			entityName=this.typeClass.getSimpleName().substring(6);
			lsf = getEntityFields(objectT);
			preparedStatement = conn.prepareStatement(SELECT_FROM);
			setPreparedStatementFindAllKey(preparedStatement, parms);
			this.dbsd.setSqlString(preparedStatement.toString());
			preparedStatement.execute();
			result = preparedStatement.getResultSet();

			if (preparedStatement.getWarnings() != null) {
				dbsd.setWarningMessage(preparedStatement.getWarnings().toString());
			}
			logSqlInstructionInfo(dbsd);

			while (result.next()) { 
				T object2 = getEntityInstance();
				setObject(object2, result, lsf); 
				ar_Objects.add(object2);
			}  

		} catch (SQLException e) {
			setErrorDetails(this.dbsd, e, entityName);
			logSqlInstructionInfo(this.dbsd);
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			throw e;
			
		} finally {			
			try {
				if (result != null) {
					result.close();
				}
				if (preparedStatement != null) {
				   preparedStatement.close();
				}
			} catch (Exception e) {
				this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_PREP_STMT__CLOSE);
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
				throw e;
			}
		}
		if (isConnToRelease) {
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
		}
		return ar_Objects;
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
	public final ResultSet execSqlGenericRSAbstract(String sqlString) throws SQLException, ExceptionAmrita {

		Statement stmt = null;
		ResultSet rs = null;
		boolean sqlSelectOk = false;
		this.dbsd.clear();
		this.dbsd.setTypeOperation(EnumDataBaseOperation.DB_GENERIC_SQL);
		this.dbsd.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
		this.dbsd.setSqlString(sqlString);

		if (this.conn == null) {
			this.conn=DataBaseConnections.getConnection();
		}

		try {
			this.conn.setAutoCommit(false);
			stmt = this.conn.createStatement();
			sqlSelectOk = stmt.execute(sqlString);

			// Se non è un'istruzione di aggiornamento è stato prodotto un ResultSet
			if (sqlSelectOk && stmt.getUpdateCount() == -1) {
				rs = stmt.getResultSet();
				logSqlInstructionInfo(this.dbsd);
			// Update instruction
			} else {
				dbsd.setUpdateCount(stmt.getUpdateCount());
			}
	
			if (stmt.getWarnings() != null) {
				dbsd.setWarningMessage(stmt.getWarnings().toString());
			}
			logSqlInstructionInfo(dbsd);


		} catch (SQLException e) {
			setErrorDetails(this.dbsd, e, "");
			logSqlInstructionInfo(this.dbsd);
			DataBaseConnections.releaseConnection(conn);
			this.conn=null;
			throw new ExceptionAmrita(EnumAmritaExceptionError.ERROR_SQL, e);
			
		} finally {
			if (isConnToRelease) {
				DataBaseConnections.releaseConnection(conn);
				this.conn=null;
			}
		}
		
		return rs;
	}	
	/*
	 * Set prepared statement from object parms for specific keys, it depends on specific request
	 */
	private final void setPreparedStatementFindAllKey(PreparedStatement preparedStatement, Object ...  parms) throws SQLException{
	   int i = 1; 
       for (Object object : parms) {
		  if (object instanceof String) {
			  preparedStatement.setString(i++,(String) object);                          
		  } else if (object instanceof Integer) {
			  preparedStatement.setShort(i++, Short.valueOf(object.toString()));  	
		  } else if (object instanceof Enum) {
			  Enum e = (Enum) object;
 			  preparedStatement.setShort(i++, Short.valueOf(e.ordinal()+""));  	
		}
	   }
	}			

    /*
     * 
     * Service method tp get an instance of the class entity
     */
	@SuppressWarnings("deprecation")
	private T getEntityInstance(){
		try{
			return typeClass.newInstance();
		}catch(Exception e){
			e.printStackTrace();
			return null;
		}
	}  


/*
 * Restituzione descrittori campi entity con  @Column e @Id
 * Impostazione valore colonna in descrittore
 * 	   
*/
private List<DataBaseItemDescriptor> getEntityFields(T o) {
	ReflectionManager rm = new ReflectionManager();
	DataBaseItemDescriptor dmid = null;
	List<DataBaseItemDescriptor> ls_dmid = null;
	
	// Persistence Annotations classes
	Entity e = null;
	Column c = null;
	Id id = null;

	// Campi per caratteristiche campo/colonna db/java
	Type javaFieldType = null;
	Class<?> javaFieldClass = null;
	String javaFieldName = "";
	String javaFieldClassName = "";
	String javaFieldClassNameSimple = "";
    String dbTableName = "";
	String dbColumnName = "";

	// Campi per utilizzo reflection su classi e annotazioni
	Object objectReflection = null;
	String methodNameReflection = "";
    String txtFieldType = "";
    
    // Descrittore campi
	ls_dmid = new ArrayList<DataBaseItemDescriptor>();

	// Get Table name
	e = (Entity) rm.getAnnotation(o.getClass(), "javax.persistence.Entity");
	// No @Entity annotation in DAO
	if (e == null) {
		return ls_dmid;
	}
	
	dbTableName = e.name();
		
	// Get Entity fields/columns
	Field[] arField = rm.getFields(o);
	arField = o.getClass().getDeclaredFields();
	for (Field f : arField) {
		// Get java fieldName/Column/Id annotation
		javaFieldName = f.getName();
		c = (Column) f.getAnnotation(Column.class);
		if (c != null) {
			dmid = new DataBaseItemDescriptor();
			dbColumnName = c.name();
			id = (Id) f.getAnnotation(Id.class);						
			
			// Tipo campo, nome classe e oggetto Class  
			javaFieldType =  f.getGenericType();
			txtFieldType = javaFieldType.toString();
			javaFieldClass = getClassFromFieldType(rm, txtFieldType);
			
			// Tipo campo non riconosciuto: errore interno oppure Entity con tipi campi di classe  non permessa
			if (javaFieldClass == null) {
				dmid.setErrorCode("ET0019");
			}
			
			// Tipo campo riconosciuto
			if (txtFieldType.startsWith("class")) {
				javaFieldClassName = javaFieldClass.getName();
				javaFieldClassNameSimple = javaFieldClass.getSimpleName();
			} else {
				javaFieldClassName = txtFieldType;
				javaFieldClassNameSimple = txtFieldType;
			}		
            
			// Invoke via reflection metodo getter getFieldname() per caricamento valore campo in struttura
			methodNameReflection = "get" + javaFieldName.substring(0, 1).toUpperCase() + javaFieldName.substring(1);
			objectReflection = rm.invokeMethod(o, methodNameReflection, null, null);
			if (rm.getException() != null) {
				dmid.setErrorCode("EF0012");
			}			

			// Populate entry field/column information
			dmid.setJavaFieldName(javaFieldName);
			dmid.setDbColumn(dbColumnName);
			dmid.setDbTable(dbTableName);

			// Populate others info
			dmid.setDbColumnType(null);
			dmid.setDbColumnValue("");
			dmid.setJavaEnumeration(false);
			dmid.setJavaFieldClass(javaFieldClass);
			dmid.setJavaFieldClassName(javaFieldClassName);
			dmid.setJavaFieldClassNameSimple(javaFieldClassNameSimple);
			dmid.setJavaFieldType(javaFieldType);
			dmid.setJavaFieldValue(objectReflection);
			dmid.setPrimaryKey(false);
			if (id != null) {
				dmid.setPrimaryKey(true);
			}
			if (objectReflection instanceof Enum ) {
				dmid.setJavaEnumeration(true);
			}
			ls_dmid.add(dmid);
		} 
	}		
	return ls_dmid;
}

/*
 * Update valore campo (Column) da oggetto Entity in descrittore campi
 * 	   
*/
private List<DataBaseItemDescriptor> updEntityFieldsValue(T o, List<DataBaseItemDescriptor> ls_dmid) {
	
	ReflectionManager rm = AmritaStartup.rm;
	
	// Campi per caratteristiche campo/colonna db/java
	String javaFieldName = "";

	// Campi per utilizzo reflection su classi e annotazioni
	Object objectReflection = null;
	String methodNameReflection = "";
    
    // Sccan decrittore campi
    for (DataBaseItemDescriptor dmid : ls_dmid) {
    	javaFieldName = dmid.getJavaFieldName();
		// Invoke via reflection metodo getter getFieldname() per caricamento valore campo in struttura
		methodNameReflection = "get" + javaFieldName.substring(0, 1).toUpperCase() + javaFieldName.substring(1);
		objectReflection = rm.invokeMethod(o, methodNameReflection, null, null);
		if (rm.getException() != null) {
			dmid.setErrorCode("EF0012");
		}			
		dmid.setJavaFieldValue(objectReflection);
	}
 	return ls_dmid;
}

/*
 * Restituisce un oggetto class a partire dal tipo di dato
 * 
 */
private Class<?> getClassFromFieldType(ReflectionManager rm, String fieldTypeFull) {
   
	String fieldType = "";
    
   // Tipo campo oggetto: può essere solo String o enum
   if (fieldTypeFull.startsWith("class")) {
    	fieldType = fieldTypeFull.substring(6);
		// Tipo campo oggetto String
		if (fieldType.equals("java.lang.String") || fieldType.startsWith("enums.") ) {
			return rm.getClass(fieldType);
		}
    	return null;
    }
    
    fieldType = fieldTypeFull;
	
	// Tipi campi primitivi
	if (fieldType.equals("char")) {
		return Character.TYPE;
	}
	if (fieldType.equals("byte")) {
		return Byte.TYPE;
	}
	if (fieldType.equals("boolean")) {
		return Boolean.TYPE;
	}
	if (fieldType.equals("int")) {
		return Integer.TYPE;
	}
	if (fieldType.equals("long")) {
		return Long.TYPE;
	}
	if (fieldType.equals("double")) {
		return Double.TYPE;
	}
	if (fieldType.equals("float")) {
		return Float.TYPE;
	}

	return null;
}
		

/* INSERT statement */
private String createInsertForPrepare(List<DataBaseItemDescriptor> lsf) {
    StringBuffer sb = new StringBuffer();
	String comma="";
	
	for (DataBaseItemDescriptor d : lsf) {
		if (sb.length() == 0) {
			sb.append("INSERT INTO ");
			sb.append(d.getDbTable() + " (");
		}
		sb.append(comma);
		sb.append(d.getDbColumn());
		comma=",";
	}
	comma="";
	sb.append(") ");
	sb.append("VALUES (");
    for (int i = 0; i < lsf.size(); i++) {
    	sb.append(comma);
    	sb.append("?");
    	comma=",";
	}
	sb.append(") ");
    return sb.toString();
}

/* SELECT statement */
private String createSelectForPrepare(List<DataBaseItemDescriptor> lsf) {
    StringBuffer sb = new StringBuffer();
	String comma="";
	String and="";
	
	sb.append("SELECT ");
	
	// Scan all columns
	for (DataBaseItemDescriptor d : lsf) {
		sb.append(comma);
		sb.append(d.getDbColumn());
		comma=",";
	}
	
	sb.append(" FROM ");
	sb.append(lsf.get(0).getDbTable());
	sb.append(" WHERE ");
	
	// Scan all columns
	for (DataBaseItemDescriptor d : lsf) {
		if (d.isPrimaryKey()) {
			sb.append(and);
			sb.append(d.getDbColumn());
			sb.append(" = ? ");
			and=" AND ";
		}
	}	
    return sb.toString();	
}


/* UPDATE statement */
private String createUpdateForPrepare(List<DataBaseItemDescriptor> lsf) {
    StringBuffer sb = new StringBuffer();
	String comma="";
	String and="";
	
	sb.append("UPDATE ");
	sb.append(lsf.get(0).getDbTable());
	sb.append(" SET ");
	
	// Scan all columns
	for (DataBaseItemDescriptor d : lsf) {
		if (!d.isPrimaryKey()) {
			sb.append(comma);
			sb.append(d.getDbColumn());
			sb.append(" = ? ");
			comma=",";
		}
	}

	sb.append(" WHERE ");
	
	// Scan all columns
	for (DataBaseItemDescriptor d : lsf) {
		if (d.isPrimaryKey()) {
			sb.append(and);
			sb.append(d.getDbColumn());
			sb.append(" = ? ");
			and=" AND ";
		}
	}	
    return sb.toString();	
}


/* DELETE statement */
private String createDeleteForPrepare(List<DataBaseItemDescriptor> lsf) {
    StringBuffer sb = new StringBuffer();
	String and="";
	
	sb.append("DELETE FROM ");
	sb.append(lsf.get(0).getDbTable());
	sb.append(" WHERE ");
	
	// Scan all columns
	for (DataBaseItemDescriptor d : lsf) {
		if (d.isPrimaryKey()) {
			sb.append(and);
			sb.append(d.getDbColumn());
			sb.append(" = ? ");
			and=" AND ";
		}
	}	
    return sb.toString();	
}

/* SELECT ... WHERE ... ORDER BY */
private String createFindForPrepare(List<DataBaseItemDescriptor> lsf) {
    StringBuffer sb = new StringBuffer();
	String comma="";
	
	sb.append("SELECT ");
	
	// Scan all columns
	for (DataBaseItemDescriptor d : lsf) {
		sb.append(comma);
		sb.append(d.getDbColumn());
		comma=",";
	}
	
	sb.append(" FROM ");
	sb.append(lsf.get(0).getDbTable());
	sb.append(" T1 ");

	return sb.toString();	}



/* Prepare statement from list of fields for only PK columns, only not PK colymns or both */
private int setPreparedStatement(int i, boolean isOnlyPK, boolean isOnlyData,  List<DataBaseItemDescriptor> lsf, PreparedStatement ps) throws ExceptionAmrita, SQLException {
	
	// Scan colonne da inserire/aggiornare
	for (DataBaseItemDescriptor dbid : lsf) {
		
		if (isOnlyPK    && !dbid.isPrimaryKey()
		||  isOnlyData  && dbid.isPrimaryKey() ) {
			continue;
		}
		
		// Tipi campi elementari
		if (dbid.getJavaFieldClassNameSimple().equals("String")) {
			dbid.setDbColumnValue("'" + (String) dbid.getJavaFieldValue() + "'");
			ps.setString(i++, (String) dbid.getJavaFieldValue());
			continue;
		}
		if (dbid.getJavaFieldClassNameSimple().equals("char")) {
			ps.setString(i++, (String) dbid.getJavaFieldValue());
			continue;
		}
		if (dbid.getJavaFieldClassNameSimple().equals("byte")) {
			ps.setString(i++, (String) dbid.getJavaFieldValue());
			continue;
		}
		if (dbid.getJavaFieldClassNameSimple().equals("boolean")) {
			ps.setBoolean(i++, (Boolean) dbid.getJavaFieldValue());
			continue;
		}
		if (dbid.getJavaFieldClassNameSimple().equals("int")) {
			ps.setInt(i++, ((Integer) dbid.getJavaFieldValue()));
			continue;
		}
		if (dbid.getJavaFieldClassNameSimple().equals("long")) {
			ps.setLong(i++, ((Long) dbid.getJavaFieldValue()));
			continue;
		}
		if (dbid.getJavaFieldClassNameSimple().equals("double")) {
			ps.setDouble(i++, ((Double) dbid.getJavaFieldValue()));
			continue;
		}
		if (dbid.getJavaFieldClassNameSimple().equals("float")) {
			ps.setFloat(i++, ((Float) dbid.getJavaFieldValue()));
			continue;
		}	
		
		// Enumerazione: estrazione numero ordinale e trattamento come int
		if (dbid.isJavaEnumeration()) {
			@SuppressWarnings("rawtypes")
			Enum e = (Enum) dbid.getJavaFieldValue();
			if (e == null) {
				ExceptionAmrita excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_DB_COLUMN_MAPPING_NULL, null);
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INTERNAL, "EI0012", null, excp); 			
				throw excp;
			}
			ps.setShort(i++, ((short) e.ordinal()));
			continue;
		}	

		// Tipo campo non gestito: logging e lancio eccezione generale gestita
		ExceptionAmrita excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_PROGRAM_INTERNAL, null);
//		logMessage(EnumMessageType.INFORMATION, "MI0045", null, dbid.getJavaFieldClassName());
 		String[] ar= {dbid.getJavaFieldClassName(), dbid.getJavaFieldName(), dbid.getDbTable()};
 		AmritaStartup.lf.writeRow(EnumMessageType.ERROR_INTERNAL, "EI0011", ar, excp);
		throw excp;
	}	
	return i;
}

/* Set object fields from resultset */
private void setObject(T object, ResultSet result, List<DataBaseItemDescriptor> lsf) throws SQLException  {
	ReflectionManager rm = new ReflectionManager();
	
	// Campi per utilizzo reflection su classi e annotazioni
	Object ar_ObjectInvoke[] = null;
	Object ar_enumsConstants[] = null;
	Class<?> ar_ObjectClassInvoke[] = null;
	Class<?> c = null;
	String methodNameReflection = "";

	// Scan colonne da inserire/aggiornare
	for (DataBaseItemDescriptor dbid : lsf) {
		
		try {
			dbid.setJavaFieldValue(result.getString(dbid.getDbColumn()));
		} catch (SQLException e) {
			// Solo se EntitySqlGeneric si utilizzano solo con le colonne che servono
			// In tutti gli altri casi tutte le colonne dichiarate in Entity devono essere dichiarate in Select 
			if (this.typeClass.getName() != "entities.EntitySqlGeneric") {
			  e.printStackTrace();
			  throw e;
			} 
		}

		
		// Imposto metodo setter
		methodNameReflection = "set" + dbid.getJavaFieldName().substring(0, 1).toUpperCase() + dbid.getJavaFieldName().substring(1);
		
		// Invoke via reflection metodo setter setFieldName(value)
		ar_ObjectInvoke = new Object[1];;
		ar_ObjectClassInvoke = new Class[1];
		ar_ObjectInvoke[0] = dbid.getJavaFieldValue();
		try {
			if (dbid.getJavaFieldClassName().equals("int")) {
				ar_ObjectInvoke[0] = Integer.parseInt((String) dbid.getJavaFieldValue());
			} else if (dbid.getJavaFieldClassName().equals("long")) {
				ar_ObjectInvoke[0] = Long.parseLong((String) dbid.getJavaFieldValue());
				dbid.setJavaFieldClass(long.class);
			} else if (dbid.getJavaFieldClassName().equals("double")) {
				ar_ObjectInvoke[0] = Double.parseDouble((String) dbid.getJavaFieldValue());
				dbid.setJavaFieldClass(double.class);
			} else if (dbid.getJavaFieldClassName().equals("boolean")) {
				ar_ObjectInvoke[0] = Boolean.parseBoolean((dbid.getJavaFieldValue().equals("0") ? "false":"true" ));
				dbid.setJavaFieldClass(boolean.class);
			} else {
				c=Class.forName(dbid.getJavaFieldClassName());
				ar_enumsConstants = c.getEnumConstants();
			    // Il campo è una Enum
				if (ar_enumsConstants != null) {
					int i = Integer.parseInt((String) dbid.getJavaFieldValue());
					ar_ObjectInvoke[0] = ar_enumsConstants[i];
				}			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		ar_ObjectClassInvoke[0] = dbid.getJavaFieldClass();
		
		rm.invokeMethod(object, methodNameReflection, ar_ObjectClassInvoke, ar_ObjectInvoke);
		if (rm.getException() != null) {
//			throw new ExceptionAmritaReflectionError(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, rm.getException());
		}
				
	}
}

/*
 * Set Sql error Details
 */
private void setErrorDetails(DataBaseStatusDetailed dbsd, SQLException e, String entityName) {
	if (e.getCause() != null) {
		dbsd.setErrorCause(e.getCause().toString());    	
	}   
	dbsd.setSqlStatus(e.getSQLState().toString());
	dbsd.setSqlErrorCode(e.getErrorCode());
	dbsd.setSqlMessage(e.getMessage());
	dbsd.setLocalizedMessage(e.getLocalizedMessage());
	dbsd.setExcpOrigin(e);	
	dbsd.setEntityName(entityName);
}

/*
 * Generazione entry su log con informazioni istruzione Sql eseguita se previsto
 * dai parametri di configurazione. 
 * Sul log vengono scritti messaggi di tipo INFO e se dovesse verificarsi qualche exceptio sql, 
 * verrebbero loggati anche i relativi messaggi di tipo DEBUG.
 * 
*/
private void logSqlInstructionInfo(DataBaseStatusDetailed dbs) {
	
	String arParm[] = null;
	
	// Nessun errore e nessuna direttiva di log per ogni istruzione sql
	if (!ucfg.isDataBaseLogAnySql() && dbs.getStatusOperation() == EnumDataBaseOperationStatus.DB_OPERATION_OK) {
		return;
	}

	// Messaggio esito operazione Sql 
	arParm = new String[6];
	arParm[0] = dbs.getTypeOperation().toString();
	arParm[1] = dbs.getStatusOperation().toString();
	arParm[2] = dbs.getSqlStatus().toString();
	arParm[3] = new Integer(dbs.getSqlErrorCode()).toString();
	arParm[4] = dbs.getEntityName();
	arParm[5] = dbs.getErrorCause();
    AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0027", arParm, null); 

    // Messaggio stringa Sql 
	if (!dbs.getSqlString().equals("")) {
		arParm = new String[1];
		arParm[0] = dbs.getSqlString();
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0028", arParm, null);
	}
	
	// Comando Jdbc 
	if (ucfg.isLogVerbose() && !dbs.getJdbcCommand().equals("")) {
		arParm = new String[1];
		arParm[0] = dbs.getJdbcCommand();
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0029", arParm, null);
	} 

}


	
	/**
	 * @throws SQLException 
	 */
	public void commit() throws SQLException {
		conn.commit();
	}

	/**
	 * @throws SQLException 
	 */
	public void rollback() throws SQLException {
		conn.rollback();
	}


	/**
	 * @return the conn
	 */
	public Connection getConn() {
		return conn;
	}


	/**
	 * @param conn the conn to set
	 */
	public void setConn(Connection conn) {
		this.conn = conn;
		this.ucfg.setDbConn(conn);
	}

	/**
	 * @return Release the current  connection
	 * @throws SQLException 
	 */
	public void releaseConn() throws SQLException {
		DataBaseConnections.releaseConnection(this.conn);
		this.conn = null;
	    ;
	}


	/**
	 * @return the isConnToClose
	 */
	public boolean isConnToRelease() {
		return isConnToRelease;
	}

	/**
	 * @param isConnToClose true clonnection closed after CRUD operation
	 */
	public void setConnTorelease(boolean isConnToClose) {
		this.isConnToRelease = isConnToClose;
	}

	
	/**
	 * @return the isCommitAfterUpdates
	 */
	public boolean isAutocommit() {
		return isAutocommit;
	}

	/**
	 * @param isAutocommit the isAutocommit to set
	 */
	public void setAutocommit(boolean isAutocommit) {
		this.isAutocommit = isAutocommit;
	}

	/**
	 * @return the dbsd
	 */
	public DataBaseStatusDetailed getDbsd() {
		return dbsd;
	}
	
}