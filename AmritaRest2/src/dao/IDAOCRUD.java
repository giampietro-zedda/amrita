/**
 * 
 */
package dao;

import java.sql.SQLException;
import java.util.List;

import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;


/**
 * Interfaccia DAO per generiche operazioni CRUD.
 */
public interface IDAOCRUD<T>  {
 
	 /** Create a non existent object  **/
	 boolean create(T o) throws SQLException, ExceptionAmrita;
	
	 /** Create a non existent object 
	 * @throws ExceptionAmritaSqlError */
	 int[] createBulk(List<T> lo) throws SQLException, ExceptionAmrita;
	
	 /** Get object by key **/
	 boolean read(T o) throws SQLException, ExceptionAmrita;
	
	 /** Update an existent object  */
	 boolean update(T o) throws SQLException, ExceptionAmrita;
	
	 /** Delete an existent object */
	 boolean delete(T o) throws SQLException, ExceptionAmrita;

}