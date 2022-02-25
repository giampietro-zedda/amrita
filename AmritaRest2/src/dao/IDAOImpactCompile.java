/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityImpactCompile;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity ImpactPlan.
 */
public interface IDAOImpactCompile extends IDAO<EntityImpactCompile> {

	/*
	 * 1) Get all objects to be compiled affected by an impact plan
	 */
	public  List<EntityImpactCompile> findAll(String sys, String idPlan) throws SQLException, ExceptionAmritaSqlError; 

}