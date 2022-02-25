package rest;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import analyzer.AmritaConstants;
import analyzer.AmritaStartup;
import analyzer.DataBaseConnections;
import analyzer.ExecutionDirectives;
import analyzer.ProcessImpactAnalysis;
import analyzer.UserActive;
import analyzer.UserConfiguration;
import dao.DAOFactory;
import dao.DAOImplImpactCompile;
import dao.DAOImplImpactObject;
import dao.DAOImplImpactPlan;
import dao.DAOImplRelation;
import dao.DAOImplWhereUsedItem;
import dao.IDAOImpactCompile;
import dao.IDAOImpactObject;
import dao.MySQLDAOFactory;
import dao.IDAOImpactPlan;
import dao.IDAORelation;
import dao.IDAOSqlGeneric;
import dao.IDAOWhereUsedItem;
import entities.EntityImpactCompile;
import entities.EntityImpactObject;
import entities.EntityImpactPlan;
import entities.EntityRelation;
import entities.EntitySqlGeneric;
import entities.EntityWhereUsedItem;
import enums.EnumCobolReservedWords;
import enums.EnumDataItemType;
import enums.EnumMessageType;
import enums.EnumObject;
import enums.EnumRelation;
import enums.EnumTypeImpactChange;
import exception.ExceptionAmrita;

/*
 * 
 * Classe con web service di servizio per:
 * 
 * 1) GestioneEntityImpactPlan (get/save/delete)
 * 2) Servizi per impact
 * ...
 */

@Path("/")   
public class RestImpactService implements AmritaConstants {


	/*
	 * Gestione CREATE EntityImpactPlan righe multiple
     *
	 */
	@POST
	@Path("createImpactPlan/{user}/{sys}/{subSys}/{idPlan}")  
	public Response createImpactPlan(
								      @PathParam("user")  	  			String user      
								     ,@PathParam("sys")  	  			String sys  		 
								     ,@PathParam("subSys")    			String subSys   	 
								     ,@PathParam("idPlan")          	String idPlan 	
 								     ,@FormParam("liRow")          		List<String> liRow   								     								     
									) throws ExceptionAmrita, SQLException  {

		UserConfiguration ucfg = null;
		List<EntityImpactPlan> al_entityImpactPlan = null;
		EntityImpactPlan entityImpactPlan =null;
		String arVal[] = null;
	    String numOp = "";  	 
		String idObjectOrigin = "";
		String typeObjectOrigin = "";
		String typeObjectOriginOrdinal = "";
		String typeImpactChange = "";      
		String typeImpactChangeOrdinal = ""; 
		String fieldColumn = "";
		String fromLength = "";      
		String toLength = "";      
		String fromInt = "";      
		String toInt = "";      
		String fromDec = "";      
		String toDec = "";      
		String fromDataType = "";      
		String fromDataTypeOrdinal = "";      
		String toDataType = "";      
		String toDataTypeOrdinal = "";  
		String fromDefaultValue = "";
		String toDefaultValue = "";
		String fromSign = "";    
		String toSign = "";   
		
		al_entityImpactPlan = new ArrayList<EntityImpactPlan>();
 
		// Scan rows
		for (String rowVal : liRow) {
			
			arVal = rowVal.split(":");
		    numOp = arVal[0]; 	 
			idObjectOrigin = arVal[1];
			typeObjectOrigin = arVal[2];
			typeObjectOriginOrdinal = arVal[3];
			typeImpactChange = arVal[4];     
			typeImpactChangeOrdinal = arVal[5];  
			fieldColumn = arVal[6]; 
			fromLength = arVal[7];     
			toLength = arVal[8]; 
			fromInt = arVal[9];     
			toInt = arVal[10];     
			fromDec = arVal[11];      
			toDec = arVal[12];     
			fromDataType = arVal[13];      
			fromDataTypeOrdinal = arVal[14];      
			toDataType = arVal[15];     
			toDataTypeOrdinal = arVal[16];     
			fromDefaultValue = arVal[17];     
			toDefaultValue = arVal[18]; 
			fromSign = arVal[19];     
			toSign = arVal[20];
            
			// Normalize values fo MAP etc
			if (fromLength.isBlank()) {fromLength = "0";}
			if (toLength.isBlank()) {toLength = "0";}
			if (fromInt.isBlank()) {fromInt = "0";}
			if (toInt.isBlank()) {toInt = "0";}
			if (fromDec.isBlank()) {fromDec = "0";}
			if (toDec.isBlank()) {toDec = "0";}			
			
			entityImpactPlan = new EntityImpactPlan();
			entityImpactPlan.setSystem(sys);      
			entityImpactPlan.setSubSystem(subSys);      
			entityImpactPlan.setIdPlan(idPlan);
			entityImpactPlan.setNumOp(Integer.parseInt(numOp));      
			entityImpactPlan.setIdObjectOrigin(idObjectOrigin);
			entityImpactPlan.setTypeObjectOrigin(EnumObject.values()[Integer.parseInt(typeObjectOriginOrdinal.trim())]);
			entityImpactPlan.setTypeImpactChange(EnumTypeImpactChange.values()[Integer.parseInt(typeImpactChangeOrdinal.trim())]);
			entityImpactPlan.setFieldColumn(fieldColumn);
			entityImpactPlan.setFromLength(Integer.parseInt(fromLength));
			entityImpactPlan.setToLength(Integer.parseInt(toLength));
			entityImpactPlan.setFromInt(Integer.parseInt(fromInt));
			entityImpactPlan.setToInt(Integer.parseInt(toInt));
			entityImpactPlan.setFromDec(Integer.parseInt(fromDec));
			entityImpactPlan.setToDec(Integer.parseInt(toDec));
			entityImpactPlan.setFromDataType(EnumDataItemType.values()[Integer.parseInt(fromDataTypeOrdinal.trim())]);
			entityImpactPlan.setToDataType(EnumDataItemType.values()[Integer.parseInt(toDataTypeOrdinal.trim())]);
			entityImpactPlan.setFromDefaultValue(fromDefaultValue);
			entityImpactPlan.setToDefaultValue(toDefaultValue);
			entityImpactPlan.setFromSign(false);    // TODO
			entityImpactPlan.setToSign(false);      // TODO
			
			al_entityImpactPlan.add(entityImpactPlan);
		}
				
        String statusRet = "";
        @SuppressWarnings("unused")
		int[] isInserted = null;;
                
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
				
		try {
			MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
			Connection conn = DataBaseConnections.getConnection();
			IDAOImpactPlan impactPlanDAO =  (DAOImplImpactPlan) sqlFactory.getDAOImpactPlan(conn, true, true, ucfg);
			isInserted = impactPlanDAO.createBulk(al_entityImpactPlan);
			conn.commit();
			statusRet="OK";
		} catch (Exception e) {
			statusRet="KO"+e.getMessage();
		}
		
	    return Response
 				.status(200)
				.entity(statusRet)     // OK or KO+Message
				.build();
	}

	/*
	 * Gestione UPDATE EntityImpactPlan singola riga
     *
	 */
	@POST
	@Path("updateImpactPlan/{user}/{sys}/{subSys}/{idPlan}")  
	public Response updateImpactPlan(
								      @PathParam("user")  	  			String user      
								     ,@PathParam("sys")  	  			String sys  		 
								     ,@PathParam("subSys")    			String subSys   	 
								     ,@PathParam("idPlan")          	String idPlan   	 
								     ,@FormParam("numOp")          		String numOp   	 
									 ,@FormParam("idObjectOrigin")  	String idObjectOrigin
									 ,@FormParam("typeObjectOrigin")    String typeObjectOrigin
									 ,@FormParam("typeImpactChange") 	String typeImpactChange      
									 ,@FormParam("fromLength") 			String fromLength      
									 ,@FormParam("toLength") 			String toLength      
									 ,@FormParam("fromInt") 			String fromInt      
									 ,@FormParam("toInt") 				String toInt      
									 ,@FormParam("fromDec") 			String fromDec      
									 ,@FormParam("toDec") 				String toDec      
									 ,@FormParam("fromDataType") 		String fromDataType      
									 ,@FormParam("toDataType") 			String toDataType      
									 ,@FormParam("fromDataTypePic") 	String fromDataTypePic      
									 ,@FormParam("toDataTypePic") 		String toDataTypePic      
									) throws ExceptionAmrita, SQLException {

		UserConfiguration ucfg = null;
		EntityImpactPlan entityImpactPlan =null;
        String statusRet = "";
		boolean isUpdated = false;
        
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOImpactPlan impactPlanDAO =  (DAOImplImpactPlan) sqlFactory.getDAOImpactPlan(conn, true, true, ucfg);

		entityImpactPlan=new EntityImpactPlan();
		entityImpactPlan.setSystem(sys);      
		entityImpactPlan.setSubSystem(subSys);      
		entityImpactPlan.setIdPlan(idPlan);
		entityImpactPlan.setNumOp(Integer.parseInt(numOp));      
		entityImpactPlan.setIdObjectOrigin(idObjectOrigin);
		entityImpactPlan.setTypeObjectOrigin(EnumObject.values()[Integer.parseInt(typeObjectOrigin.trim())]);
		entityImpactPlan.setTypeImpactChange(EnumTypeImpactChange.values()[Integer.parseInt(typeImpactChange.trim())]);
		entityImpactPlan.setFromLength(Integer.parseInt(fromLength));
		entityImpactPlan.setToLength(Integer.parseInt(toLength));
		entityImpactPlan.setFromInt(Integer.parseInt(fromInt));
		entityImpactPlan.setToInt(Integer.parseInt(toInt));
		entityImpactPlan.setFromDec(Integer.parseInt(fromDec));
		entityImpactPlan.setToDec(Integer.parseInt(toDec));
		entityImpactPlan.setFromDataType(EnumDataItemType.values()[Integer.parseInt(fromDataType.trim())]);
		entityImpactPlan.setToDataType(EnumDataItemType.values()[Integer.parseInt(toDataType.trim())]);
		entityImpactPlan.setFromDataTypePic(fromDataTypePic);
		entityImpactPlan.setToDataTypePic(toDataTypePic);
	
		isUpdated = impactPlanDAO.update(entityImpactPlan);
		
		if (isUpdated) {
			statusRet="OK";
			conn.commit();
		} else {
			statusRet="KO";
		}
		
	    return Response
 				.status(200)
				.entity(statusRet)     // OK or KO+Message
				.build();
	}
	
	
	/*
	 * Gestione DELETE Entity ImpactPlan
	 *                 Entity ImpactObject
     *                 Entity ImpactCompile
	 */
	@DELETE
	@Path("deleteImpactPlan/{user}/{sys}/{subSys}/{idImpactPlan}")  
	public Response deleteImpactPlan(
								      @PathParam("user")  	  		String user      
								     ,@PathParam("sys")  	  		String sys  		 
								     ,@PathParam("subSys")    		String subSys   	 // Non utilizzato, disponibile per usi futuri
								     ,@PathParam("idImpactPlan")    String idImpactPlan   	 
									 ) throws ExceptionAmrita, SQLException {

		UserConfiguration ucfg = null;
        String statusRet = "";
        String strSql = "";
        
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOImpactPlan impactPlanDAO =  (DAOImplImpactPlan) sqlFactory.getDAOImpactPlan(conn, true, true, ucfg);

		strSql = "DELETE FROM impactPlan  "
				+ "WHERE "
				+ "     sys = '" + sys + "'" 
				+ " AND idPlan = '" + idImpactPlan + "'" 
				;
		
		impactPlanDAO.execSqlGeneric(strSql);    

		strSql = "DELETE FROM impactObject "
				+ "WHERE "
				+ "     sys = '" + sys + "'" 
				+ " AND idPlan = '" + idImpactPlan + "'" 
				;		
		impactPlanDAO.execSqlGeneric(strSql);    
		
		strSql = "DELETE FROM impactCompile "
				+ "WHERE "
				+ "     sys = '" + sys + "'" 
				+ " AND idPlan = '" + idImpactPlan + "'" 
				;		
		impactPlanDAO.execSqlGeneric(strSql);    
		
		conn.commit();
		
		statusRet = "OK";  

	    return Response
				.status(200)
				.entity(statusRet)     // OK or KO+Message
				.build();
	}
	
	
	/*
	 * Gestione CREATE Entity ImpactObject righe multiple
	 * Fornito in input l'elenco delle operazioni relative al plan.
	 * Ogni operazione individua un tipo oggetto e oggetto origine e un campo/colonna
     *
	 */
	@POST
	@Path("createImpactObject/{user}/{sys}/{subSys}/{idPlan}")  
	public Response createImpactObject(
								      @PathParam("user")  	  			String user      
								     ,@PathParam("sys")  	  			String sys  		 
								     ,@PathParam("subSys")    			String subSys   	 
								     ,@PathParam("idPlan")          	String idPlan 								     
								     ,@FormParam("liRow")          		List<String> liRow   									     
									) throws ExceptionAmrita, SQLException {

		UserConfiguration ucfg = null;
		IDAOImpactObject impactObjectDAO = null;
		IDAOWhereUsedItem whereUsedDAO = null;
		List<EntityImpactObject> al_entityObjectPlan = null;
		List<EntityWhereUsedItem> ls_whereUsed = null;
		EntityImpactObject entityImpactObject =null;
		String arVal[] = null;
        
	    String numOp = "";	 
		String typeObjectOrigin = "";	 
		String idObjectOrigin = "";	
		String fieldColumn = "";	
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();

		al_entityObjectPlan = new ArrayList<EntityImpactObject>();
		
		// Scan rows
		for (String rowVal : liRow) {
			
			arVal = rowVal.split(":");
		    numOp = arVal[0];
			typeObjectOrigin = arVal[1];
			idObjectOrigin = arVal[2];
			fieldColumn = arVal[3];     
			
			whereUsedDAO = (DAOImplWhereUsedItem) AmritaStartup.sqlFactory.getDAOWhereUsedItem(conn, false, false, ucfg);
			ls_whereUsed=whereUsedDAO.findAll(sys, idObjectOrigin, EnumObject.values()[Integer.parseInt(typeObjectOrigin.trim())], fieldColumn);
			
			// Scan where used found and accum impactObject
			for (EntityWhereUsedItem eoWhereUsed : ls_whereUsed) {
				entityImpactObject = new EntityImpactObject();
				entityImpactObject.setSystem(sys);      
				entityImpactObject.setSubSystem(subSys);      
				entityImpactObject.setIdPlan(idPlan);
				entityImpactObject.setNumOp(Integer.parseInt(numOp));      
				entityImpactObject.setTypeObjectOrigin(EnumObject.values()[Integer.parseInt(typeObjectOrigin.trim())]);
				entityImpactObject.setIdObjectOrigin(idObjectOrigin);
				entityImpactObject.setTypeObjectTarget(eoWhereUsed.getTypeObjectRefer());
				entityImpactObject.setIdObjectTarget(eoWhereUsed.getIdObjectRefer());		
				entityImpactObject.setRowStart(eoWhereUsed.getRowStart());
				entityImpactObject.setRowEnd(eoWhereUsed.getRowEnd());
				entityImpactObject.setNumInstr(eoWhereUsed.getNumInstrRefer());
				entityImpactObject.setCobolDivision(EnumCobolReservedWords.PROC_DIVISION);
				
				al_entityObjectPlan.add(entityImpactObject);
			}						
		}
				
		String statusRet = "";
        @SuppressWarnings("unused")
		int[] isInserted = null;
        
		try {
			impactObjectDAO =  (DAOImplImpactObject) sqlFactory.getDAOImpactObject(conn, true, true, ucfg);
			isInserted = impactObjectDAO.createBulk(al_entityObjectPlan);
			conn.commit();
			statusRet="OK";
			
		} catch (Exception e) {
			statusRet="KO"+e.getMessage();
		}		
		
		DataBaseConnections.releaseConnection(conn);
		impactObjectDAO.setConn(null);
		whereUsedDAO.setConn(null);
		
	    return Response
 				.status(200)
				.entity(statusRet)     // OK or KO+Message
				.build();
	}	

	/*
	 * Gestione CREATE Entity ImpactObject righe multiple
     *
	 */
	@POST
	@Path("createImpactCompile/{user}/{sys}/{subSys}/{idPlan}")  
	public Response createImpactCompile(
								      @PathParam("user")  	  			String user      
								     ,@PathParam("sys")  	  			String sys  		 
								     ,@PathParam("subSys")    			String subSys   	 
								     ,@PathParam("idPlan")          	String idPlan   	 
 								     ,@FormParam("liRow")          		List<String> liRow   								     
									) throws ExceptionAmrita, SQLException {

		UserConfiguration ucfg = null;
		List<EntityImpactCompile> al_entityImpactCompile = null;
		EntityImpactCompile entityImpactCompile =null;
        String statusRet = "";
		String arVal[] = null;
	    String typeObjectCompile = "";   	 
	    String idObjectCompile = "";
        @SuppressWarnings("unused")
		int[] isInserted = null;;
        
       
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
	    al_entityImpactCompile = new ArrayList<EntityImpactCompile>();		
		
		// Scan rows
		for (String rowVal : liRow) {
			
			arVal = rowVal.split(":");
			
			typeObjectCompile = arVal[0];
			idObjectCompile = arVal[1];	
			
			entityImpactCompile=new EntityImpactCompile();
			entityImpactCompile.setSystem(sys);      
			entityImpactCompile.setSubSystem(subSys);      
			entityImpactCompile.setIdPlan(idPlan);
			entityImpactCompile.setTypeObjectCompile(EnumObject.values()[Integer.parseInt(typeObjectCompile.trim())]);
			entityImpactCompile.setIdObjectCompile(idObjectCompile);
			
			al_entityImpactCompile.add(entityImpactCompile);
		}
		
		try {
			MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
			Connection conn = DataBaseConnections.getConnection();
			IDAOImpactCompile impactPlanDAO =  (DAOImplImpactCompile) sqlFactory.getDAOImpactCompile(conn, true, true, ucfg);
			isInserted = impactPlanDAO.createBulk(al_entityImpactCompile);
			conn.commit();
			statusRet="OK";
		} catch (Exception e) {
			statusRet="KO"+e.getMessage();
		}
		
	    return Response
 				.status(200)
				.entity(statusRet)     // OK or KO+Message
				.build();
	}	
    
	/*
	 * Restituisce gli impact plan definiti nel sistema.
	 * Gli impact plan SONO indipendenti dal sottosistema
	 */
	@GET  
	@Produces("application/json")
	@Path("impactPlanGrouped/{user}/{sys}")  
	public Response getImpacPlanGrouped(
										  @PathParam("user")     String user
										, @PathParam("sys")      String sys
							
										) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		conn.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		ls_object=dao.sqlImpactPlan(sys);
		
	    jsonArray = new JSONArray();

		// Populate Json
		for (EntitySqlGeneric eo : ls_object) {
			jsonObject = new JSONObject();	 			
	 		jsonObject.put("impactPlan", eo.getIdObject()); 		
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);		

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}		

	/*
	 * Restituisce il count dei programmi contenenti where used di un campo.
	 * == NON USATO ==
	 */
	@GET  
	@Produces("application/json")
	@Path("impacPlanPgmCount/{user}/{sys}/{idPlan}/{idObject}/{typeObject}/{idField}")  
	public Response getImpacPlanPgmCount(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("idPlan")  	 String idPlan         // Non usato, per usi futuri (*)
			, @PathParam("idObject")     String idObject
			, @PathParam("typeObject")   String typeObject
			, @PathParam("idField")      String idField

			) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		ls_object=dao.sqlImpacPlanPgmCount(sys, idPlan, idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())], idField);
		
		// Populate Json
		jsonObject = new JSONObject();	 			
		jsonObject.put("pgmCount", ls_object.get(0).getCnt()); 	
		resultJson = jsonObject.toString();		

		DataBaseConnections.releaseConnection(conn);		

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}	

	/*
	 * Restituisce la lista dei programmi contenenti where used di un campo
	 * Viene fornito l'elenco degli oggetti origine e del corrispondente campo da trattare
	 */
	@POST  
	@Produces("application/json")
	@Path("impactPlanPgmList/{user}/{sys}/{idPlan}")  
	public Response getimpactPlanPgmList(
										  @PathParam("user")         String user
										, @PathParam("sys")          String sys
										, @PathParam("idPlan")  	 String idPlan          // NON usato, per usi futuri (*)
										, @FormParam("liRow")        List<String> liRow     // idObject|typeObject|idField!idObject|typeObject|idField!......
										
										) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null;
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_objectWrk = null;
		List<EntitySqlGeneric> ls_object = null;
		String arVal[] = null;
		String idObjectOrigin = "";
		String typeObjectOriginOrdinal = "";
		String numOp = "";
		String fieldColumn = "";
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		
	    jsonArray = new JSONArray();
	    ls_object = new ArrayList<EntitySqlGeneric>();
	    
		// Accum pgms for every change
		for (String rowVal : liRow) {
			arVal = rowVal.split(":");
			numOp = arVal[0];
			idObjectOrigin = arVal[1];
			typeObjectOriginOrdinal = arVal[2];
			fieldColumn = arVal[3]; 
  
			ls_objectWrk=dao.sqlImpactPlanPgmList(sys, idPlan, idObjectOrigin, EnumObject.values()[Integer.parseInt(typeObjectOriginOrdinal.trim())], fieldColumn);
	        
			// Update numero operazione, si sfrutta il campo cnt
			for (EntitySqlGeneric eo : ls_objectWrk) {
				eo.setCnt(Integer.parseInt(numOp));
			}
			
			ls_object.addAll(ls_objectWrk);
		}
	    
		// Populate Json
		for (EntitySqlGeneric eo : ls_object) {
			jsonObject = new JSONObject();	 			
	 		jsonObject.put("subSys", eo.getSubSystem()); 		
	 		jsonObject.put("numOp", eo.getCnt()); 		
	 		jsonObject.put("pgm", eo.getPgm()); 		
	 		jsonObject.put("typeObjectPgm", eo.getTypeObjectA()); 		
	 		jsonObject.put("typeObjectPgmOrdinal", eo.getTypeObjectAOrdinal()); 		
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);		

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}	
	
	
	/*
	 * Restituisce il count dei where used di un campo
	 * 
	 * == NON USATO ==
	 * 
	 * N.B. I whereUsed di dettaglio vengono restituiti con il web service standard a livello di Entity
	 */
	@GET  
	@Produces("application/json")
	@Path("impacPlanWhereUsedCount/{user}/{sys}/{idPlan}/{idObject}/{typeObject}/{idField}")  
	public Response getImpacPlanWhereUsedCount(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("idPlan")  	 String idPlan         // Non usato, per usi futuri (*)
			, @PathParam("idObject")     String idObject
			, @PathParam("typeObject")   String typeObject
			, @PathParam("idField")      String idField

			) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		ls_object=dao.sqlImpacPlanWhereUsedCount(sys, idPlan, idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())], idField);
		
		// Populate Json
		jsonObject = new JSONObject();	 			
		jsonObject.put("pgmCount", ls_object.get(0).getCnt()); 	
		resultJson = jsonObject.toString();		

		DataBaseConnections.releaseConnection(conn);		

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}		
	
		
	/*
	 *  3) Restituisce il dettaglio di tutte le istruzioni di tutti i programmi interessati da un impact plan
	 *  
	 *  == NON USATO ==
	 */
	@GET  
	@Produces("application/json")
	@Path("impactPlanPgmDetailAll/{user}/{sys}/{idPlan}")  
	public Response getImpacPlanPgmDetailAll(
										  @PathParam("user")     		String user
										, @PathParam("sys")      		String sys
									     ,@PathParam("idPlan")   		String idPlan           // Nome piano di impatto	 
									     ,@PathParam("idObject")  		String idObject         // Nome copy/Entity/  	 
									     ,@PathParam("typeObject")  	String typeObject       // Tipo oggetto copy/entity 
									     ,@PathParam("idField")  		String idField          // Campo/colonna	 
									     ,@PathParam("idObjectRefer")	String idObjectRefer    // Pgm dove idField è referenziato	 
							
										) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		ls_object=dao.sqlImpactPlan(sys);
		
	    jsonArray = new JSONArray();

		// Populate Json
		for (EntitySqlGeneric eo : ls_object) {
			jsonObject = new JSONObject();	 			
	 		jsonObject.put("impactPlan", eo.getIdObject()); 		
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);		

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}	
	

	/*
	 * Restituisce la lista dei programmi da ricompilare a front di un impact plan.
	 * Si ottengono i programmi dalle relazioni inverse in input.
	 */
	@POST  
	@Produces("application/json")
	@Path("impactPlanPgmToRecompile/{user}/{sys}")  
	public Response getimpactPlanPgmToRecompile(
										  @PathParam("user")         String user
										, @PathParam("sys")          String sys
										, @FormParam("liRow")        List<String> liRow     // subSys:relation:idObjectB:typeObjectB subSys:relation:idObjectB:typeObjectB:......
										
										) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null;
		UserConfiguration ucfg = null;
		Set<String> set_pgm = null;
		List<EntityRelation> ls_relation = null;
		String arVal[] = null;
		String subSys = "";                    // non utilizzato
		String relation = "";
		String idObjectB = "";
		String typeObjectB = "";               // non utilizzato
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		Connection conn = DataBaseConnections.getConnection();
		IDAORelation dao = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false, false, ucfg);
	    
		
	    jsonArray = new JSONArray();
	    set_pgm = new HashSet<String>();
	    
		// Accum pgms da portare in output
		for (String rowVal : liRow) {
			arVal = rowVal.split(":");
			subSys = arVal[0];        // non utilizzato
			relation = arVal[1];
			idObjectB = arVal[2];
			typeObjectB = arVal[3];   // Non utilizzato
	       
			ls_relation=dao.findAllBySubSysIdBRel(sys, idObjectB, EnumRelation.values()[Integer.parseInt(relation.trim())]);
			
			// Porto programmi relazione inversa in output
			for (EntityRelation eo : ls_relation) {
				if (!set_pgm.contains(eo.getIdObjectA())) {
					set_pgm.add(eo.getIdObjectA());
				}
				set_pgm.add(eo.getIdObjectA());
			}
			
		}
	    
		// Populate Json
		for (String pgmName : set_pgm) {
			jsonObject = new JSONObject();	 			
	 		jsonObject.put("pgmName", pgmName); 		
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);		

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}	
	
	///////////////////////////////////////////////////////////////////////////////////////
    // Web Services di supporto all'esecuzione
	///////////////////////////////////////////////////////////////////////////////////////	
	
	/*
	 * Attivazione processo di analisi/elaborazione Impact Plan deep
	 * 
	 */
	@PUT 
	@Produces("application/json")
	@Path("executionImpactStart/{user}/{sys}/{idPlan}")  
	public Response executionImpactStart (
									      @PathParam("user")  	  String user      
									     ,@PathParam("sys")  	  String sys  		  	 
									     ,@PathParam("idPlan")    String idPlan
									    ) throws ExceptionAmrita  {

		ExecutionDirectives di = null;                       // Al momento non gestito, da valorizzare con eventuali opzioni passate dal client		
		UserConfiguration ucfg = null;

		String result="OK";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		di = new ExecutionDirectives(ucfg);
		ucfg.setSystemOwner(sys);
		
		try {
			executionImpactStart01(ucfg, sys, idPlan, di);
		} catch (Exception e) {
			result="KO"+e.getMessage();
		}
		
		return Response
				.status(200)
				.entity(result)
				.build();
	}
    
	/* Gestione attivazione processo di deep impact analysis */
	private void executionImpactStart01(UserConfiguration ucfg, String sys, String idPlan, ExecutionDirectives di) throws InterruptedException {
		ProcessImpactAnalysis processImpactAnalysis = null;
		Thread threadImpact = null;
		
		// Tempi di esecuzione
		long timeMsStart = 0;						// Inizio elaborazione
		long timeMsEnd = 0;							// Fine elaborazione
		long timeMsTotal = 0;						// Durata in millisecondi
		
		// Di servizio
		String[] arParm = null;						// Parametri per errori
		
		//////////////////////////////////////////////////////////////////////////
        // Attivazione gestore generalizzato processi e funzioni 
		//////////////////////////////////////////////////////////////////////////
		
		timeMsStart = System.currentTimeMillis();
		ucfg.setExecMsAllElapsed(0);
		ucfg.setExecMsAllStart(timeMsStart);
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0013", null, null); // Inizio elaborazione processi ...
		
		processImpactAnalysis = new ProcessImpactAnalysis(ucfg, idPlan, di);
		threadImpact = new Thread(processImpactAnalysis, "THREADIMPACT");

		threadImpact.start();                 // Attivazione processo		
		
		

		threadImpact.join();             // Wait until processing ended

		timeMsEnd = System.currentTimeMillis();
		timeMsTotal = timeMsEnd - timeMsStart;
		
		ucfg.setExecMsAllEnd(timeMsEnd);
		ucfg.setExecMsAllElapsed(timeMsTotal);
		if (ucfg.getExecCntObjectProcessed() > 0) {
			ucfg.setExecMsAvg((int)timeMsTotal / ucfg.getExecCntObjectProcessed()); 
		} else {
			ucfg.setExecMsAvg(0); 
		}
		
		
		//////////////////////////////////////////////////////////////////////////
        // Scrittura su log informazioni di chiusura e consuntivo
		//////////////////////////////////////////////////////////////////////////
        
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0014", null, null); // Fine elaborazione processi ...
		Long timeElapsed = new Long(timeMsTotal);
		arParm = new String[1];
		arParm[0] = timeElapsed.toString();
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0015", arParm, null); // Elapsed totale
		
	}

	/*
	 * Stop elaborazione impact
	 */
	@PUT 
	@Produces("application/json")
	@Path("executionImpactStop/{user}")  
	public Response executionImpactStopWeb (
			  @PathParam("user")         String user
			) throws JSONException, SQLException, ExceptionAmrita {

		UserConfiguration ucfg = null;

		String result="OKStop Request Processd Correctly";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
			result="KOLogin Shoud Be Done";
		} else {
			ucfg.setExecStopRequired(true);
		}
		
		return Response
				.status(200)
				.entity(result)
				.build();
	}
	/*
	 * Restituisce le informazioni correnti di esecuzione a livello di utente
	 * Le informazioni restituite arrivano dall'oggetto UserConfiguration
	 */
	@GET
	@Produces("application/json")
	@Path("executionImpactInfo/{user}")  
	public Response executionImpactInfo(@PathParam("user") String user) throws JSONException, SQLException, ExceptionAmrita {

		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		String result="OK";
		jsonObject = new JSONObject();
		
		// Login should be done
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
			 result="KOLogin Required";
		} else {
			jsonObject.put("execProcess", ucfg.getExecProcess()); 									// Tipo Processo/funzione in esecuzione
			jsonObject.put("execProcessRunning", ucfg.getExecProcessRunning()); 					// Il processo di elaborazione/analisi è in esecuzione
			jsonObject.put("execStopRequired", ucfg.getExecStopRequired()); 						// Richiesto stop al processo dal Web
			jsonObject.put("execCurIdObject", ucfg.getExecCurIdObject()); 							// Oggento corrente in elaborazione
			jsonObject.put("execTotObjectToProcess", ucfg.getExecTotObjectToProcess()); 			// Numero totale oggetti in analisi/elaborazione
			jsonObject.put("execCntObjectProcessed", ucfg.getExecCntObjectProcessed());				// Counter oggetti elaborati (include quello corrente)
			jsonObject.put("execCntObjectProcessedError", ucfg.getExecCntObjectProcessedError());	// Counter oggetti processati con errori
			jsonObject.put("execCntObjectProcessedExcp", ucfg.getExecCntObjectProcessedExcp()); 	// Counter oggetti processati terminati da exception
			jsonObject.put("execMsAvg", ucfg.getExecMsAvg()); 										// Millisecondi medio elaborazione oggetti
			jsonObject.put("execMsMax", ucfg.getExecMsMax()); 										// Millisecondi massimo elaborazione oggetti
			jsonObject.put("execMsMin", ucfg.getExecMsMin()); 										// Millisecondi massimo elaborazione oggetti
			jsonObject.put("execMsMaxIdObject", ucfg.getExecMsMaxIdObject()); 						// Oggetto con Millisecondi massimo elaborazione 
			jsonObject.put("execMsMinIdObject", ucfg.getExecMsMinIdObject()); 						// Oggetto con Millisecondi massimo elaborazione 
			jsonObject.put("execMsCurExpectedEnd", ucfg.getExecMsCurExpectedEnd()); 				// Millisecondi attesi alla fine corrente elaborazione
			jsonObject.put("execMsAllExpectedEnd", ucfg.getExecMsAllExpectedEnd()); 				// Millisecondi attesi alla fine tutte le elaborazione
			jsonObject.put("execMsAllElapsed", ucfg.getExecMsAllElapsed()); 						// Millisecondi Elapsed totale
			jsonObject.put("execMsCurElapsed", ucfg.getExecMsCurElapsed()); 						// Millisecondi elaborazione oggetto corrente			
			jsonObject.put("execProcessOrdinal", ucfg.getExecProcess().ordinal());
	    }

		// Return json object
		result = jsonObject.toString();		

		return Response
				.status(200)
				.entity(result)
				.build();
	}

}

