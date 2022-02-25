package rest;
import java.io.File;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

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
import analyzer.DataBaseConnections;
import analyzer.ExecutionStarterWeb;
import analyzer.SourceManager;
import analyzer.UserActive;
import analyzer.UserConfiguration;
import dao.DAOFactory;
import dao.IDAOUser;
import dao.MySQLDAOFactory;
import dao.DAOImplUser;
import entities.EntityUser;
import exception.ExceptionAmrita;

/*
 * 
 * Classe con web service di servizio per:
 * 
 * 1) Gestione pilots di esecuzione (get/save/delete)
 * 2) Attivazione processi e funzioni
 * 
 */

@Path("/")   
public class RestAnalyzerService implements AmritaConstants {

	
	@GET
	@Path("loginUser/{user}")  
	public Response loginUser(
						      @PathParam("user")  	  String user  
							) throws ExceptionAmrita, SQLException {

		UserConfiguration ucfg = null;
        JSONObject jsonObject = null; 
        String resultJson = "";
		
		// If  User notfound get from default config or user table and add user to active users
        // If  User found just returns the user from active table
		ucfg = UserActive.addUser(user);			

		jsonObject = new JSONObject();
        populateJsonUserFromConfig(ucfg, jsonObject);
        resultJson = jsonObject.toString();	

	    return Response
				.status(200)
				.entity(resultJson)     // OK or KO+Message
				.build();
	}

	private void populateJsonUserFromConfig(UserConfiguration ucfg, JSONObject jsonObject) {
		// User Data 
 		jsonObject.put("isUserDefinedOnDb", ucfg.isUserDefinedOnDb());                           
		jsonObject.put("baseUrl", ucfg.getBaseUrl());                           
		jsonObject.put("companyCode", ucfg.getCompanyCode()); 							 
		jsonObject.put("company", ucfg.getCompany()); 									 
		jsonObject.put("userType", ucfg.getUserType());            			 
		jsonObject.put("userStatus", ucfg.getUserStatus());              	  
		jsonObject.put("language", ucfg.getLanguage());            					 
		jsonObject.put("pwd", ucfg.getPwd());          								 
		jsonObject.put("country", ucfg.getCountry()); 									 
		jsonObject.put("mail", ucfg.getMail()); 											 
		jsonObject.put("mailInfo", ucfg.getMailInfo()); 									 										  
		jsonObject.put("referManager", ucfg.getReferManager()); 							  
		jsonObject.put("referTech", ucfg.getReferTech()); 								 
		jsonObject.put("analyzerEnabled", ucfg.isAnalyzerEnabled()); 					  
		jsonObject.put("viewerEnabled", ucfg.isViewerEnabled()); 						 
		jsonObject.put("inspectorEnabled", ucfg.isInspectorEnabled()); 					 
		jsonObject.put("assesmentEnabled", ucfg.isAssesmentEnabled()); 					  

	    jsonObject.put("pathConfigFile", ucfg.getPathConfigFile());                        
	    jsonObject.put("pathRoot", ucfg.getPathRoot());                                	 
	    jsonObject.put("pathUser", ucfg.getPathUser());                                	 
	    jsonObject.put("pathPilot", ucfg.getPathPilot());                               	  
	 	
		// Directories relative a root o WEB-INF da file di configurazione generale
		jsonObject.put("dirResources", ucfg.getDirResources());                            
		jsonObject.put("dirWork", ucfg.getDirWork());                                 	 
		jsonObject.put("dirDatabase", ucfg.getDirDatabase());                                             
		jsonObject.put("dirJclInput", ucfg.getDirJclSrc());                                   
		jsonObject.put("dirCobolSrcPgmInput", ucfg.getDirCobolSrcPgm());               
		jsonObject.put("dirCobolSrcCopyInput", ucfg.getDirCobolSrcCopy());             
		jsonObject.put("dirCobolPgm", ucfg.getDirCobolObjPgm());                               		 
		jsonObject.put("dirCobolCopy", ucfg.getDirCobolObjCopy());                             
		jsonObject.put("dirJcl", ucfg.getDirJclObj());                                  		  
		jsonObject.put("dirSqlScript", ucfg.getDirSqlScript());                             	 
		jsonObject.put("dirCobolGraph", ucfg.getDirCobolGraph());                           
		jsonObject.put("dirPilot", ucfg.getDirPilot());                                	 
		jsonObject.put("dirLog", ucfg.getDirLog());                                  		 
		jsonObject.put("dirOutput", ucfg.getDirOutput());                              	 

		// Ottimizzazione processi ed elaborazioni, allocazione di arrays, collections, map
		jsonObject.put("limitMaxLinesScanFindingSourceType", ucfg.getLimitMaxLinesScanFindingSourceType());  
		jsonObject.put("limitMaxSources", ucfg.isLimitMaxSources());                    
		jsonObject.put("limitMaxSourcesInput", ucfg.getLimitMaxSourcesInput());                
		jsonObject.put("limitMaxSourcesToProcess", ucfg.getLimitMaxSourcesToProcess());        
		jsonObject.put("limitMaxObjects", ucfg.isLimitMaxObjects());                    
		jsonObject.put("limitMaxObjectsInput", ucfg.getLimitMaxObjectsInput());                
		jsonObject.put("limitMaxObjectsToProcess", ucfg.getLimitMaxObjectsToProcess());        
		jsonObject.put("debugThresholdMemoryGarbage", ucfg.getDebugThresholdMemoryGarbage()); 
		jsonObject.put("debugSourcesDetectedFreqGarbage", ucfg.getDebugSourcesDetectedFreqGarbage());  
		jsonObject.put("debugActive", ucfg.isDebugActive());                             
		jsonObject.put("logVerbose", ucfg.isLogVerbose());                               
		jsonObject.put("preferredVisitMethod", ucfg.getPreferredVisitMethod());            
		jsonObject.put("preferredCachingLevel", ucfg.getPreferredCachingLevel());         
		jsonObject.put("preferredCachingSupport", ucfg.getPreferredCachingSupport());      
		
		//  Database
		jsonObject.put("dataBaseType", ucfg.getDataBaseType());                             
		jsonObject.put("dataBaseName", ucfg.getDataBaseName());                             
		jsonObject.put("dataBaseUser", ucfg.getDataBaseUser());                             
		jsonObject.put("dataBasePwd", ucfg.getDataBasePwd());                              
		jsonObject.put("dataBaseDriver", ucfg.getDataBaseDriver());                        
		jsonObject.put("dataBaseAccessType", ucfg.getDataBaseAccessType());                
		jsonObject.put("dataBaseUrl", ucfg.getDataBaseUrl());                              
		jsonObject.put("dataBaseMaxConn", ucfg.getDataBaseMaxConn());                          
		jsonObject.put("dataBaseCommitBlockUpdates", ucfg.getDataBaseCommitBlockUpdates());    
		jsonObject.put("dataBaseLogAnySql", ucfg.isDataBaseLogAnySql());                 
	    	
		// Controllo analisi, identificazione oggetti analizzati/processati, piloti sources, filtri e processi'
		jsonObject.put("pilotDefaultSource", ucfg.getPilotDefaultSource());                 
		jsonObject.put("pilotDefaultProcess", ucfg.getPilotDefaultProcess());               
		jsonObject.put("userExitClass", ucfg.getUserExitClass());                           
				
		jsonObject.put("userTypetOrdinal", ucfg.getUserType().ordinal());
		jsonObject.put("userStatusOrdinal", ucfg.getUserStatus().ordinal());
	}	
	
	@GET  
	@Produces("application/json")
	@Path("pilotList/{user}/{typePilot}")  
	public Response getPilotNames(
			  @PathParam("user")         String user
			, @PathParam("typePilot")    String typePilot

			) throws JSONException, SQLException, ExceptionAmrita {

			String resultJson=getResultPilotListJsonObject(user, typePilot);

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}


	private String getResultPilotListJsonObject(String user, String typePilot ) throws  JSONException, SQLException, ExceptionAmrita {
		
		UserConfiguration ucfg = null;
        SourceManager sm = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		String[] pilotNames = null;;
		String pathPilot = "";
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
	    pathPilot=ucfg.getPathUser() + File.separator + ucfg.getDirPilot();
	    
	    // Get the list of pilots names
        sm = new SourceManager(ucfg);
		
        pilotNames =sm.listFiles(pathPilot);


		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (String pilotName : pilotNames) {
			jsonObject = new JSONObject();
			jsonObject.put("pilotName", pilotName);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		return result;
	}  

	@GET  
	@Produces("application/json")
	@Path("pilotSource/{user}/{pilotName}")  
	public Response getPilotSource(
			  @PathParam("user")         String user
			, @PathParam("pilotName")    String pilotName

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultPilotSourceJsonObject(user, pilotName);


		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@POST
	@Path("savePilot/{user}/{filename}/{replace}")  
	public Response savePilot(
							  @FormParam("rowPilot")  List<String> rowsPilot
						     ,@PathParam("user")  	  String user      
						     ,@PathParam("filename")  String filename  // With no suffix
						     ,@PathParam("replace")   String replace   // yes/no
							) throws ExceptionAmrita {

		UserConfiguration ucfg = null;
	    SourceManager sm = null;
		String pathPilot="";
		String pathPilotSource = "";
		String pilotName="";
		String statusSave = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		 
	    pathPilot=ucfg.getPathUser() + File.separator + ucfg.getDirPilot();
	    pilotName=filename + ".pilot";
	    
	    // Save the pilot
	    sm = new SourceManager(ucfg);	
	    pathPilotSource=pathPilot + File.separator + pilotName;
	    statusSave =sm.putFile(pathPilotSource, rowsPilot);  

	    return Response
 				.status(200)
				.entity(statusSave)     // OK or KO+Message
				.build();
	}

	@DELETE
	@Path("deletePilot/{user}/{filename}")  
	public Response deletePilot(
						      @PathParam("user")  	  String user      
						     ,@PathParam("filename")  String filename  // With suffix
							) throws ExceptionAmrita {

		UserConfiguration ucfg = null;
	    SourceManager sm = null;
		String pathPilot="";
		String pathPilotSource = "";
		String pilotName="";
		String statusSave = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		pathPilot=ucfg.getPathUser() + ucfg.getDirPilot();
	    pilotName=filename + ".pilot";
	    
	    // Delete the pilot
	    sm = new SourceManager(ucfg);	
	    pathPilotSource=pathPilot + File.separator + pilotName;
	    statusSave =sm.deleteFile(pathPilotSource);  

	    return Response
				.status(200)
				.entity(statusSave)     // OK or KO+Message
				.build();
	}
	
	private String getResultPilotSourceJsonObject(String user, String pilotName ) throws ExceptionAmrita, SQLException  {
		
		UserConfiguration ucfg = null;
	    SourceManager sm = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<String> pilotRows = null;;
		String pathPilotSource = "";
		String result = "";
	
		ucfg = UserActive.getUser(user);
		// User notfound: get from default config or user table and add user to active users
		if (ucfg == null) {
			ucfg = UserActive.addUser(user);			
		}
	    
	    // Get the list of pilots names
	    sm = new SourceManager(ucfg);
		
	    pathPilotSource=ucfg.getPathUser() + File.separator + ucfg.getDirPilot() + File.separator + pilotName;
	    pilotRows =sm.getFile(pathPilotSource);
	    
		// No data
		jsonArray = new JSONArray();
	
		// Populate Json
		for (String row : pilotRows) {
			jsonObject = new JSONObject();
			jsonObject.put("pilotRow", row);
			jsonArray.put(jsonObject);
		}
	
		// Return json object
		result = jsonArray.toString();		
	
		return result;
	}  

	
	@POST
	@Path("saveUser/{user}")  
	public Response saveUser(
		      				 @PathParam("user") 			String user      
							,@FormParam("baseUrl")  	    String baseUrl
							,@FormParam("companyCode")  	String companyCode
						    ,@FormParam("company")  	 	String company      
//		      				,@FormParam("userType")    		String userType           			     		// Tipologia utente forward (T052)
//		      				,@FormParam("userStatus")   	String userStatus            	 				// Stato utente forward (T051)
		      				,@FormParam("language")     	String language            					 	// Liguaggio in formato Locale ("en", "it", ... )
		      				,@FormParam("pwd") 	      		String pwd         								// Password
		      				,@FormParam("country")      	String country							 		// Country IT, .. 
		      				,@FormParam("mail")      		String  mail		 							// Mail principale  
		      				,@FormParam("mailInfo") 	  	String mailInfo							 		// Mail per info anomali  
		      				,@FormParam("phone") 			String phone								 	// Telefono di riferimento  
		      				,@FormParam("referManager") 	String referManager					 			// Riferimento manager 
		      				,@FormParam("referTech") 	  	String referTech					 			// Riferimento tecnico 
		      				,@FormParam("analyzerEnabled") 	String analyzerEnabled			    			//  
		      				,@FormParam("viewerEnabled") 	String viewerEnabled							//  
		      				,@FormParam("inspectorEnabled") String inspectorEnabled							//  
		      				,@FormParam("assesmentEnabled") String assesmentEnabled							//  
		      				,@FormParam("countLogin")   	String countLogin								// Counter login effettuati       			
		      				,@FormParam("dtActivation") 	String dtActivation  							// Data attivazione AAAAMMGG'
		      				,@FormParam("tmActivation") 	String tmActivation  							// Ora  attivazione HHMMSSCC'
		      				,@FormParam("dtExpiration") 	String dtExpiration  							// Data disattivazione AAAAMMGG'
		      				,@FormParam("tmExpiration") 	String tmExpiration  							// Ora  disattivazione HHMMSSCC'
// 		      				,@FormParam("dtFirstLogin") 	String dtFirstLogin  							// Data primo login AAAAMMGG'
//		      			    ,@FormParam("tmFirstLogin")		String tmFirstLogi  							// Ora  primo login HHMMSSCC	'
// 		      				,@FormParam("dtLastLogin")  	String dtLastLogin								// Data ultimo login AAAAMMGG'
// 		      				,@FormParam("tmLastLogin")  	String tmLastLogin 								// Ora  ultimo login HHMMSSCC'	

		      				// Directories relative a root o WEB-INF da file di configurazione generale
		      			    ,@FormParam("pathConfigFile") 	String pathConfigFile                  			// NOT NULL COMMENT 'Path completo file di configurazione'
		      			    ,@FormParam("pathRoot")       	String pathRoot                        			// NOT NULL COMMENT 'Phisical path to WEB-INF or application Installation'
		      			    ,@FormParam("pathUser")       	String pathUser                        			// NOT NULL COMMENT 'Phisical path to user  directory ...users/{user}'
		      			    ,@FormParam("pathPilot")      	String pathPilot                       			// Phisical path to pilot directory ...users/{user}'
		      			 	
		      			    ,@FormParam("dirResources")   	String dirResources                    			//Resource'
		      			    ,@FormParam("dirWork") 			String dirWork                         			// NOT NULL COMMENT 'Working e temporaneo'		    
		      			    ,@FormParam("dirDatabase") 		String dirDatabase                    			// Database'		              
		      			    ,@FormParam("dirJclInput") 		String dirJclInput                     			// Jcl    in input al processo di analisi (.*) '       
		      			    ,@FormParam("dirCobolSrcPgmInput") 	String dirCobolSrcPgmInput             		// Pgm    Cobol sorgenti in analisi		  (.*) '
		      			    ,@FormParam("dirCobolSrcCopyInput") String dirCobolSrcCopyInput            		// Copy   Cobol sorgenti in analisi		  (.*)'
		      			    ,@FormParam("dirCobolPgm") 			String dirCobolPgm                     		// Pgm    Cobol codificati e serializzati (.program)'			 
		      			    ,@FormParam("dirCobolCopy") 		String dirCobolCopy                    		// Copy   Cobol codificati e serializzati (.copy)'	
		      			    ,@FormParam("dirJcl") 				String dirJcl                          		// Jcl codificati e serializzati (.jclSourcedocument.getElementById("X").value=objConfig. .jclIncludedocument.getElementById("X").value=objConfig. .jclProc)'		 
		      			    ,@FormParam("dirSqlScript") 		String dirSqlScript                    		// Script Sql codificati e serializzati   (.scriptSql)'			 
		      			    ,@FormParam("dirCobolGraph")		String dirCobolGraph                   		// Grafi  Cobol codificati e serializzati (.graph)'	
		      			    ,@FormParam("dirPilot") 			String dirPilot                        		// Pilot  sources e processi e filtri     (.pilot)'
		      			    ,@FormParam("dirLog") 				String dirLog                          		// Log
		      			    ,@FormParam("dirOutput") 			String dirOutput                       		// Output per funzioni che producono text'

		      				// Ottimizzazione processi ed elaborazioni, allocazione di arrays, collections, map
		      				,@FormParam("limitMaxLinesScanFindingSourceType") String limitMaxLinesScanFindingSourceType  // Numero massimo righe da analizzare per individuare il tipo sorgente (200)'
		      				,@FormParam("limitMaxSources") 			String  limitMaxSources           		// Abilitazione limitazione ai sources trattati'
		      				,@FormParam("limitMaxSourcesInput") 	String limitMaxSourcesInput        		// Numero massimo sorgenti da considerare in input'
		      				,@FormParam("limitMaxSourcesToProcess") String limitMaxSourcesToProcess    		// Numero massimo sorgenti in input dei quali è stato intercettato il tipo'
		      				,@FormParam("limitMaxObjects") 			String  limitMaxObjects            		// Abilitazione limitazione agli oggetti processati'
		      				,@FormParam("limitMaxObjectsInput")	 	String limitMaxObjectsInput        		// Numero massimo oggetti da considerare in input ai processi (filtrati)'
		      				,@FormParam("limitMaxObjectsToProcess") String limitMaxObjectsToProcess    		// Numero massimo oggetti in input da processare'
		      				,@FormParam("debugThresholdMemoryGarbage") String debugThresholdMemoryGarbage 	// Attivazione gc() se memoria disponibile <'
		      				,@FormParam("debugSourcesDetectedFreqGarbage") String debugSourcesDetectedFreqGarbage // gc() attivata ogni 200 sorgenti'
		      				,@FormParam("debugActive")				String debugActive                 		// Attivazione debug dove previsto (messaggi log di debug)'
		      				,@FormParam("logVerbose") 				String logVerbose                  		// Dettaglio log operazioni Sql e informative'
		      				,@FormParam("preferredVisitMethod")		String preferredVisitMethod        		// Metodo di visita predefinito (BACKWARD)'
		      				,@FormParam("preferredCachingLevel") 	String preferredCachingLevel       		// Livello di caching predefinito (CACHING_PATH_ALL)'
		      				,@FormParam("preferredCachingSupport")  String preferredCachingSupport      	 	// Tipo supporto java di cache (CACHING_ON_HASH_MAP, CACHING_ON_TREE_MAP)'
		      					
		      				//  Database
		      				,@FormParam("dataBaseType") 			String dataBaseType                    	// Tipologia database MSACCESS/MYSQL'
		      				,@FormParam("dataBaseName") 			String dataBaseName                    	// Nome database (DbAmrita)'
		      				,@FormParam("dataBaseUser") 			String dataBaseUser                    	// User (GZEDDA)'
		      				,@FormParam("dataBasePwd")				String dataBasePwd                     	// Pwd (giampietro4)'
		      				,@FormParam("dataBaseDriver") 			String dataBaseDriver                  	// Driver (com.mysql.cj.jdbc.Driver)'
		      				,@FormParam("dataBaseAccessType") 		String dataBaseAccessType             	// Accesso LOCAL/REMOTE'
		      				,@FormParam("dataBaseUrl") 				String dataBaseUrl                     	// jdbc:mysql://localhost:3306/Amrita?autoReconnect=true&useSSL=false&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC'		 	
		      				,@FormParam("dataBaseMaxConn") 			String dataBaseMaxConn                 	// Numero massimo connessioni attive (1)'
		      				,@FormParam("dataBaseCommitBlockUpdates") String dataBaseCommitBlockUpdates  	// Commit automatica a fine gruppo aggiornamenti (100)'
		      				,@FormParam("dataBaseLogAnySql") 		String dataBaseLogAnySql  			 	// Log istruzioni Sql come messaggi informativi
		      					
		      				// Controllo analisi, identificazione oggetti analizzati/processati, piloti sources, filtri e processi'
		      				,@FormParam("pilotDefaultSource") 		String pilotDefaultSource              	// Pilota sorgenti (PilotDefaultSource.pilot)'
		      				,@FormParam("pilotDefaultProcess") 		String pilotDefaultProcess             	// Pilota processi (PilotDefaultProcess.pilot)'
		      				,@FormParam("userExitClass") 			String userExitClass                   	// Classe con exit applicative codificate (UserExit)'   
			) throws ExceptionAmrita, SQLException {

		UserConfiguration ucfg = null;
		EntityUser entityUser =null;
        String statusSave = "";
        boolean userUpdated = false;
        
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOUser userDAO =  (DAOImplUser) sqlFactory.getDAOUser(conn, true, true, ucfg);


		entityUser=new EntityUser();
		entityUser.setUser(user);      
		entityUser.setBaseUrl(baseUrl);      
		entityUser.setCompanyCode(companyCode);
		entityUser.setCompany(company);      
//		entityUser.setUserType(userType);           			     								// Tipologia utente forward (T052)
//		entityUser.setUserStatus(userStatus;;           	 										// Stato utente forward (T051)
		entityUser.setLanguage(language);           					 							// Liguaggio in formato Locale ("en", "it", ... )
		entityUser.setPwd(pwd);        																// Password
		entityUser.setCountry(country);							 									// Country IT, .. 
		entityUser.setMail(mail);		 															// Mail principale  
		entityUser.setMailInfo(mailInfo);							 								// Mail per info anomali  
		entityUser.setPhone(phone);								 									// Telefono di riferimento  
		entityUser.setReferManager(referManager);					 								// Riferimento manager 
		entityUser.setReferTech(referTech);					 										// Riferimento tecnico 
		entityUser.setAnalyzerEnabled(false);
		entityUser.setAnalyzerEnabled(false);			    										//  
		entityUser.setViewerEnabled(false);															//  
		entityUser.setInspectorEnabled(false);														//  
		entityUser.setAssesmentEnabled(false);														//  
		if (analyzerEnabled.equals("true")) {entityUser.setAnalyzerEnabled(true);}
		if (viewerEnabled.equals("true")) {entityUser.setViewerEnabled(true);}
		if (inspectorEnabled.equals("true")) {entityUser.setInspectorEnabled(true);}
		if (assesmentEnabled.equals("true")) {entityUser.setAssesmentEnabled(true);}
		entityUser.setCountLogin(Integer.parseInt(countLogin));										// Counter login effettuati       			
		entityUser.setDtActivation(dtActivation);  													// Data attivazione AAAAMMGG'
		entityUser.setTmActivation(tmActivation);  													// Ora  attivazione HHMMSSCC'
		entityUser.setDtExpiration(dtExpiration);  													// Data disattivazione AAAAMMGG'
		entityUser.setTmExpiration(tmExpiration);  													// Ora  disattivazione HHMMSSCC'
 		entityUser.setDtFirstLogin("");  															// Data primo login AAAAMMGG'
 	    entityUser.setTmFirstLogin("");  															// Ora  primo login HHMMSSCC	'
 		entityUser.setDtLastLogin("");																// Data ultimo login AAAAMMGG'
 		entityUser.setTmLastLogin(""); 																// Ora  ultimo login HHMMSSCC'	

		// Directories relative a root o WEB-INF da file di configurazione generale
	    entityUser.setPathConfigFile(pathConfigFile);                 								// NOT NULL COMMENT 'Path completo file di configurazione'
	    entityUser.setPathRoot(pathRoot);                        									// NOT NULL COMMENT 'Phisical path to WEB-INF or application Installation'
	    entityUser.setPathUser(pathUser);                        									// NOT NULL COMMENT 'Phisical path to user  directory ...users/{user}'
	    entityUser.setPathPilot(pathPilot);                      									// Phisical path to pilot directory ...users/{user}'
	 	
	    entityUser.setDirResources(dirResources);                    								//Resource'
	    entityUser.setDirWork(dirWork);                        										// NOT NULL COMMENT 'Working e temporaneo'		    
	    entityUser.setDirDatabase(dirDatabase);                   									// Database'		              
	    entityUser.setDirJclInput(dirJclInput);                     								// Jcl    in input al processo di analisi (.*) '       
	    entityUser.setDirCobolSrcPgmInput(dirCobolSrcPgmInput);             						// Pgm    Cobol sorgenti in analisi		  (.*) '
	    entityUser.setDirCobolSrcCopyInput(dirCobolSrcCopyInput);           						// Copy   Cobol sorgenti in analisi		  (.*)'
	    entityUser.setDirCobolPgm(dirCobolPgm);                      								// Pgm    Cobol codificati e serializzati (.program)'			 
	    entityUser.setDirCobolCopy(dirCobolCopy);                     								// Copy   Cobol codificati e serializzati (.copy)'	
	    entityUser.setDirJcl(dirJcl);                           									// Jcl codificati e serializzati (.jclSourcedocument.getElementById("X").value=objConfig. .jclIncludedocument.getElementById("X").value=objConfig. .jclProc)'		 
	    entityUser.setDirSqlScript(dirSqlScript);                     								// Script Sql codificati e serializzati   (.scriptSql)'			 
	    entityUser.setDirCobolGraph(dirCobolGraph);                   								// Grafi  Cobol codificati e serializzati (.graph)'	
	    entityUser.setDirPilot(dirPilot);                         									// Pilot  sources e processi e filtri     (.pilot)'
	    entityUser.setDirLog(dirLog);                           									// Log
	    entityUser.setDirOutput(dirOutput);                        									// Output per funzioni che producono text'

		// Ottimizzazione processi ed elaborazioni, allocazione di arrays, collections, map
		entityUser.setLimitMaxLinesScanFindingSourceType(Integer.parseInt(limitMaxLinesScanFindingSourceType));   // Numero massimo righe da analizzare per individuare il tipo sorgente (200)'
		entityUser.setLimitMaxSources(false);            											// Abilitazione limitazione ai sources trattati'
		if (limitMaxSources.equals("true")) {entityUser.setLimitMaxSources(true);}
		entityUser.setLimitMaxSourcesInput(Integer.parseInt(limitMaxSourcesInput));         		// Numero massimo sorgenti da considerare in input'
		entityUser.setLimitMaxSourcesToProcess(Integer.parseInt(limitMaxSourcesToProcess));     	// Numero massimo sorgenti in input dei quali è stato intercettato il tipo'
		entityUser.setLimitMaxObjects(false);             		// Abilitazione limitazione agli oggetti processati'
		if (limitMaxObjects.equals("true")) {entityUser.setLimitMaxObjects(true);}
		entityUser.setLimitMaxObjectsInput(Integer.parseInt(limitMaxObjectsInput));         		// Numero massimo oggetti da considerare in input ai processi (filtrati)'
		entityUser.setLimitMaxObjectsToProcess(Integer.parseInt(limitMaxObjectsToProcess));     	// Numero massimo oggetti in input da processare'
		entityUser.setDebugThresholdMemoryGarbage(Integer.parseInt(debugThresholdMemoryGarbage));  	// Attivazione gc() se memoria disponibile <'
		entityUser.setDebugSourcesDetectedFreqGarbage(Integer.parseInt(debugSourcesDetectedFreqGarbage));  // gc() attivata ogni 200 sorgenti'
		entityUser.setDebugActive(false);                  										// Attivazione debug dove previsto (messaggi log di debug)'
		if (debugActive.equals("true")) {entityUser.setDebugActive(true);}
		entityUser.setLogVerbose(false);                   										// Dettaglio log operazioni Sql e informative'
		if (logVerbose.equals("true")) {entityUser.setLogVerbose(true);}
		entityUser.setPreferredVisitMethod(preferredVisitMethod);         						// Metodo di visita predefinito (BACKWARD)'
		entityUser.setPreferredCachingLevel(preferredCachingLevel);        						// Livello di caching predefinito (CACHING_PATH_ALL)'
		entityUser.setPreferredCachingSupport(preferredCachingSupport);      	 				// Tipo supporto java di cache (CACHING_ON_HASH_MAP, CACHING_ON_TREE_MAP)'
					
		//  Database
		entityUser.setDataBaseType(dataBaseType);                    							// Tipologia database MSACCESS/MYSQL'
		entityUser.setDataBaseName(dataBaseName);                    							// Nome database (DbAmrita)'
		entityUser.setDataBaseUser(dataBaseUser);                    							// User (GZEDDA)'
		entityUser.setDataBasePwd(dataBasePwd);                     							// Pwd (giampietro4)'
		entityUser.setDataBaseDriver(dataBaseDriver);                  							// Driver (com.mysql.cj.jdbc.Driver)'
		entityUser.setDataBaseAccessType(dataBaseAccessType);             						// Accesso LOCAL/REMOTE'
		entityUser.setDataBaseUrl(dataBaseUrl);                     							// jdbc:mysql://localhost:3306/Amrita?autoReconnect=true&useSSL=false&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC'		 	
		entityUser.setDataBaseMaxConn(Integer.parseInt(dataBaseMaxConn));                 		// Numero massimo connessioni attive (1)'
		entityUser.setDataBaseCommitBlockUpdates(Integer.parseInt(dataBaseCommitBlockUpdates));;// Commit automatica a fine gruppo aggiornamenti (100)'
		entityUser.setDataBaseLogAnySql(false);  			 									// Log istruzioni Sql come messaggi informativi
		if (dataBaseLogAnySql.equals("true")) {entityUser.setDataBaseLogAnySql(true);}
			
		// Controllo analisi, identificazione oggetti analizzati/processati, piloti sources, filtri e processi'
		entityUser.setPilotDefaultSource(pilotDefaultSource);              						// Pilota sorgenti (PilotDefaultSource.pilot)'
		entityUser.setPilotDefaultProcess(pilotDefaultProcess);             					// Pilota processi (PilotDefaultProcess.pilot)'
		entityUser.setUserExitClass(userExitClass);                   							// Classe con exit applicative codificate (UserExit)'   

				
		userUpdated=userDAO.update(entityUser);		
		statusSave="OK";
		if (!userUpdated) {
			statusSave="KOUser Not Found";
		}
				
		
	    return Response
 				.status(200)
				.entity(statusSave)     // OK or KO+Message
				.build();
	}


	///////////////////////////////////////////////////////////////////////////////////////
    // Web Services di supporto all'esecuzione
	///////////////////////////////////////////////////////////////////////////////////////
	
	
	/*
	 * Attivazione processo di analisi/elaborazione
	 * 
	 */
	@PUT 
	@Produces("application/json")
	@Path("executionWebStart/{user}/{pilotName}")  
	public Response executionWebStart	(
										  @PathParam("user")         String user
										, @PathParam("pilotName")    String pilotName
							
										) throws JSONException, SQLException, ExceptionAmrita {

		UserConfiguration ucfg = null;
		ExecutionStarterWeb exsWeb = null;
		String result="OK";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		exsWeb = new ExecutionStarterWeb();
		try {
			exsWeb.exec(ucfg, pilotName);
		} catch (ExceptionAmrita e) {
			result="KO"+e.getQualifiedError();
		}
		
		return Response
				.status(200)
				.entity(result)
				.build();
	}

	/*
	 * Stop processo di analisi/elaborazione corrente
	 * 
	 */
	@PUT 
	@Produces("application/json")
	@Path("executionWebStop/{user}")  
	public Response executionWebStop(
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
	@Path("executionInfo/{user}")  
	public Response executionInfo(@PathParam("user") String user) throws JSONException, SQLException, ExceptionAmrita {

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

