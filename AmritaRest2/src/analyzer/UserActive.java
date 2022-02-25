/**
 * Models the users connected, storing the user configuration in a static Map.
 * The user configuration depicts all paths and options for the user.
 * 1) Initially the properties file is read from the installation path or WEB-INF.
 * 2) With installation database parameters (name and password) the "User" table is read
 * 3) The current database parameters (user and pwd) are replaced with the user parameters
 * 4) The table "User" is read from the user database and the object UserConfiguration populated
 * 5) The use configuration object is stored in the Map.
 */
package analyzer;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import dao.DAOFactory;
import dao.IDAOUser;
import dao.MySQLDAOFactory;
import entities.EntityUser;
import enums.EnumAmritaExceptionError;
import exception.ExceptionAmrita;

/**
 * @author giampietro Zedda
 *
 */
public class UserActive {
	private static Map<String, UserConfiguration> mapUsers = new HashMap<String, UserConfiguration> ();
    
	private UserActive() {
		super();
	}

	
	/* Add a user or get it from user table*/
	public static synchronized UserConfiguration addUser(String userLogin) throws ExceptionAmrita, SQLException {
		
		List<EntityUser> ls_object = null;
		UserConfiguration ucfg = mapUsers.get(userLogin);
		
		// User already logged in
		if (ucfg != null  ) {
			return ucfg;
		}
		
		// Get user from installation database default properties file
		ucfg = new UserConfiguration();  

		// Get specific User configuration
		Connection conn = DataBaseConnections.getConnection();
		IDAOUser eoDAO = AmritaStartup.sqlFactory.getDAOUser(conn, false, true, ucfg);
		
		try {
			ls_object=eoDAO.findAll(userLogin);
		} catch (SQLException e) {
			ExceptionAmrita excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_SQL, e);
			throw excp;
		} finally {
			DataBaseConnections.releaseConnection(conn);
		}
		
        // User found on db, update parameters
		if (ls_object.size() != 0) {
			updateUcfgFromUser(ucfg, ls_object.get(0));
			ucfg.setUserDefinedOnDb(true);
			mapUsers.put(userLogin, ucfg);
		} else {
			ucfg.setUserDefinedOnDb(false);
		}
		return ucfg;
	}

	private static void updateUcfgFromUser(UserConfiguration ucfg, EntityUser entityUser) {
		// User Data
		ucfg.setSystemOwner(entityUser.getCompanyCode());
 		ucfg.setUser(entityUser.getUser());                           
 		ucfg.setPwd(entityUser.getPwd());                           
 		ucfg.setBaseUrl(entityUser.getBaseUrl());                           
 		ucfg.setCompanyCode(entityUser.getCompanyCode()); 							 
 		ucfg.setCompany(entityUser.getCompany()); 									 
		ucfg.setUserType(entityUser.getUserType());            			 
		ucfg.setUserStatus(entityUser.getUserStatus());            	  
		ucfg.setStrLanguage(entityUser.getLanguage());            					 
		ucfg.setPwd(entityUser.getPwd());          								 
		ucfg.setCountry(entityUser.getCountry()); 									 
		ucfg.setMail(entityUser.getMail()); 											 
		ucfg.setMailInfo(entityUser.getMailInfo()); 									 
		ucfg.setReferManager(entityUser.getReferManager()); 							  
		ucfg.setReferTech(entityUser.getReferTech()); 								 
		
		ucfg.setAnalyzerEnabled(entityUser.getAnalyzerEnabled()); 					  
		ucfg.setViewerEnabled(entityUser.getViewerEnabled()); 						 
		ucfg.setInspectorEnabled(entityUser.getInspectorEnabled()); 					 
		ucfg.setAssesmentEnabled(entityUser.getAssesmentEnabled()); 					  

	    ucfg.setPathConfigFile(entityUser.getPathConfigFile());                        
	    ucfg.setPathRoot(entityUser.getPathRoot());                                	 
	    ucfg.setPathUser(entityUser.getPathUser());                                	                             	  
	 	
		// Directories relative a root o WEB-INF da file di configurazione generale
		ucfg.setDirResources(entityUser.getDirResources());                            
		ucfg.setDirWork(entityUser.getDirWork());                                 	 
		ucfg.setDirDatabase(entityUser.getDirDatabase());                                             
		ucfg.setDirJclSrc(entityUser.getDirJclInput());                                   
		ucfg.setDirCobolSrcPgm(entityUser.getDirCobolSrcPgmInput());               
		ucfg.setDirCobolSrcCopy(entityUser.getDirCobolSrcCopyInput());             
		ucfg.setDirCobolObjPgm(entityUser.getDirCobolPgm());                               		 
		ucfg.setDirCobolObjCopy(entityUser.getDirCobolCopy());                             
		ucfg.setDirJclObj(entityUser.getDirJcl());                                  		  
		ucfg.setDirSqlScript(entityUser.getDirSqlScript());                             	 
		ucfg.setDirCobolGraph(entityUser.getDirCobolGraph());                           
		ucfg.setDirPilot(entityUser.getDirPilot());                                	 
		ucfg.setDirLog(entityUser.getDirLog());                                  		 
		ucfg.setDirOutput(entityUser.getDirOutput());                              	 

		// Ottimizzazione processi ed elaborazioni, allocazione di arrays, collections, map
		ucfg.setLimitMaxLinesScanFindingSourceType(entityUser.getLimitMaxLinesScanFindingSourceType());  
		ucfg.setLimitMaxSources(entityUser.getLimitMaxSources());                    
		ucfg.setLimitMaxSourcesInput(entityUser.getLimitMaxSourcesInput());                
		ucfg.setLimitMaxSourcesToProcess(entityUser.getLimitMaxSourcesToProcess());        
		ucfg.setLimitMaxObjects(entityUser.getLimitMaxObjects());                    
		ucfg.setLimitMaxObjectsInput(entityUser.getLimitMaxObjectsInput());                
		ucfg.setLimitMaxObjectsToProcess(entityUser.getLimitMaxObjectsToProcess());        
		ucfg.setDebugThresholdMemoryGarbage(entityUser.getDebugThresholdMemoryGarbage()); 
		ucfg.setDebugSourcesDetectedFreqGarbage(entityUser.getDebugSourcesDetectedFreqGarbage());  
		ucfg.setDebugActive(entityUser.getDebugActive());                             
		ucfg.setLogVerbose(entityUser.getLogVerbose());                               
		ucfg.setPreferredVisitMethod(entityUser.getPreferredVisitMethod());            
		ucfg.setPreferredCachingLevel(entityUser.getPreferredCachingLevel());         
		ucfg.setPreferredCachingSupport(entityUser.getPreferredCachingSupport());      
		
		//  Database
		ucfg.setDataBaseType(entityUser.getDataBaseType());                             
		ucfg.setDataBaseName(entityUser.getDataBaseName());                             
		ucfg.setDataBaseUser(entityUser.getDataBaseUser());                             
		ucfg.setDataBasePwd(entityUser.getDataBasePwd());                              
		ucfg.setDataBaseDriver(entityUser.getDataBaseDriver());                        
		ucfg.setDataBaseAccessType(entityUser.getDataBaseAccessType());                
		ucfg.setDataBaseUrl(entityUser.getDataBaseUrl());                              
		ucfg.setDataBaseMaxConn(entityUser.getDataBaseMaxConn());                          
		ucfg.setDataBaseCommitBlockUpdates(entityUser.getDataBaseCommitBlockUpdates());    
		ucfg.setDataBaseLogAnySql(entityUser.getDataBaseLogAnySql());                 
	    	
		// Controllo analisi, identificazione oggetti analizzati/processati, piloti sources, filtri e processi'
		ucfg.setPilotDefaultSource(entityUser.getPilotDefaultSource());                 
		ucfg.setPilotDefaultProcess(entityUser.getPilotDefaultProcess());               
		ucfg.setUserExitClass(entityUser.getUserExitClass());                           
	}


	/* Remove a user */
	public static synchronized void removeUser(String userLogin) {
		mapUsers.remove(userLogin);
		return;
	}


	/* get user configuration */
	public static synchronized UserConfiguration getUser(String user) {
		return mapUsers.get(user); 
	}


}
