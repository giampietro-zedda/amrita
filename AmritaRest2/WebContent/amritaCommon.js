/**
 * Coomon definitions for all pages
 */
//Defines Object configuration data available to all pages
function Config( 
		      user
			 ,sys 
			 ,subSys
			 ,baseurl
			 
             ,companyCode 
             ,company 
             ,userType  
             ,userStatus  
             ,language  
             ,pwd  
             ,country  
             ,mail  
             ,mailInfo  
             ,phone  
             ,referManager  
             ,referTech  
             ,analyzerEnabled  
             ,viewerEnabled 
             ,inspectorEnabled  
             ,assesmentEnabled  
             ,countLogin   			
             ,dtActivation  
             ,tmActivation  
             ,dtExpiration  
             ,tmExpiration  
             ,dtFirstLogin  
             ,dtFirstLogin  
             ,dtLastLogin  
             ,tmLastLogin  

             ,pathConfigFile  
             ,pathRoot  
             ,pathUser  
             ,pathPilot  
	
			 // Directories relative a root o WEB-INF da file di configurazione generale
             ,dirResources  
             ,dirWork  
             ,dirDatabase       
             ,dirJclInput  
             ,dirCobolSrcPgmInput  
             ,dirCobolSrcCopyInput  
             ,dirCobolPgm  
             ,dirCobolCopy  
             ,dirJcl  
             ,dirSqlScript  	 
             ,dirCobolGraph  
             ,dirPilot  
             ,dirLog  
             ,dirOutput  

			 // Ottimizzazione processi ed elaborazioni, allocazione di arrays, collections, map
             ,limitMaxLinesScanFindingSourceType 
             ,limitMaxSources  
             ,limitMaxSourcesInput  
             ,limitMaxSourcesToProcess  
             ,limitMaxObjects  
             ,limitMaxObjectsInput 
             ,limitMaxObjectsToProcess 
             ,debugThresholdMemoryGarbage  
             ,debugSourcesDetectedFreqGarbage  
             ,debugActive  
             ,logVerbose  
             ,preferredVisitMethod  
             ,preferredCachingLevel  
             ,preferredCachingSupport  

			 //  Database
             ,dataBaseType  
             ,dataBaseName  
             ,dataBaseUser  
             ,dataBasePwd  
             ,dataBaseDriver  
             ,dataBaseAccessType  
             ,dataBaseUrl  
             ,dataBaseMaxConn  
             ,dataBaseCommitBlockUpdates  
             ,dataBaseLogAnySql 
	
			 // Controllo analisi, identificazione oggetti analizzati/processati, piloti sources, filtri e processi'
             ,pilotDefaultSource  
             ,pilotDefaultProcess  
             ,userExitClass
             
             ,userTypeOrdinal 
             ,userStatusOrdinal 
				)  {
	
	    this.user = user;  
	    this.sys = sys;  
	    this.subSys = subSys; 
	    this.baseurl = baseurl;
	    
		// User Data
		this.companyCode = companyCode; 							 // Codice societa 
		this.company = company; 									 // Descrizione societa 
		this.userType = userType;            			     		 // Tipologia utente forward (T052)
		this.userStatus = userStatus;            	 				 // Stato utente forward (T051)
		this.language = language;            					 	 // Liguaggio in formato Locale ("en", "it", ... )
		this.pwd = pwd;          								 	 // Password
		this.country = country; 									 // Country IT, .. 
		this.mail = mail; 											 // Mail principale  
		this.mailInfo = mailInfo; 									 // Mail per info anomali  
		this.phone = phone; 										 // Telefono di riferimento  
		this.referManager = referManager; 							 // Riferimento manager 
		this.referTech = referTech; 								 // Riferimento tecnico 
		this.analyzerEnabled = analyzerEnabled; 					 // True user can view Analyzer 
		this.viewerEnabled = viewerEnabled; 						 // True user can view Viewer 
		this.inspectorEnabled = inspectorEnabled; 					 // True user can view Inspector 
		this.assesmentEnabled = assesmentEnabled; 					 // True user can view Assesment 
		this.countLogin =countLogin; 								 // Counter login effettuati       			
		this.dtActivation = dtActivation;  							 // Data attivazione AAAAMMGG'
		this.tmActivation = tmActivation;  						     // Ora  attivazione HHMMSSCC'
		this.dtExpiration = dtExpiration;  							 // Data disattivazione AAAAMMGG'
		this.tmExpiration = tmExpiration;  							 // Ora  disattivazione HHMMSSCC'
		this.dtFirstLogin = dtFirstLogin;  							 // Data primo login AAAAMMGG'
		this.dtFirstLogin = tmFirstLogin;  							 // Ora  primo login HHMMSSCC	'
		this.dtLastLogin = dtLastLogin; 							 // Data ultimo login AAAAMMGG'
		this.tmLastLogin = tmLastLogin; 							 // Ora  ultimo login HHMMSSCC'	

	    this.pathConfigFile = pathConfigFile;                        // NOT NULL COMMENT 'Path completo file di configurazione'
	    this.pathRoot = pathRoot;                                	 // NOT NULL COMMENT 'Phisical path to WEB-INF or application Installation'
	    this.pathUser = pathUser;                                	 // NOT NULL COMMENT 'Phisical path to user  directory ...users/{user}'
	    this.pathPilot = pathPilot;                               	 // Phisical path to pilot directory ...users/{user}'
	 	
		// Directories relative a root o WEB-INF da file di configurazione generale
		this.dirResources = dirResources;                            //Resource'
		this.dirWork = dirWork;                                 	 // NOT NULL COMMENT 'Working e temporaneo'		    
		this.dirDatabase = dirDatabase;                              // Database'		              
		this.dirJclInput = dirJclInput;                              // Jcl    in input al processo di analisi (.*) '       
		this.dirCobolSrcPgmInput = dirCobolSrcPgmInput;              // Pgm    Cobol sorgenti in analisi		  (.*) '
		this.dirCobolSrcCopyInput = dirCobolSrcCopyInput;            // Copy   Cobol sorgenti in analisi		  (.*)'
		this.dirCobolPgm = dirCobolPgm;                              // Pgm    Cobol codificati e serializzati (.program)'			 
		this.dirCobolCopy = dirCobolCopy;                            // Copy   Cobol codificati e serializzati (.copy)'	
		this.dirJcl = dirJcl;                                  		 // Jcl codificati e serializzati (.jclSourcethis.X =  .jclIncludethis.X =  .jclProc)'		 
		this.dirSqlScript = dirSqlScript;                            // Script Sql codificati e serializzati   (.scriptSql)'			 
		this.dirCobolGraph = dirCobolGraph;                          // Grafi  Cobol codificati e serializzati (.graph)'	
		this.dirPilot = dirPilot;                                	 // Pilot  sources e processi e filtri     (.pilot)'
		this.dirLog = dirLog;                                  		 // Log
		this.dirOutput = dirOutput;                              	 // Output per funzioni che producono text'

		// Ottimizzazione processi ed elaborazioni, allocazione di arrays, collections, map
		this.limitMaxLinesScanFindingSourceType = limitMaxLinesScanFindingSourceType;  // Numero massimo righe da analizzare per individuare il tipo sorgente (200)'
		this.limitMaxSources =  limitMaxSources;                    	// Abilitazione limitazione ai sources trattati'
		this.limitMaxSourcesInput = limitMaxSourcesInput;               // Numero massimo sorgenti da considerare in input'
		this.limitMaxSourcesToProcess = limitMaxSourcesToProcess;       // Numero massimo sorgenti in input dei quali è stato intercettato il tipo'
		this.limitMaxObjects =  limitMaxObjects;                   	 	// Abilitazione limitazione agli oggetti processati'
		this.limitMaxObjectsInput = limitMaxObjectsInput;               // Numero massimo oggetti da considerare in input ai processi (filtrati)'
		this.limitMaxObjectsToProcess = imitMaxObjectsToProcess;       	// Numero massimo oggetti in input da processare'
		this.debugThresholdMemoryGarbage = debugThresholdMemoryGarbage; // Attivazione gc() se memoria disponibile <'
		this.debugSourcesDetectedFreqGarbage = debugSourcesDetectedFreqGarbage; // gc() attivata ogni 200 sorgenti'
		this.debugActive = debugActive;                             	// Attivazione debug dove previsto (messaggi log di debug)'
		this.logVerbose = logVerbose;                               	// Dettaglio log operazioni Sql e informative'
		this.preferredVisitMethod = preferredVisitMethod;            	// Metodo di visita predefinito (BACKWARD)'
		this.preferredCachingLevel = preferredCachingLevel;          	// Livello di caching predefinito (CACHING_PATH_ALL)'
		this.preferredCachingSupport = preferredCachingSupport;      	// Tipo supporto java di cache (CACHING_ON_HASH_MAP, CACHING_ON_TREE_MAP)'
		
		//  Database
		this.dataBaseType = dataBaseType;                            // Tipologia database MSACCESS/MYSQL'
		this.dataBaseName = dataBaseName;                            // Nome database (DbAmrita)'
		this.dataBaseUser = dataBaseUser;                            // User (GZEDDA)'
		this.dataBasePwd = dataBasePwd;                              // Pwd (giampietro4)'
		this.dataBaseDriver = dataBaseDriver;                        // Driver (com.mysql.cj.jdbc.Driver)'
		this.dataBaseAccessType = dataBaseAccessType;                // Accesso LOCAL/REMOTE'
		this.dataBaseUrl = dataBaseUrl;                              // jdbc:mysql://localhost:3306/Amrita?autoReconnect=true&useSSL=false&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC'		 	
		this.dataBaseMaxConn = dataBaseMaxConn;                         // Numero massimo connessioni attive (1)'
		this.dataBaseCommitBlockUpdates = dataBaseCommitBlockUpdates;   // Commit automatica a fine gruppo aggiornamenti (100)'
		this.dataBaseLogAnySql = dataBaseLogAnySql;                 // Log istruzioni Sql come messaggi informativi'
	    	
		// Controllo analisi, identificazione oggetti analizzati/processati, piloti sources, filtri e processi'
		this.pilotDefaultSource = pilotDefaultSource;                 // Pilota sorgenti (PilotDefaultSource.pilot)'
		this.pilotDefaultProcess = pilotDefaultProcess;               // Pilota processi (PilotDefaultProcess.pilot)'
		this.userExitClass = userExitClass;                           // Classe con exit applicative codificate (UserExit)'   

		// Ordinal
		this.userTypeOrdinal = userTypeOrdinal;            			  // Tipologia utente forward (T052)
		this.userStatusOrdinal = userStatusOrdinal;            	 	  // Stato utente forward (T051)

}