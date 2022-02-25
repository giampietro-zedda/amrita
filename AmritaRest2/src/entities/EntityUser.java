package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumUserStatus;
import enums.EnumUserType;

/**
	 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityUser (USER) 
	 * </h1>
	 *  <p>
	 * This class describes a user with all permissions and login informations.
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
	 * @see Analyzer
	 * @see EntityUser 
	 * @see EntityObjectOption
	 * @see EntityIndexItem
	 * @see EntityRelation
	 * @see EntityRelationOrigin
	 * @see EntityCopyEntityDefinition
	 * @see EntityWhereUsedItem
	 * @see EntityMapDescriptor
	 * @see EntityMapItem
	 * @see EntityTagValue
	 * @see EntityDynamicValueExt
	 * @see EntityScopeHeader
	 * @see EntityScopeSection
	 * @see EntityScopeItem
	 * @see EntityScopeChild
	 * @see EntityScopeProgram
	 * @see EntityScopeObject
	 * @see EntityScopeRelation
	 * @see EntityProcessLog
	 * @see EntityMetric
	 * @see EntityTableHeader    
	 * @see EntityTableStructure   
	 * @see EntityTableData 
	 * @see EntityDynamicField
	 * @see EntityDynamicFieldSub
	 * @see EntityDynamicValue
	 * @see EntityDynamicFieldSubSetting
*/

@Entity(name="User")
public class EntityUser {

	///////////////////////////////////////////////////////////////////////
    // Data Items User                                                                                                
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
	@Column(name="user") private String user = "";         				    			 // Codice utente
	
	// User Data
	@Column(name="baseUrl") private String baseUrl;                                      // Base Url ie "http://localhost:8080/AmritaRest2"
	@Column(name="companyCode") private String companyCode; 							 // Codice societa 
	@Column(name="company") private String company; 									 // Descrizione societa 
	@Column(name="userType") private EnumUserType userType = null;            			 // Tipologia utente forward (T052)
	@Column(name="userStatus") private EnumUserStatus userStatus = null;            	 // Stato utente forward (T051)
	@Column(name="language") private String language = "";            					 // Liguaggio in formato Locale ("en", "it", ... )
	@Column(name="pwd") private String pwd = "";          								 // Password
	@Column(name="country") private String country; 									 // Country IT, .. 
	@Column(name="mail") private String mail; 											 // Mail principale  
	@Column(name="mailInfo") private String mailInfo; 									 // Mail per info anomali  
	@Column(name="phone") private String phone; 										 // Telefono di riferimento  
	@Column(name="referManager") private String referManager; 							 // Riferimento manager 
	@Column(name="referTech") private String referTech; 								 // Riferimento tecnico 
	@Column(name="analyzerEnabled") private boolean analyzerEnabled; 					 // True user can view Analyzer 
	@Column(name="viewerEnabled") private boolean viewerEnabled; 						 // True user can view Viewer 
	@Column(name="inspectorEnabled") private boolean inspectorEnabled; 					 // True user can view Inspector 
	@Column(name="assesmentEnabled") private boolean assesmentEnabled; 					 // True user can view Assesment 
	@Column(name="countLogin") private int countLogin; 									 // Counter login effettuati       			
	@Column(name="dtActivation") private String dtActivation;  							 // Data attivazione AAAAMMGG'
	@Column(name="tmActivation") private String tmActivation;  						     // Ora  attivazione HHMMSSCC'
	@Column(name="dtExpiration") private String dtExpiration;  							 // Data disattivazione AAAAMMGG'
	@Column(name="tmExpiration") private String tmExpiration;  							 // Ora  disattivazione HHMMSSCC'
	@Column(name="dtFirstLogin") private String dtFirstLogin;  							 // Data primo login AAAAMMGG'
	@Column(name="dtFirstLogin") private String tmFirstLogin;  							 // Ora  primo login HHMMSSCC	'
	@Column(name="dtLastLogin") private String dtLastLogin; 							 // Data ultimo login AAAAMMGG'
	@Column(name="tmLastLogin") private String tmLastLogin; 							 // Ora  ultimo login HHMMSSCC'	

    @Column(name="pathConfigFile") private String pathConfigFile;                        // NOT NULL COMMENT 'Path completo file di configurazione'
    @Column(name="pathRoot") private String pathRoot;                                	 // NOT NULL COMMENT 'Phisical path to WEB-INF or application Installation'
    @Column(name="pathUser") private String pathUser;                                	 // NOT NULL COMMENT 'Phisical path to user  directory ...users/{user}'
    @Column(name="pathPilot") private String pathPilot;                               	 //Phisical path to pilot directory ...users/{user}'
 	
	// Directories relative a root o WEB-INF da file di configurazione generale
	@Column(name="dirResources") private String dirResources;                            //Resource'
	@Column(name="dirWork") private String dirWork;                                 	 // NOT NULL COMMENT 'Working e temporaneo'		    
	@Column(name="dirDatabase") private String dirDatabase;                              // Database'		              
	@Column(name="dirJclInput") private String dirJclInput;                              // Jcl    in input al processo di analisi (.*) '       
	@Column(name="dirCobolSrcPgmInput") private String dirCobolSrcPgmInput;              // Pgm    Cobol sorgenti in analisi		  (.*) '
	@Column(name="dirCobolSrcCopyInput") private String dirCobolSrcCopyInput;            // Copy   Cobol sorgenti in analisi		  (.*)'
	@Column(name="dirCobolPgm") private String dirCobolPgm;                              // Pgm    Cobol codificati e serializzati (.program)'			 
	@Column(name="dirCobolCopy") private String dirCobolCopy;                            // Copy   Cobol codificati e serializzati (.copy)'	
	@Column(name="dirJcl") private String dirJcl;                                  		 // Jcl codificati e serializzati (.jclSource@Column(name="X") private String  .jclInclude@Column(name="X") private String  .jclProc)'		 
	@Column(name="dirSqlScript") private String dirSqlScript;                            // Script Sql codificati e serializzati   (.scriptSql)'			 
	@Column(name="dirCobolGraph") private String dirCobolGraph;                          // Grafi  Cobol codificati e serializzati (.graph)'	
	@Column(name="dirPilot") private String dirPilot;                                	 // Pilot  sources e processi e filtri     (.pilot)'
	@Column(name="dirLog") private String dirLog;                                  		 // Log
	@Column(name="dirOutput") private String dirOutput;                              	 // Output per funzioni che producono text'

	// Ottimizzazione processi ed elaborazioni, allocazione di arrays, collections, map
	@Column(name="limitMaxLinesScanFindingSourceType") private int limitMaxLinesScanFindingSourceType;  // Numero massimo righe da analizzare per individuare il tipo sorgente (200)'
	@Column(name="limitMaxSources") private boolean  limitMaxSources;                    // Abilitazione limitazione ai sources trattati'
	@Column(name="limitMaxSourcesInput") private int limitMaxSourcesInput;               // Numero massimo sorgenti da considerare in input'
	@Column(name="limitMaxSourcesToProcess") private int limitMaxSourcesToProcess;       // Numero massimo sorgenti in input dei quali è stato intercettato il tipo'
	@Column(name="limitMaxObjects") private boolean  limitMaxObjects;                    // Abilitazione limitazione agli oggetti processati'
	@Column(name="limitMaxObjectsInput") private int limitMaxObjectsInput;               // Numero massimo oggetti da considerare in input ai processi (filtrati)'
	@Column(name="limitMaxObjectsToProcess") private int limitMaxObjectsToProcess;       // Numero massimo oggetti in input da processare'
	@Column(name="debugThresholdMemoryGarbage") private int debugThresholdMemoryGarbage; // Attivazione gc() se memoria disponibile <'
	@Column(name="debugSourcesDetectedFreqGarbage") private int debugSourcesDetectedFreqGarbage; // gc() attivata ogni 200 sorgenti'
	@Column(name="debugActive") private boolean debugActive;                             // Attivazione debug dove previsto (messaggi log di debug)'
	@Column(name="logVerbose") private boolean logVerbose;                               // Dettaglio log operazioni Sql e informative'
	@Column(name="preferredVisitMethod") private String preferredVisitMethod;            // Metodo di visita predefinito (BACKWARD)'
	@Column(name="preferredCachingLevel") private String preferredCachingLevel;          // Livello di caching predefinito (CACHING_PATH_ALL)'
	@Column(name="preferredCachingSupport") private String preferredCachingSupport;      // Tipo supporto java di cache (CACHING_ON_HASH_MAP, CACHING_ON_TREE_MAP)'
	
	//  Database
	@Column(name="dataBaseType") private String dataBaseType;                            // Tipologia database MSACCESS/MYSQL'
	@Column(name="dataBaseName") private String dataBaseName;                            // Nome database (DbAmrita)'
	@Column(name="dataBaseUser") private String dataBaseUser;                            // User (GZEDDA)'
	@Column(name="dataBasePwd") private String dataBasePwd;                              // Pwd (giampietro4)'
	@Column(name="dataBaseDriver") private String dataBaseDriver;                        // Driver (com.mysql.cj.jdbc.Driver)'
	@Column(name="dataBaseAccessType") private String dataBaseAccessType;                // Accesso LOCAL/REMOTE'
	@Column(name="dataBaseUrl") private String dataBaseUrl;                              // jdbc:mysql://localhost:3306/Amrita?autoReconnect=true&useSSL=false&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC'		 	
	@Column(name="dataBaseMaxConn") private int dataBaseMaxConn;                         // Numero massimo connessioni attive (1)'
	@Column(name="dataBaseCommitBlockUpdates") private int dataBaseCommitBlockUpdates;   // Commit automatica a fine gruppo aggiornamenti (100)'
	@Column(name="dataBaseLogAnySql") private boolean dataBaseLogAnySql;                 // Log istruzioni Sql come messaggi informativi'
    	
	// Controllo analisi, identificazione oggetti analizzati/processati, piloti sources, filtri e processi'
	@Column(name="pilotDefaultSource") private String pilotDefaultSource;                 // Pilota sorgenti (PilotDefaultSource.pilot)'
	@Column(name="pilotDefaultProcess") private String pilotDefaultProcess;               // Pilota processi (PilotDefaultProcess.pilot)'
	@Column(name="userExitClass") private String userExitClass;                           // Classe con exit applicative codificate (UserExit)'

	
	
	
	
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityUser() {
		super();
		
		userType = EnumUserType.NOT_ASSIGNED;
		userStatus = EnumUserStatus.NOT_ASSIGNED;
		
	}

	/**
	 * @return the userName
	 */
	public String getUserName() {
		return user;
	}

	/**
	 * @param user the userName to set
	 */
	public void setUserName(String userName) {
		this.user = userName;
	}

	/**
	 * @return the pwd
	 */
	public String getPwd() {
		return pwd;
	}

	/**
	 * @param pwd the pwd to set
	 */
	public void setPwd(String pwd) {
		this.pwd = pwd;
	}

	/**
	 * @return the userType
	 */
	public EnumUserType getUserType() {
		return userType;
	}

	/**
	 * @param userType the userType to set
	 */
	public void setUserType(EnumUserType userType) {
		this.userType = userType;
	}

	/**
	 * @return the userStatus
	 */
	public EnumUserStatus getUserStatus() {
		return userStatus;
	}

	/**
	 * @param userStatus the userStatus to set
	 */
	public void setUserStatus(EnumUserStatus userStatus) {
		this.userStatus = userStatus;
	}

	/**
	 * @return the language
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * @param language the language to set
	 */
	public void setLanguage(String language) {
		this.language = language;
	}

	/**
	 * @return the dtActivation
	 */
	public String getDtActivation() {
		return dtActivation;
	}

	/**
	 * @param dtActivation the dtActivation to set
	 */
	public void setDtActivation(String dtActivation) {
		this.dtActivation = dtActivation;
	}

	/**
	 * @return the dtExpiration
	 */
	public String getDtExpiration() {
		return dtExpiration;
	}

	/**
	 * @param dtExpiration the dtExpiration to set
	 */
	public void setDtExpiration(String dtExpiration) {
		this.dtExpiration = dtExpiration;
	}

	/**
	 * @return the dtFirstLogin
	 */
	public String getDtFirstLogin() {
		return dtFirstLogin;
	}

	/**
	 * @param dtFirstLogin the dtFirstLogin to set
	 */
	public void setDtFirstLogin(String dtFirstLogin) {
		this.dtFirstLogin = dtFirstLogin;
	}

	/**
	 * @return the dtLastLogin
	 */
	public String getDtLastLogin() {
		return dtLastLogin;
	}

	/**
	 * @param dtLastLogin the dtLastLogin to set
	 */
	public void setDtLastLogin(String dtLastLogin) {
		this.dtLastLogin = dtLastLogin;
	}

	/**
	 * @return the tmFirstLogin
	 */
	public String getTmFirstLogin() {
		return tmFirstLogin;
	}

	/**
	 * @param tmFirstLogin the tmFirstLogin to set
	 */
	public void setTmFirstLogin(String tmFirstLogin) {
		this.tmFirstLogin = tmFirstLogin;
	}

	/**
	 * @return the tmLastLogin
	 */
	public String getTmLastLogin() {
		return tmLastLogin;
	}

	/**
	 * @param tmLastLogin the tmLastLogin to set
	 */
	public void setTmLastLogin(String tmLastLogin) {
		this.tmLastLogin = tmLastLogin;
	}

	/**
	 * @return the user
	 */
	public String getUser() {
		return user;
	}

	/**
	 * @param user the user to set
	 */
	public void setUser(String user) {
		this.user = user;
	}

	/**
	 * @return the country
	 */
	public String getCountry() {
		return country;
	}

	/**
	 * @param country the country to set
	 */
	public void setCountry(String country) {
		this.country = country;
	}

	/**
	 * @return the companyCode
	 */
	public String getCompanyCode() {
		return companyCode;
	}

	/**
	 * @param companyCode the companyCode to set
	 */
	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	/**
	 * @return the company
	 */
	public String getCompany() {
		return company;
	}

	/**
	 * @param company the company to set
	 */
	public void setCompany(String company) {
		this.company = company;
	}

	/**
	 * @return the mail
	 */
	public String getMail() {
		return mail;
	}

	/**
	 * @param mail the mail to set
	 */
	public void setMail(String mail) {
		this.mail = mail;
	}

	/**
	 * @return the mailInfo
	 */
	public String getMailInfo() {
		return mailInfo;
	}

	/**
	 * @param mailInfo the mailInfo to set
	 */
	public void setMailInfo(String mailInfo) {
		this.mailInfo = mailInfo;
	}

	/**
	 * @return the phone
	 */
	public String getPhone() {
		return phone;
	}

	/**
	 * @param phone the phone to set
	 */
	public void setPhone(String phone) {
		this.phone = phone;
	}

	/**
	 * @return the referManager
	 */
	public String getReferManager() {
		return referManager;
	}

	/**
	 * @param referManager the referManager to set
	 */
	public void setReferManager(String referManager) {
		this.referManager = referManager;
	}

	/**
	 * @return the referTech
	 */
	public String getReferTech() {
		return referTech;
	}

	/**
	 * @param referTech the referTech to set
	 */
	public void setReferTech(String referTech) {
		this.referTech = referTech;
	}

	/**
	 * @return the analyzerEnabled
	 */
	public boolean getAnalyzerEnabled() {
		return analyzerEnabled;
	}

	/**
	 * @param analyzerEnabled the analyzerEnabled to set
	 */
	public void setAnalyzerEnabled(boolean analyzerEnabled) {
		this.analyzerEnabled = analyzerEnabled;
	}

	/**
	 * @return the viewerEnabled
	 */
	public boolean getViewerEnabled() {
		return viewerEnabled;
	}

	/**
	 * @param viewerEnabled the viewerEnabled to set
	 */
	public void setViewerEnabled(boolean viewerEnabled) {
		this.viewerEnabled = viewerEnabled;
	}

	/**
	 * @return the inspectorEnabled
	 */
	public boolean getInspectorEnabled() {
		return inspectorEnabled;
	}

	/**
	 * @param inspectorEnabled the inspectorEnabled to set
	 */
	public void setInspectorEnabled(boolean inspectorEnabled) {
		this.inspectorEnabled = inspectorEnabled;
	}

	/**
	 * @return the assesmentEnabled
	 */
	public boolean getAssesmentEnabled() {
		return assesmentEnabled;
	}

	/**
	 * @param assesmentEnabled the assesmentEnabled to set
	 */
	public void setAssesmentEnabled(boolean assesmentEnabled) {
		this.assesmentEnabled = assesmentEnabled;
	}

	/**
	 * @return the countLogin
	 */
	public int getCountLogin() {
		return countLogin;
	}

	/**
	 * @param countLogin the countLogin to set
	 */
	public void setCountLogin(int countLogin) {
		this.countLogin = countLogin;
	}

	/**
	 * @return the tmActivation
	 */
	public String getTmActivation() {
		return tmActivation;
	}

	/**
	 * @param tmActivation the tmActivation to set
	 */
	public void setTmActivation(String tmActivation) {
		this.tmActivation = tmActivation;
	}

	/**
	 * @return the tmExpiration
	 */
	public String getTmExpiration() {
		return tmExpiration;
	}

	/**
	 * @param tmExpiration the tmExpiration to set
	 */
	public void setTmExpiration(String tmExpiration) {
		this.tmExpiration = tmExpiration;
	}

	/**
	 * @return the pathConfigFile
	 */
	public String getPathConfigFile() {
		return pathConfigFile;
	}

	/**
	 * @param pathConfigFile the pathConfigFile to set
	 */
	public void setPathConfigFile(String pathConfigFile) {
		this.pathConfigFile = pathConfigFile;
	}

	/**
	 * @return the pathRoot
	 */
	public String getPathRoot() {
		return pathRoot;
	}

	/**
	 * @param pathRoot the pathRoot to set
	 */
	public void setPathRoot(String pathRoot) {
		this.pathRoot = pathRoot;
	}

	/**
	 * @return the pathUser
	 */
	public String getPathUser() {
		return pathUser;
	}

	/**
	 * @param pathUser the pathUser to set
	 */
	public void setPathUser(String pathUser) {
		this.pathUser = pathUser;
	}

	/**
	 * @return the pathPilot
	 */
	public String getPathPilot() {
		return pathPilot;
	}

	/**
	 * @param pathPilot the pathPilot to set
	 */
	public void setPathPilot(String pathPilot) {
		this.pathPilot = pathPilot;
	}

	/**
	 * @return the dirResources
	 */
	public String getDirResources() {
		return dirResources;
	}

	/**
	 * @param dirResources the dirResources to set
	 */
	public void setDirResources(String dirResources) {
		this.dirResources = dirResources;
	}

	/**
	 * @return the dirWork
	 */
	public String getDirWork() {
		return dirWork;
	}

	/**
	 * @param dirWork the dirWork to set
	 */
	public void setDirWork(String dirWork) {
		this.dirWork = dirWork;
	}

	/**
	 * @return the dirDatabase
	 */
	public String getDirDatabase() {
		return dirDatabase;
	}

	/**
	 * @param dirDatabase the dirDatabase to set
	 */
	public void setDirDatabase(String dirDatabase) {
		this.dirDatabase = dirDatabase;
	}

	/**
	 * @return the dirJclInput
	 */
	public String getDirJclInput() {
		return dirJclInput;
	}

	/**
	 * @param dirJclInput the dirJclInput to set
	 */
	public void setDirJclInput(String dirJclInput) {
		this.dirJclInput = dirJclInput;
	}

	/**
	 * @return the dirCobolSrcPgmInput
	 */
	public String getDirCobolSrcPgmInput() {
		return dirCobolSrcPgmInput;
	}

	/**
	 * @param dirCobolSrcPgmInput the dirCobolSrcPgmInput to set
	 */
	public void setDirCobolSrcPgmInput(String dirCobolSrcPgmInput) {
		this.dirCobolSrcPgmInput = dirCobolSrcPgmInput;
	}

	/**
	 * @return the dirCobolSrcCopyInput
	 */
	public String getDirCobolSrcCopyInput() {
		return dirCobolSrcCopyInput;
	}

	/**
	 * @param dirCobolSrcCopyInput the dirCobolSrcCopyInput to set
	 */
	public void setDirCobolSrcCopyInput(String dirCobolSrcCopyInput) {
		this.dirCobolSrcCopyInput = dirCobolSrcCopyInput;
	}

	/**
	 * @return the dirCobolPgm
	 */
	public String getDirCobolPgm() {
		return dirCobolPgm;
	}

	/**
	 * @param dirCobolPgm the dirCobolPgm to set
	 */
	public void setDirCobolPgm(String dirCobolPgm) {
		this.dirCobolPgm = dirCobolPgm;
	}

	/**
	 * @return the dirCobolCopy
	 */
	public String getDirCobolCopy() {
		return dirCobolCopy;
	}

	/**
	 * @param dirCobolCopy the dirCobolCopy to set
	 */
	public void setDirCobolCopy(String dirCobolCopy) {
		this.dirCobolCopy = dirCobolCopy;
	}

	/**
	 * @return the dirJcl
	 */
	public String getDirJcl() {
		return dirJcl;
	}

	/**
	 * @param dirJcl the dirJcl to set
	 */
	public void setDirJcl(String dirJcl) {
		this.dirJcl = dirJcl;
	}

	/**
	 * @return the dirSqlScript
	 */
	public String getDirSqlScript() {
		return dirSqlScript;
	}

	/**
	 * @param dirSqlScript the dirSqlScript to set
	 */
	public void setDirSqlScript(String dirSqlScript) {
		this.dirSqlScript = dirSqlScript;
	}

	/**
	 * @return the dirCobolGraph
	 */
	public String getDirCobolGraph() {
		return dirCobolGraph;
	}

	/**
	 * @param dirCobolGraph the dirCobolGraph to set
	 */
	public void setDirCobolGraph(String dirCobolGraph) {
		this.dirCobolGraph = dirCobolGraph;
	}

	/**
	 * @return the dirPilot
	 */
	public String getDirPilot() {
		return dirPilot;
	}

	/**
	 * @param dirPilot the dirPilot to set
	 */
	public void setDirPilot(String dirPilot) {
		this.dirPilot = dirPilot;
	}

	/**
	 * @return the dirLog
	 */
	public String getDirLog() {
		return dirLog;
	}

	/**
	 * @param dirLog the dirLog to set
	 */
	public void setDirLog(String dirLog) {
		this.dirLog = dirLog;
	}

	/**
	 * @return the dirOutput
	 */
	public String getDirOutput() {
		return dirOutput;
	}

	/**
	 * @param dirOutput the dirOutput to set
	 */
	public void setDirOutput(String dirOutput) {
		this.dirOutput = dirOutput;
	}

	/**
	 * @return the limitMaxLinesScanFindingSourceType
	 */
	public int getLimitMaxLinesScanFindingSourceType() {
		return limitMaxLinesScanFindingSourceType;
	}

	/**
	 * @param limitMaxLinesScanFindingSourceType the limitMaxLinesScanFindingSourceType to set
	 */
	public void setLimitMaxLinesScanFindingSourceType(int limitMaxLinesScanFindingSourceType) {
		this.limitMaxLinesScanFindingSourceType = limitMaxLinesScanFindingSourceType;
	}

	/**
	 * @return the limitMaxSources
	 */
	public boolean getLimitMaxSources() {
		return limitMaxSources;
	}

	/**
	 * @param limitMaxSources the limitMaxSources to set
	 */
	public void setLimitMaxSources(boolean limitMaxSources) {
		this.limitMaxSources = limitMaxSources;
	}

	/**
	 * @return the limitMaxSourcesInput
	 */
	public int getLimitMaxSourcesInput() {
		return limitMaxSourcesInput;
	}

	/**
	 * @param limitMaxSourcesInput the limitMaxSourcesInput to set
	 */
	public void setLimitMaxSourcesInput(int limitMaxSourcesInput) {
		this.limitMaxSourcesInput = limitMaxSourcesInput;
	}

	/**
	 * @return the limitMaxSourcesToProcess
	 */
	public int getLimitMaxSourcesToProcess() {
		return limitMaxSourcesToProcess;
	}

	/**
	 * @param limitMaxSourcesToProcess the limitMaxSourcesToProcess to set
	 */
	public void setLimitMaxSourcesToProcess(int limitMaxSourcesToProcess) {
		this.limitMaxSourcesToProcess = limitMaxSourcesToProcess;
	}

	/**
	 * @return the limitMaxObjects
	 */
	public boolean getLimitMaxObjects() {
		return limitMaxObjects;
	}

	/**
	 * @param limitMaxObjects the limitMaxObjects to set
	 */
	public void setLimitMaxObjects(boolean limitMaxObjects) {
		this.limitMaxObjects = limitMaxObjects;
	}

	/**
	 * @return the limitMaxObjectsInput
	 */
	public int getLimitMaxObjectsInput() {
		return limitMaxObjectsInput;
	}

	/**
	 * @param limitMaxObjectsInput the limitMaxObjectsInput to set
	 */
	public void setLimitMaxObjectsInput(int limitMaxObjectsInput) {
		this.limitMaxObjectsInput = limitMaxObjectsInput;
	}

	/**
	 * @return the limitMaxObjectsToProcess
	 */
	public int getLimitMaxObjectsToProcess() {
		return limitMaxObjectsToProcess;
	}

	/**
	 * @param limitMaxObjectsToProcess the limitMaxObjectsToProcess to set
	 */
	public void setLimitMaxObjectsToProcess(int limitMaxObjectsToProcess) {
		this.limitMaxObjectsToProcess = limitMaxObjectsToProcess;
	}

	/**
	 * @return the debugThresholdMemoryGarbage
	 */
	public int getDebugThresholdMemoryGarbage() {
		return debugThresholdMemoryGarbage;
	}

	/**
	 * @param debugThresholdMemoryGarbage the debugThresholdMemoryGarbage to set
	 */
	public void setDebugThresholdMemoryGarbage(int debugThresholdMemoryGarbage) {
		this.debugThresholdMemoryGarbage = debugThresholdMemoryGarbage;
	}

	/**
	 * @return the debugSourcesDetectedFreqGarbage
	 */
	public int getDebugSourcesDetectedFreqGarbage() {
		return debugSourcesDetectedFreqGarbage;
	}

	/**
	 * @param debugSourcesDetectedFreqGarbage the debugSourcesDetectedFreqGarbage to set
	 */
	public void setDebugSourcesDetectedFreqGarbage(int debugSourcesDetectedFreqGarbage) {
		this.debugSourcesDetectedFreqGarbage = debugSourcesDetectedFreqGarbage;
	}

	/**
	 * @return the debugActive
	 */
	public boolean getDebugActive() {
		return debugActive;
	}

	/**
	 * @param debugActive the debugActive to set
	 */
	public void setDebugActive(boolean debugActive) {
		this.debugActive = debugActive;
	}

	/**
	 * @return the logVerbose
	 */
	public boolean getLogVerbose() {
		return logVerbose;
	}

	/**
	 * @param logVerbose the logVerbose to set
	 */
	public void setLogVerbose(boolean logVerbose) {
		this.logVerbose = logVerbose;
	}

	/**
	 * @return the preferredVisitMethod
	 */
	public String getPreferredVisitMethod() {
		return preferredVisitMethod;
	}

	/**
	 * @param preferredVisitMethod the preferredVisitMethod to set
	 */
	public void setPreferredVisitMethod(String preferredVisitMethod) {
		this.preferredVisitMethod = preferredVisitMethod;
	}

	/**
	 * @return the preferredCachingLevel
	 */
	public String getPreferredCachingLevel() {
		return preferredCachingLevel;
	}

	/**
	 * @param preferredCachingLevel the preferredCachingLevel to set
	 */
	public void setPreferredCachingLevel(String preferredCachingLevel) {
		this.preferredCachingLevel = preferredCachingLevel;
	}

	/**
	 * @return the preferredCachingSupport
	 */
	public String getPreferredCachingSupport() {
		return preferredCachingSupport;
	}

	/**
	 * @param preferredCachingSupport the preferredCachingSupport to set
	 */
	public void setPreferredCachingSupport(String preferredCachingSupport) {
		this.preferredCachingSupport = preferredCachingSupport;
	}

	/**
	 * @return the dataBaseType
	 */
	public String getDataBaseType() {
		return dataBaseType;
	}

	/**
	 * @param dataBaseType the dataBaseType to set
	 */
	public void setDataBaseType(String dataBaseType) {
		this.dataBaseType = dataBaseType;
	}

	/**
	 * @return the dataBaseName
	 */
	public String getDataBaseName() {
		return dataBaseName;
	}

	/**
	 * @param dataBaseName the dataBaseName to set
	 */
	public void setDataBaseName(String dataBaseName) {
		this.dataBaseName = dataBaseName;
	}

	/**
	 * @return the dataBaseUser
	 */
	public String getDataBaseUser() {
		return dataBaseUser;
	}

	/**
	 * @param dataBaseUser the dataBaseUser to set
	 */
	public void setDataBaseUser(String dataBaseUser) {
		this.dataBaseUser = dataBaseUser;
	}

	/**
	 * @return the dataBasePwd
	 */
	public String getDataBasePwd() {
		return dataBasePwd;
	}

	/**
	 * @param dataBasePwd the dataBasePwd to set
	 */
	public void setDataBasePwd(String dataBasePwd) {
		this.dataBasePwd = dataBasePwd;
	}

	/**
	 * @return the dataBaseDriver
	 */
	public String getDataBaseDriver() {
		return dataBaseDriver;
	}

	/**
	 * @param dataBaseDriver the dataBaseDriver to set
	 */
	public void setDataBaseDriver(String dataBaseDriver) {
		this.dataBaseDriver = dataBaseDriver;
	}

	/**
	 * @return the dataBaseAccessType
	 */
	public String getDataBaseAccessType() {
		return dataBaseAccessType;
	}

	/**
	 * @param dataBaseAccessType the dataBaseAccessType to set
	 */
	public void setDataBaseAccessType(String dataBaseAccessType) {
		this.dataBaseAccessType = dataBaseAccessType;
	}

	/**
	 * @return the dataBaseUrl
	 */
	public String getDataBaseUrl() {
		return dataBaseUrl;
	}

	/**
	 * @param dataBaseUrl the dataBaseUrl to set
	 */
	public void setDataBaseUrl(String dataBaseUrl) {
		this.dataBaseUrl = dataBaseUrl;
	}

	/**
	 * @return the dataBaseMaxConn
	 */
	public int getDataBaseMaxConn() {
		return dataBaseMaxConn;
	}

	/**
	 * @param dataBaseMaxConn the dataBaseMaxConn to set
	 */
	public void setDataBaseMaxConn(int dataBaseMaxConn) {
		this.dataBaseMaxConn = dataBaseMaxConn;
	}

	/**
	 * @return the dataBaseCommitBlockUpdates
	 */
	public int getDataBaseCommitBlockUpdates() {
		return dataBaseCommitBlockUpdates;
	}

	/**
	 * @param dataBaseCommitBlockUpdates the dataBaseCommitBlockUpdates to set
	 */
	public void setDataBaseCommitBlockUpdates(int dataBaseCommitBlockUpdates) {
		this.dataBaseCommitBlockUpdates = dataBaseCommitBlockUpdates;
	}

	/**
	 * @return the dataBaseLogAnySql
	 */
	public boolean getDataBaseLogAnySql() {
		return dataBaseLogAnySql;
	}

	/**
	 * @param dataBaseLogAnySql the dataBaseLogAnySql to set
	 */
	public void setDataBaseLogAnySql(boolean dataBaseLogAnySql) {
		this.dataBaseLogAnySql = dataBaseLogAnySql;
	}

	/**
	 * @return the pilotDefaultSource
	 */
	public String getPilotDefaultSource() {
		return pilotDefaultSource;
	}

	/**
	 * @param pilotDefaultSource the pilotDefaultSource to set
	 */
	public void setPilotDefaultSource(String pilotDefaultSource) {
		this.pilotDefaultSource = pilotDefaultSource;
	}

	/**
	 * @return the pilotDefaultProcess
	 */
	public String getPilotDefaultProcess() {
		return pilotDefaultProcess;
	}

	/**
	 * @param pilotDefaultProcess the pilotDefaultProcess to set
	 */
	public void setPilotDefaultProcess(String pilotDefaultProcess) {
		this.pilotDefaultProcess = pilotDefaultProcess;
	}

	/**
	 * @return the userExitClass
	 */
	public String getUserExitClass() {
		return userExitClass;
	}

	/**
	 * @return the baseUrl
	 */
	public String getBaseUrl() {
		return baseUrl;
	}

	/**
	 * @param baseUrl the baseUrl to set
	 */
	public void setBaseUrl(String baseUrl) {
		this.baseUrl = baseUrl;
	}

	/**
	 * @param userExitClass the userExitClass to set
	 */
	public void setUserExitClass(String userExitClass) {
		this.userExitClass = userExitClass;
	}


}
