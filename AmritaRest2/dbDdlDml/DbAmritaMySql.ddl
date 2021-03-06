

SET GLOBAL connect_timeout=28800;
SET GLOBAL wait_timeout=28800;
SET GLOBAL interactive_timeout=28800;

-- ---------------------------------------------------------------
-- User
-- Describes a single user
-- ---------------------------------------------------------------
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS User;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE User (

-- Primary Key
	 user   CHAR(10)                  NOT NULL DEFAULT ' ' COMMENT 'Codice Utente (mail)'
	
-- Data user  
	,baseUrl VARCHAR(600)             NOT NULL DEFAULT ' ' COMMENT 'Url di base'  
	,companyCode CHAR(10)             NOT NULL DEFAULT ' ' COMMENT 'Codice societa' 
	,company VARCHAR(30)              NOT NULL DEFAULT ' ' COMMENT 'Descrizione societa' 
	,userType SMALLINT    		  	  NOT NULL DEFAULT 0   COMMENT 'Tipo utente EnumUserType (T052)'
	,userStatus SMALLINT    		  NOT NULL DEFAULT 0   COMMENT 'Tipo utente EnumUserStatus (T051)'
	,country CHAR(2)    			  NOT NULL DEFAULT ' ' COMMENT 'Country IT, ..' 
	,language CHAR(2)    			  NOT NULL DEFAULT ' ' COMMENT 'language it, ..' 
	,pwd CHAR(10)    				  NOT NULL DEFAULT ' ' COMMENT 'Password' 
	,mail VARCHAR(30)                 NOT NULL DEFAULT ' ' COMMENT 'Mail principale' 
	,mailInfo VARCHAR(30)             NOT NULL DEFAULT ' ' COMMENT 'Mail per info anomalie' 
	,phone VARCHAR(30)                NOT NULL DEFAULT ' ' COMMENT 'Telefono di riferimento' 
	,referManager VARCHAR(30)         NOT NULL DEFAULT ' ' COMMENT 'Riferimento manager' 
	,referTech VARCHAR(30)            NOT NULL DEFAULT ' ' COMMENT 'Riferimento tecnico' 
	,analyzerEnabled BOOLEAN          NOT NULL DEFAULT 0   COMMENT 'True user can view Analyzer'
	,viewerEnabled BOOLEAN            NOT NULL DEFAULT 0   COMMENT 'True user can view Viewer'
	,inspectorEnabled BOOLEAN         NOT NULL DEFAULT 0   COMMENT 'True user can view Inspector'
	,assesmentEnabled BOOLEAN         NOT NULL DEFAULT 0   COMMENT 'True user can view Assesment'
	,countLogin SMALLINT              NOT NULL DEFAULT 0   COMMENT 'Counter login effettuati' 
	,dtActivation CHAR(8)             NOT NULL DEFAULT ' ' COMMENT 'Data attivazione AAAAMMGG'
	,tmActivation CHAR(8)             NOT NULL DEFAULT ' ' COMMENT 'Ora  attivazione HHMMSSCC'
	,dtExpiration CHAR(8)             NOT NULL DEFAULT ' ' COMMENT 'Data disattivazione AAAAMMGG'
	,tmExpiration CHAR(8)             NOT NULL DEFAULT ' ' COMMENT 'Ora  disattivazione HHMMSSCC'
	,dtFirstLogin CHAR(8)             NOT NULL DEFAULT ' ' COMMENT 'Data primo login AAAAMMGG'
	,tmFirstLogin CHAR(8)             NOT NULL DEFAULT ' ' COMMENT 'Ora  primo login HHMMSSCC	'
	,dtLastLogin CHAR(8)              NOT NULL DEFAULT ' ' COMMENT 'Data ultimo login AAAAMMGG'
	,tmLastLogin CHAR(8)              NOT NULL DEFAULT ' ' COMMENT 'Ora  ultimo login HHMMSSCC'

	--  
	-- Configurazione
	--  

    ,pathConfigFile 				VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Path completo file di configurazione'
    ,pathRoot  						VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Phisical path to WEB-INF or application Installation'
    ,pathUser  						VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Phisical path to user  directory'
    ,pathPilot 						VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Phisical path to pilot default'
 	
	-- Directories relative a root o WEB-INF da file di configurazione generale
	,dirResources 					VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Resource'
	,dirWork  						VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Working e temporaneo'		    
	,dirDatabase 					VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Database'		              
	,dirJclInput 					VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Jcl    in input al processo di analisi (.*) '       
	,dirCobolSrcPgmInput 			VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Pgm    Cobol sorgenti in analisi		  (.*) '
	,dirCobolSrcCopyInput 			VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Copy   Cobol sorgenti in analisi		  (.*)'
	,dirCobolPgm 					VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Pgm    Cobol codificati e serializzati (.program)'			 
	,dirCobolCopy 					VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Copy   Cobol codificati e serializzati (.copy)'	
	,dirJcl 						VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Jcl    codificati e serializzati (.jclSource, .jclInclude, .jclProc)'		 
	,dirSqlScript 					VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Script Sql codificati e serializzati   (.scriptSql)'			 
	,dirCobolGraph 					VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Grafi  Cobol codificati e serializzati (.graph)'	
	,dirPilot 						VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Pilot  sources e processi e filtri     (.pilot)'
	,dirLog 						VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Log'
	,dirOutput 						VARCHAR(250) NOT NULL DEFAULT ' ' COMMENT 'Output per funzioni che producono text'

	-- Ottimizzazione processi ed elaborazioni, allocazione di arrays, collections, map
	,limitMaxLinesScanFindingSourceType SMALLINT NOT NULL DEFAULT 0 COMMENT 'Numero massimo righe da analizzare per individuare il tipo sorgente (200)'
	,limitMaxSources 				BOOLEAN   NOT NULL DEFAULT 0   COMMENT 'Abilitazione limitazione ai sources trattati'
	,limitMaxSourcesInput 			SMALLINT  NOT NULL DEFAULT 0   COMMENT 'Numero massimo sorgenti da considerare in input'
	,limitMaxSourcesToProcess 		SMALLINT  NOT NULL DEFAULT 0   COMMENT 'Numero massimo sorgenti in input dei quali è stato intercettato il tipo'
	,limitMaxObjects 				BOOLEAN   NOT NULL DEFAULT 0   COMMENT 'Abilitazione limitazione agli oggetti processati'
	,limitMaxObjectsInput 			SMALLINT  NOT NULL DEFAULT 0   COMMENT 'Numero massimo oggetti da considerare in input ai processi (filtrati)'
	,limitMaxObjectsToProcess 		SMALLINT  NOT NULL DEFAULT 0   COMMENT 'Numero massimo oggetti in input da processare'
	,debugThresholdMemoryGarbage 	INT       NOT NULL DEFAULT 0   COMMENT 'Attivazione gc() se memoria disponibile <'
	,debugSourcesDetectedFreqGarbage SMALLINT NOT NULL DEFAULT 0   COMMENT 'gc() attivata ogni 200 sorgenti'
	,debugActive 					BOOLEAN   NOT NULL DEFAULT 0   COMMENT 'Attivazione debug dove previsto (messaggi log di debug)'
	,logVerbose 					BOOLEAN   NOT NULL DEFAULT 0   COMMENT 'Dettaglio log operazioni Sql e informative'
	,preferredVisitMethod 			CHAR(10)  NOT NULL DEFAULT ' ' COMMENT 'Metodo di visita predefinito (BACKWARD)'
	,preferredCachingLevel 			CHAR(20)  NOT NULL DEFAULT ' ' COMMENT 'Livello di caching predefinito (CACHING_PATH_ALL)'
	,preferredCachingSupport 		CHAR(20)  NOT NULL DEFAULT ' ' COMMENT 'Tipo supporto java di cache (CACHING_ON_HASH_MAP, CACHING_ON_TREE_MAP)'
	
	-- Database
	,dataBaseType 					CHAR(8)   NOT NULL DEFAULT ' ' COMMENT 'Tipologia database MSACCESS/MYSQL'
	,dataBaseName 					CHAR(10)  NOT NULL DEFAULT ' ' COMMENT 'Nome database (DbAmrita)'
	,dataBaseUser 					CHAR(10)  NOT NULL DEFAULT ' ' COMMENT 'User (GZEDDA)'
	,dataBasePwd 					CHAR(15)  NOT NULL DEFAULT ' ' COMMENT 'Pwd (giampietro4)'
	,dataBaseDriver 				CHAR(30)  NOT NULL DEFAULT ' ' COMMENT 'Driver (com.mysql.cj.jdbc.Driver)'
	,dataBaseAccessType 			CHAR(10)  NOT NULL DEFAULT ' ' COMMENT 'Accesso LOCAL/REMOTE'
	,dataBaseUrl 					VARCHAR(200) NOT NULL DEFAULT ' ' COMMENT 'jdbc:mysql://localhost:3306/Amrita?autoReconnect=true&useSSL=false&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC'		 	
	,dataBaseMaxConn 				SMALLINT NOT NULL DEFAULT 0   COMMENT 'Numero massimo connessioni attive (1)'
	,dataBaseCommitBlockUpdates 	SMALLINT NOT NULL DEFAULT 0   COMMENT 'Commit automatica a fine gruppo aggiornamenti (100)'
	,dataBaseLogAnySql 				BOOLEAN  NOT NULL DEFAULT 0   COMMENT 'Log istruzioni Sql come messaggi informativi'
    	
	-- Controllo analisi, identificazione oggetti analizzati/processati, piloti sources, filtri e processi'
	,pilotDefaultSource 			VARCHAR(100) NOT NULL DEFAULT ' ' COMMENT 'Pilota sorgenti (PilotDefaultSource.pilot)'
	,pilotDefaultProcess 			VARCHAR(100) NOT NULL DEFAULT ' ' COMMENT 'Pilota processi (PilotDefaultProcess.pilot)'
	,userExitClass 					VARCHAR(40)  NOT NULL DEFAULT ' ' COMMENT 'Classe con exit applicative codificate (UserExit)'
 	
-- Constraints	
	,CONSTRAINT PrimaryKey PRIMARY KEY (user)
)
COMMENT 'Describes every user '
;
INSERT INTO `user` (`user`,`baseUrl`,`companyCode`,`company`,`userType`,`userStatus`,`country`,`language`,`pwd`,`mail`,`mailInfo`,`phone`,`referManager`,`referTech`,`analyzerEnabled`,`viewerEnabled`,`inspectorEnabled`,`assesmentEnabled`,`countLogin`,`dtActivation`,`tmActivation`,`dtExpiration`,`tmExpiration`,`dtFirstLogin`,`tmFirstLogin`,`dtLastLogin`,`tmLastLogin`,`pathConfigFile`,`pathRoot`,`pathUser`,`pathPilot`,`dirResources`,`dirWork`,`dirDatabase`,`dirJclInput`,`dirCobolSrcPgmInput`,`dirCobolSrcCopyInput`,`dirCobolPgm`,`dirCobolCopy`,`dirJcl`,`dirSqlScript`,`dirCobolGraph`,`dirPilot`,`dirLog`,`dirOutput`,`limitMaxLinesScanFindingSourceType`,`limitMaxSources`,`limitMaxSourcesInput`,`limitMaxSourcesToProcess`,`limitMaxObjects`,`limitMaxObjectsInput`,`limitMaxObjectsToProcess`,`debugThresholdMemoryGarbage`,`debugSourcesDetectedFreqGarbage`,`debugActive`,`logVerbose`,`preferredVisitMethod`,`preferredCachingLevel`,`preferredCachingSupport`,`dataBaseType`,`dataBaseName`,`dataBaseUser`,`dataBasePwd`,`dataBaseDriver`,`dataBaseAccessType`,`dataBaseUrl`,`dataBaseMaxConn`,`dataBaseCommitBlockUpdates`,`dataBaseLogAnySql`,`pilotDefaultSource`,`pilotDefaultProcess`,`userExitClass`) 
            VALUES ('amrita',' http://localhost:8080/AmritaRest2','VN',' Amrita Test (Valbruna)',1,1,'IT','it','amrita','giampietro.zedda@libero.it','giampietro.zedda@libero.it',' ','Giampietro Zedda','Giampietro Zedda',1,1,1,1,1,'20201201','172300','','','20201212','180000','20201512','130000','/users/amrita ','/','c:/amrita/users/amrita','/pilot','/resources','/work',' ','/src/jcl','/src/cobol/pgm','/src/cobol/copy','/objJava/cobol/pgm','/objJava/cobol/copy','/objJava/jcl','/src/sqlScript','/objJava/cobol/graph','/pilot','/log','/output',200,0,0,10,0,0,0,15000,500,1,1,'','','','DB_MYSQ','amrita','GZEDDA','giampietro4','com.mysql.cj.jdbc.Driver','LOCAL','jdbc:mysql://localhost:3306/Amrita?autoReconnect=true&rewriteBatchedStatements=true&useServerPrepStmts=true&useSSL=false&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC',3,100,0,'PilotDefaultSource.pilot','FileSourcesDetected.txt ','UserExit');


-- ---------------------------------------------------------------
-- Object
-- Describes every application object analyzed and/or created
-- ---------------------------------------------------------------
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS Object;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE Object (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys CHAR(2)                   NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject VARCHAR(255)            NOT NULL COMMENT 'Nome oggetto (es. programma, file fisico,..)'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'
	,typeSource SMALLINT              NOT NULL COMMENT 'Tipologia sorgente (T0002)'	
	
-- Data
	,idObjectSource CHAR(8)           NOT NULL COMMENT 'Nome oggetto sorgente da cui questo oggetto è derivato (es. OBJECT_MAP)'
	,idObjectExtended VARCHAR(255)    NOT NULL COMMENT 'Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel)' 
	,librarySourceObject VARCHAR(255) NOT NULL COMMENT 'Nome oggetto LIBRARY libreria sorgente' 
	,libraryDir VARCHAR(512)          NOT NULL COMMENT 'Directory libreria per oggetti LIBRARY'
	,librarySource VARCHAR(512)       NOT NULL COMMENT 'Libreria sorgente di analisi (Per oggetti direttamente dedotti da un file sorgente)'
	,fileSource VARCHAR(255)          NOT NULL COMMENT 'Nome file sorgente di analisi'
	,suffixFileSource VARCHAR(20)     NOT NULL COMMENT 'Suffisso file sorgente di analisi dopo'  
	,statusObject SMALLINT            NOT NULL COMMENT 'Stato oggetto (T0003)'
	,author CHAR(10)                  NOT NULL COMMENT 'Autore oggetto analizzato'
	,idObjectDescriptor VARCHAR(255)  NOT NULL COMMENT 'Nome oggetto descrittore. Per esempio un copy per i file'
	,typeObjectDescriptor SMALLINT    NOT NULL COMMENT 'Tipologia oggetto descrittore (T0001)	'
	,sysOwner CHAR(2)                 NOT NULL COMMENT 'Sistema applicativo proprietario'
	,subSysOwner CHAR(2)              NOT NULL COMMENT 'Sotto sistema applicativo proprietario'
	,dateWritten CHAR(8)              NOT NULL COMMENT 'Data scrittura (es. da date-written Cobol)'
	,pathDocFile VARCHAR(255)         NOT NULL COMMENT 'Path completo file documentazione'
	,dtFirstAnalysis CHAR(8)          NOT NULL COMMENT 'Data prima analisi AAAAMMGG'
	,tmFirstAnalysis CHAR(8)          NOT NULL COMMENT 'Ora  prima analisi HHMMSSCC	'
	,dtLastAnalysis CHAR(8)           NOT NULL COMMENT 'Data ultima analisi AAAAMMGG'
	,tmLastAnalysis CHAR(8)           NOT NULL COMMENT 'Ora  ultima analisi HHMMSSCC'
	
-- Constraints	
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, typeSource)
)
COMMENT 'Describes every application object analyzed and/or created'
; 
-- Used in sources extraction by type 
ALTER TABLE Object
  ADD INDEX Object_I01 
  (typeObject,  sys,  subSys, statusObject, idObject);
  
-- Used to locate by status 
ALTER TABLE Object
  ADD INDEX Object_I02 
  (idObject, typeObject,  sys,  subSys, statusObject);
	 
-- Used to locate by name 
ALTER TABLE Object
  ADD INDEX Object_I03 
  (statusObject, idObject, typeObject,  sys,  subSys);
	 
--
-- Describes dimensional and consumptive informations of an analyzed source object,
-- as source rows number, comments, empty rows and elapsed time of any analysis process.
-- Completes object informations stored on Object table.
--
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ObjectAnalysisInfo;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE ObjectAnalysisInfo (

-- Primary Key
	 sys    CHAR(2)                    NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                  NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject VARCHAR(255)             NOT NULL COMMENT 'Nome oggetto (es. programma, file fisico,..)'
	,typeObject SMALLINT               NOT NULL COMMENT 'Tipologia oggetto (T0001)'
	,typeSource SMALLINT               NOT NULL COMMENT 'Tipologia sorgente (T0002)'	

-- Data, Info dimensionali	
	,sizeBytes INT                     NOT NULL COMMENT 'Dimensioni in bytes del file (esclusi caratteri di controllo)'
	,numRowsTot INT                    NOT NULL COMMENT 'Numero totale righe totali incluse quelle vuote'
	,numRowsCommTot SMALLINT           NOT NULL COMMENT 'Numero totale righe commento'
	,numRowsEmptyTot SMALLINT          NOT NULL COMMENT 'Numero totale righe vuote (carriage return o tutti spaces)'
	,numRowsBlankTot SMALLINT          NOT NULL COMMENT 'Numero totale righe vuote (space a 7-72 X Cobol)'
	,numRowsCommData SMALLINT          NOT NULL COMMENT 'Numero commenti in Data Division Cobol'
	,numRowsData SMALLINT              NOT NULL COMMENT 'Numero righe sorgente in procedure division Cobol'
	,numRowsEmptyData SMALLINT         NOT NULL COMMENT 'Numero totale righe vuote in Data Division Cobol(carriage return)'
	,numRowsBlankData SMALLINT         NOT NULL COMMENT 'Numero totale righe vuote in Data Division Cobol (space a 7-72)'
	,numRowsCommProc SMALLINT          NOT NULL COMMENT 'Numero commenti in Procedure Division Cobol'  
	,numRowsProc SMALLINT              NOT NULL COMMENT 'Numero righe sorgente in procedure division'
	,numRowsEmptyProc SMALLINT         NOT NULL COMMENT 'Numero totale righe vuote in Procedure Division Cobol (carriage return)'
	,numRowsBlankProc SMALLINT         NOT NULL COMMENT 'Numero totale righe vuote in Procedure Division Cobol(space a 7-72)'
	,numStmtProc SMALLINT              NOT NULL COMMENT 'Numero totale statements in Procedure Division Cobol'
	,numDefData SMALLINT               NOT NULL COMMENT 'Numero totale definizioni in Data Division Cobol, incluse quelle nei copy'
	,numDefDataProgram SMALLINT        NOT NULL COMMENT 'Numero totale definizioni in Data Division Cobol, nel source del programma'
	,numDefDataCopy SMALLINT           NOT NULL COMMENT 'Numero totale definizioni in Data Division, nei copy richiamati'
	
-- Data, Info data, ora e tempi di esecuzione		
	,dtAnalysis CHAR(8)                NOT NULL COMMENT 'Data analisi'
	,tmStartAnalysis CHAR(10)          NOT NULL COMMENT 'Ora inizio analisi HHMMSSCC	'
	,tmEndAnalysis CHAR(10)            NOT NULL COMMENT 'Ora Fine analisi HHMMSSCC'
	,msElapsedTot INT                  NOT NULL COMMENT 'ms elapsed di analisi totali'
	,msParsing INT                     NOT NULL COMMENT 'ms di parsing sorgente'
	,msPostParsingOperations INT       NOT NULL COMMENT 'ms di operazioni finali post parsing sorgente'
	,msGraphCreation INT               NOT NULL COMMENT 'ms di creazione grafi e sottografi di programma'
	,msDynamicCodeSolving INT          NOT NULL COMMENT 'ms di soluzione codice dinamico del programma'
	,msMetricComputing INT             NOT NULL COMMENT 'ms elapsed di calcolo metriche'
	,msViolationDetecting INT          NOT NULL COMMENT 'ms di individuazione violazioni alle metriche'
	,msSerializationPgm INT            NOT NULL COMMENT 'ms di serializzazione pgm su disco'
	,msDbDelete INT                    NOT NULL COMMENT 'ms di delete data base'
	,msDbUpdate INT                    NOT NULL COMMENT 'ms di update data base'
    ,msUpdObject INT                   NOT NULL COMMENT 'ms di update Object aggiornati'
    ,msInsRelation INT                 NOT NULL COMMENT 'ms di insert relazioni inserite'
    ,msInsRelationOrigin INT           NOT NULL COMMENT 'ms totale relazioni inserite'
    ,msInsWhereUsed INT                NOT NULL COMMENT 'ms di insert wehereUsed inserite'
    ,msInsCopyEntity INT               NOT NULL COMMENT 'ms di insert copyEntityDefinition inserite'
    ,msInsMetric INT                   NOT NULL COMMENT 'ms di insert Metric inserite'
    ,msInsMetricViolation INT          NOT NULL COMMENT 'ms di insert MetricViolation inserite'
    ,msInsDynamicField INT             NOT NULL COMMENT 'ms di insert DynamicField inserite'
    ,cntInsObject INT                  NOT NULL COMMENT 'Counter Object inseriti'
    ,cntUpdObject INT                  NOT NULL COMMENT 'Counter Object aggiornati'
    ,cntInsRelation INT                NOT NULL COMMENT 'Counter relazioni inserite'
    ,cntInsRelationOrigin INT          NOT NULL COMMENT 'Counter relazioni origine inserite'
    ,cntInsWhereUsed INT               NOT NULL COMMENT 'Counter wehereUsed inserite'
    ,cntInsCopyEntity INT              NOT NULL COMMENT 'Counter copyEntityDefinition inserite'
    ,cntInsMetric INT                  NOT NULL COMMENT 'Counter Metric inserite'
    ,cntInsMetricViolation INT         NOT NULL COMMENT 'Counter MetricViolation inserite'
    ,cntInsDynamicField INT            NOT NULL COMMENT 'Counter DynamicField inserite'
 	
-- Data, Info tipologie errori di parsing presenti		
    ,withException BOOLEAN             NOT NULL COMMENT 'True (1) Analisi terminata con exception'
	,withAnyCobolParsingErrors BOOLEAN NOT NULL COMMENT 'True (1) indica qualche istruzione Cobol con errori di parsing'
	,withAnySqlParsingErrors BOOLEAN   NOT NULL COMMENT 'True (1) indica qualche istruzione Sql   con errori di parsing'
	,withAnyCicsParsingErrors BOOLEAN  NOT NULL COMMENT 'True (1) indica qualche istruzione Cics  con errori di parsing'
	,withAnyDL1ParsingErrors BOOLEAN   NOT NULL COMMENT 'True (1) indica qualche istruzione DL1   con errori di parsing'
			
-- Constraints		
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, typeSource)
	,CONSTRAINT Fk03       FOREIGN KEY (sys, subSys, idObject, typeObject, typeSource) 
	                  REFERENCES Object(sys, subSys, idObject, typeObject, typeSource)
)
COMMENT 'Describes dimensional and consumptive informations of an analyzed source object, as source rows number, comments, empty rows and elapsed time of any analysis process.'
;

--
-- Describes errors and warnings detected during the source analysis,
-- as the active program area, row number, last good instruction and
-- the exception stack under abnormal termination with detailed informations.
--
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ObjectAnalysisError;
SET FOREIGN_KEY_CHECKS = 1;
CREATE TABLE ObjectAnalysisError (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject VARCHAR(255)            NOT NULL COMMENT 'Nome oggetto (es. programma, file fisico,..)'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'
	,typeSource SMALLINT              NOT NULL COMMENT 'Tipologia sorgente (T0002)'	
	,rowNum SMALLINT                  NOT NULL COMMENT 'Numero riga sorgente in errore '

-- Data
	,typeProcessAnalysis SMALLINT     NOT NULL COMMENT 'Tipo processo attivo al momento dell''errore (T0025)'
	,activeCobolDivision SMALLINT     NOT NULL COMMENT 'Divisione cobol attiva (T0029)'
	,activeCobolSection SMALLINT      NOT NULL COMMENT 'Section cobol attiva come Workink, Linkage, etc. (T0029)'
	,typeInstr SMALLINT               NOT NULL COMMENT 'Categoria istruzione in errore (T0018)'
	,copyName VARCHAR(30)             NOT NULL COMMENT 'Copy in errore (stringa vuota se su programma) '
	,numInstr SMALLINT                NOT NULL COMMENT 'Numero istruzione errata'
	,sourceInstr VARCHAR(4000)        NOT NULL COMMENT 'Source istruzione errata'
	,sourceInstrLastGood VARCHAR(4000) NOT NULL COMMENT 'Source ultima istruzione analizzata senza errori'
	,tokenError VARCHAR(300)          NOT NULL COMMENT 'Token in errore'
	,rowNumInstrBegin SMALLINT        NOT NULL COMMENT 'Numero riga inizio istruzione (0-based)'
	,rowNumInstrFinal SMALLINT        NOT NULL COMMENT 'Numero riga fine istruzione (0-based)'
	,msgType SMALLINT                 NOT NULL COMMENT 'Tipo messaggio (T0024)'
	,msgCode VARCHAR(10)              NOT NULL COMMENT 'Codice messaggio di errore o informativo o di warning'
	,msgText VARCHAR(255)             NOT NULL COMMENT 'Testo messaggio di errore o informativo o di warning'
	,stackTrace VARCHAR(4000)         NOT NULL COMMENT 'Stack trace completo'
	,excpDescription VARCHAR(1000)    NOT NULL COMMENT 'Descritione exception'
	,excpWhenCopyAnalysis BOOLEAN     NOT NULL COMMENT 'True (1) indica exception durante analisi modulo copy analizzato contestualmente al pgm'
	,excp BOOLEAN                     NOT NULL COMMENT 'True (1) indica entry a fronte di exception'
	,parsingError BOOLEAN             NOT NULL COMMENT 'True (1) indica parsing error su istruzione'
	,semanticError BOOLEAN            NOT NULL COMMENT 'True (1) indica semantic error su istruzione'
	,warning BOOLEAN                  NOT NULL COMMENT 'True (1) indica warning su istruzione'
		
-- Constraints		
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, typeSource, rowNum)
	,CONSTRAINT Fk02       FOREIGN KEY (sys, subSys, idObject, typeObject, typeSource) 
	                  REFERENCES Object(sys, subSys, idObject, typeObject, typeSource)
)
COMMENT 'Describes errors and warnings detected during the source analysis, stack trace and problem determination informations'
;

--
-- Describes a characteristic, an option of the object
--
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ObjectOption;
SET FOREIGN_KEY_CHECKS = 1;
CREATE TABLE ObjectOption (

-- Primary Key
	 sys    CHAR(2)                  NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject VARCHAR(255)           NOT NULL COMMENT 'Nome oggetto (es. programma, file fisico,..)'
	,typeObject SMALLINT             NOT NULL COMMENT 'Tipologia oggetto (T0001)'
	,typeSource SMALLINT             NOT NULL COMMENT 'Tipologia sorgente (T0002)'	
	,optionObject SMALLINT           NOT NULL COMMENT 'Opzione codificata (T0004)'		

-- Data	
	,textValue VARCHAR(255)          NOT NULL COMMENT 'Info specifiche acquisite in analisi (nick text X visualizzazione) o User Tag	'
		
-- Constraints		
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, typeSource, optionObject)
	,CONSTRAINT Fk01       FOREIGN KEY (sys, subSys, idObject, typeObject, typeSource) 
	                  REFERENCES Object(sys, subSys, idObject, typeObject, typeSource)
)
COMMENT 'Describes a characteristic, an option of the object'
;

--
-- Describes an Sql index  
--
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS IndexItem;
SET FOREIGN_KEY_CHECKS = 1;
CREATE TABLE IndexItem (

-- Primary Key
	 sys    CHAR(2)                  NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject VARCHAR(255)           NOT NULL COMMENT 'Nome oggetto index'
	,typeObject SMALLINT             NOT NULL COMMENT 'Tipologia oggetto (T0001)'
	,idObjectOwner VARCHAR(255)      NOT NULL COMMENT 'Nome oggetto proprietario dell''indice Entity, Logical/Phisical File Vsam'
	,typeObjectOwner SMALLINT        NOT NULL COMMENT 'Tipologia oggetto proprietario(T0001)'
	,numSeq SMALLINT                 NOT NULL COMMENT 'Numero sequenza definizione idObjectOwner'

-- Data	
	,orderType SMALLINT                  NOT NULL COMMENT 'Ordinamento Ascending/Descending/.. (T0041)'
	,idColumnName VARCHAR(255)       NOT NULL COMMENT 'Nome campo indice come definito (ridondante)'
		
-- Constraints		
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, idObjectOwner, numSeq)
	,CONSTRAINT Fk30       FOREIGN KEY (sys, subSys, idObject, typeObject) 
	                  REFERENCES Object(sys, subSys, idObject, typeObject)
)
COMMENT 'Describes an index of a aql table'
;

--
-- Describes a tag value found in source process analysis
--
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS EntityTagValue;
SET FOREIGN_KEY_CHECKS = 1;
CREATE TABLE EntityTagValue (

-- Primary Key
	 sys CHAR(2)                  		NOT NULL COMMENT 'Sistema applicativo'
	,subSys CHAR(2)                		NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject VARCHAR(255)      NOT NULL COMMENT 'Identificativo categoria tag utente'
	,typeObject SMALLINT        NOT NULL COMMENT 'Tipologia identificativo (T0001)'
 	,progr SMALLINT                 	NOT NULL COMMENT 'Numero sequenza progressivo valore'

-- Data	
	,tagValue VARCHAR(10)       			NOT NULL COMMENT 'Valore tag da cercare nei sorgenti'
	,tagLength SMALLINT                 	NOT NULL COMMENT 'Lunghezza tag'
		
-- Constraints		
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, progr)

)
COMMENT 'Tag value found in source process analysis'


--
-- Describes a relationship between two objects.

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS Relation;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE Relation ( 

-- Primary Key
	 sys      CHAR(2)                  NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                  NOT NULL COMMENT 'Sotto sistema applicativo'
	,relation SMALLINT                 NOT NULL COMMENT 'Relazione codificata fra oggetti A e B (T0034)		'
	,idObjectA VARCHAR(255)            NOT NULL COMMENT 'Nome oggetto A(es. programma, file fisico,..)'
    ,typeObjectA SMALLINT              NOT NULL COMMENT 'Tipo oggetto A  (T0001)'
	,typeSourceA SMALLINT              NOT NULL COMMENT 'Tipologia sorgente (T0002)'	
	,idObjectB VARCHAR(255)            NOT NULL COMMENT 'Nome oggetto B(es. programma, file fisico,..)'
	,typeObjectB SMALLINT              NOT NULL COMMENT 'Tipo oggetto B  (T0001)'
	,typeSourceB SMALLINT              NOT NULL COMMENT 'Tipologia sorgente (T0002)'	
	
-- Constraints		
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, relation, idObjectA, typeObjectA, typeSourceA, idObjectB, typeObjectB, typeSourceB)
	,CONSTRAINT Fk04       FOREIGN KEY (sys, subSys, idObjectA, typeObjectA, typeSourceA) 
	                  REFERENCES Object(sys, subSys, idObject, typeObject, typeSource)
	,CONSTRAINT Fk05       FOREIGN KEY (sys, subSys, idObjectB, typeObjectB, typeSourceB)
	                  REFERENCES Object(sys, subSys, idObject, typeObject, typeSource)
)
COMMENT 'Describes a relationship between two objects.'
;
-- To get all relations related to
ALTER TABLE Relation
  ADD INDEX Relation_I01 
  (Relation,  sys,  subSys, idObjectA, typeObjectA);
-- To get all relations related with
ALTER TABLE Relation
  ADD INDEX Relation_I02 
  (Relation,  sys,  subSys, idObjectB, typeObjectB);
  
--
-- Describes the origin of a relationship between two objects.
--
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS RelationOrigin;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE RelationOrigin (

-- Primary Key
	 sys      CHAR(2)                  NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                  NOT NULL COMMENT 'Sotto sistema applicativo'
	,relation SMALLINT                 NOT NULL COMMENT 'Relazione codificata fra oggetti A e B (T0034)		'
	,idObjectA VARCHAR(255)            NOT NULL COMMENT 'Nome oggetto A(es. programma, file fisico,..)'
    ,typeObjectA SMALLINT              NOT NULL COMMENT 'Tipo oggetto A  (T0001)'
	,typeSourceA SMALLINT              NOT NULL COMMENT 'Tipologia sorgente (T0002)'	
	,idObjectB VARCHAR(255)            NOT NULL COMMENT 'Nome oggetto B(es. programma, file fisico,..)'
	,typeObjectB SMALLINT              NOT NULL COMMENT 'Tipo oggetto B  (T0001)'
	,typeSourceB SMALLINT              NOT NULL COMMENT 'Tipologia sorgente (T0002)'	
	,numInstrOrigin SMALLINT           NOT NULL COMMENT 'Numero istruzione origine in pgm A (proc/env/data) o numero progressivo origine se relazione indiretta'
	
-- Data, valido per tutti i tipi di relazione	
	,relationType SMALLINT             NOT NULL COMMENT 'Tipo relazione 	(T0005)'
	,instrProgramArea SMALLINT         NOT NULL COMMENT 'Area di programma istruzione (T0029)'
	,instrCategory SMALLINT            NOT NULL COMMENT 'Categoria istruzione (procedure o non) (T0018)'
	,relationSource SMALLINT           NOT NULL COMMENT 'Sorgente relazione 	(T0006)'
	,instrLang SMALLINT                NOT NULL COMMENT 'Linguaggio istruzione origine (Cobol,.. Pl1) (T0007)'
	,instrTypePrecompiler SMALLINT     NOT NULL COMMENT 'Tipo istruzione precompilatore (T0009)'
	,idObjectOrigin VARCHAR(255)       NOT NULL COMMENT 'Nome oggetto origine della relazione (può coincidere con idObjectRelA)'
	,typeObjectOrigin SMALLINT         NOT NULL COMMENT 'Tipo oggetto origine (T0001)'
	,rowStart SMALLINT         		   NOT NULL COMMENT 'Riga sorgente inizio istruzione (o copy) nel pgm'
	,rowEnd SMALLINT         		   NOT NULL COMMENT 'Riga sorgente fine istruzione (o copy) nel pgm'
	,rowStartInCopy SMALLINT           NOT NULL COMMENT 'Riga sorgente inizio istruzione dentro copy'
	,rowEndInCopy SMALLINT         	   NOT NULL COMMENT 'Riga sorgente fine istruzione dentro copy'
	,copyOrigin CHAR(8)                NOT NULL COMMENT 'Copy dentro cui la relazione ha origine'
	 
-- Data, valido solo per alcuni tipi di relazione		
	,librarySourceObject VARCHAR(40)   NOT NULL COMMENT 'Nome oggetto LIBRARY libreria '
	,librarySourcePath VARCHAR(512)    NOT NULL COMMENT 'Libreria sorgente (per esempio di analisi)'
	,fileSource VARCHAR(255)           NOT NULL COMMENT 'Nome file sorgente (per esempio di analisi)'
	,typeObjectCross SMALLINT          NOT NULL COMMENT 'Tipo oggetto di cross per la relazione  (T0001)'
	,idObjectCross VARCHAR(40)         NOT NULL COMMENT 'Nome oggetto di cross per la relazione (es. programma, file fisico,..) '
	,info1Cross VARCHAR(40)            NOT NULL COMMENT 'Info1 di cross per la relazione (es. step in jcl) '
	,info2Cross VARCHAR(40)            NOT NULL COMMENT 'Info2 di cross per la relazione (es. proc in jcl)'
	,info3Cross VARCHAR(40)            NOT NULL COMMENT 'Info3 di cross per la relazione (es. program exec in jcl) '
		
-- Constraints		
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, relation, idObjectA, typeObjectA, typeSourceA, idObjectB, typeObjectB, typeSourceB, numInstrOrigin)
	,CONSTRAINT Fk06       FOREIGN KEY (sys, subSys, relation, idObjectA, typeObjectA, typeSourceA, idObjectB, typeObjectB, typeSourceB) 
			        REFERENCES Relation(sys, subSys, relation, idObjectA, typeObjectA, typeSourceA, idObjectB, typeObjectB, typeSourceB)
)
COMMENT 'Describes the origin of a relationship between two objects.'
;
-- To get all relations related to
ALTER TABLE RelationOrigin
  ADD INDEX RelationOrigin_I01 
  (Relation,  sys,  subSys, idObjectA, typeObjectA, typeSourceA, numInstrOrigin);
-- To get all relations related with
ALTER TABLE RelationOrigin
  ADD INDEX RelationOrigin_I02 
  (Relation,  sys,  subSys, idObjectB, typeObjectB, typeSourceB, numInstrOrigin);


--
-- Describes an item of a copy book or a database table
-- 
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS CopyEntityDefinition   ;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE CopyEntityDefinition      (

-- Primary Key
	 sys      CHAR(2)                 NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject VARCHAR(255)            NOT NULL COMMENT 'Nome oggetto (es. programma, file fisico, copy ..)'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'
	,numSeq SMALLINT                  NOT NULL COMMENT 'Numero definizione campo da inizio copy/definizione ddl (0-based step 5). La numerazione può avere dei buchi valorizzati in fase di analisi con copy stmt o altre istruzioni'
	,idField VARCHAR(255)             NOT NULL COMMENT 'Nome campo/colonna'

-- Data, sempre validi
	,level SMALLINT                   NOT NULL COMMENT 'Livello 	(X Copy e strutture Adabas)'
	,occurs SMALLINT                  NOT NULL COMMENT 'Occorrenze 	(X Copy e strutture Adabas)'
	,lngBytes LONG                    NOT NULL COMMENT 'Lunghezza campo in bytes'
	,pos SMALLINT                     NOT NULL COMMENT 'Posizione campo (0-based)'
	,numInt SMALLINT                  NOT NULL COMMENT 'Numero interi campo'
	,numDec SMALLINT                  NOT NULL COMMENT 'Numero decimali campo'
	,signed BOOLEAN                   NOT NULL COMMENT 'True (1) Campo definito con segno'
	,groupField BOOLEAN               NOT NULL COMMENT 'True (1) Campo di gruppo'
	,itemLang SMALLINT                NOT NULL COMMENT 'Linguaggio item (Cobol, Sql, ..)  (T0007)'
	,itemType VARCHAR(255)            NOT NULL COMMENT 'Tipologia specifica item  (T0008)'

-- Data, Valori specifici per tabelle sql	
	,numDigit SMALLINT                NOT NULL COMMENT 'Numero digit complessivi colonna sql'
	,scale SMALLINT                   NOT NULL COMMENT 'Numero digit decimali'
	,notNull BOOLEAN                  NOT NULL COMMENT 'True (1) Colonna definita not null'
	,withDefault BOOLEAN              NOT NULL COMMENT 'True (1) Colonna definita with default'
	,defaultValue VARCHAR(255)        NOT NULL COMMENT 'Valore default, la lunghezza della colonna è data da lngBytes'
		
-- Constraints
    ,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, numSeq, idField)
    ,CONSTRAINT Fk08S      FOREIGN KEY (sys, subSys, idObject, typeObject) 
					  REFERENCES Object(sys, subSys, idObject, typeObject)	
)
COMMENT 'Describes an item of a copy book or database table. ex CPYE'
;
-- To support the FK of WhereUsed
ALTER TABLE CopyEntityDefinition
  ADD INDEX CopyEntityDefinition_I01 
  (sys, idObject, typeObject, idField);
-- To support queries like where is defined a field
ALTER TABLE CopyEntityDefinition
  ADD INDEX CopyEntityDefinition_I02 
  (idField, idObject, typeObject, numSeq, subSys, sys);

--
-- Describes where a copy book field or a database table column is referenced
-- 
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS WhereUsed;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE WhereUsed (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject VARCHAR(255)            NOT NULL COMMENT 'Nome oggetto (es. programma, file fisico,..)'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'
	,numSeq SMALLINT                  NOT NULL COMMENT 'Numero definizione campo da inizio copy/definizione ddl (0-based step 5). La numerazione può avere dei buchi valorizzati in fase di analisi con copy stmt o altre istruzioni'
	,idField VARCHAR(30)              NOT NULL COMMENT 'Nome campo/colonna come definito in descrittore CopyEntityDefinition '
	,idObjectRefer VARCHAR(8)         NOT NULL COMMENT 'Nome oggetto dove il campo è referenziato (Programma, copy)'
	,typeObjectRefer SMALLINT         NOT NULL COMMENT 'Tipologia oggetto programma dove il campo è referenziato (T0001)'
	,numInstrRefer SMALLINT           NOT NULL COMMENT 'Numero istruzione in programma/copy dove il campo è referenziato'
	 
-- Data
	,idAliasLocal VARCHAR(255)        NOT NULL COMMENT 'Nome campo locale utilizzato nel programma'
	,idIoarea VARCHAR(255)            NOT NULL COMMENT 'Nome Ioarea locale sotto cui il campo è definito'
	,typeWhereUsed SMALLINT           NOT NULL COMMENT 'Tipo where used (INPUT,OUTPUT) (T0010)'
	,typeAlias SMALLINT               NOT NULL COMMENT 'Tipo Alias (FULL, LOWER,..) (T0011)'
	,posInIoarea SMALLINT             NOT NULL COMMENT 'Posizione campo in ioarea (0-based)'
	,usedBytes SMALLINT               NOT NULL COMMENT 'Bytes utilizzati nel campo'
	,numInt SMALLINT                  NOT NULL COMMENT 'numero interi campo'
	,numDec SMALLINT                  NOT NULL COMMENT 'Numero decimali campo'
	,signed BOOLEAN                   NOT NULL COMMENT 'True (1) Campo definito con segno'
	,instrLang SMALLINT               NOT NULL COMMENT 'Linguaggio istruzione (Cobol, Pl1, ..)  (T0007)'
	,instrType SMALLINT               NOT NULL COMMENT 'Tipo istruzione codificata (T0009)'
	,rowStart SMALLINT         		  NOT NULL COMMENT 'Riga sorgente inizio istruzione (o copy) nel pgm'
	,rowEnd SMALLINT         		  NOT NULL COMMENT 'Riga sorgente fine istruzione (o copy) nel pgm'
	,idObjectCopy VARCHAR(255)        NOT NULL COMMENT 'Nome oggetto copy  sotto cui l''alias è definito'
	,rowStartInCopy SMALLINT          NOT NULL COMMENT 'Riga sorgente inizio istruzione dentro copy'
	,rowEndInCopy SMALLINT         	  NOT NULL COMMENT 'Riga sorgente fine istruzione dentro copy'
	
-- Constraints	
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, numSeq, idField, idObjectRefer, typeObjectRefer, numInstrRefer)
 -- ,CONSTRAINT Fk09       FOREIGN KEY (sys, subSys, idObject, typeObject, numSeq, idField) 
 --    REFERENCES CopyEntityDefinition (sys, subSys, idObject, typeObject, numSeq, idField)		
)
COMMENT 'Describes where a copy book field or a database table column is referenced. ex WHRI'
;
-- To support a list of whereUsed by program
ALTER TABLE WhereUsed
  ADD INDEX WhereUsed_I01 
  (idObjectRefer, typeObjectRefer, idField, numInstrRefer, subSys);
ALTER TABLE WhereUsed
  ADD INDEX WhereUsed_I02 
  (idField, idObjectRefer, typeObjectRefer, numInstrRefer, subSys);
  
--
-- Describes an index field of an Index Object
-- 
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS IndexItem;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE IndexItem (

-- Primary Key
	 sys      CHAR(2)                 NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject VARCHAR(255)            NOT NULL COMMENT 'Nome oggetto (es. programma, file fisico,..)'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'
	,idObjectOwner VARCHAR(255)       NOT NULL COMMENT 'Nome oggetto proprietario dell''indice Entity, Logical/Phisical File Vsam)'
	,typeObjectOwner SMALLINT         NOT NULL COMMENT 'Tipologia oggetto proprietario (T0001)'
	,numSeq SMALLINT                  NOT NULL COMMENT 'Numero sequenza definizione idObjectOwner (CPYE EntityCopyEntityDefinition)'

-- Data, valido solo per alcuni tipi di relazione		
    ,orderType SMALLINT               NOT NULL COMMENT 'Ordinamento Ascending/Descending/.. (T0012)'
	,idColumnName VARCHAR(255)        NOT NULL COMMENT 'Nome campo indice come definito in CPYE (ridondante)'
		
-- Constraints		
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, idObjectOwner, typeObjectOwner, numSeq)
	,CONSTRAINT Fk07       FOREIGN KEY (sys, subSys, idObject, typeObject) 
	                  REFERENCES Object(sys, subSys, idObject, typeObject)
)
COMMENT 'Describes an index field of an Index Object. ex IDXI'
;

--
-- Describes a generic screen or a mainfame BMS map
-- 
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS MapDescriptor;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE MapDescriptor (

-- Primary Key
	 sys        CHAR(2)               NOT NULL COMMENT 'Sistema applicativo'
	,subSys     CHAR(2)               NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject   CHAR(8)               NOT NULL COMMENT 'Nome oggetto (nome mappa)'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'	

-- Data	
	,idObjectMap CHAR(8)              NOT NULL COMMENT 'Nome oggetto Cics Map    EnumObject.CICS_MAP '	
	,typeObjectMap SMALLINT           NOT NULL COMMENT 'Tipologia oggetto (T0001)'	
	,idObjectMapset CHAR(8)           NOT NULL COMMENT 'Nome oggetto Cics Mapset EnumObject.CICS_MAPSET'	
	,typeObjectMapset SMALLINT        NOT NULL COMMENT 'Tipologia oggetto (T0001)'	
	,idObjectBmsSource CHAR(8)        NOT NULL COMMENT 'Nome oggetto BMS SOURCE EnumObject.CICS_BMS_SOURCE'	
	,typeObjectBmsSource SMALLINT     NOT NULL COMMENT 'Tipologia oggetto (T0001)'	
	,fieldCursor CHAR(8)              NOT NULL COMMENT 'Nome campo cursore'	
	,rowsSize SMALLINT                NOT NULL COMMENT 'Righe mappa'	
	,colsSize SMALLINT                NOT NULL COMMENT 'Colonne mappa'	
	,rowBegin SMALLINT                NOT NULL COMMENT 'Riga inizio nello schermo'	
	,colBegin SMALLINT                NOT NULL COMMENT 'Colonna inizio nello schermo'	

-- Constraints	
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject)
	
)
COMMENT 'Describes a generic screen or a mainfame BMS map. ex MAPD'
;

--
-- Describes a field of generic screen or a mainfame BMS map
-- 
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS MapItem;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE MapItem (

-- Primary Key
	 sys      CHAR(2)                 NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject CHAR(8)                 NOT NULL COMMENT 'Nome oggetto  (nome mappa)'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'	
	,rowNum SMALLINT                  NOT NULL COMMENT 'Riga campo'	
	,col SMALLINT                     NOT NULL COMMENT 'Colonna campo'	

-- Data	
	,bmsIdField CHAR(8     )          NOT NULL COMMENT 'Nome campo '
	,bmsDescField VARCHAR(255)        NOT NULL COMMENT 'Descrizione campo '
	,bmsFlgData BOOLEAN               NOT NULL COMMENT 'True (1) Campo Variabile digitabile, False=Campo testata protetto'
	,bmsLength SMALLINT               NOT NULL COMMENT 'Lunghezza header o campo digitabile '
	,bmsNumDec SMALLINT               NOT NULL COMMENT 'Numero decimali '
	,bmsInitial VARCHAR(255)          NOT NULL COMMENT 'Valore iniziale'
	,bmsNumGrp SMALLINT               NOT NULL COMMENT 'Numero gruppo'
	,bmsNumGrpOccurs SMALLINT         NOT NULL COMMENT 'Numero occorrenze gruppo'
	,bmsNumericSeparator BOOLEAN      NOT NULL COMMENT 'True (1) Separatore numerico'
	,bmsCtrlMinMax BOOLEAN            NOT NULL COMMENT 'True (1) Controllo Min/Max'
	,bmsFlgIc BOOLEAN                 NOT NULL COMMENT 'True (1) BMS attribute IC attivo'
	,bmsFlgProt BOOLEAN               NOT NULL COMMENT 'True (1) BMS attribute PROT attivo'
	,bmsFlgUnprot BOOLEAN             NOT NULL COMMENT 'True (1) BMS attribute UNPROT attivo'
	,bmsFlgAskip BOOLEAN              NOT NULL COMMENT 'True (1) BMS attribute ASKIP attivo'
	,bmsFlgNorm BOOLEAN               NOT NULL COMMENT 'True (1) BMS attribute NORM attivo'
	,bmsFlgBrt BOOLEAN                NOT NULL COMMENT 'True (1) BMS attribute BRIGHT attivo'
	,bmsFlgDark BOOLEAN               NOT NULL COMMENT 'True (1) BMS attribute DARK attivo'
	,bmsFlgFset BOOLEAN               NOT NULL COMMENT 'True (1) BMS attribute FSET attivo'

-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, rowNum, col)
	,CONSTRAINT Fk11       FOREIGN KEY (sys, subSys, idObject, typeObject) 
	           REFERENCES MapDescriptor(sys, subSys, idObject, typeObject)		

)
COMMENT 'Describes a field of generic screen or a mainfame BMS map. Ex. MAPI'
;


--
-- Describes a tag, that's a string to search inside sources during the analysis process
--
SET FOREIGN_KEY_CHECKS = 0; 
DROP TABLE IF EXISTS TagValue;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE TagValue (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject CHAR(8  )               NOT NULL COMMENT 'Nome oggetto (es. programma, source,..)'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'	
	,progr SMALLINT                   NOT NULL COMMENT 'Progressivo valori tag'		
	
-- Data	
	,tagValue VARCHAR(255) NOT NULL COMMENT 'Valore tag da cercare nei sorgenti'		
	,tagLength SMALLINT  NOT NULL COMMENT 'Lunghezza tag'		
	
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idObject, typeObject, progr)
  ,CONSTRAINT Fk12       FOREIGN KEY (sys, subSys, idObject, typeObject) REFERENCES Object(sys, subSys, idObject, typeObject)		
)
COMMENT 'Describes a tag, that is a string to search inside sources during the analysis process ex '
;


--
-- Describes a log record written by analysis steps with the result of the process and summary informations.
--
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ProcessLog;
SET FOREIGN_KEY_CHECKS = 1; 

CREATE TABLE ProcessLog (

-- Primary Key
	 user CHAR(10)                    NOT NULL COMMENT 'Utente applicativo'
	,sys CHAR(2)                      NOT NULL COMMENT 'Sistema applicativo'
	,typeProcess SMALLINT             NOT NULL COMMENT 'Tipo processo (T0033)'
	,idObject VARCHAR(16)             NOT NULL COMMENT 'Oggetto sotto analisi/elaborazione'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipo oggetto(T0001)'
	,dtStartProcess CHAR(8)           NOT NULL COMMENT 'Data start processo (AAAAMMGG)'
	,tmStartProcess CHAR(8)           NOT NULL COMMENT 'Ora start processo (HHMMSSCC)'
		
-- Data
	,subSys CHAR(2)                   NOT NULL COMMENT 'Sotto sistema applicativo'
	,tmEndProcess CHAR(8)             NOT NULL COMMENT 'Ora fine processo (HHMMSSCC)'
	,msDuration INT                   NOT NULL COMMENT 'Durata analisi/processo in millisecondi'
	,statusProcess SMALLINT           NOT NULL COMMENT 'Stato processo (T0013)'
	,msgError VARCHAR(255)            NOT NULL COMMENT 'Messaggio errore impostato dal processo'
	,idExcpError VARCHAR(255)         NOT NULL COMMENT 'Exception intercettata a fronte di errore'
	,excpStackTrace VARCHAR(10000)    NOT NULL COMMENT 'Stack trace al momento dell''exception'
	,threadNameError VARCHAr(30)      NOT NULL COMMENT 'Nome thread in errore'
	,javaClassError VARCHAR(255)      NOT NULL COMMENT 'Classe java in errore'

-- Constraints	
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, typeProcess, idObject, dtStartProcess, tmStartProcess)
)
COMMENT 'Describes a log record written by analysis steps with the result of the process and summary informations. Ex PLOG'
;

ALTER TABLE ProcessLog
  ADD INDEX ProcessLog_I01 
  (dtStartProcess, tmStartProcess);
ALTER TABLE ProcessLog  
  ADD INDEX ProcessLog_I02 
  (idObject, dtStartProcess, tmStartProcess);
ALTER TABLE ProcessLog   
  ADD INDEX ProcessLog_I03 
  (typeProcess, dtStartProcess, tmStartProcess); 
ALTER TABLE ProcessLog   
  ADD INDEX ProcessLog_I04 
  (User, dtStartProcess, tmStartProcess); 
  
--
-- Describes a field of a program dynamic instruction, set during the analysis process.
--  
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS DynamicField;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE DynamicField (

-- Primary Key
	 sys      CHAR(2)                 NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject VARCHAR(8)              NOT NULL COMMENT 'Nome oggetto programma'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'	
	,numInstr SMALLINT                NOT NULL COMMENT 'Numero istruzione '	
	,idField VARCHAR(30)              NOT NULL COMMENT 'Nome operando/campo da risolvere/risolto'	

-- Data 	
	,numField SMALLINT                NOT NULL COMMENT 'Numero campo codificato nella struttura di programma'	
	,instrCobolType SMALLINT          NOT NULL COMMENT 'Tipo istruzione Cobol codificata (T0029)'	
	,instrPrecompType SMALLINT        NOT NULL COMMENT 'Tipo istruzione precompilatore codificata (T0009)'	
	,instrPrecompOprndType SMALLINT   NOT NULL COMMENT 'Tipo Operando precompilatore codificato (T0009)'	
	,light BOOLEAN                    NOT NULL COMMENT 'True (1) Campo dinamico light (campo non movimentato valorizzato da value nel programma)'	
	,solved BOOLEAN                   NOT NULL COMMENT 'True (1) Campo risolto, con valori validi recuperati'	
	,solvedFull BOOLEAN               NOT NULL COMMENT 'True (1) Campo risolto, con tutti i possibili validi recuperati'	
	,waitingForData BOOLEAN           NOT NULL COMMENT 'True (1) Campo in attesa di dati esterni per essere risolto'	
	,spreaded BOOLEAN                 NOT NULL COMMENT 'True (1) Campo con assegnazioni spreaded, nei pgm chiamanti/chiamati'	
	
-- Constraints	
	,CONSTRAINT PrimaryKey PRIMARY KEY      (sys, subSys, idObject, typeObject, numInstr, idField)
	,CONSTRAINT Fk15       FOREIGN KEY      (sys, subSys, idObject, typeObject) 
	                       REFERENCES Object(sys, subSys, idObject, typeObject)			
)
COMMENT 'Describes a field of a program dynamic instruction, set during the analysis process. Ex DFLD'
;  
  
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS DynamicFieldSub;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE DynamicFieldSub (

-- Primary Key
	 sys      CHAR(2)                 NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject CHAR(8)                 NOT NULL COMMENT 'Nome oggetto programma'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'	
	,numInstr SMALLINT                NOT NULL COMMENT 'Numero istruzione '	
	,idField VARCHAR(30)              NOT NULL COMMENT 'Nome operando/campo da risolvere/risolto'	
	,idSubField VARCHAR(30)           NOT NULL COMMENT 'Nome sottocampo campo operando'  
	,numField SMALLINT                NOT NULL COMMENT 'Numero campo codificato nella struttura di programma'	
	
-- Data 	
	,numSubField SMALLINT             NOT NULL COMMENT 'Numero sottocampo codificato nella struttura di programma'
	,sizeSubField SMALLINT            NOT NULL COMMENT 'Size in bytes sottocampo codificato nella struttura di programma'
	,posSubField SMALLINT             NOT NULL COMMENT 'pos (0-based) sottocampo codificato nella struttura di programma'
    ,typeSubField SMALLINT            NOT NULL COMMENT 'Tipologia specifica sottocampo  (T0008)'

	,light BOOLEAN                    NOT NULL COMMENT 'True (1) Campo dinamico light (campo non movimentato valorizzato da value nel programma)'	
	,solved BOOLEAN                   NOT NULL COMMENT 'True (1) Campo risolto, con valori validi recuperati'	
	,waitingForData BOOLEAN           NOT NULL COMMENT 'True (1) Campo in attesa di dati esterni per essere risolto'	
	,spreaded BOOLEAN                 NOT NULL COMMENT 'True (1) Campo con assegnazioni spreaded, nei pgm chiamanti/chiamati'	
	
-- Constraints	
	,CONSTRAINT PrimaryKey PRIMARY KEY          (sys, subSys, idObject, typeObject, numInstr, idField, idSubField, numField)
	,CONSTRAINT Fk16F    FOREIGN KEY            (sys, subSys, idObject, typeObject, numInstr, idField) 
	                     REFERENCES DynamicField(sys, subSys, idObject, typeObject, numInstr, idField)			
)
COMMENT 'Describes a subfield of a field in a program dynamic instruction. Ex DSFD'
;


SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS DynamicFieldSubSetting;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE DynamicFieldSubSetting (

-- Primary Key
	 sys      CHAR(2)                 NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject CHAR(8)                 NOT NULL COMMENT 'Nome oggetto programma'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'	
	,numInstr SMALLINT                NOT NULL COMMENT 'Numero istruzione '	
	,idField VARCHAR(30)              NOT NULL COMMENT 'Nome operando/campo da risolvere/risolto'	
	,idSubField  VARCHAR(30)		  NOT NULL COMMENT 'Nome sottocampo elementare origine campo operando'  
	,idPgmSet CHAR(8)       	      NOT NULL COMMENT 'Nome programma dove è avvenuta la assegnazione'
	,numChain SMALLINT				  NOT NULL COMMENT 'Numero catena trasformazione (nel programma di set)'
	,progr SMALLINT   		          NOT NULL COMMENT 'Progressivo nella catena per ordinamento'
	,numInstrSet SMALLINT   		  NOT NULL COMMENT 'Numero istruzione di set'

-- Data 	

	,numField SMALLINT                NOT NULL COMMENT 'Numero campo codificato nella struttura di programma'
	,numSubField SMALLINT             NOT NULL COMMENT 'Numero sottocampo codificato nella struttura di programma'

-- Porzione di sottocampo oggetto dell'impostazione 
	,posInSubField SMALLINT           NOT NULL COMMENT 'Posizione in sottocampo (1-based) inizio valore di questa assegnazione'
	,lngInSubField SMALLINT           NOT NULL COMMENT 'Lunghezza in sottocampo valorizzata da questa assegnazione'
	
-- Informazioni di impostazione, programma, path e modalità di assegnazione e valore se disponibile (literal, campo di tabella)'
	,numInstrOriginSpreaded SMALLINT  NOT NULL COMMENT 'Numero istruzione origine in chiamante (Call/Xctl/Link)'
	,typeObjectPgmSet SMALLINT        NOT NULL COMMENT 'Tipologia oggetto programma dove è avvenuta la assegnazione (T0001)'
	,numPath SMALLINT                 NOT NULL COMMENT 'Numero path valido per la catena di assegnazioni'
	,setMode SMALLINT                 NOT NULL COMMENT 'Modalita ultima assegnazione campo (T0044)'
	,numUsingParm SMALLINT            NOT NULL COMMENT 'Numero parametro Using a fronte di LAST_SET_BY_COBOL_USING_PARM'
	,dspFieldInUsingParm SMALLINT     NOT NULL COMMENT 'Displacement campo in parametro Using a fronte di LAST_SET_BY_COBOL_USING_PARM'
	,dspFieldInLinkageArea  SMALLINT  NOT NULL COMMENT 'Displacement campo in area di linkage a fronte di LAST_SET_BY_COBOL_LINKAGE/TWA/CSA'
	,typePointerArea SMALLINT         NOT NULL COMMENT 'Tipo area contenente il pointer       a fronte di LAST_SET_BY_COBOL_LINKAGE (T0028)'
	,dspPointerInLinkageArea SMALLINT NOT NULL COMMENT 'Displacement pointer in tipo area indicata da typePointerArea'
	,numUsingParmPointer SMALLINT     NOT NULL COMMENT 'Numero parametro Using con pointer a fronte di LAST_SET_BY_COBOL_LINKAGE e POINTER_INSIDE_USING_PARM'
	,dspPointerInUsingParm SMALLINT   NOT NULL COMMENT 'Displacement pointer in parametro Using a fronte di LAST_SET_BY_COBOL_LINKAGE e POINTER_INSIDE_USING_PARM'
	,value VARCHAR(255)               NOT NULL COMMENT 'Valore assegnazione per literal e casi espliciti formattato in lunghezza'
	
-- Campo input in assegnazione di trasformazione.
-- Posizione e lunghezza per il sender sono quelli espressi da reference modification (posSnd:lngSnd) se indicato.
-- Posizione e lunghezza sono sempre valorizzati e se non presenti sono inizializzati (1:size(campo sender))
    ,fieldSenderId VARCHAR(30)        NOT NULL COMMENT 'Campo sender,   nome'				   
    ,fieldSenderNum SMALLINT          NOT NULL COMMENT 'Campo sender,   numero istruzione di definizione'		   
    ,fieldSenderPos SMALLINT          NOT NULL COMMENT 'Campo sender,   da posizione'			   
    ,fieldSenderLng SMALLINT          NOT NULL COMMENT 'Campo sender,   per lunghezza'	
    
-- Campo risultato in assegnazione di trasformazione o campo receiver senza trasformazioni. 
-- La posizione è quella iniziale interessata alla trasformazione.
-- La lunghezza è quella del sottocampo origine di cui trovare i valori.
-- Posizione e lunghezza sono sempre valorizzati ed inizializzati nel processo a 1, size(campo receiver)
-- Se l'istruzione Move che ha generato l'assegnazione contiene anche reference modification (pos:lng), l'informazione
-- è utilizzata solo per determinare se il receiver è influenzato dalla trasformazione, ma NON viene memorizzata.
    ,fieldReceiverId VARCHAR(30)      NOT NULL COMMENT 'Campo receiver, nome'			   
    ,fieldReceiverNum SMALLINT        NOT NULL COMMENT 'Campo receiver, numero istruzione di definizione'		   
    ,fieldReceiverPos SMALLINT        NOT NULL COMMENT 'Campo receiver, da posizione'				   
    ,fieldReceiverLng SMALLINT        NOT NULL COMMENT 'Campo receiver, per lunghezza'	
    
-- Oggetto alla cui ioarea appartiene il campo ultima trasformazione, di cui trovare i valori esternamente
-- (prima assegnazione nella catena)
    ,idObjExt VARCHAR(255)            NOT NULL COMMENT 'Nome oggetto esterno, Ts, Entity, File origine trasformazioni'
	,typeObjExt SMALLINT              NOT NULL COMMENT 'Tipologia oggetto sender (ENTITY, PHISICAL_FILE, LAST_SET_BY_SQL_SELECT, OBJECT_CICS_.., OBJECT_CICS_SYSTEM_FIELD) (T0001)'
    ,objExtSqlTableColumn VARCHAR(40) NOT NULL COMMENT 'Sql Table Column name a fronte di LAST_SET_BY_SQL_SELECT'
	,objExtSystem SMALLINT            NOT NULL COMMENT 'Campo di sistema, se OBJECT_CICS_SYSTEM_FIELD, come CICS_EIBTRMID (T0021)'
	,objExtColField VARCHAR(40)       NOT NULL COMMENT 'Nome campo/colonna come definito in CopyEntityDefinition' 
	,objExtIdCopy VARCHAR(8)          NOT NULL COMMENT 'Nome copy dove objExtColField è definito, se presente ' 
	,objExtPosCol SMALLINT            NOT NULL COMMENT 'Posizione colonna in record area/copy/TS/TD (se non definito tracciato)'
	,objExtLengthCol SMALLINT         NOT NULL COMMENT 'Lunghezza colonna in record area/copy/TS/TD (se non definito tracciato)'
	
-- Indicatori di soluzione e di valori disponibili
	,solvedObjExt BOOLEAN            NOT NULL COMMENT 'True = Ts, File etc. risolto per object esterno'
	,WaitingForExternalData BOOLEAN  NOT NULL COMMENT 'True = Dati NON disponibili in DynamicValueExt per object esterno'
	
-- Constraints	
	,CONSTRAINT PrimaryKey PRIMARY KEY               (sys, subSys, idObject, typeObject, numInstr, idField, idSubField, idPgmSet, numChain, numInstrSet, progr)
	,CONSTRAINT Fk15S      FOREIGN KEY               (sys, subSys, idObject, typeObject, numInstr, idField, idSubField) 
	                       REFERENCES DynamicFieldSub(sys, subSys, idObject, typeObject, numInstr, idField, idSubField)			
)
COMMENT 'Describes a single transformation of a subfield, of a dynamic instruction field. Ex DSFS'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS DynamicFieldSubValue;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes a field/subfield value of a program dynamic instruction, detected by analysis process.
--  
CREATE TABLE DynamicFieldSubValue (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject CHAR(8)                 NOT NULL COMMENT 'Nome oggetto programma'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'	
	,numInstr SMALLINT                NOT NULL COMMENT 'Numero istruzione data division'	
	,idField VARCHAR(30)              NOT NULL COMMENT 'Nome operando/campo da risolvere/risolto'	
	,idSubField VARCHAR(30)           NOT NULL COMMENT 'Nome sottocampo operando, in assenza di sottocampi è spaces'	
	,progr SMALLINT                   NOT NULL COMMENT 'Progressivo valore in ambito idField/idSubField'	
	
-- Data 	
	,posInSubField SMALLINT           NOT NULL COMMENT 'Posizione in sottocampo (1-based) inizio valore di questa assegnazione'	
	,lngInSubField SMALLINT           NOT NULL COMMENT 'Lunghezza in sottocampo valorizzata da questa assegnazione'	
    ,typeObjectFrom SMALLINT          NOT NULL COMMENT 'Tipologia oggetto (pgm, File, etc.) dove il valore è stato impostato (T0001)'	
	,idObjectFrom CHAR(8)             NOT NULL COMMENT 'Nome oggetto (pgm, File, etc.) dove il valore è stato impostato (T0001)'	
	,idPgmFrom CHAR(8)                NOT NULL COMMENT 'Nome Pgm di assegnazione'	
 	,numInstrFrom SMALLINT            NOT NULL COMMENT 'Numero istruzione di assegnazione'	
	,value VARCHAR(255)               NOT NULL COMMENT 'Valore sottocampo o campo'	
	,defaultValue BOOLEAN             NOT NULL COMMENT 'True (1) Valore dovuto a default Value del campo'	
	,fieldValueFull BOOLEAN           NOT NULL COMMENT 'True indica valore completo riferito al campo nel suo complesso quando idSubfield = space'	
		
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY               (sys, subSys, idObject, typeObject, numInstr, idField, idSubField, progr)
	,CONSTRAINT Fk16S      FOREIGN KEY               (sys, subSys, idObject, typeObject, numInstr, idField, idSubField) 
	                       REFERENCES DynamicFieldSub(sys, subSys, idObject, typeObject, numInstr, idField, idSubField)			
)
COMMENT 'Describes a value of a program dynamic instruction field/subfield, set during the analysis end process.'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS DynamicFieldSubWaitExt;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes an external resource necessary to complete the solution of a dynamic instruction.
--  
CREATE TABLE DynamicFieldSubWaitExt (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idObject CHAR(8)                 NOT NULL COMMENT 'Nome programma di partenza'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto programma (T0001)'	
	,numInstr SMALLINT                NOT NULL COMMENT 'Numero istruzione'
	,idField VARCHAR(30)              NOT NULL COMMENT 'Nome operando/campo da risolvere/risolto'
    ,idSubField VARCHAR(30)           NOT NULL COMMENT 'Nome sottocampo operando, in assenza di sottocampi è spaces'	
	,numProgr SMALLINT                NOT NULL COMMENT 'Numero sequenza progressivo media esterno in waiting'
	
-- Data
	,typeObjectExternal SMALLINT      NOT NULL COMMENT 'Tipologia oggetto waiting T001 (ENTITY, PHISICAL_FILE, OBJECT_CICS_.., OBJECT_CICS_SYSTEM_FIELD) (T0001)'
	,idObjectExternal VARCHAR(255)    NOT NULL COMMENT 'Nome tabella Sql/Coda Td/Ts/DDName esterno/ ...'
	,dsnameExternal VARCHAR(255)      NOT NULL COMMENT 'Nome fisico esterno, rappresenta il Dsname trovato nel JCL	'
    ,typeSystemFieldExternal SMALLINT NOT NULL COMMENT 'Tipo campo di sistema waiting, se OBJECT_CICS_SYSTEM_FIELD, come CICS_EIBTRMID (T0021)'
	,cicsNameExternal CHAR(10)        NOT NULL COMMENT 'Nome Cics waiting valido se CICS_EIBTRMID e CICS_EIBTRNID'
	,idFieldExternal VARCHAR(255)     NOT NULL COMMENT 'Nome campo/colonna waiting come definito in CopyEntityDefinition o space'
	,posColumnExternal SMALLINT       NOT NULL COMMENT 'Posizione colonna waiting in record area/copy/TS/TD (se non definito tracciato)'
	,lengthColumnExternal SMALLINT    NOT NULL COMMENT 'Lunghezza colonna waiting in record area/copy/TS/TD (se non definito tracciato)'
		
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY               (sys, subSys, idObject, typeObject, numInstr, idField, idSubField, numProgr)
    ,CONSTRAINT Fk18S      FOREIGN KEY               (sys, subSys, idObject, typeObject, numInstr, idField, idSubField) 
	                       REFERENCES DynamicFieldSub(sys, subSys, idObject, typeObject, numInstr, idField, idSubField)	
)
COMMENT 'Describes an external resource necessary to complete the solution of a dynamic instruction.'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS DynamicValueExt;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes the column value of a user managed external table, used to solve dynamic instructions in the analysis process.
--  
CREATE TABLE DynamicValueExt (

-- Primary Key
	 sys    CHAR(2)                     NOT NULL COMMENT 'Sistema applicativo'
	,subSys CHAR(2)                     NOT NULL COMMENT 'Sotto sistema applicativo'
	,typeObjectExternal SMALLINT        NOT NULL COMMENT 'Tipologia oggetto (ENTITY, PHISICAL_FILE, OBJECT_CICS_.., OBJECT_CICS_SYSTEM_FIELD) (T0001)'
	,idObjectExternal VARCHAR(255)      NOT NULL COMMENT 'Dsname file fisico/nome tabella/Segmento/Coda Ts ...'
	,dsnameExternal VARCHAR(255)        NOT NULL COMMENT 'Nome fisico esterno, rappresenta il Dsname trovato nel JCL	'
	,idFieldColumnExternal VARCHAR(30)  NOT NULL COMMENT 'Nome campo/colonna come definito in CopyEntityDefinition o space'
	,typeSystemFieldExternal SMALLINT   NOT NULL COMMENT 'Tipologia campo di sistema, se OBJECT_CICS_SYSTEM_FIELD, come CICS_EIBTRMID (T0021)'
	,cicsNameExternal CHAR(10)          NOT NULL COMMENT 'Nome Cics valido se CICS_EIBTRxxx o Vsam Ts/Td'
	,numProgr SMALLINT                  NOT NULL COMMENT 'Numero sequenza progressivo valore'
	
-- Data 
	,posColumnExternal SMALLINT         NOT NULL COMMENT 'Posizione colonna in record area/copy/TS/TD (se non definito tracciato)'
	,lengthColumnExternal SMALLINT      NOT NULL COMMENT 'Lunghezza colonna in record area/copy/TS/TD (se non definito tracciato)'
	,value VARCHAR(255)                 NOT NULL COMMENT 'Valore colonna in formato carattere no packed del fromato definito'

    -- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY       (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr)
)
COMMENT 'Describes the column value of a user managed external table, used to solve dynamic instructions in the analysis process.'
;
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS DynamicCicsMapping;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes the mapping between external dsname and ddname, for each Cics defined.
-- It's used with table DynamicValueExternal to get external values of Vsam files accessed by Exec Cics Read ... or TS/TD queue
--  
CREATE TABLE DynamicCicsMapping (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,cicsName CHAR(10)                NOT NULL COMMENT 'Nome Cics'
	,externalName VARCHAR(255)        NOT NULL COMMENT 'DDname in jcl Cics nome file Vsam/coda/ o nome tab Sql, seg DL1,...'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (ENTITY_VSAM, ENTITY_SQL, ... CICS_SYSTEM_FIELD) (T0001)'
	
-- Data	
	,dsname VARCHAR(255)              NOT NULL COMMENT 'Dsname file fisico/nome tabella/Segmento/Coda Ts ...'
		
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY       (sys, subSys, cicsName, typeObject, externalName)

)
COMMENT 'Describes the mapping between external dsname and ddname, for each Cics defined. It''s used with table DynamicValueExternal to get external values of Vsam files accessed by Exec Cics Read ... or TS/TD queue'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ImpactPlan;
SET FOREIGN_KEY_CHECKS = 1;
--
-- Describes the operations of an impact plan
-- It's used to soroe all activities to check the impact of
--  
CREATE TABLE ImpactPlan (

-- Primary Key
	 sys CHAR(2)                     NOT NULL COMMENT 'Sistema applicativo'
	,idPlan CHAR(10)                 NOT NULL COMMENT 'Identificativo piano di impatto'
	,numOp SMALLINT                  NOT NULL COMMENT 'Numero progressivo operazione'
	,typeObjectOrigin SMALLINT       NOT NULL COMMENT 'Tipologia oggetto origine(ENTITY_SQL, COPY_COBOL_DATA, ..) (T0001)'
	,idObjectOrigin  VARCHAR(40)     NOT NULL COMMENT 'Oggetto origine di cui verificare l''impatto (Table, map, campo, jcl,...)'
	
-- Data	
	,subSys CHAR(2)                  NOT NULL COMMENT 'Sotto sistema applicativo, volutamente NON in chiave'
	,fieldColumn VARCHAR(40)         NOT NULL COMMENT 'Nome campo/colonna'
	,typeImpactChange SMALLINT       NOT NULL COMMENT 'Tipologia modifica di cui verificare l''impatto'
	,fromLength SMALLINT             NOT NULL COMMENT 'Da lunghezza'
	,toLength SMALLINT               NOT NULL COMMENT 'A lunghezza'
	,fromInt SMALLINT                NOT NULL COMMENT 'Da intero'
	,toInt SMALLINT                  NOT NULL COMMENT 'Da intero'
	,fromDec SMALLINT                NOT NULL COMMENT 'Da decimali'
	,toDec SMALLINT                  NOT NULL COMMENT 'A decimali'
	,fromDataType SMALLINT           NOT NULL COMMENT 'Da formato (Packed/Display/Comp/..'
	,toDataType SMALLINT             NOT NULL COMMENT 'A  formato (Packed/Display/Comp/..'
	,fromDataTypePic VARCHAR(15)     NOT NULL COMMENT 'Da picture X(10)...'
	,toDataTypePic VARCHAR(15)       NOT NULL COMMENT 'A  picture X(10)...'
	,fromDefaultValue VARCHAR(20)    NOT NULL COMMENT 'Da Default '
	,toDefaultValue VARCHAR(20)      NOT NULL COMMENT 'A  Default'
	,fromSign BOOLEAN                NOT NULL COMMENT 'Da segno true/false'
	,toSign BOOLEAN                  NOT NULL COMMENT 'A  segno true/false'
		
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, idPlan, numOp, typeObjectOrigin, idObjectOrigin)
)
COMMENT 'Describes the operations of an impact plan'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ImpactObject;
SET FOREIGN_KEY_CHECKS = 1;
--
-- Describes changes of an impact plan
-- It's used to store all objects impacted, row start/end and instruction number
--  
CREATE TABLE ImpactObject (

-- Primary Key
	 sys CHAR(2)                     NOT NULL COMMENT 'Sistema applicativo'
	,idPlan CHAR(10)                 NOT NULL COMMENT 'Identificativo piano di impatto'
	,numOp SMALLINT                  NOT NULL COMMENT 'Numero progressivo operazione'
	,typeObjectOrigin SMALLINT       NOT NULL COMMENT 'Tipologia oggetto origine(ENTITY_SQL, COPY_COBOL_DATA, ..) (T0001)'
	,idObjectOrigin  VARCHAR(40)     NOT NULL COMMENT 'Oggetto origine di cui verificare l''impatto (Table, map, campo, jcl,...)'
	,typeObjectTarget SMALLINT       NOT NULL COMMENT 'Tipo oggetto modificato di(COPY|ENTITY|MAP|JCL|..)'
	,idObjectTarget  VARCHAR(40)     NOT NULL COMMENT 'Nome oggetto modificato'
	,numInstr SMALLINT               NOT NULL COMMENT 'Numero istruzione da modificare '
	,rowStart SMALLINT               NOT NULL COMMENT 'Numero riga inizio modifica nel sorgente'
	
-- Data	
	,rowEnd SMALLINT                 NOT NULL COMMENT 'Numero riga fine modifica nel sorgente'
	,subSys CHAR(2)                  NOT NULL COMMENT 'Sotto sistema applicativo (ridondante)'
	,cobolDivision SMALLINT          NOT NULL COMMENT 'Divisione Cobol impattata (DATA o PROC solo per programmi) EnumCobolReservedWords'
	,fieldColumn VARCHAR(40)         NOT NULL COMMENT 'Nome campo/colonna'
		
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idPlan, numOp, typeObjectOrigin, idObjectOrigin, typeObjectTarget, idObjectTarget, numInstr, rowStart)
)
COMMENT 'Describes changes on objects for an impact plan'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ImpactCompile;
SET FOREIGN_KEY_CHECKS = 1;
--
-- Describes objects affected by an impact plan
-- It's used to store all objects to be compiled/regenerated, regardless they are directly and phisically impacted
--  
CREATE TABLE ImpactCompile (

-- Primary Key
	 sys CHAR(2)                     NOT NULL COMMENT 'Sistema applicativo'
	,idPlan CHAR(10)                 NOT NULL COMMENT 'Identificativo piano di impatto'
	,typeObjectCompile SMALLINT      NOT NULL COMMENT 'Tipo oggetto da ricompilare di(PGM|ENTITY|MAP|..)'
	,idObjectCompile  VARCHAR(40)    NOT NULL COMMENT 'idObjectCompile'
	
-- Data	
	,subSys CHAR(2)                  NOT NULL COMMENT 'Sotto sistema applicativo (ridondante)'
		
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idPlan, typeObjectCompile, idObjectCompile)
)
COMMENT 'Describes changes on objects for an impact plan'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS MetricViolationConfig;
SET FOREIGN_KEY_CHECKS = 1;
--
-- Describes the configuration of violations
--
CREATE TABLE MetricViolationConfig (

-- Primary Key
	 sys    CHAR(2)                           NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                         NOT NULL COMMENT 'Sotto sistema applicativo'
	,configName CHAR(8)                       NOT NULL COMMENT 'Codice configurazione violazioni'
	,typeViolation SMALLINT                   NOT NULL COMMENT 'Tipo violazione (EnumMetricsViolation)'
	
-- Data
	,ruleEnabled BOOLEAN                      NOT NULL COMMENT 'True (1) Violazione abilitata'
	,violationDesc VARCHAR(300)               NOT NULL COMMENT 'Descrizione violazione'
	,violationSeverity SMALLINT               NOT NULL COMMENT 'A-E (EnumMetricsViolationSeverity)'
    ,ruleType SMALLINT                        NOT NULL COMMENT 'FIXED/LINEAR (EnumMetricsRuleType)'
	,qualityFactor SMALLINT                   NOT NULL COMMENT 'Fattore di qualità (EnumMetricsQualityFactors)'
	,qualityCharacteristic SMALLINT           NOT NULL COMMENT 'Caratteristica di qualità (EnumMetricsQualityCharacteristics)'
	,remediationCost SMALLINT                 NOT NULL COMMENT 'Costo remediation in minuti, ore, giorni ..'
	,remediationUnit SMALLINT                 NOT NULL COMMENT 'Unità di misura remediation (EnumMetricsViolationFixUnit)'
	,threshold SMALLINT                       NOT NULL COMMENT 'EnumThresholds (EnumThresholds)'
	,thresholdGood CHAR(2)                    NOT NULL COMMENT 'LT, GT, BT (between)'
	,thresholdLow VARCHAR(20)                 NOT NULL COMMENT 'Related to thresholdGood'
	,thresholdHigh VARCHAR(20)                NOT NULL COMMENT 'Related to thresholdGood'
		
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY      (sys, subSys, configName, typeViolation)
)
COMMENT 'Describes the configuration of a violation'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS MetricValue;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes all metrics and/or measures at program/substem/subSysem level
--  
CREATE TABLE MetricValue (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,scope SMALLINT                   NOT NULL COMMENT 'Livello di aggregazione codificato  (T045)'
	,idObject CHAR(8)                 NOT NULL COMMENT 'Nome oggetto (es. programma, Copy, Jcl, ..)'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto (T0001)'
	,section VARCHAR(60)              NOT NULL COMMENT 'Program section di aggregazione se oggetto programma '
	
-- Data	
	,sectionThru VARCHAR(40)          NOT NULL COMMENT 'Section Thru se oggetto programma'
	
-- Data, misure di conteggio sorgenti
	,cntPgmAnalyzed INT               NOT NULL COMMENT 'umero programmi analizzati'
	,cntPgmExecuted INT               NOT NULL COMMENT 'Numero programmi eseguiti da Exec batch o da Cics Link/Xctl'
	,cntSubPgmAnalyzed INT            NOT NULL COMMENT 'Numero sottoprogrammi analizzati'
	,cntSubPgmExecuted INT            NOT NULL COMMENT 'Numero sottoprogrammi richiamati con Call'
	,cntCopyDefined INT               NOT NULL COMMENT 'Numero copy definiti'
	,cntJclJob INT                    NOT NULL COMMENT 'Numero sources contenenti jcl job'
	,cntJclInclude INT                NOT NULL COMMENT 'Numero sources contenenti jcl include'
	,cntJclProc INT                   NOT NULL COMMENT 'Numero sources contenenti jcl proc'
	
-- Data, misure dimensionali sorgenti
	,sizeLinesCodeLogical INT         NOT NULL COMMENT 'Numero linee di codice logiche, includenti istruzioni, senza commenti e righe a blank'
	,sizeLinesCodePhisical INT        NOT NULL COMMENT 'Numero linee di codice fisiche, includenti istruzioni, commenti e righe a blank' 
	,sizeLinesBlank INT               NOT NULL COMMENT 'Numero linee a blank'
	,sizeLinesBlankProc INT           NOT NULL COMMENT 'Numero linee a blank in procedure division'
	,sizeLinesBlankData INT           NOT NULL COMMENT 'Numero linee a blank in data division'
	,sizeLinesComment INT             NOT NULL COMMENT 'Numero linee di commento'
	,sizeLinesCommentProc INT         NOT NULL COMMENT 'Numero linee di commento in procedure division'
	,sizeLinesCommentData INT         NOT NULL COMMENT 'Numero linee di commento in data division'
	,sizeInstr INT                    NOT NULL COMMENT 'Numero istruzioni in procedure division'
	
-- Data, misure stimate tempi di sviluppo con sizeLinesCodeLogical
	,backFiredFunctionPoint DOUBLE    NOT NULL COMMENT 'Function point stimati in base al numero logico di loc (sizeLinesCodeLogical)'
	,timeDevelopment DOUBLE           NOT NULL COMMENT 'Tempo di sviluppo in giorni stimato in base alla produttività media giornaliera'
	
-- Data, misure definizione dati
	,defFields INT                    NOT NULL COMMENT ' Numero campi definiti'
	,defFieldsInCopy INT              NOT NULL COMMENT 'Numero campi definiti dentro moduli copy'
	,defLiterals INT                  NOT NULL COMMENT 'Numero literal definite'
	
-- Data, misure di documentazione
	,percComByLogical DOUBLE          NOT NULL COMMENT '% commenti proc rispetto alle righe sorgente logiche, con istruzioni'
	,percComByPhisical DOUBLE         NOT NULL COMMENT '% commenti proc rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank'
	,percComByInstruction DOUBLE      NOT NULL COMMENT '% commenti proc per istruzione'
	,percBlankByPhisical DOUBLE       NOT NULL COMMENT '% righe a blank rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank'
	,percBlankByInstruction DOUBLE    NOT NULL COMMENT '% righe a blank per istruzione'

-- Data, misure di codice dinamico
	,dynamicPgm INT                   NOT NULL COMMENT 'Numero di programmi con codice dinamico'
	,dynamicInstr INT                 NOT NULL COMMENT 'Numero istruzioni dinamiche totali'
	,dynamicInstrLight INT            NOT NULL COMMENT 'Numero istruzioni dinamiche light, con soli campi con value'
	,percDynamicInstr DOUBLE          NOT NULL COMMENT '% istruzioni dinamiche su istruzioni totali'
	,percDynamicInstrLight DOUBLE     NOT NULL COMMENT '% istruzioni dinamiche light su istruzioni totali'

-- Data, misure violazioni generali (dettagliate solo per Squale)	
	,violations INT                     NOT NULL COMMENT 'Numero di violazioni rilevate'
	,percViolationsByLogical DOUBLE     NOT NULL COMMENT '% Violazioni rispetto alle righe sorgente logiche, con istruzioni'
	,percViolationsByPhisical DOUBLE    NOT NULL COMMENT '% Violazioni rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank'
	,percViolationsByInstruction DOUBLE NOT NULL COMMENT '% Violazioni per istruzione'
	
-- Data, misure di codice morto
	,deadFields INT                   NOT NULL COMMENT 'Numero campi definiti non utilizzati'
	,deadSubGraph INT                 NOT NULL COMMENT 'Numero sottografi sconnessi non richiamati, in cobol si tratta di section non referenziate'
	,deadInstr INT                    NOT NULL COMMENT 'Numero istruzioni definite e non referenziate (includono eventuali label)'
	,deadLabels INT                   NOT NULL COMMENT 'Numero label definite e non referenziate'
	,deadCopyData INT                 NOT NULL COMMENT 'Numero copy definiti in data division e non utilizzati'
	,deadCopyProc INT                 NOT NULL COMMENT 'Numero copy definiti in proc division e non utilizzati'

-- Data, misure di jcl	
	,jclDD INT                        NOT NULL COMMENT 'Numero di DD definite nel jcl'
	,jclStepDefined INT               NOT NULL COMMENT 'Numero di step definiti nel jcl'
	,jclStepUpdate INT                NOT NULL COMMENT 'Numero step in aggiornamento'
	,jclDsname INT                    NOT NULL COMMENT 'Numero dsname dichiarati'
	,jclDsnameReferenced INT          NOT NULL COMMENT 'Numero dsname dichiarati e referenziati dai programmi'
	,jclDsnameUnReferenced INT        NOT NULL COMMENT 'Numero dsname dichiarati e NON referenziati dai programmi'
	,jclIncludeCalled INT             NOT NULL COMMENT 'Numero include richiamate'
	,jclProcCalled INT                NOT NULL COMMENT 'Numero proc richiamate'
	
-- Data, misure di complessità strutturale e tecnica
	,structFanIn INT                  NOT NULL COMMENT ' Numero programmi chiamanti con Call, Cics Link, Cics Xctl'
	,structFanOut INT                 NOT NULL COMMENT 'Numero programmi chiamati con Call, Cics Link, Cics Xctl'
	,structSections INT               NOT NULL COMMENT 'Numero section nel programma'
	,structParagraphs INT             NOT NULL COMMENT 'Numero paragrafi nel programma'

-- Data, misure di complessità funzionale generiche	
	,funcObjects INT                  NOT NULL COMMENT 'Numero oggetti'
	,funcRelations INT                NOT NULL COMMENT 'Numero relazioni fra oggetti'
	,funcTranInternal INT             NOT NULL COMMENT 'Numero transazioni interne richiamate con Exec Cics Start o Exec Cics Return Transid'
	,funcTranExternal INT             NOT NULL COMMENT 'Numero transazioni esterne richiamate con Exec Cics Start o Exec Cics Return Transid'
	,funcMap INT                      NOT NULL COMMENT 'Numero mappe video utilizzate'
	,funcCallInternal INT             NOT NULL COMMENT 'Numero call a moduli interni'
	,funcCallExternal INT             NOT NULL COMMENT 'Numero call a moduli esterni'
	,funcAccEntityInternal INT        NOT NULL COMMENT 'Numero accessi a entity (tabelle db) interni'
	,funcAccEntityExternal INT        NOT NULL COMMENT 'Numero accessi a entity (tabelle db) esterni'
	,funcAccMediaInternal INT         NOT NULL COMMENT 'Numero accessi a files sequenziali/Vsam/code ts/.. interni'
	,funcAccMediaExternal INT         NOT NULL COMMENT 'Numero accessi a files sequenziali/Vsam/code ts/.. esterni'

-- Data, misure di complessità funzionale Function Point
	,fpExternalOutputEO INT           NOT NULL COMMENT ''
	,fpExternalInputEI INT            NOT NULL COMMENT 'Funzionalità utente (transazione o job) con add, change,delete di un ILF'
	,fpExternalInquiryEQ INT          NOT NULL COMMENT 'Funzionalità utente (transazione o job) di sola read da ILF o EIF'
	,fpInternalLogicalFileILF INT     NOT NULL COMMENT 'Tabelle/files definite e gestite dentro il sistema/sottosistema'
	,fpExternalInterfaceFileEIF INT   NOT NULL COMMENT 'Tabelle/files definite fuori dal sistema/sottosistema acceduti in read/update'
	  
-- Data, misure di complessità funzionale/tecnica per rehosting  
	,rhRateObjectRelation DOUBLE      NOT NULL COMMENT 'Rapporto tra il numero di oggetti e numero di relazioni '
	,rhObjectsInternal INT            NOT NULL COMMENT 'Numero di oggetti interni al perimetro '
	,rhObjectsExternal INT            NOT NULL COMMENT 'Numero di oggetti esterni al perimetro '
	,rhObjectsUnportable INT          NOT NULL COMMENT 'Numero di oggetti non portabili ((Assembler, PL/I, Load Module, ecc...) '
	,rhFilesBynary INT                NOT NULL COMMENT 'Numero di files/tabelle contenenti campi binari '
	
-- Data, misure di complessità ciclomatica
	,mcCabeArcs INT                   NOT NULL COMMENT 'Numero archi di programma'
	,mcCabeNodes INT                  NOT NULL COMMENT 'Numero nodi di programma'
	,mcCabeGraphConn INT              NOT NULL COMMENT 'Numero di sottografi connessi, in cobol sono section/paragrafi richiamati'
	,mcCabeOperatorsOrAnd INT         NOT NULL COMMENT 'Numero operatori condizionali OR AND per calcolo esteso'
	
-- Data, misure di complessità di Halstead (o Software Science)
	,halsteadOperators INT            NOT NULL COMMENT 'Numero operatori distinti in un programma (n1)'
	,halsteadOperands INT             NOT NULL COMMENT 'Numero operandi distinti in un programma  (n2)'
	,halsteadOperatorsOcc INT         NOT NULL COMMENT 'Numero occorrenze di operatori (N1)'
	,halsteadOperandsOcc INT          NOT NULL COMMENT 'Numero occorrenze di operandi (N2)'
	,halsteadLengthPgm INT            NOT NULL COMMENT 'Lunghezza programma'
	,halsteadVocabularyPgm INT        NOT NULL COMMENT 'Vocabolario programma'
	,halsteadVolumePgm DOUBLE         NOT NULL COMMENT 'Volume programma'
	,halsteadDifficultPgm INT         NOT NULL COMMENT 'Difficoltà programma'
	,halsteadEffortPgm INT            NOT NULL COMMENT 'Sforzo programma'
	,halsteadTimeWriting INT          NOT NULL COMMENT 'Tempo stimato di scrittura programma in secondi'

-- Data, indici di complessita/manutenibilità/Testabilità totali (somma di tutte le procedure interne)
	,idxMITot DOUBLE                  NOT NULL COMMENT 'Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister'
	,idxFPTot DOUBLE                  NOT NULL COMMENT 'Indice di punti funzione'
	,idxMcCabeTot DOUBLE              NOT NULL COMMENT 'Indice di complessità ciclomatica di McCabe con il seguente significato'
	,idxReHostingTot DOUBLE           NOT NULL COMMENT 'Indice della complessità e sforzo di rehosting di una applicazione.'

-- Data, indici di complessita/manutenibilità/Testabilità massimi
	,idxMIHigh DOUBLE                 NOT NULL COMMENT 'Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister'
	,idxFPHigh DOUBLE                 NOT NULL COMMENT 'Indice di punti funzione'
	,idxMcCabeHigh DOUBLE             NOT NULL COMMENT 'Indice di complessità ciclomatica di McCabe con il seguente significato'
	,idxReHostingHigh DOUBLE          NOT NULL COMMENT 'Indice della complessità e sforzo di rehosting di una applicazione.'
	
-- Data, indici di complessita/manutenibilità/Testabilità minimi
	,idxMILow DOUBLE                  NOT NULL COMMENT 'Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister'
	,idxFPLow DOUBLE                  NOT NULL COMMENT 'Indice di punti funzione'
	,idxMcCabeLow DOUBLE              NOT NULL COMMENT 'Indice di complessità ciclomatica di McCabe con il seguente significato'
	,idxReHostingLow DOUBLE           NOT NULL COMMENT 'Indice della complessità e sforzo di rehosting di una applicazione.'

-- Data, indici di complessita/manutenibilità/Testabilità medio	
	,idxMIAvg DOUBLE                  NOT NULL COMMENT 'Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister'
	,idxFPAvg DOUBLE                  NOT NULL COMMENT 'Indice di punti funzione'
	,idxMcCabeAvg DOUBLE              NOT NULL COMMENT 'Indice di complessità ciclomatica di McCabe con il seguente significato'
	,idxReHostingAvg DOUBLE           NOT NULL COMMENT 'Indice della complessità e sforzo di rehosting di una applicazione.'
		
-- Data, sistema di qualità SQUALE, numero violazioni per categoria gravità	
	,squaleViolationsBlocker INT      NOT NULL COMMENT 'Squale numero violazioni bloccanti'
	,squaleViolationsCritical INT     NOT NULL COMMENT 'Squale numero violazioni critiche'
	,squaleViolationsMajor INT        NOT NULL COMMENT 'Squale numero violazioni maggiori'
	,squaleViolationsMinor INT        NOT NULL COMMENT 'Squale numero violazioni minori'
	,squaleViolationsInfo INT         NOT NULL COMMENT 'Squale numero violazioni informative'

-- Data, sistema di qualità SQUALE, valori generali
	,squaleSSRL SMALLINT              NOT NULL COMMENT 'Squale rating  Livello A-E valore di rating (T0041)'
	,squaleSSRI DOUBLE                NOT NULL COMMENT 'Squale rating  value (SQI / tempo stimato di sviluppo)'
	,squaleSSCI DOUBLE                NOT NULL COMMENT 'Squale rule compliance (100 indica perfetta aderenza)'	
	,squaleSSQI INT                   NOT NULL COMMENT 'Squale absolute remediation cost. SQTI+SQRI+...+SQPI'
	
-- Data, sistema di qualità SQUALE, valori di dettaglio indice qualità assoluto SSQI, SQxI
	,squaleSQTI INT                   NOT NULL COMMENT 'Testability 		index (somma costi remediation per questa caratteristica)'
	,squaleSQRI INT                   NOT NULL COMMENT 'Reliability 		index (somma costi remediation per questa caratteristica)'
	,squaleSQCI INT                   NOT NULL COMMENT 'Changeability	index (somma costi remediation per questa caratteristica)'
	,squaleSQEI INT                   NOT NULL COMMENT 'Efficiency 		index (somma costi remediation per questa caratteristica)'
	,squaleSQSI INT                   NOT NULL COMMENT 'Security 		index (somma costi remediation per questa caratteristica)'
	,squaleSQMI INT                   NOT NULL COMMENT 'Maintenability 	index (somma costi remediation per questa caratteristica)'
	,squaleSQPI INT                   NOT NULL COMMENT 'Portability 		index (somma costi remediation per questa caratteristica)'
	
-- Data, sistema di qualità SQUALE, valori di dettaglio indici consolidati SCTx
	,squaleSCTI INT                   NOT NULL COMMENT 'index consolidato (somma costi remediation caratteristiche precedenti)'
	,squaleSCRI INT                   NOT NULL COMMENT 'index consolidato (somma costi remediation caratteristiche precedenti)'
	,squaleSCCI INT                   NOT NULL COMMENT 'index consolidato (somma costi remediation caratteristiche precedenti)'
	,squaleSCEI INT                   NOT NULL COMMENT 'index consolidato (somma costi remediation caratteristiche precedenti)'
	,squaleSCSI INT                   NOT NULL COMMENT 'index consolidato (somma costi remediation caratteristiche precedenti)'
	,squaleSCMI INT                   NOT NULL COMMENT 'index consolidato (somma costi remediation caratteristiche precedenti)'
	,squaleSCPI INT                   NOT NULL COMMENT 'index consolidato (somma costi remediation caratteristiche precedenti)'

-- Data, sistema di qualità SQUALE, valori di dettaglio indici di densita SDxI		
	,squaleSDTI DOUBLE                NOT NULL COMMENT 'Testability 	 index consolidato (SQTI / dimensioni)'
	,squaleSDRI DOUBLE                NOT NULL COMMENT 'Reliability 	 index consolidato (SQRI / dimensioni)'
	,squaleSDCI DOUBLE                NOT NULL COMMENT 'Changeability	 index consolidato (SQCI / dimensioni)'
	,squaleSDEI DOUBLE                NOT NULL COMMENT 'Efficiency 		 index consolidato (SQEI / dimensioni)'
	,squaleSDSI DOUBLE                NOT NULL COMMENT 'Security 		   index consolidato (SQSI / dimensioni)'
	,squaleSDMI DOUBLE                NOT NULL COMMENT 'Maintenability index consolidato (SQMI / dimensioni)'
	,squaleSDPI DOUBLE                NOT NULL COMMENT 'Portability 	 index consolidato (SQPI / dimensioni)'

-- Data, sistema di qualità SQUALE, valori di dettaglio indici squale rating SRxI 
	,squaleSRTI DOUBLE                NOT NULL COMMENT 'Testability 		Squale rating (SQTI / tempo stimato sviluppo)'	
	,squaleSRRI DOUBLE                NOT NULL COMMENT 'Reliability 		Squale rating (SQRI / tempo stimato sviluppo)'
	,squaleSRCI DOUBLE                NOT NULL COMMENT 'Changeability	Squale rating (SQCI / tempo stimato sviluppo)'
	,squaleSREI DOUBLE                NOT NULL COMMENT 'Efficiency 		Squale rating (SQEI / tempo stimato sviluppo)'
	,squaleSRSI DOUBLE                NOT NULL COMMENT 'Security 		Squale rating (SQSI / tempo stimato sviluppo)'
	,squaleSRMI DOUBLE                NOT NULL COMMENT 'Maintenability 	Squale rating (SQMI / tempo stimato sviluppo)'
	,squaleSRPI DOUBLE                NOT NULL COMMENT 'Portability 		Squale rating (SQPI / tempo stimato sviluppo)'

-- Data, sistema di qualità SQUALE, valori di dettaglio livelli squale rating SRxL 	
	,squaleSRTL SMALLINT              NOT NULL COMMENT 'Testability 		Livello A-E valore di rating (T0041)'
	,squaleSRRL SMALLINT              NOT NULL COMMENT 'Reliability 		Livello A-E valore di rating (T0041)'
	,squaleSRCL SMALLINT              NOT NULL COMMENT 'Changeability	    Livello A-E valore di rating (T0041)'
	,squaleSREL SMALLINT              NOT NULL COMMENT 'Efficiency 		Livello A-E valore di rating (T0041)'
	,squaleSRSL SMALLINT              NOT NULL COMMENT 'Security 		    Livello A-E valore di rating (T0041)'
	,squaleSRML SMALLINT              NOT NULL COMMENT 'Maintenability 	Livello A-E valore di rating (T0041)'
	,squaleSRPL SMALLINT              NOT NULL COMMENT 'Portability 		Livello A-E valore di rating (T0041)'
	
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY      (sys, subSys, scope, idObject, typeObject, section)
	,CONSTRAINT Fk19O      FOREIGN KEY      (sys, subSys, idObject, typeObject) 
	                       REFERENCES Object(sys, subSys, idObject, typeObject)			
)
COMMENT 'Describes metrics at the program level and/or sys/subSys'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS MetricViolation;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes a violation of metric rules at the sys/subSysem/program/section/paragraph level
--
CREATE TABLE MetricViolation (

-- Primary Key
	 sys    CHAR(2)                           NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                         NOT NULL COMMENT 'Sotto sistema applicativo'
	,scope SMALLINT                           NOT NULL COMMENT 'Livello di aggregazione codificato  (T045)'
	,idObject CHAR(8)                         NOT NULL COMMENT 'Nome oggetto (es. program ..)'
	,typeObject SMALLINT                      NOT NULL COMMENT 'Tipologia oggetto (T0001)'
	,section VARCHAR(255)                     NOT NULL COMMENT 'Program section di aggregazione se oggetto programma'
	,typeViolation SMALLINT                   NOT NULL COMMENT 'Tipologia violazione (T0046)'
	
-- Data
	,severityViolation SMALLINT               NOT NULL COMMENT 'Severità violazione (T0047)'
	,originViolation TEXT                     NOT NULL COMMENT 'Elenco di numeri istruzione/definizione'
	,originViolationRows TEXT                 NOT NULL COMMENT 'Elenco di numeri riga sorgente'
    ,originViolationRowsCopy TEXT             NOT NULL COMMENT 'Elenco di numeri riga sorgente di copy (-1 non significativo)'
	,cntViolations SMALLINT                   NOT NULL COMMENT 'Contatore violazioni'
	,value VARCHAR(300)                       NOT NULL COMMENT 'Valore violazione (Es. valore di soglia o altro)'
	,remediationCost SMALLINT                 NOT NULL COMMENT 'Costo remediation in minuti, ore, giorni ..'
	,remediationUnit SMALLINT                 NOT NULL COMMENT 'Unità di misura remediation (T0048)'
	,qualityCharacteristic SMALLINT           NOT NULL COMMENT 'Caratteristica di qualità (T0049)'
		
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY      (sys, subSys, scope, idObject, typeObject, section, typeViolation)
	,CONSTRAINT Fk20O      FOREIGN KEY      (sys, subSys, idObject, typeObject) 
	                       REFERENCES Object(sys, subSys, idObject, typeObject)			
)
COMMENT 'Describes a violation of metric rules at the sys/subSys/program/section/paragraph level'
;
-- To support a list of metrics by section
ALTER TABLE MetricViolation
  ADD INDEX MetricViolation_I01 
  (section, typeViolation, idObject, sys, subSys);
-- To support a list of metrics by typeViolation
ALTER TABLE MetricViolation
  ADD INDEX MetricViolation_I02 
  (typeViolation, section, idObject, sys, subSys);

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS MetricScenario;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes a violation customization identified by a schenario string
--
CREATE TABLE MetricScenario (

-- Primary Key
	 sys    CHAR(2)                    NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                  NOT NULL COMMENT 'Sotto sistema applicativo'
	,idScenario VARCHAR(10)            NOT NULL COMMENT 'Identificativo scenario personalizzazione'
	,typeViolation SMALLINT            NOT NULL COMMENT 'Tipologia violazione (T0046)'
	
-- Data
	,violationEnabled BOOLEAN          NOT NULL COMMENT 'True (1) Violazione abilitata'
	,severityViolation SMALLINT        NOT NULL COMMENT 'Severità violazione (T0047)'
	,remediationUnit SMALLINT          NOT NULL COMMENT 'Unità di misura remediation (T0048)'
	,remediationCost SMALLINT          NOT NULL COMMENT 'Costo remediation in minuti, ore, giorni ..'
	,thresholdLow SMALLINT             NOT NULL COMMENT 'Livello di soglia low'
	,thresholdHigh SMALLINT            NOT NULL COMMENT 'Livello di soglia high'
	,qualityCharacteristic SMALLINT    NOT NULL COMMENT 'Caratteristica di qualità (T0049)'
	
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idScenario, typeViolation)
)
COMMENT 'Describes a violation customization identified by a shenario string'
;


SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ScopeHeader;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes a scope header that identifies a set of programs
--
CREATE TABLE ScopeHeader (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idScope VARCHAR(10)              NOT NULL COMMENT 'Identificativo scope '
	
-- Data
	,descScope VARCHAR(255)           NOT NULL COMMENT 'Descrizione scope'
	,numPrograms SMALLINT             NOT NULL COMMENT 'numPrograms'
	,numObjects SMALLINT              NOT NULL COMMENT 'Numero oggetti correlati ai programmi (anche altri programmi)'
	,dateCreation CHAR(8)             NOT NULL COMMENT 'Data creazione Scope AAAAMMGG'
	,dateUpdate CHAR(8)               NOT NULL COMMENT 'Data update AAAAMMGG'
	
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, idScope)
)
COMMENT 'Describes a scope header that identifies a set of programs.'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ScopeProgram;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes programs matching all scope criterias
--
CREATE TABLE ScopeProgram (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idScope VARCHAR(10)              NOT NULL COMMENT 'Identificativo scope '
	,idProgram CHAR(8)                NOT NULL COMMENT 'Nome programma ' 
	,typeObjectPgm SMALLINT           NOT NULL COMMENT 'Tipo oggetto programma (T0001)' 
	
	
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY           (sys, subSys, idScope, idProgram, typeObjectPgm)	
	,CONSTRAINT Fk23H      FOREIGN KEY           (sys, subSys, idScope) 
	                       REFERENCES ScopeHeader(sys, subSys, idScope)			
)
COMMENT 'Describes a program matching all scope criterias'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ScopeObject;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes a specific objects included in a scope starting from Programs selected
--
CREATE TABLE ScopeObject (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                NOT NULL COMMENT 'Sotto sistema applicativo'
	,idScope VARCHAR(10)              NOT NULL COMMENT 'Identificativo scope '
	,idObject CHAR(8)                NOT NULL COMMENT 'Nome oggetto in scope, diretto/indiretto'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto programma (T0001)'
	
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY           (sys, subSys, idScope, idObject, typeObject)
	,CONSTRAINT Fk26H      FOREIGN KEY           (sys, subSys, idScope) 
	                       REFERENCES ScopeHeader(sys, subSys, idScope)			
)
COMMENT 'Describes a objects included in a scope'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ScopeChild;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describe a scope descriptor child of a scope father/child structure
--
CREATE TABLE ScopeChild (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys  CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idScope VARCHAR(10)              NOT NULL COMMENT 'Identificativo scope '
	,idScopeChild VARCHAR(10)         NOT NULL COMMENT 'Identificativo scope figlio '

-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY           (sys, subSys, idScope, idScopeChild)
	,CONSTRAINT Fk24H      FOREIGN KEY           (sys, subSys, idScope) 
	                       REFERENCES ScopeHeader(sys, subSys, idScope)			
)
COMMENT 'Describe a scope descriptor child of a structure father/child'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ScopeRelation;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Descibes all relationships of scope programs
--
CREATE TABLE ScopeRelation (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,idScope VARCHAR(10)              NOT NULL COMMENT 'Identificativo scope '
	,idObject CHAR(8)                 NOT NULL COMMENT 'Nome oggetto programma'
	,typeObject SMALLINT              NOT NULL COMMENT 'Tipologia oggetto programma (T0001)'
	,relation SMALLINT                NOT NULL COMMENT 'Relazione codificata programma/oggetto (T0034)'
	,sysRelated    CHAR(2)            NOT NULL COMMENT 'Sistema applicativo in relazione'
	,subSysRelated   CHAR(2)          NOT NULL COMMENT 'Sotto sistema applicativo in relazione'
	,idObjectRelated CHAR(8)          NOT NULL COMMENT 'Nome oggetto in relazione'
	,typeObjectRelated SMALLINT       NOT NULL COMMENT 'Tipologia oggetto in relazione (T0001)'
	
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY      (sys, subSys, idScope, idObject, typeObject, relation, idObjectRelated, typeObjectRelated)
    ,CONSTRAINT Fk27O      FOREIGN KEY      (sys, subSys, idObject, typeObject)                             
	                       REFERENCES Object(sys, subSys, idObject, typeObject)			
    ,CONSTRAINT Fk27OR     FOREIGN KEY      (sysRelated, subSysRelated, idObjectRelated, typeObjectRelated) 
	                       REFERENCES Object(sys,        subSys,        idObject,        typeObject)			
	)
COMMENT 'Describes all relationships of scope programs'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS ScopeCriteria;
SET FOREIGN_KEY_CHECKS = 1;

--
-- Describes a specific scope section, that identifies a set of criterias to be satisfied to detect object programs
--
CREATE TABLE ScopeCriteria (

-- Primary Key
	 sys    CHAR(2)                    NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                  NOT NULL COMMENT 'Sotto sistema applicativo'
	,idScope VARCHAR(10)               NOT NULL COMMENT 'Identificativo scope '
	,section SMALLINT                  NOT NULL COMMENT 'Sezione scope o parentesi aperta/chiusa (T0044)'
	,idObject VARCHAR(30)              NOT NULL COMMENT 'Nome copy/entity/oggetto relazionato (duplicato per univocità chiave)'
	,numeSeqItem SMALLINT              NOT NULL COMMENT 'Numero sequenza in EntityCopyEntityDefinition  per le sezioni che lo prevedono'

-- Data
	,mustBeTrue BOOLEAN               NOT NULL COMMENT 'True (1) indica programma da portare in scope solo se verificata la condizione in section'
	
-- Data, individuazione sorgente o campo/label/section definito dentro sorgente *
    ,posFrom SMALLINT                  NOT NULL COMMENT ' Posizione nel nome sorgente 0-based'
	,length SMALLINT                   NOT NULL COMMENT 'Lunghezza da posFrom'
	,regularExpr VARCHAR(50)           NOT NULL COMMENT 'Regular expression in alternativa da applicare ai nomi dei sorgenti/entiity/'
	,valueToBeMatched VARCHAR(255)     NOT NULL COMMENT 'Valore in nome programma/campo da trovare'
--                                                     Valore in Nome di campo che deve essere presente/assente nel programma
--                                                     Valore in Label che deve essere presente/assente nel programma
--                                                     Valore in Section che deve essere presente/assente nel programma
--                                                     Istruzione Cobol che deve essere presente/assente nel programma
--                                                     Istruzione Cics che deve essere presente/assente nel programma
--                                                     Opzione istruzione Cics che deve essere presente/assente nel programma

-- Data, caratteristiche campo che deve essere definito/non definito nel source  
	,fieldName VARCHAR(255)            NOT NULL COMMENT 'Nome campo che deve esssere definito/non definito nel source'
	,underCopy CHAR(8)                 NOT NULL COMMENT 'Nome copy sotto il quale deve essere definito il campo'
	,underGroupName VARCHAR(255)       NOT NULL COMMENT 'Nome campo di gruppo sotto il quale deve essere definito/non definito il campo'
	,underGroup BOOLEAN                NOT NULL COMMENT 'True (1) se il campo deve essere definito/non definito sotto un gruppo'
	,underRedefines BOOLEAN            NOT NULL COMMENT 'True (1) se il campo è definita/non definita sotto un campo con clausola redefines'
	,withRedefines BOOLEAN             NOT NULL COMMENT 'True (1) se il campo ha definita/non definita la clausola redefines'
	,occurs SMALLINT                   NOT NULL COMMENT 'Numero occurs con cui il campo deve essere definito/non definito'
	,occursFrom SMALLINT               NOT NULL COMMENT 'Numero occurs from con cui il campo deve essere definito/non definito'
	,occursTo SMALLINT                 NOT NULL COMMENT 'Numero occurs to con cui il campo deve essere definito/non definito'
	,pictureCobol VARCHAR(30)          NOT NULL COMMENT 'Picture Cobol come codificata nel source che deve essere presente/assente per il campo'
	,usageCobol SMALLINT               NOT NULL COMMENT 'Usage Cobol che deve essere presente/assente per il campo (T0016)'
	,numInt SMALLINT                   NOT NULL COMMENT 'Numero interi che devono essere definiti per il campo'
	,numDec SMALLINT                   NOT NULL COMMENT 'Numero decimali che devono essere definiti per il campo'
	,sizeBytes SMALLINT                NOT NULL COMMENT 'Lunghezza in bytes definiti per il campo'

-- Data, istruzioni Cobol/Cics del programma
	,instrCobol SMALLINT               NOT NULL COMMENT 'Istruzione cobol di cui verificare presenza/assenza (T0029)'
	,instrCics SMALLINT                NOT NULL COMMENT 'Istruzione cobol di cui verificare presenza/assenza (T0009)'
	,instrCicsOption SMALLINT          NOT NULL COMMENT 'Opzione che l''istruzione Cics deve avere/non avere (T0015)'

-- Data, opzioni e relazioni con altri oggetti (per esempio Copy, entity, entity_read ..)
	,programOption SMALLINT            NOT NULL COMMENT 'Opzione programma che deve avere il programma (T0004) PGM_WITH_I_O_SCREEN, PGM_WITH_I_O_SQL,....'
	,typeObjectRelated SMALLINT        NOT NULL COMMENT 'Tipo oggetto relazionato/acceduto (T0001)'
	,idObjectRelated VARCHAR(255)      NOT NULL COMMENT 'Nome oggetto relazionato/acceduto (Nome tabella, segmento Dl1, etc.)'
	,relPgmOther SMALLINT              NOT NULL COMMENT 'Relazione programma con Entity/phisical file/external file/.. (T0034) '
--                                                      PGM_ENTITY, PGM_ENTITY_READ, ... PGM_ENTITY_INSERT 
--                                                      PGM_ENTITY, PGM_ENTITY_READ, ... PGM_ENTITY_INSERT   
--                                                      PGM_PHISICAL_FILE, PGM_PHISICAL_FILE_READ, ... PPGM_PHISICAL_FILE_INSERT 
--                                                      PGM_INTERNAL_FILE, PGM_INTERNAL_FILE_READ, ... PGM_INTERNAL_FILE_INSERT   
--                                                      PGM_EXTERNAL_FILE, PGM_EXTERNAL_FILE_READ, ... PGM_EXTERNAL_FILE_INSERT   	
--                                                      PGM_COPY_BOOK

-- Data, Accesso a campo di copy o colonna di tabella
	,copyItemOrColumn VARCHAR(30)      NOT NULL COMMENT 'Nome campo copy o nome colonna'
	,typeIOitemColumn SMALLINT         NOT NULL COMMENT 'Utilizzo campo/colonna in input o output (T0010)'

	
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY           (sys, subSys, idScope, section, idObject, numeSeqItem)
	,CONSTRAINT Fk25H      FOREIGN KEY           (sys, subSys, idScope) 
	                       REFERENCES ScopeHeader(sys, subSys, idScope)			
)
COMMENT 'Describes a specific scope section, that identifies a set of criterias to be satisfied to detect object programs'
;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS TableHeader;
SET FOREIGN_KEY_CHECKS = 1;
--
-- Describes a table of the generalized amrita tables sys   , for domain control, description transcoding and configuration
-- 

CREATE TABLE TableHeader (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,numTable SMALLINT                NOT NULL COMMENT 'Tipo (numero) tabella (T0000)'	
 
-- Data	
	,descTb VARCHAR(255)              NOT NULL COMMENT 'Descrizione tabella iniziale in inglese'	
	,tableSystem BOOLEAN              NOT NULL COMMENT 'True (1) indica tabella di sistema'	
	,tableStructured BOOLEAN          NOT NULL COMMENT 'True (1) indica tabella strutturata in campi'		
	,enumJava VARCHAR(255)            NOT NULL COMMENT 'Enumerazione java che descrive il dominio' 	
	,dtCreation CHAR(8)               NOT NULL COMMENT 'Data creazione 		 (AAAAMMGG)'	
	,dtUpdate CHAR(8)                 NOT NULL COMMENT 'Data aggiornamento (AAAAMMGG)'	
	,tmCreation CHAR(8)               NOT NULL COMMENT 'Ora creazione 		 (HHMMSSCC)'	
	,tmUpdate CHAR(8)                 NOT NULL COMMENT 'Ora aggiornamento (HHMMSSCC)'	
		
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, numTable)
)
COMMENT 'Describes a table of the generalized amrita tables sys, for domain control, description transcoding and configuration Ex. TBHD'
;

--
-- Describes a data row of an amrita generalized tables sys   
-- 
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS TableData;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE TableData (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,numTable SMALLINT                NOT NULL COMMENT 'Tipo (numero) tabella (T0000)'	
	,language SMALLINT                NOT NULL COMMENT 'Linguaggio (T0040)'	
	,keySeq CHAR(4)                   NOT NULL COMMENT 'Sequenza chiave per eventuale ordinamento valori (4 char)'	
	,keyVal VARCHAR(20)               NOT NULL COMMENT 'Chiave elemento di tabella '	
 
-- Data 
	,rowData VARCHAR(2000)            NOT NULL COMMENT 'Riga dati tabella con elementi (items) descritti da TableStructure o stringa singola di trascodifica dominio'	
	
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, numTable, language, keySeq, keyVal)
    ,CONSTRAINT Fk13       FOREIGN KEY (sys, subSys, numTable) REFERENCES TableHeader(sys, subSys, numTable)		

)
COMMENT 'Describes a single item data row of an amrita generalized tables sys Ex TBDT  '
;

--
-- Describes a single table item of an amrita generalized tables sys    structure
-- 
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS TableStructure;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE TableStructure (

-- Primary Key
	 sys    CHAR(2)                   NOT NULL COMMENT 'Sistema applicativo'
	,subSys   CHAR(2)                 NOT NULL COMMENT 'Sotto sistema applicativo'
	,numTable SMALLINT                NOT NULL COMMENT 'Tipo (numero) tabella (T0000)'	
	,posItem SMALLINT                 NOT NULL COMMENT 'Posizione item nell''area dati 0-based'	
	,idItem VARCHAR(20)               NOT NULL COMMENT 'Nome item (campo) (T0030, T0031, descrizione lunga e breve)'	

-- Data 	
	,typeItem SMALLINT                NOT NULL COMMENT 'Tipo item numerico, alfanumerico e data (T0014)'
	,dateFormat SMALLINT              NOT NULL COMMENT 'Formato data se typeItem = DATA_ITEM_DATE (T0022)'
	,obbl BOOLEAN                     NOT NULL COMMENT 'Obbligatorietà'
	,valDefault SMALLINT              NOT NULL COMMENT 'Valore di default in accordo a formato/interi/decimali'
	,lngBytes SMALLINT                NOT NULL COMMENT 'Lunghezza complessiva in bytes, -1 indica tutta l''area disponibile'
	,numInt SMALLINT                  NOT NULL COMMENT 'Numero interi'
	,numDec SMALLINT                  NOT NULL COMMENT 'Numero decimali'
		
-- Constraints
	,CONSTRAINT PrimaryKey PRIMARY KEY (sys, subSys, numTable, posItem, idItem)
    ,CONSTRAINT Fk14       FOREIGN KEY (sys, subSys, numTable) REFERENCES TableHeader(sys, subSys, numTable)		
)
COMMENT 'Describes a single table item of an amrita generalized tables sys    structure'
;


--
-- Test LOGIC001
--
INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 74, 'EIBTRNID', ' ',  'EIBTRNID', 29, ' ', 0, 1, 4, 'TR01');
INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 74, 'EIBTRNID', ' ',  'EIBTRNID', 29, ' ', 1, 1, 4, 'TR02');
INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 74, 'EIBTRNID', ' ',  'EIBTRNID', 29, ' ', 2, 1, 4, 'TR03');
INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 74, 'EIBTRNID', ' ',  'EIBTRNID', 29, ' ', 3, 1, 4, 'TR04');

INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 74, 'EIBTRMID', ' ',  'EIBTRMID', 28, ' ', 0, 1, 4, 'TM01');
INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 74, 'EIBTRMID', ' ',  'EIBTRMID', 28, ' ', 1, 1, 4, 'TM02');

INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 18, 'VSAM01', ' ', ' ', 0, ' ', 0, 3, 8, 'LOGVSM01');
INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 18, 'VSAM01', ' ', ' ', 0, ' ', 1, 3, 8, 'LOGVSM02');
INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 18, 'VSAM01', ' ', ' ', 0, ' ', 2, 3, 8, 'LOGVSM03');

INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 15, 'ANSADID', ' ', 'ANSTCO', 0, ' ', 0, 1, 1, 'X');
INSERT INTO dynamicvalueext (sys, subSys, typeObjectExternal, idObjectExternal, dsnameExternal, idFieldColumnExternal, typeSystemFieldExternal, cicsNameExternal, numProgr, posColumnExternal, lengthColumnExternal, value)
VALUES ('VN', 'LO', 15, 'ANSADID', ' ', 'ANSTCO', 0, ' ', 1, 1, 1, 'Y');





update object set idObjectDescriptor = 'Anagrafica' where idObject='AN' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Distinta Base' where idObject='DB' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Contabilita' where idObject='CG' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Qualita' where idObject='QC' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Ric. Merce' where idObject='RM' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Spedizioni' where idObject='SH' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Ordini Clienti' where idObject='OC' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Ordini Fornitori' where idObject='OF' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Tabelle' where idObject='TB' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Formula FM' where idObject='FM' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Fatture Attive' where idObject='FA' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Manufactoring' where idObject='MP' and typeObject = 92 and sys='*' and subSys='*';
update object set idObjectDescriptor = 'Utilities' where idObject='UT' and typeObject = 92 and sys='*' and subSys='*';


delete from metricViolationConfig where sys = 'VN' and subSys = 'AN' and configName = 'AN-CONF';
INSERT INTO metricViolationConfig (sys, subSys, configName, typeViolation, ruleEnabled, violationDesc, ruleType, qualityCharacteristic, qualityFactor, violationSeverity, threshold, remediationCost, remediationUnit, thresholdGood, thresholdLow, thresholdHigh)
VALUES   ( 'VN', 'AN', 'AN-CONF', 001, true, 'R0001_AVOID_GOTO_OUTSIDE_SECTION', 0, 34, 7, 4, 0, 240, 1, ' ', ' ', ' ' )     				
        ,( 'VN', 'AN', 'AN-CONF', 002, true, 'R0002_AVOID_GOTO_BACK_SAME_SECTION', 0, 30, 6,3, 0, 60, 1, ' ', ' ', ' ' )    				
        ,( 'VN', 'AN', 'AN-CONF', 003, true, 'R0003_AVOID_GOTO_OUTSIDE_PARAGRAPH', 0, 34, 7, 4, 0, 240, 1, ' ', ' ', ' ' )    				
        ,( 'VN', 'AN', 'AN-CONF', 004, true, 'R0004_AVOID_GOTO_BACK_SAME_PARAGRAPH', 0, 24, 5, 1, 0, 60, 1, ' ', ' ', ' ' )    				
        ,( 'VN', 'AN', 'AN-CONF', 005, true, 'R0005_AVOID_GOTO_FWD_TO_END', 0, 24, 5, 3, 0, 2, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 006, true, 'R0006_AVOID_GOTO_FWD_NOT_TO_END', 0, 24, 5, 3, 0, 2, 1, ' ', ' ', ' ' )				
        ,( 'VN', 'AN', 'AN-CONF', 007, true, 'R0007_AVOID_RECURSIVE_PERFORM', 0, 34, 7, 4, 0, 120, 1, ' ', ' ', ' ' )				   
        ,( 'VN', 'AN', 'AN-CONF', 008, true, 'R0008_AVOID_EMPTY_PARAGRAPH', 0, 14, 2, 2, 0, 5, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 009, true, 'R0009_AVOID_UNDOCUMENTED_PARAGRAPH', 0, 14, 2, 2, 0, 30, 1, ' ', ' ', ' ' )      				
        ,( 'VN', 'AN', 'AN-CONF', 010, true, 'R0010_AVOID_LOW_DOCUMENTED_PARAGRAPH', 0, 14, 2, 3, 22, 30, 1, ' ', ' ', ' ' )         	
        ,( 'VN', 'AN', 'AN-CONF', 011, true, 'R0011_AVOID_LOW_PARAGRAPH_PERC_COMM_BY_INSTR', 0, 14, 2,2, 28, 30, 1, ' ', ' ', ' ' )
        ,( 'VN', 'AN', 'AN-CONF', 012, true, 'R0012_AVOID_LOW_PARAGRAPH_PERC_COMM_BY_ROW', 0, 14, 2,2, 27, 30, 1, ' ', ' ', ' ' )	
        ,( 'VN', 'AN', 'AN-CONF', 013, true, 'R0013_AVOID_HIGH_PARAGRAPH_INSTR_SIZE', 0, 22, 5, 3, 13, 120, 1, ' ', ' ', ' ' ) 	
        ,( 'VN', 'AN', 'AN-CONF', 014, true, 'R0014_AVOID_HIGH_MCBE_PARAGRAPH_COMPLEXITY', 0, 34, 7, 3, 31, 480, 1, ' ', ' ', ' ' )  
        ,( 'VN', 'AN', 'AN-CONF', 015, true, 'R0015_AVOID_SECTION_UNREFERENCED', 0, 14, 2, 2, 0, 5, 1, ' ', ' ', ' ' )    				 
        ,( 'VN', 'AN', 'AN-CONF', 016, true, 'R0016_AVOID_EMPTY_SECTION', 0, 14, 2, 2, 0, 5, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 017, true, 'R0017_AVOID_SECTION_WITH_NO_PARAGRAPH', 0, 22, 5, 2, 0, 60, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 018, true, 'R0018_AVOID_UNDOCUMENTED_SECTION', 0, 14, 2, 2, 0, 30, 1, ' ', ' ', ' ' )        			 
        ,( 'VN', 'AN', 'AN-CONF', 019, true, 'R0019_AVOID_LOW_DOCUMENTED_SECTION', 0, 14, 2, 2, 21, 30, 1, ' ', ' ', ' ' )         		 
        ,( 'VN', 'AN', 'AN-CONF', 020, true, 'R0020_AVOID_LOW_SECTION_PERC_COMM_BY_INSTR', 0, 14, 2,2, 26, 30, 1, ' ', ' ', ' ' )	 
        ,( 'VN', 'AN', 'AN-CONF', 021, true, 'R0021_AVOID_LOW_SECTION_PERC_COMM_BY_ROW', 0, 14, 2, 2, 25, 30, 1, ' ', ' ', ' ' )	 
        ,( 'VN', 'AN', 'AN-CONF', 022, true, 'R0022_AVOID_HIGH_SECTION_INSTR_SIZE', 0, 22, 5,3, 12, 120, 1, ' ', ' ', ' ' )      
        ,( 'VN', 'AN', 'AN-CONF', 023, true, 'R0023_AVOID_HIGH_MCBE_SECTION_COMPLEXITY', 0, 34, 7,3, 31, 480, 1, ' ', ' ', ' ' )  	
        ,( 'VN', 'AN', 'AN-CONF', 024, true, 'R0024_AVOID_HIGH_PERFORM_INNER_INSTR_SIZE', 0, 22, 5,3, 13, 120, 1, ' ', ' ', ' ' )	
        ,( 'VN', 'AN', 'AN-CONF', 025, true, 'R0025_AVOID_HIGH_LEVEL_NESTING_IF', 0, 22, 5,3, 9, 4, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 026, true, 'R0026_AVOID_LABEL_UNREFERENCED', 0, 15, 2,2, 0, 5, 1, ' ', ' ', ' ' )    				 
        ,( 'VN', 'AN', 'AN-CONF', 027, true, 'R0027_AVOID_LABEL_REFERENCED_DUPLICATED', 0, 24, 5, 4, 0, 5, 1, ' ', ' ', ' ' )    				 
        ,( 'VN', 'AN', 'AN-CONF', 028, true, 'R0028_AVOID_LABEL_UNREFERENCED_DUPLICATED', 0, 24, 5,4, 0, 5, 1, ' ', ' ', ' ' ) 					 
        ,( 'VN', 'AN', 'AN-CONF', 029, true, 'R0029_AVOID_COMMIT_INSIDE_LOOP', 0, 21, 4, 4, 0, 240, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 030, true, 'R0030_AVOID_FILE_OPEN_CLOSE_INSIDE_LOOP', 0, 21, 4, 4, 0, 30, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 031, true, 'R0031_AVOID_INDEXING_NO_USAGE_INDEX_INSIDE_LOOP', 0, 21, 4, 3, 0, 10, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 032, true, 'R0032_AVOID_ARITHMETIC_OPERAND_ZONED_INSIDE_LOOP', 0, 21, 4, 3,0, 10, 1, ' ', ' ', ' ' ) 					 
        ,( 'VN', 'AN', 'AN-CONF', 033, true, 'R0033_AVOID_INDEXING_NO_USAGE_INDEX_NO_LOOP', 0, 21, 4, 2, 0, 10, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 034, true, 'R0034_AVOID_ARITHMETIC_OPERAND_ZONED_NO_LOOP', 0, 21, 4, 2, 0, 10, 1, ' ', ' ', ' ' )			 
        ,( 'VN', 'AN', 'AN-CONF', 035, true, 'R0035_AVOID_PERFORM_VARYING_K_ZONED', 0, 21, 4, 3, 0, 10, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 036, true, 'R0036_AVOID_BLOCK_N_RECORDS', 0, 20, 4, 3, 0, 5, 1, ' ', ' ', ' ' )			 
        ,( 'VN', 'AN', 'AN-CONF', 037, true, 'R0037_AVOID_CALL_STATIC', 0, 20, 4, 3, 0, 5, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 038, true, 'R0038_AVOID_CALL_DYNAMIC_LARGE_PGM_INSIDE_LOOP', 0, 20, 4, 3, 0, 5, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 039, true, 'R0039_AVOID_FILE_OPEN_SINGLE', 0, 21, 4, 4, 0, 10, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 040, true, 'R0040_AVOID_INSTR_FORBIDDEN_FOR_CICS', 0, 29, 6, 3, 0, 480, 1, ' ', ' ', ' ' )		-- In programma Cobol/Cics ACCEPT/ENTRY/DISPLAY/FD/SD/SELECT/OPEN/CLOSEREAD/WRITER/REWRITE/DELETE/MERGE/SORT/START/STOP
        ,( 'VN', 'AN', 'AN-CONF', 041, true, 'R0041_AVOID_INSTR_SELECT_FOR_CICS', 0, 29, 6, 3, 0, 960, 1, ' ', ' ', ' ' )		    	 
        ,( 'VN', 'AN', 'AN-CONF', 042, true, 'R0042_AVOID_INSTR_FD_SD_FOR_CICS', 0, 29, 6, 3, 0, 10, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 043, true, 'R0043_AVOID_BYNARY_FIELDS_NO_SYNC', 0, 20, 4, 2, 0, 5, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 044, true, 'R0044_AVOID_MERGE_STATEMENT', 0, 34, 7, 2, 0, 240, 1, ' ', ' ', ' ' )   			 
        ,( 'VN', 'AN', 'AN-CONF', 045, true, 'R0045_AVOID_ALTER_STATEMENT', 0, 34, 7, 4, 0, 120, 1, ' ', ' ', ' ' )					  
        ,( 'VN', 'AN', 'AN-CONF', 046, true, 'R0046_AVOID_GOTO_PARAGRAPH', 0, 22, 5, 4,0, 120, 1, ' ', ' ', ' ' )				   
        ,( 'VN', 'AN', 'AN-CONF', 047, true, 'R0047_AVOID_GOTO_FROM_OUTSIDE_TO_LABEL_INSIDE_SECTION', 0, 22, 5, 4, 0, 120, 1, ' ', ' ', ' ' )				   	
        ,( 'VN', 'AN', 'AN-CONF', 048, true, 'R0048_AVOID_CICS_HANDLE_ABEND', 0, 27,6, 3,0, 60, 1, ' ', ' ', ' ' )                	 
        ,( 'VN', 'AN', 'AN-CONF', 049, true, 'R0049_AVOID_CICS_HANDLE_CONDITION', 0, 27,6, 3, 0, 60, 1, ' ', ' ', ' ' )            		 
        ,( 'VN', 'AN', 'AN-CONF', 050, true, 'R0050_AVOID_CICS_HANDLE_AID', 0, 27,6, 3, 0, 60, 1, ' ', ' ', ' ' )            		 
        ,( 'VN', 'AN', 'AN-CONF', 051, true, 'R0051_AVOID_CICS_IGNORE_CONDITION', 0, 27,6, 3, 0, 60, 1, ' ', ' ', ' ' )            		 
        ,( 'VN', 'AN', 'AN-CONF', 052, true, 'R0052_AVOID_FILE_OPEN_NOT_CLOSED', 0, 27,6, 3, 0, 10, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 053, true, 'R0053_AVOID_FILE_DEFINED_NOT_USED', 0, 14, 2, 3, 0, 5, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 054, true, 'R0054_AVOID_FILE_OPEN_NOT_USED', 0, 27,6, 3, 0, 10, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 055, true, 'R0055_AVOID_DISPLAY_UPON_CONSOLE', 0, 16, 3, 3, 0, 0, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 056, true, 'R0056_Free', 0, 0, 0, 0, 0, 0, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 057, true, 'R0057_AVOID_READ_WITHOUT_AT_END', 0, 27,6, 4, 0, 15, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 058, true, 'R0058_AVOID_SELECT_WITHOUT_FILE_STATUS', 0, 27,6, 4, 0, 15, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 059, true, 'R0059_AVOID_EVALUATE_WITHOUT_WHEN_OTHER', 0, 24, 5, 4, 0, 30, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 060, true, 'R0060_AVOID_SQLCA_NOT_INCLUDED_IN_SQL_PGM', 0, 27,6, 4, 0, 30, 1, ' ', ' ', ' ' ) 				 
        ,( 'VN', 'AN', 'AN-CONF', 061, true, 'R0061_AVOID_MOVE_REFERENCE_MODIFICATION', 0, 23, 5, 3, 0, 30, 1, ' ', ' ', ' ' )         		 
        ,( 'VN', 'AN', 'AN-CONF', 062, true, 'R0062_AVOID_CORRESPONDING_OPTION', 0, 23, 5, 2, 0, 60, 1, ' ', ' ', ' ' )         		 
        ,( 'VN', 'AN', 'AN-CONF', 063, true, 'R0063_AVOID_MOVE_TRUNCATED', 0, 22, 5, 3, 0, 30, 1, ' ', ' ', ' ' )     			 
        ,( 'VN', 'AN', 'AN-CONF', 064, true, 'R0064_AVOID_PERFORM_THRU', 0, 24, 5, 2, 0, 10, 1, ' ', ' ', ' ' )         		 
        ,( 'VN', 'AN', 'AN-CONF', 065, true, 'R0065_AVOID_PERFORM_THRU_PARAGRAPH_NOT_SINGLE', 0, 30, 6, 4, 0, 15, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 066, true, 'R0066_AVOID_EVALUATE_CLOSED_BY_DOT', 0, 14, 2, 3,0, 10, 1, ' ', ' ', ' ' )        			 
        ,( 'VN', 'AN', 'AN-CONF', 067, true, 'R0067_AVOID_IF_CLOSED_BY_DOT', 0, 14, 2, 3, 0, 10, 1, ' ', ' ', ' ' )     				 
        ,( 'VN', 'AN', 'AN-CONF', 068, true, 'R0068_AVOID_HIGH_SECTIONS_NUMBER', 0, 22, 5, 3, 18, 480, 1, ' ', ' ', ' ' )         			 
        ,( 'VN', 'AN', 'AN-CONF', 069, true, 'R0069_AVOID_HIGH_PARAGRAPHS_NUMBER', 0, 22, 5, 3,19, 480, 1, ' ', ' ', ' ' )     			 
        ,( 'VN', 'AN', 'AN-CONF', 070, true, 'R0070_AVOID_NEXT_SENTENCE', 0, 23, 5, 2, 0, 5, 1, ' ', ' ', ' ' )         		 
        ,( 'VN', 'AN', 'AN-CONF', 071, true, 'R0071_AVOID_HIGH_PGM_ROWS_SOURCE_SIZE', 0, 22, 5, 3, 10, 480, 1, ' ', ' ', ' ' )			 
        ,( 'VN', 'AN', 'AN-CONF', 072, true, 'R0072_AVOID_HIGH_PGM_INSTR_SIZE', 0, 22, 5, 3, 11, 240, 1, ' ', ' ', ' ' )     			 
        ,( 'VN', 'AN', 'AN-CONF', 073, true, 'R0073_AVOID_UNDOCUMENTED_PGM_TITLE', 0, 14, 2, 2, 0, 30, 1, ' ', ' ', ' ' )        			 
        ,( 'VN', 'AN', 'AN-CONF', 074, true, 'R0074_AVOID_LOW_DOCUMENTED_PGM_TITLE', 0, 14, 2, 2, 20, 30, 1, ' ', ' ', ' ' )         	     
        ,( 'VN', 'AN', 'AN-CONF', 075, true, 'R0075_AVOID_LOW_PGM_PERC_COMM_BY_ROW', 0, 14, 2,2, 23, 30, 1, ' ', ' ', ' ' )   
        ,( 'VN', 'AN', 'AN-CONF', 076, true, 'R0076_AVOID_LOW_PGM_PERC_COMM_BY_INSTR', 0, 14, 2,2, 24, 30, 1, ' ', ' ', ' ' ) 	 
        ,( 'VN', 'AN', 'AN-CONF', 077, true, 'R0077_AVOID_UNCHECKED_FILE_STATUS', 0, 27,6,2, 24, 30, 1, ' ', ' ', ' ' )  
        ,( 'VN', 'AN', 'AN-CONF', 078, true, 'R0078_AVOID_UNCHECKED_CICS_OPERATION', 0, 27,6,2, 24, 30, 1, ' ', ' ', ' ' ) 	
        ,( 'VN', 'AN', 'AN-CONF', 079, true, 'R0079_AVOID_UNUSED_PGM_FIELD', 0, 14, 2, 2, 0, 5, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 080, true, 'R0080_AVOID_DEAD_COPY_OR_SQL_INCLUDE', 0, 22, 5, 	2, 0, 5, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 081, true, 'R0081_AVOID_DEAD_CODE_UNREACHABLE', 0, 34, 7, 	2, 0, 15, 1, ' ', ' ', ' ' )    				 
        ,( 'VN', 'AN', 'AN-CONF', 082, true, 'R0082_AVOID_DEAD_SECTION', 0, 34, 7, 2, 0, 15, 1, ' ', ' ', ' ' )    				 
        ,( 'VN', 'AN', 'AN-CONF', 083, true, 'R0083_AVOID_DEAD_PARAGRAPH', 0, 34, 7, 2, 0, 15, 1, ' ', ' ', ' ' )    				 
        ,( 'VN', 'AN', 'AN-CONF', 084, true, 'R0084_AVOID_UNINITIALIZED_PGM_DATA_FIELD', 0, 26, 6, 2, 0, 5, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 085, true, 'R0085_AVOID_UNINITIALIZED_COPY_FIELD', 2, 26, 6, 2, 0, 5, 1, ' ', ' ', ' ' )					 
        ,( 'VN', 'AN', 'AN-CONF', 086, true, 'R0086_AVOID_LOW_PERC_FIELD_COMM_BY_ROW', 0, 14, 2, 2, 29, 30, 1, ' ', ' ', ' ' )     	 
        ,( 'VN', 'AN', 'AN-CONF', 087, true, 'R0087_AVOID_LOW_PERC_FIELD_COMM_BY_INSTR', 0, 14, 2,2, 30, 30, 1, ' ', ' ', ' ' )   	 
        ,( 'VN', 'AN', 'AN-CONF', 088, true, 'R0088_AVOID_HIGH_MCBE_PGM_COMPLEXITY', 0, 34, 7, 3, 32, 480, 1, ' ', ' ', ' ' )            	 
        ,( 'VN', 'AN', 'AN-CONF', 089, true, 'R0089_AVOID_COBOL_DIVISION_STMT_INSIDE_COPY', 0, 15, 2, 3, 0, 5, 1, ' ', ' ', ' ' )            	 
        ,( 'VN', 'AN', 'AN-CONF', 090, true, 'R0090_AVOID_HIGH_PGM_FAN_IN', 0, 22, 5, 3, 37, 480, 1, ' ', ' ', ' ' )          			 
        ,( 'VN', 'AN', 'AN-CONF', 091, true, 'R0091_AVOID_HIGH_PGM_FAN_OUT', 0, 22, 5, 3, 37, 480, 1, ' ', ' ', ' ' )          			 
        ,( 'VN', 'AN', 'AN-CONF', 092, true, 'R0092_AVOID_UNCHECKED_ALPHA_TO_NUM_MOVE', 0, 18, 3, 3, 0, 2, 1, ' ', ' ', ' ' )		
        ,( 'VN', 'AN', 'AN-CONF', 093, true, 'R0093_AVOID_CICS_COMMAREA_AND_LENGTH_MISSING', 0, 27,6, 3, 0, 120, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 094, true, 'R0094_AVOID_GOBACK_NOT_IN_MAINLINE', 0, 15, 2, 3, 0, 60, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 095, true, 'R0095_AVOID_USE_OF_POINTERS', 0, 22, 5, 3, 0, 60, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 096, true, 'R0096_AVOID_PARAGRAPH_NOT_ENDED_BY_EXIT', 0, 14, 2, 2, 0, 2, 1, ' ', ' ', ' ' )		    		 
        ,( 'VN', 'AN', 'AN-CONF', 097, true, 'R0097_AVOID_SECTION_NOT_ENDED_BY_EXIT', 0, 14, 2, 1, 0, 2, 1, ' ', ' ', ' ' )		    	 
        ,( 'VN', 'AN', 'AN-CONF', 098, true, 'R0098_AVOID_NESTED_PGMS', 0, 22, 5, 3, 0, 30, 1, ' ', ' ', ' ' )      
        ,( 'VN', 'AN', 'AN-CONF', 099, true, 'R0099_AVOID_NESTED_COPY', 0, 14, 2, 2, 0, 5, 1, ' ', ' ', ' ' )    		 
        ,( 'VN', 'AN', 'AN-CONF', 100, true, 'R0100_AVOID_COPY_ACROSS_DIVISION', 0, 15, 2,2, 0, 30, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 101, true, 'R0101_AVOID_ID_DIVISION_MISSING', 0, 11, 1, 1, 0, 5, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 102, true, 'R0102_AVOID_ENV_DIVISION_MISSING', 0, 11, 1, 1, 0, 5, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 103, true, 'R0103_AVOID_DATA_DIVISION_MISSING', 0, 11, 1, 1, 0, 5, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 104, true, 'R0104_AVOID_SOURCE_ROW_TABSET', 0, 8, 1, 2, 0, 5, 1, ' ', ' ', ' ' )    		 		
        ,( 'VN', 'AN', 'AN-CONF', 105, true, 'R0105_AVOID_UNCLOSED_FIELD_DEF_BY_DOT', 0, 11, 1, 1, 0, 5, 1, ' ', ' ', ' ' )	 
        ,( 'VN', 'AN', 'AN-CONF', 106, true, 'R0106_AVOID_INSTR_START_BEFORE_AREA_A', 0, 8, 1, 1, 0, 5, 1, ' ', ' ', ' ' )	 
        ,( 'VN', 'AN', 'AN-CONF', 107, true, 'R0107_AVOID_LABEL_AT_COL_GREATER_8', 0, 8, 1, 2, 0, 5, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 108, true, 'R0108_AVOID_NOT_STD_SIZE_LABEL', 0, 14, 2, 2, 6, 15,1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 109, true, 'R0109_AVOID_NOT_STD_SIZE_FIELD_NAME', 0, 14, 2, 2, 5, 15, 1, ' ', ' ', ' ' )  
        ,( 'VN', 'AN', 'AN-CONF', 110, true, 'R0110_AVOID_NOT_STD_PGM_NAME', 0, 15, 2, 1, 0, 30, 1, ' ', ' ', ' ' )	 
        ,( 'VN', 'AN', 'AN-CONF', 111, true, 'R0111_AVOID_NOT_STD_SECTION_NAME', 0, 15, 2, 1, 0, 30, 1, ' ', ' ', ' ' )	 
        ,( 'VN', 'AN', 'AN-CONF', 112, true, 'R0112_AVOID_NOT_STD_PARAGRAPH_NAME', 0, 15, 2, 1, 0, 30, 1, ' ', ' ', ' ' ) 
        ,( 'VN', 'AN', 'AN-CONF', 113, true, 'R0113_AVOID_NOT_STD_LABEL_NAME', 0, 15, 2, 1, 0, 5, 1, ' ', ' ', ' ' )	 
        ,( 'VN', 'AN', 'AN-CONF', 114, true, 'R0114_AVOID_NOT_STD_FIELD_NAME', 0, 15, 2, 1, 0, 5, 1, ' ', ' ', ' ' )	
        ,( 'VN', 'AN', 'AN-CONF', 115, true, 'R0115_AVOID_NOT_STD_PARAGRAPH_THRU_NAME', 0, 15, 2, 1, 0, 5, 1, ' ', ' ', ' ' )		
        ,( 'VN', 'AN', 'AN-CONF', 116, true, 'R0116_AVOID_NOT_STD_SECTION_THRU_NAME', 0, 15, 2, 1, 0, 5, 1, ' ', ' ', ' ' )		
        ,( 'VN', 'AN', 'AN-CONF', 117, true, 'R0117_AVOID_STOP_RUN_NOT_IN_MAINLINE', 0, 15, 2, 3, 0, 60, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 118, true, 'R0118_AVOID_PROGRAM_ID_UNEQUAL_FILE_NAME', 0, 15, 2, 2, 0, 60, 1, ' ', ' ', ' ' )		
        ,( 'VN', 'AN', 'AN-CONF', 119, true, 'R0119_AVOID_HIGH_CONDITIONS_NUMBER', 0, 34, 7, 3, 18, 5, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 120, true, 'R0120_AVOID_COPY_INSIDE_PROCEDURE_DIVISION', 0, 14, 2, 3, 0, 2, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 121, true, 'R0121_AVOID_INITIALIZE_STATEMENT', 0, 15, 2, 2, 0, 2, 1, ' ', ' ', ' ' )				
        ,( 'VN', 'AN', 'AN-CONF', 122, true, 'R0122_AVOID_REDEFINES_CLAUSE', 0, 23, 5, 2, 0, 2, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 123, true, 'R0123_AVOID_SECTION_IN_PROCEDURE', 2, 22, 5, 2, 	0, 2, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 124, true, 'R0124_AVOID_PARAGRAPH_IN_PROCEDURE', 2, 22, 5, 2, 0, 2, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 125, true, 'R0125_AVOID_VALUE_CLAUSE_IN_LINKAGE', 0, 29, 6, 2, 0, 2, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 126, true, 'R0126_AVOID_LINKAGE_DATA_NOT_IN_COPYBOOK', 0, 27,6, 3, 0, 2, 1, ' ', ' ', ' ' )				
        ,( 'VN', 'AN', 'AN-CONF', 127, true, 'R0127_AVOID_BLOCK_CODE_DUPLICATED', 0, 24, 5, 3, 15, 2, 1, ' ', ' ', ' ' )		
        ,( 'VN', 'AN', 'AN-CONF', 128, true, 'R0128_AVOID_BLOCK_CODE_COMMENTED', 0, 15, 2, 3, 16, 2, 1, ' ', ' ', ' ' )				
        ,( 'VN', 'AN', 'AN-CONF', 129, true, 'R0129_AVOID_EVALUATE_WHEN_WITH_LOGIC_CONDITIONAL', 0, 34, 7, 3, 0, 2, 1, ' ', ' ', ' ' )				
        ,( 'VN', 'AN', 'AN-CONF', 130, true, 'R0130_AVOID_MAGIC_LITERAL_ALPHANUMERIC', 0, 24, 5, 3, 0, 2, 1, ' ', ' ', ' ' )				
        ,( 'VN', 'AN', 'AN-CONF', 131, true, 'R0131_AVOID_MAGIC_LITERAL_NUMERIC', 0, 24, 5, 3, 0, 2, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 132, true, 'R0132_AVOID_UNINITIALIZED_CALL_PARAMETER_DEFINED_INSIDE_COPY', 2, 28, 6, 3, 0, 2, 1, ' ', ' ', ' ' )				
        ,( 'VN', 'AN', 'AN-CONF', 133, true, 'R0133_AVOID_UNINITIALIZED_CALL_PARAMETER_DEFINED_INSIDE_PGM', 0, 28, 6, 3, 0, 2, 1, ' ', ' ', ' ' )				
        ,( 'VN', 'AN', 'AN-CONF', 134, true, 'R0134_AVOID_UNINITIALIZED_CALL_PARAMETER_AREA_COPY', 0, 28, 6, 3, 0, 2, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 135, true, 'R0135_AVOID_COMPUTE_FOR_SIMPLE_OPERATIONS', 0, 21, 4, 2, 0, 2, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 136, true, 'R0136_AVOID_IF_TRUE_EMPTY', 0, 15, 2, 3, 0, 2, 1, ' ', ' ', ' ' )		
        ,( 'VN', 'AN', 'AN-CONF', 137, true, 'R0137_AVOID_IF_ELSE_EMPTY', 0, 15, 2, 3, 0, 2, 1, ' ', ' ', ' ' )		
        ,( 'VN', 'AN', 'AN-CONF', 138, true, 'R0138_AVOID_SORT_STATEMENT', 0, 34, 7, 2, 0, 2, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 139, true, 'R0139_Free', 0, 0, 0, 0, 0, 0, 0, 0, ' ', ' ', ' ' )		
        ,( 'VN', 'AN', 'AN-CONF', 140, true, 'R0140_AVOID_SQL_SELECT_STAR', 0, 20, 4, 3, 0, 120, 1, ' ', ' ', ' ' )
        ,( 'VN', 'AN', 'AN-CONF', 141, true, 'R0141_AVOID_SQL_USE', 2, 22, 5, 3, 0, 120, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 142, true, 'R0142_AVOID_SQL_OPEN_CURSOR_INSIDE_LOOP', 0, 21, 4, 3, 0, 5, 1, ' ', ' ', ' ' )		
        ,( 'VN', 'AN', 'AN-CONF', 143, true, 'R0143_AVOID_SQL_UNCLOSED_CURSOR', 0, 21, 4, 3, 0, 120, 1, ' ', ' ', ' ' )				
        ,( 'VN', 'AN', 'AN-CONF', 144, true, 'R0144_AVOID_SQL_DECLARED_AND_UNUSED_CURSOR', 0, 21, 4, 3, 0, 120, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 145, true, 'R0145_AVOID_SQL_HIGH_WHERE_CONDITIONS_NUMBER', 0, 15, 2, 3, 34, 5, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 146, true, 'R0146_AVOID_SQL_HIGH_TABLES_NUMBER', 0, 23, 5, 3, 33, 120, 1, ' ', ' ', ' ' )		    		
        ,( 'VN', 'AN', 'AN-CONF', 147, true, 'R0147_AVOID_SQL_HIGH_SUBSELECT_NESTED_NUMBER', 0, 23, 5, 3, 36, 120, 1, ' ', ' ', ' ' )		    	
        ,( 'VN', 'AN', 'AN-CONF', 148, true, 'R0148_AVOID_SQL_HIGH_TABLES_JOINED_NUMBER', 0, 23, 5, 3, 35, 120, 1, ' ', ' ', ' ' )		    		
        ,( 'VN', 'AN', 'AN-CONF', 149, true, 'R0149_AVOID_SQL_DECLARE_CURSOR_WITH_NO_ORDER_BY', 0, 8, 1, 2, 0, 5, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 150, true, 'R0150_AVOID_SQL_SELECT_WITH_UNION', 0, 20, 4, 3, 0, 120, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 151, true, 'R0151_AVOID_SQL_SELECT_WITH_DISTINCT', 0, 23, 5, 3, 0, 2, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 152, true, 'R0152_AVOID_SQL_SELECT_WITH_GROUP_BY', 0, 23, 5, 3, 0, 2, 1, ' ', ' ', ' ' )			
        ,( 'VN', 'AN', 'AN-CONF', 153, true, 'R0153_AVOID_SQL_DYNAMIC', 0, 21, 4, 3, 0, 120, 1, ' ', ' ', ' ' )					
        ,( 'VN', 'AN', 'AN-CONF', 154, true, 'R0154_AVOID_SQL_SELECT_WITH_NO_WHERE', 0, 20, 4, 2, 0, 5, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 155, true, 'R0155_AVOID_SQL_DELETE_WITH_NO_WHERE', 0, 20, 4, 2, 0, 5, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 156, true, 'R0156_AVOID_SQL_UPDATE_WITH_NO_WHERE', 0, 20, 4, 2, 0, 5, 1, ' ', ' ', ' ' )		 
        ,( 'VN', 'AN', 'AN-CONF', 157, true, 'R0157_AVOID_SQL_WHERE_WITH_LIKE_PREDICATE', 0, 20, 4, 2, 0, 2, 1, ' ', ' ', ' ' )	
        ,( 'VN', 'AN', 'AN-CONF', 158, true, 'R0158_AVOID_SQL_WITH_DEPRECATED_SYNTAX', 0, 8, 1, 2, 0, 5, 1, ' ', ' ', ' ' )	 
        ,( 'VN', 'AN', 'AN-CONF', 159, true, 'R0159_AVOID_SQL_UNCHECKED_CODE', 0, 27,6, 2, 0, 10, 1, ' ', ' ', ' ' )				 
        ,( 'VN', 'AN', 'AN-CONF', 160, true, 'R0160_AVOID_SQL_ORDER_BY_WITH_NO_INDEX', 0, 21, 4, 3, 0, 10, 1, ' ', ' ', ' ' )			 
        ,( 'VN', 'AN', 'AN-CONF', 161, true, 'R0161_AVOID_SQL_TEMPORARY_TABLE', 0, 21, 4, 3, 0, 10, 1, ' ', ' ', ' ' )
        ;					 




