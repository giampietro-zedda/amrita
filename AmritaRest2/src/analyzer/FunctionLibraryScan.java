package analyzer;

import java.io.File;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import dao.DAOImplObject;
import dao.DAOImplObjectOption;
import dao.DAOImplRelation;
import dao.IDAOObject;
import dao.IDAOObjectOption;
import dao.IDAORelation;
import utilities.StringService;
import entities.EntityObject;
import entities.EntityObjectOption;
import entities.EntityRelation;
import enums.EnumDirectivesExecution;
import enums.EnumMessageType;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import enums.EnumRelation;
import enums.EnumSourceType;
import exception.ExceptionAmrita;

/**
 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * FunctionLibraryScan
 * </h1>
 * <p>
 * Permette di classificare i sorgenti in input scrivendo una riga nella tabella Object.<br>
 * <p>
 * Viene letto il file pilota di sorgenti e filtri e vengono individuati tutti i sorgenti  implicati. 
 * Vengono restituiti i paths completi dei sorgenti individuati attraverso i files pilota e di filtro
 * in due modi: come array di oggetti SourceInput e come file nella directory di output.
 * Può essere eseguito come thread oppure in modo sincrono.
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 1/04/2010
 * @see Analyzer
 * @see ExecutionDispatcher
*/
public class FunctionLibraryScan extends ExecutionShared implements Runnable{

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza specifiche                                                                    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
 
	
    // Attenzione !! 
	// le direttive attive sono disponibili nella variabile di istanza DirectivesInfo di, della superclasse
	// valorizzata dal costruttore, che contiene anche tutte le informazioni di esecuzione.
	
	
	// Controllo sorgenti
    private int cntSourcesExtracted = 0;                 // Counter sorgenti di tipo individuato
    private int cntSourcesIdentified = 0;                // Counter sorgenti di tipo individuato
    private int cntSourcesNotIdentified = 0;             // Counter sorgenti di tipologia non identificata
    private int cntSourcesNotRequired = 0;               // Counter sorgenti di tipologia identificata ma non richiesta per l'esecuzione

	
	/**
	 * 
	 * Costruttore 
	 * 
	 * I file di pilot con i sources/directories da trattare e con i processi da attivare, 
	 * si trovano nella directory DirPilotAndFilter che permettono di personalizzare i processi di analisi. 
	 * Nei files di pilot sono presenti paths completi di sorgenti e directories da analizzare, 
	 * con eventuali commenti e informazioni di filtro.
	 * 
	 * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 * 
	 */
	public FunctionLibraryScan(UserConfiguration sd, ArrayList<ExecutionDirectives> al_di) throws ExceptionAmrita, SQLException {
		super(sd, al_di.get(al_di.size()-1));	
		this.al_di = al_di;
 	}
	
	
   /**
    * 
    * Attivato a fronte dell'esecuzione di start() sul thread  da {@link ExecutionDispatcher} <br>
    * 
    */
	public void run() {
		
		this.di.excpOccurred = null;
		this.di.isExecutionWithErrors = false;

		try {
			
			exec();
			
		} catch (Exception e) {
			// L'esecuzione è in un thread separato, l'ora di fine elaborazione è già stata impostata da execFunction.
			// Nel caso di eccezione di tipo AmritaException, è già stata loggata e lo stack trace prodotto,
			// altrimenti tutto è a cura del chiamante ExecutionDispatcher.
			logMessage(EnumMessageType.ERROR_FATAL, "EF0021", e);
			di.excpOccurred = e;
		}
		
	}
	

	/**
	 * 
	 * Funzione eseguita in modo sincrono o come thread separato.
	 * Questo metodo di ingresso permette di effettuare operazioni iniziali
	 * e finali relativi alla funzione da eseguire.
	 * Per esempiovengono impostati l'ora di inzio, fine ed elapsed di elaborazione.
	 * In caso di eccezione viene impostato il flag di funzione eseguita con errori
	 * nell'oggetto di direttiva di esecuzione DirectivesInfo.
	 *  
	 * @throws Exception 
	 * 
	 */
	public void exec() throws Exception  {

		this.di.execMsAllStart = System.currentTimeMillis();
		this.di.isExecutionWithErrors = false;
		
		try {
			
			execSourcesDetected();
			
			this.di.execMsAllEnd = System.currentTimeMillis();
			this.di.execMsAllElapsed = (int) (this.di.execMsAllEnd - this.di.execMsAllStart);
			this.ucfg.setExecMsAllElapsed(this.di.execMsAllElapsed);
			
		} catch (Exception e) {
			this.di.excpOccurred = e;
			this.di.isExecutionWithErrors = true;
			this.di.execMsAllEnd = System.currentTimeMillis();
			this.di.execMsAllElapsed = (int) (this.di.execMsAllEnd - this.di.execMsAllStart);
			this.ucfg.setExecMsAllElapsed(this.di.execMsAllElapsed);
			// Se la funzione è stata lanciata in modo statico bisogna rilanciarla al chiamante
			// Se invece la funzione è stata chiamata dinamicamente con Invoke, il controllo torna
			// in ogni caso all'istruzione dopo la Invoke, dove l'oggetto DirectivesInfo di è disponibile
			// e contiene questa exception, che viene quindi gestita in quel punto.
			// se la funzione era stata lanciata come thread separato, ExecutionDispatcher verificherà la fine
			// anomala del thread testando la presenza di exception in di.excpOccurred, disponibile a fine elaborazione.
			this.di.execCntObjectProcessedExcp++;
			this.di.exec_alObjectNameExcp.add(this.di.curObjectId);
			this.logApplicationInfoException();		 // Informazioni applicative
            this.logSystemInfoException();           // Informazioni di debug se presenti
		}
		return;
	}
	
	

	/**
	 * 
	 * Funzione eseguita in modo sincrono o come thread separato
	 *  
	 * @return SourceInput[] array with sources coded 
	 * @throws Exception 
	 * 
	 */
	public void execSourcesDetected() throws Exception {
		
		SourceManager sm = null;                    		// Gestore generalizzato recupero sorgenti
 		SourceInput si = null;                          	// Descrittore sorgente
		SourceInput siWithSource = null;                	// Descrittore sorgente con le righe sorgente lette da disco
        SourceInput ar_sourceInputDetected[] = null; 		// Descrittore sorgente 
        ArrayList<SourceInput> al_sourceInputDetected = null;// Descrittori sorgenti files intercettati
        String arRowSourcesDetected[] = null;				// Righe pilota processi/funziono
        ArrayList<String> alRowSourcesDetected = null;		// Righe pilota processi/funziono
        ExecutionDirectives diFunction = null;                   // Decrittore direttiva di esecuzione FUNCTION_DETECT_SOURCES
        String dirOutput = null;							// Directory di output
        String fileSourcesDetectedOutput = null;     		// Include eventuale postfisso finale .xxx
        String pathSourcesDetectedOutput = null;	 		// Path completo file con nomi sorgenti in output
        String arParm[] = null;                      		// Parametri messaggio
        String strRowMessage = ""; 					 		// Riga di messaaggio sorgente intercettato
        String strSourceType = "";                          // Decodifica tipo sorgente
        int cntSourcesInMemory = 0;                     	// Numero sorgenti in memoria
        int linesCurSource = 0;                         	// Numero linee sorgente corrente
        int cntSourceCur = 0;                         	    // Numero sorgente corrente
        int cntPasso = 0;                                   // Passo
        
        EnumSourceType enSourceType = EnumSourceType.NOT_ASSIGNED;
        diFunction = al_di.get(al_di.size() - 1);
  
        
        ///////////////////////////////////////////////////////////////////////////////////////////
        /// (1) Estrazione e individuazione nomi sorgenti                                       
        ///////////////////////////////////////////////////////////////////////////////////////////        
        
		// Recupero file dir e name  di output
		dirOutput = ucfg.getDirOutput();
		fileSourcesDetectedOutput = this.di.fileNameOutput;
		pathSourcesDetectedOutput = ucfg.getPathUser() + File.separator + dirOutput + File.separator + fileSourcesDetectedOutput;
		sm = new SourceManager(ucfg);
		
    	loggingMemoryStatus();
    	
		// Lettura files da trattare attraverso pilota sources (contiene direttive e filtri)
    	ar_sourceInputDetected = sm.listFilesFromPilot(al_di                                  // Descrittore direttive estrazione sorgenti
												      ,di.optSearchFilesRecursive             // Ricerca ricorsiva true/false
		 										      ,di.optDirectoryOnOutput                // Dir in output true/false
		                                              );								
	

		///////////////////////////////////////////////////////////////////////////////////////////
        /// (2) Individuazione tipologie sorgenti  
        ///////////////////////////////////////////////////////////////////////////////////////////        

		// Sono a questo punto stati estratti tutti i sorgenti che soddisfavano i criteri di filtro sul nome sorgente.
		// In questa fase, per ogni file sorgente, viene determinata la sua tipologia e vengono portati in output solo 
		// quelli della tipologia specificata, attiva nel file pilota prima della direttiva LIBRARY / FILE_PATH.

    	
		// Alloco array con i nomi dei sorgenti
		alRowSourcesDetected = new ArrayList<String>();
		al_sourceInputDetected  = new ArrayList<SourceInput>();
		
		cntSourcesExtracted = ar_sourceInputDetected.length;
		
		// Scan array descrittori sorgenti
        for (int i = 0; i < ar_sourceInputDetected.length; i++) {
        	
        	cntPasso++;
        	
        	if (cntPasso > 100) {
        		// Individuazione tipologia in corso
        		logMessage(EnumMessageType.INFORMATION, "MI0304", cntSourceCur+"", cntSourcesExtracted+"");
        		cntPasso = 0;
			}

        	
        	cntSourcesInMemory++;			// Counter sorgenti trattati in memoria
            
        	// Descrittore source SENZA array di righe da disco
         	si = (SourceInput) ar_sourceInputDetected[i].clone();
            
        	// Directory: skip
        	if (si.isDirectory()) {
  				continue;
			}
        	
        	cntSourceCur++;
	
        	// Tipologia sorgente da individuare
           	enSourceType = EnumSourceType.NOT_ASSIGNED;
           	
        	linesCurSource = 0;
        	
        	if (si.getDi().optDetectSourceType) {
        		
        		// Descrittore source CON array di righe da disco
          		siWithSource = sm.getSource(si.getPathComplete(), false, false);
          		linesCurSource = siWithSource.getArrayRowSource().length;
  				sm.detectSourceType(siWithSource);
  				enSourceType = siWithSource.getSourceType();
  				strSourceType = enSourceType.name();
  				
 			    // Tipologia sorgente NON in pilota sorgenti: skip
  	      	    if (!isSourceTypeToManage(si.getDi().al_objectTypeToProcess, enSourceType) && enSourceType != EnumSourceType.NOT_ASSIGNED) {
   	      	        cntSourcesNotRequired++;
   	      	        strSourceType = "NOT REQUIRED";
					
				// Tipologia sorgente non individuata: skip
				} else if (enSourceType == EnumSourceType.NOT_ASSIGNED) {
					cntSourcesNotIdentified++;
		          	al_sourceInputDetected.add(ar_sourceInputDetected[i]);
					if (siWithSource.getArrayRowSource().length == 0) {
						strSourceType = "EMPTY FILE";
					}
    			    
    			// Tipologia sorgente individuata: imposto a livello di descrittore sorgente  
				} else {
	 	      	    ar_sourceInputDetected[i].setSourceType(enSourceType);
	 	          	al_sourceInputDetected.add(ar_sourceInputDetected[i]);
	 	          	cntSourcesIdentified++; // Counter sorgenti di tipologia individuata
				}
         	}
         	
        	// Test su limitazione sorgenti di tipologia individuata
           	if (si.getDi().limitMaxObjects &&  cntSourcesIdentified > si.getDi().limitMaxObjectsToProcess) {	
 				AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0022", null, null); 
        		break;       // Interrompi for
			}
        	
        	// Estrazione informazioni singolo sorgente
        	StringService ssSourceType = new StringService(strSourceType);
        	StringService ssSys = new StringService(si.getSystemOwner());
        	StringService ssSubSys = new StringService(si.getSubSystemOwner());
        	StringService ssIdSource = new StringService(si.getIdSource());
          	StringService ssDate = new StringService(si.getDateLastModFormatted());
           	StringService ssSize = new StringService(new Long(si.getSize()).toString());
           	StringService ssLines = new StringService(new Integer(linesCurSource).toString());
        	StringService ssPathComplete = new StringService(si.getPathComplete());
           	
        	// Composizione riga da mandare in output
        	strRowMessage = "";
        	strRowMessage = strRowMessage + ssSys._pad(' ', 5, StringService.PAD_LEFT);
        	strRowMessage = strRowMessage + "  ";
        	strRowMessage = strRowMessage + ssSubSys._pad(' ', 5, StringService.PAD_LEFT);
        	strRowMessage = strRowMessage + "  ";
        	strRowMessage = strRowMessage + ssSourceType._pad(' ', 35, StringService.PAD_RIGHT);
        	strRowMessage = strRowMessage + ssIdSource._pad(' ', 10, StringService.PAD_RIGHT);
           	strRowMessage = strRowMessage + ssDate._pad(' ', 12, StringService.PAD_RIGHT);
           	strRowMessage = strRowMessage + ssSize._pad('0', 10, StringService.PAD_LEFT);
        	strRowMessage = strRowMessage + "  ";
          	strRowMessage = strRowMessage + ssLines._pad('0', 5, StringService.PAD_LEFT);
           	strRowMessage = strRowMessage + "  ";
        	strRowMessage = strRowMessage + ssPathComplete._pad(' ', 100, StringService.PAD_RIGHT);
        	
        	// Intabellamento riga composta e descrittore per successivo update db
        	alRowSourcesDetected.add(strRowMessage);        				
        	
			// Erase references X garbage collector
        	siWithSource = null;
		
        }	// end-for	
		
		
        ///////////////////////////////////////////////////////////////////////////////////////////
        /// (3) Generazione file di output attraverso i servizi standard di SourceManager         
        ///////////////////////////////////////////////////////////////////////////////////////////        

		arRowSourcesDetected = new String[alRowSourcesDetected.size()];
		arRowSourcesDetected = alRowSourcesDetected.toArray(arRowSourcesDetected);
        sm.writeSource(pathSourcesDetectedOutput, arRowSourcesDetected);

        
        ///////////////////////////////////////////////////////////////////////////////////////////
        /// (5) Aggiornamento db con oggetti e relazioni                                         
        ///////////////////////////////////////////////////////////////////////////////////////////        

       	if (diFunction.optUpdateDb) {
       		// Oggetti, membri di libreria e relazioni con queste
			dataBaseUpdInfoSourceDetected(al_sourceInputDetected, al_di);
		}
    	
    	// Nella direttiva di processo di lancio è disponibile, per il chiamante, l'array di sources
    	diFunction.objectOutput = ar_sourceInputDetected;

    	
        ///////////////////////////////////////////////////////////////////////////////////////////
        /// (6) Messaggi conclusivi                                         
        ///////////////////////////////////////////////////////////////////////////////////////////        
   	
		arParm = new String[4];
		arParm[0] = cntSourcesExtracted + "";
		arParm[1] = cntSourcesIdentified + "";
		arParm[2] = cntSourcesNotIdentified + "";
		arParm[3] = cntSourcesNotRequired + "";
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0043", arParm, null); 
		arParm = new String[1];
		arParm[0] = pathSourcesDetectedOutput;
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0042", arParm, null); 
    	return;

 	}

	// Rilascio memoria con garbage collector
//	if (cntSourcesInMemory > sd.getDebugSourcesDetectedFreqGarbage()) {
//		freeMemory(sd.getDebugThresholdMemoryGarbage());
//		loggingMemoryStatus();
//		cntSourcesInMemory = 0;
//	}
	
	

	///////////////////////////////////////////////////////////////////////////////
	///////   Metodi privati                                                ///////
	///////////////////////////////////////////////////////////////////////////////
	
	/*
	 * Restituisce true se il tipo sorgente è fra quelli previsti e dichiarati nel pilota sorgenti
	 */
	private boolean isSourceTypeToManage(ArrayList<EnumDirectivesExecution>  al_enSourceTypeInPilot, EnumSourceType enSourceTypeInput) {
		
		boolean bRet = false;
		
		// Scan Array con direttive di sorgente impostate in fase di analisi del pilota sorgenti
        for (EnumDirectivesExecution enExecutionPilot : al_enSourceTypeInPilot) {
			
        	// Direttiva di inclusione di tutti i sorgenti
        	if (enExecutionPilot == EnumDirectivesExecution.OBJECT_TYPE_INCLUDE_ALL) {
        		bRet = true;
        		break;
			}
 
        	// Direttiva di esclusione di tutti i sorgenti
        	if (enExecutionPilot == EnumDirectivesExecution.OBJECT_TYPE_EXCLUDE_ALL) {
        		bRet = false;
        		break;
			}

        	// Non è una direttiva source di tipo sorgente
        	if (enExecutionPilot.getSourceType()  == EnumSourceType.NOT_ASSIGNED) {
				continue;
			}
         	
        	// Tipologia oggetto sorgente fra quelle previste nell'elaborazione dl file pilota
        	if (enExecutionPilot.getSourceType().toString().equals(enSourceTypeInput.toString())) {
        		bRet = true;
        		break;
			}
        }

		return bRet;
	}
    

	
	/*
	 * 
	 *  Aggiornamento database con informazioni sui sorgenti, librerie dalle
	 *  quali sono stati estratti con relative relazioni.
	 * 
	 */
	private void dataBaseUpdInfoSourceDetected(ArrayList<SourceInput> al_sourceInputDetected, ArrayList<ExecutionDirectives> al_Di) throws Exception {

		int cntCurSources = 0;
		int cntTotSources = 0;
		int cntPasso = 0;
		
		Set<String> set_system = null;
		Set<String> set_subSystem = null;
		Set<String> set_SystemSubSystem = null;
		
		// Entities, contenitori dei dati
		EntityObject entityObject = null;
		EntityObjectOption entityObjectOption = null;
		EntityRelation entityRelation = null;
		
		// Campi di servizio
		Map<String, InnerLibrary> map_library = new HashMap<String, InnerLibrary>();   // Key = codice libreria
		InnerLibrary library = null;
		EnumObjectOption en_objectOption = null;

		boolean isObjectFound = false;
		boolean isSourceFound = false;

		set_system = new HashSet<String>() ;
		set_subSystem = new HashSet<String>() ;
		set_SystemSubSystem = new HashSet<String>() ;

        // Allocazione Entities da inserire, si usa sempre lo stesso oggetto
		entityObject = new EntityObject();
		entityObjectOption = new EntityObjectOption();
		entityRelation = new EntityRelation();

		// Classificazione librerie
		for (int i = 0; i < di.al_libraryCode.size(); i++) {
           	library = new InnerLibrary();
           	library.librarySourceLogicName = di.al_libraryCode.get(i);
           	library.librarySourcePath = di.al_libraryPath.get(i);
           	map_library.put(di.al_libraryCode.get(i), library);
			
		}
				
		// Classificazione Sistemi, sottosistemi e loro combinazione 
		// Si utilizzano i valori e le combinazioni reali trovate nei nomi sorgente
 		for (SourceInput sourceInput : al_sourceInputDetected) {

 			set_system.add(sourceInput.getSystemOwner());
 			set_subSystem.add(sourceInput.getSubSystemOwner());
 			set_SystemSubSystem.add(sourceInput.getSystemOwner() + "-" + sourceInput.getSubSystemOwner());
 			
            // Inserimento info libreria in Map per eliminare i duplicati e memorizzare info specifiche
            if (!sourceInput.getLibraryCode().equals("")) {
                library = map_library.get(sourceInput.getLibraryCode());
                // Update flag di istanza indicanti il tipo di sorgente individuato
               setTypeSourcesDetected(sourceInput.getSourceType(), library);
			} 
		}
		
	    Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false,false, ucfg);
		IDAORelation eoDAORelation = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);
		IDAOObjectOption eoDAOObjectOption = (DAOImplObjectOption) AmritaStartup.sqlFactory.getDAOObjectOption(conn, false,false, ucfg);
		
 		/////////////////////////////////////////////////////////////////////////////
 		// (0) Inserimento oggetti sys/subSys agganciati a sys/subSys di servizio */*
 		/////////////////////////////////////////////////////////////////////////////
 				
		// System (agganciato  a generico system/subsystem */*) 
		for (String sys : set_system) {
			entityObject.setSystem("*");   						 
			entityObject.setSubSystem("*");    						 
			entityObject.setIdObject(sys);    						 
			entityObject.setTypeObject(EnumObject.OBJECT_SYS);
			entityObject.setSystemOwner(di.systemInput);   						 
			entityObject.setSubSystemOwner("*");    						 
			entityObject.setStatus(EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED);
			eoDAO.create(entityObject);			
		}		 
 		
		// SubSystem (agganciato  a generico system *)
		for (String subSys : set_subSystem) {
			entityObject.setSystem(di.systemInput);   						 
			entityObject.setSubSystem("*");  
			entityObject.setIdObject(subSys);    						 
			entityObject.setTypeObject(EnumObject.OBJECT_SUBSYS);	
			entityObject.setSystemOwner("*");   						 
			entityObject.setSubSystemOwner("*");    						 
			entityObject.setStatus(EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED);
			eoDAO.create(entityObject);
		}
			
 		/////////////////////////////////////////////////////////////////////////////
 		// (1) Inserimento oggetti libreria agganciati a sys/subSys di servizio
 		/////////////////////////////////////////////////////////////////////////////
 		
 		for (String libraryKey : map_library.keySet()) {
 	   		
			// Recupero info libreria
            library = map_library.get(libraryKey);
 			
 			// Primary Key
    		entityObject.setSystem(di.systemInput);   						 
    		entityObject.setSubSystem("*"); 
 			entityObject.setIdObject(libraryKey);  
 			entityObject.setTypeObject(EnumObject.OBJECT_LIBRARY);	
			entityObject.setLibraryDir(library.librarySourcePath);
			entityObject.setLibrarySourceObject(libraryKey);
			entityObject.setLibrarySource("");
			entityObject.setFileSource("");
			entityObject.setLibraryDir(library.librarySourcePath);
 			entityObject.setStatus(EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED);
 			entityObject.setSystemOwner(di.systemInput);
 			entityObject.setSubSystemOwner("*");

            // Inserimento oggetto library generico in database: se duplicate devo aggiornare
            if (!eoDAO.create(entityObject)) {
            	eoDAO.update(entityObject);
			}
            
            // Options Library su system/subsystem generali (library con pgm,copy, bms etc)
			entityObjectOption.setSystem(di.systemInput);   						 
			entityObjectOption.setSubSystem("*"); 
 			entityObjectOption.setIdObject(libraryKey);  						 
			entityObjectOption.setTypeObject(EnumObject.OBJECT_LIBRARY);	
			insertObjectOptionLibrary(eoDAOObjectOption, entityObjectOption, library);            
   		}
 		
 		conn.commit();    // Consolido gli ultimi aggiornamenti 
		
 		/////////////////////////////////////////////////////////////////////////////
 		//(2) Inserimento oggetti sorgente / opzioni oggetti / relazione con libreria
 		/////////////////////////////////////////////////////////////////////////////

		cntTotSources = al_sourceInputDetected.size();
		cntCurSources = 0;
		cntPasso = 0;
		

 		// Scan oggetti da inserire
 		for (SourceInput sourceInput : al_sourceInputDetected) {

 	   	    cntCurSources++;
 	   	    cntPasso++;
			
 	   		if (cntPasso > 100) {
				logMessage(EnumMessageType.INFORMATION, "MI0312", cntCurSources + "", cntTotSources + "");
				cntPasso=0;
			}

	 		/////////////////////////////////////////////////////////////////////////////
 	 		//(2.1) Inserimento oggetto librearia nel sottosistema dove l'oggetto è censito
 	   		// altrimenti non si può inserire la relazione library-source member
  	 		///////////////////////////////////////////////////////////////////////////// 			
 			
			// Recupero info libreria
            library = map_library.get(sourceInput.getLibraryCode());
 			
 			// Primary Key
    		entityObject.setSystem(di.systemInput);   						 
    		entityObject.setSubSystem(sourceInput.getSubSystemOwner()); 
 			entityObject.setIdObject(sourceInput.getLibraryCode());  
 			entityObject.setTypeObject(EnumObject.OBJECT_LIBRARY);	
			entityObject.setLibraryDir(library.librarySourcePath);
			entityObject.setLibrarySourceObject(sourceInput.getLibraryCode());
			entityObject.setLibrarySource("");
			entityObject.setFileSource("");
			entityObject.setLibraryDir(library.librarySourcePath);
 			entityObject.setStatus(EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED);
 			entityObject.setSystemOwner(di.systemInput);
 			entityObject.setSubSystemOwner("*");

            // Inserimento oggetto library generico in database: se duplicate devo aggiornare
            if (!eoDAO.create(entityObject)) {
            	eoDAO.update(entityObject);
			}
 	   		
 	   		
	 		/////////////////////////////////////////////////////////////////////////////
 	 		//(2.2) Inserimento oggetto e opzioni oggetto
  	 		///////////////////////////////////////////////////////////////////////////// 			
 			
 			// Scrittura oggetto relativo al sorgente (Se SOURCE_COPY_DATA -> OBJECT_COPY)
 			
			// Primary key
			entityObject.setSystem(di.systemInput);   						 
			entityObject.setSubSystem(sourceInput.getSubSystemOwner());  
			entityObject.setSystemOwner(di.systemInput);
			entityObject.setSubSystemOwner(sourceInput.getSubSystemOwner());
			entityObject.setIdObject(sourceInput.getIdSource()); 
			entityObject.setTypeObject(sourceInput.getSourceType().getObjectType());				 
			entityObject.setTypeSource(sourceInput.getSourceType());	
			 
			// Solo se Tipologia sorgente  definita, sorgente identificabile e 
			// tipologia richiesta in pilot di esecuzione
			isObjectFound = eoDAO.read(entityObject);
            			
			// Update info source
			entityObject.setFileSource(sourceInput.getIdSource());
			entityObject.setTypeSource(sourceInput.getSourceType());	
			entityObject.setFileSource(sourceInput.getIdSource());
			entityObject.setSuffixFileSource(sourceInput.getSourceSuffix());
			entityObject.setLibrarySourceObject(sourceInput.getLibraryCode());
			entityObject.setLibrarySource(sourceInput.getDirInput());
			entityObject.setLibraryDir(sourceInput.getDirInput());
			
			// Oggetto non era presente: è stato acquisito e dovrà essere analizzato
			if (isObjectFound == false) {
				// Dati in ogni caso da aggiornare
				entityObject.setStatus(EnumObjectStatus.OBJECT_TO_BE_ANALYZED);
				if (sourceInput.getSourceType() == EnumSourceType.NOT_ASSIGNED) {  
					entityObject.setStatus(EnumObjectStatus.SOURCE_MEMBER_TYPE_NOT_DETECTED);  
				}                                                                
                // Sys/subsys proprietario
				entityObject.setSystemOwner(di.systemInput);   						 
				entityObject.setSubSystemOwner(sourceInput.getSubSystemOwner());    
				// Inserimento in database: se duplicate aggiorno, potrebbe essere cambiata libreria
				eoDAO.create(entityObject);
	            
	        // Oggetto presente: se stato OBJECT_TO_BE_ACQUIRED aggiorno a OBJECT_TO_BE_ANALYZED
	        // Inoltre potrebbe essere cambiata la libreria del source
			} else {
				// Oggetto precedentemente NON classificato puo ora essere analizzato
				if (entityObject.getStatus() == EnumObjectStatus.OBJECT_TO_BE_ACQUIRED) {				
					entityObject.setStatus(EnumObjectStatus.OBJECT_TO_BE_ANALYZED);
				}
				eoDAO.update(entityObject);
			}		

            // Se opzione oggetto individuata (copy o jcl) Inserimento ObjectOption  
            en_objectOption = getObjectOption(sourceInput.getSourceType());
            if (en_objectOption != EnumObjectOption.NOT_ASSIGNED) {
            	entityObjectOption = new EntityObjectOption();
            	entityObjectOption.setSystem(di.systemInput);   						 
            	entityObjectOption.setSubSystem(sourceInput.getSubSystemOwner());    						 
            	entityObjectOption.setIdObject(sourceInput.getIdSource());    						 
            	entityObjectOption.setTypeObject(sourceInput.getSourceType().getObjectType());				 
            	entityObjectOption.setOption(en_objectOption);				             
            	eoDAOObjectOption.create(entityObjectOption);
			}
            
	 		/////////////////////////////////////////////////////////////////////////////
 	 		//(2.3) Inserimento oggetto SOURCE_MEMBER e relazione con libreria
  	 		///////////////////////////////////////////////////////////////////////////// 			
 			

            // Scrittura oggetto OBJECT_SOURCE_MEMBER

            // Primary key
            entityObject.setTypeObject(EnumObject.OBJECT_SOURCE_MEMBER);	
            entityObject.setTypeSource(EnumSourceType.NOT_ASSIGNED);
            
			// Recupero oggetto SOURCE_MEMBER se presente
			isSourceFound = eoDAO.read(entityObject);

			// Update info source (si riporta la libreria e la sua directory
			entityObject.setFileSource(sourceInput.getIdSource());
			entityObject.setTypeSource(sourceInput.getSourceType());	
			entityObject.setFileSource(sourceInput.getIdSource());
			entityObject.setSuffixFileSource(sourceInput.getSourceSuffix());
			entityObject.setLibrarySourceObject(sourceInput.getLibraryCode());
			entityObject.setLibrarySource(sourceInput.getDirInput());
			entityObject.setLibraryDir(sourceInput.getDirInput());
            
			// Update stato
			entityObject.setStatus(EnumObjectStatus.SOURCE_MEMBER_TYPE_DETECTED);
			if (entityObject.getTypeSource() == EnumSourceType.NOT_ASSIGNED) {
				entityObject.setStatus(EnumObjectStatus.SOURCE_MEMBER_TYPE_NOT_DETECTED);
			}	

			// SOURCE_MEMBER Non trovato: probabile prima libraryScan
			if (isSourceFound == false) {
				eoDAO.create(entityObject);				
			// SOURCE_MEMBER trovato: aggiorno 
			} else {
				eoDAO.update(entityObject);
			}
			            
 			// Se libreria assente il sorgente è stato estratto probabilmente con clausola FILE
 			// Relazione libreria source source member
 			if (!sourceInput.getLibraryCode().equals("")) {
	    		// Primary key
			    entityRelation.setSystem(di.systemInput);   						 
			    entityRelation.setSubSystem(sourceInput.getSubSystemOwner());  
			    entityRelation.setRelation(EnumRelation.LIBRARY_SOURCE_MEMBER);    	
			    entityRelation.setTypeObjectA(EnumObject.OBJECT_LIBRARY); 
			    entityRelation.setIdObjectA(sourceInput.getLibraryCode());
			    entityRelation.setTypeSourceA(EnumSourceType.NOT_ASSIGNED);
			    entityRelation.setTypeObjectB(sourceInput.getSourceType().getObjectType()); 
			    entityRelation.setIdObjectB(sourceInput.getIdSource());  						 
			    entityRelation.setTypeSourceB(sourceInput.getSourceType() );
			    
			    eoDAORelation.create(entityRelation);
			}
            
            conn.commit();
  		}

 		conn.commit();    // Consolido gli ultimi aggiornamenti 
 
 		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		eoDAOObjectOption.setConn(null);
		eoDAORelation.setConn(null);
 
	}
	
	

	
	/*
	 * 
	 *  In base al tipo di sorgente individuato, restituisce la corretta opzione da inserire.
	 *  Tipicamente ciò è valido per i copy, che sono qualificati dalle opzioni.
	 * 
	 */
    private EnumObjectOption getObjectOption(EnumSourceType en_sourceType) {

    	
        switch (en_sourceType) {
					case COBOL_COPY_DATA:
						return EnumObjectOption.COPY_COBOL_OF_DATA_DIVISION;
					case COBOL_COPY_PROC:
						return EnumObjectOption.COPY_COBOL_OF_PROC_DIVISION;
					case JCL_MVS_JOB:
						return EnumObjectOption.JCL_MVS;
					case JCL_MVS_PROC:
						return EnumObjectOption.JCL_MVS;
					case JCL_MVS_INCLUDE:
						return EnumObjectOption.JCL_MVS;
					default:
						return EnumObjectOption.NOT_ASSIGNED;
        }
	}


	/*
     * 
     * Inserisce le opzioni di libreria indicanti la presenza dei tipi sorgente
     * 
     */
	private void insertObjectOptionLibrary(IDAOObjectOption eoDAOObjectOption
			                              ,EntityObjectOption entityObjectOption
			                              ,InnerLibrary lib
										  ) throws ExceptionAmrita, SQLException {

		// COBOL_PROGRAM
		if (lib.cobolProgram) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_COBOL_PROGRAM);	  
			eoDAOObjectOption.create(entityObjectOption);
		}
		// COBOL_COPY_DATA
		if (lib.cobolCopyData) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_COBOL_COPY_DATA);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// COBOL_COPY_PROC
		if (lib.cobolCopyProc) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_COBOL_COPY_PROC);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// PL1_COPY
		if (lib.pl1Copy) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_PL1_COPY);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// PL1_PROGRAM
		if (lib.pl1Program) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_PL1_PROGRAM);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// ASM_COPY
		if (lib.asmCopy) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_ASM_COPY);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// ASM_PROGRAM
		if (lib.asmProgram) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_ASM_PROGRAM);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// JAVA_PROGRAM
		if (lib.javaProgram) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_JAVA_PROGRAM);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// CICS_BMS
		if (lib.cicsBms) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_CICS_BMS);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// JCL_MVS
		if (lib.jclMvs) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_JCL_MVS_JOB);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// JCL_MVS_INCLUDE
		if (lib.jclMvsInclude) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_JCL_MVS_INCLUDE);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// JCL_MVS_PROC
		if (lib.jclMvsProc) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_JCL_MVS_PROC);
			eoDAOObjectOption.create(entityObjectOption);
		}
		// DDL_SQL
		if (lib.ddlSql) {
			entityObjectOption.setOption(EnumObjectOption.LIBRARY_WITH_SCRIPT_SQL);
			eoDAOObjectOption.create(entityObjectOption);
		}
		
		return ;
	}


	/*
     * 
     * Imposta a true il flag del tipo sorgente appropriato per la libreria
     * 
     */
	private void setTypeSourcesDetected(EnumSourceType en_sourceType, InnerLibrary lib) {

        switch (en_sourceType) {
				case COBOL_PROGRAM:
					lib.cobolProgram = true;
					break;
				case COBOL_COPY_DATA:
					lib.cobolCopyData = true;
					break;
				case COBOL_COPY_PROC:
					lib.cobolCopyProc = true;
					break;
				case CICS_BMS:
					lib.cicsBms = true;
					break;
				case JCL_MVS_JOB:
					lib.jclMvs = true;
					break;
				case JCL_MVS_PROC:
					lib.jclMvsProc = true;
					break;
				case JCL_MVS_INCLUDE:
					lib.jclMvsInclude = true;
					break;
				case SQL_SCRIPT:
					lib.ddlSql = true;
					break;
        }
	}

	
	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	
	/*
	 *   Classe contenitore di servizio con tutte le opzioni di libreria
	 */
	private class InnerLibrary {
		
		@SuppressWarnings("unused")
		String librarySourceLogicName = "";     // Nome logico nella direttiva di processo
		String librarySourcePath = "";          // Path completo libreria
 
	    // Impostati a true in presenza di sorgenti di questo tipo individuati
	    boolean cobolProgram = false;
	    boolean cobolCopyProc = false;
	    boolean cobolCopyData = false;
	    boolean pl1Copy = false;
	    boolean pl1Program = false;
	    boolean asmCopy = false;
	    boolean asmProgram = false;
	    boolean javaProgram = false;
	    boolean ddlSql = false;
	    boolean jclMvs = false;
	    boolean jclMvsProc = false;
	    boolean jclMvsInclude = false;
	    boolean cicsBms = false;
	}
	
}
