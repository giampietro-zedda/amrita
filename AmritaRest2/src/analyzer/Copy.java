package analyzer;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import enums.EnumAmritaExceptionError;
import enums.EnumMessageType;
import enums.EnumObject;
import exception.ExceptionAmrita;

/**
  * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda Turin (ITALY)
  * 
 * <h1>
 * Copy 
 * </h1>
 *  <p>
 * MOD Questa classe  modella tutti gli aspetti di un generico copy e/o Sql include, modellando
 * le caratteristiche comuni a tutti i linguaggi. Un modulo copy è composto da insiemi di oggetti
 * {@link Instruction}, specifici per il linguaggio, come {@link InstructionCobolDataItem} etc.<br>
 * Dal momento che i copy vengono attualizzati, come insiemi di normali istruzioni, nell'analisi del programma sorgente, <br>
 * è possibile che alcuni simboli non siano risolti e debbano essere risolti solo nell'ambito del programma attualizzato.<br>
 * Pertanto questa classe e le sue sottoclassi specializzate gestiscono i simboli non risolti, come una Section Cobol
 * non definita richiamata da una Perform, oppure una Redefines Cobol dove non è definito il campo da ridefinire.<br>
 * Viene inoltre gestita la ricorsività degli oggetti copy a qualsiasi livello di annidamento. <br>
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 12/04/2010
 * @see DataItem
 * @see Analyzer
 *   
 */

public class Copy implements Serializable,  AmritaConstants {

	private static final long serialVersionUID = 1L;

	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza di reference a servizi centralizzati e a oggetti condivisi                    //                                                        //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    
	protected transient UserConfiguration ucfg = null;          	// Defaults e references globali come gestore messaggi, log etc	
	protected transient MessagesManager mm = null;         	// Gestore messaggi
	protected transient LoggerFacade lf = null;            	// Gestore logging

    
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza  specifiche                                                                             //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected String sysOwner = "";                          // Sistema proprietario dove il copy è stato analizzato
    protected String subSysOwner = "";                       // SottoSistema proprietario dove il copy è stato analizzato
	protected String path = null;  							 // Path completo source copy su file system          
	protected String dir = null;  							 // Directory source copy su file system          
	protected String copyName = "";                          // Nome del modulo copy così come richiamato nei programmi
	protected EnumObject copyType = null;           		 // OBJECT_COPY_COBOL, PROC, PL1, ....
	protected boolean copyWithAnyErrorDetected = false;      // True indica copy analizzato e memorizzato, con errori di parsing/semantici
	
	
	
	/**
	 * 
	 * Costruttore
	 * 
	 */
	public Copy(UserConfiguration sd, String copyName) {
		
		// Defaults reference ai servizi condivisi
		this.ucfg = sd;
		this.mm = sd.getMessagesManager();
		this.lf = sd.getLoggerFacade();
		
		// Nome modulo copy
		this.copyName = copyName;
		
		// Inizializzazione variabili di istanza
		copyType = EnumObject.NOT_ASSIGNED;
	}

	/**
	 * 
	 * Costruttore
	 * 
	 */
	public Copy(UserConfiguration sd, String copyName, EnumObject copyType) {
		
		// Defaults reference ai servizi condivisi
		this.ucfg = sd;
		this.mm = sd.getMessagesManager();
		this.lf = sd.getLoggerFacade();
		
		// Nome modulo copy
		this.copyName = copyName;
		
		// Inizializzazione variabili di istanza
		this.copyType = copyType;
	}
	
	
	 public String getSysOwner() {
		return sysOwner;
	}

	public void setSysOwner(String sysOwner) {
		this.sysOwner = sysOwner;
	}

	public String getSubSysOwner() {
		return subSysOwner;
	}

	public void setSubSysOwner(String subSysOwner) {
		this.subSysOwner = subSysOwner;
	}

	/**
	 * @return the copyName
	 */
	public String getCopyName() {
		return copyName;
	}



	/**
	 * @param copyName the copyName to set
	 */
	public void setCopyName(String copyName) {
		this.copyName = copyName;
	}



	/**
	 * @return the copyType
	 */
	public EnumObject getCopyType() {
		return copyType;
	}



	/**
	 * @param copyType the copyType to set
	 */
	public void setCopyType(EnumObject copyType) {
		this.copyType = copyType;
	}

	/**
	 * Restituisce il path completo del copy.<br>
	 * <p>
	 * @return the path
	 */
	public String getPath() {
		return path;
	}


	/**
	 * Imposta il path completo del copy.<br>
	 * <p>
	 * @param path the path to set
	 */
	public void setPath(String path) {
		this.path = path;
	}

	

	/**
	 * @return the directory del copy
	 */
	public String getDir() {
		return dir;
	}

	/**
	 * Imposta la directory del copy
	 * 
	 * @param dir the dir to set
	 */
	public void setDir(String dir) {
		this.dir = dir;
	}

	/**
	 * 
	 * Il copy viene memorizzato su disco in formato serializzato.
	 * Vengono forniti come parametri gli elementi per comporre 
	 * il nome del file di output, a cura del chiamante.
	 *  
	 * 
	 * @throws ExceptionAmrita 
	 */
	public void  serialize() throws ExceptionAmrita {
		
		ExceptionAmrita excp = null;
		ObjectOutputStream oos = null;
		String fileName = null;
		String strMsg[] = null;
		
		fileName = ucfg.getDirCobolObjCopy().trim() + File.separator + this.copyName + "." + SUFFIX_SERIALIZED_COPY;
		
		try {
			// Open del file
			oos = new ObjectOutputStream(new FileOutputStream(fileName));
			// Scrittura su file oggetto graph corrente
			oos.writeObject (this);
			// Chiususa file
			oos.close ();
 		} catch (Exception e) {
        	strMsg = new String[2];
        	strMsg[0] = this.copyName;
        	strMsg[1] = fileName;
			lf.writeRow(EnumMessageType.ERROR_INTERNAL, "EI0013", strMsg, e);
			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_SERIALIZE_COPY);
			// Il main gestisce l'eccezione, fornisce info supplementari e continua l'elaborazione
            throw excp;
		}	
 		
 		// Serializzazione completata: log messaggio informativo
 		strMsg = new String[2];
    	strMsg[0] = this.copyName;
    	strMsg[1] = fileName;
		lf.writeRow(EnumMessageType.INFORMATION, "MI0103", strMsg, null);
		return;
	}
	
	
	
	/**
	 * 
	 * Viene restituito l'oggetto Copy popolato a partire dall'ouput serializzato
	 * memorizzata su disco. Deve essere effettuato il casting per il tipo di copy
	 * specifico.
	 * 
	 * @throws ExceptionAmrita 
	 */
	public Object _unSerialize() throws ExceptionAmrita {
		
		Object copyUnserialized = null;
        
		ExceptionAmrita excp = null;
		ObjectInputStream ois = null;
		String fileName = null;
		String strMsg[] = null;
		
		fileName = ucfg.getDirCobolObjCopy().trim() + File.separator + this.copyName + "." + SUFFIX_SERIALIZED_COPY;

		try {
			// Open del file
			ois = new ObjectInputStream(new FileInputStream(fileName));
//			copyUnserialized = (GraphManager)ois.readObject();    // TODO ???????????????????????
			ois.close ();
 		} catch (Exception e) {
        	strMsg = new String[1];
         	strMsg[0] = fileName;
			lf.writeRow(EnumMessageType.ERROR_INTERNAL, "EI0014", strMsg, e);
			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_UNSERIALIZE_COPY);
			// Il main gestisce l'eccezione, fornisce info supplementari e continua l'elaborazione
            throw excp;
		}

		// De-serializzazione completata: log messaggio informativo
 		strMsg = new String[2];
    	strMsg[0] = this.copyName;
    	strMsg[1] = fileName;
		lf.writeRow(EnumMessageType.INFORMATION, "MI0104", strMsg, null);
		
		return copyUnserialized;
	}



	/**
	 * 
	 * Indica se il copy ha qualche errore di parsing o semantico.
	 * 
	 * @return the copyWithAnyErrorDetected
	 */
	public boolean isCopyWithAnyErrorDetected() {
		return copyWithAnyErrorDetected;
	}



	/**
	 * 
	 * Imposta se il copy ha qualche errore di parsing o semantico.
	 * 
	 * @param copyWithAnyErrorDetected the copyWithAnyErrorDetected to set
	 */
	public void setCopyWithAnyErrorDetected(boolean copyWithAnyErrorDetected) {
		this.copyWithAnyErrorDetected = copyWithAnyErrorDetected;
	}

}
