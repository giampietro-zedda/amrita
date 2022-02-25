package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumDirectivesExecution;
import enums.EnumObject;
import enums.EnumProcessStatus;

/**
	 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityProcessLog (ProcessLog, PLOG) 
	 * </h1>
	 *  <p>
	 * Questa classe descrive l'entity ProcessLog, ovvero la tabella di log ProcessLog.
	 * Ogni processo o funzione o attività può essere monitorato attraverso questa tabella.
	 * Vengono inserite informazioni codificate all'inizio e alla fine di ogni processo
	 * o funzione oltre a informazioni periodiche sullo stato di avanzamento.
	 * La frequenza di aggiornamento di questa tabella viene impostata di default nel
	 * file di configurazione e può essere altrimenti specificata nelle direttive di processo.
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0ù	
	 * @since 12/03/2010
	 * @see Analyzer
	 * @see EntityObject 
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

@Entity(name="ProcessLog")
public class EntityProcessLog {

	///////////////////////////////////////////////////////////////////////
    // Data Items ProcessLog                                             //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="user")
	private String user = "";            				// User logged in
	@Id
    @Column(name="sys")
	private String system = "";            				// Sistema applicativo
	@Id
    @Column(name="subSys")
	private String subSystem = "";         				// Sotto sistema applicativo
	@Id
    @Column(name="typeProcess")
	private EnumDirectivesExecution typeProcess = null; // Tipo processo (T0033)
	@Id
    @Column(name="idObject")
	private String idObject  = "";		                // Nome oggetto in errore
	@Id
    @Column(name="typeObject")
	private EnumObject typeObject  = null;		        // Tipo oggetto in errore (T0001)
	@Id
    @Column(name="dtStartProcess")
	private String dtStartProcess = "";                 // Data start processo (AAAAMMGG)
	@Id
    @Column(name="tmStartProcess")
	private String tmStartProcess = "";                 // Ora start processo (HHMMSSCC)

 	
	// Data
    @Column(name="tmEndProcess")
	private String tmEndProcess = "";                   // Ora Fine processo (HHMMSSCC)
    @Column(name="msDuration")
	private int msDuration = 0;                   		// Durata in millisecondi
    @Column(name="statusProcess")
	private EnumProcessStatus statusProcess = null;     // Stato processo (T0013)
    @Column(name="msgError")
	private String msgError  = "";		                // Messaggio errore impostato dal processo
    @Column(name="idExcpError")
	private String idExcpError  = "";		            // Exception intercettata a fronte di errore
    @Column(name="excpStackTrace")
	private String excpStackTrace = "";                 // Stack trace al momento dell'exception
    @Column(name="threadNameError")
	private String threadNameError  = "";		        // Nome thread in errore
    @Column(name="javaClassError")
	private String javaClassError  = "";		        // Classe java in errore

	
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityProcessLog() {
		super();
		typeProcess = EnumDirectivesExecution.NOT_ASSIGNED;
		statusProcess = EnumProcessStatus.NOT_ASSIGNED;
		typeObject = EnumObject.NOT_ASSIGNED;
		
	}


	/**
	 * @return the system
	 */
	public String getSystem() {
		return system;
	}


	/**
	 * @param system the system to set
	 */
	public void setSystem(String system) {
		this.system = system;
	}


	/**
	 * @return the subSystem
	 */
	public String getSubSystem() {
		return subSystem;
	}


	/**
	 * @param subSystem the subSystem to set
	 */
	public void setSubSystem(String subSystem) {
		this.subSystem = subSystem;
	}


	/**
	 * @return the typeProcess
	 */
	public EnumDirectivesExecution getTypeProcess() {
		return typeProcess;
	}


	/**
	 * @param typeProcess the typeProcess to set
	 */
	public void setTypeProcess(EnumDirectivesExecution typeProcess) {
		this.typeProcess = typeProcess;
	}


	/**
	 * @return the dtStartProcess
	 */
	public String getDtStartProcess() {
		return dtStartProcess;
	}


	/**
	 * @param dtStartProcess the dtStartProcess to set
	 */
	public void setDtStartProcess(String dtStartProcess) {
		this.dtStartProcess = dtStartProcess;
	}


	/**
	 * @return the tmStartProcess
	 */
	public String getTmStartProcess() {
		return tmStartProcess;
	}


	/**
	 * @param tmStartProcess the tmStartProcess to set
	 */
	public void setTmStartProcess(String tmStartProcess) {
		this.tmStartProcess = tmStartProcess;
	}


	/**
	 * @return the statusProcess
	 */
	public EnumProcessStatus getStatusProcess() {
		return statusProcess;
	}


	/**
	 * @param statusProcess the statusProcess to set
	 */
	public void setStatusProcess(EnumProcessStatus statusProcess) {
		this.statusProcess = statusProcess;
	}




	/**
	 * @param typeObjectError the typeObjectError to set
	 */
	public void setTypeObject(EnumObject typeObject) {
		this.typeObject = typeObject;
	}


	/**
	 * @return the msgError
	 */
	public String getMsgError() {
		return msgError;
	}


	/**
	 * @param msgError the msgError to set
	 */
	public void setMsgError(String msgError) {
		this.msgError = msgError;
	}


	/**
	 * @return the idExcpError
	 */
	public String getIdExcpError() {
		return idExcpError;
	}


	/**
	 * @param idExcpError the idExcpError to set
	 */
	public void setIdExcpError(String idExcpError) {
		this.idExcpError = idExcpError;
	}


	/**
	 * @return the excpStackTrace
	 */
	public String getExcpStackTrace() {
		return excpStackTrace;
	}


	/**
	 * @param excpStackTrace the excpStackTrace to set
	 */
	public void setExcpStackTrace(String excpStackTrace) {
		this.excpStackTrace = excpStackTrace;
	}


	/**
	 * @return the threadNameError
	 */
	public String getThreadNameError() {
		return threadNameError;
	}


	/**
	 * @param threadNameError the threadNameError to set
	 */
	public void setThreadNameError(String threadNameError) {
		this.threadNameError = threadNameError;
	}


	/**
	 * @return the javaClassError
	 */
	public String getJavaClassError() {
		return javaClassError;
	}


	/**
	 * @param javaClassError the javaClassError to set
	 */
	public void setJavaClassError(String javaClassError) {
		this.javaClassError = javaClassError;
	}



	/**
	 * @return the idObject
	 */
	public String getIdObject() {
		return idObject;
	}


	/**
	 * @param idObject the idObject to set
	 */
	public void setIdObject(String idObject) {
		this.idObject = idObject;
	}


	/**
	 * @return the typeTypeObject
	 */
	public EnumObject getTypeObject() {
		return typeObject;
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
	 * @return the tmEndProcess
	 */
	public String getTmEndProcess() {
		return tmEndProcess;
	}


	/**
	 * @param tmEndProcess the tmEndProcess to set
	 */
	public void setTmEndProcess(String tmEndProcess) {
		this.tmEndProcess = tmEndProcess;
	}


	/**
	 * @return the msDuration
	 */
	public int getMsDuration() {
		return msDuration;
	}


	/**
	 * @param msDuration the msDuration to set
	 */
	public void setMsDuration(int msDuration) {
		this.msDuration = msDuration;
	}

	
}
