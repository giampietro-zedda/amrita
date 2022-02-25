package analyzer;
import java.io.Serializable;
import enums.EnumSourceType;

/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda 2008   Turin (ITALY)
 * 
 * <h1>
 * SourceInput
 * </h1>
 *  <p>
 * Questa classe elenca tutte le caratteristiche di un sorgente da analizzare o
 * analizzato, come nome, path e tipologia.
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 15/02/2010
 * @see Analyzer
 * @see Source
 *   
 */

public class SourceInput implements Serializable, Comparable<SourceInput>, Cloneable{
	
	///////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	///////////////////////////////////////////////////////////////////////////////////
	
	private static final long serialVersionUID = 1L;

	// Info correnti di classificazione, sistema/sottosistema, filtro e opzioni  sorgenti.
	// Contiene tutte le informazioni attraverso le quali è stato individuato il sorgente
	// descritto da questa classe
	private ExecutionDirectives di = null;                   // Contiene sistema/sottosistema, libreria, filtri etc. correnti
	
	// Informazioni sorgente	
	private EnumSourceType enSourceType = null;	        // Tipo sorgente
	private String idSource = "";            	 		// Nome sorgente da trattare  (es. file1)
	private String sourceSuffix = "";        	 		// Eventuale suffisso, può ""  (es. txt)
	private String idSourceComplete = "";    	 		// Nome sorgente da trattare completo (es. file1.txt)
	private String dirInput = "";                		// Directory dove leggere il sorgente (può essere una subdirectory della libreria)
	private String libraryCode = "";                    // Codice libreria radice directory passata dalle direttive di esecuzione
	private String libraryPath = "";                    // Path libreria radice directory passata dalle direttive di esecuzione
	private String pathComplete = "";            		// Path completo sorgente
	private String systemOwner = "";                    // Sistema proprietario del sorgente identificato in scan library
	private String subSystemOwner = "";                 // Sottosistema proprietario del sorgente identificato in scan library
	private String dateLastModFormatted = "AAAA/MM/GG";	// Data creazione/ultima modifica formattata
	private long dateLastMod = 0;            	 		// Data creazione/ultima modifica
	private boolean isDirectory = false;         		// Data creazione
	private boolean isHidden = false;            		// File nascosto
	private boolean bExists = true;              		// File esistente
	private long size = 0;                	     		// Dimensioni in bytes
	private String arRowSource[] = null;         		// Array di stringhe con il sorgente 

	
	
	/**
	 * Costruttore
	 * 
	 * @param ExecutionDirectives di
	 */
	public SourceInput(ExecutionDirectives di) {
		super();
		this.di = di;
		enSourceType = EnumSourceType.NOT_ASSIGNED;
		
	}


	/**
	 * @return the DirectivesInfo di
	 */
	public ExecutionDirectives getDi() {
		return di;
	}


	/**
	 * @param di the di to set
	 */
	public void setDi(ExecutionDirectives di) {
		this.di = di;
	}


	/**
	 * @return the idSource
	 */
	public String getIdSource() {
		return idSource;
	}

	/**
	 * @param idSource the idSource to set
	 */
	public void setIdSource(String idSource) {
		this.idSource = idSource;
	}

	/**
	 * @return the prefixSource
	 */
	public String getSourceSuffix() {
		return sourceSuffix;
	}

	/**
	 * @param sourceSuffix the sourceSuffix to set
	 */
	public void setSourceSuffix(String sourceSuffix) {
		this.sourceSuffix = sourceSuffix;
	}

	/**
	 * @return the path
	 */
	public String getPathComplete() {
		return pathComplete;
	}

	/**
	 * @param path the path to set
	 */
	public void setPathComplete(String pathComplete) {
		this.pathComplete = pathComplete;
	}

	/**
	 * @return the systemOwner
	 */
	public String getSystemOwner() {
		return systemOwner;
	}


	/**
	 * @param systemOwner the systemOwner to set
	 */
	public void setSystemOwner(String systemOwner) {
		this.systemOwner = systemOwner;
	}


	/**
	 * @return the subSystemOwner
	 */
	public String getSubSystemOwner() {
		return subSystemOwner;
	}


	/**
	 * @param subSystemOwner the subSystemOwner to set
	 */
	public void setSubSystemOwner(String subSystemOwner) {
		this.subSystemOwner = subSystemOwner;
	}


	/**
	 * @return the sourceType
	 */
	public EnumSourceType getSourceType() {
		return enSourceType;
	}

	/**
	 * @param enSourceType the sourceType to set
	 */
	public void setSourceType(EnumSourceType sourceType) {
		this.enSourceType = sourceType;
	}

	/**
	 * @return the arRowSource
	 */
	public String[] getArrayRowSource() {
		return arRowSource;
	}

	/**
	 * @param arRowSource the arRowSource to set
	 */
	public void setArrayRowSource(String arRowSource[]) {
		this.arRowSource = arRowSource;
	}

	/**
	 * @return the size
	 */
	public long getSize() {
		return size;
	}

	/**
	 * @param size the size to set
	 */
	public void setSize(long size) {
		this.size = size;
	}

	/**
	 * @return the dateLastMod
	 */
	public long getDateLastMod() {
		return dateLastMod;
	}

	/**
	 * @param dateLastMod the dateLastMod to set
	 */
	public void setDateLastMod(long dateLastMod) {
		this.dateLastMod = dateLastMod;
	}

	/**
	 * @return the dateLastModFormatted
	 */
	public String getDateLastModFormatted() {
		return dateLastModFormatted;
	}

	/**
	 * @param dateLastModFormatted the dateLastModFormatted to set
	 */
	public void setDateLastModFormatted(String dateLastModFormatted) {
		this.dateLastModFormatted = dateLastModFormatted;
	}

	/**
	 * @return the isDirectory
	 */
	public boolean isDirectory() {
		return isDirectory;
	}

	/**
	 * @param isDirectory the isDirectory to set
	 */
	public void setDirectory(boolean isDirectory) {
		this.isDirectory = isDirectory;
	}

	/**
	 * @return the dirInput
	 */
	public String getDirInput() {
		return dirInput;
	}

	/**
	 * @param dirInput the dirInput to set
	 */
	public void setDirInput(String dirInput) {
		this.dirInput = dirInput;
	}

	
	/**
	 * @return the idSourceComplete
	 */
	public String getIdSourceComplete() {
		return idSourceComplete;
	}

	/**
	 * @param idSourceComplete the idSourceComplete to set
	 */
	public void setIdSourceComplete(String idSourceComplete) {
		this.idSourceComplete = idSourceComplete;
	}

	/**
	 * @return the isHidden
	 */
	public boolean isHidden() {
		return isHidden;
	}

	/**
	 * @return the bExists
	 */
	public boolean isExists() {
		return bExists;
	}

	/**
	 * @return the bExists
	 */
	public void setExists(boolean bExists) {
		this.bExists = bExists;
		return;
	}

	/**
	 * @param isHidden the isHidden to set
	 */
	public void setHidden(boolean isHidden) {
		this.isHidden = isHidden;
	}

	/**
	 * @return the libraryCode
	 */
	public String getLibraryCode() {
		return libraryCode;
	}


	/**
	 * @param libraryCode the libraryCode to set
	 */
	public void setLibraryCode(String libraryCode) {
		this.libraryCode = libraryCode;
	}


	/**
	 * @return the libraryPath
	 */
	public String getLibraryPath() {
		return libraryPath;
	}


	/**
	 * @param libraryPath the libraryPath to set
	 */
	public void setLibraryPath(String libraryPath) {
		this.libraryPath = libraryPath;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected Object clone() throws CloneNotSupportedException {
		return super.clone();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return idSource + " " + enSourceType.toString();
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		SourceInput srci = null;
		srci = (SourceInput) obj;		
		return this.idSourceComplete.equals(srci.idSourceComplete)
		   &&  this.sourceSuffix.equals(srci.sourceSuffix);
	}


	@Override
	public int compareTo(SourceInput o) {
		String idSourceComplete = ((SourceInput)o).getIdSourceComplete();
		return this.idSourceComplete.compareTo(idSourceComplete);
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return this.idSourceComplete.hashCode();
	}
	
	
}
