package analyzer;
import java.io.Serializable;
import java.util.ArrayList;
import enums.EnumObject;
import enums.EnumObjectOption;

/**
  * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda  Turin (ITALY)
  * 
 * <h1>
 * CopyCobol 
 * </h1>
 *  <p>
 * Questa classe  modella un modulo copy relativo al linguaggio Cobol. <br>
 * <p>
 * Il modulo copy può contenere altri moduli copy senza limiti di ricorsività. In questa classe<br>
 * vengono memorizzati in chiaro tutti i data item dei copy richiamati, esplosi, ricorsivamente.<br>
 * Ogni singolo copy richiamato ricorsivamente verrà descritto a sua volta da un'apposita istanza di questa classe. Il<br>
 * modulo copy foglia verrà descritto con i soli campi di cui fa parte.<br>
 * Un modulo copy può contenere tutte le <b>tipologie di istruzioni</b> che si possono tovare in un programma e ogni<br>
 * possibile istruzione viene collocata in un oggetto della classe {@link ProgramCobolEntry}<br>
 * Un oggetto {@link ProgramCobolEntry} di un copy può essere uno statement Copy che richiama esplicitamente un altro Copy. <br>
 * In questo caso sono memorizzate tutte le informazioni eventuali di Replacing By per includere correttamente il modulo copy.<br>
 * Dal momento che il copy verrà istanziato  nei sorgenti che lo richiamano, nomi di campi specificati nel replacing By<br>
 * oppure nomi di campi unresolved nel singolo copy, possono essere risolti solo al momento dell'inclusione nel<br>
 * sorgente che li utilizza. Pertanto i simboli e i loro utilizzi vengono gestiti solo a livello generale di programma<br>
 * <p>
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 13/04/2010
 * @see DataItem
 * @see Copy
 * @see Analyzer
 * @see ProgramCobolEntry   
 */

public class CopyCobol extends Copy implements Serializable {

	private static final long serialVersionUID = 1L;

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza                                                                               //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    private transient SourceInput sourceInput = null;  					// Descrittore sorgente copy 
    private EnumObjectOption copyTypeCobol = null;      				// Tipologia di copy (Obsoleto)
    private boolean isCopyNested = false;								// Copy nested, contenente copy statements
    private boolean isCopySqlInclude = false;							// Il copy è a fronte di SQL INCLUDE 
    
	// Struttura principale di definizine dei data item componenti il copy.
	// Ogni definizione descrive il tipo di entry nella sequenza con cui è presente nel modulo copy.
	private ArrayList<ProgramCobolEntry<? extends Instruction>> al_CopyEntry = null;   		// Definizioni del modulo copy
	
	
    
	 /**
	 *  Costruttore 1
	 */
	public CopyCobol(UserConfiguration sd) {
		super(sd, "");
		
		// Allocazione strutture
		al_CopyEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();   		
	}

	
    /**
	 *  Costruttore 2
	 */
	public CopyCobol(UserConfiguration sd, String copyName, EnumObject copyType ) {
		super(sd, copyName, copyType);

        if (copyType == EnumObject.OBJECT_COPY_COBOL_PROC ) {
    		this.copyTypeCobol = EnumObjectOption.COPY_COBOL_OF_PROC_DIVISION;
		} else if (copyType == EnumObject.OBJECT_COPY_COBOL_DATA) {
			this.copyTypeCobol = EnumObjectOption.COPY_COBOL_OF_DATA_DIVISION;
		} else if (copyType == EnumObject.OBJECT_COPY_COBOL_ID) {
			this.copyTypeCobol = EnumObjectOption.COPY_COBOL_OF_ID_DIVISION;
		} else {
			this.copyType = EnumObject.OBJECT_COPY_COBOL_ENV;
		}
		// Allocazione strutture
		al_CopyEntry = new ArrayList<ProgramCobolEntry<? extends Instruction>> ();   		

	}
 	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Metodi pubblici                                                                                     //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Metodi getter/setter                                                                               //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

	
	   public String getCopyName() {
			return copyName;
		}


		public void setCopyName(String copyName) {
			this.copyName = copyName;
		}

	
	
	/**
	 * 
	 * Restituisce il tipo di copy come {@link EnumObjectOption}
	 * 
	 * @return the copyTypeCobol
	 */
	public EnumObjectOption getCopyTypeCobol() {
		return copyTypeCobol;
	}






	/**
	 * Restituisce il descrittore completo del sorgente copy<br>
	 * <p>
	 * @return the sourceInput
	 */
	public SourceInput getSourceInput() {
		return sourceInput;
	}


	/**
	 * Imposta il descrittore completo del sorgente copy<br>
	 * <p>
	 * @param sourceInput the sourceInput to set
	 */
	public void setSourceInput(SourceInput sourceInput) {
		this.sourceInput = sourceInput;
	}



	/**
	 * 
	 * Imposta il tipo di copy come {@link EnumObjectOption}
	 * 
	 * @param copyTypeCobol the copyTypeCobol to set
	 */
	public void setCopyTypeCobol(EnumObjectOption copyTypeCobol) {
		this.copyTypeCobol = copyTypeCobol;
	}


	/**
	 * 
	 * Imposta l'ArrayList con tutti gli entry del modulo copy,
	 * oggetti {@link ProgramCobolEntry} 
	 * 
	 * @param ArrayList<ProgramCobolEntry> al_CopyEntry to set
	 */
	public void setEntries(ArrayList<ProgramCobolEntry<? extends Instruction>> al_CopyEntry) {
		this.al_CopyEntry = al_CopyEntry;
	}


	/**
	 * 
	 * Restituisce un array con tutte le definizini del modulo copy,
	 * oggetti {@link ProgramCobolEntry} 
	 * 
	 * @return ProgramCobolEntry[]  the ar_DataItem
	 */
	@SuppressWarnings("unchecked")
	public ProgramCobolEntry<? extends Instruction>[] getEntries() {
		ProgramCobolEntry<? extends Instruction> ar_CopyEntry[] = null;
		
		// Converto in array
		ar_CopyEntry = new ProgramCobolEntry[al_CopyEntry.size()];
		ar_CopyEntry = al_CopyEntry.toArray(ar_CopyEntry);
		
		return ar_CopyEntry;
	}



	/**
	 * 
	 * Restituisce lo specifico entry completo di definizione nel modulo copy,
	 * attraverso la sua posizione nel copy, 0-based.<br>
	 * Viene restituito un oggetto {@link ProgramCobolEntry} con tutte le informazioni
	 * sul tipo di definizione: data item, statement Copy, istruzioni per precompilatore etc.<br>
	 * Tali oggetti ereditano da {@link Instruction} e quindi contengono anche tutte le informazioni
	 * relative alle righe sorgente.
	 * 
	 * 
	 * @return ProgramCobolEntry entryDefined
	 */
	public ProgramCobolEntry<? extends Instruction> getEntry(int numEntry) {
		
		// Solo se indice valido
		if (numEntry > al_CopyEntry.size() - 1) {
			return null;
		}
		
		return al_CopyEntry.get(numEntry);
	}

	/**
	 * 
	 * Inserisce lo specifico entry completo di definizione nel modulo copy.
	 * Viene fornito un oggetto {@link ProgramCobolEntry} con tutte le informazioni
	 * sul tipo di definizione: data item, statement Copy, istruzioni per precompilatore etc.<br>
	 * Tali oggetti ereditano da {@link Instruction} e quindi contengono anche tutte le informazioni
	 * relative alle righe sorgente.
	 * 
	 * 
	 * @return boolean risultato di inserimento su collection
	 */
	public boolean addEntry(ProgramCobolEntry<? extends Instruction> copyEntry) {
		
		al_CopyEntry.add(copyEntry);
		
		return al_CopyEntry.add(copyEntry);
	}


	/**
	 * Restituisce se il copy è nested.<br>
	 * <p>
	 * @return the isCopyNested
	 */
	public boolean isCopyNested() {
		return isCopyNested;
	}


	/**
	 * Imnposta se il copy è nested.<br>
	 * <p>
	 * @param isCopyNested the isCopyNested to set
	 */
	public void setCopyNested(boolean isCopyNested) {
		this.isCopyNested = isCopyNested;
	}


	/**
	 * @return the isCopySqlInclude
	 */
	public boolean isCopySqlInclude() {
		return isCopySqlInclude;
	}


	/**
	 * @param isCopySqlInclude the isCopySqlInclude to set
	 */
	public void setCopySqlInclude(boolean isCopySqlInclude) {
		this.isCopySqlInclude = isCopySqlInclude;
	}



	
	
	
	
	
	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	

}
