package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumCobolReservedWords;

/**
 * 
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
 * 
 * <h1>
 * InstructionCobol
 * </h1>
 *  <p>
 * Questa classe modella uno specifico statement sorgente del linguaggio Cobol. <br>
 * Vengono gestite eventuali informazioni aggiuntive per sorgenti Cobol.<br>
 * <p>
 * Sono inoltre presenti tutti i metodi specifici di caricamento e recupero
 * informazioni per istruzioni comuni alle varie divisioni Cobol come, per
 * esempio, copy statement.<br>
 * <p>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/02/2010
 * @see Instruction
 * 
 * 
*/
public class InstructionCobol extends Instruction implements Cloneable, Serializable {

	private static final long serialVersionUID = 1L;

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza  specifiche                                          						  //                                                        //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

	private EnumCobolReservedWords typeInstr = null;   // Tipo istruzione codificata
	private String sysOwner = "";                      // Sstema proprietario, valido per COPY 
	private String subSysOwner = "";                   // Sotto Sstema proprietario, valido per COPY 
	private boolean isCopySqlInclude = false;		   // Solo se DIR_COMPILER_COPY e a fronte di SQL INCLUDE 

	
	/**  
	 * Costruttore senza parametri
	 *  
	 */
	public InstructionCobol()  {
		super();
		typeInstr = EnumCobolReservedWords.NOT_ASSIGNED;
	}
	
	/**  
	 * Costruttore utilizzato per istanziare un oggetto Instruction
	 * con tutti i parametri richiesti
	 *  
	 *  @param numInstr 				Numero sequenza istruzione 0-based
	 *  @param RowStartSource 			Numero riga sorgente di analisi di inizio
	 *  @param RowEndSource 			Numero riga sorgente di analisi di fine
	 *  @param PosStart 		    	Posizione inizio istruzione in riga sorgente
	 *  @param PosEnd    		    	Posizione fine istruzione in riga sorgente	
	 *  @param ar_RowsSource          	Array righe sorgente con l'istruzione
	 *  @param ar_RowsSourceComments  	Array righe sorgente con i commenti precedenti l'istruzione
	 *  
	 */
	public InstructionCobol(int numInstr
						   ,int rowStartSource
						   ,int rowEndSource
						   ,int posStartInstr
						   ,int posEndInstr
					 	   ,String ar_RowsSource[]
				           ,String ar_CommentsBeforeInstr[]
				           ,String ar_CommentsLeftInstr[]                        
				           ,String ar_CommentsRightInstr[]                        
				           ,String name
				           ,String sourceInstr 
			               ) {
		
		super(numInstr
		 	 ,rowStartSource
			 ,rowEndSource
			 ,posStartInstr
			 ,posEndInstr
			 ,ar_RowsSource
			 ,ar_CommentsBeforeInstr
			 ,ar_CommentsLeftInstr
			 ,ar_CommentsRightInstr
			 ,name
			 ,sourceInstr
			 );	
		
		typeInstr = EnumCobolReservedWords.NOT_ASSIGNED;
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
	 * 
	 * Restituisce il codice dell'istruzione
	 * 
	 * @return the typeInstr
	 */
	public EnumCobolReservedWords getTypeInstr() {
		return typeInstr;
	}

	/**
	 * @param typeInstr the typeInstr to set
	 */
	public void setTypeInstr(EnumCobolReservedWords typeInstr) {
		this.typeInstr = typeInstr;
	}

	/**
	 * Imposta il nome della libreria presente nello statement copy.<br>
	 * <p>
	 * 
     * @param String copyLibraryName
	 */
	public void  copySetLibrary(String copyLibraryName) {
		addMapDescriptorObject("$LIB$", copyLibraryName);
	}
		
	/**
	 * Imposta l'ArrayList di oggetti {@link InnerReplacingByEntry} 
	 * che descrivono i replacing nello statement copy.<br>
	 * <p>
	 * 
     * @param ArrayList<InstructionCobol.InnerReplacingByEntry> al_CopyReplacingByEntry
	 */
	public void  copySetReplacingBy(ArrayList<InnerReplacingByEntry> al_CopyReplacingByEntry) {
		addMapDescriptorObject("$REPLACING$", al_CopyReplacingByEntry);
	}
		
	/**
	 * Imposta il nome del copy definito dallo statement.<br>
	 * <p>
	 * 
     * @param String copyName
	 */
	public void  copySetName(String copyName) {
		addMapDescriptorObject("$COPY$", copyName);
	}
		
	/**
	 * Restituisce il numero di operatori distinti secondo Halsead
	 * codificati nell'istruzione.<br>
	 * <p>
	 * Viene restituito un array list di enumerazioni {@link EnumCobolReservedWords},
	 * che codificano qualsiasi tipo di token cobol, e che rappresentano degli
	 * operatori per la metrica di Halstead.<br>
	 * Si tratta di parentesi aperte e chiuse, operatori matematici
	 * e logici e simboli di separazione.<br>
	 * <p>
	 * L'operatore distinto principale, il codice dell'istruzione, non viene restituito.<br>
	 * Vengono restituiti gli operatori presenti nelle espressioni degli identificatori
	 * cobobol e delle condizioni in IF, When, Until etc.<br>
	 * <p>
	 * Questo metodo ha senso per qualsiasi istruzione Cobol di procedure division
	 * che può contenere identificatori e, pertanto, espressioni e/o operatori.
	 * <p>
	 * Nel caso non siano presenti operatori secondo Halstead viene restituito
	 * un ArrayList vuoto.<br>
	 * <p>
     * @return the array list with operators
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<EnumCobolReservedWords>  getHalsteadOperators() {
		ArrayList<EnumCobolReservedWords> al_HalseadOperators = null;
		
		al_HalseadOperators = (ArrayList<EnumCobolReservedWords>) getMapDescriptorObject("$HLST$OPRT");
		
		if (al_HalseadOperators == null) {
			al_HalseadOperators = new ArrayList<EnumCobolReservedWords>();
		}
		return al_HalseadOperators;
	}

	/**
	 * Imposta il numero di operatori distinti secondo Halsead
	 * codificati nell'istruzione.<br>
	 * <p>
	 * Viene impostato un array list di enumerazioni {@link EnumCobolReservedWords},
	 * che codificano qualsiasi tipo di token cobol, e che rappresentano degli
	 * operatori per la metrica di Halstead.<br>
	 * Si tratta di parentesi aperte e chiuse, operatori matematici
	 * e logici e simboli di separazione.<br>
	 * <p>
	 * L'operatore distinto principale, il codice dell'istruzione, non viene impostato.<br>
	 * Vengono impostati gli operatori presenti nelle espressioni degli identificatori
	 * cobobol e delle condizioni in IF, When, Until etc.<br>
	 * <p>
	 * Questo metodo ha senso per qualsiasi istruzione Cobol di procedure division
	 * che può contenere identificatori e, pertanto, espressioni e/o operatori.
	 * <p>
	 * <p>
     * @partam the array list with operators
	 */
	public void setHalsteadOperators(ArrayList<EnumCobolReservedWords> al_HalseadOperators) {
		
		al_HalseadOperators.trimToSize();   // Optimize
		this.addMapDescriptorObject("$HLST$OPRT", al_HalseadOperators);

		return;
	}

	/**
	 * Restituisce il numero di operatori estesi secondo McCabe
	 * codificati nell'istruzione.<br>
	 * <p>
	 * Viene restituito un intero con il numero di operatori logioci
	 * OR e AND trovati nell'istruzione.<br>
	 * Si tratta di operatori logici <tt>OR</tt> e <tt>AND</tt> presenti in istruzioni
	 * con condizioni quali IF, When, Perform Until. <br>
	 * <p>
	 * Nel caso non siano presenti operatori estesi secondo McCabe viene restituito 0.<br>
	 * <p>
     * @return the number of logical operators
	 */
	public int  getMcCabeOperatorsExtended() {
		Integer cntMcCabeOperatorsExtended = 0;
		
		cntMcCabeOperatorsExtended = (Integer) getMapDescriptorObject("$MCCABE$OPRT");
		
		if (cntMcCabeOperatorsExtended == null) {
			return 0;
		}
		return cntMcCabeOperatorsExtended;
	}

	/**
	 * Imposta il numero di operatori estesi secondo McCabe
	 * codificati nell'istruzione.<br>
	 * <p>
	 * Viene restituito un intero con il numero di operatori logioci
	 * OR e AND trovati nell'istruzione.<br>
	 * Si tratta di operatori logici <tt>OR</tt> e <tt>AND</tt> presenti in istruzioni
	 * con condizioni quali IF, When, Perform Until. <br>
	 * <p>
	 * Nel caso non siano presenti operatori estesi secondo McCabe viene restituito 0.<br>
	 * <p>
     * @return the number of logical operators
	 */
	public void  setMcCabeOperatorsExtended(int cntMcCabeOperatorsExtended) {
		
		addMapDescriptorObject("$MCCABE$OPRT", new Integer(cntMcCabeOperatorsExtended));
		return;
	}

	
	/**
	 * Restituisce il nome della libreria presente nello statement copy.<br>
	 * <p>
	 * 
     * @param String copyLibraryName
	 */
	public String  copyGetLibrary() {
		String copyLibraryName = "";
		if (getMapDescriptorObject("$LIB$") == null) {
			return null;
		}
		copyLibraryName = (String) getMapDescriptorObject("$LIB$");
		return copyLibraryName;
	}
		
	/**
	 * Restituisce il nome del modulo copy definito dallo statement.<br>
	 * <p>
	 * 
     * @param String copyLibraryName
	 */
	public String  copyGetName() {
		String copyCopyName = "";
		if (getMapDescriptorObject("$COPY$") == null) {
			return null;
		}
		copyCopyName = (String) getMapDescriptorObject("$COPY$");
		return copyCopyName;
	}
		
	/**
	 * Restituisce l'ArrayList di oggetti {@link InnerReplacingByEntry} 
	 * che descrivono i replacing nello statement copy.<br>
	 * <p>
	 * 
     * @param ArrayList<InstructionCobol.InnerReplacingByEntry> al_CopyReplacingByEntry
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<InstructionCobol.InnerReplacingByEntry>  copyGetReplacingBy() {
		ArrayList<InnerReplacingByEntry> al_CopyReplacingByEntry;
		if (getMapDescriptorObject("$REPLACING$") == null) {
			return null;
		}
		
		al_CopyReplacingByEntry = (ArrayList<InstructionCobol.InnerReplacingByEntry>) getMapDescriptorObject("$REPLACING$");
		return al_CopyReplacingByEntry;
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
	

	/*
	 *   Classe contenitore di servizio con la descrizione di una copia di elementi di uno
	 *   statement copy. Viene memorizzato il valore da cercare e il valore da rimpiazzare.
	 *   
	 */
	public class InnerReplacingByEntry implements Serializable {
		
		private static final long serialVersionUID = 1L;

		String valueToFind = "";                        // Valore da cercare nel sorgente del copy
		String valueToReplace = "";                     // Valore da rimpiazzare (tipicamente un data name)
	}


}
