package analyzer;

import java.io.Serializable;

import enums.EnumCobolReservedWords;
import enums.EnumInstrDataCategory;
import enums.EnumPrecompilerReservedWords;


/**
 * 
 * Copyright (c) 2009-2011 e-Amrita - ing. Giampietro Zedda    Turin (ITALY)
 * 
 * <h1>
 * InstructionCics
 * </h1>
 *  <p>
 * Questa classe modella uno specifico statement sorgente di un precompilatore Cics. <br>
 * Si tratta du una generica Exec Cics. Questa classe
 * eredida da Instruction, che gestisce tutte le informazioni sorgente dell'istruzione stessa,
 * i commenti e il collegamento con il numero di riga e la posizione nel sorgente originale.
 *  
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/04/2010
 * @see Instruction
 * 
 * 
*/
public class InstructionCics extends Instruction implements Serializable, Cloneable {
	
	private static final long serialVersionUID = 1L;

		
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza                                             								  //                                                        //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private EnumInstrDataCategory typePrecompiler = null; 					// Tipo precompilatore
	private EnumCobolReservedWords typeInstr = null;            			// Tipo istruzione codificata
	private EnumPrecompilerReservedWords typeInstrPrecompiler = null;		// Codice istruzione
	

	/**  
	 * Costruttore vuoto
	 *  
	 */
	public InstructionCics() {
		super();	
		this.typePrecompiler = EnumInstrDataCategory.NOT_ASSIGNED; 
		this.typeInstr = EnumCobolReservedWords.NOT_ASSIGNED; 
		this.typeInstrPrecompiler = EnumPrecompilerReservedWords.NOT_ASSIGNED; 

		}

	
	/**  
	 * Costruttore utilizzato per istanziare un oggetto Instruction
	 * con tutti i parametri richiesti
	 *  
	 *  @param numInstr 				Numero sequenza istruzione 0-based
	 *  @param RowStartSource 			Numero riga sorgente di analisi di inizio
	 *  @param RowEndSource 			Numero riga sorgente di analisi di fine
	 *  @param PosStartInstruction 		Posizione inizio istruzione in riga sorgente
	 *  @param PosEndInstruction        Posizione fine istruzione in riga sorgente	
	 *  @param ar_RowsSource          	Array righe sorgente con l'istruzione
	 *  @param ar_RowsSourceComments  	Array righe sorgente con i commenti precedenti l'istruzione
	 *  @param name  					Nome istruzione
	 *  @param sourceInstruction  		Istruzione completa
	 *  
	 */
	public InstructionCics(EnumInstrDataCategory typePrecompiler
			                     ,int numInstr
								 ,int RowStartSource
								 ,int RowEndSource
								 ,int PosStartInstruction
								 ,int PosEndInstruction
							 	 ,String ar_RowsSource[]
						         ,String ar_CommentsBeforeInstr[]
						         ,String ar_CommentsLeftInstr[]                        
						         ,String ar_CommentsRightInstr[]                        
						         ,String Name
						         ,String sourceInstruction
					             ) {
		
		super(numInstr
		 	 ,RowStartSource
			 ,RowEndSource
			 ,PosStartInstruction
			 ,PosEndInstruction
			 ,ar_RowsSource
			 ,ar_CommentsBeforeInstr
			 ,ar_CommentsLeftInstr
			 ,ar_CommentsRightInstr
			 ,Name
			 ,sourceInstruction
			 );	
		
		this.typePrecompiler = EnumInstrDataCategory.NOT_ASSIGNED; 
		this.typeInstr = EnumCobolReservedWords.NOT_ASSIGNED; 
		this.typeInstrPrecompiler = EnumPrecompilerReservedWords.NOT_ASSIGNED; 

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
	 * @return the typeInstrPrecompiler
	 */
	public EnumPrecompilerReservedWords getTypeInstrPrecompiler() {
		return typeInstrPrecompiler;
	}


	/**
	 * @param typeInstrPrecompiler the typeInstrPrecompiler to set
	 */
	public void setTypeInstrPrecompiler(EnumPrecompilerReservedWords typeInstrPrecompiler) {
		this.typeInstrPrecompiler = typeInstrPrecompiler;
	}


	
	/**
	 * @return the typePrecompiler
	 */
	public EnumInstrDataCategory getTypePrecompiler() {
		return typePrecompiler;
	}


	/**
	 * @param typePrecompiler the typePrecompiler to set
	 */
	public void setTypePrecompiler(EnumInstrDataCategory typePrecompiler) {
		this.typePrecompiler = typePrecompiler;
	}


}
