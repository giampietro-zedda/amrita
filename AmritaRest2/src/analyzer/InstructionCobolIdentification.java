package analyzer;

import java.io.Serializable;

/**
 * 
 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
 * 
 * <h1>
 * InstructionCobolIdentification
 * </h1>
 *  <p>
 * Questa classe modella uno specifico statement sorgente del linguaggio Cobol di identification division. <br>
 * Vengono gestite eventuali informazioni aggiuntive per sorgenti Cobol.
 *  
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/02/2010
 * @see Instruction
 * 
 * 
*/
public class InstructionCobolIdentification extends InstructionCobol implements Serializable, Cloneable {

	private static final long serialVersionUID = 1L;

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza  specifiche                                          						  //                                                        //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

	
	/**  
	 * Costruttore senza parametri
	 *  
	 */
	public InstructionCobolIdentification()  {
		super();
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
	public InstructionCobolIdentification(int numInstr
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
		
	}


}
