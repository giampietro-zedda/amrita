package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * 
 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
 * 
 * <h1>
 * InstructionCobol
 * </h1>
 *  <p>
 * Questa classe modella uno specifico statement sorgente del linguaggio Cobol di data division. <br>
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
public class InstructionCobolDataStruct extends InstructionCobol implements Serializable, Cloneable {

	private static final long serialVersionUID = 1L;

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza  specifiche                                          						  //                                                        //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

	
	/**  
	 * Costruttore senza parametri
	 *  
	 */
	public InstructionCobolDataStruct()  {
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
	public InstructionCobolDataStruct(int numInstr
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


	/**
	 * Imposta se presente l'opzione <tt>BLOCK 0</tt> . <br>
	 * <p>
	 *  @param boolean isBlockZero
	 */
	public void fdSetBlockZero(boolean isExternal) {
		this.addMapDescriptorObject("$OPT$" + "BLOCK-ZERO", isExternal);
	}
	
	/**
	 * Restituisce se presente l'opzione <tt>BLOCK 0</tt> . <br>
	 * <p>
	 * @return boolean isBlockZero
	 */
	public boolean fdIsBlockZero() {
		Boolean isBlockZero = false;
		if (this.getMapDescriptorObject("$OPT$" + "BLOCK-ZERO") != null) {
			isBlockZero = (Boolean) this.getMapDescriptorObject("$OPT$" + "BLOCK-ZERO");
		} else {
			return false;
		}
		return isBlockZero;
	}


	/**
	 * Imposta se presente l'opzione <tt>EXTERNAL</tt> . <br>
	 * <p>
	 *  @param boolean isExternal
	 */
	public void fdSetExternal(boolean isExternal) {
		this.addMapDescriptorObject("$OPT$" + "EXTERNAL", isExternal);
	}
	
	/**
	 * Restituisce se presente l'opzione <tt>EXTERNAL</tt> . <br>
	 * <p>
	 * @return boolean isExternal
	 */
	public boolean fdIsExternal() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "EXTERNAL");
	}


	/**
	 * Imposta se presente l'opzione <tt>GLOBAL</tt> . <br>
	 * <p>
	 *  @param boolean isGlobal
	 */
	public void fdSetGlobal(boolean isGlobal) {
		this.addMapDescriptorObject("$OPT$" + "GLOBAL", isGlobal);
	}
	
	/**
	 * Restituisce se presente l'opzione <tt>GLOBAL</tt> . <br>
	 * <p>
	 * @return boolean isGlobal
	 */
	public boolean fdIsGlobal() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "GLOBAL");
	}

	/**
	 * Imposta se il blocco è espresso come dimensione in caratteri. <br>
	 * <p>
	 *  @param boolean isBlockCharacters
	 */
	public void fdSetBlockChar(boolean isBlockCharacters) {
		this.addMapDescriptorObject("$OPT$" + "BLOCK-CHAR", isBlockCharacters);
	}
	
	/**
	 * Restituisce se il blocco è espresso come dimensione in caratteri. <br>
	 * <p>
	 * @return boolean isBlockCharacters
	 */
	public boolean fdIsBlockChar() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "BLOCK-CHAR");
	}

	/**
	 * Imposta se il blocco è espresso come dimensione in records. <br>
	 * <p>
	 *  @param boolean isBlockRecords
	 */
	public void fdSetBlockRecords(boolean isBlockRecords) {
		this.addMapDescriptorObject("$OPT$" + "BLOCK-REC", isBlockRecords);
	}
	
	/**
	 * Restituisce se il blocco è espresso come dimensione in records. <br>
	 * <p>
	 * @return boolean isBlockRecords
	 */
	public boolean fdIsBlockRecords() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "BLOCK-REC");
	}


	
	/**
	 * Imposta il recording mode codificato nello statement Select. <br>
	 * <p>
	 * @param String recordingMode
	 */
	public void  fdSetRecordingMode(String recordingMode) {
		this.addMapDescriptorObject("$RECORDING-MODE$", recordingMode);
		return;
	}

	/**
	 * Restituisce il recording mode codificato nello statement Select. <br>
	 * <p>
	 * @return String recordingMode
	 */
	public String  fdGetRecordingMode() {
		return (String) getMapDescriptorObject("$RECORDING-MODE$");
	}


	/**
	 * restituisce il nome file interno codificato nello statement Select. <br>
	 * <p>
	 * @return String fileNameInternal
	 */
	public String  fdGetFileNameInternal() {
		return (String) getMapDescriptorObject("$FILE$");
	}

	/**
	 * Imposta il nome file interno codificato nello statement Select. <br>
	 * <p>
	 * @param String fileNameInternal
	 */
	public void  fdSetFileNameInternal(String fileNameInternal) {
		this.addMapDescriptorObject("$FILE$", fileNameInternal);
		return;
	}
	
	
	/**
	 * Imposta il nome del secondo campo di file status di uno statement Select. <br>
	 * <p>
	 * @param String fileStatus2
	 */
	public void  fdSetFileStatus2(String fileStatus2) {
		this.addMapDescriptorObject("$FILE-STATUS2$", fileStatus2);
		return;
	}
	
	/**
	 * Restituisce il nome del primo campo di file status di uno statement Select. <br>
	 * <p>
	 * @return String fileStatus1
	 */
	public String  fdGetFileStatus1() {
		return (String) getMapDescriptorObject("$FILE-STATUS1$");
	}


	/**
	 * Imposta il nome del primo campo di file status di uno statement Select. <br>
	 * <p>
	 * @param String fileStatus1
	 */
	public void  fdSetFileStatus1(String fileStatus1) {
		this.addMapDescriptorObject("$FILE-STATUS1$", fileStatus1);
		return;
	}
	
	/**
	 * Restituisce il nome del secondo campo di file status di uno statement Select. <br>
	 * <p>
	 * @return String fileStatus2
	 */
	public String  fdGetFileStatus2() {
		return (String) getMapDescriptorObject("$FILE-STATUS2$");
	}

	/**
	 * Imposta i DATA RECORDS codificati nello statement Select. <br>
	 * <p>
	 * @param String recordingMode
	 */
	public void  fdSetDataRecords(ArrayList<String> al_dataRecord) {
		this.addMapDescriptorObject("$DATA_REC$", al_dataRecord);
		return;
	}

	/**
	 * Restituisce i DATA RECORDS codificati nello statement Select. <br>
	 * <p>
	 * @return ArrayList<String> al_dataRecord
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String>  fdGetDataRecords() {
		return (ArrayList<String>) getMapDescriptorObject("$DATA_REC$");
	}

	/**
	 * Imposta il valore inferiore di VARYING FROM condificato nello statement Select. <br>
	 * <p>
	 * @param String recordBytesFrom
	 */
	public void  fdSetRecordBytesFrom(String recordBytesFrom) {
		this.addMapDescriptorObject("$REC-BYTE-FROM$", recordBytesFrom);
		return;
	}

	/**
	 * Restituisce il valore inferiore di VARYING FROM condificato nello statement Select. <br>
	 * <p>
	 * @return String recordBytesFrom
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String>  fdGetRecordBytesFrom() {
		return (ArrayList<String>) getMapDescriptorObject("$REC-BYTE-FROM$");
	}


	/**
	 * Imposta il valore superiore di VARYING ... TO condificato nello statement Select. <br>
	 * <p>
	 * @param String recordBytesTo
	 */
	public void  fdSetRecordBytesTo(String recordBytesTo) {
		this.addMapDescriptorObject("$REC-BYTE-TO$", recordBytesTo);
		return;
	}

	/**
	 * Restituisce il valore inferiore di VARYING ... TO condificato nello statement Select. <br>
	 * <p>
	 * @return String recordBytesTo
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String>  fdGetRecordBytesTo() {
		return (ArrayList<String>) getMapDescriptorObject("$REC-BYTE-TO$");
	}

	/**
	 * Imposta il valore superiore di VARYING ... DEPENDING ON codificato nello statement Select. <br>
	 * <p>
	 * @param String recordBytesDependingOn
	 */
	public void  fdSetRecordBytesDependingOn(String recordBytesDependingOn) {
		this.addMapDescriptorObject("$REC-BYTE-DEP-ON$", recordBytesDependingOn);
		return;
	}

	/**
	 * Restituisce il valore superiore di VARYING ... DEPENDING ON codificato nello statement Select. <br>
	 * <p>
	 * @return String recordBytesDependingOn
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String>  fdGetRecordBytesdependingOn() {
		return (ArrayList<String>) getMapDescriptorObject("$REC-BYTE-DEP-ON$");
	}

	/**
	 * Imposta il valore inferiore del numero di caratteri/records per blocco codificato nello statement Select. <br>
	 * <p>
	 * Si tratta anche della clausola <tt>BLOCK CONTAINS n RECORDS</tt>
	 * <p>
	 * @param String blockFrom
	 */
	public void  fdSetBlockFrom(String blockFrom) {
		this.addMapDescriptorObject("$BLOCK-FROM$", blockFrom);
		return;
	}

	/**
	 * Restituisce il valore inferiore del numero di caratteri/records per blocco codificato nello statement Select. <br>
	 * <p>
	 * Si tratta anche della clausola <tt>BLOCK CONTAINS n RECORDS</tt>
	 * <p>
	 * @return String blockFrom
	 */
	public String  fdGetBlockFrom() {
		return (String) getMapDescriptorObject("$BLOCK-FROM$");
	}

	/**
	 * Imposta il valore superiore del numero di caratteri/records per blocco codificato nello statement Select. <br>
	 * <p>
	 * Si tratta anche della clausola <tt>BLOCK CONTAINS n RECORDS</tt>
	 * <p>
	 * @param String blockTo
	 */
	public void  fdSetBlockTo(String blockTo) {
		this.addMapDescriptorObject("$BLOCK-FROM$", blockTo);
		return;
	}

	/**
	 * Restituisce il valore superiore del numero di caratteri/records per blocco codificato nello statement Select. <br>
	 * <p>
	 * Si tratta anche della clausola <tt>BLOCK CONTAINS n RECORDS</tt>
	 * <p>
	 * @return String blockTo
	 */
	public String  fdGetBlockTo() {
		return (String) getMapDescriptorObject("$BLOCK-TO$");
	}



}
