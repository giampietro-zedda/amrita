package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import enums.EnumExpressionItem;

/**
 * 
 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
 * 
 * <h1>
 * InstructionCobolProcedure
 * </h1>
 *  <p>
 * Questa classe modella uno specifico statement sorgente del linguaggio Cobol di procedure division. <br>
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
public class InstructionCobolProcedure extends InstructionCobol implements Serializable, Cloneable {

	private static final long serialVersionUID = 1L;

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza  specifiche                                          						  //                                                        //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

	// Tipologia nodo al quale l'istruzione fa riferimento
	private boolean terminatedWithEnd = false;			    // True indica istruzione terminata con END-xxxxxx
	private boolean withHandledException = false;			// True indica istruzione con ON SIZE ERROR, AT END etc
    													

	
	/**  
	 * Costruttore senza parametri
	 *  
	 */
	public InstructionCobolProcedure()  {
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
	public InstructionCobolProcedure(int numInstr
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
	 * 
	 * Restituisce la label .
	 * 
	 * @return the label name
	 */
	public String labelGetName() {
		String labelName = "";
		
		// La label è memorizzata in Map_Descriptor con chiave $LABEL$
		labelName = (String) this.getMapDescriptorObject("$LABEL$");
		
		return labelName;
	}

	/**
	 * 
	 * Imposta la label .
	 * 
	 * @param String labelName
	 */
	public void labelPutName(String labelName) {
		addMapDescriptorObject("$LABEL$", labelName);
		return;
	}
 

	/**
	 * Imposta se la label è un paragrafo eseguito da perform.<br>
	 * <p>
	 * Questa informazione viene aggiornata a fine analisi,
	 * dopo la costruzione del grafo di programma.<br>
	 * <p>
	 * @param boolean isParagraph
	 */
	public void labelSetParagraph(boolean isParagraph) {
		addMapDescriptorObject("$PARAGRAPH$" , isParagraph);
	}
	
	/**
	 * Restituisce se la label è un paragrafo eseguito da perform.<br>
	 * <p>
	 * Questa informazione viene aggiornata a fine analisi,
	 * dopo la costruzione del grafo di programma.<br>
	 * <p>
	 * @return boolean isParagraph[]
	 */
	public boolean labelIsParagraph() {
		if (getMapDescriptorObject("$PARAGRAPH$") == null) {
			return false;
		}
		return (Boolean) getMapDescriptorObject("$PARAGRAPH$");
	}


	/**
	 * Imposta il numero dell'ultima istruzione del paragrafo<br>
	 * <p>
	 * @param int numInstrLast
	 */
	public void labelSetLastNumInstrParagraph(int numInstrLast) {
		addMapDescriptorObject("$NUM-INSTR-LAST$", numInstrLast);
	}

	/**
	 * Restituisce il numero dell'ultima istruzione del paragrafo<br>
	 * <p>
	 * @return int numInstrLast
	 */
	public int labelGetLastNumInstrParagraph() {
		return (Integer) getMapDescriptorObject("$NUM-INSTR-LAST$");
	}


	/**
	 * @return the terminatedWithEnd
	 */
	public boolean isTerminatedWithEnd() {
		return terminatedWithEnd;
	}

	/**
	 * @param terminatedWithEnd the terminatedWithEnd to set
	 */
	public void setTerminatedWithEnd(boolean terminatedWithEnd) {
		this.terminatedWithEnd = terminatedWithEnd;
	}
	

	
	/**
	 * 
	 * Restituisce true se l'istruzione gestisce delle exception.<br>
	 * <p>
	 * Per esempio ON SIZE ERROR, AT END etc
	 * 
	 * 
	 * @return the withHandledException
	 */
	public boolean isWithHandledException() {
		return withHandledException;
	}

	/**
	 * Imposta se l'istruzione gestisce delle exception.<br>
	 * <p>
	 * Per esempio ON SIZE ERROR, AT END etc
	 * 
	 * 
	 * @param withHandledException the withHandledException to set
	 */
	public void setWithHandledException(boolean withHandledException) {
		this.withHandledException = withHandledException;
	}
	
	
	/////////////////////////////////////////////////////////////////////////////////////////////////
	// Metodi specifici di ogni istruzione
	/////////////////////////////////////////////////////////////////////////////////////////////////
    
	/**
	 * Restituisce true se Procedure Division Using ...<br>
	 * <p>
	 * 
	 * @return True/False depending on using parm
	 */
	public boolean procDivIsUsingParms() {
		boolean bUsing = false;
		if (getMapDescriptorObject("$OPT$USING") != null) {
			bUsing = true;
		}
		return bUsing;
	}

	/**
	 * Imposta gli identificatori cobol dei campi di Using<br>
	 * <p>
	 * 
	 * @param ArrayList<DataItemCobolIdentifier>)
	 */
	public void  procDivSetUsingParms(ArrayList<DataItemCobolIdentifier> al_usingParm) {
		addMapDescriptorObject("$USING$", al_usingParm);
		addMapDescriptorObject("$OPT$USING", "");
		return;
	}
	
	/**
	 * Restituisce una array list di identificatori cobol dei campi di Using<br>
	 * <p>
	 * 
	 * Se non ci sono parametri di Using restituisce un array vuoto.
	 * 
	 * @return ArrayList<DataItemCobolIdentifier>)
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<DataItemCobolIdentifier>  procDivGetUsingParms() {
		if (getMapDescriptorObject("$OPT$USING") == null) {
			return new ArrayList<DataItemCobolIdentifier>();
		}
		return (ArrayList<DataItemCobolIdentifier>) getMapDescriptorObject("$USING$");
	}
	
	/**
	 * Imposta che la <tt>Move</tt> è di tipo <tt>Corresponding</tt><br>
	 * <p>
	 * 
	 */
	public void moveSetCorr() {
		addMapDescriptorObject("$OPT$" + "CORR", "");
	}

	/**
	 * Restituisce true se la <tt>Move</tt> è di tipo <tt>Corresponding</tt><br>
	 * <p>
	 * @return boolean isCorr
	 */
	public boolean moveIsCorr() {
		if (getMapDescriptorObject("$OPT$" + "CORR") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se la <tt>Move</tt> è di tipo <tt>(pos:lng)</tt><br>
	 * ovvero reference modification, per il campo di input o di output<br>
	 * <p>
	 * 
	 */
	public void moveSetRefMod() {
		addMapDescriptorObject("$OPT$" + "REF-MOD", "");
	}

	/**
	 * Restituisce se la <tt>Move</tt> è di tipo <tt>(pos:lng)</tt><br>
	 * ovvero reference modification, per il campo di input o di output<br>
	 * <p>
	 * @return boolean isRefMod
	 */
	public boolean moveIsRefMod() {
		if (getMapDescriptorObject("$OPT$" + "REF-MOD") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta che la <tt>Move</tt> è di tipo <tt>MOVE ALL</tt><br>
	 * <p>
	 * 
	 */
	public void moveSetAll() {
		addMapDescriptorObject("$OPT$" + "ALL", "");
	}

	/**
	 * Restituisce true se la <tt>Move</tt> è di tipo <tt>MOVE ALL</tt><br>
	 * <p>
	 * @return boolean isAll
	 */
	public boolean moveIsAll() {
		if (getMapDescriptorObject("$OPT$" + "ALL") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Restituisce lidentificatore completo di input dell'istruzione Move<br>
	 * <p>
	 * 
	 * Se non esiste restituisce null.
	 * 
	 * @return DataItemCobolIdentifier
	 */
	public DataItemCobolIdentifier  moveGetIdentifierFrom() {
		if (getMapDescriptorObject("$INP$") == null) {
			return null;
		}
		return (DataItemCobolIdentifier) getMapDescriptorObject("$INP$");
	}
	
	/**
	 * Inserisce identificatore completo di input dell'istruzione Move<br>
	 * <p>
	 * 
	 */
	public void  moveSetIdentifierFrom(DataItemCobolIdentifier identifierFrom) {
		addMapDescriptorObject("$INP$", identifierFrom);
	}
	
	/**
	 * Restituisce gli identificatori completi di output dell'istruzione Move<br>
	 * <p>
	 * 
	 * Se non esistono restituisce null.
	 * 
	 * @return ArrayList<DataItemCobolIdentifier>)
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<DataItemCobolIdentifier>  moveGetIdentifiersTo() {
		if (getMapDescriptorObject("$OUT$") == null) {
			return null;
		}
		return (ArrayList<DataItemCobolIdentifier>) getMapDescriptorObject("$OUT$");
	}
	
	/**
	 * Restituisce l'identificatore di output dell'istruzione Move
	 * con il nome fornito<br>
	 * <p>
	 * 
	 * Se non esistono restituisce null.
	 * 
	 * @return DataItemCobolIdentifier 
	 */
	public DataItemCobolIdentifier moveGetIdentifierTo(String nameIdentifier) {
		DataItemCobolIdentifier identifierTo = null;
		ArrayList<DataItemCobolIdentifier> ar_identifierTo = null;
		
		ar_identifierTo = moveGetIdentifiersTo();
		if (ar_identifierTo == null) {
			return null;
		}
		
		// Scan identificatori di output
		for (DataItemCobolIdentifier dataItemCobolIdentifier : ar_identifierTo) {
			if (dataItemCobolIdentifier.getNameIdentifier().equals(nameIdentifier)) {
				identifierTo = dataItemCobolIdentifier;
				break;
			}
		}
		
		return identifierTo;
	}
	
	/**
	 * Inserisce lidentificatori completi di output dell'istruzione Move<br>
	 * <p>
	 * 
	 */
	public void  moveSetIdentifiersTo(ArrayList<DataItemCobolIdentifier> al_identifierTo) {
		addMapDescriptorObject("$OUT$", al_identifierTo);
	}


	
	/**
	 * Imposta il nome del file interno di una istruzione Read. <br>
	 * <p>
	 * 
	 */
	public void  readSetFileNameInternal(String fileNameRead) {
		addMapDescriptorObject("$FILE$", fileNameRead);
	}

	/**
	 * Restituisce i nome dei files esterni di una istruzione Read. <br>
	 * <p>
	 * 
	 */
	public String[]  readGetFileNamesExternal() {
		return (String[]) getMapDescriptorObject("$DDNAMES$");
	}
	

	/**
	 * Imposta il nome dei files esterni di una istruzione Read. <br>
	 * <p>
	 * 
	 */
	public void  readSetFileNamesExternal(String ar_FileNameExternal[]) {
		addMapDescriptorObject("$DDNAMES$", ar_FileNameExternal);
	}
	
	/**
	 * Imposta l'dentificatore Cobol della Ioarea di una istruzione Read. <br>
	 * <p>
	 * @param DataItemCobolIdentifier intoIoareaIdentifier
	 */
	public void  readSetIoareaInto(DataItemCobolIdentifier intoIoareaIdentifier) {
		addMapDescriptorObject("$IOAREA$", intoIoareaIdentifier);
	}
	
	/**
	 * Restituisce il nome del file interno di una istruzione Read. <br>
	 * <p>
	 * 
	 */
	public String  readGetFileNameInternal() {
		return (String) getMapDescriptorObject("$FILE$");
	}
	
	/**
	 * Restituisce l'dentificatore Cobol della Ioarea di una istruzione Read. <br>
	 * <p>
	 * @return DataItemCobolIdentifier ioarea
	 */
	public DataItemCobolIdentifier  readGetIoareaInto() {
		return (DataItemCobolIdentifier) getMapDescriptorObject("$IOAREA$");
	}

	
	/**
	 * Imposta il nome del file interno di una istruzione Write. <br>
	 * <p>
	 * 
	 */
	public void  writeSetFileNameInternal(String fileNameWrite) {
		addMapDescriptorObject("$FILE$", fileNameWrite);
	}
	
	/**
	 * Imposta il nome dei files esterni di una istruzione Write. <br>
	 * <p>
	 * 
	 */
	public void  writeSetFilesNameExternal(String ar_FileNameExternal[]) {
		addMapDescriptorObject("$DDNAMES$", ar_FileNameExternal);
	}
	
	/**
	 * Imposta l'dentificatore Cobol della Ioarea di una istruzione Write. <br>
	 * <p>
	 * @param DataItemCobolIdentifier identifierIoarea
	 */
	public void  writeSetIoareaFrom(DataItemCobolIdentifier identifierIoarea) {
		addMapDescriptorObject("$IOAREA$", identifierIoarea);
	}
	
	/**
	 * Restituisce il nome del file interno di una istruzione Write. <br>
	 * <p>
	 * 
	 */
	public String  writeGetFileNameInternal() {
		return (String) getMapDescriptorObject("$FILE$");
	}
	
	/**
	 * Restituisce i nome dei files esterni di una istruzione Write. <br>
	 * <p>
	 * 
	 */
	public String[]  writeGetFilesNameExternal() {
		return (String[]) getMapDescriptorObject("$DDNAMES$");
	}
	
	/**
	 * Restituisce l'dentificatore Cobol della Ioarea di una istruzione Write. <br>
	 * <p>
	 * @return DataItemCobolIdentifier identifierIoarea
	 */
	public DataItemCobolIdentifier  writeGetIoareaFrom() {
		return (DataItemCobolIdentifier) getMapDescriptorObject("$IOAREA$");
	}

	/**
	 * Imposta il nome del file interno di una istruzione Rewrite. <br>
	 * <p>
	 * 
	 */
	public void  rewriteSetFileNameInternal(String fileNamereWrite) {
		addMapDescriptorObject("$FILE$", fileNamereWrite);
	}
	
	/**
	 * Imposta il nome dei files esterni di una istruzione Rewrite. <br>
	 * <p>
	 * 
	 */
	public void  rewriteSetFilesNameExternal(String ar_FileNameExternal[]) {
		addMapDescriptorObject("$DDNAMES$", ar_FileNameExternal);
	}
	
	/**
	 * Imposta l'dentificatore Cobol della Ioarea di una istruzione Rewrite. <br>
	 * <p>
	 * @param DataItemCobolIdentifier intoIoareaIdentifier
	 */
	public void  rewriteSetIoareFrom(DataItemCobolIdentifier fromIoareaIdentifier) {
		addMapDescriptorObject("$IOAREA$", fromIoareaIdentifier);
	}
	
	/**
	 * Restituisce il nome del file interno di una istruzione Rerite. <br>
	 * <p>
	 * 
	 */
	public String  rewriteGetFileNameInternal() {
		return (String) getMapDescriptorObject("$FILE$");
	}
	
	/**
	 * Restituisce i nome dei files esterni di una istruzione Rewrite. <br>
	 * <p>
	 * 
	 */
	public String[]  rewriteGetFilesNameExternal() {
		return (String[]) getMapDescriptorObject("$DDNAMES$");
	}
	
	/**
	 * Restituisce l'dentificatore Cobol della Ioarea di una istruzione Rewrite. <br>
	 * <p>
	 * @return DataItemCobolIdentifier ioareaIdentiier
	 */
	public DataItemCobolIdentifier  rewriteGetIoareaFrom() {
		return (DataItemCobolIdentifier) getMapDescriptorObject("$IOAREA$");
	}


	/**
	 * Imposta il nome del file interno di una istruzione Delete. <br>
	 * <p>
	 * 
	 */
	public void  deleteSetFileNameInternal(String fileNameDelete) {
		addMapDescriptorObject("$FILE$", fileNameDelete);
	}
	
	/**
	 * Imposta il nome dei files esterni di una istruzione Delete. <br>
	 * <p>
	 * 
	 */
	public void  deleteSetFilesNameExternal(String ar_FileNameExternal[]) {
		addMapDescriptorObject("$DDNAMES$", ar_FileNameExternal);
	}
		
	/**
	 * Restituisce il nome del file interno di una istruzione Rerite. <br>
	 * <p>
	 * 
	 */
	public String  deleteGetFileNameInternal() {
		return (String) getMapDescriptorObject("$FILE$");
	}
	
	/**
	 * Restituisce i nome dei files esterni di una istruzione Delete. <br>
	 * <p>
	 * 
	 */
	public String[]  deleteGetFilesNameExternal() {
		return (String[]) getMapDescriptorObject("$DDNAMES$");
	}


	
	/**
	 * Imposta il nome del file interno di una istruzione Start. <br>
	 * <p>
	 * 
	 */
	public void  startSetFileNameInternal(String fileNameDelete) {
		addMapDescriptorObject("$FILE$", fileNameDelete);
	}
	
	/**
	 * Imposta il nome dei files esterni di una istruzione Start. <br>
	 * <p>
	 * 
	 */
	public void  startSetFilesNameExternal(String ar_FileNameExternal[]) {
		addMapDescriptorObject("$DDNAMES$", ar_FileNameExternal);
	}
		
	/**
	 * Imposta la condizione di start come espressione Cobol. <br>
	 * <p>
	 * 
	 */
	public void  startSetCondition(EnumExpressionItem condition) {
		addMapDescriptorObject("$COND$", condition);
	}
		
	/**
	 * Imposta la chiave di start come definizione di campo Cobol. <br>
	 * <p>
	 * 
	 */
	public void  startSetKey(InstructionCobolDataItem dataItemKey) {
		addMapDescriptorObject("$KEY$", dataItemKey);
	}
		
	/**
	 * Restituisce il nome del file interno di una istruzione Start. <br>
	 * <p>
	 * 
	 */
	public String  startGetFileNameInternal() {
		return (String) getMapDescriptorObject("$FILE$");
	}
	
	/**
	 * Restituisce i nome dei files esterni di una istruzione Start. <br>
	 * <p>
	 * 
	 */
	public String[]  startGetFilesNameExternal() {
		return (String[]) getMapDescriptorObject("$DDNAMES$");
	}


	/**
	 * Restituisce la condizione di start come espressione Cobol. <br>
	 * <p>
	 * 
	 */
	public EnumExpressionItem  startGetCondition() {
		return (EnumExpressionItem) getMapDescriptorObject("$COND$");
	}
		
	/**
	 * Restituisce la chiave di start come definizione di campo Cobol. <br>
	 * <p>
	 * 
	 */
	public InstructionCobolDataItem  startGetKey() {
		return (InstructionCobolDataItem) getMapDescriptorObject("$KEY$");
	}
		

	/**
	 * Restituisce l'identificatore completo del nome del programma<br>
	 * <p>
	 * 
	 * Se non esiste restituisce null.
	 * 
	 * @return DataItemCobolIdentifier
	 */
	public DataItemCobolIdentifier callGetProgram() {
		if (getMapDescriptorObject("$PARM$" + "PROGRAM") == null) {
			return null;
		}
		@SuppressWarnings("unchecked")
		ArrayList<DataItemCobolIdentifier> al = (ArrayList<DataItemCobolIdentifier>) getMapDescriptorObject("$PARM$" + "PROGRAM");
		return al.get(0);
	}
	
	/**
	 * Imposta l'identificatore completo del nome del programma<br>
	 * <p>
	 * 
	 */
	public void callSetProgram(DataItemCobolIdentifier identifierProgram) {
		addMapDescriptorObject("$PARM$" + "PROGRAM", identifierProgram);   
	}

	
	/**
	 * Restituisce l'identificatore completo del campo di GIVING, per Cobol MF<br>
	 * <p>
	 * 
	 * Se non esiste restituisce null.
	 * 
	 * @return DataItemCobolIdentifier
	 */
	public DataItemCobolIdentifier callGetGivingInto() {
		if (getMapDescriptorObject("$PARM$" + "GIVING") == null) {
			return null;
		}
		return (DataItemCobolIdentifier) getMapDescriptorObject("$PARM$" + "GIVING");
	}
	
	/**
	 * Imposta l'identificatore completo del campo di GIVING, per Cobol MF<br>
	 * <p>
	 * 
	 */
	public void callSetGiving(DataItemCobolIdentifier identifierGiving) {
		addMapDescriptorObject("$PARM$" + "GIVING", identifierGiving);
	}

	/**
	 * Restituisce l'identificatore completo del campo di RETURNING, per Cobol MF<br>
	 * <p>
	 * 
	 * Se non esiste restituisce null.
	 * 
	 * @return DataItemCobolIdentifier
	 */
	public DataItemCobolIdentifier callGetReturning() {
		if (getMapDescriptorObject("$PARM$" + "RETURNING") == null) {
			return null;
		}
		return (DataItemCobolIdentifier) getMapDescriptorObject("$PARM$" + "RETURNING");
	}
	
	/**
	 * Imposta l'identificatore completo del campo di RETURNING, per Cobol MF<br>
	 * <p>
	 * 
	 */
	public void callSetReturning(DataItemCobolIdentifier identifierReurning) {
		addMapDescriptorObject("$PARM$" + "RETURNING", identifierReurning);
	}

	
	
	
	/**
	 * Imposta gli identificatori cobol dei campi di Using<br>
	 * <p>
	 * 
	 * @param ArrayList<DataItemCobolIdentifier>)
	 */
	public void  callSetUsingParms(ArrayList<DataItemCobolIdentifier> al_usingParm) {
		addMapDescriptorObject("$USING$", al_usingParm);
		addMapDescriptorObject("$OPT$USING", "");
		return;
	}
	
	/**
	 * Restituisce una array list di identificatori cobol dei campi di Using<br>
	 * <p>
	 * 
	 * Se non ci sono parametri di Using restituisce un array vuoto.
	 * 
	 * @return ArrayList<DataItemCobolIdentifier>)
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<DataItemCobolIdentifier>  callGetUsingParms() {
		if (getMapDescriptorObject("$OPT$USING") == null) {
			return new ArrayList<DataItemCobolIdentifier>();
		}
		return (ArrayList<DataItemCobolIdentifier>) getMapDescriptorObject("$USING$");
	}
	

	/**
	 * Restituisce true se Call Using ...<br>
	 * <p>
	 * 
	 * @return True/False depending on using parm
	 */
	public boolean callIsUsingParms() {
		boolean bUsing = false;
		if (getMapDescriptorObject("$OPT$USING") != null) {
			bUsing = true;
		}
		return bUsing;
	}

	
	
	
	
	/**
	 * Restituisce un ArrayList con gli identificatori cobol completi dei programmi
	 * nello statement cancel<br>
	 * <p>
	 * 
	 * Se non esiste restituisce un array vuoto.
	 * 
	 * @return ArrayList<DataItemCobolIdentifier>
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<DataItemCobolIdentifier> callCancelGetPgmFieldIdentifiers() {
		if (getMapDescriptorObject("$PARM$" + "PROGRAM") == null) {
			return new ArrayList<DataItemCobolIdentifier>();
		}
		return (ArrayList<DataItemCobolIdentifier>) getMapDescriptorObject("$PARM$" + "PROGRAM");
	}
	
	/**
	 * Imposta il nome della section di programma<br>
	 * <p>
	 * 
	 */
	public void sectionSetName(String sectionName) {
		addMapDescriptorObject("$SECTION$", sectionName);
	}

	/**
	 * Imposta la priorità della section di programma
	 * espressa dal parametri prty<br>
	 * <p>
	 * @param int priority
	 */
	public void sectionSetPriority(int priority) {
		addMapDescriptorObject("$PRTY$", priority);
	}

	/**
	 * Restituisce la priorità della section di programma
	 * espressa dal parametri prty<br>
	 * <p>
	 * @return int priority
	 */
	public int sectionGetPriority(int priority) {
		addMapDescriptorObject("$PRTY$", priority);
		return (Integer) getMapDescriptorObject("$PRTY$");
	}

	/**
	 * Imposta il numero dell'ultima istruzione della section<br>
	 * <p>
	 * @param int numInstrLast
	 */
	public void sectionSetLastNumInstr(int numInstrLast) {
		addMapDescriptorObject("$NUM-INSTR-LAST$", numInstrLast);
	}

	/**
	 * Restituisce il numero dell'ultima istruzione della section<br>
	 * <p>
	 * @return int numInstrLast
	 */
	public int sectionGetLastNumInstr() {
		return (Integer) getMapDescriptorObject("$NUM-INSTR-LAST$");
	}

	/**
	 * Restituisce il nome della section di programma<br>
	 * <p>
	 * se non esiste restituisce null.
	 * 
	 */
	public String sectionGetName() {
		return (String) getMapDescriptorObject("$SECTION$");
	}


	
	/**
	 * Imposta gli identificatori cobol completi dei programmi nello statement cancel<br>
	 * <p>
	 * 
	 * @param ArrayList<DataItemCobolIdentifier>
	 */
	public void callCancelPutPgmFieldIdentifiers(ArrayList<DataItemCobolIdentifier> al_identifierProgram) {
		addMapDescriptorObject("$PARM$" + "PROGRAM", al_identifierProgram);
		return ;
	}
	
	/**
	 * 
	 * Restituisce le label a fronte di GoTo incondizionato o oppure DependingOn
	 * 
	 * @return array di the label name
	 */
	public String[] goToGetLabels() {
		String ar_labelName[] = null;
		ar_labelName = (String[]) this.getMapDescriptorObject("$LABEL$");
		return ar_labelName;
	}

	/**
	 * Imposta se l'istruzione GoTo ha l'opzione Depending On<br>
	 */
	public void goToSetDependingOn() {
		addMapDescriptorObject("OPT$"+"DEP-ON", "");
	}

	/**
	 * Restituisce se l'istruzione GoTo ha l'opzione Depending On<br>
	 * <p>
	 * @return boolean isDependingOn
	 */
	public boolean goToIsDependingOn() {
		if (getMapDescriptorObject("OPT$"+"DEP-ON") == null) {
			return false;
		}
		return true;
	}


	/**
	 * 
	 * Imposta le label a fronte di GoTo incondizionato o oppure DependingOn
	 * 
	 * @param String ar_labelName[]
	 */
	public void goToPutLabels(String ar_labelName[]) {
		this.addMapDescriptorObject("$LABEL$", ar_labelName);
		return;
	}

	/**
	 * 
	 * Restituisce il campo di GoTo incondizionato o oppure DependingOn
	 * 
	 * @return DataItemCobolIdentifier dependingOn field
	 */
	public DataItemCobolIdentifier goToGetDependingOnField() {
		DataItemCobolIdentifier dependingOnField = null;
		dependingOnField = (DataItemCobolIdentifier) this.getMapDescriptorObject("$DEP-ON$");
		return dependingOnField;
	}

	/**
	 * 
	 *  Imposta il campo di GoTo incondizionato o oppure DependingOn
	 * 
	 * @param DataItemCobolIdentifier dependingOn field
	 */
	public void goToPutDependingOnField(DataItemCobolIdentifier dependingOnField) {
		this.addMapDescriptorObject("$DEP-ON$", dependingOnField);
		return;
	}

	/**
	 * Imposta se la perform richiama o meno una procedura interna
	 * 
	 * @param boolean isInnerPerform[]
	 */
	public void performSetInnerPerform(boolean isInnerPerform) {
		addMapDescriptorObject("$INNER-PERFORM$" , "");
	}
	
	/**
	 * Restituisce se la perform richiama o meno una procedura interna
	 * <p>
	 * @return boolean isInnerPerform[]
	 */
	public boolean performIsInnerPerform() {
		if (getMapDescriptorObject("$INNER-PERFORM$") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se la perform è del tipo PERFORM A THRU B<br>
	 * <p>
	 * 
	 */
	public void performSetWithThru() {
		addMapDescriptorObject("$WITH-THRU$" , "");
	}
	
	/**
	 * Restituisce se la perform è del tipo PERFORM A THRU B<br>
	 * <p>
	 * @return boolean isWithThru
	 */
	public boolean performIsWithThru() {
		if (getMapDescriptorObject("$WITH-THRU$") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta il nome della procedura interna richiamata, una section
	 * o un paragrafo.<br>
	 * <p>
	 * @param String procedureNameFrom
	 */
	public void performSetFrom(String procedureNameFrom) {
		addMapDescriptorObject("$PROC$" , procedureNameFrom);
	}

	/**
	 * Restituisce il nome della procedura interna richiamata, una section
	 * o un paragrafo.<br>
	 * <p>
	 * @return String procedureNameFrom
	 */
	public String performGetFrom() {
		return (String) getMapDescriptorObject("$PROC$");
	}
	
	/**
	 * Restituisce il numero dell'istruzione della procedura interna richiamata, 
	 * una section o un paragrafo.<br>
	 * <p>
	 * @return the number of instruction of the procedure performed
	 */
	public Integer performGetFromNumInstr() {
		if (getMapDescriptorObject("$PROC-INSTR$") == null) {
			return null;
		}
		return (Integer) getMapDescriptorObject("$PROC-INSTR$");
	}
	
	/**
	 * Imposta il numero dell'istruzione della procedura interna richiamata, 
	 * una section o un paragrafo.<br>
	 * <p>
	 * @return the number of instruction of the procedure performed
	 */
	public void performSetFromNumInstr(int numInstrFrom) {
		this.addMapDescriptorObject("$PROC-INSTR$", numInstrFrom);
		return;
	}
	
	/**
	 * Restituisce il nome della procedura interna richiamata, una section
	 * o un paragrafo, con il nome utilizzato per la codifica come sottografo
	 * interno.<br>
	 * <p>
	 * Si tratta del nome della procedura interna richiamata + ":" +
	 * il nome della label, se richiamata con thru.<br>
	 * <p>
	 * @return String procedureName   coded for GraphManager
	 */
	public String performGetIdSubGraphCalled() {
		String idProc = "";
		String idProcThru = "";
		idProc = this.performGetFrom();
		idProc = idProc + ":";
		idProcThru = this.performGetThru();
		if (idProcThru != null && !idProcThru.equals("")) {
			idProc = idProc + idProcThru;
		}
		return idProc;
	}
	

	/**
	 * Restituisce il nome della fine della procedura interna richiamata, una section
	 * o un paragrafo.<br>
	 * Restituisce space se thru non presente
	 * <p>
	 * @return String procedureNameThru
	 */
	public String performGetThru() {
		String performThru = "";
		performThru = (String) getMapDescriptorObject("$PROC-THRU$");
		return (performThru == null) ? "" : performThru;
	}

	/**
	 * Restituisce il numero di istruzione label della fine della procedura interna richiamata.<br>
	 * <p>
	 * Se la procedura non è stata richiamata con thru restituiscer il numero dell'ultima istruzione
	 * della Section o del paragrafo.<br>
	 * <p>
	 * @return int num procedureNameThru
	 */
	public int performGetThruNumInstr() {
		Integer performThruNumInstr = (Integer) getMapDescriptorObject("$PROC-THRU-INSTR$");		
		return (performThruNumInstr == null) ? 0 : performThruNumInstr ;
	}


	/**
	 * Imposta il nome della fine procedura interna richiamata, una section
	 * o un paragrafo.<br>
	 * <p>
	 * @param String procedureNameThru
	 */
	public void performSetThru(String procedureNameThru) {
		addMapDescriptorObject("$PROC-THRU$" , procedureNameThru);
	}

	/**
	 * Imposta il numero di istruzione label della fine della procedura interna richiamata.<br>
	 * <p>
	 * Se la procedura non è stata richiamata con thru imposta il numero dell'ultima istruzione
	 * della Section o del paragrafo.<br>
	 * <p>
	 * @param int num procedureNameThru
	 */
	public void performSetThruNumInstr(int numInstrThru) {
		addMapDescriptorObject("$PROC-THRU-INSTR$", numInstrThru);
	}

	/**
	 * Imposta il numero di volte che la procedura viene eseguita, una section
	 * o un paragrafo.<br>
	 * Il numero di "times" è espresso come un identificatore cobol completo.<br>
	 * <p>
	 * @param DataItemCobolIdentifier timesIdentifier
	 */
	public void performSetTimes(DataItemCobolIdentifier timesIdentifier) {
		addMapDescriptorObject("$TIMES$" , timesIdentifier);
		
	}
	
	/**
	 * Restituisce il numero di volte che la procedura viene eseguita, una section
	 * o un paragrafo.<br>
	 * Il numero di "times" è espresso come un identificatore cobol completo.<br>
	 * <p>
	 * @return DataItemCobolIdentifier timesIdentifier
	 */
	public DataItemCobolIdentifier performGetTimes() {
		return (DataItemCobolIdentifier) getMapDescriptorObject("$TIMES$");
		
	}

	/**
	 * Imposta il campo di varying della perform, espresso come un identificatore cobol completo.<br>
	 * <p>
	 * @param DataItemCobolIdentifier varyingIdentifier
	 */
	public void performSetVarying(DataItemCobolIdentifier varyingIdentifier) {
		addMapDescriptorObject("$VARYING$" , varyingIdentifier);
		
	}
	
	/**
	 * Restituisce il campo di varying della perform, espresso come un identificatore cobol completo.<br>
	 * <p>
	 * @return DataItemCobolIdentifier timesIdentifier
	 */
	public DataItemCobolIdentifier performGetVarying() {
		return (DataItemCobolIdentifier) getMapDescriptorObject("$VARYING$");
		
	}

	/**
	 * Imposta se è presente l'opzione with test after nella perform.<br>
	 * <p>
	 * @param boolean isWithTestAfter
	 */
	public void performSetWithTestAfter() {
		addMapDescriptorObject("$WITH-TEST-AFTER$" , "");
		
	}
	/**
	 * Restituisce se è presente l'opzione with test after nella perform.<br>
	 * <p>
	 * @return boolean isWithTestAfter
	 */
	public Boolean performIsWithTestAfter() {
		if (getMapDescriptorObject("$WITH-TEST-AFTER$") == null) {
			return false;
		}
		return true;
		
	}
	
	/**
	 * Imposta se è presente l'opzione VARYING k nella perform.<br>
	 * <p>
	 * @param boolean isVarying
	 */
	public void performSetWithVarying(boolean isVarying) {
		addMapDescriptorObject("$WITH-VARYING$" , "");
		
	}
	/**
     * Restituisce se è presente l'opzione VARYING k nella perform.<br>
	 * <p>
	 * @return boolean isVarying
	 */
	public Boolean performIsWithVarying() {
		if (getMapDescriptorObject("$WITH-VARYING$") == null) {
			return false;
		}
		return true;
		
	}
	/**
	 * Imposta se è presente la condizione di UNTIL nella perform.<br>
	 * <p>
	 * @param boolean isUntil
	 */
	public void performSetWithUntil() {
		addMapDescriptorObject("$WITH-UNTIL$" , "");
		
	}
	/**
     * Restituisce se è presente la condizione di UNTIL nella perform.<br>
	 * <p>
	 * @return boolean isUntil
	 */
	public Boolean performIsWithUntil() {
		if (getMapDescriptorObject("$WITH-UNTIL$") == null) {
			return false;
		}
		return true;
	}
	/**
	 * Imposta se la perform è della forma VARYING K TIMES.<br>
	 * <p>
	 * @param boolean isWithTimes
	 */
	public void performSetWithTimes() {
		addMapDescriptorObject("$WITH-TIMES$" , "");
		
	}
	/**
     * Restituisce se la perform è della forma VARYING K TIMES.<br>
	 * <p>
	 * @return boolean isWithTimes
	 */
	public Boolean performIsWithTimes() {
		if (getMapDescriptorObject("$WITH-TIMES$") == null) {
			return false;
		}
		return true;
		
	}
	
	/**
	 * Imposta se è presente l'opzione with test before nella perform.<br>
	 * <p>
	 * @param boolean isWithTestBefore
	 */
	public void performSetWithTestBefore() {
		addMapDescriptorObject("$WITH-TEST-BEFORE$" , "");
		
	}
	
	/**
	 * Restituisce se è presente l'opzione with test before nella perform.<br>
	 * <p>
	 * @return boolean isWithTestBefore
	 */
	public Boolean performIsWithTestBefore() {
		if (getMapDescriptorObject("$WITH-TEST-BEFORE$") == null) {
			return false;
		}
		return true;
		
	}

	/**
	 * Imposta le condizioni di Until della perform.<br>
	 * <p>
	 * @param ArrayList<CobolPerformLoop> al_innerLoopCondition
	 */
	public void performSetUntil(ArrayList<CobolPerformLoop> al_innerLoopCondition) {
		addMapDescriptorObject("$COND-LOOP$" , al_innerLoopCondition);
	}

	/**
	 * Restituisce le condizioni di Until della perform.<br>
	 * <p>
	 * Le condizioni vengono restituite in un ArrayList di oggetti<br>
	 * {@link CobolPerformLoop} contenenti Varying identifier, <br>
     *  From expression, By identifier e Condition expression<br>
     *  <p>
 	 * @return ArrayList<CobolPerformLoop> al_innerLoopCondition
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<CobolPerformLoop> performGetUntil() {
		ArrayList<CobolPerformLoop> al_innerLoopCondition = null;
		al_innerLoopCondition = (ArrayList<CobolPerformLoop>) getMapDescriptorObject("$COND-LOOP$" );
		if (al_innerLoopCondition == null) {
			al_innerLoopCondition = new  ArrayList<CobolPerformLoop>();
		}
		return al_innerLoopCondition;
	}

	/**
	 * Imposta la condizione dello statement If.<br>
	 * <p>
	 * La condizione è codificata da un oggetto {@link ExpressionCobol}
	 * 
	 * @param ExpressionCobol expression
	 */
	public void ifSetCondition(ExpressionCobol expression) {
		addMapDescriptorObject("$COND$", expression);
	}

	/**
	 * Restituisce le espressioni da valutare codificate nello statement Evaluate.<br>
	 * <p>
	 * Ogni espressione è codificata da un oggetto {@link ExpressionCobol}<br>
	 * Le espressioni successive alla prina sono quelle codificate con la clausola ALSO.<br>
	 * <p>
	 * @return ArrayList<ExpressionCobol> expressions
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<ExpressionCobol> evaluateGetExpressions() {
		return (ArrayList<ExpressionCobol>) getMapDescriptorObject("$EXPR$");
	}

	/**
	 * Imposta le espressioni da valutare codificate nello statement Evaluate.<br>
	 * <p>
	 * Ogni espressione è codificata da un oggetto {@link ExpressionCobol}<br>
	 * Le espressioni successive alla prina sono quelle codificate con la clausola ALSO.<br>
	 * <p>
	 * 
	 * @param ExpressionCobol expression
	 */
	public void evaluateSetExpressions( ArrayList<ExpressionCobol> al_expression) {
		addMapDescriptorObject("$EXPR$", al_expression);
	}

	/**
	 * Restituisce la condizione dello statement If.<br>
	 * <p>
	 * La condizione è codificata da un oggetto {@link ExpressionCobol}
	 * 
	 * @return ExpressionCobol expression
	 */
	public ExpressionCobol ifGetCondition() {
		return (ExpressionCobol) getMapDescriptorObject("$COND$");
	}

	/**
	 * Imposta l'espressione assegnata nello statement Compute.<br>
	 * <p>
	 * L'espressione è codificata da un oggetto {@link ExpressionCobol}
	 * 
	 * @param ExpressionCobol expression
	 */
	public void computeSetExpressionAssigned(ExpressionCobol expression) {
		addMapDescriptorObject("$EXPR$", expression);
	}

	/**
	 * Restituisce l'espressione assegnata nello statement Compute.<br>
	 * <p>
	 * L'espressione è codificata da un oggetto {@link ExpressionCobol}
	 * 
	 * @return ExpressionCobol expression
	 */
	public ExpressionCobol computeGetExpressionAssigned() {
		return (ExpressionCobol) getMapDescriptorObject("$EXPR$");
	}

	/**
	 * Imposta l'identificatore cobol del campo risultato nello statement Compute.<br>
	 * <p>
	 * L'identificatore è codificata da un oggetto {@link DataItemCobolIdentifier}
	 * 
	 * @param DataItemCobolIdentifier result
	 */
	public void computeSetResultIdentifier(DataItemCobolIdentifier result) {
		addMapDescriptorObject("$RESULT$", result);
	}
 
	/**
	 * Restituisce l'identificatore cobol del campo risultato nello statement Compute.<br>
	 * <p>
	 * L'identificatore è codificata da un oggetto {@link DataItemCobolIdentifier}
	 * 
	 * @param DataItemCobolIdentifier result
	 */
	public DataItemCobolIdentifier computeGetResultIdentifier() {
		return (DataItemCobolIdentifier) getMapDescriptorObject("$RESULT$");
	}
 
	/**
	 * Restituisce un ArrayList con le espressioni condizionali presenti 
	 * nello statement When di Evaluate o di Search.<br>
	 * <p>
	 * L'espressione è codificata da un oggetto {@link ExpressionCobol}
	 * 
	 * @return le espressioni cobol
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<ExpressionCobol> whenGetConditions() {
		return  (ArrayList<ExpressionCobol>) getMapDescriptorObject("$COND$");
	}

	/**
	 * Imposta un ArrayList con le espressioni condizionali presenti 
	 * nello statement When di Evaluate o di Search.<br>
	 * <p>
	 * L'espressione è codificata da un oggetto {@link ExpressionCobol}
	 * 
	 * @return le espressioni cobol
	 */
	public void whenSetConditions(ArrayList<ExpressionCobol> al_expression) {
		addMapDescriptorObject("$COND$", al_expression);
		return;
	}

	/**
	 * Imposta tse l'istruzione e WHEN OTHER di EVALUATE<br>
	 * <p>
	 * 
	 * @return le espressioni cobol
	 */
	public void whenSetOther() {
		addMapDescriptorObject("OPT$"+"OTHER", "");
		return;
	}

	/**
	 * Restituisce true se l'istruzione e WHEN OTHER di EVALUATE<br>
	 * <p>
	 * @return boolean isToUpBy
	 */
	public boolean whenIsOther() {
		if (getMapDescriptorObject("OPT$"+"OTHER") == null) {
			return false;
		}
		return true;
	}
	/**
	 * Imposta se l'istruzione set è del tipo SET campo UP BY campo/costante
	 */
	public void setSetToUpBy() {
		addMapDescriptorObject("OPT$"+"TO-UPBY", "");
	}

	/**
	 * Restituisce true se se l'istruzione set è del tipo SET campo UP BY campo/costante
	 * <p>
	 * @return boolean isToUpBy
	 */
	public boolean setIsToUpBy() {
		if (getMapDescriptorObject("OPT$"+"TO-UPBY") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se l'istruzione set è del tipo SET campo DOWN BY campo/costante
	 */
	public void setSetToDownBy() {
		addMapDescriptorObject("OPT$"+"TO-DOWNBY", "");
	}


	/**
	 * Restituisce true se se l'istruzione set è del tipo SET pointerFieldOut TO pointerFieldInp
	 * <p>
	 * @return boolean isFieldToField
	 */
	public boolean setIsFieldToField() {
		if (getMapDescriptorObject("OPT$"+"FLD-TO-FLD") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta true se se l'istruzione set è del tipo SET pointerFieldOut TO pointerFieldInp
	 */
	public void setSetFieldToField() {
		addMapDescriptorObject("OPT$"+"FLD-TO-FLD", "");
	}

	/**
	 * Restituisce true se se l'istruzione set è del tipo SET campo DOWN BY campo/costante
	 * <p>
	 * @return boolean isToDownBy
	 */
	public boolean setIsToDownBy() {
		if (getMapDescriptorObject("OPT$"+"TO-UPBY") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se l'istruzione set è del tipo SET campo-condizione TO TRUE
	 */
	public void setSetToTrue() {
		addMapDescriptorObject("OPT$"+"TO-TRUE", "");
	}

	/**
	 * Restituisce true se l'istruzione set è del tipo SET campo-condizione TO TRUE
	 * <p>
	 * @return boolean isToTrue
	 */
	public boolean setIsToTrue() {
		if (getMapDescriptorObject("OPT$"+"TO-TRUE") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se l'istruzione set è del tipo SET campo-condizione TO ON
	 */
	public void setSetToOn() {
		addMapDescriptorObject("OPT$"+"TO-ON", "");
	}

	/**
	 * Restituisce true se l'istruzione set è del tipo SET campo-condizione TO ON
	 * <p>
	 * @return boolean isToOn
	 */
	public boolean setIsToOn() {
		if (getMapDescriptorObject("OPT$"+"TO-ON") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se l'istruzione set è del tipo SET campo-condizione TO OFF
	 */
	public void setSetToOff() {
		addMapDescriptorObject("OPT$"+"TO-OFF", "");
	}

	/**
	 * Restituisce true se l'istruzione set è del tipo SET campo-condizione TO OFF
	 * <p>
	 * @return boolean isToOff
	 */
	public boolean setIsToOff() {
		if (getMapDescriptorObject("OPT$"+"TO-OFF") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se l'istruzione set è del tipo SET campo-condizione TO OFF
	 */
	public void setSetToNull() {
		addMapDescriptorObject("OPT$"+"TO-NULL", "");
	}

	/**
	 * Restituisce true se l'istruzione set è del tipo SET campo-condizione TO NULL/NULLS
	 * <p>
	 * @return boolean isToNull
	 */
	public boolean setIsToNull() {
		if (getMapDescriptorObject("OPT$"+"TO-NULL") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se l'istruzione set è del tipo SET campo1 campo2 TO campo/numero
	 */
	public void setSetToValue() {
		addMapDescriptorObject("OPT$"+"TO-VALUE", "");
	}

	/**
	 * Restituisce true se l'istruzione set è del tipo SET campo1 campo2 TO campo/numero
	 * <p>
	 * @return boolean isToValue
	 */
	public boolean setIsToValue() {
		if (getMapDescriptorObject("OPT$"+"TO-VALUE") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se l'istruzione set è del tipo SET ADDRESS OF Area TO Pointer
	 */
	public void setSetAddressOfToPointer() {
		addMapDescriptorObject("OPT$"+"ADDRESS-OF-TO-POINTER", "");
	}

	/**
	 * Restituisce true se l'istruzione set è del tipo SET ADDRESS OF Area TO Pointer
	 * <p>
	 * @return boolean isAddressOfToPointer
	 */
	public boolean setIsAddressOfToPointer() {
		if (getMapDescriptorObject("OPT$"+"ADDRESS-OF-TO-POINTER") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se l'istruzione set è del tipo SET pointer TO ADDRESS OF Area 
	 */
	public void setSetPointerToAddressOf() {
		addMapDescriptorObject("OPT$"+"POINTER-TO-ADDRESS-OF", "");
	}

	/**
	 * Restituisce true se l'istruzione set è del tipo SET pointer TO ADDRESS OF Area 
	 * <p>
	 * @return boolean isPointerToAddressOf
	 */
	public boolean setIsPointerToAddressOf() {
		if (getMapDescriptorObject("OPT$"+"POINTER-TO-ADDRESS-OF") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta l'dentificatore del pointer nello statement SET.<br>
	 * <p>
	 * Si tratta di istruzioni del tipo:<br>
	 * SET ADDRESS OF Area TO Pointer <br>
	 * SET pointer TO ADDRESS OF Area <br>
	 * <p>
	 * L'identificatore è codificata da un oggetto {@link DataItemCobolIdentifier}
	 * 
	 * @param DataItemCobolIdentifier result
	 */
	public void setSetPointer(DataItemCobolIdentifier pointer) {
		addMapDescriptorObject("$POINTER$", pointer);
	}
 
	/**
	 * Restituisce l'dentificatore del pointer nello statement SET.<br>
	 * <p>
	 * Si tratta di istruzioni del tipo:<br>
	 * SET ADDRESS OF Area TO Pointer <br>
	 * SET pointer TO ADDRESS OF Area <br>
	 * <p>
	 * L'identificatore è codificata da un oggetto {@link DataItemCobolIdentifier}<br>
	 * Se non esiste restituisce null.
	 * 
	 * @return DataItemCobolIdentifier pointer
	 */
	public DataItemCobolIdentifier setGetPointer() {
		if (getMapDescriptorObject("$POINTER$") == null) {
			return null;
		}
		return (DataItemCobolIdentifier) getMapDescriptorObject("$POINTER$");
	}
	
	/**
	 * Imposta l'dentificatore dell'area indirizzata nello statement SET.<br>
	 * <p>
	 * Si tratta di istruzioni del tipo:<br>
	 * SET ADDRESS OF Area TO Pointer <br>
	 * SET pointer TO ADDRESS OF Area <br>
	 * <p>
	 * L'identificatore è codificata da un oggetto {@link DataItemCobolIdentifier}
	 * 
	 * @param DataItemCobolIdentifier result
	 */
	public void setSetAreaAddressed(DataItemCobolIdentifier areaAddressed) {
		addMapDescriptorObject("$AREA-ADDRESSED$", areaAddressed);
	}
 
	/**
	 * Restituisce l'dentificatore dell'area indirizzata nello statement SET.<br>
	 * <p>
	 * Si tratta di istruzioni del tipo:<br>
	 * SET ADDRESS OF Area TO Pointer <br>
	 * SET pointer TO ADDRESS OF Area <br>
	 * <p>
	 * L'identificatore è codificata da un oggetto {@link DataItemCobolIdentifier}<br>
	 * Se non esiste restituisce null.
	 * 
	 * @return DataItemCobolIdentifier pointer
	 */
	public DataItemCobolIdentifier setGetAreaAddressed() {
		if (getMapDescriptorObject("$AREA-ADDRESSED$") == null) {
			return null;
		}
		return (DataItemCobolIdentifier) getMapDescriptorObject("$AREA-ADDRESSED$");
	}
	
	/**
	 * Imposta gli identificatori impostati dallo statement SET.<br>
	 * <p>
	 * Si tratta di istruzioni del tipo:<br>
	 * SET campo1, campo2 TO ON<br>
	 * SET campo1 TO campx<br>
	 * SET campo1 TO 34<br>
	 * <p>
	 * Gli identificatore sono codificati da oggetti {@link DataItemCobolIdentifier}
	 * e sono forniti in una ArrayList
	 * 
	 * @param Array List di DataItemCobolIdentifier items
	 */
	public void setSetItemsOutput(ArrayList<DataItemCobolIdentifier> al_itemsSet) {
		addMapDescriptorObject("$ITEMS-OUTPUT$", al_itemsSet);
	}
 
	/**
	 * Restituisce gli identificatori impostati dallo statement SET.<br>
	 * <p>
	 * Si tratta di istruzioni del tipo:<br>
	 * SET campo1, campo2 TO ON<br>
	 * SET campo1 TO campx<br>
	 * SET campo1 TO 34<br>
	 * <p>
	 * Gli identificatore sono codificati da oggetti {@link DataItemCobolIdentifier}
	 * e sono forniti in una ArrayList
	 * 
	 * @return Array List di DataItemCobolIdentifier items
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<DataItemCobolIdentifier> setGetItemsOutput() {
		if (getMapDescriptorObject("$ITEMS-OUTPUT$") == null) {
			return null;
		}
		return (ArrayList<DataItemCobolIdentifier>) getMapDescriptorObject("$ITEMS-OUTPUT$");
	}
	
	/**
	 * Imposta l'dentificatori in input allo statement SET.<br>
	 * <p>
	 * Si tratta di istruzioni del tipo:<br>
	 * SET campo1 TO campInput<br>
	 * SET campo1 TO 34<br>
	 * <p>
	 * L'identificatore è codificato da un oggetto {@link DataItemCobolIdentifier}
	 * 
	 * @param DataItemCobolIdentifier itemInput
	 */
	public void setSetItemInput(DataItemCobolIdentifier itemsInput) {
		addMapDescriptorObject("$ITEM-INPUT$", itemsInput);
	}
 
	/**
	 * Restituisce l'dentificatori in input allo statement SET.<br>
	 * <p>
	 * Si tratta di istruzioni del tipo:<br>
	 * SET campo1 TO campInput<br>
	 * SET campo1 TO 34<br>
	 * <p>
	 * L'identificatore è codificato da un oggetto {@link DataItemCobolIdentifier}
	 * 
	 * @return DataItemCobolIdentifier itemInput
	 */
	public DataItemCobolIdentifier setGetItemInput() {
		if (getMapDescriptorObject("$ITEM-INPUT$") == null) {
			return null;
		}
		return (DataItemCobolIdentifier) getMapDescriptorObject("$ITEM-INPUT$");
	}

	/**
	 * Restituisce i files in input dell'istruzione Open<br>
	 * <p>
	 * @param ArrayList<String> al_openInput
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> openGetInputFiles() {
        return (ArrayList<String>) getMapDescriptorObject("$INPUT$");
	}


	/**
	 * Imposta i files in input dell'istruzione Open<br>
	 * <p>
	 * @param ArrayList<String> al_openInput
	 */
	public void openSetInputFiles(ArrayList<String> al_openInput) {
		addMapDescriptorObject("$INPUT$", al_openInput);

	}

	/**
	 * Restituisce i files in output dell'istruzione Open<br>
	 * <p>
	 * @param ArrayList<String> al_openInput
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> openGetOutputFiles() {
        return (ArrayList<String>) getMapDescriptorObject("$OUTPUT$");
	}


	/**
	 * Imposta i files in output dell'istruzione Open<br>
	 * <p>
	 * @param ArrayList<String> al_openOutput
	 */
	public void openSetOutputFiles(ArrayList<String> al_openOutput) {
		addMapDescriptorObject("$OUTPUT$", al_openOutput);

	}

	/**
	 * Restituisce i files in IO dell'istruzione Open<br>
	 * <p>
	 * @param ArrayList<String> al_openIO
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> openGetIOFiles() {
        return (ArrayList<String>) getMapDescriptorObject("$I-O$");
	}


	/**
	 * Imposta i files in IO dell'istruzione Open<br>
	 * <p>
	 * @param ArrayList<String> al_openOutput
	 */
	public void openSetIOFiles(ArrayList<String> al_openIO) {
		addMapDescriptorObject("$I-O$", al_openIO);

	}

	/**
	 * Restituisce i files in EXTEND dell'istruzione Open<br>
	 * <p>
	 * @param ArrayList<String> al_openExtend
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> openGetExtendFiles() {
        return (ArrayList<String>) getMapDescriptorObject("$EXTEND$");
	}


	/**
	 * Imposta i files in EXTEND dell'istruzione Open<br>
	 * <p>
	 * @param ArrayList<String> al_openExtend
	 */
	public void openSetExtendFiles(ArrayList<String> al_openExtend) {
		addMapDescriptorObject("$EXTEND$", al_openExtend);

	}

	
	/**
	 * Restituisce i files specificati nell'istruzione Close<br>
	 * <p>
	 * @param ArrayList<String> al_closeFile
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> closeGetFiles() {
        return (ArrayList<String>) getMapDescriptorObject("$CLOSE-FILES$");
	}


	/**
	 * Imposta i files specificati nell'istruzione Close<br>
	 * <p>
	 * @param ArrayList<String> al_closeFile
	 */
	public void closeSetFiles(ArrayList<String> al_closeFile) {
		addMapDescriptorObject("$CLOSE-FILES$", al_closeFile);

	}

	/**
	 * Restituisce gli identificatori completi dei campi dell'istruzione DISPLAY<br>
	 * <p>
	 * @param ArrayList<String> al_closeFile
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<DataItemCobolIdentifier> displayGetIdentifiersInput() {
        return (ArrayList<DataItemCobolIdentifier>) getMapDescriptorObject("$DISPLAY-INPUT$");
	}

	/**
	 * Imposta gli identificatori completi dei campi dell'istruzione DISPLAY<br>
	 * <p>
	 * @param ArrayList<DataItemCobolIdentifier> al_identifierInput
	 */
	public void displaySetIdentifiersInput(ArrayList<DataItemCobolIdentifier> al_identifierInput) {
		addMapDescriptorObject("$DISPLAY-INPUT$", al_identifierInput);
	}

	/**
	 * Restituisce l'identificatore completo del nome mnemonico UPON dell'istruzione DISPLAY<br>
	 * <p>
	 * @return  DataItemCobolIdentifier identifierUpon
	 */
	public DataItemCobolIdentifier displayGetIdentifierUpon() {
        return (DataItemCobolIdentifier) getMapDescriptorObject("$DISPLAY-UPON$");
	}

	/**
	 * Imposta l'identificatori completo del nome mnemonico UPON dell'istruzione DISPLAY<br>
	 * <p>
	 * @param identifierUpon
	 */
	public void displaySetIdentifierUpon(DataItemCobolIdentifier identifierUpon) {
		addMapDescriptorObject("$DISPLAY-UPON$", identifierUpon);
	}

	/**
	 * Imposta se è presente la clausola UPON nella Display.<br>
	 * <p>
	 * @param boolean isUpon
	 */
	public void displaySetWithUpon() {
		addMapDescriptorObject("$WITH-UPON$" , "");
		
	}
	/**
	 * Restituisce se è presente la clausola UPON nella Display.<br>
	 * <p>
	 * @return boolean isUpon
	 */
	public Boolean displayIsWithUpon() {
		if (getMapDescriptorObject("$WITH-UPON$") == null) {
			return false;
		}
		return true;
	}

	
	/**
	 * Imposta se lo statement SORT definisce una input procedure
	 * 
	 * @param boolean isInputProcedure
	 */
	public void sortMergeSetWithInputProcedure() {
		addMapDescriptorObject("$INPUT-PROCEDURE$" , "");
	}
	

	/**
	 *  Restituisce se lo statement SORT definisce una input procedure
	 * <p>
	 * @return boolean isInputProcedure
	 */
	public boolean sortMergeIsInputProcedure() {
		if (getMapDescriptorObject("$INPUT-PROCEDURE$") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se lo statement SORT definisce una Output procedure
	 * 
	 * @param boolean isInputProcedure
	 */
	public void sortMergeSetWithOutputProcedure() {
		addMapDescriptorObject("$OUTPUT-PROCEDURE$" , "");
	}
	

	/**
	 *  Restituisce se lo statement SORT definisce una Output procedure
	 * <p>
	 * @return boolean isOutputProcedure
	 */
	public boolean sortMergeIsOutputProcedure() {
		if (getMapDescriptorObject("$OUTPUT-PROCEDURE$") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se input procedure è specificata con THRU<br>
	 * <p>
	 * 
	 */
	public void sortMergeSetWithThruInputProcedure() {
		addMapDescriptorObject("$WITH-THRU-INPUT-PROC$" , "");
	}
	
	/**
	 * Restituisce se input procedure è specificata con THRU<br>
	 * <p>
	 * 
	 * @return boolean isWithThruInputProcedure
	 */
	public boolean sortMergeIsWithThruInputProcedure() {
		if (getMapDescriptorObject("$WITH-THRU-INPUT-PROC$") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta se output procedure è specificata con THRU<br>
	 * <p>
	 * 
	 */
	public void sortMergeSetWithThruOutputProcedure() {
		addMapDescriptorObject("$WITH-THRU-OUTPUT-PROC$" , "");
	}
	
	/**
	 * Restituisce se output procedure è specificata con THRU<br>
	 * <p>
	 * 
	 * @return boolean isWithThruOutputProcedure
	 */
	public boolean sortMergeIsWithThruOutputProcedure() {
		if (getMapDescriptorObject("$WITH-THRU-OUTPUT-PROC$") == null) {
			return false;
		}
		return true;
	}

	/**
	 * Imposta il nome della procedura interna di input, una section.<br>
	 * <p>
	 * @param String procedureInputNameFrom
	 */
	public void sortMergeSetFromInputProcedure(String procedureInputNameFrom) {
		addMapDescriptorObject("$PROC-FROM-INPUT$" , procedureInputNameFrom);
	}

	/**
	 * Restituisce il nome della procedura interna di input, una section.<br>
	 * <p>
	 * @return String procedureInputNameFrom
	 */
	public String sortMergeGetFromInputProcedure() {
		return (String) getMapDescriptorObject("$PROC-FROM-INPUT$");
	}
	
	/**
	 * Imposta il nome della procedura interna di output, una section.<br>
	 * <p>
	 * @param String procedureOutputNameFrom
	 */
	public void sortMergeSetFromOutputProcedure(String procedureOutputNameFrom) {
		addMapDescriptorObject("$PROC-FROM-OUTPUT$" , procedureOutputNameFrom);
	}

	/**
	 * Restituisce il nome della procedura interna di output, una section.<br>
	 * <p>
	 * @return String procedureOutputNameFrom
	 */
	public String sortMergeGetFromOutputProcedure() {
		return (String) getMapDescriptorObject("$PROC-FROM-OUTPUT$");
	}
	
	/**
	 * Restituisce il numero dell'istruzione della procedura di input, una section.<br>
	 * <p>
	 * @return the number of instruction of the procedure performed
	 */
	public int sortMergeGetFromInputProcedureNumInstr() {
		return (Integer) getMapDescriptorObject("$PROC-INPUT-INSTR$");
	}
	
	/**
	 * Imposta il numero dell'istruzione della procedura di input, una section.<br>
	 * <p>
	 * @return the number of instruction of the procedure performed
	 */
	public void sortMergeSetFromInputProcedureNumInstr(int numInstrFrom) {
		this.addMapDescriptorObject("$PROC-INPUT-INSTR$", numInstrFrom);
		return;
	}
	
	/**
	 * Restituisce il numero dell'istruzione della procedura di output, una section.<br>
	 * <p>
	 * @return the number of instruction of the procedure performed
	 */
	public int sortMergeGetFromOutputProcedureNumInstr() {
		return (Integer) getMapDescriptorObject("$PROC-OUTPUT-INSTR$");
	}
	
	/**
	 * Imposta il numero dell'istruzione della procedura di input, una section.<br>
	 * <p>
	 * @return the number of instruction of the procedure performed
	 */
	public void sortMergeSetFromOutputProcedureNumInstr(int numInstrFrom) {
		this.addMapDescriptorObject("$PROC-OUTPUT-INSTR$", numInstrFrom);
		return;
	}
	
	/**
	 * Restituisce il nome della procedura interna di input, una section, 
	 * con il nome utilizzato per la codifica come sottografo interno.<br>
	 * <p>
	 * Si tratta del nome della procedura interna di input + ":" +
	 * il nome della label, se definita con thru.<br>
	 * <p>
	 * @return String procedureInputName   coded for GraphManager
	 */
	public String sortMergeGetInputProcedureIdSubGraphCalled() {
		String idProc = "";
		String idProcThru = "";
		idProc = this.sortMergeGetFromInputProcedure();
		idProc = idProc + ":";
		idProcThru = this.sortMergeGetThruInputProcedure();
		if (idProcThru != null && !idProcThru.equals("")) {
			idProc = idProc + idProcThru;
		}
		return idProc;
	}
	
	/**
	 * Restituisce il nome della procedura interna di output, una section, 
	 * con il nome utilizzato per la codifica come sottografo interno.<br>
	 * <p>
	 * Si tratta del nome della procedura interna di output + ":" +
	 * il nome della label, se definita con thru.<br>
	 * <p>
	 * @return String procedureInputName   coded for GraphManager
	 */
	public String sortMergeGetOutputProcedureIdSubGraphCalled() {
		String idProc = "";
		String idProcThru = "";
		idProc = this.sortMergeGetFromOutputProcedure();
		idProc = idProc + ":";
		idProcThru = this.sortMergeGetThruOutputProcedure();
		if (idProcThru != null && !idProcThru.equals("")) {
			idProc = idProc + idProcThru;
		}
		return idProc;
	}
	

	/**
	 * Restituisce il nome della fine della procedura di input, una section.<br>
	 * <p>
	 * @return String procedureNameThruInput
	 */
	public String sortMergeGetThruInputProcedure() {
		return (String) getMapDescriptorObject("$PROC-INPUT-THRU$");
	}

	/**
     * Imposta il nome della fine della procedura di input, una section.<br>
	 * <p>
	 * @param String procedureNameThruInput
	 */
	public void sortMergeSetThruInputProcedure(String procedureNameThru) {
		addMapDescriptorObject("$PROC-INPUT-THRU$" , procedureNameThru);
	}

	/**
	 * Restituisce il nome della fine della procedura di input, una section.<br>
	 * <p>
	 * @return String procedureNameThruInput
	 */
	public String sortMergeGetThruOutputProcedure() {
		return (String) getMapDescriptorObject("$PROC-OUTPUT-THRU$");
	}

	/**
     * Imposta il nome della fine della procedura di input, una section.<br>
	 * <p>
	 * @param String procedureNameThruInput
	 */
	public void sortMergeSetThruOutputProcedure(String procedureNameThru) {
		addMapDescriptorObject("$PROC-OUTPUT-THRU$" , procedureNameThru);
	}


	/**
	 * Restituisce il numero di istruzione label della fine della procedura di input richiamata.<br>
	 * <p>
	 * Se la procedura non è stata richiamata con thru restituiscer il numero dell'ultima istruzione
	 * della Section.<br>
	 * <p>
	 * @return int num procedureNameThru
	 */
	public int sortMergeGetThruInputProcedureNumInstr() {
		return (Integer) getMapDescriptorObject("$PROC-THRU-INPUT-INSTR$");
	}


	/**
	 * Imposta il numero di istruzione label della fine della procedura di input richiamata.<br>
	 * <p>
	 * Se la procedura non è stata richiamata con thru restituiscer il numero dell'ultima istruzione
	 * della Section.<br>
	 * <p>
	 * @param int num procedureNameThru
	 */
	public void sortMergeSetThruInputProcedureNumInstr(int numInstrThru) {
		addMapDescriptorObject("$PROC-THRU-INPUT-INSTR$", numInstrThru);
	}
	/**
	 * Restituisce il numero di istruzione label della fine della procedura di output richiamata.<br>
	 * <p>
	 * Se la procedura non è stata richiamata con thru restituiscer il numero dell'ultima istruzione
	 * della Section.<br>
	 * <p>
	 * @return int num procedureNameThru
	 */
	public int sortMergeGetThruOutputProcedureNumInstr() {
		return (Integer) getMapDescriptorObject("$PROC-THRU-OUTPUT-INSTR$");
	}


	/**
	 * Imposta il numero di istruzione label della fine della procedura di output richiamata.<br>
	 * <p>
	 * Se la procedura non è stata richiamata con thru restituiscer il numero dell'ultima istruzione
	 * della Section.<br>
	 * <p>
	 * @param int num procedureNameThru
	 */
	public void sortMergeSetThruOutputProcedureNumInstr(int numInstrThru) {
		addMapDescriptorObject("$PROC-THRU-OUTPUT-INSTR$", numInstrThru);
	}

	/**
	 * Restituisce il nome del file interno di una istruzione Sort. <br>
	 * <p>
	 * 
	 */
	public String  sortMergeGetFileNameInternal() {
		return (String) getMapDescriptorObject("$FILE$");
	}

	/**
	 * Imposta il nome del file interno di una istruzione Sort. <br>
	 * <p>
	 * 
	 */
	public void  sortMergeSetFileNameInternal(String fileName) {
		addMapDescriptorObject("$FILE$", fileName);
	}
	
	/**
	 * Restituisce i nome dei files esterni di una istruzione sort/merge. <br>
	 * <p>
	 * 
	 */
	public String[]  sortMergeGetFileNamesExternal() {
		return (String[]) getMapDescriptorObject("$DDNAMES$");
	}
	

	/**
	 * Imposta il nome dei files esterni di una istruzione sort/merge. <br>
	 * <p>
	 * 
	 */
	public void  sortMergeSetFileNamesExternal(String ar_FileNameExternal[]) {
		addMapDescriptorObject("$DDNAMES$", ar_FileNameExternal);
	}
	

}
