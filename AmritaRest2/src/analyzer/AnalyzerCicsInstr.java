package analyzer;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

import entities.EntityDynamicField;
import entities.EntityObjectOption;
import enums.EnumAmritaExceptionError;
import enums.EnumCobolReservedWords;
import enums.EnumInstrDataCategory;
import enums.EnumInstrPrecompilerType;
import enums.EnumMessageType;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import enums.EnumPrecompilerReservedWords;
import enums.EnumRelation;
import enums.EnumRelationSourceProcess;
import enums.EnumSymbolType;
import exception.ExceptionAmrita;



/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)  
 * 
 * <h1>
 * AnalyzerCics
 * </h1>
 * <p>
 * Questa classe modella tutte le funzionalità relative all'analisi dell'ambiente Cics Ibm. <br>
 * <p>
 * Tali funzionalità includono l'analisi degli statement <b>"Exec Cics"</b> e in generale di turri gli oggetti
 * collegati al Cics quali Pct, Fct, Ppt, Csd e Bms.<br>
 *  <p>
 * Questa classe eredita da {@link ExecutionShared} e pertanto ha tutte le capacità operative di
 * esecuzione, come l'accesso alla base dati.
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 31/05/2010
 * @see Instruction
 * @see AnalyzerCobolProgram
 * 
*/

public class AnalyzerCicsInstr extends ExecutionShared implements AmritaConstants {
	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza generali e usate nei processi ricorsivi                                       //                                                        //
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private AnalyzerCobol acp = null;                                
	private Map<String, ArrayList<InnerInstructionWordsEntry>> map_CicsReservedWords;	
	private LogicSamePgm logicSamePgm = null;
	private AnalyzerDbInfo analyzerDbInfo = null;
	private String programName = ""; 
	
	private DataItemCobolIdentifier dataItemIdentifier1 = null;
	private DataItemCobolIdentifier dataItemIdentifier2 = null;
	private ArrayList<ArrayList<String>> al_ValuesOperands = null;
    private EnumCobolReservedWords activeDivisionArea = null;
	
	// Reference al descrittore delle istruzioni dinamiche.
	// Contiene tutte le informazioni per ogni campo di istruzione dinamica.
	// Le informazioni sono eventualmente pronte per essere inserite su db.
	private LogicInfoDynamic logicInfoDynamic = null;

	// Sistema/sottosistema del programma sotto analisi
    private UserExitInfo userExitInfoPgm = null;

    
	/**
	 * Costruttore 
	 * 
	 * 
	 */
	public AnalyzerCicsInstr(UserConfiguration sd, ExecutionDirectives di, AnalyzerCobol acp) {
		super(sd, di);
		
        this.acp = acp;
		this.logicSamePgm = acp.getLogicSamePgm();
		this.logicInfoDynamic = acp.getLogicInfoDynamic();
		this.analyzerDbInfo = acp.getDbInfo();
		this.programName = acp.getProgramName();
		this.activeDivisionArea = EnumCobolReservedWords.NOT_ASSIGNED;
		
		// Caricamento Map parole riservate/Enum per exec cics
		this.map_CicsReservedWords = new HashMap<String, ArrayList<InnerInstructionWordsEntry>>(500);
		 	
		// Scan enumerazione con parole riservate Cics gestite
		for (EnumPrecompilerReservedWords en_reservedWord : EnumPrecompilerReservedWords.values()) {
			
			// Interessaasno solo le Exec Cics
			if (!en_reservedWord.name().startsWith("CICS")) {
				continue;
			}
			
			putMapReservedWords(en_reservedWord); // -> map_ReservedWords
		}
	} 
	

	/**
	 * Analisi istruzione precompilatore Cics. <br>
	 * <p>
	 * Viene analizzato il source dell'istruzione e valorizzata la map_Descriptor 
	 * con gli operandi e gli operatori presenti nell'Exec Cics.<br>
	 * Vengono inoltre aggiornati i simboli in input all'istruzione, in output
	 * e definiti dall'istruzione stessa (literal).
	 * <p>
	 * Nella struttturra map_Descriptor dell'istruzione vengono inserite coppie chiave/valore del tipo<br>
	 * <p>
	 *  1) <"$INSTR$>, 		EnumPrecompilerReservedWords> <br>
	 *  2) <"$OPRND$"+name,  DataItemCobolIdentifier> <br>
	 *  3) <"$OPT$"+option, String> <br> option-name/""
	 *  4) <"$COND$"+cond>, <String><br> condition-name/label
	 * <p>
	 * Nella strutture dei simboli definiti/utilizzati dall'istruzione:<br>
	 * <p>
	 *  1) Simboli in input	      : I nomi dei campi dei parametri in input all'istruzione (come FROM o INVREQ(label))<br>
	 *  2) Simboli in output	  : I nomi dei campi dei parametri in output all'istruzione (como INTO)  <br>
	 *  3) Simbili definiti inside: Le literal dei parametri dell'istruzione (como PROGRAM("Program-name"))
	 * @throws ExceptionAmrita 
	 * 
	 */
	public void analyzeCicsInstruction(InstructionCics instruction) throws ExceptionAmrita {
		
		EntityDynamicField edf = null; 
		ArrayList<DataItemCobolIdentifier> al_identifier = null;
		DataItemCobolIdentifier identifierOperand = null;
		DataItemCobolIdentifier identifierOperandKey1 = null;
		DataItemCobolIdentifier identifierOperandKey2 = null;
		InnerInstructionWordsEntry instructionsWordsEntry = null;
		ArrayList<InnerInstructionWordsEntry> al_instructionsWordsEntry = null;
		EnumPrecompilerReservedWords typeInstr = EnumPrecompilerReservedWords.NOT_ASSIGNED;
		EnumPrecompilerReservedWords typeOperandIdentifier = EnumPrecompilerReservedWords.NOT_ASSIGNED;
		EnumPrecompilerReservedWords typeOperandKey1 = EnumPrecompilerReservedWords.NOT_ASSIGNED;
		EnumPrecompilerReservedWords typeOperandKey2 = EnumPrecompilerReservedWords.NOT_ASSIGNED;
		ExceptionAmrita excp = null;
		
		String strInstr = "";
		Scanner scn = null;
		String token = "";
		String str = "";
		String key1Instr = "";
		String key2Instr = "";
		String key2Mapped = "";
		String operandOption = "";
		String operandIdentifier = "";
		String nameOperandKey1 = "";
		String nameOperandKey2 = "";
		int keySetSize = 0;
		
		// Strutture per identificatori e label
		al_identifier = new ArrayList<DataItemCobolIdentifier> ();
		
		// Recupero e normalizzo istruzione per analisi con Scanner
		strInstr = instruction.getSourceInstr();
		str = strInstr.replace("(", " ( ");
		str = str.replace(")", " ) ");
		
		scn = new Scanner(str);
		token = nextToken(scn);								// EXEC			
		
		// Istruzione incompleta o malformata
        if (!token.equals("EXEC")) {
        	instruction.setParsingError(true);
			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_PARSING_CICS_INSTRUCTION);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0008", "", excp, new String[]{});
			return;
		}
        
        token = nextToken(scn);								// CICS
       
 		// Istruzione incompleta o malformata
        if (!token.equals("CICS")) {
        	instruction.setParsingError(true);
			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_PARSING_CICS_INSTRUCTION);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0008", "", excp, new String[]{});
			return;
		}
        
		// Corpo istruzione
        token = nextToken(scn);								// LINK|WRITE|SEND|TRACE|....			 

        // Istruzione incompleta o vuota
        if (token.equals("") || token.equals("END-EXEC")) {
        	instruction.setParsingError(true);
			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_PARSING_CICS_INSTRUCTION);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0008", "", excp, new String[]{});
			return;
		}
        
        // Prima parola chiave
        key1Instr = token;
        token = nextToken(scn);								// Posizionamento su seconda keyword	 o primo operando / opzione	
        
        typeInstr = EnumPrecompilerReservedWords.NOT_ASSIGNED;
        
        // Istruzione Cics non censita
        al_instructionsWordsEntry = map_CicsReservedWords.get(key1Instr);
		if (al_instructionsWordsEntry == null) {
//			instruction.setWarning(true);
//			instruction.setInfoError(EnumMessageType.WARNING, "MW0009", "", excp, new String[]{});
			return;
 		}
		
		keySetSize = getKeySetSize(al_instructionsWordsEntry);
		
		// Istruzione Cics censita, di una sola parola chiave possibile
		if ( al_instructionsWordsEntry != null 
		&&  (keySetSize == 1  
				||
		    (keySetSize > 1 && al_instructionsWordsEntry.get(0).al_wordKey.get(0).equals("REWRITE"))
		    )
		   ) {
			instructionsWordsEntry = al_instructionsWordsEntry.get(0);
			typeInstr = instructionsWordsEntry.en_WordReservedOwner;
 		}
		
		// Istruzione Cics censita, di + parole chiave possibili e diversa da Exec Cics Rewrite
		if (al_instructionsWordsEntry != null 
		&&  keySetSize > 1 
		&& !al_instructionsWordsEntry.get(0).al_wordKey.get(0).equals("REWRITE")) {

			// Recupero seconda parola chiave
	       
	        key2Instr = token;
	        token = nextToken(scn);	    // Primo Token utile dopo key words
	        
	        // Istruzione malformata
	        if (key2Instr.equals("END-EXEC") || key2Instr.equals("")) {
	        	instruction.setParsingError(true);
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_PARSING_CICS_INSTRUCTION);
				instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0008", "", excp, new String[]{});
				return;
			}
			
			// Scan istruzioni possibili che iniziano per key1Instr
			for (InnerInstructionWordsEntry instructionWordsEntry : al_instructionsWordsEntry) {
				key2Mapped = instructionWordsEntry.al_wordKey.get(1);
				if (key2Instr.equals(key2Mapped)) {
					typeInstr = instructionWordsEntry.en_WordReservedOwner;
					break;
				} // end-if
			} // end-for
			
			// Istruzione non censita
			if (typeInstr == EnumPrecompilerReservedWords.NOT_ASSIGNED) {
//				instruction.setWarning(true);
//				instruction.setInfoError(EnumMessageType.WARNING, "MW0009", "", excp, new String[]{});
			} // end-if
			
 		} // end-if
		
		
		// Lo scanner è posizionato sulla prima word dopo quella chiave
		
		
		// Codifica del tipo istruzione Exec Cics dentro l'istruzione
		instruction.addMapDescriptorObject("$INSTR$", typeInstr);
		instruction.setTypePrecompiler(EnumInstrDataCategory.CICS_PRECOMPILER);
		instruction.setTypeInstrPrecompiler(typeInstr);		
        
		// Scan operandi e opzioni presenti nell'istruzione
		while (!token.equals("")) {
			
			if (token.equals("END-EXEC")) {
				token = nextToken(scn);
				continue;
			}
			
			al_instructionsWordsEntry = map_CicsReservedWords.get(token);
	        
	        // Operando o opzione non censito: warning non bloccante
            if (al_instructionsWordsEntry == null) {
//            	instruction.setWarning(true);
//            	instruction.setInfoError(EnumMessageType.WARNING, "MW0009", "", excp, new String[]{});
			} else {
				instructionsWordsEntry = al_instructionsWordsEntry.get(0);
			}
			
            // Operando o opzione
            operandOption = token;													// Nome operando o opzione cics
            operandIdentifier = "";													// Identificatore completo cobol
            
            token = nextToken(scn);													// Parentesi o next operando/opzione
            if (token.equals("(")) {
            	token = nextToken(scn);	
            	while (!token.equals("")) {
            		if (token.equals(")")) {
            			token = nextToken(scn);
            			break;
					}
            		operandIdentifier = operandIdentifier + " " + token;
            		token = nextToken(scn);
            	}
 			}
            
            // Eliminazione spazi a inizio e fine
            operandIdentifier = operandIdentifier.trim();
            
            // Gestione opzione comune a tutte le istruzioni
            if (operandIdentifier.equals("")) {
                instruction.addOption(operandOption);
            	continue;
 			}

            // Operando/opzione codificato
            typeOperandIdentifier = instructionsWordsEntry.en_WordReservedOwner; 	

            // Gestione label se Handle Abend/Aid/Condition/Ignore Condition
            if (typeInstr == EnumPrecompilerReservedWords.CICS_INSTR_HANDLE_ABEND
            ||  typeInstr == EnumPrecompilerReservedWords.CICS_INSTR_HANDLE_AID
            ||  typeInstr == EnumPrecompilerReservedWords.CICS_INSTR_HANDLE_CONDITION) {
            	if (typeInstr == EnumPrecompilerReservedWords.CICS_INSTR_HANDLE_ABEND
            	&&  operandOption.equals("LABEL")) {
    				instruction.addMapDescriptorObject("$LABEL$", operandIdentifier);
               		instruction.addSymbolInput(operandIdentifier, EnumSymbolType.COBOL_SYMBOL_LABEL);
                	continue;
				}
            	if (typeInstr == EnumPrecompilerReservedWords.CICS_INSTR_HANDLE_AID) {
    				instruction.addMapDescriptorObject("$AID$" + operandOption, operandIdentifier);
               		instruction.addSymbolInput(operandIdentifier, EnumSymbolType.COBOL_SYMBOL_LABEL);
                	continue;
				}
            	if (typeInstr == EnumPrecompilerReservedWords.CICS_INSTR_HANDLE_CONDITION) {
    				instruction.addMapDescriptorObject("$COND$" + operandOption, operandIdentifier);
               		instruction.addSymbolInput(operandIdentifier, EnumSymbolType.COBOL_SYMBOL_LABEL);
                	continue;
				}
 			}
            
            // Gestione operando con identificatore Cobol
        	al_identifier = acp.extractIdentifiers(instruction, operandIdentifier);
        	if (instruction.isParsingError() || instruction.isSemanticError()) {
				return;
			}
        	identifierOperand = al_identifier.get(0);
        	instruction.addOperand(operandOption, identifierOperand);
        	
         	// Operando chiave dinamico: impostazione flag istruzione
        	if (operandOption.equals("MAP")
            ||  operandOption.equals("PROGRAM")		
            ||  operandOption.equals("TRANSID")		
        	||  operandOption.equals("FILE")		
        	||  operandOption.equals("QUEUE")		
        	||  operandOption.equals("QNAME")		
        	||  operandOption.equals("ABCODE")) {
        		// Campo chiave è un campo: istruzione dinamica da risolvere
				if (identifierOperand.getIdentifierType() == EnumCobolReservedWords.DATA_DIV_DATA_ITEM) {
					instruction.setDynamic(true);
					identifierOperandKey1 = identifierOperand;
					nameOperandKey1 = operandOption;
				}
			}

           	// Operando chiave dinamico: impostazione flag istruzione
        	if (operandOption.equals("MAPSET")) {
        		// Campo chiave è un campo: istruzione dinamica da risolvere
				if (identifierOperand.getIdentifierType() == EnumCobolReservedWords.DATA_DIV_DATA_ITEM) {
					instruction.setDynamic(true);
					identifierOperandKey2 = identifierOperand;
					nameOperandKey2 = operandOption;
				}
			}
            // Inserimento nome operando come simbolo di input/output
        	if (typeOperandIdentifier.isOutputOperand()) {
        		instruction.addOperandOutput(operandOption, identifierOperand);
			} else {
				instruction.addOperandInput(operandOption, identifierOperand);
			}
		}
		
		// Istruzione dinamica: inserimento in tabella istruzioni dinamiche da risolvere  DynamicField
		//                      inserimento in istruzione operandi dinamici
		if (instruction.isDynamic()) {
			
			// Inserimento operando key1 istruzione dinamica in tabella DynamicField  
			if (identifierOperandKey1 != null) {
				// Struttura db istruzioni dinamiche
				typeOperandKey1 = getEnumOperand(nameOperandKey1);
				edf = new EntityDynamicField();
				edf.setSystem(this.di.systemInput); // Key
				edf.setSubSystem(this.di.subSystemInput);
				edf.setIdObject(this.programName);
				edf.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
				edf.setNumInstr(this.acp.getCurNumDefProcDiv());
				edf.setIdField(identifierOperandKey1.getNameIdentifier());
				edf.setNumField(identifierOperandKey1.getDataItem().getNumInstr()); // Data
				edf.setInstrCobolType(EnumCobolReservedWords.PRECOMPILER_CICS);
				edf.setInstrPrecompType(typeInstr);
				edf.setInstrPrecompOprndType(typeOperandKey1);
				this.analyzerDbInfo.addObjEntityDynamicField(edf);
				// Struttura interna istruzione dinamica
				instruction.addOperandDynamic(nameOperandKey1, identifierOperandKey1);
			}
			
			// Inserimento operando key2 istruzione dinamica in tabella DynamicField   (MAPSET)
			if (identifierOperandKey2 != null) {
				typeOperandKey2 = getEnumOperand(nameOperandKey2);
				edf = new EntityDynamicField();
				edf.setSystem(this.di.systemInput); // Key
				edf.setSubSystem(this.di.subSystemInput);
				edf.setIdObject(this.programName);
				edf.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
				edf.setNumInstr(this.acp.getCurNumDefProcDiv());
				edf.setIdField(identifierOperandKey2.getNameIdentifier());
				edf.setNumField(identifierOperandKey2.getDataItem().getNumInstr()); // Data
				edf.setInstrCobolType(EnumCobolReservedWords.PRECOMPILER_CICS);
				edf.setInstrPrecompType(typeInstr);
				edf.setInstrPrecompOprndType(typeOperandKey2);
				this.analyzerDbInfo.addObjEntityDynamicField(edf);
				// Struttura interna istruzione dinamica
				instruction.addOperandDynamic(nameOperandKey2, identifierOperandKey2);
			}
		}
	} 

	/* --------------------------------------------------------------------
	 * Restituisce il numero di possibili sequenze di key per l'istruzione
	 * --------------------------------------------------------------------
	 * 
	 * Alcune parole chiave possono essere sia istruzioni si exceptio (es. TRACE)
	 * Si conteggiano solo le sequenze di parole chiave di istruzioni
	 * 
	 */
	private int getKeySetSize(ArrayList<InnerInstructionWordsEntry> al_wordEntry) {
		int keySize = 0;
		
		// Scan possibili sequenze di chiavi
		for (InnerInstructionWordsEntry wordEntry : al_wordEntry) {
			if (wordEntry.en_WordReservedOwner.getTypeEntry() == EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION) {
				keySize++;
			}
		}
		return keySize;
	}


	/**
	 * Risolve l'istruzione Cics e determina gli aggiornamenti su db da effettuare. <br>
	 * <p>
	 * L'inserimento fisico degli oggetti, delle relazioni etc.
	 * viene effettuato in modo centralizzato a fronte dell'analisi del programma,
	 * da AnalyzerCobolProgram, dal quale questo metodo viene richiamato, o a fronte
	 * della soluzione delle logiche stesso programma o spreaded.
	 * Sfruttando il metodo di AnalyzerDbInfo prepareForDbObjectRelatedToAny(..),
	 * vengono accodati gli aggiornamenti da effettuare, che saranno eseguiti in modo
	 * cumulativo alla fine dell'analisi completa del programma.
	 * In questo metodo, per l'istruzione fornita, vengono accodate direttamente nelle
	 * strutture del chiamante AnalyzerCobolProgram, gli aggiornamenti da effettuare.
	 * <p>
	 * Questo metodo viene richiamato in fase di analisi preliminare del programma e in fase
	 * di consolidamento, ovvero di soluzione di istruzioni dinamiche non risolte in prima
	 * battuta. In questo caso il metodo viene eseguito come LOGIC_DYNAMIC_SPREADED_PGM.<br>
	 * In caso di LOGIC_DYNAMIC_SPREADED_PGM vengono in ogni caso restituiti tutti i valori
	 * dinamici inclusi quelli parziali eventualmente restituiti in elaborazioni precedenti.
	 * <p>
	 * Il processo di soluzione delle istruzioni dinamiche spreaded, effettua un'eliminazione
	 * preliminare sul data base di tutte le relazioni, le origini e altro associate al
	 * numero dell'istruzione dinamica.
	 * 
	 * 
	 * @param InstructionCics Cics instruction
	 * @param int typeLogicDynamic da AmritaConstants LOGIC_DYNAMIC_SAME_PGM o LOGIC_DYNAMIC_SPREADED_PGM
	 * @throws ExceptionAmrita 
	 * @throws SQLException 
	 */
	public void solveInstruction(
		                          InstructionCics instruction  		// Istruzione da risolvere
							    , int typeLogicDynamic			    		// LOGIC_DYNAMIC_SAME_PGM, LOGIC_DYNAMIC_SPREADED_PGM da passare a LogicManager
							    ) throws ExceptionAmrita, SQLException {
		
		
		 EntityObjectOption entityObjectOption  = null;
		
		 // Mark program come programma Cics
		 entityObjectOption = new EntityObjectOption();
		 entityObjectOption.setIdObject(this.programName);
		 entityObjectOption.setSystem(this.di.systemInput);
		 entityObjectOption.setSubSystem(this.di.subSystemInput);
		 entityObjectOption.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
		 entityObjectOption.setOption(EnumObjectOption.PGM_CICS);
		 this.analyzerDbInfo.addObjEntityOption(entityObjectOption);
         
		 // Mark program come programma Online
		 entityObjectOption = new EntityObjectOption();
		 entityObjectOption.setIdObject(this.programName);
		 entityObjectOption.setSystem(this.di.systemInput);
		 entityObjectOption.setSubSystem(this.di.subSystemInput);
		 entityObjectOption.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
		 entityObjectOption.setOption(EnumObjectOption.PGM_ONLINE);
		 this.analyzerDbInfo.addObjEntityOption(entityObjectOption);
		 
		 // Soluzione istruzione Cics
		 switch (instruction.getTypeInstrPrecompiler()) {
		
		    // Send map/text
			case CICS_INSTR_SEND:
				logicCicsSendReceiveShared(instruction, typeLogicDynamic, EnumObjectOption.PGM_CICS_WITH_SEND_MAP);
				break;
			// Receive/Map
			case CICS_INSTR_RECEIVE:
				logicCicsSendReceiveShared(instruction, typeLogicDynamic, EnumObjectOption.PGM_CICS_WITH_RECEIVE_MAP);
				break;
	
			// File control
			case CICS_INSTR_READ:
				logicCicsVsamShared(instruction, typeLogicDynamic, EnumRelation.PGM_EXTERNAL_FILE_READ, EnumRelation.PGM_ENTITY_READ);
				break;
			case CICS_INSTR_WRITE:
				logicCicsVsamShared(instruction, typeLogicDynamic, EnumRelation.PGM_EXTERNAL_FILE_INSERT,EnumRelation.PGM_ENTITY_INSERT);
				break;
			case CICS_INSTR_REWRITE:
				logicCicsVsamShared(instruction, typeLogicDynamic, EnumRelation.PGM_EXTERNAL_FILE_UPDATE, EnumRelation.PGM_ENTITY_UPDATE);
				break;
			case CICS_INSTR_DELETE:
				logicCicsVsamShared(instruction, typeLogicDynamic, EnumRelation.PGM_EXTERNAL_FILE_DELETE, EnumRelation.PGM_ENTITY_DELETE);
				break;
			case CICS_INSTR_UNLOCK:
				logicCicsVsamShared(instruction, typeLogicDynamic, null, null);
				break;
			case CICS_INSTR_STARTBR:
				logicCicsVsamShared(instruction, typeLogicDynamic, null, null);
				break;
			case CICS_INSTR_READNEXT:
				logicCicsVsamShared(instruction, typeLogicDynamic, EnumRelation.PGM_EXTERNAL_FILE_READNEXT, EnumRelation.PGM_ENTITY_READNEXT);
				break;
			case CICS_INSTR_READPREV:
				logicCicsVsamShared(instruction, typeLogicDynamic, EnumRelation.PGM_EXTERNAL_FILE_READPREV, EnumRelation.PGM_ENTITY_READPREV);
				break;
			case CICS_INSTR_RESETBR:
				logicCicsVsamShared(instruction, typeLogicDynamic, null, null);
				break;
			case CICS_INSTR_ENDBR:
				logicCicsVsamShared(instruction, typeLogicDynamic, null, null);
				break;
				
			// Temporary storage/Transient data (TS o TD)	
			case CICS_INSTR_WRITEQ:
				logicCicsWriteq(instruction, typeLogicDynamic);
				break;
			case CICS_INSTR_READQ:
				logicCicsReadq(instruction, typeLogicDynamic);
				break;
			case CICS_INSTR_DELETEQ:
				logicCicsDeleteq(instruction, typeLogicDynamic);
				break;
			
			// Program control
			case CICS_INSTR_LINK:
				logicCicsLinkXctlLoadRelease(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_LINK_PGM);
				break;
			case CICS_INSTR_XCTL:
				logicCicsLinkXctlLoadRelease(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_XCTL_PGM);
				break;
			case CICS_INSTR_LOAD:
				logicCicsLinkXctlLoadRelease(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_LOAD_PGM);
				break;
			case CICS_INSTR_RELEASE:
				logicCicsLinkXctlLoadRelease(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_RELEASE_PGM);
				break;
			case CICS_INSTR_RETURN:
				logicCicsStartReturn(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_RETURN_TRANSID);
				break;
			case CICS_INSTR_START:
				logicCicsStartReturn(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_START_TRANSID);
				break;
			
			// Abnormal termination 
			case CICS_INSTR_ABEND:
				logicCicsAbend(instruction, typeLogicDynamic);
				break;
				
		default:
			break;
		}
	} 
	
	/**
	 * 
	 * Imposta la divisione Cobol attiva
	 * 
	 * @param activeDivisionArea the activeDivisionArea to set
	 */
	public void setActiveDivisionArea(EnumCobolReservedWords activeDivisionArea) {
		this.activeDivisionArea = activeDivisionArea;
	}
	
	/**
	 * Restituisce le informzaioni di sistema/sottosistem acquisite
	 * da user exti esterna o da pilot di esecuzione.<br>
	 * <p>
	 * 
	 * @return the userExitInfoPgm
	 */
	public UserExitInfo getUserExitInfoPgm() {
		return userExitInfoPgm;
	}

	/**
	 * Imposta le informzaioni di sistema/sottosistem acquisite
	 * da user exti esterna o da pilot di esecuzione.<br>
	 * <p>
	 * 
	 * @param userExitInfoPgm the userExitInfoPgm to set
	 */
	public void setUserExitInfoPgm(UserExitInfo userExitInfoPgm) {
		this.userExitInfoPgm = userExitInfoPgm;
	}



	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Metodi privati                                        /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

	/*
	 * 
	 * Inserimento in map parole riservate Cobol, con collegamento alla EnumCobolResevedWords
	 * principale che identifica l'istruzione o direttiva o parola chiave figurativa etc.
	 * 
	 */
	private void putMapReservedWords(EnumPrecompilerReservedWords en_reservedWord) {

		ArrayList<String> al_wordKey = null;										// Parole chiave valide come inizio di istruzione
		ArrayList<InnerInstructionWordsEntry> al_instructionsWordsEntry = null;     // Mapping Insieme di sequenze valide di chiavi
		InnerInstructionWordsEntry instructionsWordsEntry = null;      				// Singola sequenza
		String keyMap = "";                                                         //
		
		al_wordKey = new ArrayList<String> ();
				
		keyMap = en_reservedWord.getValueText1();
		al_wordKey.add(keyMap);		    							// Per esempio HANDLE 
		
		if (!en_reservedWord.getValueText2().equals("")) {
			al_wordKey.add(en_reservedWord.getValueText2());		// Per esempio AID 
		}
		
		// Sequenza di parole individuante l'istruzione cics
		instructionsWordsEntry = new InnerInstructionWordsEntry();
		instructionsWordsEntry.en_WordReservedOwner = en_reservedWord;
		instructionsWordsEntry.typeEntry = en_reservedWord.getTypeEntry();
		instructionsWordsEntry.al_wordKey = al_wordKey;
		
		// Recupero insieme di sequenze valide per la parola chiave di inizio istruzione
		al_instructionsWordsEntry = this.map_CicsReservedWords.get(en_reservedWord.getValueText1());
		if (al_instructionsWordsEntry == null) {
			al_instructionsWordsEntry = new ArrayList<InnerInstructionWordsEntry> ();
		}
		
		// Aggiorno la map con le sequenze valide aggiornate
		al_instructionsWordsEntry.add(instructionsWordsEntry);
		this.map_CicsReservedWords.put(en_reservedWord.getValueText1(), al_instructionsWordsEntry);
	}
	
	/*
	 * Restituisce l'enumerazione di parola riservata di precompilatore
	 * a partire dal nome dell'operando
	 */
	private EnumPrecompilerReservedWords getEnumOperand(String nameIdentifier) {
		EnumPrecompilerReservedWords enumPrecompilerReservedWord = null;
		 
		// Scan elementi enumerazione
		for (EnumPrecompilerReservedWords elemPrecompilerReservedWord : EnumPrecompilerReservedWords.values()) {
			if (elemPrecompilerReservedWord.getValueText1().equals(nameIdentifier)) {
				return elemPrecompilerReservedWord;
			}
		}
		return enumPrecompilerReservedWord;
	}

	
	/*
	 * 
	 * Gestione Exec Cics Send
	 * Gestione Exec Cics Send Map
	 * Gestione Exec Cics Send Text
	 * Gestione Exec Cics Receive
	 * Gestione Exec Cics Receive Map
	 * 
	 */
	private void logicCicsSendReceiveShared(InstructionCics instruction, int typeLogicDynamic, EnumObjectOption optSendReceive) throws ExceptionAmrita, SQLException {
		EnumRelation typeRelation = null;
				
		// Valori estratti per Map e Mapset
	    ArrayList<String> al_Map = null;
	    ArrayList<String> al_MapSet = null;
	    
	    // Valori estratti per Form (Map+Mapset)
	    ArrayList<String> al_Form = null;

		// Operandi statici (literal) o dinamici (campi)
		EnumRelationSourceProcess typeSourceIdentifier1 = EnumRelationSourceProcess.NOT_ASSIGNED;
		EnumRelationSourceProcess typeSourceIdentifier2 = EnumRelationSourceProcess.NOT_ASSIGNED;
		
	    // Gestione opzioni
	    EntityObjectOption entityObjectOption = null;

		// Inizializzazione strutture
		al_Map = new ArrayList<String> ();
		al_MapSet = new ArrayList<String> ();
		al_Form = new ArrayList<String>();
		
		
		// Istruzione statica risolta.
		if (instruction.isStaticSolved()) {
			return;
		}

		// Istruzione dinamica completamente risolta
		if (instruction.isDynamic() 
		&&  instruction.isDynamicSolvedFull()) {
			return;
		}
		
		// Istruzione ancora da risolvere, anche solo parzialmente
		
		// Send Text: inserimento della sola opzione di programma
        if (instruction.isThereOption("TEXT")) {
        	entityObjectOption = new EntityObjectOption();
			entityObjectOption.setSystem(this.userExitInfoPgm.getSystem());
			entityObjectOption.setSubSystem(this.userExitInfoPgm.getSubSystem());
    		entityObjectOption.setIdObject(this.programName);
    		entityObjectOption.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
    		entityObjectOption.setOption(EnumObjectOption.PGM_WITH_I_O_SCREEN);
    		this.analyzerDbInfo.addObjEntityOption(entityObjectOption);
            return;	
		}
		
		// Estrazione operando Map
		dataItemIdentifier1 = (DataItemCobolIdentifier) instruction.getOperand("MAP");
				
		// Non è Send/Receive Map
		if (dataItemIdentifier1 == null) {
			return;
		}
		
		// Impostazione tipologia sorgente processo soluzione dinamica per Map
		if (dataItemIdentifier1.getIdentifierType() ==  EnumCobolReservedWords.DATA_DIV_DATA_ITEM) {
			typeSourceIdentifier1 = EnumRelationSourceProcess.SOURCE_DYNAMIC_LOCAL;
		}  else {
			typeSourceIdentifier1 = EnumRelationSourceProcess.SOURCE_STATIC_LOCAL;
		}
		
		// Send map: verifico se c'è il Mapset, che è opzionale
		dataItemIdentifier2 = (DataItemCobolIdentifier) instruction.getOperand("MAPSET");
		
		if (dataItemIdentifier2 != null) {
			// Informazioni su tipologia sorgente processo MapSet
			if (dataItemIdentifier2.getIdentifierType() ==  EnumCobolReservedWords.DATA_DIV_DATA_ITEM) {
				typeSourceIdentifier2 = EnumRelationSourceProcess.SOURCE_DYNAMIC_LOCAL;
			}  else {
				typeSourceIdentifier2 = EnumRelationSourceProcess.SOURCE_STATIC_LOCAL;
			}
		}

		// Attivazione LogicSamePgm se istruzione dinamica o recupero e combinazione valori statici
		al_ValuesOperands = logicCicsSendReceiveSharedGetValues(instruction, dataItemIdentifier1, dataItemIdentifier2, typeLogicDynamic);
		
		// Istruzione dinamica: nessun valore individuato o recupero valori disabilitato
		if (al_ValuesOperands == null 
		|| al_ValuesOperands.get(0).size() == 0
		|| al_ValuesOperands.get(1).size() == 0) {
			return;
		}
		
		// Specifici valori da relazionare
		al_Map = al_ValuesOperands.get(0);
		al_MapSet = al_ValuesOperands.get(1);
		al_Form = al_ValuesOperands.get(2);
		
		// Inserimento oggetti Map e relazioni Pgm-Map con relativa origine
		for (String map : al_Map) {
			
			// Recupera inserisce in struttura da sysSubSys corr/Owner/New
			this.analyzerDbInfo.getAddObjectEntitled(map, EnumObject.OBJECT_CICS_MAP);

			if (instruction.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.CICS_INSTR_SEND) {
				typeRelation = EnumRelation.PGM_CICS_SEND_MAP;
			} else {
				typeRelation = EnumRelation.PGM_CICS_RECEIVE_MAP;
			}
			
			this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
								                    EnumObject.OBJECT_PGM_COBOL
								                  , this.programName
								                  , EnumObject.OBJECT_CICS_MAP
												  , map
												  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
												  , typeRelation
												  , typeSourceIdentifier1
												  , this.programName
												  , instruction.getNumInstr()
												  , instruction.getRowStartSource()
												  , instruction.getRowEndSource()
												  , ""
												  , 0
												  , 0
												  , instruction.getTypeInstrCategory()
												  , this.activeDivisionArea
												  , false
												  , false
												  , false
												  , this.userExitInfoPgm
												   );
		}

		// Inserimento oggetti MapSet e relazioni Pgm-MapSet con relativa origine
		for (String mapSet : al_MapSet) {

			// Recupera inserisce in struttura da sysSubSys corr/Owner/New
			this.analyzerDbInfo.getAddObjectEntitled(mapSet, EnumObject.OBJECT_CICS_MAPSET);

			this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
								                    EnumObject.OBJECT_PGM_COBOL
								                  , this.programName
								                  , EnumObject.OBJECT_CICS_MAPSET
												  , mapSet
												  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
												  , EnumRelation.PGM_CICS_MAPSET
												  , typeSourceIdentifier2
												  , this.programName
												  , instruction.getNumInstr()
												  , instruction.getRowStartSource()
												  , instruction.getRowEndSource()
												  , ""
												  , 0
												  , 0
												  , instruction.getTypeInstrCategory()
												  , this.activeDivisionArea
												  , false
												  , false
												  , false
												  , this.userExitInfoPgm
												   );
		}
		
		// Inserimento oggetti Form e relazioni Pgm-Form con relativa origine
		for (String formName : al_Form) {
			
			// A causa della moltiplicazione dei valori di map e mapset, potrebbe generarsi qualche combinazione inesistente
			// Tipicamente in codice altamente generalizzato. Poichè si presume le mappe BMS siano già state analizzate, generando
			// anche gli oggetti FORM, si scarta la combinazione se il FORM non è definito
			if (this.analyzerDbInfo.getObjectOwner(EnumObject.OBJECT_CICS_FORM, formName, this.userExitInfoPgm.getSystem(), this.userExitInfoPgm.getSubSystem(), false, false) == null) {
				continue;
			}			
			
			// Recupera inserisce in struttura da sysSubSys corr/Owner/New
			this.analyzerDbInfo.getAddObjectEntitled(formName, EnumObject.OBJECT_CICS_FORM);

			this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
								                    EnumObject.OBJECT_PGM_COBOL
								                  , this.programName
								                  , EnumObject.OBJECT_CICS_FORM
												  , formName
												  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
												  , EnumRelation.PGM_CICS_FORM
												  , typeSourceIdentifier2
												  , this.programName
												  , instruction.getNumInstr()
												  , instruction.getRowStartSource()
												  , instruction.getRowEndSource()
												  , ""
												  , 0
												  , 0
												  , instruction.getTypeInstrCategory()
												  , this.activeDivisionArea
												  , false
												  , false
												  , false
												  , this.userExitInfoPgm
												   );
		}
		
		// Marcatura istruzione dinamica come risolta se tutti gli operandi dinamici sono completamente risolti
		// E' possibile che pur essendo stati prodotti dei valori per ogni opeando, alcuni path presentino ancora
		// delle impostazioni da risolvere. In ogni caso si inseriscono le relazioni risolte.
		
		// Trovati valori per tutti gli operandi
		if (al_Map.size() > 0 
			&& al_MapSet.size() > 0 
			&& instruction.isDynamic()) {
				instruction.setDynamicSolvedFull(true);
		}
			
		// Istruzione statica risolta
		if (!instruction.isDynamic()) {
			instruction.setStaticSolved(true);
		}

		// Valori individuati già inseriti in strutture db in logicCicsSendReceiveSharedGetValues(...)
		
		// Inserimento in map descriptor istruzione valori trovati per ogni operando (incluso quello dummy FORM)
		instruction.setDynamicOperandValues("MAP", al_Map);
		instruction.setDynamicOperandValues("MAPSET", al_MapSet);
		instruction.setDynamicOperandValues("FORM", al_Form);
        
		// Inserimento opzioni di programma
		
		// I-O a terminale generico
		addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_SCREEN);

		// Programma con Send o Receive Map
		addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, optSendReceive);
		return;
	}
	

	
	
	
	
	/*
	 * Gestione attivazione LogicManager per recupero valori in caso di istruzione dinamica.
	 * 
	 * In caso di istruzione statica vengono composti i valori e restituiti al chiamante.
	 * 
	 */
	private ArrayList<ArrayList<String>> logicCicsSendReceiveSharedGetValues(
																			  InstructionCics instruction
																			, DataItemCobolIdentifier dataItemIdentifierMap
																			, DataItemCobolIdentifier dataItemIdentifierMapSet
																			, int typeLogicDynamic
																			) throws ExceptionAmrita, SQLException {

		LogicDynamicInstruction logicDynamicInstr = null; 	// Codifica istruzione dinamica per risoluzione valori		
	    ArrayList<String> al_Map = null;					// Valori estratti per Map  
	    ArrayList<String> al_MapSet = null;					// Valori estratti per MapSet
	    ArrayList<String> al_Form = null;					// Valori estratti per Form (Map-Mapset)
        String mapName = "";								// Di servizio
        String mapSetName = "";								// Di servizio
        String formName = "";								// Di servizio

	    // Cics solving non abilitato
        if (!this.di.optSolveCodeCics) {
			return null;
		}
	    
    	al_Map = new ArrayList<String> ();
    	al_MapSet = new ArrayList<String> ();
    	al_Form = new ArrayList<String> ();

		// Impostazioni per LogicInfoDynamic 
        logicDynamicInstr = this.logicInfoDynamic.getDynamicInstr(instruction.getNumInstr());
		if (logicDynamicInstr == null) {
			logicDynamicInstr = new LogicDynamicInstruction();
			logicDynamicInstr.programName = this.programName;
			logicDynamicInstr.setDynamicInstr(instruction);
		}

    	al_ValuesOperands = new ArrayList<ArrayList<String>> ();
   	
	    // Map espresso da literal	
	    if(dataItemIdentifierMap.getIdentifierType() == EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
	    	mapName = dataItemIdentifierMap.getNameIdentifierFormatted();
	    	al_Map.add(mapName);	    
		} else {
			instruction.setDynamic(true);
			// Map espresso da campo di cui trovare i valori nel programma	
			if (di.optSolveDynamicLocal) {
				// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
				if (typeLogicDynamic == LOGIC_DYNAMIC_SAME_PGM) {
					addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE);
					// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
					al_Map = this.logicSamePgm.dynamicValues(acp.getProgramCobol(), instruction, dataItemIdentifierMap);
					logicDynamicInstr.al_dynamicField.add(this.logicSamePgm.getDynamicFieldToSolve());			
				}
				// Verifica se valori resi disponibili dal processo CHIAMANTE logic spreaded 
				if (typeLogicDynamic == LOGIC_DYNAMIC_SPREADED_PGM) {
					addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SPREAD);
					if (this.logicInfoDynamic.isDynamicFieldSpreaded(instruction.getNumInstr(), dataItemIdentifierMap.getNameIdentifier())) {
						al_Map = this.logicInfoDynamic.getDynamicFieldSpreadedValues(instruction.getNumInstr(), dataItemIdentifierMap.getNameIdentifier());
					}
				}
			}
		}
	    
	    // Mapset non specificato, si assumono gli stessi valori di map
	    if (al_Map.size() > 0 &&  dataItemIdentifierMapSet == null) {
	    	al_MapSet.addAll(al_Map);
	    } else {
		    // Mapset espresso da literal	
		    if(dataItemIdentifierMapSet.getIdentifierType() == EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
		    	mapSetName = dataItemIdentifierMapSet.getNameIdentifierFormatted();
		    	al_MapSet.add(mapSetName);	    	
			} else {
				instruction.setDynamic(true);
				// Mapset espresso da campo 
				if (di.optSolveDynamicLocal ) {
					// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
					if (typeLogicDynamic == LOGIC_DYNAMIC_SAME_PGM) {	
						addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE);
						al_MapSet = this.logicSamePgm.dynamicValues(acp.getProgramCobol(), instruction, dataItemIdentifierMapSet);
						logicDynamicInstr.al_dynamicField.add(this.logicSamePgm.getDynamicFieldToSolve());			
					}
					// Verifica se valori resi disponibili dal processo CHIAMANTE logic spreaded 
					if (typeLogicDynamic == LOGIC_DYNAMIC_SPREADED_PGM) {
						addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SPREAD);
						if (this.logicInfoDynamic.isDynamicFieldSpreaded(instruction.getNumInstr(), dataItemIdentifierMap.getNameIdentifier())) {
							al_MapSet = this.logicInfoDynamic.getDynamicFieldSpreadedValues(instruction.getNumInstr(), dataItemIdentifierMap.getNameIdentifier());
						}
					}
				}
			}
		}

	    // Aggiornamento struttura info dinamiche e strutture per update db finale 
	    // In questo punto in quanto possono essere presenti 2 parametri (Map/Mapset)
	    if (typeLogicDynamic == LOGIC_DYNAMIC_SAME_PGM) {				
			this.logicInfoDynamic.putDatabaseInfoAll(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifierMap.getNameIdentifier());			
	    }
	    
	    // VALORI PER Map e Mapset individuati	
	    if(al_Map.size() > 0 && al_MapSet.size() > 0) {
	    	// Map o Mapset espressi da campo
	    	if (dataItemIdentifierMap.getIdentifierType() != EnumCobolReservedWords.OPERAND_LITERAL_ALPHA
	    	||  dataItemIdentifierMapSet.getIdentifierType() != EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
	    		if (typeLogicDynamic == LOGIC_DYNAMIC_SAME_PGM) {	
		    		this.logicInfoDynamic.addDynamicInstr(instruction, logicDynamicInstr);
	    		}
			}
			// Moltiplico i valori di map e mapset
			for (String map : al_Map) {				
				for (String mapSet : al_MapSet) {
					formName = map.trim() + "-" + mapSet.trim();
					al_Form.add(formName);					
				} // end-for
				
			} // end-for

	    	al_ValuesOperands.add(al_Map);
	    	al_ValuesOperands.add(al_MapSet);
	    	al_ValuesOperands.add(al_Form);
	    	
	    	// Update stato istruzione
	    	if  (dataItemIdentifierMap.getIdentifierType() != EnumCobolReservedWords.OPERAND_LITERAL_ALPHA && al_Map.size() > 0) {	
				if ((dataItemIdentifierMapSet != null && dataItemIdentifierMapSet.getIdentifierType() != EnumCobolReservedWords.OPERAND_LITERAL_ALPHA)
				||  (dataItemIdentifierMapSet == null) ) {
			    	instruction.setDynamicSolvedFull(true);	    		
			    	instruction.setDynamicSolved(true);					
				}
			}
	    	return al_ValuesOperands;
		}
	        
		return null;
	}


	/*
	 * Inserimento option per cvodice dinamico spreaded
	 */
	private void addDbOptionForThePgm(EnumObject typeObject, EnumObjectOption option) {
		EntityObjectOption entityObjectOption = new EntityObjectOption();
		entityObjectOption.setSystem(acp.getProgramCobol().sysOwner);
		entityObjectOption.setSubSystem(acp.getProgramCobol().subSysOwner);
		entityObjectOption.setIdObject(acp.getProgramCobol().programName);
		entityObjectOption.setTypeObject(typeObject);
		entityObjectOption.setOption(option);
		analyzerDbInfo.addObjEntityOption(entityObjectOption);
	}


	/*
	 * 
	 * Gestione Exec Cics Read
	 * Gestione Exec Cics ReadNext
	 * Gestione Exec Cics ReadPrev
	 * Gestione Exec Cics Startbr
	 * Gestione Exec Cics Endbr
	 * Gestione Exec Cics Resetbr
	 * Gestione Exec Cics Unlock
	 * Gestione Exec Cics Write
	 * Gestione Exec Cics Rewrite
	 * Gestione Exec Cics Delete
	 * 
	 */
	private void  logicCicsVsamShared(InstructionCics instruction, int typeLogicDynamic, EnumRelation typeRelation, EnumRelation typeRelationEntity) throws ExceptionAmrita, SQLException {

		LogicDynamicInstruction logicDynamicInstr = null; 		// Codifica istruzione dinamica per risoluzione valori		
		ExceptionAmrita excp = null;
		EntityObjectOption entityObjectOption = null;
	    ArrayList<String> al_FileEntity = null;  				// Valori estratti per FILE
		EnumRelationSourceProcess typeSourceProcess = null;  	// Operando File statico (literal) o dinamico (campo)
		DataItemCobolIdentifier ioarea = null;                  // Ioarea per read ... write, rewrite
		String fileName = "";
		boolean isFileWithBinaryFields = false;
		int pointerIoarea = 0;
		
	    // Cics solving non abilitato
        if (!this.di.optSolveCodeCics) {
			return;
		}
	    		
		// Istruzione statica già risolta.
		if (instruction.isStaticSolved()) {
			return;
		}
		
		// Istruzione dinamica, local o spreaded già completamente risolta
		if (instruction.isDynamic() && instruction.isDynamicSolvedFull()) {
			return;
		}
		
		// Istruzione ancora da risolvere, anche parzialmente

		// Inizializzazione strutture e varie
		al_FileEntity = new ArrayList<String> ();

		// Estrazione operando File
		dataItemIdentifier1 = instruction.getOperand("FILE");
		
		if (dataItemIdentifier1 == null) {
			instruction.setParsingError(true);
			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_PARSING_CICS_INSTRUCTION);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "EI0027", "", excp, new String[]{"FILE", ""});
			return;
		}

		// Nome file espresso da literal
		if (this.dataItemIdentifier1.getIdentifierType() ==  EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
			typeSourceProcess = EnumRelationSourceProcess.SOURCE_STATIC_LOCAL;
			fileName = dataItemIdentifier1.getValueStringFormatted();
			al_FileEntity.add(fileName);
			instruction.setStaticSolved(true);
		} else {
			typeSourceProcess = EnumRelationSourceProcess.SOURCE_DYNAMIC;
			addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE);
			// Nome file espresso da campo dinamico 
			if (di.optSolveDynamicLocal) {
				// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
				if (typeLogicDynamic == LOGIC_DYNAMIC_SAME_PGM) {
					// Impostazioni per LogicInfoDynamic 
			        logicDynamicInstr = this.logicInfoDynamic.getDynamicInstr(instruction.getNumInstr());
					if (logicDynamicInstr == null) {
						logicDynamicInstr = new LogicDynamicInstruction();
						logicDynamicInstr.setDynamicInstr(instruction);
					}
					// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
					al_FileEntity = this.logicSamePgm.dynamicValues(acp.getProgramCobol(), instruction, dataItemIdentifier1);
					logicDynamicInstr.al_dynamicField.add(this.logicSamePgm.getDynamicFieldToSolve());			
					// Aggiornamento struttura info dinamiche e strutture per update db finale 
					this.logicInfoDynamic.putDatabaseInfoAll(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifier1.getNameIdentifier());			
				}
				// Verifica se valori resi disponibili dal processo CHIAMANTE logic spreaded 
				if (typeLogicDynamic == LOGIC_DYNAMIC_SPREADED_PGM) {
					addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SPREAD);
					if (this.logicInfoDynamic.isDynamicFieldSpreaded(instruction.getNumInstr(), dataItemIdentifier1.getNameIdentifier())) {
						al_FileEntity = this.logicInfoDynamic.getDynamicFieldSpreadedValues(instruction.getNumInstr(), dataItemIdentifier1.getNameIdentifier());
						this.logicInfoDynamic.putDatabaseInfoValuesOnly(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifier1.getNameIdentifier());
						instruction.setDynamicSpreaded(true);
					}
				}
				if (al_FileEntity.size() > 0) {
					instruction.setDynamicSolved(true);
					instruction.setDynamicSolvedFull(true);
				}
			}
		}
	
		// Istruzione dinamica: nessun valore individuato o recupero valori disabilitato
		if (al_FileEntity == null || al_FileEntity.size() == 0 ) {
			return;
		}
		
		// Valori individuati per l'operando File e già inseriti in struttura per db da logicInfoDynamic.putDatabaseInfo(..)
		instruction.setDynamicSolvedFull(true);
		
		// Inserimento valori in map descriptor istruzione per il parametro FILE
        instruction.setDynamicOperandValues("FILE", al_FileEntity);
		
       
		// Estrazione operando INTO o FROM come ioarea
		ioarea = instruction.getOperand("INTO");
		if (ioarea == null) {
			ioarea = instruction.getOperand("FROM");
		}

		// Ioarea presente.
		// Inserimento opzione di programma con files con dati binari se necessario
		if (ioarea != null) {
		  	pointerIoarea = ioarea.getNumInstr();
		    if (this.acp.getProgramCobol().isDataItemGroupWithBynaryFields(pointerIoarea)) {
		    	isFileWithBinaryFields = true;
		    	addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_BINARY_FIELDS_FILES);
			}
		}
		      
		// Inserimento oggetti External File e relazioni Pgm-External_File con relativa origine
		for (String file : al_FileEntity) {

			// Recupera inserisce in struttura da sysSubSys corr/Owner/New
			this.analyzerDbInfo.getAddObjectEntitled(file, EnumObject.OBJECT_EXTERNAL_FILE);
						
			// Relazione generale fra programma e file esterno Vsam (nel jcl Cics)
			this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
								                    EnumObject.OBJECT_PGM_COBOL
								                  , this.programName
								                  , EnumObject.OBJECT_EXTERNAL_FILE
												  , file
												  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
												  , EnumRelation.PGM_EXTERNAL_FILE
												  , typeSourceProcess
												  , this.programName
												  , instruction.getNumInstr()
												  , instruction.getRowStartSource()
												  , instruction.getRowEndSource()
												  , ""
												  , 0
												  , 0
												  , instruction.getTypeInstrCategory()
												  , this.activeDivisionArea
												  , false
												  , false 
												  , false
												  , this.userExitInfoPgm
												   );
			

			// Relazione a fronte di campi binari con oggetto external file
			if (isFileWithBinaryFields) {
				this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
	                    EnumObject.OBJECT_PGM_COBOL
	                  , this.programName
	                  , EnumObject.OBJECT_EXTERNAL_FILE
					  , file
					  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
					  , EnumRelation.PGM_EXTERNAL_FILE_BYNARY_FIELDS
					  , typeSourceProcess
					  , this.programName
					  , instruction.getNumInstr()
					  , instruction.getRowStartSource()
					  , instruction.getRowEndSource()
					  , ""
					  , 0
					  , 0
					  , instruction.getTypeInstrCategory()
					  , this.activeDivisionArea
					  , false
					  , false  
					  , false
					  , this.userExitInfoPgm
				   );
			}
			
			// Relazione specifica in base al tipo di accesso (READNEXT, READPREV, ... )
			if (typeRelation != null) {
				this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
	                    EnumObject.OBJECT_PGM_COBOL
	                  , this.programName
	                  , EnumObject.OBJECT_EXTERNAL_FILE
					  , file
					  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
					  , typeRelation
					  , typeSourceProcess
					  , this.programName
					  , instruction.getNumInstr()
					  , instruction.getRowStartSource()
					  , instruction.getRowEndSource()
					  , ""
					  , 0
					  , 0
					  , instruction.getTypeInstrCategory()
					  , this.activeDivisionArea
					  , false
					  , true										// Opzione PGM_WITH_I_O_VSAM
					  , false
					  , this.userExitInfoPgm
				   );
			
			}
		}	
		// Inserimento oggetti Entity e relazioni PGM_ENTITY_READ, PGM_ENTITY_READNEXT, ... , PGM_ENTITY_INSERT
		// I file Vsam visti dal Cics sono trattati come entity con nome coincidente con il nome interno (ed esterno)
		for (String entity : al_FileEntity) {

			// Recupera inserisce in struttura da sysSubSys corr/Owner/New
			this.analyzerDbInfo.getAddObjectEntitled(entity, EnumObject.OBJECT_ENTITY_VSAM);
						
			// Relazione generale fra programma e Entity
			this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
								                    EnumObject.OBJECT_PGM_COBOL
								                  , this.programName
								                  , EnumObject.OBJECT_ENTITY_VSAM
												  , entity
												  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
												  , EnumRelation.PGM_ENTITY
												  , typeSourceProcess
												  , this.programName
												  , instruction.getNumInstr()
												  , instruction.getRowStartSource()
												  , instruction.getRowEndSource()
												  , ""
												  , 0
												  , 0
												  , instruction.getTypeInstrCategory()
												  , this.activeDivisionArea
												  , false
												  , false 
												  , false
												  , this.userExitInfoPgm
											   );
			

			// Relazione specifica in base al tipo di accesso (READNEXT, READPREV, ... )
			if (typeRelation != null) {
				this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
	                    EnumObject.OBJECT_PGM_COBOL
	                  , this.programName
	                  , EnumObject.OBJECT_ENTITY_VSAM
					  , entity
					  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
					  , typeRelationEntity
					  , typeSourceProcess
					  , this.programName
					  , instruction.getNumInstr()
					  , instruction.getRowStartSource()
					  , instruction.getRowEndSource()
					  , ""
					  , 0
					  , 0
					  , instruction.getTypeInstrCategory()
					  , this.activeDivisionArea
					  , false
					  , true										// Opzione PGM_WITH_I_O_VSAM
					  , false
					  , this.userExitInfoPgm
			   );
			}
			
			// Relazione a fronte di campi binari con oggetto entity + opzioni di programma
			if (isFileWithBinaryFields) {
				// Relazione
				this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
	                    EnumObject.OBJECT_PGM_COBOL
	                  , this.programName
	                  , EnumObject.OBJECT_ENTITY_VSAM
					  , entity
					  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
					  , EnumRelation.PGM_ENTITY_FILE_BYNARY_FIELDS
					  , typeSourceProcess
					  , this.programName
					  , instruction.getNumInstr()
					  , instruction.getRowStartSource()
					  , instruction.getRowEndSource()
					  , ""
					  , 0
					  , 0
					  , instruction.getTypeInstrCategory()
					  , this.activeDivisionArea
					  , false
					  , false      
					  , false
					  , this.userExitInfoPgm
					   );
				
				// Opzione
				entityObjectOption = new EntityObjectOption();
				entityObjectOption.setSystem(this.userExitInfoPgm.getSystem());
				entityObjectOption.setSubSystem(this.userExitInfoPgm.getSubSystem());
				entityObjectOption.setIdObject(entity);
				entityObjectOption.setTypeObject(EnumObject.OBJECT_ENTITY_VSAM);
				entityObjectOption.setOption(EnumObjectOption.ENTITY_WITH_BINARY_FIELDS);
				analyzerDbInfo.addObjEntityOption(entityObjectOption);
			}
		}
								
		// Inserimento opzioni di programma
		
		// Programma con I-O Vsam
		addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_I_O_VSAM);
		return;
		
	}
	
 	/*
	 * 
	 * Gestione Exec Cics Writeq
	 * 
	 */
	private void  logicCicsWriteq(InstructionCics instruction, int typeLogicDynamic) throws ExceptionAmrita, SQLException {
		
		if (instruction.isThereOption("TS")) {
			if (instruction.isThereOption("REWRITE")) {
				logicCicsWriteqReadqDeletqShared(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_TS_QUEUE_REWRITE, EnumObject.OBJECT_CICS_TS_QUEUE);
			} else {
				logicCicsWriteqReadqDeletqShared(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_TD_QUEUE_WRITE, EnumObject.OBJECT_CICS_TS_QUEUE);
			}
		} else {
			logicCicsWriteqReadqDeletqShared(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_TD_QUEUE_WRITE, EnumObject.OBJECT_CICS_TD_QUEUE);
		}
		return;
		
	}
	/*
	 * 
	 * Gestione Exec Cics Readq
	 * 
	 */
	private void  logicCicsReadq(InstructionCics instruction, int typeLogicDynamic) throws ExceptionAmrita, SQLException {
		
		if (instruction.isThereOption("TS")) {
			logicCicsWriteqReadqDeletqShared(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_TS_QUEUE_READ, EnumObject.OBJECT_CICS_TS_QUEUE);
		} else {
			logicCicsWriteqReadqDeletqShared(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_TD_QUEUE_READ, EnumObject.OBJECT_CICS_TD_QUEUE);
		}
		return;
		
	}
	/*
	 * 
	 * Gestione Exec Cics Deleteq
	 * 
	 */
	private void  logicCicsDeleteq(InstructionCics instruction, int typeLogicDynamic) throws ExceptionAmrita, SQLException {
		
		if (instruction.isThereOption("TS")) {
			logicCicsWriteqReadqDeletqShared(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_TS_QUEUE_DELETE, EnumObject.OBJECT_CICS_TS_QUEUE);
		} else {
			logicCicsWriteqReadqDeletqShared(instruction, typeLogicDynamic, EnumRelation.PGM_CICS_TD_QUEUE_DELETE, EnumObject.OBJECT_CICS_TD_QUEUE);
		}
		
		return;
		
	}


	/*
	 * 
	 * Gestione Exec Cics Readq
	 * Gestione Exec Cics Writeq
	 * Gestione Exec Cics Deleteq
	 * 
	 */
	private void  logicCicsWriteqReadqDeletqShared(
													  InstructionCics instruction
													, int typeLogicDynamic
													, EnumRelation typeRelation
													, EnumObject typeObjectTsTd
													) throws ExceptionAmrita, SQLException {
		
		
		LogicDynamicInstruction logicDynamicInstr = null; 		// Codifica istruzione dinamica per risoluzione valori		
		ExceptionAmrita excp = null;
		ArrayList<String> al_Queue = null;  					// Valori estratti perQueue
		EnumRelationSourceProcess typeSourceProcess = null;  	// Operando File statico (literal) o dinamico (campo)
		String queueName = "";                                  // 
				
	    // Cics solving non abilitato
        if (!this.di.optSolveCodeCics) {
			return;
		}
	    		
		// Istruzione statica già risolta.
		if (instruction.isStaticSolved()) {
			return;
		}
		
		// Istruzione dinamica già completamente risolta
		if (instruction.isDynamic() && instruction.isDynamicSolvedFull()) {
			return;
		}
		
		// Inizializzazione strutture
		al_Queue = new ArrayList<String> ();
		
		// Impostazioni per LogicInfoDynamic 
        logicDynamicInstr = this.logicInfoDynamic.getDynamicInstr(instruction.getNumInstr());
		if (logicDynamicInstr == null) {
			logicDynamicInstr = new LogicDynamicInstruction();
			logicDynamicInstr.setDynamicInstr(instruction);
		}
		
		// Estrazione operando QUEUE O QNAME
		dataItemIdentifier1 = (DataItemCobolIdentifier) instruction.getOperand("QUEUE");
		if (dataItemIdentifier1 == null) {
			dataItemIdentifier1 = (DataItemCobolIdentifier) instruction.getOperand("QNAME");
		}		

		// Deve essere valorizzato
		if (dataItemIdentifier1 == null) {
			instruction.setParsingError(true);
			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_PARSING_CICS_INSTRUCTION);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "EI0027", "", excp, new String[]{"QUEUE", ""});
			return;
		}

		// Queue espresso da literal
		if (this.dataItemIdentifier1.getIdentifierType() ==  EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
			typeSourceProcess = EnumRelationSourceProcess.SOURCE_STATIC_LOCAL;
			queueName = dataItemIdentifier1.getValueStringFormatted();
			al_Queue.add(queueName);
			instruction.setStaticSolved(true);
		} else {
			typeSourceProcess = EnumRelationSourceProcess.SOURCE_DYNAMIC;
			// Queue espresso da campo dinamico
			if (di.optSolveDynamicLocal) {
				// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
				if (typeLogicDynamic == LOGIC_DYNAMIC_SAME_PGM) {
					addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE);
					// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
					al_Queue = this.logicSamePgm.dynamicValues(acp.getProgramCobol(), instruction, dataItemIdentifier1);
					logicDynamicInstr.al_dynamicField.add(this.logicSamePgm.getDynamicFieldToSolve());			
					// Aggiornamento struttura info dinamiche e strutture per update db finale 
					this.logicInfoDynamic.putDatabaseInfoAll(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifier1.getNameIdentifier());			
				}
				// Verifica se valori resi disponibili dal processo CHIAMANTE logic spreaded 
				if (typeLogicDynamic == LOGIC_DYNAMIC_SPREADED_PGM) {
					addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SPREAD);
					if (this.logicInfoDynamic.isDynamicFieldSpreaded(instruction.getNumInstr(), dataItemIdentifier1.getNameIdentifier())) {
						al_Queue = this.logicInfoDynamic.getDynamicFieldSpreadedValues(instruction.getNumInstr(), dataItemIdentifier1.getNameIdentifier());
						this.logicInfoDynamic.putDatabaseInfoValuesOnly(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifier1.getNameIdentifier());	
					}
				}
			}
		}
		
		// Istruzione dinamica: nessun valore individuato o recupero valori disabilitato
		if (al_Queue == null || al_Queue.size() == 0) {
			return;
		}
		
		// Valori individuati per l'operando Queue e già inseriti in struttura per db da logicInfoDynamic.putDatabaseInfo(..)
	
		// Inserimento valori in map descriptor istruzione per il parametro QUEUE/QNAME
		instruction.setDynamicOperandValues("QUEUE", al_Queue);
		instruction.setDynamicSolved(true);
		instruction.setDynamicSolvedFull(true);
		
		// Inserimento oggetti Internal File e relazioni Pgm-Internal_File con relativa origine
		for (String queue : al_Queue) {

			// Recupera inserisce in struttura da sysSubSys corr/Owner/New
			this.analyzerDbInfo.getAddObjectEntitled(queue, typeObjectTsTd);
						
			// Relazione generale fra programma e file interno
			this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
								                    EnumObject.OBJECT_PGM_COBOL
								                  , this.programName
								                  , typeObjectTsTd
												  , queue
												  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
												  , typeRelation
												  , typeSourceProcess
												  , this.programName
												  , instruction.getNumInstr()
												  , instruction.getRowStartSource()
												  , instruction.getRowEndSource()
												  , ""
												  , 0
												  , 0
												  , instruction.getTypeInstrCategory()
												  , this.activeDivisionArea
												  , false
												  , false 
												  , false
												  , this.userExitInfoPgm
											   );
			
		}

		return;
		
	}

			
	/*
	 * 
	 * Gestione Exec Cics Link
	 * Gestione Exec Cics Xctl
	 * Gestione Exec Cics Load
	 * Gestione Exec Cics Release
	 * 
	 * Viene gestito il parametro Program()
	 * 
	 * 
	 */
	private void  logicCicsLinkXctlLoadRelease(InstructionCics instruction, int typeLogicDynamic, EnumRelation typeRelation) throws ExceptionAmrita, SQLException {
		
		LogicDynamicInstruction logicDynamicInstr = null; 		// Codifica istruzione dinamica per risoluzione valori		
		ExceptionAmrita excp = null;
	    ArrayList<String> al_Program = null;  					// Valori estratti per PROGRAM
		EnumRelationSourceProcess typeSourceProcess = null;     // Operando File statico (literal) o dinamico (campo)
	    String programName = "";
		
	    // Cics solving non abilitato
        if (!this.di.optSolveCodeCics) {
			return;
		}
	    		
		// Istruzione statica già risolta.
		if (instruction.isStaticSolved()) {
			return;
		}
		
		// Istruzione dinamica, local o spreaded già completamente risolta
		if (instruction.isDynamic() && instruction.isDynamicSolvedFull()) {
			return;
		}

		// Istruzione ancora da risolvere, anche parzialmente
		
		// Inizializzazione strutture
		al_Program = new ArrayList<String> ();
		
		// Impostazioni per LogicInfoDynamic 
        logicDynamicInstr = this.logicInfoDynamic.getDynamicInstr(instruction.getNumInstr());
		if (logicDynamicInstr == null) {
			logicDynamicInstr = new LogicDynamicInstruction();
			logicDynamicInstr.setDynamicInstr(instruction);
		}

		// Estrazione operando Program
		dataItemIdentifier1 = (DataItemCobolIdentifier) instruction.getOperand("PROGRAM");
		
		// Deve essere valorizzato
		if (dataItemIdentifier1 == null) {
			instruction.setParsingError(true);
			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_PARSING_CICS_INSTRUCTION);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "EI0027", "", excp, new String[]{"PROGRAM", ""});
			return;
		}

		// PROGRAM espresso da literal	
		if(dataItemIdentifier1.getIdentifierType() ==  EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
			typeSourceProcess = EnumRelationSourceProcess.SOURCE_STATIC_LOCAL;
			programName = dataItemIdentifier1.getValueStringFormatted();
			al_Program.add(programName);
			instruction.setStaticSolved(true);			
		} else {
			typeSourceProcess = EnumRelationSourceProcess.SOURCE_DYNAMIC;
			// PROGRAM espresso da campo dinamico
			if (di.optSolveDynamicLocal) {
				// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
				if (typeLogicDynamic == LOGIC_DYNAMIC_SAME_PGM) {
					addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE);
					// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
					al_Program = this.logicSamePgm.dynamicValues(acp.getProgramCobol(), instruction, dataItemIdentifier1);
					logicDynamicInstr.al_dynamicField.add(this.logicSamePgm.getDynamicFieldToSolve());			
					// Aggiornamento struttura info dinamiche e strutture per update db finale 
					this.logicInfoDynamic.putDatabaseInfoAll(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifier1.getNameIdentifier());			
				}
				// Verifica se valori resi disponibili dal processo CHIAMANTE logic spreaded 
				if (typeLogicDynamic == LOGIC_DYNAMIC_SPREADED_PGM) {
					addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SPREAD);
					if (this.logicInfoDynamic.isDynamicFieldSpreaded(instruction.getNumInstr(), dataItemIdentifier1.getNameIdentifier())) {
						al_Program = this.logicInfoDynamic.getDynamicFieldSpreadedValues(instruction.getNumInstr(), dataItemIdentifier1.getNameIdentifier());
						this.logicInfoDynamic.putDatabaseInfoValuesOnly(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifier1.getNameIdentifier());
						instruction.setDynamicSpreaded(true);
					}
				}
			}
		}
		
		// Istruzione dinamica: nessun valore individuato o recupero valori disabilitato
		if (al_Program == null || al_Program.size() == 0) {
			return;
		}
		
		// Valori individuati per l'operando Program  e già inseriti in struttura per db da logicInfoDynamic.putDatabaseInfo(..)
		
		// Inserimento valori in map descriptor istruzione per il parametro PROGRAM
		instruction.setDynamicOperandValues("PROGRAM", al_Program);
		instruction.setDynamicSolved(true);
		instruction.setDynamicSolvedFull(true);

		// Inserimento oggetti program con relativa origine
		for (String programCalled : al_Program) {

			// Recupera inserisce in struttura da sysSubSys corr/Owner/New
			this.analyzerDbInfo.getAddObjectEntitled(programCalled, EnumObject.OBJECT_PGM_COBOL);

			// Relazione generale fra programma e programma chiamato
			this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
								                    EnumObject.OBJECT_PGM_COBOL
								                  , this.programName
								                  , EnumObject.OBJECT_PGM_COBOL
												  , programCalled
												  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
												  , typeRelation
												  , typeSourceProcess
												  , this.programName
												  , instruction.getNumInstr()
												  , instruction.getRowStartSource()
												  , instruction.getRowEndSource()
												  , ""
												  , 0
												  , 0
												  , instruction.getTypeInstrCategory()
												  , this.activeDivisionArea
												  , false
												  , false 
												  , false
												  , this.userExitInfoPgm
											   );
			
		}

		return;
		
	}
	


	/*
	 * 
	 * Gestione Exec Cics Start
	 * Gestione Exec Cics Return
	 * 
	 * Viene gestito il parametro Transid()
	 * 
	 */
	private void  logicCicsStartReturn(InstructionCics instruction, int typeLogicDynamic, EnumRelation typeRelation) throws ExceptionAmrita, SQLException {
        
		LogicDynamicInstruction logicDynamicInstr = null; 		// Codifica istruzione dinamica per risoluzione valori		
		ExceptionAmrita excp = null;
	    ArrayList<String> al_Transid = null;  					// Valori estratti per TRANSID
		EnumRelationSourceProcess typeSourceProcess = null;  	// Operando TRANSID statico (literal) o dinamico (campo)
	    String transidName = "";

	    // Cics solving non abilitato
        if (!this.di.optSolveCodeCics) {
			return;
		}
	    		
		// Istruzione statica già risolta.
		if (instruction.isStaticSolved()) {
			return;
		}
		
		// Istruzione dinamica, local o spreaded già completamente risolta
		if (instruction.isDynamic() && instruction.isDynamicSolvedFull()) {
			return;
		}
		
		// Istruzione ancora da risolvere, anche parzialmente
		
		// Inizializzazione strutture
		al_Transid = new ArrayList<String> ();

		// Impostazioni per LogicInfoDynamic 
        logicDynamicInstr = this.logicInfoDynamic.getDynamicInstr(instruction.getNumInstr());
		if (logicDynamicInstr == null) {
			logicDynamicInstr = new LogicDynamicInstruction();
			logicDynamicInstr.setDynamicInstr(instruction);
		}

		// Estrazione operando TRANSID
		dataItemIdentifier1 = (DataItemCobolIdentifier) instruction.getOperand("TRANSID");
		
		// Se Cics Start Transid è obbligatorio
		if (instruction.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.CICS_INSTR_START
		&&  dataItemIdentifier1 == null) {

			instruction.setSemanticError(true);
			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_PARSING_CICS_INSTRUCTION);
			instruction.setInfoError(EnumMessageType.ERROR_INPUT, "EI0027", "", excp, new String[]{"PROGRAM", ""});
			return;
		}
		
		// Se Cics Return,  Transid è opzionale: nessuna operazione
		if (instruction.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.CICS_INSTR_RETURN
		&&  dataItemIdentifier1 == null) {
			return;
		}
		
		// Transid espresso da literal
		if (dataItemIdentifier1.getIdentifierType() ==  EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
			typeSourceProcess = EnumRelationSourceProcess.SOURCE_STATIC_LOCAL;
			transidName = dataItemIdentifier1.getValueStringFormatted();
			al_Transid.add(transidName);
			instruction.setStaticSolved(true);
		} else {
			typeSourceProcess = EnumRelationSourceProcess.SOURCE_DYNAMIC;
			// Nome Transid espresso da campo dinamico 
			if (di.optSolveDynamicLocal) {
				// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
				if (typeLogicDynamic == LOGIC_DYNAMIC_SAME_PGM) {
					addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE);
					// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
					al_Transid = this.logicSamePgm.dynamicValues(acp.getProgramCobol(), instruction, dataItemIdentifier1);
					logicDynamicInstr.al_dynamicField.add(this.logicSamePgm.getDynamicFieldToSolve());			
					// Aggiornamento struttura info dinamiche e strutture per update db finale 
					this.logicInfoDynamic.putDatabaseInfoAll(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifier1.getNameIdentifier());			
				}
				// Verifica se valori resi disponibili dal processo CHIAMANTE logic spreaded 
				if (typeLogicDynamic == LOGIC_DYNAMIC_SPREADED_PGM) {
					addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SPREAD);
					if (this.logicInfoDynamic.isDynamicFieldSpreaded(instruction.getNumInstr(), dataItemIdentifier1.getNameIdentifier())) {
						al_Transid = this.logicInfoDynamic.getDynamicFieldSpreadedValues(instruction.getNumInstr(), dataItemIdentifier1.getNameIdentifier());
						this.logicInfoDynamic.putDatabaseInfoValuesOnly(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifier1.getNameIdentifier());
						instruction.setDynamicSpreaded(true);
					}
				}
				if (al_Transid.size() > 0) {
					instruction.setDynamicSolved(true);
					instruction.setDynamicSolvedFull(true);
				}
			}		
		}

		// Istruzione dinamica o statica: nessun valore individuato o recupero valori disabilitato
		if (al_Transid == null || al_Transid.size() == 0 ) {
			return;
		}
				
		// Valori individuati per l'operando TRANSID e già inseriti in struttura per db da logicInfoDynamic.putDatabaseInfo(..)
		
		// Inserimento valori in map descriptor istruzione per il parametro TRANSID
		instruction.setDynamicOperandValues("TRANSID", al_Transid);
		instruction.setDynamicSolved(true);
		instruction.setDynamicSolvedFull(true);

		// Inserimento oggetti Internal File e relazioni Pgm-Cics_Start_Transid con relativa origine
		for (String transid : al_Transid) {

			// Recupera inserisce in struttura da sysSubSys corr/Owner/New
			this.analyzerDbInfo.getAddObjectEntitled(transid, EnumObject.OBJECT_CICS_TRANSID);

			// Relazione generale fra programma e file esterno Vsam (nel jcl Cics)
			this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
								                    EnumObject.OBJECT_PGM_COBOL
								                  , this.programName
								                  , EnumObject.OBJECT_CICS_TRANSID
												  , transid
												  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
												  , typeRelation
												  , typeSourceProcess
												  , this.programName
												  , instruction.getNumInstr()
												  , instruction.getRowStartSource()
												  , instruction.getRowEndSource()
												  , ""
												  , 0
												  , 0
												  , instruction.getTypeInstrCategory()
												  , this.activeDivisionArea
												  , false
												  , false  
												  , false
												  , this.userExitInfoPgm
											   );
			
		}

		return;
		
	}

	/*
	 * 
	 * Gestione Exec Cics Abend
	 * 
	 */
	private void  logicCicsAbend(InstructionCics instruction, int typeLogicDynamic) throws ExceptionAmrita, SQLException {

		LogicDynamicInstruction logicDynamicInstr = null; 		// Codifica istruzione dinamica per risoluzione valori		
		ArrayList<String> al_Abcode = null;  					// Valori estratti per ABCODE
		EnumRelationSourceProcess typeSourceProcess= null;  	// Operando ABCODE statico (literal) o dinamico (campo)
	    String abcodeName = "";

	    // Cics solving non abilitato
        if (!this.di.optSolveCodeCics) {
			return;
		}
	    		
		// Istruzione statica già risolta.
		if (instruction.isStaticSolved()) {
			return;
		}
		
		// Istruzione dinamica già completamente risolta
		if (instruction.isDynamic() && instruction.isDynamicSolvedFull()) {
			return;
		}
		
		// Istruzione ancora da risolvere, anche parzialmente
		
		// Inizializzazione strutture
	    al_Abcode = new ArrayList<String> ();

		// Impostazioni per LogicInfoDynamic 
        logicDynamicInstr = this.logicInfoDynamic.getDynamicInstr(instruction.getNumInstr());
		if (logicDynamicInstr == null) {
			logicDynamicInstr = new LogicDynamicInstruction();
			logicDynamicInstr.setDynamicInstr(instruction);
		}

	    // Estrazione operando ABCODE
		dataItemIdentifier1 = (DataItemCobolIdentifier) instruction.getOperand("ABCODE");

		// Abcode opzionale e assente: nessuna operazione
		if (dataItemIdentifier1 == null) {
			return;
		}
		
		// ABCODE espresso da literal
		if (dataItemIdentifier1.getIdentifierType() ==  EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
			typeSourceProcess = EnumRelationSourceProcess.SOURCE_STATIC_LOCAL;
			abcodeName = dataItemIdentifier1.getValueStringFormatted();
			al_Abcode.add(abcodeName);
			instruction.setStaticSolved(true);
		} else {
			typeSourceProcess = EnumRelationSourceProcess.SOURCE_DYNAMIC;
			// ABCODE espresso da campo dinamico  
			if (typeLogicDynamic == LOGIC_DYNAMIC_SAME_PGM) {
				addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE);
				// Tentativo di risolvere l'istruzione dinamica nello stesso programma  
				al_Abcode = this.logicSamePgm.dynamicValues(acp.getProgramCobol(), instruction, dataItemIdentifier1);
				logicDynamicInstr.al_dynamicField.add(this.logicSamePgm.getDynamicFieldToSolve());			
				// Aggiornamento struttura info dinamiche e strutture per update db finale 
				this.logicInfoDynamic.putDatabaseInfoAll(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifier1.getNameIdentifier());			
			}
			// Verifica se valori resi disponibili dal processo CHIAMANTE logic spreaded 
			if (typeLogicDynamic == LOGIC_DYNAMIC_SPREADED_PGM) {
				addDbOptionForThePgm(EnumObject.OBJECT_PGM_COBOL, EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SPREAD);
				if (this.logicInfoDynamic.isDynamicFieldSpreaded(instruction.getNumInstr(), dataItemIdentifier1.getNameIdentifier())) {
					al_Abcode = this.logicInfoDynamic.getDynamicFieldSpreadedValues(instruction.getNumInstr(), dataItemIdentifier1.getNameIdentifier());
					this.logicInfoDynamic.putDatabaseInfoValuesOnly(this.analyzerDbInfo, logicDynamicInstr, dataItemIdentifier1.getNameIdentifier());
					instruction.setDynamicSpreaded(true);
				}
			}
			if (al_Abcode.size() > 0) {
				instruction.setDynamicSolved(true);
				instruction.setDynamicSolvedFull(true);
			}
	}

		// Istruzione dinamica: nessun valore individuato o recupero valori disabilitato
		if (al_Abcode == null || al_Abcode.size() == 0) {
			return;
		}
		
		// Valori individuati per l'operando Abcode e già inseriti in struttura per db da logicInfoDynamic.putDatabaseInfo(..)
		
		// Inserimento valori in map descriptor istruzione per il parametro ABCODE
		instruction.setDynamicOperandValues("ABCODE", al_Abcode);
		instruction.setDynamicSolved(true);
		instruction.setDynamicSolvedFull(true);

		// Inserimento oggetti Internal File e relazioni Pgm-Internal_File con relativa origine

		for (String transid : al_Abcode) {			

			// Recupera inserisce in struttura da sysSubSys corr/Owner/New
			this.analyzerDbInfo.getAddObjectEntitled(transid, EnumObject.OBJECT_CICS_ABCODE);
			
			// Relazione generale fra programma e file esterno Vsam (nel jcl Cics)
			this.analyzerDbInfo.prepareForDbObjectRelatedToAny(
								                    EnumObject.OBJECT_PGM_COBOL
								                  , this.programName
								                  , EnumObject.OBJECT_CICS_ABCODE
												  , transid
												  , EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED
												  , EnumRelation.PGM_CICS_ABCODE
												  , typeSourceProcess
												  , this.programName
												  , instruction.getNumInstr()
												  , instruction.getRowStartSource()
												  , instruction.getRowEndSource()
												  , ""
												  , 0
												  , 0
												  , instruction.getTypeInstrCategory()
												  , this.activeDivisionArea
												  , false
												  , false
												  , false
												  , this.userExitInfoPgm
											   );
			
		}
	
		return;
		
	}
	

	/*
     * 
     * 
     *  Restituisce il token successivo dopo aver verificato che esiste.
     *  Se non esiste restituisce ""
     * 
     */
	private String nextToken(Scanner scn)  {
		
		String newToken = "";
		
		// Non ci sono altri token: return stringa vuota
		if (!scn.hasNext()) {
			return newToken;
		} 
			
		newToken = scn.next();
		
		return newToken;
	}

	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

	
	/*
	 *   Classe contenitore di servizio associato alla prima parola riservata di istruzioni Cics.
	 *   
	 *   Ogni entry contiene la sequenza di parole chiave che identificano un'istruzione Cics.
	 *   Più istruzioni Cics possono iniziare con la stessa parola, come:
	 *   handle Aid, Handle Abend, Handle condition
	 *   
	 *   
	 */
	private class InnerInstructionWordsEntry {
		
		public EnumPrecompilerReservedWords en_WordReservedOwner = null; 					// Enum parola riservata indicante l'istruzione
		@SuppressWarnings("unused")
		public EnumInstrPrecompilerType typeEntry = EnumInstrPrecompilerType.NOT_ASSIGNED; 	// Constant: istruzione, operando opzione o exception (EXEC_CICS_INSTRUCTION, ..)
		public ArrayList<String> al_wordKey = null;                		 					// Sequenza completa di parole chiave valide identificanti l'istruzione 
		
        /*
         * Costruttore
         */
        private InnerInstructionWordsEntry() {
        	en_WordReservedOwner = EnumPrecompilerReservedWords.NOT_ASSIGNED;
        	al_wordKey = new ArrayList<String> ();
		}
	}


	
}
