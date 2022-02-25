/**
 * ---------- 
 * LogicTools 
 * ---------- 
 * 
 * Classe di gestione comune delle logiche applicative con metodi di servizio di utilita <br>
 *  
 * 1) Generazione paths con sola struttura di richiamo perform <br>
 * 2) Generazione paths con esplosione di tutte le istruzioni <br>
 * 3) Generazione elenco campi in I/O ordinati per nome <br>
 * 4) Generazione catene di trasformazione dato elementare con impostazione ultima assegnazione <br>
 * 5) Restituzione sottocampi elementari di campo di gruppo <br>
 * 
 */
package analyzer;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import analyzer.Instruction.InnerSymbolEntry;
import dao.DAOImplDynamicCicsMapping;
import dao.DAOImplDynamicFieldSubWaitExt;
import dao.DAOImplDynamicValueExt;
import dao.DAOImplRelation;
import dao.DAOImplRelationOrigin;
import dao.IDAODynamicCicsMapping;
import dao.IDAODynamicFieldSubWaitExt;
import dao.IDAODynamicValueExt;
import dao.IDAORelation;
import dao.IDAORelationOrigin;
import entities.EntityDynamicCicsMapping;
import entities.EntityDynamicFieldSub;
import entities.EntityDynamicFieldSubSetting;
import entities.EntityDynamicFieldSubValue;
import entities.EntityDynamicFieldSubWaitExt;
import entities.EntityDynamicValueExt;
import entities.EntityRelation;
import entities.EntityRelationOrigin;
import enums.EnumCobolFigurativeConstants;
import enums.EnumCobolReservedWords;
import enums.EnumCobolValueType;
import enums.EnumDataItemGeneric;
import enums.EnumDataItemSystemEnvironment;
import enums.EnumInstrDataCategory;
import enums.EnumLogicSetMode;
import enums.EnumLogicSetPointerArea;
import enums.EnumMessageType;
import enums.EnumObject;
import enums.EnumPrecompilerReservedWords;
import enums.EnumRelation;
import enums.EnumSymbolType;
import exception.ExceptionAmrita;
import utilities.StringService;


/**
 * -----------------------------------------------------
 * LogicTools
 * -----------------------------------------------------
 * 
 * Contiene tutto il codice di utilità per gestione delle logiche di programma come paths di esecuzione
 * catene di trasformazione, etc oltre a tutto il codice in comune fra LogicSamePgm e LogicSpreadedPgm.
 * Il codice in comune eè codificato con metodi statici.
 * 
 * 
 * @author giampietroZedda
 *
 */
public class LogicTools extends ExecutionShared implements AmritaConstants {
	public static final String EXPAND_PERFORM_STACK_ONLY = "S";            	// Solo stack perform richiamate
	public static final String EXPAND_PERFORM_TO_PARAGRAPH = "P";		    // Stack perform richiamate + istruzioni da inizio paragrafo a perform.
	public static final String EXPAND_PERFORM_DETAIL_ANY_LEVEL = "D";		// Come EXPAND_PERFORM_TO_PARAGRAPH + istruzioni paragrafi richiamati a qualsiasi livello

	private LogicInfoDynamic logicInfoDynamic = null;                      // Contiene informazioni organizzate su campi/sottocampi/valori/valori in waiting
	private ProgramCobol program = null;
	private boolean isAnalysis = false;                                     // False = attivazione da Web Impact/Logic

	public LogicTools(ProgramCobol program, boolean isAnalysis, UserConfiguration ucfg) {
		super.ucfg = ucfg;
		this.program = program;
		this.isAnalysis = isAnalysis;
		this.logicInfoDynamic = program.getLogicInfoDynamic();
	}

	/*
	 * ---------------------------------------------------------------------
	 * Generazione paths di esecuzione possibili da inizio a fine istruzione 
	 * ---------------------------------------------------------------------
	 * <br>
	 * Se expandToPerform true gli statements da istruzione di inizio fino alla perform vengono portate in output.<br>
	 * Se expandToPerform true e expandPerforms true vengono espanse ricorsivamente tutte le perform <br>
	 * Se detailPeforms true ogni istruzione dentro i paragrafi performati viene portata in output, altrimenti <br>
	 * solo un arrayList con il range di istruzioni nnnn-nnnn di inizio e fine del paragrafo performato.<br>
	 * <br>
	 * La routine soddisfa quindi esigenze quali:<br>
	 * <br>
	 * 1) Paths di esecuzione con le sole perform  ai paragrafi richiamati + l'istruzione finale target
	 * 2) Paths di esecuzione con i paths richiamati e tutte le istruzioni incontrate, a livello 0, senza esplodere le perform
	 * 3) Paths di esecuzione come 2) con esplosione del dettaglio di tutte le perform incontrate a tutti i lvelli di profondità
	 * <br>
	 * Viene restituito un array di paths ProgramPath dove è presente un array di istruzioni ProgramPathEntry
	 * a supporto dei punti 1) 2) 3) 4)
	 * <br>
	 * 
	 * @param fromNumInstr Starting instruction to consider, normally Procedure Division statement
	 * @param toNumInstr ending instruction to consider to find all possible pexecution paths
	 * @param typeExpandPerform "S" Solo stack perform richiamate
	 *                          "P" Stack perform richiamate + istruzioni da inizio paragrafo a perform
	 *                          "D" Come EXPAND_PERFORM_TO_PARAGRAPH + istruzioni paragrafi richiamati a qualsiasi livello
	 */
	public ProgramPath[] paths(ProgramCobol program, int fromNumInstr, int toNumInstr, String typeExpandPerform) {

		List<ProgramPath> alPath = null;
		List<ProgramPathEntry> alPathEntryCur = null;	
		Set<Integer> setPerformTo = null;
		ProgramPath arPath[] = null;
		ProgramCobolEntry<? extends Instruction>[] arProcEntry = null;

		setPerformTo = new HashSet<Integer>();
		alPath = new ArrayList<ProgramPath>();
		alPathEntryCur = new ArrayList<ProgramPathEntry>();

		arProcEntry = program.entriesProcedure();

		pathsRecursiveOnlyStack(program, arProcEntry, alPath, alPathEntryCur, fromNumInstr, toNumInstr, toNumInstr, setPerformTo);

		// Converto to array 
		arPath = new ProgramPath[alPath.size()];
		arPath = alPath.toArray(arPath);
		return arPath;
	};


	// Esecuzione ricorsiva di estrazione path
	// Vengono estratte le perform richiamate a qualsiasi livello
	// Il processo si estende a qualsiasi livello di profondita
	private void pathsRecursiveOnlyStack(ProgramCobol program
										,ProgramCobolEntry<? extends Instruction>[] arProcEntry
										,List<ProgramPath> alPath
										,List<ProgramPathEntry> alPathEntryCur
										,int fromNumInstr
										,int toNumInstr
										,int toNumInstrOrigin
										,Set<Integer> setPerformTo) {

		ProgramPathEntry pathEntry = null;
		ProgramPathEntry arPathEntry[] = null;
		List<ProgramPathEntry> alPathInstrNew = null;
		List<ProgramPathEntry> alPathEntryRev = null;
		ProgramPath pathCur = null;
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		ProgramCobolEntry<? extends Instruction> entryProcCaller = null;
		InstructionCobolProcedure instrCobol = null;
		InstructionCobolProcedure instrCaller = null;
		EnumCobolReservedWords typeInstr = null;   
		int[] arXrefLabelSection = null;
		int numPath = 0;
		int lvlDeep = 0;
		String labelSection = "";
		String idPath = "";

		// Scan istruzioni dall'ultima istruzione target fino a quella di inizio
		// o a label di inizio paragrafo/section
		// Nel processo ricorsivo quella di inizio diventa la label di inizio paragrafo/section
		for (int i = toNumInstr; i >= fromNumInstr; i--) {

			entryProc = arProcEntry[i];
			typeInstr = arProcEntry[i].getTypeInstr();

			// Porto COMUNQUE in output l'istruzione se start/end
			if (i == toNumInstrOrigin || i == fromNumInstr || i == toNumInstr) {
				pathEntry = new ProgramPathEntry();
				setPathEntry(pathEntry, arProcEntry[i], arProcEntry);
				alPathEntryCur.add(pathEntry);
				continue;
			}

			// Ricerca chiamanti
			if (typeInstr == EnumCobolReservedWords.PROC_LABEL 
			||  typeInstr == EnumCobolReservedWords.PROC_SECTION) {
				instrCobol = (InstructionCobolProcedure) entryProc.getInstruction();
				// Label/Section get callers
				if (typeInstr == EnumCobolReservedWords.PROC_LABEL) {
					labelSection = instrCobol.labelGetName();
					arXrefLabelSection = program.xrefToLabel(labelSection);
				} else {
					labelSection = instrCobol.sectionGetName();
					arXrefLabelSection = program.xrefToSection(labelSection);
				}

				// Section NON referenziata: codice morto
				if (typeInstr == EnumCobolReservedWords.PROC_SECTION && arXrefLabelSection.length == 0) {
					break;
				}
				
				// Label NON referenziata: bypass (continue) se dentro section o prima, altrimenti return
				if (typeInstr == EnumCobolReservedWords.PROC_LABEL && arXrefLabelSection.length == 0) {
					if (isLabelDeadCode(arProcEntry, instrCobol, labelSection)) {
						return;
					}
					continue;  
				}
				
				// Scan perform a label/section (o goto)
				// Attivazione ricorsiva	per ogni caller	
				// Se già trattato si scarta
				for (int j = 0; j < arXrefLabelSection.length; j++) {
					if (setPerformTo.contains(arXrefLabelSection[j])) {
						continue;
					}
					setPerformTo.add(arXrefLabelSection[j]);
					entryProcCaller = arProcEntry[arXrefLabelSection[j]];
					instrCaller = (InstructionCobolProcedure) entryProcCaller.getInstruction();
					typeInstr = instrCaller.getTypeInstr();
					labelSection = program.procInternalNameOwner(instrCaller.getNumInstr());

					// Attivazione ricorsiva
					if (typeInstr == EnumCobolReservedWords.PROC_PERFORM
					||  typeInstr == EnumCobolReservedWords.PROC_GOTO) {
						alPathInstrNew = new ArrayList<ProgramPathEntry>();
						alPathInstrNew.addAll(alPathEntryCur);
						pathsRecursiveOnlyStack(program, arProcEntry, alPath, alPathInstrNew, fromNumInstr, instrCaller.getNumInstr(), toNumInstrOrigin, setPerformTo);
					}
				} 
				// Elaborati tutti i chiamanti, paths già generati a livello ricorsivo
				return;
			}
		}

		// Si è Arrivati all'istruzione di inizio path
		numPath = alPath.size();
		idPath = "PATH" + numPath;
		pathCur = new ProgramPath(idPath, fromNumInstr, toNumInstrOrigin);

		// Normalizzazione livello annidamento da 0 e reverse in ordine di istruzione
		alPathEntryRev = new ArrayList<>();
		ProgramPathEntry ppe = null;
		for (int i = alPathEntryCur.size() - 1; i >= 0; i--) {
			ppe = alPathEntryCur.get(i);			
			ppe.setLvlDeep(lvlDeep);
			alPathEntryRev.add(ppe);
			lvlDeep++;
		}
		arPathEntry = new ProgramPathEntry[alPathEntryRev.size()];
		arPathEntry = (ProgramPathEntry[]) alPathEntryRev.toArray(arPathEntry);
		pathCur.setEntries(arPathEntry);

		// Accodamento nei path generali da portare in output
		alPath.add(pathCur);
		return;
	}

	/*
	 * Restituisce true se la label non referenziata è da considerarsi dead code, altrimenti false
	 * 
	 * NON è dead code se:
	 * 
	 * 1) Prima istruzione dopo procedure division
	 * 2) Istruzione dentro section
	 */
	private boolean isLabelDeadCode(ProgramCobolEntry<? extends Instruction>[] arProcEntry, InstructionCobolProcedure instrCobol, String labelSection) {
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		EnumCobolReservedWords typeInstr = null;

		// Section non referenziata: dead code
		if (this.program.isSection(labelSection)) {
			return true;
		}
		
		// Label
		
		// Istruzione dopo procedure division: bypass
		if (instrCobol.getNumInstr() == 1) {
			return false;
		}
		
		// Verifica se label non referenziata dentro Section		
		for (int i = instrCobol.getNumInstr() - 1; i >= 0; i--) {
			entryProc = arProcEntry[i];	
			typeInstr = entryProc.getTypeInstr();
			
			// Label dentro section: bypass
			if (typeInstr == EnumCobolReservedWords.PROC_SECTION) {
				return false;
			}		
		}
		
		
		// Verifica se label non referenziata preceduta da altra label: dead code		
		for (int i = instrCobol.getNumInstr() - 1; i >= 0; i--) {
			entryProc = arProcEntry[i];	
			typeInstr = entryProc.getTypeInstr();
			
			// Label dentro section: bypass
			if (typeInstr == EnumCobolReservedWords.PROC_LABEL) {
				return true;
			}		
		}
		
		
		return false;
	}

	/*
	 * -------------------------------------------------------------------
	 * Gestione espansione Per il path fornito 
	 * -------------------------------------------------------------------
	 * 
	 *  EXPAND_PERFORM_TO_PARAGRAPH
	 *    Si inseriscono nel path, prima della Perform a paragrafo, tutte le istruzioni precedenti fino alla label o section di pertinenza.
	 *    Eventuali altre perform presenti vengono lasciate NON explose
	 *    
	 *  EXPAND_PERFORM_DETAIL_ANY_LEVEL
	 *    Si opera come  EXPAND_PERFORM_TO_PARAGRAPH ma, successivamente, si espandono tutte le perform richiamate,
	 *    a qualsuasi livello, inserendo le istruzioninel path sotto esame.
	 *   
	 */
	public ProgramPath pathExpand(ProgramCobol programCobol, ProgramPath path, String typeExpandPerform) {

		ProgramPath pathExpanded = null;

		if (typeExpandPerform.equals(EXPAND_PERFORM_TO_PARAGRAPH)
		||  typeExpandPerform.equals(EXPAND_PERFORM_DETAIL_ANY_LEVEL)) {
			pathExpanded = pathExpandPerformToParagraph(programCobol, path);
			if (typeExpandPerform.equals(EXPAND_PERFORM_DETAIL_ANY_LEVEL)) {
				pathExpanded = pathExpandPerformDetailAnyLevel(programCobol, path, typeExpandPerform);
			}
		}		
		return pathExpanded;
	}

	/*
	 * -------------------------------------------------------------------
	 * Gestione espansione array di path fornito
	 * -------------------------------------------------------------------
	 * 
	 *  EXPAND_PERFORM_TO_PARAGRAPH
	 *    Si inseriscono nel path, prima della Perform a paragrafo, tutte le istruzioni precedenti fino alla label o section di pertinenza.
	 *    Eventuali altre perform presenti vengono lasciate NON explose
	 *    
	 *  EXPAND_PERFORM_DETAIL_ANY_LEVEL
	 *    Si opera come  EXPAND_PERFORM_TO_PARAGRAPH ma, successivamente, si espandono tutte le perform richiamate,
	 *    a qualsuasi livello, inserendo le istruzioninel path sotto esame.
	 *   
	 */
	public ProgramPath[] pathsExpand(ProgramCobol programCobol, ProgramPath[] paths, String typeExpandPerform) {

		ProgramPath[] pathsExpanded = null;
		ProgramPath pathExpanded = null;
		ProgramPath path = null;

		pathsExpanded = new ProgramPath[paths.length];

		// Scan paths perform stack level only
		for (int i = 0; i < paths.length; i++) {
			path = paths[i];

			if (typeExpandPerform.equals(EXPAND_PERFORM_TO_PARAGRAPH)
			||  typeExpandPerform.equals(EXPAND_PERFORM_DETAIL_ANY_LEVEL)) {
				pathExpanded = pathExpandPerformToParagraph(programCobol, path);
				if (typeExpandPerform.equals(EXPAND_PERFORM_DETAIL_ANY_LEVEL)) {
					pathExpanded = pathExpandPerformDetailAnyLevel(programCobol, path, typeExpandPerform);
				}
			}
			pathsExpanded[i] = pathExpanded;
		}

		return pathsExpanded;
	}




	/*
	 * -----------------------------------------------------------------------------------------------------------------------
	 * inserimento, per ogni perform nello stack di perform si inseriscono section/par performato e istruzioni fino a perform
	 * -----------------------------------------------------------------------------------------------------------------------
	 * 
	 * Procedure division
	 * perform A
	 *   perform B
	 *     perform C
	 *     
	 * Diventa:
	 * 
	 * Procedure division
	 * istr1
	 * istr2
	 * ...
	 * perform A
	 *   A.
	 *   ...
	 *   ...
	 *   perform B
	 *     B.
	 *     ...
	 *     ...
	 *     perfrom C
	 *       C.
	 *       ...
	 *       ... target
	 * 
	 */
	private ProgramPath pathExpandPerformToParagraph(ProgramCobol programCobol, ProgramPath path) {
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		ProgramPathEntry[] arPathEntry = null;
		ProgramPathEntry[] arPathEntryNew = null;
		ProgramPathEntry pathEntry = null;
		ArrayList<ProgramPathEntry> alPathEntry = null;
		ProgramCobolEntry<? extends Instruction>[] arProc = null;
		InstructionCobolProcedure instrCobol = null;
		int numInstrFrom = 0;
		int numInstrEnd = 0;

		arProc = programCobol.entriesProcedure();

		arPathEntry = path.getEntries();		
		alPathEntry = new ArrayList<ProgramPathEntry>();

		// Put procedure division
		pathEntry = arPathEntry[0];
		alPathEntry.add(pathEntry);     
		pathEntry = arPathEntry[1];
		numInstrEnd = pathEntry.getNumInstr() -  1;

		// Scan path instructions from procedure division to first perform in stack
		for (int i = 1; i <= numInstrEnd; i++) {
			entryProc = arProc[i];	
			pathExpandPutInstructionInPath(entryProc, alPathEntry, arProc, 1);		
		}

		// Scan path instructions (they are already ordered by instruction number)) 
		for (int i = 1; i < arPathEntry.length; i++) {

			// Put perform label/section
			pathEntry = arPathEntry[i];
			alPathEntry.add(pathEntry);    // Procedure division or perform
			entryProc = arProc[pathEntry.getNumInstr()];	

			// Put paragraph/section in path
			if (i > 0 && i < arPathEntry.length - 1) {
				instrCobol = (InstructionCobolProcedure) entryProc.getInstruction();

				// Estrazione istruzione inizio fine istruzioni paragrafo fino a successiva perform in stack esclusa
				numInstrFrom = instrCobol.performGetFromNumInstr();
				numInstrEnd = arPathEntry[i+1].getNumInstr();

				// Scan paragraph/section instructions
				for (int j = numInstrFrom; j < numInstrEnd; j++) {
					entryProc = arProc[j];	
					pathExpandPutInstructionInPath(entryProc, alPathEntry, arProc, pathEntry.getLvlDeep() + 1);		
				}					
			}	
		}

		// Conversion to array and store 
		arPathEntryNew = new ProgramPathEntry[alPathEntry.size()];
		arPathEntryNew = alPathEntry.toArray(arPathEntryNew);
		path.setEntries(arPathEntryNew);

		return path;		
	}

	//------------------------------------------------------
	// Put istruzione in paragrafo/section
	//------------------------------------------------------
	private void pathExpandPutInstructionInPath(ProgramCobolEntry<? extends Instruction> entryProc, ArrayList<ProgramPathEntry> alPathEntry, ProgramCobolEntry<? extends Instruction>[] arProc, int lvlDeep) {
		ProgramPathEntry pathEntry = null;
		Instruction instr = null;
		InstructionCobolProcedure instrCobol = null;
		int niStart = 0;
		int niEnd = 0;

		instr = (Instruction) entryProc.getInstruction();
		pathEntry = new ProgramPathEntry();
		pathEntry.setLvlDeep(lvlDeep);
		pathEntry.setNumInstr(instr.getNumInstr());
		pathEntry.setSourceInstr(instr.getSourceInstr());
		pathEntry.setRowStartSource(instr.getRowStartSource());
		pathEntry.setRowEndSource(instr.getRowEndSource());
		pathEntry.setUnderCopyName(entryProc.getUnderCopyName());
		pathEntry.setNumInstrIfOwner(entryProc.getNumEntryOwnerConditional());
		pathEntry.setPerform(false);

		// Perform statement, set called paragraph/section information
		if (instr.getTypeInstr() == EnumCobolReservedWords.PROC_PERFORM) {
			instrCobol = (InstructionCobolProcedure) entryProc.getInstruction();
			pathEntry.setPerform(true);
			niStart = instrCobol.performGetFromNumInstr();
			niEnd = getThruNumInstr(instrCobol, arProc);
			pathEntry.setNumInstrStartParagrah(niStart);
			pathEntry.setNumInstrEndParagrah(niEnd);
			pathEntry.setRowStartParagrah(arProc[niStart].getInstruction().getPosStartInstr());
			pathEntry.setRowEndParagrah(arProc[niEnd].getInstruction().getPosEndInstr());
			pathEntry.setIdParagrah(instrCobol.performGetFrom());
			pathEntry.setIdParagrahThru(instrCobol.performGetThru());
		}								
		alPathEntry.add(pathEntry);
	}

	/*
	 * -----------------------------------------------------------------------------------------------------------------------
	 * inserimento, per ogni perform nello stack di perform si inseriscono section/par performato e istruzioni fino a perform
	 * -----------------------------------------------------------------------------------------------------------------------
	 * 
	 * Procedure division
	 * perform A
	 *   perform B
	 *     perform C
	 *     
	 * Diventa:
	 * 
	 * Procedure division
	 * istr1
	 * istr2
	 * ...
	 * perform A
	 *   A.
	 *   perform A1
	 *     A1.
	 *     ...
	 *     perform A11
	 *       ...
	 *   ...
	 *   perform B
	 *     B.
	 *     ...
	 *     ...
	 *     perfrom C
	 *       C.
	 *       ...
	 *       ... target
	 * 
	 */
	private ProgramPath pathExpandPerformDetailAnyLevel(ProgramCobol programCobol, ProgramPath path, String typeExpandPerform) {
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		ProgramPathEntry[] arPathEntry = null;
		ProgramPathEntry[] arPathEntryNew = null;
		ProgramPathEntry pathEntry = null;
		ArrayList<ProgramPathEntry> alPathEntry = null;
		ProgramCobolEntry<? extends Instruction>[] arProc = null;
		InstructionCobolProcedure instrCobol = null;
		int numInstrFrom = 0;
		int numInstrEnd = 0;

		arProc = programCobol.entriesProcedure();

		arPathEntry = path.getEntries();		
		alPathEntry = new ArrayList<ProgramPathEntry>();

		// Scan path instructions (they are already in sequence)) 
		for (int i = 0; i < arPathEntry.length; i++) {

			// Put instruction in path
			pathEntry = arPathEntry[i];
			alPathEntry.add(pathEntry);     

			// We are interested in perform expanding NOT at level 0
			if (pathEntry.isPerform() && !pathEntry.isPerformLevel0()) {
				// Get perform instruction
				entryProc = arProc[pathEntry.getNumInstr()];	
				instrCobol = (InstructionCobolProcedure) entryProc.getInstruction();
				numInstrFrom = instrCobol.performGetFromNumInstr();
				numInstrEnd = getThruNumInstr(instrCobol, arProc);
				gestExpandPerformDetailRecursive(alPathEntry, numInstrFrom, numInstrEnd, arProc, pathEntry.getLvlDeep() + 1);
			}
		}

		// Conversion to array and store 
		arPathEntryNew = new ProgramPathEntry[alPathEntry.size()];
		arPathEntryNew = alPathEntry.toArray(arPathEntryNew);
		path.setEntries(arPathEntryNew);
		return path;		 
	}

	/*
	 * Espansione paragrafo ricorsivamente con il dettaglio di tutte le istruzioni
	 */
	private void gestExpandPerformDetailRecursive(List<ProgramPathEntry> al_pathEntry, int numInstrFrom, int numInstrThru, ProgramCobolEntry<? extends Instruction>[] ar_proc, int lvlDeep) {
		ProgramPathEntry pathEntry = null;
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		InstructionCobolProcedure instrCobol = null;
		Instruction instr = null;
		int niStart = 0;
		int niEnd = 0;

		// Scan paragraph instructions
		for (int i = numInstrFrom; i <= numInstrThru; i++) {
			entryProc = ar_proc[i];
			instr = (Instruction) entryProc.getInstruction();
			pathEntry = new ProgramPathEntry();		
			pathEntry.setLvlDeep(lvlDeep);
			pathEntry.setNumInstr(instr.getNumInstr());
			pathEntry.setSourceInstr(instr.getSourceInstr());
			pathEntry.setRowStartSource(instr.getRowStartSource());
			pathEntry.setRowEndSource(instr.getRowEndSource());
			pathEntry.setUnderCopyName(entryProc.getUnderCopyName());
			pathEntry.setNumInstrIfOwner(entryProc.getNumEntryOwnerConditional());
			pathEntry.setPerform(false);

			// Perform, set called paragraphs information
			if (instr.getTypeInstr() == EnumCobolReservedWords.PROC_PERFORM) {
				instrCobol = (InstructionCobolProcedure) entryProc.getInstruction();
				pathEntry.setPerform(true);
				niStart = instrCobol.performGetFromNumInstr();
				niEnd = getThruNumInstr(instrCobol, ar_proc);
				pathEntry.setNumInstrStartParagrah(niStart);
				pathEntry.setNumInstrEndParagrah(niEnd);
				pathEntry.setRowStartParagrah(ar_proc[niStart].getInstruction().getPosStartInstr());
				pathEntry.setRowEndParagrah(ar_proc[niEnd].getInstruction().getPosEndInstr());
				pathEntry.setIdParagrah(instrCobol.performGetFrom());
				pathEntry.setIdParagrahThru(instrCobol.performGetThru());
			}								
			al_pathEntry.add(pathEntry);

			// Expand perform paragraph
			if (instr.getTypeInstr() == EnumCobolReservedWords.PROC_PERFORM) {
				gestExpandPerformDetailRecursive(al_pathEntry, niStart, niEnd, ar_proc, lvlDeep + 1);		
			}

		} // end-for		
	}



	/*
	 * Recupero numero di istruzione di fine paragrafo.
	 * Se non indicato nell'istruzione si cerca la successiva label/section
	 */
	private int getThruNumInstr(InstructionCobolProcedure instrProc, ProgramCobolEntry<? extends Instruction>[] ar_proc) {
		ProgramCobolEntry<? extends Instruction> entryProc = null;
		ProgramCobolEntry<? extends Instruction> entryProcFrom = null;
		InstructionCobolProcedure instrFrom = null;
		Instruction instr = null;

		// Perform esplicito a paragraph/section thru endParagraph
		if (instrProc.performIsWithThru()) {
			return instrProc.performGetThruNumInstr();
		}

		// Get Label/Section
		entryProcFrom = ar_proc[instrProc.performGetFromNumInstr()];
		instrFrom = (InstructionCobolProcedure) entryProcFrom.getInstruction();

		// Perform a Section: cerco next section o fine progrogramma
		if (instrFrom.getTypeInstr() == EnumCobolReservedWords.PROC_SECTION) {
			for (int i = instrProc.performGetFromNumInstr() + 1; i < ar_proc.length; i++) {
				entryProc = ar_proc[i];
				instr = (Instruction) entryProc.getInstruction();
				if (instr.getTypeInstr() == EnumCobolReservedWords.PROC_SECTION) {
					return i - 1;
				}
			}
			return ar_proc.length - 1;
		}

		// Perform paragraph, cerco successiva label o section
		for (int i = instrProc.performGetFromNumInstr() + 1; i < ar_proc.length; i++) {
			entryProc = ar_proc[i];
			instr = (Instruction) entryProc.getInstruction();
			if (instr.getTypeInstr() == EnumCobolReservedWords.PROC_LABEL
					|| instr.getTypeInstr() == EnumCobolReservedWords.PROC_SECTION) {
				return i - 1;
			}
		}

		// Fine programma, probabile presenza di stop run o goback
		// In ogni caso fine del paragrafo performato
		return ar_proc.length - 1;
	}

	// Impostazione valori istruzione corrente
	private void setPathEntry(ProgramPathEntry pathEntry, ProgramCobolEntry<? extends Instruction> procEntry, ProgramCobolEntry<? extends Instruction>[] arProcEntry) {
		Instruction instr = null;
		InstructionCobolProcedure instrCobol = null;
		ProgramCobolEntry<? extends Instruction> procEntryParagraph = null;
		int numInstrThru = 0;

		instr = (Instruction) procEntry.getInstruction();
		pathEntry.setPerformLevel0(true);
		pathEntry.setSourceInstr(instr.getSourceInstr());
		pathEntry.setPerform(false);
		pathEntry.setNumInstr(instr.getNumInstr());
		pathEntry.setUnderCopyName(procEntry.getUnderCopyName());
		pathEntry.setRowStartSource(instr.getRowStartSource());
		pathEntry.setRowEndSource(instr.getRowStartSource());
		pathEntry.setNumInstrIfOwner(procEntry.getNumEntryOwnerConditional());
		if (instr.getTypeInstr() == EnumCobolReservedWords.PROC_PERFORM) {
			instrCobol = (InstructionCobolProcedure) procEntry.getInstruction();
			procEntryParagraph = arProcEntry[instrCobol.performGetFromNumInstr()];
			pathEntry.setPerform(true);
			pathEntry.setIdParagrah(instrCobol.performGetFrom());
			pathEntry.setNumInstrStartParagrah(instrCobol.performGetFromNumInstr());
			pathEntry.setRowStartParagrah(procEntryParagraph.getInstruction().getRowStartSource());
			pathEntry.setNumInstrEndParagrah(instrCobol.performGetThruNumInstr());
			if (instrCobol.performGetThruNumInstr() == 0) {
				numInstrThru = getThruNumInstr(instrCobol, arProcEntry);
				pathEntry.setNumInstrEndParagrah(numInstrThru);
				procEntryParagraph = arProcEntry[numInstrThru];
				pathEntry.setRowEndParagrah(procEntryParagraph.getInstruction().getRowEndSource());
			} else {
				procEntryParagraph = arProcEntry[instrCobol.performGetThruNumInstr()];
				pathEntry.setRowEndParagrah(procEntryParagraph.getInstruction().getRowEndSource());
			}
			pathEntry.setParagraphUnderCopy(procEntryParagraph.getUnderCopyName());
		}
	}


	/*
	 * ------------------------------------------------------------------
	 * Generazione lista campi in I/O di un path di esecuzione specifico <br>
	 * ------------------------------------------------------------------
	 * <br>
	 * 
	 * Viene restituito un array di oggetti ProgramPathFieldIO
	 * 
	 * 
	 * @param ProgramPath path
	 */
	public ProgramPathFieldIO[] pathFieldsIO(ProgramPath path) {
		ProgramPathFieldIO arPathFieldIO[] = null;
		ProgramPathFieldIO pathField = null;
		Instruction instr = null;
		ProgramCobolEntry<? extends Instruction>[] arProcEntry = null;
		ProgramCobolEntry<? extends Instruction> procEntry = null;
		InnerSymbolEntry[] arFieldInput = null;
		InnerSymbolEntry[] arFieldOutput = null;
		TreeMap<String, ProgramPathFieldIO> mapFieldIO = null;
		EnumSymbolType symbolType = null;
		int ni = 0;
		int i = 0;
		int mapSize = 0;

		mapFieldIO = new TreeMap<>();

		arProcEntry = program.entriesProcedure();
		procEntry = arProcEntry[0];

		for (ProgramPathEntry pathEntry : path.getEntries()) {
			ni = pathEntry.getNumInstr();
			procEntry = arProcEntry[ni];
			instr = (Instruction) procEntry.getInstruction();
			arFieldInput = instr.symbolsUsedInput();
			arFieldOutput = instr.symbolsUsedOutput();

			// Scan input fields referenced
			for (InnerSymbolEntry xrefField : arFieldInput) {
				symbolType = this.program.symbolType(xrefField.symbolName);
				if (symbolType != EnumSymbolType.COBOL_SYMBOL_DATA_ITEM) {
					continue;
				}				
				pathField = mapFieldIO.get(xrefField.symbolName);
				if (pathField == null) {
					pathField = new ProgramPathFieldIO();
					mapFieldIO.put(xrefField.symbolName, pathField);
					pathField.setFieldName(xrefField.symbolName);
					setSectionInstrDef(pathField);
				}
				pathField.getXrefInput().add(ni);
			}

			// Scan output fields referenced
			for (InnerSymbolEntry xrefField : arFieldOutput) {
				symbolType = this.program.symbolType(xrefField.symbolName);
				if (symbolType != EnumSymbolType.COBOL_SYMBOL_DATA_ITEM) {
					continue;
				}
				pathField = mapFieldIO.get(xrefField.symbolName);
				if (pathField == null) {
					pathField = new ProgramPathFieldIO();
					mapFieldIO.put(xrefField.symbolName, pathField);
					pathField.setFieldName(xrefField.symbolName);
					setSectionInstrDef(pathField);
				}
				pathField.getXrefOutput().add(ni);
			}
		}

		mapSize = mapFieldIO.entrySet().size();
		arPathFieldIO = new ProgramPathFieldIO[mapSize];
		i = 0;
		for (Entry<String, ProgramPathFieldIO> pathFieldIO : mapFieldIO.entrySet()) {
			arPathFieldIO[i] = pathFieldIO.getValue();
			i++;
		}
		return arPathFieldIO;
	};

	/*
	 * Completa informazioni sul campo, Working, Linkage, FD e numero istruzione
	 */
	private void setSectionInstrDef(ProgramPathFieldIO pathField) {
		ProgramCobolEntry<? extends Instruction>[] arEntriesData = null;
		ProgramCobolEntry<? extends Instruction> entryData = null;
		int[] arDataNumInstr = null;
		arEntriesData = this.program.entriesData();
		arDataNumInstr = this.program.getXrefSymbolDefData(pathField.getFieldName());

		// Simbolo non definito (errore di programma)
		// Simbolo definito ma non trovata definizione (Label/Section/Literal)
		if (arDataNumInstr == null) {return;}
		if (arDataNumInstr.length == 0) {return;}

		entryData = arEntriesData[arDataNumInstr[0]];
		pathField.setNumInstrDef(arDataNumInstr[0]);
		if (entryData.getProgramSection() == EnumCobolReservedWords.DATA_DIV_WS_SECTION) {
			pathField.setSection("W");
		} else if (entryData.getProgramSection() == EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION) {
			pathField.setSection("L");
		} else if (entryData.getProgramSection() == EnumCobolReservedWords.DATA_DIV_FILE_SECTION) {
			pathField.setSection("F");
		} else {
			pathField.setSection("X");
		}
	}





	/*
	 * Restituisce true se il campo è in input nel path <br>
	 * 
	 * @param ProgramPath path
	 * @param fieldName
	 */
	public boolean isFieldInputInPath(ProgramPath path, String fieldName) {
		ProgramPathFieldIO[] arPathFieldIO = null;
		arPathFieldIO = pathFieldsIO(path);

		// Scan Campi referenziati nel path
		for (ProgramPathFieldIO pathFieldIO : arPathFieldIO) {
			if (pathFieldIO.getFieldName().equals(fieldName) && pathFieldIO.getXrefInput().size() > 0) {
				return true;
			}
		}		
		return false;
	};

	/* ---------------------------------------------------------
	 * Restituisce true se il campo è in output nel path <br>
	 * ---------------------------------------------------------
	 * 
	 * @param ProgramPath path
	 * @param fieldName
	 */
	public boolean isFieldOutputInPath(ProgramPath path, String fieldName) {
		ProgramPathFieldIO[] arPathFieldIO = null;
		arPathFieldIO = pathFieldsIO(path);

		// Scan Campi referenziati nel path
		for (ProgramPathFieldIO pathFieldIO : arPathFieldIO) {
			if (pathFieldIO.getFieldName().equals(fieldName) && pathFieldIO.getXrefOutput().size() > 0) {
				return true;
			}
		}		
		return false;
	};

	/* ---------------------------------------------------------------------------------------------
	 * Restituisce true se il campo espresso dal suo numero di istruzione è in input nel path <br>
	 * ---------------------------------------------------------------------------------------------
	 * 
	 * @param ProgramPath path
	 * @param fieldNumInstr
	 */
	public boolean isFieldInputInPath(ProgramPath path, int fieldNumInstr) {
		ProgramCobolEntry<? extends Instruction>[] arEntriesData = null;
		ProgramCobolEntry<? extends Instruction> entryData = null;
		InstructionCobolDataItem dataItem = null;
		String fieldName = "";

		arEntriesData = this.program.entriesData();
		entryData = arEntriesData[fieldNumInstr];
		dataItem = (InstructionCobolDataItem) entryData.getInstruction();
		fieldName = dataItem.getDataName();

		return isFieldInputInPath(path, fieldName);
	};

	/* ---------------------------------------------------------------------------------------------
	 * Restituisce true se il campo espresso dal suo numero di istruzione è in output nel path <br>
	 * ---------------------------------------------------------------------------------------------
	 * 
	 * @param ProgramPath path
	 * @param fieldNumInstr
	 */
	public boolean isFieldOutputInPath(ProgramPath path, int fieldNumInstr) {
		ProgramCobolEntry<? extends Instruction>[] arEntriesData = null;
		ProgramCobolEntry<? extends Instruction> entryData = null;
		InstructionCobolDataItem dataItem = null;
		String fieldName = "";

		arEntriesData = this.program.entriesData();
		entryData = arEntriesData[fieldNumInstr];
		dataItem = (InstructionCobolDataItem) entryData.getInstruction();
		fieldName = dataItem.getDataName();

		return isFieldOutputInPath(path, fieldName);
	};

	/*
	 * ---------------------------------------------------------------------
	 * Generazione possibili catene di trasformazione di un campo elementare 
	 * ---------------------------------------------------------------------
	 * 
	 * Viene restituito un ArrayList di catene di trasformazione, in un ArrayList.
	 * 
	 * Generazione possibili catene di trasformazioni elementari per il campo 
	 * fino a ultima trasformazione origine (la prima in sequenza) o input da media esterni
	 * 
	 * Il metodo restituisce tutte le specifiche catene di trasformazione generate per il campo elemetare in input (subField).
	 * Tuttavia tutte le informazioni, le trasformazioni, il loro stato, i pathe che li coprono e i valori individuati
	 * sono ottenibili a partire dal campo dinamico ilpw.dynamicFieldToSolve e turtti i suoi sottocampi.
	 * Metodo richiamato da web service e da LogicSamePgm.
	 * 
	 * Classe di work LogicWorkProcess fornita in input già allocata e inizializzata da metodo statico di questa classe.
	 * 
	 * 
	 * ---------------------------------------------------------------------------
	 * Ricerca e aggiornamento catene di trasformazione dei sottocampi elementari.
	 * ---------------------------------------------------------------------------
	 * 
	 * 
	 * Viene aggiornato direttamente il descrittore del sottocampo LogicDynamicSubField.
	 * Vengono tracciate tutte le assegnazioni all'indietro del sottocampo in input
	 * fino a quando l'operando in input ha un valore potenzialmente risolvibile in quanto:
	 * 
	 * 1) E' una literal
	 * 2) E' un campo che non è referenziato in output e ha un valore iniziale
	 * 3) E' un campo indicizzato che rappresenta un elemento di tabella Occurs
	 * 4) E' un campo non risolvibile nello stesso programma in quanto campo di:
	 * 4.1) Using parm di programma
	 * 4.2) Linkage Section
	 * 4.3) Valorizzato da istruzione di Read Cobol
	 * 4.4) Sistema Cics come EIBTRMID, EIBTRNID
	 * 4.5) Valorizzato da istruzione di Read Cics (Vsam, Ts, Td) 
	 *  
	 * L'attività di ricerca delle catene di trasformazione è un processo ricorsivo che può
	 * diversificare le catene di trasformazione generate. 
	 * Infatti un campo può essere in output in punti diversi del programma. 
	 * Ogni punto genera una diversa catena di trasformazione, ricorsivamente.
	 * 
	 * In questa fase non si tiene conto della validità delle catene di trasformazione
	 * individuate. Vengon estratte tutte quelle possibili e successivamente vengono
	 * validate rispetto ai path effettivi di esecuzione.	 
	 *  
	 * 
	 * @param dataField field as data instruction to find transformations of
	 * @param fromNumInstr Starting instruction to consider, normally Procedure Division statement
	 * @param toNumInstr ending instruction to consider to find all possible pexecution paths
	 * @return ArrayList<ArrayList<LogicDynamicFieldSubSetting>>
	 */
	public ArrayList<ArrayList<LogicDynamicFieldSubSetting>>  chainSet(LogicWorkProcess ilpw, LogicDynamicFieldSub dynamicFieldSub, int fromNumInstr, int toNumInstr) throws ExceptionAmrita, SQLException {

		ilpw.dynamicFieldSubToSolve = dynamicFieldSub;

		DataItemCobolIdentifier identifierReceiver = null;                   		   		   			// Descrittore campo di cui trovare il valore

		// Impostazioni di partenza: primo receiver iniziale con il suo default + ..
		identifierReceiver = ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField;
		ilpw.identifierReceiver = identifierReceiver;													// Receiver iniziale da cui startare la ricerca valori
		addValueDefaultIfNeeds(ilpw);                                                                   // Gestione inserimento valore default (anche in struttura db)
		ilpw.numInstrSet = ilpw.instrDynamicOrigin.getNumInstr();                                     	// Di default l'impostazione è dovuta all'istruzione dinamica
		ilpw.numInstrSetOrigin = ilpw.instrDynamicOrigin.getNumInstr();                               	// Di default l'impostazione è dovuta all'istruzione dinamica

		ilpw.al_al_chainSetSubFieldAll = new ArrayList<ArrayList<LogicDynamicFieldSubSetting>> ();		// Catene di trasformazioni individuate
		ilpw.al_chainSetSubFieldCur = new ArrayList<LogicDynamicFieldSubSetting> ();					// Catena di trasformazioni corrente di lavoro
		ilpw.map_identifierReceiverManaged.clear();                                             		// Map <nome campo, identificatore> per loop prevention
		
		retrieveChainsSetSubFieldRecursive(ilpw);													    // Estrazione catene di trasformazione per il subfield 

		// Catene trasformazioni sottocampo in ilpw.dynamicFieldSubOrigin.al_al_chainSetSubField
		cloneAllEntityDynamicFieldSubSetting(ilpw);
		return ilpw.dynamicFieldSubToSolve.al_al_chainSetSubField;
	}

    /*
     * Inserimento valore di default se presente a livello di sottocampo
     * Come valore string in array list
     * come oggetto LogicDynamicValue che contiene le strutture per inserimento db EntityDynamicFieldSubValue
     */
	private void addValueDefaultIfNeeds(LogicWorkProcess ilpw) {
		EntityDynamicFieldSubValue entityDynamicSubFieldValue = null;
		LogicDynamicValue logicDynamicValue = null;
		String valueDefault = "";
		
		valueDefault = ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getDataItem().getValueStringFormattedToSize();
		
		// Sottocampo senza value
		if (valueDefault.isBlank()) {
			return;
		}
		
		// Valore di default a livello di sottocampo
		ilpw.dynamicFieldSubToSolve.valueDefault = ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getDataItem().getValueStringFormattedToSize();
		
		// Informazioni valore X struttura db
		entityDynamicSubFieldValue = storeInfoDbSingleValue(ilpw);
		entityDynamicSubFieldValue.setPosInSubField(1); 											 // Posizione in sottocampo (1-based) inizio valore di questa assegnazione
		entityDynamicSubFieldValue.setLngInSubField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getSizeSubField()); // Lunghezza sottocampo 
		entityDynamicSubFieldValue.setDefaultValue(true);			                                 // E' un valore di default		
		entityDynamicSubFieldValue.setValue(valueDefault);          							 	 // Valore sottocampo o campo 
		entityDynamicSubFieldValue.setTypeObjectFrom(EnumObject.OBJECT_PGM_COBOL);  				 // Tipo oggetto (pgm, File,  etc) dove il valore è stato impostato
		entityDynamicSubFieldValue.setIdObjectFrom(ilpw.programCur.programName);	  					 // Nome oggetto (pgm) dove il valore è stato impostato
		entityDynamicSubFieldValue.setIdPgmFrom(ilpw.programCur.programName);							 // Pgm di assegnazione corrente
		entityDynamicSubFieldValue.setNumInstrFrom(0);			                                     // Nessun Numero istruzione di assegnazione		
		
		logicDynamicValue = new LogicDynamicValue(program.getSysOwner(), program.getSubSysOwner());		
		logicDynamicValue.isDefaulValue = true;                                     				 // Nessuna istruzione che ha generato il valore nel programma corrente
		logicDynamicValue.value = valueDefault;														 // Valore campo o sottocampo
		logicDynamicValue.pgmSet = ilpw.programCur.programName;                        				 // Programma di assegnazione del valore	
		logicDynamicValue.numChainSet = 0;                                          				 // Per chiarezza
		logicDynamicValue.numInstrSet = 0;       													 // Per chiarezza
		logicDynamicValue.entityDynamicValue = entityDynamicSubFieldValue;          				 // Save in struttura dinamica valore
		
		ilpw.dynamicFieldSubToSolve.al_value.add(logicDynamicValue);                                 // Save valore in struttura sottocampo da risolvere                   
		
	}

	/*
	 * ------------------------------------------------------------------------
	 * Creazione catene di trasformazioni elementari con attivazione ricorsiva
	 * ------------------------------------------------------------------------
	 * 
	 * A partire dal sottocampo e dall'istruzione dinamica in cui è referenziato,
	 * vengono individuate tutte le possibili catene di trasformazione, dallo
	 * statement di procedure division fino all'istruzione dinamica,
	 * che trasformano il sottocampo con assegnazioni successive.
	 * Le trasformazioni vengono individuate a partire dall'ultima, all'indietro.
	 * 
	 * Questo metodo viene richiamato ricorsivamente a fronte di ogni trasformazione intermedia.
	 * 
	 * Ogni catena di trasformazione può individuare, come ultima (prima) assegnazione:
	 * 
	 * 1) una literal
	 * 2) Un campo che non è referenziato in output e ha un valore iniziale
	 * 3) Un campo indicizzato che rappresenta un elemento di tabella Occurs
	 * 4) Un campo non risolvibile nello stesso programma in quanto campo di:
	 * 4.1) Using parm di una Call
	 * 4.2) Using parm di programma
	 * 4.3) Linkage Section
	 * 4.4) Valorizzato da istruzione di Read Cobol
	 * 4.5) Sistema Cics come EIBTRMID, EIBTRNID
	 * 4.6) Valorizzato da istruzione di Read Cics (Vsam, Ts, Td) 
	 *  
	 * Assegnazioni multiple stesso campo
	 * ----------------------------------
	 * 
	 * La prima operazione effettuata, è quella di individuare tutte le assegnazioni
	 * che valorizzano in output il campo receiver fornito, per ottenere i campi sender.
	 * Ogni assegnazione sullo stesso campo, genera ricorsivamente una catena di 
	 * trasformazioni diversa per ogni campo sender individuato. 
	 * Dal momento che in questa fase non si tiene conto dei path di esecuzione effettivi, 
	 * si possono generare delle catene di trasformazione  inconsistenti, 
	 * ovvero è possibile che nei path di esecuzione, validi per la catena di trasformazione, 
	 * ci siano assegnazioni successive dello stesso campo. 
	 * Si dovrà verificare, per ogni trasformazione, che in nessun path, ci siano istruzioni 
	 * di assegnazione SUCCESSIVE. 
	 * 
	 * 
	 * Call fra due assegnazioni
	 * -------------------------
	 * 
	 * E' possibile che fra due assegnazioni, per esempio due Move, dove la prima aggiorna
	 * un campo e la seconda utilizza quel campo in input, ci siano una o più statemets Call,
	 * che utilizzano direttamente o indirettamente quel campo, che potrebbe pertanto essere
	 * potenzialmente aggiornato dalla catena di programmi chiamati.
	 * Il campo viene dichiarato esplicitamente come parametro nella using oppure è definito 
	 * sotto qualche gruppo di uno dei parametri. 
	 * E' necessario seguire le trasformazioni nella catena dei programmi chiamati. 
	 * Tale catena potrebbe contenere valorizzazioni via Read, Exec Cics Read ... o altro. 
	 * L'informazione di Call potenzialmente influente ai fini dell'ultima impostazione,
	 * viene memorizzata nella struttura di ultima assegnazione successiva alla Call.
	 * Viene quindi generata una catena di trasformazione che inizia con la Call in
	 * questione, come lastSet. L'attivazione di LogicSpreadedPgm
	 * tenterà di risolvere nel programma richiamato con Call, e nella sua catena di
	 * chiamati. Se non ci sono valorizzazioni valide nella catena dei programma
	 * chiamati, non è da considerarsi un errore: semplicemente i programmi chiamati
	 * non aggiornano il campo in questione.
	 * 
	 * 
	 * Ricorsivita
	 * -----------
	 * 
	 * Questo metodo viene attivato ricorsivamente nel caso di assegnazioni 
	 * (xref) del campo receiver e di ogni suo receiver implicito. 
	 * In questo caso viene creata una nuova catena di trasformazioni indipendente, 
	 * per ogni assegnazione individuata. 
	 * 
	 * -------------------------------------------------------
	 * Gestione assegnazione da campo a campo via Move
	 * -------------------------------------------------------
	 * 
	 * Viene tracciata la catena di trasformazioni effettuata tramite assegnazioni, 
	 * in Cobol Move. Sono utilizzati il sender, il receiver e il numero di istruzione.

	 * Viene semplicemente tracciata la trasformazione del  dato elementare, che viene
	 * inserita nella catena di trasformazioni corrente. Ogni trasformazione sposta
	 * l'attenzione sul Sender, che determina i nuovi Receiver di cui trovare l'assegnazione
	 * finale, ricorsivamente.
	 * 
	 * Oggetto della trasformazione è sempre la modifica diretta o indiretta del sottocampo origine.
	 * La trasformazione può anche essere parziale, ovvero modificare una porzione del sottocampo
	 * che non inizia dalla prima posizione e non termina sull'ultima.
	 * 
	 * Nel caso il receiver sia espresso con reference modification, si verifica che l'assegnazione
	 * influenzi il sottocampo origine. Viene presa in considerazione l'assegnazione se la posizione
	 * del sender + la posizione di reference modification, è all'interno del sotto campo dinamico 
	 * da risolvere.
	 * 
	 * Il Receiver è il campo assegnato da un campo Sender in una move. 
	 * Il primo campo Receiver è inizialmente il sottocampo nell'istruzione dinamica.
	 * Al campo Receiver è sempre associata una pos/lng correnti nel sottocampo in esame (da posizione 1-based per lunghezza).
	 * Al campo Receiver e al Sender può essere associata una (pos:lng) di reference modification Cobol,
	 * che si legge da posizione 1-based per lunghezza.
	 * Per il campo Receiver viene sempre valutata la compatibilità di pos/lng con (pos:lng).
	 * Al primo sottocampo Receiver nell'istruzione dinamica viene assegnata pos=1 e lng=size sottocampo.
	 * Nel processo ricorsivo di assegnazione il campo Receiver può assumere una pos <> 1 e una
	 * lunghezza <> da quella del sottocampo origine.
	 * 
	 * Nell'ultima assegnazione, a fronte di literal, si utilizza la posizione e lunghezza ereditate
	 * ricorsivamente per estrarre la porzione di valore iniziale.
	 * Si può quindi affermare che ogni Receiver eredita una pos/lng dalle assegnazioni precedenti.
	 * La lunghezza utile del receiver può solo diminuire rispetto alla lunghezza del sottocampo
	 * originario.
	 * 
	 * 
	 * Terminologia:
	 * -------------
	 * 
	 *  1) lngSubField
	 *     E' la lunghezza in bytes del sottocampo dinamico di origine. Vengono tracciate le trasformazioni
	 *     tenendo sempre conto di questa lunghezza utile origine.
	 *  
	 *  2) Sender
	 *     2.1  (posSnd:lngSnd)
	 *          Reference modification Sender. 
	 *          Se non espliciti sono impostati a (1:size).
	 *          
	 *  3) Receiver
	 *     3.1  (posRcv:lngRcv)
	 *          Reference modification Receiver. 
	 *          Se non espliciti sono impostati a (1:size).
	 *          
	 *     3.2  posRcv/lngRcv
	 *          Porzione receiver interessata alla trasformazione. 
	 *          Se non espliciti sono impostati a (1:lngSubField).
	 *          
	 * 
	 * Algoritmo di soluzione
	 * ----------------------
	 * 
	 * 0) Varie.
	 *    0.1) Sender di gruppo.
	 *         Se il sender è un campo di gruppo, si calcolano pos/lng per ogni campo definito nel gruppo da (posSnd:) 
	 *         e si attiva come nuovo receiver.
	 *         La nuova attivazione viene effettuata sul nuovo nome di campo, posizione calcolata e lunghezza origine.
	 *    0.2) Sender ridefinito/rinominato
	 *         Se esistono dei campi che ridefiniscono il gruppo o lo rinominano, si calcolano pos/lng per ogni campo
	 *    	   e si attiva come nuovo receiver.
	 *         La nuova attivazione viene effettuata sul nuovo nome di campo, posizione calcolata e lunghezza origine.
	 *    0.3) Reference Modification.
	 *         A livello di trasformazione, sia per il sender sia per il receiver, si memorizzano sempre posizione
	 *         e lunghezza di reference modification (pos:lng) anche se non sono indicate esplicitamente nell'istruzione.
	 *         In questo caso vengono impostate a (1:size()). Viene gestito un flag per indicare che il ref mode del
	 *         sender o del receiver era stato effettivamente codificato nell'istruzione.
	 *    0.4) Controlli.
	 *         Si controlla sempre che posRcv e lngRcv siano compatibili con (posRcv:lngRcv) e se non lo sono
	 *         la trasformazione non ha effetto.
	 *         Si controlla che la lunghezza del campo Sender sia >= a quella origine e se non, la trasformazione
	 *         non si considera.
	 * 1) Receiver (posRcv:) >= Recever posRcv + lngRcv
	 *    L'assegnazione NON ha effetto sulla porzione di Receiver interessata, deve essere scartata
	 * 2) Receiver (posRcv:) + (:lngRcv) <= Recever posRcv 
	 *    Il campo receiver risulta assegnato a SPACES.
	 * 3) Receiver (posRcv:) = Recever posRcv 
	 *    Il nuovo campo receiver sarà il campo sender da (posSnd:lngOrigine)
	 * 4) Caso non coperto dai casi 2/3/4
	 *    Situazione ibrida NON risolvibile in modo generale.
	 *      
	 *  
	 * A titolo esplicativo segue un esempio di trasformazioni intercettate da questo
	 * metodo. Le trasformazioni dei campi C, B1 e B2 NON rientrano nelle casistiche
	 * delle altre tipologie di ultime assegnazioni.
	 * 
	 * 
	 * MOVE C(2:3) TO B1
	 * ..
	 * MOVE B1 TO A
	 * ..
	 * MOVE B2 TO A
	 * ..
	 * EXEC CICS LINK PGMRID(A)
	 * 	 
	 */
	private void retrieveChainsSetSubFieldRecursive(LogicWorkProcess ilpw) throws ExceptionAmrita, SQLException {

		LogicWorkProcess ilpwRecursive = null;                        			// Nuovo contesto per attivazione ricorsiva
		ArrayList<LogicDynamicFieldSubSetting> al_chainSetSubFieldRecursive = null;   //
		ArrayList<LogicDynamicFieldSubSetting> al_chainSetSubFieldSave = null;   //
		EnumLogicSetMode setMode = null;                                         //
		@SuppressWarnings("unused")
		int ar_XrefProcReceiverInput[] = null;									// Xref utilizzi in input  (per individuare le Call)
		int ar_XrefProcReceiverOutput[] = null;									// Xref utilizzi in output (per individuare le Move, Read ..)
		String receiverUnderGroupOf = "";                                    	// Gruppo sotto il quale è definito il receiver corrente
        String idReceiver = "";

		// Inizializzazione
		ilpw.lastSetManaged = false;                        
		idReceiver = ilpw.identifierReceiver.getNameIdentifier();
		
		// Ciclo generale trasformazioni sottocampo fino alla trasformazione finale (last set)
		// risolvibile nel programma oppure da risolvere nei programmi chiamanti.
		// Le trasformazioni sono tipicamente a fronte di assegnazioni Move.

		// Recupero numeri istruzione dove il data item è referenziato in input e in output
		if (ilpw.identifierReceiver.getIdentifierType() == EnumCobolReservedWords.DATA_DIV_DATA_ITEM) {
			// Data item definito in modo univoco e non referenziato con clausola specifica OF  
			if (!ilpw.identifierReceiver.getQualifier().isUnderGroupDeclared()) {
				ar_XrefProcReceiverInput  = ilpw.programCur.xrefToDataItemInProcedure(ilpw.identifierReceiver.getNumInstr(), INSTR_USE_DATA_ITEM_INPUT);
				ar_XrefProcReceiverOutput = ilpw.programCur.xrefToDataItemInProcedure(ilpw.identifierReceiver.getNumInstr(), INSTR_USE_DATA_ITEM_OUTPUT);
			}

			// Data item definito sotto un gruppo e referenziato con clausola specifica OF  
			if (ilpw.identifierReceiver.getQualifier().isUnderGroupDeclared()) {
				receiverUnderGroupOf = ilpw.identifierReceiver.getQualifier().getGroupNameField();
				ar_XrefProcReceiverInput  = ilpw.programCur.xrefToDataItemInProcedureOf(ilpw.identifierReceiver.getNameIdentifier(), receiverUnderGroupOf, INSTR_USE_DATA_ITEM_INPUT);
				ar_XrefProcReceiverOutput = ilpw.programCur.xrefToDataItemInProcedureOf(ilpw.identifierReceiver.getNameIdentifier(), receiverUnderGroupOf, INSTR_USE_DATA_ITEM_OUTPUT);
			}
		}

		//-------------------------------------------------------------------------------------------------
		// (1) 	Data item receiver NON referenziato in output.
		// 		Le assegnazioni di trasformazione del sottocampo oppure il sottocampo originario stesso, 
		// 		hanno portato ad aree Cobol origine che NON possono essere risolte nello stesso
		// 		programma (Using e Linkage). Oppure a campi inizializzati con Value di cui si possono 
		// 		individuare i valori.
		// 		Si individua la natura dell'origine campo e si imposta come ultima assegnazione.
		//-------------------------------------------------------------------------------------------------

		if (ar_XrefProcReceiverOutput == null) {

			ilpw.lastSetManaged = false;                        // Last set non individuata per default

			// Valori receiver NON risolvibili nel programma o a fronte di default value 
			ilpw.lastSetManaged = lastSetByValueDefault     (ilpw);           					// spaces/zero/'literal'/num/        
			ilpw.lastSetManaged = lastSetByUsingParm   		(ilpw, ilpw.identifierReceiver);    // Procedure division using field
			ilpw.lastSetManaged = lastSetByLinkage	   		(ilpw, ilpw.identifierReceiver);    // Linkage field
			ilpw.lastSetManaged = lastSetByCicsSystem  		(ilpw, ilpw.identifierReceiver);    // EIBTRMID, EIBTRNID, DFHCOMMAREA, TWA, ...
			ilpw.lastSetManaged = lastSetByCobolRead   		(ilpw, ilpw.identifierReceiver);    // Field in Ioarea di FD file
		    ilpw.lastSetManaged = lastSetByCicsRead   		(ilpw, ilpw.identifierReceiver);    // VsamField|TsField|TdField of IOAREA ..
			ilpw.lastSetManaged = lastSetBySqlSelect  		(ilpw, ilpw.identifierReceiver);    // Table ColumnName of IOAREA ...

			// Individuata ultima trasformazione sicuramente valida.
			// Accodo catana trasformazione corrente a ArrayList catene trasformazione cumulative.
			// Accodo valore individuato in struttura dinamica.
			if (ilpw.lastSetManaged) {
				ilpw.al_chainSetSubFieldCur.add(ilpw.logicDynamicFieldSubSettingWrk);
				al_chainSetSubFieldSave =  cloneChainSetSubField(ilpw.al_chainSetSubFieldCur);	
				ilpw.dynamicFieldSubToSolve.al_al_chainSetSubField.add(al_chainSetSubFieldSave);
				ilpw.dynamicFieldSubToSolve.al_lastSetTotal.add(ilpw.logicDynamicFieldSubSettingWrk);   // Ultima assegnazione NON di trasformazione comune a tutte le catene
				setNumChain(ilpw.dynamicFieldSubToSolve.al_al_chainSetSubField.size() - 1, ilpw.al_chainSetSubFieldCur);
				return;       						// return back nel processo ricorsivo
			} // end-if

			// Successiva eventuale gestione receiver impliciti.
			// Si inserisce in map perchè un receiver implicito potrebbe includere il campo
			ilpw.map_identifierReceiverManaged.put(idReceiver, ilpw.identifierReceiver);  
			
		} // end-if


		//-----------------------------------------------------------------------------------------------------------------
		// (2)	Data item receiver referenziato esplicitamente in output.
		// 		Si può trattare di ultima assegnazione da Move o media (Vsam, Sql, Ts,..)
		// 		Scan Xref data item in output (receiver)
		//-----------------------------------------------------------------------------------------------------------------
		
		if (ar_XrefProcReceiverOutput != null) {
			
			// Scan Xref data item in output (receiver)
			for (int i = 0; i < ar_XrefProcReceiverOutput.length; i++) {
				
				ilpw.numInstrSet = ar_XrefProcReceiverOutput[i];				
				ilpw.lastSetManaged = false;                        								

				// Recupero istruzione di setting se istruzione Cobol nativa
				ilpw.instrCobolProcSet = null;
				ilpw.cobolProcEntry = ilpw.programCur.entryProcedure(ilpw.numInstrSet);
				if (ilpw.cobolProcEntry.getEntryType() == EnumInstrDataCategory.COBOL_PROC_INSTRUCTION) {
					ilpw.instrCobolProcSet = (InstructionCobolProcedure) ilpw.cobolProcEntry.getInstruction();
				}

				// Move: attivazione a partire dal sender in Move
				if (ilpw.instrCobolProcSet != null 
				&&  ilpw.instrCobolProcSet.getTypeInstr() == EnumCobolReservedWords.PROC_MOVE) {
		
					ilpw.identifierSender = ilpw.instrCobolProcSet.moveGetIdentifierFrom();
					ilpw.identifierReceiver = ilpw.instrCobolProcSet.moveGetIdentifierTo(ilpw.identifierReceiver.getNameIdentifier());

					// Ultima assegnazione A fronte di MOVE 
					ilpw.lastSetManaged = lastSetByMoveLiteralSpaceZero (ilpw, ilpw.identifierSender);        // MOVE spaces/zero/'literal'/num/ TO ...
					ilpw.lastSetManaged = lastSetByMoveTbItemOfOccurs	(ilpw, ilpw.identifierSender);   	  // MOVE tbItem(idx...) TO ...
					ilpw.lastSetManaged = lastSetByCobolRead   			(ilpw, ilpw.identifierSender);        // MOVE fileField TO ...
					ilpw.lastSetManaged = lastSetByCicsRead   			(ilpw, ilpw.identifierSender);        // MOVE vsamField|TsField|TdField TO ...
					ilpw.lastSetManaged = lastSetBySqlSelect  			(ilpw, ilpw.identifierSender);        // MOVE columnName TO ...

					if (ilpw.lastSetManaged) {
						ilpw.al_chainSetSubFieldCur.add(ilpw.logicDynamicFieldSubSettingWrk);
						ilpw.al_chainSetSubFieldCur = cloneChainSetSubField(ilpw.al_chainSetSubFieldCur);
						al_chainSetSubFieldSave = new ArrayList<LogicDynamicFieldSubSetting>(ilpw.al_chainSetSubFieldCur);   // Per Salvare ultimo elemento 
						ilpw.dynamicFieldSubToSolve.al_al_chainSetSubField.add(al_chainSetSubFieldSave);
						ilpw.dynamicFieldSubToSolve.al_lastSetTotal.add(ilpw.logicDynamicFieldSubSettingWrk);   // Ultima assegnazione NON di trasformazione comune a tutte le catene
						setNumChain(ilpw.dynamicFieldSubToSolve.al_al_chainSetSubField.size() - 1, ilpw.al_chainSetSubFieldCur);
						// Remove ultima assegnazione di trasfromazione da catena precedente
						ilpw.al_chainSetSubFieldCur.remove(ilpw.al_chainSetSubFieldCur.size() - 1);
						ilpw.map_identifierReceiverManaged.put(idReceiver, ilpw.identifierReceiver); 
						continue;                             // tratta successivo xref in output
					}		
					
					// Non era una Move di ultima assegnazione: verifico se trasformazione valida 
					setMode = checkSetTransformationMove(ilpw);
					
					// Assegnazione da scartare
					if (setMode == EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_BEYOND_DISCARDED
					||  setMode == EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_ACROSS_DISCARDED
					||  setMode == EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_REF_MOD_DISCARDED) {
						continue;
					}	
					
                    // Move di assegnazione/trasformazione da tracciare
					// Inserimento trasformazione in catena corrente
					ilpw.curLastSet = setMode;                                                       // NON è l'ultima assegnazione ma solo una trasformazione
					storeInfoLogicSettingMove(ilpw);                  								 // Store Informazioni trasformazione in ilpw.logicDynamicFieldSubSetting  			 
					ilpw.al_chainSetSubFieldCur.add(ilpw.logicDynamicFieldSubSettingWrk);            // Accoodamento Move di trasformazione                  				

					// Nuovi valori correnti per pos/lng receiver e sottocampo origine
					computeNextSubFieldPosLng(ilpw);                                                // Calcolo nuova posizione/lunghezza calcolati in base a volori correnti e reference mod
					computeNextReceiverPosLng(ilpw);                                                // Calcolo nuova posizione/lunghezza calcolati in base a volori correnti e reference mod

					// Loop prevention: futuro receiver esplicito (il sender) già trattato ricorsivamente a questo numero di istruzione: bypass
					if (ilpw.map_identifierReceiverManaged.get(ilpw.identifierSender.getNameIdentifier()) != null) {continue;}
					
					ilpw.numInstrSetOrigin = ilpw.numInstrSet;                                      							 
	
					// Nuovo contesto ricorsivo clonato da quello corrente
					// Nuova catena corrente per processo ricorsivo clonata CON ultima Move di trasformazione
					ilpwRecursive = cloneContextRecursive(ilpw);
		 			if (ar_XrefProcReceiverOutput.length > 1) {
						ilpw.al_chainSetSubFieldCur = cloneChainSetSubField(ilpw.al_chainSetSubFieldCur);	
					}
					al_chainSetSubFieldRecursive = new ArrayList<LogicDynamicFieldSubSetting>(ilpw.al_chainSetSubFieldCur);
					al_chainSetSubFieldSave = new ArrayList<LogicDynamicFieldSubSetting>(ilpw.al_chainSetSubFieldCur);
					ilpwRecursive.al_chainSetSubFieldCur = al_chainSetSubFieldRecursive;
					ilpwRecursive.identifierReceiver = ilpw.identifierSender;		  				// Nuovo receiver esplicito candidato principale da trattare ricorsivamente				
														
					// Settings for next loop prevention
					ilpw.map_identifierReceiverManaged.put(idReceiver, ilpw.identifierReceiver);
					
					retrieveChainsSetSubFieldRecursive(ilpwRecursive);								// Attivazione ricorsiva
					
					// Remove ultima Move di trasfromazione da catena precedente e ripristino catena attiva
					al_chainSetSubFieldSave.remove(al_chainSetSubFieldSave.size() - 1);
					ilpw.al_chainSetSubFieldCur = al_chainSetSubFieldSave;
					continue;    																	// Next assegnazione su stesso receiver ma diverso Sender

				// NON è move: verifica se Read/CicRead/Ts/Td/Select 
				} else {					
					// Valori receiver NON risolvibili nel programma da media esterni
					ilpw.lastSetManaged = lastSetByCobolRead   (ilpw, ilpw.identifierReceiver);
					ilpw.lastSetManaged = lastSetByCicsRead    (ilpw, ilpw.identifierReceiver);
					ilpw.lastSetManaged = lastSetBySqlSelect   (ilpw, ilpw.identifierReceiver);
				}

				// Campo receiver in output su istruzione non gestita (es. transform o altro) e NO accesso a dati esterni: skip
				if (!ilpw.lastSetManaged) {
					continue;     																	// Next assegnazione su stesso receiver ma diverso Sender
				}

				// Individuata ultima trasformazione (o accesso a dati esterni) con assegnazione valori: accodo catena trasformazioni corrente a ArrayList catene trasformazione cumulative
				// Accodamento catena a catene complessive individuate
				ilpw.al_chainSetSubFieldCur.add(ilpw.logicDynamicFieldSubSettingWrk);
				al_chainSetSubFieldSave = cloneChainSetSubField(ilpw.al_chainSetSubFieldCur);
				ilpw.dynamicFieldSubToSolve.al_al_chainSetSubField.add(al_chainSetSubFieldSave);
				ilpw.dynamicFieldSubToSolve.al_lastSetTotal.add(ilpw.logicDynamicFieldSubSettingWrk);
				setNumChain(ilpw.dynamicFieldSubToSolve.al_al_chainSetSubField.size() - 1, ilpw.al_chainSetSubFieldCur);
				// Nuova catena corrente clonata SENZA l'ultimo elemento di set					
				ilpw.al_chainSetSubFieldCur.remove(ilpw.al_chainSetSubFieldCur.size() - 1);

			} // end-for

		} // end-if
 

		//-------------------------------------------------------------------------------------------------
		// (3)	Attivazione ricorsiva su tutti i receiver impliciti   (Campi di gruppo, redefines etc)
		//-------------------------------------------------------------------------------------------------

		// Individividuazione eventuali successivi receiver impliciti  
		getAllNextReceiverImplicit(ilpw); 										

		// Update struttura valorizzata da Move precedente il cui sender è diventato il receiver corrente e già inserita in catena trasformazioni
		if (ilpw.al_nextReceiverImplicit.size() > 0) {
			
			ilpw.numChainSet = ilpw.dynamicFieldSubToSolve.al_al_chainSetSubField.size() - 1;

			// Scan campi receiver impliciti dedotti dal receiver corrente
			for (int j = 0; j < ilpw.al_nextReceiverImplicit.size(); j++) {
				// Loop prevention: receiver implicito già trattato ricorsivamente a partire da stessa istruzione, catena di trasformazione non risolvibile
				idReceiver = ilpw.al_nextReceiverImplicit.get(j).getNameIdentifier();
				if (ilpw.map_identifierReceiverManaged.get(idReceiver) != null) {continue;}
				// Receiver implicito non referenziato: skip preventivo
				if (ilpw.programCur.xrefToDataItemInProcedure(ilpw.al_nextReceiverImplicit.get(j).getNumInstr(), INSTR_USE_DATA_ITEM_OUTPUT) == null) {continue;}

				// Nuovo receiver implicito di cui trovare i valori ricorsivamente
				ilpw.map_identifierReceiverManaged.put(idReceiver, ilpw.al_nextReceiverImplicit.get(j));  
				ilpwRecursive = cloneContextRecursive(ilpw);							 // Nuovo contesto ricorsivo
				ilpwRecursive.identifierReceiver = ilpw.al_nextReceiverImplicit.get(j);  // Nuovo receiver da trattare ricorsivamente
				ilpwRecursive.posRcv = ilpw.al_nextReceiverImplicitPos.get(j);			 // Nuova posizione di inizio utile in receiver
				ilpwRecursive.lngRcv = ilpw.al_nextReceiverImplicitLng.get(j);			 // Nuova lunghezza da considerare nel receiver
				ilpwRecursive.posInSubField = ilpw.al_nextSubFieldImplicitPos.get(j);	 // Nuova posizione di inizio utile in sottocampo origine
				ilpwRecursive.lngInSubField = ilpw.al_nextSubFieldImplicitLng.get(j);	 // Nuova lunghezza di inizio utile in sottocampo origine
				ilpwRecursive.numChainSet = ilpw.numChainSet + 1;
				
				al_chainSetSubFieldSave = new ArrayList<LogicDynamicFieldSubSetting>(ilpw.al_chainSetSubFieldCur);
				ilpw.al_chainSetSubFieldCur = cloneChainSetSubField(ilpw.al_chainSetSubFieldCur);
				retrieveChainsSetSubFieldRecursive(ilpwRecursive);						 // Attivazione ricorsiva
				ilpw.al_chainSetSubFieldCur = al_chainSetSubFieldSave;

			} // end-for
		}

	} // end-method

	/*--------------------------------------
	 * Clone della catena di trasformazione 
	 * -------------------------------------
	 * 
	 * Le catene di trasformazione, fino a un eventuale split dovuto a reference in output multipli
	 * condividono gli stessi elementi di trasformazione.
	 * E' necessario tuttavia che ogni catena di trasformazione e in particolare entityDynamicFieldSubSetting siano oggetti sppecifici.
	 * L'esigenza è che il numero di catena sia specifico per ogni catena di trasformazione, che viene invece sovrascritto
	 * in caso di stesso oggetto.
	 * 
	 */
	private ArrayList<LogicDynamicFieldSubSetting> cloneChainSetSubField(ArrayList<LogicDynamicFieldSubSetting> al_chainSetSubFieldCur) {
    	ArrayList<LogicDynamicFieldSubSetting> al_chainSetSubFieldCloned = null;
    	LogicDynamicFieldSubSetting ldfss = null;
    	al_chainSetSubFieldCloned = new ArrayList<LogicDynamicFieldSubSetting>();
    	
    	// Scan trasformazioni
    	for (LogicDynamicFieldSubSetting set : al_chainSetSubFieldCur) {
    		ldfss = set.clone();
    		al_chainSetSubFieldCloned.add(ldfss);
		}
    	
		return al_chainSetSubFieldCloned;
	}

	/*
     * Debug current chains
     */
	@SuppressWarnings("unused")
	private void debug(LogicWorkProcess ilpw ) {
		String idFieldSub;
		int numChain = 0;
		int numChains = 0;
		int numChainSets = 0;
		EnumLogicSetMode setMode = null;
		String sender = "";
		String receiver = "";
		int numInstrSet = 0;

		System.out.println("");
		System.out.println("");
		
		// for fieldSub
		for (LogicDynamicFieldSub FieldSubField : ilpw.dynamicFieldToSolve.al_FieldSub) {
			idFieldSub = FieldSubField.entityDynamicFieldSub.getIdSubField();
			numChains = FieldSubField.al_al_chainSetSubField.size();
			// for chain subfield
		    for (ArrayList<LogicDynamicFieldSubSetting> fieldSubChain : FieldSubField.al_al_chainSetSubField) {
		    	numChainSets = fieldSubChain.size();
		    	System.out.println("START CHAIN");
				// chain set
		    	for (LogicDynamicFieldSubSetting fieldSubChainSet : fieldSubChain) {
		    		numChain = fieldSubChainSet.entityDynamicFieldSetting.getNumChain();
					setMode = fieldSubChainSet.entityDynamicFieldSetting.getSetMode();
					sender = fieldSubChainSet.entityDynamicFieldSetting.getFieldSenderId();
					receiver = fieldSubChainSet.entityDynamicFieldSetting.getFieldReceiverId();
					numInstrSet = fieldSubChainSet.entityDynamicFieldSetting.getNumInstrSet();
					System.out.println("Chain:"+numChain+" "+numInstrSet+" "+setMode+" "+sender+" "+receiver+" ");
				}
		    	System.out.println("END CHAIN");
			}
		
		}		
	}

	/*
     * Nuove istanze 
     */
	private void cloneAllEntityDynamicFieldSubSetting(LogicWorkProcess ilpw ) {

		// for fieldSub
		for (LogicDynamicFieldSub FieldSubField : ilpw.dynamicFieldToSolve.al_FieldSub) {
			// for chain subfield
		    for (ArrayList<LogicDynamicFieldSubSetting> fieldSubChain : FieldSubField.al_al_chainSetSubField) {
				// chain set
		    	for (LogicDynamicFieldSubSetting fieldSubChainSet : fieldSubChain) {
		    		fieldSubChainSet.entityDynamicFieldSetting = fieldSubChainSet.entityDynamicFieldSetting.clone();
				}
			}
		
		}		
	}
	
    /*
     * Impostazione numero catena trasfromazione in struttura dinamica e info db
     */
	private void setNumChain(int numChain, ArrayList<LogicDynamicFieldSubSetting> al_chainSetSubFieldCur) {
		for (LogicDynamicFieldSubSetting logicDynamicFieldSubSetting : al_chainSetSubFieldCur) {
			logicDynamicFieldSubSetting.numChainSet = numChain;
			logicDynamicFieldSubSetting.entityDynamicFieldSetting.setNumChain(numChain);
		}		
	}

	/*
	 * ---------------------------------------------------------------
	 * Calcolo successiva posizione e lunghezza in sottocampo origine
	 * ---------------------------------------------------------------
	 * 
	 * Il nuovo receiver è il sender dell'ultima Move incontrata.
	 * L'assegnazione può modificare la pos/lng nel sottocampo origine 
	 * 
	 * La nuova posizione e lunghezza viene calcolata in base alla
	 * posizione/lunghezza receiver corrente, alla posizione/lunghezza
	 * di reference modificatione del sender e del receiver.
	 * Questo metodo viene chiamato dopo la gestione della Move se
	 * questa modifica il receiver nell'assegnazione.
	 * Questo metodo viene inoltre chiamato prima della modifica di
	 * posizione e lunghezza del receiver corrente
	 */
	private void computeNextSubFieldPosLng(LogicWorkProcess ilpw) {

		int lngInSubFieldMax = 0;

		// Receiver senza ref/mod: pos/lng in subfield non cambiano
		if (!ilpw.identifierReceiver.getQualifier().isThereRefModification()) {
			return;
		}

		// Calcolo nuova posizione/lunghezza in sottocampo origine influenzate dalla trasformazione
		// Pos ref mod è dopo la posizione corrente in subfield: cur pos in subfield si incrementa
		if (ilpw.posRcvRefMod > ilpw.posRcv) {
			ilpw.posInSubField = ilpw.posInSubField + (ilpw.posRcvRefMod - ilpw.posRcv);
			lngInSubFieldMax = ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getDataItem().getSizeBytes() - ilpw.posInSubField + 1;
		}

		// Pos ref mod è prima della posizione corrente in subfield: cur pos in subfield si decrementa
		if (ilpw.posRcvRefMod < ilpw.posRcv) {
			ilpw.posInSubField = ilpw.posInSubField - (ilpw.posRcv - ilpw.posRcvRefMod);
			ilpw.lngInSubField = ilpw.lngRcvRefMod; 
			lngInSubFieldMax = ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getDataItem().getSizeBytes() - ilpw.posInSubField + 1;
		}

		// Pos ref mod è alla stessa posizione corrente in subfield: cur pos in subfield rimane uguale
		if (ilpw.posRcvRefMod == ilpw.posRcv) {
			lngInSubFieldMax = ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getDataItem().getSizeBytes() - ilpw.posInSubField + 1;
		}

		// Nuova lunghezza da considerare per il valore parziale del sottocampo
		if (ilpw.lngRcvRefMod <= lngInSubFieldMax) {
			ilpw.lngInSubField = ilpw.lngRcvRefMod; 
		} else {
			ilpw.lngInSubField = lngInSubFieldMax; 
		}

		// Se lunghezza receiver corrente <, si assume quella
		if (ilpw.lngRcv < ilpw.lngInSubField) {
			ilpw.lngInSubField = ilpw.lngRcv;
		}
		return;

	}


	/*
	 * ----------------------------------------------------
	 * Calcolo successiva posizione e lunghezza receiver.
	 * ----------------------------------------------------
	 * 
	 * Il nuovo receiver è il sender dell'ultima Move incontrata.
	 * 
	 * La nuova posizione e lunghezza viene calcolata in base alla
	 * posizione/lunghezza receiver corrente, alla posizione/lunghezza
	 * di reference modificatione del sender e del receiver.
	 * Questo metodo viene chiamato dopo la gestione della Move se
	 * questa modifica il receiver nell'assegnazione.
	 * 
	 */
	private void computeNextReceiverPosLng(LogicWorkProcess ilpw) {

		int newPosReceiver = 0;
		int newLngReceiver = 0;

		// Di default la posizione nel nuovo receiver rimane la stessa e anche la lunghezza
		newPosReceiver = ilpw.posRcv;
		newLngReceiver = ilpw.lngRcv;

		// (1) Sender No reference modification e receiver No.
		//     Pos/Lng nuovo receiver non cambiano.
		if (ilpw.posSndRefMod == 0
		&&  ilpw.posRcvRefMod == 0) {
			// Sul nuovo receiver si considera la lunghezza < fra sender e receiver
			if (ilpw.lngSnd < newLngReceiver ) {
				ilpw.lngRcv = ilpw.lngSnd;		    										
			}
			return;
		}

		// (2) Sender Si reference modification e receiver No.
		//     New pos receiver si incrementa
		if (ilpw.posSndRefMod > 0
		&&  ilpw.posRcvRefMod == 0) {
			newPosReceiver = ilpw.posRcv + ilpw.posSndRefMod - 1;
			// Sul nuovo receiver si considera la lunghezza < fra sender e receiver
			if (ilpw.lngSnd < newLngReceiver ) {
				newLngReceiver = ilpw.lngSnd;		    										
			}
			// Update nuovi valori calcolati
			ilpw.posRcv = newPosReceiver;
			ilpw.lngRcv = newLngReceiver;
			return;
		}


		// (3) Sender No reference modification e receiver Si.
		//     New pos receiver riparte da 1.
		if (ilpw.posSndRefMod == 0
		&&  ilpw.posRcvRefMod >  0) {
			newPosReceiver = 1;
			if (ilpw.lngRcvRefMod < newLngReceiver) {
				newLngReceiver = ilpw.lngRcvRefMod;
			}
			// Update nuovi valori calcolati
			ilpw.posRcv = newPosReceiver;
			ilpw.lngRcv = newLngReceiver;
			return;
		}

		// (4) Sender Si reference modification e receiver Si
		//     Calcolo effetto combinato sender+receiver ref/mod
		if (ilpw.posSndRefMod > 0  
		&&  ilpw.posRcvRefMod > 0) {

			// Effetti receiver reference modification

			// Ref mod coincide con la posizione receiver
			if (ilpw.posRcvRefMod == ilpw.posRcv) {
				newPosReceiver = ilpw.posRcv;
			}
			// Ref mod > la posizione corrente receiver 
			if (ilpw.posRcvRefMod > ilpw.posRcv) {
				newPosReceiver = ilpw.posRcvRefMod;
			}
			// Ref mod < della posizione corrente receiver
			if (ilpw.posRcvRefMod < ilpw.posRcv) {
				if (isAnalysis) {
					logMessage(EnumMessageType.WARNING, "MW011", ilpw.programCur.programName, ilpw.dataItemFieldToSolve.getNameIdentifierFormatted(), ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getNameIdentifier(), ilpw.posRcvRefMod+"", ilpw.posRcv+"");
					logMessagesSetting(ilpw);
				} else {
					logMessageToString(EnumMessageType.WARNING, "MW011", ilpw.programCur.programName, ilpw.dataItemFieldToSolve.getNameIdentifierFormatted(), ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getNameIdentifier(), ilpw.posRcvRefMod+"", ilpw.posRcv+"");				
				}
			}
			if (ilpw.lngRcvRefMod < newLngReceiver) {
				newLngReceiver = ilpw.lngRcvRefMod;
			}
			if (ilpw.lngSndRefMod < newLngReceiver) {
				newLngReceiver = ilpw.lngSndRefMod;
			}

			// Update nuovi valori calcolati
			ilpw.posRcv = newPosReceiver;
			ilpw.lngRcv = newLngReceiver;

			return;
		}

	}


	/*
	 * --------------------------------------------------------------------------------
	 * - Individuazione successivi receiver impliciti  
	 *   - A partire dal campo receiver candidato o non movimentato.
	 *   - A partire dai suoi sottocampi 
	 *   - A partire dai gruppi che lo contengono
	 *   - A partire da redefines che lo ridefiniscono
	 * --------------------------------------------------------------------------------
	 *
	 * Il processo iniziale di individuazione valori inizia da un campo dichiarato nell'istruzione
	 * dinamica e si esplica nel processo ricorsivo per tutti i sottocampi elementari.
	 * Durante il processo ricorsivo di assegnazione valori, si possono incontrare assegnazioni
	 * da gruppi di campi dei quali bisogna seguire ancora, ricorsivamente, le assegnazioni.
	 * L'assegnazione ultima che si individua, riferita sempre al sottocampo elementare origine
	 * del campo dinamico di partenza, si esplica in una posizione e una lunghezza di un valore
	 * da assegnare al sottocampo origine.
	 * Il campo sender può essere espresso o meno con reference modification.
	 * In ogni caso posSndRefMod è normalizzato e contiene o la posizione di ref mod indicata 
	 * oppure 1 se non indicata nell'istruzione.
	 * lngSndRefMod contiene la lunghezza di ref modificastion o la lunghezzsa del campo, se
	 * non indicata nell'istruzione.
	 * 
	 * Vengono individuati tutti i campi che sono candidati a essere nuovi receiver, la cui modifica
	 * implica una modifica indiretta del campo sender in esame o di una sua porzione. 
	 * Viene calcolata la posizione nel campo e la sua lunghezza.
	 * 
	 * 
	 * Si tratta di;
	 * 
	 * 1) Campi che ridefiniscono direttamente il campo 
	 * 2) Campi che ridefiniscono un campo di gruppo contenente il campo , alla pos di ref mod,
	 * 3) Campi di gruppo sotto il quale è definito il campo 
	 * 4) Campi di gruppo o elementari definiti sotto il campo origine, se di gruppo
	 * 
	 * 
	 * Elenco situazioni gestite
	 * -------------------------
	 * 
	 * 
	 * Caso -1- New Receiver elementare coincide con il Sender elementare o di gruppo
	 * 
	 *    05 New-Receiver   						Pic XX.
	 * 
	 * 
	 * Caso -2- New Receiver sono i sottocampi elementari e/o di gruppo del Sender di gruppo
	 * 
	 *    05 Receiver-group.
	 *       ...
	 *       07 New-Receiver               Pic X.
	 *       ...
	 *  
	 *    
	 * Caso -3- New Receiver elementare ridefinisce esplicitamente il Sender elementare
	 * 
	 *    05 Receiver   						Pic XX.
	 *    05 New-Receiver Redefines Receiver 	Pic XX.
	 *   
	 *    
	 * Caso -4- New Receiver elementare ridefinisce esplicitamente il Sender di gruppo
	 * 
	 *    05 Receiver-group.
	 *       ...
	 *       07 New-Receiver               Pic X.
	 *       ...
	 *    05 New-Receiver Redefines Receiver-group Pic X(4).  
	 *    
	 *    
	 * Caso -5- New Receiver di gruppo ridefinisce esplicitamente il Sender di gruppo
	 * 
	 *    05 Receiver.
	 *       07 Filler              Pic X.
	 *       07 A1                  Pic XX.
	 *       07 Filler              Pic X.
	 *    05 New-Receiver-Group Redefines Sender.  
	 *       07 New-Receiver1       Pic X.            
	 *       ...	
	 *       07 New-Receivern       Pic X. 
	 *  
	 *       
	 * Caso -6- New Receiver di gruppo ridefinisce esplicitamente il Sender elementare
	 * 
	 *    05 Receiver                 PIC X(4).
	 *    05 New-Receiver-Group Redefines Receiver.  
	 *       07 New-Receiver1       Pic X.            
	 *       ...	
	 *       07 New-Receivern       Pic X. 
	 *    
	 *       
	 * Caso -7- New Receiver elementare ridefinisce esplicitamente il Sender dentro un gruppo
	 * 
	 *    05 A1.
	 *       07 Filler              Pic X(5).
	 *       07 Receiver            Pic XX.
	 *       07 Filler              Pic X.
	 *    05 New-Receiver Redefines A1 Pic X(8).
	 *    
	 *    
	 * Caso -8- New Receiver di gruppo ridefinisce implicitamente il Sender dentro un gruppo 
	 * 
	 *    05 A1.
	 *       07 Filler              Pic X(5).
	 *       07 Receiver            Pic XX.
	 *       07 Filler              Pic X.
	 *    05 New-Receiver-Group Redefines A1.
	 *       07 Filler              Pic X.
	 *       07 New-Receiver        Pic X(5).
	 *       07 Filler              Pic X.
	 * 
	 * 
	 */
	private void getAllNextReceiverImplicit(LogicWorkProcess ilpw) {

		ProgramCobolEntry<? extends Instruction> implicitReceiverEntry = null;	// Entry di Data division
		ProgramCobolEntry<? extends Instruction> explicitReceiverEntry = null;	// Entry di Data division
		DataItemCobolIdentifier implicitReceiverIdentifier = null;				// Nuovo identificatore receiver
		InstructionCobolDataItem implicitReceverDataItem = null;				// Istruzione di definizione dati
		InstructionCobolDataItem workDataItem = null;				            // Istruzione di definizione dati di lavoro

		// Pointer definizioni potenziali receiver
		ArrayList<Integer> al_implicitReceiverPointer = null;

		// Gruppi e campi impliciti
		int[] ar_receiverGroupOwner = null;                                     // Gruppi sotto il quale è definito il campo Receiver
		int[] ar_receiverGroupOwnerRedefines = null;                            // Campi che ridefiniscono i gruppi sotto il quale è definito il campo Receiver
		int[] ar_receiverGroupDataItem = null;                                  // Campi di gruppo o elementari definiti sotto il campo Receiver, se gruppo
		int[] ar_receiverRedefines = null;                                      // Campi che ridefinizcono il campo Receiver
		int[] ar_redefiningGroupDataItem = null;                                // Campi di gruppo o elementari definiti sotto il campo ridefinente il gruppo owner

		// Posizioni per receiver impliciti
		int posInNextReceiver = 0;
		int lngAssignMax = 0;
		int lngNextImplicitReceiver = 0;
		int posNextInSubField = 0;
		int lngNextInSubField = 0;

		// Gestione gruppi multipli con lo stesso nome a fronte di clausol OF in Move
		String underGroupName = "";  											// Nome gruppo di cui cercare i receiver impliciti
		int ar_numDefGroup[] = null;                                            // Definizioni gruppo

		// Receiver origine e impliciti dedotti dal Sender (le 5 ArrayList viaggiano insieme)
		ilpw.al_nextReceiverImplicit = new ArrayList<DataItemCobolIdentifier> ();
		ilpw.al_nextReceiverImplicitPos = new ArrayList<Integer> ();
		ilpw.al_nextReceiverImplicitLng = new ArrayList<Integer> ();
		ilpw.al_nextSubFieldImplicitPos = new ArrayList<Integer> ();
		ilpw.al_nextSubFieldImplicitLng = new ArrayList<Integer> ();



		//////////////////////////////////////////////////////////////////////////////////////////
		// (1) Condizioni di scarto Next Receiver
		//////////////////////////////////////////////////////////////////////////////////////////

		// Receiver candidato principale campo di precompilatore non definito, nessuna ricerca di receiver impliciti: exit
		if (ilpw.identifierReceiver.getIdentifierType() == EnumCobolReservedWords.PRECOMPILER_CICS
		||  ilpw.identifierReceiver.getIdentifierType() == EnumCobolReservedWords.PRECOMPILER_SQL) {
			return;
		}  

		// Receiver candidato principale literal o costante figurativa, nessuna ricerca di receiver impliciti: exit
		if (ilpw.identifierReceiver.getQualifier().getSymbolType() == EnumSymbolType.COBOL_SYMBOL_LITERAL_ALPHA
		|| 	ilpw.identifierReceiver.getQualifier().getSymbolType() == EnumSymbolType.COBOL_SYMBOL_FIGURATIVE) {
			return;
		}

		// Receiver elemento di tabella, sotto occurs oppure campo occursato: si considera solo in input e si scarta
		if (ilpw.programCur.isDataItemTableElement(ilpw.identifierReceiver.getNumInstr())) {
			return;
		}   		


		//////////////////////////////////////////////////////////////////////////////////////////
		// (2) Next Receiver impliciti, estrazione di tutti i possibili  candidati
		//////////////////////////////////////////////////////////////////////////////////////////

		// Entry di programma che contiene il nuovo campo Receiver candidato esplicito
		// Si tratta di un campo Sender di una Move oppure di un sottocampo dinamico origine
		// o di un suo derivato
		explicitReceiverEntry = ilpw.programCur.entryDataDivision(ilpw.identifierReceiver.getDataItem().getNumInstr());

		// Conterrà tutti i next candidati receiver impliciti
		al_implicitReceiverPointer = new ArrayList<Integer> ();

		// Campi elementari e/o di gruppo definiti sotto il campo receiver di gruppo (array null se receiver elementare).
		ar_receiverGroupDataItem = ilpw.programCur.dataItemsUnderGroupPointers(ilpw.identifierReceiver.getDataItem().getNumInstr(), DATA_ITEMS_ALL);
		for (int pointer : ar_receiverGroupDataItem) {
			al_implicitReceiverPointer.add(pointer);
		}

		// Campi di gruppo owner contenenti il campo Receiver candidato esplicito.
		ar_receiverGroupOwner = ilpw.programCur.groupOwnerPointers(ilpw.identifierReceiver.getDataItem().getNumInstr());
		for (int pointer : ar_receiverGroupOwner) {
			// Campi di gruppo FILLER: non possono essere referenziati
			workDataItem = ilpw.programCur.instructionDataItem(pointer);
			if (workDataItem.getDataName().equals("FILLER")) {
				continue;
			}
			al_implicitReceiverPointer.add(pointer);
			// Campi che ridefiniscono i campi di gruppo owner contenenti il campo Receiver candidato esplicito.
			ar_receiverGroupOwnerRedefines = ilpw.programCur.dataItemsRedefinePointers(pointer);
			for (int pointerGroupRed : ar_receiverGroupOwnerRedefines) {
				al_implicitReceiverPointer.add(pointerGroupRed);
				// Campi sotto i campi che ridefiniscono i campi di gruppo owner contenenti il campo sender.
				ar_receiverGroupDataItem = ilpw.programCur.dataItemsUnderGroupPointers(pointerGroupRed, DATA_ITEMS_ALL);
				for (int pointerUnderGroupRed : ar_receiverGroupDataItem) {
					al_implicitReceiverPointer.add(pointerUnderGroupRed);
				}
			}
		}

		// Campi elementari e/o di gruppo che ridefiniscono il campo Receiver candidato espicito.
		ar_receiverRedefines = ilpw.programCur.dataItemsRedefinePointers(ilpw.identifierReceiver.getDataItem().getNumInstr());
		for (int pointer : ar_receiverRedefines) {
			al_implicitReceiverPointer.add(pointer);
			// Campi definiti sotto il campo ridefinente
			ar_redefiningGroupDataItem = ilpw.programCur.dataItemsUnderGroupPointers(pointer, DATA_ITEMS_ALL);
			for (int pointerRed : ar_redefiningGroupDataItem) {
				al_implicitReceiverPointer.add(pointerRed);
			}
		}


		///////////////////////////////////////////////////////////////////////////////////////////////////////
		// (3) Analisi possibili next receiver impliciti candidati
		//     Si ragiona sulle posizioni fisiche dei campi nell'ambito delle stessa struttura dati a liv 01
		//     e sulle loro dimensioni in bytes. Non si tiene + conto del nomi dei campi.
		///////////////////////////////////////////////////////////////////////////////////////////////////////

		for (int pointerReceiver : al_implicitReceiverPointer) {

			// Next receiver implicito candidato
			implicitReceiverEntry = ilpw.programCur.entryDataDivision(pointerReceiver);
			implicitReceiverIdentifier = packageNewIdentifier(ilpw, pointerReceiver);		
			implicitReceverDataItem = implicitReceiverIdentifier.getDataItem();


			// Nel processo ricorsivo è stato già trattato il sottocampo origine: skip
			if (implicitReceiverIdentifier.getNameIdentifier().equals(ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getNameIdentifier())) {
				continue;
			}

			// Nel processo ricorsivo di questa catena il campo è stato già trattato: skip
			if (ilpw.map_identifierReceiverManaged.get(implicitReceiverIdentifier.getNameIdentifier()) != null) {
				continue;
			}

			// Controllo compatibilità a livello di intero campo receiver candidato
			// Si scarta se il next receiver inizia e finisce prima del receiver oppure dopo

			// Next receiver implicito incompatibile, posizionato fisicamente PRIMA del campo receiver esplicito: skip
			if (implicitReceiverEntry.getPos() + implicitReceverDataItem.getSizeBytes() < explicitReceiverEntry.getPos()) {
				continue;
			}

			// Next receiver implicito incompatibile, posizionato PRIMA del campo receiver esplicito: skip
			if (implicitReceiverEntry.getPos() + implicitReceverDataItem.getSizeBytes() < explicitReceiverEntry.getPos() + ilpw.posRcv) {
				continue;
			}

			// Next receiver implicito incompatibile, posizionato DOPO la fine della parte utile del receiver esplicito: skip
			if (implicitReceiverEntry.getPos() >= explicitReceiverEntry.getPos() + ilpw.posRcv + ilpw.lngRcv - 1) {
				continue;
			}

			// Nuova posizione e lunghezza mappata in sottocampo origine, relativa al nuovo receiver
			// In assenza di assegnazioni anche indirette del sottocampo origine a gruppi contenenti a 
			// loro volta dei sottocampi, pos/lng rimangono fisse a 1/lng subfield.
			// Altrimenti la posizione mappata sul sottocampo origine, si incrementa di conseguenza.
			posNextInSubField = ilpw.posInSubField;				    // Si suppone non cambi
			lngNextInSubField = ilpw.lngInSubField;					// Si suppone non cambi

			// Next receiver implicito ha una porzione all'interno dell'area utile del receiver esplicito.
			// Le posizioni possono coincidere, come nel caso di redefines dirette di receiver esplicito
			// o primo sottocampo di un gruppo etc.
			// oppure NON coincidere come nel caso di receiver implicito con dei sottocampi, dal secondo sottocampo.
			// Un caso classico è l'assegnazione di un sottocampo elementare con un gruppo con dei subfield definiti.
			// In questo caso la posizione del secondo campo non coincide con quella nel receiver esplicito.


			// Posizione Next receiver implicito coincide con il receiver esplicito esattamente da pos 1 o > 1
			// La posizione nel receiver implicito si calcola a partire da 1
			if (explicitReceiverEntry.getPos() + ilpw.posRcv - 1 >= implicitReceiverEntry.getPos()) {
				posInNextReceiver = explicitReceiverEntry.getPos() + ilpw.posRcv - implicitReceiverEntry.getPos();
				lngAssignMax = implicitReceverDataItem.getSizeBytes() - posInNextReceiver + 1;	// Lunghezza massima utile teorica nel campo receiver candidato

				// Posizione Next receiver implicito > di receiver esplicito.
				// E' la situazione di assegnazione di campi sottoinsieme del sottocomapo origine o di receiver trasformati ricorsivamente
			} else  {
				posInNextReceiver =  1;
				posNextInSubField = posNextInSubField + implicitReceiverEntry.getPos() - explicitReceiverEntry.getPos() - ilpw.posRcv + 1;
				lngAssignMax = implicitReceverDataItem.getSizeBytes();							// Lunghezza massima utile teorica nel campo receiver candidato
			}

			// Nuova posizione mappata nel sottocampo origine incompatibile: skip
			if (posNextInSubField > ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getDataItem().getSizeBytes()) {
				continue;
			}

			// Si considera la lunghezza < fra new receiver candidato e quella corrente
			if (ilpw.lngRcv < lngAssignMax ) {
				lngNextImplicitReceiver = ilpw.lngRcv;		    	// Sul nuovo receiver si considera la lunghezza del receiver corrente, minore
			} else {
				lngNextImplicitReceiver = lngAssignMax;		    	// Sul nuovo receiver si considera la lunghezza utile del sender valorizzato da Move, minore
			}
			lngNextInSubField = lngNextImplicitReceiver;			// Nuova lunghezza mappata in soubfield origine dal valore individuato per il receiver

			// Se il receiver implicito è sotto lo stesso gruppo di cui si erano richiesti i reciver impliciti
			// e se esistono più gruppi con lo stesso nome, l'assegnazione era stata probabilmente qualificata 
			// dalla clausola OF e quindi si deve qualificare il receiver implicito esattamente come
			// appartenente a quel gruppo, per garantire la corretta ricerca ricorsiva.
			if (ilpw.identifierReceiver.getDataItem().isGroupField()) {
				// Gruppo sotto l quale il campo è immediatamente definito
				underGroupName = ilpw.programCur.groupOwnerName(implicitReceiverIdentifier.getDataItem().getNumInstr());
				if (underGroupName.equals(ilpw.identifierReceiver.getNameIdentifier())) {
					// Più di un gruppo contiene il receiver implicito con lo stesso nome: receiver implicito da qualificare
					ar_numDefGroup = ilpw.programCur.dataItemPointers(implicitReceiverIdentifier.getNameIdentifier());
					if (ar_numDefGroup.length > 1) {
						implicitReceiverIdentifier.getQualifier().setUnderGroupDeclared(true);
						implicitReceiverIdentifier.getQualifier().setGroupNameOwner(underGroupName);
					} // end-if
				} // end-if
			} // end-if


			// Accodamento info nuovo receiver implicito e sua mappatura in sottocampo origine
			ilpw.al_nextReceiverImplicit.add(implicitReceiverIdentifier);	// Nuovo Possibile campo Receiver
			ilpw.al_nextReceiverImplicitPos.add(posInNextReceiver);			//  Da posizione
			ilpw.al_nextReceiverImplicitLng.add(lngNextImplicitReceiver);	//    per lunghezza
			ilpw.al_nextSubFieldImplicitPos.add(posNextInSubField);			// Che mappa il sottocampo origine da posizione 
			ilpw.al_nextSubFieldImplicitLng.add(lngNextInSubField);			//    per Lunghezza 

		}

		return;
	}


	/*
	 * -----------------------------------------------------------------------------
	 * Gestione ultima assegnazione da campo parametro in Procedure Division Using.
	 * -----------------------------------------------------------------------------
	 * 
	 * Il metodo viene richiamato a fronte di due casi:
	 * 
	 * 1) Assegnazione via Move,  con sender, receiver e numero istruzione procedure valorizzati.
	 *    Il parametro Using  è il sender, indicato da identifierToDeal
	 * 2) Assegnazione con semplice dichiarazione, per esempio direttamente nell'istruzione, 
	 *    con receiver valorizzato  e con con sender e numero istruzione NON valorizzati.  
	 *    Il parametro Using  è il receiver, indicato da identifierToDeal. 
	 *    nel numero di istruzione viene messo il numero di definizione dati del campo di using.
	 */
	private boolean lastSetByUsingParm(LogicWorkProcess ilpw, DataItemCobolIdentifier identifierToDeal) {

		ProgramCobolEntry<? extends Instruction> cobolEntryProcDiv = null;
		ProgramCobolEntry<? extends Instruction> cobolEntryDataDiv = null;
		InstructionCobolProcedure instrCobolProcDiv = null;					    // Istruzione Procedure Division
		InstructionCobolDataItem instrCobolDataItem = null;					    // Istruzione dati
		ArrayList<DataItemCobolIdentifier> al_identifierParmUsing = null;		// Identificatori campi parametri Using
		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;		// Singola trasformazione
		String identifierToDealName = "";										// Nome campo receiver
		String usingParmName = "";												// Nome campo in using
		int dspFieldInUsingParm = 0;                                            // Displacement campo in parametro using
		int numDefIdentifierToDeal = 0;											// Pointer a definizione campo receiver

		if (ilpw.lastSetManaged) {
			return true;
		}

		// Si tratta di un data item non definito nel programma ma inserito da un precompilatore
		if (identifierToDeal.getIdentifierType()  == EnumCobolReservedWords.PRECOMPILER_CICS
		||  identifierToDeal.getIdentifierType()  == EnumCobolReservedWords.PRECOMPILER_SQL) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Non si tratta di una data item ma di literal o costante figurativa
		if (identifierToDeal.getIdentifierType() != EnumCobolReservedWords.DATA_DIV_DATA_ITEM) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Data item definito nel programma Cobol

		// Recupero entry contenente la definizione del receiver
		numDefIdentifierToDeal = identifierToDeal.getDataItem().getNumInstr();
		cobolEntryDataDiv = ilpw.programCur.entryDataDivision(numDefIdentifierToDeal);

		// Receiver non di linkage: non può essere un parametro di Using
		if (cobolEntryDataDiv.getProgramSection() != EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Imposto il nome del campo da cercare, dal receiver
		identifierToDealName = identifierToDeal.getNameIdentifier();

		// Recupero entry 0 e istruzione di procedure division Using 
		cobolEntryProcDiv = ilpw.programCur.entryProcedure(0);
		instrCobolProcDiv = (InstructionCobolProcedure) cobolEntryProcDiv.getInstruction();


		///////////////////////////////////////////////////////////////////////
		// Gestione data item receiver in Procedure Division Using  
		///////////////////////////////////////////////////////////////////////

		if (instrCobolProcDiv.procDivIsUsingParms()) {

			al_identifierParmUsing = instrCobolProcDiv.procDivGetUsingParms();

			// Scan parametri Using
			for (int i = 0; i < al_identifierParmUsing.size(); i++) {

				DataItemCobolIdentifier dataItemParmUsing = al_identifierParmUsing.get(i);
				usingParmName = dataItemParmUsing.getNameIdentifier();

				// Parametro using di gruppo e receiver non definito sotto il parametro using
				if (dataItemParmUsing.getDataItem().isGroupField()
				&& !ilpw.programCur.isDataItemUnderGroupNameAnyLevel(numDefIdentifierToDeal, usingParmName)
				&& !usingParmName.equals(identifierToDealName)) {
					continue;
				}

				// Parametro using elementare e receiver non coincidenti
				if (!dataItemParmUsing.getDataItem().isGroupField()
				&&  !usingParmName.equals(identifierToDealName)) {
					continue;
				}

				// Campo sotto il parametro Using definito come gruppo
				// oppure campo non di gruppo con lo stesso nome.
				// determino il displacement del campo, nel parametro using
				for (int j = dataItemParmUsing.getNumInstr(); j < ilpw.programCur.entriesData().length; j++) {
					cobolEntryDataDiv = ilpw.programCur.entryDataDivision(j);
					if (!(cobolEntryDataDiv.getInstruction() instanceof InstructionCobolDataItem)) {continue;}
					instrCobolDataItem = (InstructionCobolDataItem) cobolEntryDataDiv.getInstruction();

					// E' il receiver: fine ricerca
					if (instrCobolDataItem.getDataName().equals(identifierToDealName)) {
						break;
					}

					// Update displacement se non è un gruppo
					if (!instrCobolDataItem.isGroupField()) {
						dspFieldInUsingParm = dspFieldInUsingParm + instrCobolDataItem.getSizeBytes();
					}
				}


				// Valorizzazione preliminare info trasformazione in struttura db, usata anche come servizio
				// Attributi, tra cui identifierSender, saranno aggiornati in seguito
				entityDynamicSubFieldSetting = storeInfoDbSingleSetting(ilpw);

				// Info specifiche ultima trasformazione
				ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM;
				entityDynamicSubFieldSetting.setSetMode(EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM);
				entityDynamicSubFieldSetting.setNumUsingParm(i+1);
				entityDynamicSubFieldSetting.setDspFieldInUsingParm(dspFieldInUsingParm);
				entityDynamicSubFieldSetting.setFieldReceiverNum(0);
				entityDynamicSubFieldSetting.setFieldReceiverId("");
				entityDynamicSubFieldSetting.setFieldReceiverPos(0);										 
				entityDynamicSubFieldSetting.setFieldReceiverLng(0);										 

				// Nuovo oggetto con informazioni ultima trasformazione
				ilpw.logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting();
				ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicSubFieldSetting;
               
				// Catena, posizione e lunghezza in sottocampo origine valorizzata dalla trasformazione
				ilpw.logicDynamicFieldSubSettingWrk.numChainSet = ilpw.numChainSet;
				ilpw.logicDynamicFieldSubSettingWrk.posInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getPosInSubField();
				ilpw.logicDynamicFieldSubSettingWrk.lngInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getLngInSubField();

				// Mark istruzione dinamica origine e descrittore campo dinamico in struttura db come dynamic spreaded
				// Solo se si stanno eseguendo logiche stesso programma
				if (!ilpw.isExecLogicSpreaded) {
					ilpw.instrDynamicOrigin.setDynamicSpreaded(true);
					ilpw.dynamicFieldSubToSolve.isSpreaded = true;
				}

				ilpw.lastSetManaged = true;
				return true;
			}
		} 

		ilpw.lastSetManaged = false;
		return false;
	}


	/*
	 * -----------------------------------------------------------------------------
	 * Gestione ultima assegnazione da campo in Linkage Section.
	 * -----------------------------------------------------------------------------
	 * 
	 * Il metodo viene richiamato a fronte di assegnazione da un area
	 * di Linkage applicativa non Cics/Dl1/Sql etc., a fronte di:
	 * 
	 * 1) Assegnazione via Move.
	 *    Il sender è in un'area di Linkage applicativa  
	 * 2) Assegnazione con semplice dichiarazione, per esempio direttamente nell'istruzione, 
	 *    di un campo di un'area di linkage.
	 * 
	 * L'area di Linkage individuata deve fare riferimento, alla fine
	 * delle trasformazioni, nel programma corrente o nei programmi chiamanti,
	 * a un'area fisica di working storige o a una literal assegnata o a dati esterni.
	 * Quindi si deve individuare, nella catena di trasformazione corrente,
	 * l'indirizzamento dell'area di linkage tramite una istruzione Cobol
	 * SET ADDRESS OF Area-Linkage TO pointer.
	 * 
	 * Il pointer può essere in un parametro di una Call Using della catena
	 * di trasformazione, in un parametro di Procedure Division Using
	 * o in un'altra area di linkage, ricorsivamente.
	 * 
	 * La soluzione di un'ultima assegnazione tipo lastSetByLinkage può
	 * quindi in definitiva avvenire:
	 * 
	 * 1) Nel programma corrente se pointer viene valorizzato con
	 *    SET POINTER TO ADDREESS OF Area-Working nel pgm corrente
	 * 2) In un programma chiamante
	 * 3) In un programma chiamato
	 * 
	 * N.B. Al momento non vemgono considerati i programmi chiamati.
	 * 
	 */
	@SuppressWarnings("unchecked")
	private boolean lastSetByLinkage(LogicWorkProcess ilpw, DataItemCobolIdentifier identifierToDeal) {

		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;		// Singola trasformazione
		ProgramCobolEntry<Instruction> identifierEntryDataDiv = null;		    // Entry definizione dati

		if (ilpw.lastSetManaged) {
			return true;
		}

		// Si tratta di un data item non definito direttamente nel programma e inserito da un precompilatore
		if (identifierToDeal.getIdentifierType()  == EnumCobolReservedWords.PRECOMPILER_CICS
		||  identifierToDeal.getIdentifierType()  == EnumCobolReservedWords.PRECOMPILER_SQL) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Non si tratta di una data item 
		if (identifierToDeal.getIdentifierType() != EnumCobolReservedWords.DATA_DIV_DATA_ITEM) {
			ilpw.lastSetManaged = false;
			return false;
		}


		// Data item definito nel programma Cobol


		// Imposto entry di definizione del campo da cercare
		identifierEntryDataDiv =   (ProgramCobolEntry<Instruction>) ilpw.programCur.entryDataDivision(identifierToDeal.getNumInstr());

		// Non è un campo di linkage
		if (identifierEntryDataDiv.getProgramSection() != EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// E' un campo definito in Linkage Section, verifica se DFHCOMMAREA o definito sotto 01 DFHCOMMAREA
        if (identifierToDeal.getNameIdentifier().toUpperCase().equals("DFHCOMMAREA")
        ||	ilpw.programCur.isDataItemUnderGroupNameAnyLevel(identifierToDeal.getNumInstr(), "DFHCOMMAREA")) {
			ilpw.lastSetManaged = false;
			return false;
		}
				
		// Campo di Linkage NON di gruppo DFHCOMMAREA, ricerca modalità indirizzamento

		// Displacement campo nell'area di linkage (livello 01)
		ilpw.dspFieldInLinkageArea = identifierEntryDataDiv.getPos();

		// Si suppone nessun pointer di indirizzamento individuato
		ilpw.dataItemPointer = null;            								//
		ilpw.typePointerArea = EnumLogicSetPointerArea.NOT_ASSIGNED;            //
		ilpw.dspPointerInLinkageArea = 0;                                       //
		ilpw.numUsingParmPointer = 0;                                           //
		ilpw.dspPointerInUsingParm = 0;                                         //
		
		// Recupero informazioni sul pointer che indirizza l'area di linkage, passato dai pgm chiamanti
		// Potrebbero essere individuati + pointer,al momento si considera solo il primo
		lastSetByLinkageGetInfoPointer(ilpw);

		// Non sono stati trovati pointer che indirizzano l'area di linkage
		if (ilpw.al_dataItemPointer.size() == 0) {
			ilpw.lastSetManaged = false;
			return false;
		}
		
		// Presente almeno un pointer, si considera il primo.
		// TODO Estendere gestione pointer per + pointer
		ilpw.dataItemPointer = ilpw.al_dataItemPointer.get(0);
		ilpw.typePointerArea = ilpw.al_typePointerArea.get(0); 
		ilpw.dspPointerInLinkageArea = ilpw.al_dspPointerInLinkageArea.get(0); 
		ilpw.numUsingParmPointer = ilpw.al_numUsingParmPointer.get(0);  
		ilpw.dspPointerInUsingParm = ilpw.al_dspUsingParmPointer.get(0); 


		// Valorizzazione preliminare info trasformazione in struttura db, usata anche come servizio
		// Attributi, tra cui identifierSender, saranno aggiornati in seguito
		entityDynamicSubFieldSetting = storeInfoDbSingleSetting(ilpw);

		// Info specifiche ultima trasformazione
		ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE;
		entityDynamicSubFieldSetting.setSetMode(EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE);
		entityDynamicSubFieldSetting.setDspFieldInLinkageArea(ilpw.dspFieldInLinkageArea);  
		entityDynamicSubFieldSetting.setTypePointerArea(ilpw.typePointerArea);
		entityDynamicSubFieldSetting.setDspPointerInLinkageArea(ilpw.dspPointerInLinkageArea);
		entityDynamicSubFieldSetting.setNumUsingParmPointer(ilpw.numUsingParmPointer);
		entityDynamicSubFieldSetting.setDspPointerInUsingParm(ilpw.dspPointerInUsingParm);		
		entityDynamicSubFieldSetting.setFieldSenderNum(entityDynamicSubFieldSetting.getFieldReceiverNum());
		entityDynamicSubFieldSetting.setFieldSenderId(entityDynamicSubFieldSetting.getFieldReceiverId());
		entityDynamicSubFieldSetting.setFieldSenderPos(entityDynamicSubFieldSetting.getFieldReceiverPos());										 
		entityDynamicSubFieldSetting.setFieldSenderLng(entityDynamicSubFieldSetting.getFieldReceiverLng());										 
		entityDynamicSubFieldSetting.setFieldSenderNum(0);
		entityDynamicSubFieldSetting.setFieldSenderId("");
		entityDynamicSubFieldSetting.setFieldSenderPos(0);										 
		entityDynamicSubFieldSetting.setFieldSenderLng(0);										 

		// Nuovo oggetto con informazioni ultima trasformazione
		ilpw.logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting();
		ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicSubFieldSetting;

		// Posizione e lunghezza in sottocampo origine valorizzata dalla trasformazione
		ilpw.logicDynamicFieldSubSettingWrk.posInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getPosInSubField();
		ilpw.logicDynamicFieldSubSettingWrk.lngInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getLngInSubField();

		// mark istruzione dinamica origine e descrittore campo dinamico in struttura db come dynamic spreaded
		// Solo se si stanno eseguindo logiche stesso programma		
		if (!ilpw.isExecLogicSpreaded) {
			ilpw.dynamicFieldSubToSolve.isSpreaded = true;
			ilpw.instrDynamicOrigin.setDynamicSpreaded(true);
		}

		ilpw.lastSetManaged = true;
		return true;
	}

	
	/* -------------------------------------------------------------------
	 * Recupero informazioni sul pointer che indirizza l'area di linkage
	 * -------------------------------------------------------------------
	 * 
	 * Relativamente alla definizione del pointer per:
	 * 
	 * Tipo di area appartenenza
	 * Displacement nell'area di livello 01 in cui è definito
	 * Numero parametro using dove è definito
	 * Displacement nel parametro using 
	 * 
	 */
	private void lastSetByLinkageGetInfoPointer(LogicWorkProcess ilpw) {

		ProgramCobolEntry<? extends Instruction> entryInstr = null;						// Entry generico istruzione definizione dati
		ProgramCobolEntry<? extends Instruction> entryReceiverLvl01 = null;				// Entry campo receiver di cui trovare l'indirizzamento
		ProgramCobolEntry<? extends Instruction> entryPointerLvl01 = null;				// Entry campo area livello 01 contenente il pointer
		ProgramCobolEntry<? extends Instruction> entryPointer = null;				    // Entry pointer 
		InstructionCobolProcedure instr = null;											// Istruzione generica di procedure division
		InstructionCobolDataItem dataItemPointer = null;								// Definizione dati per il pointer
		InstructionCobolDataItem dataItemPointerLvl01 = null;				    		// Definizione dati livello 01 contenente il pointer
		InstructionCobolDataItem dataItemReceiverLvl01 = null;							// Campo di gruppo livello 01 sotto cui il receiver è definito
		ArrayList<Integer> al_pointer = null;										    // Num. def. pointer in istruzioni Set Pointer To Address Of campo

		int[] ar_numInstrGroup = null;                                          		// Definizioni dati gruppi contenenti il receiver
		int numInstrPointerLvl01 = 0;                                           		// Definizione liv. 01 sotto cui è definito il pointer
		int numInstrReceiverLvl01 = 0;                                          		// Definizione liv. 01 sotto cui è definito il receiver
		int posPointer = 0;                                               				//

		// Allocazioni array list con info pointer (viaggiano tutti insieme e hanno lo stesso numero di elementi)
		ilpw.al_typePointerArea = new ArrayList<EnumLogicSetPointerArea> ();			//  
		ilpw.al_dspPointerInLinkageArea = new ArrayList<Integer> ();					//  
		ilpw.al_numUsingParmPointer = new ArrayList<Integer> ();						//  
		ilpw.al_dspUsingParmPointer = new ArrayList<Integer> ();						//  
		ilpw.al_dataItemPointer = new ArrayList<InstructionCobolDataItem> ();			//  


		///////////////////////////////////////////////////////////////////////////////////
		// Individuazione istruzioni SET di area liv 01 contenente il campo receiver
		// Intabellamento di tutti i campi pointer nelle istruzioni, non duplicati    
		///////////////////////////////////////////////////////////////////////////////////

		// Recupero data item livello 01 di gruppo sotto il quale è definito il receiver
		ar_numInstrGroup = ilpw.programCur.groupOwnerPointers(ilpw.identifierReceiver.getNumInstr());
		numInstrReceiverLvl01 = ar_numInstrGroup[ar_numInstrGroup.length - 1];
		entryReceiverLvl01 = ilpw.programCur.entryDataDivision(numInstrReceiverLvl01);
		dataItemReceiverLvl01 = (InstructionCobolDataItem) entryReceiverLvl01.getInstruction();

		// Individuazione pointer di indirizzamento area contenente il receiver con SET ADDRESS OF AreaLinkage To Pointer
		al_pointer = new ArrayList<Integer> ();

		// Scan istruzioni SET ADDRESS OF area TO pointer
		for (int numInstrSet : ilpw.set_setCobolAddressOf) {

			entryInstr = ilpw.programCur.entryProcedure(numInstrSet);
			instr = (InstructionCobolProcedure) entryInstr.getInstruction();

			// La SET non indirizza l'area interessata: skip
			if (!instr.setGetAreaAddressed().getDataItem().getDataName().equals(dataItemReceiverLvl01.getDataName())) {
				continue;
			}

			// Append pointer di indirizzamento dell'area
			al_pointer.add(instr.setGetPointer().getDataItem().getNumInstr());
		}


		// Nessun pointer nel programma indirizza l'area di linkage, possibile errore nel programma sorgente: return 
		if (al_pointer.size() == 0) {
			return;
		}

		// Scan pointer individuati in istruzioni SET ADDRESS OF linkageArea TO pointer.
		// Potrebbero esserci + pointer, tutti validi, che indirizzano l'area.
		// Si trattano DFHCOMMAREA/TWA/CSA/Using Parm come aree in cui i chiamanti possono passare i pointer
		for (int numInstrPointer : al_pointer) {

			// Istruzione di definizione del pointer
			dataItemPointer = (InstructionCobolDataItem) ilpw.programCur.entryDataDivision(numInstrPointer).getInstruction();
			entryPointer = ilpw.programCur.entryDataDivision(dataItemPointer.getNumInstr());

			// Individuazione area in cui il pointer è definito (l'ultimo elemento è il livello 01)
			if (dataItemPointer.getLevelNumber() != 1) {
				// Il livello 01 è il campo di gruppo sotto il quale il campo è definito (TWA/CSA/DFHCOMMAREA/Working)
				ar_numInstrGroup = ilpw.programCur.groupOwnerPointers(dataItemPointer.getNumInstr());
				numInstrPointerLvl01 = ar_numInstrGroup[ar_numInstrGroup.length - 1];
				entryPointerLvl01 = ilpw.programCur.entryDataDivision(numInstrPointerLvl01);
				dataItemPointerLvl01 = (InstructionCobolDataItem) entryPointerLvl01.getInstruction();
				posPointer = entryPointer.getPos();
			} else {
				// Il livello 01 è lo stesso campo (TWA/CSA/DFHCOMMAREA/Working)
				numInstrPointerLvl01 = dataItemPointer.getNumInstr();
				entryPointerLvl01 = entryPointer;
				dataItemPointerLvl01 = dataItemPointer;
				posPointer = 0;
			}

			///////////////////////////////////////////////////////////////////////////////
			// Store info pointer in base al tipo di area
			///////////////////////////////////////////////////////////////////////////////

			// Il pointer è definito in area di linkage, in un parametro di procedure division using
			if (entryPointerLvl01.getProgramSection() == EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION 
			&&  isDataItemInUsingParm(ilpw, dataItemPointer)) {    //-> ilpw.numUsingParmPointer, ilpw.dspUsingParmPointer
				// Update ArrayList con info pointer
				ilpw.al_typePointerArea.add(EnumLogicSetPointerArea.POINTER_INSIDE_USING_PARM); 
				ilpw.al_dataItemPointer.add(dataItemPointer);	  
				ilpw.al_dspPointerInLinkageArea.add(ilpw.dspPointerInLinkageArea);
				ilpw.al_numUsingParmPointer.add(ilpw.numUsingParmFound);
				ilpw.al_dspUsingParmPointer.add(ilpw.dspInUsingParmFound);  
				continue;
			}

			// Pointer definito direttamente, o trasformato indirettamente, in area di linkage, sotto DFHCOMMAREA
			// La trasformazione può essere passata da definizioni strutturate in Working di DFHCOMMAREA
			if (entryPointerLvl01.getProgramSection() == EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION 
			&&	dataItemPointerLvl01.getDataName().equals("DFHCOMMAREA")) {
				// Update ArrayList con info pointer
				ilpw.al_typePointerArea.add(EnumLogicSetPointerArea.POINTER_INSIDE_CICS_DFHCOMMAREA); 
				ilpw.al_dataItemPointer.add(dataItemPointer);	  
				ilpw.al_dspPointerInLinkageArea.add(posPointer);    // 0-based
				ilpw.al_numUsingParmPointer.add(0);
				ilpw.al_dspUsingParmPointer.add(0);  
				continue;
			}

			// Pointer definito in area di linkage, Cics TWA
			if (entryPointerLvl01.getProgramSection() == EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION 
			&&  isDataItemInCicsTwaCsa(ilpw, "TWA", dataItemPointerLvl01)) {
				// Update ArrayList con info pointer
				ilpw.al_typePointerArea.add(EnumLogicSetPointerArea.POINTER_INSIDE_CICS_TWA); 
				ilpw.al_dataItemPointer.add(dataItemPointer);	  
				ilpw.al_dspPointerInLinkageArea.add(posPointer);     // 0-based
				ilpw.al_numUsingParmPointer.add(0);
				ilpw.al_dspUsingParmPointer.add(0);  
				continue;
			}

			// Pointer definito in area di linkage, Cics CSA
			if (entryPointerLvl01.getProgramSection() == EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION 
			&&  isDataItemInCicsTwaCsa(ilpw, "CSA", dataItemPointerLvl01)) {
				// Update ArrayList con info pointer
				ilpw.al_typePointerArea.add(EnumLogicSetPointerArea.POINTER_INSIDE_CICS_CSA); 
				ilpw.al_dataItemPointer.add(dataItemPointer);	  
				ilpw.al_dspPointerInLinkageArea.add(posPointer);   // 0-based
				ilpw.al_numUsingParmPointer.add(0);
				ilpw.al_dspUsingParmPointer.add(0);  
				continue;
			}

		} // end-for pointer individuati in istruzioni SET ADDRESS OF lnk-area TO Pointer

	}
	

	/*
	 * ----------------------------------------------------------------------
	 * Gestione ultima assegnazione da campo di sistema Cics.
	 * ----------------------------------------------------------------------
	 * 
	 * Vengono gestiti i casi di ultima assegnazione da campi in aree Cics quali:
	 * 
	 * 1) EIBTRMID
	 * 2) EIBTRNID
	 * 3) DFHCOMMAREA
	 * 4) TWA
	 * 5) CSA
	 * 
	 */
	private boolean lastSetByCicsSystem(LogicWorkProcess ilpw, DataItemCobolIdentifier identifierToDeal) throws ExceptionAmrita, SQLException {

		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;				// Singola trasformazione
		EntityDynamicFieldSubValue entityDynamicSubFieldValue;                  		// Singolo valore generato da trasformazione
		InstructionCobolDataItem dataItemLvl01 = null;                                  // Livello 01 sotto cui il campo in esame è definito
		LogicDynamicValue logicDynamicValue = null;                             		// Valore individuato		 
		ProgramCobolEntry<? extends Instruction> entryIdentifierToDeal = null;          // Contenitore campo in cui è definito il receiver
		ProgramCobolEntry<? extends Instruction> entryIdentifierToDealGroup = null;     // Contenitore campo di gruppo sotto cui è definito il receiver
		ArrayList<String> al_valueCicsField = null;			                    		// Valori da tabella esterna per EIBTRNID/EIBTRMID in Cics 
		String identifierToDealname = "";
		int numeDefGroupIdentifierToDeal = 0;                                           // Numero istruzione gruppo receiver

		if (ilpw.lastSetManaged) {
			return true;
		}

		// Non si tratta di una data item (dichiarato o inserito da precompilatore) 
		if (identifierToDeal.getIdentifierType() != EnumCobolReservedWords.DATA_DIV_DATA_ITEM
		&&  identifierToDeal.getIdentifierType() != EnumCobolReservedWords.PRECOMPILER_CICS	) {
			return false;
		}

		ilpw.curLastSet = EnumLogicSetMode.NOT_ASSIGNED;

		// Imposto il nome del campo da cercare, dal receiver 
		identifierToDealname = identifierToDeal.getNameIdentifier();

		// Il data item, se campo Cics, deve essere in Linkage Section
		// Tuttavia campi come EIBTRMID e EIBTRNID, vengono inseriti dal precompilatore Cics e non si trovano nelle definizioni di Linkage

		// Terminale
		if (identifierToDealname.equals("EIBTRMID")) {
			ilpw.externalObjectName = "EIBTRMID";
			ilpw.externalIdFieldColumn = "EIBTRMID"; 
			ilpw.externalTypeSystemField = EnumDataItemSystemEnvironment.CICS_EIBTRMID;
			ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRMID;

		// Transazione
		} else if (identifierToDealname.equals("EIBTRNID")) {
			ilpw.externalObjectName = "EIBTRNID";
			ilpw.externalIdFieldColumn = "EIBTRNID"; 
			ilpw.externalTypeSystemField = EnumDataItemSystemEnvironment.CICS_EIBTRNID;
			ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRNID;
 
		// Verifica se DFHCOMMAREA|TWA|CSA
		} else {	
			
			// Non è un campo di linkage, probabilmente definito in Working
			entryIdentifierToDeal = ilpw.programCur.entryDataDivision(identifierToDeal.getDataItem().getNumInstr());
			if (entryIdentifierToDeal.getProgramSection() != EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION) {
				ilpw.lastSetManaged = false;
				return false;
			}
			
			// DFHCOMMAREA 
			// Deve essere definita un'area in Linkage con questo nome    
			dataItemLvl01 = getLvl01Owner(ilpw, identifierToDeal.getDataItem());
			if (identifierToDeal.getNameIdentifier().toUpperCase().equals("DFHCOMMAREA") 
			||  dataItemLvl01.getDataName().equals("DFHCOMMAREA")) {
				// Imposto entry di definizione del campo da cercare
				entryIdentifierToDeal = ilpw.programCur.entryDataDivision(identifierToDeal.getNumInstr());
				ilpw.externalTypeObject = EnumObject.OBJECT_CICS_SYSTEM_FIELD;
				ilpw.externalTypeSystemField = EnumDataItemSystemEnvironment.CICS_DFHCOMMAREA;
				ilpw.externalObjectName = "DFHCOMMAREA";
				ilpw.externalIdFieldColumn = "";				
				ilpw.externalDsname = "";
				ilpw.externalCicsName = "";
				ilpw.dspFieldInLinkageArea = ilpw.programCur.entryDataDivision(identifierToDeal.getNumInstr()).getPos();
				ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_CICS_DFHCOMMAREA;
				// Mark istruzione dinamica origine e descrittore campo dinamico in struttura db come dynamic spreaded
				// Solo se si stanno eseguendo logiche stesso programma
				if (!ilpw.isExecLogicSpreaded) {
					ilpw.instrDynamicOrigin.setDynamicSpreaded(true);
					ilpw.dynamicFieldSubToSolve.isSpreaded = true;
				}
		
			// TWA
			// Verifica se presenti:
			//   Exec Cics Address Twa(pointer) end-exec
			//   Exec Cics Address Set Twa(Address Of Area) Using Pointer end-exec oppure SET Address Of Area TO Pointer
			} else if (isDataItemInCicsTwaCsa(ilpw, "TWA", identifierToDeal.getDataItem()) && !ilpw.isExecLogicSpreaded) {
	            // Impostazioni per TWA
				ilpw.externalTypeObject = EnumObject.OBJECT_CICS_SYSTEM_FIELD;
				ilpw.externalTypeSystemField = EnumDataItemSystemEnvironment.CICS_TWA;
				ilpw.externalObjectName = "TWA";
				ilpw.externalIdFieldColumn = "";
				ilpw.externalDsname = "";
				ilpw.externalCicsName = "";
				// Adjust pos se necessario, riferita al livello 01
				if (ilpw.identifierReceiver.getDataItem().isGroupField()) {
					numeDefGroupIdentifierToDeal = ilpw.programCur.groupOwnerDefinition(identifierToDeal.getNumInstr());
					entryIdentifierToDealGroup = ilpw.programCur.entryDataDivision(numeDefGroupIdentifierToDeal);
					ilpw.posRcv = ilpw.posRcv + entryIdentifierToDealGroup.getPos();
				} else {
					entryIdentifierToDeal = ilpw.programCur.entryDataDivision(identifierToDeal.getDataItem().getNumInstr());
					ilpw.posRcv = ilpw.posRcv + entryIdentifierToDeal.getPos();
				}
				// Mark istruzione dinamica origine e descrittore campo dinamico in struttura db come dynamic spreaded
				// Solo se si stanno eseguendo logiche stesso programma
				if (!ilpw.isExecLogicSpreaded) {
					ilpw.instrDynamicOrigin.setDynamicSpreaded(true);
					ilpw.dynamicFieldSubToSolve.isSpreaded = true;
				}
				ilpw.dspFieldInLinkageArea = ilpw.posRcv - 1;
				ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_CICS_TWA;
	
			// CSA
			// Verifica se presenti:
			//   Exec Cics Address Csa(pointer) end-exec
			//   Exec Cics Address Set Csa(Address Of Area) Using Pointer end-exec oppure SET Address Of Area TO Pointer
			} else if (isDataItemInCicsTwaCsa(ilpw, "CSA", identifierToDeal.getDataItem()) && !ilpw.isExecLogicSpreaded) {
				// Impostazioni per CSA
				ilpw.externalTypeObject = EnumObject.OBJECT_CICS_SYSTEM_FIELD;
				ilpw.externalTypeSystemField = EnumDataItemSystemEnvironment.CICS_CSA;
				ilpw.externalObjectName = "CSA";
				ilpw.externalIdFieldColumn = "";
				ilpw.externalDsname = "";
				ilpw.externalCicsName = "";
				// Adjust pos se necessario, riferita al livello 01
				if (ilpw.identifierReceiver.getDataItem().isGroupField()) {
					numeDefGroupIdentifierToDeal = ilpw.programCur.groupOwnerDefinition(identifierToDeal.getNumInstr());
					entryIdentifierToDealGroup = ilpw.programCur.entryDataDivision(numeDefGroupIdentifierToDeal);
					ilpw.posRcv = ilpw.posRcv + entryIdentifierToDealGroup.getPos();
				} else {
					entryIdentifierToDeal = ilpw.programCur.entryDataDivision(identifierToDeal.getDataItem().getNumInstr());
					ilpw.posRcv = ilpw.posRcv + entryIdentifierToDeal.getPos();
				}
				// Mark istruzione dinamica origine e descrittore campo dinamico in struttura db come dynamic spreaded
				// Solo se si stanno eseguendo logiche stesso programma
				if (!ilpw.isExecLogicSpreaded) {
					ilpw.instrDynamicOrigin.setDynamicSpreaded(true);
					ilpw.dynamicFieldSubToSolve.isSpreaded = true;
				}
				ilpw.dspFieldInLinkageArea = ilpw.posRcv - 1;
				ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_CICS_CSA;
	
			// Non è un campo Cics gestito
			} else {
				ilpw.lastSetManaged = false;
				return false;
			}
		}
			
		// Default per valori esterni non immediatamente recuperabili
		al_valueCicsField = new ArrayList<String>();
		
		// Nuovo oggetto con informazioni ultima trasformazione
		ilpw.logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting();

		// Estrazione valori memorizzati per EIBTRMID e EIBTRNID
		if (ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRMID 
		||  ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRNID) {
			
			// Recupero valori (o una loro parte), di EIBTRMID/EIBTRNID
			ilpw.externalTypeObject = EnumObject.OBJECT_CICS_SYSTEM_FIELD;
			ilpw.externalDsname = "";
			ilpw.externalPosColumn = 1;
			ilpw.externalLngColumn = 4;
			ilpw.externalPosInColumn = ilpw.posRcv;
			ilpw.externalLngInColumn = ilpw.lngRcv;
			al_valueCicsField = detectValuesFromExternalMedia(ilpw);
			// Impostazione flag di waiting for external data in assegnazione e sottocampo
			if (al_valueCicsField.size() == 0) {
				ilpw.dynamicFieldSubToSolve.isWaitingForExternalData = true;
				ilpw.logicDynamicFieldSubSettingWrk.isWaitingForExternalData = true;
				ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = false;
				ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSubWaitExt = ilpw.entityDynamicFieldSubWaitExtWrk;
			} else {				
				ilpw.dynamicFieldSubToSolve.isWaitingForExternalData = false;
				ilpw.logicDynamicFieldSubSettingWrk.isWaitingForExternalData = false;
				ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;
			}
		}

		// Campo di sistema Cics gestito
 
		// Valorizzazione preliminare info trasformazione in struttura db, usata anche come servizio
		// Attributi, tra cui identifierSender, saranno aggiornati in seguito
		entityDynamicSubFieldSetting = storeInfoDbSingleSetting(ilpw);
		
		// Info specifiche ultima trasformazione
		entityDynamicSubFieldSetting.setSetMode(ilpw.curLastSet);  
		entityDynamicSubFieldSetting = storeInfoDbLastSetting(ilpw, entityDynamicSubFieldSetting);
		entityDynamicSubFieldSetting.setDspFieldInLinkageArea(ilpw.dspFieldInLinkageArea);  
		entityDynamicSubFieldSetting.setFieldReceiverNum(0);
		entityDynamicSubFieldSetting.setFieldReceiverId("");
		entityDynamicSubFieldSetting.setFieldReceiverPos(0);										 
		entityDynamicSubFieldSetting.setFieldReceiverLng(0);										 		

		// Catena, posizione e lunghezza in sottocampo origine valorizzata dalla trasformazione
		ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicSubFieldSetting;
		ilpw.logicDynamicFieldSubSettingWrk.numChainSet = ilpw.numChainSet;
		ilpw.logicDynamicFieldSubSettingWrk.posInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getPosInSubField();
		ilpw.logicDynamicFieldSubSettingWrk.lngInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getLngInSubField();
		
		// Valori estratti, utilizzati successivamente, salvati solo come valori per comodità a livello di ultima assegnazione
		ilpw.logicDynamicFieldSubSettingWrk.al_Value = al_valueCicsField;   
        if (al_valueCicsField.size() > 0) {
        	ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;
		}
        
		// Inserimento valori in struttura logica dinamica valori sottocampo dinamico 
		// Valori multipli possibili solo per per EIBTRMID, EIBTRNID, se dati disponibili da struttura esterna
		for (String value : al_valueCicsField) {
			logicDynamicValue = new LogicDynamicValue(program.getSysOwner(), program.getSubSysOwner());		
			logicDynamicValue.value = value;											// Valore campo o sottocampo
			logicDynamicValue.pgmSet = ilpw.programCur.programName;                        // Programma di assegnazione del valore	
			logicDynamicValue.numChainSet = ilpw.numChainSet;                           // Numero catena di assegnazione nel programma di assegnazione
			logicDynamicValue.numInstrSet = ilpw.instrCobolProcSet.getNumInstr();       // Numero istruzione che ha generato il valore nel programma corrente
			
			// Informazioni valore in struttura db
			entityDynamicSubFieldValue = storeInfoDbSingleValue(ilpw);
			entityDynamicSubFieldValue.setPosInSubField(ilpw.logicDynamicFieldSubSettingWrk.posInSubField); // Posizione in sottocampo (1-based) inizio valore di questa assegnazione
			entityDynamicSubFieldValue.setLngInSubField(ilpw.logicDynamicFieldSubSettingWrk.lngInSubField); // Lunghezza in sottocampo valorizzata da questa assegnazione
			entityDynamicSubFieldValue.setValue(value);          							 			 // Valore sottocampo o campo 
			entityDynamicSubFieldValue.setTypeObjectFrom(EnumObject.OBJECT_CICS_SYSTEM_FIELD);  		 // Tipo oggetto (pgm, File,  etc) dove il valore è stato impostato
			entityDynamicSubFieldValue.setIdObjectFrom(ilpw.externalObjectName);	  					 // Nome oggetto (Cics) dove il valore è stato impostato
			entityDynamicSubFieldValue.setIdPgmFrom(ilpw.programCur.programName);							 // Pgm di assegnazione corrente
			entityDynamicSubFieldValue.setNumInstrFrom(ilpw.instrCobolProcSet.getNumInstr());			 // Numero istruzione di assegnazione		
			logicDynamicValue.entityDynamicValue = entityDynamicSubFieldValue;          				 // Save in struttura dinamica valore
			
			ilpw.dynamicFieldSubToSolve.al_value.add(logicDynamicValue);                                 // Save valore in struttura sottocampo da risolvere                   
		}		
		
		ilpw.lastSetManaged = true;
		return true;
	}

    /*
     * Aggiornamento informazioni per db di ultima assegnazione ancora da inserire.
     * Per esempio per LAST_SET_BY_CICS_FIELD necessario impostare receiver ultimo assegnazione
     * e posizione e lunghezza di sender e receiver.
     * Se non esistono assegnazioni precedenti, il campo di assegnazione, per esempio EIBTRMID, o una colonna di tabellla
     * era un operando dell'istruzione e quindi non si effettua nessuna operazione.    
     */
	private EntityDynamicFieldSubSetting storeInfoDbLastSetting(LogicWorkProcess ilpw, EntityDynamicFieldSubSetting entityDynamicSubFieldSetting) {
		EntityDynamicFieldSubSetting entityDynamicSubFieldSettingPrec = null;
		int iLast = 0;
		
		// Non esistono assegnazioni precedenti
		if (ilpw.al_chainSetSubFieldCur.size() == 0) {
			return entityDynamicSubFieldSetting;
		}
		
		// Get previous last setting
		iLast = ilpw.al_chainSetSubFieldCur.size() - 1;
		entityDynamicSubFieldSettingPrec = ilpw.al_chainSetSubFieldCur.get(iLast).entityDynamicFieldSetting;
		
		// Receiver, info uguali ad assegnazione precedente		
		entityDynamicSubFieldSetting.setFieldReceiverId(entityDynamicSubFieldSettingPrec.getFieldReceiverId());
		entityDynamicSubFieldSetting.setFieldReceiverNum(entityDynamicSubFieldSettingPrec.getFieldReceiverNum());
		entityDynamicSubFieldSetting.setFieldReceiverPos(entityDynamicSubFieldSettingPrec.getFieldReceiverPos());
		entityDynamicSubFieldSetting.setFieldReceiverLng(entityDynamicSubFieldSettingPrec.getFieldReceiverLng());
		
		// Sender, info uguali ad assegnazione precedente
		entityDynamicSubFieldSetting.setFieldSenderId(entityDynamicSubFieldSettingPrec.getFieldSenderId());
		entityDynamicSubFieldSetting.setFieldSenderNum(entityDynamicSubFieldSettingPrec.getFieldSenderNum());
		entityDynamicSubFieldSetting.setFieldSenderPos(entityDynamicSubFieldSettingPrec.getFieldSenderPos());
		entityDynamicSubFieldSetting.setFieldSenderLng(entityDynamicSubFieldSettingPrec.getFieldSenderLng());

		// Flag di soluzione e waiting su dati esterni
		entityDynamicSubFieldSetting.setWaitingForExternalData(ilpw.logicDynamicFieldSubSettingWrk.isWaitingForExternalData);
		entityDynamicSubFieldSetting.setSolvedObjExt(ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved);
		
		return entityDynamicSubFieldSetting;
	}

	/*
	 * -----------------------------------------------------------------------------
	 * Gestione ultima assegnazione da campo con value 
	 * -----------------------------------------------------------------------------
	 * 
	 * Se literal e' relativa al valore iniziale di un campo senza più assegnazioni.
	 * Se literal numerica Il campo dovrebbe essere definito Zoned e il valore numerico fillato con zeri.
     *
     * Se literal alfanumerica
 	 * Sono disponibili la posizione e lunghezza dai quali estrarre il valore,
	 * ereditati dal processo ricorsivo backward di assegnazione.
	 * Il valore assegnato al sottocampo origine viene estratto dal valore iniziale
	 * del campo o della literal di ultima assegnazione.
	 * Tale valore può risultare troncato, fillato a spaces fino alla lunghezza in
	 * bytes del sottocampo origine o rappresentare una porzione all'interno
	 * della literal di value. 
     *
	 * Receiver non valorizzato con value.
	 * L'attivazione è a fronte di campo senza ulteriori assegnazioni.
	 * Si cerca in value identificatore receiver.
	 *
	 * 	ZERO|ZEROES
	 *  SPACE|SPACES
	 *  Literal alphanumeric
	 *  Literal numeric

	 */
	private boolean lastSetByValueDefault(LogicWorkProcess ilpw) {

		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;		// Singola trasformazione
		InstructionCobolDataItem dataItemCobol = null; 							// Descrittore di servizio definizione dati										 
		DataItemCobolIdentifier identifierOperand = null;                       // Identificatore sender o receiver da trattare
		String valueInitialText = "";											// Valore iniziale data item 
		int valueNumeric = 0;											        // Valore literal	
		int sizeBytesSubField = 0;   											// Dimensioni in bytes del sottocampo origine                               


		if (ilpw.lastSetManaged) {
			return true;
		}

		// Campo receiver indicizzato: exit
		if (ilpw.identifierReceiver.getQualifier().isOccursed()) {
			return false;
		}

		// Imposto il nome del campo da cercare, dal receiver
		identifierOperand = ilpw.identifierReceiver;
		ilpw.curLastSet = EnumLogicSetMode.NOT_ASSIGNED;

		// Si tratta di un data item non definito di un precompilatore
		if (ilpw.identifierReceiver.getIdentifierType()  == EnumCobolReservedWords.PRECOMPILER_CICS
		||  ilpw.identifierReceiver.getIdentifierType()  == EnumCobolReservedWords.PRECOMPILER_SQL) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Estrazione dimensioni originarie sottocampo in bytes
		sizeBytesSubField = ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getDataItem().getSizeBytes();

		// Se esecuzione logiche spreaded su un pgm chiamante, la lunghezza da considerare non è quella del
		// sottocampo ma quella con cui è stato startato il processo ricorsivo sull'ultima assegnazione.
		// Per esempio a fronte di LAST_SET_BY_CICS_DFHCOMMAREA si deve considerare come lunghezza campo
		// quella indicata in ultima assegnazione e non quella del campo di commarea.
		if (ilpw.isExecLogicSpreaded) {
			sizeBytesSubField = ilpw.lngRcvInSubFieldSpreaded;
		}

		// Identifier Data item.  
		if (identifierOperand.getIdentifierType() == EnumCobolReservedWords.DATA_DIV_DATA_ITEM
		||  identifierOperand.getNameIdentifier().toUpperCase().equals("FILLER")) {

			// Recupero istruzione definizione dati memorizzata nell'identificatore
			dataItemCobol = identifierOperand.getDataItem();

			// Campo numerico 
			if (dataItemCobol.getGenericType() == EnumDataItemGeneric.DATA_ITEM_NUMERIC) {
				// Value costante figurativa
				if (dataItemCobol.getValueType() == EnumCobolValueType.VALUE_FIGURATIVE) {
					// Value ZERO
					if (dataItemCobol.getValueFigurative() == EnumCobolFigurativeConstants.ZERO
					||  dataItemCobol.getValueFigurative() == EnumCobolFigurativeConstants.ZEROS   
					||  dataItemCobol.getValueFigurative() == EnumCobolFigurativeConstants.ZEROES){  
						ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_ZEROES;
						ilpw.valueLiteral = StringService._pad("0", '0', sizeBytesSubField, StringService.PAD_LEFT);
					}
				// Value number
				} else if (dataItemCobol.getValueType() == EnumCobolValueType.VALUE_LITERAL_NUM_INT 
					|| dataItemCobol.getValueType() == EnumCobolValueType.VALUE_LITERAL_NUM_LONG) {
					valueNumeric = dataItemCobol.getValueNumeric();
					ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_LITERAL_NUM;
					ilpw.valueLiteral = StringService._pad(Integer.toString(valueNumeric), '0', sizeBytesSubField, StringService.PAD_LEFT);
				}
			}
			// Campo text PIC X
			if (dataItemCobol.getGenericType() == EnumDataItemGeneric.DATA_ITEM_TEXT) {
				// Value costante figurativa
				if (dataItemCobol.getValueType() == EnumCobolValueType.VALUE_FIGURATIVE) {
					// Value space
					if (dataItemCobol.getValueFigurative() == EnumCobolFigurativeConstants.SPACE
					||  dataItemCobol.getValueFigurative() == EnumCobolFigurativeConstants.SPACES){  
						ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_SPACES;
						ilpw.valueLiteral = StringService._pad(" ", ' ', sizeBytesSubField, StringService.PAD_RIGHT);
					}
				// Value costante alfanumerica
				} else if(dataItemCobol.getValueType() == EnumCobolValueType.VALUE_LITERAL_ALPHA) {
					valueInitialText = dataItemCobol.getValueStringFormatted().trim();
					ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_VALUE_LITERAL_ALPHA;
					ilpw.valueLiteral = StringService._pad(valueInitialText, ' ', sizeBytesSubField, StringService.PAD_RIGHT);
				}
			}
		}

		
		// Il campo non ha value gestito
		if (ilpw.curLastSet == EnumLogicSetMode.NOT_ASSIGNED) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Valorizzazione preliminare info trasformazione in struttura db, usata anche come servizio
		// Attributi, tra cui identifierSender, saranno aggiornati in seguito
		entityDynamicSubFieldSetting = storeInfoDbSingleSetting(ilpw);

		// Info specifiche ultima trasformazione
		entityDynamicSubFieldSetting.setSetMode(ilpw.curLastSet);
		entityDynamicSubFieldSetting.setFieldSenderNum(entityDynamicSubFieldSetting.getFieldReceiverNum());
		entityDynamicSubFieldSetting.setFieldSenderId(entityDynamicSubFieldSetting.getFieldReceiverId());
		entityDynamicSubFieldSetting.setFieldSenderPos(entityDynamicSubFieldSetting.getFieldReceiverPos());										 
		entityDynamicSubFieldSetting.setFieldSenderLng(entityDynamicSubFieldSetting.getFieldReceiverLng());										 
		entityDynamicSubFieldSetting.setFieldReceiverNum(0);
		entityDynamicSubFieldSetting.setFieldReceiverId("");
		entityDynamicSubFieldSetting.setFieldReceiverPos(0);										 
		entityDynamicSubFieldSetting.setFieldReceiverLng(0);										 
		entityDynamicSubFieldSetting.setValue(ilpw.valueLiteral);

		// Nuovo oggetto con informazioni ultima trasformazione
		ilpw.logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting();
		ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicSubFieldSetting;
		ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;

		// Catena, posizione e lunghezza in sottocampo origine valorizzata dalla trasformazione
		ilpw.logicDynamicFieldSubSettingWrk.numChainSet = ilpw.numChainSet;
		ilpw.logicDynamicFieldSubSettingWrk.posInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getPosInSubField();
		ilpw.logicDynamicFieldSubSettingWrk.lngInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getLngInSubField();

		ilpw.lastSetManaged = true;
		return true;
	}


	/*
	 * -----------------------------------------------------------------------------
	 * Gestione ultima assegnazione da Move di literal/space/zero
	 * -----------------------------------------------------------------------------
	 * 
	 * Si gestisce l'ultima assegnazione da 
	 *  MOVE literal-alpha  
	 *  MOVE literal-numeric  
	 *  MOVE ZERO
	 *  MOVE spaces
	 *    
	 * ereditati dal processo ricorsivo backward di assegnazione.
	 * Il valore assegnato al sottocampo origine viene estratto dal valore iniziale
	 * del campo o della literal di ultima assegnazione.
	 * Tale valore può risultare troncato, fillato a spaces fino alla lunghezza in
	 * bytes del sottocampo origine o rappresentare una porzione all'interno
	 * della literal di value. 
	 * 
	 */
	private boolean lastSetByMoveLiteralSpaceZero(LogicWorkProcess ilpw, DataItemCobolIdentifier identifierToDeal) {
		LogicDynamicValue logicDynamicValue = null;                             // Valore individuato
		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;		// Singola trasformazione in catena trasformazioni
   		EntityDynamicFieldSubValue entityDynamicSubFieldValue;                  // Singolo valore generato da trasformazione
 		int sizeBytesSubField = 0;   											// Dimensioni in bytes del sottocampo origine                               

		if (ilpw.lastSetManaged) {
			return true;
		}

		// Si tratta di un data item non definito di un precompilatore
		if (identifierToDeal.getIdentifierType()  == EnumCobolReservedWords.PRECOMPILER_CICS
		||  identifierToDeal.getIdentifierType()  == EnumCobolReservedWords.PRECOMPILER_SQL) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Campo receiver indicizzato: exit
		if (identifierToDeal.getQualifier().isOccursed()) {
			return false;
		}

		ilpw.curLastSet = EnumLogicSetMode.NOT_ASSIGNED;
		
		// Estrazione dimensioni originarie sottocampo in bytes e numero istruzione dinamica
		sizeBytesSubField = ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getDataItem().getSizeBytes();

		// Se esecuzione logiche spreaded su un pgm chiamante, la lunghezza da considerare non è quella del
		// sottocampo ma quella con cui è stato startato il processo ricorsivo sull'ultima assegnazione.
		// Per esempio a fronte di LAST_SET_BY_CICS_DFHCOMMAREA si deve considerare come lunghezza campo
		// quella indicata in ultima assegnazione e non quella del campo di commarea.
		if (ilpw.isExecLogicSpreaded) {
			sizeBytesSubField = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();
		}

		// MOVE literal-alphanuneric 
		if (identifierToDeal.getIdentifierType() == EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
			ilpw.valueLiteral = identifierToDeal.getValueStringFormatted();
			ilpw.valueLiteral = StringService._pad(ilpw.valueLiteral, ' ', sizeBytesSubField, StringService.PAD_RIGHT);
			ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_LITERAL_ALPHA;
		// MOVE literal-numeric
		} else if (identifierToDeal.getIdentifierType() == EnumCobolReservedWords.OPERAND_LITERAL_NUM) {
			ilpw.valueLiteral = identifierToDeal.getQualifier().getSymbolValueNumericLong() + "";
			ilpw.valueLiteral = StringService._pad(ilpw.valueLiteral, '0', sizeBytesSubField, StringService.PAD_RIGHT);
			ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_LITERAL_NUM;
		// MOVE SPACE|SPACES
		} else if (identifierToDeal.getIdentifierType() == EnumCobolReservedWords.FIGURATIVE_SPACE) {
			ilpw.valueLiteral = StringService._pad(" ", ' ', sizeBytesSubField, StringService.PAD_LEFT);
			ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_SPACES;
		// MOVE ZERO|ZEROES
		} else if (identifierToDeal.getIdentifierType() == EnumCobolReservedWords.FIGURATIVE_ZERO) {
			ilpw.valueLiteral = StringService._pad("0", '0', sizeBytesSubField, StringService.PAD_RIGHT);
			ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_ZEROES;
		}

		// Il campo non ha value literal alfanumeric
		if (ilpw.curLastSet == EnumLogicSetMode.NOT_ASSIGNED) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Valorizzazione preliminare info trasformazione in struttura db, usata anche come servizio
		// Attributi, tra cui identifierSender, saranno aggiornati in seguito
		entityDynamicSubFieldSetting = storeInfoDbSingleSetting(ilpw);
		
		// Info specifiche ultima trasformazione
		entityDynamicSubFieldSetting.setSetMode(ilpw.curLastSet);
		entityDynamicSubFieldSetting.setValue(ilpw.valueLiteral);

		// Nuovo oggetto con informazioni ultima trasformazione
		ilpw.logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting();
		ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicSubFieldSetting;
		ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;

		// Catena, posizione e lunghezza in sottocampo origine valorizzata dalla trasformazione
		ilpw.logicDynamicFieldSubSettingWrk.numChainSet = ilpw.numChainSet;
		ilpw.logicDynamicFieldSubSettingWrk.posInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getPosInSubField();
		ilpw.logicDynamicFieldSubSettingWrk.lngInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getLngInSubField();

		// Inserimento valori in struttura assegnazione
		ilpw.logicDynamicFieldSubSettingWrk.al_Value.add(ilpw.valueLiteral);
		logicDynamicValue = new LogicDynamicValue(program.getSysOwner(), program.getSubSysOwner());		
		logicDynamicValue.value = ilpw.valueLiteral;								// Valore campo o sottocampo
		logicDynamicValue.pgmSet = ilpw.programCur.programName;                        // Programma di assegnazione del valore	
		logicDynamicValue.numChainSet = ilpw.numChainSet;                           // Numero catena di assegnazione nel programma di assegnazione
		logicDynamicValue.numInstrSet = ilpw.instrCobolProcSet.getNumInstr();       // Numero istruzione che ha generato il valore nel programma corrente
		
		// Informazioni valore in struttura db
		entityDynamicSubFieldValue = storeInfoDbSingleValue(ilpw);
		entityDynamicSubFieldValue.setPosInSubField(ilpw.logicDynamicFieldSubSettingWrk.posInSubField); // Posizione in sottocampo (1-based) inizio valore di questa assegnazione
		entityDynamicSubFieldValue.setLngInSubField(ilpw.logicDynamicFieldSubSettingWrk.lngInSubField); // Lunghezza in sottocampo valorizzata da questa assegnazione
		entityDynamicSubFieldValue.setValue(ilpw.valueLiteral);          							 // Valore sottocampo o campo 
		entityDynamicSubFieldValue.setTypeObjectFrom(EnumObject.OBJECT_PGM_COBOL);  				 // Tipo oggetto (pgm, File,  etc) dove il valore è stato impostato
		entityDynamicSubFieldValue.setIdObjectFrom(ilpw.programCur.programName);	  					 // Nome oggetto (pgm, File,  etc) dove il valore è stato impostato
		entityDynamicSubFieldValue.setIdPgmFrom(ilpw.programCur.programName);							 // Pgm di assegnazione corrente
		entityDynamicSubFieldValue.setNumInstrFrom(ilpw.instrCobolProcSet.getNumInstr());			 // Numero istruzione di assegnazione		
		logicDynamicValue.entityDynamicValue = entityDynamicSubFieldValue;          				 // Save in struttura dinamica valore
		
		ilpw.dynamicFieldSubToSolve.al_value.add(logicDynamicValue);                                 // Save valore in struttura sottocampo da risolvere                   
		
		ilpw.lastSetManaged = true;
		return true;
	}	


	/*-------------------------------------------------------------------
	 * Gestione ultima assegnazione da campo in tabella di working.
	 * ------------------------------------------------------------------
	 * 
	 * Questo metodo viene chiamato a fronte di una assegnazione con Move.
	 * Sono valorizzati gli identificatori Sender, Receiver e il numero di 
	 * istruzione.
	 * 
	 * Il campo sender è un elemento indicizzato di una tabella Occurs di n dimensioni.
	 * Il campo receiver è quello del quale si cercano i valori assegnati.
	 * 
	 * 
	 * Tipologia casi
	 * --------------
	 * 
	 * A titolo esplicativo si elencano le casistiche gestite:
	 * 
	 * 1) 02 TAB-WS.
	 *       05 FILLER   PIC X VALUE 'A'.
	 *       05 FILLER   PIC X VALUE SPACE.
	 *       ---- 70 Bytes length
	 *       05 FILLER   PIC X VALUE 'B'.
	 *    02 TAB-OCC REDEFINES TAB-WS.
	 *       05 TAB-REDEFINED.
	 *          07 ROW OCCURS 5.
	 *             09 COL OCCURS 7.
	 *                11 CAMPO1 PIC X.
	 *                11 CAMPO2 PIC X.
	 * 
	 * 2) 01 TAB-WS PIC X(70) VALUE 'A B ................'.
	 *    01 TAB-OCC REDEFINES TAB-WS OCCURS 70 PIC X.
	 *    
	 * 3) 01 TAB-WS VALUE 'A B ................'.
	 *      05 TAB-OCC OCCURS 70 PIC X.
	 * 
	 * 4) 01 TAB-WS PIC X(70) VALUE 'A B ................'.
	 *    01 TAB-OCC REDEFINES TAB-WS.
	 *       05 EL-TAB-OCC OCCURS 35.
	 *          07 CAMPO1 PIC X.
	 *          07 CAMPO2 PIC X.
	 *                
	 * 5) 01 TAB-WS  VALUE 'A B ................'.   90 crt
	 *       05 TAB-OCC-GRP.
	 *          07 EL-TAB-OCC1 OCCURS 30.
	 *             09 EL-TAB-OCC2 OCCURS 3.
	 *                11 CAMPO1 PIC X.
	 * 
	 * 6) 01 T2. 
	 *       05 T-OBJ PIC 9 VALUE 3. 
	 *       05 T OCCURS 5 TIMES DEPENDING ON T-OBJ. 
	 *          10 X PIC XX VALUE “AA”. 
	 *          10 Y PIC 99 VALUE 19. 
	 *          10 Z PIC XX VALUE “BB”.
	 * 
	 * 
	 * 7) 01 SAMPLE-TABLE-FOUR 05 TABLE-DEPTH OCCURS 3 TIMES INDEXED BY INX-A. 
	 *       10 TABLE-ROW OCCURS 4 TIMES INDEXED BY INX-B. 
	 *          15 TABLE-COLUMN OCCURS 8 TIMES INDEXED BY INX-C PIC X(8). 
	 *       
	 *   Suppose you code the following relative indexing reference to SAMPLE-TABLE-FOUR:
	 *        TABLE-COLUMN (INX-A + 1, INX-B + 2, INX-C - 1)
	 *        
	 *   This reference causes the following computation of the displacement to the TABLE-COLUMN element: 
	 *         
	 *           (contents of INX-A) + (256 * 1) 
	 *         + (contents of INX-B) + (64 * 2) 
	 *         + (contents of INX-C) - (8 * 1) 
	 *         
	 *    This calculation is based on the following element lengths: 
	 *         v Each occurrence of TABLE-DEPTH is 256 bytes in length (4 * 8 * 8). 
	 *         v Each occurrence of TABLE-ROW is 64 bytes in length (8 * 8). 
	 *         v Each occurrence of TABLE-COLUMN is 8 bytes in length.
	 * 
	 * 
	 * 
	 * 
	 * 
	 * Algoritmo di soluzione
	 * ----------------------
	 * 
	 * 1) Verifica se receiver indicizzato da tabella di working storage
	 * 2) Calcolo posizione fisica, lunghezza valore nella tabella
	 * 3) Individuazione area di working ridefinita dalla tabella
	 * 4) Composizione di stringa completa valori a partire dai singoli valori iniziali
	 * 5) Estrazione valore cercato
	 * 
	 * 
	 * Valori estratti
	 * ---------------
	 * 
	 * Dopo aver individuato la tabella ridefinita, il numero e la dimensione degli elementi
	 * e la posizione del data item in ogni elemento della tabella, si pone il problema di
	 * estrarre i valori potenzialmente insiti nell'assegnazione.
	 * Se tutti gli indici del campo receiver sono dei campi, l'istruzione potenzialmente
	 * assegna, supponendo due indici, il valore, di tutte le righe. In pratica
	 * i valori potenziali sono una intera colonna della tabella.
	 * 
	 * 
	 */
	private boolean lastSetByMoveTbItemOfOccurs(LogicWorkProcess ilpw, DataItemCobolIdentifier identifierToDeal) {

		LogicDynamicValue logicDynamicValue = null;                             // Valore individuato
		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;		// Singola trasformazione
		EntityDynamicFieldSubValue entityDynamicSubFieldValue;                  // Singolo valore generato da trasformazione
		InstructionCobolDataItem dataItemCobol = null; 							// Descrittore di servizio definizione dati										 

		// Info sui valori estratti dalla tabella
		ArrayList<String> al_valueFromTable = null;						        // Valori validi estratti dalla tabella
		String valueTable = "";													// Stringa con valore completo tabella
		String curValueReceiver = "";                                           // Valore receiver in uno specifico elemento di tabella

		// Varie
		String curElementTable = "";											// Stringa con valore elemento tabella corrente
		String curElementTableField = "";										// Stringa con valore campo das elemento tabella corrente
		String ar_indexLiteral[] = null;                                        // Restituito da classe CobolExpression
		String indexLiteral = "";                                               // Valore testuale literal indice 
		String redefinesTableDataName = "";                                     // Nome campo di gruppo ridefinito con Values tabella
		ExpressionCobol ar_indexUsed[] = null;                                  // Indici utilizzati nel qualificatore sender
		ExpressionCobol exprIndex = null;                                       // Espresione singolo indice
		ArrayList<Integer> al_dimTable = null;									// Numero Occurs per ogni dimensione
		String valueLiteral = "";											    // Valore iniziale o literal in Move
		int ar_groupOwnerPointer[] = null;                                      // Campi di guppo sotto il quale è definito ll campo di elemento di tabella
		int ar_elementTablePointer[] = null;                                    // Campi componenti elemento di tabella
		int sizeBytesSubField = 0;   											// Dimensioni in bytes del sottocampo origine                               
		int index1Numeric = 0;                                                  // Valore numerico literal indice 
		int index2Numeric = 0;                                                  // Valore numerico literal indice 

		// Numeri istruzioni di definizione 
		int ar_NumDefInstr[] = null;											// Pointers a definizioni dati
		int numInstrDefReceiver = 0;                                            // Numero istruzione definizione receiver
		int numInstrDefFirstDimOccurs = 0;                                      // Numero istruzione definizione prima  Occurs
		int numInstrDefLastDimOccurs = 0;                                       // Numero istruzione definizione ultima Occurs
		int numInstrDefFirstElement = 0;                                        // Numero istruzione definizione primo  campo elemento
		int numInstrDefLastElement = 0;                                         // Numero istruzione definizione ultimo campo elemento
		int numInstrDefRedefinedTable = 0;                                      // Numero istruzione definizione tabellas ridefinita con values

		// Info tabella e elemento di tabella
		int cntDimTable = 0;													// Contatore dimensioni tabella
		int numDimTable = 0;                                                    // Numero dimensioni tabella
		int sizeElementTable = 0;												// Lunghezza in bytes elemento di tabella
		int dspElementTable = 0;												// Displacement elemento di tabella
		int numTotElementTable = 0;												// Numero totale elementi di tabella
		int numElementTableRequired = -1;										// Numero specifico elemento di tabella indexato (-1 = tutti)
		int dspReceiverInElementTable = 0;										// Displacemet campo receiver indexato in elemento di tabella
		int posInElementTable = 0;                                              // Posizione in elemento sender (pos rcv o pos ref mod se presente)
		int i = 0;																// Generic index

		// Varie
		boolean areAllIndexLiteralNum = false;                                   // False indica che qualche indice è un campo e non una literal numerica
		boolean areAllIndexDataItem = false;                                     // False indica che qualche indice è un literal numerica
		boolean isTbElementToExtract = false;                                    // True indica che l'elemento identificato dagli indici è da portare in output
        boolean isRedefinesPreviousFound = false;                                // True indica REDEFINES presente in definizioni precedenti 


		if (ilpw.lastSetManaged) {
			return true;
		}

		// Si tratta di un data item receiver da non trattare
		if (identifierToDeal.getIdentifierType()  == EnumCobolReservedWords.PRECOMPILER_CICS
		||  identifierToDeal.getIdentifierType()  == EnumCobolReservedWords.PRECOMPILER_SQL) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Non si tratta di una data item ma di literal o costante figurativa
		if (identifierToDeal.getQualifier().getSymbolType() != EnumSymbolType.COBOL_SYMBOL_DATA_ITEM) {
			return false;
		}

		// Campo Sander non indicizzato, NON è un campo occursato: exit
		if (!identifierToDeal.getQualifier().isOccursed()) {
			return false;
		}

		// Campo sender occursato: estraggo gli indici utilizzati (gestiti come espressioni cobol)
		ar_indexUsed = identifierToDeal.getQualifier().getIndexes();
		areAllIndexLiteralNum = true;
		areAllIndexDataItem = true;
		for (ExpressionCobol expr : ar_indexUsed) {
			// Sono presenti uno o più campi nell'espressione dell'indice
			if (expr.getOperandsFields().length > 0) {
				areAllIndexLiteralNum = false;
			}
			// Sono presenti uno o più campi nell'espressione dell'indice
			if (expr.getOperandsLiteralNumeric().length > 0) {
				areAllIndexDataItem = false;
			}
		}

		// Imposto il nome del campo da cercare, dal receiver
		ilpw.curLastSet = EnumLogicSetMode.NOT_ASSIGNED;

		// Estrazione dimensioni originarie sottocampo in bytes
		sizeBytesSubField = ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getDataItem().getSizeBytes();

		// Se esecuzione logiche spreaded su un pgm chiamante, la lunghezza da considerare non è quella del
		// sottocampo ma quella con cui è stato startato il processo ricorsivo sull'ultima assegnazione.
		// Per esempio a fronte di LAST_SET_BY_CICS_DFHCOMMAREA si deve considerare come lunghezza campo
		// quella indicata in ultima assegnazione e non quella del campo di commarea.
		if (ilpw.isExecLogicSpreaded) {
			sizeBytesSubField = ilpw.lngRcvInSubFieldSpreaded;
		}

		// Numero dimensioni tabella.
		// Coincidono con il numero di indici utilizzati per accederci.
		numDimTable = ar_indexUsed.length;

		// Determino lunghezza elemento tabella
		// Ricerco il numero di definizione del primo elemento, subito dopo la definizione di Occurs
		numInstrDefReceiver = identifierToDeal.getNumInstr();
		ar_groupOwnerPointer = ilpw.programCur.groupOwnerPointers(numInstrDefReceiver);


		//////////////////////////////////////////////////////////////////////////////////////////
		// (1) Occurs di gruppo e receiver corrente indicizzato appartenente al gruppo occursato
		//////////////////////////////////////////////////////////////////////////////////////////

		// Individuazione definizione primo campo elemento tabella e definizione occurs ultima dimensione
		if (identifierToDeal.getDataItem().getOccursNumber() == 0) {

			numInstrDefFirstDimOccurs = 0;
			numInstrDefLastDimOccurs = 0;

			// Individuazione pointer a prima e ultima definizione con clausola occurs tabella (dal + annidato al meno annidato)
			for (int groupOwnerPointer : ar_groupOwnerPointer) {
				dataItemCobol = ilpw.programCur.dataItemDefinition(groupOwnerPointer);
				if (!dataItemCobol.isOccursClause()) {continue;}
				if (numInstrDefLastDimOccurs == 0) {
					numInstrDefLastDimOccurs = groupOwnerPointer;
				}
				numInstrDefFirstDimOccurs = groupOwnerPointer;
			}

			// Individuazione definizione primo e ultimo campo componenti l'elemento tabella
			ar_elementTablePointer = ilpw.programCur.dataItemsUnderGroupPointers(numInstrDefLastDimOccurs, DATA_ITEMS_ELEMENTARY_FIELDS);
			numInstrDefFirstElement = ar_elementTablePointer[0];
			numInstrDefLastElement = ar_elementTablePointer[ar_elementTablePointer.length - 1];

		}

		//////////////////////////////////////////////////////////////////////////////////////////
		// (2) Occurs di campo singolo e receiver indicizzato coincidente con questo
		//////////////////////////////////////////////////////////////////////////////////////////

		if (identifierToDeal.getDataItem().getOccursNumber() > 0) {
			numInstrDefFirstElement = numInstrDefReceiver;
			numInstrDefLastElement = numInstrDefReceiver;
			numInstrDefFirstDimOccurs = numInstrDefReceiver;
			numInstrDefLastDimOccurs = numInstrDefReceiver;
		}


		/////////////////////////////////////////////////////////////////////////////////////////
		// (3) Individuazione valore di ogni dimensione (Occurs n Times) da ogni  Occurs
		/////////////////////////////////////////////////////////////////////////////////////////

		al_dimTable = new ArrayList<Integer> ();
		cntDimTable = 0;
		for (i = numInstrDefFirstDimOccurs; i <= numInstrDefLastDimOccurs; i++) {

			dataItemCobol = ilpw.programCur.dataItemDefinition(i);

			// Definizione Occurs x Times
			if (dataItemCobol.getOccursNumber() > 0) {
				cntDimTable++;
				al_dimTable.add(dataItemCobol.getOccursNumber());
				// Controllo di sicurezza
				if (cntDimTable >= numDimTable) {
					break;
				}
			}
		}

		//////////////////////////////////////////////////////////////////////////
		// (4) Calcolo numero elemento di tabella richiesto, se specifico
		//////////////////////////////////////////////////////////////////////////

		numElementTableRequired = -1;
		if (areAllIndexLiteralNum) {

			// Estrazione valore numerico primo indice, sempre presente
			exprIndex = ar_indexUsed[0];
			ar_indexLiteral = exprIndex.getOperandsLiteralNumeric();
			indexLiteral = ar_indexLiteral[0];
			index1Numeric = StringService._getNumericInt(indexLiteral);

			// Tabella a 1 dimensione; l'elemento coincide con il valore del primo indice
			if (ar_indexUsed.length == 1) {
				numElementTableRequired = index1Numeric;
			}

			// Tabella a 2 dimensioni; l'elemento coincide con il valore dell'indice
			if (ar_indexUsed.length == 2) {
				exprIndex = ar_indexUsed[1];
				ar_indexLiteral = exprIndex.getOperandsLiteralNumeric();
				indexLiteral = ar_indexLiteral[0];
				index2Numeric = StringService._getNumericInt(indexLiteral);
				numElementTableRequired = (index1Numeric - 1) * al_dimTable.get(1) + index2Numeric;
			}
		}


		///////////////////////////////////////////////////////////////////////////////////////////
		// (5) Calcolo lunghezza totale elemento di tabella e posizione campo Receiver in elemento
		///////////////////////////////////////////////////////////////////////////////////////////

		// Vengono sommate le lunghezze in bytes di tutti i campi dell'elemento di tabella.
		sizeElementTable = 0;
		for (i = numInstrDefFirstElement; i <= numInstrDefLastElement; i++) {

			dataItemCobol = ilpw.programCur.dataItemDefinition(i);

			// Posizione 0-based (displacement)
			if (i == numInstrDefReceiver) {
				dspReceiverInElementTable = sizeElementTable;
			}

			// Conteggio lunghezza solo se campo elementare
			if (!dataItemCobol.isGroupField()) {
				sizeElementTable = sizeElementTable + dataItemCobol.getSizeBytes();
			}
		}

		/////////////////////////////////////////////////////////////////////////////
		// (6) Individuazione area di working ridefinita dalla tabella
		/////////////////////////////////////////////////////////////////////////////

		// Verifico se il campo precedente alla definizione della prima dimensione di Occurs è un gruppo e se contiene la clausola Redefines
		// Nel caso caso Redefines Occurs e Pic nello stesso statement prendo la definizione con occurs
		
		// Redefines nello stesso statemen con occurs
		dataItemCobol = identifierToDeal.getDataItem();
		isRedefinesPreviousFound = false;
		if (dataItemCobol.isRedefinesClause()) {			
			isRedefinesPreviousFound = true;
		}
		
		// Redefines da cercare nelle definizioni di gruppo precedenti
		if (!isRedefinesPreviousFound) {
			dataItemCobol = ilpw.programCur.dataItemDefinition(numInstrDefFirstElement - 1);
			for (i = 0; i < ar_groupOwnerPointer.length; i++) {
				dataItemCobol = ilpw.programCur.dataItemDefinition(ar_groupOwnerPointer[i]);
				if (dataItemCobol.isRedefinesClause()) {
					isRedefinesPreviousFound = true;
					break;
				}
			}
		}

		// Non è è stato trovato un gruppo redefines: impossibile assegnare i valori
		if (!isRedefinesPreviousFound) {
			if (isAnalysis) {
				logMessage(EnumMessageType.WARNING, "MW0008", ilpw.programCur.programName);
				logMessagesSetting(ilpw);	
			} else {
				logMessageToString(EnumMessageType.WARNING, "MW0008", ilpw.programCur.programName);
			}
			ilpw.lastSetManaged = false;
			return false;
		}

		///////////////////////////////////////////////////////////////////////////////////
		// (7) Composizione stringa completa valori tabella (dai singoli value)
		///////////////////////////////////////////////////////////////////////////////////

		// Si accodano tutti i valori iniziali 
		redefinesTableDataName = dataItemCobol.getRedefinesDataName();
		ar_NumDefInstr = ilpw.programCur.dataItemPointers(redefinesTableDataName);
		numInstrDefRedefinedTable = ar_NumDefInstr[0];
		ar_NumDefInstr = ilpw.programCur.dataItemsUnderGroupPointers(numInstrDefRedefinedTable, DATA_ITEMS_ELEMENTARY_FIELDS);

		// Valorizzazione stringa con valore completo elementi tabella
		for (int pointerDef : ar_NumDefInstr) {
			dataItemCobol = ilpw.programCur.dataItemDefinition(pointerDef);
			valueTable = valueTable + dataItemCobol.getValueStringFormatted();  // Accodo valore iniziale già formatatto della lunghezza corretta
		}


		///////////////////////////////////////////////////////////////////////////////////
		// (8) Estrazione valori da tabella identificata dal sender
		///////////////////////////////////////////////////////////////////////////////////

		// Calcolo numero totale elementi tabella
		numTotElementTable = al_dimTable.get(0);
		for (i = 1; i < al_dimTable.size(); i++) {
			numTotElementTable = numTotElementTable * al_dimTable.get(i);
		}

		// Estrazione valori sender (riga, colonna, ... tabella).

		// Scan tutti gli elementi di tabella.
		al_valueFromTable = new ArrayList<String> ();
		for (i = 1; i <= numTotElementTable; i++) {
			isTbElementToExtract = false;

			// Indici generici espressi da campi: elemento da estrarre
			if (areAllIndexDataItem) {
				isTbElementToExtract = true;
			}

			// Indici espressi da literal numeriche: verifica se elemento corrente da estrarre
			if (areAllIndexLiteralNum) {
				if (i == numElementTableRequired) {
					isTbElementToExtract = true;
				}
			}

			// Elemento di tabella da trattare: estrazione elemento e valore al suo interno
			if (isTbElementToExtract) {
				// Valore completo elemento di tabella 
				curElementTable = valueTable.substring(dspElementTable, dspElementTable + sizeElementTable);
				curElementTableField = curElementTable.substring(dspReceiverInElementTable, dspReceiverInElementTable + identifierToDeal.getDataItem().getSizeBytes());

				// Posizione in elemento di tabella da cui prelevare il valore.
				// Se non specificato reference modification vale il valore della pos acquisita dal receiver.
				// Altrimenti si prende la posizione esplicita di reference modification.
				posInElementTable = ilpw.posRcv;
				if (ilpw.posSndRefMod > 1) {
					posInElementTable = ilpw.posSndRefMod;
				}
				// Estrazione e intabellamento porzione richiesta receiver occursato rispetto a pos/lng acquisita ricorsivamente
				curValueReceiver = curElementTableField.substring(posInElementTable - 1, posInElementTable + ilpw.lngRcv - 1);
				valueLiteral = StringService._pad(curValueReceiver, ' ', sizeBytesSubField, StringService.PAD_RIGHT);
				al_valueFromTable.add(valueLiteral);							
			}


			// Next element table displacement
			dspElementTable = dspElementTable + sizeElementTable;			
		}

		ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_TBITEM_OF_OCCURS;

		// Valorizzazione preliminare info trasformazione in struttura db, usata anche come servizio
		// Attributi, tra cui identifierSender, saranno aggiornati in seguito
		entityDynamicSubFieldSetting = storeInfoDbSingleSetting(ilpw);

		// Info specifiche ultima trasformazione
		entityDynamicSubFieldSetting.setSetMode(ilpw.curLastSet);

		// Nuovo oggetto con informazioni ultima trasformazione
		ilpw.logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting();
		ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicSubFieldSetting;
		ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;

		// Catena, posizione e lunghezza in sottocampo origine valorizzata dalla trasformazione
		ilpw.logicDynamicFieldSubSettingWrk.numChainSet = ilpw.numChainSet;
		ilpw.logicDynamicFieldSubSettingWrk.posInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getPosInSubField();
		ilpw.logicDynamicFieldSubSettingWrk.lngInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getLngInSubField();

		// Valori estratti, utilizzati successivamente, salvati solo come valori per comodità a livello di ultima assegnazione
		ilpw.logicDynamicFieldSubSettingWrk.al_Value = al_valueFromTable;

		// Inserimento valori in struttura logica dinamica valori sottocampo dinamico 
		for (String value : al_valueFromTable) {
			// Default value potrebbe essere un valore presente in tabella: elimina duplicato
			if (isFieldSubValueFound(ilpw, value)) {
				continue;
			}
			logicDynamicValue = new LogicDynamicValue(program.getSysOwner(), program.getSubSysOwner());		
			logicDynamicValue.value = value;											// Valore campo o sottocampo
			logicDynamicValue.pgmSet = ilpw.programCur.programName;                        // Programma di assegnazione del valore	
			logicDynamicValue.numChainSet = ilpw.numChainSet;                           // Numero catena di assegnazione nel programma di assegnazione
			logicDynamicValue.numInstrSet = ilpw.instrCobolProcSet.getNumInstr();       // Numero istruzione che ha generato il valore nel programma corrente
			
			// Informazioni valore in struttura db
			entityDynamicSubFieldValue = storeInfoDbSingleValue(ilpw);
			entityDynamicSubFieldValue.setPosInSubField(ilpw.logicDynamicFieldSubSettingWrk.posInSubField); // Posizione in sottocampo (1-based) inizio valore di questa assegnazione
			entityDynamicSubFieldValue.setLngInSubField(ilpw.logicDynamicFieldSubSettingWrk.lngInSubField); // Lunghezza in sottocampo valorizzata da questa assegnazione
			entityDynamicSubFieldValue.setValue(value);          							 			 // Valore sottocampo o campo 
			entityDynamicSubFieldValue.setTypeObjectFrom(EnumObject.OBJECT_PGM_COBOL);  				 // Tipo oggetto (pgm, File,  etc) dove il valore è stato impostato
			entityDynamicSubFieldValue.setIdObjectFrom(ilpw.programCur.programName);	  					 // Nome oggetto (pgm) dove il valore è stato impostato
			entityDynamicSubFieldValue.setIdPgmFrom(ilpw.programCur.programName);							 // Pgm di assegnazione corrente
			entityDynamicSubFieldValue.setNumInstrFrom(ilpw.instrCobolProcSet.getNumInstr());			 // Numero istruzione di assegnazione		
			logicDynamicValue.entityDynamicValue = entityDynamicSubFieldValue;          				 // Save in struttura dinamica valore
			
			ilpw.dynamicFieldSubToSolve.al_value.add(logicDynamicValue);                                 // Save valore in struttura sottocampo da risolvere                   
		}		
		
		ilpw.lastSetManaged = true;
		return true;
	}

    // Restituisce true se il valore del sottocampo è già presente nella struttura dinamica
	private boolean isFieldSubValueFound(LogicWorkProcess ilpw, String value) {
		for (LogicDynamicValue logiDynamicValue : ilpw.dynamicFieldSubToSolve.al_value) {
			if (logiDynamicValue.value.equals(value)) {
				return true;
			}
		}
		return false;
	}

	/*
	 * ----------------------------------------------------------------------------
	 * Gestione ultima assegnazione da campo in Ioarea di istruzione Cobol Read.
	 * ----------------------------------------------------------------------------
	 * 
	 * Il metodo è richiamato a fronte di:
	 * 
	 * 1) MOVE fileField TO receiver
	 * 2) receiver non movimentato e in output (nella sua IOAREA) a Read cobol
	 * 
	 * Il parametro identifierToDeal identifica il sender (fileField) o il receiver nella ioarea
	 * 
	 * Sono valorizzati e validi il campo receiver e il numero di istruzione di set.
	 * Il numero di istruzione di Set può essere il numero dell'istruzione Cobol Read.
	 * Si è interessati all'aggiornamento effettuato da una istruzione Cobol Read.
	 * Ciò può avvenire prima dell'istruzione dinamica senza trasformazioni, 
	 * oppure a fronte di una catena di trasformazioni del sottocampo comunque lunga. 
	 *
	 * Il campo di ultima assegnazione è un campo di un record di file letto con Read.
	 * Il nome del file interno è indicato in chiaro nell'istruzione Cobol.
	 * Il nome del file esterno (ddname), viene estratto dalla Select associata.
	 * E' necessario individuare la Ioarea, se non specificata dal parametro Into, e  dedurre la 
	 * posizione del campo (colonna) in questione.
	 * Dopodichè si verifica l'esisteza di valori esterni su DVAE e se non presenti si inserisce
	 * la richiesta sempre su DynamicValueExternal.
	 * I valori richiesti sobo tuttavia su un file fisico, come oggetto OBJECT_PHISICAL_FILE mentre
	 * a progranmma si ha un riferimento a un oggetto EXTERNAL_FILE, ovvero una ddname.
	 * E' il processo di analisi del Jcl che inserisce le relazioni PGM_EXTERNAL_FILE e PGM_PHISICAL_FILE.
	 * 
	 * Pertanto, per inserire la richiesta corretta di dati esterni du DynamicValueExternal è necesario:
	 * 
	 * 1) Estrarre la relazione PGM_PHISICAL_FILE originata nel numero di istruzione in esame
	 * 2) Recuperare l'oggetto PHISICAL_FILE da inserire in tabella valori esterni DynamicValueExternal.
	 * 
	 * Si sfrutta il metodo generalizzato detectValuesSubFieldByReadShared().
	 */
	private boolean lastSetByCobolRead(LogicWorkProcess ilpw, DataItemCobolIdentifier identifierToDeal) throws ExceptionAmrita, SQLException {

		LogicDynamicValue logicDynamicValue = null;                             // Valore individuato
		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;		// Singola trasformazione
		EntityDynamicFieldSubValue entityDynamicSubFieldValue;                  // Singolo valore generato da trasformazione
		ProgramCobolEntry<? extends Instruction> cobolProcEntry = null;			// Entry di procedure division
		ProgramCobolEntry<? extends Instruction> cobolDataEntryIoarea = null;	// Entry di data division per la ioarea istruzione di Read
		ProgramCobolEntry<? extends Instruction> cobolDataEntryReceiver = null;	// Entry di data division per il receiver ultimo assegnato riconducibile alla ioarea
		InstructionCobolProcedure instrCobolProcSet = null;					    // Numero istruzione di set, trasformazione
		InstructionCobolDataItem dataItemIoarea = null;					        // Ioarea output di Read
		ArrayList<String> al_valueFromFile = null;                              // Valori estratti da media esterni
		String externalFileName = "";                                           // File esterno, DDname
		int posIoarea = 0;                                                      // Posizione Ioarea 
		int posInIoarea = 0;                                                    // Posizione receiver in ioarea into (1-based)

		if (ilpw.lastSetManaged) {
			return true;
		}

		cobolProcEntry = ilpw.programCur.entryProcedure(ilpw.numInstrSet);

		// Non è una istruzione Cobol Nativa: return false
		if (cobolProcEntry.getEntryType() != EnumInstrDataCategory.COBOL_PROC_INSTRUCTION) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Recupero istruzione Cobol nativa 
		instrCobolProcSet = (InstructionCobolProcedure) cobolProcEntry.getInstruction();

		// Non è una Read Cobol: return false
		if (instrCobolProcSet.getTypeInstr() != EnumCobolReservedWords.PROC_READ) {
			ilpw.lastSetManaged = false;
			return false;
		}
		
		ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_READ;
		externalFileName = instrCobolProcSet.readGetFileNamesExternal()[0];;
		
		// Recupero valori colonna (o una sua parte), dal file in read
		ilpw.externalTypeObject = EnumObject.OBJECT_EXTERNAL_FILE;
		ilpw.externalObjectName = externalFileName;                                              // DDNAME (File esterno) impostata successivamente
		ilpw.externalTypeSystemField = EnumDataItemSystemEnvironment.NOT_ASSIGNED;
		ilpw.externalIdFieldColumn = "";
		ilpw.externalDsname = "";
		ilpw.externalCicsName = "";

		// Calcolo posizione campo (colonna) nella ioarea file (record) 
		// Recupero valori colonna (o una sua parte)
		dataItemIoarea = instrCobolProcSet.readGetIoareaInto().getDataItem();
		cobolDataEntryIoarea = ilpw.programCur.entryDataDivision(dataItemIoarea.getNumInstr());
		cobolDataEntryReceiver = ilpw.programCur.entryDataDivision(ilpw.identifierReceiver.getDataItem().getNumInstr());

		// Il receiver coincide con la ioarea: la pos/lng della colonna è quella del receiver
		if (cobolDataEntryIoarea == cobolDataEntryReceiver) {			
			ilpw.externalPosColumn = ilpw.posRcv;
			ilpw.externalLngColumn = ilpw.lngRcv;
			ilpw.externalPosInColumn = 1;
			ilpw.externalLngInColumn = ilpw.lngRcv;
			ilpw.externalIdFieldColumn = "";
		} else {
			// Il receiver coincide con un campo della ioarea: la pos/lng della colonna è quella del campo
			posIoarea = cobolDataEntryIoarea.getPos();
			posInIoarea = cobolDataEntryReceiver.getPos() - posIoarea + 1;      // 1-based
			ilpw.externalPosColumn = posInIoarea;
			ilpw.externalLngColumn = ilpw.identifierReceiver.getDataItem().getSizeBytes();
			ilpw.externalPosInColumn = ilpw.posRcv;
			ilpw.externalLngInColumn = ilpw.lngRcv;
			ilpw.externalIdFieldColumn = "";
		}
        
		// Recupero file fisici esterni (Dsbane) in cui cercare i valori
		for (String fileDDNameExternal : instrCobolProcSet.readGetFileNamesExternal()) {
			ilpw.al_externalDDName.add(fileDDNameExternal);
			ilpw.externalObjectName = fileDDNameExternal;
		}
		// Recupero valori da media esterni se disponibili 
		al_valueFromFile = detectValuesFromExternalMedia(ilpw);

		// Nuovo oggetto con informazioni ultima trasformazione
		ilpw.logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting();
		
		// Impostazione flag di waiting for external data in assegnazione e sottocampo
		if (al_valueFromFile.size() == 0) {
			ilpw.dynamicFieldSubToSolve.isWaitingForExternalData = true;
			ilpw.logicDynamicFieldSubSettingWrk.isWaitingForExternalData = true;
			ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = false;
			ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSubWaitExt = ilpw.entityDynamicFieldSubWaitExtWrk;
		} else {
			ilpw.dynamicFieldSubToSolve.isWaitingForExternalData = false;
			ilpw.logicDynamicFieldSubSettingWrk.isWaitingForExternalData = false;
			ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;
		}

		// Valorizzazione preliminare info trasformazione in struttura db, usata anche come servizio
		// Attributi, tra cui identifierSender, saranno aggiornati in seguito
		entityDynamicSubFieldSetting = storeInfoDbSingleSetting(ilpw);

		// Info specifiche ultima trasformazione
		ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_COBOL_READ;
		entityDynamicSubFieldSetting.setSetMode(EnumLogicSetMode.LAST_SET_BY_COBOL_READ);
		entityDynamicSubFieldSetting.setIdObjExt(externalFileName);
		entityDynamicSubFieldSetting.setFieldSenderNum(entityDynamicSubFieldSetting.getFieldReceiverNum());
		entityDynamicSubFieldSetting.setFieldSenderId(entityDynamicSubFieldSetting.getFieldReceiverId());
		entityDynamicSubFieldSetting.setFieldSenderPos(entityDynamicSubFieldSetting.getFieldReceiverPos());										 
		entityDynamicSubFieldSetting.setFieldSenderLng(entityDynamicSubFieldSetting.getFieldReceiverLng());										 
		entityDynamicSubFieldSetting.setFieldReceiverNum(0);
		entityDynamicSubFieldSetting.setFieldReceiverId("");
		entityDynamicSubFieldSetting.setFieldReceiverPos(0);										 
		entityDynamicSubFieldSetting.setFieldReceiverLng(0);										 

		// Catena, posizione e lunghezza in sottocampo origine valorizzata dalla trasformazione
		ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicSubFieldSetting;
		ilpw.logicDynamicFieldSubSettingWrk.numChainSet = ilpw.numChainSet;
		ilpw.logicDynamicFieldSubSettingWrk.posInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getPosInSubField();
		ilpw.logicDynamicFieldSubSettingWrk.lngInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getLngInSubField();

		// Valori estratti, utilizzati successivamente, salvati solo come valori per comodità a livello di ultima assegnazione
		ilpw.logicDynamicFieldSubSettingWrk.al_Value = al_valueFromFile;
		if (al_valueFromFile.size() > 0) {
			ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;
		}
		
		// Inserimento valori in struttura logica dinamica valori sottocampo dinamico 
		for (String value : al_valueFromFile) {
			logicDynamicValue = new LogicDynamicValue(program.getSysOwner(), program.getSubSysOwner());		
			logicDynamicValue.value = value;											// Valore campo o sottocampo
			logicDynamicValue.pgmSet = ilpw.programCur.programName;                        // Programma di assegnazione del valore	
			logicDynamicValue.numChainSet = ilpw.numChainSet;                           // Numero catena di assegnazione nel programma di assegnazione
			logicDynamicValue.numInstrSet = ilpw.instrCobolProcSet.getNumInstr();       // Numero istruzione che ha generato il valore nel programma corrente
			
			// Informazioni valore in struttura db
			entityDynamicSubFieldValue = storeInfoDbSingleValue(ilpw);
			entityDynamicSubFieldValue.setPosInSubField(ilpw.logicDynamicFieldSubSettingWrk.posInSubField); // Posizione in sottocampo (1-based) inizio valore di questa assegnazione
			entityDynamicSubFieldValue.setLngInSubField(ilpw.logicDynamicFieldSubSettingWrk.lngInSubField); // Lunghezza in sottocampo valorizzata da questa assegnazione
			entityDynamicSubFieldValue.setValue(value);          							 			 // Valore sottocampo o campo 
			entityDynamicSubFieldValue.setTypeObjectFrom(EnumObject.OBJECT_ENTITY_VSAM);  				 // Tipo oggetto (pgm, File,  etc) dove il valore è stato impostato
			entityDynamicSubFieldValue.setIdObjectFrom(ilpw.programCur.programName);	  					 // Nome oggetto (pgm) dove il valore è stato impostato
			entityDynamicSubFieldValue.setIdPgmFrom(ilpw.programCur.programName);							 // Pgm di assegnazione corrente
			entityDynamicSubFieldValue.setNumInstrFrom(ilpw.instrCobolProcSet.getNumInstr());			 // Numero istruzione di assegnazione		
			logicDynamicValue.entityDynamicValue = entityDynamicSubFieldValue;          				 // Save in struttura dinamica valore
			
			ilpw.dynamicFieldSubToSolve.al_value.add(logicDynamicValue);                                 // Save valore in struttura sottocampo da risolvere                   
		}		
				
		ilpw.lastSetManaged = true;
		return true;
	}


	/*
	 * ----------------------------------------------------------------------
	 * Gestione ultima assegnazione da campo in Ioarea di istruzione 
	 * Cics Read, ReadPrev, ReadNext, Read Ts, Read Td
	 * ----------------------------------------------------------------------
	 * 
	 * Il metodo è richiamato a fronte di:
	 * 
	 * 1) MOVE vsamField|TsField|TdField TO receiver
	 * 2) receiver non movimentato e in output (nella sua IOAREA) in ReadNext, ReadPrev Cics etc.
	 * 
	 * Il parametro identifierToDeal identifica il sender (fileField) o il receiver nella ioarea	 
	 * Sono valorizzati e validi il campo receiver e il numero di istruzione di set.
	 * Il numero di istruzione di Set può essere il numero dell'istruzione Cics Read.
	 * Si è interessati all'aggiornamento effettuato da una istruzione Cics Read.
	 * Ciò può avvenire prima dell'istruzione dinamica senza trasformazioni, 
	 * oppure a fronte di una catena di trasformazioni del sottocampo comunque lunga. 
	 * 
	 * Vengono trattate le seguenti istruzioni:
	 * 
	 * Exec Cics Read
	 * Exec Cics ReadNext
	 * Exec Cics ReadPrev
	 * Exec Cics Readq Ts
	 * Exec Cics Readq Td
	 * 
	 * In generasle sono trattate le istruzioni di lettura che utilizzano il parametri INTO e QUEUE.
	 * 
	 * Il campo di ultima assegnazione è un campo di ioarea Into di Exec Cics Read ....
	 * E' necessario individuare la Ioarea e dedurre la posizione del campo (colonna) in questione.
	 * 
	 * La Exec Cics ... può a sua volta non essere risolta per quanto riguarda il nome della coda di Ts
	 * o Td o del nome del File. In questo caso il processo si ferma perchè l'istruzione dinamica
	 * originaria NON può essere risolta se prima non si risolve questa istruzione.
	 * Se l'istruzione Exec Cics ... risulta risolta, allora i valori degli operandi sono memorizzati
	 * nell'istruzione stessa, con chiave "$VALUES$FILE" o $VALUES$QUEUE".
	 * Sono pertanto disponibili i nomi degli external file o delle code dove cercare i valori, per il sottocamp
	 * da risolvere nell'istruzione dinamica originaria. Le trasformazione di tale sottocampo hanno 
	 * portato, come ultima trasformazsione, a questa istruzione Exec Cics Read ... 
	 * 
	 * Nel caso di Exec Cics Read ReadNext e ReadPrev, il nome del file Vsam è la ddname del file
	 * esterno del Cics sul quale gira il programma. Non sempre è disponibile questa informazione
	 * nel jcl Cics in quanto può essere completamente dinamica. I valori del sottocampo dell'istruzione
	 * dinamica originaria devono essere cercati nel file fisico (dsname) associato al file esterno (ddname)
	 * esplicitato dal parametro FILE della Exec Cics.
	 * 
	 * Queste informazioni di mapping fra la ddname e il dsname vengono caricate a cura dell'utente nella 
	 * tabella DynamicCicsMapping.
	 * Tale tabella contiene il nome del Cics, il tipo riga (TS, TD, MQ, VSAM), la ddname, il nome della coda
	 * e il dsname fisico associato. Un file o una coda potrebbe essere disponibile al Cics anche in remoto
	 * attraverso software specifico utente di middleware. Per questo motivo il caricamento di questa tabella
	 * è a cura dell'utente.
	 * A fronte del mapping Cics/ddname/dsname è sempre a cura dell'utente il caricamento della tabella con
	 * i valori esterni DynamicValueExternal (DVAE), dove vengono inseriti i valori per il dsname (oggetto
	 * PHISICAL_FILE).
	 * LogicManager caricherà il record con numero progressivo 0 per il file fisico (dsname) se non sono presente
	 * valori esterni oppure estrarrà i valori esterni presenti.
	 *
	 */
	private boolean lastSetByCicsRead(LogicWorkProcess ilpw, DataItemCobolIdentifier identifierToDeal) throws ExceptionAmrita, SQLException {

		LogicDynamicValue logicDynamicValue = null;                             // Valore individuato
		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;		// Singola trasformazione
		EntityDynamicFieldSubValue entityDynamicSubFieldValue;                  // Singolo valore generato da trasformazione
		ProgramCobolEntry<? extends Instruction> cobolProcEntry = null;			// Entry di procedure division
		ProgramCobolEntry<? extends Instruction> cobolDataEntryIoarea = null;	// Entry di data division per la ioarea istruzione di Read
		ProgramCobolEntry<? extends Instruction> cobolDataEntryReceiver = null;	// Entry di data division per il receiver ultimo assegnato riconducibile alla ioarea
		DataItemCobolIdentifier intoIdentifier = null;                          // Identificatore Cobol Ioarea Into
		InstructionCics instructionCics = null;									// Istruzione Cics
		DataItemCobolIdentifier identifierFileQueue = null;                     // Identificatore Cobol completo operando FILE o QUEUE o QNAME
		ArrayList<String> al_FileQueue = null;                                  // Valori parametro FILE/QUEUE (1 solo valore se istruzione statica)
		ArrayList<String> al_valueFromFileQueue = null;                         // Valori estratti da FILE/QUEUE utilizzando il file
		String operandName = "";                                                // Nome operando FILE o QUEUE o QNAME
		String valueFileQueue = "";                                             // Valore literal FILE/QUEUE
		int posIoarea = 0;                                                      // Posizione Ioarea 
		int posInIoarea = 0;                                                    // Posizione receiver in ioarea into (1-based)
		int numInstrDynamic = 0;                                                // Numero istruzione dinamica origine

		if (ilpw.lastSetManaged) {
			return true;
		}

		cobolProcEntry = ilpw.programCur.entryProcedure(ilpw.numInstrSet);

		// Non è una istruzione Cics: return false
		if (cobolProcEntry.getEntryType() != EnumInstrDataCategory.CICS_PRECOMPILER) {
			ilpw.lastSetManaged = false;
			return false;
		}

		numInstrDynamic = ilpw.instrDynamicOrigin.getNumInstr();

		// Istruzione dinamica referenziata è quella di partenza: return false
		// Succede quando il campo chiave dell'istruzione dinamica di partenza è per esempio
		// definito nella IOAREA dell'istruzione stessa.
		// In questo caso è referenziato in output e si manifesta un riferimento circolare.
		if (ilpw.numInstrSet == numInstrDynamic) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Initial e recupero istruzione Cics  
		ilpw.curLastSet = EnumLogicSetMode.NOT_ASSIGNED;
		instructionCics = (InstructionCics) ilpw.programCur.instructionProcedure(ilpw.numInstrSet);

		// E una Exec Cics Read ..
		if (instructionCics.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.CICS_INSTR_READ
		||  instructionCics.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.CICS_INSTR_READNEXT
		||  instructionCics.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.CICS_INSTR_READPREV) {
			ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM;
			ilpw.externalTypeObject = EnumObject.OBJECT_ENTITY_VSAM;
			operandName = "FILE";
			identifierFileQueue = instructionCics.getOperand(operandName);
			
		// E una Exec Cics Readq Ts/Td
		} else if (instructionCics.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.CICS_INSTR_READQ) {
			// Exec Cics Readq Ts
			if (instructionCics.isThereOption("TS")) {
				ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_CICS_READ_TS_QUEUE;
				ilpw.externalTypeObject = EnumObject.OBJECT_CICS_TS_QUEUE;

			} else {
				ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_CICS_READ_TD_QUEUE;
				ilpw.externalTypeObject = EnumObject.OBJECT_CICS_TD_QUEUE;

			}
			operandName = "QUEUE";
			if (instructionCics.isThereOption("QNAME")) {
				operandName = "QNAME";
			}
			identifierFileQueue = instructionCics.getOperand(operandName);
		}

		// Non è una Exec Cics gestita: return
		if (ilpw.curLastSet == EnumLogicSetMode.NOT_ASSIGNED) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Ioarea aggiornata dall'istruzione
		intoIdentifier = instructionCics.getOperand("INTO");

		// Parametro INTO non presente: probabilmente è presente il parametro set (non gestito)
		// Impossibile gestire l'assegnazione
		if (intoIdentifier == null) {
			if (isAnalysis) {
				logMessage(EnumMessageType.WARNING, "ET0016");
			} else {
				logMessageToString(EnumMessageType.WARNING, "ET0016");
			}

			ilpw.lastSetManaged = false;
			return false;
		}

		// Read/Readnext/Readprev/Readq Ts/Readq Td
        // Se istruzione dinamica NON risolta bisogna prima risolvere il parametro FILE o QNAME
		// Per individuare il media esterno da cui prendere i valori

		// Default
		al_FileQueue = new ArrayList<String> ();

		// Istruzione dinamica risolta: estrazione valori parametro File o Queue
		if (instructionCics.isDynamic()
		&&  instructionCics.isDynamicSolvedFull()){
			al_FileQueue = instructionCics.getDynamicOperandValues(operandName);
		} 

		// Istruzione dinamica NON risolta: Necessario attivare il processo ricorsivo sui parametri FILE/QNAME
		if (instructionCics.isDynamic()
		&& !instructionCics.isDynamicSolvedFull()
		&&  ilpw.numInstrSet != numInstrDynamic){			
			// TODO   attivazione ricorsiva pe determinare i valori	
			// al_FileQueue = xxxxx
			ilpw.lastSetManaged = false;
			return false;
		}

		// Istruzione statica: estrazione literal parametro File o Queue
		if (!instructionCics.isDynamic()){
			valueFileQueue = identifierFileQueue.getNameIdentifierFormatted();      // Senza apici iniziali e finali
			al_FileQueue = new ArrayList<String>();
			al_FileQueue.add(valueFileQueue);
			al_FileQueue.trimToSize();
		}

		// Default in caso non fossero disponibili nomi esterni di file/queue in cui cercare i valori
		al_valueFromFileQueue = new ArrayList<String> ();
		al_valueFromFileQueue.trimToSize();

		// Valori per nome file/coda disponibili: si possono cercari i valori dai media esterni con tali nomi
		if (al_FileQueue.size() > 0) {
			// Calcolo posizione campo (colonna) nella ioarea file (record) 
			// Recupero valori colonna (o una sua parte), di File/Queue
			intoIdentifier = instructionCics.getOperand("INTO");
			cobolDataEntryIoarea = ilpw.programCur.entryDataDivision(intoIdentifier.getDataItem().getNumInstr());
			cobolDataEntryReceiver = ilpw.programCur.entryDataDivision(ilpw.identifierReceiver.getDataItem().getNumInstr());

			// Il receiver coincide con la ioarea.
			// E' possibile si sia arrivati qui ricorsivamente analizzando i campi redefines/gruppo del sottocampo origine.
			// Come colonna si considera sempre il campo entro cui cade la posizione richiesta nella ioarea.
			// Potrebbe essere la ioarea stessa o un suo sottocampo
			if (cobolDataEntryIoarea == cobolDataEntryReceiver) {
				ilpw.externalObjectName = valueFileQueue;
				ilpw.externalTypeSystemField = EnumDataItemSystemEnvironment.NOT_ASSIGNED;
				ilpw.al_externalDDName = al_FileQueue;
				ilpw.externalIdFieldColumn = "";
				ilpw.externalCicsName = "";
				// Valori di default 
				ilpw.externalPosColumn = ilpw.posRcv;
				ilpw.externalLngColumn = ilpw.lngRcv;
				ilpw.externalPosInColumn = 1;
				ilpw.externalLngInColumn = ilpw.lngRcv;
				setExternalColumnInfo(ilpw, cobolDataEntryIoarea);   
			} else {
				// Il receiver coincide con un campo della ioarea: la pos/lng della colonna è quella del campo
				posIoarea = cobolDataEntryIoarea.getPos();
				posInIoarea = cobolDataEntryReceiver.getPos() - posIoarea + 1;      // 1-based
				ilpw.externalObjectName = valueFileQueue;
				ilpw.externalTypeSystemField = EnumDataItemSystemEnvironment.NOT_ASSIGNED;
				ilpw.externalIdFieldColumn = "";
				ilpw.externalPosColumn = posInIoarea;
				ilpw.externalLngColumn = ilpw.identifierReceiver.getDataItem().getSizeBytes();
				ilpw.externalPosInColumn = ilpw.posRcv;
				ilpw.externalLngInColumn = ilpw.lngRcv;
				ilpw.al_externalDDName = al_FileQueue;
			}
			
			// Estrazione valori esterni
			al_valueFromFileQueue = detectValuesFromExternalMedia(ilpw);

			// Nuovo oggetto con informazioni ultima trasformazione
			ilpw.logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting();

			// Impostazione flag di waiting for external data in assegnazione e sottocampo
			if (al_valueFromFileQueue.size() == 0) {
				ilpw.dynamicFieldSubToSolve.isWaitingForExternalData = true;
				ilpw.logicDynamicFieldSubSettingWrk.isWaitingForExternalData = true;
				ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = false;
				ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSubWaitExt = ilpw.entityDynamicFieldSubWaitExtWrk;
			} else {
				ilpw.dynamicFieldSubToSolve.isWaitingForExternalData = false;
				ilpw.logicDynamicFieldSubSettingWrk.isWaitingForExternalData = false;
				ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;
			}
		}

		// Valorizzazione preliminare info trasformazione in struttura db, usata anche come servizio
		// Attributi, tra cui identifierSender, saranno aggiornati in seguito
		entityDynamicSubFieldSetting = storeInfoDbSingleSetting(ilpw);

		// Info specifiche ultima trasformazione
		ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM;
		entityDynamicSubFieldSetting.setSetMode(EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM);
		entityDynamicSubFieldSetting = storeInfoDbLastSetting(ilpw, entityDynamicSubFieldSetting);
		entityDynamicSubFieldSetting.setIdObjExt(valueFileQueue);
		entityDynamicSubFieldSetting.setFieldReceiverNum(0);
		entityDynamicSubFieldSetting.setFieldReceiverId("");
		entityDynamicSubFieldSetting.setFieldReceiverPos(0);										 
		entityDynamicSubFieldSetting.setFieldReceiverLng(0);	
		
		ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicSubFieldSetting;		

		// Catena, posizione e lunghezza in sottocampo origine valorizzata dalla trasformazione
		ilpw.logicDynamicFieldSubSettingWrk.numChainSet = ilpw.numChainSet;
		ilpw.logicDynamicFieldSubSettingWrk.posInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getPosInSubField();
		ilpw.logicDynamicFieldSubSettingWrk.lngInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getLngInSubField();

		// Valori estratti, utilizzati successivamente, salvati solo come valori per comodità a livello di ultima assegnazione
		ilpw.logicDynamicFieldSubSettingWrk.al_Value = al_valueFromFileQueue;
        if (al_valueFromFileQueue.size() > 0) {
        	ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;
		}

		// Inserimento valori in struttura logica dinamica valori sottocampo dinamico 
		for (String value : al_valueFromFileQueue) {
			logicDynamicValue = new LogicDynamicValue(program.getSysOwner(), program.getSubSysOwner());		
			logicDynamicValue.value = value;											// Valore campo o sottocampo
			logicDynamicValue.pgmSet = ilpw.programCur.programName;                     // Programma di assegnazione del valore	
			logicDynamicValue.numChainSet = ilpw.numChainSet;                           // Numero catena di assegnazione nel programma di assegnazione
			logicDynamicValue.numInstrSet = instructionCics.getNumInstr();              // Numero istruzione che ha generato il valore nel programma corrente
			
			// Informazioni valore in struttura db
			entityDynamicSubFieldValue = storeInfoDbSingleValue(ilpw);
			entityDynamicSubFieldValue.setPosInSubField(ilpw.logicDynamicFieldSubSettingWrk.posInSubField); // Posizione in sottocampo (1-based) inizio valore di questa assegnazione
			entityDynamicSubFieldValue.setLngInSubField(ilpw.logicDynamicFieldSubSettingWrk.lngInSubField); // Lunghezza in sottocampo valorizzata da questa assegnazione
			entityDynamicSubFieldValue.setValue(value);          							 			 	// Valore sottocampo o campo 
			entityDynamicSubFieldValue.setTypeObjectFrom(EnumObject.OBJECT_ENTITY_VSAM);  				 	// Tipo oggetto (pgm, File,  etc) dove il valore è stato impostato
			entityDynamicSubFieldValue.setIdObjectFrom(ilpw.externalObjectName);	  				 		// Nome oggetto (pgm) dove il valore è stato impostato
			entityDynamicSubFieldValue.setIdPgmFrom(ilpw.programCur.programName);						 	// Pgm di assegnazione corrente
			entityDynamicSubFieldValue.setNumInstrFrom(instructionCics.getNumInstr());			 			// Numero istruzione di assegnazione		
			logicDynamicValue.entityDynamicValue = entityDynamicSubFieldValue;          				 	// Save in struttura dinamica valore
			
			ilpw.dynamicFieldSubToSolve.al_value.add(logicDynamicValue);                                 	// Save valore in struttura sottocampo da risolvere                   
		}	
		
		ilpw.lastSetManaged = true;
		return true;
	}

	/*
	 * -------------------------------------------------------------------------------------------------
	 * Soluzione sottocampo in attesa di dati esterni X Cics Read, ReadPrev, ReadNext, Read Ts, Read Td
	 * -------------------------------------------------------------------------------------------------
	 * 
	 * Il metodo è richiamato a fronte di esecuzione logiche SPREADED da LogicSpreadedPgm:
	 * 
	 * A fronte di istruzioni:
	 * 
	 *  Exec Cics Read
	 *  Exec Cics ReadNext
	 *  Exec Cics ReadPrev
	 *  Exec Cics Readq Ts
	 *  Exec Cics Readq Td
	 * 
	 * In fase di analisi è stata inserita una richiesta di dati esterni, se non trovati contestualmente.
	 * Le strutture di logiche dinamiche dell'istruzione sono le stesse memorizzate in fase di analisi
	 * 
     * In base alla richiesta di dati esterni Vengono cercati i valori in EntityDynamicValueExt.
     * Se trovati si popolano le strutture dinamiche dei valori come se si fosse in fase di analisi.
     * Si aggiornano i flag a livello di ultima assegnazione del sottocampo
     * 
	 *
	 */
	public ArrayList<String> lastSetByCicsReadWaitingForData(LogicWorkProcess ilpw
															,LogicDynamicFieldSubSetting lastSet
															,AnalyzerDbInfo adbi) throws ExceptionAmrita, SQLException {

		LogicDynamicValue logicDynamicValue = null;                             // Valore individuato
		EntityDynamicFieldSubValue entityDynamicSubFieldValue;                  // Singolo valore generato da trasformazione
		ArrayList<String> al_valueFromFileQueue = null;                         // Valori estratti da FILE/QUEUE utilizzando il file

        // Estrazione parametri per individuazione valori
		ilpw.externalTypeObject = lastSet.entityDynamicFieldSetting.getTypeObjExt();           // SQL|VSAM
		ilpw.externalObjectName = lastSet.entityDynamicFieldSetting.getIdObjExt();             // Nome file|Ts|Td
		ilpw.al_externalDDName.add(lastSet.entityDynamicFieldSetting.getIdObjExt());	       // 
		ilpw.externalPosInColumn = 1;	                                                       // I valori in media esterno saranno da posizione 1
		ilpw.externalLngInColumn = lastSet.entityDynamicFieldSetting.getLngInSubField();       // Per la lunghezza della porzione del sottocampo richiesto                                                    // 
		
		// Estrazione valori esterni da db
		al_valueFromFileQueue = detectValuesFromExternalMedia(ilpw);
		
		// Impostazione flag di waiting for external data in assegnazione e sottocampo
		if (al_valueFromFileQueue.size() == 0) {
			ilpw.dynamicFieldSubToSolve.isWaitingForExternalData = true;
			lastSet.isWaitingForExternalData = true;
		} else {
			ilpw.dynamicFieldSubToSolve.isWaitingForExternalData = false;
			lastSet.isWaitingForExternalData = false;
		}		 
        
		// Update flag a livello sottocampo e ultima assegnazione
		insertUpdateFlagsOnDb(ilpw, lastSet, ilpw.dynamicFieldSubToSolve, adbi);
		
		// Inserimento valori in struttura logica dinamica valori sottocampo dinamico 
		for (String value : al_valueFromFileQueue) {
			logicDynamicValue = new LogicDynamicValue(program.getSysOwner(), program.getSubSysOwner());		
			logicDynamicValue.value = value;													// Valore campo o sottocampo
			logicDynamicValue.pgmSet = lastSet.entityDynamicFieldSetting.getIdPgmSet();     	// Programma di assegnazione del valore	
			logicDynamicValue.numInstrSet = lastSet.entityDynamicFieldSetting.getNumInstrSet();	// Numero istruzione che ha generato il valore nel programma corrente
			logicDynamicValue.numChainSet = lastSet.entityDynamicFieldSetting.getNumChain();	// Numero catena di assegnazione nel programma di assegnazione
			logicDynamicValue.isDbUpdated = false;                                          	// Inserito su db a fine elaborazione istruzione
			
			// Informazioni valore in struttura db
			entityDynamicSubFieldValue = storeInfoDbSingleValue(ilpw);
			entityDynamicSubFieldValue.setPosInSubField(lastSet.entityDynamicFieldSetting.getPosInSubField()); 	// Posizione in sottocampo (1-based) inizio valore di questa assegnazione
			entityDynamicSubFieldValue.setLngInSubField(lastSet.entityDynamicFieldSetting.getLngInSubField()); 	// Lunghezza in sottocampo valorizzata da questa assegnazione
			entityDynamicSubFieldValue.setValue(value);          							 			 		// Valore sottocampo o campo 
			entityDynamicSubFieldValue.setTypeObjectFrom(EnumObject.OBJECT_ENTITY_VSAM);  				 		// Tipo oggetto (pgm, File,  etc) da cui il valore proviene
			entityDynamicSubFieldValue.setIdObjectFrom(lastSet.entityDynamicFieldSetting.getIdObjExt());	  	// Nome oggetto da cui il valore proviene
			entityDynamicSubFieldValue.setIdPgmFrom(lastSet.entityDynamicFieldSetting.getIdPgmSet());			// Pgm di assegnazione corrente
			entityDynamicSubFieldValue.setNumInstrFrom(lastSet.entityDynamicFieldSetting.getNumInstrSet());		// Numero istruzione di assegnazione		
			logicDynamicValue.entityDynamicValue = entityDynamicSubFieldValue;          				 		// Save in struttura dinamica valore
			
			ilpw.dynamicFieldSubToSolve.al_value.add(logicDynamicValue);                                 		// Save valore in struttura sottocampo da risolvere                   
		}	
		
		return al_valueFromFileQueue;
	}

	/*
	 * Inserimento statements di update flag in struttura programma origine X aggiornamenti finali
	 */
	private void insertUpdateFlagsOnDb(LogicWorkProcess ilpw
									 , LogicDynamicFieldSubSetting lastSet
									 , LogicDynamicFieldSub dynamicFieldSub
									 , AnalyzerDbInfo adbi
									  ) {
	   	String strSql = "";
    	
	   	// Update DynamicFieldSub
		strSql = "UPDATE DynamicFieldSub" +
				 "    SET solved = "  + ilpw.dynamicFieldSubToSolve.isSolved  +
				 "       ,waitingForData = "  + ilpw.dynamicFieldSubToSolve.isWaitingForExternalData   +				 
				 "  WHERE sys = '"   + ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getSystem() + "'" +
		         "    AND subSys = '" + ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getSubSystem() + "'" +
		         "    AND idObject = '" + ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdObject() + "'" +
		         "    AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal() +
		         "    AND numInstr = " + ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getNumInstr() + 
		         "    AND idField = '" + ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdField() + "'" +
		         "    AND idSubField = '" + ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdSubField() + "'" 
		         ;
		adbi.addSqUpdateStatement(strSql);
		
		// Update DynamicFieldSubSetting
		strSql = "UPDATE DynamicFieldSubSetting" +
				 "    SET waitingForExternalData = "  + lastSet.isWaitingForExternalData  +				 
				 "  WHERE sys = '"   + lastSet.entityDynamicFieldSetting.getSystem() + "'" +
		         "    AND subSys = '" + lastSet.entityDynamicFieldSetting.getSubSystem() + "'" +
		         "    AND idObject = '" + lastSet.entityDynamicFieldSetting.getIdObject() + "'" +
		         "    AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal() +
		         "    AND numInstr = " + lastSet.entityDynamicFieldSetting.getNumInstr() + 
		         "    AND idField = '" + lastSet.entityDynamicFieldSetting.getIdField() + "'" +
		         "    AND idSubField = '" + lastSet.entityDynamicFieldSetting.getIdSubField() + "'" +
				 "    AND idPgmSet = '" + lastSet.entityDynamicFieldSetting.getIdPgmSet() + "'" +
				 "    AND numChain = " + lastSet.entityDynamicFieldSetting.getNumChain() + 
				 "    AND progr = " + lastSet.entityDynamicFieldSetting.getProgr() + 
				 "    AND numInstrSet = " + lastSet.entityDynamicFieldSetting.getNumInstrSet()
		         ;	
		adbi.addSqUpdateStatement(strSql);		                           	   	
	}

	/* -------------------------------------------------
	 * Clonatura area di contesto ricorsiva.
	 * -------------------------------------------------
	 * 
	 * Prima viene clonata direttamente l'area, clonasndo quindi i campi elementari.
	 * Successivamente vengono clonate, all'interno dell'area clonata, le strutture
	 * non elementari
	 * 
	 */
	@SuppressWarnings("unchecked")
	private LogicWorkProcess cloneContextRecursive(LogicWorkProcess ilpw) {
		LogicWorkProcess ilpwCloned;
		ilpwCloned = ilpw.clone();
		ilpwCloned.al_chainSetSubFieldCur = (ArrayList<LogicDynamicFieldSubSetting>) ilpw.al_chainSetSubFieldCur.clone();
		ilpwCloned.al_nextReceiverImplicit = (ArrayList<DataItemCobolIdentifier>) ilpw.al_nextReceiverImplicit.clone();
		ilpwCloned.al_nextReceiverImplicitPos = (ArrayList<Integer>) ilpw.al_nextReceiverImplicitPos.clone();
		ilpwCloned.al_nextReceiverImplicitLng = (ArrayList<Integer>) ilpw.al_nextReceiverImplicitLng.clone();
		ilpwCloned.al_nextSubFieldImplicitPos = (ArrayList<Integer>) ilpw.al_nextSubFieldImplicitPos.clone();
		ilpwCloned.al_nextSubFieldImplicitLng = (ArrayList<Integer>) ilpw.al_nextSubFieldImplicitLng.clone();
		return ilpwCloned;
	}


	/*
	 * ------------------------------------------------------------------
	 * Individuazione valori memorizzati su media esterni
	 * ------------------------------------------------------------------
	 * 
	 * Questo metodo viene richiamato a fronte di ultima assegnazione da 
	 * Cobol read, da Cics read, readnext,.., readq TS/TD, da Sql Select
	 * etc.
	 * 
	 * Sono disponibili i nomi dei files o delle code o delle tabelle in cui 
	 * cercare il valori, oltre alla posizione e lunghezza del campo interessato, 
	 * nel file o nella coda.
	 * E' inoltre disponibile la posizione e la lunghezza, nel campo esterno in
	 * questione, individuato dal processo ricorsivo di trasformaszione.
	 * 
	 * I nomi dei files o delle code in cui cercare i valori sono gli external name,
	 * ddname dichiarati nel jcl, e possono essere multipli.
	 * Nel caso di read cobol del file l'external name (ddname) può essere referenziata
	 * in più di un jcl ognuno mappato sul proprio DSNAME fisico.
	 * Nel caso di Cics Read di files o code Ts, l'external name può essere presente 
	 * in più Cics anche in questo caso ognuno mappato sul proprio DSNAME fisico.
	 * 
	 * Per recuperare i valori esterni del file dalla tabella DynamicFieldSubValue, si fa sempre riferimento 
	 * al DSNAME fisico, recuperato attraverso le relazioni per i file letti con Cobol
	 * Read oppure attraverso il mapping Cics/nome esterno, nella tabella DynamicCicsMapping.
	 * 
	 * Quindi Nella tabella con i valori caricati esternamente DVAE, il file/coda è 
	 * identificato dal suo nome fisico, dal DSNAME='phisical-name'.
	 * 
	 * Riassumendo a fronte del nome logico di file/coda possono esserci + file fisici associati.
	 * Nel caso di read Cobol questi sono individuati attraverso le relazioni con il Jcl.
	 * Nel caso di file/code a fronte di Exec Cics Read..., il nome del file fisico è
	 * specializzato per nome Cics e memorizzato nella tabella EntityDynamicCicsMapping.
	 * 
	 * Le direttive di esecuzione indicano quale/quali Cics prendere in considerazione.
	 * Se nessun Cics viene indicato si assume valido il primo Dsname del primo Cics.
	 * Se nessun Cics è codificato NON è possibile risolvere l'istruzione con nessun file fisico.
	 * Se + Cics venngono indicati, si utilizzano tutti i Dsname presenti, generando valori multipli.
	 * 
	 * Dopo avere individuato il/i phisical name associati al media acceduto, si estrae
	 * la colonna di valori dalla tabella DynamicValueExternal.
	 * Da questa colonna si porta in output la porzione richiesta, da posizione per lunghezza.
	 * In ogni caso si inseriscono gli estremi di richiesta del media esterno, per l'istruzione, in DynamicFieldSubWaitExt.
	 */
	private ArrayList<String> detectValuesFromExternalMedia(LogicWorkProcess ilpw) throws ExceptionAmrita, SQLException {

		ArrayList<String> al_valuesExternal = null;					// Valori da tabella esterna
		ArrayList<String> al_valuesExternalOut = null;			    // Valori da tabella esterna da portare in output da posizione per lunghezza
		ArrayList<String> al_phisicalFile = null;           		// File fisici associati all'operazione di Read 
		EnumDataItemSystemEnvironment setSystemField = null;		// Campo di sistema Cics
		String valueManaged = "";                                   // Valore estratto da corretta posizione/lunghezza
		int numProgLast = 0;                                        // Ultimo progressivo richieste esterne
		
		setSystemField = EnumDataItemSystemEnvironment.NOT_ASSIGNED;
		al_valuesExternal = new ArrayList<String> ();
		al_valuesExternalOut = new ArrayList<String>();

		// Recupero phisical files dipendenti dai Cics in cui i files esterni sono dichiarati
		// Se non esistono Cics in cui mappare il file Vsam/Ts/Td si considera solo il nome esterno (DDname)
		// ai fini del recupero dei valori esterni
		if (ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM
		|| 	ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TS_QUEUE
		|| 	ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TD_QUEUE) {
			ilpw.externalTypeObject = EnumObject.OBJECT_ENTITY_VSAM;
			ilpw.externalTypeSystemField = EnumDataItemSystemEnvironment.NOT_ASSIGNED;
//			ilpw.al_externalCicsName = this.di.al_cicsName;
			al_phisicalFile = getPhisicalFilesFromCicsMapping(ilpw);
			ilpw.al_externalPhisicalFile = al_phisicalFile;
			al_valuesExternal = getValuesExternal(ilpw);
			 
		// Recupero phisical files dipendenti dal jcl in cui i file esterni sono dichiarati
		} else if (ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_COBOL_READ) {
			ilpw.externalTypeObject = EnumObject.OBJECT_ENTITY_VSAM;
			al_phisicalFile = getPhisicalFilesFromRelations(ilpw);
			ilpw.al_externalPhisicalFile = al_phisicalFile;
			al_valuesExternal = getValuesExternal(ilpw);
			
		// Recupero valori esterni per terminale Cics
		} else if (ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRMID) {
			setSystemField = EnumDataItemSystemEnvironment.CICS_EIBTRMID;
			ilpw.externalTypeSystemField = setSystemField;
			ilpw.externalIdFieldColumn = "EIBTRMID";
			al_valuesExternal = getValuesExternal(ilpw);
		
		// Recupero valori esterni per transazione Cics
		} else if (ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRNID) {
			setSystemField = EnumDataItemSystemEnvironment.CICS_EIBTRNID;
			ilpw.externalTypeSystemField = setSystemField;
			ilpw.externalIdFieldColumn = "EIBTRNID";
			al_valuesExternal = getValuesExternal(ilpw);

		// Recupero valori esterni per transazione Cics
		} else if (ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_SQL_SELECT) {
			al_valuesExternal = getValuesExternal(ilpw);
		}

		// Valori individuati: restituisco al chiamante
		if (al_valuesExternal.size() > 0) {
			al_valuesExternalOut = new ArrayList<String>();
			for (String valueToManage : al_valuesExternal) {
				// Valore troppo piccolo: skip
				if ((ilpw.externalPosInColumn + ilpw.externalLngInColumn - 1) > valueToManage.length()) {
					continue;
				}
				valueManaged = valueToManage.substring(ilpw.externalPosInColumn - 1, ilpw.externalPosInColumn + ilpw.externalLngInColumn - 1);
				al_valuesExternalOut.add(valueManaged);
			}
		}

		// Valori individuati da media esterni: fine operazioni
		if (al_valuesExternalOut.size() > 0) {
			return al_valuesExternalOut;
		}
		
		
		// Valori NON individuati su media esterno: inserimento richiesta valori/necessità dati esterni in tabella.
		// Viene memorizzata una riga per numero istruzione e campo e sottocampo.
		// Possono essere presenti + righe con progressivo diverso, per EIBTRMID EIBTRNID su Cics diversi.

		// Recupero ultimo progressivo valori esterni in waiting inserito per l'istruzione
		numProgLast = getLastProgDynamicWaitingExternal(ilpw);       
        
		// Richiesta dati esterni già presente eliminata a inizio analisi programma

		// Inserimento richiesta dati esterni, si suppone NON debba essere inserita
		ilpw.entityDynamicFieldSubWaitExtWrk = null;
		insertRequestExternalData(ilpw, setSystemField, al_phisicalFile, numProgLast);

		return al_valuesExternalOut;
	}


	/*
	 * ----------------------------------------------------------------------
	 * Recupero nome file fisici associati a statement Read Cobol.
	 * ----------------------------------------------------------------------
	 * 
	 * Questo metodo viene chiamato a fronte di ultima impostazione da statement Cobol Read.
	 * 
	 * Lo statement read fa riferimento a un file Internal e lo statemet 
	 * Select internal-file Assigned To external-file fa riferimento al file esterno
	 * ovvero alla ddname nel jcl che utilizza il programma.
	 * Il nome del file interno e di tutti i nomi esterni (ddname) sono memorizzati nella
	 * istruzione stessa.
	 * Il processo di analisi Cobol inserisce una relazione PGM_EXTERNAL_FILE e INTERNAL_FILE_EXTERNAL_FILE.
	 * Il processo di analisi dei Jcl inserisce le relazioni EXTERNAL_FILE_JCL_SOURCE e inoltre
	 * le relazioni PGM_JCL_SOURCE. Per ognuna di queste relazioni viene inserito nella relazione
	 * lo step, la proce e il pgm di exec, nelle informazioni di incrocio.
	 * Quindi un programma può essere presente in tanti jcl, in step diversi che fanno riferimento
	 * alla stessa ddname. Pertanto a un file interno di un programma Cobol si possono associare
	 * multipli phisical-file.
	 * 
	 * Per individuare i phisical-file associati a una generica read Cobol si utilizza la sola relazione
	 * EXTERNAL_FILE_JCL_SOURCE, in quanto nelle informazioni di incrocio è presente sia il phisical file
	 * a cui ci si riferisce, sia il programma e lo step che lo utilizzano.
	 * Viene quindi accodata in where la condizione sul nome programma corrente.
	 * 
	 * 
	 */
	private ArrayList<String> getPhisicalFilesFromRelations(LogicWorkProcess ilpw) throws SQLException, ExceptionAmrita {

		EntityRelationOrigin objRelationOrigin = null;
		List<EntityRelationOrigin> ar_obj = null;
		ArrayList<String> al_phisicalFile = null;			// Files fisici (dsname) in output
		String whereCondition = "";							// Condizione di where

		// Operazioni per accesso al database
		Connection conn = DataBaseConnections.getConnection();
		IDAORelationOrigin eoDAO = (DAOImplRelationOrigin) AmritaStartup.sqlFactory.getDAORelationOrigin(conn, false,false, ucfg);


		// Estrazione diretta origne relazioni con phisical files interessati.
		// E' sufficiente la relazione EXTERNAL_FILE_JCL_SOURCE con tutte le info di incrocio
		//       RELOTYPC = OBJECT-PHISICAL-FILE 
		//       RELOIDOC = phisical file name 
		//       RELOIN1C = step
		//       RELOIN2C = Proc
		//       RELOIN3C = exec pgm

		// Composizione condizione Where 
		whereCondition =                      "      sys  =  '" + ilpw.programCur.getSysOwner() + "'";
		whereCondition = whereCondition +	  " AND  subSys  =  '" + ilpw.programCur.getSubSysOwner() + "'";
		whereCondition = whereCondition +     " AND  relation  =  " + EnumRelation.EXTERNAL_FILE_JCL_JOB.ordinal();
		whereCondition = whereCondition +     " AND  typeObjectA  =  " + EnumObject.OBJECT_EXTERNAL_FILE.ordinal();
		whereCondition = whereCondition +     " AND (idObjectA  =  '" + ilpw.al_externalDDName.get(0) + "'";
		for (int i = 1; i < ilpw.al_externalDDName.size(); i++) {
			whereCondition = whereCondition +            " OR idObjectA = '" + ilpw.al_externalDDName.get(i) + "'" ;
		}
		whereCondition = whereCondition +      "     ) ";
		whereCondition = whereCondition +     " AND  info3Cross   =  '" + ilpw.programCur.programName + "'";

		// Esecuzione query su Relation con condizione fornita
		ar_obj = eoDAO.readSetEntityWhere(whereCondition, "");
		
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		al_phisicalFile = new ArrayList<String> ();

		// Nessun risultato: return array vuoto
		if (ar_obj.size() == 0) {
			return al_phisicalFile;
		}

		// Scan origine relazioni contenenti i phisical file cercati
		for (Object obj : ar_obj) {
			objRelationOrigin = (EntityRelationOrigin) obj;
			al_phisicalFile.add(objRelationOrigin.getIdObjectCross());
		}

		return al_phisicalFile;
	}


	/*
	 * ----------------------------------------------------------------------------------------------
	 * Recupero nomi file fisici (Dsname) associati a operando FILE o QUEUE di Exec Cics Read ...
	 * ----------------------------------------------------------------------------------------------
	 * 
	 * Questo metodo viene chiamato a fronte di ultima impostazione da statement Exec Cics Read ... che
	 * fa riferimento a un external file dichiarato staticamente al Cics o dinamico oppure a una coda di TS o TD.
	 * 
	 * Gli eventuali dsname associati non possono che essere caricati manualmente ed esternamente dall'utente.
	 * Ciò viene effettuato nella tabella DynamicCicsMapping (DCXM), dove ogni riga indica il Cics, la ddname
	 * (external file) e il dsname (phisical file), oltre alla natura della riga (ENTITY_VSAM, ...).
	 * 
	 * Per individuare i phisical-file associati a una generica Exec Cics Read Cics si effettuano gli step:
	 * 
	 * 1) Lettura tabella DynamicCicsMapping, con Where su ddname o queue name
	 * 2) Intabellamento dsname eliminando i duplicati e restituzione al chiamante.
	 * 
	 */
	private ArrayList<String> getPhisicalFilesFromCicsMapping(LogicWorkProcess ilpw) throws ExceptionAmrita, SQLException {

		EntityDynamicCicsMapping objCicsMapping = null;
		List<EntityDynamicCicsMapping> ar_obj = null;
		ArrayList<String> al_phisicalFile = null;			// Files fisici (dsname) in output
		String whereCondition = "";							// Condizione di where
		int i = 0;

		// Operazioni per accesso al database
		Connection conn = DataBaseConnections.getConnection();
		IDAODynamicCicsMapping eoDAO = (DAOImplDynamicCicsMapping) AmritaStartup.sqlFactory.getDAODynamicCicsMapping(conn, false,false, ucfg);

		objCicsMapping = new EntityDynamicCicsMapping();

		// Composizione condizione Where 
		whereCondition =                      "      sys = '" + ilpw.programCur.getSysOwner() + "'";
		whereCondition = whereCondition +	  " AND  subSys = '" + ilpw.programCur.getSubSysOwner() + "'";
		whereCondition = whereCondition +	  " AND  typeObject =  " + ilpw.externalTypeObject.ordinal();
		if (ilpw.al_externalCicsName.size() > 0) {
			whereCondition = whereCondition +     " AND (cicsName = '" + ilpw.al_externalCicsName.get(0) + "'";
			for (i = 1; i < ilpw.al_externalCicsName.size(); i++) {
				whereCondition = whereCondition +       " OR cicsName = '" + ilpw.al_externalCicsName.get(i) + "'" ;
			}
			whereCondition = whereCondition +      "     ) ";
		}
		whereCondition = whereCondition +     " AND (externalName = '" + ilpw.al_externalDDName.get(0) + "'";
		for (i = 1; i < ilpw.al_externalDDName.size(); i++) {
			whereCondition = whereCondition +       " OR externalName = '" + ilpw.al_externalDDName.get(i) + "'" ;
		}
		whereCondition = whereCondition +      "     ) ";


		// Esecuzione query su Relation con condizione fornita
		ar_obj = eoDAO.readSetEntityWhere(whereCondition, "");
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		al_phisicalFile = new ArrayList<String> ();

		// Nessun risultato: return array vuoto
		if (ar_obj.size() == 0) {
			return al_phisicalFile;
		}

		// Scan msapping external name/Cics/phisical file
		for (Object obj : ar_obj) {
			objCicsMapping = (EntityDynamicCicsMapping) obj;
			al_phisicalFile.add(objCicsMapping.getDsname());
		}
		return al_phisicalFile;
	}



	/*
	 * -------------------------------------------------------
	 * Gestione assegnazione da campo a campo via Move
	 * -------------------------------------------------------
	 * 
	 * 
	 * Viene tracciata la catena di trasformazioni effettuata tramite assegnazioni, 
	 * in Cobol Move. Sono utilizzati il sender, il receiver e il numero di istruzione.

	 * Viene semplicemente tracciata la trasformazione del  dato elementare, che viene
	 * inserita nella catena di trasformazioni corrente. Ogni trasformazione sposta
	 * l'attenzione sul Sender, che determina i nuovi Receiver di cui trovare l'assegnazione
	 * finale, ricorsivamente.
	 * 
	 * Oggetto della trasformazione è sempre la modifica diretta o indiretta del sottocampo origine.
	 * La trasformazione può anche essere parziale, ovvero modificare una porzione del sottocampo
	 * che non inizia dalla prima posizione e non termina sull'ultima.
	 * 
	 */	 
	private void storeInfoLogicSettingMove(LogicWorkProcess ilpw) {

		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;		// Singola trasformazione su db

		// Inizializzazione
		ilpw.lastSetManaged = true;
		ilpw.identifierSender = ilpw.instrCobolProcSet.moveGetIdentifierFrom();
		ilpw.identifierReceiver = ilpw.instrCobolProcSet.moveGetIdentifierTo(ilpw.identifierReceiver.getNameIdentifier());

		// Valorizzazione preliminare info trasformazione in struttura db, usata anche come servizio
		// Attributi, tra cui identifierSender, saranno aggiornati in seguito
		entityDynamicSubFieldSetting = storeInfoDbSingleSetting(ilpw);

		// Info specifiche ultima trasformazione
		entityDynamicSubFieldSetting.setSetMode(ilpw.curLastSet);
		entityDynamicSubFieldSetting.setFieldReceiverPos(ilpw.posRcv);
		entityDynamicSubFieldSetting.setFieldReceiverLng(ilpw.lngRcv);
		entityDynamicSubFieldSetting.setFieldSenderPos(ilpw.posSndRefMod);
		entityDynamicSubFieldSetting.setFieldSenderLng(ilpw.lngSndRefMod);
		// Se NON presente reference modification imposto pos/lng del campo
		if (ilpw.posSndRefMod == 0) {
			entityDynamicSubFieldSetting.setFieldSenderPos(ilpw.posSnd);
		}
		if (ilpw.lngSndRefMod == 0) {
			entityDynamicSubFieldSetting.setFieldSenderLng(ilpw.lngSnd);
		}				
		entityDynamicSubFieldSetting.setPosInSubField(ilpw.posInSubField);
		entityDynamicSubFieldSetting.setLngInSubField(ilpw.lngInSubField);

		// Nuovo oggetto con informazioni ultima trasformazione
		ilpw.logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting();
		ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicSubFieldSetting;

		// Impostazione main receiver, receiver e sender per gestione inserimento valore di default
		ilpw.logicDynamicFieldSubSettingWrk.mainReceiver = ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField;
		ilpw.logicDynamicFieldSubSettingWrk.receiver = ilpw.identifierReceiver;
		ilpw.logicDynamicFieldSubSettingWrk.sender = ilpw.identifierSender;

		// Posizione e lunghezza in sottocampo origine valorizzata dalla trasformazione
		ilpw.logicDynamicFieldSubSettingWrk.posInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getPosInSubField();
		ilpw.logicDynamicFieldSubSettingWrk.lngInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getLngInSubField();

		// Trasformazione inserita in ArrayList catena trasformazioni correnti successivamete
		return;
	}

	/* ------------------------------------------------
	 * Restituzione info sulla Move di trasformazione
	 * ------------------------------------------------
	 * 
	 * Restituisce una enumerazione indicante se l'assegnazione è valida o meno.
	 * La validità è data dal fatto che l'assegnazione modifica o meno il receiver
	 * considerando la posizione corrente e l'eventuale presenza di reference modification
	 * in sender e/o receiver.
	 * Imposta anche pos/lng di reference modification per sender e receiver.
	 */
	private EnumLogicSetMode checkSetTransformationMove(LogicWorkProcess ilpw) {

		ExpressionCobol posRefModExpr = null;									// Espressione posizione reference modification
		ExpressionCobol lenRefModExpr = null;									// Espressione lunghezza reference modification
		EnumLogicSetMode setMode = EnumLogicSetMode.NOT_ASSIGNED; 				// Tipologia impostazione
		int posInReceiver = 0;                                                  // Posizione calcolata receiver 
		boolean isRefModProcessable = false;                                    // Devono essere espressioni di un elemento di literal numerica


		////////////////////////////////////////////////////////////////////////////////
		// Receiver: Estrazione posizione e lunghezza di reference modification
		//           Verifica se trasformazione utile
		///////////////////////////////////////////////////////////////////////////////

		// Si suppone receiver espresso senza qualificazione di reference modification (pos:lng)
		// La lunghezza ilpw.lngRcv è stata impostata alla lunghezza massima del sottocampo e
		// nel processo di trasformazione può solo diminuire, a causa di asegnazioni a campi di
		// lunghezza minore oppure di lunghezza esplicita di reference modification
		ilpw.posRcvRefMod = 0;	
		ilpw.lngRcvRefMod = 0;


		// Receiver qualificato da pos e lng di reference modification
		if (ilpw.identifierReceiver.getQualifier().isThereRefModification()) {

			// Espressioni di reference modification per posizione e lunghezza
			posRefModExpr = ilpw.identifierReceiver.getQualifier().getPos();
			lenRefModExpr = ilpw.identifierReceiver.getQualifier().getLength();

			// Verifico validità reference modification
			isRefModProcessable = false;
			if (posRefModExpr.getElements().length <= 1
			&&	lenRefModExpr.getElements().length <= 1) {
				isRefModProcessable = true;
				// Posizione o lunghezza non literal numerica
				if ((posRefModExpr.getElements().length > 0 && posRefModExpr.getElements()[0].getSymbolType() != EnumSymbolType.COBOL_SYMBOL_LITERAL_NUM)
				||  (lenRefModExpr.getElements().length > 0 && lenRefModExpr.getElements()[0].getSymbolType() != EnumSymbolType.COBOL_SYMBOL_LITERAL_NUM)) {
					isRefModProcessable = false;
					setMode = EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_REF_MOD_DISCARDED;
				}
				// Posizione e lunghezza portano oltre il sottocampo origine
				if (ilpw.posRcvRefMod + ilpw.lngRcvRefMod + ilpw.posInSubField - 2 > ilpw.dynamicFieldSubToSolve.dataItemIdentifierSubField.getDataItem().getSizeBytes()) {
					isRefModProcessable = false;
					setMode = EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_REF_MOD_DISCARDED;
				}  
				// Reference modification in receiver espressa con singole literal numeriche
				if (isRefModProcessable) {
					ilpw.posRcvRefMod = posRefModExpr.getElements()[0].getValueNumeric();
					// Caso (:lng): default 1
					if (ilpw.posRcvRefMod == 0) {
						ilpw.posRcvRefMod = 1;
					}
					// Lunghezza ref. mod. non presente significa fino a fine campo
					if (lenRefModExpr.getElements().length == 0) {
						ilpw.lngRcvRefMod = ilpw.identifierReceiver.getDataItem().getSizeBytes() - ilpw.posRcvRefMod;
					} else {
						ilpw.lngRcvRefMod = lenRefModExpr.getElements()[0].getValueNumeric();
					}
				}
			}
		}	


		////////////////////////////////////////////////////////////////////////////////
		// Sender: Estrazione posizione e lunghezza di reference modification per
		//         utilizzo in determinazione next receiver in processo ricorsivo.
		//         Verifica se trasformazione utile
		///////////////////////////////////////////////////////////////////////////////

		// Si suppone sender espresso senza qualificazione di reference modification (pos:lng)
		ilpw.posSndRefMod = 0;	
		ilpw.lngSndRefMod = 0;
		ilpw.posSnd = 1;	

		// Lunghezza sender di default uguale a quella definita
		if (ilpw.identifierSender.getIdentifierType() == EnumCobolReservedWords.DATA_DIV_DATA_ITEM) {
			ilpw.lngSnd = ilpw.identifierSender.getDataItem().getSizeBytes();
		// Literal
		} else if (ilpw.identifierSender.getQualifier().getSymbolType() == EnumSymbolType.COBOL_SYMBOL_LITERAL_ALPHA) {
			ilpw.lngSnd = ilpw.identifierSender.getValueStringFormatted().length();
		// Campo Cics non definito esplicitamente nel programma: forzo lunghezza
		} else if (ilpw.identifierSender.getNameIdentifier().toUpperCase().equals("EIBTRNID")
			   ||  ilpw.identifierSender.getNameIdentifier().toUpperCase().equals("EIBTRMID")) {
			ilpw.lngSnd = 4;
		}

		// Sender qualificato da pos e lng di reference modification
		if (setMode == EnumLogicSetMode.NOT_ASSIGNED
		&&  ilpw.identifierSender.getQualifier().isThereRefModification()) {

			// Espressioni di reference modification per posizione e lunghezza
			posRefModExpr = ilpw.identifierSender.getQualifier().getPos();
			lenRefModExpr = ilpw.identifierSender.getQualifier().getLength();

			// Verifico validità reference modification
			isRefModProcessable = false;
			if (posRefModExpr.getElements().length <= 1
			&&	lenRefModExpr.getElements().length <= 1) {
				isRefModProcessable = true;
				// Posizione o lunghezza non literal numerica ma un campo
				if ((posRefModExpr.getElements().length > 0 && posRefModExpr.getElements()[0].getSymbolType() != EnumSymbolType.COBOL_SYMBOL_LITERAL_NUM)
				||  (lenRefModExpr.getElements().length > 0 && lenRefModExpr.getElements()[0].getSymbolType() != EnumSymbolType.COBOL_SYMBOL_LITERAL_NUM)) {
					isRefModProcessable = false;
					setMode = EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_REF_MOD_DISCARDED;
				}
				// Reference modification in Sender espressa con singole literal numeriche
				if (isRefModProcessable) {
					ilpw.posSndRefMod = posRefModExpr.getElements()[0].getValueNumeric();
					// Caso (:lng): default 1
					if (ilpw.posSndRefMod == 0) {
						ilpw.posSndRefMod = 1;
					}
					// La posizione da considerare è quella forzata da ref mod
					if (ilpw.posSndRefMod > 1) {
						ilpw.posSnd = ilpw.posSndRefMod;
					}
					// Lunghezza ref. mod. non presente significa fino a fine campo
					if (lenRefModExpr.getElements().length == 0) {
						ilpw.lngSndRefMod = ilpw.identifierSender.getDataItem().getSizeBytes() - ilpw.posSndRefMod + 1;
						ilpw.lngSnd = ilpw.lngSndRefMod ;
					} else {
						ilpw.lngSndRefMod = lenRefModExpr.getElements()[0].getValueNumeric();
						ilpw.lngSnd = lenRefModExpr.getElements()[0].getValueNumeric();
					}
				}
			}
		}	


		///////////////////////////////////////////////////////////////////////////////////
		// Assegnazione formalmente valida: verifica se modifica il receiver alla pos utile
		///////////////////////////////////////////////////////////////////////////////////

		if (setMode == EnumLogicSetMode.NOT_ASSIGNED) {
			posInReceiver = ilpw.posRcv;
			if (ilpw.identifierReceiver.getQualifier().isThereRefModification()) {
				posInReceiver = ilpw.posRcvRefMod;
			}

			// Assegnazione SENZA effetto.
			// La posizione è successiva alla fine del campo receiver utile corrente
			if ((posInReceiver) > ilpw.posRcv + ilpw.lngRcv - 1) {
				setMode = EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_BEYOND_DISCARDED;
				ilpw.lastSetManaged = false;
			}

			// Assegnazione implicita a SPACES.
			// La posizione + lunghezza è precedente all'inizio del sottocampo origine
			if (posInReceiver + ilpw.lngRcv - 1 < ilpw.posRcv) {
				setMode = EnumLogicSetMode.LAST_SET_BY_COBOL_MOVE_SPACES_IMPLICIT;
				ilpw.lastSetManaged = true;
			}

			// Assegnazione tipologia trasformazione
			if (setMode == EnumLogicSetMode.NOT_ASSIGNED) {
				ilpw.lastSetManaged = true;
				if (ilpw.identifierReceiver.getDataItem().isGroupField()) {
					setMode = EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_GROUP;
				} else {
					setMode = EnumLogicSetMode.ASSIGN_BY_COBOL_MOVE_FIELD;
				}
			}
		}

		return setMode;
	}

	
	/* -------------------------------------------------------------------------
	 * Gestione ultima assegnazione da campo in Ioarea di istruzione Sql Select
	 * -------------------------------------------------------------------------
	 * 
	 * Il metodo è richiamato a fronte di:
	 * 
	 * 1) MOVE columnName TO receiver
	 * 2) sender non movimentato e in output (nella sua IOAREA) in Select
	 * 
	 * Il sender risulta essere un campo di una istruzione Select.
	 * 
	 */
	private boolean lastSetBySqlSelect(LogicWorkProcess ilpw, DataItemCobolIdentifier identifierToDeal) throws ExceptionAmrita, SQLException {

		LogicDynamicValue logicDynamicValue = null;                             // Valore individuato
		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;		// Singola trasformazione
		EntityDynamicFieldSubValue entityDynamicSubFieldValue;                  // Singolo valore generato da trasformazione
		ProgramCobolEntry<? extends Instruction> cobolProcEntry = null;			// Entry di procedure division
		InstructionSql instructionSql = null;									// Istruzione Sql
		ArrayList<String> al_valueFromColumnSql = null;                         // Valori estratti da colonna Sql
		SqlSelectStatement sqlSelectDescriptor = null;                          // Descrittore generale istruzione SQL
		ArrayList<SqlTableDeclared> sqlEntitiesDeclared = null;                 // Tables declared descriptor
		ArrayList<SqlTableHostVarDeclared> sqlTableHostVarsDeclared  = null;
		String tableName = "";                                                  // Nome tabella contenente la tabella interessata
		String columnName = "";                                                 // Nome colonna associata a receiver
        String hostVarName = "";                                                // Nome host var
		
		if (ilpw.lastSetManaged) {
			return true;
		}

		cobolProcEntry = ilpw.programCur.entryProcedure(ilpw.numInstrSet);

		// Non è una istruzione Cics: return false
		if (cobolProcEntry.getEntryType() != EnumInstrDataCategory.SQL_PRECOMPILER) {
			ilpw.lastSetManaged = false;
			return false;
		}

		// Recupero istruzione Sql  
		instructionSql = (InstructionSql) ilpw.programCur.instructionProcedure(ilpw.numInstrSet);

		// Non è una Exec Sql Select 
		if (instructionSql.getTypeInstrPrecompiler() != EnumPrecompilerReservedWords.SQL_SELECT) {
			ilpw.lastSetManaged = false;
			return false;
		}
	
		ilpw.curLastSet = EnumLogicSetMode.LAST_SET_BY_SQL_SELECT;

		hostVarName = ilpw.identifierReceiver.getNameIdentifier();
		columnName = "";
		
		sqlSelectDescriptor = instructionSql.sqlSelectGetDescriptor();
  		sqlTableHostVarsDeclared  = sqlSelectDescriptor.getHostVarsIntoByEntityFormatted(tableName);
  		
		// Scan host var dichiarate nello statement
	    for (SqlTableHostVarDeclared hostVarDeclared : sqlTableHostVarsDeclared) {
			if (!hostVarDeclared.getHostVarName().equals(hostVarName)) {continue;}
			columnName = hostVarDeclared.getColumnName();
			break;
		}			
		
	    // Nome colonna individuata in base al nome della variabile host	   
	    // Impostazione preliminare del nome tabella alla prima incontrata nella select
		sqlEntitiesDeclared = sqlSelectDescriptor.getEntitiesDeclared();
		tableName = sqlEntitiesDeclared.get(0).getTableName();
		
		// Select con + tabelle in From, impossibile determinare la tabella corretta solo in base al nome colonna
		// Necessario interrogare il db per verificare in quale tabella la colonna sia definita
		// Se il DDL non è definito si lascia la prima tabella
		tableName = getTableNameFromDdl(tableName, columnName);

		// Nuovo oggetto con informazioni ultima trasformazione
		ilpw.logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting();

		// Recupero valori colonna (o una sua parte)
		ilpw.externalTypeObject = EnumObject.OBJECT_ENTITY_SQL;
		ilpw.externalObjectName = tableName;
		ilpw.externalIdFieldColumn = columnName;
		ilpw.externalTypeSystemField = EnumDataItemSystemEnvironment.NOT_ASSIGNED;
		ilpw.externalDsname = "";
		ilpw.externalCicsName = "";
		ilpw.externalPosColumn = 1;
		ilpw.externalLngColumn = ilpw.identifierReceiver.getDataItem().getSizeBytes();
		ilpw.externalPosInColumn = ilpw.posRcv;
		ilpw.externalLngInColumn = ilpw.lngRcv;
		al_valueFromColumnSql = detectValuesFromExternalMedia(ilpw);

		// Impostazione flag di waiting for external data in assegnazione e sottocampo
		if (al_valueFromColumnSql.size() == 0) {
			ilpw.dynamicFieldSubToSolve.isWaitingForExternalData = true;
			ilpw.logicDynamicFieldSubSettingWrk.isWaitingForExternalData = true;
			ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = false;
			ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSubWaitExt = ilpw.entityDynamicFieldSubWaitExtWrk;
		} else {
			ilpw.dynamicFieldSubToSolve.isWaitingForExternalData = false;
			ilpw.logicDynamicFieldSubSettingWrk.isWaitingForExternalData = false;
			ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;		
		}
		
		// Valorizzazione preliminare info trasformazione in struttura db, usata anche come servizio
		// Attributi, tra cui identifierSender, saranno aggiornati in seguito
		entityDynamicSubFieldSetting = storeInfoDbSingleSetting(ilpw);

		// Info specifiche ultima trasformazione
		entityDynamicSubFieldSetting.setSetMode(ilpw.curLastSet);
		entityDynamicSubFieldSetting.setIdObjExt(tableName);
		entityDynamicSubFieldSetting.setTypeObjExt(EnumObject.OBJECT_ENTITY_SQL);
		entityDynamicSubFieldSetting.setObjExtSqlTableColumn(columnName);
		entityDynamicSubFieldSetting.setWaitingForExternalData(ilpw.logicDynamicFieldSubSettingWrk.isWaitingForExternalData);
		entityDynamicSubFieldSetting.setFieldReceiverNum(0);
		entityDynamicSubFieldSetting.setFieldReceiverId("");
		entityDynamicSubFieldSetting.setFieldReceiverPos(0);										 
		entityDynamicSubFieldSetting.setFieldReceiverLng(0);										 
				
		// Catena, posizione e lunghezza in sottocampo origine valorizzata dalla trasformazione
		ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicSubFieldSetting;
		ilpw.logicDynamicFieldSubSettingWrk.numChainSet = ilpw.numChainSet;
		ilpw.logicDynamicFieldSubSettingWrk.posInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getPosInSubField();
		ilpw.logicDynamicFieldSubSettingWrk.lngInSubField = ilpw.logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting.getLngInSubField();
		
		// Valori estratti, utilizzati successivasmente
		ilpw.logicDynamicFieldSubSettingWrk.al_Value = al_valueFromColumnSql;
		if (al_valueFromColumnSql.size() > 0) {
			ilpw.logicDynamicFieldSubSettingWrk.lastSetSolved = true;
		}

		// Inserimento valori in struttura logica dinamica valori sottocampo dinamico 
		for (String value : al_valueFromColumnSql) {
			logicDynamicValue = new LogicDynamicValue(program.getSysOwner(), program.getSubSysOwner());		
			logicDynamicValue.value = value;											// Valore campo o sottocampo
			logicDynamicValue.pgmSet = ilpw.programCur.programName;                     // Programma di assegnazione del valore	
			logicDynamicValue.numChainSet = ilpw.numChainSet;                           // Numero catena di assegnazione nel programma di assegnazione
			logicDynamicValue.numInstrSet = ilpw.numInstrSet;       					// Numero istruzione che ha generato il valore nel programma corrente
			
			// Informazioni valore in struttura db
			entityDynamicSubFieldValue = storeInfoDbSingleValue(ilpw);
			entityDynamicSubFieldValue.setPosInSubField(ilpw.logicDynamicFieldSubSettingWrk.posInSubField); // Posizione in sottocampo (1-based) inizio valore di questa assegnazione
			entityDynamicSubFieldValue.setLngInSubField(ilpw.logicDynamicFieldSubSettingWrk.lngInSubField); // Lunghezza in sottocampo valorizzata da questa assegnazione
			entityDynamicSubFieldValue.setValue(value);          							 			 	// Valore sottocampo o campo 
			entityDynamicSubFieldValue.setTypeObjectFrom(EnumObject.OBJECT_ENTITY_SQL);  				 	// Tipo oggetto Sql da cui il valore proviene
			entityDynamicSubFieldValue.setIdObjectFrom(ilpw.programCur.programName);	  					// Nome oggetto (pgm) dove il valore è stato impostato
			entityDynamicSubFieldValue.setIdPgmFrom(ilpw.programCur.programName);							// Pgm di assegnazione corrente
			entityDynamicSubFieldValue.setNumInstrFrom(ilpw.numInstrSet);			 						// Numero istruzione di assegnazione		
			logicDynamicValue.entityDynamicValue = entityDynamicSubFieldValue;          				 	// Save in struttura dinamica valore
				
			ilpw.dynamicFieldSubToSolve.al_value.add(logicDynamicValue);                                	 // Save valore in struttura sottocampo da risolvere                   
		}			
		
		ilpw.lastSetManaged = true;
		return ilpw.lastSetManaged;
	}

    /* ---------------------------------------------------------------------
     * Restituisce il nome della tabella dove la colonna è definita
     * ---------------------------------------------------------------------
     * 
     * Se non si trova la definizione della colonna si restituisce lo stesso nome  in input
     * Si cerca la tabella in tutti sottosistemi possibili
     */
	private String getTableNameFromDdl(String tableName, String columnName) {
		// TODO Auto-generated method stub
		return tableName;
	}

	/* ----------------------------------------------------------------
	 * Restituisce true se il pointer è definito in un parametro Using
	 * ----------------------------------------------------------------
	 * 
	 * Si verifica se un data item, fornito in input,
	 * è dichiarato in qualche parametro della Procedure Division Using.
	 * Nel caso il data item pointer sia definito in un parametro using
	 * viene restituito true e vengono aggiornati:
	 * 
	 *   ilpw.numUsingParmFound
	 *   ilpw.dspUsingParmFound
	 *    
	 */
	private boolean isDataItemInUsingParm(LogicWorkProcess ilpw, InstructionCobolDataItem dataItemToFind) {

		ProgramCobolEntry<? extends Instruction> cobolEntryProcDiv = null;
		ProgramCobolEntry<? extends Instruction> cobolEntryDataDiv = null;
		InstructionCobolDataItem instrCobolDataItem = null;
		InstructionCobolProcedure instrCobolProcDiv = null;
		ArrayList<DataItemCobolIdentifier> al_identifierParmUsing = null;
		String usingParmName = "";
		int numeDefDataItemToFind = 0;
		int dspInUsingParm = 0;

		// Imposto il nome del campo da cercare, dal receiver
		numeDefDataItemToFind = dataItemToFind.getNumInstr();

		// Recupero entry 0 e istruzione di procedure division Using 
		cobolEntryProcDiv = ilpw.programCur.entryProcedure(0);
		instrCobolProcDiv = (InstructionCobolProcedure) cobolEntryProcDiv.getInstruction();

		// Procedure division senza using: skip
		if (!instrCobolProcDiv.procDivIsUsingParms()) {
			return false;
		}

		// Estrazione parametri dichiarati in Procedure Division Using Parm1, Parm2, ... , ParmN
		al_identifierParmUsing = instrCobolProcDiv.procDivGetUsingParms();

		// Scan parametri Using
		for (int i = 0; i < al_identifierParmUsing.size(); i++) {

			DataItemCobolIdentifier dataItemParmUsing = al_identifierParmUsing.get(i);
			usingParmName = dataItemParmUsing.getNameIdentifier();

			// Parametro using di gruppo e dataItemToFind non definito sotto il gruppo: next using parm
			if (dataItemParmUsing.getDataItem().isGroupField()
			&& !ilpw.programCur.isDataItemUnderGroupNameAnyLevel(numeDefDataItemToFind, usingParmName)
			&& !usingParmName.equals(dataItemToFind)) {
				continue;
			}

			// Parametro using elementare e dataItemToFind non coincidenti
			if (!dataItemParmUsing.getDataItem().isGroupField()
			&&  !usingParmName.equals(dataItemToFind)) {
				continue;
			}

			// Campo sotto il parametro Using definito come gruppo
			// oppure campo con lo stesso nome del gruppo
			// oppure campo non di gruppo con lo stesso nome di un sottocampo.
			// Determino il displacement del campo, nel parametro using
			for (int j = dataItemParmUsing.getNumInstr(); j < ilpw.programCur.entriesData().length; j++) {

				cobolEntryDataDiv = ilpw.programCur.entryDataDivision(j);
				if (!(cobolEntryDataDiv.getInstruction() instanceof InstructionCobolDataItem)) {continue;}
				instrCobolDataItem = (InstructionCobolDataItem) cobolEntryDataDiv.getInstruction();

				// E' il receiver: fine ricerca
				if (instrCobolDataItem.getDataName().equals(dataItemToFind.getDataName())) {
					ilpw.numUsingParmFound = i + 1;
					ilpw.dspInUsingParmFound = dspInUsingParm;
					return true;
				}

				// Update displacement se non è un gruppo
				if (!instrCobolDataItem.isGroupField()) {
					dspInUsingParm = dspInUsingParm + instrCobolDataItem.getSizeBytes();
				}
			} // end-for Using Parm Fields

		} // end-for Using Parm	

		// Campo non presente in parametro di procedure division using

		return false;
	}

	/*
	 * ----------------------------------------------------------------------
	 * Restituisce un identificatore cobol completo dalla definizione dati.
	 * ----------------------------------------------------------------------
	 * 
	 * A tutti gli effetti l'identificatore, contenente il reference all'isttruzione
	 * di definizione dati e al qualificatore di utilizzo, è come se fosse stato
	 * definito das un'istruzione di assegnazione Move.
	 * 
	 */
	private DataItemCobolIdentifier packageNewIdentifier(LogicWorkProcess ilpw, int numDef) {
		DataItemCobolIdentifier implicitReceiverIdentifier = null;
		InstructionCobolDataItem dataItem = null;

		implicitReceiverIdentifier = new DataItemCobolIdentifier();
		implicitReceiverIdentifier.setIdentifierType(EnumCobolReservedWords.DATA_DIV_DATA_ITEM);
		dataItem = ilpw.programCur.dataItemDefinition(numDef);
		implicitReceiverIdentifier.setDataItem(dataItem);
		implicitReceiverIdentifier.setNumInstr(dataItem.getNumInstr());
		implicitReceiverIdentifier.setNameIdentifier(dataItem.getDataName());

		return implicitReceiverIdentifier;
	}



	/*
	 * -------------------------------------------------------------------------
	 * Impostazione iniziali informazioni di trasformazione dato elementare.
	 * -------------------------------------------------------------------------
	 * 
	 * Viene restituito un oggetto EntityDynamicSubFieldSetting
	 * valorizzato con le informazioni derivate dal sottocampo origine
	 * della catena di trasformazioni e con le informazioni della
	 * trasformazione specifica. L'oggetto viene aggiornato successivamente.
	 * 
	 */
	private EntityDynamicFieldSubSetting storeInfoDbSingleSetting( LogicWorkProcess ilpw) {

		EntityDynamicFieldSubSetting entityDynamicSubFieldSetting = null;				 

		// Caricamento info trasformazione in entityDynamicSubFieldSetting
		entityDynamicSubFieldSetting = new EntityDynamicFieldSubSetting();

		// Primary key
		entityDynamicSubFieldSetting.setSystem(ilpw.programCur.getSysOwner());
		entityDynamicSubFieldSetting.setSubSystem(ilpw.programCur.getSubSysOwner());
		entityDynamicSubFieldSetting.setIdObject(ilpw.programCur.programName);
		entityDynamicSubFieldSetting.setTypeObject(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getTypeObject());
		entityDynamicSubFieldSetting.setNumInstr(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getNumInstr());
		entityDynamicSubFieldSetting.setIdField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdField());
		entityDynamicSubFieldSetting.setIdSubField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdSubField());
		// Campo singolo senza sottocampi.
		// Su db il sottocampo viene lasciato a space
		if (entityDynamicSubFieldSetting.getIdField().equals(entityDynamicSubFieldSetting.getIdSubField())) {
			entityDynamicSubFieldSetting.setIdSubField("");
		}
		entityDynamicSubFieldSetting.setIdPgmSet(ilpw.programCur.programName);
		entityDynamicSubFieldSetting.setNumChain(-1);               // Aggiornato a fine elaborazione catena
		entityDynamicSubFieldSetting.setProgr(0);
		entityDynamicSubFieldSetting.setNumInstrSet(ilpw.numInstrSet);

		// Data
		entityDynamicSubFieldSetting.setNumField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getNumField());
		entityDynamicSubFieldSetting.setNumSubField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getNumSubField());
		entityDynamicSubFieldSetting.setNumInstrOriginSpreaded(ilpw.callerOrigin.numInstr);
		// Si tratta del programma origine, il numero istruzione coincide con quello dell'istruzione dinamica
		if (ilpw.callerOrigin.numInstr == 0) {
			entityDynamicSubFieldSetting.setNumInstrOriginSpreaded(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getNumInstr());
		}
		entityDynamicSubFieldSetting.setTypeObjectPgmSet(EnumObject.OBJECT_PGM_COBOL);
		entityDynamicSubFieldSetting.setNumPath(0);										 			// Aggiornare eventualmente successivamente !	
		entityDynamicSubFieldSetting.setSetMode(EnumLogicSetMode.NOT_ASSIGNED); 			        // Aggiornare eventualmente successivamente !
		entityDynamicSubFieldSetting.setNumUsingParm(0);									 		// Aggiornare eventualmente successivamente !
		entityDynamicSubFieldSetting.setPosInSubField(ilpw.posInSubField);                          // Aggiornare eventualmente successivamente !
		entityDynamicSubFieldSetting.setLngInSubField(ilpw.lngInSubField);                          // Aggiornare eventualmente successivamente !

		// Receiver (sempre presente)
		entityDynamicSubFieldSetting.setFieldReceiverNum(ilpw.identifierReceiver.getNumInstr());
		entityDynamicSubFieldSetting.setFieldReceiverId(ilpw.identifierReceiver.getNameIdentifier());
		entityDynamicSubFieldSetting.setFieldReceiverPos(ilpw.posRcv);					
		entityDynamicSubFieldSetting.setFieldReceiverLng(ilpw.lngRcv);											 

		// Sender 
		if (ilpw.identifierSender != null) {
			entityDynamicSubFieldSetting.setFieldSenderNum(ilpw.identifierSender.getNumInstr());
			entityDynamicSubFieldSetting.setFieldSenderId(ilpw.identifierSender.getNameIdentifier());
			entityDynamicSubFieldSetting.setFieldSenderPos(ilpw.posSnd);										 
			entityDynamicSubFieldSetting.setFieldSenderLng(ilpw.lngSnd);										 
		}

		// Info ultima trasformazione (Aggiornare successivamente)
		entityDynamicSubFieldSetting.setIdObjExt("");										           
		entityDynamicSubFieldSetting.setTypeObjExt(EnumObject.NOT_ASSIGNED);							 
		entityDynamicSubFieldSetting.setObjExtSystem(EnumDataItemSystemEnvironment.NOT_ASSIGNED);		 
		entityDynamicSubFieldSetting.setObjExtColField("");											 
		entityDynamicSubFieldSetting.setObjExtLengthCol(0);											 
		entityDynamicSubFieldSetting.setObjExtLengthCol(0);

		// Info stato   
		entityDynamicSubFieldSetting.setSolvedObjExt(false);										     
		entityDynamicSubFieldSetting.setWaitingForExternalData(false);							     

		return entityDynamicSubFieldSetting;
	}

	/*
	 * -------------------------------------------------------------------------
	 * Impostazione iniziali informazioni db valore dato elementare.
	 * -------------------------------------------------------------------------
	 * 
	 * Viene restituito un oggetto EntityDynamicFieldSubValue
	 * valorizzato con le informazioni derivate dall'ultima assegnazione
	 * della catena di trasformazioni.
	 * L'oggetto viene aggiornato successivamente.
	 * 
	 */
	static EntityDynamicFieldSubValue storeInfoDbSingleValue( LogicWorkProcess ilpw) {

		EntityDynamicFieldSubValue entityDynamicSubFieldValue = null;				 

		// Caricamento info trasformazione in entityDynamicFieldSubValue
		entityDynamicSubFieldValue = new EntityDynamicFieldSubValue();

		// Primary key
		entityDynamicSubFieldValue.setSystem(ilpw.programCur.getSysOwner());
		entityDynamicSubFieldValue.setSubSystem(ilpw.programCur.getSubSysOwner());
		entityDynamicSubFieldValue.setIdObject(ilpw.programCur.programName);
		entityDynamicSubFieldValue.setTypeObject(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getTypeObject());
		entityDynamicSubFieldValue.setNumInstr(ilpw.instrDynamicOrigin.getNumInstr());
		entityDynamicSubFieldValue.setIdField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdField());
		entityDynamicSubFieldValue.setIdSubField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdSubField());
		entityDynamicSubFieldValue.setProgr(0);                    					// Aggiornare in fase di update su db
		
		// Data (aggiornati successivamente)
		// Porzione di sottocampo oggetto dell'impostazione 
		entityDynamicSubFieldValue.setPosInSubField(0);   							// Posizione in sottocampo (1-based) inizio valore di questa assegnazione
		entityDynamicSubFieldValue.setLngInSubField(0);   							// Lunghezza in sottocampo valorizzata da questa assegnazione
		entityDynamicSubFieldValue.setValue("");          							// Valore sottocampo o campo 
		
		// Origine valore sottocampo
		entityDynamicSubFieldValue.setTypeObjectFrom(EnumObject.NOT_ASSIGNED);    	// Tipo oggetto (pgm, File,  etc) dove il valore è stato impostato
		entityDynamicSubFieldValue.setIdObjectFrom("");	  							// Nome oggetto (pgm) dove il valore è stato impostato
		entityDynamicSubFieldValue.setIdPgmFrom("");								// Pgm di assegnazione
		entityDynamicSubFieldValue.setNumInstrFrom(0);								// Numero istruzione di assegnazione

		return entityDynamicSubFieldValue;
	}
	

	
	/* --------------------------------------------------------------
	 * Restituisce true se il data item è definito in Cics TWA o CSA
	 * --------------------------------------------------------------
	 * 
	 * In base al paramtero fornito, 'TWA' o 'CSA' si estraggono tutte
	 * le aree mappate su Cics TWA o CSA.
	 * Queste aree si trovano in Linkage section e possono assumere qualsiasi
	 * nome, anche se normalmente si trovano come 01 TWA e 01 CSA.
	 * 
	 * Si recupera il campo di livello 01 dove il data item in input è definito.
	 * Se tale campo coincide con una delle aree mappate si restituisce true,
	 * altrimenti false.
	 * 
	 */
	private boolean isDataItemInCicsTwaCsa(LogicWorkProcess ilpw, String areaTwaCsa, InstructionCobolDataItem dataItem) {

		InstructionCobolDataItem dataItemLvl01 = null;
		ArrayList<InstructionCobolDataItem> al_areaTwaCsa = null;
		int[] ar_numInstrGroupOwner = null;
		int pointerLvl01 = 0;


		// Devo considerare il livello 01 di appartenenza
		dataItemLvl01 = dataItem;
		if (dataItem.getLevelNumber() > 1) {
			ar_numInstrGroupOwner = ilpw.programCur.groupOwnerPointers(dataItem.getNumInstr());
			pointerLvl01 = ar_numInstrGroupOwner[ar_numInstrGroupOwner.length - 1];
			dataItemLvl01 = ilpw.programCur.dataItemDefinition(pointerLvl01);
		}

		// Estrazione aree livello 01 mappate su Cics Twa o Csa
		al_areaTwaCsa = getAreasCicsTwaCsa(ilpw, areaTwaCsa);

		// Scan aree di Twa o Csa
		for (InstructionCobolDataItem dataItemTwaCsa : al_areaTwaCsa) {
			// Il data item era in un'area indirizzata in TWA/CSA
			if (dataItemTwaCsa.getDataName().equals(dataItemLvl01.getDataName())) {
				return true;
			}
		}
		return false;
	}

	/* --------------------------------------------------------------
	 * Restituisce il livello 01 di appartenebza
	 * --------------------------------------------------------------
	 * 
	 * Si recupera il campo di livello 01 dove il data item in input è definito.
	 * Se campo non sotto un gruppo restituisce lo stesso campo di input
	 * 
	 */
	private InstructionCobolDataItem getLvl01Owner(LogicWorkProcess ilpw, InstructionCobolDataItem dataItem) {

		InstructionCobolDataItem dataItemLvl01 = null;
		int[] ar_numInstrGroupOwner = null;
		int pointerLvl01 = 0;
		
		dataItemLvl01 = dataItem;
		
		// Devo considerare il livello 01 di appartenenza
		dataItemLvl01 = dataItem;
		if (dataItem.getLevelNumber() > 1) {
			ar_numInstrGroupOwner = ilpw.programCur.groupOwnerPointers(dataItem.getNumInstr());
			pointerLvl01 = ar_numInstrGroupOwner[ar_numInstrGroupOwner.length - 1];
			dataItemLvl01 = ilpw.programCur.dataItemDefinition(pointerLvl01);
		}
		return dataItemLvl01;
	}

	/* ---------------------------------------------------------------
	 * Impostazione posizione colonna esterna in cui trovare i valori
	 * ---------------------------------------------------------------
	 * 
	 * La ioarea deve essere un campo di gruppo
	 * 
	 */
	private void setExternalColumnInfo(LogicWorkProcess ilpw, ProgramCobolEntry<? extends Instruction> entryIoarea) {

		ProgramCobolEntry<? extends Instruction> entryGeneric = null;
		InstructionCobolDataItem dataItemIoarea = null;
		InstructionCobolDataItem dataItemGeneric = null;
		int lvlIoarea = 0;
		boolean isFieldInsideIoarea = false;

		dataItemIoarea = (InstructionCobolDataItem) entryIoarea.getInstruction();
		lvlIoarea = dataItemIoarea.getLevelNumber();

		// Ioarea campo elementare: va bene il default.
		if (!dataItemIoarea.isGroupField()) {
			return;
		}

		// Scan campi sotto la ioarea
		for (int i = entryIoarea.getInstruction().getNumInstr() + 1; i < ilpw.programCur.entriesData().length; i++) {

			entryGeneric = ilpw.programCur.entryDataDivision(i);

			// Non è una definizione dati: skip
			if (!(entryGeneric.getInstruction() instanceof InstructionCobolDataItem)) {continue;}

			dataItemGeneric = (InstructionCobolDataItem) entryGeneric.getInstruction();

			// Numero livello <= di quello della ioarea, campo receiver non trovato
			if (dataItemGeneric.getLevelNumber() <= lvlIoarea) {
				break;
			}

			// Considero solo i campi elementari, non di gruppo
			if (dataItemGeneric.isGroupField()) {continue;}

			// La posizione del receiver non cade sotto questo campo
			if (ilpw.posRcv > (entryGeneric.getPos() + dataItemGeneric.getSizeBytes() - entryIoarea.getPos() - 1)) {
				continue;
			}

			// Posizione all'interno
			isFieldInsideIoarea = true;
			break;
		}

		// Controllo di sicurezza se Ioarea ultimo campo definito
		if (!isFieldInsideIoarea) {
			return;
		}

		// Valori posizione riferiti al primo campo utile valido nella ioarea 
		ilpw.externalPosColumn = entryGeneric.getPos() - entryIoarea.getPos() + 1;
		ilpw.externalLngColumn = dataItemGeneric.getSizeBytes();
		ilpw.externalPosInColumn = ilpw.posRcv - entryGeneric.getPos();
		ilpw.externalLngInColumn = ilpw.lngRcv;

	}



	/* --------------------------------------------
	 * Inserimento richiesta dati esterni
	 * --------------------------------------------
	 * 
	 * Viene inserita una riga nella tabella DynamicWaitingExternal.
	 * La verifica che la riga non sia già presente è effettuata dal chiamante.
	 * Questa routine è richiamata Se i dati esterni non sono stati trovati nella tabella DynamicFieldSubValue
     *
	 * La tabella DynamicWaitingExternal è utilizzata per conoscere a livello di istruzione dinamica,
	 * a livello di campo e sottocampo, i dati esterni di cui necessita per essere risolta.
	 * I valori da individuare sono sempre riferiti a un sottocampo, ancjhe nel caso di campo
	 * elementare, dove viene inserito un sottocampo di servizio con lo stesso nome del campo.
	 * 
	 * La richiesta di dati esterni fa tipicamente riferimento a programma/istruzione/campo/sottocampo
	 * e nome file esterno (DDname) o campo Cics di sistema (EIBTRMID|EIBTRNID).
	 * 
	 * Ulteriori informazioni utili al recupero dei valori sono il dsname (nome file fisico su disco)
	 * e il nome del Cics da cui i valori dovrebbero arrivare.
	 * Si cerca per quanto di individuare il dsname a cui la ddname afferisce o il Cics da prendere in considerazione.
	 * Se non sitrovano queste informazioni non si valorizzano e se ne vengon trovate multiple, si inserisce la prima.
	 * 
	 * 
	 */
	private void insertRequestExternalData(LogicWorkProcess ilpw
										 , EnumDataItemSystemEnvironment typeSetSystemFieldExternal
										 , ArrayList<String> al_phisicalFile
										 , int numProgLast 
										) throws SQLException, ExceptionAmrita {

		EntityDynamicFieldSubWaitExt edwe = null;				    // Entity per richiesta valori esterni al programma


		// Inserimento richiesta di caricamento dati esterni per terminale/transazione
		if (ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRMID
		||  ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRNID) {

			// Preparazione entity dati esterni per inserimento richiesta/certificazione dati
			edwe = new EntityDynamicFieldSubWaitExt();
			edwe.setSystem(ilpw.programCur.getSysOwner());
			edwe.setSubSystem(ilpw.programCur.getSubSysOwner());
			edwe.setIdObject(ilpw.programOrigin.programName);                     // Nome programma origine
			edwe.setTypeObject(EnumObject.OBJECT_PGM_COBOL);                      // Tipo programma Cobol
			edwe.setNumInstr(ilpw.instrDynamicOrigin.getNumInstr());              // Numero istruzione
			edwe.setIdField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdField());   		// Nome campo dinamico
			edwe.setIdSubField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdSubField());  // Nome campo dinamico
			edwe.setNumProgr(++numProgLast);  									  // Progressivo
			
			edwe.setTypeObjectExternal(EnumObject.OBJECT_CICS_SYSTEM_FIELD);      // Tipologia oggetto generale
			edwe.setIdObjectExternal(ilpw.externalObjectName);  				  // Nome oggetto esterno di cui servono valori esterni all'applicazione
			edwe.setTypeSystemFieldExternal(typeSetSystemFieldExternal);          // Tipologia oggetto specifico
			edwe.setDsnameExternal(ilpw.externalDsname);  				          // Dsname esterno
			edwe.setCicsNameExternal(ilpw.externalCicsName);  					  // Nome Cics TODO implementare con DynamicCicsMapping
			edwe.setIdFieldExternal(ilpw.externalIdFieldColumn);  				  // Nome campo/colonna come definito in CopyEntityDefinition o EIBTRMID/EIBTRNID
			edwe.setPosColumnExternal(1); 								 		  // Posizione colonna in record area/copy/TS/TD (se non definito tracciato)
			edwe.setLengthColumnExternal(4);  									  // Lunghezza colonna in record area/copy/TS/TD (se non definito tracciato)			
			ilpw.entityDynamicFieldSubWaitExtWrk = edwe;                          // Inserita su db a fine elaborazione
			return;
		}  
		
		// Inserimento richiesta di caricamento dati esterni per file fisici di Vsam batch/online e code Ts/Td		
		if (ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM
		|| 	ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TS_QUEUE
		|| 	ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TD_QUEUE
		|| 	ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_COBOL_READ) {
			
			// Preparazione entity dati esterni per inserimento richiesta dati
			edwe = new EntityDynamicFieldSubWaitExt();
			edwe.setSystem(ilpw.programCur.getSysOwner());
			edwe.setSubSystem(ilpw.programCur.getSubSysOwner());
			edwe.setIdObject(ilpw.programOrigin.programName);                      	 	// Nome programma origine
			edwe.setTypeObject(EnumObject.OBJECT_PGM_COBOL);                        	// Tipo programma Cobol
			edwe.setNumInstr(ilpw.instrDynamicOrigin.getNumInstr());                      	// Numero istruzione
			edwe.setIdField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdField());   		// Nome campo dinamico
			edwe.setIdSubField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdSubField());  // Nome campo dinamico
			edwe.setNumProgr(++numProgLast);  										    // Progressivo
			
			edwe.setIdObjectExternal(ilpw.externalObjectName);  				        // Nome oggetto esterno (DDname) di cui trovare valori esterni all'applicazione
			edwe.setTypeObjectExternal(ilpw.externalTypeObject);  		        		// Tipologia oggetto generale
			edwe.setTypeSystemFieldExternal(EnumDataItemSystemEnvironment.NOT_ASSIGNED);// Tipologia oggetto specifico
			edwe.setCicsNameExternal(ilpw.externalCicsName);  					        // Nome Cics TODO implementare con DynamicCicsMapping
			edwe.setIdFieldExternal(ilpw.externalIdFieldColumn);  				        // Nome campo/colonna come definito in CopyEntityDefinition o EIBTRMID/EIBTRNID
			edwe.setPosColumnExternal(ilpw.externalPosColumn); 							// Posizione colonna in record area/copy/TS/TD (se non definito tracciato)
			edwe.setLengthColumnExternal(ilpw.externalLngColumn);  						// Lunghezza colonna in record area/copy/TS/TD (se non definito tracciato)
			// Set nome cics se disponibile
			if (ilpw.curLastSet != EnumLogicSetMode.LAST_SET_BY_COBOL_READ && ilpw.al_externalCicsName.size() > 0) {
				edwe.setCicsNameExternal(ilpw.al_externalCicsName.get(0));  								 
			}
			// Set Dsname se disponibile
			if (al_phisicalFile.size() > 0) {
				edwe.setDsnameExternal(al_phisicalFile.get(0));  								 
			}
			ilpw.entityDynamicFieldSubWaitExtWrk = edwe;                              // Inserita su db a fine elaborazione
			return;
		}

		// Inserimento richiesta di caricamento dati esterni per file fisici di Vsam batch/online e code Ts/Td		
		if (ilpw.curLastSet == EnumLogicSetMode.LAST_SET_BY_SQL_SELECT) {
			
			// Preparazione entity dati esterni per inserimento richiesta dati
			edwe = new EntityDynamicFieldSubWaitExt();
			edwe.setSystem(ilpw.programCur.getSysOwner());
			edwe.setSubSystem(ilpw.programCur.getSubSysOwner());
			edwe.setIdObject(ilpw.programOrigin.programName);                      	 	// Nome programma origine
			edwe.setTypeObject(EnumObject.OBJECT_PGM_COBOL);                        	// Tipo programma Cobol
			edwe.setNumInstr(ilpw.instrDynamicOrigin.getNumInstr());                    // Numero istruzione
			edwe.setIdField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdField());   		// Nome campo dinamico
			edwe.setIdSubField(ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdSubField());  // Nome campo dinamico
			edwe.setNumProgr(++numProgLast);  										    // Progressivo
			
			edwe.setIdObjectExternal(ilpw.externalObjectName);  				        // Nome oggetto esterno (Tabella Sql) da cui trovare valori esterni all'applicazione
			edwe.setTypeObjectExternal(ilpw.externalTypeObject);  		        		// Tipologia oggetto generale
			edwe.setTypeSystemFieldExternal(EnumDataItemSystemEnvironment.NOT_ASSIGNED);// Tipologia oggetto specifico
			edwe.setCicsNameExternal(ilpw.externalCicsName);  					        // Nome Cics TODO implementare con DynamicCicsMapping
			edwe.setIdFieldExternal(ilpw.externalIdFieldColumn);  						// Nome colonna tabella Sql
			edwe.setPosColumnExternal(ilpw.externalPosColumn); 							// Posizione colonna in record area/copy/TS/TD (se non definito tracciato)
			edwe.setLengthColumnExternal(ilpw.externalLngColumn);  						// Lunghezza colonna in record area/copy/TS/TD (se non definito tracciato)
			ilpw.entityDynamicFieldSubWaitExtWrk = edwe;                                // Inserita su db a fine elaborazione
			return;
		}	
		
		
		
		
	}

	/* -----------------------------------------------------------
	 * Restituisce tutte le aree mappate sulla TWA o CSA del Cics
	 * -----------------------------------------------------------
	 * 
	 * In base al paramtero fornito, 'TWA' o 'CSA' si individuano
	 * tutte le aree cobol mappate sulla corrispondente area Cics.
	 * Queste aree si trovano in Linkage section e possono assumere qualsiasi
	 * nome, anche se normalmente si trovano come 01 TWA e 01 CSA.
	 * Tali arre possono essere ridefinite e pertamto vengono portate
	 * in output anche tutte le ridefinizioni delle stesse.
	 * 
	 * 1) Si cercano le EXEC CICS ADDRESS TWA(pointer)
	 * 2) Si cercano le EXEC CICS ADDRESS SET(Address of Area) USING pointer
	 * 3) Si cercano le SET ADDRESS OF Area TO pointer
	 * 4) Si intabellano le aree (campi) individuati da 2) e 3)
	 * 
	 * Ottimizzazione
	 * 
	 * Si utilizzano nell'area di controllo tre SET di numeri istruzione
	 *        
	 * 1) set_setCicsAddressTwaCsa
	 * 2) set_setCicsAddressSet
	 * 3) set_setCobolAddressOf
	 * 
	 */
	private ArrayList<InstructionCobolDataItem> getAreasCicsTwaCsa(LogicWorkProcess ilpw, String areaTwaCsa) {

		InstructionCobolProcedure instrCobol = null;
		InstructionCics instrPrecompilerAddress = null;
		InstructionCics instrPrecompilerSet = null;
		DataItemCobolIdentifier identifierUsing = null;;
		DataItemCobolIdentifier identifierArea = null;;
		InstructionCobolDataItem dataItemTwaCsaRed = null;
		ArrayList<InstructionCobolDataItem> al_areaMappedOnTwaCsa = null;
		ArrayList<InstructionCobolDataItem> al_areaMappedOnTwaCsaRed = null;
		int[] ar_numDefTwaCsaRed = null;
		int numDefTwaCsa = 0;
		int numDefTwaCsaRed = 0;

		al_areaMappedOnTwaCsa = new ArrayList<InstructionCobolDataItem> ();


		// Scan istruzioni EXEC CICS ADDRESS TWA|CSA presenti nel programma
		// Intabellamento di tutte le possibili aree indirizzate in TWA/TSA
		for (int numInstrAddress : ilpw.set_setCicsAddressTwaCsa) {

			instrPrecompilerAddress = (InstructionCics) ilpw.programCur.entryProcedure(numInstrAddress).getInstruction();

			// Considero la TWA o la CSA come da parametro in input
			if (!instrPrecompilerAddress.isThereOperand(areaTwaCsa)) {continue;}

			// Scan EXEC CICS ADDRESS SET(Address Of Area) USING(pointer)
			for (int i : ilpw.set_setCicsAddressSet) {
				instrPrecompilerSet = (InstructionCics) ilpw.programCur.entryProcedure(i).getInstruction();
				identifierUsing = instrPrecompilerSet.getOperand("USING");
				identifierArea = instrPrecompilerSet.getOperand("SET");

				// Area indirizzata in TWA/CSA
				if (identifierUsing.getNumInstr() == instrPrecompilerAddress.getOperand(areaTwaCsa).getNumInstr()) {
					al_areaMappedOnTwaCsa.add(identifierArea.getDataItem());
				}
			} // end-for EXEC CICS ADDRESS SET(Address Of Area) USING(pointer)

			// Scan SET ADDRESS OF Area TO Pointer
			for (int i : ilpw.set_setCobolAddressOf) {

				instrCobol =  (InstructionCobolProcedure) ilpw.programCur.entryProcedure(i).getInstruction();
				identifierUsing = instrCobol.setGetPointer();
				identifierArea = instrCobol.setGetAreaAddressed();

				// Area indirizzata in TWA/CSA
				if (identifierUsing.getNumInstr() == instrPrecompilerAddress.getNumInstr()) {
					al_areaMappedOnTwaCsa.add(identifierArea.getDataItem());
				}
			} // end-for SET ADDRESS OF Area TO Pointer

		} // end-for EXEC CICS ADDRESS TWA|CSA 


		// Scan aree di Twa/Csa individuate e accodamento in output redefines
		al_areaMappedOnTwaCsaRed = new ArrayList<InstructionCobolDataItem> ();
		for (InstructionCobolDataItem dataItemTwaCsa : al_areaMappedOnTwaCsa) {
			numDefTwaCsa = dataItemTwaCsa.getNumInstr();
			// Estraggo eventuali redefines di area individuata direttamente 
			ar_numDefTwaCsaRed = ilpw.programCur.dataItemsRedefinePointers(numDefTwaCsa);
			if (ar_numDefTwaCsaRed != null) {
				// Scan redefines
				for (int i : ar_numDefTwaCsaRed) {
					numDefTwaCsaRed = i;
					dataItemTwaCsaRed = ilpw.programCur.dataItemDefinition(numDefTwaCsaRed);
					al_areaMappedOnTwaCsaRed.add(dataItemTwaCsaRed);
				}
			}
		}
		al_areaMappedOnTwaCsa.addAll(al_areaMappedOnTwaCsaRed);

		return al_areaMappedOnTwaCsa;
	}



	/* ------------------------------------------------------------------------------
	 * Recupero ultimo progressivo inserito per istruzione e dati esterni in waiting 
	 * ------------------------------------------------------------------------------
	 * 
	 * Se non sono presenti righe restituisce -1.
	 * 
	 */
	private int getLastProgDynamicWaitingExternal(LogicWorkProcess ilpw) throws SQLException, ExceptionAmrita {

		EntityDynamicFieldSubWaitExt objDynamicWaitingExternal = null;
		List<EntityDynamicFieldSubWaitExt> ar_obj = null;
		String whereCondition = "";							// Condizione di where
		int numProgr = 0;									//

		// Operazioni per accesso al database
		Connection conn = DataBaseConnections.getConnection();
		IDAODynamicFieldSubWaitExt eoDAO = (DAOImplDynamicFieldSubWaitExt) AmritaStartup.sqlFactory.getDAODynamicFieldSubWaitExt(conn, false,false, ucfg);
	
		// Composizione condizione Where 
		whereCondition =                      "      sys  =  '" + ilpw.programCur.getSysOwner() + "'";
		whereCondition = whereCondition +	  " AND  subSys  =  '" + ilpw.programCur.getSubSysOwner() + "'";
		whereCondition = whereCondition +     " AND  idObject  =  '" + ilpw.programOrigin.programName + "'";
		whereCondition = whereCondition +     " AND  typeObject  =   " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		whereCondition = whereCondition +     " AND  numInstr  =   " + ilpw.instrDynamicOrigin.getNumInstr();
		whereCondition = whereCondition +     " AND  idField  =  '" + ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdField() + "'";
		whereCondition = whereCondition +     " AND  idSubField  =  '" + ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdSubField() + "'";

		// Esecuzione query su DWTE con condizione fornita
		objDynamicWaitingExternal = new EntityDynamicFieldSubWaitExt();
		ar_obj = eoDAO.readSetEntityWhere(whereCondition, " ORDER BY numProgr");
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		// Nessun risultato: return 0
		if (ar_obj.size() == 0) {
			return -1;
		}

		// Ultima risorsa esterna inserita
		int i = ar_obj.size() - 1;
		objDynamicWaitingExternal =  ar_obj.get(i);
		numProgr = objDynamicWaitingExternal.getNumProgr();

		return numProgr;
	}

	/*
	 * -------------------------------------------------------------------------------
	 * Lettura valori esterni per il sottocampo e restituzione ArrayList con valori.
	 * -------------------------------------------------------------------------------
	 * 
	 * Vengono letti i valori della tabella DynamicValueExternal  con chiave completa
	 * Se non viene restituita nessuna riga significa che la richiesta di dati esterni
	 * non è stata evasa.
	 * 
	 * Se ci sono più righe con  il campo value viene intabellato e restituito al chiamante.
	 * Un singolo valore rappresenta il contenuto di una colonna di un file, il nome di un
	 * terminale etc.
	 * 
	 */
	private ArrayList<String> getValuesExternal(LogicWorkProcess ilpw) throws ExceptionAmrita, SQLException  {

		List<EntityDynamicValueExt>  ar_obj = null;												// Righe estratte
		EntityDynamicValueExt objValueExternal = null;                 // Riga estratta
		ArrayList<String> al_valuesExternal = null;							// Valori da tabella esterna
		String whereCondition = "";											// Condizione di Where su tabellla

		objValueExternal = new EntityDynamicValueExt();
		al_valuesExternal = new ArrayList<String>();

		// Operazioni per accesso al database
		Connection conn = DataBaseConnections.getConnection();
		IDAODynamicValueExt eoDAO = (DAOImplDynamicValueExt) AmritaStartup.sqlFactory.getDAODynamicValueExt(conn, false,false, ucfg);

		// Composizione Where per lettura EntityDynamicValueExternal
		whereCondition =                      "     T1.sys = '" + ilpw.programCur.getSysOwner() + "'";
		whereCondition = whereCondition +	  " AND T1.subSys = '" + ilpw.programCur.getSubSysOwner() + "'";
		whereCondition = whereCondition +     " AND T1.typeObjectExternal = "  + ilpw.externalTypeObject.ordinal();    // Table/Vsam/Ts/Td/System Cics Field
		whereCondition = whereCondition +     " AND T1.idObjectExternal = '" + ilpw.externalObjectName + "'";          // EIBTRMID|EIBTRNID|Column|Vsam File Name     
		if (!ilpw.externalIdFieldColumn.isBlank()) {
			whereCondition = whereCondition +     " AND T1.idFieldColumnExternal = '" + ilpw.externalIdFieldColumn + "'";  // EIBTRMID|EIBTRNID|Column
		}
		whereCondition = whereCondition +     " AND T1.typeSystemFieldExternal = "  + ilpw.externalTypeSystemField.ordinal();		
		if (ilpw.al_externalPhisicalFile.size() > 0) {
			whereCondition = whereCondition +     " AND (T1.dsnameExternal = '" + ilpw.al_externalPhisicalFile.get(0) + "'";
			for (int i = 1; i < ilpw.al_externalPhisicalFile.size(); i++) {
				whereCondition = whereCondition +       " OR T1.dsnameExternal = '" + ilpw.al_externalPhisicalFile.get(i) + "'" ;
			}
			whereCondition = whereCondition +      "     ) ";
		}

		ar_obj = eoDAO.readSetEntityWhere(whereCondition, "");
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		// Nessun valore individuato  
		if (ar_obj.size() == 0) {
			return al_valuesExternal;
		}

		// Estrazione valori
		for (Object obj : ar_obj) {
			objValueExternal = (EntityDynamicValueExt) obj;
			al_valuesExternal.add(objValueExternal.getValue());
		}

		return al_valuesExternal;
	}


	/*
	 * Logging informazioni sull'ultima impostazione sottocampo effettuata o meno.
	 * 
	 * Sono utilizzate le informazioni di ultima assegnazione in ilpw
	 * 
	 * Vengono loggate le informazioni di origine: programma, campo e sottocampo
	 * Vengono loggate le informazioni correnti: programma, sender, receiver e tipo assegnazione
	 * Viene loggata l'istruzione corrente di assegnazione completa
	 *     
	 */
	private void logMessagesSetting(LogicWorkProcess ilpw) {

		Instruction instr = null;
		int numInstr = 0;
		String strNumInstr = "";
		String fieldSender = "";
		String fieldReceiver = "";
		String typeSet = "";
		String txtPosInReceiver = "";
		String txtNumChainCur = "";
		String sourceInstr = "";

		// Informazioni su istruzione origine, programma e campo origine
		logMessageDynamicInstrOrigin(ilpw);

		// Informazioni su istruzione di assegnazione, campi sender e receiver corrente
		numInstr = ilpw.numInstrSet;
		strNumInstr = numInstr + "";
		instr = (Instruction) ilpw.programCur.instructionProcedure(numInstr);
		fieldSender = "*";
		if (ilpw.identifierSender != null) {fieldSender = ilpw.identifierSender.getDataItem().getName();}
		fieldReceiver = ilpw.identifierReceiver.getDataItem().getName();
		txtPosInReceiver = ilpw.posRcv + "";
		typeSet = ilpw.curLastSet.toString();
		txtNumChainCur = ilpw.dynamicFieldSubToSolve.numChainCur + "";
		if (isAnalysis) {
			logMessage(EnumMessageType.INFORMATION, "MI0037", ilpw.programCur.programName, strNumInstr, fieldSender, fieldReceiver, txtPosInReceiver, typeSet, txtNumChainCur);
		} else {
			logMessageToString(EnumMessageType.INFORMATION, "MI0037", ilpw.programCur.programName, strNumInstr, fieldSender, fieldReceiver, txtPosInReceiver, typeSet, txtNumChainCur);
		}

		// Informazioni su istruzione sorgente corrente completa di assegnazione
		sourceInstr = instr.getSourceInstr();
		if (isAnalysis) {
			logMessage(EnumMessageType.INFORMATION, "MI0038", ilpw.programCur.programName, strNumInstr, sourceInstr);
		} else {
			logMessageToString(EnumMessageType.INFORMATION, "MI0038", ilpw.programCur.programName, strNumInstr, sourceInstr);

		}
	}

	/*
	 * Logging informazioni sull'istruzione dinamica origine
	 * 
	 * Sono utilizzate le informazioni di ultima assegnazione in ilpw
	 * 
	 */
	private void logMessageDynamicInstrOrigin(LogicWorkProcess ilpw) {

		Instruction instr = null;
		int numInstr = 0;
		String strNumInstr = "";
		String txtInstr = "";
		String field = "";
		String subField = "";
		String txtLngSubField = "";
		int numDefSubField = 0;
		int lngSubField = 0;

		// Informazioni su istruzione origine, programma e campo origine
		numInstr = ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getNumInstr();
		strNumInstr = numInstr + "";
		instr = (Instruction) ilpw.programOrigin.instructionProcedure(numInstr);
		txtInstr = instr.getName();
		field = ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdField();
		subField = ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getIdSubField();
		numDefSubField = ilpw.dynamicFieldSubToSolve.entityDynamicFieldSub.getNumField();
		lngSubField = ilpw.programOrigin.instructionDataItem(numDefSubField).getSizeBytes();
		txtLngSubField = lngSubField + "";
		if (isAnalysis) {
			logMessage(EnumMessageType.INFORMATION, "MI0036", ilpw.programOrigin.programName, strNumInstr, txtInstr, field, subField, txtLngSubField);
		} else {
			logMessageToString(EnumMessageType.INFORMATION, "MI0036", ilpw.programOrigin.programName, strNumInstr, txtInstr, field, subField, txtLngSubField);
		}

	}



	/**
	 * @return the logicInfoDynamic
	 */
	public LogicInfoDynamic getLogicInfoDynamic() {
		return logicInfoDynamic;
	}

	/**
	 * @param logicInfoDynamic the logicInfoDynamic to set
	 */
	public void setLogicInfoDynamic(LogicInfoDynamic logicInfoDynamic) {
		this.logicInfoDynamic = logicInfoDynamic;
	}


	/* --------------------------------------------------
	 * inizializzazioni generali processo individuazione
	 * valori dinamici con creazione oggetto di lavoro
	 * --------------------------------------------------
	 * 
	 * 1) Creazione oggetto di lavoro
	 * 2) Caricamento Set con Exec CIcs Address TWA/CSA
	 * 3) Caricamento Set con Cobol Set
	 * 
	 * 
	 */
	static LogicWorkProcess dynamicValuesInitial(ProgramCobol programOrigin, Instruction instr, DataItemCobolIdentifier dataItemToSolve) {
		LogicWorkProcess ilpw = null;									// Informazioni e reference per il completo controllo del processo normale e ricorsivo
		
		// Attività iniziali di inizializzazione
		ilpw = new LogicWorkProcess();                    				// Allocazione oggetto di lavoro (allocazioni varie nel costruttore) 
		ilpw.isExecLogicSpreaded = false;                       		// Esecuzione logiche stesso programma origine, NOT spreaded
		ilpw.programOrigin = programOrigin;								// Programma radice cone istruzione dinamica da risolvere
		ilpw.programCur = programOrigin;								// Programma corrente in processo soluzione istruzioni
		ilpw.instrDynamicOrigin = instr;			                    // Istruzione dinamica da risolvere
		ilpw.dataItemFieldToSolve = dataItemToSolve;                  	// Identificatore completo campo istruzione dinamica da risolvere
		loadInfoPointersAndAreasAddressed(ilpw);                        // Individuazione istruzioni SET e EXEC CICS ADDRESS
		
		return ilpw;
	}

	/* --------------------------------------------------
	 * inizializzazioni generali processo individuazione
	 * valori dinamici SENZA creazione oggetto di lavoro.
	 * Metodo richiamato da LogicSpreadedPgm
	 * --------------------------------------------------
	 * 
	 * 1) Caricamento Set con Exec CIcs Address TWA/CSA
	 * 2) Caricamento Set con Cobol Set
	 * 
	 */
	static LogicWorkProcess dynamicValuesInitial(LogicWorkProcess ilpw, ProgramCobol programCaller, ProgramCobol programOrigin, Instruction instr, DataItemCobolIdentifier dataItemToSolve) {
		
		// Attività iniziali di inizializzazione
		ilpw.isExecLogicSpreaded = true;                       		    // Esecuzione logiche stesso programma origine, NOT spreaded
		ilpw.programOrigin = programOrigin;								// Programma radice cone istruzione dinamica da risolvere
		ilpw.programCur = programCaller;								// Programma corrente (caller) in processo soluzione istruzioni
		ilpw.instrDynamicOrigin = instr;			                    // Istruzione dinamica da risolvere
		ilpw.dataItemFieldToSolve = dataItemToSolve;                  	// Identificatore completo campo istruzione dinamica da risolvere
		loadInfoPointersAndAreasAddressed(ilpw);                        // Individuazione istruzioni SET e EXEC CICS ADDRESS		
		return ilpw;
	}

	/* --------------------------------------------------
	 * Catalogazione info su pointer e aree indirizzate
	 * --------------------------------------------------
	 */
	static void loadInfoPointersAndAreasAddressed(LogicWorkProcess ilpw) {
		
		InstructionCobolProcedure instrCobol = null;
		InstructionCics instrCics = null;
		
		
		//////////////////////////////////////////////////////////////////
		// 1) Caricamento Set per ottimizzazione ricerca TWA/CSA
		//////////////////////////////////////////////////////////////////
		
	    ilpw.set_setCicsAddressTwaCsa.clear();							// Numeri istruzioni EXEC CICS ADDRESS TWA|CSA(pointer)
	    ilpw.set_setCicsAddressSet.clear();								// Numeri istruzioni EXEC CICS ADDRESS SET(Address Of area) USING(pointer)
	    ilpw.set_setCobolAddressOf.clear();								// Numeri istruzioni SET ADDRESS OF area TO pointer
	    ilpw.set_setCobolToAddressOf.clear();							// Numeri istruzioni SET pointer TO ADDRESS OF area  

		
		// Scan istruzioni
		for (ProgramCobolEntry<? extends Instruction> entryProcedure : ilpw.programCur.entriesProcedure()) {
			
			// Istruzione Cobol SET
			if (entryProcedure.getInstruction() instanceof InstructionCobolProcedure 
			&&  entryProcedure.getTypeInstr() == EnumCobolReservedWords.PROC_SET) {
				instrCobol = (InstructionCobolProcedure) entryProcedure.getInstruction();
				
				// SET ADDRESS OF area TO pointer
				if (instrCobol.setIsAddressOfToPointer()) {
					ilpw.set_setCobolAddressOf.add(instrCobol.getNumInstr());
				}
				// SET pointer TO ADDRESS OF area  
				if (instrCobol.setIsPointerToAddressOf()) {
					ilpw.set_setCobolToAddressOf.add(instrCobol.getNumInstr());
				}
				continue;
			}
			
			// Istruzione Cics
			if (entryProcedure.getInstruction() instanceof InstructionCics 
			&&  entryProcedure.getEntryType() == EnumInstrDataCategory.CICS_PRECOMPILER) {
				instrCics = (InstructionCics) entryProcedure.getInstruction();
				// Interessano solo le EXEC CICS ADDRESS
				if (instrCics.getTypeInstrPrecompiler() != EnumPrecompilerReservedWords.CICS_INSTR_ADDRESS) {
					continue;
				}
				// Presente il parametro TWA quindi si tratta di EXEC CICS ADDRESS TWA
				if (instrCics.isThereOperand("TWA")) {
					ilpw.set_setCicsAddressTwaCsa.add(instrCics.getNumInstr());
					continue;
				}
				// Presente il parametro CSA quindi si tratta di EXEC CICS ADDRESS CSA
			    if (instrCics.isThereOperand("CSA")) {
					ilpw.set_setCicsAddressTwaCsa.add(instrCics.getNumInstr());
					continue;
				}
				 
				// Presente il parametro SET quindi si tratta di EXEC CICS ADDRESS SET(Address Of Area) USING(pointer)
				if (instrCics.isThereOperand("SET")) {
					ilpw.set_setCicsAddressSet.add(instrCics.getNumInstr());
					continue;
				}
				
			} // end-if
			
 		} // end-for
		
	}


	/* -------------------------------------------------------- 
	 * Aggiornamento flags istruzione dinamica origine logiche  
	 * -------------------------------------------------------- 
	 * 
	 * Il metodo viene richiamato da LogicSamePgm e LogicSpreadedPgm  con operandi
	 * in struttura di controllo ilpw.arDataItemFieldSubToSolve
	 * 
	 * La risoluzione di una istruzione dinamica è fatta da LogicSamePgm sempre un operando per volta
	 * e le strutture dinamiche sono aggiornate inclusi i flags dinamici a livello di campo/sottocampo/last set
	 * 
	 * I flag a livello di campo vengono portati a livello di istruzione
	 * Nel caso ci siano due campi (Exec Cics Map .. Mapset ..) si tiene conto
	 * 
	 * Se sono presenti solo campi dinamici non movimentati con value   si accende il flag instrDynamicLight.
	 * Se sono presenti ultime assegnazioni spreaded 					si accende il flag instrDynamicSpreaded.
     * Se sono stati individuati dei valori per TUTTI i campi 			si accende il flag instrDynamicSolved.
     * Se sono state risolte TUTTE le ultime assegnazioni 				si accende il flag instrDynamicSolvedFull.
     * Se sono presenti ultime assegnazioni in attesa di dati esterni 	si accende il flag instrDynamicWaitingForData.
     * 
	 */
	static void updateInstr(LogicWorkProcess ilpw) {

	    boolean isLight = true;
	    boolean isSolved = true;
	    boolean isSolvedFull = true;
	    boolean isSpreaded = false;
	    boolean isWaitingForData = false;

		// Verifica se ci sono ancora ultime assegnazioni da risolvere o in attesa di dati esterni.
		
		// Scan CAMPI in programma origine 
		for (int i = 0; i < ilpw.arDataItemFieldSubToSolve.length; i++) {

			// Campo literal alfanumerica: skip
			if (ilpw.arDataItemFieldSubToSolve[i].getIdentifierType() == EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {continue;}
			
			ilpw.dataItemFieldToSolve = ilpw.arDataItemFieldSubToSolve[i];
			
			if (ilpw.dynamicFieldToSolve.entityDynamicField.getLight() == false) {
				isLight = false;
			}
			if (ilpw.dynamicFieldToSolve.entityDynamicField.getSolved() == false) {
				isSolved = false;
				isSolvedFull = false;
			}
			if (ilpw.dynamicFieldToSolve.entityDynamicField.getSpreaded() == true) {
				isSpreaded = true;
			}
			if (ilpw.dynamicFieldToSolve.entityDynamicField.getWaitingForData() == true) {
				isWaitingForData = true;
			}


		} // end-for campi
		
        // Flags stato soluzione istruzione
		ilpw.instrDynamicOrigin.setDynamicLight(isLight);
		ilpw.instrDynamicOrigin.setDynamicSolved(isSolved);
		ilpw.instrDynamicOrigin.setDynamicSolvedFull(isSolvedFull);
		ilpw.instrDynamicOrigin.setDynamicSpreaded(isSpreaded);
		ilpw.instrDynamicOrigin.setDynamicWaitingForData(isWaitingForData);

	}

	/* ----------------------------------------------------------------------------
	 * Aggiornamento flags istruzione dinamica origine logiche programmi chiamanti
	 * ----------------------------------------------------------------------------
	 * 
	 * Il metodo viene richiamato da LogicSpreadedPgm con operandi
	 * in struttura di controllo ilpw.arDataItemFieldSubToSolve
	 * 
	 * La risoluzione di una istruzione dinamica è fatta da LogicSamePgm sempre un operando per volta
	 * e le strutture dinamiche sono aggiornate inclusi i flags dinamici a livello di campo/sottocampo/last set
	 * 
	 * I flag a livello di campo vengono portati a livello di istruzione
	 * Nel caso ci siano due campi (Exec Cics Map .. Mapset ..) si tiene conto
	 * 
	 * Se sono presenti solo campi dinamici non movimentati con value   si accende il flag instrDynamicLight.
	 * Se sono presenti ultime assegnazioni spreaded 					si accende il flag instrDynamicSpreaded.
     * Se sono stati individuati dei valori per TUTTI i campi 			si accende il flag instrDynamicSolved.
     * Se sono state risolte TUTTE le ultime assegnazioni 				si accende il flag instrDynamicSolvedFull.
     * Se sono presenti ultime assegnazioni in attesa di dati esterni 	si accende il flag instrDynamicWaitingForData.
     * 
	 */
	static void updateInstrSpreadedPgm(LogicWorkProcess ilpw) {

	    boolean isLight = true;
	    boolean isSolved = true;
	    boolean isSolvedFull = true;
	    boolean isSpreaded = false;
	    boolean isWaitingForData = false;

		// Verifica se ci sono ancora ultime assegnazioni da risolvere o in attesa di dati esterni.
		
		// Scan CAMPI in programma origine 
		for (int i = 0; i < ilpw.arDataItemFieldSubToSolve.length; i++) {

			// Campo literal alfanumerica: skip
			if (ilpw.arDataItemFieldSubToSolve[i].getIdentifierType() == EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {continue;}
			
			ilpw.dataItemFieldToSolve = ilpw.arDataItemFieldSubToSolve[i];
			
			if (ilpw.dynamicFieldToSolve.entityDynamicField.getLight() == false) {
				isLight = false;
			}
			if (ilpw.dynamicFieldToSolve.entityDynamicField.getSolved() == false) {
				isSolved = false;
				isSolvedFull = false;
			}
			if (ilpw.dynamicFieldToSolve.entityDynamicField.getSpreaded() == true) {
				isSpreaded = true;
			}
			if (ilpw.dynamicFieldToSolve.entityDynamicField.getWaitingForData() == true) {
				isWaitingForData = true;
			}


		} // end-for campi
		
        // Flags stato soluzione istruzione
		ilpw.instrDynamicOrigin.setDynamicLight(isLight);
		ilpw.instrDynamicOrigin.setDynamicSolved(isSolved);
		ilpw.instrDynamicOrigin.setDynamicSolvedFull(isSolvedFull);
		ilpw.instrDynamicOrigin.setDynamicSpreaded(isSpreaded);
		ilpw.instrDynamicOrigin.setDynamicWaitingForData(isWaitingForData);

	}	
	/* --------------------------------------------------------------------
     * Restituisce true se ultima assegnazione in attesa di dati esterni
     * --------------------------------------------------------------------
     * 
     * L0ultima assegnazione fornita in input NON è ancora stata risolta.
     * 
     */
	static boolean isLastSetFromExternalData(EnumLogicSetMode setMode) {

		if (setMode == EnumLogicSetMode.LAST_SET_BY_COBOL_READ
		||  setMode == EnumLogicSetMode.LAST_SET_BY_SQL_SELECT	
		||  setMode == EnumLogicSetMode.LAST_SET_BY_FILE_GET
		||  setMode == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRMID
		||  setMode == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRNID		
		||  setMode == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM	
		||  setMode == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TS_QUEUE	
		||  setMode == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TD_QUEUE	
		||  setMode == EnumLogicSetMode.LAST_SET_BY_CICS_RECEIVE_MAP) {
			return true;
		}

		return false;
	}

	/*
	 * Restituisce true se la trasformazione è di ultima assegnazione da risolvere 
	 * nei programmi chiamanti
	 * 
	 */
    static boolean isLastSetSpreaded(EnumLogicSetMode setMode) {
    	    	
		if (setMode == EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
		||  setMode == EnumLogicSetMode.LAST_SET_BY_CICS_DFHCOMMAREA	
		||  setMode == EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE	
		||  setMode == EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM		
		||  setMode == EnumLogicSetMode.LAST_SET_BY_CICS_TWA	
		||  setMode == EnumLogicSetMode.LAST_SET_BY_CICS_CSA) {
			return true;
		}
		return false;
	}


	
	/* ----------------------------------------------------
     * Restituisce i nomi di tutti i programmi chiamanti
     * ----------------------------------------------------
     * 
     * Si accede alla tabella Relations e si estraggono tutti i programmi in relazione con.
     * 
     */
	public static ArrayList<String> getPgmCallers(String programName
										  , String sys
										  , String subSys
										  , UserConfiguration ucfg
										  ) throws ExceptionAmrita, SQLException {
		
		EntityRelation objRelation = null;							// Relazione fra programmi
		List<EntityRelation> ar_obj = null;						    // Output generico da accesso ai dati
		ArrayList<String> al_PgmCaller = null;						// Elenco programmi chiamanti
		Set<String> set_PgmCaller = null;						    // Set    programmi chiamanti
		String whereCondition = "";									// Condizione di where
		String orderBy = "";							    		// Order by
		
		// Operazioni per accesso al database
		Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione condizione Where 
		whereCondition =                      "      sys  =  '" 	 + sys    + "'";
		whereCondition = whereCondition +	  " AND  subSys  =  '" 	 + subSys + "'";
		whereCondition = whereCondition +     " AND (relation  =   " + EnumRelation.PGM_CALLED_PGM.ordinal()    + " OR ";
		whereCondition = whereCondition +     "      relation  =   " + EnumRelation.PGM_XCTL_PGM.ordinal()      + " OR ";
		whereCondition = whereCondition +     "      relation  =   " + EnumRelation.PGM_CICS_LINK_PGM.ordinal() + " OR ";
		whereCondition = whereCondition +     "      relation  =   " + EnumRelation.PGM_CICS_XCTL_PGM.ordinal();
		whereCondition = whereCondition +     "     ) ";
		whereCondition = whereCondition +     " AND  typeObjectB  =   " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		whereCondition = whereCondition +     " AND  idObjectB  =  '" + programName + "'";
		
        // Ordinamento per numero istruzione
		orderBy = " ORDER BY idObjectA";
		
        // Esecuzione query su Relation con condizione fornita
		ar_obj = eoDAO.readSetEntityWhere(whereCondition, orderBy);
		
		al_PgmCaller = new ArrayList<String> ();
		set_PgmCaller = new HashSet<String> ();

		// Scan relazioni e accodamento nomi programma
		for (Object obj : ar_obj) {
			
			objRelation = (EntityRelation) obj;
			set_PgmCaller.add(objRelation.getIdObjectA());
		}

		al_PgmCaller.addAll(set_PgmCaller);

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return al_PgmCaller;
	}

	
	/* --------------------------------------------------------
	 * Restituisce le origini relazione nel programma chimante
	 * --------------------------------------------------------
	 * 
	 * Viene restituito un ArrayList di oggetti LogicCallerOrigin
	 * che qualificano completamente il programma chiamante.
	 * Si tratta dei punti di call/link/Xctl
	 * Se non ci sono chiamanti restituisce un array list vuoto.
	 * 
	 */
	static ArrayList<LogicCallerOrigin> getPgmCallerOrigin( String programNameCaller
														  , String programNameCalled
														  , int lvlCaller
														  , String sys
														  , String subSys
														  , UserConfiguration ucfg
															) throws SQLException, ExceptionAmrita {

		EntityRelationOrigin objRelationOrigin = null;				// Origine relazione, con numero istruzione e tipo
		List<EntityRelationOrigin> ar_obj = null;					// Output generico da accesso ai dati
		LogicCallerOrigin callerOrigin = null;						// Descrittore singolo punto in chiamante
		ArrayList<LogicCallerOrigin> al_callerOrigin = null;		// Elenco punti chiamanti
		String whereCondition = "";									// Condizione di where
		String orderBy = "";							    		// Order by
		
		// Operazioni per accesso al database
		Connection conn = DataBaseConnections.getConnection();
		IDAORelationOrigin eoDAO = (DAOImplRelationOrigin) AmritaStartup.sqlFactory.getDAORelationOrigin(conn, false,false, ucfg);

		// Composizione condizione Where 
		whereCondition =                      "      sys  =  '" 	 + sys    + "'";
		whereCondition = whereCondition +	  " AND  subSys  =  '" 	 + subSys + "'";
		whereCondition = whereCondition +     " AND (relation  =   " + EnumRelation.PGM_CALLED_PGM.ordinal()    + " OR ";
		whereCondition = whereCondition +     "      relation  =   " + EnumRelation.PGM_XCTL_PGM.ordinal()      + " OR ";
		whereCondition = whereCondition +     "      relation  =   " + EnumRelation.PGM_CICS_LINK_PGM.ordinal() + " OR ";
		whereCondition = whereCondition +     "      relation  =   " + EnumRelation.PGM_CICS_XCTL_PGM.ordinal();
		whereCondition = whereCondition +     "     ) ";
		whereCondition = whereCondition +     " AND  typeObjectA  =   " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		whereCondition = whereCondition +     " AND  idObjectA  =  '" + programNameCaller + "'";
		whereCondition = whereCondition +     " AND  typeObjectB  =   " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		whereCondition = whereCondition +     " AND  idObjectB  =  '" + programNameCalled + "'";
		
        // Ordinamento per numero istruzione
		orderBy = " ORDER BY numInstrOrigin";
		
		
        // Esecuzione query su RELO con condizione fornita
		ar_obj = eoDAO.readSetEntityWhere(whereCondition, orderBy);
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
        
		al_callerOrigin = new ArrayList<LogicCallerOrigin> ();
		

		// Scan origine relazioni con i punti di chiamata cercati allo stesso livello
		for (Object obj : ar_obj) {
			
			objRelationOrigin = (EntityRelationOrigin) obj;
			
			// Recupero programma chiamante serializzato e impostazione info chiamante
			callerOrigin = new LogicCallerOrigin();
			callerOrigin.idProgramCaller = programNameCaller;
			callerOrigin.typeProgramCaller = objRelationOrigin.getTypeObjectA();
			callerOrigin.numInstr = objRelationOrigin.getNumInstrOrigin();
			callerOrigin.lvlCaller = lvlCaller;    
			callerOrigin.idProgramCalled = programNameCalled;
			
			// Accodamento punto di chiamata in programma chiamante 
			al_callerOrigin.add(callerOrigin);
		}

		return al_callerOrigin;
	}

	/* --------------------------------------------------------------------------
	 * Generazione valori definitivi CAMPI come moltiplicazione valori sottocampi
	 * --------------------------------------------------------------------------
	 * 
	 * Prima vengono composti i valori assunti da ogni sottocampo.
	 * Poi vengono moltiplicati i valori di tutti i sottocampi.
	 * Vengono eliminati i duplicati e i valori vengono ordinati.
	 * Vengono inoltre aggiornate le strutture del sottocampo con le ultime
	 *   assegnazioni local e spreaded
	 *   
	*/
	static ArrayList<String> generateFieldValuesBySubFieldsComposition(LogicWorkProcess ilpw) {
		
		// Ipotizzo che non tutti i sottocampi siano stati risolti con dei valori
		ilpw.isInstrDynamicSolvedFull = false;
		
		// Allocazione ArrayList di valori per il campo
		ilpw.al_valuesField = new ArrayList<String> ();
		
		//--------------------------------------------------------------------------------
		// 1) Normalizzazione valori di ogni sottocampo
		//--------------------------------------------------------------------------------

		// L'operando è un campo di cui comporre i valori
		// Verifica se qualche sottocampo non è stato risolto
		ilpw.al_DynamicFieldSubWrk = ilpw.dynamicFieldToSolve.al_FieldSub;
		ilpw.allSubFieldsWithValues = true;

		// Scan sottocampi campo dinamico in input
		for (LogicDynamicFieldSub dynamicFieldSub : ilpw.dynamicFieldToSolve.al_FieldSub) {
			
			// Il sottocampo o qualche suo receiver implicito generato è in attesa di dati esterni
			// Sicuramente NON si possono comporre i valori da restituire
			if (dynamicFieldSub.isWaitingForExternalData) {
				ilpw.dynamicFieldToSolve.al_valuesField = ilpw.al_valuesField;
				
				// Se esiste il valore di default a livello di campo viene restituito
				for (LogicDynamicValue value : dynamicFieldSub.al_value) {
					if (value.isDefaulValue 
					&& value.entityDynamicValue.getPosInSubField() == 1
					&& value.entityDynamicValue.getLngInSubField() == dynamicFieldSub.dataItemIdentifierSubField.getDataItem().getSizeBytes()) {
						ilpw.al_valuesField.add(value.entityDynamicValue.getValue());
					}
				}				
				return ilpw.al_valuesField;
			}			
			
			ilpw.dynamicFieldSubToSolve = dynamicFieldSub;

			// Scan catene trasformazione sottocampo
			for (int k = 0; k < dynamicFieldSub.al_al_chainSetSubField.size(); k++) {
				// TODO verificare con souluzione in programma chiamante
				// In caso di attivazione spreaded NON deve effettuare nessun controllo
				// I controllo sono già stati fatti nell'individuazione dei valori nel programma chiamante
				if (!ilpw.isExecLogicSpreaded) {
					// Catena di trasformazione non coperta da path o invalidata: skip
					if (dynamicFieldSub.ar_chainSetFlagPathCovering[k] == false) {continue;}
					if (dynamicFieldSub.ar_chainSetFlagDisabled[k]     == true)  {continue;}

					// Catena di trasformazione valida ma non risolta: specifica ultima assegnazione sottocampo non risolta
					if (dynamicFieldSub.ar_chainSetFlagValues[k] == false) {
						ilpw.isInstrDynamicSolvedFull = false;
					} // end-if
				}

			} // end-for catene trasformazione

			// Valori non generati o generati solo valori parziali 
			// Update dynamicSubField con last set spreaded
			if (!areSubFieldValuesFullCovering(ilpw, dynamicFieldSub)) {
				ilpw.allSubFieldsWithValues = false;
				continue;
			}

			// Valori sottocampo disponibili: produzione valori stringa effettivi per il sottocampo ordinati e senza duplicati
			ilpw.al_valueSubField = valuesSubFieldBinded(ilpw, dynamicFieldSub);
			dynamicFieldSub.al_valueComplete = ilpw.al_valueSubField;

		} // end-for sottocampi

		//--------------------------------------------------------------------------------
		// 2) Combinazione valori sottocampi per composizione valori campo
		//--------------------------------------------------------------------------------
		
		// Tutti i sottocampi sono almeno parzialmente risolti: moltiplicazione valori 
		if (ilpw.allSubFieldsWithValues) {
			ilpw.isInstrDynamicSolvedFull = true;
			ilpw.isInstrDynamicToSolve = false;
			ilpw.al_ValuesWrk = new ArrayList<String> ();
			ilpw.dynamicFieldWrk =ilpw.dynamicFieldToSolve;
			generateFieldValuesRecursive(ilpw.dynamicFieldWrk, 0,ilpw.al_ValuesWrk, "");  // Moltiplicazione singoli valori
			// Scan valori campi individuati
			for (String valueField : ilpw.al_ValuesWrk) {
				ilpw.al_valuesField.add(valueField);
			}
			// per logiche local, update istruzione
			if (!ilpw.isExecLogicSpreaded) {
				ilpw.dynamicFieldToSolve.entityDynamicField.setSolved(true);
			}
		}

		//--------------------------------------------------------------------------------
		// 3) Eliminazione duplicati e ordinamento valori
		//--------------------------------------------------------------------------------
				
		Set<String> set_valuesField = null;
		set_valuesField = new HashSet<String>();
		set_valuesField.addAll(ilpw.al_valuesField);
		ilpw.al_valuesField.clear();
		ilpw.al_valuesField.addAll(set_valuesField);
		Collections.sort(ilpw.al_valuesField);

		//--------------------------------------------------------------------------------
		// 4) Update valori in struttura campo dinamico.
		//    Il dettaglio dei valori, con la loro origine e la db entity è a livello di sottocampo
		//    In caso di campo elementare la riga di EntityDynamicFieldSub con sottocampo a space
		//    di servizio, è già stata caricata a livello di sottocampo
		//--------------------------------------------------------------------------------
	    
		ilpw.dynamicFieldToSolve.al_valuesField = ilpw.al_valuesField;
		
		return ilpw.al_valuesField;
	}	
	
	/* ------------------------------------------------------------------------------------
     * Restituisce true se i valori generati per il sottocampo coprono la dimensione completa
     * ------------------------------------------------------------------------------------
     * 
     * Per esempio potrebbero esserci nel programma assegnazioni
     * parziali di literal e assegnazioni sempre parziali da campi
     * da risolvere nei pgm chiamanti, come DFHCOMMAREA oppure
     * Using Parm.
     * 
     */
	static boolean areSubFieldValuesFullCovering(LogicWorkProcess ilpw, LogicDynamicFieldSub logicDynamicFieldSub) {
		
		int sizeSubField = 0;
		int posStart = 0;								// Posizione inizio valore
		int posEnd = 0;									// Posizione fine valore 
		boolean ar_posSubField[] = null;				// Ogni posizione indica se coperta da valori assunti dal sottocampo		
		
		// Dimensioni sottocampo in programma origine
		sizeSubField = logicDynamicFieldSub.entityDynamicFieldSub.getSizeSubField();
		
		// In caso di logiche spreaded, la ricerca dei valori sottocampo potrebbe
		// iniziare NON da un sottocampo completo ma già da una posizione di un campo,
		// come nel caso di chiamante Cics Link Commarea(WS-COMMAREA).
		// Ciò significa che è stato attivato il processo ricorsivo in modo
		// artificioso, da una condizione che non si riscontra nella soluzione delle
		// logiche stesso programma, partendo da un sottocampo di lavoro che NON
		// è da considerarsi atomico, ai fini della ricomposizione dei valori.
		// Quindi, in questi casi, il size da considerare per il sottocampo, è
		// dato dalla lunghezza nel sottocampo originario da cercare, individuata
		// nel programma origine e impostata nell'ultima assegnazione spreaded.
// 		if (ilpw.isExecLogicSpreaded && ilpw.posInSubField > 1) {
		
// TODO rivedere ?????????????????????? possibile errore ??????????????????		
//		if (ilpw.isExecLogicSpreaded ) {
//			sizeSubField = ilpw.lngInSubField;
//		}
		
		ar_posSubField = new boolean[sizeSubField];
		
		// Default tutte le posizioni non coperte da valori
		for (int i = 0; i < ar_posSubField.length; i++) {
			ar_posSubField[i] = false;
		}
		
		// Scan valori individuati per il sottocampo origine , da posizione per lunghezza
		for (LogicDynamicValue logicDynamicValue : logicDynamicFieldSub.al_value) {
			
			// posizione di inizio e lunghezza nel sottocampo originario impostato dall'ultima assegnazione
			posStart = logicDynamicValue.entityDynamicValue.getPosInSubField() - 1;					// 0-based
			posEnd = posStart + logicDynamicValue.entityDynamicValue.getLngInSubField() - 1;		// 0-based
			
			// In caso di receiver impliciti la catena di trasformazione può fare riferimento a un valore
			// parziale che NON copre tutto il sottocampo origine. 
			// In quel caso la catena di trasformazione NON deve essere considerata per la composizione dei valori
			// Update posizioni sottocampo come coperte da valori
			for (int i = posStart; i <= posEnd; i++) {
				ar_posSubField[i] = true;
			}
			
		}

        // Scan posizioni sottocampo
		for (int i = 0; i < ar_posSubField.length; i++) {
			
			// Posizione non coperta da valore
			if (ar_posSubField[i] == false) {
				return false;
			}
		}
		
		return true;
	}


	/*
	 * ------------------------------------------------------------------------------
	 * Generazione valori effettivi assunti dal sottocampo nelle varie trasformazioni
	 * ------------------------------------------------------------------------------
	 * 
	 * I valori assunti dal sottocampo a livello di sottocampo in LogicDynamicSubField.al_value.
	 * Possono esserci valori parziali dovuti ad assegnazioni che hanno generato receiver impliciti.
	 * Un caso comune è quello di un sottocampo assegnato a un campo di gruppo con
	 * assegnazioni successive ai singoli campi componenti il gruppo. 
     *
     * In ogni caso i valori del sottocampo da posizione per lunghezza devono coprire tutto il sottocampo.
     * Non sono ammesse sovrapposizioni in quanto il valori discendono sempre da Move a campi e gruppi
     * che, sottodefiniti, si riferiscono sempre al sottocampo origine da posizione per lunghezza.
     *
	 * Quindi il valore può essere relativa solo a una porzione di sottocampo ed è quindi 
	 * necessario comporre i valori definitivi assunti dal sottocampo
	 * sulla base dei valori assunti da porzioni del sottocampo stesso.
	 * 
	 * CASI gestiti di pos-lng, a cui corrispondono assegnazioni multiple di valori
	 * 
	 * 1) Porzioni di sottocampo NON sovrapposte 
	 *    1-2
	 *    3-3
	 *    4-1
	 *    5-3 
	 * 2) Porzioni di campo sovrapposte (non deve succedere, non si producono valori)
	 *    1-2
	 *    1-5
	 *    1-7
	 *    3-2
	 *    3-5
	 *    5-3
	 * 3) Porzioni di campo con dei buchi (non si può procedere alla produzione dei valori)
	 *    1-2
	 *    5-3 
	 */
	@SuppressWarnings("unchecked")
	static ArrayList<String> valuesSubFieldBinded(LogicWorkProcess ilpw, LogicDynamicFieldSub dynamicFieldSub) {
		
		TreeMap<String,ArrayList<String>> tmap_values = null;       			// Key=pos"-"lng, Data=value
		Entry<String, ArrayList<String>> entry_tmap_values = null;				// Entry gruppo pos-lng valori in loop
		Entry<String, ArrayList<String>> ar_entry_tmap_values[] = null;         // Array Entry gruppo pos-lng 
		ArrayList<String> al_valueComplete = null;								// Valori definitivi sottocampo
		ArrayList<String> al_valueCompleteWork = null;							// Valori definitivi sottocampo in progress
		ArrayList<String> al_valuePartial = null;								// Valori parziali sottocampo
		String ar_tmapKey[] = null;												// Chiavi pos-lng gruppi valori
		String keyValue = "";													// Chiave pos-lng gruppi valori 
		String valueDefault = "";												// Valore di default sottocampo
		String valuePartial = "";												// Valore parziale sottocampo
		boolean areAllValuesFromPos1 = true;								    // True indica tutti valori parziali da pos 1 e NON necessità di frammentazione
		int numChainSet = 0;                                                    //
		int posValuePartial = 0;                                                // Pos parziale valori
		int lngValuePartial = 0;                                                // Lng parziale valori
		int iValue = 0;															// Indice di servizio
		int i = 0;																// Indice di servizio
			
		// Initial
		tmap_values = new TreeMap<String, ArrayList<String>>();
		valueDefault = "";
		
		
		//////////////////////////////////////////////////////////////////////////
		// (1) Popolamento preliminare tree map con i valori parziali disponibili
		//     I valori sono ordinati per chiave pos-lng
		//     I valori possono essere multipli per pos-lng
		//////////////////////////////////////////////////////////////////////////
		
		// Scan valori parziali individuati per il sottocampo
		for (iValue = 0; iValue < dynamicFieldSub.al_value.size(); iValue++) {
			
            // Info valore parziale sottocampo
			valuePartial = dynamicFieldSub.al_value.get(iValue).entityDynamicValue.getValue();
			posValuePartial = dynamicFieldSub.al_value.get(iValue).entityDynamicValue.getPosInSubField();
			if (ilpw.isSpreadedSubFielValuePartial) {
				posValuePartial = ilpw.posRcvInSubFieldSpreaded;
			}
			lngValuePartial = dynamicFieldSub.al_value.get(iValue).entityDynamicValue.getLngInSubField();
			
			// Valore di default che copre tutto il sottocampo: si gestisce alla fine
			if ( dynamicFieldSub.al_value.get(iValue).isDefaulValue) {
				valueDefault = StringService._pad(valuePartial, ' ', lngValuePartial, StringService.PAD_RIGHT);
				continue;
			}
			
			// In caso di attivazione spreaded NON deve effettuare nessun controllo
			// I controllo sono già stati fatti nell'individuazione dei valori nel programma chiamante
 			if (ilpw.isExecLogicSpreaded = false) {
				// Il valore è relativo a una catena senza path a copertura o disabilitata: skip
				numChainSet = dynamicFieldSub.al_value.get(iValue).numChainSet;			// Catena di trasformazione che ha originato il valore
				if (dynamicFieldSub.ar_chainSetFlagPathCovering[numChainSet] == false
			    ||  dynamicFieldSub.ar_chainSetFlagDisabled[numChainSet] == true) {
					continue;
				}
 			}
			
			// Recupero o creazione array list valori
			keyValue = posValuePartial + "-" + lngValuePartial;
			al_valuePartial = tmap_values.get(keyValue);
			if (al_valuePartial == null) {
				al_valuePartial = new ArrayList<String>();
			}

			// Formattazione alla lunghezza corretta e accodamento valore a key pos-lng
			valuePartial = StringService._pad(valuePartial, ' ', lngValuePartial, StringService.PAD_RIGHT);
			al_valuePartial.add(valuePartial);
			tmap_values.put(keyValue, al_valuePartial);
			
		}

		//////////////////////////////////////////////////////////////////////////
		// (2) Gestione semplificata se tutti i valori iniziano a colonna 1
		//     I valori vengono portati direttamente in output.
		//////////////////////////////////////////////////////////////////////////
		
		ar_entry_tmap_values = new Entry[tmap_values.entrySet().size()];
		ar_entry_tmap_values = tmap_values.entrySet().toArray(ar_entry_tmap_values);
		al_valueComplete = new ArrayList<String> ();
		
		// Se tutti i valori sottocampo iniziano da pos 1 li porto direttamente in output  
		for (i = 0; i < ar_entry_tmap_values.length; i++) {
			entry_tmap_values = ar_entry_tmap_values[i];
			ar_tmapKey = entry_tmap_values.getKey().split("-");
			posValuePartial = Integer.parseInt(ar_tmapKey[0]);
			if (posValuePartial > 1) {
				areAllValuesFromPos1 = false;
			}
			for (String value : entry_tmap_values.getValue()) {
				al_valueComplete.add(value);
			}
		}
		// Tutti i valori del sottocampo iniziano da pos 1
		if (areAllValuesFromPos1) {
			// Inserisco il valore di default non ancora gestito solo se diverso da space
			if (!valueDefault.isBlank()) {
				al_valueComplete.add(valueDefault);
			}
			// Elimino duplicati e sorto
			Set<String> set_valueComplete = null;
			set_valueComplete = new HashSet<String>();
			set_valueComplete.addAll(al_valueComplete);
			al_valueComplete.clear();
			al_valueComplete.addAll(set_valueComplete);
			Collections.sort(al_valueComplete);
			return al_valueComplete;
		}
			
        
        //////////////////////////////////////////////////////////////////////////////////////
        // (3) Moltiplicazione valori parziali.
        //     Vengono semplicemente accodati progressivamente i valori a posizione crescente
        //     Valori restituiti in al_valueComplete
        //////////////////////////////////////////////////////////////////////////////////////
        
		al_valueComplete.clear();
		
		// Scan partial key/values
        for (Entry<String, ArrayList<String>> entryPartial : tmap_values.entrySet()) {
			
        	al_valuePartial = entryPartial.getValue();
        	
        	// Sono i primi valori parziali: vanno inseriti e non moltiplicati
        	if (al_valueComplete.size() == 0) {
        	   for (String value : al_valuePartial) {
					al_valueComplete.add(value);
					continue;			// Next valore parziale stessa pos-lng
			   }
        	   continue;                // next valori parziali successiva pos-lng
			}
        	
        	// E' già stato inserito il primo gruppo di valori parziali: ora si devono moltiplicare
        	al_valueCompleteWork = new ArrayList<String>();
        	
        	// Scan valori completi progressivi fino a questo momento
        	for (String valueComplete2 : al_valueComplete) {
        		// Scan valori parziali da moltiplicare
				for (String valuePartial2 : al_valuePartial) {
					al_valueCompleteWork.add(valueComplete2 + valuePartial2);
				}
			}
        	
        	// L'array list di lavoro diventa quello aggiornato
        	al_valueComplete = al_valueCompleteWork;
  		}
        
        // Inserimento default
        if (!valueDefault.isBlank()) {
        	al_valueComplete.add(valueDefault);
		}
		///////////////////////////////////////////////////////////////////////////////////////
	    // (4) Eliminazione duplicati e ordinamento valori
		///////////////////////////////////////////////////////////////////////////////////////
        
		Set<String> set_valueComplete = null;
		set_valueComplete = new HashSet<String>();
		set_valueComplete.addAll(al_valueComplete);
		al_valueComplete.clear();
		al_valueComplete.addAll(set_valueComplete);
		Collections.sort(al_valueComplete);
		
		return al_valueComplete;
	}	
	

	/*
	 * ---------------------------------------------------------------------
     * Moltiplicazione valori sottocampi per generazione valori finali campi
     * ---------------------------------------------------------------------
     * 
     * Ogni sottocampo del campo in input ha generato dei valori nei path di esecuzione possibili.
     * Ora bisogna ricomporre i valori del campo come moltiplicazione dei valori dei singoli sottocampi.
     * Tutti i valori dei sottocampi sono già della lunghezzsa corretta e la loro somma coincide 
     * esattamente con le dimensioni del campo.
     * 
     * Viene valorizzato ricorsivamente l'ArrayList<String> al_Values con i valori individuati e ricomposti
     * per il campo, come somma dei singoli valori dei sottocampi componenti il campo.
     * 
     */
	static void generateFieldValuesRecursive(LogicDynamicField dynamicField
											, int iSubField
											, ArrayList<String> al_Values
											, String valueParzial
											) {
		
		LogicDynamicFieldSub logicSubField = null;
		String valueField = "";
		String newValueParzial = "";
		
		logicSubField = dynamicField.al_FieldSub.get(iSubField);
		
		// Ultimo subfield: generazione valori complessivi
		if (iSubField == dynamicField.al_FieldSub.size() - 1) {
			// Scan valori ultimo sottocampo e caricamento valori composti complessivi
			for (String valueComplete : logicSubField.al_valueComplete) {
				valueField = valueParzial + valueComplete;
				al_Values.add(valueField);
			}
			return;
		}
		
		// Scan valori sottocampo
		for (int i = 0; i < logicSubField.al_valueComplete.size(); i++) {
			
			newValueParzial = valueParzial + logicSubField.al_valueComplete.get(i);
			
            // Attivazione ricorsiva
            generateFieldValuesRecursive(dynamicField, iSubField + 1, al_Values, newValueParzial);
		}
		
		return;		
	}

	/*
	 * ---------------------------------------------------------------------
     * Inserimento valori generati per il campo in struttura dinamica valori
     * ---------------------------------------------------------------------
     * 
     * I valori sono SEMPRE associati a un sottocampo, un campo può sottodefinire più sottocampi
     * e il primo sottocampo definito in DynamicFieldSub è definito con sottocampo a space, riferendosi al
     * campo nel suo complesso.
     * 
     * Nel caso di campo elemetare senza sottocampi viene comunque definito un sottocampo a space.
     * Questo sottocampo individua i valori del campo nel suo complesso, in DynamicFieldSubValue
     * 
     * Il processo di ricerca valori assegna i valori sempre e solo ai sottocampi e solo a 
     * fine processo i valori vengono ricomposti per combinare i valori definitivi.
     * 
     * E' necessario inserire i valori del campo restituiti al chiamante in DynamicFieldSubValue,
     * con subfield a space, da pos=1 e lng=lunghezza campo in generale quando:
     * 
     * 1) Il campo è elementare
     * 2) Il valore NON è già presente fra quelli inseriti (controllo di robustezza)
     * 
     * Viene inoltre aggiornato il flag fieldValueFull se il valore è da pos=1 e lng=lunghezza campo
     */
	static void insertFullFieldValues(LogicWorkProcess ilpw, ArrayList<String> al_valuesField) {
		LogicDynamicValue logicDynamicValue = null;
		EntityDynamicFieldSubValue entityDynamicValue = null;
		EntityDynamicFieldSub entityDynamicFieldSubFirst = null;
		int lngField = 0;
		int progValue = 0;
		boolean isValueFound = false;
				
		// NON è un campo elementare
		if (ilpw.dynamicFieldToSolve.al_FieldSub.size() > 1) {
			return;
		}
		
		// Campo elementare, impostazioni iniziali
		entityDynamicFieldSubFirst = ilpw.dynamicFieldToSolve.al_FieldSub.get(0).entityDynamicFieldSub;
		progValue = ilpw.dynamicFieldToSolve.al_FieldSub.get(0).al_value.size();
		lngField = ilpw.dynamicFieldToSolve.dataItemCobolIdentifier.getDataItem().getSizeBytes();
		
		// Scan values presenti per update flag valore completo a livello campo
		for (LogicDynamicValue ldv : ilpw.dynamicFieldToSolve.al_FieldSub.get(0).al_value) {
			if (ldv.entityDynamicValue.getPosInSubField() == 1
			&& ldv.entityDynamicValue.getLngInSubField() == lngField) {
				ldv.entityDynamicValue.setFieldValueFull(true);
			}
		}
				
		// Scan values per inserimento valori completo a livello campo
		for (String value : al_valuesField) {
			
			// Scan valori già presenti
			isValueFound = false;
			for (LogicDynamicValue ldv : ilpw.dynamicFieldToSolve.al_FieldSub.get(0).al_value) {
				if (ldv.entityDynamicValue.getValue().equals(value)) {
					isValueFound = true;
				}
			}
			// Value già presente
			if (isValueFound) {
				continue;
			}
			
			// Contenitore valore
			logicDynamicValue = new LogicDynamicValue(entityDynamicFieldSubFirst.getSystem(), entityDynamicFieldSubFirst.getSubSystem());
			logicDynamicValue.isFieldValueFull = true;
			
			// Entity da inserire su db
			entityDynamicValue = new EntityDynamicFieldSubValue();
			entityDynamicValue.setSystem(entityDynamicFieldSubFirst.getSystem());
			entityDynamicValue.setSubSystem(entityDynamicFieldSubFirst.getSubSystem());
			entityDynamicValue.setIdObject(entityDynamicFieldSubFirst.getIdObject());
			entityDynamicValue.setTypeObject(entityDynamicFieldSubFirst.getTypeObject());
			entityDynamicValue.setNumInstr(entityDynamicFieldSubFirst.getNumInstr());
			entityDynamicValue.setIdField(entityDynamicFieldSubFirst.getIdField());
			entityDynamicValue.setIdSubField("");
			entityDynamicValue.setProgr(progValue++);
			
			entityDynamicValue.setFieldValueFull(true);
			entityDynamicValue.setIdPgmFrom("");
			entityDynamicValue.setTypeObjectFrom(EnumObject.NOT_ASSIGNED);
			entityDynamicValue.setPosInSubField(1);
			entityDynamicValue.setLngInSubField(lngField);
			entityDynamicValue.setValue(value);				
			
			// Accoda valore a struttura del sottocampo 0
			logicDynamicValue.entityDynamicValue = entityDynamicValue;
			ilpw.dynamicFieldToSolve.al_FieldSub.get(0).al_value.add(logicDynamicValue);			
		}
		
	}
		
	
	/*
	 * -------------------------------------------------------------------
	 * Restituisce tutti i sottocampi elementari di un campo di gruppo
	 * -------------------------------------------------------------------
	 * 
	 *  Se campo in input elementare si restituisce lo stesso campo
	 *  Si scartano eventuali definizioni di gruppo.
	 *  Si restituisce un array di InstructionCobolDataItem.
	 *   
	 * @param DataItemCobolIdentifier field
	 * @return DataItemCobolIdentifier[]
	 */
	static DataItemCobolIdentifier[] getSubFields(ProgramCobol program, DataItemCobolIdentifier field) {

		DataItemCobolIdentifier[] arDataItemCobolIdentifier = null;
		DataItemCobolIdentifier dataItemCobolIdentifier = null;
		ArrayList<DataItemCobolIdentifier> alDataItemCobolIdentifier = null;
		ProgramCobolEntry<? extends Instruction>[] entriesData = null;
		ProgramCobolEntry<? extends Instruction> entryDataField = null;
		ProgramCobolEntry<? extends Instruction> entryData = null;
		InstructionCobolDataItem instrData = null;
		EnumCobolReservedWords sectionField = null;
		int levelNumber = 0;
		int numInstrData = 0;

		// Campo elementare: restituisco il campo in input
		if (!field.getDataItem().isGroupField()) {
			arDataItemCobolIdentifier = new DataItemCobolIdentifier[1];
			arDataItemCobolIdentifier[0] = field;
			return arDataItemCobolIdentifier;
		}

		// Campo di gruppo

		entriesData = program.entriesData();
		alDataItemCobolIdentifier = new ArrayList<DataItemCobolIdentifier>();

		levelNumber = field.getDataItem().getLevelNumber();
		entryDataField = entriesData[field.getNumInstr()];
		sectionField = entryDataField.getProgramSection();

		// Scan campi successivi fino a livello = livello gruppo
		numInstrData = field.getDataItem().getNumInstr();

		// Scan data entries
		for (int i = numInstrData + 1; i < entriesData.length; i++) {

			entryData = entriesData[i];
			instrData = (InstructionCobolDataItem) entryData.getInstruction();
			dataItemCobolIdentifier = new DataItemCobolIdentifier();
			dataItemCobolIdentifier.setNameIdentifier(instrData.getDataName());
			dataItemCobolIdentifier.setIdentifierType(EnumCobolReservedWords.DATA_DIV_DATA_ITEM);
			dataItemCobolIdentifier.setDataItem(instrData);
			dataItemCobolIdentifier.setNumInstr(instrData.getNumInstr());

			// Fine Working
			if (entryData.getProgramSection() != sectionField) {
				break;
			}

			// Livello 88: skip
			if (dataItemCobolIdentifier.getDataItem().getLevelNumber() == 88) {
				continue;
			}

			// Fine definizione campo di gruppo
			if (dataItemCobolIdentifier.getDataItem().getLevelNumber() <= levelNumber 
			|| dataItemCobolIdentifier.getDataItem().getLevelNumber() == 77) {
				break;
			}

			// Campo di gruppo
			if (dataItemCobolIdentifier.getDataItem().isGroupField()) {
				continue;
			}

			// accodo in output campo elementare
			alDataItemCobolIdentifier.add(dataItemCobolIdentifier);
		}

		// Convero in array
		arDataItemCobolIdentifier = new DataItemCobolIdentifier[alDataItemCobolIdentifier.size()];
		arDataItemCobolIdentifier = alDataItemCobolIdentifier.toArray(arDataItemCobolIdentifier);

		return arDataItemCobolIdentifier;
	}
	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
	////////////////////////////////////////////////////////////////////////////////////////////

	
}

