package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Set;
import java.util.Map.Entry;
import enums.EnumSymbolType;
import enums.EnumObject;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
  * 
 * <h1>
 * ScriptSql 
 * </h1>
 *  <p>
 * Questa classe  modella un generico script contenente istruzioni e commands DB2 Sql.<br>
 * Tutte le caratteristiche e le gestioni comuni a tutti i linguaggi, tra cui la catalogazioe e archiviazione
 * dei simboli, sono modellate dalla classe madre Program. Lo script viene trattao a tutti gli effetti come un programma<br>
 * 
 *
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/04/2010
 * @see Program
 * @see DataItem
 * @see Analyzer
 * @see AnalyzerCobol
 * @see AnalyzerCobolCopyDataDivision
 * @see AnalyzerCobolCopyProcedure
 * @see AnalyzerCobolProgram
 *   
 */

public class ScriptSql extends Program implements Serializable, AmritaConstants {

	private static final long serialVersionUID = 1L;

  	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza specifiche per script Sql                                                     //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    
	// Struttura con tutte le istruzioni di procedure division del programma. 
 	private ArrayList<ScriptSqlEntry> al_scriptEntry = null;			 
	
    // Metriche associate allo script
	private Metrics metrics = null;

	/**
	 *  Costruttore 1
	 */
	public ScriptSql(UserConfiguration sd) {
		super(sd, "");
		this.programType = EnumObject.OBJECT_SQL_SCRIPT;
		al_scriptEntry = new ArrayList<ScriptSqlEntry> ();
	}

	
    /**
	 *  Costruttore 2
	 */
	public ScriptSql(UserConfiguration sd, String scriptName) {
		super(sd, scriptName);
		this.programType = EnumObject.OBJECT_SQL_SCRIPT;
		al_scriptEntry = new ArrayList<ScriptSqlEntry> ();

	}
 	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////                     Metodi pubblici                   /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////



	/**
	 * Restituisce il nome dello script Sql corrente.<br>
	 * <p>
	 * @return the scriptName
	 */
	public String getScriptName() {
		return this.programName;					// In calsse madre Program, da cui eredita
	}


	/**
	 * Impostail nome dello script Sql corrente.<br>
	 * <p>
	 * @param scriptName the scriptName to set
	 */
	public void setScriptName(String scriptName) {
		this.programName = scriptName;
	}



	/**
	 * 
	 * Inserisce un entry di definizione istruzione dello script<br> 
	 * <p>
	 * @param ProgramCobolEntry dataInstructionEntry
     * @return Int pointer a elemento inserito
	 * 
	 */
 	public int addEntryScript (ScriptSqlEntry scriptSqlEntry){
 		al_scriptEntry.add(scriptSqlEntry);
    	return al_scriptEntry.size()- 1;
    }

 

	/**
	 * 
	 * Restituisce tutte le istruzioni dello script.<br> 
	 * <p>
	 * @return ProgramCobolEntry<?>[]
	 * 
	 */
	public ScriptSqlEntry[] entriesScript (){
 		ScriptSqlEntry ar_ScriptSqlEntry[] = null;
 		ar_ScriptSqlEntry = new ScriptSqlEntry[al_scriptEntry.size()];
 		ar_ScriptSqlEntry = al_scriptEntry.toArray(ar_ScriptSqlEntry);
    	return ar_ScriptSqlEntry;
 	}

 	/**
	 * 
	 * Operazioni finali di consolidamento delle strutture interne.<br> 
	 * Gli ArrayList con le definizioni dati e istruzioni vengono trimmati alle dimensioni effettive<br>
	 * <p>
	 * 
	 */
 	public void optimize(){
 		
     	al_scriptEntry.trimToSize();
   		return;
    }


	
  

	/**
	 * 
	 * Restituisce i puntatori alle istruzioni che referenziano una label di uno script Sql<br> 
	 * <p>
	 * Coincide con il numeri delle istruzioni dello script.<br>
	 * Se la label non è referenziata restituisce null.<br>
	 * <p>
	 * @param String label name
	 * @return int[] con il numero di istruzione che richiama la label
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] xrefToLabel(String labelName){
 		
		Object ar_o[] = null;
		EnumSymbolType symbolType = null; 
 		int ar_XrefInputProcedure[] = null;
 		ArrayList<Integer> al_XrefInputProcedure = null;
 	    ArrayList<Integer> al_InstructionEntry = null;
 		
 		ar_o = map_Symbol.get(labelName); 
 		
		// Verifica se la section è definito come simbolo
 		if (ar_o == null) {
			return null;
		}
 
 		symbolType = (EnumSymbolType) ar_o[0];
 		
 		// Verifica se il simbolo è una label Sql
 		if (symbolType != EnumSymbolType.SQL_SYMBOL_LABEL) {
			return null;
		}

 		al_InstructionEntry = (ArrayList<Integer>) ar_o[2];
 		
 		// Verifica se la section è definita come definizione
		if (al_InstructionEntry.size() == 0) {
			return null;
		}
	 
		al_XrefInputProcedure =  (ArrayList<Integer>) ar_o[3];
        
		// Converto in array semplice di interi
		ar_XrefInputProcedure = new int[al_XrefInputProcedure.size()];
		for (int i = 0; i < ar_XrefInputProcedure.length; i++) {
			ar_XrefInputProcedure[i] = al_XrefInputProcedure.get(i);
		}
 		
     	return ar_XrefInputProcedure;
    }

 

 

 




 	/**
	 * 
	 * Restituisce tutte le label definite nel programma<br> 
	 * <p>
	 * @return String[] con i nomi delle label definite
	 * 
	 */
 	public String[] labelNames(){
 		
		Object ar_o[] = null;                                   // Mappato da ogni simbolo
		EnumSymbolType symbolType = null;    					// Da elemento (0) mappato in Map simboli
 		Set<Entry<String, Object[]>> set_EntrySymbol = null;	// Entry elemento map simboli
		String ar_LabelOut[] = null;							// Array di output con label non referenziate
		ArrayList<String> al_LabelOut = null;					// Di servizio
        
		al_LabelOut = new ArrayList<String>();
		
		// Set di coppie simbolo/Object[]
 		set_EntrySymbol = map_Symbol.entrySet();
 		
 		// Scan simboli definiti
 		for (Entry<String, Object[]> entry : set_EntrySymbol) {
			
 			// Array di oggetti associati al simbolo
 			ar_o = entry.getValue();
 			
			// Tipologia simbolo nel primo elemento
 			symbolType = (EnumSymbolType) ar_o[0];
	 		
	 		// Label: porto in outout
	 		if (symbolType == EnumSymbolType.SQL_SYMBOL_LABEL) {
	 			al_LabelOut.add(entry.getKey().trim());
				continue;
			}
		}
 		
		// Converto in array semplice di Stringhe
 		ar_LabelOut = new String[al_LabelOut.size()];
 		ar_LabelOut = al_LabelOut.toArray(ar_LabelOut);
 		
     	return ar_LabelOut;
    }



	/**
	 * 
	 * Restituisce i nomi dei simboli non label definiti nel programma <br> 
	 * <p>
	 * Si tratta di nomi di campi, literal, costanti figurative etc.
	 * @return String[] con i nomi delle section definite nel programma
	 * 
	 */
 	public String[] symbolNames(){
 		
		Object ar_o[] = null;                                   // Mappato da ogni simbolo
		EnumSymbolType symbolType = null;    					// Da elemento (0) mappato in Map simboli
 		Set<Entry<String, Object[]>> set_EntrySymbol = null;	// Entry elemento map simboli
		String ar_SymbolOut[] = null;							// Array di output con label non referenziate
		ArrayList<String> al_SymbolOut = null;				    // Di servizio
        
		al_SymbolOut = new ArrayList<String>();
		
		// Set di coppie simbolo/Object[]
 		set_EntrySymbol = map_Symbol.entrySet();
 		  
 		// Scan simboli definiti
 		for (Entry<String, Object[]> entry : set_EntrySymbol) {
			
 			// Array di oggetti associati al simbolo
 			ar_o = entry.getValue();
 			
			// Tipologia simbolo nel primo elemento
 			symbolType = (EnumSymbolType) ar_o[0];
	 		
	 		// Section: porto in outout
	 		if (symbolType != EnumSymbolType.SQL_SYMBOL_LABEL) {
	 			al_SymbolOut.add(entry.getKey());
				continue;
			}
		}
 		
		// Converto in array semplice di Stringhe
 		ar_SymbolOut = new String[al_SymbolOut.size()];
 		ar_SymbolOut = al_SymbolOut.toArray(ar_SymbolOut);
 		
     	return ar_SymbolOut;
    }


 	/**
	 * 
	 * Restituisce la definizione completa dell' entry nello script<br> 
	 * <p>
	 * Coincide on il numero dell'istruzione.<br>
	 * Se il pointer fornito è errato restituisce null.<br>
	 * <p>
	 * @param int instructionPointer
	 * @return ProgramCobolEntry con la definizione completa dell'entry
	 * 
	 */
 	public ScriptSqlEntry entryScript (int instructionPointer){
 		
 		ScriptSqlEntry entryScript = null;
 		
  		// Istruzione richiesta fuori range 
 		if (instructionPointer >= al_scriptEntry.size()) {
			return null;
		}
 		
 		entryScript = al_scriptEntry.get(instructionPointer);
 		 		
 	   	return entryScript;
    }


	/**
	 * 
	 * Restituisce l'istruzione nello script, memorizzata nell'entry del quale è fornito il numero<br>
	 * <p> 
	 * Il chiamante deve effettuare il casting all'oggetto istruzione corretto
	 * Se il pointer fornito è errato restituisce null.
	 * <p>
	 * @param int definitionPointer
	 * @return Object con istruzione di procedure division memorizzata nell'entry
	 * 
	 */
	public InstructionSql entryScriptInstruction (int instructionPointer){
 		
		ScriptSqlEntry entryInstrSql = null;
		InstructionSql instrSql = null;
		
 		// Richista out of ranged
 		if (instructionPointer >= al_scriptEntry.size()) {
			return null; 
		}
 		
 		entryInstrSql = al_scriptEntry.get(instructionPointer);
 		instrSql = entryInstrSql.getInstruction();
	
 	   	return instrSql;
    }
 	
	/**
	 * Restituisce le metriche aassociate  allo script
	 * come istanza dell'oggetto {@link Metrics}
	 * 
	 * @return the metrics
	 */
	public Metrics getMetrics() {
		return metrics;
	}


	/**
     * Imposta le metriche aassociate  allo script
	 * come istanza dell'oggetto {@link Metrics}
	 * 
      * @param metrics the metrics to set
	 */
	public void setMetrics(Metrics metrics) {
		this.metrics = metrics;
	}


 
 	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Metodi privati                                        /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

 	

	@Override
	public String toString() {
		return "Script:" + this.programName;
	}

	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

}
