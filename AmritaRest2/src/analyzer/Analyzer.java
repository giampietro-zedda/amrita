package analyzer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

import enums.EnumCobolReservedWords;
import enums.EnumPrecompilerReservedWords;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * Analyzer  
 * </h1>
 * <p>
 * Questa classe modella le funzionalità comuni del processi di analisi dei sorgenti. <br>
 * <p>
 * Tali funzionalità includono il caricamento iniziale del sorgente (Cobol, CopyCobol, PL1, Jl, et.), la
 * definizioone delle strutture dati generiche e di ottimizzazione e tutti i metodi a supporto del processo di analisi.<br>
 *  <p>
 * <b>Le classi specifiche del processo di analisi di un determinato tipo di sorgente estendono questa classe.</b> <br>
 * <p>
 * Analyzer è progettato per ottenere la massima velocità di esecuzione, realizzata con ampio uso del
 * multithreading, l'ottimizzazione completamente configurata da parametri esterni codifiati nel file di
 * configurazione .config oltre alla più completa flessibilità di utilizzo. <br>
 * <p>
 * <b>Tutte logiche di analisi generalizzate e comuni a tutti i tipi di sorgente sono codificate in questa classe.</b> <br>
 * <p>
 * A fronte del processo di analisi vengono memorizzate informazioni serializzate che descrivono completamente l'oggetto
 * analizzato.
 * <p>
 * Nel caso di un programma, di qualsiasi linguaggio si tratti, vengono serializzate informazioni in files di tipo
 * (.PGC) e  (.GRF)	per codificare, rispettivamente, la rappresentazione interna complessiva del programma e la rappresentazione
 * delle sole istruzioni, come un grafo orientato generalizzzato.<br>
 * Le strutture di tipo include/copy vengono analizzate e serializzate separatamente in files di tipo (.CPC) e vengono quindi
 * incluse e attualizzate (potrebbero esserci dei replacyng by) direttamente nelle strutture di programma (.PGC)
 * <p>
 * Sono gestite le seguenti categorie di informazioni, serializzate nelle tipologie di files a margine.
 * <Ul>
 * <Li> (.program) Istruzioni codificate   			 
 * <Li> (.program) Definizioni campi dati elementari  
 * <Li> (.program) Definizione strutture dati come aggregati di campi elementari 
 * <Li> (.program) Symbol table di tutti i dati elementaari trattati 
 * <Li> (.program) Cross reference dato elementare/istruzione/nodo/arco 
 * <Li> (.graph) Grafo di programma con nodi e archi e riferimenti ai numeri di istruzione 
 * <Li> (.copy) Modulo copy di istruzioni procedurali 
 * <Li> (.copy) Modulo copy di definizioni di dati 
 * </Ul>
 * 
 * <p>
 * Sono gestite le seguenti funzionalità:
 * <Ul>
 * <Li> Gestione multithreading
 * <Li> Caricamento sorgente 
 * <Li> Gestione processi di analisi
 * <Li> Gestione Inserimento oggetti/relazioni su data base
 * <Li> Gestione logging informazioni iniziali
 * <Li> Gestione intercettazione e logging exception classi figlie speifiche
 * <Li> Gestione intercettazione e logging exception classi figlie speifiche
 * <Li> Metodi di interrogazione istruzioni generalizzate, risolte, da risolvere
 * </Ul>
 *   
 * <p>
 * Sono gestiti i seguenti tipi di processo di analisi, o direttamente in questa classe o attracerso le
 * classi figlie specializzate:
 * <Ul>
 * <Li> Analisi, codifica e serializzazione singolo sorgente  
 * <Li> Soluzione codice dinamico stesso programma
 * <Li> Soluzione codice dinamico fra programmi diversi (consolidamento)
 * <Li> Soluzione relazioni indirette
 * <Li> Soluzione relazioni aggregate di programmi facenti capo a una transazione scatenante
 * <Li> Soluzione istruzioni CICS
 * <Li> Soluzione istruzioni SQL  
 * <Li> Soluzione istruzioni DL/1
 * <Li> Soluzione istruzioni ADABAS
 * <Li> Intercettamento codice morto
 * <Li> Intercettamento stringhe e caricamento relazioni
 * <Li> Calcolo metriche di programma
 * </Ul>
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 15/02/2010
 * @see GraphManager
 * @see Instruction
*/

public class Analyzer extends ExecutionShared {
	
	public SourceInput si = null;
	

	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza generali e usate nei processi ricorsivi                                                                                           //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
	
    /**
	 * Costruttore  
	 */
	public Analyzer(UserConfiguration sd, ExecutionDirectives di) {
		super(sd, di);

	
	
	} 
	
	/**
	 * Composizione map pilot per parser individuazione istruzioni in sorgente<br>
	 * <p>
	 * Sulla base dei descrittori delle sequenze di chiave, viene<br>
	 * restituita una Map che mappa la prima parola chiave con tutte<br>
	 * le possibili sequenze di chiave valide che iniziano con la stessa.<br>
	 * <p>
	 * Insiemeall'ArrayList delle sequenze di chiave, deve essere fornito <br>
	 * anche il corrispondente ArrayList dei numeri ordinali dell'elemento <br>
	 * di enum che descrive la sequenza di chiave. Questo ordinale identifica <br>
	 * l'istruzione nella enumerazione di riferimento.<br>
	 * 
	 * Il primo elemento di ogni ArrayList di sequenze valide di chiave,<br>
	 * contiene il numero ordinale che l'ha generata, in modo che il <br>
	 * chiamante possa associare l'istruzione corretta alla sequenza di chiavi.<br>
	 * <p> 
	 *  
	 * Sequenze di input possono essere, per esempio:
	 * 
	 * 	"SET", "*", "CURRENT SERVER|CURRENT PACKAGESET" 
	 *  "SET 	                                    
	 *  "SET", "*", "CURRENT SERVER|CURRENT PACKAGESET" 
	 *  "SET", "*", "PATH|CURRENT_PATH|CURRENT PATH|CURRENT FUNCTION PATH", ""
	 *  
	 * Che generano sequenze valide mappate da SET quali:
	 *  
	 *  "SET 
	 *  "SET", "*", "CURRENT" "SERVER" 
	 *  "SET", "*", "CURRENT" "PACKAGESET" 
	 *  "SET", "PATH" 
	 *  "SET", "CURRENT_PATH" 
	 *  "SET", "CURRENT", "PATH" 
	 *  "SET", "CURRENT", "FUNCTION", "PATH" 
	 * 
	 * Ognuna delle quali identifica univocamente un'istruzione.<br>
	 * <p>
	 * Gli utilizzatori di questo metodo ottengono le sequenze di keys descriptor
	 * dalle specifiche enumerazioni dei vari linguaggi come {@link EnumPrecompilerReservedWords}
	 * {@link EnumCobolReservedWords} etc.
	 * 
	 * 
	 */
	public Map<String, ArrayList<ArrayList<String>>> getParserSequenceKeysList(ArrayList<ArrayList<String>> al_al_sequenceKeysDescriptor
			                                                                 , ArrayList<Integer> al_ordinalEnumSequenceKeys) {

		Map<String, ArrayList<ArrayList<String>>> map_output = null;
		
		ArrayList<String> al_sequenceKeysDescriptor = null;
		ArrayList<ArrayList<String>> al_al_map_output = null;
		ArrayList<ArrayList<String>> al_al_key = null;     							// Insiemi di sequenze valide di chiavi
		ArrayList<String> al_keyWork = null;
		String keyMap = "";                                                         //
		int ordinalEnumSequenceKeys = 0;
		
		map_output = new HashMap<String, ArrayList<ArrayList<String>>> ();
					
		// Scan elenchi di sequenze di key words descrittori
		for (int i = 0; i < al_al_sequenceKeysDescriptor.size(); i++) {
			
			al_sequenceKeysDescriptor = al_al_sequenceKeysDescriptor.get(i);
			keyMap = al_sequenceKeysDescriptor.get(0);
			ordinalEnumSequenceKeys = al_ordinalEnumSequenceKeys.get(i);
			
			al_al_map_output = map_output.get(keyMap);
			if (al_al_map_output == null) {
				al_al_map_output = new ArrayList<ArrayList<String>> ();
				map_output.put(keyMap, al_al_map_output);
			}
			
			// Estrazione ricorsiva di tutte le possibili sequenze valide di chiave  
			al_al_key = new ArrayList<ArrayList<String>> ();
			al_keyWork = new ArrayList<String> ();
			al_keyWork.add(ordinalEnumSequenceKeys+"");						// Il primo elemento contiene il numero ordinale origine
			getParserSequenceKeysListRecursive(al_sequenceKeysDescriptor, al_al_key, al_keyWork, 0, ordinalEnumSequenceKeys);
				
			al_al_map_output.addAll(al_al_key);	
		}

		return map_output;
	}

	/*
	 * Produce ricorsivamente le sequenze di chiave possibili
	 * 
	 */
	@SuppressWarnings("unchecked")
	private void getParserSequenceKeysListRecursive(ArrayList<String> al_sequenceKeysDescriptor
												  , ArrayList<ArrayList<String>> al_al_key
												  , ArrayList<String> al_keyWork
												  , int iKeyStart
												  , int ordinalEnumSequenceKeys
												   ) {
		
		 ArrayList<String> al_keyWorkSaved = null;
		 String[] ar_string = null;
		 String sequenceKey = "";
		 String sequencePositional = "";
		 Scanner scn = null;
		 int iSeparator = 0;
        
		 
		 // Scan key descriptor list a partire dalla chiave indicate
		 for (int i = iKeyStart; i < al_sequenceKeysDescriptor.size(); i++) {

			 sequenceKey = al_sequenceKeysDescriptor.get(i);
			 iSeparator = sequenceKey.indexOf("|");
			 
			 // key da portare in output sicuramente (valore key atomico oppure *)
			 if (iSeparator < 0) {
				 al_keyWork.add(sequenceKey);
				 continue;
			}
			
			// Key words alternative
			ar_string = sequenceKey.split("\\|");
			 
			// Scan key words alternative da inserire
			for (String sequenceKeyAlt : ar_string) {
				
				al_keyWorkSaved = (ArrayList<String>) al_keyWork.clone();
				
				// Key opzionale (dovrebbe essere la prima key alternativa)
				if (sequenceKeyAlt.trim().equals("")) {
					getParserSequenceKeysListRecursive(al_sequenceKeysDescriptor, al_al_key, (ArrayList<String>) al_keyWork.clone(), i + 1, ordinalEnumSequenceKeys);
					al_keyWork = al_keyWorkSaved;
					continue;
				}
				
				// Key singola
				if (sequenceKeyAlt.indexOf(" ") < 0) {
					al_keyWork.add(sequenceKeyAlt);
					getParserSequenceKeysListRecursive(al_sequenceKeysDescriptor, al_al_key, (ArrayList<String>) al_keyWork.clone(), i + 1, ordinalEnumSequenceKeys);
					al_keyWork = al_keyWorkSaved;
					continue;
				}
				
				// Key multiple separate da spazio
				scn = new Scanner(sequenceKeyAlt);
				while (scn.hasNext()) {
					
					sequencePositional = scn.next();
					al_keyWork.add(sequencePositional);
					
				} // end-while chiavi posizionali dentro key alternativa
				getParserSequenceKeysListRecursive(al_sequenceKeysDescriptor, al_al_key, (ArrayList<String>) al_keyWork.clone(), i + 1, ordinalEnumSequenceKeys);
				al_keyWork = al_keyWorkSaved;
				
			} // end-for keys alternative
			
		 } // end key descriptor list
		 
		// Fine sequenza di chiavi: accodamento in array generale di output
		al_al_key.add(al_keyWork);
		
		return;
	}
	

	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

	
}
