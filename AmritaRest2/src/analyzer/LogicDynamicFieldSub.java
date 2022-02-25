package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import entities.EntityDynamicFieldSub;

/**
 *   Classe contenitore di servizio con la descrizione di un sottocampo
 *   elementare di un operando. <br>
 *   <p>
 *   L'analisi viene sempre ricondotta alle  trasformazioni dei sottocampi elementari. 
 *   Se l'operando non è un gruppo, viene in ogni caso inserito per omogeneità 
 *   di gestione in questa struttura come un sottocampo.  <br>
 *   <p>
 *   La struttura è utilizzata anche per le logiche spreaded, nei pgm
 *   chiamanti anche se si fa riferimento a questa istanza per il descrittore
 *   del sottocampo origine.
 *   <p>
 *   A questo scopo è presente una ArrayList di strutture chiamanti, ognuna relativa
 *   a una ultima assegnazione spreaded.<br>
 *   A loro volta ogni struttura può attivarne altre, ai vari livelli di chiamnte.<br>
 *   <p>
 *   In ogni caso qualsiasi assegnazione nei programmi chiamanti fa riferimento a
 *   posizione e lunghezza nel sottocampo origine.<br<
 *   <p>
 */
public class LogicDynamicFieldSub implements Serializable {
	
	private static final long serialVersionUID = 1L;

	// Descrittore operando sotto campo dinamico di istruzione
	public EntityDynamicFieldSub entityDynamicFieldSub = null;
	
	// Identificatore completo Cobol del sottocampo
	public DataItemCobolIdentifier dataItemIdentifierSubField = null;
	
	// Valore di default dal value formattato alla lunghezza del sottocampo
	public String valueDefault = "";
	
	// Valori assunti dal sottocampo nelle varie catene di trasformazione.
	// Ogni catena di trasformazione è supportata da almeno un path valido.
	// Valore di default verra incluso.
	// I valori memorizzati possono essere di porzioni del sottocampo.
	public ArrayList<LogicDynamicValue> al_value = null;

	// Valori completi assunti dal sottocampo nelle varie catene di trasformazione.
	// Si tratta dei valori possibili già in formato stringa, composti sulla base
	// di tutti i valori assunti dalle porzioni di sottocampo modifificate.
	// Il Valore di default verra incluso.
	public ArrayList<String> al_valueComplete = null;

	// Catene di trasformazioni effettuate dal sottocampo nel programma.
	// Ogni catena di trasformazione è supportata da almeno un path valido
	// I path validi vanno dalla prima istruzione del programma fino all'istruzione dinamica.
	// Nel caso di logiche dinamiche stesso programma si tratta del programma origine.
	// Nel caso di logiche spreaded su tratta di un programma chiamante.
	// Ogni elemento dell'array principale contiene una catena di assegnazioni.
	// Ogni elemento dell'array secondario contiene una trasformazione del sottocampo.
	// L'ultima trasformazione indica se il sottocampo è risolto o meno.
	public ArrayList<ArrayList<LogicDynamicFieldSubSetting>> al_al_chainSetSubField = null;
	
	// Paths a supporto delle catene di trasformazioni.
	// Ogni elemento dell'Array di Array di Integer è associato a un elemento di al_chainSetSubField
	// e indica i numeri di path che supportano la catena di trasformazioni, 0 elementi se nessun path le supporta.
	public ArrayList<Integer[]> al_chainSetPathNumCovering = null;
	
	// Copertura delle catene di trasformazioni in almeno un path di esecuzione.
	// Ogni elemento dell'Array di boolean è associato a un elemento di al_chainSetSubField
	// e indica se qualche path copre la catena di trasformazioni.
	public Boolean[] ar_chainSetFlagPathCovering = null;

	// Indicazione disabilitazione catena, a valle del processo ricorsivo.
	// Ogni elemento dell'Array di boolean è associato a un elemento di al_chainSetSubField
	// e indica se la catena di trasformazione è stata disabilitata dai controlli,
	// effettuati prima di ricomporre per moltiplicazione i valori finali dei campi.
	// Le cause possono essere per esempio valore assegnato overridato incondizionatamente
	// successivamente da altra catena o altro, valore di default impossibile etc..
	public Boolean[] ar_chainSetFlagDisabled = null;

	// Generazione valori sottocampo in catene di trasformazioni.
	// Ogni elemento dell'Array di boolean è associato a un elemento di al_chainSetSubField
	// e indica se esistono o meno valori per il sottocampo nella catena di trasformazioni.
	// Se non ci sono valori per la catena di trasformazioni significa che l'ultima 
	// trasformazione era da risolvere in programmi chiamanti oppure era in media esterni
	// (files, code cics, etc.) di cui non si è riusciti a individuare il valore.
	public Boolean[] ar_chainSetFlagValues = null;

	// Numero corrente catena trasformazioni in progress.
	public int numChainCur = 0;   	
	
	// SubField già memorizzato su db
	public boolean isDbUpdated = false;
	
	// Almeno una catena di trasformazione in un path possibile.
	// Verranno generati uno o più valori.
	public boolean anyChainSetFlagPathCovering = false;

	// Tutte le catene di trasformazione hanno generato valori.
	// Significa che il processo ha generato valori per ogni catena di trasformazioni
	// valida.
	public boolean allChainSetWithValues = false;
    
	// True indica sottocampo risolto.
	// Può essere localmente al programma origine o spreaded nei programmi chiamanti.
	// Sono stati generati dei valori per il sottocampo, memorizzati in al_value e al_valueComplete.
	public boolean isSolved = false;
	
	// True indica sottocampo risolto/da risolvere in programmi chiamanti..
	public boolean isSpreaded = false;
	
	// True indica sottocampo o qualche sua trasformazione,  in attesa di dati esterni.
	public boolean isWaitingForExternalData = false;

	// Ultime assegnazioni locali/spreaded, ancora da risolvere o meno cumulative per tutte le catene di trasformazione
	// Queste assegnazioni vengono generate a fronte di logiche locali o spreaded nei pgm chiamanti
	// Se per ogni ultima assegnazione sono stati trovati dei valori il flag lastSetSolved è a true.
	public ArrayList<LogicDynamicFieldSubSetting> al_lastSetTotal = null;

	// Di servizio per determinazione valori logiche spreaded.
	// Clone di al_value.
	public ArrayList<LogicDynamicValue> al_valueSaved = null;  // Struttura per salvataggio (via clone)
		
	/*
	 * Costruttore
	 */
	public LogicDynamicFieldSub(String system, String subSystem) {
		entityDynamicFieldSub = new EntityDynamicFieldSub();
		al_value = new ArrayList<LogicDynamicValue>  ();
		al_valueComplete = new ArrayList<String>  ();
		al_al_chainSetSubField = new ArrayList<ArrayList<LogicDynamicFieldSubSetting>> ();
		ar_chainSetFlagValues = new Boolean[0];
		ar_chainSetFlagPathCovering = new Boolean[0];
		al_chainSetPathNumCovering = new ArrayList<Integer[]> ();
		ar_chainSetFlagDisabled = new Boolean[0];
		al_lastSetTotal = new ArrayList<LogicDynamicFieldSubSetting> ();
	}
	
	/**
	 * Svuotamento ArrayList di lavoro per
	 * gestione catene di trasformazione sottocampo.
	 * 
	 */
	public void clearWorkingStructures()  {
		al_al_chainSetSubField.clear();
		ar_chainSetFlagValues = new Boolean[0];
		ar_chainSetFlagPathCovering = new Boolean[0];
		ar_chainSetFlagDisabled = new Boolean[0];
	}
	
}