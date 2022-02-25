package analyzer;

import java.io.Serializable;
import entities.EntityDynamicFieldSubValue;

/**
 *   Classe contenitore di servizio con il valore di un sottocampo o di
 *   un campo dinamico di una istruzione.
 */
public class LogicDynamicValue implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	public EntityDynamicFieldSubValue entityDynamicValue = null;       // Struttura pronta per l'inserimento su db
	public boolean isDbUpdated = false;                                // Valore già memorizzato su db
	public boolean isDefaulValue = false;                              // True indica valore da Value e non a fronte di catena di trasformazione    
	public boolean isFieldValueFull = false;                           // True indica valore completo relativo al campo   
	public String value = "";											// Valore campo o sottocampo
	public String pgmSet = "";                                         // Programma di assegnazione del valore	
	public int numChainSet = 0;                                        // Numero catena di assegnazione nel programma di assegnazione
	public int numInstrSet = 0;                                        // Numero istruzione che ha generato il valore nel programma corrente
	
	// Info supplementari sull'origine del valore, recuperate dall'ultima assegnazione che lo ha generato
	public int lvlPgmSet = 0;                                          // Livello programma di assegnazione valore (0=programma origine)
	                                                            	   // Viaggiano in coppia

	/*
	 * Costruttore
	 */
	public LogicDynamicValue(String system, String subSystem) {
		entityDynamicValue = new EntityDynamicFieldSubValue();
		entityDynamicValue.setSystem(system);
		entityDynamicValue.setSubSystem(subSystem);
	}

	@Override
	public String toString() {
		return "Value:" + entityDynamicValue.getValue() + " " + lvlPgmSet + " ";
	}

	
}