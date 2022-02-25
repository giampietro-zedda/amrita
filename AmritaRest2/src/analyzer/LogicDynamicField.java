package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import entities.EntityDynamicField;

/**
 *   Classe contenitore di servizio con la descrizione di un operando
 *   dinamico (campo) di una istruzione.
 *   
 *   I campi sono pubblici senza getter and setter per semplicità di utilizzo.
 *   
 *   Vengono modellati anche tutti i sottocampi con i rispettivi valori
 *   attraverso la classe {@link LogicDynamicFieldSub} e {@link LogicDynamicValue}.
 *   {@link LogicDynamicFieldSub} modella tutte le informazioni utili nel processo
 *   di determinazione dei valori del sottocampo.
 *   Sono disponibili i metodi di servizio per ottenere i valori del campo,
 *   i sottocampi e i valori dei sottocampi. 
 *   
 */
public class LogicDynamicField implements Serializable {
    
	private static final long serialVersionUID = 1L;

	// Struttura db campo dinamico di istruzione
	public EntityDynamicField entityDynamicField = null;
	
	// Identificatore completo Cobol (per comodità)
	public DataItemCobolIdentifier dataItemCobolIdentifier = null;
	
	// Sottocampi dell'operando dinamico da risolvere/risolti
	public ArrayList<LogicDynamicFieldSub> al_FieldSub = null;

	// Valori assunti dal campo.
	public ArrayList<String> al_valuesField = null;
	
	// Valore già memorizzato su db
	public boolean isDbUpdated = false;                                

	/*
	 * Costruttore
	 */
	public LogicDynamicField(String system, String subSystem) {
		entityDynamicField = new EntityDynamicField();
		entityDynamicField.setSystem(system);
		entityDynamicField.setSubSystem(subSystem);
		al_FieldSub = new ArrayList<LogicDynamicFieldSub>();
		al_valuesField = new ArrayList<String> ();
	}
	
    /*
     * Restituisce il sottocampo elementare del campo dinamico.
     * 
     * @param subFieldName
     * @return LogicDynamicFieldSub 
     */
	public LogicDynamicFieldSub getDynamicFieldSub(String subFieldName) {
		
		for (LogicDynamicFieldSub logicDynamicFieldSub : al_FieldSub) {
			if (logicDynamicFieldSub.dataItemIdentifierSubField.getDataItem().getDataName().equals(subFieldName)) {
				return logicDynamicFieldSub;
			}
		}		
		return null;
	}

    /*
     * Restituisce i valori del campo dinamico pronti per inserimento su db.
     * 
     * @return ArrayList<String> 
     */
	public ArrayList<String> getValuesField() {
		return al_valuesField;
	}

	

    /*
     * Restituisce i valori del sottocampo elementare del campo dinamico.
     * 
     * @param subFieldName
     * @return ArrayList<LogicDynamicValue> 
     */
	public ArrayList<LogicDynamicValue> getDynamicFieldSubValue(String subFieldName) {
		
		for (LogicDynamicFieldSub logicDynamicFieldSub : al_FieldSub) {
			if (logicDynamicFieldSub.dataItemIdentifierSubField.getDataItem().getDataName().equals(subFieldName)) {
				return logicDynamicFieldSub.al_value;
			}
		}		
		return null;
	}

	
	@Override
	public String toString() {
		return "Field:"	+ dataItemCobolIdentifier.getNameIdentifier();
	}



	
}

