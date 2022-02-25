package analyzer;
import java.util.ArrayList;

/**
 *   Classe contenitore di servizio con le informazioni su una singola
 *   catena di trasformazioni di un campo.<br>
 *   <p>
 *   Si tratta di insiemi di numeri istruzione di assegnazione, 
 *   di campi impostati e di posizioni in tali campi.
 *   Ciò a fronte di assegnazioni effettuate con MOVE, SET,
 *   istruzioni di I/O etc., fino a campi non più modificati.
 *   Le assegnazioni tengono conto di campi di gruppo, redefines
 *   e renames.
 */
public class LogicWorkTransformedDataItemChain  implements Cloneable{
	
	// Campo origine (in realtà, nel programma, il campo destinazione da cui inizia il processo
	InstructionCobolDataItem dataItemStart = null; 					// Campo di cui verificare le assegnazioni backward
	InstructionCobolDataItem dataItemFirstSet = null;               // Campo input in prima move di assegnazione, anche indiretto
	int dataItemFirstPosSet = 0;                                    // Posizione (1-based) di inizio in dataItemFirstSet
	                                                                // Si tratta dell'ultimo elemento di al_dataItemPosSet
	
	// Le seguenti ArrayList hanno lo stesso numero di elementi e viaggiano parallelamente
	ArrayList<InstructionCobolDataItem> al_dataItemSet = null;      // Campi assegnati dall'istruzione
	ArrayList<Integer> al_numInstrSet = null;                       // Numeri istruzioni di assegnazione (MOVE/SET/READ/...
	ArrayList<Integer> al_dataItemPosSet = null;                    // Posizione di inizio nel campo (1-based)
	ArrayList<Integer> al_dataItemLngSet = null;                    // Lunghezza utile nel campo  
	ArrayList<Integer> al_dataItemOriginPosInside = null;           // Posizione mappata sul dataItemStart  
	ArrayList<Integer> al_dataItemOriginLngInside = null;           // Lunghezza mappata sul dataItemStart  
	
	
	/*
	 * Costruttore
	 */
	public LogicWorkTransformedDataItemChain() {
		super();
		al_dataItemSet = new ArrayList<InstructionCobolDataItem> ();
		al_numInstrSet = new ArrayList<Integer> ();
		al_dataItemPosSet = new ArrayList<Integer> ();
		al_dataItemLngSet = new ArrayList<Integer> ();
		al_dataItemOriginPosInside = new ArrayList<Integer> ();
		al_dataItemOriginLngInside = new ArrayList<Integer> ();
	}


	@Override
	public String toString() {
		String toStrigOut = "";
		for (int i = 0; i < al_numInstrSet.size(); i++) {
			toStrigOut = toStrigOut + " " + al_numInstrSet.get(i).toString();
			toStrigOut = toStrigOut + " " + al_dataItemSet.get(i).getDataName();
			toStrigOut = toStrigOut + " " + al_dataItemPosSet.get(i).toString();
		}
		return toStrigOut;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@SuppressWarnings("unchecked")
	@Override
	public LogicWorkTransformedDataItemChain clone() {
		Object objectCloned = null;
		LogicWorkTransformedDataItemChain innerTransformedDataItemChainCloned = null;
		
		try {
			objectCloned = super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
		
		innerTransformedDataItemChainCloned = (LogicWorkTransformedDataItemChain) objectCloned;
		innerTransformedDataItemChainCloned.al_dataItemSet = (ArrayList<InstructionCobolDataItem>) innerTransformedDataItemChainCloned.al_dataItemSet.clone();
		innerTransformedDataItemChainCloned.al_numInstrSet = (ArrayList<Integer>) innerTransformedDataItemChainCloned.al_numInstrSet.clone();
		innerTransformedDataItemChainCloned.al_dataItemPosSet = (ArrayList<Integer>) innerTransformedDataItemChainCloned.al_dataItemPosSet.clone();
		innerTransformedDataItemChainCloned.al_dataItemLngSet = (ArrayList<Integer>) innerTransformedDataItemChainCloned.al_dataItemLngSet.clone();
		innerTransformedDataItemChainCloned.al_dataItemOriginPosInside = (ArrayList<Integer>) innerTransformedDataItemChainCloned.al_dataItemOriginPosInside.clone();
		innerTransformedDataItemChainCloned.al_dataItemOriginLngInside = (ArrayList<Integer>) innerTransformedDataItemChainCloned.al_dataItemOriginLngInside.clone();

		return innerTransformedDataItemChainCloned;
	}	
}