package analyzer;
import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumSymbolType;

/**
  * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
  * 
  * <h1>
  * DataItemQualifier  
  * </h1>
  *  <p>
  *
  * Viene descritto il tipo di symbolo e, nel caso di data item, tutte le informazioni sulle
  * modalità di utilizzo<br>
  * <p>
  * Si tratta di informazioni supplementari sull'utilizzo del data item, tipicamente Cobol, che vengono <br>
  * memorizzate all'interno di oggetti {@link Instruction}. In dettaglio si tratta di:<br>
  * <p>
  * 1) Qualificatore By Reference per campi di call Using <br>
  * 2) Qualificatore By Content   per campi di call Using<br>
  * 3) Qualificatore Of  per indicare l'appartenenza a una definizione di gruppo precisa<br>
  * 4) Qualificatore Length Of<br>
  * 5) Qualificatore Address Of<br>
  * 6) Indici utilizzati per il data item, nel caso si tratti di un elemento di tabella, senza limiti di espressione.<br>
  * 7) Posizione di inizio per reference modification, senza limiti di espressione<br>
  * 8) Lunghezza per reference modification, senza limiti di espressione<br>
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 25/04/2010
  * @see InstructionCobolDataItem
  * @see Analyzer
  *   
 */

public class DataItemQualifier  implements Serializable {

	private static final long serialVersionUID = 1L;
    
	// Valore simbolo. Può essre il nome del campo o il valore della literal 
	private String symbolValue = "";

	// Valore simbolo numerico. 
	// Valido in caso di literal numerica intera 
	private long symbolValueNumericLong = 0;

	// Valore simbolo numerico. 
	// Valido in caso di literal numerica con virgola o intera con numero di digit
	// insufficiente per int.
	private double symbolValueNumericDouble = 0;

	// Valore simbolo numerico. 
	// Valido in caso di literal numerica floating point.
	private float symbolValueNumericFloat = 0;

	// Tipologia simbolo 
	private EnumSymbolType symbolType = null;
	
	/////////////////////////////////////////////////////////////////////////////
	// Informazioni generali di qualificazione
	/////////////////////////////////////////////////////////////////////////////

	// Presenza qualificazioni campo
	private boolean byReference = false;					    // By Reference in Call Using
	private boolean byContent = false;					        // By Content   in Call Using
	private boolean byValue = false;					        // By Value     in Call Using
	private boolean addressOfDeclared = false;					// Address Of DataName
	private boolean lengthOfDeclared = false;        			// Length  Of DataName
	private boolean underGroupDeclared = false;         		// dataName Of GroupName
	private boolean roundedDeclared = false;         		    // Rounded in Comput

	// Primo campo di gruppo espresso in clausola OF/IN, situazione normale
	private String groupNameField = "";				   			// Campo di gruppo appartenenza del data item (OF/IN clausola)
	
	// Elenco completo campi OF dichiarati
	// Si tratta di situazioni a fronte di, per esempio, MOVE fld OF grpA OF grpB OF grpC (I*2 + 6)
	ArrayList<String> al_groupNameField = null;
	
	// Indici utilizzati per referenziare il data item.
	// Ogni indice viene memorizzato come un elenco di operandi e operatori.
	private ExpressionCobol ar_index[] = null;					// Es Move A(i1, i2, i3)(pos:lng) OF group-c To B
	
	// Ref modification: presenza qualificazioni successivi al campo 
	private boolean refModification = false;            		// True indica campo qualificato da (pos:lng)
	private ExpressionCobol pos = null;				   			// Pos nel data item come generica espressione Cobol
	private ExpressionCobol length = null;				   		// Lng nel data item come generica espressione Cobol

	
	
	/**
	 * 
	 * Costruttore vuoto
	 * 
	 */
	public DataItemQualifier( ) {
		super();
        symbolType = EnumSymbolType.NOT_ASSIGNED;
        al_groupNameField = new ArrayList<String> ();
 	}




	/**
	 * @return the symbolValue
	 */
	public String getSymbolValue() {
		return symbolValue;
	}




	/**
	 * @param symbolValue the symbolValue to set
	 */
	public void setSymbolValue(String symbolValue) {
		this.symbolValue = symbolValue;
	}





	/**
	 * Restituisce il valore numerico del data item quando questi è numerico.<br>
	 * <p>
	 * In particolare se il campo è numerico intero o con la virgola.
	 * 
	 * 
	 * @return the symbolValueNumericDouble
	 */
	public double getSymbolValueNumericDouble() {
		return symbolValueNumericDouble;
	}


	/**
	 * Restituisce il valore numerico del data item quando questi è numerico.<br>
	 * <p>
	 * In particolare se il campo è numerico intero.
	 * 
	 * 
	 * @return the symbolValueNumericLong
	 */
	public long getSymbolValueNumericLong() {
		return symbolValueNumericLong;
	}




	/**
     * Restituisce il valore numerico floating point del data item.<br>
	 * <p>
	 * 
     * @return the symbolValueNumericFloat
	 */
	public float getSymbolValueNumericFloat() {
		return symbolValueNumericFloat;
	}




	/**
     * Imposta il valore numerico floating point del data item.<br>
	 * <p>
	 * 
	 * @param symbolValueNumericFloat the symbolValueNumericFloat to set
	 */
	public void setSymbolValueNumericFloat(float symbolValueNumericFloat) {
		this.symbolValueNumericFloat = symbolValueNumericFloat;
	}




	/**
	 * Imposta il valore numerico del data item quando questi è con la virgiìola.<br>
	 * <p>
	 * 
	 * @param symbolValueNumericDouble the symbolValueNumericDouble to set
	 */
	public void setSymbolValueNumericDouble(double symbolValueNumericDouble) {
		this.symbolValueNumericDouble = symbolValueNumericDouble;
	}


	/**
	 * Imposta il valore numerico del data item quando questi è con la virgiìola.<br>
	 * <p>
	 * 
	 * @param symbolValueNumericLong the symbolValueNumericDouble to set
	 */
	public void setSymbolValueNumericLong(long symbolValueNumericLong) {
		this.symbolValueNumericLong = symbolValueNumericLong;
	}




	/**
	 * 
	 * Restituisce il campo di gruppo dichiarato dalla clausola OF
	 * sotto il quale il data item è definito.
	 * 
	 * @return the groupNameOwner
	 */
	public String getGroupNameField() {
		return groupNameField;
	}




	/**
	 * Restituisce i nomi di tutti i campi di qualificazione OF<br>
	 * <p>
	 * Sono supportate situaziioni quali:<br>
	 * <p>
	 * MOVE fiels OF grpA OF grpB OF grpC(i*3 + 23) TO ....<br>
	 * <p>
	 * @return the al_groupNameField
	 */
	public ArrayList<String> getGroupNameFields() {
		return al_groupNameField;
	}

	
	/**
	 * Imposta i nomi di tutti i campi di qualificazione OF<br>
	 * <p>
	 * Sono supportate situaziioni quali:<br>
	 * <p>
	 * MOVE fiels OF grpA OF grpB OF grpC(i*3 + 23) TO ....<br>
	 * <p>
	 * @param al_groupNameField the al_groupNameField to set
	 */
	public void setGroupNameFields(ArrayList<String> al_groupNameField) {
		this.al_groupNameField = al_groupNameField;
	}




	/**
	 * @return the symbolType
	 */
	public EnumSymbolType getSymbolType() {
		return symbolType;
	}




	/**
	 * @param symbolType the symbolType to set
	 */
	public void setSymbolType(EnumSymbolType symbolType) {
		this.symbolType = symbolType;
	}




	/**
	 * @param groupNameOwner the groupNameOwner to set
	 */
	public void setGroupNameOwner(String groupNameField) {
		this.groupNameField = groupNameField;
	}








	/**
	 * 
	 * Restituisce le espressioni dei campi indice utilizzati per accedere al data item.<br>
	 * <p>
	 * Se il data item non è indicizzato restituisce null o un array vuoto.
	 * 
	 * @return the ExpressionCobol[] ar_Index
	 */
	public ExpressionCobol[] getIndexes() {
		return ar_index;
	}




	/**
	 * 
	 * Imposta i campi indice utilizzati per accedere al data item.
	 * 
	 * @param ar_Index the ar_Index to set
	 */
	public void setIndexes(ExpressionCobol[] ar_index) {
		this.ar_index = ar_index;
	}




	/**
	 * @return the byReference
	 */
	public boolean isByReferenceDeclared() {
		return byReference;
	}




	/**
	 * @param byReference the byReference to set
	 */
	public void setByReferenceDeclared(boolean byReference) {
		this.byReference = byReference;
	}




	/**
	 * @return the byContent
	 */
	public boolean isByContentDeclared() {
		return byContent;
	}




	/**
	 * @param byContent the byContent to set
	 */
	public void setByContentDeclared(boolean byContent) {
		this.byContent = byContent;
	}




	/**
	 * @return the byValue
	 */
	public boolean isByValueDeclared() {
		return byValue;
	}




	/**
	 * @param byValue the byValue to set
	 */
	public void setByValueDeclared(boolean byValue) {
		this.byValue = byValue;
	}




	/**
	 * @return the addressOfDeclared
	 */
	public boolean isAddressOfDeclared() {
		return addressOfDeclared;
	}




	/**
	 * @param addressOfDeclared the addressOfDeclared to set
	 */
	public void setAddressOfDeclared(boolean addressOfDeclared) {
		this.addressOfDeclared = addressOfDeclared;
	}





	/**
	 * @return the underGroupDeclared
	 */
	public boolean isUnderGroupDeclared() {
		return underGroupDeclared;
	}



	/**
	 * Restituisce True se il campo è un elemento di tabella
	 * e sono specificati gli indici per recuperarne il valore.
	 * 
	 * @return true se il campo e qualificato da indici di tabella
	 */
	public boolean isOccursed() {
		
		// Condizioni di NON occurs
		if (this.ar_index == null) {
			return false;
		}
		if (this.ar_index.length == 0) {
			return false;
		}
		
		// campo elemento di tabella qualificato da uno o + indici
		
		return true;
	}




	/**
	 * @param underGroupDeclared the underGroupDeclared to set
	 */
	public void setUnderGroupDeclared(boolean underGroupDeclared) {
		this.underGroupDeclared = underGroupDeclared;
	}




	/**
	 * @return the roundedDeclared
	 */
	public boolean isRoundedDeclared() {
		return roundedDeclared;
	}




	/**
	 * @param roundedDeclared the roundedDeclared to set
	 */
	public void setRoundedDeclared(boolean roundedDeclared) {
		this.roundedDeclared = roundedDeclared;
	}




	/**
	 * @return the lengthOfDeclared
	 */
	public boolean isLengthOfDeclared() {
		return lengthOfDeclared;
	}




	/**
	 * @param lengthOfDeclared the lengthOfDeclared to set
	 */
	public void setLengthOfDeclared(boolean lengthOfDeclared) {
		this.lengthOfDeclared = lengthOfDeclared;
	}




	/**
	 * @return the refModification
	 */
	public boolean isThereRefModification() {
		return refModification;
	}




	/**
	 * @param refModification the refModification to set
	 */
	public void setRefModification(boolean refModification) {
		this.refModification = refModification;
	}










	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return this.symbolValue + " " + this.symbolType.toString();
	}




	/**
	 * 
	 * Restituisce l'espressione Cobol della posizione di reference modification.<br>
	 * <p>
	 * Si tratta di una espressione generale che può contenere campi, operatori e 
	 * registri speciali.
	 * 
	 * @return the pos
	 */
	public ExpressionCobol getPos() {
		return pos;
	}

	/**
	 * 
	 * Imposta l'espressione Cobol della posizione di reference modification.<br>
	 * <p>
	 * Si tratta di una espressione generale che può contenere campi, operatori e 
	 * registri speciali.
	 * 
	 * 
	 * @param pos the pos to set
	 */
	public void setPos(ExpressionCobol pos) {
		this.pos = pos;
	}




	/**
	 * 
	 * Restituisce l'espressione Cobol della lunghezza di reference modification.<br>
	 * <p>
	 * Si tratta di una espressione generale che può contenere campi, operatori e 
	 * registri speciali.
	 * 
	 * @return the length
	 */
	public ExpressionCobol getLength() {
		return length;
	}




	/**
	 * 
	 * Imposta l'espressione Cobol della lunghezza di reference modification.<br>
	 * <p>
	 * Si tratta di una espressione generale che può contenere campi, operatori e 
	 * registri speciali.
	 * 
	 * @param length the length to set
	 */
	public void setLength(ExpressionCobol length) {
		this.length = length;
	}




}
