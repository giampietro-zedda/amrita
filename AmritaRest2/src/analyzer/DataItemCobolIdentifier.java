package analyzer;
import java.io.Serializable;
import enums.EnumCobolReservedWords;

/**
  * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
  * 
  * <h1>
  * DataItemCobolIdentifier
  * </h1>
  *  <p>
  *
  * Descrive un singolo identificatore Cobol utilizzato in un istruzione.<br>
  * <p>
  * Include un oggetto DataItemQualifier e altre informazioni, tra cui il numero di istruzione
  * di definizione dove l'identificatore (campo) è definito.
  * Questa classe viene utilizzata anche esternamente nell'analisi delle Exec Cics.
  * Viene condivisa tutta la gestione dell'analisi di un generico data item Cobol,
  * in quanto può essere specificato come parametro di una Exec Cics.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 31/05/2010
  * @see InstructionCobolDataItem
  * @see Analyzer
  *   
 */

public class DataItemCobolIdentifier  implements Serializable, Cloneable {

	private static final long serialVersionUID = 1L;
    
	private EnumCobolReservedWords identifierType = null;       // Tipologia (Data item, figurative, Literal, special register ..)
	private String nameIdentifier = "";                         // Nome campo o literal completa di apici o special register
	private InstructionCobolDataItem dataItem = null;           // Reference a DataItem se identifierType = DataItem
	private DataItemQualifier qualifier = null;                 // Tutte le informazioni di utilizzo (indici e reference modification)
    private int numInstr = 0;                                	// Numero istruzione di definizione nel programma (Data Division)
	
	
	/**
	 * 
	 * Costruttore vuoto
	 * 
	 */
	public DataItemCobolIdentifier( ) {
		super();
		qualifier = new DataItemQualifier();
	}



	/**
	 * @return the identifierType
	 */
	public EnumCobolReservedWords getIdentifierType() {
		return identifierType;
	}



	/**
	 * @param identifierType the identifierType to set
	 */
	public void setIdentifierType(EnumCobolReservedWords identifierType) {
		this.identifierType = identifierType;
	}



	/**
	 * @return the nameIdentifier
	 */
	public String getNameIdentifier() {
		return nameIdentifier;
	}



	/**
	 * Restituisce il nome dell'identificatore senza apici iniziali o finali,
	 * nel caso di literal alfanumerica.<br>
	 * Negli altri casi restituisce il nome dell'identificatore così come memorizzato.
	 * 
	 * @return the nameIdentifier
	 */
	public String getNameIdentifierFormatted() {
        
		String nameFormatted = "";
		
		if (this.identifierType == EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
			nameFormatted = this.nameIdentifier.substring(1);
			nameFormatted = nameFormatted.substring(0, nameFormatted.length() - 1);
			return nameFormatted;
		}
		
		return this.nameIdentifier;
	}




	/**
	 * @param nameIdentifier the nameIdentifier to set
	 */
	public void setNameIdentifier(String nameIdentifier) {
		this.nameIdentifier = nameIdentifier;
	}



	/**
	 * @return the qualifier
	 */
	public DataItemQualifier getQualifier() {
		return qualifier;
	}



	/**
	 * @param qualifier the qualifier to set
	 */
	public void setQualifier(DataItemQualifier qualifier) {
		this.qualifier = qualifier;
	}



	/**
	 * 
	 * Restituisce il numero di istruzione di definizione
	 * del data item.
	 * 
	 * @return the numInstrDef
	 */
	public int getNumInstr() {
		return numInstr;
	}

	/**
	 * 
	 * Restituisce il valore della stringa con gli apici.<b>
	 * <p>
	 * Il metodo ha senso se l'identificatore descrive una literal,
	 * 
	 * @return the valueString
	 */
	public String getValueString() {
		return nameIdentifier;
	}


	/**
	 * 
	 * Restituisce la stringa senza l'apice iniziale e finale.<b>
	 * <p>
	 * Il metodo ha senso se l'identificatore descrive una literal.
	 * 
	 * 
	 * @return the valueString
	 */
	public String getValueStringFormatted() {
		String valueFormatted = "";
		if (	this.identifierType == EnumCobolReservedWords.OPERAND_LITERAL_ALPHA) {
			valueFormatted = nameIdentifier.substring(1);
			valueFormatted = valueFormatted.substring(0, valueFormatted.length() - 1);
		}
		return valueFormatted;
	}



	/**
	 * 
	 * Imposta il numero di istruzione di definizione
	 * del data item.
	 * 
	 * @param numInstrDef the numInstrDef to set
	 */
	public void setNumInstr(int numInstr) {
		this.numInstr = numInstr;
	}



	/**
	 * 
	 * Restituisce il reference all'oggetto {@link InstructionCobolDataItem}
	 * che rappresenta il campo dell'identificatore.
	 * 
	 * @return the dataItem
	 */
	public InstructionCobolDataItem getDataItem() {
		return dataItem;
	}



	/**
	 * 
	 * Imposta il reference all'oggetto {@link InstructionCobolDataItem}
	 * che rappresenta il campo dell'identificatore.
	 * 
	 * @param dataItem the dataItem to set
	 */
	public void setDataItem(InstructionCobolDataItem dataItem) {
		this.dataItem = dataItem;
	}



	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return this.nameIdentifier.toString();
	}



	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected Object clone()  {
		try {
			return super.clone();
		} catch (Exception e) {
			return null;
		}
	}



	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		DataItemCobolIdentifier objIdentifier = null;
		objIdentifier = (DataItemCobolIdentifier) obj;

		// Numeri definizioni e nomi campi uguali
		if (objIdentifier.getDataItem() != null
		&&  objIdentifier.getDataItem().getNumInstr() == this.getDataItem().getNumInstr()
		&&  objIdentifier.nameIdentifier.equals(this.nameIdentifier)) {
			return true;
		}

		return false;
	}




}
