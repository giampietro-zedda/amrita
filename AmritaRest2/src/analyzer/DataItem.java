package analyzer;
import java.io.Serializable;

import enums.EnumDataItemGeneric;

/**
  * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda  Turin (ITALY)
  * 
 * <h1>
 * DataItem  
 * </h1>
 *  <p>
 * Questa classe descrive un generico attributo, data item, elementare. Nella forma più generica
 * un data item rappresenta una informazione numerica o testuale e viene qualificata da un nome 
 * e un valore iniziale.
 * Le classi figlie che ereditano da questa classe dettaglieranno, come per esemopio per il Cobol,
 * le specifiche caratteristiche quali il numero di livello, gli attributi, la picture e altro.<br>
 * Qualunque data item, tuttavia, è inserito in un sorgente, con dei commenti eventuali e si può considerare
 * come una generica istruzione di definizione dati. Pertando questa classe eredita dalla classe {@link Instruction}
 * che gestisce le problematiche di collegamento con il sorgente e la memorizzazione dei commenti. 
 *
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 15/02/2010
 * @see InstructionCobolDataItem
 * @see Analyzer
 *   
 */

public class DataItem extends InstructionCobol implements Serializable {

	private static final long serialVersionUID = 1L;

	
	/////////////////////////////////////////////////////////////////////////////
	// Informazioni generali del data item
	/////////////////////////////////////////////////////////////////////////////

	/**
	 * 
	 */
	private String dataName = "";				   			// Nome campo 
	private EnumDataItemGeneric en_GenericType = null; 		// Tipologia campo generica, non specializzata per linguaggio
	
    // Formato, dimensioni, interi e decimali campo
	private int sizeBytes = 0;                      		// Dimensioni campo in caratteri se alfanumerico
	private int numInt = 0;                         		// Numero interi definiti o massimi se campo numerico
	private int numDec = 0;                         		// Numero decimali definiti se campo numerico

	
	
	/**
	 * Costruttore
	 */
	public DataItem(String dataName) {
		this.dataName = dataName;
		this.en_GenericType = EnumDataItemGeneric.NOT_ASSIGNED;
	}



	/**
	 * Restituisce il nome del campo.<br>
	 * <p>
	 * 
	 * @return the dataName
	 */
	public String getDataName() {
		return dataName;
	}



	/**
	 * Imposta il nome del campo.<br>
	 * <p>
	 * 
	 * @param dataName the dataName to set
	 */
	public void setDataName(String dataName) {
		this.dataName = dataName;
	}



	/**
	 * @return the genericType
	 */
	public EnumDataItemGeneric getGenericType() {
		return en_GenericType;
	}



	/**
	 * @param genericType the genericType to set
	 */
	public void setGenericType(EnumDataItemGeneric en_GenericType) {
		this.en_GenericType = en_GenericType;
	}



	/**
	 * @return the sizeBytes
	 */
	public int getSizeBytes() {
		return sizeBytes;
	}



	/**
	 * @param sizeBytes the sizeBytes to set
	 */
	public void setSizeBytes(int sizeBytes) {
		this.sizeBytes = sizeBytes;
	}



	/**
	 * @return the numInt
	 */
	public int getNumInt() {
		return numInt;
	}



	/**
	 * @param numInt the numInt to set
	 */
	public void setNumInt(int numInt) {
		this.numInt = numInt;
	}



	/**
	 * @return the numDec
	 */
	public int getNumDec() {
		return numDec;
	}



	/**
	 * @param numDec the numDec to set
	 */
	public void setNumDec(int numDec) {
		this.numDec = numDec;
	}

}
