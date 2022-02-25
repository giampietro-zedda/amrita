
package analyzer;
import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDescriptorPartition
 * </h1>
 * <p>
 * Descrive tutte le informazioni di una singola partizione di un indice o di una tabella Sql, a fronte di uno statement <br>
 * CREATE TABLE o CREATE INDEX.br>
 * <p>
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/lug/2011 
 * @see SqlDescriptorTable
 * @see SqlDescriptorTableColumn
 * @see SqlDescriptorIndex
 * @see SqlDescriptorIndexColumn
 *
*/

/**
 * @author Amrita
 *
 */
public class SqlPartitionValues implements Serializable {

 	private static final long serialVersionUID = 1L;
 	
 	// Informazioni di identificazione keys
	private int partitionNumber= 0; 							// PARTITION integer
	private boolean isInclusive = false;                        // PARTITION integer ENDING |AT ( ... ) |INCLUSIVE
	private ArrayList<String> al_endingAtValue = null;          // PARTITION integer ENDING |AT (literal|MAXVALUE|MINVALUE) |INCLUSIVE
	private int hashSpaceValue = 0;                             // HASH SPACE  int-value  
	private String hashSpaceType = "";                          // HASH SPACE  int-value K|M|G 
	
	// Informazioni di allocazione (valide per indici)
	private boolean isErase = false; 							// ERASE YES|NO
	private String usingVcat = "";                           	// USING VCAT catalog-name
	private String usingStogroup = "";                          // USING STOGROUP storage-group
	private String gbpCache = "";                           	// GBPCACHE CHANGED|ALL 
	private int priQty = 0;                           		  	// PRIQTY 	Primary quantity 
	private int seqQty = 0;                           		  	// SECQTY 	Secondary quantity 
	private int freePage = 0;                           		// FREEPAGE Free page 
	private int pctFree = 0;                           		  	// PCTFREE 	Free pct  

	
	/**
	 * Costruttore 
	 */
	public SqlPartitionValues() {
		super();
		al_endingAtValue = new ArrayList<String> ();
	}


	/**
	 * Restituisce il numero della partizione.<br>
	 * <p>
	 * @return the partitionNumber
	 */
	public int getPartitionNumber() {
		return partitionNumber;
	}


	/**
	 * Restituisce il valore espresso da <tt>HASH VALUE int-value K|M|G</tt><br>
	 * <p>
	 * @return the hashSpaceValue
	 */
	public int getHashSpaceValue() {
		return hashSpaceValue;
	}


	/**
	 * Imposta il valore espresso da <tt>HASH VALUE int-value K|M|G</tt><br>
	 * <p>
	 * @param hashSpaceValue the hashSpaceValue to set
	 */
	public void setHashSpaceValue(int hashSpaceValue) {
		this.hashSpaceValue = hashSpaceValue;
	}


	/**
	 * Restituisce il tipo espresso da <tt>HASH VALUE int-value K|M|G</tt><br>
	 * <p>
	 * @return the hashSpaceType
	 */
	public String getHashSpaceType() {
		return hashSpaceType;
	}


	/**
	 * Imposta il tipo espresso da <tt>HASH VALUE int-value K|M|G</tt><br>
	 * <p>
	 * @param hashSpaceType the hashSpaceType to set
	 */
	public void setHashSpaceType(String hashSpaceType) {
		this.hashSpaceType = hashSpaceType;
	}


	/**
	 * Imposta il numero della partizione.<br>
	 * <p>
	 * @param partitionNumber the partitionNumber to set
	 */
	public void setPartitionNumber(int partitionNumber) {
		this.partitionNumber = partitionNumber;
	}



	/**
	 * Restituisce true se presente la clausula INCLUSEVE.<br>
	 * <p>
	 * @return the isInclusive
	 */
	public boolean isInclusive() {
		return isInclusive;
	}


	/**
	 * Imposta se presente la clausula INCLUSEVE.<br>
	 * <p>
	 * @param isInclusive the isInclusive to set
	 */
	public void setInclusive(boolean isInclusive) {
		this.isInclusive = isInclusive;
	}



	/**
	 * Restituisce i valori ENDING AT presenti.<br>
	 * Ogni valore è abbinato alla colonna corrispondente.<br>
	 * <p>
	 * @return the al_endingAtValue
	 */
	public ArrayList<String> getEndingAtValues() {
		return al_endingAtValue;
	}


	/**
	 * Imposta i valori ENDING AT presenti.<br>
	 * Ogni valore è abbinato alla colonna corrispondente.<br>
	 * <p>
	 * @param al_endingAtValue the al_endingAtValue to set
	 */
	public void setAl_endingAtValue(ArrayList<String> al_endingAtValue) {
		this.al_endingAtValue = al_endingAtValue;
	}


	/**
	 * Restituisce il nome del catalogo indicato in
	 * <tt>USING VCAT catalog-name</tt><br> 
	 * <p>
	 * @return the usingVcat
	 */
	public String getUsingVcat() {
		return usingVcat;
	}


	/**
	 * Imposta il nome del catalogo indicato in
	 * <tt>USING VCAT catalog-name</tt><br> 
	 * <p>
	 * @param usingVcat the usingVcat to set
	 */
	public void setUsingVcat(String usingVcat) {
		this.usingVcat = usingVcat;
	}


	/**
	 * Restituisce il nome dello storage group indicato in
	 * <tt>USING STOGROUP storageGroup-name</tt><br> 
	 * <p>
	 * @return the usingStogroup
	 */
	public String getUsingStogroup() {
		return usingStogroup;
	}


	/**
	 * Imposta il nome dello storage group indicato in
	 * <tt>USING STOGROUP storageGroup-name</tt><br> 
	 * <p>
	 * @param usingStogroup the usingStogroup to set
	 */
	public void setUsingStogroup(String usingStogroup) {
		this.usingStogroup = usingStogroup;
	}

	/**
	 * Restituisce la quantità minima primaria di allocazione
	 * indicata in <tt>PRIQTY number</tt><br> 
	 * <p>
	 * @return the priQty
	 */
	public int getPriQty() {
		return priQty;
	}


	/**
	 * Imposta la quantità minima primaria di allocazione
	 * indicata in <tt>PRIQTY number</tt><br> 
	 * <p>
	 * @param priQty the priQty to set
	 */
	public void setPriQty(int priQty) {
		this.priQty = priQty;
	}


	/**
	 * Restituisce la quantità minima secondaria di allocazione
	 * indicata in <tt>SEQQTY number</tt><br> 
	 * <p>
	 * @return the seqQty
	 */
	public int getSeqQty() {
		return seqQty;
	}


	/**
	 * Imposta la quantità minima secondaria di allocazione
	 * indicata in <tt>SEQQTY number</tt><br> 
	 * <p>
	 * @param seqQty the seqQty to set
	 */
	public void setSeqQty(int seqQty) {
		this.seqQty = seqQty;
	}


	/**
	 * Restituisce la percentuale di spazio libero di pagina
	 * indicata in <tt>FREEPAGE number</tt><br> 
	 * <p>
	 * @return the freePage
	 */
	public int getFreePage() {
		return freePage;
	}


	/**
	 * Imposta la percentuale di spazio libero di pagina
	 * indicata in <tt>FREEPAGE number</tt><br> 
	 * <p>
	 * @param freePage the freePage to set
	 */
	public void setFreePage(int freePage) {
		this.freePage = freePage;
	}


	/**
	 * Restituisce la percentuale di spazio libero 
	 * indicata in <tt>PCTFREE number</tt><br> 
	 * <p>
	 * @return the pctFree
	 */
	public int getPctFree() {
		return pctFree;
	}


	/**
	 * Imposta la percentuale di spazio libero 
	 * indicata in <tt>PCTFREE number</tt><br> 
	 * <p>
	 * @param pctFree the pctFree to set
	 */
	public void setPctFree(int pctFree) {
		this.pctFree = pctFree;
	}

	/**
	 * Restituisce true se presente l'opzione  <tt>ERASE YES</tt> <br>
     * <p>
	 * @return the isErase
	 */
	public boolean isErase() {
		return isErase;
	}


	/**
	 * Imposta se presente l'opzione  <tt>ERASE YES|NO</tt> <br>
     * <p>
	 * @param isErase the isErase to set
	 */
	public void setErase(boolean isErase) {
		this.isErase = isErase;
	}

	/**
	 * Restituisce <tt>CHANGED</> o <tt>ALL</> come indicato <br>
	 * nel parametro GBPCACHE<br>
	 * <p>
	 * @return the gbpCache
	 */
	public String getGbpCache() {
		return gbpCache;
	}


	/**
	 * Imposta il parametro GBPCACHE<br>
	 * <p>
	 * @param gbpCache the gbpCache to set
	 */
	public void setGbpCache(String gbpCache) {
		this.gbpCache = gbpCache;
	}


}
