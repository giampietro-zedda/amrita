
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDescriptorIndex
 * </h1>
 * <p>
 * Descrive tutte le informazioni di un tabella Sql, a fronte di uno statement CREATE INDEX.br>
 * <p>
 * 
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 11/lug/2011 
 * @see SqlDescriptorTable
 * @see SqlDescriptorTableColumn
 * @see SqlDescriptorIndexColumn
*/

/**
 * @author Amrita
 *
 */
public class Sqlndex implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////////
    // Caratteristiche generali indice
    //////////////////////////////////////////////////////////////////////////
    
	// Valori parametri numerici o alfanumerici
	private String indexFullName = "";                           		  			// Nome completo qualificato indice
	private String indexName = "";                           		  			    // Nome applicativo indice
	private String indexOwner = "";                           		  			    // Nome owner indice se qualificato su 2 campi
	private String tableFullName = "";                           		  			// Nome completo qualificato
	private String tableOwner = "";                           		  			    // Nome owner se qualificazione su 3 o 2 campi
	private String tableName = "";                           		  			    // Nome applicativo tabella 
	private String usingVcat = "";                           		  			    // USING VCAT catalog-name
	private String usingStogroup = "";                           		  			// USING STOGROUP storage-group
	private String bufferPool = "";                           		  			    // BUFFERPOOL
	private String pieceSizeType = "";                           		  			// PIECESIZE integer K|M|G	 
	private String gbpCache = "";                           		  			    // CHANGED|ALL 
	private int pieceSizeValue = 0;                           		  			    // PIECESIZE integer 	 
	private int priQty = 0;                           		  			            // PRIQTY 	Primary quantity 
	private int secQty = 0;                           		  			            // SECQTY 	Secondary quantity 
	private int freePage = 0;                           		  			        // FREEPAGE Free page 
	private int pctFree = 0;                           		  			            // PCTFREE 	Free pct  
	
	// Opzioni parm-name YES/NO o |NOT parm-name
	private boolean isBusinessTimeWithoutOverlaps = false;                          // BUSINESS_TIME WITHOUT OVERLAPS
	private boolean isPartitioned = false;                                          // PARTITIONED 
	private boolean isUnique = false;                                               // UNIQUE   
	private boolean isUniqueWhereNotNull = false;                                   // UNIQUE   WHERE NOT NULL
	private boolean isCluster = false;                                              // |NOT CLUSTER  
	private boolean isPadded = false;                                               // |NOT PADDED 
	private boolean isDefine = false;                                               // DEFINE 	YES|NO
	private boolean isDefer = false;                                                // DEFER 	YES|NO
	private boolean isCompress = false;                                             // COMPRESS	YES|NO
	private boolean isErase = false;                                                // ERASE 	YES|NO
	private boolean isCopy = false;                                             	// COPY   	YES|NO
	private boolean isClose = false;                                                // CLOSE 	YES|NO
	
	// Colonne indice con il proprio descrittore
	private ArrayList<SqlIndexColumn> al_column = null;                   // Colonne indice 

	// Partizioni
	private SqlPartitions partitions = null;                              // PARTITION BY RANGE ... PARTITION 1 ENDING AT (MINVALUE'), ...

	
	/**
	 * Costruttore vuoto
	 */
	public Sqlndex() {
		super();
		al_column = new  ArrayList<SqlIndexColumn> ();

	}


	/**
	 * Restituisce il nome qualificato della tabella,
	 * come <tt>serverId.Owner.tableName</tt>
	 * 
	 * @return the tableFullName
	 */
	public String getTableFullName() {
		return tableFullName;
	}


	/**
	 * Imposta il nome qualificato della tabella,
	 * come <tt>serverId.Owner.tableName</tt>
	 * 
	 * @param tableFullName the tableFullName to set
	 */
	public void setTableFullName(String tableFullName) {
		this.tableFullName = tableFullName;
	}


	/**
	 * Restituisce l'owner della tabella, a fronte
	 * di <tt>serverId.Owner.tableName</tt> oppure di
	 * <tt>Owner.tableName</tt><br>
	 * <p>
	 * 
	 * @return the tableOwner
	 */
	public String getTableOwner() {
		return tableOwner;
	}


	/**
	 * Imposta l'owner della tabella, a fronte
	 * di <tt>serverId.Owner.tableName</tt> oppure di
	 * <tt>Owner.tableName</tt><br>
	 * <p>
	 * 
	 * @param tableOwner the tableOwner to set
	 */
	public void setTableOwner(String tableOwner) {
		this.tableOwner = tableOwner;
	}


	/**
	 * Restituisce il nome  della tabella, a fronte
	 * di <tt>serverId.Owner.tableName</tt> oppure di
	 * <tt>Owner.tableName</tt><br> oppure di
	 * tableName per nome non qualificato.
	 * <p>
	 * 
	 * @return the tableName
	 */
	public String getTableName() {
		return tableName;
	}


	/**
	 * Imposta il nome  della tabella, a fronte
	 * di <tt>serverId.Owner.tableName</tt> oppure di
	 * <tt>Owner.tableName</tt><br> oppure di
	 * tableName per nome non qualificato.
	 * <p>
	 * 
	 * @param tableName the tableName to set
	 */
	public void setTableName(String tableName) {
		this.tableName = tableName;
	}


	/**
	 * Restituisce il nome  dell'indice, a fronte di
	 * <tt>Owner.indexName</tt><br> oppure di
	 *indexName non qualificato.
	 * <p>
	 * 
	 * @return the indexFullName
	 */
	public String getIndexFullName() {
		return indexFullName;
	}


	/**
	 * Impposta il nome  dell'indice, a fronte di
	 * <tt>Owner.indexName</tt><br> oppure di
	 *indexName non qualificato.
	 * <p>
	 * 
	 * @param indexFullName the indexFullName to set
	 */
	public void setIndexFullName(String indexFullName) {
		this.indexFullName = indexFullName;
	}


	/**
	 * Restituisce il nome  dell'indice, a fronte di
	 * <tt>Owner.indexName</tt><br> oppure di
	 * indexName non qualificato.
	 * <p>
	 * 
	 * @return the indexName
	 */
	public String getIndexName() {
		return indexName;
	}


	/**
	 * Imposta il nome  dell'indice, a fronte di
	 * <tt>Owner.indexName</tt><br> oppure di
	 * indexName non qualificato.
	 * <p>
	 * 
	 * @param indexName the indexName to set
	 */
	public void setIndexName(String indexName) {
		this.indexName = indexName;
	}


	/**
	 * Restituisce l'owner  dell'indice, a fronte di
	 * <tt>Owner.indexName</tt><br> 
	 * <p>
	 * 
	 * @return the indexOwner
	 */
	public String getIndexOwner() {
		return indexOwner;
	}


	/**
	 * Imposta l'owner  dell'indice, a fronte di
	 * <tt>Owner.indexName</tt><br> 
	 * <p>
	 * @param indexOwner the indexOwner to set
	 */
	public void setIndexOwner(String indexOwner) {
		this.indexOwner = indexOwner;
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
	 * Restituisce il nome del buffer pool indicato in
	 * <tt>BUFFERPOOL bp-name</tt><br> 
	 * <p>
	 * @return the bufferPool
	 */
	public String getBufferPool() {
		return bufferPool;
	}


	/**
	 * Imposta il nome del buffer pool indicato in
	 * <tt>BUFFERPOOL bp-name</tt><br> 
	 * <p>
	 * @param bufferPool the bufferPool to set
	 */
	public void setBufferPool(String bufferPool) {
		this.bufferPool = bufferPool;
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
	 * indicata in <tt>SECQTY number</tt><br> 
	 * <p>
	 * @return the secQty
	 */
	public int getSecQty() {
		return secQty;
	}


	/**
	 * Imposta la quantità minima secondaria di allocazione
	 * indicata in <tt>SECQTY number</tt><br> 
	 * <p>
	 * @param secQty the secQty to set
	 */
	public void setSecQty(int secQty) {
		this.secQty = secQty;
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
	 * Restituisce la massima indirizzabilità di ogni data set di un
	 * indice secondario, indicata in <tt>PIECESIZE integer K|M|G</tt><br> 
	 * <p>
	 * @return the pieceSizeValue
	 */
	public int getPieceSizeValue() {
		return pieceSizeValue;
	}


	/**
	 * Imposta la massima indirizzabilità di ogni data set di un
	 * indice secondario, indicata in <tt>PIECESIZE integer K|M|G</tt><br> 
	 * <p>
	 * @param pieceSizeValue the pieceSizeValue to set
	 */
	public void setPieceSizeValue(int pieceSizeValue) {
		this.pieceSizeValue = pieceSizeValue;
	}


	/**
	 * Restituisce <tt>K</> o <tt>M</> o <tt>G</> per indicare <br>
	 * indicare che la massima indirizzabilità indica K-Bytes <br>
	 * Mega-Bytes o Giga-Bytes.<br>
	 * Indicata in <tt>PIECESIZE integer K|M|G</tt><br> 
	 * <p>
	 * @return the pieceSizeType
	 */
	public String getPieceSizeType() {
		return pieceSizeType;
	}


	/**
	 * Imposta <tt>K</> o <tt>M</> o <tt>G</> per indicare <br>
	 * indicare che la massima indirizzabilità indica K-Bytes <br>
	 * Mega-Bytes o Giga-Bytes.<br>
	 * Indicata in <tt>PIECESIZE integer K|M|G</tt><br> 
	 * <p>
	 * @param pieceSizeType the pieceSizeType to set
	 */
	public void setPieceSizeType(String pieceSizeType) {
		this.pieceSizeType = pieceSizeType;
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


	/**
	 * Restituisce true se presente l'opzione  <tt>PARTITIONED</tt> <br>
	 * <p>
	 * @return the isPartitioned
	 */
	public boolean isPartitioned() {
		return isPartitioned;
	}


	/**
	 * Imposta se presente l'opzione <tt>PARTITIONED</tt> <br>
	 * <p>
	 * @param isPartitioned the isPartitioned to set
	 */
	public void setPartitioned(boolean isPartitioned) {
		this.isPartitioned = isPartitioned;
	}


	/**
	 * Restituisce true se presente l'opzione  <tt>BUSINESS_TIME WITHOUT OVERLAPS</tt> <br>
	 * <p>
	 * @return the isBusinessTimeWithoutOverlaps
	 */
	public boolean isBusinessTimeWithoutOverlaps() {
		return isBusinessTimeWithoutOverlaps;
	}


	/**
	 * Imposta se presente l'opzione <tt>BUSINESS_TIME WITHOUT OVERLAPS</tt> <br>
	 * <p>
	 * @param isBusinessTimeWithoutOverlaps the isBusinessTimeWithoutOverlaps to set
	 */
	public void setBusinessTimeWithoutOverlaps(boolean isBusinessTimeWithoutOverlaps) {
		this.isBusinessTimeWithoutOverlaps = isBusinessTimeWithoutOverlaps;
	}


	/**
	 * Restituisce true se presente l'opzione  <tt>UNIQUE</tt> <br>
     * Indicata in <tt>CREATE INDEX UNIQUE</tt><br> 	 
     * <p>
	 * @return the isUnique
	 */
	public boolean isUnique() {
		return isUnique;
	}


	/**
	 * Imposta se presente l'opzione  <tt>UNIQUE</tt> <br>
     * Indicata in <tt>CREATE INDEX UNIQUE</tt><br> 	 
     * <p>
	 * @param isUnique the isUnique to set
	 */
	public void setUnique(boolean isUnique) {
		this.isUnique = isUnique;
	}


	/**
	 * Restituisce true se presente l'opzione  <tt>WHERE NOT NULL</tt> <br>
     * Indicata in <tt>CREATE INDEX UNIQUE WHERE NOT NULL</tt><br> 	 
     * <p>
	 * @return the isUniqueWhereNotNull
	 */
	public boolean isUniqueWhereNotNull() {
		return isUniqueWhereNotNull;
	}


	/**
	 * Imposta se presente l'opzione  <tt>WHERE NOT NULL</tt> <br>
     * Indicata in <tt>CREATE INDEX UNIQUE WHERE NOT NULL</tt><br> 	 
     * <p>
	 * @param isUniqueWhereNotNull the isUniqueWhereNotNull to set
	 */
	public void setUniqueWhereNotNull(boolean isUniqueWhereNotNull) {
		this.isUniqueWhereNotNull = isUniqueWhereNotNull;
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
	 * Restituisce true se presente l'opzione  <tt>CLOSE YES</tt> <br>
     * <p>
	 * @return the isClose
	 */
	public boolean isClose() {
		return isClose;
	}


	/**
	 * Imposta se presente l'opzione  <tt>CLOSE YES|NO</tt> <br>
     * <p>
	 * @param isClose the isClose to set
	 */
	public void setClose(boolean isClose) {
		this.isClose = isClose;
	}


	/**
	 * Restituisce true se presente l'opzione  <tt>CLUSTER</tt> <br>
     * <p>
	 * @return the isCluster
	 */
	public boolean isCluster() {
		return isCluster;
	}


	/**
	 * Imposta se presente l'opzione  <tt>CLUSTER</tt> <br>
     * <p>
	 * @param isCluster the isCluster to set
	 */
	public void setCluster(boolean isCluster) {
		this.isCluster = isCluster;
	}


	/**
	 * Restituisce true se presente l'opzione  <tt>PADDED</tt> <br>
     * <p>
	 * @return the isPadded
	 */
	public boolean isPadded() {
		return isPadded;
	}


	/**
	 * Imposta se presente l'opzione  <tt>PADDED</tt> <br>
     * <p>
	 * @param isPadded the isPadded to set
	 */
	public void setPadded(boolean isPadded) {
		this.isPadded = isPadded;
	}


	/**
	 * Restituisce true se presente l'opzione  <tt>DEFINE YES</tt> <br>
     * <p>
	 * @return the isDefine
	 */
	public boolean isDefine() {
		return isDefine;
	}


	/**
	 * Imposta se presente l'opzione  <tt>DEFINE YES|NO</tt> <br>
     * <p>
	 * @param isDefine the isDefine to set
	 */
	public void setDefine(boolean isDefine) {
		this.isDefine = isDefine;
	}


	/**
	 * Restituisce true se presente l'opzione  <tt>DEFER YES</tt> <br>
     * <p>
	 * @return the isDefer
	 */
	public boolean isDefer() {
		return isDefer;
	}


	/**
	 * Imposta se presente l'opzione  <tt>DEFER YES|NO</tt> <br>
     * <p>
	 * @param isDefer the isDefer to set
	 */
	public void setDefer(boolean isDefer) {
		this.isDefer = isDefer;
	}


	/**
	 * Restituisce true se presente l'opzione  <tt>COMPRESS YES</tt> <br>
     * <p>
	 * @return the isCompress
	 */
	public boolean isCompress() {
		return isCompress;
	}


	/**
	 * Imposta se presente l'opzione  <tt>COMPRESS YES|NO</tt> <br>
     * <p>
	 * @param isCompress the isCompress to set
	 */
	public void setCompress(boolean isCompress) {
		this.isCompress = isCompress;
	}


	/**
	 * Restituisce true se presente l'opzione  <tt>COPY YES</tt> <br>
     * <p>
	 * @return the isCopy
	 */
	public boolean isCopy() {
		return isCopy;
	}


	/**
	 * Imposta se presente l'opzione  <tt>COPY YES|NO</tt> <br>
     * <p>
	 * @param isCopy the isCopy to set
	 */
	public void setCopy(boolean isCopy) {
		this.isCopy = isCopy;
	}


	/**
	 * Restituisce i descrittori delle colonne componenti l'indice<br>
	 * gestiti da istanze della classe {@link SqlIndexColumn}<br>
	 * <p>
	 * @return the al_column
	 */
	public ArrayList<SqlIndexColumn> getColumns() {
		return al_column;
	}


	/**
	 * Imposta i descrittori delle colonne componenti l'indice<br>
	 * gestiti da istanze della classe {@link SqlIndexColumn}<br>
	 * <p>
	 * @param al_column the al_column to set
	 */
	public void setColumns(ArrayList<SqlIndexColumn> al_column) {
		this.al_column = al_column;
	}

	/**
	 * Restituisce il descrittore delle partizioni della  tabella.<br>
	 * <p>
	 * @return the partitions
	 */
	public SqlPartitions getPartitionsDescriptor() {
		return partitions;
	}


	/**
	 * Imposta il descrittore delle partizioni della  tabella.<br>
	 * <p>
	 * @param partitions the partitions to set
	 */
	public void setPartitionsDescriptor(SqlPartitions partitions) {
		this.partitions = partitions;
	}
		

	
}
