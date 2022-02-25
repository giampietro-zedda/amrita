
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
public class SqlPartitions implements Serializable {

 	private static final long serialVersionUID = 1L;	
 	
	// Colonne e valori corrispondenti, viaggiano in coppia
    private ArrayList<SqlPartitionColumn> al_column = null;		// Ogni elemento identifica una colonna della partizione
    private ArrayList<SqlPartitionValues> al_value = null;		// Ogni elemento identifica una partizione
	
    // Tipologia partizioni
    private boolean isPartitionByRange = false;                 // PARTITION BY |RANGE (cols ASC|DESC) (PARTITION num ENDING |AT (....) ...., PARTITION ...)
    private boolean isPartitionBySize = false;                  // PARTITION BY |SIZE EVERY integer G
    private boolean isInclusive = false;                        // PARTITION number ENDING AT ("AA", "BB") INCLUSIVE
    
	// Informazioni di allocazione (per partizioni di indici)
    private int partitionBySizeEveryValue = 0;                  // PARTITION BY SIZE EVERY int-value G

	
	/**
	 * Costruttore 
	 */
	public SqlPartitions() {
		super();
	    al_column = new ArrayList<SqlPartitionColumn> ();
	    al_value = new ArrayList<SqlPartitionValues> ();
	}


	/**
	 * Restituisce i descrittori delle singole colonne  della partizione.<br>
	 * <p>
	 * @return the al_column
	 */
	public ArrayList<SqlPartitionColumn> getColumns() {
		return al_column;
	}


	/**
	 * Imposta i descrittori delle singole colonne  della partizione.<br>
	 * <p>
	 * @return the al_column
	 * @param alColumn the al_column to set
	 */
	public void setColumns(ArrayList<SqlPartitionColumn> al_column) {
		this.al_column = al_column;
	}


	/**
	 * Restituisce i descrittori dei singli valori delle colonne  della partizione.<br>
	 * <p>
	 * @return the al_value
	 */
	public ArrayList<SqlPartitionValues> getValues() {
		return al_value;
	}


	/**
	 * Imposta i descrittori dei singli valori delle colonne  della partizione.<br>
	 * <p>
	 * @param alValue the al_value to set
	 */
	public void setValues(ArrayList<SqlPartitionValues> al_value) {
		this.al_value = al_value;
	}


	/**
	 * Restituisce se le partizioni sono della forma <tt>PARTITION BY |RANGE ( ... </tt><br>
	 * 
	 * @return the isPartitionByRange
	 */
	public boolean isPartitionByRange() {
		return isPartitionByRange;
	}


	/**
	 * Imposta se le partizioni sono della forma <tt>PARTITION BY |RANGE ( ... </tt><br>
	 * 
	 * @param isPartitionByRange the isPartitionByRange to set
	 */
	public void setPartitionByRange(boolean isPartitionByRange) {
		this.isPartitionByRange = isPartitionByRange;
	}


	/**
	 * Restituisce se le partizione sono della forma <tt>PARTITION BY |SIZE EVERY integer G </tt><br>
	 * <p>
	 * @return the isPartitionBySize
	 */
	public boolean isPartitionBySize() {
		return isPartitionBySize;
	}


	/**
	 * Imposta se le partizione sono della forma <tt>PARTITION BY |SIZE EVERY integer G </tt><br>
	 * <p>
	 * @param isPartitionBySize the isPartitionBySize to set
	 */
	public void setPartitionBySize(boolean isPartitionBySize) {
		this.isPartitionBySize = isPartitionBySize;
	}


	/**
	 * Restituisce se le partizione ha il parametro INCLUSIVE </tt><br>
	 * <p>
	 * @return the isInclusive
	 */
	public boolean isInclusive() {
		return isInclusive;
	}


	/**
	 * Imposta se le partizione ha il parametro INCLUSIVE </tt><br>
	 * <p>
	 * @param isInclusive the isInclusive to set
	 */
	public void setInclusive(boolean isInclusive) {
		this.isInclusive = isInclusive;
	}


	/**
	 * Restituisce il valore espresso da <tt>PARTITION BY SIZE EVERY in-value G </tt> <br>
	 * <p>
	 * @return the partitionBySizeEveryValue
	 */
	public int getPartitionBySizeEveryValue() {
		return partitionBySizeEveryValue;
	}


	/**
	 * Imposta il valore espresso da <tt>PARTITION BY SIZE EVERY in-value G </tt> <br>
	 * <p>
	 * @param partitionBySizeEveryValue the partitionBySizeEveryValue to set
	 */
	public void setPartitionBySizeEveryValue(int partitionBySizeEveryValue) {
		this.partitionBySizeEveryValue = partitionBySizeEveryValue;
	}
}
