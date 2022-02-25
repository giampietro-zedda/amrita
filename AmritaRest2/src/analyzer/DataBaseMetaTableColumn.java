package analyzer;
import enums.EnumDataBaseJdbcSqlType;


/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseMetaTableColumn
 * </h1>
 * <p>
 * Descrive le informazioni di una specifica colonna di una tabella come recuperate direttamente 
 * dal driver corrente, attraverso i metodi della classe standard di java ResultSetMetaData.
 * 
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/mar/2010 
 * @see DataBaseManager
 * @see DataBaseItemdescriptor
 * @see DataBaseMetaTable
 *
*/

public class DataBaseMetaTableColumn{

	private String TableName = "";						 // Table
	private String columnName = "";						 // Nome colonna
	private int dataType = 0;					    	 // Tipo colonna formato java.sql.types
	private EnumDataBaseJdbcSqlType dataTypeCoded = null;// Enum per dataType
	private int columnSize = 0;					    	 // Dimensione colonna o numero totale cifre
	private int decimalDigits = 0;					     // Numero decimali
	private boolean primaryKey = false;					 // True se colonna di primary key
	private boolean nullable = false;					 // Permessi valori a NULL
	private String remarks = "";					 	 // Commenti
	private String defaultValue = "";					 // Valore di default se stringa	
	private boolean autoIncrement = false;			     // True = colonna auto increment
	private String indexName = "";			             // Nome indice se colonna appartenente a un indice
	private boolean indexUnique = false;			     // True = indice unique
	private boolean indexAscending = false;			     // True = colonna indice ascending
	
	/**
	 * Costruttore vuoto
	 */
	public DataBaseMetaTableColumn() {
		super();
	}

	/**
	 * @return the tableName
	 */
	public String getTableName() {
		return TableName;
	}

	/**
	 * @param tableName the tableName to set
	 */
	public void setTableName(String tableName) {
		TableName = tableName;
	}

	/**
	 * @return the columnName
	 */
	public String getColumnName() {
		return columnName;
	}

	/**
	 * @param columnName the columnName to set
	 */
	public void setColumnName(String columnName) {
		this.columnName = columnName;
	}

	/**
	 * @return the dataType
	 */
	public int getDataType() {
		return dataType;
	}

	/**
	 * @param dataType the dataType to set
	 */
	public void setDataType(int dataType) {
		this.dataType = dataType;
	}

	/**
	 * @return the dataTypeCoded
	 */
	public EnumDataBaseJdbcSqlType getDataTypeCoded() {
		return dataTypeCoded;
	}

	/**
	 * @param dataTypeCoded the dataTypeCoded to set
	 */
	public void setDataTypeCoded(EnumDataBaseJdbcSqlType dataTypeCoded) {
		this.dataTypeCoded = dataTypeCoded;
	}

	/**
	 * @return the columnSize
	 */
	public int getColumnSize() {
		return columnSize;
	}

	/**
	 * @param columnSize the columnSize to set
	 */
	public void setColumnSize(int columnSize) {
		this.columnSize = columnSize;
	}

	/**
	 * @return the decimalDigits
	 */
	public int getDecimalDigits() {
		return decimalDigits;
	}

	/**
	 * @param decimalDigits the decimalDigits to set
	 */
	public void setDecimalDigits(int decimalDigits) {
		this.decimalDigits = decimalDigits;
	}

	
	/**
	 * @return the primaryKey
	 */
	public boolean isPrimaryKey() {
		return primaryKey;
	}

	/**
	 * @param primaryKey the primaryKey to set
	 */
	public void setPrimaryKey(boolean primaryKey) {
		this.primaryKey = primaryKey;
	}

	/**
	 * @return the nullable
	 */
	public boolean isNullable() {
		return nullable;
	}

	/**
	 * @param nullable the nullable to set
	 */
	public void setNullable(boolean nullable) {
		this.nullable = nullable;
	}

	/**
	 * @return the remarks
	 */
	public String getRemarks() {
		return remarks;
	}

	/**
	 * @param remarks the remarks to set
	 */
	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

	/**
	 * @return the defaultValue
	 */
	public String getDefaultValue() {
		return defaultValue;
	}

	/**
	 * @param defaultValue the defaultValue to set
	 */
	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

	/**
	 * @return the autoIncrement
	 */
	public boolean isAutoIncrement() {
		return autoIncrement;
	}

	/**
	 * @param autoIncrement the autoIncrement to set
	 */
	public void setAutoIncrement(boolean autoIncrement) {
		this.autoIncrement = autoIncrement;
	}

	/**
	 * @return the indexName
	 */
	public String getIndexName() {
		return indexName;
	}

	/**
	 * @param indexName the indexName to set
	 */
	public void setIndexName(String indexName) {
		this.indexName = indexName;
	}

	/**
	 * @return the indexUnique
	 */
	public boolean isIndexUnique() {
		return indexUnique;
	}

	/**
	 * @param indexUnique the indexUnique to set
	 */
	public void setIndexUnique(boolean indexUnique) {
		this.indexUnique = indexUnique;
	}

	/**
	 * @return the indexAscending
	 */
	public boolean isIndexAscending() {
		return indexAscending;
	}

	/**
	 * @param indexAscending the indexAscending to set
	 */
	public void setIndexAscending(boolean indexAscending) {
		this.indexAscending = indexAscending;
	}


}