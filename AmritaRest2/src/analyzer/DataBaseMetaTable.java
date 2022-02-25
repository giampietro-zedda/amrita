package analyzer;


/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseMetaTable
 * </h1>
 * <p>
 * Descrive le informazioni di una tabella come recuperate direttamente dal driver
 * corrente attraverso i metodi della classe standard di java DataBaseMetaData.
 * 
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/mar/2010 
 * @see DataBaseManager
 * @see DataBaseItemdescriptor
 *
*/

public class DataBaseMetaTable{
	
	private String catalog = "";				// Catalogo
	private String schema = "";					// Schema (database)
	private String tableName = "";				// Nome tabella
	private String tableType = "";				// tipo tabella
	private String remarks = "";				// Commenti
	
	/**
	 * Costruttore vuoto
	 */
	public DataBaseMetaTable() {
		super();
	}

	/**
	 * @return the catalog
	 */
	public String getCatalog() {
		return catalog;
	}

	/**
	 * @param catalog the catalog to set
	 */
	public void setCatalog(String catalog) {
		this.catalog = catalog;
	}

	/**
	 * @return the schema
	 */
	public String getSchema() {
		return schema;
	}

	/**
	 * @param schema the schema to set
	 */
	public void setSchema(String schema) {
		this.schema = schema;
	}

	/**
	 * @return the tableName
	 */
	public String getTableName() {
		return tableName;
	}

	/**
	 * @param tableName the tableName to set
	 */
	public void setTableName(String tableName) {
		this.tableName = tableName;
	}

	/**
	 * @return the tableType
	 */
	public String getTableType() {
		return tableType;
	}

	/**
	 * @param tableType the tableType to set
	 */
	public void setTableType(String tableType) {
		this.tableType = tableType;
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

}
