/**
  * copyright (c) 2008 Amrita-Forward - Giampietro Zedda 2008   Turin (ITALY)
 */
package forward;

/**
 * ForwardDbItemDescriptor
 * 
 * This class is used to collect phisical informations about
 * an elementary data item of a Sql table.
 * 
 * 
 * @date 02-10-2008
 * @version 1.0 
 * @author Giampietro Zedda 
 *
 */
public class ForwardDbItemDescriptor {
    // Constant values for ColumnType
	public static final int COL_TYPE_INTEGER = 1;
	public static final int COL_TYPE_DOUBLE = 2;
	public static final int COL_TYPE_STRING = 3;
	public static final int COL_TYPE_DATE = 4;
	public static final int COL_TYPE_TIME = 5;
	public static final int COL_TYPE_TIMESTAMP = 6;
	
	private String columnName;     
	private String columnLabel;  
	private int columnType; 
	private int columnTypeSql; 
	private String columnTypeSqlName; 
	private int columnLng;
	private int columnNumInt;
	private int columnNumDec;
	private String tableOwner;     

	/**
	 * Constructor
	 */
	public ForwardDbItemDescriptor() {
		// TODO Auto-generated constructor stub
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
	 * @return the columnTypeSql
	 */
	public int getcolumnTypeSql() {
		return columnTypeSql;
	}

	/**
	 * @param columnTypeSql the columnTypeSql to set
	 */
	public void setcolumnTypeSql(int columnTypeSql) {
		this.columnTypeSql = columnTypeSql;
	}

	/**
	 * @return the tableOwner
	 */
	public String getTableOwner() {
		return tableOwner;
	}

	/**
	 * @param tableOwner the tableOwner to set
	 */
	public void setTableOwner(String tableOwner) {
		this.tableOwner = tableOwner;
	}

	/**
	 * @return the columnLng
	 */
	public int getColumnLng() {
		return columnLng;
	}

	/**
	 * @param columnLng the columnLng to set
	 */
	public void setColumnLng(int columnLng) {
		this.columnLng = columnLng;
	}

	/**
	 * @return the columnNumInt
	 */
	public int getColumnNumInt() {
		return columnNumInt;
	}

	/**
	 * @param columnNumInt the columnNumInt to set
	 */
	public void setColumnNumInt(int columnNumInt) {
		this.columnNumInt = columnNumInt;
	}

	/**
	 * @return the columnNumDec
	 */
	public int getcolumnNumDec() {
		return columnNumDec;
	}

	/**
	 * @param columnNumDec the columnNumDec to set
	 */
	public void setColumnNumDec(int columnNumDec) {
		this.columnNumDec = columnNumDec;
	}

	/**
	 * @return the columnTypeSqlName
	 */
	public String getcolumnTypeSqlName() {
		return columnTypeSqlName;
	}

	/**
	 * @param columnTypeSqlName the columnTypeSqlName to set
	 */
	public void setcolumnTypeSqlName(String columnTypeSqlName) {
		this.columnTypeSqlName = columnTypeSqlName;
	}

	/**
	 * @return the columnLabel
	 */
	public String getColumnLabel() {
		return columnLabel;
	}

	/**
	 * @param columnLabel the columnLabel to set
	 */
	public void setColumnLabel(String columnLabel) {
		this.columnLabel = columnLabel;
	}

	/**
	 * @return the columnType
	 */
	public int getColumnType() {
		return columnType;
	}

	/**
	 * @param columnType the columnType to set
	 */
	public void setColumnType(int columnType) {
		this.columnType = columnType;
	}

	/**
	 * @return the columnTypeSql
	 */
	public int getColumnTypeSql() {
		return columnTypeSql;
	}

	/**
	 * @param columnTypeSql the columnTypeSql to set
	 */
	public void setColumnTypeSql(int columnTypeSql) {
		this.columnTypeSql = columnTypeSql;
	}

	/**
	 * @return the columnTypeSqlName
	 */
	public String getColumnTypeSqlName() {
		return columnTypeSqlName;
	}

	/**
	 * @param columnTypeSqlName the columnTypeSqlName to set
	 */
	public void setColumnTypeSqlName(String columnTypeSqlName) {
		this.columnTypeSqlName = columnTypeSqlName;
	}

	/**
	 * @return the columnNumDec
	 */
	public int getColumnNumDec() {
		return columnNumDec;
	}

}
