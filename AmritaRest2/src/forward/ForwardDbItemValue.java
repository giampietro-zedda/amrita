/**
  * copyright (c) 2008 Amrita-Forward - Giampietro Zedda 2008   Turin (ITALY)
 */
package forward;

/**
 * ForwardDbItemValue
 * 
 * This class is used to describe any elementary data item
 * retrieved from an Sql table.
 * Data base items are stored on db with different
 * formats depending on dbms type. 
 * From Java application point of view, all you need 
 * to know is that the elementary data item  is numeric, string, data, 
 * time, boolean and so on, together the value.
 * One instance of this class describes a single data item from a
 * row of a Sql table.
 * 
 * 
 * @date 02-10-2008
 * @version 1.0 
 * @author Giampietro Zedda
 *
 */
public class ForwardDbItemValue {
	private String columnName;     
	private int columnType; 
	private String ValueString;  
	private int valueInt;  
	private long valueLong;  
	private double valueDouble; 
	private ForwardDbItemDescriptor oDbItemDescriptor;
	
    /**
	 * Constructor
	 */
	public ForwardDbItemValue() {
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
	 * @return the ValueString
	 */
	public String getValueString() {
		return ValueString;
	}
	/**
	 * @param ValueString the ValueString to set
	 */
	public void setValueString(String ValueString) {
		this.ValueString = ValueString;
	}
	/**
	 * @return the valueInt
	 */
	public int getValueInt() {
		return valueInt;
	}
	/**
	 * @param valueInt the valueInt to set
	 */
	public void setValueInt(int valueInt) {
		this.valueInt = valueInt;
	}
	/**
	 * @return the valueLong
	 */
	public long getValueLong() {
		return valueLong;
	}
	/**
	 * @param valueLong the valueLong to set
	 */
	public void setValueLong(long valueLong) {
		this.valueLong = valueLong;
	}

	/**
	 * @return the valueDouble
	 */
	public double getValueDouble() {
		return valueDouble;
	}
	/**
	 * @param valueDouble the valueDouble to set
	 */
	public void setValueDouble(double valueDouble) {
		this.valueDouble = valueDouble;
	}
	/**
	 * @return the oDbItemDescriptor
	 */
	public ForwardDbItemDescriptor getODbItemDescriptor() {
		return oDbItemDescriptor;
	}
	/**
	 * @param dbItemDescriptor the oDbItemDescriptor to set
	 */
	public void setODbItemDescriptor(ForwardDbItemDescriptor dbItemDescriptor) {
		oDbItemDescriptor = dbItemDescriptor;
	}

	
}
