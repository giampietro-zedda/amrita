/**
  * copyright (c) 2008 Amrita-Forward - Giampietro Zedda 2008   Turin (ITALY)
 */
package forward;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.AbstractList;
import java.util.ArrayList;

/**
 * ForwardDbTableRow
 * 
 * This class allow to store and manage all
 * elementary data  base items achieved from a
 * Sql table row. Each item is then described by an
 * instance of ForwardDbItemValue.
 * This class needs the descriptor of each column
 * and the resultset, to get all data according to
 * the type.
 * 
 *   
 * @date 02-10-2008
 * @version 1.0 
 * @author Giampietro Zedda
 *
 */
public class ForwardDbTableRow {
	private ResultSet rs;
	private ForwardDbItemValue[] oa_ItemValue;  
    private ForwardDbItemDescriptor[] oa_Descriptor;
    private ForwardDbItemDescriptor oDbItemDescriptor;
	/**
	 * Constructor
	 * @throws SQLException 
	 */
    public ForwardDbTableRow(ResultSet rs, ForwardDbItemDescriptor[] oa_Descriptor) {
    	this.rs = rs;
    	this.oa_Descriptor = oa_Descriptor;

    	oa_ItemValue = new ForwardDbItemValue[oa_Descriptor.length];  

    	// Scan Columns descriptor and store value according to data type
    	for (int i = 0; i < oa_Descriptor.length; i++) {
    		try {
	    		switch (oa_Descriptor[i].getColumnType()) {
		    		case ForwardDbItemDescriptor.COL_TYPE_INTEGER:
		    			oa_ItemValue[i].setValueInt(rs.getInt(i));   
		    			break;
		    		case ForwardDbItemDescriptor.COL_TYPE_DOUBLE:
		    			oa_ItemValue[i].setValueDouble(rs.getDouble(i));  
		    			break;
		    		case ForwardDbItemDescriptor.COL_TYPE_STRING:
		    			oa_ItemValue[i].setValueString(rs.getString(i));  
		    			break;
		    		case ForwardDbItemDescriptor.COL_TYPE_DATE:
		    			oa_ItemValue[i].setValueInt(rs.getInt(i));  
		    			rs.getDate(i);
		    			break;
		    		case ForwardDbItemDescriptor.COL_TYPE_TIME:
		    			oa_ItemValue[i].setValueString(rs.getString(i));  
		    			break;
		    		case ForwardDbItemDescriptor.COL_TYPE_TIMESTAMP:
		    			oa_ItemValue[i].setValueString(rs.getTimestamp(i).toString());  
		    			break;
		    		default:
		    			oa_ItemValue[i].setValueString(rs.getString(i));
		    			break;
		    		}
				} catch (SQLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}  // Store text value anyway

    		}
 	}

	/**
	 * Return an array with data values stored by
	 * constructor
	 * 
	 * @return the oa_DbItemValue
	 */
	public ForwardDbItemValue[] getItemsValue() {
		return oa_ItemValue;
	}

	/**
	 * Return an ForwardDbTableRow object with values of
	 * data items for the current row
	 * 
	 * @return String oa_ItemValueString[]
	 */
	public String[] getItemsValueToString() {
		String oa_ItemValueString[] = new String[oa_Descriptor.length];
        
		// return an array string of data values
		for (int i = 0; i < oa_ItemValueString.length; i++) {
			oa_ItemValueString[i] = oa_ItemValue[i].getValueString();
		}
				
		return oa_ItemValueString;
	}

}
