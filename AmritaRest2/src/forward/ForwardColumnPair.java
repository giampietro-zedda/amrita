package forward;
import analyzer.DataBaseItemDescriptor;


/**
 * Copyright (c) 2009-2012 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ForwardColumnPair
 * </h1>
 * <p>
 * this class describes a single couple of columns of a <code>FOR_ANY</code> or  <code>FOR_EACH</code><br>
 * relationschip, as declared in a logical data view {@link ForwardLogicalDataView}.<br>.
 * A such a type of relationship is established between columns in a entity and columns in the related entity.<br>
 * Each column is described by a {@link DataBaseItemDescriptor} object, holding both java and sql informations like,<br>
 * for example, the name of jva field and sql column, the java field type, the sql data type and so on.<br>
 * <p>
 * All these informations are used to map sql data with java fields and vice versa.<br>
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 03/mar/2012 
 * @see ForwardMonitorDesktop
 * @see ForwardLogicalDataView
 *
*/

public class ForwardColumnPair {
	
	private DataBaseItemDescriptor column = null;
	private DataBaseItemDescriptor columnRelated = null;
	
	/**
	 * Creates a couple of columns descriptor.<br>
	 */
	public ForwardColumnPair(DataBaseItemDescriptor column, DataBaseItemDescriptor columnRelated) {
		this.column = column;
		this.columnRelated = columnRelated;
	}
	
	
	/**
	 * Get the column descriptor of the couple of columns.<br>
	 * <p>
	 * @return the column
	 */
	public DataBaseItemDescriptor getColumn() {
		return column;
	}


	/**
	 * Get the column related descriptor of the couple of columns.<br>
	 * <p>
	 * @return the columnRelated
	 */
	public DataBaseItemDescriptor getColumnRelated() {
		return columnRelated;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Column:"+column.getJavaFieldName()+",ColumnRelated:"+columnRelated.getJavaFieldName();
	}

	
	
}
