package forward;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JComboBox;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.JTableHeader;
import analyzer.DataBaseMappedTable;

/**
 * copyright (c) 2009-2012 e-Amrita - Ing. Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardTableModel
 * </h1>
 * <p>
 * Extends {@link AbstractTableModel} and holds additional data for a {@link JTable} swing control useful for <br>
 * applications and completely user dependent. <br>
 * Often it's showed a list rows in the table but structured information must be maintened for each row,<br>
 * like a key or a numeric value application dependent.<br>
 * It's managed by this class an object for each row in the list and it's responsability of application manage the<br>
 * array of ojects when inserting, removing and updating rows int table.<br>
 * <p>
 * In any case item and objects bound must hold the same index.
 * <br>
 * The forward aim is to automatize the process development as much as possible, so all database informations are stored too.<br>
 * In forward the origin of data base informations, at entity level, is stored in the object that manage an entity been.<br>
 * In the entity been object, thru annotations, are depicts the database name of table and columns and the java internal name<br>
 * mapped. So it's enaugh declare the entity been object to bring all informations in the model.<br>
 * <p>
 * Because this class inherits from {@link AbstractTableModel}, to avoid confusion, all specific forward methods are starting with _ (underscore).<br>
 * Forward methods let to populate the model with header, data, tooltip and binding row data.<br>
 * Other methods let to remove and get rows.<br>
 * Any column related information is managed by class {@link ForwardTableModelColumn} for each column declared.<br>
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 08/dic/2011 
 * @see ForwardListModel
 * @see ForwardTableModelColumn
 *
*/

class ForwardTableModel extends AbstractTableModel implements Serializable{
	
	private static final long serialVersionUID = 1L;
	
	private JTable jtable = null;										// La tabella JTable che utilizza il modello
	private String tableDbName = "";                                    // Nome tabella come definita nel database
	private Object entityTableManager = null;							// Classe di gestione db entity con cui mappare, popolare e gestire la tabella
	private ArrayList<ForwardTableModelColumn> al_column = null;        // Colonne componenti la tabella
    private ArrayList<Integer> al_rowNotEditable = null;				// Un elemento per ogni numero di riga non editabile
    private Map<String, String> hs_cellToolTipText = null;				// key=row"-"col data= tooltip di cella
    private ArrayList<ArrayList<Object>> al_al_data = null;				// N elementi di riga, ognuno con n elementi di colonna
    private ArrayList<Object> al_dataBound = null;						// Un elemento per ogni riga, null se non significativo
    private String varNameDataBound = "";                               // Nome variabile runtime valorizzata dal monitor con il valore corrente bound di riga
    private boolean isTableDataBound = false;							// Bound attivo oppure no
   
 
    /** Constructor */
	public ForwardTableModel() {
 		super();
  		
 		al_column = new ArrayList<ForwardTableModelColumn> ();
	    al_rowNotEditable = new ArrayList<Integer> ();	
	    hs_cellToolTipText = new HashMap<String, String> ();
 	    al_al_data = new ArrayList<ArrayList<Object>> ();		 
	    al_dataBound = new ArrayList<Object> ();	
	}

	public ForwardTableModel(JTable jtable) {
 		super();
 		this.jtable = jtable;
 		
 		al_column = new ArrayList<ForwardTableModelColumn> ();
	    al_rowNotEditable = new ArrayList<Integer> ();	
	    hs_cellToolTipText = new HashMap<String, String> ();
 	    al_al_data = new ArrayList<ArrayList<Object>> ();		 
	    al_dataBound = new ArrayList<Object> ();	
	    this.jtable.setTableHeader(createDefaultTableHeader());
	}

    /* ------------------------------------------------------------------------
     * Metodi standard overridati o implementati del modello AbstractTableModel
     * ------------------------------------------------------------------------
     */
    
    /**
      * @see javax.swing.table.TableModel#getColumnCount()
     */
    public int getColumnCount() {
         return al_column.size();
    }

    /**
      * @see javax.swing.table.TableModel#getRowCount()
     */
    public int getRowCount() {
          return al_al_data.size();
    }

    /**
      * @see javax.swing.table.AbstractTableModel#getColumnName(int)
      * Gets the header text
     */
    public String getColumnName(int col) {
        return al_column.get(col).getHeader();
    }
    
    /**
     * @see javax.swing.table.TableModel#getValueAt(int, int)
     */
    public Object getValueAt(int row, int col) {
    	ArrayList<Object> al_row = null;
    	al_row = this.al_al_data.get(row);
        return al_row.get(col);
    }

    /**
      * @see javax.swing.table.AbstractTableModel#setValueAt(java.lang.Object, int, int)
     */
    public void setValueAt(Object value, int row, int col) {
    	ArrayList<Object> al_row = null;
    	al_row = this.al_al_data.get(row);
    	al_row.set(col, value);
        fireTableCellUpdated(row, col);
    }
    

    /**
      * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
     */
     public Class<?> getColumnClass(int col) {
        return al_column.get(col).getClassType();
    }
 
     /**
      * @see javax.swing.table.AbstractTableModel#isCellEditable(int, int)
      */
     public boolean isCellEditable(int row, int col) {
    	 
    	 // Colonna non editabile
    	 if (!al_column.get(col).isEditable()) {
			return false;
		}
    	
    	// Numero di riga non editabile
    	 if (al_rowNotEditable.contains(new Integer(row))) {
			return false;
		}
      	// Riga e colonna editabili
        return true;
    }

     
    /**
     * Directly called by getToolTipText(), the forward overrided jtable method.<br>
     * <p>
     * This method is intended for internal forward use only.<br>
     * <p>
     * Return the cell level tooltip, if any.
     */
     public String getToolTipText(MouseEvent e) {
    	
        String tooTipText = null;
        java.awt.Point p = e.getPoint();
        int rowIndex = this.jtable.rowAtPoint(p);
        int colIndex = this.jtable.columnAtPoint(p);
        int realRowIndex = this.jtable.convertRowIndexToModel(rowIndex);
        int realColumnIndex = this.jtable.convertColumnIndexToModel(colIndex);
        
        // Impostato applicativamente, se non impostato restituisce null
        tooTipText = this.hs_cellToolTipText.get(realRowIndex+"-"+realColumnIndex);
        
        return tooTipText;
    }

    
    
    
    
    /**
     * This method is intended for internal forward use only.<br>
     * A {@link JTableHeader} object will be created, containing the public method <code>getToolTipText(MouseEvent e)</code><br>
     * returning the tooltip declared by application function.<br>
     * This new {@link JTableHeader} object is set 
     * @return a new JTableHeader object is then set for the JTable.<br>
     */
    public JTableHeader createDefaultTableHeader() {
        return new JTableHeader(jtable.getColumnModel()) {
												  			private static final long serialVersionUID = 1L;
															public String getToolTipText(MouseEvent e) {
												                java.awt.Point p = e.getPoint();
												                int index = columnModel.getColumnIndexAtX(p.x);
												                int realIndex = columnModel.getColumn(index).getModelIndex();
												                return al_column.get(realIndex).getToolTipText();
												            }
        };
    }

    /* --------------------------------------------------
     * Metodi specifici per gestione applicativa forward
     * --------------------------------------------------
     */
    
	/**
	 * Gets the JTable object the this model pupulates.<br>
	 * <p>
	 * @return the jtable
	 */
	public JTable _getJtable() {
		return jtable;
	}

	/**
	 * Sets the JTable object the this model pupulates.<br>
	 * >p>
	 * @param jtable the jtable to set
	 */
	public void _setJtable(JTable jtable) {
		this.jtable = jtable;
	    this.jtable.setTableHeader(createDefaultTableHeader());

	}

	/**
	 * Gets the column index.<br>
	 * <p>
	 * If the column name is not declared, a -1 valuye will be returned.<br>
	 * <p>
	 * @return the index of the column with the input name
	 */
	public int _getColumnIndex(String columnName) {
		for (ForwardTableModelColumn column : al_column) {
			if (column.getName().equals(columnName)) {
				return column.getIndex();
			}
		}
		return -1;
	}

	/**
	 * Gets all columns forward descriptor.<br>
	 * <p>
	 * @return the al_column as an ArrayList of {@link ForwardTableModelColumn}
	 */
	public ArrayList<ForwardTableModelColumn> _getColumns() {
		return al_column;
	}

	/**
	 * Gets the column forward descriptor.<br>
	 * <p>
	 * @param indexColumn the column number, 0-based
	 * @return the column descriptor as a {@link ForwardTableModelColumn} object
	 */
	public ForwardTableModelColumn _getColumn(int indexColumn) {
		return al_column.get(indexColumn);
	}

	/**
	 * Gets the column forward descriptor.<br>
	 * <p>
	 * If the column is undefined, a null value will be returned.<br>
	 * <p>
	 * @param columnName  
	 * @return the column descriptor as a {@link ForwardTableModelColumn} object
	 */
	public ForwardTableModelColumn _getColumn(String columnName) {
		int indexColumn = 0;
		indexColumn = _getColumnIndex(columnName);
		if (indexColumn < 0) {return null;}
		return al_column.get(indexColumn);
	}

	/**
	 * Gets row numbers not editable in the table.<br>
	 * <p>
	 * Each element contains an integer that is a row number not editable<br>
	 * <p>
	 * @return the al_rowNotEditable
	 */
	public ArrayList<Integer> _getRowsNotEditable() {
		return al_rowNotEditable;
	}

	/**
	 * Sets row numbers not editable in the table.<br>
	 * <p>
	 * Each element contains an integer that is a row number not editable<br>
	 * <p>
	 * @param al_rowNotEditable the al_rowNotEditable to set
	 */
	public void _setRowsNotEditable(ArrayList<Integer> al_rowNotEditable) {
		this.al_rowNotEditable = al_rowNotEditable;
	}


	/**
	 * Gets the cell tooltip cell for row and column.<br>
	 * <p>
	 * If no cell toolTip defined an empty string is returned.<br>
	 * <p>
	 * @return the hs_cellToolTipText
	 */
	public String _getCellToolTipText(int row, int col) {
		String cellToolTipText = "";
		cellToolTipText = this.hs_cellToolTipText.get(row+"-"+col);
		if (cellToolTipText == null) {
			cellToolTipText = "";
		}
		return cellToolTipText;
	}

	/**
	 * Sets a toolTip text for the cell with row and column specified.<br>
	 * <p>
	 * @param hs_cellToolTipText the hs_cellToolTipText to set
	 */
	public void _setCellToolTipText(int row, int col, String value) {
		hs_cellToolTipText.put(row+"-"+col, value);
	}

	/**
	 * Gets the table row data stored in the model.<br>
	 * <p>
	 * A two dimension ArrayList of ArrayList of objects is returned.<br>
	 * <p>
	 * If the row is not a valid number it will be returned null.<br>
	 * 
	 * @return the al_al_data
	 */
	public ArrayList<Object> _getDataRow(int row) {
		ArrayList<Object> al_row = al_al_data.get(row);
		return al_row;
	}

	/**
	 * Gets all table rows data stored in the model.<br>
	 * <p>
	 * A two dimension ArrayList of ArrayList of objects is returned.<br>
	 * <p>
	 * 
	 * @return the al_al_data
	 */
	public ArrayList<ArrayList<Object>> _getDataRows() {
		return al_al_data;
	}

	/**
	 * Sets all table rows data stored in the model.<br>
	 * <p>
	 * A two dimension ArrayList of ArrayList of objects is required.<br>
	 * <p>
	 * 
	 * @param al_al_data the al_al_data to set
	 */
	public void _setDataRows(ArrayList<ArrayList<Object>> al_al_data) {
		this.al_al_data = al_al_data;
		fireTableDataChanged();
	}

	/**
	 * Sets the table data row in the model.<br>
	 * <p>
	 * If the row number is not valid return false.<br>
	 * <p>
	 * 
	 * @param the row number
	 * @param the data row to set
	 */
	public void _setDataRow(int row, ArrayList<Object> al_dataRow) {
		this.al_al_data.set(row, al_dataRow);
		fireTableRowsUpdated(row, row);
	}

	/**
	 * Gets the application object bound to the row.
	 * <p>
	 * If the num row is out of range, a null value willbe returned.<br>
	 * <p>
	 * @param row the row number
	 * @return the object bound to the row
	 */
	public Object _getDataBoundAtRow(int row) {
		if (row >= al_dataBound.size()) {return null;}
		return al_dataBound.get(row);
	}

	/**
	 * Gets the structure twin of table data, with application objects.
	 * <p>
	 * It's returned an ArrayList of objects, where  each element <br>
	 * is referred to the row in the data model for the table.<br>
	 * <p>
	 * 
	 * @return the al_dataBound arrayList
	 */
	public ArrayList<Object> _getDataBound() {
		return al_dataBound;
	}

	
	/**
	 * Gets the variable name used by forward monitor, to store<br>
	 * the row data bound object. This variable, allocated in the<br>
	 * forward heap at the function starting, will be updated when<br>
	 * a row willò be selected and can be updated by user application logic too.<br>
	 * <p>
	 * @return the varNameDataBound
	 */
	public String _getVarNameDataBound() {
		return varNameDataBound;
	}

	/**
	 * Sets the variable name used by forward monitor, to store<br>
	 * the row data bound object. This variable, allocated in the<br>
	 * forward heap at the function starting, will be updated when<br>
	 * a row will be selected and can be updated by user application logic too.<br>
	 * <p>
	 * @param varNameDataBound the varNameDataBound to set
	 */
	public void _setVarNameDataBound(String varNameDataBound) {
		this.varNameDataBound = varNameDataBound;
	}

	/**
	 * Sets application data bound to data stored in the model.<br>
	 * <p>
	 * It's required an ArrayList of objects, where  each element <br>
	 * is referred to the row in the data model for the table.<br>
	 * <p>
	 * @param al_dataBound the al_dataBound to set
	 */
	public void _setDataBound(ArrayList<Object> al_dataBound) {
		this.al_dataBound = al_dataBound;
	}

	/**
	 * Sets the application row bound object.
	 * <p>
	 * If the num row is out of range, no action will be taken.<br>
	 * <p>
	 * @param row the row number
	 * @param objectBound the the application object to bound
	 */
	public void _setDataBoundAtRow(int row, Object objectBound) {
		if (row >= al_dataBound.size()) {return;}
		this.al_dataBound.set(row, objectBound);
	}

	/**
	 * Gets if the table manage data bound.<br>
	 * <p>
	 * True means that is managed an application object for any row.
	 * <br>
	 * @return the isTableDataBound
	 */
	public boolean _isTableDataBound() {
		return isTableDataBound;
	}

	/**
	 * Sets if the table manage data bound.<br>
	 * <p>
	 * True means that is managed an application object for any row.
	 * <br>
	 * @param isTableDataBound the isTableDataBound to set
	 */
	public void _setTableDataBound(boolean isTableDataBound) {
		this.isTableDataBound = isTableDataBound;
	}

	/////////////////////////////////////////////////////
	// Metodi di gestione delle righe della tabella
    /////////////////////////////////////////////////////

	/**
	 * Clear all data in the table<br>
	 * <p>
	 * The table is set to scratch erasing data for<br>
	 * <pre>
	 * columnClass
	 * columnName
	 * columnEditable
	 * rowNotEditable
	 * columnToolTip
	 * columnCellRenderer
	 * columnCellEditor
	 * cellToolTip
	 * boundData
	 * rowsData
	 * </pre>
	 * 
	 */
	public void _clear() {
	    al_column.clear();			 
 	    al_rowNotEditable.clear();		
	    hs_cellToolTipText.clear();		
 	    al_al_data.clear();		 
	    al_dataBound.clear();
	    fireTableDataChanged();
	}

	
	/**
	 * Remove all rows from the table<br>
	 * <p>
	 * Class type, column names and all other informations will be not removed<br>
	 * All data row bound will be removed too.<br>
	 * After this operation the table is ready to be filled with new data, for example<br>
	 * during a paging operation.<br>
	 */
	public void _deleteAllRows() {
 	    al_al_data.clear();	
 	    al_dataBound.clear();	
 	    fireTableDataChanged();
 	}

	/**
	 * Remove a range of data rows from the table<br>
	 * <p>
	 * The data row bound with the same row number will be removed too.<br>
	 * If the row number specified is not valid return false with no operations.
	 * @param row number
	 */
	public boolean _deleteRows(int rowStart, int rowEnd) {
		int i = 0;
		for (i = 0; i <= rowEnd - rowStart; i++) {
			al_al_data.remove(rowStart);
			al_dataBound.remove(rowStart);
		}
 	    fireTableRowsDeleted(rowStart, rowEnd);
 	    return true;
	}

	/**
	 * Remove a set of data rows from the table<br>
	 * <p>
	 * The data row bound with the same row number will be removed too.<br>
	 * If the row number specified is not valid return false with no operations.
	 * @param row number
	 */
	public boolean _deleteRows(ArrayList<Integer> al_rowNum) {
		int cntDel = 0;
		Collections.sort(al_rowNum);
		for (Integer numRow : al_rowNum) {
			al_al_data.remove(numRow - cntDel);
			al_dataBound.remove(numRow - cntDel);
			cntDel++;
		    fireTableRowsDeleted(numRow, numRow);
		}
  	    return true;
	}

	/**
	 * Remove a single data row from the table<br>
	 * <p>
	 * The data row bound with the same row number will be removed too.<br>
	 * If the row number specified is not valid return false with no operations.
	 * @param row number
	 */
	public boolean _deleteRow(int numRow) {
		al_al_data.remove(numRow);
		al_dataBound.remove(numRow);
		fireTableRowsDeleted(numRow, numRow); 
  	    return true;
	}

	
	/**
	 * Append a data row in the table<br>
	 * <p>
	 * A null data row bound will be appended, in the own structure, at the same <br>
	 */
	public void _appendRow(ArrayList<Object> al_rowCol) {
 	    al_al_data.add(al_rowCol);
 	    al_dataBound.add(null);
 	    fireTableRowsInserted(al_al_data.size() - 1, al_al_data.size() - 1);
	}

	/**
	 * Append a data row in the table<br>
	 * <p>
	 * The data row bound will be appended, in the own structure, at the same row number<br>
	 */
	public void _appendRow(ArrayList<Object> al_row,  Object dataBound) {
 	    al_al_data.add(al_row);
 	    al_dataBound.add(dataBound);
 	    fireTableRowsInserted(al_al_data.size() - 1, al_al_data.size() - 1);
	}

	/**
	 * Inserts the specified data row at the specified position in the table rows. <br>
	 * Shifts the element currently at that position (if any) and any subsequent <br>
	 * elements to the right (adds one to their indices).<br>
	 * A null data row bound will be inserted, in the own structure, at the same <br>
	 * row number.
	 * <p>
	 */
	public void _insertRow(int row, ArrayList<Object> al_row) {
 	    al_al_data.add(row, al_row);
 	    al_dataBound.add(row, null);
	    fireTableRowsInserted(row, row);
	}

	/**
	 * Inserts the specified data row at the specified position in the table rows. <br>
	 * Shifts the element currently at that position (if any) and any subsequent <br>
	 * elements to the right (adds one to their indices).<br>
	 * The data row bound will be inserted, in the own structure, at the same <br>
	 * row number.
	 * <p>
	 */
	public void _insertRow(int row, ArrayList<Object> al_row, Object dataBound) {
 	    al_al_data.add(row, al_row);
 	    al_dataBound.add(row, dataBound);
	    fireTableRowsInserted(row, row);
	}

	/**
	 * Update all cell objects of the specified data row. <br>
	 * <p>
	 * Data object bound will be updated too.
	 */
	public void _updateRow(int row, ArrayList<Object> al_row, Object dataBound) {
		Object objDataCell = null;
		for (int col = 0; col < al_row.size(); col++) {
			objDataCell = al_row.get(col);
			al_dataBound.set(row, dataBound);
			setValueAt(objDataCell, row, col);
		}
	}

	/**
	 * Update all cell objects of the specified data row. <br>
	 * <p>
	 * Data object bound will be not updated.
	 */
	public void _updateRow(int row, ArrayList<Object> al_row) {
		Object objDataCell = null;
		for (int col = 0; col < al_row.size(); col++) {
			objDataCell = al_row.get(col);
			setValueAt(objDataCell, row, col);
		}
	}

	
	/**
	 * All column cell editors specified at function declaring time or modified nextly will be applied.<br>
	 * <p>
	 * Tipically, it's possible to use a cell editor for a {@link JComboBox} object.<br>
	 * ComboBox rendering is done as a normal text field but to edit it, a comboBox it's used<br>
	 * and automatically activated by java at click on the cell.<br>
	 * The cell editor specified at declaring time will be, for example:
	 * <pre>new DefaultCellEditor(getJComboBox("cb_table")</pre>
	 * Where <code>cb_table</code> is a specific comboBox object component not to be layed in any panel.<br>
	 * <p>
	 * An other use of a cell editor is for a {@link Color} object.<br>
	 * The cell editor specified at declaring time will be, simply, <code>new ColorEditor()</code>:<br>
	 * Java automatically will call a color chooser dialog at click.<br>
	 * <p>
	 * 
	 */
	public void _applyDeclaredColumnsCellEditor() {
		for (int i = 0; i < al_column.size(); i++) {
			al_column.get(i).applyCellEditor();
		}
	}

	/**
	 * All column cell renderer specified at function declaring time or modified nextly will be applied.<br>
	 * <p>
	 * Tipically, it's possible to use a cell renderer for a {@link Color} object.<br>
	 * Color rendering is done as a normal JLabel managed by java and set to the color value<br>
	 * The cell editor specified at declaring time will be, simply, <br>
	 * <p>
	 * <code>new ColorRenderer(true)</code><br>
	 * <p>
	 * Where <code>true</code> is for a cell bordered with the cell not filled completely,<br>
	 * whereas with <code>false</code> the cell will be filled completely by the color.<br>
	 * <p>
	 */
	public void _applyDeclaredColumnsCellRenderer() {
		for (int i = 0; i < al_column.size(); i++) {
			al_column.get(i).applyCellRenderer();
		}
	}

	/**
	 * All column width specified at function declaring time will be applied.<br>
	 * <p>
	 */
	public void _applyDeclaredColumsWidth() {
		for (int i = 0; i < al_column.size(); i++) {
			al_column.get(i).applyWidth();
		}
	}

	/**
	 * All column resizable status specified at function declaring time will be applied.<br>
	 * <p>
	 */
	public void _applyDeclaredColumsResizable() {
		for (int i = 0; i < al_column.size(); i++) {
			al_column.get(i).applyResizable();
		}
	}

	/**
	 * For each columns, will be called the following methods of the {@link ForwardTableModelColumn} object.<br>
	 * <pre>
	 * applyResizable();
	 * applyWidth();
     * applyCellRenderer();
	 * applyCellEditor();
     * </pre>
     * They are column properties that must be set directly on the table object, to be effective.<br>
     * <p>
     * @see ForwardTableModelColumn
	 */
	public void _applyColumsSettings() {
		for (int i = 0; i < al_column.size(); i++) {
			al_column.get(i).applyAllSettings();
		}
	}

	
	/**
	 * Gets the entity table manager, the been to manage a single badabase table.<br>
	 * <p>
	 * With this information it's possible realize a complete automatic management
	 * of any database table, to populate, delete and inserting rows, thru the JTable.<br>
	 * <p>
	 * The Entity table manager object it's used by forward for automatic access to database<br>
	 * and it describes, by annotations, database field database mapping.<br>
	 * See {@link DataBaseMappedTable}  and  {@link DataBaseMappedColumn}.<br>
	 * <p>    
	 * @return the entityTableManager
	 */
	public Object _getEntityTableManager() {
		return entityTableManager;
	}

	/**
	 * Sets the entity table manager, the been to manage a single badabase table.<br>
	 * <p>
	 * With this information it's possible realize a complete automatic management
	 * of any database table, to populate, delete and inserting rows, thru the JTable.<br>
	 * <p>
	 * The Entity table manager object it's used by forward for automatic access to database<br>
	 * and it describes, by annotations, database field database mapping.<br>
	 * See {@link DataBaseMappedTable}  and  {@link DataBaseMappedColumn}.<br>
	 * <p>    
	 * @param entityTableManager the entityTableManager to set
	 */
	public void _setEntityTableManager(Object entityTableManager) {
		this.entityTableManager = entityTableManager;
	}


	/**
	 * Gets the database table name defined for the table.<br>
	 * <p>
	 * Table name has been retrieved by the entity table manager object set for this model,<br>
	 * using standard annotations defined for it {@link DataBaseMappedTable} set by <code>_setTableDbInfo()</code><p>
	 * 
	 * @return the al_columnDbNameJava
	 */
	public String _getTableDbName() {
		return tableDbName;
	}

	/**
	 * Sets database info to manage the table.<br>
	 * <p>
	 * Thru the entity table manager object,  using annotations {@link DataBaseMappedTable}  and  {@link DataBaseMappedColumn},<br>
	 * are retrieved the database table name, all database column names and all java column names mapped<br>
	 * <p>
	 * With this information it's possible realize a complete automatic management<br>
	 * of any database table, to populate, delete and inserting rows, thru the JTable.<br>
	 * <p>
	 * The Entity table manager object it's used by forward for automatic access to database<br>
	 * and it describes, by annotations, database field database mapping.<br>
	 * <br>
	 * If the object is a not valid entity object no operation will be performed.<br>
	 * <p>
	 * @param alColumnDbNameJava the al_columnDbNameJava to set
	 */
	public void _setTableDbInfo(Object entityTableManager) {
		this.entityTableManager = entityTableManager;
		
		// TODO impostazione tableDbName
		// TODO impostazione al_columnDbName
		// TODO impostazione al_columnDbNameJava
	}

	
}

