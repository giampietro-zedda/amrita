package forward;
//import guiSunExamples.ColorEditor;

import java.awt.Color;
import java.io.Serializable;

import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import analyzer.DataBaseMappedTable;

/**
 * copyright (c) 2009-2012 e-Amrita - Ing. Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardTableModelColumn
 * </h1>
 * <p>
 * Works with {@link ForwardTableModel}, that extends {@link AbstractTableModel}, and holds all informations related to a JTable column. <br>
 * They are both specific forward informations and standard java table informations stored in classes such as {@link TableColumn}<br>
 * All table informations have been set at function declaring time. 
 * <p>
 * Methods of this class let to get and update any column characteristic.<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 08/dic/2011 
 * @see ForwardTableModel
 *
*/


@SuppressWarnings("rawtypes")
class ForwardTableModelColumn implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private JTable jtable = null;                   // Oggetto swing JTable
	int index = 0;                                  // Numero colonna 0-based
    private String name = "";						// Nome della colonna come conosciuto dall'applicazione
    private String dbName = "";						// Nome della colonna db nel database
    private String dbNameJava = "";					// Nome java della colonna db 
    private String header = "";						// Intestazione della colonna
 	private Class classType = null;					// Classe indicante il tipo colonna
    private boolean isEditable = false;				// true se editabile e false se non editabile
    private boolean isResizable = true;				// true se resizabile e false se non resizabile
    private boolean isHidden = true;				// true se la colonna non è visualizzata
    private String toolTipText = "";				// ToolTip di colonna
    private int widthOriginal = -1;					// Larghezza colonna impostata a width originale o successive modificazioni, usata se idHidden è true
    private int width = -1;						    // Larghezza preferred, se -1 si lascia quella predefinita
    private TableCellRenderer cellRenderer = null;	// Renderer da utilizzare o null se resta il default
    private TableCellEditor cellEditor = null;		// Editor da utilizzare o null se resta il default
   
 
    /** Constructor */
	public ForwardTableModelColumn(JTable jtable, int index) {
 		super();
 		this.jtable = jtable;
 		this.index = index;
  	}

     
	/**
	 * Gets the JTable object the this model pupulates.<br>
	 * <p>
	 * @return the jtable
	 */
	public JTable getJtable() {
		return jtable;
	}

	/**
	 * Sets the JTable object the this model pupulates.<br>
	 * >p>
	 * @param jtable the jtable to set
	 */
	public void setJtable(JTable jtable) {
		this.jtable = jtable;
	}

	
	
	/**
	 * Gets the column index, 0-based<br>
	 * <p>
	 * @return the index
	 */
	public int getIndex() {
		return index;
	}


	/**
	 * Sets the column index, 0-based<br>
	 * <p>
	 * @param index the index to set
	 */
	public void setIndex(int index) {
		this.index = index;
	}


	/**
	 * Gets the name of the column as known by the application.<br>
	 * Normally is the same name of the java name of the column, in the class managing the table on db,<br>
	 * to let the automatic binding.<br>
	 * In any case this is the name assigned at function declaring time.<br>
	 * <p>
	 * @return the name assigned to the column
	 */
	public String getName() {
		return name;
	}


	/**
	 * Sets the name of the column as known by the application.<br>
	 * Normally is the same name of the java name of the column, in the class managing the table on db,<br>
	 * to let the automatic binding.<br>
	 * In any case this is the name assigned at function declaring time.<br>
	 * <p>
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}


	/**
	 * Gets columns type as class objects.<br>
	 * <p>
	 * @return the classType
	 */
	public Class getClassType() {
		return this.classType;
	}

	/**
	 * Sets columns type as class objects.<br>
	 * <p>
	 * @param classType the classType to set
	 */
	public void setClassType(Class classType) {
		this.classType = classType;
	}

	/**
	 * Gets column header text of the table.<br>
	 * <p>
	 * @return the header of the column
	 */
	public String getHeader() {
		return this.header;
	}


	/**
	 * Sets columns header text of the table.<br>
	 * <p>
	 * @param al_columnHeader the al_columnHeader to set
	 */
	public void setHeader(String header) {
		this.header = header;
	}


	/**
	 * Gets if the column is editable.<br>
	 * <p>
	 * @return the editable column status as a boolean
	 */
	public boolean isEditable() {
		return this.isEditable;
	}

	/**
	 * Sets if the column is editable.<br>
	 * <p>
	 * @param isEditable the boolean value to set
	 */
	public void setEditable(boolean isEditable) {
		this.isEditable = isEditable;
	}



	/**
	 * Gets if the column is resizable.<br>
	 * <p>
	 * @return the resizable column status as a boolean
	 */
	public boolean isResizable() {
		return this.isResizable;
	}

	/**
	 * Sets if the column is resizable.<br>
	 * <p>
	 * @param isResizable the boolean value to set
	 */
	public void setResizable(boolean isResizable) {
		this.isResizable = isResizable;
	}


	
	
	/**
	 * Gets if the column is hidden, therefore declared but not painted.<br>
	 * <p>
	 * @return the isHidden
	 */
	public boolean isHidden() {
		return isHidden;
	}


	/**
	 * Sets if the column is hidden, therefore declared but painted or not.<br>
	 * <p>
	 * @param isHidden the isHidden to set
	 */
	public void setHidden(boolean isHidden) {
		this.isHidden = isHidden;
		// Column to hide
		if (isHidden) {
			if (this.width >= 0) {
				this.widthOriginal = this.width;
			}
			this.width = 0;
			jtable.getColumnModel().getColumn(this.index).setMinWidth(0);
			jtable.getColumnModel().getColumn(this.index).setMaxWidth(0);						// Necessario altrimenti swing non mette a zero le dimensioni
			jtable.getColumnModel().getColumn(this.index).setPreferredWidth(0);
		} else { // Column to show
			this.width = this.widthOriginal;
			jtable.getColumnModel().getColumn(this.index).setMinWidth(this.width);
			jtable.getColumnModel().getColumn(this.index).setMaxWidth(Integer.MAX_VALUE);	
			jtable.getColumnModel().getColumn(this.index).setPreferredWidth(this.width);

		}
	}


	/**
	 * Gets the original column width declared by function or any further non zero modified value.<br>
	 * <p>
	 * @return the widthOriginal
	 */
	public int getWidthOriginal() {
		return widthOriginal;
	}


	/**
	 * Sets the original column width declared or any further modified value.<br>
	 * <p>
	 * @param widthOriginal the widthOriginal to set
	 */
	public void setWidthOriginal(int widthOriginal) {
		this.widthOriginal = widthOriginal;
	}


	/**
	 * Gets the column width.<br>
	 * <p>
	 * The column width is used to set the preferred size of the column <br>
	 * A <code>-1</code> value means no width setting and with default active.<br>.
	 * A value greater then 0 set just the preferred width size that can be changed resizing the column,<br>
	 * if it's enabled.<br>
	 * <p>
	 * @return the column width
	 */
	public int getWidth() {
		return this.width;
	}

	/**
	 * Sets the column width.<br>
	 * <p>
	 * The column width is used to set the preferred size of the column <br>
	 * A <code>-1</code> value means no widht setting and with default active.<br>.
	 * A value greater then 0 set just the preferred width size that can be changed resizing the column,<br>
	 * if it's enabled.<br>
	 * <p>
	 * @param width the column width to set
	 */
	public void setWidth(int width) {
		this.width = width;
		applyWidth();
	}


	/**
	 * Gets the header tooltip text.<br>
	 * <p>
	 * @return the toolTipText
	 */
	public String getToolTipText() {
		return this.toolTipText;
	}

	/**
	 * Sets the header tooltip text.<br>
	 * <p>
	 * @param al_columnToolTip the al_columnToolTip to set
	 */
	public void setToolTipText(String toolTipText) {
		this.toolTipText = toolTipText;
	}


	/**
	 * Gets the column cell renderer.<br>
	 * <p>
	 * For each cell of the column, the renderer manage how to show the cell<br>
	 * If renderer is null, java will use a standard default renderer depending on the column type,<br>
	 * as coded by the class column type.<br>
	 * <br>
	 * here an example of standard java of renderer using a combobox in column number 3:
	 * <pre>
	 *  JTable table;
	 *  ..
	 *  JComboBox comboBox = new JComboBox();
        comboBox.addItem("Snowboarding");
        comboBox.addItem("Rowing");
        table.getColumnModel().getColumn(2).setCellEditor(new DefaultCellEditor(comboBox));
 	 * </pre>
 	 * In the forward environment the last instruction is:
 	 * <pre>
 	 * setCellRenderer(new DefaultCellEditor(comboBox))
 	 * </pre>
	 * For a column with {@link Color} object the render is set by:
 	 * <pre>
        setCellRenderer(new ColorRenderer(true));
        </pre>
	 * <p>
	 * @return the al_columnCellRenderer
	 */
	public TableCellRenderer getCellRenderer() {
		return this.cellRenderer;
	}

	/**
	 * Sets the column cell renderer.<br>
	 * <p>
	 * For each cell of the column, the renderer manage how to show the cell<br>
	 * If renderer is null, java will use a standard default renderer depending on the column type,<br>
	 * as coded by the class column type.<br>
	 * <p>
	 * here an example of standard java of renderer using a combobox in column number 3:
	 * <pre>
	 *  JTable table;
	 *  ..
	 *  JComboBox comboBox = new JComboBox();
        comboBox.addItem("Snowboarding");
        comboBox.addItem("Rowing");
        table.getColumnModel().getColumn(2).setCellEditor(new DefaultCellEditor(comboBox));
  
        //Sets up tool tips for the cell number 2
        DefaultTableCellRenderer renderer = new DefaultTableCellRenderer();
        renderer.setToolTipText("Click for combo box");
        table.getColumnModel().getColumn(2)..setCellRenderer(renderer);

 	 * </pre>
 	 * In the forward environment the last instruction is:
 	 * <pre>
 	 * _getColumnsCellrenderer().set(2, new DefaultTableCellRenderer())
 	 * </pre>
 	 * For a column with {@link Color} object the render is set by:
 	 * <pre>
        table.getColumnModel().getColumn(2).setCellRenderer(new ColorRenderer(true));
        </pre>
	 * <p>
	 * @param al_columnCellRenderer the al_columnCellRenderer to set
	 */
	public void setCellRenderer(TableCellRenderer cellRenderer) {
		this.cellRenderer = cellRenderer;
	}

	/**
	 * Gets the column cell editor.<br>
	 * <p>
	 * Each element, one for each column, is a editor object for any cell of the column<br>
	 * <br>
	 * here an example of standard java of editor using a combobox in column number 3:
	 * <pre>
	 *  JTable table;
	 *  ..
	 *  JComboBox comboBox = new JComboBox();
        comboBox.addItem("Snowboarding");
        comboBox.addItem("Rowing");
        table.getColumnModel().getColumn(2).setCellEditor(new DefaultCellEditor(comboBox));
 	 * </pre>
 	 * In the forward environment the last instruction is:
 	 * <pre>
 	 * _getColumnsCellEditor.set(2, new DefaultCellEditor(comboBox))
 	 * </pre>
	 * For a column with {@link Color} object the editor is set by:
 	 * <pre>
        table.getColumnModel().getColumn(2).setCellEditor(new ColorEditor());
        </pre>
	 * <p>
	 * @return the cell editor for the column
	 */
	public TableCellEditor getCellEditor() {
		return this.cellEditor;
	}

	/**
	 * Sets the column cell editor.<br>
	 * <p>
	 * Each element, one for each column, is a editor object for any cell of the column<br>
	 * <br>
	 * here an example of standard java of editor using a combobox in column number 3:
	 * <pre>
	 *  JTable table;
	 *  ..
	 *  JComboBox comboBox = new JComboBox();
        comboBox.addItem("Snowboarding");
        comboBox.addItem("Rowing");
        table.getColumnModel().getColumn(2).setCellEditor(new DefaultCellEditor(comboBox));
 	 * </pre>
 	 * In the forward environment the last instruction is:
 	 * <pre>
 	 * _getColumnsCellEditor.set(2, new DefaultCellEditor(comboBox))
 	 * </pre>
	 * For a column with {@link Color} object the editor is set by:
 	 * <pre>
        table.getColumnModel().getColumn(2).setCellEditor(new ColorEditor());
        </pre>
	 * <p>
	 * @param cellEditor the cellEditor to set
	 */
	public void setCellEditor(TableCellEditor cellEditor) {
		this.cellEditor = cellEditor;
	}

	

	/**
	 * Sets a specific cell editor to edit colors<br>
	 * <p>
	 * It's a convenient method to quickly set an editor to choose colors.<br>
	 * Java automatically will show a dialog to choose a color on click event.<br>
	 * If the column number is out of range, no action will be taken.<br>
	 * If the column type is not {@link Color}, no action will be taken.<br>
	 * <p>
	 */
	public void setCellEditorForColor() {
		if (this.classType != Color.class) {return;}
//		this.cellEditor = new ColorEditor();
	}

	/**
	 * Sets a specific cell editor to edit a text field with a comboBox<br>
	 * <p>
	 * If the column type is not {@String}, no action will be taken.<br>
	 * Java automatically will open the combo to choose an item.<br>
	 * ComboBox object must be declared for the function and normally not layed in any panel.
	 * <p>
	 * @param jcomboBox the {@link JComboBox} to use to edit the cell
	 */
	public void setCellEditorForComboBox(JComboBox jcomboBox) {
		if (classType != String.class) {return;}
		this.cellEditor = new DefaultCellEditor(jcomboBox);
	}


	/**
	 * Gets the java db name defined for the table in the entity class.<br>
	 * <p>
	 * Name has been retrieved by the entity table manager object set for this model,<br>
	 * using standard annotations defined for it {@link DataBaseMappedTable}  and  {@link DataBaseMappedColumn},<br>
	 * 
	 * @return the dbNameJava
	 */
	public String getDbNameJava() {
		return this.dbNameJava;
	}

	/**
	 * Gets the database name of the column<br>
	 * <p>
	 * Name has been retrieved by the entity table manager object set for this model,<br>
	 * using standard annotations defined for it {@link DataBaseMappedTable}  and  {@link DataBaseMappedColumn},<br>
	 * 
	 * @return the dbName
	 */
	public String getDbName() {
		return this.dbName;
	}

	/**
	 * The cell editor currently active will be applied to the table.<br>
	 * <p>
	 * 
	 */
	public void applyCellEditor() {
		if (jtable.getColumnModel().getColumnCount() == 0) {return;}
		if (jtable.getColumnModel().getColumnCount() == 0) {return;}
		jtable.getColumnModel().getColumn(this.index).setCellEditor(this.cellEditor);
	}

	/**
	 * The cell renderer currently active will be applied to the table.<br>
	 * <p>
	 */
	public void applyCellRenderer() {
		if (jtable.getColumnModel().getColumnCount() == 0) {return;}
		jtable.getColumnModel().getColumn(this.index).setCellRenderer(this.cellRenderer);
	}

	/**
	 * The column width currently active will be applied to the table<br>
	 * <p>
	 * If the column width is less then zero no action will be taken.<br>
	 * The <code>preferredWith</code> property will be set too.<br>
	 * <p>
	 */
	public void applyWidth() {
		if (jtable.getColumnModel().getColumnCount() == 0) {return;}
		if (this.width < 0) {return;}				// Si lascia fare a java
		jtable.getColumnModel().getColumn(this.index).setWidth(this.width);
		jtable.getColumnModel().getColumn(this.index).setPreferredWidth(this.width);
	}

	/**
	 * The column content fit width currently active will be applied to the table<br>
	 * <p>
	 * If the column width is less then zero no action will be taken.<br>
	 * <p>
	 */
	public void applySizeWidthToFit() {
		if (jtable.getColumnModel().getColumnCount() == 0) {return;}
		jtable.getColumnModel().getColumn(this.index).sizeWidthToFit();
	}

	/**
	 * The table column will be set to be resizable.<br>
	 * <p>
	 */
	public void applyResizable() {
		if (jtable.getColumnModel().getColumnCount() == 0) {return;}
		jtable.getColumnModel().getColumn(this.index).setResizable(this.isResizable);
	}


	/**
	 * Will be executed the following methods:<pre>
	 * applyResizable()
	 * applyWidth()
	 * applyCellRenderer()
	 * applyCellEditor()
	 * <p>
	 */
	public void applyAllSettings() {
		 applyResizable();
		 applyWidth();
		 applyCellRenderer();
		 applyCellEditor();
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Name:"+name;
	}


}

