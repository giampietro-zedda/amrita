package forward;
import java.io.Serializable;
import java.util.ArrayList;
import javax.swing.JTable;

import enums.EnumForwardBorderType;
import enums.EnumForwardComponent;
import enums.EnumForwardLayout;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;

/**
 * copyright (c) 2009-2011 e-Amrita - ing. Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardToolbar
 * </h1>
 * <p>
 * Models a generic toolbar Gui object with all component.<br>
  * <p>
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardFunction
 *
*/

public class ForwardToolbar implements Serializable{
	
	private static final long serialVersionUID = 1L;
	
	// Reference forward monitor
	private ForwardMonitorDesktop forwardMonitor = null;
	
	// Info generali pannello
	private String panelName = "";									// Nome pannello ad uso interno
	private String panelTitle = "";									// Titolo pannello che compare nel bordo semplice di default
	private EnumForwardPanelType panelType = null;					// Tipologia pannello, ovvero contenitore o applicativo
	private EnumForwardLayout layoutManager = null;					// Layout manager utilizzato per disporre gli altri panel o i campi DETAIL (da ForwardFormLayout) 
    private boolean isPanelBorderDefault = false;                   // Indica se il bordo al pannello deve essere aggiunto	
	
	// Informazioni grafiche.
	// L'oggetto grafico (JComponent) è lo stesso oggetto ForwardPanel, che eredita da JPanel
	// Campi per layout BOX_LAYOUT, FLOW_LAYOUT e CARD_LAYOUT
	private EnumForwardComponent componentType = null;				// Tipo componente grafico codificato da forward
    private int boxLayout_Axis = 0;        							// javax.swing.BoxLayout.X_AXIS, .Y_AXIS, .LINE_AXIS, .PAGE_AXIS
    private int flowLayoutAlignment = 0;        				    // javax.swing.FlowLayout.LEFT, CENTER, RIGHT
    private EnumForwardBorderType borderType = null;                // Tipo bordo pannello
    private String borderTitle = "";                    			// Titolo se border titled
    
	// Caratteristiche funzionali pannello.
	private ArrayList<EnumForwardOption> al_panelOption = null;
	
	// Supporto multilingua help.
	// Tabella multilingua con key = codtab + panelName
	// Presente 1 campo con la stringa HTML, in lingua: 
	private String tableCodeHelpTechnical = "";    					// Codice tabella sistema tabelle forward Html con help di tipo tecnico specialistico     
	private String tableCodelHelpEndUser = "";              		// Codice tabella sistema tabelle forward Html con help di tipo funzionale per l'utente finale 
	
	// Supporto multilingue caption panel e descrizioni campi.
	// Questi valori vanno in override di eventuali descrizioni già presenti per il metacampo.
	// In alcuni pannelli si potrebbero volere descrizioni diverse da quella generale per il metacampo
	// Tabella multilingua con key = codtab + fieldName 
	// Presenti 1 campo dati con il testo in lingua 
	// Se fieldName = "Panel" il testo in lingua si riferisce alla caption del pannello
	private String tableCodeCaptions = "";          				// Codice tabella multilingua per caption pannello e descrizioni campi
    
	// La vista logica di popolamento dati viene impostata con una
	// dichiarazione DATA_SOURCE(nomePanel,...) nella dichiarazione generale della funzione
	  
	// Informazioni se pannello PANEL_GRID
	private String gridTableName = "";								// Nome tabella
	private String gridToolTipText = "";							// ToolTip specifico tabella
	private JTable gridGraphicObject = null;						// Oggetto grafico JTable che implementa panel type GRID
	private ArrayList<InnerTableColumn>	 al_gridColumn = null;  	// Colonne in tabella 
	
	// Pannelli e campi se pannello TABBED
	private ArrayList<InnerForwardTabbedForm> al_tabbedForm = null;	// Ogni elemento descrive un form, al limite di un solo pannello	
	
	// Informazioni se pannello PANEL_SPLIT
	private String splitPanelLeftOrTop = "";						// Nome pannello a sinistra o in alto
	private String splitPanelRightOrDown = "";						// Nome pannello a destra o in basso
	
	// Contenuto di dettaglio se pannello DETAIL
	private ArrayList<ForwardPanelComponent> al_panelField = null;  	// Elenco descrittori campi con informazioni applicative e GUI
	
	
	/*
	 * Costruttore vuoto
	 */
	public ForwardToolbar() {
		super();
		this.al_tabbedForm = new ArrayList<InnerForwardTabbedForm> ();
		this.al_panelField = new ArrayList<ForwardPanelComponent> ();
		this.al_panelOption = new ArrayList<EnumForwardOption> ();
		this.al_gridColumn = new ArrayList<InnerTableColumn> ();
	}
	 
	/*
	 * Costruttore
	 */
	public ForwardToolbar(String panelName, EnumForwardPanelType panelType, 	EnumForwardLayout layoutManager) {
		super();
	
		this.panelName = panelName;
		this.panelType = panelType;
		this.layoutManager = layoutManager;
		this.al_tabbedForm = new ArrayList<InnerForwardTabbedForm> ();
		this.al_panelField = new ArrayList<ForwardPanelComponent> ();
	}
	 
	
	

	/**
	 * Get the reference to forward monitor.<br>
	 * <p>
	 * @return the forwardMonitor
	 */
	public ForwardMonitorDesktop _getForwardMonitor() {
		return forwardMonitor;
	}

	/**
	 * Set the reference to forward monitor.<br>
	 * <p>
	 * @param forwardMonitor the forwardMonitor to set
	 */
	public void _setForwardMonitor(ForwardMonitorDesktop forwardMonitor) {
		this.forwardMonitor = forwardMonitor;
	}

	/**
	 * Restituisce il nome del pannello descritto da questa classe.<br>
	 * <p>
	 * @return the panelName
	 */
	public String _getPanelName() {
		return panelName;
	}



	/**
	 * Imposta il nome del pannello descritto da questa classe.<br>
	 * <p>
	 * @param panelName the panelName to set
	 */
	public void _setPanelName(String panelName) {
		this.panelName = panelName;
	}



	/**
	 * Get the title displayed with border, left aligned by default
	 * <p>
	 * @return the panelTitle
	 */
	public String _getPanelTitle() {
		return panelTitle;
	}

	/**
	 * Set the title displayed with border, left aligned by default
	 * <p>
	 * @param panelTitle the panelTitle to set
	 */
	public void _setPanelTitle(String panelTitle) {
		this.panelTitle = panelTitle;
	}

	/**
	 * Restituisce la tipologia del pannello.<br>
	 * <p>
	 * @return the panelType
	 */
	public EnumForwardPanelType _getPanelType() {
		return panelType;
	}



	/**
	 * Imposta la tipologia del pannello.<br>
	 * <p>
	 * @param panelType the panelType to set
	 */
	public void _setPanelType(EnumForwardPanelType panelType) {
		this.panelType = panelType;
	}



	/**
	 * Get if the default border is active.<br>
	 * <p>
	 * @return the isPanelBorderDefault
	 */
	public boolean _isPanelBorderDefault() {
		return isPanelBorderDefault;
	}

	/**
	 * Set if the default border is active.<br>
	 * <p>
	 * @param isPanelBorderDefault the isPanelBorderDefault to set
	 */
	public void _setPanelBorderDefault(boolean isPanelBorderDefault) {
		this.isPanelBorderDefault = isPanelBorderDefault;
	}

	/**
	 * Restituisce il tipo di layout manager per il pammello se DETAIL.<br>
	 * <p>
	 * @return the layoutManager
	 */
	public EnumForwardLayout _getLayoutManager() {
		return layoutManager;
	}



	/**
	 * Imposta il tipo di layout manager per il pammello se DETAIL.<br>
	 * <p>
	 * @param layoutManager the layoutManager to set
	 */
	public void _setLayoutManager(EnumForwardLayout layoutManager) {
		this.layoutManager = layoutManager;
	}



	/**
	 * Get the JComponent type as coded by forward.<br>
	 * <p>
	 * @return the componentType
	 */
	public EnumForwardComponent _getComponentType() {
		return componentType;
	}

	/**
	 * Set the JComponent type as coded by forward.<br>
	 * <p>
	 * @param componentType the componentType to set
	 */
	public void _setComponentType(EnumForwardComponent componentType) {
		this.componentType = componentType;
	}

	
	/**
	 * Get the axis where to lay out components.<br>
	 * <p>
	 * The value is specified by javax.swing.BoxLayout.X_AXIS, .Y_AXIS, .LINE_AXIS, .PAGE_AXIS<br>
	 * <p>
	 * @return the boxLayout_Axis
	 */
	public int _getBoxLayout_Axis() {
		return boxLayout_Axis;
	}

	/**
	 * Set the axis where to lay out components for BOX_LAYOUT.<br>
	 * <p>
	 * The value is specified by javax.swing.BoxLayout.X_AXIS, .Y_AXIS, .LINE_AXIS, .PAGE_AXIS<br>
	 * <p>
	 * @param boxLayoutAxis the boxLayout_Axis to set
	 */
	public void _setBoxLayout_Axis(int boxLayoutAxis) {
		boxLayout_Axis = boxLayoutAxis;
	}

	/**
	 * Get the alignment for FLOW_LAYOUT.<br>
	 * <p>
	 * The value is specified by javax.swing.FlowLayout.LEFT, CENTER, RIGHT<br>
	 * <p>
	 * @return the flowLayoutAlignment
	 */
	public int _getFlowLayoutAlignment() {
		return flowLayoutAlignment;
	}

	/**
	 * Set the alignment for FLOW_LAYOUT.<br>
	 * <p>
	 * The value is specified by javax.swing.FlowLayout.LEFT, CENTER, RIGHT<br>
	 * <p>
	 * @param flowLayoutAlignment the flowLayoutAlignment to set
	 */
	public void _setFlowLayoutAlignment(int flowLayoutAlignment) {
		this.flowLayoutAlignment = flowLayoutAlignment;
	}

	
	
	/**
	 * Get the border type as coded by forward.<br>
	 * <p>
	 * @return the borderType
	 */
	public EnumForwardBorderType _getBorderType() {
		return borderType;
	}

	/**
	 * Set the border type as coded by forward.<br>
	 * <p>
	 * @param borderType the borderType to set
	 */
	public void _setBorderType(EnumForwardBorderType borderType) {
		this.borderType = borderType;
	}

	/**
	 * Get the border title for titled borders.<br>
	 * <p>
	 * @return the borderTitle
	 */
	public String _getBorderTitle() {
		return borderTitle;
	}

	/**
	 * Get the border title for titled borders.<br>
	 * <p>
	 * @param borderTitle the borderTitle to set
	 */
	public void _setBorderTitle(String borderTitle) {
		this.borderTitle = borderTitle;
	}

	/**
	 * Get the JPanel, JDialog, JTabbedPane, JSplitPane graphic object<br>
	 * implemented by panel.
	 * <p>
	 * @return the graphicObject
	 */
	public Object _getGraphicObject() {
		return this;
	}


	/**
	 * Restituisce il codice tabella con il codice HTML di help a livello tecnico specializtico per il pannello.<br>
	 * <p>
	 * La tabella usufruisce della gestione del supporto multilingua.
	 * <p>
	 * 
	 * @return the tableCodeHelpTechnical
	 */
	public String _getTableCodeHelpTechnical() {
		return tableCodeHelpTechnical;
	}



	/**
	 * Imposta il codice tabella con il codice HTML di help a livello tecnico specializtico per il pannello.<br>
	 * <p>
	 * La tabella usufruisce della gestione del supporto multilingua.
	 * <p>
	 * 
	 * @param tableCodeHelpTechnical the tableCodeHelpTechnical to set
	 */
	public void _setTableCodeHelpTechnical(String tableCodeHelpTechnical) {
		this.tableCodeHelpTechnical = tableCodeHelpTechnical;
	}



	/**
	 * Restituisce il codice tabella con il codice HTML di help a livello funzionale/applicativo per il pannello.<br>
	 * <p>
	 * La tabella usufruisce della gestione del supporto multilingua.
	 * <p>
	 * 
	 * @return the tableCodelHelpEndUser
	 */
	public String _getTableCodelHelpEndUser() {
		return tableCodelHelpEndUser;
	}



	/**
	 * Imposta il codice tabella con il codice HTML di help a livello funzionale/applicativo per il pannello.<br>
	 * <p>
	 * La tabella usufruisce della gestione del supporto multilingua.
	 * <p>
	 * 
	 * @param tableCodelHelpEndUser the tableCodelHelpEndUser to set
	 */
	public void _setTableCodelHelpEndUser(String tableCodelHelpEndUser) {
		this.tableCodelHelpEndUser = tableCodelHelpEndUser;
	}



	/**
	 * Restituisce il codice tabella per il supporto multilingua del caption del pannello e delle descrizioni dei campi.<br>
	 * <p>
	 * Le descrizioni in lingua di questa tabella vanno in override di eventuali descrizioni già presenti per il metacampo, <br>
	 * in quanto in alcuni pannelli si potrebbero volere descrizioni diverse da quella generale per il metacampo<br>
	 * Tabella multilingua con key = codtab + fieldName 
	 * Presenti 1 campo dati con il testo in lingua 
	 * Se fieldName = "Panel" il testo in lingua si riferisce alla caption del pannello
	 * <br>
	 * @return the tableCodeCaptions
	 */
	public String _getTableCodeCaptions() {
		return tableCodeCaptions;
	}


	/**
	 * Imposta il codice tabella per il supporto multilingua del caption del pannello e delle descrizioni dei campi.<br>
	 * <p>
	 * Le descrizioni in lingua di questa tabella vanno in override di eventuali descrizioni già presenti per il metacampo, <br>
	 * in quanto in alcuni pannelli si potrebbero volere descrizioni diverse da quella generale per il metacampo<br>
	 * Tabella multilingua con key = codtab + fieldName 
	 * Presenti 1 campo dati con il testo in lingua 
	 * Se fieldName = "Panel" il testo in lingua si riferisce alla caption del pannello
	 * <br>
	 * @param tableCodeCaptions the tableCodeCaptions to set
	 */
	public void _setTableCodeCaptions(String tableCodeCaptions) {
		this.tableCodeCaptions = tableCodeCaptions;
	}



	/**
	 * Restituisce i tabbed form attivabili nel tabbedPanel.<br>
	 * <p>
	 * Viene restituito un ArrayList di oggetti della classe interna <br>
	 * InnerForwardTabbedForm<br>
	 * <p>
	 * @return the al_tabbedForm
	 */
	public ArrayList<InnerForwardTabbedForm> _getTabbedForms() {
		return al_tabbedForm;
	}



	/**
	 * Accoda il descrittore di form tabbed e la sua caption.<br>
	 * <p>
	 * @param alTabbedForm the al_tabbedForm to set
	 */
	public void _addTabbedForm(String tabCaption, String formName) {
		InnerForwardTabbedForm innerForwardTabbedForm = null;
		innerForwardTabbedForm = new InnerForwardTabbedForm();
		innerForwardTabbedForm.tabCaption = tabCaption;
		innerForwardTabbedForm.formName = formName;
		al_tabbedForm.add(innerForwardTabbedForm);
	}




	/**
	 * Restituisce i campi e gli oggetti grafici da posizionare<br>
	 * nel pannello. Ogni campo contiene le informazioni necessarie<br>
	 * per la sua visualizzazione, come il numero di riga, colonna, <br>
	 * numero di righe e colonne etc.<br>
	 * <p>
	 * @return the al_panelField
	 */
	public ArrayList<ForwardPanelComponent> _getPanelFields() {
		return al_panelField;
	}

	/**
	 * Restituisce il campo con o l'oggetto grafico con il nome specificato <br>
	 * definito nel pannello<br>
	 * Ogni campo contiene le informazioni necessarie per la sua visualizzazione, 
	 * come il numero di riga, colonna, numero di righe e colonne etc.<br>
	 * <p>
	 * Se il nome fornito è inesistente restituisce null.<br>
	 * <p>
	 * @return the al_panelField
	 */
	public ForwardPanelComponent _getPanelField(String fieldName) {
		ForwardPanelComponent panelField = null;
		
		// Scan oggetti definiti nel pannello
		for (ForwardPanelComponent panelFieldLoop : al_panelField) {
			if (panelFieldLoop.getName().equals(fieldName)) {
				return panelField;
			}
		}
		
		return panelField;
	}


	/**
	 * Returns all options declared for the panel<br>
	 * Every option causes a different behaviour, type of image, rendering and so on.<br>
	 * <p>
	 * @return the al_panelOption
	 */
	public ArrayList<EnumForwardOption> _getPanelOptions() {
		return al_panelOption;
	}


	/**
	 * Accoda il descrittore di un campo o di un oggetto grafico da posizionare<br>
	 * nel pannello. Ogni campo contiene le informazioni necessarie<br>
	 * per la sua visualizzazione, come il numero di riga, colonna, <br>
	 * numero di righe e colonne etc.<br>
	 * <p>
	 * @param alPanelField the al_panelField to set
	 */
	public void _addPanelField(ForwardPanelComponent panelField) {
		al_panelField.add(panelField);
	}



	/**
	 * Returns the table name, name of the object {@link JTable}<br>
	 * implemented by the panel <code>PANEL_GRID</code>.<br>
	 * <p>
	 * @return the gridTableName
	 */
	public String _getGridTableName() {
		return gridTableName;
	}

	/**
	 * Set the table name, name of the object {@link JTable}<br>
	 * implemented by the panel <code>PANEL_GRID</code>.<br>
	 * <p>
	 * @param gridTableName the gridTableName to set
	 */
	public void _setGridTableName(String gridTableName) {
		this.gridTableName = gridTableName;
	}

	
	/**
	 * Get the {@link JTable} swing graphic object used to implement the GRID panel type.<br>
	 * <p>
	 * @return the gridGraphicObject
	 */
	public JTable _getGridGraphicObject() {
		return gridGraphicObject;
	}

	/**
	 * Set the {@link JTable} swing graphic object used to implement the GRID panel type.<br>
	 * <p>
	 * @param gridGraphicObject the gridGraphicObject to set
	 */
	public void _setGridGraphicObject(JTable gridGraphicObject) {
		this.gridGraphicObject = gridGraphicObject;
	}



	/**
	 * Get the split panel left or top when the panel type is SPLIT.<br>
	 * <p>
	 * @return the splitPanelLeftOrTop
	 */
	public String _getSplitPanelLeftOrTop() {
		return splitPanelLeftOrTop;
	}

	/**
	 * Set the split panel left or top when the panel type is SPLIT.<br>
	 * <p>
	 * @param splitPanelLeftOrTop the splitPanelLeftOrTop to set
	 */
	public void _setSplitPanelLeftOrTop(String splitPanelLeftOrTop) {
		this.splitPanelLeftOrTop = splitPanelLeftOrTop;
	}

	/**
	 * Get the split panel right or down when the panel type is SPLIT.<br>
	 * <p>
	 * @return the splitPanelRightOrDown
	 */
	public String _getSplitPanelRightOrDown() {
		return splitPanelRightOrDown;
	}

	/**
	 * Set the split panel right or down when the panel type is SPLIT.<br>
	 * <p>
	 * @param splitPanelRightOrDown the splitPanelRightOrDown to set
	 */
	public void _setSplitPanelRightOrDown(String splitPanelRightOrDown) {
		this.splitPanelRightOrDown = splitPanelRightOrDown;
	}


	/**
	 * Returns the toolTip text of the object {@link JTable}<br>
	 * implemented by the panel <code>PANEL_GRID</code>.<br>
	 * <p>
	 * @return the gridToolTipText
	 */
	public String _getGridToolTipText() {
		return gridToolTipText;
	}

	/**
	 * Set the toolTip text of the object {@link JTable}<br>
	 * implemented by the panel <code>PANEL_GRID</code>.<br>
	 * <p>
	 * @param gridToolTipText the gridToolTipText to set
	 */
	public void _setGridToolTipText(String gridToolTipText) {
		this.gridToolTipText = gridToolTipText;
	}

	/**
	 * Set the toolTip text of the object {@link JTable}<br>
	 * implemented by the panel <code>PANEL_GRID</code>.<br>
	 * <p>
	 * @param gridToolTipText the gridToolTipText to set
	 */
	public void _addGridColumn(String columnName, String columnHeader, String columnNameLdv, String columnToolTipText, Class<?> columnRenderType) {
		
		InnerTableColumn innerTableColumn = null;
		innerTableColumn = new InnerTableColumn();
		
		innerTableColumn.columnName = columnName;
		innerTableColumn.columnNameLdv = columnNameLdv;
		innerTableColumn.columnHeader = columnHeader; 
		innerTableColumn.columnToolTipText = columnToolTipText;
		innerTableColumn.columnRenderType = columnRenderType;
		this.al_gridColumn.add(innerTableColumn);
	}



	/*
	 * Info panel da collocare nel tabbed place.
	 * Viene definito un pannello container di servizio e 
	 * un oggetto ForwardForm descrive la struttura di questo pannello,
	 * assimilabile a un form vero e proprio.
	 */
	 @SuppressWarnings("unused")
	private class InnerForwardTabbedForm{
		 
			String tabCaption = "";			// Caption 
			String formName = null;			// Nome form descritto da ForwardForm
		 
		 /* Costruttore */
		 private InnerForwardTabbedForm() {
		}
	 }
	 
	/*
	 * Descrittore colonna tabella implementata da JTable e cablata in un pannello PANEL_GRID.
	*/
	@SuppressWarnings("unused")
	private class InnerTableColumn{
		String columnName = "";					// Nome colonna, coincide con il nome variabile utilizzato
		String columnNameLdv = "";				// Nome colonna logical data view di accesso dati
		String columnHeader = "";				// Text header colonna
		String columnToolTipText = "";			// ToolTip specifico colonna
		Class<?> columnRenderType = null;		// Classe oggetto di rendering
	 }
}
