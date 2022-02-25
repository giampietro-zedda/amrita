package forward;
import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.FlowLayout;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.io.Serializable;
import java.util.ArrayList;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
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
 * ForwardPanel
 * </h1>
 * <p>
 * Questa classe descrive un pannello, con tutte le caratteristiche minime codificate da Forward, per una corretta gestione della GUI.<br>
 * In particolare viene memorizzato il suo gestore di layout e i suoi campi, modellati dalla classe {@link ForwardPanelComponent}.<br>
 * <p>
 * Le caratteristiche minime rappresentano i parametri essenziali per la visualizzazione del<br>
 * pannello, come le dimensioni massime, minime e preferite e pochi altri parametri.<br>
 * Qualsiasi proprietà particolare o specifica viene eventualmente inserita nel codice java<br>
 * di eccezione, a livello di funzione. Tale codice viene eseguito da Forward all'inizializzazione<br>
 * dell'interfaccia.<br>
 * Questo approccio rende operativamente snella la dichiarazione della funzione applicativa<br>
 * nella maggior parte dei casi, senza limitazioni nella gestione di casi specifici.<br>
 * Inoltre l'approccio di <code>Forward</code> alle problematiche multipiattaforma mette al centro<br>
 * gli stessi oggetti java sia a livello di Desktop sia di server.<br>
 * Mentre a livello Desktop gli oggetti grafici sono già naturalmente pronti per comporre la GUI<br>
 * applicativa, a livello server, per applicazioni Web, contengono esaustivamente tutte le informazioni<br>
 * per generare codice HTML equivalente.<br>
 * <p>
 * Un pannello può essere riutilizzato senza modifiche in più applicazioni è può essere<br>
 * caratterizzato da una propria strategia di accesso ai dati per il suo popolamento.<br>
 * <p>
 * E' prevista tutta una serie di pannelli predefiniti in grado di implementare la maggior parte<br>
 * delle necessità applicative. Si tratta di funzionalità applicative complesse riusabili.<br>
 * L'elenco delle funzionalità sviluppate da pannelli specializzati è codificata in {@link EnumForwardPanelType}.<br>
 * Queste funzionalità sono codificate in classi java che ereditano da <code>JPanel</code>e che normalmente<br>
 * utilizzano altri pannelli interni per implementare la funzionalità prevista.<br>
 * Dal punto di vista applicativo la funzione forward deve solo dichiarare l'utilizzo di tali pannelli<br>
 * predefiniti collocandoli nella struttura dei pannelli che compongono l'applicazione.<br>
 * Questi pannelli applicativi sono completamente integrati con il monitor GUI di Forward e sono completamente<br>
 * riutilizzabili in applicazioni diverse.
 * <p>
 * Le logiche applicative non sono codificate a livello di pannello ma a livello della dichiarazione<br>
 * della funzione applicativa.<br>
 * <p>
 * Come già accennato, un pannello è identificato da una tipologia che può andare dal tipo pannello contenitore, <br>
 * al tipo pannello dati, contenente i componenti grafici di interazione con l'utente.<br>
 * A parte il pannello di tipo contenitore, tutte le altre tipologie sono di tipo applicativo<br>
 * ed eventualmente specializzato.<br>
 * <p>
 * E' possibile avere pannelli applicativi a <t>scomparsa</t>, tecnicamente costruiti con due pannelli <br>
 * verticali di cui il primo, sottile, in grado di far scomparire il secondo, pannelli <t>Menu</t>, <br>
 * in grado di implementare menu<br> di pulsanti, etichette, liste, alberi etc, pannelli Table <br>
 * in grado di implementare dati visualizzati a griglia e altri tipi ancora.<br>
 * Il tipo pannello <t>Data</t> definisce gli oggetti grafici elementari ed è implementato utilizzando <br>
 * il gestore di layout <tt>GRID_BAG_LAYOUT</tt><br>
 * Il tipo pannello <t>Data</t> definisce gli oggetti grafici elementari ed è implementato utilizzando <br>
 * il gestore di layout <tt>GRID_BAG_LAYOUT</tt>
 * Il tipo pannello <t>Properties</t> ha l'aspetto di un tipico pannello di proprietà ed utilizza sempre il <br>
 * gestore di layout <tt>GRID_BAG_LAYOUT</tt>
 * <p>
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardFunction
 *
*/

public class ForwardPanel implements Serializable{
	
	private static final long serialVersionUID = 1L;
	
	// Reference forward monitor
	private ForwardMonitorDesktop forwardMonitor = null;
    private ForwardFunction function = null;						// Active function
	
	// Info generali pannello
	private String panelName = "";									// Nome pannello ad uso interno
	private String panelTabText = "";								// Testo pannello in tab di attivazione, se attivato su tabbed panel
	private String panelTitle = "";									// Titolo pannello che compare nel bordo semplice di default
	private EnumForwardPanelType panelType = null;					// Tipologia pannello, ovvero contenitore o applicativo
	private EnumForwardLayout layoutManager = null;					// Layout manager utilizzato per disporre gli altri panel o i controlli swing
    private String popUpMenuName = "";								// Nome popUpMenu specifico da attivare sul pressed/released del mouse sul controllo

	// Informazioni e oggetti grafici Swing.
	// Campi per layout BOX_LAYOUT, FLOW_LAYOUT e CARD_LAYOUT
	private EnumForwardComponent componentType = null;				// Tipo componente grafico codificato da forward (JPanel, JDIalog, ..)
	private Object graphicObject = null;    						// Oggetto grafico (JPanel, JDialog, JSPlitPane, JTabbedPane)
	private FlowLayout flowLayout = null;							// Se FLOW_LAYOUT manager deve essere usato per disporre componenti nel pannello 
	private CardLayout cardLayout = null;							// Se CARD_LAYOUT manager deve essere usato per disporre componenti nel pannello 
	private BorderLayout borderLayout = null;						// Se BORDER_LAYOUT manager deve essere usato per disporre componenti nel pannello 
	private BoxLayout boxLayout = null;								// Se BOX_LAYOUT manager deve essere usato per disporre componenti nel pannello 
	private GridLayout gridLayout = null;							// Se GRID_LAYOUT manager deve essere usato per disporre componenti nel pannello 
	private GridBagLayout gridBagLayout = null;						// Se GRID_BAG_LAYOUT manager deve essere usato per disporre componenti nel pannello 
	
	// Valori essenziali per alcuni layout
	private int boxLayoutAxis = 0;        							// javax.swing.BoxLayout.X_AXIS, .Y_AXIS, .LINE_AXIS, .PAGE_AXIS
    private int flowLayoutAlign = 0;        				    	// javax.swing.FlowLayout.LEFT, CENTER, RIGHT, LEADING, TRAILING
    private int hgap = 0;        				    				// Horizontal gap se FLOW_LAYOUT o GRID_LAYOUT
    private int vgap = 0;        				    				// Vertical gap se FLOW_LAYOUT o GRID_LAYOUT
    private int gridRows = 0;                                       // Numero righe se GRID_LAYOUT o GRID_BAG_LAYOUT
    private int gridColumns = 0;                                    // Numero colonne se GRID_LAYOUT o GRID_BAG_LAYOUT
    private EnumForwardBorderType borderType = null;                // Tipo bordo pannello
    private String borderTitle = "";                    			// Titolo se border titled
    
	// Caratteristiche funzionali pannello.
	private ArrayList<EnumForwardOption> al_panelOption = null;  
	
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
	private ArrayList<ForwardPanelComponent> al_panelComponent = null;  // Elenco descrittori campi con informazioni applicative e GUI
	
	
	/*
	 * Minimal constructor
	 */
	public ForwardPanel() {
		super();
		this.al_tabbedForm = new ArrayList<InnerForwardTabbedForm> ();
		this.al_panelComponent = new ArrayList<ForwardPanelComponent> ();
		this.al_panelOption = new ArrayList<EnumForwardOption> ();
		this.al_gridColumn = new ArrayList<InnerTableColumn> ();
	}
	 
	/*
	 * Full constructor
	 */
	public ForwardPanel(String panelName, EnumForwardPanelType panelType, EnumForwardLayout layoutManager) {
		super();
	
		this.panelName = panelName;
		this.panelType = panelType;
		this.layoutManager = layoutManager;
		this.al_tabbedForm = new ArrayList<InnerForwardTabbedForm> ();
		this.al_panelComponent = new ArrayList<ForwardPanelComponent> ();
		this.al_panelOption = new ArrayList<EnumForwardOption> ();
		this.al_gridColumn = new ArrayList<InnerTableColumn> ();
		JPanel jpanel = new JPanel();
		jpanel.setName(panelName);
		this.graphicObject = jpanel;
	}
	 
	
	

	/**
	 * Get the reference to forward monitor.<br>
	 * <p>
	 * @return the forwardMonitor
	 */
	public ForwardMonitorDesktop getForwardMonitor() {
		return forwardMonitor;
	}

	/**
	 * Set the reference to forward monitor.<br>
	 * <p>
	 * @param forwardMonitor the forwardMonitor to set
	 */
	public void setForwardMonitor(ForwardMonitorDesktop forwardMonitor) {
		this.forwardMonitor = forwardMonitor;
	}

	
	/**
	 * Get the function where the panel is declared.<br>
	 * <p>
	 * @return the function
	 */
	public ForwardFunction getFunction() {
		return function;
	}

	/**
	 * Set the function where the panel is declared.<br>
	 * <p>
	 * @param function the function to set
	 */
	public void setFunction(ForwardFunction function) {
		this.function = function;
	}

	/**
	 * Restituisce il nome del pannello descritto da questa classe.<br>
	 * <p>
	 * @return the panelName
	 */
	public String getName() {
		return panelName;
	}



	/**
	 * Imposta il nome del pannello descritto da questa classe.<br>
	 * <p>
	 * @param panelName the panelName to set
	 */
	public void setName(String panelName) {
		this.panelName = panelName;
	}



	/**
	 * Get the text displayed on activation tab, if the panel is displayed
	 * on a tabbed panel.<br>
	 * <p>
	 * @return the panelTabText
	 */
	public String getTabText() {
		return panelTabText;
	}

	/**
	 * Set the text displayed on activation tab, if the panel is displayed
	 * on a tabbed panel.<br>
	 * <p>
	 * @param panelTabText the panelTabText to set
	 */
	public void setTabText(String panelTabText) {
		this.panelTabText = panelTabText;
	}

	/**
	 * Get the title displayed with border, left aligned by default
	 * <p>
	 * @return the panelTitle
	 */
	public String getTitle() {
		return panelTitle;
	}

	/**
	 * Set the title displayed with border, left aligned by default
	 * <p>
	 * @param panelTitle the panelTitle to set
	 */
	public void setTitle(String panelTitle) {
		this.panelTitle = panelTitle;
	}

	/**
	 * Restituisce la tipologia del pannello.<br>
	 * <p>
	 * @return the panelType
	 */
	public EnumForwardPanelType getType() {
		return panelType;
	}



	/**
	 * Imposta la tipologia del pannello.<br>
	 * <p>
	 * @param panelType the panelType to set
	 */
	public void setType(EnumForwardPanelType panelType) {
		this.panelType = panelType;
	}


	/**
	 * Restituisce il tipo di layout manager per il pammello se DETAIL.<br>
	 * <p>
	 * @return the layoutManager
	 */
	public EnumForwardLayout getLayoutManager() {
		return layoutManager;
	}



	/**
	 * Imposta il tipo di layout manager per il pammello se DETAIL.<br>
	 * <p>
	 * @param layoutManager the layoutManager to set
	 */
	public void setLayoutManager(EnumForwardLayout layoutManager) {
		this.layoutManager = layoutManager;
	}

	/**
	 * Get the popUp menu linked to the panel.<br>
	 * <p>
	 * @return the popUpMenuName
	 */
	public String getPopUpMenuName() {
		return popUpMenuName;
	}


	/**
	 * Set the popUp menu linked to the panel.<br>
	 * <p>
	 * @param popUpMenuName the popUpMenuName to set
	 */
	public void setPopUpMenuName(String popUpMenuName) {
		this.popUpMenuName = popUpMenuName;
	}



	/**
	 * Get the JComponent type as coded by forward.<br>
	 * <p>
	 * @return the componentType
	 */
	public EnumForwardComponent getComponentType() {
		return componentType;
	}

	/**
	 * Set the JComponent type as coded by forward.<br>
	 * <p>
	 * @param componentType the componentType to set
	 */
	public void setComponentType(EnumForwardComponent componentType) {
		this.componentType = componentType;
	}

	/**
	 * Get the Swing panel object<br>
	 * <p>
	 * It could be a JPanel, JDialog, JSplitPane or JTabbedPane<br>
	 * <p>
	 * @return Object Swing
	 */
	public Object getGraphicObject() {
		return graphicObject;
	}


	

	/**
	 * Set the Swing panel object<br>
	 * <p>
	 * It could be a JPanel, JDialog, JSplitPane or JTabbedPane<br>
	 * <p>
	 * @param graphicObject the graphicObject to set
	 */
	public void setGraphicObject(Object graphicObject) {
		this.graphicObject = graphicObject;
	}



	/**
	 * Get the {@link FlowLayout} manager to be used to lay out Swing components
	 * on panel.<br>
	 * <p>
	 * @return the flowLayout
	 */
	public FlowLayout getFlowLayout() {
		return flowLayout;
	}

	/**
	 * Set the {@link FlowLayout} manager to be used to lay out Swing components
	 * on panel.<br>
	 * <p>
	 * @param flowLayout the flowLayout to set
	 */
	public void setFlowLayout(FlowLayout flowLayout) {
		this.flowLayout = flowLayout;
	}

	/**
	 * Get the {@link CardLayout} manager to be used to lay out Swing components
	 * on panel container.<br>
	 * <p>
	 * @return the cardLayout
	 */
	public CardLayout getCardLayout() {
		return cardLayout;
	}

	/**
	 * Set the {@link CardLayout} manager to be used to lay out Swing components
	 * on panel container.<br>
	 * <p>
	 * @param cardLayout the cardLayout to set
	 */
	public void setCardLayout(CardLayout cardLayout) {
		this.cardLayout = cardLayout;
	}

	/**
	 * Get the {@link BorderLayout} manager to be used to lay out Swing components
	 * on panel container.<br>
	 * <p>
	 * @return the borderLayout
	 */
	public BorderLayout getBorderLayout() {
		return borderLayout;
	}

	/**
	 * Set the {@link BorderLayout} manager to be used to lay out Swing components
	 * on panel container.<br>
	 * <p>
	 * @param borderLayout the borderLayout to set
	 */
	public void setBorderLayout(BorderLayout borderLayout) {
		this.borderLayout = borderLayout;
	}

	/**
	 * Get the {@link BoxLayout} manager to be used to lay out Swing components
	 * on panel.<br>
	 * <p>
	 * @return the boxLayout
	 */
	public BoxLayout getBoxLayout() {
		return boxLayout;
	}

	/**
	 * Set the {@link BoxLayout} manager to be used to lay out Swing components
	 * on panel.<br>
	 * <p>
	 * @param boxLayout the boxLayout to set
	 */
	public void setBoxLayout(BoxLayout boxLayout) {
		this.boxLayout = boxLayout;
	}

	/**
	 * Get the {@link GridLayout} manager to be used to lay out Swing components
	 * on panel.<br>
	 * <p>
	 * @return the gridLayout
	 */
	public GridLayout getGridLayout() {
		return gridLayout;
	}

	/**
	 * Set the {@link GridLayout} manager to be used to lay out Swing components
	 * on panel.<br>
	 * <p>
	 * @param gridLayout the gridLayout to set
	 */
	public void setGridLayout(GridLayout gridLayout) {
		this.gridLayout = gridLayout;
	}

	/**
	 * Get the {@link GridBagLayout} manager to be used to lay out Swing components
	 * on panel.<br>
	 * <p>
	 * @return the gridBagLayout
	 */
	public GridBagLayout getGridBagLayout() {
		return gridBagLayout;
	}

	/**
	 * Set the {@link GridBagLayout} manager to be used to lay out Swing components
	 * on panel.<br>
	 * <p>
	 * @param gridBagLayout the gridBagLayout to set
	 */
	public void setGridBagLayout(GridBagLayout gridBagLayout) {
		this.gridBagLayout = gridBagLayout;
	}

	/**
	 * Get the axis where to lay out components.<br>
	 * <p>
	 * The value is specified by javax.swing.BoxLayout.X_AXIS, .Y_AXIS, .LINE_AXIS, .PAGE_AXIS<br>
	 * <p>
	 * @return the boxLayoutAxis
	 */
	public int getBoxLayoutAxis() {
		return boxLayoutAxis;
	}

	/**
	 * Set the axis where to lay out components for BOX_LAYOUT.<br>
	 * <p>
	 * The value is specified by javax.swing.BoxLayout.X_AXIS, .Y_AXIS, .LINE_AXIS, .PAGE_AXIS<br>
	 * <p>
	 * @param boxLayoutAxis the boxLayoutAxis to set
	 */
	public void setBoxLayoutAxis(int boxLayoutAxis) {
		this.boxLayoutAxis = boxLayoutAxis;
	}

	/**
	 * Get the alignment for FLOW_LAYOUT.<br>
	 * <p>
	 * The value is specified by javax.swing.FlowLayout.LEFT, CENTER, RIGHT, LEADING, TRAILING<br>
	 * <p>
	 * @return the flowLayoutAlign
	 */
	public int getFlowLayoutAlign() {
		return flowLayoutAlign;
	}

	/**
	 * Set the alignment for FLOW_LAYOUT.<br>
	 * <p>
	 * The value is specified by javax.swing.FlowLayout.LEFT, CENTER, RIGHT, LEADING, TRAILING<br>
	 * <p>
	 * @param flowLayoutAlign the flowLayoutAlign to set
	 */
	public void setFlowLayoutAlign(int flowLayoutAlign) {
		this.flowLayoutAlign = flowLayoutAlign;
	}

	
	
	/**
	 * Get the horizontal gap for components laid out by FLOW_LAYOUT or GRID_LAYOUT manager.<br>
	 * <p>
	 * @return the hgap
	 */
	public int getHgap() {
		return hgap;
	}

	/**
	 * Set the horizontal gap for components laid out by FLOW_LAYOUT or GRID_LAYOUT manager.<br>
	 * <p>
	 * @param hgap the hgap to set
	 */
	public void setHgap(int hgap) {
		this.hgap = hgap;
	}

	/**
	 * Get the vertical gap for components laid out by FLOW_LAYOUT or GRID_LAYOUT manager.<br>
	 * <p>
	 * @return the vgap
	 */
	public int getVgap() {
		return vgap;
	}

	/**
	 * Set the vertical gap for components laid out by FLOW_LAYOUT or GRID_LAYOUT manager.<br>
	 * <p>
	 * @param vgap the vgap to set
	 */
	public void setVgap(int vgap) {
		this.vgap = vgap;
	}

	/**
	 * Get the number of rows for GRID_LAYOUT or GRID_BAG_LAYOUT.<br>
	 * <p>
	 * @return the gridRows
	 */
	public int getGridRows() {
		return gridRows;
	}

	/**
	 * Set the number of rows for GRID_LAYOUT or GRID_BAG_LAYOUT.<br>
	 * <p>
	 * @param gridRows the gridRows to set
	 */
	public void setGridRows(int gridRows) {
		this.gridRows = gridRows;
	}

	/**
	 * Get the number of columns for GRID_LAYOUT or GRID_BAG_LAYOUT.<br>
	 * <p>
	 * @return the gridColumns
	 */
	public int getGridColumns() {
		return gridColumns;
	}

	/**
	 * Set the number of columns for GRID_LAYOUT or GRID_BAG_LAYOUT.<br>
	 * <p>
	 * @param gridColumns the gridColumns to set
	 */
	public void setGridColumns(int gridColumns) {
		this.gridColumns = gridColumns;
	}

	/**
	 * Get the border type as coded by forward.<br>
	 * <p>
	 * @return the borderType
	 */
	public EnumForwardBorderType getBorderType() {
		return borderType;
	}

	/**
	 * Set the border type as coded by forward.<br>
	 * <p>
	 * @param borderType the borderType to set
	 */
	public void setBorderType(EnumForwardBorderType borderType) {
		this.borderType = borderType;
	}

	/**
	 * Get the border title for titled borders.<br>
	 * <p>
	 * @return the borderTitle
	 */
	public String getBorderTitle() {
		return borderTitle;
	}

	/**
	 * Get the border title for titled borders.<br>
	 * <p>
	 * @param borderTitle the borderTitle to set
	 */
	public void setBorderTitle(String borderTitle) {
		this.borderTitle = borderTitle;
	}


	/**
	 * Restituisce i tabbed form attivabili nel tabbedPanel.<br>
	 * <p>
	 * Viene restituito un ArrayList di oggetti della classe interna <br>
	 * InnerForwardTabbedForm<br>
	 * <p>
	 * @return the al_tabbedForm
	 */
	public ArrayList<InnerForwardTabbedForm> getTabbedForms() {
		return al_tabbedForm;
	}



	/**
	 * Accoda il descrittore di form tabbed e la sua caption.<br>
	 * <p>
	 * @param alTabbedForm the al_tabbedForm to set
	 */
	public void addTabbedForm(String tabCaption, String formName) {
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
	public ArrayList<ForwardPanelComponent> getComponents() {
		return al_panelComponent;
	}

	/**
	 * Get the count of components declared in a specific row<br>
	 * <p>
	 * @param numRow
	 * @return the count of row components
	 */
	public int getComponentsRowCount(int numRow) {
		int componentsCount = 0;
		for (ForwardPanelComponent panelComponent : this.al_panelComponent) {
			if (panelComponent.getRow() == numRow) {componentsCount++;}
		}
		return componentsCount;
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
	public ForwardPanelComponent getComponent(String componentName) {
		
		// Scan oggetti definiti nel pannello
		for (ForwardPanelComponent panelComponentLoop : al_panelComponent) {
			if (panelComponentLoop.getName().equals(componentName)) {
				return panelComponentLoop;
			}
		}
		
		return null;
	}


	/**
	 * Returns all options declared for the panel<br>
	 * Every option causes a different behaviour, type of image, rendering and so on.<br>
	 * <p>
	 * @return the al_panelOption
	 */
	public ArrayList<EnumForwardOption> getOptions() {
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
	public void addPanelField(ForwardPanelComponent panelField) {
		al_panelComponent.add(panelField);
	}



	/**
	 * Returns the table name, name of the object {@link JTable}<br>
	 * implemented by the panel <code>PANEL_GRID</code>.<br>
	 * <p>
	 * @return the gridTableName
	 */
	public String getGridTableName() {
		return gridTableName;
	}

	/**
	 * Set the table name, name of the object {@link JTable}<br>
	 * implemented by the panel <code>PANEL_GRID</code>.<br>
	 * <p>
	 * @param gridTableName the gridTableName to set
	 */
	public void setGridTableName(String gridTableName) {
		this.gridTableName = gridTableName;
	}

	
	/**
	 * Get the {@link JTable} swing graphic object used to implement the GRID panel type.<br>
	 * <p>
	 * @return the gridGraphicObject
	 */
	public JTable getGridGraphicObject() {
		return gridGraphicObject;
	}

	/**
	 * Set the {@link JTable} swing graphic object used to implement the GRID panel type.<br>
	 * <p>
	 * @param gridGraphicObject the gridGraphicObject to set
	 */
	public void setGridGraphicObject(JTable gridGraphicObject) {
		this.gridGraphicObject = gridGraphicObject;
	}



	/**
	 * Get the split panel left or top when the panel type is SPLIT.<br>
	 * <p>
	 * @return the splitPanelLeftOrTop
	 */
	public String getSplitPanelLeftOrTop() {
		return splitPanelLeftOrTop;
	}

	/**
	 * Set the split panel left or top when the panel type is SPLIT.<br>
	 * <p>
	 * @param splitPanelLeftOrTop the splitPanelLeftOrTop to set
	 */
	public void setSplitPanelLeftOrTop(String splitPanelLeftOrTop) {
		this.splitPanelLeftOrTop = splitPanelLeftOrTop;
	}

	/**
	 * Get the split panel right or down when the panel type is SPLIT.<br>
	 * <p>
	 * @return the splitPanelRightOrDown
	 */
	public String getSplitPanelRightOrDown() {
		return splitPanelRightOrDown;
	}

	/**
	 * Set the split panel right or down when the panel type is SPLIT.<br>
	 * <p>
	 * @param splitPanelRightOrDown the splitPanelRightOrDown to set
	 */
	public void setSplitPanelRightOrDown(String splitPanelRightOrDown) {
		this.splitPanelRightOrDown = splitPanelRightOrDown;
	}


	/**
	 * Returns the toolTip text of the object {@link JTable}<br>
	 * implemented by the panel <code>PANEL_GRID</code>.<br>
	 * <p>
	 * @return the gridToolTipText
	 */
	public String getGridToolTipText() {
		return gridToolTipText;
	}

	/**
	 * Set the toolTip text of the object {@link JTable}<br>
	 * implemented by the panel <code>PANEL_GRID</code>.<br>
	 * <p>
	 * @param gridToolTipText the gridToolTipText to set
	 */
	public void setGridToolTipText(String gridToolTipText) {
		this.gridToolTipText = gridToolTipText;
	}

	/**
	 * Set the toolTip text of the object {@link JTable}<br>
	 * implemented by the panel <code>PANEL_GRID</code>.<br>
	 * <p>
	 * @param gridToolTipText the gridToolTipText to set
	 */
	public void addGridColumn(String columnName, String columnHeader, String columnNameLdv, String columnToolTipText, Class<?> columnRenderType) {
		
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

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
			return "panel:"+panelName+",type:"+panelType.toString()+","+layoutManager.toString();
	}
	
}
