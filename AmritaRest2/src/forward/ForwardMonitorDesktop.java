package forward;
import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.sql.Connection;
import java.sql.SQLException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Scanner;
import java.util.Set;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JApplet;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JPopupMenu;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JSpinner;
import javax.swing.JSpinner.DateEditor;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.ListSelectionModel;
import javax.swing.SpinnerDateModel;
import javax.swing.SpinnerListModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.text.BadLocationException;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;


import utilities.DateTimeService;
import utilities.NumericService;
import utilities.ReflectionManager;
import utilities.StringService;
import analyzer.AmritaConstants;
import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import analyzer.ExecutionDirectives;
import analyzer.LoggerFacade;
import analyzer.MessagesManager;
import analyzer.UserConfiguration;
import entities.EntityCustomization;
import entities.EntityTableData;
import enums.EnumAmritaExceptionError;
import enums.EnumForwardAction;
import enums.EnumForwardCaptionCustomization;
import enums.EnumForwardEvent;
import enums.EnumForwardComponent;
import enums.EnumForwardLayout; 
import enums.EnumForwardLogicalDataViewControl;
import enums.EnumForwardOption;
import enums.EnumForwardReturnCode;
import enums.EnumForwardScope;
import enums.EnumLanguage;
import enums.EnumMessageType;
import enums.EnumTable;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;
import forward.ForwardForm.InnerForwardPanel;
import forward.ForwardDoParms;
import forward.ForwardFunction.InnerActionsGroup;
import forward.ForwardFunction.InnerComponent;
import forward.ForwardFunction.InnerComponentProperty;
import forward.ForwardFunction.InnerOnEvent;


/**
 * copyright (c) 2009-2012 e-Amrita - Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardMonitor
 * </h1>
 * This class implements shared capabilities for desktop monitor, using GUI Swing and web monitor, using a specialized Servelet.<br>
 * <p>
 * The execution of the application is driven by application model declared by a {@link ForwardFunction} object and<br>
 * personalized by user, function, panel and specific fields.<br>
 * Any text field and caption is managed in a multilanguage fashion to fit runtime localization needs.<br>
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardFunction
 *
*/ 

public class ForwardMonitorDesktop  implements  Runnable,
									     	    ActionListener,    		// User action like click on JButton
												AdjustmentListener,		// JScrollBar
												FocusListener,			// Focus gained/lost
												ItemListener,			// Stato JCheckBox ...
												ChangeListener,			// JSlider
												PropertyChangeListener, // Al variare del valore di proprietà e in chiusura dialogo 
												ListSelectionListener,	// Selezione JList e JTable
												KeyListener,			// Keys
												DocumentListener,		// JTextField, ...
												MouseListener,			// Mouse entered, exited, pressed, ...
												MouseWheelListener,		// Rotazione rotella del mouse
												MouseMotionListener,	// Drag & Drop
												WindowListener,			// Open/Close/.. windows
												TreeModelListener,      // Editing nodo, change struttura tree, nodo iserito, deletato
												TreeSelectionListener,  // Selezione nuovo nodo, per individuare il nodo deselezionato
												TreeExpansionListener,  // Espansione/collapse nodo
												AmritaConstants
												{
  
	
	
	// Default di sistema e direttive di esecuzione
	private UserConfiguration sd = null; 
	private MessagesManager mm = null;         							// Gestore messaggi
    private LoggerFacade lf = null;            							// Gestore logging
	private ExecutionDirectives di = null;									// Direttive di esecuzione
 	private ReflectionManager rm = null;								// Reflection manager object
    private DataBaseManager dbm = null;									// Gestore database
	private Connection dbConn = null;									// Connessione con il database	
    private DataBaseEntityInterface dbei = null;						// Interfaccia per operazioni CRUD su entities			     
	private DataBaseStatusDetailed dbs = null;							// Status esito operazione dettagliato
	
  	// Gestione funzioni, attive e chiamate
  	private ArrayList<InnerMonitorControBlock> al_monitorControlBlock = null;
  	private int mcbIndex = 0;											// Numero blocco di controllo corrente, corrisponde allo stack attivo
  	public InnerMonitorControBlock mcb = null;							// Blocco di controllo corrente, pubblico per essere gestito come property
  	private InnerMonitorControBlock mcbCalled = null;					// Blocco di controllo della funzione chiamata
	private ForwardFunction functionStart = null;						// Funzione applicativa di partenza
	private ForwardFunction functionLoaded = null;						// Funzione applicativa in fase di start
	private Map<String, ForwardFunction> hm_functionCalled = null;		// Key=nome funzione, Data=oggetto ForwardFunction
	
	// Variabili comuni a tutte le funzioni chiamate.
	// Il reference di queste map viene memorizzato nel blocco di controllo mcb della funzione chiamata
	// La struttura per le variabili con scope di funzione viene allocata solo all'inizializzazione della funzione
	Map<String, InnerVar> hm_varScopeSession = null;					// Variabili applicative con scope a livello session
	Map<String, InnerVar> hm_varScopeServer = null;						// Variabili applicative con scope a livello server per tutte le funzioni in tutte le sessioni

	
	
	/** Constructor */
	public ForwardMonitorDesktop(UserConfiguration sd, ExecutionDirectives di, ForwardFunction function) {
		
		this.sd = sd;
		mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        this.rm = new ReflectionManager();
		this.di = di;
		this.functionStart = function;
		this.functionStart.setMonitor(this);
		this.mcb = new InnerMonitorControBlock();
		this.mcb.functionStart = this.functionStart;
		this.al_monitorControlBlock = new ArrayList<InnerMonitorControBlock> ();
		this.hm_functionCalled = new  HashMap<String, ForwardFunction> ();
		this.hm_varScopeSession = new HashMap<String, InnerVar> ();				 
		this.hm_varScopeServer = new HashMap<String, InnerVar> ();			
	}


	/**
	 * The GUI will be created and the application function will be started.<br>
	 * <p>
	 * This method is intended for use with native java GUI not applet for a correct exception management.
	 * <p>
	 * <p>
	 */
	public  void run() { 
		
		try {
			
			createGUI(this.mcb);
			this.mcb.frameFunction.setVisible(true);				// Show
			
		} catch (Exception e) {
			this.di.excpOccurred = e;
			return;
		}
	}
	
 
	/*
	 * The GUI will be created and the application function will be started.<br>
	 * 
	 * La funzione parte sempre con un form principale che alla fine si identifica in un primo JPanel main container
	 * Il JPanel main container potrà essere disposto in un JFrame, JDialog o JDesktopPanel
	 * Nel caso della funzione iniziale può essere disposto in un JFrame, in un JDesktopPanel 
	 * Nel caso di funzioni richiamate sempre in un JDialog
	 * <p>
	 * This constructor is intended for use with native java GUI not applet.
	 * <p>
	 */
	private void createGUI(InnerMonitorControBlock mcb) {
		
	  	// Frame principale della funzione.
		mcb.frameFunction = new JFrame();										// 
 		
		// Creazione concreta GUI.
		// Verranno creati i pannelli e i controlli per tutti i form definiti.
		// I pannelli predefiniti come i menu sono creati istanziandoli.
		// I pannelli di dettaglio sono creati disponendo i controlli su essi.
		// Il listener per i controlli applicativi è questa classe.

		functionInitial(mcb);													// Inizializzazione funzione, internazionalizzazione, defaults, lay out componenti, lay out pannelli, creazione main panel
        
		// Preparazione all'esecuzione della funzione.
		// Impostazione main panel in frame funzione (già presente)
	    // Impostazione Posizione
	    // Impostazione menubar
	    // Pack frame
	    // Completamento frame con titolo etc
	    // Esecuzione logica iniziale applicativa della funzione

		functionPreparingForExecutionOnJFrame(mcb);				// Prepare & Show
										

	}


    /* ----------------------------------------------------------
     * Preparazione della funzione per l'esecuzione in un JFrame
     * ----------------------------------------------------------
     * 
     * - Impostazione main panel in frame funzione (già presente)
     * - Impostazione Posizione
     * - Impostazione menubar
     * - pack frame
     * - completamento frame con titolo etc
     * - esecuzione logica iniziale applicativa della funzione
     * 
     */
    private void functionPreparingForExecutionOnJFrame(InnerMonitorControBlock mcb) {
    	
        // Dimensioni in base al contenuto e alle dimensioni preferred o minime dei pannelli
    	// Il listener lo inserisco qua, inquanto il JFrame NON viene registrato come componente
    	// Il nome del JFrame, utilizzabile negli eventi, è quello del form di partenza
		mcb.frameFunction.setContentPane(mcb.panelFunctionForm);
		mcb.frameFunction.addWindowListener(this);
		mcb.frameFunction.setName(mcb.function.getFormStartName());
	  	mcb.isStartedOnJFrame = true;														    // Utilizzato alla return
		
		// Decorazione e posizione 
		mcb.panelFunctionForm.setOpaque(true);
		mcb.frameFunction.setVisible(false);
		mcb.frameFunction.setEnabled(false);
		mcb.frameFunction.setUndecorated(false);
		if (mcb.function.getOptions().contains(EnumForwardOption.FUNCTION_UNDECORATED)) {
			mcb.frameFunction.setUndecorated(true);
		}
		mcb.frameFunction.pack();
		mcb.frameFunction.setEnabled(true);
 		
	
		// Posizione nello schermo
		if (mcb.function.getOptions().contains(EnumForwardOption.FUNCTION_CENTER_SCREEN)) {
			mcb.frameFunction.setLocationRelativeTo(null);
		} else if (mcb.function.getOptions().contains(EnumForwardOption.FUNCTION_CENTER_FRAME_OWNER)) {
			mcb.frameFunction.setLocationRelativeTo(mcb.frameFunctionCaller);
		} else {
			mcb.frameFunction.setLocation(0, 0);
		}
		
 		// Completamento Frame per la funzione e condizioni di uscita
		mcb.frameFunction.setTitle(mcb.function.getTitle());					 	// Main window function title		

		// Impostazione eventuale menuBar direttamente sul frame principale
		if (mcb.formStart.isMenubar()) {
			mcb.forwardMenuBar = mcb.functionStart.getMenu(mcb.formStart.getMenuBarName());
			if (mcb.forwardMenuBar != null) {
				mcb.forwardMenuBar.setForwardMonitor(this);
				mcb.jmenuBar = mcb.forwardMenuBar.getJMenuBar();
				mcb.frameFunction.setJMenuBar(mcb.jmenuBar);
			}
		}
		
 		// Si esegue l'eventuale logica iniziale
		manageLogicDeclaredActivation(mcb, mcb.functionStart.getFunctionName(), EnumForwardEvent.ON_FUNCTION_INITIAL);

		// Per visualizzare la funzione si deve rendere visibile il JFrame principale
		if (this.al_monitorControlBlock.size() > 1) {
			this.mcb.frameFunction.setAlwaysOnTop(true);		// Garantisce il primo piano
		}
		this.mcb.frameFunction.setVisible(true);				// Show
	}

    /* ----------------------------------------------------------
     * Preparazione della funzione per l'esecuzione in un JDialog
     * ----------------------------------------------------------
     * 
     * - Impostazione main panel in frame funzione (già presente)
     * - Impostazione Posizione
     * - Impostazione menubar
     * - pack frame
     * - completamento frame con titolo etc
     * - esecuzione logica iniziale applicativa della funzione
     * 
     */
    private void functionPreparingForExecutionOnJDialog(InnerMonitorControBlock mcb) {
    	
	  	ForwardDialog dialog = null;
	  	
	    // Dimensioni in base al contenuto e alle dimensioni preferred o minime dei pannelli
	    // Il listener lo inserisco qua, inquanto il JFrame NON viene registrato come componente
	    // Il nome del JFrame, utilizzabile negli eventi, è quello del form di partenza
		dialog = new ForwardDialog(mcb.system.getActiveFrame(), mcb.function.getFormStartName(), true, mcb.function.getTitle());
	  	mcb.dialogFunction = dialog;
	  	mcb.dialogFunction.setContentPane(mcb.panelFunctionForm);								// Form, ovvero JPanel applicativo da visualizzare
	  	mcb.dialogFunction.addWindowListener(this);												// Su windowClosing().  Attivazione logiche se chiusura forzata con X
	  	mcb.isStartedOnJFrame = false;														    // Utilizzato alla return
        
		// Decorazione e posizione 
	  	mcb.dialogFunction.setEnabled(false);
	  	mcb.dialogFunction.setUndecorated(false);
		if (mcb.function.getOptions().contains(EnumForwardOption.FUNCTION_UNDECORATED)) {
			mcb.dialogFunction.setUndecorated(true);
		}
		mcb.dialogFunction.pack();
		mcb.dialogFunction.setEnabled(true);
	    
		// Posizione nello schermo
		if (mcb.function.getOptions().contains(EnumForwardOption.FUNCTION_CENTER_SCREEN)) {
			mcb.dialogFunction.setLocationRelativeTo(null);
		} else if (mcb.function.getOptions().contains(EnumForwardOption.FUNCTION_CENTER_FRAME_OWNER)) {
			mcb.dialogFunction.setLocationRelativeTo(mcb.frameFunctionCaller);
		} else {
			mcb.dialogFunction.setLocation(0, 0);
		}

		// Impostazione eventuale menuBar direttamente sul JDialog principale
		if (mcb.formStart.isMenubar()) {
			mcb.forwardMenuBar = mcb.functionStart.getMenu(mcb.formStart.getMenuBarName());
			if (mcb.forwardMenuBar != null) {
				mcb.forwardMenuBar.setForwardMonitor(this);
				mcb.jmenuBar = mcb.forwardMenuBar.getJMenuBar();
				mcb.dialogFunction.setJMenuBar(mcb.jmenuBar);
			}
		}
		
 		// Si esegue l'eventuale logica iniziale
		manageLogicDeclaredActivation(mcb, mcb.functionStart.getFunctionName(), EnumForwardEvent.ON_FUNCTION_INITIAL);

        // Visualizzazione dialogo
		mcb.dialogFunction.setVisible(true);
		mcb.isStartedOnJFrame = false;	
	}


	/* --------------------------------------
     * Inizializzazione funzione applicativa.
     * --------------------------------------
     * 
     * Il main panel sarà pronto per la disposizione nel frame  di visualizzazione
     * 
     */
	private void functionInitial(InnerMonitorControBlock mcb) {
		
		functionInitialize(mcb);												// Monitor structure allocation and initialization, symbol table, frame iniziale ...
		functionSetTextsByLocale(mcb);											// Impostazione testi in lingua se necessario
		functionCompleteComponentsDeclared(mcb);								// For each component declared set default properties and listeners
		functionCustomizationByUser(mcb);										// Applicazione personalizzazioni utente se necessario, incluse eventuali caption
		functionDisposeControlsInDetailPanels(mcb);								// Lay out controls in detail panels according to layout managers
		functionCreateMainFormPanel(mcb, mcb.formStart);						// Lay out panels structure recursively according to layout managers

		// Function e form di partenza
		mcb.formFunctionStartName = mcb.function.getFormStartName();
		mcb.formFunctionStart = mcb.function.getForm(mcb.formFunctionStartName);
		mcb.system.setActiveForm(mcb.formFunctionStart);						// Form attivo nel blocco di controllo
		mcb.system.setActiveFrame(mcb.frameFunction);							// Frame attivo nel blocco di controllo
		mcb.panelFunctionForm = mcb.formFunctionStart.getJrootPanel();
		mcb.panelFunctionForm.setOpaque(true); 									// content panel must be opaque

		// Oggetti per accesso a database e defaults di sistema
		mcb.system.setDbei(dbei);
		mcb.system.setDbm(dbm);
		mcb.system.setSystemDefaults(sd);
		
		// Update Look & Feel di tutta la gerarchia di panels del form  
 		SwingUtilities.updateComponentTreeUI(mcb.panelFunctionForm);
	 
	}


	/**
	 * Gets the system defaul object.<br>
	 * <p>
	 * @return the sd
	 */
	public UserConfiguration getSd() {
		return sd;
	}


	/**
	 * Sets the system defaul object.<br>
	 * <p>
	 * @param sd the sd to set
	 */
	public void setSd(UserConfiguration sd) {
		this.sd = sd;
	}


	/**
	 * Gets the data base manager, with database connection established<br>
	 * <p>
	 * @return the dbm
	 */
	public DataBaseManager getDbm() {
		return dbm;
	}


	/**
	 * Sets the data base manager, with database connection established<br>
	 * <p>
	 * @param dbm the dbm to set
	 */
	public void setDbm(DataBaseManager dbm) {
		this.dbm = dbm;
	}



	/**
	 * Gets the data base entity interface.<br>
	 * <p>
	 * @return the dbei
	 */
	public DataBaseEntityInterface getDbei() {
		return dbei;
	}


	/**
	 * Sets the data base entity interface.<br>
	 * <p>
	 * @param dbei the dbei to set
	 */
	public void setDbei(DataBaseEntityInterface dbei) {
		this.dbei = dbei;
	}


	/*
     * Inizializzazione funzione applicativa
     */
	private void functionInitialize(InnerMonitorControBlock mcb) {
		InnerVar innerVar = null;
		
		// Rendo attiva la funzione di partenza
		mcb.function = mcb.functionStart;										// Oggetto funzione dichiarata
		mcb.function.mcb = mcb;													// Reference blocco di controllo
		mcb.system = new ForwardSystem();										// Oggetto di controllo sistema per l'applicazione
		mcb.system.setMonitorDesktop(this);										// Reference al monitor nell'oggetto system, passato all'applicazione
		this.al_monitorControlBlock.add(mcb);									// Blocco controllo funzione
		this.mcbIndex = al_monitorControlBlock.size() - 1;						// Indice stack funzione attiva
		
		// Imposto il form di partenza della funzione
		for (ForwardForm form : mcb.function.getForms()) {
			if (form.getFormType() == EnumForwardOption.FORM_TYPE_START) {
				mcb.formStart = form;
				break;
			}
		}

		// Strutture per variabili scope server/session che NON cambiano 
	  	mcb.hm_varScopeServer = this.hm_varScopeServer;				 
	  	mcb.hm_varScopeSession = this.hm_varScopeSession;
	  	
	  	// Struttura per variabili di scope function specifiche per la funzione corrente
	  	mcb.hm_varScopeFunction = new HashMap<String, InnerVar> ();			 

		// Update livelli menu in dichiarazione funzione e altro a livello funzione
		mcb.functionStart.setSystem(mcb.system);
		mcb.functionStart.menuLevelUpdate();
		
		// Linguaggio corrente è quello dichiarato
		mcb.functionStart.setCurLanguage(mcb.functionStart.getLanguage());
		
		// Creazione oggetti per valore variabili, dichiarate per la funzione applicativa con VAR()
		for (Entry<String, InnerComponent> entryComponent : mcb.function.getVarsMap().entrySet()) {
			
			if (entryComponent.getValue().componentType != EnumForwardComponent.DeclaredVariable) {continue;}
			
			innerVar = new InnerVar();
			innerVar.varName = entryComponent.getKey();
			innerVar.varType= entryComponent.getValue().varType;
			innerVar.varScope = entryComponent.getValue().varScope;
			innerVar.varObject = entryComponent.getValue().varInitial;
			innerVar.varInitial = entryComponent.getValue().varInitial;
			
			switch (innerVar.varScope) {
				case SCOPE_FUNCTION:
					mcb.hm_varScopeFunction.put(innerVar.varName, innerVar);
					break;
				case SCOPE_SESSION:
					mcb.hm_varScopeSession.put(innerVar.varName, innerVar);
					break;
				case SCOPE_SERVER:
					mcb.hm_varScopeServer.put(innerVar.varName, innerVar);
					break;
				}
		}
		
		// Impostazione Look & feel
        try {
    		
    		//UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel");
            UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
            //UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel");
        } catch (UnsupportedLookAndFeelException ex) {
            ex.printStackTrace();
        } catch (IllegalAccessException ex) {
            ex.printStackTrace();
        } catch (InstantiationException ex) {
            ex.printStackTrace();
        } catch (ClassNotFoundException ex) {
            ex.printStackTrace();
        }
        
        //   Turn off metal's use of bold fonts  
        UIManager.put("swing.boldMetal", Boolean.FALSE);

 	}

	/* -------------------------------------------------------------------------------
	 * Applicazione personalizzazioni funzione per utente
	 * -------------------------------------------------------------------------------
	 * 
	 * Si utilizza EntityCustomization (CUST) 	
	 * 
	 * - Si legge l'entity CUST con funzione+user
	 * - Se si tratta di personalizzazione di caption si effettua con functionSetTextsByLocale()
	 * - se si tratta di una personalizzazione di una property si esegue via reflection
	 * 
	 */
	private void functionCustomizationByUser(InnerMonitorControBlock mcb) {
		
		EntityCustomization entityCustomization = null;
		Object ar_objEntityCustomization[] = null;
		InnerComponent innerComponent = null;
		ArrayList<ForwardDoParms> al_actionMethodName = null;
		ArrayList<ForwardDoParms> al_actionFunctionName = null;
		ArrayList<ForwardDoParms> al_actionLdvName = null;
		String ar_token[] = null;
		String componentName = "";
		String subComponentName = "";
		String propertyName = "";
		String textValue = "";
		String fontName = "";
		int fontType = 0;				// Font.PLAIN, Font.BOLD, Font.ITALIC
		int fontDim = 0;				// Dimensioni font
		int colorRgb1 = 0;				//  
		int colorRgb2 = 0;				//  
		int colorRgb3 = 0;				//  
		int typeCaptionCustomizationNum = 0;
		
		// Recupero personalizzazioni per funzione/user
		ar_objEntityCustomization = functionGetCustomizations(mcb);
		if (ar_objEntityCustomization.length == 0) {return;}		
		
		// Recupero actions potenzialmente da customizzare
		al_actionMethodName = getActionsStartingBy(mcb, "EXEC_METHOD");
		al_actionFunctionName = getActionsStartingBy(mcb, "FUNCTION");
		al_actionLdvName = getActionsStartingBy(mcb, "LDV");
		
		// Scan personalizzazioni
		for (Object objCustomization : ar_objEntityCustomization) {
			entityCustomization = (EntityCustomization) objCustomization;
			
			// Personalizzazione specifica di caption per l'utente
			if (entityCustomization.getTypeCustomization().toString().startsWith("CAPTION")) {
				componentName = entityCustomization.getComponentName();
				subComponentName = entityCustomization.getSubComponentName();
				typeCaptionCustomizationNum = entityCustomization.getTypeCustomization().ordinal();
				propertyName = entityCustomization.getPropertyName();		// E' anche il nome del metodo/funzione/ldv/.. da personalizzare
				textValue = entityCustomization.getValue();
				// Update caption in definizioni e action con stesso metodo per internazionalizzazione captions
	     		functionUpdateCaptions(mcb, componentName, subComponentName, typeCaptionCustomizationNum, textValue);
	     		continue;
			}
	        
			// Personalizzazione specifica di proprietà/nome metodo/funzione/logical data view/ ... per lo user
			switch (entityCustomization.getTypeCustomization()) {
					// Valore iniziale controllo
					case CUSTOMIZED_INITIAL_VALUE:
						innerComponent = mcb.function.getComponentDeclared(entityCustomization.getComponentName());
						// Componente non definito nellla funzione
						if (innerComponent == null) {
						    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0202", new String[]{mcb.function.getClass().getSimpleName(),entityCustomization.getComponentName()}, null);
							break;
						}
						functionCustomizationInitialValue(mcb, innerComponent, textValue);
						break;
						
					// Nome metodo da richiamare	
					case CUSTOMIZED_METHOD_NAME:
						for (ForwardDoParms actionMethodName : al_actionMethodName) {
							if (!actionMethodName.methodName.equals(propertyName)) {continue;}
							actionMethodName.methodName = textValue;
						}
						break;
						
					// Nome funzione da richiamare
					case CUSTOMIZED_FUNCTION_NAME:
						for (ForwardDoParms actionFunctionName : al_actionFunctionName) {
							if (actionFunctionName.functionName.equals(propertyName)) {
								actionFunctionName.functionName = textValue;
								continue;
							}
							if (actionFunctionName.functionNameToLoad.equals(propertyName)) {
								actionFunctionName.functionNameToLoad = textValue;
								continue;
							}
							if (actionFunctionName.functionNameToStart.equals(propertyName)) {
								actionFunctionName.functionNameToStart = textValue;
								continue;
							}
						}
						break;
						
					// Nome logical data view 
					case CUSTOMIZED_LDV_NAME:
						for (ForwardDoParms actionLdvName : al_actionLdvName) {
							if (!actionLdvName.ldvClassName.equals(propertyName)) {continue;}
							actionLdvName.ldvClassName = textValue;
						}
						break;
					
					// CUSTOMIZED_PROPERTY_NUM
					case CUSTOMIZED_PROPERTY_NUM:
						innerComponent = mcb.function.getComponentDeclared(entityCustomization.getComponentName());
						// Componente non definito nellla funzione
						if (innerComponent == null) {
						    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0202", new String[]{mcb.function.getClass().getSimpleName(),entityCustomization.getComponentName()}, null);
							break;
						}
						// Valore di personalizzazione non numerico
						if (!StringService._isNumericInt(textValue)) {
						    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0201", new String[]{mcb.function.getClass().getSimpleName(),innerComponent.componentName,innerComponent.component.getClass().getSimpleName(),propertyName,textValue}, null);
							break;
						}
						// Esecuzione via reflection impostazione proprietà, in caso di errore viene segnalato in log
						setComponentProperty(mcb, innerComponent.component.getClass(), innerComponent.componentName, innerComponent.component, propertyName, StringService._getNumericInt(textValue), mcb.system.getUserLogin());
						break;
					
					// Nome property text da valorizzare
					case CUSTOMIZED_PROPERTY_TEXT:
						innerComponent = mcb.function.getComponentDeclared(entityCustomization.getComponentName());
						// Componente non definito nellla funzione
						if (innerComponent == null) {
						    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0202", new String[]{mcb.function.getClass().getSimpleName(),entityCustomization.getComponentName()}, null);
							break;
						}
						// Esecuzione via reflection impostazione proprietà, in caso di errore viene segnalato in log
						setComponentProperty(mcb, innerComponent.component.getClass(), innerComponent.componentName, innerComponent.component, propertyName, StringService._getNumericInt(textValue), mcb.system.getUserLogin());
						break;
					
					// Nome property booleana da valorizzare
					case CUSTOMIZED_PROPERTY_BOOLEAN:
						innerComponent = mcb.function.getComponentDeclared(entityCustomization.getComponentName());
						// Componente non definito nellla funzione
						if (innerComponent == null) {
						    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0202", new String[]{mcb.function.getClass().getSimpleName(),entityCustomization.getComponentName()}, null);
							break;
						}
						// Valore di personalizzazione non booleano
						if (!textValue.toUpperCase().equals("TRUE") && !textValue.toUpperCase().equals("FALSE")) {
						    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0203", new String[]{mcb.function.getClass().getSimpleName(),innerComponent.componentName,innerComponent.component.getClass().getSimpleName(),propertyName,textValue}, null);
							break;
						}
						// Esecuzione via reflection impostazione proprietà, in caso di errore viene segnalato in log
						setComponentProperty(mcb, innerComponent.component.getClass(), innerComponent.componentName, innerComponent.component, propertyName, Boolean.parseBoolean(textValue), mcb.system.getUserLogin());
						break;
	
					// Nome property Font da valorizzare.
					// Il valore di personalizzazione contiene tre valori
				    // Il primo è il nome del font
					// IL secondo il modo e vale 0, 1, 2, 3 per FONT.PLAIN, FONT.BOLD, FONT.ITALIC o FONT.BOLD+FONT.ITALIC
					// Il terzo è un numero con le dimensioni del carattere
					case CUSTOMIZED_PROPERTY_FONT:
						ar_token = textValue.split(" ");
						// Parametri di personalizzazione insufficienti per font
						if (ar_token.length < 3) {
						    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0205", new String[]{mcb.function.getClass().getSimpleName(),innerComponent.componentName,innerComponent.component.getClass().getSimpleName(),propertyName,textValue}, null);
							break;
						}
						if (!StringService._isNumericInt(ar_token[1]) || !StringService._isNumericInt(ar_token[2])) {
						    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0205", new String[]{mcb.function.getClass().getSimpleName(),innerComponent.componentName,innerComponent.component.getClass().getSimpleName(),propertyName,textValue}, null);
							break;
						}
						// Creazione font
						fontName = ar_token[0];
						fontType = StringService._getNumericInt(ar_token[1]);				// Font.PLAIN, Font.BOLD, Font.ITALIC
						fontDim = StringService._getNumericInt(ar_token[2]);				// Dimensioni font
						Font font = new Font(fontName, fontType, fontDim);
						// Esecuzione via reflection impostazione proprietà, in caso di errore viene segnalato in log
						setComponentProperty(mcb, innerComponent.component.getClass(), innerComponent.componentName, innerComponent.component, propertyName, font, mcb.system.getUserLogin());
						break;
						
					// Nome property Color da valorizzare.
					// Il valore di personalizzazione contiene i tre interi rgb
					case CUSTOMIZED_PROPERTY_COLOR:
						ar_token = textValue.split(" ");
						// Parametri di personalizzazione insufficienti per Color
						if (ar_token.length < 3) {
						    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0205", new String[]{mcb.function.getClass().getSimpleName(),innerComponent.componentName,innerComponent.component.getClass().getSimpleName(),propertyName,textValue}, null);
							break;
						}
						if (!StringService._isNumericInt(ar_token[1]) || !StringService._isNumericInt(ar_token[2]) || !StringService._isNumericInt(ar_token[23])) {
						    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0206", new String[]{mcb.function.getClass().getSimpleName(),innerComponent.componentName,innerComponent.component.getClass().getSimpleName(),propertyName,textValue}, null);
							break;
						}
						// Creazione color
						colorRgb1 = StringService._getNumericInt(ar_token[0]);				 
						colorRgb2 = StringService._getNumericInt(ar_token[1]);				 
						colorRgb3 = StringService._getNumericInt(ar_token[2]);				 
						Color color = new Color(colorRgb1, colorRgb2, colorRgb3);
						// Esecuzione via reflection impostazione proprietà, in caso di errore viene segnalato in log
						setComponentProperty(mcb, innerComponent.component.getClass(), innerComponent.componentName, innerComponent.component, propertyName, color, mcb.system.getUserLogin());
					break;
			
				default:
					break;
				}
		}
	}
	
	/* ------------------------------------------------
	 * Impostazione valore iniziale per il componente
	 * ------------------------------------------------
	 */
	private void functionCustomizationInitialValue(InnerMonitorControBlock mcb, InnerComponent innerComponent, String textValue) {
		
		// Controllo correttezza parametri numerici
		
		// Controllo correttezza parametri date
		
		switch (innerComponent.componentType) {
				case JLabel:
					((JLabel)innerComponent.component).setText(textValue);
					break;
				case JTextField:
					((JTextField)innerComponent.component).setText(textValue);
					break;
				case JFormattedTextField:
					if (innerComponent.component instanceof String ) {
						((JFormattedTextField)innerComponent.component).setValue(textValue);
					}
					if (innerComponent.component instanceof Integer ) {
						((JFormattedTextField)innerComponent.component).setValue(StringService._getNumericInt(textValue));
					}
					if (innerComponent.component instanceof Date ) {
						 Long dateNum = 0L;
						 Date dt = DateTimeService.getDate(textValue, "yyyyMMdd");
						 dateNum = DateTimeService.getDateMillis(dt);
						((JFormattedTextField)innerComponent.component).setValue(dateNum);
					}
					break;
				case JPasswordField:
					((JPasswordField)innerComponent.component).setText(textValue);
					break;
				case JTextArea:
					((JTextArea)innerComponent.component).setText(textValue);
					break;
				case JSlider:
					((JSlider)innerComponent.component).setValue(StringService._getNumericInt(textValue));
					break;
				case JSpinner:
					// Spinner di text
					if (innerComponent.component  instanceof SpinnerListModel) {
						((JSpinner)innerComponent.component).setValue(textValue);
						}
					// Spinner di numeri
					if (innerComponent.component instanceof SpinnerNumberModel) {
						((JSpinner)innerComponent.component).setValue(StringService._getNumericInt(textValue));
					}
					// Spinner di date
					if (innerComponent.component instanceof SpinnerDateModel) {
						 Long dateNum = 0L;
						 Date dt = DateTimeService.getDate(textValue, "yyyyMMdd");
						 dateNum = DateTimeService.getDateMillis(dt);
						((JSpinner)innerComponent.component).setValue(dateNum);
					}
					break;
				case JComboBox:
					((JComboBox)innerComponent.component).setSelectedItem(textValue);
					break;
				case JCheckBox:
					((JCheckBox)innerComponent.component).setSelected(Boolean.parseBoolean(textValue));
					break;
				case JRadioButton:
					((JRadioButton)innerComponent.component).setSelected(Boolean.parseBoolean(textValue));
					break;
				case JToggleButton:
					((JToggleButton)innerComponent.component).setSelected(Boolean.parseBoolean(textValue));
					break;
				case JProgressBar:
					((JProgressBar)innerComponent.component).setValue(StringService._getNumericInt(textValue));
					break;
		
				default:
					break;
		}
	}


	/* --------------------------------------------------------------
	 * Impostazione proprietà specifica 
	 * --------------------------------------------------------------
	 * 
	 * Nel caso di proprietà non definita o errore di esecuzione non viene intrapresa alcuna azione
	 * 
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private void setComponentProperty(InnerMonitorControBlock mcb, Class componentClass, String componentName, Object component, String propertyName, Object propertyValue, String user) {
		
		ExceptionAmrita excp = null;
		Class<? extends JComponent> ar_class[] = null;
		Object ar_object[] = null;
			
		// Impostazione via reflection della proprietà
		ar_class = new Class[1];
		ar_class[0] = (Class<? extends JComponent>) propertyValue.getClass();
		ar_object = new Object[1];
		ar_object[0] = propertyValue;
		Object[] ar_return = this.rm.invokeMethodWithStatus(component, "set"+propertyName, ar_class, ar_object);
		if (ar_return[0] == null) {
		    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0204", new String[]{mcb.function.getClass().getSimpleName()
                    ,componentName
                    ,component.getClass().getSimpleName()
                    ,propertyName
                    ,propertyValue.toString()
					,user  }, null);
			return;
		}
		
		// Logging con exception completa
       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, (Exception) ar_return[0]);
	    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0200", new String[]{mcb.function.getClass().getSimpleName()
	                                                                   ,componentName
	                                                                   ,component.getClass().getSimpleName()
	                                                                   ,propertyName
	                                                                   ,propertyValue.toString()
	    															   }, excp);
	}


	/* -----------------------------------------------------------
	 * Restituisce le action che iniziano con la stringa fornita
	 * -----------------------------------------------------------
	 * - Scan eventi presenti
	 * - Per ogni evento scan actions e accodamento eventuale
	 */
	private ArrayList<ForwardDoParms> getActionsStartingBy(InnerMonitorControBlock mcb, String start) {
		ArrayList<ForwardDoParms> al_action = null;
		
		al_action = new ArrayList<ForwardDoParms> ();
		
		// Scan eventi
		for (InnerOnEvent event : mcb.function.getOnEventList()) {
			// Scan actions
			for (ForwardDoParms action : event.al_actionCoded) {
				if (!action.action.toString().startsWith(start)) {continue;}
				al_action.add(action);
			}
		}
		return al_action;
	}


	/* -------------------------------------------------------------------------------
	 * Lettura personalizzazioni per funzione/user
	 * -------------------------------------------------------------------------------
	 * 
	 * Restituisce un array di oggetti EntityCustomization
	 */
	private Object[] functionGetCustomizations(InnerMonitorControBlock mcb) {

		Object ar_objEntityCustomization[] = null;
		EntityCustomization entityCustomization = null;
	    String whereCondition = "";
	    
    	// Composizione Where di lettura EntityCustomization
     	whereCondition = whereCondition +   "     CUSTUSER = '*'";
     	whereCondition = whereCondition +   " AND CUSTFUNC = '" + mcb.function.getClass().getSimpleName() + "'";
      	entityCustomization = new EntityCustomization();


        // Operazioni per accesso al databsae
     	try {
			dbs = new DataBaseStatusDetailed();
			dbConn = dbm.getConnection(dbs);
			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
			ar_objEntityCustomization = dbei.readSetEntity(entityCustomization, whereCondition, "");
			dbei.commit();
			dbm.releaseConnection(dbConn, dbs);
			return ar_objEntityCustomization;
		} catch (ExceptionAmritaSqlError e) {
			// Logging già effettuato da DataBaseManager
		} catch (SQLException e) {
			// Logging già effettuato da DataBaseManager
		} catch (ExceptionAmrita e) {
			// Logging già effettuato da DataBaseManager
		}
     	return new Object[0];
	}

	
	/* -------------------------------------------------------------------------------
	 * Impostazione testi in lingua diversa da quella di dichiarazione della funzione
	 * -------------------------------------------------------------------------------
	 * 
	 * La tabella di caption, indicata nella funzione, ha la seguente chiave;
	 * 
	 * functionName:typeCustomization:componentName:subComponentName:textValue
	 * 
	 * - functionName, se presente deve coincidere con il codice della funzione (della classe)
	 * - typeCustomization è l'ordinal di EnumForwardCaptionCustomization
	 * - componentName è i nome della componente GUI a cui assegnare la caption o il nome identificativo della action
	 * - subComponentName è il nome della colonna della tabella jtable o del tab panel e vale solo se CAPTION_TABLE_COLUMN_HEADER/CAPTION_PANEL_TAB
	 * - textValue è il valore testuale della caption in lingua
	 * 
	 * La lingua viene passata come parametro perchè questo metodo può essere richiamato anche dopo
	 * l'inizializzazione della funzione per una diversa localizzazione o a fronte di personalizzazioni utente su CUST
	 * 
	 */
	private void functionSetTextsByLocale(InnerMonitorControBlock mcb) {
		
		EntityTableData entityTableData = null;
		Object ar_objTableData[] = null;
		String keyVal = "";
		String functionName = "";
		String typeCaptionCustomization = "";
		String componentName = "";
		String subComponentName = "";
		String textValue = "";
		int typeCaptionCustomizationNum = 0;
		int i1 = 0;
		int i2 = 0;
		int i3 = 0;
		int i4 = 0;
		
		// La funzione non dichiara una tabella di caption multilingua
		if (mcb.function.getNumTableCaptions() < 0) {
			return;
		}
		
		// Dichiarata una tabella con le caption/text/tooltip multilingua per la funzione
		
		ar_objTableData = functionGetCaptionsLocalized(mcb);
		
        // Recupero captions definite per la funzione
    	for (Object objTableData : ar_objTableData) {
     		
    		entityTableData = (EntityTableData) objTableData;
     		keyVal = entityTableData.getKeyVal(); 
     		
     		// Delimiters in key
     		i1 = keyVal.indexOf(":");
     		if (i1 < 0) {continue;}
      		i2 = keyVal.indexOf(":", i1);
     		if (i2 < 0) {continue;}
     		i3 = keyVal.indexOf(":", i2);
     		if (i3 < 0) {continue;}
     		i4 = keyVal.indexOf(":", i3);
     		if (i4 < 0) {continue;}
     		
     		// Identificazione caption
    		functionName = keyVal.substring(0, i1);
      		typeCaptionCustomization = keyVal.substring(i1+1, i2);
     		componentName = keyVal.substring(i2+1, i3);
     		subComponentName = keyVal.substring(i3+1, 14);
     		textValue = keyVal.substring(i4+1);
     		
     		// Tipo componente NON numerico
     		if (!StringService._isNumericInt(typeCaptionCustomization)) {
				continue;
			}
       		typeCaptionCustomizationNum = StringService._getNumericInt(typeCaptionCustomization);
   		
    		// Function name NON coincide con quella indicata nella funzione
     		if (!functionName.equals(mcb.function.getFunctionName())) {
				continue;
			}

     		// Update captions nelle dichiarazioni e nelle action
       		functionUpdateCaptions(mcb, componentName, subComponentName, typeCaptionCustomizationNum, textValue);
       	}		
	}

	/* ------------------------------------------------------------------------------------
	 * Aggiornamento delle caption nella definizione dei singoli componenti e nelle action
	 * ------------------------------------------------------------------------------------
	 * 
	 */
	private void functionUpdateCaptions(InnerMonitorControBlock mcb, String componentName, String subComponentName, int typeCaptionCustomizationNum, String textValue) {
		
		Map<String, InnerComponent> hm_component = null;
		InnerComponent innerComponent = null;
		JComponent jcomponent = null;
		ForwardTableModel forwardTableModel = null;
		ForwardTableModelColumn forwardTableModelColumn = null;
		String[] ar_textValue = null;
		
		hm_component = mcb.function.getComponentsMap();
		
		// Deve esistere come componente
		if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_PANEL_TITLE.ordinal()
		||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_PANEL_TOOLTIP.ordinal()		
		||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_PANEL_TAB.ordinal()		
		||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_CONTROL.ordinal()		
		||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_CONTROL_TITLE.ordinal()		
		||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_CONTROL_TOOLTIP.ordinal()		
		||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_TABLE_COLUMN_HEADER.ordinal()
		||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_TABLE_TOOLTIP_CELL.ordinal()
		||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_TABLE_TOOLTIP_HEADER.ordinal()) {
			
			// Componente NON definito nella funzione o nome errato
			innerComponent = hm_component.get(componentName);
			if (innerComponent == null) {return;} 
			
			// Si tratta di un JComponent
			if (innerComponent.component instanceof JComponent) {
				
				jcomponent = (JComponent) innerComponent.component;
				
			    // La caption  tooltip è a livello di JComponent
				if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_PANEL_TOOLTIP.ordinal()
				||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_CONTROL_TOOLTIP.ordinal()) {
					jcomponent.setToolTipText(textValue);
					return;
				}
				
				// La caption è a livello di header di colonna di jtable
				if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_TABLE_COLUMN_HEADER.ordinal()) {
					forwardTableModel = (ForwardTableModel)((JTable)jcomponent).getModel();
					forwardTableModelColumn = forwardTableModel._getColumn(subComponentName);
					forwardTableModelColumn.setHeader(textValue);
					return;
				}

				// La caption  tooltip è a livello di header di colonna di jtable
				if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_TABLE_TOOLTIP_HEADER.ordinal()) {
					forwardTableModel = (ForwardTableModel)((JTable)jcomponent).getModel();
					forwardTableModelColumn = forwardTableModel._getColumn(subComponentName);
					forwardTableModelColumn.setToolTipText(textValue);
					return;
				}
				
				// La caption tooltip è a livello di cella di jtable
				if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_TABLE_TOOLTIP_CELL.ordinal()) {
					// TODO
					return;
				}
				// La caption è di controllo GUI
				if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_CONTROL.ordinal()) {
					switch (innerComponent.componentType) {
						// Label e campi di immissione valori testuali
						case JLabel:
							((JLabel)jcomponent).setText(textValue);
							break;
						case JButton:
							((JButton)jcomponent).setText(textValue);
							break;
						case JToggleButton:
							((JButton)jcomponent).setText(textValue);
							break;
						case JCheckBox:
							((JCheckBox)jcomponent).setText(textValue);
							break;
						case JRadioButton:
							((JRadioButton)jcomponent).setText(textValue);
							break;
						case JMenu:
							((JMenu)jcomponent).setText(textValue);
							break;
						case JMenuItem:
							((JMenuItem)jcomponent).setText(textValue);
							break;
						case JCheckBoxMenuItem:
							((JCheckBoxMenuItem)jcomponent).setText(textValue);
							break;
						case JRadioButtonMenuItem:
							((JRadioButtonMenuItem)jcomponent).setText(textValue);
							break;
					default:
							break;
					}
					return;
				}
				
				// La caption è di un titolo di BORDER
				if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_PANEL_TITLE.ordinal()
				||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_CONTROL_TITLE.ordinal()) {
					if (jcomponent.getBorder() instanceof TitledBorder) {
						((TitledBorder)jcomponent.getBorder()).setTitle(textValue);
						return;
					}
					return;
				}
			}
			return;
		}
		
		// Caption relativa a frame principale della funzione
		if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_FUNCTION_TITLE.ordinal()) {
			mcb.frameFunction.setTitle(textValue);
			return;
		}
		
		// Caption relativa a frame di dialog e e testi applicativi in dialog
		if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_DIALOG_TITLE.ordinal()		
		||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_DIALOG_MESSAGE.ordinal()
		||  typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_DIALOG_SELECTION_VALUES.ordinal()) {
			// Scan eventi funzione
			for (InnerOnEvent onEvent : mcb.function.getOnEventList()) {
				// Scan Actions dichiarate per l'evento
				for (ForwardDoParms doParms : onEvent.al_actionCoded) {
					if (!doParms.action.toString().startsWith("DIALOG_START_")) {continue;}		// Interessano solo le DIALOG_START_xxx
					// Titolo finestra di dialogo
					if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_DIALOG_TITLE.ordinal()
					&& !doParms.fileChooserTitle.equals("")) {
						doParms.fileChooserTitle = textValue;
						continue;
					}
					// Messaggio di dialogo
					if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_DIALOG_MESSAGE.ordinal()
					&& !doParms.dialogMessage.equals("")) {
						doParms.dialogMessage = textValue;
						continue;
					}
					// Valori da selezionare in comboBox di messaggio di dialogo
					// Si considera un elenco di valori, l'ultimo e' il valore iniziale.
					// Se sono presenti meno valori di quelli definiti nella action, si aggiornano solo quelli disponibili.
					if (typeCaptionCustomizationNum == EnumForwardCaptionCustomization.CAPTION_DIALOG_SELECTION_VALUES.ordinal()
					&&  doParms.dialogSelectionValues != null) {
						ar_textValue = textValue.split(" ");
						// Valori da selezionare in comboBox
						for (int i = 0; i < ar_textValue.length; i++) {
							if (i >= doParms.dialogSelectionValues.length) {break;}
							doParms.dialogSelectionValues[i] = ar_textValue[i];
						}
						// Valore iniziale
						if (ar_textValue.length > doParms.dialogSelectionValues.length) {
							doParms.dialogInitialSelectionValue = ar_textValue[doParms.dialogSelectionValues.length];
						}
						
						// Next action
						continue;
					}
				}
			}
		}
	}

 
	/* ---------------------------------
	 * Lettura captions per la funzione
	 * ---------------------------------
	 * 
	 */
	private Object[] functionGetCaptionsLocalized(InnerMonitorControBlock mcb) {

		// Recupero captions funzione 
		Object ar_objTableData[] = null;
	    EntityTableData entityTableData = null;
	    String whereCondition = "";
	    
    	// Composizione Where di lettura EntityTableData
     	whereCondition = whereCondition +   "     TBDTSYST = '*'";
     	whereCondition = whereCondition +   " AND TBDTSUBS = '*'";
     	whereCondition = whereCondition +   " AND TBDTLANG = " + mcb.system.getActiveLanguage().ordinal();
     	whereCondition = whereCondition +   " AND TBDTTTAB = " + mcb.function.getNumTableCaptions();
     	entityTableData = new EntityTableData();


        // Operazioni per accesso al databsae
     	try {
			dbs = new DataBaseStatusDetailed();
			dbConn = dbm.getConnection(dbs);
			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
			ar_objTableData = dbei.readSetEntity(entityTableData, whereCondition, "");
			dbei.commit();
			dbm.releaseConnection(dbConn, dbs);
			return ar_objTableData;
		} catch (ExceptionAmritaSqlError e) {
			// Logging già effettuato da DataBaseManager
		} catch (SQLException e) {
			// Logging già effettuato da DataBaseManager
		} catch (ExceptionAmrita e) {
			// Logging già effettuato da DataBaseManager
		}
		return new Object[0];
	}


	/* ----------------------------------------------------
     * Completamento definizioni singoli componenti java.
     * ----------------------------------------------------
     * 
     * Normalmente si tratta di oggetti grafici componenti la GUI, anche in form diversi.
     * Tuttavia si può trattare di qualsiasi oggetto, come un Timer.
     * 
     * Per ogni componente dichiarato;
     *    - Impostazione proprietà specifiche definite per il controllo 
     *    - Registrazione listener se controllo in ON_CONDITION()
     *    - Impostazione oggetti Icon quando dichiarate icone
     *   I bordi sono già stati valorizzati in fase di dichiarazione della funzione
     */
	private void functionCompleteComponentsDeclared(InnerMonitorControBlock mcb) {
		
		Map<String, ArrayList<JComponent>> hm_group = null;							// Gruppi di JRadioButton/JMenuItemRadioButton
		ButtonGroup buttonGroup = null;												// Singolo gruppo di JRadioButton
		ForwardMenu menu = null;
		ForwardMenuItem menuItem = null;
		ForwardPanelComponent panelComponent = null;
		JComponent jcomponent = null;
		JButton menuItemJButton = null;
		Timer timerSwing = null;
		String menuName = "";
		String controlName = "";
		
		// La funzione era già stata richiamata e quindi i listener sono già assegnati
		if (mcb.function.getCounterStarts() > 1) {
			return;
		}
		
		
		hm_group = new HashMap<String, ArrayList<JComponent>> ();
		
		// Scan componenti dichiarate per la funzione
		for (Entry<String, InnerComponent> entryJavaComponent : mcb.function.getComponentsMap().entrySet()) {
			
			switch (entryJavaComponent.getValue().componentType) {
			
				// Label e campi di immissione valori testuali
				case JLabel:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JLabel.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JLabel.class, jcomponent, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false, false);
					break;
				case JTextField:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JTextField.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JTextField.class, jcomponent, true, true, true, true, true, false, true, false, false, false, true, false, false, false, false, false);
					break;
				case JPasswordField:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JPasswordField.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JPasswordField.class, jcomponent, true, true, true, true, true, false, true, false, false, false, true, false, false, false, false, false);
					break;
				case JFormattedTextField:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JFormattedTextField.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JFormattedTextField.class, jcomponent, true, true, true, true, true, false, true, false, false, false, true, false, false, false, false, false);
					break;
					
				// Aree di testo normali, con stile e rich
				case JTextArea:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JTextArea.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JTextArea.class, jcomponent, true, true, true, true, true, false, false, false, false, false, true, false, false, false, false, false);
					break;
				case JTextPane:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JTextPane.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JTextPane.class, jcomponent, true, true, true, true, true, false, false, false, false, false, true, false, false, false, false, false);
					break;
				case JEditorPane:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JEditorPane.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JEditorPane.class, jcomponent, true, true, true, true, true, false, false, false, false, false, true, false, false, false, false, false);
					break;
					
				// Pulsanti
				case JButton:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JButton.class, jcomponent.getName(), jcomponent);
					panelComponent = mcb.function.getPanelComponent(jcomponent.getName());
					if (!panelComponent.getIconPath().equals("")) {
						JButton jbutton = (JButton) jcomponent;
						jbutton.setIcon(createImageIcon(panelComponent.getIconPath()));
					}
					setListenersOnComponentDeclared(mcb, JButton.class, jcomponent, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false);
					break;
				case JToggleButton:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JToggleButton.class, jcomponent.getName(), jcomponent);
					panelComponent = mcb.function.getPanelComponent(jcomponent.getName());
					if (!panelComponent.getIconPath().equals("")) {
						JButton jbutton = (JButton) jcomponent;
						jbutton.setIcon(createImageIcon(panelComponent.getIconPath()));
					}
					setListenersOnComponentDeclared(mcb, JToggleButton.class, jcomponent, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false);
					break;
					
				// Opzioni
				case JCheckBox:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					addComponentToGroupIfAny(hm_group, entryJavaComponent);
					setComponentPropertiesSpecific(mcb, JCheckBox.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JCheckBox.class, jcomponent, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false);
					break;
				case JRadioButton:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					addComponentToGroupIfAny(hm_group, entryJavaComponent);
					setComponentPropertiesSpecific(mcb, JRadioButton.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JRadioButton.class, jcomponent, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false);
					break;
					 
				// Elenchi alberi e tabelle						
				case JList:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JList.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JList.class, jcomponent, true, true, true, true, true, false, false, true, false, false, false, false, false, false, false, false);
					break;
				case JTree:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JTree.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JTree.class, jcomponent, true, true, true, true, true, false, false, false, false, false, false, false, false, true, true, true);
					JTree jtree = (JTree) jcomponent;
					ForwardTreeModel treeModel = (ForwardTreeModel) jtree.getModel();
				    DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
				    renderer.setLeafIcon(null);
				    renderer.setClosedIcon(null);
 				    renderer.setOpenIcon(null);
					if (!treeModel._getIconPathOpen().equals("")) 	{renderer.setOpenIcon(createImageIcon(treeModel._getIconPathOpen()));}
					if (!treeModel._getIconPathClosed().equals("")) {renderer.setOpenIcon(createImageIcon(treeModel._getIconPathClosed()));}
					if (!treeModel._getIconPathLeaf().equals("")) 	{renderer.setOpenIcon(createImageIcon(treeModel._getIconPathClosed()));}
					renderer.setLeafIcon(createImageIcon("middle.gif"));
				    jtree.setCellRenderer(renderer);
					break;
				case JTable:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JTable.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JTable.class, jcomponent, true, true, true, true, true, false, false, true, false, false, false, false, false, false, false, false);
					// Asssegnazione TableModel
					panelComponent = mcb.function.getPanelComponent(jcomponent.getName());
					JTable jtable = (JTable) jcomponent;
					jtable.setModel(panelComponent.getTableModel());
					panelComponent.getTableModel()._applyDeclaredColumnsCellEditor();
					panelComponent.getTableModel()._applyDeclaredColumnsCellRenderer();
 					panelComponent.getTableModel()._applyDeclaredColumsWidth();
					panelComponent.getTableModel()._applyDeclaredColumsResizable();
					break;
					
				// Selezione valori da insiemi numerici, di numeri/date/stringhe e combobox				
				case JComboBox:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JComboBox.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JComboBox.class, jcomponent, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false);
					break;
				case JSlider:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JSlider.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JSlider.class, jcomponent, true, true, true, true, true, false, false, true, true, false, false, false, false, false, false, false);
					break;
				case JSpinner:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JSpinner.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JSpinner.class, jcomponent, true, true, true, true, true, false, false, false, true, false, false, false, false, false, false, false);
					break;
				
				// Contenitori e barra di scorrimento
				case JScrollBar:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JScrollBar.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JScrollBar.class, jcomponent, true, true, true, true, true, true, true, false, false, true, false, true, false, false, false, false);
					break;
				case JScrollPane:  
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JScrollPane.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JScrollPane.class, jcomponent, true, true, true, true, true, false, false, false, false, true, false, false, false, false, false, false);
					break;
				case JPanel:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JPanel.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JPanel.class, jcomponent, true, true, true, true, true, false, false, false, false, true, false, false, false, false, false, false);
					break;
				case JDialog:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JDialog.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JDialog.class, jcomponent, true, true, true, true, true, false, false, false, false, false, false, false, true, false, false, false);
					break;
				case JTabbedPane:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JTabbedPane.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JTabbedPane.class, jcomponent, true, true, true, true, true, false, false, false, true, true, false, false, false, false, false, false);
					break;
				case JSplitPane:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JSplitPane.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JSplitPane.class, jcomponent, true, true, true, true, true, false, false, false, false, true, false, false, false, false, false, false);
					break;
				case JFrame:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JFrame.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JFrame.class, jcomponent, true, true, true, true, true, false, false, false, false, true, false, false, true, false, false, false);
					break;
					
				// Toolbar/Menu/MenuItem/..
				case JToolBar:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JToolBar.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JToolBar.class, jcomponent, true, true, true, true, true, false, false, false, false, true, false, false, false, false, false, false);
					break;
				case JMenu:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JMenu.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JMenu.class, jcomponent, true, true, true, true, true, false, false, false, false, true, false, false, false, false, false, false);
					break;
				case JMenuItem:
					// Applicazione properties e listener a JMenuItem
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, JMenuItem.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JMenuItem.class, jcomponent, true, true, true, true, true, true, true, false, true, true, false, false, false, false, false, false);
					// Applicazione properties e listener a JButton, se il menu NON è realizzato come menuBar ma come TOOLBAR o PLAIN_BUTTONS
					controlName =  jcomponent.getName(); 
					menu = mcb.function.getMenuOwner(controlName);
					if (menu.getMenuRendering() != EnumForwardOption.MENU_RENDERING_MENUBAR) {
						menuName = menu.getMenuName();
						menuItem = mcb.function.getMenuItem(menuName, controlName);
						if (menuItem.getGraphicObject() != null && menuItem.getGraphicObject() instanceof JButton) {
							menuItemJButton = (JButton) menuItem.getGraphicObject();
							setListenersOnComponentDeclared(mcb, JButton.class, menuItemJButton, true, true, true, true, true, true, true, false, true, true, false, false, false, false, false, false);
						}
					}
					break;
				case JCheckBoxMenuItem:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					addComponentToGroupIfAny(hm_group, entryJavaComponent);
					setComponentPropertiesSpecific(mcb, JCheckBoxMenuItem.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JCheckBoxMenuItem.class, jcomponent, true, true, true, true, true, true, true, false, true, true, false, false, false, false, false, false);
					break;
				case JRadioButtonMenuItem:
					jcomponent = (JComponent) entryJavaComponent.getValue().component;  
					addComponentToGroupIfAny(hm_group, entryJavaComponent);
					setComponentPropertiesSpecific(mcb, JRadioButtonMenuItem.class, jcomponent.getName(), jcomponent);
					setListenersOnComponentDeclared(mcb, JRadioButtonMenuItem.class, jcomponent, true, true, true, true, true, true, true, false, true, true, false, false, false, false, false, false);
					break;
					
				// Componenti non grafici
				case TimerSwing:
					timerSwing = (Timer) entryJavaComponent.getValue().component;  
					setComponentPropertiesSpecific(mcb, Timer.class, entryJavaComponent.getValue().componentName, timerSwing);
					setListenersOnComponentDeclared(mcb, Timer.class, timerSwing, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false);
					break;
					
				default:
					break;
			}

		    // Accodamento componenti al gruppo di appartenenza (JRadioButton, JMenuItemRadioButton, ..)
			for (Entry<String, ArrayList<JComponent>> entryGroup : hm_group.entrySet()) {
				buttonGroup = new ButtonGroup();
				for (JComponent jcomponentGroup : entryGroup.getValue()) {
					buttonGroup.add((AbstractButton) jcomponentGroup);
				}
			} // end-for groups
			
		} // end-for components		
		
	}
	

	/*
     * Se componente appartenente a un gruppo, accodamento in map
     */
    private void addComponentToGroupIfAny(Map<String, ArrayList<JComponent>> hm_group, Entry<String, InnerComponent> entryJavaComponent) {
    	InnerComponent innerComponent = null;
    	ArrayList<JComponent> al_jcomponent = null;
     	String groupName = "";
    	innerComponent = entryJavaComponent.getValue();
    	
     	if (innerComponent.groupName.equals("")) {return;}
     	groupName = innerComponent.groupName;
     	
    	// Gestione creazione gruppo e accodamento componente
    	al_jcomponent = hm_group.get(innerComponent.groupName);
    	if (al_jcomponent == null) {
    		al_jcomponent = new ArrayList<JComponent>();
    		hm_group.put(groupName, al_jcomponent);
		}
    	al_jcomponent.add((JComponent) innerComponent.component);
	}

	/*
     * Impostazione listener in base a tipo componente e dichiarazioni ON_EVENT()
     * per oggetti estensione di JComponent
     */
	@SuppressWarnings("rawtypes")
	private void setListenersOnComponentDeclared(  InnerMonitorControBlock mcb 
												 , Class<?> classComponent
												 , Object component
												 , boolean isFocusListenerToAdd
												 , boolean isKeyListenerToAdd
												 , boolean isMouseListenerToAdd
												 , boolean isMouseMotionListenerToAdd
												 , boolean isMouseWheelListenerToAdd
												 , boolean isItemListenerToAdd
												 , boolean isActionListenerToAdd
												 , boolean isListSelectionListenerToAdd
												 , boolean isChangeListenerToAdd
												 , boolean isPropertyChangeListenerToAdd
												 , boolean isDocumentListenerToAdd
												 , boolean isAdjustmenListenerToAdd
												 , boolean isWindowListenerToAdd
												 , boolean isTreeModelListener       // Editing nodo, change struttura tree, nodo iserito, deletato
												 , boolean isTreeSelectionListener   // Selezione nuovo nodo, per individuare il nodo deselezionato
												 , boolean isTreeExpansionListener   // Espansione/collapse nodo

												  ) {

		JTable jtable = null;
		InnerComponent innerComponent = null;
		JFrame jframe = null;
		JDialog jdialog = null;
		JComponent jcomponent = null;
		String componentName = "";
		  
		// Individuazione nome componente (JDialog e JFrame NON derivano da JComponent)
		if (component instanceof JComponent) {
			jcomponent = (JComponent) component;
			componentName = jcomponent.getName();
		} else if (component instanceof JFrame) {
			jframe = (JFrame) component;  
			componentName = jframe.getName();
		} else if (component instanceof JDialog) {
			jdialog = (JDialog) component;
			componentName = jdialog.getName();
		} else if (component instanceof Timer) {
			componentName = mcb.function.getComponentName(component);			// Individuo il nome dell'oggetto Timer 		}
		}
		
		// Listener applicabili a tutti i componenti
		if (isFocusListenerToAdd(mcb, componentName)) {jcomponent.addFocusListener(this);}
		if (isKeyListenerToAdd(mcb, componentName)) {jcomponent.addKeyListener(this);}
		if (isMouseListenerToAdd(mcb, componentName)) {jcomponent.addMouseListener(this);}
		if (isMouseMotionListenerToAdd(mcb, componentName)) {jcomponent.addMouseMotionListener(this);}
		if (isMouseWheelListenerToAdd(mcb, componentName)) {jcomponent.addMouseWheelListener(this);}
		
		// ItemStateChanged di ItemListener
		if (!componentName.equals("") && isItemListenerToAdd(mcb, componentName)) {
			if (classComponent == JComboBox.class) {
				JComboBox jcomboBox = (JComboBox) component;
				jcomboBox.addItemListener(this);
			}
			if (classComponent == JCheckBox.class) {
				JCheckBox jcheckBox = (JCheckBox) component;
				jcheckBox.addItemListener(this);
			}
			if (classComponent == JRadioButton.class) {
				JRadioButton jradioButton = (JRadioButton) component;
				jradioButton.addItemListener(this);
			}
			if (classComponent == JToggleButton.class) {
				JToggleButton jtoggleButton = (JToggleButton) component;
				jtoggleButton.addItemListener(this);
			}
			if (classComponent == JRadioButtonMenuItem.class) {
				JRadioButtonMenuItem jradioButtonMenuItem = (JRadioButtonMenuItem) component;
				jradioButtonMenuItem.addItemListener(this);
			}
			if (classComponent == JMenuItem.class) {
				JMenuItem jmenuItem = (JMenuItem) component;
				jmenuItem.addItemListener(this);
			}
			if (classComponent == JCheckBoxMenuItem.class) {
				JCheckBoxMenuItem jcheckBoxMenuItem = (JCheckBoxMenuItem) component;
				jcheckBoxMenuItem.addItemListener(this);
			}
		}

		// ActionPerformed di ActionListener
		if (!componentName.equals("") && isActionListenerToAdd(mcb, componentName)) {
			if (classComponent == JCheckBox.class) {
				JCheckBox jcheckBox = (JCheckBox) component;
				jcheckBox.addActionListener(this);
			}
			if (classComponent == JButton.class) {
				JButton jbutton = (JButton) component;
				jbutton.addActionListener(this);
			}
			if (classComponent == JToggleButton.class) {
				JToggleButton jtoggleButton = (JToggleButton) component;
				jtoggleButton.addActionListener(this);
			}
			if (classComponent == JTextField.class) {
				JTextField jtextField = (JTextField) component;
				jtextField.addActionListener(this);
			}
			if (classComponent == JPasswordField.class) {
				JPasswordField jpasswordField = (JPasswordField) component;
				jpasswordField.addActionListener(this);
			}
			if (classComponent == JFormattedTextField.class) {
				JFormattedTextField jformattedTextField = (JFormattedTextField) component;
				jformattedTextField.addActionListener(this);
			}
			if (classComponent == JMenuItem.class) {
				JMenuItem jmenuItem = (JMenuItem) component;
				jmenuItem.addActionListener(this);
			}
			if (classComponent == JRadioButtonMenuItem.class) {
				JRadioButtonMenuItem jradioButtonMenuItem = (JRadioButtonMenuItem) component;
				jradioButtonMenuItem.addActionListener(this);
			}
			if (classComponent == Timer.class) {
				Timer timer = (Timer) component;
				timer.addActionListener(this);
			}
		}
		
		// ValueChanged di ListSelectionListener
		if (!componentName.equals("") && isListSelectionListenerToAdd(mcb, componentName)) {
			if (classComponent == JList.class) {
				JList jlist = (JList) component;
				jlist.addListSelectionListener(this);
			}
			if (classComponent == JTable.class) {
				innerComponent = mcb.function.getComponentDeclared(componentName);
				jtable = (JTable) innerComponent.component;
				jtable.getSelectionModel().addListSelectionListener(this);
			}
		}
		
		// StateChanged di ChangeListener.
		// Per JTabbedPane viene sempre attivato.
		if (!componentName.equals("") && (isChangeListenerToAdd(mcb, componentName) || classComponent == JTabbedPane.class)) {
			if (classComponent == JSlider.class) {
				JSlider jslider = (JSlider) component;
				jslider.addChangeListener(this);
			}
			if (classComponent == JSpinner.class) {
				JSpinner jslider = (JSpinner) component;
				jslider.addChangeListener(this);
			}
			if (classComponent == JTabbedPane.class) {
				JTabbedPane jtabbedPane = (JTabbedPane) component;
				jtabbedPane.addChangeListener(this);
			}
			if (classComponent == JMenuItem.class) {
				JMenuItem jmenuItem = (JMenuItem) component;
				jmenuItem.addChangeListener(this);
			}
			if (classComponent == JCheckBoxMenuItem.class) {
				JCheckBoxMenuItem jcheckBoxMenuItem = (JCheckBoxMenuItem) component;
				jcheckBoxMenuItem.addChangeListener(this);
			}
			if (classComponent == JRadioButtonMenuItem.class) {
				JRadioButtonMenuItem jradioButtonMenuItem = (JRadioButtonMenuItem) component;
				jradioButtonMenuItem.addChangeListener(this);
			}
		}
		
		// PropertyChanged di PropertyChangeListener
		if (!componentName.equals("") && isPropertyChangeListenerToAdd(mcb, componentName)) {
			if (classComponent == JToolBar.class) {
				JToolBar jtoolBar = (JToolBar) component;
				jtoolBar.addPropertyChangeListener(this);
			}
			if (classComponent == JSlider.class) {
				JSlider jslider = (JSlider) component;
				jslider.addPropertyChangeListener(this);
			}
			if (classComponent == JScrollBar.class) {
				JScrollBar jscrollBar = (JScrollBar) component;
				jscrollBar.addPropertyChangeListener(this);
			}
			if (classComponent == JPanel.class) {
				JPanel jpanel = (JPanel) component;
				jpanel.addPropertyChangeListener(this);
			}
			if (classComponent == JTabbedPane.class) {
				JTabbedPane jtabbedPane = (JTabbedPane) component;
				jtabbedPane.addPropertyChangeListener(this);
			}
			if (classComponent == JSplitPane.class) {
				JSplitPane jsplitPane = (JSplitPane) component;
				jsplitPane.addPropertyChangeListener(this);
			}
			if (classComponent == JFrame.class) {
				jframe = (JFrame) component;
				jframe.addPropertyChangeListener(this);
			}
			if (classComponent == JDialog.class) {
 				jdialog = (JDialog) component;
 				jdialog.addPropertyChangeListener(this);
			}
			if (classComponent == JMenuItem.class) {
				JMenuItem jmenuItem = (JMenuItem) component;
				jmenuItem.addPropertyChangeListener(this);
			}
			if (classComponent == JCheckBoxMenuItem.class) {
				JCheckBoxMenuItem jcheckBoxMenuItem = (JCheckBoxMenuItem) component;
				jcheckBoxMenuItem.addPropertyChangeListener(this);
			}
			if (classComponent == JRadioButtonMenuItem.class) {
				JRadioButtonMenuItem jradioButtonMenuItem = (JRadioButtonMenuItem) component;
				jradioButtonMenuItem.addPropertyChangeListener(this);
			}
		}
		
		// ChangeUpdate, InsertUpdate, ... di DocumentListener
		if (!componentName.equals("") && isDocumentListenerToAdd(mcb, componentName)) {
			if (classComponent == JTextField.class) {
				JTextField jtextField = (JTextField) component;
				jtextField.getDocument().addDocumentListener(this);
				}
			if (classComponent == JTextArea.class) {
				JTextArea jtextArea = (JTextArea) component;
				jtextArea.getDocument().addDocumentListener(this);
			}
			if (classComponent == JTextPane.class) {
				JTextPane jtextPane = (JTextPane) component;
				jtextPane.getDocument().addDocumentListener(this);
				return;
			}
			if (classComponent == JEditorPane.class) {
				JEditorPane jeditorPane = (JEditorPane) component;
				jeditorPane.getDocument().addDocumentListener(this);
			}
			if (classComponent == JPasswordField.class) {
				JPasswordField jpasswordField = (JPasswordField) component;
				jpasswordField.getDocument().addDocumentListener(this);
			}
			if (classComponent == JFormattedTextField.class) {
				JFormattedTextField jformattedTextField = (JFormattedTextField) component;
				jformattedTextField.getDocument().addDocumentListener(this);
			}
		}
		
		// AdjustmentValueChanged di AdjustmentListener
		if (!componentName.equals("") && isAdjustmentListenerToAdd(mcb, componentName)) {
			if (classComponent == JScrollBar.class) {
				JScrollBar jscrollBar = (JScrollBar) component;
				jscrollBar.addAdjustmentListener(this);
			}
		}
		
		// windowOpened, windowClosing, windowClosed, windowActivated, windowDeactivated, windowIconified, windowDeiconified  di WindowListener
		if (!componentName.equals("") && isWindowListenerToAdd(mcb, componentName)) {
			if (classComponent == JFrame.class) {
				jframe = (JFrame) component;
				jframe.addWindowListener(this);
			}
			if (classComponent == JDialog.class) {
				jdialog = (JDialog) component;
				jdialog.addWindowListener(this);
			}
		}
		
		// treeNodesChanged, treeNodesInserted,  di TreeModelListener, treeNodesRemoved, treeStructureChanged di TreeModelListener
		if (!componentName.equals("") && isTreeModelListenerToAdd(mcb, componentName)) {
			if (classComponent == JTree.class) {
				JTree jtree = (JTree) component;
				TreeModel treeModel = jtree.getModel();
				treeModel.addTreeModelListener(this);
			}
		}

		// treeNodesChanged di TreeSelectionListener
		if (!componentName.equals("") && isTreeSelectionlListenerToAdd(mcb, componentName)) {
			if (classComponent == JTree.class) {
				JTree jtree = (JTree) component;
				jtree.addTreeSelectionListener(this);
			}
		}
 
		// treeExpanded, treeCollapsed di TreeExpansionListener 
		if (!componentName.equals("") && isTreeExpansionListenerToAdd(mcb, componentName)) {
			if (classComponent == JTree.class) {
				JTree jtree = (JTree) component;
				jtree.addTreeExpansionListener(this);
			}
		}

	}

	/* --------------------------------------------------------------
	 * Impostazione proprietà specifiche definite per il componente.
	 * --------------------------------------------------------------
	 * 
	 * Si utilizzano le proprietà definite a livello di funzione e vengono eseguite via reflection.
	 * 
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private void setComponentPropertiesSpecific(InnerMonitorControBlock mcb, Class componentClass, String componentName, Object component) {
		
		ExceptionAmrita excp = null;
		Class<? extends JComponent> ar_class[] = null;
		Object ar_object[] = null;
		ArrayList<InnerComponentProperty> al_propertySpecific = null;
		
		al_propertySpecific = mcb.function.getProperties(componentName);
		if (al_propertySpecific == null) {
			return;
		}
		
		// Default specifici presenti per componente
		
		// Scan properties 
		for (InnerComponentProperty propertyDefault : al_propertySpecific) {
			ar_class = new Class[1];
			ar_class[0] = (Class<? extends JComponent>) propertyDefault.propertyValue.getClass();
			ar_object = new Object[1];
			ar_object[0] = propertyDefault.propertyValue;
			Object[] ar_return = this.rm.invokeMethodWithStatus(component, "set"+propertyDefault.propertyName, ar_class, ar_object);
			if (ar_return[0] == null) {continue;}
			
			// Logging con exception completa
	       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, (Exception) ar_return[0]);
		    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0200", new String[]{mcb.function.getClass().getSimpleName()
		                                                                   ,componentName
		                                                                   ,component.getClass().getSimpleName()
		                                                                   ,propertyDefault.propertyName
		                                                                   ,propertyDefault.propertyValue.toString()
		    															   }, excp);
		}		
	}



	/*
   * Restituisce an ImageIcon, or null if the path è errato. 
   */
  private ImageIcon createImageIcon(String path) {
      java.net.URL imgURL = this.getClass().getResource(path);
      if (imgURL != null) {
          return new ImageIcon(imgURL);
      } else {
          return null;
      }
  }

    /*
     * Verifica se qualche ON_EVENT necessita di un focus listener per il nome del controllo
     */
    private boolean isFocusListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
    	
    	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al focus
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_FOCUS_GAINED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_FOCUS_LOST) {
				continue;
			}
			
			// FocusListener da registrare
			return true;
		}
    	
		return false;
	}




    /*
     * Verifica se qualche ON_EVENT necessita di un KeyListener per il nome del controllo
     */
	private boolean isKeyListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al focus
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_KEY_PRESSED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_KEY_RELEASED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_KEY_TYPED) {
				continue;
			}
			
			// KeyListener da registrare
			return true;
		}
    	
		return false;
	}



    /*
     * Verifica se qualche ON_EVENT necessita di un MouseListener per il nome del controllo
     */
	private boolean isMouseListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
		ForwardPanelComponent panelComponent = null;
		InnerComponent innerComponent = null;
		ArrayList<InnerOnEvent> al_OnEvent = null;
		
    	al_OnEvent = mcb.function.getOnEventList();
     	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al focus
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_MOUSE_CLICKED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_MOUSE_ENTERED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_MOUSE_ENTERED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_MOUSE_PRESSED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_MOUSE_RELEASED
			&&  innerOnEvent.event != EnumForwardEvent.ON_DOUBLE_CLICK
			&&  innerOnEvent.event != EnumForwardEvent.ON_TREE_NODE_SELECTED
			) {
				continue;
			}
			
			// MouseListener da registrare
			return true;
		}
    	
      	// Verifica se popUpMenu associato al componente
    	innerComponent = mcb.function.getComponentDeclared(componentName);
    	
      	// Se jJTable registro comunque il listenere per set prorietà al click
        if (innerComponent.componentType == EnumForwardComponent.JTable) {
			return true;
		} 	
   
    	// Potrebbe essere un item di attivazione submenu e in questo caso non registrato come componente
 		if (innerComponent != null && !innerComponent.popUpMenuName.equals("")) {
			return true;
		}
    	
 		// Potrebbe essere stata dichiarata una funzione di lookup
 		panelComponent = mcb.function.getPanelComponent(componentName);
 		if (panelComponent != null && (panelComponent.getLookupTableNum() >= 0 || !panelComponent.getLookupFunction().equals(""))) {
			return true;
		}
 		
		return false;
	}



    /*
     * Verifica se qualche ON_EVENT necessita di un MouseMotionlistener per il nome del controllo
     */
	private boolean isMouseMotionListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al focus
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_MOUSE_DRAGGED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_MOUSE_MOVED) {
				continue;
			}
			
			// MouseMotionlistener da registrare
			return true;
		}
    	
		return false;
	}


    /*
     * Verifica se qualche ON_EVENT necessita di un MouseWheelListener per il nome del controllo
     */
	private boolean isMouseWheelListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al focus
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_WHEEL_MOVED) {
				continue;
			}
			
			// MouseWheelListener da registrare
			return true;
		}
    	
		return false;
	}


	/*
     * Se il nome del contrallo fornito è in qualche ON_CONDITION() da implementare con action listener
     * restituisce true
     */
	private boolean isActionListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	
    	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al focus
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			
			// Interessano sologli eventi per action performed
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_ACTION_PERFORMED
			&&  innerOnEvent.event != EnumForwardEvent.ON_CLICK
			&&  innerOnEvent.event != EnumForwardEvent.ON_DOUBLE_CLICK
			&&  innerOnEvent.event != EnumForwardEvent.ON_MENU_ITEM_SELECTED
			&&  innerOnEvent.event != EnumForwardEvent.ON_SYSTEM_TIMER_EXPIRED) {
				continue;
			}
			
			// Actionlistener da registrare
			return true;
		}
    	
		return false;
	}

	/*
     * Se il nome del contrallo fornito è in qualche ON_CONDITION() da implementare con listSelectionListener listener
     * restituisce true
     */
	private boolean isListSelectionListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi alla selezione di liste
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_VALUE_CHANGED 
			&&  innerOnEvent.event != EnumForwardEvent.ON_LIST_ROWS_SELECTED
			&&  innerOnEvent.event != EnumForwardEvent.ON_LIST_ROWS_UNSELECTED
			&&  innerOnEvent.event != EnumForwardEvent.ON_TABLE_ROWS_SELECTED
			&&  innerOnEvent.event != EnumForwardEvent.ON_TABLE_ROWS_UNSELECTED
			&&  innerOnEvent.event != EnumForwardEvent.ON_TABLE_COLS_SELECTED
			&&  innerOnEvent.event != EnumForwardEvent.ON_TABLE_COLS_UNSELECTED
			&&  innerOnEvent.event != EnumForwardEvent.ON_TABLE_CELLS_SELECTED
			&&  innerOnEvent.event != EnumForwardEvent.ON_TABLE_CELLS_UNSELECTED
			   ) {
				continue;
			}
			
			// ListSelectionlistener da registrare
			return true;
		}
		return false;
	}

	/*
     * Se il nome del contrallo fornito è in qualche ON_CONDITION() da implementare con itemListener listener
     * restituisce true. Il metodo di evento è ItemStateChanged
     */
	private boolean isItemListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al cambiamento di stato
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_ITEM_STATE_CHANGED
			&&  innerOnEvent.event != EnumForwardEvent.ON_CHECKBOX_CHECKED
			&&  innerOnEvent.event != EnumForwardEvent.ON_CHECKBOX_UNCHECKED
			&&  innerOnEvent.event != EnumForwardEvent.ON_RADIOBUTTON_CHECKED
			&&  innerOnEvent.event != EnumForwardEvent.ON_RADIOBUTTON_UNCHECKED
			&&  innerOnEvent.event != EnumForwardEvent.ON_BUTTON_TOGGLED
			&&  innerOnEvent.event != EnumForwardEvent.ON_BUTTON_UNTOGGLED
			&&  innerOnEvent.event != EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM
			&&  innerOnEvent.event != EnumForwardEvent.ON_MENU_ITEM_CHECKED
			&&  innerOnEvent.event != EnumForwardEvent.ON_MENU_ITEM_UNCHECKED
				) {
				continue;
			}
			
			// ListSelectionlistener da registrare
			return true;
		}
		return false;
	}


	/*
     * Se il nome del contrallo fornito è in qualche ON_CONDITION() da implementare con itemListener listener
     * restituisce true. Il metodo di evento è ItemStateChanged
     */
	private boolean isChangeListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al cambiamento di stato
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_STATE_CHANGED
			&&  innerOnEvent.event != EnumForwardEvent.ON_SLIDER_TICKED
			&&  innerOnEvent.event != EnumForwardEvent.ON_SPINNER_TICKED
			&&  innerOnEvent.event != EnumForwardEvent.ON_TABBED_PANE_TICKED
			   ) {
				continue;
			}
			
			// Changelistener da registrare 
			return true;
		}
		return false;
	}


	/*
     * Se il nome del contrallo fornito è in qualche ON_CONDITION() da implementare con itemListener listener
     * restituisce true. Il metodo di evento è ItemStateChanged
     */
	private boolean isPropertyChangeListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al cambiamento valore di proprietà
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_PROPERTY_CHANGED
			&&  innerOnEvent.event != EnumForwardEvent.ON_DIALOG_BEFORE_CLOSING
			   ) {
				continue;
			}
			
			// Changelistener da registrare
			return true;
		}
    	return false;
	}

	
	/*
     * Se il nome del contrallo fornito è in qualche ON_CONDITION() da implementare con itemListener listener
     * restituisce true. Il metodo di evento è ItemStateChanged
     */
	private boolean isDocumentListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al cambiamento valore di proprietà
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_CHANGE_UPDATE
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_INSERT_UPDATE
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_REMOVE_UPDATE
			&&  innerOnEvent.event != EnumForwardEvent.ON_CHARS_INSERT
			&&  innerOnEvent.event != EnumForwardEvent.ON_CHARS_UPDATE
			&&  innerOnEvent.event != EnumForwardEvent.ON_CHARS_DELETE
			   ) {
				continue;
			}
			
			// DocumentListener da registrare
			return true;
		}
    	return false;
	}


	
	/*
     * Se il nome del contrallo fornito è in qualche ON_CONDITION() da implementare con itemListener listener
     * restituisce true. Il metodo di evento è ItemStateChanged
     */
	private boolean isAdjustmentListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al cambiamento valore di proprietà
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_ADJUSTMENT_VALUE_CHANGED) {
				continue;
			}
			
			// AdjustmentListener da registrare
			return true;
		}
    	return false;
	}



	
	/*
     * Se il nome del contrallo fornito è in qualche ON_CONDITION() da implementare con windowListener  
     * restituisce true.  
     */
	private boolean isWindowListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al cambiamento valore di proprietà
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_WINDOW_ACTIVATED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_WINDOW_CLOSED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_WINDOW_CLOSING		
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_WINDOW_DEACTIVATED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_WINDOW_DEICONIFIED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_WINDOW_ICONIFIED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_WINDOW_OPENED		
			   ) {
				continue;
			}
			
			// WindowListener da registrare
			return true;
		}
    	
    	// Lo attivo in ogni caso
    	
		return false;
	}


	/*
     * Se il nome del contrallo fornito è in qualche ON_CONDITION() da implementare con itemListener listener
     * restituisce true. Il metodo di evento è ItemStateChanged
     */
	private boolean isTreeModelListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al cambiamento valore di proprietà
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_TREE_NODES_CHANGED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_TREE_NODES_INSERTED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_TREE_NODES_REMOVED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_TREE_STRUCTURE_CHANGED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_TREE_NODE_EDITED_TEXT		
			&&  innerOnEvent.event != EnumForwardEvent.ON_TREE_NODE_INSERTED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_TREE_NODE_REMOVED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_TREE_STRUCTURE_CHANGED		
			   ) {
				continue;
			}
			
			// WindowListener da registrare
			return true;
		}
		return false;
	}

	/*
     * Se il nome del contrallo fornito è in qualche ON_CONDITION() da implementare con listener
     * restituisce true.  
     */
	private boolean isTreeSelectionlListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al cambiamento valore di proprietà
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_TREE_VALUE_CHANGED
			&&  innerOnEvent.event != EnumForwardEvent.ON_TREE_NODE_SELECTED		
			   ) {
				continue;
			}
			
			// WindowListener da registrare
			return true;
		}
		return false;
	}
 
	/*
     * Se il nome del controllo fornito è in qualche ON_CONDITION() da implementare con listener
     * restituisce true.  
     */
	private boolean isTreeExpansionListenerToAdd(InnerMonitorControBlock mcb, String componentName) {
	   	ArrayList<InnerOnEvent> al_OnEvent = null;
    	al_OnEvent = mcb.function.getOnEventList();
    	
    	// Scan OnEvent declared
    	for (InnerOnEvent innerOnEvent : al_OnEvent) {
    		
    		// Interessano solo gli eventi sul componente relativi al cambiamento valore di proprietà
			if (!innerOnEvent.componentName.equals(componentName)) {continue;}
			if (innerOnEvent.event != EnumForwardEvent.ON_JAVA_TREE_EXPANDED
			&&  innerOnEvent.event != EnumForwardEvent.ON_JAVA_TREE_COLLAPSED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_TREE_NODE_EXPANDED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_TREE_NODE_COLLAPSED		
			&&  innerOnEvent.event != EnumForwardEvent.ON_TREE_NODE_CHILDREN_TO_ADD		
			   ) {
				continue;
			}
			
			// WindowListener da registrare
			return true;
		}
		return false;
	}


    /*
     * Per ogni pannello di dettaglio vengono disposti i controlli in base al layout manager.
     * 
     * Recupero pannelli di dettaglio con layout da utilizzare
     *  Per ogni pannello recupero componenti da inserire
     *    Disposizione nel pannello
     *    
     * Ai controlli da inserire nel pannello contenitore sono già state applicate tutte le
     * prprietà di default, quelle specifiche e i parametri di tuning del layout.
     * Sono pertanto controlli pronti per la disposizione finale.
     * Ogni controllo da inserire ha indicati il numero di riga e il progressivo nella riga.
     */
	private void functionDisposeControlsInDetailPanels(InnerMonitorControBlock mcb) {
		
		List<ForwardPanel> ls_panel = null;
		
		// La funzione era già stata richiamata e quindi illayout già completato
		if (mcb.function.getCounterStarts() > 1) {
			return;
		}
		
		ls_panel = mcb.function.getPanelsDetail();
		
		for (ForwardPanel forwardPanel : ls_panel) {
			
			// BOX_LAYOUT panelTableSelType
			if (forwardPanel.getLayoutManager() == EnumForwardLayout.BOX_LAYOUT) {
				disposeControlsInDetailPanelsBoxLayout(mcb, forwardPanel);
				manageLogicDeclaredActivation(mcb, forwardPanel.getName(), EnumForwardEvent.ON_PANEL_AFTER_CONTROLS_DECLARING);
				continue;
			}
			// FLOW_LAYOUT
			if (forwardPanel.getLayoutManager() == EnumForwardLayout.FLOW_LAYOUT) {
				disposeControlsInDetailPanelsFlowLayout(mcb, forwardPanel);
				manageLogicDeclaredActivation(mcb, forwardPanel.getName(), EnumForwardEvent.ON_PANEL_AFTER_CONTROLS_DECLARING);
				continue;
			}
			// GRID_LAYOUT
			if (forwardPanel.getLayoutManager() == EnumForwardLayout.GRID_LAYOUT) {
				disposeControlsInDetailPanelGridLayout(mcb, forwardPanel);
				manageLogicDeclaredActivation(mcb, forwardPanel.getName(), EnumForwardEvent.ON_PANEL_AFTER_CONTROLS_DECLARING);
				continue;
			}
			// GRID_BAG_LAYOUT
			if (forwardPanel.getLayoutManager() == EnumForwardLayout.GRID_BAG_LAYOUT) {
				disposeControlsInDetailPanelGridBagLayout(mcb, forwardPanel);
				manageLogicDeclaredActivation(mcb, forwardPanel.getName(), EnumForwardEvent.ON_PANEL_AFTER_CONTROLS_DECLARING);
				continue;
			}
		}
	}


    /*
     * Dispone i controlli sul pannello utilizzando BOX layout manager.
     * 
     * Axis è verticale e viene inserito un BOX di servizio per ogni riga.
     * In questo BOX di servizio viene usato sempre BOX_LAYOUT con axis orizzontale.
     * Su quest'ultimo BOX di servizio vengono disposti i controlli, di riga, in sequenza.
     * Eventuali spazi fra le righe sono gestiti da un separatore di servizio di BOX_LAYOUT
     * Eventuali spazi fra i campi della riga sono gestiti da un separatore di servizio sempre di BOX_LAYOUT
    */
    private void disposeControlsInDetailPanelsBoxLayout(InnerMonitorControBlock mcb, ForwardPanel forwardPanel) {
    	
    	JPanel panelDetail = null;
    	JComponent jcomponent = null;
    	JScrollPane jscrollPane = null;
       	Box boxHorizontal = null;
		int rowPrec = -1;
		boolean isVerticalGlueToAppend = true;
		
		// Il box verticale conterrà i box con le righe (possono essere box di spaziatura o glue)
		panelDetail = mcb.function.getJPanel(forwardPanel.getName());
		panelDetail.setLayout(new BoxLayout(panelDetail, BoxLayout.Y_AXIS));
		
		// Nessun componente di dettaglio definito per ilpannello
		if (forwardPanel.getComponents().size() == 0) {
			return;
		}
		
		// Scan componenti pannello
		for (ForwardPanelComponent panelComponent : forwardPanel.getComponents()) {
			
			// Inizio riga, creazione box riga
			if (panelComponent.getRow() != rowPrec) {
				
				// Inserimento BOX con riga precedente completata
				if (rowPrec != -1) {
					boxHorizontal.add(Box.createHorizontalGlue());
					panelDetail.add(boxHorizontal);
				}
				
				// Allocazione BOX per riga corrente
				rowPrec = panelComponent.getRow();
				boxHorizontal = Box.createHorizontalBox();
				
			}

			// Unico campo nella riga: verifica se un Box separator
			if (forwardPanel.getComponentsRowCount(panelComponent.getRow()) == 1) {
				// Spaziatura fra righe con area rigida
				if (panelComponent.getType() == EnumForwardComponent.JBoxRigidArea) {
					panelDetail.add(Box.createRigidArea(new Dimension(panelComponent.getFillerHorizontal(), panelComponent.getFillerVertical())));
					continue;
				}
				// Spaziatura fra righe con filler verticale
				if (panelComponent.getType() == EnumForwardComponent.JBoxStrutVertical) {
					panelDetail.add(Box.createVerticalStrut(panelComponent.getFillerVertical()));
					continue;
				}
				// Spaziatura fra righe con filler elastico
				if (panelComponent.getType() == EnumForwardComponent.JBoxGlueVertical) {
					panelDetail.add(Box.createVerticalGlue());
					isVerticalGlueToAppend = false;
					continue;
				}
			}

			
			// Spaziatura fra colonne con area rigida
			if (panelComponent.getType() == EnumForwardComponent.JBoxRigidArea) {
				boxHorizontal.add(Box.createRigidArea(new Dimension(panelComponent.getFillerHorizontal(), panelComponent.getFillerVertical())));
				continue;
			}
			// Spaziatura fra colonne con filler orizzontale
			if (panelComponent.getType() == EnumForwardComponent.JBoxStrutHorizontal) {
				boxHorizontal.add(Box.createHorizontalStrut(panelComponent.getFillerHorizontal()));
				continue;
			}
			// Spaziatura fra colonne con filler elastico horizzontale
			if (panelComponent.getType() == EnumForwardComponent.JBoxGlueHorizontal) {
				boxHorizontal.add(Box.createHorizontalGlue());
				continue;
			}
			// Spaziatura fra colonne con filler elastico horizzontale/verticale
			if (panelComponent.getType() == EnumForwardComponent.JBoxGlue) {
				boxHorizontal.add(Box.createGlue());
				continue;
			}
			// Spaziatura fra colonne con filler verticale: skip
			if (panelComponent.getType() == EnumForwardComponent.JBoxStrutVertical) {
				continue;
			}
			
			// Componente applicativo da disporre in riga
			jcomponent = mcb.function.getJComponent(panelComponent.getName());
			
			// Al posto del componente deve essere disposto il JScrollPane che lo contiene
			if (panelComponent.getScrollPane() != null) {
 				jscrollPane = panelComponent.getScrollPane();
 				jcomponent = jscrollPane;
			}
			boxHorizontal.add(jcomponent);
		}
		
		// Ultima riga pendente
		boxHorizontal.add(Box.createHorizontalGlue());
		panelDetail.add(boxHorizontal);
		
		// Se non c'è nessuna riga con un solo campo di glue verticale definito, bisogna accodare 
		// un glue finale per evitare che swing allinei l'ultimo controllo in basso.
		// Se invece è stata dichiarata almeno una riga di con un solo campo di glue verticale
		// allora non si deve fare nulla, per rispettare il layout utente
		if (isVerticalGlueToAppend) {
			panelDetail.add(Box.createVerticalGlue());
		}
}



    /*
     * Dispone i controlli sul pannello utilizzando FLOW_LAYOUT manager.
     * 
     * E' il più semplice dei layout manager.
     * Semplicemente si aggiungono i componenti al pannello.
     * I componenti di separazione vengono scartati.
     */
	private void disposeControlsInDetailPanelsFlowLayout(InnerMonitorControBlock mcb, ForwardPanel forwardPanel) {
	   	
    	JPanel panelDetail = null;
    	JComponent jcomponent = null;
    	FlowLayout flowLayout = null;
    	JComponent jscrollPane = null;
    	
    	// Nuovo grid layout
    	flowLayout = new FlowLayout();
		flowLayout.setAlignment(forwardPanel.getFlowLayoutAlign());
		flowLayout.setHgap(forwardPanel.getHgap());
		flowLayout.setVgap(forwardPanel.getVgap());
		panelDetail = mcb.function.getJPanel(forwardPanel.getName());
		panelDetail.setLayout(flowLayout);
		
		// Scan componenti
		for (ForwardPanelComponent panelComponent : forwardPanel.getComponents()) {
			
			// Componenti di spaziatura NON gestiti da questo layout manager: skip
			if (panelComponent.getType() == EnumForwardComponent.JBoxRigidArea
			||  panelComponent.getType() == EnumForwardComponent.JBoxStrutHorizontal 
			||  panelComponent.getType() == EnumForwardComponent.JBoxGlueHorizontal) {
				continue;
			}
			
			// Componente applicativo nel pannello
			jcomponent = mcb.function.getJComponent(panelComponent.getName());
			
			// Al posto del componente deve essere disposto il JScrollPane che lo contiene
			if (panelComponent.getScrollPane() != null) {
 				jscrollPane = panelComponent.getScrollPane();
				jcomponent = jscrollPane;
			}

			panelDetail.add(jcomponent);
		}
	}

   /* -------------------------------------------------------------------
    * Dispone i controlli sul pannello utilizzando GRID_LAYOUT  manager.
    * -------------------------------------------------------------------
    * 
    * Questo layout manager predispone una una griglia con le celle di eguali dimensioni.
    * Adatto per icone, immaggini, pulsanti etc.
    * Dispone dalla prima riga, prima colonna e prosegue sulla stessa riga e sulle successive.
    * I componenti devono essere stati dichiarati per adattarsi a questa disposizione.
    */
	private void disposeControlsInDetailPanelGridLayout(InnerMonitorControBlock mcb, ForwardPanel forwardPanel) {
    	
		JPanel panelDetail = null;
    	JComponent jcomponent = null;
    	GridLayout gridLayout = null;
       	JScrollPane jscrollPane = null;
       	
    	// Nuovo grid layout
		gridLayout = new GridLayout();
		gridLayout.setRows(forwardPanel.getGridRows());
		gridLayout.setColumns(forwardPanel.getGridColumns());
		gridLayout.setHgap(forwardPanel.getHgap());
		gridLayout.setVgap(forwardPanel.getVgap());
		panelDetail = mcb.function.getJPanel(forwardPanel.getName());
		panelDetail.setLayout(gridLayout);
		
		// Scan componenti
		for (ForwardPanelComponent panelComponent : forwardPanel.getComponents()) {
			
			// Componenti di spaziatura NON gestiti da questo layout manager: skip
			if (panelComponent.getType() == EnumForwardComponent.JBoxRigidArea
			||  panelComponent.getType() == EnumForwardComponent.JBoxStrutHorizontal 
			||  panelComponent.getType() == EnumForwardComponent.JBoxGlueHorizontal) {
				continue;
			}
			
			// Componente applicativo nel pannello
			jcomponent = mcb.function.getJComponent(panelComponent.getName());
			
			// Al posto del componente deve essere disposto il JScrollPane che lo contiene
			if (panelComponent.getScrollPane() != null) {
 				jscrollPane = panelComponent.getScrollPane();
				jcomponent = jscrollPane;
			}

			panelDetail.add(jcomponent);
		}
	}




   /* -----------------------------------------------------------------------
    * Dispone i controlli sul pannello utilizzando GRID_BAG_LAYOUT  manager.
    * -----------------------------------------------------------------------
    * 
    * Questo layout manager dispone i componenti nel modo più flessibile possibile.
    * Ogni componente da inserire ha dei vincoli dati da un oggetto GridBagConstraints.
    * 
    * I vincoli sono:
    *  gridx 			Riga cella di inizio 
    *  gridy 			Colonna cella di inizio 
    *  gridWidth 		Numero celle di espansione sulla riga (default 1)
    *  gridHeght 		Numero celle di espansione sulla colonna (default 1)
    *  internalPaddingX Riempimento interno alla cella orizzontale (in pixel)
    *  internalPaddingY Riempimento interno alla cella verticale (in pixel)
    *  fill 			Riempimento della cella ai valori massimi componente come  GridBagConstraints.NONE HORIZONTAL VERTICAL BOTH
    *  anchor 			Posizionamento nella cella come GridBagConstraints.LINE_START LINE_END ...
    *  inset 			Distanza in pixel dai bordi della cella
    *  
    * I valori dell'oggetto vincolo sono stati completamente definiti a livello di dichiarazione
    * della funzione, per ogni componente inserita nel pannello.
    * 
    */
	private void disposeControlsInDetailPanelGridBagLayout(InnerMonitorControBlock mcb, ForwardPanel forwardPanel) {
		GridBagConstraints gridBagConstraints = null;

		JPanel panelDetail = null;
    	JComponent jcomponent = null;
    	GridBagLayout gridBagLayout = null;
    	 
    	// Nuovo grid layout
    	gridBagLayout = new GridBagLayout();
		panelDetail = mcb.function.getJPanel(forwardPanel.getName());
		panelDetail.setLayout(gridBagLayout);
		
		// Scan componenti
		for (ForwardPanelComponent panelComponent : forwardPanel.getComponents()) {
			
			// Componenti di spaziatura NON gestiti da questo layout manager: skip
			if (panelComponent.getType() == EnumForwardComponent.JBoxRigidArea
			||  panelComponent.getType() == EnumForwardComponent.JBoxStrutHorizontal 
			||  panelComponent.getType() == EnumForwardComponent.JBoxGlueHorizontal) {
				continue;
			}
			
			// Componente applicativo nel pannello
			jcomponent = mcb.function.getJComponent(panelComponent.getName());
			gridBagConstraints = panelComponent.getConstraints();
			
			// Al posto del componente deve essere disposto il JScrollPane che lo contiene
			if (panelComponent.getScrollPane() != null) {
 				JScrollPane jscrollPane = panelComponent.getScrollPane();
				jcomponent = jscrollPane;
			}

			panelDetail.add(jcomponent, gridBagConstraints);
		}
	}





	/* -------------------------
	 * Creazione form principale
	 * -------------------------
	 * 
	 * Viene completato il pannello root del form principale, da attivare alla partenza.
     * A questo punto i controlli elementari sono definiti, con proprietà e listener impostati.
     * Inoltre sono già stati disposti nei relativi pannelli di dettaglio.
     * In questa fase si devono disporre i pannelli container secondo i layout manager dichiarati.
     * Se i pannelli sono di tipo predefinito, come quelli di menu, vengono istanziati e disposti
     * direttamente, in quanto sono indipendenti e gestiscono a volte in parte gli eventi.
     * La disposizione ricorsiva dei pannelli produce il mainForm panel, relativa per esempio 
     * al form di partenza.
     * 
     */
	private void functionCreateMainFormPanel(InnerMonitorControBlock mcb, ForwardForm formStart) {
		InnerForwardPanel innerForwardPanel = null;
		
		// La funzione era già stata richiamata e quindi il main panel già creato
		if (mcb.function.getCounterStarts() > 1) {
			return;
		}
		
		innerForwardPanel = formStart.getPanelStructure(formStart.getRootPanelName());
		layOutPanelsRecursive(mcb, formStart, innerForwardPanel);
	}

	/* ------------------------------------
	 * Disposizione ricorsiva pannelli.
	 * ------------------------------------
	 * 
	 * La prima esecuzione ha inizio dal rootPanel.
	 * Le successive esecuzioni estraggono ricorsivamente i pannelli figli.
	 *  
	 */
    private void layOutPanelsRecursive(InnerMonitorControBlock mcb, ForwardForm formStart, InnerForwardPanel innerForwardPanel) {
    	
    	ForwardForm formDeclared = null;
    	ForwardPanel forwardPanel = null;
       	ForwardPanelMenu forwardPanelMenu = null;
       	ForwardMenu forwardMenu = null;
    	InnerForwardPanel innerForwardPanelRecursive = null;
    	InnerForwardPanel innerForwardPanelForm = null;
       	JPanel jpanel = null;
     	JTabbedPane jtabbedPane = null;
     	JSplitPane jsplitPane = null;
     	Container jcontainerLayedOut = null;
    	JPanel jpanelLayedOutLeft = null;
    	JPanel jpanelLayedOutRight = null;
     	BorderLayout borderLayout = null;
    	FlowLayout flowLayout = null;
    	BoxLayout boxLayout = null;
    	CardLayout cardLayout = null;
     	String panelTabText = "";
     	String panelLeft = "";
     	String panelRight = "";
    	String menuName = "";
    	int axis = 0;
    	
		switch (innerForwardPanel.panelType) {
		
		    // Panel di dettaglio applicativo, nessuna operazione. 
			// Panel pronto per essere disposto sul layout
			case DETAIL:
				forwardPanel = innerForwardPanel.panel;
				forwardPanel.setForwardMonitor(this);
				forwardPanel.setFunction(mcb.function);
				break;
            
			// Panel container di altri panel e/o contenitori
			case CONTAINER:
				forwardPanel = innerForwardPanel.panel;
				forwardPanel.setForwardMonitor(this);
				forwardPanel.setFunction(mcb.function);
				jpanel = (JPanel) forwardPanel.getGraphicObject();					// Istanziato in fase di dichiarazione
				
				// Gestione panel con border layout
				if (innerForwardPanel.layoutManager == EnumForwardLayout.BORDER_LAYOUT) {
					
					borderLayout = new BorderLayout();
					borderLayout.setHgap(forwardPanel.getHgap());
					borderLayout.setVgap(forwardPanel.getVgap());
					forwardPanel.setBorderLayout(borderLayout);
					jpanel.setLayout(borderLayout);
					
					// Disposizione pannelli, composti ricorsivamente
					if (!innerForwardPanel.panelChildrenNorth.equals("")) {
						innerForwardPanelRecursive = formStart.getPanelStructure(innerForwardPanel.panelChildrenNorth);
						// pannello dichiarato e definito
						if (innerForwardPanelRecursive != null && innerForwardPanelRecursive.panel != null) {
							layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
							jcontainerLayedOut = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
							jpanel.add(jcontainerLayedOut,  BorderLayout.PAGE_START);
						}
					}	
					if (!innerForwardPanel.panelChildrenWest.equals("") && innerForwardPanel.panel != null) {
						innerForwardPanelRecursive = formStart.getPanelStructure(innerForwardPanel.panelChildrenWest);
						// pannello dichiarato e definito
						if (innerForwardPanelRecursive != null  && innerForwardPanelRecursive.panel != null) {
							layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
							jcontainerLayedOut = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
							jpanel.add(jcontainerLayedOut,  BorderLayout.LINE_START);
						}
					}
					if (!innerForwardPanel.panelChildrenEast.equals("")) {
						innerForwardPanelRecursive = formStart.getPanelStructure(innerForwardPanel.panelChildrenEast);
						// pannello dichiarato e definito
						if (innerForwardPanelRecursive != null  && innerForwardPanelRecursive.panel != null) {
							layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
							jcontainerLayedOut = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
							jpanel.add(jcontainerLayedOut,  BorderLayout.LINE_END);
						}
					}
					if (!innerForwardPanel.panelChildrenSouth.equals("")) {
						innerForwardPanelRecursive = formStart.getPanelStructure(innerForwardPanel.panelChildrenSouth);
						// pannello dichiarato e definito
						if (innerForwardPanelRecursive != null  && innerForwardPanelRecursive.panel != null) {
							layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
							jcontainerLayedOut = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
							jpanel.add(jcontainerLayedOut,  BorderLayout.PAGE_END);
						}
					}
					if (!innerForwardPanel.panelChildrenCenter.equals("")) {
						innerForwardPanelRecursive = formStart.getPanelStructure(innerForwardPanel.panelChildrenCenter);
						// pannello dichiarato e definito
						if (innerForwardPanelRecursive != null  && innerForwardPanelRecursive.panel != null) {
							layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
							jcontainerLayedOut = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
							jpanel.add(jcontainerLayedOut, BorderLayout.CENTER);
						}
					}
					return;
				}
				
				// Gestione panel con flow layout
				if (innerForwardPanel.layoutManager == EnumForwardLayout.FLOW_LAYOUT) {
					
					flowLayout = new FlowLayout();
					flowLayout.setHgap(forwardPanel.getHgap());
					flowLayout.setVgap(forwardPanel.getVgap());
					flowLayout.setAlignment(forwardPanel.getFlowLayoutAlign());
					forwardPanel.setFlowLayout(flowLayout);
					jpanel.setLayout(flowLayout);
					
					// Scan pannelli da disporre
					for (String panelChildren : innerForwardPanel.al_panelChildren) {
						innerForwardPanelRecursive = formStart.getPanelStructure(panelChildren);
						if (innerForwardPanelRecursive == null) {continue;}							// pannello dichiarato ma non definito
						if (innerForwardPanelRecursive.panel == null) {continue;}					// pannello dichiarato ma non definito
						layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
						jcontainerLayedOut = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
						jpanel.add(jcontainerLayedOut);
					}
				}

				// Gestione panel con box layout
				if (innerForwardPanel.layoutManager == EnumForwardLayout.BOX_LAYOUT) {
					axis = BoxLayout.Y_AXIS;
					if (innerForwardPanel.layoutAxis == ForwardForm.AXIS_HORIZONTAL) {axis = BoxLayout.X_AXIS;}
					boxLayout = new BoxLayout(jpanel, axis);
					forwardPanel.setBoxLayout(boxLayout);
					jpanel.setLayout(boxLayout);
					
					// Scan pannelli da disporre
					for (String panelChildren : innerForwardPanel.al_panelChildren) {
						innerForwardPanelRecursive = formStart.getPanelStructure(panelChildren);
						if (innerForwardPanelRecursive == null) {continue;}							// pannello dichiarato ma non definito
						if (innerForwardPanelRecursive.panel == null) {continue;}					// pannello dichiarato ma non definito
						layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
						jcontainerLayedOut = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
						jpanel.add(jcontainerLayedOut);
					}
				}
				
				// Gestione panel con card layout
				if (innerForwardPanel.layoutManager == EnumForwardLayout.CARD_LAYOUT) {
					cardLayout = new CardLayout();
					cardLayout.setHgap(forwardPanel.getHgap());
					cardLayout.setVgap(forwardPanel.getVgap());
					forwardPanel.setCardLayout(cardLayout);
					jpanel.setLayout(cardLayout);
					
					// Scan pannelli da disporre
					for (String panelChildren : innerForwardPanel.al_panelChildren) {
						innerForwardPanelRecursive = formStart.getPanelStructure(panelChildren);
						if (innerForwardPanelRecursive == null) {continue;}							// pannello dichiarato ma non definito
						if (innerForwardPanelRecursive.panel == null) {continue;}					// pannello dichiarato ma non definito
						layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
						jcontainerLayedOut = (Container) innerForwardPanelRecursive.panel.getGraphicObject();
						jpanel.add(jcontainerLayedOut, jcontainerLayedOut.getName());
					}
				}
				break;
            
			// Panel tabbed
			case TABBED:
				
				forwardPanel = innerForwardPanel.panel;
				forwardPanel.setForwardMonitor(this);
				forwardPanel.setFunction(mcb.function);
				jtabbedPane = (JTabbedPane) forwardPanel.getGraphicObject();					// Istanziato in fase di dichiarazione
			
				// Scan pannelli da disporre
				for (String panelChildren : innerForwardPanel.al_panelChildren) {
					innerForwardPanelRecursive = formStart.getPanelStructure(panelChildren);
					if (innerForwardPanelRecursive == null) {break;}							// pannello dichiarato ma non definito
					if (innerForwardPanelRecursive.panel == null) {continue;}					// pannello dichiarato ma non definito
					layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
					jcontainerLayedOut = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
					if (innerForwardPanelRecursive.panel.getTabText().equals("")) {
						panelTabText = jcontainerLayedOut.getName();
					} else {
						panelTabText = innerForwardPanelRecursive.panel.getTabText();
					}
					jtabbedPane.addTab(panelTabText, jcontainerLayedOut);
				}
				break;
	
		    // Split panel.
			// Tutte le caratteristiche sono già state impostate in dichiarazione (orientation, divider)
			case SPLIT:
				
				forwardPanel = innerForwardPanel.panel;
				forwardPanel.setForwardMonitor(this);
				forwardPanel.setFunction(mcb.function);
				jsplitPane = (JSplitPane) forwardPanel.getGraphicObject();					// Istanziato in fase di dichiarazione
				
				// Pannello left/top
				panelLeft = innerForwardPanel.al_panelChildren.get(0);
				innerForwardPanelRecursive = formStart.getPanelStructure(panelLeft);
				if (innerForwardPanelRecursive == null) {break;}							// pannello dichiarato ma non definito
				if (innerForwardPanelRecursive.panel == null) {break;}					    // pannello dichiarato ma non definito
				layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
				jpanelLayedOutLeft = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
				// Pannello right/bottom
				panelRight = innerForwardPanel.al_panelChildren.get(1);
				innerForwardPanelRecursive = formStart.getPanelStructure(panelRight);
				if (innerForwardPanelRecursive == null) {break;}							// pannello dichiarato ma non definito
				if (innerForwardPanelRecursive.panel == null) {break;}					    // pannello dichiarato ma non definito
				layOutPanelsRecursive(mcb, formStart, innerForwardPanelRecursive);
				jpanelLayedOutRight = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
                // Inserimento pannelli
				jsplitPane.setLeftComponent(jpanelLayedOutLeft);
				jsplitPane.setRightComponent(jpanelLayedOutRight);
				break;
				
			// Form.
			// Viene disposto il panelRoot del form dichiarato per il pannello.
			// Il panelRoot rappresenta il risultato finale del processo ricorsivo di composizione del pannello.
			case FORM:
				formDeclared = null;
				forwardPanel = innerForwardPanel.panel;
				forwardPanel.setForwardMonitor(this);
				forwardPanel.setFunction(mcb.function);
				jpanel = (JPanel) forwardPanel.getGraphicObject();					// Istanziato in fase di dichiarazione

				// Scan form definiti nella funzione
				for (ForwardForm form : mcb.function.getForms()) {
					if (form.getFormType() == EnumForwardOption.FORM_TYPE_DECLARED) {
						formDeclared = form;
						break;
					}
				}
				// Form dichiarato ma non definito
				if (formDeclared == null) {break;}
                // Disposizione ricorsiva a partire dal root panel del form
				innerForwardPanelRecursive = formDeclared.getPanelStructure(formDeclared.getRootPanelName());
				layOutPanelsRecursive(mcb, formDeclared, innerForwardPanelForm);
				jcontainerLayedOut = (JPanel) innerForwardPanelRecursive.panel.getGraphicObject();
				jpanel.add(jcontainerLayedOut, BorderLayout.CENTER);				// Per permettere l'espandersi alla dimensione massima
				break;
			
			// Menu collocabile in un pannello.
			// Si tratta di menu orizzontali o verticali di pulsanti o label.
			// In caso di menu toolbar, dovrebbe essere disposta in BorderLayout.NORTH, con disposizione applicativa
			case MENU:
				forwardPanel = innerForwardPanel.panel;
				forwardPanel.setForwardMonitor(this);
				forwardPanel.setFunction(mcb.function);
				jpanel = (JPanel) forwardPanel.getGraphicObject();				// Istanziato in fase di dichiarazione
				menuName = innerForwardPanel.menuName;
				forwardMenu = mcb.function.getMenu(menuName);
				if (forwardMenu == null) {break;}								// menu dichiarato ma non definito
				forwardMenu.setForwardMonitor(this);
				forwardPanelMenu = new ForwardPanelMenu(this, forwardPanel.getName(), forwardMenu);
				forwardPanelMenu.getActivesJFrame().add(mcb.frameFunction);
				forwardPanelMenu.setForwardMonitor(this);
				forwardPanelMenu.setFunction(mcb.function);
				jcontainerLayedOut = (JPanel) forwardPanelMenu.getGraphicObject();
				// Il panel toolbar va allineato a sinistra senza bordi
				if (forwardMenu.getMenuRendering() == EnumForwardOption.MENU_RENDERING_TOOLBAR) {
					jpanel.setLayout(new BorderLayout());
					jpanel.add(jcontainerLayedOut, BorderLayout.PAGE_START);
				} else {
					jpanel.add(jcontainerLayedOut, BorderLayout.CENTER);				// Per permettere l'espandersi alla dimensione massima
				}
				break;
				
			case LIST:
				break;

			case TREE:
				break;

			case GRID:
				break;

			case BUTTON_GRID_PILOT:
				break;

			case TABLE_CONTENT:
				break;

			case TABLE_CONTENT_LIST:
				break;

			case DUAL_SELECTION:
				break;

			case PROPERTIES:
				break;

			case USER_DEFINED:
				break;
			case HELP_HTML:
				break;

			case SOURCE_VIEWER:
				break;

			case DIAGRAM:
				break;

			case LOGIN:
				break;

			case SPLASH:
				break;
				
			default:
				break;
		}

		return;
	}
    
    
 


	/** <h4>Perform unconditionally an action</h4> 
	 * 
	 * Actions are ranked by {@link EnumForwardAction} and can be automatically activated by declaring an action<br>
	 * on a <code>ON_EVENT()</code> or invoking a <code>ACTION()</code> directive in any application reusable method.<br>
	 * <p>
	 * In the first case the activation is made automatically by the forward monitor when the event specified occurs.<br>
	 * In the second case the activation is fully indipendent and decided only by specific user java application code logic.<br>
	 * Furthermore, the user java application code can itself be activated by an <code>ON_EVENT()</code> declaration and<br>
	 * phisically the code (a normal java method) can be coded in the same class declaring the function (a <code>ForwardFunction</code> object)<br>
	 * or in any other class configutated runtime by forward monitor.<br>
	 * <br>
	 * This logic execution model lets to configure runtime whith no coding wich elementary code to execute, making<br>
	 * the application like a big puzzle where any piece can be changed without to modify the shape.<br> 
	 * <p>
	 * As final result forward realize an applicative behaviour that can be completely configuration dependent and <br>
	 * even any bit of code fully reusable.<br>
	 * The application model and any user application code can be deployed and executed in the web context, by the specific<br>
	 * monitor (a specialized servelet), whith no changes.
	 * <p>
	 * For example you could code an <code>ACTION</code<br> that under specific circumstances causes the refresh of<br>
	 * a panel, the execution of a logica data view or discard a row in table under loading.
	 * <p>
	 * Here one example:<br>
	 * <p>
	 * <pre>
	 * ON_EVENT(EnumForwardEvent.value, "panel", "field", EnumForwardAction.REFRESH, "objectName1",...,"objectNamen")
	 * </pre>
	 * @param action the action type as coded by forward
	 * @param actionCoded the coded action parameters
	 */
	public void actionStarter(InnerMonitorControBlock mcb, EnumForwardAction action, ForwardDoParms actionCoded) {

		InnerOnEvent innerOnEvent = null;
		innerOnEvent = mcb.function.factoryGetInstanceInnerOnEvent();
		innerOnEvent.action = action;
		innerOnEvent.componentName = actionCoded.componentName;
		innerOnEvent.al_actionCoded.add(actionCoded);
		mcb.system.setActiveAction(action);
		mcb.system.setActiveEvent(innerOnEvent.event);
		execActions(mcb, innerOnEvent);
		
	}
	
	
	/**
	 * Creates a variable in the runtime monitor execution<br>
	 * If a variable with the same name already exists, it will be replaced.<br>
	 * <p>
	 */
	public void createVar(EnumForwardScope scope, Class<?> varType, String varName, Object valueInitial) {
		InnerVar innerVar;
		
		innerVar = new InnerVar();
		innerVar.varName = varName;
		innerVar.varType = varType;
		innerVar.varScope = scope;
		innerVar.varInitial = valueInitial;
		
		switch (scope) {
			case SCOPE_FUNCTION:
				mcb.hm_varScopeFunction.put(varName, innerVar);
				break;
			case SCOPE_SESSION:
				mcb.hm_varScopeSession.put(varName, innerVar);
				break;
			case SCOPE_SERVER:
				mcb.hm_varScopeServer.put(varName, innerVar);
				break;
		}
		return;
	}

	/**
	 * Returns true if the variable is declared for the scope specified.<br>
	 * <p>
	 * If the variable is not declared or is not declared for the scope specified
	 * returns false.<br>
	 * <p>
	 * @param varName
	 * @param EnumForwardScope scope
	 */
	public boolean isVarDeclared(String varName, EnumForwardScope scope) {
		InnerVar innerVar = null;
		InnerMonitorControBlock mcb = null;
		mcb = this.al_monitorControlBlock.get(mcbIndex);
		
		switch (scope) {
			case SCOPE_FUNCTION:
				innerVar = mcb.hm_varScopeFunction.get(varName);
				break;
			case SCOPE_SESSION:
				innerVar = mcb.hm_varScopeSession.get(varName);
				break;
			case SCOPE_SERVER:
				innerVar = mcb.hm_varScopeServer.get(varName);
				break;
		}
		if (innerVar != null) {return true;}
		return false;
	}


	/**
	 * Return the current value of the variable in the runtime forward monitor at function scope level.
	 * <br>
	 * This method returns the object containing the value of the variable to be casted invoking it.<br>
	 * <p>
	 * If the variable doesn't exist in the scope specified, a null value will be returned.<br>
	 * <p>
	 * @param varName
	 * @return the object var value
	 */
	public Object getVarValue(InnerMonitorControBlock mcb, String varName) {
		InnerVar innerVar = null;
		
		innerVar = getVarInnerVar(varName, EnumForwardScope.SCOPE_FUNCTION, mcb);
		if (innerVar == null) {return null;}
		return innerVar.varObject;
	}
    
	/**
	 * Return the current value of the variable in the runtime forward monitor in the scope required.
	 * <br>
	 * This method returns the object containing the value of the variable to be casted invoking it.<br>
	 * <p>
	 * If the variable doesn't exist in the scope specified, a null value will be returned.<br>
	 * <p>
	 * @param varName
	 * @param scope as function, session or server scope
	 * @return the object var value
	 */
	public Object getVarValue(InnerMonitorControBlock mcb, String varName, EnumForwardScope scope) {
		InnerVar innerVar = null;
		innerVar = getVarInnerVar(varName, scope, mcb);
		if (innerVar == null) {return null;}
		return innerVar.varObject;
	}
    
	/**
	 * Gets the variable descriptor in the currently active application function<br>
	 * <p>
	 * @param varName the variable name
	 * @param scope as a <code>EnumForwardScope</code> enumeration value
	 * @param mcb as a InnerMonitorControBlock object
	 */
	public InnerVar getVarInnerVar(String varName, EnumForwardScope scope, InnerMonitorControBlock mcb) {
		InnerVar innerVar = null;

		switch (scope) {
			case SCOPE_FUNCTION:
				innerVar = mcb.hm_varScopeFunction.get(varName);
				break;
			case SCOPE_SESSION:
				innerVar = mcb.hm_varScopeSession.get(varName);
				break;
			case SCOPE_SERVER:
				innerVar = mcb.hm_varScopeServer.get(varName);
				break;
		}
		if (innerVar == null) {return null;}
		return innerVar;
	}

	/**
	 * Gets the variable descriptor in the application function active in the input monitor control block<br>
	 * <p>
	 * @param mcb the <code>InnerMonitorControBlock</code> to be used as reference for an application function
	 * @param varName the variable name
	 * @param scope as a <code>EnumForwardScope</code> enumeration value
	 */
	public InnerVar getVarInnerVar(InnerMonitorControBlock mcb, String varName, EnumForwardScope scope) {
		InnerVar innerVar = null;

		switch (scope) {
			case SCOPE_FUNCTION:
				innerVar = mcb.hm_varScopeFunction.get(varName);
				break;
			case SCOPE_SESSION:
				innerVar = mcb.hm_varScopeSession.get(varName);
				break;
			case SCOPE_SERVER:
				innerVar = mcb.hm_varScopeServer.get(varName);
				break;
		}
		if (innerVar == null) {return null;}
		return innerVar;
	}

	/**
	 * Set a variable value, in the scope at function level, in the runtime forward monitor.
	 * <br>
	 * If the variable doesn't exist or is not declared at function levele scope, no action will be taken.<br>
	 * If the varValue object type is not of the same type of the variable, no action will be taken.<br>
	 * <p>
	 * It's a way to set complex and structured parameters, organized by means of an application object, <br>
	 * with the visibility wished.<br>
	 * <p>
	 * @param varName
	 * @param varValue 
	 * @return the object var value
	 */
	public void  setVarValue(InnerMonitorControBlock mcb, String varName, Object varValue) {
		InnerVar innerVar = null;
		
		innerVar = getVarInnerVar(varName, EnumForwardScope.SCOPE_FUNCTION, mcb);
		if (innerVar == null) {return;}
		
		// Il tipo dell'oggetto valore coincide con quello della variabile: assegnazione nuovo valore
		if (varValue != null && innerVar.varType == varValue.getClass()) {
			innerVar.varObject = varValue;
			return;
		}
		
		// Valore a null: assegnazione nuovo valore
		if (varValue == null) {
			innerVar.varObject = varValue;
			return;
		}
		
		// I tipi NON coincidono, si assegna solo se discendono dalla stessa classe
		// Per esempio per LogicalDataView Objects
		innerVar.varType.getSuperclass();
		if (varValue.getClass().getSuperclass() == innerVar.varType.getSuperclass()) {
			innerVar.varObject = varValue;
		}
		
		return;
	}

	/**
	 * Set a variable value, for the scope declared, in the runtime forward monitor.
	 * <br>
	 * If the variable doesn't exist or is not declared in the input scope, no action will be taken.<br>
	 * <p>
	 * It's a way to set complex and structured parameters, organized by means of an application object, <br>
	 * with the visibility wished.<br>
	 * If the varValue object type is not of the same type of the variable, no action will be taken.<br>
	 * <p>
	 * @param varName
	 * @param scope as a {@link EnumForwardScope} enumeration
	 * @param varValue  
	 * @return the object var value
	 */
	public void  setVarValue(InnerMonitorControBlock mcb, String varName, EnumForwardScope scope, Object varValue) {
		InnerVar innerVar = null;

		innerVar = getVarInnerVar(varName, scope, mcb);
		if (innerVar == null) {return;}
		if (innerVar.varType != varValue.getClass()) {return;}
		innerVar.varObject = varValue;
		return;
	}

	/**
	 * Return the current String value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for function scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @return the String value
	 */
	public String getValueString(String varName) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, EnumForwardScope.SCOPE_FUNCTION);
		if (objVar == null) {return "";}
		if (!(objVar instanceof String)) {return "";}
		return (String)objVar ;
	}

 
	/**
	 * Return the current String value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for function scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @param scope as a {@link EnumForwardScope} enumeration
	 * @return the String value
	 */
	public String getValueString(String varName, EnumForwardScope scope) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, scope);
		if (objVar == null) {return "";}
		if (!(objVar instanceof String)) {return "";}
		return (String)objVar ;
	}

	/**
	 * Return the current int value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for the function scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @return the Integer value
	 */
	public Integer getValueInt(String varName) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, EnumForwardScope.SCOPE_FUNCTION);
		if (objVar == null) {return new Integer(0);}
		if (!(objVar instanceof Integer)) {return new Integer(0);}
		return (Integer)objVar ;
	}

	/**
	 * Return the current int value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for the  scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @param scope as a {@link EnumForwardScope} enumeration
	 * @return the Integer value
	 */
	public Integer getValueInt(String varName, EnumForwardScope scope) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, scope);
		if (objVar == null) {return new Integer(0);}
		if (!(objVar instanceof Integer)) {return new Integer(0);}
		return (Integer)objVar ;
	}

	/**
	 * Return the current boolean value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for the function scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @param scope as a {@link EnumForwardScope} enumeration
	 * @return the Boolean value
	 */
	public Boolean getValueBoolean(String varName) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, EnumForwardScope.SCOPE_FUNCTION);
		if (objVar == null) {return new Boolean(false);}
		if (!(objVar instanceof Boolean)) {return new Boolean(false);}
		return (Boolean)objVar ;
	}

	/**
	 * Return the current boolean value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @param scope as a {@link EnumForwardScope} enumeration
	 * @return the Boolean value
	 */
	public Boolean getValueBoolean(String varName, EnumForwardScope scope) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, scope);
		if (objVar == null) {return new Boolean(false);}
		if (!(objVar instanceof Boolean)) {return new Boolean(false);}
		return (Boolean)objVar ;
	}

	/**
	 * Return the current float value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for the function scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @return the Float value
	 */
	public Float getValueFloat(String varName) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, EnumForwardScope.SCOPE_FUNCTION);
		if (objVar == null) {return new Float(0);}
		if (!(objVar instanceof Float)) {return new Float(0);}
		return (Float)objVar ;
	}

	/**
	 * Return the current float value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @param scope as a {@link EnumForwardScope} enumeration
	 * @return the Float value
	 */
	public Float getValueFloat(String varName, EnumForwardScope scope) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, scope);
		if (objVar == null) {return new Float(0);}
		if (!(objVar instanceof Float)) {return new Float(0);}
		return (Float)objVar ;
	}

	/**
	 * Return the current double value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for the function scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @return the Double value
	 */
	public Double getValueDouble(String varName) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, EnumForwardScope.SCOPE_FUNCTION);
		if (objVar == null) {return new Double(0);}
		if (!(objVar instanceof Double)) {return new Double(0);}
		return (Double)objVar ;
	}

	/**
	 * Return the current double value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @param scope as a {@link EnumForwardScope} enumeration
	 * @return the Double value
	 */
	public Double getValueDouble(String varName, EnumForwardScope scope) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, scope);
		if (objVar == null) {return new Double(0);}
		if (!(objVar instanceof Double)) {return new Double(0);}
		return (Double)objVar ;
	}

	
	/**
	 * Return the current double value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for the function scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @return the Double value
	 */
	public Date getValueDate(String varName) {
		Object objVar = null;
 		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, EnumForwardScope.SCOPE_FUNCTION);
		if (objVar == null) {return new Date();}
		if (!(objVar instanceof Date)) {return new Date();}
		return (Date)objVar ;
	}

	/**
	 * Return the current double value of the variable in the runtime forward monitor.
	 * <br>
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @param scope as a {@link EnumForwardScope} enumeration
	 * @return the Double value
	 */
	public Date getValueDate(String varName, EnumForwardScope scope) {
		Object objVar = null;
		InnerMonitorControBlock mcb = null;
 		mcb = this.al_monitorControlBlock.get(this.mcbIndex);
		objVar = getVarValue(mcb, varName, scope);
		if (!(objVar instanceof Date)) {return null;}
		return (Date)objVar ;
	}

	
	///////////////////////////////////////////////////////////////////////
    // Events manager methods
	///////////////////////////////////////////////////////////////////////

	
	/* ---------------------
     * ActionListener  
     * ---------------------
     * 
     * Metodo attivato da java a fronte di:
     * 
     * click su JButton/JToggleButton  
     * click su JRadioButton (check, l'altro checked viene disabilitato da swing)
     * click su JMenuItem, JCheckBoxMenuItem, JRadioButtonMenuItem in menu
     * click su JTable (recupero riga e colonna)
     * click su JComponent generico (per esempio su JLabel)
     * End delay su oggetto Timer
     */
	public void actionPerformed(ActionEvent e) {

		ForwardMenu menu = null;
 		Object objComponent = null;
		JComponent jcomponent = null;
		InnerComponent innerComponent = null;
		String panelName = "";
		String componentName = "";
		String menuName = null;;
		
		// Recupero oggetto
		this.mcb.system.setEventAction(e);
		objComponent = e.getSource();
		
		// Recupero descrittore componente dichiarato, con oggetto swing, nome e pannello in cui è collocato
		// Se il componente e' un JComponent si considera il nome assegnato all'oggetto
		// Nel caso di JMenuItem implementati con menu di JButton, l'oggetto è un JButton ma il componente dichiarato è JMenuItem
		// Per questo motivivo in caso di JComponent si recupera il nome dall'oggetto
		// Negli altri casi si utilizza il reference all'oggetto (come il Timer)
		if (objComponent instanceof JComponent) {
			jcomponent =  (JComponent) objComponent;
			componentName = jcomponent.getName();
		} else {
			componentName = this.mcb.function.getComponentName(objComponent);			// Individuo il nome dell'oggetto dal suo refernce
		}
		innerComponent = this.mcb.function.getComponentDeclared(componentName);
		if (innerComponent == null) {return;}											// Probabile errore
		panelName = innerComponent.panelName; 
		
		// Update campi di sistema disponibili alle logiche applicative
		this.mcb.system.setActivePanel(panelName);
		this.mcb.system.setEventComponentObject(jcomponent);
		this.mcb.system.setEventComponentType(innerComponent.componentType);
		this.mcb.system.setEventComponentName(componentName);

	    // Evento di Timer (fine tempo di delay)
	    if (objComponent instanceof Timer) {
    		manageLogicDeclaredActivation(this.mcb, componentName, EnumForwardEvent.ON_SYSTEM_TIMER_EXPIRED);
		    return;
	    }

	    // Click su menuItem foglia implementato da JMenuItem o JCheckBoxMenuItem o JRadioButtonMenuItem o JButton
		if (!innerComponent.menuName.equals("")
		&& (objComponent instanceof JMenuItem 
			|| objComponent instanceof JButton
			|| objComponent instanceof JCheckBoxMenuItem 
			|| objComponent instanceof JRadioButtonMenuItem)
		   ) {
			manageLogicDeclaredActivation(this.mcb, componentName, EnumForwardEvent.ON_MENU_ITEM_SELECTED);
		    // Gestione hide submenu aperti
			menuName = innerComponent.menuName;
			menu = this.mcb.function.getMenu(menuName);
			if (menu.getMenuRendering() == EnumForwardOption.MENU_RENDERING_PLAIN_BUTTONS) {	
				panelName = menu.getPanelName();
				ForwardPanelMenu panelMenuObject = menu.getForwardPanelMenu();
				panelMenuObject.hideSubmenusOpen();
			}
			return;
		}
	    

        // Click su JButton.
		// Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
	    if (objComponent instanceof JButton) {
		    manageLogicDeclaredActivation(this.mcb,componentName, EnumForwardEvent.ON_JAVA_ACTION_PERFORMED
		    													, EnumForwardEvent.ON_CLICK);
		    return;
	    }

	    
	    // Click su JRadioButton gestito da ItemListener con itemStateChanged()
	    // Click su JTable       gestito da ListSelectionListener con valueChanged()

	    
	    
	    // Click su JComponent generico.
	    // Per esempio una label o un pulsante.
		// Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
	    if (objComponent instanceof JComponent) {
		    manageLogicDeclaredActivation(this.mcb, componentName, EnumForwardEvent.ON_JAVA_ACTION_PERFORMED
													             , EnumForwardEvent.ON_CLICK);
		    return;
	    }

	    
 	}

 	  
	/* ---------------------
     * AdjustmentListener  
     * ---------------------
     * 
     * Metodo attivato da java a fronte di click o movimento con rotella mouse di JScrollBar
     * 
     */	
	public void adjustmentValueChanged(AdjustmentEvent e) {

		Object objComponent = null;
		JComponent jcomponent = null;
		ForwardPanelComponent panelComponent = null;
		@SuppressWarnings("unused")
		String panelName = "";
		String panelComponentName = "";
		
		// Valore di aggiustamento: nessuna operazione
		if (e.getValueIsAdjusting()) {
			return;
		}
		
		this.mcb.system.setEventAdjustment(e);
		objComponent = e.getSource();
		
		// Recupero oggetto swing, nome e pannello in cui è collocato
		jcomponent =  (JComponent) objComponent;
		panelComponentName = jcomponent.getName();
		panelComponent = this.mcb.function.getPanelComponent(panelComponentName);
		panelName = this.mcb.function.getPanelOwner(panelComponentName);  
		this.mcb.system.setEventComponentObject(jcomponent);
		this.mcb.system.setEventComponentType(panelComponent.getType());
		this.mcb.system.setEventComponentName(panelComponentName);
		this.mcb.system.setEventAdjustmentType(e.getAdjustmentType());
		this.mcb.system.setEventAdjustmentValue(e.getValue());

		
		// Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
		manageLogicDeclaredActivation(this.mcb, panelComponentName, EnumForwardEvent.ON_JAVA_ADJUSTMENT_VALUE_CHANGED);
	     
	}
	
	/* ---------------------
     * FocusListener  
     * ---------------------
     * 
     * Metodo attivato da java a fronte di:
     * 
     * A fronte di got focus su qualsiasi componente
     *  
     */
	public void focusGained(FocusEvent e) {
		focusGainedLost(e, EnumForwardEvent.ON_JAVA_FOCUS_GAINED);
	}
	
	
	/* ---------------------
     * FocusListener  
     * ---------------------
     * 
     * Metodo attivato da java a fronte di:
     * 
     * A fronte di lost focus su qualsiasi componente
     *  
     */
	public void focusLost(FocusEvent e) {
		focusGainedLost(e, EnumForwardEvent.ON_JAVA_FOCUS_LOST);
	}

	/*
	 * Gestione comune a focus got e lost
	 */
	private void focusGainedLost(FocusEvent e, EnumForwardEvent forwardEvent) {
		
		Object objComponent = null;
		JComponent jcomponent = null;
		Window window = null;
		ForwardPanelComponent panelComponent = null;
		String panelName = "";
		String panelComponentName = "";
		
		this.mcb.system.setEventFocus(e);
		objComponent = e.getSource();
		
		// JPanel, JSplitPanel, JOptionPane, JLabel, JTextField, ...
		if (objComponent instanceof JComponent) { 
			jcomponent =  (JComponent) objComponent;
			panelComponentName = jcomponent.getName();
			panelComponent = this.mcb.function.getPanelComponent(panelComponentName);
			panelName = this.mcb.function.getPanelOwner(panelComponentName); 
			this.mcb.system.setEventComponentObject(jcomponent);
			this.mcb.system.setEventComponentType(panelComponent.getType());
			this.mcb.system.setEventComponentName(panelComponentName);
		} 
		
		// JFrame, JWindow, JDialog
		if (objComponent instanceof Window) { 
			window = (Window) objComponent;
			panelName = window.getName();
			panelComponentName = "";
			this.mcb.function.getComponentType(panelName);
			this.mcb.system.setEventComponentObject(objComponent);
			this.mcb.system.setEventComponentType(this.mcb.function.getComponentType(panelName));
			this.mcb.system.setEventComponentName(panelName);
		}

		// Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
	    manageLogicDeclaredActivation(this.mcb, panelComponentName, forwardEvent);
	}
	
	
	/* ------------
	 * ItemListener 
	 * ------------
	 * 
	 * A fronte di:
	 *  
	 *  JCheckBox checked/unchecked
	 *  JRadioButton acceso/spento
 	 *  JToggleButton toggled/untoggled
     *  JComboBox A fronte di click di selezione di un item
     *  JComboBox A fronte di freccia su/giu/sinistra/destra/first di selezione di un item
	 * 
	 * 
	 */
	
	@SuppressWarnings("rawtypes")
	public void itemStateChanged(ItemEvent e) {
		
		Object objComponent = null;
		JComponent jcomponent = null;
		JComboBox jcomboBox = null;
		ForwardPanelComponent panelComponent = null;
		ForwardComboBoxModel comboBoxModel = null;
		EnumForwardEvent ar_eventToSearch[] = null;
		InnerOnEvent eventStateChanged1 = null;
		InnerOnEvent eventStateChanged2 = null;
		@SuppressWarnings("unused")
		String panelName = "";
		String panelComponentName = "";
		int comboBoxSelectedIndex = 0;
		
		this.mcb.system.setEventItem(e);
		objComponent = e.getSource();
		
		// Recupero oggetto swing, nome e pannello in cui è collocato
		jcomponent =  (JComponent) objComponent;
		panelComponentName = jcomponent.getName();
		panelComponent = this.mcb.function.getPanelComponent(panelComponentName);
		if (panelComponent == null) {return;}											// Evento attivato su componente di Funzione (JDialog) GIA' rilasciata
		panelName = this.mcb.function.getPanelOwner(panelComponentName);    
		this.mcb.system.setEventComponentObject(jcomponent);
		this.mcb.system.setEventComponentType(panelComponent.getType());
		this.mcb.system.setEventComponentName(panelComponentName);

		
		////////////////////////////////////////////////
        // Gestine Selezionano/Deselezionano JCheckBox
		////////////////////////////////////////////////
		
	    if (objComponent instanceof JCheckBox) {
		   if (e.getStateChange() == ItemEvent.DESELECTED){
			   ar_eventToSearch = new EnumForwardEvent[]{EnumForwardEvent.ON_JAVA_ITEM_STATE_CHANGED, EnumForwardEvent.ON_CHECKBOX_UNCHECKED};
		   } else {
			   ar_eventToSearch = new EnumForwardEvent[]{EnumForwardEvent.ON_JAVA_ITEM_STATE_CHANGED, EnumForwardEvent.ON_CHECKBOX_CHECKED};
		   }
		   // Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
		   manageLogicDeclaredActivation(this.mcb, panelComponentName, ar_eventToSearch);
		   return;
	    }

		///////////////////////////////////////////////////
        // Gestione Selezionano/Deselezionano JRadioButton
		///////////////////////////////////////////////////
		
	    if (objComponent instanceof JRadioButton) {
		   if (e.getStateChange() == ItemEvent.DESELECTED){
			   ar_eventToSearch = new EnumForwardEvent[]{EnumForwardEvent.ON_JAVA_ITEM_STATE_CHANGED, EnumForwardEvent.ON_RADIOBUTTON_UNCHECKED};
		   } else {
			   ar_eventToSearch = new EnumForwardEvent[]{EnumForwardEvent.ON_JAVA_ITEM_STATE_CHANGED, EnumForwardEvent.ON_RADIOBUTTON_CHECKED};
		   }
		   // Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
		   manageLogicDeclaredActivation(this.mcb, panelComponentName, ar_eventToSearch);
		   return;
	    }

		///////////////////////////////////////////////////
        // Gestione Selezionano/Deselezionano JToggleButton
		///////////////////////////////////////////////////
		
	    if (objComponent instanceof JToggleButton) {
		   if (e.getStateChange() == ItemEvent.DESELECTED){
			   ar_eventToSearch = new EnumForwardEvent[]{EnumForwardEvent.ON_JAVA_ITEM_STATE_CHANGED, EnumForwardEvent.ON_BUTTON_UNTOGGLED};
		   } else {
			   ar_eventToSearch = new EnumForwardEvent[]{EnumForwardEvent.ON_JAVA_ITEM_STATE_CHANGED, EnumForwardEvent.ON_BUTTON_TOGGLED};
		   }
		   // Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
		   manageLogicDeclaredActivation(this.mcb, panelComponentName, ar_eventToSearch);
		   return;
	    }

	    
		//////////////////////////////////////
        // Gestione selezione item comboBox
		//////////////////////////////////////
	    
	    if (objComponent instanceof JComboBox) {
	    	jcomboBox = (JComboBox) objComponent;
		    this.mcb.system.setComboBoxName(panelComponentName);

		    // Item NON selezionata, probabilmente a fronte di remove sulla JList
	    	comboBoxSelectedIndex = jcomboBox.getSelectedIndex();
		    if (comboBoxSelectedIndex == -1) {
			   this.mcb.system.setComboBoxBoundObject(null);
			   this.mcb.system.setComboBoxSelectedItem("");
	           return;		
		   }
		   
		   // Java fire il listener sullo stesso item anche per la deselezione: si considera solo la selezione
		   if (e.getStateChange() == ItemEvent.DESELECTED){return;}

		    // Recupero modello forward per la lista (recuperabile anche con jcomboBox.getModel)
		   comboBoxModel = panelComponent.getComboBoxModel();
		   
		   // Indice item selezionato/deselezionato, valore text e object binded
		   this.mcb.system.setComboBoxSelectedItem((String) jcomboBox.getSelectedItem());
		   this.mcb.system.setComboBoxBoundObject(comboBoxModel._getDataBound(comboBoxSelectedIndex));					
	       this.mcb.system.setComboBoxSelectedIndex(comboBoxSelectedIndex);
		   ar_eventToSearch = new EnumForwardEvent[]{EnumForwardEvent.ON_JAVA_ITEM_STATE_CHANGED, EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM};

		   // Se l'evento è avvenuto durante il popolamento della comboBox le eventuali azioni vengono mascherate
		   // L'eventuale esecuzione è a cura del codice di gestione del popolamento della comboBox, a fine popolamento
		   if (this.mcb.system.isComboBoxEventsMasked()) {
			   eventStateChanged1 = getEventDescriptor(this.mcb, panelComponentName, EnumForwardEvent.ON_JAVA_ITEM_STATE_CHANGED);
			   eventStateChanged2 = getEventDescriptor(this.mcb, panelComponentName, EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM);
			   if (eventStateChanged1 != null) {this.mcb.system.getComboBoxEventsMasked().add(eventStateChanged1);}
			   if (eventStateChanged2 != null) {this.mcb.system.getComboBoxEventsMasked().add(eventStateChanged2);}
			   return;
		   }
		   
	       // Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
		   manageLogicDeclaredActivation(this.mcb, panelComponentName, ar_eventToSearch);
		   return;
	    }

	}
	
	/* ----------------
	 * MouseListener 
	 * ----------------
	 *
	 * A fronte di:
	 * 
	 * DoubleClick su qualsiasi componente
	 *   Click è già intercettato con ActionPerformed con estrazione delle informazioni correnti
	 *   
	 */
	public void mouseClicked(MouseEvent e) {
		
		Object objComponent = null;
		JComponent jcomponent = null;
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelComponent = null;
		JTable jtable = null;
		@SuppressWarnings("unused")
		String panelName = "";
		String componentName = "";

		this.mcb.system.setEventMouse(e);
		objComponent = e.getSource();

		// Recupero oggetto swing, nome e pannello in cui è collocato
		jcomponent =  (JComponent) objComponent;
		componentName = jcomponent.getName();
		innerComponent = this.mcb.function.getComponentDeclared(componentName);
		if (innerComponent == null) {return;}					// Evento su funzione errata o componente non definito o errore interno
		panelName = this.mcb.function.getPanelOwner(componentName);    
		this.mcb.system.setEventComponentObject(jcomponent);
		this.mcb.system.setEventComponentType(innerComponent.componentType);
		this.mcb.system.setEventComponentName(componentName);

		
	    // Click o DoubleClick su JTable.
		// Recupero numero riga e colonna selezionata.
	    // Valorizzazione in system, per evento su jtable
	    if (objComponent instanceof JTable) {
	    	jtable = (JTable) objComponent;
	    	this.mcb.system.setTableSelectedRow(jtable.getSelectedRow());
	    	this.mcb.system.setTableSelectedRowCount(jtable.getSelectedRowCount());
	    	this.mcb.system.setTableSelectedRows(jtable.getSelectedRows());
	    	this.mcb.system.setTableSelectedColumn(jtable.getSelectedColumn());
	    	this.mcb.system.setTableSelectedColumnCount(jtable.getSelectedColumnCount());
	    	this.mcb.system.setTableSelectedColumns(jtable.getSelectedColumns());
	    }


	    // Attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
	   

		// Click, attivazione logiche eventuali
		if (e.getClickCount() == 1) {
			manageLogicDeclaredActivation(this.mcb, componentName, EnumForwardEvent.ON_CLICK);
			return;
		}
		// DoubleClick, attivazione logiche eventuali
		if (e.getClickCount() == 2) {
			manageLogicDeclaredActivation(this.mcb, componentName, EnumForwardEvent.ON_DOUBLE_CLICK);
			panelComponent = this.mcb.function.getPanelComponent(componentName);
			if (panelComponent == null) {return;}
			// Attivazione funzione di lookup rule table
			if (panelComponent.getLookupTableNum() >= 0) {
				manageStartLookupRuleTable(panelComponent);
				return;
			}
			// Attivazione funzione di lookup function
			if (!panelComponent.getLookupFunction().equals("")) {
				manageStartLookupFunction(panelComponent);
				return;
			}
			return;
		}
		
		// Click su item di menu (button o JMenuItem)
		if (e.getClickCount() == 1 
		&& !innerComponent.menuName.equals("")
		&& innerComponent.componentType == EnumForwardComponent.JMenuItem) {
			manageLogicDeclaredActivation(this.mcb, componentName, EnumForwardEvent.ON_MENU_ITEM_SELECTED);
			return;
		}
		
		// Click su nodo di un JTree
		if (e.getClickCount() == 1 
		&& innerComponent.componentType == EnumForwardComponent.JTree) {
			// TODO
			return;
		}
	    
	}
	
	/*
	 * Gestione attivazione funzione di looKup rule table
	 * Vengono effettuate automaticamente tutte le operazioni normalmente effettuate manualmente
	 */
	private void manageStartLookupRuleTable(ForwardPanelComponent panelComponent) {
		
		ForwardDoParms doFunctionLoad = null;
		ForwardDoParms doFunctionSetParmRequired1 = null;
		ForwardDoParms doFunctionSetParmRequired2 = null;
		ForwardDoParms doFunctionStart = null;
		
		// Creazione actions come se fossero dichiarate nella funzione
		doFunctionLoad = this.mcb.function.DO_FUNCTION_LOAD(FUNCTION_LOOKUP_RULE_TABLE);
		doFunctionSetParmRequired1 = this.mcb.function.DO_FUNCTION_SET_PARM_REQUIRED("FunctionLookUpRuleTable", panelComponent.getLookupTableNum(), "numTable", null);
		doFunctionSetParmRequired2 = this.mcb.function.DO_FUNCTION_SET_PARM_REQUIRED("FunctionLookUpRuleTable",this.mcb.function.getLanguage().ordinal(), "language", null);
		doFunctionStart = this.mcb.function.DO_FUNCTION_START(FUNCTION_LOOKUP_RULE_TABLE, panelComponent.getName(), true);

	    // Esecuzione azioni
		execActionForFunction(this.mcb, doFunctionLoad, false);
		this.functionLoaded.setFunctionLookup(true);
		execActionForFunction(this.mcb, doFunctionSetParmRequired1, false);
		execActionForFunction(this.mcb, doFunctionSetParmRequired2, false);
        manageLogicDeclaredActivation(this.mcb, panelComponent.getName(), EnumForwardEvent.ON_FUNCTION_BEFORE_START);	// Eventuali logiche utente 
        this.mcb.isLookupFunctionAutomaticParms = true;						// Alla return valorizza automaticamente le variabili nella funzione chiamante
		execActionForFunction(this.mcb, doFunctionStart, false);
	}
 
	/*
	 * Gestione attivazione funzione di lookUp funzione
	 * Vengono effettuate automaticamente tutte le operazioni normalmente effettuate manualmente
	 */
	private void manageStartLookupFunction(ForwardPanelComponent panelComponent) {
		Scanner scn = null;
		ArrayList<String> al_parmRequired = null;
		ForwardDoParms doFunctionLoad = null;
		ForwardDoParms doFunctionSetParmRequired = null;
		ForwardDoParms doFunctionStart = null;
		String parmRequiredCaller = "";					// Nome componente in funzione chiamante (corrente)
		String parmRequiredCalled = "";					// Nome parametro in funzione da chiamare
		String functionLookup = "";
		
		// Esecuzione caricamento funzione 
		functionLookup = panelComponent.getLookupFunction();
		doFunctionLoad = this.mcb.function.DO_FUNCTION_LOAD(functionLookup);
		execActionForFunction(this.mcb, doFunctionLoad, false);
        this.functionLoaded.setFunctionLookup(true);
        
		// Esecuzione caricamento parametri nella funzione di lookup chiamata
		al_parmRequired = panelComponent.getLookupFunctionParmsRequired();
		for (String parmRequired : al_parmRequired) {
			scn = new Scanner(parmRequired);
			parmRequiredCaller = scn.next();
			if (scn.hasNext()) {
				parmRequiredCalled = scn.next();
				doFunctionSetParmRequired = this.mcb.function.DO_FUNCTION_SET_PARM_REQUIRED(functionLookup, parmRequiredCaller, parmRequiredCalled);
				execActionForFunction(this.mcb, doFunctionSetParmRequired, false);
			}
		}
		
		// Esecuzione attivazione funzione
        manageLogicDeclaredActivation(this.mcb, panelComponent.getName(), EnumForwardEvent.ON_FUNCTION_BEFORE_START);	// Eventuali logiche utente 
		doFunctionStart = this.mcb.function.DO_FUNCTION_START(functionLookup, panelComponent.getName());
	    this.mcb.isLookupFunctionAutomaticParms = true;						// Alla return valorizza automaticamente le variabili nella funzione chiamante
		execActionForFunction(this.mcb, doFunctionStart, false);
	}



	/* -----------------
	 * MouseListener 
	 * -----------------
	 * 
	 * A fronte di ingresso mouse su qualsiasi componente
	 * A fronte di uscita mouse su qualsiasi componente
	 * A fronte di mouse premuto (Click tenuto premuto) su qualsiasi componente
	 * A fronte di mouse rilasciato (Click rilasciato) su qualsiasi componente
	 */
	 public void mouseEntered(MouseEvent e) {mouseEventManagement(e, EnumForwardEvent.ON_JAVA_MOUSE_ENTERED, false);} 	// A fronte di ingresso mouse su qualsiasi componente
	 public void mouseExited (MouseEvent e) {mouseEventManagement(e, EnumForwardEvent.ON_JAVA_MOUSE_EXITED, false);} 	// A fronte di uscita mouse su qualsiasi componente
	 public void mousePressed(MouseEvent e) {mouseEventManagement(e, EnumForwardEvent.ON_JAVA_MOUSE_PRESSED, false);} 	// A fronte di mouse premuto (Click tenuto premuto) su qualsiasi componente
	 public void mouseReleased(MouseEvent e){mouseEventManagement(e, EnumForwardEvent.ON_JAVA_MOUSE_RELEASED, false);}	// A fronte di mouse rilasciato (Click rilasciato) su qualsiasi componente
	
	 
	/* --------------------
	 * MouseMotionListener 
	 * --------------------
	 * 
	 * A fronte di movimento mouse su un qualsiasi componente
	 * A fronte di mouse cliccato e con successivo spostamento fino a successivo rilascio (anche fuori dal componente)
	 *  
	 */
	 public void mouseMoved(MouseEvent e)   {mouseEventManagement(e, EnumForwardEvent.ON_JAVA_MOUSE_MOVED, true);} 	 // A fronte di movimento mouse su qualsiasi componente
	 public void mouseDragged(MouseEvent e) {mouseEventManagement(e, EnumForwardEvent.ON_JAVA_MOUSE_DRAGGED, true);} //  A fronte di mouse cliccato e spostamento
	
	
	/* -------------------------
	 * MouseWheelMotionListener
	 * -------------------------
	 * 
	 * A fronte di rotazione dell rotella del mouse.
	 * Una unità di rotazione è equivalente a un click.
	 * Il segno del numero di rotazioni indica la direzione
	 */
	public void mouseWheelMoved(MouseWheelEvent e) {
		Object objComponent = null;
		JComponent jcomponent = null;
		ForwardPanelComponent panelComponent = null;
		@SuppressWarnings("unused")
		String panelName = "";
		String panelComponentName = "";
		
		this.mcb.system.setEventMouseWheelMotion(e);
		objComponent = e.getSource();

		// Recupero oggetto swing, nome e pannello in cui è collocato
		jcomponent =  (JComponent) objComponent;
		panelComponentName = jcomponent.getName();
		panelComponent = this.mcb.function.getPanelComponent(panelComponentName);
		panelName = this.mcb.function.getPanelOwner(panelComponentName);    
		this.mcb.system.setEventComponentObject(jcomponent);
		this.mcb.system.setEventComponentType(panelComponent.getType());
		this.mcb.system.setEventComponentName(panelComponentName);
		this.mcb.system.setEventPoint(e.getPoint());
		this.mcb.system.setEventX(e.getX());
		this.mcb.system.setEventX(e.getY());
		this.mcb.system.setEventWheelRotations(e.getWheelRotation());

	    // Attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
	    manageLogicDeclaredActivation(this.mcb, panelComponentName, EnumForwardEvent.ON_JAVA_WHEEL_MOVED, EnumForwardEvent.ON_CLICK);
	}

	/*
	 * Gestione eventi di mouse intercettati da Mouse Listener/MouseMotionListener
	 */
	private void mouseEventManagement(MouseEvent e, EnumForwardEvent onJavaMouseEvent, boolean isMouseMotionListener) {
		Object objComponent = null;
		JComponent jcomponent = null;
		ForwardPanelComponent panelComponent = null;
		@SuppressWarnings("unused")
		String panelName = "";
		String panelComponentName = "";
		
		if (isMouseMotionListener) {
			this.mcb.system.setEventMouseMotion(e);
		} else {
			this.mcb.system.setEventMouse(e);
		}
		objComponent = e.getSource();

		
		// Recupero oggetto swing, nome e pannello in cui è collocato
		jcomponent =  (JComponent) objComponent;
		panelComponentName = jcomponent.getName();
		if (!(objComponent instanceof JPanel)
		&&  !(objComponent instanceof JTabbedPane
		&&  !(objComponent instanceof JDialog))
		&&  !(objComponent instanceof JMenuItem)) {
			panelComponent = this.mcb.function.getPanelComponent(panelComponentName);
			panelName = this.mcb.function.getPanelOwner(panelComponentName);    
		}
		// Componente NON su panel (probabilmente su un menu)
		if (panelComponent != null) {
			this.mcb.system.setEventComponentType(panelComponent.getType());
		}
		this.mcb.system.setEventComponentObject(jcomponent);
		this.mcb.system.setEventComponentName(panelComponentName);
		this.mcb.system.setEventPoint(e.getPoint());
		this.mcb.system.setEventX(e.getX());
		this.mcb.system.setEventX(e.getY());
 
		// Gestione attivazione popUpMenu
		if (onJavaMouseEvent == EnumForwardEvent.ON_JAVA_MOUSE_PRESSED
		||  onJavaMouseEvent == EnumForwardEvent.ON_JAVA_MOUSE_RELEASED) {
			showPopUpMenuManagement(e);
		}
		
		
	    // Attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
	    manageLogicDeclaredActivation(this.mcb, panelComponentName, onJavaMouseEvent);
	}

	/* ---------------------------------
	 * Gestione attivazione popUpMenu
	 * ---------------------------------
	 */
	private void showPopUpMenuManagement(MouseEvent e) {
		
		JPopupMenu jpopUpMenu = null;
		InnerComponent innerComponent = null;
		ForwardMenu forwardMenu = null;
		
		// Verifico se il componente ha un PopUpMenu associato
		innerComponent = this.mcb.function.getComponentDeclared(this.mcb.system.getEventComponentName());
		if (innerComponent == null) {return;}
		if (innerComponent.popUpMenuName.equals("")) {return;}
		
		// Recupero l'oggetto JPopupMenu
		forwardMenu = this.mcb.function.getMenu(innerComponent.popUpMenuName);
		if (forwardMenu == null) {return;}
		jpopUpMenu = (JPopupMenu) forwardMenu.getJPopupMenu();
		
		// Attivazione PopupMenu
        if (e.isPopupTrigger()) {
        	jpopUpMenu.show(e.getComponent(), e.getX(), e.getY());
        }
	}


	/* -------------
	 * KeyListener 
	 * -------------
	 *  
	 *  A fronte di:
	 *  
	 *  Key di tastiera premuto
	 *  Key di tastiera rilasciato
	 *  Key di tastiera digitato
	 */
	public void keyPressed(KeyEvent e) 	{keyEventManagement(e, EnumForwardEvent.ON_JAVA_KEY_PRESSED);}
	public void keyReleased(KeyEvent e) {keyEventManagement(e, EnumForwardEvent.ON_JAVA_KEY_RELEASED);}
	public void keyTyped(KeyEvent e) 	{keyEventManagement(e, EnumForwardEvent.ON_JAVA_KEY_TYPED);}
	
	/*
	 * Gestione generalizzata eventi di key intercettati da KeyListener
	 */
	private void keyEventManagement(KeyEvent e, EnumForwardEvent onJavaKeyEvent) {
		
		Object objComponent = null;
		JComponent jcomponent = null;
		ForwardPanelComponent panelComponent = null;
		@SuppressWarnings("unused")
		String panelName = "";
		String panelComponentName = "";

		// Maschere per individuazione modifiers al tasto premuto
	    int modifiersExtended = 0;
	 	int SHIFT = 4;
	    int CTRL = 128;
	    int ALT = 512;
	    int ALT_GRAPH = CTRL | ALT;
	    
        // Recupero oggetto source
		this.mcb.system.setEventKey(e);
		objComponent = e.getSource();

		// Recupero oggetto swing, nome e pannello in cui è collocato
		jcomponent =  (JComponent) objComponent;
		panelComponentName = jcomponent.getName();
		panelComponent = this.mcb.function.getPanelComponent(panelComponentName);
		panelName = this.mcb.function.getPanelOwner(panelComponentName);    
		this.mcb.system.setEventComponentObject(jcomponent);
		this.mcb.system.setEventComponentType(panelComponent.getType());
		this.mcb.system.setEventComponentName(panelComponentName);
        this.mcb.system.setEventKeyCode(e.getKeyCode());
        this.mcb.system.setEventKeyChar(e.getKeyChar());
		
        // Carattere typed/pressed/released o code (per esempio Shift)
        if (e.getID() == KeyEvent.KEY_TYPED) {
        	this.mcb.system.setEventKeyChar(e.getKeyChar());
        	this.mcb.system.setEventKeyCode(-1);
        } else {
        	 this.mcb.system.setEventKeyCode(e.getKeyCode());
        	 this.mcb.system.setEventKeyChar(' ');
        	 this.mcb.system.setEventKeyText( KeyEvent.getKeyText(e.getKeyCode()));
         }
        
        // Mask key modifiers come shift, Ctrl, Alt e relativo testo come Ctrl+Shift+Alt
        this.mcb.system.setEventKeyModifiersMask(e.getModifiersEx());
        this.mcb.system.setEventKeyModifiersText(e.getModifiersEx());
        
        // Indicazione di action key come F1, Enter etc
        if (e.isActionKey()) {
        	this.mcb.system.setEventKeyIsActionKey(true);
        } else {
        	this.mcb.system.setEventKeyIsActionKey(false);
        }
        
        // Locazione della key nel tasto della tastiera
        this.mcb.system.setEventKeyLocation(e.getKeyLocation());
        if (e.getKeyLocation() == KeyEvent.KEY_LOCATION_STANDARD) {
        	 	this.mcb.system.setEventKeyLocationString("standard");
          } else if (e.getKeyLocation() == KeyEvent.KEY_LOCATION_LEFT) {
        	  	this.mcb.system.setEventKeyLocationString("left");
        } else if (e.getKeyLocation() == KeyEvent.KEY_LOCATION_RIGHT) {
        		this.mcb.system.setEventKeyLocationString("right");
        } else if (e.getKeyLocation() == KeyEvent.KEY_LOCATION_NUMPAD) {
        		this.mcb.system.setEventKeyLocationString("numpad");
        } else { // (location == KeyEvent.KEY_LOCATION_UNKNOWN)
        		this.mcb.system.setEventKeyLocationString("unknow");
        }
        
        // Impostazione di ALT, SHIFT, CTRL, ALT_GRAPH contestuali
        this.mcb.system.setEventKeyShift(false);
        this.mcb.system.setEventKeyAlt(false);
        this.mcb.system.setEventKeyCtrl(false);
        this.mcb.system.setEventKeyAltGraph(false);
        modifiersExtended = e.getModifiersEx();
        if ((modifiersExtended & SHIFT) 	== SHIFT) 		{this.mcb.system.setEventKeyShift(true);}
        if ((modifiersExtended & ALT)   	== ALT)     	{this.mcb.system.setEventKeyAlt(true);}
        if ((modifiersExtended & CTRL)  	== CTRL) 		{this.mcb.system.setEventKeyCtrl(true);}
        if ((modifiersExtended & ALT_GRAPH) == ALT_GRAPH) 	{this.mcb.system.setEventKeyAltGraph(true);}
        
 	    // Attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
	    manageLogicDeclaredActivation(this.mcb, panelComponentName, onJavaKeyEvent);

	}

	
	/* ---------------
	 * ChangeListener 
	 * ---------------
	 *  
	 *  A fronte di:
	 *  
	 *  Variazione tick in JSlider
	 *  Variazione tick in JSpinner
	 *  Selezione tab in JTabbedPane
	 */
	public void stateChanged(ChangeEvent e) {
		
		JComponent jcomponent = null;
		JSlider jslider = null;
		JSpinner jspinner = null;
		JFormattedTextField formattedTextField = null;
		JTabbedPane tabbedPane = null;
		JPanel panel = null;
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelComponent = null;
		String panelName = "";
		String panelComponentName = "";
		String spinnerValueString = "";
		Integer spinnerValueNum = null;
		Date spinnerValueDate = null;
		int sliderValue = 0;
		
		// Valore di aggiustamento: nessuna operazione
		this.mcb.system.setEventChange(e);
		
		// JSlider
		if (e.getSource() instanceof JSlider) {
			jslider = (JSlider) e.getSource();
			if (jslider.getValueIsAdjusting()) {
				return;
			}
			sliderValue = (int)jslider.getValue();
			this.mcb.system.setEventSliderChangedValue(sliderValue);
			jcomponent = jslider;
			// Recupero nome e pannello in cui è collocato jslider 
			panelComponentName = jcomponent.getName();
			panelComponent = this.mcb.function.getPanelComponent(panelComponentName);
			panelName = this.mcb.function.getPanelOwner(panelComponentName);  
			this.mcb.system.setEventComponentObject(jcomponent);
			this.mcb.system.setEventComponentType(panelComponent.getType());
			this.mcb.system.setEventComponentName(panelComponentName);
		}
        
		// JSPinner
		if (e.getSource() instanceof JSpinner) {
			jspinner = (JSpinner) e.getSource();
			jcomponent = jspinner;
			// JSpinner di testi
			if (jspinner.getValue() instanceof String) {
				spinnerValueString = (String)jspinner.getValue();
				this.mcb.system.setEventSpinnerChangedValueText(spinnerValueString);
			} else if (jspinner.getValue() instanceof Integer) {
				spinnerValueNum = (Integer)jspinner.getValue();
				spinnerValueString = spinnerValueNum + "";
				this.mcb.system.setEventSpinnerChangedValueText(spinnerValueString);
				this.mcb.system.setEventSpinnerChangedValueNum(spinnerValueNum.intValue());
			} else if (jspinner.getValue() instanceof Date) {
				spinnerValueDate = (Date)jspinner.getValue();
				DateEditor dateEditor = (DateEditor) jspinner.getEditor();  
				spinnerValueString = dateEditor.getTextField().getText();
				// Il valore di dateEditor NON è ancora stato aggiornato, mentre il valore dello spinner si, quindi si riformatta quello con maschera di default
				// Se l'applicazione ha bisogno di yna data formattata diversamente, ha a disposizione l'ogetto Date() da manipolare
				try {
					formattedTextField = new JFormattedTextField(spinnerValueDate);
					spinnerValueString = formattedTextField.getFormatter().valueToString(spinnerValueDate);
				} catch (ParseException e1) {
				}
				this.mcb.system.setEventSpinnerChangedValueText(spinnerValueString);
				this.mcb.system.setEventSpinnerChangedValueDate(spinnerValueDate);
			}
			// Recupero nome e pannello in cui è collocato jspinner
			panelComponentName = jcomponent.getName();
			panelComponent = this.mcb.function.getPanelComponent(panelComponentName);
			panelName = this.mcb.function.getPanelOwner(panelComponentName);  
			this.mcb.system.setEventComponentObject(jcomponent);
			this.mcb.system.setEventComponentType(panelComponent.getType());
			this.mcb.system.setEventComponentName(panelComponentName);
		}
        
		// JTabbedPane
		if (e.getSource() instanceof JTabbedPane) {
			tabbedPane = (JTabbedPane) e.getSource();
			panel = (JPanel) tabbedPane.getSelectedComponent();
			panelName = panel.getName();
			this.mcb.system.setEventComponentObject(jcomponent);
			this.mcb.system.setEventComponentType(EnumForwardComponent.JTabbedPane);
			this.mcb.system.setEventComponentName(tabbedPane.getName());
			this.mcb.system.setTabbedPaneTicked(panelName);
		}

		// Gestione attivazione logiche applicative dichiarate per l'evento, se jslider o jspinner 
		if (e.getSource() instanceof JSlider || e.getSource() instanceof JSpinner) {
			manageLogicDeclaredActivation(this.mcb, panelComponentName, EnumForwardEvent.ON_JAVA_STATE_CHANGED
											                		  , EnumForwardEvent.ON_SLIDER_TICKED 
											                		  , EnumForwardEvent.ON_SPINNER_TICKED);
			return;
		}
		
		// Gestione attivazione logiche applicative dichiarate per l'evento, se jtabbedPane
		if (e.getSource() instanceof JTabbedPane) {
			manageLogicDeclaredActivation(this.mcb, panelName, EnumForwardEvent.ON_TABBED_PANE_TICKED);
			innerComponent = this.mcb.function.getComponentDeclared(panelName);
			// Attivazione logiche prima della prima visualizzazione
			if (!innerComponent.isPaneFirstShowDone) {
				manageLogicDeclaredActivation(this.mcb, panelName, EnumForwardEvent.ON_PANEL_BEFORE_FIRST_SHOW);
				innerComponent.isPaneFirstShowDone = true;
			}
		}
	}

	/* -----------------------
	 * PropertyChangeListener
	 * -----------------------
	 * 
	 * A fronte di variazione property di qualsiasi oggetto:
	 * 
	 * Click su pulsanti di opzione di JDialog, property VALUE 
	 * 
	 */
	public void propertyChange(PropertyChangeEvent e) {
		JDialog jdialog = null;
		JOptionPane dialogOptionPanel = null;
		String property = "";
		int dialogOptionChoosed = 0; 
		
		// Gestione chiusura di JDialog modale attivata con JOptionPane standard
		// Viene generato questo evento a fronte di push buttons OK, CANCEL, etc o X in titleBar
		// Si gestisce l'attivazione delle logiche prima della chiusura
		// Si effettua la chiusura del dialogo rendendolo invisibile.
        property = e.getPropertyName();
        if (this.mcb.system.isDialogActive() 
        && this.mcb.system.isDialogModal()
        && JOptionPane.VALUE_PROPERTY.equals(property)
        && e.getSource() instanceof JOptionPane) {
        	jdialog = this.mcb.system.getDialogActive();
        	
        	// Attivazione logiche prima di chiusura e oscuramento dialogo 
        	if (jdialog.isVisible() && e.getSource() instanceof JOptionPane) {
                dialogOptionPanel =  (JOptionPane) e.getSource();
                
                // Tentativo di chiusura con X su title bar, nessuna chiusura.
                // L'evento viene intercettato da WindowsClosing e li viene attivata la logica applicativa eventuale
                if (dialogOptionPanel.getValue() ==  JOptionPane.UNINITIALIZED_VALUE ||  dialogOptionPanel.getValue() ==  null) {
                 	return;
				}
                dialogOptionChoosed = (Integer) dialogOptionPanel.getValue();
                this.mcb.system.setDialogOptionChoosed(dialogOptionChoosed);
                
                // Valore selezionato da JTextField o JComboBox in caso dialogo standard di input
                if (dialogOptionPanel.getInputValue() != null) {
                	this.mcb.system.setDialogInputValue((String) dialogOptionPanel.getInputValue());
        		}
                // Logiche prima e dopo la chiusura
          		manageLogicDeclaredActivation(this.mcb, jdialog.getName(), EnumForwardEvent.ON_DIALOG_BEFORE_CLOSING); // Eventuali logiche da attivare prima della chiusura del pannello di opzione
                this.mcb.system.setDialogActive(false);
        		jdialog.setVisible(false);
          		execActionForStartDialogLogicActivation(this.mcb);  													 // Logiche da attivare a fronte del pulsante di opzione selezionato
			}
		}
 	}



    /* ---------------------
     * ListSelectionListener  
     * ---------------------
     * 
     * Metodo attivato da java a fronte di:
     * 
     * JList A fronte selezione di una o + righe
     * JTable a fronte di selezione di una o più righe e/o colonne
     * 
     */
	@SuppressWarnings("rawtypes")
	public void valueChanged(ListSelectionEvent e) {
		
		Object objComponent = null;
		ForwardPanelComponent panelComponent = null;
		ForwardListModel listModel = null;
		ForwardTableModel tableModel = null;
		ForwardTableModelColumn tableColumn = null;
		ListSelectionModel listSelectionModel = null; 
		JList jlist = null;
		JTable jtable = null;
		InnerComponent innerComponent =null;
        List<Integer> li_rowNewSelected = null;
        List<Integer> li_rowUnselected = null;
        List<Integer> li_colNewSelected = null;
        List<Integer> li_colUnselected = null;
        List<EnumForwardEvent> li_eventTrigger = null;
        Object tableValueCell = null;
  		@SuppressWarnings("unused")
		String panelName = "";
		String panelComponentName = "";
		int listSelectedIndex = 0;
		int numRow = 0;
		
		this.mcb.system.setEventListSelection(e);
		objComponent = e.getSource();
	
		
		////////////////////////////////////
        // Gestione selezione in JList
		////////////////////////////////////
		
	    if (objComponent instanceof JList) {
	    	
		   // Attivazione di adjustment di servizio: skip
		   if (e.getValueIsAdjusting() == true) {
			   return;
		   }
		   
		   jlist = (JList) objComponent;
		   listSelectedIndex = jlist.getSelectedIndex();
		   
		   // Nome pannello e componente scatenante l'evento
		   panelComponent = this.mcb.function.getPanelComponent(jlist.getName());
		   panelName = this.mcb.function.getPanelOwner(jlist.getName());
		   panelComponentName = jlist.getName();
		   
           // Caricamento valori di sistema per l'evento disponibili all'applicazione
		   this.mcb.system.setEventComponentObject(objComponent);
		   this.mcb.system.setEventComponentType(panelComponent.getType());
		   this.mcb.system.setEventComponentName(panelComponentName);
		   this.mcb.system.setListSelectedIndex(listSelectedIndex);
		   this.mcb.system.setListName(jlist.getName());

		   // Riga NON selezionata, probabilmente a fronte di remove sulla JList
		   if (listSelectedIndex == -1) {
			   this.mcb.system.setListBoundObject(null);
			   this.mcb.system.getListSelectedObjects().clear();
			   this.mcb.system.setListSelectedItem(null);
			   this.mcb.system.getListSelectedItems().clear();
			   this.mcb.system.setListSelectedIndexLeading(-1);
			   this.mcb.system.getListSelectedIndexes().clear();
	           return;		
		   }
		   
		   // Recupero modello forward per la lista (recuperabile anche con jlist.getModel)
		   panelComponent = this.mcb.function.getPanelComponent(this.mcb.system.getListName());
		   listModel = panelComponent.getListModel();
		   
		   // Valore text e object riga selezionata o prima riga se selezione multipla
		   this.mcb.system.setListSelectedItem((String)jlist.getSelectedValue());	
		   this.mcb.system.setListBoundObject(listModel._getDataBound(listSelectedIndex));			 // Oggetto binded a null se non disponibile					

		   // Indici righe multiple selezionate in ordine crescente (può essere una sola riga)
		   // Valori text e oggetti binded per tutte le righe selezionate
		   this.mcb.system.setListSelectedIndexLeading(jlist.getLeadSelectionIndex());					 // Ultimo indice selezionato
		   this.mcb.system.getListSelectedItems().clear();
		   this.mcb.system.getListSelectedObjects().clear();
		   for (int index : jlist.getSelectedIndices()) {
			   this.mcb.system.getListSelectedIndexes().add(index) ;  
			   this.mcb.system.getListSelectedItems().add((String) listModel.getElementAt(index)) ;  
		   }

		   // Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
		   manageLogicDeclaredActivation(this.mcb, panelComponentName, EnumForwardEvent.ON_JAVA_VALUE_CHANGED
														   			 , EnumForwardEvent.ON_LIST_ROWS_SELECTED
														   			 , EnumForwardEvent.ON_LIST_ROWS_UNSELECTED);
		   return;
	    }
	    
	    
		/////////////////////////////////////////////////////////////
        // Gestione selezione in JTable.
	    // Il listener è attivo sul listSelectionModel della tabella.
	    // Il listener è attivo sia per le righe sia per le colonne
		/////////////////////////////////////////////////////////////

	    if (objComponent instanceof ListSelectionModel) {
	    	listSelectionModel = (ListSelectionModel)e.getSource();
	    	
	    	// Attivazione di adjustment di servizio: skip
	    	if (e.getValueIsAdjusting() == true) {
			   return;
	    	}
	    	// selezione vuota
	    	if (listSelectionModel.isSelectionEmpty()) {
				return;
			}
	    	
	    	// Recupero descrittore componente JTable e oggetto swing
	    	innerComponent = this.mcb.function.getComponentJTable(listSelectionModel);
	    	if (innerComponent == null) {
	    		return;
			}	
	    	jtable = (JTable) innerComponent.component;
	    	
		    // Nome pannello e componente scatenante l'evento
		    panelComponent = this.mcb.function.getPanelComponent(jtable.getName());
		    panelName = this.mcb.function.getPanelOwner(jtable.getName());
		    panelComponentName = jtable.getName();

            // Caricamento valori di sistema per l'evento disponibili all'applicazione
		    this.mcb.system.setEventComponentObject(jtable);
		    this.mcb.system.setEventComponentType(panelComponent.getType());
		    this.mcb.system.setEventComponentName(panelComponentName);
	    	
	       	// Impostazione righe deselezionate dalla selezione corrente
	    	li_eventTrigger = new ArrayList<EnumForwardEvent> ();
	        li_rowNewSelected = new ArrayList<Integer> ();
	        li_rowUnselected = new ArrayList<Integer> ();
            for (int rowNewSelected : jtable.getSelectedRows()) {
            	li_rowNewSelected.add(rowNewSelected);
 			}
            for (int rowOldSelected : this.mcb.system.getTableSelectedRows()) {
               	if (!li_rowNewSelected.contains(rowOldSelected)) {
               		li_rowUnselected.add(rowOldSelected);
				}
			}
            int ar_unselectedRows[] = new int[li_rowUnselected.size()];
            for (int i = 0; i < li_rowUnselected.size(); i++) {
            	ar_unselectedRows[i] = li_rowUnselected.get(i);
			}
            this.mcb.system.setTableUnselectedRows(ar_unselectedRows);
            
	       	// Impostazione righe deselezionate dalla selezione corrente
	        li_colNewSelected = new ArrayList<Integer> ();
	        li_colUnselected = new ArrayList<Integer> ();
            for (int colNewSelected : jtable.getSelectedColumns()) {
            	li_colNewSelected.add(colNewSelected);
 			}  
            for (int colOldSelected : this.mcb.system.getTableSelectedColumns()) {
               	if (!li_colNewSelected.contains(colOldSelected)) {
               		li_colUnselected.add(colOldSelected);
				}
			}
            int ar_unselectedCols[] = new int[li_colUnselected.size()];
            for (int i = 0; i < li_colUnselected.size(); i++) {
            	ar_unselectedCols[i] = li_colUnselected.get(i);
			}
            this.mcb.system.setTableUnselectedColumns(ar_unselectedRows);
            
		    // Impostazione righe e colonne selezionate in system
	    	this.mcb.system.setTableSelectedRow(jtable.getSelectedRow());
	    	this.mcb.system.setTableSelectedRowCount(jtable.getSelectedRowCount());
	    	this.mcb.system.setTableSelectedRows(jtable.getSelectedRows());
	    	this.mcb.system.setTableSelectedColumn(jtable.getSelectedColumn());
	    	this.mcb.system.setTableSelectedColumnCount(jtable.getSelectedColumnCount());
	    	this.mcb.system.setTableSelectedColumns(jtable.getSelectedColumns());
	    	
	    	// Impostazione valore variabili colonne tabella 
	    	tableModel = (ForwardTableModel) jtable.getModel();
	    	numRow = jtable.getSelectedRow();
	    	this.mcb.system.setTableBoundObject(tableModel._getDataBoundAtRow(numRow));
	    	for (int numCol = 0; numCol < tableModel._getColumns().size(); numCol++) {
	    		tableColumn = tableModel._getColumns().get(numCol);
	    		tableValueCell = tableModel.getValueAt(numRow, numCol);
	    		setVarValue(this.mcb, tableColumn.getName(), tableValueCell);
			}
	    	// Impostazione valore variabile oggetto bound della riga selezionata (la prima)
		    setVarValue(this.mcb, tableModel._getVarNameDataBound(), tableModel._getDataBoundAtRow(numRow));
	    	
	    	// Impostazione eventi di cui eseguire eventuali logiche applicative
	    	li_eventTrigger.add(EnumForwardEvent.ON_JAVA_VALUE_CHANGED);
	    	li_eventTrigger.add(EnumForwardEvent.ON_TABLE_ROWS_SELECTED);
	    	li_eventTrigger.add(EnumForwardEvent.ON_TABLE_COLS_SELECTED);
	    	if (li_rowUnselected.size() > 0) {
	    		li_eventTrigger.add(EnumForwardEvent.ON_TABLE_ROWS_UNSELECTED);
			}
	    	if (li_colUnselected.size() > 0) {
	    		li_eventTrigger.add(EnumForwardEvent.ON_TABLE_COLS_UNSELECTED);
			}
	    	
		    // Gestione attivazione logiche applicative dichiarate per l'evento, se eveneti dichiarati sul componente che ha scatenato l'evento 
		    manageLogicDeclaredActivation(this.mcb, panelComponentName, EnumForwardEvent.ON_JAVA_VALUE_CHANGED
																      , EnumForwardEvent.ON_TABLE_ROWS_SELECTED
																      , EnumForwardEvent.ON_TABLE_ROWS_UNSELECTED
																      , EnumForwardEvent.ON_TABLE_COLS_SELECTED
																      , EnumForwardEvent.ON_TABLE_COLS_UNSELECTED
										);
 		}
	}

	/* ------------------
	 * DocumentListener 
	 * ------------------
	 * 
	 * A fronte di inserimento/delete/update caratteri in un document (come un JField).
	 * 
	 */
	public void changedUpdate(DocumentEvent e) {documentEventManagement(e, EnumForwardEvent.ON_JAVA_CHANGE_UPDATE, EnumForwardEvent.ON_CHARS_UPDATE );}
	public void insertUpdate(DocumentEvent e)  {documentEventManagement(e, EnumForwardEvent.ON_JAVA_INSERT_UPDATE, EnumForwardEvent.ON_CHARS_INSERT );}
	public void removeUpdate(DocumentEvent e)  {documentEventManagement(e, EnumForwardEvent.ON_JAVA_REMOVE_UPDATE, EnumForwardEvent.ON_CHARS_DELETE);}

	/*
	 * Gestione generalizzata venti  intercettati da DocumentListener
	 * 
	 * Il document può essere relativo a un JTextField, JFormattedTextField
	 * JPasswordField, JTextArea, , JTextPane
	 * controllo in cui può essere iserito del testo.
	 * In fase di definizione della funzione e di inserimento di un controllo
	 * nel pannello, viene impostata la proprietà "ownerName" del documento associato 
	 * al controllo. Questo è il solo metodo per conoscere il nome dell'oggetto
	 * che ha scatenato l'evento di document listener.
	 * 
	 */
	private void documentEventManagement(DocumentEvent e, EnumForwardEvent ... onJavaDocumentEvent) {
		
		ForwardPanelComponent panelComponent = null;
		@SuppressWarnings("unused")
		String panelName = "";
		String componentName = "";  
		 
		// Recupero nome source: se evento su control non registrato dalla definizione della funzione skip
		componentName = (String) e.getDocument().getProperty("ownerName");
	    if (componentName == null) {
			return;
		}
	    
	    // Recupero componente e pannello
		panelName = this.mcb.function.getPanelOwner(componentName);
	    panelComponent = this.mcb.function.getPanelComponent(componentName);
	    if (panelComponent == null) {
			return;
		}
	    
	    // Update info generali correnti evento
		this.mcb.system.setEventDocument(e);
	    this.mcb.system.setEventComponentObject(panelComponent.getGraphicObject());
	    this.mcb.system.setEventComponentType(panelComponent.getType());
	    this.mcb.system.setEventComponentName(componentName);

	    // Update info specifiche correnti evento DocumentListener
	    this.mcb.system.setEventDocumentCharLenght(e.getLength());										// Numero crt inseriti/deletati/rimpiazzati nel documento
	    this.mcb.system.setEventDocumentCharOffset(e.getOffset());										// Offset crt inseriti/deletati/rimpiazzati nel documento
	    this.mcb.system.setEventDocumentStartOffset(e.getDocument().getStartPosition().getOffset());    // Offset di inizio del documento          
	    this.mcb.system.setEventDocumentEndOffset(e.getDocument().getEndPosition().getOffset());		// Offset di fine del documento (ultimo crt)
	    this.mcb.system.setEventDocumentLength(e.getDocument().getLength());							// Numero crt totali presenti nel documento
  	    try {
			this.mcb.system.setEventDocumentCharText(e.getDocument().getText(e.getOffset(), e.getLength()));// Testo  crt inseriti/deletati/rimpiazzati nel documento
			this.mcb.system.setEventDocumentText(e.getDocument().getText(0, e.getDocument().getLength()));  // Testo completo documento
		} catch (BadLocationException e1) {
			this.mcb.system.setEventDocumentCharText("");
			this.mcb.system.setEventDocumentText("");
		}
    
	    // Gestione attivazione logiche applicative dichiarate per l'evento, se eveneti dichiarati sul componente che ha scatenato l'evento  
	    manageLogicDeclaredActivation(this.mcb, componentName, onJavaDocumentEvent );
		
	}


	/* ----------------
	 * WindowListener
	 * ----------------
	 * 
	 * A fronte di Apertura/chiusura/Attivazione/Disattivazione/Iconificazione/Deiconificazione
	 */
	public void windowOpened(WindowEvent e) 	{windowEventManagement(e, EnumForwardEvent.ON_JAVA_WINDOW_OPENED);}
	public void windowClosing(WindowEvent e) 	{windowEventManagement(e, EnumForwardEvent.ON_JAVA_WINDOW_CLOSING);}
	public void windowClosed(WindowEvent e) 	{windowEventManagement(e, EnumForwardEvent.ON_JAVA_WINDOW_CLOSED);}
	public void windowActivated(WindowEvent e) 	{windowEventManagement(e, EnumForwardEvent.ON_JAVA_WINDOW_ACTIVATED);}
	public void windowDeactivated(WindowEvent e){windowEventManagement(e, EnumForwardEvent.ON_JAVA_WINDOW_DEACTIVATED);}
	public void windowIconified(WindowEvent e) 	{windowEventManagement(e, EnumForwardEvent.ON_JAVA_WINDOW_ICONIFIED);}
	public void windowDeiconified(WindowEvent e){windowEventManagement(e, EnumForwardEvent.ON_JAVA_WINDOW_DEICONIFIED);}

	
	/*
	 * Gestione eventi  intecettati da window Listener
	 */
	private void windowEventManagement(WindowEvent e, EnumForwardEvent ... onJavaWindowEvent) {
		
		Object objComponent = null;
		JFrame jframe = null;
		JDialog jdialog = null;
		EnumForwardEvent onJavaWindowEventExtended[] = null;
		String dialogName = "";
        String frameName = "";				// JFrame, JDialog o JPanel
		
		this.mcb.system.setEventWindow(e);
		objComponent = e.getSource();
		this.mcb.system.setEventComponentObject(objComponent);

		// Window event di un JDialog attivato con ACTION DIALOG_START
		if (objComponent instanceof JDialog && this.mcb.system.isDialogActive()) {
			jdialog = (JDialog) objComponent;
			this.mcb.system.setEventComponentType(EnumForwardComponent.JDialog);
			dialogName = jdialog.getName();
			this.mcb.system.setEventComponentName(dialogName);
			frameName = dialogName;
			// Window closing
			if (onJavaWindowEvent[0] == EnumForwardEvent.ON_JAVA_WINDOW_CLOSING) {
				this.mcb.system.removeDialogActive(dialogName);						// Remove dialogo da elenco dialoghi attivi
				this.mcb.system.setDialogOptionChoosed(JOptionPane.CLOSED_OPTION);
				onJavaWindowEventExtended = new EnumForwardEvent[2];
				onJavaWindowEventExtended[0] = onJavaWindowEvent[0];
				onJavaWindowEventExtended[1] = EnumForwardEvent.ON_DIALOG_CLOSING_ATTEMPTED;
			    manageLogicDeclaredActivation(this.mcb, frameName, onJavaWindowEventExtended); // Attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento 
			    return;
			}
			// Window event generico su Dialog
		    manageLogicDeclaredActivation(this.mcb, frameName, onJavaWindowEvent);
            return;
		}
		
		// Window event di chiusura JFrame/JDialog con X di una funzione attivata da START/XCTL.
		// La chiusura è stata effettuata con click su X della finestra
		// Viene ripristinato il contesto della funzione chiamante.
		// Vengono eseguiote le eventuali logicge sulle evento di finestra
		// Le eventuali logiche di restituzione parametri o altro sono eseguite come a fronte dell'action FUNCTION_RETURN attivata applicativamente
		// La funzione resta caricata nella struttura hash 
		if ((objComponent instanceof JFrame || objComponent instanceof JDialog)
  		&& onJavaWindowEvent[0] == EnumForwardEvent.ON_JAVA_WINDOW_CLOSING) {
		    manageLogicDeclaredActivation(this.mcb, frameName, onJavaWindowEvent); // Attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento 
		    ForwardDoParms actionCoded = null;
		    actionCoded = new ForwardDoParms();
		    actionCoded.action = EnumForwardAction.FUNCTION_RETURN;
		    execActionForFunction(this.mcb, actionCoded, false);
    		return;
		}

		// Window event generico di un JFrame, non di chiusura
		if (objComponent instanceof JFrame) {
			jframe = (JFrame) objComponent;
			this.mcb.system.setEventComponentType(EnumForwardComponent.JFrame);
			this.mcb.system.setEventComponentName(jframe.getName());
			frameName = jframe.getName();
			onJavaWindowEventExtended = new EnumForwardEvent[1];
		    manageLogicDeclaredActivation(this.mcb, frameName, onJavaWindowEvent); // Attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento 
		    return;
		} 
		
	}

	
	/* ------------------
	 * TreeModelListener
	 * ------------------
	 * 
	 * JTree, a fronte di editing testo nodo (attivato da 3 click), insert, remove nodo, modifica struttura tree
	 */
	public void treeNodesChanged(TreeModelEvent e) 		{treeNodesEventModelManagement(e, EnumForwardEvent.ON_JAVA_TREE_NODES_CHANGED,  	EnumForwardEvent.ON_TREE_NODE_EDITED_TEXT);}
	public void treeNodesInserted(TreeModelEvent e) 	{treeNodesEventModelManagement(e, EnumForwardEvent.ON_JAVA_TREE_NODES_INSERTED,  	EnumForwardEvent.ON_TREE_NODE_INSERTED);}
	public void treeNodesRemoved(TreeModelEvent e) 		{treeNodesEventModelManagement(e, EnumForwardEvent.ON_JAVA_TREE_NODES_REMOVED,  	EnumForwardEvent.ON_TREE_NODE_REMOVED);}
	public void treeStructureChanged(TreeModelEvent e) 	{treeNodesEventModelManagement(e, EnumForwardEvent.ON_JAVA_TREE_STRUCTURE_CHANGED,  EnumForwardEvent.ON_TREE_STRUCTURE_CHANGED);}

	/*
	 * Gestione eventi generati da JTree e intercettati con TreeModelListener
	 */
	private void treeNodesEventModelManagement(TreeModelEvent e, EnumForwardEvent ... onJavaTreeEvent) {
		
		Object objComponent = null;
		ForwardPanelComponent panelComponent = null;
		JTree jtree = null;
		DefaultMutableTreeNode node = null;
		DefaultMutableTreeNode nodeParent = null;
		ForwardTreeModel treeModel = null; 
		TreePath treePath = null;										// Contiene, nell'ordine, il nodo root, fino al nodo parent di quello correntemente selezionato
		TreePath treePathParent = null;									// Contiene, nell'ordine, il nodo root, fino al nodo parent di quello correntemente selezionato
		DefaultMutableTreeNode[] treePathParentNodes = null;
		Object[] treePathParentNodesObject = null;
		String panelName = "";
		String panelComponentName = "";
		int indexNode = 0;												// Indice nodo corrente selezionato, 0-based, figlio di nodeParent
		 
		objComponent = e.getSource();
	
		// Controllo di sicurezza
		if (!(objComponent instanceof DefaultTreeModel)) {return; }
		
		// Recupero informazioni tree, nodo, path 
		treeModel = (ForwardTreeModel) objComponent;
		jtree = treeModel._getJtree();
		treePath = e.getTreePath();												// Path con nodi da root fino a nodo parent di quello changed/removed/inserted
		nodeParent = (DefaultMutableTreeNode) treePath.getLastPathComponent();	// Nodo parent diretto
		treePathParent = treePath.getParentPath();                    			// Path con nodi da root fino a parent nodo selzionato
		treePathParentNodes = new DefaultMutableTreeNode[treePathParent.getPath().length];
		treePathParentNodesObject = new Object[treePathParent.getPath().length];
		for (int i = 0; i < treePathParent.getPath().length; i++) {
			treePathParentNodes[i] = (DefaultMutableTreeNode) treePathParent.getPath()[i];
			treePathParentNodesObject[i] = treePathParentNodes[i].getUserObject();
		}
		switch (onJavaTreeEvent[0]) {
			case ON_JAVA_TREE_NODES_CHANGED:
			case ON_JAVA_TREE_NODES_INSERTED:
				indexNode = e.getChildIndices()[0];
		        node = (DefaultMutableTreeNode)(nodeParent.getChildAt(indexNode));
				break;
			case ON_JAVA_TREE_NODES_REMOVED:
		        node = (DefaultMutableTreeNode)(e.getChildren()[0]);
				break;
			case ON_JAVA_TREE_STRUCTURE_CHANGED:
				break;
		}
	   
		// Nome pannello e componente scatenante l'evento
		panelComponent = this.mcb.function.getPanelComponent(jtree.getName());
		panelName = this.mcb.function.getPanelOwner(jtree.getName());
		panelComponentName = jtree.getName();
	   
		// Caricamento valori di sistema per l'evento disponibili all'applicazione
		this.mcb.system.setActivePanel(panelName);
		this.mcb.system.setEventComponentObject(jtree);
		this.mcb.system.setEventComponentType(panelComponent.getType());
		this.mcb.system.setEventComponentName(panelComponentName);
		this.mcb.system.setEventTreeModelEvent(e);
		this.mcb.system.setEventTreeModel(treeModel);
		this.mcb.system.setEventTreeNode(node);
		this.mcb.system.setEventTreeNodeIndex(indexNode);
		this.mcb.system.setEventTreeNodeParent(nodeParent);
		if (nodeParent != null) {this.mcb.system.setEventTreeNodeParentObject(nodeParent.getUserObject());}
		this.mcb.system.setEventTreeNodesParent(treePathParentNodes);
 		this.mcb.system.setEventTreeNodeObject(node.getUserObject());
		this.mcb.system.setEventTreeNodesParentObject(treePathParentNodesObject);

	    // Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
	    manageLogicDeclaredActivation(this.mcb, panelComponentName, onJavaTreeEvent);
	    return;
	}



	/* -----------------------
	 * TreeSelectionListener
	 * -----------------------
	 * 
	 * JTree, a fronte di selezione nodo e attivato anche a fronte di remove, da java
	 * Usato per individuare un nodo deselzionato.
	 * Il nodo selezionato viene invece individuato da MouseListener (keyPressed)
	 */

	public void valueChanged(TreeSelectionEvent e) {
		
		Object objComponent = null;
		ForwardPanelComponent panelComponent = null;
		JTree jtree = null;
		DefaultMutableTreeNode node = null;
		DefaultMutableTreeNode nodeParent = null;
		DefaultMutableTreeNode nodeWork = null;
		ForwardTreeModel treeModel = null; 
		TreePath treePath = null;										// Contiene, nell'ordine, il nodo root, fino al nodo parent di quello correntemente selezionato
		TreePath treePathParent = null;									// Contiene, nell'ordine, il nodo root, fino al nodo parent di quello correntemente selezionato
		TreePath[] selectionPaths = null;
		TreeSelectionModel selectionModel = null;
		DefaultMutableTreeNode[] treePathParentNodes = null;
		Object[] treePathParentNodesObject = null;
		List<DefaultMutableTreeNode> li_TreeNodesSelected = null;
		List<ForwardTreeUserObject> li_TreeNodesSelectedObject = null;
		DefaultMutableTreeNode[] treeNodesSelected = null;
		Object[] treeNodesSelectedObject = null;
		String panelName = "";
		String panelComponentName = "";
		int cntSelected = 0;
		int indexNode = 0;												// Indice nodo corrente selezionato, 0-based, figlio di nodeParent
		
		
		this.mcb.system.setEventTreeSelection(e);
		objComponent = e.getSource();
	
		// Controllo di sicurezza
		if (!(objComponent instanceof JTree)) {return; }
		
		// Recupero informazioni base quali tree, nodo, path 
		jtree = (JTree) objComponent;
		treeModel = (ForwardTreeModel) jtree.getModel();
		selectionModel = jtree.getSelectionModel();
		treePath = selectionModel.getSelectionPath();						// Path con array di nodi fino a quello selezionato incluso
		
		// Probabile attivazione da parte di java a fronte di remove: skip
		if (treePath == null) {
			return;
		}
		
		// Nome pannello e componente scatenante l'evento
		panelComponent = this.mcb.function.getPanelComponent(jtree.getName());
		panelName = this.mcb.function.getPanelOwner(jtree.getName());
		panelComponentName = jtree.getName();
	   
        // Recupero informazioni di dettaglio selezione
		cntSelected = selectionModel.getSelectionCount();					// Numero nodi selezionati
		selectionPaths = selectionModel.getSelectionPaths();				// Singoli array di path di nodi selezionati
		node = (DefaultMutableTreeNode) treePath.getLastPathComponent();	// Nodo selezionato 
		nodeParent = (DefaultMutableTreeNode) node.getParent();				// Node parent diretto
		treePathParent = treePath.getParentPath();                    		// Path con nodi da root fino a parent nodo selezionato
		// Su expand/collapse root è null
		if (treePathParent != null) {
			treePathParentNodes = new DefaultMutableTreeNode[treePathParent.getPath().length];
			treePathParentNodesObject = new Object[treePathParentNodes.length];
			for (int i = 0; i < treePathParent.getPath().length; i++) {
				treePathParentNodes[i] = (DefaultMutableTreeNode) treePathParent.getPath()[i];
				treePathParentNodesObject[i] = treePathParentNodes[i].getUserObject();
			}
		}

		// Estrazione di tutti i nodi selezionati, in tutti i path, anche su selezioni multiple NON contigue
		li_TreeNodesSelected = new ArrayList<DefaultMutableTreeNode>();
		li_TreeNodesSelectedObject = new ArrayList<ForwardTreeUserObject>();
		for (TreePath selectedPath : selectionPaths) {
			nodeWork = (DefaultMutableTreeNode) selectedPath.getLastPathComponent();
			li_TreeNodesSelected.add(nodeWork);
			li_TreeNodesSelectedObject.add((ForwardTreeUserObject) nodeWork.getUserObject());
		}
		treeNodesSelected = new DefaultMutableTreeNode[li_TreeNodesSelected.size()];
		treeNodesSelectedObject = new ForwardTreeUserObject[li_TreeNodesSelectedObject.size()];
		treeNodesSelected = li_TreeNodesSelected.toArray(treeNodesSelected);
		treeNodesSelectedObject = li_TreeNodesSelectedObject.toArray(treeNodesSelectedObject);
		indexNode = treeModel.getIndexOfChild(nodeParent, node);
				
		// Caricamento valori di sistema per l'evento disponibili all'applicazione
		this.mcb.system.setActivePanel(panelName);
		this.mcb.system.setEventComponentObject(jtree);
		this.mcb.system.setEventComponentType(panelComponent.getType());
		this.mcb.system.setEventComponentName(panelComponentName);
		this.mcb.system.setEventTreeSelection(e);
		this.mcb.system.setEventTreeModel(treeModel);
		this.mcb.system.setEventTreeNode(node);
		this.mcb.system.setEventTreeNodeIndex(indexNode);
		this.mcb.system.setEventTreeNodeParent(nodeParent);
		this.mcb.system.setEventTreeNodesParent(treePathParentNodes);
		this.mcb.system.setEventTreeNodesSelectedCount(cntSelected);
		this.mcb.system.setEventTreeNodesSelected(treeNodesSelected);
		this.mcb.system.setEventTreeNodeObject(node.getUserObject());
		if (nodeParent != null) {this.mcb.system.setEventTreeNodeParentObject(nodeParent.getUserObject());}
		this.mcb.system.setEventTreeNodesParentObject(treePathParentNodesObject);
		this.mcb.system.setEventTreeNodesSelectedObject(treeNodesSelectedObject);

		
	    // Gestione attivazione logiche applicative dichiarate per l'evento, se eventi dichiarati sul componente che ha scatenato l'evento  
	    manageLogicDeclaredActivation(this.mcb, panelComponentName, EnumForwardEvent.ON_JAVA_TREE_VALUE_CHANGED
	    														  , EnumForwardEvent.ON_TREE_NODE_SELECTED);
	}



	/* -----------------------
	 * TreeExpansionListener
	 * -----------------------
	 * 
	 * JTree, a fronte di expand/collapse nodo
	 */
	public void treeExpanded(TreeExpansionEvent e) 	{treeNodesEventExpansionManagement(e, EnumForwardEvent.ON_JAVA_TREE_EXPANDED,  	EnumForwardEvent.ON_TREE_NODE_EXPANDED);}
	public void treeCollapsed(TreeExpansionEvent e) {treeNodesEventExpansionManagement(e, EnumForwardEvent.ON_JAVA_TREE_COLLAPSED,  EnumForwardEvent.ON_TREE_NODE_COLLAPSED);}

	
	/*
	 * Gestione eventi generati da JTree e intercettati con TreeExpansionListener
	 */
	private void treeNodesEventExpansionManagement(TreeExpansionEvent e, EnumForwardEvent ... onJavaTreeEvent) {
		Object objComponent = null;
		JTree jtree = null;
		ForwardPanelComponent panelComponent = null;
		ForwardTreeModel treeModel = null; 
		DefaultMutableTreeNode node = null;
		DefaultMutableTreeNode nodeParent = null;
		DefaultMutableTreeNode nodeChild = null;
		ForwardTreeUserObject nodeChildObjectFirst = null;
		DefaultMutableTreeNode[] treePathParentNodes = null;
		TreePath treePath = null;										// Contiene, nell'ordine, il nodo root, fino al nodo parent di quello correntemente selezionato
		TreePath treePathParent = null;									// Contiene, nell'ordine, il nodo root, fino al nodo parent di quello correntemente selezionato
		DefaultMutableTreeNode treeNodesChild[] = null;
		Object treeNodesChildObject[] = null; 
		Object[] treePathParentNodesObject = null;
		String panelName = "";
		String panelComponentName = "";
		int childCount = 0;		
		int iChild = 0;		
				
		objComponent = e.getSource();
	
		// Controllo di sicurezza
		if (!(objComponent instanceof JTree)) {return; }

		// Recupero informazioni tree, nodo, path 
		jtree = (JTree) objComponent;
		treeModel = (ForwardTreeModel) jtree.getModel();
		treePath = e.getPath();												// Path con nodi da root fino a nodo expanded/collapsed
		node = (DefaultMutableTreeNode) treePath.getLastPathComponent();	// Nodo expanded/collapsed
		nodeParent = (DefaultMutableTreeNode) node.getParent();				// Nodo parent diretto
		treePathParent = treePath.getParentPath();                    		// Path con nodi da root fino a parent nodo selzionato
		// Su expand/collapse roo è null
		if (treePathParent != null) {
			treePathParentNodes = new DefaultMutableTreeNode[treePathParent.getPath().length];
			treePathParentNodesObject = new Object[treePathParent.getPath().length];
			for (int i = 0; i < treePathParent.getPath().length; i++) {
				treePathParentNodes[i] = (DefaultMutableTreeNode) treePathParent.getPath()[i];
				treePathParentNodesObject[i] = treePathParentNodes[i].getUserObject();
			}
		}
		
	    // Recupero nodi figli a fronte di expand di nodo con child già presenti
		childCount = node.getChildCount();
		nodeChild = (DefaultMutableTreeNode) node.getFirstChild();
		nodeChildObjectFirst = (ForwardTreeUserObject) nodeChild.getUserObject();
		
		// Recupero nodi figli a fronte di expand di nodo con child già presenti.
		// Se si tratta dell'espansione di un nodo folder con un solo nodo di servizio
		// i children devono essere inseriti dall'applicazione 
		if (onJavaTreeEvent[0] == EnumForwardEvent.ON_JAVA_TREE_EXPANDED 
		&& !nodeChildObjectFirst.isNodeService()) {
			treeNodesChild = new DefaultMutableTreeNode[childCount];
			treeNodesChildObject = new Object[childCount];
			while (nodeChild != null) {
				treeNodesChild[iChild] = nodeChild;
				treeNodesChildObject[iChild] = nodeChild.getUserObject();
				nodeChild = nodeChild.getNextSibling();
				iChild++;
			}
		}
		
		// Nome pannello e componente scatenante l'evento
		panelComponent = this.mcb.function.getPanelComponent(jtree.getName());
		panelName = this.mcb.function.getPanelOwner(jtree.getName());
		panelComponentName = jtree.getName();

		// Caricamento valori di sistema per l'evento disponibili all'applicazione
		this.mcb.system.setActivePanel(panelName);
		this.mcb.system.setEventComponentObject(jtree);
		this.mcb.system.setEventComponentType(panelComponent.getType());
		this.mcb.system.setEventComponentName(panelComponentName);
		this.mcb.system.setEventTreeExpansion(e);
		this.mcb.system.setEventTreeModel(treeModel);
		this.mcb.system.setEventTreeNode(node);
		this.mcb.system.setEventTreeNodeParent(nodeParent);
		this.mcb.system.setEventTreeNodesParent(treePathParentNodes);
		this.mcb.system.setEventTreeNodesChild(treeNodesChild);
 		this.mcb.system.setEventTreeNodeObject(node.getUserObject());
		this.mcb.system.setEventTreeNodesParentObject(treePathParentNodesObject);
		this.mcb.system.setEventTreeNodesChildObject(treeNodesChildObject);

		// Prima espansione di nodo folder con nodo child di servizio.
		// Si elimina il nodo di servizio.
		// Si attiva l'eventuale logica applicativa di caricamento children
		if (onJavaTreeEvent[0] == EnumForwardEvent.ON_JAVA_TREE_EXPANDED 
		&&  nodeChildObjectFirst.isNodeService()) {
			treeModel._removeNode(nodeChildObjectFirst);
			manageLogicDeclaredActivation(this.mcb, panelComponentName, EnumForwardEvent.ON_TREE_NODE_CHILDREN_TO_ADD);
			return;
		}
		
		
	    // Gestione attivazione logiche applicative dichiarate per l'evento di expand/collapse
	    manageLogicDeclaredActivation(this.mcb, panelComponentName, onJavaTreeEvent);
	}



    ////////////////////////////////////////////////////////////////////////////////////////
	// Metodi privati
	////////////////////////////////////////////////////////////////////////////////////////
    
	 
	/* ---------------------------------------------------------------------
	 * Gestione attivazione actions a fronte di evento 
	 * ---------------------------------------------------------------------
	 * 
	 * Si analizzano le dichiarazioni ON_EVENT() della funzione
	 * 	Si trattano quelle relative al componente fornito
	 *    Si verfica se l'evento dichiarato è fra quelli forniti in input
	 * 		in base a ON_EVENT()
	 *        	Esecuzione esplicita comando  
	 *          Call via reflection metodo di logica applicativa riusabile
	 * 
	 */
    private void manageLogicDeclaredActivation(InnerMonitorControBlock mcb, String componentName, EnumForwardEvent ... ar_eventTrigger ) {
		
        InnerOnEvent eventDeclared = null;
   	
    	// Scan eventi da cercare, di cui eseguire le actions
    	for (EnumForwardEvent eventTrigger : ar_eventTrigger) {
			
         	
        	// Ricerca evento su specifico componente o identificativo
        	eventDeclared = this.mcb.function.getOnEventMap().get(eventTrigger.ordinal() + ":" + componentName);
        	
        	// Evento Non individuato per il componente o l'identificativo specifico
        	// Verifica esistenza evento generico non specifico per componente
        	if (eventDeclared == null) {
        		eventDeclared = this.mcb.function.getOnEventMap().get(eventTrigger.ordinal() + ":" + "");
        		// Non è presente nemmeno un evento generico applicabile: next
        		if (eventDeclared == null) {
        			continue;
        		}
          	}
  
			// Evento maskerato, da non eseguire per il componente
			if (isEventMasked(mcb, componentName, eventDeclared.event)) {
				continue;
			}
			
			// Esecuzione logiche con eventuale esecuzione ON_EXCEPTION in caso di exception
			logicExecution(mcb, eventDeclared);
        	
		}
   	}

    /*
     * Esecuzione actions associate all'evento
     */
	private void logicExecution(InnerMonitorControBlock mcb, InnerOnEvent innerOnEvent) {
		
		execActions(mcb, innerOnEvent);
		
		// Nessuna exception applicativa in esecuzione del metodo
		if (mcb.system.getReusableMethodException() == null) {return;}

		// Exception: attivazione eventuali ON_EVENT(EnumForwardEvent.ON_SYSTEM_EXCEPTION_FUNCTION, ..))
		for (InnerOnEvent innerOnEventNested : this.mcb.function.getOnEventList()) {
			if (innerOnEventNested.event != EnumForwardEvent.ON_SYSTEM_EXCEPTION_FUNCTION) {continue;}
			mcb.system.setReusableMethodExceptionHandling(true);
			
			execActions(mcb, innerOnEventNested);
			
			mcb.system.setReusableMethodException(null);
			mcb.system.setReusableMethodExceptionHandling(false);
		}
	}


	/* -------------------------------------------------------------------------------------------
	 * Restituisce l'event descriptor con le action da eseguire su un certo oggetto per un evento
	 * -------------------------------------------------------------------------------------------
	 * 
	 * Si utilizza a fini di ottimizzazione, per esempio a fronte di populate da db
	 * per evitare overhead nella ricerca.
	 * Se nessun evento è stato trovato per l0offetto restituisce null
	 * L'esecuzione deve essere poi effettuata con logicExecution(InnerOnEvent innerOnEvent)
	 * che gestisce anche l'exception
	 */
    private InnerOnEvent getEventDescriptor(InnerMonitorControBlock mcb, String componentName, EnumForwardEvent eventTrigger ) {
    	InnerOnEvent eventDeclared = null;
    	
    	// Ricerca evento su specifico componente o identificativo
    	eventDeclared = mcb.function.getOnEventMap().get(eventTrigger.ordinal() + ":" + componentName);
    	
    	// Evento individuato per il componente o l'identificativo specifico
    	if (eventDeclared != null) {
			return eventDeclared;
		}
    	
    	// Se dichiarato un evento generico si prende quello per buono
    	eventDeclared = mcb.function.getOnEventMap().get(eventTrigger.ordinal() + ":");
    	
    	// Evento generico individuato valido per qualsiasi componente
    	if (eventDeclared != null) {
			return eventDeclared;
		}

    	// Nessun evento, specifico o generico dichiarato
    	
    	return null;
	}
  
    /*
     * Dispatcher esecuzione azioni applicative, dichiarate su ON_EVENT() o esplicitamente con ACTION()
      */
    private void execActions(InnerMonitorControBlock mcb, InnerOnEvent innerOnEvent) {
    	boolean isActionExecuted = false;
     	int activeActionsStackLevel = 0;
     	
     	// Evento con le azioni da eseguire fa incrementare lo stack
    	mcb.system.setActiveEvent(innerOnEvent.event);
    	activeActionsStackLevel = mcb.system.getActiveActionsStackLevel();
    	activeActionsStackLevel++;
    	mcb.system.setActiveActionsStackLevel(activeActionsStackLevel);
    	
    	
     	// Scan azioni codificate da eseguire  
     	for (ForwardDoParms actionCoded : innerOnEvent.al_actionCoded) {
     	    
     		// Skip in corso
     		if (mcb.system.getActiveActionsToSkip()) {
     			continue;
     		}
     		
     		mcb.system.setActiveAction(actionCoded.action);
     		mcb.system.setActiveDoParms(actionCoded);
 
      		isActionExecuted = false;
     		 
    		// Esecuzione azioni per categoria
    		isActionExecuted = execActionForComponent(mcb, actionCoded, isActionExecuted);  						 	 
    		isActionExecuted = execActionForState(mcb, actionCoded, isActionExecuted);  	            			 
    		isActionExecuted = execActionForComboBox(mcb, actionCoded, isActionExecuted);            				  
    		isActionExecuted = execActionForList(mcb, actionCoded, isActionExecuted);  	            				  
    		isActionExecuted = execActionForTable(mcb, actionCoded, isActionExecuted);                				 	 
    		isActionExecuted = execActionForTree(mcb, actionCoded, isActionExecuted);  								  
    		isActionExecuted = execActionForPanel(mcb, actionCoded, isActionExecuted);  							 	 
    		isActionExecuted = execActionForLayout(mcb, actionCoded, isActionExecuted);  							 	 
    		isActionExecuted = execActionForDialogAndFunction(mcb, actionCoded, innerOnEvent, isActionExecuted);  				 
       		isActionExecuted = execActionForLdv(mcb, actionCoded, isActionExecuted);                 				 	 
    		isActionExecuted = execActionForFunction(mcb, actionCoded, isActionExecuted);  	 						 
    		isActionExecuted = execActionForSystem(mcb, actionCoded, isActionExecuted); 
       		isActionExecuted = execActionForExec(mcb, actionCoded, isActionExecuted);                 				 	 
    		
		}
     
     	// Azioni eseguite, decremento lo stack
    	activeActionsStackLevel = mcb.system.getActiveActionsStackLevel();
    	activeActionsStackLevel--;
    	mcb.system.setActiveActionsStackLevel(activeActionsStackLevel);
     	
    	// Nessun evento in stack, eventuali skip di actions terminano
    	if (activeActionsStackLevel == 0) {
    		mcb.system.setActiveActionsToSkip(false);
    		mcb.system.setActiveActionSkipId("");
		}
    	
	}


    /*
     * Dispatcher esecuzione azioni applicative, dichiarate su un gruppo di actions
      */
    private void execActionsGroup(InnerMonitorControBlock mcb, InnerActionsGroup innerActionsGroup) {
    	boolean isActionExecuted = false;
    	
     	// Scan azioni codificate da eseguire  
     	for (ForwardDoParms actionCoded : innerActionsGroup.al_actionCoded) {
     	    
     		// Skip in corso
     		if (mcb.system.getActiveActionsToSkip()) {
     			continue;
     		}
     		
     		mcb.system.setActiveAction(actionCoded.action);
     		mcb.system.setActiveDoParms(actionCoded);
 
      		isActionExecuted = false;
     		 
    		// Esecuzione azioni per categoria
    		isActionExecuted = execActionForComponent(mcb, actionCoded, isActionExecuted);  						 	 
    		isActionExecuted = execActionForState(mcb, actionCoded, isActionExecuted);  	            			 
    		isActionExecuted = execActionForComboBox(mcb, actionCoded, isActionExecuted);            				  
    		isActionExecuted = execActionForList(mcb, actionCoded, isActionExecuted);  	            				  
    		isActionExecuted = execActionForTable(mcb, actionCoded, isActionExecuted);                				 	 
    		isActionExecuted = execActionForTree(mcb, actionCoded, isActionExecuted);  								  
    		isActionExecuted = execActionForPanel(mcb, actionCoded, isActionExecuted);  							 	 
    		isActionExecuted = execActionForLayout(mcb, actionCoded, isActionExecuted);  							 	 
//    		isActionExecuted = execActionForDialogAndFunction(mcb, actionCoded, innerOnEvent, isActionExecuted);  				 
       		isActionExecuted = execActionForLdv(mcb, actionCoded, isActionExecuted);                 				 	 
    		isActionExecuted = execActionForFunction(mcb, actionCoded, isActionExecuted);  	 						 
    		isActionExecuted = execActionForSystem(mcb, actionCoded, isActionExecuted); 
       		isActionExecuted = execActionForExec(mcb, actionCoded, isActionExecuted);                 				 	 
    		
		}
     
	}


	/* -----------------------------------------
     * Action generali per qualsiasi componente
     * -----------------------------------------
     * 
     * 		COMPONENT_REFRESH 					 
	 *		COMPONENT_GET_VALUE 				 			 
	 *		COMPONENT_SET_VALUE 				 			 
	 *		COMPONENT_SET_FOCUS 				 			 
	 *		COMPONENT_SET_FOCUSABLE 		 	 
     *		COMPONENT_SET_UNFOCUSABLE 			 
	 *		COMPONENT_SET_ERROR 				 
	 *		COMPONENT_SET_NO_ERROR 			     
	 *		COMPONENT_SET_TOOLTIP 			    
	 *		COMPONENT_HILIGHT 			 		 		  
	 *		COMPONENT_NO_HILIGHT 				 	  
	 *		COMPONENT_ENABLE 					 	 
	 *		COMPONENT_DISABLE					  	 							 
	 *		COMPONENT_SHOW 						 		 
	 *		COMPONENT_HIDE 						  
     *      COMPONENT_MASK_EVENTS        
     */
    private boolean execActionForComponent(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
		
    	ForwardPanelComponent panelComponent = null;
    	ForwardForm forwardForm = null;
      	ForwardDoParms actionParmsComponent = null;
    	CardLayout cardLayout = null;
    	InnerForwardPanel innerForwardPanel = null;
    	InnerComponent innerComponent = null;
       	JComponent jcomponent = null;
       	Object value = null;
       	String toolTipText = "";
       	String componentName = "";
       	String panelNameParent = "";
     	boolean isDone = false;
     	
     	// Azione già individuata e eseguota o exception in corso
     	if (mcb.system.getReusableMethodException() != null)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
		
    	switch (actionCoded.action) {
    	
    		// Refresh componente	
			case COMPONENT_REFRESH: 				
				isDone = true;
				if (!mcb.function.isComponentDeclared(actionCoded.componentName)) {break;}
				componentRefreshFromDb(mcb, actionCoded);
				break;

			// Legge il valore del componente GUI  e valorizza una variabile		
			case COMPONENT_GET_VALUE:
				isDone = true;
				if (!mcb.function.isComponentDeclared(actionCoded.componentName)) {break;}
				if (!mcb.function.isVarDeclared(actionCoded.componentValueVarName)) {break;}
				value = mcb.function.getValueControlGUI(actionCoded.componentName);
				mcb.function.setVarValue(actionCoded.componentName, value);
				break;
				
    		// Imposta il valore per il componente GUI 			
			case COMPONENT_SET_VALUE: 				
				isDone = true;
				if (!mcb.function.isComponentDeclared(actionCoded.componentName)) {break;}
				// Valore embedded
				if (actionCoded.componentValue != null) {
					mcb.function.setValueControlGui(actionCoded.componentName, actionCoded.componentValue);
					break;
				}
				// Valore in variabile
				mcb.function.setValueControlGuiFromVar(actionCoded.componentName, actionCoded.componentValueVarName);
				break;

			// Sposta il focus sul componente	
			case COMPONENT_SET_FONT: 				
				isDone = true;
				actionParmsComponent = actionCoded;
				componentName = actionParmsComponent.componentName;
				if (!mcb.function.isComponentDeclared(componentName)) {break;}
				jcomponent = mcb.function.getJComponent(componentName);
				jcomponent.setFont(actionParmsComponent.componentFont);
				break;
				
			// Sposta il focus sul componente		
			case COMPONENT_SET_FOCUS: 				
				isDone = true;
				actionParmsComponent = actionCoded;
				componentName = actionParmsComponent.componentName;
				if (!mcb.function.isComponentDeclared(componentName)) {break;}
				jcomponent = mcb.function.getJComponent(componentName);
				jcomponent.requestFocusInWindow();
				break;
			
			// Il componente può ottenere il focus	
			case COMPONENT_SET_FOCUSABLE: 			
				isDone = true;
				actionParmsComponent = actionCoded;
				componentName = actionParmsComponent.componentName;
				if (!mcb.function.isComponentDeclared(componentName)) {break;}
				jcomponent = mcb.function.getJComponent(componentName);
				jcomponent.setFocusable(actionParmsComponent.componentFocusable);
				break;
			
			// Imposta un campo come in errore	
			case COMPONENT_SET_ERROR: 				
				isDone = true;
				actionParmsComponent = actionCoded;
				componentName = actionParmsComponent.componentName;
				if (!mcb.function.isComponentDeclared(componentName)) {break;}
				panelComponent = mcb.function.getPanelComponent(componentName);
				panelComponent.setWithErrors(actionParmsComponent.componentError);
				break;

			// Imposta il tooltip text al passaggio del mouse
			case COMPONENT_SET_TOOLTIP: 			
				isDone = true;
				actionParmsComponent = actionCoded;
				componentName = actionParmsComponent.componentName;
				toolTipText = actionParmsComponent.toolTipText;
				if (!mcb.function.isComponentDeclared(componentName)) {break;}
				jcomponent = mcb.function.getJComponent(componentName);
				jcomponent.setToolTipText(toolTipText);
				break;

			// Mette in evidenza un controllo (blinking: colore: etc.)
			case COMPONENT_HILIGHT: 				
				actionParmsComponent = actionCoded;
				componentName = actionParmsComponent.componentName;
				//TODO
				break;
				
			// Abilita un componente	
			case COMPONENT_ENABLE: 					
				isDone = true;
				actionParmsComponent = actionCoded;
				componentName = actionParmsComponent.componentName;
				if (!mcb.function.isComponentDeclared(componentName)) {break;}
				jcomponent = mcb.function.getJComponent(componentName);
				jcomponent.setEnabled(actionParmsComponent.componentEnabled);
				break;

			// Rende un componente visibile(form: card: panel: campo)
			case COMPONENT_VISIBLE: 					
				isDone = true;
				actionParmsComponent = actionCoded;
				componentName = actionParmsComponent.componentName;
				// Il componente da trattare è un form di dialogo
				if (mcb.function.getForm(componentName) != null
				&&  mcb.system.isDialogActive()) {
					mcb.system.getDialogActive().setVisible(actionCoded.componentVisible);
					break;
				}
				// Pannello della funzione applicativa corrente
				if (mcb.function.getComponentType(componentName) == EnumForwardComponent.JPanel
				||  mcb.function.getComponentType(componentName) == EnumForwardComponent.JTabbedPane	
				||  mcb.function.getComponentType(componentName) == EnumForwardComponent.JSplitPane) {
					forwardForm = mcb.system.getActiveForm();
					innerForwardPanel = forwardForm.getPanelStructure(componentName);
					panelNameParent = forwardForm.getPanelNameParent(innerForwardPanel.panelName);
					innerForwardPanel = forwardForm.getPanelStructure(panelNameParent);
					// Attivazione logiche prima della prima visualizzazione del pannello
					innerComponent = mcb.function.getComponentDeclared(componentName);
					if (!innerComponent.isPaneFirstShowDone) {
						manageLogicDeclaredActivation(mcb, componentName, EnumForwardEvent.ON_PANEL_BEFORE_FIRST_SHOW);
						innerComponent.isPaneFirstShowDone = true;
					}
					// Card layout: si agisce sul layout manager
					if (innerForwardPanel.layoutManager == EnumForwardLayout.CARD_LAYOUT) {
						cardLayout = innerForwardPanel.panel.getCardLayout();
						cardLayout.show((Container) innerForwardPanel.panel.getGraphicObject(), componentName);
						break;
					}
				}
				// Per qualsiasi altro componente basta renderlo visibile o invisibile
				if (!mcb.function.isComponentDeclared(componentName)) {break;}
				jcomponent = mcb.function.getJComponent(componentName);
				jcomponent.setVisible(actionCoded.componentVisible);
				break;

			// Rende eseguibili/non eseguibili le azioni associate a eventi, per il componente
			case COMPONENT_MASK_EVENTS: 					
				isDone = true;
				actionParmsComponent = actionCoded;
				componentName = actionParmsComponent.componentName;
				if (!mcb.function.isComponentDeclared(componentName)) {break;}
                // Mask false e no eventi specificati: rimozione completa
				if (!actionParmsComponent.componentEventsToMask) {
					mcb.hm_maskedEvents.remove(componentName);
					break;
				}
				// Eventi specificati nella action
				Set<EnumForwardEvent> hs_events = mcb.hm_maskedEvents.get(componentName);
				if (hs_events == null) {
					hs_events = new HashSet<EnumForwardEvent> ();
					mcb.hm_maskedEvents.put(componentName, hs_events);
				}
				// Inserimento/rimozione eventi 
				for (EnumForwardEvent eventToMask : actionParmsComponent.componentMaskedEvents) {
					// Evento da mascherare
					if (actionParmsComponent.componentEventsToMask) {
						hs_events.add(eventToMask);
						continue;
					}
					// Evento attivo, se non esiste no problem
					hs_events.remove(eventToMask);
				}
    	}
    	
		return isDone;
	}

    /* ----------------------------------
     * Gestione refresh componente da db
     * ----------------------------------
     * 
     * Combobox: si effettua il popolamento da db
     * 
     */
    private void componentRefreshFromDb(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
    	
       	ForwardDoParms actionRefresh = null;
		InnerComponent innerComponent = null;
		Scanner scn = null;
		String[] ar_tableJavaFieldsBound = null;
		String[] ar_tableLdvColumnsBound = null;
		ArrayList<String> al_tableJavaFieldsBound = null;
		ArrayList<String> al_tableLdvColumnsBound = null;
		String ldvClassName = "";
		String LdvColBound = "";
		String javaName = "";
		
		mcb.system.setReturnCode(0);
		
		// Lettura decsrittore componente swing 
		innerComponent = mcb.function.getComponentDeclared(actionCoded.componentName);

		// ComboBox
		if (innerComponent.componentType == EnumForwardComponent.JComboBox) {
			ldvClassName = innerComponent.ldvName;
			if (ldvClassName.equals("")) {return;}									// Non è stata definito un DATA_SOURCE() per la list
			if (innerComponent.al_columnBound.size() == 0) {return;}				// Non è stata definita una colonna bound nel DATA_SOURCE() per la list
			LdvColBound = innerComponent.al_columnBound.get(0);						// Impostato con DATA_SOURCE()
	    	actionRefresh = mcb.function.DO_COMBOBOX_POPULATE_FROM_DB(actionCoded.componentName, ldvClassName, LdvColBound);
			mcb.function.ACTION(actionRefresh);
			return;
		}
		 
		// List
		if (innerComponent.componentType == EnumForwardComponent.JList) {
			ldvClassName = innerComponent.ldvName;
			if (ldvClassName.equals("")) {return;}									// Non è stata definito un DATA_SOURCE() per la list
			if (innerComponent.al_columnBound.size() == 0) {return;}				// Non è stata definita una colonna bound nel DATA_SOURCE() per la list
			LdvColBound = innerComponent.al_columnBound.get(0);						// Impostato con DATA_SOURCE()
	    	actionRefresh = mcb.function.DO_LIST_POPULATE_FROM_DB(actionCoded.componentName, ldvClassName, LdvColBound);
			mcb.function.ACTION(actionRefresh);
			return;
		}
		
		// Table
		if (innerComponent.componentType == EnumForwardComponent.JTable) {
			ldvClassName = innerComponent.ldvName;
			if (ldvClassName.equals("")) {return;}									// Non è stata definito un DATA_SOURCE() per la list
			if (innerComponent.al_columnBound.size() == 0) {return;}				// Non è stata definito un DATA_SOURCE() per la list
			LdvColBound = innerComponent.al_columnBound.get(0);						// Impostato con DATA_SOURCE()
			al_tableJavaFieldsBound = new ArrayList<String> ();						// Nome campo java 
			al_tableLdvColumnsBound = new ArrayList<String> ();						// Nome colonna ldv bound
			// Scan columns bound
			for (String columnBound : innerComponent.al_columnBound) {
				scn = new Scanner(columnBound);
				javaName = "";
				LdvColBound = "";
				if (scn.hasNext()) {
					javaName = scn.next();
					if (scn.hasNext()) {
						LdvColBound = scn.next();
					}
				}
				al_tableJavaFieldsBound.add(javaName);
				al_tableLdvColumnsBound.add(LdvColBound);
			}
			
			// Creazione arrays
			ar_tableJavaFieldsBound = new String[al_tableJavaFieldsBound.size()];
			ar_tableLdvColumnsBound = new String[al_tableLdvColumnsBound.size()];
			ar_tableJavaFieldsBound = al_tableJavaFieldsBound.toArray(ar_tableJavaFieldsBound);
			ar_tableLdvColumnsBound = al_tableLdvColumnsBound.toArray(ar_tableLdvColumnsBound);
			
			// Esecuzione populate
	    	actionRefresh = mcb.function.DO_TABLE_POPULATE_FROM_DB(actionCoded.componentName, ldvClassName, ar_tableJavaFieldsBound, ar_tableLdvColumnsBound);
			mcb.function.ACTION(actionRefresh);
			return;
		}
		
	}


	/* -------------------------------------------------------------------------------
     * Action componenti con uno stato ON/OFF (JCheckBox, JRadioButton, JToggleButton)
     * -------------------------------------------------------------------------------
     * 
     * STATE_SET_CHECKED  		String comboBoxName|radioButtonName
     * STATE_SET_UNCHECKED  	String comboBoxName|radioButtonName
     * 
     */
	private boolean execActionForState(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
		
	   	ForwardDoParms actionParmsChecked = null;
		JCheckBox jcheckBox = null;
		JCheckBoxMenuItem jcheckBoxMenuItem = null;
		JRadioButton jradioButton = null;
		JRadioButtonMenuItem jradioButtonMenuItem = null;
		String componentName = "";
		boolean isDone = false;
		
     	// Azione già individuata e eseguota o exception in corso
     	if (mcb.system.getReusableMethodException() != null)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
    	
       	switch (actionCoded.action) {
       	
 			case COMPONENT_SET_CHECKED:  			    // Rende checkbox/JRadioButton checked, JToggleButton toggled	
				isDone = true;
		       	// Parametri codificati in fase di dichiarazione
				actionParmsChecked = actionCoded;
				componentName = actionParmsChecked.componentName;
				if (!mcb.function.isComponentDeclared(componentName)) {break;}
				if (mcb.function.isComponentType(componentName, EnumForwardComponent.JCheckBox)) {
					jcheckBox = mcb.function.getJCheckBox(componentName);
					jcheckBox.setSelected(actionParmsChecked.componentChecked);
					break;
				}
				if (mcb.function.isComponentType(componentName, EnumForwardComponent.JCheckBoxMenuItem)) {
					jcheckBoxMenuItem = mcb.function.getJCheckBoxMenuItem(componentName);
					jcheckBoxMenuItem.setSelected(actionParmsChecked.componentChecked);
					break;
				}
				if (mcb.function.isComponentType(componentName, EnumForwardComponent.JRadioButton)) {
					jradioButton = mcb.function.getJRadioButton(componentName);
					jradioButton.setSelected(true);
					break;
				}
				if (mcb.function.isComponentType(componentName, EnumForwardComponent.JRadioButton)) {
					jradioButtonMenuItem = mcb.function.getJRadioButtonMenuItem(componentName);
					jradioButtonMenuItem.setSelected(actionParmsChecked.componentChecked);
					break;
				}
				break;
      	}
		return isDone;
	}


	/* ---------------------------------
     * Action per comboBox (JComboBox)
     * ---------------------------------
     * 
     * 	COMBOBOX_POPULATE_FROM_DB			 
     * 	COMBOBOX_POPULATE_DISCARD_ITEM		 
     * 	COMBOBOX_POPULATE_MODIFY_ITEM	
     *  COMBOBOX_POPULATE_SET_OBJECT_BOUND	 
     *  COMBOBOX_GET_ORDINAL
     *  COMBOBOX_GET_OBJECT_BOUND
     * 	COMBOBOX_CLEAR 						 
	 * 	COMBOBOX_SELECT_ITEM  				 
	 * 	COMBOBOX_UNSELECT_ITEM  			 
	 * 	COMBOBOX_APPEND_ITEM  				 
	 * 	COMBOBOX_INSERT_ITEM  				 
	 * 	COMBOBOX_DELETE_ITEM   				 
	 * 	COMBOBOX_DELETE_SELECTED_ITEM   	 
	 *  COMBOBOX_UPDATE_SELECTED_ITEM		 
     */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private boolean execActionForComboBox(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
		
		JComboBox jComboBox = null;
		ForwardComboBoxModel forwardComboBoxModel = null;
		ForwardLogicalDataView ldvObject = null;
		ForwardDoParms actionCodedLdv = null;
		InnerOnEvent eventBeforeAddElement = null;
		Object itemObjectBound = null;
		String comboBoxName = "";
		String columnNameSource = "";
		String itemTextValue = "";
		int index = 0;
		int retCode = 0;
		boolean isDone = false;
		
     	// Azione già individuata e eseguita o exception in corso
     	if (mcb.system.getReusableMethodException() != null)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
    	
		switch (actionCoded.action) {
       	
			// Esegue la logical data view e popola la combo box
			case COMBOBOX_POPULATE_FROM_DB: 						
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				forwardComboBoxModel = mcb.function.getPanelComponent(comboBoxName).getComboBoxModel();
				forwardComboBoxModel.removeAllElements();			// items & objects bound, if any
				forwardComboBoxModel._getAllDataBound().clear();	// 	
				manageLogicDeclaredActivation(mcb, comboBoxName, EnumForwardEvent.ON_COMBOBOX_AFTER_CLEAR);			// Per eventuale inserimento items iniziali, di default con relativo oggetto bound o altro
				mcb.system.setLdvName(actionCoded.comboBoxLdvClassName);
				mcb.system.setReturnCode(0);
				// Create, validate in sequenza
				actionCodedLdv = new ForwardDoParms ();
				actionCodedLdv.ldvClassName = actionCoded.comboBoxLdvClassName;
				actionCodedLdv.action = EnumForwardAction.LDV_CREATE;
				retCode = execActionLdvCreateCommon(mcb, actionCodedLdv);
				if (retCode > 0) {manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);break;}
				actionCodedLdv.action = EnumForwardAction.LDV_VALIDATE;
				retCode = execActionLdvValidateCommon(mcb, actionCodedLdv);
				if (retCode > 0) {manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);break;}
				retCode = mcb.system.getReturnCode();
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
 				// Impostazione numero rule table, se ldv di solo accesso a rule table specifica
				if (actionCoded.comboBoxLdvRuleTableNum >= 0) {
					ldvObject.setRuleTableNum(actionCoded.comboBoxLdvRuleTableNum);
				} 
				// Impostazione di default lingua di login
				// Attivazione logiche prima del popolamento della ldv, ottimizzazione recupero evento
			    manageLogicDeclaredActivation(mcb, comboBoxName, EnumForwardEvent.ON_COMBOBOX_BEFORE_POPULATE);
			    manageLogicDeclaredActivation(mcb, actionCoded.comboBoxLdvClassName, EnumForwardEvent.ON_LDV_BEFORE_EXECUTE);
			    if (mcb.system.getReturnCode() != 0) {break;}
				retCode = ldvObject.execute();
				mcb.system.setLdvReturnCode(retCode);
				if (retCode != 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
				if (retCode != 0) {break;}
				mcb.system.setComboBoxEventsMasked(true);
				eventBeforeAddElement = getEventDescriptor(mcb, comboBoxName, EnumForwardEvent.ON_COMBOBOX_BEFORE_ADD_ELEMENT); // Recupero per ottimizzazione evento
				columnNameSource = actionCoded.comboBoxLdvColSource;
				// Scan righe lette dalla logical data view
				for (int i = 0; i < ldvObject.getCountRows(); i++) {
					ldvObject.read(i);												// Variabili disponibili nella ldv
					itemTextValue = ldvObject.getValueString(columnNameSource);
					mcb.system.setComboBoxBoundObject(itemTextValue);   			// Eventuale colonna kdv come oggetto bound
					// Esecuzione ON_COMBOBOX_BEFORE_ADD_ELEMENT
					mcb.system.setComboBoxItemToDiscard(false);
 					mcb.system.setComboBoxBoundObject(null);						// Suppongo nessun oggetto bound da associare all'item
					mcb.system.setComboBoxSelectedItem(itemTextValue);
					if (eventBeforeAddElement != null) {
						logicExecution(mcb, eventBeforeAddElement);
						if (mcb.system.getReturnCode() != 0) {break;}
					}
					// Eventuale item modificato e/o oggetto bound impostato dall'applicazione
					itemTextValue = mcb.system.getComboBoxSelectedItem();			// Eventualmente modificato
					itemObjectBound = mcb.system.getComboBoxBoundObject();			// Eventualmente impostato su ON_COMBOBOX_BEFORE_ADD_ELEMENT
					if (mcb.system.isComboBoxItemToDiscard()) {continue;}			// Eventualmente da scartare
					// Item NON scartato, modificato e/o 
					forwardComboBoxModel._addElement(itemTextValue, itemObjectBound);
				}
				mcb.system.setComboBoxEventsMasked(false);
				mcb.system.getComboBoxEventsMasked().clear();
				manageLogicDeclaredActivation(mcb, comboBoxName, EnumForwardEvent.ON_COMBOBOX_AFTER_POPULATE);
				break;

			// Durante COMBOBOX_POPULATE_FROM_DB, a fronte di ON_COMBOBOX_BEFORE_ADD_ELEMENT, scarta l'elemento corrente  letto da db	
			case COMBOBOX_POPULATE_DISCARD_ITEM:
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				mcb.system.setComboBoxItemToDiscard(true);
				break;
			
			// Durante COMBOBOX_POPULATE_FROM_DB, a fronte di ON_COMBOBOX_BEFORE_ADD_ELEMENT, modifica l'elemento corrente letto da db
			case COMBOBOX_POPULATE_MODIFY_ITEM:	
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				mcb.system.setComboBoxSelectedItem(actionCoded.comboBoxItemText);
				break;
			
			// Durante COMBOBOX_POPULATE_FROM_DB, a fronte di ON_COMBOBOX_BEFORE_ADD_ELEMENT, imposta l'oggetto bound esplicitamente o da colonna ldv
			case COMBOBOX_POPULATE_SET_OBJECT_BOUND:		
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				if (actionCoded.comboBoxItemObjectBound != null) {
					mcb.system.setComboBoxBoundObject(actionCoded.comboBoxItemObjectBound);
				} else {
					itemObjectBound = ((ForwardLogicalDataView)mcb.system.getLdvObject()).getValue(actionCoded.comboBoxLdvColObjectBound);
					if (actionCoded.comboBoxObjectBoundToInteger && itemObjectBound instanceof String) {
						itemObjectBound = StringService._getNumericInt((String)itemObjectBound);
					}
					mcb.system.setComboBoxBoundObject(itemObjectBound);
				}
				break;
				
			// Imposta una variabile di funzione con l'ordina 0-based selezionato della comboBox
			case COMBOBOX_GET_ORDINAL:
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				if (!mcb.function.isVarDeclared(actionCoded.comboBoxOrdinalVarNameReturned)) {break;}
				if (!(mcb.function.getVarValue(actionCoded.comboBoxOrdinalVarNameReturned) instanceof Integer)) {break;}
				if (!actionCoded.comboBoxOrdinalVarNameValue.equals("") &&  !mcb.function.isVarDeclared(actionCoded.comboBoxOrdinalVarNameValue)) {break;}
				if (!actionCoded.comboBoxOrdinalVarNameValue.equals("") &&  !(mcb.function.getVarValue(actionCoded.comboBoxOrdinalVarNameValue) instanceof String)) {break;}
				forwardComboBoxModel = mcb.function.getPanelComponent(comboBoxName).getComboBoxModel();
				// Ordinal da trovare dell'item corrente, del valore embedded o del valore in una variabile
				itemTextValue = (String) forwardComboBoxModel.getSelectedItem();
				if (!actionCoded.comboBoxOrdinalValue.equals("")) {itemTextValue = actionCoded.comboBoxOrdinalValue;}
				if (!actionCoded.comboBoxOrdinalVarNameValue.equals("")) {itemTextValue = (String) mcb.function.getVarValue(actionCoded.comboBoxOrdinalVarNameValue);}
				// Individua l'ordinal
				index = forwardComboBoxModel.getIndexOf(itemTextValue);
				mcb.function.setVarValue(actionCoded.comboBoxOrdinalVarNameReturned, new Integer(index));
				break;

			// Imposta una variabile di funzione con l'oggetto bound
			case COMBOBOX_GET_OBJECT_BOUND:	
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				if (!mcb.function.isVarDeclared(actionCoded.comboBoxVarNameObjectBound)) {break;}
				forwardComboBoxModel = mcb.function.getPanelComponent(comboBoxName).getComboBoxModel();
				itemTextValue = (String) forwardComboBoxModel.getSelectedItem();
				index = forwardComboBoxModel.getIndexOf(itemTextValue);
				itemObjectBound = forwardComboBoxModel._getDataBound(index);
				mcb.function.setVarValue(actionCoded.comboBoxVarNameObjectBound, itemObjectBound);
				break;
				
			// Elimina tutti gli item della comboBox
 			case COMBOBOX_CLEAR: 							
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				forwardComboBoxModel = mcb.function.getPanelComponent(comboBoxName).getComboBoxModel();
				forwardComboBoxModel.removeAllElements();			// items & objects binded, if any
				forwardComboBoxModel._getAllDataBound().clear();
				manageLogicDeclaredActivation(mcb, comboBoxName, EnumForwardEvent.ON_COMBOBOX_AFTER_CLEAR);
				break;
			
			// Seleziona l'item (0-based) della ComboBoX
			case COMBOBOX_SELECT_ITEM: 						
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				index =  actionCoded.comboBoxItemIndex;
				jComboBox = mcb.function.getJComboBox(comboBoxName);
				jComboBox.setSelectedIndex(index);
				break;
			
			// Deseleziona l'item correntemente selezionato della ComboBoX
			case COMBOBOX_UNSELECT_ITEM: 						
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				jComboBox = mcb.function.getJComboBox(comboBoxName);
				jComboBox.setSelectedIndex(-1);
				break;
			
			// Inserisce un item come ultimo elemento della ComboBoX
			case COMBOBOX_APPEND_ITEM: 							
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				itemTextValue = actionCoded.comboBoxItemText;
				itemObjectBound = actionCoded.comboBoxItemObjectBound;
				forwardComboBoxModel = mcb.function.getPanelComponent(comboBoxName).getComboBoxModel();
				forwardComboBoxModel.addElement(itemTextValue);
				forwardComboBoxModel._getAllDataBound().add(itemObjectBound);
				break;
			
			// Inserisce un item prima di un elemento (0-based) della ComboBoX
			case COMBOBOX_INSERT_ITEM: 				
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				itemTextValue = actionCoded.comboBoxItemText;
				itemObjectBound = actionCoded.comboBoxItemObjectBound;
				forwardComboBoxModel = mcb.function.getPanelComponent(comboBoxName).getComboBoxModel();
				jComboBox = mcb.function.getJComboBox(comboBoxName);
		        index = jComboBox.getSelectedIndex(); 			// Indice selezionato (informazione certa a livello di componente)
	            if (index == -1) { // Nessuna selezione, insert all'inizio
	                index = 0;
	            } else {           // Insert dopo item selezionato
	                index++;
	            }
	            forwardComboBoxModel.insertElementAt(itemTextValue, index);
	            forwardComboBoxModel._getAllDataBound().add(index, itemObjectBound);
	        
	         // Elimina un item (0-based) della ComboBoX
			case COMBOBOX_DELETE_ITEM: 				
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				forwardComboBoxModel = mcb.function.getPanelComponent(comboBoxName).getComboBoxModel();
				index =  actionCoded.comboBoxItemIndex;
				forwardComboBoxModel.removeElementAt(index);
				forwardComboBoxModel._getAllDataBound().remove(index);
				break;
			
			// Elimina l'item correntemente selezionato della ComboBoX
			case COMBOBOX_DELETE_SELECTED_ITEM: 				
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				forwardComboBoxModel = mcb.function.getPanelComponent(comboBoxName).getComboBoxModel();
				jComboBox = mcb.function.getJComboBox(comboBoxName);  
				index = jComboBox.getSelectedIndex();		// indice item selezionato 0-based ordinati
				if (index == -1) {break;}
				forwardComboBoxModel.removeElementAt(index);
				forwardComboBoxModel._getAllDataBound().remove(index);
				break;
			
			// Aggiorna l'item selezionato di una lista (0-based)
			case COMBOBOX_UPDATE_SELECTED_ITEM:    		
				isDone = true;
				comboBoxName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(comboBoxName)) {break;}
				if (!mcb.function.isComponentType(comboBoxName, EnumForwardComponent.JComboBox)) {break;}
				jComboBox = mcb.function.getJComboBox(comboBoxName);  
				forwardComboBoxModel = mcb.function.getPanelComponent(comboBoxName).getComboBoxModel();
				index = jComboBox.getSelectedIndex();		// indice item selezionato 0-based 
				if (index == -1) {break;}
				itemTextValue = actionCoded.comboBoxItemText;
				forwardComboBoxModel.setSelectedItem(itemTextValue);
				// Update object bound se richiesto
				if (actionCoded.comboBoxObjectBoundToUpdate) {
					itemObjectBound = actionCoded.comboBoxItemObjectBound;
					itemObjectBound = forwardComboBoxModel._getAllDataBound().set(index, itemObjectBound);
				}
				break;
     	}
		return isDone;
	}


	/* ---------------------------
     * Action  per liste (JList)
     * ---------------------------
     * 
     * 	LIST_POPULATE_FROM_DB 			String listName
     * 	LIST_POPULATE_DISCARD_MODIFY	String listName
     * 	LIST_CLEAR 						String listName
	 * 	LIST_SELECT_ITEMS 				String listName, String itemTextValue  Integer[] index
	 * 	LIST_APPEND_ITEM  				String listName, String itemTextValue  |Object itemObjectBound
	 * 	LIST_INSERT_ITEM  				String listName, String itemTextValue  |Object itemObjectBound
	 * 	LIST_DELETE_ITEMS  				String listName, Integer[] index
	 * 	LIST_DELETE_SELECTED_ITEMS  	String listName 
	 * 	LIST_UPDATE_SELECTED_ITEM  		String listName, String itemTextValue,  |Object itemObjectBound
	 * 	LIST_UPDATE_ITEM  				String listName, String itemTextValue,  Integer index |Object itemObjectBound
	 * 	LIST_MAKE_VISIBLE_ITEM  		String listName, Integer index
     */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private boolean execActionForList(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
		
		JList jlist = null;
		ForwardListModel forwardListModel = null;
		ForwardLogicalDataView ldvObject = null;
      	ForwardDoParms actionCodedLdv = null;
		InnerOnEvent eventBeforeAddElement = null;
		String listName = "";
		String itemTextValue = "";
		String controlNameSource = "";
		Object itemObjectBound = null;
		int ar_selectedIndices[] = null;
		int retCode = 0;
		int index = 0;
		int cntItemDeleted = 0;
		boolean isDone = false;
	
		// Azione già individuata e eseguita o exception in corso
     	if (mcb.system.getReusableMethodException() != null)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
    	
 		switch (actionCoded.action) {
       	
		case LIST_POPULATE_FROM_DB:    					// Esegue la logical data view della list e la utilizza per popolare il modello della list 
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			forwardListModel.clear();			// Rows & objects binded, if any
			manageLogicDeclaredActivation(mcb, listName, EnumForwardEvent.ON_LIST_AFTER_CLEAR);			// Per eventuale inserimento items iuniziali, di default con relativo oggetto bound
			forwardListModel._getAllDataBound().clear();	
		    mcb.system.setLdvName(actionCoded.listLdvClassName);
			mcb.system.setReturnCode(0);
			
			// Create, validate ed execute in seqiuenza
			actionCodedLdv = new ForwardDoParms ();
			actionCodedLdv.ldvClassName = actionCoded.listLdvClassName;
			actionCodedLdv.action = EnumForwardAction.LDV_RUN;
			execActionForLdv(mcb, actionCodedLdv, false);
			retCode = mcb.system.getReturnCode();
			// Errore in Create/Validate/Execute già gestito e con eventuali logiche già richiamate
			if (retCode != 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
			if (retCode != 0) {break;}
			ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
			// Attivazione logiche prima del popolamento e dell'esecuzione ed esecuzione della ldv,
		    manageLogicDeclaredActivation(mcb, listName, EnumForwardEvent.ON_LIST_BEFORE_POPULATE);
		    manageLogicDeclaredActivation(mcb, actionCoded.comboBoxLdvClassName, EnumForwardEvent.ON_LDV_BEFORE_EXECUTE);
		    if (mcb.system.getReturnCode() != 0) {break;}
			retCode = ldvObject.execute();
			mcb.system.setLdvReturnCode(retCode);
			if (retCode != 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
			if (retCode != 0) {break;}
			eventBeforeAddElement = getEventDescriptor(mcb, listName, EnumForwardEvent.ON_LIST_BEFORE_ADD_ELEMENT); // Recupero per ottimizzazione evento
			controlNameSource = actionCoded.listLdvColSource;
			// Scan righe lette dalla logical data view
			for (int i = 0; i < ldvObject.getCountRows(); i++) {
				ldvObject.read(i);									// Variabili disponibili nella ldv
				itemTextValue = ldvObject.getValueString(controlNameSource);
				// Esecuzione ON_LIST_BEFORE_ADD_ELEMENT
				mcb.system.setListItemToDiscard(false);
				mcb.system.setListBoundObject(null);						// Suppongo nessun oggetto bound associato alla riga
				mcb.system.setListSelectedItem(itemTextValue);
				if (eventBeforeAddElement != null) {
					logicExecution(mcb, eventBeforeAddElement);
					if (mcb.system.getReturnCode() != 0) {break;}
				}
				// Eventuale item modificato e/o oggetto bound impostato dall'applicazione 
				itemTextValue = mcb.system.getListSelectedItem();			// Eventualmente modificato
				itemObjectBound = mcb.system.getListBoundObject();			// Eventualmente impostato su ON_LIST_BEFORE_ADD_ELEMENT
				if (mcb.system.isListItemToDiscard()) {continue;}			// Eventualmente da scartare
				// Item NON scartato, modificato e/o 
				forwardListModel._addElement(itemTextValue, itemObjectBound);
			}
			manageLogicDeclaredActivation(mcb, listName, EnumForwardEvent.ON_LIST_AFTER_POPULATE);
			break;

		// Durante LIST_POPULATE_FROM_DB, a fronte di ON_COMBOBOX_BEFORE_ADD_ELEMENT, scarta l'elemento corrente  letto da db	
		case LIST_POPULATE_DISCARD_ITEM:
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			mcb.system.setListItemToDiscard(true);
			break;
			
		// Durante LIST_POPULATE_FROM_DB, a fronte di ON_COMBOBOX_BEFORE_ADD_ELEMENT, modifica l'elemento corrente letto da db
		case LIST_POPULATE_MODIFY_ITEM:	
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			mcb.system.setComboBoxSelectedItem(actionCoded.comboBoxItemText);
			break;
		
		// Durante LIST_POPULATE_FROM_DB, a fronte di ON_COMBOBOX_BEFORE_ADD_ELEMENT, imposta l'oggetto bound
		case LIST_POPULATE_SET_OBJECT_BOUND:		
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			mcb.system.setListBoundObject(actionCoded.listItemObjectBound);
			break;

		// Imposta una variabile di funzione con l'oggetto bound
		case LIST_GET_OBJECT_BOUND:	
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			if (!mcb.function.isVarDeclared(actionCoded.listVarNameObjectBound)) {break;}
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			jlist = mcb.function.getJList(listName);
			index = jlist.getSelectedIndex();
			itemObjectBound = forwardListModel._getDataBound(index);
			mcb.function.setVarValue(actionCoded.listVarNameObjectBound, itemObjectBound);
			break;
			
		// Elimina tutte le righe di una lista
		case LIST_CLEAR:    					
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			forwardListModel.clear();			// Rows & objects binded, if any
			forwardListModel._getAllDataBound().clear();	
			break;
		
		// Seleziona una o più righe di una lista
		case LIST_SELECT_ITEMS:    					
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			jlist = mcb.function.getJList(listName);
			for (int indexToSelect: actionCoded.listItemIndexes) {
				if (indexToSelect < forwardListModel.getSize()) {
					jlist.setSelectedIndex(indexToSelect);
				}
			}
			break;
			
		// Inserisce una riga di alla fine una lista			
		case LIST_APPEND_ITEM:    				
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			itemTextValue = actionCoded.listItemText;
			itemObjectBound = actionCoded.listItemObjectBound;
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			forwardListModel.addElement(itemTextValue);
			forwardListModel._addElement(itemTextValue, itemObjectBound);
			break;

		// Inserisce una riga di una lista
		case LIST_INSERT_ITEM:    				
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			itemTextValue = actionCoded.listItemText;
			itemObjectBound = actionCoded.listItemObjectBound;
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			jlist = mcb.function.getJList(listName);
	        index = jlist.getSelectedIndex(); 	// Indice selezionato (informazione certa a livello di componente)
            if (index == -1) { // Nessuna selezione, insert all'inizio
                index = 0;
            } else {           // Insert dopo item selezionato
                index++;
            }
            forwardListModel._insertElementAt(itemTextValue, itemObjectBound, index);
			break;

		// Elimina gli item specificati di una lista (0-based)
		case LIST_DELETE_ITEMS:    				
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			Arrays.sort(actionCoded.listItemIndexes);
			for (int indexItemToDelete : actionCoded.listItemIndexes) {
				index = indexItemToDelete - cntItemDeleted;
				forwardListModel._remove(index);						// Elimina item e oggetto binded
				cntItemDeleted++;
			}
			break;
			
		// Elimine le righe selezionate di una lista
		case LIST_DELETE_SELECTED_ITEMS:    	
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			jlist = mcb.function.getJList(listName);
			ar_selectedIndices = jlist.getSelectedIndices();			// indici items 0-based ordinati
			for (int indexToDelete : ar_selectedIndices) {
				index = (Integer) indexToDelete - cntItemDeleted;
				forwardListModel._remove(index);						// Elimina item e oggetto binded
				cntItemDeleted++;
			}
			break;
		
		// Aggiorna l'item selezionato di una lista (0-based)
		case LIST_UPDATE_SELECTED_ITEM:    		
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			itemTextValue = actionCoded.listItemText;
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			jlist = mcb.function.getJList(listName);
	        index = jlist.getSelectedIndex(); 					// Indice selezionato (informazione certa a livello di componente)
            if (index == -1) {break; }							// Nessuna selezione, nessuna operazione
            forwardListModel.set(index, itemTextValue);			// Update item
            // Update oggetto binded
            if (actionCoded.listObjectBoundToUpdate) {
	            itemObjectBound = actionCoded.listItemObjectBound;
	            forwardListModel._getAllDataBound().set(index, itemObjectBound);
			}
			break;

		// Aggiorna l'item specificato di una lista (0-based)
		case LIST_UPDATE_ITEM:    					
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			itemTextValue =  actionCoded.listItemText;
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			jlist = mcb.function.getJList(listName);
	        index = actionCoded.listItemIndex; 		// Indice fornito
            forwardListModel.set(index, itemTextValue);
            // Update oggetto binded
            if (actionCoded.listObjectBoundToUpdate) {
	            itemObjectBound = actionCoded.listItemObjectBound;
	            forwardListModel._getAllDataBound().set(index, itemObjectBound);
			}
			break;
		
		// Aggiorna l'item specificato di una lista (0-based)
		case LIST_MAKE_VISIBLE_ITEM:    			// Rende l'item visibile se non rientra nell'area scrollabile 
			isDone = true;
			listName = actionCoded.componentName;
			if (!mcb.function.isComponentDeclared(listName)) {break;}
			if (!mcb.function.isComponentType(listName, EnumForwardComponent.JList)) {break;}
			forwardListModel = mcb.function.getPanelComponent(listName).getListModel();
			jlist = mcb.function.getJList(listName);
			if (index < forwardListModel.getSize()) {
				jlist.ensureIndexIsVisible(index);		
			}
			break;
      	}
		return isDone;
	}


	/* ---------------------------
     * Action per tabelle (JTable)
     * ---------------------------
     * 
     * 	TABLE_POPULATE_FROM_DB					 
     *  TABLE_POPULATE_DISCARD_ROW	
     *  TABLE_POPULATE_SET_OBJECT_BOUND		 
     * 	TABLE_CLEAR 							 
     * 	TABLE_SELECT_ROWS 						 
     * 	TABLE_SELECT_COLS 						 
     *  TABLE_ENABLE_ROWS_SELECTION				 
     *  TABLE_ENABLE_COLS_SELECTION				 
     *  TABLE_ENABLE_CELLS_SELECTION			 
     *  TABLE_MODE_SINGLE_SELECTION				 
     *  TABLE_MODE_SINGLE_INTERVAL_SELECTION	 
     *  TABLE_MODE_MULTIPLE_INTERVAL_SELECTION	 
     *  TABLE_SET_TOOLTIP_COLUMN				 
     *  TABLE_SET_TOOLTIP_CELL					 
     *  TABLE_APPEND_ROW  						 
     *  TABLE_INSERT_ROW  						 
     *  TABLE_DELETE_ALL_ROWS					 
     *  TABLE_DELETE_ROWS						 
     *  TABLE_DELETE_SELECTED_ROWS				 
     *  TABLE_DELETE_SELECTED_ROW               
     */	
	private boolean execActionForTable(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
		
		ForwardTableModel forwardTableModel = null;
		JTable jtable = null;
		ForwardLogicalDataView ldvObject = null;
		ForwardDoParms actionCodedLdv = null;
		InnerOnEvent eventBeforeAddRow = null;
		ArrayList<Object> al_objRow = null;
		ArrayList<Integer> al_objRowNumber = null;
		Object[] ar_objRow = null;
		Object objRowBound = null;
		String tableName = "";
		String toolTipText = "";
		boolean isDone = false;
		boolean isOk = false;
		int columnNumber = 0;
		int rowNumber = 0;
		int rowNumberFrom = 0;
		int rowNumberTo = 0;
		int colNumberFrom = 0;
		int colNumberTo = 0;
		int retCode = 0;
		
     	// Azione già individuata e eseguita o exception in corso
     	if (mcb.system.getReusableMethodException() != null)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
    	
		
       	switch (actionCoded.action) {
       	
		    case TABLE_POPULATE_FROM_DB: 							// Esegue la logical data view della table e la utilizza per popolare il modello della table 
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				forwardTableModel._deleteAllRows();							// Elimina Rows & objects bound, if any
				manageLogicDeclaredActivation(mcb, tableName, EnumForwardEvent.ON_TABLE_AFTER_CLEAR);			// Per eventuale inserimento items iniziali, di default con relativo oggetto bound
			    mcb.system.setLdvName(actionCoded.tableLdvClassName);
				mcb.system.setReturnCode(0);
				// Attivazione logiche prima del popolamento e dell'esecuzione ed esecuzione della ldv, ottimizzazione recupero evento
			    manageLogicDeclaredActivation(mcb, tableName, EnumForwardEvent.ON_TABLE_BEFORE_POPULATE);
				// Create, validate ed execute in sequenza
				actionCodedLdv = new ForwardDoParms ();
				actionCodedLdv.ldvClassName = actionCoded.tableLdvClassName;
				actionCodedLdv.action = EnumForwardAction.LDV_RUN;
				execActionForLdv(mcb, actionCodedLdv, false);
				retCode = mcb.system.getReturnCode();
				// Errore in Create/Validate/Execute già gestito e con eventuali logiche già richiamate
				if (retCode != 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
				if (retCode != 0) {break;}
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
				// Recupero evento prima di inserimento riga 
				mcb.system.setLdvReturnCode(0);
				eventBeforeAddRow = getEventDescriptor(mcb, tableName, EnumForwardEvent.ON_TABLE_BEFORE_ADD_ROW); // Recupero per ottimizzazione evento
				// Scan righe lette dalla logical data view
				for (int i = 0; i < ldvObject.getCountRows(); i++) {
					ldvObject.read(i);													// Variabili disponibili nella ldv
  					mcb.system.setTableRowToDiscard(false);								// Suppongo riga da NON scartare
					mcb.system.setTableBoundObject(null);								// Suppongo oggetto bound NON presente
					if (eventBeforeAddRow != null) {
						logicExecution(mcb, eventBeforeAddRow);
						if (mcb.system.getReturnCode() != 0) {break;}
					}
					// Eventuali variabili sono state modificate direttamente nella logical data view dalle logiche attivate a fronte di ON_LDV_BEFORE_ADD_ROW
					tableUpdColVarsWithLdvColValues(mcb, forwardTableModel, actionCoded, ldvObject); // Variabili di colonna aggiornate con i valori di riga da logical data view
					objRowBound = mcb.system.getTableBoundObject();						// Eventualmente impostato
					if (mcb.system.isTableRowToDiscard()) {continue;}					// Eventualmente da scartare
					// Item NON scartato, modificato e/o 
					al_objRow = new ArrayList<Object> ();
					// Scan colonne tabella
					for (ForwardTableModelColumn tableColumn : forwardTableModel._getColumns()) {
						al_objRow.add(getVarValue(mcb, tableColumn.getName()));					// Valore cella
					}
					forwardTableModel._appendRow(al_objRow, objRowBound);
				}
				manageLogicDeclaredActivation(mcb, tableName, EnumForwardEvent.ON_TABLE_AFTER_POPULATE);
				break;
				
			// Durante TABLE_POPULATE_FROM_DB, a fronte di ON_TABLE_BEFORE_ADD_ROW, scarta l'elemento corrente  letto da db	
			case TABLE_POPULATE_DISCARD_ROW:
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				mcb.system.setTableRowToDiscard(true);
				break;
				
				
			// Durante TABLE_POPULATE_FROM_DB, a fronte di ON_TABLE_BEFORE_ADD_ROW, modifica l'elemento corrente letto da db
			case TABLE_POPULATE_MODIFY_COLUMN:	
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				mcb.system.setComboBoxSelectedItem(actionCoded.comboBoxItemText);
				break;
				
			// Durante TABLE_POPULATE_FROM_DB, a fronte di ON_TABLE_BEFORE_ADD_ROW, imposta l'oggetto bound da colonna ldv o da valore embedded
			case TABLE_POPULATE_SET_OBJECT_BOUND:		
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
	 			if (actionCoded.tableRowObjectBound != null) {
					mcb.system.setTableBoundObject(actionCoded.tableRowObjectBound);
				} else {
					objRowBound = ((ForwardLogicalDataView)mcb.system.getLdvObject()).getValue(actionCoded.tableLdvColObjectBound);
					mcb.system.setTableBoundObject(objRowBound);
				}
				break;

			// Imposta una variabile di funzione con l'oggetto bound
			case TABLE_GET_OBJECT_BOUND:	
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				if (!mcb.function.isVarDeclared(actionCoded.tableVarNameObjectBound)) {break;}
				forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				jtable = mcb.function.getJTable(tableName);
				rowNumber = jtable.getSelectedRow();
				objRowBound = forwardTableModel._getDataBoundAtRow(rowNumber);
				mcb.function.setVarValue(actionCoded.tableVarNameObjectBound, objRowBound);
				break;
				
			// Azzera definizione e righe tabella, incluse gli oggetti bound
		    case TABLE_CLEAR: 							
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				forwardTableModel._clear();			// Elimina Rows & objects binded, if any
				manageLogicDeclaredActivation(mcb, tableName, EnumForwardEvent.ON_TABLE_AFTER_CLEAR);
				break;

			// Seleziona una o più righe di una tabella
		    case TABLE_SELECT_ROWS: 					
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
		    	jtable.setRowSelectionAllowed(true);	
		    	jtable.setColumnSelectionAllowed(false);
				forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				rowNumberFrom = -1;
				rowNumberTo = -1;
				if (actionCoded.tableRowIndexes.length >= 1) {rowNumberFrom = actionCoded.tableRowIndexes[0];}
				if (actionCoded.tableRowIndexes.length > 1)  {rowNumberTo = actionCoded.tableRowIndexes[1];}
				if (rowNumberFrom >= 0 && rowNumberFrom >= forwardTableModel.getRowCount()) {break;}
				if (rowNumberTo >= 0 && rowNumberTo >= forwardTableModel.getRowCount()) {break;}
				// Selezione singola riga
				if (rowNumberFrom >= 0 && rowNumberTo < 0) {
					jtable.getSelectionModel().setSelectionInterval(rowNumberFrom, rowNumberFrom);
					break;
				}
				// Selezione intervallo di righe
				if (rowNumberFrom >= 0 && rowNumberTo >= 0) {
					jtable.getSelectionModel().setSelectionInterval(rowNumberFrom, rowNumberTo);
					break;
				}
				break;

			// Seleziona una o più colonne di una tabella
		    case TABLE_SELECT_COLS: 					
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
		    	jtable.setRowSelectionAllowed(false);	
		    	jtable.setColumnSelectionAllowed(true);
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				if (actionCoded.tableSelectionColFrom >= forwardTableModel.getColumnCount()) {break;}
				if (actionCoded.tableSelectionColTo >= forwardTableModel.getColumnCount()) {break;}
				jtable.setColumnSelectionInterval(actionCoded.tableSelectionColFrom, actionCoded.tableSelectionColTo);
				break;
		    	
			// Elimina la selezione di righe e colonne
			case TABLE_CLEAR_SELECTION:      				
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				jtable.clearSelection();
				mcb.system.setTableSelectedColumnCount(0);
				mcb.system.setTableSelectedRowCount(0);
		    	break;

		    // Attiva la selezione  di una singola riga/colonna
		    case TABLE_ENABLE_SELECTION_SINGLE: 			
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
		    	jtable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		    	break;

		    // Attiva la selezione di un intervallo di righe/colonne
		    case TABLE_ENABLE_SELECTION_SINGLE_INTERVAL: 	
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
		    	jtable.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		    	break;
	
		    // Attiva la selezione di intervalli multipli di righe/colonne
		    case TABLE_ENABLE_SELECTION_MULTIPLE_INTERVAL:
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
		    	jtable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		    	break;
	
		    // Attiva la selezione di righe
		    case TABLE_ENABLE_SELECTION_ROWS: 			
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
		    	jtable.setRowSelectionAllowed(actionCoded.tableSelectionRows);  // true/false
		    	break;

		    // Attiva la selezione di colonne
		    case TABLE_ENABLE_SELECTION_COLS: 			
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
		    	jtable.setColumnSelectionAllowed(mcb.system.getActiveDoParms().tableSelectionCols);  // true/false
		    	break;
		    	
		    // Attiva la selezione di celle    	
 		    case TABLE_ENABLE_SELECTION_CELLS: 			
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
		    	jtable.setRowSelectionAllowed(true);
		    	jtable.setColumnSelectionAllowed(true);
		    	break;

		    // Imposta il tooltip di colonna nel modello, già predisposto per la visualizzazione
		    case TABLE_SET_COLUMN_TOOLTIP: 				
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				toolTipText = actionCoded.tableColToolTipText;
				columnNumber = actionCoded.tableColIndex;
				if (!actionCoded.tableColumnName.equals("")) {
					columnNumber = forwardTableModel._getColumnIndex(actionCoded.tableColumnName);
				}
				if (columnNumber < 0) {break;}
				if (columnNumber >= forwardTableModel._getColumns().size()) {break;}
		    	forwardTableModel._getColumn(columnNumber).setToolTipText(toolTipText);
		    	break;

		    // Imposta il tooltip di cell nel modello, già predisposto per la visualizzazione
		    case TABLE_SET_CELL_TOOLTIP: 				
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				toolTipText = actionCoded.tableCellToolTipText;
				columnNumber = actionCoded.tableColIndex;
				rowNumber = actionCoded.tableRowIndex;
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
		    	if (rowNumber >= forwardTableModel.getRowCount() || columnNumber >= forwardTableModel.getColumnCount()) {break;}
		    	forwardTableModel._setCellToolTipText(rowNumber, columnNumber, toolTipText);
		    	break;

		    // Imposta la proprità dio rsize della colonna a true/false
		    case TABLE_SET_COLUMN_RESIZABLE: 				
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				colNumberFrom = actionCoded.tableColumnResizableNumStart;
				colNumberTo =  actionCoded.tableColumnResizableNumEnd;
				if (colNumberFrom > 0 && colNumberFrom >= forwardTableModel.getColumnCount()) {break;}
				if (colNumberTo > 0 && colNumberTo >= forwardTableModel.getColumnCount()) {break;}
				// Colonna specifica
				if (!actionCoded.tableColumnName.equals("") 
				|| (colNumberFrom >= 0 && colNumberTo == -1)) {
					columnNumber = colNumberFrom;
					if (!actionCoded.tableColumnName.equals("") ) {columnNumber = forwardTableModel._getColumnIndex(actionCoded.tableColumnName);}
				    if (columnNumber < 0) {break;}
					if (columnNumber >= forwardTableModel._getColumns().size()) {break;}
			    	forwardTableModel._getColumn(columnNumber).setResizable(actionCoded.tableColumnResizable);
			    	forwardTableModel._getColumn(columnNumber).applyResizable();
			    	break;
				}
	            // Range di colonne
				if (actionCoded.tableColumnName.equals("")  && colNumberFrom >= 0 && colNumberTo > 0 && colNumberTo > colNumberFrom) {
					for (int i = colNumberFrom; i <= colNumberTo; i++) {
						forwardTableModel._getColumn(i).setResizable(actionCoded.tableColumnResizable);
						forwardTableModel._getColumn(i).applyResizable();
					}
					break;
				}
				// Colonne selezionate o tutte
				if (colNumberFrom == -1 && colNumberTo == -1 && actionCoded.tableColumnName.equals("")) {
					// Selezionate
					jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
					if (actionCoded.tableColumnResizableOnlySelectedCols == true) {
						if (colNumberFrom < 0 && colNumberTo < 0) {
							for (int i : jtable.getSelectedColumns()) {
								forwardTableModel._getColumn(i).setResizable(actionCoded.tableColumnResizable);
								forwardTableModel._getColumn(i).applyResizable();	
							}
						}
						break;
					}
			    	// Tutte
					for (int i = 0; i < forwardTableModel.getColumnCount(); i++) {
						forwardTableModel._getColumn(i).setResizable(actionCoded.tableColumnResizable);
						forwardTableModel._getColumn(i).applyResizable();
					}
				}
		    	break;

		    // Imposta la colonna editabile o no
		    case TABLE_SET_COLUMN_EDITABLE:                
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				colNumberFrom = actionCoded.tableColumnEditableNumStart;
				colNumberTo =  actionCoded.tableColumnEditableNumEnd;
				if (colNumberFrom > 0 && colNumberFrom >= forwardTableModel.getColumnCount()) {break;}
				if (colNumberTo > 0 && colNumberTo >= forwardTableModel.getColumnCount()) {break;}
				// Colonna specifica
				if (!actionCoded.tableColumnName.equals("") 
				|| (colNumberFrom >= 0 && colNumberTo == -1)) {
					columnNumber = colNumberFrom;
					if (!actionCoded.tableColumnName.equals("") ) {columnNumber = forwardTableModel._getColumnIndex(actionCoded.tableColumnName);}
				    if (columnNumber < 0) {break;}
					if (columnNumber >= forwardTableModel._getColumns().size()) {break;}
			    	forwardTableModel._getColumn(columnNumber).setEditable(actionCoded.tableColumnEditable);
			    	break;
				}
	            // Range di colonne
				if (actionCoded.tableColumnName.equals("")  && colNumberFrom >= 0 && colNumberTo > 0 && colNumberTo > colNumberFrom) {
					for (int i = colNumberFrom; i <= colNumberTo; i++) {forwardTableModel._getColumn(i).setEditable(actionCoded.tableColumnEditable);}
					break;
				}
				// Colonne selezionate o tutte
				if (colNumberFrom == -1 && colNumberTo == -1 && actionCoded.tableColumnName.equals("")) {
					// Selezionate
					jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
					if (actionCoded.tableColumnEditableOnlySelectedCols == true) {
						if (colNumberFrom < 0 && colNumberTo < 0) {
							for (int i : jtable.getSelectedColumns()) {forwardTableModel._getColumn(i).setEditable(actionCoded.tableColumnEditable);}
						}
						break;
					}
			    	// Tutte
					for (int i = 0; i < forwardTableModel.getColumnCount(); i++) {forwardTableModel._getColumn(i).setEditable(actionCoded.tableColumnEditable);}
				}
		    	break;

		    // Imposta la colonna nascosta oppure no
		    case TABLE_SET_COLUMN_HIDDEN:                  	
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				colNumberFrom = actionCoded.tableColumnHiddenNumStart;
				colNumberTo =  actionCoded.tableColumnHiddenNumEnd;
				if (colNumberFrom > 0 && colNumberFrom >= forwardTableModel.getColumnCount()) {break;}
				if (colNumberTo > 0 && colNumberTo >= forwardTableModel.getColumnCount()) {break;}
				// Colonna specifica
				if (!actionCoded.tableColumnName.equals("") 
				|| (colNumberFrom >= 0 && colNumberTo < 0)) {
					columnNumber = colNumberFrom;
					if (!actionCoded.tableColumnName.equals("") ) {columnNumber = forwardTableModel._getColumnIndex(actionCoded.tableColumnName);}
				    if (columnNumber < 0) {break;}
					if (columnNumber >= forwardTableModel._getColumns().size()) {break;}
			    	forwardTableModel._getColumn(columnNumber).setHidden(actionCoded.tableColumnHidden);
			    	break;
				}
	            // Range di colonne
				if (colNumberFrom > 0 && colNumberTo > 0 && colNumberTo > colNumberFrom) {
					for (int i = colNumberFrom; i <= colNumberTo; i++) {forwardTableModel._getColumn(i).setHidden(actionCoded.tableColumnHidden);}
					break;
				}
				// Colonne selezionate
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				if (colNumberFrom < 0 && colNumberTo < 0) {
					for (int i : jtable.getSelectedColumns()) {forwardTableModel._getColumn(i).setHidden(actionCoded.tableColumnHidden);}
				}
				break;

			// Imposta la dimensione in pixel della colonna
		    case TABLE_SET_COLUMN_WIDTH:                  	
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
		    	// Set width colonne selezionate
		    	if (actionCoded.tableColIndex == -1 && actionCoded.tableColumnName.equals("")) {
					jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
                    for (int numCol : jtable.getSelectedColumns()) {
           		    	forwardTableModel._getColumn(numCol).setWidth(actionCoded.tableColumnWidth);
  					}
					break;
				}
				columnNumber = actionCoded.tableColIndex;
		    	// Column width per nome colonna o numero
				if (columnNumber < 0) {columnNumber = forwardTableModel._getColumnIndex(actionCoded.tableColumnName);}
				if (columnNumber < 0) {break;}
				if (columnNumber >= forwardTableModel._getColumns().size()) {break;}
		    	forwardTableModel._getColumn(columnNumber).setWidth(actionCoded.tableColumnWidth);
		    	break;

		    // Imposta il margine in pixel fra le colonne
		    case TABLE_SET_COLUMN_MARGIN:                  
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				jtable.getColumnModel().setColumnMargin(actionCoded.tableColumnMargin);
		    	break;

		    // Imposta la dimensione in pixel delle righe
		    case TABLE_SET_ROW_HEIGHT:                  	
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
		    	rowNumberFrom = actionCoded.tableRowHeightNumStart;
		    	rowNumberTo = actionCoded.tableRowHeightNumEnd;
		    	if (rowNumberFrom >= 0 && rowNumberFrom >= forwardTableModel.getRowCount()) {break;}
		    	if (rowNumberTo >= 0 && rowNumberTo >= forwardTableModel.getRowCount()) {break;}
		    	// Height su singola riga
		    	if (rowNumberFrom >= 0 && rowNumberTo <= 0) {
		    		jtable.setRowHeight(rowNumberFrom, actionCoded.tableRowHeight);
		    		break;
		    	}
		    	// Height su range di righe
		    	if (rowNumberFrom >= 0 && rowNumberTo > 0 && rowNumberTo > rowNumberFrom) {
					for (int i = rowNumberFrom; i <= rowNumberTo; i++) {
						jtable.setRowHeight(i, actionCoded.tableRowHeight);
					}
		    		break;
		    	}
		    	// Height solo su righe correntemente selezionate
				if (rowNumberFrom== -1 && rowNumberTo == -1 && actionCoded.tableRowHeightOnlySelectedRows) {
					for (int numRowSelected : jtable.getSelectedRows()) {
						jtable.setRowHeight(numRowSelected, actionCoded.tableRowHeight);
					}
					break;
				} 
		    	// Height su tutte le righe  
				if (rowNumberFrom== -1 && rowNumberTo == -1 && !actionCoded.tableRowHeightOnlySelectedRows) {
					for (int i = 0; i < forwardTableModel.getRowCount(); i++) {
						jtable.setRowHeight(i, actionCoded.tableRowHeight);
					}
					break;
				} 
		    	break;

		    // Imposta il margine in pixel fra le righe
		    case TABLE_SET_ROW_MARGIN:                  	
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				jtable.setRowMargin(actionCoded.tableRowMargin);
		    	break;

		    // Imposta il colore foreground di selezione
		    case TABLE_SET_GRID_COLOR:      				
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				jtable.setGridColor(actionCoded.tableGridColor);
		    	break;

		    // Imposta il colore foreground di selezione
		    case TABLE_SET_SELECTION_FOREGROUND_COLOR:      
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				jtable.setSelectionForeground(actionCoded.tableSelectionForegroundColor);
		    	break;
		  
		    // Imposta il colore background di selezione
		    case TABLE_SET_SELECTION_BACKGROUND_COLOR:      
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				jtable.setSelectionBackground(actionCoded.tableSelectionBackgroundColor);
		    	break;
	
		    // Imposta la griglia con line verticali e orizzontali visibile
		    case TABLE_SET_VERTICAL_LINES_VISIBLE:      	
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				jtable.setShowVerticalLines(actionCoded.tableVerticalLinesVisible);				// true/false
		    	break;

		    // Imposta la griglia con line orizzontali visibili
		    case TABLE_SET_HORIZONTAL_LINES_VISIBLE:      	
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				jtable.setShowHorizontalLines(actionCoded.tableHorizontalLinesVisible);			// true/false
		    	break;

		    // Imposta la griglia con line verticali e orizzontali visibili
		    case TABLE_SET_GRID_VISIBLE:      				
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();	// true/false
				jtable.setShowGrid(actionCoded.tableGridVisible);
		    	break;

		    // Riempe o meno tutta l'area con la tabella
		    case TABLE_SET_FILLS_VIEWPORT_HEIGH:      		
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				jtable.setFillsViewportHeight(actionCoded.tableFillViewPortHeight);			// true/false
		    	break;

		    // Inizia l'editing dui una cella
		    case TABLE_EDIT_CELL_AT:      						
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				if (actionCoded.tableRowIndex >= jtable.getRowCount()) {break;}
				if (actionCoded.tableColIndex >= jtable.getColumnCount()) {break;}
				jtable.editCellAt(actionCoded.tableRowIndex, actionCoded.tableColIndex);
		    	break;

		    // Aggiorna i campi swing con le variabili correnti di colonna
		    case TABLE_REFRESH_PANEL_FROM_VARS:      						
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				forwardTableModel = (ForwardTableModel) jtable.getModel();
				for (ForwardTableModelColumn tableColumn : forwardTableModel._getColumns()) {
					updatePanelsControlsOrTableRowVars(mcb, tableColumn, true);
				}
				updatePanelControlsOrTableRowBoundVar(mcb, tableName, forwardTableModel, true);
				break;

			// Aggiorna le variabili correnti di colonna e bound con i campi swing dichiarati nei pannelli
		    case TABLE_REFRESH_VARS_FROM_PANEL:      							
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				forwardTableModel = (ForwardTableModel) jtable.getModel();
				for (ForwardTableModelColumn tableColumn : forwardTableModel._getColumns()) {
					updatePanelsControlsOrTableRowVars(mcb, tableColumn, false);
				}
				updatePanelControlsOrTableRowBoundVar(mcb, tableName, forwardTableModel, false);
				break;
				 
			// Accoda una riga in una tabella	
		    case TABLE_APPEND_ROW: 							
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				// Append con i valori correnti delle VAR() o con quelli forniti in input
		    	if (actionCoded.tableRowObjects == null) {
		    		ar_objRow = tableGetVarObjects(mcb, forwardTableModel);
				} else {
			    	ar_objRow =  actionCoded.tableRowObjects;
			    	if (ar_objRow.length != forwardTableModel.getColumnCount()) {break;}		// Numero colonne insufficienti
				}
		    	// Valorizzazione object bound
		    	if (actionCoded.tableObjectBoundFromVar) {
		    		objRowBound = this.getVarValue(mcb, forwardTableModel._getVarNameDataBound(), EnumForwardScope.SCOPE_FUNCTION);
				} else {
					objRowBound = actionCoded.tableRowObjectBound;
				}
		    	// Append riga tabella
				al_objRow = new ArrayList<Object> ();
                for (int i = 0; i < forwardTableModel._getColumns().size(); i++) {
                	if (ar_objRow[i].getClass() != forwardTableModel._getColumns().get(i).getClassType()) {break;}  // tipo oggetto colonna diverso da quello definito
                  	al_objRow.add(ar_objRow[i]);
 				}	
   		    	forwardTableModel._appendRow(al_objRow, objRowBound);
		    	break;

		    // Inserisce una riga di una tabella alla riga specificata
		    case TABLE_INSERT_ROW: 						
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				ar_objRow =  actionCoded.tableRowObjects;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				// Insert con i valori correnti delle VAR() o con quelli forniti in input
		    	if (actionCoded.tableRowObjects == null) {
		    		ar_objRow = tableGetVarObjects(mcb, forwardTableModel);
				} else {
			    	ar_objRow =  actionCoded.tableRowObjects;
			    	if (ar_objRow.length != forwardTableModel.getColumnCount()) {break;}		// Numero colonne insufficienti
				}
		    	// Valorizzazione object bound
		    	if (actionCoded.tableObjectBoundFromVar) {
		    		objRowBound = this.getVarValue(mcb, forwardTableModel._getVarNameDataBound(), EnumForwardScope.SCOPE_FUNCTION);
				} else {
					objRowBound = actionCoded.tableRowObjectBound;
				}
		    	// Insert riga tabella
				al_objRow = new ArrayList<Object> ();
                for (int i = 0; i < forwardTableModel._getColumns().size(); i++) {
                	if (ar_objRow[i].getClass() != forwardTableModel._getColumns().get(i).getClassType()) {break;}  // tipo oggetto colonna diverso da quello definito
                  	al_objRow.add(ar_objRow[i]);
 				}	
				rowNumber = actionCoded.tableRowIndex;
  		    	forwardTableModel._insertRow(rowNumber, al_objRow, objRowBound);
  		    	break;

  		   // Delete di tutte le righe di una tabella e del corrispondente oggetto binded
		    case TABLE_DELETE_ALL_ROWS: 				
				isDone = true;
		       	// Parametri codificati in fase di dichiarazione
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				forwardTableModel._deleteAllRows();			// Elimina Rows & objects binded, if any
				break;

			// Delete le righe specificate di una tabella (0-based)
		    case TABLE_DELETE_ROWS: 					
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
		    	al_objRowNumber = new ArrayList<Integer>();
                for (int i = 0; i < actionCoded.tableRowIndexes.length; i++) {
     				rowNumber = actionCoded.tableRowIndexes[i];
     				if (rowNumber >= forwardTableModel.getRowCount()) {continue;}  			// Numero riga out of range
                	al_objRowNumber.add(rowNumber);
 				}				
   		    	forwardTableModel._deleteRows(al_objRowNumber);	// Delete rows e fire listeners
		    	break;

		    // Elimine le righe selezionate di una tabella
		    case TABLE_DELETE_SELECTED_ROWS: 			
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
		    	al_objRowNumber = new ArrayList<Integer>();
                for (int rowSelected : mcb.system.getTableSelectedRows()) {
                	al_objRowNumber.add(rowSelected);
 				}				
   		    	forwardTableModel._deleteRows(al_objRowNumber);	// Delete rows selected e fire listeners
		    	break;

		    // Aggiorna la righe di una tabella (0-based)
		    case TABLE_UPDATE_ROW: 						
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				rowNumber = actionCoded.tableRowIndex;
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				ar_objRow =  actionCoded.tableRowObjects;
                objRowBound = actionCoded.tableRowObjectBound;
				// Update con i valori correnti delle VAR() o con quelli forniti in input
		    	if (actionCoded.tableRowObjects == null) {
		    		ar_objRow = tableGetVarObjects(mcb, forwardTableModel);
				} else {
			    	ar_objRow =  actionCoded.tableRowObjects;
			    	if (ar_objRow.length != forwardTableModel.getColumnCount()) {break;}		// Numero colonne insufficienti
				}
		    	// Valorizzazione object bound
		    	if (actionCoded.tableObjectBoundFromVar) {
		    		objRowBound = this.getVarValue(mcb, forwardTableModel._getVarNameDataBound(), EnumForwardScope.SCOPE_FUNCTION);
				} else {
					objRowBound = actionCoded.tableRowObjectBound;
				}
		    	// Creazione arrayList per update riga specifica
				al_objRow = new ArrayList<Object> ();
				isOk = true;
                for (int i = 0; i < forwardTableModel._getColumns().size(); i++) {
                	if (ar_objRow[i].getClass() != forwardTableModel._getColumns().get(i).getClassType()) {
                		isOk = false;
                		break;
                	}
                  	al_objRow.add(ar_objRow[i]);
 				}
                if (!isOk) {break;}
                // Update con o senza oggetto bound
                if (actionCoded.tableObjectBoundToUpdate) {
      		    	forwardTableModel._updateRow(rowNumber, al_objRow, objRowBound);
				} else {
					forwardTableModel._updateRow(rowNumber, al_objRow);
				}
 		    	break;

 		    // Aggiorna la righe di una tabella (0-based)
		    case TABLE_UPDATE_SELECTED_ROWS: 						
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
				// Update con i valori correnti delle VAR() o con quelli forniti in input
		    	if (actionCoded.tableRowObjects == null) {
		    		ar_objRow = tableGetVarObjects(mcb, forwardTableModel);
				} else {
			    	ar_objRow =  actionCoded.tableRowObjects;
			    	if (ar_objRow.length != forwardTableModel.getColumnCount()) {break;}		// Numero colonne insufficienti
				}
		    	// Valorizzazione object bound
		    	if (actionCoded.tableObjectBoundFromVar) {
		    		objRowBound = this.getVarValue(mcb, forwardTableModel._getVarNameDataBound(), EnumForwardScope.SCOPE_FUNCTION);
				} else {
					objRowBound = actionCoded.tableRowObjectBound;
				}
		    	// Creazione arrayList per update righe
				al_objRow = new ArrayList<Object> ();
                for (int i = 0; i < forwardTableModel._getColumns().size(); i++) {
                	if (ar_objRow[i].getClass() != forwardTableModel._getColumns().get(i).getClassType()) {break;}
                  	al_objRow.add(ar_objRow[i]);
 				}
		    	// Scan righe selezionate
		    	for (int rowSelected : mcb.system.getTableSelectedRows()) {
                    // Update con o senza oggetto bound
                    if (actionCoded.tableObjectBoundToUpdate) {
          		    	forwardTableModel._updateRow(rowSelected, al_objRow, objRowBound);
    				} else {
    					forwardTableModel._updateRow(rowSelected, al_objRow);
    				}
  				}	
   		    	break;

   		    //  Aggiorna la la cella individuata da numero riga e nome colonna
		    case TABLE_UPDATE_ROW_COLUMN_CELL: 							
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
		    	if (actionCoded.tableColIndex == -1) {
		    		actionCoded.tableColIndex = forwardTableModel._getColumnIndex(actionCoded.tableColumnName);
				}
		    	if (actionCoded.tableRowIndex >= forwardTableModel.getRowCount()) {break;}
		    	if (actionCoded.tableColIndex >= forwardTableModel.getColumnCount()) {break;}
		    	if (actionCoded.tableColIndex < 0) {break;}
	      		forwardTableModel.setValueAt(actionCoded.tableCellObject, actionCoded.tableRowIndex, actionCoded.tableColIndex);   	// Update cell e fire listeners
		    	break;

		    //  Aggiorna l'oggetto bound alla riga fornita
		    case TABLE_UPDATE_ROW_BOUND_OBJECT: 						
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	forwardTableModel = mcb.function.getPanelComponent(tableName).getTableModel();
		    	if (actionCoded.tableRowIndex >= forwardTableModel.getRowCount()) {break;}
		    	forwardTableModel._setDataBoundAtRow(actionCoded.tableRowIndex, actionCoded.tableRowObjectBound);
		    	break;

		    // Stampa la tabella
		    case TABLE_PRINT:      									
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
		    	// TODO
		    	break;

		    // Imposta la dimensione della colonna per adattarsi al contenuto della colonna
		    case TABLE_SET_COLUMN_WIDTH_TO_FIT_CONTENT:       		
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				// TODO
		    	break;

		    // Durante il popolamento di una tabella: scarta la riga corrente in fase di caricamento
		    case TABLE_ROW_DISCARD:      				
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				// TODO
		    	break;

		    // Visualizza la prima pagina della tabella
			case TABLE_SHOW_FIRST_PAGE:        			
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				// TODO
		    	break;

		    // Visualizza l'ultima pagina della tabella
		    case TABLE_SHOW_LAST_PAGE:      				
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				// TODO
		    	break;

		    // Visualizza la pagina successiva tabella
		    case TABLE_SHOW_NEXT_PAGE:      				
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				// TODO
		    	break;

		    // Visualizza la pagina precedente della tabella
		    case TABLE_SHOW_PREV_PAGE:      				
				isDone = true;
				tableName = actionCoded.componentName;
				if (!mcb.function.isComponentDeclared(tableName)) {break;}
				if (!mcb.function.isComponentType(tableName, EnumForwardComponent.JTable)) {break;}
				jtable = (JTable) mcb.function.getPanelComponent(tableName).getGraphicObject();
				// TODO
		    	break;
       	}
       	
       	

		return isDone;
	}
    
	/*
	 * Aggiorna le variabili di colonna con i valori delle colonne della ldv con lo stesso nome.
	 * 
	 * Se nella Action sono specificati degli alias per il nome della colonna, vemgono usati quelli
	 */
    private void tableUpdColVarsWithLdvColValues(InnerMonitorControBlock mcb, ForwardTableModel forwardTableModel, ForwardDoParms actionCoded, ForwardLogicalDataView ldvObject) {
       	String columnName = "";
       	String columnNameLdv = "";
       	
		// Update variabili di colonna tabella con valori correnti logical data view
		for (ForwardTableModelColumn tableColumn : forwardTableModel._getColumns()) {
			columnName = tableColumn.getName();
			columnNameLdv = columnName;
			for (int i = 0; i < actionCoded.tableJavaFieldsBound.length; i++) {
				if (actionCoded.tableJavaFieldsBound[i].equals(columnName)) {
					columnNameLdv = actionCoded.tableLdvColumnsBound[i];
					break;
				}
			}
			
			// Colonna non esposto dalla logical data view
			if (!ldvObject.isVarDeclared(columnNameLdv)) {
				continue;
			}
			
			// Update variabile (se di tipo compatibile)
			setVarValue(mcb, columnNameLdv, ldvObject.getValue(columnNameLdv));
			
		}
		
	}


	/*
     * Restituisce le variabili (valorizzate ad ogni selezione di riga) di colonna della tabella come un array di oggetti
     */
	private Object[] tableGetVarObjects(InnerMonitorControBlock mcb, ForwardTableModel forwardTableModel) {
		Object ar_var[] = null;
		Object varObject = null;

		ar_var = new Object[forwardTableModel._getColumns().size()];
        for (int i = 0; i < forwardTableModel._getColumns().size(); i++) {
    		varObject = this.getVarValue(mcb, forwardTableModel._getColumns().get(i).getName(), EnumForwardScope.SCOPE_FUNCTION); // Variabile runtime con lo stesso nome della colonna della tabella: è stata definita a fronte di DATA_VALUES() della JTable 
    		ar_var[i] = varObject;
        }	
		return ar_var;
	}


	/*
	 * Update controllo swing con il valore della variabile di colonna 
     *   Recupero valore variabile corrente
     *   Update componente GUI se definita con lo stesso nome ed è compatibile con il tipo variabile
 	 * Update variabile di colonna con valore controllo swing 
 	 *   Recupero valore controllo swing
 	 *   Update valore variabile corrente
     */
 	private void updatePanelsControlsOrTableRowVars(InnerMonitorControBlock mcb, ForwardTableModelColumn tableColumn, boolean isSwingControlToUpdate) {
 		Class<?> columnClass = null;
		Object varObject = null;
		InnerComponent innerComponent = null;
		
		// Lettura componente swing con il nome della colonna della tabella
		innerComponent = mcb.function.getComponentDeclared(tableColumn.getName());

		// Nessun componente swing definito con lo stesso nome: nessuna operazione
        if (innerComponent == null) {
			return;
		}
		
		// Lettura variabile runtime con lo stesso nome della colonna della tabella: è stata definita a fronte di DATA_VALUES() della JTable 
		varObject = this.getVarValue(mcb, tableColumn.getName(), EnumForwardScope.SCOPE_FUNCTION);
		
		// Update oggetti swing se tipo variabile compatibile
		columnClass = tableColumn.getClassType();
		
		updateGUIControlsOrVariables(mcb, varObject, columnClass, innerComponent, isSwingControlToUpdate);
		
	}

 	
	/*
	 * Update controllo swing con il valore della variabile bound della tabella
     *   Recupero valore variabile corrente oggetto bound 
     *   Update componente GUI se definita con lo stesso nome ed è compatibile con il tipo variabile
 	 * Update variabile bound valore controllo swing 
 	 *   Recupero valore controllo swing
 	 *   Update valore variabile bound
	 */
	private void updatePanelControlsOrTableRowBoundVar(InnerMonitorControBlock mcb, String tableName, ForwardTableModel forwardTableModel, boolean isSwingControlToUpdate) {
		Class<?> objectBoundClass = null;
		Object varObjectBound = null;
		InnerComponent innerComponent = null;

		// Lettura componente swing con il nome della variabile row bound
		innerComponent = mcb.function.getComponentDeclared(forwardTableModel._getVarNameDataBound());

		// Nessun componente swing definito con il nome previsto: nessuna operazione
        if (innerComponent == null) {
			return;
		}

  		// Lettura variabile runtime per l'oggetto bound di riga corrente: è stata definita a fronte di DATA_VALUES() della JTable 
		varObjectBound = this.getVarValue(mcb, forwardTableModel._getVarNameDataBound(), EnumForwardScope.SCOPE_FUNCTION);
		
		// Variabile bound a null.
		// Significa che la dichiarative DATA_BOUND() ha un numero di item < delle righe
		// e che è stata effettuata una selezione di una riga senza data bound.
		// Non viene aggiornato nessun campo di pannello.
		if (varObjectBound == null) {
			return;
		}
		
		// Update oggetto swing se tipo variabile compatibile
		objectBoundClass = varObjectBound.getClass();
		updateGUIControlsOrVariables(mcb, varObjectBound, objectBoundClass, innerComponent, isSwingControlToUpdate);
		
	}


	/*
     * Update componente GUI con valore variabile se isSwingControlToUpdate = true
     * Update variabile con valore componente GUI se isSwingControlToUpdate = false
     */
	private void updateGUIControlsOrVariables(InnerMonitorControBlock mcb, Object varObject, Class<?> columnClass, InnerComponent innerComponent, boolean isSwingControlToUpdate) {
		
		Integer intValue = null;
		Float floatValue = null;
		Double doubleValue = null;
		Date dateValue = null;
		Color colorValue = null;
	
		// Colonna di tipo String
		if (columnClass == String.class) {
			if (innerComponent.componentType == EnumForwardComponent.JTextField) {
				if (isSwingControlToUpdate) {
					((JTextField) innerComponent.component).setText(varObject.toString());
					return;
				}
				setVarValue(mcb, innerComponent.componentName, ((JTextField) innerComponent.component).getText());
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JPasswordField) {
				if (isSwingControlToUpdate) {
					((JPasswordField) innerComponent.component).setText(varObject.toString());
					return;
				}
				setVarValue(mcb, innerComponent.componentName, ((JPasswordField) innerComponent.component).getPassword());
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JFormattedTextField) {
				if (isSwingControlToUpdate) {
					((JFormattedTextField) innerComponent.component).setText(varObject.toString());
					return;
				}
				setVarValue(mcb, innerComponent.componentName, ((JFormattedTextField) innerComponent.component).getText());
			};
			if (innerComponent.componentType == EnumForwardComponent.JTextArea) {
				if (isSwingControlToUpdate) {
					((JTextArea) innerComponent.component).setText(varObject.toString());
					return;
				}
				setVarValue(mcb, innerComponent.componentName, ((JTextArea) innerComponent.component).getText());
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JLabel) {
				if (isSwingControlToUpdate) {
					((JLabel) innerComponent.component).setText(varObject.toString());
					return;
				}
				setVarValue(mcb, innerComponent.componentName, ((JLabel) innerComponent.component).getText());
				return;
			};

		} 

		// Colonna di tipo boolean
		if (columnClass == Boolean.class) {
			if (innerComponent.componentType == EnumForwardComponent.JCheckBox) {
				JCheckBox jcheckBox = (JCheckBox) innerComponent.component;
				if (isSwingControlToUpdate) {
					jcheckBox.setSelected((Boolean) varObject);
				} else {
					setVarValue(mcb, innerComponent.componentName, new Boolean(jcheckBox.isSelected()));
				}
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JRadioButton) {
				JRadioButton jradioButton = (JRadioButton) innerComponent.component;
				if (isSwingControlToUpdate) {
					jradioButton.setSelected((Boolean) varObject);
				} else {
					setVarValue(mcb, innerComponent.componentName, new Boolean(jradioButton.isSelected()));
				}
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JToggleButton) {
				JToggleButton jtoggleButton = (JToggleButton) innerComponent.component;
				if (isSwingControlToUpdate) {
					jtoggleButton.setSelected((Boolean) varObject);
				} else {
					setVarValue(mcb, innerComponent.componentName, new Boolean(jtoggleButton.isSelected()));
				}
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JCheckBoxMenuItem) {
				JCheckBoxMenuItem jcheckBoxMenuItem = (JCheckBoxMenuItem) innerComponent.component;
				if (isSwingControlToUpdate) {
					jcheckBoxMenuItem.setSelected((Boolean) varObject);
				} else {
					setVarValue(mcb, innerComponent.componentName, new Boolean(jcheckBoxMenuItem.isSelected()));
				}
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JRadioButtonMenuItem) {
				JRadioButtonMenuItem jradioButtonMenuItem = (JRadioButtonMenuItem) innerComponent.component;
				if (isSwingControlToUpdate) {
					jradioButtonMenuItem.setSelected((Boolean) varObject);
				} else {
					setVarValue(mcb, innerComponent.componentName, new Boolean(jradioButtonMenuItem.isSelected()));
				}
				return;
			};
		} 
		
		// Colonna di tipo integer
		if (columnClass == Integer.class) {
			if (innerComponent.componentType == EnumForwardComponent.JTextField) {
				JTextField jtextField = (JTextField) innerComponent.component;
				if (isSwingControlToUpdate) {
					jtextField.setText(varObject.toString());
				} else {
					// Se la string contiene un intero converto e valorizzo
					if (StringService._isNumericInt(jtextField.getText())) {
						int numInt = StringService._getNumericInt(jtextField.getText());
						setVarValue(mcb, innerComponent.componentName, new Integer(numInt));
					}
				}
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JFormattedTextField) {
				JFormattedTextField jformattedTextField = (JFormattedTextField) innerComponent.component;
				if (jformattedTextField.getValue() != null && jformattedTextField.getValue() instanceof Integer) {
					if (isSwingControlToUpdate) {
						intValue = (Integer) varObject;
						jformattedTextField.setValue(intValue);
					} else {
						setVarValue(mcb, innerComponent.componentName, new Integer((Integer)jformattedTextField.getValue()));
					}
					return;
				}
				if (jformattedTextField.getValue() != null && jformattedTextField.getValue() instanceof String) {
					if (isSwingControlToUpdate) {
						intValue = (Integer) varObject;
						jformattedTextField.setValue(intValue);
					}
				}
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JProgressBar) {
				JProgressBar jprogressBar = (JProgressBar) innerComponent.component;
				if (isSwingControlToUpdate) {
					intValue = (Integer) varObject;
					jprogressBar.setValue(intValue.intValue());
				} else {
					setVarValue(mcb, innerComponent.componentName, new Integer((Integer)jprogressBar.getValue()));
				}
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JSlider) {
				JSlider jslider = (JSlider) innerComponent.component;
				if (isSwingControlToUpdate) {
					intValue = (Integer) varObject;
					jslider.setValue(intValue.intValue());
				} else {
					setVarValue(mcb, innerComponent.componentName, new Integer((Integer)jslider.getValue()));
				}
				return;
			}; 
		} 
		
		// Colonna di tipo Float
		if (columnClass == Float.class) {
			floatValue = (Float) varObject;
			if (innerComponent.componentType == EnumForwardComponent.JTextField) {
				JTextField jtextField = (JTextField) innerComponent.component;
				jtextField.setText(floatValue.toString());
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JFormattedTextField) {
				JFormattedTextField jformattedTextField = (JFormattedTextField) innerComponent.component;
				if (jformattedTextField.getValue() != null && jformattedTextField.getValue() instanceof Float) {
					if (isSwingControlToUpdate) {
						jformattedTextField.setValue(floatValue.floatValue());
					} else {
						setVarValue(mcb, innerComponent.componentName, new Float((Float)jformattedTextField.getValue()));
					}
					return;
				}
				if (jformattedTextField.getValue() != null && jformattedTextField.getValue() instanceof String) {
					jformattedTextField.setValue(floatValue.toString());
					return;
				}
			};
			return;
		}  
		
		// Colonna di tipo Double
		if (columnClass == Double.class) {
			doubleValue = (Double) varObject;
			if (innerComponent.componentType == EnumForwardComponent.JTextField) {
				JTextField jtextField = (JTextField) innerComponent.component;
				jtextField.setText(doubleValue.toString());
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JFormattedTextField) {
				JFormattedTextField jformattedTextField = (JFormattedTextField) innerComponent.component;
				if (jformattedTextField.getValue() != null && jformattedTextField.getValue() instanceof Double) {
					if (isSwingControlToUpdate) {
						jformattedTextField.setValue(doubleValue.doubleValue());
					} else {
						setVarValue(mcb, innerComponent.componentName, new Double((Double)jformattedTextField.getValue()));
					}
					return;
				}
				if (jformattedTextField.getValue() != null && jformattedTextField.getValue() instanceof String) {
					jformattedTextField.setValue(doubleValue.toString());
					return;
				}
			};
			return;
		}  
		
		// Colonna di tipo Date
		if (columnClass == Date.class) {
			dateValue = (Date) varObject;
			if (innerComponent.componentType == EnumForwardComponent.JTextField) {
				JTextField jtextField = (JTextField) innerComponent.component;
				jtextField.setText(DateTimeService.getDateFormatted(dateValue, "dd-MM-yyyy"));
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JFormattedTextField) {
				JFormattedTextField jformattedTextField = (JFormattedTextField) innerComponent.component;
				if (jformattedTextField.getValue() != null && jformattedTextField.getValue() instanceof Date) {
					if (isSwingControlToUpdate) {
						jformattedTextField.setValue(dateValue);
					} else {
						setVarValue(mcb, innerComponent.componentName, new Date(((Date)jformattedTextField.getValue()).getTime()));
					}
					jformattedTextField.setValue(dateValue);
					return;
				}
				if (jformattedTextField.getValue() != null && jformattedTextField.getValue() instanceof String) {
					jformattedTextField.setValue(DateTimeService.getDateFormatted(dateValue, "dd-MM-yyyy"));
					return;
				}
			};
			return;
		}   
		
		// Colonna di tipo Color
		if (columnClass == Color.class) {
			colorValue = (Color) varObject;
			if (innerComponent.componentType == EnumForwardComponent.JLabel) {
				JLabel jlabel = (JLabel) innerComponent.component;
				if (isSwingControlToUpdate) {
					jlabel.setBackground(colorValue);
					jlabel.setOpaque(true);
				} else {
					setVarValue(mcb, innerComponent.componentName, new Color(jlabel.getBackground().getRed(), jlabel.getBackground().getGreen(), jlabel.getBackground().getBlue()));
				}
				return;
			};
			if (innerComponent.componentType == EnumForwardComponent.JTextField) {
				JTextField jtextField = (JTextField) innerComponent.component;
				jtextField.setText(colorValue.getGreen() + " " + colorValue.getRed() + " " + colorValue.getBlue());
				return;
			};
            return;			
		}
	}

	

	/* --------------------------
     * Action per alberi (JTree)
     * --------------------------
     * 
     * 
     */	
    private boolean execActionForTree(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {

    	boolean isDone = false;

     	// Azione già individuata e eseguita o exception in corso
     	if (mcb.system.getReusableMethodException() != null)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
		
       	switch (actionCoded.action) {
			case TREE_NODE_ADD: 					// Insersce il nodo in un albero
				// TODO
				isDone = true;
				break;
			case TREE_NODE_EXPAND: 					// Espande il nodo di un albero
				// TODO
				isDone = true;
				break;
			case TREE_NODE_COLLAPSE: 				// Contrae il nodo di un albero
				// TODO
				isDone = true;
				break;
			case TREE_NODE_SET_ICON: 				// Modifica l'icona del nodo
				// TODO
				isDone = true;
				break;
			case TREE_NODE_SET_TEXT: 				// Modifica il testo della foglia
				// TODO
				isDone = true;
				break;
			case TREE_CHILD_ADD: 					// Insersce un figlio a un nodo in un albero
				// TODO
				isDone = true;
				break;
			case TREE_CHILD_DELETE: 				// Elimina un figlio a un nodo in un albero
				// TODO
				isDone = true;
				break;
       	}
		return isDone;
	}


 	/* -----------------------------------------
     * Action per pannelli di dettaglio (JPanel)
     * -----------------------------------------
     * 
     * 
     */	
    private boolean execActionForPanel(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
      	boolean isDone = false;
  		
     	// Azione già individuata e eseguita o exception in corso
     	if (mcb.system.getReusableMethodException() != null)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
		
       	switch (actionCoded.action) {
       	
       		// Popola il pannello se questo ha una logical data view associata
			case PANEL_POPULATE_FROM_DB:
				isDone = true;
				panelPopulateFromDb(mcb, actionCoded);
			    manageLogicDeclaredActivation(mcb, actionCoded.componentName, EnumForwardEvent.ON_PANEL_AFTER_POPULATE); // Eventuale logica applicativa di specifica
				break;
				
			// Popola ogni combobox presente nel pannello con i dati della propria logical data view specificata 
			case PANEL_POPULATE_COMBOBOXES_FROM_DB:
				isDone = true;
				// Scan controlli definiti per il pannello
				for (ForwardPanelComponent panelComponent : mcb.function.getPanel(actionCoded.componentName).getComponents()) {
					if (panelComponent.getType() != EnumForwardComponent.JComboBox) {continue;}
					actionCoded.componentName = panelComponent.getName();
					componentRefreshFromDb(mcb, actionCoded);
				}
				break;
			 	
			//	Attiva in sequenza tutti i controlli FORMAL, EXISTENCE_ENTITY, EXISTENCE_RULE_TABLE, USER
			case PANEL_CONTROLS:
				isDone = true;
				// Reset iniziali
				mcb.system.getMessages().clear();	
				mcb.system.setErrorsFound(false);	
				mcb.system.setErrorsFormal(false);	
				mcb.system.setErrorsExistenceRuleTable(false);
				mcb.system.setErrorsExistenceEntity(false);
				mcb.system.setErrorsUser(false);
				// Esecuzione controlli in cascata
				panelControlsFormal(mcb, actionCoded);
				panelControlsExistenceRuleTable(mcb, actionCoded);
				panelControlsExistenceEntity(mcb, actionCoded);
				mcb.system.setActivePanel(actionCoded.panelName);
 				execReusableMethod(mcb, actionCoded);
 				if (mcb.system.isErrorsFound()) {Toolkit.getDefaultToolkit().beep();}
 				// Logiche eventuali da eseguire dopo i controlli.
 				// La funzione standard di visualizzazione messaggi è attivata SOLO se presente 
 				// ON_EVENT(ON_PANEL_CONTROLS_WITH_ERRORS|ON_PANEL_CONTROLS_WITH_NO_ERRORS,"panel", DO_FUNCTION_START_SHOW_MESSAGESS()
  				if ( mcb.system.isErrorsFound()) {manageLogicDeclaredActivation(mcb, actionCoded.panelName, EnumForwardEvent.ON_PANEL_CONTROLS_WITH_ERRORS);}
 				if (!mcb.system.isErrorsFound()) {manageLogicDeclaredActivation(mcb, actionCoded.panelName, EnumForwardEvent.ON_PANEL_CONTROLS_WITH_NO_ERRORS);}
				break;
		 
			// Attiva i controlli formali di numericità, decimali, range, max, min, ... 
			// La funzione standard di visualizzazione messaggi è attivata SOLO se presente 
 			// ON_EVENT(ON_PANEL_CONTROLS_FORMAL_WITH_ERRORS|ON_PANEL_CONTROLS_FORMAL_WITH_NO_ERRORS,"panel", DO_FUNCTION_START_SHOW_MESSAGESS()
			case PANEL_CONTROLS_FORMAL:
				isDone = true;
				mcb.system.setErrorsFormal(false);	
				mcb.system.clearMessages(EnumForwardAction.PANEL_CONTROLS_FORMAL);
				panelControlsFormal(mcb, actionCoded);
				// Logiche eventuali da eseguiire dopo i controlli formali
 				if ( mcb.system.isErrorsFormal()) {manageLogicDeclaredActivation(mcb, actionCoded.panelName, EnumForwardEvent.ON_PANEL_CONTROLS_FORMAL_WITH_ERRORS);}
 				if (!mcb.system.isErrorsFormal()) {manageLogicDeclaredActivation(mcb, actionCoded.panelName, EnumForwardEvent.ON_PANEL_CONTROLS_WITH_NO_ERRORS);}
 				break;

 			// Attiva il controllo di esistenza su db di rule tables le cui chiavi sono sul pannello
 			// La funzione standard di visualizzazione messaggi è attivata SOLO se presente 
 	 		// ON_EVENT(ON_PANEL_CONTROLS_EXISTENCE_RULE_TABLE_WITH_ERRORS|ON_PANEL_CONTROLS_EXISTENCE_RULE_TABLE_WITH_NO_ERRORS,"panel", DO_FUNCTION_START_SHOW_MESSAGESS()
			case PANEL_CONTROLS_EXISTENCE_RULE_TABLE:
				isDone = true;
				mcb.system.setErrorsExistenceRuleTable(false);
				mcb.system.clearMessages(EnumForwardAction.PANEL_CONTROLS_EXISTENCE_RULE_TABLE);
				panelControlsExistenceRuleTable(mcb, actionCoded);
				// Logiche eventuali da eseguiire dopo i controlli di esistenza in rule table
 				if ( mcb.system.isErrorsFormal()) {manageLogicDeclaredActivation(mcb, actionCoded.panelName, EnumForwardEvent.ON_PANEL_CONTROLS_EXISTENCE_RULE_TABLE_WITH_ERRORS);}
 				if (!mcb.system.isErrorsFormal()) {manageLogicDeclaredActivation(mcb, actionCoded.panelName, EnumForwardEvent.ON_PANEL_CONTROLS_EXISTENCE_RULE_TABLE_WITH_NO_ERRORS);}
				break;
			
			// Attiva il controllo di esistenza su db di entities le cui chiavi sono sul pannello
			case PANEL_CONTROLS_EXISTENCE_ENTITY:
				isDone = true;
				mcb.system.setErrorsExistenceEntity(false);
				mcb.system.clearMessages(EnumForwardAction.PANEL_CONTROLS_EXISTENCE_ENTITY);
				panelControlsExistenceEntity(mcb, actionCoded);
				// Logiche eventuali da eseguiire dopo i controlli di esistenza in entity attraverso ldv
 				if ( mcb.system.isErrorsExistenceEntity()) {manageLogicDeclaredActivation(mcb, actionCoded.panelName, EnumForwardEvent.ON_PANEL_CONTROLS_EXISTENCE_ENTITY_WITH_ERRORS);}
 				if (!mcb.system.isErrorsExistenceEntity()) {manageLogicDeclaredActivation(mcb, actionCoded.panelName, EnumForwardEvent.ON_PANEL_CONTROLS_EXISTENCE_ENTITY_WITH_NO_ERRORS);}
				break;
			
			// Attiva il controllo applicativo utente di contesto sui campi del pannello
			case PANEL_CONTROLS_USER:
				isDone = true;
				mcb.system.setErrorsUser(false);
				mcb.system.setErrorsUser(false);
				mcb.system.clearMessages(EnumForwardAction.PANEL_CONTROLS_USER);
				mcb.system.setActivePanel(actionCoded.componentName);
 				execReusableMethod(mcb, actionCoded);
				// Logiche eventuali da eseguiire dopo i controlli user
 				if ( mcb.system.isErrorsUser()) {manageLogicDeclaredActivation(mcb, actionCoded.panelName, EnumForwardEvent.ON_PANEL_CONTROLS_USER_WITH_ERRORS);}
 				if (!mcb.system.isErrorsUser()) {manageLogicDeclaredActivation(mcb, actionCoded.panelName, EnumForwardEvent.ON_PANEL_CONTROLS_USER_WITH_NO_ERRORS);}
				break;
			 
			// Imposta i valori di default per tutti campi del pannello  
			case PANEL_DEFAULTS_INITIAL:  			
				isDone = true;
 				// Eventuale esecuzione multipla su + panels
 				for (String panelName : actionCoded.panelNames) {
 					actionCoded.componentName = panelName;
 					panelSetDefaultControls(mcb, actionCoded);
 					manageLogicDeclaredActivation(mcb, panelName, EnumForwardEvent.ON_PANEL_AFTER_DEFAULTS_INITIAL); 		// Eventuale logica applicativa di default specifica
 				}
				break;
				
			// Call al metodo di gestione default del pannello  	
			case PANEL_DEFAULTS_USER:  				
				isDone = true;
				execReusableMethod(mcb, actionCoded);
				break;
				
       	}
		return isDone;
	}

    /* -----------------------------------------------
     * Gestione controlli formali componenti panel
     * -----------------------------------------------
     * 
     */
    private void panelControlsFormal(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
    	
		ForwardPanel forwardPanel = null;
		InnerComponent innerComponent = null;
		ArrayList<ForwardPanelComponent> al_panelComponent = null;
		Object valueComponentObject ="";
		boolean isWithFormalErrors = false;
		
		// Descrittore panel
		forwardPanel = mcb.function.getPanelDescriptor(actionCoded.panelName);
 		
 		// Scan componenti
 		al_panelComponent = forwardPanel.getComponents();
 		for (ForwardPanelComponent panelComponent : al_panelComponent) {
 			
 			// Componenti per il SI attivano i controlli formali
 			if (panelComponent.getType() != EnumForwardComponent.JTextField
 			&&	panelComponent.getType() != EnumForwardComponent.JPasswordField		
 			&&	panelComponent.getType() != EnumForwardComponent.JFormattedTextField) {
				continue;
			}
 		
 			valueComponentObject = mcb.function.getValueControlGUI(panelComponent.getName());
 			innerComponent = mcb.function.getComponentDeclared(panelComponent.getName());
 			
 			// Reset component formal error
			panelComponent.setWithFormalErrors(false);
            ((JComponent) panelComponent.getGraphicObject()).setBorder(panelComponent.getBorderObject());
			
 			// Controlli su valori stringa o date (anche data NON gestita con JFormattedField)
			if (valueComponentObject.getClass() == String.class || valueComponentObject.getClass() == Date.class) {
				isWithFormalErrors = panelControlFormalString(mcb, actionCoded, panelComponent, innerComponent, (String) valueComponentObject);
				panelComponent.setWithFormalErrors(isWithFormalErrors);
				if (!panelComponent.isWithFormalErrors()) {continue;}
				mcb.system.setErrorsFound(true);
				mcb.system.setErrorsFormal(true);
//		        ((JComponent) panelComponent.getGraphicObject()).setBorder(BorderFactory.createLineBorder(Color.RED, 1, false));
		        ((JComponent) panelComponent.getGraphicObject()).setBorder(BorderFactory.createLineBorder(Color.RED, 1));
				continue;
			}
 
 			// Controlli su valori numerici interi Integer o Long
			if (valueComponentObject.getClass() == Integer.class || valueComponentObject.getClass() == Long.class) {
				isWithFormalErrors = panelControlFormalIntegerLong(mcb, actionCoded, panelComponent, innerComponent, valueComponentObject);
				panelComponent.setWithFormalErrors(isWithFormalErrors);
				if (!panelComponent.isWithFormalErrors()) {continue;}
//				((JComponent) panelComponent.getGraphicObject()).setBorder(BorderFactory.createLineBorder(Color.RED, 1, false));
				((JComponent) panelComponent.getGraphicObject()).setBorder(BorderFactory.createLineBorder(Color.RED, 1));
				mcb.system.setErrorsFound(true);
				mcb.system.setErrorsFormal(true);
				continue;
			}

 			// Controlli su valori numerici con virgola Double 
			if (valueComponentObject.getClass() == Double.class ) {
				isWithFormalErrors = panelControlFormalDouble(mcb, actionCoded, panelComponent, innerComponent, (Double) valueComponentObject);
				panelComponent.setWithFormalErrors(isWithFormalErrors);
				if (!panelComponent.isWithFormalErrors()) {continue;}
//				((JComponent) panelComponent.getGraphicObject()).setBorder(BorderFactory.createLineBorder(Color.RED, 1, false));
				((JComponent) panelComponent.getGraphicObject()).setBorder(BorderFactory.createLineBorder(Color.RED, 1));
				mcb.system.setErrorsFound(true);
				mcb.system.setErrorsFormal(true);
				continue;
			}
		}
 	}
 
    /* -------------------------------------------------------------
     * Controlli formali su controllo GUI associato a String o Date
     * -------------------------------------------------------------
     * 
     */
    private boolean panelControlFormalString(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, ForwardPanelComponent panelComponent, InnerComponent innerComponent, String valueString) {
		
		// Controllo obbligatorietà
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_MANDATORY) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_MANDATORY) == true) {
			// Errore di obbligatorietà
			if (valueString.equals("")) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0104", "Mandatory control and no data typed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo uppercase
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_UPPERCASE) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_UPPERCASE) == true) {
			if (!StringService._isUpperCase(valueString)) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0105", "Characters typed must be uppercase");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo lowercase
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_LOWERCASE) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_LOWERCASE) == true) {
			if (!StringService._isLowerCase(valueString)) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0106", "Characters typed must be lowercase");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo alphabetic
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_ALPHABETIC) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_ALPHABETIC) == true) {
			if (!StringService._isAlphabetic(valueString)) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0107", "Characters typed must be alphabetic");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo Numeric (COMPONENT_CTRL_MASK non è stato specificato)
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_NUMERIC) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_NUMERIC) == true
		&&  panelComponent.getCtrlMask().equals("")) {
			if (!StringService._isNumericString(valueString)) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0108", "All characters must be numeric");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo valore massimo
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_MAX) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_MAX) == true) {
			if (valueString.compareTo((String)panelComponent.getCtrlValueMax()) > 0) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0109", "String value typed greater than allowed value");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo valore minimo
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_MIN) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_MIN) == true) {
			if (valueString.compareTo((String)panelComponent.getCtrlValueMin()) < 0) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0110", "String value typed less than allowed value");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo valore in range
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_RANGE) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_RANGE) == true) {
			if (valueString.compareTo((String)panelComponent.getCtrlValueMax()) > 0 
			||  valueString.compareTo((String)panelComponent.getCtrlValueMin()) < 0) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0111", "String value typed not in the allowed range of values");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo numero caratteri massimo
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_MAX) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_MAX) == true) {
			if (valueString.length() > panelComponent.getCtrlTextSizeMax()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0112", "Number of characters grater than max allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo numero caratteri minimo
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_MIN) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_MIN) == true) {
			if (valueString.length() < panelComponent.getCtrlTextSizeMin()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0113", "Number of characters less than max allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo range numero caratteri digitati
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_RANGE) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_RANGE) == true) {
			if (valueString.length() > panelComponent.getCtrlTextSizeMax() 
			||  valueString.length() < panelComponent.getCtrlTextSizeMin()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0114", "Number of characters typed not in the allowed range");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo data, deve essere stata definita una mask con COMPONENT_CTRL_MASK
		if (((panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE) != null && 
		      panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE) == true
		     )
		  || (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_MAX) != null && 
			  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_MAX) == true
			 )
		  || (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_MIN) != null && 
			  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_MIN) == true
			 )
		  || (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_RANGE) != null && 
		      panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_RANGE) == true
		     )
		    )
		&& !panelComponent.getCtrlMask().equals("")) {
			if (!DateTimeService.isValidDate(valueString, panelComponent.getCtrlMask())) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0115", "Incorrect date or wrong mask");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo valore massimo data, deve essere stata definita una mask con COMPONENT_CTRL_MASK
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_MAX) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_MAX) == true
		&& !panelComponent.getCtrlMask().equals("")) {
            Date dtValueString = DateTimeService.getDate(valueString, panelComponent.getCtrlMask());
			Date dtMax = DateTimeService.getDate((String) panelComponent.getCtrlValueMax(), panelComponent.getCtrlMask()); 
			if (DateTimeService.compareDate(dtValueString, dtMax) > 0) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0116", "Date greater than maximimum allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo valore minimo data, deve essere stata definita una mask con COMPONENT_CTRL_MASK
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_MIN) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_MIN) == true
		&& !panelComponent.getCtrlMask().equals("")) {
            Date dtValueString = DateTimeService.getDate(valueString, panelComponent.getCtrlMask());
			Date dtMin = DateTimeService.getDate((String) panelComponent.getCtrlValueMin(), panelComponent.getCtrlMask()); 
			if (DateTimeService.compareDate(dtValueString, dtMin) < 0) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0117", "Date less than maximimum allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		// Controllo range data, deve essere stata definita una mask con COMPONENT_CTRL_MASK
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_RANGE) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_DATE_RANGE) == true
		&& !panelComponent.getCtrlMask().equals("")) {
            Date dtValueString = DateTimeService.getDate(valueString, panelComponent.getCtrlMask());
			Date dtMin = DateTimeService.getDate((String) panelComponent.getCtrlValueMin(), panelComponent.getCtrlMask()); 
			Date dtMax = DateTimeService.getDate((String) panelComponent.getCtrlValueMax(), panelComponent.getCtrlMask()); 
			if (DateTimeService.compareDate(dtValueString, dtMin) < 0
			||  DateTimeService.compareDate(dtValueString, dtMax) > 0) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0118", "Date not in the allowed range");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		return false;
	}


    /* ---------------------------------------------------------------
     * Controlli formali su controllo GUI associato a Integer o Long
     * ---------------------------------------------------------------
     * 
     */
	private boolean panelControlFormalIntegerLong(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, ForwardPanelComponent panelComponent, InnerComponent innerComponent, Object valueIntegerLong) {

		long value = 0;
		int valueInteger = 0;
		
		if (valueIntegerLong instanceof Long) {
			value = (Long) valueIntegerLong;
		}
		if (valueIntegerLong instanceof Integer) {
			valueInteger = (Integer) valueIntegerLong;
			value = valueInteger;
		}
		
		// Controllo valore massimo intero
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MAX) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MAX) == true) {
			if (value > (Integer) panelComponent.getCtrlValueMax()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0119", "Integer value typed greater than allowed maximum value");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}	
		
		// Controllo valore minimo intero
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MIN) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MIN) == true) {
			if (value < (Integer) panelComponent.getCtrlValueMin()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0120", "Integer value typed less than allowed minimum value");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}	
		
		// Controllo valore in range di interi
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_RANGE) != null 
		&&  panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_RANGE) == true) {
			if (value < (Integer) panelComponent.getCtrlValueMin() 
			||  value > (Integer) panelComponent.getCtrlValueMax()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0121", "Integer value typed not in the allowed range");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}
		
		return false;
	}


    /* ---------------------------------------------------------------
     * Controlli formali su controllo GUI associato a Double 
     * ---------------------------------------------------------------
     * 
     */
	private boolean panelControlFormalDouble(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, ForwardPanelComponent panelComponent, InnerComponent innerComponent, Double valueDouble) {
        long digitsIntValue = 0;
        int digitsDecValue = 0;

        // Controllo numero cifre intere massimo di campo con virgola
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_MAX) != null 
		&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_MAX) == true) {
			digitsIntValue = valueDouble.intValue();
			if (digitsIntValue > (Integer) panelComponent.getCtrlValueIntMax()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0122", "Integer value greater than maximum allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}		
	
		// Controllo numero cifre intere minimo di campo con virgola
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_MIN) != null 
		&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_MIN) == true) {
			digitsIntValue = valueDouble.intValue();
			if (digitsIntValue < (Integer) panelComponent.getCtrlValueIntMin()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0123", "Integer value less then minimum allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}		
		
		// Controllo numero cifre intere in range di minimo e massimo di campo con virgola
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_RANGE) != null 
		&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_RANGE) == true) {
			digitsIntValue = valueDouble.intValue();
			if (digitsIntValue < (Integer) panelComponent.getCtrlValueIntMin()
			||  digitsIntValue > (Integer) panelComponent.getCtrlValueIntMax()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0124", "Integer value not in the range allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}		

		// Controllo numero cifre decimali massimo di campo con virgola
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_DEC_MAX) != null 
		&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_DEC_MAX) == true) {
			digitsDecValue  = NumericService.getDoubleDecValue(valueDouble);
			if (digitsDecValue > panelComponent.getCtrlValueDecMax()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0125", "Decimal value greater the maximum allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}	
		
		// Controllo numero cifre decimali minimo di campo con virgola
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_DEC_MIN) != null 
		&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_DEC_MIN) == true) {
			digitsDecValue  = NumericService.getDoubleDecValue(valueDouble);
			if (digitsDecValue < panelComponent.getCtrlValueDecMin()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0126", "Decimal value less than minimum allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}	
		
		// Controllo numero cifre decimali in range di campo con virgola
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_RANGE) != null 
		&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_RANGE) == true) {
			digitsDecValue  = NumericService.getDoubleDecValue(valueDouble);
			if (digitsDecValue < panelComponent.getCtrlValueDecMin()
			||  digitsDecValue > panelComponent.getCtrlValueDecMax()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0127", "Decimal value not in the range allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}		
	
	 	
		// Controllo valore massimo di campo con virgola
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MAX) != null 
		&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MAX) == true) {
			if (panelComponent.getCtrlValueMax() instanceof Double && valueDouble > (Double) panelComponent.getCtrlValueMax()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0128", "Decimal value not in the range allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}		
	
		// Controllo valore minimo di campo con virgola
		if (panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MIN) != null 
		&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MIN) == true) {
			if (panelComponent.getCtrlValueMin() instanceof Double && valueDouble > (Double) panelComponent.getCtrlValueMin()) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_FORMAL, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0129", "Value typed less than minimum allowed");
				panelComponent.setWithFormalErrors(true);
				return true;
			}
		}		
	
		return false;
	}


 
	/* -------------------------------------------------------------
     * Gestione controlli esistenza in rule table componenti panel
     * -------------------------------------------------------------
     * 
     * Viene controllata l'esistenza/non esistenza in rule table associata al campo
     * Viene eseguita la ldv di accesso all rule table
     * Vengono valorizzati automaticamente i componenti con le colonne della ldv (descrizione)
     */
	private void panelControlsExistenceRuleTable(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
		
		ForwardPanel forwardPanel = null;
		ForwardDoParms actionLdvCreate = null;
		ForwardLogicalDataView ldv = null;
		ArrayList<ForwardPanelComponent> al_panelComponent = null;
		Object valueComponentObject ="";
		String valueComponent ="";
		int retCode = 0;
		
		// Descrittore panel
		forwardPanel = mcb.function.getPanelDescriptor(actionCoded.panelName);
 		
 		// Scan componenti
 		al_panelComponent = forwardPanel.getComponents();
 		for (ForwardPanelComponent panelComponent : al_panelComponent) {
 			
			// Componenti per il SI attivano i controlli di esistenza in rule table
 			if (panelComponent.getType() != EnumForwardComponent.JTextField
 			&&	panelComponent.getType() != EnumForwardComponent.JFormattedTextField) {
				continue;
			}

 			// Già presente un errore formale: skip
 			if (panelComponent.isWithFormalErrors()) {continue;}
 			
 			// Nessun controllo da fare per il componente o NON è un campo da controllare in rule table
			if (panelComponent.getCtrlTableNum() < 0) {continue;}
			if (!(panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_EXISTS_RULE_TABLE)   != null && panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_EXISTS_RULE_TABLE))
			&&  !(panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_UNEXISTS_RULE_TABLE) != null && panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_UNEXISTS_RULE_TABLE))) {
				continue;
			}
			
 			// Reset component existence rule table error
			panelComponent.setWithExistsRuleTableErrors(false);
            ((JComponent) panelComponent.getGraphicObject()).setBorder(panelComponent.getBorderObject());

			// Gestione accesso alla vista logica  
			actionLdvCreate = mcb.function.DO_LDV_CREATE(LDV_RULE_TABLE_READ_ITEM_DEFAULT);
			retCode = execActionLdvCreateCommon(mcb, actionLdvCreate);
			if (retCode > 0) {break;}
			retCode = execActionLdvValidateCommon(mcb, actionLdvCreate);
			if (retCode > 0) {break;}
			
			// Valore key componente GUI per tabella da controllare
			valueComponentObject = mcb.function.getValueControlGUI(panelComponent.getName());
			if (valueComponentObject instanceof String) {
				valueComponent = (String) valueComponentObject;
			} else if (valueComponentObject instanceof Integer) {
				valueComponent = ((Integer) valueComponentObject).toString();
			} else {
				continue;
			}
			
			// Impostazione chiavi di accesso
			ldv = (ForwardLogicalDataView) mcb.system.getLdvObject();
			ldv.setRuleTableNum(panelComponent.getCtrlTableNum());
			ldv.setRuleTableLanguage(mcb.function.getLanguage());
			ldv.setValue("keyVal", valueComponent);
			
			// Esecuzione vista logica
			retCode = execActionLdvExecuteCommon(mcb, actionLdvCreate);
			if (retCode > 0) {break;}
		
			// Read prima riga
			retCode = ldv.readFirst();
			mcb.system.setLdvReturnCode(retCode);
			mcb.system.setReturnCode(0);
			if (retCode > 0 && retCode != EnumForwardLogicalDataViewControl.LDV_EOF.ordinal()) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
			
			// La key DEVE esistere
			if (retCode == EnumForwardLogicalDataViewControl.LDV_EOF.ordinal() 
			&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_EXISTS_RULE_TABLE) != null 
			&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_EXISTS_RULE_TABLE)) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_EXISTENCE_RULE_TABLE, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0130", "Field not defined in the table ");
//				((JComponent) panelComponent.getGraphicObject()).setBorder(BorderFactory.createLineBorder(Color.RED, 1, false));
				((JComponent) panelComponent.getGraphicObject()).setBorder(BorderFactory.createLineBorder(Color.RED, 1));
				panelComponent.setWithExistsRuleTableErrors(true);
				continue;
			}
			 
			// La key NON deve esistere
			if (retCode == 0 
			&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_UNEXISTS_RULE_TABLE) != null
			&& panelComponent.getOptionsMap().get(EnumForwardOption.COMPONENT_CTRL_UNEXISTS_RULE_TABLE)) {
				mcb.system.addMessage(EnumForwardAction.PANEL_CONTROLS_EXISTENCE_RULE_TABLE, EnumMessageType.ERROR_INPUT, panelComponent.getLookupTableNum(), actionCoded.panelName, panelComponent.getName(), "ET0131", "Field defined in the table ");
//				((JComponent) panelComponent.getGraphicObject()).setBorder(BorderFactory.createLineBorder(Color.RED, 1, false));
				((JComponent) panelComponent.getGraphicObject()).setBorder(BorderFactory.createLineBorder(Color.RED, 1));
				panelComponent.setWithExistsRuleTableErrors(true);
				continue;
			}
		}
 		
 		
	}
    /* ------------------------------------------------------------
     * Gestione controlli di esistenza in entity componenti panel
     * ------------------------------------------------------------
     * 
     * Viene controllata l'esistenza/non esistenza in rule table associata al campo
     * Viene eseguita la ldv di accesso all rule table
     * Vengono valorizzati automaticamente i componenti con le colonne della ldv (descrizione)
     */
	private void panelControlsExistenceEntity(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
		// TODO Auto-generated method stub
	}





	/* -------------------------------------------------
     * Popolamento pannello con dati db
     * -------------------------------------------------
     * 
     *  - Verifica se codificata ldv
     *  - Esecuzione ldv
     *  - Popolamento campi panel da ldv
     *  - Scan controlli panel
     *  -- se comboBox gestione popolamento con ldv
     *  -- se list gestione popolamento con ldv
     *  -- se table gestione popolamento con ldv
     *  -- se tree gestione popolamento con ldv
     */
    private void panelPopulateFromDb(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
    	
		InnerComponent componentDeclared = null;
		ForwardDoParms actionLdv = null;
		ForwardDoParms actionRefresh = null;
		ForwardLogicalDataView ldvObject = null;
		String panelLdvClassName = "";
		String panelName = "";
		int retCode = 0;
		
		// Verifica se il componente è un JPanel
		panelName = actionCoded.componentName;
		componentDeclared = mcb.function.getComponentDeclared(panelName);
		if (componentDeclared.componentType != EnumForwardComponent.JPanel) { return;}
		
		panelLdvClassName = actionCoded.panelLdvClassName;
		if (panelLdvClassName.equals("")) {
			panelLdvClassName = componentDeclared.ldvName;
		}
		
		// Ldv dichiarata per il pannello o impostata nella action
		if (!panelLdvClassName.equals("")) {
			// Creazione, validazione, esecuzione ldv
			// I campi chiave e le variabili host da utilizzare si considerano già valorizzati applicativamente
			actionLdv = new ForwardDoParms();
			actionLdv.componentName = panelLdvClassName;
		    mcb.system.setLdvName(panelLdvClassName);
			mcb.system.setReturnCode(0);
	
			// Recupero o creazione ldv e validazione
			ldvObject = mcb.hm_ldv.get(actionLdv.ldvClassName);
			if (ldvObject == null) {
				retCode = ldvCreate(mcb, actionLdv);
				mcb.system.setLdvReturnCode(retCode);
				if (retCode != 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
				if (retCode != 0) {return;}
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
				retCode = ldvObject.validate();
				mcb.system.setLdvReturnCode(retCode);
				if (retCode != 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
				if (retCode != 0) {return;}
			}
			mcb.system.setLdvObject(ldvObject);
			
			// Esecuzione
			retCode = ldvObject.execute();
			mcb.system.setLdvReturnCode(retCode);
			if (retCode != 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
			if (retCode != 0) {return;}
			if (mcb.system.getReturnCode() > 0) {return;}
			
			// Lettura prima riga ldv
			ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
			if (ldvObject.getCountRows() > 0) {
				actionLdv.action = EnumForwardAction.LDV_READFIRST;
				execActionForLdv(mcb, actionLdv, false);
				if (mcb.system.getReturnCode() > 0) {return;} 
			}
			
			// Valorizzazione campi panel con colonne logical data view
			actionLdv.action = EnumForwardAction.LDV_SET_FUNCTION_GUI_CONTROLS;
			execActionForLdv(mcb, actionLdv, false);
			if (mcb.system.getReturnCode() > 0) {return;} 
		}		
		
		actionRefresh = new ForwardDoParms();
		
		// Scan controlli definiti per il pannello per popolamento tabelle e liste
		for (ForwardPanelComponent panelComponent : mcb.function.getPanel(panelName).getComponents()) {
			 
			switch (panelComponent.getType()) {
				case JList:
					actionRefresh.componentName = panelComponent.getName();
					componentRefreshFromDb(mcb, actionRefresh);
					break; 
				case JTable:
					actionRefresh.componentName = panelComponent.getName();
					componentRefreshFromDb(mcb, actionRefresh);
					break;
			}
		}
	}

	/* -------------------------------------------------------------
     * Impostazione valori di default per i controlli del pannello.
     * -------------------------------------------------------------
     * 
     * Si analizzano tutti i controlli del pannello
     * 	Per ogni controllo si verifica il valore di default del decrittore del campo del pannello
     * 		Se valorizzato si applica al controllo
     * 			Valore String per JText, JFormattedText, JPassword, JSpinner text, JComboBox
     * 			Valore Numerico per JSlider, JSpinner numeric, ....
     * 			Valore Data per JSpinner data
     *          ... etc.
     */
	@SuppressWarnings("rawtypes")
	private void panelSetDefaultControls(InnerMonitorControBlock mcb, ForwardDoParms actionParmsPanel) {
		InnerComponent componentDeclared = null;
		String panelName = "";
		
		// Verifica se il componente è un JPanel
		panelName = actionParmsPanel.componentName;
		componentDeclared = mcb.function.getComponentDeclared(panelName);
		
		// Componente NON definito o NON panel
		if (componentDeclared == null)  { return;}
		if (componentDeclared.componentType != EnumForwardComponent.JPanel) { return;}
		
		// Scan controlli definiti per il pannello
		for (ForwardPanelComponent panelComponent : mcb.function.getPanel(panelName).getComponents()) {
			
			switch (panelComponent.getType()) {
				case JLabel:
					if (panelComponent.getDefaultValue() != null) {
						JLabel jlabel = (JLabel) panelComponent.getGraphicObject();
						jlabel.setText((String) panelComponent.getDefaultValue());
					}
					break;
				case JTextField:
					if (panelComponent.getDefaultValue() != null && panelComponent.getDefaultValue()  instanceof String) {
						JTextField jtextField = (JTextField) panelComponent.getGraphicObject();
						jtextField.setText((String) panelComponent.getDefaultValue());
					}
					break;
				case JFormattedTextField:
					if (panelComponent.getDefaultValue() != null) {
						if (panelComponent.getDefaultValue()  instanceof String 
						||  panelComponent.getDefaultValue()  instanceof Integer
						||  panelComponent.getDefaultValue()  instanceof Date) {
							JFormattedTextField jformattedTextField = (JFormattedTextField) panelComponent.getGraphicObject();
							jformattedTextField.setValue(panelComponent.getDefaultValue());
						}
					}
					break;
				case JPasswordField:
					if (panelComponent.getDefaultValue() != null && panelComponent.getDefaultValue()  instanceof String) {
						JPasswordField jPasswordField = (JPasswordField) panelComponent.getGraphicObject();
						jPasswordField.setText((String) panelComponent.getDefaultValue());
					}
					break;
				case JTextArea:
					if (panelComponent.getDefaultValue() != null && panelComponent.getDefaultValue()  instanceof String) {
						JTextArea jtextArea = (JTextArea) panelComponent.getGraphicObject();
						jtextArea.setText((String) panelComponent.getDefaultValue());
					}
					break;
				case JSlider:
					if (panelComponent.getDefaultValue() != null && panelComponent.getDefaultValue()  instanceof Integer) {
						JSlider jslider = (JSlider) panelComponent.getGraphicObject();
						jslider.setValue((Integer) panelComponent.getDefaultValue());
					}
					break;
				case JSpinner:
					if (panelComponent.getDefaultValue() != null) {
						JSpinner jspinner = (JSpinner) panelComponent.getGraphicObject();
						// Spinner di text
						if (jspinner.getModel() instanceof SpinnerListModel &&  panelComponent.getDefaultValue() instanceof String) {
							jspinner.setValue((String) panelComponent.getDefaultValue());
						}
						// Spinner di numeri
						if (jspinner.getModel() instanceof SpinnerNumberModel &&  panelComponent.getDefaultValue() instanceof Integer) {
							jspinner.setValue((Integer) panelComponent.getDefaultValue());
						}
						// Spinner di date
						if (jspinner.getModel() instanceof SpinnerDateModel &&  panelComponent.getDefaultValue() instanceof Date) {
							jspinner.setValue((Date) panelComponent.getDefaultValue());
						}
					}
					break;
				case JComboBox:
					if (panelComponent.getDefaultValue() != null && panelComponent.getDefaultValue() instanceof String) {
						JComboBox jcomboBox = (JComboBox) panelComponent.getGraphicObject();
						jcomboBox.setSelectedItem((String) panelComponent.getDefaultValue());
					}
					break;
				case JCheckBox:
					if (panelComponent.getDefaultValue() != null && panelComponent.getDefaultValue() instanceof Boolean) {
						Boolean isSelected = (Boolean) panelComponent.getDefaultValue();
						JCheckBox jcheckBox = (JCheckBox) panelComponent.getGraphicObject();
						jcheckBox.setSelected(isSelected);
					}
					break;
				case JRadioButton:
					if (panelComponent.getDefaultValue() != null && panelComponent.getDefaultValue()  instanceof Boolean) {
						Boolean isSelected = (Boolean) panelComponent.getDefaultValue();
						JRadioButton jradioButtom = (JRadioButton) panelComponent.getGraphicObject();
						jradioButtom.setSelected(isSelected);
					}
					break;
				case JToggleButton:
					if (panelComponent.getDefaultValue() != null && panelComponent.getDefaultValue()  instanceof Boolean) {
						Boolean isSelected = (Boolean) panelComponent.getDefaultValue();
						JToggleButton jtoggleButtom = (JToggleButton) panelComponent.getGraphicObject();
						jtoggleButtom.setSelected(isSelected);
					}
					break;
				case JProgressBar:
					if (panelComponent.getDefaultValue() != null && panelComponent.getDefaultValue()  instanceof Integer) {
						JProgressBar jprogressBar = (JProgressBar) panelComponent.getGraphicObject();
						jprogressBar.setValue((Integer) panelComponent.getDefaultValue());
					}
					break;
	
				default:
					break;
				}
		}
		
	}


	/* ----------------------------
     * Action per struttura layout
     * ----------------------------
     * 
     * 
     */
    private boolean execActionForLayout(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
    	
     	// Azione già individuata e eseguita o exception in corso
     	if (mcb.system.getReusableMethodException() != null)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
		
       	switch (actionCoded.action) {
			case LAYOUT_REMOVE_FORM: 				// Rimuove un form da un pannello ovvero il pannello main e tutti quelli contenuti
				//TODO
				break;
				
			case LAYOUT_REMOVE_PANELS_CHILD: 		// Rimuove tutti i pannelli figli dalla struttura della funzione
				//TODO
				break;
      	}
		return false;
	}


	/* -------------------------------------------------
     * Action per attivazione/chiusura funzioni e dialog
     * -------------------------------------------------
     * 
     * 
     */
    private boolean execActionForDialogAndFunction(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, InnerOnEvent innerOnEvent, boolean isActionExecuted) {
	   	boolean isDone = false;
  		String dialogName = "";
  		
     	// Azione già individuata e eseguita o exception in corso
     	if (mcb.system.getReusableMethodException() != null)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
    	
       	switch (actionCoded.action) {
       	
	    	case DIALOG_START_PLAIN_MESSAGE: 				// Attiva una funzione modale di dialogo per messaggio info senza icona OK_OPTION
	    	case DIALOG_START_INFORMATION: 					// Attiva una funzione modale di dialogo per messaggio info OK_OPTION
	    	case DIALOG_START_WARNING: 						// Attiva una funzione modale di dialogo per messaggio warning OK_OPTION
	    	case DIALOG_START_ERROR: 						// Attiva una funzione modale di dialogo per messaggio warning OK_OPTION
	    	case DIALOG_START_QUESTION_YES_NO: 				// Attiva una funzione modale di dialogo per messaggio question YES_NO_OPTION
	    	case DIALOG_START_QUESTION_YES_NO_CANCEL: 		// Attiva una funzione modale di dialogo per messaggio question YES_NO_CANCEL_OPTION
	    	case DIALOG_START_QUESTION_OK_CANCEL: 			// Attiva una funzione modale di dialogo per messaggio question OK_CANCEL_OPTION
	    	case DIALOG_START_INPUT_BY_TEXT: 				// Attiva una funzione modale di dialogo per messaggio input da  textField OK_CANCEL_OPTION
	    	case DIALOG_START_INPUT_BY_COMBO_BOX: 			// Attiva una funzione modale di dialogo per messaggio input da comboBox OK_CANCEL_OPTION
				execActionForStartDialogStdManager(mcb, actionCoded, innerOnEvent);
				isDone = true;
		    	break;
		    
		    // Attiva il dialogo modale java standard di selezione file da file system	
	    	case DIALOG_START_FILE_OPEN_CHOOSER:     		
	    		isDone = true;
	    		execActionForStartDialogFileChooserManager(mcb, actionCoded, innerOnEvent);
	    		break;
	    		
	    	// Attiva il dialogo modale java standard di selezione file da file system	    		
	    	case DIALOG_START_FILE_SAVE_CHOOSER:     		
	    		isDone = true;
	    		execActionForStartDialogFileChooserManager(mcb, actionCoded, innerOnEvent);
	    		break;

	    	// Attiva il dialogo modale java standard di selezione colore
	    	case DIALOG_START_COLOR_CHOOSER:    			
	    		isDone = true;
	    		execActionForStartDialogColorChooserManager(mcb, actionCoded, innerOnEvent);
	    		break;

	    	// Attiva un dialogo modale forward di selezione font
	    	case DIALOG_START_FONT_CHOOSER:    				
	    		isDone = true;
	    		execActionForStartDialogFontChooserManager(mcb, actionCoded, innerOnEvent);
	    		break;
	
	    	// Attiva una funzione non modale applicativa  
	    	// Attiva una funzione modale applicativa  
	    	case DIALOG_START_USER_NO_MODAL: 				 
	    	case DIALOG_START_USER_MODAL: 					 
	    		execActionForStartDialogUserManager(mcb, actionCoded, innerOnEvent);
				isDone = true;
		    	break;

		    // Chiusura dialogo applicativo modale o non modale
	    	case DIALOG_CLOSE:							
	    		isDone = true;
 	    		dialogName = actionCoded.dialogName;
 	    		mcb.system.removeDialogActive(dialogName);
	    		break;

	    	// Chiusura di tutti i dialoghi aperti
	    	case DIALOG_CLOSE_ALL:							
	    		isDone = true;
 	    		for (String dialogActiveName : mcb.system.getDialogUserActiveNames()) {mcb.system.removeDialogActive(dialogActiveName);}
	    		break;

	    	// Move dialogo alla posizione X, Y
	    	case DIALOG_MOVE:								
	    		JDialog dialog = mcb.system.getDialogActive(actionCoded.dialogForm);
	    		if (dialog != null) {
		    		mcb.system.getDialogActive().setLocation(actionCoded.dialogPosX, actionCoded.dialogPosY);
				}
	    		break;
	    		
     	}
		return isDone;
	}
    

    
	/* -----------------------------------------------------
     * Gestione attivazione dialogo standard
     * -----------------------------------------------------
     * 
     * Dialogo di tipo modale Information, Error, Plain, input, ..
     * Dialogo applicativo modale
     * Dialogo applicativo NON modale
     * Registrazione listener sul monitor
     * Attivazione dialogo
     * Lettura valore opzione scelta fatta attraverso property listener
     * Lettura valore immesso da JTextField o selezionato da JComboBox 
     * Attivazione eventuale logica ON_RETURN_FROM_DIALOG
     */
    private void execActionForStartDialogStdManager(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, InnerOnEvent innerOnEvent) {
    	
    	ForwardDoParms actionParmsDialog = null;
    	JOptionPane optionPane = null;
    	ForwardDialog dialog = null;
 
      	// Parametri codificati in fase di dichiarazione
		actionParmsDialog = actionCoded;
		
		// Valorizzazione campi in System
		mcb.system.setDialogName(actionParmsDialog.dialogName);
		mcb.system.setDialogTitle(actionParmsDialog.dialogTitle);
		mcb.system.setDialogMessage(actionParmsDialog.dialogMessage);
		mcb.system.setDialogModal(actionParmsDialog.dialogModal);
		mcb.system.setDialogType(actionParmsDialog.dialogType);							// Per esempio JOptionPane.WARNING_MESSAGE
		mcb.system.setDialogTypeOptions(actionParmsDialog.dialogTypeOptions);					// Per esempio JOptionPane.OK_OPTION
		mcb.system.setDialogWithIcon(actionParmsDialog.isDialogWithIcon);
		mcb.system.setDialogWithIconUser(actionParmsDialog.isDialogWithIconUser);
		mcb.system.setDialogIconPath(actionParmsDialog.dialogIconPath);
		mcb.system.setDialogLabelsCustom(actionParmsDialog.dialogLabelsOptionCustom);
		mcb.system.setDialogLabelCustomSelected(actionParmsDialog.dialogLabelOptionCustomSelected);
		mcb.system.setDialogSelectionValues(actionParmsDialog.dialogSelectionValues);
		mcb.system.setDialogInitialSelectionValue(actionParmsDialog.dialogInitialSelectionValue);
		mcb.system.setDialogWantsInput(actionParmsDialog.isDialogWantsInput);
		mcb.system.setDialogActive(true);


		// Creazione OptionPanel standard WARNING/INFORMATION/INPUT/... e dialogo MODALE con cui visualizzarlo
		// Queasta modalità di attivazione NON permette la chiusura con X di titleBar
		// Il blocco della chiusura della finestra viene effettuata su propertyChange() di propertyChangeListener
		optionPane = new JOptionPane();
		optionPane.setName("forwardOptionPanel");												// Obbligatorio
		optionPane.setMessage(mcb.system.getDialogMessage());								// Obbligatorio (String)
		optionPane.setMessageType(mcb.system.getDialogType());								// Obbligatorio JOptionPane.ERROR_MESSAGE, INFORMATION_MESSAGE, WARNING_MESSAGE, QUESTION_MESSAGE, PLAIN_MESSAGE
		optionPane.setOptionType(mcb.system.getDialogTypeOptions());						// Obbligatorio JOptionPane.YES_NO_OPTION, YES_NO_CANCEL_OPTION, OK_CANCEL_OPTION, DEFAULT_OPTION
		optionPane.setIcon(mcb.system.getDialogIcon());									// Opzionale
		optionPane.setOptions(mcb.system.getDialogLabelsCustom());							// Opzionale
		optionPane.setInitialValue(mcb.system.getDialogLabelCustomSelected());				// Opzionale
		optionPane.setSelectionValues(mcb.system.getDialogSelectionValues());				// Opzionale
		optionPane.setInitialSelectionValue(mcb.system.getDialogInitialSelectionValue());	// Opzionale
		optionPane.setWantsInput(mcb.system.isDialogWantsInput());							// Opzionale
		
		// Nuovo Dialog
		dialog = new ForwardDialog(mcb.system.getActiveFrame(), mcb.system.getDialogName(), true, mcb.system.getDialogTitle());
        mcb.system.setDialogActive(dialog);
        dialog._setOptionPanel(optionPane);
        dialog.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        dialog.setContentPane(optionPane);
        
        // Listener per gestione eventi dialogo e optionPane
        dialog.addWindowListener(this);															// Su windowClosing().  Attivazione logiche se chiusura forzata con X
        optionPane.addPropertyChangeListener(this);												// Su propertyChange(). Attivazione logiche su conferma e prima di chiusura dialogo
        
        // Attivazione dialogo 
        dialog.pack();
        dialog.setLocationRelativeTo(mcb.system.getActiveFrame());
        dialog.setVisible(true);
		
		
		// Le logiche applicative ON_EVENT() sono richiamate nel gestore di eventi per PropertyChangeListener e WindowListener
		// Nello stesso gestore sono valorizzati i campi di system per il pulsante di opzione premuto e il valore eventualmente
		// immesso.
	}
    
    
	/* -----------------------------------------------------
     * Gestione attivazione dialogo applicativo
     * -----------------------------------------------------
     * 
     * Dialogo applicativo modale
     * Dialogo applicativo NON modale
     * Dialogo user 
     * Attivazione dialogo
     * 
     */
    private void execActionForStartDialogUserManager(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, InnerOnEvent innerOnEvent) {
    	
	  	ForwardDoParms actionParmsDialog = null;
	  	ForwardDialog dialog = null;
	  	ForwardForm formDialog = null;
	  	
     	// Parametri codificati in fase di dichiarazione
		actionParmsDialog = actionCoded;
		
		// Valorizzazione campi in System
		mcb.system.setDialogUser(true);
		mcb.system.setDialogName(actionParmsDialog.dialogName);
		mcb.system.setDialogTitle(actionParmsDialog.dialogTitle);
		mcb.system.setDialogUserFormName(actionParmsDialog.dialogForm);
		mcb.system.setDialogModal(actionParmsDialog.dialogModal);
	
		// Dialogo custom applicativo con form(JPanel) definito dall'utente.
		if (mcb.system.getActiveAction() == EnumForwardAction.DIALOG_START_USER_MODAL
		||  mcb.system.getActiveAction() == EnumForwardAction.DIALOG_START_USER_NO_MODAL) {
			formDialog = mcb.function.getForm(mcb.system.getDialogUserFormName());
			if (formDialog != null) {
				functionCreateMainFormPanel(mcb, formDialog);								// lay out panels structure recursively according to layout managers
 				mcb.system.setDialogUserForm(formDialog.getJrootPanel());					// Il form coincide con il jpanel principale
				// Dialogo Modale/Non Modale
				if (mcb.system.getActiveAction() == EnumForwardAction.DIALOG_START_USER_MODAL) {
					mcb.system.setDialogModal(true);
					dialog = new ForwardDialog(mcb.system.getActiveFrame(), actionParmsDialog.dialogForm, true, mcb.system.getDialogTitle());
					dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
				} else {
					mcb.system.setDialogModal(false);
					dialog = new ForwardDialog(mcb.system.getActiveFrame(), actionParmsDialog.dialogForm, false, mcb.system.getDialogTitle());
					dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
				}
				// Adjust dialogo e variabili di sistema
				formDialog.setDialog(dialog);												// JDialog in oggetto form
		        mcb.system.setDialogActive(dialog);											// JDialog attivo a livello di sistema
		        mcb.system.getDialogsUserActive().add(dialog);								// JDialog in list disloghi attivi
		        mcb.system.setDialogActive(true);											// Spento in chiusura window
		        mcb.system.getDialogUserForm().addPropertyChangeListener(this);				// Su propertyChange(). Attivazione logiche su conferma e prima di chiusura dialogo
		        dialog.setUndecorated(actionCoded.isDialogUndecorated);						// Decorazione frame o meno
		        dialog.setContentPane(mcb.system.getDialogUserForm());						// Form, ovvero JPanel applicativo da visualizzare
		        dialog.addWindowListener(this);												// Su windowClosing().  Attivazione logiche se chiusura forzata con X
		        dialog.pack();
		        // Posizione dialogo nello schermo
		        if (actionCoded.dialogPos == EnumForwardOption.DIALOG_ON_CENTER_PARENT) {
		        	dialog.setLocationRelativeTo(mcb.system.getActiveFrame());
				} else if (actionCoded.dialogPos == EnumForwardOption.DIALOG_ON_CENTER_SCREEN) {
					dialog.setLocationRelativeTo(null);
				} else {
					dialog.setLocation(actionCoded.dialogPosX, actionCoded.dialogPosY);
				}
		        // Visualizzazione dialogo
		        dialog.setVisible(true);
		        return;
			}
		} 
		
	}
    

	/* ----------------------------------------------------------------------
     * Gestione attivazione logiche applicative su opzione di dialogo scelta
     * ----------------------------------------------------------------------
     * 
     * Il dialogo può essere stato confermato con YES, NO, CANCEL, OK, CLOSED
     * In base al valore si attiva la corrispondente logica ON_EVENT() dichiarata
     */
    private void execActionForStartDialogLogicActivation(InnerMonitorControBlock mcb) {
		
    	// Esecuzione generica logiche su return dal dialogo
    	manageLogicDeclaredActivation(mcb, mcb.system.getDialogName(), EnumForwardEvent.ON_DIALOG_RETURN);
    	
    	// OK o YES
        if (mcb.system.getDialogOptionChoosed() == JOptionPane.OK_OPTION || mcb.system.getDialogOptionChoosed() == JOptionPane.YES_OPTION) {
           	manageLogicDeclaredActivation(mcb, mcb.system.getDialogName(), EnumForwardEvent.ON_DIALOG_YES_OK_OPTION);
           	return;
        }  

     	// NO
        if (mcb.system.getDialogOptionChoosed() == JOptionPane.NO_OPTION) {
           	manageLogicDeclaredActivation(mcb, mcb.system.getDialogName(), EnumForwardEvent.ON_DIALOG_NO_OPTION);
           	return;
        }  

    	// CANCEL
        if (mcb.system.getDialogOptionChoosed() == JOptionPane.CANCEL_OPTION) {
           	manageLogicDeclaredActivation(mcb, mcb.system.getDialogName(), EnumForwardEvent.ON_DIALOG_CANCEL_OPTION);
           	return;
        }  

    	// NO CHOICE non può succedere
        if (mcb.system.getDialogOptionChoosed() == JOptionPane.CLOSED_OPTION) {
           	manageLogicDeclaredActivation(mcb, mcb.system.getDialogName(), EnumForwardEvent.ON_DIALOG_CLOSING_ATTEMPTED);
           	return;
        }  

 	}

    /* --------------------------------------------------------------
     * Gestione attivazione dialogo chooser file 
     * --------------------------------------------------------------
     * 
     * Dialogo di tipo modale FileChooser, ColorChooser, FontChooser
     * Attivazione dialogo
     * Lettura valori restituiti e store in system
     * Attivazione logiche applicative
     * 
     */
    private void execActionForStartDialogFileChooserManager(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, InnerOnEvent innerOnEvent) {
    	ForwardDoParms parmStartDialogFileOpenChooser = null;
    	ForwardDoParms parmStartDialogFileSaveChooser = null;
     	JFileChooser jfileChooser = null;
    	int fileChooserResponse = 0;

		//  Valorizzazione campi in System  per Dialogo di open file di sistema
		if (actionCoded.action == EnumForwardAction.DIALOG_START_FILE_OPEN_CHOOSER) {
			parmStartDialogFileOpenChooser = actionCoded;
			// Valorizzazione campi in System 
			jfileChooser = new JFileChooser();
			mcb.system.setFileChooserActive(jfileChooser);
			mcb.system.setFileChooserName(parmStartDialogFileOpenChooser.fileChooserName);
			mcb.system.setFileChooserTitle(parmStartDialogFileOpenChooser.fileChooserTitle);
			mcb.system.setFileChooserFileSelectionMode(parmStartDialogFileOpenChooser.fileChooserFileSelectionMode);
			mcb.system.setFileChooserMultiSelectionEnabled(parmStartDialogFileOpenChooser.isFileChooserMultiSelectionEnabled);
			mcb.system.setDialogName(parmStartDialogFileOpenChooser.fileChooserName);
		} 
 	
		//  Valorizzazione campi in System  per Dialogo di save file di sistema
		if (actionCoded.action == EnumForwardAction.DIALOG_START_FILE_SAVE_CHOOSER) {
			parmStartDialogFileSaveChooser = actionCoded;
			// Valorizzazione campi in System 
			jfileChooser = new JFileChooser();
			mcb.system.setFileChooserActive(jfileChooser);
			mcb.system.setFileChooserName(parmStartDialogFileSaveChooser.fileChooserName);
			mcb.system.setFileChooserTitle(parmStartDialogFileSaveChooser.fileChooserTitle);
			mcb.system.setFileChooserFileSelectionMode(parmStartDialogFileSaveChooser.fileChooserFileSelectionMode);
			mcb.system.setFileChooserMultiSelectionEnabled(parmStartDialogFileSaveChooser.isFileChooserMultiSelectionEnabled);
			mcb.system.setDialogName(parmStartDialogFileSaveChooser.fileChooserName);
		} 
 
		// Attivazione dialogo
		jfileChooser = mcb.system.getFileChooserActive();
		if (actionCoded.action == EnumForwardAction.DIALOG_START_FILE_OPEN_CHOOSER) {
			fileChooserResponse = jfileChooser.showOpenDialog(mcb.system.getActiveFrame());
		} else {
			fileChooserResponse = jfileChooser.showSaveDialog(mcb.system.getActiveFrame());
		}
		mcb.system.setFileChooserResponse(fileChooserResponse);
		
		// Attivazione logica applicativa generica al ritorno da un dialogo
	    manageLogicDeclaredActivation(mcb, innerOnEvent.componentName, EnumForwardEvent.ON_DIALOG_RETURN);
		 
       	// Attivazione logiche applicative su eventi specifici
     	mcb.system.setFileChooserResponse(fileChooserResponse);
        if (fileChooserResponse == JFileChooser.APPROVE_OPTION) {
            manageLogicDeclaredActivation(mcb, innerOnEvent.componentName, EnumForwardEvent.ON_FILE_CHOOSER_APPROVE_OPTION);
            return;
        } 
        if (fileChooserResponse == JFileChooser.CANCEL_OPTION) {
            manageLogicDeclaredActivation(mcb, innerOnEvent.componentName, EnumForwardEvent.ON_FILE_CHOOSER_CANCEL_OPTION);
            return;
        } 
        if (fileChooserResponse == JFileChooser.ERROR_OPTION) {
            manageLogicDeclaredActivation(mcb, innerOnEvent.componentName, EnumForwardEvent.ON_FILE_CHOOSER_ERROR_OPTION);
            return;
        } 
			 
	}

    
    /* --------------------------------------------------------------
     * Gestione attivazione dialogo chooser color 
     * --------------------------------------------------------------
     * 
     * Dialogo di tipo modale ColorChooser, FontChooser
     * Attivazione dialogo
     * Lettura valori restituiti e store in system
     * Attivazione logiche applicative
     * 
     */
    private void execActionForStartDialogColorChooserManager(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, InnerOnEvent innerOnEvent) {
    	ForwardDoParms parmStartDialogColorChooser = null;
    	
		// Dialogo di selezione colore di sistema
		if (actionCoded.action == EnumForwardAction.DIALOG_START_COLOR_CHOOSER) {
			parmStartDialogColorChooser = actionCoded;
			mcb.system.setDialogName(parmStartDialogColorChooser.colorChooserName);
			mcb.system.setDialogName(parmStartDialogColorChooser.colorChooserName);

		    Color newColor = JColorChooser.showDialog(mcb.system.getActiveFrame(), parmStartDialogColorChooser.colorChooserTitle,  Color.BLACK);
			if (newColor != null) {
				mcb.system.setChooserColorSelected(newColor);
				
				// Attivazione logica applicativa generica al ritorno da un dialogo
			    manageLogicDeclaredActivation(mcb, innerOnEvent.componentName, EnumForwardEvent.ON_DIALOG_RETURN);
				 
			    // Logica specifica su colore selezionato
				manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_COLOR_CHOOSED);
			}
		}
		
	}

    /* --------------------------------------------------------------
     * Gestione attivazione dialogo chooser Font 
     * --------------------------------------------------------------
     * 
     * Dialogo di tipo modale ColorChooser, FontChooser
     * Attivazione dialogo
     * Lettura valori restituiti e store in system
     * Attivazione logiche applicative
     * 
     */
    private void execActionForStartDialogFontChooserManager(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, InnerOnEvent innerOnEvent) {
		 
		// Dialogo di selezione Font di sistema
		if (mcb.system.getActiveAction() == EnumForwardAction.DIALOG_START_FONT_CHOOSER) {
			// TODO
		}
		
	}


	/* --------------------------------------------------
     * Action per esecuzione logiche 
     * --------------------------------------------------
     * 
     * EXEC_REUSABLE_METHOD 
     * EXEC_REUSABLE_METHOD 
     * SKIP_EVENT_ACTIONS	 
     * SKIP_SET_ID	 
     * VAR_SET_VALUE	 
     * MESSAGE
     */
    private boolean execActionForExec(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
 
    	InnerOnEvent innerOnEventModel = null;
    	String messageLocalized = "";
    	boolean isDone = false;
		
     	// Azione già individuata e eseguita o exception in corso
     	if (mcb.system.getReusableMethodException() != null 
     	&& !mcb.system.isReusableMethodExceptionHandling())  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
    	
  		
 		switch (actionCoded.action) {
 			// Esegue metodo applicativo riusabile
			case EXEC_METHOD: 				
				isDone = true;
 				execReusableMethod(mcb, actionCoded);
                break;

 			// Esegue le action codificate per un altro evento dichiarato (subroutine)
			case EXEC_ACTIONS_ON_EVENT: 				
				isDone = true;
				innerOnEventModel = getEventDescriptor(mcb, actionCoded.actionEventComponentName, actionCoded.actionEvent);
				manageLogicDeclaredActivation(mcb, actionCoded.actionEventComponentName, actionCoded.actionEvent);
                break;
 
 			// Esegue un gruppo di action codificate (subroutine)
			case EXEC_ACTIONS_GROUP: 				
				isDone = true;
 				InnerActionsGroup innerActionsGroup = null;
 				innerActionsGroup = mcb.function.getGroupActionsMap().get(actionCoded.groupActionsName);
 				if (innerActionsGroup == null) {break;}
 				execActionsGroup(mcb, innerActionsGroup);
                break;
        			
                
            // Stop di tutte le action ancora da eseguire per l'evento corrente
			case SKIP_EVENT_ACTIONS: 				 
				isDone = true;
 				if (actionCoded.actionSkipId.equals(mcb.system.getActiveActionSkipId())) {
 					mcb.system.setActiveActionsToSkip(true);
				}
                break;
                
            // Set identificativo stop testato da STOP_EVENT_ACTIONS
			case SKIP_SET_ID: 				 
				isDone = true;
 				mcb.system.setActiveActionSkipId(actionCoded.actionSkipId);
                break;

            // Impostazione variabile dichiarata con VAR()
 			case VAR_SET_VALUE: 				 
 				isDone = true;
				if (!mcb.function.isVarDeclared(actionCoded.varName)) {break;}
				// Valore embedded
				if (actionCoded.varValue != null) {
					mcb.function.setVarValue(actionCoded.varName, actionCoded.varValue);
					break;
				}
				// Valore in variabile
				mcb.function.setValueVarFromVar(actionCoded.varName, actionCoded.varValueComponentName);
                break;
     				

            // Recupero e impostazione messaggio codificato in rule table
			case MESSAGE: 				 
				isDone = true;
 				messageLocalized = getMessage(actionCoded.messageCode, actionCoded.messageType, actionCoded.messageLanguage);
                // Impostazione controllo GUI e/o variabile con lo stesso nome
				mcb.function.setValueControlGui(actionCoded.messageComponentName, messageLocalized);
				setVarValue(mcb, actionCoded.messageComponentName, messageLocalized);
 				break;

            // Action dummy, per esempio per nooperizzare eventi standard
			case NOTHING: 				 
				isDone = true;
  				break;

 		}	
		return isDone;
	}

	/**
	 * Returns the localized message and, if not found, the message code between "??".<br>
	 * <p>
	 * The message key is the message type as a prefix MW, MI, DG, DB, EI, ET, EF, TC followed by a numeric string value of 5 digits size.<br>
	 * There is a forward table for each message type.<br>
	 * Depending on the message type, it will be read a specific messages table.<br>
	 * Every table contains both system messages and application function messsages.<br>
	 * Application function messages start from the code <code>10000</code><br>
	 * <p>
	 * 
	 * @param messageCode the numeric string, five digits to identify the message
	 * @param messageType a {@link EnumMessageType} enumeration with the type of message
	 * @param messageLanguage a {@link EnumLanguage} enumeration for the message language
	 * 
	 */
	public String getMessage(String messageCode, EnumMessageType messageType, EnumLanguage messageLanguage) {

		Object ar_objEntityTableData[] = null;
		EntityTableData entityTableData = null;
	    String whereCondition = "";
	    String kPrefix = "";
		String kVal = "";
		int language = 0;
		int numTable = 0;

		// Linguaggio esplicitamente assegnato
		if (messageLanguage != EnumLanguage.NOT_ASSIGNED) {
			language = messageLanguage.ordinal();
		} else {
			// Messaggio dichiarato nella funzione o personalizzato al login
			language = mcb.system.getActiveLanguage().ordinal();
		}
		
		// Impostazione numero tabella con il messaggio corretto
		switch (messageType) {
			case WARNING:
//				numTable = EnumTable.EnumMsgWarning.getNumTable();
				kPrefix = "MW";
				break;
			case INFORMATION:
//				numTable = EnumTable.EnumMsgInformation.getNumTable();
				kPrefix = "MI";
				break;
			case ERROR_INPUT:
//				numTable = EnumTable.EnumMsgErrorInput.getNumTable();
				kPrefix = "ET";
				break;
			case ERROR_INTERNAL:
//				numTable = EnumTable.EnumMsgErrorInternal.getNumTable();
				kPrefix = "EI";
				break;
			case ERROR_FATAL:
//				numTable = EnumTable.EnumMsgErrorFatal.getNumTable();
				break;
			case ERROR_DATABASE:
//				numTable = EnumTable.EnumMsgErrorDb.getNumTable();
				kPrefix = "DB";
				break;
			case TEXT:
//				numTable = EnumTable.EnumMsgText.getNumTable();
				kPrefix = "TC";
				break;
			case DEBUG:
//				numTable = EnumTable.EnumMsgDebug.getNumTable();
				kPrefix = "DG";
				break;
			default:
				break;
		}
		
		// Chiave di accesso
		kVal = kPrefix + messageCode;
		
    	// Composizione Where di lettura EntityCustomization
     	whereCondition = whereCondition +   "     TBDTSYST = '*'";
     	whereCondition = whereCondition +   " AND TBDTSUBS = '*'";
     	whereCondition = whereCondition +   " AND TBDTLANG = " + language;
     	whereCondition = whereCondition +   " AND TBDTTTAB = " + numTable;
     	whereCondition = whereCondition +   " AND TBDTKVAL = '" + kVal + "'";
		entityTableData = new EntityTableData();

     	try {
			dbs = new DataBaseStatusDetailed();
			dbConn = dbm.getConnection(dbs);
			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
			
			ar_objEntityTableData = dbei.readSetEntity(entityTableData, whereCondition, "");
			
			dbei.commit();
			dbm.releaseConnection(dbConn, dbs);
			if (ar_objEntityTableData.length == 0) {return "";}
			entityTableData = (EntityTableData) ar_objEntityTableData[0];
			return entityTableData.getRowData();
			
		} catch (ExceptionAmritaSqlError e) {
			// Logging già effettuato da DataBaseManager
		} catch (SQLException e) {
			// Logging già effettuato da DataBaseManager
		} catch (ExceptionAmrita e) {
			// Logging già effettuato da DataBaseManager
		}
     	return "?? " + messageCode + " ??";
	}

    
	/* --------------------------------------------------
     * Action per logical data view
     * --------------------------------------------------
     * 
     */
    private boolean execActionForLdv(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
		ForwardLogicalDataView ldvObject = null;
		InnerOnEvent eventLdvErrorExecution = null;
		InnerComponent innerComponent = null;
 		Class<?> columnClass = null;
		Object varObject = null;
		Object varObjectColumn = null;
		Object valueObject = null;
		EnumLanguage language = null;
		int ruleTableNum = 0;
        int retCode = 0;
  		boolean isDone = false;

     	// Azione già individuata e eseguita o exception in corso
     	if (mcb.system.getReusableMethodException() != null 
     	&& !mcb.system.isReusableMethodExceptionHandling())  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
    	
		// Eventuale ON_EVENT() dichiarata su condizione di errore action di accesso ai dati
		eventLdvErrorExecution = getEventDescriptor(mcb, actionCoded.ldvClassName, EnumForwardEvent.ON_LDV_ERROR_EXECUTION); // Recupero per ottimizzazione evento
 
 		switch (actionCoded.action) {
	 		
			// Istanzia una logical data view, esegue il metodo declare()
			case LDV_CREATE: 								
				isDone = true;
				retCode = execActionLdvCreateCommon(mcb, actionCoded);			// Imposta return codes in system generali e di ldv
				if (retCode > 0) {manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);}
				break;
			
			// Valida la logical data view e crea lo statement sql
			case LDV_VALIDATE: 								
				isDone = true;
				retCode = execActionLdvValidateCommon(mcb, actionCoded);    	 // Imposta return codes in system generali e di ldv
				if (retCode > 0) {manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);}
				break;
	
			// Esegue la logical data view e crea il recordset
			case LDV_EXECUTE: 								
				isDone = true;
				retCode = execActionLdvExecuteCommon(mcb, actionCoded);
				if (retCode > 0) {manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);}
				break;

			// Esegue in sequenza CREATE, VALIDATE ed EXECUTE
			case LDV_RUN: 								
				isDone = true;
				retCode = execActionLdvCreateCommon(mcb, actionCoded);
				if (retCode > 0) {manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);break;}
				retCode = execActionLdvValidateCommon(mcb, actionCoded);
				if (retCode > 0) {manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);break;}
				retCode = execActionLdvExecuteCommon(mcb, actionCoded);
				if (retCode > 0) {manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);}				
				break;

			// Imposta il numero di righe di una pagina logica 
			case LDV_SET_PAGE_ROWS: 								
				isDone = true;
				retCode = ldvInitialLoad(mcb, actionCoded);					// Reset return codes, load ldv, logiche su errore
				if (retCode > 0) {break;}							
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				mcb.system.setLdvName(actionCoded.ldvClassName);
				mcb.system.setLdvObject(ldvObject);
				ldvObject.setPageRows(actionCoded.ldvPageRows);
				break;
			
				
			// Popolamento GUI con i valori correnti della logical data view
	 		case LDV_SET_FUNCTION_GUI_CONTROLS: 						
				isDone = true;
				ldvSetGuiControlsOrFunctionVars(mcb, actionCoded.ldvClassName, true);
				break;  
			
			// Imposta la variabili della funzione con il valore delle variabili con lo stesso nome della logical data view
	 		case LDV_SET_FUNCTION_VARS: 					
				isDone = true;
				ldvSetGuiControlsOrFunctionVars(mcb, actionCoded.ldvClassName, false);
				break;
			
			// Impostazione controllo GUI con il valore corrente del campo della logical data view con lo stesso nome
	 		case LDV_SET_FUNCTION_GUI_CONTROL:
				isDone = true;
				retCode = ldvInitialLoad(mcb, actionCoded);					// Reset return codes, load ldv, logiche su errore
				if (retCode > 0) {break;}							
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				// Lettura componente GUI e colonna ldv
				innerComponent = mcb.function.getComponentDeclared(actionCoded.ldvFunctionControlName);
				varObjectColumn = ldvObject.getValue(actionCoded.ldvColumnName);									// Valore corrente variabile colonna
				if (innerComponent == null) {break;}
		        if (varObjectColumn == null) {break;}
				// Update componente GUI swing con valore colonna
				columnClass = varObjectColumn.getClass();															 
				updateGUIControlsOrVariables(mcb, varObjectColumn, columnClass, innerComponent, true); 				// Update GUI control con lo stesso nome se compatibile
	 			break;
	 			
	 		// Impostazione variabile funzione con il valore corrente del campo della logical data view con lo stesso nome
	 		case LDV_SET_FUNCTION_VAR:
				isDone = true;
		        // Recupero vista logica
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				if (ldvObject == null) {break;}
				// Lettura componente GUI e colonna ldv
				innerComponent = mcb.function.getVarDeclared(actionCoded.ldvFunctionVarName);
				varObject = ldvObject.getValue(actionCoded.ldvColumnName);									// Valore corrente variabile colonna
				if (innerComponent == null) {break;}
		        if (varObject == null) {break;}
				// Update variabile funzione con valore colonna
				setVarValue(mcb, actionCoded.ldvFunctionVarName, varObject);										// Update variabile di funzione se compatibile
	 			break;
	 			
			// Imposta i campi chiave della FOR_ANY() root della logical data view da controlli GUI definiti con lo stesso nome
	 		case LDV_SET_FIELDS_KEY_ROOT_FROM_GUI: 		   	
				isDone = true;
				ldvSetFieldsKeyRootFromGuiOrVar(mcb, actionCoded, true, true);
				break;
			
			// Imposta i campi chiave FOR_ANY() root della logical data view da VAR() definite con lo stesso nome
	 		case LDV_SET_FIELDS_KEY_ROOT_FROM_VAR: 		   	
				isDone = true;
				ldvSetFieldsKeyRootFromGuiOrVar(mcb, actionCoded, true, false);
				break;
			
			// Imposta i campi chiave della logical data view da controlli GUI definiti con lo stesso nome
	 		case LDV_SET_FIELDS_KEY_FROM_GUI: 		   		
				isDone = true;
				ldvSetFieldsFromGuiOrVar(mcb, actionCoded, true, true);
				break;
			 
			// Imposta i campi NON chiave della logical data view da controlli GUI definiti con lo stesso nome
	 		case LDV_SET_FIELDS_NOT_KEY_FROM_GUI: 			
				isDone = true;
				ldvSetFieldsFromGuiOrVar(mcb, actionCoded, false, true);
				break;
		 		
			// Imposta i campi chiave della logical data view da VAR() definite con lo stesso nome
	 		case LDV_SET_FIELDS_KEY_FROM_VAR: 				
				isDone = true;
				ldvSetFieldsFromGuiOrVar(mcb, actionCoded, true, false);
				break;
	 	
			// Imposta i campi NON chiave della logical data view da VAR() definite con lo stesso nome
	 		case LDV_SET_FIELDS_NOT_KEY_FROM_VAR: 			
				isDone = true;
				ldvSetFieldsFromGuiOrVar(mcb, actionCoded, false, false);
				break;
			
			// Imposta il campo, colonna della logical data view
	 		case LDV_SET_FIELD: 							
				isDone = true;
				ldvSetField(mcb, actionCoded);
				break;
				
			// Imposta la variabile della logical data view utilizzata come host var	
	 		case LDV_SET_VAR: 								
				isDone = true;
				ldvSetVar(mcb, actionCoded);
				break;
				
			// Imposta il numero tabella di rule table 	
	 		case LDV_SET_RULE_TABLE_NUM: 								
				isDone = true;
				retCode = ldvInitialLoad(mcb, actionCoded);					// Reset return codes, load ldv, logiche su errore
				if (retCode > 0) {break;}							
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				if (actionCoded.ldvRuleTableNum >= 0) {
					ldvObject.setRuleTableNum(actionCoded.ldvRuleTableNum);
					break;
				}
				Object value = null;
				value = mcb.function.getValueInt(actionCoded.ldvVarName);
				if (value == null) {break;}											// Variabile/controllo GUI non definito o non riconducibile a un intero
				ruleTableNum = (Integer) value;
				ldvObject.setRuleTableNum(ruleTableNum);
				break;

			// Imposta il numero tabella di rule table 	
	 		case LDV_SET_SQL_WHERE: 								
				isDone = true;
				// Reset return codes
				mcb.system.setReturnCode(0);;
				mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_OK.ordinal());
				// Recupero ldv già creata
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				mcb.system.setLdvName(actionCoded.ldvClassName);
				if (ldvObject == null) {break;}
				// Impostazione condizione di where
				mcb.system.setLdvObject(ldvObject);
				String sqlWhere = "";
				// Recupero stringa sql where embedded o da variabile
				if (actionCoded.ldvVarName.equals("")) {
					sqlWhere = actionCoded.ldvWhereSql;
				} else {
					if (!mcb.function.isVarDeclared(actionCoded.ldvVarName)) {break;}
					sqlWhere = mcb.function.getValueString(actionCoded.ldvVarName);
				}
				// Impostazione stringa sql where
				if (actionCoded.ldvEntityIndex >= 0) {
					ldvObject.setWhereSql(actionCoded.ldvEntityIndex, sqlWhere);
				} else {
					ldvObject.setWhereSql(actionCoded.ldvEntityNameAs, sqlWhere);
				}
				break;
				
				
			// Imposta il linguaggio come ordinal di EnumLanguage, valido per accesso a rule table 	
	 		case LDV_SET_LANGUAGE: 								
				isDone = true;
				retCode = ldvInitialLoad(mcb, actionCoded);					// Reset return codes, load ldv, logiche su errore
				if (retCode > 0) {break;}							
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				ldvObject.setPageRows(actionCoded.ldvPageRows);
				// Language da valore codificato nella action o da variabile di funzione
				if (actionCoded.ldvFunctionVarName.equals("")) {
					language = actionCoded.ldvLanguage;
				} else {
					valueObject = mcb.function.getValueVar(actionCoded.ldvFunctionVarName);
					if (valueObject == null) {break;}
					if (!(valueObject instanceof EnumLanguage)) {break;}
					language = (EnumLanguage) valueObject;
				}
		        // Update variabile corrente colonna in logical data view
		        ldvObject.setRuleTableLanguage(language);
				break;
			
			// Elimina le righe e rilascia le risorse allocate
	 		case LDV_CLEAR: 								
				isDone = true;
				retCode = ldvInitialLoad(mcb, actionCoded);					// Reset return codes, load ldv, logiche su errore
				if (retCode > 0) {break;}							
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				// Clear, eliminazione righe
				ldvObject.clear();
				break;
			 
			// Legge una riga specifica 
	 		case LDV_READROW: 								
				isDone = true;
				// Operazioni comuni prima di operazione di lettura
				retCode = ldvReadCommonBefore(mcb, actionCoded);
				if (retCode > 0) {break;}
				// Read
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
				retCode = ldvObject.read(actionCoded.ldvRow);
				// Operazioni comuni dopo operazione di lettura
				ldvReadCommonAfter(mcb, actionCoded, retCode);
				break;  
			
			// Legge la prìma riga
	 		case LDV_READFIRST: 								
				isDone = true;
				// Operazioni comuni prima di operazione di lettura
				retCode = ldvReadCommonBefore(mcb, actionCoded);
				if (retCode > 0) {break;}
				// ReadFirst
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
				retCode = ldvObject.readFirst();
				// Operazioni comuni dopo operazione di lettura
				ldvReadCommonAfter(mcb, actionCoded, retCode);
				break;
				
			// Legge l'ultima riga 	
	 		case LDV_READLAST: 								
				isDone = true;
				// Operazioni comuni prima di operazione di lettura
				retCode = ldvReadCommonBefore(mcb, actionCoded);
				if (retCode > 0) {break;}
				// ReadLast
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
				retCode = ldvObject.readLast();
				// Operazioni comuni dopo operazione di lettura
				ldvReadCommonAfter(mcb, actionCoded, retCode);
				break;
			
			// Legge la riga successiva a quella corrente
	 		case LDV_READNEXT: 								
				isDone = true;
				// Operazioni comuni prima di operazione di lettura
				retCode = ldvReadCommonBefore(mcb, actionCoded);
				if (retCode > 0) {break;}
				// ReadLast
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
				retCode = ldvObject.readNext();
				// Operazioni comuni dopo operazione di lettura
				ldvReadCommonAfter(mcb, actionCoded, retCode);
				break;
			
			// Legge la riga precedente a quella corrente
	 		case LDV_READPREV: 								
				isDone = true;
				// Operazioni comuni prima di operazione di lettura
				retCode = ldvReadCommonBefore(mcb, actionCoded);
				if (retCode > 0) {break;}
				// ReadPrev
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
				retCode = ldvObject.readPrev();
				// Operazioni comuni dopo operazione di lettura
				ldvReadCommonAfter(mcb, actionCoded, retCode);
				break;
			
			// Aggiorna tutta la logical data view su db o solo l'entity specificata
	 		case LDV_UPDATE: 								
				isDone = true;
				// Operazioni comuni prima di operazioni di update, insert, delete
				retCode = ldvUpdInsDelCommonBefore(mcb, actionCoded, EnumForwardEvent.ON_LDV_BEFORE_UPDATE);
				if (retCode > 0) {break;}
				// Update fisico su db
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
				if (actionCoded.ldvEntityName.equals("")) {
					retCode = ldvObject.update();								// Update di tutte le entity
				} else {
					retCode = ldvObject.update(actionCoded.ldvEntityNameAs); 	// Update di entity specifica
				}
				// Operazioni comuni dopo operazione di insert/delete/update
				ldvUpdInsDelCommonAfter(mcb, actionCoded, retCode, EnumForwardEvent.ON_LDV_AFTER_UPDATE, EnumForwardEvent.ON_LDV_UPDATE_NOTFOUND);
				break;
			
			// Inserisce l'entity specificata
	 		case LDV_INSERT: 								
				isDone = true;
				// Operazioni comuni prima di operazioni di update, insert, delete
				retCode = ldvUpdInsDelCommonBefore(mcb, actionCoded, EnumForwardEvent.ON_LDV_BEFORE_INSERT);
				if (retCode > 0) {break;}
				// Insert fisico su db
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
				retCode = ldvObject.insert(actionCoded.ldvEntityNameAs);
				// Operazioni comuni dopo operazione di insert/delete/update
				ldvUpdInsDelCommonAfter(mcb, actionCoded, retCode, EnumForwardEvent.ON_LDV_AFTER_INSERT, EnumForwardEvent.ON_LDV_INSERT_DUPLICATE);
				break;
			
			// Deleta l'entity specificata
	 		case LDV_DELETE:    							
				isDone = true;
				// Operazioni comuni prima di operazioni di update, insert, delete
				retCode = ldvUpdInsDelCommonBefore(mcb, actionCoded, EnumForwardEvent.ON_LDV_BEFORE_DELETE);
				if (retCode > 0) {break;}
				// Delete fisico su db
				ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
				retCode = ldvObject.delete(actionCoded.ldvEntityNameAs);
				// Operazioni comuni dopo operazione di insert/delete/update
				ldvUpdInsDelCommonAfter(mcb, actionCoded, retCode, EnumForwardEvent.ON_LDV_AFTER_DELETE, EnumForwardEvent.ON_LDV_DELETE_NOTFOUND);
				break;
			
			// Imposta parametri generali di accesso al data base come limitazione al numero righe, commit automatica, etc.
			case LDV_DB_SET_ACCESS_PARMS: 								
				isDone = true;
				retCode = ldvInitialLoad(mcb, actionCoded);					// Reset return codes, load ldv, logiche su errore
				if (retCode > 0) {break;}							
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				// Impostazione parametri
				mcb.system.setLdvName(actionCoded.ldvClassName);
				mcb.system.setLdvObject(ldvObject);
				ldvObject.setLimitRows(actionCoded.ldvLimitRows);
				ldvObject.setDbAutoCommit(actionCoded.ldvDbAutoCommit);
				ldvObject.setDbAutoConnection(actionCoded.ldvDbAutoConnection);
				break;
				
			// Acquisisce una nuova connessione al database e memorizza il reference nella logical data view
			case LDV_DB_GET_CONNECTION: 								
				isDone = true;
				retCode = ldvInitialLoad(mcb, actionCoded);					// Reset return codes, load ldv, logiche su errore
				if (retCode > 0) {break;}							
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				// Acquisizione connessione
				mcb.system.setLdvName(actionCoded.ldvClassName);
				mcb.system.setLdvObject(ldvObject);
				retCode = ldvObject.getDbConnection();
				mcb.system.setDbs(ldvObject.getDbs());
				if (retCode > 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
				if (eventLdvErrorExecution != null && mcb.system.getReturnCode() > 0) {logicExecution(mcb, eventLdvErrorExecution);}
				break;
				 
			// Rilascia la connessione corrente al database e azzera il reference nella logical data view
			case LDV_DB_RELEASE_CONNECTION: 								
				isDone = true;
				retCode = ldvInitialLoad(mcb, actionCoded);					// Reset return codes, load ldv, logiche su errore
				if (retCode > 0) {break;}							
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				// Rilascio connessione
				mcb.system.setLdvName(actionCoded.ldvClassName);
				mcb.system.setLdvObject(ldvObject);
				retCode = ldvObject.releaseDbConnection();
				mcb.system.setDbs(ldvObject.getDbs());
				if (retCode > 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
				if (eventLdvErrorExecution != null && mcb.system.getReturnCode() > 0) {logicExecution(mcb, eventLdvErrorExecution);}
				break;
				
			// Effettua una commit al data base
			case LDV_DB_COMMIT: 								
				isDone = true;
				retCode = ldvInitialLoad(mcb, actionCoded);					// Reset return codes, load ldv, logiche su errore
				if (retCode > 0) {break;}							
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				// Esecuzione commit
				mcb.system.setLdvName(actionCoded.ldvClassName);
				mcb.system.setLdvObject(ldvObject);
				retCode = ldvObject.dbCommit();
				mcb.system.setDbs(ldvObject.getDbs());
				if (retCode > 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
				if (eventLdvErrorExecution != null && mcb.system.getReturnCode() > 0) {logicExecution(mcb, eventLdvErrorExecution);}
				break;
					
			// Effettua una rollback al data base
			case LDV_DB_ROLLBACK: 								
				isDone = true;
				retCode = ldvInitialLoad(mcb, actionCoded);					// Reset return codes, load ldv, logiche su errore
				if (retCode > 0) {break;}							
				ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
				// Esecuzione rollback
				mcb.system.setLdvName(actionCoded.ldvClassName);
				mcb.system.setLdvObject(ldvObject);
				retCode = ldvObject.dbRollback();
				mcb.system.setDbs(ldvObject.getDbs());
				if (retCode > 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
				if (eventLdvErrorExecution != null && mcb.system.getReturnCode() > 0) {logicExecution(mcb, eventLdvErrorExecution);}
				break;
						
      	}
		return isDone;
	}


    /*
     * Operazioni inziali di recupero vista logica
     * - Attivazione logiche in caso di errore
     */
	private int ldvInitialLoad(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
		ForwardLogicalDataView ldvObject = null;
		int retCode = 0;
		
		// Reset return codes
		mcb.system.setReturnCode(0);;
		mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_OK.ordinal());
		
		// Nessuna create precedente per la ldv
		ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
		mcb.system.setLdvName(actionCoded.ldvClassName);
		if (ldvObject == null) {
			retCode = EnumForwardReturnCode.RC_ERROR_LDV.ordinal();
			mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_ERROR_NOT_DECLARED.ordinal());
			mcb.system.setLdvObject(null);
			manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);
			return retCode;
		}
		mcb.system.setLdvObject(ldvObject);
		return retCode;
	}


	/*
     * Parte comune finale a tutte le operazioni di insert/delete/update logical data view.
     * 
     * - Esecuzione logiche in caso di errore fisico
     * - Esecuzione logiche dopo dell'aggiornamento, con l'id operazione fornito
     */
	private void ldvUpdInsDelCommonAfter(InnerMonitorControBlock mcb
									   , ForwardDoParms actionCoded
									   , int retCode
									   , EnumForwardEvent onLdvAfter     				// ON_LDV_AFTER_UPDATE,    ON_LDV_AFTER_INSERT,     ON_LDV_AFTER_DELETE
									   , EnumForwardEvent onLdvFoundNotfound			// ON_LDV_UPDATE_NOTFOUND, ON_LDV_INSERT_DUPLICATE, ON_LDV_DELETE_NOTFOUND
									    ) {
		
	   ForwardLogicalDataView ldvObject = null;

	   ldvObject = (ForwardLogicalDataView) mcb.system.getLdvObject();
	   
	   // Reset return codes
	   mcb.system.setLdvReturnCode(retCode);
	   mcb.system.setReturnCode(0);;
	   mcb.system.setDbs(ldvObject.getDbs());
	
	   // La vista logica ha restituito un errore
	   if (mcb.system.getLdvReturnCode()  > 0 
	   &&  mcb.system.getLdvReturnCode() !=  EnumForwardLogicalDataViewControl.LDV_ERROR_NOTFOUND.ordinal()
	   &&  mcb.system.getLdvReturnCode() !=  EnumForwardLogicalDataViewControl.LDV_ERROR_DUPLICATE.ordinal()) {
		   manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);
		   mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());
		   return;
	   }

	   // Update/insert/delete effettuato senza errori fisici 
	   
	   // Esecuzione logiche dopo update/insert/delete andato a buon fine
	   if (retCode == 0) {
		   manageLogicDeclaredActivation(mcb, actionCoded.ldvIdOperation, onLdvAfter);
		   return;
	   }
	   
	   // Esecuzione logiche dopo update di riga inesistente, insert di riga esistente o delete di riga inesistente
	   if (retCode == EnumForwardLogicalDataViewControl.LDV_ERROR_NOTFOUND.ordinal()
	   ||  retCode == EnumForwardLogicalDataViewControl.LDV_ERROR_DUPLICATE.ordinal()) {
		   manageLogicDeclaredActivation(mcb, actionCoded.ldvIdOperation, onLdvFoundNotfound);
		   return;
	   }
	}


	/*
     * Parte comune iniziale a tutte le operazioni di insert/delete/update logical data view.
     * 
     * - Caricamento iniziale logical data view
     * - Esecuzione logiche prima dell'aggiornamento, con l'id operazione fornito
     */
	private int ldvUpdInsDelCommonBefore(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, EnumForwardEvent onLdvBefore) {
		ForwardLogicalDataView ldvObject = null;
		
		// Reset return codes
		mcb.system.setReturnCode(0);;
		mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_OK.ordinal());
		
		// Test di esistenza e consistenza
		ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
		if (ldvObject == null) {
			mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_ERROR_NOT_DECLARED.ordinal());
			mcb.system.setLdvName(actionCoded.ldvClassName);
			mcb.system.setLdvObject(null);
			manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);
			return EnumForwardLogicalDataViewControl.LDV_ERROR_NOT_DECLARED.ordinal();
		}
		
		// Esecuzione logiche prima di update o insert o delete
		mcb.system.setLdvName(actionCoded.ldvClassName);
		mcb.system.setLdvEntityAs(actionCoded.ldvEntityNameAs);
		mcb.system.setLdvObject(ldvObject);
		manageLogicDeclaredActivation(mcb, actionCoded.ldvIdOperation, onLdvBefore);
		return 0;
	}


	/*
     * Parte comune iniziale a tutte le operazioni di lettura logical data view.
     * 
     * - Caricamento iniziale logical data view
     * - Esecuzione logiche prima della lettura, con l'id operazione fornito
     */
	private int ldvReadCommonBefore(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
		ForwardLogicalDataView ldvObject = null;
		
		// Reset return codes
		mcb.system.setReturnCode(0);;
		mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_OK.ordinal());
		
		// Test di esistenza e consistenza
		ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
		if (ldvObject == null) {
			mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_ERROR_NOT_DECLARED.ordinal());
			mcb.system.setLdvName(actionCoded.ldvClassName);
			mcb.system.setLdvObject(null);
			manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);
			return EnumForwardLogicalDataViewControl.LDV_ERROR_NOT_DECLARED.ordinal();
		}
		
		// Esecuzione logiche prima di read
		mcb.system.setLdvName(actionCoded.ldvClassName);
		mcb.system.setLdvEntityAs(actionCoded.ldvEntityNameAs);
		mcb.system.setLdvObject(ldvObject);
		mcb.system.setLdvClass(ldvObject.getClass());
		manageLogicDeclaredActivation(mcb, actionCoded.ldvIdOperation, EnumForwardEvent.ON_LDV_BEFORE_GET);
		
		return 0;
	}

	/*
     * Parte comune eseguita dopo tutte le operazioni di lettura logical data view.
     * 
     * - Esecuzione logiche su errore, se presenti errori
     * - Esecuzione logiche dopo lettura per trovato e non trovato
     */
   private void ldvReadCommonAfter(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, int retCode) {
		
	   // Reset return codes
	   mcb.system.setLdvReturnCode(retCode);
	   mcb.system.setReturnCode(0);;
	
	   // La vista logica ha restituito un errore, per esempio non è stata validata
	   if (mcb.system.getLdvReturnCode()  > 0 
	   &&  mcb.system.getLdvReturnCode() != EnumForwardLogicalDataViewControl.LDV_ERROR_INDEX_OUT_OF_BOUND.ordinal()
	   &&  mcb.system.getLdvReturnCode() != EnumForwardLogicalDataViewControl.LDV_EOF.ordinal()) {
		   manageLogicDeclaredActivation(mcb, "", EnumForwardEvent.ON_LDV_ERROR_EXECUTION);
		   mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());
		   return;
	   }
	   
	   // Riga trovata o non trovata
	   
	   // Esecuzione logiche su evento ON_LDV_READ_NOTFOUND
	   if (retCode > 0) {
		   manageLogicDeclaredActivation(mcb, actionCoded.ldvIdOperation, EnumForwardEvent.ON_LDV_READ_NOTFOUND);
		   return;
	   }
		
	   // Esecuzione logiche dopo read e riga trovata
	    manageLogicDeclaredActivation(mcb, actionCoded.ldvIdOperation, EnumForwardEvent.ON_LDV_AFTER_READ);
		
	}


	/*
     * Gestion action LDV_CREATE
     */
	private int execActionLdvCreateCommon(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
		ForwardLogicalDataView ldvObject = null;
		int retCode = 0;
		
		// Reset return codes
		mcb.system.setReturnCode(0);
		mcb.system.setLdvReturnCode(0);
		
		// Logical data view già creata
		ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
		if (ldvObject != null) {
			return retCode;
		}
		// Creazione logical data view
		retCode = ldvCreate(mcb, actionCoded);
		mcb.system.setLdvReturnCode(retCode);
		if (mcb.system.getLdvReturnCode() > 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
		return retCode;
	}
  
	/*
     * Gestion action LDV_VALIDATE
     */
	private int execActionLdvValidateCommon(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
		
		ForwardLogicalDataView ldvObject = null;
		int retCode = 0;
		
		// Reset return codes
		mcb.system.setReturnCode(0);
		mcb.system.setLdvReturnCode(0);
	
		// Nessuna Create eseguita per la ldv precedentemente
		ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
		if (ldvObject == null) {
			retCode = EnumForwardReturnCode.RC_ERROR_LDV.ordinal();
			mcb.system.setReturnCode(retCode);
			mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_ERROR_NOT_DECLARED.ordinal());
			mcb.system.setLdvName(actionCoded.ldvClassName);
			mcb.system.setLdvObject(null);
			mcb.system.setLdvClass(null);
			return retCode;
		}
		
		// Dati correnti ldv attiva
		mcb.system.setLdvName(actionCoded.ldvClassName);
		mcb.system.setLdvObject(ldvObject);
		mcb.system.setLdvClass(ldvObject.getClass());
		
		// Ldv già caricata da una Create precedemte, si può fare la validate
		retCode = ldvObject.validate();
		mcb.system.setLdvReturnCode(retCode);
		if (retCode > 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
		return retCode;
	}

	/*
     * Gestione action LDV_EXECUTE
     */
	private int execActionLdvExecuteCommon(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
		ForwardLogicalDataView ldvObject = null;
		int retCode = 0;

		// Reset return codes
		mcb.system.setReturnCode(0);
		mcb.system.setLdvReturnCode(0);
	
		// Nessuna Create eseguita per la ldv precedentemente
		ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
		if (ldvObject == null) {
			retCode = EnumForwardReturnCode.RC_ERROR_LDV.ordinal();
			mcb.system.setReturnCode(retCode);
			mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_ERROR_NOT_DECLARED.ordinal());
			return retCode;
		}
		
		// ldv definita, esecuzione logiche prima di esecuzione
		mcb.system.setLdvName(actionCoded.ldvClassName);
		mcb.system.setLdvObject(ldvObject);
		mcb.system.setLdvClass(ldvObject.getClass());
	    manageLogicDeclaredActivation(mcb, actionCoded.ldvClassName, EnumForwardEvent.ON_LDV_BEFORE_EXECUTE);
		
	    // Esecuzione ldv
	    retCode = ldvObject.execute();
		mcb.system.setLdvReturnCode(retCode);
		mcb.system.setReturnCode(0);
		mcb.system.setDbs(ldvObject.getDbs());
		if (retCode > 0) {mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());}
		if (retCode > 0) {return retCode;}
		
		// Esecuzione logiche su evento ON_LDV_RUN_NO_ROWS
		if (ldvObject.getCountRows() == 0) {
			manageLogicDeclaredActivation(mcb, actionCoded.ldvClassName, EnumForwardEvent.ON_LDV_NO_ROWS);
			return retCode;
		}
		
		// Esecuzione logiche dopo esecuzione andata a buon fine 
		manageLogicDeclaredActivation(mcb, actionCoded.ldvClassName, EnumForwardEvent.ON_LDV_AFTER_EXECUTE);
		return retCode;
	}



	/* ---------------------------------------------------------------------------------------
     * Update controls GUI o variabili funzione con i valori correnti della logical data view.
     * ---------------------------------------------------------------------------------------
     * 
     * - Si recuperano i nomi di tutte le colonne esposte dalla logical data view
     * - Si ottiene dalla logical data view il valore di ogni variabile
     * - Si aggiornano i controlli swing o le variabili di fuinzione con lo stesso nome
     *    oppure
     * - Si aggiornano le variabili di funzione con lo stesso nome
     */
    private void ldvSetGuiControlsOrFunctionVars(InnerMonitorControBlock mcb, String ldvClassName, boolean updateControlGUI) {
		
    	ForwardLogicalDataView ldvObject = null;
		ArrayList<String> al_columnAsName = null;
 		Class<?> columnClass = null;
		Object varObject = null;
		InnerComponent innerComponent = null;
         
		ldvObject = mcb.hm_ldv.get(ldvClassName);
		if (ldvObject == null) {return;}
		
		// Colonne esposte dalla logical data view
		al_columnAsName = ldvObject.getColumnNamesExposed();
		
        // Scan singole colonne
		for (String columnAsName : al_columnAsName) {
			
			// Lettura componente swing con il nome della colonna della logical data view
			// Se nessun componente swing definito con lo stesso nome: nessuna operazione
			innerComponent = mcb.function.getComponentDeclared(columnAsName);
	        if (innerComponent == null) {continue;}
			        
			// Lettura variabile e update componente GUI swing
			varObject = ldvObject.getValue(columnAsName);												// Valore corrente variabile colonna
			columnClass = varObject.getClass();															// Update oggetti swing se tipo variabile compatibile
			
			// Update controllo GUI o variabile di funzione
			if (updateControlGUI) {
				updateGUIControlsOrVariables(mcb, varObject, columnClass, innerComponent, true); 				// Update GUI controls con lo stesso nome
			} else {
				setVarValue(mcb, columnAsName, varObject);														// Update variabile di funzione
			}
			
		}
	}


 
    /* ---------------------------------------------------------------------------------
     * Impostazione campi chiave prima FOR_ANY() con valori da GUI controls o variabile
     * ---------------------------------------------------------------------------------
     * 
     * - Si verifica se la view inizia con FOR_ANY()
     * - Si recuperano i PK della FOR_ANY()
     * - Si recuperano in nomi AS newName che coincidono con i nomi variabili/GUI controls
     * - Si recupera il valore di ogni variabile/GUI control e si aggiorna la view
     */
    private void ldvSetFieldsKeyRootFromGuiOrVar(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean getPK, boolean fromGui) {
    	
		ForwardLogicalDataView ldvObject = null;
		String[] al_entities =null;
		String[] ar_entityFieldAsName = null;
		String entityNameFirst = "";
		
		ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
		if (ldvObject == null) {return;}
		
		// Non è una ldv di query o non inizia con FOR_ANY(): no operations
		al_entities = ldvObject.getEntityNames();
		if (al_entities.length == 0) {return;}
		if (ldvObject.getEntryType(0) != EnumForwardLogicalDataViewControl.LDV_FOR_ANY) {return;}
		
		// Si tratta la prima entity in FOR_ANY()
		entityNameFirst = al_entities[0];
		if (getPK) {
			ar_entityFieldAsName = ldvObject.getEntityPKFieldNames(entityNameFirst);
		} else {
			ar_entityFieldAsName = ldvObject.getEntityNotPKFieldNames(entityNameFirst);
		}
		
		// Update colonne ldv da GUI o variabili applicative
		ldvSetFieldsFromGuiOrVarCommon(mcb, ldvObject, ar_entityFieldAsName, fromGui);
	}

    /* ---------------------------------------------------------------------------------
     * Impostazione campi entity con valori da GUI controls o variabile
     * ---------------------------------------------------------------------------------
     * 
     * - Si verifica se la view esista
     * - Si verifica se la view contenga l'entity 
     * - Per ogni entity dichiarata
     * -- Si recuperano i PK o NotPK  
     * -- Si recuperano in nomi AS newName che coincidono con i nomi variabili/GUI controls
     * -- Si recupera il valore di ogni variabile/GUI control e si aggiorna la view
     */
    private void ldvSetFieldsFromGuiOrVar(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean getPK, boolean fromGui) {
    	
		ForwardLogicalDataView ldvObject = null;
		ArrayList<String> al_entities = null;
		List<String> li_entities = null;
		String[] ar_entitiesRead =null;
		String[] ar_entitiesInsUpdDel =null;
		String[] ar_entityFieldAsName = null;
		
		ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
		if (ldvObject == null) {return;}
		
		// Ldv in for-any(), for-each(), insert, delete e update
		ar_entitiesRead = ldvObject.getEntityNamesAs();
		ar_entitiesInsUpdDel = ldvObject.getEntityNamesInsUpdDelAs();
		
		// Creazione ArrayList cumulativa
		al_entities = new ArrayList<String>();
		li_entities = Arrays.asList(ar_entitiesRead);
		al_entities.addAll(li_entities);
		li_entities = Arrays.asList(ar_entitiesInsUpdDel);
		al_entities.addAll(li_entities);
		
		if (al_entities.size() == 0) {return;}
		
		// Scan entities definite dalla logical data view
		for (String entityLoop : al_entities) {
			
			// Non è l'entity esplicitamente dichiarata: skip
			if (!actionCoded.ldvEntityNameAs.equals("") && !entityLoop.equals(actionCoded.ldvEntityNameAs)) {
				continue;
			}
			
			// Operazione da effettuarsi per entity dichiarata o per tutte
			
			// Recupero i nomi delle colonne esposte
			if (getPK) {
				ar_entityFieldAsName = ldvObject.getEntityPKFieldNames(entityLoop);
			} else {
				ar_entityFieldAsName = ldvObject.getEntityNotPKFieldNames(entityLoop);
			}
			
			// Update colonne ldv da GUI o variabili applicative
			ldvSetFieldsFromGuiOrVarCommon(mcb, ldvObject, ar_entityFieldAsName, fromGui);
		
		}
		
	}

    /*
     * Update colonne logical data view da GUI o variabili applicative
     */
    private void ldvSetFieldsFromGuiOrVarCommon(InnerMonitorControBlock mcb, ForwardLogicalDataView ldvObject, String[] ar_entityFieldAsName, boolean fromGui) {
 
		Object valueObject = null;
		InnerComponent innerComponent = null;

		// Scan nomi colonne esposte dalla logical data view (PK o non PK)
		for (String columnAsName : ar_entityFieldAsName) {
	
			// Lettura componente swing con il nome della colonna PK della logical data view
			// Se nessun componente swing definito con lo stesso nome: nessuna operazione
			innerComponent = mcb.function.getComponentDeclared(columnAsName);
	        if (innerComponent == null) {continue;}
			
	        // Lettura valore nell'applicazione. 
            // Da componente swing o da variabile dichiarata o creata automaticamente.
	        if (fromGui) {
		        valueObject = mcb.function.getValueControlGUI(columnAsName);
			} else {
				valueObject = mcb.function.getValueVar(columnAsName);
			}
	        
	        // Nessun controllo GUI o variabile con il nome fornito
	        if (valueObject == null) {continue;}
	        
	        // Update variabile corrente colonna in logical data view
	        ldvObject.setValue(columnAsName, valueObject);
		}

    }

    
	/* -------------------------------------------------------------
     * Impostazione campo, colonna della logical data view
     * -------------------------------------------------------------
     * 
     * - Impostazione da valore fornito nella action
     * - Impostazione da contenuto variabile della funzione
     */
	private void ldvSetField(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
		 
		ForwardLogicalDataView ldvObject = null;
		Object valueObject = null;
		
		ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
		if (ldvObject == null) {return;}
		
		// Value da valore codificato nella action o da variabile/controllo swing di funzione 
		valueObject = actionCoded.ldvColumnValue;
        if (valueObject == null) {
        	valueObject = mcb.function.getValue(actionCoded.ldvFunctionVarName);
        }
        
        // Variabile NON definita con VAR() dalla funzione.
        // Lascio il valore di default della ldv
        if (valueObject == null) {
			return;
		}
        
        // Update variabile corrente colonna in logical data view
        ldvObject.setValue(actionCoded.ldvColumnName, valueObject);
	}

	/* -------------------------------------------------------------
     * Impostazione variabile della logical data view
     * -------------------------------------------------------------
     * 
     * - Imnpostazione da valore fornito nella action
     * - Impostazione da contenuto variabile della funzione
     */
	private void ldvSetVar(InnerMonitorControBlock mcb, ForwardDoParms actionCoded) {
		ForwardLogicalDataView ldvObject = null;
		Object valueObject = null;
		
		ldvObject = mcb.hm_ldv.get(actionCoded.ldvClassName);
		if (ldvObject == null) {return;}
		
		// Value da valore codificato nella action o da variabile di funzione
		valueObject = actionCoded.ldvVarValue;
        if (valueObject == null) {
        	valueObject = mcb.function.getValueVar(actionCoded.ldvFunctionVarName);
        }
        
        // Update variabile corrente colonna in logical data view
        ldvObject.setValue(actionCoded.ldvVarName, valueObject);
	}


	/* -------------------------------------------------------------
     * Istanzia la logical data view ed esegue il metodo declare()
     * -------------------------------------------------------------
     */
	private int ldvCreate(InnerMonitorControBlock mcb, ForwardDoParms actionParmsExec) {
		ExceptionAmrita excp = null;									// Exception generata
		ForwardLogicalDataView ldvObject = null;
		String ldvClassNameComplete = "";								// Nome completo della classe applicativa di gestione della funzione
		String classModel = "" ;                               	 		// Modello completo classe incluso package
        int j = 0;

      	classModel = this.getClass().getName();
		j = classModel.indexOf(this.getClass().getSimpleName());
		ldvClassNameComplete = classModel.substring(0, j) + actionParmsExec.ldvClassName;
		
		// Generazione istanza classe di gestione funzione
		Object objLdvToStart = rm.newInstance(ldvClassNameComplete, new Class[0], new Object[0]);
		
		// Gestione errore di istanziazione
		if (objLdvToStart == null) {
	       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_NEW_INSTANCE, rm.getException());
		    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0103", new String[]{ldvClassNameComplete}, excp);
			mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_ERROR_NOT_DECLARED.ordinal());
			mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());
		    return EnumForwardLogicalDataViewControl.LDV_ERROR_NOT_DECLARED.ordinal();
		}

		// Esecuzione dichiarazione ldv e valorizzazione strutture interne
		ldvObject = (ForwardLogicalDataView) objLdvToStart;
		this.di.fwdLdvClass = ldvObject;
		ldvObject.setSd(this.sd);
		ldvObject.setDbm(this.dbm);
		ldvObject.setDbei(this.dbei);
		ldvObject.setLf(this.lf);
		ldvObject.setForwardSystem(mcb.system); 
		ldvObject.runDeclare(di);
		ldvObject.setRuleTableLanguage(mcb.system.getActiveLanguage());

		// Valori in area di system
		mcb.system.setLdvName(actionParmsExec.ldvClassName);
		mcb.system.setLdvObject(ldvObject);
		mcb.system.setLdvClass(ldvObject.getClass());
		mcb.system.setLdvReturnCode(ldvObject.getReturnCode());
		mcb.hm_ldv.put(actionParmsExec.ldvClassName, ldvObject);
		
		// Exception in dichiarazione logical data view, errore interno con exception
		if (di.excpOccurred != null) {
			lf.writeRow(EnumMessageType.ERROR_INPUT, "EI0102", new String[]{ldvObject.getClass().getSimpleName(), ldvClassNameComplete}, di.excpOccurred);
			mcb.system.setLdvReturnCode(EnumForwardLogicalDataViewControl.LDV_ERROR_INTERNAL.ordinal());
			mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());
			return EnumForwardReturnCode.RC_ERROR_LDV.ordinal();
		}

		// Exception in dichiarazione logical data view, errore interno
		if (ldvObject.getReturnCode() > 0) {
			lf.writeRow(EnumMessageType.ERROR_INPUT, "EI0103", new String[]{ldvObject.getClass().getSimpleName(), ldvClassNameComplete}, di.excpOccurred);
			mcb.system.setReturnCode(EnumForwardReturnCode.RC_ERROR_LDV.ordinal());
			return EnumForwardReturnCode.RC_ERROR_LDV.ordinal();
		}

	    mcb.system.setReturnCode(0);
		return 0;
	}

    /*
     * Esecuzione metodo applicativo, metodo shared
     */
	private void execReusableMethod(InnerMonitorControBlock mcb, ForwardDoParms actionParmsExec) {

    	@SuppressWarnings("rawtypes")
		Class[] ar_class = null;
      	Object[] ar_object = null;
      	Object[] ar_statusInvoke = null;
       	String methodName = "";
        	
		methodName = actionParmsExec.methodName;
		if (!this.rm.isMethodDefined(mcb.function.getClass(), methodName)) {return;}
		
	    // Valorizzazione campi in System
		mcb.system.setReusableMethodClassName(mcb.function.getClass().getSimpleName());
		mcb.system.setReusableMethodName(methodName);
		mcb.system.setReusableMethodRunning(true);
		
		// Valorizzazione per reflection
		ar_class = new Class[2];			 
		ar_class[0] = ForwardSystem.class;
		ar_class[1] = ForwardFunction.class;
		ar_object = new Object[2];	
		ar_object[0] = mcb.system;
		ar_object[1] = mcb.function;
		try {
			ar_statusInvoke = this.rm.invokeMethodWithStatus(mcb.function, methodName, ar_class, ar_object);
			mcb.system.setReusableMethodRunning(false);
		// Il metodo non è stato eseguito a causa di exception alla sua attivazione
		// Trace exception per successiva gestione da parte del monitor
		} catch (Exception e) {
			mcb.system.setReusableMethodRunning(false);
			mcb.system.setReusableMethodException(e);
			return;
		}
		
		// Il metodo è stato eseguito ed è andato in exception al suo interno
		// Trace exception per successiva gestione da parte del monitor
		if (ar_statusInvoke[0] instanceof Exception) {
			mcb.system.setReusableMethodRunning(false);
			mcb.system.setReusableMethodException((Exception) ar_statusInvoke[0]);
		}
		
		// metodo terminato correttamente
	}



	/* ------------------------------
     * Action per controllo funzione
     * ------------------------------
     * 
     * FUNCTION_LOAD	 
     * FUNCTION_QUIT	 
     * FUNCTION_START
     * FUNCTION_START_LOOKUP_TABLE	
     * FUNCTION_START_SHOW_LDV_ERROR
	 * FUNCTION_START_SHOW_MESSAGES
	 * FUNCTION_START_SHOW_EXCEPTION_ERROR
     * FUNCTION_XCTL	 
     * FUNCTION_RETURN	 
     * FUNCTION_SET_VAR	 
     * FUNCTION_SET_GUI_CONTROL	 
     * FUNCTION_SET_PARM_REQUIRED 
     * FUNCTION_SET_VAR_RETURNED 
     */
    private boolean execActionForFunction(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
		ExceptionAmrita excp = null;
		InnerMonitorControBlock mcbCalled = null;                       // Valido solo al ritorno di una funzione chiamante per action FUNCTION_SET_PARM_RETURNED
		ForwardFunction functionObject = null;							// Funzione applicativa da attivare
		ForwardDoParms doFunctionLoad = null;
		ForwardDoParms doFunctionStart = null;
		ForwardDoParms doFunctionSetParmRequired1 = null;
		ForwardDoParms doFunctionSetParmRequired2 = null;
		Object varValueReturned = null;
		Object varValueParm = null;
		InnerVar innerVar = null;
		String classModel = "";
		String functionClassNameComplete = "";
		@SuppressWarnings("unused")
		String functionNameCalled = "";
		String functionId = "";
      	boolean isDone = false;
		int j = 0;
		int returnCode = 0;
		
     	// Azione già individuata e eseguita o exception in corso e non si sta gestendo l'evento di exception
//     	if (mcb.system.getReusableMethodException() != null && actionCoded.action != EnumForwardAction.FUNCTION_START_SHOW_EXCEPTION_ERROR)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
       	
		switch (actionCoded.action) {
       	
			// Carica la funzione applicativa per successiva start della stessa
    	    // Il reference è reso disponibile in this.functionLoaded
			case FUNCTION_LOAD:        						
				isDone = true;
				this.functionLoaded = null;
				// Composizione nome classe funzione e generazione istanza  
		    	classModel = mcb.function.getClass().getName();
				j = classModel.indexOf(mcb.function.getClass().getSimpleName());
				functionClassNameComplete = classModel.substring(0, j) + actionCoded.functionNameToLoad;
				functionObject = this.hm_functionCalled.get(actionCoded.functionNameToLoad);
				this.functionLoaded = functionObject;
				mcb.system.setFunctionObject(functionObject);
				mcb.system.setFunctionName(actionCoded.functionNameToLoad);
				// Funzione già caricata e ancora disponibile
				if (functionObject != null) {break;}
				// Istanziazione
				lf.writeRow(EnumMessageType.INFORMATION, "MI0201", new String[]{functionClassNameComplete}, null); 		// Start ..
				Object objFunctionToStart = rm.newInstance(functionClassNameComplete, null, null);
				// Gestione errore di istanziazione
				if (objFunctionToStart == null) {
			       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_NEW_INSTANCE, rm.getException());
				    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0101", new String[]{functionClassNameComplete}, excp);
				    manageLogicDeclaredActivation(mcb, actionCoded.functionNameToLoad, EnumForwardEvent.ON_ERROR);
				    break;
				}
				// Esecuzione dichiarazione funzione e valorizzazione strutture interne
				functionObject = (ForwardFunction) objFunctionToStart;
				this.di.fwdFunctionClass = functionObject;
				functionObject.runDeclare(di);
				// Exception in dichiarazione funzione, errore interno
				if (di.excpOccurred != null) {
					lf.writeRow(EnumMessageType.ERROR_INPUT, "EI0101", new String[]{functionObject.getFunctionName(), functionClassNameComplete}, di.excpOccurred);
					mcb.system.setExcp(di.excpOccurred);
					manageLogicDeclaredActivation(mcb, actionCoded.functionNameToLoad, EnumForwardEvent.ON_SYSTEM_EXCEPTION_FUNCTION);
					break;
				}
				// Caricamento estremi nuova funzione
				this.functionLoaded = functionObject;
				this.functionLoaded.setMonitor(this);
				this.hm_functionCalled.put(actionCoded.functionNameToLoad, functionObject);
				mcb.system.setFunctionObject(functionObject);
				mcb.system.setFunctionName(actionCoded.functionNameToLoad);
				// Creazione nuovo blocco di controllo e inizializzazione funzione, per avere il contesto di esecuzione (variabili) completo PRIMA della START 
	    		this.mcbCalled = new InnerMonitorControBlock();
	    		this.mcbCalled.system = new ForwardSystem();
	    		this.mcbCalled.functionStart = this.functionLoaded;
	    		this.mcbCalled.function = this.functionLoaded;
	    		this.mcbCalled.frameFunction = mcb.frameFunction;				// Il frame della funzione chiamata è sempre quello iniziale principale
	    		this.functionLoaded.mcb = this.mcbCalled;
	    		this.mcb = this.mcbCalled;										// Il bolocco di controllo nel monitor deve essere quello nuovo, utilizzato anche da ForwardPanelMenu							
	    		// Inizializzazione generale funzione, creazione GUI
	    		functionInitial(this.mcbCalled);
	    		// Esecuzione logiche dopo load
	  	    	manageLogicDeclaredActivation(this.mcbCalled, actionCoded.functionNameToStart, EnumForwardEvent.ON_FUNCTION_AFTER_LOAD);
	    		// Ripristino indice corrente stack alterato da functionInitial ed elimino stack appena creato
	    		this.mcbIndex = al_monitorControlBlock.size() - 2;
	    		this.mcb = mcb;  												// Ripristino il blocco di controllo, la Load non deve alterare il contesto
 	    		this.al_monitorControlBlock.remove(this.mcbIndex + 1);
				break;
	
		    // Attiva una funzione applicativa che rimpiazza temporaneamente quella corrente
	    	case FUNCTION_START: 							
	    		isDone = true;
	    		// Funzione non caricata o con errori che ne hanno impedito il caricamento
	    		this.functionLoaded = this.hm_functionCalled.get(actionCoded.functionNameToStart);
				mcb.system.setFunctionObject(this.functionLoaded);
				mcb.system.setFunctionName(actionCoded.functionNameToStart);
	    		if (this.functionLoaded == null) {break;}
				this.functionLoaded.setCounterStarts(this.functionLoaded.getCounterStarts() + 1);   	// Counter starts
				this.functionLoaded.setFunctionId(actionCoded.functionId);								// Function identification
				this.functionLoaded.setLookupFunctionAutomaticParms(mcb.isLookupFunctionAutomaticParms);// True indica attivazione con gestione automatica valorizzazione parametri
	    		// Disabilitazione e/o oscuramento funzione corrente ancora attiva
	    		mcb.frameFunction.setEnabled(actionCoded.functionCurEnabled);
	    		mcb.frameFunction.setVisible(actionCoded.functionCurVisible);
	    		// Inizializzazione blocco di controllo e stack per la funzione chiamata 
	    		this.mcbCalled = this.functionLoaded.mcb;
	    		this.mcb = this.mcbCalled;
	    		this.al_monitorControlBlock.add(this.mcbCalled);
	    		this.mcbIndex = this.al_monitorControlBlock.size() - 1;
	    		this.mcbCalled.system.setDialogUserForm((JPanel) this.mcbCalled.panelFunctionForm);
	    		this.mcbCalled.system.setSystemCaller(mcb.system);
	    		this.mcbCalled.function = this.functionLoaded;
	    		this.mcbCalled.system.setActiveLanguage(mcb.system.getActiveLanguage());
	    		// Preparazione finale funzione per l'esecuzione e visualizzazione frame
	    		this.mcbCalled.frameFunction = new JFrame();	
	    		this.mcbCalled.functionCaller = mcb.function;
	    		this.mcbCalled.frameFunctionCaller = mcb.frameFunction;
	    		functionPreparingForExecutionOnJFrame(this.mcbCalled);				// Prepare & Show
//	    		functionPreparingForExecutionOnJDialog(this.mcbCalled);				// Prepare & Show
			    break;
	    		
    	    // Attiva una funzione modale di lookup per ricerca e restituzione valori da tabelle forward	
	    	case FUNCTION_START_LOOKUP_TABLE: 				
	    		isDone = true;
	    		// Creazione actions come se fossero dichiarate nella funzione
	    		doFunctionLoad = mcb.function.DO_FUNCTION_LOAD(FUNCTION_LOOKUP_RULE_TABLE);
	    		doFunctionSetParmRequired1 = this.mcb.function.DO_FUNCTION_SET_PARM_REQUIRED("FunctionLookUpRuleTable", actionCoded.functionLookupTableNum, "numTable", null);
	    		doFunctionSetParmRequired2 = this.mcb.function.DO_FUNCTION_SET_PARM_REQUIRED("FunctionLookUpRuleTable", mcb.function.getLanguage().ordinal(), "language", null);
	    		doFunctionStart = mcb.function.DO_FUNCTION_START(FUNCTION_LOOKUP_RULE_TABLE, actionCoded.functionId, true);
	    	    // Esecuzione azioni
	    		execActionForFunction(mcb, doFunctionLoad, false);
	    		this.functionLoaded.setFunctionLookup(true);
	    		execActionForFunction(mcb, doFunctionSetParmRequired1, false);
	    		execActionForFunction(mcb, doFunctionSetParmRequired2, false);
	            manageLogicDeclaredActivation(this.mcb, actionCoded.functionId, EnumForwardEvent.ON_FUNCTION_BEFORE_START);	// Eventuali logiche utente 
	            mcb.isLookupFunctionAutomaticParms = true;						// Alla return valorizza automaticamente le variabili nella funzione chiamante
	    		execActionForFunction(mcb, doFunctionStart, false);
	    		break;
    	         
	    	// Attiva un dialogo modale di visualizzazione informazioni di errore 
	    	case FUNCTION_START_SHOW_LDV_ERROR:
	    		isDone = true;
	    		// Creazione actions come se fossero dichiarate nella funzione
	    		doFunctionLoad = mcb.function.DO_FUNCTION_LOAD(FUNCTION_SHOW_LDV_ACCESS);
	    		doFunctionStart = mcb.function.DO_FUNCTION_START(FUNCTION_SHOW_LDV_ACCESS, "");
	    	    // Esecuzione azioni X attivazione funzione
	    		execActionForFunction(mcb, doFunctionLoad, false);
	    		this.functionLoaded.setParmValueRequired("systemObjectCaller", mcb.system);			// Oggetto system funzione chiamante
	    		this.functionLoaded.setParmValueRequired("ldvObject", mcb.system.getLdvObject());	// Oggetto LogicalDataView da esporre
	    		execActionForFunction(mcb, doFunctionStart, false);
	    		break;
		    	 
	    	// Attiva un dialogo modale di visualizzazione messaggi informativi e/o di errore 
	    	case FUNCTION_START_SHOW_MESSAGES:
	    		isDone = true;
	    		// Creazione actions come se fossero dichiarate nella funzione
	    		doFunctionLoad = mcb.function.DO_FUNCTION_LOAD(FUNCTION_SHOW_MESSAGES);
	    		doFunctionStart = mcb.function.DO_FUNCTION_START(FUNCTION_SHOW_MESSAGES, "");
	    	    // Esecuzione azioni X attivazione funzione
	    		execActionForFunction(mcb, doFunctionLoad, false);
	    		this.functionLoaded.setParmValueRequired("systemObjectCaller", mcb.system);			// Unico parametro l'oggetto system 
	    		execActionForFunction(mcb, doFunctionStart, false);
	    		break;
	    				
	    	// Attiva un dialogo modale di visualizzazione informazioni sull'exception avvenuto
	    	case FUNCTION_START_SHOW_EXCEPTION_ERROR:
	    		isDone = true;
	    		// Creazione actions come se fossero dichiarate nella funzione
	    		doFunctionLoad = mcb.function.DO_FUNCTION_LOAD(FUNCTION_SHOW_EXCEPTION_FAILURE);
	    		doFunctionStart = mcb.function.DO_FUNCTION_START(FUNCTION_SHOW_EXCEPTION_FAILURE, "");
	    	    // Esecuzione azioni X attivazione funzione
	    		execActionForFunction(mcb, doFunctionLoad, false);
	    		this.functionLoaded.setParmValueRequired("systemObjectCaller", mcb.system);			// Unico parametro l'oggetto system 
	    		execActionForFunction(mcb, doFunctionStart, false);
	    		break;
    	        
			// Esce dalla funzione corrente e restituisce il controllo al chiamante, restituendo eventuali valori	
			case FUNCTION_RETURN:        			
				isDone = true;
				functionNameCalled = mcb.function.getFunctionName();
				functionId = mcb.function.getFunctionId();
				// Return dal primo livello: exit completo, coincide con quit
				if (this.al_monitorControlBlock.size() == 1) {
					System.exit(0);
				}
				// Ripristino contesto funzione chiamante che diventa corrente
				this.mcbIndex = this.al_monitorControlBlock.size() - 2;
				this.mcb = this.al_monitorControlBlock.get(this.mcbIndex);
				// Potrebbe essere una funzione gà attivata come dialogo
				if (this.mcb.isStartedOnJFrame) {
					this.mcb.frameFunction.setEnabled(true);
					this.mcb.frameFunction.setVisible(true);
				} else {
					this.mcb.dialogFunction.setEnabled(true);
					this.mcb.dialogFunction.setVisible(true);
				}
				// Gestione automatica valorizzazione variabili returned per funzioni automatiche di lookup
				if (mcb.function.isLookupFunctionAutomaticParms() && mcb.function.isFunctionLookup()) {
					manageFunctionReturnLookup(this.mcb, actionCoded, mcb.function.getFunctionId(),  mcb.function.getClass().getSimpleName());
				}
				this.mcb.isLookupFunctionAutomaticParms = false;
				// Logiche su return da funzione chiamata, per esempio per impostare le variabili returned
				// la funzione chiamata, con tutte le sue variabili, è ancora diponibile
				manageLogicDeclaredActivation(this.mcb, functionId, EnumForwardEvent.ON_FUNCTION_RETURN);		// Esecuzione actions dichiarate per la funzione
				// Rilascio risorse funzione chiamata (lo fa il garbage)
				if (mcb.isStartedOnJFrame) {
					mcb.frameFunction.setVisible(false);
					mcb.frameFunction.setEnabled(false);
					mcb.frameFunction.dispose();
					mcb.frameFunction = null;
				} else {
					mcb.dialogFunction.setVisible(false);
					mcb.dialogFunction.setEnabled(false);
					mcb.dialogFunction.dispose();
					mcb.dialogFunction = null;
				}
                // Rimozione oggetto funzione chiamata se previsto	e rilascio blocco controllo funzione chiamata			
				if (actionCoded.functionReleaseOnReturn) {this.hm_functionCalled.remove(actionCoded.functionName);}
				this.al_monitorControlBlock.remove(this.al_monitorControlBlock.size() - 1);
				// Eventuali logiche al ritorno alla funzione chiamante
				manageLogicDeclaredActivation(this.mcb, functionId, EnumForwardEvent.ON_FUNCTION_RETURNED);		// Esecuzione actions dichiarate per la funzione
				break;
    
	    	// Esce dalla funzione corrente e restituisce il controllo al sistema	
			case FUNCTION_QUIT: 					 
				isDone = true;
				returnCode = actionCoded.functionReturnCode;
				System.exit(returnCode);
				break;
				
	    	// Attiva una nuova funzione e gli trasferisce il controllo senza incrementare lo stack   		
	    	case FUNCTION_XCTL: 						
	    		isDone = true;
	    		// Funzione non caricata o con errori che ne hanno impedito il caricamento
	    		this.functionLoaded = this.hm_functionCalled.get(actionCoded.functionNameToStart);
				mcb.system.setFunctionObject(this.functionLoaded);
				mcb.system.setFunctionName(actionCoded.functionNameToStart);
	    		if (this.functionLoaded == null) {break;}
				this.functionLoaded.setCounterStarts(this.functionLoaded.getCounterStarts() + 1);   	// Counter starts/xctl
				this.functionLoaded.setFunctionId(actionCoded.functionId);								// Function identification
				this.functionLoaded.setLookupFunctionAutomaticParms(mcb.isLookupFunctionAutomaticParms);// True indica attivazione con gestione automatica valorizzazione parametri
	    		// Disabilitazione, oscuramento e rilascio risorse funzione corrente ancora attiva
	    		mcb.frameFunction.setEnabled(false);
	    		mcb.frameFunction.setVisible(false);
	    		mcb.frameFunction.dispose();						// Rilascio fisico delle risorse
	    		// Inizializzazione blocco di controllo e stack per la funzione chiamata che rimane quello corrente aggiornato
	    		this.mcbCalled = this.functionLoaded.mcb;
	    		this.mcb = this.mcbCalled;						
	    		this.mcbIndex = this.al_monitorControlBlock.size() - 1;
	    		this.al_monitorControlBlock.remove(this.mcbIndex);
	    		this.al_monitorControlBlock.add(this.mcbCalled);
	    		this.mcbIndex = this.al_monitorControlBlock.size() - 1;
	    		this.mcbCalled.system.setDialogUserForm((JPanel) this.mcbCalled.panelFunctionForm);
	    		this.mcbCalled.system.setSystemCaller(mcb.system);
	    		this.mcbCalled.function = this.functionLoaded;
	    		this.mcbCalled.system.setActiveLanguage(mcb.system.getActiveLanguage());
	    		// Preparazione finale funzione per l'esecuzione e visualizzazione frame
	    		functionPreparingForExecutionOnJFrame(this.mcbCalled);				// Prepare & Show
	    		break;

			// Imposta la variabile della funzione con il valore corrente del linguaggio, una enumerazione EmumLanguage 		
			case FUNCTION_GET_LANGUAGE:                	
				isDone = true;
				if (!mcb.function.isVarDeclared(actionCoded.functionVarName)) {break;}
				mcb.function.setVarValue(actionCoded.functionVarName, mcb.system.getActiveLanguage());
				break;

	    		
			// Imposta la variabile della funzione dichiarata con VAR() con un valore costante o il contenuto di un control GUI				
			case FUNCTION_SET_VAR:                	
				isDone = true;
				// Impostazione valore variabile da oggetto embedded in action
				if (actionCoded.functionVarValue != null) {
					this.setVarValue(mcb, actionCoded.functionVarName, actionCoded.functionVarValue);
					break;
				}
				// Impostazione valore variabile da controllo GUI
				Object controlGuiValue = mcb.function.getValueControlGUI(actionCoded.functionGuiControlName);
				mcb.function.setVarValue(actionCoded.functionVarName, controlGuiValue);
				break;

			// Imposta il controllo GUI con un valore costante o con il contenuto di una variabile di funzione				
			case FUNCTION_SET_GUI_CONTROL:          
				isDone = true;
				// Impostazione GUI control da oggetto embedded in action
				if (actionCoded.functionVarValue != null) {
					mcb.function.setValueControlGui(actionCoded.functionGuiControlName, actionCoded.functionVarValue);
					break;
				}
				// Impostazione GUI control da valore variabile
				mcb.function.setValueControlGuiFromVar(actionCoded.functionGuiControlName, actionCoded.functionVarName);
				break;
				
			// Imposta la variabile della funzione chiamata dichiarata con PARM_REQUIRED() con un valore costante o il contenuto di una variabile del chiamante 
			// La funzione chiamata deve essere già stata caricata da FUNCTION_LOAD()
			// La funzione corrente è ancora quella chiamante.
			// Le variabili del chiamante/chiamato hanno entrambe scope di function
			case FUNCTION_SET_PARM_REQUIRED:                	
				isDone = true;
				// Funzione mai caricata con FUNCTION_LOAD()
				this.functionLoaded = this.hm_functionCalled.get(actionCoded.functionName);
				if (this.functionLoaded == null) {break;}
				// Impostazione valore variabile da oggetto embedded in action
				if (actionCoded.functionParmValue != null) {
					this.functionLoaded.setVarValue(actionCoded.functionParmNameInCalled, actionCoded.functionParmValue);
					break;
				}
				// Impostazione valore nome parametro, che rappresenta una variabile o un controllo GUI 
				varValueParm = mcb.function.getValueControlGUI(actionCoded.functionParmNameInCaller);
				this.setVarValueMcb(actionCoded.functionParmNameInCalled, varValueParm, this.functionLoaded.mcb);
				break;
 
			// Imposta la variabile della funzione chiamante, con il parametro della funzione chiamata dichiarato con PARM_RETURNED()
			// La funzione corrente è già quella chiamante, quella chiamante è in stack, immediatamente dopo quella corrente.
		    // Le variabili del chiamato hanno scope di function e valgono solo nella funzione chiamata
		    // Le variabili del chiamante (funzione corrente) possono avere qualsiasi scope
			case FUNCTION_SET_PARM_RETURNED:                	
				isDone = true;
				// Funzione mai caricata con FUNCTION_LOAD() o esecuzione NON a fronte di return da funzione
				this.functionLoaded = this.hm_functionCalled.get(actionCoded.functionName);
				if (this.functionLoaded == null) {break;}
				if (this.mcbIndex != this.al_monitorControlBlock.size() - 2) {break;}
				mcbCalled = this.al_monitorControlBlock.get(this.mcbIndex + 1);
				// Funzione chiamata in stack diversa da quella indicata nell'action
	            if (!mcbCalled.function.getClass().getSimpleName().equals(actionCoded.functionName)) {break;}
				// Recupero valore parametro in funzione chiamata (non + attiva)
                innerVar = mcbCalled.hm_varScopeFunction.get(actionCoded.functionParmNameInCalled);
				if (innerVar == null) {break;}						// Variabile da restituire al chiamante NON definita nella funzione chiamata
				varValueReturned = innerVar.varObject;
				// Imposto valore parametro in funzione corrente, che è quella chiamante, con il valore restituito dalla funzione chiamata
				mcb.function.setValue(actionCoded.functionParmNameInCaller, varValueReturned);
				break;
				 
			// Imposta tutte le variabile definite con PARM_RETURNED()) al loro valore di default
			case FUNCTION_SET_PARMS_RETURNED_INITIAL:                	
				isDone = true;
				for (String parmNameReturned : mcb.function.getParmNamesReturned()) {
					innerVar = mcb.hm_varScopeFunction.get(parmNameReturned);
										 
				}
				break;
       	}
		return isDone;
	}

    /*
     * Gestione automatica valorizzazione parametri restituiti dalla funzione chiamata
     * Il blocco di controllo in input è quello della funzione chiamante
     */
	private void manageFunctionReturnLookup(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, String functionId, String functionLookup) {
		Scanner scn = null;
		ForwardPanelComponent panelComponent = null;
		ArrayList<String> al_parmReturned = null;
		ForwardDoParms doFunctionSetParmReturned = null;
		String parmReturnedCaller = "";					// Nome componente in funzione chiamante (corrente)
		String parmReturnedCalled = "";					// Nome parametro restituito in funzione chiamata
		
        // La funzione  di lookup è stata attivata a fronte di DblClick su un componente GUI
		// Il nome del componente è diventato il function identifier memorizzato nella funzione chiamata
		panelComponent = mcb.function.getPanelComponent(functionId);
		
		// Esecuzione caricamento parametri restituiti dalla funzione di lookup chiamata
		al_parmReturned = panelComponent.getLookupFunctionParmsReturned();
		for (String parmReturned : al_parmReturned) {
			scn = new Scanner(parmReturned);
			parmReturnedCaller = scn.next();
			if (scn.hasNext()) {
				parmReturnedCalled = scn.next();
				doFunctionSetParmReturned = mcb.function.DO_FUNCTION_SET_PARM_RETURNED(functionLookup, parmReturnedCaller, parmReturnedCalled);
				execActionForFunction(mcb, doFunctionSetParmReturned, false);
			}
		}
	}


	/*
	 * Set a variable value, in the scope at function level, in the runtime forward monitor.
	 * <br>
	 * If the variable doesn't exist or is not declared at function levele scope, no action will be taken.<br>
	 * If the varValue object type is not of the same type of the variable, no action will be taken.<br>
	 * <p>
	 * It's a way to set complex and structured parameters, organized by means of an application object, <br>
	 * with the visibility wished.<br>
	 */
	private void  setVarValueMcb(String varName, Object varValue, InnerMonitorControBlock mcb) {
		InnerVar innerVar = null;
		
		innerVar = getVarInnerVar(varName, EnumForwardScope.SCOPE_FUNCTION, mcb);
		if (innerVar == null) {return;}
		if (varValue != null && innerVar.varType != varValue.getClass()) {return;}
		innerVar.varObject = varValue;
		return;
	}

	/* ------------------------------
     * Action per controllo funzione
     * ------------------------------
     * 
     * SYSTEM_BEEP
     * SYSTEM_TIMER_START 	 
     * SYSTEM_TIMER_STOP 	 
     * SYSTEM_TIMER_DELAY 	 
     * SYSTEM_LOAD_FUNCTION	 
     */
    private boolean execActionForSystem(InnerMonitorControBlock mcb, ForwardDoParms actionCoded, boolean isActionExecuted) {
    	
     	Timer timer = null;
    	String timerName = "";
    	boolean isDone = false;
		
     	// Azione già individuata e eseguita o exception in corso
     	if (mcb.system.getReusableMethodException() != null)  {return isActionExecuted;}
		if (isActionExecuted) {return true;}
       	
    	switch (actionCoded.action) {
       	
    		// Emette un beep di sistema		
    		case SYSTEM_BEEP:        								
				isDone = true;
				Toolkit.getDefaultToolkit().beep();
				break;
			
			// Starta il timer
			case SYSTEM_TIMER_START:        						
				isDone = true;
				timerName = actionCoded.systemTimerName;
				timer = mcb.function.getTimer(timerName);
				timer.start();
				break;
			
			// Stoppa il timer
			case SYSTEM_TIMER_STOP:        							
				isDone = true;
				timerName = actionCoded.systemTimerName;
				timer = mcb.function.getTimer(timerName);
				timer.stop();
				break;
			
			// Immposta il delay iniziale e il delay di intervallo in millisecondi
			case SYSTEM_TIMER_DELAY:        						
				isDone = true;
				timerName = actionCoded.systemTimerName;
				timer = mcb.function.getTimer(timerName);
				timer.setDelay(actionCoded.systemTimerDelay);
				timer.setInitialDelay(actionCoded.systemTimerDelayInitial);
				break;

       	}
		return isDone;
	}

	/* --------------------------------------------------------------
     * Restituisce true se l'evento per il componente è mascherato
     * --------------------------------------------------------------
     */
    private boolean isEventMasked(InnerMonitorControBlock mcb, String componentName, EnumForwardEvent event) {
    	Set<EnumForwardEvent> hs_eventsMasked = null;
    	
        hs_eventsMasked = mcb.hm_maskedEvents.get(componentName);
        if (hs_eventsMasked == null) {return false;}
        if (!hs_eventsMasked.contains(event)) {return false;}
    	return true;
    }
    	
    
}



/* ---------------------------------------
 * Blocco di controllo esecuzione monitor
 * ---------------------------------------
 * 
 * Contiene le strutture di controllo della funzione e dellop stato di esecuzione.
 * Una funzione può attivare altre funzioni in modo modale e indipendente e ogni
 * funzione richiamata, a qualsiasi livello, ha il suo blocco di controllo.
 * Forward monitor mantiene lo stack dei blocchi di controllo attivi.
 *  
*/
 class InnerMonitorControBlock implements Cloneable {
	 
	ForwardSystem system = null;									// Active system informations
	ForwardFunction function = null;								// Active function
	ForwardFunction functionStart = null;							// Start function
	ForwardFunction functionCaller = null;							// Funzione applicativa chiamante
	ForwardForm formStart = null;									// Start form della funzione
	JFrame frameFunction = null;									// JFrame in cui è stato disposto il pannello principale della funzione iniziale di primo livello
	JFrame frameFunctionCaller = null;								// JFrame in cui è stato disposto il pannello principale della funzione chiamante
	JDialog dialogFunction = null;									// JDialog in cui è stato disposto il pannello principale della funzione attivata con START in stack
	JApplet appletFunction = null;									// JApplet in cui è stato disposto il pannello principale della funzione attivata da web
 	Map<String, InnerVar> hm_varScopeFunction = null;				// Variabili applicative con scope a livello funzione per la funzione corrente
  	Map<String, InnerVar> hm_varScopeSession = null;				// Variabili applicative con scope a livello session
  	Map<String, InnerVar> hm_varScopeServer = null;					// Variabili applicative con scope a livello server per tutte le funzioni in tutte le sessioni
    Map<String, ForwardLogicalDataView> hm_ldv = null;				// Logical data view objects della funzione
	Map<String, Set<EnumForwardEvent>> hm_maskedEvents = null;		// Eventi correntemente mascherati per la funzione corrente
 
	// Work & service
	ForwardMenu forwardMenuBar = null;
	ForwardPanelMenu forwardPanelMenu = null;
	JMenuBar jmenuBar = null;
	JComponent panelFunctionForm = null;
	ForwardForm formFunctionStart = null;
	ForwardMonitorDesktop fmd = null;
	String formFunctionStartName = "";
	boolean isLookupFunctionAutomaticParms = false;
	boolean isStartedOnJFrame = false;
	
	/* Costruttore */
	InnerMonitorControBlock() {
	 	hm_varScopeFunction = new HashMap<String, InnerVar> (); 
	 	hm_varScopeSession = new HashMap<String, InnerVar> (); 
	 	hm_varScopeServer = new HashMap<String, InnerVar> (); 
	 	hm_ldv = new HashMap<String, ForwardLogicalDataView> ();
		hm_maskedEvents = new HashMap<String, Set<EnumForwardEvent>> ();
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected Object clone()  {
		InnerMonitorControBlock thisCloned = null;
		try {
			thisCloned = (InnerMonitorControBlock) super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
		return thisCloned;
	}

}


 /* -------------------------------------------
  * Descrittore runtime variabili applicative
  * -------------------------------------------
  * 
  * Contiene il valore corrente in esecuzione delle variabili definite dalla funzione
  * o dichiarate implicitamente dalle colonne di una jtable.
  * Le informazionioni vengono copiate dal descrittore della funzione, per una gestione runtime + efficiente
  * Qui viene aggiunto il valore corrente della variabile come oggetto specifico.
  * Si può trattare di un oggetto String, Integer, Boolean, Long, Float, Double, Timer o di un qualsiasi
  * oggetto applicativo dichiarato come una variabile.
  *  
 */
  class InnerVar{
		String varName = "";									// Nome variabile
		EnumForwardScope varScope = null;						// Scope variabile
		Class<?> varType = null;								// Tipo variabile (Integer, String, Boolean, Double, Float, Date, Color, ..)
		Object varInitial = null;     							// Valore iniziale variabile, contiene un oggetto di tipo varType
		Object varObject = null;     							// Valore corrente variabile, 	contiene un oggetto di tipo varType
		Object varObjectPrev = null;     						// Valore precedente variabile, contiene un oggetto di tipo varType
		boolean isAnySetDone = false;                        	// True indica almeno una SET_VAR() eseguita

  }

  /* -------------------------------------------
   * Descrittore messaggio
   * -------------------------------------------
   * 
   * Descrive un singolo messaggio associato al componente e valorizzato in fase controlli
   *  
  */
   class InnerComponentMessage{
	    EnumForwardAction msgOrigin = null;						// Action origine del messaggio
		String panelName = "";									// Nome pannello in cui il componente è collocato
		int numRuleTable = 0;									// Numero tabella di controllo del campo
		String componentName = "";								// Nome componente GUI in errore
 		EnumMessageType msgType = null;                         // Tipo messaggio (Information, Wawning, Error, Fatal, ...)
 		String msgCode = "";									// Codice messaggio codificato in tabella
 		String msgShortText = "";                               // Testo breve messaggio su una riga
 		String msgLongText = "";                                // Testo lungo messaggio in HTML
		public InnerComponentMessage() {
			msgOrigin = EnumForwardAction.NOT_ASSIGNED;
		}
   }




//Return the focus to the typing area.
//typingArea.requestFocusInWindow();

/*
 *               "dd MMMMM yyyy",
                 "dd.MM.yy",
                 "MM/dd/yy",
                 "yyyy.MM.dd G 'at' hh:mm:ss z",
                 "EEE, MMM d, ''yy",
                 "h:mm a",
                 "H:mm:ss:SSS",
                 "K:mm a,z",
                 "yyyy.MMMMM.dd GGG hh:mm aaa"

// static final String newline = System.getProperty("line.separator");

 
 	**
	 * The GUI will be created and the application function will be started.<br>
	 * <p>
	 * This constructor is intended for use as applet.<br>
	 * <p>
	 * @param frame as a {@link JApplet} object
	 *
	public  void createAndShowGUI(JApplet frame) {
		
		
		// Creazione GUI.
		// Verranno creati i pannelli e i controlli per tutti i form definiti.
		// I pannelli predefiniti come i menu sono creati istanziandoli.
		// I pannelli di dettaglio sono creati disponendo i controlli su essi.
		// Il listener per i controlli applicativi è questa classe.
		
		monitorInitialize();					// Monitor structure allocation and initialization, symbol table. ...
		completeComponentsDeclared();			// For each component declared set default properties and listeners
		disposeControlsInDetailPanels();		// Lay out controls in detail panels according to layout managers
		createMainFormPanel(this.formStart);	// lay out panels structure recursively according to layout managers
		setPanelsDefaultValues();				// For each panel set default by declaring or by database
		
		// Creazione Frame per la funzione
		appletFunction = frame;

		// Impostazione eventuale menuBar direttamente sul frame dell'applet
		if (this.functionStart.isMenubar()) {
			forwardMenuBar = this.functionStart.getMenu(this.functionStart.getFunctionMenuBarName());
			if (forwardMenuBar != null) {
				forwardPanelMenu = new ForwardPanelMenu(this, this.functionStart.getFunctionMenuBarName(), forwardMenuBar);
				jmenuBar = forwardPanelMenu.getJMenuBar();
				appletFunction.setJMenuBar(jmenuBar);
			}
		}
				
        // Il content pane del frame è il pannello radice del form di partenza
		formFunctionStartName = this.mcb.function.getFormStartName();
		formFunctionStart = this.mcb.function.getForm(formFunctionStartName);
		panelFunctionForm = formFunctionStart.getJrootPanel();
		panelFunctionForm.setOpaque(true); 									// content panes must be opaque
 
        // Dimensioni in base alle dimensioni preferred o minime dei pannelli
		appletFunction.setContentPane(panelFunctionForm);
        appletFunction.setVisible(true);
	}
   
// Calcolo dimensioni in pixel di una stringa per un dato font
FontMetrics metrics = table.getFontMetrics(table.getFont());
int width = metrics.stringWidth(strCellContent);


 * JFormattedText Field per NON uscire se ci sono errori nella digitazione
 * JFormattedTextField extends JTextField adding support for formatting arbitrary values, as well as retrieving a particular 
 * object once the user has edited the text. The following illustrates configuring a JFormattedTextField to edit dates: 

   JFormattedTextField ftf = new JFormattedTextField();
   ftf.setValue(new Date());
 
Once a JFormattedTextField has been created, you can listen for editing changes by way of adding a PropertyChangeListener
and listening for PropertyChangeEvents with the property name value. 

JFormattedTextField allows configuring what action should be taken when focus is lost. The possible configurations are: Value
 Description
 
JFormattedTextField.REVERT  Revert the display to match that of getValue, possibly losing the current edit.  
JFormattedTextField.COMMIT  Commits the current value. If the value being edited isn't considered a legal value by the AbstractFormatter that is, a ParseException is thrown, then the value will not change, and then edited value will persist.  
JFormattedTextField.COMMIT_OR_REVERT  Similar to COMMIT, but if the value isn't legal, behave like REVERT.  
JFormattedTextField.PERSIST  Do nothing, don't obtain a new AbstractFormatter, and don't update the value.  
The default is JFormattedTextField.COMMIT_OR_REVERT, refer to setFocusLostBehavior for more information on this. 

JFormattedTextField allows the focus to leave, even if the currently edited value is invalid. 
To lock the focus down while the JFormattedTextField is an invalid edit state you can attach an InputVerifier. 
The following code snippet shows a potential implementation of such an InputVerifier: 

 public class FormattedTextFieldVerifier extends InputVerifier {
     public boolean verify(JComponent input) {
         if (input instanceof JFormattedTextField) {
             JFormattedTextField ftf = (JFormattedTextField)input;
             AbstractFormatter formatter = ftf.getFormatter();
             if (formatter != null) {
                 String text = ftf.getText();
                 try {
                      formatter.stringToValue(text);
                      return true;
                  } catch (ParseException pe) {
                      return false;
                  }
              }
          }
          return true;
      }
      public boolean shouldYieldFocus(JComponent input) {
          return verify(input);
      }
  }
 
Alternatively, you could invoke commitEdit, which would also commit the value. 
JFormattedTextField does not do the formatting it self, rather formatting is done through an instance of JFormattedTextField.AbstractFormatter which is obtained from an instance of JFormattedTextField.AbstractFormatterFactory. Instances of JFormattedTextField.AbstractFormatter are notified when they become active by way of the install method, at which point the JFormattedTextField.AbstractFormatter can install whatever it needs to, typically a DocumentFilter. Similarly when JFormattedTextField no longer needs the AbstractFormatter, it will invoke uninstall. 
JFormattedTextField typically queries the AbstractFormatterFactory for an AbstractFormat when it gains or loses focus. Although this can change based on the focus lost policy. If the focus lost policy is JFormattedTextField.PERSIST and the JFormattedTextField has been edited, the AbstractFormatterFactory will not be queried until the value has been commited. Similarly if the focus lost policy is JFormattedTextField.COMMIT and an exception is thrown from stringToValue, the AbstractFormatterFactory will not be querired when focus is lost or gained. 
JFormattedTextField.AbstractFormatter is also responsible for determining when values are commited to the JFormattedTextField. Some JFormattedTextField.AbstractFormatters will make new values available on every edit, and others will never commit the value. You can force the current value to be obtained from the current JFormattedTextField.AbstractFormatter by way of invoking commitEdit. commitEdit will be invoked whenever return is pressed in the JFormattedTextField. 

If an AbstractFormatterFactory has not been explicitly set, one will be set based on the Class of the value type after setValue has been invoked (assuming value is non-null). 
For example, in the following code an appropriate AbstractFormatterFactory and AbstractFormatter will be created to handle formatting of numbers: 

   JFormattedTextField tf = new JFormattedTextField();
   tf.setValue(new Number(100));
 

 * 
 * 
 */

  /*
   * Renderer speciale per date in una tabella
   * 
   * public class DateCellRenderer extends DefaultTableCellRenderer{
														public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column){
																super.getTableCellRendererComponent( table, value, isSelected, hasFocus, row, column );
																if ( value instanceof Date ){
																	// Use SimpleDateFormat class to get a formatted String from Date object.
																	String strDate = new SimpleDateFormat("MM/dd/yyyy").format((Date)value);
																	// Sorting algorithm will work with model value. So you dont need to worry
																	// about the renderer's display value.
																	this.setText( strDate );
																}
																return this;
														}
	}


   */
  
         
        /* Provvisorio per sanare situazione */
//   	    Object ar_objMetric[] = null;
//     	EntityMetric entityMetric = null;
//     	String whereCondition = "";
//     	String section = "";
//     	String sectionThru = "";
//    	boolean readOk = false;
//       	boolean deleteOk = false;
//       	boolean createOk = false;
//     	int i = 0;
//     	
//       // Operazioni per accesso al databsae
//     	try {
//			dbs = new DataBaseStatusDetailed();
//			dbConn = dbm.getConnection(dbs);
//			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
//			
//			entityMetric = new EntityMetric();
//			
//			    
//     
//			// Composizione Where di lettura EntityRelation
//			whereCondition = "METRIDOB = 'ARUKA300'  "; 
//			
//			ar_objMetric = dbei.readSetEntity(entityMetric, "", "");
//			
//			for (Object objMetric : ar_objMetric) {
//				entityMetric = (EntityMetric) objMetric;
//				section = entityMetric.getSection();
//				i = section.indexOf(":");
//				if (i > 0) {
//					sectionThru = section.substring(i+1);
//					section = section.substring(0, i);
//					deleteOk = dbei.delete(entityMetric);
//					entityMetric.setSection(section);
//					entityMetric.setSectionThru(sectionThru);
//					createOk = dbei.create(entityMetric);
//				}
//				
//			}
//    
//			dbei.commit();
//			dbm.releaseConnection(dbConn, dbs);
//		} catch (ExceptionAmritaSqlError e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (SQLException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (ExceptionAmrita e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} 
        
        
   
  
  
