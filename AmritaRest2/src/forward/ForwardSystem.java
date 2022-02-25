package forward;

import java.awt.Color;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.AdjustmentEvent;
import java.awt.event.FocusEvent;
import java.awt.event.ItemEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;

import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.event.ChangeEvent;
import javax.swing.event.DocumentEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;

import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;

import enums.EnumForwardAction;
import enums.EnumForwardComponent;
import enums.EnumForwardEvent;
import enums.EnumForwardLogicalDataViewControl;
import enums.EnumForwardReturnCode;
import enums.EnumLanguage;
import enums.EnumMessageType;

/**
 * copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardSystem
 * </h1>
 * Questa classe descrive tutte le informazioni di sistema, di ambiente, di sessione,<br>
 * i defaulte di esecuzione, i parametri di installazione e di sessione e, in generale,<br>
 * tutte le informazioni utili runtime all'applicazione.<br>
 * <p>
 * Sono inoltre presenti i metodi e i servizi di sistema non direttamente relativi alla funzione<br>
 * come il reperimento dei messaggi attraverso il codice.<br>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardFunction
 *
*/ 

public class ForwardSystem implements Cloneable {
	
	////////////////////////////////////////////////////
	// Info tecniche sul desktop monitor e di ambiente
	////////////////////////////////////////////////////
	
	private ForwardMonitorDesktop monitorDesktop = null;			// E' un reference al monitor in execuzione
	private ForwardSystem systemCaller = null;                      // E' un reference all'oggetto ForwardSystem della funzione chiamante
	private int release = 0;										// Release forward monitor
	private int version = 0;										// Versione	forward monitor
	private int modification = 0;									// Modification forward monitor
	private UserConfiguration sd = null; 								// Defaults generali di sistema

	
	//////////////////////////////////////////////////////////////////////////////
	// Info per accesso diretto al database dal codice applicativo della funzione
	//////////////////////////////////////////////////////////////////////////////
	
    private DataBaseManager dbm = null;									// Gestore database
	private Connection dbConn = null;									// Connessione con il database	
    private DataBaseEntityInterface dbei = null;						// Interfaccia per operazioni CRUD su entities			     

	
	////////////////////////////////////////////////////////
	// Info user cliente e di login
	////////////////////////////////////////////////////////
	
	private String userCustomerCode = "";							// Codice cliente
	private String userLogin = "";									// User login di autenticazione
	
	
	////////////////////////////////////////////////////////
	// Info funzione caricata, chiamata, attivata
	////////////////////////////////////////////////////////
	
	private String functionName = null;								// Funzione applicativa in fase di load/start/xctl
	private ForwardFunction functionObject = null;					// Funzione applicativa oggetto

	
	////////////////////////////////////////////////////////////////
	// Informazioni correnti generali di esecuzione e di controllo
	////////////////////////////////////////////////////////////////
	
	private int returnCode = 0;                						// Ultimo return code impostato (ordinal di EnumForwardReturnCode)
	private Exception excp = null;									// Exception generata dalla funzione
	private DataBaseStatusDetailed dbs = null;                   	// Ultimo errore fisico dettagliato di accesso ai dati in caso di errore database
	private JFrame activeFrame = null; 								// Frame corrente in esecuzione
	private ForwardForm activeForm = null; 						    // Form corrente in esecuzione
	private String activePanel = null; 								// Panel corrente in esecuzione, su cui si è verificato l'evento
	private boolean activePanelContextControlsGood = false;         // Stato controlli di contesto pannello corrente a fronte di action PANEL_CONTROLS_CONTEXT
	private EnumForwardAction activeAction = null;            		// Action corrente come START_DIALOG_INFORMATION, ... START_DIALOG_USER_NO_MODAL
	private ForwardDoParms activeDoParms = null;            		// Descrittore parametri action corrente
	private EnumForwardEvent activeEvent = null;            		// Evento corrente come ON_APPL_TREE_NODE_SELECTED, ...  
    private String activeActionSkipId = "";							// Identificativo di skip azioni da eseguire possibile impostato da  DO_SKIP_SET_ID("id") 
    private String activeActionSkipIdOccurred = "";					// Identificativo di skip effettivamente avvenuto 
    private boolean activeActionsToSkip = false;                    // Indica se stoppare tutte le successive azioni da eseguire relative all'identificativo di stop corrente
    private int activeActionsStackLevel = 0;                    	// Indica il livello dello stack 1-based di esecuzione degli eventi, utilizzato per skip esecuzione eventi
    private EnumLanguage activeLanguage = null;						// Linguaggio corrente attivo, impostato allo user login, default inglese
    
	/////////////////////////////////////////////////////
	// Info metodo riusabile in esecuzione o eseguito
	/////////////////////////////////////////////////////
	
	private String reusableMethodClassName = "";					// Nome classe dove è definito il metodo riusabile
	private String reusableMethodName = "";							// Nome metodo riusabile in esecuzione o eseguito
	private Exception reusableMethodException = null;				// Exception con cui il metodo è terminato in  modo anormale
	private boolean isReusableMethodExceptionHandling = false;	    // True indica trattamento in corso di exeception verificata
	private boolean isReusableMethodRunning = false;				// True indica che il metodo è in esecuzione
	
	
	//////////////////////////////////////////////////////////////////
	// Info Dialog attivo o ultimo terminato
	//////////////////////////////////////////////////////////////////
	
	private boolean isDialogActive = false;                         // True indica Dialogo di sistema o applicativo ancora attivo
	private boolean isDialogModal = false;             				// True indica dialogo di tipo modale
	private boolean isDialogUser = false;             				// True indica dialogo attivo applicativo, con un panel specifico da visualizzare
	private boolean isDialogWithIcon = false;             			// True indica dialogo con icona visualizzata
	private boolean isDialogWithIconUser = false;          		    // True indica dialogo con icona non standard impostata dall'utente
	private JDialog dialogActive = null;                            // Oggetto JDialog attivo o ultimo attivo
	private String dialogName = "";                                 // Nome JDialog attivo o ultimo attivo
	private ArrayList<JDialog> al_dialogUserActive = null;			// JDialog applicative modali e non modali attive
	private String dialogTitle = "";                                // JDialog title
	private Object dialogMessage = "";                          	// Text message
	private String dialogUserFormName = null;                       // Form applicativo da gestire come dialogo a fronte di START_DIALOG_USER_NO_MODAL o START_DIALOG_USER_MODAL
	private JPanel dialogUserForm = null;                           // JPanel principale form
	private int dialogType = 0;                                 	// Tipo dialogo da JOptionPane.ERROR_MESSAGE, INFORMATION_MESSAGE, WARNING_MESSAGE, QUESTION_MESSAGE, PLAIN_MESSAGE
	private int dialogTypeOptions = 0;                              // Opzioni da JOptionPane.YES_NO_OPTION, YES_NO_CANCEL_OPTION, OK_CANCEL_OPTION, DEFAULT_OPTION
	private String dialogOptionPanelName = "";                      // Nome JOptionPane visualizzato su JDialog attivo o ultimo attivo
	private Icon dialogIcon = null;          						// Oggetto Icon se isDialogModalWithIconUser = true
	private String dialogIconPath = null;          					// Path Icon se isDialogModalWithIconUser = true
	private String dialogLabelsCustom[] = null;          			// Array con label personalizzate
	private String dialogLabelCustomSelected = null;          		// Label custom preselezionata
	private int dialogOptionChoosed = 0;                            // Da JOptionPane.YES_OPTION, NO_OPTION, CANCEL_OPTION, OK_OPTION, CLOSED_OPTION
	private boolean dialogWantsInput = false;                       // True indica JDialog di input con i successivi campi significativi
	private Object dialogSelectionValues[] = null;                  // Valori da selezionare da comboBox
	private Object dialogInitialSelectionValue = null;              // Valore di default per JTextBox o JComboBox di input
	private String dialogInputValue = "";                           // Valore immesso da JTextField o JComboBox (normalmente un testo)

	
	//////////////////////////////////////////////////////////////////
	// Info Ultimo colore/font selezionato da dialogo standard
	//////////////////////////////////////////////////////////////////
	
	private Color chooserColorSelected = null;						// Colore selezionato
	private Font chooserFontSelected = null;						// Font selezionato

	
	//////////////////////////////////////////////////////////////////
	// Info per LookUpTable
	//////////////////////////////////////////////////////////////////
	
    private String lookupTableCode = "";							// Codice tabella forward
    
    
	/////////////////////////////////////////////////////////////////////////////////////
	// Info specifiche per fileChooser
    //   Metodi wrapper per fileChooserName, fileChooserTitle, fileChooserSelectionMode
    //                    , fileChooserSelectedFile, fileChooserSelectedFiles
	/////////////////////////////////////////////////////////////////////////////////////
	
    private JFileChooser fileChooserActive = null;					// Oggetto JFileChooser attivo
    private boolean isFileChooserMultiSelectionEnabled = false;		// True indica selezione multipla abilitata
	private int fileChooserResponse = 0;                            // Responso come JFileChooser.APPROVE_OPTION, CANCEL_OPTION, ERROR_OPTION
   
    
	 /////////////////////////////////////////////////////////////
	 // Info specifiche per logical data view (ldv)
	 /////////////////////////////////////////////////////////////
	private String ldvName = "";                                   	// Nome logical data view
	private Class<? extends ForwardLogicalDataView> ldvClass = null;// Classe logical data view
	private String ldvEntityAs = "";                                // Nome interno entity nella logical data view
	private Object ldvObject = null;								// Oggetto logical data view
	private int ldvReturnCode = 0;									// Return code codificato (ordinal di EnumForwardLogicalDataViewControl)
	private int ldvRow = 0;											// Numero corrente riga 
	private int ldvPage = 0;										// Numero corrente pagina 
	private int ldvLimitRows = 0;									// Limite numero righe da leggere
	private int ldvPageRows = 0;									// Numero righe per pagina da restituire all'applicazione

    
	/////////////////////////////////////////////////////////////////////////////
	// Ultimi eventi intercettati dai listener, per eventuale gestione custom 
	/////////////////////////////////////////////////////////////////////////////
	
	private ListSelectionEvent eventListSelection = null;			//
	private ActionEvent eventAction = null;							//
	private AdjustmentEvent eventAdjustment = null;					//
	private FocusEvent eventFocus = null;							//
	private ItemEvent eventItem = null;								//
	private MouseEvent eventMouse = null;							//
	private MouseEvent eventMouseMotion = null;						//
	private MouseWheelEvent eventMouseWheelMotion = null;   		//
	private KeyEvent eventKey = null;   							//
	private ChangeEvent eventChange = null;   						//
	private DocumentEvent eventDocument = null;   					//
	private WindowEvent eventWindow = null;   						//
	private TreeExpansionEvent eventTreeExpansion = null;			//
	private TreeSelectionEvent eventTreeSelection = null;			//
	private TreeModelEvent eventTreeModelEvent = null;				//
	

	/////////////////////////////////////////////////////////////////////////////
	// Ultimo componente intercettato da evento a fronte di qualsiasi listener
	// Per esempio da listener ActionPerformed a fronte di Click
	/////////////////////////////////////////////////////////////////////////////
	
	private EnumForwardComponent eventComponentType = null;			// Tipo componente
	private Object eventComponentObject = null;						// Oggetto swing componente
	private String eventComponentName = null;						// Nome componente su cui è avvenuto l'evento
	private int eventClickCount = 0;								// Numero click a fronte di eventi di mouse
	private Point eventPoint = null;								// Coordinate del componente su cui è stato fatto click
	private int eventX = 0;											// Coordinate X del componente su cui è stato fatto click
	private int eventY = 0;											// Coordinate Y del componente su cui è stato fatto click
	private int eventWheelRotations = 0;							// Numero rotazioni (clik) della rotella del mouse
	private int eventAdjustmentType = 0;							// Tipo adjustment per JScrollBar
	private int eventAdjustmentValue = 0;							// Valore adjustment value

	// Info correnti a fronte di eventi intercettati da ChangeListener X JSlider e JSpinner
	private int eventSliderChangedValue = 0;						// Valore numerico slider
	private String eventSpinnerChangedValueText = "";				// Valore text spinner 
	private int eventSpinnerChangedValueNum = 0;					// Valore numerico spinner 
	private Date eventSpinnerChangedValueDate = null;				// Valore date spinner 
	
	// Info correnti a fronte di eventi intercettati da DocumentListener
	private int eventDocumentCharLenght = 0;						// N. crt inseriti/deletati/rimpiazzati
	private int eventDocumentCharOffset = 0;						// Offset crt inseriti/deletati/rimpiazzati
	private int eventDocumentLength = 0;							// N. crt presenti nel documento/campo
	private int eventDocumentStartOffset = 0;						// Offset di inizio nel documento/campo
	private int eventDocumentEndOffset = 0;							// Offset di fine nel documento/campo
	private String eventDocumentCharText = "";						// Testo documento da CharOffset x CharLength
	private String eventDocumentText = "";							// Testo documento da offset 0 a Length

	// Info correnti a fronte di eventi intercettati da KeyListener
	private int eventKeyCode = 0; 									// Valore numerico key di tastiera es.(KeyEvent.VK_ENTER)
	private char eventKeyChar = 0; 									// Carattere di tastiera digitato
	private String eventKeyText = ""; 								// Testo carattere decodificato e localizzato (se Enter è "invio",...)
	private int eventKeyLocation = 0; 								// Posizione key nel tasto della keyboard, da  KeyEvent.KEY_LOCATION_STANDARD, ....
	private String eventKeyLocationString = ""; 					// Testo Posizione key nel tasto della keyboard, da  KeyEvent.KEY_LOCATION_STANDARD, ....
	private boolean eventKeyIsActionKey = false; 					// True indica action key come F1, F2, Enter
	private int eventKeyModifiersMask = 0; 							// Contiene le maschere dei modifiers attivi ovvero CTRL, SHIFT etc
	private int eventKeyModifiersText = 0; 							// Contiene le maschere in forma testuale come Shift+Ctrl etc
	private boolean isEventKeyShift = false;                        // True indica SHIFT premuto insieme al tasto premuto/rilasciato
	private boolean isEventKeyCtrl = false;                      	// True indica CTRL premuto insieme al tasto premuto/rilasciato
	private boolean isEventKeyAlt = false;                      	// True indica ALT premuto insieme al tasto premuto/rilasciato
	private boolean isEventKeyAltGraph = false;                     // True indica ALT_GRAPH premuto insieme al tasto premuto/rilasciato
	
	// Info correnti per JTabbedPane valorizzate da evento più recente
	private String tabbedPaneTicked = "";							// Nome del pannello selezionato attraverso click sul tabbed pane
	
	// Info correnti per JTable valorizzate da evento più recente.
	// Disponobili in codice applicativo riusabile a fronte di eventi su JList
	// e attivazione listener attraverso ON_EVENT() direttive
	private Object tableBoundObject = null;							// Oggetto applicativo associato a riga	tabella	 
	private int tableSelectedRow = 0;								// Numero riga selezionata (o la prima)
	private int tableSelectedRowCount = 0;							// Numero righe selezionate
	private int tableSelectedRows[] = null;							// Numeri righe selezionate			
	private int tableUnselectedRows[] = null;						// Numeri righe deselezionate			
	private int tableSelectedColumn = 0;							// Numero colonna selezionata (o la prima)
	private int tableSelectedColumnCount = 0;						// Numero colonne selezionate
	private int tableSelectedColumns[] = null;						// Numeri colonne selezionate			
	private int tableUnselectedColumns[] = null;					// Numeri colonne deselezionate			
	private int tableSelectionMode = 0; 							// ListSelectionModel.MULTIPLE_INTERVAL_SELECTION, ListSelectionModel.SINGLE_INTERVAL_SELECTION
	private boolean isTableRowsSelected = false; 					// E' stata selezionata una o più righe
	private boolean isTableColumnsSelected = false; 				// E' stata selezionata una o più colonne
	private boolean isTableRowSelectionAllowed = false; 			// Abilitazione selezione di righe
	private boolean isTableColumnSelectionAllowed = false; 			// Abilitazione selezione di colonne
	private boolean isTableCellSelectionAllowed = false; 			// Abilitazione selezione di celle
	private boolean isTableRowToDiscard = false;                	// In fase di populate, true fa scartare la riga

	
	// Info correnti per JTree valorizzate da evento più recente.
	// Disponobili in codice applicativo riusabile a fronte di eventi su JTree
	// e attivazione listener attraverso ON_EVENT() direttive
	private ForwardTreeModel eventTreeModel = null;                 // Modello tree di forward che eredita da DefaultTreeModel (contiene tutte le info particolari)
    private TreeSelectionModel eventTreeSelectionModel = null;      // Selection model tree
	private int eventTreeNodeIndex = 0;    							// Indice, nel suo parent del nodo selezionato, 0-based
	private DefaultMutableTreeNode eventTreeNode = null;    		// Nodo correntemente selezionato, da un click o primo di una selezione singola o multipla
	private DefaultMutableTreeNode eventTreeNodeParent = null;    	// Nodo genitore di quello selected
	private DefaultMutableTreeNode eventTreeNodesParent[] = null;   // Nodi parents da root fino al parent diretto di node
	private DefaultMutableTreeNode eventTreeNodesSelected[] = null; // Nodi correntemente selezionati, anche se a fronte di selezioni multiple
	private DefaultMutableTreeNode eventTreeNodesChild[] = null; 	// Nodi figli del nodo expanded
	private Object eventTreeNodeObject = null;    					// User object associato a nodo correntemente selezionato, o al primo nodo selezionato
	private Object eventTreeNodeParentObject = null;        		// Oggetto applicativo nodo genitore di quello selected
	private Object eventTreeNodesParentObject[] = null;    			// Oggetti applicativi nodi parents da root fino al parent diretto di node
	private Object eventTreeNodesSelectedObject[] = null; 			// Oggetti applicativi associati a nodi correntemente selezionati, anche se a fronte di selezioni multiple
	private Object eventTreeNodesChildObject[] = null; 				// Oggetti applicativi associati Nodi figli del nodo expanded
	private int eventTreeNodesSelectedCount = 0;    				// Numero nodi selezionati, anche non contigui. Coincide con il size di eventTreeNodesSelectedObject e eventTreeNodesSelected
	private int eventTreeNodeChildCount = 0;    					// Numero figli del nodo eventTreeNode, 0 se nodo leaf 
	private boolean isEventTreeNodeLeaf = false;                    // True se il nodo eventTreeNode è leaf
	
	// Info correnti per JList valorizzate da evento più recente.
	// Disponobili in codice applicativo riusabile a fronte di eventi su JList
	// e attivazione listener attraverso ON_EVENT() direttive
	private String listName = "";									// Nome JList			 
	private int listSelectedIndex = 0;								// Numero riga list 0-based selezionata				 
	private int listSelectedIndexLeading = 0;						// Fino a a riga					 
	private String listSelectedItem = "";							// Valore riga selezionata	(o primo valore)		 
	private Object listBoundObject = null;							// Oggetto applicativo associato a riga	selezionata	(o primo oggetto)			 
	private ArrayList<String> al_listSelectedItem = null;  			// Valori selezionati in caso selezione multipla
	private ArrayList<Object> al_listBoundObject = null;			// Oggetti applicativi associati a righe selezionate	 
	private ArrayList<Integer> al_listSelectedIndex = null; 		// Index righe in caso selezione multipla
	private ArrayList<Integer> al_listUnSelectedIndex = null;   	// Index righe deselezionate da ultima selezione
	private ArrayList<String> al_listUnSelectedItem = null;  		// Valori deselezionati da ultima selezione
	private boolean isListItemToDiscard = false;                	// In fase di populate, true fa scartare l'item
	
	// Info correnti per JComboBox valorizzate da evento piu recente.
	private String comboBoxName = "";								// Nome JComboBox
	private int comboBoxSelectedIndex = 0;							// Numero riga 0-based selezionata				 
	private String comboBoxSelectedItem = "";						// Valore item selezionata	 
	private Object comboBoxBoundObject = null;						// Oggetto applicativo associato a item	selezionato	  
	private ArrayList<Object> al_comboBoxEventMasked = null;		// Oggetti InnerOnEvent maskerati su evento stateChanged
	private boolean isComboBoxItemToDiscard = false;                // In fase di populate, true fa scartare l'item
	private boolean isComboBoxEventMasked = false;					// In fase di populate, true nooperizza evento di state changed, innescato automaticamente da java al popolamento della combo
	
	// Info correnti errori a fronte di action di PANEL_CONTROLS
	private boolean errorsFound = false;                            // Individuati errori nel controllo di un panel, formalim, di eseistenza o user
	private boolean errorsFormal = false;               			// Individuati errori nel controllo di un panel, formali
	private boolean errorsExistenceRuleTable = false;               // Individuati errori nel controllo di un panel, esistenza/inesistenza in rule table
	private boolean errorsExistenceEntity = false;               	// Individuati errori nel controllo di un panel, esistenza/inesistenza entity
	private boolean errorsUser = false;               				// Individuati errori nel controllo di un panel, nel metodo user dependent
	private ArrayList<InnerComponentMessage> al_message = null;    	// Lista ultimi messaggi di errore e non riscontrati
	
	
	
	/** Constructor empty */
	public ForwardSystem() {
		super();
		al_listSelectedItem = new ArrayList<String> ();
		al_listUnSelectedItem = new ArrayList<String> ();
		al_listBoundObject = new ArrayList<Object> ();
		al_listSelectedIndex = new ArrayList<Integer> ();
		al_listUnSelectedIndex = new ArrayList<Integer> ();
		tableSelectedRows = new int[0];	
		tableUnselectedRows = new int[0];		
		tableSelectedColumns = new int[0];			
		tableUnselectedColumns = new int[0];		
		activeAction = EnumForwardAction.NOT_ASSIGNED;
		activeEvent = EnumForwardEvent.NOT_ASSIGNED;
		al_dialogUserActive = new ArrayList<JDialog> ();
		al_comboBoxEventMasked = new ArrayList<Object> ();
		al_message = new ArrayList<InnerComponentMessage> ();
		activeLanguage = EnumLanguage.ENGLISH;

	}

	/**
	 * Gets the user customer code for the active license.<br>
	 * <p>
	 * @return the userCustomerCode
	 */
	public String getUserCustomerCode() {
		return userCustomerCode;
	}

	/**
	 * Sets the user customer code for the active license.<br>
	 * <p>
	 * @param userCustomerCode the userCustomerCode to set
	 */
	public void setUserCustomerCode(String userCustomerCode) {
		this.userCustomerCode = userCustomerCode;
	}

	/**
	 * Gets the name of the class where the reusable method is defined.<br>
	 * Call getReusableMethodName() to obtain the methodName.<br>
     * Forward monitor search the method in the function class and, if is not
     * defined there, in all classes configurated.<br>
     * <p>
	 * @return the reusableMethodClassName
	 */
	public String getReusableMethodClassName() {
		return reusableMethodClassName;
	}

	/**
	 * Sets the name of the class where the reusable method is defined.<br>
	 * Call getReusableMethodName() to obtain the methodName.<br>
     * Forward monitor search the method in the function class and, if is not
     * defined there, in all classes configurated.<br>
     * <p>
	 * @param reusableMethodClassName the reusableMethodClassName to set
	 */
	public void setReusableMethodClassName(String reusableMethodClassName) {
		this.reusableMethodClassName = reusableMethodClassName;
	}

	/**
	 * Gets the reusable method name running or run.<br>
	 * <p>
	 * @return the reusableMethodName
	 */
	public String getReusableMethodName() {
		return reusableMethodName;
	}

	/**
	 * Sets the reusable method name running or run.<br>
	 * <p>
	 * @param reusableMethodName the reusableMethodName to set
	 */
	public void setReusableMethodName(String reusableMethodName) {
		this.reusableMethodName = reusableMethodName;
	}

	/**
	 * Gets the exception occurred during reusable method running.<br>
	 * Null means method correctly terminated.<br>
	 * <p>
	 * @return the reusableMethodException
	 */
	public Exception getReusableMethodException() {
		return reusableMethodException;
	}

	/**
	 * Gets the exception occurred during reusable method running.<br>
	 * Null means method correctly terminated.<br>
	 * <p>
	 * @param reusableMethodException the reusableMethodException to set
	 */
	public void setReusableMethodException(Exception reusableMethodException) {
		this.reusableMethodException = reusableMethodException;
	}

	/**
	 * Gets if the reusable method is currently running.<br>
	 * Call getReusableMethodName() to obtain the methodName,<br>
	 * <p>
	 * @return the isReusableMethodRunning
	 */
	public boolean isReusableMethodRunning() {
		return isReusableMethodRunning;
	}

	/**
	 * Sets if the reusable method is currently running.<br>
	 * Call getReusableMethodName() to obtain the methodName,<br>
	 * <p>
	 * @param isReusableMethodRunning the isReusableMethodRunning to set
	 */
	public void setReusableMethodRunning(boolean isReusableMethodRunning) {
		this.isReusableMethodRunning = isReusableMethodRunning;
	}

	
	
	/**
	 * Gets if an exception in a reusable application method occurred and it's currently under management.<br>
	 * Forward monitor will try to exec a declared function method to handle the exception, if any.<br>
	 * <p>
	 * @return the isReusableMethodExceptionHandling
	 */
	public boolean isReusableMethodExceptionHandling() {
		return isReusableMethodExceptionHandling;
	}

	/**
	 * Sets if an exception in a reusable application method occurred and it's currently under management.<br>
	 * Forward monitor will try to exec a declared function method to handle the exception, if any.<br>
	 * <p>
	 * @param isReusableMethodExceptionHandling the isReusableMethodExceptionHandling to set
	 */
	public void setReusableMethodExceptionHandling(boolean isReusableMethodExceptionHandling) {
		this.isReusableMethodExceptionHandling = isReusableMethodExceptionHandling;
	}

	/**
	 * @return the release
	 */
	public int getRelease() {
		return release;
	}

	/**
	 * @param release the release to set
	 */
	public void setRelease(int release) {
		this.release = release;
	}

	/**
	 * @return the version
	 */
	public int getVersion() {
		return version;
	}

	/**
	 * @param version the version to set
	 */
	public void setVersion(int version) {
		this.version = version;
	}

	/**
	 * @return the modification
	 */
	public int getModification() {
		return modification;
	}

	/**
	 * @param modification the modification to set
	 */
	public void setModification(int modification) {
		this.modification = modification;
	}

	/**
	 * Gets system defaults.<br>
	 * <p>
	 * @return the sd
	 */
	public UserConfiguration getSystemDefaults() {
		return sd;
	}

	/**
	 * Sets system defaults.<br>
	 * <p>
	 * @param sd the sd to set
	 */
	public void setSystemDefaults(UserConfiguration sd) {
		this.sd = sd;
	}

	/**
	 * Gets the {@link DataBaseManager} object to perform direct database access<br>
	 * without to use the forward logocal data view system.<br>
	 * <p>
	 * @return the dbm
	 */
	public DataBaseManager getDbm() {
		return dbm;
	}

	/**
	 * Set the {@link DataBaseManager} object to perform direct database access<br>
	 * without to use the forward logocal data view system.<br>
	 * <p>
	 * @param dbm the dbm to set
	 */
	public void setDbm(DataBaseManager dbm) {
		this.dbm = dbm;
	}

	/**
	 * Gets the Connectio object to perform direct database access<br>
	 * without to use the forward logocal data view system.<br>
	 * <p>
	 * @return the dbConn
	 */
	public Connection getDbConn() {
		return dbConn;
	}

	/**
	 * Sets the Connectio object to perform direct database access<br>
	 * without to use the forward logocal data view system.<br>
	 * <p>
	 * @param dbConn the dbConn to set
	 */
	public void setDbConn(Connection dbConn) {
		this.dbConn = dbConn;
	}

	/**
	 * Gets the {@link DataBaseEntityInterface} object to perform direct database access<br>
	 * without to use the forward logocal data view system.<br>
	 * <p>
	 * @return the dbei
	 */
	public DataBaseEntityInterface getDbei() {
		return dbei;
	}

	/**
	 * sets the {@link DataBaseEntityInterface} object to perform direct database access<br>
	 * without to use the forward logocal data view system.<br>
	 * <p>
	 * @param dbei the dbei to set
	 */
	public void setDbei(DataBaseEntityInterface dbei) {
		this.dbei = dbei;
	}

	/**
	 * Sets the user login code.<br>
	 * <p>
	 * @return the userLogin
	 */
	public String getUserLogin() {
		return userLogin;
	}

	/**
	 * Sets the user login code.<br>
	 * <p>
	 * @param userLogin the userLogin to set
	 */
	public void setUserLogin(String userLogin) {
		this.userLogin = userLogin;
	}

	
	/**
	 * Gets the function name for load/start/xctl actions.<br>
	 * <p>
	 * @return the functionName
	 */
	public String getFunctionName() {
		return functionName;
	}

	/**
	 * Sets the function name for load/start/xctl actions.<br>
	 * <p>
	 * @param functionName the functionName to set
	 */
	public void setFunctionName(String functionName) {
		this.functionName = functionName;
	}

	/**
	 * Gets the function object for load/start/xctl actions.<br>
	 * <p>
	 * @return the functionObject
	 */
	public ForwardFunction getFunctionObject() {
		return functionObject;
	}

	/**
	 * Sets the function object for load/start/xctl actions.<br>
	 * <p>
	 * @param functionObject the functionObject to set
	 */
	public void setFunctionObject(ForwardFunction functionObject) {
		this.functionObject = functionObject;
	}

	/**
	 * Gets the return code of last operation.<br>
	 * <p>
	 * It's an ordinal or {@link EnumForwardReturnCode}.<br>
	 * <p>
	 * @return the returnCode
	 */
	public int getReturnCode() {
		return returnCode;
	}

	/**
	 * Sets the return code.<br>
	 * <p>
	 * It's an ordinal or {@link EnumForwardReturnCode}.<br>
	 * <p>
	 * @param returnCode the returnCode to set
	 */
	public void setReturnCode(int returnCode) {
		this.returnCode = returnCode;
	}

	
	/**
	 * Gets the last exception occurred.<br>
	 * <p>
	 * @return the excp
	 */
	public Exception getExcp() {
		return excp;
	}

	/**
	 * Sets the last exception occurred.<br>
	 * <p>
	 * @param excp the excp to set
	 */
	public void setExcp(Exception excp) {
		this.excp = excp;
	}

	/**
	 * Gets the detailed status of the last database operation in error.<br>
	 * <p>
	 * @return the dbs
	 */
	public DataBaseStatusDetailed getDbs() {
		return dbs;
	}

	/**
	 * Sets the detailed status of the last database operation in error.<br>
	 * <p>
	 * @param dbs the dbs to set
	 */
	public void setDbs(DataBaseStatusDetailed dbs) {
		this.dbs = dbs;
	}

	/**
	 * Gets the function descriptor currently running.<br>
	 * <p>
	 * @return the activeFunction
	 */
	public ForwardFunction getActiveFunction() {
		return this.monitorDesktop.mcb.function;
	}

	/**
	 * Gets the function descriptor currently running.<br>
	 * <p>
	 * @return the monitor dektop reference
	 */
	public ForwardMonitorDesktop getMonitorDesktop() {
		return this.monitorDesktop;
	}

	/**
	 * Sets the function descriptor currently running.<br>
	 * <p>
	 * @param the monitor dektop reference
	 */
	public void setMonitorDesktop(ForwardMonitorDesktop monitorDesktop) {
		this.monitorDesktop = monitorDesktop;
		return;
	}

	

	/**
	 * Get the reference to the {@link ForwardSystem} object of caller function.<br>
	 * <p>
	 * This method is intended for internal use only.<br>
	 * <p>
	 * @return the systemCaller
	 */
	public ForwardSystem getSystemCaller() {
		return systemCaller;
	}

	/**
	 * sets the reference to the {@link ForwardSystem} object of caller function.<br>
	 * <p>
	 * This method is intended for internal use only.<br>
	 * <p>
	 * @param systemCaller the systemCaller to set
	 */
	public void setSystemCaller(ForwardSystem systemCaller) {
		this.systemCaller = systemCaller;
	}

	/**
	 * Gets the {@link JFrame} currently active as declared by function.<br>
	 * >p>
	 * @return the activeFrame
	 */
	public JFrame getActiveFrame() {
		return activeFrame;
	}

	/**
	 * Gets the {@link JFrame} currently active as declared by function.<br>
	 * >p>
	 * @param activeFrame the frameActive to set
	 */
	public void setActiveFrame(JFrame activeFrame) {
		this.activeFrame = activeFrame;
	}

	
	/**
	 * Gets the form currently active.<br>
	 * <p>
	 * @return the activeForm
	 */
	public ForwardForm getActiveForm() {
		return activeForm;
	}

	/**
	 * set the form currently active.<br>
	 * <p>
	 * @param activeForm the activeForm to set
	 */
	public void setActiveForm(ForwardForm activeForm) {
		this.activeForm = activeForm;
	}

	
	/**
	 * Gets the active panel.<br>
	 * <p>
	 * The active panel is valid when an event occurs on a component layed out a panel.<br>
	 * <p> 
	 * @return the activePanel name
	 */
	public String getActivePanel() {
		return activePanel;
	}

	/**
	 * Sets the active panel.<br>
	 * <p>
	 * The active panel is valid when an event occurs on a component layed out a panel.<br>
	 * <p> 
	 * @param activePanel the active Panel name to set
	 */
	public void setActivePanel(String activePanel) {
		this.activePanel = activePanel;
	}

	/**
	 * Gets the active forward action, declared by function as a <code>ON_CONDITION(..DO_..)<br>
	 * directive, available in the reusable application method called when <br>
	 * the event occurs.<br>
	 * <p>
	 * @return the activeAction
	 */
	public EnumForwardAction getActiveAction() {
		return activeAction;
	}

	/**
	 * Sets the active forward action, declared by function as a <code>ON_CONDITION(..DO_..)<br>
	 * directive, available in the reusable application method called when <br>
	 * the event occurs.<br>
	 * <p>
	 * @param activeAction the activeAction to set
	 */
	public void setActiveAction(EnumForwardAction activeAction) {
		this.activeAction = activeAction;
	}

	
	
	/**
	 * Gets the the parameters object of the current application action,<br>
	 * activated by a ON_EVENT() DO_something declaration.<br>
	 * This object is available for the user logic method called.<br>
	 * <p>
	 * The main scope of this method is to let the application access to parameters<br>
	 * thru the variable array of {@link ForwardDoParms} parms.<br>
	 * @return the activeDoParms
	 */
	public ForwardDoParms getActiveDoParms() {
		return activeDoParms;
	}
 
	/**
	 * Sets the the parameters object of the current application action,<br>
	 * activated by a ON_EVENT() DO_something declaration.<br>
	 * This object is available for the user logic method called.<br>
	 * <p>
	 * The main scope of this method is to let the application access to parameters<br>
	 * thru the variable array of {@link ForwardDoParms} parms.<br>
	 * @param activeDoParms the activeDoParms to set
	 */
	public void setActiveDoParms(ForwardDoParms activeDoParms) {
		this.activeDoParms = activeDoParms;
	}

	/**
	 * Gets the active forward event, declared by function as a <code>ON_CONDITION(..)<br>
	 * directive, available in the reusable application method called when <br>
	 * the event occurs.<br>
	 * <p>
	 * It's a convenient way to share logic application among different events.<br>
	 * <p>
	 * @return the activeEvent
	 */
	public EnumForwardEvent getActiveEvent() {
		return activeEvent;
	}

	/**
	 * Sets the active forward event, declared by function as a <code>ON_CONDITION(..)<br>
	 * directive, available in the reusable application method called when <br>
	 * the event occurs.<br>
	 * <p>
	 * It's a convenient way to share logic application among different events.<br>
	 * <p>
	 * This method is intended for the the esclusive use of forward monitor.<br>
	 * <p>
	 * @param activeEvent the activeEvent to set
	 */
	public void setActiveEvent(EnumForwardEvent activeEvent) {
		this.activeEvent = activeEvent;
	}

	
	/**
	 * Gets the active action stop identifier<br>
	 * <p>
	 * When a declared <code>SKIP_EVENT_ACTIONS()</code> has been executed
	 * on a specific <code>ON_EVENT()</code> condition, 
	 * and the stop identifier set by means of <code>SKIP_SET_ID()</code><br>
	 * is equal, all next action there will be not executed.<br>
	 * 
	 * 
	 * @return the activeActionSkipId
	 */
	public String getActiveActionSkipId() {
		return activeActionSkipId;
	}

	/**
	 * Sets the active action skip identifier<br>
	 * <p>
	 * When a declared <code>STOP_EVENT_ACTIONS()</code> has been executed
	 * on a specific <code>ON_EVENT()</code> condition, 
	 * and the stop identifier set by means of <code>STOP_SET_ID()</code><br>
	 * is equal, all next action there will be not executed.<br>
	 * 
	 * @param activeActionSkipId the activeActionSkipId to set
	 */
	public void setActiveActionSkipId(String activeActionSkipId) {
		this.activeActionSkipId = activeActionSkipId;
	}

	/**
	 * Gets if the execution of all next action must be skipped<br>
	 * <p>
	 * @return the activeActionsToSkip
	 */
	public boolean getActiveActionsToSkip() {
		return activeActionsToSkip;
	}

	/**
	 * Sets if the execution of all next action must be skipped<br>
	 * <p>
	 * @param activeActionsToSkip the activeActionsToSkip to set
	 */
	public void setActiveActionsToSkip(boolean activeActionsToSkip) {
		this.activeActionsToSkip = activeActionsToSkip;
	}

	/**
	 * Gets the active action skip identifier occurred<br>
	 * <p>
	 * When a declared <code>SKIP_EVENT_ACTIONS()</code> has been executed
	 * on a specific <code>ON_EVENT()</code> condition, 
	 * and the stop identifier set by means of <code>SKIP_SET_ID()</code><br>
	 * is equal, all next action there will be not executed.<br>
	 * 
	 * 
	 * @return the activeActionSkipIdOccurred
	 */
	public String getActiveActionSkipIdOccurred() {
		return activeActionSkipIdOccurred;
	}

	/**
	 * Sets the active action skip identifier occurred<br>
	 * <p>
	 * When a declared <code>SKIP_EVENT_ACTIONS()</code> has been executed
	 * on a specific <code>ON_EVENT()</code> condition, 
	 * and the stop identifier set by means of <code>SKIP_SET_ID()</code><br>
	 * is equal, all next action there will be not executed.<br>
	 * <p>
	 * @param activeActionSkipIdOccurred the activeActionSkipIdOccurred to set
	 */
	public void setActiveActionSkipIdOccurred(String activeActionSkipIdOccurred) {
		this.activeActionSkipIdOccurred = activeActionSkipIdOccurred;
	}

	/**
	 * Gets the current 1-based action execution stack level.<br>
	 * <p>
	 * When an events declared by funzion occurs, actions related are executed,
	 * at the first stack level.<br>
	 * Some events could then generated a recursive actions execution.<br>
	 * <p>
	 * @return the activeActionsStackLevel
	 */
	public int getActiveActionsStackLevel() {
		return activeActionsStackLevel;
	}

	/**
	 * Sets the current 1-based action execution stack level.<br>
	 * <p>
	 * When an events declared by funzion occurs, actions related are executed,
	 * at the first stack level.<br>
	 * Some events could then generated a recursive actions execution.<br>
	 * <p>
	 * @param activeActionsStackLevel the activeActionsStackLevel to set
	 */
	public void setActiveActionsStackLevel(int activeActionsStackLevel) {
		this.activeActionsStackLevel = activeActionsStackLevel;
	}

	
	
	/**
	 * Gets the active language.<br>
	 * <p>
	 * By default the language is english and it's normally used for all<br>
	 * started functions, customizations and so on.<br>
	 * The language will be updated with the language of the user login.<br>
	 * <p>
	 * @return the activeLanguage
	 */
	public EnumLanguage getActiveLanguage() {
		return activeLanguage;
	}

	/**
	 * Sets the active language.<br>
	 * <p>
	 * By default the language is english and it's normally used for all<br>
	 * started functions, customizations and so on.<br>
	 * The language will be updated with the language of the user login.<br>
	 * <p>
	 * @param activeLanguage the activeLanguage to set
	 */
	public void setActiveLanguage(EnumLanguage activeLanguage) {
		this.activeLanguage = activeLanguage;
	}

	/**
	 * Gets the status of context controls on the current panel.<br>
	 * <p>
	 * When a <code>PANEL_CONTROLS_CONTEXT</code> action is executed, an application
	 * method is activated to make context controls among fields.<br>
	 * This method must set the status of controls calling the <code>setActivePanelContextControlsGood()</code> method.<br>
	 * <br>
	 * @return the activePanelContextControlsGood
	 */
	public boolean isActivePanelContextControlsGood() {
		return activePanelContextControlsGood;
	}

	/**
	 * Sets the status of context controls on the current panel.<br>
	 * <p>
	 * When a <code>PANEL_CONTROLS_CONTEXT</code> action is executed, an application
	 * method is activated to make context controls among fields.<br>
	 * This method must set the status of controls calling the <code>setActivePanelContextControlsGood()</code> method.<br>
	 * <br>
	 * @param activePanelContextControlsGood the activePanelContextControlsGood to set
	 */
	public void setActivePanelContextControlsGood(boolean activePanelContextControlsGood) {
		this.activePanelContextControlsGood = activePanelContextControlsGood;
	}

	/**
	 * Gets if a {@link JDialog} or {@link JOptionPane} panel is currently active.<br>
	 * <p>
	 * @return the isDialogActive
	 */
	public boolean isDialogActive() {
		return isDialogActive;
	}

	/**
	 * Sets if a {@link JDialog} or {@link JOptionPane} panel is currently active.<br>
	 * <p>
	 * @param isDialogActive the isDialogActive to set
	 */
	public void setDialogActive(boolean isDialogActive) {
		this.isDialogActive = isDialogActive;
	}

	
	
	/**
	 * Gets all user application non modal dialogs currently active.<br>
	 * <p>
	 * An array of {@link JDialog} is returned and the property <code>name</code> with
	 * the name assigned by application, is set by forward at dialog start up.<br>
	 * <p>
	 * @return the al_dialogUser
	 */
	public ArrayList<JDialog> getDialogsUserActive() {
		return al_dialogUserActive;
	}

	/**
	 * Sets all user application non modal dialogs currently active.<br>
	 * <p>
	 * An array of {@link JDialog} is returned and the property <code>name</code> with
	 * the name assigned by application, is set by forward at dialog start up.<br>
	 * <p>
	 * @param al_dialogUserActive the al_dialogUserActive to set
	 */
	public void setDialogUserActive(ArrayList<JDialog> al_dialogUserActive) {
		this.al_dialogUserActive = al_dialogUserActive;
	}

	/**
	 * Remove the input name dialog from the list of active dialogs<br>
	 * <p>
	 * @param dialogName the name of the dialog
	 */
	public void removeDialogActive(String dialogName) {
		
 		for (JDialog dialogActive : this.getDialogsUserActive()) {
 			if (dialogActive.getName().equals(dialogName)) {
 				getDialogsUserActive().remove(dialogActive);
 				dialogActive.setVisible(false);
 				dialogActive.dispose();
 				break;
			}
		}

	}

	
	
	/**
	 * Gets if the dialog is modal.<br>
	 * <p>
	 * A modal dialog gets the focus until it will be closed.<br>
	 * <p>
	 * @return the isDialogModal
	 */
	public boolean isDialogModal() {
		return isDialogModal;
	}

	/**
	 * Sets if the dialog is modal.<br>
	 * <p>
	 * A modal dialog gets the focus until it will be closed.<br>
	 * <p>
	 * @param isDialogModal the isDialogModal to set
	 */
	public void setDialogModal(boolean isDialogModal) {
		this.isDialogModal = isDialogModal;
	}

	
	/**
	 * Gets if the dialog is a user application dialog.<br>
	 * <p>
	 * This is not a standard WARNING, ERROR, .. dialog but an application<br>
	 * dialog with a own user panel.<br>
	 * This dialog can be managed as modal or not modal.<br>
	 * <p>
	 * @return the isDialogUser
	 */
	public boolean isDialogUser() {
		return isDialogUser;
	}

	/**
	 * Sets if the dialog is a user application dialog.<br>
	 * <p>
	 * This is not a standard WARNING, ERROR, .. dialog but an application<br>
	 * dialog with a own user panel.<br>
	 * This dialog can be managed as modal or not modal.<br>
	 * <p>
	 * @param isDialogUser the isDialogUser to set
	 */
	public void setDialogUser(boolean isDialogUser) {
		this.isDialogUser = isDialogUser;
	}

	/**
	 * Gets the {@link JDialog} with the name specified, currently showed.<br>
	 * <p> 
	 * If the dialog is not in the curren active list, a null value will be returned.<br>
	 * <p>
	 * @return the {@link JDialog} object with the name specified
	 */
	public JDialog getDialogActive(String dialogName) {
		
		for (JDialog jdialog : this.getDialogsUserActive()) {
			if (jdialog.getName().equals(dialogName)) {
				return jdialog;
			}
		}
		return null;
	}

	/**
	 * Gets the {@link JDialog} object currently showed or the last showed.<br>
	 * <p> 
	 * @return the dialogActive
	 */
	public JDialog getDialogActive() {
		return dialogActive;
	}

	/**
	 * Sets the {@link JDialog} object currently showed or the last showed.<br>
	 * <p> 
	 * @param dialogActive the dialogActive to set
	 */
	public void setDialogActive(JDialog dialogActive) {
		this.dialogActive = dialogActive;
	}

	/**
	 * Gets the name assigned to the {@link JDialog} panel currently active.<br>
	 * <p>
	 * When a standard option Dialog (realized with a {@link JOptionPane} has to be executed<br>
	 * in modal mode, a {@link JDialog} modal panel will be created and the {@link JOptionPane} laid out.<br>
	 * <p>
	 * @return the dialogName
	 */
	public String getDialogName() {
		return dialogName;
	}

	/**
	 * Sets the name assigned to the {@link JDialog} panel currently active.<br>
	 * <p>
	 * When a standard option Dialog (realized with a {@link JOptionPane} has to be executed<br>
	 * in modal mode, a {@link JDialog} modal panel will be created and the {@link JOptionPane} laid out.<br>
	 * <p>
	 * @param dialogName the dialogName to set
	 */
	public void setDialogName(String dialogName) {
		this.dialogName = dialogName;
	}

	/**
	 * Gets the names of all currently active dialogs<br>
	 * <p>
	 * @return the dialogName
	 */
	public String[] getDialogUserActiveNames() {
		String ar_dialogName[] = null;
		int i = 0;
		
		ar_dialogName = new String[this.getDialogsUserActive().size()];
		for (JDialog jdialog : this.getDialogsUserActive()) {
			ar_dialogName[i++] = jdialog.getName();
		}
		return ar_dialogName;
	}


	/**
	 * Gets the title of the {@link JDialog}.<br>
	 * <p>
	 * @return the dialogTitle
	 */
	public String getDialogTitle() {
		return dialogTitle;
	}

	/**
	 * Sets the title of the {@link JDialog}.<br>
	 * <p>
	 * @param dialogTitle the dialogTitle to set
	 */
	public void setDialogTitle(String dialogTitle) {
		this.dialogTitle = dialogTitle;
	}

	/**
	 * Gets the message text displayed on the {@link JDialog}.<br>
	 * <p>
	 * @return the dialogMessage
	 */
	public Object getDialogMessage() {
		return dialogMessage;
	}

	/**
	 * Sets the message text displayed on the {@link JDialog}.<br>
	 * <p>
	 * @param dialogMessage the dialogMessage to set
	 */
	public void setDialogMessage(Object dialogMessage) {
		this.dialogMessage = dialogMessage;
	}

	
	/**
	 * Gets the user form name describing the user dialog, as declared by function.
	 * <br>
	 * @return the dialogUserFormName
	 */
	public String getDialogUserFormName() {
		return dialogUserFormName;
	}

	/**
	 * Sets the user form name describing the user dialog, as declared by function.
	 * <br>
	 * @param dialogUserFormName the dialogUserFormName to set
	 */
	public void setDialogUserFormName(String dialogUserFormName) {
		this.dialogUserFormName = dialogUserFormName;
	}

	/**
	 * Gets the user panel, as a {@link JPanel} object to be displyed as a dialog.<br>
	 * <p>
	 * The modal dialog option can be set to make the user panel modal.<br>
	 * The user panel can be declared by function as a complex form bye <code>BEGIN_FORM()</code> and<br>
	 *  <code>END_FORM()</code> declarations.
	 * <p>
	 * @return the dialogUserForm
	 */
	public JPanel getDialogUserForm() {
		return dialogUserForm;
	}

	/**
	 * Sets the user panel, as a {@link JPanel} object to be displyed as a dialog.<br>
	 * <p>
	 * The modal dialog option can be set to make the user panel modal.<br>
	 * <p>
	 * @param dialogUserForm the dialogUserForm to set
	 */
	public void setDialogUserForm(JPanel dialogUserForm) {
		this.dialogUserForm = dialogUserForm;
	}


	/**
	 * Gets the type of dialog, as a JOptionPane constant, for standardoption dialog.<br>
	 * <p>
	 * Valid values are JOptionPane constants as:
	 * <pre>
	 *  ERROR_MESSAGE
	 *  INFORMATION_MESSAGE
 	 *  WARNING_MESSAGE
 	 *  QUESTION_MESSAGE
 	 *  PLAIN_MESSAGE
 	 *  </pre>
 	 *  Actions origin could be {@link EnumForwardAction} as:
 	 * <pre>
	 * START_DIALOG_INFORMATION 
	 * START_DIALOG_WARNING 
	 * START_DIALOG_ERROR 
	 * START_DIALOG_QUESTION_YES_NO 
	 * START_DIALOG_QUESTION_YES_NO_CANCEL 
	 * START_DIALOG_INPUT_BY_TEXT 
	 * START_DIALOG_INPUT_BY_TEXTS 
 	 *  </pre>
	 * @return the dialogType
	 */
	public int getDialogType() {
		return dialogType;
	}

	/**
	 * Sets the type of dialog, as a JOptionPane constant, for standardoption dialog.<br>
	 * <p>
	 * Valid values are JOptionPane constants as:
	 * <pre>
	 *  ERROR_MESSAGE
	 *  INFORMATION_MESSAGE
 	 *  WARNING_MESSAGE
 	 *  QUESTION_MESSAGE
 	 *  PLAIN_MESSAGE
 	 *  </pre>
 	 *  Actions origin could be {@link EnumForwardAction} as:
 	 * <pre>
	 * START_DIALOG_INFORMATION 
	 * START_DIALOG_WARNING 
	 * START_DIALOG_ERROR 
	 * START_DIALOG_QUESTION_YES_NO 
	 * START_DIALOG_QUESTION_YES_NO_CANCEL 
	 * START_DIALOG_INPUT_BY_TEXT 
	 * START_DIALOG_INPUT_BY_TEXTS 
 	 *  </pre>
	 * @param dialogType the dialogType to set
	 */
	public void setDialogType(int dialogType) {
		this.dialogType = dialogType;
	}

	/**
	 * Gets the type of option, as a JOptionPane constant, for the standard dialog,<br>
	 * to display desired buttons.<br>
	 * 
	 * Valid values are JOptionPane constants as:
	 * <pre>
	 *  YES_NO_OPTION
	 *  YES_NO_CANCEL_OPTION
 	 *  OK_CANCEL_OPTION
 	 *  DEFAULT_OPTION
  	 *  </pre>
	 * @return the dialogTypeOptions
	 */
	public int getDialogTypeOptions() {
		return dialogTypeOptions;
	}

	/**
	 * Sets the type of option, as a JOptionPane constant, for the standard dialog,<br>
	 * to display desired buttons.<br>
	 * 
	 * Valid values are JOptionPane constants as:
	 * <pre>
	 *  YES_NO_OPTION
	 *  YES_NO_CANCEL_OPTION
 	 *  OK_CANCEL_OPTION
 	 *  DEFAULT_OPTION
  	 *  </pre>
	 * @param dialogTypeOptions the dialogTypeOption to set
	 */
	public void setDialogTypeOptions(int dialogTypeOptions) {
		this.dialogTypeOptions = dialogTypeOptions;
	}

	/**
	 * Gets the name assigned to the {@link JOptionPane} panel currently active.<br>
	 * <p>
	 * When a standard option Dialog (realized with a {@link JOptionPane} has to be executed<br>
	 * in modal mode, a {@link JDialog} modal panel will be created and the {@link JOptionPane} laid out.<br>
	 * The name of the option panel is assugned by forward monitor.<br>
	 * <p>
	 * @return the dialogOptionPanelName
	 */
	public String getDialogOptionPanelName() {
		return dialogOptionPanelName;
	}

	/**
	 * Sets the name assigned to the {@link JOptionPane} panel currently active.<br>
	 * <p>
	 * When a standard option Dialog (realized with a {@link JOptionPane} has to be executed<br>
	 * in modal mode, a {@link JDialog} modal panel will be created and the {@link JOptionPane} laid out.<br>
	 * The name of the option panel is assugned by forward monitor.<br>
	 * <p>
	 * @param dialogOptionPanelName the dialogOptionPanelName to set
	 */
	public void setDialogOptionPanelName(String dialogOptionPanelName) {
		this.dialogOptionPanelName = dialogOptionPanelName;
	}

	/**
	 * Gets if an icon will be displayed by dialog<br>
	 * <p>
	 * @return the isDialogModalWithIcon
	 */
	public boolean isDialogWithIcon() {
		return isDialogWithIcon;
	}

	/**
	 * Sets if an icon will be displayed by dialog<br>
	 * <p>
	 * @param isDialogWithIcon the isDialogWithIcon to set
	 */
	public void setDialogWithIcon(boolean isDialogWithIcon) {
		this.isDialogWithIcon = isDialogWithIcon;
	}

	/**
	 * Gets if the icon that will be displayed by dialog is user and not the standard icon<br>
	 * <p>
	 * @return the isDialogWithIconUser
	 */
	public boolean isDialogWithIconUser() {
		return isDialogWithIconUser;
	}

	/**
	 * Sets if the icon that will be displayed by dialog is user and not the standard icon<br>
	 * <p>
	 * @param isDialogModalWithIconUser the isDialogModalWithIconUser to set
	 */
	public void setDialogWithIconUser(boolean isDialogWithIconUser) {
		this.isDialogWithIconUser = isDialogWithIconUser;
	}

	/**
	 * Gets the user {@link Icon} to display.<br>
	 * <p>
	 * @return the dialogIcon
	 */
	public Icon getDialogIcon() {
		return dialogIcon;
	}

	/**
	 * Sets the user {@link Icon} to display.<br>
	 * <p>
	 * @param dialogIcon the dialogIcon to set
	 */
	public void setDialogIcon(Icon dialogIcon) {
		this.dialogIcon = dialogIcon;
	}

	/**
	 * Gets the path of icon to be displayed on dialog panel.<br>
	 * <p>
	 * @return the dialogIconPath
	 */
	public String getDialogIconPath() {
		return dialogIconPath;
	}

	/**
	 * Sets the path of icon to be displayed on dialog panel.<br>
	 * <p>
	 * @param dialogIconPath the dialogIconPath to set
	 */
	public void setDialogIconPath(String dialogIconPath) {
		this.dialogIconPath = dialogIconPath;
	}

	/**
	 * Gets all customized label string that will be set for buttons.<br>
	 * <p>
	 * @return the dialogLabelsCustom
	 */
	public String[] getDialogLabelsCustom() {
		return dialogLabelsCustom;
	}

	/**
	 * Sets all customized label string that will be set for buttons.<br>
	 * <p>
	 * @param dialogLabelsCustom the dialogLabelsCustom to set
	 */
	public void setDialogLabelsCustom(String[] dialogLabelsCustom) {
		this.dialogLabelsCustom = dialogLabelsCustom;
	}

	
	/**
	 * Gets the label custom that will be selected by default.<br>
	 * <pr>
	 * @return the dialogLabelCustomSelected
	 */
	public String getDialogLabelCustomSelected() {
		return dialogLabelCustomSelected;
	}

	/**
	 * Sets the label custom that will be selected by default.<br>
	 * <pr>
	 * @param dialogLabelCustomSelected the dialogLabelCustomSelected to set
	 */
	public void setDialogLabelCustomSelected(String dialogLabelCustomSelected) {
		this.dialogLabelCustomSelected = dialogLabelCustomSelected;
	}

	/**
	 * Gets the button pressed as a {@link JOptionPane}
	 * @return the dialogOptionChoosed constant.<br>
	 * <p>
	 * Valid values are:
	 * <pre>
	 *  YES_OPTION
	 *  NO_OPTION
	 *  CANCEL_OPTION
	 *  OK_OPTION
	 *  CLOSED_OPTION when no button has been pressed but the window has been closed
	 *  </pre>
	 */
	public int getDialogOptionChoosed() {
		return dialogOptionChoosed;
	}

	/**
	 * Sets the button pressed as a {@link JOptionPane}
	 * @return the dialogOptionChoosed constant.<br>
	 * <p>
	 * Valid values are:
	 * <pre>
	 *  YES_OPTION
	 *  NO_OPTION
	 *  CANCEL_OPTION
	 *  OK_OPTION
	 *  CLOSED_OPTION when no button has been pressed but the window has been closed
	 *  </pre>
	 * @param dialogOptionChoosed the dialogOptionChoosed to set
	 */
	public void setDialogOptionChoosed(int dialogOptionChoosed) {
		this.dialogOptionChoosed = dialogOptionChoosed;
	}

	
	
	/**
	 * Gets if the {@link JDialog} is an input dialog, requiring inputs.<br>
	 * <p>
	 * A true value makes the dialog showing a {@link JTextField} or a {@link JComboBox}<br>
	 * to let an input value to be eneterd.<br>
	 * <p>
	 * @see getDialogSelectionValues()
	 * @see getDialogInitialValue()
	 * @see getDialogInputValue()
	 * @return the dialogWantsInput
	 */
	public boolean isDialogWantsInput() {
		return dialogWantsInput;
	}

	/**
	 * Sets if the {@link JDialog} is an input dialog, requiring inputs.<br>
	 * <p>
	 * A true value makes the dialog showing a {@link JTextField} or a {@link JComboBox}<br>
	 * to let an input value to be eneterd.<br>
	 * <p>
	 * @see getDialogSelectionValues()
	 * @see getDialogInitialValue()
	 * @see getDialogInputValue()
	 * @param dialogWantsInput the dialogWantsInput to set
	 */
	public void setDialogWantsInput(boolean dialogWantsInput) {
		this.dialogWantsInput = dialogWantsInput;
	}

	/**
	 * Gets the values to select by a {@link JComboBox} in a input {@link JDialog}.<br>
	 * <p>
	 * @return the dialogSelectionValues
	 */
	public Object[] getDialogSelectionValues() {
		return dialogSelectionValues;
	}

	/**
	 * Sets the values to select by a {@link JComboBox} in a input {@link JDialog}.<br>
	 * <p>
	 * @param dialogSelectionValues the dialogSelectionValues to set
	 */
	public void setDialogSelectionValues(Object[] dialogSelectionValues) {
		this.dialogSelectionValues = dialogSelectionValues;
	}

	/**
	 * Gets the initial value in a input {@link JDialog}.<br>
	 * <p>
	 * For multiple values selected by a {@link JComboBox} it's a comboBox item.<br>
	 * For single values selected by a {@link JTextField} it's the initial text value.<br>
	 * <p>
	 * @return the dialogInitialSelectionValue
	 */
	public Object getDialogInitialSelectionValue() {
		return dialogInitialSelectionValue;
	}

	/**
	 * Sets the initial value in a input {@link JDialog}.<br>
	 * <p>
	 * For multiple values selected by a {@link JComboBox} it's a comboBox item.<br>
	 * For single values selected by a {@link JTextField} it's the initial text value.<br>
	 * <p>
	 * @param dialogInitialSelectionValue the dialogInitialSelectionValue to set
	 */
	public void setDialogInitialSelectionValue(Object dialogInitialSelectionValue) {
		this.dialogInitialSelectionValue = dialogInitialSelectionValue;
	}

	/**
	 * Gets the text entered by dialog window.<br>
	 * <p>
	 * The forward action origine should be START_DIALOG_INPUT_BY_TEXT if a {@link JTextField} 
	 * is used or START_DIALOG_INPUT_BY_TEXTS if a {@link JComboBox} has been used instead.<br>
	 * <p>
	 * @return the dialogInputValue
	 */
	public String getDialogInputValue() {
		return dialogInputValue;
	}

	/**
	 * Sets the text entered by dialog window.<br>
	 * <p>
	 * The forward action origine should be START_DIALOG_INPUT_BY_TEXT.<br>
	 * <p>
	 * @param dialogInputValue the dialogInputValue to set
	 */
	public void setDialogInputValue(String dialogInputValue) {
		this.dialogInputValue = dialogInputValue;
	}

	
	
	/**
	 * Gets the color selected by last standard chooser dialog.<br>
	 * <p>
	 * @return the chooserColorSelected
	 */
	public Color getChooserColorSelected() {
		return chooserColorSelected;
	}

	/**
	 * Sets the color selected by last standard chooser dialog.<br>
	 * <p>
	 * @param chooserColorSelected the chooserColorSelected to set
	 */
	public void setChooserColorSelected(Color chooserColorSelected) {
		this.chooserColorSelected = chooserColorSelected;
	}

	/**
	 * Gets the font selected by last standard chooser dialog.<br>
	 * <p>
	 * @return the chooserFontSelected
	 */
	public Font getChooserFontSelected() {
		return chooserFontSelected;
	}

	/**
	 * Sets the font selected by last standard chooser dialog.<br>
	 * <p>
	 * @param chooserFontSelected the chooserFontSelected to set
	 */
	public void setChooserFontSelected(Font chooserFontSelected) {
		this.chooserFontSelected = chooserFontSelected;
	}

	/**
	 * Gets the code of the forward table.<br>
	 * <p>
	 * @return the lookupTableCode
	 */
	public String getLookupTableCode() {
		return lookupTableCode;
	}

	/**
	 * Sets the code of the forward table.<br>
	 * <p>
	 * @param lookupTableCode the lookupTableCode to set
	 */
	public void setLookupTableCode(String lookupTableCode) {
		this.lookupTableCode = lookupTableCode;
	}

	
	
	
	/**
	 * Gets if the active fileChooser allows the multiple selection.<br>
	 * <p>
	 * @return the isFileChooserMultiSelection
	 */
	public boolean isFileChooserMultiSelectionEnabled() {
		if (this.fileChooserActive == null) {return false;}
		return this.isFileChooserMultiSelectionEnabled;
	}

	/**
	 * Gets if the active fileChooser allows the multiple selection.<br>
	 * <p>
	 * @param isFileChooserMultiSelectionEnabled the isFileChooserMultiSelectionEnabled to set
	 */
	public void setFileChooserMultiSelectionEnabled(boolean isFileChooserMultiSelectionEnabled) {
		if (this.fileChooserActive == null) {return;}
		this.fileChooserActive.setMultiSelectionEnabled(isFileChooserMultiSelectionEnabled);
		this.isFileChooserMultiSelectionEnabled = isFileChooserMultiSelectionEnabled;
	}

	/**
	 * Gets the active fileChooser as a JFileChooser object.<br>
	 * <p>
	 * @return the fileChooserActive
	 */
	public JFileChooser getFileChooserActive() {
		return fileChooserActive;
	}

	/**
	 * Sets the active fileChooser as a JFileChooser object.<br>
	 * <p>
	 * @param fileChooserActive the fileChooserActive to set
	 */
	public void setFileChooserActive(JFileChooser fileChooserActive) {
		this.fileChooserActive = fileChooserActive;
	}

	/**
	 * Gets name of the active fileChooser set by application.<br>
	 * <p>
	 * @return the fileChooserName
	 */
	public String getFileChooserName() {
		if (this.fileChooserActive == null) {return "";}
		return this.fileChooserActive.getName();
	}

	/**
	 * Sets name of the active fileChooser set by application.<br>
	 * <p>
	 * @param fileChooserName the fileChooserName to set
	 */
	public void setFileChooserName(String fileChooserName) {
		if (this.fileChooserActive == null) {return;}
		this.fileChooserActive.setName(fileChooserName);
	}

	/**
	 * Gets the fileChooser dialog title<br>
	 * <p>
	 * @return the fileChooserTitle
	 */
	public String getFileChooserTitle() {
		if (this.fileChooserActive == null) {return "";}
		return this.fileChooserActive.getDialogTitle();
	}

	/**
	 * Sets the fileChooser dialog title<br>
	 * <p>
	 * @param fileChooserTitle the fileChooserTitle to set
	 */
	public void setFileChooserTitle(String fileChooserTitle) {
		if (this.fileChooserActive == null) {return;}
		this.fileChooserActive.setDialogTitle(fileChooserTitle);
	}

	/**
	 * Gets the fileChooser file selection mode.<br>
	 * <p>
	 * Valid values are JFileChooser constants as JFileChooser.FILES_AND_DIRECTORIES, JFileChooser.DIRECTORIES_ONLY, ...<br>
	 * <p>
	 * @return the fileChooserFileSelectionMode
	 */
	public int getFileChooserFileSelectionMode() {
		if (this.fileChooserActive == null) {return 0;}
		return this.fileChooserActive.getFileSelectionMode();
	}

	/**
	 * Sets the fileChooser file selection mode.<br>
	 * <p>
	 * Valid values are JFileChooser constants as JFileChooser.FILES_AND_DIRECTORIES, JFileChooser.DIRECTORIES_ONLY, ...<br>
	 * <p>
	 * @param fileChooserFileSelectionMode the fileChooserFileSelectionMode to set
	 */
	public void setFileChooserFileSelectionMode(int fileChooserFileSelectionMode) {
		if (this.fileChooserActive == null) {return;}
		this.fileChooserActive.setFileSelectionMode(fileChooserFileSelectionMode);
	}

	/**
	 * Gets the fileChooser response.<br>
	 * <p>
	 * Valid values are JFileChooser constants as  JFileChooser.APPROVE_OPTION, CANCEL_OPTION, ERROR_OPTION<br>
	 * <p>
	 * @return the fileChooserResponse
	 */
	public int getFileChooserResponse() {
		return fileChooserResponse;
	}

	/**
	 * Sets the fileChooser response.<br>
	 * <p>
	 * Valid values are JFileChooser constants as  JFileChooser.APPROVE_OPTION, CANCEL_OPTION, ERROR_OPTION<br>
	 * <p>
	 * @param fileChooserResponse the fileChooserResponse to set
	 */
	public void setFileChooserResponse(int fileChooserResponse) {
		this.fileChooserResponse = fileChooserResponse;
	}

	/**
	 * Gets the fileChooser selected file, or the first selected file.<br>
	 * <p>
	 * @return the fileChooserSelectedFile
	 */
	public File getFileChooserSelectedFile() {
		if (this.fileChooserActive == null) {return null;}
		return this.fileChooserActive.getSelectedFile();
	}

	/**
	 * Sets the fileChooser selected file, or the first selected file.<br>
	 * <p>
	 * @param fileChooserSelectedFile the fileChooserSelectedFile to set
	 */
	public void setFileChooserSelectedFile(File fileChooserSelectedFile) {
		if (this.fileChooserActive == null) {return;}
		this.fileChooserActive.setSelectedFile(fileChooserSelectedFile);
	}

	/**
	 * Gets all fileChooser selected files, when multiple selection is allowed.<br>
	 * <p>
	 * @return the fileChooserSelectedFiles
	 */
	public File[] getFileChooserSelectedFiles() {
		if (this.fileChooserActive == null) {return null;}
		return this.fileChooserActive.getSelectedFiles();
	}

	/**
	 * Sets all fileChooser selected files, when multiple selection is allowed.<br>
	 * <p>
	 * @param fileChooserSelectedFiles the fileChooserSelectedFiles to set
	 */
	public void setFileChooserSelectedFiles(File[] fileChooserSelectedFiles) {
		if (this.fileChooserActive == null) {return;}
		this.fileChooserActive.setSelectedFiles(fileChooserSelectedFiles);
	}

	/**
	 * Gets the logical data view name.<br>
	 * <p>
	 * @return the ldvName
	 */
	public String getLdvName() {
		return ldvName;
	}

	/**
	 * Sets the logical data view name.<br>
	 * <p>
	 * @param ldvName the ldvName to set
	 */
	public void setLdvName(String ldvName) {
		this.ldvName = ldvName;
	}

	/**
	 * Gets the logical data view class.<br>
	 * <p>
	 * @return the ldvClass
	 */
	public Class<? extends ForwardLogicalDataView> getLdvClass() {
		return ldvClass;
	}

	/**
	 * Sets the logical data view class.<br>
	 * <p>
	 * @param classLogicalDataView the ldvClass to set
	 */
	public void setLdvClass(Class<? extends ForwardLogicalDataView> classLogicalDataView) {
		this.ldvClass = classLogicalDataView;
	}

	
	/**
	 * Sets the logical data view entity internal name reference<br>
	 * <p>
	 * @return the ldvEntityAs
	 */
	public String getLdvEntityAs() {
		return ldvEntityAs;
	}

	/**
	 * Gets the current logical data view entity internal name reference<br>
	 * <p>
	 * @param ldvEntityAs the ldvEntityAs to set
	 */
	public void setLdvEntityAs(String ldvEntityAs) {
		this.ldvEntityAs = ldvEntityAs;
	}

	/**
	 * Gets the logical data view object.<br>
	 * <p>
	 * @return the ldvObject
	 */
	public Object getLdvObject() {
		return ldvObject;
	}

	/**
	 * Gets the logical data view object.<br>
	 * <p>
	 * @param ldvObject the ldvObject to set
	 */
	public void setLdvObject(Object ldvObject) {
		this.ldvObject = ldvObject;
	}

	/**
	 * Gets the last logical data view object return code.<br>
	 * <p>
	 * That's an ordinal of {@link EnumForwardLogicalDataViewControl}.<br>
	 * <p>
	 * @return the ldvReturnCode
	 */
	public int getLdvReturnCode() {
		return ldvReturnCode;
	}

	/**
	 * Sets the last logical data view object return code.<br>
	 * <p>
	 * That's an ordinal of {@link EnumForwardLogicalDataViewControl}.<br>
	 * <p>
	 * @param ldvReturnCode the ldvReturnCode to set
	 */
	public void setLdvReturnCode(int ldvReturnCode) {
		this.ldvReturnCode = ldvReturnCode;
	}

	/**
	 * Gets the current logical data view row in the recordset.<br>
	 * <p>
	 * The value is 0-based.<br>
	 * <p>
	 * @return the ldvRow
	 */
	public int getLdvRow() {
		return ldvRow;
	}

	/**
	 * Sets the current logical data view row in the recordset.<br>
	 * <p>
	 * The value is 0-based.<br>
	 * <p>
	 * @param ldvRow the ldvRow to set
	 */
	public void setLdvRow(int ldvRow) {
		this.ldvRow = ldvRow;
	}

	/**
	 * Gets the current logical data view page.<br>
	 * <p>
	 * @return the ldvPage
	 */
	public int getLdvPage() {
		return ldvPage;
	}

	/**
	 * Sets the current logical data view page.<br>
	 * <p>
	 * @param ldvPage the ldvPage to set
	 */
	public void setLdvPage(int ldvPage) {
		this.ldvPage = ldvPage;
	}

	/**
	 * Gets the limit row property.<br>
	 * <p>
	 * That's the max number of rows that can be read from database.<br>
	 * <p>
	 * @return the ldvLimitRows
	 */
	public int getLdvLimitRows() {
		return ldvLimitRows;
	}

	/**
	 * Sets the limit row property.<br>
	 * <p>
	 * That's the max number of rows that can be read from database.<br>
	 * <p>
	 * @param ldvLimitRows the ldvLimitRows to set
	 */
	public void setLdvLimitRows(int ldvLimitRows) {
		this.ldvLimitRows = ldvLimitRows;
	}

	/**
	 * Gets the number of rows in a page.<br>
	 * <p>
	 * @return the ldvPageRows
	 */
	public int getLdvPageRows() {
		return ldvPageRows;
	}

	/**
	 * Sets the number of rows in a page.<br>
	 * <p>
	 * @param ldvPageRows the ldvPageRows to set
	 */
	public void setLdvPageRows(int ldvPageRows) {
		this.ldvPageRows = ldvPageRows;
	}


	/**
	 * Gets the last event object {@link ListSelectionEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventListSelection
	 */
	public ListSelectionEvent getEventListSelection() {
		return eventListSelection;
	}

	/**
	 * Gets the last event object {@link ListSelectionEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventListSelection the eventListSelection to set
	 */
	public void setEventListSelection(ListSelectionEvent eventListSelection) {
		this.eventListSelection = eventListSelection;
	}


	
	/**
	 * Gets the last event object {@link ActionEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventAction
	 */
	public ActionEvent getEventAction() {
		return eventAction;
	}

	/**
	 * Sets the last event object {@link ActionEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventAction the eventAction to set
	 */
	public void setEventAction(ActionEvent eventAction) {
		this.eventAction = eventAction;
	}

	/**
	 * Gets the last event object {@link AdjustmentEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventAdjustment
	 */
	public AdjustmentEvent getEventAdjustment() {
		return eventAdjustment;
	}

	/**
	 * Sets the last event object {@link AdjustmentEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventAdjustment the eventAdjustment to set
	 */
	public void setEventAdjustment(AdjustmentEvent eventAdjustment) {
		this.eventAdjustment = eventAdjustment;
	}

	/**
	 * Gets the last event object {@link FocusEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventFocus
	 */
	public FocusEvent getEventFocus() {
		return eventFocus;
	}

	/**
	 * Sets the last event object {@link FocusEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventFocus the eventFocus to set
	 */
	public void setEventFocus(FocusEvent eventFocus) {
		this.eventFocus = eventFocus;
	}

	/**
	 * Gets the last event object {@link ItemEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventItem
	 */
	public ItemEvent getEventItem() {
		return eventItem;
	}

	/**
	 * Sets the last event object {@link ItemEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventItem the eventItem to set
	 */
	public void setEventItem(ItemEvent eventItem) {
		this.eventItem = eventItem;
	}

	/**
	 * Gets the last event object {@link MouseEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventMouse
	 */
	public MouseEvent getEventMouse() {
		return eventMouse;
	}

	/**
	 * Sets the last event object {@link MouseEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventMouse the eventMouse to set
	 */
	public void setEventMouse(MouseEvent eventMouse) {
		this.eventMouse = eventMouse;
	}

	/**
	 * Gets the last event object {@link MouseEvent} for motion event to let manual management if it needs.<br>
	 * <p>
	 * @return the eventMouseMotion
	 */
	public MouseEvent getEventMouseMotion() {
		return eventMouseMotion;
	}

	/**
	 * Sets the last event object {@link MouseEvent} for motion event to let manual management if it needs.<br>
	 * <p>
	 * @param eventMotion the eventMotion to set
	 */
	public void setEventMouseMotion(MouseEvent eventMouseMotion) {
		this.eventMouseMotion = eventMouseMotion;
	}

	/**
	 * Gets the last event object {@link MouseWheelEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventMouseWheelMotion
	 */
	public MouseWheelEvent getEventMouseWheelMotion() {
		return eventMouseWheelMotion;
	}

	/**
	 * Sets the last event object {@link MouseWheelEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventMouseWheelMotion the eventMouseWheelMotion to set
	 */
	public void setEventMouseWheelMotion(MouseWheelEvent eventMouseWheelMotion) {
		this.eventMouseWheelMotion = eventMouseWheelMotion;
	}

	/**
	 * Gets the last event object {@link KeyEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventKey
	 */
	public KeyEvent getEventKey() {
		return eventKey;
	}

	/**
	 * Sets the last event object {@link KeyEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventKey the eventKey to set
	 */
	public void setEventKey(KeyEvent eventKey) {
		this.eventKey = eventKey;
	}

	/**
	 * Gets the last event object {@link ChangeEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventChange
	 */
	public ChangeEvent getEventChange() {
		return eventChange;
	}

	/**
	 * Sets the last event object {@link ChangeEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventChange the eventChange to set
	 */
	public void setEventChange(ChangeEvent eventChange) {
		this.eventChange = eventChange;
	}

	/**
	 * Gets the last event object {@link DocumentEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventDocument
	 */
	public DocumentEvent getEventDocument() {
		return eventDocument;
	}

	/**
	 * Sets the last event object {@link DocumentEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventDocument the eventDocument to set
	 */
	public void setEventDocument(DocumentEvent eventDocument) {
		this.eventDocument = eventDocument;
	}

	/**
	 * Gets the last event object {@link WindowEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventWindow
	 */
	public WindowEvent getEventWindow() {
		return eventWindow;
	}

	/**
	 * Sets the last event object {@link WindowEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventWindow the eventWindow to set
	 */
	public void setEventWindow(WindowEvent eventWindow) {
		this.eventWindow = eventWindow;
	}

	
	/**
	 * Gets the last event object {@link TreeExpansionEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventTreeExpansion
	 */
	public TreeExpansionEvent getEventTreeExpansion() {
		return eventTreeExpansion;
	}

	/**
	 * Sets the last event object {@link TreeExpansionEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventTreeExpansion the eventTreeExpansion to set
	 */
	public void setEventTreeExpansion(TreeExpansionEvent eventTreeExpansion) {
		this.eventTreeExpansion = eventTreeExpansion;
	}

	/**
	 * Gets the last event object {@link TreeSelectionEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventTreeSelection
	 */
	public TreeSelectionEvent getEventTreeSelection() {
		return eventTreeSelection;
	}

	/**
	 * Sets the last event object {@link TreeSelectionEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventTreeSelection the eventTreeSelection to set
	 */
	public void setEventTreeSelection(TreeSelectionEvent eventTreeSelection) {
		this.eventTreeSelection = eventTreeSelection;
	}

	
	/**
	 * Gets the last event object {@link TreeModelEvent} to let manual management if it needs.<br>
	 * <p>
	 * @return the eventTreeModelEvent
	 */
	public TreeModelEvent getEventTreeModelEvent() {
		return eventTreeModelEvent;
	}

	/**
	 * Sets the last event object {@link TreeModelEvent} to let manual management if it needs.<br>
	 * <p>
	 * @param eventTreeModelEvent the eventTreeModelEvent to set
	 */
	public void setEventTreeModelEvent(TreeModelEvent eventTreeModelEvent) {
		this.eventTreeModelEvent = eventTreeModelEvent;
	}

	
	
	/**
	 * Gets the child count of the current node selectd.<br>
	 * When mulltiple nodes selection is set, will be returned the count of the first selected node.
	 * <br>
	 * @return the eventTreeNodeChildCount
	 */
	public int getEventTreeNodeChildCount() {
		return eventTreeNodeChildCount;
	}

	/**
	 * Sets the child count of the current node selectd.<br>
	 * When mulltiple nodes selection is set, will be returned the count of the first selected node.
	 * <br>
	 * @param eventTreeNodeChildCount the eventTreeNodeChildCount to set
	 */
	public void setEventTreeNodeChildCount(int eventTreeNodeChildCount) {
		this.eventTreeNodeChildCount = eventTreeNodeChildCount;
	}

	
	/**
	 * Gets the count of all current selected nodes.<br>
	 * <br>
	 * @return the eventTreeNodesSelectedCount
	 */
	public int getEventTreeNodesSelectedCount() {
		return eventTreeNodesSelectedCount;
	}

	
	
	/**
	 * Sets the count of all current selected nodes.<br>
	 * <p>
	 * This method is intended for forward internal use only.<br>
	 * <p>
	 * @param eventTreeNodesSelectedCount the eventTreeNodesSelectedCount to set
	 */
	public void setEventTreeNodesSelectedCount(int eventTreeNodesSelectedCount) {
		this.eventTreeNodesSelectedCount = eventTreeNodesSelectedCount;
	}

	/**
	 * Gets an object array of user application objects of nodes currently selected.<br>
	 * <p>
	 * Application must do casting to proper object type.<br>
	 * <p>
	 * @return the eventTreeNodesSelectedObject
	 */
	public Object[] getEventTreeNodesSelectedObject() {
		return eventTreeNodesSelectedObject;
	}

	/**
	 * Sets an object array of user application objects of nodes currently selected.<br>
	 * <p>
	 * This method is intended for forward internal use only.<br>
	 * <p>
	 * @param eventTreeNodesSelectedObject the eventTreeNodesSelectedObject to set
	 */
	public void setEventTreeNodesSelectedObject(Object[] eventTreeNodesSelectedObject) {
		this.eventTreeNodesSelectedObject = eventTreeNodesSelectedObject;
	}

	
	
	/**
	 * Gets the user node parent object of current active node<br>
	 * of the {@link JTree}, for the event occurred.<br>
	 * Application must do casting to the correct object type.<br>
	 * <p>
	 * @return the eventTreeNodeParentObject
	 */
	public Object getEventTreeNodeParentObject() {
		return eventTreeNodeParentObject;
	}

	/**
	 * Sets the user node parent object of current active node<br>
	 * of the {@link JTree}, for the event occurred.<br>
	 * <p>
	 * This method is intended for forward internal use only.<br>
	 * <p>
	 * @param eventTreeNodeParentObject the eventTreeNodeParentObject to set
	 */
	public void setEventTreeNodeParentObject(Object eventTreeNodeParentObject) {
		this.eventTreeNodeParentObject = eventTreeNodeParentObject;
	}

	/**
	 * Gets all user object parent of current node active<br>
	 * of the {@link JTree}, for the event occurred.<br>
	 * That's the nodes object list starting with the root node to the parent of the current node.
	 * <p>
	 * @return the eventTreeNodesParentObject
	 */
	public Object[] getEventTreeNodesParentObject() {
		return eventTreeNodesParentObject;
	}

	/**
	 * Sets all user object parent of current node active<br>
	 * of the {@link JTree}, for the event occurred.<br>
	 * That's the nodes object list starting with the root node to the parent of the current node.
	 * <p>
	 * This method is intended for forward internal use only.<br>
	 * <p>
	 * @param eventTreeNodesParentObject the eventTreeNodesParentObject to set
	 */
	public void setEventTreeNodesParentObject(Object[] eventTreeNodesParentObject) {
		this.eventTreeNodesParentObject = eventTreeNodesParentObject;
	}

	/**
	 * Gets child nodes of the currently expanded node.<br>
	 * It will be returned the first child level with both leaf and not nodes.<br>
	 * <p>
	 * @return the eventTreeNodesChild
	 */
	public DefaultMutableTreeNode[] getEventTreeNodesChild() {
		return eventTreeNodesChild;
	}

	/**
	 * Sets child nodes of the currently expanded node.<br>
	 * It will be returned the first child level with both leaf and not nodes.<br>
	 * <p>
	 * This method is intended forn forward internal use only.<br>
	 * <p>
	 * @param eventTreeNodesChild the eventTreeNodesChild to set
	 */
	public void setEventTreeNodesChild(DefaultMutableTreeNode[] eventTreeNodesChild) {
		this.eventTreeNodesChild = eventTreeNodesChild;
	}

	/**
	 * Gets user application objects of child nodes of the currently expanded node.<br>
	 * It will be returned the first child level with both leaf and not nodes.<br>
	 * @return the eventTreeNodesChildObject
	 */
	public Object[] getEventTreeNodesChildObject() {
		return eventTreeNodesChildObject;
	}

	/**
	 * Gets user application objects of child nodes of the currently expanded node.<br>
	 * It will be returned the first child level with both leaf and not nodes.<br>
	 * <p>
	 * This method is intended forn forward internal use only.<br>
	 * <p>
	 * @param eventTreeNodesChildObject the eventTreeNodesChildObject to set
	 */
	public void setEventTreeNodesChildObject(Object[] eventTreeNodesChildObject) {
		this.eventTreeNodesChildObject = eventTreeNodesChildObject;
	}

	/**
	 * Gets if the current node selected is leaf.<br>
	 * When mulltiple nodes selection is set, will be returned the status of the first selected node.
	 * @return the isEventTreeNodeLeaf
	 */
	public boolean isEventTreeNodeLeaf() {
		return isEventTreeNodeLeaf;
	}

	/**
	 * Sets if the current node selected is leaf.<br>
	 * This method is intended for forward internal use only.<br>
	 * <p>
	 * @param isEventTreeNodeLeaf the isEventTreeNodeLeaf to set
	 */
	public void setEventTreeNodeLeaf(boolean isEventTreeNodeLeaf) {
		this.isEventTreeNodeLeaf = isEventTreeNodeLeaf;
	}

	/**
	 * Gets the component type for the last event detected.<br>
	 * For example the last componend clicked or selected and so on.<br>
	 * <p>
	 * @return the eventComponentType
	 */
	public EnumForwardComponent getEventComponentType() {
		return eventComponentType;
	}

	/**
	 * Sets the component type for the last event detected.<br>
	 * For example the last componend clicked or selected and so on.<br>
	 * <p>
	 * @param eventComponentType the eventComponentType to set
	 */
	public void setEventComponentType(EnumForwardComponent eventComponentType) {
		this.eventComponentType = eventComponentType;
	}

	/**
	 * Gets the component, as a swing object, for the last event detected.<br>
	 * For example the last componend clicked or selected and so on.<br>
	 * <p>
	 * @return the eventComponentObject
	 */
	public Object getEventComponentObject() {
		return eventComponentObject;
	}

	/**
	 * Sets the component, as a swing object, for the last event detected.<br>
	 * For example the last componend clicked or selected and so on.<br>
	 * <p>
	 * @param eventComponentObject the eventComponentObject to set
	 */
	public void setEventComponentObject(Object eventComponentObject) {
		this.eventComponentObject = eventComponentObject;
	}

	
	
	/**
	 * Gets the component name of the swing object, source of the last event detected.<br>
	 * <p>
	 * @return the eventComponentName
	 */
	public String getEventComponentName() {
		return eventComponentName;
	}

	/**
	 * Sets the component name of the swing object, source of the last event detected.<br>
	 * <p>
	 * @param eventComponentName the eventComponentName to set
	 */
	public void setEventComponentName(String eventComponentName) {
		this.eventComponentName = eventComponentName;
	}

	
	
	/**
	 * Gets the count of click detected on Click by mouse event.<br>
	 * <br>
	 * @return the eventClickCount
	 */
	public int getEventClickCount() {
		return eventClickCount;
	}

	/**
	 * Sets the count of click detected on Click by mouse event.<br>
	 * <br>
	 * @param eventClickCount the eventClickCount to set
	 */
	public void setEventClickCount(int eventClickCount) {
		this.eventClickCount = eventClickCount;
	}

	/**
	 * Gets the point of coordinates of the component on Click by mouse event.<br>
	 * <br>
	 * @return the eventPoint
	 */
	public Point getEventPoint() {
		return eventPoint;
	}

	/**
	 * Sets the point of coordinates of the component on Click by mouse event.<br>
	 * <br>
	 * @param eventPoint the eventPoint to set
	 */
	public void setEventPoint(Point eventPoint) {
		this.eventPoint = eventPoint;
	}

	/**
	 * Gets the X coordinate of the component on Click by mouse event.<br>
	 * <br>
	 * @return the eventX
	 */
	public int getEventX() {
		return eventX;
	}

	/**
	 * Sets the X coordinate of the component on Click by mouse event.<br>
	 * <br>
	 * @param eventX the eventX to set
	 */
	public void setEventX(int eventX) {
		this.eventX = eventX;
	}

	/**
	 * Gets the Y coordinate of the component on Click by mouse event.<br>
	 * <br>
	 * @return the eventY
	 */
	public int getEventY() {
		return eventY;
	}

	/**
	 * Sets the Y coordinate of the component on Click by mouse event.<br>
	 * <br>
	 * @param eventY the eventY to set
	 */
	public void setEventY(int eventY) {
		this.eventY = eventY;
	}

	
	/**
	 * Gets the number of rotations of the mouse wheel.<br>
	 * <p>
	 * The number is equivalent to Clicks.<br>
	 * Sign indicates the direction.<br>
	 * <p>
	 * @return the eventWheelRotations
	 */
	public int getEventWheelRotations() {
		return eventWheelRotations;
	}

	/**
	 * Sets the number of rotations of the mouse wheel.<br>
	 * <p>
	 * The number is equivalent to Clicks.<br>
	 * Sign indicates the direction.<br>
	 * <p>
	 * @param eventWheelRotations the eventWheelRotations to set
	 */
	public void setEventWheelRotations(int eventWheelRotations) {
		this.eventWheelRotations = eventWheelRotations;
	}

	/**
	 * Gets the adjustment type for {@link JScrollBar} object.<br>
	 * Values are descripted on {@link Adjustment} class.<br>
	 * <p> 
	 * @return the eventAdjustmentType
	 */
	public int getEventAdjustmentType() {
		return eventAdjustmentType;
	}

	/**
	 * Sets the adjustment type for {@link JScrollBar} object.<br>
	 * Values are descripted on {@link Adjustment} class.<br>
	 * <p> 
	 * @param eventAdjustmentType the eventAdjustmentType to set
	 */
	public void setEventAdjustmentType(int eventAdjustmentType) {
		this.eventAdjustmentType = eventAdjustmentType;
	}

	/**
	 * Gets the value for {@link JScrollBar} object.<br>
	 * <p> 
	 * @return the eventAdjustmentValue
	 */
	public int getEventAdjustmentValue() {
		return eventAdjustmentValue;
	}

	/**
	 * Sets the value for {@link JScrollBar} object.<br>
	 * <p> 
	 * @param eventAdjustmentValue the eventAdjustmentValue to set
	 */
	public void setEventAdjustmentValue(int eventAdjustmentValue) {
		this.eventAdjustmentValue = eventAdjustmentValue;
	}

	
	
	/**
	 * Gets the value by ChangeListener for a JSlider swing conbtrol.<br>
	 * <p>
	 * @return the eventSliderChangedValue
	 */
	public int getEventSliderChangedValue() {
		return eventSliderChangedValue;
	}

	/**
	 * Sets the value by ChangeListener for a JSlider swing control.<br>
	 * <p>
	 * @param eventSliderChangedValue the eventSliderChangedValue to set
	 */
	public void setEventSliderChangedValue(int eventSliderChangedValue) {
		this.eventSliderChangedValue = eventSliderChangedValue;
	}

	/**
	 * Gets the value by ChangeListener for a text JSpinner swing control.<br>
	 * <p>
	 * @return the eventSpinnerChangedValueText
	 */
	public String getEventSpinnerChangedValueText() {
		return eventSpinnerChangedValueText;
	}

	/**
	 * Sets the value by ChangeListener for a text JSpinner swing control.<br>
	 * <p>
	 * @param eventSpinnerChangedValueText the eventSpinnerChangedValueText to set
	 */
	public void setEventSpinnerChangedValueText(String eventSpinnerChangedValueText) {
		this.eventSpinnerChangedValueText = eventSpinnerChangedValueText;
	}

	/**
	 * Gets the value by ChangeListener for a numeric JSpinner swing control.<br>
	 * <p>
	 * @return the eventSpinnerChangedValueNum
	 */
	public int getEventSpinnerChangedValueNum() {
		return eventSpinnerChangedValueNum;
	}

	/**
	 * Sets the value by ChangeListener for a numeric JSpinner swing control.<br>
	 * <p>
	 * @param eventSpinnerChangedValueNum the eventSpinnerChangedValueNum to set
	 */
	public void setEventSpinnerChangedValueNum(int eventSpinnerChangedValueNum) {
		this.eventSpinnerChangedValueNum = eventSpinnerChangedValueNum;
	}


	/**
	 * Sets the value by ChangeListener for a date JSpinner swing control.<br>
	 * <p>
	 * @param eventSpinnerChangedValueDate the eventSpinnerChangedValueDate to set
	 */
	public void setEventSpinnerChangedValueDate(Date eventSpinnerChangedValueDate) {
		this.eventSpinnerChangedValueDate = eventSpinnerChangedValueDate;
	}

	/**
	 * Gets the value by ChangeListener for a date JSpinner swing control.<br>
	 * <p>
	 * @return the eventSpinnerChangedValueDate
	 */
	public Date getEventSpinnerChangedValueDate() {
		return eventSpinnerChangedValueDate;
	}

	/**
	 * Gets the number of characters iserted, deleted or replaced in<br>
	 * in the document (may be simply a text field).<br>
	 * <p>
	 * @return the eventDocumentCharLenght
	 */
	public int getEventDocumentCharLenght() {
		return eventDocumentCharLenght;
	}

	/**
	 * Sets the number of characters iserted, deleted or replaced in<br>
	 * in the document (may be simply a text field).<br>
	 * <p>
	 * @param eventDocumentCharLenght the eventDocumentCharLenght to set
	 */
	public void setEventDocumentCharLenght(int eventDocumentCharLenght) {
		this.eventDocumentCharLenght = eventDocumentCharLenght;
	}

	/**
	 * Gets the offset of characters inserted, deleted or replaced<br>
	 * in the document (may be simply a text field).<br>
	 * <p>
	 * @return the eventDocumentCharOffset
	 */
	public int getEventDocumentCharOffset() {
		return eventDocumentCharOffset;
	}

	/**
	 * Sets the offset of characters inserted, deleted or replaced<br>
	 * in the document (may be simply a text field).<br>
	 * <p>
	 * @param eventDocumentCharOffset the eventDocumentCharOffset to set
	 */
	public void setEventDocumentCharOffset(int eventDocumentCharOffset) {
		this.eventDocumentCharOffset = eventDocumentCharOffset;
	}

	/**
	 * Gets the numer of characters in the document (may be simply a text field).<br>
	 * <p>
	 * @return the eventDocumentLength
	 */
	public int getEventDocumentLength() {
		return eventDocumentLength;
	}

	/**
	 * Sets the numer of characters in the document (may be simply a text field).<br>
	 * <p>
	 * @param eventDocumentLength the eventDocumentLength to set
	 */
	public void setEventDocumentLength(int eventDocumentLength) {
		this.eventDocumentLength = eventDocumentLength;
	}

	/**
	 * Gets the offset of cgharacters iserted, deleted or replaced in the document<br>
	 * (may be simply a text field).<br>
	 * <p>
	 * @return the eventDocumentStartOffset
	 */
	public int getEventDocumentStartOffset() {
		return eventDocumentStartOffset;
	}

	/**
	 * Sets the offset of cgharacters iserted, deleted or replaced in the document<br>
	 * (may be simply a text field).<br>
	 * <p>
	 * @param eventDocumentStartOffset the eventDocumentStartOffset to set
	 */
	public void setEventDocumentStartOffset(int eventDocumentStartOffset) {
		this.eventDocumentStartOffset = eventDocumentStartOffset;
	}

	/**
	 * Gets the offset end in the document (may be simply a text field).<br>
	 * <p>
	 * @return the eventDocumentEndOffset
	 */
	public int getEventDocumentEndOffset() {
		return eventDocumentEndOffset;
	}

	/**
	 * Sets the offset end in the document (may be simply a text field).<br>
	 * <p>
	 * @param eventDocumentEndOffset the eventDocumentEndOffset to set
	 */
	public void setEventDocumentEndOffset(int eventDocumentEndOffset) {
		this.eventDocumentEndOffset = eventDocumentEndOffset;
	}

	/**
	 * Gets the text inserted, deleted or updated in the document.<br>
	 * It's used documentCharOffset() and documentCharLength() to get the text<br>
	 * from the document (may be simply a text field).<br>
	 * <p>
	 * @return the eventDocumentCharText
	 */
	public String getEventDocumentCharText() {
		return eventDocumentCharText;
	}

	/**
	 * Sets the text inserted, deleted or updated in the document.<br>
	 * It's used documentCharOffset() and documentCharLength() to get the text<br>
	 * from the document (may be simply a text field).<br>
	 * <p>
	 * @param eventDocumentCharText the eventDocumentCharText to set
	 */
	public void setEventDocumentCharText(String eventDocumentCharText) {
		this.eventDocumentCharText = eventDocumentCharText;
	}

	/**
	 * Gets the text of the whole document (may be simply a text field). <br>
	 * <p>
	 * @return the eventDocumentText
	 */
	public String getEventDocumentText() {
		return eventDocumentText;
	}

	/**
	 * Sets the text of the whole document (may be simply a text field). <br>
	 * <p>
	 * @param eventDocumentText the eventDocumentText to set
	 */
	public void setEventDocumentText(String eventDocumentText) {
		this.eventDocumentText = eventDocumentText;
	}

	
	
	/**
	 * Gets the keyboard numeric code.<br>
	 * <p>
	 * @return the eventKeyCode
	 */
	public int getEventKeyCode() {
		return eventKeyCode;
	}

	/**
	 * Sets the keyboard numeric code.<br>
	 * <p>
	 * @param eventKeyCode the eventKeyCode to set
	 */
	public void setEventKeyCode(int eventKeyCode) {
		this.eventKeyCode = eventKeyCode;
	}

	/**
	 * Gets the keyboard character typed on keyboard.<br>
	 * <p>
	 * @return the eventKeyChar
	 */
	public char getEventKeyChar() {
		return eventKeyChar;
	}

	/**
	 * Gets the keyboard character typed on keyboard.<br>
	 * <p>
	 * @param eventKeyChar the eventKeyChar to set
	 */
	public void setEventKeyChar(char eventKeyChar) {
		this.eventKeyChar = eventKeyChar;
	}

	/**
	 * Gets the localized text for the keyboard key, such as "HOME", "F1" or "A".<br>
	 * <p>
	 * @return the eventKeyText
	 */
	public String getEventKeyText() {
		return eventKeyText;
	}

	/**
	 * Sets the localized text for the keyboard key, such as "HOME", "F1" or "A".<br>
	 * @param eventKeyText the eventKeyText to set
	 */
	public void setEventKeyText(String eventKeyText) {
		this.eventKeyText = eventKeyText;
	}

	/**
	 * Gets if the key is an action key like F1, Enter and so on.<br>
	 * <p>
	 * @return the eventKeyIsActionKey
	 */
	public boolean isEventKeyIsActionKey() {
		return eventKeyIsActionKey;
	}

	/**
	 * Sets if the key is an action key like F1, Enter and so on.<br>
	 * <p>
	 * @param eventKeyIsActionKey the eventKeyIsActionKey to set
	 */
	public void setEventKeyIsActionKey(boolean eventKeyIsActionKey) {
		this.eventKeyIsActionKey = eventKeyIsActionKey;
	}

	
	
	/**
	 * Gets the key modifiers mask to detect SHIFT CTRL and so on<br>
	 * with the key presssed or typed.<br>
	 * <p>
	 * Standard java documentation follows:
	 * <pre>
	 * Returns the extended modifier mask for this event. Extended modifiers represent the state of all modal keys, such as ALT, CTRL, META, and the mouse buttons just after the event occurred 

For example, if the user presses button 1 followed by button 2, and then releases them in the same order, the following sequence of events is generated: 

    MOUSE_PRESSED:  BUTTON1_DOWN_MASK
    MOUSE_PRESSED:  BUTTON1_DOWN_MASK | BUTTON2_DOWN_MASK
    MOUSE_RELEASED: BUTTON2_DOWN_MASK
    MOUSE_CLICKED:  BUTTON2_DOWN_MASK
    MOUSE_RELEASED:
    MOUSE_CLICKED:
 
It is not recommended to compare the return value of this method using == because new modifiers can be added in the future. For example, the appropriate way to check that SHIFT and BUTTON1 are down, but CTRL is up is demonstrated by the following code: 

    int onmask = SHIFT_DOWN_MASK | BUTTON1_DOWN_MASK;
    int offmask = CTRL_DOWN_MASK;
    if ((event.getModifiersEx() & (onmask | offmask)) == onmask) {
        ...
    }
 
The above code will work even if new modifiers are added.
     </pre>
	 * @return the eventKeyModifiersMask
	 */
	public int getEventKeyModifiersMask() {
		return eventKeyModifiersMask;
	}

	/**
	 * Sets the key modifiers mask to detect SHIFT CTRL and so on<br>
	 * with the key presssed or typed.<br>
	 * <p>
	 * Standard java documentation follows:
	 * <pre>
	 * Returns the extended modifier mask for this event. Extended modifiers represent the state of all modal keys, such as ALT, CTRL, META, and the mouse buttons just after the event occurred 

For example, if the user presses button 1 followed by button 2, and then releases them in the same order, the following sequence of events is generated: 

    MOUSE_PRESSED:  BUTTON1_DOWN_MASK
    MOUSE_PRESSED:  BUTTON1_DOWN_MASK | BUTTON2_DOWN_MASK
    MOUSE_RELEASED: BUTTON2_DOWN_MASK
    MOUSE_CLICKED:  BUTTON2_DOWN_MASK
    MOUSE_RELEASED:
    MOUSE_CLICKED:
 
It is not recommended to compare the return value of this method using == because new modifiers can be added in the future. For example, the appropriate way to check that SHIFT and BUTTON1 are down, but CTRL is up is demonstrated by the following code: 

    int onmask = SHIFT_DOWN_MASK | BUTTON1_DOWN_MASK;
    int offmask = CTRL_DOWN_MASK;
    if ((event.getModifiersEx() & (onmask | offmask)) == onmask) {
        ...
    }
 
The above code will work even if new modifiers are added.
     </pre>
	 * @param eventKeyModifiersMask the eventKeyModifiersMask to set
	 */
	public void setEventKeyModifiersMask(int eventKeyModifiersMask) {
		this.eventKeyModifiersMask = eventKeyModifiersMask;
	}

	
	/**
	 * Gets a text for the key modifiers mask to detect for example Ctrl+Shift and so on<br>
	 * with the key presssed or typed.<br>
	 * <p>
	 * @return the eventKeyModifiersText
	 */
	public int getEventKeyModifiersText() {
		return eventKeyModifiersText;
	}

	
	
	/**
	 * Gets if is pressed the SHIFT key together other keys.<br>
	 * <p>
	 * @return the isEventKeyShift
	 */
	public boolean isEventKeyShift() {
		return isEventKeyShift;
	}

	/**
	 * Sets if is pressed the SHIFT key together other keys.<br>
	 * <p>
	 * @param isEventKeyShift the isEventKeyShift to set
	 */
	public void setEventKeyShift(boolean isEventKeyShift) {
		this.isEventKeyShift = isEventKeyShift;
	}

	/**
	 * Gets if is pressed the CTRL key together other keys.<br>
	 * <p>
	 * @return the isEventKeyCtrl
	 */
	public boolean isEventKeyCtrl() {
		return isEventKeyCtrl;
	}

	/**
	 * Sets if is pressed the CTRL key together other keys.<br>
	 * <p>
	 * @param isEventKeyCtrl the isEventKeyCtrl to set
	 */
	public void setEventKeyCtrl(boolean isEventKeyCtrl) {
		this.isEventKeyCtrl = isEventKeyCtrl;
	}

	/**
	 * Gets if is pressed the ALT key together other keys.<br>
	 * <p>
	 * @return the isEventKeyAlt
	 */
	public boolean isEventKeyAlt() {
		return isEventKeyAlt;
	}

	/**
	 * Sets if is pressed the ALT key together other keys.<br>
	 * <p>
	 * @param isEventKeyAlt the isEventKeyAlt to set
	 */
	public void setEventKeyAlt(boolean isEventKeyAlt) {
		this.isEventKeyAlt = isEventKeyAlt;
	}

	/**
	 * Gets if is pressed the ALT_GRAPH key together other keys.<br>
	 * <p>
	 * @return the isEventKeyAltGraph
	 */
	public boolean isEventKeyAltGraph() {
		return isEventKeyAltGraph;
	}

	/**
	 * Sets if is pressed the ALT_GRAPH key together other keys.<br>
	 * <p>
	 * @param isEventKeyAltGraph the isEventKeyAltGraph to set
	 */
	public void setEventKeyAltGraph(boolean isEventKeyAltGraph) {
		this.isEventKeyAltGraph = isEventKeyAltGraph;
	}

	/**
	 * Sets a text for the key modifiers mask to detect for example Ctrl+Shift and so on<br>
	 * with the key presssed or typed.<br>
	 * <p>
	 * @param eventKeyModifiersText the eventKeyModifiersText to set
	 */
	public void setEventKeyModifiersText(int eventKeyModifiersText) {
		this.eventKeyModifiersText = eventKeyModifiersText;
	}

	/**
	 * Gets a text for the location of the key in the keyboard button.<br>
	 * Mapped by  KeyEvent.KEY_LOCATION_STANDARD, .... constants.<br>
	 * <p>
	 * @return the eventKeyLocationString
	 */
	public String getEventKeyLocationString() {
		return eventKeyLocationString;
	}

	/**
	 * Sets a text for the location of the key in the keyboard button.<br>
	 * Mapped by  KeyEvent.KEY_LOCATION_STANDARD, .... constants.<br>
	 * <p>
	 * @param eventKeyLocationString the eventKeyLocationString to set
	 */
	public void setEventKeyLocationString(String eventKeyLocationString) {
		this.eventKeyLocationString = eventKeyLocationString;
	}

	/**
	 * Gets the location of the key in the keyboard button.<br>
	 * Mapped by  KeyEvent.KEY_LOCATION_STANDARD, .... constants.<br>
	 * <p>
	 * @return the eventKeyLocation
	 */
	public int getEventKeyLocation() {
		return eventKeyLocation;
	}

	/**
	 * Sets the location of the key in the keyboard button.<br>
	 * Mapped by  KeyEvent.KEY_LOCATION_STANDARD, .... constants.<br>
	 * <p>
	 * @param eventKeyLocation the eventKeyLocation to set
	 */
	public void setEventKeyLocation(int eventKeyLocation) {
		this.eventKeyLocation = eventKeyLocation;
	}


	/**
	 * Gets the name of the panel selected by ticking a tabbed pane.<br>
	 * <p>
	 * @return the tabbedPaneTicked
	 */
	public String getTabbedPaneTicked() {
		return tabbedPaneTicked;
	}

	/**
	 * Sets the name of the panel selected by ticking a tabbed pane.<br>
	 * <p>
	 * @param tabbedPaneTicked the tabbedPaneTicked to set
	 */
	public void setTabbedPaneTicked(String tabbedPaneTicked) {
		this.tabbedPaneTicked = tabbedPaneTicked;
	}

	/**
	 * Gets the bound object of the current table row.<br>
	 * <p>
	 * @return the tableBoundObject
	 */
	public Object getTableBoundObject() {
		return tableBoundObject;
	}

	/**
	 * Sets the bound object of the current table row.<br>
	 * <p>
	 * @param tableBoundObject the tableBoundObject to set
	 */
	public void setTableBoundObject(Object tableBoundObject) {
		this.tableBoundObject = tableBoundObject;
	}

	/**
	 * Gets the row selected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the tableSelectedRow
	 */
	public int getTableSelectedRow() {
		return tableSelectedRow;
	}

	/**
	 * Sets the row selected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param tableSelectedRow the tableSelectedRow to set
	 */
	public void setTableSelectedRow(int tableSelectedRow) {
		this.tableSelectedRow = tableSelectedRow;
	}

	/**
	 * Gets the count of selected rows in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the tableSelectedRowCount
	 */
	public int getTableSelectedRowCount() {
		return tableSelectedRowCount;
	}

	/**
	 * Sets the count of selected rows in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param tableSelectedRowCount the tableSelectedRowCount to set
	 */
	public void setTableSelectedRowCount(int tableSelectedRowCount) {
		this.tableSelectedRowCount = tableSelectedRowCount;
	}

	/**
	 * Gets rows selected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the tableSelectedRows
	 */
	public int[] getTableSelectedRows() {
		return tableSelectedRows;
	}

	/**
	 * Sets rows selected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param tableSelectedRows the tableSelectedRows to set
	 */
	public void setTableSelectedRows(int[] tableSelectedRows) {
		this.tableSelectedRows = tableSelectedRows;
	}

	
	
	
	/**
	 * Gets rows unselected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the tableUnselectedRows
	 */
	public int[] getTableUnselectedRows() {
		return tableUnselectedRows;
	}

	/**
	 * Sets rows unselected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param tableUnselectedRows the tableUnselectedRows to set
	 */
	public void setTableUnselectedRows(int[] tableUnselectedRows) {
		this.tableUnselectedRows = tableUnselectedRows;
	}

	/**
	 * Gets columns unselected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the tableUnselectedColumns
	 */
	public int[] getTableUnselectedColumns() {
		return tableUnselectedColumns;
	}

	/**
	 * Sets columns unselected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param tableUnselectedColumns the tableUnselectedColumns to set
	 */
	public void setTableUnselectedColumns(int[] tableUnselectedColumns) {
		this.tableUnselectedColumns = tableUnselectedColumns;
	}

	/**
	 * Gets columns selected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the tableSelectedColumns
	 */
	public int[] getTableSelectedColumns() {
		return tableSelectedColumns;
	}

	/**
	 * Sets columns selected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param tableSelectedColumns the tableSelectedColumns to set
	 */
	public void setTableSelectedColumns(int[] tableSelectedColumns) {
		this.tableSelectedColumns = tableSelectedColumns;
	}

	/**
	 * Gets the table selection mode for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * The value can be:
	 * <pre>
	 * ListSelectionModel.MULTIPLE_INTERVAL_SELECTION
	 * ListSelectionModel.SINGLE_INTERVAL_SELECTION
	 * </pre>
	 * @return the tableSelectionMode
	 */
	public int getTableSelectionMode() {
		return tableSelectionMode;
	}

	/**
	 * set the table selection mode for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * The value can be:
	 * <pre>
	 * ListSelectionModel.MULTIPLE_INTERVAL_SELECTION
	 * ListSelectionModel.SINGLE_INTERVAL_SELECTION
	 * </pre>
	 * @param tableSelectionMode the tableSelectionMode to set
	 */
	public void setTableSelectionMode(int tableSelectionMode) {
		this.tableSelectionMode = tableSelectionMode;
	}

	/**
	 * Gets if it has been selected one or more rows for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the isTableRowsSelected
	 */
	public boolean isTableRowsSelected() {
		return isTableRowsSelected;
	}

	/**
	 * Sets if it has been selected one or more rows for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param isTableRowsSelected the isTableRowsSelected to set
	 */
	public void setTableRowsSelected(boolean isTableRowsSelected) {
		this.isTableRowsSelected = isTableRowsSelected;
	}

	/**
	 * Gets if it has been selected one or more columns for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the isTableColumnsSelected
	 */
	public boolean isTableColumnsSelected() {
		return isTableColumnsSelected;
	}

	/**
	 * Sets if it has been selected one or more columns for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param isTableColumnsSelected the isTableColumnsSelected to set
	 */
	public void setTableColumnsSelected(boolean isTableColumnsSelected) {
		this.isTableColumnsSelected = isTableColumnsSelected;
	}

	/**
	 * Gets if the selection of rows is allowed, for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the isTableRowSelectionAllowed
	 */
	public boolean isTableRowSelectionAllowed() {
		return isTableRowSelectionAllowed;
	}

	/**
	 * Sets if the selection of rows is allowed, for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param isTableRowSelectionAllowed the isTableRowSelectionAllowed to set
	 */
	public void setTableRowSelectionAllowed(boolean isTableRowSelectionAllowed) {
		this.isTableRowSelectionAllowed = isTableRowSelectionAllowed;
	}

	/**
	 * Gets if the selection of columns is allowed, for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the isTableColumnSelectionAllowed
	 */
	public boolean isTableColumnSelectionAllowed() {
		return isTableColumnSelectionAllowed;
	}

	/**
	 * Sets if the selection of columns is allowed, for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param isTableColumnSelectionAllowed the isTableColumnSelectionAllowed to set
	 */
	public void setTableColumnSelectionAllowed(boolean isTableColumnSelectionAllowed) {
		this.isTableColumnSelectionAllowed = isTableColumnSelectionAllowed;
	}

	/**
	 * Gets if the selection of cells is allowed, for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the isTableCellSelectionAllowed
	 */
	public boolean isTableCellSelectionAllowed() {
		return isTableCellSelectionAllowed;
	}

	
	/**
	 * Returns true or false depending on input row numbers are currently selected,
	 * for the {@link JTable}, in the last event detected. <br>
	 * <p>
	 * @return if rows selected
	 */
	public boolean isTableSelectedRows(int ar_row[]) {
		return Arrays.asList(ar_row).containsAll(Arrays.asList(this.tableSelectedRows));
	}

	/**
	 * Returns true or false depending on input column numbers are currently selected,
	 * for the {@link JTable}, in the last event detected. <br>
	 * <p>
	 * @return if rows selected
	 */
	public boolean isTableSelectedColumns(int ar_column[]) {
		return Arrays.asList(ar_column).containsAll(Arrays.asList(this.tableSelectedColumns));
	}

	
	
	/**
	 * Sets if the selection of cells is allowed, for the {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param isTableCellSelectionAllowed the isTableCellSelectionAllowed to set
	 */
	public void setTableCellSelectionAllowed(boolean isTableCellSelectionAllowed) {
		this.isTableCellSelectionAllowed = isTableCellSelectionAllowed;
	}

	/**
	 * Gets the column selected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the tableSelectedColumn
	 */
	public int getTableSelectedColumn() {
		return tableSelectedColumn;
	}

	/**
	 * Sets the column selected in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param tableSelectedColumn the tableSelectedColumn to set
	 */
	public void setTableSelectedColumn(int tableSelectedColumn) {
		this.tableSelectedColumn = tableSelectedColumn;
	}

	/**
	 * Gets the count of selected columns in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @return the tableSelectedColumnCount
	 */
	public int getTableSelectedColumnCount() {
		return tableSelectedColumnCount;
	}

	/**
	 * Sets the count of selected columns in {@link JTable}, in the last event detected.<br>
	 * <p>
	 * @param tableSelectedColumnCount the tableSelectedColumnCount to set
	 */
	public void setTableSelectedColumnCount(int tableSelectedColumnCount) {
		this.tableSelectedColumnCount = tableSelectedColumnCount;
	}

	
	
	/**
	 * Gets if the current table row is to be discarded,<br>
	 * <p>
	 * @return the isTableRowToDiscard
	 */
	public boolean isTableRowToDiscard() {
		return isTableRowToDiscard;
	}

	/**
	 * Sets if the current table row is to be discarded,<br>
	 * <p>
	 * @param isTableRowToDiscard the isTableRowToDiscard to set
	 */
	public void setTableRowToDiscard(boolean isTableRowToDiscard) {
		this.isTableRowToDiscard = isTableRowToDiscard;
	}

	/**
	 * Gets if the current list item is to be discarded,<br>
	 * <p>
	 * @return the isListItemToDiscard
	 */
	public boolean isListItemToDiscard() {
		return isListItemToDiscard;
	}

	/**
	 * Sets if the current list item is to be discarded,<br>
	 * <p>
	 * @param isListItemToDiscard the isListItemToDiscard to set
	 */
	public void setListItemToDiscard(boolean isListItemToDiscard) {
		this.isListItemToDiscard = isListItemToDiscard;
	}

	/**
	 * Gets the list name implemented by a JList object and involved in the last event.<br>
	 * <p>
	 * @return the listName
	 */
	public String getListName() {
		return listName;
	}

	/**
	 * Gets the list name implemented by a JList object and involved in the last event.<br>
	 * <p>
	 * @param listName the listName to set
	 */
	public void setListName(String listName) {
		this.listName = listName;
	}

	/**
	 * Gets the first list index selected in the last event.<br>
	 * <p>
	 * @return the listSelectedIndex
	 */
	public int getListSelectedIndex() {
		return listSelectedIndex;
	}

	/**
	 * Sets the first list index selected in the last event.<br>
	 * <p>
	 * @param listSelectedIndex the listSelectedIndex to set
	 */
	public void setListSelectedIndex(int listSelectedIndex) {
		this.listSelectedIndex = listSelectedIndex;
	}

	/**
	 * Gets the last list index selected in the last event.<br>
	 * <p>
	 * @return the listSelectedIndexLeading
	 */
	public int getListSelectedIndexLeading() {
		return listSelectedIndexLeading;
	}

	/**
	 * Sets the last list index selected in the last event.<br>
	 * <p>
	 * @param listSelectedIndexLeading the listSelectedIndexLeading to set
	 */
	public void setListSelectedIndexLeading(int listSelectedIndexLeading) {
		this.listSelectedIndexLeading = listSelectedIndexLeading;
	}

	/**
	 * Gets the list item selected in the last event.<br>
	 * If more then one item has been selected, then the first one will be returned.<br>
	 * <p>
	 * @return the listSelectedItem
	 */
	public String getListSelectedItem() {
		return listSelectedItem;
	}

	/**
	 * Sets the list item selected in the last event.<br>
	 * If more then one item has been selected, then the first one will be set.<br>
	 * <p>
	 * @param listSelectedItem the listSelectedItem to set
	 */
	public void setListSelectedItem(String listSelectedItem) {
		this.listSelectedItem = listSelectedItem;
	}

	/**
	 * Gets the application bound object of the list item selected in the last event.<br>
	 * If more then one item has been selected, then the first one will be returned.<br>
	 * <p>
	 * @return the listBoundObject
	 */
	public Object getListBoundObject() {
		return listBoundObject;
	}

	/**
	 * Sets the application bound object of the list item selected in the last event.<br>
	 * If more then one item has been selected, then the first one will be set.<br>
	 * <p>
	 * @param listBoundObject the listBoundObject to set
	 */
	public void setListBoundObject(Object listBoundObject) {
		this.listBoundObject = listBoundObject;
	}

	/**
	 * Gets all application bound object of the list items selected in the last event.<br>
	 * <p>
	 * @return the al_listBoundObjects
	 */
	public ArrayList<Object> getListSelectedObjects() {
		return al_listBoundObject;
	}

	/**
	 * Sets all application bound object of the list items selected in the last event.<br>
	 * <p>
	 * @param al_listBoundObjects the al_listBoundObjects to set
	 */
	public void setListSelectedObjects(ArrayList<Object> al_listBoundObjects) {
		this.al_listBoundObject = al_listBoundObjects;
	}

	/**
	 * Gets items values of all list item selected in the last event.<br>
	 * <p>
	 * @return the al_listSelectedValue
	 */
	public ArrayList<String> getListSelectedItems() {
		return al_listSelectedItem;
	}

	/**
	 * Sets items values of all list item selected in the last event.<br>
	 * <p>
	 * @param al_listSelectedValue the al_listSelectedValue to set
	 */
	public void setListSelectedItems(ArrayList<String> al_listSelectedItem) {
		this.al_listSelectedItem = al_listSelectedItem;
	}


	/**
	 * Gets items values of all list item unselected due the last event.<br>
	 * <p>
	 * @return the al_listUnSelectedItem
	 */
	public ArrayList<String> getListUnSelectedItems() {
		return al_listUnSelectedItem;
	}

	/**
	 * Sets items values of all list item unselected in the last event.<br>
	 * <p>
	 * @param al_listUnSelectedItem the al_listUnSelectedItem to set
	 */
	public void setListUnSelectedItems(ArrayList<String> al_listUnSelectedItem) {
		this.al_listUnSelectedItem = al_listUnSelectedItem;
	}


	
	/**
	 * Gets indexes of all list item selected in the last event.<br>
	 * <p>
	 * @return the al_listSelectedIndexes
	 */
	public ArrayList<Integer> getListSelectedIndexes() {
		return al_listSelectedIndex;
	}

	/**
	 * Sets indexes of all list item selected in the last event.<br>
	 * <p>
	 * @param al_listSelectedIndexes the al_listSelectedIndexes to set
	 */
	public void setListSelectedIndexes(ArrayList<Integer> al_listSelectedIndexes) {
		this.al_listSelectedIndex = al_listSelectedIndexes;
	}

	
	/**
	 * Gets indexes of all list item uselected in the last event.<br>
	 * <p>
	 * @return the al_listUnSelectedIndex
	 */
	public ArrayList<Integer> getListUnSelectedIndexes() {
		return al_listUnSelectedIndex;
	}

	/**
	 * Sets indexes of all list item unselected in the last event.<br>
	 * <p>
	 * @param al_listUnSelectedIndex the al_listUnSelectedIndex to set
	 */
	public void setListUnSelectedIndexes(ArrayList<Integer> al_listUnSelectedIndex) {
		this.al_listUnSelectedIndex = al_listUnSelectedIndex;
	}

	
	/**
	 * Gets the {@link TreeSelectionModel} of the {@link JTree} for the event occurred.<br>
	 * <p>
	 * @return the eventTreeSelectionModel
	 */
	public TreeSelectionModel getEventTreeSelectionModel() {
		return eventTreeSelectionModel;
	}

	/**
	 * Sets the {@link TreeSelectionModel} of the {@link JTree} for the event occurred.<br>
	 * <p>
	 * @param eventTreeSelectionModel the eventTreeSelectionModel to set
	 */
	public void setEventTreeSelectionModel(
			TreeSelectionModel eventTreeSelectionModel) {
		this.eventTreeSelectionModel = eventTreeSelectionModel;
	}

	
	/**
	 * Gets the index, 0-based, of the current or first selected node,<br>
	 * under his own parent.<br>
	 * <p>
	 * @return the eventTreeNodeIndex
	 */
	public int getEventTreeNodeIndex() {
		return eventTreeNodeIndex;
	}

	/**
	 * Sets the index, 0-based, of the current or first selected node,<br>
	 * under his own parent.<br>
	 * <p>
	 * @param eventTreeNodeIndex the eventTreeNodeIndex to set
	 */
	public void setEventTreeNodeIndex(int eventTreeNodeIndex) {
		this.eventTreeNodeIndex = eventTreeNodeIndex;
	}

	/**
	 * Gets the the current node active as a DefaultMutableTreeNode object <br>
	 * of the {@link JTree}, for the event occurred.<br>
	 * <p>
	 * @return the eventTreeNode
	 */
	public DefaultMutableTreeNode getEventTreeNode() {
		return eventTreeNode;
	}

	/**
	 * Sets the the current node active as a DefaultMutableTreeNode object <br>
	 * of the {@link JTree}, for the event occurred.<br>
	 * <p>
	 * @param eventTreeNode the eventTreeNode to set
	 */
	public void setEventTreeNode(DefaultMutableTreeNode eventTreeNode) {
		this.eventTreeNode = eventTreeNode;
	}

	/**
	 * Gets the user application object of the current node selected.<br>
	 * When more then one node is selected, will be returned the first.<br>
	 * <p>
	 * @return the eventTreeNodeObject
	 */
	public Object getEventTreeNodeObject() {
		return eventTreeNodeObject;
	}

	/**
	 * Sets the user application object of the current node selected.<br>
	 * <p>
	 * This method is intended for internal forward use only<br>
	 * <p>
	 * @param eventTreeNodeObject the eventTreeNodeObject to set
	 */
	public void setEventTreeNodeObject(Object eventTreeNodeObject) {
		this.eventTreeNodeObject = eventTreeNodeObject;
	}

	/**
	 * Gets the the parent of current node active as a DefaultMutableTreeNode object <br>
	 * of the {@link JTree}, for the event occurred.<br>
	 * <p>
	 * @return the eventTreeNodeParent
	 */
	public DefaultMutableTreeNode getEventTreeNodeParent() {
		return eventTreeNodeParent;
	}

	/**
	 * Sets the the parent of current node active as a DefaultMutableTreeNode object <br>
	 * of the {@link JTree}, for the event occurred.<br>
	 * <p>
	 * @param eventTreeNodeParent the eventTreeNodeParent to set
	 */
	public void setEventTreeNodeParent(DefaultMutableTreeNode eventTreeNodeParent) {
		this.eventTreeNodeParent = eventTreeNodeParent;
	}

	/**
	 * Gets all parent of current node active as a DefaultMutableTreeNode object <br>
	 * of the {@link JTree}, for the event occurred.<br>
	 * That's the nodes list starting with the root node to dithe parent of the current node.
	 * <p>
	 * @return the eventTreeNodesParent
	 */
	public DefaultMutableTreeNode[] getEventTreeNodesParent() {
		return eventTreeNodesParent;
	}

	/**
	 * Sets all parenst of current node active as an array of DefaultMutableTreeNode objects <br>
	 * of the {@link JTree}, for the event occurred.<br>
	 * That's the nodes list starting with the root node to dithe parent of the current node.
	 * <p>
	 * @param eventTreeNodesParent the eventTreeNodesParent to set
	 */
	public void setEventTreeNodesParent(DefaultMutableTreeNode[] eventTreeNodesParent) {
		this.eventTreeNodesParent = eventTreeNodesParent;
	}

	/**
	 * Gets all nodes currently selected, even when multiple selection is enabled.<br>
	 * <p>
	 * @return the eventTreeNodesSelected
	 */
	public DefaultMutableTreeNode[] getEventTreeNodesSelected() {
		return eventTreeNodesSelected;
	}

	/**
	 * Sets all nodes currently selected, even when multiple selection is enabled.<br>
	 * <p>
	 * @param eventTreeNodesSelected the eventTreeNodesSelected to set
	 */
	public void setEventTreeNodesSelected(DefaultMutableTreeNode[] eventTreeNodesSelected) {
		this.eventTreeNodesSelected = eventTreeNodesSelected;
	}

	
	/**
	 * Gets the tree model of tree involved in the last event.<br>
	 * That's a {@link ForwardTreeModel} object that inherits from standard {@link DefaultTreeModel}<br>
	 * with additional informations.<br>
	 * <p>
	 * @return the TreeModel object
	 */
	public ForwardTreeModel getEventTreeModel() {
		return eventTreeModel;
	}

	/**
	 * Sets the tree model of tree involved in the last event.<br>
	 * That's a {@link ForwardTreeModel} object that inherits from standard {@link DefaultTreeModel}<br>
	 * with additional informations.<br>
	 * <p>
	 * @param eventTreeModel the TreeModel to set
	 */
	public void setEventTreeModel(ForwardTreeModel eventTreeModel) {
		this.eventTreeModel = eventTreeModel;
	}

	/**
	 * Gets the name of the last combobox of wich an item has been selected.<br>
	 * To use on occurred event.<br>
	 * <p>
	 * @return the comboBoxName
	 */
	public String getComboBoxName() {
		return comboBoxName;
	}

	/**
	 * Sets the name of the last combobox of wich an item has been selected.<br>
	 * To use on occurred event.<br>
	 * <p>
	 * @param comboBoxName the comboBoxName to set
	 */
	public void setComboBoxName(String comboBoxName) {
		this.comboBoxName = comboBoxName;
	}

	/**
	 * Gets the index of the last combobox of wich an item has been selected.<br>
	 * To use on occurred event.<br>
	 * <p>
	 * @return the comboBoxSelectedIndex
	 */
	public int getComboBoxSelectedIndex() {
		return comboBoxSelectedIndex;
	}

	/**
	 * Sets the index of the last combobox of wich an item has been selected.<br>
	 * To use on occurred event.<br>
	 * <p>
	 * @param comboBoxSelectedIndex the comboBoxSelectedIndex to set
	 */
	public void setComboBoxSelectedIndex(int comboBoxSelectedIndex) {
		this.comboBoxSelectedIndex = comboBoxSelectedIndex;
	}

	/**
	 * Gets the item of the last combobox of wich an item has been selected.<br>
	 * To use on occurred event.<br>
	 * <p>
	 * @return the comboBoxSelectedItem
	 */
	public String getComboBoxSelectedItem() {
		return comboBoxSelectedItem;
	}

	/**
	 * Sets the item of the last combobox of wich an item has been selected.<br>
	 * To use on occurred event.<br>
	 * <p>
	 * @param comboBoxSelectedItem the comboBoxSelectedItem to set
	 */
	public void setComboBoxSelectedItem(String comboBoxSelectedItem) {
		this.comboBoxSelectedItem = comboBoxSelectedItem;
	}

	/**
	 * Gets the object bound to the item selected of the last combobox of wich an item has been selected.<br>
	 * To use on occurred event.<br>
	 * <p>
	 * @return the comboBoxBoundObject
	 */
	public Object getComboBoxBoundObject() {
		return comboBoxBoundObject;
	}

	/**
	 * Sets the object bound to the item selected of the last combobox of wich an item has been selected.<br>
	 * To use on occurred event.<br>
	 * <p>
	 * @param comboBoxBoundObject the comboBoxBoundObject to set
	 */
	public void setComboBoxBoundObject(Object comboBoxBoundObject) {
		this.comboBoxBoundObject = comboBoxBoundObject;
	}

	/**
	 * Gets if the current combobox item is to be discarded,<br>
	 * <p>
	 * @return the isComboBoxItemToDiscard
	 */
	public boolean isComboBoxItemToDiscard() {
		return isComboBoxItemToDiscard;
	}

	
	
	/**
	 * Masks the combo box state changed event, with no action executions, if any.<br>
	 * <p>
	 * When a combobox populating is in progress, the coding of the first item of the combobox<br>
	 * causes the state changed event to be triggered by java events handling.<br>
	 * May be convenient to postpone the actions to be executed, after the populating process,<br>
	 * not to change the current state.<br>
	 * When the populating process ends all masked actions can be executed.<br>
	 * <p>
	 * This method is to be intended for forward monitor internal use only.<br>
	 * <p>
	 * 
	 * @return the isComboBoxEventMasked
	 */
	public boolean isComboBoxEventsMasked() {
		return isComboBoxEventMasked;
	}

	
	/**
	 * Reset the mask condition for the combo box state changed event, allowing action executions, if any.<br>
	 * <p>
	 * When a combobox populating is in progress, the coding of the first item of the combobox<br>
	 * causes the state changed event to be triggered by java events handling.<br>
	 * May be convenient to postpone the actions to be executed, after the populating process,<br>
	 * not to change the current state.<br>
	 * When the populating process ends all masked actions can be executed.<br>
	 * <p>
	 * This method is to be intended for forward monitor internal use only.<br>
	 * <p>
	 * 
	 * @param isComboBoxEventMasked the isComboBoxEventMasked to set
	 */
	public void setComboBoxEventsMasked(boolean isComboBoxEventMasked) {
		this.isComboBoxEventMasked = isComboBoxEventMasked;
	}

	
	/**
	 * Gets if some GUI control has been set in error by panel controls.<br>
	 * <p>
	 * @return the errorsFound
	 */
	public boolean isErrorsFound() {
		return errorsFound;
	}

	/**
	 * Sets if some GUI control has been set in error by panel controls.<br>
	 * <p>
	 * @param errorsFound the errorsFound to set
	 */
	public void setErrorsFound(boolean errorsFound) {
		this.errorsFound = errorsFound;
	}

	
	/**
	 * Gets if some GUI control has been set in formal error by panel controls.<br>
	 * <p>
	 * @return the errorsFormal
	 */
	public boolean isErrorsFormal() {
		return errorsFormal;
	}

	/**
	 * Sets if some GUI control has been set in formal error by panel controls.<br>
	 * <p>
	 * @param errorsFormal the errorsFormal to set
	 */
	public void setErrorsFormal(boolean errorsFormal) {
		this.errorsFormal = errorsFormal;
	}

	/**
	 * Gets if some GUI control has been set in rule table existence/unexistence error by panel controls.<br>
	 * <p>
	 * @return the errorsExistenceRuleTable
	 */
	public boolean isErrorsExistenceRuleTable() {
		return errorsExistenceRuleTable;
	}

	/**
	 * Sets if some GUI control has been set in rule table existence/unexistence error by panel controls.<br>
	 * <p>
	 * @param errorsExistenceRuleTable the errorsExistenceRuleTable to set
	 */
	public void setErrorsExistenceRuleTable(boolean errorsExistenceRuleTable) {
		this.errorsExistenceRuleTable = errorsExistenceRuleTable;
	}

	/**
	 * Gets if some GUI control has been set in entity existence/unexistence error by panel controls.<br>
	 * <p>
	 * @return the errorsExistenceEntity
	 */
	public boolean isErrorsExistenceEntity() {
		return errorsExistenceEntity;
	}

	/**
	 * Sets if some GUI control has been set in entity existence/unexistence error by panel controls.<br>
	 * <p>
	 * @param errorsExistenceEntity the errorsExistenceEntity to set
	 */
	public void setErrorsExistenceEntity(boolean errorsExistenceEntity) {
		this.errorsExistenceEntity = errorsExistenceEntity;
	}

	
	/**
	 * Gets if some GUI control has been set as wrong by user controls method during panel controls.<br>
	 * <p>
	 * @return the errorsUser
	 */
	public boolean isErrorsUser() {
		return errorsUser;
	}

	/**
	 * Sets if some GUI control has been set as wrong by user controls method during panel controls.<br>
	 * <p>
	 * @param errorsUser the errorsUser to set
	 */
	public void setErrorsUser(boolean errorsUser) {
		this.errorsUser = errorsUser;
	}

	/**
	 * Gets if the component has an error message<br>
	 * <p>
	 * @return true if some error is active for the component
	 */
	public boolean isComponentError(String componentName) {
        for (InnerComponentMessage component : this.al_message) {
			if (component.componentName.equals(componentName)) {
				if (component.msgType != EnumMessageType.WARNING 
			    &&  component.msgType != EnumMessageType.INFORMATION
			    &&  component.msgType != EnumMessageType.DEBUG	) {
					return true;
				}
				return false;
			}
		}
		return false;
	}
	/**
	 * Get error and not errors messages structure.<br>
	 * <p>
	 * It will be returned an ArrayList with all last messages.<br>
	 * <p>
	 * @return all messages 
	 */
	public ArrayList<InnerComponentMessage> getMessages() {
		return al_message;
	}

	/**
	 * Add a message in the function structure to be displayed by the standard forward function {@link FunctionSystemShowMessages}<br>
	 * <p>
	 * @param msgOrigin
	 * @param msgType
	 * @param numRuleTable
	 * @param panelName
	 * @param componentName
	 * @param msgCode
	 * @param msgShortText
	 * @return the internal 
	 */
	public void addMessage(EnumForwardAction msgOrigin, EnumMessageType msgType, int numRuleTable, String panelName, String componentName, String msgCode, String msgShortText) {
		InnerComponentMessage innerComponentMessage	= null;
		innerComponentMessage = new InnerComponentMessage();
		innerComponentMessage.msgOrigin =msgOrigin;
		innerComponentMessage.msgType = msgType;
		innerComponentMessage.numRuleTable = numRuleTable;
		innerComponentMessage.panelName = panelName;
		innerComponentMessage.componentName = componentName;
		innerComponentMessage.msgCode = msgCode;
		innerComponentMessage.msgShortText = msgShortText;
		
		
		// A
		getMessages().add(innerComponentMessage);
		
		// Impostazione flags di errore
		setErrorByMsgType(msgOrigin, msgType);
	}


	/**
	 * Add a message in the function structure to be displayed by the standard forward function {@link FunctionSystemShowMessages}<br>
	 * <p>
	 * Add a message to be displayed after panel controls by meanns of the user conontrol method.<br>
	 * For error message types i will be automatically flagged the errors detecting.
	 * <p>
	 * @param msgType
	 * @param numRuleTable
	 * @param panelName
	 * @param componentName
	 * @param msgCode
	 * @param msgShortText
	 * @return the internal 
	 */
	public void addMessageUser(EnumMessageType msgType, int numRuleTable, String panelName, String componentName, String msgCode, String msgShortText) {
		InnerComponentMessage innerComponentMessage	= null;
		innerComponentMessage = new InnerComponentMessage();
		innerComponentMessage.msgOrigin = EnumForwardAction.PANEL_CONTROLS_USER;
		innerComponentMessage.msgType = msgType;
		innerComponentMessage.numRuleTable = numRuleTable;
		innerComponentMessage.panelName = panelName;
		innerComponentMessage.componentName = componentName;
		innerComponentMessage.msgCode = msgCode;
		innerComponentMessage.msgShortText = msgShortText;
		getMessages().add(innerComponentMessage);
		
		// Impostazione flags di errore
		setErrorByMsgType(EnumForwardAction.PANEL_CONTROLS_USER, msgType);
	}

	/*
	 * Impostazione flag generale di errore in base a tipo messaggio e origine
	 */
	private void setErrorByMsgType(EnumForwardAction msgOrigin, EnumMessageType msgType) {
		if (msgType == EnumMessageType.ERROR_INPUT) {
			this.setErrorsFound(true);
			if (msgOrigin == EnumForwardAction.PANEL_CONTROLS_FORMAL) 				{this.setErrorsFormal(true);}
			if (msgOrigin == EnumForwardAction.PANEL_CONTROLS_EXISTENCE_RULE_TABLE) {this.setErrorsExistenceRuleTable(true);}
			if (msgOrigin == EnumForwardAction.PANEL_CONTROLS_EXISTENCE_ENTITY) 	{this.setErrorsExistenceEntity(true);}
			if (msgOrigin == EnumForwardAction.PANEL_CONTROLS_EXISTENCE_ENTITY) 	{this.setErrorsUser(true);}
			return;
			
		}
		if (msgType == EnumMessageType.ERROR_DATABASE
		||  msgType == EnumMessageType.ERROR_FATAL   
		||  msgType == EnumMessageType.ERROR_INTERNAL) {
			this.setErrorsFound(true);
		}
	}

	/**
	 * Clear messages explicitely added by the specified function declared action origin<br>
	 * <p>
	 * @param msgOrigin as a {@link EnumForwardAction} 	enumeration <pre>
PANEL_CONTROLS_FORMAL
PANEL_CONTROLS_EXISTENCE_ENTITY
PANEL_CONTROLS_EXISTENCE_RULE_TABLE
PANEL_CONTROLS_USER
	 * 	 */
	public void clearMessages(EnumForwardAction msgOrigin) {
		InnerComponentMessage message = null;
		Collection<InnerComponentMessage> li_msgToRemove = null;
		
		// Ricerca messaggi da rimuovere
		li_msgToRemove = new ArrayList<InnerComponentMessage> ();
		for (int i = 0; i < this.getMessages().size(); i++) {
			message = this.getMessages().get(i);
			if (message.msgOrigin == msgOrigin) {
				li_msgToRemove.add(message);
			}
		}
		// rimozione fisica messaggi
		this.getMessages().remove(li_msgToRemove);
	}

	/**
	 * Gets an array list of objects with events masked when combobox state changed events<br>
	 * occurred under the populate process.<br>
	 * The array list will be updated only if setComboBoxEventMasked(true) has been executed.<br>
	 * <p>
	 * This method is to be intended for forward monitor internal use only.<br>
	 * <p>
	 * @return the al_comboBoxEventMasked
	 */
	public ArrayList<Object> getComboBoxEventsMasked() {
		return al_comboBoxEventMasked;
	}



	/**
	 * Sets if the current combobox item is to be discarded,<br>
	 * <p>
	 * @param isComboBoxItemToDiscard the isComboBoxItemToDiscard to set
	 */
	public void setComboBoxItemToDiscard(boolean isComboBoxItemToDiscard) {
		this.isComboBoxItemToDiscard = isComboBoxItemToDiscard;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected Object clone()  {
		ForwardSystem thisCloned = null;
		try {
			thisCloned = (ForwardSystem) super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
		return thisCloned;
	}
}
