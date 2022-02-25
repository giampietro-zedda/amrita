package forward;

import java.awt.Color;
import java.awt.Font;

import enums.EnumForwardAction;
import enums.EnumForwardEvent;
import enums.EnumForwardOption;
import enums.EnumLanguage;
import enums.EnumMessageType;
/**
 * copyright (c) 2009-2012 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ForwardDoParms
 * </h1>
 * <p>
 * Models all informations for actions to do as declared in a forward function. <br>
 * Here are simply all parameters definitions.
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 29/dic/2011 
 *
*/

	public class ForwardDoParms  {
		
		 // Info comuni a + azioni
		 EnumForwardAction action = null;     					// Azione da intraprendere
	     String componentName = "";								// Nome componente swing 
	     Object parms[] = null;                              	// Parametri non codificati passati validi per DO_EXEC_METHOD()
	     
	     // Info associate ad azioni generali e comuni a più componenti
	     EnumForwardEvent[] componentMaskedEvents = null;       // Eventi da maskerare per il componente
	     Object componentValue = "";                            // Oggetto con cui valorizzare il controllo GUI
	     String componentValueVarName = "";                     // Nome variabile applicativa con cui valorizzare il controllo GUI
	     boolean componentEventsToMask = false;					// True maskera gli eventi precedenti, false li elimina dalle strutture di mascheramento
	     Font componentFont = null;                      		// Font del componente
	     boolean componentEnabled = false;                      // Proprieta enable di componento
	     boolean componentVisible = false;                      // Proprieta visible di componento
	     boolean componentChecked = false;                      // Proprieta checked di checkBox/radioButton/toggleButton
	     boolean componentError = false;                      	// Componente in errore
	     boolean componentFocusable = false;                    // Componente focusabile
	     boolean componentHiglight = false;                     // Componente evidenziato
	     
		 ////////////////////////////////////////////////
		 // Info per dialog
		 ////////////////////////////////////////////////
		 String dialogName  = "";								// Nome dialogo utilizzato a livello applicativo come reference
	     boolean dialogModal = true;                      		// Opzional
	     
	     // Info specifiche per dialoghi standard info/error/warning/...
	     int dialogType  = 0;					    			// Tipo dialogo come JOptionPane.ERROR_MESSAGE, INFORMATION_MESSAGE, WARNING_MESSAGE, QUESTION_MESSAGE, PLAIN_MESSAGE
	     String dialogTitle  = "";								// Titolo frame
	     String dialogMessage  = "";							// messaggio da visualizzare
	     int dialogTypeOptions = 0;  							// Opzioni standard JOptionPane.YES_NO_OPTION, YES_NO_CANCEL_OPTION, OK_CANCEL_OPTION, DEFAULT_OPTION
	     String dialogIconPath = "";  							// Optional
	     String dialogSelectionValues[] = null;					// Optional
	     String dialogInitialSelectionValue = "";				// Optional
	     String dialogLabelsOptionCustom[] = null;				// Optional
	     String dialogLabelOptionCustomSelected = "";			// Optional
	     boolean isDialogWantsInput = false;                    // True per dialoghi di richiesta input
	     // Icone dialoghi standard
	     boolean isDialogWithIcon = false;           			// True indica utilizzo di icona
	     boolean isDialogWithIconUser = false;       			// True indica utilizzo di icona fornita dall'utente
		 // Info per dialog user stessa funzione
	     String dialogForm = null;                   			// Form da visualizzare nel dialogo, identificato dal suo panel radice
	     String dialogIconPathTitleBar = "";                    // Icona in barra del titolo
	     int dialogPosX = 0;                                    // Posizione X iniziale del dialogo o di succeessiva Move
	     int dialogPosY = 0;                                    // Posizione Y iniziale del dialogo o di succeessiva Move
	     EnumForwardOption dialogPos = null;                    // Dialogo al centro dello schermo o del parent o in posizione specifica X, Y
	     boolean isDialogUndecorated = false;                   // Dialogo con o senza cornice
	     
	     
		 ////////////////////////////////////////////////
		 // Info per dialog file chooser
		 ////////////////////////////////////////////////
	     String fileChooserName = "";  							// Nome dialogo e oggetto JFileChooser
		 String fileChooserTitle = ""; 							// Titoloframe di dialogo
	     boolean isFileChooserMultiSelectionEnabled = false; 	// Abilitazione selezione multipla
		 int fileChooserFileSelectionMode = 0;				    // Opzionale JFileChooser.FILES_AND_DIRECTORIES, JFileChooser.FILES_ONLY, JFileChooser.DIRECTORIES_ONLY

		 ////////////////////////////////////////////////
		 // Info per dialog color chooser
		 ////////////////////////////////////////////////
	     String colorChooserName = "";  						// Nome dialogo e oggetto JColorChooser
		 String colorChooserTitle = ""; 						// Titoloframe di dialogo
		 Color colorChooserInitial = null; 						// Initial color
		 
		 ////////////////////////////////////////////////
		 // Info per esecuzione metodi con logiche 
		 ////////////////////////////////////////////////
	     String methodName = "";								// Metodo da eseguire/eseguito
	
		 ////////////////////////////////////////////////
		 // Info per esecuzione gruppi di actions
		 ////////////////////////////////////////////////
	     String groupActionsName = "";							// Identificativo gruppo di actions
	     ForwardDoParms[] groupActionsList = null;				// Actions da eseguire definite nel gruppo
	     
		 /////////////////////////////////////////////////////////////
		 // Info associate a componenti per refresh, show, hide, etc.
		 /////////////////////////////////////////////////////////////
	     String errorCode = "";									// Codice errore applicativo impostato
	     String toolTipText = "";								// Testo toolTip
	     String localeToolTipText = "";							// Codice internazionalizzazione testo toolTip

	
	     /////////////////////////////////////////////////////////////
		 // Info associate a panel
		 /////////////////////////////////////////////////////////////
	     
	     String panelName = "";									// Nome pannello
	     String panelNames[] = null;							// Nomi pannelli swing
	     String panelLdvClassName = "";                      	// Nome classe di gestione vista logica di popolamento

	     
		 /////////////////////////////////////////////////////////////
		 // Info associate ad azioni su combo (JComboBox)
		 /////////////////////////////////////////////////////////////
	     int comboBoxItemIndex = 0;								// Index 0-based in comboBox da sel/update/delete
	     String comboBoxItemText = "";						    // Item text value to append/update
	     String comboBoxLdvColSource = "";                      // Nome colonna vista logica da utilizzare per il caricamento, visibile all'utente
	     String comboBoxLdvClassName = "";                      // Nome classe di gestione vista logica
	     String comboBoxLdvColObjectBound = null;               // Nome colonna ldv con oggetto bound da collegare all'item
	     int comboBoxLdvRuleTableNum = 0;               		// Numero rule table da utilizzare con la logical data view per popolare la combobox
	     Object comboBoxItemObjectBound = null;                	// Item object bound
	     String comboBoxOrdinalVarNameReturned = "";            // Nome variabile dove restituire l'integer rappresentante l'ordinal dell'item selezionato, 0-based
	     String comboBoxOrdinalVarNameValue = "";               // Nome variabile contenente il valore di un item, di cui restituire l'integer rappresentante l'ordinal dell'item selezionato, 0-based
	     String comboBoxOrdinalValue = "";               		// Valore embedded nella action di cui restituire l'integer rappresentante l'ordinal dell'item, 0-based
	     String comboBoxVarNameObjectBound = "";                // Nome variabile a livello funzione che conterrà l'oggetto bound
	     boolean comboBoxObjectBoundToUpdate = false;          	// True indica che l'oggetto bound dell'item deve essere  aggiornato
	     boolean comboBoxObjectBoundToInteger = false;          // True indica che l'oggetto bound dell'item deve essere convertito da String a Integer prima di essere memorizzato
	     
	     
		 /////////////////////////////////////////////////////////////
		 // Info associate ad azioni su liste (JList)
		 /////////////////////////////////////////////////////////////
	     int listItemIndex = 0;								    // Index 0-based in list da sel/update/delete
	     int listItemIndexes[] = null;                          // Indexes 0-based in list da sel/update/delete
	     String listItemText = "";								// Item text value to append/update
	     String listLdvColSource = "";                       	// Nome colonna vista logica da utilizzare per il caricamento
	     String listLdvClassName = "";                      	// Nome classe di gestione vista logica
	     String listLdvColObjectBound = null;                	// Nome colonna ldv con oggetto bound da collegare all'item
	     int listLdvRuleTableNum = 0;               			// Numero rule table da utilizzare con la logical data view per popolare la list
	     Object listItemObjectBound = null;            			// Item object bound
	     String listVarNameObjectBound = "";                	// Nome variabile a livello funzione che conterrà l'oggetto bound
	     boolean listObjectBoundToUpdate = false;      			// True indica che l'oggetto bound dell'item deve essere  aggiornato

		 /////////////////////////////////////////////////////////////
		 // Info associate ad azioni su tabelle (JTable)
		 /////////////////////////////////////////////////////////////
	     String tableLdvClassName = "";                      	// Nome classe di gestione vista logica
	     Object tableItemObjectBound = null;                	// Item object bound
	     String tableLdvColumnsBound[] = null; 					// Colonne logical data view da associare ai campi
	     String tableJavaFieldsBound[] = null; 					// Nomi componenti java da valorizzare con la colonna di tableLdvColumnsBound
	     String tableLdvColumnName = null; 						// Nome colonna in logical data view
	     Object tableLdvColumnNewValue = null; 					// Nuovo valore da assegnare a colonna logical data view prima di renderre disponibile i dati
	     String tableColToolTipText = "";					    // Testo toolTip a livello di colonna (header)
	     String tableCellToolTipText = "";					    // Testo toolTip a livello di cella (row+col)
	     int tableRowIndex = 0;							    	// Index Row 0-based in table da sel/update/delete
	     int tableColIndex = 0;							    	// Index Col 0-based in table  
	     int tableRowIndexes[] = null;                          // Indexes 0-based in table da sel/update/delete
	     int tableColIndexes[] = null;                          // Indexes 0-based in table da sel/update/delete
	     Object tableRowObjectBound = null;            			// Row object bound
	     String tableLdvColObjectBound = null;                	// Nome colonna ldv con oggetto bound da collegare alla riga
	     boolean tableObjectBoundToUpdate = false;      		// True indica che l'oggetto bound della riga deve essere aggiornato, o dal valore di VAR() o dal valore in input
	     boolean tableObjectBoundFromVar = false;      			// True indica che l'oggetto bound della riga deve essere  recuperato dalla variabile VAR()
	     String tableVarNameObjectBound = "";                	// Nome variabile a livello funzione che conterrà l'oggetto bound
	     Object tableRowObjects[] = null;                       // Array di oggetti componenti la riga da inserire nella tabella
	     Object tableCellObject = null;                         // Oggetto cella di tabella identificato da riga e colonna
	     String tableColumnName = null;                         // Nome colonna tabella
	     boolean tableColumnResizable = false;                  // Colonna tabella con resize ammesso/vietato
	     boolean tableColumnResizableOnlySelectedCols = false;  // True imposta la property resizable solo delle righe selezionate, false di tutte
	     int tableColumnResizableNumStart = -1;                 // Numero col di inizio a cui si applica resizable, per -1 si applica a tutte le col
	     int tableColumnResizableNumEnd = -1;                   // Numero col di fine a cui si applica resizable, per -1 si applica a tutte le col
	     boolean tableColumnEditable = false;                   // Colonna tabella con editing ammesso/vietato
	     boolean tableColumnEditableOnlySelectedCols = false;  // True imposta la property editable solo delle righe selezionate, false di tutte
	     int tableColumnEditableNumStart = -1;                  // Numero col di inizio a cui si applica editable, per -1 si applica a tutte le col
	     int tableColumnEditableNumEnd = -1;                    // Numero col di fine a cui si applica editable, per -1 si applica a tutte le col
	     boolean tableColumnHidden = false;                     // Colonna tabella con invisibile/visibile
	     int tableColumnHiddenNumStart = -1;                    // Numero col di inizio a cui si applica hidden, per -1 si applica a tutte le col
	     int tableColumnHiddenNumEnd = -1;                     	// Numero col di fine a cui si applica hidden, per -1 si applica a tutte le col
	     boolean tableColumnWidthToFitContent = false;          // True per adattare automaticamente le dimensione della colonna al contenuto
	     boolean tableColumnWidthToFitHeader = false;           // True per adattare automaticamente le dimensione della colonna a quelle dell'header
	     boolean tableGridVisible = false;                 		// True per linee orizzontali e verticali visibili
	     boolean tableVerticalLinesVisible = false;             // True per linee verticali visibili
	     boolean tableHorizontalLinesVisible = false;           // True per linee orizzontali visibili
	     boolean tableFillViewPortHeight = false;           	// True per riempire tutta l'area tabella
	     boolean tableSelectionSingle = false;            		// Selezione singole righe abilitata
	     boolean tableSelectionSingleInterval = false;    		// Selezione singolo intervallo righe abilitata
	     boolean tableSelectionMultipleInterval = false;  		// Selezione multipli intervalli righe abilitata
	     boolean tableSelectionRows = false;  					// Selezione righe abilitata
	     boolean tableSelectionCols = false;  					// Selezione colonne abilitata
	     boolean tableSelectionCells = false;  					// Selezione celle abilitata
	     int tableSelectionColFrom = 0;                         // Selezione colonne da
	     int tableSelectionColTo = 0;                         	// Selezione colonne a
	     int tableColumnMargin = 0;                     		// Margine in pixel fra colonne
	     int tableColumnWidth = 0;                     			// Width in pixel colonna
	     int tableRowHeight = 0;                     			// Height in pixel di riga 
	     boolean tableRowHeightOnlySelectedRows = false;        // True imposta lo spessore solo delle righe selezionate
	     int tableRowHeightNumStart = -1;                     	// Numero riga di inizio a cui si applica tableRowHeight, per -1 si applica a tutte le righe
	     int tableRowHeightNumEnd = -1;                     	// Numero riga di fine a cui si applica tableRowHeight, per -1 si applica a tutte le righe
	     int tableRowMargin = 0;                     			// Margine in pixel fra righe
	     Color tableSelectionForegroundColor = null;            // Colore fg di selezione
	     Color tableSelectionBackgroundColor = null;            // Colore bg di selezione
	     Color tableGridColor = null;            				// Colore della griglia
		     
		 /////////////////////////////////////////////////////////////
		 // Info associate a logical data view (ldv)
		 /////////////////////////////////////////////////////////////
	     String ldvIdOperation = "";                                  	// Identificativo da utilizzare nelle ON:CONDITION()
	     String ldvClassName = "";                                  	// Nome classe logical data view
	     Class<? extends ForwardLogicalDataView> ldvClass = null;		// Classe logical data view
	     String ldvEntityNameAs = "";                                 	// Nome interno AS utilizzato per l'entity
	     String ldvEntityName = "";                                 	// Nome classe bean di gestione dell'entity
	     int ldvEntityIndex = 0;                                 		// Ordinal dell'entity, 0-based, nella logical data view
	     Class<? extends ForwardLogicalDataView> ldvEntityClass = null;	// Classe ean di gestione dell'entity
	     String ldvColumnName = "";										// Nome colonna di cui impostare il valore
	     Object ldvColumnValue = null;									// Valore con cui valorizzare la variabile di colonna
	     String ldvVarName = "";										// Nome variabile applicativa da assegnare a ldvColumnName
	     String ldvWhereSql = "";										// Sql where da applicare sull'entity
	     Object ldvVarValue = null;										// Valore con cui valorizzare la variabile ldvVarName
	     int ldvRuleTableNum = 0;										// Numero tabella a cui accedere
	     String ldvFunctionVarName = "";								// Nome variabile applicativa da assegnare a ldvColumnName
	     String ldvFunctionControlName = "";							// Nome component GUI da valorizzare con il contenuto di ldvColumnName
	     String ldvFunctionComponentName = "";							// Nome component GUI o di variabile da valorizzare con il contenuto di ldvColumnName
	     String[] ldvColsToExclude = null;								// Colonne da escludere sa insert/update CRUD, per ldv di aggiornamento/Inserimento
	     int ldvRow = 0;												// Numero riga da leggere
	     int ldvPage = 0;												// Numero pagina da leggere 
	     int ldvPageRows = -1;											// Numero righe per pagina da restituire all'applicazione, -1 = no limit
	     int ldvPageMax = 0;											// Numero massimo di pagine da restituire all'applicazione
	     int ldvLimitRows = -1;											// Limite numero righe da leggere, -1 = no limit
	     EnumLanguage ldvLanguage = null;								// Linguaggio, valido solo per accesso a rule tables
	     boolean ldvDbAutoCommit = false;                               // Commit automatica dopo aggiornamenti
	     boolean ldvDbAutoConnection = false;  							// Connessione da acquisire/rilasciare automaticamente                       
	     
		 /////////////////////////////////////////////////////////////
		 // Info associate alla funzione o alla funzione da attivare
		 /////////////////////////////////////////////////////////////
	     int functionReturnCode = 0;									// Return code
	     String functionId = "";                               			// Codice di riconoscimento funzione chiamata, utilizzato al ritorno
	     String functionName = "";                               		// Funzione di cui modificare variabili, parametri, ...
	     String functionNameToLoad = "";                               	// Funzione da caricare
	     String functionNameToStart = "";                               // Funzione da attivare
	     boolean functionModal = false;									// Funzione da attivare in modo modale
	     boolean functionOnJFrame = false;								// Funzione da attivare in un JFrame
	     boolean functionOnJDialog = false;								// Funzione da attivare in un JDialog
	     boolean functionCurEnabled = false;							// Funzione corrente da disabilitare
	     boolean functionCurVisible = false;							// Funzione corrente da rendere visibile / invisibile
	     boolean functionReleaseOnReturn = false;						// Funzione da NON rilasciare alla return
	     boolean functionUndecorated = false;                   		// Dialogo funzione con o senza cornice
	     int functionPosX = 0;                                    		// Posizione X iniziale del dialogo della funzione o di succeessiva Move
	     int functionPosY = 0;                                    		// Posizione Y iniziale del dialogo della funzione o di succeessiva Move
	     EnumForwardOption functionPos = null;                    		// Dialogo funzione al centro dello schermo o del parent o in posizione specifica X, Y
         String functionVarName = "";                               	// Variabile di funzione da aggiornare
         Object functionVarValue = "";                               	// Oggetto con cui aggiornare la variabile
         String functionParmNameInCalled = "";                          // Parametro di funzione chiamata da aggiornare
         String functionParmNameInCaller = "";                          // Parametro di funzione chiamante da usare come input
         Object functionParmValue = "";                               	// Oggetto con cui aggiornare il parametro in funzione chiamata emnedded
         String functionParmComponentName = "";                         // Controllo Gui o variabile con cui aggiornare il parametro in funzione chiamata
         Object functionGuiValue = "";                               	// Oggetto con cui aggiornare il controllo GUI
         String functionGuiControlName = "";                            // Controllo GUI con cui aggiornare la variabile
	     int functionLookupTableNum = 0;                		    	// Numero tabella per funzione di lookupTabella

		 /////////////////////////////////////////////////////////////
		 // Info associate a informazioni di sistema
		 /////////////////////////////////////////////////////////////
	     String systemTimerName = "";									// Nome timer
	     int systemTimerDelayInitial = 0;								// Delay timer iniziale in millisecondi
	     int systemTimerDelay = 0;										// Delay timer in millisecondi

		 /////////////////////////////////////////////////////////////
		 // Info associate a messaggi di errore/warning/info/..
		 /////////////////////////////////////////////////////////////
	     String messageCode = "";                                       // Codice messaggio
	     EnumMessageType messageType = null;                            // Tipo messaggio
	     EnumLanguage messageLanguage = null;                           // Linguaggio
	     String messageComponentName = "";                              // Nome componente GUI/variabile da valorizzare con il testo del messaggio
	     
		 /////////////////////////////////////////////////////////////
		 // Info associate ad azioni sulle variabili
		 /////////////////////////////////////////////////////////////
	     String varName = "";											// Nome variabile
	     Object varValue = "";											// Valore da assegnare alla variabile
	     String varValueComponentName = "";							    // Nome componenete o altra variabile con il valore da impostare

	     
		 /////////////////////////////////////////////////////////////
		 // Info associate alle action da eseguire
		 /////////////////////////////////////////////////////////////
	     String actionSkipId = "";										// Identificativo Skip
	     EnumForwardEvent actionEvent = null;                           // Tipo evento di cui eseguire le action codificate
	     String actionEventComponentName = null;                        // Nome componente evento di cui eseguire le action codificate
	     
     public ForwardDoParms() {
    	 this.parms = new Object[0];
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return this.action.toString();
	}
     
}
