package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2010-2012 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardEvent
  * </h1>
  *  <p>
  * Questa enum elenca gli eventi applicativi gestiti da Forward in modo dichiarativo,<br>
  * a livello di dichiarazione della funzione applicativa.<br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardEvent {
	
	NOT_ASSIGNED,       						// 000 Di servizio 
	
	// Eventi standard di java
	ON_JAVA_ACTION_PERFORMED, 					// 001 A fronte di click su JButton, JCheckBox, JComboBox, JTextField, JRadioButton 									 
	ON_JAVA_FOCUS_GAINED, 						// 002 A fronte di focus ottenuto da un oggetto								 
	ON_JAVA_FOCUS_LOST, 						// 003 A fronte di focus perso da un oggetto							 
	ON_JAVA_ITEM_STATE_CHANGED, 				// 004 A fronte di selezione/deselezione JButton, JCheckBox, JComboBox, JTextField, JRadioButton 									 
	ON_JAVA_VALUE_CHANGED, 						// 005 A fronte di ListSelectionListener per selezione/deselezione item di JList								 
	ON_JAVA_STATE_CHANGED, 						// 006 A fronte di ChangeListener per variazione valore di JSlider								 
	ON_JAVA_PROPERTY_CHANGED, 					// 007 A fronte di PropertyChangeListener per variazione valore di una property								 
	ON_JAVA_ADJUSTMENT_VALUE_CHANGED, 			// 008 A fronte di spostamento di JScrollBar con le frecce o con un click									 
	ON_JAVA_KEY_PRESSED, 						// 009 A fronte di pressione key sulla tastiera									 
	ON_JAVA_KEY_TYPED, 							// 010 A fronte di digitazione key sulla tastiera									 
	ON_JAVA_KEY_RELEASED, 						// 011 A fronte di rilascio key sulla tastiera									 
	ON_JAVA_MOUSE_CLICKED, 						// 012 A fronte di click su un componenente (anche double click)									 
	ON_JAVA_MOUSE_ENTERED, 						// 013 A fronte di spostamento del mouse nell'area del componente							 
	ON_JAVA_MOUSE_EXITED, 						// 014 A fronte di uscita del mouse nell'area del componente							 
	ON_JAVA_MOUSE_PRESSED, 						// 015 A fronte di pressione del mouse (click)						 
	ON_JAVA_MOUSE_RELEASED, 					// 016 A fronte di rilascio del mouse (da click)						 
	ON_JAVA_MOUSE_DRAGGED, 						// 017 A fronte di movimento del puntatore del mouse sopra un componente					 
	ON_JAVA_MOUSE_MOVED, 						// 018 A fronte di movimento del puntatore del mouse fuori da un componente					 
	ON_JAVA_WHEEL_MOVED, 						// 019 A fronte di rotazione rotella del mouse				 
	ON_JAVA_CHANGE_UPDATE, 						// 020 A fronte di DocumentListener su update caratteri in JText...					 
	ON_JAVA_INSERT_UPDATE, 						// 021 A fronte di DocumentListener su insert caratteri in JText...					 
	ON_JAVA_REMOVE_UPDATE, 						// 022 A fronte di DocumentListener su delete caratteri in JText...					 
	ON_JAVA_WINDOW_ACTIVATED, 					// 023 A fronte dell'apertura di una finestra				 
	ON_JAVA_WINDOW_CLOSED, 						// 024 A fronte della chiusura di una finestra, quando è stata chiusa			 
	ON_JAVA_WINDOW_CLOSING, 					// 025 A fronte della chiusura di una finestra, durante la chiusura			 
	ON_JAVA_WINDOW_DEACTIVATED, 				// 026 A fronte di disattivazione della finestra		 
	ON_JAVA_WINDOW_DEICONIFIED, 				// 027 A fronte di deiconificazione della finestra		 
	ON_JAVA_WINDOW_ICONIFIED, 					// 028 A fronte di iconificazione della finestra		 
	ON_JAVA_WINDOW_OPENED, 						// 029 A fronte di apertura della finestra		 
	ON_JAVA_TREE_NODES_CHANGED, 				// 030 A fronte di TreeModelListener a fine editing testo nodo di JTree	 
	ON_JAVA_TREE_NODES_INSERTED, 				// 031 A fronte di TreeModelListener a insert nodo di JTree	 
	ON_JAVA_TREE_NODES_REMOVED, 				// 032 A fronte di TreeModelListener a remove nodo di JTree	 
	ON_JAVA_TREE_STRUCTURE_CHANGED, 			// 033 A fronte di TreeModelListener a cambiamento struttura di JTree	 
	ON_JAVA_TREE_VALUE_CHANGED, 				// 034 A fronte di TreeSelectionListener su selezioni nodi o remove di JTree	 
	ON_JAVA_TREE_EXPANDED, 						// 035 A fronte di TreeExpansionListener su espansione nodo di JTree	 
	ON_JAVA_TREE_COLLAPSED, 					// 036 A fronte di TreeExpansionListener su contrazione nodo di JTree	 
	
	// Eventi applicativi classificati per qualsiasi componente
	ON_CLICK,  									// 037 Equivalente a ON_JAVA_ACTION_PERFORMED     									 
	ON_DOUBLE_CLICK,  							// 038 A fronte di mouse double click su un componente (gestito con ON_JAVA_MOUSE_CLICKED)        									 
	ON_TEXT_MODIFIED, 							// 039 A fronte di campo modificato, al lost focus o enter   									 
	ON_COMPONENT_INITIAL, 						// 040 Dopo inzializzazione standard componente (function, panel, control)									 
	
	// Eventi applicativi classificati per menu (comunque implementati)
	ON_MENU_ITEM_SELECTED,  					// 041 A fronte di selezione di un item foglia di menu (click su button, tre, list, ..) 									 
	ON_MENU_ITEM_CHECKED,  			    		// 042 A fronte di selezione di un item foglia di menu (CheckBoxMenuItem, RadioButtonMenuItem.) 									 
	ON_MENU_ITEM_UNCHECKED,  					// 043 A fronte di selezione di un item foglia di menu (CheckBoxMenuItem, RadioButtonMenuItem.) 									 
	
	// Eventi applicativi classificati per componenti di stato (JCheckBox, JOptionBox, JToggleButton)
	ON_CHECKBOX_CHECKED,  			    		// 044 A fronte di checkbox da uncheck a checked									 
	ON_CHECKBOX_UNCHECKED,  					// 045 A fronte di checkbox da check a unchecked										 
	ON_RADIOBUTTON_CHECKED,  			    	// 046 A fronte di radioButton da uncheck a checked									 
	ON_RADIOBUTTON_UNCHECKED,  					// 047 A fronte di radioButton da check a unchecked										 
	ON_BUTTON_TOGGLED,  			    		// 048 A fronte di togglebutton da untoggled a toggled									 
	ON_BUTTON_UNTOGGLED,  						// 049 A fronte di togglebutton da toggled a untoggled											 

	// Eventi applicativi classificati per selettori di valori/pannelli (JSlider, JSpinner, JTabbedPane)
	ON_SLIDER_TICKED, 							// 050 A fronte di selezione nuovo numero con JSlider
	ON_SPINNER_TICKED, 							// 051 A fronte di selezione nuovo testo/numero/data con JSpinner
	ON_TABBED_PANE_TICKED, 						// 052 A fronte di selezione nuovo panel con JTabbedPane
	
	// Eventi applicativi classificati per componenti di gestione testo (JTextField, JFormattedTextField, JPassword)
	ON_CHARS_UPDATE, 							// 053 A fronte di inserimento caratteri in controllo JText...
	ON_CHARS_INSERT, 							// 054 A fronte di inserimento caratteri in controllo JText...
	ON_CHARS_DELETE, 							// 055 A fronte di delete caratteri in controllo JText...

	// Eventi applicativi classificati per per liste (JComboBox)
	ON_COMBOBOX_BEFORE_POPULATE, 				// 056 Prima di inserimento primo item nella combobox, per inserimento manuale items a inizio combo				 
	ON_COMBOBOX_AFTER_POPULATE, 				// 057 Dopo il caricamento degli item della combobox							 
	ON_COMBOBOX_AFTER_CLEAR, 					// 058 Dopo l'eliminazione degli item della combobox e prima del popolamento 							 
	ON_COMBOBOX_BEFORE_ADD_ELEMENT, 			// 059 Prima di inserimento item nella combobox, principalemte in fase di populate per discard o modify					 
	ON_COMBOBOX_SELECTED_ITEM,  				// 060 A fronte di combo box, item selezionato con click o freccia up/down/left/right/->									 
	
	// Eventi applicativi classificati per per liste (JList)
	ON_LIST_BEFORE_POPULATE, 					// 061 Prima del caricamento prima riga della list, per inserimento manuale righe a inizio combo									 
	ON_LIST_AFTER_POPULATE, 					// 062 Dopo il caricamento delle righe della list							 
	ON_LIST_AFTER_CLEAR, 						// 063 Dopo l'eliminazione delle righe della list e prima del popolamento 								 
	ON_LIST_BEFORE_ADD_ELEMENT, 				// 064 Prima di inserimento item nella lista, principalemte in fase di populate	per discard o modify					 
	ON_LIST_ROWS_SELECTED,              		// 065 A fronte di selezione di una o più item di una lista (swing JList) 									 
	ON_LIST_ROWS_UNSELECTED,            		// 066 A fronte di deselezione di una o più item di una lista (swing JList) 									 
	
	// Eventi applicativi classificati per tabelle (JTable)
	ON_TABLE_BEFORE_POPULATE, 					// 067 Prima di caricamento prima riga di unatabella									 
	ON_TABLE_AFTER_POPULATE, 					// 068 Dopo il caricamento di una tabella									 
	ON_TABLE_AFTER_CLEAR, 						// 069 Dopo l'eliminazione delle righe della tabella									 
	ON_TABLE_BEFORE_ADD_ROW, 					// 070 Prima di inserimento riga nella tabella, principalemte in fase di populate per discard o modify					 
	ON_TABLE_ROWS_SELECTED, 					// 071 A fronte di selezione di una o più righe di una tabella    									 
	ON_TABLE_ROWS_UNSELECTED, 					// 072 A fronte di deselezione di una o più righe di una tabella   
	ON_TABLE_COLS_SELECTED, 					// 073 A fronte di selezione di una o più colonne di una tabella    									 
	ON_TABLE_COLS_UNSELECTED, 					// 074 A fronte di deselezione di una o più colonne di una tabella   
	ON_TABLE_CELLS_SELECTED, 					// 075 A fronte di selezione di una o più celle di una tabella    									 
	ON_TABLE_CELLS_UNSELECTED, 					// 076 A fronte di deselezione di una o più celle di una tabella   

	// Eventi applicativi classificati per alberi (JTree)
	ON_TREE_AFTER_POPULATE, 					// 077 Dopo il caricamento dell'albero								 
	ON_TREE_AFTER_CLEAR, 						// 078 Dopo l'eliminazione dei nodi dell'albero							 
	ON_TREE_BEFORE_ADD_NODE, 					// 079 Prima di inserimento nodo nell'albero, principalemte in fase di populate per discard o modify					 
	ON_TREE_NODE_EDITED_TEXT,             		// 080 A fronte di change text nodo da parte dell'utente
	ON_TREE_NODE_INSERTED,             			// 081 A fronte di inserimento di un nuovo nodo
	ON_TREE_NODE_REMOVED,             			// 082 A fronte di remove di un nodo
	ON_TREE_NODE_EXPANDED, 						// 083 A fronte di TreeExpansionListener su espansione nodo di JTree	 
	ON_TREE_NODE_COLLAPSED, 					// 084 A fronte di TreeExpansionListener su contrazione nodo di JTree	 
	ON_TREE_NODE_SELECTED, 						// 085 A fronte di selezione nodo di JTree con click o freccia up/down 
	ON_TREE_STRUCTURE_CHANGED,             		// 086 A fronte di cambiamento struttura di JTree
	ON_TREE_NODE_CHILDREN_TO_ADD, 				// 087 A fronte di TreeExpansionListener su prima espansione nodo con child di servizio di JTree	 
	
	// Eventi applicativi classificati legati alla gestione dei pannelli
	ON_PANEL_AFTER_DEFAULTS_INITIAL,  						// 088 Dopo l'applicazione dei valori di default a ogni controllo del pannello 									 
	ON_PANEL_AFTER_CONTROLS_DECLARING, 						// 089 Dopo il caricamento dei controlli swing nel pannello								 
	ON_PANEL_BEFORE_FIRST_SHOW, 							// 090 Prima della prima show del pannello, per popolamento combobox etc.							 
	ON_PANEL_AFTER_POPULATE, 								// 091 Dopo il popolamento del pannello, attraverso la vista logica di accesso ai dati									 
    ON_PANEL_CONTROLS_WITH_ERRORS,              			// 092 Dopo esecuzione di qualsiasi action di PANEL_CONTROLS_XXX ed errori riscontrati
    ON_PANEL_CONTROLS_WITH_NO_ERRORS,           			// 093 Dopo esecuzione di qualsiasi action di PANEL_CONTROLS_XXX e nessun erore riscontrato
    ON_PANEL_CONTROLS_FORMAL_WITH_ERRORS,            		// 094 Dopo l'esecuzione di PANEL_CONTROLS_FORMAL ed errori riscontrati
    ON_PANEL_CONTROLS_FORMAL_WITH_NO_ERRORS,            	// 095 Dopo l'esecuzione di PANEL_CONTROLS_FORMAL e nessun errore riscontrato
    ON_PANEL_CONTROLS_EXISTENCE_ENTITY_WITH_ERRORS,         // 096 Dopo l'esecuzione di PANEL_CONTROLS_EXISTENCE_ENTITY ed errori riscontrati
    ON_PANEL_CONTROLS_EXISTENCE_ENTITY_WITH_NO_ERRORS,      // 097 Dopo l'esecuzione di PANEL_CONTROLS_EXISTENCE_ENTITY e nessun errore riscontrato
    ON_PANEL_CONTROLS_EXISTENCE_RULE_TABLE_WITH_ERRORS,     // 098 Dopo l'esecuzione di PANEL_CONTROLS_EXISTENCE_RULE_TABLE ed errori riscontrati
    ON_PANEL_CONTROLS_EXISTENCE_RULE_TABLE_WITH_NO_ERRORS,	// 099 Dopo l'esecuzione di PANEL_CONTROLS_EXISTENCE_RULE_TABLE e nessun errore riscontrato
    ON_PANEL_CONTROLS_USER_WITH_ERRORS,  					// 100 Dopo l'esecuzione di PANEL_CONTROLS_USER ed errori riscontrati
    ON_PANEL_CONTROLS_USER_WITH_NO_ERRORS,  				// 101 Dopo l'esecuzione di PANEL_CONTROLS_USER e nessun errore riscontrato

	// Eventi applicativi classificati per LookUp table dialog
	ON_LOOkUP_TABLE_ROWS_SELECTED, 				// 192 A fronte di selezione di una o più righe di una forward table attivata con dialogo di LookUp    									 

	// Eventi specifici per dialoghi
	ON_DIALOG_BEFORE_CLOSING,					// 102 A fronte di selezione opzione pannello di dialogo, prima della chiusura e prima dell'analisi delle opzioni
	ON_DIALOG_CLOSING_ATTEMPTED,				// 103 A fronte di tentativo di chiusura pannello di dialogo modale con X
	ON_DIALOG_YES_OK_OPTION,					// 104 A fronte di chiusura pannello di dialogo con tasto YES oppure OK
	ON_DIALOG_NO_OPTION,						// 105 A fronte di chiusura pannello di dialogo con tasto NO
	ON_DIALOG_CANCEL_OPTION,					// 106 A fronte di chiusura pannello di dialogo con tasto CANCEL
	ON_DIALOG_START, 							// 107 Prima dell'attivazione di un dialogo (form) nella funzione corrente
	ON_DIALOG_RETURN, 							// 108 A fronte di return da visualizzazione, selezione opzione e chiusura pannello di dialogo

	// Eventi a fronte di selezione file
	ON_FILE_CHOOSER_CANCEL_OPTION,				// 109 A fronte di Cancel in dialogo di selezione file
	ON_FILE_CHOOSER_APPROVE_OPTION,				// 110 A fronte di Approve in dialogo di selezione file
	ON_FILE_CHOOSER_ERROR_OPTION,				// 111 A fronte di Error in dialogo di selezione file
	
	// Eventi a fronte di selezione colore e font
	ON_COLOR_CHOOSED,							// 112 A fronte di dialogo selezione colore con schelta dello stesso
	ON_FONT_CHOOSED,							// 113 A fronte di dialogo selezione colore con schelta dello stesso
	
	// Eventi applicativi classificati per gestione funzioni
	ON_FUNCTION_AFTER_LOAD, 					// 114 Dopo il caricamento  di una nuova funzione
	ON_FUNCTION_BEFORE_START, 					// 115 Prima dell'attivazione automatica di una nuova funzione, principalmente per lookup function
	ON_FUNCTION_INITIAL, 						// 116 Dopo l'inizializzazione della funzione, come prima operazione
	ON_FUNCTION_RETURN, 					    // 117 A fronte di return da funzione richiamata, prima di cedere il controllo alla funzione chiamante
	ON_FUNCTION_RETURNED, 					    // 118 A fronte di return da funzione richiamata, dopo aver ceduto il controllo nuovamente alla funzione chiamante

	// Eventi associati alle viste logiche
	ON_LDV_BEFORE_EXECUTE,                      // 119 Prima dell'esecuzione di una vista logica (Per valorizzazione variabili vista logica)
	ON_LDV_BEFORE_GET,                          // 120 Prima dell'esecuzione di una read(), ..., readLst() della vista logica
	ON_LDV_BEFORE_INSERT,                       // 121 Prima dell'esecuzione di una INSERT della vista logica
	ON_LDV_BEFORE_UPDATE,                       // 122 Prima dell'esecuzione di una UPDATE della vista logica
	ON_LDV_BEFORE_DELETE,                       // 123 Prima dell'esecuzione di una DELETE della vista logica
	ON_LDV_AFTER_READ,                          // 124 Dopo l'esecuzione di una READxxx della vista logica
	ON_LDV_AFTER_INSERT,                        // 125 Dopo l'esecuzione di una INSERT della vista logica OK
	ON_LDV_AFTER_UPDATE,                        // 126 Dopo l'esecuzione di una UPDATE della vista logica OK
	ON_LDV_AFTER_DELETE,                        // 127 Dopo l'esecuzione di una DELETE della vista logica OK
	ON_LDV_AFTER_EXECUTE,                       // 128 Dopo l'esecuzione della vista logica di generazione recordset
	ON_LDV_ERROR_EXECUTION,						// 129 A fronte di errore in create/validate/execute vista logica sia Sql/reflection con exception e senza
	ON_LDV_INSERT_DUPLICATE,					// 130 A fronte di riga esistente da inserire
	ON_LDV_DELETE_NOTFOUND,						// 131 A fronte di riga inesistente da deletare
	ON_LDV_UPDATE_NOTFOUND,						// 132 A fronte di riga inesistente da aggiornare
	ON_LDV_READ_NOTFOUND,						// 133 A fronte di LDV_READROW, LDV_READFIRST, LDV_READNEXT, LDV_REAPREV, LDV_READLAST e nessuna eiga da restituire
	ON_LDV_NO_ROWS,								// 134 A fronte di LDV_RUN o LDV_EXECUTE logical data view e nessuna riga da restituire 
  
	// Eventi su aggiornamenti db complessivi (che potrebbero implicare LDV multiple)
	ON_UPDATES_DB_BEFORE,						// 135 Prima degli aggiornamenti su db complessivi  	
	ON_UPDATES_DB_AFTER,						// 136 Dopo gli aggiornamenti su db complessivi  	

	// Eventi applicativi classificati legati ai form applicativi dichiarati
	ON_FORM_AFTER_LOAD, 						// 137 Dopo il caricamento iniziale del form 
	
	// Eventi di sistema e in generale di esecuzione, esclusi gli errori sul LDV
	ON_SYSTEM_TIMER_EXPIRED,					// 138 A fronte del trascorrere di n millisecondi
	ON_SYSTEM_EXCEPTION_FUNCTION,				// 139 A fronte di exception in codice applicativo applicativo, anche di dichiarazione
	ON_SYSTEM_EXCEPTION_MONITOR,				// 140 A fronte di exception in codice di sistema (monitor,..)
    ON_ERROR,                             		// 141 A fronte di errore generico codificato o senza codifica
 }
