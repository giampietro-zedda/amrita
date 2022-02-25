package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2010-2012 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardAction
  * </h1>
  *  <p>
  * Questa enum elenca le azioni a fronte di eventi applicativi gestiti da Forward in modo dichiarativo<br>
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
public enum EnumForwardAction {
	
	NOT_ASSIGNED,       						// 000 Di servizio 
	
	// Action varie per componente
	COMPONENT_REFRESH, 							// 001 Refresh componente									 
	COMPONENT_GET_VALUE, 						// ??? Legge il valore del componente GUI  e valorizza una variabile				 
	COMPONENT_SET_VALUE, 						// ??? Imposta il valore per il componente GUI 					 
	COMPONENT_SET_FOCUS, 						// 002 Sposta il focus sul componente									 
	COMPONENT_SET_ERROR, 						// 003 Imposta un campo come errato								 
	COMPONENT_SET_FOCUSABLE, 					// 004 Il componente può ottenere il focus								 
	COMPONENT_SET_FONT,  			    		// 005 Imposta il font del componente	 
	COMPONENT_SET_TOOLTIP, 						// 006 Imposta il tooltip text al passaggio del mouse								 
	COMPONENT_SET_CHECKED,  			    	// 007 Rende checkbox/JRadioButton checked, JToggleButton ON/OFF			 
	COMPONENT_HILIGHT, 							// 008 Mette in evidenza un controllo (blinking, colore, etc.)									 
	COMPONENT_ENABLE, 							// 009 Abilita/disabilita un componente	
	COMPONENT_VISIBLE, 							// 010 Rende un componente visibile/Invisibile(form, card, panel, campo)
	COMPONENT_MASK_EVENTS,                      // 011 Rende eseguibili/non eseguibili le azioni associate a eventi, per il componente specificato
	
	// Action per comboBox (JComboBox)
	COMBOBOX_POPULATE_FROM_DB,					// 012 Esegue la logical data view della combo e la utilizza per popolare il modello della combo 
	COMBOBOX_POPULATE_DISCARD_ITEM,				// 013 Durante COMBOBOX_POPULATE_FROM_DB, a fronte di ON_COMBOBOX_BEFORE_ADD_ELEMENT, scarta l'elemento corrente  letto da db
	COMBOBOX_POPULATE_MODIFY_ITEM,				// 014 Durante COMBOBOX_POPULATE_FROM_DB, a fronte di ON_COMBOBOX_BEFORE_ADD_ELEMENT, modifica l'elemento corrente letto da db
	COMBOBOX_POPULATE_SET_OBJECT_BOUND,			// 015 Durante COMBOBOX_POPULATE_FROM_DB, a fronte di ON_COMBOBOX_BEFORE_ADD_ELEMENT, imposta l'oggetto bound
	COMBOBOX_GET_ORDINAL,						// ??? Imposta una variabile di funzione con l'ordinal 0-based selezionato della comboBox o di un valore fornito 
	COMBOBOX_GET_OBJECT_BOUND,					// 016 Imposta una variabile di funzione con l'oggetto bound
	COMBOBOX_CLEAR, 							// 017 Elimina tutti gli item della comboBox
	COMBOBOX_SELECT_ITEM, 			    		// 018 Seleziona l'item (0-based) della ComboBoX attraverso il numero di item o il valore
	COMBOBOX_UNSELECT_ITEM, 					// 019 Deseleziona l'item correntemente selezionato della ComboBoX
	COMBOBOX_APPEND_ITEM, 						// 020 Inserisce un item come ultimo elemento della ComboBoX
	COMBOBOX_INSERT_ITEM, 						// 021 Inserisce un item prima di un elemento (0-based)della ComboBoX
	COMBOBOX_DELETE_ITEM, 						// 022 Elimina un item (0-based) della ComboBoX
	COMBOBOX_DELETE_SELECTED_ITEM, 	    		// 023 Elimina l'item correntemente selezionato della ComboBoX
	COMBOBOX_UPDATE_SELECTED_ITEM, 				// 024 Aggiorna l'item correntemente selezionato della ComboBoX
	
	// Action per list (JList)
	LIST_POPULATE_FROM_DB,						// 025 Esegue la logical data view della list e la utilizza per popolare il modello della list 
	LIST_POPULATE_DISCARD_ITEM,					// 026 Durante LIST_POPULATE_FROM_DB, a fronte di ON_LIST_BEFORE_ADD_ELEMENT, scarta l'elemento corrente  letto da db
	LIST_POPULATE_MODIFY_ITEM,					// 027 Durante LIST_POPULATE_FROM_DB, a fronte di ON_LIST_BEFORE_ADD_ELEMENT, modifica l'elemento corrente letto da db
	LIST_POPULATE_SET_OBJECT_BOUND,				// 028 Durante LIST_POPULATE_FROM_DB, a fronte di ON_LIST_BEFORE_ADD_ELEMENT, imposta l'oggetto bound
	LIST_GET_OBJECT_BOUND,						// 029 Imposta una variabile di funzione con l'oggetto bound
	LIST_CLEAR, 								// 030 Elimina tutte le righe di una lista
	LIST_SELECT_ITEMS, 							// 031 Seleziona una o più righe di una lista
	LIST_APPEND_ITEM, 							// 032 Inserisce una riga alla fine di una lista
	LIST_INSERT_ITEM, 							// 033 Inserisce una riga di una lista dopo quella correntemente selezionata
	LIST_DELETE_ITEMS, 							// 034 Elimine le righe di una lista (0-based)
	LIST_DELETE_SELECTED_ITEMS, 				// 035 Elimine le righe selezionate di una lista
	LIST_UPDATE_SELECTED_ITEM, 					// 036 Aggiorna la riga selezionata di una lista 
	LIST_UPDATE_ITEM, 							// 037 Aggiorna la riga specifica di una lista (0-based)
	LIST_MAKE_VISIBLE_ITEM, 					// 038 Rende la riga non visualizzata perchè scrollata, visibile (0-based)
 
	// Action per tabelle (JTable)
	TABLE_POPULATE_FROM_DB,						// 039 Esegue la logical data view della table e la utilizza per popolare il modello della table 
	TABLE_POPULATE_DISCARD_ROW,					// 040 Durante TABLE_POPULATE_FROM_DB, a fronte di ON_TABLE_BEFORE_ADD_ROW, scarta l'elemento corrente  letto da db
	TABLE_POPULATE_MODIFY_COLUMN,				// 041 Durante TABLE_POPULATE_FROM_DB, a fronte di ON_TABLE_BEFORE_ADD_ROW, modifica l'elemento corrente letto da db
	TABLE_POPULATE_SET_OBJECT_BOUND,			// 042 Durante TABLE_POPULATE_FROM_DB, a fronte di ON_TABLE_BEFORE_ADD_ROW, imposta l'oggetto bound
	TABLE_GET_OBJECT_BOUND,						// 043 Imposta una variabile di funzione con l'oggetto bound
	TABLE_CLEAR, 								// 044 Azzera definizione e righe tabella
	TABLE_SELECT_ROWS, 							// 045 Seleziona una o più righe di una tabella
	TABLE_SELECT_COLS, 							// 046 Seleziona una o più colonne di una tabella
	TABLE_CLEAR_SELECTION,       				// 047 Elimina la selezione di righe e colonne
	TABLE_ENABLE_SELECTION_SINGLE, 				// 048 Abilita la selezione singola di righe/colonne
	TABLE_ENABLE_SELECTION_SINGLE_INTERVAL, 	// 049 Abilita la selezione di un singolo intervallo di righe/colonne
	TABLE_ENABLE_SELECTION_MULTIPLE_INTERVAL, 	// 050 Abilita la selezione di intervalli multipli di righe/colonne
	TABLE_ENABLE_SELECTION_ROWS, 				// 051 Abilita la selezione di righe
	TABLE_ENABLE_SELECTION_COLS, 				// 052 Abilita la selezione di colonne
	TABLE_ENABLE_SELECTION_CELLS, 				// 053 Abilita la selezione di celle
	TABLE_SET_CELL_TOOLTIP, 					// 054 Imposta il tooltip di colonna 
	TABLE_SET_COLUMN_TOOLTIP, 					// 055 Imposta il tooltip di colonna  
	TABLE_SET_COLUMN_RESIZABLE,                 // 056 Imposta la colonna resizable o no
	TABLE_SET_COLUMN_EDITABLE,                  // 057 Imposta la colonna editabile o no
	TABLE_SET_COLUMN_HIDDEN,                  	// 058 Imposta la colonna nascosta oppure no
	TABLE_SET_COLUMN_WIDTH,                  	// 059 Imposta la dimensione in pixel della colonna
	TABLE_SET_COLUMN_WIDTH_TO_FIT_CONTENT,      // 060 Imposta la dimensione della colonna per adattarsi al contenuto della colonna
	TABLE_SET_COLUMN_MARGIN,                  	// 061 Imposta il margine in pixel fra le colonne
	TABLE_SET_ROW_HEIGHT,                  		// 062 Imposta la dimensione in pixel delle righe
	TABLE_SET_ROW_MARGIN,                  		// 063 Imposta il margine in pixel fra le righe
	TABLE_SET_SELECTION_FOREGROUND_COLOR,       // 064 Imposta il colore foreground di selezione
	TABLE_SET_SELECTION_BACKGROUND_COLOR,       // 065 Imposta il colore background di selezione
	TABLE_SET_GRID_COLOR,       				// 066 Imposta il colore della griglia
	TABLE_SET_GRID_VISIBLE,       				// 067 Imposta la griglia con line verticali e orizzontali visibile
	TABLE_SET_VERTICAL_LINES_VISIBLE,       	// 068 Imposta la griglia con line verticali visibili
	TABLE_SET_HORIZONTAL_LINES_VISIBLE,       	// 069 Imposta la griglia con line orizzontali visibili
	TABLE_SET_FILLS_VIEWPORT_HEIGH,       		// 070 Riempe o meno tutta l'area con la tabella
	TABLE_REFRESH_VARS_FROM_PANEL,       		// 071 Aggiorna le variabili di colonna della tabella, incluso l'oggetto bound, dai campi nel/nei pannelli
	TABLE_REFRESH_PANEL_FROM_VARS,       		// 072 Aggiorna i valori della riga selezionata (VAR() di colonna) nei campi del pannello dove sono definiti
	TABLE_EDIT_CELL_AT,       					// 073 Inizia l' editing di una cella
	TABLE_PRINT,       							// 074 Stampa la tabella
	TABLE_APPEND_ROW, 							// 075 Accoda una riga alla tabella
	TABLE_INSERT_ROW, 							// 076 Inserisce una riga di una tabella alla posizione 0-based
	TABLE_DELETE_ALL_ROWS, 						// 077 Elimine tutte le righe di una tabella (0-based)
	TABLE_DELETE_ROWS, 							// 078 Elimine le righe di una tabella (0-based)
	TABLE_DELETE_SELECTED_ROWS, 				// 079 Elimine le righe selezionate di una tabella
	TABLE_UPDATE_ROW, 							// 080 Aggiorna la riga di una tabella (0-based)
	TABLE_UPDATE_SELECTED_ROWS, 				// 081 Aggiorna la/le riga selezionate di una tabella con i valori correnti delle variabili di colonna
	TABLE_UPDATE_ROW_COLUMN_CELL, 				// 082 Aggiorna la la cella individuata da numero riga e nome colonna
	TABLE_UPDATE_ROW_BOUND_OBJECT, 				// 083 Aggiorna l'oggetto binded alla riga fornita
	TABLE_ROW_DISCARD,      					// 084 Durante il popolamento di una tabella, scarta la riga corrente in fase di caricamento
	TABLE_SHOW_FIRST_PAGE,        				// 085 Visualizza la prima pagina della tabella
	TABLE_SHOW_LAST_PAGE,      					// 086 Visualizza l'ultima pagina della tabella
	TABLE_SHOW_NEXT_PAGE,      					// 087 Visualizza la pagina successiva tabella
	TABLE_SHOW_PREV_PAGE,      					// 088 Visualizza la pagina precedente della tabella

	// Action per alberi (JTree)
	TREE_POPULATE_FROM_DB,						// 089 Esegue la logical data view del tree e la utilizza per popolare il modello del tree 
	TREE_POPULATE_DISCARD,						// 090 Durante TREE_POPULATE_FROM_DB, a fronte di ON_TREE_BEFORE_ADD_ELEMENT, scarta l'elemento corrente  letto da db
	TREE_POPULATE_MODIFY,						// 091 Durante TREE_POPULATE_FROM_DB, a fronte di ON_TREE_BEFORE_ADD_ELEMENT, modifica l'elemento corrente letto da db
	TREE_POPULATE_SET_OBJECT_BOUND,				// 092 Durante TREE_POPULATE_FROM_DB, a fronte di ON_TREE_BEFORE_ADD_ELEMENT, imposta l'oggetto bound
	TREE_CLEAR,      							// 093 Eliminzazione nodi tree a parte la radice
	TREE_NODE_ADD, 								// 094 Insersce il nodo in un albero
	TREE_NODE_EXPAND, 							// 095 Espande il nodo di un albero
	TREE_NODE_COLLAPSE, 						// 096 Contrae il nodo di un albero
	TREE_NODE_SET_ICON, 						// 097 Imposta l'icona del nodo
	TREE_NODE_SET_TEXT, 						// 098 Imposta il testo della foglia
	TREE_CHILD_ADD, 							// 099 Insersce un figlio a un nodo in un albero
	TREE_CHILD_DELETE, 							// 100 Elimina un figlio a un nodo in un albero

	// Action per pannelli di dettaglio (JPanel)
	PANEL_POPULATE_FROM_DB,						// 101 Esegue la logical data view del panel e la utilizza per popolare il pannello 
	PANEL_POPULATE_COMBOBOXES_FROM_DB,			// 102 Popola ogni combobox presente nel pannello con i dati della logical data view specificata 
	PANEL_CONTROLS,            					// 103 Attiva in sequenza tutti i controlli FORMAL, EXISTENCE_ENTITY, EXISTENCE_RULE_TABLE, USER
	PANEL_CONTROLS_FORMAL,            			// 104 Attiva i controlli formali di numericità, decimali, range, max, min 
	PANEL_CONTROLS_EXISTENCE_ENTITY,            // 105 Attiva il controllo di esistenza su db di entity
	PANEL_CONTROLS_EXISTENCE_RULE_TABLE,        // 106 Attiva il controllo di esistenza su db di rule tables
	PANEL_CONTROLS_USER,  						// 107 Attiva il controllo applicativo utente di contesto sui campi del pannello   
	PANEL_DEFAULTS_INITIAL,  					// 108 Imposta i valori di default per tutti campi del pannello, come dichiarati dalla funzione   
	PANEL_DEFAULTS_USER,  					    // 109 Attiva le logiche di default applicativo del pannello   
	
	// Action struttura lyout
	LAYOUT_REMOVE_FORM, 						// 110 Rimuove un form da un pannello ovvero il pannello main e tutti quelli contenuti
	LAYOUT_REMOVE_PANELS_CHILD, 				// 111 Rimuove tutti i pannelli figli dalla struttura della funzione
	
	
	// Action di attivazione dialog stessa funzione
	DIALOG_START_USER_NO_MODAL, 				// 112 Attiva un dialogo non modale applicativa  
	DIALOG_START_USER_MODAL, 					// 113 Attiva un dialogo modale applicativa  
	DIALOG_START_PLAIN_MESSAGE, 				// 114 Attiva un dialogo modale di dialogo per messaggio informativo senza icone
	DIALOG_START_INFORMATION, 					// 115 Attiva un dialogo modale di dialogo per messaggio informativo
	DIALOG_START_WARNING, 						// 116 Attiva un dialogo modale di dialogo per messaggio warning
	DIALOG_START_ERROR, 						// 117 Attiva un dialogo modale di dialogo per messaggio warning
	DIALOG_START_QUESTION_YES_NO, 				// 118 Attiva un dialogo modale di dialogo per scelta yes/no
	DIALOG_START_QUESTION_YES_NO_CANCEL, 		// 129 Attiva un dialogo modale di dialogo per scelta yes/no/cancel
	DIALOG_START_QUESTION_OK_CANCEL, 			// 120 Attiva un dialogo modale di dialogo per scelta ok/cancel
	DIALOG_START_INPUT_BY_TEXT, 				// 121 Attiva un dialogo modale di dialogo per input valore yes/cancel
	DIALOG_START_INPUT_BY_COMBO_BOX, 			// 122 Attiva un dialogo per input valori da comboBox
	DIALOG_START_FILE_OPEN_CHOOSER,     		// 123 Attiva un dialogo modale java standard di selezione file da file system
	DIALOG_START_FILE_SAVE_CHOOSER,     		// 124 Attiva un dialogo modale java standard di selezione file da file system
	DIALOG_START_COLOR_CHOOSER,    				// 125 Attiva un dialogo modale java standard di selezione colore
	DIALOG_START_FONT_CHOOSER,    				// 126 Attiva un dialogo modale forward di selezione font
	DIALOG_CLOSE,                 				// 127 Chiusura dialogo applicativo modale o non modale
	DIALOG_CLOSE_ALL,                 			// 128 Chiusura di tutti i dialoghi aperti
	DIALOG_MOVE,                 				// 129 Move dialog nel suo spazio di coordinate
	
	// Action varie non catalogate
	EXEC_METHOD, 								// 130 Esegue metodo applicativo riusabile
	EXEC_ACTIONS_ON_EVENT,						// ??? Esegue le action codificate per un altro evento dichiarato
	EXEC_ACTIONS_GROUP,							// ??? Esegue un gruppo di action codificate
	SKIP_EVENT_ACTIONS,                         // 131 Stop di tutte le action ancora da eseguire per l'evento corrente
	SKIP_SET_ID,                         		// 132 Impostazione identificativo stop testato da SKIP_EVENT_ACTIONS
	VAR_SET_VALUE,                         		// ??? Impostazione variabile dichiarata con VAR()
	MESSAGE,                         			// ??? Recupero e impostazione messaggio codificato in rule table
	NOTHING,                         			// ??? Action dummy, per esempio per nooperizzare eventi standard
	
	// Action specifiche per logical data view
	LDV_CREATE, 								// 133 Istanzia la logical data view ed esegue il metodo declare()
	LDV_VALIDATE, 								// 134 Valida la logical data view e crea lo statement sql
	LDV_EXECUTE, 								// 135 Esegue la logical data view e crea il recordset 
	LDV_RUN, 									// 136 Esegue in sequenza create/validate/execute
	LDV_SET_PAGE_ROWS,							// 137 Imposta il numero di righe per pagina 
	LDV_SET_FUNCTION_GUI_CONTROLS,				// 138 Popolamento GUI con i valori correnti della logical data view
	LDV_SET_FUNCTION_GUI_CONTROL,				// 139 Impostazione controllo GUI con il valore corrente del campo della logical data view con lo stesso nome
	LDV_SET_FUNCTION_VARS,						// 140 Imposta la variabili della funzione con il valore delle variabili con lo stesso nome della logical data view
	LDV_SET_FUNCTION_VAR,						// 141 Impostazione variabile funzione con il valore corrente del campo della logical data view con lo stesso nome
	LDV_SET_FIELDS_KEY_ROOT_FROM_GUI, 			// 142 Imposta i campi chiave della FOR_ANY() root della logical data view da controlli GUI definiti con lo stesso nome
	LDV_SET_FIELDS_KEY_ROOT_FROM_VAR, 			// 143 Imposta i campi chiave FOR_ANY() root della logical data view da VAR() definite con lo stesso nome
	LDV_SET_FIELDS_KEY_FROM_GUI, 				// 144 Imposta i campi chiave della logical data view da controlli GUI definiti con lo stesso nome
	LDV_SET_FIELDS_KEY_FROM_VAR, 				// 145 Imposta i campi chiave della logical data view da VAR() definite con lo stesso nome
	LDV_SET_FIELDS_NOT_KEY_FROM_GUI, 			// 146 Imposta i campi NON chiave della logical data view da controlli GUI definiti con lo stesso nome
	LDV_SET_FIELDS_NOT_KEY_FROM_VAR, 			// 147 Imposta i campi NON chiave della logical data view da VAR() definite con lo stesso nome
	LDV_SET_FIELD, 								// 148 Imposta il campo, la colonna della logical data view 
	LDV_SET_VAR, 								// 149 Imposta la variabile della logical data view utilizzata come host var (equivalente a LDV_SET_FIELD)
	LDV_SET_RULE_TABLE_NUM, 					// 150 Imposta il numero tabella di rule table 
	LDV_SET_SQL_WHERE, 							// ??? Imposta la condizione di where supplementare per una specifica dichiarazione di accesso 
	LDV_SET_LANGUAGE, 							// 151 Imposta il linguaggio come ordinal di EnumLanguage, valido per accesso a rule table 
	LDV_CLEAR, 									// 152 Elimina le righe e rilascia le risorse allocate
	LDV_READROW, 								// 153 Legge una riga specifica aggiornando le variabili applicative con i valori correnti
	LDV_READFIRST, 							    // 154 Legge la prìma riga aggiornando le variabili applicative con i valori correnti
	LDV_READLAST, 								// 155 Legge l'ultima riga aggiornando le variabili applicative con i valori correnti
	LDV_READNEXT, 								// 156 Legge la riga successiva a quella corrente aggiornando le variabili applicative con i valori correnti
	LDV_READPREV, 								// 157 Legge la riga precedente a quella corrente aggiornando le variabili applicative con i valori correnti
	LDV_UPDATE, 								// 158 Aggiorna tutta la logical data view o solo l'entity specificata con i valori correnti delle variabili applicative
	LDV_INSERT, 								// 159 Inserisce l'entity specificata con i valori correnti delle variabili applicative
	LDV_DELETE, 								// 160 Deleta l'entity specificata con i valori correnti delle variabili applicative contenenti le chiavi
	LDV_DB_GET_CONNECTION, 						// 161 Acquisisce una nuova connessione al database e memorizza il reference nella logical data view
	LDV_DB_RELEASE_CONNECTION, 					// 162 Rilascia la connessione corrente al database e azzera il reference nella logical data view
	LDV_DB_SET_ACCESS_PARMS, 					// 163 Imposta parametri generali di accesso al data base come limitazione al numero righe, commit automatica, etc.
	LDV_DB_COMMIT, 								// 164 Effettua una commit al data base
	LDV_DB_ROLLBACK, 							// 165 Effettua una rollback al data base
 
	
	// Action di controllo funzione 
	// Action di attivazione nuova funzione o nuovo form in funzione corrente
	FUNCTION_LOAD,                				// 166 Carica la funzione applicativa per successiva start della stessa
	FUNCTION_START, 							// 167 Attiva una funzione applicativa che rimpiazza temporaneamente quella corrente
	FUNCTION_START_LOOKUP_TABLE, 				// 168 Attiva una funzione modale di lookup per ricerca e restituzione valori da tabelle forward
	FUNCTION_START_SHOW_LDV_ERROR,    			// 169 Attiva una funzione modale di visualizzazione informazioni di errore di accesso ai dati 
	FUNCTION_START_SHOW_EXCEPTION_ERROR,        // 170 Attiva una funzione modale di visualizzazione informazioni di errore di exception
	FUNCTION_START_SHOW_MESSAGES,           	// 171 Attiva una funzione modale modale di visualizzazione messaggi di errore e/o informativi a fronte di controlli
	FUNCTION_XCTL, 								// 172 Attiva una nuova funzione e gli trasferisce il controllo se nza incrementare lo stack
	FUNCTION_GET_LANGUAGE,                 		// ??? Imposta la variabile della funzione con il valore corrente del linguaggio, una enumerazione EmumLanguage 
	FUNCTION_SET_PARM_REQUIRED,                 // 173 Imposta la variabile della funzione chiamata dichiarata con PARM_REQUIRED() con un valore costante o il contenuto di una variabile del chiamante 
	FUNCTION_SET_PARM_RETURNED,                	// 174 Imposta la variabile della funzione chiamante con il parametro della funzione chiamata dichiarato con PARM_RETURNED()) 
	FUNCTION_SET_PARMS_RETURNED_INITIAL,        // 175 Imposta tutte le variabile definite con PARM_RETURNED()) al loro valore di default
	FUNCTION_SET_VAR,                			// 176 Imposta la variabile della funzione dichiarata con VAR() con un valore costante o il contenuto di un control GUI
	FUNCTION_SET_GUI_CONTROL,                	// 177 Imposta il controllo GUI con un valore costante o con il contenuto di una variabile di funzione
	FUNCTION_RETURN,        					// 178 Esce dalla funzione corrente e restituisce il controllo al chiamante, restituendo eventuali valori
	FUNCTION_QUIT, 								// 179 Esce dalla funzione corrente e restituisce il controllo al sistema	
	FUNCTION_RELEASE, 							// 180 Rilascia tutte le risorse della funzione	
	
	// Action di sistema
	SYSTEM_BEEP,        						// 181 Beep di sistema
	SYSTEM_TIMER_START,        					// 182 Starta il timer
	SYSTEM_TIMER_STOP,        					// 183 Stoppa il timer
	SYSTEM_TIMER_DELAY,        					// 184 Immposta il delay iniziale e quello di intervallo in millisecondi
	
}












