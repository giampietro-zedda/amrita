package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - ing. Giampietro Zedda    Turin (italy)
  *
  * <h1>
  * EnumForwardPanelOption
  * </h1>
  *  <p>
  * This enum lists, for each panel type, options and characteristics that's possible<br>
  * to choose in function declaring.<br>
  * Each option qualifies the pannel or add a characteristic or a behaviour.<br>
  * <p>
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardOption {
	
	NOT_ASSIGNED,           				// 000 Di servizio 
	
	// Opzioni function
	FUNCTION_CENTER_SCREEN,					// 000 Funzione al centro dello schermo
	FUNCTION_CENTER_FRAME_OWNER,			// 001 Funzione al centro del frame di attivazione
	FUNCTION_UNDECORATED,					// 002 Funzione senza decorazione frame, barra del titolo e icone di minimizzazione
	FUNCTION_INQUIRY,           			// 003 Funzione di solo inquiry 
	FUNCTION_UPDATE,           				// 003 Funzione con aggiornamenti abilitati
	FUNCTION_PROTOTYPE_INQUIRY,           	// 004 Funzione di solo inquiry di prototipo 
	FUNCTION_PROTOTYPE_UPDATE,           	// ??? Funzione con aggiornamenti abilitati di prototipo 
	
	// Opzioni pannello griglia, realizzata con JTable
	GRID_INQUIRY,           				// 001 Solo visualizzazione dati
	GRID_UPDATE,           			    	// 002 Visualizzazione e aggiornamenti
	GRID_UPDATE_INS,           				// 003 Aggiornamento di inserimento righe abilito
	GRID_UPDATE_UPD,           				// 004 Aggiornamento di update righe abilito
	GRID_UPDATE_DEL,           				// 005 Aggiornamento di delete righe abilito
	GRID_UPDATES_DEFERRED,           		// 006 Aggiornamenti fisici effettuati in modo cumulativo
	GRID_CTRL_CHANGED_VALUES,    			// 007 Aggiornamenti fisici effettuati in modo cumulativo e controllo variazione valori
	GRID_SELECTION_ROW_MULTIPLE,    		// 008 Aggiornamenti fisici effettuati in modo cumulativo e controllo variazione valori
	
	// Opzioni pannello splittato, realizzato con JSplitPane
	SPLIT_HORIZONTAL,           			// 009 Due pannelli orizzontali
	SPLIT_VERTICAL,           				// 010 Due pannelli verticali
	
	// Opzioni pannello toolbar, realizzata con JToolbar
	TOLBAR_HORIZONTAL,           			// 011 Tolbar disposta verticalmente
	TOLBAR_VERTICAL,           				// 012 Tolbar disposta orizzontalmente
	TOLBAR_FLOATING,           				// 013 Toolbar removibile

	// Opzioni pannello menu, realizzato in vari modi
	MENU_RENDERING_MENUBAR,           		// 014 Menubar classico associato al JFrame in alto 
	MENU_RENDERING_TOOLBAR,           		// 015 Toolbar 
	MENU_RENDERING_PLAIN_BUTTONS,     		// 016 Semplici pulsanti a uno più livelli
	MENU_RENDERING_SECTIONS_HIDE,      	 	// 018 Sezioni menù a scomparsa nello stesso livello per menu verticali di due livelli
	MENU_TYPE_ROOT,         				// 019 Menu di primo livello
	MENU_TYPE_SUBMENU,         				// 020 Sottomenu di qualsiasi tipo di menu di primo livello
	MENU_TYPE_POPUP,         				// 021 PopUp menu da attivare con pulsante destro
	MENU_DIRECTION_VERTICAL, 				// 022 Disposto verticalmente di solito a sinistra									 
	MENU_DIRECTION_HORIZONTAL, 				// 023 Disposto orizzontalmente di solito in alto
	MENU_HEIGH_EXTRA_THIN,         	    	// 024 Pulsanti ultra sottili
	MENU_HEIGH_THIN,         				// 025 Pulsanti sottili
	MENU_HEIGH_MEDIUM,         				// 026 Pulsanti di altezza media
	MENU_HEIGH_LARGE,         				// 027 Pulsanti di altezza grande
	MENU_STYLE_AUTOMATIC,      				// 028 Stile di default scelto da forward
	MENU_STYLE_CUSTOM,      				// 029 Stile completamente personalizzato dalla funzione
	MENU_STYLE_JAVA_DEFAULT,      			// 030 Stile completamente personalizzato dalla funzione
	MENU_STYLE1,      						// 031 Stile Arancione su sfondo grigio senza bordo si rollover
	MENU_STYLE2,      						// 032 Stile Arancione su sfondo grigio con bordo bianco no rollover
	MENU_STYLE3,      						// 033 Stile ??
	MENU_STYLE4,      						// 034 Stile ??
	MENU_STYLE5,      						// 035 Stile ??
	MENU_STYLE6,      						// 036 Stile ??
	MENU_STYLE7,      						// 037 Stile ??
	MENU_STYLE8,      						// 038 Stile ??
	MENU_STYLE9,      						// 039 Stile ??
	MENU_STYLE10,      						// 040 Stile ??
	
    // Opzioni form
	FORM_TYPE_START, 						// 041 Form iniziale della funzione, direttamente disposto nel pannello dei contenuti principale								 
	FORM_TYPE_DIALOG, 		    			// 042 Form di dialogo modale o meno attivato a fronte logica applicativa								 
	FORM_TYPE_DECLARED, 					// 043 Form dichiarato e disposto su un pannello									 
	
	// Opzioni per Dialog
	DIALOG_ON_CENTER_SCREEN, 				// 044 Form di dialogo modale o meno attivato posizionato a centro schermo								 
	DIALOG_ON_CENTER_PARENT, 				// 045 Form di dialogo modale o meno attivato posizionato a centro parent 								 
	DIALOG_AT_X_Y, 							// 046 Form di dialogo posizioinato alle coordinate X, Y							 
	
	// Controlli da effettuare sul componente
	COMPONENT_CTRL_FORMAL,					// 047 Indica se il campo deve essere sottoposto ai controlli formali successivi
	COMPONENT_CTRL_VALUES,              	// 048 Indica se il campo deve essere sottoposto al controllo di esistenza in un insieme di valori 
	COMPONENT_CTRL_MASK,					// 049 Indica se il campo deve essere sottoposto al controllo attraverso una maschera generica
	COMPONENT_CTRL_TEXT_SIZE_MIN,			// 050 Indica se il campo deve essere sottoposto al controllo di lunghezza, numero minimo di caratteri
	COMPONENT_CTRL_TEXT_SIZE_MAX,			// 051 Indica se il campo deve essere sottoposto al controllo di lunghezza, numero massimo di caratteri
	COMPONENT_CTRL_TEXT_SIZE_RANGE,			// 052 Indica se il campo deve essere sottoposto al controllo di lunghezza, range di numero di caratteri, estremi inclusi
	COMPONENT_CTRL_TEXT_VALUE_MAX,			// 053 Indica se il campo deve essere sottoposto al controllo di numericita, valore parte intera massimo
	COMPONENT_CTRL_TEXT_VALUE_MIN,			// 054 Indica se il campo deve essere sottoposto al controllo di numericita, valore parte intera minima
	COMPONENT_CTRL_TEXT_VALUE_RANGE,		// 055 Indica se il campo deve essere sottoposto al controllo di numericita, valore parte intera in range inclusi estremi
	COMPONENT_CTRL_TEXT_UPPERCASE,			// 056 Indica se il campo deve essere sottoposto al controllo di esistenza di soli caratteri uppercase
	COMPONENT_CTRL_TEXT_LOWERCASE,			// 057 Indica se il campo deve essere sottoposto al controllo di esistenza di soli caratteri lowercase
	COMPONENT_CTRL_TEXT_ALPHABETIC,			// 058 Indica se il campo deve essere sottoposto al controllo di esistenza di soli caratteri alfabetici
	COMPONENT_CTRL_TEXT_NUMERIC,			// 059 Indica se il campo deve essere sottoposto al controllo di esistenza di soli caratteri numerici
	COMPONENT_CTRL_DATE,					// 060 Indica se il campo deve essere sottoposto al controllo formale data
	COMPONENT_CTRL_DATE_MAX,				// 061 Indica se il campo deve essere sottoposto al controllo formale data, valore massimo
	COMPONENT_CTRL_DATE_MIN,				// 062 Indica se il campo deve essere sottoposto al controllo formale data, valore minimo
	COMPONENT_CTRL_DATE_RANGE,				// 063 Indica se il campo deve essere sottoposto al controllo formale data, valore compreso in range
	COMPONENT_CTRL_NUMERIC_VALUE_MAX,		// 064 Indica se il campo deve essere sottoposto al controllo di numericita, valore massimo
	COMPONENT_CTRL_NUMERIC_VALUE_MIN,		// 065 Indica se il campo deve essere sottoposto al controllo di numericita, valore minimo
	COMPONENT_CTRL_NUMERIC_VALUE_RANGE,		// 066 Indica se il campo deve essere sottoposto al controllo di numericita, valore in range inclusi estremi
	COMPONENT_CTRL_NUMERIC_VALUE_INT_MAX,	// 067 Indica se il campo deve essere sottoposto al controllo di numericita, valore parte intera massimo
	COMPONENT_CTRL_NUMERIC_VALUE_INT_MIN,	// 068 Indica se il campo deve essere sottoposto al controllo di numericita, valore parte intera minima
	COMPONENT_CTRL_NUMERIC_VALUE_INT_RANGE,	// 069 Indica se il campo deve essere sottoposto al controllo di numericita, valore parte intera in range inclusi estremi
	COMPONENT_CTRL_NUMERIC_VALUE_DEC_MAX,	// 070 Indica se il campo deve essere sottoposto al controllo di numericita, valore parte decimale massimo
	COMPONENT_CTRL_NUMERIC_VALUE_DEC_MIN,	// 071 Indica se il campo deve essere sottoposto al controllo di numericita, valore parte decimale minima
	COMPONENT_CTRL_NUMERIC_VALUE_DEC_RANGE,	// 072 Indica se il campo deve essere sottoposto al controllo di numericita, valore parte decimale in range inclusi estremi
	COMPONENT_CTRL_MANDATORY,				// 073 Indica se il campo deve essere sottoposto al controllo di  obbligatorietà
    
	// Controlli di esistenza da effettuare sul componente
	COMPONENT_CTRL_EXISTS_RULE_TABLE,		// 074 Indica se il campo deve essere sottoposto al controllo di esistenza nel sistema tabelle
	COMPONENT_CTRL_UNEXISTS_RULE_TABLE,		// 075 Indica se il campo deve essere sottoposto al controllo di NON esistenza nel sistema tabelle
	COMPONENT_CTRL_EXISTS_ENTITY,			// 076 Indica se il campo deve essere sottoposto al controllo di esistenza entity via logical data view
	COMPONENT_CTRL_UNEXISTS_ENTITY,			// 077 Indica se il campo deve essere sottoposto al controllo di NON esistenza entity via logical data view

	// Opzioni e flag di controllo componente
	COMPONENT_WITH_ERRORS,					// 078 Indica se il campo è  in errore, come impostato dall'applicazione 
	COMPONENT_GOOD_WITH_ERRORS,				// 079 Indica se il campo è  da considerarsi valido anche se con errore
	COMPONENT_HIDDEN,						// 080 Indica se il campo è  non visibile
	COMPONENT_DISABLED,						// 081 Indica se il campo è  non enabled
	COMPONENT_AUTOSKIP,						// 082 Indica se il campo è  bypassato dal tasto Tab 
	COMPONENT_HILIGHT,						// 083 Indica se il campo è  evidenziato rispetto agli altri
	COMPONENT_HELP_TECH_ACTIVE,				// 084 Indica se è attivo l'help contestuale sul campo tecnico
	COMPONENT_HELP_USER_ACTIVE,				// 085 Indica se è attivo l'help contestuale sul campo applicativo funzionale

	
	//>>>> fare man mano
	
	// Opzioni pannello tabbed, realizzato con JTabbedPane

	
	DETAIL,           						// 086 Campi e oggetti grafici di dettaglio definiti nella funzione
	LIST,           						// 087 Elenco di informazioni realizzato con JList
	TREE,           						// 088 Albero di informazioni realizzato con JTree
	GRID,           						// 089 Albero di informazioni realizzato con JTree
	BUTTON_GRID_PILOT,           			// 090 Pulsanti standard di navigazione/update su una griglia
	PROPERTIES,           					// 091 Pannello di proprietà con due colonne
	TABLE_LIST,           					// 092 Elenco tabelle definite nel sistema gestione tabelle di forward
	TABLE_CONTENT,           				// 093 Contenuto elementi tabella sistema gestione tabelle di forward
	TABLE_CONTENT_LIST,           	    	// 094 Elenco righe tabella sistema gestione tabelle di forward, per funzione di selezione
	DUAL_SELECTION,           		    	// 095 Selezione/deselezione valori con list valori deselezionati <<, >>, >, < e lista valori selezionati 
	HELP_HTML,           		    		// 096 help con testo html o altro
	SOURCE_VIEWER,           		   		// 097 Visualizzatore sorgente con funzioni elementari di ricerca
	DIAGRAM,          		    			// 098 Grafico a torta/barre/kiviat
	LOGIN,           		    			// 099 Login con user/password
	SPLASH             		    			// 100 Schermata iniziale applicazione
}












