package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardPanel
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie di pannelli previsti in una funzione Forward.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardPanelType {
	
	NOT_ASSIGNED,           			// 00 Di servizio 
	
	FORM,								// 01 Panel definito da un form completo, identificato dal suo root panel
	CONTAINER,           				// 02 Contenitore generico di altri pannelli applicativi o altri contenitori
	USER_DEFINED,           			// 03 Panel utente personalizzato che eredita da ForwardPanel, trattato come quelli predefiniti successivi
	TABBED,           					// 04 Contenitore di altri pannelli realizzata con JTabbedPane 
	SPLIT,           					// 05 Contenitore di 2 pannelli in split realizzata con JSplitPane 
	MENU,           					// 06 Menu orizzontale, verticale, di label, button, a scomparsa, con toolbar etc.
	DETAIL,           					// 07 Campi e oggetti grafici di dettaglio definiti nella funzione, con eventuali list, table, tree
	                                    // * Pannelli funzionali *
	LIST,           					// 08 Gestione elenco lenco di informazioni realizzato con JList
	GRID,           					// 09 Griglia di informazioni realizzata con JTable
	TREE,           					// 10 Albero di informazioni realizzato con JTree
	BUTTON_GRID_PILOT,                  // 11 Pulsanti di navigazione standard
	PROPERTIES,           				// 12 Pannello di proprietà con due colonne
	TABLE_LIST,           				// 13 Elenco tabelle definite nel sistema gestione tabelle di forward
	TABLE_CONTENT,           			// 14 Contenuto elementi tabella sistema gestione tabelle di forward
	TABLE_CONTENT_LIST,           	    // 15 Elenco righe tabella sistema gestione tabelle di forward, per funzione di selezione
	DUAL_SELECTION,           		    // 16 Selezione/deselezione valori con list valori deselezionati <<, >>, >, < e lista valori selezionati 
	HELP_HTML,           		    	// 17 help con testo html o altro
	SOURCE_VIEWER,           		    // 18 Visualizzatore sorgente con funzioni elementari di ricerca
	DIAGRAM,          		    		// 19 Grafico a torta/barre/kiviat
	LOGIN,           		    		// 20 Login con user/password
	SPLASH             		    		// 21 Schermata iniziale applicazione
}
