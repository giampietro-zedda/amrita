package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2010-2012 e-Amrita - Giampietro Zedda    Turin (Italy)
  *
  * <h1>
  * EnumForwardCaptionCustomization
  * </h1>
  *  <p>
  * Are classified all types of caption in a forward function application used to manage.<br>
  * the multilanguage localization of each text.<br>
  * When no caption customization has been coded for the function and no caption has beeen coded in any<br>
  * forward rule table caption, then will be used texts coded at declaration time.
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardCaptionCustomization {
	
	NOT_ASSIGNED,       						// 00 Di servizio 
	
	// Valori testuali per localizzazione e customizzazione
	CAPTION_FUNCTION_TITLE, 					// 01 Caption barra finestra principale di funzione funzione 							 
	CAPTION_DIALOG_TITLE, 						// 01 Caption barra finestra principale dialog									 
	CAPTION_PANEL_TITLE, 						// 02 Caption border title pannello									 
	CAPTION_PANEL_TOOLTIP, 						// 03 Caption tooltip pannello									 
	CAPTION_PANEL_TAB, 							// 04 Caption Tab di tabbed panel								 
	CAPTION_CONTROL, 							// 05 Caption JLabel, JRadioBUtton, JMenuRadioButton, JTable etc o di JButton e JToggleButton
	CAPTION_CONTROL_TITLE, 						// 06 Caption border title controllo GUI
	CAPTION_CONTROL_TOOLTIP, 					// 07 Caption tooltip controllo GUI
	CAPTION_TABLE_COLUMN_HEADER,    			// 08 Caption header di colonna di tabella
	CAPTION_TABLE_TOOLTIP_HEADER,   			// 09 Caption tooltip colonna di tabella
	CAPTION_TABLE_TOOLTIP_CELL,     			// 10 Caption tooltip cella di tabella
	CAPTION_DIALOG_MESSAGE, 					// 11 Caption Testo messaggio in dialogo 
	CAPTION_DIALOG_SELECTION_VALUES,			// 12 Caption valori da selezionare in comboBox in dialogo 
	
	// Valori per customizzazione
	CUSTOMIZED_INITIAL_VALUE,					// 14 Valore iniziale controllo
	CUSTOMIZED_METHOD_NAME,						// 15 Nome metodo da richiamare
	CUSTOMIZED_FUNCTION_NAME,					// 16 Nome funzione da richiamare
	CUSTOMIZED_LDV_NAME,						// 17 Nome logical data view 
	CUSTOMIZED_PROPERTY_NUM,					// 18 Nome property numerica da valorizzare
	CUSTOMIZED_PROPERTY_TEXT,					// 19 Nome property text da valorizzare
	CUSTOMIZED_PROPERTY_BOOLEAN,				// 20 Nome property booleana da valorizzare
	CUSTOMIZED_PROPERTY_FONT,					// 21 Nome property Color da valorizzare
	CUSTOMIZED_PROPERTY_COLOR,					// 22 Nome property Color da valorizzare
}












