package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardInputFieldType
  * </h1>
  *  <p>
  * Questa enum elenca le possibili modalità con cui l'utente finale può immettere/modificare informazioni<br>
  * <p>
  * Ogni modalità implica l'utilizzo di un oggetto grafico. Nel caso più elementare si possono utilizzare<br>
  * degli oggetti GUI JText.<br>
  * <p<
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardInputFieldType {
	
	NOT_ASSIGNED,           		// 00 Di servizio 
	
	INPUT_BY_TEXT_FIELD,     		// 01 Digitato in un JTextField
	INPUT_BY_COMBO_BOX,     		// 02 Selezionato in JComboBox, non necessariamente un valore text ma anche qualsiasi oggetto associato alla riga selezionata  
	INPUT_BY_LIST,     				// 03 Selezionato con JList, non necessariamente un valore text ma anche qualsiasi oggetto associato alla riga selezionata 
	INPUT_BY_SLIDER,     			// 04 Selezionato con JSlider, valore numerico
	INPUT_BY_SPINNER,     			// 05 Selezionato con JSpinner, valore numerico o data
	INPUT_BY_CHECK_BOX,     		// 06 Selezionato con JCheckBox, valore booleano
	INPUT_BY_RADIO_BUTTON,     		// 07 Selezionato con JRadioButton, valore booleano
	INPUT_BY_OPTION_GROUP,     		// 08 Selezionato con ButtonGroup
	INPUT_BY_DB_TABLE,     			// 09 Letto da una tebella del sistema tabelle di forward
	INPUT_BY_DB_LDV,     	    	// 10 Letto da una logical data view
	INPUT_BY_LOOKUP_FUNCTION, 		// 11 Restituito da funzione applicativa modale di selezione
	INPUT_BY_LOOKUP_TABLE, 			// 12 Restituito da funzione forward modale di selezione da sistema tabelle
	INPUT_INDIRECT,     	    	// 13 Impostato indirettamente da altro campo
	INPUT_AUTOMATIC;     	    	// 14 Impostato automaticamente da Forward al valore della variabile con lo stesso nome
}












