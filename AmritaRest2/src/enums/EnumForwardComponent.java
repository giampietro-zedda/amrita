package enums;

import javax.swing.JComponent;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (Italy)
  *
  * <h1>
  * EnumForwardJComponent
  * </h1>
  *  <p>
  * This enumeration lists all java oject types that's possible to lay in a panel.<br>
  * They are graphic standard objects extension of {@link JComponent} .<br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardComponent {
	
	NOT_ASSIGNED,       	// 00 Di servizio 
	
	//////////////////////////////////////////////////////////
	// Oggetti grafici
	//////////////////////////////////////////////////////////
	
	// Contenitori e barra di scorrimento
	JFrame,					// 01
	JApplet,				// 02
	JPanel,					// 03
	JDialog,				// 04
	JTabbedPane,			// 05
	JSplitPane,				// 06
	JScrollPane,			// ??
	JScrollBar,				// ??
	
	// Label e campi e combo
	JLabel, 				// 07
	JTextField, 			// 08
	JPasswordField, 		// 09
	JFormattedTextField, 	// 10
	JComboBox, 				// 11
	
	// Aree di testo normali, con stile e rich
	JTextArea, 				// 12
	JTextPane, 				// 13
	JEditorPane, 			// 14
	
	// Pulsanti
	JButton, 				// 15
	JToggleButton, 			// 16
	
	// Opzioni
	JCheckBox, 				// 17
	JRadioButton, 			// 18
	
	// Elenchi alberi e tabelle
	JList, 					// 19
	JTree, 					// 20
	JTable, 				// 21
	
	// Selezione valori numerici, progress bar
	JSlider, 				// 22
	JSpinner, 				// 23
	JProgressBar, 			// 24
	
	// Toolbar 
	JToolBar, 				// 25

	// Separatori
	JSeparator, 			// 26			// Separatore per menu, toolbar e in generale fra oggetti
	JBoxRigidArea, 			// 27			// Separatore per BOX layout come oggetto rigido trasparente
	JBoxStrutHorizontal, 	// 28			// Separatore per BOX layout come filler orizzontale trasparente
	JBoxStrutVertical, 		// 29			// Separatore per BOX layout come filler verticale trasparente
	JBoxGlue, 				// 30			// Separatore per BOX layout come elastico sia in orizzontale sia in verticale
	JBoxGlueHorizontal, 	// 31			// Separatore per BOX layout come elastico orizzontale
	JBoxGlueVertical, 		// 32			// Separatore per BOX layout come elastico verticale

    // Menu
	JMenu,					// 33
	JMenuItem, 				// 34
	JCheckBoxMenuItem, 		// 35
	JRadioButtonMenuItem, 	// 36
	
	//////////////////////////////////////////////////////////
	// Oggetti NON grafici
	//////////////////////////////////////////////////////////
	
	TimerSwing,             // 37			// Timer di swing
	DeclaredVariable,       // 38			// Variabile dichiarata per la funzione
	
}












