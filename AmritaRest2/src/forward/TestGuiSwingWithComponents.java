/**
 * 
 */
package forward;
import java.awt.Dimension;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

/**
 * Classe per testare l'inserimento di oggetti nel form
 * 
 * @author ADMIN
 *
 */
public class TestGuiSwingWithComponents extends JFrame{
    // Definisco i button come oggetti singoli
	JButton annulla = new JButton("Annulla");
    JButton riprova = new JButton("riprova");
    JButton esci = new JButton("esci");
    
    // Definisco Label
    JLabel LabelField1 = new JLabel("Campo1 LEFT",SwingConstants.LEFT);
    JLabel LabelField2 = new JLabel("Campo2 CENTER",SwingConstants.CENTER);
    JLabel LabelField3 = new JLabel("Campo3 RIGHT",SwingConstants.RIGHT);
    JLabel LabelUser = new JLabel("Utente",SwingConstants.LEFT);  
    JLabel LabelPassword = new JLabel("Password:",SwingConstants.LEFT);  
    JLabel Labelcommenti = new JLabel("Test JScrollPane",SwingConstants.LEFT); 
    
    // Definisco TextField
    JTextField textField1 = new JTextField("Campo1 ",15);
    JTextField textField2 = new JTextField("Campo2 ",15);
    JTextField textField3 = new JTextField("Campo3 ",15);
    JTextField nomeUtente = new JTextField("User ",15);
    
    // Definisco Password e commenti su scrolled panel
    JPasswordField password = new JPasswordField(15);
    JTextArea commenti = new JTextArea(4, 15);
    JScrollPane scorrimento = new JScrollPane(commenti,
    		  ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
    		  ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS
    		  );
    
    // Definisco CheckBox e ComboBox
    ButtonGroup sceltaButton = new ButtonGroup();
    JRadioButton r1 = new JRadioButton("Scelta 1",true);
    JRadioButton r2 = new JRadioButton("Scelta 2",false);    
  
    ButtonGroup sceltaCheck = new ButtonGroup();
    JCheckBox c1 = new JCheckBox("Scelta 1",true);
    JCheckBox c2 = new JCheckBox("Scelta 2",false);    
    
    // Definisco barra di scorrimento
    JScrollBar sb1 = new JScrollBar(JScrollBar.HORIZONTAL,
    		                        5,    // Valore iniziale
    		                        0,    // Larghezza (default)
    		                        0,    // Valore minimo
    		                        100    // Valore massimo
    		                        );
    
    
	public TestGuiSwingWithComponents() {
	  super("Prova GUI Swing con button ....");
	  setSize(800,500);
	  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	  // Definisco il pannello (container) dove mettere i button
	  JPanel pannello = new JPanel();
	 
	  // Inserisco i bottoni nel pannell
	  pannello.add(annulla);
	  pannello.add(riprova);
	  pannello.add(esci);	

	  //Definisco altro pannello
	  JPanel pannello2 = new JPanel();
	  pannello2.add(LabelField1);
	  pannello2.add(textField1);
	  pannello2.add(LabelField2);
	  pannello2.add(textField2);	  
	  pannello2.add(LabelField3);
	  pannello2.add(textField3);

	  //Inserisco pannello2 in pannello1
	  pannello2.setBorder(new TitledBorder(new EtchedBorder(), "Label e text"));	
	  pannello2.setToolTipText("OK");
	  pannello.add(pannello2);

	  //Definisco altro pannello
	  JPanel pannello3 = new JPanel();
	  commenti.setLineWrap(true);
	  commenti.setWrapStyleWord(true);
	  pannello3.add(LabelUser);
	  pannello3.add(nomeUtente);
	  pannello3.add(LabelPassword);
	  pannello3.add(password);
	  pannello3.add(Labelcommenti);
	  pannello3.add(scorrimento);  

	  //Inserisco pannello3 in pannello1
	  pannello3.setBorder(new TitledBorder(new EtchedBorder(), "Login"));	
	  pannello.add(pannello3);

	  //Definisco altro pannello x radio e check
	  JPanel pannello4 = new JPanel();
      sceltaCheck.add(r1);	  
      sceltaCheck.add(r2);	
	  sceltaButton.add(c1);	  
      sceltaButton.add(c2);	
	  pannello4.add(r1);
	  pannello4.add(r2);  
	  pannello4.add(c1);
	  pannello4.add(c2);  

	  //Inserisco pannello4 in pannello1
	  pannello4.setBorder(new TitledBorder(new EtchedBorder(), "Test ButtonGroup"));
	  pannello.add(pannello4);

	  //Definisco altro pannello x test Scroll bar
	  sb1.setSize(new Dimension(350,100));
	  JPanel pannello5 = new JPanel();
	  pannello5.add(sb1);
	  //Inserisco pannello5 in pannello1
	  pannello5.setBorder(new TitledBorder(new EtchedBorder(), "Test JScrollBar"));
	  pannello.add(pannello5);

	  
	  // fa diventare il pannello il riquadro dei contenuti del frame
	  setContentPane(pannello);
	  setVisible(true);
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		TestGuiSwingWithComponents t = new TestGuiSwingWithComponents();

	}

}
