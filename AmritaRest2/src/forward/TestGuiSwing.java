package forward;
import javax.swing.JFrame;

/**
 * @author ADMIN
 *
 */
public class TestGuiSwing extends JFrame{

	/**
	 * 
	 */
	public TestGuiSwing() {
	  super("Prova GUI con Swing no listener ....");
	  setSize(600,600);
	  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	  setVisible(true);
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
	  TestGuiSwing t = new TestGuiSwing();

	}

}
