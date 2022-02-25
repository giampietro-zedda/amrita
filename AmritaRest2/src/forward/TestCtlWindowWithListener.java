/**
 * 
 */
package forward;
import javax.swing.JFrame;

/**
 * @author ADMIN
 *
 */
public class TestCtlWindowWithListener extends JFrame{

	/**
	 * 
	 */
	public TestCtlWindowWithListener() { 
	  super("Prova GUI con Swing e controllo window con Listener");
	  setSize(600,600);
      TestCtlWindow esci = new TestCtlWindow();
      addWindowListener(esci);
	  setVisible(true);
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
       TestCtlWindowWithListener t = new TestCtlWindowWithListener();

	}

}
