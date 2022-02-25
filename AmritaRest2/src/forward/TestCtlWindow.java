package forward;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/*
 * 
 * Controlla la chiusura di una window ed esce
 * @see TestCtlWindowWithListener
 */
public class TestCtlWindow extends WindowAdapter {
	public void windowClosing(WindowEvent e) {
		// TODO Auto-generated method stub
		System.exit(0);
	}

	
}
