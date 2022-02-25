/**
 * 
 */
package forward;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.GregorianCalendar;
import javax.swing.JApplet;   



/**
 *
 * 
 * 
 * @author ADMIN
 *
 */
public class TestApplet extends JApplet{ 
 private Color beige =new Color(255, 204, 102);
 private String oraPrecedente = "";
 
 
 public void init(){
	 String nomeCarattere = getParameter("carattere");
	 String dimensione = getParameter("dimensione");	
	 System.out.println("carattere="+nomeCarattere);
	 System.out.println("dimensione="+dimensione);	 
 }
 public void start(){
	 
 }
 public void stop(){
	 
 }
 public void destroy(){
 }
 public void paint(Graphics schermo){
   Graphics2D schermo2D = (Graphics2D) schermo;	  
   Font tipo = new Font("Monospaced",Font.BOLD, 20);
   schermo2D.setFont(tipo);
   GregorianCalendar giorno = new GregorianCalendar();
   String ora = giorno.getTime().toString();
   schermo2D.setColor(Color.black);
   schermo2D.drawString(oraPrecedente, 5, 25);   
   schermo2D.setColor(beige);
   schermo2D.drawString(ora, 5, 25);
   try {
		Thread.sleep(1000); // Niente per 1000 millisecondi
   } catch (InterruptedException e) {
       // Nulla
   }
   repaint(); 
}

}
