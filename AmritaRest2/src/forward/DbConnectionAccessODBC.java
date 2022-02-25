package forward;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Enumeration;
/**
 * Test connessione a database Access
 * 
 * @author ADMIN
 *
 */
public class DbConnectionAccessODBC {

	public DbConnectionAccessODBC() {  
	}

	/**
	 * @param args
	 * @throws ClassNotFoundException 
	 */
	public static void main(String[] args) throws ClassNotFoundException {
	    String dataSource = "jdbc:odbc:TestJava";
	    String sqlStmt = "SELECT * FROM TabProva";
	    String str = "";
	    
	    // Elenca i driver disponibili
	    Enumeration en = DriverManager.getDrivers();
	    for (; en.hasMoreElements();) {    	
			str = en.nextElement().toString();
		    System.out.println(str);				
		}

	    try {
			Class.forName("sun.jdbc.odbc.JdbcOdbcDriver");      // Carica il driver
			Connection conn = DriverManager.getConnection(dataSource,"","");
			Statement st = conn.createStatement();
			ResultSet rec = st.executeQuery(sqlStmt);
			 
			System.out.println("Codice\tDescrizione\tData");
			while (rec.next()) {
				System.out.println(rec.getString(1) + "\t"
						         + rec.getString(2) + "\t"
						         + rec.getString(3));
			}
			st.close();
		} catch (SQLException e) {
			System.out.println("Errore SQL: " + e.toString() + e.getErrorCode() +  " " + e.getSQLState() );
			
        	}
		  catch (Exception e) {
			System.out.println("Errore: " + e.toString() +  " " + e.getMessage() );
        	}	    
	    
	}

}
