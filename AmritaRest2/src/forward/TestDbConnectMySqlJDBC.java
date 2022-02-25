package forward;
import java.sql.ResultSet;
import java.util.Vector;


public class TestDbConnectMySqlJDBC {

	public TestDbConnectMySqlJDBC() {
		// TODO Auto-generated constructor stub
	}

	/** 
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

		
		DbManagerMySqlJDBC db = new DbManagerMySqlJDBC("mysql","Amrita","maurizia");
//		DbManagerMySqlJDBC db = new DbManagerMySqlJDBC("mysql");

		if ( !db.connetti() ) {
		   System.out.println("Errore durante la connessione.");
		   System.out.println( db.getErrore() );
		   System.exit(0);
		}

		// Eseguo una query sul database. La tabella si chiama Tbl.
		Vector v = db.eseguiQuery( "SELECT * FROM user;" );
		
		// Stampiamo i risultati:
		int i = 0;
		while ( i<v.size() ) {
		   String[] record = (String[]) v.elementAt(i);
		   System.out.println("Record numero " + (i+1) );
		   for (int j=0; j<record.length; j++) {
		      System.out.println( record[j] );
		   }
		   i++;
		}

		// Eseguo un aggiornamento sul campo 'nomecampo' della tabella Tbl:
		if ( !db.eseguiAggiornamento("UPDATE Tbl SET nomecampo=valore WHERE nomecampo>0;") ) {
		   System.out.println("Errore nell'aggiornamento!");
		   System.out.println( db.getErrore() );
		}

		// Ora chiudo la connessione col Database:
		db.disconnetti();
		
		
		
		
	}

}
