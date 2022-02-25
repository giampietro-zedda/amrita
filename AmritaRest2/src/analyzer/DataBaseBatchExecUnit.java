package analyzer;


/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseBatchExecUnit
 * </h1>
 * <p>
 * Questa classe descrive una singola unita di esecuzione di un insieme di comandi Sql che
 * vengono eseguiti cumulativamente con istruzione excecuteBatch().
 * Ogni comando eseguit via batch ha il proprio count di oggetti modificati.
 * Oggetti di questa classe vengono utilizzati per descrivere generali insiemi batch di 
 * comandi da eseguire.
 * 
 * 
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/mar/2010 
 * @see DataBaseManager
 * @see DataBaseFacade
 * @see DataBaseItemDescriptor
 * @see DataBaseStatusDetailed
*/

public class DataBaseBatchExecUnit {
	
	private String sqlCommand = "";				    // Comando Sql
	private String descr = "";                      // Descrizione operazione effettuata
	private int countUpdates = 0;                   // Numero aggiornamenti o codice di errore
	
	/**
	 * Costruttore vuoto
	*/
	public DataBaseBatchExecUnit() {
		super();
	}

	/**
	 * @return the sqlCommand
	 */
	public String getSqlCommand() {
		return sqlCommand;
	}

	/**
	 * @param sqlCommand the sqlCommand to set
	 */
	public void setSqlCommand(String sqlCommand) {
		this.sqlCommand = sqlCommand;
	}

	/**
	 * @return the descr
	 */
	public String getDescr() {
		return descr;
	}

	/**
	 * @param descr the descr to set
	 */
	public void setDescr(String descr) {
		this.descr = descr;
	}

	/**
	 * @return the countUpdates
	 */
	public int getCountUpdates() {
		return countUpdates;
	}

	/**
	 * @param countUpdates the countUpdates to set
	 */
	public void setCountUpdates(int countUpdates) {
		this.countUpdates = countUpdates;
	}

	

}
