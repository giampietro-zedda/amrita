package analyzer;
import java.sql.Connection;


/**
 * copyright (c) 2009-2020 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseConnection
 * </h1>
 * <p>
 * Questa classe descrive i dati di connessione/disconnessione al db.
 * 
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 20/dic/2020
*/

public class DataBaseConnection {
	
	private Connection dbConn = null;		  // Connessione al data base			
	private boolean isConnActive = false;	  // True = connessione attiva e assegnabile
	private boolean isConnFree = true;		  // True = connessione rassegnabile
	private String idAssigned = "";			  // identificativo richiedente
	private String errorCodeSQL = "";         // 
	private String errorStateSQL = "";        //
	private String userLogin = "";            //

	
	/**
	 * @return the dbConn
	 */
	public Connection getDbConn() {
		return dbConn;
	}
	/**
	 * @param dbConn the dbConn to set
	 */
	public void setDbConn(Connection dbConn) {
		this.dbConn = dbConn;
	}
	/**
	 * @return the isConnActive
	 */
	public boolean isConnActive() {
		return isConnActive;
	}
	/**
	 * @param isConnActive the isConnActive to set
	 */
	public void setConnActive(boolean isConnActive) {
		this.isConnActive = isConnActive;
	}
	/**
	 * @return the isConnFree
	 */
	public boolean isConnFree() {
		return isConnFree;
	}
	/**
	 * @param isConnFree the isConnFree to set
	 */
	public void setConnFree(boolean isConnFree) {
		this.isConnFree = isConnFree;
	}
	/**
	 * @return the idAssigned
	 */
	public String getIdAssigned() {
		return idAssigned;
	}
	/**
	 * @param idAssigned the idAssigned to set
	 */
	public void setIdAssigned(String idAssigned) {
		this.idAssigned = idAssigned;
	}
	/**
	 * @return the errorCodeSQL
	 */
	public String getErrorCodeSQL() {
		return errorCodeSQL;
	}
	/**
	 * @param errorCodeSQL the errorCodeSQL to set
	 */
	public void setErrorCodeSQL(String errorCodeSQL) {
		this.errorCodeSQL = errorCodeSQL;
	}
	/**
	 * @return the errorStateSQL
	 */
	public String getErrorStateSQL() {
		return errorStateSQL;
	}
	/**
	 * @param errorStateSQL the errorStateSQL to set
	 */
	public void setErrorStateSQL(String errorStateSQL) {
		this.errorStateSQL = errorStateSQL;
	}
	/**
	 * @return the userLogin
	 */
	public String getUserLogin() {
		return userLogin;
	}
	/**
	 * @param userLogin the userLogin to set
	 */
	public void setUserLogin(String userLogin) {
		this.userLogin = userLogin;
	}
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "DataBaseConnection [dbConn=" + dbConn + ", isConnActive=" + isConnActive + ", isConnFree=" + isConnFree
				+ ", idAssigned=" + idAssigned + ", errorCodeSQL=" + errorCodeSQL + ", errorStateSQL=" + errorStateSQL
				+ ", userLogin=" + userLogin + "]";
	}
	

}
