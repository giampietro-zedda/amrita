/**
 * copyright (c) 2008 Amrita-Forward - Giampietro Zedda 2008   Turin (ITALY)
 */
package forward;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

/**
 * ForwardDbConnection
 * 
 * This classe implements all necessary for connecting to DBMS
 * Are managed different kind of data base, 
 * with ODBC connection or with the standard Java JConnector.
 * 
 * @version 1.0
 * @date 20-10-2008
 * @author Giampietro Zedda
 *
*/
public class ForwardDbConnection {

	// DBMS managed
    public static final int DB_MYSQL = 1;
    public static final int DB_ACCESS = 2;
    public static final int DB_ORACLE = 3;
    // Drivers
    public static final String ODBC_DRIVER_SUN = "sun.jdbc.odbc.JdbcOdbcDriver";
    public static final String JDBC_DRIVER_MYSQL = "com.mysql.jdbc.Driver";
    
	// Definitions for access with ODBC driver or JAVA JDBC JConnector
	private int dbType;                     		// Database type
    private String ipAddress = "Localhost"; 		// Database address
    private String dBName = ""; 		    		// Database name
    private String userId;   						// User name
    private String pwd;      						// Password
	private String dataSourceODBC = "";     		// ODBC data source completed at connection
    private String connectString = "jdbc:mysql:/";  // Completed at connection

	// Definitions for access with JAVA JDBC JConnector
	private Connection dbConnection;     			// Database connection
	private boolean bConnection;    				// True means connection active
    private String errorCodeSQL = "";               // 
    private String errorStateSQL = "";              //
	
	/**
     * Minimal Constructor 
     */
	public ForwardDbConnection() { 
	    bConnection = false;
	    errorCodeSQL = "";
	    errorStateSQL = "";
	}

    /**
     * Constructor for Microsoft Access ODBC data source
     *
     * @param dataBaseName
     */
	public ForwardDbConnection(String dBName, String dataSourceODBC ) { 
		this(DB_ACCESS, dBName, "", "", ""); 
		setDataSourceODBC(dataSourceODBC);
	}

	/**
     * Default constructor for MYSQL
     *
     * @param dataBaseName
     */
	public ForwardDbConnection(int dbType, String dBName, String ipAddress, String userId, String pwd) {
	    this();
		this.dbType = dbType;
	    this.dBName = dBName;
	    this.ipAddress = ipAddress;
	    this.userId = userId;
	    this.pwd = pwd;
    }
	
	/**
     * Default constructor for local MYSQL
     *
     * @param dataBaseName
     */
	public ForwardDbConnection(int dbType, String dBName, String userId, String pwd) {
		this(dbType, dBName, "Localhost", userId, pwd);
    }

	
	/**
	 * Connect
	 * 
	 * Connect to database
	 * 
	 * @return Connection
	 */
	public Connection connect() {
		dbConnection = null;
		
		try {
			//-----------------------------------------------------------------------
			// Connect to MYSQL
			//-----------------------------------------------------------------------
			
			if (dbType == DB_MYSQL) {
				Class.forName(JDBC_DRIVER_MYSQL);  // Load JDBC driver for connection to MySQL

				// Dbname control
				if (dBName.equals("" )) {
					System.out.println("Manca il nome del database!!");
					System.out.println("Scrivere il nome del database da utilizzare all'interno del file \"config.xml\"");
					return null;
				}

				connectString = connectString + "/" + ipAddress + "/" + dBName;;

				// Userid necessary
				if (!userId.equals("")) {
					connectString = connectString + "/" + ipAddress + "/" + "?user=" + userId;
				}

				// Password necessary
				if (!userId.equals("") && !pwd.equals("") ) {
					connectString = connectString + "/" + ipAddress + "/" + "&password=" + pwd;
				}

				dbConnection = DriverManager.getConnection(connectString);

				// Successfully connection
				return dbConnection;
			};

			//-----------------------------------------------------------------------
			// Connect to ACCESS
			//-----------------------------------------------------------------------
			if (dbType == DB_ACCESS) {
				Class.forName(ODBC_DRIVER_SUN);      // Load ODBC driver
				dbConnection  = DriverManager.getConnection("dbc:odbc"+dataSourceODBC,"","");
			};

		} catch (SQLException e) {
			System.out.println("SQL error:" + e.toString() + e.getErrorCode() +  " " + e.getSQLState() );
		}
		catch (Exception e) {
			System.out.println("Error: " + e.toString() +  " " + e.getMessage() );
		}	    

		return dbConnection;
	}

	
	/**
	 * disconnect
	 * 
	 * Disconnect from database
	 */
	public boolean disconnect() {
		boolean bRet = false;
		try {
			dbConnection.close();
			bRet = true;
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			System.out.println("Error: " + e.toString() +  " " + e.getMessage() );
		}	    
		return bRet;
	}

	/**
	 * @return the dbType
	 */
	public int getDbType() {
		return dbType;
	}

	/**
	 * @param dbType the dbType to set
	 */
	public void setDbType(int dbType) {
		this.dbType = dbType;
	}

	/**
	 * @return the ipAddress
	 */
	public String getIpAddress() {
		return ipAddress;
	}

	/**
	 * @param ipAddress the ipAddress to set
	 */
	public void setIpAddress(String ipAddress) {
		this.ipAddress = ipAddress;
	}

	/**
	 * @return the dBName
	 */
	public String getDBName() {
		return dBName;
	}

	/**
	 * @param name the dBName to set
	 */
	public void setDBName(String name) {
		dBName = name;
	}

	/**
	 * @return the userId
	 */
	public String getUserId() {
		return userId;
	}

	/**
	 * @param userId the userId to set
	 */
	public void setUserId(String userId) {
		this.userId = userId;
	}

	/**
	 * @return the pwd
	 */
	public String getPwd() {
		return pwd;
	}

	/**
	 * @param pwd the pwd to set
	 */
	public void setPwd(String pwd) {
		this.pwd = pwd;
	}

	/**
	 * @return the dataSourceODBC
	 */
	public String getDataSourceODBC() {
		return dataSourceODBC;
	}

	/**
	 * @param dataSourceODBC the dataSourceODBC to set
	 */
	public void setDataSourceODBC(String dataSourceODBC) {
		this.dataSourceODBC = dataSourceODBC;
	}

	/**
	 * @return the dbConnection
	 */
	public Connection getDbConnection() {
		return dbConnection;
	}


	/**
	 * @return the errorCodeSQL
	 */
	public String getErrorCodeSQL() {
		return errorCodeSQL;
	}

	/**
	 * @return the errorStateSQL
	 */
	public String getErrorStateSQL() {
		return errorStateSQL;
	}

}



