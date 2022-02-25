package analyzer;
import java.lang.reflect.Type;
import enums.EnumDataBaseJdbcSqlType;


/**
 * Copyright (c) 2010-2012 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseItemDescriptor
 * </h1>
 * <p>
 * Questa classe descrive i campi delle Entity che devono essere resi persistenti nel database.
 * Sono memorizzate le informazioni lato Java (nome campo e tipo) e quelle lato dDatabase
 * (nome colonna e tipo dato Sql). <br>
 * Quete informazioni sono recuperate dalle annotation delle classi Entity di ogni tabella e
 * sono utilizzate dalle classi di gestione dell'accesso ai dati {@link DataBaseManager} {@link DataBaseFacade}.
 * 
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/mar/2010 
 * @see DataBaseManager
 *
*/
public class DataBaseItemDescriptor {
	
	// Informazioni da Annotation Entity e funzioni di ispezione Reflection sul campo definito in Entity...
	private String dbTable = "";				    	// Nome tabella sql a cui appartiene la colonna
	private boolean primaryKey = false;					// True = primary key
	private String javaFieldName = "";					// Nome campo java in classe Entity.... 
	private Type javaFieldType = null;				    // Tipo campo java in classe Entity.... come java.lang.String via reflection
	private Object javaFieldValue = null;				// Oggetto con valore campo java in classe Entity....
    private boolean javaEnumeration = false;            // True indica che il campo è un oggetto di una enumerazione
	private Class<?> javaFieldClass = null;             // Classe campo
	private String javaFieldClassName = "";             // Nome classe completo di package
	private String javaFieldClassNameSimple = "";       // Nome classe semplice (es. String, Int, ..) 
	private String dbColumn = "";				    	// Nome colonna in tabella Sql
	private String asName = "";				    		// As name assegnato al dbColumn. Usato per le logical data view. Normalmente i due nomi coincidono.
	                                                    // E' il nome della variabile definita dalla logical data view e letta dall'applicazione
	
	// Informazione da/per funzioni di ispezione db/tabelle/indici
	private int dbColumnIndex = 0;				    	// Numero colonna in istruzione Sql (1-based)
	private EnumDataBaseJdbcSqlType dbColumnType = null;// Tipo colonna in tabella Sql mutuati da java.sql.types
	private String dbColumnValue = null;				// Valore colonna nel formato javaFieldType nel formato di inserimento nel db
	private Object dbColumnLength = null;				// Lunghezza campo o numero complessivo cifre
	private Object dbColumnDec = null;			    	// Numero decimali 
	private boolean dbNullable = false;			    	// Nullable
	
	// Informazione impostata a fronte dei parametri impostati dal chiamante 
	private boolean preparedPlaceHolder = false;    	// True = campo segnaposto in istruzione prepared

	// Informazione impostata a fronte dei parametri impostati dal chiamante 
	private String errorCode = "";                   	// Error detected during loading of the entry

		
	
	/**
	 * Creates a data base item descriptor.
	 */
	public DataBaseItemDescriptor() {
		super();
		dbColumnType = EnumDataBaseJdbcSqlType.NOT_ASSIGNED; 
	}
	/**
	 * Gets if the item is a primary key item.<br>
	 * <p>
	 * @return the primaryKey
	 */
	public boolean isPrimaryKey() {
		return primaryKey;   
	}
	/**
	 * Sets if the item is a primary key item.<br>
	 * <p>
	 * @param primaryKey the primaryKey to set
	 */
	public void setPrimaryKey(boolean primaryKey) {
		this.primaryKey = primaryKey;
	}
	/**
	 * Gets the java field name defined in the entity bean class.<br>
	 * <p>
	 * @return the javaFieldName
	 */
	public String getJavaFieldName() {
		return javaFieldName;
	}
	/**
	 * Sets the java field name defined in the entity bean class.<br>
	 * <p>
	 * @param javaFieldName the javaFieldName to set
	 */
	public void setJavaFieldName(String javaFieldName) {
		this.javaFieldName = javaFieldName;
	}
	/**
	 * Gets the java field type defined in the entity bean class.<br>
	 * <p>
	 * @return the javaFieldType
	 */
	public Type getJavaFieldType() {
		return javaFieldType;
	}
	/**
	 * Sets the java field type defined in the entity bean class.<br>
	 * <p>
	 * @param javaFieldType the javaFieldType to set
	 */
	public void setJavaFieldType(Type javaFieldType) {
		this.javaFieldType = javaFieldType;
	}

	/**
	 * Gets the java field value.<br>
	 * <p>
	 * @return the javaFieldValue
	 */
	public Object getJavaFieldValue() {
		return javaFieldValue;
	}
	/**
	 * Sets the java field value.<br>
	 * <p>
	 * @param javaFieldValue the javaFieldValue to set
	 */
	public void setJavaFieldValue(Object javaFieldValue) {
		this.javaFieldValue = javaFieldValue;
	}
	/**
	 * Gets the database table name where the item is defined<br>
	 * <p>
	 * @return the dbTable
	 */
	public String getDbTable() {
		return dbTable;
	}
	/**
	 * Sets the database table name where the item is defined<br>
	 * <p>
	 * @param dbTable the dbTable to set
	 */
	public void setDbTable(String dbTable) {
		this.dbTable = dbTable;
	}
	/**
	 * Gets the database item column name<br>
	 * <p>
	 * @return the dbColumn
	 */
	public String getDbColumn() {
		return dbColumn;
	}
	/**
	 * Sets the database item column name<br>
	 * <p>
	 * @param dbColumn the dbColumn to set
	 */
	public void setDbColumn(String dbColumn) {
		this.dbColumn = dbColumn;
	}
	
	
	/**
	 * Gets the name assigned to the column with the <code>AS</code> clause.<br>
	 * <p>
	 * @return the asName
	 */
	public String getAsName() {
		return asName;
	}
	/**
	 * Sets the name assigned to the column with the <code>AS</code> clause.<br>
	 * <p>
	 * @param asName the asName to set
	 */
	public void setAsName(String asName) {
		this.asName = asName;
	}
	/**
	 * Gets the column number (1-baseD) in the sql statement.<br>
	 * <p>
	 * @return the dbColumnIndex
	 */
	public int getDbColumnIndex() {
		return dbColumnIndex;
	}
	/**
	 * Sets the column number (1-baseD) in the sql statement.<br>
	 * <p>
	 * @param dbColumnIndex the dbColumnIndex to set
	 */
	public void setDbColumnIndex(int dbColumnIndex) {
		this.dbColumnIndex = dbColumnIndex;
	}
	/**
	 * @return the dbColumnType da java.sql.types
	 */
	public EnumDataBaseJdbcSqlType getDbColumnType() {
		return dbColumnType;
	}
	/**
	 * @param dbColumnType the dbColumnType to set from java.sql.types
	 */
	public void setDbColumnType(EnumDataBaseJdbcSqlType dbColumnType) {
		this.dbColumnType = dbColumnType;
	}
	/**
	 * @return the dbColumnValue
	 */
	public Object getDbColumnValue() {
		return dbColumnValue;
	}
	/**
	 * @param dbColumnValue the dbColumnValue to set
	 */
	public void setDbColumnValue(String dbColumnValue) {
		this.dbColumnValue = dbColumnValue;
	}
	/**
	 * @return the preparedPlaceHolder
	 */
	public boolean isPreparedPlaceHolder() {
		return preparedPlaceHolder;
	}
	/**
	 * @param preparedPlaceHolder the preparedPlaceHolder to set
	 */
	public void setPreparedPlaceHolder(boolean preparedPlaceHolder) {
		this.preparedPlaceHolder = preparedPlaceHolder;
	}
	
	/**
	 * @return the javaEnumeration
	 */
	public boolean isJavaEnumeration() {
		return javaEnumeration;
	}
	/**
	 * @param javaEnumeration the javaEnumeration to set
	 */
	public void setJavaEnumeration(boolean javaEnumeration) {
		this.javaEnumeration = javaEnumeration;
	}
	
	/**
	 * @return the javaFieldClass
	 */
	public Class<?> getJavaFieldClass() {
		return javaFieldClass;
	}
	/**
	 * @param javaFieldClass the javaFieldClass to set
	 */
	public void setJavaFieldClass(Class<?> javaFieldClass) {
		this.javaFieldClass = javaFieldClass;
	}
	/**
	 * @return the javaFieldClassName
	 */
	public String getJavaFieldClassName() {
		return javaFieldClassName;
	}
	/**
	 * @param javaFieldClassName the javaFieldClassName to set
	 */
	public void setJavaFieldClassName(String javaFieldClassName) {
		this.javaFieldClassName = javaFieldClassName;
	}
	/**
	 * @return the javaFieldClassNameSimple
	 */
	public String getJavaFieldClassNameSimple() {
		return javaFieldClassNameSimple;
	}
	/**
	 * @param javaFieldClassNameSimple the javaFieldClassNameSimple to set
	 */
	public void setJavaFieldClassNameSimple(String javaFieldClassNameSimple) {
		this.javaFieldClassNameSimple = javaFieldClassNameSimple;
	}
	/**
	 * @return the dbColumnLength
	 */
	public Object getDbColumnLength() {
		return dbColumnLength;
	}
	/**
	 * @param dbColumnLength the dbColumnLength to set
	 */
	public void setDbColumnLength(Object dbColumnLength) {
		this.dbColumnLength = dbColumnLength;
	}
	/**
	 * @return the dbColumnDec
	 */
	public Object getDbColumnDec() {
		return dbColumnDec;
	}
	/**
	 * @param dbColumnDec the dbColumnDec to set
	 */
	public void setDbColumnDec(Object dbColumnDec) {
		this.dbColumnDec = dbColumnDec;
	}
	/**
	 * @return the dbNullable
	 */
	public boolean isDbNullable() {
		return dbNullable;
	}
	/**
	 * @param dbNullable the dbNullable to set
	 */
	public void setDbNullable(boolean dbNullable) {
		this.dbNullable = dbNullable;
	}
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "JavaFieldName:"+this.javaFieldName+",PK:"+this.isPrimaryKey();
	}
	/**
	 * @return the errorCode
	 */
	public String getErrorCode() {
		return errorCode;
	}
	/**
	 * @param errorCode the errorCode to set
	 */
	public void setErrorCode(String errorCode) {
		this.errorCode = errorCode;
	}
	
	
}
