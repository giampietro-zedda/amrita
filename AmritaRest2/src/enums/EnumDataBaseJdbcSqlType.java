package enums;
/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumDataBaseJdbcSqlType  
 * </h1>
 *  <p>
 * Questa enum elenca i possibili tipi di dati Sql gestiti da java jdbc.
 * si tratta di un clone di java.sql.types, che permette solo di ottenere il numero
 * del type sql senza la descrizione. Questa Enum permette di controllare il valore
 * testuale di ogni tipo in modo centralizzato.
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/03/2010
 * @see DataBaseManager
 * @see DataBaseFacade
 * @see DataBaseItemDescriptor
 *  
*/
public enum EnumDataBaseJdbcSqlType {

	 
	 NOT_ASSIGNED,              // 00   // Di servizio            

     ARRAY,                		// 01
     BIGINT,                	// 02
     BINARY,                	// 03
     BIT,                		// 04
     BLOB,                		// 05
     BOOLEAN,                	// 06
     CHAR,                		// 07
     CLOB,                		// 08
     DATALINK,                	// 09
     DATE,                		// 10
     DECIMAL,                	// 11
     DISTINCT,                	// 12
     DOUBLE,                	// 13
     FLOAT,                		// 14
     INTEGER,                	// 15
     JAVA_OBJECT,               // 16
     LONGNVARCHAR,              // 17
     LONGVARBINARY,             // 18
     LONGVARCHAR,               // 19
     NCHAR,                		// 20
     NCLOB,                		// 21
     NULL,                		// 22
     NUMERIC,                	// 23
     NVARCHAR,                	// 24
     OTHER,                		// 25
     REAL,                		// 26
     REF,                		// 27
     ROWID,                		// 28
     SMALLINT,                	// 29
     SQLXML,                	// 30
     STRUCT,                	// 31
     TIME,                		// 32
     TIMESTAMP,                	// 33
     TINYINT,                	// 34
     VARBINARY,                	// 35
     VARCHAR;                	// 36
	
}
