/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseEntityInterfaceTest
 * </h1>
 * <p>
 * Test interfaccia accesso0 dati CRUD
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 29/mar/2010 
 *
*/

package junit.test;

import static org.junit.Assert.*;

import java.sql.Connection;
import java.sql.SQLException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import analyzer.LoggerFacade;
import analyzer.MessagesManager;
import analyzer.UserConfiguration;
import entities.EntityObject;
import enums.EnumDataBase;
import enums.EnumObject;
import enums.EnumObjectStatus;
import enums.EnumSourceType;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaAnnotationMissing;
import exception.ExceptionAmritaReflectionError;
import exception.ExceptionAmritaSqlAccessDuplicate;
import exception.ExceptionAmritaSqlAccessNotfound;
import exception.ExceptionAmritaSqlError;

/**
 * @author amrita
 *
 */
public class DataBaseEntityInterfaceTest {
    
     // Variabili fixture
	 protected UserConfiguration sd;
	 protected LoggerFacade lf;
	 protected MessagesManager mm;
     protected DataBaseEntityInterface dbei = null;	
     protected DataBaseManager dbm = null;	
	 protected String url = "";
	 protected String user = "";
	 protected String pwd = "";
	 protected String dbname = "";
     protected String driver = "";
     protected EnumDataBase database = null;
     protected DataBaseStatusDetailed dbs = null;
     protected Connection dbConn = null;
     protected EntityObject entityObject = null;
     
	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
	
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for {@link analyzer.DataBaseEntityInterface#DataBaseEntityInterface(analyzer.UserConfiguration, analyzer.DataBaseManager, java.sql.Connection)}.
	 */
	@Test
	public final void testDataBaseEntityInterface() {
		assertTrue(true);
	}

	/**
	 * Test method for {@link analyzer.DataBaseEntityInterface#read(java.lang.Object)}.
	 * @throws ExceptionAmrita 
	 * @throws ExceptionAmritaSqlAccessNotfound 
	 */
	@Test
	public final void testDeleteAll()  {
		initDefaultsAndLog();
		
		try {
			
			dbm = new DataBaseManager(sd, database, driver, url, dbname, user,pwd, 3);
			dbConn = dbm.getConnection(dbs);
			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
		} catch (Exception e) {
			assertTrue(false);
			e.printStackTrace();
		}
		
		try {
			entityObject.setSystem("A1");   						 // OBJTSYST(PK) Sistema applicativo
			entityObject.setSubSystem("SU");  						 // OBJTSUBS(PK) Sotto sistema applicativo
			entityObject.setIdObject("AK210");  					 // OBJTIDOE(PK) Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel) 
			entityObject.setTypeObject(EnumObject.OBJECT_PGM_COBOL); // OBJTTYPO(PK) Tipologia oggetto (T0001)
			dbei.delete(entityObject);
			dbei.commit();
		} catch (Exception e) {
			assertTrue(true);
		}
		
		try {
			entityObject.setIdObject("PGMZEDDA");  					 // OBJTIDOE(PK) Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel) 
			entityObject.setTypeObject(EnumObject.OBJECT_PGM_JAVA);  // OBJTTYPO(PK) Tipologia oggetto (T0001)
			dbei.delete(entityObject);
			dbei.commit();
		} catch (Exception e) {
			assertTrue(true);
		}

		try {
			entityObject.setSystem("Z1");   						 // OBJTSYST(PK) Sistema applicativo
			entityObject.setSubSystem("Z2");  						 // OBJTSUBS(PK) Sotto sistema applicativo
			entityObject.setIdObject("ZZZZ");  					     // OBJTIDOE(PK) Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel) 
			entityObject.setTypeObject(EnumObject.OBJECT_PGM_COBOL); // OBJTTYPO(PK) Tipologia oggetto (T0001)
			dbei.delete(entityObject);
			dbei.commit();
		} catch (Exception e) {
			assertTrue(true);
		}
	}

	
	/**
	 * Test method for {@link analyzer.DataBaseEntityInterface#create(java.lang.Object)}.
	 */
	@Test
	public final void testCreate() {

		initDefaultsAndLog();
		
		
		try {
			
			dbm = new DataBaseManager(sd, database, driver, url, dbname, user,pwd, 3);
			dbConn = dbm.getConnection(dbs);
			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
			
			entityObject.setSystem("A1");   						 // OBJTSYST(PK) Sistema applicativo
			entityObject.setSubSystem("SU");  						 // OBJTSUBS(PK) Sotto sistema applicativo
			entityObject.setIdObject("AK210");  					 // OBJTIDOE(PK) Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel) 
			entityObject.setTypeObject(EnumObject.OBJECT_PGM_COBOL); // OBJTTYPO(PK) Tipologia oggetto (T0001)
			entityObject.setLibrarySource("C:\\lib1\\src");  	     // OBJTLIBS     Libreria sorgente di analisi (Per oggetti direttamente dedotti da un file sorgente)
			entityObject.setFileSource("AK210");  				     // OBJTFILS     Nome file sorgente di analisi 
			entityObject.setTypeSource(EnumSourceType.COBOL_PROGRAM);// OBJTTYPS     Tipologia sorgente (T0002)
			entityObject.setLibraryDir(""); 					     // OBJTLDIR     Directory libreria per oggetti LIBRARY
			entityObject.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS); 
			                                                         // OBJTSTAT     Stato oggetto (T0003)

			entityObject.setAuthor("Zedda");				    	 // OBJTAUTH     Autore 
			entityObject.setSystemOwner("SO");       			     // OBJTSYOW  	Sistema applicativo proprietario
			entityObject.setSubSystemOwner("PP");    			     // OBJTSSOW  	Sotto sistema applicativo proprietario
			entityObject.setDateWritten("14-01-1989");			     // OBJTDTWR     Data scrittura (es. da date-written Cobol)
			entityObject.setPathDocFile("C:/doc/js/AK210.doc");	     // OBJTPDOC     Path completo file documentazione
			entityObject.setDtFirstAnalysis("20100328");	         // OBJTDTFA     Data prima analisi AAAAMMGG
			entityObject.setTmFirstAnalysis("23032300");	         // OBJTTMFA     Ora  prima analisi HHMMSSCC	
			entityObject.setDtLastAnalysis("20100329");	             // OBJTDTLA     Data ultima analisi AAAAMMGG
			entityObject.setTmLastAnalysis("23032300");	             // OBJTTMLA     Ora  ultima analisi HHMMSSCC	
		
			dbei.create(entityObject);
			dbei.create(entityObject);

			entityObject.setSystem("A1");   						  // OBJTSYST(PK) Sistema applicativo
			entityObject.setSubSystem("SU");  						  // OBJTSUBS(PK) Sotto sistema applicativo
			entityObject.setIdObject("PGMZEDDA");  					  // OBJTIDOE(PK) Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel) 
			entityObject.setTypeObject(EnumObject.OBJECT_PGM_JAVA);   // OBJTTYPO(PK) Tipologia oggetto (T0001)
			entityObject.setAuthor("Maurizia");				    	  // OBJTAUTH     Autore 

			dbei.create(entityObject);
			dbei.commit();
			
			dbm.closeConnection(dbConn, dbs);
			assertTrue(true);

		} catch (ExceptionAmritaSqlAccessDuplicate e) {
			assertTrue(false);
		} catch (ExceptionAmritaSqlError e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaAnnotationMissing e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaReflectionError e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmrita e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (SQLException e) {
			assertTrue(false);
			e.printStackTrace();
		} 
	}

	
	
	/**
	 * Test method for {@link analyzer.DataBaseEntityInterface#read(java.lang.Object)}.
	 * @throws ExceptionAmrita 
	 * @throws ExceptionAmritaSqlAccessNotfound 
	 */
	@Test
	public final void testRead()  {
		initDefaultsAndLog();
		
		try {
			
			dbm = new DataBaseManager(sd, database, driver, url, dbname, user,pwd, 3);
			dbConn = dbm.getConnection(dbs);
			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
			
			entityObject.setSystem("A1");   						 // OBJTSYST(PK) Sistema applicativo
			entityObject.setSubSystem("SU");  						 // OBJTSUBS(PK) Sotto sistema applicativo
			entityObject.setIdObject("AK210");  					 // OBJTIDOE(PK) Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel) 
			entityObject.setTypeObject(EnumObject.OBJECT_PGM_COBOL); // OBJTTYPO(PK) Tipologia oggetto (T0001)
		
			dbei.read(entityObject);

			assertTrue(entityObject.getFileSource().equals("AK210"));  				     // OBJTFILS     Nome file sorgente di analisi 
			assertTrue(entityObject.getTypeSource() == EnumSourceType.COBOL_PROGRAM);    // OBJTTYPS     Tipologia sorgente (T0002)
			assertTrue(entityObject.getLibraryDir().equals("")); 					     // OBJTLDIR     Directory libreria per oggetti LIBRARY
			assertTrue(entityObject.getStatus() == EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS); 
			                                                         					 // OBJTSTAT     Stato oggetto (T0003)

			assertTrue(entityObject.getAuthor().equals("Zedda"));				    	 // OBJTAUTH     Autore 
			assertTrue(entityObject.getSystemOwner().equals("SO"));       			     // OBJTSYOW  	Sistema applicativo proprietario
			assertTrue(entityObject.getSubSystemOwner().equals("PP"));    			     // OBJTSSOW  	Sotto sistema applicativo proprietario
			assertTrue(entityObject.getDateWritten().equals("14-01-1989"));			     // OBJTDTWR     Data scrittura (es. da date-written Cobol)
			assertTrue(entityObject.getPathDocFile().equals("C:/doc/js/AK210.doc"));	 // OBJTPDOC     Path completo file documentazione
			assertTrue(entityObject.getDtFirstAnalysis().equals("20100328"));	         // OBJTDTFA     Data prima analisi AAAAMMGG
			assertTrue(entityObject.getTmFirstAnalysis().equals("23032300"));	         // OBJTTMFA     Ora  prima analisi HHMMSSCC	
			assertTrue(entityObject.getDtLastAnalysis().equals("20100329"));	         // OBJTDTLA     Data ultima analisi AAAAMMGG
			assertTrue(entityObject.getTmLastAnalysis().equals("23032300"));	         // OBJTTMLA     Ora  ultima analisi HHMMSSCC	
			
			
			entityObject.setSystem("A1");   						 // OBJTSYST(PK) Sistema applicativo
			entityObject.setSubSystem("SU");  						 // OBJTSUBS(PK) Sotto sistema applicativo
			entityObject.setIdObject("PGMZEDDA");  					 // OBJTIDOE(PK) Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel) 
			entityObject.setTypeObject(EnumObject.OBJECT_PGM_JAVA);  // OBJTTYPO(PK) Tipologia oggetto (T0001)
		
			dbei.read(entityObject);
			
			dbei.commit();
			dbm.closeConnection(dbConn, dbs);
			assertTrue(true);
			
		} catch (ExceptionAmritaSqlAccessNotfound e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaSqlError e) {
			e.printStackTrace();
			assertTrue(false);
		} catch (ExceptionAmritaAnnotationMissing e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaReflectionError e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmrita e) {
			assertTrue(false);
			e.printStackTrace();
		} 
		 catch (Exception e) {
			assertTrue(false);
			e.printStackTrace();
			}
	}

	/**
	 * Test method for {@link analyzer.DataBaseEntityInterface#update(java.lang.Object)}.
	 */
	@Test
	public final void testUpdate() {
		initDefaultsAndLog();
		
		
		try {
			
			dbm = new DataBaseManager(sd, database, driver, url, dbname, user,pwd, 3);
			dbConn = dbm.getConnection(dbs);
			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
			
			entityObject.setSystem("A1");   						 // OBJTSYST(PK) Sistema applicativo
			entityObject.setSubSystem("SU");  						 // OBJTSUBS(PK) Sotto sistema applicativo
			entityObject.setIdObject("PGMZEDDA");  					 // OBJTIDOE(PK) Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel) 
			entityObject.setTypeObject(EnumObject.OBJECT_PGM_JAVA);  // OBJTTYPO(PK) Tipologia oggetto (T0001)
		
			dbei.read(entityObject);

			entityObject.setFileSource("AK210 UPDATED");  				     				// OBJTFILS     Nome file sorgente di analisi 
			entityObject.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS); 
			                                                         					 	// OBJTSTAT     Stato oggetto (T0003)
			dbei.update(entityObject);
			dbei.commit();
			
			dbm.closeConnection(dbConn, dbs);
			assertTrue(true);

		} catch (ExceptionAmritaSqlAccessNotfound e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaSqlError e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaAnnotationMissing e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaReflectionError e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmrita e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (Exception e) {
			assertTrue(false);
			e.printStackTrace();
			}
	}

	/**
	 * Test method for {@link analyzer.DataBaseEntityInterface#delete(java.lang.Object)}.
	 */
	@Test
	public final void testDelete() {
		initDefaultsAndLog();
		
		
		try {
			
			dbm = new DataBaseManager(sd, database, driver, url, dbname, user,pwd, 3);
			dbConn = dbm.getConnection(dbs);
			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
			
			entityObject.setSystem("A1");   						 // OBJTSYST(PK) Sistema applicativo
			entityObject.setSubSystem("SU");  						 // OBJTSUBS(PK) Sotto sistema applicativo
			entityObject.setIdObject("AK210");  					 // OBJTIDOE(PK) Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel) 
			entityObject.setTypeObject(EnumObject.OBJECT_PGM_COBOL); // OBJTTYPO(PK) Tipologia oggetto (T0001)
		
			dbei.delete(entityObject);

			dbei.commit();
			dbm.closeConnection(dbConn, dbs);
			assertTrue(true);


		} catch (ExceptionAmritaSqlAccessNotfound e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaSqlError e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaAnnotationMissing e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaReflectionError e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmrita e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (Exception e) {
			assertTrue(false);
			e.printStackTrace();
		}
	}

	/**
	 * Test method for {@link analyzer.DataBaseEntityInterface#rollback(java.sql.Connection)}.
	 */
	@Test
	public final void testRollback() {
		initDefaultsAndLog();
		
		
		try {
			
			dbm = new DataBaseManager(sd, database, driver, url, dbname, user,pwd, 3);
			dbConn = dbm.getConnection(dbs);
			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
			
			entityObject.setSystem("Z1");   						 // OBJTSYST(PK) Sistema applicativo
			entityObject.setSubSystem("Z2");  						 // OBJTSUBS(PK) Sotto sistema applicativo
			entityObject.setIdObject("ZZZZ");  					     // OBJTIDOE(PK) Nome oggetto esteso (Phisical file, User tag,..) (EnumObjectKeyModel) 
			entityObject.setTypeObject(EnumObject.OBJECT_PGM_COBOL); // OBJTTYPO(PK) Tipologia oggetto (T0001)
			entityObject.setLibrarySource("C:\\lib1\\src");  	     // OBJTLIBS     Libreria sorgente di analisi (Per oggetti direttamente dedotti da un file sorgente)
			entityObject.setFileSource("AK210");  				     // OBJTFILS     Nome file sorgente di analisi 
			entityObject.setTypeSource(EnumSourceType.COBOL_PROGRAM);// OBJTTYPS     Tipologia sorgente (T0002)
			entityObject.setLibraryDir(""); 					     // OBJTLDIR     Directory libreria per oggetti LIBRARY
			entityObject.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS); 
			                                                         // OBJTSTAT     Stato oggetto (T0003)

			entityObject.setAuthor("Zedda");				    	 // OBJTAUTH     Autore 
			entityObject.setSystemOwner("SO");       			     // OBJTSYOW  	Sistema applicativo proprietario
			entityObject.setSubSystemOwner("PP");    			     // OBJTSSOW  	Sotto sistema applicativo proprietario
			entityObject.setDateWritten("14-01-1989");			     // OBJTDTWR     Data scrittura (es. da date-written Cobol)
			entityObject.setPathDocFile("C:/doc/js/AK210.doc");	     // OBJTPDOC     Path completo file documentazione
			entityObject.setDtFirstAnalysis("20100328");	         // OBJTDTFA     Data prima analisi AAAAMMGG
			entityObject.setTmFirstAnalysis("23032300");	         // OBJTTMFA     Ora  prima analisi HHMMSSCC	
			entityObject.setDtLastAnalysis("20100329");	             // OBJTDTLA     Data ultima analisi AAAAMMGG
			entityObject.setTmLastAnalysis("23032300");	             // OBJTTMLA     Ora  ultima analisi HHMMSSCC	
		
			dbei.create(entityObject);

			dbei.rollback();
			dbm.closeConnection(dbConn, dbs);
			assertTrue(true);

		} catch (ExceptionAmritaSqlAccessDuplicate e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaSqlAccessNotfound e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaSqlError e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaAnnotationMissing e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmritaReflectionError e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (ExceptionAmrita e) {
			assertTrue(false);
			e.printStackTrace();
		} catch (SQLException e) {
			assertTrue(false);
			e.printStackTrace();
		} 
	}

	/**
	 * Test method for {@link analyzer.GraphManager#optimizeGraph()}.
	 */
	private  void initDefaultsAndLog() {
		// Defaults
		try {
			sd = new UserConfiguration();
		} catch (ExceptionAmrita e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        mm = new MessagesManager(sd);
        sd.setMessagesManager(mm);
        lf = new LoggerFacade(sd);
        sd.setLoggerFacade(lf);
        
		url = sd.getDataBaseUrl();
		user = sd.getDataBaseUser();
		pwd = sd.getDataBasePwd();
		dbname = sd.getDataBaseName();
		driver = sd.getDataBaseDriver();
		database = sd.getDataBaseType();
		dbs = new DataBaseStatusDetailed();
		entityObject = new EntityObject();
        
 	}

}
