package junit.test;
import static org.junit.Assert.assertTrue;

import java.sql.Connection;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import analyzer.DataBaseConnections;
import analyzer.UserActive;
import analyzer.UserConfiguration;
import dao.DAOFactory;
import dao.IDAOObject;
import dao.MySQLDAOFactory;
import dao.DAOImplObject;
import entities.EntityObject;
import enums.EnumObject;
import enums.EnumObjectStatus;
import enums.EnumSourceType;

public class TestDAO_CRUD {


	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

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
	 * Test Read
	 * @throws Exception 
	 */
	@Test
	public final void testCRUD() throws Exception {
		UserConfiguration ucfg = null;
		EntityObject object = null;
		boolean res = false;

		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		ucfg=UserActive.getUser("amrita");
		Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO =  (DAOImplObject) sqlFactory.getDAOObject(conn, false,true, ucfg);

		
		// Create 1
		object=new EntityObject();
		object.setSystem("KK");
		object.setSubSystem("ZZ");
		object.setIdObject("PDM0005");
		object.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
		object.setTypeSource(EnumSourceType.COBOL_PROGRAM);
		object.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS);
		object.setSuffixFileSource("CBL");
		res=eoDAO.create(object);
// 		assertTrue(res == true);

		// Read 
		object=new EntityObject();
		object.setSystem("KK");
		object.setSubSystem("ZZ");
		object.setIdObject("PDM0005");
		object.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
		object.setTypeSource(EnumSourceType.COBOL_PROGRAM);
		res=eoDAO.read(object);
		assertTrue(res && object.getSuffixFileSource().equals("CBL"));	

		// Update 
		object=new EntityObject();
		object.setSystem("KK");
		object.setSubSystem("ZZ");
		object.setIdObject("PDM0005");
		object.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
		object.setTypeSource(EnumSourceType.COBOL_PROGRAM);
		object.setSuffixFileSource("ECO");
		object.setLibrarySource("NEWLIBRARY");
		res=eoDAO.update(object);
 		assertTrue(res == true);

 		// Delete 1
		object=new EntityObject();
		object.setSystem("KK");
		object.setSubSystem("ZZ");
		object.setIdObject("PDM0005");
		object.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
		object.setTypeSource(EnumSourceType.COBOL_PROGRAM);
		res=eoDAO.delete(object);
		assertTrue(res == true);
		
		eoDAO.releaseConn();
	}



	private void deleteObjects(IDAOObject eoDAO) throws Exception {

		EntityObject object = null;
		boolean res = false;
		
		// Delete 2
		object=new EntityObject();
		object.setSystem("KK");
		object.setSubSystem("ZZ");
		object.setIdObject("PDM0001");
		object.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
		object.setTypeSource(EnumSourceType.COBOL_PROGRAM);
		res=eoDAO.delete(object);
		assertTrue(res == true); 	
		object.setIdObject("PDM0002");
		res=eoDAO.delete(object);
		assertTrue(res == true); 	
		object.setIdObject("PDM0003");
		res=eoDAO.delete(object);
		assertTrue(res == true); 	
		object.setIdObject("PDM0004");
		object.setTypeObject(EnumObject.OBJECT_COPY_COBOL_PROC);
		object.setTypeSource(EnumSourceType.COBOL_COPY_PROC);
		res=eoDAO.delete(object);
		assertTrue(res == true); 	
		
	}

	private void createObjects(IDAOObject eoDAO) throws Exception {

		EntityObject object = new EntityObject();
		boolean res = false;
		
		object.setSystem("KK");
		object.setSubSystem("ZZ");
		object.setIdObject("PDM0001");
		object.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
		object.setTypeSource(EnumSourceType.COBOL_PROGRAM);
		object.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS);
		object.setSuffixFileSource("CBL");
		res = eoDAO.create(object);
 		assertTrue(res == true);		
		object.setIdObject("PDM0002");
		res = eoDAO.create(object);
 		assertTrue(res == true);		
		object.setIdObject("PDM0003");
		res = eoDAO.create(object);
 		assertTrue(res == true);		
		object.setIdObject("PDM0004");
		object.setTypeObject(EnumObject.OBJECT_COPY_COBOL_PROC);
		object.setTypeSource(EnumSourceType.COBOL_COPY_PROC);
		object.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_EXCEPTION);
		res = eoDAO.create(object);
 		assertTrue(res == true);		
	}
}
