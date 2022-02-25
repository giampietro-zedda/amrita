package junit.test;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.sql.Connection;
import java.util.List;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;

import org.glassfish.jersey.client.ClientConfig;
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

public class TestREST {


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


	@Test
	public final void testFindAllBySysTypeStatus() throws Exception {
		UserConfiguration ucfg = null;
		List<EntityObject> ls_object = null;

		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		ucfg=UserActive.getUser("amrita");
		
		Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO =  (DAOImplObject) sqlFactory.getDAOObject(conn, false,true, ucfg);
		
		createObjects(eoDAO);
		
		ls_object=eoDAO.findAll("KK",  EnumObject.OBJECT_COPY_COBOL_PROC, EnumObjectStatus.OBJECT_ANALYZED_WITH_EXCEPTION);       
		if (ls_object == null) {
			deleteObjects(eoDAO);
			assertTrue(false); 	
		}

		String webServiceURI = "http://localhost:8080/AmritaRest2/rest/object";
		
		ClientConfig clientConfig = new ClientConfig();
		Client client = ClientBuilder.newClient(clientConfig);
		URI serviceURI = UriBuilder.fromUri(webServiceURI).build();
		WebTarget webTarget = client.target(serviceURI);  
    
		//1 findAllBySysType /sys/*/type/*
		System.out.println(webTarget.path("KK").path("*").path("1").path("*").request()
				.accept(MediaType.APPLICATION_JSON_TYPE).get(String.class));

		//2 findAllBySysSubSysType /sys/subSys/type/*
		System.out.println(webTarget.path("KK").path("ZZ").path("1").path("*").request()
				.accept(MediaType.APPLICATION_JSON_TYPE).get(String.class));
		
		//3 findAllBySysSubSysStatus /sys/subSys/*/status
		System.out.println(webTarget.path("KK").path("ZZ").path("*").path("6").request()
				.accept(MediaType.APPLICATION_JSON_TYPE).get(String.class));
		
		//4 findAllBySysSubSysTypeStatus /sys/subSys/type/status
		System.out.println(webTarget.path("KK").path("ZZ").path("1").path("6").request()
				.accept(MediaType.APPLICATION_JSON_TYPE).get(String.class));
		
		//5 findAllBySysTypeStatus /sys/*/type/status
		System.out.println(webTarget.path("KK").path("*").path("1").path("4").request()
				.accept(MediaType.APPLICATION_JSON_TYPE).get(String.class));
		
		deleteObjects(eoDAO);
		assertTrue(true);			
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
		object.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS); // 4
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
		object.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_EXCEPTION); // 6
		res = eoDAO.create(object);
 		assertTrue(res == true);		
	}
}


