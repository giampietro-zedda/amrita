package junit.test;
import static org.junit.jupiter.api.Assertions.*;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import javax.persistence.Entity;
import javax.persistence.Column;
import javax.persistence.Id;

import org.junit.jupiter.api.Test;
import analyzer.DataBaseItemDescriptor;
import entities.EntityObject;
import utilities.ReflectionManager;

class TestDbnew {

	@Test
	void test() {
		EntityObject o = new EntityObject();
		ReflectionManager rm = new ReflectionManager();
		String entityName = "";
		String fieldName = "";
		String columnName = "";
		Entity e = null;
		Column c = null;
		Id id = null;
		
		// Get Table name
		e = (Entity) rm.getAnnotation(o.getClass(), "javax.persistence.Entity");
		entityName = e.name();
		
		// Get Entity fields/columns
		Field[] arField = rm.getFields(o);
		arField = o.getClass().getDeclaredFields();
		for (Field f : arField) {
			fieldName = f.getName();
			
			// Get Column/Id annotation
			c = (Column) f.getAnnotation(Column.class);
			columnName = c.name();
			id = (Id) f.getAnnotation(Id.class);
		}
		
		
		fail("Not yet implemented");
	}

	private DataBaseItemDescriptor[] getEntityFields(EntityObject o) {
		ReflectionManager rm = new ReflectionManager();
		String entityName = "";
		Annotation a = null;
		a = (javax.persistence.Entity) rm.getAnnotation(o, "Entities.Entity");
		entityName = a.toString();
		
		
		return null;
	}
		
	/*
	 * Restituisce il nome della tabella da @Entity   
	*/
	private String getTableName(EntityObject object) {
		
		return null;
	}
			
	
}

