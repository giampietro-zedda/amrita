package analyzer;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;



/**
 * copyright (c) 2010-2012 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseMappedForEach  Annotation
 * </h1>
 *  <p>
 *  
 * This annotation must be used with entity definition classes like, for example, {@link EntityMetric}.<br>
 * It's used to map entities with a relationship <code>FOR_EACH</code>, to say one to many.<br>
 * When <code>FOR_EACH</code> entities are mapped to an entity by means of this annotation, related entities are automatically<br>
 * available to the forward logical data view declaring they.<br>
 * Informations declared by this annotation are used to get all key columns necessary to access to related entity.<br>
 * You are not concerned if the phisical database defines or not necessary keys.<br>
 * <p>
 * It must be specified the entity name in one to many relationship and a list of couples of java columns name.<br>
 * The first java column name is the field name in the entity where the annotation is declared, whereas the second<br>
 * java column name is the java column name to match in the entity <code>FOR_EACH</code> relationship with.<br>
 * <p>
 * Many <code>FOR_EACH</code> can be specified.<br>
 * <p>
 * Here is an example to declare a relationship  <code>FOR_EACH</code> between {@link EntityMetric} and {@link EntityMetricViolation}:<pre>
 '@DataBaseMappedForEach(forEach = {
			'@forEachEntity(entity = "EntityMetricViolation",
			   colsBound = {"system 	system"
				    	   ,"subSystem 	subSystem"    
					       ,"scope 	     scope"    
					       ,"idObject 	idObject"    
					       ,"typeObject typeObject"  
					       ,"section     section"  
					       }
			   )
		   }
)
   </pre>
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 1/03/2012
 * @see DataBaseManager  
*/
@Retention(RetentionPolicy.RUNTIME)
public @interface DataBaseMappedForEachs {
	DataBaseMappedForEach[] forEach();
}
