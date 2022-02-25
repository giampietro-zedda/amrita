package analyzer;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;



/**
 * copyright (c) 2010-2012 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseMappedForAny  Annotation
 * </h1>
 *  <p>
 *  
 * This annotation must be used with entity definition classes like, for example, {@link EntityMetricViolation}.<br>
 * It's used to map entities with a relationship <code>FOR_ANY</code>, to say many to one.<br>
 * When <code>FOR_ANY</code> entities are mapped to an entity by means of this annotation, related entities are automatically<br>
 * available to the forward logical data view declaring they.<br>
 * Informations declared by this annotation are used to get all key columns necessary to access to related entity.<br>
 * You are not concerned if the phisical database defines or not necessary keys.<br>
 * <p>
 * It must be specified the entity name in many to one relationship and a list of couples of java columns name.<br>
 * The first java column name is the field name in the entity where the annotation is declared, whereas the second<br>
 * java column name is the java column name to match in the entity <code>FOR_ANY</code> relationship with.<br>
 * <p>
 * Here is an example to declare a relationship  <code>FOR_ANY</code> between {@link EntityMetricViolation} and {@link EntityMetric}:<pre>
'@DataBaseMappedForAny(forAnyEntity = "EntityMetric",
			  forAnyColsBound = {"system 	system"
				    	  ,"subSystem 	subSystem"    
					  ,"scope 	scope"    
					  ,"idObject 	idObject"    
					  ,"typeObject 	typeObject"  
					  ,"section 	section"  
					  }

   </pre>
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 1/03/2012
 * @see DataBaseManager  
*/
@Retention(RetentionPolicy.RUNTIME)
public @interface DataBaseMappedForAnys {
	DataBaseMappedForAny[] forAny();
}
