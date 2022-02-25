package utilities;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import analyzer.ProcessAnalyzeSource;


/**
 * <h1>
 * ReflectionManager  
 * </h1>
 *  <p>
 * Questa classe espone tutti metodi necessari alla reflection di Java. <br>
 * Sono disponibili metodi per interrogare una classe e ottenere elenchi di metodi,
 * costruttori, interfacce, annotazioni etc. Inoltre sono disponibili metodi per caricare dinamicamente
 * classi, istanziare oggetti e invocare metodi. 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 2/11/2009
 */
@SuppressWarnings("unchecked")
public class ReflectionManager {

	// Impostata a fronte di operazione di reflection e disponibile al chiamante
	Exception exception = null;
	
	
	/**
	 * 
	 * Restituisce un oggetto Class, null se la classe non esiste.<br>
	 * Il nome dell classe deve essere completamente qualificato.<br>
	 * Nel caso di errore di esecuzione viene restituito null ed è disponibile
	 * al chiamante l'eccezione generata.
     *
	 * @param String nome completo classe
	 * @return Class className
	 */
	public  Class<?> getClass(String className) {
		
		Class<?> c = null;
        
		this.exception = null;
		try {
          c =  Class.forName(className);
         }
         catch (ClassNotFoundException e) {
        	 this.exception = e;
         }
		return c;
	}

	/**
	 * 
	 * Restituisce un oggetto Class, null se la classe non esiste.<br>
	 * Il nome dell classe NON deve essere  qualificato.<br>
	 * Il nome completa della classe, ovvero includente la sua posizione,<br>
	 * viene composto sulla base dell'istanza della classe modello fornita, <br>
	 * che deve quindi essere nella stessa directory.<br>
	 * <p>
	 * Nel caso di errore di esecuzione viene restituito null ed è disponibile
	 * al chiamante l'eccezione generata.
     *
	 * @param String nome non qualificato classe
	 * @param Object objectClassModel della stessa locazione della classe richiesta
	 * @return Class className
	 */
	public  Class<?> getClass(String className, Object objectClassModel) {
		
		Class<?> c = null;
		String classModel = objectClassModel.getClass().getName();
		int iStart = classModel.indexOf(objectClassModel.getClass().getSimpleName());
		String classExitNameComplete = classModel.substring(0, iStart) + className;
		this.exception = null;
		
		try {
          c =  Class.forName(classExitNameComplete);
         }
         catch (ClassNotFoundException e) {
        	 this.exception = e;
         }
		return c;
	}

	
	/**
	 * Restituisce l'oggetto enumeration relativo all'ordinale fornito, per la classe di input.<br>
	 * Se l'oggetto non è una enumerazione restituisce null
	 * @param Class c
	 * @return the enumeration constant object or null
	 */
	public static Object getEnumeration(Class<?> c, int ordinal) {
		Object[] ar_enumConstant = null;
		Object enumConstant = null;
		ar_enumConstant = c.getEnumConstants();
		if (ar_enumConstant == null) {return null;}
		if (ordinal >= ar_enumConstant.length) {return null;}
		enumConstant = ar_enumConstant[ordinal];
		return  enumConstant;
	}


	
	/**
	 * 
	 * Restituisce l'elenco di metodi di una classe, fornito il suo oggetto Class
	 * Vengono restituiti solo i metodi dichiarati, non quelli ereditati.
	 * 
	 * @param Class c
	 * @return Method mtd[]
	 */
	public  Method[] getMethods(Class<?> c) {
		
		Method m[] = null;
	    
		m = c.getDeclaredMethods();
		return m;
	}

	/**
	 * 
	 * Restituisce i metodi di una classe attraverso una sua istanza
	 * Vengono restituiti solo i metodi dichiarati, non quelli ereditati.
     *
	 * @param Object o
	 * @return Method mtd[]
	 */
	public  Method[] getMethods(Object o) {
		
		Class<?> c = o.getClass();
	    
		return c.getDeclaredMethods();
	}
	
	
	/**
	 * 
	 * Restituisce i nomi dei metodi di un classe con un certo nome
	 * Se la classe non è definita restituisce null.
	 * Non viene fatto alcun controllo sulla natura della classe.
	 * Nel caso di errore di esecuzione viene restituito null ed è disponibile
	 * al chiamante l'eccezione generata.
	 * 
	 * @param String className
	 * @return String methodNames[]
	 */
	public  String[] getMethodNames(String className) {
		
		Method methods[] = null;
		Class<?> c = null;  
		
		this.exception = null;
		try {
			c = Class.forName(className);
		}
		catch (ClassNotFoundException e) {
			this.exception = e;
			return null;
		}
		methods = c.getMethods();
		String[] methodNames = new String[methods.length];
		int i = 0;
		for (Method method : methods) {
			methodNames[i++] = method.getName();
		}
		return methodNames;
	}

	
	/**
	 * 
	 * Restituisce i campi della classe fornita in input.
	 * Se la classe non è definita restituisce null.
	 * Non viene fatto alcun controllo sulla natura della classe.
	 * Nel caso di errore di esecuzione viene restituito null ed è disponibile
	 * al chiamante l'eccezione generata.
	 * 
	 * @param String className
	 * @return Field[] fieldNames
	 */
	public  Field[] getFields(String className) {
		
		Field fields[] = null;
		Class<?> c = null;
		
		this.exception = null;
		try {
			c = Class.forName(className);
			fields = c.getFields();
		}
		catch (ClassNotFoundException e) {
			this.exception = e;
		}
		return fields;
	}

	/**
	 * 
	 * Restituisce i campi della classe di cui è fornito l'oggetto in input.
	 * Nel caso di errore di esecuzione viene restituito null ed è disponibile
	 * al chiamante l'eccezione generata.
	 * 
	 * @param Object className
	 * @return Field[] fieldNames
	 */
	public  Field[] getFields(Object o) {
		
		Field fields[] = null;
        Class<?> c = o.getClass();
		
        fields = c.getDeclaredFields();
		return fields;
	}

	/**
	 * 
	 * Restituisce il campo della classe di cui è fornito l'oggetto in input.
	 * Nel caso di errore di esecuzione viene restituito null ed è disponibile
	 * al chiamante l'eccezione generata.
	 * 
	 * @param Object className può essere un'istanza di un oggetto o direttamente l'oggetto classe della stessa
	 * @return Field[] fieldNames
	 */
	public  Field getField(Object o, String fieldName) {
		Class<?> c = null;
		Field field = null;
		
		if (o instanceof Class) {
			c = (Class<?>) o;
		} else {
			c = o.getClass();
		}
        
		this.exception = null;
        try {
			field = c.getDeclaredField(fieldName);
		} catch (SecurityException e) {
			this.exception = e;
		} catch (NoSuchFieldException e) {
			this.exception = e;
		}
		return field;
	}

	
	/**
	 * 
	 * Restituisce true se il metodo è definito nella classe, di cui
	 * è fornito l'oggetto Class.
	 * 
	 * @param Class class
	 * @param String methodName
	 * @return boolean isMethodDefined
	 */
	public  Boolean isMethodDefined(Class<?> c, String methodName) {
       
		boolean bDefined = false;
       
        Method aMtd[] = getMethods(c);
		for (Method method : aMtd) {
			 
			if (method.getName().equals(methodName)) {
				bDefined = true;
				break;
			}
		}
	    return bDefined;
	}

	

	/**
	 * 
	 * Restituisce true se il metodo è definito nella classe, di cui
	 * è fornito il nome completo.
	 * 
	 * @param Class class
	 * @param String methodName
	 * @return boolean isMethodDefined
	 */
	public  Boolean isMethodDefined(String className, String methodName) {
        
		boolean bDefined = false;
        Class<?> classObject = null;
        
        classObject = getClass(className);
		if (exception != null) {  // Annotazione inesistente
			return null;
		}

        Method aMtd[] = getMethods(classObject);
		for (Method method : aMtd) {
			if (method.toString().equals(methodName)) {
				bDefined = true;
				break;
			}
		}
	    return bDefined;
	}

	
	

	/**
	 * 
	 * Restituisce i nomi delle interfacce implementate da una classe con un certo nome.
	 * Se la classe non è definita restituisce null.
	 * Non viene fatto alcun controllo sulla natura della classe.
	 * Nel caso di errore di esecuzione viene restituito null ed è disponibile
	 * al chiamante l'eccezione generata.

	 * 
	 * @param String className
	 * @return String interfaceNames[]
	 */
	public  String[] getInterfaceNames(String className) {
        
		Class<?> interfaces[] = null;
        Class<?> c = null;
        
        this.exception = null;
        try {
          c = Class.forName(className);
         }
         catch (ClassNotFoundException e) {
        	 this.exception = e;
        	 return null;
         }
         interfaces = c.getInterfaces();
         String[] interfaceNames = new String[interfaces.length];
         int i = 0;
         for (Class<?> interf : interfaces) {
        	 interfaceNames[i++] = interf.getName();
		}
	    return interfaceNames;
	}

	/**
	 * 
	 * Restituisce le interfacce di una classe (sono sempre classi) attraverso una sua istanza
	 * 
	 * @param Object o
	 * @return Interface[] interfaces
	 */

	public Class<?>[] getInterfaces(Object o) {
		
		Class<?> c = o.getClass();
	    
		return c.getInterfaces();
	}

	/**
	 * 
	 * Restituisce le annotazioni di una classe attraverso una sua istanza
	 * 
	 * @param Object o
	 * @return Annotation[] interfaces
	 */
	public Annotation[] getAnnotations(Object o) {
		
		Class<?> c = o.getClass();
	    
		return c.getAnnotations();
	}

	/**
	 * 
	 * Restituisce le annotazioni di una classe di cui viene fornito l'oggetto Class
	 * 
	 * @param Class class of annotation
	 * @return Annotation[] annotation
	 */
	public Annotation[] getAnnotations(Class c) {
		@SuppressWarnings("unused")
		Annotation a = null;
		a = c.getAnnotation(c);
		return c.getAnnotations();
	}

	/**
	 * 
	 * Restituisce una annotazione di cui viene fornito l'oggetto nel quale
	 * deve essere trovata e il suo nome.
	 * 
	 * @param Object o
     * @param String annotationName
	 * @return Annotation annotation
	 */
	public Annotation getAnnotation(Object o, String annotationName) {
		
		Class<?> classObject = null;
		Class<Annotation> classAnnotation = null;
		Annotation annotation = null;
		
		// Recupero classe da oggetto fornito dove deve trovare l'annotazione
		if (o instanceof Class) {
			classObject = (Class<?>) o;
		} else {
			classObject = o.getClass();
		}
		

		// Recupero classe annotazione	
		classAnnotation = (Class<Annotation>) getClass(annotationName);   // -> this.exception
		if (this.exception != null) {  // Annotazione inesistente
			return null;
		}
		annotation = classObject.getAnnotation(classAnnotation);
		return annotation;
	}

	/**
	 * 
	 * Restituisce una annotazione di cui viene fornito l'oggetto
	 * classe dove deve essere trovata e il suo nome.
	 * 
	 * @param Class c
     * @param String annotationName
	 * @return Annotation annotation
	 */
	public Annotation getAnnotation(Class<?> classObject, String annotationName) {
		
		Class<Annotation> classAnnotation = null;
		Annotation annotation = null;
		
		// Recupero classe annotazione	
		classAnnotation = (Class<Annotation>) this.getClass(annotationName);   // -> this.exception
		if (this.exception != null) {  // Annotazione inesistente
			return null;
		}
		annotation = classObject.getAnnotation(classAnnotation);
		return annotation;
	}


	
	/**
	 * Istanzia un oggetto di una classe utilizzando il costruttore
	 * identificato dall'arry di Class come secondo parametro.
	 * Nel caso di errore di istanziazione viene restituit null ed è disponibile
	 * al chiamante l'eccezione generata.
	 * 
	 * @param Class<T> classObject
	 * @param Class[] paramiter type
	 * @param Object objectConstructor[]
	 * @return Object newObject
	 */
	public Object newInstance(Class<?> classObject, Class<?> parmConstructorType[], Object ObjectConstructor[]) {
         
		Object newObject = null;             // Oggetto da istanziare
         
		this.exception = null;
         try {
        	 Constructor<?> constructor = classObject.getConstructor(parmConstructorType);
        	 newObject  = constructor.newInstance(ObjectConstructor);
         } catch (SecurityException e) {
        	 this.exception = e;
         } catch (IllegalArgumentException e) {
        	 this.exception = e;
         } catch (NoSuchMethodException e) {
        	 this.exception = e;
         } catch (InstantiationException e) {
        	 this.exception = e;
         } catch (IllegalAccessException e) {
        	 this.exception = e;
         } catch (InvocationTargetException e) {
        	 this.exception = e;
         }
 	    return newObject;
	}

	/**
	 * 
	 * Istanzia un oggetto di una classe utilizzando il costruttore
	 * identificato dall'arry di Class come secondo parametro.
	 * Un singolo oggetto di Class si crea con className.Class (es. Integer.Class)
	 * La classe viene fornita con il suo nome completo.
	 * 
	 * Nel caso di errore di istanziazione viene restituito null ed è disponibile
	 * al chiamante l'eccezione generata.
	 * 
	 * @param String class name 
	 * @param Class[] parameter type
	 * @param Object objectConstructor[]
	 * @return Object newObject
	 */
	public Object newInstance(String className, Class<?> parmConstructor[], Object ObjectConstructor[]) {
       
		Object newObject = null;
		this.exception = null;
        try {
			newObject = null;
	  	    Class<?> classObject = (Class<?>)ClassLoader.getSystemClassLoader().loadClass(className);
		    Constructor<?> constructor = classObject.getConstructor(parmConstructor);
		    newObject  = constructor.newInstance(ObjectConstructor);
		} catch (SecurityException e) {
			this.exception = e;
		} catch (IllegalArgumentException e) {
			this.exception = e;
		} catch (ClassNotFoundException e) {
			this.exception = e;
		} catch (NoSuchMethodException e) {
			this.exception = e;
		} catch (InstantiationException e) {
			this.exception = e;
		} catch (IllegalAccessException e) {
			this.exception = e;
		} catch (InvocationTargetException e) {
			this.exception = e;
		}
        return newObject;
	}
	
	
	/**
	 * 
	 * Invoca un metodo con parametri su una classe.
	 * Se la lunghezza dell'array Class di parametri di tipo è zero, il metodo è senza parametri.
     * Un singolo oggetto di Class si crea con className.Class (es. Integer.Class)
	 * Nel caso di errore di invocazione viene restituito null ed è disponibile
	 * al chiamante l'eccezione generata.
     *
	 * @param Object o 					Oggetto su cui invocare il metodo 
     * @param String methodName		    Nome metodo da invocare
	 * @param Class methodParmType[]	Parametri metodo
	 * @param Object methodObject[]     Oggetti parametri metodo
	 * @return Object retObject			Oggetto restituito
	 */
	public  Object invokeMethod(Object o, String methodName, Class<?> methodParmType[], Object methodObject[] ) {
        
		Object retObject = null;
        Class<?> c = o.getClass();

        this.exception = null;
        try {
			Method m = c.getMethod(methodName, methodParmType);      // Metodo definito nella classe
			retObject  = m.invoke(o,methodObject);                   // Invocazione metodo sull'oggetto passato			
		} catch (SecurityException e) {
			this.exception = e;
		} catch (IllegalArgumentException e) {
			this.exception = e;
		} catch (NoSuchMethodException e) {
			this.exception = e;
		} catch (IllegalAccessException e) {
			this.exception = e;
		} catch (InvocationTargetException e) {
			this.exception = e;
		}
		return retObject;
	}

	/**
	 * 
	 * Invoca un metodo con parametri su una classe.
	 * Se la lunghezza dell'array Class di parametri di tipo è zero, il metodo è senza parametri.
     * Un singolo oggetto di Class si crea con className.Class (es. Integer.Class)<br>
     * Viene restituito al chiamante un array di oggetti:<br>
     * Il primo contiene null se il metodo è stato richiamato correttamente<br>
     * oppure è un oggetto {@link Exception} con l'eccezione generata in caso di exception.<br>
     * Il secondo elemento contiene l'oggetto eventualmente restituito dal metodo, se questo<br>
     * lo prevere, altrimenti null.<br>
     *
	 * @param Oggetto su cui invocare il metodo 
     * @param Nome metodo da invocare
	 * @param array classi parametri metodo
	 * @param oggetti parametri metodo
	 * @return array di due oggetti restituito
	 */
	public  Object[] invokeMethodWithStatus(Object o, String methodName, Class<?> methodParmType[], Object methodObject[] ) {
        
		Object[] ar_retObject = null;
        Class<?> c = o.getClass();

        ar_retObject = new Object[2];
        ar_retObject[0]  = null;                  							// Invocazione andata a buon fine		
        
        this.exception = null;
        try {
			Method m = c.getMethod(methodName, methodParmType);      		// Metodo definito nella classe
			ar_retObject[1]  = m.invoke(o,methodObject);                  	// Oggetto eventuale restituito		
		} catch (SecurityException e) {
			this.exception = e;
			ar_retObject[0] = e;
		} catch (IllegalArgumentException e) {
			this.exception = e;
			ar_retObject[0] = e;
		} catch (NoSuchMethodException e) {
			this.exception = e;
			ar_retObject[0] = e;
		} catch (IllegalAccessException e) {
			this.exception = e;
			ar_retObject[0] = e;
		} catch (InvocationTargetException e) {
			this.exception = e;
			ar_retObject[0] = e;
		}
		return ar_retObject;
	}


	/**
	 * 
	 * Invoca un metodo con parametri di una classe statica, come Enum.
	 * Se la lunghezza dell'array Class di parametri di tipo è zero, il metodo è senza parametri.
     * Un singolo oggetto di Class si crea con className.Class (es. Integer.Class)
	 * Nel caso di errore di invocazione viene restituito null ed è disponibile
	 * al chiamante l'eccezione generata.
     *
	 * @param Class c 					Classe contenente il metodo da invocare 
     * @param String methodName		    Nome metodo da invocare
	 * @param Class methodParmType[]	Parametri metodo
	 * @param Object methodObject[]     Oggetti parametri metodo
	 * @return Object retObject			Oggetto restituito
	 */
	public  Object invokeMethodStatic(Class<?> c, String methodName, Class<?> methodParmType[], Object methodObject[] ) {
        
		Object retObject = null;

		this.exception = null;
        try {
			Method m = c.getDeclaredMethod(methodName, methodParmType);      // Metodo definito nella classe
			retObject  = m.invoke(null,methodObject);                  		 // Invocazione metodo con object a null		
		} catch (SecurityException e) {
			this.exception = e;
		} catch (IllegalArgumentException e) {
			this.exception = e;
		} catch (NoSuchMethodException e) {
			this.exception = e;
		} catch (IllegalAccessException e) {
			this.exception = e;
		} catch (InvocationTargetException e) {
			this.exception = e;
		}

		return retObject;
	}
	
	
	/**
	 * @return the exception
	 */
	public Exception getException() {
		return exception;
	}


	/**
	 * @param exception the exception to set
	 */
	public void setException(Exception exception) {
		this.exception = exception;
	}
	
	
}
