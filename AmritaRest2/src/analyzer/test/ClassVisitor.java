package analyzer.test;
import java.io.*;
import java.lang.reflect.*;
import java.lang.annotation.*;

@SuppressWarnings("unchecked")
public class ClassVisitor {

   private static int spaces = 0;

   protected static void indent() {
      spaces++;
   }
   protected static void deindent() {
      spaces --;
   }
   protected static void print(String s) {
      for(int i=0 ; i<spaces ; i++)
         System.out.print(" ");
      System.out.println(s);
   }
   public static void visit(Class c) {
      if(c.isInterface())
         visitInterface(c);
      else
         visitClass(c);
   }


public static void visitClass(Class c) {
      String description = Modifier.toString(c.getModifiers())+" ";
      description = description + parseAnnotatedElement(c);
      description = description + " class " + c.getSimpleName() + parseGenericDeclaration(c)+" ";
      Class superclass = c.getSuperclass();
      if(superclass != null) {
         description = description + "extends " + superclass.getSimpleName();
         description = description + parseGenericDeclaration(superclass)+" ";
      }
      Class[] interfaces = c.getInterfaces();
      if(interfaces.length!=0) {
         description = description + "implements ";
         for(Class i : interfaces) {
            description = description + i.getSimpleName();
            description = description + parseGenericDeclaration(i) + ",";
         }
         description = description.substring(0,description.length()-1);
      }
      print(description);
      visitClassOrInterfaceDeclarations(c);
   }
   public static void visitInterface(Class c) {
      String description = Modifier.toString(c.getModifiers())+" ";
      description = description + parseAnnotatedElement(c);
      description = description + "interface" +
                           parseGenericDeclaration(c) + c.getSimpleName() + " ";
      Class[] interfaces = c.getInterfaces();
      if(interfaces.length!=0) {
         description = description + "extends ";
         for(Class i : interfaces) {
            description = description + i.getSimpleName();
            description = description + parseGenericDeclaration(i) + ",";
         }
         description = description.substring(0,description.length()-1);
      }
      print(description);
      visitClassOrInterfaceDeclarations(c);
   }

   public static void visitClassOrInterfaceDeclarations(Class c) {
      indent();
      Field[] fields = c.getDeclaredFields();
      for(Field f : fields)
         visit(f);
      Constructor[] constructors = c.getDeclaredConstructors();
      for(Constructor constructor : constructors)
         visit(constructor);
      Method[] methods = c.getDeclaredMethods();
      for(Method m : methods)
         visit(m);
      Class[] classes = c.getDeclaredClasses();
      for(Class cl : classes)
         visit(cl);
      deindent();
   }

   public static void visit(Constructor c) {
      String description = Modifier.toString(c.getModifiers())+" ";
      description = description + parseAnnotatedElement(c);
      description = description + parseGenericDeclaration(c);
      description = description + c.getName() + "(";
      Class[] parameters = c.getParameterTypes();
      if(parameters.length!=0) {
         for(Class p : parameters)
            description = description + p.getSimpleName()+parseGenericDeclaration(p)+",";
            description = description.substring(0,description.length()-1);
         }
      description = description + ")";
      Class[] exceptions = c.getExceptionTypes();
      if(exceptions.length != 0) {
         description = description + " throws ";
         for(Class e : exceptions)
            description = description + e.getSimpleName() +parseGenericDeclaration(e)+",";
            description = description.substring(0,description.length()-1);
      }
      print(description);
   }

   public static void visit(Field f) {
      String description = Modifier.toString(f.getModifiers())+" ";
      description = description + parseAnnotatedElement(f);
      description = description +(Object)f.getGenericType();
      description = description +" " +f.getName();
      print(description);
   }

   public static void visit(Method m) {
      String description = Modifier.toString(m.getModifiers())+" ";
      description = description + parseAnnotatedElement(m);
      description = description + m.getReturnType().getSimpleName() +
                           parseGenericDeclaration(m) + " ";
      TypeVariable[] typeVariables = m.getTypeParameters();
      for(TypeVariable typeVariable : typeVariables)
         description = description + typeVariable.getName() +" ";
      description = description + m.getName() +"(";
      Class[] parameters = m.getParameterTypes();
      if(parameters.length!=0) {
         for(Class p : parameters)
            description = description + p.getSimpleName()+parseGenericDeclaration(p)+",";
      description = description.substring(0,description.length()-1);
   }
   description = description + ") ";
   Class[] exceptions = m.getExceptionTypes();
   if(exceptions.length != 0) {
      description = description + "throws ";
   for(Class e : exceptions)
      description = description + e.getSimpleName() +parseGenericDeclaration(e) +",";
      description = description.substring(0,description.length()-1);
   }
   print(description);
   }

   protected static String parseAnnotatedElement(AnnotatedElement annotatedElement) {
      Annotation[] annotations = annotatedElement.getAnnotations();
      String description = "";
      for(Annotation a : annotations)
         description = description + a + " ";
         return description;
   }

   protected static String parseGenericDeclaration(GenericDeclaration genericDeclaration) {
      TypeVariable[] typeVariables = genericDeclaration.getTypeParameters();
      String description = "";
      if(typeVariables.length!=0) {
         description = "<";
         for(TypeVariable t : typeVariables) {
            description = description + t.getName();
            description = description + ",";
         }
         description = description.substring(0,description.length()-1);
         description = description + ">";
         
      }
      return description;
   }

   public static void main(String argv[]) throws Exception {
      BufferedReader lineReader = new BufferedReader(new InputStreamReader(System.in));
      System.out.println("Inserire il nome completo di una classe: ");
      String className = lineReader.readLine();
      Class c = Class.forName(className);
      visit(c);
   }
} 
