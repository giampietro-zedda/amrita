package dao;


public abstract class DAOFactory {
	 
   /** Membro statico per la factory MySQL */
   public static final int MYSQL = 0;  
   /** Membro statico per la factory Oracle */
   public static final int ORACLE = 1;


   /**
    * Metodo Factory
    * 
    * @param database
    *            il database da scegliere
    * @return la factory corrispondente
    */
   public static DAOFactory getDAOFactory(int database) {
       switch (database) {
       case MYSQL:
           return new MySQLDAOFactory();
       case ORACLE:
            return null;
       default:
           return null;
       }
   }
   
}