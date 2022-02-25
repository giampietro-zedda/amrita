package utilities;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;


/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * DateTimeService 
 * </h1>
 *  <p>
 * Utilities di semplificazione gestione e formattazione date. Vengono esposti tutti i metodi della classe 
 * Java.Calendar. Ulteriori metodi semplificati sono disponibili  con prefisso _ (underscore).
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0	
 * @since 21/03/2010
 * @see Analyzer
 */
public class DateTimeService {

   public static final String FORMAT_YYYY_MM_DD = "yyyy-MM-dd";
   public static final String FORMAT_YYYYMMDD = "yyyyMMdd";
   public static final String FORMAT_YYYYMMDD_SLASHES = "yyyy/MM/dd";
   public static final String GENERIC_DISPLAY_FORMAT = "E, dd MMM yyyy";
   public static final String TIME_DISPLAY_FORMAT = "HH mm ss";
   public static final String TIME_DISPLAY_FORMAT_HHmmss = "HHmmss";
   public static final int LAST_WEEK = 1;
   public static final int LAST_MONTH = 2;

	private Date date = null;			// Data corrente
	
	/**
	 * 
	 * Costruttore vuoto: si considera la data corrente
	 *  
	 */
	public DateTimeService() {
		super();
		this.date = new Date();
	}

	/*
	 * 
	 * Costruttore con data specificata 
	 * SimpleDateFormat df = new SimpleDateFormat("dd-MM-yyyy");
	 * (df.format(date)
	 */
	public DateTimeService(Date date) {
		super();
		this.date = date;
	}

	/**
	 * @return the date
	 */
	public static Date getDate() {
		return new Date();
	}
	/**
	 * @param date the date to set
	 */
	public void setDate(Date date) {
		this.date = date;
	}
	
	
	/**
     * 
     * Restituissce la data corrente formattata con specificazione locale
     * Possibili maschere: dd-MM-yyyy 
     * 
     * @return
     */
	static public String getDateFormatted(Date dateInput, String pattern, Locale locale) {
	    String dateFormatted = null;
		SimpleDateFormat formatter;

		try {
			formatter = new SimpleDateFormat(pattern, locale);
			dateFormatted = formatter.format(dateInput);

		} catch (Exception e) {
			dateFormatted = "";
		}
		return dateFormatted;
	}
	
	/**
     * Restituissce la data corrente formattata
     * Possibili maschere: dd-MM-yyyy, yyyyMMdd
     * 
     * @param dateInput
     * @param patterns as an array of masks to be validated
     * @return
     */
	static public String getDateFormatted(Date dateInput, String ... patterns) {
	    String dateFormatted = null;
		SimpleDateFormat formatter;
		
		for (String pattern : patterns) {
			try {
				formatter = new SimpleDateFormat(pattern);
				dateFormatted = formatter.format(dateInput);
				return dateFormatted;
			} catch (Exception e) {
				dateFormatted = "";
				break;
			}
		}
		return dateFormatted;
	}

	/**
     * 
     * Metodo statico.
     * Restituisce l'ora corrente formattata
     * Possibili maschere: hh:mm:ss:SS, hhmmssSS <br>
     * 
     * SS indica millisecondi
     * 
     * @return
     */
	static public String getTimeFormatted(Date dateInput, String pattern) {
	    String timeFormatted = null;
		SimpleDateFormat formatter;

		try {
			formatter = new SimpleDateFormat(pattern);
			timeFormatted = formatter.format(dateInput);

		} catch (Exception e) {
			timeFormatted = "";
		}
		return timeFormatted;
	}
	
	/**
     * 
     * Metodo statico.
     * Restituisce l'ora corrente formattata
     * Possibili maschere: hh:mm:ss:SS, hhmmssSS <br>
     * 
     * SS indica millisecondi
     * 
     * @return
     */
	static public String getTimeFormatted(Date dateInput, String pattern, Locale locale) {
	    String timeFormatted = null;
		SimpleDateFormat formatter;

		try {
			formatter = new SimpleDateFormat(pattern, locale);
			timeFormatted = formatter.format(dateInput);

		} catch (Exception e) {
			timeFormatted = "";
		}
		return timeFormatted;
	}
	

    /**
     * Gets if the string date is a valid date.
     * 
     * @param date as a string to validate
     * @param mask as the format like MM/dd/yyyy, MM-dd-yyyy, MM.dd.yyyy, dd.MM.yyyy etc.
     */
	public static boolean isValidDate(String date, String mask) {
		    // set date format, this can be changed to whatever format
		    // you want, MM-dd-yyyy, MM.dd.yyyy, dd.MM.yyyy etc.
		    // you can read more about it here:
		    // http://java.sun.com/j2se/1.4.2/docs/api/index.html
		     
		    SimpleDateFormat sdf = new SimpleDateFormat(mask);
		     
		    // declare and initialize testDate variable, this is what will hold
		    // our converted string
		     
		    Date testDate = null;
		 
		    // we will now try to parse the string into date form
		    try
		    {
		      testDate = sdf.parse(date);
		    }
		 
		    // if the format of the string provided doesn't match the format we
		    // declared in SimpleDateFormat() we will get an exception
	 
		    catch (ParseException e) {
			      return false;
		    }
		 
		    // dateformat.parse will accept any date as long as it's in the format
		    // you defined, it simply rolls dates over, for example, december 32
		    // becomes jan 1 and december 0 becomes november 30
		    // This statement will make sure that once the string
		    // has been checked for proper formatting that the date is still the
		    // date that was entered, if it's not, we assume that the date is invalid
		 
		    if (!sdf.format(testDate).equals(date)) {
		        return false;
		    }
		   
		    // if we make it to here without getting an error it is assumed that
		    // the date was a valid one and that it's in the proper format
		 
		    return true;
		 
		} // end isValidDate
	
    /**
     * Gets an object date parsing a string with a mask.<br>
     * <p>
     * When a parsing error occurs, a null value will be returned.<br>
     * <p>
     * 
     * @param date as a string to parse
     * @param mask as the format like MM/dd/yyyy, MM-dd-yyyy, MM.dd.yyyy, dd.MM.yyyy etc.
     */
	public static Date getDate(String date, String mask) {
		
		    SimpleDateFormat sdf = new SimpleDateFormat(mask);
		     
		    // declare and initialize testDate variable, this is what will hold
		    // our converted string
		     
		    Date testDate = null;
		 
		    // we will now try to parse the string into date form
		    try
		    {
		      testDate = sdf.parse(date);
		    }
		 
		    // if the format of the string provided doesn't match the format we
		    // declared in SimpleDateFormat() we will get an exception
	 
		    catch (ParseException e) {
			      return null;
		    }
		 
		    // dateformat.parse will accept any date as long as it's in the format
		    // you defined, it simply rolls dates over, for example, december 32
		    // becomes jan 1 and december 0 becomes november 30
		    // This statement will make sure that once the string
		    // has been checked for proper formatting that the date is still the
		    // date that was entered, if it's not, we assume that the date is invalid
		 
		    if (!sdf.format(testDate).equals(date)) {
		        return null;
		    }
		   
		    // if we make it to here without getting an error it is assumed that
		    // the date was a valid one and that it's in the proper format
		 
		    return testDate;
		 
		} // end isValidDate
	
    /**
     * Compare two date<br>
     * <p>
     * It will be returned:<br>
     *   0 if date1 = date2<br>
     *  -1 if date1 < date2<br>
     *  +1 if date1 > date2<br>
     * <p>
     * 
     * @param date as a string to parse
     * @param mask as the format like MM/dd/yyyy, MM-dd-yyyy, MM.dd.yyyy, dd.MM.yyyy etc.
     */
	public static int compareDate(Date date1, Date date2) {
		long date1Millis = 0;
		long date2Millis = 0;
		date1Millis = getDateMillis(date1);
		date2Millis = getDateMillis(date2);
		if (date1Millis == date2Millis) {return 0;}
		if (date1Millis > date2Millis) {return 1;}
		return -1;
		 
	}  
	
	
   /**
    * Gets the current date as year-month-day
    */
   public static String getDateString() {
      Calendar cal = Calendar.getInstance();
      cal.setTime(new Date() );

      int year = cal.get( Calendar.YEAR );
      int month = cal.get( Calendar.MONTH ) + 1;
      int day = cal.get( Calendar.DAY_OF_MONTH );

      return "" + year
             + "-" + month
             + "-" + day;
   }

   /**
    * Gets a long value with the millisecind date<br>
    * <p>
    * @param dt as a Date object
    * @return the milliseconds value
    */
   public static final long getDateMillis(Date dt) {
	     GregorianCalendar cal = new GregorianCalendar();
	     cal.setTime(dt);
	     return cal.getTimeInMillis();
	   }

   /**
    * Gets the current time formatted string hh:mm:ss:ms
    */
   public static String getTime() {
      Calendar cal = Calendar.getInstance();
      cal.setTime( new Date() );

      int hours = cal.get( Calendar.HOUR_OF_DAY );
      // use 24 hour clock
      int minutes = cal.get( Calendar.MINUTE );
      int seconds = cal.get( Calendar.SECOND );
      int milli = cal.get( Calendar.MILLISECOND );

      return formatTime( hours, minutes, seconds, milli );
   }

   /**
    * Gets the current time formatted timestamp as year-month-day hh:mm:ss:ms
    */
   public static String getTimestamp() {
      Calendar cal = Calendar.getInstance();
      cal.setTime( new Date() );

      int year = cal.get( Calendar.YEAR );
      int month = cal.get( Calendar.MONTH ) + 1;
      int day = cal.get( Calendar.DAY_OF_MONTH );
      int hours = cal.get( Calendar.HOUR_OF_DAY );
      // use 24 hour clock
      int minutes = cal.get( Calendar.MINUTE );
      int seconds = cal.get( Calendar.SECOND );
      int milli = cal.get( Calendar.MILLISECOND );

      return "" + year
             + "-" + month
             + "-" + day
             + "_" + formatTime( hours, minutes, seconds, milli );
   }

   private static String formatTime( int hours, int minutes, int seconds, int milli )
   {
      StringBuffer buf = new StringBuffer();
      buf.append( "" + hours );

      buf.append( "." );

      if( minutes < 10 )
         buf.append( "0" + minutes );
      else
         buf.append( "" + minutes );

      buf.append( "." );

      if( seconds < 10 )
         buf.append( "0" + seconds );
      else
         buf.append( "" + seconds );

      buf.append( "-" );

      if( milli < 10 )
         buf.append( "00" + milli );
      else if( milli < 100 )
         buf.append( "0" + milli );
      else
         buf.append( "" + milli );

      return buf.toString();
   }
   

   public static final String formatDate(Date dt, String format) {
     GregorianCalendar cal = new GregorianCalendar();
     cal.setTime(dt);
     
     java.text.SimpleDateFormat sdf = new java.text.SimpleDateFormat(format);
     sdf.setTimeZone(TimeZone.getDefault());     
     return (sdf.format(cal.getTime()));   
   }
   
   public static final String getCurrentDate(String format) {
     Calendar cal = Calendar.getInstance(TimeZone.getDefault());
       java.text.SimpleDateFormat sdf = new java.text.SimpleDateFormat(format);
       sdf.setTimeZone(TimeZone.getDefault());     
       return (sdf.format(cal.getTime()));
   }
   
   public static final String dateToString(Date dt, String dateformat) {
     GregorianCalendar cal = new GregorianCalendar();
     cal.setTime(dt);
     
     StringBuffer ret = new StringBuffer();
     String separator = new String();
     if(dateformat.equals(FORMAT_YYYYMMDD) ) {
       separator = "-";
     }
     if(dateformat.equals(FORMAT_YYYYMMDD_SLASHES) ) {
       separator = "/";
     }
     ret.append(cal.get(Calendar.YEAR));
     ret.append(separator);
     ret.append(cal.get(Calendar.MONTH) + 1);
     ret.append(separator);
     ret.append(cal.get(Calendar.DATE));

     return ret.toString();
   }
   
   public static final String dateToString(Date dt, String tzString, String dateformat) {
     GregorianCalendar cal = new GregorianCalendar();
     cal.setTime(dt);
     cal.setTimeZone(TimeZone.getTimeZone(tzString));
     
     StringBuffer ret = new StringBuffer();
     String separator = new String();
     if(dateformat.equals(FORMAT_YYYYMMDD) ) {
       separator = "-";
     }
     if(dateformat.equals(FORMAT_YYYYMMDD_SLASHES) ) {
       separator = "/";
     }
     ret.append(cal.get(Calendar.YEAR));
     ret.append(separator);
     ret.append(cal.get(Calendar.MONTH) + 1);
     ret.append(separator);
     ret.append(cal.get(Calendar.DATE));

     return ret.toString();
   }

   public static final String getTimeFromDate(Date dt) {
     Calendar cal = new GregorianCalendar();
     cal.setTime(dt);
     
     StringBuffer ret = new StringBuffer();
     ret.append(cal.get(Calendar.HOUR));
     ret.append(":");
     ret.append(cal.get(Calendar.MINUTE));
     
     return ret.toString();
   }
   
   public static final String getTimeFromDate(Date dt, String tzString) {
     try {
       GregorianCalendar gc = new GregorianCalendar();
       gc.setTime(dt);
       gc.setTimeZone(TimeZone.getTimeZone(tzString));
       StringBuffer ret = new StringBuffer();
       ret.append(gc.get(Calendar.HOUR));
       ret.append(":");
       ret.append(gc.get(Calendar.MINUTE));
       ret.append(" ");
       if(gc.get(Calendar.AM_PM) == 0) {
         ret.append("AM");
       }
       else { 
         ret.append("PM");
       }
       return ret.toString();
     }
     catch(Exception e) {
       return "";
     }
   }
   
   public static final String getDateTimeFromDate(Date dt, String tzString) {
     try {
       GregorianCalendar gc = new GregorianCalendar();
       gc.setTime(dt);
       gc.setTimeZone(TimeZone.getTimeZone(tzString));
       StringBuffer ret = new StringBuffer();
       ret.append(gc.get(Calendar.YEAR));
       ret.append("-");
       ret.append(gc.get(Calendar.MONTH) - 1);
       ret.append("-");
       ret.append(gc.get(Calendar.DATE));
       ret.append(" ");
       ret.append(gc.get(Calendar.HOUR));
       ret.append(":");
       ret.append(gc.get(Calendar.MINUTE));
       ret.append(" ");
       if(gc.get(Calendar.AM_PM) == 0) {
         ret.append("AM");
       }
       else { 
         ret.append("PM");
       }
       return ret.toString();
     }
     catch(Exception e) {
       return "";
     }
   }
   
   public static final String calendarToString(Calendar cal, String dateformat) {
     StringBuffer ret = new StringBuffer();
     if(dateformat.equals(FORMAT_YYYYMMDD) ) {
       ret.append(cal.get(Calendar.YEAR));
       ret.append("-");
       
       String month = null;
       int mo = cal.get(Calendar.MONTH) + 1; /* Calendar month is zero indexed, string months are not */
       if(mo < 10) {
         month = "0" + mo;
       }
       else {
         month = "" + mo;
       }
       ret.append(month);      
       
       ret.append("-");
       
       String date = null;
       int dt = cal.get(Calendar.DATE);
       if(dt < 10) {
         date = "0" + dt;
       }
       else {
         date = "" + dt;
       }
       ret.append(date);
     }
     
     return ret.toString();
   }


   
   public static final GregorianCalendar getCurrentCalendar(String utimezonestring) {
     try {
       GregorianCalendar gc = new GregorianCalendar();

       gc.setTimeZone(TimeZone.getTimeZone(utimezonestring));
       return gc;
     }
     catch(Exception e) {
       //If exception, return server TimeStamp
       return new GregorianCalendar();
     }
   }
   
   public static String[] getDateRange(int cmd) {
     GregorianCalendar gc = new GregorianCalendar();
     GregorianCalendar gc2 = new GregorianCalendar();
     
     SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd"); //$NON-NLS-1$
     String ret[] = new String[2];
     ret[1] = sdf.format(gc.getTime());
     
     if(cmd == LAST_WEEK) {      
       for(int i = 0; i < 7; i++) {
         gc2.add(Calendar.DATE, -1); 
       }
       
     }
     if(cmd == LAST_MONTH) {
       gc2.add(Calendar.MONTH, -1);    
     }
     ret[0] = sdf.format(gc2.getTime());
     return ret;
   }


   public static final String getDayString(int day) {
     switch (day) {
       case Calendar.SUNDAY:
         return "SUNDAY";      
       case Calendar.MONDAY:
         return "MONDAY";
       case Calendar.TUESDAY:
         return "TUESDAY";
       case Calendar.WEDNESDAY:
         return "WEDNESDAY";
       case Calendar.THURSDAY:
         return "THURSDAY";
       case Calendar.FRIDAY:
         return "FRIDAY";
       case Calendar.SATURDAY:
         return "SATURDAY";
     }
     return "";
   }
	   
}
