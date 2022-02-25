package utilities;

import java.util.Formatter;

public class testGeneric {

	public testGeneric() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Formatter fmt;
		Double d;
	       double myDub;
	        myDub=1234.5678;
	        long myLong;
	        myLong=(int)myDub;
	        myDub=(myDub%1)*10000;
	        int myInt=(int)myDub;
	        System.out.println(myLong + "\n" + myInt);
	        myDub=54677888.3;
	        d = new Double(myDub);
	        d.toString();
	        fmt = new Formatter();
	        myLong=(int)myDub;
	        myDub=(myDub%1)*10000;
	        myInt=(int)myDub;
	        System.out.println(myLong + "\n" + myInt);
	        
	        double n=47758896698.358765;
	        n=467.34;
	        String s = ""+n; 
	
	}

}
