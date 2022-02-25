package analyzer;

import java.util.Arrays;

/*
 * ---------------
 * PathInstruction
 * ---------------
 * 
 * Descrive un path di esecuzione
 * 
 * 
 */
public class ProgramPath {
	private String idPath = "";
    private int fromInstr = 0;
    private int toInstr = 0;
    private ProgramPathEntry ar_entry[] = null;	
 
    public ProgramPath(String idPath, int fromInstr, int toInstr ) {
		this.idPath = idPath;
		this.fromInstr = fromInstr;
		this.toInstr = toInstr;
	}

    
    
	/**
	 * @return the idPath
	 */
	public String getIdPath() {
		return idPath;
	}



	/**
	 * @param idPath the idPath to set
	 */
	public void setIdPath(String idPath) {
		this.idPath = idPath;
	}



	/**
	 * @return the fromInstr
	 */
	public int getFromInstr() {
		return fromInstr;
	}



	/**
	 * @param fromInstr the fromInstr to set
	 */
	public void setFromInstr(int fromInstr) {
		this.fromInstr = fromInstr;
	}



	/**
	 * @return the toInstr
	 */
	public int getToInstr() {
		return toInstr;
	}



	/**
	 * @param toInstr the toInstr to set
	 */
	public void setToInstr(int toInstr) {
		this.toInstr = toInstr;
	}



	/**
	 * @return the entries of the path
	 */
	public ProgramPathEntry[] getEntries() {
		return ar_entry;
	}


	/**
	 * @param instr the instr to set
	 */
	public void setEntries(ProgramPathEntry[] entries) {
		this.ar_entry = entries;
	}



	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Path [idPath=" + idPath + ", fromInstr=" + fromInstr + ", toInstr=" + toInstr + ", entries="
				+ Arrays.toString(ar_entry) + "]";
	}

    
}
