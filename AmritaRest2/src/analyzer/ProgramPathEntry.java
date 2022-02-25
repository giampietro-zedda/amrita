package analyzer;

import java.util.ArrayList;

/*
 * -----------------------
 * ProgramPathInstruction
 * -----------------------
 * 
 * Descrive una singola istruzione di un path di esecuzione
 * Nel caso di perform può elencare il dettaglio dei paragrafi richiamati (range inizio e fine)
 * 
 */
public class ProgramPathEntry {
	
	private int lvlDeep = 0;                        // Da 0, livello annidamento perform
	private boolean isPerformLevel0 = false;        // True se perform primo livello stack execution perform
	
	// Descriptor Perform to paragraph or generic instruction
	private boolean isPerform = false;              // True if the instruction is a perform
    private int numInstr = 0;        		        // Numero istruzione (perform o altro)
    private int rowStartSource = 0;        			// Numero riga start istruzione   
    private int rowEndSource = 0;        		    // Numero riga end istruzione     
    private String underCopyName = "";			    // Copy where the instruction/perform is defined
    private int numInstrIfOwner = 0;      	        // instruction/Perform a paragrafo sotto condizione IF 
    private String sourceInstr = "";                // Source instruction
    
    // Info called Paragraph 
    private int numInstrStartParagrah = 0;     		// Numero istruzione inizio paragrafo
    private int numInstrEndParagrah = 0;     		// Numero istruzione fine paragrafo
    private int rowStartParagrah = 0;     			// Numero riga start paragrafo
    private int rowEndParagrah = 0;     			// Numero riga end paragrafo
    private String idParagrah = "";       			// Perform a paragrafo
    private String idParagrahThru = "";       		// Perform thru
    private String paragraphUnderCopy = "";			// Copy where the Paragraph is defined 
      
    // Può essere valorizzato se isPerform = true
    // L'array list descrive inizio e fine di tutti i paragrafi richiamati a qualsisia livello di profondità
    private ArrayList<ProgramPathParagraph> calledParagraphs = null;       
    
    public ProgramPathEntry() {
		super();
	}

	/**
	 * @return the lvlDeep
	 */
	public int getLvlDeep() {
		return lvlDeep;
	}

	/**
	 * @param lvlDeep the lvlDeep to set
	 */
	public void setLvlDeep(int lvlDeep) {
		this.lvlDeep = lvlDeep;
	}

	/**
	 * @return the numInstr of the perform to paragraph or the instruction 
	 */
	public int getNumInstr() {
		return numInstr;
	}

	/**
	 * @param numInstrPerform the numInstr to the paragraph to set
	 */
	public void setNumInstr(int numInstr) {
		this.numInstr = numInstr;
	}

	/**
	 * @return the instruction/perform numInstrIfOwner 
	 */
	public int getNumInstrIfOwner() {
		return numInstrIfOwner;
	}

	/**
	 * @param numInstrIfOwner the numInstrIfOwner to perform to set
	 */
	public void setNumInstrIfOwner(int numInstrIfOwner) {
		this.numInstrIfOwner = numInstrIfOwner;
	}

	
	/**
	 * @return the srcInstr
	 */
	public String getSourceInstr() {
		return sourceInstr;
	}

	/**
	 * @param srcInstr the srcInstr to set
	 */
	public void setSourceInstr(String srcInstr) {
		this.sourceInstr = srcInstr;
	}

	/**
	 * @param underCopyName the copy where the instruction/perform is to set
	 */
	public void setUnderCopyName(String underCopyName) {
		this.underCopyName = underCopyName;
	}

	
	/**
	 * @return the underCopyName
	 */
	public String getUnderCopyName() {
		return underCopyName;
	}

	/**
	 * @return the numInstrStartParagrah
	 */
	public int getNumInstrStartParagrah() {
		return numInstrStartParagrah;
	}

	/**
	 * @param numInstrStartParagrah the numInstrStartParagrah to set
	 */
	public void setNumInstrStartParagrah(int numInstrStartParagrah) {
		this.numInstrStartParagrah = numInstrStartParagrah;
	}

	/**
	 * @return the numInstrEndParagrah
	 */
	public int getNumInstrEndParagrah() {
		return numInstrEndParagrah;
	}

	/**
	 * @param numInstrEndParagrah the numInstrEndParagrah to set
	 */
	public void setNumInstrEndParagrah(int numInstrEndParagrah) {
		this.numInstrEndParagrah = numInstrEndParagrah;
	}

	/**
	 * @return the idParagrah
	 */
	public String getIdParagrah() {
		return idParagrah;
	}

	/**
	 * @param idParagrah the idParagrah to set
	 */
	public void setIdParagrah(String idParagrah) {
		this.idParagrah = idParagrah;
	}

	/**
	 * @return the copy name where the paragraph is defined
	 */
	public String getParagraphUnderCopy() {
		return paragraphUnderCopy;
	}

	/**
	 * @param paragraphUnderCopy the copy where the paragraph is defined to set
	 */
	public void setParagraphUnderCopy(String paragraphUnderCopy) {
		this.paragraphUnderCopy = paragraphUnderCopy;
	}

	/**
	 * @return the rowStartSource
	 */
	public int getRowStartSource() {
		return rowStartSource;
	}

	/**
	 * @param rowStartSource the rowStartSource to set
	 */
	public void setRowStartSource(int rowStartSource) {
		this.rowStartSource = rowStartSource;
	}

	/**
	 * @return the rowEndSource
	 */
	public int getRowEndSource() {
		return rowEndSource;
	}

	/**
	 * @param rowEndSource the rowEndSource to set
	 */
	public void setRowEndSource(int rowEndSource) {
		this.rowEndSource = rowEndSource;
	}

	/**
	 * @return the rowStartParagrah
	 */
	public int getRowStartParagrah() {
		return rowStartParagrah;
	}

	/**
	 * @param rowStartParagrah the rowStartParagrah to set
	 */
	public void setRowStartParagrah(int rowStartParagrah) {
		this.rowStartParagrah = rowStartParagrah;
	}

	/**
	 * @return the rowEndParagrah
	 */
	public int getRowEndParagrah() {
		return rowEndParagrah;
	}

	/**
	 * @param rowEndParagrah the rowEndParagrah to set
	 */
	public void setRowEndParagrah(int rowEndParagrah) {
		this.rowEndParagrah = rowEndParagrah;
	}

	/**
	 * @return the isPerform
	 */
	public boolean isPerform() {
		return isPerform;
	}

	/**
	 * @param isPerform the isPerform to set
	 */
	public void setPerform(boolean isPerform) {
		this.isPerform = isPerform;
	}

	/**
	 * @return the calledParagraphs
	 */
	public ArrayList<ProgramPathParagraph> getCalledParagraphs() {
		return calledParagraphs;
	}

	/**
	 * @param calledParagraphs the calledParagraphs to set
	 */
	public void setCalledParagraphs(ArrayList<ProgramPathParagraph> calledParagraphs) {
		this.calledParagraphs = calledParagraphs;
	}

	
	/**
	 * @return the idParagrahThru
	 */
	public String getIdParagrahThru() {
		return idParagrahThru;
	}

	/**
	 * @param idParagrahThru the idParagrahThru to set
	 */
	public void setIdParagrahThru(String idParagrahThru) {
		this.idParagrahThru = idParagrahThru;
	}

	
	/**
	 * @return the isPerformLevel0
	 */
	public boolean isPerformLevel0() {
		return isPerformLevel0;
	}

	/**
	 * @param isPerformLevel0 the isPerformLevel0 to set
	 */
	public void setPerformLevel0(boolean isPerformLevel0) {
		this.isPerformLevel0 = isPerformLevel0;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return ""
				+ "isPerform=" + isPerform + ", numInstr=" + numInstr
				+ ", rowStartSource=" + rowStartSource + ", rowEndSource=" + rowEndSource + ", underCopyName="
				+ underCopyName + ", performNumInstrIfOwner=" + numInstrIfOwner + ", numInstrStartParagrah="
				+ numInstrStartParagrah + ", numInstrEndParagrah=" + numInstrEndParagrah + ", rowStartParagrah="
				+ rowStartParagrah + ", rowEndParagrah=" + rowEndParagrah + ", idParagrah=" + idParagrah
				+ ", paragraphUnderCopy=" + paragraphUnderCopy + ", calledParagraphs=" + calledParagraphs + "]";
	}

	
}
