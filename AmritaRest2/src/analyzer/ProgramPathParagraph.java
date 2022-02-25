package analyzer;

/*
 * -------------------- 
 * ProgramPathParagraph
 * -------------------- 
 * 
 * Descrive un singolo paragrafo in termini di inizio/fine istruzione/riga
 * Utilizzato da ProgramPathInstruction per elencare i paragrafi richiamati dalla struttura
 * principale del path a qualsiasi livello di profondità
 * 
 */
public class ProgramPathParagraph {
	
    private String idParagrah = "";       			// Nome paragrafo/Section
    private String idParagrahThru = "";       		// Nome paragrafo/Section thru
    private int numInstrStartParagrah = 0;     		// Numero istruzione inizio paragrafo
    private int numInstrEndParagrah = 0;     		// Numero istruzione fine paragrafo
    private int rowStartParagrah = 0;     			// Numero riga start paragrafo
    private int rowEndParagrah = 0;     			// Numero riga end paragrafo
    private String paragraphUnderCopy = "";			// Copy where the Paragraph is defined 
       

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

}
