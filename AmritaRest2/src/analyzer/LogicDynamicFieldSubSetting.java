package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import entities.EntityDynamicFieldSubSetting;
import entities.EntityDynamicFieldSubWaitExt;

/**
 *   ---------------------------------------------------------------------------------
 *   Classe contenitore di servizio con una trasformazione di un sottocampo. <br>
 *   ---------------------------------------------------------------------------------
 *   <p>
 *   Nel processo di trasformazione il sottocampo elementare origine può 
 *   trasformarsi in un insieme di ulteriori sottocampi di lunghezza <.<br>
 *   Quindi nel processo ricorsivo di trasformazioni il sottocampo origine
 *   potrebbe non essere più l'unità atomica di cui individuare i valori.<br>
 *   <p>
 *   Per esempio potrei avere:<br>
 *   
 *   05 SUBFIELD PIC X(6).<br>
 *   ...<br>
 *   ...<br>
 *   05 F12.<br>
 *      07 F1   PIC XXX.<br>
 *      07 F2   PIC XXX.<br>
 *   ...<br>
 *   ...<br>
 *   MOVE SUBFIELD TO F12.<br>
 *   ...
 *   MOVE '001' TO F1.<br>
 *   ...<br>
 *   MOVE '002' TO F1.<br>
 *   ...<br>
 *   MOVE 'ZZZ' TO F2<br>
 *   ...<br>
 *   <p>
 *   Generando i valori 001ZZZ e 002ZZ.<br>
 *   <p>
 *   Per gestire anche questa problematica, in aggiunta all'individuazione
 *   dei valori diretti e indiretti del sottocampo origine, ogni assegnazione
 *   contiene le seguenti informazioni, relative sottocampo origine:<br>
 *   <p>
 *   1) Posizione 1-based  <br>
 *   2) lunghezza <br>
 *   
 *   Il default è 1 per la posizione e le dimensioni del sottocampo per la lunghezza.<br>
 *   In fase di ricomposizione dei valori, per ogni catena di trasformazioni, si terrà
 *   conto della valorizzazione anche solo parziale dei sottocampi.
 *   
 */
public class LogicDynamicFieldSubSetting implements Serializable, Cloneable {
	
	private static final long serialVersionUID = 1L;
    
	// Descrittore completo trasformazione in formato database
	public EntityDynamicFieldSubSetting entityDynamicFieldSetting = null;
	
	// Descrittore completo richiesta dati da media esterni da database
	// Valido in caso di ultima assegnazione con dati da EIBTRMID/EIBTRNID/File/Table,...
	public EntityDynamicFieldSubWaitExt entityDynamicFieldSubWaitExt = null;
	
	// Se true indica trasformazione già aggiornata su database.
	// Utile se attivazione a fronte di LogicSpreadedPgm per inserire leù
	// Sole trasformazioni effettuate nei programmi chiamanti
	public boolean isDbUpdated = false;
	
	// Valido per ultima assegnazione di catena di trasformazioni.
	// true per Ultima assegnazione risolta, con generazione di valori.
	// Se true valore presente in entityDynamicFieldSetting, se valore singolo
	// oppure in al_Value, se valori multipli
	boolean lastSetSolved = false;

	// Valido per ultima assegnazione di catena di trasformazioni.
	// true indica in attesa di dati esterni.
	boolean isWaitingForExternalData = false;
	
	// Numero catena di assegnazione 0-based nel programma corrente e in quello origine 
	int numChainSet = 0;
	
	// Info supplementari sull'assegnazione, valide in caso di ultima assegnazione
	int lvlPgmSet = 0;                                                  // Livello programma di assegnazione valore (0=programma origine)
    																	// Viaggiano in coppia

	// Valori estratti da tabella interna Cobol se LAST_SET_BY_COBOL_TBITEM.
	// Valore da assegnazione o value Space, literal, High-Value ...
	ArrayList<String> al_Value = null;                                 // 
	
	// Receiver principale.
	DataItemCobolIdentifier mainReceiver = null;
	
	// Receiver Move.
	DataItemCobolIdentifier receiver = null;
	
	// Sender Move.
	DataItemCobolIdentifier sender = null;
	
    // Receiver impliciti.
	// Vengono valorizzati a fronte di istruzione di assegnazione Move.
	// Includono tutti i campi che ridefiniscono o rinominano il campo sender.
	// Includono tutti i campi di gruppo sotto i quali il campo sender è definito.
	// Includono tutti i campi elementari definiti sotto il campo sender, se di gruppo.
	// Viene calcolata la posizione nel sender, che diventerà poi il nuovo receiver.
	// Viene calcolata la posizione/lunghezza nel settocampo origine.
	// Le 5 ArrayList viaggiano insieme.
	ArrayList<DataItemCobolIdentifier> al_nextReceiverImplicit = null;
	ArrayList<Integer> al_nextReceiverImplicitPos = null;
	ArrayList<Integer> al_nextReceiverImplicitLng = null;
	ArrayList<Integer> al_nextSubFieldImplicitPos = null;
	ArrayList<Integer> al_nextSubFieldImplicitLng = null;
    
	// Per gestione valorizzazioni di porzioni del sottocampo.
	int posInSubField = 1;													// Default inizio sottocampo
	int lngInSubField = 0;													// Default size in bytes sottocampo
	
    
	/*
	 * Costruttore
	 */
	public LogicDynamicFieldSubSetting(String system, String subSystem) {
		this.entityDynamicFieldSetting = new EntityDynamicFieldSubSetting();
		this.entityDynamicFieldSetting.setSystem(system);
		this.entityDynamicFieldSetting.setSubSystem(subSystem);
		this.al_Value = new ArrayList<String> ();
		this.al_nextReceiverImplicit = new ArrayList<DataItemCobolIdentifier> ();
		this.al_nextReceiverImplicitPos = new ArrayList<Integer> ();
		this.al_nextReceiverImplicitLng = new ArrayList<Integer> ();
	}

	/*
	 * Costruttore
	 */
	public LogicDynamicFieldSubSetting() {
		this.entityDynamicFieldSetting = new EntityDynamicFieldSubSetting();
		this.al_Value = new ArrayList<String> ();
		this.al_nextReceiverImplicit = new ArrayList<DataItemCobolIdentifier> ();
		this.al_nextReceiverImplicitPos = new ArrayList<Integer> ();
		this.al_nextReceiverImplicitLng = new ArrayList<Integer> ();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected LogicDynamicFieldSubSetting clone()  {
		LogicDynamicFieldSubSetting dynamicSubFieldSettingCloned = null;
		try {
			dynamicSubFieldSettingCloned = (LogicDynamicFieldSubSetting) super.clone();
		} catch (CloneNotSupportedException e) {
			dynamicSubFieldSettingCloned = null;
		}
		return dynamicSubFieldSettingCloned;
	}
/*
	@Override
	public String toString() {
		return " ProgramSet:"+entityDynamicFieldSetting.getIdPgmSet()
		   +   " Mode:"+entityDynamicFieldSetting.getSetMode().toString()
		   +   " Instr:"+entityDynamicFieldSetting.getNumInstr()
		   +   " FieldSpreaded:"+entityDynamicFieldSetting.getFieldReceiverId()
		   +   " FieldOrigin:"+entityDynamicFieldSetting.getIdField()
		   +   " PosInSubFld:"+entityDynamicFieldSetting.getPosInSubField()
		   +   " LngInSubFld:"+entityDynamicFieldSetting.getLngInSubField();
	}
	
	*/
}