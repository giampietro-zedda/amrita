package analyzer;
import java.io.Serializable;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;
import dao.DAOImplRelation;
import dao.IDAORelation;
import entities.EntityMetricValue;
import enums.EnumMetricsScope;
import enums.EnumMetricsSqualeRating;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumRelation;
import exception.ExceptionAmrita;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * Metrics 
 * </h1>
 *  <p>
 * Questa classe  modella le informazioni relative alle metriche di un oggetto software<br>
 * <p>
 * Gli oggetti interessati sono gli oggetti programma, tipicamente cobol, i copy e i jcl.<br>
 * Le metriche rappresentano un insieme di misurazioni di parametri software, ottenuti al <br>
 * momento dell'analisi dei sorgenti e da elaborazioni successive, combinate insieme per <br>
 * fornire un indicatore della qualità, complessità e manutenibilità del software.<br>
 * <p>
 * Le misurazioni necessarie al calcolo delle metriche vengono effettuate a livello di programma
 * o di jcl. Nel caso di programma cobol possono essere a livello si singola section, in base alle<br>
 * direttive di analisi e di esecuzione del processo di calcolo delle metriche.<br>
 * <p>
 * Il processo di calcolo delle metriche creerà delle aggregazioni a livello di:<br>
 * <ul>
 *    <li><tt>Programma</tt></li><br>
 *    <li><tt>Jcl</tt></li><br>
 *    <li><tt>Sottosistema</tt></li><br>
 *    <li><tt>Sistema</tt></li><br>
 *    <li><tt>Ambiente complessivo</tt></li><br>
 * </ul>
 * <br>
 * Le informazioni aggregate, descritte successivamente, vengono memorizzate in modo persistente nella tabella
 * <tt>METR</tt>, la cui persistenza è gestita dalla classe {@link EntityMetric}.<br>
 * <br>
 * Vengono prese in considerazione le seguenti misurazioni:<br>
 * <p>
 *     <h2>Misure di conteggio sorgenti</h2><br>
 *        <ul>
 *          <li><b>#SPRGA</b></li><br>
 *             Numero programmi analizzati<br>
 *             La misura è solo a livello di sottosistema,  sistema.<br>
 *          <li><b>#SPRGE</b></li><br>
 *             Numero programmi eseguiti da Exec batch o da Cics Link/Xctl<br>
 *             La misura è solo a livello di sottosistema,  sistema.<br>
 *          <li><b>#SSUBA</b></li><br>
 *             Numero sottoprogrammi analizzati.<br>
 *             La misura è solo a livello di sottosistema,  sistema.<br>
 *          <li><b>#SSUBE</b></li><br>
 *             Numero sottoprogrammi richiamati con Call.<br>
 *             La misura è solo a livello di sottosistema,  sistema.<br>
 *             La misura è solo a livello di sottosistema,  sistema.<br>
 *          <li><b>#SCOPY</b></li><br>
 *             Numero copy definiti.<br>
 *             La misura è solo a livello di sottosistema,  sistema.<br>
 *          <li><b>#SJCLJOB</b></li><br>
 *             Numero sources contenenti jcl job.<br>
 *             La misura è solo a livello di sottosistema,  sistema.<br>
 *          <li><b>#SJCLINC</b></li><br>
 *             Numero sources contenenti jcl include.<br>
 *             La misura è solo a livello di sottosistema,  sistema.<br>
 *          <li><b>#SJCLPROC</b></li><br>
 *             Numero sources contenenti jcl proc.<br>
 *             La misura è solo a livello di sottosistema,  sistema.<br>
 *        </ul>
 *        
 *     <h2>Misure dimensionali sorgenti</h2><br>
 *       <ul>
 *          <li><b>#LLOC</b></li><br>
 *             Numero linee di codice logiche, con istruzioni, senza commenti e righe a blank.<br>
 *             In pratica è il numero di linee del sorgente contenenti istruzioni o parte di istruzioni.<br>
 *          <li><b>#SLOC</b></li><br>
 *             Numero linee di codice fisiche, con istruzioni, commenti e righe a blank.<br>
 *             In pratica è il numero di linee del sorgente.<br>
 *          <li><b>#BLOC</b></li><br>
 *             Numero linee a blank.<br>
 *          <li><b>#CLOC</b></li><br>
 *             Numero linee di commento.<br>
 *          <li><b>#CLOCP</b></li><br>
 *             Numero linee di commento in procedure division.<br>
 *          <li><b>#CLOCD</b></li><br>
 *             Numero linee di commento in data division.<br>
 *          <li><b>#INST</b></li><br>
 *             Numero istruzioni in procedure division.<br>
 *        </ul>
 *        
 *     <h2>Misure stimate</h2><br>
 *       <ul>
 *          <li><b>#BFFP</b></li><br>
 *             Backfired Function Point<br>
 *             Si moliplica #LLOC per un fattore di conversione dipendente dal linguaggio.<br>
 *          <li><b>#TMDV</b></li><br>
 *             Tempo di sviluppo stimato in giorni<br>
 *             Si moliplica #LLOC per un fattore di produttività dipendente dal linguaggio.<br>
 *        </ul>
 *        
 *     <h2>Misure definizione dati</h2><br>
 *        <ul>
 *          <li><b>#DEFF</b></li><br>
 *             Numero campi definiti.<br>
 *          <li><b>#DEFFC</b></li><br>
 *             Numero campi definiti dentro moduli copy.<br>
 *          <li><b>#DEFL</b></li><br>
 *             Numero literal definite.<br>
 *        </ul>
 *     
 *     <h2>Misure di codice morto</h2><br>
 *        <ul>
 *          <li><b>#DEADF</b></li><br>
 *             Numero campi definiti non utilizzati.<br>
 *          <li><b>#DEADFC</b></li><br>
 *             Numero campi definiti dentro moduli copy non utilizzati.<br>
 *         <li><b>#DEADP</b></li><br>
 *             Numero sottografi sconnessi non richiamati, in cobol si tratta di section
 *             non referenziate.<br> 
 *          <li><b>#DEADL</b></li><br>
 *             Numero label definite e non referenziate.
 *          <li><b>#DEADCPD</b></li><br>
 *             Numero copy di data division definiti e non utilizzati.
 *          <li><b>#DEADCPP</b></li><br>
 *             Numero copy di proc division definiti e non utilizzati.
 *        </ul>
 *     <h2>Misure di jcl</h2><br>
 *        <ul>
 *          <li><b>#JCLDD</b></li><br>
 *              Numero di DD definite nel jcl.<br>
 *          <li><b>#JCLSTEP</b></li><br>
 *              Numero di step definiti nel jcl.<br>
 *              Vengono conteggiati anche quelli all'interno di proc richiamate<br>
 *              o di sources jcl inseriti con <tt>Include Member=sourceName</tt>
 *          <li><b>J#CLSTEPU</b></li><br>
 *              Numero step in aggiornamento.<br>
 *              Se lo step fa riferimento alla exec di un programma censito e analizzato,<br>
 *              lo step viene marcato in aggiornamento se il programma aggiorna almeno un <br>
 *              dsname dichiarato in una DD dello step.<br>
 *          <li><b>#JCLDSN</b></li><br>
 *              Numero dsname dichiarati.
 *          <li><b>#JCLDSNR</b></li><br>
 *              Numero dsname dichiarati e referenziati dai programmi.
 *          <li><b>#JCLDSNU</b></li><br>
 *              Numero dsname dichiarati e NON referenziati dai programmi.
 *          <li><b>#JCLINCR</b></li><br>
 *              Numero include richiamate.
 *          <li><b>#JCLPROCR</b></li><br>
 *              Numero proc richiamate.
 *        </ul>
 *     
 *     
 *     <h2>Misure di complessità strutturale</h2><br>
 *        Sono misure che indicano il grado di accoppiamento fra moduli e di complessità strutturale degli stessi.<br>
 *        I moduli con grande fan-in di solito sono piccoli e appartengono ai<br>
 *        livelli inferiori di un’architettura sw a livelli. I moduli grossi e <br>
 *        complessi hanno di solito fan-in piccolo. I moduli con grande fan-out <br>
 *        dipendono da parecchi moduli e alzano il livello di accoppiamento.<br>
 *        <tt>fan-in</tt> piccolo e <tt>fan-out</tt> grande indica cattiva qualità di progettazione.
 *        <ul>
 *          <li><b>#FANIN</b></li><br>
 *             Numero programmi chiamanti con <tt>Call, Cics Link, Cics Xctl</tt> etc.<br>
 *          <li><b>#FANOUT</b></li><br>
 *             Numero programmi chiamati con <tt>Call, Cics Link, Cics Xctl</tt> etc.<br>
 *          <li><b>#CSECT</b></li><br>
 *             Numero section (richiamate con perform) presenti nel programma cobol.<br>
 *          <li><b>#CPAR</b></li><br>
 *             Numero paragrafi (richiamati con perform) presenti nel programma cobol.<br>
 *        </ul>
 *        
 *     <h2>Misure di complessità funzionale generiche</h2><br>
 *        Sono misure generali attraverso le quali viene valutata la complessità funzionale a livello<br>
 *        di singolo oggetto, sottosistema e sistema.<br>
 *        Per interno si intende un oggetto definito dentro un sistema/sottosistema,<br>
 *        per esterno un oggetto definito in un diverso sistema/sottosistema.<br>
 *        <p>
 *        <ul>
 *          <li><b>#FPOBJ</b></li><br>
 *              Numero oggetti.
 *          <li><b>#FPRELA</b></li><br>
 *              Numero relazioni fra oggetti.
 *          <li><b>#FPTRANI</b></li><br>
 *              Numero transazioni interne richiamate con Exec Cics Start o Exec Cics Return Transid.
 *          <li><b>#FPTRANE</b></li><br>
 *              Numero transazioni esterne richiamate con Exec Cics Start o Exec Cics Return Transid.
 *          <li><b>#FPMAP</b></li><br>
 *              Numero mappe video utilizzate.
 *          <li><b>#FPCALLI</b></li><br>
 *              Numero call a moduli interni.
 *          <li><b>#FPCALLE</b></li><br>
 *              Numero call a moduli esterni.
 *          <li><b>#FPENTITYI</b></li><br>
 *              Numero accessi a entity (tabelle db) interni.
 *          <li><b>#FPENTITYE</b></li><br>
 *              Numero accessi a entity (tabelle db) esterni.
 *          <li><b>#FPDATAI</b></li><br>
 *              Numero accessi a files sequenziali/Vsam/code ts/.. interni.
 *          <li><b>#FDATAE</b></li><br>
 *              Numero accessi a files sequenziali/Vsam/code ts/.. esterni.
 *        </ul>
 *  
 *     <h2>Misure di complessità funzionale per function point </h2><br>
 *        Sono misure specifiche attraverso le quali vengono calcolati i function point (FP)<br>
 *        a livello di singolo oggetto, sottosistema e sistema.<br>
 *        Per interno si intende un oggetto definito dentro un sistema/sottosistema,<br>
 *        per esterno un oggetto definito in un diverso sistema/sottosistema.<br>
 *        Le metriche di function point (FP) prevedono 5 misurazioni:
 *      <p>
 *        <ul>
 *          <li><b>#EO</b></li><br>
 *              <tt>External Output</tt><br> 
 *              Si tratta di funzionalità utente (transazione o job) con output generati 
 *              leggendo da un ILF o EIF.<br>
 *          <li><b>#EI</b></li><br>
 *              <tt>External Input</tt><br>
 *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
 *          <li><b>#EQ</b></li><br>
 *              <tt>External Inquiry</tt><br>
 *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
 *          <li><b>#ILF</b></li><br>
 *              <tt>Internal Logical Files</tt><br>
 *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
 *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
 *              in insert, update e delete.<br>
 *          <li><b>#EIF</b></li><br>
 *              <tt>External Interface Files</tt><br>
 *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
 *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
 *              ma accedute in read, insert, update e delete.<br>
 *        </ul>
 *  
 *     <h2>Misure di complessità funzionale per rehosting </h2><br>
 *        Sono misure specifiche attraverso le quali si vuole valutare lo sforzo<br>
 *        e la complessita di rehosting di una applicazione, a livello di sottosistema e sistema.<br>
 *        Per perimetro si intende il sistema/sottosistema in considerazione.<br>
 *        Per oggetto perimetrato si intende un oggetto definito dentro il sistema/sottosistema <br>
 *        in considerazione.<br>
 *      <p>
 *        <ul>
 *          <li><b>#RHRATE</b></li><br>
 *              Rapporto tra il numero di oggetti e numero di relazioni
 *          <li><b>#RHORL</b></li><br>
 *              Numero di oggetti interni al perimetro
 *          <li><b>#RHEXT</b></li><br>
 *              Numero di oggetti esterni al perimetro
 *          <li><b>#RHUNP</b></li><br>
 *              Numero di oggetti non portabili ((Assembler, PL/I, Load Module, ecc...)
 *          <li><b>#RHBYN</b></li><br>
 *              Numero di files/tabelle contenenti campi binari
 *        </ul>
 *     
 *     <h2>Misure di complessità ciclomatica</h2><br>
 *        Misurano il numero di cammini esecutivi indipendenti (o il numero di condizioni binarie)<br>
 *        per il calcolo dell'indice di complessità ciclomatica di McCabe.<br>
 *        Si utilizza la versione estesa che tiene conto della complessità delle condizioni.<br>
 *        Quando un costrutto di controllo contiene un’espressione logica con OR e/o AND, <br>
 *        la misura di Complessità Ciclomatica Estesa aumenta di uno per ciascun operatore 
 *        logico impiegato nel costrutto <br>
 *        Queste misure stimano la complessità, la testabilità e la comprensibilità del codice<br>
 *        e sono un indicatore dello sforzo necessario per testare un programma.<br>
 *        Si tratta di misure che stimano la fiducia che il codice sia privo di errori e sono<br>
 *        indipendenti dal tipo di linguaggio.<br>
 *        Si applicano solo a oggetti programma e necessitano che sia stato creato il grafo di programma.<br>
 *        <ul>
 *          <li><b>#MCBEE</b></li><br>
 *              Numero archi di programma.
 *          <li><b>#MCBEN</b></li><br>
 *              Numero nodi di programma.
 *          <li><b>#MCBEP</b></li><br>
 *              Numero di sottografi sconnessi, in cobol sono section non referenziate.
 *        </ul>
 *     
 *     
 *     <h2>Misure di complessità di Halstead (o Software Science)</h2><br>
 *        Secondo Halstead un programma si compone di un numero finito di token, <br>
 *        classificati in <tt>operatori</tt> e <tt>operandi</tt> necessari al calcolo <br>
 *        dell'indice di complessità di Halstead.<br>
 *        La complessità di un programma, ed il suo tempo di sviluppo e manutenzione,<br>
 *        è proporzionale al tipo e numero di operatori e operandi che contiene<br>
 *        Halstead conteggia operatori e operandi che vanno intesi in una accezione specifica.<br>
 *        Operatore è un simbolo o parola chiave che specifica un'azione, come una istruzione.<br>
 *        Operando è un simbolo che rappresenta dati.<br>
 *        Questa metrica è controversa per la difficoltà di distinguere fra operatori e operandi<br>
 *        ma molto usata.<br>
 *        Altra obbiezione è che le metriche di Halstead misurano la complessità lessicale<br>
 *        e testuale più che la complessità strutturale o logica.<br>
 *        <ul>
 *          <li><b>#HLSTn1</b></li><br>
 *              Numero operatori distinti in un programma.<br>
 *          <li><b>#HLSTn2</b></li><br>
 *              Numero operandi distinti in un programma
 *          <li><b>#HLSTN1</b></li><br>
 *              Numero occorrenze di operatori.<br>
 *          <li><b>#HLSTN2</b></li><br>
 *               Numero occorrenze di operandi.<br>
 *          <li><b>#HLSTL</b></li><br>
 *              <b>Lunghezza</b> programma che si calcola formalmente come:<br>
 *              <h4>L = N1 + N2<br></h4>
 *              <p>
 *              Nel caso concreto:<br>
 *              <p>
 *              <h4>#HLSTL = #HLSTN1 + #HLSTN2<br><br></h4>
 *          <li><b>HLSTn</b></li><br>
 *              <b>Vocabolario</b> programma che si calcola formalmente come:<br>
 *              <h4>n = n1 + n2<br></h4>
 *              <p>
 *              Nel caso concreto:<br>
 *              <p>
 *              <h4>#HLSTn = #HLSTn1 + HLSTn2<br><br></h4>
 *         <li><b>#HLSTV</b></li><br>
 *              <b>Volume</b> programma che si calcola formalmente come:<br>
 *              <h4>V = N * (log2 n)<br></h4>
 *              <p>
 *              Nel caso concreto:<br>
 *              <p>
 *              <h4>#HLSTV = #HLSTL * (log2 #HLSTn)<br></h4>
 *          <li><b>#HLSTD</b></li><br>
 *              <b>Difficoltà</b> programma che si calcola formalmente come:<br>
 *              <h4>D = (n1/2) * (N2/n2))<br></h4>
 *              <p>
 *              Nel caso concreto:<br>
 *              <p>
 *              <h4>#HLSTD = (#HLSTn1/2) * (#HLSTN2/#HLSTn2))<br></h4>
 *          <li><b>HLSTS</b></li><br>
 *              <b>Sforzo</b> programma che correla difficoltà e volume programma e che<br>
 *              si calcola formalmente come:<br>
 *              <h4>E = D * V<br></h4>
 *              <p>
 *              Nel caso concreto:<br>
 *              <p>
 *              <h4>#HLSTS = #HLSTD * #HLSTV<br></h4>
 *        </ul>
 *     
 *     <h2>Misure di complessità dinamica</h2><br>
 *        Si tratta di misure che indicano quanto il programma sia dinamico <br>
 *        ovvero con chiamate a programmi o servizi espressi attraverso un campo <br>
 *        e non esplicitamente con una literal alfanumerica<br>
 *        Programmi con istruzioni dinamiche sno più difficili da testare e classificare<br>
 *        in quanto gli oggetti interessati sono disponibili solo run time.<br>
 *        Inoltre programmi fortemente dinamici sono più difficili da convertire, trasportare e
 *        non è possibile effettuare a priori una analisi degli impatti.<br>
 *        <tt>Amrita</tt> risolve il problema traducendo, attraverso la sola analisi statica del
 *        codice, le chiamate dinamiche con gli effettivi valori dinamici, senza interventi
 *        run time.<br>
 *        <p>
 *        Sono disponibili le seguenti misure:
 *        <p>
 *        <ul>
 *          <li><b>#DYN01</b></li><br>
 *              Numero programmi con codice dinamico.<br>
 *          <li><b>#DYN02</b></li><br>
 *              Numero istruzioni dinamiche
 *          <li><b>#DYN03</b></li><br>
 *              Numero istruzioni dinamiche light<br>
 *              Si intendono light le istruzioni dinamiche i cui campi non sono movimentati<br>
 *              e hanno un valore iniziale. In queste circostanze viene applicata una logica di<br>
 *              soluzione delle istruzioni dinamiche semplificata.<br> 
  *        </ul>
 *     
 *    <h2>Indici di documentazione</h2><br>
 *      <ul>
 *         <li><b>RCOML</b></li><br>
 *             Rate commenti rispetto alle righe sorgente logiche, con istruzioni.<br>
 *             E' il rapporto fra le righe di commento, incluse le righe a blank, <br>
 *             e quelle logiche sorgente, contenenti istruzioni.<br>
 *             Fornisce un'idea immediata della documentazione media di ogni istruzione<br>
 *             di programma ma non tiene conto di un uso eccessivo di righe a blank.<br>
 *             <p>
 *             Si calcola con la formula:<br>
 *             <p>
 *             <h4>RCOM = (#CLOC + #BLOC) / #LLOC</h4>
 *             
 *         <li><b>RCOMS</b></li><br>
 *             Rate commenti rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank.<br>
 *             E' il rapporto fra le righe di commento, incluse le righe a blank e quelle fisiche sorgente.<br>
 *             Fornisce una indicazione di quanto sia commentato un sorgente in generale ma non tiene<br>
 *             conto di un uso eccessivo di righe a blank.<br>
 *             <p>
 *             Si calcola con la formula:<br>
 *             <p>
 *             <h4>RCOMS = (#CLOC + #BLOC) / #SLOC</h4>
 *             
 *         <li><b>RCOMI</b></li><br>
 *             Rate commenti per istruzione.<br>
 *             E' il rapporto fra le righe di commento, incluse le righe a blank e il numero di istruzioni.<br>
 *             Fornisce una indicazione delle righe di commento medie per istruzione<br>
 *             <p>
 *             Si calcola con la formula:<br>
 *             <p>
 *             <h4>RCOMS = (#CLOC + #BLOC) / #INST</h4>
 *             
 *         <li><b>RBLKS</b></li><br>
 *             Rate righe a blank rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank.<br>
 *             E' il rapporto fra le righe a blank e quelle fisiche sorgente.<br>
 *             Fornisce un'idea immediata dell' uso eccessivo di righe a blank nel programma<br>
 *             senza contributo documentativo.<br>
 *             <p>
 *             Si calcola con la formula:<br>
 *             <p>
 *             <h4>RCOM = #BLOC / #SLOC</h4>
 *             
 *         <li><b>RBLKI</b></li><br>
 *             Rate righe a blank per istruzione.<br>
 *             E' il rapporto fra le righe a blank e il numero di istruzioni.<br>
 *             Fornisce un'idea immediata dell' uso eccessivo di righe a blank prima di ogni istruzione<br>
 *             <p>
 *             Si calcola con la formula:<br>
 *             <p>
 *             <h4>RBLKI = #BLOC / #INST</h4>
 *             
 *        </ul>
 * 
 *    <h2>Indici di complessita/manutenibilità</h2><br>
 *        <ul>
 *          <li><b>#MCBEI</b></li><br>
 *             E' l'indice di complessità ciclomatica di McCabe con il seguente significato.<br>
 *             1-10 modulo semplice<br>
 *             11-20 modulo a rischio moderato<br>
 *             21-50 modulo complesso<br>
 *             >50 modulo a rischio altissimo, non testabile<br>
 *             <p>
 *             Si calcola con la formula:<br>
 *             <h4>M = e - n + 2p<br></h4>
 *             <p>
 *             Dove, in termini formali:<br>
 *             <p>
 *             • e = Numero di archi<br>
 *             • n = Numero di nodi<br>
 *             • p = Numero di sottografi sconnessi<br>
 *             <p>
 *             Nel caso concreto:<br>
 *             <p>
 *             <h4>#MCBEI = #MCBEE - #MCBEN + 2 * #MCBEP<br><br></h4>
 *             
 *          <li><b>MI</b></li><br>
 *             Si tratta dell'indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister.<br>
 *             <tt>MI</tt> incorpora le metriche di Halstead (sforzo o volume), la complessità ciclomatica<br> 
 *             estesa di McCabe,la dimensione in LOC, ed il conteggio dei commenti.<br>
 *             La complessità ciclomatica estesa tiene conto della complessità aggiuntiva <br>
 *             derivante dalle espressioni condizionali complesse.<br>
 *             Un’espressione condizionale complessa contiene condizioni multiple separate da OR o AND.
 *             Quando un costrutto di controllo contiene un’espressione logica con OR e/o AND, <br>
 *             la misura di Complessità Ciclomatica Estesa aumenta di uno per ciascun operatore 
 *             logico impiegato nel costrutto <br>
 *             <p>
 *             Si calcola con la formula:<br>
 *             <h4>MI = 171 - 5.2 * ln(aveV) - 0.23 * aveV(g') - 16.2 * ln (aveLOC) + 50 * sin (sqrt(2.4 * perCM))<br><br></h4>
 *             <p>
 *             • I coefficienti ponderali sono stati derivati empiricamente<br>
 *             • I termini dell’equazione sono:<br>
 *               – aveV = Volume di Halstead medio V per modulo<br>
 *               – aveV(g') = complessità ciclomatica estesa media per modulo<br>
 *               – aveLOC = media LOC per modulo; e, opzionalmente <br>
 *               – perCM = percentuale media di righe commento per modulo <br>
 *               
 *         <li><b>FPI</b></li><br>
 *         <li><b>RHI</b></li><br>
 *             Si tratta di un indice della complessità e sforzo di rehosting di una applicazione.
 *         
 *   </ul>
 *
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 25/02/2011
 * @see EntityMetric
 *   
 */
public class Metrics  extends ExecutionShared implements Serializable  {

	private static final long serialVersionUID = 1L;

	
  	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza specifiche                                                                    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// Campo di validità metricche  
	private EnumMetricsScope scope = null;                 // Livello di aggregazione codificato
	
	// Dati di identificazione livello di aggregazione
	private String system = "";                        	   // Sistema di aggregazione
	private String subSystem = "";                     	   // Sotto Sistema di aggregazione
	private String idObject = "";          			       // Nome oggetto (es. programma, copy, jcl, include, ..)
	private EnumObject typeObject = null;       	       // Tipologia oggetto di aggregazione(T0001) 
	private String section = "";                           // Object section di aggregazione se oggetto programma
	
	// Misure di conteggio sorgenti
	private long  cntPgmAnalyzed = 0;                      // CPGA SPRGA     Numero programmi analizzati
	private long  cntPgmExecuted = 0;                      // CPGE SPRGE     Numero programmi eseguiti da Exec batch o da Cics Link/Xctl
	private long  cntSubPgmAnalyzed = 0;                   // CSPA SSUBA     Numero sottoprogrammi analizzati
	private long  cntSubPgmExecuted = 0;                   // CSPE SSUBE     Numero sottoprogrammi richiamati con Call
	private long  cntCopyDefined = 0;                      // CCPD SCOPY     Numero copy definiti
	private long  cntJclJob = 0;                           // CJCJ SJCLJOB   Numero sources contenenti jcl job
	private long  cntJclInclude = 0;                       // CJCI SJCLINC   Numero sources contenenti jcl include
	private long  cntJclProc = 0;                          // CJCP SJCLPROC  Numero sources contenenti jcl include
	
	// Misure dimensionali sorgenti
	private long  sizeLinesCodeLogical = 0;                // SLCL LLOC      Numero linee di codice logiche, includenti istruzioni, senza commenti e righe a blank
	private long  sizeLinesCodePhisical = 0;               // SLCP SLOC      Numero linee di codice fisiche, includenti istruzioni, commenti e righe a blank
	private long  sizeLinesBlank = 0;                      // SLCB BLOC      Numero linee a blank
	private long  sizeLinesBlankProc = 0;                  // SLPB BLOCP     Numero linee a blank in procedure division
	private long  sizeLinesBlankData = 0;                  // SLDB BLOCD     Numero linee a blank in data division
	private long  sizeLinesComment = 0;                    // SLCC CLOC      Numero linee di commento
	private long  sizeLinesCommentProc = 0;                // SLCI CLOCP     Numero linee di commento in procedure division
	private long  sizeLinesCommentData = 0;                // SLCD CLOCD     Numero linee di commento in data division
	private long  sizeInstr = 0;                           // SINS INST      Numero istruzioni in procedure division

	// Misure stimate con sizeLinesCodeLogical
	private double  backFiredFunctionPoint = 0;   		   // BFFP BFFP      Function point stimati in base al numero logico di loc (sizeLinesCodeLogical)
	private double timeDevelopment = 0;     			   // TMDV TMDV      Tempo di sviluppo in giorni stimato in base alla produttività media giornaliera

	// Misure definizione dati
	private long  defFields = 0;                           // DFLD DEFF      Numero campi definiti
	private long  defFieldsInCopy = 0;                     // DFCP DEFFC     Numero campi definiti dentro moduli copy
	private long  defLiterals = 0;                         // DLIT DEFL      Numero literal definite
	
	// Indici di documentazione
	private double percComByLogical = 0;                   // RCML RCOML    % commenti rispetto alle righe sorgente logiche, con istruzioni
	private double percComByPhisical = 0;                  // RCMP RCOMS    % commenti rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
	private double percComByInstruction = 0;               // RCMI RCOMI    % commenti per istruzione
	private double percBlankByPhisical = 0;                // RBKP RBLKS    % righe a blank rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
	private double percBlankByInstruction = 0;             // RBKI RBLKI    % righe a blank per istruzione
	
	// Misure di codice dinamico
	private long dynamicPgm = 0;                     	   // DYN01 DPGM    Numero di programmi con codice dinamico
	private long dynamicInstr = 0;              	       // DYN02 DINT    Numero istruzioni dinamiche totali
	private long dynamicInstrLight = 0;              	   // DYN03 DINL    Numero istruzioni dinamiche light, con soli campi con value
	private double percDynamicInstr = 0;         		   // DYN04 RDIT    % istruzioni dinamiche su istruzioni totali
	private double percDynamicInstrLight = 0;    		   // DYN05 RDLT    % istruzioni dinamiche light su istruzioni totali

	// Misure violazioni
	private long violations = 0;              	 		   // VCNT VRIL     Numero di violazioni rilevate
	private double percViolationsByLogical = 0;  		   // VPBL RPBL     % Violazioni rispetto alle righe sorgente logiche, con istruzioni
	private double percViolationsByPhisical = 0;  		   // VPBP RPBP     % Violazioni rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
	private double percViolationsByInstruction = 0;		   // VPBI RPBI     % Violazioni per istruzione

	// Misure di codice morto
	private long  deadFields = 0;                          // XFLD DEADF     Numero campi definiti non utilizzati
	private long  deadInstr = 0;                           // XINS DEADI     Numero istruzioni definite e non referenziate (includono eventuali label)
	private long  deadLabels = 0;                          // XLAB DEADL     Numero label definite e non referenziate
	private long  deadCopyData = 0;                        // XCPY DEADCPD   Numero copy di data division definiti e non utilizzati
	private long  deadCopyProc = 0;                        // XCPY DEADCPP   Numero copy di proc division definiti e non utilizzati
	private long  deadSubGraph = 0;                        // XSBG DEADP     Numero sottografi sconnessi non richiamati, in cobol si tratta di section non referenziate

	// Misure di jcl
	private long  jclDD = 0;                               // JDDN JCLDD     Numero di DD definite nel jcl
	private long  jclStepDefined = 0;                      // JSTD JCLSTEP   Numero di step definiti nel jcl
	private long  jclStepUpdate = 0;                       // JSTU CLSTEPU   Numero step in aggiornamento
	private long  jclDsname = 0;                           // JDSN JCLDSN    Numero dsname dichiarati
	private long  jclDsnameReferenced = 0;                 // JDSR JCLDSNR   Numero dsname dichiarati e referenziati dai programmi
	private long  jclDsnameUnReferenced = 0;               // JDSU JCLDSNU   Numero dsname dichiarati e NON referenziati dai programmi
	private long  jclIncludeCalled = 0;                    // JINC JCLINCR   Numero include richiamate
	private long  jclProcCalled = 0;                       // JPRC JCLPROCR  Numero proc richiamate
	

	// Misure di complessità strutturale
	private long  structFanIn = 0;                         // SFAI FANIN     Numero programmi chiamanti con <tt>Call, Cics Link, Cics Xctl
	private long  structFanOut = 0;                        // SFAO FANOUT    Numero programmi chiamati con <tt>Call, Cics Link, Cics Xctl
	private long  structSections = 0;           		   // SSEC CSECT     Numero section nel programma
	private long  structParagraphs = 0;         	       // SPAR CPAR      Numero paragrafi nel programma
	
	// Misure di complessità funzionale generiche
	private long  funcObjects = 0;                         // FOBJ FPRELA    Numero oggetti
	private long  funcRelations = 0;                       // FREL FPOBJ     Numero relazioni con altri oggetti
	private long  funcTranInternal = 0;                    // FTRI FPTRANI   Numero transazioni interne richiamate con Exec Cics Start o Exec Cics Return Transid
	private long  funcTranExternal = 0;                    // FTRE FPTRANE   Numero transazioni esterne richiamate con Exec Cics Start o Exec Cics Return Transid
	private long  funcMap = 0;                             // FMAP FPMAP     Numero mappe video utilizzate
	private long  funcCallInternal = 0;                    // FCAI FPCALLI   Numero call a moduli interni
	private long  funcCallExternal = 0;                    // FCAE FPCALLE   Numero call a moduli esterni
	private long  funcAccEntityInternal = 0;               // FAEI FPENTITYI Numero accessi a entity (tabelle db) interni
	private long  funcAccEntityExternal = 0;               // FAEE FPENTITYE Numero accessi a entity (tabelle db) esterni
	private long  funcAccMediaInternal = 0;                // FAMI FPDATAI   Numero accessi a files sequenziali/Vsam/code ts/.. interni
	private long  funcAccMediaExternal = 0;                // FAME FDATAE    Numero accessi a files sequenziali/Vsam/code ts/.. esterni

	// Misure di complessità funzionale Function Point
	private long  fpExternalOutputEO = 0;                  // PXEO  FPEO     Funzionalità utente (transazione o job) con output generati da un ILF o EIF
	private long  fpExternalInputEI = 0;                   // PXIO  FPEI     Funzionalità utente (transazione o job) con add, change,delete di un ILF
	private long  fpExternalInquiryEQ = 0;                 // PXEQ  FPEQ     Funzionalità utente (transazione o job) di sola read da ILF o EIF
	private long  fpInternalLogicalFileILF = 0;            // PILF  FPILF    Tabelle/files definite e gestite dentro il sistema/sottosistema
	private long  fpExternalInterfaceFileEIF = 0;          // PEIF  FPEIF    Tabelle/files definite fuori dal sistema/sottosistema acceduti in read/update

	// Misure di complessità funzionale/tecnica per rehosting 
	private long  rhObjectsInternal = 0;                   // HINT  RHINT    Numero di oggetti interni al perimetro 
	private long  rhObjectsExternal  = 0;                  // HEXT  RHEXT    Numero di oggetti esterni al perimetro 
	private long  rhObjectsUnportable = 0;                 // HUNP  RHUNP    Numero di oggetti non portabili ((Assembler, PL/I, Load Module, ecc...) 
	private double rhRateObjectRelation = 0;               // HROR  RHORL    Rapporto tra il numero di oggetti e numero di relazioni 
	private long  rhFilesBynary = 0;                       // HBYN  RHBYN    Numero di files/tabelle contenenti campi binari 
	
	// Misure di complessità ciclomatica
	private long  mcCabeArcs = 0;                          // MARC MCBEE    Numero archi di programma
	private long  mcCabeNodes = 0;                         // MNOD MCBEN    Numero nodi di programma
	private long  mcCabeOperatorsOrAnd = 0;                // MOPC MCBEX    Numero operatori condizionali OR AND
	private long  mcCabeGraphConn = 0;                     // MGNC MCBEP    Numero di sottografi connessi, in cobol sono section non referenziate

	// Misure di complessità di Halstead (o Software Science)
	private long  halsteadOperators = 0;                   // HOPT HLSTn1   Numero operatori distinti in un programma (es. istruzione IF)
	private long  halsteadOperands = 0;                    // HOPN HLSTn1   Numero operandi distinti in un programma (es. un campo definito)
	private long  halsteadOperatorsOcc = 0;                // HPTO HLSTN1   Numero occorrenze di operatori (#IF)
	private long  halsteadOperandsOcc = 0;                 // HPNO HLSTN2   Numero occorrenze di operandi (#utilizzi campo)
	private long  halsteadLengthPgm = 0;                   // HLNP HLSTL    Lunghezza programma
	private long  halsteadVocabularyPgm = 0;               // HVBP HLSTn    Vocabolario programma
	private double halsteadVolumePgm = 0;                  // HVLP HLSTV    Volume programma
	private double halsteadDifficultPgm = 0;               // HDFP HLSTD    Difficoltà programma
	private double halsteadEffortPgm = 0;                  // HEFP HLSTS    Sforzo programma
	private long halsteadTimeWriting = 0;                  // HTMW HLSTT    Tempo stimato di scrittura programma in secondi
	
	// Indici di complessita/manutenibilità totali somma delle procedure interne del programma
	private double idxMITot = 0;                            // IMII MI       Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
	private double idxFPTot = 0;                            // IFPP FP       IndiceNumero punti funzione
	private double idxMcCabeTot = 0;                        // IMCB MCBEI    Indice di complessità ciclomatica di McCabe con il seguente significato
	private double idxReHostingTot = 0;                     // IREH RHI      Indice della complessità e sforzo di rehosting di una applicazione.
	
	// Indici di complessita/manutenibilità massimo
	private double idxMIHigh = 0;                           // IMII MI       Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
	private double idxFPHigh = 0;                           // IFPP FP       IndiceNumero punti funzione
	private double idxMcCabeHigh = 0;                       // IMCB MCBEI    Indice di complessità ciclomatica di McCabe con il seguente significato
	private double idxReHostingHigh = 0;                    // IREH RHI      Indice della complessità e sforzo di rehosting di una applicazione.
	
	// Indici di complessita/manutenibilità minimi
	private double idxMILow = 0;                            // IMII MI       Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
	private double idxFPLow = 0;                            // IFPP FP       IndiceNumero punti funzione
	private double idxMcCabeLow = 0;                        // IMCB MCBEI    Indice di complessità ciclomatica di McCabe con il seguente significato
	private double idxReHostingLow = 0;                     // IREH RHI      Indice della complessità e sforzo di rehosting di una applicazione.
	
	// Indici di complessita/manutenibilità medi
	private double idxMIAvg = 0;                            // IMII MI       Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
	private double idxFPAvg = 0;                            // IFPP FP       IndiceNumero punti funzione
	private double idxMcCabeAvg = 0;                        // IMCB MCBEI    Indice di complessità ciclomatica di McCabe con il seguente significato
	private double idxReHostingAvg = 0;                     // IREH RHI      Indice della complessità e sforzo di rehosting di una applicazione.
	
	// Sistema di qualità SQUALE, numero violazioni per categoria gravità
	private long squaleViolationsBlocker = 0;     			// SVBK    Squale numero violazioni bloccanti
	private long squaleViolationsCritical = 0;    			// SVCR    Squale numero violazioni critiche
	private long squaleViolationsMajor = 0;    				// SVMJ    Squale numero violazioni maggiori
	private long squaleViolationsMinor = 0;    				// SVMN    Squale numero violazioni minori
	private long squaleViolationsInfo = 0;    				// SVIN    Squale numero violazioni informative

	// Sistema di qualità SQUALE, valori generali
	private EnumMetricsSqualeRating squaleSSRL = null;		// SSRL    Squale rating  Livello A-E valore di rating (T0041)
	private double squaleSSRI = 0;        	 	    		// SSRI    Squale rating  (SSQI / tempo stimato di sviluppo)
	private double squaleSSCI = 0;        					// SSCI    Squale rule compliance (100 indica perfetta aderenza)
	private long squaleSSQI = 0;					 		// SSQI    Squale absolute remediation cost. SQTI+SQRI+...+SQPI

	// Sistema di qualità SQUALE, valori di dettaglio indice qualità assoluto SSQI, SQxI
	private long squaleSQTI = 0;							// SQTI    Testability 		index (somma costi remediation per questa caratteristica)
	private long squaleSQRI = 0;							// SQRI    Reliability 		index (somma costi remediation per questa caratteristica)
	private long squaleSQCI = 0;							// SQCI    Changeability	index (somma costi remediation per questa caratteristica)
	private long squaleSQEI = 0;							// SQEI    Efficiency 		index (somma costi remediation per questa caratteristica)
	private long squaleSQSI = 0;							// SQSI    Security 		index (somma costi remediation per questa caratteristica)
	private long squaleSQMI = 0;							// SQNI    Maintenability 	index (somma costi remediation per questa caratteristica)
	private long squaleSQPI = 0;							// SQPI    Portability 		index (somma costi remediation per questa caratteristica)

	// Sistema di qualità SQUALE, valori di dettaglio indici consolidati SCTx
	private long squaleSCTI = 0;							// SCTI    Testability 		index consolidato (somma costi remediation caratteristiche precedenti)
	private long squaleSCRI = 0;							// SCRI    Reliability 		index consolidato (somma costi remediation caratteristiche precedenti)
	private long squaleSCCI = 0;							// SCCI    Changeability	index consolidato (somma costi remediation caratteristiche precedenti)
	private long squaleSCEI = 0;							// SCEI    Efficiency 		index consolidato (somma costi remediation caratteristiche precedenti)
	private long squaleSCSI = 0;							// SCSI    Security 		index consolidato (somma costi remediation caratteristiche precedenti)
	private long squaleSCMI = 0;							// SCNI    Maintenability 	index consolidato (somma costi remediation caratteristiche precedenti)
	private long squaleSCPI = 0;							// SCPI    Portability 		index consolidato (somma costi remediation caratteristiche precedenti)

	// Sistema di qualità SQUALE, valori di dettaglio indici di densita SDxI
	private double squaleSDTI = 0;							// SDTI    Testability 		index densita (SQTI / dimensioni)
	private double squaleSDRI = 0;							// SDRI    Reliability 		index densita (SQRI / dimensioni)
	private double squaleSDCI = 0;							// SDCI    Changeability	index densita (SQCI / dimensioni)
	private double squaleSDEI = 0;							// SDEI    Efficiency 		index densita (SQEI / dimensioni)
	private double squaleSDSI = 0;							// SDSI    Security 		index densita (SQSI / dimensioni)
	private double squaleSDMI = 0;							// SDNI    Maintenability 	index densita (SQMI / dimensioni)
	private double squaleSDPI = 0;							// SDPI    Portability 		index densita (SQPI / dimensioni)

	// Sistema di qualità SQUALE, valori di dettaglio indici squale rating SRxI 
	private double squaleSRTI = 0;							// SRTI    Testability 		Squale rating (SQTI / tempo stimato sviluppo)
	private double squaleSRRI = 0;							// SRRI    Reliability 		Squale rating (SQRI / tempo stimato sviluppo)
	private double squaleSRCI = 0;							// SRCI    Changeability	Squale rating (SQCI / tempo stimato sviluppo)
	private double squaleSREI = 0;							// SREI    Efficiency 		Squale rating (SQEI / tempo stimato sviluppo)
	private double squaleSRSI = 0;							// SRSI    Security 		Squale rating (SQSI / tempo stimato sviluppo)
	private double squaleSRMI = 0;							// SRNI    Maintenability 	Squale rating (SQMI / tempo stimato sviluppo)
	private double squaleSRPI = 0;							// SRPI    Portability 		Squale rating (SQPI / tempo stimato sviluppo)

	// Sistema di qualità SQUALE, valori di dettaglio livelli squale rating SRxL 
	private EnumMetricsSqualeRating squaleSRTL = null;		// SRTL    Testability 		Livello A-E valore di rating (T0041)
	private EnumMetricsSqualeRating squaleSRRL = null;		// SRRL    Reliability 		Livello A-E valore di rating (T0041)
	private EnumMetricsSqualeRating squaleSRCL = null;		// SRCL    Changeability	Livello A-E valore di rating (T0041)
	private EnumMetricsSqualeRating squaleSREL = null;		// SREL    Efficiency 		Livello A-E valore di rating (T0041)
	private EnumMetricsSqualeRating squaleSRSL = null;		// SRSL    Security 		Livello A-E valore di rating (T0041)
	private EnumMetricsSqualeRating squaleSRML = null;		// SRNL    Maintenability 	Livello A-E valore di rating (T0041)
	private EnumMetricsSqualeRating squaleSRPL = null;		// SRPL    Portability 		Livello A-E valore di rating (T0041)
	
		
	
	
	/**
	 *  Costruttore  senza parametri
	 */
	public Metrics() {
		scope = EnumMetricsScope.NOT_ASSIGNED;
		typeObject = EnumObject.NOT_ASSIGNED;
		section = "*";
		squaleSSRL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRTL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRRL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRCL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSREL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRSL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRML = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRPL = EnumMetricsSqualeRating.NOT_ASSIGNED;
	}

	/**
	 *  Costruttore  standard
	 */
	public Metrics(UserConfiguration sd, ExecutionDirectives di) {
		super(sd, di);
		scope = EnumMetricsScope.SCOPE_LEVEL_OBJECT;
		typeObject = EnumObject.NOT_ASSIGNED;
		section = "*";
		squaleSSRL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRTL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRRL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRCL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSREL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRSL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRML = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRPL = EnumMetricsSqualeRating.NOT_ASSIGNED;
	}


	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////                     Metodi pubblici                   /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////





	/**
	 * Restituisce il sistema di aggregazione metriche.<br>
	 * <p>
	 * Se l'aggregazione non è a livello sistema restituisce stringa vuota.<br>
	 * <p>
	 * 
	 * @return the aggrSystem
	 */
	public String getSystem() {
		return system;
	}

	/**
	 * Restituisce il livello di aggregazione metriche.<br>
	 * <p>
	 * Si tratta di section di programma, programma,
	 * sottosistema, sistema o nessuna aggregazione,
	 * come descritto da {@link EnumMetricsScope}.<br>
	 * 
	 * @return the scopeMetrics
	 */
	public EnumMetricsScope getScope() {
		return scope;
	}

	/**
	 * Imposta il livello di aggregazione metriche.<br>
	 * <p>
	 * Si tratta di section di programma, programma,
	 * sottosistema, sistema o nessuna aggregazione,
	 * come descritto da {@link EnumMetricsScope}.<br>
	 * 
	 * @param scopeMetrics the scopeMetrics to set
	 */
	public void setScope(EnumMetricsScope scope) {
		this.scope = scope;
		
		// Impostazion dati identificativi aggregazione
		switch (this.scope) {
			case SCOPE_LEVEL_OBJECT:
				this.section = "*";
				break;
			case SCOPE_LEVEL_SYSTEM:
				this.subSystem = "*";
				break;
			case SCOPE_LEVEL_GLOBAL:
				this.system = "*";
				this.subSystem = "*";
				this.idObject = "*";
				this.typeObject = EnumObject.NOT_ASSIGNED;
				this.section = "*";
				break;
			default:
				break;
			}
	}




	/**
	 * Imposta il sistema di aggregazione metriche.<br>
	 * <p>
	 * Se l'aggregazione non è a livello sistema restituisce stringa vuota.<br>
	 * <p>
	 * 
	 * @param aggrSystem the aggrSystem to set
	 */
	public void setSystem(String system) {
		this.system = system;
	}



	/**
	 * Restituisce il sotto sistema di aggregazione metriche.<br>
	 * <p>
	 * Se l'aggregazione non è a livello sotto sistema restituisce stringa vuota.<br>
	 * <p>
	 * 
	 * @return the aggrSubSystem
	 */
	public String getSubSystem() {
		return subSystem;
	}



	/**
	 * Imposta il sotto sistema di aggregazione metriche.<br>
	 * <p>
	 * Se l'aggregazione non è a livello sotto sistema restituisce stringa vuota.<br>
	 * <p>
	 * 
	 * @param aggrSubSystem the aggrSubSystem to set
	 */
	public void setSubSystem(String subSystem) {
		this.subSystem = subSystem;
	}



	/**
	 * Restituisce il nome dell'oggetto di aggregazione metriche.<br>
	 * <p>
	 * Se l'aggregazione non è a livello di programma restituisce stringa vuota.<br>
	 * <p>
	 * 
	 * @return the idObject
	 */ 
	public String getIdObject() {
		return idObject;
	}



	/**
	 * Imposta il nome dell'oggetto di aggregazione metriche.<br>
	 * <p>
	 * Se l'aggregazione non è a livello di programma restituisce stringa vuota.<br>
	 * <p>
	 * 
	 * @param idObject the idObject to set
	 */
	public void setIdObject(String idObject) {
		this.idObject = idObject;
	}



	/**
	 * Restituisce il tipo dell'oggetto di aggregazione metriche.<br>
	 * <p>
	 * Se l'aggregazione non è a livello di programma restituisce stringa vuota.<br>
	 * <p>
	 * 
	 * @return the typeObject
	 */
	public EnumObject getTypeObject() {
		return typeObject;
	}




	/**
	 * Imposta il tipo dell'oggetto di aggregazione metriche.<br>
	 * <p>
	 * Se l'aggregazione non è a livello di programma restituisce stringa vuota.<br>
	 * <p>
	 * 
	 * @param typeObject the typeObject to set
	 */
	public void setTypeObject(EnumObject typeObject) {
		this.typeObject = typeObject;
	}



	/**
	 * Restituisce la section di programma di aggregazione metriche.<br>
	 * <p>
	 * Deve essere specificato anche il programma di aggreagazione.<br>
	 * <p>
	 * 
	 * @return the section
	 */
	public String getSection() {
		return section;
	}



	/**
	 * Imposta la section di programma di aggregazione metriche.<br>
	 * <p>
	 * Deve essere specificato anche il programma di aggreagazione.<br>
	 * <p>
	 * 
	 * @param section the section to set
	 */
	public void setSection(String section) {
		this.section = section;
	}


	/**
	 * Restituisce il numero di programmi analizzati,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the cntPgmAnalyzed
	 */
	public long getCntPgmAnalyzed() {
		return cntPgmAnalyzed;
	}



	/**
	 * Imposta il numero di programmi analizzati,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param cntPgmAnalyzed the cntPgmAnalyzed to set
	 */
	public void setCntPgmAnalyzed(long cntPgmAnalyzed) {
		this.cntPgmAnalyzed = cntPgmAnalyzed;
	}



	/**
	 * Restituisce il numero di programmi eseguiti da Exec batch o da Cics Link/Xctl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the cntPgmExecuted
	 */
	public long getCntPgmExecuted() {
		return cntPgmExecuted;
	}



	/**
	 * Imposta il numero di programmi eseguiti da Exec batch o da Cics Link/Xctl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param cntPgmExecuted the cntPgmExecuted to set
	 */
	public void setCntPgmExecuted(long cntPgmExecuted) {
		this.cntPgmExecuted = cntPgmExecuted;
	}



	/**
	 * Restituisce il numero di sotto programmi analizzati,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the cntSubPgmAnalyzed
	 */
	public long getCntSubPgmAnalyzed() {
		return cntSubPgmAnalyzed;
	}



	/**
	 * Imposta il numero di sotto programmi analizzati,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param cntSubPgmAnalyzed the cntSubPgmAnalyzed to set
	 */
	public void setCntSubPgmAnalyzed(long cntSubPgmAnalyzed) {
		this.cntSubPgmAnalyzed = cntSubPgmAnalyzed;
	}



	/**
	 * Restituisce il numero di sotto programmi chiamati con Call,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @return the cntSubPgmExecuted
	 */
	public long getCntSubPgmExecuted() {
		return cntSubPgmExecuted;
	}



	/**
	 * Imposta il numero di sotto programmi chiamati con Call,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param cntSubPgmExecuted the cntSubPgmExecuted to set
	 */
	public void setCntSubPgmExecuted(long cntSubPgmExecuted) {
		this.cntSubPgmExecuted = cntSubPgmExecuted;
	}



	/**
	 * Restituisce il numero di copy definitil,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @return the cntCopyDefined
	 */
	public long getCntCopyDefined() {
		return cntCopyDefined;
	}



	/**
	 * Imposta il numero di copy definitil,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param cntCopyDefined the cntCopyDefined to set
	 */
	public void setCntCopyDefined(long cntCopyDefined) {
		this.cntCopyDefined = cntCopyDefined;
	}



	/**
	 * Restituisce il numero di sources contenenti jcl job,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @return the cntJclJob
	 */
	public long getCntJclJob() {
		return cntJclJob;
	}



	/**
	 * Imposta il numero di sources contenenti jcl job,
	 * presenti nel livello corrente di aggregazione.<br>
	 * @param cntJclJob the cntJclJob to set
	 */
	public void setCntJclJob(long cntJclJob) {
		this.cntJclJob = cntJclJob;
	}



	/**
	 * Restituisce il numero di sources contenenti jcl include,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @return the cntJclInclude
	 */
	public long getCntJclInclude() {
		return cntJclInclude;
	}



	/**
	 * Imposta il numero di sources contenenti jcl include,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param cntJclInclude the cntJclInclude to set
	 */
	public void setCntJclInclude(long cntJclInclude) {
		this.cntJclInclude = cntJclInclude;
	}



	/**
	 * Restituisce il numero di sources contenenti jcl proc,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @return the cntJclProc
	 */
	public long getCntJclProc() {
		return cntJclProc;
	}



	/**
	 * Imposta il numero di sources contenenti jcl proc,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param cntJclProc the cntJclProc to set
	 */
	public void setCntJclProc(long cntJclProc) {
		this.cntJclProc = cntJclProc;
	}



	/**
	 * Restituisce il numero linee di codice logiche, con istruzioni, 
	 * senza commenti e righe a blank, presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the sizeLinesCodeLogical
	 */
	public long getSizeLinesCodeLogical() {
		return sizeLinesCodeLogical;
	}


	/**
	 * Imposta il numero linee di codice logiche, con istruzioni, 
	 * senza commenti e righe a blank, presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param sizeLinesCodeLogical the sizeLinesCodeLogical to set
	 */
	public void setSizeLinesCodeLogical(long sizeLinesCodeLogical) {
		this.sizeLinesCodeLogical = sizeLinesCodeLogical;
	}



	/**
	 * Restituisce il numero linee di codice fisiche, con istruzioni, commenti e righe a blank,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the sizeLinesCodePhisical
	 */
	public long getSizeLinesCodePhisical() {
		return sizeLinesCodePhisical;
	}



	/**
	 * Imposta il numero linee di codice fisiche, con istruzioni, commenti e righe a blank,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param sizeLinesCodePhisical the sizeLinesCodePhisical to set
	 */
	public void setSizeLinesCodePhisical(long sizeLinesCodePhisical) {
		this.sizeLinesCodePhisical = sizeLinesCodePhisical;
	}



	/**
	 * Restituisce il numero linee di codice righe a blank,
	 * di procedure, presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the sizeLinesBlank
	 */
	public long getSizeLinesBlankProc() {
		return sizeLinesBlankProc;
	}

	/**
	 * Restituisce il numero linee di codice righe a blank,
	 * di data division, presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the sizeLinesBlank
	 */
	public long getSizeLinesBlankData() {
		return sizeLinesBlankData;
	}

	/**
	 * Restituisce il numero linee di codice righe a blank,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the sizeLinesBlank
	 */
	public long getSizeLinesBlank() {
		return sizeLinesBlank;
	}



	/**
	 * Imposta il numero linee di codice righe a blank,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param sizeLinesBlank the sizeLinesBlank to set
	 */
	public void setSizeLinesBlank(long sizeLinesBlank) {
		this.sizeLinesBlank = sizeLinesBlank;
	}
	/**
	 * Imposta il numero linee di codice righe a blank,
	 * di procedure presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param sizeLinesBlank the sizeLinesBlank to set
	 */
	public void setSizeLinesBlankProc(long sizeLinesBlank) {
		this.sizeLinesBlankProc = sizeLinesBlank;
	}

	/**
	 * Imposta il numero linee di codice righe a blank,
	 * di data division presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param sizeLinesBlank the sizeLinesBlank to set
	 */
	public void setSizeLinesBlankData(long sizeLinesBlank) {
		this.sizeLinesBlankData = sizeLinesBlank;
	}



	/**
	 * Restituisce il numero linee di commento,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the sizeLinesComment
	 */
	public long getSizeLinesComment() {
		return sizeLinesComment;
	}



	/**
	 * Imposta il numero linee di commento,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param sizeLinesComment the sizeLinesComment to set
	 */
	public void setSizeLinesComment(long sizeLinesComment) {
		this.sizeLinesComment = sizeLinesComment;
	}



	/**
	 * Restituisce il numero linee di commento in procedure,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the sizeLinesCommentProc
	 */
	public long getSizeLinesCommentProc() {
		return sizeLinesCommentProc;
	}



	/**
	 * Imposta il numero linee di commento in procedure,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param sizeLinesCommentProc the sizeLinesCommentProc to set
	 */
	public void setSizeLinesCommentProc(long sizeLinesCommentProc) {
		this.sizeLinesCommentProc = sizeLinesCommentProc;
	}



	/**
	 * Restituisce il numero linee di commento in data division,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the sizeLinesCommentData
	 */
	public long getSizeLinesCommentData() {
		return sizeLinesCommentData;
	}



	/**
	 * Imposta il numero linee di commento in data division,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param sizeLinesCommentData the sizeLinesCommentData to set
	 */
	public void setSizeLinesCommentData(long sizeLinesCommentData) {
		this.sizeLinesCommentData = sizeLinesCommentData;
	}



	/**
	 * Restituisce il numero di istruzioni nel programma, jcl o copy,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the sizeInstr
	 */
	public long getSizeInstr() {
		return sizeInstr;
	}



	/**
	 * Imposta il numero di istruzioni nel programma, jcl o copy, Script Sql
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param sizeInstr the sizeInstr to set
	 */
	public void setSizeInstr(long sizeInstr) {
		this.sizeInstr = sizeInstr;
	}



	
	/**
	 * Restituisce il numero di <tt>funcion point</tt> stimati chiamati <tt>Backfired Function Point</tt><br>
	 * <p>
	 * I backfired function Point si ottengono moltiplicando le linee di codice, ovvero le linee <br>
	 * di codice contenenti istruzioni non commento e non blank.<br>
	 * Nel caso del Cobol il fattore di moltiplicazione è <tt>0,0057142857142857</tt>,<br>
	 * In altri termini si conteggia un <tt>FP</tt> ogni 175 LOC Cobol di procedure division.<br>
	 * <p>
	 * @return the backFiredFunctionPoint
	 */
	public double getBackFiredFunctionPoint() {
		return backFiredFunctionPoint;
	}

	/**
	 * Calcola il numero di <tt>funcion point</tt> stimati chiamati <tt>Backfired Function Point</tt><br>
	 * <p>
	 * I backfired function Point si ottengono moltiplicando le linee di codice, ovvero le linee <br>
	 * di codice contenenti istruzioni non commento e non blank.<br>
	 * Nel caso del Cobol il fattore di moltiplicazione è <tt>0,0057142857142857</tt>,<br>
	 * In altri termini si conteggia un <tt>FP</tt> ogni 175 LOC Cobol di procedure division.<br>
	 * <p>
	 */
	public void evaluateBackFiredFunctionPoint() {
		this.backFiredFunctionPoint = this.sizeLinesCodeLogical * 0.0057142857142857;
		return;
	}

	/**
	 * Calcola le percentuali delle <tt>violazioni</tt> rispetto a LOC, SLOC e INSTR<br>
	 * <p>
	 */
	public void evaluateViolationsPercent() {
		this.percViolationsByInstruction = (this.violations * 100d) / this.sizeInstr;
		this.percViolationsByLogical = (this.violations * 100d) / this.sizeLinesCodeLogical;
		this.percViolationsByPhisical = (this.violations * 100d) / this.sizeLinesCodePhisical;
		return;
	}

	/**
	 * Calcola le percentuali del codice dinamico e del codice dinamico light rispetto al numero di istruzioni<br>
	 * <p>
	 */
	public void evaluateDynamicCodePercent() {
		this.percDynamicInstr = (this.dynamicInstr * 100d) / this.sizeInstr;
		this.percDynamicInstrLight = (this.dynamicInstrLight * 100d) / this.sizeInstr;
		return;
	}


	/**
	 * Calcola la Squale Rule Compliance<br>
	 * <p>
	 * Indica quanto il codice sia conforme alle regole previste.<br>
	 * <p>
	 * Si utilizza la formula SSCI = 100 - (pesoRegole/LOC * 100)<br>
	 * <p>
	 * Dove pesoRegole è dato da:<br>
	 * <p>
	 * NumViolazioni Blocker*10<br>
	 * +<br>
	 * NumViolazioni Critical*5<br>
	 * +<br>
	 * NumViolazioni Major*3<br>
	 * +<br>
	 * NumViolazioni Minor*1<br>
	 * +<br>
	 * NumViolazioni Info*1<br>
	 * +<br>
	 * 
	 */
	public void evaluateSqualeRuleComplianceIndex() {
		long pesoRegole = 0;
		
		pesoRegole = this.squaleViolationsBlocker * 10
				   + this.squaleViolationsCritical * 5
				   + this.squaleViolationsMajor * 3
				   + this.squaleViolationsMinor * 1
				   + this.squaleViolationsInfo * 0;
		
		this.squaleSSCI = 100 - (((double) pesoRegole / this.sizeLinesCodeLogical) * 100);
		return;
	}
    
	/**
	 * Calcola la Squale Rating<br>
	 * <p>
	 * Indica quanto tempo deve essere speso per remediation rispetto al tempo di sviluppo.<br>
	 * Entrambi i costi sono stimati.<br>
	 * <p>
	 * Viene calcolato lo Squale rating complessivo e per ogni caratteristica di qualità.<br>
	 * <p>
	 * Si utilizzano le seguenti formule:<br>
	 * <p>
	 *  SSRI = SSQI / (tempo stimato sviluppo)<br> <tt>All Characteristics</tt>
	 * 	SRTI = SQTI / (tempo stimato sviluppo) <tt>Testabiliti</tt><br>				
	 *  SRRI = SQRI / (tempo stimato sviluppo) <tt>Reliability</tt> 	<br>	 
	 *  SRCI = SQCI / (tempo stimato sviluppo) <tt>Changeability</tt>	<br> 
	 *  SREI = SQEI / (tempo stimato sviluppo) <tt>Efficiency</tt> <br>		 
	 *  SRSI = SQSI / (tempo stimato sviluppo) <tt>Security</tt> <br>		 
	 *  SRMI = SQMI / (tempo stimato sviluppo) <tt>Maintenability</tt> <br>	 
	 *  SRPI = SQPI / (tempo stimato sviluppo) <tt>Portability</tt> <br>
	 * <p>
	 * I valori ottenuti sono collocate in una scala da A a E.<br>
	 * <p>
	 * A 0% 10%<br>
	 * B 10% 20%<br>
	 * C 20% 50%<br>
	 * D 50% 100%<br>
	 * E 100% >100%<br>
	 *  <p>	
	 *  I valori di rating vengono calcolati per ogni caratteristica:
	 *  <p>
	 *  SRTL Testability 		Livello A-E valore di rating 
	 *  SRRL Reliability 		Livello A-E valore di rating  
	 *  SRCL Changeability		Livello A-E valore di rating  
	 *  SREL Efficiency 		Livello A-E valore di rating  
	 *  SRSL Security 			Livello A-E valore di rating  
	 *  SRML Maintenability 	Livello A-E valore di rating 
	 *  SRPL Portability 		Livello A-E valore di rating  
	 */
	public void evaluateSqualeRatingIndex(Map<String, EnumMetricsSqualeRating> hs_squaleRating) {
        
		// Squale rating complessivo
		this.squaleSSRI = this.squaleSSQI / (this.timeDevelopment);
        
		// Squale rating per ogni caratteristica di qualità
		this.squaleSRTI = this.squaleSQTI / (this.timeDevelopment);	// Testability
		this.squaleSRRI = this.squaleSQRI / (this.timeDevelopment); // Reliability
		this.squaleSRCI = this.squaleSQCI / (this.timeDevelopment); // Changeability
		this.squaleSREI = this.squaleSQEI / (this.timeDevelopment); // Efficiency
		this.squaleSRSI = this.squaleSQSI / (this.timeDevelopment); // Security
		this.squaleSRMI = this.squaleSQMI / (this.timeDevelopment); // Maintenability
		this.squaleSRPI = this.squaleSQPI / (this.timeDevelopment); // Portability
		
		// Livello rating A-E complessivo
		this.squaleSSRL = getLevelSqualeRating(squaleSSRI, hs_squaleRating);	 
		
		// Livello rating A-E per ogni caratteristica di qualità
		this.squaleSRTL = getLevelSqualeRating(squaleSRTI * 100, hs_squaleRating); // Testability
		this.squaleSRRL = getLevelSqualeRating(squaleSRRI * 100, hs_squaleRating); // Reliability
		this.squaleSRCL = getLevelSqualeRating(squaleSRCI * 100, hs_squaleRating); // Changeability
		this.squaleSREL = getLevelSqualeRating(squaleSREI * 100 , hs_squaleRating); // Efficiency
		this.squaleSRSL = getLevelSqualeRating(squaleSRSI * 100, hs_squaleRating); // Security
		this.squaleSRML = getLevelSqualeRating(squaleSRMI * 100, hs_squaleRating); // Maintenability
		this.squaleSRPL = getLevelSqualeRating(squaleSRPI * 100, hs_squaleRating); // Portability
		return;
	}
    
	/*
	 * Restituisce l'enumerazione con il livello di rating della fascia A-E di appartenenza.
	 */
	private EnumMetricsSqualeRating getLevelSqualeRating(double squaleRating, Map<String, EnumMetricsSqualeRating> hs_squaleRating) {
		
		if (squaleRating < hs_squaleRating.get("SQALE_RATING_A").getThresholdHigh()) {
			return EnumMetricsSqualeRating.SQALE_RATING_A;
		}
		
		if (squaleRating > hs_squaleRating.get("SQALE_RATING_B").getThresholdLow() 
		&&  squaleRating < hs_squaleRating.get("SQALE_RATING_B").getThresholdHigh()) {
			return EnumMetricsSqualeRating.SQALE_RATING_B;
		}
		
		if (squaleRating > hs_squaleRating.get("SQALE_RATING_C").getThresholdLow() 
		&&  squaleRating < hs_squaleRating.get("SQALE_RATING_C").getThresholdHigh()) {
			return EnumMetricsSqualeRating.SQALE_RATING_C;
		}
		
		if (squaleRating > hs_squaleRating.get("SQALE_RATING_D").getThresholdLow() 
		&&  squaleRating < hs_squaleRating.get("SQALE_RATING_D").getThresholdHigh()) {
			return EnumMetricsSqualeRating.SQALE_RATING_D;
		}
		
		if (squaleRating > hs_squaleRating.get("SQALE_RATING_E").getThresholdLow()){
			return EnumMetricsSqualeRating.SQALE_RATING_E;
		}
		
		return EnumMetricsSqualeRating.NOT_ASSIGNED;
	}

	/**
	 * Calcola lo squale quality index.<br>
	 * <p>
	 * Si tratta di un costo di remediation complessivo e rappresenta<br>
	 * un debito tecnico associato al sorgente.<br>
	 * Si sommano i costi di remediation di tutte le caratteristiche <br>
	 * di qualità del modello Squale.<br>
	 * <p>
	 * Si utilizza la formula SSQI=SQTI+SQRI+SQCI+SQEI+SQSI+SQMI+SQPI
	 * <p>
	 * 
	 */
	public void evaluateSqualeQualityAbsoluteIndex() {
		this.squaleSSQI = this.squaleSQTI 
						+ this.squaleSQRI 
						+ this.squaleSQCI 
						+ this.squaleSQEI 
						+ this.squaleSQSI 
						+ this.squaleSQMI 
						+ this.squaleSQPI;
		return;
	}

	/**
	 * Calcola gli squale consolidated quality index di ogni caratteristica di qualità.<br>
	 * <p>
	 * Il modello Squale è piramidale basato sul ciclo di sviluppo.<br>
	 * Le caratteristiche di qualità sono associate a una fase del ciclo di sviluppo,<br>
	 * pertanto ogni attività di remediation di una caratteristica, influisce direttamente<br>
	 * su una fase del ciclo di sviluppo<br>
	 * Tuttavià attività in fase di rilascio generano attività anche nelle fasi di sviluppo<br>
	 * precedenti e ciò è valido per ogni fase del ciclo di sviluppo.<br>
	 * L'indice di qualità consolidato somma, per ogni caratteristica di qualità del modello,
	 * anche i costi di remediation delle caratteristiche di qualità precedenti, nel modello
	 * piramidale di Squale.<br>
	 * <p>
	 * Si utilizzano le formule:
	 * <p>
	 * Testability Squale consolidated Index 
	 * SCTI=SQTI<br>
	 * <p>
	 * Reliability Squale consolidated Index 
	 * SCRI= SQTI+SQRI<br>
	 *  <p>
	 * Changeability Squale consolidated Index 
	 * SCCI= SQTI+SQRI+SQCI<br>
	 *  <p>
	 * Efficiency Squale consolidated Index 
	 * SCEI= SQTI+SRI+SQCI+SQEI<br>
	 *  <p>
	 * Security Squale consolidated Index 
	 * SCSI= SQTI+SQRI+SQCI+SQEI+SQSI<br>
	 *  <p>
	 * Maintenability Squale consolidated Index 
	 * SCMI= SQTI+SQRI+SQCI+SQEI+SQSI+SQMI<br>
	 *  <p>
	 * Portability Squale consolidated Index 
	 * SCPI= SQTI+SQRI+SQCI+SQEI+SQSI+SQMI+SQPI<br>
	 *  <p>
	 */
	public void evaluateSqualeQualityConsolidateIndex() {
		
		this.squaleSCTI = this.squaleSQTI; // Testability Squale consolidated Index 
		this.squaleSCRI = this.squaleSQTI + this.squaleSQRI; // Reliability Squale consolidated Index  
		this.squaleSCCI = this.squaleSQTI + this.squaleSQRI + this.squaleSQCI; // Changeability Squale consolidated Index 
		this.squaleSCEI = this.squaleSQTI + this.squaleSQRI + this.squaleSQCI + this.squaleSQEI; // Efficiency Squale consolidated Index  
		this.squaleSCSI = this.squaleSQTI + this.squaleSQRI + this.squaleSQCI + this.squaleSQEI + this.squaleSQSI; // Security Squale consolidated Index 
		this.squaleSCMI = this.squaleSQTI + this.squaleSQRI + this.squaleSQCI + this.squaleSQEI + this.squaleSQSI + this.squaleSQMI; // Maintenability Squale consolidated Index  
		this.squaleSCPI = this.squaleSQTI + this.squaleSQRI + this.squaleSQCI + this.squaleSQEI + this.squaleSQSI + this.squaleSQMI + this.squaleSQPI; // Portability Squale consolidated Index   
		return;
	}

	/**
	 * Calcola gli indici di densità Squale per ogni caratteristica di qualità.<br>
	 * <p>
	 * Per ogni caratteristica di qualità si divide l'indice di qualità assoluto,<br>
	 * che rappresenta un costo di remediation, per un valore dimensionale.<br>
	 * Come valore dimensionale del sorgente è possibile utilizzare le misure di LOC,<br>
	 * numero istruzioni o complessità di McCabe.<br>
	 * In questa sede si utilizza come dimensione il numero di LOC.<br>
	 * <p>
	 * Vengono utilizzate le seguenti formule:<br>
	 * <p>
	 * SDTI = SQTI / Dimension <tt>Testability</tt><br>
	 * SDRI = SQRI / Dimension <tt>Reliability</tt> <br>	 
	 * SDCI = SQCI / Dimension <tt>Changeability</tt>	 <br>
	 * SDEI = SQEI / Dimension <tt>Efficiency</tt> <br>		 
	 * SDSI = SQSI / Dimension <tt>Security</tt> <br>		 
	 * SDMI = SQMI / Dimension <tt>Maintenability</tt> <br> 
	 * SDPI = SQPI / Dimension <tt>Portability</tt> <br> 
     * <p>
	 */
	public void evaluateSqualeDensityQualityIndex() {
		this.squaleSDTI = (double) this.squaleSQTI / (double) this.sizeLinesCodeLogical; // Testability
		this.squaleSDRI = (double) this.squaleSQRI / (double) this.sizeLinesCodeLogical; // Reliability
		this.squaleSDCI = (double) this.squaleSQCI / (double) this.sizeLinesCodeLogical; // Changeability
		this.squaleSDEI = (double) this.squaleSQEI / (double) this.sizeLinesCodeLogical; // Efficiency
		this.squaleSDSI = (double) this.squaleSQSI / (double) this.sizeLinesCodeLogical; // Security
		this.squaleSDMI = (double) this.squaleSQMI / (double) this.sizeLinesCodeLogical; // Maintenability
		this.squaleSDPI = (double) this.squaleSQPI / (double) this.sizeLinesCodeLogical; // Portability
		return;
	}
 



	/**
	 * Imposta il numero di <tt>funcion point</tt> stimati chiamati <tt>Backfired Function Point</tt><br>
	 * <p>
	 * I backfired function Point si ottengono moltiplicando le linee di codice, ovvero le linee <br>
	 * di codice contenenti istruzioni non commento e non blank, per un fattore di conversione.<br>
	 * Nel caso del Cobol il fattore di conversione è <tt>0,0057142857142857</tt>,<br>
	 * In altri termini si conteggia un <tt>FP</tt> ogni 175 LOC Cobol di procedure division.<br>
	 * <p>
	 * @param backFiredFunctionPoint the backFiredFunctionPoint to set
	 */
	public void setBackFiredFunctionPoint(double backFiredFunctionPoint) {
		this.backFiredFunctionPoint = backFiredFunctionPoint;
	}




	/**
	 * Restituisce il tempo di sviluppo stimato in giorni uomo<br>
	 * <p>
	 * Il tempo di sviluppo si ottiene moltiplicando le linee di codice, ovvero le linee <br>
	 * di codice contenenti istruzioni non commento e non blank, per un fattore di produttività.<br>
	 * Nel caso del Cobol il fattore di produttività è <tt>0,0033</tt>,<br>
	 * In altri termini si stima una produzione di <tt>30 LOC</tt> Cobol di procedure division al giorno.<br>
	 * <p>
	 * @return the timeDevelopment
	 */
	public double getTimeDevelopment() {
		return timeDevelopment;
	}

	/**
	 * Calcola il tempo di sviluppo stimato in giorni uomo<br>
	 * <p>
	 * Il tempo di sviluppo si ottiene moltiplicando le linee di codice, ovvero le linee <br>
	 * di codice contenenti istruzioni non commento e non blank, per un fattore di produttività.<br>
	 * Nel caso del Cobol il fattore di produttività è <tt>0,033</tt>,<br>
	 * In altri termini si stima una produzione di <tt>30 LOC</tt> Cobol di procedure division al giorno.<br>
	 * <p>
	 * @return the timeDevelopment
	 */
	public void evaluateTimeDevelopment() {
		this.timeDevelopment = this.sizeLinesCodeLogical * 0.033;
		return;
	}




	/**
	 * Imposta il tempo di sviluppo stimato in giorni uomo<br>
	 * <p>
	 * Il tempo di sviluppo si ottiene moltiplicando le linee di codice, ovvero le linee <br>
	 * di codice contenenti istruzioni non commento e non blank, per un fattore di produttività.<br>
	 * Nel caso del Cobol il fattore di produttività è <tt>0,0033</tt>,<br>
	 * In altri termini si stima una produzione di <tt>30 LOC</tt> Cobol di procedure division al giorno.<br>
	 * <p>
	 * @param timeDevelopment the timeDevelopment to set
	 */
	public void setTimeDevelopment(Double timeDevelopment) {
		this.timeDevelopment = timeDevelopment;
	}




	/**
	 * Restituisce il numero di campi definiti,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the defFields
	 */
	public long getDefFields() {
		return defFields;
	}



	/**
	 * Imposta il numero di campi definiti,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param defFields the defFields to set
	 */
	public void setDefFields(long defFields) {
		this.defFields = defFields;
	}



	/**
	 * Restituisce il numero di campi definiti nel copy,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the defFieldsInCopy
	 */
	public long getDefFieldsInCopy() {
		return defFieldsInCopy;
	}



	/**
	 * Imposta il numero di campi definiti nel copy,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param defFieldsInCopy the defFieldsInCopy to set
	 */
	public void setDefFieldsInCopy(long defFieldsInCopy) {
		this.defFieldsInCopy = defFieldsInCopy;
	}



	/**
	 * Restituisce il numero di literal definite,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the defLiterals
	 */
	public long getDefLiterals() {
		return defLiterals;
	}



	/**
	 * Imposta il numero di literal definite,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param defLiterals the defLiterals to set
	 */
	public void setDefLiterals(long defLiterals) {
		this.defLiterals = defLiterals;
	}



	/**
	 * Restituisce il numero di campi e variabili definite
	 * e non utilizzate,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the deadFields
	 */
	public long getDeadFields() {
		return deadFields;
	}



	/**
	 * Imposta il numero di campi e variabili definite
	 * e non utilizzate,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param deadField the deadField to set
	 */
	public void setDeadFields(long deadFields) {
		this.deadFields = deadFields;
	}



	/**
	 * Restituisce il numero di Section Cobol definite
	 * e non richiamate da nessuna perform,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the deadSubGraph
	 */
	public long getDeadSubGraph() {
		return deadSubGraph;
	}



	/**
	 * Imposta il numero di Section Cobol definite
	 * e non richiamate da nessuna perform,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param deadSubGraph the deadSubGraph to set
	 */
	public void setDeadSubGraph(long deadSubGraph) {
		this.deadSubGraph = deadSubGraph;
	}



	/**
	 * Restituisce il numero di label definite
	 * e non richiamate da nessuno,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the deadLabels
	 */
	public long getDeadLabels() {
		return deadLabels;
	}



	/**
	 * Imposta il numero di label definite
	 * e non richiamate da nessuno,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param deadLabels the deadLabel to set
	 */
	public void setDeadLabels(long deadLabels) {
		this.deadLabels = deadLabels;
	}



	/**
	 * Restituisce il numero di istruzioni definite
	 * e non richiamate da nessuno,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the deadInstr
	 */
	public long getDeadInstr() {
		return deadInstr;
	}




	/**
     * Imposta il numero di istruzioni definite
	 * e non richiamate da nessuno,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
 * @param deadInstr the deadInstr to set
	 */
	public void setDeadInstr(long deadInstr) {
		this.deadInstr = deadInstr;
	}




	/**
	 * Restituisce il numero di copy di data division definiti
	 * e non richiamati da nessun programma,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the deadCopy
	 */
	public long getDeadCopyData() {
		return deadCopyData;
	}



	/**
	 * Imposta il numero di copy di data division definiti
	 * e non richiamati da nessun programma,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param deadCopyData the deadCopyData to set
	 */
	public void setDeadCopyData(long deadCopyData) {
		this.deadCopyData = deadCopyData;
	}


	/**
	 * Restituisce il numero di copy di data division definiti
	 * e non richiamati da nessun programma,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the deadCopyProc
	 */
	public long getDeadCopyProc() {
		return deadCopyProc;
	}



	/**
	 * Imposta il numero di copy di proc division definiti
	 * e non richiamati da nessun programma,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param deadCopyProc the deadCopyProc to set
	 */
	public void setDeadCopyProc(long deadCopyProc) {
		this.deadCopyProc = deadCopyProc;
	}



	/**
	 * Restituisce il numero di moduli che richiamano
	 * il modulo corrente,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the structFanIn
	 */
	public long getStructFanIn() {
		return structFanIn;
	}



	/**
	 * Imposta il numero di moduli che richiamano
	 * il modulo corrente,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param structFanIn the structFanIn to set
	 */
	public void setStructFanIn(long structFanIn) {
		this.structFanIn = structFanIn;
	}



	/**
	 * Restituisce il numero di moduli chiamati dal modulo corrente,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the structFanOut
	 */
	public long getStructFanOut() {
		return structFanOut;
	}



	/**
	 * Imposta il numero di moduli chiamati dal modulo corrente,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param structFanOut the structFanOut to set
	 */
	public void setStructFanOut(long structFanOut) {
		this.structFanOut = structFanOut;
	}



	/**
	 * Restituisce il numero di section Cobol richiamate con perform.<br>
	 * <p>
	 * @return the structSections
	 */
	public long getStructSections() {
		return structSections;
	}




	/**
	 * Imposta il numero di section Cobol richiamate con perform.<br>
	 * <p>
	 * @param structSections the structSections to set
	 */
	public void setStructSections(long structSections) {
		this.structSections = structSections;
	}




	/**
	 * Restituisce il numero di paragrafi Cobol richiamati con perform.<br>
	 * <p>
	 * @return the structParagraphs
	 */
	public long getStructParagraphs() {
		return structParagraphs;
	}




	/**
	 * Imposa il numero di paragrafi Cobol richiamati con perform.<br>
	 * <p>
	 * @param structParagraphs the structParagraphs to set
	 */
	public void setStructParagraphs(long structParagraphs) {
		this.structParagraphs = structParagraphs;
	}




	/**
	 * Restituisce il numero di oggetti presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcObjects
	 */
	public long getFuncObjects() {
		return funcObjects;
	}




	/**
	 * Imposta il numero di oggetti presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcObjects the funcObjects to set
	 */
	public void setFuncObjects(long funcObjects) {
		this.funcObjects = funcObjects;
	}




	/**
	 * Restituisce il numero di relazioni del programma corrente,
	 * con tutti gli altri oggetti,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcRelations
	 */
	public long getFuncRelations() {
		return funcRelations;
	}



	/**
	 * Imposta il numero di relazioni del programma corrente,
	 * con tutti gli altri oggetti,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcRelations the funcRelations to set
	 */
	public void setFuncRelations(long funcRelations) {
		this.funcRelations = funcRelations;
	}



	/**
	 * Restituisce il numero transazioni interne richiamate con Exec Cics Start
	 * o Exec Cics Return Transid,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcTranInternal
	 */
	public long getFuncTranInternal() {
		return funcTranInternal;
	}



	/**
	 * Imposta il numero transazioni interne richiamate con Exec Cics Start
	 * o Exec Cics Return Transid,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcTranInternal the funcTranInternal to set
	 */
	public void setFuncTranInternal(long funcTranInternal) {
		this.funcTranInternal = funcTranInternal;
	}



	/**
	 * Restituisce il numero transazioni esterne richiamate con Exec Cics Start
	 * o Exec Cics Return Transid,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcTranExternal
	 */
	public long getFuncTranExternal() {
		return funcTranExternal;
	}



	/**
	 * Imposta il numero transazioni esterne richiamate con Exec Cics Start
	 * o Exec Cics Return Transid,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcTranExternal the funcTranExternal to set
	 */
	public void setFuncTranExternal(long funcTranExternal) {
		this.funcTranExternal = funcTranExternal;
	}



	/**
	 * Restituisce il numero di mappe video utilizzate
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcMap
	 */
	public long getFuncMap() {
		return funcMap;
	}



	/**
	 * Imposta il numero di mappe video utilizzate
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcMap the funcMap to set
	 */
	public void setFuncMap(long funcMap) {
		this.funcMap = funcMap;
	}



	/**
	 * Restituisce il numero di call a moduli interni
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcCallInternal
	 */
	public long getFuncCallInternal() {
		return funcCallInternal;
	}



	/**
	 * Imposta il numero di call a moduli interni
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcCallInternal the funcCallInternal to set
	 */
	public void setFuncCallInternal(long funcCallInternal) {
		this.funcCallInternal = funcCallInternal;
	}



	/**
	 * Restituisce il numero di call a moduli esterni
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcCallExternal
	 */
	public long getFuncCallExternal() {
		return funcCallExternal;
	}



	/**
	 * Imposta il numero di call a moduli esterni
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcCallExternal the funcCallExternal to set
	 */
	public void setFuncCallExternal(long funcCallExternal) {
		this.funcCallExternal = funcCallExternal;
	}



	/**
	 * Restituisce il numero di accessi a entity (tabelle db) interni
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcAccEntityInternal
	 */
	public long getFuncAccEntityInternal() {
		return funcAccEntityInternal;
	}



	/**
	 * Imposta il numero di accessi a entity (tabelle db) interni
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcAccEntityInternal the funcAccEntityInternal to set
	 */
	public void setFuncAccEntityInternal(long funcAccEntityInternal) {
		this.funcAccEntityInternal = funcAccEntityInternal;
	}



	/**
	 * Restituisce il numero di accessi a entity (tabelle db) esterne
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcAccEntityExternal
	 */
	public long getFuncAccEntityExternal() {
		return funcAccEntityExternal;
	}



	/**
	 * Imposta il numero di accessi a entity (tabelle db) esterne
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcAccEntityExternal the funcAccEntityExternal to set
	 */
	public void setFuncAccEntityExternal(long funcAccEntityExternal) {
		this.funcAccEntityExternal = funcAccEntityExternal;
	}



	/**
	 * Restituisce il numero di accessi a files sequenziali/Vsam/code ts/.. interni
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcAccMediaInternal
	 */
	public long getFuncAccMediaInternal() {
		return funcAccMediaInternal;
	}



	/**
	 * Imposta il numero di accessi a files sequenziali/Vsam/code ts/.. interni
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcAccMediaInternal the funcAccMediaInternal to set
	 */
	public void setFuncAccMediaInternal(long funcAccMediaInternal) {
		this.funcAccMediaInternal = funcAccMediaInternal;
	}



	/**
	 * Restituisce il numero di accessi a files sequenziali/Vsam/code ts/.. esterni
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the funcAccMediaExternal
	 */
	public long getFuncAccMediaExternal() {
		return funcAccMediaExternal;
	}



	/**
	 * Imposta il numero di accessi a files sequenziali/Vsam/code ts/.. esterni
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param funcAccMediaExternal the funcAccMediaExternal to set
	 */
	public void setFuncAccMediaExternal(long funcAccMediaExternal) {
		this.funcAccMediaExternal = funcAccMediaExternal;
	}


	/**
	 * <tt>Function Point ExternalOutput EO<br>
	 * <p>
	 * Restituisce il numero di funzionalità utente (transazione o job),
	 * con output generati da un ILF o EIF<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @return the fpExternalOutputEO
	 */
	public long getFpExternalOutputEO() {
		return fpExternalOutputEO;
	}

 


	/**
 	 * <tt>Function Point ExternalOutput EO<br>
	 * <p>
	 * Imposta il numero di funzionalità utente (transazione o job),
	 * con output generati da un ILF o EIF<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * 
	 * @param fpExternalOutputEO the fpExternalOutputEO to set
	 */
	public void setFpExternalOutputEO(long fpExternalOutputEO) {
		this.fpExternalOutputEO = fpExternalOutputEO;
	}




	/**
	 * <tt>Function Point External Input EI<br>
	 * <p>
	 * Restituisce il numero di funzionalità utente (transazione o job),
	 * caratterizzati da update di dati (add, change, dele, uppdate) 
	 * di un ILF<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @return the fpExternalInputEI
	 */
	public long getFpExternalInputEI() {
		return fpExternalInputEI;
	}




	/**
	 * <tt>Function Point External Input EI<br>
	 * <p>
	 * Imposta il numero di funzionalità utente (transazione o job),
	 * caratterizzati da update di dati (add, change, dele, uppdate) 
	 * di un ILF<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * 
	 * @param fpExternalInputEI the fpExternalInputEI to set
	 */
	public void setFpExternalInputEI(long fpExternalInputEI) {
		this.fpExternalInputEI = fpExternalInputEI;
	}




	/**
	 * <tt>Function Point External Inquiry EQ<br>
	 * <p>
	 * Restituisce il numero di funzionalità utente (transazione o job),
	 * caratterizzati da sole  read di dati di un ILF o un EIF.<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @return the fpExternalInquiryEQ
	 */
	public long getFpExternalInquiryEQ() {
		return fpExternalInquiryEQ;
	}




	/**
	 * <tt>Function Point External Inquiry EQ<br>
	 * <p>
	 * Imposta il numero di funzionalità utente (transazione o job),
	 * caratterizzati da sole  read di dati di un ILF o un EIF.<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @param fpExternalInquiryEQ the fpExternalInquiryEQ to set
	 */
	public void setFpExternalInquiryEQ(long fpExternalInquiryEQ) {
		this.fpExternalInquiryEQ = fpExternalInquiryEQ;
	}




	/**
	 * <tt>Function Point Internal Logical File ILF<br>
	 * <p>
	 * Restituisce il numero di tabelle e/o files residenti all'interno
	 * del confine dell'applicazione e gestite dallo stesso, ovvero
	 * nell'ambito del sistema/sottosistema proprietario.<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @return the fpInternalLogicalFileILF
	 */
	public long getFpInternalLogicalFileILF() {
		return fpInternalLogicalFileILF;
	}




	/**
	 * <tt>Function Point Internal Logical File ILF<br>
	 * <p>
	 * Imposta il numero di tabelle e/o files residenti all'interno
	 * del confine dell'applicazione e gestite dallo stesso, ovvero
	 * nell'ambito del sistema/sottosistema proprietario.<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @param fpInternalLogicalFileILF the fpInternalLogicalFileILF to set
	 */
	public void setFpInternalLogicalFileILF(long fpInternalLogicalFileILF) {
		this.fpInternalLogicalFileILF = fpInternalLogicalFileILF;
	}




	/**
	 * <tt>Function Point External Interface File EIF<br>
	 * <p>
	 * Restituisce il numero di tabelle e/o files residenti all'esterno
	 * del confine dell'applicazione e gestite da altre applicazioni,
	 * ovvero da un sistema/sottosistema diverso.<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @return the fpExternalInterfaceFileEIF
	 */
	public long getFpExternalInterfaceFileEIF() {
		return fpExternalInterfaceFileEIF;
	}




	/**
	 * <tt>Function Point External Interface File EIF<br>
	 * <p>
	 * Imposta il numero di tabelle e/o files residenti all'esterno
	 * del confine dell'applicazione e gestite da altre applicazioni,
	 * ovvero da un sistema/sottosistema diverso.<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @param fpExternalInterfaceFileEIF the fpExternalInterfaceFileEIF to set
	 */
	public void setFpExternalInterfaceFileEIF(long fpExternalInterfaceFileEIF) {
		this.fpExternalInterfaceFileEIF = fpExternalInterfaceFileEIF;
	}




	/**
	 * Misura di rehosting.<br>
	 * Restituisce il rapporto fra il numero di oggetti
	 * del perimetro (sistema/sottosistema proprietario)
	 * e il numero di relazioni.<br>
	 * <p>
	 * @return the rhRateObjectRelation
	 */
	public double getRhRateObjectRelation() {
		return rhRateObjectRelation;
	}




	/**
	 * Misura di rehosting.<br>
	 * Imposta il rapporto fra il numero di oggetti
	 * del perimetro (sistema/sottosistema proprietario)
	 * e il numero di relazioni.<br>
	 * <p>
	 * @param rhRateObjectRelation the rhRateObjectRelation to set
	 */
	public void setRhRateObjectRelation(double rhRateObjectRelation) {
		this.rhRateObjectRelation = rhRateObjectRelation;
	}




	/**
	 * Misura di rehosting.<br>
	 * Restituisce il il numero di oggetti definiti
	 * nel perimetro (sistema/sottosistema proprietario).<br>
	 * <p>
	 * @return the rhObjectsInternal
	 */
	public long getRhObjectsInternal() {
		return rhObjectsInternal;
	}




	/**
	 * Misura di rehosting.<br>
	 * Imposta il il numero di oggetti definiti
	 * nel perimetro (sistema/sottosistema proprietario).<br>
	 * <p>

	 * @param rhObjectsInternal the rhObjectsInternal to set
	 */
	public void setRhObjectsInternal(long rhObjectsInternal) {
		this.rhObjectsInternal = rhObjectsInternal;
	}




	/**
	 * Misura di rehosting.<br>
	 * Restituisce il il numero di oggetti definiti
	 * esternamente al perimetro (sistema/sottosistema proprietario).<br>
	 * <p>
	 * @return the rhObjectsExternal
	 */
	public long getRhObjectsExternal() {
		return rhObjectsExternal;
	}




	/**
	 * Misura di rehosting.<br>
	 * Imposta il il numero di oggetti definiti
	 * esternamente al perimetro (sistema/sottosistema proprietario).<br>
	 * <p>
	 * @param rhObjectsExternal the rhObjectsExternal to set
	 */
	public void setRhObjectsExternal(long rhObjectsExternal) {
		this.rhObjectsExternal = rhObjectsExternal;
	}




	/**
	 * Misura di rehosting.<br>
	 * Restituisce il il numero di oggetti non portabili
	 * in una attività di rehosting.<br>
	 * Si tratta di moduli Assembler, PL1, Load etc.<br>
	 * <p>
	 * @return the rhObjectsUnportable
	 */
	public long getRhObjectsUnportable() {
		return rhObjectsUnportable;
	}




	/**
	 * Misura di rehosting.<br>
	 * Imposta il il numero di oggetti non portabili
	 * in una attività di rehosting.<br>
	 * Si tratta di moduli Assembler, PL1, Load etc.<br>
	 * <p>
	 * @param rhObjectsUnportable the rhObjectsUnportable to set
	 */
	public void setRhObjectsUnportable(long rhObjectsUnportable) {
		this.rhObjectsUnportable = rhObjectsUnportable;
	}




	/**
	 * Misura di rehosting.<br>
	 * Restituisce il il numero di tabelle/files contenenti
	 * campi binari.<br>
	 * <p>
	 * @return the rhFilesBynary
	 */
	public long getRhFilesBynary() {
		return rhFilesBynary;
	}




	/**
	 * Misura di rehosting.<br>
	 * Imposta il il numero di tabelle/files contenenti
	 * campi binari.<br>
	 * <p>
	 * @param rhFilesBynary the rhFilesBynary to set
	 */
	public void setRhFilesBynary(long rhFilesBynary) {
		this.rhFilesBynary = rhFilesBynary;
	}




	/**
	 * Restituisce il numero di DD definite nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the jclDD
	 */
	public long getJclDD() {
		return jclDD;
	}



	/**
	 * Imposta il numero di DD definite nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param jclDD the jclDD to set
	 */
	public void setJclDD(long jclDD) {
		this.jclDD = jclDD;
	}



	/**
	 * Restituisce il numero di step definite nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the jclStepDefined
	 */
	public long getJclStepDefined() {
		return jclStepDefined;
	}



	/**
	 * Imposta il numero di step definiti nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param jclStepDefined the jclStepDefined to set
	 */
	public void setJclStepDefined(long jclStepDefined) {
		this.jclStepDefined = jclStepDefined;
	}



	/**
	 * Restituisce il numero di step in update definiti nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the jclStepUpdate
	 */
	public long getJclStepUpdate() {
		return jclStepUpdate;
	}



	/**
	 * Imposta il numero di step in update definiti nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param jclStepUpdate the jclStepUpdate to set
	 */
	public void setJclStepUpdate(long jclStepUpdate) {
		this.jclStepUpdate = jclStepUpdate;
	}



	/**
	 * @return the jclDsname
	 */
	public long getJclDsname() {
		return jclDsname;
	}



	/**
	 * @param jclDsname the jclDsname to set
	 */
	public void setJclDsname(long jclDsname) {
		this.jclDsname = jclDsname;
	}



	/**
	 * @return the jclDsnameReferenced
	 */
	public long getJclDsnameReferenced() {
		return jclDsnameReferenced;
	}



	/**
	 * @param jclDsnameReferenced the jclDsnameReferenced to set
	 */
	public void setJclDsnameReferenced(long jclDsnameReferenced) {
		this.jclDsnameReferenced = jclDsnameReferenced;
	}



	/**
	 * Restituisce il numero di dsname in update definiti nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the jclDsnameUnReferenced
	 */
	public long getJclDsnameUnReferenced() {
		return jclDsnameUnReferenced;
	}



	/**
	 * Imposta il numero di dsname in update definiti nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param jclDsnameUnReferenced the jclDsnameUnReferenced to set
	 */
	public void setJclDsnameUnReferenced(long jclDsnameUnReferenced) {
		this.jclDsnameUnReferenced = jclDsnameUnReferenced;
	}



	/**
	 * Restituisce il numero di include richiamate nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the jclIncludeCalled
	 */
	public long getJclIncludeCalled() {
		return jclIncludeCalled;
	}



	/**
	 * Imposta il numero di include richiamate nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param jclIncludeCalled the jclIncludeCalled to set
	 */
	public void setJclIncludeCalled(long jclIncludeCalled) {
		this.jclIncludeCalled = jclIncludeCalled;
	}



	/**
	 * Restituisce il numero di proc richiamate nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the jclProcCalled
	 */
	public long getJclProcCalled() {
		return jclProcCalled;
	}



	/**
	 * Imposta il numero di proc richiamate nel jcl,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param jclProcCalled the jclProcCalled to set
	 */
	public void setJclProcCalled(long jclProcCalled) {
		this.jclProcCalled = jclProcCalled;
	}



	/**
	 * Restituisce il numero di archi del grafo di programma,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the mcCabeArcs
	 */
	public long getMcCabeArcs() {
		return mcCabeArcs;
	}



	/**
	 * Imposta il numero di archi del grafo di programma,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param mcCabeArcs the mcCabeArcs to set
	 */
	public void setMcCabeArcs(long mcCabeArcs) {
		this.mcCabeArcs = mcCabeArcs;
	}



	/**
	 * Restituisce il numero di nodi del grafo di programma,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the mcCabeNodes
	 */
	public long getMcCabeNodes() {
		return mcCabeNodes;
	}



	/**
	 * Imposta il numero di nodi del grafo di programma,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param mcCabeNodes the mcCabeNodes to set
	 */
	public void setMcCabeNodes(long mcCabeNodes) {
		this.mcCabeNodes = mcCabeNodes;
	}



	/**
	 * Restituisce il numero di operatori logici AND e OR
	 * presenti nei nodi condizione.<br>
	 * Si utilizza per il calcolo esteso della complessità
	 * ciclomatica.<br> 
	 * 
	 * @return the mcCabeOperatorsOrAnd
	 */
	public long getMcCabeOperatorsOrAnd() {
		return mcCabeOperatorsOrAnd;
	}




	/**
	 * Imposta il numero di operatori logici AND e OR
	 * presenti nei nodi condizione.<br>
	 * Si utilizza per il calcolo esteso della complessità
	 * ciclomatica.<br> 
	 * 
	 * @param mcCabeOperatorsOrAnd the mcCabeOperatorsOrAnd to set
	 */
	public void setMcCabeOperatorsOrAnd(long mcCabeOperatorsOrAnd) {
		this.mcCabeOperatorsOrAnd = mcCabeOperatorsOrAnd;
	}




	/**
	 * Restituisce il numero di sottografi sconnessi nel grafo di programma,
	 * ovvero le section cobol non referenziate,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the mcCabeGraphConn
	 */
	public long getMcCabeGraphConn() {
		return mcCabeGraphConn;
	}



	/**
	 * Imposta il numero di sottografi sconnessi nel grafo di programma,
	 * ovvero le section cobol non referenziate,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param mcCabeGraphConn the mcCabeGraphConn to set
	 */
	public void setMcCabeGraphConn(long mcCabeGraphConn) {
		this.mcCabeGraphConn = mcCabeGraphConn;
	}



	/**
	 * Restituisce il numero di operatori del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the halsteadOperators
	 */
	public long getHalsteadOperators() {
		return halsteadOperators;
	}



	/**
	 * Imposta il numero di operatori del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param halsteadOperators the halsteadOperators to set
	 */
	public void setHalsteadOperators(long halsteadOperators) {
		this.halsteadOperators = halsteadOperators;
	}



	/**
	 * Restituisce il numero di operandi del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the halsteadOperands
	 */
	public long getHalsteadOperands() {
		return halsteadOperands;
	}



	/**
	 * Imposta il numero di operandi del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param halsteadOperands the halsteadOperands to set
	 */
	public void setHalsteadOperands(long halsteadOperands) {
		this.halsteadOperands = halsteadOperands;
	}



	/**
	 * Restituisce il numero di occorrenze di operatori del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the halsteadOperatorsOcc
	 */
	public long getHalsteadOperatorsOcc() {
		return halsteadOperatorsOcc;
	}



	/**
	 * Imposta il numero di occorrenze di operatori del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param halsteadOperatorsOcc the halsteadOperatorsOcc to set
	 */
	public void setHalsteadOperatorsOcc(long halsteadOperatorsOcc) {
		this.halsteadOperatorsOcc = halsteadOperatorsOcc;
	}



	/**
	 * Restituisce il numero di occorrenze di operandi del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the halsteadOperandssOcc
	 */
	public long getHalsteadOperandsOcc() {
		return halsteadOperandsOcc;
	}



	/**
	 * Imposta il numero di occorrenze di operandi del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param halsteadOperandssOcc the halsteadOperandssOcc to set
	 */
	public void setHalsteadOperandsOcc(long halsteadOperandssOcc) {
		this.halsteadOperandsOcc = halsteadOperandssOcc;
	}



	/**
	 * Restituisce la lunghezza del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the halsteadLengthPgm
	 */
	public long getHalsteadLengthPgm() {
		return halsteadLengthPgm;
	}



	/**
	 * Imposta la lunghezza del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param halsteadLengthPgm the halsteadLengthPgm to set
	 */
	public void setHalsteadLengthPgm(long halsteadLengthPgm) {
		this.halsteadLengthPgm = halsteadLengthPgm;
	}



	/**
	 * Restituisce il vocabolario del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the halsteadVocabularyPgm
	 */
	public long getHalsteadVocabularyPgm() {
		return halsteadVocabularyPgm;
	}



	/**
	 * Imposta il vocabolario del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param halsteadVocabularyPgm the halsteadVocabularyPgm to set
	 */
	public void setHalsteadVocabularyPgm(long halsteadVocabularyPgm) {
		this.halsteadVocabularyPgm = halsteadVocabularyPgm;
	}



	/**
	 * Restituisce il volume del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the halsteadVolumePgm
	 */
	public double getHalsteadVolumePgm() {
		return halsteadVolumePgm;
	}



	/**
	 * Imposta il volume del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param halsteadVolumePgm the halsteadVolumePgm to set
	 */
	public void setHalsteadVolumePgm(double halsteadVolumePgm) {
		this.halsteadVolumePgm = halsteadVolumePgm;
	}



	/**
	 * Restituisce la difficoltà del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the halsteadDifficultPgm
	 */
	public double getHalsteadDifficultPgm() {
		return halsteadDifficultPgm;
	}



	/**
	 * Imposta la difficoltà del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param halsteadDifficultPgm the halsteadDifficultPgm to set
	 */
	public void setHalsteadDifficultPgm(double halsteadDifficultPgm) {
		this.halsteadDifficultPgm = halsteadDifficultPgm;
	}



	/**
	 * Restituisce lo sforzo del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the halsteadEffortPgm
	 */
	public double getHalsteadEffortPgm() {
		return halsteadEffortPgm;
	}



	/**
	 * Imposta lo sforzo del programma secondo Halstead,
	 * presenti nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param halsteadEffortPgm the halsteadEffortPgm to set
	 */
	public void setHalsteadEffortPgm(double halsteadEffortPgm) {
		this.halsteadEffortPgm = halsteadEffortPgm;
	}

	
	
	/**
	 * Restituisce il tempo stimato di scrittura programma secondo Halstead.<br>
	 * <p>
	 * Si applica la formula T = E / S<br>
	 * <p>
	 * Dove E è lo sforszo Halstead e S il numero di Stroud ovvero <br>
	 * il numero di discriminanti mentali stimate per secondo, fra 5 e 20.<br>
	 * Si utilizza il valore 18.<br>
	 * Il tempo T è espresso in secondi.<br>
	 * <p>
	 * @return the halsteadTimeWriting
	 */
	public long getHalsteadTimeWriting() {
		return halsteadTimeWriting;
	}




	/**
	 * Imposta il tempo stimato di scrittura programma secondo Halstead.<br>
	 * <p>
	 * Si applica la formula T = E / S<br>
	 * <p>
	 * Dove E è lo sforszo Halstead e S il numero di Stroud ovvero <br>
	 * il numero di discriminanti mentali stimate per secondo, fra 5 e 20.<br>
	 * Si utilizza il valore 18.<br>
	 * Il tempo T è espresso in secondi.<br>
	 * <p>
	 * @param halsteadTimeWriting the halsteadTimeWriting to set
	 */
	public void setHalsteadTimeWriting(long halsteadTimeWriting) {
		this.halsteadTimeWriting = halsteadTimeWriting;
	}




	/**
	 * Restituisce il numero di programmi con istruzioni dinamiche.<br>
	 * <p>
	 * @return the dynamicPgm
	 */
	public long getDynamicPgm() {
		return dynamicPgm;
	}




	/**
	 * Imposta il numero di programmi con istruzioni dinamiche.<br>
	 * <p>
	 * @param dynamicPgm the dynamicPgm to set
	 */
	public void setDynamicPgm(long dynamicPgm) {
		this.dynamicPgm = dynamicPgm;
	}




	/**
	 * Restituisce il numero di istruzioni dinamiche totali.<br>
	 * <p>
	 * @return the dynamicInstr 
	 */
	public long getDynamicInstr() {
		return dynamicInstr;
	}




	/**
	 * Imposta il numero di istruzioni dinamiche totali.<br>
	 * <p>
	 * @param dynamicInstr the dynamicInstr to set
	 */
	public void setDynamicInstr(long dynamicInstr) {
		this.dynamicInstr = dynamicInstr;
	}




	/**
	 * Restituisce il numero di istruzioni dinamiche light.<br>
	 * <p>
	 * Per istruzione dinamica light si intende una istruzione dinamica<br>
	 * i cui campi non sono movimentati e hanno un valore iniziale.<br>
	 * <p>
	 * @return the dynamicInstrLight
	 */
	public long getDynamicInstrLight() {
		return dynamicInstrLight;
	}




	/**
	 * Imposta il numero di istruzioni dinamiche light.<br>
	 * <p>
	 * Per istruzione dinamica light si intende una istruzione dinamica<br>
	 * i cui campi non sono movimentati e hanno un valore iniziale.<br>
	 * <p>
	 * @param dynamicInstrLight the dynamicInstrLight to set
	 */
	public void setDynamicInstrLight(long dynamicInstrLight) {
		this.dynamicInstrLight = dynamicInstrLight;
	}




	/**
	 * Restituisce la percentuale di istruzioni dinamiche sul numero totale di istruzioni.<br>
	 * <p>
	 * @return the percDynamicInstr
	 */
	public double getPercDynamicInstr() {
		return percDynamicInstr;
	}

	/**
	 * Imposta la percentuale di istruzioni dinamiche sul numero totale di istruzioni.<br>
	 * <p>
	 * @param percDynamicInstr the percDynamicInstr to set
	 */
	public void setPercDynamicInstr(double percDynamicInstr) {
		this.percDynamicInstr = percDynamicInstr;
	}

	/**
	 * Restituisce la percentuale di istruzioni dinamiche light sul numero totale di istruzioni.<br>
	 * <p>
	 * Si tratta di istruzioni dinamiche risolvibili con senza attivare il gestore di logice dinamiche<br>
	 * in quanto i campi dinamici sono inizializzati, senza modifiche nel programma.<br>
	 * <p>
	 * @return the percDynamicInstrLight
	 */
	public double getPercDynamicInstrLight() {
		return percDynamicInstrLight;
	}

	/**
	 * Imposta la percentuale di istruzioni dinamiche light sul numero totale di istruzioni.<br>
	 * <p>
	 * Si tratta di istruzioni dinamiche risolvibili con senza attivare il gestore di logice dinamiche<br>
	 * in quanto i campi dinamici sono inizializzati, senza modifiche nel programma.<br>
	 * <p>
	 * @param percDynamicInstrLight the percDynamicInstrLight to set
	 */
	public void setPercDynamicInstrLight(double percDynamicInstrLight) {
		this.percDynamicInstrLight = percDynamicInstrLight;
	}

	
	
	
	/**
	 * Restituisce il numero di violazioni individuate.<br>
	 * <p>
	 * @return the violations
	 */
	public long getViolations() {
		return violations;
	}

	/**
	 * Imposta il numero di violazioni individuate.<br>
	 * <p>
	 * @param violations the violations to set
	 */
	public void setViolations(long violations) {
		this.violations = violations;
	}

	/**
	 * Restituisce la percentuale di violazioni rispetto al numero<br>
	 * di linee di codice logiche in procedure division (LOC)<br>
	 * senza commenti e righe vuote.<br>
	 * <p> 
	 * @return the percViolationsByLogical
	 */
	public double getPercViolationsByLogical() {
		return percViolationsByLogical;
	}

	/**
	 * Imposta la percentuale di violazioni rispetto al numero<br>
	 * di linee di codice logiche in procedure division (LOC)<br>
	 * senza commenti e righe vuote.<br>
	 * <p> 
	 * @param percViolationsByLogical the percViolationsByLogical to set
	 */
	public void setPercViolationsByLogical(double percViolationsByLogical) {
		this.percViolationsByLogical = percViolationsByLogical;
	}

	/**
	 * Restituisce la percentuale di violazioni rispetto al numero<br>
	 * di linee di codice fisiche in procedure division (LOC)<br>
	 * includenti commenti e righe vuote.<br>
	 * <p> 
	 * @return the percViolationsByPhisical
	 */
	public double getPercViolationsByPhisical() {
		return percViolationsByPhisical;
	}

	/**
	 * Imposta la percentuale di violazioni rispetto al numero<br>
	 * di linee di codice fisiche in procedure division (LOC)<br>
	 * includenti commenti e righe vuote.<br>
	 * <p> 
	 * @param percViolationsByPhisical the percViolationsByPhisical to set
	 */
	public void setPercViolationsByPhisical(double percViolationsByPhisical) {
		this.percViolationsByPhisical = percViolationsByPhisical;
	}

	/**
	 * Restituisce la percentuale di violazioni rispetto al numero<br>
	 * di istruzioni in procedure division.<br>
	 * <p> 
	 * @return the percViolationsByInstruction
	 */
	public double getPercViolationsByInstruction() {
		return percViolationsByInstruction;
	}

	/**
	 * Imposta la percentuale di violazioni rispetto al numero<br>
	 * di istruzioni in procedure division.<br>
	 * <p> 
	 * @param percViolationsByInstruction the percViolationsByInstruction to set
	 */
	public void setPercViolationsByInstruction(double percViolationsByInstruction) {
		this.percViolationsByInstruction = percViolationsByInstruction;
	}

	/**
	 * Restituisce l'indice di McCabe,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxMcCabeAvg
	 */
	public double getIdxMcCabeAvg() {
		return idxMcCabeAvg;
	}



	/**
	 * Imposta l'indice di McCabe,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxMcCabeAvg the idxMcCabeAvg to set
	 */
	public void setIdxMcCabeAvg(double idxMcCabeAvg) {
		this.idxMcCabeAvg = idxMcCabeAvg;
	}



	/**
	 * Restituisce l'indice di manutenibilità MI,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxMIAvg
	 */
	public double getIdxMIAvg() {
		return idxMIAvg;
	}



	/**
	 * Imposta l'indice di manutenibilità MI,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxMIAvg the idxMIAvg to set
	 */
	public void setIdxMIAvg(double idxMIAvg) {
		this.idxMIAvg = idxMIAvg;
	}



	/**
	 * Restituisce il numero di Function Point,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxFPAvg
	 */
	public double getIdxFPAvg() {
		return idxFPAvg;
	}



	/**
	 * Imposta il numero di Function Point,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxFPAvg the idxFPAvg to set
	 */
	public void setIdxFPAvg(double idxFPAvg) {
		this.idxFPAvg = idxFPAvg;
	}



	/**
	 * Restituisce l'indice di rehosting,
	 * che misura la complessità. difficoltà
	 * di rehosting, nel livello corrente 
	 * di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxReHostingAvg
	 */
	public double getIdxReHostingAvg() {
		return idxReHostingAvg;
	}




	/**
	 * Imposta l'indice di rehosting,
	 * che misura la complessità. difficoltà
	 * di rehosting, nel livello corrente 
	 * di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxReHostingAvg the idxReHostingAvg to set
	 */
	public void setIdxReHostingAvg(double idxReHostingAvg) {
		this.idxReHostingAvg = idxReHostingAvg;
	}


	


	/**
	 * Restituisce l'indice di McCabe,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxMcCabeLow
	 */
	public double getIdxMcCabeLow() {
		return idxMcCabeLow;
	}



	/**
	 * Imposta l'indice di McCabe,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxMcCabeLow the idxMcCabeLow to set
	 */
	public void setIdxMcCabeLow(double idxMcCabeLow) {
		this.idxMcCabeLow = idxMcCabeLow;
	}




	/**
	 * Restituisce l'indice di manutenibilità MI,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxMILow
	 */
	public double getIdxMILow() {
		return idxMILow;
	}



	/**
	 * Imposta l'indice di manutenibilità MI,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxMILow the idxMILow to set
	 */
	public void setIdxMILow(double idxMILow) {
		this.idxMILow = idxMILow;
	}



	/**
	 * Restituisce il numero di Function Point,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxFPLow
	 */
	public double getIdxFPLow() {
		return idxFPLow;
	}



	/**
	 * Imposta il numero di Function Point,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxFPLow the idxFPLow to set
	 */
	public void setIdxFPLow(double idxFPLow) {
		this.idxFPLow = idxFPLow;
	}



	/**
	 * Restituisce l'indice di rehosting,
	 * che misura la complessità. difficoltà
	 * di rehosting, nel livello corrente 
	 * di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxReHostingLow
	 */
	public double getIdxReHostingLow() {
		return idxReHostingLow;
	}




	/**
	 * Imposta l'indice di rehosting,
	 * che misura la complessità. difficoltà
	 * di rehosting, nel livello corrente 
	 * di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxReHostingLow the idxReHostingLow to set
	 */
	public void setIdxReHostingLow(double idxReHostingLow) {
		this.idxReHostingLow = idxReHostingLow;
	}
	 
		
	


	/**
	 * Restituisce l'indice di McCabe,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxMcCabeHigh
	 */
	public double getIdxMcCabeHigh() {
		return idxMcCabeHigh;
	}



	/**
	 * Imposta l'indice di McCabe,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxMcCabeHigh the idxMcCabeHigh to set
	 */
	public void setIdxMcCabeHigh(double idxMcCabeHigh) {
		this.idxMcCabeHigh = idxMcCabeHigh;
	}





	/**
	 * Restituisce l'indice di manutenibilità MI,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxMIHigh
	 */
	public double getIdxMIHigh() {
		return idxMIHigh;
	}



	/**
	 * Imposta l'indice di manutenibilità MI,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxMIHigh the idxMIHigh to set
	 */
	public void setIdxMIHigh(double idxMIHigh) {
		this.idxMIHigh = idxMIHigh;
	}



	/**
	 * Restituisce il numero di Function Point,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxFPHigh
	 */
	public double getIdxFPHigh() {
		return idxFPHigh;
	}



	/**
	 * Imposta il numero di Function Point,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxFPHigh the idxFPHigh to set
	 */
	public void setIdxFPHigh(double idxFPHigh) {
		this.idxFPHigh = idxFPHigh;
	}



	/**
	 * Restituisce l'indice di rehosting,
	 * che misura la complessità. difficoltà
	 * di rehosting, nel livello corrente 
	 * di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxReHostingHigh
	 */
	public double getIdxReHostingHigh() {
		return idxReHostingHigh;
	}




	/**
	 * Imposta l'indice di rehosting,
	 * che misura la complessità. difficoltà
	 * di rehosting, nel livello corrente 
	 * di aggregazione.<br>
	 * <p>
	 * 
	 * @param idxReHostingHigh the idxReHostingHigh to set
	 */
	public void setIdxReHostingHigh(double idxReHostingHigh) {
		this.idxReHostingHigh = idxReHostingHigh;
	}
	 
	



	/**
	 * Restituisce il rate commenti rispetto alle righe sorgente logiche, con istruzioni
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the percComByLogical
	 */
	public double getPercComByLogical() {
		return percComByLogical;
	}






	/**
	 * Restituisce l'indice di manuteninìbilita totale,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * @return the idxMITot
	 */
	public double getIdxMITot() {
		return idxMITot;
	}




	/**
	 * Imposta l'indice di manuteninìbilita totale,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param idxMITot the idxMITot to set
	 */
	public void setIdxMITot(double idxMITot) {
		this.idxMITot = idxMITot;
	}




	/**
	 * Restituisce il numero di FP totale,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * @return the idxFPTot
	 */
	public double getIdxFPTot() {
		return idxFPTot;
	}

  


	/**
	 * Imposta il numero di FP totale,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param idxFPTot the idxFPTot to set
	 */
	public void setIdxFPTot(double idxFPTot) {
		this.idxFPTot = idxFPTot;
	}




	/**
	 * Restituisce l'indice di McCabe totale,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * @return the idxMcCabeTot
	 */
	public double getIdxMcCabeTot() {
		return idxMcCabeTot;
	}




	/**
	 * Imposta l'indice di McCabe totale,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param idxMcCabeTot the idxMcCabeTot to set
	 */
	public void setIdxMcCabeTot(double idxMcCabeTot) {
		this.idxMcCabeTot = idxMcCabeTot;
	}




	/**
	 * Restituisce l'indice di Re-hosting totale,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * @return the idxReHostingTot
	 */
	public double getIdxReHostingTot() {
		return idxReHostingTot;
	}




	/**
	 * Imposta l'indice di Re-hosting totale,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * @param idxReHostingTot the idxReHostingTot to set
	 */
	public void setIdxReHostingTot(double idxReHostingTot) {
		this.idxReHostingTot = idxReHostingTot;
	}




	/**
	 * Imposta il rate commenti rispetto alle righe sorgente logiche, con istruzioni
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param percComByLogical the percComByLogical to set
	 */
	public void setPercComByLogical(double percComByLogical) {
		this.percComByLogical = percComByLogical;
	}



	/**
	 * Restituisce il rate commenti rispetto alle righe sorgente fisiche, con istruzioni
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxComByPhisical
	 */
	public double getPercComByPhisical() {
		return percComByPhisical;
	}



	/**
	 * Imposta il rate commenti rispetto alle righe sorgente fisiche, con istruzioni
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param percComByPhisical the percComByPhisical to set
	 */
	public void setPercComByPhisical(double percComByPhisical) {
		this.percComByPhisical = percComByPhisical;
	}



	/**
	 * Restituisce il rate commenti rispetto alle istruzioni del programma,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the percComByInstruction
	 */
	public double getPercComByInstruction() {
		return percComByInstruction;
	}



	/**
	 * Imposta il rate commenti rispetto alle istruzioni del programma,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param percComByInstruction the percComByInstruction to set
	 */
	public void setPercComByInstruction(double percComByInstruction) {
		this.percComByInstruction = percComByInstruction;
	}



	/**
	 * Restituisce il Rate righe a blank rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the idxBlankByPhisical
	 */
	public double getPercBlankByPhisical() {
		return percBlankByPhisical;
	}



	/**
	 * Imposta il Rate righe a blank rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param percBlankByPhisical the percBlankByPhisical to set
	 */
	public void setPercBlankByPhisical(double percBlankByPhisical) {
		this.percBlankByPhisical = percBlankByPhisical;
	}



	/**
	 * Restituisce il Rate righe a blank per istruzione,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @return the percBlankByInstruction
	 */
	public double getPercBlankByInstruction() {
		return percBlankByInstruction;
	}



	/**
	 * Imposta il Rate righe a blank per istruzione,
	 * nel livello corrente di aggregazione.<br>
	 * <p>
	 * 
	 * @param percBlankByInstruction the percBlankByInstruction to set
	 */
	public void setPercBlankByInstruction(double percBlankByInstruction) {
		this.percBlankByInstruction = percBlankByInstruction;
	}


    /**
     *    <h2>Calcolo Indici di documentazione</h2><br>
     *      <ul>
	 *         <li><b>RCOML</b></li><br>
	 *             Rate commenti rispetto alle righe sorgente logiche, con istruzioni.<br>
	 *             E' il rapporto fra le righe di commento, incluse le righe a blank, <br>
	 *             e quelle logiche sorgente, contenenti istruzioni.<br>
	 *             Fornisce un'idea immediata della documentazione media di ogni istruzione<br>
	 *             di programma ma non tiene conto di un uso eccessivo di righe a blank.<br>
	 *             <p>
	 *             Si calcola con la formula:<br>
	 *             <p>
	 *             <h4>RCOM = (#CLOC + #BLOC) / #LLOC</h4>
	 *             
	 *         <li><b>RCOMS</b></li><br>
	 *             Rate commenti rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank.<br>
	 *             E' il rapporto fra le righe di commento, incluse le righe a blank e quelle fisiche sorgente.<br>
	 *             Fornisce una indicazione di quanto sia commentato un sorgente in generale ma non tiene<br>
	 *             conto di un uso eccessivo di righe a blank.<br>
	 *             <p>
	 *             Si calcola con la formula:<br>
	 *             <p>
	 *             <h4>RCOMS = (#CLOC + #BLOC) / #SLOC</h4>
	 *             
	 *         <li><b>RCOMI</b></li><br>
	 *             Rate commenti per istruzione.<br>
	 *             E' il rapporto fra le righe di commento, incluse le righe a blank e il numero di istruzioni.<br>
	 *             Fornisce una indicazione delle righe di commento medie per istruzione<br>
	 *             <p>
	 *             Si calcola con la formula:<br>
	 *             <p>
	 *             <h4>RCOMS = (#CLOC + #BLOC) / #INST</h4>
	 *             
	 *         <li><b>RBLKS</b></li><br>
	 *             Rate righe a blank rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank.<br>
	 *             E' il rapporto fra le righe a blank e quelle fisiche sorgente.<br>
	 *             Fornisce un'idea immediata dell' uso eccessivo di righe a blank nel programma<br>
	 *             senza contributo documentativo.<br>
	 *             <p>
	 *             Si calcola con la formula:<br>
	 *             <p>
	 *             <h4>RCOM = #BLOC / #SLOC</h4>
	 *             
	 *         <li><b>RBLKI</b></li><br>
	 *             Rate righe a blank per istruzione.<br>
	 *             E' il rapporto fra le righe a blank e il numero di istruzioni.<br>
	 *             Fornisce un'idea immediata dell' uso eccessivo di righe a blank prima di ogni istruzione<br>
	 *             <p>
	 *             Si calcola con la formula:<br>
	 *             <p>
	 *             <h4>RBLKI = #BLOC / #INST</h4>
	 *             
	 *        </ul>
     * 
     */
	public void computeIdxDoc() {
		
		// Commenti per linee di codice logiche
		this.percComByLogical = 0;
		if (this.sizeLinesCodeLogical > 0) {
			this.percComByLogical = ((this.sizeLinesComment * 100d) / this.sizeLinesCodeLogical);
		}
        
		// Commenti per linee di codice fisiche
		this.percComByPhisical = 0;
		if (this.sizeLinesCodePhisical > 0) {
			this.percComByPhisical = ((this.sizeLinesComment * 100d) / this.sizeLinesCodePhisical);
		}
		
		// Commenti per istruzione
		this.percComByInstruction = 0;
		if (this.sizeInstr > 0) {
			this.percComByInstruction = ((this.sizeLinesComment * 100d) / this.sizeInstr);
		}
		
		// Linee blank su linee fisiche
		this.percBlankByPhisical = 0;
		if (this.sizeLinesCodePhisical > 0) {
			this.percBlankByPhisical = ((this.sizeLinesBlank * 100d)  / this.sizeLinesCodePhisical);

		}
		
		// Linee blank per istruzione
		this.percBlankByInstruction = 0;
		if (this.sizeInstr > 0) {
			this.percBlankByInstruction = ((this.sizeLinesBlank * 100d)  / this.sizeInstr);
		}
	
	}

	
	/**
	 *     <h2>Calcolo indice di complessità ciclomatica</h2><br>
	 *        McCabe misura il numero di cammini esecutivi indipendenti (o il numero di condizioni binarie)<br>
	 *        per il calcolo dell'indice di complessità ciclomatica di McCabe.<br>
	 *        Si utilizza la versione estesa che tiene conto della complessità delle condizioni.<br>
	 *        Quando un costrutto di controllo contiene un’espressione logica con OR e/o AND, <br>
	 *        la misura di Complessità Ciclomatica Estesa aumenta di uno per ciascun operatore 
	 *        logico impiegato nel costrutto <br>
	 *        Queste misure stimano la complessità, la testabilità e la comprensibilità del codice<br>
	 *        e sono un indicatore dello sforzo necessario per testare un programma.<br>
	 *        Si tratta di misure che stimano la fiducia che il codice sia privo di errori e sono<br>
	 *        indipendenti dal tipo di linguaggio.<br>
	 *        Si applicano solo a oggetti programma e necessitano che sia stato creato il grafo di programma.<br>
	 *        <ul>
	 *          <li><b>#MCBEE</b></li><br>
	 *              Numero archi di programma.
	 *          <li><b>#MCBEN</b></li><br>
	 *              Numero nodi di programma.
	 *          <li><b>#MCBEP</b></li><br>
	 *              Numero di sottografi sconnessi, in cobol sono section non referenziate.
	 *        </ul>
     * 
	 * Calcolo indice di McCabe esteso.<br>
	 * <p>
	 * Viene usata la fornula:<br>
	 * <p>
	 * 
	 * <tt>idxMcCabe = cntMcCabeArcs - cntMcCabeNodes + 2 * cntMcCabeGraphNotConn + mcCabeOperatorsOrAnd;</tt>
	 * 
	 */
	public void computeIdxMcCabe() {
		this.idxMcCabeAvg = (int) (mcCabeArcs - mcCabeNodes + 2 * mcCabeGraphConn + mcCabeOperatorsOrAnd);
	}

	/**
	 *     <h2>Calcolo indice di complessità ciclomatica con formula semplificata</h2><br>
	 *        McCabe misura il numero di cammini esecutivi indipendenti (o il numero di condizioni binarie)<br>
	 *        per il calcolo dell'indice di complessità ciclomatica di McCabe.<br>
	 *        Si utilizza la versione estesa che tiene conto della complessità delle condizioni.<br>
	 *        Quando un costrutto di controllo contiene un’espressione logica con OR e/o AND, <br>
	 *        la misura di Complessità Ciclomatica Estesa aumenta di uno per ciascun operatore 
	 *        logico impiegato nel costrutto <br>
	 *        Queste misure stimano la complessità, la testabilità e la comprensibilità del codice<br>
	 *        e sono un indicatore dello sforzo necessario per testare un programma.<br>
	 *        Si tratta di misure che stimano la fiducia che il codice sia privo di errori e sono<br>
	 *        indipendenti dal tipo di linguaggio.<br>
	 *        Si applicano solo a oggetti programma e necessitano che sia stato creato il grafo di programma.<br>
	 *        <ul>
	 *          <li><b>#MCBEE</b></li><br>
	 *              Numero archi di programma.
	 *          <li><b>#MCBEN</b></li><br>
	 *              Numero nodi di programma.
	 *          <li><b>#MCBEP</b></li><br>
	 *              Numero di sottografi sconnessi, in cobol sono section non referenziate.
	 *        </ul>
     * 
	 * Calcolo indice di McCabe esteso.<br>
	 * <p>
	 * Viene usata la fornula:<br>
	 * <p>
	 * 
	 * <tt>idxMcCabe = cntMcCabeArcs - cntMcCabeNodes + 2 * cntMcCabeGraphNotConn + mcCabeOperatorsOrAnd;</tt>
	 * 
	 */
	public void computeIdxMcCabeSemplified(int cntPredicate, int cntOperatorExtended) {
		this.idxMcCabeAvg = cntPredicate + cntOperatorExtended + 1;
	}



	/**
	 * Calcolo indice di manutenibilità.<br>
	 * <p>
	 * 
	 */

	public void computeIdxMI() {
		
		// Programma/Section/Label vuoto
		if (this.halsteadVolumePgm == 0 || this.sizeLinesCodeLogical == 0) {
			this.idxMIAvg = 0;
			return;
		}
		
		// Calcolo possibile
		this.idxMIAvg = 171 
		              - 5.2*Math.log(this.halsteadVolumePgm) 
		              - 0.23*this.idxMcCabeAvg 
		              - 16.2*Math.log(this.sizeLinesCodeLogical)
		              + 50*Math.sin(Math.sqrt(2.4*this.percComByLogical));
		
	}



	/**
	 * Calcolo indice di function point.<br>
	 * <p>
	 *     <h2>Misure di complessità funzionale per function point </h2><br>
	 *        Sono misure specifiche attraverso le quali vengono calcolati i function point (FP)<br>
	 *        a livello di singolo oggetto, sottosistema e sistema.<br>
	 *        Per interno si intende un oggetto definito dentro un sistema/sottosistema,<br>
	 *        per esterno un oggetto definito in un diverso sistema/sottosistema.<br>
	 *        Le metriche di function point (FP) prevedono 5 misurazioni:
	 *      <p>
	 *        <ul>
	 *          <li><b>#EO</b></li><br>
	 *              <tt>External Output</tt><br> 
	 *              Si tratta di funzionalità utente (transazione o job) con output generati 
	 *              leggendo da un ILF o EIF.<br>
	 *          <li><b>#EI</b></li><br>
	 *              <tt>External Input</tt><br>
	 *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
	 *          <li><b>#EQ</b></li><br>
	 *              <tt>External Inquiry</tt><br>
	 *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
	 *          <li><b>#ILF</b></li><br>
	 *              <tt>Internal Logical Files</tt><br>
	 *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
	 *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
	 *              in insert, update e delete.<br>
	 *          <li><b>#EIF</b></li><br>
	 *              <tt>External Interface Files</tt><br>
	 *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
	 *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
	 *              ma accedute in read, insert, update e delete.<br>
	 *        </ul>
	 */

	public void computeIdxFP() {
		// TODO Auto-generated method stub
		
	}


	/**
	 * Calcolo indice di rehosting.<br>
	 * <p>
	 * 
	 */

	public void computeIdxRH() {
		// TODO Auto-generated method stub
		
	}



	/**
	 *     <h2>Misure di complessità di Halstead (o Software Science)</h2><br>
	 *        Secondo Halstead un programma si compone di un numero finito di token, <br>
	 *        classificati in <tt>operatori</tt> e <tt>operandi</tt> necessari al calcolo <br>
	 *        dell'indice di complessità di Halstead.<br>
	 *        La complessità di un programma, ed il suo tempo di sviluppo e manutenzione,<br>
	 *        è proporzionale al tipo e numero di operatori e operandi che contiene<br>
	 *        Halstead conteggia operatori e operandi che vanno intesi in una accezione specifica.<br>
	 *        Operatore è un simbolo o parola chiave che specifica un'azione, come una istruzione.<br>
	 *        Operando è un simbolo che rappresenta dati.<br>
	 *        Questa metrica è controversa per la difficoltà di distinguere fra operatori e operandi<br>
	 *        ma molto usata.<br>
	 *        Altra obbiezione è che le metriche di Halstead misurano la complessità lessicale<br>
	 *        e testuale più che la complessità strutturale o logica.<br>
	 *        <ul>
	 *          <li><b>#HLSTn1</b></li><br>
	 *              Numero operatori distinti in un programma.<br>
	 *          <li><b>#HLSTn2</b></li><br>
	 *              Numero operandi distinti in un programma
	 *          <li><b>#HLSTN1</b></li><br>
	 *              Numero occorrenze di operatori.<br>
	 *          <li><b>#HLSTN2</b></li><br>
	 *               Numero occorrenze di operandi.<br>
	 *          <li><b>#HLSTL</b></li><br>
	 *              <b>Lunghezza</b> programma che si calcola formalmente come:<br>
	 *              <h4>L = N1 + N2<br></h4>
	 *              <p>
	 *              Nel caso concreto:<br>
	 *              <p>
	 *              <h4>#HLSTL = #HLSTN1 + #HLSTN2<br><br></h4>
	 *          <li><b>HLSTn</b></li><br>
	 *              <b>Vocabolario</b> programma che si calcola formalmente come:<br>
	 *              <h4>n = n1 + n2<br></h4>
	 *              <p>
	 *              Nel caso concreto:<br>
	 *              <p>
	 *              <h4>#HLSTn = #HLSTn1 + HLSTn2<br><br></h4>
	 *         <li><b>#HLSTV</b></li><br>
	 *              <b>Volume</b> programma che si calcola formalmente come:<br>
	 *              <h4>V = N * (log2 n)<br></h4>
	 *              <p>
	 *              Nel caso concreto:<br>
	 *              <p>
	 *              <h4>#HLSTV = #HLSTL * (log2 #HLSTn)<br></h4>
	 *          <li><b>#HLSTD</b></li><br>
	 *              <b>Difficoltà</b> programma che si calcola formalmente come:<br>
	 *              <h4>D = (n1/2) * (N2/n2))<br></h4>
	 *              <p>
	 *              Nel caso concreto:<br>
	 *              <p>
	 *              <h4>#HLSTD = (#HLSTn1/2) * (#HLSTN2/#HLSTn2))<br></h4>
	 *          <li><b>HLSTS</b></li><br>
	 *              <b>Sforzo</b> programma che correla difficoltà e volume programma e che<br>
	 *              si calcola formalmente come:<br>
	 *              <h4>E = D * V<br></h4>
	 *              <p>
	 *              Nel caso concreto:<br>
	 *              <p>
	 *              <h4>#HLSTS = #HLSTD * #HLSTV<br></h4>
	 *        </ul>
	 *        
	 * <h2>Tabella riassuntiva misurazioni di Halstead</h2><br>
	 * <p>
	 * Si parte dal numero di operatori e operandi distinti n1 e n2
	 * e da numero di occorrenze di operatori e operandi N1 e N2
	 * In particolare si calcola, per il programma:<br>
	 * <p>
	 * <b>Lunghezza</b> <tt><b>N = N1 + N2 </b></tt><br>
	 * <b>Vocabolario</b> <tt><b>n = n1 + n2 	</b></tt><br>
	 * <b>Difficoltà</b> <tt><b>D = (n1/2) * (N2/n2)</b></tt><br>
	 * <b>Volume</b>  <tt><b>V = N * log2(n)</b></tt><br>
	 * <b>Sforzo</b> <tt><b>E = D * V </b></tt><br>
	 * <b>Tempo di scrittura</b> <tt><b>T = E / 18 </b></tt><br>
	 * 
	 * 
	 * 
	 *
	 */
	public void computeIdxHalstead() {
		
		// Probabile programma/section/paragrafo vuoti
		if (this.halsteadOperands == 0) {
		   	this.halsteadVocabularyPgm = 0;
		   	this.halsteadLengthPgm = 0;
		   	this.halsteadDifficultPgm = 0;
		   	this.halsteadVolumePgm = 0;
		   	this.halsteadEffortPgm = 0;
		   	this.halsteadTimeWriting = 0;
		   	return;
		}
		
		// Calcolo normale
	   	this.halsteadVocabularyPgm = this.halsteadOperators + this.halsteadOperands;             // n = n1 + n2 			Vocabolario programma
	   	this.halsteadLengthPgm = this.halsteadOperatorsOcc + this.halsteadOperandsOcc;           // N = N1 + N2 			Lunghezza programma
	   	this.halsteadDifficultPgm = (this.halsteadOperators / 2)             					 // D = (n1/2) * (N2/n2) 	Difficoltà programma
			                      * (this.halsteadOperandsOcc / this.halsteadOperands);  
	   	this.halsteadVolumePgm = this.halsteadLengthPgm * Math.log(this.halsteadVocabularyPgm);  // V = N * log2(n)		    Volume programma
	   	this.halsteadEffortPgm = this.halsteadDifficultPgm * this.halsteadVolumePgm;             // E = D * V   			Sforzo programma
	   	this.halsteadTimeWriting = (long) (this.halsteadEffortPgm / 18);             			 // T = E * S   			Tempo di scrittura in secondi
	}

 	
	/* ---------------------------------------------------------
	 * Restituisce il numero di moduli chiamanti  questo modulo
	 * ---------------------------------------------------------
	 * 
	 */
	public long evaluatePgmMetricFanIn() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countFanIn = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura EntityObject
		strSql = "SELECT COUNT(*) FROM  (SELECT R.idObjectA  FROM Relation AS R WHERE"; 
		strSql = strSql + getSqlScopeCondition(false, "relation");  
		strSql = strSql +               " AND (R.relation =  " + EnumRelation.PGM_CALLED_PGM.ordinal();
		strSql = strSql +               "   OR R.relation =  " + EnumRelation.PGM_XCTL_PGM.ordinal();
		strSql = strSql +               "   OR R.relation =  " + EnumRelation.PGM_CALLED_PGM.ordinal();
		strSql = strSql +               "   OR R.relation =  " + EnumRelation.PGM_CANCEL_PGM.ordinal();
		strSql = strSql +               "      ) ";
		strSql = strSql +               " GROUP BY R.idObjectA";
		strSql = strSql +             " )";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		if (rs.next()) {
			countFanIn = rs.getInt(1);
		}
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countFanIn;
	}
    
		

	
	
	/* ---------------------------------------------------------
	 * Restituisce il numero di moduli chiamati da questo modulo
	 * ---------------------------------------------------------
	 * 
	 */
	public long evaluatePgmMetricFanOut() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countFanOut = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);
		
		
		// Composizione Select di lettura EntityObject
		strSql = "SELECT COUNT(*) FROM  (SELECT R.idObjectB  FROM Relation AS R WHERE"; 
		strSql = strSql + getSqlScopeCondition(true, "Relation"); 
		strSql = strSql +               " AND ( R.relation =  " + EnumRelation.PGM_CALLED_PGM.ordinal();
		strSql = strSql +               "   OR  R.relation =  " + EnumRelation.PGM_XCTL_PGM.ordinal();
		strSql = strSql +               "   OR  R.relation =  " + EnumRelation.PGM_CALLED_PGM.ordinal();
		strSql = strSql +               "   OR  R.relation =  " + EnumRelation.PGM_CANCEL_PGM.ordinal();
		strSql = strSql +               "      ) ";
		strSql = strSql +               " GROUP BY  R.idObjectB";
		strSql = strSql +             " )";

		
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		if (rs.next()) {
		   countFanOut = rs.getLong(1);
		}
		
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return countFanOut;   
	}


	/**
	 * Restituisce il numero di relazioni del programma con gli altri oggetti<br>
	 * 
	 * 
	 */

	public long evaluateFuncRelations() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countRela = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);
        
        // Composizione Select di lettura EntityObject
		strSql = "SELECT COUNT(*)  FROM Relation AS R WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "relation"); 
	
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countRela = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countRela;
	}

	/**
	 * Restituisce il numero di oggetti generati o utilizzati dal programma.<br>
	 * <p>
	 * Si tratta di tutti gli oggetti relazionati.
	 *  
	 * 
	 */
	public long evaluateFuncObjects() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countObjects = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura EntityObject
		strSql = "SELECT COUNT(*)"; 
		strSql = strSql +   "  FROM  (SELECT  R.idObjectB  FROM Relation AS R WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "relation"); 
		strSql = strSql +     " GROUP BY  R.idObjectB";
		strSql = strSql +  " )";
	
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countObjects = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countObjects;
	}

	/**
	 *  
	 * Restituisce il numero di transazioni richiamate dal programma, definite all'interno<br>
	 * del sistema e al sottosistema.<br>
	 * <p>
	 * Si tratta di transazioni richiamate con:<br>
	 * <p>
	 *  Cics Return Transid
	 *  Cics Start
	 *  
	 * 
	 */
	public long evaluateFuncTranInternal() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countTranInternal = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql =          "SELECT COUNT(*)"; 
		strSql = strSql +   "  FROM  (SELECT R.idObjectB AS idObjectB "; 
		strSql = strSql +            " FROM Relation AS R,  Object  AS O  WHERE  ( ";
		strSql = strSql + getSqlScopeCondition(true, "Relation"); 
		strSql = strSql +            "          AND (R.relation =  " + EnumRelation.PGM_CICS_RETURN_TRANSID.ordinal() + " OR ";
		strSql = strSql +            "               R.relation =  " + EnumRelation.PGM_CICS_START_TRANSID.ordinal();
		strSql = strSql +            "              ) ";
		strSql = strSql +            "          AND  O.sysOwner = '" + this.system + "'";
		strSql = strSql +            "          AND  O.subSysOwner = '" + this.subSystem + "'";
		strSql = strSql +            "          AND  O.typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		strSql = strSql +            "          AND  O.idObject = R.idObjectB";
		strSql = strSql +            "         )";
		strSql = strSql +            "  GROUP BY R.idObjectB";
		strSql = strSql +          "                                    )";
	
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countTranInternal = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return countTranInternal;
	}

	/**
	 * Rrestituisce il numero di transazioni richiamate dal programma, definite all'esterno<br>
	 * del sistema e al sottosistema.<br>
	 * <p>
	 * Si tratta di transazioni richiamate con:<br>
	 * <p>
	 *  Cics Return Transid<br>
	 *  Cics Start<br>
	 * -----------------------------------------------------------
	 */ 
	public long evaluateFuncTranExternal() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countTranExternal = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql =          "SELECT COUNT(*)"; 
		strSql = strSql +   "  FROM  (SELECT R.idObjectB "; 
		strSql = strSql +            " FROM Relation AS R, Object AS O WHERE  (";
		strSql = strSql + getSqlScopeCondition(true, "relation"); 
		strSql = strSql +            "          AND (R.relation =  " + EnumRelation.PGM_CICS_RETURN_TRANSID.ordinal() + " OR ";
		strSql = strSql +            "               R.relation =  " + EnumRelation.PGM_CICS_START_TRANSID.ordinal();
		strSql = strSql +            "              ) ";
		strSql = strSql +            "          AND (O.sysOwner <> '" + this.system + "'  OR ";
		strSql = strSql +            "               O.subSysOwner <> '" + this.subSystem + "'";
		strSql = strSql +            "              ) ";
		strSql = strSql +            "          AND  O.typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		strSql = strSql +            "          AND  O.idObject = R.idObjectB";
		strSql = strSql +            "                                 )";
		strSql = strSql +            "  GROUP BY R.idObjectB";
		strSql = strSql +          " )";
		
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);
 
		rs.next();
		countTranExternal = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return countTranExternal;
	}


	/**
	 * Restituisce il numero di mappe video utilizzate dal programma.<br>
	 * <p> 
	 * Si tratta di mappe oggetto di <tt>Exec Cics Send Map</tt> e  <tt>Exec Cics Receive Map</tt><br>
	 * <p>
	 * 
	 */
	public long evaluateFuncMap() throws SQLException, ExceptionAmrita {
		ResultSet rs = null;
		long countMaps = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura EntityObject
		strSql =          "SELECT COUNT(*)"; 
		strSql = strSql +   "  FROM   (SELECT R.idObjectB FROM Relation AS R  WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "relation"); 
		strSql = strSql +   "               AND  R.relation =  " + EnumRelation.PGM_CICS_MAP.ordinal();
		strSql = strSql +   "               AND  R.typeObjectB =  " + EnumObject.OBJECT_CICS_MAP.ordinal();
		strSql = strSql +   "            GROUP BY R.idObjectB";
		strSql = strSql +   "         )";
	
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countMaps = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return countMaps;
	}

	/* ---------------------------------------------------------
	 * Restituisce il numero di Call/Link/XcTl a moduli interni,
	 * ovvero appartenenti allo stesso sistema/sottosistema
	 * applicativo.
	 * ---------------------------------------------------------
	 */
	public long evaluateFuncCallInternal() throws SQLException, ExceptionAmrita {

		ResultSet rs = null;
		long countCallInternal = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura EntityObject
		strSql = "SELECT COUNT(*) FROM RelationOrigin AS R, Object AS O WHERE ";
		strSql = strSql + getSqlScopeCondition(true, "RelationOrigin"); 
		strSql = strSql +     " AND (R.relation = " + EnumRelation.PGM_CALLED_PGM.ordinal();
		strSql = strSql +     "   OR R.relation = " + EnumRelation.PGM_XCTL_PGM.ordinal();
		strSql = strSql +     "   OR R.relation = " + EnumRelation.PGM_CALLED_PGM.ordinal();
		strSql = strSql +     "   OR R.relation = " + EnumRelation.PGM_CANCEL_PGM.ordinal();
		strSql = strSql +     "      ) ";
		strSql = strSql +     " AND  O.sysOwner = '" + this.system + "'";
		strSql = strSql +     " AND  O.subSysOwner = '" + this.subSystem + "'";
		strSql = strSql +     " AND  O.typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		strSql = strSql +     " AND  O.idObject = R.idObjectA";
	
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countCallInternal = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countCallInternal;
	}

	/* ---------------------------------------------------------
	 * Restituisce il numero di Call/Link/XcTl a moduli esterni,
	 * ovvero appartenenti a un diverso sistema/sottosistema
	 * applicativo.
	 * ---------------------------------------------------------
	 */
	public long evaluateFuncCallExternal() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countCallExternal = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql = "SELECT COUNT(*) FROMRelationOrigin AS R, Object AS O WHERE"; 
		strSql = strSql + getSqlScopeCondition(true, "RelationOrigin"); 
		strSql = strSql +     " AND (R.relation = " + EnumRelation.PGM_CALLED_PGM.ordinal();
		strSql = strSql +     "   OR R.relation = " + EnumRelation.PGM_XCTL_PGM.ordinal();
		strSql = strSql +     "   OR R.relation = " + EnumRelation.PGM_CALLED_PGM.ordinal();
		strSql = strSql +     "   OR R.relation = " + EnumRelation.PGM_CANCEL_PGM.ordinal();
		strSql = strSql +     "      ) ";
		strSql = strSql +     " AND  (O.sysOwner <> '" + this.system+ "'  OR ";
		strSql = strSql +     "       O.subSysOwner <> '" + this.subSystem + "'";
		strSql = strSql +     "      ) ";
		strSql = strSql +     " AND  O.typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		strSql = strSql +     " AND  O.idObject = R.idObjectA";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countCallExternal = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countCallExternal;
	}


	/* ---------------------------------------------------------
	 * Numero accessi a entity Db2, DL1,etc. interne al sistema
	 * e al sottosistema
	 * ---------------------------------------------------------
	 * 
	 */
	public long evaluateFuncAccEntityInternal() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countAccEntityInternal = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql = "SELECT COUNT(*) FROMRelationOrigin AS R, Object AS O  WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "RelationOrigin"); 
		strSql = strSql +     " AND (R.relation =  " + EnumRelation.PGM_ENTITY_READ.ordinal()     + " OR ";
		strSql = strSql +     "      R.relation =  " + EnumRelation.PGM_ENTITY_READNEXT.ordinal() + " OR ";
		strSql = strSql +     "      R.relation =  " + EnumRelation.PGM_ENTITY_READPREV.ordinal() + " OR ";
		strSql = strSql +     "      R.relation =  " + EnumRelation.PGM_ENTITY_UPDATE.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation =  " + EnumRelation.PGM_ENTITY_DELETE.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation =  " + EnumRelation.PGM_ENTITY_INSERT.ordinal();
		strSql = strSql +     "     ) ";
		strSql = strSql +     " AND  O.sys = '" + this.system + "'";
		strSql = strSql +     " AND  O.subSys = '" + this.subSystem + "'";
		strSql = strSql +     " AND  O.typeObject = R.typeObjectB";
		strSql = strSql +     " AND  O.idObject = R.idObjectB";
		strSql = strSql +     " AND  O.sysOwner = '" + this.di.systemInput + "'";
		strSql = strSql +     " AND  O.subSysOwner = '" + this.di.subSystemInput + "'";
	
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countAccEntityInternal = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return countAccEntityInternal;
	}

	
	

	/*
	 *  -----------------------------------------------------------
	 * Numero accessi a entity Db2, DL1,etc. definite all'esterno
	 * del sistema e al sottosistema
	 * -----------------------------------------------------------
	 * 
	 */
	public long evaluateFuncAccEntityExternal() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countAccEntityExternal = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql = "SELECT COUNT(*) FROMRelationOrigin AS R, Object AS O  WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "RelationOrigin"); 
		strSql = strSql +     " AND (R.relation =  " + EnumRelation.PGM_ENTITY_READ.ordinal()     + " OR ";
		strSql = strSql +     "      R.relation =  " + EnumRelation.PGM_ENTITY_READNEXT.ordinal() + " OR ";
		strSql = strSql +     "      R.relation =  " + EnumRelation.PGM_ENTITY_READPREV.ordinal() + " OR ";
		strSql = strSql +     "      R.relation =  " + EnumRelation.PGM_ENTITY_UPDATE.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation =  " + EnumRelation.PGM_ENTITY_DELETE.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation =  " + EnumRelation.PGM_ENTITY_INSERT.ordinal();
		strSql = strSql +     "     ) ";
		strSql = strSql +     " AND  O.sys = '" + this.system + "'";
		strSql = strSql +     " AND  O.subSys = '" + this.subSystem + "'";
		strSql = strSql +     " AND  O.typeObject = R.typeObjectB";
		strSql = strSql +     " AND  O.idObject = R.idObjectB";
		strSql = strSql +     " AND (O.sysOwner <> '" + this.di.systemInput + "'  OR ";
		strSql = strSql +     "      O.subSysOwner <> '" + this.di.subSystemInput + "'";
		strSql = strSql +     "     ) ";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countAccEntityExternal = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return countAccEntityExternal;
	}

	
	/*
	 * -----------------------------------------------------------
	 * Numero accessi a dati non su database definite all'interno
	 * del sistema e al sottosistema.
	 * Si tratta di files Vsam, Sequenziali, Code Cics TS/TD/MQ
	 * -----------------------------------------------------------
	 * 
	 */
	public long evaluateFuncAccMediaInternal() throws SQLException, ExceptionAmrita {
		ResultSet rs = null;
		long countAccMediaInternal = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql = "SELECT COUNT(*) FROMRelationOrigin AS R, Object AS O  WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "RelationOrigin"); 
		strSql = strSql +     " AND (R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_READ.ordinal()     + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_READNEXT.ordinal() + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_READPREV.ordinal() + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_UPDATE.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_DELETE.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_INSERT.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_SORT.ordinal()     + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_MERGE.ordinal()    + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TS_QUEUE_READ.ordinal()     + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TS_QUEUE_WRITE.ordinal()    + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TS_QUEUE_REWRITE.ordinal()  + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TS_QUEUE_DELETE.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TD_QUEUE_READ.ordinal()     + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TD_QUEUE_WRITE.ordinal()    + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TD_QUEUE_DELETE.ordinal();
		strSql = strSql +     "     ) ";
		strSql = strSql +     " AND  O.sys = '" + this.system + "'";
		strSql = strSql +     " AND  O.subSys = '" + this.subSystem + "'";
		strSql = strSql +     " AND  O.typeObject = R.typeObjectB";
		strSql = strSql +     " AND  O.idObject = R.idObjectB";
		strSql = strSql +     " AND  O.sysOwner = '" + this.di.systemInput + "'";
		strSql = strSql +     " AND  O.subSysOwner = '" + this.di.subSystemInput + "'";
		
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countAccMediaInternal = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countAccMediaInternal;
	}
	
    /*
	 * -----------------------------------------------------------
	 * Numero accessi a dati non su database definite all'esterno
	 * del sistema e al sottosistema.
	 * Si tratta di files Vsam, Sequenziali, Code Cics TS/TD/MQ
	 * -----------------------------------------------------------
	 */
	public long evaluateFuncAccMediaExternal() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countAccMediaExternal = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql = "SELECT COUNT(*) FROMRelationOriginAS R, Object AS O  WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "RelationOrigin"); 
		strSql = strSql +     " AND (R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_READ.ordinal()     + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_READNEXT.ordinal() + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_READPREV.ordinal() + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_UPDATE.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_DELETE.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_INSERT.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_SORT.ordinal()     + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_EXTERNAL_FILE_MERGE.ordinal()    + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TS_QUEUE_READ.ordinal()     + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TS_QUEUE_WRITE.ordinal()    + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TS_QUEUE_REWRITE.ordinal()  + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TS_QUEUE_DELETE.ordinal()   + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TD_QUEUE_READ.ordinal()     + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TD_QUEUE_WRITE.ordinal()    + " OR ";
		strSql = strSql +     "      R.relation = " + EnumRelation.PGM_CICS_TD_QUEUE_DELETE.ordinal();
		strSql = strSql +     "     ) ";
		strSql = strSql +     " AND  O.sys = '" + this.system+ "'";
		strSql = strSql +     " AND  O.subSys = '" + this.subSystem + "'";
		strSql = strSql +     " AND  O.typeObject = R.typeObjectB";
		strSql = strSql +     " AND  O.idObject = R.idObjectB";
		strSql = strSql +     " AND (O.sysOwner <> '" + this.system + "'  OR ";
		strSql = strSql +     "      O.subSysOwner <> '" + this.subSystem + "'";
		strSql = strSql +     "     ) ";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countAccMediaExternal = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);		
		eoDAO.setConn(null);
		return countAccMediaExternal;
	}

     /**
	 * <tt>Calcolo function point ExternalOutput EO<br>
	 * <p>
	 * Si tratta del numero di funzionalità utente (transazione o job),
	 * con output generati da un ILF o EIF<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
     */
	public long evaluateFpExternalOutputEO() throws SQLException, ExceptionAmrita {

		ResultSet rs = null;
		long countFpExternalOutputEO = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql = "SELECT COUNT(*) FROM (SELECT O.idObject FROM Object AS O  WHERE "; 
		strSql = strSql +     "     O.typeObject = " + EnumObject.OBJECT_CICS_TRANSID.ordinal();
		strSql = strSql + getSqlScopeConditionObject(" ");
		strSql = strSql +   "   AND  EXISTS (SELECT * FROM Relation AS R ";
		strSql = strSql +   "                   WHERE   O.sys = R.sys";
		strSql = strSql +	"                      AND  O.subSys = R.subSys";
		strSql = strSql +   "                      AND  O.idObject = R.idObjectA";
	    strSql = strSql +   "                      AND (R.relation =  " + EnumRelation.TRANSID_ENTITY.ordinal()        + " OR ";
		strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_PHISICAL_FILE.ordinal() + " OR ";
		strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_EXTERNAL_FILE.ordinal() ;
		strSql = strSql +   "                          ) ";
		strSql = strSql +   "               ) ";
		strSql = strSql +   "          ) ";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countFpExternalOutputEO = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countFpExternalOutputEO;
	}	
	
	/**
	 * <tt>Calcolo function point External Input EI<br>
	 * <p>
	 * Si tratta del numero di funzionalità utente (transazione o job),
	 * caratterizzati da update di dati (add, change, dele, uppdate) 
	 * di un ILF<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @return the fpExternalInputEI
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 */
	public long evaluateFpExternalInputEI() throws SQLException, ExceptionAmrita {

		ResultSet rs = null;
		long countFpExternalInputEI = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql = "SELECT COUNT(*) FROM (SELECT O.idObject FROM Object AS O  "; 
		strSql = strSql +   "    WHERE     O.typeObject = " + EnumObject.OBJECT_CICS_TRANSID.ordinal();
		strSql = strSql + getSqlScopeConditionObject(" ");
		strSql = strSql +   "      AND  EXISTS (SELECT * FROM Relation AS R, Object AS O2";
		strSql = strSql +   "                   WHERE   O2.sys = O.sys";
		strSql = strSql +	"                      AND  O2.subSys = O.subSys";
		strSql = strSql +   "                      AND  O2.idObject = O.idObject";
		strSql = strSql +   "                      AND  O2.sysOwner = '" + this.di.systemInput + "'";
		strSql = strSql +   "                      AND  O2.subSysOwner = '" + this.di.subSystemInput + "'";
	    strSql = strSql +   "                      AND (R.relation =  " + EnumRelation.TRANSID_ENTITY.ordinal()               + " OR ";
	    strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_ENTITY_DELETE.ordinal()        + " OR ";
	    strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_ENTITY_INSERT.ordinal()        + " OR ";
	    strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_ENTITY_UPDATE.ordinal()        + " OR ";
		strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_PHISICAL_FILE_DELETE.ordinal() + " OR ";
		strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_PHISICAL_FILE_INSERT.ordinal() + " OR ";
		strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_PHISICAL_FILE_UPDATE.ordinal() + " OR ";
		strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_EXTERNAL_FILE_DELETE.ordinal() + " OR ";
		strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_EXTERNAL_FILE_INSERT.ordinal() + " OR ";
		strSql = strSql +   "                           R.relation =  " + EnumRelation.TRANSID_EXTERNAL_FILE_UPDATE.ordinal() ;
		strSql = strSql +   "                          ) ";
		strSql = strSql +   "               ) ";
		strSql = strSql +   "          ) ";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countFpExternalInputEI = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countFpExternalInputEI;
	}

	/**
	 * <tt>Calcolo function point External Inquiry EQ<br>
	 * <p>
	 * Si tratta del numero di funzionalità utente (transazione o job),
	 * caratterizzati da sole read di dati di un ILF o un EIF.<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @return the fpExternalInquiryEQ
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 */
	public long evaluateFpExternalInquiryEQ() throws SQLException, ExceptionAmrita {


		ResultSet rs = null;
		long countFpExternalInputEI = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql = "SELECT COUNT(*) FROM (SELECT O.idObject FROM Object AS O  "; 
		strSql = strSql +   "    WHERE     O.typeObject = " + EnumObject.OBJECT_CICS_TRANSID.ordinal();
		strSql = strSql + getSqlScopeConditionObject(" ");
		strSql = strSql +   "      AND  EXISTS (SELECT * FROM Relation AS R1, Object AS O1";
		strSql = strSql +   "                   WHERE   O1.sys = O.sys";
		strSql = strSql +	"                      AND  O1.subSys = O.subSys";
		strSql = strSql +   "                      AND  O1.idObject = O.idObject";
		strSql = strSql +   "                      AND  R1.idObjectA  = O.idObject";
		strSql = strSql +   "                      AND  R1.typeObjectA  = O.typeObject";
	    strSql = strSql +   "                      AND (R1.relation =  " + EnumRelation.TRANSID_ENTITY_READ.ordinal()        + " OR ";
		strSql = strSql +   "                           R1.relation =  " + EnumRelation.TRANSID_PHISICAL_FILE_READ.ordinal() + " OR ";
		strSql = strSql +   "                           R1.relation =  " + EnumRelation.TRANSID_EXTERNAL_FILE_READ.ordinal() ;
		strSql = strSql +   "                          ) ";
		strSql = strSql +   "                 ) ";
		strSql = strSql +   "      AND  NOT EXISTS (SELECT * FROM Relation AS R2, Object AS O2";
		strSql = strSql +   "                   WHERE   O2.sys = O.sys";
		strSql = strSql +	"                      AND  O2.subSys = O.subSys";
		strSql = strSql +   "                      AND  O2.idObject = O.idObject";
		strSql = strSql +   "                      AND  R2.idObjectA = O.idObject";
		strSql = strSql +   "                      AND  R2.typeObjectA = O.typeObject";
	    strSql = strSql +   "                      AND (R2.relation =  " + EnumRelation.TRANSID_ENTITY_DELETE.ordinal()        + " OR ";
	    strSql = strSql +   "                           R2.relation =  " + EnumRelation.TRANSID_ENTITY_INSERT.ordinal()        + " OR ";
	    strSql = strSql +   "                           R2.relation =  " + EnumRelation.TRANSID_ENTITY_UPDATE.ordinal()        + " OR ";
		strSql = strSql +   "                           R2.relation =  " + EnumRelation.TRANSID_PHISICAL_FILE_DELETE.ordinal() + " OR ";
		strSql = strSql +   "                           R2.relation =  " + EnumRelation.TRANSID_PHISICAL_FILE_INSERT.ordinal() + " OR ";
		strSql = strSql +   "                           R2.relation =  " + EnumRelation.TRANSID_PHISICAL_FILE_UPDATE.ordinal() + " OR ";
		strSql = strSql +   "                           R2.relation =  " + EnumRelation.TRANSID_EXTERNAL_FILE_DELETE.ordinal() + " OR ";
		strSql = strSql +   "                           R2.relation =  " + EnumRelation.TRANSID_EXTERNAL_FILE_INSERT.ordinal() + " OR ";
		strSql = strSql +   "                           R2.relation =  " + EnumRelation.TRANSID_EXTERNAL_FILE_UPDATE.ordinal() ;
		strSql = strSql +   "                          ) ";
		strSql = strSql +   "               ) ";
		strSql = strSql +   "          ) ";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countFpExternalInputEI = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return countFpExternalInputEI;
	}

	/**
	 * <tt>Calcolo function point Internal Logical File ILF<br>
	 * <p>
	 * Si tratta del numero di tabelle e/o files residenti all'interno
	 * del confine dell'applicazione e gestite dallo stesso, ovvero
	 * nell'ambito del sistema/sottosistema proprietario.<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @return the fpInternalLogicalFileILF
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 */
	@SuppressWarnings("unused")
	public long evaluateFpInternalLogicalFileILF() throws SQLException, ExceptionAmrita {

		ResultSet rs = null;
		long FpInternalLogicalFileILF = 0;
        String strSql = "";

        // TODO
		
	return FpInternalLogicalFileILF;
	}

	/**
	 * <tt>Calcolo function point External Interface File EIF<br>
	 * <p>
	 * Si tratta del numero di tabelle e/o files residenti all'esterno
	 * del confine dell'applicazione e gestite da altre applicazioni,
	 * ovvero da un sistema/sottosistema diverso.<br>
	 * <p>
	 * <b>Definizioni</b>
	 *        <ul>
     *          <li><b>#EO</b></li><br>
     *              <tt>External Output</tt><br> 
     *              Si tratta di funzionalità utente (transazione o job) con output generati 
     *              leggendo da un ILF o EIF.<br>
     *          <li><b>#EI</b></li><br>
     *              <tt>External Input</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
     *          <li><b>#EQ</b></li><br>
     *              <tt>External Inquiry</tt><br>
     *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
     *          <li><b>#ILF</b></li><br>
     *              <tt>Internal Logical Files</tt><br>
     *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
     *              in insert, update e delete.<br>
     *          <li><b>#EIF</b></li><br>
     *              <tt>External Interface Files</tt><br>
     *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
     *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
     *              ma accedute in read, insert, update e delete.<br>
     *        </ul>
     * <p>
	 * @return the fpExternalInterfaceFileEIF
	 */
	public long evaluateFpExternalInterfaceFileEIF() {
		// TODO Auto-generated method stub
		return 0;
	}


	/**
	 * Rehosting.<br>
	 * Restituisce il numero di oggetti relazionati 
	 * interni al sistema/sottosistema.
	 * 
	 */
	public long evaluateRhObjectsInternal() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countRhObjectsInternal = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql = "   SELECT COUNT(*) FROM "; 
		strSql = strSql +     " (SELECT  R.idObjectB FROM Relation AS R, Object AS O  WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "relation"); 
		strSql = strSql +     "     AND  O.sys = '" + this.subSystem + "'";
		strSql = strSql +     "     AND  O.subSys = '" + this.subSystem + "'";
		strSql = strSql +     "     AND  O.typeObject = R.typeObjectB ";
		strSql = strSql +     "     AND  O.idObject = R.idObjectB";
		strSql = strSql +     "     AND  O.sysOwner = '" + this.di.systemInput + "'";
		strSql = strSql +     "     AND  O.subSysOwner = '" + this.di.subSystemInput + "'";
		strSql = strSql +     "   GROUP BY R.idObjectB ";
		strSql = strSql +     " ) ";
	
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countRhObjectsInternal = rs.getInt(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return countRhObjectsInternal;
	}


	/**
	 * Rehosting.<br>
	 * Restituisce il numero di oggetti relazionati dal
	 * programma esterni al sistem/sottosistema.
	 * 
	 */
	public long evaluateRhObjectsExternal() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countRhObjectsExternal = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura  
		strSql = "   SELECT COUNT(*)  "; 
		strSql = strSql + "FROM (SELECT  R.idObjectB FROM Relation AS R, Object AS O  WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "relation"); 
		strSql = strSql +     "     AND  O.sys = '" + this.system + "'";
		strSql = strSql +     "     AND  O.subSys = '" + this.subSystem + "'";
		strSql = strSql +     "     AND  O.typeObject = R.typeObjectB ";
		strSql = strSql +     "     AND  O.idObject = R.idObjectB";
		strSql = strSql +     "     AND (O.sysOwner <> '" + this.di.systemInput + "'  OR ";
		strSql = strSql +     "          O.subSysOwner <> '" + this.di.subSystemInput + "'";
		strSql = strSql +     "         ) ";
		strSql = strSql +     "   GROUP BY R.idObjectB ";
		strSql = strSql +     " ) ";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countRhObjectsExternal = rs.getInt(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countRhObjectsExternal;
	}

	/**
	 * Rehosting.<br>
	 * Restituisce il il rapporto fra il numero di oggetti 
	 * interni/esterni al sistem/sottosistema e le relazioni
	 * fra gli stessi.
	 * 
	 */
	public double evaluateRhRateObjectsRelation() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countRhObjects = 0;
		long countRhRelations = 0;
		double countRhRateObjectsRelation = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura relazioni 
		strSql = "SELECT COUNT(*) FROM Relation AS R  WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "relation"); 
	
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countRhRelations = rs.getLong(1);
		rs.close();
	
		
		// Composizione Select di lettura oggetti 
		strSql = "SELECT COUNT(*) FROM  ("; 
		strSql = strSql + "SELECT R.idObjectB FROM Relation AS R  WHERE"; 
		strSql = strSql + getSqlScopeCondition(true, "relation"); 
		strSql = strSql +     "   GROUP BY R.idObjectB ";
		strSql = strSql + "UNION ";
		strSql = strSql + "SELECT R.idObjectA FROM Relation AS R  WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "relation"); 
		strSql = strSql +     "   GROUP BY R.idObjectA ";
		strSql = strSql +     "        ) ";
	
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countRhObjects = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		countRhRateObjectsRelation = (double) countRhObjects / (double) countRhRelations;
		
		return countRhRateObjectsRelation;
	}
    
    /**
     * Rehosting.<br>
     * Restituisce il numero di oggetti non portabili come
     * programmi Assembler, Pl1, Load Module etc.
     * 
     */
	public long evaluateRhObjectsUnportable() throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		long countRhObjectsUnportable = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura relazioni 
		strSql = "SELECT COUNT(*) FROM Object AS O  WHERE ";
		strSql = strSql + getSqlScopeConditionObject(" AND "); 
		strSql = strSql +     "    (O.typeObject =  " + EnumObject.OBJECT_PGM_ASM.ordinal() + " OR ";
		strSql = strSql +     "     O.typeObject =  " + EnumObject.OBJECT_LOAD_MODULE.ordinal();
		strSql = strSql +     "    )";
	
		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countRhObjectsUnportable = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countRhObjectsUnportable;
	}

	/**
	 * Rehosting.<br>
	 * Restituisce il numero di tabelle/files utilizzati,
	 * ovvero presenti in almeno una relazione,
	 * contenenti campi binari, nel sistema/sottosistema.
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public long evaluateRhFilesBynary() throws SQLException, ExceptionAmrita {

		ResultSet rs = null;
		long countRhFilesBynary = 0;
        String strSql = "";

	    // Operazioni per accesso al databsae
	    Connection conn = DataBaseConnections.getConnection();
		IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

		// Composizione Select di lettura relazioni 
		strSql = "SELECT COUNT(*) FROM ( "; 
		strSql = strSql +   "SELECT  R.idObjectB FROM Relation AS R  WHERE "; 
		strSql = strSql + getSqlScopeCondition(true, "relation"); 
	    strSql = strSql +   "   AND (R.relation =  " + EnumRelation.PGM_EXTERNAL_FILE.ordinal() + " OR ";
		strSql = strSql +   "        R.relation =  " + EnumRelation.PGM_ENTITY.ordinal();
		strSql = strSql +   "       ) ";
		strSql = strSql +   "   AND  EXISTS (SELECT * FROM OBJO AS O ";
		strSql = strSql +                    " WHERE    O.OBJOSYST = R.sys";
		strSql = strSql +	                     " AND  O.OBJOSUBS = R.subSys";
		strSql = strSql +                        " AND  O.OBJOIDOB = R.idObjectA";
		strSql = strSql +                        " AND (O.OBJOOPTO = " + EnumObjectOption.ENTITY_WITH_BINARY_FIELDS.ordinal() + " OR ";
		strSql = strSql +                        "      O.OBJOOPTO = " + EnumObjectOption.PHISICAL_FILE_WITH_BINARY_FIELDS.ordinal();
		strSql = strSql +                        "     ) ";
		strSql = strSql +        "          ) ";
		strSql = strSql +        "     ) ";

		// Esecuzione query e produzione  ResultSet
		rs = eoDAO.execSqlGeneric(strSql);

		rs.next();
		countRhFilesBynary = rs.getLong(1);
		rs.close();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return countRhFilesBynary;
	}


	
    /**
	 *     <h2>Misure di complessità strutturale</h2><br>
	 *     <p>
	 *        Sono misure che indicano il grado di accoppiamento fra moduli.<br>
	 *        I moduli con grande fan-in di solito sono piccoli e appartengono ai<br>
	 *        livelli inferiori di un’architettura sw a livelli. I moduli grossi e <br>
	 *        complessi hanno di solito fan-in piccolo. I moduli con grande fan-out <br>
	 *        dipendono da parecchi moduli e alzano il livello di accoppiamento.<br>
	 *        <tt>fan-in</tt> piccolo e <tt>fan-out</tt> grande indica cattiva qualità di progettazione.
	 *        <ul>
	 *          <li><b>#FANIN</b></li><br>
	 *             Numero programmi chiamanti con <tt>Call, Cics Link, Cics Xctl</tt> etc.<br>
	 *          <li><b>#FANOUT</b></li><br>
	 *             Numero programmi chiamati con <tt>Call, Cics Link, Cics Xctl</tt> etc.<br>
	 *        </ul>
	 *        
     * @throws ExceptionAmrita 
     * @throws SQLException 
     */
	public void evaluateMeasuresComplexityStructure() throws SQLException, ExceptionAmrita {		
		this.structFanIn = evaluatePgmMetricFanIn();		 
		this.structFanOut = evaluatePgmMetricFanOut();       
	}



	/**
	 *     <h2>Misure di complessità funzionale generiche</h2><br>
	 *        Sono misure generali attraverso le quali viene valutata la complessità funzionale a livello<br>
	 *        di singolo oggetto, sottosistema e sistema.<br>
	 *        Per interno si intende un oggetto definito dentro un sistema/sottosistema,<br>
	 *        per esterno un oggetto definito in un diverso sistema/sottosistema.<br>
	 *        <p>
	 *        <ul>
	 *          <li><b>#FPOBJ</b></li><br>
	 *              Numero oggetti.
	 *          <li><b>#FPRELA</b></li><br>
	 *              Numero relazioni fra oggetti.
	 *          <li><b>#FPTRANI</b></li><br>
	 *              Numero transazioni interne richiamate con Exec Cics Start o Exec Cics Return Transid.
	 *          <li><b>#FPTRANE</b></li><br>
	 *              Numero transazioni esterne richiamate con Exec Cics Start o Exec Cics Return Transid.
	 *          <li><b>#FPMAP</b></li><br>
	 *              Numero mappe video utilizzate.
	 *          <li><b>#FPCALLI</b></li><br>
	 *              Numero call a moduli interni.
	 *          <li><b>#FPCALLE</b></li><br>
	 *              Numero call a moduli esterni.
	 *          <li><b>#FPENTITYI</b></li><br>
	 *              Numero accessi a entity (tabelle db) interni.
	 *          <li><b>#FPENTITYE</b></li><br>
	 *              Numero accessi a entity (tabelle db) esterni.
	 *          <li><b>#FPDATAI</b></li><br>
	 *              Numero accessi a files sequenziali/Vsam/code ts/.. interni.
	 *          <li><b>#FDATAE</b></li><br>
	 *              Numero accessi a files sequenziali/Vsam/code ts/.. esterni.
	 *        </ul>
     * <p>
     * @throws ExceptionAmrita 
     * @throws SQLException 
     */
	public void evaluateMeasuresComplexityFunction() throws SQLException, ExceptionAmrita {
		this.funcObjects = evaluateFuncObjects();    					              
		this.funcRelations = evaluateFuncRelations();            		       
		this.funcTranInternal = evaluateFuncTranInternal();  		                      
		this.funcTranExternal = evaluateFuncTranExternal();                     
		this.funcMap = evaluateFuncMap();                                                       
		this.funcCallInternal = evaluateFuncCallInternal();                           
		this.funcCallExternal = evaluateFuncCallExternal();               
		this.funcAccEntityInternal = evaluateFuncAccEntityInternal();            
		this.funcAccEntityExternal = evaluateFuncAccEntityExternal();                
		this.funcAccMediaInternal = evaluateFuncAccMediaInternal();                   
		this.funcAccMediaExternal = evaluateFuncAccMediaExternal();               
	}


	/**
	 *     <h2>Misure di complessità funzionale per function point </h2><br>
	 *     
	 *        Sono misure specifiche attraverso le quali vengono calcolati i function point (FP)<br>
	 *        a livello di singolo oggetto, sottosistema e sistema.<br>
	 *        Per interno si intende un oggetto definito dentro un sistema/sottosistema,<br>
	 *        per esterno un oggetto definito in un diverso sistema/sottosistema.<br>
	 *        Le metriche di function point (FP) prevedono 5 misurazioni:
	 *      <p>
	 *        <ul>
	 *          <li><b>#EO</b></li><br>
	 *              <tt>External Output</tt><br> 
	 *              Si tratta di funzionalità utente (transazione o job) con output generati 
	 *              leggendo da un ILF o EIF.<br>
	 *          <li><b>#EI</b></li><br>
	 *              <tt>External Input</tt><br>
	 *              Si tratta di funzionalità utente (transazione o job) con add, change,delete di un ILF<br>
	 *          <li><b>#EQ</b></li><br>
	 *              <tt>External Inquiry</tt><br>
	 *              Si tratta di funzionalità utente (transazione o job) di sole read da ILF o EIF<br>
	 *          <li><b>#ILF</b></li><br>
	 *              <tt>Internal Logical Files</tt><br>
	 *              Si tratta di tabelle/files residenti dentro il confine del sistema<br>
	 *              software, ovvero del sistema/sottosistema, e gestite dallo stesso<br>
	 *              in insert, update e delete.<br>
	 *          <li><b>#EIF</b></li><br>
	 *              <tt>External Interface Files</tt><br>
	 *              Si tratta di tabelle/files residenti all'esterno del confine del sistema<br>
	 *              software, ovvero del sistema/sottosistema, gestite da altre applicazioni,<br>
	 *              ma accedute in read, insert, update e delete.<br>
	 *        </ul>
	 *  
	 * <p>
	 * @throws ExceptionAmrita 
	 * @throws SQLException 
     */
	public void evaluateMeasuresFunctionPoint() throws SQLException, ExceptionAmrita {
		this.fpExternalOutputEO = evaluateFpExternalOutputEO();                
		this.fpExternalInputEI = evaluateFpExternalInputEI();                    
		this.fpExternalInquiryEQ = evaluateFpExternalInquiryEQ();                
		this.fpInternalLogicalFileILF = evaluateFpInternalLogicalFileILF();          
		this.fpExternalInterfaceFileEIF = evaluateFpExternalInterfaceFileEIF();         
	}

	/**
	 *     <h2>Misure di complessità funzionale per rehosting </h2><br>
	 *     
	 *        Sono misure specifiche attraverso le quali si vuole valutare lo sforzo<br>
	 *        e la complessita di rehosting di una applicazione, a livello di sottosistema e sistema.<br>
	 *        Per perimetro si intende il sistema/sottosistema in considerazione.<br>
	 *        Per oggetto perimetrato si intende un oggetto definito dentro il sistema/sottosistema <br>
	 *        in considerazione.<br>
	 *      <p>
	 *        <ul>
	 *          <li><b>#RHRATE</b></li><br>
	 *              Rapporto tra il numero di oggetti e numero di relazioni
	 *          <li><b>#RHORL</b></li><br>
	 *              Numero di oggetti interni al perimetro
	 *          <li><b>#RHEXT</b></li><br>
	 *              Numero di oggetti esterni al perimetro
	 *          <li><b>#RHUNP</b></li><br>
	 *              Numero di oggetti non portabili ((Assembler, PL/I, Load Module, ecc...)
	 *          <li><b>#RHBYN</b></li><br>
	 *              Numero di files/tabelle contenenti campi binari
	 *        </ul>
	 * @throws ExceptionAmrita 
	 * @throws SQLException 
     */
	public void evaluateMeasuresRehosting() throws SQLException, ExceptionAmrita {
		this.rhObjectsInternal = evaluateRhObjectsInternal();              
		this.rhObjectsExternal  = evaluateRhObjectsExternal();                
		this.rhRateObjectRelation = evaluateRhRateObjectsRelation();             
		this.rhObjectsUnportable = evaluateRhObjectsUnportable();                
		this.rhFilesBynary = evaluateRhFilesBynary();                                          
	}
 
	/**
	 * Popolamento oggetto {@link EntityMetric} per inserimento o
	 * aggiornamento su database, con i dati correnti.
     */
	public void dbPopulateMetric(EntityMetricValue entityMetric) {

		// Valori chiave
		entityMetric.setSystem(this.getSystem());
		entityMetric.setSubSystem(this.getSubSystem());
		entityMetric.setScope(this.getScope());
		entityMetric.setIdObject(this.getIdObject());
		entityMetric.setTypeObject(this.getTypeObject());
		entityMetric.setSection(this.getSection());
		
 		// Misure di conteggio sorgenti
		entityMetric.setCntPgmAnalyzed(this.getCntPgmAnalyzed());                     
		entityMetric.setCntPgmExecuted(this.getCntPgmExecuted());                     
		entityMetric.setCntSubPgmAnalyzed(this.getCntSubPgmAnalyzed());                  
		entityMetric.setCntSubPgmExecuted(this.getCntSubPgmExecuted());                    
		entityMetric.setCntCopyDefined(this.getCntCopyDefined());                       
		entityMetric.setCntJclJob(this.getCntJclJob());                           
		entityMetric.setCntJclInclude(this.getCntJclInclude());                       
		entityMetric.setCntJclProc(this.getCntJclProc());                         
		
		// Misure dimensionali sorgenti
		entityMetric.setSizeLinesCodeLogical(this.getSizeLinesCodeLogical());                
		entityMetric.setSizeLinesCodePhisical(this.getSizeLinesCodePhisical());                
		entityMetric.setSizeLinesBlank(this.getSizeLinesBlank());                       
		entityMetric.setSizeLinesBlankProc(this.getSizeLinesBlankProc());                   
		entityMetric.setSizeLinesBlankData(this.getSizeLinesBlankData());                  
		entityMetric.setSizeLinesComment(this.getSizeLinesComment());                     
		entityMetric.setSizeLinesCommentProc(this.getSizeLinesCommentProc());                 
		entityMetric.setSizeLinesCommentData(this.getSizeLinesCommentData());                 
		entityMetric.setSizeInstr(this.getSizeInstr());                           

		// Misure stimate
		entityMetric.setBackFiredFunctionPoint(this.getBackFiredFunctionPoint());                      
		entityMetric.setTimeDevelopment(this.getTimeDevelopment());                
		
		// Misure definizione dati
		entityMetric.setDefFields(this.getDefFields());                           
		entityMetric.setDefFieldsInCopy(this.getDefFieldsInCopy());                     
		entityMetric.setDefLiterals(this.getDefLiterals());                          

		// Misure di documentazione
		entityMetric.setPercComByLogical(this.getPercComByLogical());
		entityMetric.setPercComByPhisical(this.getPercComByPhisical());
		entityMetric.setPercComByInstruction(this.getPercComByInstruction());
		entityMetric.setPercBlankByPhisical(this.getPercBlankByPhisical());
		entityMetric.setPercBlankByInstruction(this.getPercBlankByInstruction());

		// Misure di codice dinamico
		entityMetric.setDynamicInstr(this.getDynamicInstr());
		entityMetric.setDynamicInstrLight(this.getDynamicInstrLight());
		entityMetric.setPercDynamicInstr(this.getPercDynamicInstr());
		entityMetric.setPercDynamicInstrLight(this.getPercDynamicInstrLight());

		// Misure violazioni
		entityMetric.setViolations(this.getViolations());
		entityMetric.setPercViolationsByLogical(this.getPercViolationsByLogical());
		entityMetric.setPercViolationsByPhisical(this.getPercViolationsByPhisical());
		entityMetric.setPercViolationsByInstruction(this.getPercViolationsByInstruction());

		// Misure di codice morto
		entityMetric.setDeadFields(this.getDeadFields());
		entityMetric.setDeadInstr(this.getDeadInstr());
		entityMetric.setDeadLabels(this.getDeadLabels());
		entityMetric.setDeadCopyData(this.getDeadCopyData());
		entityMetric.setDeadCopyProc(this.getDeadCopyProc());
		entityMetric.setDeadSubGraph(this.getDeadSubGraph());

		// Misure di jcl
		entityMetric.setJclDD(this.getJclDD());
		entityMetric.setJclStepDefined(this.getJclStepDefined());
		entityMetric.setJclStepUpdate(this.getJclStepUpdate());
		entityMetric.setJclDsname(this.getJclDsname());
		entityMetric.setJclDsnameReferenced(this.getJclDsnameReferenced());
		entityMetric.setJclDsnameUnReferenced(this.getJclDsnameUnReferenced());
		entityMetric.setJclIncludeCalled(this.getJclIncludeCalled());
		entityMetric.setJclProcCalled(this.getJclProcCalled());

		// Misure di complessità strutturale
		entityMetric.setStructFanIn(this.getStructFanIn());
		entityMetric.setStructFanOut(this.getStructFanOut());
		entityMetric.setStructSections(this.getStructSections());
		entityMetric.setStructParagraphs(this.getStructParagraphs());
		
		// Misure di complessità funzionale 
		entityMetric.setFuncObjects(this.getFuncObjects());
		entityMetric.setFuncRelations(this.getFuncRelations());
		entityMetric.setFuncTranInternal(this.getFuncTranInternal());
		entityMetric.setFuncTranExternal(this.getFuncTranExternal());
		entityMetric.setFuncMap(this.getFuncMap());
		entityMetric.setFuncCallInternal(this.getFuncCallInternal());
		entityMetric.setFuncCallExternal(this.getFuncCallExternal());
		entityMetric.setFuncAccEntityInternal(this.getFuncAccEntityInternal());
		entityMetric.setFuncAccEntityExternal(this.getFuncAccEntityExternal());
		entityMetric.setFuncAccMediaInternal(this.getFuncAccMediaInternal());
		entityMetric.setFuncAccMediaExternal(this.getFuncAccMediaExternal());

		// Misure di complessità ciclomatica
		entityMetric.setMcCabeArcs(this.getMcCabeArcs());
		entityMetric.setMcCabeNodes(this.getMcCabeNodes());
		entityMetric.setMcCabeOperatorsOrAnd(this.getMcCabeOperatorsOrAnd());
		entityMetric.setMcCabeGraphConn(this.getMcCabeGraphConn());

		// Misure di complessità di Halstead (o Software Science)
		entityMetric.setHalsteadOperators(this.getHalsteadOperators());
		entityMetric.setHalsteadOperands(this.getHalsteadOperands());
		entityMetric.setHalsteadOperatorsOcc(this.getHalsteadOperatorsOcc());
		entityMetric.setHalsteadOperandsOcc(this.getHalsteadOperandsOcc());
		entityMetric.setHalsteadLengthPgm(this.getHalsteadLengthPgm());
		entityMetric.setHalsteadVocabularyPgm(this.getHalsteadVocabularyPgm());
		entityMetric.setHalsteadVolumePgm(this.getHalsteadVolumePgm());
		entityMetric.setHalsteadDifficultPgm(this.getHalsteadDifficultPgm());
		entityMetric.setHalsteadEffortPgm(this.getHalsteadEffortPgm());
		entityMetric.setHalsteadTimeWriting(this.getHalsteadTimeWriting());
		
		// Indici di complessita/manutenibilità medi
		entityMetric.setIdxMcCabeAvg(this.getIdxMcCabeAvg());
		entityMetric.setIdxMIAvg(this.getIdxMIAvg());
		entityMetric.setIdxFPAvg(this.getIdxFPAvg());
		entityMetric.setIdxReHostingAvg(this.getIdxReHostingAvg());
		
		// Indici di complessita/manutenibilità massimi
		entityMetric.setIdxMcCabeHigh(this.getIdxMcCabeHigh());
		entityMetric.setIdxMIHigh(this.getIdxMIHigh());
		entityMetric.setIdxFPHigh(this.getIdxFPHigh());
		entityMetric.setIdxReHostingHigh(this.getIdxReHostingHigh());
	
		// Indici di complessita/manutenibilità minimi
		entityMetric.setIdxMcCabeLow(this.getIdxMcCabeLow());
		entityMetric.setIdxMILow(this.getIdxMILow());
		entityMetric.setIdxFPLow(this.getIdxFPLow());
		entityMetric.setIdxReHostingLow(this.getIdxReHostingLow());
	
		// Indici di complessita/manutenibilità totali
		entityMetric.setIdxMcCabeTot(this.getIdxMcCabeTot());
		entityMetric.setIdxMITot(this.getIdxMITot());
		entityMetric.setIdxFPTot(this.getIdxFPTot());
		entityMetric.setIdxReHostingTot(this.getIdxReHostingTot());

		// Sistema Squale, update campi entity
		dbPopulateMetricSquale(entityMetric);
	}
	
	/**
	 * Popolamento oggetto {@link EntityMetric} per inserimento o
	 * aggiornamento su database, con i soli dati correnti Squale.
     */
	public void dbPopulateMetricSquale(EntityMetricValue entityMetric) {

	    // Sistema Squale, numero violazioni per categoria gravità
		entityMetric.setSqualeViolationsBlocker(this.getSqualeViolationsBlocker());
		entityMetric.setSqualeViolationsCritical(this.getSqualeViolationsCritical());
		entityMetric.setSqualeViolationsMajor(this.getSqualeViolationsMajor());
		entityMetric.setSqualeViolationsMinor(this.getSqualeViolationsMinor());
		entityMetric.setSqualeViolationsInfo(this.getSqualeViolationsInfo());

		// Sistema di qualità SQUALE, valori generali
		entityMetric.setSqualeSSRI(this.getSqualeSSRI());
		entityMetric.setSqualeSSCI(this.getSqualeSSCI());
		entityMetric.setSqualeSSQI(this.getSqualeSSQI());

		// Sistema di qualità SQUALE, valori di dettaglio indice qualità assoluto SSQI, SQxI
		entityMetric.setSqualeSQTI(this.getSqualeSQTI());
		entityMetric.setSqualeSQRI(this.getSqualeSQRI());
		entityMetric.setSqualeSQCI(this.getSqualeSQCI());
		entityMetric.setSqualeSQEI(this.getSqualeSQEI());
		entityMetric.setSqualeSQSI(this.getSqualeSQSI());
		entityMetric.setSqualeSQMI(this.getSqualeSQMI());
		entityMetric.setSqualeSQPI(this.getSqualeSQPI());

		// Sistema di qualità SQUALE, valori di dettaglio indici consolidati SCTx
		entityMetric.setSqualeSCTI(this.getSqualeSCTI());
		entityMetric.setSqualeSCRI(this.getSqualeSCRI());
		entityMetric.setSqualeSCCI(this.getSqualeSCCI());
		entityMetric.setSqualeSCEI(this.getSqualeSCEI());
		entityMetric.setSqualeSCSI(this.getSqualeSCSI());
		entityMetric.setSqualeSCMI(this.getSqualeSCMI());
		entityMetric.setSqualeSCPI(this.getSqualeSCPI());

		// Sistema di qualità SQUALE, valori di dettaglio indici di densita SDxI
		entityMetric.setSqualeSDTI(this.getSqualeSDTI());
		entityMetric.setSqualeSDRI(this.getSqualeSDRI());
		entityMetric.setSqualeSDCI(this.getSqualeSDCI());
		entityMetric.setSqualeSDEI(this.getSqualeSDEI());
		entityMetric.setSqualeSDSI(this.getSqualeSDSI());
		entityMetric.setSqualeSDMI(this.getSqualeSDMI());
		entityMetric.setSqualeSDPI(this.getSqualeSDPI());

		// Sistema di qualità SQUALE, valori di dettaglio indici squale rating SRxI 
		entityMetric.setSqualeSRTI(this.getSqualeSRTI());
		entityMetric.setSqualeSRRI(this.getSqualeSRRI());
		entityMetric.setSqualeSRCI(this.getSqualeSRCI());
		entityMetric.setSqualeSREI(this.getSqualeSREI());
		entityMetric.setSqualeSRSI(this.getSqualeSRSI());
		entityMetric.setSqualeSRMI(this.getSqualeSRMI());
		entityMetric.setSqualeSRPI(this.getSqualeSRPI());

		// Sistema di qualità SQUALE, valori di dettaglio livelli squale rating SRxL 
		entityMetric.setSqualeSRTL(this.getSqualeSRTL());
		entityMetric.setSqualeSRRL(this.getSqualeSRRL());
		entityMetric.setSqualeSRCL(this.getSqualeSRCL());
		entityMetric.setSqualeSREL(this.getSqualeSREL());
		entityMetric.setSqualeSRSL(this.getSqualeSRSL());
		entityMetric.setSqualeSRML(this.getSqualeSRML());
		entityMetric.setSqualeSRPL(this.getSqualeSRPL());
	}

	/**
	 * Popolamento da oggetto {@link EntityMetric}, caricato da data base<br>
	 * per successive elaborazioni.
     */
	public void populateFromDbEntityMetric(EntityMetricValue entityMetric) {

		// Valori chiave
		this.setSystem(entityMetric.getSystem());
		this.setSubSystem(entityMetric.getSubSystem());
		this.setScope(entityMetric.getScope());
		this.setIdObject(entityMetric.getIdObject());
		this.setTypeObject(entityMetric.getTypeObject());
		this.setSection(entityMetric.getSection());
		
 		// Misure di conteggio sorgenti
		this.setCntPgmAnalyzed(entityMetric.getCntPgmAnalyzed());                     
		this.setCntPgmExecuted(entityMetric.getCntPgmExecuted());                     
		this.setCntSubPgmAnalyzed(entityMetric.getCntSubPgmAnalyzed());                  
		this.setCntSubPgmExecuted(entityMetric.getCntSubPgmExecuted());                    
		this.setCntCopyDefined(entityMetric.getCntCopyDefined());                       
		this.setCntJclJob(entityMetric.getCntJclJob());                           
		this.setCntJclInclude(entityMetric.getCntJclInclude());                       
		this.setCntJclProc(entityMetric.getCntJclProc());                         
		
		// Misure dimensionali sorgenti
		this.setSizeLinesCodeLogical(entityMetric.getSizeLinesCodeLogical());                
		this.setSizeLinesCodePhisical(entityMetric.getSizeLinesCodePhisical());                
		this.setSizeLinesBlank(entityMetric.getSizeLinesBlank());                       
		this.setSizeLinesBlankProc(entityMetric.getSizeLinesBlankProc());                   
		this.setSizeLinesBlankData(entityMetric.getSizeLinesBlankData());                  
		this.setSizeLinesComment(entityMetric.getSizeLinesComment());                     
		this.setSizeLinesCommentProc(entityMetric.getSizeLinesCommentProc());                 
		this.setSizeLinesCommentData(entityMetric.getSizeLinesCommentData());                 
		this.setSizeInstr(entityMetric.getSizeInstr());                           

		// Misure di documentazione
		this.setPercComByLogical(entityMetric.getPercComByLogical());
		this.setPercComByPhisical(entityMetric.getPercComByPhisical());
		this.setPercComByInstruction(entityMetric.getPercComByInstruction());
		this.setPercBlankByPhisical(entityMetric.getPercBlankByPhisical());
		this.setPercBlankByInstruction(entityMetric.getPercBlankByInstruction());

		// Misure di codice dinamico
		this.setDynamicInstr(entityMetric.getDynamicInstr());
		this.setDynamicInstrLight(entityMetric.getDynamicInstrLight());
		this.setPercDynamicInstr(entityMetric.getPercDynamicInstr());
		this.setPercDynamicInstrLight(entityMetric.getPercDynamicInstrLight());

		// Misure violazioni
		this.setViolations(entityMetric.getViolations());
		this.setPercViolationsByLogical(entityMetric.getPercViolationsByLogical());
		this.setPercViolationsByPhisical(entityMetric.getPercViolationsByPhisical());
		this.setPercViolationsByInstruction(entityMetric.getPercViolationsByInstruction());

		// Misure stimate
		this.setBackFiredFunctionPoint(entityMetric.getBackFiredFunctionPoint());
		this.setTimeDevelopment(entityMetric.getTimeDevelopment());
		
		// Misure definizione dati
		this.setDefFields(entityMetric.getDefFields());                           
		this.setDefFieldsInCopy(entityMetric.getDefFieldsInCopy());                     
		this.setDefLiterals(entityMetric.getDefLiterals());                          
		
		// Misure di codice morto
		this.setDeadFields(entityMetric.getDeadFields());
		this.setDeadInstr(entityMetric.getDeadInstr());
		this.setDeadLabels(entityMetric.getDeadLabels());
		this.setDeadCopyData(entityMetric.getDeadCopyData());
		this.setDeadCopyProc(entityMetric.getDeadCopyProc());
		this.setDeadSubGraph(entityMetric.getDeadSubGraph());

		// Misure di jcl
		this.setJclDD(entityMetric.getJclDD());
		this.setJclStepDefined(entityMetric.getJclStepDefined());
		this.setJclStepUpdate(entityMetric.getJclStepUpdate());
		this.setJclDsname(entityMetric.getJclDsname());
		this.setJclDsnameReferenced(entityMetric.getJclDsnameReferenced());
		this.setJclDsnameUnReferenced(entityMetric.getJclDsnameUnReferenced());
		this.setJclIncludeCalled(entityMetric.getJclIncludeCalled());
		this.setJclProcCalled(entityMetric.getJclProcCalled());

		// Misure di complessità strutturale
		this.setStructFanIn(entityMetric.getStructFanIn());
		this.setStructFanOut(entityMetric.getStructFanOut());
		this.setStructSections(entityMetric.getStructSections());
		this.setStructParagraphs(entityMetric.getStructParagraphs());
		
		// Misure di complessità funzionale 
		this.setFuncObjects(entityMetric.getFuncObjects());
		this.setFuncRelations(entityMetric.getFuncRelations());
		this.setFuncTranInternal(entityMetric.getFuncTranInternal());
		this.setFuncTranExternal(entityMetric.getFuncTranExternal());
		this.setFuncMap(entityMetric.getFuncMap());
		this.setFuncCallInternal(entityMetric.getFuncCallInternal());
		this.setFuncCallExternal(entityMetric.getFuncCallExternal());
		this.setFuncAccEntityInternal(entityMetric.getFuncAccEntityInternal());
		this.setFuncAccEntityExternal(entityMetric.getFuncAccEntityExternal());
		this.setFuncAccMediaInternal(entityMetric.getFuncAccMediaInternal());
		this.setFuncAccMediaExternal(entityMetric.getFuncAccMediaExternal());

		// Misure di complessità ciclomatica
		this.setMcCabeArcs(entityMetric.getMcCabeArcs());
		this.setMcCabeNodes(entityMetric.getMcCabeNodes());
		this.setMcCabeOperatorsOrAnd(entityMetric.getMcCabeOperatorsOrAnd());
		this.setMcCabeGraphConn(entityMetric.getMcCabeGraphConn());

		// Misure di complessità di Halstead (o Software Science)
		this.setHalsteadOperators(entityMetric.getHalsteadOperators());
		this.setHalsteadOperands(entityMetric.getHalsteadOperands());
		this.setHalsteadOperatorsOcc(entityMetric.getHalsteadOperatorsOcc());
		this.setHalsteadOperandsOcc(entityMetric.getHalsteadOperandsOcc());
		this.setHalsteadLengthPgm(entityMetric.getHalsteadLengthPgm());
		this.setHalsteadVocabularyPgm(entityMetric.getHalsteadVocabularyPgm());
		this.setHalsteadVolumePgm(entityMetric.getHalsteadVolumePgm());
		this.setHalsteadDifficultPgm(entityMetric.getHalsteadDifficultPgm());
		this.setHalsteadEffortPgm(entityMetric.getHalsteadEffortPgm());
		this.setHalsteadTimeWriting(entityMetric.getHalsteadTimeWriting());
		
		// Indici di complessita/manutenibilità medi
		this.setIdxMcCabeAvg(entityMetric.getIdxMcCabeAvg());
		this.setIdxMIAvg(entityMetric.getIdxMIAvg());
		this.setIdxFPAvg(entityMetric.getIdxFPAvg());
		this.setIdxReHostingAvg(entityMetric.getIdxReHostingAvg());
		
		// Indici di complessita/manutenibilità massimi
		this.setIdxMcCabeHigh(entityMetric.getIdxMcCabeHigh());
		this.setIdxMIHigh(entityMetric.getIdxMIHigh());
		this.setIdxFPHigh(entityMetric.getIdxFPHigh());
		this.setIdxReHostingHigh(entityMetric.getIdxReHostingHigh());
	
		// Indici di complessita/manutenibilità minimi
		this.setIdxMcCabeLow(entityMetric.getIdxMcCabeLow());
		this.setIdxMILow(entityMetric.getIdxMILow());
		this.setIdxFPLow(entityMetric.getIdxFPLow());
		this.setIdxReHostingLow(entityMetric.getIdxReHostingLow());
	
		// Indici di complessita/manutenibilità totali
		this.setIdxMcCabeTot(entityMetric.getIdxMcCabeTot());
		this.setIdxMITot(entityMetric.getIdxMITot());
		this.setIdxFPTot(entityMetric.getIdxFPLow());
		this.setIdxReHostingTot(entityMetric.getIdxReHostingTot());
		
		// Sistema di qualità Squale
		populateFromDbEntityMetricSquale(entityMetric);

	}

	/**
	 * Popolamento da oggetto {@link EntityMetric}, caricato da data base<br>
	 * per successive elaborazioni, per le sole informazioni del siustema Squale.
     */
	public void populateFromDbEntityMetricSquale(EntityMetricValue entityMetric) {

			
		// Sistema Squale, numero violazioni per categoria gravità
		this.setSqualeViolationsBlocker(entityMetric.getSqualeViolationsBlocker());
		this.setSqualeViolationsCritical(entityMetric.getSqualeViolationsCritical());
		this.setSqualeViolationsMajor(entityMetric.getSqualeViolationsMajor());
		this.setSqualeViolationsMinor(entityMetric.getSqualeViolationsMinor());
		this.setSqualeViolationsInfo(entityMetric.getSqualeViolationsInfo());

		// Sistema di qualità SQUALE, valori generali
		this.setSqualeSSRI(entityMetric.getSqualeSSRI());
		this.setSqualeSSCI(entityMetric.getSqualeSSCI());
		this.setSqualeSSQI(entityMetric.getSqualeSSQI());

		// Sistema di qualità SQUALE, valori di dettaglio indice qualità assoluto SSQI, SQxI
		this.setSqualeSQTI(entityMetric.getSqualeSQTI());
		this.setSqualeSQRI(entityMetric.getSqualeSQRI());
		this.setSqualeSQCI(entityMetric.getSqualeSQCI());
		this.setSqualeSQEI(entityMetric.getSqualeSQEI());
		this.setSqualeSQSI(entityMetric.getSqualeSQSI());
		this.setSqualeSQMI(entityMetric.getSqualeSQMI());
		this.setSqualeSQPI(entityMetric.getSqualeSQPI());

		// Sistema di qualità SQUALE, valori di dettaglio indici consolidati SCTx
		this.setSqualeSCTI(entityMetric.getSqualeSCTI());
		this.setSqualeSCRI(entityMetric.getSqualeSCRI());
		this.setSqualeSCCI(entityMetric.getSqualeSCCI());
		this.setSqualeSCEI(entityMetric.getSqualeSCEI());
		this.setSqualeSCSI(entityMetric.getSqualeSCSI());
		this.setSqualeSCMI(entityMetric.getSqualeSCMI());
		this.setSqualeSCPI(entityMetric.getSqualeSCPI());

		// Sistema di qualità SQUALE, valori di dettaglio indici di densita SDxI
		this.setSqualeSDTI(entityMetric.getSqualeSDTI());
		this.setSqualeSDRI(entityMetric.getSqualeSDRI());
		this.setSqualeSDCI(entityMetric.getSqualeSDCI());
		this.setSqualeSDEI(entityMetric.getSqualeSDEI());
		this.setSqualeSDSI(entityMetric.getSqualeSDSI());
		this.setSqualeSDMI(entityMetric.getSqualeSDMI());
		this.setSqualeSDPI(entityMetric.getSqualeSDPI());

		// Sistema di qualità SQUALE, valori di dettaglio indici squale rating SRxI 
		this.setSqualeSRTI(entityMetric.getSqualeSRTI());
		this.setSqualeSRRI(entityMetric.getSqualeSRRI());
		this.setSqualeSRCI(entityMetric.getSqualeSRCI());
		this.setSqualeSREI(entityMetric.getSqualeSREI());
		this.setSqualeSRSI(entityMetric.getSqualeSRSI());
		this.setSqualeSRMI(entityMetric.getSqualeSRMI());
		this.setSqualeSRPI(entityMetric.getSqualeSRPI());

		// Sistema di qualità SQUALE, valori di dettaglio livelli squale rating SRxL 
		this.setSqualeSRTL(entityMetric.getSqualeSRTL());
		this.setSqualeSRRL(entityMetric.getSqualeSRRL());
		this.setSqualeSRCL(entityMetric.getSqualeSRCL());
		this.setSqualeSREL(entityMetric.getSqualeSREL());
		this.setSqualeSRSL(entityMetric.getSqualeSRSL());
		this.setSqualeSRML(entityMetric.getSqualeSRML());
		this.setSqualeSRPL(entityMetric.getSqualeSRPL());

	}



 	/**
 	 * Restituisce il numero di violazioni bloccanti.<br>
 	 * <p>
	 * @return the squaleViolationsBlocker
	 */
	public long getSqualeViolationsBlocker() {
		return squaleViolationsBlocker;
	}

	/**
 	 * Imposta il numero di violazioni bloccanti.<br>
 	 * <p>
	 * @param squaleViolationsBlocker the squaleViolationsBlocker to set
	 */
	public void setSqualeViolationsBlocker(long squaleViolationsBlocker) {
		this.squaleViolationsBlocker = squaleViolationsBlocker;
	}

	/**
 	 * Restituisce il numero di violazioni critiche.<br>
 	 * <p>
	 * @return the squaleViolationsCritical
	 */
	public long getSqualeViolationsCritical() {
		return squaleViolationsCritical;
	}

	/**
	 * Imposta il numero di violazioni critiche.<br>
 	 * <p>
	 * @param squaleViolationsCritical the squaleViolationsCritical to set
	 */
	public void setSqualeViolationsCritical(long squaleViolationsCritical) {
		this.squaleViolationsCritical = squaleViolationsCritical;
	}

	/**
	 * Restituisce il numero di violazioni major.<br>
 	 * <p>
	 * @return the squaleViolationsMajor
	 */
	public long getSqualeViolationsMajor() {
		return squaleViolationsMajor;
	}

	/**
	 * Imposta il numero di violazioni major.<br>
 	 * <p>
	 * @param squaleViolationsMajor the squaleViolationsMajor to set
	 */
	public void setSqualeViolationsMajor(long squaleViolationsMajor) {
		this.squaleViolationsMajor = squaleViolationsMajor;
	}

	/**
	 * Restituisce il numero di violazioni minor.<br>
 	 * <p>
	 * @return the squaleViolationsMinor
	 */
	public long getSqualeViolationsMinor() {
		return squaleViolationsMinor;
	}

	/**
	 * Imposta il numero di violazioni minor.<br>
 	 * <p>
	 * @param squaleViolationsMinor the squaleViolationsMinor to set
	 */
	public void setSqualeViolationsMinor(long squaleViolationsMinor) {
		this.squaleViolationsMinor = squaleViolationsMinor;
	}

	/**
	 * Restituisce il numero di violazioni info.<br>
 	 * <p>
	 * @return the squaleViolationsInfo
	 */
	public long getSqualeViolationsInfo() {
		return squaleViolationsInfo;
	}

	/**
	 * Imposta il numero di violazioni info.<br>
 	 * <p>
	 * @param squaleViolationsInfo the squaleViolationsInfo to set
	 */
	public void setSqualeViolationsInfo(long squaleViolationsInfo) {
		this.squaleViolationsInfo = squaleViolationsInfo;
	}

	/**
 	 * Restituisce lo il livello generale di Squale rating  A-E.<br>
 	 * <p>
 	 * Si tratta del livello in cui si colloca il rapporto  (SQI / tempo stimato di sviluppo)<br>
 	 * <p>
	 * @return the squaleSSRL
	 */
	public EnumMetricsSqualeRating getSqualeSSRL() {
		return squaleSSRL;
	}

	/**
	 * Imposta lo il livello generale di Squale rating  A-E.<br>
 	 * <p>
 	 * Si tratta del livello in cui si colloca il rapporto  (SQI / tempo stimato di sviluppo)<br>
 	 * <p>
	 * @param squaleSSRL the squaleSSRL to set
	 */
	public void setSqualeSSRL(EnumMetricsSqualeRating squaleSSRL) {
		this.squaleSSRL = squaleSSRL;
	}

	/**
	 * Restituisce lo Squale rating (SSQI / tempo stimato di sviluppo).<br>
	 * <p>
	 * SSQI rappresenta la somma di tutti costi di remediation di tutte le caratteristiche.<br>
	 * <p>
	 * @return the squaleSSRI
	 */
	public double getSqualeSSRI() {
		return squaleSSRI;
	}

	/**
	 * Imposta lo Squale rating (SSQI / tempo stimato di sviluppo).<br>
	 * <p>
	 * SSQI è l'indice di qualità assoluto e rappresenta la somma di tutti <br>
	 * costi di remediation di tutte le caratteristiche.<br>
	 * <p>
	 * @param squaleSRI the squaleSRI to set
	 */
	public void setSqualeSSRI(double squaleSSRI) {
		this.squaleSSRI = squaleSSRI;
	}

	/**
	 * Restituisce lo Squale rule compliance.<br>
	 * <p>
	 * Indica quanto il codice sia conforme alle regole previste.<br>
	 * <p>
	 * Si utilizza la formula SCI = 100 - (pesoRegole/LOC * 100)<br>
	 * <p>
	 * Dove pesoRegole è dato da:<br>
	 * <p>
	 * NumViolazioni Blocker*10<br>
	 * +<br>
	 * NumViolazioni Critical*5<br>
	 * +<br>
	 * NumViolazioni Major*3<br>
	 * +<br>
	 * NumViolazioni Minor*1<br>
	 * +<br>
	 * NumViolazioni Info*1<br>
	 * +<br>
	 * 
	 * @return the squaleSCI
	 */
	public double getSqualeSSCI() {
		return squaleSSCI;
	}

	/**
	 * Imposta lo Squale rule compliance.<br>
	 * <p>
	 * Indica quanto il codice sia conforme alle regole previste.<br>
	 * <p>
	 * Si utilizza la formula SCI = 100 - (pesoRegole/LOC * 100)<br>
	 * <p>
	 * Dove pesoRegole è dato da:<br>
	 * <p>
	 * NumViolazioni Blocker*10<br>
	 * +<br>
	 * NumViolazioni Critical*5<br>
	 * +<br>
	 * NumViolazioni Major*3<br>
	 * +<br>
	 * NumViolazioni Minor*1<br>
	 * +<br>
	 * NumViolazioni Info*1<br>
	 * +<br>
	 * 
	 * @param squaleSSCI the squaleSSCI to set
	 */
	public void setSqualeSSCI(double squaleSSCI) {
		this.squaleSSCI = squaleSSCI;
	}

	/**
	 * Restituisce l'indice di qualità assoluto Squale.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation di tutte le caratteristiche.<br>
	 * <p>
	 * SSQI=SSTI+SSRI+SSCI+SSEI+SSSI+SSMI+SSPI<br>
	 * <p>
	 * @return the squaleSSQI
	 */
	public long getSqualeSSQI() {
		return squaleSSQI;
	}

	/**
	 * Imposta l'indice di qualità assoluto Squale.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation di tutte le caratteristiche.<br>
	 * <p>
	 * SSQI=SSTI+SSRI+SSCI+SSEI+SSSI+SSMI+SSPI<br>
	 * <p>
	 * @param squaleSSQI the squaleSSQI to set
	 */
	public void setSqualeSSQI(long squaleSSQI) {
		this.squaleSSQI = squaleSSQI;
	}

	/**
	 * Restituisce l'indice di qualità assoluto Squale per la caratteristica Testability.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @return the squaleSQTI
	 */
	public long getSqualeSQTI() {
		return squaleSQTI;
	}

	/**
	 * Imposta l'indice di qualità assoluto Squale per la caratteristica Testability.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @param squaleSQTI the squaleSQTI to set
	 */
	public void setSqualeSQTI(long squaleSQTI) {
		this.squaleSQTI = squaleSQTI;
	}

	/**
	 * Restituisce l'indice di qualità assoluto Squale per la caratteristica Reliability.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @return the squaleSQRI
	 */
	public long getSqualeSQRI() {
		return squaleSQRI;
	}

	/**
	 * Imposta l'indice di qualità assoluto Squale per la caratteristica Reliability.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @param squaleSQRI the squaleSQRI to set
	 */
	public void setSqualeSQRI(long squaleSQRI) {
		this.squaleSQRI = squaleSQRI;
	}

	/**
	 * Restituisce l'indice di qualità assoluto Squale per la caratteristica Changeability.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @return the squaleSQCI
	 */
	public long getSqualeSQCI() {
		return squaleSQCI;
	}

	/**
	 * Imposta l'indice di qualità assoluto Squale per la caratteristica Changeability.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @param squaleSQCI the squaleSQCI to set
	 */
	public void setSqualeSQCI(long squaleSQCI) {
		this.squaleSQCI = squaleSQCI;
	}

	/**
	 * Restituisce l'indice di qualità assoluto Squale per la caratteristica Efficiency.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @return the squaleSQEI
	 */
	public long getSqualeSQEI() {
		return squaleSQEI;
	}

	/**
	 * Imposta l'indice di qualità assoluto Squale per la caratteristica Efficiency.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @param squaleSQEI the squaleSQEI to set
	 */
	public void setSqualeSQEI(long squaleSQEI) {
		this.squaleSQEI = squaleSQEI;
	}

	/**
	 * Restituisce l'indice di qualità assoluto Squale per la caratteristica Security.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @return the squaleSQSI
	 */
	public long getSqualeSQSI() {
		return squaleSQSI;
	}

	/**
	 * Imposta l'indice di qualità assoluto Squale per la caratteristica Security.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @param squaleSQSI the squaleSQSI to set
	 */
	public void setSqualeSQSI(long squaleSQSI) {
		this.squaleSQSI = squaleSQSI;
	}

	/**
	 * Restituisce l'indice di qualità assoluto Squale per la caratteristica Maintenability.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @return the squaleSQMI
	 */
	public long getSqualeSQMI() {
		return squaleSQMI;
	}

	/**
	 * Imposta l'indice di qualità assoluto Squale per la caratteristica Maintenability.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @param squaleSQMI the squaleSQMI to set
	 */
	public void setSqualeSQMI(long squaleSQMI) {
		this.squaleSQMI = squaleSQMI;
	}

	/**
	 * Restituisce l'indice di qualità assoluto Squale per la caratteristica Portability.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @return the squaleSQPI
	 */
	public long getSqualeSQPI() {
		return squaleSQPI;
	}

	/**
	 * Imposta l'indice di qualità assoluto Squale per la caratteristica Portability.<br>
	 * <p>
	 * Si tratta della somma dei costi di remediation per tutte le regole <br>
	 * che violano questa caratteristica.<br>
	 * <p>
	 * @param squaleSQPI the squaleSQPI to set
	 */
	public void setSqualeSQPI(long squaleSQPI) {
		this.squaleSQPI = squaleSQPI;
	}

	/**
	 * Restituisce l'indice di qualità consolidato Squale per la caratteristica Testability.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @return the squaleSCTI
	 */
	public long getSqualeSCTI() {
		return squaleSCTI;
	}

	/**
	 * Imposta l'indice di qualità consolidato Squale per la caratteristica Testability.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @param squaleSCTI the squaleSCTI to set
	 */
	public void setSqualeSCTI(long squaleSCTI) {
		this.squaleSCTI = squaleSCTI;
	}

	/**
	 * Restituisce l'indice di qualità consolidato Squale per la caratteristica Reliability.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @return the squaleSCRI
	 */
	public long getSqualeSCRI() {
		return squaleSCRI;
	}

	/**
	 * Imposta l'indice di qualità consolidato Squale per la caratteristica Reliability.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @param squaleSCRI the squaleSCRI to set
	 */
	public void setSqualeSCRI(long squaleSCRI) {
		this.squaleSCRI = squaleSCRI;
	}

	/**
	 * Restituisce l'indice di qualità consolidato Squale per la caratteristica Changeability.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @return the squaleSCCI
	 */
	public long getSqualeSCCI() {
		return squaleSCCI;
	}

	/**
	 * Imposta l'indice di qualità consolidato Squale per la caratteristica Changeability.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @param squaleSCCI the squaleSCCI to set
	 */
	public void setSqualeSCCI(long squaleSCCI) {
		this.squaleSCCI = squaleSCCI;
	}

	/**
	 * Restituisce l'indice di qualità consolidato Squale per la caratteristica Efficiency.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @return the squaleSCEI
	 */
	public long getSqualeSCEI() {
		return squaleSCEI;
	}

	/**
	 * Imposta l'indice di qualità consolidato Squale per la caratteristica Efficiency.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @param squaleSCEI the squaleSCEI to set
	 */
	public void setSqualeSCEI(long squaleSCEI) {
		this.squaleSCEI = squaleSCEI;
	}

	/**
	 * Restituisce l'indice di qualità consolidato Squale per la caratteristica Security.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @return the squaleSCSI
	 */
	public long getSqualeSCSI() {
		return squaleSCSI;
	}

	/**
	 * Imposta l'indice di qualità consolidato Squale per la caratteristica Security.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @param squaleSCSI the squaleSCSI to set
	 */
	public void setSqualeSCSI(long squaleSCSI) {
		this.squaleSCSI = squaleSCSI;
	}

	/**
	 * Restituisce l'indice di qualità consolidato Squale per la caratteristica Maintenability.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @return the squaleSCMI
	 */
	public long getSqualeSCMI() {
		return squaleSCMI;
	}

	/**
	 * Imposta l'indice di qualità consolidato Squale per la caratteristica Maintenability.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @param squaleSCMI the squaleSCMI to set
	 */
	public void setSqualeSCMI(long squaleSCMI) {
		this.squaleSCMI = squaleSCMI;
	}

	/**
	 * Restituisce l'indice di qualità consolidato Squale per la caratteristica Portability.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @return the squaleSCPI
	 */
	public long getSqualeSCPI() {
		return squaleSCPI;
	}

	/**
	 * Imposta l'indice di qualità consolidato Squale per la caratteristica Portability.<br>
	 * <p>
	 * Si tratta della somma dei costi di questa caratteristica <br>
	 * e di tutte quelle precdenti nel modello piramidale Squale<br>
	 * <p>
	 * @param squaleSCPI the squaleSCPI to set
	 */
	public void setSqualeSCPI(long squaleSCPI) {
		this.squaleSCPI = squaleSCPI;
	}

	/**
	 * Restituisce l'indice di densità Squale per la caratteristica Testability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQTI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @return the squaleSDTI
	 */
	public double getSqualeSDTI() {
		return squaleSDTI;
	}

	/**
	 * Imposta l'indice di densità Squale per la caratteristica Testability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQTI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @param squaleSDTI the squaleSDTI to set
	 */
	public void setSqualeSDTI(double squaleSDTI) {
		this.squaleSDTI = squaleSDTI;
	}

	/**
	 * Restituisce l'indice di densità Squale per la caratteristica Reliability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQRI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @return the squaleSDRI
	 */
	public double getSqualeSDRI() {
		return squaleSDRI;
	}

	/**
	 * Imposta l'indice di densità Squale per la caratteristica Reliability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQRI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @param squaleSDRI the squaleSDRI to set
	 */
	public void setSqualeSDRI(double squaleSDRI) {
		this.squaleSDRI = squaleSDRI;
	}

	/**
	 * Restituisce l'indice di densità Squale per la caratteristica Changeability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQCI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @return the squaleSDCI
	 */
	public double getSqualeSDCI() {
		return squaleSDCI;
	}

	/**
	 * Imposta l'indice di densità Squale per la caratteristica Changeability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQCI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @param squaleSDCI the squaleSDCI to set
	 */
	public void setSqualeSDCI(double squaleSDCI) {
		this.squaleSDCI = squaleSDCI;
	}

	/**
	 * Restituisce l'indice di densità Squale per la caratteristica Efficiency.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQEI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @return the squaleSDEI
	 */
	public double getSqualeSDEI() {
		return squaleSDEI;
	}

	/**
	 * Imposta l'indice di densità Squale per la caratteristica Efficiency.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQEI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @param squaleSDEI the squaleSDEI to set
	 */
	public void setSqualeSDEI(double squaleSDEI) {
		this.squaleSDEI = squaleSDEI;
	}

	/**
	 * Restituisce l'indice di densità Squale per la caratteristica Security.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQSI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @return the squaleSDSI
	 */
	public double getSqualeSDSI() {
		return squaleSDSI;
	}

	/**
	 * Imposta l'indice di densità Squale per la caratteristica Security.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQSI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @param squaleSDSI the squaleSDSI to set
	 */
	public void setSqualeSDSI(double squaleSDSI) {
		this.squaleSDSI = squaleSDSI;
	}

	/**
	 * Restituisce l'indice di densità Squale per la caratteristica Maintenability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQMI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @return the squaleSDMI
	 */
	public double getSqualeSDMI() {
		return squaleSDMI;
	}

	/**
	 * Imposta l'indice di densità Squale per la caratteristica Maintenability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQMI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @param squaleSDMI the squaleSDMI to set
	 */
	public void setSqualeSDMI(double squaleSDMI) {
		this.squaleSDMI = squaleSDMI;
	}

	/**
	 * Restituisce l'indice di densità Squale per la caratteristica Portability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQPI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @return the squaleSDPI
	 */
	public double getSqualeSDPI() {
		return squaleSDPI;
	}

	/**
	 * Imposta l'indice di densità Squale per la caratteristica Portability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQPI / dimensioni <br>
	 * <p>
	 * Le dimensioni possono essere LOC, istruzioni o misure di complessità.<br>
	 * <p>
	 * @param squaleSDPI the squaleSDPI to set
	 */
	public void setSqualeSDPI(double squaleSDPI) {
		this.squaleSDPI = squaleSDPI;
	}

	/**
	 * Restituisce l'indice di Squale rating per la caratteristica Testability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQTI / tempo stimato di sviluppo <br>
	 * <p>
	 * @return the squaleSRTI
	 */
	public double getSqualeSRTI() {
		return squaleSRTI;
	}

	/**
	 * Imposta l'indice di Squale rating per la caratteristica Testability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQTI / tempo stimato di sviluppo <br>
	 * <p>
	 * @param squaleSRTI the squaleSRTI to set
	 */
	public void setSqualeSRTI(double squaleSRTI) {
		this.squaleSRTI = squaleSRTI;
	}

	/**
	 * Restituisce l'indice di Squale rating per la caratteristica Reliability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQRI / tempo stimato di sviluppo <br>
	 * <p>
	 * @return the squaleSRRI
	 */
	public double getSqualeSRRI() {
		return squaleSRRI;
	}

	/**
	 * Imposta l'indice di Squale rating per la caratteristica Reliability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQRI / tempo stimato di sviluppo <br>
	 * <p>
	 * @param squaleSRRI the squaleSRRI to set
	 */
	public void setSqualeSRRI(double squaleSRRI) {
		this.squaleSRRI = squaleSRRI;
	}

	/**
	 * Restituisce l'indice di Squale rating per la caratteristica Changeability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQCI / tempo stimato di sviluppo <br>
	 * <p>
	 * @return the squaleSRCI
	 */
	public double getSqualeSRCI() {
		return squaleSRCI;
	}

	/**
	 * Imposta l'indice di Squale rating per la caratteristica Changeability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQCI / tempo stimato di sviluppo <br>
	 * <p>
	 * @param squaleSRCI the squaleSRCI to set
	 */
	public void setSqualeSRCI(double squaleSRCI) {
		this.squaleSRCI = squaleSRCI;
	}

	/**
	 * Restituisce l'indice di Squale rating per la caratteristica Efficiency.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQEI / tempo stimato di sviluppo <br>
	 * <p>
	 * @return the squaleSREI
	 */
	public double getSqualeSREI() {
		return squaleSREI;
	}

	/**
	 * Imposta l'indice di Squale rating per la caratteristica Efficiency.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQEI / tempo stimato di sviluppo <br>
	 * <p>
	 * @param squaleSREI the squaleSREI to set
	 */
	public void setSqualeSREI(double squaleSREI) {
		this.squaleSREI = squaleSREI;
	}

	/**
	 * Restituisce l'indice di Squale rating per la caratteristica Security.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQSI / tempo stimato di sviluppo <br>
	 * <p>
	 * @return the squaleSRSI
	 */
	public double getSqualeSRSI() {
		return squaleSRSI;
	}

	/**
	 * Imposta l'indice di Squale rating per la caratteristica Security.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQSI / tempo stimato di sviluppo <br>
	 * <p>
	 * @param squaleSRSI the squaleSRSI to set
	 */
	public void setSqualeSRSI(double squaleSRSI) {
		this.squaleSRSI = squaleSRSI;
	}

	/**
	 * Restituisce l'indice di Squale rating per la caratteristica Maintenability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQMI / tempo stimato di sviluppo <br>
	 * <p>
	 * @return the squaleSRMI
	 */
	public double getSqualeSRMI() {
		return squaleSRMI;
	}

	/**
	 * Imposta l'indice di Squale rating per la caratteristica Maintenability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQMI / tempo stimato di sviluppo <br>
	 * <p>
	 * @param squaleSRMI the squaleSRMI to set
	 */
	public void setSqualeSRMI(double squaleSRMI) {
		this.squaleSRMI = squaleSRMI;
	}

	/**
	 * Restituisce l'indice di Squale rating per la caratteristica Portability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQPI / tempo stimato di sviluppo <br>
	 * <p>
	 * @return the squaleSRPI
	 */
	public double getSqualeSRPI() {
		return squaleSRPI;
	}

	/**
	 * Imposta l'indice di Squale rating per la caratteristica Portability.<br>
	 * <p>
	 * Si tratta del rapporto fra l'indice di qualità assoluto SQPI / tempo stimato di sviluppo <br>
	 * <p>
	 * @param squaleSRPI the squaleSRPI to set
	 */
	public void setSqualeSRPI(double squaleSRPI) {
		this.squaleSRPI = squaleSRPI;
	}

	/**
	 * Restituisce il livello di Squale rating per la caratteristica Testability.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @return the squaleSRTL
	 */
	public EnumMetricsSqualeRating getSqualeSRTL() {
		return squaleSRTL;
	}

	/**
	 * Imposta il livello di Squale rating per la caratteristica Testability.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @param squaleSRTL the squaleSRTL to set
	 */
	public void setSqualeSRTL(EnumMetricsSqualeRating squaleSRTL) {
		this.squaleSRTL = squaleSRTL;
	}

	/**
	 * Restituisce il livello di Squale rating per la caratteristica Reliability.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @return the squaleSRRL
	 */
	public EnumMetricsSqualeRating getSqualeSRRL() {
		return squaleSRRL;
	}

	/**
	 * Imposta il livello di Squale rating per la caratteristica Reliability.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @param squaleSRRL the squaleSRRL to set
	 */
	public void setSqualeSRRL(EnumMetricsSqualeRating squaleSRRL) {
		this.squaleSRRL = squaleSRRL;
	}

	/**
	 * Restituisce il livello di Squale rating per la caratteristica Changeability.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @return the squaleSRCL
	 */
	public EnumMetricsSqualeRating getSqualeSRCL() {
		return squaleSRCL;
	}

	/**
	 * Imposta il livello di Squale rating per la caratteristica Changeability.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @param squaleSRCL the squaleSRCL to set
	 */
	public void setSqualeSRCL(EnumMetricsSqualeRating squaleSRCL) {
		this.squaleSRCL = squaleSRCL;
	}

	/**
	 * Restituisce il livello di Squale rating per la caratteristica Efficiency.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @return the squaleSREL
	 */
	public EnumMetricsSqualeRating getSqualeSREL() {
		return squaleSREL;
	}

	/**
	 * Imposta il livello di Squale rating per la caratteristica Efficiency.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @param squaleSREL the squaleSREL to set
	 */
	public void setSqualeSREL(EnumMetricsSqualeRating squaleSREL) {
		this.squaleSREL = squaleSREL;
	}

	/**
	 * Restituisce il livello di Squale rating per la caratteristica Security.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @return the squaleSRSL
	 */
	public EnumMetricsSqualeRating getSqualeSRSL() {
		return squaleSRSL;
	}

	/**
	 * Imposta il livello di Squale rating per la caratteristica Security.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @param squaleSRSL the squaleSRSL to set
	 */
	public void setSqualeSRSL(EnumMetricsSqualeRating squaleSRSL) {
		this.squaleSRSL = squaleSRSL;
	}

	/**
	 * Restituisce il livello di Squale rating per la caratteristica Maintenability.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @return the squaleSRML
	 */
	public EnumMetricsSqualeRating getSqualeSRML() {
		return squaleSRML;
	}

	/**
	 * Imposta il livello di Squale rating per la caratteristica Maintenability.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @param squaleSRML the squaleSRML to set
	 */
	public void setSqualeSRML(EnumMetricsSqualeRating squaleSRML) {
		this.squaleSRML = squaleSRML;
	}

	/**
	 * Restituisce il livello di Squale rating per la caratteristica Portability.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @return the squaleSRPL
	 */
	public EnumMetricsSqualeRating getSqualeSRPL() {
		return squaleSRPL;
	}

	/**
	 * Imposta il livello di Squale rating per la caratteristica Portability.<br>
	 * <p>
	 * Si tratta della codifica del livello A, B, C, D o E.<br>
	 * <p>
	 * @param squaleSRPL the squaleSRPL to set
	 */
	public void setSqualeSRPL(EnumMetricsSqualeRating squaleSRPL) {
		this.squaleSRPL = squaleSRPL;
	}

	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Metodi privati                                        /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	/*
	 * Restituisce le corrette condizioni di Scope
	 * da inserire nello statement Sql dopo la Where
	 * di RELA,
	 * 
	 * 
	 */
	private String getSqlScopeCondition(boolean isRelaA, String tbName)  {
		String sqlScopeCond = "";
		
		switch (this.scope) {
		
			case SCOPE_LEVEL_OBJECT:
				if (tbName.equals("relation")) {
					if (isRelaA) {
						sqlScopeCond = sqlScopeCond + "       R.sys = '" + this.di.systemInput + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.subSys = '" + this.di.subSystemInput + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.idObjectA = '" + this.idObject + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
					} else {
						sqlScopeCond = sqlScopeCond + "       R.sys = '" + this.di.systemInput + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.subSys = '" + this.di.subSystemInput + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.idObjectB = '" + this.idObject + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.typeObjectB = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
					}
				} else if (tbName.equals("RelationOrigin")) {
					if (isRelaA) {
						sqlScopeCond = sqlScopeCond + "       R.sys = '" + this.di.systemInput + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.subSys = '" + this.di.subSystemInput + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.idObjectA = '" + this.idObject + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
					} else {
						sqlScopeCond = sqlScopeCond + "       R.sys = '" + this.di.systemInput + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.subSys = '" + this.di.subSystemInput + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.idObjectB = '" + this.idObject + "'";
						sqlScopeCond = sqlScopeCond + " AND   R.typeObjectB = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
					}
				}
				
				break;
	
			case SCOPE_LEVEL_SECTION:
				break;
	
			case SCOPE_LEVEL_SUBSYSTEM:
				break;
	
			case SCOPE_LEVEL_SYSTEM:
				break;
	
			case SCOPE_LEVEL_GLOBAL:
				break;
	
			default:
				break;
		}
		
		return sqlScopeCond;
	}

	/*
	 * Restituisce le corrette condizioni di Scope
	 * da inserire nello statement Sql dopo la Where
	 * per gli oggetti.
	 */
	private String getSqlScopeConditionObject(String sqlEnd)  {
		String sqlScopeCond = "";
	
		
		
		switch (this.scope) {
		
			case SCOPE_LEVEL_SECTION:
				// Nessuna condizione
					break;
	
			case SCOPE_LEVEL_SUBSYSTEM:
				sqlScopeCond = sqlScopeCond + "          O.sys = '" + this.system + "'";
				sqlScopeCond = sqlScopeCond +	  " AND  O.subSys = '" + this.subSystem + "'";
				sqlScopeCond = sqlScopeCond + sqlEnd;
				break;
	
			case SCOPE_LEVEL_SYSTEM:
				sqlScopeCond = sqlScopeCond + "          O.sys = '" + this.system + "'";
				sqlScopeCond = sqlScopeCond + sqlEnd;
				break;
	
			case SCOPE_LEVEL_GLOBAL:
				// Nessuna condizione
				sqlScopeCond = sqlEnd;
				break;
	
			default:
				break;
		}

		return sqlScopeCond;
	}




	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return idObject + "-" + section;
	}

	
}
