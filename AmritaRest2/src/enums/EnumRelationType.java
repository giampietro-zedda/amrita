package enums;

import analyzer.DataBaseMappedEnumeration;

/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumRelationType 
 * </h1>
 *  <p>
 * Questa enum elenca i tipi di relazioni, dirette o indirette.
 * Le relazioni dirette sono originate a fronte dell'analisi di un sorgente.
 * Le relazioni indirette, invece, sono valorizzate a fronte di relazioni con altri oggetti.
 * Per esempio un programma può avere una relazione indiretta di I-O su un file, 
 * generata da un programma generalizzato di accesso ai dati, richiamato a vari livelli di annidamento.
 * <br>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 10/03/2010
 * @see Analyzer
 * @see EnumRelation
 * @see EnumRelationOriginInstr
 * @see EnumRelationSourceProcess
*/
@DataBaseMappedEnumeration
public enum EnumRelationType {

	NOT_ASSIGNED,				// 00 
	
	RELATION_DIRECT,           	// 01 Relazione originata nel programma
	RELATION_INDIRECT,         	// 02 Relazione originata indirettamente
}
