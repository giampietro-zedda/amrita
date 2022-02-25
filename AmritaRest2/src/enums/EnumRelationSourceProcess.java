package enums;

import analyzer.DataBaseMappedEnumeration;

/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumRelationSourceProcess 
 * </h1>
 *  <p>
 * Questa enum elenca le possibili sorgenti delle relazioni elencate dal  {@link EnumRelation}
 * <br>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/02/2010
 * @see Analyzer
 * @see EnumRelation
 * @see EnumRelationOriginInstr
 * @see EnumRelationType
*/
@DataBaseMappedEnumeration
public enum EnumRelationSourceProcess {
	
	NOT_ASSIGNED,				 // 00 
	
	SOURCE_STATIC_LOCAL,         // 01 A fronte di campi valorizzati staticamente nello stesso programma con literal
	SOURCE_DYNAMIC,              // 02 A fronte di campi valorizzati dinamicamente in generale
	SOURCE_DYNAMIC_LOCAL,        // 03 A fronte di campi valorizzati dinamicamente nello stesso programma con assegnazioni
	SOURCE_DYNAMIC_LOCAL_LIGHT,  // 04 A fronte di campi dinamici con value senza assegnazioni
	SOURCE_DYNAMIC_SPREADED,     // 05 A fronte di campi valorizzati dinamicamente in programmi diversi (literal e/o assegnazioni)
	SOURCE_PROCESS,              // 06 A fronte di processo o elaborazioni successive all'analisii
	SOURCE_JCL;         		 // 07 A fronte di analisi del Jcl
}
