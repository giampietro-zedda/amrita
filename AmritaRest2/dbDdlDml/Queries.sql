--
--
-- (00) Enumerazioni presenti
--
select * from tableheader;  
select * from tabledata; 
--
-- (00) Conteggi generali di tutti gli oggetti
--
select 'CNT-OBJ' AS CNT
  , (select count(*)  from Object o where statusObject = 04 ) as OBJECT_ANALYZED_WITH_NO_ERRORS
  , (select count(*)  from Object o where statusObject = 05 ) as OBJECT_ANALYZED_WITH_ERRORS
  , (select count(*)  from Object o where statusObject = 06 ) as OBJECT_ANALYZED_WITH_EXCEPTION 
  , (select count(*)  from Object o where statusObject = 10 ) as OBJECT_TO_BE_ANALYZED 
  , (select count(*)  from Object o where statusObject = 11 ) as OBJECT_NOT_TO_BE_ANALYZED  
  , (select count(*)  from Object o where statusObject = 01)  as SOURCE_MEMBER_ACQUIRED  
  , (select count(*)  from Object o where statusObject = 02)  as SOURCE_MEMBER_TYPE_DETECTED
  , (select count(*)  from Object o where statusObject = 03 ) as SOURCE_MEMBER_TYPE_NOT_DETECTED
  , (select count(*)  from Object o where statusObject = 12 ) as OBJECT_TO_BE_ACQUIRED
  , (select count(*)  from Object o where statusObject = 07 ) as OBJECT_PROCESSED_WITH_NO_ERRORS   
  , (select count(*)  from Object o where statusObject = 08 ) as OBJECT_PROCESSED_WITH_ERRORS   
  , (select count(*)  from Object o where statusObject = 09 ) as OBJECT_PROCESSED_WITH_EXCEPTION
  , (select count(*)  from Object o where statusObject = 13 ) as OBJECT_ANALYZED_DYNAMIC_SOLVED
     UNION
select 'CNT-COPY' AS CNT
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 04 ) as OBJECT_ANALYZED_WITH_NO_ERRORS
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 05 ) as OBJECT_ANALYZED_WITH_ERRORS
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 06 ) as OBJECT_ANALYZED_WITH_EXCEPTION 
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 10 ) as OBJECT_TO_BE_ANALYZED 
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 11 ) as OBJECT_NOT_TO_BE_ANALYZED  
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 01)  as SOURCE_MEMBER_ACQUIRED  
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 02)  as SOURCE_MEMBER_TYPE_DETECTED
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 03 ) as SOURCE_MEMBER_TYPE_NOT_DETECTED
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 12 ) as OBJECT_TO_BE_ACQUIRED
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 07 ) as OBJECT_PROCESSED_WITH_NO_ERRORS   
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 08 ) as OBJECT_PROCESSED_WITH_ERRORS   
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 09 ) as OBJECT_PROCESSED_WITH_EXCEPTION
  , (select count(*)  from Object o where typeObject in(9, 10) and statusObject = 13 ) as OBJECT_ANALYZED_DYNAMIC_SOLVED
     UNION
select 'CNT-COPY-PROC' AS CNT
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 04 ) as OBJECT_ANALYZED_WITH_NO_ERRORS
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 05 ) as OBJECT_ANALYZED_WITH_ERRORS
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 06 ) as OBJECT_ANALYZED_WITH_EXCEPTION 
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 10 ) as OBJECT_TO_BE_ANALYZED 
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 11 ) as OBJECT_NOT_TO_BE_ANALYZED  
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 01)  as SOURCE_MEMBER_ACQUIRED  
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 02)  as SOURCE_MEMBER_TYPE_DETECTED
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 03 ) as SOURCE_MEMBER_TYPE_NOT_DETECTED
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 12 ) as OBJECT_TO_BE_ACQUIRED
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 07 ) as OBJECT_PROCESSED_WITH_NO_ERRORS   
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 08 ) as OBJECT_PROCESSED_WITH_ERRORS   
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 09 ) as OBJECT_PROCESSED_WITH_EXCEPTION
  , (select count(*)  from Object o where typeObject in(9) and statusObject = 13 ) as OBJECT_ANALYZED_DYNAMIC_SOLVED
     UNION
select 'CNT-COPY-DATA' AS CNT
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 04 ) as OBJECT_ANALYZED_WITH_NO_ERRORS
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 05 ) as OBJECT_ANALYZED_WITH_ERRORS
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 06 ) as OBJECT_ANALYZED_WITH_EXCEPTION 
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 10 ) as OBJECT_TO_BE_ANALYZED 
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 11 ) as OBJECT_NOT_TO_BE_ANALYZED  
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 01)  as SOURCE_MEMBER_ACQUIRED  
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 02)  as SOURCE_MEMBER_TYPE_DETECTED
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 03 ) as SOURCE_MEMBER_TYPE_NOT_DETECTED
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 12 ) as OBJECT_TO_BE_ACQUIRED
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 07 ) as OBJECT_PROCESSED_WITH_NO_ERRORS   
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 08 ) as OBJECT_PROCESSED_WITH_ERRORS   
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 09 ) as OBJECT_PROCESSED_WITH_EXCEPTION
  , (select count(*)  from Object o where typeObject in(10) and statusObject = 13 ) as OBJECT_ANALYZED_DYNAMIC_SOLVED
     UNION
select 'CNT-PGM-COB' AS CNT
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 04 ) as OBJECT_ANALYZED_WITH_NO_ERRORS
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 05 ) as OBJECT_ANALYZED_WITH_ERRORS
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 06 ) as OBJECT_ANALYZED_WITH_EXCEPTION 
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 10 ) as OBJECT_TO_BE_ANALYZED 
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 11 ) as OBJECT_NOT_TO_BE_ANALYZED  
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 01)  as SOURCE_MEMBER_ACQUIRED  
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 02)  as SOURCE_MEMBER_TYPE_DETECTED
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 03 ) as SOURCE_MEMBER_TYPE_NOT_DETECTED
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 12 ) as OBJECT_TO_BE_ACQUIRED
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 07 ) as OBJECT_PROCESSED_WITH_NO_ERRORS   
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 08 ) as OBJECT_PROCESSED_WITH_ERRORS   
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 09 ) as OBJECT_PROCESSED_WITH_EXCEPTION
  , (select count(*)  from Object o where typeObject in(1) and statusObject = 13 ) as OBJECT_ANALYZED_DYNAMIC_SOLVED
;


-
-- (01) Objects (tutti)
--
select r.sys, r.subsys, 
       r.idObject
     , t1.rowData AS typeObject
     , t2.rowData AS typeSource
     , t3.rowData AS statusObject
     , t4.rowData AS typeObjectDescriptor
     , r.*
  from Object r
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=2  -- EnumSourceType
        and t2.keyVal = CAST(r.typeSource AS CHAR)  
  LEFT  JOIN TableData t3  ON
            t3.sys='*'
        and t3.subsys='*'
        and t3.numTable=3  -- EnumObjectStatus
        and t3.keyVal = CAST(r.statusObject AS CHAR)     
  LEFT  JOIN TableData t4  ON
            t4.sys='*'
		and t4.subsys='*'
        and t4.numTable=1  -- EnumObject
        and t4.keyVal = CAST(r.typeObjectDescriptor AS CHAR) 
--    where idObject in ('GTSVBFC')
-- where r.sys='Z' and r.subSys='GP'	
-- where r.TypeObject=88  -- OBJECT_LIBRARY
--     idObject='XXXXXXX'
--     r.TypeObject=88  -- OBJECT_LIBRARY
-- and r.statusObject = 01 -- SOURCE_MEMBER_ACQUIRED,         // 01 Sorgente acquisito nella libreria di pertinenza
-- and r.statusObject = 02 -- SOURCE_MEMBER_TYPE_DETECTED,    // 02 Sorgente identificato come tipologia
-- and r.statusObject = 03 -- SOURCE_MEMBER_TYPE_NOT_DETECTED,    // 02 Sorgente identificato come tipologia
-- and r.statusObject = 04 -- OBJECT_ANALYZED_WITH_NO_ERRORS, // 03 Oggetto a fronte di analisi sorgente senza errori
-- and r.statusObject = 05 -- OBJECT_ANALYZED_WITH_ERRORS,    // 04 Oggetto a fronte di analisi sorgente con   errori
-- and r.statusObject = 06 -- OBJECT_ANALYZED_WITH_EXCEPTION, // 05 Oggetto a fronte di analisi sorgente in exception
-- and r.statusObject = 07 -- OBJECT_PROCESSED_WITH_NO_ERRORS,// 06 Oggetto a fronte di processo senza errori
-- and r.statusObject = 08 -- OBJECT_PROCESSED_WITH_ERRORS,   // 07 Oggetto a fronte di processo con errori
-- and r.statusObject = 09 -- OBJECT_PROCESSED_WITH_EXCEPTION,// 08 Oggetto a fronte di processo in exception
-- and r.statusObject = 10 -- OBJECT_TO_BE_ANALYZED,          // 09 Oggetto ancora da analizzare, a fronte di analisi di altri oggetti
-- and r.statusObject = 11 -- OBJECT_NOT_TO_BE_ANALYZED,      // 10 Oggetto al quale non corrisponde un sorgente da analizzare
-- and r.statusObject = 12 -- OBJECT_TO_BE_ACQUIRED,          // 11 Oggetto da acquisire con library scan (tipicamente un programma)
-- and r.statusObject = 13 -- OBJECT_ANALYZED_DYNAMIC_SOLVED, // 12 Oggetto con codice dinamico locale e spreaded risolto
-- and r.statusObject = 14 -- OBJECT_NOT_ACTIVE          	  // 13 Oggetto non + attivo, da considerare come non esistente in analisi e processi
order by r.typeObject, r.idObject;

--
-- (01A) Objects (COPY)
--
select r.sys, r.subsys, 
       r.idObject
     , t1.rowData AS typeObject
     , t2.rowData AS typeSource
     , t3.rowData AS statusObject
     , t4.rowData AS typeObjectDescriptor
     , r.*
  from Object r
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=2  -- EnumSourceType
        and t2.keyVal = CAST(r.typeSource AS CHAR)  
  LEFT  JOIN TableData t3  ON
            t3.sys='*'
        and t3.subsys='*'
        and t3.numTable=3  -- EnumObjectStatus
        and t3.keyVal = CAST(r.statusObject AS CHAR)     
  LEFT  JOIN TableData t4  ON
            t4.sys='*'
		and t4.subsys='*'
        and t4.numTable=1  -- EnumObject
        and t4.keyVal = CAST(r.typeObjectDescriptor AS CHAR) 
   where r.typeObject in(9, 10)  -- OBJECT_COPY_COBOL_PROC, OBJECT_COPY_COBOL_DATA
order by r.typeObject, r.statusObject, r.idObject;

--
-- (01B) Objects (PGM_COBOL)
--
select r.sys, r.subsys, 
       r.idObject
     , t1.rowData AS typeObject
     , t2.rowData AS typeSource
     , t3.rowData AS statusObject
     , t4.rowData AS typeObjectDescriptor
     , r.*
  from Object r
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=2  -- EnumSourceType
        and t2.keyVal = CAST(r.typeSource AS CHAR)  
  LEFT  JOIN TableData t3  ON
            t3.sys='*'
        and t3.subsys='*'
        and t3.numTable=3  -- EnumObjectStatus
        and t3.keyVal = CAST(r.statusObject AS CHAR)     
  LEFT  JOIN TableData t4  ON
            t4.sys='*'
		and t4.subsys='*'
        and t4.numTable=1  -- EnumObject
        and t4.keyVal = CAST(r.typeObjectDescriptor AS CHAR) 
   where r.typeObject in(1)  -- OBJECT_PGM_COBOL
order by r.typeObject, r.statusObject, r.idObject;

--
-- (02) Object Option
--
select r.sys, r.subsys, 
       r.idObject
     , t1.rowData AS typeObject
     , t2.rowData AS optionObject
     , r.*
  from ObjectOption r
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=4  -- EnumObjectOption
        and t2.keyVal = CAST(r.optionObject AS CHAR)  
-- where idObject='XXXXXXX'
order by r.idObject, r.optionObject;

--
-- (03) Relations  
--
select r.sys, r.subsys
     , r.idObjectA
     , r.idObjectB
     , r.relation
     , t1.rowData AS relationDesc 
 from relation r, TableData t1
   LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=34  -- EnumRelation
        and t1.keyVal = CAST(r.relation AS CHAR)
-- where idObject='XXXXXXX'
order by r.idObjectA, r.idObjectB;


--
-- (04) Relations Origin
--
select r.sys, r.subsys, r.idObjectA, r.idObjectB, r.relation
       , t1.rowData AS relation
       , t2.rowData AS relationType
       , t3.rowData AS instrProgramArea
       , t4.rowData AS relationSource
       , t5.rowData AS instrTypePrecompiler
	   , t6.rowData AS instrCategory
       , t7.rowData AS typeObjectOrigin
       , r.*  
  from relationOrigin r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=34  -- EnumRelation
        and t1.keyVal = CAST(r.relation AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=5  -- EnumRelationType
        and t2.keyVal = CAST(r.relationType AS CHAR)  
  LEFT  JOIN TableData t3  ON
            t3.sys='*'
        and t3.subsys='*'
        and t3.numTable=29  -- EnumCobolReservedWords
        and t3.keyVal = CAST(r.instrProgramArea AS CHAR)     
  LEFT  JOIN TableData t4  ON
            t4.sys='*'
		and t4.subsys='*'
        and t4.numTable=6  -- EnumRelationSourceProcess
        and t4.keyVal = CAST(r.relationSource AS CHAR) 
  LEFT  JOIN TableData t5  ON
            t5.sys='*'
		and t5.subsys='*'
        and t5.numTable=9  -- EnumPrecompilerReservedWords
		and t5.keyVal = CAST(r.instrTypePrecompiler AS CHAR) 
  LEFT  JOIN TableData t6  ON
           t6.sys='*'
	   and t6.subsys='*'
       and t6.numTable=18  -- EnumInstrDataCategory
       and t6.keyVal = CAST(r.instrCategory AS CHAR)           
  LEFT  JOIN TableData t7  ON
           t7.sys='*'
	   and t7.subsys='*'
       and t7.numTable=1  -- EnumObject
       and t7.keyVal = CAST(r.typeObjectOrigin AS CHAR)                
-- where idObjectA='XXXXXXX'
order by r.idObjectA, r.idObjectB, r.relation;


--
-- (05) Where Used Item
--
select r.sys, r.subsys
       , r.idObject 
       , t1.rowData AS typeObject
       , r.idfield
       , t2.rowData AS typeObjectRefer
	   , t3.rowData AS typeWhereUsed
	   , t4.rowData AS typeAlias
	   , t5.rowData AS instrLang
	   , t6.rowData AS instrType
       , r.*  
  from WhereUsed r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=1  -- EnumObject
        and t2.keyVal = CAST(r.typeObjectRefer AS CHAR)         
  LEFT  JOIN TableData t3 ON
            t3.sys='*'
	    and t3.subsys='*'
        and t3.numTable=10  -- EnumWhereUsedType
        and t3.keyVal = CAST(r.typeWhereUsed AS CHAR)  
  LEFT  JOIN TableData t4  ON
            t4.sys='*'
        and t4.subsys='*'
        and t4.numTable=11  -- EnumWhereUsedTypeAlias
        and t4.keyVal = CAST(r.typeAlias AS CHAR)     
  LEFT  JOIN TableData t5  ON
            t5.sys='*'
		and t5.subsys='*'
        and t5.numTable=7 -- EnumLanguageItemInstr
        and t5.keyVal = CAST(r.instrLang AS CHAR) 
  LEFT  JOIN TableData t6  ON
            t6.sys='*'
		and t6.subsys='*'
        and t6.numTable=9  -- EnumPrecompilerReservedWords
		and t6.keyVal = CAST(r.instrType AS CHAR)        
-- where idObject='XXXXXXX'
order by r.idObject, r.idfield ;

--
-- (06) Objects Analysys Info  
--
select r.sys, r.subsys
       , r.idObject 
       , t1.rowData AS typeObject
       , r.*  
  from objectAnalysisInfo r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
-- where idObjectA='XXXXXXX'
order by r.idObject;

--
-- (07) Objects Analysys Error  
--
select r.sys, r.subsys
       , r.idObject 
       , t1.rowData AS typeObject
	   , t2.rowData AS typeProcessAnalysis
	   , t3.rowData AS typeInstr
	   , t4.rowData AS activeCobolDivision
	   , t5.rowData AS activeCobolSection
       , r.*  
  from objectAnalysisError r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=25  -- EnumTypeProcessAnalysis
        and t2.keyVal = CAST(r.typeProcessAnalysis AS CHAR)  
  LEFT  JOIN TableData t3  ON
            t3.sys='*'
        and t3.subsys='*'
        and t3.numTable=18  -- EnumInstrDataCategory
        and t3.keyVal = CAST(r.typeInstr AS CHAR)     
  LEFT  JOIN TableData t4  ON
            t4.sys='*'
		and t4.subsys='*'
        and t4.numTable=29 -- EnumCobolReservedWords
        and t4.keyVal = CAST(r.activeCobolDivision AS CHAR) 
  LEFT  JOIN TableData t5  ON
            t5.sys='*'
		and t5.subsys='*'
        and t5.numTable=29  -- EnumCobolReservedWords
		and t5.keyVal = CAST(r.activeCobolSection AS CHAR)        
-- where idObjectA='XXXXXXX'
order by r.idObject;


--
-- (08) Entity/Copy definition  
--
select r.sys, r.subsys
       , r.idObject 
       , r.idField 
       , t1.rowData AS typeObject
	   , t2.rowData AS itemLang
	   , t3.rowData AS itemType
       , r.*  
  from CopyEntityDefinition r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=7  -- EnumLanguageItemInstr
        and t2.keyVal = CAST(r.itemLang AS CHAR)  
  LEFT  JOIN TableData t3  ON
            t3.sys='*'
        and t3.subsys='*'
        and t3.numTable=8  -- EnumDataItemType
        and t3.keyVal = CAST(r.itemType AS CHAR)       
-- where idObjectA='XXXXXXX'
order by r.idObject;

--
-- (09) Metric Value
--
select r.sys, r.subsys
       , r.idObject 
	   , t2.rowData AS typeObject
	   , t1.rowData AS scope
       , r.section 
       , r.*  
  from MetricValue r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=45  -- EnumMetricsScope
        and t1.keyVal = CAST(r.scope AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=1  -- EnumObject
        and t2.keyVal = CAST(r.typeObject AS CHAR) 
-- where r.idObject='XXXXXXX'
--  and r.scope= 1 -- SCOPE_LEVEL_OBJECT,         // (01) Metriche a livello di oggetto di analisi (PROGRAM, SCRIPT_SQL etc.) 
--  and r.scope= 2 -- SCOPE_LEVEL_SECTION,        // (02) Metriche a livello di singola sezione oggetto di analisi, come section di programma
--  and r.scope= 3 -- SCOPE_LEVEL_SUBSYSTEM,      // (03) Metriche a livello di sottosistema applicativo (in un sistema)  
--  and r.scope= 4 -- SCOPE_LEVEL_SYSTEM,         // (04) Metriche a livello di sistema applicativo   
--  and r.scope= 5 -- SCOPE_LEVEL_GLOBAL;         // (05) Metriche a livello globale per tutti i sistemi e sottosistemi  
order by r.idObject, r.scope, r.section;

--
-- (10) Metric Violation
--
select r.sys, r.subsys
       , r.idObject 
       , t1.rowData AS typeObject
	   , r.section 
	   , t2.rowData AS typeViolation
	   , t3.rowData AS severityViolation
	   , t4.rowData AS remediationUnit
	   , t5.rowData AS qualityCharacteristic
       , r.*  
  from MetricViolation r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=46  -- EnumMetricsViolation
        and t2.keyVal = CAST(r.typeViolation AS CHAR)  
  LEFT  JOIN TableData t3  ON
            t3.sys='*'
        and t3.subsys='*'
        and t3.numTable=47  -- EnumMetricsViolationSeverity
        and t3.keyVal = CAST(r.severityViolation AS CHAR)     
  LEFT  JOIN TableData t4  ON
            t4.sys='*'
		and t4.subsys='*'
        and t4.numTable=48 -- EnumMetricsViolationFixUnit
        and t4.keyVal = CAST(r.remediationUnit AS CHAR) 
  LEFT  JOIN TableData t5  ON
            t5.sys='*'
		and t5.subsys='*'
        and t5.numTable=49  -- EnumMetricsQualityCharacteristics
		and t5.keyVal = CAST(r.qualityCharacteristic AS CHAR)        
-- where idObject='XXXXXXX'
order by r.idObject, r.section, r.typeViolation;


--
-- (11) Dynamic Field, campi dinamici nel programma
--
select r.sys, r.subsys
       , r.idObject 
       , t1.rowData AS typeObject
       , r.idfield
	   , t2.rowData AS instrCobolType
	   , t3.rowData AS instrPrecompType
	   , t4.rowData AS instrOprndType
       , r.*  
  from DynamicField r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=29  -- EnumCobolReservedWords
        and t2.keyVal = CAST(r.instrCobolType AS CHAR)         
  LEFT  JOIN TableData t3 ON
            t3.sys='*'
	    and t3.subsys='*'
        and t3.numTable=9  -- EnumPrecompilerReservedWords
        and t3.keyVal = CAST(r.instrPrecompType AS CHAR)  
  LEFT  JOIN TableData t4  ON
            t4.sys='*'
        and t4.subsys='*'
        and t4.numTable=9  -- EnumPrecompilerReservedWords
        and t4.keyVal = CAST(r.instrPrecompOprndType AS CHAR)     
   
-- where idObject='XXXXXXX'
order by r.idObject, r.idfield ;

--
-- (12) Dynamic Field sub, composizione campi dinamici
--
select r.sys, r.subsys
       , r.idObject 
       , t1.rowData AS typeObject
       , r.idField
	   , r.idSubField
       , r.*  
  from DynamicFieldSub r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
-- where idObject='XXXXXXX'
order by r.idObject, r.idfield ;


--
-- (13) Dynamic Field sub Setting
--
select r.sys, r.subsys
       , r.idObject 
       , r.idField
       , r.idSubField
       , r.value
       , t1.rowData AS typeObject
	   , t2.rowData AS typeObjectPgmSet
	   , t3.rowData AS setMode
	   , t4.rowData AS typePointerArea
       , r.*  
  from DynamicFieldSubSetting r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
            t2.sys='*'
	    and t2.subsys='*'
        and t2.numTable=1  -- EnumObject
        and t2.keyVal = CAST(r.typeObjectPgmSet AS CHAR)         
  LEFT  JOIN TableData t3 ON
            t3.sys='*'
	    and t3.subsys='*'
        and t3.numTable=44  -- EnumLogicSetMode
        and t3.keyVal = CAST(r.setMode AS CHAR)  
  LEFT  JOIN TableData t4  ON
            t4.sys='*'
        and t4.subsys='*'
        and t4.numTable=28 -- EnumLogicSetPointerArea
        and t4.keyVal = CAST(r.typePointerArea AS CHAR)     
   
-- where idObject='XXXXXXX'
order by r.idObject, r.idfield ;

--
-- (14) Dynamic Field sub value
--
select r.sys, r.subsys
       , r.idObject 
       , t1.rowData AS typeObject
	   , r.numInstr
       , r.idField
       , r.idSubField
       , r.value
       , r.progr
       , r.posInSubField
       , r.lngInSubField
       , r.idObjectFrom
       , t2.rowData AS typeObjectFrom
	   , r.numInstrFrom
  from DynamicFieldSubValue r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
		    t2.sys='*'
		and t2.subsys='*'
        and t2.numTable=1  -- EnumObject
        and t2.keyVal = CAST(r.typeObjectFrom AS CHAR)        
-- where idObject='XXXXXXX'
order by r.idObject, r.idfield ;


--
-- (15) Dynamic Value External
--
select r.sys, r.subsys
       , r.idObjectExternal
       , t1.rowData AS typeObjectExternal
       , t2.rowData AS systemFieldExternal
       , r.cicsNameExternal
       , r.idFieldExternal
       , r.posColumnExternal
       , r.lengthColumnExternal
       , r.numProgr
  from DynamicValueExt r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObjectExternal AS CHAR)
  LEFT  JOIN TableData t2 ON
		    t2.sys='*'
		and t2.subsys='*'
        and t2.numTable=21  -- EnumDataItemSystemEnvironment
        and t2.keyVal = CAST(r.systemFieldExternal AS CHAR)
        
-- where idObject='XXXXXXX'
order by r.idObjectExternal ;

--
-- (16) Dynamic Value Waiting External
--
select r.sys, r.subsys
       , r.idObjectExternal
       , t1.rowData AS typeObject
       , r.idObjectExternal
       , t2.rowData AS typeObjectExternal
       , t3.rowData AS typeSystemFieldExternal
       , r.numInstr
       , r.idField
       , r.numProgr
       , r.cicsNameExternal
       , r.idFieldExternal
       , r.posColumnExternal
       , r.lengthColumnExternal
  from DynamicFieldSubWaitExt r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObjectExternal AS CHAR)
  LEFT  JOIN TableData t2 ON
		    t2.sys='*'
		and t2.subsys='*'
        and t2.numTable=1  -- EnumObject
        and t2.keyVal = CAST(r.typeObjectExternal AS CHAR)
  LEFT  JOIN TableData t3 ON
		    t3.sys='*'
		and t3.subsys='*'
        and t3.numTable=21  -- EnumDataItemSystemEnvironment
        and t3.keyVal = CAST(r.typeSystemFieldExternal AS CHAR)
-- where idObject='XXXXXXX'
order by r.idObjectExternal ;


--
-- (17) Index Item
--
select r.sys, r.subsys
       , r.idObject
       , t1.rowData AS typeObject
       , r.idObjectOwner
       , t2.rowData AS typeObjectOwner
       , r.numSeq
       , r.idColumnName
      , t3.rowData AS orderType
  from IndexItem r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
  LEFT  JOIN TableData t2 ON
		    t2.sys='*'
		and t2.subsys='*'
        and t2.numTable=1  -- EnumObject
        and t2.keyVal = CAST(r.typeObjectOwner AS CHAR)
  LEFT  JOIN TableData t3 ON
		    t3.sys='*'
		and t3.subsys='*'
        and t3.numTable=41  -- EnumIndexOrder
        and t3.keyVal = CAST(r.orderType AS CHAR)
-- where idObject='XXXXXXX'
order by r.idObject ;


--
-- (18) Map Descriptor
--
select r.sys, r.subsys
       , r.idObject
       , t1.rowData AS typeObject
       , r.idObjectMapset       
       , r.idObjectMap       
       , r.fieldCursor       
       , r.rowsSize       
       , r.colsSize       
       , r.rowBegin       
       , r.colBegin       
  from MapDescriptor r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
order by r.idObject,rowBegin, r.colBegin;

--
-- (19) Map Item
--
select r.sys, r.subsys
       , r.idObject
       , t1.rowData AS typeObject
       , r.*           
  from MapItem r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
order by r.idObject,rowNum, r.col;

--
-- (20) Tag Value
--
select r.sys, r.subsys
       , r.idObject
       , t1.rowData AS typeObject
       , r.progr
       , r.tagValue                  
       , r.tagLength                  
  from TagValue r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=1  -- EnumObject
        and t1.keyVal = CAST(r.typeObject AS CHAR)
order by r.idObject,progr;

--
-- (21) Process Log
--
select r.sys, r.subsys
       , t1.rowData AS typeProcess       
       , t2.rowData AS statusProcess
       , t3.rowData AS typeObjectError
       , r.*                
  from ProcessLog r 
  LEFT  JOIN TableData t1 ON
		    t1.sys='*'
		and t1.subsys='*'
        and t1.numTable=33  -- EnumDirectivesExecution
        and t1.keyVal = CAST(r.typeProcess AS CHAR)
  LEFT  JOIN TableData t2 ON
		    t2.sys='*'
		and t2.subsys='*'
        and t2.numTable=13  -- EnumProcessStatus
        and t2.keyVal = CAST(r.statusProcess AS CHAR)
  LEFT  JOIN TableData t3 ON
		    t3.sys='*'
		and t3.subsys='*'
        and t3.numTable=1  -- EnumObject
        and t3.keyVal = CAST(r.typeObjectError AS CHAR)
  order by r.dtStartProcess;

  --
-- (XX) Pulizia tabelle Z/ZZ
--
delete FROM amrita.dynamicFieldSubSetting where sys='Z' and subSys='ZZ';
delete FROM amrita.dynamicFieldSubValue where sys='Z' and subSys='ZZ';
delete FROM amrita.dynamicFieldSubWaitExt where sys='Z' and subSys='ZZ';
delete FROM amrita.dynamicFieldSub where sys='Z' and subSys='ZZ';
delete FROM amrita.dynamicfield where sys='Z' and subSys='ZZ';
delete FROM amrita.metricValue where sys='Z' and subSys='ZZ';
delete FROM amrita.metricViolation where sys='Z' and subSys='ZZ';
delete FROM amrita.objectAnalysisError where sys='Z' and subSys='ZZ';
delete FROM amrita.objectAnalysisInfo where sys='Z' and subSys='ZZ';
delete FROM amrita.objectOption where sys='Z' and subSys='ZZ';
delete FROM amrita.relationOrigin where sys='Z' and subSys='ZZ';
delete FROM amrita.relation where sys='Z' and subSys='ZZ';
delete FROM amrita.relation where sys='*' and subSys='*';
delete FROM amrita.copyEntityDefinition where sys='Z' and subSys='ZZ';
delete FROM amrita.object where sys='Z' and subSys='ZZ';
delete FROM amrita.object where sys='*' and subSys='*';
--
-- (XX) Pulizia tabelle Z/GP
--
delete FROM amrita.dynamicFieldSubSetting where sys='Z' and subSys='GP';
delete FROM amrita.dynamicFieldSubValue where sys='Z' and subSys='GP';
delete FROM amrita.dynamicFieldSubWaitExt where sys='Z' and subSys='GP';
delete FROM amrita.dynamicFieldSub where sys='Z' and subSys='GP';
delete FROM amrita.dynamicfield where sys='Z' and subSys='GP';
delete FROM amrita.metricValue where sys='Z' and subSys='GP';
delete FROM amrita.metricViolation where sys='Z' and subSys='GP';
delete FROM amrita.objectAnalysisError where sys='Z' and subSys='GP';
delete FROM amrita.objectAnalysisInfo where sys='Z' and subSys='GP';
delete FROM amrita.objectOption where sys='Z' and subSys='GP';
delete FROM amrita.relationOrigin where sys='Z' and subSys='GP';
delete FROM amrita.relation where sys='Z' and subSys='GP';
delete FROM amrita.relation where sys='*' and subSys='*';
delete FROM amrita.copyEntityDefinition where sys='Z' and subSys='GP';
delete FROM amrita.object where sys='Z' and subSys='GP';
delete FROM amrita.object where sys='*' and subSys='*';


--
-- Pulizia tabelle VN / AN
--
-- Clear DynamicFieldValue
delete from dynamicfieldsubvalue where idObject IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);
-- Clear DynamicFieldSub
delete from WhereUsed where idObject IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);
-- Clear DynamicField 
delete from DynamicField where idObject IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);
-- Clear DynamicField 
delete from DynamicField where idObject IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);
-- Clear DynamicField 
delete from RelationOrigin where idObjectA IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);
-- Clear DynamicField 
delete from Relation where idObjectA IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);
-- Clear ObjectAnalysisInfo 
delete from ObjectAnalysisInfo where idObject IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);
-- Clear ObjectAnalysisInfo 
delete from ObjectAnalysisError where idObject IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);

-- Clear ObjectOption
delete from ObjectOption where idObject IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);

-- Clear Relation VN AN
delete from Relation where idObjectA IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);

-- Clear Relation VN AN
delete from Relation where idObjectB IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);

-- Clear copyEntityDefinition VN AN
delete from Relation where idObjectA IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);

-- Clear copyEntityDefinition VN AN
delete from Relation where idObjectB IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);

-- Clear Object VN
delete from Object where idObject IN
(
 'ANCOFMR'
,'ANCOH4B'
,'ANFMFMR'
,'ANFMKMR'
,'ANSAAIP'
,'ANSAATP'
,'ANSAAVP'
,'ANSACCP'
,'ANSACPP'
,'ANSACSB'
,'ANSAD1P'
,'ANSADBP'
,'ANSAEIP'
,'ANSAEXB'
,'ANSAFMB'
,'ANSAFMR'
,'ANSAFOP'
,'ANSAGTP'
,'ANSAH4B'
,'ANSAIDP'
,'ANSAIFP'
,'ANSAINP'
,'ANSAINX'
,'ANSAIPP'
,'ANSAIVP'
,'ANSAIXP'
,'ANSAK1P'
,'ANSAK2P'
,'ANSAK3P'
,'ANSAMUP'
,'ANSAPRX'
,'ANSAPSP'
,'ANSAQUP'
,'ANSAR1P'
,'ANSARCP'
,'ANSARTP'
,'ANSASPP'
,'ANSASSB'
,'ANSATCP'
,'ANSATXB'
,'ANSATXT'
,'ANSAWCP'
,'ANSOTBB'
,'CONVDATE'
,'CONVDATEALFA'
,'CONVDATESEC'
,'CONVNUM'
,'CONVNUMALFA'
,'FCLAFMCO'
,'FCWXPLAT'
,'FMSATXNC'
,'GTSACBI'
,'GTSACCM'
,'GTSATPS'
,'GTSATUT'
,'GTSAVCT'
,'GTSAVLI'
,'STRSTUB'
,'TBITEM'
,'TBSAUVP'
,'TBSQCFLK'
,'TSGESM'
,'UTSAANN'
,'UTSAAPLK'
,'UTSACMBO'
,'UTSACPLK'
,'UTSADDLK'
,'UTSALDCK'
,'UTSAPAR'
,'UTSARC00'
,'UTSARC02'
,'UTSARC04'
,'UTSARC45'
,'UTSASOP'
,'UTSASTK'
,'UTSATBCK'
,'UTSATTCK'
,'UTSAUXSV'
,'UTSAWRK2'
);


select * from Object where sys='VN' and subSys='AN' and idObject='ANCOFMR' and typeSource=0;