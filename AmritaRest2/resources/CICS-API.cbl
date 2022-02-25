  2931*   (C) COPYRIGHT International Business Machines Corp.
  2932*   1993, 2007
  2933*   All Rights Reserved
  2934*   Licensed Materials - Property of IBM
  2935*   5724-B44
  2936*
  2937*   US Government Users Restricted Rights -
  2938*   Use, duplication or disclosure restricted by
  2939*   GSA ADP Schedule Contract with IBM Corp.
  2940*
  2941*
  2942 01 CICS-ARGS.
  2943     02 CICS-FN-CODE          PIC S9(4) COMP-5.
  2944     02 CICS-DEBUG-LINE       PIC S9(4) COMP-5.
  2945     02 CICS-STACK-LEN        PIC S9(8) COMP-5.
  2946     02 CICS-STACK-PTR        POINTER.
  2947     02 CICS-ARG-MASK         PIC S9(8) COMP-5.
  2948     02 CICS-ARG-COUNT        PIC S9(4) COMP-5.
  2949     02 CICS-ARG-CODE         PIC S9(4) COMP-5 OCCURS 96.
  2950     02 FILLER                PIC X(2).
  2951     02 CICS-PROG-LANG        PIC S9(8) COMP-5.
  2952     02 CICS-EIB-PTR          POINTER.
  2953     02 CICS-ARG-DATA OCCURS  96.
  2954       03 CICS-ARG-DATA-UNION PIC X(8).
  2955       03 CICS-BYTE-VALUE-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2956         04 CICS-BYTE-VALUE PIC X.
  2957         04 FILLER     PIC X(7).
  2958       03 CICS-BYTE-AREA-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2959         04 CICS-BYTE-AREA POINTER.
  2960         04 FILLER     PIC X(4).
  2961       03 CICS-SHORT-VALUE-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2962         04 CICS-SHORT-VALUE PIC S9(4) COMP-5.
  2963         04 FILLER     PIC X(6).
  2964       03 CICS-SHORT-AREA-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2965         04 CICS-SHORT-AREA POINTER.
  2966         04 FILLER     PIC X(4).
  2967       03 CICS-LONG-VALUE-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2968         04 CICS-LONG-VALUE PIC S9(8) COMP-5.
  2969         04 FILLER     PIC X(4).
  2970       03 CICS-LONG-AREA-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2971         04 CICS-LONG-AREA POINTER.
  2972         04 FILLER     PIC X(4).
  2973       03 CICS-DATA-AREA-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2974         04 CICS-DATA-AREA POINTER.
  2975         04 FILLER     PIC X(4).
  2976       03 CICS-POINTER-VALUE-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2977         04 CICS-POINTER-VALUE POINTER.
  2978         04 FILLER     PIC X(4).
  2979       03 CICS-POINTER-REF-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2980         04 CICS-POINTER-REF POINTER.
  2981         04 FILLER     PIC X(4).
  2982       03 CICS-STRING-VALUE-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2983         04 CICS-STRING-VALUE PIC X(8).
  2984       03 CICS-LONG-STRING-VALUE-REDEF REDEFINES
  2985              CICS-ARG-DATA-UNION.
  2986         04 CICS-LONG-STRING-VALUE POINTER.
  2987         04 FILLER     PIC X(4).
  2988       03 CICS-STRING-AREA-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2989         04 CICS-STRING-AREA POINTER.
  2990         04 FILLER     PIC X(4).
  2991       03 CICS-BCD-4-VALUE-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2992         04 CICS-BCD-4-VALUE PIC S9(7) COMP-3.
  2993         04 FILLER     PIC X(4).
  2994       03 CICS-BCD-8-VALUE-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2995         04 CICS-BCD-8-VALUE PIC S9(15) COMP-3.
  2996       03 CICS-DATA-VALUE-REDEF REDEFINES CICS-ARG-DATA-UNION.
  2997         04 CICS-DATA-VALUE PIC X(8).
  2998     02 CICS-LONG-STRING-TAB PIC X(256) OCCURS 3.
  2999*
  3000 01  CICS-API-TEMP-STORAGE.
  3001     02  CICS-API-TEMP        PIC S9(4) COMP-5 OCCURS 2.
  3002     02  CICS-API-TEMPPTR     USAGE POINTER OCCURS 8.
  3003     02  CICS-API-TEMP-SHORT  PIC S9(4) COMP-5 OCCURS 4.
  3004     02  CICS-API-TEMP-LONG   PIC S9(8) COMP-5 OCCURS 4.
  3005 01  CICS-API-WS-END          PIC X.
