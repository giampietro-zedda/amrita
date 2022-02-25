  3009*
  3010* NAME:    CICS-EIB
  3011* VERSION: 1.2
  3012*
  3013*   (C) COPYRIGHT International Business Machines Corp.
  3014*   1993, 2007
  3015*   All Rights Reserved
  3016*   Licensed Materials - Property of IBM
  3017*   5724-B44
  3018*
  3019*   US Government Users Restricted Rights -
  3020*   Use, duplication or disclosure restricted by
  3021*   GSA ADP Schedule Contract with IBM Corp.
  3022*
  3023*
  3024 01 DFHEIBLK.
  3025     02 EIBTIME          PIC S9(7) COMP-3.
  3026     02 EIBDATE          PIC S9(7) COMP-3.
  3027     02 EIBTRNID         PIC X(4).
  3028     02 EIBTASKN         PIC S9(7) COMP-3.
  3029     02 EIBTRMID         PIC X(4).
  3030     02 EIBFIL01         PIC S9(4) COMP-5.
  3031     02 EIBCPOSN         PIC S9(4) COMP-5.
  3032     02 EIBCALEN         PIC S9(4) COMP-5.
  3033     02 EIBAID           PIC X.
  3034     02 EIBFN            PIC X(2).
  3035     02 EIBRCODE         PIC X(6).
  3036     02 EIBDS            PIC X(8).
  3037     02 EIBREQID         PIC X(8).
  3038     02 EIBRSRCE         PIC X(8).
  3039     02 EIBSYNC          PIC X.
  3040     02 EIBFREE          PIC X.
  3041     02 EIBRECV          PIC X.
  3042     02 EIBFIL02         PIC X.
  3043     02 EIBATT           PIC X.
  3044     02 EIBEOC           PIC X.
  3045     02 EIBFMH           PIC X.
  3046     02 EIBCOMPL         PIC X.
  3047     02 EIBSIG           PIC X.
  3048     02 EIBCONF          PIC X.
  3049     02 EIBERR           PIC X.
  3050     02 EIBERRCD         PIC X(4).
  3051     02 EIBSYNRB         PIC X.
  3052     02 EIBNODAT         PIC X.
  3053     02 EIBRESP          PIC S9(8) COMP-5.
  3054     02 EIBRESP2         PIC S9(8) COMP-5.
  3055     02 EIBRLDBK         PIC X.
  3056     02 FILLER           PIC X.
  3057     02 EIBLABEL         PIC S9(4) COMP-5.
  3058     02 EIBGLOBALVARS    POINTER.