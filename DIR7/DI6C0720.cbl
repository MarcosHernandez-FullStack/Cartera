       IDENTIFICATION DIVISION.                                         00000100
      *_______________________                                          00000200
       PROGRAM-ID. DI6C0720.                                            00000300
      *________________________________________________________________ 00000400
      *                                                                 00000500
      *            POSICION DE CARTERA PARA CONTINET - DETALLE          00000600
      *                                                                 00000700
      * SE MODIFICA PARA ATENDER LA INCIDENCIA T-1368, EVERIS T-1368    00000800
      * PBI100030336 @001 23012026 APANTIGOSO AMPLIAR NRO.DOCUMENTO     00000800
      *________________________________________________________________ 00000900
                                                                        00001000
       ENVIRONMENT DIVISION.                                            00001100
      *____________________                                             00001200
       CONFIGURATION SECTION.                                           00001300
                                                                        00001400
       DATA DIVISION.                                                   00001500
      *_____________                                                    00001600
       WORKING-STORAGE SECTION.                                         00001700
      *_______________________                                          00001800
       01 WSV-TALON-N                 PIC 9(08).                        00001810
       01 WSV-NUM-BANCO               PIC X(12).                        00001820
       01 WSV-NUM-BANCO-S             PIC X(10).                        00001830
       01  W-MOV-NUMCLI-E-RDF.                                          00001900
           05  W-MOV-NUMCLI-E-SERIE PIC X(04).                          00002000
           05  W-MOV-NUMCLI-E-CORRE PIC 9(09) COMP-3.                   00002100
           05  W-MOV-DES            PIC 9.                              00002200
                                                                        00002300
       01  W-MOV-NUMCLI-SALIDA      PIC X(12).                          00002400
                                                                        00002500
       01  FILLER             PIC   X(40) VALUE                         00002600
                              '***INICIO WORKING-STORAGE SECTION***'.   00002700
                                                                        00002800
       01  RESPONSE                   PIC S9(09) COMP.                  00002900
       01  RESPONSE-DIDESCOD          PIC S9(09) COMP.                  00003000
                                                                        00003100
       01  WS-DIMAESTR-LENGTH         PIC S9(04) COMP  VALUE +600.      00003200
       01  WS-DIPENDTE-LENGTH         PIC S9(04) COMP  VALUE +350.      00003300
       01  WS-DIDESEMB-LENGTH         PIC S9(04) COMP  VALUE +180.      00003400
                                                                        00003500
       01  WS-IMPRES                  PIC 9(13)V99 COMP-3 VALUE ZEROS.  00003600
       01  WS-IMPRES-ED               PIC 999,999,999.99.               00003700
       01  WS-COD-FECOPE.                                               00003800
           02 WS-CODIGO.                                                00003900
              04 WS-CODOFI            PIC 9(03)        VALUE ZEROS.     00004000
              04 WS-CODMON            PIC 9(01)        VALUE ZEROS.     00004100
              04 WS-CODCLI            PIC 9(06)        VALUE ZEROS.     00004200
              04 WS-CODSER            PIC 9(02)        VALUE ZEROS.     00004300
           02 WS-FECOPE               PIC 9(08)        VALUE ZEROS.     00004400
       01  WS-FIN-DIPENDTE            PIC 9            VALUE ZEROS.     00004500
       01  WS-FIN-DIDESEMB            PIC 9            VALUE ZEROS.     00004600
       01  WS-HAY-DIPENDTE            PIC 9            VALUE ZEROS.     00004700
       01  WS-HAY-DIDESEMB            PIC 9            VALUE ZEROS.     00004800
       01  WS-CODIGO-LEOS             PIC S9(04)  COMP  VALUE ZEROS.    00004900
       01  WS-CODIGO-LEOSX            PIC 9(04)        VALUE ZEROS.     00005000
       01  WS-CODIGO-LEPR             PIC S9(04)  COMP  VALUE ZEROS.    00005100
       01  WS-CODIGO-LEPRX            PIC S9(04)  COMP  VALUE ZEROS.    00005200
       01  I                          PIC S9(04)  COMP  VALUE ZEROS.    00005300
       01  J                          PIC 9(04)        VALUE ZEROS.     00005400
       01  N                          PIC S9(04)   COMP VALUE ZEROS.    00005500
       01  WSV-PCAR-NUMCLI.                                             00005510
           05 WSV-PCAR-NUMCLI-1        PIC X(04) VALUE SPACES.          00005520
           05 WSV-PCAR-NUMCLI-2        PIC 9(08) COMP-3.                00005530
           05 WSV-PCAR-NUMCLI-3        PIC 9(01) VALUE 0.               00005540
       01  WS-IMPORT-TOT              PIC 9(13)V99 COMP-3 VALUE ZEROS.  00005600
       01  WS-NUMDOC-TOT-PR           PIC S9(04)   COMP VALUE ZEROS.    00005700
       01  WS-IMPORT-TOT-PR           PIC 9(13)V99 COMP-3 VALUE ZEROS.  00005800
       01  WS-DCAR-IMPORT             PIC 9(13)V99 COMP-3 VALUE ZEROS.  00005900
       01  WS-DCAR-NUMDOC             PIC 9(05)        VALUE ZEROS.     00006000
       01  WS-CLASRT                  PIC 9(18)        VALUE ZEROS.     00006100
       01  WS-CLASRT-I                PIC S9(04)   COMP VALUE ZEROS.    00006200
       01  WS-DCAR-NRDOCR             PIC S9(05)   COMP VALUE ZEROS.    00006200
       01  WS-DCAR-NRDEVL             PIC S9(05)   COMP VALUE ZEROS.    00006200
                                                                        00006300
       01  W720-SALIDA-S.                                               00006400
           10 W720-DETALLE-S                 OCCURS 8000 TIMES.         00006700
              20 W720-MARSRT-S               PIC X.                     00006900
              20 W720-CLASRT-SS.                                        00007000
                 30 W720-FECVEN-S            PIC 9(08).                 00007100
                 30 W720-NUMBCO-S            PIC 9(10).                 00007200
              20 W720-CLASRT-S REDEFINES W720-CLASRT-SS  PIC 9(18).     00007300
      *@001-I
      *       20 W720-NUMBCOORI-S            PIC X(10).                 00007400
              20 W720-NUMBCOORI-S            PIC X(12).                 00007400
      *@001-F
              20 W720-NOMACE-S               PIC X(40).                 00007500
              20 W720-IMPORT-S               PIC 9(13)V99 COMP-3.       00007600
              20 W720-SITUAC-S               PIC X(03).                 00007700
                                                                        00007800
       01  W720-SALIDA-X.                                               00007900
           10 W720-DETALLE-X                 OCCURS 8000 TIMES.         00008200
              20 W720-MARSRT-X               PIC X.                     00008400
              20 W720-CLASRT-XX.                                        00008500
                 30 W720-FECVEN-X            PIC 9(08).                 00008600
                 30 W720-NUMBCO-X            PIC 9(10).                 00008700
              20 W720-CLASRT-X REDEFINES W720-CLASRT-XX  PIC 9(18).     00008800
      *@001-I
      *       20 W720-NUMBCOORI-X            PIC X(10).                 00008900
              20 W720-NUMBCOORI-X            PIC X(12).                 00008900
      *@001-F
              20 W720-NOMACE-X               PIC X(40).                 00009000
              20 W720-IMPORT-X               PIC 9(13)V99 COMP-3.       00009100
              20 W720-SITUAC-X               PIC X(03).                 00009200
                                                                        00009300
       01 WS-F720-FING-I PIC X(08).
       01 WS-F720-FI REDEFINES WS-F720-FING-I.
          05 WS-FEC-ING-I PIC 9(08).

       01 WS-F720-FING-F PIC X(08).
       01 WS-F720-FF REDEFINES WS-F720-FING-F.
          05 WS-FEC-ING-F PIC 9(08).

      *____MAESTRO     DE CARTERA______________________________________*00009400
           COPY DIMAESTR.                                               00009500
                                                                        00009600
      *____PENDIENTE   DE CARTERA______________________________________*00009700
           COPY DIPENDTE.                                               00009800
                                                                        00009900
      *____DESEMBOLSOS DE CARTERA______________________________________*00010000
           COPY DIDESEMB.                                               00010100
                                                                        00010200
       01  FILLER             PIC   X(40) VALUE                         00010300
                              '***FINAL WORKING-STORAGE SECTION***'.    00010400
                                                                        00010500
       LINKAGE SECTION.                                                 00010600
      *_______________                                                  00010700
       01  DFHCOMMAREA.                                                 00010800
      *    COPY DIWC720.                                                00010900
      *________________________________________________________________*00010910
      *    AREA DE COMUNICACION CON MODULO INTERFASE DE CARTERA        *00010920
      *________________________________________________________________*00010930
      *    DIWC720 AREA DE ENLACE CON EL MODULO DE CONTINET            *00010940
      *            POSICION DE CARTERA                                 *00010950
      *            LONGITUD: 11990                                     *00010960
      *                                                                *00010970
      *          . EL NUMERO TOTAL DE LINEAS DE DETALLE (W720-DETALLE) *00010980
      *            ESTA EN : W720-NUMDOC-TOT                           *00010990
      *                                                                *00010991
      *          . W720-SITUAC = 'ATN' -->  ATENDIDA                   *00010992
      *            W720-SITUAC = 'VIG' -->  VIGENTE                    *00010993
      *                                                                *00010994
      *          . W720-INDIC-PAGINAC = 'S' SI HAY MAS DATOS (OUTPUT)  *00010995
      *            W720-INDIC-PAGINAC = 'N' NO HAY MAS DATOS (OUTPUT)  *00010996
      *            W720-INDIC-PAGINAC = ' ' PRIMERA PAGINA   (INPUT)   *00010997
      *            W720-INDIC-PAGINAC = 'S' SI PAGINACION    (INPUT)   *00010998
      *                                                                *00010999
      *          . W720-FECVEN-PAGINAC=     FECVEN PUNTERO   (OUTPUT)  *00011000
      *            W720-FECVEN-PAGINAC=     FECVEN PUNTERO   (INPUT)   *00011001
      *                                                                *00011002
      *          . W720-NUMBCO-PAGINAC=     NUMBCO PUNTERO   (OUTPUT)  *00011003
      *            W720-NUMBCO-PAGINAC=     NUMBCO PUNTERO   (INPUT)   *00011004
      *________________________________________________________________*00011005
      * W720-DIRCAPLN W720-CODRETOR W720-REFERENC                      *00011006
      * _____________ _____________ _____________                      *00011007
      *               00                                               *00011008
      * DIE7208       08            ERROR CODIGO CARTERA NO NUMERICO   *00011009
      * DIE7210       10            ERROR INDIC  PAGINAC INVALIDO      *00011010
      * DIE7211       11            ERROR FECVEN PAGINAC INVALIDO      *00011011
      * DIE7212       12            ERROR NUMBCO PAGINAC INVALIDO      *00011012
      * DIE7250       50            ERROR POSIC  FECVENNUMBCO INVALIDO *00011013
      * DIE7267       67            ERROR DIMAESTR CLAVE  NO EXISTE    *00011014
      * DIE7268       68            ERROR DIMAESTR CERRADO             *00011015
      * DIE7269       69            ERROR DIMAESTR READ                *00011016
      * DIE7281       81            ERROR DIDESEMB ENDBR               *00011017
      * DIE7286       86            ERROR DIDESEMB READNEXT            *00011018
      * DIE7287       87            ERROR DIDESEMB CLAVE  NO EXISTE    *00011019
      * DIE7288       88            ERROR DIDESEMB CERRADO             *00011020
      * DIE7289       89            ERROR DIDESEMB STARTBR             *00011021
      * DIE7291       91            ERROR DIPENDTE ENDBR               *00011022
      * DIE7296       96            ERROR DIPENDTE READNEXT            *00011023
      * DIE7297       97            ERROR DIPENDTE CLAVE  NO EXISTE    *00011024
      * DIE7298       98            ERROR DIPENDTE CERRADO             *00011025
      * DIE7299       99            ERROR DIPENDTE STARTBR             *00011026
      *________________________________________________________________*00011027
           02 W720-DIWC720.                                             00011028
              05 W720-ENTRADA.                                          00011029
                 10 W720-CODIGO.                                        00011030
                    40 W720-CODOFI                 PIC 9(03).           00011031
                    40 W720-CODMON                 PIC 9(01).           00011032
                    40 W720-CODCLI                 PIC 9(06).           00011033
                    40 W720-CODSER                 PIC 9(02).           00011034
              05 W720-FECRANG.
                 10 W720-BFECHA01                  PIC X(10).
                 10 W720-BFECHA02                  PIC X(10).
      *__________________________________________________12 BYTES_____  00011035
              05 W720-RETORNO.                                          00011036
                 10 W720-CODRETOR                  PIC X(02).           00011037
                 10 W720-REFERENC                  PIC X(30).           00011038
                 10 W720-DIRCAPLN                  PIC X(07).           00011039
      *__________________________________________________39 BYTES_____  00011040
              05 W720-PAGINAC.                                          00011041
                 10 W720-OCURRENCIAS               PIC 9(04).           00011042
                 10 W720-INDIC-PAGINAC             PIC X(01).           00011043
                 10 W720-AREA-PAGINAC.                                  00011044
                    20 W720-FECVEN-PAGINAC         PIC 9(08).           00011045
                    20 W720-NUMBCO-PAGINAC         PIC 9(10).           00011046
      *__________________________________________________23 BYTES_____  00011047
              05 W720-SALIDA.                                           00011048
                 10 W720-TOTALES.                                       00011049
                    20 W720-CODIGO-TOT             PIC 9(12).           00011050
                    20 W720-SERVICIO-TOT           PIC X(20).           00011051
                    20 W720-NUMDOC-TOT             PIC 9(05).           00011052
                    20 W720-IMPORT-TOT             PIC 9(13)V99 COMP-3. 00011053
                    20 W720-NUMDOC-TOT-PR          PIC 9(05).           00011054
                    20 W720-IMPORT-TOT-PR          PIC 9(13)V99 COMP-3. 00011055
                    20 W720-IMPORT-TOT-RYPR        PIC 9(13)V99 COMP-3. 00011056
      *__________________________________________________66 BYTES_____  00011057
                 10 W720-DETALLE                   OCCURS 130 TIMES.    00011059
                    20 W720-FECVEN                 PIC 9(08).           00011060
                    20 W720-NUMBCO                 PIC 9(10).           00011061
      *@001-I
      *             20 W720-NUMBCOORI             PIC X(10) JUST RIGHT. 00011063
                    20 W720-NUMBCOORI             PIC X(12) JUST RIGHT. 00011063
      *@001-F
                    20 W720-NOMACE                 PIC X(40).           00011064
                    20 W720-IMPORT                 PIC 9(13)V99 COMP-3. 00011065
                    20 W720-SITUAC                 PIC X(03).           00011066
      *__________________________________________________79 BYTES_____  00011067
      *_______________________________________________11990 BYTES_____  00011068
                                                                        00011070
       PROCEDURE DIVISION.                                              00011100
      *__________________                                               00011200
                                                                        00011300
       000-INICIO.                                                      00011400
      *__________                                                       00011500
           REPLACE ==PISPLAY== BY ==DISPLAY==.
           PERFORM 100-INICIO-PROGRAMA.                                 00011600
           PERFORM 200-MODULO-CENTRAL.                                  00011700
           PERFORM 900-FIN-PROGRAMA.                                    00011800
                                                                        00011900
       100-INICIO-PROGRAMA.                                             00012000
      *___________________                                              00012100
           MOVE '00'                                  TO W720-CODRETOR. 00012200
           MOVE SPACES                                TO W720-REFERENC. 00012300
           MOVE SPACES                                TO W720-DIRCAPLN. 00012400
           INITIALIZE                                    W720-SALIDA.   00012500
           INITIALIZE                                    W720-SALIDA-S. 00012600
           INITIALIZE                                    W720-SALIDA-X. 00012700
                                                                        00012800
           IF  (W720-CODIGO NOT NUMERIC)                                00012900
                MOVE '08'                             TO W720-CODRETOR  00013000
                MOVE 'ERROR CODIGO NO NUMERICO'       TO W720-REFERENC  00013100
                MOVE 'DIE7208'                        TO W720-DIRCAPLN  00013200
                PERFORM 900-FIN-PROGRAMA                                00013300
           END-IF.                                                      00013400
                                                                        00013500
           IF W720-INDIC-PAGINAC = 'S'
              CONTINUE
           ELSE
              MOVE SPACES             TO W720-INDIC-PAGINAC
           END-IF

           IF  W720-INDIC-PAGINAC = 'S' OR ' '                          00013600
                IF W720-INDIC-PAGINAC = 'S'                             00013700
                   IF W720-FECVEN-PAGINAC > ZEROS                       00013800
                      CONTINUE                                          00013900
                   ELSE                                                 00014000
                      MOVE '00'                  TO W720-CODRETOR       00014100
                      MOVE 'NO HAY MAS REGISTRO' TO W720-REFERENC       00014200
                      MOVE '       '             TO W720-DIRCAPLN       00014400
                      MOVE 'N'                   TO W720-INDIC-PAGINAC
                      PERFORM 900-FIN-PROGRAMA                          00014500
                   END-IF                                               00014600
                   IF W720-NUMBCO-PAGINAC > ZEROS                       00014700
                      CONTINUE                                          00014800
                   ELSE                                                 00014900
                      MOVE '12'                       TO W720-CODRETOR  00015000
                      MOVE 'ERROR NUMBCO PAGINAC INVALIDO'              00015100
                                                      TO W720-REFERENC  00015200
                      MOVE 'DIE7212'                  TO W720-DIRCAPLN  00015300
                      PERFORM 900-FIN-PROGRAMA                          00015400
                   END-IF                                               00015500
                END-IF                                                  00015600
           ELSE                                                         00015700
                MOVE '10'                             TO W720-CODRETOR  00015800
                MOVE 'ERROR INDIC PAGINAC INVALIDO  ' TO W720-REFERENC  00015900
                MOVE 'DIE7210'                        TO W720-DIRCAPLN  00016000
                PERFORM 900-FIN-PROGRAMA                                00016100
           END-IF.                                                      00016200

           MOVE W720-BFECHA01(01:04) TO WS-F720-FING-I(01:04)
           MOVE W720-BFECHA01(05:02) TO WS-F720-FING-I(05:02)
           MOVE W720-BFECHA01(07:02) TO WS-F720-FING-I(07:02)

           MOVE W720-BFECHA02(01:04) TO WS-F720-FING-F(01:04)
           MOVE W720-BFECHA02(05:02) TO WS-F720-FING-F(05:02)
           MOVE W720-BFECHA02(07:02) TO WS-F720-FING-F(07:02)
                                                                        00016300
           MOVE W720-CODIGO                           TO WS-CODIGO.     00016400
                                                                        00016500
           PERFORM 100-OBTIENE-IMPRES.                                  00016600
           PERFORM 110-STARTBR-CODIGO.                                  00016700
                                                                        00016800
       100-OBTIENE-IMPRES.                                              00016900
      *__________________                                               00017000
           MOVE WS-CODIGO                             TO MCAR-CODAPL.   00017100
                                                                        00017200
           EXEC CICS READ      DATASET   ('DIMAESTR')                   00017300
                               RIDFLD    (MCAR-CODAPL)                  00017400
                               KEYLENGTH (+12)                          00017500
                               INTO      (REG-MAESTRO-CARTERA)          00017600
                               LENGTH    (WS-DIMAESTR-LENGTH)           00017700
                               RESP      (RESPONSE)                     00017800
                               NOHANDLE                                 00017900
           END-EXEC.                                                    00018000
                                                                        00018100
           EVALUATE RESPONSE                                            00018200
           WHEN DFHRESP(NORMAL)                                         00018300
           WHEN DFHRESP(LENGERR)                                        00018400
                MOVE MCAR-IMPRES                      TO WS-IMPRES      00018500
           WHEN DFHRESP(NOTFND)                                         00018600
                MOVE '67'                             TO W720-CODRETOR  00018700
                MOVE 'ERROR DIMAESTR CLAVE NO EXI'    TO W720-REFERENC  00018800
                MOVE 'DIE7267'                        TO W720-DIRCAPLN  00018900
                PERFORM 900-FIN-PROGRAMA                                00019000
           WHEN DFHRESP(NOTOPEN)                                        00019100
                MOVE '68'                             TO W720-CODRETOR  00019200
                MOVE 'ERROR DIMAESTR CERRADO     '    TO W720-REFERENC  00019300
                MOVE 'DIE7268'                        TO W720-DIRCAPLN  00019400
                PERFORM 900-FIN-PROGRAMA                                00019500
           WHEN OTHER                                                   00019600
                MOVE '69'                             TO W720-CODRETOR  00019700
                MOVE 'ERROR DIMAESTR READ    '        TO W720-REFERENC  00019800
                MOVE 'DIE7269'                        TO W720-DIRCAPLN  00019900
                PERFORM 900-FIN-PROGRAMA                                00020000
           END-EVALUATE.                                                00020100
                                                                        00020200
       110-STARTBR-CODIGO.                                              00020300
      *__________________                                               00020400
           MOVE WS-CODIGO                             TO PCAR-CODAPL.   00020500

           IF W720-INDIC-PAGINAC = 'S'
              MOVE W720-NUMBCO-PAGINAC             TO PCAR-NUMBCO
           ELSE
              MOVE 0000000000                      TO PCAR-NUMBCO       00020600
           END-IF
                                                                        00020700
           EXEC CICS STARTBR DATASET   ('DIPENDTE')                     00020800
                             RIDFLD    (PCAR-KEYPEN)                    00020900
                             KEYLENGTH (+22)                            00021000
                             RESP      (RESPONSE)                       00021100
                             GTEQ                                       00021200
                             NOHANDLE                                   00021300
           END-EXEC.                                                    00021400
                                                                        00021500
           EVALUATE RESPONSE                                            00021600
           WHEN DFHRESP(NORMAL)                                         00021700
           WHEN DFHRESP(LENGERR)                                        00021800
                CONTINUE                                                00021900
           WHEN DFHRESP(NOTFND)                                         00022000
                CONTINUE                                                00022100
           WHEN DFHRESP(NOTOPEN)                                        00022200
                MOVE '98'                             TO W720-CODRETOR  00022300
                MOVE 'ERROR DIPENDTE CERRADO     '    TO W720-REFERENC  00022400
                MOVE 'DIE7298'                        TO W720-DIRCAPLN  00022500
                PERFORM 900-FIN-PROGRAMA                                00022600
           WHEN OTHER                                                   00022700
                MOVE '99'                             TO W720-CODRETOR  00022800
                MOVE 'ERROR DIPENDTE STARTBR     '    TO W720-REFERENC  00022900
                MOVE 'DIE7299'                        TO W720-DIRCAPLN  00023000
                PERFORM 900-FIN-PROGRAMA                                00023100
           END-EVALUATE.                                                00023200
                                                                        00023300
       200-MODULO-CENTRAL.                                              00023400
      *__________________                                               00023500
           PERFORM 210-OBTIENE-DATOS UNTIL                              00023600
                   WS-CODIGO   NOT = PCAR-CODAPL      OR                00023700
                   WS-FIN-DIPENDTE = 1                OR                00023800
                   N               >= 8000.                             00024100

      *---------------------
           PISPLAY ' N : 'N
      *---------------------                                            00024300
           PERFORM 250-OBTIENE-POR-REGISTRAR.                           00024400
           PERFORM 290-LLENA-SALIDA-TOTALES.                            00024500
                                                                        00024700
           MOVE 1  TO  I                                                00024900
           PERFORM 640-LLENA-SALIDA-DETALLES                            00025000
                                                                        00028000
           EXEC CICS ENDBR    DATASET   ('DIPENDTE')                    00028100
                              RESP      (RESPONSE)                      00028200
                              NOHANDLE                                  00028300
           END-EXEC.                                                    00028400
                                                                        00028500
           EVALUATE RESPONSE                                            00028600
           WHEN DFHRESP(NORMAL)                                         00028700
                CONTINUE                                                00028800
           WHEN OTHER                                                   00028900
                MOVE '91'                             TO W720-CODRETOR  00029000
                MOVE 'ERROR DIPENDTE ENDBR        '   TO W720-REFERENC  00029100
                MOVE 'DIE7291'                        TO W720-DIRCAPLN  00029200
                PERFORM 900-FIN-PROGRAMA                                00029300
           END-EVALUATE.                                                00029400
                                                                        00029500
       210-OBTIENE-DATOS.                                               00029600
      *_________________                                                00029700
           PERFORM 220-LEER-NEXT.                                       00029800
           IF WS-FIN-DIPENDTE = 0                                       00029900
              IF  WS-CODIGO = PCAR-CODAPL                               00030000
                  IF    PCAR-FECING >= WS-F720-FING-I
                    AND PCAR-FECING <= WS-F720-FING-F
                      PERFORM 240-LLENA-TEMPORAL-DETALLE                00030100
                  ELSE
                    CONTINUE
                  END-IF
              ELSE                                                      00030200
                  MOVE 1                              TO WS-FIN-DIPENDTE00030300
                  MOVE 'CNE0141'                      TO W720-DIRCAPLN  00030400
              END-IF                                                    00030500
           END-IF.                                                      00030600
                                                                        00030700
       220-LEER-NEXT.                                                   00030800
      *_____________                                                    00030900
           EXEC CICS READNEXT  DATASET   ('DIPENDTE')                   00031000
                               RIDFLD    (PCAR-KEYPEN)                  00031100
                               INTO      (REG-PENDIENTE-CARTERA)        00031200
                               RESP      (RESPONSE)                     00031300
                               NOHANDLE                                 00031400
           END-EXEC.                                                    00031500
                                                                        00031600
           EVALUATE RESPONSE                                            00031700
           WHEN DFHRESP(NORMAL)                                         00031800
           WHEN DFHRESP(LENGERR)                                        00031900
                IF WS-CODIGO = PCAR-CODAPL                              00032000
                   ADD  1                             TO WS-CODIGO-LEOS 00032100
                   MOVE 1                             TO WS-HAY-DIPENDTE00032200
                ELSE                                                    00032300
                   MOVE 1                             TO WS-FIN-DIPENDTE00032400
                   MOVE 'CNE0141'                     TO W720-DIRCAPLN  00032500
                   IF WS-CODIGO-LEOS = 0                                00032600
                      MOVE '97'                       TO W720-CODRETOR  00032700
                      MOVE 'ERROR DIPENDTE CLAVE NO EXI'                00032800
                                                      TO W720-REFERENC  00032900
                      MOVE 'DIE7297'                  TO W720-DIRCAPLN  00033000
                      PERFORM 900-FIN-PROGRAMA                          00033100
                   END-IF                                               00033200
                END-IF                                                  00033300
           WHEN DFHRESP(ENDFILE)                                        00033400
                MOVE 1                                TO WS-FIN-DIPENDTE00033500
                MOVE 'CNE0141'                        TO W720-DIRCAPLN  00033600
           WHEN OTHER                                                   00033700
                MOVE '96'                             TO W720-CODRETOR  00033800
                MOVE 'ERROR DIPENDTE READNEXT'        TO W720-REFERENC  00033900
                MOVE 'DIE7296'                        TO W720-DIRCAPLN  00034000
                PERFORM 900-FIN-PROGRAMA                                00034100
           END-EVALUATE.                                                00034200
                                                                        00034300
       240-LLENA-TEMPORAL-DETALLE.                                      00034400
      *__________________________                                       00034500
           ADD  1                                TO N.                  00034600
           MOVE PCAR-FECVEN                      TO W720-FECVEN-S(N).   00034700
           MOVE PCAR-NUMBCO                      TO W720-NUMBCO-S(N).   00034800
           IF PCAR-TDOCUM = 14                                          00035000
              MOVE PCAR-NUMCLI       TO WSV-PCAR-NUMCLI                 00035010
              MOVE WSV-PCAR-NUMCLI-1 TO WSV-NUM-BANCO(1:4)              00035020
              MOVE WSV-PCAR-NUMCLI-2 TO WSV-TALON-N                     00035030
              IF  WSV-PCAR-NUMCLI-3 IS NUMERIC AND                      00035040
                  WSV-PCAR-NUMCLI-3 IS GREATER THAN ZEROS               00035050
                  MOVE WSV-TALON-N(1:WSV-PCAR-NUMCLI-3)                 00035060
                                        TO WSV-NUM-BANCO(5:8)           00035070
                  MOVE WSV-NUM-BANCO    TO WSV-NUM-BANCO-S(1:10)        00035071
                  MOVE WSV-NUM-BANCO-S  TO W720-NUMBCOORI-S(N)          00035072
              ELSE                                                      00035080
                  MOVE WSV-TALON-N(1:8)                                 00035090
                                        TO WSV-NUM-BANCO(5:8)           00035091
      *@001-I
      *                                    W720-NUMBCOORI-S(N)(5:6)     00035092
                                           W720-NUMBCOORI-S(N)(5:8)     00035092
      *@001-F
                  MOVE PCAR-NUMCLI      TO W720-NUMBCOORI-S(N)(1:4)     00035093
              END-IF                                                    00035094
           ELSE                                                         00035500
              MOVE PCAR-NUMCLI               TO W-MOV-NUMCLI-SALIDA     00035610
              MOVE W-MOV-NUMCLI-SALIDA       TO W720-NUMBCOORI-S(N)     00035620
           END-IF                                                       00035700
           MOVE PCAR-NOMGIR                      TO W720-NOMACE-S(N).   00035800
           MOVE PCAR-IMPACT                      TO W720-IMPORT-S(N).   00035900
           ADD  PCAR-IMPACT                      TO WS-IMPORT-TOT.      00036000
      *_________DOCUMENTO CANCELADO O RENOVADO______________________    00036100
      *    IF   PCAR-SWCANC = 1 OR PCAR-RENOVA > 0                      00036200
      *_________DOCUMENTO CANCELADO EN EL DIA_______________________    00036300
           IF   PCAR-SWCANC = 1                                         00036400
                MOVE 'ATN'                       TO W720-SITUAC-S(N)    00036500
           ELSE                                                         00036600
                MOVE 'VIG'                       TO W720-SITUAC-S(N)    00036700
           END-IF.                                                      00036800
                                                                        00036900
       250-OBTIENE-POR-REGISTRAR.                                       00037000
      *_________________________                                        00037100
           MOVE WS-CODIGO                        TO DCAR-CODAPL.        00037200
                                                                        00037300
           EXEC CICS STARTBR DATASET   ('DIDESCOD')                     00037400
                             RIDFLD    (DCAR-CODAPL)                    00037500
                             KEYLENGTH (+12)                            00037600
                             RESP      (RESPONSE-DIDESCOD)              00037700
                             GTEQ                                       00037800
                             NOHANDLE                                   00037900
           END-EXEC.                                                    00038000
                                                                        00038100
           EVALUATE RESPONSE-DIDESCOD                                   00038200
           WHEN DFHRESP(NORMAL)                                         00038300
           WHEN DFHRESP(LENGERR)                                        00038400
                CONTINUE                                                00038500
           WHEN DFHRESP(NOTFND)                                         00038600
                CONTINUE                                                00038700
           WHEN DFHRESP(NOTOPEN)                                        00038800
                MOVE '88'                             TO W720-CODRETOR  00038900
                MOVE 'ERROR DIDESEMB CERRADO '        TO W720-REFERENC  00039000
                MOVE 'DIE7288'                        TO W720-DIRCAPLN  00039100
                PERFORM 900-FIN-PROGRAMA                                00039200
           WHEN OTHER                                                   00039300
                MOVE '89'                             TO W720-CODRETOR  00039400
                MOVE 'ERROR DIDESEMB STARTBR EQUAL  ' TO W720-REFERENC  00039500
                MOVE 'DIE7289'                        TO W720-DIRCAPLN  00039600
                PERFORM 900-FIN-PROGRAMA                                00039700
           END-EVALUATE.                                                00039800
                                                                        00039900
           PERFORM 260-OBTIENE-PR  UNTIL                                00040000
                   WS-FIN-DIDESEMB = 1                OR                00040100
                   WS-CODIGO   NOT = DCAR-CODAPL.                       00040200
                                                                        00040300
           EXEC CICS ENDBR   DATASET ('DIDESCOD')                       00040400
                             RESP    (RESPONSE-DIDESCOD)                00040500
                             NOHANDLE                                   00040600
           END-EXEC.                                                    00040700
                                                                        00040800
           EVALUATE RESPONSE-DIDESCOD                                   00040900
           WHEN DFHRESP(NORMAL)                                         00041000
                CONTINUE                                                00041100
           WHEN OTHER                                                   00041200
                MOVE '81'                             TO W720-CODRETOR  00041300
                MOVE 'ERROR DIDESEMB ENDBR   '        TO W720-REFERENC  00041400
                MOVE 'DIE7281'                        TO W720-DIRCAPLN  00041500
                PERFORM 900-FIN-PROGRAMA                                00041600
           END-EVALUATE.                                                00041700
                                                                        00041800
       260-OBTIENE-PR.                                                  00041900
      *______________                                                   00042000
           PERFORM 270-LEER-NEXT-PR.                                    00042100
           IF WS-FIN-DIDESEMB = 0                                       00042200
              IF  WS-CODIGO = DCAR-CODAPL                               00042300
                  IF DCAR-SITUAC = 1 OR 2 OR 3                          00042400
                     PERFORM 280-ACUMULA-DIDESEMB                       00042500
                  END-IF                                                00042600
              ELSE                                                      00042700
                  MOVE 1 TO WS-FIN-DIDESEMB                             00042800
              END-IF                                                    00042900
           END-IF.                                                      00043000
                                                                        00043100
       270-LEER-NEXT-PR.                                                00043200
      *________________                                                 00043300
           EXEC CICS READNEXT  DATASET   ('DIDESCOD')                   00043400
                               RIDFLD    (DCAR-CODAPL)                  00043500
                               INTO      (REG-DEVOLUCION-CARTERA)       00043600
                               RESP      (RESPONSE-DIDESCOD)            00043700
                               NOHANDLE                                 00043800
           END-EXEC                                                     00043900
                                                                        00044000
           EVALUATE RESPONSE-DIDESCOD                                   00044100
           WHEN DFHRESP(NORMAL)                                         00044200
           WHEN DFHRESP(LENGERR)                                        00044300
           WHEN 000000015                                               00044400
                ADD  1                                TO WS-CODIGO-LEPRX00044500
           WHEN DFHRESP(ENDFILE)                                        00044600
                MOVE 1                                TO WS-FIN-DIDESEMB00044700
           WHEN OTHER                                                   00044800
                MOVE '86'                             TO W720-CODRETOR  00044900
                MOVE 'ERROR DIDESEMB READNEXT '       TO W720-REFERENC  00045000
                MOVE 'DIE7286'                        TO W720-DIRCAPLN  00045100
                PERFORM 900-FIN-PROGRAMA                                00045200
           END-EVALUATE.                                                00045300
                                                                        00045400
       280-ACUMULA-DIDESEMB.                                            00045500
      *____________________                                             00045600
           ADD  1                                 TO WS-CODIGO-LEPR.    00045700
           MOVE 1                                 TO WS-HAY-DIDESEMB.   00045800
           MOVE ZEROS         TO WS-DCAR-NUMDOC      WS-DCAR-IMPORT.    00045900
           MOVE ZEROES                            TO WS-DCAR-NRDOCR
           MOVE ZEROES                            TO WS-DCAR-NRDEVL
           MOVE DCAR-NRDOCR                       TO WS-DCAR-NRDOCR     00045900
           MOVE DCAR-NRDEVL                       TO WS-DCAR-NRDEVL     00045900
           COMPUTE WS-DCAR-NUMDOC = WS-DCAR-NRDOCR - WS-DCAR-NRDEVL.    00046000
           COMPUTE WS-DCAR-IMPORT = DCAR-IMPORT -       DCAR-IMPDVL.    00046100
           ADD     WS-DCAR-NUMDOC                 TO WS-NUMDOC-TOT-PR.  00046200
           ADD     WS-DCAR-IMPORT                 TO WS-IMPORT-TOT-PR.  00046300
                                                                        00046400
       290-LLENA-SALIDA-TOTALES.                                        00046500
      *________________________                                         00046600
           MOVE WS-CODIGO                         TO W720-CODIGO-TOT    00046700
           EVALUATE WS-CODSER                                           00046800
               WHEN 01 MOVE 'COBRANZA LIBRE'      TO W720-SERVICIO-TOT  00046900
               WHEN 07                                                  00047000
               WHEN 08                                                  00047100
               WHEN 13 MOVE 'COBRANZA GARANTIA'   TO W720-SERVICIO-TOT  00047200
               WHEN 50 MOVE 'DESCUENTO DE LETRAS' TO W720-SERVICIO-TOT  00047300
           END-EVALUATE.                                                00047400
           MOVE N                                 TO W720-NUMDOC-TOT    00047500
           MOVE WS-IMPRES                         TO W720-IMPORT-TOT.   00047600
           MOVE WS-NUMDOC-TOT-PR                  TO W720-NUMDOC-TOT-PR.00047800
           MOVE WS-IMPORT-TOT-PR                  TO W720-IMPORT-TOT-PR.00047900
                                                                        00048000
           ADD  WS-IMPORT-TOT WS-IMPORT-TOT-PR                          00048100
                                        GIVING    W720-IMPORT-TOT-RYPR. 00048200
                                                                        00052900
       640-LLENA-SALIDA-DETALLES.                                       00053000
      *_________________________                                        00053100
           PERFORM  TEST BEFORE VARYING J FROM 1 BY 1         UNTIL     00053200
                                     (J > 130)                OR        00053400
                                      I > N                   OR        00053500
                                      I > 8000                          00053800
              MOVE  W720-FECVEN-S (I)             TO W720-FECVEN (J)    00054200
              MOVE  W720-NUMBCO-S (I)             TO W720-NUMBCO (J)    00054300
              MOVE  W720-NUMBCOORI-S (I)          TO W720-NUMBCOORI (J) 00054400
              MOVE  W720-NOMACE-S (I)             TO W720-NOMACE (J)    00054500
              MOVE  W720-IMPORT-S (I)             TO W720-IMPORT (J)    00054600
              MOVE  W720-SITUAC-S (I)             TO W720-SITUAC (J)    00054700
              ADD   1                             TO I                  00054800
           END-PERFORM.                                                 00054900
                                                                        00055000
           IF J > 130                                                   00055100
              MOVE 'S'                            TO W720-INDIC-PAGINAC 00055300
              MOVE  W720-FECVEN-S (I)             TO W720-FECVEN-PAGINAC00055400
              MOVE  W720-NUMBCO-S (I)             TO W720-NUMBCO-PAGINAC00055500
      *-->SE FUERZA SALIDA DE TRAMA
              IF W720-FECVEN-S (I) = ZEROS                              00055400
                 MOVE 'N'                         TO W720-INDIC-PAGINAC 00055800
              END-IF
      *-->
           ELSE                                                         00055600
              MOVE  N                             TO W720-OCURRENCIAS   00055700
              MOVE 'N'                            TO W720-INDIC-PAGINAC 00055800
              INITIALIZE                             W720-AREA-PAGINAC  00055900
           END-IF.                                                      00056000
                                                                        00056100
       900-FIN-PROGRAMA.                                                00056200
      *________________                                                 00056300

           EXEC  CICS  RETURN  END-EXEC.                                00056400
