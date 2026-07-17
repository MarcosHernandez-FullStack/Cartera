      *--------------------------------------------------------------*
      *         DI2CDIR2 : RELACION DE OPERACIONES                   *
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *                IDENTIFICATION DIVISION                       *
      *--------------------------------------------------------------*
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. DI2CDIR2.
       AUTHOR.       STEFANINI PERU.
       DATE-WRITTEN. 26/10/2009.
      *
      ******************************************************************
      *        L O G    D E    M O D I F I C A C I O N E S             *
      ******************************************************************
      *  AUTOR   FECHA          DESCRIPCION                            *
      * ------- ------- ---------------------------------------------- *
      ******************************************************************
      *--------------------------------------------------------------*
      * CLON CN1CFN3M
      *--------------------------------------------------------------*
      *                 ENVIRONMENT DIVISION                         *
      *--------------------------------------------------------------*
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-3090.
       OBJECT-COMPUTER.
           IBM-3090.
      *
      *--------------------------------------------------------------*
      *                  DATA DIVISION                               *
      *--------------------------------------------------------------*
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
      *         IDENTIFICADOR DE INICIO DE WORKING STORAGE           *
      *
       01 LT-INICIO-WS.
           05 LT-INICIO-W                    PIC X(50) VALUE
                   '*** INICIO WORKING STORAGE (DI2CDIR2) ***'.
      *
       01  WSV-VARIABLES.                                               01060000
           05 WSV-SALIDA-TS.                                            01100000
              10 WSV-TS-FORMATO        PIC X(08)  VALUE SPACES.
              10 WSV-TS-DETALLE        PIC X(100) VALUE SPACES.
           05 WS-RESPUESTA             PIC S9(8)  COMP.
           05 WS-RESPUESTA-N           PIC S9(10) VALUE ZEROES.
           05 WS-ITEMS-TOT             PIC 9(04)  VALUE ZEROES.
           05 WS-IE09                  PIC 9(04)  VALUE ZEROES.
           05 WS-FECHA-8               PIC 9(08).
           05 WS-FECHA-10              PIC X(10).
      *--------------------------------------------------------------*
      *                       CONSTANTES                             *
      *--------------------------------------------------------------*
      *
       01  RUT-RUTINAS.
           05 RUT-KN1CC260                PIC X(08) VALUE  'KN1CC260'.
           05 RUT-DI7C0081                PIC X(08) VALUE 'DI7C0081'.
           05 RUT-DI7C0091                PIC X(08) VALUE 'DI7C0091'.
           05 RUT-KN1CC300                PIC X(08) VALUE  'KN1CC300'.
       01  PGM.
           05 PGM-PROGRAMA                PIC X(08) VALUE  'DI2CDIR2'.
       01  WS-RETORNO.
           05 CON-RETORNO-OK              PIC X(02) VALUE '00'.
           05 CON-RETORNO-GRAVE           PIC X(02) VALUE '98'.
           05 CON-RETORNO-NOK             PIC X(02) VALUE '99'.
       01  WS-CONSTANTES.
           05 CON-C                       PIC X(01) VALUE  'C'.
           05 CON-D                       PIC X(01) VALUE  'D'.
           05 CON-ACEPTADA                PIC X(01) VALUE 'S'.
           05 CON-NO-ACEPTADA             PIC X(01) VALUE 'N'.
           05 CON-SIN-ROLLBACK            PIC X(02) VALUE '01'.
           05 CON-CON-ROLLBACK            PIC X(02) VALUE '02'.
           05 CON-202S                    PIC X(04) VALUE '202S'.
           05 CON-201S                    PIC X(04) VALUE '201S'.
           05 CON-COPYS.                                                00593002
              10 WSC-DIECBSS2             PIC X(08) VALUE 'DIECBSS2'.   00600003
              10 WSC-DIECBDS1             PIC X(08) VALUE 'DIECBDS1'.   00600003
      *
      *--------------------------------------------------------------*
      *                 S W I T C H E S                              *
      *--------------------------------------------------------------*
      *
       01  SW-SWITCH.
           05 SW-OCCURS                  PIC X(01) VALUE 'N'.
             88 SW-NO-FIN-OCCURS        VALUE 'N'.
             88 SW-FIN-OCCURS           VALUE 'S'.
           05 SW-MSG                     PIC X(01) VALUE 'N'.
             88 SW-NO-SIZE-MSG          VALUE 'N'.
             88 SW-SIZE-MSG             VALUE 'S'.
      *
      *--------------------------------------------------------------*
      *                 DEFINICION DE VARIABLES                      *
      *--------------------------------------------------------------*
      *
       01  WS-VARIABLES.
           05 WS-DET-TS.
              10 TS-NOMBRE-DET              PIC X(03) VALUE  '&01'.
              10 TS-SUFIFO.
                 15 TS-SUFIJO-DET           PIC 9(05) VALUE  ZEROES.
           05 WS-TSNAME.                                                01200000
              10 WSV-TS-NOMBRE         PIC X(04)       VALUE SPACES.    01210003
              10 WSV-TS-SUFIJO         PIC X(04)       VALUE SPACES.    01220003
           05 WS-OBJETO                PIC X(08) VALUE  SPACES.
           05 WS-ACUMDET               PIC 9(04) VALUE  ZEROES.
           05 WS-TSLENGTH              PIC S9(04) COMP.
           05 WS-DIR2-IMP-N            PIC S9(15)V9(02) VALUE ZEROES.
           05 WS-DIR2-IMP-A REDEFINES  WS-DIR2-IMP-N
                                       PIC X(17).
           05 WSC-QG1CABC              PIC X(07)       VALUE 'QG1CABC'.
      *
      *--------------------------------------------------------------*
      *                COPYS AREAS DE TRABAJO                        *
      *--------------------------------------------------------------*
      *
      *-----------  COPY AREA DE TRABAJO DE LA RUTINA DI7C0081         *
       01 WS-DIWC0081.
           COPY DIWC0081.
      *
      *-----------  COPY AREA DE TRABAJO DE LA RUTINA DI7C0091
      *
       01 WS-DIWC0091.
           COPY DIWC0091.
      *
      *-----------  COPY AREA DE TRABAJO DE COLECTOR KN1CC260          *
       01  REG-C26-KNDCC260.                                            01350000
           COPY KNDCC261.                                               01380000
      *                                                                 01390003
      *----------------------------------------------------------------*01640000
      *             COPY AREA DE TRABAJO DE COLECTOR KN1CC300          *01650000
      *----------------------------------------------------------------*01660000
       01  REG-C30-KNDCC300.                                            01670000
           COPY KNDCC301.                                               01700000

      *------  FORMATO DE SALIDA DE LA FUNCION KW3M                    *
           COPY DIECBSS2.                                               01850004
           COPY DIECBDS1.                                               01850004

      *                COPY INFORMACION DE ERRORES                     *
      *--------------------------------------------------------------- *
       01  QGECABC-01.
           COPY QGECABC.

      ******************************************************************
      *        INCLUDE DE SQLCA PARA FORZAR COMPILACION CON DB2        *
      ******************************************************************
      *
       01 LT-FINAL-WS.
           05 LT-FINAL-W                     PIC X(40) VALUE
                   '*** FINAL WORKING STORAGE  (DI2CDIR2) **'.
      *
      *--------------------------------------------------------------*
      *                   LINKAGE SECTION                            *
      *--------------------------------------------------------------*
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA.
      *    COPY KNECCIC3.                                               02000000
           COPY QGECCAA.
           COPY DIECDIR2.                                               02010000
      *
      *--------------------------------------------------------------*
      *                PROCEDURE DIVISION                            *
      *--------------------------------------------------------------*
      *
       PROCEDURE DIVISION.
      *
           PERFORM  1000-INICIO.
      *
           PERFORM  2000-PROCESO.
      *
           PERFORM  3000-FIN.
      *
      *--------------------------------------------------------------*
      *                 1000 INICIO                                  *
      *--------------------------------------------------------------*
      *
       1000-INICIO.
      *
           EXEC CICS
              IGNORE CONDITION ERROR
           END-EXEC
      *
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *       MOVE CON-SIN-ROLLBACK         TO CAA-CNYRELCE
              MOVE 'IGNORE'                 TO CAA-REFER-AUTO
              MOVE PGM-PROGRAMA             TO CAA-OBJETO-ERROR
      *                                        CAA-PROGRAMA-ERR
              PERFORM 4100-MOVER-ERRORES-CICS
              PERFORM 4500-FIN-ANORMAL
              PERFORM 3000-FIN
           END-IF
      *
           SET ADDRESS OF REG-DIECDIR2    TO CAA-PTR-COPYIN             02300000
                                                                        02320000
           INITIALIZE C26-KNDCC260                                      02330000
                      C30-KNDCC300                                      02350000
      *               CAA-ERRORES                                       02360000
                      WSV-VARIABLES
                                                                        02650000
           MOVE '+DC1'                     TO WSV-TS-NOMBRE             02660000
           MOVE CAA-TERMINAL             TO WSV-TS-SUFIJO               02670000
           MOVE CON-ACEPTADA               TO C30-IND-ACEPTADA          02680000
      *
           PERFORM 1001-0000-BORRAR-COLAS-TS.
      *
      *--------------------------------------------------------------*
      *                1001 0000 BORRAR COLAS TS                     *
      *--------------------------------------------------------------*
      *
       1001-0000-BORRAR-COLAS-TS.
      *
           EXEC CICS
              DELETEQ TS QUEUE(WS-TSNAME)
              NOHANDLE
           END-EXEC

           IF EIBRESP EQUAL DFHRESP(NORMAL) OR +44
              CONTINUE
           ELSE
      *       MOVE CON-SIN-ROLLBACK    TO CAA-CNYRELCE
              MOVE WSV-SALIDA-TS       TO CAA-REFER-AUTO
              MOVE WS-TSNAME           TO CAA-OBJETO-ERROR
              PERFORM 4100-MOVER-ERRORES-CICS
              PERFORM 4500-FIN-ANORMAL
              PERFORM 3000-FIN
           END-IF.
      *
      *--------------------------------------------------------------*
      *                   2000 PROCESO                               *
      *--------------------------------------------------------------*
      *
       2000-PROCESO.
      *
      *    PERFORM ASTI-0000-PREPROCESO-AST.
      *
           PERFORM FNEG-0000-PROCESO-DE-NEGOCIO.
      *
      *    PERFORM ASTF-0000-FIN-PROCESO-AST.
      *
      *--------------------------------------------------------------*
      *                ASTI 0000 PREPROCESO AST                      *
      *--------------------------------------------------------------*
      *
      * ASTI-0000-PREPROCESO-AST.
      *
      *     PERFORM ASTI-1000-VALIDA-ACCES-FUNCION.
      *
      *--------------------------------------------------------------*
      *              ASTI 1000 VALIDA ACCES FUNCION                  *
      *--------------------------------------------------------------*
      *
      * ASTI-1000-VALIDA-ACCES-FUNCION.
      *
      *    MOVE CAA-CNFUNCID              TO C26-COD-FUNCION.
      *    SET  C26-PTR-COPYIN              TO CAA-PTR-APL-AST.
      *    MOVE SPACES                      TO C26-ASUNTO-PROPIO.
      *    MOVE SPACES                      TO C26-ASUNTO-ASOCIADO.
      *
      *    EXEC CICS
      *     LINK PROGRAM(RUT-KN1CC260)
      *      COMMAREA    (C26-KNDCC260)                                 03350000
      *      LENGTH      (LENGTH OF REG-C26-KNDCC260)                   03360000
      *     END-EXEC
      *
      *    IF EIBRESP EQUAL DFHRESP(NORMAL)
      *       IF C26-COD-RESPUESTA EQUAL CON-RETORNO-OK
      *         MOVE C26-OPERACION          TO CAA-CNNOPERA
      *        ELSE
      *         MOVE CON-SIN-ROLLBACK       TO CAA-CNYRELCE
      *         MOVE SPACES                 TO CAA-ABEND-CODE
      *         MOVE C26-PROGRAMA           TO CAA-PROGRAMA-ERR
      *         MOVE C26-REFERENCIA         TO CAA-REFERENCIA-ERR
      *         MOVE C26-OBJETO             TO CAA-OBJETO-ERR
      *         MOVE C26-SQLCODE            TO CAA-SQLCODE-ERR
      *         MOVE C26-SQLERRM            TO CAA-SQLERRM-ERR
      *         MOVE C26-EIBFN              TO CAA-EIBFN
      *         MOVE C26-EIBRSRCE           TO CAA-EIBRSRCE
      *         MOVE C26-EIBRCODE           TO CAA-EIBRCODE
      *         MOVE C26-EIBRESP1           TO CAA-EIBRESP1
      *         MOVE C26-EIBRESP2           TO CAA-EIBRESP2
      *         MOVE C26-OPERACION          TO CAA-CNNOPERA
      *         MOVE C26-COD-ERROR          TO CAA-CNRCAPLN
      *         MOVE C26-VALOR-ERROR-UNO    TO CAA-CNCOVAR1
      *         MOVE C26-VALOR-ERROR-DOS    TO CAA-CNCOVAR2
      *         PERFORM 4500-FIN-ANORMAL
      *         PERFORM 3000-FIN
      *       END-IF
      *    ELSE
      *       MOVE CON-SIN-ROLLBACK         TO CAA-CNYRELCE
      *       MOVE C26-KNDCC260             TO CAA-REFERENCIA-ERR
      *       MOVE RUT-KN1CC260             TO CAA-OBJETO-ERR
      *                                       CAA-PROGRAMA-ERR
      *       PERFORM 4100-MOVER-ERRORES-CICS
      *       PERFORM 4500-FIN-ANORMAL
      *       PERFORM 3000-FIN
      *    END-IF.
      *
      *--------------------------------------------------------------*
      *                FNEG 0000 PROCESO DE NEGOCIO                  *
      *--------------------------------------------------------------*
      *
       FNEG-0000-PROCESO-DE-NEGOCIO.
      *
           SET SW-NO-FIN-OCCURS             TO TRUE

           IF DIR2-INDAUX1 = CON-C
               PERFORM FNEG-1000-RELAC-COBRANZA
           ELSE
               IF DIR2-INDAUX1 = CON-D
                   PERFORM FNEG-2000-RELAC-DESCUENTO
               END-IF
           END-IF.
      *
      *--------------------------------------------------------------*
      *                 FNEG 1000 RELAC COBRANZA                     *
      *--------------------------------------------------------------*
      *
       FNEG-1000-RELAC-COBRANZA.
      *
           INITIALIZE W0081-DIWC0081

           MOVE DIR2-CODCTAA(1:12)    TO   W0081-CODIGO
           MOVE DIR2-INDPAGI          TO   W0081-INDPAGI
           MOVE DIR2-PAGINAC          TO   W0081-PAGINAC
           MOVE DIR2-PAGSIZE          TO   W0081-PAGSIZE
      *
           EXEC CICS
              LINK PROGRAM(RUT-DI7C0081)
              COMMAREA(W0081-DIWC0081)
              RESP(WS-RESPUESTA)
           END-EXEC
      *
           IF WS-RESPUESTA NOT EQUAL DFHRESP(NORMAL)
      *    AND +44
      *       MOVE  RUT-DI7C0081       TO CAA-PROGRAMA-ERR
              MOVE  W0081-REFERENC      TO CAA-REFER-AUTO
      *       MOVE  CON-SIN-ROLLBACK   TO CAA-CNYRELCE
              MOVE  WS-RESPUESTA       TO WS-RESPUESTA-N
      *       MOVE  W0081-DIRCAPLN      TO CAA-CNRCAPLN
              MOVE  W0081-CODRETOR      TO CAA-VAR1-AVISO1
              MOVE  ' EIBRESP NO OK '  TO CAA-VAR1-AVISO2
              PERFORM 4100-MOVER-ERRORES-CICS
              PERFORM 4500-FIN-ANORMAL
              PERFORM 3000-FIN
           ELSE
              INITIALIZE REG-DIECBSS2  WSV-SALIDA-TS
      *       MOVE '202S'                     TO CAA-ID-FORMATO01
              MOVE 1                          TO CAA-NUM-ITEM-SELEC
      *       MOVE LENGTH OF REG-DIECBSS2     TO CAA-LGTH-FORMATO01
              MOVE W0081-CODIGO-TIT            TO BSS2-CODCARTERA
              MOVE W0081-SERVICIO-TIT          TO BSS2-SERVICIO
              MOVE W0081-MONEDA-TIT            TO BSS2-DIVISA
              MOVE W0081-INDPAGI               TO BSS2-INDPAGI
              MOVE W0081-PAGINAC               TO BSS2-PAGINAC
              MOVE WSC-DIECBSS2               TO WSV-TS-FORMATO         05582104
              MOVE REG-DIECBSS2               TO WSV-TS-DETALLE         05582120
              MOVE LENGTH OF REG-DIECBSS2     TO WS-TSLENGTH            05582130
              PERFORM 200222-WRITEQ-TS                                  05582140
              MOVE 0                          TO WS-ITEMS-TOT
              MOVE 1                          TO WS-IE09
              PERFORM FNEG-1100-LEE-OCCURS-COB UNTIL SW-FIN-OCCURS
           END-IF.
      *
      *--------------------------------------------------------------*
      *                    FNEG-1100-LEE-OCCURS-COB                  *
      *--------------------------------------------------------------*
      *
       FNEG-1100-LEE-OCCURS-COB.
      *-------------------------*
           IF WS-IE09 <= W0081-N
             INITIALIZE REG-DIECBDS1  WSV-SALIDA-TS
             MOVE WSC-DIECBDS1             TO WSV-TS-FORMATO            05582104
             ADD  1                        TO WS-ITEMS-TOT
             MOVE W0081-FECOPE(WS-IE09)     TO WS-FECHA-8
             MOVE WS-FECHA-8(1:4)          TO WS-FECHA-10(1:4)
             MOVE '-'                      TO WS-FECHA-10(5:1)
             MOVE WS-FECHA-8(5:2)          TO WS-FECHA-10(6:2)
             MOVE '-'                      TO WS-FECHA-10(8:1)
             MOVE WS-FECHA-8(7:2)          TO WS-FECHA-10(9:2)
             MOVE WS-FECHA-10              TO BDS1-FECOPER
             MOVE W0081-CARGOS(WS-IE09)     TO BDS1-TOTCAR
             MOVE W0081-ABONOS(WS-IE09)     TO BDS1-TOTABO
             ADD  1                        TO WS-IE09
             MOVE REG-DIECBDS1             TO WSV-TS-DETALLE            05584504
             MOVE LENGTH OF REG-DIECBDS1   TO WS-TSLENGTH               05584803
             PERFORM 200222-WRITEQ-TS                                   05585003
           ELSE
             SET SW-FIN-OCCURS             TO TRUE
           END-IF.
      *
      *--------------------------------------------------------------*
      *                 FNEG 2000 RELAC DESCUENTO                    *
      *--------------------------------------------------------------*
      *
       FNEG-2000-RELAC-DESCUENTO.
      *
           INITIALIZE W0091-DIWC0091

           MOVE DIR2-CODCTAA(1:12)    TO   W0091-CODIGO
           MOVE DIR2-INDPAGI          TO   W0091-INDPAGI
           MOVE DIR2-PAGINAC          TO   W0091-PAGINAC
           MOVE DIR2-PAGSIZE          TO   W0091-PAGSIZE
      *
           EXEC CICS
              LINK PROGRAM(RUT-DI7C0091)
              COMMAREA(W0091-DIWC0091)
              RESP(WS-RESPUESTA)
           END-EXEC
      *
           IF WS-RESPUESTA NOT EQUAL DFHRESP(NORMAL)
      *    AND +44
      *       MOVE  RUT-DI7C0091       TO CAA-PROGRAMA-ERR
              MOVE  W0091-REFERENC      TO CAA-REFER-AUTO
      *       MOVE  CON-SIN-ROLLBACK   TO CAA-CNYRELCE
              MOVE  WS-RESPUESTA       TO WS-RESPUESTA-N
      *       MOVE  W0091-DIRCAPLN      TO CAA-CNRCAPLN
              MOVE  W0091-CODRETOR      TO CAA-VAR1-AVISO1
              MOVE  ' EIBRESP NO OK '  TO CAA-VAR1-AVISO2
              PERFORM 4100-MOVER-ERRORES-CICS
              PERFORM 4500-FIN-ANORMAL
              PERFORM 3000-FIN
           ELSE
              INITIALIZE REG-DIECBSS2  WSV-SALIDA-TS
      *       MOVE '202S'                     TO CAA-ID-FORMATO01
              MOVE 1                          TO CAA-NUM-ITEM-SELEC
      *       MOVE LENGTH OF REG-DIECBSS2    TO CAA-LGTH-FORMATO01
              MOVE W0091-CODIGO-TIT            TO BSS2-CODCARTERA
              MOVE W0091-SERVICIO-TIT          TO BSS2-SERVICIO
              MOVE W0091-MONEDA-TIT            TO BSS2-DIVISA
              MOVE W0091-INDPAGI               TO BSS2-INDPAGI
              MOVE W0091-PAGINAC               TO BSS2-PAGINAC
              MOVE WSC-DIECBSS2               TO WSV-TS-FORMATO         05582104
              MOVE REG-DIECBSS2              TO WSV-TS-DETALLE          05582120
              MOVE LENGTH OF REG-DIECBSS2    TO WS-TSLENGTH             05582130
              PERFORM 200222-WRITEQ-TS                                  05582140
              MOVE 0                          TO WS-ITEMS-TOT
              MOVE 1                          TO WS-IE09
              PERFORM FNEG-1100-LEE-OCCURS-DSCTO UNTIL SW-FIN-OCCURS
           END-IF.
      *
      *--------------------------------------------------------------*
      *                 FNEG-1100-LEE-OCCURS-DSCTO                   *
      *--------------------------------------------------------------*
      *
       FNEG-1100-LEE-OCCURS-DSCTO.
      *---------------------------*
           IF WS-IE09 <= W0091-N
             INITIALIZE REG-DIECBDS1  WSV-SALIDA-TS
             MOVE WSC-DIECBDS1             TO WSV-TS-FORMATO            05582104
             ADD  1                        TO WS-ITEMS-TOT
             MOVE W0091-FECOPE(WS-IE09)     TO WS-FECHA-8
             MOVE WS-FECHA-8(1:4)          TO WS-FECHA-10(1:4)
             MOVE '-'                      TO WS-FECHA-10(5:1)
             MOVE WS-FECHA-8(5:2)          TO WS-FECHA-10(6:2)
             MOVE '-'                      TO WS-FECHA-10(8:1)
             MOVE WS-FECHA-8(7:2)          TO WS-FECHA-10(9:2)
             MOVE WS-FECHA-10              TO BDS1-FECOPER
             MOVE W0091-CARGOS(WS-IE09)    TO BDS1-TOTCAR
             MOVE W0091-ABONOS(WS-IE09)    TO BDS1-TOTABO
             ADD  1                        TO WS-IE09
             MOVE REG-DIECBDS1            TO WSV-TS-DETALLE             05584504
             MOVE LENGTH OF REG-DIECBDS1  TO WS-TSLENGTH                05584803
             PERFORM 200222-WRITEQ-TS                                   05585003
           ELSE
             SET SW-FIN-OCCURS             TO TRUE
           END-IF.
      *
      *
      *----------------------------------------------------------------*05585703
      *                   200222-WRITEQ-TS.                            *05585803
      *----------------------------------------------------------------*05586003
       200222-WRITEQ-TS.                                                05586103
      *-----------------*                                               05586203
            MOVE CAA-IDIOMA-TERM            TO CAA-IDIOMA(1)
            MOVE 'P'                        TO CAA-IND-PANDOC(1)
            MOVE '+DC1'                     TO CAA-DESTINO(1)
            MOVE SPACES                     TO CAA-NUM-DOCUM(1)
            MOVE SPACES                     TO CAA-PRILIN-DOCUM(1)
            MOVE SPACES                     TO CAA-IMPRESO(1)
      *BBMD                                                             05586303
            ADD  +8              TO WS-TSLENGTH
      *BBMD
            EXEC CICS                                                   05586904
                WRITEQ TS QUEUE(WS-TSNAME)                              05586905
                FROM(WSV-SALIDA-TS)                                     05586906
                LENGTH(WS-TSLENGTH)                                     05586907
                MAIN NOHANDLE                                           05586908
           END-EXEC                                                     05586909
      *                                                                 05587003
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *       MOVE CON-SIN-ROLLBACK         TO CAA-CNYRELCE
              MOVE WS-TSNAME                TO CAA-OBJETO-ERROR
              PERFORM 4100-MOVER-ERRORES-CICS
              PERFORM 4500-FIN-ANORMAL
              PERFORM 3000-FIN
           END-IF.
      *                                                                 07020003
      *--------------------------------------------------------------*
      *                ASTF 0000 FIN PROCESO AST                     *
      *--------------------------------------------------------------*
      *
      *ASTF-0000-FIN-PROCESO-AST.
      *
      *    PERFORM ASTF-1000-GRABAR-LOG.
      *
      *--------------------------------------------------------------*
      *                 ASTF 1000 GRABAR LOG                         *
      *--------------------------------------------------------------*
      *
      * ASTF-1000-GRABAR-LOG.
      *
      *    SET  C30-PTR-COPYIN              TO CAA-PTR-APL-AST
      *    MOVE CAA-CNYTACT1              TO C30-PARAM-LOG
      *    MOVE CAA-CNYTACT2              TO C30-PARAM-MON
      *    MOVE SPACES                      TO C30-DIVISA
      *    MOVE CAA-CNRCAPLN              TO C30-COD-ERROR-E
      *    MOVE CAA-CNCOVAR1              TO C30-VALOR-UNO-E
      *    MOVE CAA-CNCOVAR2              TO C30-VALOR-DOS-E
      *    MOVE CAA-CNFOPERA              TO C30-FECHA-TRANS
      *    MOVE CAA-CNHTRANS              TO C30-HORA-TRANS
      *    MOVE CAA-CNCNLLLA              TO C30-CANAL-LLAMANTE
      *    MOVE CAA-CNYREVER              TO C30-IND-REVERSO
      *    MOVE SPACES                      TO C30-REF-INTERNA
      *
      *    EXEC CICS
      *       LINK PROGRAM(RUT-KN1CC300)
      *       COMMAREA(C30-KNDCC300)
      *    END-EXEC
      *
      *    IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *       MOVE CON-SIN-ROLLBACK         TO CAA-CNYRELCE
      *       MOVE C30-KNDCC300             TO CAA-REFERENCIA-ERR
      *       MOVE RUT-KN1CC300             TO CAA-OBJETO-ERR
      *       MOVE RUT-KN1CC300             TO CAA-PROGRAMA-ERR
      *       PERFORM 4100-MOVER-ERRORES-CICS
      *       PERFORM 3000-FIN
      *    ELSE
      *       IF C30-COD-RESPUESTA NOT EQUAL CON-RETORNO-OK
      *          MOVE CON-SIN-ROLLBACK         TO CAA-CNYRELCE
      *          IF   C30-COD-RESPUESTA     EQUAL CON-RETORNO-GRAVE
      *               MOVE CON-CON-ROLLBACK    TO CAA-CNYRELCE
      *          END-IF
      *          MOVE SPACES                   TO CAA-ABEND-CODE
      *          MOVE C30-PROGRAMA             TO CAA-PROGRAMA-ERR
      *          MOVE C30-REFERENCIA           TO CAA-REFERENCIA-ERR
      *          MOVE C30-OBJETO               TO CAA-OBJETO-ERR
      *          MOVE C30-SQLCODE              TO CAA-SQLCODE-ERR
      *          MOVE C30-SQLERRM              TO CAA-SQLERRM-ERR
      *          MOVE C30-EIBFN                TO CAA-EIBFN
      *          MOVE C30-EIBRSRCE             TO CAA-EIBRSRCE
      *          MOVE C30-EIBRCODE             TO CAA-EIBRCODE
      *          MOVE C30-EIBRESP1             TO CAA-EIBRESP1
      *          MOVE C30-EIBRESP2             TO CAA-EIBRESP2
      *          MOVE C30-COD-ERROR            TO CAA-CNRCAPLN
      *          MOVE C30-VALOR-ERROR-UNO      TO CAA-CNCOVAR1
      *          MOVE C30-VALOR-ERROR-DOS      TO CAA-CNCOVAR2
      *       END-IF
      *    END-IF.
      *
      *--------------------------------------------------------------*
      *                  4100 MOVER ERRORES CICS                     *
      *--------------------------------------------------------------*
      *
       4100-MOVER-ERRORES-CICS.
      *
           MOVE EIBFN                       TO CAA-EIBFN.
           MOVE EIBRSRCE                    TO CAA-EIBRSRCE.
           MOVE EIBRCODE                    TO CAA-EIBRCODE.
           MOVE EIBRESP                     TO CAA-EIBRESP1.
           MOVE EIBRESP2                    TO CAA-EIBRESP2.
           PERFORM 900200-ABEND-CICS.

       900200-ABEND-CICS.
      *
      * MI167860325 -----I
      *    SET WSS-INDERR-OK                TO TRUE
      *    SET WSS-INDERR-NOK               TO TRUE
      * MI167860325 -----F
      *    SET WSS-INDLOG-OK                TO TRUE
      *
           INITIALIZE                       QGECABC
           MOVE 'N'                         TO ABC-ABEND
      *    MOVE WSV-ERR-REF                 TO ABC-REFERENCIA
           MOVE EIBFN                       TO ABC-EIBFN
           MOVE EIBRSRCE                    TO ABC-EIBRSRCE
           MOVE EIBRCODE                    TO ABC-EIBRCODE
           MOVE EIBRESP                     TO ABC-EIBRESP1
           MOVE EIBRESP2                    TO ABC-EIBRESP2
      *
           EXEC CICS
              LINK PROGRAM (WSC-QG1CABC)
              COMMAREA (QGECABC)
           END-EXEC
           .
      *
      *--------------------------------------------------------------*
      *                  4500 FIN ANORMAL                            *
      *--------------------------------------------------------------*
      *
       4500-FIN-ANORMAL.
      *
           MOVE CON-NO-ACEPTADA             TO C30-IND-ACEPTADA.
      *
      *     PERFORM ASTF-1000-GRABAR-LOG.
      *--------------------------------------------------------------*
      *                       3000  FIN                              *
      *--------------------------------------------------------------*
      *
       3000-FIN.
      *
           EXEC CICS
              RETURN
           END-EXEC.
      *

