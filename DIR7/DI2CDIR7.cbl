      *--------------------------------------------------------------*
      *  DI2CDIR4 : CONSULTA DE DOCUMENTOS                           *
      *  CLON DE CN1CFN3K / KN1CKW3K                                 *
      *--------------------------------------------------------------*
      *        L O G    D E   M O D I F I C A C I O N E S            *
      *--------------------------------------------------------------*
      *  AUTOR   FECHA          DESCRIPCION                          *
      * ------- ------- ---------------------------------------------*
      *
      *--------------------------------------------------------------*
      *                  IDENTIFICATION DIVISION                     *
      *--------------------------------------------------------------*
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. DI2CDIR7.
       AUTHOR.     GFT - JZC.
       DATE-WRITTEN. MAYO 2026.
      *
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
      *                      DATA DIVISION                           *
      *--------------------------------------------------------------*
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *           IDENTIFICADOR DE INICIO DE WORKING STORAGE
       01  FILLER.
           05 FILLER                        PIC X(50) VALUE
              '*** INICIO WORKING STORAGE (KN1CKW3K) ***'.
      *
      *--------------------------------------------------------------*
      *                       CONSTANTES                             *
      *--------------------------------------------------------------*
      *
       01  CON-NOMBRES.
           03 CON-DI6C072O                  PIC X(08) VALUE 'DI6C072O'.
           03 CON-DI2CDIR7                  PIC X(08) VALUE 'DI2CDIR7'.
           03 CON-DIECBS3K                  PIC X(08) VALUE 'DIECBS3K'.
           03 CON-DIECBD3K                  PIC X(08) VALUE 'DIECBD3K'.
           03 CON-DIE5167                   PIC X(08) VALUE 'UGE5167'.
           03 CON-DIE0550                   PIC X(08) VALUE 'UAE0550'.
           03 CON-QCCECSO0                  PIC X(08) VALUE 'QCCECSO0'.
           03 CON-DIE0133                   PIC X(07) VALUE 'CNE0133'.
       01  WS-RETORNO.
           03 CON-RETORNO-OK                PIC X(02) VALUE '00'.
           03 CON-RETORNO-NTFND             PIC X(02) VALUE '10'.
           03 CON-RETORNO-NOK               PIC X(02) VALUE '99'.
       01  WS-CONSTANTES.
           03 CON-CERO                      PIC 9(01) VALUE  0.
           03 CON-UNO                       PIC 9(01) VALUE  1.
           03 CON-SI                        PIC X(01) VALUE 'S'.
           03 CON-NO                        PIC X(01) VALUE 'N'.
           03 CON-MAXREG                    PIC 9(03) VALUE 130.
           03 CON-SIN-ROLLBACK              PIC X(02) VALUE '01'.
           03 CON-BS3K                      PIC X(04) VALUE 'BS3K'.
           03 CON-FUNCION                   PIC X(04) VALUE '+DC1'.
      *
       01  WSV-FECHA1.
           03 WSV-AA1                       PIC 9(04) VALUE ZEROS.
           03 WSV-MM1                       PIC 9(02) VALUE ZEROS.
           03 WSV-DD1                       PIC 9(02) VALUE ZEROS.
       01  WSV-FECHA2.
           03 WSV-AA2                       PIC 9(04) VALUE ZEROS.
           03 WSV-MM2                       PIC 9(02) VALUE ZEROS.
           03 WSV-DD2                       PIC 9(02) VALUE ZEROS.
      *--------------------------------------------------------------*
      *                 DEFINICION DE VARIABLES                      *
      *--------------------------------------------------------------*
      *
       01  WS-VARIABLES.
           05 WSA-TS.
              10 WSA-TS-NOMBRE              PIC X(04) VALUE SPACES.
              10 WSA-TS-SUFIJO              PIC X(04) VALUE SPACES.
      *------------------  ESTRUCTURA COLA CANAL  ---------------------*
           05 WSV-CNL-TS.
              10 FILLER                     PIC X(06) VALUE '+CANAL'.
              10 WSV-CNL-CANAL              PIC X(02) VALUE SPACES.
           05 WSV-PTR-TS                    POINTER   VALUE NULL.
           05 WSV-TSLONG2                   PIC S9(04) COMP VALUE +0.
           05 WSV-CNL-ITEM                  PIC S9(04) COMP VALUE ZERO.
           05 WS-FECHA-8                    PIC 9(08).
           05 WS-FECHA-10                   PIC X(10).
           05 WS-ITEMS-TOTALES              PIC 9(04) VALUE  ZEROES.
           05 WS-IE09                       PIC 9(04) VALUE  ZEROES.
           05 WS-SALIDA-TS                  PIC X(256) VALUE SPACES.
           05 WS-TSLENGTH                   PIC S9(04) COMP VALUE ZEROS.
           05 WS-BUSQUEDA.
              10 WS-FECVEN-PAGINAC          PIC X(08) VALUE SPACES.
              10 WS-NUMBCO-PAGINAC          PIC X(10) VALUE SPACES.
              10 FILLER                     PIC X(20) VALUE SPACES.
      *
      *--------------------------------------------------------------*
      *                 DEFINICION DE SWITCH'S                       *
      *--------------------------------------------------------------*
      *
       01  SW-SWITCH.
           05 SW-OCCURS                  PIC X(01) VALUE 'N'.
              88 SW-NO-FIN-OCCURS        VALUE 'N'.
              88 SW-FIN-OCCURS           VALUE 'S'.
      *
      *-------------COPY AREA DE TRABAJO DE LA RUTINA DI7C0720
      *
       01  WS-DIWC72O.
           COPY DIWC72O.
      *
      *------------------COPYS DE SALIDA ------------------------------*
      * COPY CABECERA: KNECBS3K
      * COPY DETALLE : KNECBD3K
      *----------------------------------------------------------------*01640000
           COPY DIECBS3K.
           COPY DIECBD3K.
      *
      *----------------------------------------------------------------*
      * COPY DE DATOS DE CANALES
      *----------------------------------------------------------------*
           COPY QGECCNLM.

      *-- COPY PARA EL PROGRAMA DE ABEND ------------------------------*
       01  FILLER.
           COPY QGECABC.

       01  FILLER.
           03 FILLER                     PIC X(40) VALUE
              '*** FINAL WORKING STORAGE  (KN1CKW3K) **'.
      *
      *--------------------------------------------------------------*
      *                     LINKAGE  SECTION                         *
      *--------------------------------------------------------------*
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
           COPY DIECBE01.
      * COLA PARA CANAL DE TRABAJO
       01  WSV-TSDATOZ.
           05 TSDATOS    OCCURS 4000 TIMES PIC X(1).
      *
      *--------------------------------------------------------------*
      *                     PROCEDURE DIVISION                       *
      *--------------------------------------------------------------*
      *
       PROCEDURE DIVISION.
      *
           PERFORM  1000-INICIO
           PERFORM  2000-PROCESO
           PERFORM  3000-FIN
           .
      *
      *--------------------------------------------------------------*
      *                         1000 INICIO                          *
      *--------------------------------------------------------------*
      *
       1000-INICIO.
           INITIALIZE QGECABC

           EXEC CICS
              IGNORE CONDITION ERROR
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
              MOVE 'ERROR CICS IGNORE'      TO ABC-REFERENCIA
              PERFORM 4100-ERRORES-CICS
           END-IF
      *
           PERFORM 1050-OBTENER-CANAL
           SET ADDRESS OF BE01-DIECBE01     TO CAA-PTR-COPYIN
           MOVE SPACES                      TO CAA-COD-AVISO1
                                               CAA-COD-AVISO2
                                               CAA-COD-ERROR
                                               CAA-CODTRAN-SIG
      *
           INITIALIZE CAA-SALIDA-TERM
      *
           INITIALIZE BS3K-DIECBS3K
           INITIALIZE BD3K-DIECBD3K
      *
           PERFORM 1070-VALIDA-ENTRADA
      *
           MOVE '+DC1'                     TO WSA-TS-NOMBRE
           MOVE CAA-TERMINAL               TO WSA-TS-SUFIJO
      *
           PERFORM 1100-BORRAR-COLAS-TS.
      *
      *----------------------------------------------------------------*
      * OBTIENE CANAL DE TRABAJO                                       *
      *----------------------------------------------------------------*
       1050-OBTENER-CANAL.
      *------------------*

           MOVE CAA-CANAL                  TO WSV-CNL-CANAL
           MOVE 1                          TO WSV-CNL-ITEM
      *
           EXEC CICS
              READQ TS QUEUE (WSV-CNL-TS)
                    SET (WSV-PTR-TS)
                    LENGTH (WSV-TSLONG2)
                    ITEM (WSV-CNL-ITEM)
           END-EXEC

           EVALUATE EIBRESP
              WHEN DFHRESP(NORMAL)
                   SET ADDRESS OF WSV-TSDATOZ TO WSV-PTR-TS
                   MOVE WSV-TSDATOZ           TO QGECCNLM
              WHEN DFHRESP(QIDERR)
                   MOVE CON-DIE5167           TO CAA-COD-ERROR
                   PERFORM 3000-FIN
              WHEN OTHER
                  MOVE 'ERROR CICS READQ'     TO ABC-REFERENCIA
                  MOVE CON-QCCECSO0           TO ABC-PROGRAMA
                  PERFORM 4100-ERRORES-CICS
           END-EVALUATE
           .
      *
      *----------------------------------------------------------------*
      * VALIDA ENTRADA                                                 *
      *----------------------------------------------------------------*
       1070-VALIDA-ENTRADA.
      *-------------------*
           MOVE BE01-BCODCTAA(1:12)            TO W720-CODIGO.
           IF W720-CODIGO IS NOT NUMERIC
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'CAMPO CODIGO CARTERA'      TO CAA-VAR1-ERROR
              MOVE 'NO NUMERICO         '      TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR7                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF

           MOVE BE01-BFECHA01(1:4)             TO WSV-AA1
           MOVE BE01-BFECHA01(6:2)             TO WSV-MM1
           MOVE BE01-BFECHA01(9:2)             TO WSV-DD1
           IF (WSV-AA1 < 1900) OR
              (WSV-MM1 < 1  OR WSV-MM1 > 12) OR
              (WSV-DD1 < 1  OR WSV-MM1 > 31)
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'CAMPO FECHA INICIO'        TO CAA-VAR1-ERROR
              MOVE 'NO NUMERICO         '      TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR7                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF

           MOVE BE01-BFECHA02(1:4)             TO WSV-AA2
           MOVE BE01-BFECHA02(6:2)             TO WSV-MM2
           MOVE BE01-BFECHA02(9:2)             TO WSV-DD2
           IF (WSV-AA2 < 1900) OR
              (WSV-MM2 < 1  OR WSV-MM2 > 12) OR
              (WSV-DD2 < 1  OR WSV-MM2 > 31)
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'CAMPO FECHA FIN'           TO CAA-VAR1-ERROR
              MOVE 'RANGO NO VALIDA    '       TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR7                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF

           IF WSV-FECHA1 > WSV-FECHA2
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'FECHA RANGO INICIO'        TO CAA-VAR1-ERROR
              MOVE 'MAYOR FECHA FIN'           TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR7                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF

           IF BE01-BINDPAGI = LOW-VALUES
              MOVE SPACES                      TO BE01-BINDPAGI
           END-IF
           IF BE01-BINDPAGI NOT = ' ' AND NOT = 'S'
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'ERROR INDICADOR DE'        TO CAA-VAR1-ERROR
              MOVE 'PAGINACION '               TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR7                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF

           IF BE01-BINDPAGI = 'S'
              IF BE01-BPAGINAC = SPACES
                 MOVE CON-DIE0550              TO CAA-COD-ERROR
                 MOVE 'NO ENVIO ARGUMENTO '    TO CAA-VAR1-ERROR
                 MOVE 'DE BUSQUEDA '           TO CAA-VAR2-ERROR
                 MOVE CON-DI2CDIR7             TO CAA-OBJETO-ERROR
                 PERFORM 3000-FIN
              END-IF
           END-IF
           .

      *-------------------------------------------------------------*
      *                  1100 BORRAR COLAS TS                        *
      *--------------------------------------------------------------*
      *
       1100-BORRAR-COLAS-TS.
      *
           EXEC CICS
                DELETEQ TS QUEUE(WSA-TS)
                NOHANDLE
           END-EXEC.
      *
           EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
               WHEN DFHRESP(QIDERR)
                    CONTINUE
               WHEN OTHER
                    MOVE 'ERROR CICS DELETEQ'     TO ABC-REFERENCIA
                    MOVE WSA-TS                   TO CAA-OBJETO-ERROR
                    PERFORM 4100-ERRORES-CICS
           END-EVALUATE
           .
      *--------------------------------------------------------------*
      *                       2000 PROCESO                           *
      *--------------------------------------------------------------*
      *
       2000-PROCESO.
      *
           INITIALIZE BS3K-DIECBS3K
           PERFORM 2100-RECUPERA-DOCS
      *
           MOVE W720-CODIGO-TOT             TO BS3K-COD-CARTERA.
           MOVE W720-SERVICIO-TOT           TO BS3K-SERVICIO.
           MOVE W720-NUMDOC-TOT             TO BS3K-NUMDOC-TOT.
           MOVE W720-IMPORT-TOT             TO BS3K-IMPTOT-REGS.
           MOVE W720-NUMDOC-TOT-PR          TO BS3K-NUMDOC-X-REGS.
           MOVE W720-IMPORT-TOT-PR          TO BS3K-IMPTOT-X-REGS.
           MOVE W720-IMPORT-TOT-RYPR        TO BS3K-SALDO-TOTAL.
      *
           IF W720-INDIC-PAGINAC = CON-SI
              MOVE W720-INDIC-PAGINAC    TO BS3K-INDIC-PAGINAC
              MOVE W720-AREA-PAGINAC     TO BS3K-ARG-BUSQUEDA(1:30)
              MOVE W720-FECVEN-PAGINAC   TO WS-FECVEN-PAGINAC
              MOVE W720-NUMBCO-PAGINAC   TO WS-NUMBCO-PAGINAC
           ELSE
              MOVE CON-NO                TO BS3K-INDIC-PAGINAC
           END-IF
      *
           PERFORM 2300-ARMA-SALIDA.
      *
           IF W720-NUMDOC-TOT  > ZEROS
               PERFORM 2200-LEE-OCCURS UNTIL SW-FIN-OCCURS
           END-IF.
      *
      *--------------------------------------------------------------*
      *                 2100 RECUPERA DOCS                           *
      *--------------------------------------------------------------*
      *
       2100-RECUPERA-DOCS.
      *
           INITIALIZE W720-DIWC720.
      *
           MOVE BE01-BCODCTAA(1:12)   TO W720-CODIGO
           MOVE BE01-BFECHA01         TO W720-BFECHA01
           MOVE BE01-BFECHA02         TO W720-BFECHA02
      *
           IF (BE01-BPAGINAC NOT EQUAL SPACES OR
               BE01-BPAGINAC NOT EQUAL LOW-VALUES)
               MOVE BE01-BINDPAGI     TO  W720-INDIC-PAGINAC
               MOVE BE01-BPAGINAC     TO  W720-AREA-PAGINAC
           END-IF
      *
           EXEC CICS
                LINK PROGRAM(CON-DI6C072O)
                COMMAREA(W720-DIWC720)
           END-EXEC
      *
           IF EIBRESP EQUAL DFHRESP(NORMAL)
              IF W720-CODRETOR EQUAL CON-RETORNO-OK
                  MOVE 0                   TO WS-ITEMS-TOTALES
                  MOVE 1                   TO WS-IE09
              ELSE
                MOVE W720-DIRCAPLN      TO CAA-COD-ERROR
                MOVE W720-REFERENC      TO CAA-VAR1-ERROR
                PERFORM 3000-FIN
             END-IF
           ELSE
             MOVE 'ERROR LINK DI6C0720'    TO ABC-REFERENCIA
             MOVE W720-CODRETOR            TO CAA-OBJETO-ERROR
             PERFORM 4100-ERRORES-CICS
             PERFORM 3000-FIN
           END-IF
           .
      *
      *--------------------------------------------------------------*
      *                       2200 LEE OCCURS                        *
      *--------------------------------------------------------------*
      *
       2200-LEE-OCCURS.
      *
           IF WS-IE09 <= CON-MAXREG AND WS-IE09 <= W720-NUMDOC-TOT
              AND W720-FECVEN(WS-IE09) > ZEROS
      *
              INITIALIZE BD3K-DIECBD3K   WS-SALIDA-TS
              MOVE CON-DIECBD3K            TO  BD3K-FORMATO
              ADD  1                       TO  WS-ITEMS-TOTALES
              MOVE W720-FECVEN(WS-IE09)    TO  WS-FECHA-8
              MOVE WS-FECHA-8(1:4)         TO  WS-FECHA-10(1:4)
              MOVE '-'                     TO  WS-FECHA-10(5:1)
              MOVE WS-FECHA-8(5:2)         TO  WS-FECHA-10(6:2)
              MOVE '-'                     TO  WS-FECHA-10(8:1)
              MOVE WS-FECHA-8(7:2)         TO  WS-FECHA-10(9:2)
      *OCURRENCIA
              MOVE WS-FECHA-10             TO BD3K-FECH-VENC
              MOVE W720-NUMBCO(WS-IE09)    TO BD3K-NUM-DOCUM
              MOVE W720-NOMACE(WS-IE09)    TO BD3K-NOMB-ACEPT
              MOVE W720-IMPORT(WS-IE09)    TO BD3K-IMPORTE
              MOVE W720-SITUAC(WS-IE09)    TO BD3K-SITUACION
              MOVE W720-NUMBCOORI(WS-IE09) TO BD3K-NRORIGINAL
              ADD 1                        TO WS-IE09
      *
              MOVE BD3K-DIECBD3K           TO WS-SALIDA-TS
              MOVE LENGTH OF BD3K-DIECBD3K TO WS-TSLENGTH
              PERFORM 2400-WRITEQ-SALIDA
      *
           ELSE
              SET SW-FIN-OCCURS            TO TRUE
           END-IF.
      *
      *--------------------------------------------------------------*
      *                       2500 ARMA SALIDA                       *
      *--------------------------------------------------------------*
       2300-ARMA-SALIDA.
      *----------------*
      *
           MOVE CON-DIECBS3K               TO BS3K-FORMATO
           MOVE BS3K-DIECBS3K              TO WS-SALIDA-TS
           MOVE LENGTH OF BS3K-DIECBS3K    TO WS-TSLENGTH
           PERFORM 2400-WRITEQ-SALIDA.
      *
      *--------------------------------------------------------------*
      *                  2400 WRITEQ SALIDA                          *
      *--------------------------------------------------------------*
       2400-WRITEQ-SALIDA.
      *------------------*
      *
           MOVE '+DC1'           TO CAA-DESTINO(1)
           MOVE 'P'              TO CAA-IND-PANDOC(1)

           MOVE '+DC1'           TO WSA-TS-NOMBRE
           MOVE CAA-TERMINAL     TO WSA-TS-SUFIJO

      *    ADD +8                TO WS-TSLENGTH

           EXEC CICS
                WRITEQ TS QUEUE(WSA-TS)
                FROM           (WS-SALIDA-TS)
                LENGTH         (WS-TSLENGTH)
                MAIN NOHANDLE
           END-EXEC.
      *
           EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE WS-SALIDA-TS         TO CAA-OBJETO-ERROR
                    MOVE 'ERROR CICS  WRITEQ' TO ABC-REFERENCIA
                    PERFORM 4100-ERRORES-CICS
                    PERFORM 3000-FIN
           END-EVALUATE
           .
      *
      *--------------------------------------------------------------*
      *                         3000   FIN                           *
      *--------------------------------------------------------------*
      *
       3000-FIN.
      *
           IF CAA-COD-ERROR EQUAL SPACES
              EXEC CICS
                   SYNCPOINT
              END-EXEC
           ELSE
              PERFORM 1100-BORRAR-COLAS-TS
           END-IF
      *
           EXEC CICS
              RETURN
           END-EXEC
           .
      *
      *--------------------------------------------------------------*
      *                  4100 MOVER ERRORES CICS                     *
      *--------------------------------------------------------------*
      *
       4100-ERRORES-CICS.
      *
           MOVE 'S'                         TO ABC-ABEND
           MOVE CON-DI2CDIR7                TO ABC-PROGRAMA
           MOVE EIBFN                       TO ABC-EIBFN
           MOVE EIBRSRCE                    TO ABC-EIBRSRCE
           MOVE EIBRCODE                    TO ABC-EIBRCODE
           MOVE EIBRESP                     TO ABC-EIBRESP1
           MOVE EIBRESP2                    TO ABC-EIBRESP2

           EXEC CICS LINK  PROGRAM('QG1CABC')
                           COMMAREA(QGECABC)
           END-EXEC
           .
      *
