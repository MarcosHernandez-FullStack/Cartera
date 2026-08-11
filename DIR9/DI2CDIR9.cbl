      *----------------------------------------------------------------+
      *  DI2CDIR9 : CONSULTA DE DOCUMENTOS POR ACEPTANTE - CARTERAS    |
      *----------------------------------------------------------------+
      *  CLON CN1CFN4A / KN1CKW4A                                      |
      *----------------------------------------------------------------+
      * REF.       FECHA   AUTOR    MOTIVO                             |
      *----------------------------------------------------------------+
      * PETICION. DDMMMAA AUTOR...  DESCRIPCION........................|
      *----------------------------------------------------------------+
      *                    IDENTIFICATION DIVISION                     *
      *----------------------------------------------------------------*
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. DI2CDIR9.
       AUTHOR.     GFT - JZC.
       DATE-WRITTEN. MAYO 2026.
      *
      *----------------------------------------------------------------*
      *                   ENVIRONMENT DIVISION                         *
      *----------------------------------------------------------------*
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-3090.
       OBJECT-COMPUTER.
           IBM-3090.
      *
      *----------------------------------------------------------------*
      *                        DATA DIVISION                           *
      *----------------------------------------------------------------*
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *           IDENTIFICADOR DE INICIO DE WORKING STORAGE
       01  FILLER.
           05 FILLER                   PIC X(50) VALUE
              '*** INICIO WORKING STORAGE (DI2CDIR9) ***'.
       01  WSC-CONSTANTES.
           05 CON-DI2CDIR9          PIC X(08)       VALUE 'DI2CDIR9'.
           05 CON-DI7C0PRC          PIC X(08)       VALUE 'DI7C0PRC'.
           05 CON-DI6C0461          PIC X(08)       VALUE 'DI6C0461'.
           05 CON-QCCECSO0          PIC X(08)       VALUE 'QCCECSO0'.
           05 CON-DIECBD4A          PIC X(08)       VALUE 'DIECBD4A'.
           05 CON-DIECBS4A          PIC X(08)       VALUE 'DIECBS4A'.
           05 CON-DIE0141           PIC X(07)       VALUE 'CNE0141'.
           05 CON-DIE1030           PIC X(07)       VALUE 'CNE1030'.
           05 CON-DIE5167           PIC X(07)       VALUE 'UGE5167'.
           05 CON-DIE0550           PIC X(07)       VALUE 'UAE0550'.

       01  XXXXXX.
           05 WSC-50                PIC 9(02)    VALUE 50.
           05 WSC-1                 PIC 9(01)    VALUE 1.
           05 WSC-CERO              PIC 9(01)    VALUE 0.
           05 WSC-UNO               PIC X(01)    VALUE '1'.
           05 WSC-SI                PIC X(01)    VALUE 'S'.
           05 WSC-NO                PIC X(01)    VALUE 'N'.
           05 WSC-NO-ACEPTADA       PIC X(01)    VALUE 'N'.
           05 WSC-SIN-ROLLBACK      PIC X(02)    VALUE '01'.
           05 WSC-CON-ROLLBACK      PIC X(02)    VALUE '02'.
           05 WSC-OPER-CANCELADA    PIC X(20)    VALUE
              'OPERACION CANCELADA '.
           05 WSC-HORA-NO-PERMIT    PIC X(20)    VALUE
              'HORARIO NO PERMITIDO'.
      *
      *----------------------------------------------------------------*
      *                         CONSTANTES                             *
      *----------------------------------------------------------------*
       01  WS-RETORNO.
           05 C-RETORNO-OK                  PIC X(02) VALUE '00'.
           05 C-RETORNO-NTFND               PIC X(02) VALUE '10'.
           05 C-RETORNO-NOK                 PIC X(02) VALUE '99'.

       01  WS-CONSTANTES.
           05 C-CERO                        PIC X(01) VALUE '0'.
           05 C-UNO                         PIC X(01) VALUE '1'.
           05 C-AVISOS                      PIC X(01) VALUE 'A'.
           05 C-MENSAJES                    PIC X(01) VALUE 'M'.
           05 C-SI                          PIC X(01) VALUE 'S'.
           05 C-NO                          PIC X(01) VALUE 'N'.
           05 C-ACEPTADA                    PIC X(01) VALUE 'S'.
           05 C-NO-ACEPTADA                 PIC X(01) VALUE 'N'.
           05 C-SIN-ROLLBACK                PIC X(02) VALUE '01'.
           05 C-FORMATO                     PIC X(04) VALUE 'BS71'.
      *
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
           05 WSV-CNL-ITEM                  PIC S9(04) COMP VALUE ZERO.
           05 WSV-TSLONG2                   PIC S9(04) COMP VALUE +0.
           05 WSA-OBJETO                    PIC X(08) VALUE SPACES.
           05 WS-SALIDA-TS                  PIC X(300).
           05 WS-TSLENGTH                   PIC S9(04) COMP.
           05 WS-I                          PIC 9(03) VALUE 0.

      *----------------------------------------------------------------*
      *             AREA DE TRABAJO DE COPY DIWCOPRC
      *----------------------------------------------------------------*
       01  DIWC0PRC-1.
           COPY DIWC0PRC.
      *
      *---- COPY AREA DE TRABAJO DE LA RUTINA DI6C0460
      *
       01  W-DIWC46R.
           COPY DIWC46R.
      *
      *---- COPY RUTINA DE DESCRIPCON DE DIVISA
           COPY TCWC1200.
      *        COPY AREA DE TRABAJO DE RUTINA CN9CR185
       COPY CNDCR185.
      *
      *---- FORMATO DE SALIDA
           COPY DIECBS4A.
           COPY DIECBD4A.
      *
      *----------------------------------------------------------------*
      * COPY DE DATOS DE CANALES
      *----------------------------------------------------------------*
           COPY QGECCNLM.

      *-- COPY PARA EL PROGRAMA DE ABEND ------------------------------*
       01  FILLER.
           COPY QGECABC.

       01  FILLER.
           05 FILLER                       PIC X(40) VALUE
              '*** FINAL WORKING STORAGE  (DI2CDIR9) **'.
      *
      *--------------------------------------------------------------*
      *                     LINKAGE  SECTION                         *
      *--------------------------------------------------------------*
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
           COPY DIECBE02.
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
           PERFORM  1000-INICIO.
           PERFORM  2000-PROCESO.
           PERFORM  3000-FIN.
      *
      *--------------------------------------------------------------*
      *                         1000-INICIO                          *
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
           END-IF.
      *
           PERFORM 1050-OBTENER-CANAL
           SET ADDRESS OF BE02-DIECBE02     TO CAA-PTR-COPYIN.
           MOVE SPACES                      TO CAA-COD-AVISO1
                                               CAA-COD-AVISO2
                                               CAA-COD-ERROR
                                               CAA-CODTRAN-SIG
      *
           INITIALIZE CAA-SALIDA-TERM
           INITIALIZE BS4A-DIECBS4A
           INITIALIZE BD4A-DIECBD4A
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
           IF BE02-USUARIO1 IS NOT NUMERIC
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'CAMPO CODIGO CLIENTE'      TO CAA-VAR1-ERROR
              MOVE 'NO NUMERICO         '      TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR9                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF

      *PAGSIZE-INI
      *    SI EL CANAL NO INFORMA TAMANO DE PAGINA SE NORMALIZA A CERO
      *    Y LA RUTINA DI6C0461 ASUME EL MAXIMO. LA VALIDACION CONTRA
      *    EL MAXIMO (100) LA RESUELVE LA RUTINA.
           IF BE02-BPAGSIZE IS NOT NUMERIC
              MOVE ZEROS                       TO BE02-BPAGSIZE
           END-IF
      *PAGSIZE-FIN

           IF BE02-BINDPAGI NOT = ' ' AND NOT = 'S'
              MOVE SPACES                      TO BE02-BINDPAGI
           END-IF
           IF BE02-BINDPAGI NOT = ' ' AND NOT = 'S'
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'ERROR INDICADOR DE'        TO CAA-VAR1-ERROR
              MOVE 'PAGINACION '               TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR9                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF

           IF BE02-BINDPAGI = 'S'
              IF BE02-BPAGINAC = SPACES
                 MOVE CON-DIE0550              TO CAA-COD-ERROR
                 MOVE 'NO ENVIO ARGUMENTO '    TO CAA-VAR1-ERROR
                 MOVE 'DE BUSQUEDA '           TO CAA-VAR2-ERROR
                 MOVE CON-DI2CDIR9             TO CAA-OBJETO-ERROR
                 PERFORM 3000-FIN
              END-IF
           END-IF
           .
      *--------------------------------------------------------------*
      *                  1100-BORRAR-COLAS-TS                        *
      *--------------------------------------------------------------*
       1100-BORRAR-COLAS-TS.
      *
           EXEC CICS
                DELETEQ TS QUEUE(WSA-TS)
                NOHANDLE
           END-EXEC
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
      *
      *--------------------------------------------------------------*
      *                       2000-PROCESO                           *
      *--------------------------------------------------------------*
      *
       2000-PROCESO.
      *
           INITIALIZE BS4A-DIECBS4A

           PERFORM 2100-VERIFICA-DISPONIBILIDAD
           PERFORM 2200-CONSULTAR-DOCUMENTOS
           .
      *
      *----------------------------------------------------------------*
      *              2100 VERIFICA DISPONIBILIDAD
      *                 APLICATIVO DE CARTERA
      *----------------------------------------------------------------*
       2100-VERIFICA-DISPONIBILIDAD.
      *----------------------------*
           INITIALIZE  DIWC0PRC-1
           MOVE '0'    TO DIPRC-OPCION
           MOVE 'DI10' TO DIPRC-CODPROC

           EXEC CICS
                LINK PROGRAM (CON-DI7C0PRC)
                     COMMAREA(DIWC0PRC)
           END-EXEC
           IF EIBRESP NOT = DFHRESP(NORMAL)
              MOVE DIPRC-COD-ERROR          TO CAA-COD-ERROR
              MOVE CON-DI7C0PRC             TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF
           EVALUATE DIPRC-COD-RETORN
           WHEN '00'
                IF DIPRC-COD-ESTADO NOT EQUAL 'A'
                  MOVE CON-DIE1030          TO CAA-COD-ERROR
                  MOVE WSC-OPER-CANCELADA   TO CAA-VAR1-ERROR
                  MOVE WSC-HORA-NO-PERMIT   TO CAA-VAR2-ERROR
                  MOVE CON-DI2CDIR9         TO CAA-OBJETO-ERROR
                  PERFORM 3000-FIN
                END-IF
           WHEN OTHER
                MOVE DIPRC-COD-ERROR        TO CAA-COD-ERROR
                MOVE CON-DI7C0PRC           TO CAA-OBJETO-ERROR
                PERFORM 3000-FIN
           END-EVALUATE
           .
      *----------------------------------------------------------------*
      *               2200 CONSULTAR DOCUMENTOS                        *
      *----------------------------------------------------------------*
      *
       2200-CONSULTAR-DOCUMENTOS.
      ***************************
           INITIALIZE W-DIWC46R
           MOVE BE02-USUARIO1                   TO W460-NUM-CODCEN-ENT
      *PAGSIZE-INI
           MOVE BE02-BPAGSIZE                   TO W460-PAGSIZE
      *PAGSIZE-FIN

           IF BE02-BINDPAGI = C-SI THEN
              MOVE BE02-BPAGINAC(1:8)            TO W460-NUM-CODCEN-ENT
              MOVE BE02-BPAGINAC(9:10)           TO W460-FECVEN-ENT
              MOVE BE02-BPAGINAC(19:10)          TO W460-NRODOC-ENT
           END-IF
      *

           EXEC CICS
                LINK PROGRAM (CON-DI6C0461)
                COMMAREA     (W-DIWC46R)
           END-EXEC

           IF EIBRESP NOT = DFHRESP(NORMAL) THEN
              MOVE 'ERROR LINK DI6C0461'    TO ABC-REFERENCIA
              MOVE W460-CODRETOR            TO CAA-COD-ERROR
              MOVE CON-DI6C0461             TO CAA-OBJETO-ERROR
              PERFORM 4100-ERRORES-CICS
              PERFORM 3000-FIN
           END-IF
      *
           IF W460-CODRETOR EQUAL C-RETORNO-OK THEN
              CONTINUE
           ELSE
              MOVE CON-DIE0141              TO CAA-COD-ERROR
              MOVE W460-REFERENC            TO CAA-VAR1-ERROR
              MOVE CON-DI6C0461             TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF

      *    MUEVE LOS DATOS A COPY DE SALIDA
      ****  CABECERA ****

           INITIALIZE BS4A-DIECBS4A  WS-SALIDA-TS
           MOVE CON-DIECBS4A               TO BS4A-FORMATO

           IF W460-INDIC-PAGINAC EQUAL C-SI THEN
              MOVE C-SI                     TO BS4A-IND-PAGINACION
              MOVE W460-CODCEN-PAGINAC      TO BS4A-ARG-BUSQUEDA(1:8)
              MOVE W460-FECVEN-PAGINAC      TO BS4A-ARG-BUSQUEDA(9:10)
              MOVE W460-NRODOC-PAGINAC      TO BS4A-ARG-BUSQUEDA(19:10)
RI            MOVE W460-OCURRENCIAS         TO BS4A-ARG-BUSQUEDA(29:3)
           ELSE
              MOVE C-NO                     TO BS4A-IND-PAGINACION
              MOVE SPACES                   TO BS4A-ARG-BUSQUEDA
           END-IF.

           MOVE BS4A-DIECBS4A              TO WS-SALIDA-TS
           MOVE LENGTH OF BS4A-DIECBS4A    TO WS-TSLENGTH
           PERFORM 2250-GRABA-COLA-SALIDA

      **     DETALLE     **

           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL (WS-I > W460-OCURRENCIAS)
                      OR (WS-I > 100)
                      OR (W460-CODCAR(WS-I) = SPACES OR LOW-VALUES)

              INITIALIZE BD4A-DIECBD4A WS-SALIDA-TS
              MOVE CON-DIECBD4A             TO BD4A-FORMATO

              MOVE W460-CODCAR(WS-I)        TO BD4A-CODCAR
              MOVE W460-NRODOC(WS-I)        TO BD4A-NRODOC
              MOVE W460-FECVEN(WS-I)        TO BD4A-FECVEN
              MOVE W460-NOMBRE(WS-I)        TO BD4A-NOMBRE
              MOVE W460-IMPORTE(WS-I)       TO BD4A-IMPORTE
              MOVE W460-TIPO(WS-I)          TO BD4A-TIPO
              MOVE W460-NUMBCOORI(WS-I)     TO BD4A-NRORIG
              MOVE W460-SITUACION(WS-I)     TO BD4A-SITUAC

              MOVE BD4A-DIECBD4A              TO WS-SALIDA-TS
              MOVE LENGTH OF BD4A-DIECBD4A    TO WS-TSLENGTH
              PERFORM 2250-GRABA-COLA-SALIDA

           END-PERFORM
           .

      *--------------------------------------------------------------*
      *                  2250 GRABA COLA SALIDA                      *
      *--------------------------------------------------------------*
       2250-GRABA-COLA-SALIDA.
      ************************
           MOVE '+DC1'           TO CAA-DESTINO(1)
           MOVE 'P'              TO CAA-IND-PANDOC(1)

           MOVE '+DC1'           TO WSA-TS-NOMBRE
           MOVE CAA-TERMINAL     TO WSA-TS-SUFIJO

           ADD +8                TO WS-TSLENGTH

               EXEC CICS
                    WRITEQ TS QUEUE(WSA-TS)
                    FROM(WS-SALIDA-TS)
                    LENGTH(WS-TSLENGTH)
                    MAIN NOHANDLE
               END-EXEC

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
      *                         3000-FIN                             *
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
      *                  4100 ERRORES CICS                           *
      *--------------------------------------------------------------*
      *
       4100-ERRORES-CICS.
      *
           MOVE 'S'                         TO ABC-ABEND
           MOVE CON-DI2CDIR9                TO ABC-PROGRAMA
           MOVE EIBFN                       TO ABC-EIBFN
           MOVE EIBRSRCE                    TO ABC-EIBRSRCE
           MOVE EIBRCODE                    TO ABC-EIBRCODE
           MOVE EIBRESP                     TO ABC-EIBRESP1
           MOVE EIBRESP2                    TO ABC-EIBRESP2

           EXEC CICS LINK  PROGRAM('QG1CABC')
                           COMMAREA(QGECABC)
           END-EXEC
           .
