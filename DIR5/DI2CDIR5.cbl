      *----------------------------------------------------------------+
      *  DI2CDIR5 : PRELIQUIDACION LETRAS - CARTYERA                   |
      *----------------------------------------------------------------+
      *  CLON CN1CFN35 / KN1CKW35                                      |
      *----------------------------------------------------------------+
      * REF.       FECHA   AUTOR    MOTIVO                             |
      *----------------------------------------------------------------+
      * PETICION. DDMMMAA AUTOR...  DESCRIPCION........................|
      *----------------------------------------------------------------+
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID.   DI2CDIR5.
       AUTHOR.       GFT - JZC.
       DATE-WRITTEN. MAYO 2026.
      *
      ******************************************************************
      *                 ENVIRONMENT DIVISION                           *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-3090.
       OBJECT-COMPUTER.
           IBM-3090.
      ******************************************************************
      *                  DATA DIVISION                                 *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  FILLER.
           05 FILLER               PIC X(50) VALUE
           '*** INICIO WORKING STORAGE (DI2CDIR5) ***'.

      ******************************************************************
      *                       CONSTANTES                               *
      ******************************************************************
       01  CON-RUTINAS.
           05 CON-DI2CDIR5         PIC X(08) VALUE 'DI2CDIR5'.
           05 CON-DI6C0359         PIC X(08) VALUE 'DI6C0359'.
           05 CON-DI7C0351         PIC X(08) VALUE 'DI7C0351'.
           05 CON-DI6C0352         PIC X(08) VALUE 'DI6C0352'.
           05 CON-DI7C0PRC         PIC X(08) VALUE 'DI7C0PRC'.
           05 CON-DI7CKWD2         PIC X(08) VALUE 'DI7CKWD2'.
           05 CON-QCCECSO0         PIC X(08) VALUE 'QCCECS00'.
           05 CON-DIECBD35         PIC X(08) VALUE 'DIECBD35'.
           05 CON-DIECBE35         PIC X(08) VALUE 'DIECBE35'.
           05 CON-DIE9998          PIC X(07) VALUE 'CNE9998'.
           05 CON-DIE1030          PIC X(07) VALUE 'CNE1030'.
           05 CON-DIE5167          PIC X(07) VALUE 'DIE5167'.
           05 CON-DIE0550          PIC X(07) VALUE 'DIE0550'.

       01  WS-RETORNO.
           05 CON-RETORNO-OK       PIC X(02) VALUE '00'.
           05 CON-RETORNO-GRAVE    PIC X(02) VALUE '98'.
           05 CON-RETORNO-NOK      PIC X(02) VALUE '99'.

       01  WS-CONSTANTES.
           05 CON-BD35             PIC X(04) VALUE 'BD35'.
      *
      *----------------------------------------------------------------*
      *                           CONSTANTES                           *
      *----------------------------------------------------------------*
       01  WSC-CONSTANTES.
           05 WSC-FUNCION          PIC X(04)       VALUE '+DC1'.
           05 WSC-ASTA             PIC X(04)       VALUE 'ASTA'.
           05 WSC-50               PIC 9(02)       VALUE 50.
           05 WSC-100              PIC 9(03)       VALUE 100.
           05 WSC-1                PIC 9(01)       VALUE 1.
           05 WSC-CERO             PIC 9(01)       VALUE 0.
           05 WSC-UNO              PIC X(01)       VALUE '1'.
           05 WSC-SI               PIC X(01)       VALUE 'S'.
           05 WSC-NO               PIC X(01)       VALUE 'N'.
           05 WSC-A                PIC X(01)       VALUE 'A'.
           05 WSC-RETORNO-OK       PIC X(02)       VALUE '00'.
           05 WSC-RETORNO-APL      PIC X(02)       VALUE '10'.
           05 WSC-RETORNO-ERRORDB2 PIC X(02)       VALUE '98'.
           05 WSC-RETORNO-NOK      PIC X(02)       VALUE '99'.
           05 WSC-ACEPTADA         PIC X(01)       VALUE 'S'.
           05 WSC-NO-ACEPTADA      PIC X(01)       VALUE 'N'.
           05 WSC-SIN-ROLLBACK     PIC X(02)       VALUE '01'.
           05 WSC-CON-ROLLBACK     PIC X(02)       VALUE '02'.
           05 WSC-OPER-CANCELADA   PIC X(20)       VALUE
              'OPERACION CANCELADA '.
           05 WSC-HORA-NO-PERMIT   PIC X(20)       VALUE
              'HORARIO NO PERMITIDO'.

      ******************************************************************
      *                 S W I T C H E S                                *
      ******************************************************************
       01 ESTADO-DATOS             PIC X(02) VALUE 'NO'.
          88 HAY-DATOS                       VALUE 'SI'.
          88 NO-HAY-DATOS                    VALUE 'NO'.

      ******************************************************************
      *                 DEFINICION DE VARIABLES                        *
      ******************************************************************
       01  WS-VARIABLES.
           05 WSA-TS.
              10 WSA-TS-NOMBRE     PIC X(04) VALUE SPACES.
              10 WSA-TS-SUFIJO     PIC X(04) VALUE SPACES.
      *------------------  ESTRUCTURA COLA CANAL  ---------------------*
           05 WSV-CNL-TS.
              10 FILLER            PIC X(06) VALUE '+CANAL'.
              10 WSV-CNL-CANAL     PIC X(02) VALUE SPACES.
           05 WSV-PTR-TS           POINTER   VALUE NULL.
           05 WSV-CNL-ITEM         PIC S9(04) COMP VALUE ZERO.
           05 WSV-TSLONG2          PIC S9(04) COMP VALUE +0.
           05 WSA-OBJETO           PIC X(08) VALUE SPACES.
           05 WS-SALIDA-TS         PIC X(300).
           05 WS-TSLENGTH          PIC S9(04) COMP.
           05 WS-ITEM              PIC S9(04) COMP.
           05 W-LONG-CNTRACE       PIC S9(04) COMP.
           05 WS-ACUMDET           PIC 9(04) VALUE  ZEROES.
           05 WS-LONGITUD-ESCRITA  PIC 9(15) VALUE  ZEROES.
           05 WS-ITEMS-TOTALES     PIC 9(04) VALUE  ZEROES.
           05 W-CLIENTE-PU         PIC X(08) VALUE SPACES.
           05 WS-NOMBRE            PIC X(50).
           05 WS-NUMBCO            PIC X(10).
           05 WSR-NUMBCO   REDEFINES   WS-NUMBCO      PIC 9(10).
           05 WSA-CODCART          PIC X(12).
           05 WSN-CODCART  REDEFINES   WSA-CODCART PIC 9(12).
           05 WS-I                 PIC 9(03) VALUE  ZEROES.
           05 WS-ISER-INTER.
              10 ISER-CAR-NUEVEN   PIC X(10)     VALUE SPACES.
              10 ISER-CAR-NUEIMP   PIC 9(13)V99  VALUE  ZEROES.
              10 ISER-CAR-INDRENO  PIC X(01)     VALUE  ' '.
              10 ISER-CAR-TIPMOV   PIC 9(02)     VALUE  ZEROES.
           05 WD35-SITUACION       PIC X(01) VALUE ' '.

      *---------------------------------------------------------------*
      *    COPY DE RUTINA QUE VALIDA Y DATOS DE LA MOV                *
      *---------------------------------------------------------------*
       01  PECW5201.
           COPY PEWC5201.

       01  CPY-DI7CKWD2.
           COPY DIWCKWD2.
      *
       01  DIWC0PRC-1.
           COPY DIWC0PRC.
      *-----------  COPY AREA DE TRABAJO DE LA RUTINA DI7C0350         *
       01  DIWC350-01.

           COPY DIWC350.
           10 W350-CAR-MARCA7      PIC X(01).

      *-----------  COPY AREA DE TRABAJO DE LA RUTINA DI7C0351         *
       01  DIWC351-01.
           COPY DIWC351.

      *-----------  COPY AREA DE TRABAJO DE LA RUTINA DI6C0352         *
       01  DIWC352-01.
           COPY DIWC352.

      *------  FORMATO DE SALIDA DIECBD35.
           COPY DIECBD35.
      *----------------------------------------------------------------*
      * COPY DE DATOS DE CANALES
      *----------------------------------------------------------------*
           COPY QGECCNLM.

      *-- COPY PARA EL PROGRAMA DE ABEND ------------------------------*
       01  FILLER.
           COPY QGECABC.

       01  FILLER.
           05 FILLER               PIC X(40) VALUE
           '*** FINAL WORKING STORAGE  (DI2CDIR5) **'.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY QGECCAA.
           COPY DIECBE35.
      * COLA PARA CANAL DE TRABAJO
       01  WSV-TSDATOZ.
           05 TSDATOS    OCCURS 4000 TIMES PIC X(1).
      *
      ******************************************************************
      *                PROCEDURE DIVISION                              *
      ******************************************************************
       PROCEDURE DIVISION.

           PERFORM  1000-INICIO.
           PERFORM  2000-PROCESO.
           PERFORM  3000-FIN.
      *
      ******************************************************************
      *                 1000 INICIO                                    *
      *      INICIALIZACION DE VARIABLES Y DIRECCIONAMIENTO            *
      *      DEL AREA DE COMUNICACIONES STANDARD                       *
      ******************************************************************
       1000-INICIO.
      *
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
           SET ADDRESS OF BE35-DIECBE35     TO CAA-PTR-COPYIN.
           MOVE SPACES                      TO CAA-COD-AVISO1
                                               CAA-COD-AVISO2
                                               CAA-COD-ERROR
                                               CAA-CODTRAN-SIG
      *
           INITIALIZE W350-DIWC350
                      W351-DIWC351
                      W352-DIWC352
                      BD35-DIECBD35

           PERFORM 1070-VALIDA-ENTRADA
      *
           MOVE '+DC1'                     TO WSA-TS-NOMBRE
           MOVE CAA-TERMINAL               TO WSA-TS-SUFIJO
      *
           PERFORM 1100-BORRAR-COLAS-TS
           .
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
           IF BE35-USUARIO IS NOT NUMERIC
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'CAMPO CODIGO CLIENTE'      TO CAA-VAR1-ERROR
              MOVE 'NO NUMERICO         '      TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR5                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF

           IF BE35-INDINT = LOW-VALUES
              MOVE 'N'                         TO BE35-INDINT
           END-IF
           IF BE35-INDINT NOT = 'N' AND NOT = 'S'
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'ERROR INDICADOR DE'        TO CAA-VAR1-ERROR
              MOVE 'INTERES '                  TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR5                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-IF

           IF BE35-INDCAN = LOW-VALUES
              MOVE ' '                         TO BE35-INDCAN
           END-IF
           IF BE35-INDCAN NOT = ' ' AND NOT = 'A' AND NOT = 'R'
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'ERROR INDICADOR DE'        TO CAA-VAR1-ERROR
              MOVE 'CANCELACION '              TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR5                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN

           IF BE35-BCODAUX = LOW-VALUES
              MOVE ' '                         TO BE35-BCODAUX
           END-IF
           IF BE35-BCODAUX NOT = 'N' AND NOT = 'N'
              MOVE CON-DIE0550                 TO CAA-COD-ERROR
              MOVE 'ERROR INDICADOR DE'        TO CAA-VAR1-ERROR
              MOVE 'DEVOLUCION '               TO CAA-VAR2-ERROR
              MOVE CON-DI2CDIR5                TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
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
      ******************************************************************
      *                     2000 PROCESO                               *
      * EL PROCESO DE TODA FUNCION ESTA DIVIDIDO EN TRES PARTES, QUE   *
      * LLEVAN A CABO POR SEPARADO LAS ACTIVIDADES DE EL INICIO Y FIN  *
      * DEL AST QUE LES SON COMUNES Y LAS ESPECIFICAS DE LA FUNCION DE *
      * NEGOCIO.                                                       *
      ******************************************************************
       2000-PROCESO.

      *    PERFORM ASTI-1000-VALIDA-ACCES-FUNCION.

      *    PERFORM FNEG-0000-PROCESO-DE-NEGOCIO.
      *    PERFORM FNEG-5300-OBTENER-CLI
      *    PERFORM FNEG-1000-VERIF-DISP-APPCART
           PERFORM 2200-VERIF-DISP-APPCART
      *
           MOVE WSC-1 TO WS-I
           PERFORM UNTIL WS-I > WSC-100 OR (BE35-COD-CARTERA(WS-I)
                                          EQUAL SPACES OR LOW-VALUES)
      *
              PERFORM 2250-VALIDACION-ENTRADA

      * FACTURAS ELECTRONICAS RENOVACION
              MOVE 'A'    TO ISER-CAR-INDRENO
              MOVE '0'    TO    WD35-SITUACION
              IF W350-CAR-TDOCUM  =  '14'
                 IF W350-CAR-MARCA7 = 'R'
                    PERFORM 2300-BUSCA-DIDTMOV
                 ELSE
                    CONTINUE
      *          MOVE CON-CON-ROLLBACK      TO CCIC0-CNYRELCE
      *          MOVE 'CNE0019'             TO CCIC0-CNRCAPLN
      *          MOVE 'IND.(A)CANCEL.'      TO CCIC0-CNCOVAR1
      *          MOVE '; (R)AMORTIZAC.'     TO CCIC0-CNCOVAR2
      *          PERFORM 4500-FIN-ANORMAL
                 END-IF
              END-IF
      **
              PERFORM 2400-CALCULO-INTERESES
              PERFORM 2500-CALCULO-COMISIONES
              PERFORM 2600-CARGA-DATOS-SALIDA
              ADD WSC-1 TO WS-I
           END-PERFORM
           .
      *

      *    PERFORM 2700-GRABAR-LOG
      *    .

      ******************************************************************
      *              ASTI 1000 VALIDA ACCES FUNCION                    *
      * SE VALIDA EL ACCESO A LA FUNCION DEL CLIENTE A TRAVES DEL      *
      * COLECTOR CN2CC265.                                             *
      ******************************************************************
      *ASTI-1000-VALIDA-ACCES-FUNCION.
      *------------------------------*
      *    MOVE CCIC0-CNFUNCID              TO C26-COD-FUNCION.
      *    SET  C26-PTR-COPYIN              TO CCIC0-PTR-APL-AST.
      *    MOVE SPACES                      TO C26-ASUNTO-PROPIO.
      *    MOVE SPACES                      TO C26-ASUNTO-ASOCIADO.

      *    MOVE CCIC0-CNFUNCID        TO C26-COD-FUNCION
      *    SET  C26-PTR-COPYIN        TO CCIC0-PTR-APL-AST
      *    MOVE SPACES                TO C26-ASUNTO-PROPIO-TIPO
      *                                  C26-ASUNTO-PROPIO-ID
      *                                  C26-ASUNTO-ASOCIADO-TIPO
      *                                  C26-ASUNTO-ASOCIADO-ID
      *    MOVE ZEROES                TO C26-IMPORTE
      *    MOVE SPACES                TO C26-VALIDAR-PASSWORD
      *PSDAEMCS2-6-I
      *    MOVE BE00-USUARIO1         TO C26-USUARIO
      *    MOVE BE35-USUARIO          TO C26-USUARIO
      *PSDAEMCS2-6-F
      *    MOVE SPACES                TO C26-RESTO-ENTRADA
      *    MOVE WSC-ASTA              TO C26-RESTO-ENTRADA(6:4)
      *
      *
      *    EXEC CICS
      *         LINK PROGRAM(RUT-KN1CC260)
      *         COMMAREA    (REG-C26-KNDCC260)
      *         LENGTH      (LENGTH OF REG-C26-KNDCC260)
      *    END-EXEC
      *
      *    IF EIBRESP EQUAL DFHRESP(NORMAL)
      *       IF C26-COD-RESPUESTA EQUAL CON-RETORNO-OK
      *         MOVE C26-OPERACION          TO CCIC0-CNNOPERA
      *        CONTINUE
      *       ELSE
      *         MOVE WSC-SIN-ROLLBACK       TO CCIC0-CNYRELCE
      *         MOVE SPACES                 TO CCIC0-ABEND-CODE
      *         MOVE C26-PROGRAMA           TO CCIC0-PROGRAMA-ERR
      *         MOVE C26-REFERENCIA         TO CCIC0-REFERENCIA-ERR
      *         MOVE C26-OBJETO             TO CCIC0-OBJETO-ERR
      *         MOVE C26-SQLCODE            TO CCIC0-SQLCODE-ERR
      *         MOVE C26-SQLERRM            TO CCIC0-SQLERRM-ERR
      *         MOVE C26-EIBFN              TO CCIC0-EIBFN
      *         MOVE C26-EIBRSRCE           TO CCIC0-EIBRSRCE
      *         MOVE C26-EIBRCODE           TO CCIC0-EIBRCODE
      *         MOVE C26-EIBRESP1           TO CCIC0-EIBRESP1
      *         MOVE C26-EIBRESP2           TO CCIC0-EIBRESP2
      *         MOVE C26-OPERACION          TO CCIC0-CNNOPERA
      *         MOVE C26-COD-ERROR          TO CCIC0-CNRCAPLN
      *         MOVE C26-VALOR-ERROR-UNO    TO CCIC0-CNCOVAR1
      *         MOVE C26-VALOR-ERROR-DOS    TO CCIC0-CNCOVAR2
      *   OJO
      *         PERFORM 4100-MOVER-ERRORES-CICS
      *
      *         PERFORM 4500-FIN-ANORMAL
      *       END-IF
      *    ELSE
      *       MOVE WSC-CON-ROLLBACK   TO CCIC0-CNYRELCE
      *       MOVE C26-KNDCC260       TO CCIC0-REFERENCIA-ERR
      *       MOVE RUT-KN1CC260       TO CCIC0-OBJETO-ERR
      *                                  CCIC0-PROGRAMA-ERR
      *       PERFORM 4100-MOVER-ERRORES-CICS
      *       PERFORM 4500-FIN-ANORMAL
      *    END-IF
      *    .
      *
      ******************************************************************
      *                FNEG 0000 PROCESO DE NEGOCIO                    *
      * EN ESTE PARRAFO SE INCLUYE TODA LA LOGICA DE NEGOCIO QUE SE    *
      * NECESITE PARA PROPORCIONAR EL SERVICIO, INCUIDO EL CONTROL DE  *
      * PAGINACION  Y LAS VALIDACIONES DE DATOS QUE HAYA NECESIDAD DE  *
      * HACER Y QUE NO PUEDAN SER PROGRAMADAS DENTRO DE UNA RUTINA DE  *
      * APLICATIVO.                                                    *
      ******************************************************************
      *FNEG-0000-PROCESO-DE-NEGOCIO.
      *FNEG-1000-VERIF-DISP-APPCART.
       2200-VERIF-DISP-APPCART.
      *-----------------------*
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
                  MOVE CON-DI2CDIR5         TO CAA-OBJETO-ERROR
                  PERFORM 3000-FIN
                END-IF
           WHEN OTHER
                MOVE DIPRC-COD-ERROR        TO CAA-COD-ERROR
                MOVE CON-DI7C0PRC           TO CAA-OBJETO-ERROR
                PERFORM 3000-FIN
           END-EVALUATE
           .
      *SA20190452-F
      ******************************************************************
      *                  FNEG-5300-OBTENER-CLI                         *
      ******************************************************************
      *FNEG-5300-OBTENER-CLI.
      *
      *      INITIALIZE REG-KNECPG42.
      *      MOVE BE00-USUARIO1                 TO ACC-CNACCCNL-E.
      *      MOVE CCIC0-CNCNLIDA                TO ACC-CNCNLIDA-E.
      *      MOVE CCIC0-CNCENTI1                TO ACC-CNCENTID-E.
      ****   ARE FULL THIS FIELD
      *      MOVE SPACES                        TO ACC-CNCCLIPU-E.
      *
      *
      *
      *        EXEC CICS
      *           LINK PROGRAM(RUT-KN7CPP18)
      *           COMMAREA (REG-KNECPG42)
      *           LENGTH  (LENGTH OF REG-KNECPG42)
      *        END-EXEC
      *
      *
      *      IF EIBRESP NOT = DFHRESP(NORMAL)
      *         MOVE 'LINK'                TO WSA-OBJETO
      *         MOVE RUT-KN7CPP18          TO WSC-PROGRAMA
      *         PERFORM 4100-MOVER-ERRORES-CICS
      *
      *         PERFORM 4500-FIN-ANORMAL
      *      END-IF
      *
      *      IF ACC-COD-RESPUESTA  NOT EQUAL WSC-RETORNO-OK
      *          MOVE ACC-COD-ERROR        TO CCIC0-CNRCAPLN
      *          MOVE ACC-VALOR-ERROR-UNO  TO CCIC0-CNCOVAR1
      *          MOVE ACC-VALOR-ERROR-DOS  TO CCIC0-CNCOVAR2
      *          MOVE ACC-PROGRAMA         TO CCIC0-PROGRAMA-ERR
      *          MOVE ACC-REFERENCIA       TO CCIC0-REFERENCIA-ERR
      *          MOVE ACC-OBJETO           TO CCIC0-OBJETO-ERR
      *          MOVE ACC-SQLCODE          TO CCIC0-SQLCODE-ERR
      *          MOVE ACC-SQLERRM          TO CCIC0-SQLERRM-ERR
      *          MOVE ACC-EIBFN            TO CCIC0-EIBFN
      *          MOVE ACC-EIBRSRCE         TO CCIC0-EIBRSRCE
      *          MOVE ACC-EIBRCODE         TO CCIC0-EIBRCODE
      *          MOVE ACC-EIBRESP1         TO CCIC0-EIBRESP1
      *          MOVE ACC-EIBRESP2         TO CCIC0-EIBRESP2
      *****      MOVE ERR-ERRORES          TO CCIC0-INFO-ERROR
      *          MOVE RUT-KN7CPP18         TO CCIC0-OBJETO-ERR
      *                                       CCIC0-PROGRAMA-ERR
      *          PERFORM 4100-MOVER-ERRORES-CICS
      *          PERFORM 4500-FIN-ANORMAL
      *      ELSE
      *          PERFORM OBTIENE-ACC-CNCCLIPU
      *      END-IF
      *      .
      *----------------------------------------------------------------*
      *OBTIENE-ACC-CNCCLIPU.
      *----------------------------------------------------------------*
      *
      *    MOVE  1                             TO WSN-I
      *
      *    PERFORM
      *      UNTIL WSN-I > 25
      *         OR ACC-CNCCLIPU(WSN-I) = SPACES
      *            IF   ACC-CNNETNAM(WSN-I) (1:1) = '*'
      *               MOVE ACC-CNCCLIPU(WSN-I) TO WSA-CLIENTE-PU
      *               MOVE 26                  TO WSN-I
      *            ELSE
      *               ADD 1                    TO WSN-I
      *            END-IF
      *    END-PERFORM
      *    .
      *
      *FNEG-1000-VALIDACION-DE-DATOS.
       2250-VALIDACION-ENTRADA.
      *-----------------------*
           INITIALIZE  DIWC350-01.
           MOVE BE35-COD-CARTERA(WS-I)  TO    WSA-CODCART
           MOVE WSN-CODCART             TO    W350-COD-CARTERA
           MOVE BE35-NUMBCO(WS-I)       TO    WS-NUMBCO
           MOVE WSR-NUMBCO              TO    W350-NUMBCO
           MOVE BE35-INDINT             TO    W350-INDINT
           MOVE BE35-INDCAN             TO    W350-INDCAN
           MOVE SPACES                  TO    W350-CODCTA

           EXEC CICS
                LINK PROGRAM (CON-DI6C0359)
                     COMMAREA(W350-DIWC350)
           END-EXEC

           IF EIBRESP NOT = DFHRESP(NORMAL)
              MOVE 'ERROR LINK DI6C0359'    TO ABC-REFERENCIA
              MOVE W350-CODRETOR            TO CAA-COD-ERROR
              MOVE CON-DI6C0359             TO CAA-OBJETO-ERROR
              PERFORM 4100-ERRORES-CICS
              PERFORM 3000-FIN
           END-IF

           EVALUATE W350-CODRETOR
           WHEN '00'
                CONTINUE
           WHEN OTHER
              MOVE W350-CODERROR            TO CAA-COD-ERROR
              MOVE W350-REFERENC            TO CAA-VAR1-ERROR
              MOVE CON-DI6C0359             TO CAA-OBJETO-ERROR
              PERFORM 3000-FIN
           END-EVALUATE
           .
      *
      ******************************************************************
      *                    2300 BUSCA DIDTMOV                          *
      ******************************************************************
      *4510-BUCA-DIDTMOV.
       2300-BUSCA-DIDTMOV.
      *------------------*
           INITIALIZE CPY-DI7CKWD2
           MOVE BE35-NUMBCO(WS-I)   TO  DIWCKWD2-E-NUMBCO
           MOVE 'SE'                TO  DIWCKWD2-E-OPCION

           EXEC CICS
                LINK PROGRAM (CON-DI7CKWD2)
                     COMMAREA (CPY-DI7CKWD2)
                     LENGTH (LENGTH OF CPY-DI7CKWD2)
           END-EXEC

           IF EIBRESP NOT = DFHRESP(NORMAL)
              MOVE 'ERROR LINK DI7CKWD2'    TO ABC-REFERENCIA
              MOVE CON-DIE9998              TO CAA-COD-ERROR
              MOVE CON-DI7CKWD2             TO CAA-OBJETO-ERROR
              PERFORM 4100-ERRORES-CICS
              PERFORM 3000-FIN
           END-IF

           EVALUATE DIWCKWD2-S-COD-RETORNO
               WHEN CON-RETORNO-OK
                 MOVE DIWCKWD2-S-FECVTO-REN(1:2) TO ISER-CAR-NUEVEN(9:2)
                 MOVE DIWCKWD2-S-FECVTO-REN(3:1) TO ISER-CAR-NUEVEN(8:1)
                 MOVE DIWCKWD2-S-FECVTO-REN(4:2) TO ISER-CAR-NUEVEN(6:2)
                 MOVE DIWCKWD2-S-FECVTO-REN(6:1) TO ISER-CAR-NUEVEN(5:1)
                 MOVE DIWCKWD2-S-FECVTO-REN(7:4) TO ISER-CAR-NUEVEN(1:4)
                 COMPUTE ISER-CAR-NUEIMP  = W350-CAR-IMPACT -
                                            DIWCKWD2-S-IMPORTE-REN
                 MOVE 'R'                        TO ISER-CAR-INDRENO
                 MOVE '2'                        TO WD35-SITUACION
               WHEN OTHER
                    MOVE '1'                    TO WD35-SITUACION
           END-EVALUATE
           .

       2400-CALCULO-INTERESES.
      *----------------------*

           INITIALIZE  W351-DIWC351
           MOVE W350-COD-CARTERA       TO    W351-COD-CARTERA.
           MOVE W350-INDINT            TO    W351-INDINT
      *    MOVE CCIC0-CNFTRANS         TO    W351-FECTRA
           MOVE CAA-FECHA-OPER2        TO    W351-FECTRA
           MOVE W350-CAR-FECVEN        TO    W351-FECVEN
           MOVE W350-CAR-IMPACT        TO    W351-IMPACT
           MOVE W350-TIPCAM            TO    W351-TIPCAM
           MOVE W350-CAR-CAMPAN        TO    W351-CAMPAN
           MOVE W350-CAR-PLAORI        TO    W351-PLAORI
           MOVE BE35-NUMBCO(WS-I)      TO    W351-FILLER(1:10)
           MOVE WS-ISER-INTER          TO    W351-FILLER(11:28)

           EXEC CICS
                LINK PROGRAM (CON-DI7C0351)
                     COMMAREA(W351-DIWC351)
           END-EXEC

           IF EIBRESP NOT = DFHRESP(NORMAL)
              MOVE 'ERROR LINK DI7C0351'    TO ABC-REFERENCIA
              MOVE W351-CODRETOR            TO CAA-COD-ERROR
              MOVE CON-DI7C0351             TO CAA-OBJETO-ERROR
              PERFORM 4100-ERRORES-CICS
              PERFORM 3000-FIN
           END-IF

           EVALUATE W351-CODRETOR
           WHEN '00'
                CONTINUE
           WHEN OTHER
                MOVE W351-CODERROR            TO CAA-COD-ERROR
                MOVE W351-REFERENC            TO CAA-VAR1-ERROR
                MOVE CON-DI7C0351             TO CAA-OBJETO-ERROR
                PERFORM 3000-FIN
           END-EVALUATE.

       2500-CALCULO-COMISIONES.
      *-----------------------*

           INITIALIZE  W352-DIWC352
           IF BE35-BCODAUX EQUAL WSC-SI
              MOVE WSC-SI TO W352-DEVDOC
           ELSE
              MOVE WSC-NO TO W352-DEVDOC
           END-IF

           MOVE W350-COD-CARTERA       TO    W352-COD-CARTERA
           MOVE W350-CAR-IMPACT        TO    W352-CAR-IMPACT
           MOVE W350-CAR-PLAORI        TO    W352-PLAORI
           MOVE W350-CAR-GASNOT        TO    W352-CAR-GASNOT
           MOVE W350-CAR-INDPRO        TO    W352-CAR-INDPRO
           MOVE W350-CAR-INSCOB        TO    W352-CAR-INSCOB
           MOVE W350-MCAR-TARIFAS      TO    W352-MCAR-TARIFAS
           MOVE W350-CAR-COMISIONES    TO    W352-CAR-COMISIONES
           MOVE W351-CAR-INTERESES     TO    W352-CAR-INTERESES
           MOVE W350-CAR-IMPUESTOS     TO    W352-CAR-IMPUESTOS

           MOVE W350-CAR-FECVEN        TO    W352-FECVEN.
      *    MOVE CCIC0-CNFTRANS         TO    W352-FECTRA.
      *    MOVE CCIC0-CNHTRANS         TO    W352-HORINI.
           MOVE CAA-FECHA-OPER2        TO    W352-FECTRA
           MOVE CAA-HORA-TRANS         TO    W352-HORINI
           MOVE BE35-NUMBCO(WS-I)      TO    W352-FILLER(1:10)
           MOVE W351-FILLER(11:28)     TO    W352-FILLER(11:28)

           EXEC CICS
                LINK PROGRAM (CON-DI6C0352)
                     COMMAREA(W352-DIWC352)
           END-EXEC

           IF EIBRESP NOT = DFHRESP(NORMAL)
              MOVE 'ERROR LINK DI6C0352'    TO ABC-REFERENCIA
              MOVE W352-CODRETOR            TO CAA-COD-ERROR
              MOVE CON-DI6C0352             TO CAA-OBJETO-ERROR
              PERFORM 4100-ERRORES-CICS
              PERFORM 3000-FIN
           END-IF

           EVALUATE W352-CODRETOR
           WHEN '00'
                MOVE W352-CAR-GASNOT        TO W350-CAR-GASNOT
           WHEN OTHER
                MOVE 'ERROR LINK DI6C0352'    TO ABC-REFERENCIA
                MOVE W352-CODERROR            TO CAA-COD-ERROR
                MOVE CON-DI6C0352             TO CAA-OBJETO-ERROR
                PERFORM 4100-ERRORES-CICS
                PERFORM 3000-FIN
           END-EVALUATE
           .

       2600-CARGA-DATOS-SALIDA.
      *-----------------------*
           MOVE  CON-DIECBD35       TO    BD35-FORMATO
           MOVE  '--'               TO    BD35-NRO-MICROFI
           MOVE  W352-IMPTRA        TO    BD35-IMP-TOT-PAG
           MOVE  W350-DES-DIVISA    TO    BD35-DIVISA
           MOVE  W350-CAR-NUEBCO    TO    BD35-NROBCO
           MOVE  W350-CAR-NUMBCOORI TO    BD35-NRORIGINAL.
           MOVE  W350-CAR-NOMCLI    TO    BD35-NOM-CLIENTE
      * FACTURAS ELECTRONICAS
           MOVE  ISER-CAR-NUEVEN    TO    BD35-NVA-FECHA
           MOVE  ISER-CAR-NUEIMP    TO    BD35-NUEVO-IMP
           MOVE  WD35-SITUACION     TO    BD35-SITUACION
           IF W350-CAR-TDOCUM = '14'
              MOVE  '3'             TO    BD35-TIPDOC
           ELSE
              MOVE  '2'             TO    BD35-TIPDOC
           END-IF

           MOVE BD35-DIECBD35              TO WS-SALIDA-TS
           MOVE LENGTH OF BD35-DIECBD35    TO WS-TSLENGTH
           PERFORM 2650-WRITEQ-DETALLE
           ADD  1                         TO WS-ACUMDET
           .
      *    MOVE CON-BD35                  TO CCIC0-ID-FORMATO01
      *    MOVE WS-ACUMDET                TO CCIC0-NITEM-FORMATO01
      *    MOVE WSV-TSLENGTH              TO CCIC0-LGTH-FORMATO01.

      ******************************************************************
      *                  FNEG 4100 WRITEQ DETALLE.                     *
      ******************************************************************
       2650-WRITEQ-DETALLE.
      *------------------------*
      *
           MOVE '+DC1'           TO CAA-DESTINO(1)
           MOVE 'P'              TO CAA-IND-PANDOC(1)

           MOVE '+DC1'           TO WSA-TS-NOMBRE
           MOVE CAA-TERMINAL     TO WSA-TS-SUFIJO

      *     ADD +8                TO WS-TSLENGTH

           EXEC CICS
                WRITEQ TS QUEUE(WSA-TS)
                FROM           (WS-SALIDA-TS)
                LENGTH         (WS-TSLENGTH)
                MAIN NOHANDLE
           END-EXEC.
      *
           EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                    ADD WS-TSLENGTH         TO WS-LONGITUD-ESCRITA
                    ADD 1                   TO WS-ITEMS-TOTALES
               WHEN OTHER
                    MOVE WS-SALIDA-TS         TO CAA-OBJETO-ERROR
                    MOVE 'ERROR CICS WRITEQ'  TO ABC-REFERENCIA
                    PERFORM 4100-ERRORES-CICS
                    PERFORM 3000-FIN
           END-EVALUATE
           .
      ******************************************************************
      *                  ASTF 0000 FIN PROCESO AST                     *
      * SE VALIDA EL ACCESO A LA FUNCION DEL CLIENTE A TRAVES DEL      *
      * COLECTOR CN2CC265.                                             *
      ******************************************************************
      ******************************************************************
      *                   2700 GRABAR LOG                              *
      ******************************************************************
      *2700-GRABAR-LOG.
      *---------------*
      *     SET  C30-PTR-COPYIN        TO CCIC0-PTR-APL-AST
      *     MOVE CCIC0-CNYTACT1        TO C30-PARAM-LOG
      *     MOVE CCIC0-CNYTACT2        TO C30-PARAM-MON
      *     MOVE SPACES                TO C30-DIVISA
      *     MOVE CCIC0-CNRCAPLN        TO C30-COD-ERROR-E
      **     MOVE CCIC0-CNCOVAR1        TO C30-VALOR-ERROR-UNO
      *     MOVE CCIC0-CNCOVAR2        TO C30-VALOR-ERROR-DOS
      *     MOVE CCIC0-CNFTRANS        TO C30-FECHA-TRANS
      *     MOVE CCIC0-CNHTRANS        TO C30-HORA-TRANS
      *     MOVE CCIC0-CNCNLLLA        TO C30-CANAL-LLAMANTE
      *     MOVE CCIC0-CNYREVER        TO C30-IND-REVERSO
      *     MOVE CCIC0-CNTOPERA        TO C30-TIPO-OPERA
      *     MOVE CCIC0-CNSOPERA        TO C30-SUB-OPERA
      *     MOVE CCIC0-CNVERTRA        TO C30-VERSION-TRAN
      *     MOVE CCIC0-CNMEDACC        TO C30-MEDIO-ACCESO
      *     MOVE CCIC0-CNCODSPR        TO C30-COD-SERVICIO
      *     MOVE CCIC0-CNTIDCLI        TO C30-TIPO-IDENT
      *     MOVE CCIC0-CNDOCIDC        TO C30-DOC-IDENT.
      *    MOVE BE00-BCODAUX2(1:1)          TO C30-CNYLIB10


      *     EXEC CICS
      *        LINK PROGRAM(RUT-KN1CC300)
      *        COMMAREA    (REG-C30-KNDCC300)
      *        LENGTH      (LENGTH OF REG-C30-KNDCC300)
      *     END-EXEC

      *
      *     IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *        MOVE WSC-SIN-ROLLBACK         TO CCIC0-CNYRELCE
      *        MOVE C30-KNDCC300             TO CCIC0-REFERENCIA-ERR
      *        MOVE RUT-KN1CC300             TO CCIC0-OBJETO-ERR
      *                                         CCIC0-PROGRAMA-ERR
      *        PERFORM 4100-ERRORES-CICS
      *        PERFORM 3000-FIN
      *     ELSE
      *        IF C30-COD-RESPUESTA EQUAL CON-RETORNO-OK
      *           CONTINUE
      *        ELSE
      *           MOVE WSC-SIN-ROLLBACK         TO CCIC0-CNYRELCE
      *           IF   C30-COD-RESPUESTA     EQUAL CON-RETORNO-GRAVE
      *                MOVE WSC-CON-ROLLBACK    TO CCIC0-CNYRELCE
      *           END-IF
      **           MOVE SPACES                   TO CCIC0-ABEND-CODE
      *           MOVE C30-PROGRAMA             TO CCIC0-PROGRAMA-ERR
      *           MOVE C30-REFERENCIA           TO CCIC0-REFERENCIA-ERR
      *           MOVE C30-OBJETO               TO CCIC0-OBJETO-ERR
      *           MOVE C30-SQLCODE              TO CCIC0-SQLCODE-ERR
      *           MOVE C30-SQLERRM              TO CCIC0-SQLERRM-ERR
      **           MOVE C30-EIBFN                TO CCIC0-EIBFN
      **           MOVE C30-EIBRSRCE             TO CCIC0-EIBRSRCE
      *           MOVE C30-EIBRCODE             TO CCIC0-EIBRCODE
      *           MOVE C30-EIBRESP1             TO CCIC0-EIBRESP1
      *           MOVE C30-EIBRESP2             TO CCIC0-EIBRESP2
      *           MOVE C30-COD-ERROR            TO CCIC0-CNRCAPLN
      *           MOVE C30-VALOR-ERROR-UNO      TO CCIC0-CNCOVAR1
      *           MOVE C30-VALOR-ERROR-DOS      TO CCIC0-CNCOVAR2
      *        END-IF
      *    END-IF.

      ******************************************************************
      *                    4500 FIN ANORMAL                            *
      ******************************************************************
      * 4500-FIN-ANORMAL.

      *     MOVE WSC-NO-ACEPTADA             TO C30-IND-ACEPTADA

      *     PERFORM 2700-GRABAR-LOG.

      *     PERFORM 3000-FIN.

      ******************************************************************
      *                    3000 FIN                                    *
      ******************************************************************
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
      ******************************************************************
      *                  4100 ERRORES CICS                             *
      ******************************************************************
       4100-ERRORES-CICS.
      *
           MOVE 'S'                         TO ABC-ABEND
           MOVE CON-DI2CDIR5                TO ABC-PROGRAMA
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
