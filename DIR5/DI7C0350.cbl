       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DI7C0350.
      *************************************************************
      **                                                         **
      **  VERIFICA ENTRADA DE DATOS PRE-LIQUIDACION Y CANCELACION**
      **  CARTERA                                                **
      **                                                         **
      *************************************************************

      *************************************************************
      **               LOG  DE  MODIFICACIONES                   **
      **---------------------------------------------------------**
      ** @M001 C9521  SE A#ADE CONTROL DE CODIGO DE RETORNO 'NOT **
      ** 030103       FOUND' AL LEER LOS ARCHIVOS DE CARTERA     **
      **                                                         **
      *************************************************************

       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
         SOURCE-COMPUTER. IBM-3090.
         OBJECT-COMPUTER. IBM-3090.

       DATA DIVISION.
      *==============*

       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER             PIC   X(40) VALUE
                              '** INICIO WORKING-STORAGE SECTION **'.
      *
       01  RESPONSE                   PIC S9(09)  COMP.
       01  NOMBRES-DE-ARCHIVOS.
           05  WS-ARCHIVO-MAES        PIC  X(08)      VALUE 'DIMAESTR'.
           05  WS-ARCHIVO-PDTE        PIC  X(08)      VALUE 'DIPENDTE'.
JABO       05  WS-ARCHIVO-PDTE-BCO    PIC  X(08)      VALUE 'DIPENBCO'.
       01  LONGITUD-DE-REGISTROS.
           05  WS-LONG-REG-MAES       PIC S9(04) COMP VALUE +0600.
           05  WS-LONG-REG-PDTE       PIC S9(04) COMP VALUE +0350.
       01  LONGITUD-DE-CLAVES.
           05  WS-LONG-KEY-MAES       PIC S9(04) COMP VALUE +0012.
           05  WS-LONG-KEY-PDTE       PIC S9(04) COMP VALUE +0022.
JABO       05  WS-LONG-KEY-PDTE-BCO   PIC S9(04) COMP VALUE +0010.
      *
       01  VARIABLES.
           02 W-APOST                 PIC X   VALUE X'7D'.

       01  WS-FECTRA.
           02 WS-FECHOY        PIC 9(08).
           02 FILLER  REDEFINES  WS-FECHOY.
                 04 WS-ANO-HOY PIC 9999.
                 04 WS-MES-HOY PIC 99.
                 04 WS-DIA-HOY PIC 99.
       01  WS-FECWORK-8        PIC X(10).


      *    COPY DEL MODULO DE VALIDACION DE CUENTA   BG9CMDC4
       01  BG9CMDC4-AREA.
             COPY BGECMDC4.

      *    COPY RUTINA PERSONAS PE9C5000
       01  W-PEWC5000.
           COPY PEWC5000.

      *    MAESTRO DE CARTERA.
           COPY DIMAESTR.

      *    PENDIENTE DE CARTERA.
           COPY DIPENDTE.
      *
      *---
      *---  INCLUDE DE SQLCA PARA FORZAR COMPILACION CON DB2
      *---
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE DIDTATR END-EXEC.

       01  FILLER             PIC   X(40) VALUE
                              '** FINAL WORKING-STORAGE SECTION **'.
      *
       LINKAGE SECTION.
      *---------------*
       01  DFHCOMMAREA.
           COPY  DIWC350.

      *==================*
       PROCEDURE DIVISION.
      *==================*
       000-INICIO.
           PERFORM  100-INICIO-PROGRAMA.
           PERFORM  200-MODULO-CENTRAL.
           PERFORM  900-FIN-PROGRAMA.

      *===================*
       100-INICIO-PROGRAMA.
      *===================*

           MOVE '00'                TO  W350-CODRETOR

JABO       PERFORM  101-CARGO-DATOS-FALTANTES.

           IF  (W350-COD-CARTERA NOT  NUMERIC)  OR
               (W350-COD-CARTERA EQUAL SPACES)
                MOVE '20'                TO  W350-CODRETOR
                MOVE 'CNE0115'           TO  W350-CODERROR
                MOVE 'ERROR COD.CARTERA' TO  W350-REFERENC
                PERFORM  900-FIN-PROGRAMA.

           IF  (W350-TIPCTA     < 51) AND
               (W350-TIPCTA NOT = 30)
               CONTINUE
           ELSE
                MOVE '20'                TO  W350-CODRETOR
                MOVE 'CNE0121'           TO  W350-CODERROR
                MOVE 'ERR.TIPCTA NO CORRESP.A LETRAS'
                                         TO  W350-REFERENC
                PERFORM  900-FIN-PROGRAMA
           END-IF.

           EVALUATE W350-CODMON
              WHEN (1)
                   MOVE 'SOLES'          TO  W350-DES-DIVISA
              WHEN (2)
                   MOVE 'DOLARES'        TO  W350-DES-DIVISA
              WHEN OTHER
                   MOVE SPACES           TO  W350-DES-DIVISA
           END-EVALUATE.

JABO   101-CARGO-DATOS-FALTANTES.
JABO  *-------------------------*
JABO
JABO       MOVE  W350-NUMBCO             TO  PCAR-NUMBCO
JABO
JABO       EXEC CICS READ     DATASET   (WS-ARCHIVO-PDTE-BCO)
JABO                          LENGTH    (WS-LONG-REG-PDTE)
JABO                          RIDFLD    (PCAR-NUMBCO)
JABO                          KEYLENGTH (WS-LONG-KEY-PDTE-BCO)
JABO                          INTO      (REG-PENDIENTE-CARTERA)
JABO                          RESP      (RESPONSE)
JABO                          EQUAL
JABO                          NOHANDLE
JABO       END-EXEC.
JABO
JABO       IF   RESPONSE = DFHRESP (NORMAL)
JABO            MOVE  PCAR-CODIGO      TO  W350-CODIGO
JABO            MOVE  PCAR-TIPCTA      TO  W350-TIPCTA
JABO       ELSE
      *@M001-I
JABO  *         MOVE '99'              TO  W350-CODRETOR
JABO  *         MOVE 'CNE0116'         TO  W350-CODERROR
JABO  *         MOVE 'ERROR READ PDTE' TO  W350-REFERENC
JABO  *         PERFORM  900-FIN-PROGRAMA
              IF RESPONSE = DFHRESP (NOTFND)
                MOVE '10'              TO  W350-CODRETOR
                MOVE 'CNE0114'         TO  W350-CODERROR
                MOVE 'NOTFND DIPENBCO' TO  W350-REFERENC
                PERFORM  900-FIN-PROGRAMA
              ELSE
                MOVE '99'              TO  W350-CODRETOR
                MOVE 'CNE0116'         TO  W350-CODERROR
                MOVE 'ERROR READ PDTE' TO  W350-REFERENC
                PERFORM  900-FIN-PROGRAMA
              END-IF
           END-IF.
      *@M001-F

      *==================*
       200-MODULO-CENTRAL.
      *==================*

           PERFORM 210-VERIFICA-CODIGO-CLIENTE.
           PERFORM 220-VERIFICA-NUMERO-BANCO.
           PERFORM 230-REALIZA-VALIDACIONES.
           IF W350-INDCAN = 'A'
              PERFORM 240-VALIDA-CUENTA-CARGO
              PERFORM 250-RECUPERA-DATOS-TITU-CTA
           END-IF.
           PERFORM 260-CARGA-DATOS-SALIDA.


       210-VERIFICA-CODIGO-CLIENTE.
      *--------------------------*

           MOVE  W350-CODIGO             TO  MCAR-CODIGO
           MOVE  W350-TIPCTA             TO  MCAR-TIPCTA

           EXEC CICS READ     DATASET   (WS-ARCHIVO-MAES)
                              LENGTH    (WS-LONG-REG-MAES)
                              RIDFLD    (MCAR-KEYMAE)
                              KEYLENGTH (WS-LONG-KEY-MAES)
                              INTO      (REG-MAESTRO-CARTERA)
                              RESP      (RESPONSE)
                              EQUAL
                              NOHANDLE
           END-EXEC.

           IF   RESPONSE = DFHRESP (NORMAL)
                CONTINUE
           ELSE
      *@M001-I
      *         MOVE '99'              TO  W350-CODRETOR
      *         MOVE 'CNE0116'         TO  W350-CODERROR
      *         MOVE 'ERROR READ MAES' TO  W350-REFERENC
      *         PERFORM  900-FIN-PROGRAMA
              IF RESPONSE = DFHRESP (NOTFND)
                MOVE '10'              TO  W350-CODRETOR
                MOVE 'CNE0114'         TO  W350-CODERROR
                MOVE 'NOTFND DIMAESTR' TO  W350-REFERENC
                PERFORM  900-FIN-PROGRAMA
              ELSE
                MOVE '99'              TO  W350-CODRETOR
                MOVE 'CNE0116'         TO  W350-CODERROR
                MOVE 'ERROR READ MAES' TO  W350-REFERENC
                PERFORM  900-FIN-PROGRAMA
              END-IF
           END-IF.
      *@M001-F


       220-VERIFICA-NUMERO-BANCO.
      *-------------------------*

           MOVE  W350-CODIGO             TO  PCAR-CODIGO
           MOVE  W350-TIPCTA             TO  PCAR-TIPCTA
           MOVE  W350-NUMBCO             TO  PCAR-NUMBCO

           EXEC CICS READ     DATASET   (WS-ARCHIVO-PDTE)
                              LENGTH    (WS-LONG-REG-PDTE)
                              RIDFLD    (PCAR-KEYPEN)
                              KEYLENGTH (WS-LONG-KEY-PDTE)
                              INTO      (REG-PENDIENTE-CARTERA)
                              RESP      (RESPONSE)
                              EQUAL
                              NOHANDLE
           END-EXEC.

           IF   RESPONSE = DFHRESP (NORMAL)
                CONTINUE
           ELSE
      *@M001-I
      *         MOVE '99'              TO  W350-CODRETOR
      *         MOVE 'CNE0116'         TO  W350-CODERROR
      *         MOVE 'ERROR READ PDTE' TO  W350-REFERENC
      *         PERFORM  900-FIN-PROGRAMA
              IF RESPONSE = DFHRESP (NOTFND)
                MOVE '10'              TO  W350-CODRETOR
                MOVE 'CNE0114'         TO  W350-CODERROR
                MOVE 'NOTFND DIPENDTE' TO  W350-REFERENC
                PERFORM  900-FIN-PROGRAMA
              ELSE
                MOVE '99'              TO  W350-CODRETOR
                MOVE 'CNE0116'         TO  W350-CODERROR
                MOVE 'ERROR READ PDTE' TO  W350-REFERENC
                PERFORM  900-FIN-PROGRAMA
              END-IF
           END-IF.
      *@M001-F


       230-REALIZA-VALIDACIONES.
      *------------------------*

           IF PCAR-OFICIN NOT NUMERIC
              MOVE 902                   TO W350-AGEORI
           ELSE
              IF PCAR-OFICIN = 000 OR 999
                 MOVE 902                TO W350-AGEORI
              ELSE
                 MOVE PCAR-OFICIN        TO W350-AGEORI.

      *--- CHEQUEA SI DOCUMENTO YA HA SIDO CANCELADO ------------------*
           IF PCAR-SWCANC = 1
              MOVE '30'                             TO  W350-CODRETOR
              MOVE 'CNE0117'                        TO  W350-CODERROR
              MOVE 'DOCUMENTO YA HA SIDO CANCELADO' TO  W350-REFERENC
              PERFORM  900-FIN-PROGRAMA
           END-IF.

      *--- CHEQUEA SI DOCUMENTO ESTA EN CORRESPONSAL ------------------*
           IF PCAR-PLAACT > 0999
              MOVE '30'                             TO  W350-CODRETOR
              MOVE 'CNE0118'                        TO  W350-CODERROR
              MOVE 'DOC.EN CORRESPONS.NO LIQUIDABL' TO  W350-REFERENC
              PERFORM  900-FIN-PROGRAMA
           END-IF.

      *--- CHEQUEA SI DOCUMENTO ESTA EN JUDICIAL O EXTRAJUDICIAL ------*
           IF PCAR-PLAACT = 0903 OR 0908
              MOVE '30'                             TO  W350-CODRETOR
              MOVE 'CNE0118'                        TO  W350-CODERROR
              MOVE 'DOC.EN LEGAL NO LIQUIDABLE    ' TO  W350-REFERENC
              PERFORM  900-FIN-PROGRAMA
           END-IF.

      *--- CHEQUEA COBRO DE INTERESES, SI ES BLANCO LE MUEVE 'S' ------*
           IF W350-INDINT  NOT = 'S' AND ' '
              MOVE '20'                             TO  W350-CODRETOR
              MOVE 'CNE0115'                        TO  W350-CODERROR
              MOVE 'ERROR INDIC.DE COBRO INTERESES' TO  W350-REFERENC
              PERFORM  900-FIN-PROGRAMA
           ELSE
              IF (W350-TIPCTA    = 12)       OR
                 (PCAR-TDOCUM  = 03 OR 04)
                 MOVE 'N' TO W350-INDINT
      *6762014055-INI
                 IF W350-TIPCTA = 50
                    MOVE 'S' TO W350-INDINT
                 END-IF
      *6762014055-FIN
              ELSE
                 IF W350-INDINT = ' '
                    MOVE 'S' TO W350-INDINT
                 END-IF
              END-IF
           END-IF.


       240-VALIDA-CUENTA-CARGO.
      *-----------------------*
           INITIALIZE BGECMDC
           MOVE W350-CODCTA(1:4)          TO MDC-ENTIDAD
           MOVE W350-CODCTA(5:4)          TO MDC-CENTRO-ALTA
           MOVE W350-CODCTA(9:10)         TO MDC-CUENTA
           MOVE 'CN'                      TO MDC-FILLER(1:2).

           CALL 'BG9CMDC4' USING BG9CMDC4-AREA.

           IF MDC-CODERR  EQUAL SPACES OR LOW-VALUES
              MOVE MDC-DIVISA               TO  W350-CODDIV-CTACAR
              IF MDC-INDINMOV    =  'S'
                 MOVE '30'                  TO  W350-CODRETOR
                 MOVE 'CNE0122'             TO  W350-CODERROR
                 MOVE 'CUENTA INACTIVA    ' TO  W350-REFERENC
                 PERFORM  900-FIN-PROGRAMA
              ELSE
                 EVALUATE MDC-INDESTA
                    WHEN  'A'
                       CONTINUE
                    WHEN  'C'
                       MOVE '30'                  TO  W350-CODRETOR
                       MOVE 'CNE0122'             TO  W350-CODERROR
                       MOVE 'CUENTA CANCELADA   ' TO  W350-REFERENC
                       PERFORM  900-FIN-PROGRAMA
                    WHEN  'P'
                       MOVE '30'                  TO  W350-CODRETOR
                       MOVE 'CNE0122'             TO  W350-CODERROR
                       MOVE 'CUENTA PRECANCELADA' TO  W350-REFERENC
                       PERFORM  900-FIN-PROGRAMA
                    WHEN  OTHER
                       MOVE '30'                  TO  W350-CODRETOR
                       MOVE 'CNE0122'             TO  W350-CODERROR
                       MOVE 'CUENTA ERRADA      ' TO  W350-REFERENC
                       PERFORM  900-FIN-PROGRAMA
                 END-EVALUATE
              END-IF
           ELSE
              IF MDC-CODERR  EQUAL 'BGE0002'
                 MOVE '30'                  TO  W350-CODRETOR
                 MOVE 'CNE0122'             TO  W350-CODERROR
                 MOVE 'CUENTA NO EXISTE   ' TO  W350-REFERENC
                 PERFORM  900-FIN-PROGRAMA
              ELSE
                 MOVE '30'                  TO  W350-CODRETOR
                 MOVE 'CNE0122'             TO  W350-CODERROR
                 MOVE 'CUENTA NO DISPONIBL' TO  W350-REFERENC
                 PERFORM  900-FIN-PROGRAMA
              END-IF
           END-IF.


       250-RECUPERA-DATOS-TITU-CTA.
      *---------------------------*
           INITIALIZE W500-REGISTRO.

           MOVE W350-CODCTA(1:4)            TO W500-PECENTID
           MOVE W350-CODCTA(5:4)            TO W500-OFIAPE
           MOVE W350-CODCTA(9:2)            TO W500-CODISER
           MOVE W350-CODCTA(11:8)           TO W500-NUMECTA
           MOVE 'T'                         TO W500-CLAINTER
           MOVE '01'                        TO W500-SECINTER
           MOVE 'U'                         TO W500-PEYSELEC

           CALL 'PE9C5000'  USING W-PEWC5000
           EVALUATE W500-PECRETOR
               WHEN  '00'
      *--------------NOMBRE DEL TITULAR
                     IF W500-SUJGRUP(1) = 'F'
                        STRING W500-PRIAPE(1) DELIMITED BY '  '
                                          ' ' DELIMITED BY SIZE
                               W500-SEGAPE(1) DELIMITED BY '  '
                                          ' ' DELIMITED BY SIZE
                               W500-NOMBRE(1) DELIMITED BY '  '
                                              INTO  W350-NOMORI
                     ELSE
                        STRING W500-NOMBRE(1) DELIMITED BY SIZE
                               W500-PRIAPE(1) DELIMITED BY SIZE
                               W500-SEGAPE(1) DELIMITED BY SIZE
                                              INTO  W350-NOMORI
                     END-IF
      *--------------DIRECCION
                     IF W500-DIREC1(1) = SPACES AND
                        W500-DIREC3(1) = SPACES
                        MOVE  'SIN DIRECCION '  TO  W350-NOMDES
                     ELSE
                        STRING W500-IDEDIRE1(1) DELIMITED BY '  '
                                 ' '            DELIMITED BY SIZE
                               W500-DIREC1  (1) DELIMITED BY '  '
                                 ' '            DELIMITED BY SIZE
                               W500-DIREC3  (1) DELIMITED BY '  '
                                 ' '            DELIMITED BY SIZE
                               W500-APTTO   (1) DELIMITED BY '  '
                                 ' '            DELIMITED BY SIZE
                               W500-IDEDIRE2(1) DELIMITED BY '  '
                                 ' '            DELIMITED BY SIZE
                               W500-DIREC2  (1) DELIMITED BY '  '
                                 ' '            DELIMITED BY SIZE
                               W500-POBLACI (1) DELIMITED BY '  '
                                                INTO   W350-NOMDES
                       INSPECT W350-NOMDES  REPLACING ALL W-APOST BY ' '
                     END-IF
               WHEN  OTHER
                     MOVE 'ERROR NOMBRE'       TO    W350-NOMORI
                     MOVE 'ERROR DIRECCION'    TO    W350-NOMDES
           END-EVALUATE.


       260-CARGA-DATOS-SALIDA.
      *----------------------*
           MOVE ZEROS                   TO W350-CAR-COMPRO
                                           W350-CAR-COMMOR
                                           W350-CAR-COMNVO
                                           W350-CAR-COMCOB
                                           W350-CAR-COMTRF
                                           W350-CAR-COMCTE
                                           W350-CAR-PORTES
                                           W350-CAR-INTPRO
                                           W350-CAR-INTVEN
                                           W350-CAR-INTMOR
                                           W350-CAR-INTNVO
                                           W350-CAR-INTDEV
                                           W350-CAR-DIAVEN
                                           W350-CAR-DIAMOR
                                           W350-CAR-DIANVO
                                           W350-CAR-DIADEV
                                           W350-CAR-DIAPRO
                                           W350-CAR-IMPISC
                                           W350-CAR-IMPINT
                                           W350-CAR-DEVISC.
           MOVE ZEROS                   TO W350-CAR-TASINT
                                           W350-CAR-NROLIQ
           MOVE MCAR-NOMBRE             TO W350-CAR-NOMCLI
           MOVE MCAR-PLAORI             TO W350-CAR-PLAORI
           MOVE MCAR-FILLE1(1:1)        TO W350-CAR-CAMPAN
           MOVE MCAR-COMPRO             TO W350-COMPRO
           MOVE MCAR-COMCOB             TO W350-COMCOB
           MOVE MCAR-COMCTE             TO W350-COMCTE
           MOVE MCAR-PORTES             TO W350-PORTES
           MOVE MCAR-TRASNC             TO W350-TRASNC
           MOVE MCAR-TRASBC             TO W350-TRASBC
           MOVE MCAR-TRASBN             TO W350-TRASBN
           MOVE MCAR-COMTRA             TO W350-COMTRA
           MOVE MCAR-COMACE             TO W350-COMACE
           MOVE MCAR-INTERE             TO W350-INTERE
           MOVE MCAR-COMISI             TO W350-COMISI

           INSPECT W350-CAR-NOMCLI REPLACING ALL W-APOST BY ' '
           MOVE PCAR-NOMGIR             TO W350-CAR-NOMGIR
           INSPECT W350-CAR-NOMGIR REPLACING ALL W-APOST BY ' '
           MOVE PCAR-INSCOB             TO W350-CAR-INSCOB
           MOVE PCAR-TIPEMP             TO W350-CAR-TIPEMP
           MOVE PCAR-INDPRO             TO W350-CAR-INDPRO
           IF   PCAR-INSCOB  NOT =  10
                MOVE PCAR-GASNOT        TO W350-CAR-GASNOT
           END-IF.
           MOVE PCAR-TASA01             TO W350-CAR-TASA01
           MOVE PCAR-TASA02             TO W350-CAR-TASA02
           MOVE PCAR-TDOCUM             TO W350-CAR-TDOCUM
           MOVE PCAR-PLACOB             TO W350-CAR-PLACOB
           MOVE PCAR-PLAACT             TO W350-CAR-PLAACT
           MOVE '-'                     TO WS-FECWORK-8(5:1)
                                           WS-FECWORK-8(8:1)
           MOVE PCAR-VENDIA             TO WS-FECWORK-8(9:2)
           MOVE PCAR-VENMES             TO WS-FECWORK-8(6:2)
           MOVE PCAR-VENANO             TO WS-FECWORK-8(1:4)
           MOVE WS-FECWORK-8            TO W350-CAR-FECVEN
           MOVE SPACES                  TO WS-FECWORK-8
           MOVE '-'                     TO WS-FECWORK-8(5:1)
                                           WS-FECWORK-8(8:1)
           MOVE PCAR-INGDIA             TO WS-FECWORK-8(9:2)
           MOVE PCAR-INGMES             TO WS-FECWORK-8(6:2)
           MOVE PCAR-INGANO             TO WS-FECWORK-8(1:4)
           MOVE WS-FECWORK-8            TO W350-CAR-FECING
           MOVE SPACES                  TO WS-FECWORK-8
           MOVE '-'                     TO WS-FECWORK-8(5:1)
                                           WS-FECWORK-8(8:1)
           MOVE PCAR-IORDIA             TO WS-FECWORK-8(9:2)
           MOVE PCAR-IORMES             TO WS-FECWORK-8(6:2)
           MOVE PCAR-IORANO             TO WS-FECWORK-8(1:4)
           MOVE WS-FECWORK-8            TO W350-CAR-FECIOR
           MOVE SPACES                  TO WS-FECWORK-8
           MOVE '-'                     TO WS-FECWORK-8(5:1)
                                           WS-FECWORK-8(8:1)
           MOVE PCAR-CNDDIA             TO WS-FECWORK-8(9:2)
           MOVE PCAR-CNDMES             TO WS-FECWORK-8(6:2)
           MOVE PCAR-CNDANO             TO WS-FECWORK-8(1:4)
           MOVE WS-FECWORK-8            TO W350-CAR-FECCAR
           MOVE PCAR-IMPACT             TO W350-CAR-IMPACT
           MOVE PCAR-PORREN             TO W350-CAR-PORREN
           MOVE PCAR-NUMBCO             TO W350-CAR-NUEBCO
           MOVE W350-CODMON             TO W350-CODMDA
      *MONEDA CUENTA DEL SPTP.. NO FUNCIONA
      *    MOVE W350-CTAMON             TO WS-ACHCAJ
           MOVE PCAR-PORTAS             TO W350-TIPCAM
           MOVE  PCAR-IMPMIN        TO     W350-CAR-COMCTE.
      *    IF  PCAR-FECVEN  >  WS-FECHOY
      *        MOVE WS-FECHOY        TO  W3-FECHA1
      *        MOVE PCAR-FECVEN      TO  W3-FECHA2
      *        PERFORM 496-NRODIAS-DOSFECHAS
      *        MOVE  WW-PLAZO        TO  ISER-OPECON
      *    END-IF.

      *================*
       900-FIN-PROGRAMA.
      *================*

           EXEC  CICS  RETURN  END-EXEC.
