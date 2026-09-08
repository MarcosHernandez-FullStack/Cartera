       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DI7C0351.
      *************************************************************
      **                                                         **
      **  CALCULA INTERESES PARA LA CANCELACION CARTERA          **
      **                                                         **
      *************************************************************
      *----------------------------------------------------------------*
      * MODIFICACIONES:                                                *
      * REF.      FECHA      PGMADOR    DESCRIPCION                    *
      *----------------------------------------------------------------*
      * 200308030 22-10-2003 C.ROSALES  ELIMINA DESAGIO EN PAGO ANTICI-*
      *                                 PADO DE LETRAS                 *
      * 200804211 05-04-2008 E.HUAMANI  FERIADOS                       *
      *----------------------------------------------------------------*

       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
         SOURCE-COMPUTER. IBM-3090.
         OBJECT-COMPUTER. IBM-3090.

       DATA DIVISION.
      *==============*

       WORKING-STORAGE SECTION.
      *-----------------------*
      *
       01  XXX                PIC   X(14) VALUE SPACES.
       01  XXXX               PIC   99999 VALUE ZEROS.
       01  XXXXX              PIC  9(13)V99 VALUE ZEROS.
       01  FILLER             PIC   X(40) VALUE
                              '** INICIO WORKING-STORAGE SECTION **'.
      *
       01  RESPONSE                   PIC S9(09)  COMP.
       01  NOMBRES-DE-ARCHIVOS.
           05  WS-ARCHIVO-TARI        PIC  X(08)      VALUE 'DITARNEW'.
           05  WS-ARCHIVO-PDTE        PIC  X(08)      VALUE 'DIPENDTE'.
       01  LONGITUD-DE-REGISTROS.
           05  WS-LONG-REG-TARI       PIC S9(04) COMP VALUE +0132.
           05  WS-LONG-REG-PDTE       PIC S9(04) COMP VALUE +0350.
       01  LONGITUD-DE-CLAVES.
           05  WS-LONG-KEY-TARI       PIC S9(04) COMP VALUE +0021.
           05  WS-LONG-KEY-PDTE       PIC S9(04) COMP VALUE +0022.
      *
       01  VARIABLES.
           02 WS-NRODIAS         PIC 99999         VALUE ZEROS.
           02 WS-NRODIAS-DEV     PIC 99999         VALUE ZEROS.
           02 WS-FEC-SGTE-HABIL  PIC X(10)         VALUE SPACES.
           02 WS-FEC-ANTE-HABIL  PIC X(10)         VALUE SPACES.
           02 WS-TASA01          PIC 99V9(3)       VALUE ZEROS.
           02 WS-COTAMO          PIC 9(04)         VALUE ZEROS.
           02 WS-COTAVE          PIC 9(04)         VALUE ZEROS.
           02 WS-COTARE          PIC 9(04)         VALUE ZEROS.
           02 WS-INTERES         PIC S9(15)V99     VALUE ZEROS.
           02 WS-KEYTARIFA.
              03 WS-PLAZA        PIC  9(4)         VALUE  0100.
              03 WS-MONEDA       PIC  9            VALUE  0.
              03 WS-TIPCT2       PIC  9(2)         VALUE  ZEROS.
              03 WS-CODIGO.
                  04 WS-CODTAR   PIC  9(2)         VALUE  ZEROS.
                  04 WS-CODPRE   PIC  9(2)         VALUE  ZEROS.
              03 WS-RANGOS       PIC  9(2)         VALUE  ZEROS.
              03 WS-FECTER       PIC  9(8)         VALUE  ZEROS.
      *    02 ISER-CAR-NUEVEN    PIC 9(06).
      *    02 ISER-CAR-NUEIMP    PIC 9(13)V99  COMP-3.

           02 WS-ISER-INTER.
              05 ISER-CAR-NUEVEN        PIC X(10).
              05 ISER-CAR-NUEIMP        PIC 9(13)V99.
              05 ISER-CAR-INDRENO       PIC X(01).
              05 ISER-CAR-TIPMOV        PIC 9(02).

           02 WK-MINREN                PIC 9(13)V99 COMP-3 VALUE ZEROS.
           02 WCAR-FECVEN.
              04 WCAR-VENANO           PIC 9(04).
              04 WCAR-FILL1            PIC X(01) VALUE '-'.
              04 WCAR-VENMES           PIC 9(02).
              04 WCAR-FILL2            PIC X(01) VALUE '-'.
              04 WCAR-VENDIA           PIC 9(02).
      *
      *------- INDICADOR DE COBRO DE INTERESES ------------------------*
      *    WK-INTERE  ==> 20 VENC/MOR   C/QUIEBRE
      *                   30 VENC/MOR   S/QUIEBRE
      *                   40 VENC/MOR   T/VIGENTE
      *    WK-INTPND  ==> 20 PAG NO DESC
      *    WK-INTDEV  ==> 20 DEVUELTOS
      *    WK-INTREN  ==> 20 X RENOVACION
      *
           02 WK-INTERE       PIC  9(02)          VALUE  0.
           02 WK-INTPND       PIC  9(02)          VALUE  0.
           02 WK-INTDEV       PIC  9(02)          VALUE  0.
           02 WK-INTREN       PIC  9(02)          VALUE  0.
           02 WK-FERIAD       PIC  9(01)          VALUE  0.
           02 WK-FECFIN       PIC  9(08)          VALUE  ZEROS.
           02 WK-NROQUI       PIC  9(02)          VALUE  0.

      *
      *---
      *--- SWITCHES
      *---

       01 ESTADO-DIAS              PIC X(02) VALUE 'NO'.
          88 SI-HABIL                        VALUE 'SI'.
          88 NO-HABIL                        VALUE 'NO'.

       01 ESTADO-COBRO-INT         PIC X(02) VALUE 'NO'.
          88 SI-COBRO-INTERES                VALUE 'SI'.
          88 NO-COBRO-INTERES                VALUE 'NO'.

       01 ESTADO-CALCULA-INTDEV    PIC X(02) VALUE 'NO'.
          88 CALCULA-INTDEV                  VALUE 'SI'.
          88 NO-CALCULA-INTDEV               VALUE 'NO'.

       01 ESTADO-CALCULO-INTDEV    PIC X(02) VALUE 'NO'.
          88 CALCULA-INTERE                  VALUE 'SI'.
          88 NO-CALCULA-INTERE               VALUE 'NO'.


      *    COPY RUTINA TC TRATAMIENTO DE FECHAS
           COPY TCWC1400.

      *    COPY RUTINA TC TRATAMIENTO DE FECHAS
           COPY TCWC1750.

      *    ARCHIVO DE TARIFA.
           COPY 'STS$TCAR'.

      *200804211-INI
      *    PENDIENTE DE CARTERA.
           COPY DIPENDTE.
      *
      *200804211-FIN

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
           COPY  DIWC351.

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

           MOVE '00'                TO  W351-CODRETOR

           SET NO-COBRO-INTERES     TO  TRUE
           SET NO-CALCULA-INTDEV    TO  TRUE
           SET NO-CALCULA-INTERE    TO  TRUE
           MOVE W351-TIPCTA         TO  WS-TIPCT2
           MOVE W351-CODMON         TO  WS-MONEDA
           MOVE W351-FILLER(11:28)  TO  WS-ISER-INTER


           IF  (W351-COD-CARTERA NOT  NUMERIC)  OR
               (W351-COD-CARTERA EQUAL SPACES)
                MOVE '20'                TO  W351-CODRETOR
                MOVE 'CNE0115'           TO  W351-CODERROR
                MOVE 'ERROR COD.CARTERA' TO  W351-REFERENC
                PERFORM  900-FIN-PROGRAMA.

           PERFORM  110-VERIFICA-COBRO-INTERES.


       110-VERIFICA-COBRO-INTERES.
      *--------------------------*

      *200804211-INI
           PERFORM 220-LEE-PENDIENTE
      *200804211-INI
           IF W351-FECTRA   >  W351-FECVEN
              PERFORM 111-VERIFICA-FECVEN-DIAUTIL
              IF NO-HABIL
                 IF W351-FECTRA     =  WS-FEC-SGTE-HABIL
                    SET NO-COBRO-INTERES  TO TRUE
                 ELSE
                    SET SI-COBRO-INTERES  TO TRUE
                 END-IF
              ELSE
                 SET SI-COBRO-INTERES  TO TRUE
              END-IF
              PERFORM 112-CALCULA-NRO-DIAS
           END-IF.

           IF W351-FECTRA < W351-FECVEN
              PERFORM 112-CALCULA-NRO-DIAS-DEV
           END-IF.

       111-VERIFICA-FECVEN-DIAUTIL.
      *---------------------------*

           INITIALIZE TCWC1400
           MOVE '2'                    TO W140-CDOPCIO
           MOVE W351-FECVEN(9:2)       TO W140-FHTRAT1(1:2)
           MOVE W351-FECVEN(6:2)       TO W140-FHTRAT1(4:2)
           MOVE W351-FECVEN(1:4)       TO W140-FHTRAT1(7:4)
           MOVE '.'                    TO W140-FHTRAT1(3:1)
                                          W140-FHTRAT1(6:1)
           MOVE '1'                    TO W140-TRSABADO
      *200804211-INI
      *    MOVE '3'                    TO W140-TRATFEST
           MOVE '0011'                 TO W140-CDEMPRES
           IF (PCAR-PLACOB = 0901 OR 0904 OR 0905 OR 0906 OR  0907)
               OR (PCAR-PLACOB > 3000)
                MOVE '3'               TO W140-TRATFEST
           ELSE
                IF PCAR-PLACOB = 0902
                   MOVE  0777              TO W140-STOFICI
                   MOVE '4'                TO W140-TRATFEST
                   MOVE '0011'             TO W140-STBANCO
                ELSE
                   MOVE PCAR-PLACOB        TO W140-STOFICI
                   MOVE '4'                TO W140-TRATFEST
                   MOVE '0011'             TO W140-STBANCO
               END-IF
           END-IF
      *200804211-FIN
           CALL 'TC9C120C' USING TCWC1400

           EVALUATE W140-CDRETORN
           WHEN '00'
                 IF W140-TIPODIA  = 'H'
                    SET  SI-HABIL       TO TRUE
                 ELSE
                    SET  NO-HABIL       TO TRUE
                 END-IF
                 MOVE W140-FHHABIL(1:2) TO WS-FEC-SGTE-HABIL(9:2)
                 MOVE W140-FHHABIL(4:2) TO WS-FEC-SGTE-HABIL(6:2)
                 MOVE W140-FHHABIL(7:4) TO WS-FEC-SGTE-HABIL(1:4)
                 MOVE '-'               TO WS-FEC-SGTE-HABIL(5:1)
                                           WS-FEC-SGTE-HABIL(8:1)
                 MOVE W140-FHHABAN(1:2) TO WS-FEC-ANTE-HABIL(9:2)
                 MOVE W140-FHHABAN(4:2) TO WS-FEC-ANTE-HABIL(6:2)
                 MOVE W140-FHHABAN(7:4) TO WS-FEC-ANTE-HABIL(1:4)
                 MOVE '-'               TO WS-FEC-ANTE-HABIL(5:1)
                                           WS-FEC-ANTE-HABIL(8:1)

           WHEN OTHER
                MOVE '98'                    TO  W351-CODRETOR
                MOVE 'CNE0119'               TO  W351-CODERROR
                MOVE 'ERROR TC9C120C'        TO  W351-REFERENC(1:14)
                MOVE W140-CDRETORN           TO  W351-REFERENC(16:2)
                MOVE W140-TABLENAME          TO  W351-REFERENC(19:10)
                PERFORM  900-FIN-PROGRAMA
           END-EVALUATE.


       112-CALCULA-NRO-DIAS.
      *---------------------*

           INITIALIZE TCWC1750
           MOVE '3'                    TO W175-CDOPCIO
           MOVE W351-FECVEN(9:2)       TO W175-FHGRE1 (1:2)
           MOVE W351-FECVEN(6:2)       TO W175-FHGRE1 (3:2)
           MOVE W351-FECVEN(1:4)       TO W175-FHGRE1 (5:4)

           MOVE W351-FECTRA(9:2)       TO W175-FHGRE2 (1:2)
           MOVE W351-FECTRA(6:2)       TO W175-FHGRE2 (3:2)
           MOVE W351-FECTRA(1:4)       TO W175-FHGRE2 (5:4)
           MOVE 'NA'                   TO W175-TPTRAT

           CALL 'TC8C1220' USING TCWC1750

           EVALUATE W175-CDRETORN
           WHEN '00'
                 MOVE  W175-NUMDIAS    TO WS-NRODIAS

           WHEN OTHER
                MOVE '98'                    TO  W351-CODRETOR
                MOVE 'CNE0119'               TO  W351-CODERROR
                MOVE 'ERROR RUTINA TC8C1220' TO  W351-REFERENC(1:21)
                MOVE W175-CDRETORN           TO  W351-REFERENC(23:2)
                PERFORM  900-FIN-PROGRAMA
           END-EVALUATE.


       112-CALCULA-NRO-DIAS-DEV.
      *------------------------*

           INITIALIZE TCWC1750
           MOVE '3'                    TO W175-CDOPCIO
           MOVE W351-FECTRA(9:2)       TO W175-FHGRE1 (1:2)
           MOVE W351-FECTRA(6:2)       TO W175-FHGRE1 (3:2)
           MOVE W351-FECTRA(1:4)       TO W175-FHGRE1 (5:4)
           MOVE W351-FECVEN(9:2)       TO W175-FHGRE2 (1:2)
           MOVE W351-FECVEN(6:2)       TO W175-FHGRE2 (3:2)
           MOVE W351-FECVEN(1:4)       TO W175-FHGRE2 (5:4)
           MOVE 'NA'                   TO W175-TPTRAT

           CALL 'TC8C1220' USING TCWC1750

           EVALUATE W175-CDRETORN
           WHEN '00'
                 MOVE  W175-NUMDIAS    TO WS-NRODIAS-DEV

           WHEN OTHER
                MOVE '98'                    TO  W351-CODRETOR
                MOVE 'ERROR RUTINA TC8C1220' TO  W351-REFERENC(1:21)
                MOVE W175-CDRETORN           TO  W351-REFERENC(23:2)
                PERFORM  900-FIN-PROGRAMA
           END-EVALUATE.


      *==================*
       200-MODULO-CENTRAL.
      *==================*

           PERFORM 110-CARGA-DATOS.
           PERFORM 120-CALCULO-PRINCIPAL.

           MOVE WS-ISER-INTER  TO  W351-FILLER(11:28).

       110-CARGA-DATOS.
      *---------------*
           IF  W351-TIPCTA = 50
               IF W351-FECTRA < W351-FECVEN
      *           MOVE 20         TO WS-INTDEV
                  SET CALCULA-INTDEV  TO  TRUE
               ELSE
                  IF W351-INDINT    = 'S'
                     IF W351-FECTRA > W351-FECVEN
      *                 IF WK-FERIAD NOT = 1
                        IF SI-COBRO-INTERES
      *                    MOVE 40 TO WS-INTERE
                           SET CALCULA-INTERE  TO  TRUE
                        END-IF
                  END-IF
               END-IF
           ELSE
               IF W351-FECTRA > W351-FECVEN
                  IF SI-COBRO-INTERES
                     IF W351-INDINT = 'N'
                        CONTINUE
                     ELSE
                        IF W351-CAMPAN  =  'S' OR 'S'
      * PROTESTO
                           IF WS-NRODIAS > 8
                              SET CALCULA-INTERE  TO  TRUE
                           END-IF
                        ELSE
                           SET CALCULA-INTERE  TO  TRUE
                        END-IF
                     END-IF
                  END-IF
               END-IF
           END-IF.

      *--------- NO EXISTE TASA PREFERENCIAL PARA ACEPTANTES -
      *-- PARA LA BUSQUEDA DE TARIFA EN ARCHIVO DITARNEW
           MOVE  8100            TO WS-COTAMO
           MOVE  8200            TO WS-COTAVE
 VHF  **   MOVE  8300            TO WS-COTARE.
 VHF       MOVE  8400            TO WS-COTARE.


       120-CALCULO-PRINCIPAL.
      *----------------------*

           MOVE  ZEROS               TO W351-CAR-TASINT.

      *    IF WK-INTERE = 40
           IF CALCULA-INTERE
              PERFORM 121-CALC-INT-VENC-T-VIGENTE
              MOVE  WS-NRODIAS       TO W351-CAR-DIAVEN
              MOVE  WS-INTERES       TO W351-CAR-INTVEN
              MOVE  TCAR-PORCEN      TO W351-CAR-TASINT
              MOVE  TCAR-TIPTAS      TO W351-CAR-TIPTAS
              IF W351-FECTRA > W351-FECVEN
                 PERFORM 122-CALCULO-INTERES-MORA
                 MOVE    WS-NRODIAS  TO W351-CAR-DIAMOR
                 MOVE    WS-INTERES  TO W351-CAR-INTMOR
              END-IF
           END-IF.

      *    IF WK-INTDEV = 20
      *200308030-INI
      *    IF CALCULA-INTDEV
      *       PERFORM 123-CALC-INT-DEVUELTO
      *       IF W351-CAR-TASINT = ZEROS
      *          MOVE TCAR-PORCEN    TO W351-CAR-TASINT
      *          MOVE TCAR-TIPTAS    TO W351-CAR-TIPTAS
      *       END-IF
      *    END-IF
      *200308030-FIN

           MOVE  WS-TASA01     TO  W351-CAR-TASA01.
 VHFI
 AQUI      IF ISER-CAR-INDRENO = 'R'
              PERFORM 910-CONSIS-INTERENOVA


              IF ISER-CAR-TIPMOV = 02
                 MOVE ZEROS             TO WK-INTDEV
                 IF W351-TIPCTA > 0 AND < 60
                    MOVE  20            TO WK-INTREN
                 END-IF
              END-IF

              IF WK-INTREN = 20


                 PERFORM 340-CALC-INT-X-RENOVACION

                 IF W351-TIPCTA = 51 OR 61
                    MOVE TCAR-PORCEN    TO W351-CAR-TASINT
                    MOVE TCAR-TIPTAS    TO W351-CAR-TIPTAS
                 ELSE
                    IF W351-CAR-TASINT = ZEROS
                       MOVE TCAR-PORCEN    TO W351-CAR-TASINT
                       MOVE TCAR-TIPTAS    TO W351-CAR-TIPTAS
                    END-IF
                 END-IF
              END-IF
 VHFF      END-IF.

       121-CALC-INT-VENC-T-VIGENTE.
      *----------------------------*
           MOVE '99999999'            TO WS-FECTER
           MOVE WS-COTAVE             TO WS-CODIGO
           MOVE ZEROS                 TO WS-RANGOS
           PERFORM 129-LECTURA-DITARNEW

           COMPUTE WS-INTERES ROUNDED =  W351-IMPACT  *
                              ((TCAR-FACTOR ** WS-NRODIAS) - 1).

      *      MOVE WS-INTERES          TO XXXXX
      *      EXEC CICS  SYNCPOINT END-EXEC.

       122-CALCULO-INTERES-MORA.
      *------------------------*
           MOVE '99999999'            TO WS-FECTER
           MOVE WS-COTAMO             TO WS-CODIGO
           MOVE ZEROS                 TO WS-RANGOS
           PERFORM 129-LECTURA-DITARNEW

           PERFORM  130-CALCULA-FACTOR

           COMPUTE WS-INTERES ROUNDED = W351-IMPACT  *
                             ((TCAR-FACTOR ** WS-NRODIAS) - 1).

           MOVE  TCAR-PORCEN  TO  WS-TASA01.

      *      MOVE WS-INTERES          TO XXXXX
      *      EXEC CICS  SYNCPOINT END-EXEC.

       123-CALC-INT-DEVUELTO.
      *----------------------*
           IF W351-TIPCAM = 0
              MOVE '98'                    TO  W351-CODRETOR
              MOVE 'CNE0120'               TO  W351-CODERROR
              MOVE 'ERROR TIPO DE CAMBIO ' TO  W351-REFERENC
              PERFORM  900-FIN-PROGRAMA
           END-IF

           IF W351-CODMON = 1
              MOVE 1              TO TCAR-TIPTAS
           ELSE
              MOVE 2              TO TCAR-TIPTAS
           END-IF

           MOVE W351-TIPCAM       TO TCAR-PORCEN

           PERFORM 130-CALCULA-FACTOR

           MOVE WS-NRODIAS-DEV       TO W351-CAR-DIADEV
           MOVE WS-NRODIAS-DEV       TO W351-CAR-DEVDIA
           COMPUTE W351-CAR-INTDEV ROUNDED =  W351-IMPACT *
                        ( 1 - (1 / (TCAR-FACTOR ** WS-NRODIAS-DEV))).

       220-LEE-PENDIENTE.
      *-----------------*
           MOVE W351-COD-CARTERA    TO PCAR-CODAPL
           MOVE W351-FILLER(1:10)   TO PCAR-NUMBCO
      *-- LECTURA DEL ARCHIVO PENDIENTE
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
                MOVE '99'                   TO  W351-CODRETOR
                MOVE 'CNE0116'              TO  W351-CODERROR
                MOVE 'ERROR READ PENDIENTE' TO  W351-REFERENC
                PERFORM  900-FIN-PROGRAMA.


       129-LECTURA-DITARNEW.
      *---------------------*
           IF WS-CODPRE = 00
              MOVE 0100               TO WS-PLAZA
           ELSE
              MOVE W351-PLAORI        TO WS-PLAZA.

           IF (W351-TIPCTA < 44) AND
              (WS-PLAZA = 0100)
              MOVE 01                 TO WS-TIPCT2.
      *-- LECTURA DEL ARCHIVO DITARNEW
           EXEC CICS READ     DATASET   (WS-ARCHIVO-TARI)
                              LENGTH    (WS-LONG-REG-TARI)
                              RIDFLD    (WS-KEYTARIFA)
                              KEYLENGTH (WS-LONG-KEY-TARI)
                              INTO      (REG-D02FTAR)
                              RESP      (RESPONSE)
                              EQUAL
                              NOHANDLE
           END-EXEC.

           IF   RESPONSE = DFHRESP (NORMAL)
                CONTINUE
           ELSE
                MOVE '99'                  TO  W351-CODRETOR
                MOVE 'CNE0116'             TO  W351-CODERROR
                MOVE 'ERROR READ DITARNEW' TO  W351-REFERENC
                PERFORM  900-FIN-PROGRAMA.


       130-CALCULA-FACTOR.
      *-------------------*

           COMPUTE TCAR-FACTOR = ( 1 + TCAR-PORCEN / 100 ) **
                                 ( 1 / 360 ).

      *================*
       900-FIN-PROGRAMA.
      *================*

           EXEC  CICS
                 RETURN
           END-EXEC.

      *================================================================*
      *                          910-CONSIS-INTERENOVA
      *================================================================*
       910-CONSIS-INTERENOVA.
      *=====================*

      *-- PARA RENOVACIONES - CHEQUEA  NUEVA FECHA DE VENCIMIENTO -----*
           MOVE  01      TO   ISER-CAR-TIPMOV.
           IF ISER-CAR-NUEVEN IS NOT = SPACES
              IF ISER-CAR-NUEIMP > ZEROS
                 MOVE 02 TO   ISER-CAR-TIPMOV
                 IF PCAR-TIPCTA = 50 AND W351-CAMPAN = 'X'
                   MOVE '98'                   TO  W351-CODRETOR
                   MOVE 'DIE0005'              TO  W351-CODERROR
                   MOVE 'CTA.ADELAN.FACT.NO P' TO  W351-REFERENC(1:20)
                   MOVE 'ERMI.RENOV'           TO  W351-REFERENC(21:10)
                   PERFORM  900-FIN-PROGRAMA
                 END-IF
              ELSE
                   MOVE '98'                   TO  W351-CODRETOR
                   MOVE 'DIE0100'              TO  W351-CODERROR
                   MOVE 'EL NUEVO IMPORTE EST' TO  W351-REFERENC(1:20)
                   MOVE 'A EN CEROS'           TO  W351-REFERENC(21:10)
                   PERFORM  900-FIN-PROGRAMA
              END-IF
           END-IF.

      *    IF ISER-CAR-NUEIMP > ZEROS
      *       IF ISER-CAR-NUEVEN IS NOT = SPACES
      *201207017-INI  RENOVACION FACULTADES
      *          MOVE 02 TO   ISER-CAR-TIPMOV
      *          IF (PCAR-TIPCTA = 50) AND
      *             (ISER-CAR-NUEIMP = PCAR-IMPACT)
      *             PERFORM 500-VERIFICA-FACULTAD
      *             IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *                MOVE 948                     TO ISER-CODRET
      *                MOVE 'ERROR LINK QG7C0010'   TO ISER-WORKAREA
      *                PERFORM  500-FIN-DE-PROGRAMA
      *             END-IF
      *             EVALUATE QGEC0010-CODRETORN
      *               WHEN '1'
      *                  CONTINUE
      *               WHEN OTHER
      *                  MOVE 948                     TO ISER-CODRET
      *                  MOVE 'ERROR, NO TIENE FACULTAD '
      *                                            TO ISER-WORKAREA
      *                  PERFORM  500-FIN-DE-PROGRAMA
      *             END-EVALUATE
      *
      *          END-IF
      *       ELSE
      *          MOVE 0411         TO  ISER-CODRET
      *          PERFORM  500-FIN-DE-PROGRAMA
      *       END-IF
      *    END-IF.

           IF ISER-CAR-TIPMOV = 02
 BR   *       IF ISER-CAR-NUEVEN < W351-FECTRA
              IF ISER-CAR-NUEVEN < W351-FECTRA
                   MOVE '98'                   TO  W351-CODRETOR
                   MOVE 'DIE0807'              TO  W351-CODERROR
                   MOVE 'NUEVO VCMTO < FECHA ' TO  W351-REFERENC(1:20)
                   MOVE 'TRANSACCIO'           TO  W351-REFERENC(21:10)
                   PERFORM  900-FIN-PROGRAMA
              END-IF

      *       IF ISER-CAR-REBAJA > 0
      *          IF ISER-CAR-REBAJA > (PCAR-IMPACT - ISER-CAR-NUEIMP)
      *                            OR
      *             ISER-CAR-REBAJA = (PCAR-IMPACT - ISER-CAR-NUEIMP)
      *             MOVE 0414              TO ISER-CODRET
      *             PERFORM 500-FIN-DE-PROGRAMA
      *          END-IF
      *       END-IF

      *       IF W351-FECTRA > PCAR-FECVEN
              IF W351-FECTRA > PCAR-FECVEN
                 INITIALIZE TCWC1750
      *               MMMMDDAA
                 MOVE PCAR-FECVEN(7:2)       TO W175-FHGRE1 (1:2)
                 MOVE PCAR-FECVEN(5:2)       TO W175-FHGRE1 (3:2)
                 MOVE PCAR-FECVEN(1:4)       TO W175-FHGRE1 (5:4)
      *
                 MOVE W351-FECTRA(9:2)       TO W175-FHGRE2 (1:2)
                 MOVE W351-FECTRA(6:2)       TO W175-FHGRE2 (3:2)
                 MOVE W351-FECTRA(1:4)       TO W175-FHGRE2 (5:4)

                 PERFORM 920-NRODIAS-DOSFECHAS

                 IF WS-NRODIAS  > 90
 OJO  **            AND   ISER-CODTRA = 3410
                   MOVE '98'                   TO  W351-CODRETOR
                   MOVE 'DIE0888'              TO  W351-CODERROR
                   MOVE 'NRO DIAS MAYOR A 90 ' TO  W351-REFERENC(1:20)
                   MOVE 'DIAS      '           TO  W351-REFERENC(21:10)
                   PERFORM  900-FIN-PROGRAMA
                 END-IF
              ELSE
                 INITIALIZE TCWC1750
      *               MMMMDDAA
                 MOVE W351-FECTRA(9:2)       TO W175-FHGRE1 (1:2)
                 MOVE W351-FECTRA(6:2)       TO W175-FHGRE1 (3:2)
                 MOVE W351-FECTRA(1:4)       TO W175-FHGRE1 (5:4)
      *
                 MOVE ISER-CAR-NUEVEN(9:2)       TO W175-FHGRE2 (1:2)
                 MOVE ISER-CAR-NUEVEN(6:2)       TO W175-FHGRE2 (3:2)
                 MOVE ISER-CAR-NUEVEN(1:4)       TO W175-FHGRE2 (5:4)

                 PERFORM 920-NRODIAS-DOSFECHAS

                 IF WS-NRODIAS  > 90
      *             AND ISER-CODTRA = 3410
                   MOVE '98'                   TO  W351-CODRETOR
                   MOVE 'DIE0115'              TO  W351-CODERROR
                   MOVE 'NRO DIAS > A 90 DIAS' TO  W351-REFERENC(1:20)
                   MOVE ' XXXXXXXX '           TO  W351-REFERENC(21:10)
                   PERFORM  900-FIN-PROGRAMA
                 END-IF

              END-IF
              IF ISER-CAR-NUEIMP > PCAR-IMPACT
                   MOVE '98'                   TO  W351-CODRETOR
                   MOVE 'DIE0115'              TO  W351-CODERROR
                   MOVE 'NVO.IMPORTE > IMP.AC' TO  W351-REFERENC(1:20)
                   MOVE 'TUAL      '           TO  W351-REFERENC(21:10)
                   PERFORM  900-FIN-PROGRAMA
              END-IF

      *200506063-INI
      *       MOVE ISER-CAR-NUEVEN      TO WK-INUEVEN(3:6)
      *       IF ISER-CAR-NUEVEN(1:2) > 50
      *          MOVE  19              TO  WK-INUEVEN(1:2)
      *       ELSE
      *          MOVE  20              TO  WK-INUEVEN(1:2)
      *       END-IF
      *200506063-FIN
      *       MOVE WK-INUEANO           TO WK-NUEANO
      *       MOVE WK-INUEMES           TO WK-NUEMES
      *       MOVE WK-INUEDIA           TO WK-NUEDIA
      *       MOVE WK-NUEVEN            TO R4-FFECHA
      *       PERFORM 495-CHEQUEA-FECHA
      *
      *       IF  WK-NUEVEN < WK-FECWORK  OR SW-FECOKY = 'N' OR
      *
      *          (((PCAR-TIPCTA > 0 AND < 51) OR PCAR-TIPCTA = 56) AND
      *          WK-NUEVEN = WK-FECWORK )
      *           OR  WK-NUEVEN < W351-FECTRA
      *
      *          MOVE 0411              TO ISER-CODRET
      *          PERFORM 500-FIN-DE-PROGRAMA
      *
      *       END-IF
      *    END-IF.
      *
           IF ISER-CAR-TIPMOV = 02
                 IF PCAR-IMPORI = 0
                    MOVE PCAR-IMPACT       TO PCAR-IMPORI
                 END-IF

                 COMPUTE WK-MINREN = (PCAR-PORREN * PCAR-IMPORI) / 100

                 IF ((PCAR-IMPACT - ISER-CAR-NUEIMP) < WK-MINREN)
      *             AND (ISER-CODTRA = 3410)
                   MOVE '98'                   TO  W351-CODRETOR
                   MOVE 'DIE0115'              TO  W351-CODERROR
                   MOVE 'ERR.IMP.MINIMO DE RE' TO  W351-REFERENC(1:20)
                   MOVE 'NOVACION  '           TO  W351-REFERENC(21:10)
                   PERFORM  900-FIN-PROGRAMA
                 END-IF
           END-IF.
      *       END-IF.
      *    END-IF.

      *--- SOLO PARA FACTORING CHEQUEA QUE NO SEA RENOVACION ----------*
           IF (ISER-CAR-TIPMOV = 02) AND
              (PCAR-TIPCTA     = 80 OR 30)
                MOVE '98'                    TO  W351-CODRETOR
                MOVE 'DIE8100'               TO  W351-CODERROR
                MOVE 'FACTORIN NO PUEDE SE'  TO  W351-REFERENC(1:20)
                MOVE 'R RENOVADO'            TO  W351-REFERENC(21:10)
                PERFORM  900-FIN-PROGRAMA
           END-IF

      *--- NO PERMITE RENOVAR FACTURAS --------------------------------*
           IF (    ISER-CAR-TIPMOV = 02  ) AND
              (    PCAR-TIPCTA     < 51  ) AND
              (PCAR-TDOCUM = '03' OR '04')
                MOVE '98'                    TO  W351-CODRETOR
                MOVE 'DIE0115'               TO  W351-CODERROR
                MOVE 'FACT.NO PUEDEN SER R'  TO  W351-REFERENC(1:20)
                MOVE 'ENOVADAS  '            TO  W351-REFERENC(21:10)
                PERFORM  900-FIN-PROGRAMA
           END-IF.

       920-NRODIAS-DOSFECHAS.
      *---------------------*
           MOVE '3'                    TO W175-CDOPCIO
           MOVE 'NA'                   TO W175-TPTRAT

           CALL 'TC8C1220' USING TCWC1750

           EVALUATE W175-CDRETORN
           WHEN '00'
                 MOVE  W175-NUMDIAS    TO WS-NRODIAS
           WHEN OTHER
                MOVE '98'                    TO  W351-CODRETOR
                MOVE 'CNE0119'               TO  W351-CODERROR
                MOVE 'ERROR RUTINA TC8C1220' TO  W351-REFERENC(1:21)
                MOVE W175-CDRETORN           TO  W351-REFERENC(23:2)
                PERFORM  900-FIN-PROGRAMA
           END-EVALUATE.

AQUI1  340-CALC-INT-X-RENOVACION.
      *--------------------------*
           MOVE ZEROS                TO W351-CAR-DIANVO W351-CAR-INTNVO
           IF (PCAR-TIPCTA < 51) OR
              (PCAR-TIPCTA = 56)
      *       MOVE '99999999'            TO WS-FECTER
              MOVE '00000000'            TO WS-FECTER
              MOVE WS-COTARE             TO WS-CODIGO
              MOVE ZEROS                 TO WS-RANGOS
      *       PERFORM  480-READ-RANDOM-TARIFAS
              PERFORM  129-LECTURA-DITARNEW
           ELSE
              IF W351-TIPCAM = 0
                MOVE '98'                    TO  W351-CODRETOR
                MOVE 'CNE0119'               TO  W351-CODERROR
                MOVE 'W351-TIPCAM = CERO  '  TO  W351-REFERENC(1:20)
                MOVE '          '            TO  W351-REFERENC(21:10)
                PERFORM  900-FIN-PROGRAMA
              END-IF
              IF PCAR-CODMON = 1
                 MOVE 1              TO TCAR-TIPTAS
              ELSE
                 MOVE 2              TO TCAR-TIPTAS
              END-IF
              MOVE W351-TIPCAM       TO TCAR-PORCEN
           END-IF

           PERFORM 130-CALCULA-FACTOR

           MOVE PCAR-VENANO    TO WCAR-VENANO
           MOVE PCAR-VENMES    TO WCAR-VENMES
           MOVE PCAR-VENDIA    TO WCAR-VENDIA

           IF WCAR-FECVEN = ISER-CAR-NUEVEN
              MOVE ZEROS             TO WS-NRODIAS
           ELSE
      *       IF PCAR-FECVEN > W351-FECTRA
              IF WCAR-FECVEN > W351-FECTRA
      *          MOVE PCAR-FECVEN TO FECHA-A
                 MOVE PCAR-FECVEN(7:2) TO W175-FHGRE1 (1:2)
                 MOVE PCAR-FECVEN(5:2) TO W175-FHGRE1 (3:2)
                 MOVE PCAR-FECVEN(1:4) TO W175-FHGRE1 (5:4)
              ELSE
                 IF WK-FERIAD = 0
 OJO  *  SUGERENCIA MARITZA OBISPO
                 OR W351-FECTRA < ISER-CAR-NUEVEN
      ***********************************
      *             MOVE W351-FECTRA     TO FECHA-A
                    MOVE W351-FECTRA(9:2) TO W175-FHGRE1 (1:2)
                    MOVE W351-FECTRA(6:2) TO W175-FHGRE1 (3:2)
                    MOVE W351-FECTRA(1:4) TO W175-FHGRE1 (5:4)
                 ELSE
      *             MOVE PCAR-FECVEN TO FECHA-A
                    MOVE PCAR-FECVEN(7:2) TO W175-FHGRE1 (1:2)
                    MOVE PCAR-FECVEN(5:2) TO W175-FHGRE1 (3:2)
                    MOVE PCAR-FECVEN(1:4) TO W175-FHGRE1 (5:4)
                 END-IF
              END-IF

           END-IF
      *200506063-INI
      *    MOVE ISER-CAR-NUEVEN      TO FECHA-V
           MOVE ISER-CAR-NUEVEN(9:2) TO W175-FHGRE2 (1:2)
           MOVE ISER-CAR-NUEVEN(6:2) TO W175-FHGRE2 (3:2)
           MOVE ISER-CAR-NUEVEN(1:4) TO W175-FHGRE2 (5:4)
      *200506063-FIN

           PERFORM 920-NRODIAS-DOSFECHAS

      *    IF (PCAR-TIPCTA < 51 OR = 56)
 OJO  ******  AND (ISER-CODTRA = 3410)
      *       IF (PCAR-PLAACT > 0999 AND WS-NRODIAS < 30) OR
      *          (PCAR-PLAACT < 1000 AND WS-NRODIAS < 10)
      *           MOVE '98'                   TO  W351-CODRETOR
      *           MOVE 'DIE0115'              TO  W351-CODERROR
      *           MOVE 'PLAACT NO SINCRINIZA' TO  W351-REFERENC(1:20)
      *           MOVE ' NRO.DIAS '           TO  W351-REFERENC(21:10)
      *           PERFORM  900-FIN-PROGRAMA
      *       END-IF
      *    END-IF

      *    IF (PCAR-TIPCTA = 50 OR 56) AND
 OJO  ******  (ISER-CODTRA = 3410)     AND
      *       WS-NRODIAS > 30
      *       MOVE '98'                   TO  W351-CODRETOR
      *       MOVE 'DIE0115'              TO  W351-CODERROR
      *       MOVE 'TIPCTA NO SINCRINIZA' TO  W351-REFERENC(1:20)
      *       MOVE ' NRO.DIAS '           TO  W351-REFERENC(21:10)
      *       PERFORM  900-FIN-PROGRAMA
      *    END-IF

           MOVE WS-NRODIAS           TO W351-CAR-DIANVO.


           COMPUTE W351-CAR-INTNVO ROUNDED =  ISER-CAR-NUEIMP *
                        ( 1 - (1 / (TCAR-FACTOR ** W351-CAR-DIANVO))).

      *    MOVE  TCAR-PORCEN  TO  WK-TASA02.
