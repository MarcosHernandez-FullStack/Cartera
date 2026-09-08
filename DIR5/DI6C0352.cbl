       IDENTIFICATION DIVISION.
      *========================*

       PROGRAM-ID.    DI6C0352.
      ******************************************************************
      *                                                                *
      *   CALCULA COMISIONES PARA LA CANCELACION DE CARTERA            *
      *                                                                *
      ******************************************************************
      * REFERENCIA    FECHA      PROGRAMADOR  DESCRIPCION              *
      *                                                                *
      * 200210109    SETIE-2003  JUAN MEDINA  NUEVO COBRO PROTESTO     *
      * 200310088    OCTUB-2003  COBRO PORTES AUTOMATICOS              *
      * 200804211    MAYO -2008  FERIADOS                              *
      *                                                                *
      ******************************************************************

       ENVIRONMENT DIVISION.
      *====================*

       CONFIGURATION SECTION.
      *-----------------------*
         SOURCE-COMPUTER. IBM-3090.
         OBJECT-COMPUTER. IBM-3090.

       DATA DIVISION.
      *==============*

       WORKING-STORAGE SECTION.
      *-----------------------*

       01  XXX                PIC   9(13)V99 VALUE ZEROS.
       01  FILLER             PIC   X(40) VALUE
                              '** INICIO WORKING-STORAGE SECTION **'.
      *200804211-INI
      *    PENDIENTE DE CARTERA.
           COPY DIPENDTE.
      *
      *200804211-FIN
      *--200210109-INI
      *
       COPY TCWC1750.

       01  WK-VARIABLES.
           02  CON-TC8C1220           PIC X(08) VALUE 'TC8C1220'.
      *
      *--200210109-FIN
      *
       01  RESPONSE                   PIC S9(09)  COMP.
       01  NOMBRES-DE-ARCHIVOS.
           05  WS-ARCHIVO-TARI        PIC  X(08)      VALUE 'DITARNEW'.
           05  WS-ARCHIVO-PDTE        PIC  X(08)      VALUE 'DIPENBCO'.
       01  LONGITUD-DE-REGISTROS.
           05  WS-LONG-REG-TARI       PIC S9(04) COMP VALUE +0132.
           05  WS-LONG-REG-PDTE       PIC S9(04) COMP VALUE +0350.
       01  LONGITUD-DE-CLAVES.
           05  WS-LONG-KEY-TARI       PIC S9(04) COMP VALUE +0021.
           05  WS-LONG-KEY-PDTE       PIC S9(04) COMP VALUE +0010.
      *
       01  VARIABLES.
           02 WS-CAR-ISALDO       PIC S9(13)V99  VALUE +0.
           02 WS-COMISION         PIC S9(13)V99  VALUE +0.
           02 WS-INDICA           PIC 9(01)      VALUE 0.
           02 WS-KEYTARIFA.
              03 WS-PLAZA         PIC 9(4)       VALUE 0100.
              03 WS-MONEDA        PIC 9          VALUE 0.
              03 WS-TIPCT2        PIC 99         VALUE 0.
              03 WS-CODTAR        PIC X(4).
              03 WR-CODTAR        REDEFINES WS-CODTAR.
                 05 WS-TARIFA     PIC 9(02).
                 05 WS-PREFER     PIC 9(02).
              03 WS-RANTIC        PIC 9(2)       VALUE 0.
              03 WS-FECHA         PIC X(8)       VALUE ' '.
           02 WS-ISER-INTER.
              03 ISER-CAR-NUEVEN        PIC X(10).
              03 ISER-CAR-NUEIMP        PIC 9(13)V99.
              03 ISER-CAR-INDRENO       PIC X(01).
              03 ISER-CAR-TIPMOV        PIC 9(02).

      *---
      *--- SWITCHES
      *---


      *    COPY RUTINA TC TRATAMIENTO DE FECHAS
           COPY TCWC0400.

      *    ARCHIVO DE TARIFA.
           COPY 'STS$TCAR'.

      *--200210109-INI
      *
       01  WK-DATOS-FECHA.
           02  WK-DD               PIC S9(5)   COMP-3.
           02  WK-CDDIASEM         PIC  X(01).
           02  WK-CDDIASAN         PIC  X(01).
           02  WK-FHHABAN.
               04 WK-DIABAN        PIC  X(02).
               04 FILLER           PIC  X(01) VALUE '.'.
               04 WK-MESBAN        PIC  X(02).
               04 FILLER           PIC  X(01) VALUE '.'.
               04 WK-ANOBAN        PIC  X(04).

           02  WK-FECANT           PIC  9(08).
           02  WK-TIPODIA          PIC  X(01).
           02  WK-COBRAR           PIC  X(01).
           02  WK-HORINI           PIC  X(06).
           02  WK-FECTRA.
               04 WK-FECHOY        PIC  X(10).
               04 FILLER           REDEFINES WK-FECHOY.
                  06 WK-ANOHOY     PIC  9(04).
                  06 FILLER        PIC  X(01).
                  06 WK-MESHOY     PIC  9(02).
                  06 FILLER        PIC  X(01).
                  06 WK-DIAHOY     PIC  9(02).

       01  WK-OTRAS-FECHA.
           02 WK-FECPRO.
              04 WK-DIAPRO         PIC  X(02).
              04 FILLER            PIC  X(01) VALUE '.'.
              04 WK-MESPRO         PIC  X(02).
              04 FILLER            PIC  X(01) VALUE '.'.
              04 WK-ANOPRO         PIC  X(04).
      *
      *--200210109-FIN

       01  FILLER             PIC   X(40) VALUE
                              '** FINAL WORKING-STORAGE SECTION **'.
      *
       LINKAGE SECTION.
      *---------------*
       01  DFHCOMMAREA.
           COPY  DIWC352.

       PROCEDURE DIVISION.
      *==================*
       000-INICIO.
      *-----------*

           PERFORM  100-INICIO-PROGRAMA.
           PERFORM  200-MODULO-CENTRAL.
           PERFORM  900-FIN-PROGRAMA.

       100-INICIO-PROGRAMA.
      *-------------------*

           MOVE '00'                       TO  W352-CODRETOR
           MOVE ZEROS                      TO  WS-FECHA

           IF  (W352-COD-CARTERA NOT  NUMERIC)  OR
               (W352-COD-CARTERA EQUAL SPACES)
                MOVE '20'                  TO  W352-CODRETOR
                MOVE 'CNE0115'             TO  W352-CODERROR
                MOVE 'ERROR COD.CARTERA'   TO  W352-REFERENC
                PERFORM  900-FIN-PROGRAMA
           END-IF.

           IF  W352-DEVDOC NOT = 'S' AND 'N'
                MOVE '20'                  TO  W352-CODRETOR
                MOVE 'CNE0115'             TO  W352-CODERROR
                MOVE 'ERROR DEV.DOCUMENTO' TO  W352-REFERENC
                PERFORM  900-FIN-PROGRAMA
           END-IF.

      *--200210109-INI
      *
           INITIALIZE                 WK-DATOS-FECHA.
           MOVE  W352-FECTRA          TO  WK-FECTRA.
           MOVE  WK-ANOHOY            TO  WK-ANOPRO.
           MOVE  WK-MESHOY            TO  WK-MESPRO.
           MOVE  WK-DIAHOY            TO  WK-DIAPRO.
           MOVE  W352-HORINI          TO  WK-HORINI.
           MOVE W352-FILLER(11:28)    TO  WS-ISER-INTER.
      *
      *--200210109-FIN

       200-MODULO-CENTRAL.
      *-------------------*

           MOVE W352-TRASNC           TO WS-CODTAR
           MOVE ZEROS                 TO WS-PREFER
           MOVE WS-CODTAR             TO W352-TRASNC
           MOVE W352-TRASBC           TO WS-CODTAR
           MOVE ZEROS                 TO WS-PREFER
           MOVE WS-CODTAR             TO W352-TRASBC
           MOVE W352-TRASBN           TO WS-CODTAR
           MOVE ZEROS                 TO WS-PREFER
           MOVE WS-CODTAR             TO W352-TRASBN
           MOVE W352-COMTRA           TO WS-CODTAR
           MOVE ZEROS                 TO WS-PREFER
           MOVE WS-CODTAR             TO W352-COMTRA
           MOVE W352-COMACE           TO WS-CODTAR
           MOVE ZEROS                 TO WS-PREFER
           MOVE WS-CODTAR             TO W352-COMACE
           MOVE W352-COMCOB           TO WS-CODTAR
           MOVE ZEROS                 TO WS-PREFER
           MOVE WS-CODTAR             TO W352-COMCOB
           MOVE W352-COMCTE           TO WS-CODTAR
           MOVE ZEROS                 TO WS-PREFER
           MOVE WS-CODTAR             TO W352-COMCTE
           MOVE W352-COMPRO           TO WS-CODTAR
           MOVE ZEROS                 TO WS-PREFER
           MOVE WS-CODTAR             TO W352-COMPRO
           MOVE W352-INTERE           TO WS-CODTAR
           MOVE ZEROS                 TO WS-PREFER
           MOVE WS-CODTAR             TO W352-INTERE
           MOVE W352-COMISI           TO WS-CODTAR
           MOVE ZEROS                 TO WS-PREFER
           MOVE WS-CODTAR             TO W352-COMISI

      ****************************************************
      *    SE COBRA PORTES EN CASO QUE EL CLIENTE DECIDA
      *    QUE LE DEVUELVAN EL DOCUMENTO (LETRA)

      *--200310088-INI
      *    IF W352-DEVDOC = 'S'
      *       PERFORM 210-CALCULO-PORTES
      *    END-IF
      *--200310088-FIN
      ****************************************************

           PERFORM 220-LEE-PENDIENTE
           PERFORM 220-CALCULO-PROTESTO

 VHFI * RENOVACION DE FACTURAS ELECTRONICAS
           IF ISER-CAR-INDRENO = 'R'
              PERFORM 230-CALCULA-GESTION
           END-IF
 VHFF *
           MOVE ZEROES   TO  WS-CAR-ISALDO
           IF ISER-CAR-INDRENO = 'R'
              COMPUTE WS-CAR-ISALDO = W352-CAR-IMPACT -
                                        ISER-CAR-NUEIMP
              ADD W352-CAR-COMCOB W352-CAR-PORTES W352-CAR-COMPRO
                  W352-CAR-COMTRF                 W352-CAR-IMPISC
                  W352-CAR-IMPINT W352-CAR-INTVEN W352-CAR-INTMOR
                  W352-CAR-INTNVO WS-CAR-ISALDO   W352-CAR-GASNOT
                  W352-CAR-COMNVO W352-CAR-COMMOR
              TO  W352-IMPTRA
           ELSE
              ADD W352-CAR-COMCOB W352-CAR-PORTES W352-CAR-COMPRO
                  W352-CAR-COMTRF                 W352-CAR-IMPISC
                  W352-CAR-IMPINT W352-CAR-INTVEN W352-CAR-INTMOR
                  W352-CAR-INTNVO W352-CAR-IMPACT W352-CAR-GASNOT
                  W352-CAR-COMNVO W352-CAR-COMMOR
              TO  W352-IMPTRA
           END-IF

      *--200210109-INI
           MOVE  WK-COBRAR    TO  W352-COBRAR.
      *--200210109-FIN

      *    SUBTRACT W352-CAR-INTDEV   FROM  W352-MONCON
           SUBTRACT W352-CAR-INTDEV   FROM  W352-IMPTRA.

      *    MOVE W352-MONCON           TO ISER-IMPTRA
      *------------ VERIFICA CARGO EN CUENTA --------------------------*
      *
      *    IF W532-INDAUT = 0
      *          PERFORM 250-VERIFICA-CHEQUE-CTACTE
 ******          MOVE 1               TO ISER-INDSOB
 ******-- IMPORTE A AUTORIZAR         TO ISER-CODRET
 ******          MOVE 428             TO ISER-CODRET
      *    END-IF.

       230-CALCULA-GESTION.
      *--------------------*

           IF W352-INTERE > 0
              IF ISER-CAR-TIPMOV = 02  AND  W352-TIPCTA  =  50 OR 56
                 MOVE W352-INTERE     TO WS-CODTAR
      *201207017-INI
      *          MOVE ZEROS           TO WK-PREFER
      *201207017-FIN
      *          MOVE ISER-CAR-NUEIMP TO WK-IMPORTE
      *
                 PERFORM 290-CALCULO-COMISION

      **         MOVE WK-COMISION     TO W352-CAR-COMNVO
                 MOVE WS-COMISION     TO W352-CAR-COMNVO
      **         MOVE ZEROS           TO WK-COMISION WK-IMPORTE
              END-IF
           END-IF.

       210-CALCULO-PORTES.
      *------------------*
      *    MOVE  W352-PORTES     TO XXX
      *    EXEC CICS SYNCPOINT END-EXEC.

           IF W352-PORTES > 0
              MOVE W352-PORTES        TO WS-CODTAR
              IF (W352-PORTES > 2899 AND < 2950)
                 MOVE ZEROS           TO WS-PREFER
              ELSE
                 MOVE 0100            TO W352-PLAORI
              END-IF
              PERFORM 290-CALCULO-COMISION
              MOVE WS-COMISION        TO W352-CAR-PORTES
           END-IF.

       220-CALCULO-PROTESTO.
      *--------------------*

           IF W352-CAR-INSCOB = 06 OR 07 OR 08 OR 09
              IF  W352-CAR-INDPRO  =  2
      *--200210109-INI
                  MOVE  'S'    TO  WK-COBRAR
                  PERFORM  700-VALIDA-COBRO
                  IF  WK-COBRAR = 'S'
                      MOVE 3500               TO WS-CODTAR
                      PERFORM 290-CALCULO-COMISION
                      MOVE WS-COMISION        TO W352-CAR-COMPRO
                  END-IF
      *--200210109-FIN
              END-IF
           ELSE
              IF W352-CAR-GASNOT > ZEROS AND W352-COMPRO > 0
                 IF  W352-CAR-INDPRO  =  4
                     CONTINUE
                 ELSE
      *--200210109-INI
                     MOVE  'S'    TO  WK-COBRAR
                     IF  W352-CAR-INDPRO = 2
                         PERFORM  700-VALIDA-COBRO
                     END-IF
                     IF  WK-COBRAR = 'S'
                         MOVE W352-COMPRO     TO WS-CODTAR
                         PERFORM 290-CALCULO-COMISION
                         MOVE WS-COMISION     TO W352-CAR-COMPRO
                     END-IF
      *--200210109-FIN
                 END-IF
              END-IF
           END-IF.

       290-CALCULO-COMISION.
      *---------------------*
           MOVE 0                     TO WS-COMISION
           MOVE 0                     TO WS-INDICA
           MOVE 0                     TO WS-RANTIC
           PERFORM VARYING WS-RANTIC FROM 1 BY 1 UNTIL WS-INDICA > 0
            PERFORM 291-LEE-CTARIFA
            IF (W352-CAR-IMPACT       > TCAR-LIMMEN OR
                W352-CAR-IMPACT       = TCAR-LIMMEN)    AND
               (W352-CAR-IMPACT       < TCAR-LIMMAY OR
                W352-CAR-IMPACT       = TCAR-LIMMAY)
               MOVE 1                  TO WS-INDICA
            END-IF
           END-PERFORM.

           IF ISER-CAR-INDRENO = 'R'
              COMPUTE WS-COMISION ROUNDED = ( ISER-CAR-NUEIMP  *
                                             TCAR-PORCEN ) / 100
           ELSE

              COMPUTE WS-COMISION ROUNDED = ( W352-CAR-IMPACT  *
                                             TCAR-PORCEN ) / 100
           END-IF


           IF WS-COMISION  < TCAR-MINIMO
              MOVE TCAR-MINIMO        TO WS-COMISION
           END-IF.
      *    MOVE  TCAR-MINIMO     TO XXX
      *    EXEC CICS SYNCPOINT END-EXEC

      *    MOVE  TCAR-MAXIMO     TO XXX
      *    EXEC CICS SYNCPOINT END-EXEC
           IF WS-COMISION  > TCAR-MAXIMO
              MOVE TCAR-MAXIMO        TO WS-COMISION
           END-IF.


       291-LEE-CTARIFA.
      *---------------*
           IF WS-PREFER = 00
              MOVE 0100               TO WS-PLAZA
           ELSE
              MOVE W352-PLAORI        TO WS-PLAZA.

           MOVE W352-CODMON           TO WS-MONEDA
           MOVE W352-TIPCTA           TO WS-TIPCT2
           IF (WS-TIPCT2       < 44) AND
              (WS-PLAZA      = 0100)
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
                MOVE '99'                  TO  W352-CODRETOR
                MOVE 'CNE0116'             TO  W352-CODERROR
                MOVE 'ERROR READ DITARNEW' TO  W352-REFERENC
                PERFORM  900-FIN-PROGRAMA.

       220-LEE-PENDIENTE.
      *-----------------*
      *    MOVE W351-COD-CARTERA    TO PCAR-CODAPL
           MOVE W352-FILLER(1:10)   TO PCAR-NUMBCO
      *-- LECTURA DEL ARCHIVO PENDIENTE
           EXEC CICS READ     DATASET   (WS-ARCHIVO-PDTE)
                              LENGTH    (WS-LONG-REG-PDTE)
                              RIDFLD    (PCAR-NUMBCO)
      *                       RIDFLD    (PCAR-KEYPEN)
                              KEYLENGTH (WS-LONG-KEY-PDTE)
                              INTO      (REG-PENDIENTE-CARTERA)
                              RESP      (RESPONSE)
                              EQUAL
                              NOHANDLE
           END-EXEC.

      *    IF   RESPONSE = DFHRESP (NORMAL)
      *         CONTINUE
      *    ELSE
      *         MOVE '99'                   TO  W352-CODRETOR
      *         MOVE 'CNE0116'              TO  W352-CODERROR
      *         MOVE 'ERROR READ PENDIENTE' TO  W352-REFERENC
      *         PERFORM  900-FIN-PROGRAMA.
      *

            EVALUATE EIBRESP
              WHEN DFHRESP(NORMAL)
                   CONTINUE
              WHEN DFHRESP(NOTFND)
                   MOVE '99'                   TO  W352-CODRETOR
                   MOVE 'CNE0116'              TO  W352-CODERROR
                   MOVE 'ERROR READ PENDIENTE' TO  W352-REFERENC
                   PERFORM  900-FIN-PROGRAMA
              WHEN DFHRESP(NOTOPEN)
                   MOVE '98'                   TO  W352-CODRETOR
                   MOVE 'CNE0116'              TO  W352-CODERROR
                   MOVE 'ERROR READ PENDIENTE' TO  W352-REFERENC
                   PERFORM  900-FIN-PROGRAMA
              WHEN DFHRESP(DISABLED)
                   MOVE '98'                   TO  W352-CODRETOR
                   MOVE 'CNE0116'              TO  W352-CODERROR
                   MOVE 'ERROR READ PENDIENTE' TO  W352-REFERENC
                   PERFORM  900-FIN-PROGRAMA
              WHEN DFHRESP(DSIDERR)
                   MOVE '97'                   TO  W352-CODRETOR
                   MOVE 'CNE0116'              TO  W352-CODERROR
                   MOVE 'ERROR READ PENDIENTE' TO  W352-REFERENC
                   PERFORM  900-FIN-PROGRAMA
              WHEN DFHRESP(IOERR)
                   MOVE '96'                   TO  W352-CODRETOR
                   MOVE 'CNE0116'              TO  W352-CODERROR
                   MOVE 'ERROR READ PENDIENTE' TO  W352-REFERENC
                   PERFORM  900-FIN-PROGRAMA
              WHEN DFHRESP(ILLOGIC)
                   MOVE '95'                   TO  W352-CODRETOR
                   MOVE 'CNE0116'              TO  W352-CODERROR
                   MOVE 'ERROR READ PENDIENTE' TO  W352-REFERENC
                   PERFORM  900-FIN-PROGRAMA
              WHEN OTHER
                   MOVE '94'                   TO  W352-CODRETOR
                   MOVE 'CNE0116'              TO  W352-CODERROR
                   MOVE 'ERROR READ PENDIENTE' TO  W352-REFERENC
                   PERFORM  900-FIN-PROGRAMA
            END-EVALUATE.
      *--200210109-INI
      *
       700-VALIDA-COBRO.
      *-----------------*
           PERFORM  710-LEER-FECHA.
           PERFORM  720-LEER-FECHA.
           PERFORM  750-RECUPERO-8VO-DIA.

           IF  WK-TIPODIA = 'H'
               PERFORM  705-VALIDA-SI-HABIL
           ELSE
               PERFORM  705-VALIDA-NO-HABIL
           END-IF.
           IF  WK-COBRAR = 'N'
               MOVE  ZEROS   TO  W352-CAR-GASNOT
           END-IF.

       705-VALIDA-SI-HABIL.
      *--------------------*
           IF  WK-CDDIASEM = 1 OR 6 OR 7
               IF  WK-CDDIASEM = 1
                   IF  WK-HORINI < 190001 AND WK-DD < 8
                       MOVE  'N'  TO  WK-COBRAR
                   END-IF
               ELSE
                   IF   WK-DD < 8
                        MOVE  'N'  TO  WK-COBRAR
                   END-IF
               END-IF
           ELSE
               EVALUATE  WK-CDDIASEM
                   WHEN  '2'
                         IF  WK-CDDIASAN NOT = 1
                             IF  WK-HORINI < 190001  AND WK-DD < 8
                                 MOVE  'N'   TO  WK-COBRAR
                             END-IF
                         END-IF
                   WHEN  '3'
                         IF  WK-CDDIASAN NOT = 2 AND 1
                             IF  WK-HORINI < 190001 AND WK-DD < 8
                                 MOVE  'N'   TO  WK-COBRAR
                             END-IF
                         ELSE
                             IF  WK-CDDIASAN = 1
                                 IF  WK-HORINI < 190001 AND WK-DD < 8
                                     MOVE  'N'   TO  WK-COBRAR
                                 END-IF
                             END-IF
                         END-IF
                   WHEN  '4'
                         IF  WK-CDDIASAN NOT = 3 AND 2 AND 1
                             IF  WK-HORINI < 190001 AND WK-DD < 8
                                 MOVE  'N'   TO  WK-COBRAR
                             END-IF
                         ELSE
                             IF  WK-CDDIASAN = 1 OR 2
                                 IF  WK-HORINI < 190001 AND WK-DD < 8
                                     MOVE  'N'   TO  WK-COBRAR
                                 END-IF
                             END-IF
                         END-IF
                   WHEN  '5'
                         IF  WK-CDDIASAN NOT = 4 AND 3 AND 2 AND 1
                             IF  WK-HORINI < 190001 AND WK-DD < 8
                                 MOVE  'N'   TO  WK-COBRAR
                             END-IF
                         ELSE
                             IF  WK-CDDIASAN = 1 OR 2 OR 3
                                 IF  WK-HORINI < 190001 AND WK-DD < 8
                                     MOVE  'N'   TO  WK-COBRAR
                                 END-IF
                             END-IF
                         END-IF
               END-EVALUATE
           END-IF.

       705-VALIDA-NO-HABIL.
      *--------------------*
           EVALUATE  WK-CDDIASEM
               WHEN '2'
                    IF WK-CDDIASAN NOT = 1 AND WK-DD < 8
                       MOVE  'N'   TO  WK-COBRAR
                    END-IF
               WHEN '3'
                    IF WK-CDDIASAN NOT = 2 AND 1 AND WK-DD < 8
                       MOVE  'N'   TO  WK-COBRAR
                    ELSE
                       IF WK-CDDIASAN = 1 AND WK-DD < 8
                          MOVE  'N'   TO  WK-COBRAR
                       END-IF
                    END-IF
               WHEN '4'
                    IF WK-CDDIASAN NOT = 3 AND 2 AND 1 AND WK-DD < 8
                       MOVE  'N'   TO  WK-COBRAR
                    ELSE
                       IF (WK-CDDIASAN = 1 OR 2) AND WK-DD < 8
                           MOVE  'N'   TO  WK-COBRAR
                       END-IF
                    END-IF
               WHEN '5'
                    IF WK-CDDIASAN NOT = 4 AND 3 AND 2 AND 1 AND
                       WK-DD < 8
                       MOVE  'N'   TO  WK-COBRAR
                    ELSE
                       IF (WK-CDDIASAN = 1 OR 2 OR 3) AND WK-DD < 8
                           MOVE  'N'   TO  WK-COBRAR
                       END-IF
                    END-IF
               WHEN OTHER
                    IF WK-DD < 8
                       MOVE  'N'   TO  WK-COBRAR
                    END-IF
           END-EVALUATE.

       710-LEER-FECHA.
      *---------------*
           INITIALIZE W040-ENTRADA  W040-SALIDA.
           MOVE '1'                TO W040-CDOPCIO
           MOVE WK-FECPRO          TO W040-FHTRAT1
           MOVE '0'                TO W040-TRSABADO
           MOVE '3'                TO W040-TRATFEST
           MOVE '0011'             TO W040-CDEMPRES
           EXEC CICS
                LINK PROGRAM ('TC2C1200')
                COMMAREA (TCWC0400)
           END-EXEC

           EVALUATE W040-CDRETORN
              WHEN '00'
                    MOVE  W040-CDDIASEM  TO  WK-CDDIASEM
                    MOVE  W040-TIPODIA   TO  WK-TIPODIA
              WHEN  OTHER
                    MOVE '98'                    TO  W352-CODRETOR
                    MOVE 'CNE0119'               TO  W352-CODERROR
                    MOVE 'ERROR RUTINA TC2C1200' TO  W352-REFERENC(1:21)
                    MOVE W040-CDRETORN           TO  W352-REFERENC(23:2)
                    PERFORM 900-FIN-PROGRAMA
           END-EVALUATE.

       720-LEER-FECHA.
      *---------------*
           INITIALIZE W040-ENTRADA  W040-SALIDA.
           MOVE '2'                TO W040-CDOPCIO
           MOVE WK-FECPRO          TO W040-FHTRAT1
           MOVE '0'                TO W040-TRSABADO
      *    MOVE '3'                TO W040-TRATFEST
           MOVE '0011'             TO W040-STBANCO
      *200804211-INI
           IF (PCAR-PLACOB = 0901 OR 0904 OR 0905 OR 0906 OR  0907)
               OR (PCAR-PLACOB > 3000)
                MOVE '3'                TO W040-TRATFEST
           ELSE
                IF PCAR-PLACOB = 0902
                   MOVE  0777              TO W040-STOFICI
                   MOVE '4'                TO W040-TRATFEST
                   MOVE '0011'             TO W040-STBANCO
                ELSE
                   MOVE PCAR-PLACOB        TO W040-STOFICI
                   MOVE '4'                TO W040-TRATFEST
                   MOVE '0011'             TO W040-STBANCO
                END-IF
           END-IF
           EXEC CICS
                LINK PROGRAM ('TC2C1200')
                COMMAREA (TCWC0400)
           END-EXEC

           EVALUATE W040-CDRETORN
              WHEN '00'
      *            IF W040-TRATFEST = '3'
                      MOVE  W040-CDDIASAN          TO  WK-CDDIASAN
                      MOVE  W040-FHHABAN           TO  WK-FHHABAN
                      MOVE  WK-DIABAN              TO  WK-FECANT (7:2)
                      MOVE  WK-MESBAN              TO  WK-FECANT (5:2)
                      MOVE  WK-ANOBAN              TO  WK-FECANT (1:4)
      *            ELSE
      *              IF W040-TIPODIA = 'H'
      *                MOVE  W040-CDDIASAN          TO  WK-CDDIASAN
      *                MOVE  W040-FHHABAN           TO  WK-FHHABAN
      *                MOVE  WK-DIABAN              TO  WK-FECANT (7:2)
      *                MOVE  WK-MESBAN              TO  WK-FECANT (5:2)
      *                MOVE  WK-ANOBAN              TO  WK-FECANT (1:4)
      *              ELSE
      *                MOVE  W040-CDDIASAN          TO  WK-CDDIASAN
      *                MOVE  W040-FHHABIL           TO  WK-FHHABAN
      *                MOVE  WK-DIABAN              TO  WK-FECANT (7:2)
      *                MOVE  WK-MESBAN              TO  WK-FECANT (5:2)
      *                MOVE  WK-ANOBAN              TO  WK-FECANT (1:4)
      *              END-IF
      *             END-IF
              WHEN  OTHER
                    MOVE '98'                    TO  W352-CODRETOR
                    MOVE 'CNE0119'               TO  W352-CODERROR
                    MOVE 'ERROR RUTINA TC2C1200' TO  W352-REFERENC(1:21)
                    MOVE W040-CDRETORN           TO  W352-REFERENC(23:2)
                    PERFORM 900-FIN-PROGRAMA
           END-EVALUATE.

       750-RECUPERO-8VO-DIA.
      *---------------------*
           INITIALIZE TCWC1750
           MOVE  '3'                   TO  W175-CDOPCIO
           MOVE  W352-FECVEN (1:4)     TO  W175-FHGRE1 (5:4)
           MOVE  W352-FECVEN (6:2)     TO  W175-FHGRE1 (3:2)
           MOVE  W352-FECVEN (9:2)     TO  W175-FHGRE1 (1:2)

           MOVE  WK-FECANT (1:4)       TO  W175-FHGRE2 (5:4)
           MOVE  WK-FECANT (5:2)       TO  W175-FHGRE2 (3:2)
           MOVE  WK-FECANT (7:2)       TO  W175-FHGRE2 (1:2)
           MOVE  'NA'                  TO  W175-TPTRAT

           CALL CON-TC8C1220  USING TCWC1750.

           EVALUATE W175-CDRETORN
               WHEN '00'
                    MOVE  W175-NUMDIAS           TO  WK-DD
               WHEN OTHER
                    MOVE '98'                    TO  W352-CODRETOR
                    MOVE 'CNE0119'               TO  W352-CODERROR
                    MOVE 'ERROR RUTINA TC8C1220' TO  W352-REFERENC(1:21)
                    MOVE W175-CDRETORN           TO  W352-REFERENC(23:2)
                    PERFORM 900-FIN-PROGRAMA
           END-EVALUATE.
      *
      *--200210109-FIN

       900-FIN-PROGRAMA.
      *================*

           EXEC  CICS
                 RETURN
           END-EXEC.
      *
