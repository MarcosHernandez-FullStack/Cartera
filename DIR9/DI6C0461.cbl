       IDENTIFICATION DIVISION.                                         00000010
       PROGRAM-ID. DI6C0461.                                            00000020
      *************************************************************     00000030
      **                                                         **     00000030
      **  CONSULTA  -    PENDIENTES   CARTERA                    **     00000030
      **  CONTINET  -    PAGO LETRAS  Y FACTURAS                 **     00000030
      **6762016047-2 P023664 11-11-2016 CAVALI - INGRESO DE FACTURAS
      **SDA-34398 ENT.& CIB. 15-02-2022 INDI.DE RENOVACI DE FACT **
      *************************************************************     00000030
                                                                        00000110
       ENVIRONMENT DIVISION.                                            00000120
      *====================*                                            00000130
       CONFIGURATION SECTION.                                           00000140
         SOURCE-COMPUTER. IBM-3090.                                     00000150
         OBJECT-COMPUTER. IBM-3090.                                     00000160
                                                                        00000170
       DATA DIVISION.                                                   00000180
      *==============*                                                  00000190
                                                                        00000200
       WORKING-STORAGE SECTION.                                         00000210
      *-----------------------*                                         00000220
       01  FILLER             PIC   X(40) VALUE                         00000230
                              '** INICIO WORKING-STORAGE SECTION **'.   00000240
      *                                                                 00000250
       01  RESPONSE                   PIC S9(09)  COMP.                 00000300
       01  NOMBRES-DE-ARCHIVOS.                                         00000340
           05  WS-ARCHIVO-MAES        PIC  X(08)      VALUE 'DIPENCOD'. 00000350
           05  WS-ARCHIVO-PDTE        PIC  X(08)      VALUE 'DIPENDTE'. 00000350
      *                                                                 00001770
       01  WS-OCURRENCIAS             PIC 9(03)  VALUE ZEROS.           00000300
       01  WS-FECVEN                  PIC X(10)  VALUE SPACES.          00000300
       01  II                         PIC 9(03)  VALUE ZEROS.           00000300
       01  SW-FIN                     PIC 9      VALUE ZEROS.           00000300
      *PAGSIZE-INI
       01  SW-HAY-MAS                 PIC 9      VALUE ZEROS.
       01  WS-PAGSIZE                 PIC 9(03)  VALUE ZEROS.
       01  WS-MAX-PAGSIZE             PIC 9(03)  VALUE 100.
      *PAGSIZE-FIN
       01  W-ACON-REG-LEN             PIC S9(04) COMP      VALUE +145.
       01  W-LONG-REG-PDTE            PIC S9(04) COMP      VALUE +0350.
       01  W-ACON-KEY-LEN             PIC S9(04) COMP      VALUE +28.
       01  W-LONG-KEY-PDTE            PIC S9(04) COMP      VALUE +0022.
       01  W-DL0BCCCE.
           10 W-ACON-KEY.
              15  W-NUM-CODCEN-ENT    PIC X(08).
              15  W-FECVEN-ENT        PIC X(10).
              15  W-NRODOC-ENT        PIC X(10).
           10 W-CODCAR-ENT            PIC 9(12).
           10 W-NOMBRE-GIRADO-ENT     PIC X(40).
           10 W-IMPORTE-ENT           PIC S9(15)V99.
           10 W-TIPDOC-ENT            PIC X.
           10 FILLER                  PIC X(47).

       01  W-ACON-REG-INTO.
           10 W-ACON-KEY-INTO.
              15  W-NUM-CODCEN-INTO          PIC X(08).
              15  W-FECVEN-INTO              PIC X(10).
              15  W-NRODOC-INTO              PIC X(10).
           10 W-CODCAR-INTO                  PIC 9(12).
           10 W-NOMBRE-GIRADO-INTO           PIC X(40).
           10 W-IMPORTE-ACT-INTO             PIC S9(15)V99.
           10 W-TIPDOC-INTO                  PIC X.
           10 FILLER                         PIC X(47).
      *6762016047-2-INI
       01  CONSTANTES.
           05 WSC-14                         PIC 9(02) VALUE 14.

       01  WSV-PCAR-NUMCLI.
           05 WSV-PCAR-NUMCLI-1              PIC X(04) VALUE SPACES.
           05 WSV-PCAR-NUMCLI-2              PIC 9(08) COMP-3.
           05 WSV-PCAR-NUMCLI-3              PIC 9(01) VALUE 0.
       01  WSV-TALON-C                       PIC X(08) VALUE SPACES.
      *6762016047-2-FIN

      *    PENDIENTE DE CARTERA.
           COPY DIPENDTE.
      *SDA-34398-INI
       01  QGECABC-01.
           COPY QGECABC.

      *    DEFINICION DE TABLAS DB2
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE DIGTMOV1
           END-EXEC.
      *SDA-34398-FIN



       01  FILLER             PIC   X(40) VALUE                         00001890
                              '** FINAL WORKING-STORAGE SECTION **'.    00001900
      *                                                                 00001910
       LINKAGE SECTION.                                                 00001930
      *---------------*                                                 00001940
       01  DFHCOMMAREA.                                                 00001950
      *SDA-34398-INI
      *    COPY  DIWC460.                                               00001790
           COPY  DIWC46R.                                               00001790
      *SDA-34398-FIN

      *==================*                                              00001970
       PROCEDURE DIVISION.                                              00001980
      *==================*                                              00001990
       000-INICIO.                                                      00002000
           PERFORM  100-INICIO-PROGRAMA.                                00002010
           PERFORM  200-MODULO-CENTRAL.                                 00002020
           PERFORM  900-FIN-PROGRAMA.                                   00002030
                                                                        00002040
      *===================*                                             00002050
       100-INICIO-PROGRAMA.                                             00002060
      *===================*                                             00002070
                                                                        00002120
           MOVE '00'                TO  W460-CODRETOR                   00006740

           IF  (W460-NUM-CODCEN-ENT  NOT  NUMERIC)                      00002770
                MOVE '20'                TO  W460-CODRETOR              00006740
                MOVE 'ERROR COD CEN NONUMERICO' TO  W460-REFERENC       00006740
                PERFORM  900-FIN-PROGRAMA.                              00002030

      *PAGSIZE-INI
           PERFORM 110-VALIDA-PAGSIZE.
      *PAGSIZE-FIN

           PERFORM 120-POSICIONA-PRIMER-REG.

      *PAGSIZE-INI
       110-VALIDA-PAGSIZE.
      *-------------------*
      *    EL MAXIMO ES EL OCCURS DE W460-OPERACIONES EN DIWC46R (100).
      *    PAGSIZE NO INFORMADO (CERO / NO NUMERICO) = COMPATIBILIDAD
      *    CON LOS CONSUMIDORES ACTUALES, SE ASUME EL MAXIMO.
      *    PAGSIZE MAYOR AL MAXIMO SE LIMITA AL MAXIMO.
           IF  W460-PAGSIZE NOT NUMERIC        OR
               W460-PAGSIZE = ZEROS            OR
               W460-PAGSIZE > WS-MAX-PAGSIZE
               MOVE WS-MAX-PAGSIZE   TO  WS-PAGSIZE
           ELSE
               MOVE W460-PAGSIZE     TO  WS-PAGSIZE
           END-IF.
      *PAGSIZE-FIN


                                                                        00002770

       120-POSICIONA-PRIMER-REG.
      *------------------------*
           MOVE W460-ENTRADA           TO  W-ACON-KEY
           EXEC CICS STARTBR DATASET   (WS-ARCHIVO-MAES)
                             RIDFLD    (W-ACON-KEY)
                             KEYLENGTH (W-ACON-KEY-LEN)
                             GTEQ
                             RESP      (RESPONSE)
                             NOHANDLE
           END-EXEC.

           EVALUATE RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(NOTOPEN)
                MOVE '99'                      TO  W460-CODRETOR        00006740
                MOVE 'ERROR DIPENCOD CERRADO ' TO  W460-REFERENC        00006740
                PERFORM  900-FIN-PROGRAMA                               00002030
           WHEN OTHER                                                   00006760
                MOVE '99'                      TO  W460-CODRETOR        00006740
                MOVE 'ERROR STARTBR MAES '     TO  W460-REFERENC        00006740
                PERFORM  900-FIN-PROGRAMA                               00002030
           END-EVALUATE.

                                                                        00002770
                                                                        00002770
      *==================*                                              00002130
       200-MODULO-CENTRAL.                                              00002140
      *==================*                                              00002150

           MOVE W460-NUM-CODCEN-ENT       TO  W-NUM-CODCEN-INTO
           MOVE ZEROS                     TO  II
      *PAGSIZE-INI
      *    SE LLENAN COMO MAXIMO WS-PAGSIZE OCURRENCIAS. EL CORTE
      *    POR CAMBIO DE CENTRO YA LO RESUELVE 211-LEER-NEXT (SW-FIN).
           PERFORM 210-OBTIENE-DATOS                                    00002160
                          UNTIL II     >= WS-PAGSIZE  OR
                          SW-FIN        = 1.

      *    LECTURA DE ADELANTO: DETERMINA SI QUEDAN REGISTROS SIN
      *    DEVOLVER, SIN CARGARLOS EN LA TABLA.
           IF SW-FIN = 0
              PERFORM 212-VERIFICA-HAY-MAS
           END-IF.

           MOVE WS-OCURRENCIAS        TO W460-OCURRENCIAS

           IF SW-HAY-MAS = 1
              MOVE 'S'                TO W460-INDIC-PAGINAC
              MOVE  W-NUM-CODCEN-INTO TO W460-CODCEN-PAGINAC
              MOVE  W-FECVEN-INTO     TO W460-FECVEN-PAGINAC
              MOVE  W-NRODOC-INTO     TO W460-NRODOC-PAGINAC
           ELSE
              MOVE 'N'                TO W460-INDIC-PAGINAC
              MOVE ZEROS              TO W460-CODCEN-PAGINAC
              MOVE SPACES             TO W460-FECVEN-PAGINAC
              MOVE SPACES             TO W460-NRODOC-PAGINAC
              IF WS-OCURRENCIAS = 0
                MOVE '10'              TO  W460-CODRETOR                00006740
                MOVE 'NO HAY REGISTRO' TO  W460-REFERENC                00006740
                PERFORM  900-FIN-PROGRAMA                               00002030
              END-IF
           END-IF.
      *PAGSIZE-FIN

           EXEC CICS ENDBR    DATASET   (WS-ARCHIVO-MAES)
                              RESP      (RESPONSE)
                              NOHANDLE
           END-EXEC.

           IF   RESPONSE = DFHRESP (NORMAL)                             00006740
                CONTINUE                                                00006750
           ELSE                                                         00006760
                MOVE '99'              TO  W460-CODRETOR                00006740
                MOVE 'ERROR ENBR MAES' TO  W460-REFERENC                00006740
                PERFORM  900-FIN-PROGRAMA.                              00002030
                                                                        00002770
                                                                        00002670
       210-OBTIENE-DATOS.                                               00002680
      *------------------*                                              00002690

           PERFORM 211-LEER-NEXT.
           IF SW-FIN  NOT  =  1
              PERFORM 210-BUSCA-CANCELADO
              IF PCAR-SWCANC = 0
                 ADD  1                       TO  II
                 ADD  1                       TO  WS-OCURRENCIAS
                 MOVE W-NRODOC-INTO           TO  W460-NRODOC  (II)
                 MOVE PCAR-NUMCLI             TO  W460-NUMBCOORI (II)
      *6762016047-2-INI
                 IF PCAR-TDOCUM EQUAL WSC-14
                    MOVE PCAR-NUMCLI          TO WSV-PCAR-NUMCLI
                    MOVE WSV-PCAR-NUMCLI-2    TO WSV-TALON-C
                    MOVE WSV-PCAR-NUMCLI-1    TO W460-NUMBCOORI(II)(1:4)
                    MOVE WSV-TALON-C(1:WSV-PCAR-NUMCLI-3)
                                              TO W460-NUMBCOORI(II)(5:8)
                 END-IF
      *6762016047-2-FIN
                 MOVE W-FECVEN-INTO           TO  W460-FECVEN  (II)
                 MOVE W-IMPORTE-ACT-INTO      TO  W460-IMPORTE (II)
      *SDA-34398-INI
                 IF PCAR-MARCA7 EQUAL 'R'
                    MOVE '1'                  TO  W460-SITUACION (II)
                    PERFORM 900-SELECT-DIDTMOV
                 ELSE
                    MOVE '0'                  TO  W460-SITUACION (II)
                 END-IF
      *SDA-34398-FIN
                 MOVE W-NOMBRE-GIRADO-INTO    TO  W460-NOMBRE  (II)
                 MOVE W-TIPDOC-INTO           TO  W460-TIPO    (II)
                 MOVE W-CODCAR-INTO           TO  W460-CODCAR  (II)
              END-IF
           END-IF.


      *SDA-34398-INI
       900-SELECT-DIDTMOV.
      *--------------------*
           INITIALIZE DCLDIDTMOV
      *
           MOVE W-NRODOC-INTO        TO  MOV-NUMBCO
           MOVE '00000000'           TO  MOV-NUMOPE
           MOVE 'R'                  TO  MOV-INDOPE
           MOVE 'I'                  TO  MOV-ESTADO-RENOV
      *
           EXEC SQL
              SELECT  MOV_NUMBCO
                     ,MOV_NUMOPE
                     ,MOV_INDOPE
                     ,MOV_IMPCOM
                     ,MOV_CUENTA_COM
                     ,MOV_ESTADO_RENOV
                     ,MOV_FECVEN_RENOV
               INTO  :MOV-NUMBCO
                    ,:MOV-NUMOPE
                    ,:MOV-INDOPE
                    ,:MOV-IMPCOM
                    ,:MOV-CUENTA-COM
                    ,:MOV-ESTADO-RENOV
                    ,:MOV-FECVEN-RENOV
                FROM DIDTMOV
               WHERE MOV_NUMBCO       = : MOV-NUMBCO
                 AND MOV_NUMOPE       > : MOV-NUMOPE
                 AND MOV_INDOPE       = : MOV-INDOPE
                 AND MOV_ESTADO_RENOV = : MOV-ESTADO-RENOV
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
                    MOVE '2'                TO W460-SITUACION (II)
                    MOVE ZEROES             TO W460-IMPORTE (II)
                    MOVE MOV-IMPCOM         TO W460-IMPORTE (II)
               WHEN +100
                    CONTINUE
               WHEN -811
                    MOVE '81'                  TO W460-CODRETOR
                    MOVE 'DUPLICADO NUMOPE'    TO W460-REFERENC
                    PERFORM  900-FIN-PROGRAMA
               WHEN OTHER
                    MOVE '99'                  TO W460-CODRETOR
                    MOVE 'NO EXISTE NUMOPE'    TO W460-REFERENC
                    PERFORM  900-FIN-PROGRAMA
           END-EVALUATE.
      *SDA-34398-FIN

       210-BUSCA-CANCELADO.
      *--------------------*
           MOVE  W-CODCAR-INTO           TO  PCAR-CODAPL
           MOVE  W-NRODOC-INTO           TO  PCAR-NUMBCO
           EXEC CICS READ     DATASET   (WS-ARCHIVO-PDTE)
                              RIDFLD    (PCAR-KEYPEN)
                              KEYLENGTH (W-LONG-KEY-PDTE)
                              INTO      (REG-PENDIENTE-CARTERA)         00006670
                              RESP      (RESPONSE)                      00006680
                              EQUAL                                     00006700
                              NOHANDLE                                  00006710
           END-EXEC.
           IF   RESPONSE = DFHRESP (NORMAL)                             00006740
                CONTINUE                                                00006750
           ELSE                                                         00006760
                MOVE '99'              TO  W460-CODRETOR                00006740
                MOVE 'ERROR READ PDTE' TO  W460-REFERENC                00006740
                PERFORM  900-FIN-PROGRAMA.                              00002030

       211-LEER-NEXT.
      *-------------*
           EXEC CICS READNEXT  DATASET   (WS-ARCHIVO-MAES)              00006730
                               LENGTH    (W-ACON-REG-LEN)               00006730
                               RIDFLD    (W-ACON-KEY)                   00006730
                               KEYLENGTH (W-ACON-KEY-LEN)               00006730
                               INTO      (W-ACON-REG-INTO)              00006730
                               RESP      (RESPONSE)                     00006730
                               NOHANDLE                                 00006730
           END-EXEC

           EVALUATE RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(ENDFILE)
                MOVE 1                 TO  SW-FIN                       00006750
           WHEN OTHER                                                   00006760
                MOVE '99'              TO  W460-CODRETOR                00006740
                MOVE 'ERROR READ MAES' TO  W460-REFERENC                00006740
                PERFORM  900-FIN-PROGRAMA                               00002030
           END-EVALUATE.
                                                                        00002770
           IF  SW-FIN  =  0
               IF  W460-NUM-CODCEN-ENT  =  W-NUM-CODCEN-INTO
                   CONTINUE
               ELSE
                  MOVE     1 TO SW-FIN
               END-IF
           END-IF.

      *PAGSIZE-INI
       212-VERIFICA-HAY-MAS.
      *---------------------*
      *    AVANZA HASTA EL PRIMER REGISTRO NO CANCELADO QUE NO ENTRO
      *    EN ESTA PAGINA. SI EXISTE, SU CLAVE (W-ACON-KEY-INTO) ES EL
      *    ARGUMENTO DE BUSQUEDA DE LA PAGINA SIGUIENTE.
           MOVE ZEROS TO SW-HAY-MAS
           PERFORM UNTIL SW-FIN = 1  OR  SW-HAY-MAS = 1
              PERFORM 211-LEER-NEXT
              IF SW-FIN NOT = 1
                 PERFORM 210-BUSCA-CANCELADO
                 IF PCAR-SWCANC = 0
                    MOVE 1 TO SW-HAY-MAS
                 END-IF
              END-IF
           END-PERFORM.
      *PAGSIZE-FIN

                                                                        00002860
      *================*                                                00009250
       900-FIN-PROGRAMA.                                                00009260
      *================*                                                00009270

           EXEC  CICS  RETURN  END-EXEC.                                00009280
