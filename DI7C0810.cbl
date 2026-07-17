      *_______________________                                          00010000
       IDENTIFICATION DIVISION.                                         00020000
      *_______________________                                          00030000
       PROGRAM-ID. DI7C0810.                                            00040000
      *________________________________________________________________ 00050000
      *                                                                 00060000
      *            DIETARIO DE CARTERA PARA CONTINET                    00070000
      *________________________________________________________________ 00080000
                                                                        00090000
       ENVIRONMENT DIVISION.                                            00100000
      *____________________                                             00110000
       CONFIGURATION SECTION.                                           00120000
         SOURCE-COMPUTER. IBM-3090.                                     00130000
         OBJECT-COMPUTER. IBM-3090.                                     00140000
                                                                        00150000
       DATA DIVISION.                                                   00160000
      *_____________                                                    00170000
                                                                        00180000
       WORKING-STORAGE SECTION.                                         00190000
      *_______________________                                          00200000
       01  FILLER             PIC   X(40) VALUE                         00210000
                              '***INICIO WORKING-STORAGE SECTION***'.   00220000
                                                                        00230000
       01  RESPONSE                   PIC S9(09) COMP.                  00240000
                                                                        00250000
       01  WS-DIDIECOB-LENGTH         PIC S9(04) COMP  VALUE +200.      00260000
       01  WS-COD-FECOPE.                                               00270000
           02 WS-CODIGO.                                                00280003
              04 WS-CODOFI            PIC 9(03)        VALUE ZEROS.     00290003
              04 WS-CODMON            PIC 9(01)        VALUE ZEROS.     00300003
              04 WS-CODCLI            PIC 9(06)        VALUE ZEROS.     00310003
              04 WS-CODSER            PIC 9(02)        VALUE ZEROS.     00320003
           02 WS-FECOPE               PIC 9(08)        VALUE ZEROS.     00330000
       01  WS-FIN                     PIC 9            VALUE ZEROS.     00340000
       01  N                          PIC 9(05)        VALUE ZEROS.     00350000
       01  WS-CODIGO-LEOS             PIC 9(03)        VALUE ZEROS.     00360019
                                                                        00370000
       01  REG-DIDIECOB.                                                00380000
           02  DIEC-KEY.                                                00390000
               04  DIEC-COD-FECOPE.                                     00400000
                   06  DIEC-CODIGO.                                     00410000
                       08  DIEC-CODOFI     PIC 9(03).                   00420000
                       08  DIEC-CODMON     PIC 9(01).                   00430000
                       08  DIEC-CODCLI     PIC 9(06).                   00440000
                       08  DIEC-CODSER     PIC 9(02).                   00450000
      *_________________________________________________________________00460000
      *            FECOPE = FECMOV     EL DATO SE OBTIENE DE DL0BCMOV   00470000
      *            FECOPE   SIEMPRE ES PRESENTADO DE MAYOR A MENOR      00480000
      *_________________________________________________________________00490000
                   06  DIEC-FECOPE         PIC 9(08).                   00500038
      *_________________________________________________________________00510000
      *            NUMBCO = 0000000000 ES REG DE TOTALES ABONO/CARGO    00520000
      *            NUMBCO = 0000000001 ES REG DE TOTALES                00530000
      *_________________________________________________________________00540000
               04  DIEC-NUMBCO             PIC 9(10).                   00550000
      *_________________________________________________________________00560000
      *            POSICION                                             00570000
      *            SITUAC = ATE     SI ES DOCUMENTO RENOV/CANCEL        00580000
      *            SITUAC = VIG     SI ES DOCUMENTO POR VENCER          00590000
      *_________________________________________________________________00600000
      *            DIETARIO COBRANZA                                    00610000
      *            SITUAC = CAN     SI ES DOCUMENTO CANCELADO           00620000
      *            SITUAC = REN     SI ES DOCUMENTO RENOVADO            00630000
      *            SITUAC = RRN     SI ES DOCUMENTO RENOVADO REINGRESO  00640000
      *            SITUAC = DEV     SI ES DOCUMENTO DEVUELTO            00650000
      *            SITUAC = DVP     SI ES DOCUMENTO DEVUELTO PROTESTADO 00660000
      *            SITUAC = DEE     SI ES DOCUMENTO DESCARGADO POR ERROR00670000
      *            SITUAC = ING     SI ES DOCUMENTO INGRESADO           00680000
      *            SITUAC = INE     SI ES DOCUMENTO INGRESADO  POR ERROR00690000
      *_________________________________________________________________00700000
               04  DIEC-SITUAC             PIC X(03).                   00710000
      *_________________________________________________________________00720000
      *            NUMDOC = NNNNN   SI ES REG DE TOTALES/REGISTR/POR REG00730000
      *            NUMDOC = 00000   SI ES REG DE DETALLE                00740000
      *_________________________________________________________________00750000
           02  DIEC-NUMDOC                 PIC 9(05).                   00760000
           02  DIEC-DETALLE.                                            00770000
               04  DIEC-FECVEN             PIC 9(08).                   00780000
      *_________________________________________________________________00790000
      *     NOMACE(01:10) = CTAABO  SI ES REG DE TOTALES (DL0BCSAL)     00800000
      *     NOMACE(11:10) = CTACAR  SI ES REG DE TOTALES (DL0BCSAL)     00810000
      *_________________________________________________________________00820000
               04  DIEC-NOMACE             PIC X(40).                   00830000
               04  DIEC-NOMACE-REDEFINE    REDEFINES DIEC-NOMACE.       00840000
                   06 DIEC-CTAABO          PIC 9(10).                   00850000
                   06 DIEC-CTACAR          PIC 9(10).                   00860000
                   06 DIEC-CTA-FILLER      PIC X(20).                   00870000
      *_________________________________________________________________00880000
      *        IMPORTES   = ZEROS   SI ES REG DE TOTALES/REGISTR/POR REG00890000
      *_________________________________________________________________00900000
               04  DIEC-INGRESO            PIC 9(13)V99 COMP-3.         00910000
               04  DIEC-IMPORTE            PIC 9(13)V99 COMP-3.         00920000
               04  DIEC-INTERES            PIC 9(13)V99 COMP-3.         00930000
               04  DIEC-COMPORT            PIC 9(13)V99 COMP-3.         00940000
               04  DIEC-GASTOS             PIC 9(13)V99 COMP-3.         00950000
           02  FILLER                      PIC X(49).                   00960000
                                                                        00970000
       01  FILLER             PIC   X(40) VALUE                         00980000
                              '***FINAL WORKING-STORAGE SECTION***'.    00990000
                                                                        01000000
       LINKAGE SECTION.                                                 01010000
      *_______________                                                  01020000
       01  DFHCOMMAREA.                                                 01030000
           COPY DIWC810.                                                01040028
                                                                        01050000
       PROCEDURE DIVISION.                                              01060000
      *__________________                                               01070000
                                                                        01080000
       000-INICIO.                                                      01090000
      *__________                                                       01100000
           PERFORM 100-INICIO-PROGRAMA.                                 01110000
           PERFORM 200-MODULO-CENTRAL.                                  01120000
           PERFORM 900-FIN-PROGRAMA.                                    01130000
                                                                        01140000
       100-INICIO-PROGRAMA.                                             01150000
      *___________________                                              01160000
           MOVE '00'                                  TO W810-CODRETOR. 01170022
           MOVE SPACES                                TO W810-REFERENC. 01180022
           MOVE 'CNE0141'                             TO W810-DIRCAPLN. 01181048
                                                                        01190000
           IF  (W810-CODIGO NOT NUMERIC)                                01200000
                MOVE '08'                             TO W810-CODRETOR  01210000
                MOVE 'ERROR CODIGO NO NUMERICO'       TO W810-REFERENC  01220036
                MOVE 'DIE8108'                        TO W810-DIRCAPLN  01221049
                PERFORM 900-FIN-PROGRAMA                                01230003
           END-IF.                                                      01240000
                                                                        01250000
           IF  (W810-CODSER = 01 OR 07 OR 08 OR 13)                     01260045
                CONTINUE                                                01260145
           ELSE                                                         01261045
                MOVE '09'                             TO W810-CODRETOR  01270041
                MOVE 'ERROR CODIGO NO VALIDO  '       TO W810-REFERENC  01280041
                MOVE 'DIE8109'                        TO W810-DIRCAPLN  01281049
                PERFORM 900-FIN-PROGRAMA                                01290041
           END-IF.                                                      01300041
                                                                        01310041
           PERFORM 110-POSICIONA-PRIMERO.                               01320006
                                                                        01330000
       110-POSICIONA-PRIMERO.                                           01340006
      *_____________________                                            01350006
           MOVE W810-CODIGO                           TO DIEC-CODIGO    01360020
                                                         WS-CODIGO.     01370022
           MOVE ZEROS                                 TO DIEC-FECOPE.   01380006
           MOVE 0000000000                            TO DIEC-NUMBCO.   01390000
           MOVE SPACES                                TO DIEC-SITUAC.   01400000
                                                                        01410000
           EXEC CICS STARTBR DATASET   ('DIDIECOB')                     01420000
                             RIDFLD    (DIEC-KEY)                       01430000
                             KEYLENGTH (+33)                            01440000
                             RESP      (RESPONSE)                       01450000
                             GTEQ                                       01460012
                             NOHANDLE                                   01470000
           END-EXEC.                                                    01480000
                                                                        01490000
           EVALUATE RESPONSE                                            01500000
           WHEN DFHRESP(NORMAL)                                         01510000
           WHEN DFHRESP(LENGERR)                                        01520000
                CONTINUE                                                01530016
           WHEN DFHRESP(NOTFND)                                         01540000
                MOVE '97'                             TO W810-CODRETOR  01550000
                MOVE 'ERROR DIDIECOB CLAVE NO EXISTE' TO W810-REFERENC  01560037
                MOVE 'DIE8197'                        TO W810-DIRCAPLN  01561049
                PERFORM 900-FIN-PROGRAMA                                01570003
           WHEN DFHRESP(NOTOPEN)                                        01580000
                MOVE '98'                             TO W810-CODRETOR  01590000
                MOVE 'ERROR DIDIECOB CERRADO '        TO W810-REFERENC  01600037
                MOVE 'DIE8198'                        TO W810-DIRCAPLN  01601049
                PERFORM 900-FIN-PROGRAMA                                01610003
           WHEN OTHER                                                   01620000
                MOVE '99'                             TO W810-CODRETOR  01630000
                MOVE 'ERROR DIDIECOB STARTBR '        TO W810-REFERENC  01640037
                MOVE 'DIE8199'                        TO W810-DIRCAPLN  01641049
                PERFORM 900-FIN-PROGRAMA                                01650003
           END-EVALUATE.                                                01660000
                                                                        01670000
       200-MODULO-CENTRAL.                                              01680000
      *__________________                                               01690000
           PERFORM 220-LEER-NEXT.                                       01700016
           PERFORM 240-LLENA-SALIDA-TITULOS.                            01710016
           PERFORM 230-LLENA-SALIDA                                     01720016
                                                                        01730016
           PERFORM 210-OBTIENE-DATOS UNTIL                              01740000
                   WS-FIN            = 1              OR                01750000
                   WS-CODIGO     NOT = DIEC-CODIGO    OR                01760014
                   N                 > 200.                             01770014
                                                                        01780039
           MOVE    N                                  TO W810-N         01790008
                                                                        01800000
           EXEC CICS ENDBR   DATASET ('DIDIECOB')                       01810013
                             RESP    (RESPONSE)                         01820013
                             NOHANDLE                                   01830013
           END-EXEC.                                                    01840000
                                                                        01850000
           EVALUATE RESPONSE                                            01860000
           WHEN DFHRESP(NORMAL)                                         01870000
                CONTINUE                                                01880000
           WHEN OTHER                                                   01890000
                MOVE '91'                             TO W810-CODRETOR  01900000
                MOVE 'ERROR DIDIECOB ENDBR   '        TO W810-REFERENC  01910037
                MOVE 'DIE8191'                        TO W810-DIRCAPLN  01911049
                PERFORM 900-FIN-PROGRAMA                                01920003
           END-EVALUATE.                                                01930000
                                                                        01940000
       210-OBTIENE-DATOS.                                               01950000
      *_________________                                                01960000
           PERFORM 220-LEER-NEXT.                                       01970000
           IF WS-FIN = 0                                                01980000
              IF  WS-CODIGO = DIEC-CODIGO                               01990006
                  PERFORM 230-LLENA-SALIDA                              02000000
              ELSE                                                      02010000
                  MOVE 1 TO WS-FIN                                      02020000
              END-IF                                                    02030000
           END-IF.                                                      02040000
                                                                        02050000
       220-LEER-NEXT.                                                   02060000
      *_____________                                                    02070000
           EXEC CICS READNEXT  DATASET   ('DIDIECOB')                   02080000
                               RIDFLD    (DIEC-KEY)                     02090000
                               KEYLENGTH (+33)                          02100000
                               INTO      (REG-DIDIECOB)                 02110000
                               LENGTH    (WS-DIDIECOB-LENGTH)           02120000
                               RESP      (RESPONSE)                     02130000
                               NOHANDLE                                 02140000
           END-EXEC.                                                    02150017
                                                                        02160000
           EVALUATE RESPONSE                                            02170000
           WHEN DFHRESP(NORMAL)                                         02180000
                IF WS-CODIGO = DIEC-CODIGO AND                          02190017
                   ZEROS     < DIEC-FECOPE                              02200023
                   ADD 1                              TO WS-CODIGO-LEOS 02210019
                ELSE                                                    02220017
                   MOVE 1                             TO WS-FIN         02230023
                   IF WS-CODIGO-LEOS = 0                                02240019
                      MOVE '97'                       TO W810-CODRETOR  02250021
                      MOVE 'ERROR DIDIECOB CODIGO NO EXIST'             02260037
                                                      TO W810-REFERENC  02270021
                      MOVE 'DIE8197'                TO W810-DIRCAPLN    02271049
                      PERFORM 900-FIN-PROGRAMA                          02280018
                   END-IF                                               02290018
                END-IF                                                  02300017
           WHEN DFHRESP(ENDFILE)                                        02310000
                MOVE 1                                TO WS-FIN         02320000
           WHEN OTHER                                                   02330000
                MOVE '96'                             TO W810-CODRETOR  02340000
                MOVE 'ERROR DIDIECOB READNEXT'        TO W810-REFERENC  02350037
                MOVE 'DIE8196'                        TO W810-DIRCAPLN  02351049
                PERFORM 900-FIN-PROGRAMA                                02360003
           END-EVALUATE.                                                02370000
                                                                        02380000
       230-LLENA-SALIDA.                                                02390000
      *________________                                                 02400000
           EVALUATE DIEC-NUMBCO                                         02410000
               WHEN 0000000000         PERFORM 260-LLENA-SALIDA-DETALLE 02420006
               WHEN 1 THRU 9999999999  CONTINUE                         02430002
           END-EVALUATE.                                                02440000
                                                                        02450000
       240-LLENA-SALIDA-TITULOS.                                        02460000
      *________________________                                         02470000
           MOVE  WS-CODIGO                         TO W810-CODIGO-TIT.  02480003
           EVALUATE WS-CODSER                                           02490027
               WHEN  7                                                  02500027
               WHEN  8                                                  02510027
               WHEN 13 MOVE 'COBRANZA GARANTIA   ' TO W810-SERVICIO-TIT 02520027
               WHEN OTHER                                               02530027
                       MOVE 'COBRANZA LIBRE      ' TO W810-SERVICIO-TIT 02540027
           END-EVALUATE.                                                02550027
           IF  WS-CODMON = 1 MOVE 'SOLES   '       TO W810-MONEDA-TIT.  02560040
           IF  WS-CODMON = 2 MOVE 'DOLARES '       TO W810-MONEDA-TIT.  02570040
                                                                        02580000
       260-LLENA-SALIDA-DETALLE.                                        02590000
      *________________________                                         02600000
           ADD  1                                  TO N.                02610003
           MOVE DIEC-FECOPE                        TO W810-FECOPE (N).  02620007
      *____TOTAL ABONADO______________________________________________  02621047
           MOVE DIEC-INGRESO                       TO W810-ABONOS (N).  02622047
      *____TOTAL CARGADO______________________________________________  02630003
           MOVE DIEC-IMPORTE                       TO W810-CARGOS (N).  02640047
                                                                        02670000
       900-FIN-PROGRAMA.                                                02680000
      *________________                                                 02690000
           EXEC  CICS  RETURN  END-EXEC.                                02700000
