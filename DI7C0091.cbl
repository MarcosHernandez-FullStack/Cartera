      *_______________________                                          00010000
       IDENTIFICATION DIVISION.                                         00020000
      *_______________________                                          00030000
       PROGRAM-ID. DI7C0091.                                            00040000
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
       01  WS-DIDIEDES-LENGTH         PIC S9(04) COMP  VALUE +200.      00260000
       01  WS-COD-FECOPE.                                               00270000
           02 WS-CODIGO.                                                00280000
              04 WS-CODOFI            PIC 9(03)        VALUE ZEROS.     00290000
              04 WS-CODMON            PIC 9(01)        VALUE ZEROS.     00300000
              04 WS-CODCLI            PIC 9(06)        VALUE ZEROS.     00310000
              04 WS-CODSER            PIC 9(02)        VALUE ZEROS.     00320000
           02 WS-FECOPE               PIC 9(08)        VALUE ZEROS.     00330000
       01  WS-FIN                     PIC 9            VALUE ZEROS.     00340000
       01  N                          PIC 9(05)        VALUE ZEROS.     00350000
       01  WS-CODIGO-LEOS             PIC 9(03)        VALUE ZEROS.     00360000
       01  WS-MAXITEM                 PIC 9(05)        VALUE 200.
                                                                        00370000
       01  REG-DIDIEDES.                                                00380000
           02  DIED-KEY.                                                00390000
               04  DIED-COD-FECOPE.                                     00400000
                   06  DIED-CODIGO.                                     00410000
                       08  DIED-CODOFI     PIC 9(03).                   00420000
                       08  DIED-CODMON     PIC 9(01).                   00430000
                       08  DIED-CODCLI     PIC 9(06).                   00440000
                       08  DIED-CODSER     PIC 9(02).                   00450000
      *_________________________________________________________________00460000
      *            FECOPE = FECMOV     EL DATO SE OBTIENE DE DL0BCMOV   00470000
      *            FECOPE   SIEMPRE ES PRESENTADO DE MAYOR A MENOR      00480000
      *_________________________________________________________________00490000
                   06  DIED-FECOPE         PIC 9(08).                   00500014
      *_________________________________________________________________00510000
      *            NUMBCO = 0000000000 ES REG DE TOTALES ABONO/CARGO    00520000
      *            NUMBCO = 0000000001 ES REG DE TOTALES                00530000
      *_________________________________________________________________00540000
               04  DIED-NUMBCO             PIC 9(10).                   00550000
      *_________________________________________________________________00560000
      *            POSICION                                             00570000
      *            SITUAC = ATE     SI ES DOCUMENTO RENOV/CANCEL        00580000
      *            SITUAC = VIG     SI ES DOCUMENTO POR VENCER          00590000
      *_________________________________________________________________00600000
      *            DIETARIO DESCUENTOS                                  00610001
      *            SITUAC = CAN     SI ES DOCUMENTO CANCELADO           00620000
      *            SITUAC = REN     SI ES DOCUMENTO RENOVADO            00630000
      *            SITUAC = RRN     SI ES DOCUMENTO RENOVADO REINGRESO  00640000
      *            SITUAC = DEV     SI ES DOCUMENTO DEVUELTO            00650000
      *            SITUAC = DVP     SI ES DOCUMENTO DEVUELTO PROTESTADO 00660000
      *            SITUAC = DEE     SI ES DOCUMENTO DESCARGADO POR ERROR00670000
      *            SITUAC = ING     SI ES DOCUMENTO INGRESADO           00680000
      *            SITUAC = INE     SI ES DOCUMENTO INGRESADO  POR ERROR00690000
      *_________________________________________________________________00700000
               04  DIED-SITUAC             PIC X(03).                   00710000
      *_________________________________________________________________00720000
      *            NUMDOC = NNNNN   SI ES REG DE TOTALES/REGISTR/POR REG00730000
      *            NUMDOC = 00000   SI ES REG DE DETALLE                00740000
      *_________________________________________________________________00750000
           02  DIED-NUMDOC                 PIC 9(05).                   00760000
           02  DIED-DETALLE.                                            00770000
               04  DIED-FECVEN             PIC 9(08).                   00780000
      *_________________________________________________________________00790000
      *     NOMACE(01:10) = CTAABO  SI ES REG DE TOTALES (DL0BCSAL)     00800000
      *     NOMACE(11:10) = CTACAR  SI ES REG DE TOTALES (DL0BCSAL)     00810000
      *_________________________________________________________________00820000
               04  DIED-NOMACE             PIC X(40).                   00830000
               04  DIED-NOMACE-REDEFINE    REDEFINES DIED-NOMACE.       00840000
                   06 DIED-CTAABO          PIC 9(10).                   00850000
                   06 DIED-CTACAR          PIC 9(10).                   00860000
                   06 DIED-CTA-FILLER      PIC X(20).                   00870000
      *_________________________________________________________________00880000
      *        IMPORTES   = ZEROS   SI ES REG DE TOTALES/REGISTR/POR REG00890000
      *_________________________________________________________________00900000
               04  DIED-INGRESO            PIC 9(13)V99 COMP-3.         00910000
               04  DIED-IMPORTE            PIC 9(13)V99 COMP-3.         00920000
               04  DIED-INTERES            PIC 9(13)V99 COMP-3.         00930000
               04  DIED-COMPORT            PIC 9(13)V99 COMP-3.         00940000
               04  DIED-GASTOS             PIC 9(13)V99 COMP-3.         00950000
           02  FILLER                      PIC X(49).                   00960000
                                                                        00970000
       01  FILLER             PIC   X(40) VALUE                         00980000
                              '***FINAL WORKING-STORAGE SECTION***'.    00990000
                                                                        01000000
       LINKAGE SECTION.                                                 01010000
      *_______________                                                  01020000
       01  DFHCOMMAREA.                                                 01030000
           COPY DIWC0091.                                               01040009
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
           MOVE '00'                                  TO W0091-CODRETOR.01170002
           MOVE SPACES                                TO W0091-REFERENC.01180020
           MOVE 'CNE0141'                             TO W0091-DIRCAPLN.01181020
                                                                        01190000
           IF  (W0091-CODIGO NOT NUMERIC)                               01200000
                MOVE '08'                             TO W0091-CODRETOR 01210000
                MOVE 'ERROR CODIGO NO NUMERICO'       TO W0091-REFERENC 01220013
                MOVE 'DIE9108'                        TO W0091-DIRCAPLN 01221020
                PERFORM 900-FIN-PROGRAMA                                01230000
           END-IF.                                                      01240000
                                                                        01250000
           IF  (W0091-CODSER = 50)                                      01260018
                CONTINUE                                                01261018
           ELSE                                                         01262018
                MOVE '09'                             TO W0091-CODRETOR 01270016
                MOVE 'ERROR CODIGO NO VALIDO  '       TO W0091-REFERENC 01280016
                MOVE 'DIE9109'                        TO W0091-DIRCAPLN 01281020
                PERFORM 900-FIN-PROGRAMA                                01290016
           END-IF.                                                      01300016
                                                                        01310016
           PERFORM 105-INICIALIZA-PAGINACION.
                                                                        01330000
           PERFORM 110-POSICIONA-PRIMERO.                               01320000
                                                                        01330000
       105-INICIALIZA-PAGINACION.
      *_____________________________
           MOVE 200                                   TO WS-MAXITEM
           IF W0091-INDPAGI = 'S'
                IF W0091-PAGSIZE IS NUMERIC AND
                   W0091-PAGSIZE > 0        AND
                   W0091-PAGSIZE <= 200
                     MOVE W0091-PAGSIZE                TO WS-MAXITEM
                END-IF
           ELSE
                MOVE SPACES                            TO W0091-PAGINAC
           END-IF.
                                                                        01330000
       110-POSICIONA-PRIMERO.                                           01340000
      *_____________________                                            01350000
           IF W0091-INDPAGI = 'S' AND W0091-PAGINAC NOT = SPACES
                MOVE W0091-CODIGO                     TO WS-CODIGO
                MOVE W0091-PAGINAC(1:33)              TO DIED-KEY
           ELSE
                MOVE W0091-CODIGO                     TO DIED-CODIGO    01360000
                                                         WS-CODIGO      01370002
                MOVE ZEROS                            TO DIED-FECOPE   01380000
                MOVE 0000000000                       TO DIED-NUMBCO   01390000
                MOVE SPACES                           TO DIED-SITUAC   01400000
           END-IF.
                                                                        01410000
           EXEC CICS STARTBR DATASET   ('DIDIEDES')                     01420000
                             RIDFLD    (DIED-KEY)                       01430000
                             KEYLENGTH (+33)                            01440000
                             RESP      (RESPONSE)                       01450000
                             GTEQ                                       01460000
                             NOHANDLE                                   01470000
           END-EXEC.                                                    01480000
                                                                        01490000
           EVALUATE RESPONSE                                            01500000
           WHEN DFHRESP(NORMAL)                                         01510000
           WHEN DFHRESP(LENGERR)                                        01520000
                CONTINUE                                                01530000
           WHEN DFHRESP(NOTFND)                                         01540000
                MOVE '97'                             TO W0091-CODRETOR 01550000
                MOVE 'ERROR DIDIEDES CLAVE NO EXISTE' TO W0091-REFERENC 01560013
                MOVE 'DIE9197'                        TO W0091-DIRCAPLN 01561020
                PERFORM 900-FIN-PROGRAMA                                01570000
           WHEN DFHRESP(NOTOPEN)                                        01580000
                MOVE '98'                             TO W0091-CODRETOR 01590000
                MOVE 'ERROR DIDIEDES CERRADO '        TO W0091-REFERENC 01600013
                MOVE 'DIE9198'                        TO W0091-DIRCAPLN 01601020
                PERFORM 900-FIN-PROGRAMA                                01610000
           WHEN OTHER                                                   01620000
                MOVE '99'                             TO W0091-CODRETOR 01630000
                MOVE 'ERROR DIDIEDES STARTBR '        TO W0091-REFERENC 01640013
                MOVE 'DIE9199'                        TO W0091-DIRCAPLN 01641020
                PERFORM 900-FIN-PROGRAMA                                01650000
           END-EVALUATE.                                                01660000
                                                                        01670000
           IF W0091-INDPAGI = 'S' AND W0091-PAGINAC NOT = SPACES
                PERFORM 115-DESCARTA-CURSOR-PAGINACION
           END-IF.
                                                                        01670000
       115-DESCARTA-CURSOR-PAGINACION.
      *_______________________________
      *    DESCARTA EL REGISTRO YA ENTREGADO EN LA PAGINA ANTERIOR,
      *    CUYA CLAVE SE USO PARA REPOSICIONAR EL STARTBR (GTEQ).
           EXEC CICS READNEXT  DATASET   ('DIDIEDES')                   02080000
                               RIDFLD    (DIED-KEY)                     02090000
                               KEYLENGTH (+33)                          02100000
                               INTO      (REG-DIDIEDES)                 02110000
                               LENGTH    (WS-DIDIEDES-LENGTH)           02120000
                               RESP      (RESPONSE)                     02130000
                               NOHANDLE                                 02140000
           END-EXEC.                                                    02150000
                                                                        02160000
           EVALUATE RESPONSE                                            02170000
           WHEN DFHRESP(NORMAL)                                         02180000
                CONTINUE
           WHEN DFHRESP(ENDFILE)                                        02310000
                MOVE 1                                TO WS-FIN         02320000
           WHEN OTHER                                                   02330000
                MOVE '96'                             TO W0091-CODRETOR 02340000
                MOVE 'ERROR DIDIEDES READNEXT'        TO W0091-REFERENC 02350013
                MOVE 'DIE9196'                        TO W0091-DIRCAPLN 02351020
                PERFORM 900-FIN-PROGRAMA                                02360000
           END-EVALUATE.                                                02370000
                                                                        01670000
       200-MODULO-CENTRAL.                                              01680000
      *__________________                                               01690000
           PERFORM 240-LLENA-SALIDA-TITULOS.                            01710000
           IF WS-FIN = 0
                PERFORM 220-LEER-NEXT                                   01700000
                PERFORM 230-LLENA-SALIDA                                01720000
           END-IF.
                                                                        01730000
           PERFORM 210-OBTIENE-DATOS UNTIL                              01740000
                   WS-FIN            = 1              OR                01750000
                   WS-CODIGO     NOT = DIED-CODIGO    OR                01760000
                   N                >= WS-MAXITEM.                      01770000
                                                                        01780016
           IF WS-FIN = 1
                MOVE 'N'                               TO W0091-INDPAGI
                MOVE SPACES                            TO W0091-PAGINAC
           ELSE
                MOVE 'S'                         TO W0091-INDPAGI
                MOVE DIED-KEY                    TO W0091-PAGINAC(1:33)
           END-IF.

           MOVE    WS-MAXITEM                          TO W0091-PAGSIZE

           MOVE    N                                 TO W0091-N         01790000
                                                                        01800000
           EXEC CICS ENDBR   DATASET ('DIDIEDES')                       01810000
                             RESP    (RESPONSE)                         01820000
                             NOHANDLE                                   01830000
           END-EXEC.                                                    01840000
                                                                        01850000
           EVALUATE RESPONSE                                            01860000
           WHEN DFHRESP(NORMAL)                                         01870000
                CONTINUE                                                01880000
           WHEN OTHER                                                   01890000
                MOVE '91'                             TO W0091-CODRETOR 01900000
                MOVE 'ERROR DIDIEDES ENDBR   '        TO W0091-REFERENC 01910013
                MOVE 'DIE9191'                        TO W0091-DIRCAPLN 01911020
                PERFORM 900-FIN-PROGRAMA                                01920000
           END-EVALUATE.                                                01930000
                                                                        01940000
       210-OBTIENE-DATOS.                                               01950000
      *_________________                                                01960000
           PERFORM 220-LEER-NEXT.                                       01970000
           IF WS-FIN = 0                                                01980000
              IF  WS-CODIGO = DIED-CODIGO                               01990000
                  PERFORM 230-LLENA-SALIDA                              02000000
              ELSE                                                      02010000
                  MOVE 1 TO WS-FIN                                      02020000
              END-IF                                                    02030000
           END-IF.                                                      02040000
                                                                        02050000
       220-LEER-NEXT.                                                   02060000
      *_____________                                                    02070000
           EXEC CICS READNEXT  DATASET   ('DIDIEDES')                   02080000
                               RIDFLD    (DIED-KEY)                     02090000
                               KEYLENGTH (+33)                          02100000
                               INTO      (REG-DIDIEDES)                 02110000
                               LENGTH    (WS-DIDIEDES-LENGTH)           02120000
                               RESP      (RESPONSE)                     02130000
                               NOHANDLE                                 02140000
           END-EXEC.                                                    02150000
                                                                        02160000
           EVALUATE RESPONSE                                            02170000
           WHEN DFHRESP(NORMAL)                                         02180000
                IF WS-CODIGO = DIED-CODIGO AND                          02190000
                   ZEROS     < DIED-FECOPE                              02200003
                   ADD 1                              TO WS-CODIGO-LEOS 02210000
                ELSE                                                    02220000
                   MOVE 1                             TO WS-FIN         02230003
                   IF WS-CODIGO-LEOS = 0                                02240000
                      MOVE '97'                       TO W0091-CODRETOR 02250000
                      MOVE 'ERROR DIDIEDES CODIGO NO EXIST'             02260013
                                                      TO W0091-REFERENC 02270000
                      MOVE 'DIE9197'                  TO W0091-DIRCAPLN 02271020
                      PERFORM 900-FIN-PROGRAMA                          02280000
                   END-IF                                               02290003
                END-IF                                                  02300000
           WHEN DFHRESP(ENDFILE)                                        02310000
                MOVE 1                                TO WS-FIN         02320000
           WHEN OTHER                                                   02330000
                MOVE '96'                             TO W0091-CODRETOR 02340000
                MOVE 'ERROR DIDIEDES READNEXT'        TO W0091-REFERENC 02350013
                MOVE 'DIE9196'                        TO W0091-DIRCAPLN 02351020
                PERFORM 900-FIN-PROGRAMA                                02360000
           END-EVALUATE.                                                02370000
                                                                        02380000
       230-LLENA-SALIDA.                                                02390000
      *________________                                                 02400000
           EVALUATE DIED-NUMBCO                                         02410000
               WHEN 0000000000         PERFORM 260-LLENA-SALIDA-DETALLE 02420000
               WHEN 1 THRU 9999999999  CONTINUE                         02430000
           END-EVALUATE.                                                02440000
                                                                        02450000
       240-LLENA-SALIDA-TITULOS.                                        02460000
      *________________________                                         02470000
           MOVE  WS-CODIGO                        TO W0091-CODIGO-TIT.  02480000
           MOVE 'DESCUENTO DE LETRAS '            TO W0091-SERVICIO-TIT.02490007
           IF    WS-CODMON = 1 MOVE 'SOLES   '    TO W0091-MONEDA-TIT.  02500000
           IF    WS-CODMON = 2 MOVE 'DOLARES '    TO W0091-MONEDA-TIT.  02510000
                                                                        02520000
       260-LLENA-SALIDA-DETALLE.                                        02530000
      *________________________                                         02540000
           ADD  1                                  TO N.                02550000
           MOVE DIED-FECOPE                       TO W0091-FECOPE (N).  02560000
      *____TOTAL ABONADO______________________________________________  02561019
           MOVE DIED-INGRESO                      TO W0091-ABONOS (N).  02562019
      *____TOTAL CARGADO______________________________________________  02570000
           MOVE DIED-IMPORTE                      TO W0091-CARGOS (N).  02580019
                                                                        02610000
       900-FIN-PROGRAMA.                                                02620000
      *________________                                                 02630000
           EXEC  CICS  RETURN  END-EXEC.                                02640000
