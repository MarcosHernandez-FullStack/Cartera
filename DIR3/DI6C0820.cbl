      *_______________________                                          00010000
       IDENTIFICATION DIVISION.                                         00020000
      *_______________________                                          00030000
       PROGRAM-ID. DI6C0820.                                            00040000
      *________________________________________________________________ 00050000
      *                                                                 00060000
      *            DIETARIO CARTERA PARA CONTINET - DETALLE COBRANZAS   00070001
      *________________________________________________________________ 00080000
                                                                        00090000
       ENVIRONMENT DIVISION.                                            00100000
      *____________________                                             00110000
       CONFIGURATION SECTION.                                           00120000
                                                                        00150000
       DATA DIVISION.                                                   00160000
      *_____________                                                    00170000
       WORKING-STORAGE SECTION.                                         00190000
      *_______________________                                          00200000
       01  FILLER             PIC   X(40) VALUE                         00210000
                              '***INICIO WORKING-STORAGE SECTION***'.   00220000
                                                                        00230000
       01  RESPONSE                   PIC S9(09) COMP.                  00240000
                                                                        00250000
       01  WS-DIDIECOB-LENGTH         PIC S9(04) COMP  VALUE +200.      00260000
       01  WS-COD-FECOPE.                                               00270000
           02 WS-CODIGO.                                                00280000
              04 WS-CODOFI            PIC 9(03)        VALUE ZEROS.     00290000
              04 WS-CODMON            PIC 9(01)        VALUE ZEROS.     00300000
              04 WS-CODCLI            PIC 9(06)        VALUE ZEROS.     00310000
              04 WS-CODSER            PIC 9(02)        VALUE ZEROS.     00320000
           02 WS-FECOPE               PIC 9(08)        VALUE ZEROS.     00330000
       01  WS-FIN                     PIC 9            VALUE ZEROS.     00340000
       01  WS-NUMDOC-TOT              PIC 9(05)        VALUE ZEROS.     00350000
       01  I                          PIC 9(05)        VALUE ZEROS.     00360000
       01  J                          PIC 9(05)        VALUE ZEROS.     00370000
       01  N                          PIC 9(05)        VALUE ZEROS.     00380000
       01  WS-CODIGO-LEOS             PIC 9(03)        VALUE ZEROS.     00390000
       01  WS-CLASRT                  PIC 9(18)        VALUE ZEROS.     00400000
       01  WS-CLASRT-I                PIC 9(03)        VALUE ZEROS.     00410000
                                                                        00420000
       01  W820-SALIDA-S.                                               00430000
           10 W820-DETALLE-S          OCCURS 100 TIMES.                 00440000
              20 W820-MARSRT-S        PIC X.                            00450000
              20 W820-CLASRT-S.                                         00460000
                 30 W820-FECVEN-S     PIC 9(08).                        00470000
                 30 W820-NUMBCO-S     PIC 9(10).                        00480000
              20 W820-NUMBCOORI-S     PIC 9(10).                        00490001
              20 W820-NOMACE-S        PIC X(40).                        00491001
              20 W820-INGRESO-S       PIC 9(13)V99 COMP-3.              00500000
              20 W820-IMPORTE-S       PIC 9(13)V99 COMP-3.              00510000
              20 W820-INTERES-S       PIC 9(13)V99 COMP-3.              00520000
              20 W820-COMPORT-S       PIC 9(13)V99 COMP-3.              00530000
              20 W820-GASTOS-S        PIC 9(13)V99 COMP-3.              00540000
              20 W820-SITUAC-S        PIC X(03).                        00550000
                                                                        00560000
       01  REG-DIDIECOB.                                                00570000
           02  DIEC-KEY.                                                00580000
               04  DIEC-COD-FECOPE.                                     00590000
                   06  DIEC-CODIGO.                                     00600000
                       08  DIEC-CODOFI     PIC 9(03).                   00610000
                       08  DIEC-CODMON     PIC 9(01).                   00620000
                       08  DIEC-CODCLI     PIC 9(06).                   00630000
                       08  DIEC-CODSER     PIC 9(02).                   00640000
      *_________________________________________________________________00650000
      *            FECOPE = FECMOV     EL DATO SE OBTIENE DE DL0BCMOV   00660000
      *            FECOPE   SIEMPRE ES PRESENTADO DE MAYOR A MENOR      00670000
      *_________________________________________________________________00680000
                   06  DIEC-FECOPE         PIC 9(08).                   00690000
      *_________________________________________________________________00700000
      *            NUMBCO = 0000000000 ES REG DE TOTALES ABONO/CARGO    00710000
      *            NUMBCO = 0000000001 ES REG DE TOTALES                00720000
      *_________________________________________________________________00730000
               04  DIEC-NUMBCO             PIC 9(10).                   00740000
      *_________________________________________________________________00750000
      *            POSICION                                             00760000
      *            SITUAC = ATE     SI ES DOCUMENTO RENOV/CANCEL        00770000
      *            SITUAC = VIG     SI ES DOCUMENTO POR VENCER          00780000
      *_________________________________________________________________00790000
      *            DIETARIO COBRANZA                                    00800000
      *            SITUAC = CAN     SI ES DOCUMENTO CANCELADO           00810000
      *            SITUAC = REN     SI ES DOCUMENTO RENOVADO            00820000
      *            SITUAC = RRI     SI ES DOCUMENTO RENOVADO REINGRESO  00830000
      *            SITUAC = DEV     SI ES DOCUMENTO DEVUELTO            00840000
      *            SITUAC = DVP     SI ES DOCUMENTO DEVUELTO PROTESTADO 00850000
      *            SITUAC = DEE     SI ES DOCUMENTO DESCARGADO POR ERROR00860000
      *            SITUAC = ING     SI ES DOCUMENTO INGRESADO           00870000
      *            SITUAC = INE     SI ES DOCUMENTO INGRESADO  POR ERROR00880000
      *_________________________________________________________________00890000
               04  DIEC-SITUAC             PIC X(03).                   00900000
      *_________________________________________________________________00910000
      *            NUMDOC = NNNNN   SI ES REG DE TOTALES/REGISTR/POR REG00920000
      *            NUMDOC = 00000   SI ES REG DE DETALLE                00930000
      *_________________________________________________________________00940000
           02  DIEC-NUMDOC                 PIC 9(05).                   00950000
           02  DIEC-DETALLE.                                            00960000
               04  DIEC-FECVEN             PIC 9(08).                   00970000
      *_________________________________________________________________00980000
      *     NOMACE(01:20) = CTAABO  SI ES REG DE TOTALES (DL0BCSAL)     00990000
      *     NOMACE(21:20) = CTACAR  SI ES REG DE TOTALES (DL0BCSAL)     01000000
      *_________________________________________________________________01010000
               04  DIEC-NOMACE             PIC X(40).                   01020000
               04  DIEC-NOMACE-REDEFINE    REDEFINES DIEC-NOMACE.       01030000
                   06 DIEC-CTAABO          PIC X(20).                   01040000
                   06 DIEC-CTACAR          PIC X(20).                   01050000
      *_________________________________________________________________01060000
      *        IMPORTES   = ZEROS   SI ES REG DE TOTALES/REGISTR/POR REG01070000
      *_________________________________________________________________01080000
               04  DIEC-INGRESO            PIC 9(13)V99 COMP-3.         01090000
               04  DIEC-IMPORTE            PIC 9(13)V99 COMP-3.         01100000
               04  DIEC-INTERES            PIC 9(13)V99 COMP-3.         01110000
               04  DIEC-COMPORT            PIC 9(13)V99 COMP-3.         01120000
               04  DIEC-GASTOS             PIC 9(13)V99 COMP-3.         01130000
               04  DIEC-NUMBCOORI          PIC 9(10).                   01131002
           02  FILLER                      PIC X(39).                   01140002
                                                                        01150000
      *____ PARA OBTENER MONEDA DE LA CUENTA___________________________ 01160000
       01  BGECMDC-01.                                                  01170000
           COPY BGECMDC4.                                               01180000
                                                                        01190000
       01  FILLER             PIC   X(40) VALUE                         01200000
                              '***FINAL WORKING-STORAGE SECTION***'.    01210000
                                                                        01220000
       LINKAGE SECTION.                                                 01230000
      *_______________                                                  01240000
       01  DFHCOMMAREA.                                                 01250000
           COPY DIWC820.                                                01260000
                                                                        01270000
       PROCEDURE DIVISION.                                              01280000
      *__________________                                               01290000
                                                                        01300000
       000-INICIO.                                                      01310000
      *__________                                                       01320000
           PERFORM 100-INICIO-PROGRAMA.                                 01330000
           PERFORM 200-MODULO-CENTRAL.                                  01340000
           PERFORM 900-FIN-PROGRAMA.                                    01350000
                                                                        01360000
       100-INICIO-PROGRAMA.                                             01370000
      *___________________                                              01380000
           MOVE '00'                                  TO W820-CODRETOR. 01390000
           MOVE SPACES                                TO W820-REFERENC. 01400000
           MOVE SPACES                                TO W820-DIRCAPLN. 01410000
           INITIALIZE                                    W820-SALIDA.   01420000
           INITIALIZE                                    W820-SALIDA-S. 01430000
                                                                        01440000
           IF  (W820-CODIGO NOT NUMERIC)                                01450000
                MOVE '08'                             TO W820-CODRETOR  01460000
                MOVE 'ERROR CODIGO NO NUMERICO'       TO W820-REFERENC  01470000
                MOVE 'DIE8208'                        TO W820-DIRCAPLN  01480000
                PERFORM 900-FIN-PROGRAMA                                01490000
           END-IF.                                                      01500000
                                                                        01510000
           IF  (W820-FECOPE NOT NUMERIC)                                01520000
                MOVE '09'                             TO W820-CODRETOR  01530000
                MOVE 'ERROR FECHA OPERACION INVALIDO' TO W820-REFERENC  01540000
                MOVE 'DIE8209'                        TO W820-DIRCAPLN  01550000
                PERFORM 900-FIN-PROGRAMA                                01560000
           END-IF.                                                      01570000
                                                                        01580000
           IF  (W820-INDIC-PAGINAC = 'S' OR ' ')                        01590000
                IF W820-INDIC-PAGINAC = 'S'                             01600000
                   IF W820-NUMBCO-PAGINAC > ZEROS                       01610000
                      CONTINUE                                          01620000
                   ELSE                                                 01630000
                      MOVE '12'                       TO W820-CODRETOR  01640000
                      MOVE 'ERROR NUMBCO PAGINAC INVALIDO'              01650000
                                                      TO W820-REFERENC  01660000
                      MOVE 'DIE8212'                  TO W820-DIRCAPLN  01670000
                      PERFORM 900-FIN-PROGRAMA                          01680000
                   END-IF                                               01690000
                END-IF                                                  01700000
           ELSE                                                         01710000
                MOVE '10'                             TO W820-CODRETOR  01720000
                MOVE 'ERROR INDIC PAGINAC INVALIDO  ' TO W820-REFERENC  01730000
                MOVE 'DIE8210'                        TO W820-DIRCAPLN  01740000
                PERFORM 900-FIN-PROGRAMA                                01750000
           END-IF.                                                      01760000
                                                                        01770000
           PERFORM 110-POSICIONA-TITULO.                                01780000
                                                                        01790000
       110-POSICIONA-TITULO.                                            01800000
      *____________________                                             01810000
           MOVE W820-CODIGO                           TO DIEC-CODIGO    01820000
                                                           WS-CODIGO.   01830000
           MOVE W820-FECOPE                           TO DIEC-FECOPE    01840000
                                                           WS-FECOPE.   01850000
           IF W820-INDIC-PAGINAC = ' '                                  01860000
              MOVE ZEROS                              TO DIEC-NUMBCO    01870000
           ELSE                                                         01880000
              MOVE W820-NUMBCO-PAGINAC                TO DIEC-NUMBCO    01890000
           END-IF.                                                      01900000
           MOVE SPACES                                TO DIEC-SITUAC.   01910000
                                                                        01920000
           EXEC CICS STARTBR DATASET   ('DIDIECOB')                     01930000
                             RIDFLD    (DIEC-KEY)                       01940000
                             KEYLENGTH (+33)                            01950000
                             RESP      (RESPONSE)                       01960000
                             GTEQ                                       01970000
                             NOHANDLE                                   01980000
           END-EXEC.                                                    01990000
                                                                        02000000
           EVALUATE RESPONSE                                            02010000
           WHEN DFHRESP(NORMAL)                                         02020000
           WHEN DFHRESP(LENGERR)                                        02030000
                CONTINUE                                                02040000
           WHEN DFHRESP(NOTFND)                                         02050000
                MOVE '97'                             TO W820-CODRETOR  02060000
                MOVE 'ERROR DIDIECOB CLAVE NO EXISTE' TO W820-REFERENC  02070000
                MOVE 'DIE8297'                        TO W820-DIRCAPLN  02080000
                PERFORM 900-FIN-PROGRAMA                                02090000
           WHEN DFHRESP(NOTOPEN)                                        02100000
                MOVE '98'                             TO W820-CODRETOR  02110000
                MOVE 'ERROR DIDIECOB CERRADO '        TO W820-REFERENC  02120000
                MOVE 'DIE8298'                        TO W820-DIRCAPLN  02130000
                PERFORM 900-FIN-PROGRAMA                                02140000
           WHEN OTHER                                                   02150000
                MOVE '99'                             TO W820-CODRETOR  02160000
                MOVE 'ERROR DIDIECOB STARTBR '        TO W820-REFERENC  02170000
                MOVE 'DIE8299'                        TO W820-DIRCAPLN  02180000
                PERFORM 900-FIN-PROGRAMA                                02190000
           END-EVALUATE.                                                02200000
                                                                        02210000
       200-MODULO-CENTRAL.                                              02220000
      *__________________                                               02230000
           IF W820-INDIC-PAGINAC = ' '                                  02240000
              PERFORM 220-LEER-NEXT                                     02250000
              PERFORM 230-LLENA-SALIDA                                  02260000
              MOVE DIEC-NUMDOC                        TO WS-NUMDOC-TOT  02270000
           END-IF.                                                      02280000
                                                                        02290000
           PERFORM 210-OBTIENE-DATOS UNTIL                              02300000
                   N                 > 100            OR                02310000
                   WS-FIN            = 1              OR                02320000
                   WS-COD-FECOPE NOT = DIEC-COD-FECOPE.                 02330000
                                                                        02340000
      *       MOVE '0X'                  TO W820-CODRETOR.              02350000
      *       MOVE W820-CTAABO-MON       TO W820-REFERENC(01:12)        02360000
      *       MOVE W820-CTACAR-MON       TO W820-REFERENC(13:12).       02370000
      *       MOVE W820-SERVICIO-TIT(1:6) TO W820-REFERENC(25:06).      02380000
      *       PERFORM 900-FIN-PROGRAMA                                  02390000
      *       MOVE W820-FECVEN(W820-NUMDOC-TOT) TO W820-REFERENC(14:08).02400000
      *       MOVE W820-NUMBCO(W820-NUMDOC-TOT) TO W820-REFERENC(22:09).02410000
      *       MOVE W820-NUMDOC-TOT       TO W820-REFERENC(11:05).       02420000
      *       MOVE W820-NUMDOC-TOT-PR    TO W820-REFERENC(16:05).       02430000
      *       MOVE W820-IMPORT-TOT       TO WS-IMPRES-ED.               02440000
      *       MOVE W820-IMPORT-TOT-PR    TO WS-IMPRES-ED.               02450000
      *       MOVE WS-IMPRES-ED          TO W820-REFERENC(11:16).       02460000
                                                                        02470000
      *    PERFORM 500-CLASIFICA-DETALLES.                              02480000
                                                                        02490000
           IF WS-FIN = 1                                                02500000
              ADD   WS-NUMDOC-TOT      TO W820-OCURRENCIAS              02510000
              MOVE 'N'                 TO W820-INDIC-PAGINAC            02520000
              INITIALIZE                  W820-AREA-PAGINAC             02530000
           ELSE                                                         02540000
              MOVE  ZEROS              TO W820-OCURRENCIAS              02550000
              MOVE 'S'                 TO W820-INDIC-PAGINAC            02560000
              MOVE  DIEC-NUMBCO        TO W820-NUMBCO-PAGINAC           02570000
           END-IF.                                                      02580000
                                                                        02590000
           EXEC CICS ENDBR    DATASET   ('DIDIECOB')                    02600000
                              RESP      (RESPONSE)                      02610000
                              NOHANDLE                                  02620000
           END-EXEC.                                                    02630000
                                                                        02640000
           EVALUATE RESPONSE                                            02650000
           WHEN DFHRESP(NORMAL)                                         02660000
                CONTINUE                                                02670000
           WHEN OTHER                                                   02680000
                MOVE '91'                             TO W820-CODRETOR  02690000
                MOVE 'ERROR DIDIECOB ENDBR   '        TO W820-REFERENC  02700000
                MOVE 'DIE8291'                        TO W820-DIRCAPLN  02710000
                PERFORM 900-FIN-PROGRAMA                                02720000
           END-EVALUATE.                                                02730000
                                                                        02740000
       210-OBTIENE-DATOS.                                               02750000
      *_________________                                                02760000
           PERFORM 220-LEER-NEXT.                                       02770000
           IF WS-FIN = 0                                                02780000
              IF  WS-COD-FECOPE = DIEC-COD-FECOPE                       02790000
                  PERFORM 230-LLENA-SALIDA                              02800000
              ELSE                                                      02810000
                  MOVE 1                              TO WS-FIN         02820000
                  MOVE 'CNE0141'                      TO W820-DIRCAPLN  02821000
              END-IF                                                    02822000
           END-IF.                                                      02823000
                                                                        02824000
       220-LEER-NEXT.                                                   02825000
      *_____________                                                    02826000
           EXEC CICS READNEXT  DATASET   ('DIDIECOB')                   02827000
                               RIDFLD    (DIEC-KEY)                     02828000
                               INTO      (REG-DIDIECOB)                 02829000
                               RESP      (RESPONSE)                     02830000
                               NOHANDLE                                 02840000
           END-EXEC.                                                    02850000
                                                                        02860000
           EVALUATE RESPONSE                                            02870000
           WHEN DFHRESP(NORMAL)                                         02880000
           WHEN DFHRESP(LENGERR)                                        02890000
                IF WS-COD-FECOPE = DIEC-COD-FECOPE                      02900000
                   ADD 1                              TO WS-CODIGO-LEOS 02910000
                ELSE                                                    02920000
                   MOVE 1                             TO WS-FIN         02930000
                   MOVE 'CNE0141'                     TO W820-DIRCAPLN  02940000
                   IF WS-CODIGO-LEOS = 0                                02950000
                      MOVE '97'                       TO W820-CODRETOR  02960000
                      MOVE 'ERROR DIDIECOB CLAVE NO EXISTE'             02970000
                                                      TO W820-REFERENC  02980000
                      MOVE 'DIE8297'                  TO W820-DIRCAPLN  02990000
                      PERFORM 900-FIN-PROGRAMA                          03000000
                   END-IF                                               03010000
                END-IF                                                  03020000
           WHEN DFHRESP(ENDFILE)                                        03030000
                MOVE 1                                TO WS-FIN         03040000
                MOVE 'CNE0141'                        TO W820-DIRCAPLN  03050000
           WHEN OTHER                                                   03060000
                MOVE '96'                             TO W820-CODRETOR  03070000
                MOVE 'ERROR DIDIECOB READNEXT'        TO W820-REFERENC  03080000
                MOVE 'DIE8296'                        TO W820-DIRCAPLN  03090000
                PERFORM 900-FIN-PROGRAMA                                03100000
           END-EVALUATE.                                                03110000
                                                                        03120000
       230-LLENA-SALIDA.                                                03130000
      *________________                                                 03140000
           EVALUATE DIEC-NUMBCO                                         03150000
               WHEN 0000000000         PERFORM 240-LLENA-SALIDA-TITULOS 03160000
               WHEN 0000000001         PERFORM 250-LLENA-SALIDA-TOTALES 03170000
               WHEN 2 THRU 9999999999  PERFORM 260-LLENA-SALIDA-DETALLE 03180000
           END-EVALUATE.                                                03190000
                                                                        03200000
       240-LLENA-SALIDA-TITULOS.                                        03210000
      *________________________                                         03220000
           MOVE WS-CODIGO                          TO W820-CODIGO-TIT.  03230000
           EVALUATE WS-CODSER                                           03240000
               WHEN  7                                                  03250000
               WHEN  8                                                  03260000
               WHEN 13 MOVE 'COBRANZA GARANTIA   ' TO W820-SERVICIO-TIT 03270000
               WHEN OTHER                                               03280000
                       MOVE 'COBRANZA LIBRE      ' TO W820-SERVICIO-TIT 03290000
           END-EVALUATE.                                                03300000
           MOVE DIEC-CTAABO                        TO W820-CTAABO.      03310000
           MOVE DIEC-CTACAR                        TO W820-CTACAR.      03320000
           PERFORM 241-OBTIENE-DIVISA.                                  03330000
      *____TOTAL ABONADO______________________________________________  03340000
           MOVE DIEC-INGRESO                       TO W820-INGRESO-TIT. 03350000
      *____TOTAL CARGADO______________________________________________  03360000
           MOVE DIEC-IMPORTE                       TO W820-IMPORTE-TIT. 03370000
                                                                        03380000
       241-OBTIENE-DIVISA.                                              03390000
      *___________________                                              03400000
           INITIALIZE                                 BGECMDC.          03410000
                                                                        03420000
           MOVE DIEC-CTAABO(1:4)                   TO MDC-ENTIDAD       03430000
           MOVE DIEC-CTAABO(5:4)                   TO MDC-CENTRO-ALTA   03440000
           MOVE DIEC-CTAABO(11:10)                 TO MDC-CUENTA        03450000
                                                                        03460000
           EXEC CICS                                                    03470000
                LINK PROGRAM('BG2CMDC4')                                03480000
                    COMMAREA(BGECMDC)                                   03490000
           END-EXEC.                                                    03500000
                                                                        03510000
           IF EIBRESP EQUAL DFHRESP(NORMAL)                             03520000
              IF MDC-CODERR = SPACES                                    03530000
                 EVALUATE MDC-DIVISA(1:3)                               03540000
                     WHEN 'PEN'  MOVE 'SOLES'        TO W820-CTAABO-MON 03550000
                     WHEN 'USD'  MOVE 'DOLARES'      TO W820-CTAABO-MON 03560000
                     WHEN 'EUR'  MOVE 'EUROS  '      TO W820-CTAABO-MON 03570000
                     WHEN 'GBP'  MOVE 'LIBRAS ESTER' TO W820-CTAABO-MON 03580000
                     WHEN  OTHER MOVE MDC-DIVISA     TO W820-CTAABO-MON 03590000
                 END-EVALUATE                                           03600000
              ELSE                                                      03610000
                 MOVE '81'                           TO W820-CODRETOR   03620000
                 MOVE 'CUENTA ALTAMIRA NO EXISTE  '  TO W820-REFERENC   03630003
                 MOVE 'DIE8281'                      TO W820-DIRCAPLN   03640000
                 PERFORM 900-FIN-PROGRAMA                               03650000
              END-IF                                                    03660000
           ELSE                                                         03670000
              MOVE '80'                              TO W820-CODRETOR   03680000
              MOVE 'ERROR RUTINA OBTIENE DIVISA'     TO W820-REFERENC   03690000
              MOVE 'DIE8280'                         TO W820-DIRCAPLN   03700000
              PERFORM 900-FIN-PROGRAMA                                  03710000
           END-IF.                                                      03720000
                                                                        03730000
           INITIALIZE                                 BGECMDC.          03740000
                                                                        03750000
           MOVE DIEC-CTACAR(1:4)                     TO MDC-ENTIDAD     03760000
           MOVE DIEC-CTACAR(5:4)                     TO MDC-CENTRO-ALTA 03770000
           MOVE DIEC-CTACAR(11:10)                   TO MDC-CUENTA      03780000
                                                                        03790000
           EXEC CICS                                                    03800000
                LINK PROGRAM('BG2CMDC4')                                03810000
                    COMMAREA(BGECMDC)                                   03820000
           END-EXEC.                                                    03830000
                                                                        03840000
           IF EIBRESP EQUAL DFHRESP(NORMAL)                             03850000
              IF MDC-CODERR = SPACES                                    03860000
                 EVALUATE MDC-DIVISA(1:3)                               03870000
                     WHEN 'PEN'  MOVE 'SOLES'        TO W820-CTACAR-MON 03880000
                     WHEN 'USD'  MOVE 'DOLARES'      TO W820-CTACAR-MON 03890000
                     WHEN 'EUR'  MOVE 'EUROS  '      TO W820-CTACAR-MON 03900000
                     WHEN 'GBP'  MOVE 'LIBRAS EST'   TO W820-CTACAR-MON 03910000
                     WHEN  OTHER MOVE MDC-DIVISA     TO W820-CTACAR-MON 03920000
                 END-EVALUATE                                           03930000
              ELSE                                                      03940000
                 MOVE '81'                           TO W820-CODRETOR   03950000
                 MOVE 'CUENTA C ALTAMIRA NO EXISTE'  TO W820-REFERENC   03960000
                 MOVE 'DIE8281'                      TO W820-DIRCAPLN   03970000
                 PERFORM 900-FIN-PROGRAMA                               03980000
              END-IF                                                    03990000
           ELSE                                                         04000000
              MOVE '80'                              TO W820-CODRETOR   04010000
              MOVE 'ERROR RUTINA OBTIENE DIVISA'     TO W820-REFERENC   04020000
              MOVE 'DIE8280'                         TO W820-DIRCAPLN   04030000
              PERFORM 900-FIN-PROGRAMA                                  04040000
           END-IF.                                                      04050000
                                                                        04060000
       250-LLENA-SALIDA-TOTALES.                                        04070000
      *________________________                                         04080000
           MOVE WS-FECOPE                          TO W820-FECOPE-TOT.  04090000
           MOVE DIEC-NUMDOC                        TO W820-NUMDOC-TOT.  04100000
      *    MOVE DIEC-IMPORTE                       TO W820-IMPORTE-TOT. 04110000
           MOVE DIEC-INTERES                       TO W820-INTERES-TOT. 04120000
           MOVE DIEC-COMPORT                       TO W820-COMPORT-TOT. 04130000
           MOVE DIEC-GASTOS                        TO W820-GASTOS-TOT.  04140000
                                                                        04140100
           COMPUTE W820-IMPORTE-TOT                =  W820-INGRESO-TIT -04140200
                                                      W820-INTERES-TOT. 04140300
                                                                        04140400
       260-LLENA-SALIDA-DETALLE.                                        04140500
      *________________________                                         04140600
           ADD  1                                  TO N.                04140700
           MOVE DIEC-FECVEN                        TO W820-FECVEN  (N). 04140800
           MOVE DIEC-NUMBCO                        TO W820-NUMBCO  (N). 04140900
           MOVE DIEC-NUMBCOORI                     TO W820-NUMBCOORI(N).04141002
           MOVE DIEC-NOMACE                        TO W820-NOMACE  (N). 04141102
           MOVE DIEC-INGRESO                       TO W820-INGRESO (N). 04142000
           MOVE DIEC-IMPORTE                       TO W820-IMPORTE (N). 04143000
           MOVE DIEC-INTERES                       TO W820-INTERES (N). 04144000
           MOVE DIEC-COMPORT                       TO W820-COMPORT (N). 04145000
           MOVE DIEC-GASTOS                        TO W820-GASTOS  (N). 04146000
           MOVE DIEC-SITUAC                        TO W820-SITUAC  (N). 04147000
                                                                        04148000
           IF N > 1                                                     04148104
           IF W820-SITUAC (N) = 'RRI' AND W820-SITUAC (N - 1) = 'REN'   04149000
              IF W820-NUMBCO (N) = W820-NUMBCO (N - 1)                  04150000
                 MOVE    W820-IMPORTE (N - 1) TO   W820-INGRESO(N - 1)  04160000
                 COMPUTE W820-IMPORTE (N - 1) =    W820-INGRESO(N - 1) -04170000
                                                   W820-INGRESO(N)      04180000
              END-IF                                                    04190000
           END-IF                                                       04200004
           END-IF.                                                      04201004
                                                                        04210000
      *500-CLASIFICA-DETALLES.                                          04220000
      *______________________                                           04230000
      *    PERFORM VARYING J FROM 1 BY 1 UNTIL J > N                    04240000
      *                                                                 04250000
      *       MOVE 999999999999999999               TO WS-CLASRT        04260000
      *       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N                 04270000
      *          IF W820-MARSRT-S(I) NOT = 'S'                          04280000
      *             IF  W820-CLASRT-S(I) < WS-CLASRT                    04290000
      *                 MOVE W820-CLASRT-S(I)       TO WS-CLASRT        04300000
      *                 MOVE I                      TO WS-CLASRT-I      04310000
      *             END-IF                                              04320000
      *          END-IF                                                 04330000
      *       END-PERFORM                                               04340000
      *                                                                 04350000
      *       MOVE 'S'               TO W820-MARSRT-S (WS-CLASRT-I)     04360000
      *       MOVE W820-FECVEN-S (WS-CLASRT-I)      TO W820-FECVEN (J)  04370000
      *       MOVE W820-NUMBCO-S (WS-CLASRT-I)      TO W820-NUMBCO (J)  04380000
      *       MOVE W820-NOMACE-S (WS-CLASRT-I)      TO W820-NOMACE (J)  04390000
      *       MOVE W820-INGRESO-S(WS-CLASRT-I)      TO W820-INGRESO(J)  04400000
      *       MOVE W820-IMPORTE-S(WS-CLASRT-I)      TO W820-IMPORTE(J)  04410000
      *       MOVE W820-INTERES-S(WS-CLASRT-I)      TO W820-INTERES(J)  04420000
      *       MOVE W820-COMPORT-S(WS-CLASRT-I)      TO W820-COMPORT(J)  04430000
      *       MOVE W820-GASTOS-S (WS-CLASRT-I)      TO W820-GASTOS (J)  04440000
      *       MOVE W820-SITUAC-S (WS-CLASRT-I)      TO W820-SITUAC (J)  04450000
      *                                                                 04460000
      *    END-PERFORM.                                                 04470000
                                                                        04480000
       900-FIN-PROGRAMA.                                                04490000
      *________________                                                 04500000
           EXEC  CICS  RETURN  END-EXEC.                                04510000
