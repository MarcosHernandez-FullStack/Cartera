      *----------------------------------------------------------------*
      *   FUNCION DE NEGOCIO PARA CANALES ALTERNATIVOS                 *
      *                                                                *
      *   DI2CDIR3 : DET.OPER. COBRANZA LIBRE                          *
      *                                                                *
      * RUTINAS UTILIZADAS:                                            *
      *                                                                *
      *   KN1CC260  RUTINA VALIDACION DE ACCESO A FUNCIONES DE NEGOCIO *
      *   KN1CC300  RUTINA QUE GRABA EN EL LOG DE OPERACIONES CANALES  *
      *   DI6C0820  RUTINA QUE DEVUELVE DET.OPER. COBRANZA LIBRE       *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *   MODIFICO  FECHA  DESCRIPCION                                 *
      *   --------  -----  -----------                                 *
      *                                                                *
      *----------------------------------------------------------------*
      *                    IDENTIFICATION DIVISION                     *
      *----------------------------------------------------------------*

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DI2CDIR3.
       AUTHOR.        DIDIER DAHIAN MORENO MOSQUERA.
       DATE-WRITTEN.  06-JUL-2026.
      *                                                                *
      *----------------------------------------------------------------*
      *                     ENVIRONMENT DIVISION                       *
      *----------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-3090.
       OBJECT-COMPUTER.
           IBM-3090.

      *----------------------------------------------------------------*
      *                         DATA DIVISION                          *
      *----------------------------------------------------------------*

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *            IDENTIFICADOR DE INICIO DE WORKING STORAGE
       01 LT-INICIO-WS.
          05 LT-INICIO-W                    PIC X(50) VALUE
                 '*** INICIO WORKING STORAGE (KN1CKW3N) ***'.

      *----------------------------------------------------------------*
      *                           CONSTANTES                           *
      *----------------------------------------------------------------*

       01  WSC-CONSTANTES.
           05 WSC-KN1CC260             PIC X(08)       VALUE 'KN1CC260'.
           05 WSC-KN1CC300             PIC X(08)       VALUE 'KN1CC300'.
           05 CON-QCCECSO0             PIC X(08)       VALUE 'QCCECSO0'.
           05 WSC-DIECBS3N             PIC X(08)       VALUE 'DIECBS3N'.
           05 WSC-DIECBD3N             PIC X(08)       VALUE 'DIECBD3N'.
           05 WSC-DI2CDIR3             PIC X(08)       VALUE 'DI2CDIR3'.
           05 WSC-DI6C0822             PIC X(08)       VALUE 'DI6C0822'.
           05 WSC-FUNCION              PIC X(04)       VALUE '+DC1'.
           05 WSC-ASTA                 PIC X(04)       VALUE 'ASTA'.
           05 WSC-RETORNO-OK           PIC X(02)       VALUE '00'.
           05 WSC-RETORNO-APL          PIC X(02)       VALUE '10'.
           05 WSC-RC-ERR-CICS          PIC X(02)       VALUE '98'.
           05 WSC-RETORNO-NOK          PIC X(02)       VALUE '99'.
           05 WSC-ACEPTADA             PIC X(01)       VALUE 'S'.
           05 WSC-NO-ACEPTADA          PIC X(01)       VALUE 'N'.
           05 WSC-SIN-ROLLBACK         PIC X(02)       VALUE '01'.
           05 WSC-CON-ROLLBACK         PIC X(02)       VALUE '02'.
           05 WSC-S                    PIC X(01)       VALUE 'S'.
           05 WSC-N                    PIC X(01)       VALUE 'N'.
           05 WSC-P                    PIC X(01)       VALUE 'P'.
           05 WSC-GUION                PIC X(01)       VALUE '-'.
           05 WSC-CNE9998              PIC X(07)       VALUE  'CNE9998'.

      *----------------------------------------------------------------*
      *                           VARIABLES                            *
      *----------------------------------------------------------------*
       01  WSA-VARIABLES.
           05 WSA-IND-OK               PIC X(02)       VALUE SPACES.
           05 WSA-SALIDA-TS            PIC X(256)      VALUE SPACES.
           05 WSN-TSLENGTH             PIC S9(04) COMP VALUE ZEROS.
           05 WSA-PAGINAC-COMP         PIC X(10)    JUSTIFIED RIGHT.
           05 WSA-CONTROL-RESPUESTA.
              10 WSA-COD-RESPUESTA     PIC X(02)       VALUE SPACES.
              10 WSA-COD-ERROR.
                 15 WSA-VAR-TIPO       PIC X(03)       VALUE SPACES.
                 15 WSN-VAR-NUMERO     PIC 9(04)       VALUE ZEROES.
              10 WSA-VALOR-ERROR-UNO   PIC X(20)       VALUE SPACES.
              10 WSA-VALOR-ERROR-DOS   PIC X(20)       VALUE SPACES.
              10 WSA-COD-AVISO         PIC X(07)       VALUE SPACES.
           05 WSA-TS.
              10 WSA-TS-NOMBRE         PIC X(04)       VALUE SPACES.
              10 WSA-TS-SUFIJO         PIC X(04)       VALUE SPACES.
           05 WSA-OBJETO               PIC X(08)       VALUE SPACES.
           05 WSA-REFERENCIA           PIC X(20)       VALUE SPACES.
           05 WSA-FECHA-AAAAMMDD       PIC X(10)    VALUE '0001-01-01'.
           05 WSA-TIME-HHMMSS          PIC X(08)    VALUE '00:00:00'.
           05 WSN-ABSTIME              PIC S9(15)      COMP-3 VALUE 0.
           05 WSN-I                    PIC 9(03)       VALUE ZEROES.
           05 WSA-ARGUMENTO.
              10 WSN-ARGUMENTO         PIC 9(10)       VALUE ZEROES.
              10 FILLER                PIC X(41)       VALUE SPACES.
           05 WSA-FECHA-8              PIC 9(08).
           05 WSA-FECHA-10             PIC X(10).
       01  SW-FIN-MOVIMIENTOS          PIC X(01)       VALUE 'N'.
           88 FIN-MOVIMIENTOS                          VALUE 'S'.
       01  SW-OPERACION-OK             PIC X(01)       VALUE 'N'.
           88 OPERACION-OK                             VALUE 'S'.

      *----------------------------------------------------------------*
      *             COPY AREA DE TRABAJO DE COLECTOR KN1CC260          *
      *----------------------------------------------------------------*
       01  REG-C26-KNDCC260.
           COPY KNDCC261.
      *----------------------------------------------------------------*
      *             COPY AREA DE TRABAJO DE COLECTOR KN1CC300          *
      *----------------------------------------------------------------*
       01  REG-C30-KNDCC300.
           COPY KNDCC301.
      *----------------------------------------------------------------*
      * COPY PARA EL PROGRAMA DE ABEND
      *----------------------------------------------------------------*
       01  QGECABC-01.
           COPY QGECABC.
      *----------------------------------------------------------------*
      *           COPY AREA DE SALIDA DEL MENSAJE                      *
      *----------------------------------------------------------------*
      *    COPY KNECBS3N.
      *    COPY KNECBD3N.
           COPY DIECBS3N.
           COPY DIECBD3N.
      *----------------------------------------------------------------*
      *             AREA DE ERRORES PARA LOG DE ARQUITECTURA           *
      *----------------------------------------------------------------*
           COPY CNDCERR0.
           COPY CNDCERRW.
      *------ COPY AREA DE TRABAJO DE LA RUTINA CN9CE060 --------------*
      *
       COPY CNDCE060.
      *
      *-------------COPY AREA DE TRABAJO DE LA RUTINA DI6C0820 --------*
      *
       01 WS-DIWC822.
       COPY DIWC822.
PMI                 20 W822-TIPDOC         PIC 9(02).
      *


       01 LT-FINAL-WS.
          05 LT-FINAL-W                     PIC X(40) VALUE
                   '*** FINAL WORKING STORAGE  (KN1CKW3N) **'.

       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY QGECCAA.
      *    COPY KNECCIC3.
           COPY DIECBE33.

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION                       *
      *----------------------------------------------------------------*

       PROCEDURE DIVISION.
           PERFORM  100000-INICIO
           PERFORM  200000-PROCESO
           PERFORM  300000-FIN.

      *----------------------------------------------------------------*
      *    INICIALIZACION DE VARIABLES Y DIRECCIONAMIENTO              *
      *    DEL AREA DE COMUNICACIONES STANDARD                         *
      *----------------------------------------------------------------*
       100000-INICIO.
      *-------------*
      *
      *
           EXEC CICS
              IGNORE CONDITION ERROR
           END-EXEC

           REPLACE ==PISPLAY== BY ==DISPLAY==  .

           PISPLAY 'DIDIER-INICIA DI2CDIR3'
           PISPLAY '----------------------'
           PISPLAY 'DIDIER-RETORNO-CICS-CONDITION'
           PISPLAY 'EIBRESP---' EIBRESP
           PISPLAY '----------------------'


           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
              MOVE 'ERROR CICS IGNORE'         TO ABC-REFERENCIA
              PERFORM 900100-MOVER-ERRORES-CICS
              PERFORM 300000-FIN
           END-IF

           SET ADDRESS OF BE33-DIECBE33    TO CAA-PTR-COPYIN.

            MOVE SPACES                    TO CAA-COD-AVISO1
                                              CAA-COD-AVISO2
                                              CAA-COD-ERROR
                                             CAA-CODTRAN-SIG
           INITIALIZE C26-KNDCC260
                      C30-KNDCC300
                      CAA-INFORMA-ERROR

      *    PERFORM 1070-VALIDA-ENTRADA
           MOVE WSC-FUNCION                TO WSA-TS-NOMBRE
           MOVE CAA-TERMINAL               TO WSA-TS-SUFIJO
           MOVE WSC-ACEPTADA               TO C30-IND-ACEPTADA
           PERFORM 100100-BORRAR-COLAS-TS.
      *----------------------------------------------------------------*
      * VALIDA ENTRADA                                                 *
      *----------------------------------------------------------------*
      *1070-VALIDA-ENTRADA.
      *-------------------*
      *    IF BE03-USUARIO1 IS NOT NUMERIC
      *       MOVE CON-DIE0550                 TO CAA-COD-ERROR
      *       MOVE 'CAMPO CODIGO CLIENTE'      TO CAA-VAR1-ERROR
      *       MOVE 'NO NUMERICO         '      TO CAA-VAR2-ERROR
      *       MOVE CON-DI2CDIR8                TO CAA-OBJETO-ERROR
      *       PERFORM 300000-FIN
      *    END-IF
      *
      *    IF BE03-BCODCTAA (1:12) IS NOT NUMERIC
      *       MOVE CON-DIE0550                 TO CAA-COD-ERROR
      *       MOVE 'CAMPO CODIGO CARTERA'      TO CAA-VAR1-ERROR
      *       MOVE 'NO NUMERICO         '      TO CAA-VAR2-ERROR
      *       MOVE CON-DI2CDIR8                TO CAA-OBJETO-ERROR
      *       PERFORM 300000-FIN
      *    END-IF
      *
      *    IF BE03-BFECHA01 = SPACES
      *       MOVE CON-DIE0550                 TO CAA-COD-ERROR
      *       MOVE 'CAMPO FECHA '              TO CAA-VAR1-ERROR
      *       MOVE 'NO INGRESADO  '            TO CAA-VAR2-ERROR
      *       MOVE CON-DI2CDIR8                TO CAA-OBJETO-ERROR
      *       PERFORM 300000-FIN
      *    END-IF
      *
      *    MOVE BE03-BFECHA01(1:4)             TO WSV-AA1
      *    MOVE BE03-BFECHA01(6:2)             TO WSV-MM1
      *    MOVE BE03-BFECHA01(9:2)             TO WSV-DD1
      *    IF (WSV-AA1 < 1900) OR
      *       (WSV-MM1 < 1  OR WSV-MM1 > 12) OR
      *       (WSV-DD1 < 1  OR WSV-MM1 > 31)
      *       MOVE CON-DIE0550                 TO CAA-COD-ERROR
      *       MOVE 'CAMPO FECHA '              TO CAA-VAR1-ERROR
      *       MOVE 'NO VALIDO'                 TO CAA-VAR2-ERROR
      *       MOVE CON-DI2CDIR8                TO CAA-OBJETO-ERROR
      *       PERFORM 300000-FIN
      *    END-IF
      *    .
      *----------------------------------------------------------------*
      * OBTIENE CANAL DE TRABAJO                                       *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *       BORRA LA COLA DE SALIDA EN CASO DE QUE EXISTA            *
      *----------------------------------------------------------------*
       100100-BORRAR-COLAS-TS.
      *----------------------*
           PISPLAY 'DIDIER BORRAR-COLAS-TS'
      *
           EXEC CICS
                DELETEQ TS QUEUE(WSA-TS)
           END-EXEC
           EVALUATE EIBRESP
              WHEN DFHRESP(NORMAL)
      *            SET ADDRESS OF WSV-TSDATOZ TO WSV-PTR-TS
      *            MOVE WSV-TSDATOZ           TO QGECCNLM
                   CONTINUE
              WHEN DFHRESP(QIDERR)
      *            MOVE CON-DIE5167           TO CAA-COD-ERROR
      *            PERFORM 3000-FIN
                   CONTINUE
              WHEN OTHER
                  MOVE 'ERROR CICS READQ'     TO ABC-REFERENCIA
                  MOVE CON-QCCECSO0           TO ABC-PROGRAMA
                  MOVE WSA-TS                 TO CAA-OBJETO-ERROR
                  PERFORM 900100-MOVER-ERRORES-CICS
           END-EVALUATE
           .
      *
      *----------------------------------------------------------------*
      *  EL PROCESO ESTA DIVIDIDO EN TRES PARTES, QUE LLEVAN A CABO POR*
      *  SEPARADO LAS ACTIVIDADES DE INICIO Y FIN DEL AST QUE LES SON  *
      *  COMUNES Y LAS ESPECIFICAS DE LA FUNCION DE NEGOCIO.           *
      *----------------------------------------------------------------*
       200000-PROCESO.
      *--------------*
           PISPLAY '-----------------'
           PISPLAY 'DATOS DE ENTRADA:'
           PISPLAY 'BE33-USUARIO1:'BE33-USUARIO1
           PISPLAY 'BE33-BCODCTAA:'BE33-BCODCTAA
           PISPLAY 'BE33-BFECHA01:'BE33-BFECHA01
           PISPLAY 'BE33-BINDPAGI:'BE33-BINDPAGI
           PISPLAY 'BE33-BPAGINAC:'BE33-BPAGINAC
           PISPLAY 'BE33-BPAGSIZE:'BE33-BPAGSIZE
           PISPLAY '-----------------'
           PISPLAY 'DIDIER PROCESO'
      *    PERFORM 200100-PREPROCESO-AST
           PERFORM 200200-PROCESO-NEGOCIO.
      *    PERFORM 200400-FIN-PROCESO-AST.
      *
      *----------------------------------------------------------------*
      *    EN ESTE PREPROCESO SE LLEVA A CABO LA VERIFICACION DE       *
      *    ACCESO DEL CLIENTE A LA FUNCION, EN DIVERSOS ASPECTOS DE    *
      *    SEGURIDAD, ACCESO A OPERACIONES MONETARIOS, PERFIL, ETC.    *
      *                                                                *
      *    LA VALIDACION SE REALIZA A TRAVES DEL COLECTOR KN1CC260 Y   *
      *    SE DEBERAN LLENAR LOS CAMPOS DE ESTE CONFORME LOS REQUERI   *
      *    MIENTOS DE EJECUCION DE LA FUNCION DE NEGOCIO               *
      *----------------------------------------------------------------*
      *200100-PREPROCESO-AST.
      *---------------------*
      *    MOVE CCIC0-CNFUNCID             TO C26-COD-FUNCION
      *    SET  C26-PTR-COPYIN             TO CCIC0-PTR-APL-AST
      *    MOVE SPACES                     TO C26-ASUNTO-PROPIO-TIPO
      *                                       C26-ASUNTO-PROPIO-ID
      *                                       C26-ASUNTO-ASOCIADO-TIPO
      *                                       C26-ASUNTO-ASOCIADO-ID
      *    MOVE ZEROES                     TO C26-IMPORTE
      *    MOVE SPACES                     TO C26-VALIDAR-PASSWORD
      *    MOVE BE00-USUARIO1              TO C26-USUARIO
      *    MOVE SPACES                     TO C26-RESTO-ENTRADA
      *    MOVE WSC-ASTA                   TO C26-RESTO-ENTRADA(6:4)
      *
      *    EXEC CICS
      *         LINK PROGRAM(WSC-KN1CC260)
      *         COMMAREA    (C26-KNDCC260)
      *         LENGTH      (LENGTH OF REG-C26-KNDCC260)
      *    END-EXEC
      *
      *    IF EIBRESP EQUAL DFHRESP(NORMAL)
      *       IF C26-COD-RESPUESTA EQUAL WSC-RETORNO-OK
      *         CONTINUE
      *       ELSE
      *         MOVE WSC-SIN-ROLLBACK          TO CCIC0-CNYRELCE
      *         MOVE SPACES                    TO CCIC0-ABEND-CODE
      *         MOVE C26-PROGRAMA              TO CCIC0-PROGRAMA-ERR
      *         MOVE C26-REFERENCIA            TO CCIC0-REFERENCIA-ERR
      *         MOVE C26-OBJETO                TO CCIC0-OBJETO-ERR
      *         MOVE C26-SQLCODE               TO CCIC0-SQLCODE-ERR
      *         MOVE C26-SQLERRM               TO CCIC0-SQLERRM-ERR
      *         MOVE C26-EIBFN                 TO CCIC0-EIBFN
      *         MOVE C26-EIBRSRCE              TO CCIC0-EIBRSRCE
      *         MOVE C26-EIBRCODE              TO CCIC0-EIBRCODE
      *         MOVE C26-EIBRESP1              TO CCIC0-EIBRESP1
      *         MOVE C26-EIBRESP2              TO CCIC0-EIBRESP2
      *         MOVE C26-COD-ERROR             TO CCIC0-CNRCAPLN
      *         MOVE C26-VALOR-ERROR-UNO       TO CCIC0-CNCOVAR1
      *         MOVE C26-VALOR-ERROR-DOS       TO CCIC0-CNCOVAR2
      *         PERFORM 900200-FIN-ANORMAL
      *       END-IF
      *    ELSE
      *       MOVE WSC-CON-ROLLBACK            TO CCIC0-CNYRELCE
      *       MOVE C26-KNDCC260                TO CCIC0-REFERENCIA-ERR
      *       MOVE C26-KNDCC260                TO CCIC0-OBJETO-ERR
      *       PERFORM 900100-MOVER-ERRORES-CICS
      *       PERFORM 900200-FIN-ANORMAL
      *    END-IF.
      *
      *----------------------------------------------------------------*
      *  REALIZA CONSULTA SALDO Y MOVIMIENTOS - CONSULTA MOVIMIENTOS   *
      *----------------------------------------------------------------*
       200200-PROCESO-NEGOCIO.
      *----------------------*
           PERFORM FNEG-2000-MOVIMIENTOS-CUENTA

           PERFORM FNEG-2100-COPIA-MOVIMIENTOS
           .
      *
      ******************************************************************
      *                    FNEG-2000-MOVIMIENTOS-CUENTA                *
      ******************************************************************
       FNEG-2000-MOVIMIENTOS-CUENTA.
           PISPLAY 'DIDIER MOVIMIENTO-CUENTAS'
           PISPLAY '-------------------------'
      *----------------------------*
           INITIALIZE W822-DIWC822

           PISPLAY 'DIDIER BE33-BCODCTAA1: ' BE33-BCODCTAA(1:12)
           PISPLAY 'DIDIER BE33-BCODCTAA2: ' BE33-BCODCTAA
           PISPLAY 'DIDIER BE33-BCODCTAA: '  W822-CODIGO
           PISPLAY '-------------------------'
           PISPLAY 'DIDIER DATOS-PAGINACION'
           PISPLAY 'PAGISIZE: ' BE33-BPAGSIZE
           PISPLAY '-------------------------'
           MOVE BE33-BCODCTAA(1:12)             TO W822-CODIGO
           MOVE BE33-BFECHA01(1:4)              TO W822-FECOPE-ANO
           MOVE BE33-BFECHA01(6:2)              TO W822-FECOPE-MES
           MOVE BE33-BFECHA01(9:2)              TO W822-FECOPE-DIA
      *
           IF BE33-BINDPAGI = WSC-S
              INITIALIZE  WSA-PAGINAC-COMP
              MOVE WSC-S                        TO W822-INDIC-PAGINAC
              MOVE BE33-BPAGINAC(1:10)          TO WSN-ARGUMENTO
              MOVE WSN-ARGUMENTO                TO W822-AREA-PAGINAC
           END-IF
      *
           IF BE33-BPAGSIZE NOT EQUAL ZEROES
              MOVE BE33-BPAGSIZE                TO W822-SIZE-PAGINAC
           END-IF
      *
           PISPLAY 'NUEVA W822-NUMBCO-PAGINAC :' W822-NUMBCO-PAGINAC
           PISPLAY 'VARIABLE PARA PAGINACION: '
           PISPLAY 'WSA-PAGINAC-COMP: ' WSA-PAGINAC-COMP
      *
           EXEC CICS
             LINK PROGRAM(WSC-DI6C0822)
PMI          COMMAREA(WS-DIWC822)
PMI   *      COMMAREA(W822-DIWC820)
           END-EXEC
      *
           PISPLAY 'DIDIER SALIDA-RUTINA-DI6C0821: ' W822-CODRETOR
           PISPLAY 'DIDIER EIBRESP: ' EIBRESP
           PISPLAY 'DIDIER RETORNOS:' W822-REFERENC
           PISPLAY 'DIDIER RETORNOS:' W822-DIRCAPLN
           PISPLAY '-----------------'
           IF EIBRESP EQUAL DFHRESP(NORMAL)
              IF W822-CODRETOR EQUAL WSC-RETORNO-OK
                 MOVE 1                         TO WSN-I
              ELSE
                MOVE W822-CODRETOR              TO CAA-VAR2-ERROR
                MOVE W822-REFERENC              TO CAA-VAR1-ERROR
                MOVE W822-DIRCAPLN              TO CAA-COD-ERROR
                MOVE WSC-DI6C0822               TO CAA-OBJETO-ERROR
                PERFORM 900100-MOVER-ERRORES-CICS
                PERFORM 300000-FIN
              END-IF
           ELSE
              MOVE W822-CODRETOR              TO CAA-VAR2-ERROR
              MOVE W822-REFERENC              TO CAA-VAR1-ERROR
              MOVE W822-DIRCAPLN              TO CAA-COD-ERROR
              MOVE WSC-DI6C0822               TO CAA-OBJETO-ERROR
              PERFORM 900100-MOVER-ERRORES-CICS
              PERFORM 300000-FIN
           END-IF
           .
      *
      ******************************************************************
      *                   FNEG-2100-COPIA-MOVIMIENTOS                  *
      ******************************************************************
       FNEG-2100-COPIA-MOVIMIENTOS.
           PISPLAY 'DIDIER COPIA-MOVIMIENTOS'
      *---------------------------*
           MOVE LOW-VALUES                      TO BD3N-ARREGLO-MOVTOS
      *
           IF W822-NUMDOC-TOT  > ZEROS
               PERFORM UNTIL FIN-MOVIMIENTOS
                  IF WSN-I <= 100 AND WSN-I <= W822-OCURRENCIAS
                     AND W822-FECVEN(WSN-I) > ZEROS

                     INITIALIZE BD3N-DIECBD3N  WSA-SALIDA-TS
                     MOVE WSC-DIECBD3N          TO BD3N-FORMATO
                     MOVE W822-FECVEN(WSN-I)    TO WSA-FECHA-8
                     MOVE WSA-FECHA-8(1:4)      TO WSA-FECHA-10(1:4)
                     MOVE WSC-GUION             TO WSA-FECHA-10(5:1)
                     MOVE WSA-FECHA-8(5:2)      TO WSA-FECHA-10(6:2)
                     MOVE WSC-GUION             TO WSA-FECHA-10(8:1)
                     MOVE WSA-FECHA-8(7:2)      TO WSA-FECHA-10(9:2)
                     MOVE WSA-FECHA-10          TO BD3N-FECHA-VENCIM
                     MOVE W822-NUMBCO(WSN-I)    TO BD3N-NUM-DOCU
                     MOVE W822-NUMBCOORI(WSN-I) TO BD3N-NUM-ORIGINAL
                     MOVE W822-NOMACE(WSN-I)    TO BD3N-NOM-ACEPTA
                     MOVE W822-INGRESO(WSN-I)   TO BD3N-INGRESO
                     MOVE W822-IMPORTE(WSN-I)   TO BD3N-IMPORTE
                     MOVE W822-INTERES(WSN-I)   TO BD3N-INTERES
                     MOVE W822-COMPORT(WSN-I)   TO BD3N-COMISIO
                     MOVE W822-GASTOS(WSN-I)    TO BD3N-GASTOS
PMI                  MOVE SPACES  TO BD3N-SITUACION
PMI                  EVALUATE W822-SITUAC(WSN-I)
PMI                    WHEN 'CAN' MOVE '1'        TO BD3N-SITUACION(1:1)
PMI                    WHEN 'REN' MOVE '2'        TO BD3N-SITUACION(1:1)
PMI                    WHEN 'RRI' MOVE '3'        TO BD3N-SITUACION(1:1)
PMI                    WHEN 'DEV' MOVE '4'        TO BD3N-SITUACION(1:1)
PMI                    WHEN 'DVP' MOVE '5'        TO BD3N-SITUACION(1:1)
PMI                    WHEN 'DEE' MOVE '6'        TO BD3N-SITUACION(1:1)
PMI                    WHEN 'ING' MOVE '7'        TO BD3N-SITUACION(1:1)
PMI                    WHEN 'INE' MOVE '8'        TO BD3N-SITUACION(1:1)
PMI                  END-EVALUATE
PMI                  MOVE W822-TIPDOC(WSN-I)      TO BD3N-SITUACION(2:2)
                     MOVE BD3N-DIECBD3N           TO WSA-SALIDA-TS
                     MOVE LENGTH OF BD3N-DIECBD3N TO WSN-TSLENGTH
                     PERFORM 200220-GRABA-COLA-SALIDA
                     ADD 1                      TO WSN-I
                  ELSE
                     SET FIN-MOVIMIENTOS        TO TRUE
                  END-IF
               END-PERFORM
           END-IF

      *MOVER CABECERA DE LA SALIDA
           INITIALIZE BS3N-DIECBS3N  WSA-SALIDA-TS

           MOVE WSC-DIECBS3N                    TO BS3N-FORMATO
           MOVE W822-CODIGO-TIT                 TO BS3N-COD-CARTERA
           MOVE W822-SERVICIO-TIT               TO BS3N-SERVICIO
           MOVE W822-CTAABO                     TO BS3N-CTA-ABONO
           MOVE W822-CTAABO-MON                 TO BS3N-DIV-ABONO
           MOVE W822-CTACAR                     TO BS3N-CTA-CARGO
           MOVE W822-CTACAR-MON                 TO BS3N-DIV-CARGO
           MOVE W822-INGRESO-TIT                TO BS3N-TOT-ABONO
           MOVE W822-IMPORTE-TIT                TO BS3N-TOT-CARGO
           MOVE W822-FECOPE-TOT(1:4)            TO BS3N-FECHA-OPE(1:4)
           MOVE WSC-GUION                       TO BS3N-FECHA-OPE(5:1)
           MOVE W822-FECOPE-TOT(5:2)            TO BS3N-FECHA-OPE(6:2)
           MOVE WSC-GUION                       TO BS3N-FECHA-OPE(8:1)
           MOVE W822-FECOPE-TOT(7:2)            TO BS3N-FECHA-OPE(9:2)
           MOVE W822-NUMDOC-TOT                 TO BS3N-NUM-DOCS
           MOVE W822-IMPORTE-TOT                TO BS3N-IMPORTE-TOT
           MOVE W822-INTERES-TOT                TO BS3N-INTERES-TOT
           MOVE W822-COMPORT-TOT                TO BS3N-COMISION-TOT
           MOVE W822-GASTOS-TOT                 TO BS3N-GASTO-TOT

           IF W822-INDIC-PAGINAC = WSC-S
              MOVE W822-INDIC-PAGINAC         TO BS3N-INDICADOR-PAGINACI
              MOVE W822-NUMBCO-PAGINAC        TO BS3N-ARGUMENTO-BUSQUEDA
           ELSE
              MOVE WSC-N                      TO BS3N-INDICADOR-PAGINACI
              MOVE SPACES                     TO BS3N-ARGUMENTO-BUSQUEDA
           END-IF

      *    MOVE 'BS3N'                          TO CCIC0-ID-FORMATO01
      *    MOVE 1                               TO CCIC0-NITEM-FORMATO01
      *    MOVE LENGTH OF BS3N-DIECBS3N         TO CCIC0-LGTH-FORMATO01

           MOVE WSC-DIECBS3N                    TO BS3N-FORMATO
           MOVE BS3N-DIECBS3N                   TO WSA-SALIDA-TS
           MOVE LENGTH OF BS3N-DIECBS3N         TO WSN-TSLENGTH
           PERFORM 200220-GRABA-COLA-SALIDA
           .
      *
      *----------------------------------------------------------------*
      *    GRABA LA COLA DE SALIDA CON LA INFORMACION DEL RESULTADO    *
      *    DE LA FUNCION                                               *
      *----------------------------------------------------------------*
       200220-GRABA-COLA-SALIDA.
           PISPLAY 'DIDIER-GRABA-COLA-SALIDA'
      *------------------------*
           MOVE CAA-IDIOMA-TERM                 TO CAA-IDIOMA(1)
           MOVE WSC-FUNCION                     TO CAA-DESTINO(1)
           MOVE WSC-P                           TO CAA-IND-PANDOC(1)
      *
           MOVE '+DC1'                          TO WSA-TS-NOMBRE
           MOVE CAA-TERMINAL                    TO WSA-TS-SUFIJO
      *
           EXEC CICS
                WRITEQ TS QUEUE(WSA-TS)
                FROM(WSA-SALIDA-TS)
                LENGTH(WSN-TSLENGTH)
                MAIN NOHANDLE
           END-EXEC
      *
           IF EIBRESP EQUAL DFHRESP(NORMAL)
              CONTINUE
           ELSE
              MOVE WSA-SALIDA-TS               TO CAA-OBJETO-ERROR
              MOVE 'ERROR CICS  WRITEQ'        TO ABC-REFERENCIA
              PERFORM 900100-MOVER-ERRORES-CICS
              PERFORM 300000-FIN
           END-IF.

      *----------------------------------------------------------------*
      *    SE TERMINA EL PROCESO DE LA FUNCION GENERANDO EL REGISTRO   *
      *    DE LA TRANSACCION EN EL LOG DE OPERACIONES DE AST USANDO    *
      *    EL COLECTOR (C30).                                          *
      *----------------------------------------------------------------*
      *200400-FIN-PROCESO-AST.
      *----------------------*
      *    SET  C30-PTR-COPYIN                 TO CCIC0-PTR-APL-AST
      *    MOVE CCIC0-CNYTACT1                 TO C30-PARAM-LOG
      *    MOVE CCIC0-CNYTACT2                 TO C30-PARAM-MON
      *    MOVE SPACES                         TO C30-DIVISA
      *    MOVE CCIC0-CNRCAPLN                 TO C30-COD-ERROR-E
      *    MOVE CCIC0-CNCOVAR1                 TO C30-VALOR-UNO-E
      *    MOVE CCIC0-CNCOVAR2                 TO C30-VALOR-DOS-E
      *    MOVE SPACES                         TO C30-REF-INTERNA
      *    MOVE SPACES                         TO C30-REF-EXTERNA
      *    MOVE SPACES                         TO C30-TEXTO-LIBRE
      *    MOVE CCIC0-CNFTRANS                 TO C30-FECHA-TRANS
      *    MOVE CCIC0-CNHTRANS                 TO C30-HORA-TRANS
      *    MOVE CCIC0-CNTOPERA                 TO C30-TIPO-OPERA
      *    MOVE CCIC0-CNSOPERA                 TO C30-SUB-OPERA
      *    MOVE CCIC0-CNVERTRA                 TO C30-VERSION-TRAN
      *    MOVE CCIC0-CNCNLLLA                 TO C30-CANAL-LLAMANTE
      *    MOVE CCIC0-CNMEDACC                 TO C30-MEDIO-ACCESO
      *    MOVE CCIC0-CNCODSPR                 TO C30-COD-SERVICIO
      *    MOVE CCIC0-CNTIDCLI                 TO C30-TIPO-IDENT
      *    MOVE CCIC0-CNDOCIDC                 TO C30-DOC-IDENT
      *    MOVE CCIC0-CNYREVER                 TO C30-IND-REVERSO
      *
      *    EXEC CICS
      *       LINK PROGRAM(WSC-KN1CC300)
      *       COMMAREA    (REG-C30-KNDCC300)
      *       LENGTH      (LENGTH OF REG-C30-KNDCC300)
      *    END-EXEC
      *
      *    IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *
      *       MOVE WSC-CON-ROLLBACK            TO CCIC0-CNYRELCE
      *       MOVE C30-KNDCC300                TO CCIC0-REFERENCIA-ERR
      *       MOVE WSC-KN1CC300                TO CCIC0-OBJETO-ERR
      *       PERFORM 900100-MOVER-ERRORES-CICS
      *       PERFORM 900200-FIN-ANORMAL
      *    ELSE
      *       IF C30-COD-RESPUESTA EQUAL WSC-RETORNO-OK
      *         CONTINUE
      *       ELSE
      *          MOVE WSC-CON-ROLLBACK         TO CCIC0-CNYRELCE
      *          MOVE SPACES                   TO CCIC0-ABEND-CODE
      *          MOVE C30-PROGRAMA             TO CCIC0-PROGRAMA-ERR
      *          MOVE C30-REFERENCIA           TO CCIC0-REFERENCIA-ERR
      *          MOVE C30-OBJETO               TO CCIC0-OBJETO-ERR
      *          MOVE C30-SQLCODE              TO CCIC0-SQLCODE-ERR
      *          MOVE C30-SQLERRM              TO CCIC0-SQLERRM-ERR
      *          MOVE C30-EIBFN                TO CCIC0-EIBFN
      *          MOVE C30-EIBRSRCE             TO CCIC0-EIBRSRCE
      *          MOVE C30-EIBRCODE             TO CCIC0-EIBRCODE
      *          MOVE C30-EIBRESP1             TO CCIC0-EIBRESP1
      *          MOVE C30-EIBRESP2             TO CCIC0-EIBRESP2
      *          MOVE C30-COD-ERROR            TO CCIC0-CNRCAPLN
      *          MOVE C30-VALOR-ERROR-UNO      TO CCIC0-CNCOVAR1
      *          MOVE C30-VALOR-ERROR-DOS      TO CCIC0-CNCOVAR2
      *       END-IF
      *    END-IF.
      *
      *----------------------------------------------------------------*
      *    MOVIMIENTO DE ERRORES DE CICS                               *
      *----------------------------------------------------------------*
       900100-MOVER-ERRORES-CICS.
           PISPLAY 'DIDIER MOVER-ERRORES-CICS'
      *-------------------------*
           MOVE 'S'                         TO ABC-ABEND
           MOVE WSC-DI2CDIR3                TO ABC-PROGRAMA
           MOVE EIBFN                       TO ABC-EIBFN
           MOVE EIBRSRCE                    TO ABC-EIBRSRCE
           MOVE EIBRCODE                    TO ABC-EIBRCODE
           MOVE EIBRESP                     TO ABC-EIBRESP1
           MOVE EIBRESP2                    TO ABC-EIBRESP2
           .
      *

      ******************************************************************
      *                                                                *
      *                    4200-OBTENER-FECHA-CICS                     *
      *                                                                *
      ******************************************************************
      *4200-OBTENER-FECHA-CICS.
      *-----------------------*
      *    EXEC CICS
      *         ASKTIME ABSTIME(WSN-ABSTIME)
      *    END-EXEC.
      *
      *    IF EIBRESP NOT = DFHRESP(NORMAL)
      *       MOVE WSC-SIN-ROLLBACK    TO CCIC0-CNYRELCE
      *       MOVE 'ASKTIME'           TO CCIC0-REFERENCIA-ERR
      *       MOVE WSC-PROGRAMA        TO CCIC0-OBJETO-ERR
      *       PERFORM 900100-MOVER-ERRORES-CICS
      *       PERFORM 900200-FIN-ANORMAL
      *    END-IF
      *
      *    EXEC CICS
      *         FORMATTIME
      *         ABSTIME (WSN-ABSTIME)
      *         YYYYMMDD(WSA-FECHA-AAAAMMDD)
      *         DATESEP (WSC-GUION)
      *         TIME    (WSA-TIME-HHMMSS)
      *         TIMESEP
      *    END-EXEC
      *
      *    IF EIBRESP NOT = DFHRESP(NORMAL)
      *       MOVE WSC-SIN-ROLLBACK     TO CCIC0-CNYRELCE
      *       MOVE 'FORMATTIME'         TO CCIC0-REFERENCIA-ERR
      *       MOVE WSC-PROGRAMA         TO CCIC0-OBJETO-ERR
      *       PERFORM 900100-MOVER-ERRORES-CICS
      *       PERFORM 900200-FIN-ANORMAL
      *    END-IF.
      *----------------------------------------------------------------*
      *    TERMINO DE LA FUNCION                                       *
      *----------------------------------------------------------------*
       300000-FIN.
      *------------------*
           PISPLAY 'DIDIER FIN---'
           PISPLAY 'CAA-COD-ERROR-FIN: ' CAA-COD-ERROR
      *
           IF CAA-COD-ERROR EQUAL SPACES
              EXEC CICS
                   SYNCPOINT
              END-EXEC
           ELSE
              PERFORM 100100-BORRAR-COLAS-TS
           END-IF
      *
           EXEC CICS
              RETURN
           END-EXEC
           .
      *----------------------------------------------------------------*
