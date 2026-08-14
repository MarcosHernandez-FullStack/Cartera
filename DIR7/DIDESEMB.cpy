      *===============  DESEMBOLSO DE CARTERA  ==================*      00000010
      *  NOMBRE DEL BOOK      : STSCDCAR                         *      00000020
      *  LONGITUD DE REGISTRO : 180                              *      00000030
      *  ORGANIZACION         : INDEXADA                         *      00000040
      *  CLAVE DEL ARCHIVO    : DCAR-KEYDES                      *      00000050
      *  NOMBRE FISICO        : PLDIQ.DM.DIDESEMB                *      00000060
      *  NOMBRE CICS          : DIDESEMB                         *      00000070
      *  CODIGO PIDA          : 92                               *      00000080
      *==========================================================*      00000090
      *  DCAR-OFIREG : OFICINA DE DESEMBOLSO/REGISTRO            *      00000100
      *  DCAR-DIAJUL : DIA JULIANO DEL DESEMBOLSO/REGISTRO       *      00000110
      *  DCAR-NROCON : NUMERO CONSECUTIVO DE OFICINA EN EL DIA   *      00000120
      *  DCAR-DIGCHE : DIGITO DE CHEQUEO DE LA PLANILLA          *      00000130
      *  DCAR-FECREG : FECHA DEL DESEMBOLSO/REGISTRO             *      00000140
      *  DCAR-CODIGO : CODIGO                                    *      00000150
      *  DCAR-CODOFI : CODIGO DE OFICINA                         *      00000160
      *  DCAR-CODMON : CODIGO DE MONEDA                          *      00000170
      *  DCAR-CODCLI : RESTO DEL CODIGO                          *      00000180
      *  DCAR-CODSER : CODIGO DE SERVICIO DE CARTERA             *      00000190
      *  DCAR-OFIPRO : OFICINA PROPIETARIA DE LOS DOCUMENTOS     *      00000200
      *  DCAR-IMPORT : IMPORTE DEL DESEMBOLSO/REGISTRO           *      00000210
      *  DCAR-MONEDA : MONEDA DE LOS DOCUMENTOS DE LA PLANILLA   *      00000220
      *  DCAR-CTACTE : CUENTA DONDE SE ABONA DESEMBOLSO          *      00000230
      *  DCAR-NRDOCR : NRO DE DOCUMENTOS REGISTRADOS DE PLANILLA *      00000240
      *  DCAR-NRDOCI : NRO DE DOCUMENTOS INGRESADOS DE PLANILLA  *      00000250
      *  DCAR-CODCAJ : CODIGO DEL CAJERO                         *      00000260
      *  DCAR-TASINT : TASA DE INTERES                           *      00000270
      *  DCAR-FECING : FECHA DE INGRESO REGULARIZACION           *      00000280
      *  DCAR-IMPING : IMPORTE INGRESADO/REGULARIZADO            *      00000290
      *  DCAR-IMPABO : ABONO POR DIFERENCIA A LA OFICINA         *      00000300
      *  DCAR-IMPCAR : CARGO POR DIFERENCIA A LA OFICINA         *      00000310
      *  DCAR-IMPDVL : IMPORTE POR DEVOLUCION A CLIENTE EN LINEA *      00000320
      *  DCAR-IMPDVB : IMPORTE POR DEVOLUCION A CLIENTE EN BATCH *      00000330
      *  DCAR-TOTDIA-S TOPE POR OFICINA SOLES                    *      00000330
      *  DCAR-TOTDIA-D TOPE POR OFICINA DOLARES                  *      00000330
      *  DCAR-NRDEVL : NRO DE DCTOS. DEVUELTOS                   *      00000350
      *  DCAR-NRDEVB : NRO DE DCTOS. DEVUELTOS                   *      00000350
      *  DCAR-CAJAUT : CAJERO QUE DESEMBOLSA                     *      00000350
      *  DCAR-PTRJRN : PUNTERO DEL JOURNAL DEL REGISTRO          *      00000350
      *  DCAR-CLASIF : CLASIFICACION                             *      00000350
      *  DCAR-CAJVAL : CAJERO QUE AUTORIZA DESEMBOLSO            *      00000350
      *  FILLER      : EN BLANCO                                 *      00000350
      *  DCAR-SITUAC : SITUACION DE LA PLANILLA                  *      00000340
      *                0. PLANILLA REGISTRADA                    *      00000340
      *                1. EN CTA. TRAMITE DE INGRESO O DESEMBOL. *      00000340
      *                2. EN CTA. DCTOS. EN PROCESO DE REGULARIZ.*      00000340
      *                3. EN CTA. OPERAC. POR LIQUIDAR OFICINAS  *      00000340
      *                5. REGUL. DE LA CTA. TRAMITE DE INGRESO   *      00000340
      *                6. REGUL. DE LA CTA. DCTOS. EN PROCESO DE *      00000340
      *                   REGULARIZACION                         *      00000340
      *                7. REGUL. DE LA CTA. OPERACIONES POR      *      00000340
      *                   LIQUIDAR OFICINAS                      *      00000340
      *                8. PLANILLA AUTORIZADA                    *      00000340
      *                9. EXTORNADAS                             *      00000340
      *==========================================================*      00000360
       01  REG-DEVOLUCION-CARTERA.                                      00000370
           02 DCAR-KEYDES.                                              00000380
              03 DCAR-CODPLA.                                           00000390
                 04 DCAR-OFIING        PIC 999.                         00000400
                 04 DCAR-DIAJUL        PIC 999.                         00000410
                 04 DCAR-NROCON        PIC 999.                         00000420
                 04 DCAR-DIGCHE        PIC 9.                           00000430
           02 DCAR-FECDES.                                              00000440
              03 DCAR-DESANO           PIC 9(4).                        00000450
              03 DCAR-DESMES           PIC 99.                          00000460
              03 DCAR-DESDIA           PIC 99.                          00000470
           02 DCAR-FILLE1              PIC X(08).                       00000480
           02 DCAR-CODAPL.                                              00000490
              03 DCAR-CODIGO.                                           00000500
                 04 DCAR-CODOFI        PIC 9(03).                       00000510
                 04 DCAR-CODMON        PIC 9(01).                       00000520
                 04 DCAR-CODCLI        PIC 9(06).                       00000530
              03 DCAR-CODSER           PIC 9(02).                       00000540
           02 DCAR-OFIPRO              PIC 9(3).                        00000550
           02 DCAR-INDDES              PIC X(1).                        00000560
           02 DCAR-IMPORT              PIC 9(13)V99  COMP-3.            00000570
           02 DCAR-MONEDA              PIC 9.                           00000580
           02 DCAR-FILLE2              PIC X(10).                       00000590
           02 DCAR-CTACTE              PIC 9(10).                       00000600
           02 DCAR-NRDOCR              PIC 9(5)      COMP-3.            00000610
           02 DCAR-NRDOCI              PIC 9(5)      COMP-3.            00000620
           02 DCAR-CODCAJ              PIC 9(5)      COMP-3.            00000630
           02 DCAR-TASINT              PIC 9(4)V9(3) COMP-3.            00000640
           02 DCAR-FECING              PIC 9(8).                        00000650
           02 DCAR-IMPORTES.
              03 DCAR-IMPING           PIC 9(13)V99  COMP-3.            00000660
              03 DCAR-IMPABO           PIC 9(13)V99  COMP-3.            00000670
              03 DCAR-IMPCAR           PIC 9(13)V99  COMP-3.            00000680
              03 DCAR-IMPDVL           PIC 9(13)V99  COMP-3.            00000690
              03 DCAR-IMPDVB           PIC 9(13)V99  COMP-3.            00000700
           02 DCAR-TOPES-OFICINA       REDEFINES DCAR-IMPORTES.
              03 DCAR-TOTDIA-S         PIC 9(13)V99  COMP-3.            00000660
              03 DCAR-TOTDIA-D         PIC 9(13)V99  COMP-3.            00000670
              03 FILLER                PIC 9(13)V99  COMP-3.            00000680
              03 FILLER                PIC 9(13)V99  COMP-3.            00000690
              03 FILLER                PIC 9(13)V99  COMP-3.            00000700
           02 DCAR-NRDEVL              PIC 9(5)      COMP-3.            00000710
           02 DCAR-NRDEVB              PIC 9(5)      COMP-3.            00000720
           02 DCAR-CAJAUT              PIC 9(05)     COMP-3.            00000720
           02 DCAR-PTRJRN              PIC 9(06).                       00000720
           02 DCAR-CLASIF              PIC X(02).                       00000720
           02 DCAR-CAJVAL              PIC 9(05)     COMP-3.            00000720
      *--200210108-INI
           02 DCAR-NRORET              PIC 9(06).
           02 DCAR-INTRET              PIC 9(11)V99  COMP-3.
           02 DCAR-LEVRET              PIC X(01).
      *--SDA37011-INI
      *    02 FILLER                   PIC X(13).
           02 DCAR-ICOTIZA             PIC X(12).
INDLIN     02 FILLER                   PIC X(01).
      *--SDA37011-FIN
      *    02 FILLER                   PIC X(27).
      *--200210108-FIN
           02 DCAR-SITUAC              PIC 9.                           00000740
