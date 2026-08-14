      *________________________________________________________________*00010000
      *    AREA DE COMUNICACION CON MODULO INTERFASE DE CARTERA        *00020000
      *________________________________________________________________*00030000
      *    DIWC822 AREA DE ENLACE CON EL MODULO DE CONTINET            *00040000
      *            DIETARIO DE CARTERA - COBRANZAS                     *00050000
      *            LONGITUD: 11330                                     *00060000
      *                                                                *00070000
      *          . EL NUMERO TOTAL DE LINEAS DE DETALLE (W822-DETALLE) *00080000
      *            ESTA EN : W822-NUMDOC-TOT                           *00090000
      *                                                                *00091000
      *          . W822-SITUAC = 'CAN' -->  CANCELADO                  *00100000
      *            W822-SITUAC = 'REN' -->  RENOVADO                   *00110000
      *            W822-SITUAC = 'RRI' -->  RENOVADO REINGRESO         *00120000
      *            W822-SITUAC = 'DEV' -->  DEVUELTO                   *00130000
      *            W822-SITUAC = 'DVP' -->  DEVUELTO PROTESTADO        *00140000
      *            W822-SITUAC = 'DEE' -->  DESCARGO POR ERROR         *00150000
      *            W822-SITUAC = 'ING' -->  INGRESO                    *00160000
      *            W822-SITUAC = 'INE' -->  INGRESO  POR ERROR         *00170000
      *                                                                *00171000
      *          . W822-INDIC-PAGINAC = 'S' SI HAY MAS DATOS (OUTPUT)  *00180000
      *            W822-INDIC-PAGINAC = 'N' NO HAY MAS DATOS (OUTPUT)  *00190000
      *            W822-INDIC-PAGINAC = ' ' PRIMERA PAGINA   (INPUT)   *00192000
      *            W822-INDIC-PAGINAC = 'S' SI PAGINACION    (INPUT)   *00193000
      *                                                                *00194000
      *          . W822-NUMBCO-PAGINAC=     NUMBCO PUNTERO   (OUTPUT)  *00195000
      *            W822-NUMBCO-PAGINAC=     NUMBCO PUNTERO   (INPUT)   *00196000
      *________________________________________________________________*00200000
      * W822-DIRCAPLN W822-CODRETOR W822-REFERENC                      *00210000
      * _____________ _____________ _____________                      *00211000
      *               00                                               *00240000
      * DIE8208       08            ERROR CODIGO CARTERA  NO NUMERICO  *00250000
      * DIE8209       09            ERROR FECHA OPERACION NO NUMERICO  *00260000
      * DIE8210       10            ERROR INDICADOR PAGINAC  INVALIDO  *00261000
      * DIE8212       12            ERROR NUMBCO PAGINAC     INVALIDO  *00262000
      * DIE8280       80            ERROR RUTINA OBTIENE DIVISA        *00270000
      * DIE8281       81            ERROR CUENTA ALTAMIRA NO EXISTE    *00280000
      * DIE8291       91            ERROR DIDIECOB ENDBR               *00290000
      * DIE8296       96            ERROR DIDIECOB READNEXT            *00300000
      * DIE8297       97            ERROR DIDIECOB CLAVE  NO EXISTE    *00310000
      * DIE8298       98            ERROR DIDIECOB CERRADO             *00320000
      * DIE8299       99            ERROR DIDIECOB STARTBR             *00330000
      *________________________________________________________________*00350000
           02 W822-DIWC822.                                             00360000
              05 W822-ENTRADA.                                          00370000
                 10 W822-COD-FECOPE.                                    00380000
                    20 W822-CODIGO.                                     00390000
                       40 W822-CODOFI              PIC 9(03).           00400000
                       40 W822-CODMON              PIC 9(01).           00410000
                       40 W822-CODCLI              PIC 9(06).           00420000
                       40 W822-CODSER              PIC 9(02).           00430000
                    20 W822-FECOPE.                                     00440000
                       40 W822-FECOPE-ANO          PIC 9(04).           00450000
                       40 W822-FECOPE-MES          PIC 9(02).           00460000
                       40 W822-FECOPE-DIA          PIC 9(02).           00470000
      *__________________________________________________20 BYTES_____  00480000
              05 W822-RETORNO.                                          00490000
                 10 W822-CODRETOR                  PIC X(02).           00500000
                 10 W822-REFERENC                  PIC X(30).           00510000
                 10 W822-DIRCAPLN                  PIC X(07).           00511000
      *__________________________________________________39 BYTES_____  00520000
              05 W822-PAGINAC.                                          00530000
                 10 W822-OCURRENCIAS               PIC 9(03).           00540000
                 10 W822-INDIC-PAGINAC             PIC X(01).           00550000
                 10 W822-SIZE-PAGINAC              PIC 9(10).           00550100
                 10 W822-AREA-PAGINAC.                                  00560000
                    20 W822-NUMBCO-PAGINAC         PIC 9(10).           00581000
      *__________________________________________________14 BYTES_____  00610000
              05 W822-SALIDA.                                           00620000
                 10 W822-TITULO.                                        00630000
                    20 W822-CODIGO-TIT             PIC 9(12).           00640000
                    20 W822-SERVICIO-TIT           PIC X(20).           00650000
                    20 W822-CTAABO                 PIC X(20).           00660000
                    20 W822-CTAABO-MON             PIC X(12).           00670000
                    20 W822-CTACAR                 PIC X(20).           00680000
                    20 W822-CTACAR-MON             PIC X(12).           00690000
                    20 W822-INGRESO-TIT            PIC 9(13)V99 COMP-3. 00700000
                    20 W822-IMPORTE-TIT            PIC 9(13)V99 COMP-3. 00710000
      *_________________________________________________112 BYTES_____  00720000
                 10 W822-TOTALES.                                       00730000
                    20 W822-FECOPE-TOT             PIC 9(08).           00740000
                    20 W822-NUMDOC-TOT             PIC 9(05).           00750000
                    20 W822-IMPORTE-TOT            PIC 9(13)V99 COMP-3. 00760000
                    20 W822-INTERES-TOT            PIC 9(13)V99 COMP-3. 00770000
                    20 W822-COMPORT-TOT            PIC 9(13)V99 COMP-3. 00780000
                    20 W822-GASTOS-TOT             PIC 9(13)V99 COMP-3. 00790000
      *__________________________________________________45 BYTES_____  00800000
                 10 W822-DETALLE                   OCCURS 100 TIMES.    00810000
                    20 W822-FECVEN                 PIC 9(08).           00820000
                    20 W822-NUMBCO                 PIC 9(10).           00830000
PMI   *             20 W822-NUMBCOORI              PIC 9(10).           00831000
PMI                 20 W822-NUMBCOORI              PIC X(12).           00832000
                    20 W822-NOMACE                 PIC X(40).           00840000
                    20 W822-INGRESO                PIC 9(13)V99 COMP-3. 00850000
                    20 W822-IMPORTE                PIC 9(13)V99 COMP-3. 00860000
                    20 W822-INTERES                PIC 9(13)V99 COMP-3. 00870000
                    20 W822-COMPORT                PIC 9(13)V99 COMP-3. 00880000
                    20 W822-GASTOS                 PIC 9(13)V99 COMP-3. 00890000
                    20 W822-SITUAC                 PIC X(03).           00900000
      *_________________________________________________111 BYTES_____  00910000
      *_______________________________________________11330 BYTES_____  00920000
