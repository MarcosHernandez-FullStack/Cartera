      *________________________________________________________________*00010000
      *    AREA DE COMUNICACION CON MODULO INTERFASE DE CARTERA        *00020000
      *________________________________________________________________*00030000
      *    DIWC9100 AREA DE ENLACE CON EL MODULO DE CONTINET           *00040000
      *             DIETARIO DE CARTERA - DESCUENTOS                   *00050001
      *             LONGITUD: 4957                                     *00060007
      *             EL NUMERO DE ITEMS (W9100-DETALLE) CON INFORMACION *00061002
      *             ESTA EN : W9100-N                                  *00062002
      *________________________________________________________________*00070000
      *                                                                *00080000
      * W9100-DIRCAPLN W9100-CODRETOR W9100-REFERENC                   *00090007
      * _____________ _____________ _____________                      *00100007
      *               00                                               *00110010
      * DIE9108       08            ERROR CODIGO CARTERA NO NUMERICO   *00111007
      * DIE9109       09            ERROR CODIGO CARTERA NO VALIDO     *00112007
      * DIE9191       91            ERROR DIDIEDES ENDBR               *00120007
      * DIE9196       96            ERROR DIDIEDES READNEXT            *00130007
      * DIE9197       97            ERROR DIDIEDES CODIGO CARTERA NO EX*00140007
      * DIE9198       98            ERROR DIDIEDES CERRADO             *00150007
      * DIE9199       99            ERROR DIDIEDES STARTBR             *00160007
      *________________________________________________________________*00180000
           02 W9100-DIWC9100.                                           00190000
              05 W9100-ENTRADA.                                         00200000
                 10 W9100-CODIGO.                                       00210000
                    20 W9100-CODOFI                 PIC 9(03).          00220000
                    20 W9100-CODMON                 PIC 9(01).          00230000
                    20 W9100-CODCLI                 PIC 9(06).          00240000
                    20 W9100-CODSER                 PIC 9(02).          00250000
      *__________________________________________________12 BYTES_____  00260000
              05 W9100-RETORNO.                                         00270000
                 10 W9100-CODRETOR                  PIC X(02).          00280000
                 10 W9100-REFERENC                  PIC X(30).          00290000
                 10 W9100-DIRCAPLN                  PIC X(07).          00291007
      *__________________________________________________39 BYTES_____  00300009
              05 W9100-SALIDA.                                          00310000
                 10 W9100-TITULO.                                       00320000
                    20 W9100-CODIGO-TIT             PIC 9(12).          00330003
                    20 W9100-SERVICIO-TIT           PIC X(20).          00340000
                    20 W9100-MONEDA-TIT             PIC X(08).          00350000
                    20 W9100-N                      PIC 9(05).          00360000
      *__________________________________________________45 BYTES_____  00370003
                 10 W9100-DETALLE                   OCCURS 200 TIMES.   00380000
                    20 W9100-FECOPE                 PIC 9(08).          00390000
                    20 W9100-ABONOS                 PIC 9(13)V99 COMP-3.00391005
                    20 W9100-CARGOS                 PIC 9(13)V99 COMP-3.00400000
      *__________________________________________________61 BYTES_____  00420000
              05 W9100-PAGINAC.
                 10 W8100-INDPAGI                   PIC X(01).
                 10 W8100-PAGINAC                   PIC X(50).
                 10 W8100-PAGSIZE                   PIC 9(10).
      *________________________________________________4957 BYTES_____  00430007
