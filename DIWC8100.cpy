      *________________________________________________________________*00010000
      *    AREA DE COMUNICACION CON MODULO INTERFASE DE CARTERA        *00020000
      *________________________________________________________________*00030000
      *    DIWC8100 AREA DE ENLACE CON EL MODULO DE CONTINET           *00040000
      *             DIETARIO DE CARTERA - COBRANZAS                    *00050001
      *             LONGITUD: 4957                                     *00060010
      *             EL NUMERO DE ITEMS (W8100-DETALLE) CON INFORMACION *00061004
      *             ESTA EN : W8100-N                                  *00062004
      *________________________________________________________________*00070000
      *                                                                *00080001
      * W8100-DIRCAPLN W8100-CODRETOR W8100-REFERENC                   *00090009
      * _____________ _____________ _____________                      *00100009
      *               00                                               *00110011
      * DIE8108       08            ERROR CODIGO CARTERA NO NUMERICO   *00111009
      * DIE8109       09            ERROR CODIGO CARTERA NO VALIDO     *00112009
      * DIE8191       91            ERROR DIDIEDES ENDBR               *00120009
      * DIE8196       96            ERROR DIDIEDES READNEXT            *00130009
      * DIE8197       97            ERROR DIDIEDES CODIGO CARTERA NO EX*00140009
      * DIE8198       98            ERROR DIDIEDES CERRADO             *00150009
      * DIE8199       99            ERROR DIDIEDES STARTBR             *00160009
      *________________________________________________________________*00180001
           02 W8100-DIWC810.                                            00190000
              05 W8100-ENTRADA.                                         00200000
                 10 W8100-CODIGO.                                       00210000
                    20 W8100-CODOFI                 PIC 9(03).          00220000
                    20 W8100-CODMON                 PIC 9(01).          00230000
                    20 W8100-CODCLI                 PIC 9(06).          00240000
                    20 W8100-CODSER                 PIC 9(02).          00250000
      *__________________________________________________12 BYTES_____  00260000
              05 W8100-RETORNO.                                         00270000
                 10 W8100-CODRETOR                  PIC X(02).          00280000
                 10 W8100-REFERENC                  PIC X(30).          00290000
                 10 W8100-DIRCAPLN                  PIC X(07).          00291010
      *__________________________________________________39 BYTES_____  00300010
              05 W8100-SALIDA.                                          00310000
                 10 W8100-TITULO.                                       00320000
                    20 W8100-CODIGO-TIT             PIC 9(12).          00330005
                    20 W8100-SERVICIO-TIT           PIC X(20).          00340000
                    20 W8100-MONEDA-TIT             PIC X(08).          00350000
                    20 W8100-N                      PIC 9(05).          00360000
      *__________________________________________________45 BYTES_____  00370005
                 10 W8100-DETALLE                   OCCURS 200 TIMES.   00380000
                    20 W8100-FECOPE                 PIC 9(08).          00390000
                    20 W8100-ABONOS                 PIC 9(13)V99 COMP-3.00391008
                    20 W8100-CARGOS                 PIC 9(13)V99 COMP-3.00400000
      *__________________________________________________61 BYTES_____  00420000
              05 W8100-DATOS-PAGINAC.
                 10 W8100-INDPAGI                   PIC X(01).
                 10 W8100-PAGINAC                   PIC X(50).
                 10 W8100-PAGSIZE                   PIC 9(10).

      *________________________________________________4957 BYTES_____  00430010
