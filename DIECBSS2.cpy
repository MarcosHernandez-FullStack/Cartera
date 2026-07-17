      ******************************************************************
      *                                                                *
      * TRX     :DIR2 CARTERA - RELACION DE OPERACIONES                *
      * DIECBSS2:FORMATO DE SALIDA                                     *
      *                                                                *
      *     CAMPO____LITERAL______________DELIM_ATT__LNG__             *
      *  01 CODCART  COD. CARTERA          01    A   12                *
      *  02 SERV     SERVICIO              02    A   20                *
      *  03 DIVISA   DIVISA                03    A    8                *
      *  04 INDPAGI  INDICADOR PAGINACION  04    A    1                *
      *  05 PAGINAC  CURSOR DE PAGINACION  05    A   50                *
      ******************************************************************
       01 REG-DIECBSS2.
           05 BSS2-CODCARTERA       PIC X(12).
           05 BSS2-SERVICIO         PIC X(20).
           05 BSS2-DIVISA           PIC X(08).
           05 BSS2-INDPAGI          PIC X(01).
           05 BSS2-PAGINAC          PIC X(50).
      ******************************************************************
      *                                                                *
      *         LONGITUD TOTAL DEL REGISTRO EN BYTES :   91            *
      *                                                                *
      ******************************************************************

