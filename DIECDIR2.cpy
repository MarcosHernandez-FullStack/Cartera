      ******************************************************************
      *                                                                *
      * TRX     :DIR2 CARTERA - SALDOS/MVTOS                           *
      * DIECDIR2:FORMATO DE ENTRADA
      *                                                                *
      *     CAMPO____LITERAL______________DELIM_ATT__LNG__             *
      *  01 CODCTAA  CODIGO DE CUENTA      01    A   35                *
      *  02 INDAUX1  INDICADOR AUXILIAR 1  02    A    1                *
      ******************************************************************
       01 REG-DIECDIR2.
           05 DIR2-CODCTAA                       PIC X(35).
           05 DIR2-INDAUX1                       PIC X(01).
           05 DIR2-INDPAGI                       PIC X(01).
           05 DIR2-PAGINAC                       PIC X(50).
           05 DIR2-PAGSIZE                       PIC 9(10).
      ******************************************************************
      *                                                                *
      *         LONGITUD TOTAL DEL REGISTRO EN BYTES :   36            *
      *                                                                *
      ******************************************************************

