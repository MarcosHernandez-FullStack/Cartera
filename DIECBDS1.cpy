      ******************************************************************
      *                                                                *
      * TRX     :DIR2 CARTERA - SALDOS/MVTOS                           *
      * DIECBDS1:FORMATO DE SALIDA
      *                                                                *
      *     CAMPO____LITERAL______________DELIM_ATT__LNG__             *
      *  01 FECOPER  FECHA DE OPERACION    01    A   10                *
      *  02 TOTCAR   TOTAL CARGOS          02    S   17,2              *
      *  03 TOTABO   TOTAL ABONOS          03    S   17,2              *
      ******************************************************************
       01 REG-DIECBDS1.
           05 BDS1-FECOPER          PIC X(10).
           05 BDS1-TOTCAR           PIC S9(15)V9(2).
           05 BDS1-TOTABO           PIC S9(15)V9(2).
      ******************************************************************
      *                                                                *
      *         LONGITUD TOTAL DEL REGISTRO EN BYTES :   44            *
      *                                                                *
      ******************************************************************
