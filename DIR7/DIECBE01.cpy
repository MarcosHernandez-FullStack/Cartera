      *****************************************************************
      * APLICATIVO: CARTERA MIGRACION NETCASH A SENDA                 *
      *             FRONTAL - ASO - APX - HOST (CICS, DB2, APLICATIVO *
      *             FORMATO DE ENTRADA BE00  CANAL 0026               *
      * CLON DE KNECBE00 / DIECBE00                                   *
      *****************************************************************
      *----------------------------------------------------------------
      *  FORMATO DE ENTRADA ESTANDAR CANAL 0026
      *----------------------------------------------------------------
      *  DESCRIPCION DE CAMPOS - FORMATO BE01   LONGITUD REG:  399
      *----------------------------------------------------------------
      *      BE01-BCODACCC  - CODIGO DE ACCESO          1   35
      *      BE01-USUARIO1  - COD.USUARIO 1            36    8
      *      BE01-USUARIO2  - COD.USUARIO 2            44    8
      *      BE01-BCODCTAA  - COD.DE CUENTA            52   35
      *      BE01-BPALACCE  - PALABRA DE PASO          87   16
      *      BE01-BPALACC2  - PALABRA OMC PASO        103   16
      *      BE01-BASUNPRO  - ASUNTO PROPIO           119   37
      *      BE01-BASUNASO  - ASUNTO ASOCIADO         156   37
      *      BE01-BFECHA01  - FECHA 1                 193   10
      *      BE01-BFECHA02  - FECHA 2                 203   10
      *      BE01-BIMPORTE  - IMPORTE PRINCIPAL       213   17
      *      BE01-BIMPOAUX  - IMPORTE AUXILIAR        230   17
      *      BE01-BINDAUX1  - INDICADOR 1             247    1
      *      BE01-BINDAUX2  - INDICADOR 2             248    1
      *      BE01-BCODAUX1  - CODIGO    1             249   35
      *      BE01-BCODAUX2  - CODIGO    2             284   35
      *      BE01-BNUMAUX1  - NUMERO    1             319   15
      *      BE01-BNUMAUX2  - NUMERO    2             334   15
      *      BE01-BINDPAGI  - INDICADOR DE PAGINACION 349    1
      *      BE01-BPAGINAC  - AREA DE PAGINACION      350   50
      *
       01 BE01-DIECBE01.
          05 BE01-BCODACCC           PIC X(35).
          05 BE01-USUARIO1           PIC X(08).
          05 BE01-USUARIO2           PIC X(08).
          05 BE01-BCODCTAA           PIC X(35).
          05 BE01-BPALACCE           PIC X(16).
          05 BE01-BPALACC2           PIC X(16).
          05 BE01-BASUNPRO           PIC X(37).
          05 BE01-BASUNASO           PIC X(37).
          05 BE01-BFECHA01           PIC X(10).
          05 BE01-BFECHA02           PIC X(10).
          05 BE01-BIMPORTE           PIC S9(15)V99.
          05 BE01-BIMPOAUX           PIC S9(15)V99.
          05 BE01-BINDAUX1           PIC X(01).
          05 BE01-BINDAUX2           PIC X(01).
          05 BE01-BCODAUX1           PIC X(35).
          05 BE01-BCODAUX2           PIC X(35).
          05 BE01-BNUMAUX1           PIC S9(15).
          05 BE01-BNUMAUX2           PIC S9(15).
          05 BE01-BINDPAGI           PIC X(01).
          05 BE01-BPAGINAC           PIC X(50).
      *
      *****************************************************************
      *                     FIN COPY DIECBE01                         *
      *****************************************************************
