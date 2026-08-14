      *****************************************************************
      * APLICATIVO: CARTERA MIGRACION NETCASH A SENDA                 *
      *             FRONTAL - ASO - APX - HOST (CICS, DB2, APLICATIVO *
      *             FORMATO DE ENTRADA BE33  CANAL 0026               *
      * CLON DE KNECBE33                                              *
      *****************************************************************
      *----------------------------------------------------------------
      *  FORMATO DE ENTRADA ESTANDAR CANAL 0026
      *----------------------------------------------------------------
      *  DESCRIPCION DE CAMPOS - FORMATO BE33   LONGITUD REG:  399
      *----------------------------------------------------------------
      *      BE33-USUARIO1  - COD.USUARIO 1            36    8
      *      BE33-BCODCTAA  - COD.DE CUENTA            52   35
      *      BE33-BFECHA01  - FECHA 1                 193   10
      *      BE33-BINDPAGI  - INDICADOR DE PAGINACION 349    1
      *      BE33-BPAGINAC  - AREA DE PAGINACION      350   50
      *      BE33-BPAGSIZE  - REGISTROS A DEVOLVER     10   50
      *
       01 BE33-DIECBE33.
          05 BE33-USUARIO1           PIC X(08).
          05 BE33-BCODCTAA           PIC X(35).
          05 BE33-BFECHA01           PIC X(10).
          05 BE33-BINDPAGI           PIC X(01).
          05 BE33-BPAGINAC           PIC X(50).
          05 BE33-BPAGSIZE           PIC 9(10).
      *
      *****************************************************************
      *                     FIN COPY DIECBE33                         *
      *****************************************************************
