      ******************************************************************
      *                                                                *
      *                            DIWC350                             *
      *                            _______                             *
      *                                                                *
      *   COPY DE RUTINA DE CONSULTA PENDIENTE CARTERA DI6C0350        *
      *                                                                *
      ******************************************************************
      * MODIFICACIÓN                                                   *
      * EVE001   03/06/2010 :  SE REALIZA CAMBIO PARA EL TIPO DE       *
      *                        CREDITO.                                *
      *                        10 W350-CAR-TIPEMP      PIC 9(01).->    *
      *                        10 W350-CAR-TIPEMP      PIC X(01)       *
      ******************************************************************
      *****************************************************************
      *                 LOG DE MODIFICACIONES                         *
      *  MARCA      AUTOR   FECHA     DESCRIPCION                     *
      *  ---------  ------- --------  --------------------------------*
      * 6762016047 P018899 22-09-2016 CAVALI - FACTURA AMPLIAR DE     *
      *                                        10 A 12 POSICIONES     *
      *  -------    ------- --------  ----------------------------    *
      *---------------------------------------------------------------*
       02  W350-DIWC350.
           05 W350-ENTRADA.
              10 W350-COD-CARTERA.
                 15 W350-CODIGO       PIC 9(10).
                 15 W350-CODCOD  REDEFINES  W350-CODIGO.
                    20 W350-CODOFI    PIC 9(03).
                    20 W350-CODMON    PIC 9(01).
                    20 W350-CODCLI    PIC 9(06).
                 15 W350-TIPCTA       PIC 9(02).
              10 W350-NUMBCO          PIC 9(10).
              10 W350-INDINT          PIC X(01).
              10 W350-CODCTA          PIC X(20).
              10 W350-INDCAN          PIC X(01).
              10 W350-NUMBCOORI       PIC X(10).
              10 FILLER               PIC X(90).
      *_________________________________________________144 BYTES_____
           05 W350-RETORNO.
              10 W350-CODRETOR        PIC X(02).
              10 W350-CODERROR        PIC X(07).
              10 W350-REFERENC        PIC X(30).
      *__________________________________________________39 BYTES_____
           05 W350-SALIDA.
              10 W350-CODDIV-CTACAR   PIC X(04).
              10 W350-DES-DIVISA      PIC X(12).
              10 W350-AGEORI          PIC 9(03).
              10 W350-NOMORI          PIC X(40).
              10 W350-NOMDES          PIC X(40).
              10 W350-CAR-TASINT      PIC 99V999.
              10 W350-CAR-NROLIQ      PIC 9(07).
              10 W350-CAR-NOMCLI      PIC X(20).
              10 W350-CAR-NOMGIR      PIC X(20).
              10 W350-CAR-INSCOB      PIC 9(02).
      *EVE001-I
      *       10 W350-CAR-TIPEMP      PIC 9(01).
              10 W350-CAR-TIPEMP      PIC X(01).
      *EVE001-F
              10 W350-CAR-INDPRO      PIC 9(01).
              10 W350-CAR-GASNOT      PIC 9(09)V99 COMP-3.
              10 W350-CAR-TASA01      PIC 9(02)V99 COMP-3.
              10 W350-CAR-TASA02      PIC 9(02)V99 COMP-3.
              10 W350-CAR-TDOCUM      PIC 9(02).
              10 W350-CAR-PLACOB      PIC 9(04).
              10 W350-CAR-PLAACT      PIC 9(04).
              10 W350-CAR-FECVEN      PIC X(10).
              10 W350-CAR-FECING      PIC X(10).
              10 W350-CAR-FECIOR      PIC X(10).
              10 W350-CAR-FECCAR      PIC X(10).
              10 W350-CAR-IMPACT      PIC S9(15)V99.
              10 W350-CAR-PORREN      PIC 9(03).
              10 W350-CAR-NUEBCO      PIC 9(10).
      *6762016047-I
      *       10 W350-CAR-NUMBCOORI   PIC X(10).
              10 W350-CAR-NUMBCOORI   PIC X(12).
      *6762016047-F
              10 W350-CAR-PLAORI      PIC 9(04).
              10 W350-CAR-CAMPAN      PIC X(01).
              10 W350-CODMDA          PIC 9(02).
              10 W350-TIPCAM          PIC 9(03)V9(04).
              10 W350-MCAR-TARIFAS.
                 15 W350-PORTES         PIC 9(04).
                 15 W350-TRASNC         PIC 9(04).
                 15 W350-TRASBC         PIC 9(04).
                 15 W350-TRASBN         PIC 9(04).
                 15 W350-COMTRA         PIC 9(04).
                 15 W350-COMACE         PIC 9(04).
                 15 W350-COMCOB         PIC 9(04).
                 15 W350-COMCTE         PIC 9(04).
                 15 W350-COMPRO         PIC 9(04).
                 15 W350-INTERE         PIC 9(04).
                 15 W350-COMISI         PIC 9(04).
              10 W350-CAR-COMISIONES.
                 15 W350-CAR-COMPRO     PIC 9(09)V99  COMP-3.
                 15 W350-CAR-COMMOR     PIC 9(09)V99  COMP-3.
                 15 W350-CAR-COMNVO     PIC 9(09)V99  COMP-3.
                 15 W350-CAR-COMCOB     PIC 9(09)V99  COMP-3.
                 15 W350-CAR-COMTRF     PIC 9(09)V99  COMP-3.
                 15 W350-CAR-COMCTE     PIC 9(09)V99  COMP-3.
                 15 W350-CAR-PORTES     PIC 9(09)V99  COMP-3.
              10 W350-CAR-INTERESES.
                 15 W350-CAR-INTPRO      PIC 9(09)V99  COMP-3.
                 15 W350-CAR-INTVEN      PIC 9(09)V99  COMP-3.
                 15 W350-CAR-INTMOR      PIC 9(09)V99  COMP-3.
                 15 W350-CAR-INTNVO      PIC 9(09)V99  COMP-3.
                 15 W350-CAR-INTDEV      PIC 9(09)V99  COMP-3.
              10 W350-CAR-DIAS-CALCULADOS.
                 15 W350-CAR-DIAVEN      PIC 9(03)     COMP-3.
                 15 W350-CAR-DIAMOR      PIC 9(03)     COMP-3.
                 15 W350-CAR-DIANVO      PIC 9(03)     COMP-3.
                 15 W350-CAR-DIADEV      PIC 9(03)     COMP-3.
                 15 W350-CAR-DIAPRO      PIC 9(03)     COMP-3.
              10 W350-CAR-IMPUESTOS.
                 15 W350-CAR-IMPISC      PIC 9(09)V99  COMP-3.
                 15 W350-CAR-IMPINT      PIC 9(09)V99  COMP-3.
                 15 W350-CAR-DEVISC      PIC 9(09)V99  COMP-3.
      *_________________________________________________415 BYTES_____
      *_________________________________________________598 BYTES_____
