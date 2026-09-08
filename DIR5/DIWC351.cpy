      ******************************************************************
      *                                                                *
      *                            DIWC351                             *
      *                            --------                            *
      *                                                                *
      *   COPY DE RUTINA DE CONSULTA PENDIENTE CARTERA DI6C0351        *
      *                                                                *
      ******************************************************************
       02  W351-DIWC351.
           05 W351-ENTRADA.
              10 W351-COD-CARTERA.
                 15 W351-CODIGO       PIC 9(10).
                 15 W351-CODCOD  REDEFINES  W351-CODIGO.
                    20 W351-CODOFI    PIC 9(03).
                    20 W351-CODMON    PIC 9(01).
                    20 W351-CODCLI    PIC 9(06).
                 15 W351-TIPCTA       PIC 9(02).
              10 W351-INDINT          PIC X(01).
              10 W351-FECTRA          PIC X(10).
              10 W351-FECVEN          PIC X(10).
              10 W351-IMPACT          PIC S9(13)V99.
              10 W351-TIPCAM          PIC 9(03)V9(04).
              10 W351-CAMPAN          PIC X(01).
              10 W351-PLAORI          PIC 9(04).
              10 W351-FILLER          PIC X(100).
           05 W351-RETORNO.
              10 W351-CODRETOR        PIC X(02).
              10 W351-CODERROR        PIC X(07).
              10 W351-REFERENC        PIC X(30).
           05 W351-SALIDA.
              10 W351-CAR-TASINT      PIC 9(4)V9(4).
              10 W351-CAR-TIPTAS      PIC 9(01).
              10 W351-CAR-TASA01      PIC 9(02)V99 COMP-3.
              10 W351-CAR-TASA02      PIC 9(02)V99 COMP-3.
              10 W351-CAR-INTERESES.
                 15 W351-CAR-INTPRO      PIC 9(09)V99  COMP-3.
                 15 W351-CAR-INTVEN      PIC 9(09)V99  COMP-3.
                 15 W351-CAR-INTMOR      PIC 9(09)V99  COMP-3.
                 15 W351-CAR-INTNVO      PIC 9(09)V99  COMP-3.
                 15 W351-CAR-INTDEV      PIC 9(09)V99  COMP-3.
              10 W351-CAR-DIAS-CALCULADOS.
                 15 W351-CAR-DIAVEN      PIC 9(03)     COMP-3.
                 15 W351-CAR-DIAMOR      PIC 9(03)     COMP-3.
                 15 W351-CAR-DIANVO      PIC 9(03)     COMP-3.
                 15 W351-CAR-DIADEV      PIC 9(03)     COMP-3.
                 15 W351-CAR-DIAPRO      PIC 9(03)     COMP-3.
              10 W351-CAR-DEVDIA         PIC 9(05)     COMP-3.
