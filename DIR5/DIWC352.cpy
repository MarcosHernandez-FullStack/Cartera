      ******************************************************************
      *                                                                *
      *                            DIWC352                             *
      *                            --------                            *
      *                                                                *
      *   COPY DE RUTINA DE CONSULTA PENDIENTE CARTERA DI6C0352        *
      *                                                                *
      ******************************************************************
       02  W352-DIWC352.
           05 W352-ENTRADA.
              10 W352-COD-CARTERA.
                 15 W352-CODIGO         PIC 9(10).
                 15 W352-CODCOD  REDEFINES  W352-CODIGO.
                    20 W352-CODOFI      PIC 9(03).
                    20 W352-CODMON      PIC 9(01).
                    20 W352-CODCLI      PIC 9(06).
                 15 W352-TIPCTA         PIC 9(02).
              10 W352-CAR-IMPACT        PIC S9(13)V99.
              10 W352-PLAORI            PIC 9(04).
              10 W352-CAR-GASNOT        PIC 9(09)V99 COMP-3.
              10 W352-CAR-INDPRO        PIC 9(01).
              10 W352-CAR-INSCOB        PIC 9(02).
              10 W352-MCAR-TARIFAS.
                 15 W352-PORTES         PIC 9(04).
                 15 W352-TRASNC         PIC 9(04).
                 15 W352-TRASBC         PIC 9(04).
                 15 W352-TRASBN         PIC 9(04).
                 15 W352-COMTRA         PIC 9(04).
                 15 W352-COMACE         PIC 9(04).
                 15 W352-COMCOB         PIC 9(04).
                 15 W352-COMCTE         PIC 9(04).
                 15 W352-COMPRO         PIC 9(04).
                 15 W352-INTERE         PIC 9(04).
                 15 W352-COMISI         PIC 9(04).
              10 W352-CAR-COMISIONES.
                 15 W352-CAR-COMPRO     PIC 9(09)V99  COMP-3.
                 15 W352-CAR-COMMOR     PIC 9(09)V99  COMP-3.
                 15 W352-CAR-COMNVO     PIC 9(09)V99  COMP-3.
                 15 W352-CAR-COMCOB     PIC 9(09)V99  COMP-3.
                 15 W352-CAR-COMTRF     PIC 9(09)V99  COMP-3.
                 15 W352-CAR-COMCTE     PIC 9(09)V99  COMP-3.
                 15 W352-CAR-PORTES     PIC 9(09)V99  COMP-3.
              10 W352-CAR-INTERESES.
                 15 W352-CAR-INTPRO      PIC 9(09)V99  COMP-3.
                 15 W352-CAR-INTVEN      PIC 9(09)V99  COMP-3.
                 15 W352-CAR-INTMOR      PIC 9(09)V99  COMP-3.
                 15 W352-CAR-INTNVO      PIC 9(09)V99  COMP-3.
                 15 W352-CAR-INTDEV      PIC 9(09)V99  COMP-3.
              10 W352-CAR-IMPUESTOS.
                 15 W352-CAR-IMPISC      PIC 9(09)V99  COMP-3.
                 15 W352-CAR-IMPINT      PIC 9(09)V99  COMP-3.
                 15 W352-CAR-DEVISC      PIC 9(09)V99  COMP-3.
              10 W352-DEVDOC             PIC X(01).
      *
      *--200210109-INI
      *
              10 W352-FECVEN             PIC X(10).
              10 W352-FECTRA             PIC X(10).
              10 W352-HORINI             PIC X(06).
              10 W352-COBRAR             PIC X(01).
      *       10 FILLER                  PIC X(99).
              10 W352-FILLER             PIC X(72).
      *
      *--200210109-FIN
      *
           05 W352-RETORNO.
              10 W352-CODRETOR           PIC X(02).
              10 W352-CODERROR           PIC X(07).
              10 W352-REFERENC           PIC X(30).
           05 W352-SALIDA.
              10 W352-IMPTRA             PIC S9(15)V9(2).
