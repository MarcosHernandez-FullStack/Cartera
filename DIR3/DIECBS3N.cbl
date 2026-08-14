      *****************************************************************
      *                 FORMATO DE SALIDA BS3N                        *
      *****************************************************************
       01 BS3N-DIECBS3N.
          05 BS3N-DETALLE-CUENTA.
             10 BS3N-FORMATO               PIC X(08).
             10 BS3N-COD-CARTERA           PIC X(12).
             10 BS3N-SERVICIO              PIC X(20).
             10 BS3N-CTA-ABONO             PIC X(20).
             10 BS3N-DIV-ABONO             PIC X(12).
             10 BS3N-CTA-CARGO             PIC X(20).
             10 BS3N-DIV-CARGO             PIC X(12).
             10 BS3N-TOT-ABONO             PIC S9(15)V9(2).
             10 BS3N-TOT-CARGO             PIC S9(15)V9(2).
             10 BS3N-FECHA-OPE             PIC X(10).
             10 BS3N-NUM-DOCS              PIC 9(05).
             10 BS3N-IMPORTE-TOT           PIC S9(15)V9(2).
             10 BS3N-INTERES-TOT           PIC S9(15)V9(2).
             10 BS3N-COMISION-TOT          PIC S9(15)V9(2).
             10 BS3N-GASTO-TOT             PIC S9(15)V9(2).
          05 BS3N-MOVIMIENTOS-CUENTA.
             10 BS3N-INDICADOR-PAGINACI    PIC X(01).
             10 BS3N-ARGUMENTO-BUSQUEDA    PIC X(20).
