      *--------------------------------------------------------------*
      * DIECBS3K : COPY DE FORMATO DE SALIDA BS3K                    *
      *            CONSULTA DE DOCUMENTOS                            *
      *--------------------------------------------------------------*
      * ---   LONGITUD DEL REGISTRO :  146 BYTES   ------------------*
      *--------------------------------------------------------------*
      *
       01 BS3K-DIECBS3K.
          05 BS3K-FORMATO                      PIC  X(08)        .
          05 BS3K-INDIC-PAGINAC                PIC  X(01)       .
          05 BS3K-ARG-BUSQUEDA                 PIC  X(50)       .
          05 BS3K-COD-CARTERA                  PIC  X(12)       .
          05 BS3K-SERVICIO                     PIC  X(20)       .
          05 BS3K-NUMDOC-TOT                   PIC  S9(05)      .
          05 BS3K-IMPTOT-REGS                  PIC  S9(13)V9(02).
          05 BS3K-NUMDOC-X-REGS                PIC  S9(05)      .
          05 BS3K-IMPTOT-X-REGS                PIC  S9(13)V9(02).
          05 BS3K-SALDO-TOTAL                  PIC  S9(13)V9(02).
      *
