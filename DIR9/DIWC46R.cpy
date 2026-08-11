      ******************************************************************
      *                                                                *
      *                            DIWC46R                             *
      *                            _______                             *
      *                                                                *
      *   COPY DE RUTINA DE CONSULTA PENDIENTE CARTERA DI6C0461        *
      *                                                                *
      ******************************************************************
      *****************************************************************
      *                 LOG DE MODIFICACIONES                         *
      *  MARCA      AUTOR   FECHA     DESCRIPCION                     *
      *  ---------  ------- --------  --------------------------------*
      *  PAGSIZE    GFT     10-08-26  SE ADICIONA TAMANO DE PAGINA    *
      *---------------------------------------------------------------*
       02  W460-DIWC46R.
           05 W460-ENTRADA.
              10 W460-NUM-CODCEN-ENT         PIC X(08).
              10 W460-FECVEN-ENT             PIC X(10).
              10 W460-NRODOC-ENT             PIC X(10).
      *__________________________________________________28 BYTES_____
      *PAGSIZE-INI
      *    TAMANO DE PAGINA SOLICITADO. CERO = ASUME EL MAXIMO (100).
      *    NO FORMA PARTE DE W460-ENTRADA PORQUE ESE GRUPO ES LA
      *    CLAVE VSAM DE DIPENCOD (28 BYTES) USADA EN EL STARTBR.
           05 W460-PAGSIZE                   PIC 9(10).
      *__________________________________________________10 BYTES_____
      *PAGSIZE-FIN
           05 W460-RETORNO.
              10 W460-CODRETOR               PIC X(02).
              10 W460-REFERENC               PIC X(30).
      *__________________________________________________32 BYTES_____
           05 W460-SALIDA.
              10 W460-OCURRENCIAS            PIC 9(03).
              10 W460-INDIC-PAGINAC          PIC X(01).
              10 W460-AREA-PAGINAC.
                 15 W460-CODCEN-PAGINAC      PIC X(08).
                 15 W460-FECVEN-PAGINAC      PIC X(10).
                 15 W460-NRODOC-PAGINAC      PIC X(10).
      *__________________________________________________32 BYTES_____
              10 W460-OPERACIONES     OCCURS 100 TIMES.
                 15 W460-CODCAR              PIC 9(12).
                 15 W460-NRODOC              PIC 9(10).
                 15 W460-NUMBCOORI           PIC X(12).
                 15 W460-FECVEN              PIC X(10).
                 15 W460-NOMBRE              PIC X(40).
                 15 W460-IMPORTE             PIC S9(15)V99.
                 15 W460-TIPO                PIC X.
                 15 W460-SITUACION           PIC X.
      *_________________________________________________103 BYTES_____
      *_______________________________________________10402 BYTES_____
