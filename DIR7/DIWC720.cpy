      *________________________________________________________________*
      *    AREA DE COMUNICACION CON MODULO INTERFASE DE CARTERA        *
      *________________________________________________________________*
      *    DIWC720 AREA DE ENLACE CON EL MODULO DE CONTINET            *
      *            POSICION DE CARTERA                                 *
      *            LONGITUD: 11990                                     *
      *                                                                *
      *          . EL NUMERO TOTAL DE LINEAS DE DETALLE (W720-DETALLE) *
      *            ESTA EN : W720-NUMDOC-TOT                           *
      *                                                                *
      *          . W720-SITUAC = 'ATN' -->  ATENDIDA                   *
      *            W720-SITUAC = 'VIG' -->  VIGENTE                    *
      *                                                                *
      *          . W720-INDIC-PAGINAC = 'S' SI HAY MAS DATOS (OUTPUT)  *
      *            W720-INDIC-PAGINAC = 'N' NO HAY MAS DATOS (OUTPUT)  *
      *            W720-INDIC-PAGINAC = ' ' PRIMERA PAGINA   (INPUT)   *
      *            W720-INDIC-PAGINAC = 'S' SI PAGINACION    (INPUT)   *
      *                                                                *
      *          . W720-FECVEN-PAGINAC=     FECVEN PUNTERO   (OUTPUT)  *
      *            W720-FECVEN-PAGINAC=     FECVEN PUNTERO   (INPUT)   *
      *                                                                *
      *          . W720-NUMBCO-PAGINAC=     NUMBCO PUNTERO   (OUTPUT)  *
      *            W720-NUMBCO-PAGINAC=     NUMBCO PUNTERO   (INPUT)   *
      *________________________________________________________________*
      * W720-DIRCAPLN W720-CODRETOR W720-REFERENC                      *
      * _____________ _____________ _____________                      *
      *               00                                               *
      * DIE7208       08            ERROR CODIGO CARTERA NO NUMERICO   *
      * DIE7210       10            ERROR INDIC  PAGINAC INVALIDO      *
      * DIE7211       11            ERROR FECVEN PAGINAC INVALIDO      *
      * DIE7212       12            ERROR NUMBCO PAGINAC INVALIDO      *
      * DIE7250       50            ERROR POSIC  FECVENNUMBCO INVALIDO *
      * DIE7267       67            ERROR DIMAESTR CLAVE  NO EXISTE    *
      * DIE7268       68            ERROR DIMAESTR CERRADO             *
      * DIE7269       69            ERROR DIMAESTR READ                *
      * DIE7281       81            ERROR DIDESEMB ENDBR               *
      * DIE7286       86            ERROR DIDESEMB READNEXT            *
      * DIE7287       87            ERROR DIDESEMB CLAVE  NO EXISTE    *
      * DIE7288       88            ERROR DIDESEMB CERRADO             *
      * DIE7289       89            ERROR DIDESEMB STARTBR             *
      * DIE7291       91            ERROR DIPENDTE ENDBR               *
      * DIE7296       96            ERROR DIPENDTE READNEXT            *
      * DIE7297       97            ERROR DIPENDTE CLAVE  NO EXISTE    *
      * DIE7298       98            ERROR DIPENDTE CERRADO             *
      * DIE7299       99            ERROR DIPENDTE STARTBR             *
      *________________________________________________________________*
           02 W720-DIWC720.
              05 W720-ENTRADA.
                 10 W720-CODIGO.
                    40 W720-CODOFI                 PIC 9(03).
                    40 W720-CODMON                 PIC 9(01).
                    40 W720-CODCLI                 PIC 9(06).
                    40 W720-CODSER                 PIC 9(02).
              05 W720-FECRANG.
                 10 W720-BFECHA01                  PIC X(10).
                 10 W720-BFECHA02                  PIC X(10).
      *__________________________________________________12 BYTES_____
              05 W720-RETORNO.
                 10 W720-CODRETOR                  PIC X(02).
                 10 W720-REFERENC                  PIC X(30).
                 10 W720-DIRCAPLN                  PIC X(07).
      *__________________________________________________39 BYTES_____
              05 W720-PAGINAC.
                 10 W720-OCURRENCIAS               PIC 9(04).
                 10 W720-INDIC-PAGINAC             PIC X(01).
                 10 W720-AREA-PAGINAC.
                    20 W720-FECVEN-PAGINAC         PIC 9(08).
                    20 W720-NUMBCO-PAGINAC         PIC 9(10).
      *__________________________________________________23 BYTES_____
              05 W720-SALIDA.
                 10 W720-TOTALES.
                    20 W720-CODIGO-TOT             PIC 9(12).
                    20 W720-SERVICIO-TOT           PIC X(20).
                    20 W720-NUMDOC-TOT             PIC 9(05).
                    20 W720-IMPORT-TOT             PIC 9(13)V99 COMP-3.
                    20 W720-NUMDOC-TOT-PR          PIC 9(05).
                    20 W720-IMPORT-TOT-PR          PIC 9(13)V99 COMP-3.
                    20 W720-IMPORT-TOT-RYPR        PIC 9(13)V99 COMP-3.
      *__________________________________________________66 BYTES_____
      *          10 W720-DETALLE                   OCCURS 150 TIMES.
                 10 W720-DETALLE                   OCCURS 130 TIMES.
                    20 W720-FECVEN                 PIC 9(08).
                    20 W720-NUMBCO                 PIC 9(10).
      *             20 W720-NUMBCOORI              PIC 9(10).
      *@001-I
      *             20 W720-NUMBCOORI              PIC X(10).
                    20 W720-NUMBCOORI              PIC X(12).
      *@001-F
                    20 W720-NOMACE                 PIC X(40).
                    20 W720-IMPORT                 PIC 9(13)V99 COMP-3.
                    20 W720-SITUAC                 PIC X(03).
      *__________________________________________________79 BYTES_____
      *_______________________________________________11990 BYTES_____
