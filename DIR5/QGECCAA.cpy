      ******************************************************************
      * QGECCAA: COMMAREA DE ARQUITECTURA DE APLICACIONES              *
      ******************************************************************
      *ASTAAU0002 UNIFICACION DE COMMAREAS                             *
      *F.INSTAL:18012007                                               *
      ******************************************************************
      *                                                                *
      * NOMBRE DEL OBJETO:  QGECCAA                                    *
      *                                                                *
      * DESCRIPCION:    COMMAREA DE ARQUITECTURA DE APLICACIONES       *
      * --------------------------------------------------------       *
      *                                                                *
      * AREA DE COMUNICACION ENTRE PROGRAMAS DE APLICACION             *
      * Y ARQUITECTURA                                                 *
      *                                                                *
      *                                                                *
      *                                                                *
      *                   **---------------------------------------**  *
      *                   **------  ENTRADA A LA APLICACION  ------**  *
      *                   **----------- DATOS GENERALES -----------**  *
      *                   **---------------------------------------**  *
      * DATOS-GRLES                                                    *
      * ENTIDAD           ENTIDAD                  CONTABLE            *
      * CENTRO-CONT       OFICINA                  CONTABLE            *
      * NETNAME-CONT      TERMINAL RED  (NETNAME)  CONTABLE            *
      * TERMINAL-CONT     TERMINAL CICS (EIBTRMID) CONTABLE            *
      * FECHA-CONT        FECHA CONTABLE                 AAAAMMDD      *
      * FECHA-CONT2       FECHA CONTABLE DB2             AAAA-MM-DD    *
      * FECHA-CONTED      FECHA CONTABLE EDITADA         DD/MM/AAAA    *
      * FECHA-OPER        FECHA OPERACION                AAAAMMDD      *
      * FECHA-OPER2       FECHA OPERACION DB2            AAAA-MM-DD    *
      * FECHA-OPERED      FECHA OPERACION EDITADA        DD/MM/AAAA    *
      * FECHA-TRANS       FECHA TRANSMISION              AAAAMMDD      *
      * FECHA-TRANS2      FECHA TRANSMISION DB2          AAAA-MM-DD    *
      * FECHA-TRANSED     FECHA TRANSMISION EDITADA      DD/MM/AAAA    *
      * HORA-TRANS        HORA  TRANSMISION              HHMMSS        *
      * HORA-TRANSED      HORA  TRANSMISION DB2/EDITADA  HH:MM:SS      *
      * NETNAME           TERMINAL RED  (NETNAME)  FISICO              *
      * TERMINAL          TERMINAL CICS (EIBTRMID) FISICO              *
      * USERID            USUARIO IDENFIFICADO AL CICS                 *
      * SESION            SESION ==> MANANA / TARDE                    *
      * SESION-MANANA     <'M'> MANANA                                 *
      * SESION-TARDE      <'T'> TARDE                                  *
      * TIPO-TERM         TIPO DE TERMINAL                             *
      * TIPO-TERM-4700    <'11'>             = 4700                    *
      * TIPO-TERM-5935    <'12'>             = 5935                    *
      * TIPO-TERM-3270    <'20', '28', '29'> = 3270                    *
      * TIPO-TERM-PS-1    <'13'>             = PS/2 ALTAMIRA           *
      * TIPO-TERM-PS-2    <'14'>             = PS/2 RONDA              *
      * TIPO-TERM-PS-3    <'15'>             = PS/2 ICO                *
      * TIPO-TERM-PS-4    <'17'>             = PS/2 BCT                *
      * TIPO-TERM-PS-5    <'18'>             = PS/2 CAIXA CATALUNYA    *
      * TIPO-TERM-PS-6    <'19'>             = PS/2 FFS - FOUNDATION   *
      * TIPO-TERM-PS      <'13' '14' '15' '16' '17' '18' '19'> = PS/2  *
      * TIPO-TERM-VIDEOTEX<'16'>             = TERMINAL VIDEOTEX       *
      * TIPO-TERM-DIEBOLD <'39'>             = CAJERO DIEBOLD          *
      * TIPO-TERM-CAJERO  <'40'>             = CAJERO                  *
      * TIPO-TERM-CECA1   <'41'>             = CECA MASTER             *
      * TIPO-TERM-CECA2   <'42'>             = CECA                    *
      * TIPO-TERM-CECA    <'41' '42'>          CECA                    *
      * TIPO-TERM-TPV     <'43'>             = TPV                     *
      * TIPO-TERM-COR-E   <'44'>             = CORONA ENTRADA          *
      * TIPO-TERM-COR-S   <'45'>             = CORONA SALIDA           *
      * TIPO-TERM-CORONA  <'44' '45' '52'>   = CORONA                  *
      * TIPO-TERM-SEMP-AE <'46'>             = SEMP EMISOR             *
      * TIPO-TERM-SEMP-AA <'47'>             = SEMP ADQUIRENTE         *
      * TIPO-TERM-SEMP-RF <'48'>             = SEMP RECEP.FICHEROS     *
      * TIPO-TERM-SEMP-AF <'49'>             = SEMP ACTUAL.FICHEROS    *
      * TIPO-TERM-SEMP-F  <'50'>             = SEMP FICTICIOS          *
      * TIPO-TERM-SEMP    <'46' '47' '48' '49' '50' > SEMP             *
      * TIPO-TERM-ENTRADA <'53'>             = TRM ENTRADA             *
      * TIPO-TERM-4B-REAL <'54'>             = 4B REAL                 *
      * TIPO-TERM-4B-FIC  <'55'>             = 4B FICTICIO             *
      * TIPO-TERM-4B      <'54' '55'>        = 4B                      *
      * TIPO-TERM-PROSA-F <'56'>             = PROSA FICTICIOS         *
      * TIPO-TERM-PROSA-AP<'57'>             = PROSA ATM - POS         *
      * TIPO-TERM-PROSA   <'56' '57'>        = PROSA                   *
      * TIPO-TERM-CENTROAUT   '40' '41' '42' '43' '44' '45' '46' '47'  *
      *                       '48' '49' '50' '52' '53' '54' '55' '39'  *
      *                       '56' '57'                                *
      * TIPO-TERM-PS7B    <'84'>             = PS7B                    *
      * TIPO-TERM-VISANET <'85'>             = VISA NET 'VNET'         *
      * TIPO-TERM-CIRRUS  <'86'>             = CIRRUS   'CIRR'         *
      * CICS              IDENFIFICADOR DEL CICS - SYSID               *
      * CODTRAN           CODIGO DE TRANSACCION                        *
      * TIPO-PROCESO      TIPO DE PROCESO                              *
      * TIPO-PROCESO-ONLI <'O'> ON-LINE                                *
      * TIPO-PROCESO-AUTO <'A'> AUTORIZACION                           *
      * TIPO-PROCESO-TELE <'T'> TELEDISCO                              *
      * TIPO-PROCESO-OFF  <'F'> OFF-LINE                               *
      * ESTADO-APLIC                                                   *
      * ACTIVA            <'A'> ACTIVA                                 *
      * DESACTIVA         <'D'> DESACTIVA                              *
      * CAMBIO-SESION     <'C'> EN CAMBIO DE SESION                    *
      * RECUPERACION      <'R'> EN RECUPERACION                        *
      * NUM-REINTENTOS    NUMERO DE REINTENTOS EN BLOQUEO DE DB2       *
      * NUM-SECUE         NUMERO DE SECUENCIA DE LA TRANS. EN TERM.    *
      * VERSION-GAT       VERSION GAT /APB                             *
      * IDIOMA-TERM       CODIGO DE IDIOMA DEL TEMINALISTA             *
      *                   CATALAN = 'C'                                *
      *                   ESPA%OL = 'E'                                *
      * FILLER            RELLENO PARA FUTURAS INCORPORACIONES         *
      *                   **---------------------------------------**  *
      *                   **------  ENTRADA A LA APLICACION  ------**  *
      *                   **---------- DATOS DEL MENSAJE ----------**  *
      *                   **---------------------------------------**  *
      * ENTRADA-TERM                                                   *
      * TECLA             TECLA PULSADA                                *
      * CAJERO            CAJERO UTILIZADO   (CAJA/COMPENSACION)       *
      * CAJERO-A          <'A'> CAJERO 'A'                             *
      * CAJERO-B          <'B'> CAJERO 'B'                             *
      * MOD-TAG           MODIFICACION DATOS EN PANEL ENTRADA          *
      * MOD-TAG-SI        <'S'> SI HA HABIDO MODIFICACIONES            *
      * MOD-TAG-NO        <'N'> NO HA HABIDO MODIFICACIONES            *
      * MOD-TAG-NADA      <' '> NO PROCEDE. ENTRADA DISTINTA SALIDA    *
      * PTR-COPYIN        APUNTADOR MAPA DATOS PANTALLA DE ENT/SAL     *
      * PTR-COPYIN-VACIO  <HIGH-VALUES> NO HAY DATOS                   *
      * PAPEL             INFORMA SI EL TERMINAL TIENE YS PAPEL O NO   *
      * FILLER            FILLER PARA FUTURAS APLICACIONES             *
      *                   **---------------------------------------**  *
      *                   **---  AUTORIZACIONES (ENTRADA/SALIDA) --**  *
      *                   **---------------------------------------**  *
      * AUTORIZACION                                                   *
      * AUTORIZ           AUTORIZACIONES (COD.RECHAZOS YA AUTORIZ)     *
      * CODERR-AUT        CODIGO DE ERROR AUTORIZADO                   *
      * SITUACION-AUT     USUARIO QUE AUTORIZA                         *
      * IND-AUTO          INDICADOR (S/N) - ERROR PENDIENTE AUTORIZ.   *
      * IND-AUTO-SI       <'S'>  PENDIENTE DE AUTORIZAR                *
      * IND-AUTO-NO       <'N', ' '>  NO ESTA PENDIENTE DE AUTORIZAR   *
      * IMPORTE-AUTO      IMPORTE PENDIENTE DE AUTORIZAR               *
      * REFER-AUTO        REFERENCIA DE LA OPERACION SEGUN APLICACIO   *
      * ERROR-AUTO-ALT    ERROR EN LA AUTORIZACION ALTAMIRA            *
      * ERROR-AUTO-ALT-SI <'S'> SI ERROR EN LA VALICACION USUARIO/*    *
      *                     PASSWORD EN LA AUTORIZACION                *
      * FILLER            RELLENO PARA FUTURAS INCORPORACIONES         *
      *                   **---------------------------------------**  *
      *                   **---  AUTORIZACIONES (PTR. SALIDA)    --**  *
      *                   **---------------------------------------**  *
      * AUTORIZACION-AUX                                               *
      * PTR-CUA           PUNTERO A DATOS DE AUTORIZACIONES(QGECCUA)   *
      * NUMSEQ            NUMERO DE SECUENCIA DE LA AUTORIZACION       *
      *                   **---------------------------------------**  *
      *                   **-  DATOS CONVERSACION ENTRADA/SALIDA  -**  *
      *                   **---------------------------------------**  *
      * CONVERS                                                        *
      * ESTADO            ESTADO  (I=INICIO / C=CONTIN / X=CONFIRM)    *
      * ESTADO-INICIO     <'I'>  INICIO                                *
      * ESTADO-CONTIN     <'C'>  CONTINUACION                          *
      * ESTADO-CONFIR     <'X'>  CONFIRMACION                          *
      * CASO              CASO    PARA LA APLICACION                   *
      * DATOS             DATOS   PROPIOS DE LA APLICACION             *
      *  LISTADO-DIN      DATOS  UTILIZADOS POR EL LISTADO DINAMICO    *
      *                   DE TABLAS                                    *
      *    IDTABLA        NOMBRE DE LA TABLA PARA EL LISTADO DINAMICO  *
      *    POCICION-COM   POSICION DENTRO DE LA COMMAREA DE LA APLICA  *
      *                   CION PARA LA SENTENCIA WHERE                 *
      *    LONGITUD-COM   LONGITUD DENTRO DE LA COMMAREA DE LA APLICA  *
      *                   CION PARA LA SENTENCIA WHERE                 *
      * LONDATA           LONG.   DATOS PROPIOS ADICIONALES APLIC.     *
      * PTRDATA           POINTER DATOS PROPIOS ADICIONALES APLIC.     *
      * PTRDATA-VACIO     <HIGH-VALUES> NO HAY DATOS                   *
      * RED-EXTERNA       OPERAC CON RED-EXTERNA   -> NO LNK QC1CSAL   *
      * PTR-MEN-INT       POINTER A MENSAJE INTERNO                    *
      * PTR-NOARQ         PUNTERO PARA DATOS NO ALTAMIRA               *
      * FILLER            RELLENO PARA FUTURAS INCORPORACIONES         *
      *                   **---------------------------------------**  *
      *                   **--  DATOS DE SIGUIENTE TRANSACCION   --**  *
      *                   **---------------------------------------**  *
      * PROXIMA-TRN                                                    *
      * CODTRAN-SIG       TRANSACCION A LA QUE CEDERLE EN CONTROL      *
      * CODTRAN-SIG-ULTI  <'ULTI'> A LA ULTIMA QUE NOS DIO CONTROL     *
      * CODTRAN-SIG-SAME  <'SAME'> A LA QUE ESTA EN PANTALLA           *
      * CODTRAN-SIG-MENU  <'MENU'> AL MENU PRINCIPAL                   *
      * CODTRAN-SIG-VACIO <SPACES> A NINGUNA                           *
      * AUTOMATICA        IND. SI LA SIG. TRANS. SE ARRANCA AUTOMATI   *
      * AUTOMATICA-SI     <'S'> SIGUIENTE ARRANCADA AUTOMATICAMENTE    *
      * AUTOMATICA-NO     <'N'> SIGUIENTE NO ARRANCADA AUTOMATICAMENTE *
      * ACCION            ACCION  (PRG=PROGRAMA / TER=TERMINAL)        *
      * ACCION-PROGRAMA   <'PRG'>        PROGRAMA                      *
      * ACCION-TERMINAL   <'TER', '   '> TERMINAL                      *
      * CADENA            CADENA                                       *
      * CADENA-INICIO     <'I'> INICIO     DE CADENA                   *
      * CADENA-ANADIR     <'A'> A-ADIR     A  CADENA                   *
      * CASO-CAD          CASO AL RETROCEDER EN LA CADENA              *
      * DATOS-CAD         DATOS PROPIOS ENTRADA AL RETROC. CADENA      *
      *                   **---------------------------------------**  *
      *                   **------  SALIDA DE LA APLICACION  ------**  *
      *                   **------     DATOS DEL MENSAJE     ------**  *
      *                   **---------------------------------------**  *
      * SALIDA-TERM                                                    *
      * COD-ERROR         CODIGO DE ERROR       (' ' SI BIEN)          *
      * COD-ERROR-VACIO   <SPACES> NO HAY CODIGO DE ERROR              *
      * COD-AVISO1        CODIGO DE AVISO 1                            *
      * COD-AVISO1-VACIO  <SPACES> NO HAY CODIGO DE AVISO1             *
      * COD-AVISO2        CODIGO DE AVISO 2                            *
      * COD-AVISO2-VACIO  <SPACES> NO HAY CODIGO DE AVISO2             *
      * VAR1-ERROR        VARIABLE-1 A INCLUIR EN MENSAJE ERROR        *
      * VAR2-ERROR        VARIABLE-2 A INCLUIR EN MENSAJE ERROR        *
      * VAR1-AVISO1       VARIABLE-1 A INCLUIR EN MENSAJE AVISO 1      *
      * VAR2-AVISO1       VARIABLE-2 A INCLUIR EN MENSAJE AVISO 1      *
      * VAR1-AVISO2       VARIABLE-1 A INCLUIR EN MENSAJE AVISO 2      *
      * VAR2-AVISO2       VARIABLE-2 A INCLUIR EN MENSAJE AVISO 2      *
      * IMPORTE-DISP      IMPORTE DEL DISPENSADOR                      *
      * DIARIO-LOCAL      CAMPO A ACTUALIZAR EN DIARIO ELEC. LOCAL     *
      * TIPO-SALIDA       PANTALLA A ENVIAR AL TERMINAL                *
      * TIPO-SALIDA-ENT   <'E'> LA DE ENTRADA                          *
      * TIPO-SALIDA-SAL   <'S'> LA DE SALIDA                           *
      * TIPO-SALIDA-PAG   <'P'> PAGINACION DE ARQUITECTURA             *
      * TIPO-SALIDA-NO    <' '> NINGUNA                                *
      * COPY-OUT          NOMBRE COPY (FDF) SALIDA  - COPY-OUT         *
      * PANEL-OUT         NOMBRE PANEL BMS DE SALIDA                   *
      * DESTINOS          INFORMACION DE SALIDAS DESTINO TERMINAL      *
      * DESTINO           PREFIJO TS QUE CONTIENE SALIDA ( PF,  DC)    *
      * IND-PANDOC        IND. DESTINO: 'P' PANTALLA, 'D' DOCUMENTO    *
      * NUM-DOCUM         NUMERO DE DOCUMENTO                          *
      * PRILIN-DOCUM      POSICION DE LA PRIMERA LINEA A DOCUMENTO     *
      * IMPRESO           CODIGO DE IMPRESO DE SALIDA                  *
      * IDIOMA            CODIGO DE IDIOMA DE SALIDA                   *
      *                   CATALAN = 'C'                                *
      *                   ESPA%OL = 'E'                                *
      *                   **---------------------------------------**  *
      *                   **------  SALIDA DE LA APLICACION  ------**  *
      *                   **-- DATOS PARA GESTION DE PAGINACION  --**  *
      *                   **---------------------------------------**  *
      * PAGINAR                                                        *
      * CONTENID          CONTENIDO GENERICO DEL LISTADO               *
      * SELEC-PERMIT                                                   *
      * SEL-PERMIT        DIGITOS A ADMITIR COMO CAMPO DE SELECCION    *
      * IND-VARSEL        IND. SE MULTISELECCION PERMITIDA             *
      * IND-VARSEL-SI     <'S'>    SE PERMITE SELECC. VARIAS LINEAS    *
      * IND-VARSEL-NO     <'N'> NO SE PERMITE SELECC. VARIAS LINEAS    *
      * MARGEN-FIJO       MARGEN A MANTENER EN DESPLAZAM. IZDA.        *
      * FKEY              TECLAS DE FUNCION PERMITIDAS (NO PAGIN.)     *
      * FKEY-NUM          NUMERO DE LA TECLA PERMITIDA                 *
      * FKEY-LIT          LITERAL TECLA A MOSTRAR EN PANEL             *
      * FKEY-SEL          <S>EXIGIDA SELEC. <N>NO PERMITIDA, < > NAD   *
      * IND-AVPAG         IND. DE DEVOLVER CONTROL CON PF8 AL LISTAD   *
      * IND-AVPAG-SI      <'S'> DEVUELVE CONTROL AL LISTADO CON PF8    *
      * IND-AVPAG-NO      <'N',' '> NO DEVUELVE CONTROL AL LISTADO PF8 *
      * IND-MOD-DATO      IND. DE MODIFICACION DE LINEA DE DATOS       *
      * IND-MOD-DATO-SI   <'S'>    SE PERMITE MODIFICAR LA LINEA DE D. *
      * IND-MOD-DATO-NO   <'N'> NO SE PERMITE MODIFICAR LA LINEA DE D. *
      * LINEA-PANT        LINEA DE PANTALLA. USADO POR EL PROGRAMA D   *
      *                   GESTION DE TS'S.                             *
      * COLUM-PANT        COLUM.DE PANTALLA. USADO POR EL PROGRAMA D   *
      *                   GESTION DE TS'S.                             *
      * NUM-LIN-CAB       NUMNERO DE LINEAS FIJAS EN EL SCROLL ARRIBA  *
      *                   Y ABAJO. (LINEAS DE CABECERA. SI NO SE IN-   *
      *                   FORMA, SE TOMA 1 POR DEFECTO).               *
      * IND-SCROLL-LAT    INDICADOR DE SCROLL LATERAL                  *
      * IND-SCROLL-SI     <'S' ' '> SE REALIZA SCROLL LATERAL          *
      * IND-SCROLL-NO     <'N'> NO SE REALIZA SCROLL LATERAL           *
      * NUM-ITEM-SELEC    NUMERO DE ITEM SELECCIONADO (EN EL CASO DE   *
      *                   SELECCION UNICA). EN CASO DE SELECCION MUL-  *
      *                   TIPLE, EL PRIMERO SELECCIONADO. ES UN CAMPO  *
      *                   DE SALIDA.                                   *
      * FILLER            RELLENO PARA FUTURAS INCORPORACIONES         *
      *                   **---------------------------------------**  *
      *                   **------  SALIDA DE LA APLICACION  ------**  *
      *                   **------       DATOS PARA          ------**  *
      *                   **------  ANALITICA Y ESTADISTICAS ------**  *
      *                   **---------------------------------------**  *
      * ANALITICA                                                      *
      * ENTIDAD-ANA       ENTIDAD DESTINO   ANALITICA                  *
      * CENTRO-ANA        CENTRO DESTINO    ANALITICA                  *
      * PRODUCTO-ANA      CLAVE PRODUCTO    APLICACION                 *
      * CLIENTE-ANA       CLIENTE           ANALITICA                  *
      * IMPORTE-ANA       IMPORTE           ANALITICA                  *
      * SUBPROD-ANA       SUBGRUPO PARA     ANALITICA                  *
      * FINALID-ANA       FINALIDAD         ANALITICA                  *
      * GARANTIA-ANA      GARANTIA          ANALITICA                  *
      * SUB-CLASIF        SUB-CLASIFICACION TRANSACCION                *
      * TIOPER            TIPO DE OPERACION REALIZADA                  *
      * TIOPER-ALTA       <'A'>  ALTA                                  *
      * TIOPER-BAJA       <'B'>  BAJA                                  *
      * TIOPER-MODIF      <'M'>  MODIFICACION                          *
      * TIOPER-CONSULTA   <'C'>  CONSULTA                              *
      * TIOPER-EDICION    <'E'>  EDICION                               *
      * TIOPER-PETICION   <'P'>  PETICION AL BATCH                     *
      * TIOPER-ENTSAL     <'O'>  OPERACION DE ENTRADA / SALIDA         *
      * TIOPER-NADA       <'N'>  NADA                                  *
      * CONTABLE          OPERACION REALIZADA ES CONTABLE (SI/NO)      *
      * CONTABLE-SI       <'S'> OPERACION    CONTABLE                  *
      * CONTABLE-NO       <'N'> OPERACION NO CONTABLE                  *
      * DATOS-APLIC       DATOS DE LIBRE USO PARA LA APLICACION        *
      * FILLER            RELLENO PARA FUTURAS INCORPORACIONES         *
      *                   **---------------------------------------**  *
      *                   **-SALIDA APLIC: DATOS ERROR INESPERADO -**  *
      *                   **---------------------------------------**  *
      * INFORMA-ERROR                                                  *
      * OBJETO-ERROR      NOMBRE DEL OBJETO DONDE SE PRODUJO ERROR     *
      * SQLCODE           CODIGO DE RETORNO DEL DB2                    *
      * SQLERRM           INFORMACION ERROR DB2                        *
      * EIBFN             ULTIMA FUNCION CICS                          *
      * EIBRSRCE          ULTIMO RECURSO CICS                          *
      * EIBRCODE          CODIGO DE RESPUESTA DEL CICS                 *
      * EIBRESP1          CONDICION PRODUCIDA POR ULT.FUNCION CICS     *
      * EIBRESP2          INFORMACION ADICIONAL A EIBRESP1             *
      * FILLER            RELLENO PARA FUTURAS INCORPORACIONES         *
      *                                                                *
      ******************************************************************

       02 QGECCAA.
      *          **---------------------------------------**
      *          **------  ENTRADA A LA APLICACION  ------**
      *          **----------- DATOS GENERALES -----------**
      *          **---------------------------------------**
          05 CAA-DATOS-GRLES.
             10 CAA-ENTIDAD                 PIC X(04).
             10 CAA-ENTIDAD-9               REDEFINES CAA-ENTIDAD
                                            PIC 9(04).
             10 CAA-CENTRO-CONT             PIC X(04).
             10 CAA-CENTRO-CONT-9           REDEFINES CAA-CENTRO-CONT
                                            PIC 9(04).
             10 CAA-NETNAME-CONT            PIC X(08).
             10 CAA-TERMINAL-CONT           PIC X(04).
             10 CAA-FECHA-CONT              PIC X(08).
             10 CAA-FECHA-CONT-9            REDEFINES CAA-FECHA-CONT
                                            PIC 9(08).
             10 CAA-FECHA-CONT2             PIC X(10).
             10 CAA-FECHA-CONTED            PIC X(10).
             10 CAA-FECHA-OPER              PIC X(08).
             10 CAA-FECHA-OPER-9            REDEFINES CAA-FECHA-OPER
                                            PIC 9(08).
             10 CAA-FECHA-OPER2             PIC X(10).
             10 CAA-FECHA-OPERED            PIC X(10).
             10 CAA-FECHA-TRANS             PIC X(08).
             10 CAA-FECHA-TRANS-9           REDEFINES CAA-FECHA-TRANS
                                            PIC 9(08).
             10 CAA-FECHA-TRANS2            PIC X(10).
             10 CAA-FECHA-TRANSED           PIC X(10).
             10 CAA-HORA-TRANS              PIC X(06).
             10 CAA-HORA-TRANS-9            REDEFINES CAA-HORA-TRANS
                                            PIC 9(06).
             10 CAA-HORA-TRANSED            PIC X(08).
             10 CAA-NETNAME                 PIC X(08).
             10 CAA-TERMINAL                PIC X(04).
             10 CAA-USERID                  PIC X(08).
             10 CAA-SESION                  PIC X(01).
                88 CAA-88-SESION-MANANA     VALUE 'M'.
                88 CAA-88-SESION-TARDE      VALUE 'T'.
             10 CAA-TIPO-TERM               PIC X(02).
                88 CAA-88-TIPO-TERM-4700    VALUE '11'.
                88 CAA-88-TIPO-TERM-5935    VALUE '12'.
                88 CAA-88-TIPO-TERM-3270    VALUE '20', '28', '29'.
                88 CAA-88-TIPO-TERM-PS-1    VALUE '13'.
                88 CAA-88-TIPO-TERM-PS-2    VALUE '14'.
                88 CAA-88-TIPO-TERM-PS-3    VALUE '15'.
                88 CAA-88-TIPO-TERM-PS-4    VALUE '17'.
                88 CAA-88-TIPO-TERM-PS-5    VALUE '18'.
                88 CAA-88-TIPO-TERM-PS-6    VALUE '19'.
                88 CAA-88-TIPO-TERM-PS-8    VALUE '25'.
                88 CAA-88-TIPO-TERM-PS-9    VALUE '26'.
                88 CAA-88-TIPO-TERM-PS  VALUE '13' '14' '15' '16' '17'
                                              '18' '19' '21' '25' '26'.
                88 CAA-88-TIPO-TERM-VIDEOTEX VALUE '16'.
                88 CAA-88-TIPO-TERM-DIEBOLD  VALUE '39'.
                88 CAA-88-TIPO-TERM-CAJERO   VALUE '40'.
                88 CAA-88-TIPO-TERM-CECA1    VALUE '41'.
                88 CAA-88-TIPO-TERM-CECA2    VALUE '42'.
                88 CAA-88-TIPO-TERM-CECA     VALUE '41' '42'.
                88 CAA-88-TIPO-TERM-TPV      VALUE '43'.
                88 CAA-88-TIPO-TERM-COR-E    VALUE '44'.
                88 CAA-88-TIPO-TERM-COR-S    VALUE '45'.
                88 CAA-88-TIPO-TERM-COR-P    VALUE '52'.
                88 CAA-88-TIPO-TERM-CORONA   VALUE '44' '45' '52'.
                88 CAA-88-TIPO-TERM-SEMP-AE  VALUE '46'.
                88 CAA-88-TIPO-TERM-SEMP-AA  VALUE '47'.
                88 CAA-88-TIPO-TERM-SEMP-RF  VALUE '48'.
                88 CAA-88-TIPO-TERM-SEMP-AF  VALUE '49'.
                88 CAA-88-TIPO-TERM-SEMP-F   VALUE '50'.
                88 CAA-88-TIPO-TERM-ENTRADA  VALUE '53'.
                88 CAA-88-TIPO-TERM-4B-REAL  VALUE '54'.
                88 CAA-88-TIPO-TERM-4B-FIC   VALUE '55'.
                88 CAA-88-TIPO-TERM-4B       VALUE '54' '55'.
                 88 CAA-88-TIPO-TERM-PROSA-F  VALUE '90'.
                 88 CAA-88-TIPO-TERM-PROSA-SA VALUE '91'.
                 88 CAA-88-TIPO-TERM-PROSA-SP VALUE '92'.
                 88 CAA-88-TIPO-TERM-PROSA-AP VALUE '93'.
                 88 CAA-88-TIPO-TERM-PROSA   VALUE '90' '91' '92' '93'.
                 88 CAA-88-TIPO-TERM-MASTER-F VALUE '56'.
                 88 CAA-88-TIPO-TERM-MASTER-S1 VALUE '57'.
                 88 CAA-88-TIPO-TERM-MASTER-S2 VALUE '58'.
                 88 CAA-88-TIPO-TERM-MASTER  VALUE '56' '57' '58' '80'.
                 88 CAA-88-TIPO-TERM-SEMP-S1  VALUE '60'.
                 88 CAA-88-TIPO-TERM-SEMP-S2  VALUE '61'.
                 88 CAA-88-TIPO-TERM-SEMP
                                             VALUE '46' '47' '48' '49'
                                                   '50' '60' '61'.
                  88 CAA-88-TIPO-TERM-VISA-E   VALUE '62'.
                  88 CAA-88-TIPO-TERM-VISA-SA  VALUE '63'.
                  88 CAA-88-TIPO-TERM-VISA-SP  VALUE '64'.
                  88 CAA-88-TIPO-TERM-VISA-F   VALUE '65'.
                  88 CAA-88-TIPO-TERM-VISA-S1  VALUE '88'.
                  88 CAA-88-TIPO-TERM-VISA-S2  VALUE '89'.
                  88 CAA-88-TIPO-TERM-VISA    VALUE '62' '63' '64' '65'
                                                    '88' '89' '82'.
                  88 CAA-88-TIPO-TERM-TECB-E   VALUE '66'.
                  88 CAA-88-TIPO-TERM-TECB-S1  VALUE '67'.
                  88 CAA-88-TIPO-TERM-TECB-F   VALUE '69'.
                  88 CAA-88-TIPO-TERM-TECB    VALUE '66' '67' '69'.
                  88 CAA-88-TIPO-TERM-ASCR-F   VALUE '87'.
                  88 CAA-88-TIPO-TERM-ASCR-SA  VALUE '71'.
                  88 CAA-88-TIPO-TERM-ASCR-SP  VALUE '94'.
                  88 CAA-88-TIPO-TERM-ASCR    VALUE '87' '71' '94'.
                  88 CAA-88-TIPO-TERM-OPT      VALUE '68'.
                  88 CAA-88-TIPO-TERM-S7B-ATM  VALUE '70'.
                  88 CAA-88-TIPO-TERM-VZLA-ATM VALUE '72'.
                  88 CAA-88-TIPO-TERM-VZLA-POS VALUE '73'.
                  88 CAA-88-TIPO-TERM-TAND-ALT VALUE '74'.
                  88 CAA-88-TIPO-TERM-TAND-CNX VALUE '75'.
                  88 CAA-88-TIPO-TERM-TAND     VALUE '74' '75'.
                  88 CAA-88-TIPO-TERM-VISA-VZLA VALUE '82'.
                  88 CAA-88-TIPO-TERM-MAST      VALUE '80'.
                  88 CAA-88-TIPO-TERM-INTE     VALUE '79'.
                  88 CAA-88-TIPO-TERM-CONV     VALUE '78'.
                  88 CAA-88-TIPO-TERM-DEC-ANI  VALUE '70' '72' '78'.
                  88 CAA-88-TIPO-TERM-DEC-IS85 VALUE '80'.
                  88 CAA-88-TIPO-TERM-DEC-IBVZ VALUE '73'.
                  88 CAA-88-TIPO-TERM-DEC-BA24 VALUE '74' '75'.
                  88 CAA-88-TIPO-TERM-PVI      VALUE '83'.
                  88 CAA-88-TIPO-TERM-PS7B     VALUE '84'.
                  88 CAA-88-TIPO-TERM-VISANET  VALUE '85'.
                  88 CAA-88-TIPO-TERM-CIRRUS   VALUE '86'.
             10 CAA-CICS                    PIC X(04).
             10 CAA-CODTRAN                 PIC X(04).
             10 CAA-TIPO-PROCESO            PIC X(01).
                88 CAA-88-TIPO-PROCESO-ONLI VALUE 'O'.
                88 CAA-88-TIPO-PROCESO-AUTO VALUE 'A' 'U'.
                88 CAA-88-TIPO-PROCESO-TELE VALUE 'T'.
                88 CAA-88-TIPO-PROCESO-OFF  VALUE 'F'.
             10 CAA-ESTADO-APLIC            PIC X(01).
                88 CAA-88-ACTIVA            VALUE 'A'.
                88 CAA-88-DESACTIVA         VALUE 'D'.
                88 CAA-88-CAMBIO-SESION     VALUE 'C'.
                88 CAA-88-RECUPERACION      VALUE 'R'.
             10 CAA-NUM-REINTENTOS          PIC S9(4) COMP.
             10 CAA-NUM-SECUE               PIC X(05).
             10 CAA-NUM-SECUE-9  REDEFINES CAA-NUM-SECUE
                                            PIC 9(05).
             10 CAA-VERSION-GAT             PIC X(04).
             10 CAA-IDIOMA-TERM             PIC X(01).
                88 CAA-88-IDITERM-NADA      VALUE ' '.
                88 CAA-88-IDITERM-CAT       VALUE 'C'.
                88 CAA-88-IDITERM-ESP       VALUE 'E'.
      *ASTAAU0002-I UNIFICACION DE COMMAREAS
      *      10 FILLER                      PIC X(18).
             10 CAA-PTR-CIA                 POINTER.
             10 FILLER                      PIC X(03).
             10 CAA-SUCURSAL-USERID         PIC 9(03).
             10 CAA-FECHA-VALOR             PIC X(08).
             10 CAA-FECHA-VALOR-9           REDEFINES CAA-FECHA-VALOR
                                                 PIC 9(08).
      *ASTAAU0002-F
      *          **---------------------------------------**
      *          **------  ENTRADA A LA APLICACION  ------**
      *          **---------- DATOS DEL MENSAJE ----------**
      *          **---------------------------------------**
          05 CAA-ENTRADA-TERM.
             10 CAA-TECLA                   PIC X(02).
             10 CAA-CAJERO                  PIC X(01).
                88 CAA-88-CAJERO-A          VALUE 'A'.
                88 CAA-88-CAJERO-B          VALUE 'B'.
             10 CAA-MOD-TAG                 PIC X(01).
                88 CAA-88-MOD-TAG-SI        VALUE 'S'.
                88 CAA-88-MOD-TAG-NO        VALUE 'N'.
                88 CAA-88-MOD-TAG-NADA      VALUE ' '.
             10 CAA-PTR-COPYIN              POINTER.
             10 CAA-PAPEL                   PIC X(1).
                88 CAA-88-PAPEL-SI          VALUE 'S'.
                88 CAA-88-PAPEL-NO          VALUE 'N'.
             10 CAA-DIVISA-ENT             PIC X(03).
             10 CAA-TYPE-INPUT             PIC X(01).
                       88 CAA-88-TYPE-BMS              VALUE 'B'.
                       88 CAA-88-TYPE-COPY             VALUE 'C' 'G'.
                       88 CAA-88-TYPE-COPY-MENOS2K     VALUE 'C'.
                       88 CAA-88-TYPE-COPY-MAS2K       VALUE 'G'.
             10 FILLER                     PIC X(05).
      *          **---------------------------------------**
      *          **---  AUTORIZACIONES (ENTRADA/SALIDA) --**
      *          **---------------------------------------**
          05 CAA-AUTORIZACION.
             10 CAA-AUTORIZ                 PIC X(0150).
             10 CAA-AUTORIZ-O               REDEFINES CAA-AUTORIZ
                                            OCCURS 10 TIMES.
                15 CAA-CODERR-AUT           PIC X(07).
                15 CAA-SITUACION-AUT        PIC X(08).
             10 CAA-IND-AUTO                PIC X(01).
                88 CAA-88-IND-AUTO-SI       VALUE 'S'.
                88 CAA-88-IND-AUTO-NO       VALUE 'N' ' '.
      *ASTAAU0002-I UNIFICACION DE COMMAREAS
      *  10 CAA-IMPORTE-AUTO            PIC S9(13)      COMP-3.
      *  10 CAA-IMPORTE-AUTO            PIC S9(13)V99   COMP-3.
         10 CAA-IMPORTE-AUTO            PIC S9(11)V99   COMP-3.
      *ASTAAU0002-F UNIFICACION DE COMMAREAS
             10 CAA-REFER-AUTO              PIC X(20).
             10 CAA-ERROR-AUTO-ALT          PIC X(1).
                88 CAA-88-ERROR-AUTO-ALT-SI VALUE 'S'.
      *ASTAAU0002-I UNIFICACION DE COMMAREAS
             10 FILLER                      PIC X(21).
      *      10 FILLER                      PIC X(20).
      *ASTAAU0002-F
      *          **---------------------------------------**
      *          **---  AUTORIZACIONES (PTRO AUXILIAR)  --**
      *          **---------------------------------------**
          05 CAA-AUTORIZACION-AUX.
             10 CAA-NUMSEQ                  PIC S9(07) COMP-3.
             10 CAA-NUMSEQ-R                REDEFINES  CAA-NUMSEQ
                                            PIC X(04).
             10 CAA-PTR-CUA                 POINTER.
             10 CAA-PTR-CUA-R               REDEFINES CAA-PTR-CUA
                                            PIC X(04).
      *          **---------------------------------------**
      *          **-  DATOS CONVERSACION ENTRADA/SALIDA  -**
      *          **---------------------------------------**
          05 CAA-CONVERS.
             10 CAA-ESTADO                  PIC X(01).
                88 CAA-88-ESTADO-INICIO     VALUE 'I'.
                88 CAA-88-ESTADO-CONTIN     VALUE 'C'.
                88 CAA-88-ESTADO-CONFIR     VALUE 'X'.
             10 CAA-CASO                    PIC X(01).
             10 CAA-DATOS                   PIC X(30).
             10 CAA-DATOS-RED     REDEFINES CAA-DATOS.
                15 FILLER                   PIC X(11).
                15 CAA-LISTADO-DIN.
                   20 CAA-IDTABLA           PIC X(10).
                   20 CAA-POSICION-COM      PIC 9(4).
                   20 CAA-LONGITUD-COM      PIC 9(4).
                15 FILLER                   PIC X(1).
             10 CAA-LONDATA                 PIC S9(05)      COMP-3.
             10 CAA-PTRDATA                 POINTER.
      *
             10 CAA-RED-EXTERNA             PIC X(1).
                88 CAA-88-RED-EXTERNA       VALUE 'S'.
      *@PASTAMP10
                88 CAA-88-MATAR-TRAN        VALUE 'S'.
      *@FASTAMP10
             10 CAA-PTR-MEN-INT             POINTER.
             10 CAA-PTR-MEN-INT-H
                        REDEFINES  CAA-PTR-MEN-INT PIC X(4).
                88 CAA-88-PTR-MEN-VACIO VALUE HIGH-VALUES.
             10 CAA-PTR-NOARQ               POINTER.
             10 FILLER                      PIC X(01).
      *          **---------------------------------------**
      *          **--  DATOS DE SIGUIENTE TRANSACCION   --**
      *          **---------------------------------------**
          05 CAA-PROXIMA-TRN.
             10 CAA-CODTRAN-SIG             PIC X(04).
                88 CAA-88-CODTRAN-SIG-ULTI  VALUE 'ULTI'.
                88 CAA-88-CODTRAN-SIG-SAME  VALUE 'SAME'.
                88 CAA-88-CODTRAN-SIG-MENU  VALUE 'MENU'.
                88 CAA-88-CODTRAN-SIG-VACIO VALUE SPACES.
             10 CAA-AUTOMATICA              PIC X(01).
                88 CAA-88-AUTOMATICA-SI     VALUE 'S'.
                88 CAA-88-AUTOMATICA-NO     VALUE 'N' ' '.
             10 CAA-ACCION                  PIC X(03).
                88 CAA-88-ACCION-PROGRAMA   VALUE 'PRG'.
                88 CAA-88-ACCION-TERMINAL   VALUE 'TER', '   '.
             10 CAA-CADENA                  PIC X(01).
                88 CAA-88-CADENA-INICIO     VALUE 'I'.
                88 CAA-88-CADENA-ANADIR     VALUE 'A'.
             10 CAA-CASO-CAD                PIC X(01).
             10 CAA-DATOS-CAD               PIC X(30).
      *          **---------------------------------------**
      *          **------  SALIDA DE LA APLICACION  ------**
      *          **------     DATOS DEL MENSAJE     ------**
      *          **---------------------------------------**
          05 CAA-SALIDA-TERM.
             10 CAA-COD-ERROR               PIC X(07).
                88 CAA-88-COD-ERROR-VACIO   VALUE SPACES.
             10 CAA-COD-AVISO1              PIC X(07).
                88 CAA-88-COD-AVISO1-VACIO  VALUE SPACES.
             10 CAA-COD-AVISO2              PIC X(07).
                88 CAA-88-COD-AVISO2-VACIO  VALUE SPACES.
             10 CAA-VAR1-ERROR              PIC X(20).
             10 CAA-VAR2-ERROR              PIC X(20).
             10 CAA-VAR1-AVISO1             PIC X(20).
             10 CAA-VAR2-AVISO1             PIC X(20).
             10 CAA-VAR1-AVISO2             PIC X(20).
             10 CAA-VAR2-AVISO2             PIC X(20).
             10 CAA-IMPORTE-DISP            PIC S9(13)      COMP-3.
             10 CAA-DIARIO-LOCAL            PIC X(80).
             10 CAA-TIPO-SALIDA             PIC X(01).
                88 CAA-88-TIPO-SALIDA-ENT   VALUE 'E'.
                88 CAA-88-TIPO-SALIDA-SAL   VALUE 'S'.
                88 CAA-88-TIPO-SALIDA-PAG   VALUE 'P'.
                88 CAA-88-TIPO-SALIDA-NO    VALUE ' '.
             10 CAA-COPY-OUT                PIC X(08).
             10 CAA-PANEL-OUT               PIC X(08).
             10 CAA-DESTINOS                PIC X(0075).
             10 CAA-DESTINOS-O              REDEFINES CAA-DESTINOS
                                            OCCURS 5  TIMES.
                15 CAA-DESTINO              PIC X(04).
                15 CAA-IND-PANDOC           PIC X(01).
                15 CAA-NUM-DOCUM            PIC X(01).
                15 CAA-PRILIN-DOCUM         PIC X(02).
                15 CAA-PRILIN-DOCUM-9       REDEFINES CAA-PRILIN-DOCUM
                                            PIC 9(02).
                15 CAA-IMPRESO              PIC X(06).
                15 CAA-IDIOMA               PIC X(01).
      *          **---------------------------------------**
      *          **------  SALIDA DE LA APLICACION  ------**
      *          **-- DATOS PARA GESTION DE PAGINACION  --**
      *          **---------------------------------------**
          05 CAA-PAGINAR.
             10 CAA-CONTENID                PIC X(50).
             10 CAA-SELEC-PERMIT            PIC X(0010).
             10 CAA-SELEC-PERMIT-O          REDEFINES CAA-SELEC-PERMIT
                                            OCCURS 10 TIMES.
                15 CAA-SEL-PERMIT           PIC X(01).
             10 CAA-IND-VARSEL              PIC X(01).
                88 CAA-88-IND-VARSEL-SI     VALUE 'S'.
                88 CAA-88-IND-VARSEL-NO     VALUE 'N'.
             10 CAA-MARGEN-FIJO             PIC X(02).
             10 CAA-MARGEN-FIJO-9           REDEFINES CAA-MARGEN-FIJO
                                            PIC 9(02).
             10 CAA-FKEY                    PIC X(0072).
             10 CAA-FKEY-O                  REDEFINES CAA-FKEY
                                            OCCURS 8  TIMES.
                15 CAA-FKEY-NUM             PIC X(02).
                15 CAA-FKEY-LIT             PIC X(06).
                15 CAA-FKEY-SEL             PIC X(01).
             10 CAA-IND-AVPAG               PIC X(01).
                88 CAA-88-IND-AVPAG-SI      VALUE 'S'.
                88 CAA-88-IND-AVPAG-NO      VALUE 'N' ' '.
             10 CAA-IND-MOD-DATO            PIC X(01).
                88 CAA-88-IND-MOD-DATO-SI   VALUE 'S'.
                88 CAA-88-IND-MOD-DATO-NO   VALUE 'N'.
             10 CAA-LINEA-PANT              PIC S9(05)      COMP-3.
             10 CAA-COLUM-PANT              PIC S9(05)      COMP-3.
             10 CAA-NUM-LIN-CAB             PIC X(02).
             10 CAA-NUM-LIN-CAB-9  REDEFINES CAA-NUM-LIN-CAB
                                            PIC 9(02).
             10 CAA-IND-SCROLL-LAT          PIC X(01).
                88 CAA-88-IND-SCROLL-SI     VALUE 'S' ' '.
                88 CAA-88-IND-SCROLL-NO     VALUE 'N'.
             10 CAA-NUM-ITEM-SELEC          PIC S9(05)    COMP-3.
             10 CAA-MAX-LISTAR             PIC S9(04)    COMP.
             10 CAA-IND-VAR-HEADER         PIC X(01).
                88 CAA-88-IND-VAR-HEA-SI               VALUE 'S'.
                88 CAA-88-IND-VAR-HEA-NO               VALUE 'N' ' '.
             10 CAA-COPYOUT-GTS            PIC X(8).
          05 FILLER.
             10 FILLER                     PIC X(1).
             10 FILLER                     PIC X(1).
             10 FILLER                     PIC X(4).
      *          **---------------------------------------**
      *          **------  SALIDA DE LA APLICACION  ------**
      *          **------       DATOS PARA          ------**
      *          **------  ANALITICA Y ESTADISTICAS ------**
      *          **---------------------------------------**
          05 CAA-ANALITICA.
             10 CAA-ENTIDAD-ANA             PIC X(04).
             10 CAA-ENTIDAD-ANA-9           REDEFINES CAA-ENTIDAD-ANA
                                            PIC 9(04).
             10 CAA-CENTRO-ANA              PIC X(04).
             10 CAA-CENTRO-ANA-9            REDEFINES CAA-CENTRO-ANA
                                            PIC 9(04).
             10 CAA-PRODUCTO-ANA            PIC X(20).
             10 CAA-ANALITIC1-GRUPO.
                15 CAA-CLIENTE-ANA          PIC X(08).
                15 CAA-IMPORTE-ANA          PIC S9(13)V9(2) COMP-3.
                15 CAA-SUBPROD-ANA          PIC X(04).
                15 CAA-FINALID-ANA          PIC X(03).
                15 CAA-GARANTIA-ANA         PIC X(01).
                15 CAA-SUB-CLASIF           PIC X(03).
             10 CAA-ANALITICA-1  REDEFINES CAA-ANALITIC1-GRUPO
                                            PIC X(27).
             10 CAA-TIOPER                  PIC X(01).
                88 CAA-88-TIOPER-ALTA       VALUE 'A'.
                88 CAA-88-TIOPER-BAJA       VALUE 'B'.
                88 CAA-88-TIOPER-MODIF      VALUE 'M'.
                88 CAA-88-TIOPER-CONSULTA   VALUE 'C'.
                88 CAA-88-TIOPER-EDICION    VALUE 'E'.
                88 CAA-88-TIOPER-PETICION   VALUE 'P'.
                88 CAA-88-TIOPER-ENTSAL     VALUE 'O'.
                88 CAA-88-TIOPER-NADA       VALUE 'N'.
             10 CAA-CONTABLE                PIC X(01).
                88 CAA-88-CONTABLE-SI       VALUE 'S'.
                88 CAA-88-CONTABLE-NO       VALUE 'N'.
             10 CAA-DATOS-APLIC             PIC X(20).
             10 CAA-ANALITICA-2             PIC X(30).
      *          **---------------------------------------**
      *          **-SALIDA APLIC: DATOS ERROR INESPERADO -**
      *          **---------------------------------------**
             10 CAA-AREA-ALTAMIRA-II        REDEFINES CAA-ANALITICA-2.
                15 CAA-CANAL                PIC X(02).
                15 CAA-STARTCODE            PIC X(02).
                   88 CAA-88-DPL-NO-SYNC            VALUE 'D '.
                   88 CAA-88-DPL-SI-SYNC            VALUE 'DS'.
                   88 CAA-88-DPL                    VALUE 'D ' 'DS'.
                   88 CAA-88-COLA-TD                VALUE 'QD'.
                   88 CAA-88-START-SIN-DATOS        VALUE 'S '.
                   88 CAA-88-START-CON-DATOS        VALUE 'SD'.
                   88 CAA-88-FEPI                   VALUE 'SZ'.
                   88 CAA-88-TERMINAL               VALUE 'TD'.
                   88 CAA-88-SYNCPOINT-PERMITIDO    VALUE
                                       'DS' 'QD' 'S ' 'SD' 'SZ' 'TD'.
                   88 CAA-88-SYNCPOINT-NO-PERMITIDO VALUE 'D '.
                15 CAA-USERID-LOGON         PIC X(08).
                15 CAA-PREFORMATEAR         PIC X(01).
                    88 CAA-88-PREFORMATEAR-SI       VALUE 'Y'.
                    88 CAA-88-PREFORMATEAR-NO       VALUE 'N'.
                15 CAA-ENTORNO              PIC X.
      *@PASTAMP01
      *         15 FILLER                   PIC X(07).
                15 CAA-CANAL-DEST           PIC X(02).
                15 FILLER                   PIC X(05).
      *@FASTAMP01
                15 CAA-PACKS                PIC X(09).

          05 CAA-INFORMA-ERROR.
             10 CAA-OBJETO-ERROR            PIC X(08).
             10 CAA-SQLCODE                 PIC S9(09)      COMP.
             10 CAA-SQLERRM                 PIC X(70).
             10 CAA-EIBFN                   PIC X(02).
             10 CAA-EIBRSRCE                PIC X(08).
             10 CAA-EIBRCODE                PIC X(06).
             10 CAA-EIBRESP1                PIC S9(09)      COMP.
             10 CAA-EIBRESP2                PIC S9(09)      COMP.
      *ASTAAU0002-I UNIFICACION DE COMMAREAS
      *      10 FILLER                      PIC X(30).
             10 CAA-NIVEL-GENERAL-LOG       PIC 9.
             10 CAA-NIVEL-GENERAL-AUT       PIC 9.
             10 CAA-ABEND-CODE              PIC X(7).
             10 CAA-CANAL-OPERACION         PIC X(2).
             10 CAA-PTR-SWITCH              POINTER.
             10 FILLER                      PIC X(15).
      *ASTAAU0002-F

      ******************************************************************
      *                                                                *
      *         LONGITUD TOTAL DEL REGISTRO EN BYTES :   1225          *
      *                                                                *
      ******************************************************************
