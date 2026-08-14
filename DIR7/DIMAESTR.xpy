      *=======  MAESTRO DE CARTERA A USARSE EN LINEA  ========*         00000010
      *  NOMBRE DEL BOOK      : DIMAESTR                      *         00000020
      *  CLAVE DEL ARCHIVO    : CODIGO DEL CLIENTE            *         00000030
      *  LONGITUD DE CLAVE    : 12                            *         00000040
      *  LONGITUD DE REGISTRO : 600                           *         00000050
      *  NOMBRE FISICO        : PLDIQ.DM.DIMAESTR             *         00000060
      *  NOMBRE CICS          : DIMAESTR                      *         00000070
      *  CODIGO PIDA          : 12                            *         00000080
      *=======================================================*         00000090
      *********************************************************
      * LOG      FECHA    DESCRIPCION
      *--------- -------- ---------------------------------
      *@EVE-0001 24-12-09 SE MODIFICA CAMPO TIPO DE CREDITO.  *
      *
       01  REG-MAESTRO-CARTERA.                                         00000100
           02  MCAR-KEYMAE.                                             00000110
               04  MCAR-CODAPL.                                         00000120
                   06  MCAR-CODIGO        PIC 9(10).                    00000130
                   06  MCAR-CODCOD  REDEFINES  MCAR-CODIGO.             00000140
                       08  MCAR-CODOFI    PIC 9(03).                    00000150
                       08  MCAR-CODMON    PIC 9(01).                    00000160
                       08  MCAR-CODCLI    PIC 9(06).                    00000170
                   06  MCAR-TIPCTA        PIC 9(02).                    00000180
      *---------------------------------------------   12  --------     00000190
           02  MCAR-DATOS-GENERALES.                                    00000200
               04  MCAR-OFICIN            PIC 9(03).                    00000210
               04  MCAR-NOMBRE            PIC X(40).                    00000220
               04  MCAR-DIREC1            PIC X(40).                    00000230
               04  MCAR-DIREC2            PIC X(20).                    00000240
               04  MCAR-NATJUR            PIC X(01).                    00000250
               04  MCAR-CODGRE.                                         00000260
                   06  MCAR-GREMIO        PIC 9(02).                    00000270
                   06  MCAR-ACTECO        PIC 9(04).                    00000280
                   06  MCAR-FILGRE        PIC X(04).                    00000290
               04  MCAR-CODRUC            PIC X(10).                    00000300
               04  MCAR-CODFUN            PIC 9(03).                    00000310
               04  MCAR-PORGAR            PIC 9(03).                    00000320
               04  MCAR-CLASIF            PIC 9(02).                    00000330
               04  MCAR-FECAPE.                                         00000340
                   06 MCAR-ANOAPE         PIC 9(04).                    00000350
                   06 MCAR-MESAPE         PIC 99.                       00000360
                   06 MCAR-DIAAPE         PIC 99.                       00000370
               04  MCAR-FECULT.                                         00000380
                   06 MCAR-ANOULT         PIC 9(04).                    00000390
                   06 MCAR-MESULT         PIC 99.                       00000400
                   06 MCAR-DIAULT         PIC 99.                       00000410
               04  MCAR-CODCEN            PIC 9(08)      COMP-3.        00000420
               04  MCAR-CTAABO            PIC 9(10).                    00000430
               04  MCAR-CTAGAR         REDEFINES MCAR-CTAABO PIC 9(10). 00000440
               04  MCAR-CTACAR            PIC 9(10).                    00000450
               04  MCAR-CTAORD         REDEFINES MCAR-CTACAR PIC 9(10). 00000460
               04  MCAR-SITUAC            PIC 9(01).                    00000470
               04  MCAR-NUMMOV            PIC 9(01).                    00000480
               04  MCAR-CHEGER            PIC X(01).                    00000490
               04  MCAR-IMPUES            PIC X(01).                    00000500
               04  MCAR-CODPLA            PIC 9(04).                    00000510
               04  MCAR-PLAORI            PIC 9(04).                    00000520
               04  MCAR-CODVEN            PIC 9(03).                    00000530
               04  MCAR-CODCOM            PIC 9(03).                    00000540
      *---------------------------------------------   191 --------     00000550
           02  MCAR-CODIGOS-TARIFAS.                                    00000560
               04  MCAR-PORTES            PIC 9(04).                    00000570
               04  MCAR-TRASNC            PIC 9(04).                    00000580
               04  MCAR-TRASBC            PIC 9(04).                    00000590
               04  MCAR-TRASBN            PIC 9(04).                    00000600
               04  MCAR-COMTRA            PIC 9(04).                    00000610
               04  MCAR-COMACE            PIC 9(04).                    00000620
               04  MCAR-COMCOB            PIC 9(04).                    00000630
               04  MCAR-COMCTE            PIC 9(04).                    00000640
               04  MCAR-COMPRO            PIC 9(04).                    00000650
               04  MCAR-COMRET            PIC 9(04).                    00000660
               04  MCAR-INTERE            PIC 9(04).                    00000670
               04  MCAR-COMISI            PIC 9(04).                    00000680
               04  MCAR-COMCUS            PIC 9(04).                    00000690
               04  MCAR-GASNOT            PIC 9(04).                    00000700
               04  MCAR-COTAMO            PIC 9(04).                    00000710
               04  MCAR-COTAVE            PIC 9(04).                    00000720
               04  MCAR-COTARE            PIC 9(04).                    00000730
               04  MCAR-COMMAN            PIC 9(04).                    00000740
      *---------------------------------------------   72  --------     00000750
           02  MCAR-ESTADISTICOS.                                       00000760
               04  MCAR-IMPRES            PIC 9(13)V99   COMP-3.        00000770
               04  MCAR-IMPMOR            PIC 9(13)V99   COMP-3.        00000780
               04  MCAR-IMPVEN            PIC 9(13)V99   COMP-3.        00000790
               04  MCAR-IMPJUD            PIC 9(13)V99   COMP-3.        00000800
               04  MCAR-SALMAX            PIC 9(13)V99   COMP-3.        00000810
               04  MCAR-SALMIN            PIC 9(13)V99   COMP-3.        00000820
               04  MCAR-IMPSAE            PIC 9(13)V99   COMP-3.        00000830
               04  MCAR-RESANT            PIC 9(13)V99   COMP-3.        00000840
               04  MCAR-MORANT            PIC 9(13)V99   COMP-3.        00000850
               04  MCAR-VENANT            PIC 9(13)V99   COMP-3.        00000860
               04  MCAR-JUDANT            PIC 9(13)V99   COMP-3.        00000870
               04  MCAR-NROING            PIC 9(05)      COMP-3.        00000880
               04  MCAR-IMPING            PIC 9(13)V99   COMP-3.        00000890
               04  MCAR-NROSAL            PIC 9(05)      COMP-3.        00000900
               04  MCAR-IMPSAL            PIC 9(13)V99   COMP-3.        00000910
               04  MCAR-NUMERA            PIC S9(13)V99  COMP-3.        00000920
               04  MCAR-NRODIA            PIC 9(03)      COMP-3.        00000930
      *---------------------------------------------  120  --------     00000940
           02  MCAR-LINEAS-CREDITOS.                                    00000950
               04  MCAR-LINCRE  OCCURS 4 TIMES.                         00000960
                   06  MCAR-APERCR          PIC 9(08).                  00000970
                   06  MCAR-VENCCR          PIC 9(08).                  00000980
                   06  MCAR-CANCCR          PIC X(01).                  00000990
                   06  MCAR-CONOCR          PIC X(01).                  00001000
                   06  MCAR-GARACR          PIC 9(01).                  00001010
                   06  MCAR-IMPOCR          PIC 9(13)V99  COMP-3.       00001020
                   06  MCAR-DERECR          PIC 9(03)V99  COMP-3.       00001030
                   06  MCAR-NOUTCR          PIC 9(13)V99  COMP-3.       00001040
                   06  MCAR-ULINCR          PIC 9(11)V99  COMP-3.       00001050
      *---------------------------------------------   180 --------     00001060
               04  MCAR-LIMICR              PIC 9(13)V99  COMP-3.       00001070
           02  MCAR-OTROS-DATOS.
               04  MCAR-SALDEU              PIC 9(13)V99  COMP-3.
               04  MCAR-IDENTI              PIC X(01).
               04  MCAR-CAMPAN              PIC X(01).
           02  MCAR-FILLE1                  PIC X(03).
      * EVE-0001-I
      *    02  MCAR-FILLER                  PIC X(04).
           02  MCAR-CORASU                  PIC X(03).
           02  MCAR-TIP-CRED                PIC X(01).
      * EVE-0001-F
      *---------------------------------------------   25  --------     00001090
