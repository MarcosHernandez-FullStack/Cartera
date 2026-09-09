      *===============  PENDIENTES DE CARTERA  ==================*      00000010
      *  NOMBRE DEL BOOK      : DIPENDTE                         *      00000020
      *  LONGITUD DE REGISTRO : 350                              *      00000030
      *  CLAVE DEL ARCHIVO    : PCAR-KEYPEN                      *      00000040
      *  LONGITUD DE CLAVE    : 22                               *      00000050
      *  NOMBRE FISICO        : PLDIQ.DM.DIPENDTE                *      00000060
      *  NOMBRE CICS          : DIPENDTE                         *      00000070
      *  CODIGO PIDA          : 13                               *      00000080
      *  EVE-001               2010-01-03 EVERIS                *       00000080
      *                         CAMBIO DE TIPEMP Y NROPAQ        *      00000080
      *==========================================================*      00000090
       01  REG-PENDIENTE-CARTERA.                                       00000100
           02 PCAR-KEYPEN.                                              00000110
              04 PCAR-CODAPL.                                           00000120
                 06 PCAR-CODIGO.                                        00000130
                    08 PCAR-CODOFI     PIC 9(03).                       00000140
                    08 PCAR-CODMON     PIC 9(01).                       00000150
                    08 PCAR-CODCLI     PIC 9(06).                       00000160
                 06 PCAR-TIPCTA        PIC 9(02).                       00000170
              04 PCAR-NUMBCO           PIC 9(10).                       00000180
           02 PCAR-PORREN              PIC 9(02)V9(03)  COMP-3.         00000190
           02 PCAR-RENOVA              PIC 9(03).                       00000200
           02 PCAR-TDOCUM              PIC 9(02).                       00000210
           02 PCAR-INDPRO              PIC 9(01).                       00000220
           02 PCAR-PLAORI              PIC 9(04).                       00000230
           02 PCAR-OFICIN              PIC 9(03).                       00000240
           02 PCAR-NOMGIR              PIC X(40).                       00000250
           02 PCAR-DIRGIR.                                              00000260
              04 PCAR-DIRGI1           PIC X(40).                       00000270
              04 PCAR-DIRGI2           PIC X(20).                       00000280
           02 PCAR-PLACOB              PIC 9(04).                       00000290
           02 PCAR-PLAACT              PIC 9(04).                       00000300
           02 PCAR-INSCOB              PIC 9(02).                       00000310
      *EVE-00001-INI
      *    02 PCAR-TIPEMP              PIC 9(01).                       00000320
           02 PCAR-TIPEMP              PIC X(01).                       00000320
      *EVE-00001-FIN
           02 PCAR-SWCANC              PIC 9(01).                       00000330
           02 PCAR-DOCCRE              PIC 9(01).                       00000340
           02 PCAR-NUMCLI              PIC X(10).                       00000350
           02 PCAR-CODCEN              PIC 9(08).                       00000360
           02 PCAR-FECVEN.                                              00000370
              04 PCAR-VENANO           PIC 9(04).                       00000380
              04 PCAR-VENMES           PIC 9(02).                       00000390
              04 PCAR-VENDIA           PIC 9(02).                       00000400
           02 PCAR-FECING.                                              00000410
              04 PCAR-INGANO           PIC 9(04).                       00000420
              04 PCAR-INGMES           PIC 9(02).                       00000430
              04 PCAR-INGDIA           PIC 9(02).                       00000440
           02 PCAR-FECIOR.                                              00000450
              04 PCAR-IORANO           PIC 9(04).                       00000460
              04 PCAR-IORMES           PIC 9(02).                       00000470
              04 PCAR-IORDIA           PIC 9(02).                       00000480
           02 PCAR-FECGIR.                                              00000490
              04 PCAR-GIRANO           PIC 9(04).                       00000500
              04 PCAR-GIRMES           PIC 9(02).                       00000510
              04 PCAR-GIRDIA           PIC 9(02).                       00000520
           02 PCAR-FECCND.                                              00000530
              04 PCAR-CNDANO           PIC 9(04).                       00000540
              04 PCAR-CNDMES           PIC 9(02).                       00000550
              04 PCAR-CNDDIA           PIC 9(02).                       00000560
           02 PCAR-FECDES.                                              00000570
              04 PCAR-DESANO           PIC 9(04).                       00000580
              04 PCAR-DESMES           PIC 9(02).                       00000590
              04 PCAR-DESDIA           PIC 9(02).                       00000600
           02 PCAR-KEYFEC.                                              00000610
              04 PCAR-FECCAR.                                           00000620
                 06 PCAR-CARANO        PIC 9(04).                       00000630
                 06 PCAR-CARMES        PIC 9(02).                       00000640
                 06 PCAR-CARDIA        PIC 9(02).                       00000650
              04 PCAR-IMPACT           PIC 9(13)V99   COMP-3.           00000660
           02 PCAR-IMPORI              PIC 9(13)V99   COMP-3.           00000670
           02 PCAR-IMPMIN              PIC 9(13)V99   COMP-3.           00000680
           02 PCAR-INTDES              PIC 9(09)V99   COMP-3.           00000690
           02 PCAR-GASNOT              PIC 9(09)V99   COMP-3.           00000700
JMA   **   02 PCAR-TASANT              PIC 9(08)V999  COMP-3.           00000710
JMA        02 PCAR-TASA01              PIC 9(02)V999  COMP-3.           00000710
JMA        02 PCAR-TASA02              PIC 9(02)V999  COMP-3.           00000710
           02 PCAR-FECPRO              PIC 9(08).                       00000720
           02 PCAR-CTACTE              PIC 9(10).                       00000730
           02 PCAR-MARCA7              PIC X(01).                       00000740
           02 PCAR-PORTAS              PIC 9(03)V9(03).                 00000750
           02 PCAR-IMPAMO              PIC 9(13)V99   COMP-3.           00000760
           02 PCAR-PERGRA              PIC 9(02).                       00000770
           02 PCAR-NROAMO              PIC 9(03).                       00000780
           02 PCAR-PERAMO              PIC 9(03).                       00000790
           02 PCAR-CONPAG              PIC X(01).                       00000800
           02 PCAR-DOCIDE              PIC X(11).                       00000810
           02 PCAR-TIPIDE              PIC X(01).                       00000810
           02 PCAR-CODDEU              PIC 9(12)      COMP-3.           00000850
           02 PCAR-INTRES              PIC 9(09)V99   COMP-3.
           02 PCAR-PERCOB              PIC 9(02).
           02 PCAR-TIPING              PIC X(01).
      *EVE-00001-INI
      *    02 PCAR-NROPAQ              PIC 9(05)      COMP-3.
           02 PCAR-NROPAQ              PIC X(03).
      *EVE-00001-FIN
           02 PCAR-FECPAQ              PIC 9(08).
           02 PCAR-REMESA              PIC X(01).
           02 PCAR-INDSAE              PIC X(01).
