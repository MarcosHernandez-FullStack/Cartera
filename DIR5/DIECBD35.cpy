      *--------------------------------------------------------------*
      * EMISION DE ULTIMO ESTADO DE CUENTA                           *
      *--------------------------------------------------------------*
      *--------------------------------------------------------------*
      * MODIFICACIONES                                               *
      * PETICION  FECHA   AUTOR    DESCRIPCION                       *
      * AAAAMMNNN DDMMMAA XXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *--------------------------------------------------------------*
      *
       01  BD35-DIECBD35.
           05 BD35-FORMATO           PIC X(08).
           05 BD35-NRO-MICROFI       PIC X(07).
           05 BD35-IMP-TOT-PAG       PIC S9(15)V99.
           05 BD35-DIVISA            PIC X(12).
           05 BD35-NROBCO            PIC X(10).
           05 BD35-NRORIGINAL        PIC X(12).
           05 BD35-NOM-CLIENTE       PIC X(40).
           05 BD35-SITUACION         PIC X(01).
           05 BD35-NUEVO-IMP         PIC S9(15)V99.
           05 BD35-NVA-FECHA         PIC X(10).
           05 BD35-TIPDOC            PIC X(01).
