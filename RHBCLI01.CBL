       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RHBCLI01.
       AUTHOR.        Odair Garcia Arouca.
       DATE-WRITTEN.  14/7/2019.
       SECURITY.  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ?
                  ?     Relatório de clientes   ³±
                  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±
                    ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
      ******************************************************************
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-AT.
       OBJECT-COMPUTER. PC-AT.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "C:\HBSIS\FTT\FCLI0001.SEL".
           COPY "C:\HBSIS\FTT\FEMPRESA.SEL".
      *
           SELECT SORTWK1 ASSIGN TO DISK.
      *
           SELECT RELAT ASSIGN TO PRINTER.
      *
           SELECT  RDISCO ASSIGN TO WS-RDISCO
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS  IS WK01-RETCOD.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY "C:\HBSIS\FTT\FCLI0001.FD".
           COPY "C:\HBSIS\FTT\FEMPRESA.FD".
      *
       FD RELAT
            LABEL RECORD OMITTED.
       01 REG-RELAT             PIC X(220).
      *
       FD  RDISCO
           RECORDING MODE IS VARIABLE
           DATA RECORD ARE REG-RDISCO.
       01  REG-RDISCO              PIC X(220).
      *
       SD  SORTWK1
           VALUE OF FILE-ID IS "SRFT005.$$$".
       01  SOR-REG.
           05 SOR-Cod                      PIC  9(007).
           05 SOR-cnpj                     PIC  X(014).
           05 SOR-Cliente                  PIC  X(040).
           05 SOR-Latitude                 PIC  s9(003)V99999999.
           05 SOR-Longitude                PIC  s9(003)V99999999.

      *
       WORKING-STORAGE SECTION.
       01  CAMPOS-TRABALHO.
           05 WS-RDISCO            PIC X(050)     VALUE "C:\EMAILS\".
           05 WS-FORMATA-HTML      PIC X(005)     VALUE "<PRE>".
           05 WS-DIAMES            PIC 9999       VALUE ZEROS.
            05 WS-GERAL-REL-REAL        PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-REL-DOLLAR      PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-REL-MARCO       PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-REL-EURO        PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-REL-CUBAGEM     PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-REL-PROCESSO    PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-REL-CONTAINER   PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-REAL        PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-DOLLAR      PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-MARCO       PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-EURO        PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-CUBAGEM     PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-PROCESSO    PIC 9(012)V999 VALUE ZEROS.
           05 WS-GERAL-CONTAINER   PIC 9(012)V999 VALUE ZEROS.
           05 WS-TOTAL-REAL        PIC 9(012)V999 VALUE ZEROS.
           05 WS-TOTAL-DOLLAR      PIC 9(012)V999 VALUE ZEROS.
           05 WS-TOTAL-MARCO       PIC 9(012)V999 VALUE ZEROS.
           05 WS-TOTAL-EURO        PIC 9(012)V999 VALUE ZEROS.
           05 WS-TOTAL-CUBAGEM     PIC 9(012)V999 VALUE ZEROS.
           05 WS-TOTAL-PROCESSO    PIC 9(012)V999 VALUE ZEROS.
           05 WS-TOTAL-CONTAINER   PIC 9(012)V999 VALUE ZEROS.
           05 ws-mes-old           pic 9(002)     value zeros.
           05 WS-FOR-COD           PIC 9(005)     VALUE ZEROS.
           05 WS-CLI-COD           PIC 9(005)     VALUE ZEROS.
           05 WS-REP-COD           PIC 9(005)     VALUE ZEROS.
           05 WS-REG-COD           PIC 9(003)     VALUE ZEROS.
           05 REP-NOME-AUX-WK      PIC X(040)     VALUE SPACES.
           05 FOR-NOME-AUX-WK      PIC X(040)     VALUE SPACES.
           05 CLI-NOME-AUX-WK      PIC X(040)     VALUE SPACES.
           05 WS-ERRO-START        PIC X(003)     VALUE SPACES.
           05 WS-PRIM-IMPRESSAO    PIC X(003)     VALUE SPACES.
           05 WS-CONTINUA          PIC X(003)     VALUE SPACES.
           05 WS-FIM-ARQUIVO       PIC X(003)     VALUE "NAO".
           05 WS-PED-NUM           PIC X(020)     VALUE SPACES.
           05 WS-INDICE-TAM        PIC 9(002)     VALUE ZEROS.
           05 WS-IND1              PIC 9(002)     VALUE ZEROS.
           05 WS-SAIDA             PIC X(003)     VALUE "NAO".
           05 WS-SAIDA-TRANSF      PIC X(003)     VALUE SPACES.
           05 WS-SAIDA-TAM         PIC X(003)     VALUE SPACES.
           05 WS-GRAVA-SORT        PIC X(003)     VALUE "SIM".
           05 WS-PROXIMO           PIC X(003)     VALUE "NAO".
           05 WS-INDICE-AUX        PIC 9(002)     VALUE ZEROS.
           05 WS-TAM-CONT          PIC X(010)     VALUE SPACES.
           05 WS-CBM-ALPHA         PIC X(008)     VALUE SPACES.
           05 WS-TOT-GERAL-DOLAR   PIC 9(013)V999  VALUE ZEROS.
           05 WS-TOT-GERAL-EURO    PIC 9(013)V999  VALUE ZEROS.
           05 WS-TOT-GERAL-MARCO   PIC 9(013)V999  VALUE ZEROS.
           05 WS-TOTAL-CBM         PIC 9(009)V999 VALUE ZEROS.
           05 WS-QTD-CONT          PIC 9(009)v999 VALUE ZEROS.
           05 WS-PED-CONTAINER     PIC 9(003)     VALUE ZEROS.
           05 WS-IND-ACHADO        PIC 9(003)     VALUE 001.
           05 WS-IND-SALVO         PIC 9(003)     VALUE 001.
           05 WS-IND               PIC 9(003)     VALUE 001.
           05 WS-IND2              PIC 9(002)     VALUE 001.
           05 WS-IND4              PIC 9(002)     VALUE 001.
           05 WS-UMEDIDA           PIC 9(002)     VALUE 001.
           05 WS-VIRGULA           PIC 9(002)     VALUE 001.
           05 WS-ponto             PIC 9(002)     VALUE 001.
           05 WS-POSICAO           PIC 9(002)     VALUE 001.
           05 CONT                 PIC 9(002)     VALUE ZEROS.
           05 WS-SAIDA-CONT        PIC X(003)     VALUE "NAO".
           05 WS-SAIDA-IMPRIME     PIC X(003)     VALUE "NAO".
           05 WS-SAIDA-COMPARA     PIC X(003)     VALUE "NAO".
           05 WS-CONTINUA-SORT     PIC X(003)     VALUE "NAO".
           05 Cliente-ANT    PIC X(015)     VALUE SPACES.
           05 MES-ANT              PIC 9(002)     VALUE ZEROS.
           05 ANO-ANT              PIC 9(004)     VALUE ZEROS.
           05 TOT-VLR-REP-REAL-WK  PIC 9(013)V999  VALUE ZEROS.
           05 TOT-VLR-REP-EURO-WK  PIC 9(013)V999  VALUE ZEROS.
           05 TOT-VLR-REP-DOLAR-WK PIC 9(013)V999  VALUE ZEROS.
           05 TOT-VLR-REP-MARCO-WK PIC 9(013)V999  VALUE ZEROS.
           05 TOT-VLR-REP-LIBRA-WK PIC 9(013)V999  VALUE ZEROS.
           05 VLR-COMISSAO1-WK     PIC 9(013)V999  VALUE ZEROS.
           05 VLR-COMISSAO2-WK     PIC 9(013)V999  VALUE ZEROS.
           05 VLR-COMISSAO3-WK     PIC 9(013)V999  VALUE ZEROS.
           05 DESC-TOTAL-WK        PIC 9(011)V999  VALUE ZEROS.
           05 TOTAL-PEDIDO-WK      PIC 9(013)V999  VALUE ZEROS.
           05 DATA-EDITADA         PIC 99/99/9999 VALUE ZEROS.
           05 WS-HORA              PIC 9(008)     VALUE ZEROS.
           05 WS-HORAR REDEFINES WS-HORA.
              10 HORA-WK          PIC 9(002).
              10 MIN-WK           PIC 9(002).
              10 SEG-WK           PIC 9(004).
           05 WS-HORA-EDIT         PIC X(010) VALUE spaces.
           05 WS-HORA-EDITR REDEFINES WS-HORA-EDIT.
              10 HORA-EDIT-WK          PIC X(002).
              10 PONTO1-WK             PIC X(001).
              10 MIN-EDIT-WK           PIC X(002).
              10 PONTO2-WK             PIC X(001).
              10 SEG-EDIT-WK           PIC X(004).
           05 DATA-SAIDA-OLD-WK    PIC 9(008)     VALUE ZEROS.
           05 DATA-SAIDA-OLD-WKR REDEFINES DATA-SAIDA-OLD-WK.
              10 DIA-SAIDA-OLD-WK  PIC 9(002).
              10 MES-SAIDA-OLD-WK  PIC 9(002).
              10 ANO-SAIDA-OLD-WK  PIC 9(004).
           05 DATA-AUX-WK          PIC 9(008)     VALUE ZEROS.
           05 DATA-AUX-WKR REDEFINES DATA-AUX-WK.
              10 DIA-AUX-WK       PIC 9(002).
              10 MES-AUX-WK       PIC 9(002).
              10 ANO-AUX-WK       PIC 9(004).
           05 DATA-MM-WK          PIC 9(008)     VALUE ZEROS.
           05 DATA-MM-WKR REDEFINES DATA-MM-WK.
              10 DIA-MM-WK       PIC 9(002).
              10 MES-MM-WK       PIC 9(002).
              10 ANO-MM-WK       PIC 9(004).
           05 DATA-PRN-WK         PIC 9(008)     VALUE ZEROS.
           05 DATA-PRN-WKR REDEFINES DATA-PRN-WK.
              10 DIA-PRN-WK       PIC 9(002).
              10 MES-PRN-WK       PIC 9(002).
              10 ANO-PRN-WK       PIC 9(004).
           05 DATA-INI-WK         PIC 9(008)     VALUE ZEROS.
           05 DATA-INI-WKR REDEFINES DATA-INI-WK.
              10 ANO-INI-WK       PIC 9(004).
              10 MES-INI-WK       PIC 9(002).
              10 DIA-INI-WK       PIC 9(002).
           05 DATA-FIM-WK         PIC 9(008)     VALUE ZEROS.
           05 DATA-FIM-WKR REDEFINES DATA-FIM-WK.
              10 ANO-FIM-WK       PIC 9(004).
              10 MES-FIM-WK       PIC 9(002).
              10 DIA-FIM-WK       PIC 9(002).
           05 FLAG-PRIM-QUEBRA    PIC XXX        VALUE "SIM".
           05 FLAG-PRIM-VEZ       PIC XXX        VALUE "SIM".
           05 IMPRIME-LINHA-09    PIC XXX        VALUE SPACES.
           05 PED-FOR-COD-ANT     PIC 9(005)     VALUE ZEROS.
           05 PED-CLI-COD-ANT     PIC 9(005)     VALUE ZEROS.
           05 FOR-COD-WK          PIC ZZZZ9.
           05 CLI-COD-WK          PIC ZZZZ9.
           05 REP-COD-WK          PIC ZZZZ9.
           05 FOR-AUX-WK          PIC 9(005)     VALUE ZEROS.
           05 CLI-AUX-WK          PIC 9(005)     VALUE ZEROS.
           05 WS-MOMENTO          PIC X          VALUE SPACES.
           05 WS-DATA             PIC 9(006)     VALUE ZEROS.
           05 WK-OCCURS           PIC 9          VALUE ZEROS.
           05 MSG                 PIC X(050)     VALUE SPACES.
           05 AUXILIAR            PIC X          VALUE SPACES.
           05 NUM-OPCOES          PIC 9(002)     VALUE ZEROS.
           05 INDICE              PIC 9(002)     VALUE ZEROS.
           05 IND                 PIC 9(002)     VALUE ZEROS.
           05 OPCAO-PED-WK        PIC 9(002)     VALUE ZEROS.
           05 OPCAO-IMP-WK        PIC 9(002)     VALUE ZEROS.
           05 FIM-ARQUIVO         PIC XXX        VALUE "NAO".
           05 ERRO-START          PIC XXX        VALUE "NAO".
           05 ERRO-LEITURA        PIC XXX        VALUE SPACES.
           05 ERRO-LEITURA-CPL    PIC XXX        VALUE SPACES.
           05 ERRO-ARQOAP         PIC XXX        VALUE SPACES.
           05 FIM-IMP-ITENS       PIC XXX        VALUE SPACES.
           05 CRIA-OBS-ADICIONAL  PIC X          VALUE SPACES.
           05 CONTLIN             PIC 99         VALUE 66.
           05 CONTPAG             PIC 999        VALUE ZEROS.
           05 DATA-WK             PIC 9(6)       VALUE ZEROS.
           05 DATA-WKR REDEFINES DATA-WK.
              10 ANO-WK           PIC 99.
              10 MES-WK           PIC 99.
              10 DIA-WK           PIC 99.
           05 EDICAO              PIC ZZZZZZZZ9,999.
           05 DEPTO-WK            PIC 9          VALUE ZEROS.
           05 CODESPECIFPROD-WK   PIC 9(5)       VALUE ZEROS.
           05 SAIDA-WK            PIC X          VALUE SPACES.
           05 TECLA               PIC XX         VALUE SPACES.

           05 WS-MODO-IMPRESSAO   PIC 9(001).
              88 IMPRESSORA       VALUE 1.
              88 VIDEO            VALUE 2.
              88 WS-EMAIL         VALUE 3.
              88 WS-HTML          VALUE 4.

           05 DATA-EDIT-WK         PIC X(010)     VALUE SPACES.
           05 DATA-EDIT-WKR REDEFINES DATA-EDIT-WK.
              10 DIA-EDIT-WK       PIC X(002).
              10 BARRA1-WK          PIC X(001).
              10 MES-EDIT-WK       PIC X(002).
              10 BARRA2-WK          PIC X(001).
              10 ANO-EDIT-WK       PIC X(004).

           05 PED-SIT-WK          PIC 9(001).
              88 PEDIDOS-GERAL    VALUE 1.
              88 PEDIDOS-ABERTOS  VALUE 2.
              88 PEDIDOS-BAIXADOS VALUE 3.

           05 MES-EXTENSO-WK.
              10 FILLER           PIC X(009)     VALUE "January  ".
              10 FILLER           PIC X(009)     VALUE "February ".
              10 FILLER           PIC X(009)     VALUE "March    ".
              10 FILLER           PIC X(009)     VALUE "April    ".
              10 FILLER           PIC X(009)     VALUE "May      ".
              10 FILLER           PIC X(009)     VALUE "June     ".
              10 FILLER           PIC X(009)     VALUE "July     ".
              10 FILLER           PIC X(009)     VALUE "August   ".
              10 FILLER           PIC X(009)     VALUE "September".
              10 FILLER           PIC X(009)     VALUE "October  ".
              10 FILLER           PIC X(009)     VALUE "November ".
              10 FILLER           PIC X(009)     VALUE "December ".
           05 MES-EXTENSO-WKR REDEFINES MES-EXTENSO-WK.
              10 MES-EXTENSO OCCURS 12 TIMES     PIC X(009).
           05 OBS-ADIC-WK.
              10 OBS01-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS02-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS03-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS04-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS05-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS06-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS07-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS08-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS09-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS10-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS11-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS12-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS13-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS14-ADIC-WK    PIC X(060)     VALUE SPACES.
              10 OBS15-ADIC-WK    PIC X(060)     VALUE SPACES.

       01 LINHA-MENU              PIC X(040) OCCURS 3 TIMES.

       01 WS-PARAMETRO-DOS            PIC X(0065).
       01 REDEFINES WS-PARAMETRO-DOS.
          03 WS-COMANDO-1             PIC X(0045).
          03 WS-COMANDO-2             PIC X(0020).
       01 WS-FUNCAO-DOS               PIC 99      COMP-X VALUE 35.
       01 WS-LINHA-COMANDO.
          03 WS-COMANDO               PIC X       COMP-X VALUE ZEROES.

       77 WS-NOME-PROG                PIC X(008) VALUE SPACES.
       77 WS-RESULTADO                PIC 9(0002) COMP-X.
       77 WS-COD-RETORNO              PIC 9(0002) COMP-X.
       77 WS-COMPARA-CONT PIC 9(002) OCCURS 25 TIMES VALUE ZEROS.
       77 VALOR-ALFA-NUMERICO     PIC X(001)     VALUE SPACES.
	   77 WS-ORDEM-REL                PIC X(001) VALUE SPACES.
	   77 WS-CLAS-REL                PIC X(001) VALUE SPACES.
       77 VALOR-NUMERICO REDEFINES VALOR-ALFA-NUMERICO
                                  PIC 9(002) COMP-X.
       01 POS.
          03 LIN-POS              PIC 99.
          03 COL-POS              PIC 99.

       01 WS-NUM-CBM         PIC 9(005)V999 VALUE ZEROS.
       01 WS-num             PIC 9(002) VALUE ZEROS.

       01 KEY-PRESSED             PIC 999.
       01 TECLA-ENTER             PIC 999        VALUE 013.
       01 TECLA-ESC               PIC 999        VALUE 027.
       01 TECLA-UP                PIC 999        VALUE 005.
       01 TECLA-DOWN              PIC 999        VALUE 006.

       01 screen-origin2          pic 9(4) comp-x value 0.
       01 screen-buffer2          pic x(2000).
       01 screen-attrs2           pic x(2000).
       01 screen-string-length2   pic 9(4) comp-x value 2000.

      *----------------------------------------------------------*
      *         Vari veis de Impressao do Relat?io              *
      *----------------------------------------------------------*
      *
       01  CONFIG-PRINTER.
           05 CONFIG-LANDSCAPE               PIC  X(005) VALUE
              "&l1O".
           05 CONFIG-LINHA-GRAFICA           PIC  X(021) VALUE
              "(12U(s1p8v0s0b4148T".
           05 CONFIG-LINHA-POR-PAGINA        PIC  X(025) VALUE
              "&12a1o5.45C(s0p16.67h0T".
           05 CONFIG-PONTOS-POLEGADA         PIC  X(006) VALUE
              "&l12D".
      *
       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(055) VALUE
              "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ?.
           05 FILLER                         PIC  X(054) VALUE ALL "?.
           05 FILLER                         PIC  X(054) VALUE ALL "?.
           05 FILLER                         PIC  X(005) VALUE
              "ÄÄÄÄ?.
       02  LINHA-02.
           05 FILLER                         PIC  X(001) VALUE "?.
           05 Empresa-r                      PIC  X(040) VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE SPACES.
           05 FILLER                         PIC  X(054) VALUE SPACES.
           05 FILLER                         PIC  X(014) VALUE
              "         PG.: ".
           05 Contpag-r                      PIC  9(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE "?.
       02  LINHA-03.
           05 FILLER                         PIC  X(055) VALUE
              "?                                                     ".
           05 FILLER                         PIC  X(054) VALUE
              "  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 FILLER                         PIC  X(054) VALUE
              "ÄÄÄÄÄÄÄÄ?                                            ".
           05 FILLER                         PIC  X(005) VALUE
              "    ?.
       02  LINHA-04.
           05 FILLER                         PIC  X(007) VALUE
              "?ata: ".
           05 Data-rel-r                     PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE " - ".
           05 Hora-rel-r                     PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(053) VALUE
              "                           ?     Listagem de clientes".
           05 FILLER                         PIC  X(045) VALUE
              "".
           05 FILLER                         PIC  X(015) VALUE
              "      RHBCLI01?.
       02  LINHA-05.
           05 FILLER                         PIC  X(055) VALUE
              "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄ?.
           05 FILLER                         PIC  X(054) VALUE
              "ÄÄÁÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄ".
           05 FILLER                         PIC  X(054) VALUE
              "ÄÄÄÄÄÄÄÄÁÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 FILLER                         PIC  X(005) VALUE
              "ÄÄÄÄ?.
       02  LINHA-06.
           05 FILLER                         PIC  X(007) VALUE
              "?  ".
           05 FILLER                         PIC  X(002) VALUE ALL "/".
           05 FILLER                         PIC  X(055) VALUE
              "Cod   ?  CNPJ    ?  Cliente              ".
           05 FILLER                         PIC  X(054) VALUE
              "                   ? Latitude    ? Longitude      ".
       02  LINHA-07.
           05 FILLER                         PIC  X(055) VALUE
              "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄ?.
           05 FILLER                         PIC  X(054) VALUE
              "ÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄ".
           05 FILLER                         PIC  X(054) VALUE
              "ÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 FILLER                         PIC  X(005) VALUE
              "ÄÄÄÄ?.
       02  LINHA-08.
           05 FILLER                         PIC  X(001) VALUE "?.
           05 Codigo-r                       PIC  ZZZZZZZ.
           05 FILLER                         PIC  X(001) VALUE "/".
           05 Cnpj-R                         PIC  X(014) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "?.
           05 Cliente-r                      PIC  X(040) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "?.
           05 FILLER                         PIC  X(004) VALUE "R$  ".
           05 latitude-r                     PIC  ZZZ,ZZZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "?S$  ".
           05 longitude-r                    PIC  ZZZ,ZZZZZZZZ.
       02  LINHA-09.
           05 FILLER                         PIC  X(055) VALUE
              "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄ?.
           05 FILLER                         PIC  X(054) VALUE
              "ÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄ".
           05 FILLER                         PIC  X(054) VALUE
              "ÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 FILLER                         PIC  X(005) VALUE
              "ÄÄÄÄ?.

      *
       COPY "C:\HBSIS\FTT\VLCGCCPF.WK".
       COPY "C:\HBSIS\FTT\WCONDATA.WK".
       COPY "C:\HBSIS\FTT\WCURSOR.WK".
       COPY "C:\HBSIS\FTT\WEDITADO.WK".
      *
      *LINKAGE SECTION.
       01  LK-CAMINHO             PIC X(025).
       COPY "C:\HBSIS\FTT\WCTRAN01.WK".
      *
       SCREEN SECTION.
       COPY "C:\HBSIS\FTT\TCOSCLI2.SS".
       COPY "C:\HBSIS\FTT\TTELART.SS".
       COPY "C:\HBSIS\FTT\TTELAADV.SS".
       COPY "C:\HBSIS\FTT\TBARRA1.SS".
       COPY "C:\HBSIS\FTT\TBARRA2.SS".
       COPY "C:\HBSIS\FTT\TPLANO2.ss".
       COPY "C:\HBSIS\FTT\TCONF.SS".
       COPY "C:\HBSIS\FTT\TMSG.SS".
       COPY "C:\HBSIS\FTT\TSHOWPED.SS".
       COPY "C:\CBSIS\CPT\TPATH.SS".
       COPY "C:\CBSIS\CPT\TVIDEO.SS".
      *
      *PROCEDURE DIVISION USING LK-TRANSFERENCIA.
       PROCEDURE DIVISION.
       PROGRAMA.
           PERFORM ABRIR-ARQUIVOS THRU ABRIR-ARQUIVOS-EXIT
           ACCEPT DATA-WK FROM DATE
           MOVE "N" TO WK-CONFIRMA SAIDA-WK
           PERFORM ENTRAR-DADOS-TELA THRU ENTRAR-DADOS-TELA-EXIT
                          UNTIL WK-CONFIRMA = "S" OR SAIDA-WK = "S"
           IF SAIDA-WK = "N" THEN
              PERFORM SALVA-TELA THRU SALVA-TELA-EXIT
              IF IMPRESSORA
                 WRITE REG-RELAT FROM CONFIG-PRINTER AFTER 0
              END-IF
              PERFORM ROTINA-SORT  THRU ROTINA-SORT-EXIT
              MOVE "NAO" TO WS-FIM-ARQUIVO
              EVALUATE TRUE
              WHEN IMPRESSORA
                 CLOSE RELAT
                 OPEN OUTPUT RELAT
                 WRITE REG-RELAT FROM SPACES AFTER PAGE
                 CLOSE RELAT
              WHEN VIDEO
                 MOVE SPACES TO REG-RDISCO
                 WRITE REG-RDISCO FROM SPACES AFTER 2
                 WRITE REG-RDISCO FROM SPACES AFTER PAGE
                 CLOSE RDISCO
                 MOVE WS-RDISCO      TO LK-CAMINHO
                 CALL   "MA00603" USING LK-CAMINHO
                 CANCEL "MA00603"
              WHEN OTHER
                  MOVE SPACES TO REG-RDISCO
                  WRITE REG-RDISCO AFTER 2
                  WRITE REG-RDISCO AFTER PAGE
                  CLOSE RDISCO
                  PERFORM TELA
              END-EVALUATE
           END-IF
           PERFORM FINALIZA THRU FINALIZA-EXIT
           GO TO PROGRAMA.
       PROGRAMA-EXIT.
           EXIT.
      *
       ROTINA-SORT.
           EVALUATE TRUE.
		   WHEN WS-CLAS-REL = 'C'
			   EVALUATE TRUE.
			   WHEN WS-ORDEM-REL = 'A'
					SORT SORTWK1 ON ASCENDING KEY SOR-COD
			   WHEN WS-ORDEM-REL = 'D'
					SORT SORTWK1 ON ASCENDING KEY SOR-COD
				END-EVALUATE
		   WHEN WS-CLAS-REL = 'N'
			   EVALUATE TRUE.
			   WHEN WS-ORDEM-REL = 'A'
					SORT SORTWK1 ON ASCENDING KEY SOR-CLIENTE
			   WHEN WS-ORDEM-REL = 'D'
					SORT SORTWK1 ON ASCENDING KEY SOR-CLIENTE
				END-EVALUATE
		   END-EVALUATE.                              
           INPUT  PROCEDURE CONDIC-START - ??????
           OUTPUT PROCEDURE PROCESSAMENTO-GERAL.
       ROTINA-SORT-EXIT.
           EXIT.
      
       CONSISTE-CONDIC.
           IF WS-PRIM-IMPRESSAO = "NAO" THEN
              PERFORM LE-PROXIMO THRU LE-PROXIMO-EXIT
           END-IF
           IF WS-FIM-ARQUIVO = "SIM" THEN
              NEXT SENTENCE
           END-IF
           MOVE "NAO" TO WS-PRIM-IMPRESSAO
           EVALUATE TRUE
           WHEN DATA-INI-WK <> ZEROS THEN
               PERFORM CONSISTE-DATA THRU CONSISTE-DATA-EXIT
           WHEN CLI-AUX-WK <> ZEROS
               PERFORM CONSISTE-CLI THRU CONSISTE-CLI-EXIT
           WHEN OTHER
               NEXT SENTENCE
           END-EVALUATE.
           EVALUATE TRUE
           WHEN WS-FIM-ARQUIVO = "NAO"
               EVALUATE TRUE
               WHEN WS-CONTINUA = "NAO" OR "   "
                   MOVE "SIM" TO WS-CONTINUA
                   GO TO CONSISTE-CONDIC
               WHEN WS-CONTINUA = "SIM"
                   PERFORM CARREGA-DADOS THRU CARREGA-DADOS-EXIT
                   GO TO CONSISTE-CONDIC
               END-EVALUATE
           WHEN WS-FIM-ARQUIVO = "SIM"
               PERFORM ZERA-SORT THRU ZERA-SORT-EXIT
               NEXT SENTENCE
           END-EVALUATE.
       CONSISTE-CONDIC-EXIT.
           EXIT.
      *
       CONSISTE-CLI.
           EVALUATE TRUE
           WHEN PDL-CLI-COD  = CLI-AUX-WK OR
                CPL-CLI-COD  = CLI-AUX-WK
                MOVE "SIM" TO WS-CONTINUA
           WHEN OTHER
                MOVE "NAO" TO WS-CONTINUA
           END-EVALUATE.
       CONSISTE-CLI-EXIT.
           EXIT.
      
      *
       CARREGA-DADOS.
           PERFORM MOSTRAR-GERACAO THRU MOSTRAR-GERACAO-EXIT
      
           MOVE CPL-CLI-COD  TO CLI-COD
           PERFORM LER-ARQCLI THRU LER-ARQCLI-EXIT
           MOVE CLI-NOME-RES            TO SOR-Cliente
           PERFORM ZERA-SORT      THRU ZERA-SORT-EXIT.
       CARREGA-DADOS-EXIT.
           EXIT.

      *
       ZERA-SORT SECTION.
           MOVE ZEROS  TO SOR-Total-container
                          SOR-Total-real
                          SOR-Total-dollar
                          SOR-Total-marco
                          SOR-Total-euro
                          SOR-Total-cubagem
                          SOR-TOTAL-PROCESSO
                          SOR-Data
                          SOR-Vlr-Comissao.

           MOVE SPACES TO SOR-Tp-Moeda
                          SOR-Moeda1
                          SOR-Tam-Container.
       ZERA-SORT-EXIT.
           EXIT.
      *
       GRAVA-SORT.
           RELEASE SOR-REG.
       GRAVA-SORT-EXIT.
           EXIT.
      *
       PROCESSAMENTO-GERAL.
           MOVE "NAO" TO FIM-ARQUIVO
           RETURN SORTWK1 AT END
               MOVE "SIM" TO FIM-ARQUIVO
                  EVALUATE TRUE
                  WHEN IMPRESSORA
                     WRITE REG-RELAT  FROM LINHA-11 AFTER 1
                     WRITE REG-RELAT  FROM LINHA-08 AFTER 1
                  WHEN VIDEO
                     WRITE REG-RDISCO FROM LINHA-11 AFTER 1
                     WRITE REG-RDISCO FROM LINHA-08 AFTER 1
                  WHEN OTHER
                     MOVE SPACES TO REG-RDISCO
                     STRING LINHA-11 DELIMITED BY SIZE INTO REG-RDISCO
                     WRITE REG-RDISCO AFTER 1
                     MOVE SPACES TO REG-RDISCO
                     STRING LINHA-08 DELIMITED BY SIZE INTO REG-RDISCO
                     WRITE REG-RDISCO AFTER 1
                  END-EVALUATE
           END-RETURN
           IF FIM-ARQUIVO = "NAO" THEN
              PERFORM IMPRIME-REGISTRO THRU IMPRIME-REGISTRO-EXIT
              GO PROCESSAMENTO-GERAL
           END-IF.
       PROCESSAMENTO-GERAL-EXIT.
           EXIT.
      *
       IMPRIME-REGISTRO.
           PERFORM MOSTRAR-IMPRESSAO THRU MOSTRAR-IMPRESSAO-EXIT
           IF FLAG-PRIM-QUEBRA = "SIM" THEN
              MOVE "NAO" TO FLAG-PRIM-QUEBRA
              MOVE SOR-Cliente       TO cliente-ANT
              MOVE SOR-MES-REL       TO MES-ANT
              MOVE SOR-ANO-REL       TO ANO-ANT
           END-IF
           IF  (CONTPAG = ZEROS) THEN
                PERFORM CABECALHO THRU CABECALHO-EXIT
           END-IF
           EVALUATE TRUE
           WHEN SOR-Cliente =  Cliente-ANT
               EVALUATE TRUE
               WHEN SOR-MES-REL = MES-ANT
                   MOVE SOR-Cliente          TO Cliente-r
                   MOVE MES-EXTENSO(MES-ANT) TO MES-REL-R
                   MOVE ANO-ANT              TO ANO-REL-R
                   ADD  SOR-Total-REAL       TO WS-total-real
                                                WS-GERAL-REAL

                   ADD  SOR-Total-DOLLAR     TO WS-total-dollar
                                                WS-GERAl-dollar

                   ADD  SOR-Total-MARCO      TO WS-total-marco
                                                WS-geral-marco

                   ADD  SOR-Total-EURO       TO WS-total-euro
                                                WS-geral-euro

                   ADD  SOR-Total-container  TO WS-Total-container
                                                WS-geral-container

                   ADD  SOR-Total-cubagem    TO WS-Total-cubagem
                                                WS-geral-cubagem

                   ADD  SOR-Total-processo   TO WS-Total-processo
                                                WS-geral-processo

                   MOVE WS-total-real        TO total-real-r
                   MOVE WS-total-dollar      TO total-dollar-r
                   MOVE WS-total-marco       TO total-marco-r
                   MOVE WS-total-euro        TO total-euro-r
                   MOVE WS-Total-container   TO Total-container-r
                   MOVE WS-Total-cubagem     TO Total-cubagem-r
                   MOVE WS-Total-processo    TO Total-processo-r
                   MOVE WS-GERal-real        TO GERal-real-r
                   MOVE WS-GERal-dollar      TO GERal-dollar-r
                   MOVE WS-GERal-marco       TO GERal-marco-r
                   MOVE WS-GERal-euro        TO GERal-euro-r
                   MOVE WS-GERal-container   TO GERal-container-r
                   MOVE WS-GERal-cubagem     TO GERal-cubagem-r
                   MOVE WS-GERal-processo    TO GERal-processo-r
               WHEN SOR-MES-REL <> MES-ANT
                   MOVE WS-total-real      TO total-real-r
                   MOVE WS-total-dollar    TO total-dollar-r
                   MOVE WS-total-marco     TO total-marco-r
                   MOVE WS-total-euro      TO total-euro-r
                   MOVE WS-Total-container TO Total-container-r
                   MOVE WS-Total-cubagem   TO Total-cubagem-r
                   MOVE WS-Total-processo  TO Total-processo-r
                   IF  (CONTLIN > 70) THEN
                        EVALUATE TRUE
                        WHEN IMPRESSORA
                           WRITE REG-RELAT FROM LINHA-09 AFTER 1
                        WHEN VIDEO
                           WRITE REG-RDISCO FROM LINHA-09 AFTER 1
                        WHEN OTHER
                           MOVE SPACES TO REG-RDISCO
                           STRING LINHA-09 DELIMITED BY SIZE INTO
                                  REG-RDISCO
                           WRITE REG-RDISCO AFTER 1
                        END-EVALUATE
                        PERFORM CABECALHO THRU CABECALHO-EXIT
                   END-IF
                  EVALUATE TRUE
                  WHEN IMPRESSORA
                     WRITE REG-RELAT  FROM LINHA-07 AFTER 1
                     WRITE REG-RELAT  FROM LINHA-08 AFTER 1
                  WHEN VIDEO
                     WRITE REG-RDISCO FROM LINHA-07 AFTER 1
                     WRITE REG-RDISCO FROM LINHA-08 AFTER 1
                  WHEN OTHER
                     MOVE SPACES TO REG-RDISCO
                     STRING LINHA-07 DELIMITED BY SIZE INTO REG-RDISCO
                     WRITE REG-RDISCO AFTER 1
                     MOVE SPACES TO REG-RDISCO
                     STRING LINHA-08 DELIMITED BY SIZE INTO REG-RDISCO
                     WRITE REG-RDISCO AFTER 1
                  END-EVALUATE
                   ADD 2 TO CONTLIN
                   PERFORM ZERA-TOTAL THRU ZERA-TOTAL-EXIT

                   MOVE SOR-MES-REL         TO MES-ANT
                   ADD  SOR-Total-REAL      TO WS-total-real
                                               WS-GERAL-REAL

                   ADD  SOR-Total-DOLLAR    TO WS-total-dollar
                                               WS-GERAl-dollar

                   ADD  SOR-Total-MARCO     TO WS-total-marco
                                               WS-geral-marco

                   ADD  SOR-Total-EURO      TO WS-total-euro
                                               WS-geral-euro

                   ADD  SOR-Total-container TO WS-Total-container
                                               WS-geral-container

                   ADD  SOR-Total-cubagem   TO WS-Total-cubagem
                                               WS-geral-cubagem

                   ADD  SOR-Total-processo  TO WS-Total-processo
                                               WS-geral-processo
                   MOVE WS-total-real      TO total-real-r
                   MOVE WS-total-dollar    TO total-dollar-r
                   MOVE WS-total-marco     TO total-marco-r
                   MOVE WS-total-euro      TO total-euro-r
                   MOVE WS-Total-container TO Total-container-r
                   MOVE WS-Total-cubagem   TO Total-cubagem-r
                   MOVE WS-Total-processo  TO Total-processo-r

                   MOVE WS-geral-real      TO geral-real-r
                   MOVE WS-geral-dollar    TO geral-dollar-r
                   MOVE WS-geral-marco     TO geral-marco-r
                   MOVE WS-geral-euro      TO geral-euro-r
                   MOVE WS-geral-container TO geral-container-r
                   MOVE WS-geral-cubagem   TO geral-cubagem-r
                   MOVE WS-geral-processo  TO geral-processo-r
               END-EVALUATE
           WHEN SOR-Cliente       <> Cliente-ANT
               PERFORM ZERA-TOTAL THRU ZERA-TOTAL-EXIT
               IF  (CONTLIN > 70) THEN
                    EVALUATE TRUE
                    WHEN IMPRESSORA
                       WRITE REG-RELAT FROM LINHA-09 AFTER 1
                    WHEN VIDEO
                       WRITE REG-RDISCO FROM LINHA-09 AFTER 1
                    WHEN OTHER
                       MOVE SPACES TO REG-RDISCO
                       STRING LINHA-09 DELIMITED BY SIZE INTO REG-RDISCO
                       WRITE REG-RDISCO AFTER 1
                    END-EVALUATE
                    PERFORM CABECALHO THRU CABECALHO-EXIT
               END-IF
               EVALUATE TRUE
               WHEN IMPRESSORA
                  WRITE REG-RELAT FROM LINHA-11 AFTER 1
                  WRITE REG-RELAT FROM LINHA-08 AFTER 1
               WHEN VIDEO
                  WRITE REG-RDISCO FROM LINHA-11 AFTER 1
                  WRITE REG-RDISCO FROM LINHA-08 AFTER 1
               WHEN OTHER
                  MOVE SPACES TO REG-RDISCO
                  STRING LINHA-11 DELIMITED BY SIZE INTO REG-RDISCO
                  WRITE REG-RDISCO AFTER 1
                  MOVE SPACES TO REG-RDISCO
                  STRING LINHA-08 DELIMITED BY SIZE INTO REG-RDISCO
                  WRITE REG-RDISCO AFTER 1
               END-EVALUATE
               PERFORM IMPRIME-TOTAL-CLI THRU IMPRIME-TOTAL-CLI-EXIT
               IF  (CONTLIN > 70) THEN
                  EVALUATE TRUE
                  WHEN IMPRESSORA
                     WRITE REG-RELAT  FROM LINHA-09 AFTER 1
                  WHEN VIDEO
                     WRITE REG-RDISCO FROM LINHA-09 AFTER 1
                  WHEN OTHER
                     MOVE SPACES TO REG-RDISCO
                     STRING LINHA-09 DELIMITED BY SIZE INTO REG-RDISCO
                     WRITE REG-RDISCO AFTER 1
                  END-EVALUATE
                    PERFORM CABECALHO THRU CABECALHO-EXIT
               END-IF
               PERFORM ZERA-GERAL THRU ZERA-GERAL-EXIT
               MOVE SOR-CLIENTE          TO CLIENTE-r
               MOVE MES-EXTENSO(MES-ANT) TO MES-REL-R
               MOVE ANO-ANT             TO ANO-REL-R
               MOVE SOR-Total-REAL      TO WS-total-real
               MOVE SOR-Total-DOLLAR    TO WS-total-dollar
               MOVE SOR-Total-MARCO     TO WS-total-marco
               MOVE SOR-Total-EURO      TO WS-total-euro
               MOVE SOR-Total-container TO WS-Total-container
               MOVE SOR-Total-cubagem   TO WS-Total-cubagem
               MOVE SOR-Total-processo  TO WS-Total-processo
               ADD  WS-total-real      TO WS-GERal-real
               ADD  WS-total-dollar    TO WS-GERal-dollar
               ADD  WS-total-marco     TO WS-GERal-marco
               ADD  WS-total-euro      TO WS-GERal-euro
               ADD  WS-Total-container TO WS-GERal-container
               ADD  WS-Total-cubagem   TO WS-GERal-cubagem
               ADD  WS-Total-processo  TO WS-GERal-processo
               MOVE WS-total-real       TO total-real-r
               MOVE WS-total-dollar     TO total-dollar-r
               MOVE WS-total-marco      TO total-marco-r
               MOVE WS-total-euro       TO total-euro-r
               MOVE WS-Total-container  TO Total-container-r
               MOVE WS-Total-cubagem    TO Total-cubagem-r
               MOVE WS-Total-processo   TO Total-processo-r
               MOVE WS-GERal-real       TO GERal-real-r
               MOVE WS-GERal-dollar     TO GERal-dollar-r
               MOVE WS-GERal-marco      TO GERal-marco-r
               MOVE WS-GERal-euro       TO GERal-euro-r
               MOVE WS-GERal-container  TO GERal-container-r
               MOVE WS-GERal-cubagem    TO GERal-cubagem-r
               MOVE WS-GERal-processo   TO GERal-processo-r
           END-EVALUATE.
       IMPRIME-REGISTRO-EXIT.
           EXIT.

      *
       CABECALHO.
           ACCEPT DATA-WK FROM DATE
           ACCEPT WS-HORA FROM TIME
           MOVE ZEROS TO CONTLIN
           ADD 1 TO CONTPAG
           MOVE CONTPAG    TO CONTPAG-R
           MOVE SOR-CLIENTE   TO CLIENTE-r
           MOVE DIA-WK TO DIA-EDIT-WK
           MOVE MES-WK TO MES-EDIT-WK
           MOVE ANO-WK TO ANO-EDIT-WK(3:2)
           IF ANO-WK > 80 THEN
              MOVE 19 TO ANO-EDIT-WK(1:2)
           ELSE
              MOVE 20 TO ANO-EDIT-WK(1:2)
           END-IF
           MOVE "/" TO BARRA1-WK BARRA2-WK
           MOVE DATA-EDIT-WK TO DATA-REL-R
           MOVE HORA-WK TO HORA-EDIT-WK
           MOVE MIN-WK  TO MIN-EDIT-WK
           MOVE SEG-WK  TO SEG-EDIT-WK
           move ":"     to ponto1-wk ponto2-wk
           MOVE ws-hora-edit TO HORA-REL-R
            IF FLAG-PRIM-VEZ = "SIM" THEN
               MOVE "NAO" TO FLAG-PRIM-VEZ
               EVALUATE TRUE
               WHEN IMPRESSORA
                  WRITE REG-RELAT  FROM LINHA-01 AFTER 1
               WHEN VIDEO
                  WRITE REG-RDISCO FROM LINHA-01 AFTER 1
               WHEN OTHER
                 MOVE SPACES TO REG-RDISCO
                 STRING WS-FORMATA-HTML DELIMITED BY SIZE
                       INTO REG-RDISCO
                 WRITE REG-RDISCO AFTER 0
                  MOVE SPACES TO REG-RDISCO
                  STRING LINHA-01 DELIMITED BY SIZE INTO REG-RDISCO
                  WRITE REG-RDISCO AFTER 1
               END-EVALUATE
               ADD 1 TO CONTLIN
            ELSE
               EVALUATE TRUE
               WHEN IMPRESSORA
                  WRITE REG-RELAT  FROM LINHA-01 AFTER PAGE
               WHEN VIDEO
                  WRITE REG-RDISCO FROM LINHA-01 AFTER PAGE
               WHEN OTHER
                  MOVE SPACES TO REG-RDISCO
                  STRING LINHA-01 DELIMITED BY SIZE INTO REG-RDISCO
                  WRITE REG-RDISCO AFTER PAGE
               END-EVALUATE
            END-IF
               EVALUATE TRUE
               WHEN IMPRESSORA
                  WRITE REG-RELAT FROM LINHA-02 AFTER 1
                  WRITE REG-RELAT FROM LINHA-03 AFTER 1
                  WRITE REG-RELAT FROM LINHA-04 AFTER 1
                  WRITE REG-RELAT FROM LINHA-05 AFTER 1
                  WRITE REG-RELAT FROM LINHA-06 AFTER 1
               WHEN VIDEO
                  WRITE REG-RDISCO  FROM LINHA-02 AFTER 1
                  WRITE REG-RDISCO  FROM LINHA-03 AFTER 1
                  WRITE REG-RDISCO  FROM LINHA-04 AFTER 1
                  WRITE REG-RDISCO  FROM LINHA-05 AFTER 1
                  WRITE REG-RDISCO  FROM LINHA-06 AFTER 1
               WHEN OTHER
                  MOVE SPACES TO REG-RDISCO
                  STRING LINHA-02 DELIMITED BY SIZE INTO REG-RDISCO
                  WRITE REG-RDISCO AFTER PAGE
                  MOVE SPACES TO REG-RDISCO
                  STRING LINHA-03 DELIMITED BY SIZE INTO REG-RDISCO
                  WRITE REG-RDISCO AFTER PAGE
                  MOVE SPACES TO REG-RDISCO
                  STRING LINHA-04 DELIMITED BY SIZE INTO REG-RDISCO
                  WRITE REG-RDISCO AFTER PAGE
                  MOVE SPACES TO REG-RDISCO
                  STRING LINHA-05 DELIMITED BY SIZE INTO REG-RDISCO
                  WRITE REG-RDISCO AFTER PAGE
                  MOVE SPACES TO REG-RDISCO
                  STRING LINHA-06 DELIMITED BY SIZE INTO REG-RDISCO
                  WRITE REG-RDISCO AFTER PAGE
               END-EVALUATE
           ADD 6 TO CONTLIN.
       CABECALHO-EXIT.
           EXIT.
      *
       ENTRAR-DADOS-TELA.
           PERFORM ESCOLHER-VIDEO-IMPR THRU ESCOLHER-VIDEO-IMPR-EXIT
           IF KEY-PRESSED = TECLA-ESC THEN
              MOVE "S" TO SAIDA-WK
              MOVE "N" TO WK-CONFIRMA
              GO ENTRAR-DADOS-TELA-EXIT
           ELSE
              IF KEY-PRESSED = TECLA-ESC THEN
                 GO ENTRAR-DADOS-TELA
              END-IF
              IF IMPRESSORA THEN
                 OPEN OUTPUT RELAT
              ELSE
                 EVALUATE TRUE
                 WHEN VIDEO
                     MOVE WS-DIAMES          TO WS-RDISCO(11:4)
                     MOVE ".TXT"             TO WS-RDISCO(15:4)
                 WHEN WS-EMAIL
                     MOVE WS-DIAMES          TO WS-RDISCO(11:4)
                     MOVE ".EML"             TO WS-RDISCO(15:4)
                 WHEN WS-HTML
                     MOVE WS-DIAMES          TO WS-RDISCO(11:4)
                     MOVE ".HTM"             TO WS-RDISCO(15:4)
                 END-EVALUATE
                 DISPLAY G-TPATH
                 ACCEPT WS-RDISCO AT 1116 WITH PROMPT AUTO-SKIP UPDATE
                 OPEN OUTPUT RDISCO
              END-IF
           END-IF
           DISPLAY G-TCOSCLI2
           MOVE ZEROS TO DATA-INI-WK DATA-FIM-WK FOR-COD-WK CLI-COD-WK
                         REP-COD-WK.
       ENTRAR-ORDEM.
           MOVE SPACES TO WS-ORDEM-REL.
           ACCEPT WS-ORDEM-REL AT 1029 WITH UPDATE AUTO-SKIP
           ACCEPT TECLA FROM ESCAPE KEY.

       ENTRAR-CLASSIFICACAO.
           MOVE SPACES TO WS-CLAS-REL.
           ACCEPT WS-CLAS-REL AT 1057 WITH UPDATE AUTO-SKIP
           ACCEPT TECLA FROM ESCAPE KEY.
           
       ENTRAR-CLIENTE-INI.
           ACCEPT CLI-COD-INI-WK AT 1425 WITH UPDATE AUTO-SKIP
           ACCEPT TECLA FROM ESCAPE KEY
           IF  TECLA EQUAL "10"
               CALL "HBCLI01Z" USING LK-TRANSFERENCIA END-CALL
               CANCEL "HBCLI01Z"
               MOVE LK-COD TO CLI-COD-WK
               GO ENTRAR-CLIENTE
           END-IF
           MOVE CLI-COD-INI-WK TO CLI-AUX-WK CLI-COD
           IF CLI-AUX-WK NOT = ZEROS THEN
              PERFORM LER-ARQCLI THRU LER-ARQCLI-EXIT
              DISPLAY CLI-NOME AT 1434
              MOVE CLI-NOME TO CLI-NOME-AUX-WK
           
           END-IF.
       ENTRAR-CLIENTE-FIM.
           ACCEPT CLI-COD-FIM-WK AT 1425 WITH UPDATE AUTO-SKIP
           ACCEPT TECLA FROM ESCAPE KEY
           IF  TECLA EQUAL "10"
               CALL "HBCLI01Z" USING LK-TRANSFERENCIA END-CALL
               CANCEL "HBCLI01Z"
               MOVE LK-COD TO CLI-COD-WK
               GO ENTRAR-CLIENTE
           END-IF
           MOVE CLI-COD-FIM-WK TO CLI-AUX-WK CLI-COD
           IF CLI-AUX-WK NOT = ZEROS THEN
              PERFORM LER-ARQCLI THRU LER-ARQCLI-EXIT
              DISPLAY CLI-NOME AT 1434
              MOVE CLI-NOME TO CLI-NOME-AUX-WK
           
           END-IF.		   
      ENTRAR-CLI-NOME-INI.
           ACCEPT CLI-NOM-INI-WK AT 1425 WITH UPDATE AUTO-SKIP
           ACCEPT TECLA FROM ESCAPE KEY
           IF  TECLA EQUAL "10"
               CALL "HBCLI01Z" USING LK-TRANSFERENCIA END-CALL
               CANCEL "HBCLI01Z"
               MOVE LK-NOME TO CLI-NOME-WK
               GO ENTRAR-CLIENTE
           END-IF
           MOVE CLI-COD-INI-WK TO CLI-AUX-WK CLI-COD
           IF CLI-AUX-WK NOT = ZEROS THEN
              PERFORM LER-ARQCLI THRU LER-ARQCLI-EXIT
              DISPLAY CLI-NOME AT 1434
              MOVE CLI-NOME TO CLI-NOME-AUX-WK
           
           END-IF.
       ENTRAR-CLI-NOME-FIM.
           ACCEPT CLI-NOM-FIM-WK AT 1425 WITH UPDATE AUTO-SKIP
           ACCEPT TECLA FROM ESCAPE KEY
           IF  TECLA EQUAL "10"
               CALL "HBCLI01Z" USING LK-TRANSFERENCIA END-CALL
               CANCEL "HBCLI01Z"
               MOVE LK-NOME TO CLI-NOME-WK
               GO ENTRAR-CLIENTE
           END-IF
           MOVE CLI-COD-FIM-WK TO CLI-AUX-WK CLI-COD
           IF CLI-AUX-WK NOT = ZEROS THEN
              PERFORM LER-ARQCLI THRU LER-ARQCLI-EXIT
              DISPLAY CLI-NOME AT 1434
              MOVE CLI-NOME TO CLI-NOME-AUX-WK
           
           END-IF.		   		   
       ENTRAR-CONFIRMACAO.
           MOVE "Confirma emissao (S/N)?" TO WK-MSG-CONF
           MOVE "S" TO WK-CONFIRMA
           PERFORM CONFIRMA THRU CONFIRMA-EXIT
           IF WK-CONFIRMA = "N" OR "n" THEN
              MOVE "SIM" TO SAIDA-WK
           END-IF.
       ENTRAR-DADOS-TELA-EXIT.
           EXIT.
      *
        LER-ARQCLI.
           MOVE "NAO" TO ERRO-LEITURA
           READ ARQCLI WITH NO LOCK INVALID KEY
                MOVE "Cliente ???????" TO CLI-NOME-RES
           END-READ.
       LER-ARQCLI-EXIT.
           EXIT.
 
      *
       MOSTRAR-GERACAO.
           DISPLAY G-TSHOWPED DISPLAY CPL-PED-NUM AT 1938.
       MOSTRAR-GERACAO-EXIT.
           EXIT.
      *
       MOSTRAR-IMPRESSAO.
           DISPLAY G-TSHOWPED DISPLAY SOR-CLIENTE AT 1938.
       MOSTRAR-IMPRESSAO-EXIT.
           EXIT.
      *
       MENSAGEM.
           PERFORM SALVA-TELA THRU SALVA-TELA-EXIT
           PERFORM TEST AFTER UNTIL TECLA = "01"
                   MOVE SPACES TO AUXILIAR
                   DISPLAY G-TMSG DISPLAY MSG AT 2116 WITH BELL
                   ACCEPT AUXILIAR AT 2580 WITH AUTO-SKIP
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
           PERFORM RESTAURA-TELA THRU RESTAURA-TELA-EXIT.
       MENSAGEM-EXIT.
           EXIT.
      *
       CONFIRMA.
           PERFORM SALVA-TELA THRU SALVA-TELA-EXIT
           DISPLAY G-TCONF
           PERFORM TEST AFTER UNTIL
                   WK-CONFIRMA = ("S" OR "s" OR "N" OR "n") OR
                                 (TECLA = "01")
                   ACCEPT WK-CONFIRMA AT 2272 WITH
                          UPDATE REVERSE-VIDEO AUTO-SKIP
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
           IF TECLA = "01" THEN
              MOVE "N" TO WK-CONFIRMA
           END-IF
           PERFORM RESTAURA-TELA THRU RESTAURA-TELA-EXIT.
       CONFIRMA-EXIT.
           EXIT.
      *
       SALVA-TELA.
           call "CBL_READ_SCR_CHATTRS" using screen-origin2
                          screen-buffer2
                          screen-attrs2
                          screen-string-length2.
       SALVA-TELA-EXIT.
           EXIT.

       RESTAURA-TELA.
           call "CBL_WRITE_SCR_CHATTRS" using screen-origin2
                      screen-buffer2
                      screen-attrs2
                      screen-string-length2.
       RESTAURA-TELA-EXIT.
           EXIT.
      *
       ABRIR-ARQUIVOS.
           MOVE "RHBCLI01"        TO WK-COD-PGM.
           MOVE "Listagem estat?tica de Clientes (Planor)"
                 TO WK-DESC-PGM.
           ACCEPT WK-DATA  FROM DATE.
           ACCEPT WS-HORA  FROM TIME.
           ACCEPT WK-HORA  FROM TIME.
           MOVE WK-DIA            TO WK-DIA-SALVO.
           MOVE WK-ANO            TO WK-DIA.
           MOVE WK-DIA-SALVO      TO WK-ANO.
           MOVE WK-DATA           TO WS-DATA.
           MOVE WK-DATA           TO WK-DT.
           PERFORM 9000-ANO2000.
           MOVE WK-DATA-MIL       TO WK-DATA-SYS.
           MOVE WK-DIA-INV        TO WK-DIA-EXT.
           MOVE WK-ANO-INV        TO WK-ANO-EXT.
           MOVE WK-MES-R (WK-MES-INV) TO WK-MES-EXT.
           MOVE LK-EMP-NOME       TO WK-EMP-NOME.
           DISPLAY G-TPLANO2.
           OPEN INPUT ARQDRI.
           MOVE " DRIVERS "           TO WK-MSG-RT2.
           PERFORM VERIF-OPEN-FILE THRU VERIF-OPEN-FILE-EXIT.
           IF  LK-EMP-COD IS NOT NUMERIC
               OR LK-EMP-COD EQUAL ZEROS
               MOVE 1 TO LK-EMP-COD.
           MOVE LK-EMP-COD TO DRI-COD-EMP.
           READ ARQDRI INVALID KEY
               MOVE " DRIVERS "       TO WK-MSG-RT2
               PERFORM 9100-RETCOD
               PERFORM FINALIZA THRU FINALIZA-EXIT
           END-READ
           MOVE DRI-CAMINHO(4) TO WS-CAMINHO.
           OPEN INPUT ARQTAB.
           MOVE " TABELA "        TO WK-MSG-RT2.
           PERFORM VERIF-OPEN-FILE THRU VERIF-OPEN-FILE-EXIT.
           MOVE DRI-CAMINHO(35) TO WS-CAMINHO.
           OPEN INPUT ARQPRO.
           MOVE " PRODUTOS "          TO WK-MSG-RT2.
           PERFORM VERIF-OPEN-FILE THRU VERIF-OPEN-FILE-EXIT.
           MOVE DRI-CAMINHO(02) TO WS-CAMINHO.
           OPEN INPUT ARQCLI.
           MOVE " CLIENTES "          TO WK-MSG-RT2.
           PERFORM VERIF-OPEN-FILE THRU VERIF-OPEN-FILE-EXIT.
           MOVE DRI-CAMINHO(34) TO WS-CAMINHO.
           OPEN INPUT ARQPDL.
           MOVE "PD.PLANOR"           TO WK-MSG-RT2.
           PERFORM VERIF-OPEN-FILE THRU VERIF-OPEN-FILE-EXIT.
           MOVE DRI-CAMINHO(41) TO WS-CAMINHO.
           OPEN INPUT ARQCPL.
           MOVE "CONTAINER"           TO WK-MSG-RT2.
           PERFORM VERIF-OPEN-FILE THRU VERIF-OPEN-FILE-EXIT.
           MOVE DRI-CAMINHO(03) TO WS-CAMINHO.
           OPEN INPUT ARQFOR.
           MOVE " FORNECED "          TO WK-MSG-RT2.
           PERFORM VERIF-OPEN-FILE THRU VERIF-OPEN-FILE-EXIT.
           MOVE DRI-CAMINHO(09) TO WS-CAMINHO.
           OPEN INPUT ARQREP
           MOVE " REPRES.  "          TO WK-MSG-RT2.
           PERFORM VERIF-OPEN-FILE THRU VERIF-OPEN-FILE-EXIT.
           OPEN INPUT ARQEMP.
           MOVE " EMPRESA "           TO WK-MSG-RT2.
           PERFORM VERIF-OPEN-FILE THRU VERIF-OPEN-FILE-EXIT.
           MOVE LK-EMP-COD TO EMP-COD
           READ ARQEMP INVALID KEY
                MOVE " EMPRESA "       TO WK-MSG-RT2
                PERFORM 9100-RETCOD
           END-READ
           MOVE EMP-NOME TO Empresa-r.
       ABRIR-ARQUIVOS-EXIT.
           EXIT.
      *
       VERIF-OPEN-FILE.
           IF WK01-RETCOD <> "00" THEN
              PERFORM 9100-RETCOD
           END-IF.
       VERIF-OPEN-FILE-EXIT.
           EXIT.
      *
      * Se‡? de starts - &st - Odair
      *
       START-ARQPDL-CH1-2.
           START ARQPDL KEY = PDL-CHAVE-PRI INVALID KEY
               MOVE "NAO" TO WS-ERRO-START
           END-START.
       START-ARQPDL-CH1-2-EXIT.
           EXIT.
      *
       START-ARQCPL-CH4.
           START ARQCPL KEY > CPL-CHAVE-QUA INVALID KEY
               MOVE "SIM" TO WS-ERRO-START WS-FIM-ARQUIVO
           END-START.
       START-ARQCPL-CH4-EXIT.
           EXIT.
      *
      *
       ESCOLHER-VIDEO-IMPR.
           DISPLAY G-TVIDEO
           MOVE 1 TO IND MOVE 09 TO LIN-POS MOVE 08 TO COL-POS
           MOVE " Impressora " TO LINHA-MENU (1)
           MOVE " Video      " TO LINHA-MENU (2)
           MOVE " Email      " TO LINHA-MENU (3)
           MOVE " Html       " TO LINHA-MENU (4)
           MOVE 4 TO NUM-OPCOES
           PERFORM PERCORRE-OPCOES THRU PERCORRE-OPCOES-EXIT
           MOVE IND TO WS-MODO-IMPRESSAO.
       ESCOLHER-VIDEO-IMPR-EXIT.
           EXIT.
      *
       PERCORRE-OPCOES.
           MOVE ZEROS TO KEY-PRESSED
           PERFORM UNTIL KEY-PRESSED = TECLA-ESC OR TECLA-ENTER
               PERFORM VERIF-KEY-PRESSED THRU VERIF-KEY-PRESSED-EXIT
               EVALUATE TRUE
               WHEN KEY-PRESSED = TECLA-UP
                    PERFORM ROTINA-UP THRU ROTINA-UP-EXIT
               WHEN KEY-PRESSED = TECLA-DOWN
                    PERFORM ROTINA-DOWN THRU ROTINA-DOWN-EXIT
               WHEN KEY-PRESSED = TECLA-ESC
                    MOVE "S" TO WK-CONFIRMA
               END-EVALUATE
           END-PERFORM.
       PERCORRE-OPCOES-EXIT.
           EXIT.
      *
       VERIF-KEY-PRESSED.
           DISPLAY " " AT 2580
           call x"af" using get-single-char key-status
           MOVE key-code-1 TO KEY-PRESSED
           IF KEY-PRESSED = "000" THEN
              MOVE key-code-2 TO KEY-PRESSED
           END-IF.
       VERIF-KEY-PRESSED-EXIT.
           EXIT.
      *
       ROTINA-UP.
           IF IND > 1 THEN
              DISPLAY LINHA-MENU (IND) AT POS
              WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 1 HIGHLIGHT
              COMPUTE IND     = IND     - 1
              COMPUTE LIN-POS = LIN-POS - 1
              DISPLAY LINHA-MENU (IND) AT POS WITH REVERSE-VIDEO
              DISPLAY " " AT 2580
           END-IF.
       ROTINA-UP-EXIT.
           EXIT.
      *
       ROTINA-DOWN.
           IF IND < NUM-OPCOES THEN
              DISPLAY LINHA-MENU (IND) AT POS
              WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 1 HIGHLIGHT
              COMPUTE IND     = IND     + 1
              COMPUTE LIN-POS = LIN-POS + 1
              DISPLAY LINHA-MENU (IND) AT POS WITH REVERSE-VIDEO
              DISPLAY " " AT 2580
           END-IF.
       ROTINA-DOWN-EXIT.
           EXIT.
      *
       FINALIZA.
           CLOSE ARQCPL ARQCLI ARQPRO ARQFOR ARQDRI ARQTAB ARQREP ARQEMP
                 ARQPDL.
           EXIT PROGRAM.
       FINALIZA-EXIT.
           EXIT.
      *
           COPY "C:\HBSIS\FTT\MANO2000.MP".
           COPY "C:\HBSIS\FTT\MRETCOD.MP".
           COPY "C:\HBSIS\FTT\MADVERT.MP".
           COPY "C:\HBSIS\FTT\MCURSOR.MP".
           COPY "C:\HBSIS\FTT\MEDITP.MP".
           COPY "C:\HBSIS\FTT\EMAIL.MP".
      *
       END PROGRAM RHBCLI01.
      *
