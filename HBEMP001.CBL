       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HBEMP001.
       AUTHOR.        ODAIR GARCIA AROUCA.
       DATE-WRITTEN.  14/07/2019.
       DATE-COMPILED.                .
      ********* Favor incluir alteracoes efetuadas ***********
      * - PROGRAMA DE LOGIN 
      *
      *
      *
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "C:\hbsis\ftt\FEMPRESA.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "C:\hbsis\ftt\FEMPRESA.FD".
       WORKING-STORAGE SECTION.
       77  WS-SAIDA                   PIC X(001)  VALUE SPACES.
       77  WS-OK                      PIC 9(002)  VALUE ZEROS.
       77  WS-b                       PIC 9(002)  VALUE 10.
       77  WS-MOMENTO                 PIC X       VALUE SPACES.
       77  WS-IND                     PIC 9(003)  VALUE 0.
       77  WS-DT-EDIT                 PIC 99/99/99.
       77  WS-VERSAO                  PIC X(010)  VALUE "14/07/2019".
       77  WS-DT-ACESSO               PIC 99/99/99.
       77  WS-USUARIO                 PIC X(008)  VALUE SPACES.
       77  WS-SENHA                   PIC X(008)  VALUE SPACES.
       77  WS-USUARIO1                PIC X(008).
       77  WS-SENHA1                  PIC X(008)  COMP-X.
       77  WS-LIBERACAO               PIC X(002).
      
       COPY "C:\HBSIS\FTT\WCONDATA.WK".
       COPY "C:\HBSIS\FTT\WCTRAN01.WK".
       SCREEN SECTION.
       COPY "C:\HBSIS\FTT\TPLANO2.ss".
       COPY "C:\HBSIS\FTT\TLOG.SS".
       COPY "C:\HBSIS\FTT\TTELART.SS".
       COPY "C:\HBSIS\FTT\TTELAADV.SS".
       COPY "C:\HBSIS\FTT\TBARRA2.SS".
       COPY "C:\HBSIS\FTT\TFIM.SS".
       PROCEDURE DIVISION.
               move "00" to wk01-tecla.
               MOVE SPACES TO LK-PGM(2) LK-PGM(3) LK-PGM(4).
               DISPLAY PARAME UPON COMMAND-LINE
               CALL X"91" USING RESULT FUNCTION-NUM PARAMETER.
       COMECA.
           PERFORM 0000-INICIO.
           PERFORM 1000-TELA.
       FINALIZA.
           CLOSE ARQEMP.
               IF  WK01-TECLA EQUAL "01"
                   STOP RUN.
           MOVE ZEROS              TO LK-TRANSFERENCIA.
           MOVE EMP-COD            TO LK-EMP-COD.
           MOVE EMP-NOME           TO LK-EMP-NOME.
           MOVE WS-LIBERACAO       TO LK-LIBERACAO.
           MOVE 01000000           TO LK-MENU.
           MOVE "HBEMP001"          TO LK-PGM-CHAMADOR.
           MOVE EMP-DATA-VALIDADE  TO LK-DT-VALIDADE.
           MOVE ZEROS              TO LK-INDICE.
           MOVE "MA00200"          TO LK-PGM(1).
           MOVE WS-USUARIO         TO LK-USUARIO.
           MOVE "S"                TO LK-CONTINUA.
       0000-VOLTA.
           CALL LK-PGM(1) USING LK-TRANSFERENCIA.
           END-CALL.
           perform 20000-conf.
           
       0000-INICIO SECTION.
       0000-1.
           MOVE "HBEMP001"              TO WK-COD-PGM.
           MOVE "ACESSO AO SISTEMA - LOGIN        "
                                       TO WK-DESC-PGM.
           ACCEPT WK-DATA  FROM DATE.
           ACCEPT WK-HORA  FROM TIME.
           MOVE WK-DIA            TO WK-DIA-SALVO.
           MOVE WK-ANO            TO WK-DIA.
           MOVE WK-DIA-SALVO      TO WK-ANO.
           PERFORM 9000-ANO2000.
           MOVE WK-DATA-MIL       TO WK-DATA-SYS.
           MOVE WK-DIA-INV        TO WK-DIA-EXT.
           MOVE WK-ANO-INV        TO WK-ANO-EXT.
           MOVE WK-MES-R (WK-MES-INV) TO WK-MES-EXT.
           DISPLAY G-TPLANO2.
           INITIALIZE EMP-REG REPLACING ALPHANUMERIC DATA BY SPACES
                      NUMERIC DATA BY ZEROS.
		   MOVE "C:\HBSIS\DB\ARQEMP.DAT" TO WS-CAMINHO EMP-ARQ-WK.
           OPEN I-O ARQEMP.
           MOVE " EMPRESA "           TO WK-MSG-RT2.
           PERFORM 9100-RETCOD.
           DISPLAY G-TLOG.
       0000-EXIT.
           EXIT.
       1000-TELA SECTION.
       1000-EMP.
		   MOVE "01" TO EMP-COD.
           ACCEPT EMP-COD AT 0842 WITH UPDATE AUTO-SKIP.
           ACCEPT WK01-TECLA FROM ESCAPE KEY.
           IF  WK01-TECLA EQUAL "00"
               MOVE "HBSIS - PROGRAMA DE TESTE " TO EMP-NOME
			   MOVE "HBSIS" TO WS-USUARIO.
			   MOVE "HBSIS" TO WS-SENHA.
               MOVE WS-USUARIO TO EMP-USUARIO(1).
               MOVE WS-SENHA   TO EMP-SENHA(1).
               MOVE 20193112   TO EMP-DATA-VALIDADE.
			   WRITE EMP-REG INVALID KEY
				   MOVE " EMPRESA "        TO WK-MSG-RT2
				   PERFORM 9100-RETCOD
				   STOP RUN.
       1000-DATA.
       1000-USUARIO.
           MOVE ZEROS TO WS-OK.
           ACCEPT  WS-USUARIO  AT 1042 WITH AUTO-SKIP UPDATE PROMPT
           ACCEPT WK01-TECLA FROM ESCAPE KEY.
           IF  WS-USUARIO EQUAL SPACES
               MOVE "Usuario deve ser diferente de espacos..."
                                       TO WK-MSG-ADV1
               MOVE "Entre com o seu nome de usuario...     "
                                       TO WK-MSG-ADV2
               PERFORM 9300-ADVERTENCIA.
           MOVE ZEROS                  TO WS-IND WS-OK.
           PERFORM 1000-PROCURA UNTIL WS-IND EQUAL 15.
           IF  WS-OK EQUAL ZEROS
               MOVE "Usuario nao cadastrado..."
                                       TO WK-MSG-ADV1
               MOVE "Falar com o administrador do sistema..."
                                       TO WK-MSG-ADV2
               PERFORM 9300-ADVERTENCIA.
           MOVE ZEROS                  TO WS-IND.
       1000-SENHA.
           MOVE SPACES                 TO WS-SENHA.
           ACCEPT WS-SENHA AT 1242 WITH AUTO-SKIP PROMPT SECURE.
           ACCEPT WK01-TECLA FROM ESCAPE KEY.
           MOVE WS-SENHA               TO WS-SENHA1.
           IF  WS-SENHA1 NOT EQUAL EMP-SENHA(WS-OK)
               MOVE "Senha invalida... "
                                       TO WK-MSG-ADV1
               MOVE "Digite a senha do usuario informado... "
                                       TO WK-MSG-ADV2
               PERFORM 9300-ADVERTENCIA
               ADD 1                   TO WS-IND
               IF  WS-IND EQUAL 3
                   MOVE "Esgotou as tentativas de acesso..."
                                       TO WK-MSG-ADV1
                   MOVE "Verifique qual o seu acesso..."
                                       TO WK-MSG-ADV2
                   PERFORM 9300-ADVERTENCIA
                   MOVE "01" TO WK01-TECLA
			   ELSE
			       MOVE "01" TO WK01-TECLA.
       1000-EXIT.
            EXIT.
           COPY "C:\HBSIS\FTT\MANO2000.MP".
           COPY "C:\HBSIS\FTT\MRETCOD.MP".
           COPY "C:\HBSIS\FTT\MADVERT.MP".
       20000-conf section.
       20000-1.
           MOVE "01" TO WK01-TECLA.
       20000-exit.
           exit.
       FINALIZA2.
           CLOSE ARQEMP.
               IF  WK01-TECLA EQUAL "01"
                   EXIT PROGRAM
                   STOP RUN.
