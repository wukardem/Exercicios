       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HBCLI001.
       AUTHOR.        Odair Garcia Arouca.
       DATE-WRITTEN.  14/07/2019.
       SECURITY.  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ?
                  ?   Manuten‡? Do Cadastro de Clientes     ³±
                  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±
                    ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           COPY "C:\Hbsis\ftt\FCLI0001.SEL".
      *
       DATA DIVISION.
       FILE SECTION.
      *
           COPY "C:\Hbsis\ftt\FCLI0001.FD".
      *
       WORKING-STORAGE SECTION.
       01 LK-COD-COMPL                PIC X(001).
       01  TECLA                      PIC XX      VALUE SPACES.
       01  MSG                        PIC X(040)  VALUE SPACES.
       77  AUXILIAR                   PIC X       VALUE SPACES.
       77  SAIDA-WS                   PIC XXX     VALUE SPACES.
       77  ERRO-LEITURA               PIC XXX     VALUE SPACES.
       77  WS-13DIG                   PIC 999.999.999.9999.
       77  WS-CLI-PRO-COD             PIC 9(013)  VALUE ZEROS.
       77  WS-CLI-CLI-COD             PIC 9(005)  VALUE ZEROS.
       77  WS-CLI-PEDIDO              PIC 9(010)  VALUE ZEROS.
       77  WS-OK                      PIC 9(002)  VALUE ZEROS.
       77  WS-MOMENTO                 PIC X       VALUE SPACES.
       77  WS-IND                     PIC 9(003)  VALUE 0.
       77  WS-DATA-EDIT               PIC 99/99/99.
       77  WS-VERSAO                  PIC X(010)  VALUE "17/07/2019".
       77  WS-TXA-JUROS               PIC 999,99.
       77  WS-CAMINHO                 PIC X(030) VALUE SPACES.
       77  CLI-NOME-WK				  PIC X(040)  VALUE SPACES.
	   77  CLI-ARQ-WK				  PIC X(080)  VALUE SPACES.

       01  LK-CHAVE.
           03  LK-CLI-COD         PIC 9(005).
           03  LK-PEDIDO          PIC X(010).

       COPY "C:\Hbsis\ftt\VLCGCCPF.WK".
       COPY "C:\Hbsis\ftt\WCONDATA.WK".7
       COPY "C:\Hbsis\ftt\WCURSOR.WK".
       COPY "C:\Hbsis\ftt\WEDITADO.WK".

       LINKAGE SECTION.
       COPY "C:\Hbsis\ftt\WCTRAN01.WK".

       SCREEN SECTION.
       COPY "C:\Hbsis\ftt\TPLANO2.ss".
       COPY "C:\Hbsis\ftt\TTELART.SS".
       COPY "C:\Hbsis\ftt\TTELAADV.SS".
       COPY "C:\Hbsis\ftt\TCLI0001.SS".
       COPY "C:\Hbsis\ftt\TOPCAO.SS".
       COPY "C:\Hbsis\ftt\TCONF.SS".
       COPY "C:\Hbsis\ftt\TBARRA1.SS".
       COPY "C:\Hbsis\ftt\TBARRA2.SS".
       COPY "C:\Hbsis\ftt\TMSG.SS".
       PROCEDURE DIVISION USING LK-TRANSFERENCIA.
      *PROCEDURE DIVISION.
       PROGRAMA.
           PERFORM ABRE-ARQUIVOS THRU
                   ABRE-ARQUIVOS-EXIT
           MOVE ZEROS TO WK-OPCAO
           MOVE "NAO" TO SAIDA-WS
           PERFORM VERIF-OPCAO THRU
                   VERIF-OPCAO-EXIT UNTIL WK-OPCAO EQUAL 9
           PERFORM FINALIZA THRU FINALIZA-EXIT.
       PROGRAMA-EXIT.
           EXIT.

       VERIF-OPCAO.
           DISPLAY G-TOPCAO.
           PERFORM 9600-LOOP-CURSOR.
           MOVE "NAO" TO SAIDA-WS
           EVALUATE TRUE
           WHEN WK-OPCAO EQUAL 1
                PERFORM ROT-INCLUSAO  THRU ROT-INCLUSAO-EXIT
                              UNTIL SAIDA-WS = "SIM"
           WHEN WK-OPCAO EQUAL 2
                PERFORM ROT-ALTERACAO THRU ROT-ALTERACAO-EXIT
                              UNTIL SAIDA-WS = "SIM"
           WHEN WK-OPCAO EQUAL 3
                PERFORM ROT-EXCLUSAO  THRU ROT-EXCLUSAO-EXIT
                              UNTIL SAIDA-WS = "SIM"
           WHEN WK-OPCAO EQUAL 4
                PERFORM ROT-IMPORTACAO  THRU ROT-IMPORTACAO-EXIT
                              UNTIL SAIDA-WS = "SIM"
           END-EVALUATE.
        VERIF-OPCAO-EXIT.
           EXIT.

       ROT-INCLUSAO.
           PERFORM MONTAR-TELA THRU MONTAR-TELA-EXIT
           PERFORM GERAR-CODIGO THRU GERAR-CODIGO-EXIT
           IF SAIDA-WS = "NAO" THEN
              PERFORM LIMPAR-CAMPOS THRU LIMPAR-CAMPOS-EXIT
                 PERFORM ENTRAR-DADOS  THRU ENTRAR-DADOS-EXIT
                 PERFORM CONFIRMA THRU CONFIRMA-EXIT
                 IF WK-CONFIRMA = "S" THEN
                    PERFORM GRAVAR-CLI-REG THRU
                            GRAVAR-CLI-REG-EXIT
                 END-IF
              END-IF
           END-IF
           DISPLAY "                           " AT 1413.
       ROT-INCLUSAO-EXIT.
           EXIT.

       ROT-ALTERACAO.
           PERFORM MONTAR-TELA THRU MONTAR-TELA-EXIT
           PERFORM ENTRAR-CONTROLE THRU ENTRAR-CONTROLE-EXIT
           IF SAIDA-WS = "NAO" THEN
              PERFORM LER-ARQCLI THRU LER-ARQCLI-EXIT
              IF ERRO-LEITURA = "NAO" THEN
                 PERFORM MOSTRAR-DADOS THRU MOSTRAR-DADOS-EXIT
                 PERFORM ENTRAR-DADOS  THRU ENTRAR-DADOS-EXIT
                 PERFORM CONFIRMA THRU CONFIRMA-EXIT
                 IF WK-CONFIRMA = "S" THEN
                    PERFORM REGRAVAR-CLI-REG THRU
                            REGRAVAR-CLI-REG-EXIT
                 END-IF
              END-IF
           END-IF
           DISPLAY "                            " AT 1413.
       ROT-ALTERACAO-EXIT.
           EXIT.

       ROT-EXCLUSAO.
           PERFORM MONTAR-TELA THRU MONTAR-TELA-EXIT
           PERFORM ENTRAR-CONTROLE THRU ENTRAR-CONTROLE-EXIT
           IF SAIDA-WS = "NAO" THEN
              PERFORM LER-ARQCLI THRU LER-ARQCLI-EXIT
              IF ERRO-LEITURA = "NAO" THEN
                 PERFORM MOSTRAR-DADOS THRU MOSTRAR-DADOS-EXIT
                 PERFORM CONFIRMA THRU CONFIRMA-EXIT
                 IF WK-CONFIRMA = "S" THEN
                    PERFORM EXCLUIR-CLI-REG THRU
                            EXCLUIR-CLI-REG-EXIT
                 END-IF
              END-IF
           END-IF
           DISPLAY "                           " AT 1413.
       ROT-EXCLUSAO-EXIT.
           EXIT.

       MONTAR-TELA.
           DISPLAY G-TCLI0001.
       MONTAR-TELA-EXIT.
           EXIT.

       MOSTRAR-DADOS.
           MOVE CLI-COD    TO WK-7DIG.
           DISPLAY WK-7DIG AT 0925 WITH REVERSE-VIDEO.
           MOVE CLI-NOME    TO CLI-NOME-WK
           DISPLAY CLI-NOME-WK AT 1125 WITH REVERSE-VIDEO.
           MOVE CLI-CNPJ TO WA-CGC-NUM.
           MOVE WA-CGC-NUM             TO WA-CGC-EDITADO.
           DISPLAY WA-CGC-EDITADO AT 1025 WITH REVERSE-VIDEO.
           MOVE CLI-LATITUDE    TO CLI-LAT-WK
           DISPLAY CLI-LAT-WK AT 1225 WITH REVERSE-VIDEO.		   
           MOVE CLI-LONGITUDE    TO CLI-LON-WK
           DISPLAY CLI-LON-WK AT 1325 WITH REVERSE-VIDEO.		   		   
       MOSTRAR-DADOS-EXIT.
           EXIT.

       ENTRAR-CONTROLE.
           MOVE ZEROS TO WK-7DIG.
       ENTRAR-COD.
           ACCEPT  WK-7DIG AT 0925 WITH UPDATE AUTO-SKIP
           DISPLAY WK-7DIG AT 0925
           ACCEPT TECLA FROM ESCAPE KEY
           IF TECLA = '01' THEN
              MOVE "SIM" TO SAIDA-WS
           END-IF
           IF TECLA = '10' THEN
              CALL "HBCL001Z" USING LK-TRANSFERENCIA
              END-CALL
              CANCEL "HBCL001Z"
              MOVE LK-COD TO WK-7DIG
              GO ENTRAR-CODIGO
           END-IF
           MOVE WK-7DIG TO CLI-COD
           IF CLI-COD = ZEROS THEN
              MOVE "SIM" TO SAIDA-WS
           END-IF.
       ENTRAR-CONTROLE-EXIT.
           EXIT.

       LIMPAR-CAMPOS.
           MOVE SPACES TO CLI-NOME.
		   MOVE SPACES TO CLI-CNPJ.
		   MOVE ZEROS  TO CLI-LATIDUDE.
		   MOVE ZEROS  TO CLI-LONGITUDE.
       LIMPAR-CAMPOS-EXIT.
           EXIT.

       ENTRAR-DADOS.
           MOVE CLI-NOME TO CLI-NOME-WK.
		   MOVE CLI-CNPJ TO WS-CGC WA-CGC-NUM.
		   MOVE CLI-LATITUDE  TO CLI-LAT-WK.		   
		   MOVE CLI-LONGITUDE TO CLI-LON-WK.		   
	   
	   ENTRAR-CNPJ.           
           MOVE WA-CGC-NUM             TO WA-CGC-EDITADO.
           ACCEPT WA-CGC-EDITADO AT 1025 WITH UPDATE AUTO-SKIP.
           ACCEPT WK01-TECLA FROM ESCAPE KEY.
           MOVE WA-CGC-EDITADO         TO WA-CGC-NUM.
           MOVE WA-CGC-NUM             TO CLI-CNPJ WA-CGC-CPF-AUX.
           PERFORM 9900-CGC.
           DISPLAY WA-CGC-EDIT AT 1025.
           IF  CLI-CNPJ EQUAL SPACES OR ZEROS
               GO  TO  7000-CGC-CONT.
           MOVE WA-CGC-CPF-AUX TO SR-NUM-CGC
           MOVE "CGC"          TO SR-DES-FUNCAO
           CALL "VLCGCCPF" USING SR-PARAM-VLCGCCPF
           IF SR88-CGC-INVALIDO
              MOVE "Digito do CGC nao confere" TO WK-MSG-ADV1
              MOVE "Verif. CGC ou digito correto..." TO WK-MSG-ADV2
              PERFORM 9300-ADVERTENCIA.
	   
	   ENTRAR-NOME.
           ACCEPT CLI-NOME-WK AT 1125 WITH UPDATE AUTO-SKIP
           ACCEPT TECLA FROM ESCAPE KEY
           MOVE CLI-NOME-WK TO CLI-NOME.

	   ENTRAR-LATITUDE.
          ACCEPT CLI-LAT-WK AT 1225 WITH UPDATE AUTO-SKIP
          ACCEPT TECLA FROM ESCAPE KEY
          MOVE CLI-LAT-WK TO CLI-LATITUDE.
	   
	   ENTRAR-LONGITUDE.
          ACCEPT CLI-LON-WK AT 1325 WITH UPDATE AUTO-SKIP
          ACCEPT TECLA FROM ESCAPE KEY
          MOVE CLI-LON-WK TO CLI-LONGITUDE.		   
       ENTRAR-DADOS-EXIT.
           EXIT.

       LER-ARQCLI.
           MOVE "NAO" TO ERRO-LEITURA
           READ ARQCLI WITH LOCK INVALID KEY
                MOVE "SIM" TO ERRO-LEITURA
           END-READ.
           PERFORM VERIF-RETCOD THRU VERIF-RETCOD-EXIT.
       LER-ARQCLI-EXIT.
           EXIT.

       GERAR-CODIGO.
           MOVE 9999999 TO CLI-COD.
           START ARQCLI KEY IS LESS THAN CLI-CHAVE-SEC INVALID KEY
               MOVE "PRIMEIRO CADASTRO, Tecle algo..." TO WK-MENSAGEM
               PERFORM 9100-PARA.
           READ ARQCLI NEXT.
           IF  CLI-COD EQUAL 9999999
               MOVE 0 TO CLI-COD WK-7DIG.
           IF  CLI-COD NOT EQUAL 9999999
               ADD 1 TO CLI-COD
               MOVE CLI-COD TO WK-7DIG.
		   MOVE "NAO" TO SAIDA-WS.
		   DISPLAY WK-7DIG AT 1025.
       GERAR-CODIGO-EXIT.
			EXIT.

       VERIF-RETCOD.
           IF WK-OPCAO = 1 THEN
              IF WK01-RETCOD = "00" THEN
                 call "CBL_READ_SCR_CHATTRS" using screen-origin(2)
                            screen-buffer(2)
                            screen-attrs(2)
                            screen-string-length(2)
                 MOVE WK01-S1            TO WK01-S1-DISP
                 MOVE WK01-S2            TO WK01-S2-DISP
                 MOVE ZEROS              TO WK01-S3-DISP
                 MOVE "Codigo   de    "  TO WK-MSG-RT1
                 MOVE " ja cadastrado "  TO WK-MSG-RT3
                 MOVE "Informe outro codigo ou va p/ alteracao"
                                         TO WK-MSG-RT4
                 DISPLAY G-TTELART
                 MOVE "TECLE ALGO PARA CONTINUAR..." TO WK-MENSAGEM
                 PERFORM 9100-PARA
                 call "CBL_WRITE_SCR_CHATTRS" using screen-origin(2)
                            screen-buffer(2)
                            screen-attrs(2)
                            screen-string-length(2)
              END-IF
           ELSE
              MOVE " CARACTERISTICAS " TO WK-MSG-RT2
              PERFORM 9100-RETCOD
           END-IF.
       VERIF-RETCOD-EXIT.
           EXIT.

       CONFIRMA.
           PERFORM SALVA-TELA THRU SALVA-TELA-EXIT
           PERFORM VERIF-MSG-OPCAO THRU VERIF-MSG-OPCAO-EXIT
           MOVE "S" TO WK-CONFIRMA.
           DISPLAY G-TCONF.
           ACCEPT G-TCONF.
           PERFORM RESTAURA-TELA THRU RESTAURA-TELA-EXIT.
       CONFIRMA-EXIT.
           EXIT.

       VERIF-MSG-OPCAO.
           EVALUATE TRUE
           WHEN WK-OPCAO = 1
                MOVE "Confirma inclusao ..." TO WK-MSG-CONF
           WHEN WK-OPCAO = 2
                MOVE "Confirma alteracao ..." TO WK-MSG-CONF
           WHEN WK-OPCAO = 3
				MOVE "Confirma exclusao ..." TO WK-MSG-CONF
           WHEN WK-OPCAO = 4
                MOVE "Confirma importacao ..." TO WK-MSG-CONF
           END-EVALUATE.
       VERIF-MSG-OPCAO-EXIT.
           EXIT.

       ABRE-ARQUIVOS.
           MOVE "HBCLI001 " TO LK-PGM(2).
           MOVE "HBCLI001 " TO WK-COD-PGM.
           MOVE " CADASTRO DE CLIENTES" TO WK-DESC-PGM.
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
           MOVE LK-EMP-NOME       TO WK-EMP-NOME.
           DISPLAY G-TPLANO2.

           MOVE "C:\HBSIS\DB\ARQCLI.DAT" TO WS-CAMINHO CLI-ARQ-WK.
           OPEN INPUT ARQCLI.
           IF WK01-RETCOD = "35" THEN
              OPEN I-O ARQCLI
              CLOSE ARQCLI
              OPEN INPUT ARQCLI
           END-IF
           MOVE " CLIENTES "           TO WK-MSG-RT2.
           PERFORM 9100-RETCOD.
       ABRE-ARQUIVOS-EXIT.
           EXIT.

       FINALIZA.
           MOVE " " TO LK-PGM(2).
           CLOSE ARQCLI.
           EXIT PROGRAM.
       FINALIZA-EXIT.
           EXIT.

       GRAVAR-CLI-REG.
           CLOSE ARQCLI.
           MOVE CLI-ARQ-WK TO WS-CAMINHO.
		   OPEN I-O   ARQCLI.
           WRITE CLI-REG INVALID KEY
                 PERFORM 9100-RETCOD
           END-WRITE.
           CLOSE ARQCLI
           OPEN INPUT ARQCLI.
       GRAVAR-CLI-REG-EXIT.
           EXIT.

       REGRAVAR-CLI-REG.
           CLOSE ARQCLI.
           MOVE CLI-ARQ-WK TO WS-CAMINHO.           
		   OPEN I-O   ARQCLI.
           REWRITE CLI-REG INVALID KEY
                   PERFORM 9100-RETCOD
           END-REWRITE.
           CLOSE ARQCLI.
           OPEN INPUT ARQCLI.
       REGRAVAR-CLI-REG-EXIT.
           EXIT.

       EXCLUIR-CLI-REG.
           CLOSE ARQCLI.
           MOVE CLI-ARQ-WK TO WS-CAMINHO.		   
           OPEN I-O   ARQCLI.
           DELETE ARQCLI INVALID KEY
                  PERFORM 9100-RETCOD
           END-DELETE.
           CLOSE ARQCLI.
           OPEN INPUT ARQCLI.
       EXCLUIR-CLI-REG-EXIT.
           EXIT.

       MENSAGEM.
           PERFORM SALVA-TELA THRU SALVA-TELA-EXIT
           PERFORM TEST AFTER UNTIL TECLA = "01"
                   MOVE SPACES TO AUXILIAR
                   DISPLAY G-TMSG DISPLAY MSG AT 2332
                   ACCEPT AUXILIAR AT 2580 WITH AUTO-SKIP
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
           PERFORM RESTAURA-TELA THRU RESTAURA-TELA.
       MENSAGEM-EXIT.
           EXIT.

       SALVA-TELA.
           call "CBL_READ_SCR_CHATTRS" using screen-origin(1)
                          screen-buffer(1)
                          screen-attrs(1)
                          screen-string-length(1).
       SALVA-TELA-EXIT.
           EXIT.

       RESTAURA-TELA.
           call "CBL_WRITE_SCR_CHATTRS" using screen-origin(1)
                      screen-buffer(1)
                      screen-attrs(1)
                      screen-string-length(1).
       RESTAURA-TELA-EXIT.
           EXIT.

           COPY "C:\Hbsis\ftt\MANO2000.MP".
           COPY "C:\Hbsis\ftt\MRETCOD.MP".
           COPY "C:\Hbsis\ftt\MADVERT.MP".
           COPY "C:\Hbsis\ftt\MCURSOR.MP".
           COPY "C:\Hbsis\ftt\MEDITP.MP".

