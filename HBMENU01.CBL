       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HBMENU01.
       AUTHOR.        ODAIR GARCIA AROUCA.
       DATE-WRITTEN.  14/07/2019.
      ********* Favor incluir alteracoes efetuadas ***********
      * - PROGRAMA DE MENU DO SISTEMA 
      *
      *
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "C:\HBSIS\FTT\FMENUHB0.SEL".
           COPY "C:\HBSIS\FTT\FEMPRESA.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "C:\HBSIS\FTT\FMENUHB0.FD".
           COPY "C:\HBSIS\FTT\FEMPRESA.FD".
       WORKING-STORAGE SECTION.

       77  TECLA                      PIC XX      VALUE SPACES.
       77  WS-OK                      PIC 9(002)  VALUE ZEROS.
       77  WS-MOMENTO                 PIC X       VALUE SPACES.
       77  WS-IND1                    PIC 9(002)  VALUE ZEROS.
       77  WS-IND2                    PIC 9(002)  VALUE ZEROS.
       77  WS-IND3                    PIC 9(002)  VALUE ZEROS.
       77  WS-IND4                    PIC 9(002)  VALUE ZEROS.
       77  WS-IND                     PIC 9(002)  VALUE ZEROS.
       77  WS-IND5                    PIC 9(002)  VALUE ZEROS.
       77  WS-DATA-EDIT               PIC 99/99/99.
       77  WS-VERSAO                  PIC X(010)  VALUE "14/07/2019".
       77  WS-NIVEL-CORRENTE          PIC 9(002) VALUE ZEROS.
       01  WS-LINCOL.
           03  WS-LIN                 PIC 9(002) VALUE ZEROS.
           03  WS-COL                 PIC 9(002) VALUE ZEROS.
       01  WS-QUEBRA-NIVEL.
           03  WS-NIVEL1              PIC 9(002) VALUE ZEROS.
           03  WS-NIVEL2              PIC 9(002) VALUE ZEROS.
           03  WS-NIVEL3              PIC 9(002) VALUE ZEROS.
           03  WS-NIVEL4              PIC 9(002) VALUE ZEROS.
       01  WS-QUEBRA-NIVELA.
           03  WS-NIVEL1A             PIC 9(002) VALUE ZEROS.
           03  WS-NIVEL2A             PIC 9(002) VALUE ZEROS.
           03  WS-NIVEL3A             PIC 9(002) VALUE ZEROS.
           03  WS-NIVEL4A             PIC 9(002) VALUE ZEROS.
       01  WS-DESC-PGMS OCCURS 36 TIMES.
           03  WS-DESC1               PIC X(030).
           03  WS-DESC2               PIC X(030).
           03  WS-DESC3               PIC X(030).
           03  WS-DESC4               PIC X(030).
       01  WS-NIVEL-PGMS OCCURS 36 TIMES.
           03  WS-N1                  PIC 9(002) VALUE ZEROS.
           03  WS-N2                  PIC 9(002) VALUE ZEROS.
           03  WS-N3                  PIC 9(002) VALUE ZEROS.
           03  WS-N4                  PIC 9(002) VALUE ZEROS.
       01  WS-PROG PIC X(008) OCCURS 18 TIMES.
       COPY "C:\HBSIS\FTT\WCONDATA.WK".
       01 screen-position.
          05 screen-row       pic 9(2) comp-x value 09.
          05 screen-col       pic 9(2) comp-x value 30.
       01 string-length        pic 9(4) comp-x value 30.

       01 top-row          pic 9(2) comp-x value 09.
       01 bottom-row           pic 9(2) comp-x value 14.

       01 reverse-vid          pic x(30) value all x"07".
       01 black-and-white      pic x(30) value all x"70".

       01 get-single-char      pic 9(2) comp-x value 26.
       01 key-status.
          05 key-type         pic x.
          05 key-code-1       pic 9(2) comp-x.
          05 key-code-2       pic 9(2) comp-x.

       01 set-bit-pairs        pic 9(2) comp-x value 1.
       01 user-key-control.
          05 user-key-setting pic 9(2) comp-x value 1.
          05 filler           pic x value "1".
          05 first-user-key   pic 9(2) comp-x value 1.
          05 number-of-keys   pic 9(2) comp-x value 4.

       78 esc-fn-key           value 0.
       78 user-fn-key          value "1".
       78 adis-fn-key          value "2".
       78 norm-fn-key          value "3".
       78 f1-key           value 1.
       78 f2-key           value 2.
       78 f3-key           value 3.
       78 f4-key           value 4.

       78 carriage-return  value 0.
       78 right-arrow      value 4.
       78 left-arrow       value 3.
       78 up-arrow         value 5.
       78 esc-arrow        value 27.
       78 down-arrow       value 6.
       78 um-arrow         value 49.
       78 dois-arrow       value 50.
       78 tres-arrow       value 51.
       78 quatro-arrow     value 52.
       78 cinco-arrow      value 53.
       78 nove-arrow       value 57.
       LINKAGE SECTION.
       COPY "C:\HBSIS\FTT\WCTRAN01.WK".

       SCREEN SECTION.
       COPY "C:\HBSIS\FTT\TPLANO2.ss".
       COPY "C:\HBSIS\FTT\TTELART.SS".
       COPY "C:\HBSIS\FTT\TTELAADV.SS".
       COPY "C:\HBSIS\FTT\TBARRA2.SS".
       COPY "C:\HBSIS\FTT\TCONF.SS".
       PROCEDURE DIVISION USING LK-TRANSFERENCIA.
      *PROCEDURE DIVISION.
       COMECO.
      *    MOVE 04020100 TO LK-MENU.
           PERFORM 0000-INICIO.
       CARREGA.
           PERFORM 1000-CARREGA.
           PERFORM 2000-MONTA.
           PERFORM 9600-LOOP-CURSOR.
           GO  TO  CARREGA.
       FINALIZA.
           IF key-code-2 NOT = 027 THEN
              GO SAI-SEM-CONFIRMAR
           END-IF
           call "CBL_READ_SCR_CHATTRS" using screen-origin(5)
                                             screen-buffer(5)
                                              screen-attrs(5)
                                      screen-string-length(5)
           MOVE "Confirma sa죆a (S/N)?" TO WK-MSG-CONF.
           MOVE "N" TO WK-CONFIRMA
           DISPLAY G-TCONF.
       ENTRAR-CONF.
           PERFORM TEST AFTER UNTIL
                        WK-CONFIRMA = "N" OR "n" OR "S" OR "s"
                   ACCEPT WK-CONFIRMA AT 2272 WITH PROMPT UPDATE
                                                   AUTO-SKIP
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
           IF TECLA = "01" THEN
              GO ENTRAR-CONF
           END-IF
           IF  WK-CONFIRMA = "N" OR "n" THEN
               call "CBL_WRITE_SCR_CHATTRS" using screen-origin(5)
                                                  screen-buffer(5)
                                                   screen-attrs(5)
                                           screen-string-length(5)
               CLOSE ARQMEN ARQEMP
               GO  TO  COMECO
           END-IF.
       SAI-SEM-CONFIRMAR.
           CLOSE ARQMEN ARQEMP.
           IF  WS-PROG(WS-IND5) EQUAL "HBEMP001"
               EXIT PROGRAM.
           MOVE "HBMENU01"        TO LK-PGM-CHAMADOR.
           MOVE ZEROS            TO LK-INDICE.
           MOVE "S"              TO LK-CONTINUA.
           MOVE WS-QUEBRA-NIVEL  TO LK-MENU.
           MOVE WS-PROG(WS-IND5) TO LK-PGM(1).
           MOVE SPACES TO LK-PGM(2) LK-PGM(3).
           CALL LK-PGM(1) USING LK-TRANSFERENCIA
                                ON EXCEPTION GO TO ERRO
           END-CALL.
           CANCEL LK-PGM(1).
           GO  TO  COMECO.
       ERRO.
           call "CBL_READ_SCR_CHATTRS" using screen-origin-A
                          screen-buffer-A
                          screen-attrs-A
                          screen-string-length-A.
           MOVE "P r o g r a m a"  TO WK-MSG-RT1
           MOVE "nao encontrado "  TO WK-MSG-RT3
           MOVE "Entrar em contado com o Programador..."
                                       TO WK-MSG-RT4
           move LK-PGM(1)          TO WK-MSG-RT2.
           DISPLAY G-TTELART.
           PERFORM 9100-PARA.
           call "CBL_WRITE_SCR_CHATTRS" using screen-origin-A
                          screen-buffer-A
                          screen-attrs-A
                          screen-string-length-A.
           GO  TO  COMECO.
       0000-INICIO SECTION.
       0000-1.
           MOVE "HBMENU01"              TO WK-COD-PGM.
           MOVE "MENU GERAL "          TO WK-DESC-PGM.
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
           OPEN INPUT ARQMEN.
           MOVE " M E N U "           TO WK-MSG-RT2.
           PERFORM 9100-RETCOD.
           OPEN I-O ARQEMP.
           MOVE " EMPRESA "           TO WK-MSG-RT2.
           PERFORM 9100-RETCOD.
           MOVE LK-EMP-COD TO EMP-COD.
           IF  WK-DATA-SYS GREATER THAN LK-DT-VALIDADE
               DELETE ARQEMP INVALID KEY
                   MOVE " EMPRESA "           TO WK-MSG-RT2
                   PERFORM 9100-RETCOD.
       0000-EXIT.
           EXIT.
       1000-CARREGA SECTION.
       1000-CARREGA-MEM.
           MOVE ZEROS                  TO MEN-COD.
           MOVE ZEROS                  TO WS-IND1 WS-IND2
                                          WS-IND3 WS-IND4.
           IF  LK-EMP-COD IS NOT NUMERIC
               MOVE 1 TO LK-EMP-COD.
           MOVE LK-EMP-COD             TO MEN-EMP-COD.
           START ARQMEN KEY IS NOT LESS THAN MEN-CHAVE-PRI INVALID KEY
               MOVE " M E N U "        TO WK-MSG-RT2
               PERFORM 9100-RETCOD
               GO  TO  1000-EXIT.
           IF  WK01-S1 EQUAL "9"
               MOVE " M E N U "        TO WK-MSG-RT2
               PERFORM 9100-RETCOD.
      *    MOVE 01050000 TO LK-MENU.
           IF  LK-MENU IS NOT NUMERIC
               OR LK-MENU EQUAL ZEROS
               MOVE 01000000 TO LK-MENU.
           MOVE LK-MENU TO WS-QUEBRA-NIVEL.
       1000-CARREGA-MEM-LE.
           READ ARQMEN NEXT AT END
               GO  TO  1000-EXIT.
           MOVE MEN-COD TO WS-QUEBRA-NIVELA.
           IF  WS-NIVEL2A EQUAL ZEROS
               ADD 1      TO WS-IND1
               GO  TO  1000-CARREGA-MEM-LE.
           IF  WS-NIVEL1 EQUAL WS-NIVEL1A
               AND WS-NIVEL3A EQUAL ZEROS
               ADD 1           TO WS-IND2
               GO  TO  1000-CARREGA-MEM-LE.
           IF  WS-NIVEL2 EQUAL WS-NIVEL2A
               AND WS-NIVEL4A EQUAL ZEROS
               AND WS-NIVEL1 EQUAL WS-NIVEL1A
               ADD 1           TO WS-IND3
               GO  TO  1000-CARREGA-MEM-LE.
           IF  WS-NIVEL3 EQUAL WS-NIVEL3A
               AND WS-NIVEL2 EQUAL WS-NIVEL2A
               AND WS-NIVEL1 EQUAL WS-NIVEL1A
               AND WS-NIVEL4A NOT EQUAL ZEROS
               ADD 1           TO WS-IND4
               GO  TO  1000-CARREGA-MEM-LE.
           GO  TO  1000-CARREGA-MEM-LE.
       1000-EXIT.
            EXIT.
       2000-MONTA SECTION.
       2000-000.
           DISPLAY G-TPLANO2.
           IF  LK-MENU IS NOT NUMERIC
               OR LK-MENU EQUAL ZEROS
               MOVE 01000000 TO LK-MENU.
           MOVE LK-MENU TO WS-QUEBRA-NIVEL.
       2000-100.
           IF  WS-NIVEL4 NOT EQUAL ZEROS
               PERFORM 2000-200 THRU 2000-500
               GO  TO  2000-EXIT.
           IF  WS-NIVEL3 NOT EQUAL ZEROS
               PERFORM 2000-200 THRU 2000-400
               GO  TO  2000-EXIT.
           IF  WS-NIVEL2 NOT EQUAL ZEROS
               PERFORM 2000-200 THRU 2000-300
               GO  TO  2000-EXIT.
           PERFORM 2000-200.
           GO  TO  2000-EXIT.
       2000-200.
           MOVE 3 TO WS-COL SCREEN-COL.
           MOVE 7 TO WS-LIN
           COMPUTE SCREEN-ROW = WS-LIN + WS-NIVEL1 - 1.
           MOVE WS-LIN TO TOP-ROW
           COMPUTE BOTTOM-ROW = WS-LIN + WS-IND1 - 1.
           DISPLAY "旼컴컴컴퀾ENU PRINCIPAL쳐컴컴컴�"  WITH
               BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
               AT WS-LINCOL.
           MOVE  LK-EMP-COD   TO MEN-EMP-COD.
           MOVE ZEROS         TO MEN-COD.
           START ARQMEN KEY IS NOT LESS THAN MEN-CHAVE-PRI INVALID KEY
               MOVE " M E N U "        TO WK-MSG-RT2
               PERFORM 9100-RETCOD
               GO  TO  2000-EXIT.
           IF  WK01-S1 EQUAL "9"
               MOVE " M E N U "        TO WK-MSG-RT2
               PERFORM 9100-RETCOD.
           MOVE ZEROS TO WS-IND WS-IND5.
           PERFORM 2000-NIVEL1 UNTIL WS-IND EQUAL 1
           ADD 1 TO WS-LIN
           MOVE 3 TO WS-COL
           DISPLAY "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸" AT WS-LINCOL
               WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           PERFORM 9600-MARCA-BLOCO.
       2000-300.
           MOVE 18 TO WS-COL SCREEN-COL.
           MOVE 8 TO  WS-LIN
           COMPUTE SCREEN-ROW = WS-LIN + WS-NIVEL2 - 1.
           MOVE WS-LIN TO TOP-ROW
           COMPUTE BOTTOM-ROW = WS-LIN + WS-IND2 - 1.
           DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴커"  WITH
               BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
               AT WS-LINCOL.
           MOVE  LK-EMP-COD   TO MEN-EMP-COD.
           MOVE ZEROS         TO MEN-COD.
           START ARQMEN KEY IS NOT LESS THAN MEN-CHAVE-PRI INVALID KEY
               MOVE " M E N U "        TO WK-MSG-RT2
               PERFORM 9100-RETCOD
               GO  TO  2000-EXIT.
           IF  WK01-S1 EQUAL "9"
               MOVE " M E N U "        TO WK-MSG-RT2
               PERFORM 9100-RETCOD.
           MOVE ZEROS TO WS-IND WS-IND5.
           PERFORM 2000-NIVEL2 UNTIL WS-IND EQUAL 1
           ADD 1 TO WS-LIN
           MOVE 18 TO WS-COL
           DISPLAY "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸" AT WS-LINCOL
               WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           PERFORM 9600-MARCA-BLOCO.
        2000-400.
           MOVE 33 TO WS-COL SCREEN-COL.
           MOVE 9 TO WS-LIN
           COMPUTE SCREEN-ROW = WS-LIN + WS-NIVEL3 - 1.
           MOVE WS-LIN TO TOP-ROW
           COMPUTE BOTTOM-ROW = WS-LIN + WS-IND3 - 1.
           DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴커"  WITH
               BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
               AT WS-LINCOL.
           MOVE  LK-EMP-COD   TO MEN-EMP-COD.
           MOVE ZEROS         TO MEN-COD.
           START ARQMEN KEY IS NOT LESS THAN MEN-CHAVE-PRI INVALID KEY
               MOVE " M E N U "        TO WK-MSG-RT2
               PERFORM 9100-RETCOD
               GO  TO  2000-EXIT.
           IF  WK01-S1 EQUAL "9"
               MOVE " M E N U "        TO WK-MSG-RT2
               PERFORM 9100-RETCOD.
           MOVE ZEROS TO WS-IND WS-IND5.
           PERFORM 2000-NIVEL3 UNTIL WS-IND EQUAL 1
           ADD 1 TO WS-LIN
           MOVE 33 TO WS-COL
           DISPLAY "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸" AT WS-LINCOL
               WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           PERFORM 9600-MARCA-BLOCO.
       2000-500.
           MOVE 48 TO WS-COL SCREEN-COL.
           MOVE 10 TO WS-LIN
           COMPUTE SCREEN-ROW = WS-LIN + WS-NIVEL4 - 1.
           MOVE WS-LIN TO TOP-ROW
           COMPUTE BOTTOM-ROW = WS-LIN + WS-IND4 - 1.
           DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴커"  WITH
               BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
               AT WS-LINCOL.
           MOVE  LK-EMP-COD   TO MEN-EMP-COD.
           MOVE ZEROS         TO MEN-COD.
           START ARQMEN KEY IS NOT LESS THAN MEN-CHAVE-PRI INVALID KEY
               MOVE " M E N U "        TO WK-MSG-RT2
               PERFORM 9100-RETCOD
               GO  TO  2000-EXIT.
           IF  WK01-S1 EQUAL "9"
               MOVE " M E N U "        TO WK-MSG-RT2
               PERFORM 9100-RETCOD.
           MOVE ZEROS TO WS-IND WS-IND5.
           PERFORM 2000-NIVEL4 UNTIL WS-IND EQUAL 1
           ADD 1 TO WS-LIN
           MOVE 48 TO WS-COL
           DISPLAY "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸" AT WS-LINCOL
               WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           PERFORM 9600-MARCA-BLOCO.
       2000-NIVEL1.
           READ ARQMEN NEXT AT END
               MOVE 1 TO WS-IND.
           IF  WS-IND EQUAL ZEROS
               MOVE MEN-COD      TO WS-QUEBRA-NIVELA
               IF  WS-NIVEL2A  NOT EQUAL ZEROS
                   GO  TO  2000-NIVEL1
               ELSE
                   ADD  1  TO WS-LIN
                   MOVE 3  TO WS-COL
                   DISPLAY "�                              �"
                       AT WS-LINCOL WITH
                       BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
                   MOVE 4  TO WS-COL
                   DISPLAY MEN-DESC AT WS-LINCOL
                   ADD 1 TO WS-IND5
                   MOVE MEN-PROG TO WS-PROG(WS-IND5)
                   GO  TO  2000-NIVEL1.
           MOVE 1 TO WS-IND.
       2000-NIVEL2.
           READ ARQMEN NEXT AT END
               MOVE 1 TO WS-IND.
           IF  WS-IND EQUAL ZEROS
               MOVE MEN-COD      TO WS-QUEBRA-NIVELA
               IF  WS-NIVEL1A NOT EQUAL WS-NIVEL1
                   GO  TO  2000-NIVEL2
               ELSE
                   IF  WS-NIVEL3A NOT EQUAL ZEROS
                       OR  WS-NIVEL2A EQUAL ZEROS
                       GO  TO  2000-NIVEL2
                   ELSE
                       ADD  1  TO WS-LIN
                       MOVE 18 TO WS-COL
                       DISPLAY "�                              �"
                           AT WS-LINCOL WITH
                           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
                       MOVE 19 TO WS-COL
                       DISPLAY MEN-DESC AT WS-LINCOL
                       ADD 1 TO WS-IND5
                       MOVE MEN-PROG TO WS-PROG(WS-IND5)
                       GO  TO  2000-NIVEL2.
           MOVE 1 TO WS-IND.
       2000-NIVEL3.
           READ ARQMEN NEXT AT END
               MOVE 1 TO WS-IND.
           IF  WS-IND EQUAL ZEROS
               MOVE MEN-COD      TO WS-QUEBRA-NIVELA
               IF  WS-NIVEL2A NOT EQUAL WS-NIVEL2 OR
                   WS-NIVEL1A NOT EQUAL WS-NIVEL1
                   GO  TO  2000-NIVEL3
               ELSE
                   IF  WS-NIVEL4A NOT EQUAL ZEROS
                       OR  WS-NIVEL3A EQUAL ZEROS
                       GO  TO  2000-NIVEL3
                   ELSE
                       ADD  1  TO WS-LIN
                       MOVE 33 TO WS-COL
                       DISPLAY "�                              �"
                           AT WS-LINCOL WITH
                           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
                       MOVE 34 TO WS-COL
                       DISPLAY MEN-DESC AT WS-LINCOL
                       ADD 1 TO WS-IND5
                       MOVE MEN-PROG TO WS-PROG(WS-IND5)
                       GO  TO  2000-NIVEL3.
           MOVE 1 TO WS-IND.
       2000-NIVEL4.
           READ ARQMEN NEXT AT END
               MOVE 1 TO WS-IND.
           IF  WS-IND EQUAL ZEROS
               MOVE MEN-COD      TO WS-QUEBRA-NIVELA
               IF  WS-NIVEL3A NOT EQUAL WS-NIVEL3 OR
                   WS-NIVEL2A NOT EQUAL WS-NIVEL2 OR
                   WS-NIVEL1A NOT EQUAL WS-NIVEL1
                   GO  TO  2000-NIVEL4
               ELSE
                   IF  WS-NIVEL4A EQUAL ZEROS
                       GO  TO  2000-NIVEL4
                   ELSE
                       ADD  1  TO WS-LIN
                       MOVE 48 TO WS-COL
                       DISPLAY "�                              �"
                           AT WS-LINCOL WITH
                           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
                       MOVE 49 TO WS-COL
                       DISPLAY MEN-DESC AT WS-LINCOL
                       ADD 1 TO WS-IND5
                       MOVE MEN-PROG TO WS-PROG(WS-IND5)
                       GO  TO  2000-NIVEL4.
           MOVE 1 TO WS-IND.
       2000-EXIT.
           EXIT.
       3000-OPCAO SECTION.
       3000-000.
           STOP " ".
       3000-EXIT.
           EXIT.

       9600-LOOP-CURSOR SECTION.
       9600-000.
           MOVE ZEROS                  TO WK-OPCAO.
           PERFORM 9600-MARCA-BLOCO.
           call x"af" using set-bit-pairs
                            user-key-control.
       9600-100.
           call x"af" using get-single-char
                    key-status.
           evaluate key-type
             when user-fn-key
               evaluate key-code-1
                 when f1-key       PERFORM 9600-CONDEN
      *          when f2-key       display hilite4-00
      *          when f3-key       display hilite5-00
      *          when f4-key       perform clear-pop-up
                 when esc-fn-key   GO  TO  9600-NOVO-MENU
                 when other    call x"e5"
               end-evaluate
             when adis-fn-key
               evaluate key-code-1
                 when left-arrow   GO  TO  9600-NOVO-MENU
                 when up-arrow     perform 9600-MOVE-BLOCO-CIMA
                 when down-arrow   perform 9600-MOVE-BLOCO-BAIXO
      *           when esc-arrow    GO TO FINALIZA
                 when carriage-return GO  TO  9600-PRONTO
                 when right-arrow     GO  TO  9600-PRONTO
                 when other    call x"e5"
               end-evaluate
             when norm-fn-key
               evaluate key-code-1
                 when um-arrow     MOVE 1 TO WK-OPCAO GO TO 9600-PRONTO
                 when dois-arrow   MOVE 2 TO WK-OPCAO GO TO 9600-PRONTO
                 when tres-arrow   MOVE 3 TO WK-OPCAO GO TO 9600-PRONTO
                 when quatro-arrow MOVE 4 TO WK-OPCAO GO TO 9600-PRONTO
                 when cinco-arrow  MOVE 5 TO WK-OPCAO GO TO 9600-PRONTO
                 when nove-arrow   MOVE 9 TO WK-OPCAO GO TO 9600-PRONTO
                 when other    call x"e5"
               end-evaluate
           end-evaluate.
           GO  TO  9600-100.
       9600-MOVE-BLOCO-CIMA.
           PERFORM 9600-APAGA-BLOCO.
           if  screen-row > top-row
               subtract 1 from screen-row
           else
               move bottom-row to screen-row
           end-if
           PERFORM 9600-MARCA-BLOCO.
       9600-MOVE-BLOCO-BAIXO.
           PERFORM 9600-APAGA-BLOCO.
           if  screen-row < bottom-row
               add 1 to screen-row
           else
               move top-row to screen-row
           end-if
           PERFORM 9600-MARCA-BLOCO.
       9600-APAGA-BLOCO.
           call "CBL_WRITE_SCR_ATTRS" using screen-position
                                            black-and-white
                                            string-length.
       9600-MARCA-BLOCO.
           call "CBL_WRITE_SCR_ATTRS" using screen-position
                        reverse-vid
                        string-length.
       9600-CONDEN.
           CALL "CONDEN".
           END-CALL.
       9600-PRONTO.
           COMPUTE WS-IND5 = SCREEN-ROW - TOP-ROW + 1.
           IF  WS-COL EQUAL 3 OR 4
               MOVE WS-IND5 TO WS-NIVEL1
               MOVE 1  TO WS-NIVEL2
               MOVE ZEROS TO WS-NIVEL3 WS-NIVEL4
               IF  WS-PROG(WS-IND5) NOT EQUAL SPACES
                   MOVE ZEROS TO WS-NIVEL2.
           IF  WS-COL EQUAL 18 OR 19
               MOVE WS-IND5 TO WS-NIVEL2
               MOVE 1 TO WS-NIVEL3
               MOVE ZEROS TO WS-NIVEL4
               IF  WS-PROG(WS-IND5) NOT EQUAL SPACES
                   MOVE ZEROS TO WS-NIVEL3.
           IF  WS-COL EQUAL 33 OR 34
               MOVE WS-IND5 TO WS-NIVEL3
               MOVE 1 TO WS-NIVEL4
               IF  WS-PROG(WS-IND5) NOT EQUAL SPACES
                   MOVE ZEROS TO WS-NIVEL4.
           IF  WS-COL GREATER THAN 40
               MOVE WS-IND5 TO WS-NIVEL4.
           MOVE WS-QUEBRA-NIVEL TO LK-MENU.
           IF  WS-PROG(WS-IND5) NOT EQUAL SPACES
               GO  TO  FINALIZA.
           GO  TO  9600-EXIT.
       9600-NOVO-MENU.
           MOVE LK-MENU TO WS-QUEBRA-NIVEL.
           IF  WS-COL GREATER 40
               MOVE ZEROS TO WS-NIVEL4.
           IF  WS-COL EQUAL 33 OR 34
               MOVE ZEROS TO WS-NIVEL3.
           IF  WS-COL EQUAL 18 OR 19
               MOVE ZEROS TO WS-NIVEL2.
           IF  WS-COL EQUAL 3 OR 4
               MOVE 1 TO WS-IND5
               MOVE "MA00102" TO WS-PROG(1)
               GO  TO  FINALIZA.
           MOVE WS-QUEBRA-NIVEL TO LK-MENU.
       9600-EXIT.
           EXIT.
           COPY "C:\HBSIS\FTT\MRETCOD.MP".
           COPY "C:\HBSIS\FTT\MADVERT.MP".
           COPY "C:\HBSIS\FTT\MANO2000.MP".