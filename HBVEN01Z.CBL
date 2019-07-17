       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HBCL001Z.
       AUTHOR.        Odair Garcia Arouca.
       DATE-WRITTEN.  14/07/2019.
      ********* Favor incluir alteracoes efetuadas *******************
      * - PROGRAMA DE ZOOM CADASTRO DE VENDEDORES
      *
      *
      *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "C:\HBSIS\FTT\FVEN0001.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "C:\HBSIS\FTT\FVEN0001.FD".
       WORKING-STORAGE SECTION.
       77  WS-VEN-COD                 PIC 9(007)  VALUE ZEROS.
       77  WS-MOMENTO                 PIC X       VALUE SPACES.
       77  WS-ULTIMO                  PIC s9(002)  VALUE ZEROS.
       77  WS-IND                     PIC S9(002)  VALUE 0.
       77  WS-IND-TELA                PIC s9(002).
       77  WS-DATA-EDIT               PIC 99/99/99.
       77  WS-VERSAO                  PIC X(010)  VALUE "06/08/2003".
       77  WS-INICIAL                 PIC X(057) VALUE SPACES.
       77  WS-TECLA                   PIC X(002) VALUE "00".
       77  WS-COD-EDITADO             PIC X(020).
       77  WS-LIMPA                   PIC X(077) VALUE SPACES.
       77  WS-CAMINHO                 PIC X(030).
       01  W01-LINCOL.
           03  W01-LIN                PIC 9(02).
           03  W01-COL                PIC 9(02).
       01  W01-CODIGO    PIC 9(007) OCCURS 15 TIMES.
       01  W01-DESCRICAO PIC X(060) OCCURS 15 TIMES.
       01  W01-TELA      PIC X(080) OCCURS 50 TIMES.

       01 screen-origin         pic 9(4) comp-x value 0.
       01 screen-buffer         pic x(2000).
       01 screen-attrs          pic x(2000).
       01 screen-string-length  pic 9(4) comp-x value 2000.

       01 screen-origin2        pic 9(4) comp-x value 0.
       01 screen-buffer2        pic x(2000).
       01 screen-attrs2         pic x(2000).
       01 screen-string-length2 pic 9(4) comp-x value 2000.

       01 screen-position.
          05 screen-row         pic 9(2) comp-x value 04.
          05 screen-col         pic 9(2) comp-x value 04.
       01 string-length         pic 9(4) comp-x value 69.

       01 top-row               pic 9(2) comp-x value 04.
       01 bottom-row            pic 9(2) comp-x value 18.

       01 reverse-vid           pic x(69) value all x"70".
       01 black-and-white       pic x(69) value all x"07".

       01 get-single-char       pic 9(2) comp-x value 26.
       01 key-status.
          05 key-type           pic x.
          05 key-code-1         pic 9(2) comp-x.
          05 key-code-2         pic 9(2) comp-x.

       01 set-bit-pairs          pic 9(2) comp-x value 1.
       01 user-key-control.
          05 user-key-setting   pic 9(2) comp-x value 1.
          05 filler             pic x value "1".
          05 first-user-key     pic 9(2) comp-x value 1.
          05 number-of-keys     pic 9(2) comp-x value 4.

       78 pgup-fn-key           value 53.
       78 pgdown-fn-key         value 54.
       78 esc-fn-key            value 0.
       78 user-fn-key           value "1".
       78 adis-fn-key           value "2".
       78 norm-fn-key           value "3".
       78 f1-key                value 1.
       78 f2-key                value 2.
       78 f3-key                value 3.
       78 f4-key                value 4.

       78 carriage-return       value 0.
       78 up-arrow              value 5.
       78 esc-arrow             value 27.
       78 down-arrow            value 6.
       78 um-arrow              value 49.
       78 dois-arrow            value 50.
       78 tres-arrow            value 51.
       78 quatro-arrow          value 52.
       78 cinco-arrow           value 53.
       78 nove-arrow            value 57.
       COPY "C:\HBSIS\FTT\WRETCOD.WK".

       LINKAGE SECTION.
       COPY "C:\HBSIS\FTT\WCTRAN01.WK".
       SCREEN SECTION.
       COPY "C:\HBSIS\FTT\TZOOMVEN.SS".
       COPY "C:\HBSIS\FTT\TTELART.SS".
       COPY "C:\HBSIS\FTT\TBARRA3.SS".
       COPY "C:\HBSIS\FTT\TBARRA2.SS".
      *
       PROCEDURE DIVISION USING LK-TRANSFERENCIA.
      *PROCEDURE DIVISION.
       MOVE "HBCL001Z" TO LK-PGM(3).
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-LOOP.
       FINALIZA.
           call "CBL_WRITE_SCR_CHATTRS" using screen-origin
                          screen-buffer
                          screen-attrs
                          screen-string-length.
           MOVE " " TO LK-PGM(3).
           CLOSE ARQVEN.
           EXIT PROGRAM.
      *
       1000-INICIO SECTION.
       1000-000-SALVATELA.
           call "CBL_READ_SCR_CHATTRS" using screen-origin
                          screen-buffer
                          screen-attrs
                          screen-string-length.

           MOVE "C:\HBSIS\DB\ARQVEN.DAT" TO WS-CAMINHO.
           OPEN INPUT ARQVEN.
           MOVE " VENDEDORES " TO WK-MSG-RT2.
           PERFORM 9100-RETCOD.
           IF  WK01-RETCOD NOT EQUAL ZEROS
               GO  TO  FINALIZA.
       1000-EXIT.
           EXIT.
       2000-LOOP SECTION.
       2000-000.
           MOVE ZEROS TO WS-IND.
           MOVE SPACES TO WS-INICIAL.
           DISPLAY G-TZOOMVEN.
           ACCEPT WS-INICIAL AT 0210.
      *    ACCEPT WS-INICIAL AT 0518 WITH UPPER.
           ACCEPT WS-TECLA FROM ESCAPE KEY.
           IF  WS-TECLA EQUAL "01"
               MOVE ZEROS TO LK-COD
               GO  TO  2000-EXIT.
           MOVE WS-INICIAL TO VEN-NOME W01-TELA (1).
           START ARQVEN KEY IS GREATER THAN OR EQUAL
                 VEN-NOME INVALID KEY
                 DISPLAY "NAO EXISTE ESTA INICIAL NO ARQUIVO" AT 2506
                 WITH BEEP
                 GO TO 2000-000.
      *    READ ARQVEN NEXT WITH IGNORE LOCK.
           READ ARQVEN NEXT.
           MOVE VEN-NOME TO W01-TELA(1).
           MOVE 1 TO WS-IND-TELA WS-IND WS-ULTIMO.
           PERFORM 4000-LE-SEGUE.
           IF  WS-ULTIMO GREATER THAN 1
               ADD 1 TO WS-IND-TELA.
           MOVE W01-DESCRICAO(WS-ULTIMO) TO W01-TELA(WS-IND-TELA).
           PERFORM 9600-LOOP-CURSOR.
           GO  TO  2000-000.
       2000-EXIT.
           EXIT.
       4000-LE-SEGUE SECTION.
       4000-000.
           MOVE W01-TELA(WS-IND-TELA)  TO VEN-NOME.
           START ARQVEN KEY IS GREATER THAN OR EQUAL
               VEN-NOME INVALID KEY
               DISPLAY " " AT 2578 WITH BEEP
               GO  TO  4000-EXIT
           END-START
           MOVE 1 TO WS-IND.
           MOVE 5 TO W01-LIN.
       4000-LE.
           READ ARQVEN NEXT AT END
               COMPUTE BOTTOM-ROW = 2 + WS-IND
               PERFORM 6000-000 UNTIL W01-LIN GREATER THAN 19
               MOVE 1 TO WS-ULTIMO
               GO  TO  4000-EXIT.
           IF  WK01-RETCOD NOT EQUAL "00"
               IF  WK01-RETCOD NOT EQUAL "02"
                   COMPUTE BOTTOM-ROW = 2 + WS-IND
                   MOVE 1 TO WS-ULTIMO
                   MOVE "VENDEDORES" TO WK-MSG-RT2
                   PERFORM 9100-RETCOD
                   GO  TO  4000-EXIT.
           MOVE ZEROS TO W01-CODIGO(WS-IND)
           MOVE VEN-COD       TO W01-CODIGO   (WS-IND) WS-COD-EDITADO.
           MOVE VEN-NOME TO W01-DESCRICAO(WS-IND)

           MOVE 04 TO W01-COL.
           DISPLAY WS-COD-EDITADO AT W01-LINCOL
           MOVE 13 TO W01-COL.
           DISPLAY W01-DESCRICAO(WS-IND) AT W01-LINCOL

           MOVE WS-IND TO WS-ULTIMO.
           ADD 1 TO WS-IND W01-LIN.
           IF  WS-IND GREATER 15
               COMPUTE BOTTOM-ROW = 5 + WS-IND
               GO  TO  4000-EXIT.
           GO  TO  4000-LE.
       4000-EXIT.
           EXIT.
       6000-LIMPA SECTION.
       6000-000.
           MOVE 04 TO W01-COL.
           DISPLAY WS-LIMPA AT W01-LINCOL.
           ADD 1 TO W01-LIN.
       6000-EXIT.
           EXIT.
       9600-LOOP-CURSOR SECTION.
       9600-000.
           PERFORM 9600-MARCA-BLOCO.
           call x"af" using set-bit-pairs
                            user-key-control.
       9600-100.
           call x"af" using get-single-char
                    key-status.
           evaluate key-type
             when user-fn-key
               evaluate key-code-1
                 when f1-key        GO  TO  9600-INC
      *          when f2-key        display hilite4-00
      *          when f3-key        display hilite5-00
      *          when f4-key        perform clear-pop-up
                 when esc-fn-key    MOVE TOP-ROW TO SCREEN-ROW
                                    GO  TO  9600-EXIT
                 when pgup-fn-key   GO  TO  9600-PGUP
                 when pgdown-fn-key GO  TO  9600-PGDOWN
                 when other    call x"e5"
               end-evaluate
             when adis-fn-key
               evaluate key-code-1
                 when up-arrow     perform 9600-MOVE-BLOCO-CIMA
                 when down-arrow   perform 9600-MOVE-BLOCO-BAIXO
                 when esc-arrow    MOVE TOP-ROW TO SCREEN-ROW
                                   GO TO 9600-EXIT
                 when carriage-return GO  TO  9600-PRONTO
                 when other    call x"e5"
               end-evaluate
             when norm-fn-key
               evaluate key-code-1
                 when um-arrow     GO TO 9600-PRONTO
                 when dois-arrow   GO TO 9600-PRONTO
                 when tres-arrow   GO TO 9600-PRONTO
                 when quatro-arrow GO TO 9600-PRONTO
                 when cinco-arrow  GO TO 9600-PRONTO
                 when nove-arrow   GO TO 9600-PRONTO
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
       9600-PRONTO.
           MOVE ZEROS TO LK-COD.
           COMPUTE WS-IND = SCREEN-ROW - 3.
           MOVE ZEROS TO LK-COD
           MOVE W01-CODIGO(WS-IND) TO LK-COD.
           GO  TO  FINALIZA.
       9600-INC.
           IF  LK-PGM(2) EQUAL "HBCL001Z"
               GO  TO  ERRO.

           call "CBL_READ_SCR_CHATTRS" using screen-origin2
                          screen-buffer2
                          screen-attrs2
                          screen-string-length2.

           CALL   "HBVEN001" USING LK-TRANSFERENCIA
                                  ON EXCEPTION GO TO ERRO.
           CANCEL "HBVEN001".

           call "CBL_WRITE_SCR_CHATTRS" using screen-origin2
                          screen-buffer2
                          screen-attrs2
                          screen-string-length2.

      ***  GO  TO  9600-100.
           GO  TO  2000-LOOP.
       ERRO.
           call "CBL_READ_SCR_CHATTRS" using screen-origin-B
                          screen-buffer-B
                          screen-attrs-B
                          screen-string-length-B.
           MOVE "P r o g r a m a"  TO WK-MSG-RT1
           MOVE "ja esta ativo  "  TO WK-MSG-RT3
           MOVE "Op‡Æo liberada somente em consulta ao cadastro"
                                   TO WK-MSG-RT4
           move "HBVEN001"          TO WK-MSG-RT2.
           DISPLAY G-TTELART.
           PERFORM 9100-PARA.
           call "CBL_WRITE_SCR_CHATTRS" using screen-origin-B
                          screen-buffer-B
                          screen-attrs-B
                          screen-string-length-B.
           GO  TO  9600-100.
       9600-PGUP.
           PERFORM 9600-APAGA-BLOCO.
           MOVE TOP-ROW TO SCREEN-ROW.
           SUBTRACT 2 FROM WS-IND-TELA.
           IF  WS-IND-TELA LESS THAN 1
               MOVE 1 TO WS-IND-TELA.
           PERFORM 4000-LE-SEGUE.
           PERFORM 9600-MARCA-BLOCO.
           IF  WS-ULTIMO GREATER THAN 1
               ADD 1 TO WS-IND-TELA.
           GO  TO  9600-100.
       9600-PGDOWN.
           PERFORM 9600-APAGA-BLOCO.
           MOVE TOP-ROW TO SCREEN-ROW.
           PERFORM 4000-LE-SEGUE.
           PERFORM 9600-MARCA-BLOCO.
           IF  WS-ULTIMO GREATER THAN 1
               ADD 1 TO WS-IND-TELA.
           IF  WS-IND-TELA GREATER THAN 40
               MOVE 40 TO WS-IND-TELA
               MOVE "PASSOU 40 TELAS, TENTE COM OUTRA INICIAL..."
                   TO WK-MENSAGEM
               PERFORM 9100-PARA.
           MOVE W01-DESCRICAO(WS-ULTIMO) TO W01-TELA(WS-IND-TELA).
           GO  TO  9600-100.
       9600-EXIT.
           EXIT.
      *
       COPY "C:\HBSIS\FTT\MRETCOD.MP".

