      ************************************************************
      *                        MRETCOD.MP                        *
      *      FUNCAO - ROTINA DE TESTE PARA COD. DE RETORNO       *
      ************************************************************
      *
       9100-RETCOD SECTION.
       9100-1.
           MOVE "Entrar em contato com o Programador..."
                                       TO WK-MSG-RT4.
           IF  WK01-RETCOD EQUAL "00"
               GO TO 9100-EXIT.
           call "CBL_READ_SCR_CHATTRS"
                 using screen-origin-A screen-buffer-A
                       screen-attrs-A  screen-string-length-A
           end-call
           MOVE WK01-S1                TO WK01-S1-DISP.
           MOVE "-"                    TO WK01-S2-DISP.
           MOVE WK01-S2-BIN            TO WK01-S3-DISP.
           IF  WK01-S1 NOT EQUAL "9"
               MOVE WK01-S1            TO WK01-S1-DISP
               MOVE WK01-S2            TO WK01-S2-DISP
               MOVE ZEROS              TO WK01-S3-DISP.
           IF  WK01-RETCOD-DISP EQUAL "35000"
               MOVE "Arquivo  de    "  TO WK-MSG-RT1
               MOVE "nao encontrado "  TO WK-MSG-RT3
               GO TO 9100-DISPLAY.
            IF  WK01-RETCOD-DISP EQUAL "23000"
               MOVE "Codigo   de    "  TO WK-MSG-RT1
               MOVE "nao cadastrado "  TO WK-MSG-RT3
               MOVE "Ver codigo no cadastro ..."
                                       TO WK-MSG-RT4
               PERFORM 9100-DISPLAY
               GO TO 9100-EXIT.
           IF  WK01-RETCOD-DISP EQUAL "05000"
               MOVE "Arquivo   de   "  TO WK-MSG-RT1
               MOVE "nao encontrado "  TO WK-MSG-RT3
               MOVE "OK Criado novo arquivo. Tecle algo..."
                                       TO WK-MSG-RT4
               PERFORM 9100-DISPLAY
               GO TO 9100-EXIT.

           IF  WK01-RETCOD-DISP EQUAL "9-068"
               MOVE "Codigo   de    "  TO WK-MSG-RT1
               MOVE "em alteracao   "  TO WK-MSG-RT3
               MOVE "Chame-o novamente apos usuario liberar..."
                                       TO WK-MSG-RT4
               PERFORM 9100-DISPLAY
               GO TO 9100-EXIT.
           MOVE "Probl.no arq de"  TO WK-MSG-RT1
           MOVE "               "  TO WK-MSG-RT3.
       9100-DISPLAY.
           DISPLAY G-TTELART.
           PERFORM 9100-PARA.
           call "CBL_WRITE_SCR_CHATTRS"
                 using screen-origin-A screen-buffer-A
                       screen-attrs-A  screen-string-length-A
           end-call.
       9100-TERMINA.
           GO  TO  FINALIZA.
       9100-PARA.
           call "CBL_READ_SCR_CHATTRS"
                 using screen-origin-B screen-buffer-B
                       screen-attrs-B  screen-string-length-B
           end-call
           DISPLAY WK-LIMPA AT 2506.
           CALL X"E5".
           DISPLAY G-TBARRA2.
           CALL X"E5".
           ACCEPT WS-MOMENTO AT 2570 WITH AUTO-SKIP.
           ACCEPT WK01-TECLA FROM ESCAPE KEY.
           call "CBL_WRITE_SCR_CHATTRS"
                 using screen-origin-B screen-buffer-B
                       screen-attrs-B  screen-string-length-B
           end-call.
       9100-EXIT.
           EXIT.
