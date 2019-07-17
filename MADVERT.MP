      ************************************************************
      *                        MADVERT.MP                        *
      *      FUNCAO - ROTINA PARA MENSAGENS DE ADVERTENCIA       *
      ************************************************************
      *

       9300-ADVERTENCIA SECTION.
       9300-1.
           call "CBL_READ_SCR_CHATTRS" using screen-origin(2)
                          screen-buffer(2)
                          screen-attrs(2)
                          screen-string-length(2).
           PERFORM 9300-DISPLAY
           GO TO 9300-EXIT.
       9300-DISPLAY.
           DISPLAY G-TTELAADV.
           CALL X"E5".
           DISPLAY "TECLE ALGO PARA CONTINUAR..." AT 2506.
           CALL X"E5".
           ACCEPT WS-MOMENTO AT 2570 WITH AUTO-SKIP.
           call "CBL_WRITE_SCR_CHATTRS" using screen-origin(2)
                          screen-buffer(2)
                          screen-attrs(2)
                          screen-string-length(2).
       9300-TERMINA.
           GO  TO  FINALIZA.
       9300-EXIT.
           EXIT.
