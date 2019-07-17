       TELA SECTION.
           call "CBL_READ_SCR_CHATTRS" using screen-origin(1)
                          screen-buffer(1)
                          screen-attrs(1)
                          screen-string-length(1).
           EVALUATE TRUE
           WHEN WS-EMAIL
              PERFORM E-MAIL
              GO TELA-EXIT
           WHEN WS-HTML
              PERFORM MOSTRA-HTML
              GO TELA-EXIT
           WHEN OTHER
              DISPLAY "LIST" UPON ENVIRONMENT-NAME
              ACCEPT WS-COMANDO-1 FROM ENVIRONMENT-VALUE
           END-EVALUATE

           MOVE WS-RDISCO    TO WS-COMANDO-2
           DISPLAY WS-PARAMETRO-DOS UPON COMMAND-LINE

           CALL X"91" USING WS-RESULTADO WS-FUNCAO-DOS WS-LINHA-COMANDO.
           MOVE WS-RESULTADO TO SR-COD-RETORNO.

           IF SR-COD-RETORNO NOT EQUAL ZEROS
              STRING "Erro na execu‡Æo do comando de listagem ! Nro. ",
                 SR-COD-RETORNO DELIMITED BY SIZE INTO MSG
              PERFORM MENSAGEM.

       TELA-EXIT.
      *    CALL "CBL_DELETE_FILE" USING WS-SAIDA-REL.

           call "CBL_WRITE_SCR_CHATTRS" using screen-origin(1)
                          screen-buffer(1)
                          screen-attrs(1)
                          screen-string-length(1).
           EXIT.

       E-MAIL SECTION.
       E-MAIL-1.
           INITIALIZE WS-COMANDO-1
           STRING "COMMAND /C COPY "
                   "3.CFG+" WS-RDISCO "+" "4.CFG "
                       DELIMITED BY SIZE INTO WS-COMANDO-1
           ACCEPT WS-COMANDO-1 FROM ENVIRONMENT-VALUE
           INITIALIZE WS-COMANDO-2
           STRING WS-DIAMES ".EML"
                  DELIMITED BY SIZE INTO WS-COMANDO-2
           DISPLAY WS-PARAMETRO-DOS UPON COMMAND-LINE

           CALL X"91" USING WS-RESULTADO WS-FUNCAO-DOS WS-LINHA-COMANDO.
           MOVE WS-RESULTADO TO SR-COD-RETORNO.

           IF SR-COD-RETORNO NOT EQUAL ZEROS
              STRING "Erro na execu‡Æo do comando de listagem ! Nro. ",
                 SR-COD-RETORNO DELIMITED BY SIZE INTO MSG
              PERFORM MENSAGEM.

           INITIALIZE WS-COMANDO-1
           MOVE "START /W" TO WS-COMANDO-1
           ACCEPT WS-COMANDO-1 FROM ENVIRONMENT-VALUE

           INITIALIZE WS-COMANDO-2
           STRING WS-DIAMES ".EML"
                  DELIMITED BY SIZE INTO WS-COMANDO-2
           DISPLAY WS-PARAMETRO-DOS UPON COMMAND-LINE

           CALL X"91" USING WS-RESULTADO WS-FUNCAO-DOS WS-LINHA-COMANDO.
           MOVE WS-RESULTADO TO SR-COD-RETORNO.

           IF SR-COD-RETORNO NOT EQUAL ZEROS
              STRING "Erro na execu‡Æo do comando de listagem ! Nro. ",
                 SR-COD-RETORNO DELIMITED BY SIZE INTO MSG
              PERFORM MENSAGEM.

       E-MAIL-EXIT.
           EXIT.
      *
       MOSTRA-HTML SECTION.
       HTML-1.
           STRING "COMMAND /C START /W "
                                     DELIMITED BY SIZE INTO WS-COMANDO-1
           ACCEPT WS-COMANDO-1 FROM ENVIRONMENT-VALUE

           INITIALIZE WS-COMANDO-2
           STRING WS-RDISCO
                  DELIMITED BY SIZE INTO WS-COMANDO-2
           DISPLAY WS-PARAMETRO-DOS UPON COMMAND-LINE

           CALL X"91" USING WS-RESULTADO WS-FUNCAO-DOS WS-LINHA-COMANDO.
           MOVE WS-RESULTADO TO SR-COD-RETORNO

           IF SR-COD-RETORNO <> ZEROS THEN
              STRING "Erro na execu‡Æo do comando de listagem ! Nro. ",
                 WK01-RETCOD DELIMITED BY SIZE INTO MSG
              PERFORM MENSAGEM.

       MOSTRA-HTML-EXIT.
           EXIT.
