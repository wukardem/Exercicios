       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.   VLCGCCPF.
       AUTHOR.       Odair.
       DATE-WRITTEN. 14/07/2019.
      *
      * OBJETIVO - SUB-ROTINA DE FUNCOES GENERICAS PARA MICRO
      * --------
      *            CNPJ/CPF
      *
      *
       ENVIRONMENT DIVISION.
      **********************
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
      ***************
       WORKING-STORAGE SECTION.
      *-----------------------*
       77  W77-DATA                PIC X(010)     VALUE '14/07/2019'.
       77  W77-HORA                PIC X(008)     VALUE '22:07:20'.
       77  W77-RESTO         PIC 9(002) VALUE 0.
       77  W77-DIGITO        PIC 9(001) VALUE 0.

       01  WK-NUM-COMPOSTO.
           03 WK-BYTE-NUM    PIC  9(001) OCCURS 17 TIMES
                             INDEXED BY IND1.

       01  WK-TOTAL          PIC  9(003) VALUE ZEROS.
       01  FILLER REDEFINES  WK-TOTAL.
           03 FILLER         PIC  X(002).
           03 WK-SOMA        PIC  9(001).

       01  WK-WORD           PIC  9(002) VALUE 0.
       01  FILLER REDEFINES  WK-WORD.
           03 WK-BYTE01      PIC  9(001).
           03 WK-BYTE02      PIC  9(001).

       01  WK-WORD-BINARIO       PIC  9(005) COMP-0 VALUE 0.
       01  FILLER REDEFINES  WK-WORD-BINARIO.
           03 WK-BYTE-BINARIO-02 PIC  X(001).
           03 WK-BYTE-BINARIO-01 PIC  X(001).

       LINKAGE SECTION.
       COPY VLCGCCPF.WK.
       PROCEDURE DIVISION USING SR-PARAM-VLCGCCPF.
      *******************************************
      *PROCEDURE DIVISION.
       0000-CONTROLE SECTION.
      *---------------------*
       0000.
           MOVE ZEROS TO SR-COD-RETORNO.
           
           IF SR-DES-FUNCAO EQUAL 'CGC'
                PERFORM 4000-CGC
           ELSE
				IF SR-DES-FUNCAO EQUAL 'CPF'
					PERFORM 5000-CPF
				ELSE 	
					MOVE 01 TO SR-COD-RETORNO.

           EXIT PROGRAM.

       0000-EXIT.
           EXIT.

       4000-CGC SECTION.
      *----------------*
       4000.
           IF SR-NUM-CGC IS NUMERIC
                NEXT SENTENCE
           ELSE MOVE 99 TO SR-COD-RETORNO
                GO TO 4000-EXIT.

           MOVE ZEROS      TO WK-TOTAL
                              WK-WORD
                              SR-COD-RETORNO.
           MOVE SR-NUM-CGC TO WK-NUM-COMPOSTO.
           MOVE 0 TO SR-COD-RETORNO.
      *    GO TO 4000-EXIT.
           SET IND1 TO 1.
           PERFORM 4100-CALCULA-DIGITO-CGC-1.
           IF SR-COD-RETORNO EQUAL ZEROS
                SET IND1 TO 1
                MOVE ZEROS TO WK-TOTAL
                              WK-WORD
                PERFORM 4200-CALCULA-DIGITO-CGC-2
                IF SR-COD-RETORNO EQUAL ZEROS
                     SET IND1 TO 1
                     MOVE ZEROS TO WK-TOTAL
                                   WK-WORD
                     PERFORM 4300-CALCULA-DIGITO-CGC-3
           ELSE NEXT SENTENCE.
       4000-EXIT.
            EXIT.

       4100-CALCULA-DIGITO-CGC-1 SECTION.
      *---------------------------------*
       4100.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 2.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-BYTE01 + WK-BYTE02.
           SET IND1 UP BY 1.
           IF IND1 EQUAL 8
                IF  WK-BYTE-NUM(1) EQUAL ZEROS
                    SUBTRACT 1 FROM WK-SOMA.
           IF IND1 EQUAL 8
                GO TO 4100-FIM
           ELSE NEXT SENTENCE.
       4100-01.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-BYTE-NUM(IND1).
           SET IND1 UP BY 1.
           GO TO 4100.
       4100-FIM.
           IF WK-SOMA EQUAL 0
                MOVE ZEROS TO W77-DIGITO
           ELSE COMPUTE W77-DIGITO = 10 - WK-SOMA.
           IF W77-DIGITO EQUAL WK-BYTE-NUM(IND1)
                NEXT SENTENCE
           ELSE MOVE 2 TO SR-COD-RETORNO.
       4100-EXIT.
            EXIT.

       4200-CALCULA-DIGITO-CGC-2 SECTION.

      *---------------------------------*
       4200.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 5.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 4.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 3.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 2.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 9.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 8.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 7.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 6.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 5.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 4.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 3.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 2.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
       4200-FIM.
           DIVIDE WK-TOTAL BY 11 GIVING WK-TOTAL REMAINDER W77-RESTO.
           IF W77-RESTO EQUAL 0 OR 1
                MOVE ZEROS TO W77-DIGITO
           ELSE COMPUTE W77-DIGITO = 11 - W77-RESTO.
           IF W77-DIGITO EQUAL WK-BYTE-NUM(IND1)
                NEXT SENTENCE
           ELSE MOVE 2 TO SR-COD-RETORNO.
       4200-EXIT.
            EXIT.

       4300-CALCULA-DIGITO-CGC-3 SECTION.
      *---------------------------------*
       4300.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 6.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 5.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.

           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 4.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 3.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 2.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 9.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 8.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 7.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 6.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 5.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 4.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 3.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 2.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
       4300-FIM.
           DIVIDE WK-TOTAL BY 11 GIVING WK-TOTAL REMAINDER W77-RESTO.
           IF W77-RESTO EQUAL 0 OR 1
                MOVE ZEROS TO W77-DIGITO
           ELSE COMPUTE W77-DIGITO = 11 - W77-RESTO.
           IF W77-DIGITO EQUAL WK-BYTE-NUM(IND1)
                NEXT SENTENCE
           ELSE MOVE 2 TO SR-COD-RETORNO.
       4300-EXIT.
            EXIT.


       5000-CPF SECTION.
      *----------------*
       5000.
      ** DESATIVA CONSISTENCIA
           MOVE ZEROS TO SR-COD-RETORNO
      *    GO TO 5000-EXIT.
      ** FIM

           IF SR-NUM-CPF IS NUMERIC
                NEXT SENTENCE
           ELSE MOVE 99 TO SR-COD-RETORNO
                GO TO 5000-EXIT.


           MOVE ZEROS TO WK-TOTAL
                         WK-WORD
                         SR-COD-RETORNO.

           MOVE SR-NUM-CPF TO WK-NUM-COMPOSTO.
           SET IND1 TO 1.
           PERFORM 5100-CALCULA-DIGITO-CPF-1.
           IF SR-COD-RETORNO EQUAL ZEROS
              SET IND1 TO 1
              MOVE ZEROS TO WK-TOTAL
                            WK-WORD
              PERFORM 5200-CALCULA-DIGITO-CPF-2.

       5000-EXIT.
           EXIT.

       5100-CALCULA-DIGITO-CPF-1 SECTION.
      *---------------------------------*
       5100.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 10.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 9.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 8.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 7.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 6.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 5.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 4.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 3.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 2.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
       5100-FIM.
           DIVIDE WK-TOTAL BY 11 GIVING WK-TOTAL REMAINDER W77-RESTO.
           IF W77-RESTO = 0 OR 1
              MOVE 0 TO W77-DIGITO
           ELSE COMPUTE W77-DIGITO = 11 - W77-RESTO.
           IF W77-DIGITO = WK-BYTE-NUM(IND1)
              NEXT SENTENCE
           ELSE MOVE 2 TO SR-COD-RETORNO.
       5100-EXIT.
            EXIT.

       5200-CALCULA-DIGITO-CPF-2 SECTION.

      *---------------------------------*
       5200.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 10.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 9.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 8.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 7.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 6.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 5.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 4.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 3.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
           COMPUTE WK-WORD = WK-BYTE-NUM(IND1) * 2.
           COMPUTE WK-TOTAL = WK-TOTAL + WK-WORD.
           SET IND1 UP BY 1.
       5200-FIM.
           DIVIDE WK-TOTAL BY 11 GIVING WK-TOTAL REMAINDER W77-RESTO.
           IF W77-RESTO EQUAL 0 OR 1
              MOVE ZEROS TO W77-DIGITO
           ELSE COMPUTE W77-DIGITO = 11 - W77-RESTO.
           IF W77-DIGITO EQUAL WK-BYTE-NUM(IND1)
              NEXT SENTENCE
           ELSE MOVE 2 TO SR-COD-RETORNO.
       5200-EXIT.
            EXIT.
