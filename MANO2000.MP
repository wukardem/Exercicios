      ************************************************************
      *                        MANO2000.MP                       *
      *      FUNCAO - ROTINA PARA CONVERTER DATA PARA ANO 2000   *
      ************************************************************
      *
       9000-ANO2000 SECTION.
           IF  WK-DT EQUAL ZEROS THEN
               MOVE WK-DATA TO WK-DT
           END-IF
           MOVE WK-DIA            TO WK-DIA-MIL
           MOVE WK-MES            TO WK-MES-MIL
           MOVE WK-ANO            TO WK-ANO2-MIL
           MOVE 20                TO WK-ANO1-MIL
           IF  WK-ANO > WK-1900
               MOVE 19            TO WK-ANO1-MIL.
           MOVE WK-DATA-MIL       TO WK-DATA-INV.
       9000-ANO2000-EXIT.
           EXIT.
       9000-ANO2000-6DIG SECTION.
       9000-6DIG.
           MOVE WK-DIA-MIL        TO WK-DIA.
           MOVE WK-MES-MIL        TO WK-MES.
           MOVE WK-ANO2-MIL       TO WK-ANO.
       9000-6DIG-EXIT.
           EXIT.
