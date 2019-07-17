       9900-EDITDADOS SECTION.
       9900-TELEFONE.
           MOVE WA-TELEFONE-NUM        TO WA-TELEFONE-AUX.
           MOVE WA-CODIGO-AREA-AUX     TO WA-CODIGO-AREA.
           MOVE WA-PREFIXO-AUX         TO WA-PREFIXO.
           MOVE WA-NUMERO-AUX          TO WA-NUMERO.
       9900-FAX.
           MOVE WA-FAX-NUM             TO WA-FAX-AUX.
           MOVE WAF-CODIGO-AREA-AUX    TO WAF-CODIGO-AREA.
           MOVE WAF-PREFIXO-AUX        TO WAF-PREFIXO.
           MOVE WAF-NUMERO-AUX         TO WAF-NUMERO.
       9900-TELEX.
           MOVE WA-TELEX-NUM           TO WA-TELEX-AUX.
           MOVE WA-AREA-TELEX-AUX      TO WA-AREA-TELEX.
           MOVE WA-NUMERO-TELEX-AUX    TO WA-NUMERO-TELEX.
       9900-CGC.
           MOVE WA-CGC-NUM             TO WA-CGC-CPF-AUX.
           MOVE WA-NRO-CGC-CPF-AUX     TO WA-NRO-CGC.
           MOVE WA-DIG-CGC-CPF-AUX     TO WA-DIG-CGC.
       9900-CPF.
           MOVE WA-CPF-NUM             TO WA-CGC-CPF-AUX.
           MOVE WA-NRO-CGC-CPF-AUX     TO WA-NRO-CPF.
           MOVE WA-DIG-CGC-CPF-AUX     TO WA-DIG-CPF.
       9900-CEP.
           MOVE WA-CEP-NUM             TO WA-CEP-AUX.
           MOVE WA-CEP1-AUX            TO WA-CEP1-EDIT.
           MOVE WA-CEP2-AUX            TO WA-CEP2-EDIT.
       9900-EXIT.
           EXIT.
