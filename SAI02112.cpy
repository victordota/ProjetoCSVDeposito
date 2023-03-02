      ******************************************************************
      * NOME BOOK : SAI02112
      * DESCRICAO : ARQUIVO DE DEPOSITOS EM CONTA-CORRENTE
      * TAMANHO   :  38 BYTES
      ************************* DADOS DE SAIDA ***********************
      * COD-CPF          : CPF DO CORRENTISTA
      * DIG-CPF          : DIGITO VERIFICADOR DO CPF
      * DAT-DEPOS        : DATA DO DEPOSITO  (dd-mm-aaaa)
      * VAL-DEPOS        : VALOR DO DEPOSITO
      ******************************************************************
          01 ARQSAI01-REGISTRO.
             03 ARQSAI01-COD-CPF        PIC 999.999.999.
             03 FILLER                  PIC X(01).
             03 ARQSAI01-DIG-CPF        PIC 99.
             03 FILLER                  PIC X(01).
             03 ARQSAI01-DAT-DEPOS      PIC X(10)  VALUE SPACES.
             03 FILLER                  PIC X(01).
             03 ARQSAI01-VAL-DEPOS      PIC Z.ZZZ.ZZ9,99.
