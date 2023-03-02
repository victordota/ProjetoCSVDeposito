      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. EXER0212.
       AUTHOR.     VICTOR.
      *================================================================*
      *                                                                *
      *================================================================*
      *    PROGRAMA....: PSDC2EX1
      *    PROGRAMADOR.: VICTOR DOTA
      *    ANALISTA....: VICTOR DOTA                                   *
      *    DATA........: 16/01/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR ARQUIVO CSV, COM O VALOR TOTAL        *
      *                    DE DEPOSITOS E DATA DO DEPOSITO             *
      *                    MAIS RECENTE.                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *       DDNAME                                 INCLUDE/BOOK      *
      *      ARQENT01                                  ENT02112
      *      ARQSAI01                                  SAI02112
      *----------------------------------------------------------------*
      *    ROTINAS.....:                                               *
      *                                                                *
      *================================================================*
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *----------------------------------------------------------------
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
      *
       FILE-CONTROL.
      *
           SELECT ARQENT01 ASSIGN      TO UT-S-ARQENT01
                      FILE STATUS      IS WRK-FS-ARQENT01.
.
           SELECT ARQSAI01 ASSIGN       TO UT-S-ARQSAI01
                      FILE STATUS      IS WRK-FS-ARQSAI01.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================
      *                                                                *
      *----------------------------------------------------------------
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------
      *    ARQUIVO DOS REGISTROS DE ENTRADA E SAIDA                    *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    INPUT:     ARQUIVO DE ENTRADA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 30                 *
      *----------------------------------------------------------------*

       FD  ARQENT01
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS  0 RECORDS.
       01 FD-ARQENT01             PIC X(30).

      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA                                *
      *               ORG. SEQUENCIAL   -   LRECL = 38                *
      *---------------------------------------------------------------*

       FD  ARQSAI01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI01             PIC X(38).

      *
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       77 FILLER                  PIC  X(050) VALUE
             'EXER0212 - INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *
       77 WRK-PROGRAMA            PIC X(008) VALUE 'EXER0212'.
       77 WRK-MASK-QTDREG         PIC ZZ.ZZ9. 
       77 ACU-LIDOS-ARQENT01      PIC 9(005) VALUE ZEROS.
       77 ACU-GRAVA-ARQSAI01      PIC 9(005) VALUE ZEROS.
       77 ACU-DEPOSITOS           PIC S9(017) V99 COMP-3 VALUE +0. 
       77 WRK-DATA-LIDA-INV       PIC 9(008) VALUE ZEROS.
       77 WRK-DATA-RECENTE-INV    PIC 9(008) VALUE ZEROS.
       
      *
       01 WRK-CPF-ANTERIOR.
          03 WRK-COD-CPF-ANT      PIC 9(009) VALUE ZEROS.
          03 WRK-DIG-CPF-ANT      PIC 9(009) VALUE ZEROS.

       01 WRK-DATA-ANT-INV.
          05 WRK-ANO              PIC  9(004) VALUE ZEROS.
          05 WRK-MES              PIC  9(002) VALUE ZEROS.
          05 WRK-DIA              PIC  9(002) VALUE ZEROS.

       01 WRK-CABEC.
          05 WRK-CABEC-ARQSAI01   PIC  X(061) VALUE
        'CPF DO CLIENTE;DATA ULTIMO DEPOSITO;VALOR TOTAL DOS DEPOSITOS'.

       77 WRK-ARQUIVO             PIC  X(008) VALUE SPACES.
       88 WRK-CN-ARQENT01                     VALUE 'ENT01113'.
       88 WRK-CN-ARQSAI01                     VALUE 'SAI01113'.

       77 WRK-COMANDO             PIC  X(005) VALUE SPACES.
       88 WRK-CN-OPEN                         VALUE 'OPEN '.
       88 WRK-CN-CLOSE                        VALUE 'CLOSE'.
       88 WRK-CN-READ                         VALUE 'READ '.
       88 WRK-CN-WRITE                        VALUE 'WRITE'.

      *----------------------------------------------------------------
       01 FILLER                  PIC  X(050) VALUE
           'AREA PARA TRATAMENTO DE FILE-STATUS'.
      *----------------------------------------------------------------*
      *
       01 WRK-AREA-FS.
          05 WRK-FS-ARQENT01      PIC  X(002) VALUE SPACES.
             88 WRK-FS-ENT01-OK               VALUE '00'.
             88 WRK-FS-ENT01-FIM              VALUE '10'.

      *
          05 WRK-FS-ARQSAI01      PIC  X(002) VALUE SPACES.
             88 WRK-FS-SAI01-OK               VALUE '00'.
          05 WRK-FS-DISPLAY       PIC X(002)  VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'AREA DOS BOOKS DOS ARQUIVOS DE ENTRADA E SAIDA'.
      *----------------------------------------------------------------*
      *
      **** AREA ARQUIVO DE ENTRADA E SAIDA

           COPY ENT02112.
           COPY SAI02112.

      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'EXER0212 - FIM DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-PRINCIPAL SECTION.
      *----------------------------------------------------------------
      *
           PERFORM 1000-INICIALIZAR
      *
           PERFORM 3000-PROCESSAR UNTIL WRK-FS-ENT01-FIM
      *
           PERFORM 4100-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------
      *    ROTINA DE INICIALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*
      *
           SET WRK-CN-OPEN TO TRUE
           OPEN INPUT ARQENT01   
      *
           IF NOT WRK-FS-ENT01-OK       
              MOVE WRK-FS-ENT01-OK TO WRK-FS-DISPLAY
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           OPEN OUTPUT ARQSAI01 

           IF NOT WRK-FS-SAI01-OK 
              MOVE WRK-FS-SAI01-OK TO WRK-FS-DISPLAY
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF     

           PERFORM 3800-LER-DEPOSITOS

           IF WRK-FS-ENT01-FIM
              DISPLAY '************************************************'
              DISPLAY '*      ARQUIVO DE ENTRADA ENT02112 VAZIO       *'
              DISPLAY '* PROGRAMA' WRK-PROGRAMA '*'
              DISPLAY '*                   CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 4100-FINALIZAR
           END-IF

           SET WRK-CN-WRITE TO TRUE
           SET WRK-CN-ARQSAI01 TO TRUE

           WRITE FD-ARQSAI01 FROM WRK-CABEC.

           IF NOT WRK-FS-SAI01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

      *    GUARDA CPF DO PRIMEIRO REGISTRO LIDO PARA ACUMULAR OS 
      *    DEPOSITOS

           MOVE ARQENT01-CPF           TO WRK-CPF-ANTERIOR
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.
           EXIT.
      *----------------------------------------------------------------
      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 3100-TRATA-DEPOSITO
              UNTIL (ARQENT01-CPF NOT EQUAL WRK-CPF-ANTERIOR)
              OR    (WRK-FS-ENT01-FIM)
              
              PERFORM 3900-GRAVA-SAIDA

      *    REINICIALIZA ACUMULADOR E MONTA CONTROLE
      *    PARA QUEBRAS DE CPF
           
              IF NOT WRK-FS-ENT01-FIM 
                 MOVE ZEROS              TO ACU-DEPOSITOS
                 MOVE ARQENT01-CPF       TO WRK-CPF-ANTERIOR
                 MOVE WRK-DATA-LIDA-INV  TO WRK-DATA-ANT-INV
                                            WRK-DATA-RECENTE-INV       
              END-IF
              . 

      *
      *----------------------------------------------------------------*
       3000-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      * ACUMULA DEPOSITO E GUARDA DATA MAIS RECENTE 
      *----------------------------------------------------------------* 
       3100-TRATA-DEPOSITO SECTION.

           IF WRK-DATA-LIDA-INV GREATER WRK-DATA-RECENTE-INV         
              MOVE WRK-DATA-LIDA-INV   TO   WRK-DATA-RECENTE-INV
           END-IF
             COMPUTE ACU-DEPOSITOS = ACU-DEPOSITOS + ARQENT01-VAL-DEPOS

             PERFORM 3800-LER-DEPOSITOS 
             .   
       3100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*       
      
      *----------------------------------------------------------------*
      *    ROTINA DE LEITURA DO ARQUIVO ARQENT01
      *----------------------------------------------------------------*
       3800-LER-DEPOSITOS SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE           ARQENT01-REGISTRO     
           SET WRK-CN-READ      TO TRUE
           SET WRK-CN-ARQENT01  TO TRUE

           READ ARQENT01 INTO ARQENT01-REGISTRO.
      *
           EVALUATE WRK-FS-ARQENT01 
              WHEN '00'
                    ADD 1 TO ACU-LIDOS-ARQENT01
                    MOVE ARQENT01-DAT-DEPOS (1:2) TO 
                         WRK-DATA-LIDA-INV (7:2)
                    MOVE ARQENT01-DAT-DEPOS (3:2) TO
                         WRK-DATA-LIDA-INV (5:2)
                    MOVE ARQENT01-DAT-DEPOS (5:4) TO
                         WRK-DATA-LIDA-INV (1:4)
               WHEN '10'
                    CONTINUE
               WHEN OTHER 
                    MOVE WRK-FS-ARQENT01 TO WRK-FS-DISPLAY 
                    PERFORM 9000-ERROS-ARQUIVOS                                 
           END-EVALUATE 
           .
      *
      *----------------------------------------------------------------*
       3800-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------
       3900-GRAVA-SAIDA SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-CPF-ANT  TO ARQSAI01-COD-CPF
           MOVE WRK-DIG-CPF-ANT  TO ARQSAI01-DIG-CPF 
           MOVE WRK-DATA-ANT-INV TO ARQSAI01-DAT-DEPOS 
           MOVE '.' TO ARQSAI01-DAT-DEPOS (3:1)
                       ARQSAI01-DAT-DEPOS (6:1)

           MOVE '-'       TO ARQSAI01-REGISTRO (12:1)
           MOVE ';'       TO ARQSAI01-REGISTRO (15:1)
                          TO ARQSAI01-REGISTRO (26:1)        

           SET WRK-CN-WRITE TO TRUE
           SET WRK-CN-ARQSAI01 TO TRUE

           WRITE FD-ARQSAI01 FROM ARQSAI01-REGISTRO

           IF NOT WRK-FS-SAI01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           COMPUTE ACU-GRAVA-ARQSAI01 = ACU-GRAVA-ARQSAI01 + 1

           INITIALIZE ARQSAI01-REGISTRO    
           .
      *----------------------------------------------------------------*
       3900-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA DE TRATAMENTO DE ERRO
      *----------------------------------------------------------------*
       9000-ERROS-ARQUIVOS SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************************************************'
           DISPLAY '*       ERRO EM OPERACAO COM ARQUIVOS          *'
           DISPLAY '* COMANDO: ' WRK-COMANDO '*'
           DISPLAY '* ARQUIVO: ' WRK-ARQUIVO '*'
           DISPLAY '* FILE-STATUS:' WRK-FS-ARQENT01 '*'
           DISPLAY '* PROGRAMA:' WRK-PROGRAMA '*'
           DISPLAY '*                   CANCELADO                  *'
           DISPLAY '************************************************'

           PERFORM 9900-ENCERRAR 
           .
      *----------------------------------------------------------------*
       9000-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       4100-FINALIZAR SECTION.
      *----------------------------------------------------------------*
           CLOSE ARQENT01.
           CLOSE ARQSAI01.

           SET WRK-CN-CLOSE TO TRUE.

           IF NOT WRK-FS-SAI01-OK AND WRK-FS-ENT01-FIM
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF
           
           MOVE ACU-LIDOS-ARQENT01     TO WRK-MASK-QTDREG
            DISPLAY '**********************************************'
            DISPLAY '* QTDE. DE REGISTROS LIDOS   : ' WRK-MASK-QTDREG
                                                         '        *'
            MOVE ACU-GRAVA-ARQSAI01     TO WRK-MASK-QTDREG
            DISPLAY '* QTDE. DE REGISTROS GRAVADOS: ' WRK-MASK-QTDREG
                                                         '        *'
            DISPLAY '* ' WRK-PROGRAMA
                              ' FIM NORMAL                        *'
            DISPLAY '**********************************************'

           PERFORM 9900-ENCERRAR 
           .
      *----------------------------------------------------------------*
       4100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       9900-ENCERRAR SECTION.
      *----------------------------------------------------------------*
           STOP RUN.