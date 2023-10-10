       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG005.
      *AUTHOR.       RICARDO SATOSHI.
      *DATE-WRITTEN. 31/08/2023.
      *--------------------------------------------------------------*
      * DISCIPLINA PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: RECEBER DADOS DA SYSIN(ACCEPT)
      *           CALCULAR A MEDIA ARITMETICA BIMESTRAL
      *--------------------------------------------------------------*
      *------------------> HISTORICO - MANUTENCAO <------------------*
      * VERSAO  MES/ANO  NR.DOC  IDENT.  DESCRICAO
      * ------  -------  ------  ------  -------------------------   *
      *  V01    FEV/2013 010001  SISTEMA MOSTRA SYSOUT
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS "R$ " WITH PICTURE SYMBOL "$"
           .
       INPUT-OUTPUT SECTION.
      *---------------------*
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05 WS-FIM                   PIC X(01)    VALUE 'N'.
           05 WS-CTLIDO                PIC 9(02)    VALUE 0.
           05 WS-TOTAL-NT1             PIC 9(04)V99 VALUE 0.
           05 WS-TOTAL-NT2             PIC 9(04)V99 VALUE 0.
           05 WS-TOTAL-ALUNOS          PIC 9(02)    VALUE 0.
           05 WS-QTD-HOMENS            PIC 9(02)    VALUE 0.
           05 WS-QTD-MULHERES          PIC 9(02)    VALUE 0.
           05 WS-QTD-MA-6              PIC 9(02)    VALUE 0.
           05 WS-TOTAL-MEDIA-G         PIC 9(04)V99 VALUE 0.
           05 WS-PERCENT-COUNTER       PIC 9(04)    VALUE 0.
           05 WS-MEDIA                 PIC 9(04)V99 VALUE 0.

      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-NUMERO-IN        PIC 9(04).
           05 WS-NOME-IN          PIC X(20).
           05 WS-SEXO-IN          PIC X(01).
           05 WS-IDADE-IN         PIC 9(02).
           05 WS-CURSO-IN         PIC X(12).
           05 WS-NOTA1-IN         PIC 9(02)V99.
           05 WS-NOTA2-IN         PIC 9(02)V99.

       01  WS-REG-SYSOUT.
           05 WS-NUM              PIC X(05).
           05 FILLER              PIC X(01)       VALUE SPACE.
           05 WS-NOM              PIC X(20).
           05 FILLER              PIC X(01)       VALUE SPACE.
           05 WS-SEX              PIC X(02).
           05 FILLER              PIC X(01)       VALUE SPACE.
           05 WS-IDA              PIC X(03).
           05 FILLER              PIC X(01)       VALUE SPACE.
           05 WS-CUR              PIC X(13).
           05 FILLER              PIC X(01)       VALUE SPACE.
           05 WS-NT1              PIC Z9,99.
           05 FILLER              PIC X(01)       VALUE SPACES.
           05 WS-NT2              PIC Z9,99.
           05 FILLER              PIC X(01)       VALUE SPACES.
           05 WS-MED              PIC Z9,99.
           05 WS-MG               PIC 9,99.
           05 WS-QTD-MA6-E        PIC Z9.
           05 WS-PERCENT-M6       PIC 99,99.
           05 FILLER              PIC X(01)       VALUE "%".

       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-CGPRG005.

           DISPLAY '--------------------------------------'
           DISPLAY 'ATIVIDADE 5'
           DISPLAY 'IZABEL '
           DISPLAY 'CALCULO DA MEDIA DOS ALUNOS A PARTIR DA SYSIN'
           DISPLAY '--------------------------------------'

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    LEITURA DADOS DA SYSIN
      *--------------------------------------------------------------*
       025-LER-SYSIN.

           ACCEPT WS-REG-SYSIN  FROM SYSIN

           IF WS-REG-SYSIN = ALL '9'
              MOVE   'S'     TO  WS-FIM
           ELSE
              ADD 1  TO WS-CTLIDO
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN ATE FIM DOS REGISTROS
      *--------------------------------------------------------------*
       030-PROCESSAR.

           COMPUTE WS-MEDIA = (WS-NOTA1-IN + WS-NOTA2-IN) / 2

           ADD WS-NOTA1-IN   TO WS-TOTAL-NT1
           ADD WS-NOTA2-IN   TO WS-TOTAL-NT2
           ADD WS-MEDIA      TO WS-TOTAL-MEDIA-G
           ADD 1             TO WS-TOTAL-ALUNOS

           MOVE WS-NUMERO-IN TO WS-NUM
           MOVE WS-NOME-IN   TO WS-NOM
           MOVE WS-SEXO-IN   TO WS-SEX
           MOVE WS-IDADE-IN  TO WS-IDA
           MOVE WS-CURSO-IN  TO WS-CUR
           MOVE WS-NOTA1-IN  TO WS-NT1
           MOVE WS-NOTA2-IN  TO WS-NT2

           IF WS-SEX = 'M'
               ADD 1 TO WS-QTD-HOMENS
           ELSE
               ADD 1 TO WS-QTD-MULHERES
           END-IF.

           IF WS-MEDIA < 6
              ADD 1 TO WS-QTD-MA-6
              ADD 1 TO WS-PERCENT-COUNTER
           END-IF.

           MOVE WS-QTD-MA-6           TO WS-QTD-MA6-E
           MOVE WS-PERCENT-COUNTER    TO WS-PERCENT-M6

           IF WS-TOTAL-ALUNOS > 0
              COMPUTE WS-MG = (WS-TOTAL-MEDIA-G / WS-CTLIDO)                -
              COMPUTE WS-PERCENT-M6 = (WS-QTD-MA-6 / WS-TOTAL-ALUNOS)       -
                 * 100

           MOVE WS-MEDIA              TO WS-MED

           DISPLAY WS-NUM WS-NOM WS-SEX WS-IDA WS-CUR WS-NT1 WS-NT2         -
            WS-MED

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG005        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS..: ' WS-CTLIDO
           DISPLAY ' * TOTAL MULHERES...: ' WS-QTD-MULHERES
           DISPLAY ' * TOTAL HOMENS.....: ' WS-QTD-HOMENS
           DISPLAY ' * MEDIA GERAL DOS ALUNOS.......: ' WS-MG
           DISPLAY ' * TOTAL DE ALUNOS COM MEDIA < 6:' WS-QTD-MA6-E
           DISPLAY ' * % DE ALUNOS COM MEDIA < 6....: ' WS-PERCENT-M6
               '%'
           DISPLAY ' *========================================*'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO CGPRG005        *'
           DISPLAY ' *----------------------------------------*'
           .
      *---------------> FIM DO PROGRAMA CGPRG005 <-------------------*
