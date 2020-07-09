      *Divisão de identificação do programa
       identification division.
       program-id. "desafio".
       author. "Daiana Weiss".
       installation. "PC".
       date-written. 08/07/2020.
       date-compiled. 08/07/2020.



      *Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *Declaração de variáveis
       data division.

      *----Variaveis de arquivos
       file section.


      *----Variaveis de trabalho
       working-storage section.
      * ------------ variaveis da tabela de pizza ---------------------
       01  relatorio  occurs  20.
           05 nome                                 pic x(15).
           05 filler                               pic X(01) value "-".
           05 diametro                             pic 9(03).
           05 filler                               pic X(01) value "-".
           05 preco                                pic 9(03)v99.
           05 filler                               pic X(01) value "-".
           05 area_pizza                           pic 9(10)V99.
           05 filler                               pic X(01) value "-".
           05 preco_cm2                            pic 9(03)v99.
           05 filler                               pic X(01) value "-".
           05 porcent                              pic 9(10)V99.
      * ------------------- variaveis adicionais -----------------------
       77  ind                                     pic 9(02).
       77  menu                                    pic x(01).
       77  pi                                      pic 9(01)V99
                                                   value 3,14.
       77  controle                                pic X(10).
       77  aux                                     pic 9(10).
       77  aux_nome                                pic X(10).
       77  qtd_pizza                               pic 9(02).
       77  diferenca                               pic 9(05)V99.

      *----Variaveis para comunicação entre programas
       linkage section.


      *----Declaração de tela
       screen section.


      *Declaração do corpo do programa
       procedure division.
      *----------------------- estruturacao ----------------------------
           display "---- Custo Beneficio de Pizza"
           perform inicializa.
           perform processamento.
           perform finaliza.
      *-----------------------------------------------------------------
      * Inicilizacao de variaveis, abertura de arquivos
      * procedimentos que serao realizados apenas uma vez
       inicializa section.
           move   "S"       to     menu
           move    1        to     ind
           move    0        to     area_pizza(ind)
           .
       inicializa-exit.
           exit.
      *-----------------------------------------------------------------
      * Corpo do site
       processamento section.
           move 0 to ind
           move 0 to qtd_pizza
           perform until menu <> "S"
               display erase
               add 1 to ind
      *        usuario nao pode cadastrar mais que 20 pizzas
               if ind > 20 then
                   display "Voce Atingiu o Limite de 20 Pizzas"
               else
                   add 1 to qtd_pizza
                   display "Informe o Nome da Pizza "
                   accept nome(ind)

                   display "Informe o Diametro "
                   accept diametro(ind)

                   display "Informe o Preco "
                   accept preco(ind)
               end-if
      *        inserindo os calculos
               perform calculo-area
               perform calculo-preco-cm2
               display "Deseja Cadastrar Mais Uma Pizza? ('S'/'N')"
               accept menu
           end-perform
      *    ordenando e calculando porcentagem com base na ordenacao
           perform ordenar
           perform calculo-porcent
      *    mostrando a tabela final
           perform varying ind from 1 by 1 until ind > 20
                                              or nome(ind) = space
               display relatorio(ind)
           end-perform
           .
       processamento-exit.
           exit.
      *----------------------------------------------------------------
      * Calculo da area da pizza
       calculo-area section.
           compute area_pizza(ind) = pi * ((diametro(ind) / 2)
           * (diametro(ind) / 2))
           .
       calculo-area-exit.
           exit.
      *-----------------------------------------------------------------
      * Calculo do preco por centimetro quadrado
       calculo-preco-cm2 section.
           compute preco_cm2(ind) = preco(ind) / area_pizza(ind)
           .
       calculo-preco-cm2-exit.
           exit.
      *-----------------------------------------------------------------
      * Ordenacao da tabela (do melhor para o pior custo beneficio)
       ordenar section.
           move "trocou"  to  controle
           perform until controle <> "trocou"
               move     1        to    ind
               move  "N_trocou"  to controle
               perform until ind = qtd_pizza
                   if preco_cm2(ind) > preco_cm2(ind + 1)
      *                movendo preco_cm2 para o lugar certo
                       move  preco_cm2(ind + 1)to   aux
                       move   preco_cm2(ind)   to   preco_cm2(ind + 1)
                       move      aux           to   preco_cm2(ind)
      *                movendo nome para o lugar certo
                       move   nome(ind + 1)    to   aux_nome
                       move   nome(ind)        to   nome(ind + 1)
                       move    aux_nome        to   nome(ind)
      *                movendo diametro para o lugar certo
                       move  diametro(ind + 1) to   aux
                       move   diametro(ind)    to   diametro(ind + 1)
                       move      aux           to   diametro(ind)
      *                movendo preco para o lugar certo
                       move  preco(ind + 1)    to   aux
                       move   preco(ind)       to   diametro(ind + 1)
                       move      aux           to   diametro(ind)
      *                movendo area_pizza para o lugar certo
                       move area_pizza(ind + 1)to   aux
                       move  area_pizza(ind)   to   area_pizza(ind + 1)
                       move      aux           to   area_pizza(ind)
      *                garantindo que fara novamente
                       move    "trocou"        to   controle
                   end-if
                   add 1 to ind
               end-perform
           end-perform
           .
       ordenar-exit.
           exit.
      *-----------------------------------------------------------------
      * Calculo de quantos % o preco eh melhor
       calculo-porcent section.
      *    inicializando as variaveis usadas aqui
           move     1        to    ind
           move     0        to    porcent(ind)
           move     0        to    diferenca
           perform until ind > qtd_pizza - 1
               move     0        to    porcent(ind)
               move     0        to    diferenca
      *        calculo de porcentagem
               compute diferenca = preco_cm2(ind + 1)- preco_cm2(ind)
               compute porcent(ind + 1
               ) = (diferenca * 100) / preco_cm2(ind)
               add 1 to ind
           end-perform
           .
       calculo-porcent-exit.
           exit.
      *-----------------------------------------------------------------
      * Finalizacao do site
       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.


