      $set sourceformat"free"
      *>Divisão de identificação do programa
       identification division.
       program-id. "lista11exercicio3".
       author. "Jade Rogelin".
       installation. "PC".
       date-written. 27/07/2020.
       date-compiled. 27/07/2020.



      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.


      *>----Variaveis de trabalho
       working-storage section.

       01  alunos  occurs  100.
           05  aluno                               pic x(25).
           05  filler                              pic x(03)
                                                   value " - ".
           05  endereco                            pic x(35).
           05  filler                              pic x(03)
                                                   value " - ".
           05  mae                                 pic x(25).
           05  filler                              pic x(03)
                                                   value " - ".
           05  pai                                 pic x(25).
           05  filler                              pic x(03)
                                                   value " - ".
           05  telefone                            pic x(15).
           05  filler                              pic x(03)
                                                   value " - ".
           05  notas occurs 4.
               10  nota                            pic 9(02)v99
                                                   value 11.
               10  filler                          pic x(03)
                                                   value " - ".

       01  wk-alunos.
           05  wk-aluno                            pic x(25).
           05  wk-endereco                         pic x(35).
           05  wk-mae                              pic x(25).
           05  wk-pai                              pic x(25).
           05  wk-tel                              pic x(15).


       01  wk-aluno-rel occurs 15.
           05 cod-rel                              pic 9(03).
           05 filler                               pic x(02)
                                                   value space.
           05 aluno-rel                            pic x(12).
           05 filler                               pic x(02)
                                                   value space.
           05 endereco-rel                         pic x(14).
           05 filler                               pic x(02)
                                                   value space.
           05 mae-rel                              pic x(12).
           05 filler                               pic x(02)
                                                   value space.
           05 pai-rel                              pic x(12).
           05 filler                               pic x(02)
                                                   value space.
           05 tel-rel                              pic x(08).
           05 filler                               pic x(02)
                                                   value space.
           05 media-rel                            pic 9(02)v99 value 0.


       77  nota_aux                                pic 9(02)v99.
       77  soma_nota                               pic 9(02)v99.
       77  qtd_notas                               pic 9(1).


       77  ind                                     pic 9(03).
       77  ind1                                    pic 9(03).
       77  ind2                                    pic 9(03).
       77  ind-nota                                pic 9(03).



       77  menu                                    pic x(02).
       77  aux                                     pic x(01).

       01 wk-tela-menu.
          05  wk-cadastro-aluno                    pic  x(01).
          05  wk-cadastro-nota                     pic  x(01).
          05  wk-consulta-cadastro                 pic  x(01).
          05  wk-sair                              pic  x(01).

       77 wk-msn                                   pic  x(50).


      *>----Variaveis para comunicação entre programas
       linkage section.


      *>----Declaração de tela
       screen section.
       01  tela-menu.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Tela Principal                                   ".
           05 line 03 col 01 value "      MENU                                                                       ".
           05 line 04 col 01 value "        [ ]Cadastro de Alunos                                                    ".
           05 line 05 col 01 value "        [ ]Cadastro de Notas                                                     ".
           05 line 06 col 01 value "        [ ]Consulta Cadastro                                                     ".

           05 sc-sair-menu            line 01  col 71 pic x(01)
           using wk-sair foreground-color 12.

           05 sc-cadastro-aluno       line 04  col 10 pic x(01)
           using wk-cadastro-aluno foreground-color 15.
           05 sc-cadastro-nota        line 05  col 10 pic x(01)
           using wk-cadastro-nota foreground-color 15.
           05 sc-consulta-cadastro    line 06  col 10 pic x(01)
           using wk-consulta-cadastro foreground-color 15.


       01  tela-cad-aluno.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Cadastro de Alunos                               ".
           05 line 03 col 01 value "                                                                                 ".
           05 line 04 col 01 value "      Aluno    :                                                                 ".
           05 line 05 col 01 value "      Endereco :                                                                 ".
           05 line 06 col 01 value "      Mae      :                                                                 ".
           05 line 07 col 01 value "      Pai      :                                                                 ".
           05 line 08 col 01 value "      Telefone :                                                                 ".
           05 line 22 col 01 value "              [__________________________________________________]               ".


           05 sc-sair-cad-alu            line 01  col 71 pic x(01)
           using wk-sair foreground-color 12.

           05 sc-aluno-cad-alu           line 04  col 17 pic x(25)
           using wk-aluno foreground-color 15.

           05 sc-endereco-cad-alu        line 05  col 17 pic x(35)
           using wk-endereco foreground-color 15.

           05 sc-mae-cad-alu             line 06  col 17 pic x(25)
           using wk-mae foreground-color 15.

           05 sc-pai-cad-alu             line 07  col 17 pic x(25)
           using wk-pai foreground-color 15.

           05 sc-tel-cad-alu             line 08  col 17 pic x(15)
           using wk-tel foreground-color 15.


           05 sc-msn-cad-alu             line 22  col 16 pic x(50)
           using wk-msn foreground-color 15.




       01  tela-cad-notas.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Cadastro de Notas                                ".
           05 line 03 col 01 value "       Cod. Aluno:                                                               ".
           05 line 04 col 01 value "       Nota      :                                                               ".
           05 line 22 col 01 value "              [__________________________________________________]               ".




           05 sc-sair-cad-not         line 01  col 71 pic x(01)
           using wk-sair foreground-color 12.

           05 sc-cod-aluno            line 03  col 19 pic 9(03)
           using ind foreground-color 15.

           05 sc-nota                 line 04  col 19 pic 9(02)v99
           using nota_aux foreground-color 15.

           05 sc-msn-cad-not          line 22  col 16 pic x(50)
           using wk-msn foreground-color 15.


       01  tela-consulta-cad.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Consulta Cadastro                                ".
           05 line 03 col 01 value " Cod  Aluno         Endereco        Mae           Pai           Tel       Media  ".

           05 line 22 col 01 value "              [__________________________________________________]               ".

           05 sc-sair-con-cad         line 01  col 71 pic x(01)
           using wk-sair foreground-color 12.
           05 sc-cad-aluno1           line 04  col 02 pic x(79)
           using wk-aluno-rel(1) foreground-color 12.
           05 sc-cad-aluno2           line 05  col 02 pic x(79)
           using wk-aluno-rel(2) foreground-color 12.
           05 sc-cad-aluno3           line 06  col 02 pic x(79)
           using wk-aluno-rel(3) foreground-color 12.
           05 sc-cad-aluno4           line 07  col 02 pic x(79)
           using wk-aluno-rel(4) foreground-color 12.
           05 sc-cad-aluno5           line 08  col 02 pic x(79)
           using wk-aluno-rel(5) foreground-color 12.
           05 sc-cad-aluno6           line 09  col 02 pic x(79)
           using wk-aluno-rel(6) foreground-color 12.
           05 sc-cad-aluno7           line 10  col 02 pic x(79)
           using wk-aluno-rel(7) foreground-color 12.
           05 sc-cad-aluno8           line 11  col 02 pic x(79)
           using wk-aluno-rel(8) foreground-color 12.
           05 sc-cad-aluno9           line 12  col 02 pic x(79)
           using wk-aluno-rel(9) foreground-color 12.
           05 sc-cad-aluno10          line 13  col 02 pic x(79)
           using wk-aluno-rel(10) foreground-color 12.
           05 sc-cad-aluno11          line 14  col 02 pic x(79)
           using wk-aluno-rel(11) foreground-color 12.
           05 sc-cad-aluno12          line 15  col 02 pic x(79)
           using wk-aluno-rel(12) foreground-color 12.
           05 sc-cad-aluno13          line 16  col 02 pic x(79)
           using wk-aluno-rel(13) foreground-color 12.
           05 sc-cad-aluno14          line 17  col 02 pic x(79)
           using wk-aluno-rel(14) foreground-color 12.
           05 sc-cad-aluno15          line 18  col 02 pic x(79)
           using wk-aluno-rel(15) foreground-color 12.



      *>Declaração do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  procedimentos de inicialização
      *>------------------------------------------------------------------------
       inicializa section.
      *>    inicializa menu
           move  spaces      to     menu
           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento Principal
      *>------------------------------------------------------------------------
       processamento section.

           perform until wk-sair = "X"
                      or wk-sair = "x"

                move   space  to  wk-cadastro-aluno
                move   space  to  wk-cadastro-nota
                move   space  to  wk-consulta-cadastro
                move   space  to  wk-sair

                display tela-menu
                accept tela-menu

                if wk-cadastro-aluno = "X"
                or wk-cadastro-aluno = "x" then
                       perform cadastrar-aluno
                end-if

                if wk-cadastro-nota = "X"
                or wk-cadastro-nota = "x" then
                       perform cadastrar-notas
                end-if

                if wk-consulta-cadastro = "X"
                or wk-consulta-cadastro = "x" then
                       perform consultar-cadastro
                end-if
           end-perform


           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  cadastro de aluno
      *>------------------------------------------------------------------------
       cadastrar-aluno section.

            perform until wk-sair = "V"
                       or wk-sair = "v"
               move spaces          to  wk-aluno
               move spaces          to  wk-endereco
               move spaces          to  wk-mae
               move spaces          to  wk-pai
               move spaces          to  wk-tel

               display tela-cad-aluno
               accept tela-cad-aluno

               move spaces          to  wk-msn

               perform buscar-prox-ind

               if ind <= 100 then
                   move wk-aluno         to  aluno(ind)
                   move wk-endereco      to  endereco(ind)
                   move wk-mae           to  mae(ind)
                   move wk-pai           to  pai(ind)
                   move wk-tel           to  telefone(ind)
               else
                   move "Quantidade limite de 100 alunos cadastrados"  to  wk-msn
               end-if
            end-perform

           .
       cadastrar-aluno-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  cadastro de notas
      *>------------------------------------------------------------------------
       cadastrar-notas section.


           perform until wk-sair = "V"
                      or wk-sair = "v"

               move zero   to  nota_aux
               move zero   to  ind

               display tela-cad-notas
               accept tela-cad-notas

               move space   to    wk-msn

               if  ind > 0
               and ind <= 100 then
                   if aluno(ind) <> space then

                       if  nota_aux >= 0
                       and nota_aux <= 10 then
                           perform buscar-prox-ind-nota
                           move nota_aux  to nota(ind ind1)
                       else
                           move  "Nota Invalida!"     to wk-msn
                       end-if
                   else
                       move  "Aluno nao cadastrado!"  to wk-msn
                   end-if
               else
                   move "Cod fora do intervalo valido (1 - 100)" to wk-msn
               end-if
           end-perform

           .
       cadastrar-notas-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  consultar cadastro
      *>------------------------------------------------------------------------
       consultar-cadastro section.

           move zero to ind2
           perform until wk-sair = "V"
                      or wk-sair = "v"

               perform varying ind from 1 by 1 until ind > 15
                                               or (ind2 + ind) > 100
                                               or aluno(ind2 + ind) = space


                   compute  cod-rel(ind) =  ind + ind2

                   move  aluno(ind + ind2)         to  aluno-rel(ind)
                   move  endereco(ind + ind2)      to  endereco-rel(ind)
                   move  mae(ind + ind2)           to  mae-rel(ind)
                   move  pai(ind + ind2)           to  pai-rel(ind)
                   move  telefone(ind + ind2)      to  tel-rel(ind)


      *>          metodo 1 para calcular a media
                   move 0 to qtd_notas
                   move zero to soma_nota
                   if   nota((ind + ind2) 4) >= 0
                   and  nota((ind + ind2) 4) <= 10 then
                       compute soma_nota = soma_nota + nota((ind + ind2) 4)
                       add 1 to qtd_notas
                   end-if

                   if   nota((ind + ind2) 3) >= 0
                   and  nota((ind + ind2) 3) <= 10 then
                       compute soma_nota = soma_nota + nota((ind + ind2) 3)
                       add 1 to qtd_notas
                   end-if

                   if   nota((ind + ind2) 2) >= 0
                   and  nota((ind + ind2) 2) <= 10 then
                       compute soma_nota = soma_nota + nota((ind + ind2) 2)
                       add 1 to qtd_notas
                   end-if

                   if   nota((ind + ind2) 1) >= 0
                   and  nota((ind + ind2) 1) <= 10 then
                       compute soma_nota = soma_nota + nota((ind + ind2) 1)
                       add 1 to qtd_notas
                   end-if


                   if qtd_notas <> 0  then    *> previnindo divisão por zero...
                       compute media-rel(ind) =  soma_nota / qtd_notas
                   else
                       move zero      to  media-rel(ind)
                   end-if


      *> ----          calculo da media do aluno método 2
      *>            move zero to soma_nota
      *>            perform varying ind-nota from 1 by 1 until ind-nota > 4
      *>                                 or nota((ind + ind2) ind-nota) = 11
      *>
      *>                compute soma_nota = soma_nota + nota((ind + ind2) ind-nota)
      *>
      *>            end-perform
      *>
      *>            compute media-rel(ind) = soma_nota / (ind-nota - 1)
      *>
               end-perform

               add  15      to     ind2
               display tela-consulta-cad
               accept tela-consulta-cad
           end-perform


           .
       consultar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  buscar proximo indice do aluno
      *>------------------------------------------------------------------------
       buscar-prox-ind section.
           perform varying ind from 1 by 1 until ind > 100
                                              or aluno(ind)=space
               continue
           end-perform
           .
       buscar-prox-ind-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  buscar proximo indice da nota
      *>------------------------------------------------------------------------
       buscar-prox-ind-nota section.
           perform varying ind1 from 1 by 1 until ind1 > 4
                                              or nota(ind ind1)=11
               continue
           end-perform
           .
       buscar-prox-ind-nota-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.

