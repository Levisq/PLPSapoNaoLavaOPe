:- use_module(library(lists)).
:- use_module(library(system)).

% 1. Grid Inicial
initial_grid([
    [terra, terra, terra, terra, terra, terra, terra],   % Linha 0 (DESTINO)
    [water, water, regia, water, water, water, water],    % Linha 1
    [water, water, water, tronco, tronco, water, water],  % Linha 2
    [water, water, water, regia, water, water, water],    % Linha 3
    [water, water, water, tronco, tronco, water, water], % Linha 4
    [regia, water, water, water, water, water, water],    % Linha 5
    [terra, terra, terra, sapo, terra, terra, terra]     % Linha 6 (BASE)
]).

% Direção das linhas móveis
direcao_linha(Y, direita) :- Y mod 2 =:= 1, Y > 0, Y < 6.
direcao_linha(Y, esquerda) :- Y mod 2 =:= 0, Y > 0, Y < 6.

% Movimento de linha
mover_linha(direita, Linha, Nova) :-
    append([Ult], Ini, Linha),
    append(Ini, [Ult], Nova).
mover_linha(esquerda, Linha, Nova) :-
    append(Ini, [Primeiro], Linha),
    append([Primeiro], Ini, Nova).

% Movimenta objetos no grid, exceto as linhas 0 e 6
mover_objetos(Grid, NovoGrid) :-
    findall(L, (
        nth0(Y, Grid, Linha),
        (Y =:= 0 ; Y =:= 6 -> NovaLinha = Linha
        ; direcao_linha(Y, Dir), mover_linha(Dir, Linha, NovaLinha)),
        L = NovaLinha
    ), NovoGrid).

% Encontra a posição do sapo (incluindo células combinadas)
find_sapo(Grid, X-Y) :-
    nth0(Y, Grid, Row),
    (nth0(X, Row, sapo) -> true ;
    (nth0(X, Row, sapo_no_tronco) -> true ;
    nth0(X, Row, sapo_na_regia))).

% Células seguras (incluindo combinações)
pode_mover(terra).
pode_mover(tronco).
pode_mover(regia).
pode_mover(sapo_no_tronco).
pode_mover(sapo_na_regia).

% Movimentos válidos
move(w, X-Y, X-NY) :- NY is Y - 1.
move(s, X-Y, X-NY) :- NY is Y + 1.
move(a, X-Y, NX-Y) :- NX is X - 1.
move(d, X-Y, NX-Y) :- NX is X + 1.

% Substitui valor em lista
replace_in_list(0, New, [_|T], [New|T]).
replace_in_list(I, New, [H|T], [H|R]) :-
    I > 0, NI is I - 1, replace_in_list(NI, New, T, R).

% Atualiza célula no grid
update_cell(Grid, X-Y, Val, NewGrid) :-
    nth0(Y, Grid, Linha),
    replace_in_list(X, Val, Linha, NL),
    replace_in_list(Y, NL, Grid, NewGrid).

% Converte célula com sapo para o tipo combinado
cria_celula_com_sapo(tronco, sapo_no_tronco).
cria_celula_com_sapo(regia, sapo_na_regia).
cria_celula_com_sapo(terra, sapo).

% Restaura célula quando sapo sai
restaura_celula(sapo_no_tronco, tronco).
restaura_celula(sapo_na_regia, regia).
restaura_celula(sapo, terra).

% Atualização principal - Versão "andar junto"
update(Grid, Move, FinalGrid) :-
    ( find_sapo(Grid, OldX-OldY),
      move(Move, OldX-OldY, NX-NY),
      NX >= 0, NX < 7, NY >= 0, NY < 7,
      nth0(NY, Grid, Row), nth0(NX, Row, Cell),

      % Remove sapo da posição antiga
      nth0(OldY, Grid, OldRow),
      nth0(OldX, OldRow, OldCell),
      (restaura_celula(OldCell, RestoredCell) -> true ; RestoredCell = water),
      update_cell(Grid, OldX-OldY, RestoredCell, G1),

      % Move sapo para nova posição
      ( Cell = water ->
          format('Caiu na agua! Reiniciando o jogo...\n'),
          initial_grid(FinalGrid)
      ; pode_mover(Cell) ->
          ( cria_celula_com_sapo(Cell, NewCell) ->
              update_cell(G1, NX-NY, NewCell, G2)
          ; update_cell(G1, NX-NY, sapo, G2)
          ),
          mover_objetos(G2, FinalGrid)
      ;
          format('Caiu na agua! Reiniciando o jogo...\n'),
          initial_grid(FinalGrid)
      )
    ;
      mover_objetos(Grid, FinalGrid)
    ).

% Impressão do grid
print_grid(Grid) :-
    nl, maplist(print_row, Grid), nl.

print_row(Row) :-
    write('['), maplist(print_cell, Row), write(']'), nl.

% Representação textual para cada célula
print_cell(terra) :- write(' terra ').
print_cell(water) :- write(' water ').
print_cell(tronco) :- write(' tronco ').
print_cell(regia) :- write(' regia ').
print_cell(sapo) :- write(' sapo ').
print_cell(sapo_no_tronco) :- write(' sapo/t ').
print_cell(sapo_na_regia) :- write(' sapo/r ').
print_cell(X) :- write(' '), write(X), write(' ').  % Fallback

% Loop principal
game_loop(Grid) :-
    print_grid(Grid),
    ( find_sapo(Grid, X-0) ->  % Verifica se chegou ao topo
        print_grid(Grid),
        format('PARABENS! O sapo chegou ao destino!\n'),
        halt
    ;
        format('Use WASD para mover (Q para sair): '),
        get_single_char(Code),
        char_code(Char, Code),
        downcase_atom(Char, Move),
        ( Move == 'q' ->
            format('Saindo do jogo...\n'), halt
        ; member(Move, [w,a,s,d]) ->
            update(Grid, Move, NewGrid),
            sleep(0.3),
            game_loop(NewGrid)
        ;
            format('Movimento invalido! Use WASD.\n'),
            game_loop(Grid)
        )
    ).

% Início do jogo
play :-
    initial_grid(Grid),
    format('Bem-vindo ao Jogo do Sapo!~n'),
    format('Objetivo: Leve o sapo ate o topo (linha 0).~n'),
    format('Controles:~n'),
    format('  W - Cima~n  A - Esquerda~n  S - Baixo~n  D - Direita~n~n'),
    format('Regras:~n'),
    format('1. So pode pular em troncos (tronco) e regias (regia)~n'),
    format('2. Cair na agua (water) reinicia o jogo~n'),
    format('3. Troncos e regias se movem automaticamente~n~n'),
    game_loop(Grid).

:- initialization(play).
