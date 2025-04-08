:- use_module(library(lists)).
:- use_module(library(system)).

% 1. Define a grade inicial do jogo com posições fixas do cenário.
% A linha 0 representa o objetivo (terra), e a linha 6 a base inicial com o sapo.
initial_grid([
    [terra, terra, terra, terra, terra, terra, terra],   % Linha 0 (Destino)
    [water, tronco, tronco, water, water, water, water],   % Linha 1
    [water, water, water, tronco, tronco, water, water],  % Linha 2
    [water, water, tronco, tronco, water, water, water],   % Linha 3
    [water, water, water, tronco, tronco, water, water],  % Linha 4
    [tronco, tronco, water, water, water, water, water],   % Linha 5
    [terra, terra, terra, sapo, terra, terra, terra]      % Linha 6 (Base)
]).

% 2. Define a direção do movimento dos troncos em cada linha.
% Linhas ímpares (1,3,5) movem para a direita; linhas pares (2,4) para a esquerda.
direcao_linha(Y, direita) :- Y mod 2 =:= 1, between(1,5,Y).
direcao_linha(Y, esquerda) :- Y mod 2 =:= 0, between(1,5,Y).

% 3. Move uma linha uma posição para a direita (último elemento vai para o início).
mover_linha(direita, Linha, Nova) :-
    append(Ini, [Ult], Linha),
    append([Ult], Ini, Nova).

% Move uma linha uma posição para a esquerda (primeiro elemento vai para o final).
mover_linha(esquerda, Linha, Nova) :-
    append([Primeiro|Fim], [], Linha),
    append(Fim, [Primeiro], Nova).

% 4. Atualiza as linhas do grid, movendo apenas as linhas móveis (1 a 5).
% As linhas 0 e 6 permanecem inalteradas.
mover_objetos(Grid, NovoGrid) :-
    findall(LinhaAtualizada,
        (nth0(Y, Grid, Linha),
         (   member(Y, [0,6])
         ->  LinhaAtualizada = Linha
         ;   direcao_linha(Y, Dir),
             mover_linha(Dir, Linha, LinhaAtualizada))
        ),
        NovoGrid).

% 5. Encontra a posição atual do sapo no grid.
% Pode estar como sapo ou sapo_no_tronco.
find_sapo(Grid, X-Y) :-
    nth0(Y, Grid, Linha),
    (nth0(X, Linha, sapo) ; nth0(X, Linha, sapo_no_tronco)).

% 6. Define quais células são seguras para o sapo ocupar.
pode_mover(terra).
pode_mover(tronco).
pode_mover(sapo_no_tronco).

% 7. Define os movimentos válidos: w (cima), s (baixo), a (esquerda), d (direita).
move(w, X-Y, X-NY) :- NY is Y - 1.
move(s, X-Y, X-NY) :- NY is Y + 1.
move(a, X-Y, NX-Y) :- NX is X - 1.
move(d, X-Y, NX-Y) :- NX is X + 1.

% 8. Substitui um elemento em uma lista em uma posição específica.
replace_in_list(0, New, [_|T], [New|T]).
replace_in_list(I, New, [H|T], [H|R]) :-
    I > 0,
    NI is I - 1,
    replace_in_list(NI, New, T, R).

% 9. Atualiza uma célula específica do grid.
update_cell(Grid, X-Y, Val, NewGrid) :-
    nth0(Y, Grid, Linha),
    replace_in_list(X, Val, Linha, NovaLinha),
    replace_in_list(Y, NovaLinha, Grid, NewGrid).

% 10. Define a célula resultante quando o sapo ocupa outra célula.
cria_celula_com_sapo(tronco, sapo_no_tronco).
cria_celula_com_sapo(terra, sapo).

% Restaura uma célula após o sapo sair dela.
restaura_celula(sapo_no_tronco, tronco).
restaura_celula(sapo, terra).

% 11. Atualiza o estado do jogo após uma tentativa de movimento.
update(Grid, Move, FinalGrid) :-
    (   find_sapo(Grid, OldX-OldY),
        move(Move, OldX-OldY, NX-NY),
        between(0,6,NX), between(0,6,NY),
        nth0(NY, Grid, LinhaAlvo),
        nth0(NX, LinhaAlvo, Celula),

        % Remove o sapo da posição anterior
        nth0(OldY, Grid, LinhaOrig),
        nth0(OldX, LinhaOrig, CelulaOrig),
        (   restaura_celula(CelulaOrig, Restaurada)
        ->  update_cell(Grid, OldX-OldY, Restaurada, GridTemp)
        ;   GridTemp = Grid),

        % Trata nova posição
        (   Celula = water
        ->  format('Caiu na agua! Reiniciando...~n'),
            initial_grid(FinalGrid)
        ;   pode_mover(Celula)
        ->  (   cria_celula_com_sapo(Celula, NovaCelula)
            ->  update_cell(GridTemp, NX-NY, NovaCelula, GridMovido)
            ;   update_cell(GridTemp, NX-NY, sapo, GridMovido)),
            mover_objetos(GridMovido, FinalGrid)
        ;   format('Movimento invalido!~n'),
            FinalGrid = Grid
        )
    ;   mover_objetos(Grid, FinalGrid)
    ).

% 12. Exibe o grid no terminal de forma formatada.
print_grid(Grid) :-
    nl,
    maplist(print_row, Grid),
    nl.

% Imprime uma linha do grid.
print_row(Row) :-
    write('['),
    maplist(print_cell, Row),
    write(']'), nl.

% Imprime uma célula individual com formatação.
print_cell(terra)           :- write(' terra ').
print_cell(water)           :- write(' water ').
print_cell(tronco)          :- write(' tronco').
print_cell(sapo)            :- write(' sapo  ').
print_cell(sapo_no_tronco)  :- write(' sapo/t').
print_cell(_)               :- write(' ???   ').

% 13. Loop principal do jogo, recebe entrada do usuário e atualiza o jogo.
game_loop(Grid) :-
    print_grid(Grid),
    (   find_sapo(Grid, _-0)
    ->  format('~nO Sapo não lavou o pé!~n~n'), halt
    ;   format('~nMovimento (WASD): '),
        get_single_char(Code),
        char_code(Comando, Code),
        downcase_atom(Comando, Move),
        (   Move == q
        ->  format('~nJogo encerrado.~n'), halt
        ;   member(Move, [w,a,s,d])
        ->  update(Grid, Move, NovoGrid),
            sleep(0.5),
            game_loop(NovoGrid)
        ;   format('~nComando invalido!~n'),
            game_loop(Grid)
        )
    ).

% 14. Inicializa o jogo.
play :-
    initial_grid(Grid),
    format('~n=== JOGO DO SAPO ===~n~n'),
    format('Objetivo: Chegar ao topo (linha 0)~n'),
    format('Controles:~n'),
    format('  W - Cima~n  A - Esquerda~n  S - Baixo~n  D - Direita~n  Q - Sair~n~n'),
    game_loop(Grid).

% Inicia o jogo automaticamente ao carregar o programa.
:- initialization(play).

