#🐸 PLP 2024.2- O Sapo Não Lava o Pé
Bem-vindo ao PLP - O Sapo Não Lava o Pé! 🎮
Este projeto é uma recriação simplificada do clássico jogo Frogger do Atari 2600, adaptado para explorar conceitos de programação funcional utilizando Haskell. Com a ajuda da biblioteca Gloss, o jogo ganha vida com gráficos interativos e dinâmicos! 🌟

#📜 Sobre o Projeto
PLP - O Sapo Não Lava o Pé é uma recriação simplificada e moderna do clássico jogo Frogger do Atari 2600, trazendo o desafio de atravessar um rio repleto de perigos em um cenário dinâmico e interativo.
Desenvolvido exclusivamente em Haskell, o projeto utiliza a biblioteca Gloss para criar uma interface gráfica, demonstrando como conceitos de programação funcional podem ser aplicados na prática.

#🎮 Funcionalidades
##Movimentação do jogador
Controle o sapo para a esquerda, direita e para cima.

##Mundo dinâmico
Troncos e vitórias-régias continuam se movendo constantemente, mesmo sem a ação do jogador.

##Cenário interativo
Um rio perigoso onde a água é letal e plataformas móveis ajudam na travessia.

##Colisão realista
Se o sapo cair na água, o jogo termina imediatamente.

##Velocidade ajustada dos objetos
Troncos e vitórias-régias possuem velocidades diferentes, aumentando o desafio.

#🌟 Elementos do Jogo

##Fase única
O cenário é representado como uma matriz (grid), contendo água, troncos, vitórias-régias e o sapo.

##Sapo
Personagem controlado pelo jogador.

##Vitória-régia
Plataforma móvel que carrega o sapo enquanto ele estiver sobre ela.

##Água
Obstáculo mortal. Cair na água retorna para o inicial.

##Tronco
Outra plataforma móvel que ajuda o sapo a atravessar o rio.

#📚 Tecnologias Utilizadas
###Haskell:
Linguagem funcional utilizada para a lógica e estrutura do jogo.

###Gloss:
Biblioteca gráfica usada para criar os elementos visuais e tornar o jogo interativo e dinâmico.
