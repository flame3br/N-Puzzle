;;; Linguagens de Programação
;;; Prof. Rodrigo Hübner
;;; Trabalho Linguagem Funcional
;;; Alunos: Bruno Mendes, Mairieli Wessel
;;; Data: 01/06/2016
#lang racket

;_________________________________________________________________

; Função que lê o valor do usuário.
(define (lePrompt prompt)
    (display prompt)
    (string->number (read-line))
)
;_________________________________________________________________

; Função incial do jogo.
(define (jogar computador?)
    (let ((tamanho (lePrompt "Tamanho do Tabuleiro: ")))
        (if (= computador? 1)
            (jogo (geraTabuleiro tamanho) tamanho)
            (jogarSomenteJogador (geraTabuleiro tamanho) tamanho)
        )
    )
)
;_________________________________________________________________

; Função que realiza o jogo.
(define (jogo tabuleiro tamanho [jogador? true])
    (if (estadoFinal tabuleiro tamanho)
        (display "***Jogo Finalizado!***")
        (and (imprime tabuleiro)
            (if jogador?
                (jogo (jogada tabuleiro tamanho (lePrompt "~Peça a ser movida: ")) tamanho false)
                (and (display "-Movimento do Computador:\n") ((jogo (escolheSucessor (sucessores tabuleiro tamanho) tamanho) tamanho true)))
            )
        )
    )
)

(define (jogada tabuleiro tamanho valor)
    (movimenta tabuleiro tamanho (indiceValor tabuleiro tamanho valor) (posicaoVazia tabuleiro tamanho))
)

(define (estadoFinal tabuleiro tamanho)
    (if (= (pecasErradas tabuleiro tamanho) 0)
        true
        false
    )
)
;_________________________________________________________________

; Função para somente o jogador jogar.
(define (jogarSomenteJogador tabuleiro tamanho)
    (if (estadoFinal tabuleiro tamanho)
        (display "***Jogo Finalizado!***")
        (and (imprime tabuleiro)
            (jogarSomenteJogador (jogada tabuleiro tamanho (lePrompt "~Peça a ser movida: ")) tamanho)
        )
    )
)
;_________________________________________________________________

; Função para imprimir um tabuleiro.
(define (imprime tabuleiro)
    (if (null? tabuleiro)
        true
        (and
            (and (imprimeLinha (car tabuleiro)) (display "\n"))
            (imprime (cdr tabuleiro))
        )
    )
)

(define (imprimeLinha linha)
    (if (null? linha)
        true
        (and
            (and (display (car linha)) (display " "))
            (imprimeLinha (cdr linha))
        )
    )
)
;_________________________________________________________________

; Função para imprimir lista de tabuleiros
(define (imprimeTabuleiros tabuleiros)
    (if (null? tabuleiros)
        null
        (and (and (imprime (car tabuleiros)) (display "\n")) (imprimeTabuleiros (cdr tabuleiros)))
    )
)
;_________________________________________________________________

; Função que retorna o índice da posição vazia.
(define (posicaoVazia tabuleiro tamanho [i 0])
    (if (>= (contemPosicaoVazia (car tabuleiro) i) 0)
        (contemPosicaoVazia (car tabuleiro) i)
        (posicaoVazia (cdr tabuleiro) tamanho (+ i tamanho))
    )
)

(define (contemPosicaoVazia linha i)
    (if (null? linha)
        -1
        (if (zero? (car linha))
            i
            (contemPosicaoVazia (cdr linha) (+ i 1))
        )
    )
)
;_________________________________________________________________

; Função que retorna a quantidade de peças erradas (heurística).
(define (pecasErradas tabuleiro tamanho [i 0] [erradas 0])
    (if (null? tabuleiro)
        erradas
        (pecasErradas (cdr tabuleiro) tamanho (+ i tamanho) (+ (pecasErradasLinha (car tabuleiro) i) erradas) )
    )
)

(define (pecasErradasLinha linha i [erradas 0])
    (if (null? linha)
        erradas
        (if (not (= i (car linha)))
            (pecasErradasLinha (cdr linha) (+ i 1) (+ erradas 1))
            (pecasErradasLinha (cdr linha) (+ i 1) erradas)
        )
    )
)

;_________________________________________________________________

; Função que retorna um tabuleiro com um movimento realizado.
(define (movimenta tabuleiro tamanho indice vazia)
    (setValorIndice (setValorIndice tabuleiro tamanho vazia (valorIndice tabuleiro tamanho indice)) tamanho indice 0)
)

;_________________________________________________________________

; Função que retorna um tabuleiro com um valor setado em uma possição.
(define (setValorIndice tabuleiro tamanho indice valor [i 0])
    (if (null? tabuleiro)
        null
        (append
            (list (setValorIndiceLinha (car tabuleiro) indice valor i))
            (setValorIndice (cdr tabuleiro) tamanho indice valor (+ i tamanho))
        )
    )
)

(define (setValorIndiceLinha linha indice valor i [novaLinha '()])
    (if (null? linha)
        novaLinha
        (if (= indice i)
            (append novaLinha (cons valor (setValorIndiceLinha (cdr linha) indice valor (+ i 1) novaLinha)))
            (append novaLinha (cons (car linha) (setValorIndiceLinha (cdr linha) indice valor (+ i 1) novaLinha)))
        )
    )
)
;_________________________________________________________________

; Função que retorna o valor de um índice.
(define (valorIndice tabuleiro tamanho indice [i 0])
    (if (>= (valorIndiceLinha (car tabuleiro) indice i) 0)
        (valorIndiceLinha (car tabuleiro) indice i)
        (valorIndice (cdr tabuleiro) tamanho indice (+ i tamanho))
    )
)

(define (valorIndiceLinha linha indice i)
    (if (null? linha)
        -1
        (if (= indice i)
            (car linha)
            (valorIndiceLinha (cdr linha) indice (+ i 1))
        )
    )
)
;_________________________________________________________________

; Função que escolhe um sucessor de acordo com a heurística
(define (escolheSucessor tabuleiros tamanho)
    (sucessorIndice
        tabuleiros
        (indiceMenorValor
            (calculaSucessores tabuleiros tamanho)
            (car (calculaSucessores tabuleiros tamanho))
        )
    )
)

(define (calculaSucessores tabuleiros tamanho)
    (if (null? tabuleiros)
        null
        (append (list (pecasErradas (car tabuleiros) tamanho)) (calculaSucessores (cdr tabuleiros) tamanho))
    )
)

(define (indiceMenorValor valores menorValor [indiceMenor 0] [i 0])
    (if (null? valores)
        indiceMenor
        (if (< (car valores) menorValor)
            (indiceMenorValor (cdr valores) (car valores) i (+ i 1))
            (indiceMenorValor (cdr valores) menorValor indiceMenor (+ i 1))
        )
    )
)

(define (sucessorIndice tabuleiros indice [i 0])
    (if (= indice i)
        (car tabuleiros)
        (sucessorIndice (cdr tabuleiros) indice (+ i 1))
    )
)
;_________________________________________________________________

; Função que retorna os possíveis sucessores
(define (sucessores tabuleiro tamanho)
    (append
        (sucessorDireita tabuleiro tamanho (- (posicaoVazia tabuleiro tamanho) 1) (posicaoVazia tabuleiro tamanho))
        (sucessorEsquerda tabuleiro tamanho (+ (posicaoVazia tabuleiro tamanho) 1) (posicaoVazia tabuleiro tamanho))
        (sucessorY tabuleiro tamanho (- (posicaoVazia tabuleiro tamanho) tamanho) (posicaoVazia tabuleiro tamanho))
        (sucessorY tabuleiro tamanho (+ (posicaoVazia tabuleiro tamanho) tamanho) (posicaoVazia tabuleiro tamanho))
    )
)

(define (sucessorY tabuleiro tamanho indice indiceVazio)
    (if (or (>= indice (* tamanho tamanho)) (< indice 0))
        null
        (list (movimenta tabuleiro tamanho indice indiceVazio))
    )
)

(define (sucessorDireita tabuleiro tamanho indice indiceVazio)
    (if (= (remainder (+ indice 1) tamanho) 0)
        null
        (list (movimenta tabuleiro tamanho indice indiceVazio))
    )
)

(define (sucessorEsquerda tabuleiro tamanho indice indiceVazio)
    (if (= (remainder indice tamanho) 0)
        null
        (list (movimenta tabuleiro tamanho indice indiceVazio))
    )
)
;_________________________________________________________________

; Função que cria um tabuleiro aleatório
(define (geraTabuleiro tamanho)
    (geraMatriz (geraVetor tamanho) tamanho)
)

(define (geraMatriz vetor tamanho [tabuleiro '()] [linha '()] [i 0])
    (if (null? vetor)
        (append (list linha) tabuleiro)
        (if (and (= (remainder i tamanho) 0) (> i 0))
            (geraMatriz (cdr vetor) tamanho (append (list linha) tabuleiro) (cons (car vetor) '()) (+ i 1))
            (geraMatriz (cdr vetor) tamanho tabuleiro (cons (car vetor) linha) (+ i 1))
        )
    )
)

(define (geraVetor tamanho [quantidade 0] [vetor '()])
    (if (= quantidade (* tamanho tamanho))
        vetor
        (let ((valor (random (* tamanho tamanho))))
            (if (>= (indiceValorLinha vetor valor 0) 0)
                (geraVetor tamanho quantidade vetor)
                (geraVetor tamanho (+ quantidade 1) (append (list valor) vetor))
            )
        )
    )
)
;_________________________________________________________________

; Função que retorna um índice de um valor
(define (indiceValor tabuleiro tamanho valor [i 0])
    (if (null? tabuleiro)
        -1
        (if (>= (indiceValorLinha (car tabuleiro) valor i) 0)
            (indiceValorLinha (car tabuleiro) valor i)
            (indiceValor (cdr tabuleiro) tamanho valor (+ i tamanho))
        )
    )
)

(define (indiceValorLinha linha valor i)
    (if (null? linha)
        -1
        (if (= valor (car linha))
            i
            (indiceValorLinha (cdr linha) valor (+ i 1))
        )
    )
)
;_________________________________________________________________

; Main
(jogar (lePrompt "Jogar com o PC? 1(Sim) 0(Não): "))