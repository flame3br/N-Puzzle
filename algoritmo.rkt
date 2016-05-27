;;; Linguagens de Programação
;;; Prof. Rodrigo Hübner
;;; Trabalho Linguagem Funcional
;;; Alunos: Bruno Mendes, Mairieli Wessel
;;; Data: 01/06/2016
#lang racket

;_________________________________________________________________

; Função que realiza o algoritmo.
(define (busca tabuleiro tamanho)
    ;(imprime tabuleiro)
    ;(pecasErradas tabuleiro tamanho)
    (movimenta tabuleiro tamanho 4 (posicaoVazia tabuleiro tamanho))
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
    (calculaSucessores tabuleiros tamanho)
)

(define (calculaSucessores tabuleiros tamanho)
    (if (null? tabuleiros)
        null
        (append (list (pecasErradas (car tabuleiros) tamanho)) (calculaSucessores (cdr tabuleiros) tamanho))
    )
)
;_________________________________________________________________

; Main
;(busca '((0 1 2) (3 4 5) (6 7 8)) 3)
;(busca '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15)) 4)
;(busca '((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19) (20 21 22 23 24)) 5)
(escolheSucessor '(((0 1 2) (5 4 3) (6 7 8)) ((2 4 6) (0 1 3) (5 8 7)) ((0 1 2) (3 4 5) (6 7 8))) 3)