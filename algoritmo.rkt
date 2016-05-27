;;; Linguagens de Programação
;;; Prof. Rodrigo Hübner
;;; Trabalho Linguagem Funcional
;;; Alunos: Bruno Mendes, Mairieli Wessel
;;; Data: 01/06/2016
#lang racket

; Função que realizar o algoritmo.
(define (busca tabuleiro tamanho)
    ;Como usar as Funções:
    ;(posicaoVazia tabuleiro tamanho)
    ;(imprime tabuleiro)
    (pecasErradas tabuleiro tamanho)
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

; Main
(busca '((0 1 2) (3 4 5) (6 7 8)) 3)
;(busca '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15)) 4)
;(busca '((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19) (20 21 22 23 24)) 5)