;;; Linguagens de Programação
;;; Prof. Rodrigo Hübner
;;; Trabalho Linguagem Funcional
;;; Alunos: Bruno Mendes, Mairieli Wessel
;;; Data: 01/06/2016
#lang racket

; Função que realizar o algoritmo.
; tabuleiro => lista de listas, representando o tabuleiro.
; tamanho => valor N, tamanho do tabuleiro NxN.
(define (busca tabuleiro tamanho)

)

; Função para imprimir um tabuleiro.
; tabuleiro => lista de listas, representando o tabuleiro.
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

; Função que retorna o índice da posição vazia.
; tabuleiro => lista de listas, representando o tabuleiro.
; i => Indíce auxiliar, valor default = 0.
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

; Main
(busca '((1 2 3) (4 5 6) (7 0 9)) 3)
(busca '((0 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)) 4)
(busca '((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 0 18 19 20) (21 22 23 24 25)) 5)