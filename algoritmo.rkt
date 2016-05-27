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
    (imprime tabuleiro)
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

; Main
(busca '((1 2 3) (4 5 6) (7 8 9)) 3)
(busca '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)) 4)
(busca '((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25)) 5)