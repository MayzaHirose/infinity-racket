#lang racket

;; Não é necessário editar este arquivo.
;; Você deve editar o arquivo infinity.rkt
;;
;; Veja o arquivo infinity.rkt para a descrição do que cada função deve fazer.
;;
;; As funções com nomes que começam com check são responsáveis por fazer os
;; testes. Nestas funções o último parâmetro refere-se ao valor esperado que a
;; função que está sendo testada deve retornar.
;;
;; Você pode escolher quais testes serão executados alterando a definição de
;; test-suite-infinity.

(require rackunit/text-ui rackunit "infinity2.rkt")

(define test-suite-infinity
  (test-suite
   "Infinity"
   test-suite-rotacionar
   test-suite-encaixa-h?
   test-suite-encaixa-v?
   test-suite-seguro?))

(define test-suite-rotacionar
  (test-suite
   "Função: rotacionar"
   (test-case
    "Rotação de todos os blocos"
    (check-equal? (map rotacionar (range 16))
                  '(0 2 4 6 8 10 12 14 1 3 5 7 9 11 13 15)))))

(define test-suite-encaixa-h?
  (test-suite
   "Função: encaixa-h?"
   (test-case
    "Encaixe horizontal 0/0 -> [ ][ ]"
    (check-true (encaixa-h? 0 0)))
   (test-case
    "Encaixe horizontal 0/6 -> [ ][┏]"
    (check-true (encaixa-h? 0 6)))
   (test-case
    "Não-encaixe horizontal 6/0 -> [┏][ ]"
    (check-false (encaixa-h? 6 0)))
   (test-case
    "Encaixe horizontal 6/9 -> [┏][┛]"
    (check-true (encaixa-h? 6 9)))
   (test-case
    "Encaixe horizontal 9/6 -> [┛][┏]"
    (check-true (encaixa-h? 9 6)))
   (test-case
    "Não-encaixe horizontal 6/6 -> [┏][┏]"
    (check-false (encaixa-h? 6 6)))))

(define test-suite-encaixa-v?
  (test-suite
   "Função: encaixa-v?"
   (test-case
    "Encaixe vertical 0/0 -> [ ][ ]"
    (check-true (encaixa-v? 0 0)))
   (test-case
    "Encaixe vertical 0/6 -> [ ][┏]"
    (check-true (encaixa-v? 0 6)))
   (test-case
    "Não-encaixe vertical 6/0 -> [┏][ ]"
    (check-false (encaixa-v? 6 0)))
   (test-case
    "Encaixe vertical 6/9 -> [┏][┛]"
    (check-true (encaixa-v? 6 9)))
   (test-case
    "Encaixe vertical 9/6 -> [┛][┏]"
    (check-true (encaixa-v? 9 6)))
   (test-case
    "Não-encaixe vertical 6/6 -> [┏][┏]"
    (check-false (encaixa-v? 6 6)))))

(define test-suite-seguro?
  (test-suite
   "Função: seguro?"
   (test-case
    "Seguro no canto superior esquerdo"
    (check-true (seguro? 6 '() (tamanho 4 3))))
   (test-case
    "Seguro na borda superior"
    (check-true (seguro? 14 '(6) (tamanho 4 3))))
   (test-case
    "Seguro no canto superior direito"
    (check-true (seguro? 12 '(14 6) (tamanho 4 3))))
   (test-case
    "Seguro na borda direita"
    (check-true (seguro? 5 '(9 7 12 14 6) (tamanho 4 3))))
   (test-case
    "Seguro no canto inferior direito"
    (check-true (seguro? 9 '(11 3 13 14 7 5 9 7 12 14 6) (tamanho 4 3))))
   (test-case
    "Seguro na borda inferior"
    (check-true (seguro? 11 '(3 13 14 7 5 9 7 12 14 6) (tamanho 4 3))))
   (test-case
    "Seguro no canto inferior esquerdo"
    (check-true (seguro? 3 '(13 14 7 5 9 7 12 14 6) (tamanho 4 3))))
   (test-case
    "Seguro no interior"
    (check-true (seguro? 14 '(7 5 9 7 12 14 6) (tamanho 4 3))))
   (test-case
    "Inseguro no canto superior esquerdo"
    (check-false (seguro? 9 '() (tamanho 4 3))))
   (test-case
    "Inseguro na borda superior"
    (check-false (seguro? 11 '(6) (tamanho 4 3))))
   (test-case
    "Inseguro no canto superior direito"
    (check-false (seguro? 15 '(14 6) (tamanho 4 3))))
   (test-case
    "Inseguro na borda direita"
    (check-false (seguro? 7 '(9 7 12 14 6) (tamanho 4 3))))
   (test-case
    "Inseguro no canto inferior direito"
    (check-false (seguro? 15 '(11 3 13 14 7 5 9 7 12 14 6) (tamanho 4 3))))
   (test-case
    "Inseguro na borda inferior"
    (check-false (seguro? 13 '(3 13 14 7 5 9 7 12 14 6) (tamanho 4 3))))
   (test-case
    "Inseguro no canto inferior esquerdo"
    (check-false (seguro? 13 '(13 14 7 5 9 7 12 14 6) (tamanho 4 3))))
   (test-case
    "Inseguro no interior"
    (check-false (seguro? 0 '(7 5 9 7 12 14 6) (tamanho 4 3))))
   (test-case
    "Seguro no canto superior/inferior esquerdo"
    (check-true (seguro? 2 '() (tamanho 1 3))))
   (test-case
    "Seguro na borda superior/inferior"
    (check-true (seguro? 10 '(2) (tamanho 1 3))))
   (test-case
    "Seguro no canto superior/inferior direito"
    (check-true (seguro? 8 '(10 2) (tamanho 1 3))))))

(define (executar-testes)
  (run-tests test-suite-infinity)
  (void))

(executar-testes)