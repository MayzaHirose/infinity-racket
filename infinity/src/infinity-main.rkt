#lang racket

;; Não é necessário editar este arquivo.
;; Você deve editar o arquivo infinity.rkt

;; Este arquivo é executado pelo testador.

(require "infinity.rkt")

(main (vector->list (current-command-line-arguments)))