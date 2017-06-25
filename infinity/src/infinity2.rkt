#lang racket

;; Este programa resolve um jogo infinity rotacionando seus blocos
;; até que todos se encaixem entre si. O método de resolução utilizado será
;; por meio de busca com retrocesso (backtracking).
;;
;; ** Conceitos **
;; Bloco
;;  Um bloco é uma peça do jogo Infinity. O bloco é representado como um
;;  caractere quando lido ou escrito de um arquivo, e representado como um
;;  número entre 0 e 15 quando processado dentro do programa.
;; Jogo
;;  Um jogo Infinity é um tabuleiro de M linhas e N colunas (tamanho MxN)
;;  preenchidas por blocos. O jogo infinity é dito resolvido quando todos seus
;;  blocos estão conectados. O jogo é representado por uma lista de listas.

;; exporta as funções que podem ser utilizadas em outros arquivos
(provide tamanho
         rotacionar
         encaixa-h?
         encaixa-v?
         seguro?
         resolver
         main)

;; lista com as representações dos blocos como caracteres
(define blocos-reps (string->list " ╹╺┗╻┃┏┣╸┛━┻┓┫┳╋"))

;; *****************************INICIO VERIFICA-TAMANHO**********************************
;; Tamanho representa o tamanho de um jogo em altura e largura
;;     altura : Número - quantidade de linhas do jogo.
;;    largura : Número - quantidade de colunas do jogo.
(struct tamanho (altura largura) #:transparent)

;;Jogo -> Struct tamanho
(define (verifica-tamanho jogo)
  (cond
    [(empty? jogo) 0]
    [else
     (tamanho (length jogo)(length (first jogo)))]))
;; *****************************FIM VERIFICA-TAMANHO**********************************

;; *****************************INICIO ROTACIONAR*************************************
;; Bloco -> Bloco
;; --------------
;; Rotaciona um bloco 90 graus em sentido horário
;; Exemplo: (rotacionar 5)
;;          > 10
(define (rotacionar bloco)
  (cond
    [(<= bloco 7)
     (* bloco 2)]
    [else
     (cond
       [(= bloco 8) 1]
       [(= bloco 9) 3]
       [(= bloco 10) 5]
       [(= bloco 11) 7]
       [(= bloco 12) 9]
       [(= bloco 13) 11]
       [(= bloco 14) 13]
       [(= bloco 15) 15])]))

(define (cria-lista-possibilidades-blocos jogo)
  (cond
    [(empty? jogo) empty]
    [(list? (first jogo))
     (cons (cria-lista-possibilidades-blocos (first jogo))
           (cria-lista-possibilidades-blocos (rest jogo)))]
    [else
     (cons (cria-possibilidades-bloco (first jogo))
           (cria-lista-possibilidades-blocos (rest jogo)))]))

(define (cria-possibilidades-bloco bloco)
  (define r1 (rotacionar bloco))
  (define r2 (rotacionar r1))
  (define r3 (rotacionar r2))
  (list bloco r1 r2 r3))

;; *****************************FIM ROTACIONAR*************************************

;; *****************************INICIO ENCAIXA-H?*************************************
;; Bloco Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco-e se encaixa horizontalmente à esquerda do bloco-d
;; Exemplo: (encaixa-h? 7 9)
;;          > #t
(define (encaixa-h? bloco-e bloco-d) #f)

;; *****************************FIM ENCAIXA-H?*************************************

;; *****************************INICIO ENCAIXA-V?*************************************
;; Bloco Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco-t se encaixa verticalmente acima do bloco-b
;; Exemplo: (encaixa-v? 14 11)
;;          > #t
(define (encaixa-v? bloco-t bloco-b) #f)

;; *****************************FIM ENCAIXA-V?*************************************

;; *****************************INICIO SEGURO?*************************************
;; Bloco List Tamanho -> Lógico
;; -----------------------------
;; Verifica se um bloco é seguro de ser adicionado a uma solução. Ser 
;; seguro significa que, ao ser adicionado à solução, o bloco se 
;; encaixa a todos os blocos adjacentes à posição em que ele seria 
;; inserido. Uma solução é uma lista de blocos que representa a solução 
;; do jogo até o presente momento. Para facilitar a implementação, 
;; considere que a solução será construída em ordem invertida. Assim, a 
;; solução '(9 7 12 14 6), referente ao um jogo de tamanho 4x3, 
;; representa a seguinte situação:
;; 
;; [6][14][12]    [┏][┳][┓]
;; [7][ 9][  ] => [┣][┛][ ]
;; [ ][  ][  ]    [ ][ ][ ]
;; [ ][  ][  ]    [ ][ ][ ]
;; 
;; A chamada (seguro? 5 '(9 7 12 14 6) (tamanho 4 3)) deve verificar se 
;; o bloco 5 [┃] é seguro de ser adicionado à solução, isto é, inserido 
;; na posição lin=2, col=3 da situação descrita acima. Observe que para 
;; este exemplo o bloco 5 é seguro, pois ele se encaixa a todos os 
;; blocos adjacentes: ao bloco 9 à esquerda, ao bloco 12 acima e à 
;; borda direita (branco) do tabuleiro. Veja que não houve necessidade 
;; de se verificar o encaixe com o bloco abaixo, já que o mesmo ainda 
;; não existe na solução.
(define (seguro? bloco solucao tam)
  (define bloco-binario (number->string bloco 2))
  (define solucao-reversa (reverse solucao))
  (define bloco-esq (cond [(= (length solucao) tam-largura) 0] [else (first solucao-reversa)]))
  (define bloco-dir (cond [(<= (length solucao) tam-largura) 0] [else (first solucao-reversa)]))
  (define bloco-cima (cond [(= (add1 (length solucao)) tam-largura) 0] [else (list-ref solucao (- (length solucao) 3))]))
  (define bloco-baixo)
  (cond
    [(empty? solucao)
       (and (encaixa-h? 0 bloco) (encaixa-v? 0 bloco))] ;;so cai aqui se for o primeiro bloco, por isso verifica se encaixa na boda vertical e em cima    
    [else
     (cond
       [(borda-dir? solucao tam)
        (and (encaixa-h? bloco-esq bloco) (encaixa-v? bloco-cima bloco) (encaixa-h? bloco bloco-dir))]
       [else (and (encaixa-h? bloco-esq bloco) (encaixa-v? bloco-cima bloco))])]))

(define (borda-dir? solucao tam)
  (cond
    [(= (remainder (add1(length solucao)) tam-largura) 0) true]
    [else false]))
;; *****************************FIM SEGURO?*************************************

;; *****************************INICIO LER-JOGO*************************************
;; String -> Jogo
;; Faz a leitura e processa um jogo armazenado em arquivo.
;; Exemplo: (ler-jogo "testes/5.txt")
;;          > '((0 6 6 1) (12 15 15 6) (1 10 10 0) (0 2 1 0))
(define (ler-jogo arquivo) 
;; Dica: procure pelas funções pré-definidas open-input-file e port->lines
  (define (transforma-para-decimal lst)
    (cond
    [(empty? lst) empty]
    [(list? (first lst))
     (cons (transforma-para-decimal (first lst))
           (transforma-para-decimal (rest lst)))]
    [else
     (cons (to-decimal (first lst) blocos-reps 0)
           (transforma-para-decimal (rest lst)))]))
  
  (transforma-para-decimal(map string->list(port->lines(open-input-file arquivo)))))

(define (to-decimal char blocos-reps n)
  (cond
    [(equal? (first blocos-reps) char) n]
    [else (to-decimal char (rest blocos-reps) (add1 n))]))
;; *****************************FIM LER-JOGO*************************************

;; *****************************INICIO ESCREVER-JOGO*************************************
;; Jogo -> void
;; Escreve o jogo na tela codificado em caracteres.
;; Exemplo: (escrever-jogo '((6 10 14 12) (7 14 13 5) (5 7 11 13) (3 11 10 9)))
;;          > ┏━┳┓
;;            ┣┳┫┃
;;            ┃┣┻┫
;;            ┗┻━┛
(define (escrever-jogo jogo) void)
;; Dica: procure pelas funções pré-definidas list->string e string-join

;; *****************************FIM ESCREVER-JOGO*************************************

;; *****************************INICIO RESOLVER*************************************
;; Jogo -> Jogo || #f
;; Resolve o jogo Infinity e o retorna resolvido. Caso não seja possível
;; resolvê-lo, retorna o valor falso. Por exemplo, se passado o seguinte jogo:
;;
;; '(( 0  6  6 1)         [ ][┏][┏][╹]
;;   (12 15 15 6)    =>   [┓][╋][╋][┏]
;;   ( 1 10 10 0)         [╹][━][━][ ]
;;   ( 0  2  1 0))        [ ][╺][╹][ ]
;;
;; a função deve retornar:
;;
;; '((0  6 12 4)          [ ][┏][┓][╻]
;;   (6 15 15 9)     =>   [┏][╋][╋][┛]
;;   (1  5  5 0)          [╹][┃][┃][ ]
;;   (0  1  1 0))         [ ][╹][╹][ ]
(define (resolver jogo)
  (define tam-jogo (verifica-tamanho jogo))
  (define possibilidades (cria-lista-possibilidades-blocos jogo))
  (display possibilidades))
 ;; (iter empty possibilidades tam-jogo))

#|(define (iter solucao possibilidades tam-jogo)
  (cond
    [(empty? possibilidades) solucao]
    [(list? (first possibilidades) )
    [else
     [(empty? (first(first possibilidades))) #f]
     [(seguro? (first(first(first possibilidades))) solucao tam-jogo)
      (or (iter ((cons(first(first(first possibilidades))))) (rest(first possibilidades))) tam-jogo)
      (iter solucao (rest(first(first possibilidades))) tam-jogo)]
     [else (iter solucao (rest(first(first possibilidades))) tam-jogo)]]]))|#


;; possibilidades = lista de candidatos (rotações) para cada bloco do jogo (em ordem)
;; candidatos = lista de rotações do bloco atual = (first possibilidades)
;; candidato = rotação atual do bloco atual = (first candidatos)

;; *****************************FIM RESOLVER*************************************

;; *****************************INICIO MAIN*************************************
;; List String -> void
;; Esta é a função principal. Esta função é chamada a partir do arquivo
;; infinity-main.rkt
;;
;; args é a lista de parâmetros para o programa.
;;
;; O primeiro e único parâmetro deve ser o nome (caminho) do arquivo que contém 
;; o jogo a ser resolvido. O jogo é representado na forma de caracteres, o qual
;; deverá ser primeiramente convertido para a representação numérica antes de
;; ser resolvido. Veja exemplos de arquivos no diretório de testes.
;;
;; A saída desta função é a escrita na tela do jogo resolvido, representado na
;; forma de caracteres. Caso o jogo não possua solução, nada deve ser escrito na
;; tela.
(define (main args)
  (display args)
  (define jogo (ler-jogo args)) ;;retorna lista em decimal
  (define solucao (resolver jogo))
  (escrever-jogo solucao))
;; *****************************FIM MAIN*************************************
