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

;; Tamanho representa o tamanho de um jogo em altura e largura
;;     altura : Número - quantidade de linhas do jogo.
;;    largura : Número - quantidade de colunas do jogo.
(struct tamanho (altura largura) #:transparent)

(define (game-heigth lista-jogo)
  (cond
    [(empty? lista-jogo) 0]
    [else (length lista-jogo)]))

(define (game-width lista-jogo)
  (cond
    [(empty? lista-jogo) 0]
    [else (length (first lista-jogo))]))

(define (transforma-binario lst)
  (cond
    [(empty? lst) empty]
    [(list? (first lst))
     (cons (transforma-binario (first lst))
           (transforma-binario (rest lst)))]
    [else
     (cons (string->list (number->string (first lst) 2))
           (transforma-binario (rest lst)))]))

(define (transforma-normal lst)
  (cond
    [(empty? lst) empty]
    [(list? (first lst))
     (cons (transforma-normal (first lst))
           (transforma-normal (rest lst)))]
    [else
     (cons (string->number (first lst) 2)
           (transforma-normal (rest lst)))]))
  
;; Bloco -> Bloco
;; --------------
;; Rotaciona um bloco 90 graus em sentido horário
;; Exemplo: (rotacionar 5)
;;          > 10
(define (rotacionar bloco)
  (define bloco-binario (string->list (number->string bloco 2)))
  (define zeros-esquerda (- 4 (length bloco-binario)))
  (define S (for/list([x (in-range 0 zeros-esquerda)])
            (* 0 x))) 
  (define lista-bloco-binario (append S bloco-binario))
  (display lista-bloco-binario)
  
  ;;(define rotacionado (append (rest lista-bloco-binario) (first lista-bloco-binario)))
  (define rotacionado (cons-fim (first lista-bloco-binario) (rest lista-bloco-binario)))
  ;;(define rotacionado (list 1 0 1 0))
  (display rotacionado)
  (display (number? (first rotacionado)))
  ;;(display (flatten rotacionado))
  (bin->dec rotacionado))
    
(define (cons-fim a lst)
  (cond
    [(empty? lst) (list a)]
    [else (cons (first lst)
                (cons-fim a (rest lst)))]))
;;acc é o valor acumulado referente ao valor binario em decimal, ja o acc2 é o que guarda o proximo valor da seq binária.
;; exemplo: (0110) acc = 0, acc2 = 8 (pois da esq p dir o primeiro digito vale 8); acc = 0 acc2 = 4 ..
(define (bin->dec lst0)
  (define (iter lst acc acc2)
    (cond
      [(empty? lst) acc]
      [else
       (cond         
         [(equal? "0" (first lst))
          (iter (rest lst) acc (/ acc2 2))]
         [else
          (iter (rest lst) (+ acc acc2) (/ acc2 2))])]))
  (iter lst0 0 8))

;;(define (bin->dec n)
  ;;(if (zero? n)
    ;;  n
      ;;(+ (modulo n 10) (* 2 (bin->dec (quotient n 10))))))

;; Bloco Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco-e se encaixa horizontalmente à esquerda do bloco-d
;; Exemplo: (encaixa-h? 7 9)
;;          > #t
(define (encaixa-h? bloco-e bloco-d) #f)


;; Bloco Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco-t se encaixa verticalmente acima do bloco-b
;; Exemplo: (encaixa-v? 14 11)
;;          > #t
(define (encaixa-v? bloco-t bloco-b) #f)


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
  (cond
    [(empty? solucao)false
    ]))

;; String -> Jogo
;; Faz a leitura e processa um jogo armazenado em arquivo.
;; Exemplo: (ler-jogo "testes/5.txt")
;;          > '((0 6 6 1) (12 15 15 6) (1 10 10 0) (0 2 1 0))
(define (ler-jogo arquivo)
  (transforma-para-numero(transforma-para-lista-char(port->lines(open-input-file arquivo)))))

(define (transforma-para-lista-char lst)
  (cond
    [(empty? lst) empty]
    [else (cons (string->list (first lst))
                (transforma-para-lista-char (rest lst)))]))

(define (transforma-para-numero lst)
  (cond
    [(empty? lst) empty]
    [(list? (first lst))
     (cons (transforma-para-numero (first lst))
           (transforma-para-numero (rest lst)))]
    [else
     (cons (substitui-pelo-num (first lst) blocos-reps 0)
           (transforma-para-numero (rest lst)))]))

(define (substitui-pelo-num char blocos-reps n)
  (cond
    [(equal? (first blocos-reps) char) n]
    [else (substitui-pelo-num char (rest blocos-reps) (add1 n))]))

;; Dica: procure pelas funções pré-definidas open-input-file e port->lines

;; Jogo -> void
;; Escreve o jogo na tela codificado em caracteres.
;; Exemplo: (escrever-jogo '((6 10 14 12) (7 14 13 5) (5 7 11 13) (3 11 10 9)))
;;          > ┏━┳┓
;;            ┣┳┫┃
;;            ┃┣┻┫
;;            ┗┻━┛
(define (escrever-jogo jogo)
  (display "hellow"))
;; Dica: procure pelas funções pré-definidas list->string e string-join

(define (cria-lista-possibilidades jogo)
  (cond
    [(empty? jogo) empty]
    [(list? (first jogo))
     (list (cria-lista-possibilidades (first jogo))
           (cria-lista-possibilidades (rest jogo)))]
    [else
     (list (cria-possibilidades-bloco (first jogo))
           (cria-lista-possibilidades (rest jogo)))]))

(define (cria-possibilidades-bloco bloco)
  (display bloco)
  (define p1 (rotacionar bloco))
  (define p2 (rotacionar p1))
  (define p3 (rotacionar p2))
  (define p4 (rotacionar p3))
  (list p1 p2 p3 p4))
  ;;(cons 1 (cons 2 (cons 3 (cons 4 empty)))))


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
  (display jogo)
  (define tam (tamanho (game-heigth jogo) (game-width jogo)))
  (define possibilidades (cria-lista-possibilidades jogo))
  (display possibilidades)
  (display possibilidades)) 
  ;;(define jogo-binario (transforma-binario jogo)) 
  ;;(display jogo-binario))
  ;;(display possibilidades))
  ;;(define jogo-normal (transforma-normal jogo-binario))
  ;;(display jogo-normal))

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
;;(define (iter solucao possibilidades)
  ;;(cond
    ;;[(empty? possibilidades) solucao]
    ;;[(empty? (first possibilidades)) #f]
    ;;[(seguro? (first (first possibilidadades)) solucao tam)
     ;;(or (iter (adiciona candidato solucao) (remove candidatos possibilidades))
       ;;  (iter solucao (exclui candidato possibilidades)))]
    ;;[else (iter solucao (exclui candidato possibilidades))]))

(define (main args)
  ;;(display args)
  (define jogo (ler-jogo args))
  (define solucao (resolver jogo))
  (escrever-jogo solucao)
)

