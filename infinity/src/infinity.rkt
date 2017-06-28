#lang racket
#| Resolvedor Infinity em Racket |#

#| Aluna: Mayza Yuri Hirose da Costa RA88738
   Bacharelado em Informática
   Universidade Estadual de Maringá - UEM - 2017
   Disciplina: 5200 -Paradigma de Programação Lógica e Funcional
   Professor: Lucas Pupulin Nanni |#

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

;; *****************************INICIO TAMANHO DO JOGO**********************************
;; Tamanho representa o tamanho de um jogo em altura e largura
;;     altura : Número - quantidade de linhas do jogo.
;;    largura : Número - quantidade de colunas do jogo.
(struct tamanho (altura largura) #:transparent)

;; Jogo -> Struct tamanho
;; ----------------------
;; Retorna uma struct tamanho com as dimensões do jogo
;; Exemplo: (jogo->tamanho '((3 14 12) (3 11 12)))
;;          > (tamanho 2 3)
(define (jogo->tamanho jogo)
  (cond
    [(empty? jogo) 0]
    [else
     (tamanho (length jogo)(length (first jogo)))]))
;; *****************************FIM TAMANHO DO JOGO**********************************

;; *****************************INICIO ROTACIONAR*************************************
;; Bloco -> Bloco
;; --------------
;; Rotaciona um bloco 90 graus em sentido horário
;; Exemplo: (rotacionar 5)
;;          > 10
(define (rotacionar bloco) ;;ARRUMAR ESTE ROTACIONAR, CÓDIGO ESTÁ NO OUTRO ARQUIVO.. ESTE FOI SO P PODER PROSSEGUIR COM O RESTO POIS ESTAVA ATRASANDO
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

;; Jogo -> List-Possibilidades
;; --------------
;; Recebe uma lista em decimal referente ao jogo e cria uma lista com todas as possibilidades de rotação de cada bloco
;; Exemplo: (jogo->possibilidades '((3 14 12) (3 11 12)))
;;          > '(((3 6 12 9) (14 13 11 7) (12 9 3 6)) ((3 6 12 9) (11 7 14 13) (12 9 3 6)))
(define (jogo->possibilidades jogo)
  (cond
    [(empty? jogo) empty]
    [(list? (first jogo))
     (cons (jogo->possibilidades (first jogo))
           (jogo->possibilidades (rest jogo)))]
    [else
     (cons (bloco->possibilidades (first jogo))
           (jogo->possibilidades (rest jogo)))]))

;; Bloco -> List-Possibilidades
;; --------------
;; Recebe um bloco e retorna uma lista com as rotações dele em decimal
;; Exemplo: (bloco->possibilidades 3)
;;          > '(3 6 12 9)
(define (bloco->possibilidades bloco)
  (define r1 (rotacionar bloco))
  (define r2 (rotacionar r1))
  (define r3 (rotacionar r2))
  (cond [(= 15 bloco) (list bloco 16 16 16)]
        [(= 5 bloco) (list bloco 10 16 16)]
        [(= 10 bloco) (list bloco 5 16 16)]
        [(= 0 bloco) (list bloco 16 16 16)]
        [else (list bloco r1 r2 r3)]))
;; *****************************FIM ROTACIONAR*************************************

;; *****************************INICIO ENCAIXA-H?*************************************
;; Bloco Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco-e se encaixa horizontalmente à esquerda do bloco-d
;; Exemplo: (encaixa-h? 7 9)
;;          > #t
(define (encaixa-h? bloco-e bloco-d)
  (define bloco-e-binario (bloco->binario bloco-e))
  (define bloco-d-binario (bloco->binario bloco-d))
  (equal? (list-ref bloco-e-binario 2) (list-ref bloco-d-binario 0)))
;; *****************************FIM ENCAIXA-H?*************************************

;; *****************************INICIO ENCAIXA-V?*************************************
;; Bloco Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco-t se encaixa verticalmente acima do bloco-b
;; Exemplo: (encaixa-v? 14 11)
;;          > #t
(define (encaixa-v? bloco-t bloco-b)
  (define bloco-t-binario (bloco->binario bloco-t))
  (define bloco-b-binario (bloco->binario bloco-b))
  (equal? (list-ref bloco-t-binario 1) (list-ref bloco-b-binario 3)))

;; Bloco -> Binário
;; ---------------------
;; Recebe um bloco e retorna sua representação em binário
;; Exemplo: (bloco->binario 3)
;;          > '(#\0 #\0 #\1 #\1)
(define (bloco->binario bloco)
  (define list-binary (string->list (number->string bloco 2)))
  (define zeros-esq (- 4 (length list-binary)))
  (define (junta-zeros list-binary n)
    (cond
      [(zero? n) list-binary]
      [else (junta-zeros (append (list #\0) list-binary) (sub1 n))]))
  (junta-zeros list-binary zeros-esq))

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
  (define bloco-dir 0);;Sempre zero pois é um bloco que sempre está na parede-dir ou não tem solucao na direita
  (define bloco-baixo 0);;Sempre zero pois é um bloco que sempre está no térreo ou não tem solucao abaixo
  (define bloco-esq
    (cond
      [(or (empty? solucao) (borda-esq? solucao tam)) 0]
      [else (first solucao)]))
  
  (define bloco-cima
    (cond
      [(or (empty? solucao) (borda-cima? solucao tam)) 0]
      [else (list-ref solucao (- (tamanho-largura tam) 1))]))
  
  (cond
    [(borda-dir? solucao tam)
     (cond
       [(borda-baixo? solucao tam)
        (and (encaixa-h? bloco-esq bloco) (encaixa-v? bloco-cima bloco) (encaixa-h? bloco bloco-dir) (encaixa-v? bloco bloco-baixo))]
       [else
        (and (encaixa-h? bloco-esq bloco) (encaixa-v? bloco-cima bloco) (encaixa-h? bloco bloco-dir))])]    
    [else     
     (cond
       [(borda-baixo? solucao tam)
        (and (encaixa-h? bloco-esq bloco) (encaixa-v? bloco-cima bloco) (encaixa-v? bloco bloco-baixo))]
       [else
        (and (encaixa-h? bloco-esq bloco) (encaixa-v? bloco-cima bloco))])]))

(define (borda-dir? solucao tam)
  (= (remainder (add1(length solucao)) (tamanho-largura tam)) 0))

(define (borda-esq? solucao tam)
  (= (remainder (length solucao) (tamanho-largura tam)) 0))

(define (borda-baixo? solucao tam)
  (>= (+ (length solucao) (tamanho-largura tam)) (* (tamanho-altura tam) (tamanho-largura tam))))

(define (borda-cima? solucao tam)
  (< (length solucao) (tamanho-largura tam)))
;; *****************************FIM SEGURO?*************************************

;; *****************************INICIO LER-JOGO*************************************
;; String -> Jogo
;; Faz a leitura e processa um jogo armazenado em arquivo.
;; Exemplo: (ler-jogo "testes/5.txt")
;;          > '((0 6 6 1) (12 15 15 6) (1 10 10 0) (0 2 1 0))
(define (ler-jogo arquivo) 
  (define (transforma-para-decimal lst)
    (cond
    [(empty? lst) empty]
    [(list? (first lst))
     (cons (transforma-para-decimal (first lst))
           (transforma-para-decimal (rest lst)))]
    [else
     (cons (to-decimal (first lst) blocos-reps 0)
           (transforma-para-decimal (rest lst)))]))
  
  (transforma-para-decimal(map string->list(port->lines(open-input-file (cond [(list? arquivo)(first arquivo)][else arquivo]))))))

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
;; Dica: procure pelas funções pré-definidas list->string e string-join
(define (escrever-jogo jogo0 tam-jogo)
  (define jogo-resolvido (jogo->char jogo0))
  (define (exibe-jogo jogo tam)
    (cond
      [(empty? jogo) (display "")]
      [else (display (list->string(take jogo (tamanho-largura tam))))
            (cond
              [(empty?(drop jogo (tamanho-largura tam))) (display "")]
              [else (display "\n")])
            (exibe-jogo (drop jogo (tamanho-largura tam)) tam)])) 
  (exibe-jogo jogo-resolvido tam-jogo))

;; Jogo -> char
;; Recebe um jogo e transforma em uma lista de caracteres do jogo
;; Exemplo: (jogo->char '((6 10 14 12) (7 14 13 5) (5 7 11 13) (3 11 10 9)))
;;          > '((#\┏ #\━ #\┳ #\┓) (#\┣ #\┳ #\┫ #\┃) (#\┃ #\┣ #\┻ #\┫) (#\┗ #\┻ #\━ #\┛))
(define (jogo->char jogo)
    (cond
    [(empty? jogo) empty]
    [(list? (first jogo))
     (cons (jogo->char (first jogo))
           (jogo->char (rest jogo)))]
    [else
     (cons (dec->char (first jogo))
           (jogo->char (rest jogo)))]))
  
(define (dec->char decimal)
  (list-ref blocos-reps decimal))
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
  (define tam-jogo (jogo->tamanho jogo))
  (define possibilidades (aplaina (jogo->possibilidades jogo)))
  (iter '() possibilidades tam-jogo 4)) 

;; Backtracking. ACC é a quantidade de possibilidades de um bloco.
(define (iter solucao possibilidades tam-jogo acc)
  (cond
    [(empty? possibilidades) solucao]
    [(zero? acc) #f]
    [(= 16 (first possibilidades)) #f]
    [(seguro? (first possibilidades) solucao tam-jogo)
     (or (iter (append (list (first possibilidades)) solucao) (drop possibilidades acc) tam-jogo 4)
         (iter solucao (rest possibilidades) tam-jogo (sub1 acc)))]
    [else (iter solucao (rest possibilidades) tam-jogo (sub1 acc))]))

;; Lista aninhada -> lista Plana
;; Aplaina uma lista aninhada
;; Exemplo: (aplaina '((6 10 14 12) (7 14 13 5)))
;;          > '(6 10 14 12 7 14 13 5)
(define (aplaina lst)
  (cond
    [(empty? lst) empty]
    [(list? (first lst))
     (append (aplaina (first lst))
             (aplaina (rest lst)))]
    [else
     (cons (first lst)
           (aplaina (rest lst)))]))
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
  (define jogo (ler-jogo args))
  (define tam-jogo (jogo->tamanho jogo))
  (define solucao (resolver jogo))
  (escrever-jogo (reverse solucao) tam-jogo))
;; *****************************FIM MAIN*************************************
