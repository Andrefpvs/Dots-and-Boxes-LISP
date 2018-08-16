;GRUPO 17, Andre Rodrigues 69998, Andre Silva 68707, Sofia Modesto 70206

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TIPOS DE INFORMACAO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;POSICAO
;Utilizado para representar uma posicao do tabuleiro, atraves da sua linha e coluna.

(defstruct posicao
                    linha
                    coluna
)

(defun cria-posicao (linha coluna)
					(make-posicao :linha linha :coluna coluna)
)

(defun posicoes-iguais-p (p1 p2)
                        (and (= (posicao-linha p1) (posicao-linha p2)) (= (posicao-coluna p1) (posicao-coluna p2)))
)


;FIO
;Utilizado para representar um fio do tabuleiro que liga um par de moedas (moeda-1 e moeda-2).
(defstruct fio
                    id
					origem
					destino
)
					
(defun cria-fio (identificador origem destino)
				(make-fio :id identificador :origem origem :destino destino )
)
                
;MOEDA
;Utilizado para representar uma moeda do tabuleiro que tem um valor
(defstruct moeda
                    valor
					posicao
					)

(defun cria-moeda (valor posicao)
                (make-moeda :valor valor :posicao posicao)
                )


;TABULEIRO
;Utilizado para representar o estado de um tabuleiro de jogo, contendo informacao sobre as moedas e fios nele existentes.
(defstruct tabuleiro
                    nlinhas
                    ncolunas
                    t-moedas
                    t-fios
					fio-id
					valor-total-moedas
)
					
(defun cria-tabuleiro (nl nc)
                       (make-tabuleiro :nlinhas nl :ncolunas nc :fio-id 0 :valor-total-moedas 0)
)
					   
(defun copia-tabuleiro (tab)
                       (make-tabuleiro :nlinhas (tabuleiro-nlinhas tab) :ncolunas (tabuleiro-ncolunas tab) :t-moedas (tabuleiro-t-moedas tab) :t-fios (tabuleiro-t-fios tab) 
					   :fio-id (tabuleiro-fio-id tab) :valor-total-moedas (tabuleiro-valor-total-moedas tab))
)

(defun tabuleiro-linhas (tab)
                        (tabuleiro-nlinhas tab)
)

(defun tabuleiro-colunas (tab)
                        (tabuleiro-ncolunas tab)
)

(defun tabuleiro-fios (tab)
                       (tabuleiro-t-fios tab)
)

(defun tabuleiro-fio-com-id (tab identificador)
		(let ((fio nil))
                        (loop for x in (tabuleiro-t-fios tab) do
                            (if (= (fio-id x) identificador)
                            (setf fio x) ))
						fio
        )
)
						
(defun tabuleiro-fios-posicao (tab posicao)
						(labels ((tabuleiro-fios-posicao-aux (fios posicao ret)
							(cond ((null fios) ret)
									((or (posicoes-iguais-p (fio-origem (first fios)) posicao) (posicoes-iguais-p(fio-destino (first fios)) posicao))
										(tabuleiro-fios-posicao-aux (rest fios) posicao (cons (first fios) ret)))
									(T (tabuleiro-fios-posicao-aux (rest fios) posicao ret)))
										
							))
						(tabuleiro-fios-posicao-aux (tabuleiro-t-fios tab) posicao ())
                        )
)
                        
(defun tabuleiro-moeda-posicao (tab posicao)
		(let ((valor nil))
                            (loop for x in (tabuleiro-t-moedas tab) do
                                (if (posicoes-iguais-p (moeda-posicao x) posicao)
								(setf valor  (moeda-valor x)))
                            )
							valor
        )
)

(defun tabuleiro-total-moedas (tab)
							(tabuleiro-valor-total-moedas tab)
)
                        
(defun tabuleiro-adiciona-fio! (tabuleiro posicao1 posicao2)
						(setf (tabuleiro-t-fios tabuleiro) (cons (cria-fio (incf (tabuleiro-fio-id tabuleiro)) posicao1 posicao2) (tabuleiro-t-fios tabuleiro)) )
)
                        
(defun tabuleiro-adiciona-moeda-posicao! (tabuleiro posicao valor)
		(let ((moeda nil))
                        (loop for x in (tabuleiro-t-moedas tabuleiro) do
                            (if (posicoes-iguais-p (moeda-posicao x) posicao)
								(setf moeda x))
                        )
						(if (null moeda)
							(progn
								(setf (tabuleiro-valor-total-moedas tabuleiro) (+ (tabuleiro-valor-total-moedas tabuleiro) valor))
								(setf (tabuleiro-t-moedas tabuleiro) (cons (cria-moeda valor posicao) (tabuleiro-t-moedas tabuleiro))))
							(progn 
								(setf (tabuleiro-valor-total-moedas tabuleiro) (- (+ (tabuleiro-valor-total-moedas tabuleiro) valor) (moeda-valor moeda)))
								(setf (moeda-valor moeda) valor))
                        )
		)
)

(defun tabuleiro-remove-fio-com-id! (tabuleiro identificador)
						(labels ((tabuleiro-remove-fio-com-id-aux (fios-nvistos identificador fios-vistos)
									(cond 	((null fios-nvistos)
												fios-vistos)
											((= (fio-id (first fios-nvistos)) identificador)
											(append fios-vistos (rest fios-nvistos)))
											(T (tabuleiro-remove-fio-com-id-aux (rest fios-nvistos) identificador (cons (first fios-nvistos) fios-vistos)))
									
									)))
						(setf (tabuleiro-t-fios tabuleiro ) (tabuleiro-remove-fio-com-id-aux (tabuleiro-t-fios tabuleiro) identificador ()))))
						
(defun tabuleiro-remove-moeda-posicao! (tabuleiro posicao)
						(labels ((tabuleiro-remove-moeda-posicao-aux (moedas-nvistos posicao moedas-vistos tab)
									(cond 	((null moedas-nvistos)
												moedas-vistos)
											((posicoes-iguais-p (moeda-posicao (first moedas-nvistos)) posicao)
												(setf (tabuleiro-valor-total-moedas tab) (- (tabuleiro-valor-total-moedas tab) (moeda-valor (first moedas-nvistos))))
												(append moedas-vistos (rest moedas-nvistos)))
											(T (tabuleiro-remove-moeda-posicao-aux (rest moedas-nvistos) posicao (cons (first moedas-nvistos) moedas-vistos) tab))
									
									)))
						(setf (tabuleiro-t-moedas tabuleiro ) (tabuleiro-remove-moeda-posicao-aux (tabuleiro-t-moedas tabuleiro) posicao () tabuleiro ))))
                        


;JOGO
;Representa o estado de uma instancia do jogo. Guarda o estado do tabuleiro, o numero de pontos de cada jogador, 
;qual o proximo jogador a jogar e o historico de todas as accoes efectuadas desde o inicio do jogo.
(defstruct jogo
                    estado-tabuleiro
                    pontos-jogador-a
                    pontos-jogador-b
                    proximo-jogador
                    historico
)


(defun cria-jogo (tabuleiro) 
                    (make-jogo :estado-tabuleiro tabuleiro :pontos-jogador-a 0 :pontos-jogador-b 0 :proximo-jogador 1 :historico () )
)

(defun copia-jogo (jogo)
                    (make-jogo :estado-tabuleiro (copia-tabuleiro (jogo-estado-tabuleiro jogo)) :pontos-jogador-a (jogo-pontos-jogador-a jogo) :pontos-jogador-b (jogo-pontos-jogador-b jogo)
					:proximo-jogador (jogo-proximo-jogador jogo) :historico (jogo-historico jogo))
)

(defun jogo-tabuleiro (jogo)
                    (jogo-estado-tabuleiro jogo)
)
                    
(defun jogo-jogador (jogo)
                    (jogo-proximo-jogador jogo)
)

(defun jogo-pontos-jogador1 (jogo)
                    (jogo-pontos-jogador-a jogo)
)


(defun jogo-pontos-jogador2 (jogo)
                    (jogo-pontos-jogador-b jogo)
)
                    
(defun jogo-historico-jogadas (jogo)
                    (jogo-historico jogo)
)

(defun jogo-aplica-jogada! (jogo jogada)
		(let (	(fio nil)
				(pontosganhos 0))
				(setf fio (tabuleiro-fio-com-id (jogo-tabuleiro jogo) jogada))
				(if(not(null fio))
					(progn
						(setf (jogo-historico jogo) (append (jogo-historico-jogadas jogo) (list jogada)))
						(tabuleiro-remove-fio-com-id! (jogo-tabuleiro jogo) jogada)
						(if (null (tabuleiro-fios-posicao (jogo-tabuleiro jogo) (fio-origem fio)))
							(progn (setf pontosganhos (tabuleiro-moeda-posicao (jogo-tabuleiro jogo) (fio-origem fio)))
									(tabuleiro-remove-moeda-posicao! (jogo-tabuleiro jogo) (fio-origem fio))))
						(if (null (tabuleiro-fios-posicao (jogo-tabuleiro jogo) (fio-destino fio)))
							(progn (setf pontosganhos (+ pontosganhos (tabuleiro-moeda-posicao (jogo-tabuleiro jogo) (fio-destino fio))))
									(tabuleiro-remove-moeda-posicao! (jogo-tabuleiro jogo) (fio-destino fio))))
						(if (not(= pontosganhos 0))
							(if (= (jogo-jogador jogo) 1)
								(setf (jogo-pontos-jogador-a jogo) (+ (jogo-pontos-jogador-a jogo) pontosganhos))
								(setf (jogo-pontos-jogador-b jogo) (+ (jogo-pontos-jogador-b jogo) pontosganhos))
							)
							(setf (jogo-proximo-jogador jogo) (+(rem (+ (jogo-jogador jogo) 2) 2)1) )
						)
					)
				)
		)
)
				
				
(defun jogo-terminado-p (jogo)
                    (null (tabuleiro-fios (jogo-estado-tabuleiro jogo)))
)


;PROBLEMA
;Representa um problema de procura adversaria.
(defstruct problema    
                    estado-inicial
                    jogador
                    accoes
                    resultado
                    teste-corte-p
                    funcao-avaliacao
                    historico-accoes
                    chave-equivalencia
)
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCOES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun accoes (jogo)
		(let ((lista-id ()))
			(loop for x in (tabuleiro-t-fios (jogo-estado-tabuleiro jogo )) do
				(setf lista-id (cons (fio-id x) lista-id))
			)
			(sort lista-id '>)
		)
)
		
(defun resultado (jogo id)
		(let ((novo-jogo (copia-jogo jogo)))
			(jogo-aplica-jogada! novo-jogo id)
			novo-jogo
		)
)
		
(defun teste-terminal-p (jogo profundidade)
		(declare (ignore profundidade))
		(jogo-terminado-p jogo)
)
		
(defun utilidade (jogo jogador)
		(if (= jogador 1)
			(- (jogo-pontos-jogador1 jogo) (jogo-pontos-jogador2 jogo))
			(- (jogo-pontos-jogador2 jogo) (jogo-pontos-jogador1 jogo))
		)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MINIMAX;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Minimax - Adaptacao para CLISP do algoritmo Minimax, tendo em conta o facto de o mesmo jogador
;poder jogar mais do que uma vez.
(defun minimax (problema inteiro)
	(let (	(accao nil)
			(valor nil)
			(aux-valor 0)
			(jogo-aux nil)
			(res nil)
			(nos 0))
			(if(= (funcall (problema-jogador problema) (problema-estado-inicial problema)) inteiro)
				(setf valor most-negative-fixnum)
				(setf valor most-positive-fixnum)
				)
			(loop for a in (funcall (problema-accoes problema) (problema-estado-inicial problema)) do
				(setf jogo-aux (funcall (problema-resultado problema) (problema-estado-inicial problema) a))
				(if(= (funcall (problema-jogador problema) (problema-estado-inicial problema)) inteiro) 
					(progn																																	
						(if (= (funcall (problema-jogador problema) (problema-estado-inicial problema)) (funcall (problema-jogador problema) jogo-aux))   	
							(progn	(setf res (multiple-value-list (max-value jogo-aux problema nos)))
									(setf aux-valor (first res))
									(setf nos (second res))
							)
							(progn	(setf res (multiple-value-list (min-value jogo-aux problema nos)))
									(setf aux-valor (first res))
									(setf nos (second res))
							)										
						)
						(if (< valor aux-valor)					
								(progn 	(setf accao a)
										(setf valor aux-valor))
						)
					)
					(progn																																	
						(if (= (funcall (problema-jogador problema) (problema-estado-inicial problema)) (funcall (problema-jogador problema) jogo-aux))   	
							(progn	(setf res (multiple-value-list (min-value jogo-aux problema nos)))
									(setf aux-valor (first res))
									(setf nos (second res))
							)
							(progn	(setf res (multiple-value-list (max-value jogo-aux problema nos)))
									(setf aux-valor (first res))
									(setf nos (second res))
							)
						)
						(if (> valor aux-valor)					
								(progn 	(setf accao a)
										(setf valor aux-valor))
						)
					)
				)
			)
		(values accao valor nos)
	)
)
	
(defun max-value (jogo problema nos)
	(let (	(v most-negative-fixnum)
			(jogo-aux nil)
			(res nil)
			(nosAux nos))
		(if (funcall (problema-teste-corte-p problema) jogo 10)
			(progn
				(incf nosAux)
				(setf v (funcall (problema-funcao-avaliacao problema) jogo (funcall (problema-jogador problema) jogo)))	
			)
			(progn 	(loop for a in (funcall (problema-accoes problema) jogo) do
						(setf jogo-aux (funcall (problema-resultado problema) jogo a))
						(if (= (funcall (problema-jogador problema) jogo) (funcall (problema-jogador problema) jogo-aux))
							(progn	(setf res (multiple-value-list (max-value jogo-aux problema nosAux)))
									(setf v (max v (first res)))
									(setf nosAux (second res))
							)
							(progn	(setf res (multiple-value-list (min-value jogo-aux problema nosAux)))
									(setf v (max v (first res)))
									(setf nosAux (second res))
							)
						)
					)
			)
		)
		(values v nosAux)
	)
)
	
(defun min-value (jogo problema nos)
	(let (	(v most-positive-fixnum)
			(jogo-aux nil)
			(res nil)
			(nosAux nos))
		(if (funcall (problema-teste-corte-p problema) jogo 10)
			(progn
				(incf nosAux)
				(setf v (funcall (problema-funcao-avaliacao problema) jogo (+ (rem (+ (funcall (problema-jogador problema) jogo) 2) 2)1)))
			)
			(progn 																		
					(loop for a in (funcall (problema-accoes problema) jogo) do
						(setf jogo-aux (funcall (problema-resultado problema) jogo a))
						(if (= (funcall (problema-jogador problema) jogo) (funcall (problema-jogador problema) jogo-aux))
							(progn	(setf res (multiple-value-list (min-value jogo-aux problema nosAux)))
									(setf v (min v (first res)))
									(setf nosAux (second res))
							)
							(progn	(setf res (multiple-value-list (max-value jogo-aux problema nosAux)))
									(setf v (min v (first res)))
									(setf nosAux (second res))
							)
						)
					)
			)
		)
		(values v nosAux)
	)
)
	
;Jogador Minimax Simples - Versao do jogador que utiliza o algoritmo Minimax definido acima
(defun jogador-minimax-simples (jogo jogador tempo)
	(declare (ignore tempo))
		(car (multiple-value-list (minimax (make-problema :estado-inicial jogo :jogador #'jogo-jogador :accoes #'accoes :resultado #'resultado :teste-corte-p #'teste-terminal-p :funcao-avaliacao #'utilidade) jogador)))
)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Alfa-Beta;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

;Minimax Alfa-Beta - Versao do algoritmo Minimax com hipotese de jogadas sucessivas que implementa
;cortes Alfa e Beta, diminuindo o numero de nos a processar
(defun minimax-alfa-beta (problema inteiro)
	(let (	(accao nil)
			(valor nil)
			(aux-valor 0)
			(jogo-aux nil)
			(res nil)
			(nos 0)
			(alfa most-negative-fixnum)
			(beta most-positive-fixnum))
			(if(= (funcall (problema-jogador problema) (problema-estado-inicial problema)) inteiro)
				(setf valor most-negative-fixnum)
				(setf valor most-positive-fixnum)
				)
			(loop for a in (funcall (problema-accoes problema) (problema-estado-inicial problema)) do
				(setf jogo-aux (funcall (problema-resultado problema) (problema-estado-inicial problema) a))
				(if(= (funcall (problema-jogador problema) (problema-estado-inicial problema)) inteiro) 
					(progn																																	
						(if (= (funcall (problema-jogador problema) (problema-estado-inicial problema)) (funcall (problema-jogador problema) jogo-aux))   	
							(progn	(setf res (multiple-value-list (max-value-alfa-beta jogo-aux problema nos alfa beta)))
									(setf aux-valor (first res))
									(setf nos (second res))
									(if (>= aux-valor beta)
										(return)
										(setf alfa (max alfa aux-valor))
									)
							)
							(progn	(setf res (multiple-value-list (min-value-alfa-beta jogo-aux problema nos alfa beta)))
									(setf aux-valor (first res))
									(setf nos (second res))
									(if (>= aux-valor beta)
										(return)
										(setf alfa (max alfa aux-valor))
									)
							)										
						)
						(if (< valor aux-valor)					
								(progn 	(setf accao a)
										(setf valor aux-valor))
						)
					)
					(progn																																	
						(if (= (funcall (problema-jogador problema) (problema-estado-inicial problema)) (funcall (problema-jogador problema) jogo-aux))   	
							(progn	(setf res (multiple-value-list (min-value-alfa-beta jogo-aux problema nos alfa beta)))
									(setf aux-valor (first res))
									(setf nos (second res))
									(if (<= aux-valor alfa)
										(return)
										(setf beta (min beta aux-valor))
									)
							)
							(progn	(setf res (multiple-value-list (max-value-alfa-beta jogo-aux problema nos alfa beta)))
									(setf aux-valor (first res))
									(setf nos (second res))
									(if (<= aux-valor alfa)
										(return)
										(setf beta (min beta aux-valor))
									)
							)
						)
						(if (> valor aux-valor)					
								(progn 	(setf accao a)
										(setf valor aux-valor))
						)
					)
				)
			)
		(values accao valor nos)
	)
)

(defun max-value-alfa-beta (jogo problema nos a b)
	(let (	(v most-negative-fixnum)
			(jogo-aux nil)
			(res nil)
			(nosAux nos)
			(alfa a)
			(beta b))
		(if (funcall (problema-teste-corte-p problema) jogo 10)
			(progn
				(incf nosAux)
				(setf v (funcall (problema-funcao-avaliacao problema) jogo (funcall (problema-jogador problema) jogo)))	
			)
			(progn 	(loop for a in (funcall (problema-accoes problema) jogo) do
						(setf jogo-aux (funcall (problema-resultado problema) jogo a))
						(if (= (funcall (problema-jogador problema) jogo) (funcall (problema-jogador problema) jogo-aux))
							(progn	(setf res (multiple-value-list (max-value-alfa-beta jogo-aux problema nosAux alfa beta)))
									(setf v (max v (first res)))
									(setf nosAux (second res))
									(if (>= v beta)
										(return)
										(setf alfa (max alfa v))
									)
							)
							(progn	(setf res (multiple-value-list (min-value-alfa-beta jogo-aux problema nosAux alfa beta)))
									(setf v (max v (first res)))
									(setf nosAux (second res))
									(if (>= v beta)
										(return)
										(setf alfa (max alfa v))
									)
							)
						)
					)
			)
		)
		(values v nosAux)
	)
)
	
(defun min-value-alfa-beta (jogo problema nos a b)
	(let (	(v most-positive-fixnum)
			(jogo-aux nil)
			(res nil)
			(nosAux nos)
			(alfa a)
			(beta b))
		(if (funcall (problema-teste-corte-p problema) jogo 10)
			(progn
				(incf nosAux)
				(setf v (funcall (problema-funcao-avaliacao problema) jogo (+(rem (+ (funcall (problema-jogador problema) jogo) 2) 2)1)))
			)
			(progn 																		
					(loop for a in (funcall (problema-accoes problema) jogo) do
						(setf jogo-aux (funcall (problema-resultado problema) jogo a))
						(if (= (funcall (problema-jogador problema) jogo) (funcall (problema-jogador problema) jogo-aux))
							(progn	(setf res (multiple-value-list (min-value-alfa-beta jogo-aux problema nosAux alfa beta)))
									(setf v (min v (first res)))
									(setf nosAux (second res))
									(if (<= v alfa)
										(return)
										(setf beta (min beta v))
									)
							)
							(progn	(setf res (multiple-value-list (max-value-alfa-beta jogo-aux problema nosAux alfa beta)))
									(setf v (min v (first res)))
									(setf nosAux (second res))
									(if (<= v alfa)
										(return)
										(setf beta (min beta v))
									)
							)
						)
					)
			)
		)
		(values v nosAux)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MINIMAXES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Minimax V1 - Uma versao do Minimax para jogadores
(defun minimax-v1 (problema inteiro profundidade)
	(let* (	(accao nil)
			(valor nil)
			(aux-valor 0)
			(jogo-aux nil)
			(res nil)
			(nos 0)
			(alfa most-negative-fixnum)
			(beta most-positive-fixnum)
			(listaAccoes (funcall (problema-accoes problema) (problema-estado-inicial problema))))
			(if(= (funcall (problema-jogador problema) (problema-estado-inicial problema)) inteiro)
				(setf valor most-negative-fixnum)
				(setf valor most-positive-fixnum)
			)
			(loop for a in listaAccoes do
				(setf jogo-aux (funcall (problema-resultado problema) (problema-estado-inicial problema) a))
				(if(= (funcall (problema-jogador problema) (problema-estado-inicial problema)) inteiro) 
					(progn																																
						(if (= (funcall (problema-jogador problema) (problema-estado-inicial problema)) (funcall (problema-jogador problema) jogo-aux))   	
							(progn	(setf res (multiple-value-list (max-value-v1 jogo-aux problema nos alfa beta profundidade)))
									(setf aux-valor (first res))
									(setf nos (second res))
									(if (>= aux-valor beta)
										(return)
										(setf alfa (max alfa aux-valor))
									)
							)
							(progn	(setf res (multiple-value-list (min-value-v1 jogo-aux problema nos alfa beta profundidade)))
									(setf aux-valor (first res))
									(setf nos (second res))
									(if (>= aux-valor beta)
										(return)
										(setf alfa (max alfa aux-valor))
									)
							)										
						)
						(if (< valor aux-valor)					
								(progn 	(setf accao a)
										(setf valor aux-valor))
						)
					)
					(progn																																	
						(if (= (funcall (problema-jogador problema) (problema-estado-inicial problema)) (funcall (problema-jogador problema) jogo-aux))   	
							(progn	(setf res (multiple-value-list (min-value-v1 jogo-aux problema nos alfa beta profundidade)))
									(setf aux-valor (first res))
									(setf nos (second res))
									(if (<= aux-valor alfa)
										(return)
										(setf beta (min beta aux-valor))
									)
							)
							(progn	(setf res (multiple-value-list (max-value-v1 jogo-aux problema nos alfa beta profundidade)))
									(setf aux-valor (first res))
									(setf nos (second res))
									(if (<= aux-valor alfa)
										(return)
										(setf beta (min beta aux-valor))
									)
							)
						)
						(if (> valor aux-valor)					
								(progn 	(setf accao a)
										(setf valor aux-valor))
						)
					)
				)
			)
		(values accao valor nos)
	)
)

(defun max-value-v1 (jogo problema nos a b profundidade)
	(let (	(v most-negative-fixnum)
			(jogo-aux nil)
			(res nil)
			(nosAux nos)
			(alfa a)
			(beta b))
		(if (or (funcall (problema-teste-corte-p problema) jogo 10) (= profundidade 0))
			(progn
				(setf v (funcall (problema-funcao-avaliacao problema) jogo (funcall (problema-jogador problema) jogo)))
			)
			(progn 	(loop for a in (funcall (problema-accoes problema) jogo) do
						(setf jogo-aux (funcall (problema-resultado problema) jogo a))
						(if (= (funcall (problema-jogador problema) jogo) (funcall (problema-jogador problema) jogo-aux))
							(progn	(setf res (multiple-value-list (max-value-v1 jogo-aux problema nosAux alfa beta (- profundidade 1))))
									(setf v (max v (first res)))
									(setf nosAux (second res))
									(if (>= v beta)
										(return)
										(setf alfa (max alfa v))
									)
							)
							(progn	(setf res (multiple-value-list (min-value-v1 jogo-aux problema nosAux alfa beta (- profundidade 1))))
									(setf v (max v (first res)))
									(setf nosAux (second res))
									(if (>= v beta)
										(return)
										(setf alfa (max alfa v))
									)
							)
						)
					)
			)
		)
		(incf nosAux)
		(values v nosAux)
	)
)
	
(defun min-value-v1 (jogo problema nos a b profundidade)
	(let (	(v most-positive-fixnum)
			(jogo-aux nil)
			(res nil)
			(nosAux nos)
			(alfa a)
			(beta b))
		(if (or (funcall (problema-teste-corte-p problema) jogo 10) (= profundidade 0))
			(progn
				(setf v (funcall (problema-funcao-avaliacao problema) jogo (+(rem (+ (funcall (problema-jogador problema) jogo) 2) 2)1)))
			)
			(progn 																		
					(loop for a in (funcall (problema-accoes problema) jogo) do
						(setf jogo-aux (funcall (problema-resultado problema) jogo a))
						(if (= (funcall (problema-jogador problema) jogo) (funcall (problema-jogador problema) jogo-aux))
							(progn	(setf res (multiple-value-list (min-value-v1 jogo-aux problema nosAux alfa beta (- profundidade 1))))
									(setf v (min v (first res)))
									(setf nosAux (second res))
									(if (<= v alfa)
										(return)
										(setf beta (min beta v))
									)
							)
							(progn	(setf res (multiple-value-list (max-value-v1 jogo-aux problema nosAux alfa beta (- profundidade 1))))
									(setf v (min v (first res)))
									(setf nosAux (second res))
									(if (<= v alfa)
										(return)
										(setf beta (min beta v))
									)
							)
						)
					)
			)
		)
		(incf nosAux)
		(values v nosAux)
	)
)

;Jogador Minimax VBest - Versao melhorada do jogador que utiliza o procedimento Minimax V1 (chama o V1 de modo a evitar
;codigo repetido -- abstracao procedimental). Usa o tempo de processamento de cada no da arvore (ver calcTempoNos)
(let (	(TempoNos 0)
		(iniciado nil))
	(defun jogador-minimax-vbest (jogo jogador tempo)
		(let ((profundidade 0)
			  (problema (make-problema :estado-inicial jogo :jogador #'jogo-jogador :accoes #'accoes :resultado #'resultado :teste-corte-p #'teste-terminal-p :funcao-avaliacao #'fun-avaliacao-v2))
			  (ret nil)
			  (tempoAux 0))
			(setf TempoNos (calcTempoNos problema jogador))
			(setf profundidade (calcProfundidade (- tempo 1) (length (accoes jogo)) TempoNos))
			(if (not iniciado)
				(setf tempoAux (get-internal-real-time))
			)
			(setf ret (multiple-value-list (minimax-v1 problema jogador profundidade)))
			(if (not iniciado)
				(progn
					(setf tempoAux (/ (- (get-internal-real-time) tempoAux) internal-time-units-per-second))
					(setf TempoNos (/ tempoAux (third ret)))
					(setf iniciado T)
				)
			)
			(car ret)
		)
	)
)

;Funcao que calcula em tempo de execucao, para o problema e jogadores dados, o tempo que o computador que corre
;o programa demora a processar apenas um no (uma iteracao do minimax)
(defun calcTempoNos (problema jogador)
	(let ((tempoAux 0)
		  (ret nil)
		  (TempoNos 0))
			(setf tempoAux (get-internal-real-time))
			(setf ret (multiple-value-list (minimax-v1 problema jogador 1)))
			(progn
				(setf tempoAux (/ (- (get-internal-real-time) tempoAux) internal-time-units-per-second))
				(setf TempoNos (/ tempoAux (third ret)))
			)
		TempoNos
	)
)

;Funcao Avaliacao V1 - A mais simples possivel -- usa apenas a utilidade para todos os casos
(defun fun-avaliacao-v1 (jogo jogador)
		(utilidade jogo jogador)
)

;Funcao Avaliacao V2 - Se jogo terminado, usa a utilidade (pontuacao). Caso contrario, obtem o tabuleiro
;do jogo e acumula os pontos das moedas que podem ser ganhas em sequencia a partir da proxima jogada.
(defun fun-avaliacao-v2 (jogo jogador)
	(let ((tab nil)
		 (pontos 0)
		 (moedas nil)
		 (valor nil))
		(if	(jogo-terminado-p jogo)
			(utilidade jogo jogador)
			(progn 	(setf tab (jogo-tabuleiro jogo))
					(dotimes (coluna (tabuleiro-colunas tab))
						(dotimes (linha (tabuleiro-linhas tab))
							(setf valor (tabuleiro-moeda-posicao tab (cria-posicao linha coluna)))
							(if (not (null valor))
								(cons moedas (cria-moeda valor (cria-posicao linha coluna)))
							)	
						)
					)
					(loop for m in moedas do
						(if (= 1 (length (tabuleiro-fios-posicao tab (moeda-posicao m))))
							(setf pontos (+ pontos (moeda-valor m)))
						)
					)
					(+ (utilidade jogo jogador) pontos)
			)
		)
	)
)

;Calcula Profundidade
(defun calcProfundidade (tempo ramificacao TempoPorNo)
	(let	((reducao (- 1 0.7)))
			(if (= tempo 0)
				0
				(if (= tempo 1)
					(- (car (multiple-value-list (floor (- (/ (log (+ (* (/ (/ tempo reducao) TempoPorNo) (- ramificacao 1)) 1)) (log ramificacao)) 1)))) 2)
					(if (= ramificacao 1)
						1
						(- (car (multiple-value-list (floor (- (/ (log (+ (* (/ (/ tempo reducao) TempoPorNo) (- ramificacao 1)) 1)) (log ramificacao)) 1)))) 1)
					)
				)
			)
	)
)

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LOAD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load (compile-file "src/iaproj/interface-moedas.lisp"))
;(load (compile-file "src/iaproj/exemplos.lisp"))
(load "interface-moedas.fas")
(load "exemplos.fas")
