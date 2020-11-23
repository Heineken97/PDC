#lang racket
(require racket/gui racket/include)
(include "GUI/PDC-gui.rkt")
;PDC-Sol
(define (PDC-Sol size init)
  (cond((validInput? size init)
       (PDC-Sol-Aux (car init) (cadr init) (buildMatrix size -1) '() 0))
  (else(fprintf (current-output-port) "Invalid Input"))))

;PDC-Sol-Aux, Funcion Auxiliar de PDC-Sol
;Cuando trata de aplicar siguienteMovimiento y es nulo, hace append de los pasos recolectados
;Recursivamente se llama asi misma
;InitX = el primer elemento de la funcion nexTMove
;initY = los demas elementos de la funcion nextMove
;buildedMatrix = hace check del movimiento----
;steps = append de los pasos -----
;count = aumenta en 1
(define (PDC-Sol-Aux initX initY buildedMatrix steps count)
  (cond((null? (nextMove initX initY buildedMatrix))
        (append steps (list(list initX initY))))
  (else(PDC-Sol-Aux (car (nextMove initX initY buildedMatrix)) (cadr (nextMove initX initY buildedMatrix)) (mark count initX initY buildedMatrix) (append steps(list(list initX initY))) (+ count 1)))))

;buildMatrix, Construye matriz cuadrada, se inician las posiciones con el valor dado
;llama a su funcion auxiliar usando value como la llamada recursiva se esa misma funcion auxiliar
(define (buildMatrix size value)
  (buildMatrix-Aux size (buildMatrix-Aux size value 0) 0))

;buildMatrix-Aux, Conteo es igual al tamanio retorna lista vacia, sino, crea constante llamandose recursivamente asi misma
(define (buildMatrix-Aux size value count)
  (cond((= count size)
        '())
  (else(cons value (buildMatrix-Aux size value (+ count 1))))))

;mark,Asigna un valor a un elemento de matriz en su dada posición, siendo 1 la primera posición
(define (mark value x y matrix)
  (mark_aux
   (mark_aux value y (get x matrix) 1)
   x
   matrix 1))
;recursiva de la función mark
(define (mark_aux value pos _list temp_pos)
  (cond
    ((null? _list) (list))
    ((equal? temp_pos pos)
     (cons value (mark_aux value pos (cdr _list) (+ temp_pos 1))))
    (else
     (cons (car _list) (mark_aux value pos (cdr _list) (+ temp_pos 1))))
    )
  )
;Determina siguiente movimiento a partir de una posición, utilizando la eurística de Warndorff
(define (nextMove x y matrix)
  (nextMove_aux x y matrix (list) 9 (list '( 1 2)  '( 1 -2) '( 2 1)  '( 2 -1) '(-1 2)  '(-1 -2) '(-2 1)  '(-2 -1))))
;recursiva de nextMove
(define (nextMove_aux x y matrix index minDegree moves)
  (cond((null? moves) index)
       ((and(isEmpty? (+ x (caar moves)) (+ y (cadar moves)) matrix)
            (< (getDegree (+ x (caar moves)) (+ y (cadar moves)) matrix) minDegree))
        (nextMove_aux x y matrix(list (+ x (caar moves)) (+ y (cadar moves))) (getDegree (+ x (caar moves)) (+ y (cadar moves)) matrix) (cdr moves)))
       ((and(isEmpty? (+ x (caar moves)) (+ y (cadar moves)) matrix)
            (equal? (getDegree (+ x (caar moves)) (+ y (cadar moves)) matrix) minDegree))
        (nextMove_aux x y matrix(tieBreaker (list (+ x (caar moves)) (+ y (cadar moves))) index matrix) minDegree(cdr moves)))
       (else (nextMove_aux x y matrix index minDegree (cdr moves)))))
;isEmpty?, Función booleana, comprueba si posición de la matriz ya ha sido visitada,
; por defecto un no visitado es -1
(define (isEmpty? x y matrix)
  (and (in_bounds? x y (length matrix))
       (= -1 (get y (get x matrix)))))
;in_bounds?, booleana comprueba si la posición se encuentra dentro de una matriz de tamaño ya dado
(define (in_bounds? x y size)
  (and (and (> x 0)
            (> y 0))
       (and (<= x size )
            (<= y size ))))

;get, Obtener elemento de una lista dada su posición
;usa 1 como posicion inicial
(define (get pos list)
  (get-Aux pos list 1))

;get-aux,función recursiva de la función get
(define (get-Aux pos list pivot-pos)
  (cond 
    ((equal? pivot-pos pos) (car list))
  (else (get-Aux pos (cdr list) (+ pivot-pos 1)))))

;tiebreaker, decide entre dos puntos cual es el siguiente, se basa en el que se encuentre más lejos del centro de la matriz
(define (tieBreaker index1 index2 matrix)
  (cond
    ((>=(distance index1 (middle matrix))(distance index2 (middle matrix)))
     index1)
  (else index2)))

;distancia, determina la distancia euclidiana entre dos puntos dados
(define (distance point1 point2)
  (sqrt(+(expt (- (car point1) (car point2)) 2)
         (expt (- (cadr point1) (cadr point2)) 2))))

;middle, determina la posición central de una matriz, usa redondeo hacia arriba
(define (middle matrix)
  (list (ceiling (/(length matrix) 2))
        (ceiling (/ (length matrix) 2))))

;getDegree, Determina la cantidad de movimientos validosa partir de una posición en una matriz|#
(define (getDegree x y matrix)
  (getDegree-Aux x y matrix (list '( 1 2) '( 1 -2)'( 2 1) '( 2 -1) '(-1 2) '(-1 -2)'(-2 1) '(-2 -1)) 0))

;getDegree-Aux, recursiva de la función getDegree |#
(define (getDegree-Aux x y matrix moves count)
  (cond((null? moves) count)
       ((isEmpty? (+ x (caar moves)) (+ y (cadar moves)) matrix)
        (getDegree-Aux x y matrix (cdr moves) (+ count 1)))
  (else
   (getDegree-Aux x y matrix (cdr moves) count))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PDC-Todas
(define (PDC-Todas size position)
  (cond((validInput? size position)
        (getSteps(getMoves position size '() ) size (list position) (current-seconds)))
  (else(
        (current-output-port)
        "Valor de entrada inválido"))))
;getSteps, hace el backtrack para resolver el problema
(define(getSteps moves size paths time)
  (cond
    [(> (- (current-seconds) time) 60) (list)]
    [(and (empty? moves) (= (length  paths) (* size size))) (list paths)]
    [(empty? moves) '()]
    [(and (empty? (getMoves (car moves) size paths)) (= (length paths) (- (* size size) 1))) (getSteps (cdr moves) size (append paths (list(car moves))) time)]
    [(empty? (getMoves (car moves) size paths)) (getSteps (cdr moves) size paths time)]
    [else (append (getSteps (getMoves (car moves) size (cons (car moves) paths)) size (append paths (list (car moves))) time)
                  (getSteps (cdr moves) size paths time))]))


;getMoves, Crea una lista coordenadas de los movimientos considerando el scope y los recorridos|
(define (getMoves position size paths)
  (deletePaths paths (moveScope size (buildListMoves (movUp_Right (car position) (cadr position))
                                                     (movUp_Left (car position) (cadr position))
                                                     (movDown_Right (car position) (cadr position))
                                                     (movDown_Left (car position) (cadr position))
                                                     (movRight_Top (car position) (cadr position))
                                                     (movRight_Bott(car position) (cadr position))
                                                     (movLeft_Top (car position) (cadr position))
                                                     (movLeft_Bott (car position) (cadr position))))))
;deletePaths, elimina las coordenadas ya recorridas de sus posibles movimiento
(define (deletePaths paths moves)
  (cond
    [(empty? moves) '()]
    [(lookingPaths (car moves) paths ) (deletePaths paths (cdr moves))]
    [else (cons (car moves) (deletePaths paths (cdr moves)))]))

;lookingPaths, verifica si la coordenada se encuentra entre los recorridos(paths)
(define (lookingPaths position paths)
  (cond
    [(= (length paths) 0) #f]
    [(equal? (car paths) position) #t]
    [else (lookingPaths position ( cdr paths ))]))

;moveScope,  Crea lista con los movimientos que estan dentro del tablero (considerando unicamente el scope)
(define (moveScope size moves)
  (cond
    [(empty? moves) '()]
    [(movInside? (car moves) size ) (cons (car moves) (moveScope size (cdr moves)))]
    [else (moveScope size (cdr moves))]))

;movInside?, booleana verifica si esta dentro o no del tablero
(define (movInside? position size)
  (cond
    [(and (and (<= (car position) size)  (<= (cadr position) size))  (and (> (car position) 0) (> (cadr position) 0))) #t]
    [else #f]))

;buildListMoves, crea lista con todas las coordenadas de movimientos posibles no considera el scope ni los movimientos recorridos.
(define (buildListMoves UR UL DR DL RT RB LT LB)
  (list UR UL DR DL RT RB LT LB))

;movUp_Right, Obtiene la coordenada de mover arriba y a la derecha
(define (movUp_Right row column)
  (list (- row 2) (+ column 1)))
;movUp_Left, Obtiene la coordenada de mover arriba y a la izquierda
(define (movUp_Left row column)
  (list (- row 2) (- column 1)))
;movDown_Right, Obtiene la coordenada de mover abajo y a la derecha
(define (movDown_Right row column)
  (list (+ row 2) (+ column 1)))
;movDown_Left, Obtiene la coordenada de mover abajo y a la izquierda
(define (movDown_Left row column)
  (list (+ row 2) (- column 1)))
;movRight_Top, Obtiene la coordenada de mover derecha y a arriba
(define (movRight_Top row column)
  (list (- row 1) (+ column 2)))
;movRight_Bott, Obtiene la coordenada de mover arriba y a la derecha
(define (movRight_Bott row column)
  (list (+ row 1) (+ column 2)))
;movLeft_Top Obtiene la coordenada de mover izquierda y a arriba
(define (movLeft_Top row column)
  (list (- row 1) (- column 2)))
;movLeft_Bott, Obtiene la coordenada de mover izquierda y a abajo|#
(define (movLeft_Bott row column)
  (list (+ row 1) (- column 2)))       