;Crea una nueva ventana
(define frame (new frame%
                   [label "Knight's Problem"]
                   [stretchable-height #f]
                   [stretchable-width #f]))

;Crea un nuevo dialogo de error
(define error-dialog (instantiate dialog%("Solución inválida")))

;Crea un panel vertical dentro del dialogo de error
(define panel (new horizontal-panel% [parent error-dialog]
                   [alignment '(center center)]
                   [vert-margin 50]
                   [horiz-margin 40]
                   [spacing 30]))

; Hereda la clase canvas y anade handlers para el teclado y mouse
(define my-canvas%
  (class canvas% ; The base class is canvas%
    (define/override (on-char event)(cond
                                      ((equal? (send event get-key-code) 'release)(repaint (send this get-dc)))))
    (define/public (load)
      (repaint (send this get-dc)))
    (super-new)
    ))

;Crea el canvas
(define (canvas cells)(new my-canvas%
                           [parent frame]
                           [min-width (* 75 cells)]
                           [min-height (* 75 cells)]))
;Instancia del canvas
(define canvas-instance null)

;Funcion principal, inicia la solucion
(define (PDC-Paint cells sol)
  (cond
    ((validInput2? cells sol)
     (set! path (cadr (solution? sol cells)))
     (set! size cells)
     (send frame show #t)
     (cond
       ((null? canvas-instance) (set! canvas-instance (canvas cells)))
       (else
        (send canvas-instance min-height (* 75 cells))
        (send canvas-instance min-width (* 75 cells))
        (send visual-path reset)))
     (start-path (caar path) (cadar path))
     (sleep/yield 0.5)
     (send canvas-instance load)
     (cond
       ((equal? (car (solution? sol cells)) #f)
        (new message%
             [parent panel]
             [label alert-icon])
        (new message%
             [parent panel]
             [label "No se ha ingresado una solución válida. Sin embargo, se mostrará el recorrido hasta el último movimiento válido"]
             [min-height 25]
             [min-width 610])
        (send error-dialog show #t))))
    (else (fprintf (current-output-port)
                          "Valor de entrada inválido")))
  )

;Construye el tablero
(define (board dc size hpos vpos)(cond
                                   ((equal? size vpos) #t)
                                   ((equal? size hpos) (board dc size 0 (+ vpos 1)))
                                   (else
                                    (cond
                                      ((odd? (+ hpos vpos))
                                       (send dc set-brush "sky blue" 'solid))
                                      ((even? (+ hpos vpos))
                                       (send dc set-brush "white" 'solid)))
                                    (send dc draw-rectangle (* 75 hpos) (* 75 vpos) 75 75)
                                    (board dc size (+ hpos 1) vpos))))


;Icono del caballo
(define horse-icon (read-bitmap "GUI/horse.png"))

;Icono de alerta
(define alert-icon (read-bitmap "GUI/alert.png"))

;Actualiza la posicion del caballo
(define (update dc hpos vpos)
  (send dc draw-bitmap horse-icon (* 75 (- hpos 1)) (* 75 (- vpos 1))))

;Limpia el canvas
(define (clear-canvas dc)
  (send dc clear))

;Path a seguir
(define path null)

;Tamano del tablero
(define size null)

;Actualiza la imagen
(define (repaint dc)(cond
                      ((null? path) (set! canvas-instance null))
                      (else 
                       (send dc clear)
                       (send dc set-pen "white" 1 'transparent)
                       (board dc size 0 0)
                       (add-to-path (caar path) (cadar path))
                       (send dc set-pen "black" 10 'solid)
                       (send dc set-brush "white" 'transparent)
                       (send dc draw-path visual-path)
                       (update dc (caar path) (cadar path))
                       (set! path (cdr path)))))

;visual-path =m Crea la linea del path
(define visual-path (new dc-path%))

;start-path, Define la casilla de inicio
(define (start-path hpos vpos)(send visual-path move-to (+ 50 (* 75 (- hpos 1))) (+ 50 (* 75 (- vpos 1)))))

;Add-to-path, anade un movimiento a la linea path
(define (add-to-path hpos vpos)
  (send visual-path line-to (+ 50 (* 75 (- hpos 1)))(+ 50 (* 75 (- vpos 1)))))

;contains, determinar si el elemento pertenece a la lista
(define (contains? _list element)
  (cond
    ((null? _list) #f)
    ((equal? (car _list) element) #t)
    (else (contains? (cdr _list) element))
    )
  )

;adjacent, determinar si el movimiento del caballo es permito
(define (adjacent point1 point2)
  (equal? (distance point1 point2)
          (sqrt 5)))

;solution? Comnpruebar validez de solución, sino lo es, retorna hasta el último movimiento correcto
(define (solution? solution size)
  (solution_aux solution size '()))

;solucion_aux, recursiva de solution
(define (solution_aux solution size visited)
  (cond
    ((null? solution)
     (list #f solution))
    ((null? (cdr solution))
     (cond
       ((and (in_bounds? (caar solution) (cadar solution) size)
             (equal? (+ (length visited) 1) (expt size 2)))
        (list #t (append visited (list (car solution)))))
     (else (list #f visited))))
    ((and (and (in_bounds? (caar solution) (cadar solution) size)
               (adjacent (car solution) (cadr solution)))
          (not (contains? visited (car solution))))
     (solution_aux (cdr solution) size (append visited (list (car solution)))))
    (else (list #f visited))))
;validInput?, valida entrada de funciones
(define (validInput? size start)
    (and
      (and (integer? size) (positive? size))
      (and (and (integer? (car start)) (integer? (cadr start)))
           (in_bounds? (car start) (cadr start) size))))
;validInput2?, valida posibles elementos solucion en PDC Paint y Test
(define (validInput2? size solution)
  (cond
    ((not (list? solution)) #f)
    ((null? solution) #t)
    (else (and (validInput? size (car solution))
               (validInput2? size (cdr solution))))))