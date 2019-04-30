(use-package 'conecta4)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;; -------------------------------------------------------------------------------
;; Funciones de evaluación
;; -------------------------------------------------------------------------------

(defun f-eval-bueno (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual)))
    (if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
	  (cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
		(t +val-min+)))
      (let ((puntuacion-actual 0)
	    (puntuacion-oponente 0))
	(loop for columna from 0 below (tablero-ancho tablero) do

	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;; sensores cardinales del jugador.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		     (abajo (contar-abajo tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-actual columna fila))
		     (izq (contar-izquierda tablero ficha-actual columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))

      ;; caso menor que 12 (heuristica ponderada para apertura de partida)
       (if (< (contar-fichas tablero) 12)
       (setf puntuacion-actual
		      (+ puntuacion-actual
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 100)
			       ((= abajo 3) 1000000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
             ((= der 3) 1000000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 1000000))
       ;; contemplamos las diagonales
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000000))
       (cond ((= abajo-der 0) 0)
			       ((= abajo-der 1) 10)
			       ((= abajo-der 2) 100)
			       ((= abajo-der 3) 1000000))
       (cond ((= arriba-izq 0) 0)
			       ((= arriba-izq 1) 10)
			       ((= arriba-izq 2) 100)
			       ((= arriba-izq 3) 1000000))
       (cond ((= arriba-der 0) 0)
			       ((= arriba-der 1) 10)
			       ((= arriba-der 2) 100)
			       ((= arriba-der 3) 1000000))
           ))
           ;; segunda parte de la partida evaluando heuristicas sin ponderar, una vez la apertura termina
           (setf puntuacion-actual
    		      (+ puntuacion-actual
    			 (cond ((= abajo 0) 0)
    			       ((= abajo 1) 10)
    			       ((= abajo 2) 100)
    			       ((= abajo 3) 1000000))
    			 (cond ((= der 0) 0)
    			       ((= der 1) 10)
    			       ((= der 2) 100)
                 ((= der 3) 1000000))
    			 (cond ((= izq 0) 0)
    			       ((= izq 1) 10)
    			       ((= izq 2) 100)
    			       ((= izq 3) 1000000))
           ;; contemplamos las diagonales
    			 (cond ((= abajo-izq 0) 0)
    			       ((= abajo-izq 1) 10)
    			       ((= abajo-izq 2) 100)
    			       ((= abajo-izq 3) 1000000))
           (cond ((= abajo-der 0) 0)
    			       ((= abajo-der 1) 10)
    			       ((= abajo-der 2) 100)
    			       ((= abajo-der 3) 1000000))
           (cond ((= arriba-izq 0) 0)
    			       ((= arriba-izq 1) 10)
    			       ((= arriba-izq 2) 100)
    			       ((= arriba-izq 3) 1000000))
           (cond ((= arriba-der 0) 0)
    			       ((= arriba-der 1) 10)
    			       ((= arriba-der 2) 100)
    			       ((= arriba-der 3) 1000000))
               )))
            ) ;; let-altura

	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;; sensores cardinales del jugador oponente. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		     (abajo (contar-abajo tablero ficha-oponente columna fila))
		     (der (contar-derecha tablero ficha-oponente columna fila))
		     (izq (contar-izquierda tablero ficha-oponente columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))

    (if (< (contar-fichas tablero) 12)
    ;; caso menor que 12 (heuristica ponderada para apertura de partida)
		(setf puntuacion-oponente
		      (+ puntuacion-oponente
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 100)
			       ((= abajo 3) 5000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
			       ((= der 3) 5000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 5000))
       ;; contamplamos diagonales
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 5000))
       (cond ((= abajo-der 0) 0)
			       ((= abajo-der 1) 10)
			       ((= abajo-der 2) 100)
			       ((= abajo-der 3) 5000))
       (cond ((= arriba-izq 0) 0)
			       ((= arriba-izq 1) 10)
			       ((= arriba-izq 2) 100)
			       ((= arriba-izq 3) 5000))
       (cond ((= arriba-der 0) 0)
			       ((= arriba-der 1) 10)
			       ((= arriba-der 2) 100)
			       ((= arriba-der 3) 5000))
           ))
      ;; end-primer-if
      ;; segunda parte de la partida evaluando heuristicas sin ponderar, una vez la apertura termina
       (setf puntuacion-oponente
   		      (+ puntuacion-oponente
   			 (cond ((= abajo 0) 0)
   			       ((= abajo 1) 10)
   			       ((= abajo 2) 100)
   			       ((= abajo 3) 5000))
   			 (cond ((= der 0) 0)
   			       ((= der 1) 10)
   			       ((= der 2) 100)
   			       ((= der 3) 5000))
   			 (cond ((= izq 0) 0)
   			       ((= izq 1) 10)
   			       ((= izq 2) 100)
   			       ((= izq 3) 5000))
          ;; contamplamos diagonales
   			 (cond ((= abajo-izq 0) 0)
   			       ((= abajo-izq 1) 10)
   			       ((= abajo-izq 2) 100)
   			       ((= abajo-izq 3) 5000))
          (cond ((= abajo-der 0) 0)
   			       ((= abajo-der 1) 10)
   			       ((= abajo-der 2) 100)
   			       ((= abajo-der 3) 5000))
          (cond ((= arriba-izq 0) 0)
   			       ((= arriba-izq 1) 10)
   			       ((= arriba-izq 2) 100)
   			       ((= arriba-izq 3) 5000))
          (cond ((= arriba-der 0) 0)
   			       ((= arriba-der 1) 10)
   			       ((= arriba-der 2) 100)
   			       ((= arriba-der 3) 5000))
              ))))
         ); let-loop-for
	(- puntuacion-actual puntuacion-oponente)))))


(defun contar-fichas (tablero)
 (let ((contador 0))
   (loop for fila from 0 below (tablero-alto tablero) do
     (loop for columna from 0 below (tablero-ancho tablero) do
       (if (not (null (aref (tablero-casillas tablero) fila columna)))
         (setf contador (+ 1 contador))
       )
     )
   )
   contador
 )
)

;; -------------------------------------------------------------------------------
;; Jugadores
;; -------------------------------------------------------------------------------

(defvar *jugador-aleatorio* (make-jugador :nombre 'Jugador-aleatorio
					  :f-jugador #'f-jugador-aleatorio
					  :f-eval  #'f-eval-aleatoria))

(defvar *jugador-bueno* (make-jugador :nombre 'Jugador-bueno
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'f-eval-bueno))

(defvar *jugador-humano* (make-jugador :nombre 'Jugador-humano
				       :f-jugador #'f-jugador-humano
				       :f-eval  #'f-no-eval))


;; -------------------------------------------------------------------------------
;; Algunas partidas de ejemplo:
;; -------------------------------------------------------------------------------

(setf *verbose* t)

;(print (partida *jugador-aleatorio* *jugador-aleatorio*))
;(print (partida *jugador-aleatorio* *jugador-bueno* 4))
;(print (partida *jugador-bueno* *jugador-aleatorio* 4))
;(print (partida *jugador-bueno* *jugador-bueno* 4))
;(print (partida *jugador-humano* *jugador-humano*))
;(print (partida *jugador-humano* *jugador-aleatorio* 4))
(print (partida *jugador-humano* *jugador-bueno* 4))
;(print (partida *jugador-aleatorio* *jugador-humano*))
