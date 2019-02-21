;;; NORMAL
;esta función procesa vectores con el mismo número de dimensiones.
(defun sum-rec(x y)
    (if (null x)
      0.0  
        (+ (* (first x) (first y) ) (sum-rec (rest x) (rest y) ) ) ) 
)

(defun cosine-distance-rec(x y)
    (setq arriba (sum-rec x y))
    (- 1 (/ (parte-arriba-rec x y) (* (sqrt (parte-arriba-rec x x)) (sqrt (parte-arriba-rec y y))))))

;;; MAPCAR
(defun sum-mapcar(x y)
    (if (null x) 
        0.0
        (reduce #'+ (mapcar #'* x y) ) )
)

(defun cosine-distance-mapcar(x y)
    - 1 / parte-arriba-rec(x y) * sqrt(parte-abajo-rec(x)) sqrt(parte-abajo-rec(y))
)

; mapcar #' (lambda(x) (* x x) lst)
;(cosine-distance-rec '(1 2) '(1 2 3)) => 0.40238577 (No estamos seguros de la coherencia de este resultado pues el número de dimensiones de los vectores es diferente)
;(cosine-distance-rec nil '(1 2 3)) => No se puede calcular esta operación dado que el 1º vector es vacío
;(cosine-distance-rec ’() ’()) => Dadas las listas vacías es imposible de calcular con exactitud este valor.
;(cosine-distance ’(0 0) ’(0 0)) => matemáticamente siguiendo la fórmula obtendríamos que la fórmula acabaría en el calculo de 1 - 0/0, dado que sabemos que el ángulo 
;que se forma entre dos vectores que son iguales es 0, y que cos(0º) = 1, la distancia coseno sería de 1-1 = 0, no sabemos si tenemos que demostrarlo programándolo o 
;si por conocimientos básicos de matemáticas deberíamos de llegar a esta conclusión en el cálculo. 

(defun order-vectors-cosine-distance(vector lst-of-vectors &optional (confidence-level 0))
    (if (null lst-of-vectors)
        nil
        (if (> (cosine-distance-rec vector (first lst-of-vectors)) confidence-level )
            (cons (first lst-of-vectors) (orders-vectors-cosine-distance (rest lst-of-vectors) ) )
        (orders-vectors-cosine-distance (rest lst-of-vectors))
        )
    )
)
