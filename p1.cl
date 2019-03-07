
; mapcar #' (lambda(x) (* x x) lst)
;(cosine-distance-rec '(1 2) '(1 2 3)) => 0.40238577 (No estamos seguros de la coherencia de este resultado pues el número de dimensiones de los vectores es diferente)
;(cosine-distance-rec nil '(1 2 3)) => No se puede calcular esta operación dado que el 1º vector es vacío
;(cosine-distance-rec ’() ’()) => Dadas las listas vacías es imposible de calcular con exactitud este valor.
;(cosine-distance ’(0 0) ’(0 0)) => matemáticamente siguiendo la fórmula obtendríamos que la fórmula acabaría en el calculo de 1 - 0/0, dado que sabemos que el ángulo
;que se forma entre dos vectores que son iguales es 0, y que cos(0º) = 1, la distancia coseno sería de 1-1 = 0, no sabemos si tenemos que demostrarlo programándolo o
;si por conocimientos básicos de matemáticas deberíamos de llegar a esta conclusión en el cálculo.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;esta función procesa vectores con el mismo número de dimensiones.
(defun parte-arriba-rec(x y)
    (if (null x)
      0.0
        (+ (* (first x) (first y) ) (parte-arriba-rec(rest x) (rest y) ) ) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-rec (x y)
;;; Calcula la distancia coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-rec (x y)
    (setq arriba (parte-arriba-rec x y))
    (- 1 (/ (parte-arriba-rec x y) (* (sqrt (parte-arriba-rec x x)) (sqrt (parte-arriba-rec y y)))))
)

;;; MAPCAR
(defun sum_mapcar(x y)
    (if (null x)
        0.0
        (reduce #'+ (mapcar #'* x y) ) )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-mapcar
;;; Calcula la distancia coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-mapcar (x y)
     - 1 / sum_mapcar(x y) * sqrt(sum_mapcar(x x)) sqrt(sum_mapcar(y y))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lst-of-vectors vector de vectores
;;;         confidence-level: Nivel de confianza (parametro opcional)
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;         ordenados
;;;
(defun order-vectors-cosine-distance (vector lst-of-vectors &optional (confidence-level 0))
(if (null lst-of-vectors)
        nil
        (if (> (cosine-distance-rec vector (first lst-of-vectors)) confidence-level )
            (cons (first lst-of-vectors) (orders-vectors-cosine-distance (rest lst-of-vectors) ) )
        (orders-vectors-cosine-distance (rest lst-of-vectors))
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-vectors-category (categories vectors distance-measure)
;;; Clasifica a los textos en categorias .
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         texts:      vector de vectores, representado como
;;;                     una lista de listas
;;;         distance-measure: funcion de distancia
;;; OUTPUT: Pares formados por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia
;;;
( defun get-vectors-category (categories texts distance-measure)
    (if (null texts)
        nil
        (cons (cons (first (first texts))(funcall distance-measure (rest(first categories)) (rest (first texts)))) (get-vectors-category (rest categories) (rest texts) distance-measure))
    )
)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun calcular (f df x0)
  (- x0 (/(funcall f x0)(funcall df x0)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton
;;; Estima el cero de una funcion mediante Newton-Raphson
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;;         df: derivada de f
;;;         max-iter: maximo numero de iteraciones
;;;         x0: estimacion inicial del cero (semilla)
;;;         tol: tolerancia para convergencia (parametro opcional)
;;; OUTPUT: estimacion del cero de f o NIL si no converge
;;;
( defun newton (f df max-iter x0 &optional (tol 0.001))
  (let* ((xact (calcular f df x0)) (err (abs (- xact x0))))
    (if (= max-iter 0)
      NIL
      (if(< err tol)
        xact
        (newton f df (- max-iter 1) xact tol)
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-root-newton
;;; Prueba con distintas semillas iniciales hasta que Newton
;;; converge
;;;
;;; INPUT: f : funcion de la que se desea encontrar un cero
;;;        df : derivada de f
;;;        max-iter : maximo numero de iteraciones
;;;        semillas : semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: el primer cero de f que se encuentre , o NIL si se diverge
;;;          para todas las semillas
;;;
(defun one-root-newton (f df max-iter semillas &optional (tol 0.001))
  (if (null semillas)
    NIL
    (if(eql (newton f df (first semillas) tol) NIL)
      (one-root-newton f df (rest semillas) tol)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla o nil
;;;          si para esa semilla el metodo no converge
;;;
(defun all-roots-newton (f df max-iter semillas &optional ( tol 0.001))
   (if (null semillas)
    nil
      (cons (newton f df max-iter (first semillas) tol) (all-roots-newton f df max-iter semillas &optional tol ) )
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst    ----> (combine-elt-lst 'a '(1 2 3)) -> ((A 1) (A 2) (A 3))
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun combine-elt-lst (elt lst)
  (if (null lst)
    nil
    (cons (cons elt (first lst)) (combine-elt-lst elt (rest lst) )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst ----> (combine-lst-lst'(a b c)'(1 2)) ;; --> ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas
(defun combine-lst-lst (lst1 lst2)
  (if (null lst1)
    nil
    (append (combine-elt-lst (first lst1 ) lst2) (combine-lst-lst (rest lst1) lst2)   )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts
;;(combine-list-of-lsts'((a b c) (+ -) (1 2 3 4)))
;; --> ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;;      (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;;      (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))


;;; Calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion
;;; aparezca unicamente un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos
(defun combine-list-of-lsts (lstolsts)
  (if (null lstolsts)
    nil
    (if (null (first lstolsts))
     ; (combine-list-of-lsts (rest lstolsts) )
      ;(combine-elt-lst (first (first lstolsts))

    ; estoy pensandoooooooo no rayarse sin terminar


    (;cons (combine-lst-lst (first lstolsts) ) )
  )
)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; defino operadores logicos
(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '!)

;; definiciones de valores de verdad, conectores y atomos
(defun truth-value-p (x)
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x)
  (eql x +not+))

(defun binary-connector-p (x)
  (or (eql x +bicond+)
      (eql x +cond+)))

(defun n-ary-connector-p (x)
  (or (eql x +and+)
      (eql x +or+)))

(defun bicond-connector-p (x)
  (eql x +bicond+))

(defun cond-connector-p (x)
    (eql x +cond+))

(defun connector-p (x)
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p  x)))

(defun positive-literal-p (x)
  (and (atom x)
       (not (truth-value-p x))
       (not (connector-p x))))

(defun negative-literal-p (x)
  (and (listp x)
       (eql +not+ (first x))
       (null (rest (rest x)))
       (positive-literal-p (second x))))

(defun literal-p (x)
  (or (positive-literal-p x)
      (negative-literal-p x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; truth-tree
;;; Recibe una expresion y construye su arbol de verdad para
;;; determinar si es SAT o UNSAT
;;;
; mapcar #' (lambda(x) (* x x) lst)
;(cosine-distance-rec '(1 2) '(1 2 3)) => 0.40238577 (No estamos seguros de la coherencia de este resultado pues el número de dimensiones de los vectores es diferente)
;(cosine-distance-rec nil '(1 2 3)) => No se puede calcular esta operación dado que el 1º vector es vacío
;(cosine-distance-rec ’() ’()) => Dadas las listas vacías es imposible de calcular con exactitud este valor.
;(cosine-distance ’(0 0) ’(0 0)) => matemáticamente siguiendo la fórmula obtendríamos que la fórmula acabaría en el calculo de 1 - 0/0, dado que sabemos que el ángulo
;que se forma entre dos vectores que son iguales es 0, y que cos(0º) = 1, la distancia coseno sería de 1-1 = 0, no sabemos si tenemos que demostrarlo programándolo o
;si por conocimientos básicos de matemáticas deberíamos de llegar a esta conclusión en el cálculo.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;esta función procesa vectores con el mismo número de dimensiones.
(defun parte-arriba-rec(x y)
    (if (null x)
      0.0
        (+ (* (first x) (first y) ) (parte-arriba-rec(rest x) (rest y) ) ) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-rec (x y)
;;; Calcula la distancia coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-rec (x y)
    (setq arriba (parte-arriba-rec x y))
    (- 1 (/ (parte-arriba-rec x y) (* (sqrt (parte-arriba-rec x x)) (sqrt (parte-arriba-rec y y)))))
)

;;; MAPCAR
(defun sum_mapcar(x y)
    (if (null x)
        0.0
        (reduce #'+ (mapcar #'* x y) ) )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-mapcar
;;; Calcula la distancia coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-mapcar (x y)
     - 1 / sum_mapcar(x y) * sqrt(sum_mapcar(x x)) sqrt(sum_mapcar(y y))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lst-of-vectors vector de vectores
;;;         confidence-level: Nivel de confianza (parametro opcional)
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;         ordenados
;;;
(defun order-vectors-cosine-distance (vector lst-of-vectors &optional (confidence-level 0))
(if (null lst-of-vectors)
        nil
        (if (> (cosine-distance-rec vector (first lst-of-vectors)) confidence-level )
            (cons (first lst-of-vectors) (orders-vectors-cosine-distance (rest lst-of-vectors) ) )
        (orders-vectors-cosine-distance (rest lst-of-vectors))
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-vectors-category (categories vectors distance-measure)
;;; Clasifica a los textos en categorias .
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         texts:      vector de vectores, representado como
;;;                     una lista de listas
;;;         distance-measure: funcion de distancia
;;; OUTPUT: Pares formados por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia
;;;
( defun get-vectors-category (categories texts distance-measure)
    (if (null texts)
        nil
        (cons (cons (first (first texts))(funcall distance-measure (rest(first categories)) (rest (first texts)))) (get-vectors-category (rest categories) (rest texts) distance-measure))
    )
)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun calcular (f df x0)
  (- x0 (/(funcall f x0)(funcall df x0)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton
;;; Estima el cero de una funcion mediante Newton-Raphson
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;;         df: derivada de f
;;;         max-iter: maximo numero de iteraciones
;;;         x0: estimacion inicial del cero (semilla)
;;;         tol: tolerancia para convergencia (parametro opcional)
;;; OUTPUT: estimacion del cero de f o NIL si no converge
;;;
( defun newton (f df max-iter x0 &optional (tol 0.001))
  (let* ((xact (calcular f df x0)) (err (abs (- xact x0))))
    (if (= max-iter 0)
      NIL
      (if(< err tol)
        xact
        (newton f df (- max-iter 1) xact tol)
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-root-newton
;;; Prueba con distintas semillas iniciales hasta que Newton
;;; converge
;;;
;;; INPUT: f : funcion de la que se desea encontrar un cero
;;;        df : derivada de f
;;;        max-iter : maximo numero de iteraciones
;;;        semillas : semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: el primer cero de f que se encuentre , o NIL si se diverge
;;;          para todas las semillas
;;;
(defun one-root-newton (f df max-iter semillas &optional (tol 0.001))
  (if (null semillas)
    NIL
    (if(eql (newton f df (first semillas) tol) NIL)
      (one-root-newton f df (rest semillas) tol)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla o nil
;;;          si para esa semilla el metodo no converge
;;;
(defun all-roots-newton (f df max-iter semillas &optional ( tol 0.001))
   (if (null semillas)
    nil
      (cons (newton f df max-iter (first semillas) tol) (all-roots-newton f df max-iter semillas &optional tol ) )
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst    ----> (combine-elt-lst 'a '(1 2 3)) -> ((A 1) (A 2) (A 3))
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun combine-elt-lst (elt lst)
  (if (null lst)
    nil
    (cons (cons elt (first lst)) (combine-elt-lst elt (rest lst) )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst ----> (combine-lst-lst'(a b c)'(1 2)) ;; --> ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas
(defun combine-lst-lst (lst1 lst2)
  (if (null lst1)
    nil
    (append (combine-elt-lst (first lst1 ) lst2) (combine-lst-lst (rest lst1) lst2)   )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts
;;(combine-list-of-lsts'((a b c) (+ -) (1 2 3 4)))
;; --> ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;;      (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;;      (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))


;;; Calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion
;;; aparezca unicamente un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos
(defun combine-list-of-lsts (lstolsts)
  (if (null lstolsts)
    nil
    (if (null (first lstolsts))
     ; (combine-list-of-lsts (rest lstolsts) )
      ;(combine-elt-lst (first (first lstolsts))

    ; estoy pensandoooooooo no rayarse sin terminar


    (;cons (combine-lst-lst (first lstolsts) ) )
  )
)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; defino operadores logicos
(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '!)

;; definiciones de valores de verdad, conectores y atomos
(defun truth-value-p (x)
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x)
  (eql x +not+))

(defun binary-connector-p (x)
  (or (eql x +bicond+)
      (eql x +cond+)))

(defun n-ary-connector-p (x)
  (or (eql x +and+)
      (eql x +or+)))

(defun bicond-connector-p (x)
  (eql x +bicond+))

(defun cond-connector-p (x)
    (eql x +cond+))

(defun connector-p (x)
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p  x)))

(defun positive-literal-p (x)
  (and (atom x)
       (not (truth-value-p x))
       (not (connector-p x))))

(defun negative-literal-p (x)
  (and (listp x)
       (eql +not+ (first x))
       (null (rest (rest x)))
       (positive-literal-p (second x))))

(defun literal-p (x)
  (or (positive-literal-p x)
      (negative-literal-p x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; truth-tree
;;; Recibe una expresion y construye su arbol de verdad para
;;; determinar si es SAT o UNSAT
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : T   - FBF es SAT
;;;          N   - FBF es UNSAT
;;;
(defun truth-tree (exp)
  (let ((lista '()))
    (if (tiene-contradicciones (expand-truth-tree lista exp)) NIL T)
  )
)


(defun expand-truth-tree (lista exp)  ; retorna la lista de literales en ramas (A B (B (!C) C) A)
  (if (literal-p (first exp))  ; si exp es literal se retorna
    (first exp)

    (if (eql (first exp) +cond+)   ; =>  ;traduce
      (expand-truth-tree lista (cons +or+ (cons +not+ (second exp)) (third exp)))

      (if (eql (first exp) +bicond+)  ; <=> ;traduce
        (expand-truth-tree lista (cons +and+ (cons +cond+ (second exp) (third exp)) (cons +cond+ (third exp) (second exp))))

        (if (eql (first exp) +or+)  ; v ;introduce en la lista los literales independientes listaes((A)) (A v B) => ((A) (A) (B))
          (append lista (flowor lista (rest exp)))

          (if (eql (first exp) +and+) ; ^ ;introduce en la lista una lista con los literales listaes((A)) (A ^ B) => ((A) (A B))
            (append lista (flowand lista (rest exp))))
        )
      )
    )
  )
)
(defun tiene-contradicciones (lista)
  (contradicciones-rec (first lista) (rest lista)))
  
(defun contradicciones-rec (elem lista)
  (if (null elem) T
    (if (null lista) (contradicciones-rec (first lista) (rest lista))
      (if (negative-literal-p elem)
        (if (positive-literal-p (first lista))
          (if (eql (second elem) (first lista))
            NIL
          )
        )
        (if (positive-literal-p elem)
          (if (negative-literal-p (first lista))
            (if (eql elem (second (first (lista))))
              NIL
            )
          )
        )
        (contradicciones-rec elem (rest lista))
      )
    )
  )
)

(defun flowor (lista lits)
  (if (null lits)
    NIL
    (cons (list (expand-truth-tree lista (first lits))) (flowor lista (rest lits)))))


(defun flowoand (lista lits)
  (if (null lits)
    NIL
    (cons (expand-truth-tree lista (first lits)) (flowor lista (rest lits)))))




  (if (tiene-contradicciones lista)
    NIL

    (if (literal-p (first exp)) ;LITERAL
      (cons exp list)

      (if (eql (first exp) +and+) ; AND
        ((let lits lista)
        (if (null (every #'(lambda (x) (setq lits (expand-truth-tree lits x)) ) (rest exp))) ; añade a nuevos literales por iteracion si es SAT
          NIL
          (setq lista (append lista lst))
        ))

        (if (eql (first exp) +or+) ; OR
          ((let lits lista)
          (if (null (some #'(lambda (x) (setq lista (append lista (expand-truth-tree lits x)))) (rest exp))) NIL)) ; siempre añade literales si es SAT

          (if (eql (first exp) +cond+) ; COND =>




          )
        )
      )
    )
  )
)


 (maplist #'(lambda (x) (cons 'foo x)) '(a b c d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortest-path-improved
;;; Version de busqueda en anchura que no entra en recursion
;;; infinita cuando el grafo tiene ciclos
;;; INPUT:   end: nodo final
;;;          queue: cola de nodos por explorar
;;;          net: grafo
;;; OUTPUT: camino mas corto entre dos nodos
;;;         nil si no lo encuentra

(defun bfs-improved (end queue net)
  )

(defun shortest-path-improved (end queue net)
  )

;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : T   - FBF es SAT
;;;          N   - FBF es UNSAT
;;;


  )

(expand-truth-tree lista )




  (if (tiene-contradicciones lista)
    NIL

    (if (literal-p (first exp)) ;LITERAL
      (cons exp list)

      (if (eql (first exp) +and+) ; AND
        ((let lits lista)
        (if (null (every #'(lambda (x) (setq lits (expand-truth-tree lits x)) ) (rest exp))) ; añade a nuevos literales por iteracion si es SAT
          NIL
          (setq lista (append lista lst))
        ))

        (if (eql (first exp) +or+) ; OR
          ((let lits lista)
          (if (null (some #'(lambda (x) (setq lista (append lista (expand-truth-tree lits x)))) (rest exp))) NIL)) ; siempre añade literales si es SAT

          (if (eql (first exp) +cond+) ; COND =>




          )
        )
      )
    )
  )
)


 (maplist #'(lambda (x) (cons 'foo x)) '(a b c d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Breadth-first-search in graphs;;;
(defun bfs (end queue net)
  ;Si la cola se ha quedado vacía, terminamos
  (if (null queue) '()
    ;Si no, guardamos los valores del path (recorrido) y node
    (let* ((path (first queue))
      (node (first path))) 
    ;;; si tenemos que el nodo actual es el mismo que el nodo que queremos encontrar
    ;;; lo que haremos será devolver el path (lista acumulada de nodos), del reverso, ya que 
    ;;; tal y como vamos guardando los nodos que encontramos es al revés del orden natural.
    (if (eql node end)
      (reverse path) 

    ;;; Si no encontramos el nodo del final, repetimos la operación, uniendo el resto de la cola con 
    ;;; los nuevos nodos descubiertos, de tal manera que recursivamente vayamos explorando el grafo
    (bfs end
      (append (rest queue)
        (new-paths path node net))
      net)))))

(defun new-paths (path node net)
  (mapcar #'(lambda(n)
    ;el cambio reside aquí, debido a que concatenamos siempre n al path, se generan dichos bucles, 
    ;con un control de errores chequeando si el nodo ya está en la lista, se evita la recursividad infinita.
    (if (member n path)
      nil
    (cons n path)))
  (rest (assoc node net)))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shortest-path (start end net) 
(bfs end (list (list start)) net))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortest-path-improved
;;; Version de busqueda en anchura que no entra en recursion
;;; infinita cuando el grafo tiene ciclos
;;; INPUT:   end: nodo final
;;;          queue: cola de nodos por explorar
;;;          net: grafo
;;; OUTPUT: camino mas corto entre dos nodos
;;;         nil si no lo encuentra

(defun bfs-improved (end queue net)
  ;Si la cola se ha quedado vacía, terminamos
  (if (null queue) '()
    ;Si no, guardamos los valores del path (recorrido) y node
    (let* ((path (first queue))
      (node (first path))) 
    ;;; si tenemos que el nodo actual es el mismo que el nodo que queremos encontrar
    ;;; lo que haremos será devolver el path (lista acumulada de nodos), del reverso, ya que 
    ;;; tal y como vamos guardando los nodos que encontramos es al revés del orden natural.
    (if (eql node end)
      (reverse path) 

    ;;; Si no encontramos el nodo del final, repetimos la operación, uniendo el resto de la cola con 
    ;;; los nuevos nodos descubiertos, de tal manera que recursivamente vayamos explorando el grafo
    (bfs-improved end
      (append (rest queue)
        (new-paths path node net))
      net))))
  )

(defun shortest-path-improved (end queue net)
  (bfs-improved end (list (list start)) net))
