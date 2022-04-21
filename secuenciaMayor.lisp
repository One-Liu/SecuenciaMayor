;FUNDAMENTOS
;Lista de entrada: '(55 70 10 5 90 130 40 80)
;Proceso: ((55 70) (10) (5 90 130) (40 80))
;   |-> Uso de funciones secuenciaMayor (función principal), crearSecuencia y crearListaSecuencias
;   |-> Se divide la lista ingresada en secuencias
;Salida: (5 90 130)
;   |-> Uso de funciones secuenciaMayorAux, obtenerSecuenciasMayores y crearListaSecuencias
;   |-> Se evalúa el tamaño de las secuencias creadas en el proceso y se crea una lista con las secuencias de mayor tamaño

;función que determina cuáles son las secuencias de mayor tamaño, a partir de una lista de secuencias
(defun obtenerSecuenciasMayores (listaSecuencias &optional (listaSecuenciasMayores '()) (mayorTamanio 0))
  (if (not listaSecuencias)
    listaSecuenciasMayores
    (cond
      ((> (length (car listaSecuencias)) mayorTamanio) 
        (obtenerSecuenciasMayores (cdr listaSecuencias) (crearListaSecuencias (car listaSecuencias)) (length (car listaSecuencias)))
      )
      ((< (length (car listaSecuencias)) mayorTamanio)
        (obtenerSecuenciasMayores (cdr listaSecuencias) listaSecuenciasMayores mayorTamanio)
      )
      ((= (length (car listaSecuencias)) mayorTamanio)
        (obtenerSecuenciasMayores (cdr listaSecuencias) (crearListaSecuencias (car listaSecuencias) listaSecuenciasMayores) (length (car listaSecuencias)))
      )
    )
  )  
)

;función que se ejecuta después de separar en secuencias la lista de números ingresada
(defun secuenciaMayorAux (listaSecuencias)
  (let ((listaSecuenciasMayores (obtenerSecuenciasMayores listaSecuencias)))
    (if (= (length listaSecuenciasMayores) 1)
      (car listaSecuenciasMayores)
      listaSecuenciasMayores
    )
  )
)

;función que crea una lista de listas (secuencias)
(defun crearListaSecuencias (lista &optional (listaSecuencias '()))
  (if (equal listaSecuencias nil)
    (list lista)
    (append listaSecuencias (list lista))
  )
)

;función que crea una lista (secuencia) con los átomos (números)
(defun crearSecuencia (elementosAnteriores elementoNuevo)
  (if (listp elementosAnteriores)
    (append elementosAnteriores (list elementoNuevo))
    (append (list elementosAnteriores) (list elementoNuevo))
  )
)

;función principal
(defun secuenciaMayor (lista &optional (secuencia '()) (listaSecuencias '()) (nuevaSecuencia t))
  (if (equal (cdr lista) nil)
    (secuenciaMayorAux (crearListaSecuencias (crearSecuencia secuencia (first lista)) listaSecuencias))
    (if nuevaSecuencia
      (if (< (first lista) (second lista))
        (secuenciaMayor (cdr lista) (crearSecuencia '() (first lista)) listaSecuencias nil)
        (secuenciaMayor (cdr lista) '() (crearListaSecuencias (crearSecuencia secuencia (first lista)) listaSecuencias) t)
      )
      (if (< (first lista) (second lista))
        (secuenciaMayor (cdr lista) (crearSecuencia secuencia (first lista)) listaSecuencias nil)
        (secuenciaMayor (cdr lista) '() (crearListaSecuencias (crearSecuencia secuencia (first lista)) listaSecuencias) t)
      )
    ) 
  )
)