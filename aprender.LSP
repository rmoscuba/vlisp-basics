;;;                                                                      ;
;;;                  Rutinas para el aprendizaje de VisualLisp           ;
;;;                                                                      ;
;;;  � 2006                Rodolfo M Ortega Santiesteban                 ;
;;; 			rortega@vertice.cu, rmortega77@yahoo.es        	 ;
;;;                                                                      ;
;;;  � 2006  �` V�rtice� Empresa de Dise�o y Proyectos de Ingenier�a     ;


;;;     Este comentario no se identa porque comienza con ;;;  	         ;

;;; Funci�n C�RCULOS para ser llamada desde AutoCad, comienza con c:     ;
;;; genera 10 c�rculos concentricos, muestra el uso de la funci�n WHILE  ;

(defun c:circulos (/ p1 r)
  (terpri)
  (while (setq p1 (getpoint "Indique el punto"))
    (terpri)
    (setq r 0)
    (while (> 100 r)
      (setq r (+ r 10))
      (command "circle" p1 r)
    )
  )
)

;;; Funci�n RFRACTAL para ser llamada dede VisualLisp                    ;
;;; Genera una figura fractal, para la comprensi�n de la recursi�n en    ;
;;; VisualLisp, el uso de la funci�n IF y el concepto de variable local. ;
;;; Observe que hemos definido las variables locales pt1 pt2 y p3 para   ;
;;; guardar los resultados temporales de cada nivel del �rbol recursivo, ;
;;; de otra manera los valores se sobreescribirian en p1 y p2 en cada    ;
;;; retorno de la llamada recursiva                                      ;

(defun rfractal	(p1 p2 n / pt1 pt2 p3)

  ;;  Este comentario se identa al nivel del codigo por comenzar con ;;  ;

  ;; p3 guarda el punto medio, desplazado en el eje de las y en la       ;
  ;; tercera parte de la distancia entre p1 y p2                         ;

  (setq	p3 (list (/ (+ (car p1) (car p2)) 2)
		 (/ (+ (/ (distance p1 p2) 3) (cadr p1) (cadr p2)) 2)
		 (/ (+ (caddr p1) (caddr p2)) 2)
	   )
  )

  (if (> n 0)
    (progn
      ;; La recursi�n permite definir funciones a partir de si mismas    ;
      ;; el proceso genera un arbol de todas las posibles combinaciones  ;
      ;; limitado por la condici�n del IF que detiene el proceso         ;

      (setq n (1- n))
      (setq pt1 p1)
      (setq pt2 p2)
      (rfractal pt1 p3 n)
      (rfractal p3 pt2 n)

    )

    ;; Estas son las hojas del arbol generado, la acci�n de la funci�n   ;
    ;; se hace aqu�, en dependencia de lo que queremos obtener, en este  ;
    ;; caso queremos dibujar s�lo las lineas finales correspondientes al ;
    ;; �rbol de puntos p1 y p2 generado (�ltimo nivel de iteraciones)    ;
    (progn
      (command "color" (+ 10 (* n 50)))
      (command "line" p1 p2 "")
      ;;(command "circle" p3 (/ (distance p1 p2) 2))
    )
  )

)

;;; funci�n LATOMO para ser llamada desde AutoLisp                       ;
;;; Lista todos los �tomos contenidos en una lista, y las sublistas      ;
;;; recursivamente.                                                      ;

(defun latomo (a / c r)			;    este comentario permanece   ;
  (if (atom a)				; a la derecha por que comienza  ;
    (print a)				;      con un s�lo un ;          ;
    (progn (setq c (car a))
	   (setq r (cdr a))
	   (latomo c)
	   (latomo r)
    )
  ) ;_  Comentario con ;_ se identa al lado de la expresi�n previa	 ;
)


;;; funci�n FRACTAL para ser llamada desde Autocad                       ;
;;; genera una figura fractal a partir de una llamada a la funci�n       ;
;;; recursiva rfractal, entre los puntos proporcionados por el usuario   ;
;;; con una profundidad de 6 niveles del arbol de recursi�n.             ;
;;; Para la comprensi�n del funcionamiento de la rutina, recomendamos    ;
;;; cambiar 6 por 2 o 3 y llamar la funci�n con la opci�n de debugeo     ;
;;; animado activada, Menu: Debug-->Animate                              ;

(defun c:fractal (/ p1 p2 n)
  (terpri)
  (setq p1 (getpoint "Indique el punto 1"))
  (terpri)
  (setq p2 (getpoint "Indique el punto 2"))
  (command "color" 7)
  (command "ortho" "off")
  (command "osnap" "off")
  ;;(command "line" p1 p2 "")
  (rfractal p1 p2 6)
  (command "osnap" "on")
  (command "ortho" "on")
)

;;; funci�n FIGURA para ser llamada desde Autocad                        ;
;;; genera una pared, una puerta, o una escalera  entre dos puntos de    ;
;;; acuerdo a la entrada del usuario. Muestra el uso de la funci�n COND  ;

(defun c:figura	(/ p1 p2)
  (terpri)
  (setq s (getstring "�Qu� quiere dibujar? (Pared,Cortina,Puerta):"))
  (terpri)
  (setq p1 (getpoint "Indique el punto 1"))

  (command "color" 7)
  (command "ortho" "off")
  (command "osnap" "off")
  (cond
    ((= s "Cortina")
     (progn (terpri)
	    (setq p2 (getpoint "Indique el punto 2"))
	    (command "_AecCurtainWallAdd" p1 p2 "")
     )
    )
    ((= s "Puerta") (command "door" "add" "" p1 "" ""))
    ((= s "Pared")
     (progn (terpri)
	    (setq p2 (getpoint "Indique el punto 2"))
	    (command "wall" "add" p1 p2 "" "")
     )
    )
    (t nil)
  )
  (command "osnap" "on")
  (command "ortho" "on")
)

;;; funci�n RECORRER para ser llamada desde  AutoLisp para mostrar el    ;
;;; uso de las funciones de acceso a la BD de AutoCad                    ;
;;; Recorre toda la base de datos e imprime el nombre que la identifica  ;

(defun recorrer	(/ ent)
  (terpri)
  (setq ent (entnext))
  (print ent)
  (while
    (setq ent (entnext ent))
     (terpri)
     (print ent)
  )
)

;;;  � 2006                Rodolfo M Ortega Santiesteban                 ;
;;; 			rortega@verice.cu, rmortega77@yahoo.es        	 ;