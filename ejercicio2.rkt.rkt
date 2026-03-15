#lang eopl
;Diego Armando Espinosa Ossa - 1942206


;funciones auxiliares

;list-length : List -> Int
;usage: (list-lengthl ) = the length of l
(define list-length
   (lambda (lst)
   (if(null? lst)
     0
(+ 1(list-length (cdr lst))))))
;Pruebas list-length
(list-length '('a 'b 'c))
(list-length '())
;; ejercicio 1.1
;; <bool-var>      ::= number
;;                 ::= - number
;; <clausula>      ::= (<bool-var>)
;;                 ::= <bool-var> 'or <clausula>
;; <fnc-exp>       ::= (<clausula>)
;;                 ::= <clausula> 'and <fnc-exp>
;;Constructor bool-var
(define bool-var
  (lambda (n) n)
  )

;; Uso bool-var
(define bv1 (bool-var 1))
(define bv2 (bool-var -2))
(define bv3 (bool-var 3))
(define bv4 (bool-var -4))
;;Predicado bool-var
(define bool-var?
  (lambda(bv)
    (number? bv)
    )
  )
  
(bool-var? bv1)
(bool-var? bv2)
(bool-var? bv3)

(define bool-var-list1  (list bv1
                              bv2
                              bv3)
  )
  
(define bool-var-list2  (list bv1
                              bv2)
  )
(define bool-var-list3  (list bv1
                              bv2
                              bv3
                              bv4)
  )


;;Constructor clausula
(define clausula
  (lambda (l)
    (if (eqv? (list-length l) 1)
         (list   (car l))
         (cons   (car l)  (cons 'or (clausula (cdr l))))
         )
    )
  )
;;Extrae las variables booleanas de la clausula l en una lista
(define clau->get-var
  (lambda (l)
    (if (eqv? (list-length l)1)
        (list(car l))
        (cons (car l)(clau->get-var(cddr l)))
        )
    )
 )



;;Predicado clausula
(define clausula?
  (lambda(c)
    (if (eqv? (list-length c ) 1)
        (bool-var? (car c))
        (and (eqv? (cadr c )'or) (clausula? (cddr c)))
        )
    )
  )

  
  

(define clau1 (clausula bool-var-list1))
(define clau2 (clausula bool-var-list2))
(define clau3 (clausula bool-var-list3))

(define clau4 (clausula bool-var-list2))
(define clau5 (clausula bool-var-list1))
(define clau6 (clausula bool-var-list3))

(define clau7 (clausula bool-var-list3))
(define clau8 (clausula bool-var-list2))
(define clau9 (clausula bool-var-list1))
;;Probando clau->get-var
(clau->get-var  clau1)
(clau->get-var  clau2)
(clau->get-var  clau3)
;;Probando clausula?
(clausula? (list 1))
(clausula? clau2)

(define clau-list (list clau1
                        clau2
                        clau3))
(define clau-list2 (list clau4
                         clau5
                         clau6))
(define clau-list3 (list clau7
                         clau8
                         clau9))

;;Verifica si todos los elementos de una lista son variables booleanas


;;Constructor fnc-exp
(define fnc-exp
  (lambda(l)
    (if (null? (cdr l))
        (list (car l))
        (cons (car l) (cons 'and (fnc-exp (cdr l))))
        )
      )
  )
;;Predicado fnc-exp
(define fnc-exp?
  (lambda(fnc)
       (if (eqv? (list-length fnc ) 1)
        (clausula? (car fnc))
        (and (eqv? (cadr fnc )'and) (fnc-exp? (cddr fnc)))
        )
      )
  )

;;Extrae las clausulas de la fnc-exp l
(define fnc-exp-get-clau
  (lambda(l)
    (if (eqv? (list-length l)1)
        (list(car l))
        (cons (car l)(fnc-exp-get-clau(cddr l)))
        )
      )
  )
;;Este procedimiento extrae todas las variables de una expresion fnc 
(define fnc-exp-get-var
  (lambda (fnc)
          (if (eqv? (list-length (fnc-exp-get-clau fnc))1)
              (list(clau->get-var (car fnc)))
              (cons (clau->get-var (car fnc))(fnc-exp-get-var(cddr fnc))))
          )
  )


;; usos instancias SAT
(define fnc1 (fnc-exp clau-list))
(define fnc2 (fnc-exp clau-list2))
(define fnc3 (fnc-exp clau-list3))
;;Probando fnc-exp?
(fnc-exp? fnc1)
(fnc-exp? fnc2)
(fnc-exp? fnc3)
(fnc-exp? (list clau1))
;; probando fnc-exp-get-clau
(fnc-exp-get-clau fnc1)
(fnc-exp-get-clau fnc2)
;; Probando fnc-exp-get-var
(fnc-exp-get-var fnc1)

;; Ejercicio 2.1


;;Verifica si todos los elementos de una lista son variables booleanas
(define list-of-bool-var?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((bool-var? (car l))
       (list-of-bool-var? (cdr l)))
      (else #f)
      )
    )
  )
;Probando list-of-bool-var?
(list-of-bool-var? (car (fnc-exp-get-var fnc1)))
(list-of-bool-var? (cadr (fnc-exp-get-var fnc1)))

(define parse-bool-var
  (lambda (bvl)
    (cond
      ((null? bvl) '())
      ((bool-var? (car bvl))
       (cons (list 'var (car bvl))
             (parse-bool-var (cdr bvl))))
      (else
       (eopl:error 'parse-bool-var
                   "Variable invalida ~s"
                   (car bvl))
       )
      )
    )
  )
;;Probando parse-bool-var
(parse-bool-var (car (fnc-exp-get-var fnc1)))
(parse-bool-var (cadr (fnc-exp-get-var fnc1)))

(define parse-list-of-bool-var-list
  (lambda (lbvl)
    (cond
      ((null? lbvl) '())
      ((list-of-bool-var? (car lbvl))
       (cons (parse-bool-var (car lbvl))
             (parse-list-of-bool-var-list (cdr lbvl))))
      )
    )
  )


;;Probando parse-list-of-bool-var-list
(parse-list-of-bool-var-list (fnc-exp-get-var fnc1))
(parse-list-of-bool-var-list (fnc-exp-get-var fnc2))
(define parse-clausula
  (lambda (cl)
    (cond
      ((null? cl) '())
      (else
       (cons (list 'clausula (car cl))
             (parse-clausula (cdr cl)))
       )
      )
    )
  )



;Probando parse-clausula
(parse-clausula clau-list)
(parse-clausula clau-list2)
;; Este procedimiento construye el árbol de sintaxis abstracta dada una instancia SAT representada con listas
(define PARSEBNF
  (lambda (fnc-exp)
    (let* ((lbvl (parse-list-of-bool-var-list (fnc-exp-get-var fnc-exp)))
           (cl (parse-clausula lbvl)))
      (list 'fnc-exp cl)
      )
    )
  )

;; Convierte (var n) en n
(define unparse-bool-var
  (lambda (v)
    (if (and (list? v)
             (eqv? (car v) 'var)
             (number? (cadr v)))
        (cadr v)
        (eopl:error 'unparse-bool-var
                    "Bool-var abstracta invalida ~s"
                    v))))


(define unparse-list-of-bool-var
  (lambda (lbv)
    (cond
      [(null? lbv) '()]
      [else
       (cons (unparse-bool-var (car lbv))
             (unparse-list-of-bool-var (cdr lbv)))])))

;; Convierte (clausula ((var 1) (var -2) (var 3)))
;; en (1 or -2 or 3)
(define unparse-clausula
  (lambda (cl)
    (if (and (list? cl)
             (eqv? (car cl) 'clausula)
             (list? (cadr cl)))
        (clausula (unparse-list-of-bool-var (cadr cl)))
        (eopl:error 'unparse-clausula
                    "Clausula abstracta invalida ~s"
                    cl))))


;; Convierte
;; (fnc-exp ((clausula ((var 1) (var -2) (var 3)))
;;           (clausula ((var 1) (var -2)))))
;; en
;; ((1 or -2 or 3) and ((1 or -2)))
(define unparse-list-of-clausulas
  (lambda (lcl)
    (cond
      [(null? lcl) '()]
      [else
       (cons (unparse-clausula (car lcl))
             (unparse-list-of-clausulas (cdr lcl)))])))


(define UNPARSEBNF
  (lambda (ast)
    (if (and (list? ast)
             (eqv? (car ast) 'fnc-exp)
             (list? (cadr ast)))
        (fnc-exp (unparse-list-of-clausulas (cadr ast)))
        (eopl:error 'UNPARSEBNF
                    "AST invalido ~s"
                    ast))))

