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
  (lambda (n)  n)
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
        (car l)
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
;Verifica si todos los elementos dentro de l son clausulas
(define list-of-claus?
  (lambda(l)
     (if (eqv? (list-length l ) 1)
        (clausula? (car l))
        (list-of-claus? (cdr l))
        )
    )
  )
;;Verifica si todos los elementos de una lista son variables booleanas
(define list-of-bool-var?
  (lambda(l)
     (if (eqv? (list-length l ) 1)
        (bool-var? (car l))
        (list-of-bool-var? (cdr l))
        )
    )
  )
;;Probando list-of-bool-var?
(list-of-bool-var? bool-var-list3)
(list-of-bool-var? bool-var-list2)
;;Probando list-of-claus?
(list-of-claus? clau-list3)
(list-of-claus? clau-list)
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

;;ejercicio 1.2
;;<bool-var> ::= (var number)
;;<clausula> ::= (clau <bool-var>)
;;           ::= (clau-or <bool-var> <clausula>)
;;<fnc-exp>  ::= (fnc <clausula>)
;;           ::= (fnc-and <clausula> <fnc-exp>)

;;Datatype de bool-var
(define-datatype bool-var-type bool-var-type?
  (var (numero number?))
  )

(var 1)
(var 2)
(var -3)
;;Datatype de clausula
(define-datatype clausula-type clausula-type?
  (clau (bool-var bool-var-type?))
  (clau-or (bool-var bool-var-type?)(resto clausula-type?))
  )


(clau (var 1))
(clau-or (var 1)(clau-or (var -2)(clau (var 3))))
(clau-or (var 1)(clau (var -2)))
;;Datatype de fnc-exp
 (define-datatype fnc-exp-type fnc-exp-type?
  (fnc (clau clausula-type?))
  (fnc-and (clau clausula-type?)(resto fnc-exp-type?))
  )
;;usos instancias SAT
(fnc (clau-or (var 1)(clau-or (var -2)(clau (var 3)))))
(fnc-and (clau-or (var 1)(clau (var -2)))(fnc (clau-or (var 1)(clau-or (var -2)(clau (var 3))))))
(fnc-and (clau (var -1)) (fnc-and (clau-or (var 1)(clau (var -2)))(fnc (clau-or (var 1)(clau-or (var -2)(clau (var 3)))))))