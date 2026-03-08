#lang eopl



;funciones auxiliares

;list−length : List −> Int
;usage: (list−lengthl ) = the length of l
(define list-length
   (lambda (lst)
   (if(null? lst)
     0
(+ 1(list-length (cdr lst))))))



;; ejercicio 1.1
;; <bool-var>      ::= number
;;                 ::= - number
;; <clausula>      ::= (<bool-var>)
;;                 ::= (<bool-var> 'or <clausula>)
;; <fnc-exp>       ::= (<clausula>)
;;                 ::= (<clausula> 'and <fnc-exp>)
(define bool-var
  (lambda (v)
      (list 'bool-var v)
    )
  )
;;Predicado bool-var
(define bool-var?
  (lambda(v)
     (number? v)
    )
  )
(define bool-var-list1  (list (bool-var 1)
                             (bool-var -2)
                             (bool-var 3))
  )
  
(define bool-var-list2  (list (bool-var 1)
                              (bool-var -2))
  )
(define bool-var-list3  (list (bool-var 1)
                              (bool-var -2)
                              (bool-var -3)
                              (bool-var 4))
  )
;;Extrae la variable booleana
(define list->get-var
  (lambda (l)
        (cadr l)
    )
  )

(define var2 (list->get-var (car bool-var-list1)))


(define clausula
  (lambda (l)
    (if (eqv? (list-length l) 1)
         (list  (list->get-var (car l)))
         (cons  (list->get-var (car l))  (cons 'or (clausula (cdr l))))
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

;;Extrae la clausula n de la fnc-exp l
(define fnc-exp-get-clau
  (lambda(l n)
    (if (eqv? n 1)
        (car l)
        (fnc-exp-get-clau(cddr l)(- n 1 ))
        )
      )
  )
(define fnc1 (fnc-exp clau-list))
(define fnc2 (fnc-exp clau-list2))
(define fnc3 (fnc-exp clau-list3))
;;Probando fnc-exp?
(fnc-exp? fnc1)
(fnc-exp? fnc2)
(fnc-exp? fnc3)
(fnc-exp? (list clau1))
;;ejercicio 1.2


(define-datatype bool-var-type bool-var-type?
  (var (numero number?))
  )

(var 1)
(var 2)
(var -3)


(define-datatype clausula-type clausula-type?
  (clau (bool-var bool-var-type?))
  (clau-or (bool-var bool-var-type?)(resto clausula-type?))
  )

(clau (var 1))
(clau-or (var 1)(clau-or (var -2)(clau (var 3))))
(clau-or (var 1)(clau (var -2)))

 (define-datatype fnc-exp-type fnc-exp-type?
  (fnc (clau clausula-type?))
  (fnc-and (clau clausula-type?)(resto fnc-exp-type?))
  )
;;usos instancias SAT
(fnc (clau-or (var 1)(clau-or (var -2)(clau (var 3)))))
(fnc-and (clau-or (var 1)(clau (var -2)))(fnc (clau-or (var 1)(clau-or (var -2)(clau (var 3))))))
(fnc-and (clau (var -1)) (fnc-and (clau-or (var 1)(clau (var -2)))(fnc (clau-or (var 1)(clau-or (var -2)(clau (var 3)))))))