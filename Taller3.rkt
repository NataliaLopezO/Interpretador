#lang eopl
;;INTERPRETADOR
;; Natalia Lopez Osorio - 2025618
;;Carolain JImenez Bedoya - 2071368
;;Juan Steban Diaz - 2024147
;;Gabriel Franco - 2024200
;;Hernando Lopez - 2022318

;******************************************************************************************

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>     ::= <expresion>
;;                     <un-programa (exp)>

;;  <expresion>    ::= <numero>
;;                     <numero-lit  (num)>

;;                 := "\""<texto> "\""
;;                     <texto-lit (txt)>

;;                 ::= <identificador>
;;                     <var-exp (id)>

;;                 ::= (<expression><primitiva-binaria><expression>)
;;                     <primapp-bin-exp (exp1 prim-binaria exp2)>

;;                 ::= <primitiva-unaria>(<expression>)
;;                     <primapp-un-exp (prim-unaria exp)>

;;                 ::= Si <expresion> entonces <expresion> sino <expression> finSI
;;                      <condicional-exp (test-exp true-exp false-exp)>




;;  <primitiva-binaria>   ::= + (primitiva-suma)
;;                        ::= ~ (primitiva-resta)
;;                        ::= / (primitiva-div)
;;                        ::= * (primitiva-multi)
;;                        ::= concat(primitiva-concat)

;;  <primitiva-unaria>   ::= longitud(primitiva-longitud)
;;                       ::= add1(primitiva-add1)
;;                       ::= sub1(primitiva-sub1)

;******************************************************************************************

;******************************************************************************************

;Especificación Léxica

(define scanner-spec-simple-interpreter
'(
  (white-sp    (whitespace) skip)
  (comentario     ("%" (arbno (not #\newline))) skip)
  (identificador  ("@" letter (arbno (or letter digit))) symbol)
  (texto      ( letter (arbno (or letter digit ":" "?" "=" "'" "#" "$" "&" "." "," ";" "*" "!" "¡" "¿" )) ) string)
  (numero     (digit (arbno digit)) number)
  (numero     ("-" digit (arbno digit)) number)
  (numero     (digit (arbno digit) "." digit (arbno digit)) number)
  (numero     ("-" digit (arbno digit) "." digit (arbno digit)) number)
 )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    ;;Programa
    
    (programa (expresion) un-programa)

    ;;Expresion
    
    (expresion (numero)   numero-lit)
    
    (expresion (identificador)   var-exp)

    (expresion ("\""texto"\"")   texto-lit)
    
    (expresion ("("expresion primitiva-binaria expresion")")   primapp-bin-exp)
       
    (expresion (primitiva-unaria "(" expresion ")")   primapp-un-exp)

    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)

    ;;Primitiva Binaria

    (primitiva-binaria ("+")      primitiva-suma)
    
    (primitiva-binaria ("~")      primitiva-resta)
    
    (primitiva-binaria ("/")      primitiva-div)
    
    (primitiva-binaria ("*")      primitiva-multi)
    
    (primitiva-binaria ("concat") primitiva-concat)

    ;;Primitiva Unaria
    
    (primitiva-unaria ("add1") primitiva-add1)
    
    (primitiva-unaria ("sub1") primitiva-sub1)
  )
)

;*******************************************************************************************
;Tipos de datos para la sintaxis abstracta de la gramática construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)
  )
)

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter)
)

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter)
)

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
                         
    (lambda (pgm) (eval-programa  pgm))
    
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)
   )
 )

;*******************************************************************************************
;El Interprete

;eval-programa: <programa> -> expresion
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (exp)
                 (eval-expresion exp (init-env))
      )
    )
  )
)

; Ambiente inicial

(define init-env
  (lambda ()
    (extend-env
      '(@a @b @c @d @e)
      (list 1 2 3 "Hola" "FLP")
      (empty-env)
    )
  )
)

;eval-expresion: <expresion> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      
      (numero-lit (numero) numero)
      
      (var-exp (id) (buscar-variable env id))
      
      (texto-lit (txt) txt)
      
      (primapp-bin-exp (exp1 prim-binaria exp2) (apply-primitiva-bin  exp1 prim-binaria exp2 env))
      
      (primapp-un-exp (prim-unaria exp) (apply-primitiva-un prim-unaria exp env))

      (condicional-exp (test-exp true-exp false-exp)
              (if (valor-verdad? (eval-expresion test-exp env))
                  (eval-expresion true-exp env)
                  (eval-expresion false-exp env)
               )
       )
                    
     )
   )
)

;apply-primitiva-bin: <expresion> <primitiva> <expresion> -> 

(define apply-primitiva-bin
  (lambda (exp1 prim-binaria exp2 env)
    
    (cases primitiva-binaria prim-binaria
      
      (primitiva-suma () (+ (eval-expresion exp1 env) (eval-expresion exp2 env)))
      (primitiva-resta () (- (eval-expresion exp1 env) (eval-expresion exp2 env)))
      (primitiva-div () (/ (eval-expresion exp1 env) (eval-expresion exp2 env)))
      (primitiva-multi () (* (eval-expresion exp1 env) (eval-expresion exp2 env)))
      (primitiva-concat () (string-append (eval-expresion exp1 env) (eval-expresion exp2 env)))
    )
  )
)

;apply-primitiva-un: <primitiva> <list-of-expresion> ->

(define apply-primitiva-un
  (lambda (prim-unaria exp env)
    (cases primitiva-unaria prim-unaria
      (primitiva-add1 () (+ (eval-expresion exp env) 1))
      (primitiva-sub1 () (- (eval-expresion exp env) 1))
    )
  )
)

;valor-verdad?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)
  )
)

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda () (empty-env-record))) ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)
   )
 ) 

;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env id)
    (cases environment env
      (empty-env-record () (eopl:error "Error, la variable no existe"))
      (extended-env-record (ids vals env)
                           (let(
                                 (pos (list-find-position id ids))
                                )                             
                               (
                                  if (number? pos)
                                     (list-ref vals pos)
                                     (buscar-variable env id)
                                )
                           )
      )
    )
  )
)

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

