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

;;                 := procedimiento (<identificador>*',') haga <expresion> finProc
;;                    <procedimiento-ex (ids cuerpo)>

;;                 := evaluar <expresion>(<expresion> ",")* finEval
;;                    <app-exp(exp exps)>

;;                 ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp proc-names idss bodies bodyletrec>




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
  (texto        (letter (arbno (or letter digit ":" "?" "=" "'" "#" "$" "&" "." "," ";" "*" "!" "¡" "¿" "-" "_"))) string)
  (numero       (digit (arbno digit)) number)
  (numero       ("-" digit (arbno digit)) number)
  (numero       (digit (arbno digit) "." digit (arbno digit)) number)
  (numero       ("-" digit (arbno digit) "." digit (arbno digit)) number)
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

    (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}" ) variableLocal-exp)

    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc" ) procedimiento-ex)

    (expresion ( "evaluar"  expresion "("(separated-list expresion ",") ")" "finEval") app-exp)

    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) letrec-exp)

    
    ;;Primitiva Binaria

    (primitiva-binaria ("+")      primitiva-suma)
    
    (primitiva-binaria ("~")      primitiva-resta)
    
    (primitiva-binaria ("/")      primitiva-div)
    
    (primitiva-binaria ("*")      primitiva-multi)
    
    (primitiva-binaria ("concat") primitiva-concat)

    ;;Primitiva Unaria

    (primitiva-unaria ("longitud")  primitiva-longitud)
    
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

;eval-expresion: <expresion> <enviroment> -> 
; evalua la expresión en el ambiente de entrada, para cada caso (numero-lit,var-exp,texto-lit, condicional-exp, variableLocal-exp
;procedimiento-ex, app-exp, letrec, primapp-bin-exp, primapp-un-exp) devuelve algo diferente.

(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      
      (numero-lit (numero) numero)
      
      (var-exp (id) (buscar-variable env id))
      
      (texto-lit (txt) txt)

      (condicional-exp (test-exp true-exp false-exp)
              (if (valor-verdad? (eval-expresion test-exp env))
                  (eval-expresion true-exp env)
                  (eval-expresion false-exp env)
               )
       )


      (variableLocal-exp (ids exps cuerpo)
               (let ((args (eval-rands exps env)))
                    (eval-expresion cuerpo (extend-env ids args env))
               )
       )

      (procedimiento-ex (ids cuerpo) (cerradura ids cuerpo env))

      (app-exp (exp exps)
               (let ((proc (eval-expresion exp env))
                     (args (eval-rands exps env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expresion "Attempt to apply non-procedure ~s" proc)
                  )
               )
       )

      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      
      
      (primapp-bin-exp (exp1 prim-binaria exp2) (apply-primitiva-bin  exp1 prim-binaria exp2 env))
      
      (primapp-un-exp (prim-unaria exp) (apply-primitiva-un prim-unaria exp env))

      
                    
     )
   )
)

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)

;eval-rands:
;proposito: realiza un mapeo de la funcion eval-expression por todos los elementos de la lista para que cada elemento sea evaluado.
(define eval-rands
  (lambda (exps env)
    (map (lambda (x) (eval-rand x env)) exps)
  )
)

;eval-rand:
;proposito: aplica la funcion eval-expression a una expresion en sintaxis abstracta en un ambiente.

(define eval-rand
  (lambda (exp env)
    (eval-expresion exp env)
  )
)

;apply-primitiva-bin: <expresion> <primitiva> <expresion> -> <numero> o <texto>
;Proposito: Realiza operaciones binarias como suma, resta, multiplicacion y division para los numeros o identificadores que sean
;numeros, y concat para los textos o identificadores que sean textos

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

;apply-primitiva-un: <primitiva> <expresion> -> <numero> o <texto>
;Proposito: Realiza operaciones unarias como add1 (suma una unidad a un numero), sub1 (resta una unidad a un numero)
;para los numeros o identificadores que sean numeros, y longitud para los textos o identificadores que sean textos.

(define apply-primitiva-un
  (lambda (prim-unaria exp env)
    (cases primitiva-unaria prim-unaria
      (primitiva-longitud () (string-length (eval-expresion exp env)))
      (primitiva-add1 () (+ (eval-expresion exp env) 1))
      (primitiva-sub1 () (- (eval-expresion exp env) 1))
    )
  )
)

;*******************************************************************************************
;;Booleanos

;valor-verdad?: <numero> -> <booleano>
;Proposito: determina si un valor dado corresponde a un valor booleano falso o verdadero. Devuelve true para cualquier numero
;diferente de cero, y false para cero

(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb environment?)))

;apply-procedure: 
;proposito: Evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (cerradura (ids body env)
               (eval-expresion body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)
  )
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?)
  )
)

;;Predicado para representar cualquier valor. 

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

;buscar-variable: <ambiente><identificador> -> <scheme-value>
;proposito: función que busca un símbolo en un ambiente y devuelve lo que este almacenado.
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
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position id proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (buscar-variable old-env id)))
      )
    )
  )
)

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

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
