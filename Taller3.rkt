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
  (texto        ( letter (arbno (or letter whitespace digit ":" "?" "=" "'" "#" "$" "&" "." "," ";" "*" )) ) string)
  (numero     (digit (arbno digit)) number)
  (numero      ("-" digit (arbno digit)) number)
  (numero      (digit (arbno digit) "." digit (arbno digit)) number)
  (numero      ("-" digit (arbno digit) "." digit (arbno digit)) number)
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

    ;;(expresion ("\""text"\"")   texto-lit)
    
    (expresion ("("expresion primitiva-binaria expresion")")   primapp-bin-exp)
       
    (expresion (primitiva-unaria "(" expresion ")")   primapp-un-exp)

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
