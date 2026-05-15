#lang eopl

;; ============================================================
;; Taller 3 — Chequeador estático de tipos (MiniLang+Tipos)
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710, JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701, JUAN FELIPE RUIZ 2359397
;; ============================================================

(require eopl)

;; ============================================================
;; ESPECIFICACIÓN LÉXICA Y GRAMATICAL (misma del intérprete)
;; ============================================================

(define especificacion-lexica
  '(
    (espacio-blanco (whitespace) skip)
    (comentario ("%" (arbno (not #\newline))) skip)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (identificador (letter (arbno (or letter digit "?" "$"))) symbol)
    ))

(define especificacion-gramatical
  '(
    ; Programa
    (programa (expresion) a-program)

    ; Expresiones
    (expresion (numero) lit-exp)
    (expresion (identificador) ident-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion
     (primitiva "(" (separated-list expresion ",") ")")
     prim-exp)
    (expresion
     ("if" expresion "then" expresion "else" expresion)
     if-exp)
    (expresion
     ("let" (arbno identificador "=" expresion) "in" expresion)
     let-exp)
    (expresion
     ("var" (arbno identificador "=" expresion) "in" expresion)
     var-exp)
    (expresion
     ("set" identificador "=" expresion)
     set-exp)
    (expresion
     ("begin" expresion (arbno ";" expresion) "end")
     begin-exp)
    (expresion
     ("freeze" identificador)
     freeze-exp)
    (expresion
     ("proc" "(" (separated-list identificador ",") ")" expresion)
     proc-exp)
    (expresion
     ("(" expresion (arbno expresion) ")")
     app-exp)
    (expresion
     ("cond" (arbno expresion "==>" expresion)
             "else" "==>" expresion "end")
     cond-exp)
    (expresion
     ("let*" (arbno identificador "=" expresion) "in" expresion)
     let*-exp)
    (expresion
     ("unless" expresion "then" expresion "else" expresion)
     unless-exp)

    ; Primitivas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("add1") add-prim)
    (primitiva ("sub1") sub-prim)
    (primitiva (">") mayor-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("<") menor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva ("==") igual-prim)
    (primitiva ("not") not-prim)
    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("min") min-prim)
    (primitiva ("max") max-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") pow-prim)
    ))

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define scanner
  (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))

(define parser
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; ============================================================
;; TIPOS DEL LENGUAJE
;; ============================================================

;; tipo : int-type | bool-type | void-type | ref-type | proc-type
;; Propósito: representa los tipos del lenguaje MiniLang.
(define-datatype tipo tipo?
  (int-type)
  (bool-type)
  (void-type)
  (ref-type
   (inner-type tipo?))
  (proc-type
   (param-types (list-of tipo?))
   (result-type tipo?)))

;; equal-types? : Tipo x Tipo -> Boolean
;; Propósito: verifica igualdad estructural de tipos.
(define equal-types?
  (lambda (t1 t2)
    (cases tipo t1
      (int-type ()
        (cases tipo t2
          (int-type () #t)
          (else #f)))
      (bool-type ()
        (cases tipo t2
          (bool-type () #t)
          (else #f)))
      (void-type ()
        (cases tipo t2
          (void-type () #t)
          (else #f)))
      (ref-type (inner1)
        (cases tipo t2
          (ref-type (inner2)
            (equal-types? inner1 inner2))
          (else #f)))
      (proc-type (params1 result1)
        (cases tipo t2
          (proc-type (params2 result2)
            (and (= (length params1) (length params2))
                 (andmap equal-types? params1 params2)
                 (equal-types? result1 result2)))
          (else #f))))))

;; type-to-external-form : Tipo -> S-exp
;; Propósito: convierte un tipo a forma legible.
(define type-to-external-form
  (lambda (ty)
    (cases tipo ty
      (int-type () 'int)
      (bool-type () 'bool)
      (void-type () 'void)
      (ref-type (inner)
        (list 'ref (type-to-external-form inner)))
      (proc-type (params result)
        (list (map type-to-external-form params)
              '->
              (type-to-external-form result))))))

;; expand-type-expression : S-exp -> Tipo
;; Propósito: convierte una expresión S-exp en un tipo.
(define expand-type-expression
  (lambda (texp)
    (cond
      [(symbol? texp)
       (cond
         [(eq? texp 'int) (int-type)]
         [(eq? texp 'bool) (bool-type)]
         [(eq? texp 'void) (void-type)]
         [else (eopl:error 'expand-type-expression
                 "Tipo desconocido: ~s" texp)])]
      [(pair? texp)
       (cond
         [(eq? (car texp) 'ref)
          (ref-type (expand-type-expression (cadr texp)))]
         [(and (list? (car texp)) (eq? (cadr texp) '->))
          (proc-type
           (map expand-type-expression (car texp))
           (expand-type-expression (caddr texp)))]
         [else (eopl:error 'expand-type-expression
                 "Expresión de tipo inválida: ~s" texp)])]
      [else (eopl:error 'expand-type-expression
              "Expresión de tipo inválida: ~s" texp)])))

;; ============================================================
;; AMBIENTE DE TIPOS
;; ============================================================

;; El ambiente de tipos liga identificadores con (tipo, marca)
;; donde marca ∈ {'let, 'var, 'frozen}
(define-datatype type-environment type-environment?
  (empty-tenv)
  (extended-tenv
   (ids (list-of symbol?))
   (types (list-of tipo?))
   (marcas (list-of symbol?))
   (env type-environment?)))

;; apply-tenv : TypeEnv x Symbol -> (Tipo, Marca)
;; Propósito: busca el tipo y marca de un identificador.
(define apply-tenv
  (lambda (tenv id)
    (cases type-environment tenv
      (empty-tenv ()
        (eopl:error 'apply-tenv "Variable no ligada: ~s" id))
      (extended-tenv (ids types marcas old-tenv)
        (letrec
            ((buscar
              (lambda (ids types marcas)
                (cond
                  [(null? ids) (apply-tenv old-tenv id)]
                  [(equal? (car ids) id)
                   (cons (car types) (car marcas))]
                  [else (buscar (cdr ids) (cdr types) (cdr marcas))]))))
          (buscar ids types marcas))))))

;; init-tenv : () -> TypeEnv
;; Propósito: ambiente de tipos inicial con x,y,z,a,b,c : int (inmutables).
(define init-tenv
  (lambda ()
    (extended-tenv
     '(x y z a b c)
     (list (int-type) (int-type) (int-type)
           (int-type) (int-type) (int-type))
     '(let let let let let let)
     (empty-tenv))))

;; ============================================================
;; CHEQUEADOR DE TIPOS
;; ============================================================

;; type-of-program : Programa -> Tipo
;; Propósito: punto de entrada del chequeador.
(define type-of-program
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp)
        (type-of exp (init-tenv))))))

;; type-of : Expresion x TypeEnv -> Tipo
;; Propósito: infiere el tipo de una expresión.
(define type-of
  (lambda (exp tenv)
    (cases expresion exp

      ;; lit-exp : números son int
      (lit-exp (n)
        (int-type))

      ;; ident-exp : busca el tipo en el ambiente
      (ident-exp (id)
        (let ((binding (apply-tenv tenv id)))
          (car binding)))

      ;; true-exp / false-exp : literales bool
      (true-exp () (bool-type))
      (false-exp () (bool-type))

      ;; prim-exp : chequea primitivas
      (prim-exp (prim rands)
        (type-of-prim prim rands tenv))

      ;; if-exp : condición debe ser bool, ramas deben tener mismo tipo
      (if-exp (test then-exp else-exp)
        (let ((test-type (type-of test tenv))
              (then-type (type-of then-exp tenv))
              (else-type (type-of else-exp tenv)))
          (if (equal-types? test-type (bool-type))
              (if (equal-types? then-type else-type)
                  then-type
                  (eopl:error 'if-exp
                    "Las ramas del if deben tener el mismo tipo: ~s vs ~s"
                    (type-to-external-form then-type)
                    (type-to-external-form else-type)))
              (eopl:error 'if-exp
                "La condición del if debe ser bool, se obtuvo: ~s"
                (type-to-external-form test-type)))))

      ;; let-exp : ligaduras inmutables
      (let-exp (ids rands body)
        (let ((rand-types (map (lambda (r) (type-of r tenv)) rands)))
          (type-of body
            (extended-tenv ids rand-types
              (map (lambda (x) 'let) ids) tenv))))

      ;; var-exp : ligaduras mutables
      (var-exp (ids rands body)
        (let ((rand-types (map (lambda (r) (type-of r tenv)) rands)))
          (type-of body
            (extended-tenv ids rand-types
              (map (lambda (x) 'var) ids) tenv))))

      ;; set-exp : asignación
      (set-exp (id rhs)
        (let ((binding (apply-tenv tenv id)))
          (let ((id-type (car binding))
                (marca (cdr binding))
                (rhs-type (type-of rhs tenv)))
            (cond
              [(eq? marca 'let)
               (eopl:error 'set-exp
                 "No se puede asignar a identificador inmutable: ~s" id)]
              [(eq? marca 'frozen)
               (eopl:error 'set-exp
                 "No se puede asignar a identificador congelado: ~s" id)]
              [(not (equal-types? id-type rhs-type))
               (eopl:error 'set-exp
                 "Cambio de tipo no permitido en ~s: ~s -> ~s"
                 id
                 (type-to-external-form id-type)
                 (type-to-external-form rhs-type))]
              [else (void-type)]))))

      ;; begin-exp : secuencia retorna tipo de última expresión
      (begin-exp (first rest)
        (let ((first-type (type-of first tenv)))
          (if (null? rest)
              first-type
              (letrec ((check-sequence
                        (lambda (exps)
                          (if (null? (cdr exps))
                              (type-of (car exps) tenv)
                              (begin
                                (type-of (car exps) tenv)
                                (check-sequence (cdr exps)))))))
                (check-sequence rest)))))

      ;; freeze-exp : congela variable mutable
      (freeze-exp (id)
        (let ((binding (apply-tenv tenv id)))
          (let ((marca (cdr binding)))
            (cond
              [(eq? marca 'var)
               (void-type)]
              [(eq? marca 'frozen)
               (eopl:error 'freeze-exp
                 "La variable ~s ya está congelada" id)]
              [(eq? marca 'let)
               (eopl:error 'freeze-exp
                 "No se puede congelar identificador inmutable: ~s" id)]
              [else (eopl:error 'freeze-exp
                      "Marca desconocida: ~s" marca)]))))

      ;; proc-exp : creación de procedimiento
      (proc-exp (params body)
        ;; Asumimos que los parámetros son todos int por simplicidad
        ;; En una versión completa, necesitaríamos anotaciones de tipos
        (let ((param-types (map (lambda (x) (int-type)) params)))
          (let ((body-type
                 (type-of body
                   (extended-tenv params param-types
                     (map (lambda (x) 'let) params) tenv))))
            (proc-type param-types body-type))))

      ;; app-exp : aplicación de procedimiento
      (app-exp (rator rands)
        (let ((rator-type (type-of rator tenv))
              (rand-types (map (lambda (r) (type-of r tenv)) rands)))
          (cases tipo rator-type
            (proc-type (param-types result-type)
              (if (= (length param-types) (length rand-types))
                  (if (andmap equal-types? param-types rand-types)
                      result-type
                      (eopl:error 'app-exp
                        "Tipos de argumentos incorrectos"))
                  (eopl:error 'app-exp
                    "Aridad incorrecta: esperaba ~s, recibió ~s"
                    (length param-types) (length rand-types))))
            (else
             (eopl:error 'app-exp
               "El operador debe ser un procedimiento, se obtuvo: ~s"
               (type-to-external-form rator-type))))))

      ;; cond-exp : condicional múltiple
      (cond-exp (conditions actions default)
        (letrec ((check-conditions
                  (lambda (conds acts)
                    (if (null? conds)
                        (type-of default tenv)
                        (let ((cond-type (type-of (car conds) tenv)))
                          (if (equal-types? cond-type (bool-type))
                              (let ((act-type (type-of (car acts) tenv))
                                    (rest-type (check-conditions (cdr conds) (cdr acts))))
                                (if (equal-types? act-type rest-type)
                                    act-type
                                    (eopl:error 'cond-exp
                                      "Todas las ramas deben tener el mismo tipo")))
                              (eopl:error 'cond-exp
                                "Condición debe ser bool")))))))
          (check-conditions conditions actions)))

      ;; let*-exp : ligaduras secuenciales
      (let*-exp (ids rands body)
        (letrec ((extend-tenv-seq
                  (lambda (ids rands tenv-actual)
                    (if (null? ids)
                        (type-of body tenv-actual)
                        (let ((val-type (type-of (car rands) tenv-actual)))
                          (extend-tenv-seq
                           (cdr ids)
                           (cdr rands)
                           (extended-tenv (list (car ids))
                                         (list val-type)
                                         '(let)
                                         tenv-actual)))))))
          (extend-tenv-seq ids rands tenv)))

      ;; unless-exp : condicional inverso
      (unless-exp (test usual except)
        (let ((test-type (type-of test tenv))
              (usual-type (type-of usual tenv))
              (except-type (type-of except tenv)))
          (if (equal-types? test-type (bool-type))
              (if (equal-types? usual-type except-type)
                  usual-type
                  (eopl:error 'unless-exp
                    "Ambas ramas deben tener el mismo tipo"))
              (eopl:error 'unless-exp
                "La condición debe ser bool"))))
      )))

;; ============================================================
;; CHEQUEO DE PRIMITIVAS
;; ============================================================

;; type-of-prim : Primitiva x List<Expresion> x TypeEnv -> Tipo
;; Propósito: chequea tipos de primitivas y retorna su tipo de resultado.
(define type-of-prim
  (lambda (prim rands tenv)
    (let ((arg-types (map (lambda (r) (type-of r tenv)) rands)))
      (cases primitiva prim
        
        ;; Aritméticas n-arias: requieren int, retornan int
        (sum-prim ()
          (check-all-int arg-types "suma")
          (int-type))
        
        (minus-prim ()
          (check-all-int arg-types "resta")
          (int-type))
        
        (mult-prim ()
          (check-all-int arg-types "multiplicación")
          (int-type))
        
        (div-prim ()
          (check-all-int arg-types "división")
          (int-type))
        
        ;; Aritméticas unarias
        (add-prim ()
          (check-unary-int arg-types "add1")
          (int-type))
        
        (sub-prim ()
          (check-unary-int arg-types "sub1")
          (int-type))
        
        ;; Relacionales: requieren int, retornan bool
        (mayor-prim ()
          (check-all-int arg-types ">")
          (bool-type))
        
        (mayorigual-prim ()
          (check-all-int arg-types ">=")
          (bool-type))
        
        (menor-prim ()
          (check-all-int arg-types "<")
          (bool-type))
        
        (menorigual-prim ()
          (check-all-int arg-types "<=")
          (bool-type))
        
        (igual-prim ()
          (check-all-int arg-types "==")
          (bool-type))
        
        ;; Lógicas unarias
        (not-prim ()
          (check-unary-bool arg-types "not")
          (bool-type))
        
        ;; Lógicas binarias
        (and-prim ()
          (check-binary-bool arg-types "and")
          (bool-type))
        
        (or-prim ()
          (check-binary-bool arg-types "or")
          (bool-type))
        
        ;; Min/max: requieren int, retornan int
        (min-prim ()
          (check-all-int arg-types "min")
          (int-type))
        
        (max-prim ()
          (check-all-int arg-types "max")
          (int-type))
        
        ;; Mod y pow: binarias, requieren int, retornan int
        (mod-prim ()
          (check-binary-int arg-types "mod")
          (int-type))
        
        (pow-prim ()
          (check-binary-int arg-types "pow")
          (int-type))
        ))))

;; Funciones auxiliares para chequeo de tipos de primitivas

;; check-all-int : List<Tipo> x String -> Void
;; Propósito: verifica que todos los tipos sean int.
(define check-all-int
  (lambda (types op-name)
    (if (andmap (lambda (t) (equal-types? t (int-type))) types)
        (void)
        (eopl:error 'type-of-prim
          "~a requiere argumentos de tipo int" op-name))))

;; check-unary-int : List<Tipo> x String -> Void
;; Propósito: verifica aridad 1 y tipo int.
(define check-unary-int
  (lambda (types op-name)
    (if (= (length types) 1)
        (if (equal-types? (car types) (int-type))
            (void)
            (eopl:error 'type-of-prim
              "~a requiere argumento de tipo int" op-name))
        (eopl:error 'type-of-prim
          "~a requiere exactamente 1 argumento" op-name))))

;; check-binary-int : List<Tipo> x String -> Void
;; Propósito: verifica aridad 2 y tipos int.
(define check-binary-int
  (lambda (types op-name)
    (if (= (length types) 2)
        (if (andmap (lambda (t) (equal-types? t (int-type))) types)
            (void)
            (eopl:error 'type-of-prim
              "~a requiere dos argumentos de tipo int" op-name))
        (eopl:error 'type-of-prim
          "~a requiere exactamente 2 argumentos" op-name))))

;; check-unary-bool : List<Tipo> x String -> Void
;; Propósito: verifica aridad 1 y tipo bool.
(define check-unary-bool
  (lambda (types op-name)
    (if (= (length types) 1)
        (if (equal-types? (car types) (bool-type))
            (void)
            (eopl:error 'type-of-prim
              "~a requiere argumento de tipo bool" op-name))
        (eopl:error 'type-of-prim
          "~a requiere exactamente 1 argumento" op-name))))

;; check-binary-bool : List<Tipo> x String -> Void
;; Propósito: verifica aridad 2 y tipos bool.
(define check-binary-bool
  (lambda (types op-name)
    (if (= (length types) 2)
        (if (andmap (lambda (t) (equal-types? t (bool-type))) types)
            (void)
            (eopl:error 'type-of-prim
              "~a requiere dos argumentos de tipo bool" op-name))
        (eopl:error 'type-of-prim
          "~a requiere exactamente 2 argumentos" op-name))))

;; ============================================================
;; INTERFAZ DE USUARIO
;; ============================================================

;; check-program : String -> Tipo
;; Propósito: parsea y chequea un programa completo.
(define check-program
  (lambda (str)
    (let ((pgm (parser str)))
      (type-of-program pgm))))

;; run-check : String -> String
;; Propósito: wrapper para mostrar el tipo de forma legible.
(define run-check
  (lambda (str)
    (let ((tipo (check-program str)))
      (type-to-external-form tipo))))

;; Exportar todas las definiciones
(provide (all-defined-out))
