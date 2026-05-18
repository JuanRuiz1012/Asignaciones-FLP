#lang eopl

;; ============================================================
;; Taller 3 — Chequeador estático de tipos (MiniLang+Refs+Tipos)
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710, JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701, JUAN FELIPE RUIZ 2359397
;; ============================================================

(require eopl)
(require "interprete-minilang.rkt")


;; ------------------------------------------------------------
;; Tipos del lenguaje (EOPL §7.3)
;; ------------------------------------------------------------
;; Cinco variantes:
;;   int-type, bool-type, void-type   (atómicos)
;;   ref-type(t)                      (referencia a un tipo t)
;;   proc-type(args, res)             (procedimiento)

;; type? : Any -> Boolean
;; Propósito: predicado para el datatype de tipos.
(define-datatype type type?
  (int-type)
  (bool-type)
  (void-type)
  (ref-type
   (t type?))
  (proc-type
   (args (list-of type?))
   (res  type?)))

;; expand-type-expression : tipo-exp -> type
;; Propósito: convierte una expresión sintáctica de tipo
;; (int-type-exp, bool-type-exp, etc.) a un valor del datatype type.
(define expand-type-expression
  (lambda (texp)
    (cases tipo texp
      (int-type-exp  ()     (int-type))
      (bool-type-exp ()     (bool-type))
      (void-type-exp ()     (void-type))
      (ref-type-exp  (inner) (ref-type (expand-type-expression inner)))
      (proc-type-exp (arg-types res-type)
        (proc-type
         (map expand-type-expression arg-types)
         (expand-type-expression res-type))))))

;; type-to-external-form : type -> Any
;; Propósito: produce la representación textual del tipo para
;; usarla en mensajes de error.
;;   int-type    -> 'int
;;   bool-type   -> 'bool
;;   void-type   -> 'void
;;   ref-type(t) -> (list 'ref <t-ext>)
;;   proc-type   -> (list <t1-ext> ... '-> <tr-ext>)
(define type-to-external-form
  (lambda (t)
    (cases type t
      (int-type  () 'int)
      (bool-type () 'bool)
      (void-type () 'void)
      (ref-type (inner)
        (list 'ref (type-to-external-form inner)))
      (proc-type (arg-types res-type)
        (append
         (map type-to-external-form arg-types)
         (list '->)
         (list (type-to-external-form res-type)))))))

;; check-equal-type! : type x type x Any -> Void
;; Propósito: lanza eopl:error si los tipos no son iguales.
;; Útil para verificar ramas de if-exp, set-exp, aplicación, etc.
(define check-equal-type!
  (lambda (t1 t2 contexto)
    (if (equal? t1 t2)
        #t
        (eopl:error 'check-equal-type!
                    "Tipos incompatibles en ~s~%  esperado: ~s~%  recibido: ~s"
                    contexto
                    (type-to-external-form t1)
                    (type-to-external-form t2)))))


;; ------------------------------------------------------------
;; Ambiente de tipos
;; ------------------------------------------------------------
;; Liga identificadores con un par (tipo, mutabilidad).
;; La mutabilidad se necesita para rechazar set y freeze sobre
;; identificadores inmutables o ya congelados.

;; type-environment? : Any -> Boolean
;; Propósito: predicado para el ambiente de tipos.
(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv
   (ids   (list-of symbol?))
   (types (list-of type?))
   (marks (list-of symbol?))   ;; marca de cada id: 'let o 'var
   (tenv  type-environment?)))

;; apply-tenv : type-environment x Symbol -> (cons type Symbol)
;; Propósito: busca el par (tipo, marca) ligado a id.
;; Retorna un par (type . mark). Si no está, lanza eopl:error.
(define apply-tenv
  (lambda (tenv id)
    (cases type-environment tenv
      (empty-tenv ()
        (eopl:error 'apply-tenv "Variable no declarada: ~s" id))
      (extend-tenv (ids types marks old-tenv)
        (letrec
            ((buscar
              (lambda (ids types marks)
                (cond
                  [(null? ids) (apply-tenv old-tenv id)]
                  [(equal? (car ids) id)
                   (cons (car types) (car marks))]
                  [else
                   (buscar (cdr ids) (cdr types) (cdr marks))]))))
          (buscar ids types marks))))))

;; tenv-inicial : () -> type-environment
;; Propósito: ambiente de tipos compatible con el ambiente inicial
;; del intérprete:
;;   x, y, z : int (marca 'let — inmutables)
;;   a, b, c : int (marca 'var — mutables)
(define tenv-inicial
  (lambda ()
    (extend-tenv
     '(a b c)
     (list (int-type) (int-type) (int-type))
     '(var var var)
     (extend-tenv
      '(x y z)
      (list (int-type) (int-type) (int-type))
      '(let let let)
      (empty-tenv)))))


;; ------------------------------------------------------------
;; Chequeador
;; ------------------------------------------------------------

;; type-of-program : Programa -> type
;; Propósito: chequea un programa completo y retorna su tipo.
;; Si el programa está mal tipado, lanza eopl:error con un mensaje claro.
(define type-of-program
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp)
        (type-of-expression exp (tenv-inicial))))))

;; type-of-expression : Expresion x type-environment -> type
;; Propósito: implementa las reglas de tipado del lenguaje.
;; Cubre todos los casos de la gramática.
(define type-of-expression
  (lambda (exp tenv)
    (cases expresion exp

      ;; lit-exp : siempre int
      (lit-exp (dato) (int-type))

      ;; true-exp / false-exp : siempre bool
      (true-exp  () (bool-type))
      (false-exp () (bool-type))

      ;; ident-exp : el tipo registrado en el ambiente
      (ident-exp (id)
        (car (apply-tenv tenv id)))

      ;; prim-exp : delega en type-of-primitive
      (prim-exp (prim rands)
        (let ((arg-types (map (lambda (r) (type-of-expression r tenv)) rands)))
          (type-of-primitive prim arg-types)))

      ;; if-exp : condición debe ser bool; ramas deben tener el mismo tipo
      (if-exp (test true-branch false-branch)
        (let ((t-test (type-of-expression test tenv)))
          (check-equal-type! t-test (bool-type) 'if-exp:condicion)
          (let ((t-true  (type-of-expression true-branch tenv))
                (t-false (type-of-expression false-branch tenv)))
            (check-equal-type! t-true t-false 'if-exp:ramas)
            t-true)))

      ;; let-exp : ligaduras inmutables (marca 'let)
      ;; Las expresiones se tipa con los tipos declarados.
      (let-exp (ids type-exps rands body)
        (let ((declared-types (map expand-type-expression type-exps))
              (actual-types   (map (lambda (r) (type-of-expression r tenv)) rands)))
          ;; Verificar que cada valor coincide con su tipo declarado
          (for-each
           (lambda (declared actual id)
             (check-equal-type! declared actual
                                (string->symbol (string-append "let:" (symbol->string id)))))
           declared-types actual-types ids)
          (type-of-expression
           body
           (extend-tenv ids declared-types (map (lambda (_) 'let) ids) tenv))))

      ;; var-decl-exp : ligaduras mutables (marca 'var)
      (var-decl-exp (ids type-exps rands body)
        (let ((declared-types (map expand-type-expression type-exps))
              (actual-types   (map (lambda (r) (type-of-expression r tenv)) rands)))
          (for-each
           (lambda (declared actual id)
             (check-equal-type! declared actual
                                (string->symbol (string-append "var:" (symbol->string id)))))
           declared-types actual-types ids)
          (type-of-expression
           body
           (extend-tenv ids declared-types (map (lambda (_) 'var) ids) tenv))))

      ;; set-exp : asignación
      ;; La variable debe estar ligada con marca 'var.
      ;; La expresión debe tener el mismo tipo que la variable.
      ;; Retorna void.
      (set-exp (id rand)
        (let* ((entry  (apply-tenv tenv id))
               (t-var  (car entry))
               (marca  (cdr entry))
               (t-exp  (type-of-expression rand tenv)))
          (cond
            [(eq? marca 'let)
             (eopl:error 'set-exp
                         "No se puede asignar a variable inmutable (let): ~s"
                         id)]
            [(eq? marca 'frozen)
             (eopl:error 'set-exp
                         "No se puede asignar a variable congelada: ~s"
                         id)]
            [else
             (check-equal-type! t-var t-exp
                                (string->symbol (string-append "set:" (symbol->string id))))
             (void-type)])))

      ;; begin-exp : tipo de la última expresión
      (begin-exp (first rest)
        (let loop ((t (type-of-expression first tenv))
                   (exprs rest))
          (if (null? exprs)
              t
              (loop (type-of-expression (car exprs) tenv)
                    (cdr exprs)))))

      ;; freeze-exp : congela una variable mutable
      ;; Solo procede si la marca es 'var. Retorna void.
      (freeze-exp (id)
        (let* ((entry (apply-tenv tenv id))
               (marca (cdr entry)))
          (cond
            [(eq? marca 'let)
             (eopl:error 'freeze-exp
                         "No se puede congelar una variable inmutable (let): ~s"
                         id)]
            [(eq? marca 'frozen)
             (eopl:error 'freeze-exp
                         "La variable ya está congelada: ~s"
                         id)]
            [else (void-type)])))

      ;; proc-exp : crea un tipo procedimiento
      ;; Los parámetros se ligan con marca 'let en el cuerpo.
      (proc-exp (type-exps ids body)
        (let ((arg-types (map expand-type-expression type-exps)))
          (let ((t-body
                 (type-of-expression
                  body
                  (extend-tenv ids arg-types
                               (map (lambda (_) 'let) ids)
                               tenv))))
            (proc-type arg-types t-body))))

      ;; app-exp : aplicación de procedimiento
      ;; El operador debe ser de tipo proc-type; verifica aridad y tipos.
      (app-exp (rator rands)
        (let ((t-rator (type-of-expression rator tenv))
              (t-rands (map (lambda (r) (type-of-expression r tenv)) rands)))
          (cases type t-rator
            (proc-type (param-types res-type)
              (if (= (length param-types) (length t-rands))
                  (begin
                    (for-each
                     (lambda (expected actual)
                       (check-equal-type! expected actual 'app-exp:argumento))
                     param-types t-rands)
                    res-type)
                  (eopl:error 'app-exp
                              "Aridad incorrecta: esperaba ~s argumentos, recibió ~s"
                              (length param-types) (length t-rands))))
            (else
             (eopl:error 'app-exp
                         "Se esperaba un procedimiento, recibió tipo: ~s"
                         (type-to-external-form t-rator))))))

      ;; cond-exp : todas las condiciones bool, todas las acciones mismo tipo
      (cond-exp (conditions actions default)
        (for-each
         (lambda (c)
           (check-equal-type! (type-of-expression c tenv) (bool-type)
                              'cond-exp:condicion))
         conditions)
        (let ((t-default (type-of-expression default tenv)))
          (for-each
           (lambda (a)
             (check-equal-type! (type-of-expression a tenv) t-default
                                'cond-exp:accion))
           actions)
          t-default))

      ;; let*-exp : ligaduras secuenciales sin tipo (del T2, marca 'let)
      (let*-exp (ids rands body)
        (letrec
            ((chequear-secuencial
              (lambda (ids rands tenv-actual)
                (if (null? ids)
                    (type-of-expression body tenv-actual)
                    (let ((t-val (type-of-expression (car rands) tenv-actual)))
                      (chequear-secuencial
                       (cdr ids)
                       (cdr rands)
                       (extend-tenv (list (car ids))
                                    (list t-val)
                                    '(let)
                                    tenv-actual)))))))
          (chequear-secuencial ids rands tenv)))

      ;; unless-exp : condición bool, ramas mismo tipo
      (unless-exp (test usual except)
        (check-equal-type! (type-of-expression test tenv) (bool-type)
                           'unless-exp:condicion)
        (let ((t-usual  (type-of-expression usual tenv))
              (t-except (type-of-expression except tenv)))
          (check-equal-type! t-usual t-except 'unless-exp:ramas)
          t-usual))
      )))



;; type-of-primitive : Primitiva x (Listof type) -> type
;; Propósito: chequea los tipos de los operandos según la signatura
;; de cada primitiva y retorna el tipo resultado.
(define type-of-primitive
  (lambda (prim arg-types)
    (cases primitiva prim

      ;; Primitivas aritméticas n-arias: (int ... -> int)
      (sum-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'sum-prim)) arg-types)
        (int-type))
      (minus-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'minus-prim)) arg-types)
        (int-type))
      (mult-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'mult-prim)) arg-types)
        (int-type))
      (div-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'div-prim)) arg-types)
        (int-type))
      (min-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'min-prim)) arg-types)
        (int-type))
      (max-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'max-prim)) arg-types)
        (int-type))

      ;; Primitivas unarias: (int -> int)
      (add-prim ()
        (if (= (length arg-types) 1)
            (begin (check-equal-type! (car arg-types) (int-type) 'add1-prim)
                   (int-type))
            (eopl:error 'add-prim "add1 es unaria")))
      (sub-prim ()
        (if (= (length arg-types) 1)
            (begin (check-equal-type! (car arg-types) (int-type) 'sub1-prim)
                   (int-type))
            (eopl:error 'sub-prim "sub1 es unaria")))

      ;; Primitivas binarias int: (int int -> int)
      (mod-prim ()
        (if (= (length arg-types) 2)
            (begin
              (check-equal-type! (car  arg-types) (int-type) 'mod-prim:arg1)
              (check-equal-type! (cadr arg-types) (int-type) 'mod-prim:arg2)
              (int-type))
            (eopl:error 'mod-prim "mod requiere 2 argumentos")))
      (pow-prim ()
        (if (= (length arg-types) 2)
            (begin
              (check-equal-type! (car  arg-types) (int-type) 'pow-prim:arg1)
              (check-equal-type! (cadr arg-types) (int-type) 'pow-prim:arg2)
              (int-type))
            (eopl:error 'pow-prim "pow requiere 2 argumentos")))

      ;; Primitivas relacionales n-arias: (int ... -> bool)
      (mayor-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'mayor-prim)) arg-types)
        (bool-type))
      (mayorigual-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'mayorigual-prim)) arg-types)
        (bool-type))
      (menor-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'menor-prim)) arg-types)
        (bool-type))
      (menorigual-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'menorigual-prim)) arg-types)
        (bool-type))
      (igual-prim ()
        (for-each (lambda (t) (check-equal-type! t (int-type) 'igual-prim)) arg-types)
        (bool-type))

      ;; not-prim : (bool -> bool)
      (not-prim ()
        (if (= (length arg-types) 1)
            (begin (check-equal-type! (car arg-types) (bool-type) 'not-prim)
                   (bool-type))
            (eopl:error 'not-prim "not es unaria")))

      ;; and-prim / or-prim : (bool bool -> bool)
      (and-prim ()
        (if (= (length arg-types) 2)
            (begin
              (check-equal-type! (car  arg-types) (bool-type) 'and-prim:arg1)
              (check-equal-type! (cadr arg-types) (bool-type) 'and-prim:arg2)
              (bool-type))
            (eopl:error 'and-prim "and requiere 2 argumentos")))
      (or-prim ()
        (if (= (length arg-types) 2)
            (begin
              (check-equal-type! (car  arg-types) (bool-type) 'or-prim:arg1)
              (check-equal-type! (cadr arg-types) (bool-type) 'or-prim:arg2)
              (bool-type))
            (eopl:error 'or-prim "or requiere 2 argumentos")))
      )))


(provide (all-defined-out))
