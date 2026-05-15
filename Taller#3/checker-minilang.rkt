#lang eopl

;; ============================================================
;; Taller 3 — Chequeador estático de tipos (MiniLang+Refs+Tipos)
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710, JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701, JUAN FELIPE RUIZ 2359397
;; ============================================================

(require eopl)
;; Reutilizamos gramática, datatypes, parser y utilidades del intérprete.
;; NOTA: SLLGEN genera automáticamente un datatype llamado 'tipo' con variantes
;;       int-type-exp, bool-type-exp, etc. (tipo SINTÁCTICO del AST).
;;       El tipo SEMÁNTICO del chequeador se llama 'tipo-sem' para evitar
;;       colisión de nombres al hacer (require "interprete-minilang.rkt").
(require "interprete-minilang.rkt")

;; ============================================================
;; TIPOS SEMÁNTICOS DEL CHEQUEADOR
;; ============================================================

;; define-datatype tipo-sem tipo-sem?
;; Propósito: representa los tipos de MiniLang a nivel semántico.
;;   int-sem, bool-sem, void-sem   (atómicos)
;;   ref-sem(inner)                referencia a un tipo
;;   proc-sem(params, result)      tipo de procedimiento
(define-datatype tipo-sem tipo-sem?
  (int-sem)
  (bool-sem)
  (void-sem)
  (ref-sem
   (inner-sem tipo-sem?))
  (proc-sem
   (param-sems (list-of tipo-sem?))
   (result-sem tipo-sem?)))

;; equal-types? : tipo-sem x tipo-sem -> Boolean
;; Propósito: compara estructuralmente dos tipos semánticos.
(define equal-types?
  (lambda (t1 t2)
    (cases tipo-sem t1
      (int-sem  ()
        (cases tipo-sem t2 (int-sem  () #t) (else #f)))
      (bool-sem ()
        (cases tipo-sem t2 (bool-sem () #t) (else #f)))
      (void-sem ()
        (cases tipo-sem t2 (void-sem () #t) (else #f)))
      (ref-sem (inner1)
        (cases tipo-sem t2
          (ref-sem (inner2) (equal-types? inner1 inner2))
          (else #f)))
      (proc-sem (params1 result1)
        (cases tipo-sem t2
          (proc-sem (params2 result2)
            (and (= (length params1) (length params2))
                 (andmap equal-types? params1 params2)
                 (equal-types? result1 result2)))
          (else #f))))))

;; type-to-external-form : tipo-sem -> S-exp
;; Propósito: produce la representación textual del tipo para mensajes y pruebas.
;;   int-sem   → 'int
;;   bool-sem  → 'bool
;;   void-sem  → 'void
;;   ref-sem   → (ref <t>)
;;   proc-sem  → ((<t1> ... <tn>) -> <tr>)
(define type-to-external-form
  (lambda (ty)
    (cases tipo-sem ty
      (int-sem  () 'int)
      (bool-sem () 'bool)
      (void-sem () 'void)
      (ref-sem (inner)
        (list 'ref (type-to-external-form inner)))
      (proc-sem (params result)
        (list (map type-to-external-form params)
              '->
              (type-to-external-form result))))))

;; expand-type-expression : tipo (AST SLLGEN) -> tipo-sem
;; Propósito: convierte el tipo SINTÁCTICO generado por SLLGEN
;;   (variantes int-type-exp, bool-type-exp, etc.) al tipo SEMÁNTICO
;;   del chequeador (variantes int-sem, bool-sem, etc.).
(define expand-type-expression
  (lambda (texp)
    (cases tipo texp
      (int-type-exp  () (int-sem))
      (bool-type-exp () (bool-sem))
      (void-type-exp () (void-sem))
      (ref-type-exp  (inner)
        (ref-sem (expand-type-expression inner)))
      (proc-type-exp (param-texps result-texp)
        (proc-sem
         (map expand-type-expression param-texps)
         (expand-type-expression result-texp))))))

;; ============================================================
;; AMBIENTE DE TIPOS
;; ============================================================

;; define-datatype type-environment type-environment?
;; Propósito: liga identificadores con (tipo-sem, marca-mutabilidad).
;;   La marca ('let o 'var) permite rechazar set/freeze sobre inmutables.
(define-datatype type-environment type-environment?
  (empty-tenv)
  (extended-tenv
   (ids    (list-of symbol?))
   (types  (list-of tipo-sem?))
   (marcas (list-of symbol?))
   (env    type-environment?)))

;; apply-tenv : type-environment x Symbol -> (tipo-sem . Symbol)
;; Propósito: retorna el par (tipo-sem, marca) ligado a id. Error si no existe.
(define apply-tenv
  (lambda (tenv id)
    (cases type-environment tenv
      (empty-tenv ()
        (eopl:error 'apply-tenv "Variable no ligada en ambiente de tipos: ~s" id))
      (extended-tenv (ids types marcas old-tenv)
        (letrec
            ((buscar
              (lambda (ids types marcas)
                (cond
                  [(null? ids) (apply-tenv old-tenv id)]
                  [(equal? (car ids) id) (cons (car types) (car marcas))]
                  [else (buscar (cdr ids) (cdr types) (cdr marcas))]))))
          (buscar ids types marcas))))))

;; init-tenv : () -> type-environment
;; Propósito: ambiente de tipos inicial compatible con ambiente-inicial del intérprete.
;;   x, y, z : int (marca 'let — inmutables)
;;   a, b, c : int (marca 'var — mutables)
(define init-tenv
  (lambda ()
    (extended-tenv
     '(x y z a b c)
     (list (int-sem) (int-sem) (int-sem)
           (int-sem) (int-sem) (int-sem))
     '(let let let var var var)
     (empty-tenv))))

;; ============================================================
;; CHEQUEADOR DE TIPOS
;; ============================================================

;; type-of-program : Programa -> tipo-sem
;; Propósito: punto de entrada del chequeador. Retorna el tipo semántico del programa.
(define type-of-program
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp)
        (type-of exp (init-tenv))))))

;; type-of : Expresion x type-environment -> tipo-sem
;; Propósito: implementa las reglas de tipado de MiniLang+Refs.
(define type-of
  (lambda (exp tenv)
    (cases expresion exp

      ;; lit-exp: todo número tiene tipo int
      (lit-exp (n) (int-sem))

      ;; ident-exp: tipo semántico declarado en el ambiente
      (ident-exp (id)
        (car (apply-tenv tenv id)))

      (true-exp  () (bool-sem))
      (false-exp () (bool-sem))

      ;; prim-exp: delega en type-of-prim
      (prim-exp (prim rands)
        (type-of-prim prim rands tenv))

      ;; if-exp: condición bool, ramas del mismo tipo
      (if-exp (test then-exp else-exp)
        (let ((test-type (type-of test tenv))
              (then-type (type-of then-exp tenv))
              (else-type (type-of else-exp tenv)))
          (if (equal-types? test-type (bool-sem))
              (if (equal-types? then-type else-type)
                  then-type
                  (eopl:error 'if-exp
                    "Las ramas del if deben tener el mismo tipo: ~s vs ~s"
                    (type-to-external-form then-type)
                    (type-to-external-form else-type)))
              (eopl:error 'if-exp
                "La condición del if debe ser bool, se obtuvo: ~s"
                (type-to-external-form test-type)))))

      ;; let-exp: ids, tipos-decl (AST sintáctico), rands, body
      ;;   Verifica tipo declarado == tipo inferido.
      ;;   Extiende tenv con (tipo-sem, 'let).
      (let-exp (ids tipos-decl rands body)
        (let ((rand-types (map (lambda (r) (type-of r tenv)) rands))
              (decl-types (map expand-type-expression tipos-decl)))
          (for-each
           (lambda (decl inf id)
             (if (not (equal-types? decl inf))
                 (eopl:error 'let-exp
                   "Tipo declarado ~s no coincide con tipo inferido ~s en ~s"
                   (type-to-external-form decl)
                   (type-to-external-form inf)
                   id)
                 (void-value)))
           decl-types rand-types ids)
          (type-of body
            (extended-tenv ids decl-types
              (map (lambda (x) 'let) ids) tenv))))

      ;; var-exp: igual que let-exp pero marca 'var (mutable)
      (var-exp (ids tipos-decl rands body)
        (let ((rand-types (map (lambda (r) (type-of r tenv)) rands))
              (decl-types (map expand-type-expression tipos-decl)))
          (for-each
           (lambda (decl inf id)
             (if (not (equal-types? decl inf))
                 (eopl:error 'var-exp
                   "Tipo declarado ~s no coincide con tipo inferido ~s en ~s"
                   (type-to-external-form decl)
                   (type-to-external-form inf)
                   id)
                 (void-value)))
           decl-types rand-types ids)
          (type-of body
            (extended-tenv ids decl-types
              (map (lambda (x) 'var) ids) tenv))))

      ;; set-exp: solo sobre 'var, mismo tipo, retorna void-sem
      (set-exp (id rhs)
        (let* ((binding  (apply-tenv tenv id))
               (id-type  (car binding))
               (marca    (cdr binding))
               (rhs-type (type-of rhs tenv)))
          (cond
            [(eq? marca 'let)
             (eopl:error 'set-exp
               "No se puede asignar a identificador inmutable (let): ~s" id)]
            [(eq? marca 'frozen)
             (eopl:error 'set-exp
               "No se puede asignar a identificador congelado: ~s" id)]
            [(not (equal-types? id-type rhs-type))
             (eopl:error 'set-exp
               "Cambio de tipo no permitido en ~s: ~s -> ~s"
               id
               (type-to-external-form id-type)
               (type-to-external-form rhs-type))]
            [else (void-sem)])))

      ;; begin-exp: chequea todas, retorna tipo de la última expresión
      (begin-exp (first rest)
        (if (null? rest)
            (type-of first tenv)
            (begin
              (type-of first tenv)
              (letrec ((check-seq
                        (lambda (exps)
                          (if (null? (cdr exps))
                              (type-of (car exps) tenv)
                              (begin
                                (type-of (car exps) tenv)
                                (check-seq (cdr exps)))))))
                (check-seq rest)))))

      ;; freeze-exp: solo sobre 'var, retorna void-sem
      (freeze-exp (id)
        (let* ((binding (apply-tenv tenv id))
               (marca   (cdr binding)))
          (cond
            [(eq? marca 'var)    (void-sem)]
            [(eq? marca 'frozen)
             (eopl:error 'freeze-exp
               "La variable ~s ya está congelada" id)]
            [(eq? marca 'let)
             (eopl:error 'freeze-exp
               "No se puede congelar identificador inmutable (let): ~s" id)]
            [else
             (eopl:error 'freeze-exp "Marca desconocida: ~s" marca)])))

      ;; proc-exp: tipos-params (AST sintáctico), ids, body
      ;;   Parámetros con marca 'let (inmutables dentro del cuerpo).
      (proc-exp (tipos-params ids body)
        (let* ((param-sems (map expand-type-expression tipos-params))
               (body-type
                (type-of body
                  (extended-tenv ids param-sems
                    (map (lambda (x) 'let) ids) tenv))))
          (proc-sem param-sems body-type)))

      ;; app-exp: operador debe ser proc-sem, aridad y tipos correctos
      (app-exp (rator rands)
        (let ((rator-type (type-of rator tenv))
              (rand-types (map (lambda (r) (type-of r tenv)) rands)))
          (cases tipo-sem rator-type
            (proc-sem (param-sems result-sem-val)
              (if (= (length param-sems) (length rand-types))
                  (if (andmap equal-types? param-sems rand-types)
                      result-sem-val
                      (eopl:error 'app-exp
                        "Tipos de argumentos incorrectos en aplicación"))
                  (eopl:error 'app-exp
                    "Aridad incorrecta: esperaba ~s, recibió ~s"
                    (length param-sems) (length rand-types))))
            (else
             (eopl:error 'app-exp
               "El operador debe ser un procedimiento, se obtuvo: ~s"
               (type-to-external-form rator-type))))))

      ;; cond-exp: todas las ramas deben tener el mismo tipo
      (cond-exp (conditions actions default)
        (letrec ((check-conds
                  (lambda (conds acts)
                    (if (null? conds)
                        (type-of default tenv)
                        (let ((ct (type-of (car conds) tenv)))
                          (if (equal-types? ct (bool-sem))
                              (let ((at (type-of (car acts) tenv))
                                    (rt (check-conds (cdr conds) (cdr acts))))
                                (if (equal-types? at rt)
                                    at
                                    (eopl:error 'cond-exp
                                      "Todas las ramas deben tener el mismo tipo")))
                              (eopl:error 'cond-exp
                                "Condición debe ser bool, se obtuvo: ~s"
                                (type-to-external-form ct))))))))
          (check-conds conditions actions)))

      ;; let*-exp: secuencial, cada id se agrega al tenv antes del siguiente
      (let*-exp (ids tipos-decl rands body)
        (letrec ((ext-seq
                  (lambda (ids tipos rands tenv-act)
                    (if (null? ids)
                        (type-of body tenv-act)
                        (let* ((vt   (type-of (car rands) tenv-act))
                               (decl (expand-type-expression (car tipos))))
                          (if (not (equal-types? decl vt))
                              (eopl:error 'let*-exp
                                "Tipo declarado ~s no coincide con inferido ~s en ~s"
                                (type-to-external-form decl)
                                (type-to-external-form vt)
                                (car ids))
                              (ext-seq
                               (cdr ids) (cdr tipos) (cdr rands)
                               (extended-tenv (list (car ids)) (list decl)
                                             '(let) tenv-act))))))))
          (ext-seq ids tipos-decl rands tenv)))

      ;; unless-exp: condición bool, ramas del mismo tipo
      (unless-exp (test usual except)
        (let ((test-type   (type-of test tenv))
              (usual-type  (type-of usual tenv))
              (except-type (type-of except tenv)))
          (if (equal-types? test-type (bool-sem))
              (if (equal-types? usual-type except-type)
                  usual-type
                  (eopl:error 'unless-exp
                    "Ambas ramas deben tener el mismo tipo: ~s vs ~s"
                    (type-to-external-form usual-type)
                    (type-to-external-form except-type)))
              (eopl:error 'unless-exp
                "La condición debe ser bool, se obtuvo: ~s"
                (type-to-external-form test-type)))))
      )))

;; ============================================================
;; CHEQUEO DE PRIMITIVAS
;; ============================================================

;; type-of-prim : Primitiva x List<Expresion> x type-environment -> tipo-sem
;; Propósito: infiere tipos de argumentos y verifica signaturas de cada primitiva.
(define type-of-prim
  (lambda (prim rands tenv)
    (let ((arg-types (map (lambda (r) (type-of r tenv)) rands)))
      (cases primitiva prim
        (sum-prim        () (check-all-int     arg-types "+")   (int-sem))
        (minus-prim      () (check-all-int     arg-types "-")   (int-sem))
        (mult-prim       () (check-all-int     arg-types "*")   (int-sem))
        (div-prim        () (check-all-int     arg-types "/")   (int-sem))
        (add-prim        () (check-unary-int   arg-types "add1")(int-sem))
        (sub-prim        () (check-unary-int   arg-types "sub1")(int-sem))
        (mayor-prim      () (check-all-int     arg-types ">")   (bool-sem))
        (mayorigual-prim () (check-all-int     arg-types ">=")  (bool-sem))
        (menor-prim      () (check-all-int     arg-types "<")   (bool-sem))
        (menorigual-prim () (check-all-int     arg-types "<=")  (bool-sem))
        (igual-prim      () (check-all-int     arg-types "==")  (bool-sem))
        (not-prim        () (check-unary-bool  arg-types "not") (bool-sem))
        (and-prim        () (check-binary-bool arg-types "and") (bool-sem))
        (or-prim         () (check-binary-bool arg-types "or")  (bool-sem))
        (min-prim        () (check-all-int     arg-types "min") (int-sem))
        (max-prim        () (check-all-int     arg-types "max") (int-sem))
        (mod-prim        () (check-binary-int  arg-types "mod") (int-sem))
        (pow-prim        () (check-binary-int  arg-types "pow") (int-sem))
        ))))

;; ============================================================
;; AUXILIARES DE CHEQUEO
;; ============================================================

;; check-all-int : List<tipo-sem> x String -> Void
;; Propósito: verifica que todos los tipos sean int-sem. Error si no.
(define check-all-int
  (lambda (types op-name)
    (if (andmap (lambda (t) (equal-types? t (int-sem))) types)
        (void-value)
        (eopl:error 'type-of-prim
          "~a requiere argumentos de tipo int" op-name))))

;; check-unary-int : List<tipo-sem> x String -> Void
;; Propósito: verifica exactamente 1 argumento de tipo int-sem.
(define check-unary-int
  (lambda (types op-name)
    (if (= (length types) 1)
        (if (equal-types? (car types) (int-sem))
            (void-value)
            (eopl:error 'type-of-prim
              "~a requiere argumento de tipo int" op-name))
        (eopl:error 'type-of-prim
          "~a requiere exactamente 1 argumento" op-name))))

;; check-binary-int : List<tipo-sem> x String -> Void
;; Propósito: verifica exactamente 2 argumentos de tipo int-sem.
(define check-binary-int
  (lambda (types op-name)
    (if (= (length types) 2)
        (if (andmap (lambda (t) (equal-types? t (int-sem))) types)
            (void-value)
            (eopl:error 'type-of-prim
              "~a requiere dos argumentos de tipo int" op-name))
        (eopl:error 'type-of-prim
          "~a requiere exactamente 2 argumentos" op-name))))

;; check-unary-bool : List<tipo-sem> x String -> Void
;; Propósito: verifica exactamente 1 argumento de tipo bool-sem.
(define check-unary-bool
  (lambda (types op-name)
    (if (= (length types) 1)
        (if (equal-types? (car types) (bool-sem))
            (void-value)
            (eopl:error 'type-of-prim
              "~a requiere argumento de tipo bool" op-name))
        (eopl:error 'type-of-prim
          "~a requiere exactamente 1 argumento" op-name))))

;; check-binary-bool : List<tipo-sem> x String -> Void
;; Propósito: verifica exactamente 2 argumentos de tipo bool-sem.
(define check-binary-bool
  (lambda (types op-name)
    (if (= (length types) 2)
        (if (andmap (lambda (t) (equal-types? t (bool-sem))) types)
            (void-value)
            (eopl:error 'type-of-prim
              "~a requiere dos argumentos de tipo bool" op-name))
        (eopl:error 'type-of-prim
          "~a requiere exactamente 2 argumentos" op-name))))

;; ============================================================
;; INTERFAZ DE USUARIO
;; ============================================================

;; check-program : String -> tipo-sem
;; Propósito: parsea y chequea estáticamente un programa fuente.
(define check-program
  (lambda (str)
    (type-of-program (parser str))))

;; run-check : String -> S-exp
;; Propósito: retorna la forma externa del tipo semántico (para pruebas).
(define run-check
  (lambda (str)
    (type-to-external-form (check-program str))))

(provide (all-defined-out))
