#lang eopl

;; ============================================================
;; Taller 3 — Asignación y chequeo de tipos (MiniLang+Refs)
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710, JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701, JUAN FELIPE RUIZ 2359397
;; ============================================================

(require eopl)

;; ============================================================
;; ESPECIFICACIÓN LÉXICA Y GRAMATICAL
;; ============================================================

;; especificacion-lexica : List
;; Propósito: define los tokens del lenguaje MiniLang extendido.
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

;; especificacion-gramatical : List
;; Propósito: define la gramática BNF de MiniLang con referencias y procedimientos.
(define especificacion-gramatical
  '(
    ; Programa
    (programa (expresion) a-program)

    ; Expresiones base
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
    
    ; let inmutable
    (expresion
     ("let" (arbno identificador "=" expresion) "in" expresion)
     let-exp)
    
    ; var mutable (NUEVO)
    (expresion
     ("var" (arbno identificador "=" expresion) "in" expresion)
     var-exp)
    
    ; set asignación (NUEVO)
    (expresion
     ("set" identificador "=" expresion)
     set-exp)
    
    ; begin secuenciación (NUEVO)
    (expresion
     ("begin" expresion (arbno ";" expresion) "end")
     begin-exp)
    
    ; freeze congelación (NUEVO)
    (expresion
     ("freeze" identificador)
     freeze-exp)

    ; Procedimientos (NUEVO)
    (expresion
     ("proc" "(" (separated-list identificador ",") ")" expresion)
     proc-exp)
    
    (expresion
     ("(" expresion (arbno expresion) ")")
     app-exp)

    ; Expresiones adicionales del Taller 2
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

;; Genera automáticamente los define-datatype
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; scanner : String -> List-of-tokens
;; Propósito: convierte código fuente en tokens.
(define scanner
  (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))

;; parser : String -> Programa
;; Propósito: convierte código fuente en AST.
(define parser
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; ============================================================
;; TAD REFERENCIA (EOPL §4.2)
;; ============================================================

;; La memoria se implementa como un vector que crece dinámicamente
(define the-store 'uninitialized)

;; empty-store : () -> Store
;; Propósito: inicializa la memoria como vector vacío.
(define empty-store
  (lambda () (make-vector 0)))

;; initialize-store! : () -> Unspecified
;; Propósito: reinicia la memoria global.
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;; get-store : () -> Store
;; Propósito: obtiene la memoria actual.
(define get-store
  (lambda () the-store))

;; reference? : Any -> Boolean
;; Propósito: verifica si un valor es una referencia válida.
(define reference?
  (lambda (v)
    (and (integer? v) (>= v 0) (< v (vector-length the-store)))))

;; newref : ExpVal x Symbol -> Ref
;; Propósito: crea una nueva referencia con valor y marca de mutabilidad.
;; Marca puede ser: 'let (inmutable), 'var (mutable), 'frozen (congelada)
(define newref
  (lambda (val marca)
    (let* ((next-ref (vector-length the-store))
           (new-store (make-vector (+ next-ref 1))))
      (letrec ((copy-store
                (lambda (i)
                  (if (< i next-ref)
                      (begin
                        (vector-set! new-store i (vector-ref the-store i))
                        (copy-store (+ i 1)))
                      (void)))))
        (copy-store 0))
      (vector-set! new-store next-ref (cons marca val))
      (set! the-store new-store)
      next-ref)))

;; deref : Ref -> ExpVal
;; Propósito: obtiene el valor almacenado en una referencia.
(define deref
  (lambda (ref)
    (if (reference? ref)
        (cdr (vector-ref the-store ref))
        (eopl:error 'deref "Referencia inválida: ~s" ref))))

;; setref! : Ref x ExpVal -> Void
;; Propósito: modifica el valor de una referencia mutable.
;; Lanza error si la marca es 'let o 'frozen.
(define setref!
  (lambda (ref val)
    (if (reference? ref)
        (let ((cell (vector-ref the-store ref)))
          (let ((marca (car cell)))
            (cond
              [(eq? marca 'var)
               (vector-set! the-store ref (cons 'var val))]
              [(eq? marca 'let)
               (eopl:error 'setref!
                 "No se puede asignar a identificador inmutable")]
              [(eq? marca 'frozen)
               (eopl:error 'setref!
                 "No se puede asignar a identificador congelado")]
              [else
               (eopl:error 'setref!
                 "Marca de mutabilidad desconocida: ~s" marca)])))
        (eopl:error 'setref! "Referencia inválida: ~s" ref))))

;; get-marca : Ref -> Symbol
;; Propósito: obtiene la marca de mutabilidad de una referencia.
(define get-marca
  (lambda (ref)
    (if (reference? ref)
        (car (vector-ref the-store ref))
        (eopl:error 'get-marca "Referencia inválida: ~s" ref))))

;; freeze-ref! : Ref -> Void
;; Propósito: congela una referencia mutable (var -> frozen).
;; Lanza error si ya es 'let o 'frozen.
(define freeze-ref!
  (lambda (ref)
    (if (reference? ref)
        (let ((cell (vector-ref the-store ref)))
          (let ((marca (car cell))
                (val (cdr cell)))
            (cond
              [(eq? marca 'var)
               (vector-set! the-store ref (cons 'frozen val))]
              [(eq? marca 'frozen)
               (eopl:error 'freeze-ref!
                 "La variable ya está congelada")]
              [(eq? marca 'let)
               (eopl:error 'freeze-ref!
                 "No se puede congelar un identificador inmutable (let)")]
              [else
               (eopl:error 'freeze-ref!
                 "Marca de mutabilidad desconocida: ~s" marca)])))
        (eopl:error 'freeze-ref! "Referencia inválida: ~s" ref))))

;; ============================================================
;; AMBIENTE CON REFERENCIAS
;; ============================================================

;; El ambiente ahora liga identificadores con referencias, no con valores directos
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (ids  (list-of symbol?))
   (refs (list-of reference?))
   (env  ambiente?)))

;; scheme-value? : Any -> Boolean
;; Propósito: predicado para valores expresados.
;; Ahora incluye void para efectos laterales.
(define scheme-value?
  (lambda (v)
    (or (number? v) (boolean? v) (eq? v 'void) (procedimiento? v))))

;; apply-env-ref : Ambiente x Symbol -> Ref
;; Propósito: busca la referencia asociada a un identificador.
(define apply-env-ref
  (lambda (env id)
    (cases ambiente env
      (ambiente-vacio ()
        (eopl:error 'apply-env-ref "Variable no ligada: ~s" id))
      (ambiente-extendido (ids refs old-env)
        (letrec
            ((buscar
              (lambda (ids refs)
                (cond
                  [(null? ids) (apply-env-ref old-env id)]
                  [(equal? (car ids) id) (car refs)]
                  [else (buscar (cdr ids) (cdr refs))]))))
          (buscar ids refs))))))

;; apply-env : Ambiente x Symbol -> ExpVal
;; Propósito: wrapper que busca referencia y la desreferencia.
(define apply-env
  (lambda (env id)
    (deref (apply-env-ref env id))))

;; ambiente-inicial : () -> Ambiente
;; Propósito: construye el ambiente inicial con x=4, y=2, z=5, a=4, b=5, c=6.
;; Todas las variables del ambiente inicial son inmutables ('let).
(define ambiente-inicial
  (lambda ()
    (initialize-store!)
    (ambiente-extendido
     '(x y z a b c)
     (list (newref 4 'let)
           (newref 2 'let)
           (newref 5 'let)
           (newref 4 'let)
           (newref 5 'let)
           (newref 6 'let))
     (ambiente-vacio))))

;; ============================================================
;; PROCEDIMIENTOS (CLAUSURAS)
;; ============================================================

;; procedimiento : List<Symbol> x Expresion x Ambiente
;; Propósito: representa una clausura que captura el ambiente.
(define-datatype procedimiento procedimiento?
  (cerradura
   (params (list-of symbol?))
   (body expresion?)
   (env ambiente?)))

;; apply-procedure : Procedimiento x List<ExpVal> -> ExpVal
;; Propósito: aplica un procedimiento a una lista de argumentos.
;; Los parámetros se ligan como inmutables ('let).
(define apply-procedure
  (lambda (proc args)
    (cases procedimiento proc
      (cerradura (params body env)
        (if (= (length params) (length args))
            (let ((refs (map (lambda (v) (newref v 'let)) args)))
              (evaluar-expresion body
                (ambiente-extendido params refs env)))
            (eopl:error 'apply-procedure
              "Aridad incorrecta: esperaba ~s argumentos, recibió ~s"
              (length params) (length args)))))))

;; ============================================================
;; EVALUADOR
;; ============================================================

;; evaluar-programa : Programa -> ExpVal
;; Propósito: evalúa un programa completo.
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp)
        (evaluar-expresion exp (ambiente-inicial))))))

;; evaluar-expresion : Expresion x Ambiente -> ExpVal
;; Propósito: evalúa una expresión en un ambiente con referencias.
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp

      ;; lit-exp : literal numérico
      (lit-exp (dato) dato)

      ;; ident-exp : busca el valor de un identificador
      (ident-exp (id)
        (apply-env env id))

      ;; true-exp / false-exp : literales booleanos
      (true-exp () #t)
      (false-exp () #f)

      ;; prim-exp : aplicación de primitivas
      (prim-exp (prim rands)
        (let ((args (map (lambda (r) (evaluar-expresion r env)) rands)))
          (evaluar-primitiva prim args)))

      ;; if-exp : condicional
      (if-exp (test true-exp false-exp)
        (let ((test-value (evaluar-expresion test env)))
          (if (boolean? test-value)
              (if test-value
                  (evaluar-expresion true-exp env)
                  (evaluar-expresion false-exp env))
              (eopl:error 'if-exp
                "La condición debe ser booleana, se recibió: ~s" test-value))))

      ;; let-exp : ligaduras inmutables
      (let-exp (ids rands body)
        (let* ((vals (map (lambda (r) (evaluar-expresion r env)) rands))
               (refs (map (lambda (v) (newref v 'let)) vals)))
          (evaluar-expresion body
            (ambiente-extendido ids refs env))))

      ;; var-exp : ligaduras mutables (NUEVO)
      (var-exp (ids rands body)
        (let* ((vals (map (lambda (r) (evaluar-expresion r env)) rands))
               (refs (map (lambda (v) (newref v 'var)) vals)))
          (evaluar-expresion body
            (ambiente-extendido ids refs env))))

      ;; set-exp : asignación (NUEVO)
      (set-exp (id rhs)
        (let ((ref (apply-env-ref env id))
              (val (evaluar-expresion rhs env)))
          (setref! ref val)
          'void))

      ;; begin-exp : secuenciación de efectos (NUEVO)
      (begin-exp (first rest)
        (letrec ((eval-sequence
                  (lambda (exps)
                    (if (null? exps)
                        (evaluar-expresion first env)
                        (begin
                          (evaluar-expresion first env)
                          (eval-sequence-helper rest))))))
                 (eval-sequence-helper
                  (lambda (exps)
                    (if (null? (cdr exps))
                        (evaluar-expresion (car exps) env)
                        (begin
                          (evaluar-expresion (car exps) env)
                          (eval-sequence-helper (cdr exps)))))))
          (if (null? rest)
              (evaluar-expresion first env)
              (begin
                (evaluar-expresion first env)
                (eval-sequence-helper rest)))))

      ;; freeze-exp : congelación de variables (NUEVO)
      (freeze-exp (id)
        (let ((ref (apply-env-ref env id)))
          (freeze-ref! ref)
          'void))

      ;; proc-exp : creación de procedimiento (NUEVO)
      (proc-exp (params body)
        (cerradura params body env))

      ;; app-exp : aplicación de procedimiento (NUEVO)
      (app-exp (rator rands)
        (let ((proc (evaluar-expresion rator env))
              (args (map (lambda (r) (evaluar-expresion r env)) rands)))
          (if (procedimiento? proc)
              (apply-procedure proc args)
              (eopl:error 'app-exp
                "El operador no es un procedimiento: ~s" proc))))

      ;; cond-exp : condicional múltiple
      (cond-exp (conditions actions default)
        (letrec
            ((evaluar-cond
              (lambda (conds acts)
                (cond
                  [(null? conds)
                   (evaluar-expresion default env)]
                  [else
                   (let ((test-value (evaluar-expresion (car conds) env)))
                     (if (boolean? test-value)
                         (if test-value
                             (evaluar-expresion (car acts) env)
                             (evaluar-cond (cdr conds) (cdr acts)))
                         (eopl:error 'cond-exp
                           "Condición no booleana: ~s" test-value)))]))))
          (evaluar-cond conditions actions)))

      ;; let*-exp : ligaduras secuenciales
      (let*-exp (ids rands body)
        (letrec
            ((extender-secuencial
              (lambda (ids rands env-actual)
                (if (null? ids)
                    (evaluar-expresion body env-actual)
                    (let* ((val (evaluar-expresion (car rands) env-actual))
                           (ref (newref val 'let)))
                      (extender-secuencial
                       (cdr ids)
                       (cdr rands)
                       (ambiente-extendido (list (car ids)) (list ref) env-actual)))))))
          (extender-secuencial ids rands env)))

      ;; unless-exp : condicional inverso
      (unless-exp (test usual except)
        (let ((test-value (evaluar-expresion test env)))
          (if (boolean? test-value)
              (if test-value
                  (evaluar-expresion except env)
                  (evaluar-expresion usual env))
              (eopl:error 'unless-exp
                "La condición debe ser booleana, se recibió: ~s" test-value))))
      )))

;; ============================================================
;; PRIMITIVAS
;; ============================================================

;; evaluar-primitiva : Primitiva x List<ExpVal> -> ExpVal
;; Propósito: aplica una primitiva a sus argumentos.
(define evaluar-primitiva
  (lambda (prim args)
    (cases primitiva prim
      (sum-prim () (apply + args))
      (minus-prim () (apply - args))
      (mult-prim () (apply * args))
      (div-prim () (apply / args))
      
      (add-prim ()
        (if (= (length args) 1)
            (+ (car args) 1)
            (eopl:error 'add-prim "add1 es unaria")))
      
      (sub-prim ()
        (if (= (length args) 1)
            (- (car args) 1)
            (eopl:error 'sub-prim "sub1 es unaria")))
      
      (mayor-prim () (apply > args))
      (mayorigual-prim () (apply >= args))
      (menor-prim () (apply < args))
      (menorigual-prim () (apply <= args))
      (igual-prim () (apply = args))
      
      (not-prim ()
        (if (= (length args) 1)
            (let ((v (car args)))
              (if (boolean? v)
                  (not v)
                  (eopl:error 'not-prim "not requiere booleano")))
            (eopl:error 'not-prim "not es unaria")))
      
      (and-prim ()
        (if (= (length args) 2)
            (let ((v1 (car args)) (v2 (cadr args)))
              (if (and (boolean? v1) (boolean? v2))
                  (and v1 v2)
                  (eopl:error 'and-prim "and requiere dos booleanos")))
            (eopl:error 'and-prim "and requiere 2 argumentos")))
      
      (or-prim ()
        (if (= (length args) 2)
            (let ((v1 (car args)) (v2 (cadr args)))
              (if (and (boolean? v1) (boolean? v2))
                  (or v1 v2)
                  (eopl:error 'or-prim "or requiere dos booleanos")))
            (eopl:error 'or-prim "or requiere 2 argumentos")))
      
      (min-prim ()
        (if (>= (length args) 1)
            (apply min args)
            (eopl:error 'min-prim "min requiere al menos 1 argumento")))
      
      (max-prim ()
        (if (>= (length args) 1)
            (apply max args)
            (eopl:error 'max-prim "max requiere al menos 1 argumento")))
      
      (mod-prim ()
        (if (= (length args) 2)
            (modulo (car args) (cadr args))
            (eopl:error 'mod-prim "mod requiere 2 argumentos")))
      
      (pow-prim ()
        (if (= (length args) 2)
            (expt (car args) (cadr args))
            (eopl:error 'pow-prim "pow requiere 2 argumentos")))
      )))

;; ============================================================
;; REPL
;; ============================================================

;; interpretador : () -> Void
;; Propósito: inicia el bucle Read-Eval-Print.
(define interpretador
  (sllgen:make-rep-loop
   "MiniLang> "
   (lambda (pgm) (evaluar-programa pgm))
   (sllgen:make-stream-parser
    especificacion-lexica especificacion-gramatical)))

;; Exportar todas las definiciones
(provide (all-defined-out))
