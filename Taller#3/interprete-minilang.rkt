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
;; COMPATIBILIDAD CON #lang eopl
;; ============================================================

;; void-value : () -> Symbol
;; Propósito: valor nulo compatible con #lang eopl (void no existe).
(define void-value (lambda () 'void))

;; andmap : (Any -> Bool) x List -> Bool
;; Propósito: andmap no existe en #lang eopl — se define manualmente.
(define andmap
  (lambda (pred lst)
    (cond
      [(null? lst) #t]
      [(pred (car lst)) (andmap pred (cdr lst))]
      [else #f])))

;; ============================================================
;; ESPECIFICACIÓN LÉXICA
;; ============================================================

;; especificacion-lexica : List
;; Propósito: define los tokens del lenguaje MiniLang+Refs+Tipos.
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

;; ============================================================
;; ESPECIFICACIÓN GRAMATICAL
;; ============================================================
;; Nota: las anotaciones de tipo en let, var y proc son sintácticamente
;; obligatorias (como pide el enunciado). El intérprete las recibe pero
;; las ignora; el chequeador las consume.
;;
;; Sintaxis clave:
;;   let  x : int = e  in body
;;   var  x : int = e  in body
;;   set  x := e
;;   proc (int x, bool y) body
;;   (f arg1 arg2)

;; especificacion-gramatical : List
;; Propósito: define la gramática BNF de MiniLang con referencias y tipos.
(define especificacion-gramatical
  '(
    ;; Programa
    (programa (expresion) a-program)

    ;; ---- Tipos (categoría sintáctica) ----
    (tipo ("int")  int-type-exp)
    (tipo ("bool") bool-type-exp)
    (tipo ("void") void-type-exp)
    (tipo ("ref" "(" tipo ")") ref-type-exp)
    (tipo ("(" (separated-list tipo ",") "->" tipo ")") proc-type-exp)

    ;; ---- Expresiones base ----
    (expresion (numero)        lit-exp)
    (expresion (identificador) ident-exp)
    (expresion ("true")        true-exp)
    (expresion ("false")       false-exp)

    (expresion
     (primitiva "(" (separated-list expresion ",") ")")
     prim-exp)

    (expresion
     ("if" expresion "then" expresion "else" expresion)
     if-exp)

    ;; ---- let inmutable con anotación de tipo ----
    ;; Sintaxis: let x : int = e1  y : bool = e2  in body
    (expresion
     ("let" (arbno identificador ":" tipo "=" expresion) "in" expresion)
     let-exp)

    ;; ---- var mutable con anotación de tipo ----
    ;; Sintaxis: var x : int = e1  in body
    (expresion
     ("var" (arbno identificador ":" tipo "=" expresion) "in" expresion)
     var-exp)

    ;; ---- set asignación (:= como en el enunciado) ----
    (expresion
     ("set" identificador ":=" expresion)
     set-exp)

    ;; ---- begin secuenciación ----
    (expresion
     ("begin" expresion (arbno ";" expresion) "end")
     begin-exp)

    ;; ---- freeze congelación ----
    (expresion
     ("freeze" identificador)
     freeze-exp)

    ;; ---- Procedimientos con tipos en parámetros ----
    ;; Sintaxis: proc (int x, bool y) body
    (expresion
     ("proc" "(" (separated-list tipo identificador ",") ")" expresion)
     proc-exp)

    ;; ---- Aplicación ----
    (expresion
     ("(" expresion (arbno expresion) ")")
     app-exp)

    ;; ---- Expresiones adicionales del Taller 2 ----
    (expresion
     ("cond" (arbno expresion "==>" expresion)
             "else" "==>" expresion "end")
     cond-exp)

    (expresion
     ("let*" (arbno identificador ":" tipo "=" expresion) "in" expresion)
     let*-exp)

    (expresion
     ("unless" expresion "then" expresion "else" expresion)
     unless-exp)

    ;; ---- Primitivas ----
    (primitiva ("+")   sum-prim)
    (primitiva ("-")   minus-prim)
    (primitiva ("*")   mult-prim)
    (primitiva ("/")   div-prim)
    (primitiva ("add1") add-prim)
    (primitiva ("sub1") sub-prim)
    (primitiva (">")   mayor-prim)
    (primitiva (">=")  mayorigual-prim)
    (primitiva ("<")   menor-prim)
    (primitiva ("<=")  menorigual-prim)
    (primitiva ("==")  igual-prim)
    (primitiva ("not") not-prim)
    (primitiva ("and") and-prim)
    (primitiva ("or")  or-prim)
    (primitiva ("min") min-prim)
    (primitiva ("max") max-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") pow-prim)
    ))

;; Genera automáticamente los define-datatype
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; scanner : String -> List-of-tokens
(define scanner
  (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))

;; parser : String -> Programa
;; Propósito: convierte un string fuente en un AST del programa.
(define parser
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; ============================================================
;; TAD REFERENCIA (EOPL §4.2)
;; ============================================================
;; El store es un vector global. Cada celda guarda (marca . valor).
;; Marcas: 'let (inmutable), 'var (mutable), 'frozen (congelada).

(define the-store 'uninitialized)

;; empty-store : () -> Store
;; Propósito: crea un store vacío.
(define empty-store
  (lambda () (make-vector 0)))

;; initialize-store! : () -> Void
;; Propósito: reinicia el store global al inicio de cada programa.
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;; reference? : Any -> Boolean
;; Propósito: verifica si v es un índice válido en el store actual.
(define reference?
  (lambda (v)
    (and (integer? v) (>= v 0) (< v (vector-length the-store)))))

;; newref : ExpVal x Symbol -> Ref
;; Propósito: crea una nueva celda en el store con valor val y marca dada.
;;   marca puede ser: 'let (inmutable), 'var (mutable), 'frozen (congelada).
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
                      (void-value)))))
        (copy-store 0))
      (vector-set! new-store next-ref (cons marca val))
      (set! the-store new-store)
      next-ref)))

;; deref : Ref -> ExpVal
;; Propósito: lee el valor almacenado en la posición ref del store.
(define deref
  (lambda (ref)
    (if (reference? ref)
        (cdr (vector-ref the-store ref))
        (eopl:error 'deref "Referencia inválida: ~s" ref))))

;; setref! : Ref x ExpVal -> Void
;; Propósito: escribe val en ref. Solo permite escritura sobre celdas 'var.
;;   Lanza error si la celda es 'let o 'frozen.
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
                 "No se puede asignar a identificador inmutable (let)")]
              [(eq? marca 'frozen)
               (eopl:error 'setref!
                 "No se puede asignar a identificador congelado (frozen)")]
              [else
               (eopl:error 'setref!
                 "Marca de mutabilidad desconocida: ~s" marca)])))
        (eopl:error 'setref! "Referencia inválida: ~s" ref))))

;; get-marca : Ref -> Symbol
;; Propósito: devuelve la marca de mutabilidad de la celda en ref.
(define get-marca
  (lambda (ref)
    (if (reference? ref)
        (car (vector-ref the-store ref))
        (eopl:error 'get-marca "Referencia inválida: ~s" ref))))

;; freeze-ref! : Ref -> Void
;; Propósito: cambia la marca de 'var a 'frozen. Falla si no es 'var.
(define freeze-ref!
  (lambda (ref)
    (if (reference? ref)
        (let ((cell (vector-ref the-store ref)))
          (let ((marca (car cell))
                (val   (cdr cell)))
            (cond
              [(eq? marca 'var)
               (vector-set! the-store ref (cons 'frozen val))]
              [(eq? marca 'frozen)
               (eopl:error 'freeze-ref! "La variable ya está congelada")]
              [(eq? marca 'let)
               (eopl:error 'freeze-ref!
                 "No se puede congelar un identificador inmutable (let)")]
              [else
               (eopl:error 'freeze-ref!
                 "Marca desconocida: ~s" marca)])))
        (eopl:error 'freeze-ref! "Referencia inválida: ~s" ref))))

;; ============================================================
;; AMBIENTE CON REFERENCIAS
;; ============================================================

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (ids  (list-of symbol?))
   (refs (list-of reference?))
   (env  ambiente?)))

;; apply-env-ref : Ambiente x Symbol -> Ref
;; Propósito: retorna la referencia ligada a id. Necesaria para set y freeze.
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
;; Propósito: retorna el valor desreferenciado ligado a id.
(define apply-env
  (lambda (env id)
    (deref (apply-env-ref env id))))

;; ambiente-inicial : () -> Ambiente
;; Propósito: construye el ambiente inicial con variables predefinidas.
;;   let:  x=4, y=2, z=5   (marca 'let — inmutables)
;;   var:  a=4, b=5, c=6   (marca 'var — mutables)
(define ambiente-inicial
  (lambda ()
    (initialize-store!)
    (ambiente-extendido
     '(x y z a b c)
     (list (newref 4 'let)
           (newref 2 'let)
           (newref 5 'let)
           (newref 4 'var)
           (newref 5 'var)
           (newref 6 'var))
     (ambiente-vacio))))

;; ============================================================
;; PROCEDIMIENTOS (CLAUSURAS)
;; ============================================================

(define-datatype procedimiento procedimiento?
  (cerradura
   (params (list-of symbol?))
   (body   expresion?)
   (env    ambiente?)))

;; apply-procedure : Procedimiento x List<ExpVal> -> ExpVal
;; Propósito: aplica el procedimiento extendiendo su ambiente de creación.
;;   Los parámetros se ligan con referencias 'let (paso por valor, sin mutación).
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
;; Propósito: punto de entrada del intérprete.
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp)
        (evaluar-expresion exp (ambiente-inicial))))))

;; evaluar-expresion : Expresion x Ambiente -> ExpVal
;; Propósito: recorre el AST y evalúa cada forma del lenguaje.
;;   Las anotaciones de tipo (campo `tipos`) se ignoran en el intérprete.
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp

      (lit-exp (dato) dato)

      (ident-exp (id)
        (apply-env env id))

      (true-exp  () #t)
      (false-exp () #f)

      (prim-exp (prim rands)
        (let ((args (map (lambda (r) (evaluar-expresion r env)) rands)))
          (evaluar-primitiva prim args)))

      (if-exp (test then-exp else-exp)
        (let ((test-val (evaluar-expresion test env)))
          (if (boolean? test-val)
              (if test-val
                  (evaluar-expresion then-exp env)
                  (evaluar-expresion else-exp env))
              (eopl:error 'if-exp
                "La condición debe ser booleana, se recibió: ~s" test-val))))

      ;; let-exp: ids, tipos (ignorados), rands, body
      (let-exp (ids tipos rands body)
        (let* ((vals (map (lambda (r) (evaluar-expresion r env)) rands))
               (refs (map (lambda (v) (newref v 'let)) vals)))
          (evaluar-expresion body
            (ambiente-extendido ids refs env))))

      ;; var-exp: ids, tipos (ignorados), rands, body
      (var-exp (ids tipos rands body)
        (let* ((vals (map (lambda (r) (evaluar-expresion r env)) rands))
               (refs (map (lambda (v) (newref v 'var)) vals)))
          (evaluar-expresion body
            (ambiente-extendido ids refs env))))

      ;; set-exp: asignación — solo sobre 'var
      (set-exp (id rhs)
        (let ((ref (apply-env-ref env id))
              (val (evaluar-expresion rhs env)))
          (setref! ref val)
          (void-value)))

      ;; begin-exp: secuenciación — retorna el valor de la última expresión
      (begin-exp (first rest)
        (if (null? rest)
            (evaluar-expresion first env)
            (begin
              (evaluar-expresion first env)
              (letrec ((eval-seq
                        (lambda (exps)
                          (if (null? (cdr exps))
                              (evaluar-expresion (car exps) env)
                              (begin
                                (evaluar-expresion (car exps) env)
                                (eval-seq (cdr exps)))))))
                (eval-seq rest)))))

      ;; freeze-exp: congela una variable 'var → 'frozen
      (freeze-exp (id)
        (let ((ref (apply-env-ref env id)))
          (freeze-ref! ref)
          (void-value)))

      ;; proc-exp: tipos de params (ignorados), ids de params, body
      (proc-exp (tipos-params ids body)
        (cerradura ids body env))

      ;; app-exp: aplicación de procedimiento
      (app-exp (rator rands)
        (let ((proc (evaluar-expresion rator env))
              (args (map (lambda (r) (evaluar-expresion r env)) rands)))
          (if (procedimiento? proc)
              (apply-procedure proc args)
              (eopl:error 'app-exp
                "El operador no es un procedimiento: ~s" proc))))

      ;; cond-exp: condicional múltiple
      (cond-exp (conditions actions default)
        (letrec
            ((evaluar-cond
              (lambda (conds acts)
                (cond
                  [(null? conds)
                   (evaluar-expresion default env)]
                  [else
                   (let ((test-val (evaluar-expresion (car conds) env)))
                     (if (boolean? test-val)
                         (if test-val
                             (evaluar-expresion (car acts) env)
                             (evaluar-cond (cdr conds) (cdr acts)))
                         (eopl:error 'cond-exp
                           "Condición no booleana: ~s" test-val)))]))))
          (evaluar-cond conditions actions)))

      ;; let*-exp: let secuencial con anotaciones de tipo (ignoradas)
      (let*-exp (ids tipos rands body)
        (letrec
            ((ext-seq
              (lambda (ids tipos rands env-act)
                (if (null? ids)
                    (evaluar-expresion body env-act)
                    (let* ((val (evaluar-expresion (car rands) env-act))
                           (ref (newref val 'let)))
                      (ext-seq
                       (cdr ids) (cdr tipos) (cdr rands)
                       (ambiente-extendido (list (car ids)) (list ref) env-act)))))))
          (ext-seq ids tipos rands env)))

      ;; unless-exp: if invertido
      (unless-exp (test usual except)
        (let ((test-val (evaluar-expresion test env)))
          (if (boolean? test-val)
              (if test-val
                  (evaluar-expresion except env)
                  (evaluar-expresion usual env))
              (eopl:error 'unless-exp
                "La condición debe ser booleana, se recibió: ~s" test-val))))
      )))

;; ============================================================
;; PRIMITIVAS
;; ============================================================

;; evaluar-primitiva : Primitiva x List<ExpVal> -> ExpVal
;; Propósito: aplica la primitiva a los argumentos ya evaluados.
(define evaluar-primitiva
  (lambda (prim args)
    (cases primitiva prim
      (sum-prim   () (apply + args))
      (minus-prim () (apply - args))
      (mult-prim  () (apply * args))
      (div-prim   () (apply / args))

      (add-prim ()
        (if (= (length args) 1)
            (+ (car args) 1)
            (eopl:error 'add-prim "add1 requiere exactamente 1 argumento")))

      (sub-prim ()
        (if (= (length args) 1)
            (- (car args) 1)
            (eopl:error 'sub-prim "sub1 requiere exactamente 1 argumento")))

      (mayor-prim      () (apply > args))
      (mayorigual-prim () (apply >= args))
      (menor-prim      () (apply < args))
      (menorigual-prim () (apply <= args))
      (igual-prim      () (apply = args))

      (not-prim ()
        (if (= (length args) 1)
            (let ((v (car args)))
              (if (boolean? v)
                  (not v)
                  (eopl:error 'not-prim "not requiere un booleano")))
            (eopl:error 'not-prim "not requiere exactamente 1 argumento")))

      (and-prim ()
        (if (= (length args) 2)
            (let ((v1 (car args)) (v2 (cadr args)))
              (if (and (boolean? v1) (boolean? v2))
                  (and v1 v2)
                  (eopl:error 'and-prim "and requiere dos booleanos")))
            (eopl:error 'and-prim "and requiere exactamente 2 argumentos")))

      (or-prim ()
        (if (= (length args) 2)
            (let ((v1 (car args)) (v2 (cadr args)))
              (if (and (boolean? v1) (boolean? v2))
                  (or v1 v2)
                  (eopl:error 'or-prim "or requiere dos booleanos")))
            (eopl:error 'or-prim "or requiere exactamente 2 argumentos")))

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
            (eopl:error 'mod-prim "mod requiere exactamente 2 argumentos")))

      (pow-prim ()
        (if (= (length args) 2)
            (expt (car args) (cadr args))
            (eopl:error 'pow-prim "pow requiere exactamente 2 argumentos")))
      )))

;; ============================================================
;; REPL
;; ============================================================

(define interpretador
  (sllgen:make-rep-loop
   "MiniLang> "
   (lambda (pgm) (evaluar-programa pgm))
   (sllgen:make-stream-parser
    especificacion-lexica especificacion-gramatical)))

(provide (all-defined-out))
