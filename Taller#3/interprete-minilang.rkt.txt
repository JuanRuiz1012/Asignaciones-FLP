#lang eopl

;; ============================================================
;; Taller 3: Asignación y chequeo de tipos — Intérprete
;; Fundamentos de Lenguajes de Programación — 2026-1
;; Universidad del Valle
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710,
;;          JUAN DIEGO OSPINA     2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701,
;;          JUAN FELIPE RUIZ      2359397
;; ============================================================

(require eopl)

;; ============================================================
;; SECCIÓN 1 — Especificación léxica
;; ============================================================

;; especificacion-lexica : List
;; Propósito: define los tokens del lenguaje MiniLang con refs.
;; Conserva la del Taller 2: comentarios con %, espacios ignorados,
;; enteros, flotantes con signo, identificadores con ? y $.
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
;; SECCIÓN 2 — Especificación gramatical
;; ============================================================

;; especificacion-gramatical : List
;; Propósito: define la gramática BNF de MiniLang+Refs+Tipos.
;; Extiende la gramática del Taller 2 con:
;;   - var-exp  (ligadura mutable)
;;   - set-exp  (asignación)
;;   - begin-exp (secuenciación)
;;   - freeze-exp (congelamiento de variable)
;; Nota: el antiguo var-exp(id) se renombra a ident-exp(id).
;; Las anotaciones de tipo se aceptan en la gramática pero el
;; evaluador las ignora (las usa el chequeador en Parte 3).
(define especificacion-gramatical
  '(
    ;; Programa
    (programa (expresion) a-program)

    ;; ── Expresiones ──────────────────────────────────────────

    ;; Literales numéricos
    (expresion (numero) lit-exp)

    ;; Variable (antes llamado var-exp, ahora ident-exp)
    (expresion (identificador) ident-exp)

    ;; Booleanos
    (expresion ("true")  true-exp)
    (expresion ("false") false-exp)

    ;; Primitiva aplicada a argumentos
    (expresion
     (primitiva "(" (separated-list expresion ",") ")")
     prim-exp)

    ;; if / then / else
    (expresion
     ("if" expresion "then" expresion "else" expresion)
     if-exp)

    ;; let — ligadura inmutable con anotación de tipo opcional
    ;; Sintaxis: let x : T = e , ... in body
    (expresion
     ("let" (arbno identificador ":" tipo "=" expresion) "in" expresion)
     let-exp)

    ;; var — ligadura mutable con anotación de tipo opcional
    ;; Sintaxis: var x : T = e , ... in body
    (expresion
     ("var" (arbno identificador ":" tipo "=" expresion) "in" expresion)
     var-exp)

    ;; set — asignación a variable mutable
    ;; Sintaxis: set x := e
    (expresion
     ("set" identificador ":=" expresion)
     set-exp)

    ;; begin — secuenciación; retorna el valor de la última expresión
    (expresion
     ("begin" expresion (arbno ";" expresion) "end")
     begin-exp)

    ;; freeze — congela una variable mutable
    ;; Sintaxis: freeze x
    (expresion
     ("freeze" identificador)
     freeze-exp)

    ;; cond — condicional de múltiples ramas
    (expresion
     ("cond" (arbno expresion "==>" expresion)
      "else" "==>" expresion "end")
     cond-exp)

    ;; let* — ligaduras secuenciales inmutables
    (expresion
     ("let*" (arbno identificador ":" tipo "=" expresion) "in" expresion)
     let*-exp)

    ;; unless — condicional invertido
    (expresion
     ("unless" expresion "then" expresion "else" expresion)
     unless-exp)

    ;; proc — procedimiento anónimo con anotaciones de tipo
    ;; Sintaxis: proc(T1 x1, T2 x2, ...) body
    (expresion
     ("proc" "(" (separated-list tipo identificador ",") ")" expresion "end")
     proc-exp)

    ;; apply — aplicación de procedimiento
    ;; Sintaxis: (f arg1, arg2, ...)
    (expresion
     ("(" expresion (separated-list expresion ",") ")")
     app-exp)

    ;; letrec — procedimientos mutuamente recursivos
    (expresion
     ("letrec"
      tipo identificador "(" (separated-list tipo identificador ",") ")"
      "=" expresion
      (arbno tipo identificador "(" (separated-list tipo identificador ",") ")"
             "=" expresion)
      "in" expresion "end")
     letrec-exp)

    ;; ── Tipos (para anotaciones) ──────────────────────────────
    (tipo ("int")              int-type-exp)
    (tipo ("bool")             bool-type-exp)
    (tipo ("void")             void-type-exp)
    (tipo ("ref" "(" tipo ")") ref-type-exp)
    (tipo ("(" (separated-list tipo "*") "->" tipo ")")
          proc-type-exp)

    ;; ── Primitivas ────────────────────────────────────────────
    (primitiva ("+")    sum-prim)
    (primitiva ("-")    minus-prim)
    (primitiva ("*")    mult-prim)
    (primitiva ("/")    div-prim)
    (primitiva ("add1") add-prim)
    (primitiva ("sub1") sub-prim)
    (primitiva (">")    mayor-prim)
    (primitiva (">=")   mayorigual-prim)
    (primitiva ("<")    menor-prim)
    (primitiva ("<=")   menorigual-prim)
    (primitiva ("==")   igual-prim)
    (primitiva ("not")  not-prim)
    (primitiva ("and")  and-prim)
    (primitiva ("or")   or-prim)
    (primitiva ("min")  min-prim)
    (primitiva ("max")  max-prim)
    (primitiva ("mod")  mod-prim)
    (primitiva ("pow")  pow-prim)
    ))

;; Genera automáticamente los define-datatype desde la gramática
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; scan&parse : String -> Programa
;; Propósito: convierte código fuente en AST de tipo programa.
(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; ============================================================
;; SECCIÓN 3 — TAD Referencia  (EOPL §4.2, Fig. 4.5)
;; ============================================================

;; El store global: un vector que crece dinámicamente.
;; Cada posición guarda (valor . marca) donde marca ∈ {'let 'var 'frozen}.

;; the-store : VectorOf (SchemeVal . Symbol)
(define the-store 'uninitialized)

;; initialize-store! : () -> Unspecified
;; Propósito: inicializa el store vacío antes de cada evaluación.
(define initialize-store!
  (lambda ()
    (set! the-store (make-vector 100 #f))
    (set! store-size 0)))

;; store-size : Int
;; Propósito: número de celdas actualmente usadas en el store.
(define store-size 0)

;; reference? : Any -> Boolean
;; Propósito: predicado para valores de tipo referencia.
(define reference?
  (lambda (v)
    (integer? v)))

;; newref : SchemeVal x Symbol -> Ref
;; Propósito: crea una nueva celda en el store con valor v y marca m.
;;            Retorna la posición (número entero) como referencia.
(define newref
  (lambda (v m)
    (let ((pos store-size))
      (vector-set! the-store pos (cons v m))
      (set! store-size (+ store-size 1))
      pos)))

;; deref : Ref -> SchemeVal
;; Propósito: retorna el valor almacenado en la referencia ref.
(define deref
  (lambda (ref)
    (car (vector-ref the-store ref))))

;; deref-mark : Ref -> Symbol
;; Propósito: retorna la marca de mutabilidad ('let, 'var o 'frozen).
(define deref-mark
  (lambda (ref)
    (cdr (vector-ref the-store ref))))

;; setref! : Ref x SchemeVal -> Unspecified
;; Propósito: actualiza el valor en la celda ref sin cambiar la marca.
(define setref!
  (lambda (ref v)
    (let ((marca (deref-mark ref)))
      (vector-set! the-store ref (cons v marca)))))

;; setref-mark! : Ref x Symbol -> Unspecified
;; Propósito: actualiza únicamente la marca de mutabilidad de la celda ref.
(define setref-mark!
  (lambda (ref m)
    (let ((val (deref ref)))
      (vector-set! the-store ref (cons val m)))))

;; ============================================================
;; SECCIÓN 4 — TAD Ambiente con referencias
;; ============================================================

;; El ambiente liga cada identificador con una referencia (posición en el store).
;; apply-env retorna el valor desreferenciado; apply-env-ref retorna la referencia.

;; define-datatype ambiente ambiente?
;; Variantes:
;;   ambiente-vacio : ambiente vacío (base de la cadena)
;;   ambiente-extendido : liga ids con un vector de referencias
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (ids  (list-of symbol?))
   (refs (list-of reference?))
   (env  ambiente?)))

;; apply-env : Ambiente x Symbol -> SchemeVal
;; Propósito: busca id en el ambiente y retorna el valor desreferenciado.
;;            Lanza error si id no está ligado.
(define apply-env
  (lambda (env id)
    (deref (apply-env-ref env id))))

;; apply-env-ref : Ambiente x Symbol -> Ref
;; Propósito: busca id en el ambiente y retorna la referencia asociada.
;;            Necesaria para set-exp y freeze-exp.
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
                  [(null? ids)         (apply-env-ref old-env id)]
                  [(equal? (car ids) id) (car refs)]
                  [else                (buscar (cdr ids) (cdr refs))]))))
          (buscar ids refs))))))

;; extend-env : List-of-Symbol x List-of-Ref x Ambiente -> Ambiente
;; Propósito: extiende el ambiente con las ligaduras ids↦refs.
(define extend-env
  (lambda (ids refs env)
    (ambiente-extendido ids refs env)))

;; ============================================================
;; SECCIÓN 5 — Valor del lenguaje
;; ============================================================

;; scheme-value? : Any -> Boolean
;; Propósito: predicado para valores expresados del lenguaje.
;;            Números, booleanos y void son valores válidos.
(define scheme-value?
  (lambda (v)
    (or (number? v) (boolean? v) (eq? v 'void) (procval? v))))

;; void-val : () -> Symbol
;; Propósito: valor especial que devuelven los efectos.
(define void-val
  (lambda ()
    'void))

;; procval? : Any -> Boolean
;; Propósito: predicado para clausuras.
(define procval?
  (lambda (v)
    (and (list? v) (not (null? v)) (eq? (car v) 'procval))))

;; make-procval : List-of-Symbol x Expresion x Ambiente -> ProcVal
;; Propósito: construye una clausura con parámetros, cuerpo y ambiente de creación.
(define make-procval
  (lambda (ids body env)
    (list 'procval ids body env)))

;; procval->ids : ProcVal -> List-of-Symbol
(define procval->ids  (lambda (p) (cadr  p)))
;; procval->body : ProcVal -> Expresion
(define procval->body (lambda (p) (caddr p)))
;; procval->env  : ProcVal -> Ambiente
(define procval->env  (lambda (p) (cadddr p)))

;; ============================================================
;; SECCIÓN 6 — Ambiente inicial
;; ============================================================

;; ambiente-inicial : () -> Ambiente
;; Propósito: construye el ambiente inicial con ligaduras inmutables
;;            x=4, y=2, z=5 y mutables a=4, b=5, c=6.
(define ambiente-inicial
  (lambda ()
    (let* ((ref-x (newref 4 'let))
           (ref-y (newref 2 'let))
           (ref-z (newref 5 'let))
           (ref-a (newref 4 'var))
           (ref-b (newref 5 'var))
           (ref-c (newref 6 'var)))
      (ambiente-extendido
       '(x y z a b c)
       (list ref-x ref-y ref-z ref-a ref-b ref-c)
       (ambiente-vacio)))))

;; ============================================================
;; SECCIÓN 7 — Evaluador de expresiones
;; ============================================================

;; evaluar-programa : Programa -> SchemeVal
;; Propósito: inicializa el store y evalúa el programa completo.
(define evaluar-programa
  (lambda (pgm)
    (initialize-store!)
    (cases programa pgm
      (a-program (exp)
        (evaluar-expresion exp (ambiente-inicial))))))

;; evaluar-expresion : Expresion x Ambiente -> SchemeVal
;; Propósito: evalúa exp en env siguiendo las reglas semánticas.
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp

      ;; ── Literales ──────────────────────────────────────────

      ;; lit-exp : número → retorna el número
      (lit-exp (dato) dato)

      ;; ident-exp : identificador → retorna su valor desreferenciado
      (ident-exp (id) (apply-env env id))

      ;; true-exp / false-exp : booleanos
      (true-exp  () #t)
      (false-exp () #f)

      ;; ── Primitivas ─────────────────────────────────────────

      ;; prim-exp : evalúa argumentos y aplica la primitiva
      (prim-exp (prim rands)
        (let ((args (map (lambda (r) (evaluar-expresion r env)) rands)))
          (evaluar-primitiva prim args)))

      ;; ── Condicional ────────────────────────────────────────

      ;; if-exp : evalúa condición; verifica que sea booleana;
      ;;          evalúa rama correspondiente
      (if-exp (test e-then e-else)
        (let ((test-val (evaluar-expresion test env)))
          (if (boolean? test-val)
              (if test-val
                  (evaluar-expresion e-then env)
                  (evaluar-expresion e-else env))
              (eopl:error 'if-exp
                "La condición debe ser booleana; se recibió: ~s" test-val))))

      ;; ── Ligaduras inmutables ────────────────────────────────

      ;; let-exp : evalúa rands en env; crea referencias 'let;
      ;;           evalúa body en env extendido
      (let-exp (ids tipos rands body)
        (let ((vals (map (lambda (r) (evaluar-expresion r env)) rands)))
          (let ((refs (map (lambda (v) (newref v 'let)) vals)))
            (evaluar-expresion body (extend-env ids refs env)))))

      ;; ── Ligaduras mutables ──────────────────────────────────

      ;; var-exp : evalúa rands en env; crea referencias 'var;
      ;;           evalúa body en env extendido
      (var-exp (ids tipos rands body)
        (let ((vals (map (lambda (r) (evaluar-expresion r env)) rands)))
          (let ((refs (map (lambda (v) (newref v 'var)) vals)))
            (evaluar-expresion body (extend-env ids refs env)))))

      ;; ── Asignación ─────────────────────────────────────────

      ;; set-exp : busca la referencia de id; verifica marca 'var;
      ;;           actualiza el store; retorna void
      (set-exp (id rand)
        (let ((ref   (apply-env-ref env id))
              (val   (evaluar-expresion rand env)))
          (let ((marca (deref-mark ref)))
            (cond
              [(eq? marca 'var)
               (setref! ref val)
               (void-val)]
              [(eq? marca 'let)
               (eopl:error 'set-exp
                 "No se puede asignar al identificador inmutable '~s'" id)]
              [(eq? marca 'frozen)
               (eopl:error 'set-exp
                 "No se puede asignar al identificador congelado '~s'" id)]
              [else
               (eopl:error 'set-exp "Marca desconocida ~s en '~s'" marca id)]))))

      ;; ── Secuenciación ───────────────────────────────────────

      ;; begin-exp : evalúa todas las expresiones; retorna el valor de la última
      (begin-exp (first rest)
        (letrec
            ((evaluar-secuencia
              (lambda (exps)
                (if (null? (cdr exps))
                    (evaluar-expresion (car exps) env)
                    (begin
                      (evaluar-expresion (car exps) env)
                      (evaluar-secuencia (cdr exps)))))))
          (evaluar-secuencia (cons first rest))))

      ;; ── Congelamiento ───────────────────────────────────────

      ;; freeze-exp : busca la referencia de id; verifica que sea 'var;
      ;;              cambia la marca a 'frozen; retorna void
      (freeze-exp (id)
        (let ((ref (apply-env-ref env id)))
          (let ((marca (deref-mark ref)))
            (cond
              [(eq? marca 'var)
               (setref-mark! ref 'frozen)
               (void-val)]
              [(eq? marca 'let)
               (eopl:error 'freeze-exp
                 "No se puede congelar el identificador inmutable '~s'" id)]
              [(eq? marca 'frozen)
               (eopl:error 'freeze-exp
                 "El identificador '~s' ya estaba congelado" id)]
              [else
               (eopl:error 'freeze-exp "Marca desconocida ~s en '~s'" marca id)]))))

      ;; ── Condicional múltiple ─────────────────────────────────

      ;; cond-exp : evalúa condiciones en orden; retorna la primera acción cuya
      ;;            condición sea #t; si ninguna, retorna la acción por defecto
      (cond-exp (conditions actions default)
        (letrec
            ((evaluar-cond
              (lambda (conds acts)
                (if (null? conds)
                    (evaluar-expresion default env)
                    (let ((test-val (evaluar-expresion (car conds) env)))
                      (if (boolean? test-val)
                          (if test-val
                              (evaluar-expresion (car acts) env)
                              (evaluar-cond (cdr conds) (cdr acts)))
                          (eopl:error 'cond-exp
                            "Condición no booleana: ~s" test-val)))))))
          (evaluar-cond conditions actions)))

      ;; ── let* ────────────────────────────────────────────────

      ;; let*-exp : extiende el ambiente secuencialmente (cada rand se evalúa
      ;;            en el ambiente que ya incluye las ligaduras anteriores)
      (let*-exp (ids tipos rands body)
        (letrec
            ((extender-seq
              (lambda (ids tipos rands env-actual)
                (if (null? ids)
                    (evaluar-expresion body env-actual)
                    (let ((val (evaluar-expresion (car rands) env-actual)))
                      (let ((ref (newref val 'let)))
                        (extender-seq
                         (cdr ids) (cdr tipos) (cdr rands)
                         (extend-env (list (car ids)) (list ref) env-actual))))))))
          (extender-seq ids tipos rands env)))

      ;; ── unless ──────────────────────────────────────────────

      ;; unless-exp : inverso de if; si condición es #f evalúa then, si #t evalúa else
      (unless-exp (test e-then e-else)
        (let ((test-val (evaluar-expresion test env)))
          (if (boolean? test-val)
              (if test-val
                  (evaluar-expresion e-else env)
                  (evaluar-expresion e-then env))
              (eopl:error 'unless-exp
                "La condición debe ser booleana; se recibió: ~s" test-val))))

      ;; ── Procedimientos ──────────────────────────────────────

      ;; proc-exp : construye una clausura capturando el ambiente actual
      ;; Los parámetros formales se marcan 'let (paso por valor, inmutables)
      (proc-exp (tipos ids body)
        (make-procval ids body env))

      ;; app-exp : evalúa rator y rands; aplica el procedimiento
      (app-exp (rator rands)
        (let ((proc (evaluar-expresion rator env))
              (args (map (lambda (r) (evaluar-expresion r env)) rands)))
          (if (procval? proc)
              (let ((ids  (procval->ids  proc))
                    (body (procval->body proc))
                    (cenv (procval->env  proc)))
                (if (= (length ids) (length args))
                    ;; Crea referencias 'let para los parámetros (inmutables)
                    (let ((refs (map (lambda (v) (newref v 'let)) args)))
                      (evaluar-expresion body (extend-env ids refs cenv)))
                    (eopl:error 'app-exp
                      "Aridad incorrecta: se esperaban ~s argumentos, se recibieron ~s"
                      (length ids) (length args))))
              (eopl:error 'app-exp
                "No es un procedimiento: ~s" proc))))

      ;; ── letrec ──────────────────────────────────────────────

      ;; letrec-exp : crea un ambiente mutuo-recursivo
      ;; Implementado con referencias 'let que se actualizan después de construir el env
      (letrec-exp (tipo1 nombre1 tparams1 params1 body1
                   tipos nombres tparamss paramss bodies
                   cuerpo)
        (let* ((todos-nombres (cons nombre1 nombres))
               (todos-params  (cons params1 paramss))
               (todos-bodies  (cons body1 bodies))
               ;; Crea referencias temporales
               (refs (map (lambda (_) (newref (void-val) 'let)) todos-nombres))
               ;; Ambiente con las referencias ya creadas
               (env-rec (extend-env todos-nombres refs env)))
          ;; Actualiza cada referencia con la clausura real
          (for-each
           (lambda (ref params body)
             (setref! ref (make-procval params body env-rec)))
           refs todos-params todos-bodies)
          (evaluar-expresion cuerpo env-rec)))

      )))

;; ============================================================
;; SECCIÓN 8 — Evaluador de primitivas
;; ============================================================

;; evaluar-primitiva : Primitiva x List-of-SchemeVal -> SchemeVal
;; Propósito: aplica la primitiva a la lista de argumentos evaluados.
;;            Verifica aridad y tipos según la primitiva.
(define evaluar-primitiva
  (lambda (prim args)
    (cases primitiva prim

      ;; Aritméticas n-arias
      (sum-prim   () (apply + args))
      (minus-prim () (apply - args))
      (mult-prim  () (apply * args))
      (div-prim   ()
        (if (and (= (length args) 2) (not (= (cadr args) 0)))
            (/ (car args) (cadr args))
            (if (= (length args) 2)
                (eopl:error 'div-prim "División por cero")
                (eopl:error 'div-prim "/ requiere exactamente 2 argumentos"))))

      ;; Unarias enteras
      (add-prim ()
        (if (= (length args) 1)
            (+ (car args) 1)
            (eopl:error 'add-prim "add1 es unaria; se recibieron ~s argumentos" (length args))))
      (sub-prim ()
        (if (= (length args) 1)
            (- (car args) 1)
            (eopl:error 'sub-prim "sub1 es unaria; se recibieron ~s argumentos" (length args))))

      ;; Relacionales
      (mayor-prim      () (apply >  args))
      (mayorigual-prim () (apply >= args))
      (menor-prim      () (apply <  args))
      (menorigual-prim () (apply <= args))
      (igual-prim      () (apply =  args))

      ;; Booleanas
      (not-prim ()
        (if (= (length args) 1)
            (let ((v (car args)))
              (if (boolean? v)
                  (not v)
                  (eopl:error 'not-prim "not requiere booleano; se recibió: ~s" v)))
            (eopl:error 'not-prim "not es unaria; se recibieron ~s argumentos" (length args))))
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

      ;; Numéricas adicionales
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
;; SECCIÓN 9 — Punto de entrada
;; ============================================================

;; run : String -> SchemeVal
;; Propósito: parsea y evalúa un programa fuente de MiniLang.
(define run
  (lambda (src)
    (evaluar-programa (scan&parse src))))

;; interpretador : () -> Void
;; Propósito: inicia el REPL interactivo de MiniLang.
(define interpretador
  (sllgen:make-rep-loop
   "MiniLang> "
   (lambda (pgm) (evaluar-programa pgm))
   (sllgen:make-stream-parser
    especificacion-lexica especificacion-gramatical)))

(provide (all-defined-out))