#lang eopl

;; ============================================================
;; Taller 3 — Asignación (MiniLang+Refs)
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710, JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701, JUAN FELIPE RUIZ 2359397
;; ============================================================

(require eopl)

;; ------------------------------------------------------------
;; Especificación léxica  JUAN FELIPE RUIZ 
;; ------------------------------------------------------------

;; T2 == Taller#2

;; Propósito: define los tokens del lenguaje MiniLang.
;; Reglas: comentarios con %, espacios ignorados, números enteros
;; y flotantes con signo, identificadores que inician con letra.
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

;; ------------------------------------------------------------
;; Especificación gramatical
;; ------------------------------------------------------------
;; Propósito: define la gramática BNF de MiniLang en formato SLLGEN.
;; Extiende el Taller 2 con: tipos, var-exp, set-exp, begin-exp,
;; freeze-exp, proc-exp y app-exp. La var-exp del Taller#2 pasa a ident-exp.
(define especificacion-gramatical
  '(
    ;; Programa
    (programa (expresion) a-program)

    ;; Expresiones base
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

    ;; let con anotaciones de tipo (ligadura inmutable, marca 'let)
    ;; Sintaxis: let id : tipo = exp ... in exp
    (expresion
     ("let" (arbno identificador ":" tipo "=" expresion) "in" expresion)
     let-exp)

    ;; var con anotaciones de tipo (ligadura mutable, marca 'var)
    ;; Sintaxis: var id : tipo = exp ... in exp
    (expresion
     ("var" (arbno identificador ":" tipo "=" expresion) "in" expresion)
     var-decl-exp)

    ;; Asignación: set id := exp
    (expresion
     ("set" identificador ":=" expresion)
     set-exp)

    ;; Secuencia: begin exp ; exp ... end
    (expresion
     ("begin" expresion (arbno ";" expresion) "end")
     begin-exp)

    ;; Congelación: freeze id
    (expresion
     ("freeze" identificador)
     freeze-exp)

    ;; Procedimiento: proc (tipo id, ...) exp
    (expresion
     ("proc" "(" (separated-list tipo identificador ",") ")" expresion)
     proc-exp)

    ;; Aplicación: (exp exp ...)
    (expresion
     ("(" expresion (arbno expresion) ")")
     app-exp)

    ;; Parte 3 del T2 — cond-exp
    (expresion
     ("cond" (arbno expresion "==>" expresion)
             "else" "==>" expresion "end")
     cond-exp)

    ;; Parte 3 del T2 — let*-exp (sin tipos, mantiene compatibilidad)
    (expresion
     ("let*" (arbno identificador "=" expresion) "in" expresion)
     let*-exp)

    ;; Parte 3 del T2 — unless-exp
    (expresion
     ("unless" expresion "then" expresion "else" expresion)
     unless-exp)

    ;; Primitivas aritméticas y relacionales
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

    ;; Primitivas lógicas y numéricas
    (primitiva ("not") not-prim)
    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("min") min-prim)
    (primitiva ("max") max-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") pow-prim)

    ;; Categoría sintáctica tipo
    (tipo ("int") int-type-exp)
    (tipo ("bool") bool-type-exp)
    (tipo ("void") void-type-exp)
    (tipo ("ref" "(" tipo ")") ref-type-exp)
    (tipo ("(" (separated-list tipo ",") "->" tipo ")") proc-type-exp)
    ))

;; Genera automáticamente los define-datatype desde la gramática
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; scanner : String -> List-of-tokens
;; Propósito: convierte código fuente en una lista de tokens.
(define scanner
  (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))

;; parser : String -> Programa
;; Propósito: convierte código fuente en un AST de tipo programa.
(define parser
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))


;; ------------------------------------------------------------
;; TAD 
;; ------------------------------------------------------------
;; Una referencia es una posición dentro de un vector subyacente.
;; Cada referencia lleva una marca de mutabilidad: 'let, 'var o 'frozen.

;; referencia? : Any -> Boolean
;; Propósito: predicado de referencia.
(define-datatype referencia referencia?
  (a-ref
   (val-vec  vector?)   ;; vector[0] = valor
   (marca-vec vector?))) ;; vector[0] = marca: 'let | 'var | 'frozen

;; newref : SchemeVal x Symbol -> Referencia
;; Propósito: crea una referencia nueva con el valor inicial y la marca dada.
(define newref
  (lambda (val marca)
    (a-ref (make-vector 1 val)
           (make-vector 1 marca))))

;; deref : Referencia -> SchemeVal
;; Propósito: lee el valor almacenado en la referencia.
(define deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (vv mv)
        (vector-ref vv 0)))))

;; setref! : Referencia x SchemeVal -> Void
;; Propósito: escribe val en la celda de la referencia.
(define setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (vv mv)
        (vector-set! vv 0 val)))))

;; ref-marca : Referencia -> Symbol
;; Propósito: devuelve la marca actual ('let, 'var o 'frozen).
(define ref-marca
  (lambda (ref)
    (cases referencia ref
      (a-ref (vv mv)
        (vector-ref mv 0)))))

;; congelar! : Referencia -> Void
;; Propósito: cambia la marca a 'frozen usando vector-set!.
(define congelar!
  (lambda (ref)
    (cases referencia ref
      (a-ref (vv mv)
        (vector-set! mv 0 'frozen)))))


;; ------------------------------------------------------------
;; Ambiente con referencias
;; ------------------------------------------------------------
;; Cada extensión almacena un vector de referencias.
;; apply-env retorna el valor desreferenciado.
;; apply-env-ref retorna la referencia (para set-exp y freeze-exp).

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (ids  (list-of symbol?))
   (refs (list-of referencia?))
   (env  ambiente?)))

;; apply-env : Ambiente x Symbol -> SchemeVal
;; Propósito: busca la referencia ligada a id en el ambiente y
;; retorna (deref ref). Aplica shadowing.
(define apply-env
  (lambda (env id)
    (deref (apply-env-ref env id))))

;; apply-env-ref : Ambiente x Symbol -> Referencia
;; Propósito: retorna la referencia ligada a id sin desreferenciar.
;; La usan set-exp y freeze-exp para inspeccionar y mutar la marca.
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

;; ambiente-inicial : () -> Ambiente
;; Propósito: devuelve el ambiente inicial con:
;;   let: x=4, y=2, z=5  (marca 'let)
;;   var: a=4, b=5, c=6  (marca 'var)
(define ambiente-inicial
  (lambda ()
    (ambiente-extendido
     '(a b c)
     (list (newref 4 'var)
           (newref 5 'var)
           (newref 6 'var))
     (ambiente-extendido
      '(x y z)
      (list (newref 4 'let)
            (newref 2 'let)
            (newref 5 'let))
      (ambiente-vacio)))))


;; ------------------------------------------------------------
;; Procedimientos 
;; ------------------------------------------------------------
;; Las clausuras capturan el ambiente de creación, que liga
;; identificadores con referencias. Esto permite que el cuerpo
;; del procedimiento lea y escriba sobre las variables 'var capturadas.
;; Los parámetros formales se ligan con marca 'let (paso por valor).

;; any? : Any -> Boolean
;; Propósito: predicado laxo para el cuerpo de la clausura.
(define any?
  (lambda (_) #t))

(define-datatype procval procval?
  (closure
   (params (list-of symbol?))
   (body   any?)
   (env    ambiente?)))

;; scheme-value? : Any -> Boolean
;; Propósito: predicado para valores expresados/denotados de MiniLang.
(define scheme-value?
  (lambda (v)
    (or (number? v) (boolean? v) (procval? v))))

;; apply-procedure : Procval x (Listof SchemeVal) -> SchemeVal
;; Propósito: extiende el ambiente de creación con los parámetros
;; formales ligados a referencias 'let nuevas y evalúa el cuerpo.
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (params body saved-env)
        (if (= (length params) (length args))
            (let ((new-refs (map (lambda (v) (newref v 'let)) args)))
              (evaluar-expresion
               body
               (ambiente-extendido params new-refs saved-env)))
            (eopl:error 'apply-procedure
                        "Aridad incorrecta: esperaba ~s argumentos, recibió ~s"
                        (length params) (length args)))))))


;; ------------------------------------------------------------
;; Evaluador
;; ------------------------------------------------------------

;; evaluar-programa : Programa -> SchemeVal
;; Propósito: punto de entrada del intérprete.
;; Evalúa la expresión del programa en el ambiente inicial.
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp)
        (evaluar-expresion exp (ambiente-inicial))))))

;; evaluar-expresion : Expresion x Ambiente -> SchemeVal
;; Propósito: recorre el AST y evalúa cada tipo de expresión.
;; Cubre todos los casos del Taller 2 más los nuevos del Taller 3.
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp

      ;; lit-exp : retorna directamente el valor numérico
      (lit-exp (dato) dato)

      ;; ident-exp : busca el valor ligado en el ambiente (antes var-exp)
      (ident-exp (id)
        (apply-env env id))

      ;; true-exp / false-exp : literales booleanos
      (true-exp () #t)
      (false-exp () #f)

      ;; prim-exp : evalúa operandos y aplica la primitiva
      (prim-exp (prim rands)
        (let ((args (map (lambda (r) (evaluar-expresion r env)) rands)))
          (evaluar-primitiva prim args)))

      ;; if-exp : condicional if-then-else
      ;; La condición debe ser booleana
      (if-exp (test true-branch false-branch)
        (let ((test-val (evaluar-expresion test env)))
          (if (boolean? test-val)
              (if test-val
                  (evaluar-expresion true-branch env)
                  (evaluar-expresion false-branch env))
              (eopl:error 'if-exp
                          "La condición debe ser booleana, recibió: ~s"
                          test-val))))

      ;; let-exp : ligaduras inmutables (marca 'let)
      ;; Evalúa todas las expresiones en el ambiente actual (simultáneo)
      ;; y extiende el ambiente con nuevas referencias marcadas 'let.
      (let-exp (ids types rands body)
        (let ((vals (map (lambda (r) (evaluar-expresion r env)) rands)))
          (let ((new-refs (map (lambda (v) (newref v 'let)) vals)))
            (evaluar-expresion
             body
             (ambiente-extendido ids new-refs env)))))

      ;; var-decl-exp : ligaduras mutables (marca 'var)
      ;; Igual que let-exp pero las referencias se marcan 'var.
      (var-decl-exp (ids types rands body)
        (let ((vals (map (lambda (r) (evaluar-expresion r env)) rands)))
          (let ((new-refs (map (lambda (v) (newref v 'var)) vals)))
            (evaluar-expresion
             body
             (ambiente-extendido ids new-refs env)))))

      ;; set-exp : asignación destructiva
      ;; Solo procede si la marca es 'var.
      ;; Si es 'let: error "variable inmutable".
      ;; Si es 'frozen: error "variable congelada".
      (set-exp (id rand)
        (let ((ref (apply-env-ref env id))
              (new-val (evaluar-expresion rand env)))
          (let ((marca (ref-marca ref)))
            (cond
              [(eq? marca 'var)
               (setref! ref new-val)]
              [(eq? marca 'let)
               (eopl:error 'set-exp
                           "No se puede asignar a variable inmutable: ~s"
                           id)]
              [(eq? marca 'frozen)
               (eopl:error 'set-exp
                           "No se puede asignar a variable congelada: ~s"
                           id)]
              [else
               (eopl:error 'set-exp "Marca desconocida ~s en ~s" marca id)]))))

      ;; begin-exp : secuencia de expresiones
      ;; Evalúa todas, retorna el valor de la última.
      (begin-exp (first rest)
        (letrec
            ((loop (lambda (current exprs)
                     (if (null? exprs)
                         current
                         (loop (evaluar-expresion (car exprs) env)
                               (cdr exprs))))))
          (loop (evaluar-expresion first env) rest)))

      ;; freeze-exp : congela una variable mutable
      ;; Solo procede si la marca es 'var.
      ;; Si es 'let: error "no se puede congelar variable inmutable".
      ;; Si es 'frozen: error "ya está congelada".
      (freeze-exp (id)
        (let ((ref (apply-env-ref env id)))
          (let ((marca (ref-marca ref)))
            (cond
              [(eq? marca 'var)
               (congelar! ref)]
              [(eq? marca 'let)
               (eopl:error 'freeze-exp
                           "No se puede congelar una variable inmutable (let): ~s"
                           id)]
              [(eq? marca 'frozen)
               (eopl:error 'freeze-exp
                           "La variable ya está congelada: ~s"
                           id)]
              [else
               (eopl:error 'freeze-exp "Marca desconocida ~s en ~s" marca id)]))))

      ;; proc-exp : crea una clausura capturando el ambiente actual
      ;; Los parámetros se ligan con marca 'let al aplicar el procedimiento.
      (proc-exp (types ids body)
        (closure ids body env))

      ;; app-exp : aplicación de procedimiento
      ;; Evalúa el operador y los operandos, luego aplica.
      (app-exp (rator rands)
        (let ((proc-val (evaluar-expresion rator env))
              (args (map (lambda (r) (evaluar-expresion r env)) rands)))
          (if (procval? proc-val)
              (apply-procedure proc-val args)
              (eopl:error 'app-exp
                          "Se esperaba un procedimiento, recibió: ~s"
                          proc-val))))

      ;; cond-exp : condicional múltiple (del Taller 2)
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
                                     "Condición no booleana: ~s"
                                     test-val)))]))))
          (evaluar-cond conditions actions)))

      ;; let*-exp : ligaduras secuenciales sin tipos (del Taller 2)
      (let*-exp (ids rands body)
        (letrec
            ((extender-secuencial
              (lambda (ids rands env-actual)
                (if (null? ids)
                    (evaluar-expresion body env-actual)
                    (let ((val (evaluar-expresion (car rands) env-actual)))
                      (extender-secuencial
                       (cdr ids)
                       (cdr rands)
                       (ambiente-extendido
                        (list (car ids))
                        (list (newref val 'let))
                        env-actual)))))))
          (extender-secuencial ids rands env)))

      ;; unless-exp : condicional inverso (del Taller 2)
      (unless-exp (test usual except)
        (let ((test-val (evaluar-expresion test env)))
          (if (boolean? test-val)
              (if test-val
                  (evaluar-expresion except env)
                  (evaluar-expresion usual env))
              (eopl:error 'unless-exp
                          "La condición debe ser booleana, recibió: ~s"
                          test-val))))
      )))

;; ------------------------------------------------------------
;; evaluar-primitiva
;; ------------------------------------------------------------
;; evaluar-primitiva : Primitiva x (Listof SchemeVal) -> SchemeVal
;; Propósito: aplica la primitiva indicada a la lista de argumentos
;; ya evaluados. Valida aridad y tipos cuando corresponde.
(define evaluar-primitiva
  (lambda (prim args)
    (cases primitiva prim

      ;; sum-prim : suma n-aria de números
      (sum-prim ()  (apply + args))

      ;; minus-prim : resta n-aria de números
      (minus-prim () (apply - args))

      ;; mult-prim : multiplicación n-aria de números
      (mult-prim () (apply * args))

      ;; div-prim : división n-aria de números
      (div-prim () (apply / args))

      ;; add-prim : incrementa en 1 (unaria)
      (add-prim ()
        (if (= (length args) 1)
            (+ (car args) 1)
            (eopl:error 'add-prim "add1 es unaria, recibió ~s argumentos"
                        (length args))))

      ;; sub-prim : decrementa en 1 (unaria)
      (sub-prim ()
        (if (= (length args) 1)
            (- (car args) 1)
            (eopl:error 'sub-prim "sub1 es unaria, recibió ~s argumentos"
                        (length args))))

      ;; mayor-prim : comparación > n-aria
      (mayor-prim ()    (apply > args))

      ;; mayorigual-prim : comparación >= n-aria
      (mayorigual-prim () (apply >= args))

      ;; menor-prim : comparación < n-aria
      (menor-prim ()    (apply < args))

      ;; menorigual-prim : comparación <= n-aria
      (menorigual-prim () (apply <= args))

      ;; igual-prim : comparación == n-aria
      (igual-prim ()    (apply = args))

      ;; not-prim : negación lógica unaria
      (not-prim ()
        (if (= (length args) 1)
            (let ((v (car args)))
              (if (boolean? v)
                  (not v)
                  (eopl:error 'not-prim
                              "not requiere booleano, recibió: ~s" v)))
            (eopl:error 'not-prim "not es unaria, recibió ~s argumentos"
                        (length args))))

      ;; and-prim : conjunción lógica binaria
      (and-prim ()
        (if (= (length args) 2)
            (let ((v1 (car args)) (v2 (cadr args)))
              (if (and (boolean? v1) (boolean? v2))
                  (and v1 v2)
                  (eopl:error 'and-prim
                              "and requiere dos booleanos, recibió: ~s ~s" v1 v2)))
            (eopl:error 'and-prim "and requiere exactamente 2 argumentos")))

      ;; or-prim : disyunción lógica binaria
      (or-prim ()
        (if (= (length args) 2)
            (let ((v1 (car args)) (v2 (cadr args)))
              (if (and (boolean? v1) (boolean? v2))
                  (or v1 v2)
                  (eopl:error 'or-prim
                              "or requiere dos booleanos, recibió: ~s ~s" v1 v2)))
            (eopl:error 'or-prim "or requiere exactamente 2 argumentos")))

      ;; min-prim : mínimo de n números (n >= 1)
      (min-prim ()
        (if (>= (length args) 1)
            (apply min args)
            (eopl:error 'min-prim "min requiere al menos 1 argumento")))

      ;; max-prim : máximo de n números (n >= 1)
      (max-prim ()
        (if (>= (length args) 1)
            (apply max args)
            (eopl:error 'max-prim "max requiere al menos 1 argumento")))

      ;; mod-prim : residuo de división entera (binaria)
      (mod-prim ()
        (if (= (length args) 2)
            (modulo (car args) (cadr args))
            (eopl:error 'mod-prim "mod requiere exactamente 2 argumentos")))

      ;; pow-prim : potenciación (binaria)
      (pow-prim ()
        (if (= (length args) 2)
            (expt (car args) (cadr args))
            (eopl:error 'pow-prim "pow requiere exactamente 2 argumentos")))
      )))


;; ------------------------------------------------------------
;; REPL
;; ------------------------------------------------------------
(define interpretador
  (sllgen:make-rep-loop
   "MiniLang> "
   (lambda (pgm) (evaluar-programa pgm))
   (sllgen:make-stream-parser
    especificacion-lexica especificacion-gramatical)))


(provide (all-defined-out))
