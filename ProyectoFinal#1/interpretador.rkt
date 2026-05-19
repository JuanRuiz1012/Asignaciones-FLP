#lang eopl

;; ============================================================
;; Proyecto final — Interpretador StreamLang-UV
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710,
;;          JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701,
;;          JUAN FELIPE RUIZ 2359397
;; ============================================================

;; ============================================================
;; Especificación léxica — Juan Felipe Ruiz
;; ============================================================

(define especificacion-lexica
  '(
    (espacio-blanco (whitespace) skip)

    ;; Comentarios de línea: -- hasta fin de línea
    (comentario-linea
     ("-" "-" (arbno (not #\newline)))
     skip)

    ;; Comentarios de bloque: {- ... -}
    ;; Acepta cualquier contenido sin } adentro
    (comentario-bloque
     ("{" "-" (arbno (not #\})) "-" "}")
     skip)

    ;; Números flotantes (antes que enteros para evitar conflicto)
    (flotante
     (digit (arbno digit) "." digit (arbno digit))
     number)
    (flotante
     ("-" digit (arbno digit) "." digit (arbno digit))
     number)

    ;; Números enteros
    (numero
     (digit (arbno digit))
     number)
    (numero
     ("-" digit (arbno digit))
     number)

    ;; Cadenas entre comillas dobles
    ;; SLLGEN entrega el contenido sin las comillas externas — se procesan en value-of
    (cadena
     ("\"" (arbno (not #\")) "\"")
     string)

    ;; Caracteres entre comillas simples
    (caracter
     ("'" (not #\') "'")
     string)

    ;; Constructores: inician con mayúscula A-Z, resto letra o dígito
    ;; Se listan las mayúsculas explícitamente porque 'letter' incluye minúsculas
    (constructor
     ((or "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
          "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
      (arbno (or letter digit "-" "_")))
     symbol)

    ;; Wildcard: guión bajo solo
    (wildcard
     ("_")
     symbol)

    ;; Identificadores: inician con minúscula explícita, pueden tener - _ ?
    ;; Se listan las minúsculas explícitamente para no capturar mayúsculas
    (identificador
     ((or "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
          "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
      (arbno (or letter digit "-" "_" "?")))
     symbol)
    ))

;; ============================================================
;; Especificación gramatical
;; ============================================================

(define especificacion-gramatical
  '(
    ;; Programa: cero o más decl-tipo seguidas de una expresión
    (programa
     ((arbno decl-tipo) expresion)
     a-program)

    ;; Declaración de tipo algebraico
    (decl-tipo
     ("datatype" constructor "=" variante
      (arbno "|" variante))
     a-datatype-decl)

    ;; Variante con cero o más campos
    (variante
     (constructor "(" (separated-list identificador ",") ")")
     a-variant)

    ;; ---- Expresiones ----

    (expresion (flotante) float-exp)
    (expresion (numero) num-exp)
    (expresion (cadena) str-exp)
    (expresion (caracter) char-exp)

    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("void") void-exp)
    (expresion ("empty-stream") empty-stream-exp)

    ;; let: ligaduras inmutables
    (expresion
     ("let" identificador "=" expresion
      (arbno "," identificador "=" expresion)
      "in" expresion "end")
     let-exp)

    ;; var: ligaduras mutables
    (expresion
     ("var" identificador "=" expresion
      (arbno "," identificador "=" expresion)
      "in" expresion "end")
     var-decl-exp)

    ;; set: asignación
    (expresion
     ("set" identificador ":=" expresion)
     set-exp)

    ;; freeze: congela una variable mutable
    (expresion
     ("freeze" identificador)
     freeze-exp)

    ;; begin: secuenciación
    (expresion
     ("begin" expresion (arbno ";" expresion) "end")
     begin-exp)

    ;; if/elif/else
    (expresion
     ("if" expresion "then" expresion
      (arbno "elif" expresion "then" expresion)
      "else" expresion "end")
     if-exp)

    ;; Aplicación de primitiva
    (expresion
     (primitiva "(" (separated-list expresion ",") ")")
     prim-exp)

    ;; proc: procedimiento anónimo
    (expresion
     ("proc" "(" (separated-list identificador ",") ")"
      expresion "end")
     proc-exp)

    ;; apply: aplicación de procedimiento
    (expresion
     ("apply" expresion "(" (separated-list expresion ",") ")")
     app-exp)

    ;; letrec: uno o más procedimientos recursivos mutuos
    (expresion
     ("letrec"
      identificador "(" (separated-list identificador ",") ")"
      "=" expresion
      (arbno identificador "(" (separated-list identificador ",") ")"
             "=" expresion)
      "in" expresion "end")
     letrec-exp)

    ;; stream: construcción de stream perezoso
    (expresion
     ("stream" "(" expresion "," expresion ")")
     stream-cons-exp)

    ;; head: primer elemento del stream
    (expresion
     ("head" "(" expresion ")")
     head-exp)

    ;; tail: resto del stream
    (expresion
     ("tail" "(" expresion ")")
     tail-exp)

    ;; stream-null?: predicado de stream vacío
    (expresion
     ("stream-null?" "(" expresion ")")
     stream-null-exp)

    ;; match: pattern matching
    (expresion
     ("match" expresion "with"
      "|" patron "=>" expresion
      (arbno "|" patron "=>" expresion)
      "end")
     match-exp)

    ;; map sobre stream (perezoso)
    (expresion
     ("map" "(" expresion "," expresion ")")
     map-exp)

    ;; filter sobre stream (perezoso)
    (expresion
     ("filter" "(" expresion "," expresion ")")
     filter-exp)

    ;; take: materializa n elementos de un stream
    (expresion
     ("take" "(" expresion "," expresion ")")
     take-exp)

    ;; zip-with: combina dos streams con una función
    (expresion
     ("zip-with" "(" expresion "," expresion "," expresion ")")
     zip-with-exp)

    ;; Constructor de tipo algebraico
    (expresion
     (constructor "(" (separated-list expresion ",") ")")
     ctor-exp)

    ;; Variable (debe ir después de palabras reservadas)
    (expresion (identificador) var-exp)

    ;; ---- Patrones ----

    (patron (numero) num-pat)
    (patron (cadena) str-pat)
    (patron ("true") true-pat)
    (patron ("false") false-pat)
    (patron ("empty-stream") empty-stream-pat)

    ;; Wildcard usa el token wildcard (símbolo _)
    (patron (wildcard) wildcard-pat)

    (patron
     ("stream" "(" patron "," patron ")")
     stream-pat)

    (patron
     (constructor "(" (separated-list patron ",") ")")
     ctor-pat)

    (patron (identificador) var-pat)

    ;; ---- Primitivas ----

    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)

    (primitiva (">=") mayorigual-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<") menor-prim)
    (primitiva ("==") igual-prim)
    (primitiva ("!=") distinto-prim)

    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("not") not-prim)

    (primitiva ("concat") concat-prim)
    (primitiva ("length") length-prim)

    (primitiva ("to-string") tostring-prim)
    (primitiva ("to-number") tonumber-prim)
    ))

;; Genera define-datatypes desde la gramática
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; scan&parse : String -> Programa
(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; scanner : String -> List-of-tokens
(define scanner
  (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))


;; ============================================================
;; Utilidades — compatibles con #lang eopl
;; ============================================================

;; zip-map : (A B -> C) x (Listof A) x (Listof B) -> (Listof C)
;; Propósito: map sobre dos listas simultáneamente.
;; Reemplaza (map f lst1 lst2) que no está garantizado en eopl.
(define zip-map
  (lambda (f lst1 lst2)
    (if (null? lst1)
        '()
        (cons (f (car lst1) (car lst2))
              (zip-map f (cdr lst1) (cdr lst2))))))

;; zip-map3 : (A B C -> D) x (Listof A) x (Listof B) x (Listof C) -> (Listof D)
;; Propósito: map sobre tres listas simultáneamente (para letrec).
(define zip-map3
  (lambda (f lst1 lst2 lst3)
    (if (null? lst1)
        '()
        (cons (f (car lst1) (car lst2) (car lst3))
              (zip-map3 f (cdr lst1) (cdr lst2) (cdr lst3))))))

;; make-list-of : (Listof A) x B -> (Listof B)
;; Propósito: crea una lista del mismo largo que lst, con valor val.
;; Reemplaza (map (lambda (_) val) lst).
(define make-list-of
  (lambda (lst val)
    (if (null? lst)
        '()
        (cons val (make-list-of (cdr lst) val)))))


;; ============================================================
;; Unparse — JHORMAN RICARDO LOAIZA
;; ============================================================

;; unparse-lista : (Listof T) x (T -> String) x String -> String
(define unparse-lista
  (lambda (lst fn sep)
    (if (null? lst)
        ""
        (let construir ((primero (fn (car lst)))
                        (resto (cdr lst)))
          (if (null? resto)
              primero
              (construir
               (string-append primero sep (fn (car resto)))
               (cdr resto)))))))

;; unparse-prim : primitiva -> String
(define unparse-prim
  (lambda (prim)
    (cases primitiva prim
      (sum-prim ()        "+")
      (minus-prim ()      "-")
      (mult-prim ()       "*")
      (div-prim ()        "/")
      (mod-prim ()        "%")
      (mayorigual-prim () ">=")
      (menorigual-prim () "<=")
      (mayor-prim ()      ">")
      (menor-prim ()      "<")
      (igual-prim ()      "==")
      (distinto-prim ()   "!=")
      (and-prim ()        "and")
      (or-prim ()         "or")
      (not-prim ()        "not")
      (concat-prim ()     "concat")
      (length-prim ()     "length")
      (tostring-prim ()   "to-string")
      (tonumber-prim ()   "to-number"))))

;; unparse-pat : patron -> String
(define unparse-pat
  (lambda (p)
    (cases patron p
      (num-pat (n)         (number->string n))
      (str-pat (s)         s)
      (true-pat ()         "true")
      (false-pat ()        "false")
      (empty-stream-pat () "empty-stream")
      (wildcard-pat (w)    "_")
      (stream-pat (h t)
        (string-append "stream(" (unparse-pat h) ", " (unparse-pat t) ")"))
      (ctor-pat (nombre subpats)
        (string-append (symbol->string nombre)
                       "(" (unparse-lista subpats unparse-pat ", ") ")"))
      (var-pat (id)        (symbol->string id)))))

;; unparse-exp : expresion -> String
(define unparse-exp
  (lambda (exp)
    (cases expresion exp

      (num-exp (n)    (number->string n))
      (float-exp (f)  (number->string f))
      ;; s ya viene con las comillas que capturó SLLGEN — se devuelve tal cual
      (str-exp (s)    s)
      (char-exp (c)   c)
      (true-exp ()    "true")
      (false-exp ()   "false")
      (void-exp ()    "void")
      (empty-stream-exp () "empty-stream")
      (var-exp (id)   (symbol->string id))

      (let-exp (id1 rand1 ids rands body)
        (let* ((todos-ids  (cons id1 ids))
               (todas-exps (cons rand1 rands))
               (pares (zip-map
                       (lambda (i r)
                         (string-append ", " (symbol->string i)
                                        " = " (unparse-exp r)))
                       ids rands))
               (resto-str (apply string-append pares)))
          (string-append
           "let " (symbol->string id1) " = " (unparse-exp rand1)
           resto-str
           " in " (unparse-exp body) " end")))

      (var-decl-exp (id1 rand1 ids rands body)
        (let* ((pares (zip-map
                       (lambda (i r)
                         (string-append ", " (symbol->string i)
                                        " = " (unparse-exp r)))
                       ids rands))
               (resto-str (apply string-append pares)))
          (string-append
           "var " (symbol->string id1) " = " (unparse-exp rand1)
           resto-str
           " in " (unparse-exp body) " end")))

      (set-exp (id val)
        (string-append "set " (symbol->string id) " := " (unparse-exp val)))

      (freeze-exp (id)
        (string-append "freeze " (symbol->string id)))

      (begin-exp (e1 resto)
        (let ((partes (map (lambda (e) (string-append "; " (unparse-exp e))) resto)))
          (string-append
           "begin " (unparse-exp e1)
           (apply string-append partes)
           " end")))

      (if-exp (cond1 then1 elif-conds elif-thens else-e)
        (let ((partes (zip-map
                       (lambda (c t)
                         (string-append " elif " (unparse-exp c)
                                        " then " (unparse-exp t)))
                       elif-conds elif-thens)))
          (string-append
           "if " (unparse-exp cond1) " then " (unparse-exp then1)
           (apply string-append partes)
           " else " (unparse-exp else-e) " end")))

      (prim-exp (prim args)
        (string-append
         (unparse-prim prim)
         "(" (unparse-lista args unparse-exp ", ") ")"))

      (proc-exp (ids body)
        (string-append
         "proc(" (unparse-lista ids (lambda (i) (symbol->string i)) ", ")
         ") " (unparse-exp body) " end"))

      (app-exp (rator rands)
        (string-append
         "apply " (unparse-exp rator)
         "(" (unparse-lista rands unparse-exp ", ") ")"))

      (letrec-exp (name1 ids1 body1 names idss bodies cuerpo)
        (let ((extras (zip-map3
                       (lambda (n i b)
                         (string-append
                          "\n" (symbol->string n)
                          "(" (unparse-lista i (lambda (x) (symbol->string x)) ", ") ") = "
                          (unparse-exp b)))
                       names idss bodies)))
          (string-append
           "letrec "
           (symbol->string name1)
           "(" (unparse-lista ids1 (lambda (i) (symbol->string i)) ", ") ") = "
           (unparse-exp body1)
           (apply string-append extras)
           " in " (unparse-exp cuerpo) " end")))

      (stream-cons-exp (head tail)
        (string-append "stream(" (unparse-exp head)
                       ", " (unparse-exp tail) ")"))

      (head-exp (s)
        (string-append "head(" (unparse-exp s) ")"))

      (tail-exp (s)
        (string-append "tail(" (unparse-exp s) ")"))

      (stream-null-exp (s)
        (string-append "stream-null?(" (unparse-exp s) ")"))

      (match-exp (val pat1 exp1 pats exps)
        (let ((casos (zip-map
                      (lambda (p e)
                        (string-append "\n| " (unparse-pat p)
                                       " => " (unparse-exp e)))
                      pats exps)))
          (string-append
           "match " (unparse-exp val) " with"
           "\n| " (unparse-pat pat1) " => " (unparse-exp exp1)
           (apply string-append casos)
           "\nend")))

      (map-exp (f s)
        (string-append "map(" (unparse-exp f) ", " (unparse-exp s) ")"))

      (filter-exp (pred s)
        (string-append "filter(" (unparse-exp pred) ", " (unparse-exp s) ")"))

      (take-exp (n s)
        (string-append "take(" (unparse-exp n) ", " (unparse-exp s) ")"))

      (zip-with-exp (f s1 s2)
        (string-append "zip-with(" (unparse-exp f) ", "
                       (unparse-exp s1) ", " (unparse-exp s2) ")"))

      (ctor-exp (nombre args)
        (string-append
         (symbol->string nombre)
         "(" (unparse-lista args unparse-exp ", ") ")"))
      )))

;; unparse-variante : variante -> String
(define unparse-variante
  (lambda (v)
    (cases variante v
      (a-variant (nombre campos)
        (string-append
         (symbol->string nombre)
         "(" (unparse-lista campos (lambda (c) (symbol->string c)) ", ") ")")))))

;; unparse-decl : decl-tipo -> String
(define unparse-decl
  (lambda (d)
    (cases decl-tipo d
      (a-datatype-decl (nombre primera resto)
        (let ((extras (map (lambda (v)
                             (string-append " | " (unparse-variante v)))
                           resto)))
          (string-append
           "datatype " (symbol->string nombre) " = "
           (unparse-variante primera)
           (apply string-append extras)
           "\n"))))))

;; unparse : programa -> String
(define unparse
  (lambda (pgm)
    (cases programa pgm
      (a-program (decls exp)
        (string-append
         (apply string-append (map unparse-decl decls))
         (unparse-exp exp))))))


;; ============================================================
;; Store — JHORMAN RICARDO LOAIZA
;; ============================================================

;; El store es un vector mutable que crece dinámicamente.
;; Cada posición: (cons valor frozen?)

(define the-store 'uninitialized)

;; initialize-store! : () -> Void
(define initialize-store!
  (lambda ()
    (set! the-store (make-vector 0))))

;; Ref? : Any -> Boolean
(define Ref?
  (lambda (v)
    (integer? v)))

;; newref : SchemeVal -> Ref
;; Copia manual del vector — vector-copy! no existe en #lang eopl
(define newref
  (lambda (val)
    (let* ((n (vector-length the-store))
           (nuevo-store (make-vector (+ n 1))))
      (let loop ((i 0))
        (if (< i n)
            (begin
              (vector-set! nuevo-store i (vector-ref the-store i))
              (loop (+ i 1)))
            'done))
      (vector-set! nuevo-store n (cons val #f))
      (set! the-store nuevo-store)
      n)))

;; deref : Ref -> SchemeVal
(define deref
  (lambda (ref)
    (car (vector-ref the-store ref))))

;; setref! : Ref x SchemeVal -> Void
(define setref!
  (lambda (ref val)
    (let ((celda (vector-ref the-store ref)))
      (if (cdr celda)
          (eopl:error 'setref! "Intento de modificar una referencia congelada: ~s" ref)
          (vector-set! the-store ref (cons val #f))))))

;; freezeref! : Ref -> Void
(define freezeref!
  (lambda (ref)
    (let ((celda (vector-ref the-store ref)))
      (vector-set! the-store ref (cons (car celda) #t)))))

;; frozen? : Ref -> Boolean
(define frozen?
  (lambda (ref)
    (cdr (vector-ref the-store ref))))


;; ============================================================
;; Ambiente — JUAN DIEGO OSPINA
;; ============================================================

(define scheme-value?
  (lambda (v) #t))

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (ids      (list-of symbol?))
   (vals     (list-of scheme-value?))
   (mutables (list-of boolean?))
   (env      ambiente?))
  (ambiente-recursivo
   (names     (list-of symbol?))
   (params    (list-of (list-of symbol?)))
   (bodies    (list-of scheme-value?))
   (saved-env ambiente?)))

;; buscar-en-listas : Symbol x (Listof Symbol) x (Listof V) x (Listof Bool)
;;                    -> (cons V Bool) | #f
(define buscar-en-listas
  (lambda (id ids vals muts)
    (if (null? ids)
        #f
        (if (equal? (car ids) id)
            (cons (car vals) (car muts))
            (buscar-en-listas id (cdr ids) (cdr vals) (cdr muts))))))

;; apply-env : Ambiente x Symbol -> (cons SchemeVal Boolean)
(define apply-env
  (lambda (env id)
    (cases ambiente env
      (ambiente-vacio ()
        (eopl:error 'apply-env "Variable no ligada: ~s" id))

      (ambiente-extendido (ids vals muts old-env)
        (let ((resultado (buscar-en-listas id ids vals muts)))
          (if resultado
              resultado
              (apply-env old-env id))))

      (ambiente-recursivo (names params bodies saved-env)
        (let buscar ((ns names) (ps params) (bs bodies))
          (if (null? ns)
              (apply-env saved-env id)
              (if (equal? (car ns) id)
                  (cons (make-clausura (car ps) (car bs) env) #f)
                  (buscar (cdr ns) (cdr ps) (cdr bs)))))))))

;; extend-env : (Listof Symbol) x (Listof Val) x (Listof Bool) x Env -> Env
(define extend-env
  (lambda (ids vals muts env)
    (ambiente-extendido ids vals muts env)))

;; ambiente-inicial : () -> Ambiente
(define ambiente-inicial
  (lambda ()
    (ambiente-vacio)))


;; ============================================================
;; Valores expresados — MAURICIO ALEJANDRO ROJAS
;; ============================================================

;; ----- Clausuras -----

(define-datatype clausura clausura?
  (make-clausura
   (params    (list-of symbol?))
   (body      expresion?)
   (saved-env ambiente?)))

;; Extractores manuales para clausura (casos de define-datatype)
(define clausura-params
  (lambda (c)
    (cases clausura c
      (make-clausura (p b e) p))))

(define clausura-body
  (lambda (c)
    (cases clausura c
      (make-clausura (p b e) b))))

(define clausura-env
  (lambda (c)
    (cases clausura c
      (make-clausura (p b e) e))))

;; ----- Streams perezosos -----

(define-datatype stream-par stream-par?
  (make-stream-par
   (head-val  scheme-value?)
   (thunk-val scheme-value?)))

;; Extractores manuales para stream-par
(define stream-head
  (lambda (s)
    (cases stream-par s
      (make-stream-par (h t) h))))

(define stream-thunk
  (lambda (s)
    (cases stream-par s
      (make-stream-par (h t) t))))

;; stream-empty? : Any -> Boolean
(define stream-empty?
  (lambda (val)
    (eq? val 'empty-stream)))

;; ----- Variantes de tipos algebraicos -----

(define-datatype variante-val variante-val?
  (make-variante-val
   (nombre symbol?)
   (campos list?)))

;; Extractores manuales para variante-val
(define variante-nombre
  (lambda (v)
    (cases variante-val v
      (make-variante-val (n c) n))))

(define variante-campos
  (lambda (v)
    (cases variante-val v
      (make-variante-val (n c) c))))


;; ============================================================
;; Aplicación de primitivas — JUAN FELIPE RUIZ
;; ============================================================

;; apply-primitive : Primitiva x (Listof Val) -> Val
(define apply-primitive
  (lambda (prim args)
    (cases primitiva prim

      ;; ---- Aritméticas ----
      (sum-prim ()
        (apply + args))

      (minus-prim ()
        (if (= (length args) 1)
            (- (car args))
            (apply - args)))

      (mult-prim ()
        (apply * args))

      ;; Usa if en lugar de when (when no existe en #lang eopl)
      (div-prim ()
        (if (= (cadr args) 0)
            (eopl:error 'div-prim "División por cero")
            (/ (car args) (cadr args))))

      (mod-prim ()
        (if (= (cadr args) 0)
            (eopl:error 'mod-prim "Módulo por cero")
            (modulo (car args) (cadr args))))

      ;; ---- Relacionales ----
      (mayor-prim ()      (> (car args) (cadr args)))
      (menor-prim ()      (< (car args) (cadr args)))
      (mayorigual-prim () (>= (car args) (cadr args)))
      (menorigual-prim () (<= (car args) (cadr args)))

      (igual-prim ()
        (equal? (car args) (cadr args)))

      (distinto-prim ()
        (not (equal? (car args) (cadr args))))

      ;; ---- Booleanas ----
      (and-prim ()
        (if (car args) (cadr args) #f))

      (or-prim ()
        (if (car args) #t (cadr args)))

      (not-prim ()
        (not (car args)))

      ;; ---- Cadenas ----
      (concat-prim ()
        (string-append (car args) (cadr args)))

      (length-prim ()
        (string-length (car args)))

      ;; ---- Conversión ----
      (tostring-prim ()
        (let ((v (car args)))
          (cond
            [(number? v)   (number->string v)]
            [(boolean? v)  (if v "true" "false")]
            [(string? v)   v]
            [(eq? v 'void) "void"]
            [else (eopl:error 'tostring-prim
                              "No se puede convertir a string: ~s" v)])))

      (tonumber-prim ()
        (let* ((s (car args))
               (n (string->number s)))
          (if n
              n
              (eopl:error 'tonumber-prim
                          "No se puede convertir a número: ~s" s)))))))


;; ============================================================
;; value-of — JHORMAN RICARDO LOAIZA
;; ============================================================

;; value-of : Expresion x Ambiente -> Val
(define value-of
  (lambda (exp env)
    (cases expresion exp

      ;; ---- Literales ----
      (num-exp (n) n)
      (float-exp (f) f)
      ;; SLLGEN entrega cadenas CON las comillas incluidas en el valor string.
      ;; Se stripea el primer y último carácter (las comillas " o ').
      (str-exp (s)
        (substring s 1 (- (string-length s) 1)))
      (char-exp (c)
        (substring c 1 (- (string-length c) 1)))
      (true-exp ()  #t)
      (false-exp () #f)
      (void-exp () 'void)
      (empty-stream-exp () 'empty-stream)

      ;; ---- Variable ----
      ;; Si es var (mutable), desreferencia el store.
      ;; Si es let/param (inmutable), retorna el valor directamente.
      (var-exp (id)
        (let* ((resultado (apply-env env id))
               (val  (car resultado))
               (mut? (cdr resultado)))
          (if mut?
              (deref val)
              val)))

      ;; ---- Let (ligaduras inmutables) ----
      (let-exp (id1 rand1 ids rands body)
        (let* ((todos-ids  (cons id1 ids))
               (todas-exps (cons rand1 rands))
               (vals    (map (lambda (e) (value-of e env)) todas-exps))
               (muts    (make-list-of todos-ids #f))
               (new-env (extend-env todos-ids vals muts env)))
          (value-of body new-env)))

      ;; ---- Var (ligaduras mutables) ----
      (var-decl-exp (id1 rand1 ids rands body)
        (let* ((todos-ids  (cons id1 ids))
               (todas-exps (cons rand1 rands))
               (vals    (map (lambda (e) (value-of e env)) todas-exps))
               (refs    (map newref vals))
               (muts    (make-list-of todos-ids #t))
               (new-env (extend-env todos-ids refs muts env)))
          (value-of body new-env)))

      ;; ---- Set (asignación) ----
      (set-exp (id val-exp)
        (let* ((resultado (apply-env env id))
               (ref  (car resultado))
               (mut? (cdr resultado)))
          (if (not mut?)
              (eopl:error 'set-exp
                          "No se puede asignar a variable inmutable: ~s" id)
              (begin
                (setref! ref (value-of val-exp env))
                'void))))

      ;; ---- Freeze (congelamiento) ----
      (freeze-exp (id)
        (let* ((resultado (apply-env env id))
               (ref  (car resultado))
               (mut? (cdr resultado)))
          (if (not mut?)
              (eopl:error 'freeze-exp
                          "No se puede congelar una variable let: ~s" id)
              (begin
                (freezeref! ref)
                'void))))

      ;; ---- Begin (secuenciación) ----
      (begin-exp (e1 resto)
        (let recorrer ((val (value-of e1 env))
                       (exps resto))
          (if (null? exps)
              val
              (recorrer (value-of (car exps) env)
                        (cdr exps)))))

      ;; ---- If / elif / else ----
      (if-exp (cond1 then1 elif-conds elif-thens else-e)
        (let ((v (value-of cond1 env)))
          (if (not (boolean? v))
              (eopl:error 'if-exp
                          "La condición debe ser booleana, se obtuvo: ~s" v)
              (if v
                  (value-of then1 env)
                  (let buscar-elif ((cs elif-conds) (ts elif-thens))
                    (if (null? cs)
                        (value-of else-e env)
                        (let ((cv (value-of (car cs) env)))
                          (if (not (boolean? cv))
                              (eopl:error 'if-exp
                                          "Condición elif debe ser booleana: ~s" cv)
                              (if cv
                                  (value-of (car ts) env)
                                  (buscar-elif (cdr cs) (cdr ts)))))))))))

      ;; ---- Primitivas ----
      (prim-exp (prim arg-exps)
        (let ((args (map (lambda (e) (value-of e env)) arg-exps)))
          (apply-primitive prim args)))

      ;; ---- Procedimientos ----
      (proc-exp (ids body)
        (make-clausura ids body env))

      ;; ---- Aplicación ----
      (app-exp (rator rand-exps)
        (let ((proc-val (value-of rator env))
              (args     (map (lambda (e) (value-of e env)) rand-exps)))
          (if (not (clausura? proc-val))
              (eopl:error 'app-exp
                          "Se esperaba una clausura, se obtuvo: ~s" proc-val)
              (let* ((params  (clausura-params proc-val))
                     (body    (clausura-body   proc-val))
                     (c-env   (clausura-env    proc-val))
                     (muts    (make-list-of params #f))
                     (new-env (extend-env params args muts c-env)))
                (value-of body new-env)))))

      ;; ---- Letrec (recursión mutua) ----
      (letrec-exp (name1 ids1 body1 names idss bodies cuerpo)
        (let* ((todos-names  (cons name1 names))
               (todos-params (cons ids1 idss))
               (todos-bodies (cons body1 bodies))
               (rec-env (ambiente-recursivo
                         todos-names
                         todos-params
                         todos-bodies
                         env)))
          (value-of cuerpo rec-env)))

      ;; ---- Streams (stub Entrega 3) ----
      (stream-cons-exp (head-e tail-e)
        (eopl:error 'value-of "TODO Entrega 3: stream"))

      (head-exp (s-e)
        (eopl:error 'value-of "TODO Entrega 3: head"))

      (tail-exp (s-e)
        (eopl:error 'value-of "TODO Entrega 3: tail"))

      (stream-null-exp (s-e)
        (eopl:error 'value-of "TODO Entrega 3: stream-null?"))

      (map-exp (f-e s-e)
        (eopl:error 'value-of "TODO Entrega 3: map"))

      (filter-exp (pred-e s-e)
        (eopl:error 'value-of "TODO Entrega 3: filter"))

      (take-exp (n-e s-e)
        (eopl:error 'value-of "TODO Entrega 3: take"))

      (zip-with-exp (f-e s1-e s2-e)
        (eopl:error 'value-of "TODO Entrega 3: zip-with"))

      ;; ---- Datatypes y match (stub Entrega 3) ----
      (ctor-exp (nombre arg-exps)
        (eopl:error 'value-of "TODO Entrega 3: constructor ~s" nombre))

      (match-exp (val-e pat1 exp1 pats exps)
        (eopl:error 'value-of "TODO Entrega 3: match"))
      )))


;; ============================================================
;; value-of-program y run
;; ============================================================

;; value-of-program : Programa -> Val
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases programa pgm
      (a-program (decls exp)
        (value-of exp (ambiente-inicial))))))

;; run : String -> Val
(define run
  (lambda (src)
    (value-of-program (scan&parse src))))

(provide (all-defined-out))
