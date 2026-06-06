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
;;  SECCIÓN 1 — JUAN FELIPE RUIZ
;;  Especificación léxica, gramatical y unparser
;; ============================================================

(define especificacion-lexica
  '(
    (espacio-blanco (whitespace) skip)

    ;; Comentarios de línea: -- hasta fin de línea
    (comentario-linea
     ("-" "-" (arbno (not #\newline)))
     skip)

    ;; Comentarios de bloque: {- ... -}
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
    (cadena
     ("\"" (arbno (not #\")) "\"")
     string)

    ;; Caracteres entre comillas simples
    (caracter
     ("'" (not #\') "'")
     string)

    ;; Constructores: inician con mayúscula A-Z
    (constructor
     ((or "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
          "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
      (arbno (or letter digit "-" "_")))
     symbol)

    ;; Wildcard: guión bajo solo
    (wildcard
     ("_")
     symbol)

    ;; Identificadores: inician con minúscula
    (identificador
     ((or "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
          "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
      (arbno (or letter digit "-" "_" "?")))
     symbol)
    ))

(define especificacion-gramatical
  '(
    (programa
     ((arbno decl-tipo) expresion)
     a-program)

    (decl-tipo
     ("datatype" constructor "=" variante
      (arbno "|" variante))
     a-datatype-decl)

    (variante
     (constructor "(" (separated-list identificador ",") ")")
     a-variant)

    (expresion (flotante) float-exp)
    (expresion (numero) num-exp)
    (expresion (cadena) str-exp)
    (expresion (caracter) char-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("void") void-exp)
    (expresion ("empty-stream") empty-stream-exp)

    (expresion
     ("let" identificador "=" expresion
      (arbno "," identificador "=" expresion)
      "in" expresion "end")
     let-exp)

    (expresion
     ("var" identificador "=" expresion
      (arbno "," identificador "=" expresion)
      "in" expresion "end")
     var-decl-exp)

    (expresion
     ("set" identificador ":=" expresion)
     set-exp)

    (expresion
     ("freeze" identificador)
     freeze-exp)

    (expresion
     ("begin" expresion (arbno ";" expresion) "end")
     begin-exp)

    (expresion
     ("if" expresion "then" expresion
      (arbno "elif" expresion "then" expresion)
      "else" expresion "end")
     if-exp)

    (expresion
     (primitiva "(" (separated-list expresion ",") ")")
     prim-exp)

    (expresion
     ("proc" "(" (separated-list identificador ",") ")"
      expresion "end")
     proc-exp)

    (expresion
     ("apply" expresion "(" (separated-list expresion ",") ")")
     app-exp)

    (expresion
     ("letrec"
      identificador "(" (separated-list identificador ",") ")"
      "=" expresion
      (arbno identificador "(" (separated-list identificador ",") ")"
             "=" expresion)
      "in" expresion "end")
     letrec-exp)

    (expresion
     ("stream" "(" expresion "," expresion ")")
     stream-cons-exp)

    (expresion
     ("head" "(" expresion ")")
     head-exp)

    (expresion
     ("tail" "(" expresion ")")
     tail-exp)

    (expresion
     ("stream-null?" "(" expresion ")")
     stream-null-exp)

    (expresion
     ("match" expresion "with"
      "|" patron "=>" expresion
      (arbno "|" patron "=>" expresion)
      "end")
     match-exp)

    (expresion
     ("map" "(" expresion "," expresion ")")
     map-exp)

    (expresion
     ("filter" "(" expresion "," expresion ")")
     filter-exp)

    (expresion
     ("take" "(" expresion "," expresion ")")
     take-exp)

    (expresion
     ("zip-with" "(" expresion "," expresion "," expresion ")")
     zip-with-exp)

    (expresion
     (constructor "(" (separated-list expresion ",") ")")
     ctor-exp)

    (expresion (identificador) var-exp)

    ;; Patrones
    (patron (numero) num-pat)
    (patron (cadena) str-pat)
    (patron ("true") true-pat)
    (patron ("false") false-pat)
    (patron ("empty-stream") empty-stream-pat)
    (patron (wildcard) wildcard-pat)
    (patron
     ("stream" "(" patron "," patron ")")
     stream-pat)
    (patron
     (constructor "(" (separated-list patron ",") ")")
     ctor-pat)
    (patron (identificador) var-pat)

    ;; Primitivas
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

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

(define scanner
  (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))


;; ------------------------------------------------------------
;; Utilidades
;; ------------------------------------------------------------

;; zip-map : (A B -> C) x (Listof A) x (Listof B) -> (Listof C)
(define zip-map
  (lambda (f lst1 lst2)
    (if (null? lst1)
        '()
        (cons (f (car lst1) (car lst2))
              (zip-map f (cdr lst1) (cdr lst2))))))

;; zip-map3 : (A B C -> D) x (Listof A) x (Listof B) x (Listof C) -> (Listof D)
(define zip-map3
  (lambda (f lst1 lst2 lst3)
    (if (null? lst1)
        '()
        (cons (f (car lst1) (car lst2) (car lst3))
              (zip-map3 f (cdr lst1) (cdr lst2) (cdr lst3))))))

;; make-list-of : (Listof A) x B -> (Listof B)
(define make-list-of
  (lambda (lst val)
    (if (null? lst)
        '()
        (cons val (make-list-of (cdr lst) val)))))

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
      (str-exp (s)    s)
      (char-exp (c)   c)
      (true-exp ()    "true")
      (false-exp ()   "false")
      (void-exp ()    "void")
      (empty-stream-exp () "empty-stream")
      (var-exp (id)   (symbol->string id))

      (let-exp (id1 rand1 ids rands body)
        (let* ((pares (zip-map
                       (lambda (i r)
                         (string-append ", " (symbol->string i)
                                        " = " (unparse-exp r)))
                       ids rands))
               (resto-str (apply string-append pares)))
          (string-append
           "let " (symbol->string id1) " = " (unparse-exp rand1)
           resto-str " in " (unparse-exp body) " end")))

      (var-decl-exp (id1 rand1 ids rands body)
        (let* ((pares (zip-map
                       (lambda (i r)
                         (string-append ", " (symbol->string i)
                                        " = " (unparse-exp r)))
                       ids rands))
               (resto-str (apply string-append pares)))
          (string-append
           "var " (symbol->string id1) " = " (unparse-exp rand1)
           resto-str " in " (unparse-exp body) " end")))

      (set-exp (id val)
        (string-append "set " (symbol->string id) " := " (unparse-exp val)))

      (freeze-exp (id)
        (string-append "freeze " (symbol->string id)))

      (begin-exp (e1 resto)
        (let ((partes (map (lambda (e) (string-append "; " (unparse-exp e))) resto)))
          (string-append "begin " (unparse-exp e1)
                         (apply string-append partes) " end")))

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
        (string-append (unparse-prim prim)
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
           "letrec " (symbol->string name1)
           "(" (unparse-lista ids1 (lambda (i) (symbol->string i)) ", ") ") = "
           (unparse-exp body1)
           (apply string-append extras)
           " in " (unparse-exp cuerpo) " end")))

      (stream-cons-exp (head tail)
        (string-append "stream(" (unparse-exp head) ", " (unparse-exp tail) ")"))

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
        (string-append (symbol->string nombre)
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
;;  SECCIÓN 2 — JHORMAN RICARDO LOAIZA
;;  Store y Ambiente
;; ============================================================

;; ------------------------------------------------------------
;; Store — memoria mutable con soporte de freeze
;; Cada celda del vector es (cons valor frozen?).
;; ------------------------------------------------------------

(define the-store 'uninitialized)

(define initialize-store!
  (lambda ()
    (set! the-store (make-vector 0))))

;; newref : Val -> Ref
;; Crea una celda nueva; crece el vector manualmente (no hay vector-copy! en eopl).
(define newref
  (lambda (val)
    (let* ((n (vector-length the-store))
           (nuevo (make-vector (+ n 1))))
      (let loop ((i 0))
        (if (< i n)
            (begin (vector-set! nuevo i (vector-ref the-store i))
                   (loop (+ i 1)))
            'done))
      (vector-set! nuevo n (cons val #f))
      (set! the-store nuevo)
      n)))

;; deref : Ref -> Val
(define deref
  (lambda (ref)
    (car (vector-ref the-store ref))))

;; setref! : Ref x Val -> Void
(define setref!
  (lambda (ref val)
    (let ((celda (vector-ref the-store ref)))
      (if (cdr celda)
          (eopl:error 'setref!
                      "Intento de modificar referencia congelada: ~s" ref)
          (vector-set! the-store ref (cons val #f))))))

;; freezeref! : Ref -> Void
(define freezeref!
  (lambda (ref)
    (let ((celda (vector-ref the-store ref)))
      (vector-set! the-store ref (cons (car celda) #t)))))

;; ------------------------------------------------------------
;; Ambiente
;; Cada ligadura guarda (valor . mutable?).
;; Para var: valor es una Ref; para let/param: valor es directo.
;; ------------------------------------------------------------

(define scheme-value? (lambda (v) #t))

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

(define buscar-en-listas
  (lambda (id ids vals muts)
    (if (null? ids)
        #f
        (if (equal? (car ids) id)
            (cons (car vals) (car muts))
            (buscar-en-listas id (cdr ids) (cdr vals) (cdr muts))))))

;; apply-env : Ambiente x Symbol -> (cons Val Bool)
(define apply-env
  (lambda (env id)
    (cases ambiente env
      (ambiente-vacio ()
        (eopl:error 'apply-env "Variable no ligada: ~s" id))

      (ambiente-extendido (ids vals muts old-env)
        (let ((r (buscar-en-listas id ids vals muts)))
          (if r r (apply-env old-env id))))

      (ambiente-recursivo (names params bodies saved-env)
        (let buscar ((ns names) (ps params) (bs bodies))
          (if (null? ns)
              (apply-env saved-env id)
              (if (equal? (car ns) id)
                  (cons (make-clausura (car ps) (car bs) env) #f)
                  (buscar (cdr ns) (cdr ps) (cdr bs)))))))))

(define extend-env
  (lambda (ids vals muts env)
    (ambiente-extendido ids vals muts env)))

(define ambiente-inicial
  (lambda () (ambiente-vacio)))

;; ------------------------------------------------------------
;; Clausuras
;; ------------------------------------------------------------

(define-datatype clausura clausura?
  (make-clausura
   (params    (list-of symbol?))
   (body      expresion?)
   (saved-env ambiente?)))

(define clausura-params
  (lambda (c)
    (cases clausura c (make-clausura (p b e) p))))

(define clausura-body
  (lambda (c)
    (cases clausura c (make-clausura (p b e) b))))

(define clausura-env
  (lambda (c)
    (cases clausura c (make-clausura (p b e) e))))


;; ============================================================
;;  SECCIÓN 3 — MAURICIO ALEJANDRO ROJAS
;;  Streams perezosos, tipos algebraicos y pattern matching
;; ============================================================

;; ------------------------------------------------------------
;; Streams
;; Un stream no vacío es un par (cabeza . thunk).
;; El thunk puede ser una clausura StreamLang o una lambda Scheme
;; (usada por map/filter/zip-with para eficiencia).
;; El stream vacío es el símbolo 'empty-stream.
;; ------------------------------------------------------------

(define stream-empty?
  (lambda (v) (eq? v 'empty-stream)))

(define stream-car
  (lambda (s) (car s)))

;; stream-thunk-apply : Thunk -> Stream
;; Fuerza un thunk que puede ser clausura StreamLang o lambda Scheme.
(define stream-thunk-apply
  (lambda (thunk)
    (cond
      [(clausura? thunk)  (apply-clausura thunk '())]
      [(procedure? thunk) (thunk)]
      [else (eopl:error 'stream-thunk-apply "Thunk inválido: ~s" thunk)])))

(define stream-cdr
  (lambda (s)
    (stream-thunk-apply (cdr s))))

;; apply-clausura : Clausura x (Listof Val) -> Val
;; Auxiliar central: extiende el ambiente de la clausura y evalúa su cuerpo.
(define apply-clausura
  (lambda (proc-val args)
    (let* ((params  (clausura-params proc-val))
           (body    (clausura-body   proc-val))
           (c-env   (clausura-env    proc-val))
           (muts    (make-list-of params #f))
           (new-env (extend-env params args muts c-env)))
      (value-of body new-env))))

;; ------------------------------------------------------------
;; Valores de tipos algebraicos
;; Un valor constructor es un par (nombre . lista-de-campos).
;; Esta representación es simple: fácil de inspeccionar en match.
;; ------------------------------------------------------------

;; make-variante : Symbol x (Listof Val) -> VarianteVal
(define make-variante
  (lambda (nombre campos) (cons nombre campos)))

;; variante-nombre : VarianteVal -> Symbol
(define variante-nombre (lambda (v) (car v)))

;; variante-campos : VarianteVal -> (Listof Val)
(define variante-campos (lambda (v) (cdr v)))

;; variante-val? : Any -> Boolean
(define variante-val?
  (lambda (v)
    (and (pair? v) (symbol? (car v)))))

;; ------------------------------------------------------------
;; Pattern matching
;; match-patron : Patron x Val x Ambiente -> Ambiente | #f
;; Retorna ambiente extendido con ligaduras del patrón, o #f si no coincide.
;; ------------------------------------------------------------

(define match-patron
  (lambda (pat val env)
    (cases patron pat

      ;; Literales numéricos
      (num-pat (n)
        (if (equal? val n) env #f))

      ;; Literales cadena (stripear comillas igual que str-exp)
      (str-pat (s)
        (let ((s-limpia (substring s 1 (- (string-length s) 1))))
          (if (equal? val s-limpia) env #f)))

      (true-pat ()
        (if (equal? val #t) env #f))

      (false-pat ()
        (if (equal? val #f) env #f))

      ;; Stream vacío
      (empty-stream-pat ()
        (if (stream-empty? val) env #f))

      ;; Wildcard: siempre coincide, no liga nada
      (wildcard-pat (w)
        env)

      ;; Variable: siempre coincide, liga id a val (inmutable)
      (var-pat (id)
        (extend-env (list id) (list val) (list #f) env))

      ;; Constructor: nombre debe coincidir, luego sub-patrones con campos
      (ctor-pat (nombre subpats)
        (if (and (variante-val? val)
                 (equal? (variante-nombre val) nombre)
                 (= (length subpats) (length (variante-campos val))))
            (let loop ((ps subpats)
                       (vs (variante-campos val))
                       (env-acc env))
              (if (null? ps)
                  env-acc
                  (let ((nuevo-env (match-patron (car ps) (car vs) env-acc)))
                    (if nuevo-env
                        (loop (cdr ps) (cdr vs) nuevo-env)
                        #f))))
            #f))

      ;; Stream no vacío: liga h-pat con la cabeza, t-pat con el resto forzado
      (stream-pat (h-pat t-pat)
        (if (stream-empty? val)
            #f
            (let ((env1 (match-patron h-pat (stream-car val) env)))
              (if env1
                  (match-patron t-pat (stream-cdr val) env1)
                  #f))))
      )))

;; ------------------------------------------------------------
;; Auxiliares para operaciones de stream
;; Operan sobre valores de stream directamente (sin AST).
;; ------------------------------------------------------------

;; value-of-map : Clausura x Stream -> Stream (perezoso)
(define value-of-map
  (lambda (f s)
    (if (stream-empty? s)
        'empty-stream
        (cons (apply-clausura f (list (stream-car s)))
              (lambda () (value-of-map f (stream-cdr s)))))))

;; value-of-filter : Clausura x Stream -> Stream (perezoso)
(define value-of-filter
  (lambda (pred s)
    (cond
      [(stream-empty? s) 'empty-stream]
      [(apply-clausura pred (list (stream-car s)))
       (cons (stream-car s)
             (lambda () (value-of-filter pred (stream-cdr s))))]
      [else
       (value-of-filter pred (stream-cdr s))])))

;; value-of-take : Int x Stream -> List (materializa)
(define value-of-take
  (lambda (n s)
    (cond
      [(= n 0)           '()]
      [(stream-empty? s) '()]
      [else
       (cons (stream-car s)
             (value-of-take (- n 1) (stream-cdr s)))])))

;; value-of-zip-with : Clausura x Stream x Stream -> Stream (perezoso)
(define value-of-zip-with
  (lambda (f s1 s2)
    (if (or (stream-empty? s1) (stream-empty? s2))
        'empty-stream
        (cons (apply-clausura f (list (stream-car s1) (stream-car s2)))
              (lambda ()
                (value-of-zip-with f (stream-cdr s1) (stream-cdr s2)))))))


;; ============================================================
;;  SECCIÓN 4 — JUAN DIEGO OSPINA
;;  Primitivas y value-of completo (Avances 1, 2 y 3)
;; ============================================================

;; apply-primitive : Primitiva x (Listof Val) -> Val
(define apply-primitive
  (lambda (prim args)
    (cases primitiva prim
      (sum-prim ()   (apply + args))
      (minus-prim ()
        (if (= (length args) 1) (- (car args)) (apply - args)))
      (mult-prim ()  (apply * args))
      (div-prim ()
        (if (= (cadr args) 0)
            (eopl:error 'div-prim "División por cero")
            (/ (car args) (cadr args))))
      (mod-prim ()
        (if (= (cadr args) 0)
            (eopl:error 'mod-prim "Módulo por cero")
            (modulo (car args) (cadr args))))
      (mayor-prim ()      (> (car args) (cadr args)))
      (menor-prim ()      (< (car args) (cadr args)))
      (mayorigual-prim () (>= (car args) (cadr args)))
      (menorigual-prim () (<= (car args) (cadr args)))
      (igual-prim ()      (equal? (car args) (cadr args)))
      (distinto-prim ()   (not (equal? (car args) (cadr args))))
      (and-prim () (if (car args) (cadr args) #f))
      (or-prim ()  (if (car args) #t (cadr args)))
      (not-prim () (not (car args)))
      (concat-prim () (string-append (car args) (cadr args)))
      (length-prim () (string-length (car args)))
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
        (let* ((s (car args)) (n (string->number s)))
          (if n n (eopl:error 'tonumber-prim
                              "No se puede convertir a número: ~s" s)))))))

;; ------------------------------------------------------------
;; value-of : Expresion x Ambiente -> Val
;; ------------------------------------------------------------

(define value-of
  (lambda (exp env)
    (cases expresion exp

      ;; ---- Literales ----
      (num-exp (n) n)
      (float-exp (f) f)
      ;; SLLGEN incluye las comillas en el string; se stripean.
      (str-exp (s)
        (substring s 1 (- (string-length s) 1)))
      (char-exp (c)
        (substring c 1 (- (string-length c) 1)))
      (true-exp ()  #t)
      (false-exp () #f)
      (void-exp () 'void)
      (empty-stream-exp () 'empty-stream)

      ;; ---- Variable ----
      ;; Var mutables guardan una Ref en el ambiente → desreferenciar.
      (var-exp (id)
        (let* ((r (apply-env env id))
               (val (car r)) (mut? (cdr r)))
          (if mut? (deref val) val)))

      ;; ---- Let (inmutable) ----
      (let-exp (id1 rand1 ids rands body)
        (let* ((todos-ids  (cons id1 ids))
               (todas-exps (cons rand1 rands))
               (vals    (map (lambda (e) (value-of e env)) todas-exps))
               (muts    (make-list-of todos-ids #f))
               (new-env (extend-env todos-ids vals muts env)))
          (value-of body new-env)))

      ;; ---- Var (mutable) ----
      (var-decl-exp (id1 rand1 ids rands body)
        (let* ((todos-ids  (cons id1 ids))
               (todas-exps (cons rand1 rands))
               (vals    (map (lambda (e) (value-of e env)) todas-exps))
               (refs    (map newref vals))
               (muts    (make-list-of todos-ids #t))
               (new-env (extend-env todos-ids refs muts env)))
          (value-of body new-env)))

      ;; ---- Set ----
      (set-exp (id val-exp)
        (let* ((r (apply-env env id))
               (ref (car r)) (mut? (cdr r)))
          (if (not mut?)
              (eopl:error 'set-exp
                          "No se puede asignar a variable inmutable: ~s" id)
              (begin (setref! ref (value-of val-exp env)) 'void))))

      ;; ---- Freeze ----
      (freeze-exp (id)
        (let* ((r (apply-env env id))
               (ref (car r)) (mut? (cdr r)))
          (if (not mut?)
              (eopl:error 'freeze-exp
                          "No se puede congelar una variable let: ~s" id)
              (begin (freezeref! ref) 'void))))

      ;; ---- Begin ----
      (begin-exp (e1 resto)
        (let recorrer ((val (value-of e1 env)) (exps resto))
          (if (null? exps)
              val
              (recorrer (value-of (car exps) env) (cdr exps)))))

      ;; ---- If / elif / else ----
      (if-exp (cond1 then1 elif-conds elif-thens else-e)
        (let ((v (value-of cond1 env)))
          (if (not (boolean? v))
              (eopl:error 'if-exp "La condición debe ser booleana: ~s" v)
              (if v
                  (value-of then1 env)
                  (let buscar-elif ((cs elif-conds) (ts elif-thens))
                    (if (null? cs)
                        (value-of else-e env)
                        (let ((cv (value-of (car cs) env)))
                          (if cv
                              (value-of (car ts) env)
                              (buscar-elif (cdr cs) (cdr ts))))))))))

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
              (eopl:error 'app-exp "Se esperaba una clausura: ~s" proc-val)
              (apply-clausura proc-val args))))

      ;; ---- Letrec ----
      (letrec-exp (name1 ids1 body1 names idss bodies cuerpo)
        (let* ((todos-names  (cons name1 names))
               (todos-params (cons ids1 idss))
               (todos-bodies (cons body1 bodies))
               (rec-env (ambiente-recursivo todos-names todos-params
                                            todos-bodies env)))
          (value-of cuerpo rec-env)))

      ;; ----------------------------------------------------------------
      ;; Avance #3: Streams
      ;; ----------------------------------------------------------------

      ;; stream(head, thunk): head se evalúa ya; thunk debe ser proc() ... end
      (stream-cons-exp (head-e tail-e)
        (let ((h (value-of head-e env))
              (t (value-of tail-e env)))
          (if (not (clausura? t))
              (eopl:error 'stream-cons-exp
                          "El segundo argumento debe ser proc(): ~s" t)
              (cons h t))))

      ;; head(s): primer elemento
      (head-exp (s-e)
        (let ((s (value-of s-e env)))
          (if (stream-empty? s)
              (eopl:error 'head-exp "head aplicado a empty-stream")
              (stream-car s))))

      ;; tail(s): fuerza el thunk y retorna el resto
      (tail-exp (s-e)
        (let ((s (value-of s-e env)))
          (if (stream-empty? s)
              (eopl:error 'tail-exp "tail aplicado a empty-stream")
              (stream-cdr s))))

      ;; stream-null?(s)
      (stream-null-exp (s-e)
        (stream-empty? (value-of s-e env)))

      ;; map(f, s) — perezoso
      (map-exp (f-e s-e)
        (value-of-map (value-of f-e env) (value-of s-e env)))

      ;; filter(pred, s) — perezoso
      (filter-exp (pred-e s-e)
        (value-of-filter (value-of pred-e env) (value-of s-e env)))

      ;; take(n, s) — materializa en lista
      (take-exp (n-e s-e)
        (value-of-take (value-of n-e env) (value-of s-e env)))

      ;; zip-with(f, s1, s2) — perezoso
      (zip-with-exp (f-e s1-e s2-e)
        (value-of-zip-with (value-of f-e env)
                           (value-of s1-e env)
                           (value-of s2-e env)))

      ;; ----------------------------------------------------------------
      ;; Avance #3: Constructor de tipo algebraico
      ;; ----------------------------------------------------------------
      (ctor-exp (nombre arg-exps)
        (make-variante nombre (map (lambda (e) (value-of e env)) arg-exps)))

      ;; ----------------------------------------------------------------
      ;; Avance #3: Match
      ;; ----------------------------------------------------------------
      (match-exp (val-e pat1 exp1 pats exps)
        (let ((val (value-of val-e env)))
          (let buscar ((ps (cons pat1 pats))
                       (es (cons exp1 exps)))
            (if (null? ps)
                (eopl:error 'match-exp
                            "No hubo coincidencia en match para: ~s" val)
                (let ((nuevo-env (match-patron (car ps) val env)))
                  (if nuevo-env
                      (value-of (car es) nuevo-env)
                      (buscar (cdr ps) (cdr es))))))))
      )))


;; ============================================================
;; value-of-program y run
;; ============================================================

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases programa pgm
      (a-program (decls exp)
        (value-of exp (ambiente-inicial))))))

(define run
  (lambda (src)
    (value-of-program (scan&parse src))))

(provide (all-defined-out))
