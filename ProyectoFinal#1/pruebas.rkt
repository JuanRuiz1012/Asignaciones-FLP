#lang racket

;; ============================================================
;; Proyecto final — Pruebas del interpretador StreamLang-UV
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710,
;;          JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701,
;;          JUAN FELIPE RUIZ 2359397
;;
;; Organización:
;;   raco test -s entrega-1 pruebas.rkt
;;   raco test -s entrega-2 pruebas.rkt
;;   raco test -s entrega-3 pruebas.rkt
;; ============================================================

(require rackunit)
(require rackunit/text-ui)
(require "interpretador.rkt")

;; ============================================================
;; Entrega 1 — Parser + unparse   (Mauricio Alejandro Rojas)
;; ============================================================

(define entrega-1
  (test-suite
   "Entrega 1 — Parser + unparse"

   ;; Literales
   (test-case "parser: entero positivo"
     (check-not-exn (lambda () (scan&parse "42"))))

   (test-case "parser: entero negativo"
     (check-not-exn (lambda () (scan&parse "-7"))))

   (test-case "parser: flotante positivo"
     (check-not-exn (lambda () (scan&parse "3.14"))))

   (test-case "parser: flotante negativo"
     (check-not-exn (lambda () (scan&parse "-0.5"))))

   (test-case "parser: true"
     (check-not-exn (lambda () (scan&parse "true"))))

   (test-case "parser: false"
     (check-not-exn (lambda () (scan&parse "false"))))

   (test-case "parser: void"
     (check-not-exn (lambda () (scan&parse "void"))))

   (test-case "parser: empty-stream"
     (check-not-exn (lambda () (scan&parse "empty-stream"))))

   ;; Primitivas
   (test-case "parser: suma"
     (check-not-exn (lambda () (scan&parse "+(1, 2, 3)"))))

   (test-case "parser: resta"
     (check-not-exn (lambda () (scan&parse "-(10, 3)"))))

   (test-case "parser: multiplicación"
     (check-not-exn (lambda () (scan&parse "*(2, 3)"))))

   (test-case "parser: división"
     (check-not-exn (lambda () (scan&parse "/(10, 2)"))))

   (test-case "parser: módulo"
     (check-not-exn (lambda () (scan&parse "%(7, 3)"))))

   (test-case "parser: mayor"
     (check-not-exn (lambda () (scan&parse ">(5, 3)"))))

   (test-case "parser: menor"
     (check-not-exn (lambda () (scan&parse "<(3, 5)"))))

   (test-case "parser: igual"
     (check-not-exn (lambda () (scan&parse "==(3, 3)"))))

   (test-case "parser: distinto"
     (check-not-exn (lambda () (scan&parse "!=(3, 4)"))))

   (test-case "parser: and"
     (check-not-exn (lambda () (scan&parse "and(true, false)"))))

   (test-case "parser: or"
     (check-not-exn (lambda () (scan&parse "or(false, true)"))))

   (test-case "parser: not"
     (check-not-exn (lambda () (scan&parse "not(true)"))))

   (test-case "parser: concat"
     (check-not-exn (lambda () (scan&parse "concat(\"hola\", \" mundo\")"))))

   (test-case "parser: length"
     (check-not-exn (lambda () (scan&parse "length(\"abc\")"))))

   ;; Let
   (test-case "parser: let una ligadura"
     (check-not-exn
      (lambda () (scan&parse "let x = 10 in x end"))))

   (test-case "parser: let múltiples ligaduras"
     (check-not-exn
      (lambda ()
        (scan&parse "let x = 1, y = 2 in +(x, y) end"))))

   (test-case "parser: let anidado"
     (check-not-exn
      (lambda ()
        (scan&parse "let x = 10 in let y = +(x, 1) in y end end"))))

   ;; Var, Set, Freeze
   (test-case "parser: var + set"
     (check-not-exn
      (lambda ()
        (scan&parse "var x = 0 in begin set x := +(x, 1); x end end"))))

   (test-case "parser: freeze"
     (check-not-exn
      (lambda ()
        (scan&parse "var x = 10 in begin freeze x; x end end"))))

   (test-case "parser: var múltiples ligaduras"
     (check-not-exn
      (lambda ()
        (scan&parse "var x = 10, y = 20 in +(x, y) end"))))

   ;; Begin
   (test-case "parser: begin con tres expresiones"
     (check-not-exn
      (lambda ()
        (scan&parse "begin +(1, 2); *(3, 4); 99 end"))))

   ;; If/elif/else
   (test-case "parser: if simple"
     (check-not-exn
      (lambda ()
        (scan&parse "if true then 1 else 2 end"))))

   (test-case "parser: if con elif"
     (check-not-exn
      (lambda ()
        (scan&parse "if false then 1 elif true then 2 else 3 end"))))

   (test-case "parser: if con múltiples elif"
     (check-not-exn
      (lambda ()
        (scan&parse
         "if false then 1 elif false then 2 elif true then 3 else 4 end"))))

   ;; Proc & Apply
   (test-case "parser: proc sin parámetros"
     (check-not-exn
      (lambda ()
        (scan&parse "proc() 42 end"))))

   (test-case "parser: proc con un parámetro"
     (check-not-exn
      (lambda ()
        (scan&parse "proc(x) +(x, 1) end"))))

   (test-case "parser: proc con dos parámetros"
     (check-not-exn
      (lambda ()
        (scan&parse "proc(x, y) +(x, y) end"))))

   (test-case "parser: apply a proc inline"
     (check-not-exn
      (lambda ()
        (scan&parse "apply proc(x) +(x, 1) end(5)"))))

   (test-case "parser: apply a variable"
     (check-not-exn
      (lambda ()
        (scan&parse "let f = proc(x) *(x, 2) end in apply f(3) end"))))

   ;; Letrec
   (test-case "parser: letrec factorial"
     (check-not-exn
      (lambda ()
        (scan&parse
         "letrec fact(n) = if ==(n, 0) then 1 else *(n, apply fact(-(n, 1))) end
          in apply fact(5) end"))))

   (test-case "parser: letrec recursión mutua"
     (check-not-exn
      (lambda ()
        (scan&parse
         "letrec is-even(n) = if ==(n, 0) then true else apply is-odd(-(n, 1)) end
                 is-odd(n) = if ==(n, 0) then false else apply is-even(-(n, 1)) end
          in apply is-even(10) end"))))

   ;; Streams
   (test-case "parser: stream construcción mínima"
     (check-not-exn
      (lambda ()
        (scan&parse "stream(1, proc() empty-stream end)"))))

   (test-case "parser: stream anidado"
     (check-not-exn
      (lambda ()
        (scan&parse
         "stream(1, proc() stream(2, proc() empty-stream end) end)"))))

   (test-case "parser: head"
     (check-not-exn
      (lambda ()
        (scan&parse "head(stream(42, proc() empty-stream end))"))))

   (test-case "parser: tail"
     (check-not-exn
      (lambda ()
        (scan&parse "tail(stream(1, proc() empty-stream end))"))))

   (test-case "parser: stream-null? vacío"
     (check-not-exn
      (lambda ()
        (scan&parse "stream-null?(empty-stream)"))))

   (test-case "parser: stream-null? no vacío"
     (check-not-exn
      (lambda ()
        (scan&parse "stream-null?(stream(1, proc() empty-stream end))"))))

   ;; Operaciones sobre streams
   (test-case "parser: map"
     (check-not-exn
      (lambda ()
        (scan&parse "map(proc(x) *(x, 2) end, empty-stream)"))))

   (test-case "parser: filter"
     (check-not-exn
      (lambda ()
        (scan&parse "filter(proc(x) >(x, 5) end, empty-stream)"))))

   (test-case "parser: take"
     (check-not-exn
      (lambda ()
        (scan&parse "take(5, empty-stream)"))))

   (test-case "parser: zip-with"
     (check-not-exn
      (lambda ()
        (scan&parse
         "zip-with(proc(a, b) +(a, b) end, empty-stream, empty-stream)"))))

   ;; Datatypes
   (test-case "parser: datatype sin campos"
     (check-not-exn
      (lambda ()
        (scan&parse
         "datatype Color = Red() | Green() | Blue()
          Red()"))))

   (test-case "parser: datatype con campos"
     (check-not-exn
      (lambda ()
        (scan&parse
         "datatype Shape = Circle(radius) | Rect(width, height) | Point()
          Circle(5)"))))

   (test-case "parser: datatype árbol binario"
     (check-not-exn
      (lambda ()
        (scan&parse
         "datatype Tree = Leaf(value) | Node(left, right)
          Node(Leaf(1), Leaf(2))"))))

   ;; Match
   (test-case "parser: match con wildcard"
     (check-not-exn
      (lambda ()
        (scan&parse
         "match 42 with
          | 42 => true
          | _ => false
          end"))))

   (test-case "parser: match con constructores"
     (check-not-exn
      (lambda ()
        (scan&parse
         "datatype Tree = Leaf(v) | Node(l, r)
          match Leaf(5) with
          | Leaf(x) => x
          | Node(l, r) => 0
          end"))))

   (test-case "parser: match con stream"
     (check-not-exn
      (lambda ()
        (scan&parse
         "match empty-stream with
          | empty-stream => 0
          | stream(h, t) => h
          end"))))

   (test-case "parser: match con variable y wildcard"
     (check-not-exn
      (lambda ()
        (scan&parse
         "match 10 with
          | 0 => false
          | n => true
          end"))))

   ;; Ejemplo #3
   (test-case "parser: árbol binario con datatype"
     (check-not-exn
      (lambda ()
        (scan&parse
         "datatype Tree = Leaf(value) | Node(left, right)
          letrec sum-tree(t) =
            match t with
            | Leaf(v) => v
            | Node(l, r) => +(apply sum-tree(l), apply sum-tree(r))
            end
          in apply sum-tree(Node(Node(Leaf(1), Leaf(2)), Leaf(3))) end"))))

   ;; Unparse
   (test-case "unparse: entero"
     (check-not-exn
      (lambda () (unparse (scan&parse "42")))))

   (test-case "unparse: let"
     (check-not-exn
      (lambda () (unparse (scan&parse "let x = 10 in x end")))))

   (test-case "unparse: proc"
     (check-not-exn
      (lambda () (unparse (scan&parse "proc(x) +(x, 1) end")))))

   (test-case "unparse: if"
     (check-not-exn
      (lambda () (unparse (scan&parse "if true then 1 else 2 end")))))

   (test-case "unparse: stream"
     (check-not-exn
      (lambda ()
        (unparse (scan&parse "stream(1, proc() empty-stream end)")))))

   (test-case "unparse: match"
     (check-not-exn
      (lambda ()
        (unparse (scan&parse
                  "match 1 with
                   | 1 => true
                   | _ => false
                   end")))))

   (test-case "unparse: letrec"
     (check-not-exn
      (lambda ()
        (unparse (scan&parse
                  "letrec fact(n) = if ==(n, 0) then 1 else *(n, apply fact(-(n, 1))) end
                   in apply fact(5) end")))))
   ))


;; ============================================================
;; Entrega 2 — value-of + ambiente + store  (Juan Diego Ospina)
;; ============================================================

(define entrega-2
  (test-suite
   "Entrega 2 — value-of + ambiente + store"

   ;; Literales
   (test-case "run: entero"
     (check-equal? (run "42") 42))

   (test-case "run: entero negativo"
     (check-equal? (run "-7") -7))

   (test-case "run: flotante"
     (check-equal? (run "3.14") 3.14))

   (test-case "run: flotante negativo"
     (check-equal? (run "-0.5") -0.5))

   (test-case "run: true"
     (check-equal? (run "true") #t))

   (test-case "run: false"
     (check-equal? (run "false") #f))

   (test-case "run: void"
     (check-equal? (run "void") 'void))

   ;; Primitivas aritméticas
   (test-case "run: suma dos números"
     (check-equal? (run "+(3, 4)") 7))

   (test-case "run: suma tres números"
     (check-equal? (run "+(1, 2, 3)") 6))

   (test-case "run: resta"
     (check-equal? (run "-(10, 3)") 7))

   (test-case "run: multiplicación"
     (check-equal? (run "*(4, 5)") 20))

   (test-case "run: división"
     (check-equal? (run "/(10, 2)") 5))

   (test-case "run: módulo"
     (check-equal? (run "%(7, 3)") 1))

   (test-case "run: operación con flotantes"
     (check-equal? (run "+(1.5, 2.5)") 4.0))

   ;; Relacionales
   (test-case "run: mayor verdadero"
     (check-equal? (run ">(5, 3)") #t))

   (test-case "run: mayor falso"
     (check-equal? (run ">(2, 3)") #f))

   (test-case "run: menor"
     (check-equal? (run "<(3, 5)") #t))

   (test-case "run: mayor-igual"
     (check-equal? (run ">=(5, 5)") #t))

   (test-case "run: menor-igual"
     (check-equal? (run "<=(4, 5)") #t))

   (test-case "run: igual números"
     (check-equal? (run "==(3, 3)") #t))

   (test-case "run: igual falso"
     (check-equal? (run "==(3, 4)") #f))

   (test-case "run: distinto"
     (check-equal? (run "!=(3, 4)") #t))

   ;; Booleanas
   (test-case "run: and true true"
     (check-equal? (run "and(true, true)") #t))

   (test-case "run: and true false"
     (check-equal? (run "and(true, false)") #f))

   (test-case "run: or false true"
     (check-equal? (run "or(false, true)") #t))

   (test-case "run: or false false"
     (check-equal? (run "or(false, false)") #f))

   (test-case "run: not true"
     (check-equal? (run "not(true)") #f))

   (test-case "run: not false"
     (check-equal? (run "not(false)") #t))

   ;; Cadenas
   (test-case "run: concat"
     (check-equal? (run "concat(\"hola\", \" mundo\")") "hola mundo"))

   (test-case "run: length"
     (check-equal? (run "length(\"abc\")") 3))

   (test-case "run: length cadena vacía"
     (check-equal? (run "length(\"\")") 0))

   ;; Conversión
   (test-case "run: to-string de número"
     (check-equal? (run "to-string(42)") "42"))

   (test-case "run: to-string de booleano"
     (check-equal? (run "to-string(true)") "true"))

   (test-case "run: to-number"
     (check-equal? (run "to-number(\"42\")") 42))

   ;; If/elif/else
   (test-case "run: if rama verdadera"
     (check-equal? (run "if true then 1 else 2 end") 1))

   (test-case "run: if rama falsa"
     (check-equal? (run "if false then 1 else 2 end") 2))

   (test-case "run: if con elif — primer elif"
     (check-equal?
      (run "if false then 1 elif true then 2 else 3 end") 2))

   (test-case "run: if con elif — else"
     (check-equal?
      (run "if false then 1 elif false then 2 else 3 end") 3))

   (test-case "run: if con múltiples elif"
     (check-equal?
      (run "if false then 1 elif false then 2 elif true then 3 else 4 end") 3))

   ;; Let
   (test-case "run: let una ligadura"
     (check-equal? (run "let x = 10 in x end") 10))

   (test-case "run: let con operación"
     (check-equal? (run "let x = 5 in *(x, 2) end") 10))

   (test-case "run: let múltiples ligaduras"
     (check-equal? (run "let x = 3, y = 4 in +(x, y) end") 7))

   (test-case "run: let anidado"
     (check-equal?
      (run "let x = 10 in let y = +(x, 1) in y end end") 11))

   (test-case "run: let con shadowing"
     (check-equal?
      (run "let x = 1 in let x = 2 in x end end") 2))

   ;; Var / Set / Freeze
   (test-case "run: var simple"
     (check-equal? (run "var x = 5 in x end") 5))

   (test-case "run: var + set"
     (check-equal?
      (run "var x = 0 in begin set x := 10; x end end") 10))

   (test-case "run: var + set acumulativo"
     (check-equal?
      (run "var x = 1 in begin set x := +(x, 1); set x := +(x, 1); x end end") 3))

   (test-case "run: set retorna void"
     (check-equal?
      (run "var x = 0 in set x := 5 end") 'void))

   (test-case "run: var múltiples + set"
     (check-equal?
      (run "var x = 10, y = 20 in begin set x := +(x, 1); +(x, y) end end") 31))

   (test-case "run: freeze + lectura"
     (check-equal?
      (run "var x = 10 in begin freeze x; x end end") 10))

   (test-case "run: freeze impide set"
     (check-exn exn:fail?
       (lambda ()
         (run "var x = 10 in begin freeze x; set x := 20 end end"))))

   (test-case "run: set sobre let lanza error"
     (check-exn exn:fail?
       (lambda ()
         (run "let x = 5 in set x := 10 end"))))

   ;; Begin
   (test-case "run: begin retorna último valor"
     (check-equal? (run "begin 1; 2; 3 end") 3))

   (test-case "run: begin con efectos"
     (check-equal?
      (run "var x = 0 in begin set x := 1; set x := 2; x end end") 2))

   ;; Proc y Apply
   (test-case "run: proc y apply básico"
     (check-equal?
      (run "apply proc(x) +(x, 1) end(5)") 6))

   (test-case "run: proc con dos parámetros"
     (check-equal?
      (run "apply proc(x, y) +(x, y) end(3, 4)") 7))

   (test-case "run: clausura captura ambiente"
     (check-equal?
      (run "let n = 10 in apply proc(x) +(x, n) end(5) end") 15))

   (test-case "run: proc asignado a variable"
     (check-equal?
      (run "let f = proc(x) *(x, 2) end in apply f(6) end") 12))

   (test-case "run: proc de orden superior"
     (check-equal?
      (run "let apply2 = proc(f, x) apply f(x) end
            in apply apply2(proc(x) +(x, 100) end, 1) end") 101))

   ;; Letrec
   (test-case "run: factorial con letrec"
     (check-equal?
      (run "letrec fact(n) =
              if ==(n, 0) then 1 else *(n, apply fact(-(n, 1))) end
            in apply fact(5) end") 120))

   (test-case "run: letrec factorial de 0"
     (check-equal?
      (run "letrec fact(n) =
              if ==(n, 0) then 1 else *(n, apply fact(-(n, 1))) end
            in apply fact(0) end") 1))

   (test-case "run: letrec suma recursiva"
     (check-equal?
      (run "letrec suma(n) =
              if ==(n, 0) then 0 else +(n, apply suma(-(n, 1))) end
            in apply suma(10) end") 55))

   (test-case "run: letrec recursión mutua (is-even / is-odd)"
     (check-equal?
      (run "letrec
              is-even(n) = if ==(n, 0) then true  else apply is-odd(-(n, 1)) end
              is-odd(n)  = if ==(n, 0) then false else apply is-even(-(n, 1)) end
            in apply is-even(10) end") #t))

   (test-case "run: letrec recursión mutua is-odd"
     (check-equal?
      (run "letrec
              is-even(n) = if ==(n, 0) then true  else apply is-odd(-(n, 1)) end
              is-odd(n)  = if ==(n, 0) then false else apply is-even(-(n, 1)) end
            in apply is-odd(7) end") #t))

   ;; Ejemplo 4 (freeze)
   (test-case "run: Ejemplo 4 — freeze de variables"
     (check-equal?
      (run "var x = 10, y = 20
            in
            begin
              set x := +(x, 1);
              freeze x;
              set y := +(y, x);
              +(x, y)
            end
            end") 42))

   ;; Ejemplo 10 (recursión mutua)
   (test-case "run: Ejemplo 10 — recursión mutua con and"
     (check-equal?
      (run "letrec
              is-even(n) = if ==(n, 0) then true  else apply is-odd(-(n, 1)) end
              is-odd(n)  = if ==(n, 0) then false else apply is-even(-(n, 1)) end
            in
              let r1 = apply is-even(10),
                  r2 = apply is-odd(7)
              in
                and(r1, r2)
              end
            end") #t))
   ))


;; ============================================================
;; Entrega 3 — Streams, Datatypes y Match   (Juan Felipe Ruiz)
;; ============================================================

(define entrega-3
  (test-suite
   "Entrega 3 — Streams, Datatypes y Match"

   ;; ---- head / tail / stream-null? ----

   (test-case "run: head de stream con un elemento"
     (check-equal?
      (run "head(stream(42, proc() empty-stream end))")
      42))

   (test-case "run: head de stream con varios elementos"
     (check-equal?
      (run "head(stream(1, proc() stream(2, proc() empty-stream end) end))")
      1))

   (test-case "run: tail fuerza el thunk"
     (check-equal?
      (run "head(tail(stream(1, proc() stream(2, proc() empty-stream end) end)))")
      2))

   (test-case "run: stream-null? en empty-stream"
     (check-equal?
      (run "stream-null?(empty-stream)")
      #t))

   (test-case "run: stream-null? en stream no vacío"
     (check-equal?
      (run "stream-null?(stream(1, proc() empty-stream end))")
      #f))

   (test-case "run: head en empty-stream lanza error"
     (check-exn exn:fail?
       (lambda () (run "head(empty-stream)"))))

   (test-case "run: tail en empty-stream lanza error"
     (check-exn exn:fail?
       (lambda () (run "tail(empty-stream)"))))

   ;; ---- take ----

   (test-case "run: take 0 elementos"
     (check-equal?
      (run "take(0, stream(1, proc() empty-stream end))")
      '()))

   (test-case "run: take de stream finito"
     (check-equal?
      (run "take(3, stream(1, proc() stream(2, proc() stream(3, proc() empty-stream end) end) end))")
      '(1 2 3)))

   (test-case "run: take más que el tamaño del stream"
     (check-equal?
      (run "take(10, stream(1, proc() empty-stream end))")
      '(1)))

   (test-case "run: take de empty-stream"
     (check-equal?
      (run "take(5, empty-stream)")
      '()))

   ;; ---- Stream infinito con letrec ----

   (test-case "run: Ejemplo 2 — Fibonacci (primeros 10)"
     (check-equal?
      (run "letrec fibs-from(a, b) =
              stream(a, proc() apply fibs-from(+(a, b), a) end)
            in
              take(10, apply fibs-from(0, 1))
            end")
      '(0 1 1 2 3 5 8 13 21 34)))

   (test-case "run: naturales desde n — take 5"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in take(5, apply nats-from(1)) end")
      '(1 2 3 4 5)))

   (test-case "run: Ejemplo 9 — repeat double desde 1 (take 6)"
     (check-equal?
      (run "let double = proc(x) *(x, 2) end
            in
              letrec repeat(f, n) =
                stream(n, proc() apply repeat(f, apply f(n)) end)
              in
                take(6, apply repeat(double, 1))
              end
            end")
      '(1 2 4 8 16 32)))

   ;; ---- map ----

   (test-case "run: map sobre empty-stream"
     (check-equal?
      (run "take(3, map(proc(x) *(x, 2) end, empty-stream))")
      '()))

   (test-case "run: map duplica elementos"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              take(5, map(proc(x) *(x, 2) end, apply nats-from(1)))
            end")
      '(2 4 6 8 10)))

   (test-case "run: map suma constante"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              take(4, map(proc(x) +(x, 10) end, apply nats-from(0)))
            end")
      '(10 11 12 13)))

   ;; ---- filter ----

   (test-case "run: filter sobre empty-stream"
     (check-equal?
      (run "take(5, filter(proc(x) >(x, 0) end, empty-stream))")
      '()))

   (test-case "run: filter pares — take 5"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              take(5, filter(proc(x) ==(%(x, 2), 0) end, apply nats-from(1)))
            end")
      '(2 4 6 8 10)))

   (test-case "run: filter impares — take 4"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              take(4, filter(proc(x) !=(%(x, 2), 0) end, apply nats-from(1)))
            end")
      '(1 3 5 7)))

   ;; ---- Ejemplo 5 — map + filter combinados ----

   (test-case "run: Ejemplo 5 — cuadrados de los primeros 5 pares"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              let nats = apply nats-from(1)
              in
                let evens = filter(proc(x) ==(%(x, 2), 0) end, nats)
                in
                  take(5, map(proc(x) *(x, x) end, evens))
                end
              end
            end")
      '(4 16 36 64 100)))

   ;; ---- zip-with ----

   (test-case "run: zip-with sobre streams vacíos"
     (check-equal?
      (run "take(3, zip-with(proc(a, b) +(a, b) end, empty-stream, empty-stream))")
      '()))

   (test-case "run: Ejemplo 7 — zip-with suma odds y evens (take 5)"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              let odds = filter(proc(x) !=(%(x, 2), 0) end, apply nats-from(1)),
                  evens = filter(proc(x) ==(%(x, 2), 0) end, apply nats-from(1))
              in
                take(5, zip-with(proc(a, b) +(a, b) end, odds, evens))
              end
            end")
      '(3 7 11 15 19)))

   (test-case "run: zip-with multiplica dos streams"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              take(4, zip-with(proc(a, b) *(a, b) end,
                               apply nats-from(1),
                               apply nats-from(1)))
            end")
      '(1 4 9 16)))

   ;; ---- Datatypes: constructores ----

   (test-case "run: constructor sin campos — variante-val?"
     (check-true
      (variante-val? (run "datatype Color = Red() | Green() | Blue() Red()"))))

   (test-case "run: constructor nombre correcto"
     (check-equal?
      (variante-nombre (run "datatype Color = Red() | Green() | Blue() Red()"))
      'Red))

   (test-case "run: constructor con un campo"
     (let ((v (run "datatype Shape = Circle(radius) Circle(5)")))
       (check-equal? (variante-nombre v) 'Circle)
       (check-equal? (variante-campos v) '(5))))

   (test-case "run: constructor con dos campos"
     (let ((v (run "datatype Shape = Circle(r) | Rect(w, h) Rect(3, 4)")))
       (check-equal? (variante-nombre v) 'Rect)
       (check-equal? (variante-campos v) '(3 4))))

   (test-case "run: constructor anidado"
     (let ((v (run "datatype Tree = Leaf(v) | Node(l, r)
                    Node(Leaf(1), Leaf(2))")))
       (check-equal? (variante-nombre v) 'Node)))

   ;; ---- Match: literales ----

   (test-case "run: match entero coincide"
     (check-equal?
      (run "match 42 with | 42 => true | _ => false end")
      #t))

   (test-case "run: match entero no coincide, usa wildcard"
     (check-equal?
      (run "match 99 with | 42 => true | _ => false end")
      #f))

   (test-case "run: match booleano true"
     (check-equal?
      (run "match true with | true => 1 | false => 2 end")
      1))

   (test-case "run: match booleano false"
     (check-equal?
      (run "match false with | true => 1 | false => 2 end")
      2))

   ;; ---- Match: variables y wildcard ----

   (test-case "run: match variable liga valor"
     (check-equal?
      (run "match 7 with | 0 => 0 | n => *(n, 2) end")
      14))

   (test-case "run: match wildcard descarta valor"
     (check-equal?
      (run "match 42 with | 0 => false | _ => true end")
      #t))

   ;; ---- Match: constructores ----

   (test-case "run: Ejemplo 3 — suma árbol binario"
     (check-equal?
      (run "datatype Tree = Leaf(value) | Node(left, right)
            letrec sum-tree(t) =
              match t with
              | Leaf(v)    => v
              | Node(l, r) => +(apply sum-tree(l), apply sum-tree(r))
              end
            in apply sum-tree(Node(Node(Leaf(1), Leaf(2)), Leaf(3))) end")
      6))

   (test-case "run: Ejemplo 8 — evaluador de expresiones"
     (check-equal?
      (run "datatype Expr = Num(n) | Add(left, right) | Mul(left, right)
            letrec eval-expr(e) =
              match e with
              | Num(n)    => n
              | Add(l, r) => +(apply eval-expr(l), apply eval-expr(r))
              | Mul(l, r) => *(apply eval-expr(l), apply eval-expr(r))
              end
            in
              apply eval-expr(Mul(Add(Num(2), Num(3)), Add(Num(4), Num(1))))
            end")
      25))

   (test-case "run: match shape Circle"
     (check-equal?
      (run "datatype Shape = Circle(r) | Rect(w, h) | Point()
            match Circle(5) with
            | Circle(r)  => *(r, r)
            | Rect(w, h) => *(w, h)
            | Point()    => 0
            end")
      25))

   (test-case "run: match shape Rect"
     (check-equal?
      (run "datatype Shape = Circle(r) | Rect(w, h) | Point()
            match Rect(3, 4) with
            | Circle(r)  => *(r, r)
            | Rect(w, h) => *(w, h)
            | Point()    => 0
            end")
      12))

   (test-case "run: match shape Point (sin campos)"
     (check-equal?
      (run "datatype Shape = Circle(r) | Rect(w, h) | Point()
            match Point() with
            | Circle(r)  => 1
            | Rect(w, h) => 2
            | Point()    => 0
            end")
      0))

   (test-case "run: match anidado con constructores"
     (check-equal?
      (run "datatype Tree = Leaf(v) | Node(l, r)
            match Node(Leaf(10), Leaf(20)) with
            | Leaf(v)    => v
            | Node(l, r) =>
                match l with
                | Leaf(v) => v
                | Node(a, b) => 0
                end
            end")
      10))

   (test-case "run: match sin coincidencia lanza error"
     (check-exn exn:fail?
       (lambda ()
         (run "datatype Color = Red() | Blue()
               match Red() with
               | Blue() => 1
               end"))))

   ;; ---- Match: streams ----

   (test-case "run: Ejemplo 6 — match sobre stream (h + h2)"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              let s = apply nats-from(1)
              in
                match s with
                | empty-stream => 0
                | stream(h, t)  =>
                    match t with
                    | stream(h2, _) => +(h, h2)
                    | _ => h
                    end
                end
              end
            end")
      3))

   (test-case "run: match stream empty-stream"
     (check-equal?
      (run "match empty-stream with
            | empty-stream => 99
            | stream(h, t) => h
            end")
      99))

   (test-case "run: match stream extrae cabeza"
     (check-equal?
      (run "match stream(42, proc() empty-stream end) with
            | empty-stream => 0
            | stream(h, t) => h
            end")
      42))

   (test-case "run: match stream encadena dos niveles"
     (check-equal?
      (run "match stream(1, proc() stream(2, proc() empty-stream end) end) with
            | empty-stream => 0
            | stream(h, t) =>
                match t with
                | empty-stream => h
                | stream(h2, _) => +(h, h2)
                end
            end")
      3))

   ;; ---- Casos límite / integración ----

   (test-case "run: stream + map + take con letrec"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              take(3, map(proc(x) *(x, x) end, apply nats-from(1)))
            end")
      '(1 4 9)))

   (test-case "run: stream infinito de constantes — take 4"
     (check-equal?
      (run "letrec ones() =
              stream(1, proc() apply ones() end)
            in
              take(4, apply ones())
            end")
      '(1 1 1 1)))

   (test-case "run: zip-with suma stream consigo mismo (cuadrados?)"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              take(4, zip-with(proc(a, b) +(a, b) end,
                               apply nats-from(1),
                               apply nats-from(1)))
            end")
      '(2 4 6 8)))

   (test-case "run: datatype + letrec + match — profundidad árbol"
     (check-equal?
      (run "datatype Tree = Leaf(v) | Node(l, r)
            letrec depth(t) =
              match t with
              | Leaf(v)    => 0
              | Node(l, r) =>
                  let dl = apply depth(l),
                      dr = apply depth(r)
                  in
                    +(1, if >(dl, dr) then dl else dr end)
                  end
              end
            in apply depth(Node(Node(Leaf(1), Leaf(2)), Leaf(3))) end")
      2))

   (test-case "run: filter + map encadenados"
     (check-equal?
      (run "letrec nats-from(n) =
              stream(n, proc() apply nats-from(+(n, 1)) end)
            in
              take(3,
                map(proc(x) +(x, 1) end,
                  filter(proc(x) ==(%(x, 2), 0) end,
                         apply nats-from(1))))
            end")
      '(3 5 7)))
   ))


;; ============================================================
;; Ejecución
;; ============================================================

(module+ test
  (run-tests entrega-1)
  (run-tests entrega-2)
  (run-tests entrega-3))
