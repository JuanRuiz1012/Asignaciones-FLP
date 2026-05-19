#lang racket

;; ============================================================
;; Proyecto final — Pruebas del interpretador StreamLang-UV
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710,
;;          JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701,
;;          JUAN FELIPE RUIZ 2359397
;; ============================================================

(require rackunit)
(require rackunit/text-ui)
(require "interpretador.rkt")

;; ============================================================
;; ENTREGA 1 — Parser + Unparse — Mauricio Alejandro Rojas
;; ============================================================

(define entrega-1
  (test-suite
   "Entrega 1 — Parser + unparse"

   ;; ---- Literales ----
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

   ;; ---- Primitivas ----
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

   ;; ---- Let ----
   (test-case "parser: let una ligadura"
     (check-not-exn (lambda () (scan&parse "let x = 10 in x end"))))

   (test-case "parser: let múltiples ligaduras"
     (check-not-exn
      (lambda () (scan&parse "let x = 1, y = 2 in +(x, y) end"))))

   (test-case "parser: let anidado"
     (check-not-exn
      (lambda ()
        (scan&parse "let x = 10 in let y = +(x, 1) in y end end"))))

   ;; ---- Var / Set / Freeze ----
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

   ;; ---- Begin ----
   (test-case "parser: begin con tres expresiones"
     (check-not-exn
      (lambda ()
        (scan&parse "begin +(1, 2); *(3, 4); 99 end"))))

   ;; ---- If / elif / else ----
   (test-case "parser: if simple"
     (check-not-exn
      (lambda () (scan&parse "if true then 1 else 2 end"))))

   (test-case "parser: if con elif"
     (check-not-exn
      (lambda ()
        (scan&parse "if false then 1 elif true then 2 else 3 end"))))

   (test-case "parser: if con múltiples elif"
     (check-not-exn
      (lambda ()
        (scan&parse
         "if false then 1 elif false then 2 elif true then 3 else 4 end"))))

   ;; ---- Proc / Apply ----
   (test-case "parser: proc sin parámetros"
     (check-not-exn (lambda () (scan&parse "proc() 42 end"))))

   (test-case "parser: proc con un parámetro"
     (check-not-exn (lambda () (scan&parse "proc(x) +(x, 1) end"))))

   (test-case "parser: proc con dos parámetros"
     (check-not-exn (lambda () (scan&parse "proc(x, y) +(x, y) end"))))

   (test-case "parser: apply a proc inline"
     (check-not-exn
      (lambda ()
        (scan&parse "apply proc(x) +(x, 1) end(5)"))))

   (test-case "parser: apply a variable"
     (check-not-exn
      (lambda ()
        (scan&parse "let f = proc(x) *(x, 2) end in apply f(3) end"))))

   ;; ---- Letrec ----
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
                 is-odd(n)  = if ==(n, 0) then false else apply is-even(-(n, 1)) end
          in apply is-even(10) end"))))

   ;; ---- Streams ----
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
      (lambda () (scan&parse "stream-null?(empty-stream)"))))

   (test-case "parser: stream-null? no vacío"
     (check-not-exn
      (lambda ()
        (scan&parse "stream-null?(stream(1, proc() empty-stream end))"))))

   ;; ---- Operaciones sobre streams ----
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
      (lambda () (scan&parse "take(5, empty-stream)"))))

   (test-case "parser: zip-with"
     (check-not-exn
      (lambda ()
        (scan&parse
         "zip-with(proc(a, b) +(a, b) end, empty-stream, empty-stream)"))))

   ;; ---- Datatype ----
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

   ;; ---- Match ----
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

   ;; ---- Comentarios de bloque ----
   (test-case "parser: comentario de bloque {- -}"
     (check-not-exn
      (lambda ()
        (scan&parse "{- esto es un comentario -} 42"))))

   ;; ---- Unparse ----
   (test-case "unparse: entero"
     (check-not-exn (lambda () (unparse (scan&parse "42")))))

   (test-case "unparse: let"
     (check-not-exn (lambda () (unparse (scan&parse "let x = 10 in x end")))))

   (test-case "unparse: proc"
     (check-not-exn (lambda () (unparse (scan&parse "proc(x) +(x, 1) end")))))

   (test-case "unparse: if"
     (check-not-exn (lambda () (unparse (scan&parse "if true then 1 else 2 end")))))

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
;; ENTREGA 2 — value-of, ambiente, store — Juan Diego Ospina
;; ============================================================

(define entrega-2
  (test-suite
   "Entrega 2 — value-of + ambiente + store"

   ;; ---- Literales ----
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

   ;; ---- Primitivas aritméticas ----
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

   ;; ---- Primitivas relacionales ----
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

   ;; ---- Primitivas booleanas ----
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

   ;; ---- Primitivas de cadenas ----
   (test-case "run: concat"
     (check-equal? (run "concat(\"hola\", \" mundo\")") "hola mundo"))

   (test-case "run: length"
     (check-equal? (run "length(\"abc\")") 3))

   (test-case "run: length cadena vacía"
     (check-equal? (run "length(\"\")") 0))

   ;; ---- Conversión ----
   (test-case "run: to-string de número"
     (check-equal? (run "to-string(42)") "42"))

   (test-case "run: to-string de booleano"
     (check-equal? (run "to-string(true)") "true"))

   (test-case "run: to-number"
     (check-equal? (run "to-number(\"42\")") 42))

   ;; ---- If / elif / else ----
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

   ;; ---- Let ----
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

   ;; ---- Var / Set / Freeze ----
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

   ;; ---- Begin ----
   (test-case "run: begin retorna último valor"
     (check-equal? (run "begin 1; 2; 3 end") 3))

   (test-case "run: begin con efectos"
     (check-equal?
      (run "var x = 0 in begin set x := 1; set x := 2; x end end") 2))

   ;; ---- Proc y Apply ----
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

   ;; ---- Letrec ----
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

   ;; ---- Ejemplo del enunciado: Ejemplo 4 (freeze) ----
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

   ;; ---- Ejemplo del enunciado: Ejemplo 10 (recursión mutua) ----
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
;; Ejecución de todas las suites
;; ============================================================

(module+ test
  (run-tests entrega-1)
  (run-tests entrega-2))
