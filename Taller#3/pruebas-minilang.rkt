#lang eopl

;; ============================================================
;; Taller 3 — Pruebas con rackunit (MiniLang+Refs+Tipos)
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710, JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701, JUAN FELIPE RUIZ 2359397
;; ============================================================
;;
;; Organización (compatible con autograding de GitHub Classroom):
;;   raco test pruebas-minilang.rkt          → corre las 3 suites
;;   raco test -s parte-1 pruebas-minilang.rkt
;;   raco test -s parte-2 pruebas-minilang.rkt
;;   raco test -s parte-3 pruebas-minilang.rkt
;;
;; SINTAXIS DEL LENGUAJE (con anotaciones de tipo):
;;   let  x : int = 5         in ...
;;   var  n : int = 0         in ...
;;   set  n := +(n, 1)
;;   proc (int x, bool flag)  body
;;   begin e1 ; e2 ; e3 end
;;   freeze id
;; ============================================================

(require rackunit)
(require rackunit/text-ui)
(require "interprete-minilang.rkt")
(require (prefix-in chk: "checker-minilang.rkt"))

;; ============================================================
;; UTILIDADES
;; ============================================================

;; run : String -> SchemeVal
;; Propósito: parsea y evalúa un programa fuente con el intérprete.
(define run
  (lambda (src)
    (evaluar-programa (parser src))))

;; type-of : String -> S-exp
;; Propósito: parsea y chequea un programa, retorna la forma externa del tipo.
(define type-of
  (lambda (src)
    (chk:run-check src)))

;; check-run-error : String -> Boolean
;; Propósito: retorna #t si (run src) lanza una excepción, #f si no.
;;   Usa guard (R6RS), disponible en #lang eopl.
(define check-run-error
  (lambda (src)
    (guard (exn
            [(condition? exn) #t])
      (run src)
      #f)))

;; check-type-error : String -> Boolean
;; Propósito: retorna #t si (type-of src) lanza una excepción, #f si no.
;;   Usa guard (R6RS), disponible en #lang eopl.
(define check-type-error
  (lambda (src)
    (guard (exn
            [(condition? exn) #t])
      (type-of src)
      #f)))

;; ============================================================
;; PARTE 1 — TAD referencia y asignación
;; ============================================================
;; Mínimos del enunciado:
;;   5 casos correctos: let, var, begin, freeze, combinación
;;   3 casos de error:  set sobre let, set sobre congelada, freeze sobre let

(define parte-1
  (test-suite
   "Parte 1 — TAD referencia y asignación"

   ;; ---- Casos correctos ----

   (test-case "let-exp inmutable: ligadura simple"
     ;; let crea una ligadura 'let; el cuerpo retorna su valor
     (check-equal? (run "let m : int = 5 in m")
                   5))

   (test-case "var-exp con set y begin: actualización mutable"
     ;; var crea 'var; set actualiza; begin retorna el último valor
     (check-equal?
      (run "var n : int = 0 in
              begin
                set n := +(n, 10);
                set n := *(n, 2);
                n
              end")
      20))

   (test-case "begin-exp retorna el último valor"
     ;; begin evalúa en orden y retorna la última expresión
     (check-equal?
      (run "var n : int = 0 in
              begin set n := 1; set n := 2; n end")
      2))

   (test-case "freeze-exp: valor conservado tras congelar"
     ;; freeze cambia marca de 'var a 'frozen; lectura sigue funcionando
     (check-equal?
      (run "var k : int = 7 in
              begin set k := +(k, 3); freeze k; k end")
      10))

   (test-case "combinación: let base + var anidado"
     ;; let y var se pueden anidar; let no es mutable, var sí
     (check-equal?
      (run "let base : int = 100 in
              var contador : int = 0 in
                begin
                  set contador := +(contador, base);
                  set contador := +(contador, 50);
                  contador
                end")
      150))

   (test-case "intercambio de valores con variable auxiliar"
     ;; Patrón clásico de swap usando una let temporal
     (check-equal?
      (run "var x : int = 10 in
              var y : int = 20 in
                begin
                  let tmp : int = x in
                    begin
                      set x := y;
                      set y := tmp;
                      +(x, y)
                    end
                end")
      30))

   ;; ---- Casos de error ----

   (test-case "ERROR — set sobre let debe fallar"
     ;; Intentar mutar una ligadura 'let debe lanzar error
     (check-true
      (check-run-error "let m : int = 5 in set m := 10")))

   (test-case "ERROR — set sobre variable congelada debe fallar"
     ;; Después de freeze, set debe lanzar error
     (check-true
      (check-run-error
       "var k : int = 1 in begin freeze k; set k := 2 end")))

   (test-case "ERROR — freeze sobre let debe fallar"
     ;; No se puede congelar una ligadura inmutable
     (check-true
      (check-run-error "let m : int = 5 in freeze m")))

   (test-case "ERROR — freeze dos veces sobre la misma variable"
     ;; Una variable ya congelada no puede volver a congelarse
     (check-true
      (check-run-error
       "var k : int = 10 in begin freeze k; freeze k end")))
   ))

;; ============================================================
;; PARTE 2 — Procedimientos con asignación
;; ============================================================
;; Mínimos del enunciado:
;;   2 ejemplos con clausuras que mutan variables capturadas

(define parte-2
  (test-suite
   "Parte 2 — Procedimientos con asignación"

   (test-case "clausura captura variable mutable: contador"
     ;; La clausura `inc` captura `contador` por referencia.
     ;; Cada llamada incrementa y retorna el nuevo valor.
     (check-equal?
      (run "var contador : int = 0 in
              let inc : (int -> int) = proc (int delta)
                                        begin
                                          set contador := +(contador, delta);
                                          contador
                                        end
              in begin
                   (inc 1);
                   (inc 1);
                   (inc 1)
                 end")
      3))

   (test-case "clausura captura variable mutable: cuenta bancaria"
     ;; depositar y retirar comparten la misma variable `saldo`
     (check-equal?
      (run "var saldo : int = 100 in
              let depositar : (int -> int) = proc (int m)
                                              begin
                                                set saldo := +(saldo, m);
                                                saldo
                                              end
                  retirar   : (int -> int) = proc (int m)
                                              begin
                                                set saldo := -(saldo, m);
                                                saldo
                                              end
              in begin
                   (depositar 50);
                   (depositar 25);
                   (retirar 30);
                   saldo
                 end")
      145))

   (test-case "clausura con dos mutaciones independientes"
     ;; inc y dec mutan x compartido; el resultado debe ser 1
     (check-equal?
      (run "var x : int = 0 in
              let inc : (int -> int) = proc (int dummy)
                                        begin set x := add1(x); x end
                  dec : (int -> int) = proc (int dummy)
                                        begin set x := sub1(x); x end
              in begin
                   (inc 0);
                   (inc 0);
                   (dec 0);
                   x
                 end")
      1))

   (test-case "ERROR — parámetro formal es inmutable"
     ;; Los parámetros se crean con marca 'let; set debe fallar
     (check-true
      (check-run-error
       "let f : (int -> int) = proc (int p) set p := +(p, 1)
        in (f 0)")))

   (test-case "procedimientos de orden superior"
     ;; apply recibe una función y un argumento
     (check-equal?
      (run "let apply : ((int -> int) -> int) =
                proc (int f, int arg) (f arg)
            in
              let inc : (int -> int) = proc (int n) add1(n)
              in (apply inc 5)")
      6))
   ))

;; ============================================================
;; PARTE 3 — Chequeador estático de tipos
;; ============================================================
;; Mínimos del enunciado:
;;   3 bien tipados: set, begin, freeze
;;   3 mal tipados:  set sobre let, set con cambio de tipo, freeze sobre let

(define parte-3
  (test-suite
   "Parte 3 — Chequeador estático de tipos"

   ;; ---- Tipos básicos ----

   (test-case "literal entero tiene tipo int"
     (check-equal? (type-of "42") 'int))

   (test-case "true tiene tipo bool"
     (check-equal? (type-of "true") 'bool))

   (test-case "false tiene tipo bool"
     (check-equal? (type-of "false") 'bool))

   (test-case "variable del ambiente inicial tiene tipo int"
     (check-equal? (type-of "x") 'int))

   ;; ---- Primitivas aritméticas ----

   (test-case "suma de enteros retorna int"
     (check-equal? (type-of "+(1, 2, 3)") 'int))

   (test-case "add1 retorna int"
     (check-equal? (type-of "add1(5)") 'int))

   (test-case "ERROR — suma con booleano"
     (check-true (check-type-error "+(true, 1)")))

   ;; ---- Primitivas relacionales ----

   (test-case "mayor-que retorna bool"
     (check-equal? (type-of ">(5, 3)") 'bool))

   (test-case "igual retorna bool"
     (check-equal? (type-of "==(4, 4)") 'bool))

   (test-case "ERROR — relacional con booleano"
     (check-true (check-type-error ">(true, 1)")))

   ;; ---- Primitivas lógicas ----

   (test-case "not(bool) retorna bool"
     (check-equal? (type-of "not(true)") 'bool))

   (test-case "and(bool, bool) retorna bool"
     (check-equal? (type-of "and(true, false)") 'bool))

   (test-case "ERROR — not aplicado a int"
     (check-true (check-type-error "not(5)")))

   ;; ---- if-exp ----

   (test-case "if bien tipado retorna int"
     (check-equal? (type-of "if true then 1 else 2") 'int))

   (test-case "if bien tipado retorna bool"
     (check-equal? (type-of "if false then true else false") 'bool))

   (test-case "ERROR — if con condición no booleana"
     (check-true (check-type-error "if 1 then 2 else 3")))

   (test-case "ERROR — ramas de if con tipos distintos"
     (check-true (check-type-error "if true then 1 else false")))

   ;; ---- let-exp y var-exp ----

   (test-case "let retorna el tipo del cuerpo"
     (check-equal? (type-of "let m : int = 10 in m") 'int))

   (test-case "var retorna el tipo del cuerpo"
     (check-equal? (type-of "var n : int = 5 in +(n, 1)") 'int))

   ;; ---- set-exp BIEN TIPADO ----

   (test-case "BIEN TIPADO — set sobre var retorna void"
     (check-equal? (type-of "var c : int = 0 in
                               begin set c := +(c, 3); c end")
                   'int))

   (test-case "BIEN TIPADO — begin retorna tipo de última expresión"
     (check-equal? (type-of "var x : int = 1 in
                               begin x; x end")
                   'int))

   (test-case "BIEN TIPADO — freeze sobre var retorna void, cuerpo int"
     (check-equal? (type-of "var k : int = 0 in
                               begin freeze k; k end")
                   'int))

   ;; ---- set-exp MAL TIPADO ----

   (test-case "ERROR — set sobre let (identificador inmutable)"
     ;; El chequeador debe rechazar set sobre 'let
     (check-true (check-type-error "let m : int = 5 in set m := 10")))

   (test-case "ERROR — set con cambio de tipo (int -> bool)"
     ;; No se permite cambiar el tipo de una variable mutable
     (check-true (check-type-error "var flag : bool = true in set flag := 0")))

   (test-case "ERROR — freeze sobre let (identificador inmutable)"
     (check-true (check-type-error "let m : int = 5 in freeze m")))

   ;; ---- Procedimientos ----

   (test-case "proc con un parámetro int retorna proc-type"
     (check-equal? (type-of "proc (int x) +(x, 1)")
                   '((int) -> int)))

   (test-case "proc con dos parámetros retorna proc-type"
     (check-equal? (type-of "proc (int x, int y) +(x, y)")
                   '((int int) -> int)))

   (test-case "aplicación bien tipada retorna tipo del resultado"
     (check-equal? (type-of "let f : (int -> int) = proc (int x) x in (f 5)")
                   'int))

   (test-case "ERROR — aridad incorrecta en aplicación"
     (check-true (check-type-error
                  "let f : (int -> int) = proc (int x) x in (f 1 2)")))

   ;; ---- Ejemplos compuestos ----

   (test-case "BIEN TIPADO — contador con clausura"
     (check-equal?
      (type-of "var contador : int = 0 in
                  let inc : (int -> int) = proc (int delta)
                              begin
                                set contador := +(contador, delta);
                                contador
                              end
                  in (inc 1)")
      'int))

   (test-case "BIEN TIPADO — if con primitiva relacional"
     (check-equal?
      (type-of "let f : (int -> int) = proc (int n)
                          if ==(n, 0) then 1 else *(n, 2)
                in (f 5)")
      'int))
   ))

;; ============================================================
;; EJECUCIÓN
;; ============================================================

(module+ test
  (run-tests parte-1)
  (run-tests parte-2)
  (run-tests parte-3))
