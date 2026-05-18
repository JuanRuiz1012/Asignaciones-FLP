#lang racket

;; ============================================================
;; Taller 3 — Pruebas de MiniLang+Refs+Tipos
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710, JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701, JUAN FELIPE RUIZ 2359397
;; ============================================================

(require rackunit)
(require rackunit/text-ui)
(require eopl)
(require "interprete-minilang.rkt")
(require (prefix-in chk: "checker-minilang.rkt"))

;; ------------------------------------------------------------
;; Utilidades
;; ------------------------------------------------------------

;; run : String -> SchemeVal
;; Contrato: String -> SchemeVal
;; Propósito: parsea y evalúa un programa fuente MiniLang.
(define (run src)
  (evaluar-programa (parser src)))

;; type-of : String -> type
;; Contrato: String -> type
;; Propósito: parsea y chequea estáticamente un programa fuente MiniLang.
(define (type-of src)
  (chk:type-of-program (parser src)))

;; check-run-error : String -> Boolean
;; Contrato: String -> Boolean
;; Propósito: verifica que la ejecución de src lanza un error.
;; Retorna #t si hay error, falla el test si no lo hay.
(define (check-run-error src)
  (with-handlers
      ([exn:fail? (lambda (e) #t)])
    (run src)
    #f))

;; check-type-error : String -> Boolean
;; Contrato: String -> Boolean
;; Propósito: verifica que el chequeo de tipos de src lanza un error.
(define (check-type-error src)
  (with-handlers
      ([exn:fail? (lambda (e) #t)])
    (type-of src)
    #f))


;; ------------------------------------------------------------
;; Parte 1: TAD referencia y asignación
;; ------------------------------------------------------------

(define parte-1
  (test-suite
   "Parte 1 — TAD referencia y asignación"

   ;; --- Casos correctos ---

   ;; Test 1: let-exp declara variable inmutable y la retorna
   (test-case "let-exp inmutable retorna su valor"
     (check-equal?
      (run "let m : int = 5 in m")
      5
      "let m:int=5 in m debe retornar 5"))

   ;; Test 2: var-decl con set y begin — contador con doble incremento
   (test-case "var-exp con set y begin: acumulación"
     (check-equal?
      (run "var n : int = 0 in
              begin
                set n := +(n, 10) ;
                set n := *(n, 2) ;
                n
              end")
      20
      "n = 0, +10 => 10, *2 => 20"))

   ;; Test 3: begin retorna el valor de la última expresión
   (test-case "begin-exp retorna el último valor"
     (check-equal?
      (run "var n : int = 0 in
              begin
                set n := 1 ;
                set n := 2 ;
                n
              end")
      2
      "begin retorna el último valor (2)"))

   ;; Test 4: freeze exitoso — variable mutable se puede congelar
   (test-case "freeze-exp exitoso sobre var"
     (check-equal?
      (run "var k : int = 7 in
              begin
                set k := +(k, 3) ;
                freeze k ;
                k
              end")
      10
      "k=7, set k:=10, freeze k, retorna 10"))

   ;; Test 5: combinación let (inmutable) envolviendo var (mutable)
   (test-case "combinación let + var anidados"
     (check-equal?
      (run "let base : int = 100 in
              var contador : int = 0 in
                begin
                  set contador := +(contador, base) ;
                  set contador := +(contador, base) ;
                  contador
                end")
      200
      "base=100, contador+=100 dos veces => 200"))

   ;; Test 6 (bonus): var con múltiples variables
   (test-case "var múltiple + set sobre una sola variable"
     (check-equal?
      (run "var p : int = 1 q : int = 2 in
              begin
                set p := *(p, q) ;
                set q := +(p, q) ;
                +(p, q)
              end")
      6
      "p=1*2=2, q=2+2=4, p+q=6"))

   ;; --- Casos de error ---

   ;; Test 7: set sobre variable let lanza error
   (test-case "set sobre let lanza error"
     (check-true
      (check-run-error "let m : int = 5 in set m := 10")
      "set sobre variable let debe lanzar error"))

   ;; Test 8: set sobre variable congelada lanza error
   (test-case "set sobre variable congelada lanza error"
     (check-true
      (check-run-error "var k : int = 1 in
                          begin
                            freeze k ;
                            set k := 2
                          end")
      "set sobre frozen debe lanzar error"))

   ;; Test 9: freeze sobre let lanza error
   (test-case "freeze sobre let lanza error"
     (check-true
      (check-run-error "let m : int = 5 in freeze m")
      "freeze sobre let debe lanzar error"))

   ;; Test 10 (bonus): freeze dos veces lanza error
   (test-case "freeze dos veces lanza error"
     (check-true
      (check-run-error "var k : int = 1 in
                          begin
                            freeze k ;
                            freeze k
                          end")
      "freeze sobre frozen debe lanzar error"))
   ))


;; ------------------------------------------------------------
;; Parte 2: Procedimientos con asignación
;; ------------------------------------------------------------

(define parte-2
  (test-suite
   "Parte 2 — Procedimientos con asignación"

   ;; Test 1: clausura captura variable mutable y la modifica
   ;; El cuerpo del proc lee y escribe sobre el saldo capturado.
   (test-case "clausura captura variable mutable y acumula"
     (check-equal?
      (run "var saldo : int = 100 in
              let depositar : (int -> void) =
                proc (int m) set saldo := +(saldo, m) in
                begin
                  (depositar 50) ;
                  (depositar 25) ;
                  saldo
                end")
      175
      "saldo = 100 + 50 + 25 = 175"))

   ;; Test 2: parámetro formal es inmutable (marca 'let)
   ;; Intentar set sobre un parámetro formal debe lanzar error.
   (test-case "parámetro formal es inmutable"
     (check-true
      (check-run-error "let f : (int -> int) =
                           proc (int x) begin set x := +(x, 1) ; x end
                         in (f 0)")
      "set sobre parámetro formal (let) debe lanzar error"))

   ;; Test 3: clausura comparte referencia con el ámbito externo
   ;; Dos clausuras distintas comparten la misma variable capturada.
   (test-case "dos clausuras comparten la misma variable capturada"
     (check-equal?
      (run "var cuenta : int = 0 in
              let inc : (void -> int) = proc () begin set cuenta := +(cuenta,1) ; cuenta end
                  reset : (void -> void) = proc () set cuenta := 0 in
                begin
                  (inc) ;
                  (inc) ;
                  (inc) ;
                  (reset) ;
                  (inc) ;
                  cuenta
                end")
      1
      "inc x3, reset, inc x1 => 1"))

   ;; Test 4 (bonus): procedimiento retorna resultado de la última expresión
   (test-case "proc retorna el valor de la última expresión"
     (check-equal?
      (run "let doble : (int -> int) = proc (int n) *(n, 2) in
              (doble 21)")
      42
      "(doble 21) debe retornar 42"))
   ))


;; ------------------------------------------------------------
;; Parte 3: Chequeador estático de tipos
;; ------------------------------------------------------------

(define parte-3
  (test-suite
   "Parte 3 — Chequeador estático de tipos"

   ;; --- Bien tipados ---

   ;; Test 1: var + set bien tipado retorna int
   (test-case "programa con set bien tipado: retorna int"
     (check-equal?
      (chk:type-to-external-form
       (type-of "var c : int = 0 in begin set c := +(c, 3) ; c end"))
      'int
      "var c:int=0; set c:=+(c,3); c => int"))

   ;; Test 2: let + begin bien tipado retorna int
   (test-case "programa con begin bien tipado: retorna int"
     (check-equal?
      (chk:type-to-external-form
       (type-of "let x : int = 1 in begin x ; x end"))
      'int
      "let x:int=1 in begin x; x end => int"))

   ;; Test 3: var + freeze bien tipado retorna void
   (test-case "programa con freeze bien tipado: retorna void"
     (check-equal?
      (chk:type-to-external-form
       (type-of "var k : int = 0 in begin freeze k ; k end"))
      'int
      "var k:int; begin freeze k; k end => int (retorna k)"))

   ;; Test 4 (bonus): proc bien tipado
   (test-case "proc bien tipado: chequea tipo correcto"
     (check-equal?
      (chk:type-to-external-form
       (type-of "let f : (int -> int) = proc (int n) *(n,2) in (f 5)"))
      'int
      "(f 5) donde f duplica => int"))

   ;; Test 5 (bonus): if bien tipado
   (test-case "if bien tipado con bool y ramas int"
     (check-equal?
      (chk:type-to-external-form
       (type-of "if true then 1 else 2"))
      'int
      "if true then 1 else 2 => int"))

   ;; --- Mal tipados ---

   ;; Test 6: set sobre let — el chequeador rechaza
   (test-case "set sobre let — el chequeador rechaza"
     (check-true
      (check-type-error "let m : int = 5 in set m := 10")
      "set sobre let debe lanzar error de tipos"))

   ;; Test 7: set con cambio de tipo — el chequeador rechaza
   (test-case "set con cambio de tipo — el chequeador rechaza"
     (check-true
      (check-type-error "var flag : bool = true in set flag := 0")
      "asignar int a bool debe lanzar error de tipos"))

   ;; Test 8: freeze sobre let — el chequeador rechaza
   (test-case "freeze sobre let — el chequeador rechaza"
     (check-true
      (check-type-error "let m : int = 5 in freeze m")
      "freeze sobre let debe lanzar error de tipos"))

   ;; Test 9 (bonus): aplicar no-procedimiento — error de tipos
   (test-case "aplicar no-procedimiento lanza error de tipos"
     (check-true
      (check-type-error "(5 3)")
      "aplicar 5 como procedimiento debe lanzar error"))

   ;; Test 10 (bonus): if con ramas de tipos distintos — error
   (test-case "if con ramas de tipos distintos lanza error de tipos"
     (check-true
      (check-type-error "if true then 1 else false")
      "ramas int y bool en if deben lanzar error de tipos"))
   ))


;; ------------------------------------------------------------
;; Ejecución de todas las suites
;; ------------------------------------------------------------
(module+ test
  (run-tests parte-1)
  (run-tests parte-2)
  (run-tests parte-3))
