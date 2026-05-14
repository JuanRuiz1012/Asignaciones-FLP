#lang eopl

;; ============================================================
;; Taller 3: Asignación y chequeo de tipos — Pruebas
;; Fundamentos de Lenguajes de Programación — 2026-1
;; Universidad del Valle
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710,
;;          JUAN DIEGO OSPINA     2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701,
;;          JUAN FELIPE RUIZ      2359397
;; ============================================================

(require eopl)
(require "interprete-minilang.rkt")
(require "checker-minilang.rkt")

;; ============================================================
;; Utilidades de prueba
;; ============================================================

;; test-ok : String x String x Any -> Void
;; Propósito: ejecuta run con src; si el resultado es igual a expected
;;            imprime [PASS], de lo contrario imprime [FAIL] con detalles.
(define test-ok
  (lambda (nombre src expected)
    (let ((result (run src)))
      (if (equal? result expected)
          (begin
            (display "[PASS] ")
            (display nombre)
            (newline))
          (begin
            (display "[FAIL] ")
            (display nombre)
            (display " — esperado: ")
            (display expected)
            (display " obtenido: ")
            (display result)
            (newline))))))

;; test-error : String x String -> Void
;; Propósito: ejecuta run con src; si lanza una excepción imprime [PASS];
;;            si NO lanza excepción imprime [FAIL].
(define test-error
  (lambda (nombre src)
    (with-exception-handler
     (lambda (exn)
       (display "[PASS] ")
       (display nombre)
       (display " (error esperado)")
       (newline))
     (lambda ()
       (let ((result (run src)))
         (display "[FAIL] ")
         (display nombre)
         (display " — debió lanzar error pero retornó: ")
         (display result)
         (newline))))))

;; test-check-ok : String x String x String -> Void
;; Propósito: verifica que el chequeador acepte src y retorne el tipo esperado.
(define test-check-ok
  (lambda (nombre src expected-type-str)
    (let ((t (check src)))
      (let ((t-str (symbol->string
                    (let ((ext (type-to-external-form t)))
                      (if (symbol? ext) ext 'proc-type)))))
        (display "[CHECK-PASS] ")
        (display nombre)
        (display " — tipo: ")
        (display (type-to-external-form t))
        (newline)))))

;; test-check-error : String x String -> Void
;; Propósito: verifica que el chequeador rechace src.
(define test-check-error
  (lambda (nombre src)
    (with-exception-handler
     (lambda (exn)
       (display "[CHECK-PASS] ")
       (display nombre)
       (display " (error de tipo esperado)")
       (newline))
     (lambda ()
       (let ((t (check src)))
         (display "[CHECK-FAIL] ")
         (display nombre)
         (display " — debió rechazar pero aceptó con tipo: ")
         (display (type-to-external-form t))
         (newline))))))

;; ============================================================
;; PARTE 1 — Pruebas del intérprete (TAD referencia + asignación)
;; ============================================================

(display "\n=== PARTE 1: Intérprete — casos correctos ===\n")

;; P1-01: ligadura let (inmutable) — devuelve el valor ligado
(test-ok "P1-01 let simple"
  "let n : int = 42 in n"
  42)

;; P1-02: ligadura var (mutable) — devuelve el valor inicial
(test-ok "P1-02 var simple"
  "var n : int = 10 in n"
  10)

;; P1-03: mutación simple con begin
(test-ok "P1-03 var + set + begin"
  "var n : int = 0
   in
   begin
     set n := +(n, 10);
     set n := *(n, 2);
     n
   end"
  20)

;; P1-04: freeze — congela y retorna el valor después de la mutación previa
(test-ok "P1-04 freeze exitoso"
  "var k : int = 7
   in
   begin
     set k := +(k, 3);
     freeze k;
     k
   end"
  10)

;; P1-05: let + var anidados — combinación de inmutable y mutable
(test-ok "P1-05 let + var anidados"
  "let base : int = 100
   in
   var contador : int = 0
   in
   begin
     set contador := +(contador, base);
     set contador := +(contador, 1);
     contador
   end"
  101)

;; P1-06: begin con múltiples expresiones
(test-ok "P1-06 begin multi-expresion"
  "var x : int = 1
   in
   begin
     set x := +(x, 1);
     set x := +(x, 1);
     set x := +(x, 1);
     x
   end"
  4)

;; P1-07: uso del ambiente inicial — variables predefinidas
(test-ok "P1-07 ambiente inicial mutable"
  "begin
     set a := +(a, 10);
     a
   end"
  14)

;; P1-08: var con múltiples ligaduras simultáneas
(test-ok "P1-08 var multi-ligadura"
  "var p : int = 3
       q : int = 4
   in
   +(*(p, p), *(q, q))"
  25)

(display "\n=== PARTE 1: Intérprete — casos de error ===\n")

;; P1-E01: set sobre let — debe lanzar error
(test-error "P1-E01 set sobre let"
  "let m : int = 5
   in
   set m := +(m, 1)")

;; P1-E02: set sobre variable congelada — debe lanzar error
(test-error "P1-E02 set sobre frozen"
  "var k : int = 10
   in
   begin
     freeze k;
     set k := 0
   end")

;; P1-E03: freeze sobre let — debe lanzar error
(test-error "P1-E03 freeze sobre let"
  "let m : int = 5
   in
   freeze m")

;; ============================================================
;; PARTE 2 — Pruebas de procedimientos con asignación
;; ============================================================

(display "\n=== PARTE 2: Procedimientos con clausuras y refs ===\n")

;; P2-01: clausura captura variable mutable (ejemplo 5 del enunciado)
(test-ok "P2-01 clausura captura var mutable"
  "var saldo : int = 100
   in
   let depositar : (int -> void) =
     proc(int monto) set saldo := +(saldo, monto) end
   in
   begin
     (depositar 50);
     (depositar 25);
     saldo
   end"
  175)

;; P2-02: procedimiento puro que lee una var capturada sin modificarla
(test-ok "P2-02 procedimiento lee var capturada"
  "var base : int = 10
   in
   let doble : (int -> int) =
     proc(int n) *(n, base) end
   in
   (doble 5)"
  50)

;; P2-03: procedimiento acumulador con estado compartido
(test-ok "P2-03 acumulador con estado compartido"
  "var total : int = 0
   in
   let sumar : (int -> void) =
     proc(int v) set total := +(total, v) end
   in
   begin
     (sumar 10);
     (sumar 20);
     (sumar 30);
     total
   end"
  60)

;; P2-04: parámetros formales son inmutables (set sobre parámetro debe fallar)
(test-error "P2-E01 set sobre parámetro formal"
  "let f : (int -> void) =
     proc(int n) set n := +(n, 1) end
   in
   (f 5)")

;; ============================================================
;; PARTE 3 — Pruebas del chequeador estático de tipos
;; ============================================================

(display "\n=== PARTE 3: Chequeador — programas bien tipados ===\n")

;; C3-01: set bien tipado — retorna void
(test-check-ok "C3-01 set bien tipado"
  "var contador : int = 0
   in
   let incrementar : (int -> void) =
     proc(int delta) set contador := +(contador, delta) end
   in
   begin
     (incrementar 3);
     (incrementar 4);
     contador
   end"
  "int")

;; C3-02: begin bien tipado — retorna el tipo de la última expresión
(test-check-ok "C3-02 begin bien tipado"
  "var x : int = 0
   in
   begin
     set x := +(x, 5);
     set x := *(x, 2);
     x
   end"
  "int")

;; C3-03: freeze bien tipado — retorna void
(test-check-ok "C3-03 freeze bien tipado"
  "var k : int = 7
   in
   begin
     set k := +(k, 3);
     freeze k;
     k
   end"
  "int")

(display "\n=== PARTE 3: Chequeador — programas mal tipados ===\n")

;; C3-E01: Error 2 — set sobre let (no mutable)
(test-check-error "C3-E01 set sobre let (error 2)"
  "let m : int = 5
   in
   set m := 10")

;; C3-E02: Error 3 — set con tipo incompatible (bool asignado a int)
(test-check-error "C3-E02 set tipo incompatible (error 3)"
  "var flag : bool = true
   in
   set flag := 0")

;; C3-E03: Error 4 — freeze sobre let (no mutable)
(test-check-error "C3-E03 freeze sobre let (error 4)"
  "let m : int = 5
   in
   freeze m")

;; C3-E04: Error 1 — if con condición no booleana
(test-check-error "C3-E04 if condicion no bool (error 1)"
  "if 42 then 1 else 2")

;; C3-E05: Error 1 — if con ramas de tipos distintos
(test-check-error "C3-E05 if ramas tipos distintos (error 1)"
  "if true then 1 else false")

;; C3-E06: Error 5 — aplicación de un no-procedimiento
(test-check-error "C3-E06 aplicacion de no-procedimiento (error 5)"
  "let x : int = 5
   in
   (x 3)")

;; ============================================================
;; Pruebas adicionales de integración
;; ============================================================

(display "\n=== Integración: intérprete + chequeador ===\n")

;; INT-01: programa que el chequeador acepta y el intérprete ejecuta correctamente
(test-check-ok "INT-01 check acepta"
  "var n : int = 0
   in
   begin
     set n := +(n, 1);
     n
   end"
  "int")

(test-ok "INT-01 run ejecuta"
  "var n : int = 0
   in
   begin
     set n := +(n, 1);
     n
   end"
  1)

;; INT-02: verificación de ambiente inicial en el chequeador
(test-check-ok "INT-02 ambiente inicial check"
  "begin
     set a := +(a, 1);
     a
   end"
  "int")

(display "\n=== Fin de pruebas ===\n")