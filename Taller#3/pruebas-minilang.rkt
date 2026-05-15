#lang eopl

;; ============================================================
;; Taller 3 — Pruebas completas (Intérprete y Chequeador)
;; Fundamentos de Lenguajes de Programación — 2026-1
;; ============================================================
;; Autores: JHORMAN RICARDO LOAIZA 2359710, JUAN DIEGO OSPINA 2359486,
;;          MAURICIO ALEJANDRO ROJAS 2359701, JUAN FELIPE RUIZ 2359397
;; ============================================================

(require eopl)
(require "interprete-minilang.rkt")
(require "checker-minilang.rkt")

;; ============================================================
;; UTILIDADES PARA TESTING
;; ============================================================

;; test-interprete : String x Any -> Void
;; Propósito: prueba que el intérprete evalúe correctamente.
(define test-interprete
  (lambda (programa resultado-esperado)
    (let ((pgm (parser programa)))
      (let ((resultado (evaluar-programa pgm)))
        (if (equal? resultado resultado-esperado)
            (begin
              (display "✓ PASS: ")
              (display programa)
              (newline))
            (begin
              (display "✗ FAIL: ")
              (display programa)
              (newline)
              (display "  Esperado: ")
              (display resultado-esperado)
              (display ", Obtenido: ")
              (display resultado)
              (newline)))))))

;; test-checker : String x Symbol -> Void
;; Propósito: prueba que el chequeador infiera el tipo correcto.
(define test-checker
  (lambda (programa tipo-esperado)
    (let ((tipo-obtenido (run-check programa)))
      (if (equal? tipo-obtenido tipo-esperado)
          (begin
            (display "✓ PASS TYPE: ")
            (display programa)
            (newline))
          (begin
            (display "✗ FAIL TYPE: ")
            (display programa)
            (newline)
            (display "  Esperado: ")
            (display tipo-esperado)
            (display ", Obtenido: ")
            (display tipo-obtenido)
            (newline))))))

;; test-error-interprete : String -> Void
;; Propósito: verifica que el intérprete lance un error.
(define test-error-interprete
  (lambda (programa)
    (let/cc k
      (with-handlers ([exn:fail? (lambda (e)
                                   (display "✓ PASS ERROR: ")
                                   (display programa)
                                   (newline)
                                   (k (void)))])
        (let ((pgm (parser programa)))
          (evaluar-programa pgm)
          (begin
            (display "✗ FAIL: Debió lanzar error: ")
            (display programa)
            (newline)))))))

;; test-error-checker : String -> Void
;; Propósito: verifica que el chequeador lance un error.
(define test-error-checker
  (lambda (programa)
    (let/cc k
      (with-handlers ([exn:fail? (lambda (e)
                                   (display "✓ PASS TYPE ERROR: ")
                                   (display programa)
                                   (newline)
                                   (k (void)))])
        (run-check programa)
        (begin
          (display "✗ FAIL: Debió lanzar error de tipo: ")
          (display programa)
          (newline))))))

;; ============================================================
;; PARTE 1: PRUEBAS DEL TAD REFERENCIA Y ASIGNACIÓN
;; ============================================================

(display "========================================")
(newline)
(display "PARTE 1: TAD REFERENCIA Y ASIGNACIÓN")
(newline)
(display "========================================")
(newline)
(newline)

;; ----------------------------------------
;; 1.1 let-exp (inmutables)
;; ----------------------------------------
(display "--- 1.1 let-exp (inmutables) ---")
(newline)

(test-interprete "let x = 10 in x" 10)
(test-interprete "let x = 5 y = 3 in +(x,y)" 8)
(test-interprete "let x = 10 in let y = 20 in +(x,y)" 30)

;; ----------------------------------------
;; 1.2 var-exp (mutables)
;; ----------------------------------------
(display "--- 1.2 var-exp (mutables) ---")
(newline)

(test-interprete "var x = 10 in x" 10)
(test-interprete "var x = 5 y = 3 in +(x,y)" 8)

;; ----------------------------------------
;; 1.3 set-exp (asignación)
;; ----------------------------------------
(display "--- 1.3 set-exp (asignación) ---")
(newline)

(test-interprete "var x = 10 in begin set x = 20; x end" 20)
(test-interprete "var x = 5 in begin set x = +(x,1); x end" 6)
(test-interprete 
 "var x = 0 y = 1 in begin set x = 10; set y = 20; +(x,y) end" 
 30)

;; Error: asignar a let
(test-error-interprete "let x = 10 in set x = 20")

;; ----------------------------------------
;; 1.4 begin-exp (secuenciación)
;; ----------------------------------------
(display "--- 1.4 begin-exp (secuenciación) ---")
(newline)

(test-interprete "begin 1; 2; 3 end" 3)
(test-interprete "var x = 0 in begin set x = 1; set x = 2; x end" 2)
(test-interprete 
 "var x = 10 y = 20 in begin set x = +(x,5); set y = +(y,5); +(x,y) end" 
 40)

;; ----------------------------------------
;; 1.5 freeze-exp (congelación)
;; ----------------------------------------
(display "--- 1.5 freeze-exp (congelación) ---")
(newline)

(test-interprete "var x = 10 in begin freeze x; x end" 10)
(test-interprete 
 "var x = 5 in begin set x = 10; freeze x; x end" 
 10)

;; Error: asignar después de freeze
(test-error-interprete 
 "var x = 10 in begin freeze x; set x = 20 end")

;; Error: congelar let
(test-error-interprete "let x = 10 in freeze x")

;; Error: congelar dos veces
(test-error-interprete 
 "var x = 10 in begin freeze x; freeze x end")

;; ----------------------------------------
;; 1.6 Ejemplos complejos de asignación
;; ----------------------------------------
(display "--- 1.6 Ejemplos complejos ---")
(newline)

;; Factorial iterativo
(test-interprete
 "var n = 5 fact = 1 in begin
    var i = 1 in begin
      set i = 1;
      cond
        <=(i, n) ==> begin set fact = *(fact, i); set i = add1(i) end
        <=(i, n) ==> begin set fact = *(fact, i); set i = add1(i) end
        <=(i, n) ==> begin set fact = *(fact, i); set i = add1(i) end
        <=(i, n) ==> begin set fact = *(fact, i); set i = add1(i) end
        <=(i, n) ==> begin set fact = *(fact, i); set i = add1(i) end
      else ==> 0
      end;
      fact
    end
  end"
 120)

;; Intercambio de valores
(test-interprete
 "var x = 10 y = 20 in begin
    let temp = x in begin
      set x = y;
      set y = temp;
      +(x, y)
    end
  end"
 30)

;; ============================================================
;; PARTE 2: PRUEBAS DE PROCEDIMIENTOS CON ASIGNACIÓN
;; ============================================================

(display "")
(newline)
(display "========================================")
(newline)
(display "PARTE 2: PROCEDIMIENTOS CON ASIGNACIÓN")
(newline)
(display "========================================")
(newline)
(newline)

;; ----------------------------------------
;; 2.1 proc-exp y app-exp básicos
;; ----------------------------------------
(display "--- 2.1 Procedimientos básicos ---")
(newline)

(test-interprete "(proc (x) +(x, 1) 5)" 6)
(test-interprete "(proc (x, y) +(x, y) 3 4)" 7)
(test-interprete "let f = proc (x) *(x, x) in (f 5)" 25)

;; ----------------------------------------
;; 2.2 Clausuras que capturan variables mutables
;; ----------------------------------------
(display "--- 2.2 Captura de variables mutables ---")
(newline)

;; Contador con clausura
(test-interprete
 "var contador = 0 in
  let inc = proc () begin set contador = add1(contador); contador end in
  begin
    (inc);
    (inc);
    (inc)
  end"
 3)

;; Múltiples clausuras sobre misma variable
(test-interprete
 "var x = 0 in
  let inc = proc () begin set x = add1(x); x end
      dec = proc () begin set x = sub1(x); x end
  in begin
    (inc);
    (inc);
    (dec);
    x
  end"
 1)

;; ----------------------------------------
;; 2.3 Parámetros son inmutables
;; ----------------------------------------
(display "--- 2.3 Parámetros inmutables ---")
(newline)

;; Error: asignar a parámetro
(test-error-interprete
 "let f = proc (x) set x = 10 in (f 5)")

;; ----------------------------------------
;; 2.4 Procedimientos de orden superior
;; ----------------------------------------
(display "--- 2.4 Orden superior ---")
(newline)

(test-interprete
 "let apply = proc (f, x) (f x) in
  let inc = proc (n) add1(n) in
  (apply inc 5)"
 6)

(test-interprete
 "let makeAdder = proc (x) proc (y) +(x, y) in
  let add5 = (makeAdder 5) in
  (add5 10)"
 15)

;; ============================================================
;; PARTE 3: PRUEBAS DEL CHEQUEADOR DE TIPOS
;; ============================================================

(display "")
(newline)
(display "========================================")
(newline)
(display "PARTE 3: CHEQUEADOR DE TIPOS")
(newline)
(display "========================================")
(newline)
(newline)

;; ----------------------------------------
;; 3.1 Tipos básicos
;; ----------------------------------------
(display "--- 3.1 Tipos básicos ---")
(newline)

(test-checker "42" 'int)
(test-checker "true" 'bool)
(test-checker "false" 'bool)
(test-checker "x" 'int)

;; ----------------------------------------
;; 3.2 Primitivas aritméticas
;; ----------------------------------------
(display "--- 3.2 Primitivas aritméticas ---")
(newline)

(test-checker "+(1, 2, 3)" 'int)
(test-checker "-(10, 5)" 'int)
(test-checker "*(2, 3, 4)" 'int)
(test-checker "/(10, 2)" 'int)
(test-checker "add1(5)" 'int)
(test-checker "sub1(10)" 'int)

;; Errores de tipo
(test-error-checker "+(true, 1)")
(test-error-checker "add1(false)")

;; ----------------------------------------
;; 3.3 Primitivas relacionales
;; ----------------------------------------
(display "--- 3.3 Primitivas relacionales ---")
(newline)

(test-checker ">(5, 3)" 'bool)
(test-checker "<(2, 10)" 'bool)
(test-checker ">=(5, 5)" 'bool)
(test-checker "<=(3, 7)" 'bool)
(test-checker "==(4, 4)" 'bool)

;; Errores de tipo
(test-error-checker ">(true, 1)")
(test-error-checker "==(false, false)")

;; ----------------------------------------
;; 3.4 Primitivas lógicas
;; ----------------------------------------
(display "--- 3.4 Primitivas lógicas ---")
(newline)

(test-checker "not(true)" 'bool)
(test-checker "and(true, false)" 'bool)
(test-checker "or(false, true)" 'bool)

;; Errores de tipo
(test-error-checker "not(5)")
(test-error-checker "and(true, 1)")
(test-error-checker "or(0, false)")

;; ----------------------------------------
;; 3.5 if-exp
;; ----------------------------------------
(display "--- 3.5 if-exp ---")
(newline)

(test-checker "if true then 1 else 2" 'int)
(test-checker "if false then true else false" 'bool)
(test-checker "if >(x, 0) then x else 0" 'int)

;; Errores de tipo
(test-error-checker "if 1 then 2 else 3")
(test-error-checker "if true then 1 else false")

;; ----------------------------------------
;; 3.6 let-exp y var-exp
;; ----------------------------------------
(display "--- 3.6 let-exp y var-exp ---")
(newline)

(test-checker "let x = 10 in x" 'int)
(test-checker "let x = true y = false in and(x, y)" 'bool)
(test-checker "var x = 5 in +(x, 1)" 'int)
(test-checker "var x = 10 y = 20 in +(x, y)" 'int)

;; ----------------------------------------
;; 3.7 set-exp
;; ----------------------------------------
(display "--- 3.7 set-exp ---")
(newline)

(test-checker "var x = 10 in set x = 20" 'void)
(test-checker "var x = 5 in begin set x = 10; x end" 'int)

;; Error: asignar a inmutable
(test-error-checker "let x = 10 in set x = 20")

;; Error: cambio de tipo
(test-error-checker "var x = 10 in set x = true")

;; ----------------------------------------
;; 3.8 freeze-exp
;; ----------------------------------------
(display "--- 3.8 freeze-exp ---")
(newline)

(test-checker "var x = 10 in freeze x" 'void)
(test-checker "var x = 5 in begin freeze x; x end" 'int)

;; Error: congelar inmutable
(test-error-checker "let x = 10 in freeze x")

;; Error: asignar después de congelar (esto lo detecta el intérprete, no el checker)

;; ----------------------------------------
;; 3.9 begin-exp
;; ----------------------------------------
(display "--- 3.9 begin-exp ---")
(newline)

(test-checker "begin 1; 2; 3 end" 'int)
(test-checker "begin true; false end" 'bool)
(test-checker "var x = 0 in begin set x = 1; x end" 'int)

;; ----------------------------------------
;; 3.10 Procedimientos
;; ----------------------------------------
(display "--- 3.10 Procedimientos ---")
(newline)

(test-checker "proc (x) +(x, 1)" '((int) -> int))
(test-checker "proc (x, y) +(x, y)" '((int int) -> int))
(test-checker "let f = proc (x) x in (f 5)" 'int)

;; Error de aridad
(test-error-checker "let f = proc (x) x in (f 1 2)")

;; ----------------------------------------
;; 3.11 Ejemplos complejos
;; ----------------------------------------
(display "--- 3.11 Ejemplos complejos ---")
(newline)

(test-checker
 "var x = 10 in begin set x = +(x, 5); x end"
 'int)

(test-checker
 "let f = proc (n) if ==(n, 0) then 1 else *(n, 2) in (f 5)"
 'int)

(test-checker
 "var contador = 0 in
  let inc = proc () begin set contador = add1(contador); contador end in
  (inc)"
 'int)

;; ============================================================
;; PARTE 4: EJEMPLOS EXIGIDOS EN EL ENUNCIADO
;; ============================================================

(display "")
(newline)
(display "========================================")
(newline)
(display "PARTE 4: EJEMPLOS EXIGIDOS")
(newline)
(display "========================================")
(newline)
(newline)

;; ----------------------------------------
;; Ejemplo 4.1: var básico
;; ----------------------------------------
(display "--- Ejemplo 4.1: var básico ---")
(newline)

(test-interprete "var x = 10 in x" 10)
(test-checker "var x = 10 in x" 'int)

;; ----------------------------------------
;; Ejemplo 4.2: set simple
;; ----------------------------------------
(display "--- Ejemplo 4.2: set simple ---")
(newline)

(test-interprete "var x = 10 in begin set x = 20; x end" 20)
(test-checker "var x = 10 in begin set x = 20; x end" 'int)

;; ----------------------------------------
;; Ejemplo 4.3: contador con clausura
;; ----------------------------------------
(display "--- Ejemplo 4.3: contador ---")
(newline)

(test-interprete
 "var x = 0 in let f = proc () begin set x = add1(x); x end in begin (f); (f); (f) end"
 3)

(test-checker
 "var x = 0 in let f = proc () begin set x = add1(x); x end in (f)"
 'int)

;; ----------------------------------------
;; Ejemplo 4.4: freeze
;; ----------------------------------------
(display "--- Ejemplo 4.4: freeze ---")
(newline)

(test-interprete "var x = 10 in begin freeze x; x end" 10)
(test-checker "var x = 10 in begin freeze x; x end" 'int)

;; ----------------------------------------
;; Ejemplo 4.5: Error de asignación a let
;; ----------------------------------------
(display "--- Ejemplo 4.5: Error let ---")
(newline)

(test-error-interprete "let x = 10 in set x = 20")
(test-error-checker "let x = 10 in set x = 20")

;; ----------------------------------------
;; Ejemplo 4.6: Error de cambio de tipo
;; ----------------------------------------
(display "--- Ejemplo 4.6: Error cambio tipo ---")
(newline)

(test-error-checker "var x = 10 in set x = true")

;; ============================================================
;; RESUMEN DE PRUEBAS
;; ============================================================

(display "")
(newline)
(display "========================================")
(newline)
(display "PRUEBAS COMPLETADAS")
(newline)
(display "========================================")
(newline)
(display "Revise los resultados arriba.")
(newline)
(display "Los ✓ indican pruebas exitosas.")
(newline)
(display "Los ✗ indican fallos que deben corregirse.")
(newline)
