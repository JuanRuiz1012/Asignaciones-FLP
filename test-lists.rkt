#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "map-lists.rkt")

(define test-lists
  (test-suite
   "Tests - Listas"
   (test-case "empty-map vacío"
     (check-true (empty-map? (empty-map))))
   (test-case "insert clave entera"
     (check-equal? (search (insert (empty-map) 5 'hello) 5) 'hello))
   (test-case "insert clave simbólica"
     (check-equal? (search (insert (empty-map) 'x '(1 2 3)) 'x) '(1 2 3)))
   (test-case "insert múltiples entradas"
     (let* ((m (insert (insert (insert (insert (empty-map) 5 'hello) 'x '(1 2 3)) 10 '(a b c)) 'y 42)))
       (check-equal? (search m 5) 'hello)
       (check-equal? (search m 'x) '(1 2 3))
       (check-equal? (search m 10) '(a b c))
       (check-equal? (search m 'y) 42)))
   (test-case "insert actualiza clave existente"
     (check-equal? (search (insert (insert (empty-map) 5 'hello) 5 'world) 5) 'world))
   (test-case "search error clave inexistente"
     (check-exn exn:fail? (lambda () (search (insert (empty-map) 5 'hello) 99))))
   (test-case "search error map vacío"
     (check-exn exn:fail? (lambda () (search (empty-map) 5))))
   (test-case "delete elimina entrada"
     (let* ((m2 (delete (insert (insert (empty-map) 5 'hello) 'x '(1 2 3)) 5)))
       (check-exn exn:fail? (lambda () (search m2 5)))
       (check-equal? (search m2 'x) '(1 2 3))))
   (test-case "delete error clave inexistente"
     (check-exn exn:fail? (lambda () (delete (insert (empty-map) 5 'hello) 99))))
   (test-case "delete deja map vacío"
     (check-true (empty-map? (delete (insert (empty-map) 5 'hello) 5))))))

(display "=== Tests Listas ===") (newline)
(run-tests test-lists)
