#lang eopl

;;;DEF
(define-datatype lint lint?
  (empty-lint)
  (non-empty-lint
   (num number?)
   (lst lint?)))


(define-datatype lsym lsym?
  (empty-lsym)
  (non-empty-lsym
   (sym symbol?)
   (lst lsym?)))


(define-datatype key key?
  (key-int (i number?))
  (key-sym (s symbol?)))


(define-datatype value value?
  (val-int  (i number?))
  (val-sym  (s symbol?))
  (val-lint (lst lint?))
  (val-lsym (lst lsym?)))


(define-datatype entry entry?
  (an-entry
   (k key?)
   (v value?)))


(define-datatype map map?
  (empty-map)
  (non-empty-map
   (ent entry?)
   (mp  map?)))

;;; EXTRACTORES
(define entry->k
  (lambda (e)
    (cases entry e
      (an-entry (k v) k))))


(define entry->v
  (lambda (e)
    (cases entry e
      (an-entry (k v) v))))


(define non-empty-map->ent
  (lambda (m)
    (cases map m
      (empty-map () (eopl:error "non-empty-map->ent: map vacío"))
      (non-empty-map (ent mp) ent))))


(define non-empty-map->mp
  (lambda (m)
    (cases map m
      (empty-map () (eopl:error "non-empty-map->mp: map vacío"))
      (non-empty-map (ent mp) mp))))


(define key-int->i
  (lambda (k)
    (cases key k
      (key-int (i) i)
      (key-sym (s) (eopl:error "key-int->i: clave no es entera")))))


(define key-sym->s
  (lambda (k)
    (cases key k
      (key-int (i) (eopl:error "key-sym->s: clave no es simbólica"))
      (key-sym (s) s))))


(define val-int->i
  (lambda (v)
    (cases value v
      (val-int (i) i)
      (else (eopl:error "val-int->i: valor no es entero")))))


(define val-sym->s
  (lambda (v)
    (cases value v
      (val-sym (s) s)
      (else (eopl:error "val-sym->s: valor no es simbólico")))))

(define val-lint->lst
  (lambda (v)
    (cases value v
      (val-lint (lst) lst)
      (else (eopl:error "val-lint->lst: valor no es lista de enteros")))))

(define val-lsym->lst
  (lambda (v)
    (cases value v
      (val-lsym (lst) lst)
      (else (eopl:error "val-lsym->lst: valor no es lista de símbolos")))))

(define non-empty-lint->num
  (lambda (lst)
    (cases lint lst
      (empty-lint () (eopl:error "non-empty-lint->num: lista vacía"))
      (non-empty-lint (num rest) num))))

(define non-empty-lint->lst
  (lambda (lst)
    (cases lint lst
      (empty-lint () (eopl:error "non-empty-lint->lst: lista vacía"))
      (non-empty-lint (num rest) rest))))

(define non-empty-lsym->sym
  (lambda (lst)
    (cases lsym lst
      (empty-lsym () (eopl:error "non-empty-lsym->sym: lista vacía"))
      (non-empty-lsym (sym rest) sym))))

(define non-empty-lsym->lst
  (lambda (lst)
    (cases lsym lst
      (empty-lsym () (eopl:error "non-empty-lsym->lst: lista vacía"))
      (non-empty-lsym (sym rest) rest))))

;;AUX FUNCTIONS
(define unwrap-key
  (lambda (k)
    (cases key k
      (key-int (i) i)
      (key-sym (s) s))))

(define build-key
  (lambda (k)
    (if (number? k)
        (key-int k)
        (key-sym k))))

(define build-value
  (lambda (v)
    (cond
      [(number? v) (val-int v)]
      [(symbol? v) (val-sym v)]
      [(and (list? v) (not (null? v)) (number? (car v)))
       (val-lint (list->lint v))]
      [(and (list? v) (not (null? v)) (symbol? (car v)))
       (val-lsym (list->lsym v))]
      [(null? v) (val-lint (empty-lint))]
      [else (eopl:error "build-value: tipo no soportado ~s" v)])))

(define list->lint
  (lambda (lst)
    (if (null? lst)
        (empty-lint)
        (non-empty-lint (car lst) (list->lint (cdr lst))))))

(define list->lsym
  (lambda (lst)
    (if (null? lst)
        (empty-lsym)
        (non-empty-lsym (car lst) (list->lsym (cdr lst))))))

(define unwrap-value
  (lambda (v)
    (cases value v
      (val-int  (i)   i)
      (val-sym  (s)   s)
      (val-lint (lst) (lint->list lst))
      (val-lsym (lst) (lsym->list lst)))))

(define lint->list
  (lambda (lst)
    (cases lint lst
      (empty-lint () '())
      (non-empty-lint (num rest)
                      (cons num (lint->list rest))))))

(define lsym->list
  (lambda (lst)
    (cases lsym lst
      (empty-lsym () '())
      (non-empty-lsym (sym rest)
                      (cons sym (lsym->list rest))))))

(define keys-equal?
  (lambda (k1 k2)
    (equal? k1 k2)))

(define rebuild-map
  (lambda (mp acc)
    (if (null? acc)
        mp
        (rebuild-map (non-empty-map (car acc) mp) (cdr acc)))))

;;; MAIN FUNCTIONS
(define insert
  (lambda (mp k v)
    (letrec
        ((insert-aux
          (lambda (mp k v acc)
            (cases map mp
              (empty-map ()
                         (rebuild-map
                          (non-empty-map (an-entry (build-key k) (build-value v))
                                         (empty-map))
                          acc))
              (non-empty-map (ent rest)
                             (let ((ent-key (unwrap-key (entry->k ent))))
                               (if (keys-equal? ent-key k)
                                   (rebuild-map
                                    (non-empty-map (an-entry (build-key k) (build-value v))
                                                   rest)
                                    acc)
                                   (insert-aux rest k v (cons ent acc)))))))))
      (insert-aux mp k v '()))))


(define search
  (lambda (mp k)
    (cases map mp
      (empty-map ()
                 (eopl:error "search: clave no encontrada ~s" k))
      (non-empty-map (ent rest)
                     (let ((ent-key (unwrap-key (entry->k ent))))
                       (if (keys-equal? ent-key k)
                           (unwrap-value (entry->v ent))
                           (search rest k)))))))

(define delete
  (lambda (mp k)
    (letrec
        ((delete-aux
          (lambda (mp k acc found?)
            (cases map mp
              (empty-map ()
                         (if found?
                             (rebuild-map (empty-map) acc)
                             (eopl:error "delete: clave no encontrada ~s" k)))
              (non-empty-map (ent rest)
                             (let ((ent-key (unwrap-key (entry->k ent))))
                               (if (keys-equal? ent-key k)
                                   (delete-aux rest k acc #t)
                                   (delete-aux rest k (cons ent acc) found?))))))))
      (delete-aux mp k '() #f))))

;;;PROVIDER

(provide empty-map non-empty-map map?
         an-entry entry? entry->k entry->v
         non-empty-map->ent non-empty-map->mp
         key-int key-sym key? key-int->i key-sym->s
         val-int val-sym val-lint val-lsym value?
         val-int->i val-sym->s val-lint->lst val-lsym->lst
         empty-lint non-empty-lint lint?
         non-empty-lint->num non-empty-lint->lst
         empty-lsym non-empty-lsym lsym?
         non-empty-lsym->sym non-empty-lsym->lst
         insert search delete)
