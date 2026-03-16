#lang eopl

(define empty-lint
  (lambda ()
    (lambda (s)
      (cond
        [(= s 0) 'empty-lint]
        [else (eopl:error "empty-lint: selector inválido ~s" s)]))))


(define non-empty-lint
  (lambda (num lst)
    (lambda (s)
      (cond
        [(= s 0) 'non-empty-lint]
        [(= s 1) num]
        [(= s 2) lst]
        [else (eopl:error "non-empty-lint: selector inválido ~s" s)]))))


(define empty-lint?
  (lambda (lst)
    (equal? (lst 0) 'empty-lint)))


(define non-empty-lint?
  (lambda (lst)
    (equal? (lst 0) 'non-empty-lint)))


(define non-empty-lint->num
  (lambda (lst)
    (lst 1)))


(define non-empty-lint->lst
  (lambda (lst)
    (lst 2)))

;TAD SYMS
(define empty-lsym
  (lambda ()
    (lambda (s)
      (cond
        [(= s 0) 'empty-lsym]
        [else (eopl:error "empty-lsym: selector inválido ~s" s)]))))


(define non-empty-lsym
  (lambda (sym lst)
    (lambda (s)
      (cond
        [(= s 0) 'non-empty-lsym]
        [(= s 1) sym]
        [(= s 2) lst]
        [else (eopl:error "non-empty-lsym: selector inválido ~s" s)]))))


(define empty-lsym?
  (lambda (lst)
    (equal? (lst 0) 'empty-lsym)))


(define non-empty-lsym?
  (lambda (lst)
    (equal? (lst 0) 'non-empty-lsym)))


(define non-empty-lsym->sym
  (lambda (lst)
    (lst 1)))


(define non-empty-lsym->lst
  (lambda (lst)
    (lst 2)))

;TAD MAP

(define key-int
  (lambda (i)
    (lambda (s)
      (cond
        [(= s 0) 'key-int]
        [(= s 1) i]
        [else (eopl:error "key-int: selector inválido ~s" s)]))))


(define key-sym
  (lambda (sym)
    (lambda (s)
      (cond
        [(= s 0) 'key-sym]
        [(= s 1) sym]
        [else (eopl:error "key-sym: selector inválido ~s" s)]))))


(define key-int?
  (lambda (k)
    (equal? (k 0) 'key-int)))


(define key-sym?
  (lambda (k)
    (equal? (k 0) 'key-sym)))


(define key-int->i
  (lambda (k)
    (k 1)))


(define key-sym->s
  (lambda (k)
    (k 1)))

;TAD VALUE

(define val-int
  (lambda (i)
    (lambda (s)
      (cond
        [(= s 0) 'val-int]
        [(= s 1) i]
        [else (eopl:error "val-int: selector inválido ~s" s)]))))


(define val-sym
  (lambda (sym)
    (lambda (s)
      (cond
        [(= s 0) 'val-sym]
        [(= s 1) sym]
        [else (eopl:error "val-sym: selector inválido ~s" s)]))))


(define val-lint
  (lambda (lst)
    (lambda (s)
      (cond
        [(= s 0) 'val-lint]
        [(= s 1) lst]
        [else (eopl:error "val-lint: selector inválido ~s" s)]))))


(define val-lsym
  (lambda (lst)
    (lambda (s)
      (cond
        [(= s 0) 'val-lsym]
        [(= s 1) lst]
        [else (eopl:error "val-lsym: selector inválido ~s" s)]))))


(define val-int?
  (lambda (v)
    (equal? (v 0) 'val-int)))


(define val-sym?
  (lambda (v)
    (equal? (v 0) 'val-sym)))


(define val-lint?
  (lambda (v)
    (equal? (v 0) 'val-lint)))


(define val-lsym?
  (lambda (v)
    (equal? (v 0) 'val-lsym)))


(define val-int->i
  (lambda (v)
    (v 1)))


(define val-sym->s
  (lambda (v)
    (v 1)))


(define val-lint->lst
  (lambda (v)
    (v 1)))


(define val-lsym->lst
  (lambda (v)
    (v 1)))

;TAD ENTRY

(define entry
  (lambda (k v)
    (lambda (s)
      (cond
        [(= s 0) 'entry]
        [(= s 1) k]
        [(= s 2) v]
        [else (eopl:error "entry: selector inválido ~s" s)]))))


(define entry?
  (lambda (e)
    (equal? (e 0) 'entry)))


(define entry->k
  (lambda (e)
    (e 1)))


(define entry->v
  (lambda (e)
    (e 2)))

;TAD MAPS
(define empty-map
  (lambda ()
    (lambda (s)
      (cond
        [(= s 0) 'empty-map]
        [else (eopl:error "empty-map: selector inválido ~s" s)]))))


(define non-empty-map
  (lambda (ent mp)
    (lambda (s)
      (cond
        [(= s 0) 'non-empty-map]
        [(= s 1) ent]
        [(= s 2) mp]
        [else (eopl:error "non-empty-map: selector inválido ~s" s)]))))


(define empty-map?
  (lambda (mp)
    (equal? (mp 0) 'empty-map)))


(define non-empty-map?
  (lambda (mp)
    (equal? (mp 0) 'non-empty-map)))


(define non-empty-map->ent
  (lambda (mp)
    (mp 1)))


(define non-empty-map->mp
  (lambda (mp)
    (mp 2)))

;AUX FUNCTIONS
(define keys-equal?
  (lambda (k1 k2)
    (equal? k1 k2)))


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
      [else (eopl:error "build-value: tipo de valor no soportado ~s" v)])))


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

;;;;;;;; unwrap -- extrae el valor nativo de una clave
(define unwrap-key
  (lambda (k)
    (cond
      [(key-int? k) (key-int->i k)]
      [(key-sym? k) (key-sym->s k)]
      [else (eopl:error "unwrap-key: clave inválida ~s" k)])))


(define unwrap-value
  (lambda (v)
    (cond
      [(val-int? v) (val-int->i v)]
      [(val-sym? v) (val-sym->s v)]
      [(val-lint? v) (lint->list (val-lint->lst v))]
      [(val-lsym? v) (lsym->list (val-lsym->lst v))]
      [else (eopl:error "unwrap-value: valor inválido ~s" v)])))


(define lint->list
  (lambda (lst)
    (cond
      [(empty-lint? lst) '()]
      [(non-empty-lint? lst)
       (cons (non-empty-lint->num lst)
             (lint->list (non-empty-lint->lst lst)))]
      [else (eopl:error "lint->list: lista inválida")])))


(define lsym->list
  (lambda (lst)
    (cond
      [(empty-lsym? lst) '()]
      [(non-empty-lsym? lst)
       (cons (non-empty-lsym->sym lst)
             (lsym->list (non-empty-lsym->lst lst)))]
      [else (eopl:error "lsym->list: lista inválida")])))

;;;MAIN FUNCTIONS
(define insert
  (lambda (mp k v)
    (letrec
        ((insert-aux
          (lambda (mp k v acc)
            (cond
              [(empty-map? mp)
               (rebuild-map (non-empty-map (entry (build-key k) (build-value v))
                                           (empty-map))
                            acc)]
              [(non-empty-map? mp)
               (let* ((ent (non-empty-map->ent mp))
                      (rest (non-empty-map->mp mp))
                      (ent-key (unwrap-key (entry->k ent))))
                 (if (keys-equal? ent-key k)
                     (rebuild-map (non-empty-map (entry (build-key k) (build-value v))
                                                 rest)
                                  acc)
                     (insert-aux rest k v (cons ent acc))))]
              [else (eopl:error "insert: map inválido")]))))
      (insert-aux mp k v '()))))


(define rebuild-map
  (lambda (mp acc)
    (if (null? acc)
        mp
        (rebuild-map (non-empty-map (car acc) mp) (cdr acc)))))


(define search
  (lambda (mp k)
    (cond
      [(empty-map? mp)
       (eopl:error "search: clave no encontrada ~s" k)]
      [(non-empty-map? mp)
       (let* ((ent (non-empty-map->ent mp))
              (ent-key (unwrap-key (entry->k ent))))
         (if (keys-equal? ent-key k)
             (unwrap-value (entry->v ent))
             (search (non-empty-map->mp mp) k)))]
      [else (eopl:error "search: map inválido")])))


(define delete
  (lambda (mp k)
    (letrec
        ((delete-aux
          (lambda (mp k acc found?)
            (cond
              [(empty-map? mp)
               (if found?
                   (rebuild-map (empty-map) acc)
                   (eopl:error "delete: clave no encontrada ~s" k))]
              [(non-empty-map? mp)
               (let* ((ent (non-empty-map->ent mp))
                      (rest (non-empty-map->mp mp))
                      (ent-key (unwrap-key (entry->k ent))))
                 (if (keys-equal? ent-key k)
                     (delete-aux rest k acc #t)
                     (delete-aux rest k (cons ent acc) found?)))]
              [else (eopl:error "delete: map inválido")]))))
      (delete-aux mp k '() #f))))

;;PROVIDER TEST

(provide empty-map non-empty-map empty-map? non-empty-map?
         non-empty-map->ent non-empty-map->mp
         entry entry? entry->k entry->v
         key-int key-sym key-int? key-sym? key-int->i key-sym->s
         val-int val-sym val-lint val-lsym
         val-int? val-sym? val-lint? val-lsym?
         val-int->i val-sym->s val-lint->lst val-lsym->lst
         empty-lint non-empty-lint empty-lint? non-empty-lint?
         non-empty-lint->num non-empty-lint->lst
         empty-lsym non-empty-lsym empty-lsym? non-empty-lsym?
         non-empty-lsym->sym non-empty-lsym->lst
         insert search delete)
