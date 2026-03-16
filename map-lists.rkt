#lang eopl


(define empty-lint
  (lambda ()
    (list 'empty-lint)))


(define non-empty-lint
  (lambda (num lst)
    (list 'non-empty-lint num lst)))


(define empty-lint?
  (lambda (lst)
    (equal? (car lst) 'empty-lint)))


(define non-empty-lint?
  (lambda (lst)
    (equal? (car lst) 'non-empty-lint)))


(define non-empty-lint->num
  (lambda (lst)
    (cadr lst)))


(define non-empty-lint->lst
  (lambda (lst)
    (caddr lst)))


(define empty-lsym
  (lambda ()
    (list 'empty-lsym)))


(define non-empty-lsym
  (lambda (sym lst)
    (list 'non-empty-lsym sym lst)))


(define empty-lsym?
  (lambda (lst)
    (equal? (car lst) 'empty-lsym)))


(define non-empty-lsym?
  (lambda (lst)
    (equal? (car lst) 'non-empty-lsym)))


(define non-empty-lsym->sym
  (lambda (lst)
    (cadr lst)))


(define non-empty-lsym->lst
  (lambda (lst)
    (caddr lst)))


;TAD

(define key-int
  (lambda (i)
    (list 'key-int i)))


(define key-sym
  (lambda (s)
    (list 'key-sym s)))


(define key-int?
  (lambda (k)
    (equal? (car k) 'key-int)))


(define key-sym?
  (lambda (k)
    (equal? (car k) 'key-sym)))


(define key-int->i
  (lambda (k)
    (cadr k)))


(define key-sym->s
  (lambda (k)
    (cadr k)))

;TAD VALUES

(define val-int
  (lambda (i)
    (list 'val-int i)))


(define val-sym
  (lambda (s)
    (list 'val-sym s)))


(define val-lint
  (lambda (lst)
    (list 'val-lint lst)))


(define val-lsym
  (lambda (lst)
    (list 'val-lsym lst)))


(define val-int?
  (lambda (v)
    (equal? (car v) 'val-int)))


(define val-sym?
  (lambda (v)
    (equal? (car v) 'val-sym)))


(define val-lint?
  (lambda (v)
    (equal? (car v) 'val-lint)))


(define val-lsym?
  (lambda (v)
    (equal? (car v) 'val-lsym)))


(define val-int->i
  (lambda (v)
    (cadr v)))


(define val-sym->s
  (lambda (v)
    (cadr v)))


(define val-lint->lst
  (lambda (v)
    (cadr v)))

(define val-lsym->lst
  (lambda (v)
    (cadr v)))

;TAD ENTRY

(define entry
  (lambda (k v)
    (list 'entry k v)))


(define entry?
  (lambda (e)
    (equal? (car e) 'entry)))


(define entry->k
  (lambda (e)
    (cadr e)))


(define entry->v
  (lambda (e)
    (caddr e)))

;TAD MAP

(define empty-map
  (lambda ()
    (list 'empty-map)))


(define non-empty-map
  (lambda (ent mp)
    (list 'non-empty-map ent mp)))


(define empty-map?
  (lambda (mp)
    (equal? (car mp) 'empty-map)))


(define non-empty-map?
  (lambda (mp)
    (equal? (car mp) 'non-empty-map)))


(define non-empty-map->ent
  (lambda (mp)
    (cadr mp)))


(define non-empty-map->mp
  (lambda (mp)
    (caddr mp)))

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
      [else (eopl:error "lint->list: lista inválida ~s" lst)])))


(define lsym->list
  (lambda (lst)
    (cond
      [(empty-lsym? lst) '()]
      [(non-empty-lsym? lst)
       (cons (non-empty-lsym->sym lst)
             (lsym->list (non-empty-lsym->lst lst)))]
      [else (eopl:error "lsym->list: lista inválida ~s" lst)])))

;MAP MAIN FUNCTIONS
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
              [else (eopl:error "insert: map inválido ~s" mp)]))))
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
      [else (eopl:error "search: map inválido ~s" mp)])))


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
              [else (eopl:error "delete: map inválido ~s" mp)]))))
      (delete-aux mp k '() #f))))

;PROVIDER TEST

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
