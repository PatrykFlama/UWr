#lang racket

; TODO uncomment
; (provide (struct-out column-info)
;          (struct-out table)
;          (struct-out and-f)
;          (struct-out or-f)
;          (struct-out not-f)
;          (struct-out eq-f)
;          (struct-out eq2-f)
;          (struct-out lt-f)
;          table-insert
;          table-project
;          table-sort
;          table-select
;          table-rename
;          table-cross-join
;          table-natural-join)

#|
column-info:
    name: symbol
    type: 'number/'string/'symbol/'boolean
|#
(define-struct column-info (name type) #:transparent)

#|
table:
    schema: list of column-info
    rows: list of list of data with according types and length
|#
(define-struct table (schema rows) #:transparent)

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Poland"  517 #t)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define (empty-table columns) (table columns '()))

; Insertion to table
(define (table-insert row tab)
    (define (_table-insert-check row types)
        (cond
            [(and (null? row) (null? types)) #t]
            [(and (number? (car row)) (equal? 'number (column-info-type (car types))))
                (_table-insert-check (cdr row) (cdr types))]
            [(and (string? (car row)) (equal? 'string (column-info-type (car types))))
                (_table-insert-check (cdr row) (cdr types))]
            [(and (symbol? (car row)) (equal? 'symbol (column-info-type (car types))))
                (_table-insert-check (cdr row) (cdr types))]
            [(and (boolean? (car row)) (equal? 'boolean (column-info-type (car types))))
                (_table-insert-check (cdr row) (cdr types))]
            [else #f]))
    (if (_table-insert-check row (table-schema tab))
        (table
            (table-schema tab) 
            (cons row (table-rows tab)))
        (error "table-insert: incorrect row type or length!")))

; Projection of table
(define (table-project cols tab)
    )


#|
; Sorting the table
(define (table-sort cols tab)
  ;; TODO uzupełnij
  )

; Selection of the table
(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (table-select form tab)
  ;; TODO uzupełnij
  )

; Changing name in the table
(define (table-rename col ncol tab)
  ;; TODO uzupełnij
  )

; Tables cross join
(define (table-cross-join tab1 tab2)
  ;; TODO uzupełnij
  )

; Tables join
(define (table-natural-join tab1 tab2)
  ;; TODO uzupełnij
  )

|#