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
        (error 'table-insert "incorrect row type or length!")))

; Projection of table
(define (table-project cols tab)
    (define (get_column_number column)      ; number of column in the table schema
        (define (_gcn columns cnt)
            (cond
                [(empty? columns)
                    (error 'table-project "column does not exist!")]
                [(equal? column (column-info-name (first columns)))
                    cnt]
                [else 
                    (_gcn (rest columns) (+ cnt 1))]))
        (_gcn (table-schema tab) 0))

    (define (get_cols_nums columns)
        (if (empty? columns)
            '()
            (cons 
                (get_column_number (first columns))
                (get_cols_nums (rest columns)))))

    (define (get_row row col_nums)      ; creates row from given list of columns (in order)
        (if (empty? col_nums)
            '()
            (cons
                (list-ref row (first col_nums))
                (get_row row (rest col_nums)))))
    
    (define (get_rows rows col_nums)
        (if (empty? rows)
            '()
            (cons
                (get_row (first rows) col_nums)
                (get_rows (rest rows) col_nums))))

    (define (get_column_info col_nums)
        (if (empty? col_nums)
            '()
            (cons
                (list-ref (table-schema tab) (first col_nums))
                (get_column_info (rest col_nums)))))

    (if (empty? cols)
        (empty-table)
        (let ((column_numbers (get_cols_nums cols)))
            (table
                (get_column_info column_numbers)
                (get_rows (table-rows tab) column_numbers)))))

; Changing name of column in the table
(define (table-rename col ncol tab)
    (define (get_cols cols)
        (cond 
            [(empty? cols) (error 'table-rename:get_cols "column not found")]
            [(equal? (column-info-name (first cols)) col)
                (cons
                    (column-info
                        ncol
                        (column-info-type (first cols)))
                    (rest cols))]
            [else (cons
                (first cols)
                (get_cols (rest cols)))]))
    (table (get_cols (table-schema tab)) (table-rows tab)))

; Sorting the table in ascending order by rows with cols cell priority
(define (table-sort cols tab)   ;TODO fix
    (define (get_column_number column)      ; number of column in the table schema
        (define (_gcn columns cnt)
            (cond
                [(empty? columns)
                    (error 'table-project "column does not exist!")]
                [(equal? column (column-info-name (first columns)))
                    cnt]
                [else 
                    (_gcn (rest columns) (+ cnt 1))]))
        (_gcn (table-schema tab) 0))
    (define (get_cols_nums columns)
        (if (empty? columns)
            '()
            (cons 
                (get_column_number (first columns))
                (get_cols_nums (rest columns)))))
    (define cols_nums (get_cols_nums cols)) ;* cols table represented with ordering numbers by tab

    (define (isolate row)      ; return only cells used for sorting, with priority in order
        (define (_isolate row col_nums)
            (if (empty? col_nums)
                '()
                (cons
                    (list-ref row (first col_nums))
                    (_isolate row (rest col_nums)))))
        (_isolate row cols_nums))

    (define (compare r1 r2) ; compare 2 rows
        (let ((row1 (isolate r1)) (row2 (isolate r2)))
        (define (_compare cell1 cell2)  ; compares 2 cells
            (cond
                [(equal? cell1 cell2) 0]
                [(number? cell1)
                    (if (< cell1 cell2) 1 -1)]
                [(string? cell1)
                    (if (string<? cell1 cell2) 1 -1)]
                [(symbol? cell1)
                    (if (symbol<? cell1 cell2) 1 -1)]
                [(boolean? cell1)
                    (if cell1 1 -1)]))

        (if (empty? row1) 0
            (let ((res (_compare (first row1) (first row2))))
            (if (= res 0)
                (compare (rest row1) (rest row2))
                res)))))

    (table
        (table-schema tab)
        (sort 
            (table-rows tab)
            (lambda (a b) (compare a b)))))

#|
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

; Tables cross join
(define (table-cross-join tab1 tab2)
  ;; TODO uzupełnij
  )

; Tables join
(define (table-natural-join tab1 tab2)
  ;; TODO uzupełnij
  )

|#