#lang racket
(require rackunit)

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

;! ----- Setup -----
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

;! ----- Insertion to table -----
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

;* ----- Aux function -----
(define (get_column_number column tab)      ; number of column in the table schema
    (define (_gcn columns cnt)
        (cond
            [(empty? columns)
                (error 'table-project "column does not exist!")]
            [(equal? column (column-info-name (first columns)))
                cnt]
            [else 
                (_gcn (rest columns) (+ cnt 1))]))
    (_gcn (table-schema tab) 0))

(define (get_cols_nums columns tab)
    (if (empty? columns)
        '()
        (cons 
            (get_column_number (first columns) tab)
            (get_cols_nums (rest columns) tab))))

;! ----- Projection of table -----
(define (table-project cols tab)
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
        (let ((column_numbers (get_cols_nums cols tab)))
            (table
                (get_column_info column_numbers)
                (get_rows (table-rows tab) column_numbers)))))

;! ----- Changing name of column in the table -----
(define (table-rename col ncol tab)
    (define (get_cols cols)
        (cond 
            [(empty? cols) (error 'table-rename "column not found")]
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

;! ----- Sorting the table in ascending order by rows with cols cell priority -----
(define (table-sort cols tab)
    (define cols_nums (get_cols_nums cols tab)) ;* cols table represented with ordering numbers by tab

    (define (isolate row)      ; return only cells used for sorting, with priority in order
        (define (_isolate row col_nums)
            (if (empty? col_nums)
                '()
                (cons
                    (list-ref row (first col_nums))
                    (_isolate row (rest col_nums)))))
        (_isolate row cols_nums))

    (define (less_than? r1 r2) ; compares 2 rows from table
        (define (_compare row1 row2) ; compare 2 rows, with only cells to compare in order 
            (define (_cell_compare cell1 cell2)  ; compares 2 cells
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

            (if (empty? row1) #f
                (let ((res (_cell_compare (first row1) (first row2))))
                (cond
                    [(= res 0) (_compare (rest row1) (rest row2))]
                    [(= res 1) #t]
                    [else #f]))))
        (_compare (isolate r1) (isolate r2)))

    (table
        (table-schema tab)
        (sort 
            (table-rows tab)
            less_than?)))

;! ----- Formulas -----
(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))     ; value of cell from column name equal? val
(define-struct eq2-f (name name2))  ; values of cells from columns name name2 equal?
(define-struct lt-f (name val))     ; 

;! ----- Selection of rows from the table, that fulfill given formula -----
(define (table-select form tab)
    (define (less_than? v1 v2 type)
        (cond
            [(equal? 'number type)
                (< v1 v2)]
            [(equal? 'string type)
                (string<? v1 v2)]
            [(equal? 'symbol type)
                (symbol<? v1 v2)]
            [(equal? 'boolean type)
                (if v1 #f v2)]))
    
    (define (evaluate f row)
        (cond
            [(and-f? f) (and
                (evaluate (and-f-l f) row)
                (evaluate (and-f-r f) row))]
            [(or-f? f) (or
                (evaluate (or-f-l f) row)
                (evaluate (or-f-r f) row))]
            [(not-f? f) (not
                (evaluate (not-f-e f) row))]
            [(eq-f? f) 
                (let ((column (get_column_number
                    (eq-f-name f)
                    tab)))
                (equal?
                    (list-ref row column)
                    (eq-f-val f)))]
            [(eq2-f? f) (let (
                (col1 (get_column_number (eq2-f-name  f) tab))
                (col2 (get_column_number (eq2-f-name2 f) tab)))
                (equal?
                    (list-ref row col1)
                    (list-ref row col2)))]
            [(lt-f? f)
                (let ((col (get_column_number (lt-f-name f) tab)))
                (less_than?
                    (list-ref row col)
                    (lt-f-val f)
                    (column-info-type
                        (list-ref (table-schema tab) col))))]))
    
    (define (get_rows rows)
        (cond  
            [(empty? rows) '()]
            [(evaluate form (first rows))
                (cons
                    (first rows)
                    (get_rows (rest rows)))]
            [else (get_rows (rest rows))]))
            
    (table 
        (table-schema tab)
        (get_rows (table-rows tab))))

;! ----- Tables cross join -----
(define (table-cross-join tab1 tab2)
    (define (get_rows rows1 rows2)
        (cond
            [(empty? rows1) '()]
            [(empty? rows2)
                (get_rows 
                    (rest rows1)
                    (table-rows tab2))]
            [else (cons
                (append     ; that will be sufficiently quick
                    (first rows1)
                    (first rows2))
                (get_rows rows1 (rest rows2)))]))
    (table
        (append 
            (table-schema tab1)
            (table-schema tab2))
        (get_rows
            (table-rows tab1)
            (table-rows tab2))))

;! ----- Tables join -----
(define (table-natural-join tab1 tab2)
    )

;* ----- tests -----
(check-equal?
    (table-rows (table-project '(size city) (table-rename 'area 'size 
        (table-sort '(capital area) (table-insert (list "Rzeszow" "Poland" 129 #f) cities)))))
    '((105 "Paris")
      (517 "Warsaw")
      (892 "Berlin")
      (50 "Rennes")
      (129 "Rzeszow")
      (262 "Poznań")
      (293 "Wrocław")
      (310 "Munich")))
(check-equal?
    (table-rows (table-select (and-f (eq-f 'capital #t) (not-f (lt-f 'area 300))) cities))
    '(("Warsaw" "Poland" 517 #t) ("Berlin" "Germany" 892 #t)))