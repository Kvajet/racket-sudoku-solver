#lang racket
(require racket/include)

; prints sudoku to console
; usable command by USER is sudoku-print

; ==========================================================================================
; params: r     row of sudoku to be print
(define (print-row-inner r)
  (cond
    [(not (empty? r)) (printf "~a" (car r))
                      (cond
                        [(not (empty? (cdr r))) (printf " ")]
                      )
                      (print-row-inner (cdr r))]
  )
)
; ==========================================================================================
; params: r     row of sudoku to be print
;         nth   row index to print for brackets and formatting stuff
(define (print-row r nth)
  (cond
    [(= nth 0) (printf "(")]
    [else (printf " ")]
  )
  (printf "(")
  (print-row-inner r)
  (printf ")")
)
; ==========================================================================================
; params: s     sudoku
;         nth   row index print for brackets and formatting stuff
(define (sudoku-print-inner s nth)
  (cond
    [(not (empty? s)) (print-row (car s) nth)
                      (cond
                        [(empty? (cdr s)) (printf ")")]
                      )
                      (printf "~n")
                      (sudoku-print-inner (cdr s) (+ nth 1))]
  )
)
; ==========================================================================================
; params: s     sudoku
(define (sudoku-print s)
  (sudoku-print-inner s 0)
)

; ==========================================================================================
; params: r     row
; return: row with replaced 'x' with 0
(define (replace-row r)
  (cond
    [(empty? r) null]
    [(not (list? r)) void]
    [(number? (car r)) (cons (car r) (replace-row (cdr r)))]
    [else (cons 0 (replace-row (cdr r)))]
  )
)
; ==========================================================================================
; params: s     sudoku
; return: sudoku with replaced 'x' with 0
(define (sudoku-replace s)
  (cond
    [(null? s) null]
    [else (cons (replace-row (car s)) (sudoku-replace (cdr s)))]
  )
)

; ==========================================================================================
; description: checks if given value is in valid range (1-9 for 9x9 sudoku) or 0 as unkown value
;
; params: value value to be checked
;         range size of sudoku side
; return: #T/#F
(define (in-range? value range)
  (or (and (>= range 1) (<= value range)) (= value 0))
)
; ==========================================================================================
; description: checks if it contains only unique numbers in range (with possible 0 duplicates as 'x')
;
; params: s     sudoku row/column
;         range size of sudoku side
; return: #T/#F
(define (unique? s range)
  (cond
    [(> (length s) 1)
     (cond
       [(and (= (car s) 0) (= (cadr s) 0)) (unique? (cdr s) range)]
       [(= (car s) (cadr s)) #F]
       [(not (in-range? (car s) range)) #F]
       [(not (in-range? (cadr s) range)) #F] ; maybe to refactor - checking 2 times n-1 items, could be checked by one, then the last one
       [else (unique? (cdr s) range)]
     )
    ]
  )
)
; ==========================================================================================
; description: sorts sudoku row/column and checks if it contains only unique numbers in range (with possible 0 duplicates as 'x')
;
; params: s     sudoku row
; return: #T/#F
(define (sort-unique? s range)
  (unique? (sort s <) range)
)
; ==========================================================================================
; params: s     sudoku row
;         range size of sudoku side
; return: #T/#F
(define (valid-row? s range)
  (cond
    [(not (= (length s) range)) #F]
    [else (sort-unique? s range)]
  )
)
; ==========================================================================================
; params: s     sudoku
;         range size of sudoku side
; return: #T/#F
(define (valid-rows? s range)
  (cond
    [(empty? s) #T]
    [(not (valid-row? (car s) range)) #F]
    [else (and #T  (valid-rows? (cdr s) range))]
  )
)
; ==========================================================================================
; params: s     sudoku
; return: first column as list
(define (get-firsts s)
  (cond
    [(empty? s) null]
    [else (cons (caar s) (get-firsts (cdr s)))]
  )
)
; ==========================================================================================
; description: removes first column
;
; params: s     sudoku
; return: sudoku without current first column
(define (remove-firsts s)
  (cond
    [(empty? s) null]
    [else (cons (cdr (car s)) (remove-firsts (cdr s)))]
  )
)
; ==========================================================================================
; params: s     sudoku
;         range size of sudoku side
; return: #T/#F
(define (valid-cols? s range)
  (cond
    [(empty? s) #T]
    [(empty? (car s)) #T]
    [(not (valid-row? (get-firsts s) range)) #F]
    [else (and #T (valid-cols? (remove-firsts s) range))]
))
; ==========================================================================================
; params: s     sudoku
;         y     number of rows to be skipped
;         range size of sudoku side
; return: nth row as simple list
(define (get-nth-row s y range)
  (cond
    [(> y 0) (get-nth-row (cdr s) (- y 1) range)]
    [else (car s)]
  )
)
; ==========================================================================================
; params: s     sudoku
;         x     number of cols to be skipped
;         range size of sudoku side
; return: nth column as simple list
(define (get-nth-col s x range)
  (cond
    [(> x 0) (get-nth-col (remove-firsts s) (- x 1) range)]
    [else (get-firsts s)]
  )
)
; ==========================================================================================
; description: sudoku validating function for specified position
;              (more effective than sudoku-valid? but checks only [x][y] line, row and partition
;
; params: s     sudoku
;         x     x axis position
;         y     y axis position
;         range size of sudoku side
(define (sudoku-valid-smaller s x y range)
  (cond
    [(not (sort-unique? (get-nth-row s y range) range)) #F]
    [(not (sort-unique? (get-nth-col s x range) range)) #F]
    [(not (valid-partition-by-col s x y range)) #F]
    [else #T]
  )
)

(define (2x2) '((x 2 4 x)
                (1 x x 3)
                (4 x x 2)
                (x 1 3 x)))

(define (3x3) '((7 x 2 x 8 1 x x x)
                (5 6 x 7 9 x x x x)
                (x x x x x 4 x x x)
                (x 1 7 x x x x x 3)
                (x 9 5 x 4 x 8 2 x)
                (2 x x x x x 6 9 x)
                (x x x 2 x x x x x)
                (x x x x 3 9 x 1 2)
                (x x x 8 1 x 3 x 6)))

(define (3x3_easy) '((3 x 2 x 8 x 1 x 5)
                     (x x 7 2 x x x 6 x)
                     (5 x 8 9 x x x 4 7)
                     (x 8 x 4 x x 3 x 2)
                     (x 3 x 1 6 x x 5 8)
                     (x 1 x 5 x x 6 7 x)
                     (x 2 x 3 4 5 x x 1)
                     (x x x x 2 6 x x 9)
                     (x x x x 9 x 5 2 6)))

(define (3x3_empty) '((x x x x x x x x x)
                      (x x x x x x x x x)
                      (x x x x x x x x x)
                      (x x x x x x x x x)
                      (x x x x x x x x x)
                      (x x x x x x x x x)
                      (x x x x x x x x x)
                      (x x x x x x x x x)
                      (x x x x x x x x x)))

(define (3x3_extreme) '((2 x x x 9 x x x 6)
                        (x 6 x 4 x 5 x 9 x)
                        (x x 1 x x x 5 x x)
                        (x 4 x x 5 x x 2 x)
                        (3 x x 6 x 8 x x 5)
                        (x 1 x x 2 x x 3 x)
                        (x x 6 x x x 8 x x)
                        (x 3 x 5 x 6 x 7 x)
                        (9 x x x 3 x x x 4)))

(define (3x3_solved) '((8 2 7 1 5 4 3 9 6)
                       (9 6 5 3 2 7 1 4 8)
                       (3 4 1 6 8 9 7 5 2)
                       (5 9 3 4 6 8 2 7 1)
                       (4 7 2 5 1 3 6 8 9)
                       (6 1 8 9 7 2 4 3 5)
                       (7 8 6 2 3 5 9 1 4)
                       (1 5 4 7 9 6 8 2 3)
                       (2 3 9 8 4 1 5 6 7)))

(define (3x3_invalid_range) '((7 x 2 x 8 1 x x x)
                              (5 6 x 7 9 x x x x)
                              (x x x x x 4 x 15 x)
                              (x 1 7 x x x x x 3)
                              (x 9 5 x 4 x 8 2 x)
                              (2 x x x x x 6 9 x)
                              (x x x 2 x x x x x)
                              (x x x x 3 9 x 1 2)
                              (x x x 8 1 x 3 x 6)))

(define (3x3_invalid_rows) '((7 x 2 x 8 1 x 1 x)
                             (5 6 x 7 9 x x x x)
                             (x x x x x 4 x x x)
                             (x 1 7 x x x x x 3)
                             (x 9 5 x 4 x 8 2 x)
                             (2 x x x x x 6 9 x)
                             (x x x 2 x x x x x)
                             (x x x x 3 9 x 1 2)
                             (x x x 8 1 x 3 x 6)))

(define (4x4) '(( x x  2  x x  8  x 15  x  x  x  x  1 11 10 12)
                ( x x  x  7 2  x  x  x 13 15  x  x  x  x  x  x)
                ( x x  x  0 x  9 14  x  1 10  x  x  5  x  x  x)
                ( x x  3 14 7  4  x  x  5  6  8 11  x  x  x  x)
                (15 9  0  x 4  x  x  x  x  x 11  x  x  1  x  3)
                ( x 6  x  x x 12  x  x  x  8  0  x  x  x 14  x)
                ( x 5  x  x x  x  x  x  4  x 14  x  8  x 13  2)
                ( x x 10  2 x  x  x  x  x  9  x 15  x  4  x  x)
                ( x x  x 10 x  x  3 14  x  x  x  x  x  2 15  x)
                (13 x 11  x x  x  x  9  x  7  x  6  x  x  5  1)
                ( x 2  x  3 x  x  x  x  x  x  x 13  7  9 12  x)
                ( x x  x  x x  x  x  x  0 12  9  x  x 14  x 13)
                ( 1 x  7  x 9  x  x 13  x  4  6  x 11 15  x  x)
                ( x x  4  x x  3  x  x  x  x  x  x  9  x  7  x)
                ( x x  x  6 x  x  x  7  3 11  x  x 14  x  x  x)
                ( 0 x  x  x x  x  5  x  x  x 13  x  x  3  8  6)))

(define (4x4_from) '(( x  6  x  x  x  x  x  8 11  x  x 15 14  x  x 16)
                     (15 11  x  x  x 16 14  x  x  x 12  x  x  6  x  x)
                     (13  x  9 12  x  x  x  x  3 16 14  x 15 11 10  x)
                     ( 2  x 16  x 11  x 15 10  1  x  x  x  x  x  x  x)
                     ( x 15 11 10  x  x 16  2 13  8  9 12  x  x  x  x)
                     (12 13  x  x  4  1  5  6  2  3  x  x  x  x 11 10)
                     ( 5  x  6  1 12  x  9  x 15 11 10  7 16  x  x  3)
                     ( x  2  x  x  x 10  x 11  6  x  5  x  x 13  x  9)
                     (10  7 15 11 16  x  x  x 12 13  x  x  x  x  x  6)
                     ( 9  x  x  x  x  x  1  x  x  2  x 16 10  x  x 11)
                     ( 1  x  4  6  9 13  x  x  7  x 11  x  3 16  x  x)
                     (16 14  x  x  7  x 10 15  4  6  1  x  x  x 13  8)
                     (11 10  x 15  x  x  x 16  9 12 13  x  x  1  5  4)
                     ( x  x 12  x  1  4  6  x 16  x  x  x 11 10  x  x)
                     ( x  x  5  x  8 12 13  x 10  x  x 11  2  x  x 14)
                     ( 3 16  x  x 10  x  x  7  x  x  6  x  x  x 12  x)))

; ==========================================================================================
; params: s     sudoku
;         ts    number of elements in row to skip
;         tc    number of elements in row to copy
; return: part of row in partition
(define (get-partition-cols-inner s ts tc)
  (cond
    [(> ts 0) (get-partition-cols-inner (cdr s) (- ts 1) tc)]
    [(> tc 0) (cons (car s) (get-partition-cols-inner (cdr s) ts (- tc 1)))]
    [else null]
  )
)
; ==========================================================================================
; params: s     sudoku
;         y     y axis partition index
; return: part of row in partition
(define (get-partition-cols s y range)
  (get-partition-cols-inner s (* (floor (/ y (sqrt range))) (sqrt range)) (sqrt range)) ; s - sudoku, 2rd - toskip, 3th - tocopy
)
; ==========================================================================================
; params: s     sudoku
;         ts    number of rows to skip
;         tc    number of rows to copy
; return: list of lists with parts of rows of partition
(define (get-partition-rows-inner s ts tc)
  (cond
    [(> ts 0) (get-partition-rows-inner (remove-firsts s) (- ts 1) tc)]
    [(> tc 0) (cons (get-firsts s) (get-partition-rows-inner (remove-firsts s) ts (- tc 1)))]
    [else null]
  )
)
; ==========================================================================================
; params: s     sudoku
;         x     x axis partition index
;         range size of sudoku side
; return: list of lists with parts of rows of partition
(define (get-partition-rows s x range)
  (get-partition-rows-inner s (* (floor (/ x (sqrt range))) (sqrt range)) (sqrt range))
)
; ==========================================================================================
; params: s     sudoku
;         x     iteration constant of x axis
;         y     iteration constant of y axis
;         range size of sudoku side
; return: list of [x][y] partition values
(define (get-partition s x y range)
  (apply append (get-partition-rows (get-partition-cols s y range) x range))
)
; ==========================================================================================
; params: s     sudoku
;         x     iteration constant of x axis
;         y     iteration constant of y axis, incremented by (sqrt(range - 1) to jump over all partitions
;         range size of sudoku side
; return: #T/#F
(define (valid-partition-by-col s x y range)
  (cond
    [(>= y range) #T]
    [else (not (= y (sqrt range))) (and
                                         (sort-unique? (get-partition s x y range) range)
                                         (valid-partition-by-col s x (+ y
                                                                        (cond
                                                                          [(= range 1) 1]
                                                                          [else (- (sqrt range) 1)]
                                                                        )
                                                                     ) range
                                         )
                                   )
    ]
  )
)
; ==========================================================================================
; params: s     sudoku
;         x     iteration constant of x axis, incremented by (sqrt(range) - 1) to jump over all partitions
;         range size of sudoku side
; return: #T/#F
(define (valid-partition-by-row s x range)
  (cond
    [(>= x range) #T]
    [else
         (and (valid-partition-by-row s (+ x
                                           (cond
                                             [(= range 1) 1]
                                             [else (- (sqrt range) 1)]
                                           )
                                        ) range
              )
              (valid-partition-by-col s x 0 range)
         )
    ]
  )
)
; ==========================================================================================
; params: s     sudoku
;         range size of sudoku side
; return: #T/#F
(define (valid-partitions? s range)
  (valid-partition-by-row s 0 range)
)
; ==========================================================================================
; params: s     sudoku
;         range size of sudoku side
; return: #T/#F
(define (sudoku-valid-inner s range)
  (cond
    [(not (valid-rows? s range)) #F]
    [(not (valid-cols? s range)) #F]
    [(not (valid-partitions? s range)) #F]
    [(= range 1) (or (= (get-number s 0 0) 0) (= (get-number s 0 0) 1))]
    [else #T]
  )
)
; ==========================================================================================
; params: s     sudoku
;         range size of sudoku side
; return: #T/#F
(define (sudoku-valid? s range)
  (cond
    [(empty? s) #F]
    [(not (integer? (sqrt range))) #F]
    [else (sudoku-valid-inner s range)]
  )
)
; ==========================================================================================
; params: s     sudoku row
;         val   value to be inserted to [ts-x] position
;         ts-x  number of cols to skip
; return: sudoku row with replaced value
(define (replace-number-row s val ts-x)
  (cond
    [(> ts-x 0) (cons (car s) (replace-number-row (cdr s) val (- ts-x 1)))]
    [else (cons val (cdr s))]
  )
)
; params: s     sudoku
;         val   value to be inserted to [ts-x][ts-y] position
;         ts-x  number of cols to skip
;         ts-y  number of rows to skip
; return: sudoku with replaced value
(define (replace-number s val ts-x ts-y)
  (cond
    [(> ts-y 0) (cons (car s) (replace-number (cdr s) val ts-x (- ts-y 1)))]
    [else (cons (replace-number-row (car s) val ts-x) (cdr s))]
  )
)
; ==========================================================================================
; params: s     sudoku row
;         ts-x  number of cols to skip
; return: nth element of sudoku row
(define (get-number-row s ts-x)
  (cond
    [(> ts-x 0) (get-number-row (cdr s) (- ts-x 1))]
    [else (car s)]
  )
)
; params: s     sudoku
;         ts-x  number of cols to skip
;         ts-y  number of rows to skip
; return: element of sudoku on [ts-x][ts-y] position
(define (get-number s ts-x ts-y)
  (cond
    [(> ts-y 0) (get-number (cdr s) ts-x (- ts-y 1))]
    [else (get-number-row (car s) ts-x)]
  )
)
; ==========================================================================================
; params: s     sudoku
;         value value which may be inserted into [x][y] position in sudoku
;         x
;         y
;         range size of sudoku side
; return: #T/#F, it's used as internal value
(define (sudoku-solve-and-print s value x y range)
  (cond
    [(> value range) #F]
    [(= y range) (sudoku-print s)]
    [(= x range) (sudoku-solve-and-print s 1 0 (+ y 1) range)]
    [(not (sudoku-valid-smaller s x y range)) #F]
    [(= (get-number s x y) 0)
        (cond
          [(not (sudoku-solve-and-print (replace-number s value x y) value x y range))
                (sudoku-solve-and-print s (+ value 1) x y range)
          ]
        )
    ]
    [else (sudoku-solve-and-print s 1 (+ x 1) y range)]
  )
)
; ==========================================================================================
; params: s         sudoku
;         range     size of sudoku size
;         init-secs start time of sudoku solution
(define (sudoku-solve-inner s range init-secs)
  (cond
    [(not (sudoku-valid? s range)) (printf "This sudoku is not valid.~n")]
    [else (sudoku-solve-and-print s 1 0 0 range)]
  )
  (printf "It took: ~s seconds ~n" (- (current-seconds) init-secs))
)
; ==========================================================================================
; description: validates and solves sudoku, at first it replaces 'x' with 0 for better internal compatibility,
;              sudoku solution is printed with 'x' signs
;
; params: s     sudoku
(define (sudoku-solve s)
  (sudoku-solve-inner (sudoku-replace s) (length s) (current-seconds))
)
