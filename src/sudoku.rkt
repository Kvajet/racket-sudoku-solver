#lang racket
(require racket/include)
(include "sudoku-print.rkt")      ; commands: (sudoku-print   '((...)...))
(include "sudoku-replace.rkt")    ; commands: (sudoku-replace '((...)...))
(include "sudoku-validation.rkt") ;
(include "examples.rkt")

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
; params: s     sudoku
;         range size of sudoku size
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
