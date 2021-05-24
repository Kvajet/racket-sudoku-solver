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