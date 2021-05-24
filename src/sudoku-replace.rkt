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