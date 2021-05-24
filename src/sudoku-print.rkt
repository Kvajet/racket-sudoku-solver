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

