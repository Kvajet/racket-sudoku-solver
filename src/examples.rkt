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
