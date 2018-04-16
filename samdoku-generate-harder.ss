(define-library (sudoku printing)
  (import (scheme base)
    (only (scheme write) display)
    (only (gauche base) dotimes
      integer->digit print x->string)
    (only (srfi 1) count first second))
  (export display-stat print print-sudoku)
  (begin
    (define frame
      (list "
╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗
║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║ " "  │ " "  │ " "  ║
╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝
"))
    (define (display-stat s n)
      (print #"~|s|: ~|n|"))
    (define (rate-sudoku rate sudoku)
      (let ([rating (rate (list->vector sudoku))])
        (display-stat "Guesses" (first rating))
        (display-stat "Cycles" (second rating)))
      (newline))
    (define (print-sudoku solve rate grid)
      (let ([sudoku (vector->list grid)]
            [frame (map string-copy frame)]
            [solutions (solve grid 300)])
        (display-stat "Initial clues" (count positive? sudoku))
        (display-stat "Number of solutions" solutions)
        (for-each (lambda (str x)
                          (string-set! str 0 x))
          (cdr frame)
          (map (lambda (x)
                      (if (zero? x) #\space
                          (integer->digit x)))
            sudoku))
        (print
          (apply string-append frame))
        (dotimes (i 81)
          (display #"~(vector-ref grid i) "))
        (newline) (newline)
        (when (= solutions 1)
          (rate-sudoku rate sudoku))))))

(define-library (sudoku grid)
  (import (scheme base) (srfi 8)
    (only (srfi 1) break concatenate iota
      lset= take zip)
    (only (srfi 27) default-random-source
      random-source-randomize!)
    (only (gauche sequence)
      permute shuffle shuffle!)
    (only (util combinations)
      permutations) (only (util list) slices)
    (only (gauche base) compose until))
  (export initialize! receive print-sudoku
    shuffle shuffle! solution solutions sudoku)
  (begin (define solution #f)
    (define solutions #f)
    (define sudoku #f)
    (random-source-randomize!
      default-random-source)
    (define (split3 ls) (slices ls 3))
    (define (transpose ls) (apply zip ls))
    (define (cube->rows ls)
      (map concatenate
        (concatenate ls)))
    (define (triplets f ls)
      (map f (transpose (map split3 ls))))
    (define (rotations ls)
      (let ([n (length ls)])
        (do ([i 0 (+ i 1)]
               [ls (append ls ls) (cdr ls)]
               [y (list) (cons (take ls n) y)])
              ((= i n) (reverse y)))))
    (define all-3*3-latin-squares
      (cube->rows
        (map (compose permutations rotations)
          (list (list 0 1 2) (list 0 2 1)))))
   (define (randomize-third ls)
     (let f ([ls ls] [xs (shuffle ls)] [y (list)])
       (if (null? ls) (reverse y)
           (receive (a b)
             (break
               (let ([x (car ls)])
                 (lambda (y) (lset= = x y)))
               xs)
             (f (cdr ls) (append a (cdr b))
               (cons (car b) y))))))
    (define (randomize-vertically grid)
      (concatenate
        (map transpose
          (transpose
            (map (compose split3
                       randomize-third
                       concatenate)
              (transpose
                (map (compose split3
                           transpose)
                  (split3 grid))))))))
   (define (randomize-triplets grid)
     (concatenate
       (concatenate
         (randomize-vertically
           (transpose
             (map concatenate
               (randomize-vertically
                 grid)))))))
    (define (swap-occurences grid)
      (map
        (let ([indexes
                (list->vector
                  (shuffle (iota 9 1)))])
          (lambda (x)
            (vector-ref indexes (- x 1))))
        grid))
    (define (make-solution)
      (list->vector
        (swap-occurences
          (randomize-triplets
            (permute
              (cube->rows
                (map transpose
                  (split3
                    (map
                      (lambda (xs i)
                        (split3
                          (map (lambda (x)
                                 (+ x (* i 3) 1))
                            xs)))
                      (cdr all-3*3-latin-squares)
                      (car all-3*3-latin-squares)))))
              (list 0 3 6 1 4 7 2 5 8))))))
    (define (initial-prune initial-clues)
      (let ([grid (vector-copy solution)])
        (for-each (lambda (i) (vector-set! grid i 0))
          (take (shuffle (iota 81)) (- 81 initial-clues)))
        grid))
    (define (initialize! solve initial-clues max-solutions)
      (set! solutions max-solutions)
      (until (> max-solutions solutions)
        (random-source-randomize! default-random-source)
        (shuffle! all-3*3-latin-squares)
        (set! solution (make-solution))
        (set! sudoku (initial-prune initial-clues))
        (set! solutions (solve sudoku max-solutions))))))

(define-library (sudoku solver)
  (import (scheme base) (srfi 8)
    (only (gauche base)
      compose dotimes))
  (export solve rate)
  (begin

    (define (list->rows ls)
      (let f ([i 0] [j 0] [ls ls] [y (list)])
        (cond [(> j 8) (f (+ i 1) 0 ls y)]
          [(> i 8) (reverse y)]
          [else
           (let ([k (car ls)])
             (if (> k 0)
                 (f i (+ j 1) (cdr ls)
                   (cons (list i j (- k 1)) y))
                 (let g ([k 0] [y y])
                   (if (> k 8)
                       (f i (+ j 1) (cdr ls) y)
                       (g (+ k 1)
                         (cons (list i j k) y))))))])))

    (define-record-type <node>
      (node h l u i j k r d s) #f
      (h header set-header!)
      (l left set-left!) (u up set-up!) (i ival)
      (j jval) (k kval) (r right set-right!)
      (d down set-down!)
      (s size set-size!))

    (define (update-size! f x)
      (let ([h (header x)])
        (set-size! h (f (size h) 1))))

    (define (horizontal-restore! x)
      (set-right! (left x) x)
      (set-left! (right x) x))

    (define (vertical-restore! x)
      (update-size! + x)
      (set-down! (up x) x)
      (set-up! (down x) x))

    (define (horizontal-delete! x)
      (set-right! (left x) (right x))
      (set-left! (right x) (left x)))

    (define (vertical-delete! x)
      (set-down! (up x) (down x))
      (set-up! (down x) (up x))
      (update-size! - x))

    (define (make-headers)
      (let ([g (node #f #f #f #f #f #f
                   #f #f #f)])
        (set-left! g g) (set-right! g g)
        (dotimes (i 4)
          (dotimes (j 9)
            (dotimes (k 9)
              (let ([h (node #f (left g) #f i j k g
                          #f 0)]) (set-header! h h)
                (set-up! h h) (set-down! h h)
                (horizontal-restore! h))))) g))

    (define (grid-for-each direction root f)
      (do ([x (direction root) (direction x)])
            ((eq? x root)) (f x)))

    (define (make-row! grid r c v)
      (define i 0)
      (define z (make-vector 4 #f))
      (define constraints
        (vector
          (lambda (j k) (and (= j r) (= k c)))
          (lambda (j k) (and (= j r) (= k v)))
          (lambda (j k) (and (= j c) (= k v)))
          (lambda (j k)
            (and (= k v)
              (= j (+ (* (floor-quotient r 3) 3)
                      (floor-quotient c 3)))))))
      (do ([h (right grid) (right h)])
            ((vector-ref z 3) (vector->list z))
        (and (= (ival h) i)
          ((vector-ref constraints i)
            (jval h) (kval h))
          (vector-set! z i
            (let ([x (node h #f (up h)
                         r c v #f h #f)])
              (vertical-restore! x) x))
          (set! i (+ i 1)))))

    (define (make-grid ls)
      (let ([grid (make-headers)])
        (for-each
          (lambda (rcv)
            (apply
              (lambda (r c v)
                (apply
                  (lambda (r-c r-v c-v b-v)
                    (set-left! r-c b-v)
                    (set-right! r-c r-v)
                    (set-left! r-v r-c)
                    (set-right! r-v c-v)
                    (set-left! c-v r-v)
                    (set-right! c-v b-v)
                    (set-left! b-v c-v)
                    (set-right! b-v r-c))
                  (make-row! grid r c v))) rcv))
        (list->rows ls)) grid))

    (define (vertical-cover! h)
      (grid-for-each down h
        (lambda (r) (grid-for-each right r
                             vertical-delete!)))
      (horizontal-delete! h))

    (define (vertical-uncover! h)
      (grid-for-each up h
        (lambda (h) (grid-for-each right h
                              vertical-restore!)))
      (horizontal-restore! h))

    (define (horizontal-cover! x)
      (grid-for-each right x
        (compose vertical-cover! header)))

    (define (horizontal-uncover! x)
      (grid-for-each left x
        (compose vertical-uncover!
          header)))

    (define (optimal-branch sudoku)
      (let compare
        ([constraint (right sudoku)]
         [minimum 9] [ls (list)])
        (if (eq? constraint sudoku)
            (if (= minimum 1)
                (select-rows
                  (concatenate
                    (map down ls)))
                (car ls))
            (let ([n (size constraint)])
              (cond
                [(< minimum n)
                 (compare (right constraint)
                   minimum ls)]
                [(= n minimum)
                 (compare (right constraint)
                   minimum
                   (cons constraint ls))]
                [else
                 (compare (right constraint)
                   n (list constraint))])))))

    (define (optimal-branch h)
      (do ([c (right h) (right c)]
             [m (right h)
              (if (< (size c) (size m)) c m)])
            ((or (= (size m) 1) (eq? c h))
             m)))
    (define (solve sudoku max-solutions)
      (let ([clues (list)] [solutions 0]
             [sudoku
              (make-grid
                (vector->list sudoku))])
        (call-with-current-continuation
          (lambda (return)
            (let search! ()
              (cond
                [(> solutions max-solutions)
                 (return solutions)]
                [(eq? (right sudoku) sudoku)
                 (set! solutions
                   (+ solutions 1))]
                [else
                 (let ([constraint
                         (optimal-branch
                           sudoku)])
                   (vertical-cover!
                     (header constraint))
                   (grid-for-each
                     down constraint
                     (lambda (possibility)
                       (set! clues
                         (cons possibility clues))
                       (horizontal-cover!
                         possibility)
                       (search!)
                       (let ([hint (car clues)])
                         (set! clues (cdr clues))
                         (set! possibility hint))
                       (set! constraint
                         (header possibility))
                       (horizontal-uncover!
                         possibility)))
                   (vertical-uncover!
                     (header constraint)))]))
            (return solutions)))))

    (define (optimal-rate sudoku)
      (let compare
        ([constraint (right sudoku)]
         [minimum 9] [ls (list)])
        (if (eq? constraint sudoku)
            (values minimum ls)
            (let ([n (size constraint)])
              (cond
                [(< minimum n)
                 (compare (right constraint)
                   minimum ls)]
                [(= n minimum)
                 (compare (right constraint)
                   minimum
                   (cons constraint ls))]
                [else
                 (compare (right constraint)
                   n (list constraint))])))))

    (define (rate sudoku)
      (let ([sudoku
              (make-grid
                (vector->list sudoku))]
             [clues (list)] [guesses 0]
             [cycles 0])
        (call-with-current-continuation
          (lambda (return)
            (let search! ([c 0] [g 0])
              (set! cycles c)
              (set! guesses g)
              (unless
                (eq? (right sudoku) sudoku)
                 (receive (a b)
                   (optimal-rate sudoku)
                   (cond
                     [(= a 1)
                      (for-each
                        (lambda (hint)
                          (vertical-cover! hint)
                          (grid-for-each
                            down hint
                            (lambda (possibility)
                              (set! clues
                                (cons possibility
                                  clues))
                              (horizontal-cover!
                                possibility))))
                        b)
                      (search! (+ c 1) g)]
                     [else
                      (let ([constraint (car b)])
                        (vertical-cover!
                          constraint)
                        (grid-for-each
                          down constraint
                          (lambda (possibility)
                            (set! clues
                              (cons possibility
                                clues))
                            (horizontal-cover!
                              possibility)
                            (search! c (+ g 1))
                            (set! possibility
                              (car clues))
                            (set! clues
                              (cdr clues))
                            (set! constraint
                              (header possibility))
                            (horizontal-uncover!
                              possibility)))
                        (vertical-uncover!
                          constraint))]))))
            (return (list guesses
                          cycles))))))))

(import (scheme base)
  (sudoku grid) (sudoku solver) (srfi 1)
  (sudoku printing)
  (only (gauche base) pa$ until)
  (only (srfi 27) default-random-source
    random-source-randomize!)
  (only (scheme write) display))

(random-source-randomize! default-random-source)

(define (erasables grid)
  (filter-map
    (lambda (i)
      (and (positive? (vector-ref grid i))
        (let ([grid (vector-copy grid)])
          (vector-set! grid i 0)
          (and (< (solve grid 1) 2) grid))))
    (iota 81)))

(define (remove-useless grid)
  (print "Optimizing...")
  (let* ([ls (erasables grid)]
         [g (if (null? ls) grid
                (let f ([ls ls])
                  (display "=> ")
                  (flush-output-port)
                  (let ([next
                         (concatenate
                           (map erasables ls))])
                    (if (null? next) (car ls)
                        (f next)))))])
    (print-sudoku solve rate g) g))

(define
  (finish max-solutions grid)
  (let f ([index 0]
           [attempts
            (list (vector-copy grid))]
           [max-solutions max-solutions])
    (cond
      [(> index 80)
       (let ([grid (first (shuffle attempts))])
         (print-sudoku solve rate grid)
         (finish max-solutions
           grid))]
      [(positive?
         (vector-ref (first attempts) index))
       (f (+ index 1) attempts
          max-solutions)]
      [else
       (let ([grid (vector-copy grid)])
         (vector-set! grid index
           (vector-ref solution index))
         (let ([solutions
                 (solve grid max-solutions)])
           (cond
             [(or (> solutions max-solutions)
                   (= solutions 0))
              (f (+ index 1) attempts
                 max-solutions)]
             [(= solutions 1)
              (print-sudoku solve rate grid)
              (remove-useless grid)]
             [(= solutions max-solutions)
              (f (+ index 1)
                 (cons grid attempts)
                 max-solutions)]
             [else (f (+ index 1) (list grid)
                         solutions)])))])))

(initialize! solve 23 300)
(print-sudoku solve rate solution)
(print-sudoku solve rate sudoku)
(finish solutions sudoku)
