(define-library (sudoku printing)
  (import (scheme base)
    (only (scheme write) display)
    (only (gauche base) dotimes
      integer->digit print x->string)
    (only (srfi 1) count))
  (export print-sudoku print-all)
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
    (define (print-sudoku grid)
      (let ([sudoku (vector->list grid)]
             [frame
              (map string-copy frame)])
        (print #"Initial clues: ~(count positive? sudoku)")
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
          (display #"~(vector-ref grid i) ")))
      (newline) (newline))
    (define (print-all s i g)
      (print #"Number of solutions: ~s")
      (print #"Last index changed: ~i")
      (print-sudoku g))))

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
    (only (gauche base) compose))
  (export receive print-all make-grid
    print-sudoku shuffle shuffle! solution)
  (begin (random-source-randomize!
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
      (shuffle
        (cube->rows
          (map (compose permutations
                     rotations)
            (list (list 0 1 2) (list 0 2 1))))))
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
    (define solution
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
              (list 0 3 6 1 4 7 2 5 8))))))))

(define-library (sudoku solver)
  (import (scheme base)) (export solve)
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
        (do ([i 0 (+ i 1)]) ((> i 3))
          (do ([j 0 (+ j 1)]) ((> j 8))
            (do ([k 0 (+ k 1)]) ((> k 8))
              (let ([h (node #f (left g) #f i j k g
                          #f 0)]) (set-header! h h)
                (set-up! h h) (set-down! h h)
                (horizontal-restore! h))))) g))
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
      (do ([r (down h) (down r)])
            ((eq? r h))
        (do ([c (right r) (right c)]) ((eq? c r))
          (vertical-delete! c)))
      (horizontal-delete! h))
    (define (vertical-uncover! h)
      (do ([r (up h) (up r)]) ((eq? r h))
        (do ([c (right r) (right c)]) ((eq? c r))
          (vertical-restore! c)))
      (horizontal-restore! h))
    (define (horizontal-cover! x)
      (do ([c (right x) (right c)]) ((eq? c x))
        (vertical-cover! (header c))))
    (define (horizontal-uncover! x)
      (do ([c (left x) (left c)]) ((eq? c x))
        (vertical-uncover! (header c))))
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
                   (do ([possibility
                           (down constraint)
                           (down possibility)])
                         ((eq? possibility
                             constraint))
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
                       possibility))
                   (vertical-uncover!
                     (header constraint)))]))
            (return solutions)))))))

(import (scheme base)
  (sudoku grid) (sudoku solver) (srfi 1)
  (sudoku printing)
  (only (scheme write) display))
  (define (initial-prune n)
    (let ([clueless (make-vector 81 0)])
      (for-each
        (lambda (i)
          (vector-set! clueless i
            (vector-ref solution i)))
        (take (shuffle (iota 81)) n))
      clueless))
(define (best grid n)
  (let f ([i 0] [j (list (cons 0 (vector-copy
                                           grid)))] [n n])
    (cond
      [(> i 80) (cons n (car (shuffle j)))]
      [(> (vector-ref (cdr (car j)) i) 0)
       (f (+ i 1) j n)]
      [else
       (let ([g (vector-copy grid)])
         (vector-set! g i
           (vector-ref solution i))
         (let ([m (solve g n)])
           (cond [(or (> m n) (= m 0))
                      (f (+ i 1) j n)]
             [(= m 1) (cons 1 (cons i g))]
             [(= m n)
              (f (+ i 1) (cons (cons i g) j) n)]
             [else (f (+ i 1) (list (cons i g))
                        m)])))])))
(define (init-sudoku n m)
  (let f ()
    (let* ([grid (initial-prune n)]
             [l (solve grid m)])
        (if (<= l m) (values l grid) (f)))))
(define (separate-indexes grid)
  (let f ([i 80] [clues (list)] [clueless (list)])
    (cond [(< i 0) (cons clues clueless)]
      [(> (vector-ref grid i) 0)
       (f (- i 1) (cons i clues) clueless)]
      [else
       (f (- i 1) clues (cons i clueless))])))
(define (erasables grid)
  (let f ([is (car (separate-indexes grid))]
           [y (list)])
    (if (null? is) y
        (receive (i is) (car+cdr is)
          (let ([g (vector-copy grid)])
            (vector-set! g i 0)
            (let ([n (solve g 1)])
              (if (> n 1) (f is y)
                  (f is (cons g y)))))))))
(define (prunes grid)
  (map (lambda (i)
    (let ([g (vector-copy grid)])
      (vector-set! g i 0) g))
    (car (separate-indexes grid))))
(define (adds grid solution)
  (map (lambda (i)
             (let ([g (vector-copy grid)])
               (vector-set! g i
                 (vector-ref solution i)) g))
    (cdr (separate-indexes grid))))
(define (remove-useless grid)
  (display "Optimizing...\n")
  (let* ([ls (erasables grid)]
           [g (if (null? ls) grid
                   (let f ([ls ls]) (display "=> ")
                     (let ([next
                             (concatenate
                               (map erasables
                                 ls))])
                       (if (null? next) (car ls)
                           (f next)))))])
    (print-sudoku g) g))
(define (finish-sudoku grid n)
  (let f ([n n] [grid grid])
    (let* ([m (best grid n)] [g (cdr m)]
            [m (car m)] [i (car g)] [g (cdr g)])
      (print-all m i g)
      (if (= m 1) (remove-useless g)
          (f m g)))))
(define (make-sudoku n m)
  (print-sudoku solution)
  (receive (l grid) (init-sudoku n m)
    (cond [(< l 2) (print-all 1 -1 grid)]
      [else (print-all (solve grid 800)
                 -1 grid)
       (finish-sudoku grid l)])))

(make-sudoku 23 800)
