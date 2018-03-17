;; Samdoku modular rewrite for generator integration

(define-library (sudoku solver)
  (import (scheme base)
    (only (srfi 1) iota))
  (export solve)
  (begin

#| Resetting indentation here.
    Come on I'm on my phone...
|#

(define (list->rows ls)
  (let f ([i 0] [j 0] [ls ls] [is '()] [js '()] [ks '()])
    (cond [(> j 8) (f (+ i 1) 0 ls is js ks)]
      [(> i 8) (map reverse (list is js ks))]
      [else
       (let ([k (car ls)])
         (if (> k 0)
             (f i (+ j 1) (cdr ls) (cons i is)
               (cons j js) (cons (- k 1) ks))
             (f i (+ j 1) (cdr ls)
               (append (make-list 9 i) is)
               (append (make-list 9 j) js)
               (append (iota 9 8 -1) ks))))])))

;; Matrix constructors
(define-record-type <node>
  (node h l u i j k r d s) #f
  (h header set-header!) (l left set-left!)
  (u up set-up!) (i ival) (j jval) (k kval)
  (r right set-right!) (d down set-down!)
  (s size set-size!))

(define (update-size! f x)
  (let ([h (header x)])
    (set-size! h (f (size h) 1))))

(define (horizontal-restore! x)
  (set-right! (left x) x) (set-left! (right x) x))

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

;; Make (horizontal) headers.
(define grid
  (let ([g (node #f #f #f #f #f #f #f #f #f)])
    (set-left! g g) (set-right! g g)
    (do ([i 0 (+ i 1)]) ((> i 3))
      (do ([j 0 (+ j 1)]) ((> j 8))
        (do ([k 0 (+ k 1)]) ((> k 8))
          (let ([h (node #f (left g) #f i j k
                       g #f 0)]) (set-header! h h)
            (set-up! h h) (set-down! h h)
            (horizontal-restore! h))))) g))

#| Map constraints (matrix columns)
    over possibilities (matrix rows).
|#
(define (make-row! r c v) (define i 0)
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
  (apply for-each
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
        (make-row! r c v))) (list->rows ls)))

#| Matrix modifiers for backtracking
    algorithm (they try).
|#
(define (vertical-cover! h)
  (do ([r (down h) (down r)]) ((eq? r h))
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
        ((or (= (size m) 1) (eq? c h)) m)))

(define (solve grid)
  (let ([grid (make-grid grid)] [n 'bad]
         [attempt (list)])
    (call-with-current-continuation
      (lambda (out)
        (let search! ([k 0])
          (cond
            [(eq? (right grid) grid)
             (if (eq? n 'bad) (set! n 'good)
                 (out 'many))]
            [else
             (let ([c (optimal-branch grid)])
               (vertical-cover! (header c))
               (do ([r (down c) (down r)])
                     ((eq? r c))
                 (set! attempt (cons r attempt))
                 (horizontal-cover! r)
                 (search! (+ k 1))
                 (let ([x (car attempt)])
                   (set! attempt (cdr attempt))
                   (set! r x)) (set! c (header r))
                 (horizontal-uncover! r))
               (vertical-uncover!
                 (header c)))])) (out n)))))
)) ;; End of library.
