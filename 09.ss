(import
  (chezscheme))

(define (parse-line line)
  (let ([tokens (string->list line)])
    `(,(car tokens) ,(string->number (list->string (list-tail tokens 2))))))

(define (distance x1 y1 x2 y2)
  `(,(- x1 x2) ,(- y1 y2)))

(define (vm)
  (let* ([origin '(0 0)]
         [head origin]
         [tail origin]
         [positions `(,origin)])
    (lambda (op . args)
      (let ([steps (if (not (null? args))
                     (car args)
                     0)]
            [move (lambda (x y)
                    (let* ([d (distance (car tail) (cadr tail) x y)])
                      (when (or (> (abs (car d)) 1)
                                (> (abs (cadr d)) 1))
                        (set! tail head)
                        (when (not (find (lambda (p)
                                           (equal? p tail))
                                         positions))
                          (set! positions (cons head positions))))
                      (set! head `(,x ,y))))])
        (case op
          [(POSITIONS) positions]
          [(#\U) (for-each (lambda (i)
                             (move (car head) (+ (cadr head) 1)))
                           (iota steps))]
          [(#\L) (for-each (lambda (i)
                             (move (- (car head) 1) (cadr head)))
                           (iota steps))]
          [(#\D) (for-each (lambda (i)
                             (move (car head) (- (cadr head) 1)))
                           (iota steps))]
          [(#\R) (for-each (lambda (i)
                             (move (+ (car head) 1) (cadr head)))
                           (iota steps))])))))

(define (part-one)
  (let ([m (vm)]
        [p (open-input-file "./input/9.txt")])
    (let loop ([line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          (length (m 'POSITIONS)))
        (let ([move (parse-line line)])
          (apply m move)
          (loop (get-line p)))))))

; (printf "~a\n" (part-one))

; (3,0) -> (4,1)
; (2,0) -> (3,1)

(define (vm-2)
  (let* ([origin '(0 0)]
         [knots (map (lambda (idx) origin) (iota 10))]
         [positions `(,origin)])
    (lambda (op . args)
      (let ([steps (if (not (null? args))
                     (car args)
                     0)]
            [move (lambda (idx x y)
                    (let loop ([i idx]
                               [x x]
                               [y y]
                               [results knots])
                      (if (= i (length knots))
                        (set! knots results)
                        (let* ([knot (list-ref knots i)]
                               [parent (list-ref knots (- i 1))]
                               [dist (distance (car knot) (cadr knot) x y)]
                               [x? (> (abs (car dist)) 1)]
                               [y? (> (abs (cadr dist)) 1)]
                               [follow? (or x? y?)]
                               [next-knot (if follow?
                                            (if x?
                                              `(,(car parent) ,(if y?
                                                                 (cadr parent) y))
                                              `(,(if x?
                                                   (car parent) x)
                                                 ,(cadr parent)))
                                            knot)])
                          (when (and follow?
                                     (= i (- (length knots) 1))
                                     (not (find (lambda (p)
                                                  (equal? p next-knot))
                                                positions)))
                            (set! positions (cons next-knot positions)))
                          (loop (+ i 1)
                                (car next-knot)
                                (cadr next-knot)
                                (append (list-head results (- i 1))
                                        `((,x ,y) ,next-knot)
                                        (list-tail results (+ i 1))))))))])
        (case op
          [(POSITIONS) positions]
          [(#\U) (for-each (lambda (i)
                             (let ([knot (car knots)])
                               (move 1 (car knot) (+ (cadr knot) 1))))
                           (iota steps))]
          [(#\L) (for-each (lambda (i)
                             (let ([knot (car knots)])
                               (move 1 (- (car knot) 1) (cadr knot))))
                           (iota steps))]
          [(#\D) (for-each (lambda (i)
                             (let ([knot (car knots)])
                               (move 1 (car knot) (- (cadr knot) 1))))
                           (iota steps))]
          [(#\R) (for-each (lambda (i)
                             (let ([knot (car knots)])
                               (move 1 (+ (car knot) 1) (cadr knot))))
                           (iota steps))])))))

(define (part-two)
  (let ([m (vm-2)]
        [p (open-input-file "./input/9.txt")])
    (let loop ([line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          (length (m 'POSITIONS)))
        (let ([move (parse-line line)])
          (apply m move)
          (loop (get-line p)))))))

(printf "~a\n" (part-two))
