(import
  (chezscheme))

(define (vm)
  (let ([x 1]
        [cycle 0]
        [signal-strengths '()])
    (lambda (opcode . args)
      (case opcode
        [(REGX) x]
        [(CYCLE) cycle]
        [(SIGNAL-STRENGTH) (* x cycle)]
        [(SIGNAL-STRENGTHS) signal-strengths]
        [(noop) (set! cycle (+ cycle 1))
                (set! signal-strengths (append signal-strengths `(,(* x cycle))))]
        [(addx) (let ([new-x (+ x (car args))])
                  (set! cycle (+ cycle 2))
                  (set! signal-strengths
                    (append signal-strengths
                            `(,(* x (- cycle 1)) ,(* x cycle))))
                  (set! x new-x))]))))

(define (parse-line line)
  (let* ([tokens (string->list line)]
         [opcode (string->symbol (list->string (list-head tokens 4)))])
    (if (= (length tokens) 4)
      `(,opcode)
      `(,opcode
        ,(string->number (list->string (list-tail tokens 5)))))))

(define (part-one)
  (let ([m (vm)]
        [p (open-input-file "./input/10.txt")])
    (let loop ([line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          (apply + (map (lambda (i)
                          (list-ref (m 'SIGNAL-STRENGTHS) (- i 1)))
                        `(20 60 100 140 180 220))))
        (let ([operation (parse-line line)])
          (apply m operation)
          (loop (get-line p)))))))

;(printf "~a\n" (part-one))

(define (vm-2)
  (let* ([x 1]
         [cycle 0]
         [screen-width 40]
         [screen-height 6]
         [screen (make-vector (* screen-width screen-height))]
         [render (lambda (i)
                   (let* ([idx (- i 1)]
                          [clamped (mod idx screen-width)])
                     (vector-set! screen idx (if (and (>= clamped (- x 1))
                                                      (<= clamped (+ x 1)))
                                               #\#
                                               #\.))))])
    (lambda (opcode . args)
      (case opcode
        [(REGX) x]
        [(CYCLE) cycle]
        [(RENDER) (for-each (lambda (y)
                              (for-each (lambda (x)
                                          (display (vector-ref screen (+ (* y screen-width) x)))
                                          (display " "))
                                        (iota screen-width))
                              (newline))
                            (iota screen-height))]
        [(noop) (set! cycle (+ cycle 1))
                (render cycle)]
        [(addx) (let ([new-x (+ x (car args))])
                  (set! cycle (+ cycle 2))
                  (render (- cycle 1))
                  (render cycle)
                  (set! x new-x))]))))

(define (part-two)
  (let ([m (vm-2)]
        [p (open-input-file "./input/10.txt")])
    (let loop ([line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          (m 'RENDER))
        (let ([operation (parse-line line)])
          (apply m operation)
          (loop (get-line p)))))))

(part-two)
