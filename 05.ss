(import
    (chezscheme))

; http://www.cs.rpi.edu/courses/fall00/ai/scheme/reference/scheme-workshop/stacks.html
(define (make-stack . initial)
  (let ([stack (if (null? initial)
                 '()
                 (reverse initial))])
    (lambda (message . args)
      (case message
        [(empty?) (null? stack)]
        [(push!) (set! stack (cons (car args) stack))]
        [(top) (car stack)]
        [(pop!) (let ([result (car stack)])
                  (set! stack (cdr stack))
                  result)]
        [(size) (length stack)]
        [(nth) (list-ref stack (car args))]
        [(print) (for-each (lambda (e)
                             (display e)
                             (display " "))
                           stack)
                 (newline)]))))

(define (take-while f lst)
  (if (null? lst)
    lst
    (if (f (car lst))
      (cons (car lst) (take-while f (cdr lst)))
      (list))))

(define (parse-line line)
  (let ([l (string->list line)]
        [eat-garbage (lambda (lst)
                       (take-while (lambda (c)
                                     (not (char-numeric? c)))
                                   lst))]
        [eat-number (lambda (lst)
                      (take-while char-numeric? lst))])
    (let loop ([lst l]
               [numbers '()])
      (if (null? lst)
        (reverse numbers)
        (let ([garbage (eat-garbage lst)])
          (if (null? garbage)
            (let ([number (eat-number lst)])
              (loop (list-tail lst (length number))
                    (cons (string->number (list->string number)) numbers)))
            (loop (list-tail lst (length garbage))
                  numbers)))))))

(define (apply-operation! stacks operation)
  (let ([from (list-ref stacks (- (cadr operation) 1))]
        [to (list-ref stacks (- (caddr operation) 1))])
    (let loop ([i 0])
      (when (< i (car operation))
        (begin
          (to 'push! (from 'pop!))
          (loop (+ i 1)))))))

(define (part-one)
  (let ([p (open-input-file "./input/5-a.txt")]
        [stacks (list (make-stack 'W 'R 'F)
                      (make-stack 'T 'H 'M 'C 'D 'V 'W 'P)
                      (make-stack 'P 'M 'Z 'N 'L)
                      (make-stack 'J 'C 'H 'R)
                      (make-stack 'C 'P 'G 'H 'Q 'T 'B)
                      (make-stack 'G 'C 'W 'L 'F 'Z)
                      (make-stack 'W 'V 'L 'Q 'Z 'J 'G 'C)
                      (make-stack 'P 'N 'R 'F 'W 'T 'V 'C)
                      (make-stack 'J 'W 'H 'G 'R 'S 'V))])
    (let loop ([line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          (for-each (lambda (stack)
                      (stack 'print))
                    stacks))
        (begin
          (apply-operation! stacks (parse-line line))
          (loop (get-line p)))))))

; (part-one)

(define (elements! stack n)
  (let loop ([i n]
             [els '()])
    (if (> i 0)
      (loop (- i 1)
            (cons (stack 'pop!) els))
      els)))

(define (apply-operation-2! stacks operation)
  (let* ([from (list-ref stacks (- (cadr operation) 1))]
         [to (list-ref stacks (- (caddr operation) 1))])
    (for-each (lambda (e)
                (to 'push! e))
              (elements! from (car operation)))))

(define (part-two)
  (let ([p (open-input-file "./input/5-a.txt")]
        [stacks (list (make-stack 'W 'R 'F)
                      (make-stack 'T 'H 'M 'C 'D 'V 'W 'P)
                      (make-stack 'P 'M 'Z 'N 'L)
                      (make-stack 'J 'C 'H 'R)
                      (make-stack 'C 'P 'G 'H 'Q 'T 'B)
                      (make-stack 'G 'C 'W 'L 'F 'Z)
                      (make-stack 'W 'V 'L 'Q 'Z 'J 'G 'C)
                      (make-stack 'P 'N 'R 'F 'W 'T 'V 'C)
                      (make-stack 'J 'W 'H 'G 'R 'S 'V))])
    (let loop ([line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          (for-each (lambda (stack)
                      (stack 'print))
                    stacks))
        (begin
          (apply-operation-2! stacks (parse-line line))
          (loop (get-line p)))))))

(part-two)
