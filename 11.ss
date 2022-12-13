(import
  (chezscheme))

(define (monkey initial-items operation test)
  (let ([items initial-items]
        [inspections 0])
    (lambda (proc . args)
      (case proc
        [(INSPECTIONS) inspections]
        [(toss!) (set! items (append items args))]
        [(inspect!) (let loop ([els items]
                               [result '()])
                      (if (null? els)
                        (begin
                          (set! items '())
                          result)
                        (let* ([item (car els)]
                               [worry-level (floor (/ (operation item) 3))]
                               [recipient (test worry-level)])
                          (set! inspections (+ inspections 1))
                          (loop (cdr els)
                                (cons `(,recipient ,worry-level) result)))))]))))

(define (do-times fn n)
  (let loop ([i 0])
    (when (< i n)
      (fn i)
      (loop (+ i 1)))))

(define (round! monkeys)
  (do-times (lambda (i)
              (for-each (lambda (toss)
                          ((vector-ref monkeys (car toss)) 'toss! (cadr toss)))

                        ((vector-ref monkeys i) 'inspect!)))
            (vector-length monkeys)))

(define (monkey-business! monkeys rounds)
  (do-times (lambda (rnd)
              (time (round! monkeys)))
            rounds)
  (let ([sorted (sort > (map (lambda (monkey)
                               (monkey 'INSPECTIONS))
                             monkeys))])
    (* (car sorted) (cadr sorted))))

(define (read-lines f)
  (let ([p (open-input-file f)])
    (let loop ([line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          '())
        (cons (string->list line) (loop (get-line p)))))))

(define (make-operation tokens)
  (let* ([elided (list-tail tokens 23)]
         [fn (case (car elided)
               [(#\*) *]
               [(#\+) +])]
         [n (string->number (list->string (list-tail elided 2)))])
    (lambda (item)
      (fn item (if (not n) item n)))))

(define (make-test tokens n-true n-false)
  (let ([divisor (string->number (list->string (list-tail tokens 21)))])
    (lambda (item)
      (if (= (mod item divisor) 0)
        n-true
        n-false))))

(define (take-while f lst)
  (if (null? lst)
    lst
    (if (f (car lst))
      (cons (car lst) (take-while f (cdr lst)))
      (list))))

(define (drop-while f lst)
  (if (or (null? lst)
          (not (f (car lst))))
    lst
    (drop-while f (cdr lst))))

(define (make-items tokens)
  (let loop ([elements tokens]
             [res '()])
    (if (null? elements)
      (reverse res)
      (let ([n (take-while (lambda (c)
                             (char-numeric? c))
                           elements)])
        (loop (if (null? n)
                (drop-while (lambda (c)
                              (not (char-numeric? c)))
                            elements)
                (list-tail elements (length n)))
              (if (null? n)
                res
                (cons (string->number (list->string n)) res)))))))

(define MONKEY-CONSTRUCTOR monkey)
 
(define (read-monkey lines)
  (let ([test-true-line (car (list-tail lines 4))]
        [test-false-line (car (list-tail lines 5))]
        [items (make-items (cadr lines))])
    (MONKEY-CONSTRUCTOR items
            (make-operation (caddr lines))
            (make-test (caddr (cdr lines))
                       (string->number (string (list-ref test-true-line (- (length test-true-line) 1))))
                       (string->number (string (list-ref test-false-line (- (length test-false-line) 1))))))))


(define (read-monkeys f)
  (let loop ([monkeys '()]
             [lines (read-lines f)])
    (if (null? lines)
      (list->vector (reverse monkeys))
      (let ([monkey? (not (null? (car lines)))])
        (loop (if monkey?
                (cons (read-monkey (list-head lines 6)) monkeys)
                monkeys)
              (if monkey?
                (list-tail lines 6)
                (cdr lines)))))))

(define (part-one)
  (let ([monkeys (read-monkeys "./input/11.txt")])
    (monkey-business! monkeys 20)))

; (printf "~a\n" (part-one))

(define (monkey-2 initial-items operation test)
  (let ([items initial-items]
        [inspection-count 0]
        [inspections (make-eq-hashtable)])
    (lambda (proc . args)
      (case proc
        [(INSPECTIONS) inspection-count]
        [(toss!) (set! items (append! items args))]
        [(inspect!)
         (let ([previous-items items])
           (set! items '())
           (map (lambda (item)
                  (let ([cached (hashtable-ref inspections item #f)])
                    (if cached
                      cached
                      (let* ([worry-level (time (operation item))]
                             [recipient (time (test worry-level))]
                             [memo `(,recipient ,worry-level)])
                        (hashtable-set! inspections item memo)
                        memo))))
                previous-items))]))))

(define (part-two)
  (set! MONKEY-CONSTRUCTOR monkey-2)
  (let ([monkeys (read-monkeys "./input/11-a.txt")])
    (monkey-business! monkeys 10000)))

(printf "~a\n" (part-two))
