(import
    (chezscheme))

(define (take-while f lst)
  (if (null? lst)
    lst
    (if (f (car lst))
      (cons (car lst) (take-while f (cdr lst)))
      (list))))

(define (parse-assignment lst)
  (let* ([left (take-while (lambda (c)
                            (not (equal? c #\-)))
                           lst)]
         [right (list-tail lst (+ (length left) 1))])
    (list (string->number (list->string left))
          (string->number (list->string right)))))

(define (contained? l)
  (let* ([parts (string->list l)]
         [left (take-while (lambda (c)
                                 (not (equal? c #\,)))
                               parts)]
         [parsed-left (parse-assignment left)]
         [right (list-tail parts (+ (length left) 1))]
         [parsed-right (parse-assignment right)])
    (or (and (>= (car parsed-left) (car parsed-right))
             (<= (cadr parsed-left) (cadr parsed-right)))
        (and (>= (car parsed-right) (car parsed-left))
             (<= (cadr parsed-right) (cadr parsed-left))))))

(define (part-one)
  (let ([p (open-input-file "./input/4.txt")])
    (let loop ([total 0]
               [line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          total)
        (loop (if (contained? line)
                (+ total 1)
                total)
              (get-line p))))))

; (printf "~a\n" (part-one))

(define (overlaps? l)
  (let* ([parts (string->list l)]
         [left (take-while (lambda (c)
                                 (not (equal? c #\,)))
                               parts)]
         [parsed-left (parse-assignment left)]
         [right (list-tail parts (+ (length left) 1))]
         [parsed-right (parse-assignment right)])
    (or (contained? l)
        (and (<= (car parsed-left) (car parsed-right))
             (>= (cadr parsed-left) (car parsed-right)))
        (and (<= (car parsed-right) (car parsed-left))
             (>= (cadr parsed-right) (car parsed-left))))))

(define (part-two)
  (let ([p (open-input-file "./input/4.txt")])
    (let loop ([total 0]
               [line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          total)
        (loop (if (overlaps? line)
                (+ total 1)
                total)
              (get-line p))))))

(printf "~a\n" (part-two))
