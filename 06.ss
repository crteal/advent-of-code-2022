(import
  (chezscheme))

(define (start-of-packet-marker lst)
  (let loop ([idx 0]
             [l lst]
             [seq '()])
    (if (or (null? l)
            (= (length seq) 4))
      `(,idx ,seq)
      (let* ([c (car l)]
             [found? (find (lambda (el)
                             (equal? el c))
                           seq)])
        (loop (+ 1 idx)
              (cdr l)
              (if found?
                `(,c)
                (cons c seq)))))))

(define (part-one)
  (let ([p (open-input-file "./input/6.txt")])
    (let loop ([idx 0]
               [line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          idx)
        (let* ([chars (string->list line)]
               [sopm (start-of-packet-marker chars)])
          (loop (car sopm)
                (get-line p)))))))

; (printf "~a\n" (part-one))

(define (start-of-packet-marker-2 lst)
  (let loop ([idx 0]
             [l lst]
             [seq '()])
    (if (or (null? l)
            (= (length seq) 14))
      `(,idx ,seq)
      (let* ([c (car l)]
             [found? (find (lambda (el)
                             (equal? el c))
                           seq)])
        (loop (+ 1 idx)
              (cdr l)
              (append (if found?
                        (cdr (memq found? seq))
                        seq)
                      `(,c)))))))

(define (part-two)
  (let ([p (open-input-file "./input/6.txt")])
    (let loop ([idx 0]
               [line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          idx)
        (let* ([chars (string->list line)]
               [sopm (start-of-packet-marker-2 chars)])
          (loop (car sopm)
                (get-line p)))))))

(printf "~a\n" (part-two))
