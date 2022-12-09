(import
    (chezscheme))

(define-syntax cond->
  (syntax-rules ()
    ((cond-> x pred (f args ...))
     (if pred
       (f x args ...)
       x))))

(define (alpha)
  (let loop ([idx 25]
             [lowercase '()]
             [uppercase '()])
    (if (< idx 0)
      (append lowercase uppercase)
      (loop (- idx 1)
            (cons (integer->char (+ 97 idx)) lowercase)
            (cons (integer->char (+ 65 idx)) uppercase)))))

(define (inc n)
  (+ n 1))

(define (interleave l1 l2)
  (if (null? l1)
    l2
    (cons (cons (car l1) (car l2))
          (interleave (cdr l1) (cdr l2)))))

(define priorities
  (interleave (alpha) (map inc (iota 52))))

(define (first-match haystack needles)
  (if (null? needles)
    #f
    (let loop ([search needles])
      (if (null? search)
        #f
        (let* ([term (car search)]
               [result (find (lambda (x) (equal? x term)) haystack)])
          (if result
            result
            (loop (cdr search))))))))

(define (part-one)
  (let ([p (open-input-file "./input/3.txt")])
    (let loop ([total 0]
               [line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          total)
        (let* ([items (string->list line)]
               [prioritized-items (map (lambda (c) (cdr (assq c priorities))) items)]
               [compartment-size (/ (length items) 2)]
               [first-compartment (list-head prioritized-items compartment-size)]
               [second-compartment (list-tail prioritized-items compartment-size)]
               [match (first-match first-compartment second-compartment)])
          (loop (cond-> total
                  match (+ match))
                (get-line p)))))))

; (printf "~a\n" (part-one))

(define (read-lines f)
  (let ([p (open-input-file f)])
    (let loop ([line (get-line p)]
               [lines '()])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          (reverse lines))
        (loop (get-line p)
              (cons line lines))))))

(define (part-two)
  (let* ([enum (make-enumeration (map (lambda (c)
                                       (string->symbol (make-string 1 c)))
                                     (alpha)))]
         [enum-constructor (enum-set-constructor enum)])
    (let loop ([total 0]
               [lines (map (lambda (line)
                           (enum-constructor (map (lambda (c)
                                                    (string->symbol (make-string 1 c)))
                                                  (string->list line))))
                         (read-lines "./input/3.txt"))])
      (if (null? lines)
        total
        (let* ([group (list-head lines 3)]
               [match (enum-set->list
                        (enum-set-intersection
                          (enum-set-intersection (car group) (cadr group))
                          (caddr group)))])
          (loop
            (+ total (cdr (assq (car (string->list (symbol->string (car match)))) priorities)))
            (list-tail lines 3)))))))

(printf "~a\n" (part-two))
