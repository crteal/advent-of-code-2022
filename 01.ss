(import
    (chezscheme))

(define-syntax cond->
  (syntax-rules ()
    ((cond-> x pred (f args ...))
     (if pred
       (f x args ...)
       x))))

(define (part-one)
  (let ([p (open-input-file "./input/1.txt")])
    (let loop ([line (get-line p)]
	       [top-elf 0]
	       [elves '()])
      (if (eof-object? line)
	(begin
	  (close-input-port p)
	  top-elf)
	(let* ([line-empty? (string=? line "")]
	       [not-empty? (not (null? elves))]
	       [calories (cond-> (string->number line)
				 (and not-empty? (not line-empty?)) (+ (car elves)))])
	  (loop (get-line p)
		(if line-empty?
		  top-elf
		  (max calories top-elf))
		(if line-empty?
		  (cons 0 elves)
		  (cons calories (cond-> elves not-empty? (cdr))))))))))

; (printf "~a\n" (part-one))

(define (nth list n)
  (if (= n 0)
    (car list)
    (nth (cdr list) (- n 1))))

(define (part-two)
  (let ([p (open-input-file "./input/1.txt")])
    (let loop ([line (get-line p)]
	       [elves '()])
      (if (eof-object? line)
	(begin
	  (close-input-port p)
	  (let ([sorted-elves (sort > elves)])
	    (+ (car sorted-elves) (cadr sorted-elves) (nth sorted-elves 2))))
	(let* ([line-empty? (string=? line "")]
	       [not-empty? (not (null? elves))]
	       [calories (cond-> (string->number line)
				 (and not-empty? (not line-empty?)) (+ (car elves)))])
	  (loop (get-line p)
		(if line-empty?
		  (cons 0 elves)
		  (cons calories (cond-> elves not-empty? (cdr))))))))))

(printf "~a\n" (part-two))
