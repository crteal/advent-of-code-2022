(import
    (chezscheme))

(define opponent-strategy
  '((#\A . ROCK)
    (#\B . PAPER)
    (#\C . SCISSORS)))

(define my-strategy
  '((#\X . ROCK)
    (#\Y . PAPER)
    (#\Z . SCISSORS)))

(define shape-scores
  '((ROCK . 1) (PAPER . 2) (SCISSORS . 3)))

(define (beats? s1 s2)
  (cond
    [(equal? s1 'ROCK) (equal? s2 'SCISSORS)]
    [(equal? s1 'PAPER) (equal? s2 'ROCK)]
    [(equal? s1 'SCISSORS) (equal? s2 'PAPER)]))

(define (score-match s1 s2)
  (let ([s1-shape-score (cdr (assq s1 shape-scores))]
	[s2-shape-score (cdr (assq s2 shape-scores))])
    (cond
      [(equal? s1 s2) (list (+ 3 s1-shape-score) (+ 3 s2-shape-score))]
      [(beats? s1 s2) (list (+ 6 s1-shape-score) s2-shape-score)]
      (else (list s1-shape-score (+ 6 s2-shape-score))))))

(define (part-one)
  (let ([p (open-input-file "./input/2.txt")])
    (let loop ([score 0]
	       [line (get-line p)])
      (if (eof-object? line)
	(begin
	  (close-input-port p)
	  score)
	(let* ([parts (string->list line)]
               [my-move (cdr (assq (caddr parts) my-strategy))]
	       [opponent-move (cdr (assq (car parts) opponent-strategy))]
	       [match-score (score-match my-move opponent-move)])
	  (loop (+ score (car match-score))
		(get-line p)))))))

; (printf "~a\n" (part-one))

(define (losing-move s)
  (cond
    [(equal? s 'ROCK) 'SCISSORS]
    [(equal? s 'PAPER) 'ROCK]
    [(equal? s 'SCISSORS) 'PAPER]))

(define (winning-move s)
  (cond
    [(equal? s 'ROCK) 'PAPER]
    [(equal? s 'PAPER) 'SCISSORS]
    [(equal? s 'SCISSORS) 'ROCK]))

(define (shape-by-goal goal s2)
  (cond
    [(equal? goal #\X) (losing-move s2)]
    [(equal? goal #\Y) s2]
    (else (winning-move s2))))

(define (part-two)
  (let ([p (open-input-file "./input/2.txt")])
    (let loop ([score 0]
	       [line (get-line p)])
      (if (eof-object? line)
	(begin
	  (close-input-port p)
	  score)
	(let* ([parts (string->list line)]
	       [opponent-move (cdr (assq (car parts) opponent-strategy))]
	       [my-move (shape-by-goal (car (assq (caddr parts) my-strategy))
				       opponent-move)]
	       [match-score (score-match my-move opponent-move)])
	  (loop (+ score (car match-score))
		(get-line p)))))))

(printf "~a\n" (part-two))
