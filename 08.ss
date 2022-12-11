(import
  (chezscheme))

(define (read-vector f)
  (let ([p (open-input-file f)])
    (let loop ([buffer '()]
               [line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          `(,(length (car buffer))
            ,(length buffer)
            ,(list->vector (apply append (reverse buffer)))))
        (loop (cons (map (lambda (c)
                                  (string->number (make-string 1 c)))
                                (string->list line))
                    buffer)
              (get-line p))))))

(define (distance x y width height)
  (sort (lambda (a b)
          (< (cdr a) (cdr b)))
        `((top . ,y) (right . ,(- width x)) (bottom . ,(- height y)) (left . ,x))))

(define (get-tree grid x y width height)
  (vector-ref grid (+ (* y width) x)))

(define (top-visible? x y width height grid)
  (let loop ([coord (- y 1)]
             [val (get-tree grid x y width height)])
    (if (> 0 coord)
      #t
      (if (<= val (get-tree grid x coord width height))
        #f
        (loop (- coord 1) val)))))

(define (right-visible? x y width height grid)
  (let loop ([coord (+ x 1)]
             [val (get-tree grid x y width height)])
    (if (= coord width)
      #t
      (if (<= val (get-tree grid coord y width height))
        #f
        (loop (+ coord 1) val)))))

(define (bottom-visible? x y width height grid)
  (let loop ([coord (+ y 1)]
             [val (get-tree grid x y width height)])
    (if (= coord height)
      #t
      (if (<= val (get-tree grid x coord width height))
        #f
        (loop (+ coord 1) val)))))

(define (left-visible? x y width height grid)
  (let loop ([coord (- x 1)]
             [val (get-tree grid x y width height)])
    (if (> 0 coord)
      #t
      (if (<= val (get-tree grid coord y width height))
        #f
        (loop (- coord 1) val)))))

(define (direction-visible? direction x y width height grid)
  (case direction
    [(top) (top-visible? x y width height grid)]
    [(right) (right-visible? x y width height grid)]
    [(bottom) (bottom-visible? x y width height grid)]
    [(left) (left-visible? x y width height grid)]))

(define (visible? x y grid)
  (let ([width (car grid)]
        [height (cadr grid)]
        [buffer (caddr grid)])
    (find (lambda (direction)
            (direction-visible? (car direction) x y width height buffer))
          (distance x y width height))))

(define (visible-count grid)
  (let ([count 0]
        [width (car grid)]
        [height (cadr grid)]
        [buffer (caddr grid)])
    (for-each (lambda (x)
                (for-each (lambda (y)
                            (when (visible? x y grid)
                              (set! count (+ count 1))))
                          (iota height)))
              (iota width))
    count))

(define (part-one)
  (visible-count (read-vector "./input/8.txt")))

; (printf "~a\n" (part-one))

(define (top-score x y width height grid)
  (let loop ([coord (- y 1)]
             [val (get-tree grid x y width height)])
    (if (> 0 coord)
      y
      (if (<= val (get-tree grid x coord width height))
        (- y coord)
        (loop (- coord 1) val)))))

(define (right-score x y width height grid)
  (let loop ([coord (+ x 1)]
             [val (get-tree grid x y width height)])
    (if (= coord width)
      (- width x 1)
      (if (<= val (get-tree grid coord y width height))
        (- coord x)
        (loop (+ coord 1) val)))))

(define (bottom-score x y width height grid)
  (let loop ([coord (+ y 1)]
             [val (get-tree grid x y width height)])
    (if (= coord height)
      (- height y 1)
      (if (<= val (get-tree grid x coord width height))
        (- coord y)
        (loop (+ coord 1) val)))))

(define (left-score x y width height grid)
  (let loop ([coord (- x 1)]
             [val (get-tree grid x y width height)])
    (if (> 0 coord)
      x
      (if (<= val (get-tree grid coord y width height))
        (- x coord)
        (loop (- coord 1) val)))))

(define (direction-score direction x y width height grid)
  (case direction
    [(top) (top-score x y width height grid)]
    [(right) (right-score x y width height grid)]
    [(bottom) (bottom-score x y width height grid)]
    [(left) (left-score x y width height grid)]))

(define (score x y grid)
  (let ([width (car grid)]
        [height (cadr grid)]
        [buffer (caddr grid)])
    (apply * (map (lambda (direction)
                    (direction-score (car direction) x y width height buffer))
                  (distance x y width height)))))

(define (highest-score grid)
  (let ([top -1]
        [width (car grid)]
        [height (cadr grid)]
        [buffer (caddr grid)])
    (for-each (lambda (x)
                (for-each (lambda (y)
                            (let ([s (score x y grid)])
                              (when (> s top)
                                (set! top s))))
                          (iota height)))
              (iota width))
    top))

(define (part-two)
  (highest-score (read-vector "./input/8.txt")))

(printf "~a\n" (part-two))
