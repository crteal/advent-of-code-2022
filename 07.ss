(import
  (chezscheme))

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

(define (read-whitespace lst)
  (drop-while (lambda (c)
                (char-whitespace? c))
              lst))

(define (read-alphanumeric lst)
  (take-while (lambda (c)
                (or (equal? c #\.)
                    (char-alphabetic? c)
                    (char-numeric? c)))
              lst))

(define (read-command tokens)
  (let* ([cmd (read-alphanumeric (list-tail tokens 2))])
    `(COMMAND
      ,(string->symbol (list->string cmd))
      ,@(if (> (length tokens) 4)
          (list (string->symbol (list->string (list-tail tokens 5))))
          '()))))

(define (read-output tokens)
  (let loop ([t tokens]
             [result '()])
    (if (null? t)
      (cons 'OUTPUT (reverse result))
      (let* ([chomped (read-whitespace t)]
             [part (read-alphanumeric chomped)]
             [str (list->string part)]
             [number (string->number str)])
        (loop (list-tail chomped (+ (length part)))
              (cons (if number
                      number
                      (string->symbol str))
                    result))))))

(define (parse-line line)
  (let ([tokens (string->list line)])
    (if (equal? (car tokens) #\$)
      (read-command tokens)
      (read-output tokens))))

(define make-node
  (case-lambda
    [(name) (make-node name '())]
    [(name parent)
     (let* ([children '()]
            [get-size (lambda (lst)
                        (if (pair? name)
                          (cadr name)
                          (let loop ([size 0]
                                     [nodes lst])
                            (if (null? nodes)
                              size
                              (loop (+ size (node-size (car nodes)))
                                    (cdr nodes))))))])
       (lambda (proc . args)
         (case proc
           [(parent) parent]
           [(name) name]
           [(find-by-name)
            (let ([needle (car args)])
              (find (lambda (ch)
                      (equal? needle (node-name ch)))
                    children))]
           [(child-ref) (list-ref children (car args))]
           [(add-child!) (set! children (cons (car args) children))]
           [(size) (get-size children)]
           [(print) (printf "~a ~a\n" name (get-size children))
                    (for-each node-print children)]
           [(walk) (let [(fn (car args))]
                     (for-each (lambda (child)
                                 (node-walk fn child))
                               children))])))]))

(define (node-name node)
  (node 'name))

(define (node-parent node)
  (node 'parent))

(define (node-size node)
  (node 'size))

(define (node-add-child! node child)
  (node 'add-child! child))

(define (node-find-by-name node name)
  (node 'find-by-name name))

(define (node-print node)
  (node 'print))

(define (node-walk fn node)
  (node 'walk fn)
  (fn node))

(define (run-command vm cmd . args)
  (case cmd
    [(cd) (let ([dir (car args)])
            (printf "rc: ~a\n" dir)
            )]))

(define (vm)
  (let* ([tree (make-node '/)]
         [ptr tree])
    (lambda (op . args)
      (case op
        [(TREE) tree]
        [(COMMAND) 
         (when (equal? (car (car args)) 'cd)
                     (let ([dir (cadr (car args))])
                       (case dir
                         [(..) (when (not (null? (node-parent ptr)))
                                 (set! ptr (node-parent ptr)))]
                         [(\/) (set! ptr tree)]
                         (else (let ([child (node-find-by-name ptr dir)])
                                 (when child
                                   (set! ptr child)))))))]
        [(OUTPUT) (node-add-child! ptr
                    (make-node (if (number? (car (car args)))
                                 (reverse (car args))
                                 (cadr (car args)))
                               ptr))]))))

(define (part-one)
  (let ([m (vm)]
        [p (open-input-file "./input/7.txt")])
    (let loop ([line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          (let ([total-size 0])
            (node-walk (lambda (node)
                         (when (symbol? (node-name node))
                           (let ([size (node-size node)])
                             (when (<= size 100000)
                               (set! total-size (+ total-size size))))))
                       (m 'TREE))
            total-size))
        (let* ([form (parse-line line)])
          (m (car form) (cdr form))
          (loop (get-line p)))))))

; (printf "~a\n" (part-one))

(define (part-two)
  (let ([m (vm)]
        [p (open-input-file "./input/7.txt")])
    (let loop ([line (get-line p)])
      (if (eof-object? line)
        (begin
          (close-input-port p)
          (let* ([tree (m 'TREE)]
                 [lowest-size 700000000]
                 [total-size (node-size tree)]
                 [free-size (- 70000000 total-size)])
            (node-walk (lambda (node)
                         (when (symbol? (node-name node))
                           (let ([size (node-size node)])
                             (when (and (>= (+ free-size size) 30000000)
                                        (< size lowest-size))
                               (set! lowest-size size)))))
                       (m 'TREE))
            lowest-size))
        (let* ([form (parse-line line)])
          (m (car form) (cdr form))
          (loop (get-line p)))))))

(printf "~a\n" (part-two))
