;; an iteratee takes a chunk.
;; returns 3 values:
;; 1. 'done or 'cont. if 'done, then the following value is the result. if 'cont, a proc that takes a chunk. 
;; 2. if *eof*, any value will be ignored. if *done*, then the value to return. if *cont*, then resume continuation
;; 3. the remaining chunk that's not read/used

(define (prelude-break cpred lst)
  (let loop ((rbefore '()) (after lst))
    (cond
     ((null? after)
      (values (reverse rbefore) after))
     (else 
      (cond 
       ((cpred (car after))
	(values (reverse rbefore) after))
       (else
	(loop (cons (car after) rbefore)
	      (cdr after))))))))))))

(or (equal? (receive (before after) (prelude-break (lambda (x) (> x 5)) '(1 2 3 4 5 5 6 6 1 7 8 9))
	      (list before after))
	    '((1 2 3 4 5 5) (6 6 1 7 8 9)))
    (error "failed"))

(define (make-break-iteratee break-pred)
  (define (step chunk)
    (let loop ((acc '()) (chunk chunk))
      (cond
       ((eq? #f chunk)
	(pp (list 'A 'chunk chunk)) (newline)
	(values 'done acc '()))
       ((null? chunk)
	(pp (list 'B 'chunk chunk)) (newline)
	(values 'cont (lambda (c) (loop acc c)) '()))
       (else
	(receive (before after) (prelude-break break-pred chunk)
	  (cond
	   ;; need more until after is not empty
	   ((null? after)
	    (pp (list 'C 'chunk after)) (newline)
	    (values 'cont (lambda (c) (loop (append acc before) c)) '()))
	   (else
	    (pp (list 'D 'chunk after)) (newline)
	    (values 'done acc after))))))))
  step)

  
      
(begin
  (let ((iteratee (make-break-iteratee (lambda (x) (> x 5)))))
    (let loop ((i 1)
	       (chunks '((1 2 3 3 3)
			 (4 4 3 1 5 5)
			 (5 5)
			 #f #f )))
      (pp (list 'Iteration i 'Chunk (car chunks))) (newline)
      (receive (val1 val2 leftover) (iteratee (car chunks))
	(pp (list 'val1 val1 'val2 val2 'leftover leftover)) (newline)
	(if (not (eq? val1 'done))
	    (loop (+ 1 i) (cons (append leftover (cadr chunks)) (cddr chunks))))))))
		 



