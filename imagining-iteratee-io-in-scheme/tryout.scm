;; cond-handler: a 2-arg proc. called if a condition occurs.
;; first arg is the condition: need-more-input, unexpected-eof, or any
;;   other condition (which will probably be passed as-is to the
;;   enumerator's general condition handler)
;; second arg is the continuation to call to resume this iteratee. if
;;   it's nil, then the the whole iteratee will be called if retried. if
;;   it's not nil, cc will be called with a new chunk.

;; done-token: see the multi-value return comment.

;; chunk: a chunk generated and passed by the enumerator


;; multi-value return from an iteratee:

;; first value: done-token | value
;; done-token is to be returned if iteratee does not want to process anymoref
;; value is the value to be returned to the enumerator's caller

;; second value: chunk | #f
;; 'chunk' is the chunk to be used in the next call to iteratee.
;; '#f' is to indicate to use a nil for the next iteratee call.


(define peek-iteratee (cond-handler done-token chunk)
  (cond 
   ((nil? chunk)
    (values done-token #f))
   
   ((empty-chunk? chunk)
    (cond-handler 'need-more-input nil))
   
   (else
    (values (car chunk) chunk))))



(define head-iteratee (cond-handler done-token chunk)
  (cond
   ((nil? chunk)
    (cond-handler 'unexpected-eof nil))

   ((empty-chunk? chunk)
    (cond-handler 'need-more-input nil))

   (else
    (values (car chunk) (cdr chunk)))))


(define make-break-iteratee (break-pred)
  (lambda (cond-handler done-token chunk)
    (cond
     ((nil? chunk)
      (values done-token #f))
     ((empty-chunk? chunk)
      (cond-handler 'need-more-input nil))
     (else
      (let step ((chunk chunk) (result '()))
        (call/mv 
         (prelude-break break-pred chunk)
         (lambda (before after)
           (cond
            ;; the whole chunk matches, so we need to get more
            ;; until it does not match
            ((nil? after)
             (step (call/cc
                    (lambda (resume) (cond-handler 'need-more-input resume)))
                   (append result (car break-result))))
            ;; not all chunk matches, so we are done.
            (else
             (values (append result before) after))))))))))



(define *DONE* (cons '() '()))
(define *CONT* (cons '() '()))

(define make-break-iteratee2 (break-pred)
  (lambda (chunk)
    (let ((result '()))
      (restartable ((chunk chunk))
                   (cond
                    ;; eof stream
                    ((nil? chunk)
                     (done result))
                    ((empty-chunk? chunk)
                     (yield))
                    (else
                     (call/mv 
                      (prelude-break break-pred chunk)
                      (lambda (before after)
                        (cond
                         ;; the whole chunk matches, so we need to get more
                         ;; until it does not match
                         ((nil? after)
                          (step (call/cc
                                 (lambda (resume) (cond-handler 'need-more-input resume)))
                   (append result (car break-result))))
            ;; not all chunk matches, so we are done.
            (else
             (values (append result before) after))))))))))


(define make-heads-iteratee (str-to-match)
  (if (equal? str-to-match "")
      #f
      (lambda (cond-handler done-token chunk)
        (cond
         ((nil? chunk)
          (values done-token #f))
         ((empty-chunk? chunk)
          (cond-handler 'need-more-input nil))
         (else
          (let step ((chunk chunk)
                     (remaining-match str-to-match)
                     (cnt 0))
            (cond
             ((equal? (car remaining-match) (car chunk))
              (step (call/cc
                     (lambda (resume) (cond-handler 'need-more-input resume)))
                    (cdr remaining-match)
                    (+1 cnt)))
             (else
              (values cnt chunk)))))))))



                                        ; -- return:: a -> NumberedM a
(define (return val)
  (lambda (curr_counter)
    (make-numbered-value curr_counter val)))

                                        ;-- (>>=):: NumberedM a -> (a -> NumberedM b) -> NumberedM b
(define (>>= m f)
  (lambda (curr_counter)
    (let* ((m_result (m curr_counter))
           (n1 (nvalue-tag m_result))   ; result of the delayed computation
           (v  (nvalue-val m_result))   ; represented by m
           
           (m1 (f v))                   ; feed the result to f, get another m1
           )
      (m1 n1))))                        ; The result of the bigger monad


                                        ;-- get the current id and increment it
                                        ;-- incr:: NumberedM Int
                                        ;-- incr = NumberedM $ \n -> (n+1,n)

(define incr 
  (lambda (n)
    (make-numbered-value (+ 1 n) n)))

                                        ;-- run_numberedM:: NumberedM a -> Int -> Numbered a
                                        ;-- run_numberedM (NumberedM m) init_count = m init_count

(define (runM m init-counter)
  (m init-counter))

(define (make-node val kids)
  (>>=
   incr
   (lambda (counter)
     (return (cons (make-numbered-value counter val) kids)))))


(define (make-node val kids)
  (lambda (curr_counter)
    (let* ((m_result (make-numbered-value (+ 1 curr_counter) curr_counter))
           (n1 (nvalue-tag m_result))   ; result of the delayed computation
           (v  (nvalue-val m_result))   ; represented by m
           
           (m1 (make-numbered-value (cons (make-numbered-value curr_counter val) kids) val))
           )
      (m1 n1)))
  (>>=
   (lambda (n)
     (make-numbered-value (+ 1 n) n))
   (lambda (counter)
     (make-numbered-value (cons (make-numbered-value counter val) kids) val))))





(define make-break-iteratee3 (break-pred)
  ;; result is the state that needs to be passed around
  ;; returns: tag tagvalue leftoverchunk result
  (lambda (chunk result)
    (let ((before (cdr result)))
      (cond
       ;; eof stream
       ((eq? #f chunk)
        (values 'cont 'unexpected-eof  #f (cons 'unexpected-eof before)))
       ((null? chunk)
        (values 'more 'noerror '() (cons 'noerror before)))
       (else
        (receive (break-before break-after) (prelude-break break-pred chunk)
          (let ((before* (append break-before before)))
            (cond
             ;; the whole chunk matches, so we need to get more
             ;; until it does not match
             ((null? break-after)
              (values 'more 'noerror '() (cons 'noerror before*)))
             ;; not all chunk matches, so we are done.
             (else
              (values 'done before* after (cons 'noerror before*)))))))))))

(let ((iteratee (make-break-iteratee3 (lambda (n) (> n 5))))
      (init-result (list 'initial '())))
  (let loop ((leftover '()) (result init-result))
    (let ((chunk (make-next-chunk leftover)))
      (receive (tag tagvalue leftoverchunk result*) (iteratee chunk result)
        (cond ((eq? tag 'done)
               tagvalue)
              ((eq? tag 'cont)
               ;; tag value is the error message
               (cond
                ((eq? tagvalue 'noerror)
                 (loop leftoverchunk result*)))))))))

                 
                   
;; but why pass around result?

;; returns MV: MV#1=the final resut. MV#2 the lazy list to use for the next iteratee.
(define *fail* (list 'fail))
(define break-iteratee4 (break-pred)
  (lambda (suspend init-source-list)
    (let loop ((source-list init-source-list) (consumed-chunks '()))
      (let ((chunk (car (force source-list))) (next-source (cdr (force source-list))))
        (cond
         ((error? chunk)
          (values (make-error 'non-retryable chunk) init-source-list))
         ((eof? chunk)
          (values (suspend (make-done-value result)) next-source))
         ((empty? chunk)
          (loop next-source result))
         (else
            (receive (break-before break-after) (prelude-break break-pred chunk)
              (cond
               ;; the whole chunk matches, so we need to get more
               ;; until it does not match
               ((null? break-after)
                (loop next-source (append break-before result)))
               ;; not all chunk matches, so we are done.
               (else
                (values (suspend (make-done-value (append break-before result))) next-source))))))))))
                                                  

(define (generative generator)
  (delay
    (call-with-current-continuation (lambda (k-main)
                                      (define (suspend val)
                                        (display (list "Suspend called" 'val= val))(newline)
                                        (call-with-current-continuation (lambda (k-reenter)
                                                                          (k-main
                                                                           (cons val
                                                                                 (delay
                                                                                   (call-with-current-continuation (lambda (k-new-main)
                                                                                                                     (set! k-main k-new-main)
                                                                                                                     (k-reenter #f))))))))
                                        )
                                      (display "About to call generator suspend")(newline)
                                      (generator suspend)
                                      (display "Done calling generator suspend")(newline)
                                      (k-main '())))))

(define (find-str-generator pattern str)
  (lambda (suspend)
    (let* ((pat-len (string-length pattern))
           (str-len (string-length str))
           (last-pos (- str-len pat-len))); last position where match can occur
      (do ((i 0 (+ 1 i)))
          ((> i last-pos))              ; a naive algorithm
        (if (string=? pattern (substring str i (+ i pat-len)))
            (suspend i))))))
  

(let ((sentence "Store it in the neighboring harbor"))
  (let ((a (force (generative (find-str-generator "or" sentence)))))
    (pp (list '1     (force (cdr a))))
    (pp (list '2     (force (cdr a))))
    (pp (list '3     (force (cdr a))))))
    

