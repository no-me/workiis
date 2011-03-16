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
