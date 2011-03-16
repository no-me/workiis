;; Include the directory containing GCHook.class in the classpath.
;; It can be done by specifying -classpath param to JVM or
;; by using SISC's class-path-extension-append!
(class-path-extension-append! (list "file:/home/ysantoso/share/project/scheme/unwind-protect/"))



;; unwind-protect implementation based on Taylor R. Campbell's idea:
;; http://mumble.net/~campbell/blog.txt [2006-05-01 On control brackets and resource release]
;; Thanks also to Scott G. Miller for providing hints on vlk's 
;; accessibility from debugging-native (I wouldn't have thought to look there).

(module unwind-protect
  (unwind-protect *unwind-protect *unwind-protect-unoptimised)

  (import s2j)
  (import debugging-native)


  (define (make-protector-cell protector-proc)
    (java-new (java-class '|GCHook|) (java-wrap protector-proc)))
  (define (deactivate-protector-cell protector-cell)
    ((generic-java-method '|cancel|) protector-cell))
  
  (define-syntax unwind-protect
    (syntax-rules ()
      ((UNWIND-PROTECT form protection0 protection1 ...)
       (*UNWIND-PROTECT (LAMBDA () form)
                        (LAMBDA () protection0 protection1 ...)))))

  (define (*unwind-protect thunk protector)
    (call/ec
     (lambda (k)
       (let* ((protector-cell (make-protector-cell protector))
              (result (thunk)))
         ;; optimisation: if there has been no continuation reification,
         ;; finalise immediately.
         (let ((continuation-reified-p (continuation-vlk k)))
           ;; (display (format "continuation-reified-p=~a\n" continuation-reified-p))
           (if (not continuation-reified-p)
               (begin
                 (deactivate-protector-cell protector-cell)
                 (protector))))
         ;; there is no need to call the identity-procedure like the
         ;; unoptimised implementation below because the above code
         ;; already holds a reference to protector-cell.
         result))))

  (define *IDENTITY-SIDE-EFFECT* #f)
  (define (identity-procedure what)
    (when what
      (set! *IDENTITY-SIDE-EFFECT* what)
      (set! *IDENTITY-SIDE-EFFECT* #f))
    #f)

  ;; *unwind-protect-unoptimised is not actually used.
  ;; it's here to show an alternative way of implementing unwind-protect.
  (define (*unwind-protect-unoptimised thunk protector)
    (let* ((protector-cell (make-protector-cell protector))
           (result (thunk)))
      ;; This ensures that the continuation will hold a reference to
      ;; the cell, so that it will not be garbage-collected until
      ;; after either control returns to this continuation or a
      ;; throw causes this continuation to be discarded and made
      ;; unreachable.
      (identity-procedure protector-cell)
      result)))
