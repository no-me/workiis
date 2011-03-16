;; Include the directory containing unwind-protect.scm in the classpath.
;; It can be done by specifying -classpath param to JVM or
;; by using SISC's class-path-extension-append!
(class-path-extension-append! (list "file:/home/ysantoso/share/project/scheme/unwind-protect/"))
(require-library 'unwind-protect)
(load "/home/ysantoso/share/project/scheme/unwind-protect/unwind-protect.scm")
(require-library 'sisc/libs/srfi/srfi-18) ;; threading


(module unwind-protect-test
  (do-gc)

  (import s2j)
  (import unwind-protect)
  (import srfi-18) ; threading
  (import srfi-28) ; formatting
  
  (define (do-gc) ((generic-java-method '|gc|) (java-null (java-class '|java.lang.System|))))

  (define (test-log test-num msg)
    (let ((thread-name (or (thread-name (current-thread))
                           "main")))
      (display (format "test-~a: T-~a: ~a\n" test-num thread-name msg))))
  
  ;; simple test. should finalise immediately
  (define (test-1)
    (unwind-protect
     (test-log 1 "body")
     (test-log 1 "finalised"))
    (test-log 1 "Calling GC. Should not trigger finalisation as that is supposed to have already happened")
    (do-gc))

  ;; OUTPUT:
  ;; test-1: T-main: body
  ;; test-1: T-main: finalised
  ;; test-1: T-main: Calling GC. Should not trigger finalisation as that is supposed to have already happened
  

  ;; creates a generator which when evaluated returns (list number next-number-generator).
  ;; number starts from 0 
  ;; if the returned number == max, then '(#f #f) is returned
  (define (generator max)
    (call/cc
     (lambda (return)
       (for-each
        (lambda (x)
          (call/cc
           (lambda (resume)
             (let ((number x)
                   (next-number-generator resume))
               (return (list number next-number-generator))))))
        (iota (+ 1 max)))
       (list #f #f))))
  


  ;; finalisation of a generator (continuation in the body is called multiple times) should
  ;; not occur immediately
  (define (test-2)
    (let ((result (unwind-protect
                   (generator 5)
                   (test-log 2 "finalised"))))
      (if (not (equal? result '(#f #f)))
          (let ((number (car result))
                (next-num-gen (cadr result)))
            (test-log 2 (format "Current number: ~a" number))
            (test-log 2 "Calling GC, should not finalise")
            (do-gc)
            (next-num-gen))))
    (test-log 2 "Calling GC. Should finalise")
    (do-gc))
  
  
  ;; OUTPUT:
  ;; test-2: T-main: Current number: 0
  ;; test-2: T-main: Calling GC, should not finalise
  ;; test-2: T-main: Current number: 1
  ;; test-2: T-main: Calling GC, should not finalise
  ;; test-2: T-main: Current number: 2
  ;; test-2: T-main: Calling GC, should not finalise
  ;; test-2: T-main: Current number: 3
  ;; test-2: T-main: Calling GC, should not finalise
  ;; test-2: T-main: Current number: 4
  ;; test-2: T-main: Calling GC, should not finalise
  ;; test-2: T-main: Current number: 5
  ;; test-2: T-main: Calling GC, should not finalise
  ;; test-2: T-main: Calling GC. Should finalise
  ;; test-2: T-main: finalised



  ;; calling continuation from different threads. i forgot what exactly i wanted to test in the first place...
  (define (test-3)
    (let* ((create-test-thread (lambda (name thunk) (make-thread thunk name)))
           (terminate (lambda () (raise "ignore this")))
           (test-thread (create-test-thread "test-thread-main"
                                            (lambda ()
                                              (with/fc
                                               (lambda (err err-k)
                                                 (print-exception (make-exception err err-k)))
                                               (lambda ()
                                                 (let* ((result (unwind-protect
                                                                 (generator 5)
                                                                 (test-log 3 "finalised"))))
                                                   (if (not (equal? result '(#f #f)))
                                                       (let ((number (car result))
                                                             (next-num-gen (cadr result)))
                                                         (test-log 3 (format "Current number: ~a" number))
                                                         (do-gc)
                                                         (let* ((name (format "test-thread-~a" (number-&gt;string number)))
                                                                (thread (create-test-thread name (lambda() (next-num-gen)))))
                                                           (test-log 3 (format "Starting thread: ~a" name))
                                                           (thread-start! thread)
                                                           (with/fc
                                                            (lambda (err err-k) #f)
                                                            (lambda () (thread-join! thread)))
                                                           (test-log 3 (format "Terminated thread: ~a" name))))))))
                                              (terminate)))))
      (test-log 3 "Starting test-thread")
      (thread-start! test-thread)
      (with/fc
       (lambda (err err-k) #f)
       (lambda () (thread-join! test-thread)))
      (test-log 3 (format "Joined test-thread")))
    (test-log 3 "Calling GC. Should finalise")
    (do-gc))
  
  
  ;; OUTPUT:
  ;; test-3: T-main: Starting test-thread
  ;; test-3: T-test-thread-main: Current number: 0
  ;; test-3: T-test-thread-main: Starting thread: test-thread-0
  ;; test-3: T-test-thread-0: Current number: 1
  ;; test-3: T-test-thread-0: Starting thread: test-thread-1
  ;; test-3: T-test-thread-1: Current number: 2
  ;; test-3: T-test-thread-1: Starting thread: test-thread-2
  ;; test-3: T-test-thread-2: Current number: 3
  ;; test-3: T-test-thread-2: Starting thread: test-thread-3
  ;; test-3: T-test-thread-3: Current number: 4
  ;; test-3: T-test-thread-3: Starting thread: test-thread-4
  ;; test-3: T-test-thread-4: Current number: 5
  ;; test-3: T-test-thread-4: Starting thread: test-thread-5
  ;; test-3: T-test-thread-4: Terminated thread: test-thread-5
  ;; test-3: T-test-thread-3: Terminated thread: test-thread-4
  ;; test-3: T-test-thread-2: Terminated thread: test-thread-3
  ;; test-3: T-test-thread-1: Terminated thread: test-thread-2
  ;; test-3: T-test-thread-0: Terminated thread: test-thread-1
  ;; test-3: T-test-thread-main: Terminated thread: test-thread-0
  ;; test-3: T-main: Joined test-thread
  ;; test-3: T-main: Calling GC. Should finalise
  ;; test-3: T-main: finalised




  (define *IDENTITY-SIDE-EFFECT* #f)
  (define (identity-procedure what)
    (when what
      (set! *IDENTITY-SIDE-EFFECT* what)
      (set! *IDENTITY-SIDE-EFFECT* #f))
    #f)
  
  ;; test that continuation reification is sufficient to hold off call to protector.
  ;; IOW, lack of continuation invocation should not cause protector to be called.
  (define (test-4)
    (let ((k #!void)
          (call-cont-p #t))
      (unwind-protect
       (begin
         (test-log 4 "body-1")
         (call/cc (lambda (x) (set! k x)))
         (test-log 4 "body-2"))
       (test-log 4 "finalised"))
      (test-log 4 "Calling GC, should not finalise")
      (do-gc)
      ;; need to maintain reference to k even though we never invoke it
      ;; otherwise, it will be GC-ed.
      (identity-procedure k))
    (test-log 4 "Calling GC. should finalise")
    (do-gc))

  ;; OUTPUT:
  ;; test-4: T-main: body-1
  ;; test-4: T-main: body-2
  ;; test-4: T-main: Calling GC, should not finalise
  ;; test-4: T-main: Calling GC. should finalise
  ;; test-4: T-main: finalised


  ;; how about failure continuation?
  (define (test-5)
    (let ((fk #!void))
      (unwind-protect
       (with/fc
        (lambda (err err-k)
          (test-log 5 "Error captured, saving failure continuation")
          (set! fk err-k))
        (lambda ()
          (raise "boo!")))
       (test-log 5 "finalised"))
      (test-log 5 "Calling GC, should not finalise")
      (do-gc))
    (test-log 5 "Calling GC, should finalise")
    (do-gc))
  
  
  ;; OUTPUT:
  ;; test-5: T-main: Error captured, saving failure continuation
  ;; test-5: T-main: Calling GC, should not finalise
  ;; test-5: T-main: Calling GC, should finalise
  ;; test-5: T-main: finalised

  
  
  ((lambda ()
     (test-1)
     (thread-sleep! 1) ;; sleep to allow gc to complete its cleanup
     (test-2)
     (thread-sleep! 1)
     (test-3)
     (thread-sleep! 1)
     (test-4)
     (thread-sleep! 1)
     (test-5)
     (thread-sleep! 1)
     )))
                                        




