#lang scheme/base
(define (visit-doctor)
  (define (doctor-driver-loop name responses)
    (define (reply user-response)
      (define (change-person phrase)
           (many-replace '(
                          (are-change am)
                          (your-change my)
                          (yourself-change myself)
                          (you-change i)
                          (me you)
                          (am are) 
                          (my your)
                          (myself yourself)
                          (i you)
                          (are are-change)
                          (your your-change)
                          (yourself yourself-change)
                          (you you-change)
                          ) phrase))

      (define (qualifier)
        (pick-random '((you seem to think)
                       (you feel that)
                       (why do you believe)
                       (why do you say)
                       (what makes you think that)
                       )))

      (define (qualify)
        (pick-random '((tell me more about your)
                       )))

      (define (words-for-qualify)
        '(parents
          mother
          father
          sister
          brother
          boyfriend
          girlfriend
          co-worker
          boss
          ))

      (define (reference)
        (pick-random '((earlier you said that)
                       )))

      (define (hedge)
        (pick-random '((please go on)
                       (please tell me more about this)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       )))
      
      (cond (((lambda (x) (not (null? x))) (examine_phrase user-response (words-for-qualify)))
             (let ((to-qualify (pick-random (examine_phrase user-response (words-for-qualify)))))
                  (append (qualify) (cons to-qualify null))))
            ((null? responses)
             (case (choose (random) 1 '(10/27 1))
                   ((1) (hedge))
                   (else (append (qualifier) (change-person user-response)))))
            (else
             (case (choose (random) 1 '(1/3 9/10 1))
                   ((1) (hedge))
                   ((2) (append (qualifier) (change-person user-response)))
                   (else (append (reference) (change-person (pick-random responses))))))))

    (newline)
    (print '**)
    (let ((user-response (myread '())))
      (cond ((equal? user-response '(goodbye))
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week))
             (newline)
             (visit-doctor))
            (else (print (reply user-response))
                  (doctor-driver-loop name (cons user-response responses))))))

  (let ((name (ask-patient-name)))
       (cond ((equal? name '(suppertime))
              (print '(It is time to go home)))
             (else (begin 
                     (printf "Hello, ~a!\n" (car name))
                     (print '(what seems to be the trouble?)))
                   (doctor-driver-loop name '())))))

(define (ask-patient-name)
  (print '(next!))
  (print '(who are you?))
  (myread '()))

(define (choose rand numof weights)
  (cond ((< rand (car weights)) numof)
        (else (choose rand (+ numof 1) (cdr weights)))))

(define (search_word word phrase)
        (cond ((null? phrase) null)
              ((equal? word (car phrase)) word)
              (else (search_word word (cdr phrase)))))

(define (examine_phrase phrase ptrn_lst)
        (filter (lambda (x) (not (null? x))) (map (lambda (word) (search_word word phrase)) ptrn_lst)))

(define (replace pattern replacement lst)
  (cond ((null? lst) '())
        ((equal? (car lst) pattern)
         (cons replacement
               (replace pattern replacement (cdr lst))))
        (else
         (cons (car lst)
               (replace pattern replacement (cdr lst))))))

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (many-replace replacement-pairs lst)
        (cond ((null? replacement-pairs) lst)
              (else (let ((pat-rep (car replacement-pairs)))
                      (replace (car pat-rep)
                               (cadr pat-rep)
                               (many-replace (cdr replacement-pairs) lst))))))

(define (myread input) 
  (let ((token (read)))
    (cond ((eof-object? token)
             (newline)
             (reverse input))
           (else
             (myread (cons token input))))))

(visit-doctor)