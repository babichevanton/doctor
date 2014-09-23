#lang scheme/base
(define (visit-doctor)
  (define (doctor-driver-loop name responses)
    (define (reply user-response)
      
;------------------------------- Choosing strategy -------------------------------

      (define (strategies-list)
        (list (list (ptrn-use-strategy-check) 0.3 ptrn-use-strategy)
              (list (reference-strategy-check) 0.1 reference-strategy)
              (list (hedge-strategy-check) 0.3 hedge-strategy)
              (list (qualifier-strategy-check) 0.3 qualifier-strategy)))

;------------------------------- Strategies-common -------------------------------
      
      ;----------------------------- Predicats -----------------------------------

      (define (ptrn-use-strategy-check)
        ((lambda (lst) (not (null? lst))) (filter (lambda (x) x) (map (lambda (lst) (ptrn-use-check lst)) (pattern-answer)))))
      
      (define (reference-strategy-check)
        (> (length responses) 1))

      (define (hedge-strategy-check)
        #t)

      (define (qualifier-strategy-check)
        #t)

      ;-------------------------- Implementations --------------------------------

      (define (ptrn-use-strategy)
        (pick-random (map cadr (filter (lambda (x) (car x)) (map (lambda (lst) (ptrn-use lst)) (pattern-answer))))))
      
      (define (reference-strategy)
        (append (reference) (change-person (pick-random (cdr responses)))))

      (define (hedge-strategy)
        (pick-random '((please go on)
                       (please tell me more about this)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       ; Added
                       (please continue)
                       (i heared this from most my patients)
                       (could you say more about it)
                       )))
            
      (define (qualifier-strategy)
        (append (qualifier) (change-person user-response)))

      
;---------------------------- Strategies-particular ------------------------------
      
      ;----------------------- Using key-words in response -----------------------
      
      (define (pattern-answer)
        (list (list '(depressed suicide)
                    (list (list #f '(when you feel depressed go out for ice cream))
                          (list #f '(depression is a disease that can be treated))))
              (list '(mother father parents syster brother friend boyfriend girlfriend co-worker boss)
                    (list (list #t '(tell me more about your ))
                          (list #t '(why do you feel that way about your ))))))
      
      (define (search_word word phrase)
        (cond ((null? phrase) null)
              ((equal? word (car phrase)) word)
              (else (search_word word (cdr phrase)))))
      
      (define (examine_phrase ptrn_lst)
        (filter (lambda (x) (not (null? x))) (map (lambda (word) (search_word word user-response)) ptrn_lst)))
      
      (define (ptrn-use-check scenario)
        (let ((found (examine_phrase (car scenario))))
          ((lambda (x) (not (null? x))) found)))
      
      (define (ptrn-use scenario)
        (define (form-answer word)
          (let ((answer (pick-random (cadr scenario))))
            (cond ((car answer)
                   (append (cadr answer) (list word)))
                  (else (cadr answer)))))
        
        (let ((found (examine_phrase (car scenario))))
          (cond (((lambda (x) (not (null? x))) found)
                 (list #t (form-answer (pick-random found))))
                (else (list #f)))))
      
      ;------------------------- Using previous responses ------------------------
      
      (define (reference)
        (pick-random '((earlier you said that)
                       )))

      ;-------------------------- Qualifier question -----------------------------
      
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
                       ; Added
                       (what makes you think that)
                       (you have sad that)
                       (how does it show that)
                       )))


;-------------------------------- Choosing strategy ------------------------------
      
      ; TODO - implement common choosing strategy
      (cond ((ptrn-use-strategy-check)
             (ptrn-use-strategy))
            ((reference-strategy-check)
             (case (choose (random) 1 '(10/27 1))
                   ((1) (hedge-strategy))
                   (else (qualifier-strategy))))
            (else
             (case (choose (random) 1 '(1/3 9/10 1))
                   ((1) (hedge-strategy))
                   ((2) (qualifier-strategy))
                   (else (reference-strategy))))))

;--------------------------------- End of (reply) --------------------------------
;------------------------------- Getting response --------------------------------

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

;--------------------------- End of (doctor-driver-loop) -------------------------
;----------------------------- Getting patient name ------------------------------

  (let ((name (ask-patient-name)))
       (cond ((equal? name '(suppertime))
              (print '(It is time to go home)))
             (else (begin 
                     (printf "Hello, ~a!\n" (car name))
                     (print '(what seems to be the trouble?)))
                   (doctor-driver-loop name '())))))

;----------------------------- End of (visit-doctor) -----------------------------

;------------------------------------ Other --------------------------------------

(define (ask-patient-name)
  (print '(next!))
  (print '(who are you?))
  (myread '()))

(define (choose rand numof weights)
  (cond ((< rand (car weights)) numof)
        (else (choose rand (+ numof 1) (cdr weights)))))

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


;---------------------------------- Visit doctor ---------------------------------

(visit-doctor)