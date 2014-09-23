#lang scheme/base
(define (visit-doctor)
  (define (doctor-driver-loop name responses)
    (define (reply user-response)
      
;------------------------------- Choosing strategy -------------------------------

      (define (strategies-list)
        (list (list (ptrn-use-strategy-check) 30 ptrn-use-strategy)
              (list (reference-strategy-check) 10 reference-strategy)
              (list (hedge-strategy-check) 30 hedge-strategy)
              (list (qualifier-strategy-check) 30 qualifier-strategy)))

;------------------------------- Strategies-common -------------------------------
      
      ;----------------------------- Predicates -----------------------------------

      (define (ptrn-use-strategy-check)
        ((lambda (lst) (not (null? lst))) (filter (lambda (x) x) (map (lambda (lst) (ptrn-use-check lst)) (pattern-answer)))))
      
      (define (reference-strategy-check)
        (not (null? responses)))

      (define (hedge-strategy-check)
        #t)

      (define (qualifier-strategy-check)
        #t)

      ;-------------------------- Implementations --------------------------------

      (define (ptrn-use-strategy)
        (pick-random (filter (lambda (x) (not (null? x))) (map (lambda (lst) (ptrn-use lst)) (pattern-answer)))))
      
      (define (reference-strategy)
        (append (reference) (change-person (pick-random responses))))

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
                    (list '(when you feel depressed go out for ice cream)
                          '(depression is a disease that can be treated)))
              (list '(mother father parents sister brother friend boyfriend girlfriend co-worker boss)
                    (list '(tell me more about your *)
                          '(why do you feel that way about your * ?)))))
      
      (define (examine_phrase ptrn_lst)
        (filter (lambda (word) (member word user-response)) ptrn_lst))
      
      (define (ptrn-use-check scenario)
        (findf (lambda (word) (member word user-response)) (car scenario)))
      
      (define (ptrn-use scenario)
        (define (form-answer word)
          (replace (car '(*)) word (pick-random (cadr scenario))))
        
        (let ((found (examine_phrase (car scenario))))
          (cond ((not (null? found))
                 (form-answer (pick-random found)))
                (else null))))
      
      ;------------------------- Using previous responses ------------------------
      
      (define (reference)
        (pick-random '((earlier you said that)
                       )))

      ;-------------------------- Qualifier question -----------------------------
      
      (define (change-person phrase)
           (many-replace '(
                          (me you #f)
                          (am are) 
                          (my your)
                          (myself yourself)
                          (i you)
                          ) phrase))

      (define (qualifier)
        (pick-random '((you seem to think)
                       (you feel that)
                       (why do you believe)
                       (why do you say)
                       ; Added
                       (what makes you think that)
                       (you have said that)
                       (how does it show that)
                       )))


;-------------------------------- Choosing strategy ------------------------------
      
      ; TODO - implement common choosing strategy
      
      ;(cond ((ptrn-use-strategy-check)
      ;       (ptrn-use-strategy))
      ;      ((reference-strategy-check)
      ;       (case (choose (random) 1 '(10/27 1))
      ;             ((1) (hedge-strategy))
      ;             (else (qualifier-strategy))))
      ;      (else
      ;       (case (choose (random) 1 '(1/3 9/10 1))
      ;             ((1) (hedge-strategy))
      ;             ((2) (qualifier-strategy))
      ;             (else (reference-strategy)))))
      
      ((pick-random (map (lambda (x) (caddr x)) (filter (lambda (x) (car x)) (strategies-list))))))

;--------------------------------- End of (reply) --------------------------------
;------------------------------- Getting response --------------------------------

    (newline)
    (print '**)
    (let ((user-response (read)))
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
  (read))

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
              ((and (= (length (car replacement-pairs)) 3) (not (list-ref (car replacement-pairs) 2)))
               (let ((pat-rep (car replacement-pairs)))
                      (replace (car pat-rep) (cadr pat-rep) (many-replace (cdr replacement-pairs) lst)))) 
              (else (let ((pat-rep (car replacement-pairs)))
                      (replace * 
                               (cadr pat-rep) 
                               (replace (cadr pat-rep) 
                                        (car pat-rep) 
                                        (replace (car pat-rep) 
                                                 * 
                                                 (many-replace (cdr replacement-pairs) lst))))))))


;---------------------------------- Visit doctor ---------------------------------

(visit-doctor)