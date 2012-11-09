;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the two commented-out lines at the end of the file (setting up the
;;;; global environment and starting the driver loop).

;;;;**WARNING: Don't load this file twice (or you'll lose the primitives
;;;;  interface, due to renamings of apply).

;;;from section 4.1.4 -- must precede def of metacircular apply
(defun apply-in-underlying-scheme apply)

;;;SECTION 4.1.1

(defun eval-1 (exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval-1 (cond->if exp) env))
        ((application? exp)
         (apply-1 (eval-1 (operator exp) env)
                (list-of-values (operands exp) env)))
        (t 
         (format t  "Unknown expression type ~a EVAL" exp))))

(defun apply-1 (procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (t
         (format t 
          "Unknown procedure type ~a APPLY" procedure))))


(defun list-of-values (exps env)
  (if (no-operands? exps)
      '()
      (cons (eval-1 (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
  (if (true? (eval-1 (if-predicate exp) env))
      (eval-1 (if-consequent exp) env)
      (eval-1 (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (eval-1 (first-exp exps) env))
        (t (eval-1 (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval-1 (assignment-value exp) env)
                       env)
  'ok)

(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
                    (eval-1 (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(defun self-evaluating? (exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (t false)))

(defun quoted? (exp)
  (tagged-list? exp 'quote))

(defun text-of-quotation (exp) (cadr exp))

(defun tagged-list? (exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(defun variable? (exp) (symbol? exp))

(defun assignment? (exp)
  (tagged-list? exp 'set!))

(defun assignment-variable (exp) (cadr exp))

(defun (assignment-value exp) (caddr exp))


(defun (definition? exp)
  (tagged-list? exp 'define))

(defun (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(defun (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(defun (lambda? exp) (tagged-list? exp 'lambda))

(defun (lambda-parameters exp) (cadr exp))
(defun (lambda-body exp) (cddr exp))

(defun (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(defun (if? exp) (tagged-list? exp 'if))

(defun (if-predicate exp) (cadr exp))

(defun (if-consequent exp) (caddr exp))

(defun (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(defun (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(defun (begin? exp) (tagged-list? exp 'begin))

(defun (begin-actions exp) (cdr exp))

(defun (last-exp? seq) (null? (cdr seq)))
(defun (first-exp seq) (car seq))
(defun (rest-exps seq) (cdr seq))

(defun (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(defun (make-begin seq) (cons 'begin seq))


(defun (application? exp) (pair? exp))
(defun (operator exp) (car exp))
(defun (operands exp) (cdr exp))

(defun (no-operands? ops) (null? ops))
(defun (first-operand ops) (car ops))
(defun (rest-operands ops) (cdr ops))


(defun (cond? exp) (tagged-list? exp 'cond))

(defun (cond-clauses exp) (cdr exp))

(defun (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(defun (cond-predicate clause) (car clause))

(defun (cond-actions clause) (cdr clause))

(defun (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(defun (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3

(defun (true? x)
  (not (eq? x false)))

(defun (false? x)
  (eq? x false))


(defun (make-procedure parameters body env)
  (list 'procedure parameters body env))

(defun (compound-procedure? p)
  (tagged-list? p 'procedure))


(defun (procedure-parameters p) (cadr p))
(defun (procedure-body p) (caddr p))
(defun (procedure-environment p) (cadddr p))


(defun (enclosing-environment env) (cdr env))

(defun (first-frame env) (car env))

(defun the-empty-environment '())

(defun (make-frame variables values)
  (cons variables values))

(defun (frame-variables frame) (car frame))
(defun (frame-values frame) (cdr frame))

(defun (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(defun (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(defun (lookup-variable-value var env)
  (defun (env-loop env)
    (defun (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(defun (set-variable-value! var val env)
  (defun (env-loop env)
    (defun (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(defun (define-variable! var val env)
  (let ((frame (first-frame env)))
    (defun (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(defun (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;[do later] (define the-global-environment (setup-environment))

(defun (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(defun (primitive-implementation proc) (cadr proc))

(defun primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
;;      more primitives
        ))

(defun (primitive-procedure-names)
  (map car
       primitive-procedures))

(defun (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;[moved to start of file] (define apply-in-underlying-scheme apply)

(pdefine (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



(defun input-prompt ";;; M-Eval input:")
(defun output-prompt ";;; M-Eval value:")

(defun (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval-1 input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(defun (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(defun (announce-output string)
  (newline) (display string) (newline))

(defun (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;;Following are commented out so as not to be evaluated when
;;; the file is loaded.
;;(define the-global-environment (setup-environment))
;;(driver-loop)

;'METACIRCULAR-EVALUATOR-LOADED
