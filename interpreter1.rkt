;state is a list that contains two list
;such that '( (variables) (values))
;for example '( (x y z) (4 8 #f))
;empty state is going to be '( () ())

;mValue is going to need +,-,*,/,%, and negative sign
;when doing negative sign just do (- 0 expression)
(define mValue
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '- (operator expression)) (- (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '* (operator expression)) (* (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '% (operator expression)) (remainder (mValue (leftOperand expression) state) (mValue (rightOperand expression) state))
      (else (findValue expression state)))))) ;this expression is a variable
(define operator car)
(define leftOperand cadr)
(define rightOperand caddr)

;mBool is going to need ==, !=, <, >, <=, >=, and &&, ||, !
;not going to worry about cases where 3<true
;if we're doing something like x&&true, x better be a boolean or we're not worrying
(define mBool
  (lambda (expression state)
    (cond
      ((boolean? expression) expression)
      ((eq? '== (operator expression)) (eq? (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (mBool (leftOperand expression) state) (mBool (rightOperand expression) state))))
      ((eq? '< (operator expression)) (< (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      ((eq? '> (operator expression)) (> (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      ((eq? '<= (operator expression)) (<= (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      ((eq? '>= (operator expression)) (>= (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      ((eq? '&& (operator expression)) (and (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      ((eq? '|| (operator expression)) (or (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      ((eq? '! (operator expression)) (not (mBool expression state))) ;does this work?
      (else (mValue expression state)) ;means that the expression is a value expression not a boolean expression      
    )))

;mState is going to need declare, assign, return, and if
(define mStateDeclare
  (lambda (var state)
    (pairToState (cons var (vars state)) (cons 0 (vals state))))) ;could use '() to represent uninitialized

(define mStateAssign
  (lambda (var value state)
    (if (pair? (car state))
        (if (eq? (car (vars state)) var)
            (pairToState (cons var (cdr (vars state))) (cons value (cdr (vals state))))
            (mStateAssign var value (nextPair state)))
        (error 'mState "assigning a value to an undeclared variable"))))

;returns the result of the function
;either returns the int value of the function
;or returns the boolean value of the function in form of true/false not #t/#f
(define return 
  (lambda (expression state)
    (cond
      ((isIntOperator? (operator expression)) (mValue expression state))
      (else
       (if (mBool expression state)
           'true
           'false
           )))))

(define isIntOperator?
  (lambda (op)
    (member? op '(+ - * / %))))

(define member?
  (lambda (a list)
    (if (empty? list)
        #f
        (or (eq? a (car list)) (member? a (cdr list))))))
    

;the main state method for evaluating each line
(define mState
  (lambda (expression state)
    (cond
      ((eq? 'var (operator expression) 
           (if (pair? cddr expression)
               (mStateAssign (variable expression) (assignedVal expression) (mStateDeclare (variable expression)))
               (mStateDeclare (variable expression)))))
      ((eq? '= (operator expression))
      )))))
(define variable cadr)
(define assignedVal caddr)

  
       
(define findValue
  (lambda (var state)
    (if (eq? (car (vars state)) var)
        (car (vals state))
        (findValue var (nextPair state)))))

(define pairToState
  (lambda (var value)
    (cons var (cons value '()))))

(define nextPair
  (lambda (state)
    (pairToState (cons (car (vars state)) (cdr (vars state))) (cons (car (vals state)) (cdr (vals state))))))

(define vars car)
(define vals cadr)


;(define interpret
;  (lambda (filename)