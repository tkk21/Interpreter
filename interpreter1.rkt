;*outline about state*
;state is a list that contains two list
;such that '( (variables) (values))
;for example '( (x y z) (4 8 #f))
;empty state is going to be '( () ())

(load "simpleParser.scm")
      
;*mValue function*
;code outline
;mValue is going to need +,-,*,/,%, and negative sign
;when doing negative sign just do (- 0 expression)
(define mValue
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((not (pair? expression)) (findValue expression state));this expression is a variable
      ((eq? '+ (operator expression)) (+ (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '- (operator expression)) (mValueSubtraction expression state))
      ((eq? '* (operator expression)) (* (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '% (operator expression)) (remainder (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      (else (error 'mValue "illegal operator"))))) 
(define operator car)
(define leftOperand cadr)
(define rightOperand caddr)

;refactored out the subtraction function so that you don't have if inside cond
(define mValueSubtraction
  (lambda (expression state)
    (if (pair?(cddr expression))
           (- (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)) ;case for a-b
           (- 0 (mValue (leftOperand expression) state))))) ;case for -a

;*mBool function*
;code outline
;mBool is going to need ==, !=, <, >, <=, >=, and &&, ||, !
;not going to worry about cases where 3<true
;if we're doing something like x&&true, x better be a boolean or we're not worrying
(define mBool
  (lambda (expression state)
    (cond
      ((boolean? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((not (pair? expression)) (findValue expression state));means that the expression is a variable
      ((eq? '== (operator expression)) (mBool== expression state))
      ((eq? '!= (operator expression)) (mBool!= expression state))
      ((eq? '< (operator expression)) (< (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '> (operator expression)) (> (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '<= (operator expression)) (<= (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '>= (operator expression)) (>= (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((eq? '&& (operator expression)) (and (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      ((eq? '|| (operator expression)) (or (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      ((eq? '! (operator expression)) (not (mBool (leftOperand expression) state)))
      (else (error 'mBool "illegal operator")))))

;helper method of mBool that checks the type of operands to call either mValue or mBool    
(define mBool==
  (lambda (expression state)
    (cond
      ((and(eq? (typeof (leftOperand expression)) 'int) (eq? (typeof (rightOperand expression)) 'int)) (eq? (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((and(eq? (typeof (leftOperand expression)) 'boolean) (eq? (typeof (rightOperand expression)) 'boolean)) (eq? (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      (else (error 'mBool== comparing int with boolean)))))
(define mBool!=
  (lambda (expression state)
    (not (mBool== expression state))))
;finds the type of the expression and returns the type as an atom
(define typeof
  (lambda (expression state)
    (cond
      ((number? (car expression)) 'int) ;numbers
      ((boolean? (car expression)) 'boolean) ;#t or #f
      ((not (pair? (cdr expression))) (typeof (findValue (car expression)))) ;variable
      ((isIntOperator? (operator expression)) 'int) ;int expresions
      (else 'boolean)))); 'true 'false and boolean expressions

;
;*mState functions*
;code outline
;mState is going to need declare, assign, return, and if
;

;the main state method that calls its helper methods
(define mState
  (lambda (expression state)
    (cond
      ((eq? 'var (operator expression)) (mStateDeclare (expression state)))
      ((eq? '= (operator expression)) (mStateAssign (variable expression) (mBool (assignedVal expression) state) state)) ;eg. x = 5
      ((eq? 'if (operator expression)) (mStateIf expression state))
      ((eq? 'return (operator expression)) (return (cadr expression) state))
      (else (error 'mState "illegal operator")))))

;abstractions to make mState helper calling eaasier
(define variable cadr)
(define assignedVal caddr)
(define condition cadr)
(define then caddr)
(define else cadddr)
    
;mState's helper method to do variable declaration
(define mStateDeclare
  (lambda (expression state)
    (if (pair? (cddr expression))
        (mStateAssign (variable expression) (mBool (assignedVal expression) state) (mStateInitialize (variable expression) state)) ;eg. var x = 5
        (mStateInitialize (variable expression) state)))) ; eg. var x
(define mStateInitialize
  (lambda (var state)
    (pairToState (cons var (vars state)) (cons 0 (vals state))))) ;could use '() to represent uninitialized

;mState's helper method to do variable assignment
(define mStateAssign
  (lambda (var value state)
    (cond ;using cond in case we add more types in the future
      ((eq? (typeof value) 'int) (mStateStoreValue (variable expression) (mValue (assignedVal expression) state) state))
      ((eq? (typeof value) 'boolean) (mStateStoreValue (variable expression) (mBool (assignedVal expression) state) state))
      (else (error 'mStateAssign "assigning an invalid type")))))
(define mStateStoreValue
  (lambda (var value state)
    (if (pair? (car state))
        (if (eq? (car (vars state)) var)
            (pairToState (vars state) (cons value (cdr (vals state))))
            (mStateAssign var value (nextPair state)))
        (error 'mState "assigning a value to an undeclared variable"))))

;mState's helper methods to do if statements
(define mStateIf
  (lambda (expression state)
    (if (pair? (cdddr expression)) ;if expression has an else statement
        (mStateIfElse (condition expression) (then expression) (else expression) state)
        (if (mBool (condition expression) state)
            (mState (then expression) state)
            state))))

(define mStateIfElse
  (lambda (condition then else state)
    (if (mBool condition state)
        (mState then state) ;just change the state here if I want to do the side effect condition
        (mState else state))))
    
;returns the result of the function
;either returns the int value of the function
;or returns the boolean value of the function in form of true/false not #t/#f
(define return 
  (lambda (expression state)
    (cond
      ((number? (mBool expression state)) (mBool expression state))
      (else
       (if (mBool expression state)
           'true
           'false)))))

;needed so that we know the only thing that goes inside (if) is a boolean and not an int
(define isIntOperator?
  (lambda (op)
    (member? op '(+ - * / %))))

(define member?
  (lambda (a list)
    (if (empty? list)
        #f
        (or (eq? a (car list)) (member? a (cdr list))))))
  
;finds a value inside the state by using the var to look it up
(define findValue
  (lambda (var state)
    (if (pair? (vars state))
        (if (eq? (car (vars state)) var)
            (car (vals state))
            (findValue var (nextPair state)))
        (error 'findValue "calling an undeclared variable"))))

;helper methods to help navigating state easier
(define pairToState
  (lambda (var value)
    (cons var (cons value '() ))))

(define nextPair
  (lambda (state)
    (pairToState (cdr (vars state)) (cdr (vals state)))))

(define vars car)
(define vals cadr)

(define evaluate
  (lambda (lines state)
    (if (null? lines) 
        state
        (evaluate (cdr lines) (mState (car lines) state)))))

;empty state is going to be '( () ())
(define interpret
  (lambda (filename)
    (evaluate (parser filename) (emptyState) )
    ))

(define emptyState
  (lambda()
    '(()())))
(define test
  (lambda (filename num)
    (eq? (interpret (dotTxt (string-append filename (number->string num)))) (interpret (dotTxt(string-append filename (string-append "_answer" (number->string num))))))))
    
;tests to see if they're correct
(define testBatch
  (lambda (filename num)
    (if (zero? num) 
        #t
        (and
         (test filename num)
         (testBatch filename (- num 1))
     ))))

(define dotTxt
  (lambda (filename)
    (string-append filename ".txt")))