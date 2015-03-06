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
      ((and(eq? (typeof (leftOperand expression) state) 'int) (eq? (typeof (rightOperand expression) state) 'int)) (eq? (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)))
      ((and(eq? (typeof (leftOperand expression) state) 'boolean) (eq? (typeof (rightOperand expression) state) 'boolean)) (eq? (mBool (leftOperand expression) state) (mBool (rightOperand expression) state)))
      (else (error 'mBool== comparing int with boolean)))))
(define mBool!=
  (lambda (expression state)
    (not (mBool== expression state))))
;finds the type of the expression and returns the type as an atom
(define typeof
  (lambda (expression state)
    (cond
      ((number? expression) 'int) ;numbers
      ((boolean? expression) 'boolean) ;#t or #f
      ((eq? 'true expression) 'boolean)
      ((eq? 'false expression) 'boolean)
      ((not (pair? expression)) (typeof (findValue expression state) state)) ;variable
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
      ((eq? 'var (operator expression)) (mStateDeclare expression state))
      ((eq? '= (operator expression)) (mStateAssign (variable expression) (assignedVal expression) state)) ;eg. x = 5
      ((eq? 'if (operator expression)) (mStateIf expression state))
      ((eq? 'return (operator expression)) (mStateReturn (cadr expression) state))
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
    (cond
      ((eq? (variable expression) 'return) (error 'mStateDeclare "cannot use the token return as variable"))
      ((pair? (cddr expression)) (mStateAssign (variable expression) (assignedVal expression) (mStateInitialize (variable expression) state)));eg. var x = 5
      (else (mStateInitialize (variable expression) state))))) ; eg. var x
(define mStateInitialize
  (lambda (var state)
    (pairToState (cons var (vars state)) (cons 'null (vals state)))))

;mState's helper method to do variable assignment
(define mStateAssign
  (lambda (var value state)
    (cond ;using cond in case we add more types in the future
      ((eq? (typeof value state) 'int) (mStateStoreValue var (mValue value state) state))
      ((eq? (typeof value state) 'boolean) (mStateStoreValue var (mBool value state) state))
      (else (error 'mStateAssign "assigning an invalid type")))))

;goes through all the scopes to find the value to store in
;
(define mStateStoreValue
  (lambda (var value state)
    (cond
      ((null? state) (error 'mState "assigning a value to an undeclared variable"))
      ((not (pair? (scope state))) (cons (scope state) (mStateStoreValue var value (nextScope state) ))) ;not in first scope so look at next scope
      ((eq? (car (vars (scope state))) var) (cons (pairToState (vars (scope state)) (cons value (cdr (vals (scope state))))) (nextScope state)))
      (else (mStateStoreValue var value (nextPair state))))))
(define scope car)
(define nextScope cdr)

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
(define mStateReturn
  (lambda (expression state)
    (cond
      ((eq? (typeof expression state) 'int) (mStateAssign 'return (mValue expression state) state))
      ((mBool expression state) (mStateAssign 'return 'true state))
      (else (mStateAssign 'return 'false state)))))

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
    (cond
      ((not(pair? (vars state))) (error 'findValue "calling an undeclared variable"))
      ((and (eq? (car (vars state)) var) (eq? 'null (car (vals state)))) (error 'findValue "using a variable before assigning a value"))
      ((eq? (car (vars state)) var) (car (vals state)))
      (else (findValue var (nextPair state))))))


;helper methods to help navigating state easier
;turns a pair into a state by itself
;rename this function to pairToScope
(define pairToState
  (lambda (var value)
    (cons var (cons value '() ))))
;helper method that adds a pair in front of the state (in the first scope)
(define consPairToState
  (lambda (var value state)
    (cons (cons (cons var (vars (scope state))) (cons (cons value (vals (scope state))) '())) (nextScope state))))
(define nextPair
  (lambda (state)
    (cons(pairToState (cdr (vars (scope state))) (cdr (vals (scope state)))) (nextScope state) )))

(define vars car)
(define vals cadr)

;even though this seems identical to mState, this function is needed because
;mState doesn't evaluate the statements line by line.
;eg. ((var z) (= z 10) (return z))
;would only evaluate (var z) and nothing else
;Thus, a function is needd to chain these calls to mState
(define evaluate
  (lambda (lines state)
    (if (null? lines)
        state
        (evaluate (cdr lines) (mState (car lines) state)))))
    
(define interpret
  (lambda (filename)
    (cond
      ((eq? (findValue 'return (evaluate (parser filename) (emptyState))) #t) 'true)
      ((eq? (findValue 'return (evaluate (parser filename) (emptyState))) #f) 'false)
      (else (findValue 'return (evaluate (parser filename) (emptyState)))))))

(define emptyState
  (lambda()
    '(((return)(null)))
    ))
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