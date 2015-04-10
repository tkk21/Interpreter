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
;only processes one line and return state
(define mState
  (lambda (expression state continue break)
    (cond
      ((eq? 'var (operator expression)) (mStateDeclare expression state continue break))
      ((eq? '= (operator expression)) (mStateAssign (variable expression) (assignedVal expression) state continue break)) ;eg. x = 5
      ((eq? 'if (operator expression)) (mStateIf expression state continue break))
      ((eq? 'return (operator expression)) (mStateReturn (cadr expression) state continue break))
      ((eq? 'begin (operator expression)) (mStateBeginBlock (cdr expression) state continue break))
      ((eq? 'while (operator expression)) (mStateWhile (condition expression) (body expression) state))
      ((eq? 'continue (operator expression)) (mStateContinue state continue))
      ((eq? 'break (operator expression)) (mStateBreak state break))
      (else (error 'mState "illegal operator")))))

;abstractions to make mState helper calling eaasier
(define variable cadr)
(define assignedVal caddr)
(define condition cadr)
(define then caddr)
(define else cadddr)
(define body caddr)
    
;mState's helper method to do variable declaration
(define mStateDeclare
  (lambda (expression state continue break)
    (cond
      ((eq? (variable expression) 'return) (error 'mStateDeclare "cannot use the token return as variable"))
      ((pair? (cddr expression)) (mStateAssign (variable expression) (assignedVal expression) (mStateInitialize (variable expression) state continue break) continue break));eg. var x = 5
      (else (mStateInitialize (variable expression) state continue break))))) ; eg. var x

;don't need cps, just inserting a new var in front of the state
;might need cps, will do later
(define mStateInitialize
  (lambda (var state continue break)
    (consPairToState var 'null state)))

;mState's helper method to do variable assignment
(define mStateAssign
  (lambda (var value state continue break)
    (cond ;using cond in case we add more types in the future
      ((eq? (typeof value state) 'int) (mStateStoreValue var (mValue value state) state))
      ((eq? (typeof value state) 'boolean) (mStateStoreValue var (mBool value state) state))
      (else (error 'mStateAssign "assigning an invalid type")))))

;goes through all the scopes to find the value to store in
(define mStateStoreValue-cps
  (lambda (var value state return)
    (cond
      ((null? state) (error 'mState "assigning a value to an undeclared variable"))
      ((not (pair? (vars(scope state)))) (return (mStateStoreValue-cps var value (nextScope state) (lambda (v) (cons (scope state) v)))))
      ((eq? (car (vars (scope state))) var) (return (consPairToState var value (nextPair state))))
      (else (mStateStoreValue-cps var value (nextPair state) (lambda (v) (return (consPairToState (car(vars(scope state))) (car(vals(scope state))) v))))))))
;version of mStateStoreValue that has lambda (v) v in it already
(define mStateStoreValue
  (lambda (var value state)
    (mStateStoreValue-cps var value state (lambda (v) v))))
;abstractions for scope
(define scope car)
(define nextScope cdr)

;mState's helper methods to do if statements
(define mStateIf
  (lambda (expression state continue break)
    (if (pair? (cdddr expression)) ;if expression has an else statement
        (mStateIfElse (condition expression) (then expression) (else expression) state continue break)
        (if (mBool (condition expression) state)
            (mState (then expression) state continue break)
            state))))

(define mStateIfElse
  (lambda (condition then else state continue break)
    (if (mBool condition state)
        (mState then state continue break) ;just change the state here if I want to do the side effect condition
        (mState else state continue break))))
    
;returns the result of the function
;either returns the int value of the function
;or returns the boolean value of the function in form of true/false not #t/#f
(define mStateReturn
  (lambda (expression state continue break)
    (cond
      ((eq? (typeof expression state) 'int) (mStateAssign 'return (mValue expression state) state continue break))
      ((mBool expression state) (mStateAssign 'return 'true state continue break))
      (else (mStateAssign 'return 'false state continue break)))))

;adds a new layer to state
;if a continue is seen, end the layer premateurly (uses call/cc to continue)
;ends the layer when interpreter is done evaluating lines
(define mStateBeginBlock
  (lambda (expression state continue break)
    (mStateEndBlock (call/cc (lambda (continue)
                               (mStateEvaluate  expression (cons '(() ()) state) continue break))))
    ))
;gets rid of the layer
(define mStateEndBlock
  (lambda (state)
    (cdr state)))

;while loop
;uses call/cc to break
(define mStateWhile
  (lambda (condition body state)
    (call/cc (lambda(break)
    (letrec ((loop (lambda (condition body state)
                     (if (mBool condition state)
                         (loop condition body (mState body state (lambda (v) v) break) )
                         state))
                   ))
      (loop condition body state)
      )))))

;stop and go back to beginning of the loop
(define mStateContinue
  (lambda (state continue)
    (continue state)))

;stop and exit the loop
(define mStateBreak
  (lambda (state break)
    (break (mStateEndBlock state)) ))

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
      ((null? state) (error 'findValue "calling an undeclared variable"))
      ((not(pair? (vars (scope state)))) (findValue var (nextScope state))) ;var not in current scope
      ((and (eq? (car (vars (scope state))) var) (eq? 'null (car (vals (scope state))))) (error 'findValue "using a variable before assigning a value"))
      ((eq? (car (vars (scope state))) var) (car (vals (scope state))))
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

;evaluates all the lines, updating state per line
(define mStateEvaluate
  (lambda (lines state continue break)
    (if (null? lines)
        state
        (mStateEvaluate (cdr lines) (mState (car lines) state  continue break) continue break))))
    
(define interpret
  (lambda (filename)
    (cond
      ((eq? (findValue 'return (mStateEvaluate (parser filename) (emptyState) (lambda (v) v) (lambda (v) v))) #t) 'true)
      ((eq? (findValue 'return (mStateEvaluate (parser filename) (emptyState) (lambda (v) v) (lambda (v) v))) #f) 'false)
      (else (findValue 'return (mStateEvaluate (parser filename) (emptyState) (lambda (v) v) (lambda (v) v)))))))

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