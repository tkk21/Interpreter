;*outline about state*
;state is a list that contains two list
;such that '( (variables) (values))
;for example '( (x y z) (4 8 #f))
;empty state is going to be '( () ())

(load "classParser.scm")

;**************************************************************************************************************************************************************************
;*****abstractions
;**************************************************************************************************************************************************************************

;abstractions for mValue/mBool
(define operator car)
(define operand cadr)
(define leftOperand cadr)
(define rightOperand caddr)

;abstractions to make mState helper calling eaasier
(define variable cadr)
(define assignedVal caddr)
(define condition cadr)
(define then caddr)
(define else cadddr)
(define body caddr)

(define name cadr)

(define tryblock cadr)
(define catchblock caddr)
(define finallyblock cadddr)

;abstractions for findValue

(define type car)
(define value cadr)

;abstractions for function
(define paramList car)
(define fxnBody cadr)
(define valueList cddr)

;abstractions for class
(define parseExtends caddr)
(define parseClassBody cadddr)


(define classBody caddr)

;abstractions for scope
(define scope car)
(define nextScope cdr)

;abstractions for dot
(define dot caddr)


(define vars car)
(define vals cadr)

(define valueof
  (lambda (expression state classState)
    (cond
      ((eq? 'int (typeof expression state classState)) (mValue expression state classState))
      ((eq? 'boolean (typeof expression state classState)) (mBool expression state classState))
      ((not (pair? expression)) (first (value (findValue expression state)))) ;variable
      ((eq? 'funcall (operator expression)) (functionCall expression state classState))
      ((eq? 'dot (operator expression)) (findDotValue expression state classState))
      (else (error 'valueof "unknown type")))))
      
;*mValue function*
;code outline
;mValue is going to need +,-,*,/,%, and negative sign
;when doing negative sign just do (- 0 expression)
(define mValue
  (lambda (expression state classState)
    (cond
      ((number? expression) expression)
      ((not (pair? expression)) (value(findValue expression state)));this expression is a variable
      ((eq? 'dot (operator expression)) (findDotValue expression state classState))
      ((eq? '+ (operator expression)) (+ (mValue (leftOperand expression) state classState) (mValue (rightOperand expression) state classState)))
      ((eq? '- (operator expression)) (mValueSubtraction expression state))
      ((eq? '* (operator expression)) (* (mValue (leftOperand expression) state classState) (mValue (rightOperand expression) state classState)))
      ((eq? '/ (operator expression)) (quotient (mValue (leftOperand expression) state classState) (mValue (rightOperand expression) state classState)))
      ((eq? '% (operator expression)) (remainder (mValue (leftOperand expression) state classState) (mValue (rightOperand expression) state classState)))
      ((eq? 'funcall (operator expression)) (functionCall expression state classState))
      (else (error 'mValue "illegal operator"))))) 

;refactored out the subtraction function so that you don't have if inside cond
(define mValueSubtraction
  (lambda (expression state)
    (if (pair?(cddr expression))
           (- (mValue (leftOperand expression) state) (mValue (rightOperand expression) state)) ;case for a-b
           (- 0 (mValue (leftOperand expression) state))))) ;case for -a

;*mBool function*
;code outline
;mBool is going to need ==, !=, <, >, <=, >=, and &&, ||, !
;not going to worry about cseases where 3<true
;if we're doing something like x&&true, x better be a boolean or we're not worrying
(define mBool
  (lambda (expression state classState)
    (cond
      ((boolean? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((not (pair? expression)) (value(findValue expression state)));means that the expression is a variable
      ((eq? 'dot (operator expression)) (value (findDotValue expression state classState)))
      ((eq? '== (operator expression)) (mBool== expression state))
      ((eq? '!= (operator expression)) (mBool!= expression state))
      ((eq? '< (operator expression)) (< (mValue (leftOperand expression) state classState) (mValue (rightOperand expression) state classState)))
      ((eq? '> (operator expression)) (> (mValue (leftOperand expression) state classState) (mValue (rightOperand expression) state classState)))
      ((eq? '<= (operator expression)) (<= (mValue (leftOperand expression) state classState) (mValue (rightOperand expression) state classState)))
      ((eq? '>= (operator expression)) (>= (mValue (leftOperand expression) state classState) (mValue (rightOperand expression) state classState)))
      ((eq? '&& (operator expression)) (and (mBool (leftOperand expression) state classState) (mBool (rightOperand expression) state classState)))
      ((eq? '|| (operator expression)) (or (mBool (leftOperand expression) state classState) (mBool (rightOperand expression) state classState)))
      ((eq? '! (operator expression)) (not (mBool (leftOperand expression) state classState)))
      ((eq? 'funcall (operator expression)) (functionCall expression state classState))
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
  (lambda (expression state classState)
    (cond
      ((number? expression) 'int) ;numbers
      ((boolean? expression) 'boolean) ;#t or #f
      ((eq? 'true expression) 'boolean)
      ((eq? 'false expression) 'boolean)
      ((not (pair? expression)) (first (type (findValue expression state)))) ;variable
      ((eq? 'funcall (operator expression)) (typeof (functionCall expression state classState) state classState))
      ((eq? 'dot (operator expression)) (typeof (findDotValue expression state classState) state classState))
      ((isIntOperator? (operator expression)) 'int) ;int expresions
      ((isBooleanOperator? (operator expression)) 'boolean) ;boolean expression
      (else (error 'typeof "unknown type"))))); 'true 'false and boolean expressions

;needed so that we know the only thing that goes inside (if) is a boolean and not an int
(define isIntOperator?
  (lambda (op)
    (member? op '(+ - * / %))))
(define isBooleanOperator?
  (lambda (op)
    (member? op '(== != < > <= >= && || !))))
(define member?
  (lambda (a list)
    (if (empty? list)
        #f
        (or (eq? a (car list)) (member? a (cdr list))))))

;
;*mState functions*
;code outline
;mState is going to need declare, assign, return, and if
;

;the main state method that calls its helper methods
;only processes one line and return state
(define mState
  (lambda (expression state classState return continue break throw)
    (cond
      ((eq? 'static-function (operator expression)) (mStateFunctionDeclare expression state))
      ((eq? 'function (operator expression)) (mStateFunctionDeclare expression state))
      ((eq? 'static-var (operator expression)) (mStateDeclare expression state classState))
      ((eq? 'var (operator expression)) (mStateDeclare expression state classState))
      ((eq? '= (operator expression)) (mStateAssign (variable expression) (assignedVal expression) state classState)) ;eg. x = 5
      
      ((eq? 'return (operator expression)) (mStateReturn (operand expression) state classState return))
      ((eq? 'continue (operator expression)) (continue state))
      ((eq? 'break (operator expression)) (break (mStateEndBlock state)))
      ((eq? 'throw (operator expression)) (throw (operand expression)))
      
      ((eq? 'try (operator expression)) (mStateTry expression state classState))
      ((eq? 'if (operator expression)) (mStateIf expression state classState return continue break throw))
      ((eq? 'begin (operator expression)) (mStateBeginBlock (cdr expression) state classState return continue break throw))
      ((eq? 'while (operator expression)) (mStateWhile (condition expression) (body expression) state classState return continue break throw))
      ((eq? 'funcall (operator expression)) (begin (functionCall expression state classState)state)) ;for if a function is just called by itself eg. void method
      (else (error 'mState "illegal operator")))))


(define mStateTry
  (lambda (expression state classState)
    (if (pair? (finallyblock expression))
        (evaluateBody (car (finallyblock expression))
                      (evaluateBody (car (catchblock expression)) (call/cc (lambda (e)
                                 (letrec ((eval (lambda (body state classState)
                                                  (if (null? body)
                                                      state
                                                      (eval (cdr body) (mState (car body) state (lambda(v) v) e classState) classState)))))
                                   (eval (cadr expression) state classState)))) classState) classState)
        (if (pair? (catchblock expression))
            (evaluateBody (car (catchblock expression)) (call/cc (lambda (e)
                                 (letrec ((eval (lambda (body state classState)
                                                  (if (null? body)
                                                      state
                                                      (eval (cdr body) (mState (car body) state (lambda(v) v) e classState) classState)))))
                                   (eval (cadr expression) state classState)))) classState)
            (letrec ((eval (lambda (body state classState)
                                                  (if (null? body)
                                                      state
                                                      (eval (cdr body) (mState (car body) state (lambda(v) v) e classState) classState)))))
                                   (eval (cadr expression) state classState))))))
(define mStateFunctionDeclare
  (lambda (expression state)
    (consPairToState (name expression) (box (cddr expression)) state)))

;class is made of
;1. its name
;2. what it extends
;3. its body
(define classDeclare
  (lambda (expression classState)
    (if (pair? (parseExtends expression)) ;this class extends something
        (cons (list (name expression) (name (parseExtends expression)) (cons (car(mStateGlobal (parseClassBody expression) (emptyState) classState)) (lookupClassBody (name (parseExtends expression)) classState))) classState)
        (cons (list (name expression) 'Object (mStateGlobal (parseClassBody expression) (emptyState) classState)) classState)
        )))
     

;calls the function and returns the value related to return.
;if the function does not return something, then void is returned
(define functionCall
  (lambda (expression state classState)
    (call/cc (lambda (return)
               (if (not(pair? (valueList expression)))
                   (mStateEvaluate (fxnBody (findFunction (name expression) state classState)) (functionScope state) classState return (emptyLambda) (emptyLambda) (emptyLambda))
                   (mStateEvaluate (addParamToBody (paramList(findFunction (name expression) state classState)) ;paramList
                                             (valueList expression) ;valueList
                                             (fxnBody (findFunction (name expression) state classState)) ;body
                                             state classState) ;states for addParamsToBody
                             (functionScope state) classState return (emptyLambda) (emptyLambda) (emptyLambda))))))) ;states for evaluateBody
;function scope is needed so that the variables declared from the state that calls the function does not affect the function's state
;eg. {a = 5; Math.add(1, 4);} where Math.add has the param (a, b).
(define functionScope 
  (lambda (state)
    (cons (emptyBlock) (cdr state))))
(define emptyBlock
  (lambda ()
    '(()())
     ))

;evaluates the body
;after it's done, return the body
(define evaluateBody
  (lambda (body state classState return)
    (letrec ((eval (lambda (body state classState)
                     (if (null? body)
                         state
                         (eval (cdr body) (mState (car body) state classState return (emptyLambda) (emptyLambda) (emptyState)) classState)))))
                 (eval body state classState))))
    
;adds the param of the function into the body
;for easier block evaluation
(define addParamToBody
  (lambda (paramList valueList body state classState)
    (cond
      ((and (null? paramList) (pair? valueList)) (error 'functionCall "inputted more values than there are parameters"))
      ((null? paramList) body) ;done
      (else(addParamToBody (cdr paramList) (cdr valueList) ;paramList, valueList
                           (cons (constructParamAsExpression (car paramList) (valueof (car valueList) state classState)) body);body
                           state classState)))));states
;turns a param name and its value into an expression
(define constructParamAsExpression
  (lambda (param value)
    (cons 'var (cons param (cons value '() )))))

;static versions of declaring and assigning static variables
(define mStateStaticDeclare
  (lambda (expression state classState)
    (cond
      ((eq? (variable expression) 'return) (error 'mStateDeclare "cannot use the token return as variable"))
      ((pair? (cddr expression)) (mStateStaticAssign (variable expression) (assignedVal expression) (mStateInitialize (variable expression) state ) classState));eg. var x = 5
      (else (mStateInitialize (variable expression) state))))) ; eg. var x
(define mStateStaticAssign
  (lambda (var value state classState)
    (cond ;using cond in case we add more types in the future
      ((eq? (typeof value state classState) 'int) (mStateSetBox var (list '(int static) (mValue value state classState)) state))
      ((eq? (typeof value state classState) 'boolean) (mStateSetBox var (list '(boolean static) (mBool value state classState)) state))
      ((not (pair? value)) (mStateSetBox var (list  (list (typeof (value state classState) 'static)) (findDotValue value state classState)) state));if a variable is assigned
      ((eq? (operator value) 'new) (mStateSetBox var (list (operand value) mStateNewInstance value classState)))
      (else (error 'mStateAssign "assigning an invalid type")))))
  
;mState's helper method to do variable declaration
(define mStateDeclare
  (lambda (expression state classState)
    (cond
      ((eq? (variable expression) 'return) (error 'mStateDeclare "cannot use the token return as variable"))
      ((pair? (cddr expression)) (mStateAssign (variable expression) (assignedVal expression) (mStateInitialize (variable expression) state ) classState));eg. var x = 5
      (else (mStateInitialize (variable expression) state))))) ; eg. var x

;don't need cps, just inserting a new var in front of the state
(define mStateInitialize
  (lambda (var state)
    (consPairToState var (box 'null) state)))

;mState's helper method to do variable assignment
;the variable points to a box containing a list that is (type, value)
(define mStateAssign
  (lambda (var value state classState)
    (cond ;using cond in case we add more types in the future
      ((pair? var) (mStateDotAssign var value state classState))
      
      ((eq? (typeof value state classState) 'int) (mStateSetBox var (list '(int nonstatic) (mValue value state classState)) state))
      ((eq? (typeof value state classState) 'boolean) (mStateSetBox var (list '(boolean nonstatic) (mBool value state classState)) state))
      ((not (pair? value)) (mStateSetBox var (list (list (type (findValue value state)) 'nonstatic) (findDotValue value state classState)) state));if a variable is assigned
      ((eq? (operator value) 'new) (mStateSetBox var (list (operand value) mStateNewInstance value classState)))
      (else (error 'mStateAssign "assigning an invalid type")))))
(define mStateDotAssign
  (lambda (var value state classState)
    (cond
      ((eq? 'super (name var)) (mStateSetBox (dot var) (valueWithType value state classState) (cddr state)))
      ((eq? 'this (name var)) (mStateSetBox (dot var) (valueWithType value state classState) state))
      ((and (not(pair? (name var))) (stateIncludes? (cadr var) state) (stateIncludes? (dot var) (findValue (cadr var) state)))
       (mStateSetBox (dot var) (valueWithType value state classState) (findValue (cadr var) state))) ;non-static case or if the static var is within scope
      ((not(pair? (name var))) (mStateSetBox (dot var) (valueWithType value state classState) (lookupClassBody (cadr var) classState)));non-static case
      ((and (stateIncludes? (cadr var) state) (stateIncludes? (dot var) (findValue (cadr var) state)))
       (mStateDotAssign (dot var) (valueWithType value state classState) (findValue (cadr var) state)))
      (else (mStateDotAssign (dot var) (valueWithType value state classState) (lookupClassBody (name var) classState) classState))) ;no need to (value the result) because that is already done ; this one is for nested dots
    ))

(define valueWithType
  (lambda (value state classState)
    (cond
      ((eq? (typeof value state classState) 'int) (list '(int nonstatic) (mValue value state classState)))
      ((eq? (typeof value state classState) 'boolean) (list '(boolean nonstatic) (mBool value state classState)))
      ((not (pair? value)) (list (list (type (findValue value state)) 'nonstatic) (findDotValue value state classState)));if a variable is assigned
      ((eq? (operator value) 'new) (mStateSetBox var (list (operand value) mStateNewInstance value classState)))
      (else (error 'mStateDotAssign "assigning an invalid type")))))
;copy down the static fields
;copy down the function
;declare instance fields
(define mStateNewInstance
  (lambda (expression classState)
    (mStateNewInstanceCreateState (vars(caddr (lookupClass (name expression) classState))) (vals(caddr (lookupClass (name expression) classState))) (emptyBlock) classState)
    ))
(define mStateNewInstanceCreateState
  (lambda (var val state classState)
    (if (eq? 'static (cadr (type val)))
        (mStateNewInstanceCreateState (cdr var) (cdr val) (consPairToState (car var) (car val) state) classState)
        (mStateNewInstanceCreateStata (cdr var) (cdr val) (mStateDeclare (list 'var (car var) (car val)) state classState)))
    ))
    
    
    

(define mStateSetBox-cps
  (lambda (var value state cps)
    (cond
      ((null? state) (error 'mState "assigning a value to an undeclared variable"))
      ((not (pair? (vars(scope state)))) (cps(mStateSetBox-cps var value (nextScope state) (lambda (v) (cps(cons (scope state) v)))))) ; not in this scope
      ((eq? (car (vars (scope state))) var) (begin (set-box! (car(vals(scope state))) value) (cps state))) ;found it
      (else (mStateSetBox-cps var value (nextPair state) (lambda (v) (cps (consPairToState (car(vars(scope state))) (car(vals(scope state))) v)))))))) ;not this one
(define mStateSetBox
  (lambda (var value state)
    (call/cc
     (lambda (break)
       (mStateSetBox-cps var value state break)))))

;mState's helper methods to do if statements
(define mStateIf
  (lambda (expression state classState return continue break throw)
    (if (pair? (cdddr expression)) ;if expression has an else statement
        (mStateIfElse (condition expression) (then expression) (else expression) state classState return continue break throw)
        (if (mBool (condition expression) state)
            (mState (then expression) state classState return continue break throw)
            state))))

(define mStateIfElse
  (lambda (condition then else state classState return continue break throw)
    (if (mBool condition state)
        (mState then state classState return continue break throw) ;just change the state here if I want to do the side effect condition
        (mState else state classState return continue break throw))))
    
;returns the result of the function
;either returns the int value of the function
;or returns the boolean value of the function in form of true/false not #t/#f
(define mStateReturn
  (lambda (expression state classState return)
    (cond
      ((eq? 'int (typeof expression state classState)) (return (mValue expression state classState)));expression is integer expression
      ((eq? 'boolean (typeof expression state classState)) (return (mBool expression state classState)));expression is boolean expression
      ((and (pair? expression) (eq? (car expression) 'funcall)) (return (functionCall expression state classState)));expression is a function call
      (else (error 'mStateReturn "unknown return type")))))

;adds a new layer to state
;if a continue is seen, end the layer premateurly (uses call/cc to continue)
;ends the layer when interpreter is done evaluating lines
(define mStateBeginBlock
  (lambda (expression state classState return continue break throw)
    (mStateEndBlock (call/cc (lambda (cont)
                               (mStateEvaluate  expression (cons (emptyBlock) state) classState return cont break throw))))
    ))
;gets rid of the layer
(define mStateEndBlock
  (lambda (state)
    (cdr state)))

;while loop
;uses call/cc to break
(define mStateWhile
  (lambda (condition body state classState return continue break throw)
    (call/cc (lambda(Break)
    (letrec ((loop (lambda (condition body state classState return continue break throw)
                     (if (mBool condition state)
                         (loop condition body (mState body state classState return continue Break throw) classState return continue Break throw)
                         state))
                   ))
      (loop condition body state classState return continue break throw)
      )))))

;there will be 5 types of dot operations
;1. this.field/method
;2. super.field/method
;3. object.field/method
;treat field/method as pretty much same because stuff works like (funcall (dot A a))

(define findDotValue
  (lambda (expression state classState)
    (value(lookup expression state classState))))

(define lookup
  (lambda (expression state classState)
    (cond
      ((not (eq? 'dot (operator expression))) (findValue expression state)) ;not dot
      ((eq? 'super (name expression)) (findValue (dot expression) (cddr state))) ;go to next state to find parent
      ((eq? 'this (name expression)) (findValue (dot expression) state)) ;is this the right state?
      ((and (not(pair? (name expression))) (stateIncludes? (cadr expression) state) (stateIncludes? (dot expression) (findValue (cadr expression) state))) 
       (findValue (dot expression) (findValue (cadr expression) state)));non-static case or if the static var is within scope
      ((not(pair? (name expression))) (findValue (dot expression) (lookupClassBody (cadr expression) classState))) ;non-static case
      ((and (stateIncludes? (cadr expression) state) (stateIncludes? (dot expression) (findValue (cadr expression) state)))
       (lookup (dot expression) (findValue (cadr expression) state) classState))
      (else (lookup (dot expression) (lookupClassBody (name expression) classState) classState)) ;no need to (value the result) because that is already done ; this one is for nested dots
        )))
(define stateIncludes?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((not(pair? (vars (scope state)))) (stateIncludes? var (nextScope state))) ;var not in current scope
      ((eq? (car (vars (scope state))) var) #t) ; found value
      (else (stateIncludes? var (nextPair state))))))

    
    
(define findFunction
  (lambda (fxn state classState)
    (cond
      ((not (pair? fxn)) (findValue fxn state))
      ((eq? 'super (name fxn)) (findValue (dot fxn) (cddr state))) ;go to next state to find parent
      ((eq? 'this (name fxn)) (findValue (dot fxn) state)) ;is this the right state?
      ((not(pair? (name fxn))) (findValue (dot fxn) state)) ;object case
      (else (findDotValue (dot fxn) (lookupClassBody (name fxn) classState) classState)) ;no need to (value the result) because that is already done
      ;((eq? 'dot (operator fxn)) (findValue (caddr fxn) (lookupClassBody (cadr fxn) classState)))
      
      )))

;finds a value inside the state by using the var to look it up
(define findValue
  (lambda (var state)
    (cond
      ((null? state) (error 'findValue "calling an undeclared variable"))
      ((not(pair? (vars (scope state)))) (findValue var (nextScope state))) ;var not in current scope
      ((and (eq? (car (vars (scope state))) var) (eq? 'null (car (vals (scope state))))) (error 'findValue "using a variable before assigning a value"))
      ((eq? (car (vars (scope state))) var) (unbox(car (vals (scope state))))) ; found value
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

;an outer evaluater 
(define mStateGlobal
  (lambda (lines state classState)
    (if (null? lines)
        state
        (mStateGlobal (cdr lines) (mState (car lines) state classState (emptyLambda) (emptyLambda) (emptyLambda) (emptyLambda)) classState))))

;even though this seems identical to mState, this function is needed because
;mState doesn't evaluate the statements line by line.
;eg. ((var z) (= z 10) (return z))
;would only evaluate (var z) and nothing else
;Thus, a function is needd to chain these calls to mState

;evaluates all the lines, updating state per line
(define mStateEvaluate
  (lambda (lines state classState return continue break throw)
    (if (null? lines)
        state
        (mStateEvaluate (cdr lines) (mState (car lines) state classState return continue break throw) classState return continue break throw))))
;can main call use call/cc or do we have to find it from state
(define mainCall
  (lambda (expression state classState)
    (functionCall expression (cons (emptyBlock) state) classState)
               ;(evaluateBody expression;body
                ;             (cons (emptyBlock) state) classState return)
               ))

(define declareAllClasses
  (lambda (lines classState)
    (if (pair? lines)
        (declareAllClasses (cdr lines) (classDeclare (car lines) classState))
        classState)))

; class is (name parent state)
(define lookupClassBody
  (lambda (className classState)
    (classBody(lookupClass className classState))))

(define lookupClass
  (lambda (className classState)
    (cond
      ((not(pair? classState)) (error 'lookupClass "class not found"))
      ((eq? className (car (car classState))) (car classState))
      (else (lookupClass className (cdr classState))))))

  
(define interpret
  (lambda (filename className)
    (fixBoolean (mainCall '(funcall main) (lookupClassBody className (declareAllClasses (parser filename) '())) (declareAllClasses (parser filename) '())))
    ))
  
(define fixBoolean
  (lambda (val)
    (if (boolean? val)
        (booleanToString val)
        val)))

(define booleanToString
  (lambda (bool)
    (if bool
        'true
        'false)))

(define emptyState
  (lambda()
    (cons (pairToState '(return) (cons(box 'void)'())) '())
    ;'(((return)(null)))
    ))

(define emptyLambda
  (lambda ()
    (lambda (v) v)))
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