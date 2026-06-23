;zehavi perla 326381480
;rinat ehrenfreund 213527823
#lang racket
(define-syntax while
  (syntax-rules ()
    [(while condition body ...)
     (let loop ()
       (when condition
         body ...
         (loop)))]))
(define nameOfFile "")
(define out "")
(define listlines '())
(define index 4)
;-------------------search-symbol-tables--
(define segment "")
(define offset 0)
(define Type-Var "")
(define (search-symbol-tables varName)
  (let ([found "0"] [symbol-method (list )] [symbol-class (list )])
    (set! symbol-method symbol-method-table)
    (let loop ([i  symbol-method])
      (cond ((empty? i) #t)
            (#t (unless (not (string=? found "0"))
                 (cond ((string=? (car (car i)) varName)
                        (set! found "1")
                        (set! Type-Var (car ( cdr (car i))))
                        (set! segment (caddar i))
                        (set! offset (car (cdr (cdr (cdr (car  i)))))))
                        (#t #t)) (set! symbol-method (cdr symbol-method)) (loop symbol-method)))))
    (cond ((string=? found "0")
           (set! symbol-class symbol-class-table)
             (let loop ([i  symbol-class])
      (cond ((empty? i) #t)
            (#t (unless (not (string=? found "0"))
                 (cond ((string=? (car (car i)) varName)
                        (set! found "1")
                        (set! Type-Var (car ( cdr (car i))))
                        (set! segment (caddar i))
                        (set! offset (car (cdr (cdr (cdr (car  i)))))))
                        (#t #t)) (set! symbol-class (cdr symbol-class)) (loop symbol-class))))))
          (#t #t)))
  )
;-------------------get-all-string-------
(define string-constant "")

(define (get-all-string list)
  (set! string-constant "")
  (map (lambda (x)
         (cond ((not (string=? x "</stringConstant>"))
                (set! string-constant (string-append string-constant (string-append " " x))))
               (#t (unless (string=? string-constant "")
                     (set! string-constant (substring string-constant 1 (string-length string-constant)))))))
       list))

;-------------------stringConstant-to-vm---
(define (stringConstant-to-vm)
  (display (string-append "push constant " (number->string (+ 1 (string-length string-constant)))) out);add 1 for the space at the end of the sentence
  (display "\n" out)
  (display "call String.new 1\n" out)
  (map (lambda(x) (display (string-append (string-append "push constant " (number->string (char->integer x)))  "\n") out)
                  (display "call String.appendChar 2\n" out)) (string->list string-constant))
  (display "push constant 32\n" out );add space
  (display "call String.appendChar 2\n" out)
 )
;-------------------term-----------------
(define (term)
  (set! index(+ index 1));skip <term>
  ;(display (list-ref listlines index))
  (if (string-contains? (list-ref listlines index) "</term>")
       (set! index(+ index 1))
      
  (let ( [ tag (list-ref(string-split(list-ref listlines index))0)]
        [ val-tag (list-ref(string-split(list-ref listlines index))1)]
        [ val-next-tag (string-split(list-ref listlines (+ index 1)))]
        [ varName ""]
        [seg ""]
        [offset-help ""])
    (if (>(length val-next-tag)1)
        (set! val-next-tag (list-ref val-next-tag 1))
        (set! val-next-tag (list-ref val-next-tag 0)))
    (cond ((string=? tag "<integerConstant>") (display (string-append (string-append "push constant "  val-tag) "\n") out)       
                                              (set! index(+ index 1))
                                              );skip <integerConstant> num </integerConstant>
          ((string=? tag "<stringConstant>") (get-all-string (cdr(string-split(list-ref listlines index)))); stringConstant </stringConstant>
                                             (stringConstant-to-vm) ;print stringConstant     
                                             (set! index(+ index 1))) ;skip <stringConstant> str </stringConstant>
          ((string=? val-tag "true") (display "push constant 0\n" out)
                                     (display "not\n" out)              
                                     (set! index(+ index 1)));skip <keyword> keywordConstant </keyword>
          ((string=? val-tag "false") (display "push constant 0\n" out)            
                                     (set! index(+ index 1)));skip <keyword> keywordConstant </keyword> 
          ((string=? val-tag "this") (display "push argument 0\n" out) 
                                     (display "pop pointer 0\n" out)
                                     (display "push pointer 0\n" out) 
                                     (set! index(+ index 1)));skip <keyword> keywordConstant </keyword>
          ((string=? val-tag "null") (display "push constant 0\n" out)            
                                     (set! index(+ index 1)));skip <keyword> keywordConstant </keyword>
          ((string=? val-tag "(")   (set! index(+ index 1));skip <symbol> ( </symbol>
                                    (expression) ;call expression
                                    (set! index(+ index 1)));skip <symbol> ) </symbol>
          ((string=? val-tag "-") (set! index(+ index 1));skip <symbol> unaryOp </symbol>
                                  (term)  
                                  (display "neg\n" out))
          ((string=? val-tag "~") (set! index(+ index 1));skip <symbol> unaryOp </symbol>
                                  (term)
                                  (display "not\n" out))
          ((string=? val-next-tag "[") (set! varName  val-tag)
                                       (search-symbol-tables varName)
                                       (set! seg segment)
                                       (set! offset-help offset)  
                                       (set! index(+ index 1));skip <identifier> varName </identifier>
                                       (set! index(+ index 1));skip <symbol> [ </symbol>
                                       (expression) ;call expression;
                                         (cond ((string=? seg "field") 
                                                (display (string-append "push this " (number->string offset-help)) out)
                                                (display "\n" out))
                                               ((string=? seg "static")
                                                (display (string-append "push static " (number->string offset-help)) out)
                                                (display "\n" out))
                                               ((string=? seg "var")
                                                (display (string-append "push local " (number->string offset-help)) out)
                                                (display "\n" out))
                                               (#t (display (string-append "push argument " (number->string offset-help)) out)
                                                   (display "\n" out)))
                                         (display "add\n" out)
                                         (display "pop pointer 1\n" out)
                                         (display "push that 0\n" out)
                                          (set! index(+ index 1)));skip <symbol> ] </symbol>
          ((or (string=? val-next-tag "(") (string=? val-next-tag ".")) (subroutineCall));call subroutineCall
          (#t (set! varName  (list-ref(string-split(list-ref listlines index))1));case of varName
              (search-symbol-tables varName)
              (set! seg segment)
              (set! offset-help offset)
              (set! index(+ index 1));skip <identifier> varName </identifier>
              (cond ((string=? seg "field") 
                  (display (string-append "push this " (number->string offset-help)) out)
                  (display "\n" out))
                 ((string=? seg "static")
                  (display (string-append "push static " (number->string offset-help)) out)
                  (display "\n" out))
                 ((string=? seg "var")
                  (display (string-append "push local " (number->string offset-help)) out)
                  (display "\n" out))
                 (#t (display (string-append "push argument " (number->string offset-help)) out)
                     (display "\n" out))))
      )
   
  (set! index(+ index 1));skip </term>
    ))
  )
;-------------------expression------------
(define (expression)
  (set! index(+ index 1));skip <expression>
  ;(display (list-ref listlines (+ index 1)))
  (term)
  (if (not(string-contains? (list-ref listlines index) "</expression>"))
  (let ([i (list-ref(string-split(list-ref listlines index))1)]) ;(op term)*
    ;(display i)
    (while (or (string=? i "+") (string=? i "-") (string=? i "*") (string=? i "/") (string=? i "&amp;") (string=? i "|") (string=? i "&lt;") (string=? i "&gt;") (string=? i "="))  
    (set! index(+ index 1));skip <symbol> op </symbol>
    ;(display (list-ref listlines index ))
    (term) 
    (cond ((string=? i "+") (display "add\n" out))
          ((string=? i "-") (display "sub\n" out))
          ((string=? i "*") (display "call Math.multiply 2\n" out))
          ((string=? i "/") (display "call Math.divide 2\n" out))
          ((string=? i "&amp;") (display "and\n" out))
          ((string=? i "|") (display "or\n" out))
          ((string=? i "&lt;") (display "lt\n" out))
          ((string=? i "&gt;") (display "gt\n" out))
          ((string=? i "=") (display "eq\n" out))
          (#t #t))
    (if (>(length (string-split(list-ref listlines index)))1)
          (set! i (list-ref(string-split(list-ref listlines index))1))
          (set! i (list-ref(string-split(list-ref listlines index))0)))
    
    )
   )
  1)
  (set! index(+ index 1));skip </expression>
  )
;-------------------expressionList---------
(define num-arg-in-func 0)
(define (expressionList)   
 (set! num-arg-in-func 0)
 (set! index(+ index 1));skip <expressionList>
 (if(string-contains? (list-ref listlines index) "<expression>")
    (begin
      (expression)
      (set! num-arg-in-func 1)
    )
    1)
 (while (string-contains? (list-ref listlines index) ",")
        (set! index(+ index 1));skip <symbol> , </symbol>
        (expression)
        (set! num-arg-in-func (+ 1 num-arg-in-func))
        )
 (set! index(+ index 1));skip </expressionList>
)
;-------------------subroutineCall---------
(define (subroutineCall)
  (let ( [ val-next-tag (list-ref(string-split(list-ref listlines (+ index 1)))1)]
        [ var-or-class-Name ""]
        [seg ""]
        [offset-help ""]
        [sub-name ""]
        [type-class-Var ""])
   (set! segment "")
   (set! offset 0)
   (set! Type-Var "")
   (cond((string=? val-next-tag "(") (set! sub-name (list-ref(string-split(list-ref listlines index))1))
                                      (set! index(+ index 1));skip <identifier> subroutineName </identifier>
                                      (set! index(+ index 1));skip <symbol> ( </symbol>
                                      (display "push pointer 0\n" out)
                                      (expressionList)     ;call expressionList
                                      (set! num-arg-in-func (+ 1 num-arg-in-func))
                                      (display (string-append (string-append (string-append (string-append (string-append "call "   nameOfFile ) "." ) sub-name ) " ") (number->string num-arg-in-func)) out)
                                      (display "\n" out)
                                     (set! index(+ index 1)));skip <symbol> ) </symbol>
         ((string=? val-next-tag ".") (set!  var-or-class-Name (list-ref(string-split(list-ref listlines index))1))
                                      (set! index(+ index 1));skip <identifier> className | varName </identifier>
                                      (search-symbol-tables  var-or-class-Name) 
                                      (set! seg segment)
                                      (set! offset-help offset)
                                      (set! type-class-Var Type-Var)
                                      (set! index(+ index 1));skip <symbol> . </symbol>
                                      (set! sub-name (list-ref(string-split(list-ref listlines index))1))
                                      (set! index(+ index 1));skip <identifier> subroutineName </identifier>
                                      (set! index(+ index 1));skip <symbol> ( </symbol>
  
                                      (cond ((string=? seg "") (expressionList)     ;call expressionList
                                                               (display (string-append (string-append (string-append (string-append (string-append "call "   var-or-class-Name ) "." ) sub-name ) " ") (number->string num-arg-in-func)) out)
                                                               (display "\n" out))
                                            (#t (cond ((string=? seg "field") 
                                                       (display (string-append "push this " (number->string offset-help)) out)
                                                       (display "\n" out))
                                                      ((string=? seg "static")
                                                       (display (string-append "push static " (number->string offset-help)) out)
                                                       (display "\n" out))
                                                      ((string=? seg "var")
                                                       (display (string-append "push local " (number->string offset-help)) out)
                                                       (display "\n" out))
                                                      (#t (display (string-append "push argument " (number->string offset-help)) out)
                                                          (display "\n" out)))
                                                (expressionList)     ;call expressionList
                                                (set! num-arg-in-func (+ 1 num-arg-in-func))
                                                (display (string-append (string-append (string-append (string-append (string-append "call "  type-class-Var ) "." ) sub-name ) " ") (number->string num-arg-in-func)) out)
                                                (display "\n" out)))
                                      (set! index(+ index 1))) ; skip <symbol> ) </symbol>)

      (#t #t))
    )
  )

;-------------------statements-returnStatement
(define this 0)
(define (statements-returnStatement)
 (set! index(+ index 2));skip <returnStatement> and <keyword> return </keyword>
  (set! this 0)
  (if (string-contains? (list-ref listlines index) ";");case of return;
      (display "push constant 0\n" out)
      1)
  
  (if (string-contains? (list-ref listlines (+ index 2)) "this");case of return this;
      (begin
        (display "push pointer 0\n" out)
        (set! this 1)
        (set! index(+ index 5)));skip <expression> and <term> and <keyword> this </keyword> and </term> and </expression>      
      1)
  (if  (and(string-contains? (list-ref listlines index) "<expression>")(= this 0));case of return expresion different from this
           (expression)
           1)
  
  (display "return\n" out);print return
  (set! index(+ index 2));skip  <symbol>  ; </symbol>  and </returnStatement>
  )
;-------------------statements-doStatement--
(define (statements-doStatement)
  (set! index(+ index 2));skip <doStatement> and <keyword> do </keyword>
  (subroutineCall)
  (display "pop temp 0\n" out);print 'pop temp 0'
  (set! index(+ index 2));skip <symbol>  ; </symbol> and </doStatement>
  )
;-------------------statements-whileStatement
(define while-exp 0)
(define while-end 0)
(define (statements-whileStatement)
  (let ([exp 0] [end 0])
    (set! exp while-exp)
    (set! while-exp (+ 1 while-exp ))
    (set! end while-end)
    (set! while-end (+ 1 while-end))
  (set! index(+ index 2));skip <whileStatement> and <keyword> while </keyword>
  (display (string-append (string-append "label WHILE_EXP" (number->string exp)) "\n") out)
  (set! index(+ index 1));skip <symbol>  ( </symbol>  
  (expression)
  (display "not\n" out)
  (display (string-append (string-append "if-goto WHILE_END" (number->string end))  "\n") out)
  (set! index(+ index 1));skip <symbol>  ) </symbol>
  (set! index(+ index 1));skip <symbol>  { </symbol>
  ;(display (list-ref listlines index))
  (set! index(+ index 1));skip <statements> 
  (statements)
  (set! index(+ index 1));skip </statements>
  (set! index(+ index 1));skip <symbol>  } </symbol>
  (display (string-append (string-append "goto WHILE_EXP" (number->string exp)) "\n") out) 
  (display (string-append (string-append "label WHILE_END" (number->string end)) "\n") out)
  (set! index(+ index 1));skip </whileStatement>
  ))
;-------------------statements-ifStatement--

(define if-true 0)
(define if-false 0)
(define if-end 0)
(define (statements-ifStatement)
  (let ([true 0 ] [false 0] [end 0])
    (set! true if-true)
    (set! if-true (+ 1 if-true ))
    (set! false if-false)
    (set! if-false (+ 1 if-false ))
    (set! end if-end)
    (set! if-end (+ 1 if-end))
  (set! index(+ index 3));skip <ifStatement> and <keyword> if </keyword> and <symbol>  ( </symbol>
  (expression)
  (set! index(+ index 1));skip <symbol>  ) </symbol>
  (display (string-append (string-append "if-goto IF_TRUE" (number->string true)) "\n") out) 
  (display (string-append (string-append "goto IF_FALSE" (number->string false)) "\n") out)
  (set! index(+ index 1));skip <symbol>  { </symbol>
  (display (string-append (string-append "label IF_TRUE" (number->string true)) "\n") out)
  (if (string-contains? (list-ref listlines index) "<statements>");call statements
      (begin
      (set! index(+ index 1));skip <statements> 
      (statements)
      (set! index(+ index 1));skip </statements> 
      )
      1)
  (set! index(+ index 1));skip <symbol> } </symbol>

  (display (string-append (string-append "goto IF_END" (number->string end)) "\n") out)
  (display (string-append (string-append "label IF_FALSE" (number->string false)) "\n") out)
  (if (string-contains? (list-ref listlines index) "else");case else
      (begin
        (set! index(+ index 2));skip  <keyword> else </keyword> and <symbol>  { </symbol>
        (set! index(+ index 1));skip <statements> 
        (statements)
        (set! index(+ index 1));skip </statements> 
        (set! index(+ index 1));skip <symbol>  } </symbol>
        
        )
      1)

     (display (string-append (string-append "label IF_END" (number->string end)) "\n") out)

  (set! index(+ index 1));skip </ifStatement>
  ))
;-------------------statements-letStatement--
(define (statements-letStatement)
  (let ([target-varName ""] [flag-arr "0"] [seg ""] [offset-help 0])
  (set! index(+ index 2));skip <letStatement> and <keyword> let </keyword>
  (set! target-varName (list-ref(string-split(list-ref listlines index))1))
  (set! index(+ index 1));skip <identifier> target-varName </identifier>
  (search-symbol-tables target-varName);search in symbol table for the target var
  (set! seg segment);seg has the kind of target var
  (set! offset-help offset); offset-help has the offset of target var 
  (if (string-contains? (list-ref listlines index) "[");case of '[expression]'
      (begin
        (set! flag-arr "1");there is array
        (set! index(+ index 1));skip <symbol>  [ </symbol>
        (expression);call expression
        (if (string=? seg "field")
            (display (string-append (string-append "push this " (number->string offset-help)) "\n") out)
            1)
        (if (string=? seg "static")
            (display (string-append (string-append "push static " (number->string offset-help)) "\n") out)
            1)
        (if (string=? seg "var")
            (display (string-append (string-append "push local " (number->string offset-help)) "\n") out)
            1)
        (if (string=? seg "argument")
            (display (string-append (string-append "push argument " (number->string offset-help)) "\n") out)
            1)
        (display "add\n" out)
        (set! index(+ index 1));skip <symbol>  ] </symbol>
        )
      1)
  (set! index(+ index 1));skip <symbol>  = </symbol>
  (expression)
  (set! index(+ index 1));skip <symbol> ; </symbol>
  (if (string=?  flag-arr "1")
      (begin
        (display "pop temp 0\n" out)
        (display "pop pointer 1\n" out)
        (display "push temp 0\n" out)
        (display "pop that 0\n" out)
        )
      (begin
        (if (string=? seg "field")
          (display (string-append (string-append "pop this " (number->string offset-help)) "\n") out)  
            1)
        (if (string=? seg "static")
          (display (string-append (string-append "pop static " (number->string offset-help)) "\n") out)  
            1)
        (if (string=? seg "var")
          (display (string-append (string-append "pop local " (number->string offset-help)) "\n") out)  
            1)
        (if (string=? seg "argument")
          (display (string-append (string-append "pop argument " (number->string offset-help)) "\n") out)  
            1)
        )
      )
  (set! index(+ index 1));skip </letStatement>
  
 ) )
;-------------------statements---------------
(define (statements)
    (while (or(or(or(or(string-contains? (list-ref listlines index) "<letStatement>")(string-contains? (list-ref listlines index) "<ifStatement>"))(string-contains? (list-ref listlines index) "<whileStatement>"))(string-contains? (list-ref listlines index) "<doStatement>"))(string-contains? (list-ref listlines index) "<returnStatement>"))
           ;(display (list-ref listlines index))
           
           (cond ((string-contains? (list-ref listlines index) "<letStatement>") (statements-letStatement))
                 ((string-contains? (list-ref listlines index) "<ifStatement>") (statements-ifStatement)) 
                 ((string-contains? (list-ref listlines index) "<whileStatement>") (statements-whileStatement))
                 ((string-contains? (list-ref listlines index) "<doStatement>") (statements-doStatement))
                 ((string-contains? (list-ref listlines index) "<returnStatement>") (statements-returnStatement))
                 (#t #t))
           
    )    
 )
;-------------------varDec-------------------
(define (varDec)
  (let ([name ""] [type ""] [kind ""][num 0] [row (list)])
    (while (string-contains? (list-ref listlines index) "<varDec>");end of var declaration
           (set! index (+ index 1))
           (if (string-contains? (list-ref listlines index) "var")
             (begin                             
                 (set! kind "var")
                 (set! index(+ index 1))
                 (set! type (list-ref(string-split(list-ref listlines index))1)); enter type
                 (set! index(+ index 1))
                 (set! name (list-ref(string-split(list-ref listlines index))1));enter name
                 (set! num numVar)
                 (set! numVar(+ numVar 1))
                 (set! row (list name type kind num))
                 (set! row (list row))
                 (set! symbol-method-table (append symbol-method-table row))
                 (set! index(+ index 1))
                 (while(string-contains? (list-ref listlines index) ",");while there is ,
                       (set! index(+ index 1))
                       (set! name (list-ref(string-split(list-ref listlines index))1))
                       (set! num numVar)
                       (set! numVar(+ numVar 1))
                       (set! row (list name type kind num))
                       (set! row (list row))
                       (set! symbol-method-table (append symbol-method-table row))
                       (set! index(+ index 1))
                       )
           )
             1)
         (if (string-contains? (list-ref listlines index) ";")
           (set! index(+ index 1))
           1)
         (if (string-contains? (list-ref listlines index) "</varDec>")
           (set! index(+ index 1))
           1)
         
         )
  ))
;-------------------subroutineBody------------
(define (subroutineBody)
  (set! index(+ index 2));skip <subroutineBody> and <symbol>  { </symbol>
  ;(display (list-ref listlines index))
  (if(string-contains? (list-ref listlines index) "<varDec>")     
     (varDec)
     1)
  (display " " out)
  (display numVar out)
  (display "\n" out)
  (if (string=? typeOfFunc "constructor")
      (begin
      (display "push constant " out)  
      (display numField out)
      (display "\n" out)
      (display "call Memory.alloc 1\n" out)
      (display "pop pointer 0 \n" out)
      )
     1)
  (if (string=? typeOfFunc "method")
      (begin
      (display "push argument 0\n" out)
      (display "pop pointer 0\n" out)
      )
     1)
  (if (string-contains? (list-ref listlines index) "<statements>")
      (begin
        (set! index (+ index 1));skip <statements>
        ;(display (list-ref listlines index))
        (statements)
        (set! index (+ index 1));skip  </statements> 
        )
      1)
  (set! index (+ index 2));skip <symbol>  } </symbol> and </subroutineBody>
 )
;-------------------parameterList-------------
(define (parameterList)
  (let ([name ""] [type ""] [kind ""][num 0] [row (list)])
  (set! index (+ index 1));skip <parameterList>
  (while(not(string-contains? (list-ref listlines index) "/parameterList"))
        (set! kind "argument")
        (set! num numArgument)
        (set! numArgument(+ numArgument 1))
        (set! type (list-ref(string-split(list-ref listlines index))1)); enter type
        (set! index (+ index 1))
        (set! name (list-ref(string-split(list-ref listlines index))1)); enter name
        (set! row (list name type kind num))
        (set! row (list row))
        (set! symbol-method-table (append symbol-method-table row))
        (set! index (+ index 1))
         (if (string-contains? (list-ref listlines index) ",")
           (set! index(+ index 1))
           1)
        )
    (set! index (+ index 1));skip </parameterList>
 ) )
;-------------------subroutineDec-------------
(define numArgument 0)
(define typeOfFunc "")
(define numVar 0)
(define symbol-method-table (list ))
(define nameOfFunc "")
(define (subroutineDec)
  (let ([name ""] [type ""] [kind ""][num 0] [row (list)]) 
  (while(string-contains? (list-ref listlines index) "<subroutineDec>")
        ;initialize counters in the beginning of every function
        (set! numArgument 0)
        (set! numVar 0)
        (set! symbol-method-table (list))
        (set! if-true 0)
        (set! if-false 0)
        (set! if-end 0)
        (set! while-exp 0)
        (set! while-end 0)
        ;----------------------------------
        (set! index (+ index 1));skip <subroutineDec>
        
        (set! typeOfFunc (list-ref(string-split(list-ref listlines index))1))
        
        (if (string=? typeOfFunc "method");entet this to symbol table
            (begin
              (set! name "this")
              (set! type nameOfFile)
              (set! kind "argument")
              (set! num numArgument)
              (set! numArgument(+ numArgument 1))
              (set! row (list name type kind num))
              (set! row (list row))
              (set! symbol-method-table (append symbol-method-table row))
              )
            1)
        (set! index (+ index 2))
        (set! nameOfFunc (list-ref(string-split(list-ref listlines index))1))
        (set! index (+ index 2))
        (if (string-contains? (list-ref listlines index) "<parameterList>")
            (parameterList)
            1)
        (set! nameOfFunc (string-append (string-append "function "(string-append nameOfFile ".") nameOfFunc)))
        (display nameOfFunc out); print function className.funcName
        (set! index (+ index 1));skip <symbol>  ) </symbol>
        (subroutineBody)        
        (set! index (+ index 1));skip </subroutineDec>
        ) 
  
  
 ))
;-------------------classVarDec---------------
(define numStatic 0)
(define numField 0)
(define symbol-class-table (list ))
(define (classVarDec)
  (set! if-true 0)
  (set! if-false 0)
  (set! if-end 0)
  (set! while-exp 0)
  (set! while-end 0)
  (let ([name ""] [type ""] [kind ""][num 0] [row (list)])
    (set! numField 0)
    (set! numStatic 0)
    (set! symbol-class-table (list))
    (while (not(or(string-contains? (list-ref listlines index) "subroutineDec")(string-contains? (list-ref listlines index) "}")));end of var declaration
           (if (string-contains? (list-ref listlines index) "field")
           (begin(set! kind "field")
                 (set! index(+ index 1))
                 (set! type (list-ref(string-split(list-ref listlines index))1)); enter type
                 (set! index(+ index 1))
                 (set! name (list-ref(string-split(list-ref listlines index))1));enter name
                 (set! num numField)
                 (set! numField(+ numField 1))
                 (set! row (list name type kind num))
                 (set! row (list row))
                 (set! symbol-class-table (append symbol-class-table row))
                 (set! index(+ index 1))
                 (while(string-contains? (list-ref listlines index) ",");while there is ,
                       (set! index(+ index 1))
                       (set! name (list-ref(string-split(list-ref listlines index))1))
                       (set! num numField)
                       (set! numField(+ numField 1))
                       (set! row (list name type kind num))
                       (set! row (list row))
                       (set! symbol-class-table (append symbol-class-table row))
                       (set! index(+ index 1))
                       )
           )
           1)
       (if (string-contains? (list-ref listlines index) "static")
           (begin(set! kind "static")
                 (set! index(+ index 1))
                 (set! type (list-ref(string-split(list-ref listlines index))1)); enter type
                 (set! index(+ index 1))
                 (set! name (list-ref(string-split(list-ref listlines index))1));enter name
                 (set! num numStatic)
                 (set! numStatic(+ numStatic 1))
                 (set! row (list name type kind num))
                 (set! row (list row))
                 (set! symbol-class-table (append symbol-class-table row))
                 (set! index(+ index 1))
                 (while(string-contains? (list-ref listlines index) ",");while there is ,
                       (set! index(+ index 1))
                       (set! name (list-ref(string-split(list-ref listlines index))1))
                       (set! num numStatic)
                       (set! numStatic(+ numStatic 1))
                       (set! row (list name type kind num))
                       (set! row (list row))
                       (set! symbol-class-table (append symbol-class-table row))
                       (set! index(+ index 1))
                       )
           )
           1)
       (if (or(string-contains? (list-ref listlines index) ";")(string-contains? (list-ref listlines index) "classVarDec"))
           (set! index(+ index 1))
           1)
           )
    
 ))
;------------------------------main---------------------------------
(display "Enter directory path:") ;output
(define path (read-line)) ; in 'path' there is the path where the jack files are
(define pathsep (map path->string(explode-path (string->path path))));'pathsep' is list with the names of all the directories in the path
(define listfiles (map path->string(directory-list(string->path path))));in 'listfiles' there are the names of all the file in the directory
(for ([x listfiles]);go throgh all files in folder
    (if (and (string-suffix? x ".xml")(not (string-suffix? x "T.xml")));check if file ends with .xml
        (begin
         (set! nameOfFile(list-ref (string-split x ".") 0))
         (set! out (open-output-file (string-append path "\\" nameOfFile ".vm") #:exists 'truncate))
         (set! listlines(file->lines (string-append path "\\" x)));list with all lines in the xml file
        
         (if (string-contains? (list-ref listlines index) "classVarDec")
             (begin
              (set! index(+ index 1))
              (classVarDec);create class symbol table
               )1)
         (if(string-contains? (list-ref listlines index) "subroutineDec")
              (subroutineDec);creat method symbol table
             1)
         (close-output-port out)
         (set! index 4))
        1))