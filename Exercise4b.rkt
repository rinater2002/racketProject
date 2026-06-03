;zehavi perla 326381480
;rinat ehrenfreund 213527823

#lang racket/base
(require racket/string)
(require 2htdp/batch-io)
(require dyoo-while-loop)

(define advance 0)
(define line "")
(define file '())
(define command "")
(define spacing "")
(define symbols-for-parse-expression (list "+" "-" "*" "/" "&amp;" "|" "&lt;" "&gt;" "="))
(define output-file-path "")

;------------------------------------------is-member? Function--------------------------------------------------------

(define (is-member? str strs)
  (for/or([s(in-list strs)])
    (string=? s str)))

;---------------------------------------------next-line Function------------------------------------------------

(define (next-line)
  (when (< advance (- (length file) 1))
    (set! advance (+ advance 1))
    (when  (and (< advance (- (length file) 1))(string-contains? (list-ref file advance) "tokens"))
      (set! advance (+ advance 1))))
  (list-ref file advance))
;----------------------------------------------increase-spacing Function----------------------------------------------

(define (increase-spacing)
  (set! spacing (string-append spacing "  ")))
;------------------------------------------------decrease-spacing Function---------------------------------------------

(define (decrease-spacing)
  (set! spacing (string-replace spacing "  " "" #:all? #false)))
;------------------------------------------------write-tag Function--------------------------------------------

(define (write-tag)
  (set! command (string-append command spacing line "\n")))
;------------------------------------------------parse-class-var-dec Function-------------------------------------------

(define (parse-class-var-dec)
  (set! command (string-append command spacing "<classVarDec>\n"))
  (increase-spacing)
  (write-tag);write type
  (set! line (next-line))
  (write-tag);write identifier class
  (set! line (next-line))
  (write-tag);write identifier 
  (set! line (next-line))
  (while (string-contains? line ",")
         (write-tag);write symbol ,
         (set! line (next-line))
         (write-tag);write identifier
         (set! line (next-line)))
  (write-tag);write symbol ;
  (decrease-spacing)
  (set! command (string-append command spacing "</classVarDec>\n")))
;-----------------------------------------------parse-parameters-list Function------------------------------------------

(define (parse-parameters-list)
  (set! command (string-append command spacing "<parameterList>\n"))
  (increase-spacing)
  (set! line (next-line))
  (when (not(string-contains? line ")"))
    (write-tag);write parameter type
    (set! line (next-line))
    (write-tag);write identifier
    (set! line (next-line))
    (while (string-contains? line ",")
           (write-tag);write symbol ,
           (set! line (next-line))
           (write-tag);write parameter type
           (set! line (next-line))
           (write-tag);write identifier
           (set! line (next-line))))
  (decrease-spacing)
  (set! command (string-append command spacing "</parameterList>\n")))
;-------------------------------------------------parse-var-dec Function-------------------------------------------

(define (parse-var-dec)
  (set! command (string-append command spacing "<varDec>\n"))
  (increase-spacing)
  (write-tag);write "var"
  (set! line (next-line))
  (write-tag);write type
  (set! line (next-line))
  (write-tag);write var name
  (set! line (next-line))
  (while (string-contains? line ",")
         (write-tag);write symbol ,
         (set! line (next-line))
         (write-tag);write identifier
         (set! line (next-line)))
  (write-tag);write symbol ;
   (decrease-spacing)
  (set! command (string-append command spacing "</varDec>\n")))
;-------------------------------------------------parse-array-term Function-------------------------------------------

(define (parse-array-term)
   (set! line (next-line))
  (parse-expression)
  (write-tag);write closing braces ]
  )
;--------------------------------------------------parse-expression-list Function-------------------------------------------

(define (parse-expression-list)
  (set! command (string-append command spacing "<expressionList>\n"))
  (increase-spacing)
  (when (not(string-contains? line ")"))
    (parse-expression)
    (while (string-contains? line ",")
         (write-tag);write symbol ,
         (set! line (next-line))
         (parse-expression)))
  (decrease-spacing)
  (set! command (string-append command spacing "</expressionList>\n")))
;---------------------------------------------------parse-sub-routine-call Function--------------------------------------------

(define (parse-sub-routine-call)
  (cond
    [(string-contains? line "(")(set! line (next-line))(parse-expression-list)
                                (write-tag);write closing paranthesis )
                                ]
    [else (set! line (next-line))
          (write-tag);write identifier
          (set! line (next-line))
          (write-tag);write closing paranthesis (
          (set! line (next-line))
          (parse-expression-list)
          (write-tag);write closing paranthesis )
          ]))
;--------------------------------------------------parse-term Function--------------------------------------------

(define (parse-term)
  (set! command (string-append command spacing "<term>\n"))
  (increase-spacing)
  (cond
    [(string-contains? line "integerConstant")(write-tag);write identifier
                                              (set! line (next-line))]
    [(string-contains? line "stringConstant")(write-tag);write identifier
                                             (set! line (next-line))]
    [(string-contains? line "true")(write-tag);write identifier
                                   (set! line (next-line))]
    [(string-contains? line "false") (write-tag);write identifier
                                     (set! line (next-line))]
    [(string-contains? line "null") (write-tag);write identifier
                                    (set! line (next-line))]
    [(string-contains? line "this")(write-tag);write identifier
                                   (set! line (next-line))]
    [(string-contains? line "-")(write-tag);write identifier
                                (set! line (next-line))(parse-term)];notice the recursion
    [(string-contains? line "~")(write-tag);write identifier
                                (set! line (next-line))(parse-term)];notice the recursion
    [(string-contains? line "identifier") (write-tag);write identifier
                                          (set! line (next-line))
                                          (when (string-contains? line "[")
                                            (write-tag);write opening brece [
                                            (parse-array-term)(set! line (next-line)))
                                          (when (or (string-contains? line "(") (string-contains? line "."))
                                            (write-tag);write identifier
                                            (parse-sub-routine-call)(set! line (next-line)))]
    [(string-contains? line "symbol")(write-tag);write opening paranthesis (
                                     (set! line (next-line))(parse-expression)(write-tag);write closing paranthesis )
                                     (set! line (next-line)) ] )
  (decrease-spacing)
  (set! command (string-append command spacing "</term>\n")))
;------------------------------------------------parse-expression Function--------------------------------------------

(define (parse-expression)
  (set! command (string-append command spacing "<expression>\n"))
  (increase-spacing)
  (parse-term);no advance because parse-term needs to do one lookahead
  (define symbol (list-ref (string-split line " ") 1))
  (when (string-contains? line "&amp")
    (set! line line))
  (while (is-member? symbol symbols-for-parse-expression)
    (write-tag);write symbol
    (set! line (next-line))
    (parse-term);no advance because parse-term needs to do one lookahead
    (set! symbol (list-ref (string-split line " ") 1)) )
   (decrease-spacing)
  (set! command (string-append command spacing "</expression>\n")))
;---------------------------------------------------parse-let Function--------------------------------------------

(define (parse-let)
  (set! command (string-append command spacing "<letStatement>\n"))
  (increase-spacing)
  (write-tag);write let
  (set! line (next-line))
  (write-tag);write identifier
  (set! line (next-line))
  (when (string-contains? line "[")
    (write-tag);write opening braces [
    (set! line (next-line))
    (parse-expression)
    (write-tag);write closing braces ]
    (set! line (next-line)) )
  (write-tag);write symbol =
  (set! line (next-line))
  (parse-expression)
  (write-tag);write symbol ;
  (decrease-spacing)
  (set! command (string-append command spacing "</letStatement>\n")))
;--------------------------------------------------parse-if Function--------------------------------------------

(define (parse-if)
  (set! command (string-append command spacing "<ifStatement>\n"))
  (increase-spacing)
  (write-tag);write if
  (set! line (next-line))
  (write-tag);write opening paranthesis (
  (set! line (next-line))
  (parse-expression)
  (write-tag);write closing paranthesis )
  (set! line (next-line))
  (write-tag);write opening brackets {
  (set! line (next-line))
  (parse-statements)
  (write-tag);write closing brackets }
  (set! line (next-line))
  (when (string-contains? line "else")
    (write-tag);write else
    (set! line (next-line))
    (write-tag);write opening brackets {
    (set! line (next-line))
    (parse-statements)
    (write-tag);write closing brackets }
    (set! line (next-line)) )
  (decrease-spacing)
  (set! command (string-append command spacing "</ifStatement>\n")))
;--------------------------------------------------parse-while Function--------------------------------------------

(define (parse-while)
  (set! command (string-append command spacing "<whileStatement>\n"))
  (increase-spacing)
  (write-tag);write while
  (set! line (next-line))
  (write-tag);write opening paranthesis (
  (set! line (next-line))
  (parse-expression)
  (write-tag);write closing paranthesis )
  (set! line (next-line))
  (write-tag);write opening brackets {
  (set! line (next-line))
  (parse-statements)
  (write-tag);write closing brackets }
  (decrease-spacing)
  (set! command (string-append command spacing "</whileStatement>\n")))
;---------------------------------------------------parse-do Function-------------------------------------------

(define (parse-do)
  (set! command (string-append command spacing "<doStatement>\n"))
  (increase-spacing)
  (write-tag);write do
  (set! line (next-line))
  (write-tag);write identifier
  (set! line (next-line))
  (when (or (string-contains? line ".") (string-contains? line "("))
    (write-tag);write symbol . or (
    (parse-sub-routine-call))
  (set! line (next-line))
  (write-tag);write symbol ;
  (decrease-spacing)
  (set! command (string-append command spacing "</doStatement>\n")))
;----------------------------------------------------parse-return Function---------------------------------------------

(define (parse-return)
  (set! command (string-append command spacing "<returnStatement>\n"))
  (increase-spacing)
  (write-tag);write return
  (set! line (next-line))
  (when (not (string-contains? line ";") )
    (parse-expression))
  (write-tag);write symbol ;
  (decrease-spacing)
  (set! command (string-append command spacing "</returnStatement>\n")))
;-----------------------------------------------------parse-statements Function-------------------------------------------

(define (parse-statements)
  (set! command (string-append command spacing "<statements>\n"))
  (increase-spacing)
  (while (string-contains? line "keyword")
         (cond
           [(string-contains? line "let")(parse-let)(set! line (next-line))]
           [(string-contains? line "if") (parse-if)];no advance because parse-if needs to do one lookahead
           [(string-contains? line "while")(parse-while)(set! line (next-line))]
           [(string-contains? line "do")(parse-do)(set! line (next-line))]
           [(string-contains? line "return")(parse-return)(set! line (next-line))]))
  (decrease-spacing)
  (set! command (string-append command spacing "</statements>\n")))
;------------------------------------------------------parse-subroutine-body Function------------------------------------------

(define (parse-subroutine-body)
  (set! command (string-append command spacing "<subroutineBody>\n"))
  (increase-spacing)
  (write-tag);write opening brackets {
  (set! line (next-line))
  (while (string-contains? line "var")
         (parse-var-dec)
         (set! line (next-line)))
  (parse-statements)
  (write-tag);write closing brackets }
  (decrease-spacing)
  (set! command (string-append command spacing "</subroutineBody>\n")))
;-------------------------------------------------------parse-sub-routine Function---------------------------------------

(define (parse-sub-routine)
  (set! command (string-append command spacing "<subroutineDec>\n"))
  (increase-spacing)
  (write-tag);write type
  (set! line (next-line))
  (write-tag);write return type
  (set! line (next-line))
  (write-tag);write identifier 
  (set! line (next-line))
  (write-tag);write opening paranthesis (
  (parse-parameters-list)
  (write-tag);write closing paranthesis )
  (set! line (next-line))
  (parse-subroutine-body);the closing } is matched in parse-subroutine-body
  (decrease-spacing)
  (set! command (string-append command spacing "</subroutineDec>\n")))
;------------------------------------------------------parse-class Function------------------------------------------

(define (parse-class)
  (set! line (next-line))
  (set! command (string-append command spacing "<class>\n"))
  (increase-spacing)
  (write-tag);writing class definition
  (set! line (next-line))
  (write-tag);writing class name
  (set! line (next-line))
  (write-tag);opening brackets {
  (set! line (next-line))
  (while (or (string-contains? line "static") (string-contains? line "field"))
         (parse-class-var-dec)
         (set! line (next-line)))
  (while (or (string-contains? line "constructor") (or (string-contains? line "function") (string-contains? line "method")))
         (parse-sub-routine)
         (set! line (next-line)))
  (write-tag);write closing brackets }
  (decrease-spacing)
  (set! command (string-append command spacing "</class>\n")))
;--------------------------------------------------------Main Function----------------------------------------

(define (Main file-name path)
  (set! output-file-path (string-append user-path "\\" (list-ref (string-split file-name "T.") 0) ".xml"))
  (define output-port (open-output-file output-file-path)) ;creating new file in path "user-path\\file-name.xml"
  (set! file (read-lines path))
  (set! advance 0)
  (set! line "")
  (set! command "")
  (set! spacing "")
  (parse-class)
  (for ([row file])
    (set! row (string-normalize-spaces row)))
  (display command output-port) ;writing all the file's data into "file-nameT.xml"
  (close-output-port output-port) ;close the file
  )
;----------------------------------------------------------- Function---------------------------------------

(display "Enter directory path:") ;output
(define user-path (read-line (current-input-port))) ;user input
(define files-plist(directory-list user-path)) ;the list of the files in the directory
(for([i files-plist]) ;for each file in the directory
  (define file-list(string-split (path->string i) "T.")) ;split each file to [file-name,suffix]
  (when (>(length file-list) 1)
  (when (string=? (list-ref file-list 1) "xml") ;checking if the file's suffix == xml
     (Main (path->string i) (string-append user-path "\\" (path->string i)))))) ;sending the file and the user-path\\file-name.xml to the Main function