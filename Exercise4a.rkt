;zehavi perla 326381480
;rinat ehrenfreund 213527823

#lang racket/base
(require racket/string)
(require 2htdp/batch-io)

(define output-file-path "")
(define command "")
(define symbol-list (list "{" "}" "(" ")" "[" "]" "." "," ";" "=" "-" "*" "+" "/" "&" "|" "<" ">" "~"))
(define keywors-list (list "class" "constructor" "function" "method" "field" "static" "var" "int" "char"
                           "boolean" "void" "true" "false" "null" "this" "let" "do" "if" "else" "while" "return"))
(define line-word '())

;----------------------------------------------check-symbol Function----------------------------------------------------

(define (check-symbol line)
  (define str "")
  (define list-char (string-split line ""))
  (define flag #f)
  (for([c list-char])
      (cond
        [(string=? c "\"")(cond ;if c is a start of a string
           [(equal? flag #f)(set! flag #t)(set! str "%")] ; str = "%
           [(equal? flag #t)(set! flag #f)(list-to-tokens str)(set! str "")])]; if c = " -> the end of the string
        [(equal? flag #t)(set! str (string-append str c))];while flag = true -> add c to str
        [(is-member? c symbol-list)(cond
                                  [(equal? str "")(list-to-tokens c)]
                                  [(equal? str " ")(list-to-tokens c)(set! str "")]
                                  [else(list-to-tokens str)(set! str "")(list-to-tokens c)])];for emample: main() | str = main   c = (
        [(equal? c " ")(list-to-tokens str)(set! str "")]
        [(not(equal? c ""))(set! str (string-append str c))])))

;----------------------------------------------is-member? Function----------------------------------------------------

(define (is-member? word global-list)
  (for/or([w (in-list global-list)])
    (string=? w word)))


;---------------------------------------------list-to-tokens Function---------------------------------------------------
(define (list-to-tokens word)
                          (cond
                            [(equal? word "")]
                            [(is-member? word keywors-list)(set! command (string-append command "<keyword> " word " </keyword>\n"))]  
                            [(is-member? word symbol-list)(cond
                                                           [(string=? word "<")(set! command (string-append  command "<symbol> " "&lt;" " </symbol>\n"))]
                                                           [(string=? word ">")(set! command (string-append  command "<symbol> " "&gt;" " </symbol>\n"))]
                                                           [(string=? word '"")(set! command (string-append  command "<symbol> " "&quet;" " </symbol>\n"))]
                                                           [(string=? word "&")(set! command (string-append  command "<symbol> " "&amp;" " </symbol>\n"))]
                                                           [else(set! command (string-append  command "<symbol> " word " </symbol>\n"))]  )]
                            [(integer? (string->number word))(set! command (string-append command "<integerConstant> " word " </integerConstant>\n"))]
                            [(string-prefix? word "%")(set! command (string-append command"<stringConstant> "  (list-ref (string-split word "%")0) " </stringConstant>\n"))]
                            [else (set! command (string-append  command "<identifier> " word " </identifier>\n"))]))




;-------------------------------------------------Main Function-------------------------------------------------

(define (Main file-name path_file)
  (set! output-file-path (string-append user-path "\\" (list-ref (string-split file-name ".") 0) "T.xml"))
  (define output-port (open-output-file output-file-path)) ;creating new file in path "user-path\\file-nameT.xml"
  (define lines (read-lines path_file));lines = [line1, line2, ....]
  (set! command (string-append command "<tokens>\n"))
  (for([line lines])
    (set! line (string-normalize-spaces line));Remove double spaces
    (cond
      [(string-prefix? line "//")] ;if it's commands
      [(string-prefix? line "/*")] ;if it's commands
      [(string-prefix? line "*")] ;if it's commands
      [else 
       (when (string-contains? line "//")(set! line (list-ref (string-split line "//")0)))
       (check-symbol line)]))
  (set! command (string-append command "</tokens>\n"))
  (display command output-port) ;writing all the file's data into "file-nameT.xml"
  (close-output-port output-port) ;close the file
  (set! command ""));untialaze the command to be empty for the next file



;--------------------------------------------------------------------------------------------------

(display "Enter directory path:") ;output
(define user-path (read-line (current-input-port))) ;user input
(define files-plist(directory-list user-path)) ;the list of the files in the directory
(for([i files-plist]) ;for each file in the directory
  (define file-list(string-split (path->string i) ".")) ;split each file to [file-name,suffix]
  (when (>(length file-list) 1)
    (when (string=? (list-ref file-list 1) "jack") ;checking if the file's suffix == jack
      (Main (path->string i) (string-append user-path "\\" (path->string i)))))) ;sending the file and the user-path\\file-name.jack to the Main function