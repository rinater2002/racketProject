;Zehavi Perla 326381480
;Rinat Ehrenfreund 213527823

#lang racket

(require racket/string);string operations
(require 2htdp/batch-io);input\output operations

;------------constant variables-----------
(define file "")
(define counter 0)
(define file-name "")


;-------------------------------------------------Pop Function-------------------------------------------------
(define (HandlePopCommand memory_type constant)
  (cond
    [(string=? memory_type "local")(set! file (string-append file  (string-join (list (string-append "@" constant)
                                                                                      "D=A"
                                                                                      "@LCL"
                                                                                      "D=D+M"
                                                                                      "@SP"
                                                                                      "A=M"
                                                                                      "M=D"
                                                                                      "A=A-1"
                                                                                      "D=M"
                                                                                      "@SP"
                                                                                      "A=M"
                                                                                      "A=M"
                                                                                      "M=D"
                                                                                      "@SP"
                                                                                      "M=M-1" "") "\n")))]
    [(string=? memory_type "pointer")(cond
                                       [(string=? constant "0")(set! file (string-append file (string-join(list "@SP"
                                                                                                                "A=M-1"
                                                                                                                "D=M"
                                                                                                                "@THIS"
                                                                                                                "M=D"
                                                                                                                "@SP"
                                                                                                                "M=M-1" "") "\n")))]
                                       [(string=? constant "1")(set! file (string-append file (string-join(list "@SP"
                                                                                                                "A=M-1"
                                                                                                                "D=M"
                                                                                                                "@THAT"
                                                                                                                "M=D"
                                                                                                                "@SP"
                                                                                                                "M=M-1" "") "\n")))])]
    [(string=? memory_type "temp")(set! file (string-append file (string-join (list "@SP"
                                                                                    "A=M-1"
                                                                                    "D=M"
                                                                                    (string-append "@" constant)
                                                                                    "A=A+1"
                                                                                    "A=A+1"
                                                                                    "A=A+1"
                                                                                    "A=A+1"
                                                                                    "A=A+1"
                                                                                    "M=D"
                                                                                    "@SP"
                                                                                    "M=M-1" "") "\n") ))]
    [(string=? memory_type "argument")(set! file (string-append file (string-join (list (string-append "@" constant)
                                                                                        "D=A"
                                                                                        "@ARG"
                                                                                        "D=D+M"
                                                                                        "@SP"
                                                                                        "A=M"
                                                                                        "M=D"
                                                                                        "A=A-1"
                                                                                        "D=M"
                                                                                        "@SP"
                                                                                        "A=M"
                                                                                        "A=M"
                                                                                        "M=D"
                                                                                        "@SP"
                                                                                        "M=M-1" "") "\n")))]
    [(string=? memory_type "this")(set! file (string-append file (string-join (list (string-append "@" constant)
                                                                                    "D=A"
                                                                                    "@THIS"
                                                                                    "D=D+M"
                                                                                    "@SP"
                                                                                    "A=M"
                                                                                    "M=D"
                                                                                    "A=A-1"
                                                                                    "D=M"
                                                                                    "@SP"
                                                                                    "A=M"
                                                                                    "A=M"
                                                                                    "M=D"
                                                                                    "@SP"
                                                                                    "M=M-1" "") "\n")))]
    [(string=? memory_type "that")(set! file (string-append file (string-join (list (string-append "@" constant)
                                                                                    "D=A"
                                                                                    "@THAT"
                                                                                    "D=D+M"
                                                                                    "@SP"
                                                                                    "A=M"
                                                                                    "M=D"
                                                                                    "A=A-1"
                                                                                    "D=M"
                                                                                    "@SP"
                                                                                    "A=M"
                                                                                    "A=M"
                                                                                    "M=D"
                                                                                    "@SP"
                                                                                    "M=M-1" "") "\n")))]
    [(string=? memory_type "static")(set! file (string-append file (string-join(list "@SP"
                                                                                     "A=M-1"
                                                                                     "D=M"
                                                                                     (string-append "@" file-name "." constant )
                                                                                     "M=D"
                                                                                     "@SP"
                                                                                     "M=M-1" "") "\n")))]))


;-------------------------------------------------Push Function-------------------------------------------------
(define (HandlePushCommand memory_type constant)
  (cond
    [(string=? memory_type "local")(set! file (string-append file (string-join(list (string-append "@" constant)
                                                                                    "D=A"
                                                                                    "@LCL"
                                                                                    "A=M+D"
                                                                                    "D=M"
                                                                                    "@SP"
                                                                                    "A=M"
                                                                                    "M=D"
                                                                                    "@SP"
                                                                                    "M=M+1" "") "\n")))]
    [(string=? memory_type "constant")(set! file (string-append file  (string-join(list (string-append "@" constant)
                                                                                        "D=A"
                                                                                        "@SP"
                                                                                        "A=M"
                                                                                        "M=D"
                                                                                        "@SP"
                                                                                        "M=M+1" "") "\n")))]
    [(string=? memory_type "pointer")(cond
                                       [(string=? constant "0")(set! file (string-append file (string-join(list "@THIS"
                                                                                                                "D=M"
                                                                                                                "@SP"
                                                                                                                "A=M"
                                                                                                                "M=D"
                                                                                                                "@SP"
                                                                                                                "M=M+1" "") "\n")))]
                                       [(string=? constant "1")(set! file (string-append file (string-join(list "@THAT"
                                                                                                                "D=M"
                                                                                                                "@SP"
                                                                                                                "A=M"
                                                                                                                "M=D"
                                                                                                                "@SP"
                                                                                                                "M=M+1" "" ) "\n")))])]
    [(string=? memory_type "temp")(set! file (string-append file (string-join (list (string-append "@" constant)
                                                                                    "D=A"
                                                                                    "@5"
                                                                                    "A=A+D"
                                                                                    "D=M"
                                                                                    "@SP"
                                                                                    "A=M"
                                                                                    "M=D"
                                                                                    "@SP"
                                                                                    "M=M+1" "") "\n")))]
    [(string=? memory_type "argument")(set! file (string-append file  (string-join(list (string-append "@" constant)
                                                                                        "D=A"
                                                                                        "@ARG"
                                                                                        "A=M+D"
                                                                                        "D=M"
                                                                                        "@SP"
                                                                                        "A=M"
                                                                                        "M=D"
                                                                                        "@SP"
                                                                                        "M=M+1" "") "\n")))]
    [(string=? memory_type "this")(set! file (string-append file (string-join(list (string-append "@" constant)
                                                                                   "D=A"
                                                                                   "@THIS"
                                                                                   "A=M+D"
                                                                                   "D=M"
                                                                                   "@SP"
                                                                                   "A=M"
                                                                                   "M=D"
                                                                                   "@SP"
                                                                                   "M=M+1" "") "\n")))]
    [(string=? memory_type "that")(set! file (string-append file (string-join(list (string-append "@" constant)
                                                                                   "D=A"
                                                                                   "@THAT"
                                                                                   "A=M+D"
                                                                                   "D=M"
                                                                                   "@SP"
                                                                                   "A=M"
                                                                                   "M=D"
                                                                                   "@SP"
                                                                                   "M=M+1" "") "\n")))]
    [(string=? memory_type "static")(set! file (string-append file (string-join(list (string-append "@" file-name "." constant )
                                                                                     "D=M"
                                                                                     "@SP"
                                                                                     "A=M"
                                                                                     "M=D"
                                                                                     "@SP"
                                                                                     "M=M+1" "") "\n") ))]))


;-------------------------------------------------Equal Function-------------------------------------------------
(define (HandleEqCommand)
  (set! file(string-append file (string-join(list "@SP"
                                                  "A=M-1"
                                                  "D=M"
                                                  "A=A-1"
                                                  "D=D-M"
                                                  (string-append "@IF_TRUE" (number->string counter))
                                                  "D;JEQ"
                                                  "D=0"
                                                  "@SP"
                                                  "A=M-1"
                                                  "A=A-1"
                                                  "M=D"
                                                  (string-append "@IF_FALSE" (number->string counter))
                                                  "0;JMP"
                                                  (string-append "(IF_TRUE" (number->string counter) ")")
                                                  "D=-1"
                                                  "@SP"
                                                  "A=M-1"
                                                  "A=A-1"
                                                  "M=D"
                                                  (string-append "(IF_FALSE" (number->string counter) ")")
                                                  "@SP"
                                                  "M=M-1" "") "\n")))
  (set! counter (+ 1 counter)))


;-------------------------------------------------Grater-Then Function-------------------------------------------------
(define (HandleGtCommand)
  (set! file(string-append file (string-join(list "@SP"
                                                  "A=M-1"
                                                  "D=M"
                                                  "A=A-1"
                                                  "D=M-D"
                                                  (string-append "@IF_TRUE" (number->string counter))
                                                  "D;JGT"
                                                  "D=0"
                                                  "@SP"
                                                  "A=M-1"
                                                  "A=A-1"
                                                  "M=D"
                                                  (string-append "@IF_FALSE" (number->string counter))
                                                  "0;JMP"
                                                  (string-append "(IF_TRUE" (number->string counter) ")")
                                                  "D=-1"
                                                  "@SP"
                                                  "A=M-1"
                                                  "A=A-1"
                                                  "M=D"
                                                  (string-append "(IF_FALSE" (number->string counter) ")")
                                                  "@SP"
                                                  "M=M-1" "") "\n")))
  (set! counter (+ 1 counter)))


;-------------------------------------------------Less-Then Function-------------------------------------------------
(define (HandleLtCommand)
  (set! file(string-append file (string-join(list "@SP"
                                                  "A=M-1"
                                                  "D=M"
                                                  "A=A-1"
                                                  "D=M-D"
                                                  (string-append "@IF_TRUE" (number->string counter))
                                                  "D;JLT"
                                                  "D=0"
                                                  "@SP"
                                                  "A=M-1"
                                                  "A=A-1"
                                                  "M=D"
                                                  (string-append "@IF_FALSE" (number->string counter))
                                                  "0;JMP"
                                                  (string-append "(IF_TRUE" (number->string counter) ")")
                                                  "D=-1"
                                                  "@SP"
                                                  "A=M-1"
                                                  "A=A-1"
                                                  "M=D"
                                                  (string-append "(IF_FALSE" (number->string counter) ")")
                                                  "@SP"
                                                  "M=M-1" "") "\n")))
  (set! counter (+ 1 counter)))






;-------------------------------------------------Main Function-------------------------------------------------

(define (Main path_file)
  (define lines (read-csv-file/rows path_file (lambda (x) x))) ;lines = [[line1],[line2],...]
  (for([line lines]) ;line = [line1]
    (define line-list (string-split (list-ref line 0) " ")) ;line-list=[w1,w2,w3]
    (set! file(string-append file "//" (list-ref line 0) "\n")) ;writing to the file all the data that was before plus //line1
    (cond
      [(null? line-list)] ;if the list is empty-do nothing
      [(string=?(list-ref line-list 0) "push")(HandlePushCommand (list-ref line-list 1) (list-ref line-list 2))]
      [(string=?(list-ref line-list 0) "pop")(HandlePopCommand (list-ref line-list 1) (list-ref line-list 2))]
      [(string=?(list-ref line-list 0) "add")(set! file(string-append file (string-join(list "@SP"
                                                                                             "A=M-1"
                                                                                             "D=M"
                                                                                             "A=A-1"
                                                                                             "M=D+M"
                                                                                             "@SP"
                                                                                             "M=M-1" "") "\n")))]
      [(string=?(list-ref line-list 0) "sub")(set! file(string-append file (string-join(list "@SP"
                                                                                             "A=M-1"
                                                                                             "D=M"
                                                                                             "A=A-1"
                                                                                             "M=M-D"
                                                                                             "@SP"
                                                                                             "M=M-1" "") "\n")))]
      [(string=?(list-ref line-list 0) "neg")(set! file(string-append file (string-join(list "@SP"
                                                                                             "A=M-1"
                                                                                             "M=-M" "") "\n")))]
      [(string=?(list-ref line-list 0) "eq")(HandleEqCommand)]
      [(string=?(list-ref line-list 0) "gt")(HandleGtCommand)]
      [(string=?(list-ref line-list 0) "lt")(HandleLtCommand)]
      [(string=?(list-ref line-list 0) "and")(set! file(string-append file (string-join(list "@SP"
                                                                                             "M=M-1"
                                                                                             "A=M"
                                                                                             "D=M"
                                                                                             "A=A-1"
                                                                                             "M=M&D" "") "\n")))]
      [(string=?(list-ref line-list 0) "or")(set! file(string-append file (string-join(list"@SP"
                                                                                           "M=M-1"
                                                                                           "A=M"
                                                                                           "D=M"
                                                                                           "A=A-1"
                                                                                           "M=M|D" "") "\n")))]
      [(string=?(list-ref line-list 0) "not")(set! file(string-append file (string-join(list "@SP"
                                                                                             "A=M-1"
                                                                                             "M=!M" "") "\n" )))])))




(display "Enter directory path:") ;output
(define user-path (read-line (current-input-port))) ;user input
(define files-plist(directory-list user-path)) ;the list of the files in the directory
(define list-path (string-split user-path "\\")) ;split the user's path into a list
(set! file-name (list-ref list-path (-(length list-path) 1))) ;file-name = list-path[length-1]




(for([i files-plist]) ;for each file in the directory
  (define file-list(string-split (path->string i) ".")) ;split each file to [file-name,suffix]
  (list-ref file-list 1) ;checking if the file's suffix == vm
  (when (string=? (list-ref file-list 1) "vm")
     (Main (string-append user-path "\\" (path->string i))))) ;sending the user-path\\file-name.vm to the Main function

(define output-file-path (string-append user-path "\\" file-name ".asm"))
(define output-port (open-output-file output-file-path)) ;creating new file in path "user-path\\file-name.asm"
(display file output-port) ;writing all the file's data into "file-name.asm"
(close-output-port output-port) ;close the file


