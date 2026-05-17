;zehavi perla 326381480
;rinat ehrenfreund 213527823
#lang racket

(require racket/string);string operations
(require 2htdp/batch-io);input\output operations

;------------constant variables-----------
(define file "")
(define counter 0)
(define  out_file-name "")
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

;-------------------------------------------------call other func Function-------------------------------------------------

(define (HandleCallCommand function_name argumentsNum)
  (define num (+(string->number argumentsNum)5));num = argumentsNum + 5
  (set! file(string-append file(string-join(list (string-append "@" function_name ".ReturnAddress" (number->string counter) )
                                "D=A"
                                "@SP" ;A=0
                                "A=M" ;A=RAM[0] =256
                                "M=D" ;RAM[256] = return addr
                                "@SP" ;A=0
                                "M=M+1" ;RAM[0] = 257
                                (string-append "@" "LCL")
                                "D=M" ;D=RAM[LCL]
                                "@SP" ;A=0
                                "A=M" ;A = 257
                                "M=D" ;RAM[257] = RAM[LCL]
                                "@SP" ;A=0
                                "M=M+1" ;RAM[0] = 258
                                (string-append "@" "ARG")
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"
                                (string-append "@" "THIS")
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"
                                (string-append "@" "THAT")
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"
                                ;***ARG = SP-n-5***
                                "@SP" ;A=0
                                "D=M" ;D = 261
                                (string-append "@" (number->string num)) ;A = num = argumentsNum + 5
                                "D=D-A" ;D = SP-n-5 = 261 - (argumentsNum + 5)
                                "@ARG" 
                                "M=D" ;RAM[ARG] = SP-n-5 = 261 - (argumentsNum + 5)
                                ;***LCL = sp***
                                "@SP" ;A=0
                                "D=M" ;D=RAM[0]=261
                                "@LCL"
                                "M=D" ;RAM[LCL]=SP=261
                                ;***goto g***
                                (string-append "@" function_name )
                                "0;JMP"
                                ;***label return-addr***
                                (string-append "(" function_name ".ReturnAddress" (number->string counter) ")")
                                "")"\n")))
        (set! counter (+ 1 counter)))

;-------------------------------------------------function g k-------------------------------------------------

(define (HandleFunctionCommand  function_name constant)
  
  (set! file(string-append file(string-join(list (string-append "(" function_name ")");label g
                                (string-append "@" constant) ;A=K
                                "D=A" ;D=K
                                (string-append "@" function_name ".END")
                                "D;JEQ" ;if D==0 jump to function_name.END
                                (string-append "(" function_name ".LOOP)")
                                "@SP" ;A=0
                                "A=M" ;A=RAM[0]=256
                                "M=0" ;RAM[256] = 0
                                "@SP" ;A=0
                                "M=M+1" ;RAM[0]=257
                                (string-append "@" function_name ".LOOP")
                                "D=D-1" ;D=K-1
                                "D;JNE" ;if D!=0 jump to function_name.LOOP
                                (string-append "(" function_name ".END)") "") "\n"))))

;-------------------------------------------------return command-------------------------------------------------
(define (HandleReturnCommand)
  (set! file(string-append file(string-join(list
                                            ;--- FRAME = LCL --- (DECLARING TEMP VARIABLE)
                                            "@LCL"
                                            "D=M"
                                            
                                            ;--- RET =*(FRAME-5) --- (ASSIGN RETURN ADDRESS TO TEMP)
                                            "@5"
                                            "A=D-A" ;A=FRAME-5
                                            "D=M" ;D=RAM[FRAME-5] = return addr
                                            "@13"
                                            "M=D" ;RAM[13]= return addr
                           
                                            ;--- *ARG=pop() --- (PLACE OF RETURN ADDRESS TO THE FUNCTION CALL)
                                            "@SP" ;A=0
                                            "M=M-1" ;RAM[0]=257-1=256
                                            "A=M" ;A=256
                                            "D=M" ;D=RAM[256] => D=pop()
                                            "@ARG"
                                            "A=M" ;A=RAM[ARG]
                                            "M=D" ;RAM[ARG] = D=RAM[256] => D=pop()

                                            ;--- SP=ARG+1 --- (NEW PLACE FOR SP)
                                            "@ARG"
                                            "D=M" ;D=RAM[ARG]
                                            "@SP" ;A=0
                                            "M=D+1" ;RAM[0]= D+1 = RAM[ARG]+1

                                            ;--- Restoring the segments ---
                                            "@LCL" "M=M-1" "A=M" "D=M"(string-append "@" "THAT")"M=D"
                                            "@LCL" "M=M-1" "A=M" "D=M"(string-append "@" "THIS")"M=D"
                                            "@LCL" "M=M-1" "A=M" "D=M"(string-append "@" "ARG")"M=D"
                                            "@LCL" "M=M-1" "A=M" "D=M"(string-append "@" "LCL")"M=D"

                                            ;--- Returning control to the calling function ---
                                            "@13"
                                            "A=M" ;A=RAM[13]= return addr
                                            "0;JMP" "") "\n"))))



;-------------------------------------------------sys-init function-------------------------------------------------


(define (sys-init)
  (set! file(string-append file(string-join(list "@256" "D=A" "@SP" "M=D" ;Initializing sp = 256
                                                 (string-append "@Sys.init.ReturnAddress" (number->string counter))
                                                 "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"
                                                 (string-append "@" "LCL") "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"
                                                 (string-append "@" "ARG") "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"
                                                 (string-append "@" "THIS") "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"
                                                 (string-append "@" "THAT") "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"
                                                 "@SP" "D=M" "@5" "D=D-A" "@ARG" "M=D"  "@SP" "D=M" "@LCL" "M=D"
                                                 "@Sys.init"   "0;JMP" (string-append "(Sys.init.ReturnAddress" (number->string counter)")" ) "") "\n")))
        (set! counter (+ 1 counter)))




;-------------------------------------------------Main Function-------------------------------------------------

(define (Main path_file)
  (define lines (read-csv-file/rows path_file (lambda (x) x))) ;lines = [[line1],[line2],...]
  (for([line lines]) ;line = [line1]
    (define line-list (string-split (list-ref line 0) " ")) ;line-list=[w1,w2,w3]
    (set! file(string-append file "//" (list-ref line 0) "\n")) ;writing to the file all the data that was before plus //line1
    (cond
      [(null? line-list)] ;if the list is empty - don't do nothing.
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
                                                                                             "M=!M" "") "\n" )))]
      [(string=?(list-ref line-list 0) "label")(set! file(string-append file "("file-name "." (list-ref line-list 1) ")\n"))]
      [(string=?(list-ref line-list 0) "goto")(set! file(string-append file "@" file-name "." (list-ref line-list 1) "\n" "0;JMP" "\n" ))]
      [(string=?(list-ref line-list 0) "if-goto")(set! file(string-append file (string-join(list "@SP" "M=M-1" "A=M" "D=M" (string-append "@" file-name "." (list-ref line-list 1)) "D;JNE" "") "\n" )))]
      [(string=?(list-ref line-list 0) "call")(HandleCallCommand (list-ref line-list 1) (list-ref line-list 2) )] ;call g n (function g with n arguments)
      [(string=?(list-ref line-list 0) "function")(HandleFunctionCommand (list-ref line-list 1) (list-ref line-list 2))]
      [(string=?(list-ref line-list 0) "return")(HandleReturnCommand)]
      )))

;--------------------------------------------------------------------------------------------------

(define (ismember str files) 
   (for/or ([file (in-list files)]);if there is one file startswith sys - return true
      (string=? (list-ref(string-split (path->string file) ".")0) str) ) )


(display "Enter directory path:") ;output
(define user-path (read-line (current-input-port))) ;user input
(define files-plist(directory-list user-path)) ;the list of the files in the directory
(define list-path (string-split user-path "\\")) ;split the user's path into a list
(set! out_file-name (list-ref list-path (-(length list-path) 1))) ;file-name = list-path[length-1]



(if (ismember "Sys" files-plist)(sys-init)0);if there isn't a file named "sys" in files-plist then call sys-init function
(for([i files-plist]) ;for each file in the directory
  (define file-list(string-split (path->string i) ".")) ;split each file to [file-name,suffix]
  (list-ref file-list 1) ;checking if the file's suffix == vm
  (when (string=? (list-ref file-list 1) "vm")
     (set! file-name (list-ref file-list 0));we wan't the file's name for the labels bacause there is more than one vm file
     (Main (string-append user-path "\\" (path->string i))))) ;sending the user-path\\file-name.vm to the Main function

(define output-file-path (string-append user-path "\\" out_file-name ".asm"))
(define output-port (open-output-file output-file-path)) ;creating new file in path "user-path\\file-name.asm"
(display file output-port) ;writing all the file's data into "file-name.asm"
(close-output-port output-port) ;close the file


