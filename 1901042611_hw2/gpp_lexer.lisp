(setq num 0)
(setq letter 0)
(setq operator 0)
(defvar all_w "")
(defvar all_i "")
(defvar word "")
(defvar out ())
(defvar Token_ "")
(defvar stream)
(setq flag 0)

(defun lexer-file (filename) ;opens file 
	(with-open-file (stream filename)
    (read_line stream)
	)
  (exit)
)

(defun read_line(stream) ;reads file line by line recursively
  (let ((line (read-line stream 'eof))) 
        (file-word line)
        (if (eq line 'eof) nil (cons line (read_line)))
  )
  (read_line)
)

(defun file-word(line) ;pass the line to the analyzer
  (setq word "")
  (setq num 0)
  (setq letter 0)
  (setq flag 0)
  (setq operator 0)
  (setq word (split-seq-with-space line))
  (cont word)
)

(defun rev (list) ;since push puts last element to the firts of the list, it reverse the list to make it right
  (do ((list list (rest list))
       (reversed '() (list* (first list) reversed)))
      ((endp list) reversed)))

(defun output() ;output list contains the tokens of the given code
  (setq out (rev out))
  (write out)
  (terpri)
  (setq out ())
  (if *args* (read_line stream) (lexer))
)

(defun lexer() ;gets  the user input from consol
  (setq num 0)
  (setq letter 0)
  (setq flag 0)
  (setq operator 0)
  (terpri)
  (format t "> ")
  (setq command_new (read-line)) ;reads next line 
  (setq word "")
  (setq word (split-seq-with-space command_new)) ;splits line string into list word by word
  (cont word)
)

(defun cont(word) ;takes one word at the time
  (setq flag 0)
  (setq Token_ "")
  (setq Token_ (car word))
  (if (< (length word) 1) (output) (char_divider Token_))
  (cont (cdr word)) 
)


(defun split-seq-with-space (string &key (spacep #'spacep)) ;splits the sentence from consol to its words
  (loop :for start = (position-if-not spacep string)
    :then (position-if-not spacep string :start (1+ end))
    :for end = (and start (position-if spacep string :start start))
    :when start :collect (subseq string start end)
    :while end)
)

(defun spacep (c) (char= c #\Space)) ;checks for space character

(defun char_divider(Token_) ;takes one character at the time
  (if (< (length Token_) 1) (Print_ all_w) (lexer_rest (car (coerce Token_ 'list))))
  (char_divider (cdr (coerce Token_ 'list)))
)

(defun lexer_rest(token) ;checks if it is comment
  (if (string-equal token ";") (if (and (not (eq (car (cdr (coerce Token_ 'list))) nil)) (string-equal  (car (cdr (coerce Token_ 'list))) ";")) (COMMENT) (errout)) (char_type token))
)


(defun char_type(token) ;checks the characters type
  (cond
    ((digit-char-p token) (sumInt token))

    ((alpha-char-p token) (sumWord token))

    ((not (equal (is_op (string token)) nil)) (setq operation 1))
  )
)

(defun sumInt(token) ;collects numbers as string
  (cond 
    ((and (not (eq (car (cdr (coerce Token_ 'list))) nil)) (alpha-char-p (car (cdr (coerce Token_ 'list))))) (errout))
    ((not (equal all_w "")) (turn-n-go token))
    ((and (equal token #\0) (equal all_i "")) (errout))
  )
  (setf all_i (concatenate 'string all_i (list token)))
  (setq num 1)
  (char_divider (cdr (coerce Token_ 'list)))
)

(defun turn-n-go(token) ;convert int to char and add to previous string to create identifier
  (setq token (coerce token 'character) )
  (sumWord token)
)

(defun is_kw(all_w) ;checks if the word is keyword
  (setq a (assoc all_w (token-keywords) :test #'equal))
  (coerce a 'list)
  (if (and (eq a nil) (eq flag 0)) (push (list all_w "IDENTIFIER") out) (push a out))  
  (if (eq flag 1) (push (list all_w "VALUESTR") out))
  (setq all_w "") 
  (cont (cdr word))
)

(defun sumWord(token) ;collects chars as string
  (setf all_w (concatenate 'string all_w (list token)))
  (setq letter 1)
  (char_divider (cdr (coerce Token_ 'list)))
)

(defun s-cond(token) ;if entry is like "sum)"
  (setq a (assoc all_w (token-keywords) :test #'equal))
  (coerce a 'list)
  (if (eq a nil) (push (list all_w "IDENTIFIER") out) (push a out))  
  (setq all_w "") 
)

(defun s-cond-sec(token) ;if entry is like "2)"
  (push (list all_i "VALUEI") out)
  (setq all_i "") 
)

(defun is_op(token) ;checks if the char is operator
  (if (string-equal token "\"") (setq flag 1) ())
  (if (not (equal all_w "")) (s-cond token) ())
  (if (not (equal all_i "")) (s-cond-sec token) ())
  (setq o (assoc token (token-op) :test #'equal))
  (coerce o 'list)
  (if (eq o nil) ( ) (push o out)) 
  (if (eq (length Token_) 1) (cont (cdr word)) (char_divider (cdr (coerce Token_ 'list))))
)

(defun is_num(token) ;checks if the word is number   
    (push (list all_i "VALUEI") out)
    (setq all_i "")  
    (cont (cdr word))
)

(defun Print_ (token) ;determines the data type and pass accordingly    
  (cond
    ((and (eq num 1) (eq letter 0)) (is_num token))
    ((or (and (eq num 0) (eq letter 1)) (and (eq num 1) (eq letter 1))) (is_kw token))  
    (errout)  
  ) 
)

(defun errout () ;error message
	(push (list "err" "SYNTAX ERROR") out)
  (output)
)

(defun COMMENT() ;comment message
  (push (list ";;" "COMMENT") out)
  (output)
)

(defun token-keywords () ;keywords
  (pairlis '("and" "or" "not" "equal" "less" "nil" "list" "append" 
              "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false")
		      '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND"
              "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE")))

(defun token-op () ;operations
	(pairlis '("+" "-" "/" "*" "(" ")" "**" "," ";;" "\"")
		      '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_DBLMULT" "OP_COMMA" "COMMENT" "OP_OC")))

  
(defun gppinterpreter (&optional filename) ;starting function
	(if filename (lexer-file filename) (lexer))
)

(if *args* (gppinterpreter (car *args*)) (gppinterpreter)) ;ckecks for user input
