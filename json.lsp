; AutoLISP JSON parser and formatter
; (c) 2015, Michel Beloshitsky <mbeloshitsky@gmail.com>
;
; TODO:
;   - Correct numbers handling
;   - Write formatter

(setq json:debug F)

(defun debug-print (text) 
	(if (= T json:debug) 
		(princ text)))

(defun string2list (string) 
	(setq output (list))
	(foreach charcode (vl-string->list string) 
		(setq output (cons (chr charcode) output)))
	(reverse output))

(defun starts-with (pfx stringlist) 
	(setq pfxlist (string2list pfx))
	(setq result T)
	(while (not (= pfxlist (list)))
		(setq x (car pfxlist))
		(setq y (car stringlist))
		(setq pfxlist (cdr pfxlist))
		(setq stringlist (cdr stringlist))
		(if (not (= x y))
			(setq result F)))
	(setq result result))

(defun drop (n lst) 
	(while (< 0 n)
		(setq lst (cdr lst))
		(setq n (- n 1)))
	(setq lst lst))

(defun take (n lst)
	(setq output (list)) 
	(while (< 0 n)
		(setq output (cons (car lst) output))
		(setq lst (cdr lst))
		(setq n (- n 1)))
	(reverse output))

(defun readhex (hexlist)
	(defun hex2num (hex) (cond
		((= hex "0") 0)	((= hex "1") 1)	((= hex "2") 2)	((= hex "3") 3)	((= hex "4") 4)	((= hex "5") 5)	((= hex "6") 6)
		((= hex "7") 7) ((= hex "8") 8)	((= hex "9") 9)

		((= hex "a") 10) ((= hex "b") 11) ((= hex "c") 12) ((= hex "d") 13)	((= hex "e") 14) ((= hex "f") 15)
		((= hex "A") 10) ((= hex "B") 11) ((= hex "C") 12) ((= hex "D") 13) ((= hex "E") 14) ((= hex "F") 15)
		))
	(setq output 0)
	(while (not (= hexlist (list)))
		(setq output (+ (* output 16) (hex2num (car hexlist))))
		(setq hexlist (cdr hexlist))
		)
	(setq output output))

(defun json:read (jsonText) 
	; Так как а автолиспе ниче нет, то разбор производим стейт-машиной c магазином
	(setq curr-atom "")
	(setq curr-key "")
	
	(setq curr-dict (list))
	(setq dict-history (list))

	(defun isdigit (ch) 
		(or (= ch "0") (= ch "1") (= ch "2") (= ch "3") (= ch "4") (= ch "5") (= ch "6") (= ch "7") (= ch "8") (= ch "9")))

	(defun isspace (ch)
		(or (= ch " ") (= ch "\t") (= ch "\n") (= ch "\r")))

	(defun push-kv ()
		(if (not (= curr-key ""))
			(setq curr-dict (cons (cons curr-key curr-atom) curr-dict)))
		(setq curr-key "")
		(setq curr-atom ""))

	(setq curr-array (list))
	(setq array-history (list))

	(setq string-delim "")
	
	(setq state "object")
	(setq state-history (list))

	(setq error-message "")
	(defun report-error (message) 
		(push-state "error")
		(setq error-message message)
		(setq jsonList (list)))
	
	(defun pop-state () 
		(debug-print (strcat "\n-" state))
		(setq state (car state-history))
		(setq state-history (cdr state-history)))
	(defun push-state (new-state) 
		(debug-print (strcat "\n+" new-state))
		(setq state-history (cons state state-history))
		(setq state new-state))

	(defun skip () 
		(setq a ""))

	(setq escaping F)

	(setq jsonList (string2list jsonText))
	(while (not (= jsonList (list)))
		(setq ch (car jsonList))
		(setq jsonList (cdr jsonList))
		(cond
			((= state "up") (progn 
				(setq jsonList (cons ch jsonList))
				(pop-state)
				(pop-state)))

			((= state "object") (cond
				((isspace ch) (skip))
				((= ch "{") (progn 
					(setq dict-history (cons (list curr-key curr-dict) dict-history))
					(setq curr-key "")
					(setq curr-dict (list))
					(push-state "object-next")
					))
				(T (report-error (strcat "Unexpected " ch " instead of { or space")))
				))

		 	((= state "object-semi") (cond 
				((isspace ch) (skip))
				((= ch ":") (progn 
					(setq curr-key curr-atom)
					(pop-state)
					(push-state "object-next")
					(push-state "value")))
				(T (report-error (strcat "Unexpected " ch " instead of : or space")))
		 		))

		 	((= state "object-next") (cond 
				((isspace ch) (skip))
				((= ch "\"") (progn 
					(setq jsonList (cons ch jsonList))
					(pop-state)
					(push-state "object-semi")
					(push-state "string")))
				((= ch ",") (progn 
					(push-kv)
					(pop-state)
					(push-state "object-semi")
					(push-state "string")))
				((= ch "}") (progn 
					(push-kv)
					(setq curr-atom curr-dict)
					(setq dh (car dict-history))
					(setq dict-history (cdr dict-history))
					(setq curr-key 	(nth 0 dh))
					(setq curr-dict (nth 1 dh))
					(pop-state)
					(pop-state)))
				(T (report-error (strcat "Unexpected " ch " instead of , or } or space")))
		 		))

			((= state "array") (cond 
				((isspace ch) (skip))
				((= ch "[") (progn 
					(setq array-history (cons curr-array array-history))
					(setq curr-array (list))
					(push-state "array-next")
					))
				(T (report-error (strcat "Unexpected " ch " instead of [ or space")))
				))

			((= state "array-next") (cond
				((isspace ch) (skip))
				((= ch ",") (progn
				 	(setq curr-array (cons curr-atom curr-array))
				 	(pop-state)
					(push-state "array-next")
					(push-state "value")))
				((= ch "]") (progn
					(setq curr-array (cons curr-atom curr-array))
					(setq curr-atom (reverse curr-array))
					(setq curr-array (car array-history))
					(setq array-history (cdr array-history))
					(pop-state)
					(pop-state)
					))
				(T (progn 
					(setq jsonList (cons ch jsonList))
					(push-state "value")))
				))
			
			((= state "value") (cond 
				((isspace ch) (skip))
				((= ch "\"") 
					(progn
						(setq jsonList (cons ch jsonList))
						(push-state "up")
						(push-state "string")))
				((= ch "'") 
					(progn
						(setq jsonList (cons ch jsonList))
						(push-state "up")
						(push-state "string")))
				((= ch "{") 
					(progn
						(setq jsonList (cons ch jsonList))
						(push-state "up")
						(push-state "object")))
				((= ch "[") (progn
						(setq jsonList (cons ch jsonList))
						(push-state "up")
						(push-state "array")))
				((or (= ch "-") (isdigit ch)) 
					(progn
						(setq jsonList (cons ch jsonList))
						(setq curr-atom "")
						(push-state "up")
						(push-state "number")))

				((starts-with "true" (cons ch jsonList)) 
					(progn
						(setq jsonList (drop 3 jsonList))
						(setq curr-atom 'true)
						(pop-state)))
				((starts-with "false" (cons ch jsonList)) 
					(progn
						(setq jsonList (drop 4 jsonList))
						(setq curr-atom 'false)
						(pop-state)))
				((starts-with "null" (cons ch jsonList)) 
					(progn
						(setq curr-atom 'null)
						(setq jsonList (drop 3 jsonList))
						(pop-state)))
				(T (report-error (strcat "Unexpected " ch " instead of JSON value")))
				)) 

			((= state "string") (cond 
				((isspace ch) (skip))
				((or (= ch "'") (= ch "\"")) 
					(progn 
						(setq curr-atom "")
						(setq string-delim ch)
						(push-state "instring")
					))
				(T (report-error (strcat "Unexpected " ch " instead of \" or space")))
				))

			((= state "instring") (cond 
					(escaping 		(progn
						(setq curr-atom (strcat curr-atom ch))
						(setq escaping F)))
					((= ch "\\")	
						(progn 
							(setq ext (car jsonList))
							(setq jsonList (cdr jsonList))
							(cond 
								((= ext "\\") (setq curr-atom (strcat curr-atom "\\")))
								((= ext "\"") (setq curr-atom (strcat curr-atom "\"")))
								((= ext "/")  (setq curr-atom (strcat curr-atom "/")))
								((= ext "b")  (setq curr-atom (strcat curr-atom (chr 8))))
								((= ext "f")  (setq curr-atom (strcat curr-atom (chr 12))))
								((= ext "n")  (setq curr-atom (strcat curr-atom (chr 10))))
								((= ext "r")  (setq curr-atom (strcat curr-atom (chr 13))))
								((= ext "t")  (setq curr-atom (strcat curr-atom (chr 9))))
								((= ext "u")  (progn
									(setq curr-atom (strcat curr-atom (chr (readhex (take 4 jsonList)))))
									(setq jsonList (drop 4 jsonList))
									)))
								))
					((= ch string-delim)
						(progn 
							(pop-state)
							(pop-state)))
					(T (setq curr-atom (strcat curr-atom ch)))
				)) 
			
			((= state "number") (cond
				((or (isdigit ch) (= ch ".") (= ch "e") (= ch "E") (= ch "-") (= ch "+"))
					(setq curr-atom (strcat curr-atom ch)))
				(T (progn
					(setq curr-atom (atof curr-atom))
					(setq jsonList (cons ch jsonList))
					(pop-state)
					))
				))

			((= state "error") (skip))

			(T (report-error (strcat "Unexpected  JSON parser state")))
			))
	(if (not (= state "error"))
		(setq curr-atom curr-atom)
		(progn 
			(princ (strcat error-message "\n"))
			(setq curr-atom nil))
))


;(defun json:write (obj) 
;	...)