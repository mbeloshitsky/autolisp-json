; AutoLISP json parser and formatter
; (c) 2015, Michel Beloshitsky <mbeloshitsky@gmail.com>
;
;
; TODO:
;   - Handle numbers in parser 
;   - Error handling in parser
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
					(push-state "object-semi")
					(push-state "string")))
				))

		 	((= state "object-semi") (cond 
				((isspace ch) (skip))
				((= ch ":") (progn 
					(setq curr-key curr-atom)
					(pop-state)
					(push-state "object-next")
					(push-state "value")))
		 		))

		 	((= state "object-next") (cond 
				((isspace ch) (skip))
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
		 		))

			((= state "array") (cond 
				((isspace ch) (skip))
				((= ch "[") (progn 
					(setq array-history (cons curr-array array-history))
					(setq curr-array (list))
					(push-state "array-next")
					(push-state "value")))
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
				)) 

			((= state "string") (cond 
				((isspace ch) (skip))
				((or (= ch "'") (= ch "\"")) 
					(progn 
						(setq curr-atom "")
						(setq string-delim ch)
						(push-state "instring")
					))
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
								((= ext "\\") (setq curr-atom (strcat "\\" curr-atom)))
								((= ext "\"") (setq curr-atom (strcat "\"" curr-atom)))
								((= ext "/")  (setq curr-atom (strcat "/" curr-atom)))
								((= ext "b")  (setq curr-atom (strcat (chr 8) curr-atom)))
								((= ext "f")  (setq curr-atom (strcat (chr 12) curr-atom)))
								((= ext "n")  (setq curr-atom (strcat (chr 10) curr-atom)))
								((= ext "r")  (setq curr-atom (strcat (chr 13) curr-atom)))
								((= ext "t")  (setq curr-atom (strcat (chr 9) curr-atom)))
								((= ext "u")  (...)))
								))
					((= ch string-delim)
						(progn 
							(pop-state)
							(pop-state)))
					(T (setq curr-atom (strcat curr-atom ch)))
				)) 
			
			((= state "number") ...)

			(T (princ "\nERROR")) 
			))
	(setq curr-atom curr-atom))


(defun json:write (obj) 
	...)