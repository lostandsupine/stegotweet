;;Comments for Schell!
;;Embeds a secret message according to a set of predefined keys in a tweet selected from a pool.
;;requires cl-twitter.
;;First, build your tweet pool with (get-search-results (list "#searchforthiskeyword" "#andthisonetoo")), populates *big-list*
;;Second, build codes with (defparameter *code-list* (make-codes 21 (list 1 2 3 ... 63) 0).  
;;Makes 21 codes (optimal), 3 characters positions per code, of the first 63.
;;Third, embedd your message (encode-message *big-list* *code-list* "Hello World" 3 T)
;;If you want to extract the message, feed any of the outputted embedding tweets into (multi-unencode tweets *code-list*)
;;cl-twitter also provides tweeting.


(defparameter *hash-results* nil)
(defparameter *big-list* nil ) ;;This is the pool of tweets.
(defparameter *average-tweet-length* 0)
(defparameter *average-characters* 0)

;;An list of pairs, first element of pair is the alphabetic character, second is an integer.  e.g.(#\A 3)
(defun make-assoc-list ()
  (let ((list-out))
    (dotimes (i 26)
      (push (list (code-char (+ 65 i)) 0) list-out))
    (reverse list-out)))

;;Creates a hash table from the list above.
(defparameter *alphabet-hash-table* (make-hash-table :initial-contents (make-assoc-list) :test 'equal :size 26))

;;Returns decapitalized string, non-destructive.
(defun decapitalize-string (the-string)
  (let ((string-out the-string))
    (dotimes (i (length the-string))
      (let ((curr-val (char-code (elt the-string i))))
	(if (<= curr-val 90)
	    (setf (elt string-out i) (code-char (+ 32 curr-val))))))
    string-out))

;;Returns a hash table of alphabetic letter distributions, from an inputted hash-table of tweets (*big-list*).  Relative option returns percentage.
(defun letter-distribution (the-list &optional relative)
  (let ((alphabet-hash-table (make-hash-table :test 'equal :size 26)))
    (dotimes (i (length the-list))
      (let ((curr-string (decapitalize-string (remove-non-letters (elt the-list i)))))
	(dotimes (j (length curr-string))
	  (if (gethash (elt curr-string j) alphabet-hash-table)
	      (setf (gethash (elt curr-string j) alphabet-hash-table) (+ (gethash (elt curr-string j) alphabet-hash-table) 1))
	      (setf (gethash (elt curr-string j) alphabet-hash-table) 1)))))
    (if relative
	(let ((total-count 0))
	  (maphash #'(lambda (key value) (declare (ignore key)) (setf total-count (+ total-count value))) alphabet-hash-table)
	  (maphash #'(lambda (key value) (setf (gethash key alphabet-hash-table) (float (* 100 (/ value total-count))))) alphabet-hash-table)))
    alphabet-hash-table))

;;Returns a hash table of alphabetic character length of tweets, from an inputted hash-table of tweets (*big-list*).  Relative option returns percentage.
(defun char-length-distribution (the-list &optional relative)
  (let ((char-length-hash (make-hash-table :test 'eql)))
    (dotimes (i (length the-list))
      (let ((curr-string (length (remove-non-letters (elt the-list i)))))
	(if (gethash curr-string char-length-hash)
	    (setf (gethash curr-string char-length-hash) (+ (gethash curr-string char-length-hash) 1))
	    (setf (gethash curr-string char-length-hash) 1))))
    (if relative
	(let ((total-count 0))
	  (maphash #'(lambda (key value) (declare (ignore key)) (setf total-count (+ total-count value))) char-length-hash)
	  (print total-count)
	  (maphash #'(lambda (key value) (setf (gethash key char-length-hash) (float (* 100 (/ value total-count))))) char-length-hash)))
    char-length-hash))

;;Prints a readable entry from hash-table.       
(defun print-hash-table-entry (key value)
  (format t "~{~,5f ~} " (list key value)))

;;Prints whole hash-table.
(defun print-hash-table (the-hash-table)
  (maphash #'print-hash-table-entry the-hash-table))

(defun print-hash-table-alphabetically (the-hash-table)
  (dotimes (i 26)
    (let ((curr-entry (gethash (code-char (+ 97 i)) the-hash-table)))
      (print-hash-table-entry (code-char (+ 97 i)) curr-entry))))

(defun print-hash-table-numerically (the-hash-table &optional just-values)
  (let ((key-max 0) (keys ))
    (maphash #'(lambda (key value) (declare (ignore value)) (push key keys)) the-hash-table)
    (setf key-max (max-element keys))
    (dotimes (i key-max)
      (if (gethash i the-hash-table)
	  (progn
	    (if (not just-values)
		(format t "~a " i))
	    (format t "~,5f " (gethash i the-hash-table)))
	  (progn
	    (if (not just-values)
		(format t "~a " i))
	    (format t "~a " 0))))))

;;Given a string of letter, and an alphabetic character distribution, returns chance of embedding in a single random tweet.
(defun word-chance (the-word letter-distribution)
  (let ((chance-out 1))
    (dotimes (i (length the-word))
      (setf chance-out (* chance-out (/ (gethash (elt the-word i) letter-distribution) 100))))
    chance-out))

;;Chance of embedding a string, given a pool of n tweets, using m codes and a letter distribution.
(defun encode-prob(word n-tweets m-codes letter-distribution)
  (- 1 (expt (- 1 (word-chance word letter-distribution)) (* n-tweets m-codes))))
  
(defun text-from-hash (key value)
  (declare (ignore key))
  (twitter:search-ref-text value))

(defun sqr (x)
  (* x x))

;;Populates *big-list* with tweets found from search string list. i.e. (list "#lolz" "#kittenmittens")
(defun get-search-results (search-string-list)
  (let ((temp-hash-results ))
    (if (not *big-list*)
	(defparameter *big-list* (list )))
    (dotimes (i (length search-string-list))
      (setf temp-hash-results (twitter:do-search (elt search-string-list i)))
      (loop for value being the hash-values of temp-hash-results
	   do (setf *big-list* (append *big-list* (list (twitter:search-ref-text value))))))))

(defun ave-tweet-length (the-list)
  (let ((average-tweet-length 0))
    (dotimes (i (length the-list))
      (setf average-tweet-length (+ average-tweet-length (length (elt the-list i)))))
    (setf average-tweet-length (float (/ average-tweet-length (length the-list))))))

(defun ave-char-length (the-list)
  (let ((average-characters 0))
    (dotimes (i (length the-list))
      (setf average-characters (+ average-characters (length (remove-non-letters (elt the-list i))))))
    (setf average-characters (float (/ average-characters (length the-list))))))

(defun stdv-char-length (the-list)
  (let ((stdv 0) (mean (ave-char-length the-list)))
    (dotimes (i (length the-list))
      (setf stdv (+ stdv (sqr (- mean (length (remove-non-letters (elt the-list i))))))))
    (sqrt (/ stdv (length the-list)))))

(defun concatenate-or-append (thing1 thing2)
  (cond
    ((and (typep thing1 'integer) (typep thing2 'integer)) (list thing1 thing2))
    ((and (not (typep thing1 'integer)) (typep thing2 'integer)) (append thing1 (list thing2)))
    ((and (not (typep thing2 'integer)) (typep thing1 'integer)) (append thing2 (list thing1)))
    ((and (not (typep thing2 'integer)) (not (typep thing1 'integer))) (concatenate 'list thing1 thing2))))

;;Tests if x is an element in the-list
(defun elementp (the-list x)
  (if (integerp the-list)
      (if (= the-list x)
	  (return-from elementp T)
	  (return-from elementp nil)))      
  (dotimes (i (length the-list))
    (if (= (elt the-list i) x)
	(return-from elementp T)))
  nil)

;;Returns permutations of two lists.
(defun permute-lists (list1 list2)
  (let ((output-list ) (temp-element))
    (dotimes (i (length list1))
      (dotimes (j (length list2))
	(if (not (elementp (elt list1 i) (elt list2 j)))
	    (progn
	      (setf temp-element (concatenate-or-append (elt list1 i) (elt list2 j)))
	      (push temp-element output-list)))))
    (return-from permute-lists output-list)))

;;Returns a list of n codes, given a list of elements in num-list.  max-overlap weeds out permutations that are too similar.
(defun make-codes (n num-list &optional max-overlap)
  (let ((code-list num-list))
    (dotimes (i (- n 1))
	  (setf code-list (remove-duplicates (permute-lists code-list num-list) :test #'combination-compare)))
    (if max-overlap
	(return-from make-codes (remove-overlap code-list max-overlap))
	(return-from make-codes code-list))))

;;Tests if two lists are permutations of each other. 
(defun combination-compare (combo1 combo2)
  (if (>= (length combo1) (length combo2))
      (dotimes (i (length combo1))
	(if (not (elementp combo2 (elt combo1 i)))
	    (return-from combination-compare nil)))
      (dotimes (i (length combo2))
	(if (not (elementp combo1 (elt combo2 i)))
	    (return-from combination-compare nil))))
  T)

;;Returns most common character in the tweets of input list (*big-list*).
(defun mode (the-list)
  (let ((the-hash (make-hash-table )) (freq 0) (the-mode ))
    (dotimes (j (length the-list))
      (if (gethash (elt the-list j) the-hash)
	  (setf (gethash (elt the-list j) the-hash) (+ (gethash (elt the-list j) the-hash) 1))
	  (setf (gethash (elt the-list j) the-hash) 1)))
    (maphash #'(lambda (key value) (if (> value freq) (progn (setf the-mode key) (setf freq value)))) the-hash)
    the-mode))
    
;;Non-destructive, returns new list without the elements specified in index-list.
(defun remove-element-by-index (the-list index-list)
  (let ((out-list ))
    (dotimes (j (length the-list))
      (if (not (elementp index-list j))
	  (push (elt the-list j) out-list)))
    (reverse out-list)))

 ;;Non-destructive, returns list with elements removed that overlap by specified amount.
(defun remove-overlap (the-list overlap)
  (let ((output-list (list )) (duplist (list )))
    (dotimes (i (length the-list))
      (dotimes (j (length the-list))
	(if (and (not (= i j)) (> (combination-compare2 (elt the-list i) (elt the-list j)) overlap))
	    (progn
	      (push i duplist)
	      (push j duplist)))))
    (if (not duplist)
	(return-from remove-overlap the-list)
	(progn
	  (setf output-list (remove-element-by-index the-list (list (mode duplist))))
	  (setf output-list (remove-overlap output-list overlap))))
    output-list))     
	      
;;Returns how many characters two lists have in common.
(defun combination-compare2 (combo1 combo2)
  (let ((count 0))
    (if (>= (length combo1) (length combo2))
	(dotimes (i (length combo1))
	  (if (elementp combo2 (elt combo1 i))
	      (setf count (+ count 1))))
	(dotimes (i (length combo2))
	  (if (elementp combo1 (elt combo2 i))
	      (setf count (+ count 1)))))
    count))
   
;;Returns maximum element from a list. 
(defun max-element (the-list)
  (let ((the-max (elt the-list 0)))
    (dotimes (i (- (length the-list) 1))
      (let ((temp-max (elt the-list (+ i 1))))
      (if (> temp-max the-max)
	  (setf the-max temp-max))))
    (return-from max-element the-max)))

;;Removes whitespace characters from a string.
(defun remove-whitespace (string)
  (remove-if #'(lambda (x) (char= #\Space x)) string))

;;Tests if a character is in the english alphabet.
(defun eng-alpha-char-p (c)
  (or (and (>= (char-code c) 65) (<= (char-code c) 90))
      (and (>= (char-code c) 97) (<= (char-code c) 122))))

;;Non-destructive, returns string without non-english alphabetic characters.
(defun remove-non-letters (string)
  (remove-if #'(lambda (x) (not (eng-alpha-char-p x))) string))

(defun caseless-string-eql (string1 string2)
  (if (not (= (length string1) (length string2)))
      (return-from caseless-string-eql nil))
  (dotimes (i (length string1))
    (if (not (char-equal (elt string1 i) (elt string2 i)))
	(return-from caseless-string-eql nil)))
  T)

;;Given a list of tweets, a list of keys, a message, how many embedding tweets you'd like, and the option of just the embedding tweets.
;;e.g (encode-message *big-list* '('(1 2 3) '(4 7 8) '(5 3 5)...) "Hello World" 4 nil)
(defun encode-message (the-list code-list message n-returns &optional just-tweets)
  (let ((encodes ))
    (block outer
      (dotimes (i (length the-list))
	(block inner
	  (let ((curr-tweet (remove-non-letters (elt the-list i))) (curr-tweet-min-diff 0))
	    (dotimes (l (length encodes))
	      (if just-tweets
		    (setf curr-tweet-min-diff (max curr-tweet-min-diff (string-difference curr-tweet (elt encodes l))))
		    (setf curr-tweet-min-diff (max curr-tweet-min-diff (string-difference curr-tweet (elt (elt encodes l) 1))))))
	    (if (< curr-tweet-min-diff 0.5)
		(progn
		  (dotimes (j (length code-list))
		    (if (> (length curr-tweet) (max-element (elt code-list j)))
			(progn
			  (let ((temp-word nil))
			    (dotimes (k (length message))
			      (push (elt curr-tweet (elt (elt code-list j) k)) temp-word))
			    (setf temp-word (concatenate 'string (reverse temp-word)))
			    (if (caseless-string-eql message temp-word)
				(progn
				  (if just-tweets
				      (setf encodes (append encodes (list curr-tweet)))
				      (setf encodes (append encodes (list (list (list i j) curr-tweet (elt code-list j))))))
				  (return-from inner)))))))))))
	(if (= (length encodes) n-returns)
	    (return-from outer))))
    encodes))

;;Given any tweet, and a single code, returns the unembedding.
(defun unencode-message (tweet code)
  (let ((msg ) (alpha-tweet (remove-non-letters tweet)))
    (dotimes (i (length code))
      (push (elt alpha-tweet (elt code i)) msg))
    (concatenate 'string (reverse msg))))

;;Given a single tweet and multiple codes, returns all possible unembeddings.
(defun mass-unencode (tweet code-list)
  (let ((message-list ))
    (dotimes (i (length code-list))
      (push (unencode-message tweet (elt code-list i)) message-list))
    message-list))

;;Returns a list of all characters of list1 that are also in list2.
(defun compare-lists (list1 list2)
  (let ((list-out))
    (dotimes (i (length list1))
      (dotimes (j (length list2))
	(if (caseless-string-eql (elt list1 i) (elt list2 j))
	    (push (elt list1 i) list-out))))
    list-out))

;;Returns list of elements that are common to all lists inputted.
(defun compare-multi-lists (list-of-lists)
  (if (= (length list-of-lists) 1)
      (remove-duplicates (elt list-of-lists 0) :test #'caseless-string-eql)
      (progn
	(let ((temp-duplicates))
	  (dotimes (i (- (length list-of-lists) 1))
	    (push (remove-duplicates (compare-lists (elt list-of-lists i) (elt list-of-lists (+ i 1))) :test #'caseless-string-eql) temp-duplicates))
	  (compare-multi-lists temp-duplicates)))))

;;First, gets all possible unembeddings for every tweet with every key, then gets the string common to all.  	      
(defun multi-unencode (tweet-list code-list)
  (let ((decode-list ))
    (dotimes (i (length tweet-list))
      (setf decode-list (append decode-list (list (mass-unencode (elt tweet-list i) code-list)))))
    (compare-multi-lists decode-list)))

(defun string-difference (string1 string2)
  (let ((chars-in-common 0) (min-length (min (length string1) (length string2))))
    (dotimes (i min-length)
      (if (char-equal (elt string1 i) (elt string2 i))
	  (setf chars-in-common (+ chars-in-common 1))))
    (float (/ chars-in-common min-length))))

(defun factorial (x)
  (if (= x 1)
      1
      (* x (factorial (- x 1)))))

(defun n-choose-k (n k)
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

;(defun write-list-to-file (tweet-list)
;  (with-open-file (f "/big-list" :direction :output :external-format e)
    
    

	     
