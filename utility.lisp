;; ===============================================
;; UTILITY->LISP =================================
;; ===============================================

(in-package :utility)

;; -----------------------------------------------
;; DEFINES ---------------------------------------
;; -----------------------------------------------
(defmacro define-generic (name value)
 `(defgeneric ,name ,value))

(defmacro define-exported-generic (name value)
 `(progn
    (defgeneric ,name ,value)
    (export ',name)))

(defmacro define-constant (name value)
 `(defconstant ,name ,value))

(defmacro define-exported-constant (name value)
 `(progn
    (defconstant ,name ,value)
    (export ',name)))

(defmacro define-variable (name value)
 `(defvar ,name ,value))

(defmacro define-exported-variable (name value)
 `(progn
    (defvar ,name ,value)
    (export ',name)))

(defmacro define-function (name args &rest body)
 `(defun ,name (,@args)
    ,@body))

(defmacro define-exported-function (name args &rest body)
 `(progn
    (defun ,name (,@args)
      ,@body)
    (export ',name)))

(defmacro define-method (name args &rest body)
 `(defmethod ,name (,@args)
    ,@body))

(defmacro define-exported-method (name args &rest body)
 `(progn
    (defmethod ,name (,@args)
      ,@body)
    (export ',name)))

(defmacro define-macro (name args &rest body)
 `(defmacro ,name (,@args)
    ,@body))

(defmacro define-exported-macro (name args &rest body)
 `(progn
    (defmacro ,name (,@args)
      ,@body)
    (export ',name)))

(defmacro define-exported-macro (name args &rest body)
 `(progn
    (defmacro ,name (,@args)
      ,@body)
    (export ',name)))

(defmacro define-class (name superclasses slot-specifiers)
 `(defclass ,name ,superclasses ,slot-specifiers))

;; FIX: This should also probably export all the accessors/readers defined in the class. I have to do that manually in this state.
(defmacro define-exported-class (name superclasses slot-specifiers)
 `(progn
    (defclass ,name ,superclasses ,slot-specifiers)
    (export ',name)))

;; - - - - - - - - - - - - - - - - - - - - - - - -

;; -----------------------------------------------
;; GLOBALS ---------------------------------------
;; -----------------------------------------------
(define-exported-variable *alphabet* 
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\x #\y #\z))

(define-exported-variable *numbers* 
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

;; -----------------------------------------------
;; UTILITY ---------------------------------------
;; -----------------------------------------------
(define-exported-function string-last (string)
  (when (stringp string)
    (let ((length-string (length string)))
      (subseq string (1- length-string)
                     length-string))))

(define-exported-function string-but-last (string)
  (when (stringp string)
    (let ((length-string (length string)))
      (when (> length-string 0)
        (subseq string 0 (1- length-string))))))

(define-exported-function append-newline (line)
 (concatenate 'string line (list #\Newline)))

(define-exported-function remove-trailing-newline (string)
  (when (and (stringp string)
             (> (length string) 0)
             (string= (string-last string)
                      #\Newline))
    (string-but-last string)))

(define-exported-function intern-gethash (key hash-table)
  (intern (gethash key hash-table)))

(define-exported-function remove-if-member-of (to-omit list) ;; FIXME probably horribly slow
  (remove-if #'(lambda (element)
                 (member element to-omit))
             list))

(define-exported-macro if-else (condition if else)
 `(if ,condition
    (progn ,@if)
    (progn ,@else)))

(define-exported-function flatten (list)
  (cond ((atom list) 
         list)
        ((listp (car list))
         (append (flatten (car list)) (flatten (cdr list))))
        (t 
         (append (list (car list)) (flatten (cdr list))))))

(define-exported-macro when-return-from (condition return-from body-return)
 `(when ,condition
    (return-from ,return-from ,body-return)))

(define-exported-macro unless-return-from (condition return-from body-return)
 `(unless ,condition
    (return-from ,return-from ,body-return)))

(define-exported-function mapcar-traverse (function list)
  (mapcar #'(lambda (element)
              (if (listp element)
                (mapcar-traverse function element)
                (funcall function element)))
          list))

;; http://lispy.wordpress.com/2007/11/30/your-macro-is-in-my-mapcar-no-your-mapcar-is-in-my-macro/
(define-exported-macro mapcro (macro &rest args)
 `(apply #'mapcar #'(lambda (&rest x)
                      (eval (append (list ',macro) x)))
                  ',args))

(define-exported-function list-nilp (list)
  (notany #'identity list))

(define-exported-function list-not-nilp (list)
  (not (list-nilp list)))

(define-exported-function ensure-list (atom-or-list)
  (if (listp atom-or-list) atom-or-list (list atom-or-list)))

(define-exported-function require-list (packages)
  (mapcar #'require (ensure-list packages)))

(define-exported-function class-name-of (instance)
  (class-name (class-of instance)))

(define-exported-function find-class-or-preserve (class)
  (if (symbolp class)
    (find-class class)
    class))

(define-exported-function sequencep (sequence)
  (typep sequence 'sequence))

(define-exported-function length-1 (sequence)
  "length of a sequence -1"
  (1- (length sequence)))

(define-exported-function length= (sequence-length-0 sequence-length-1)
  ;; FIX: Throw an error instead of just nil?
  (cond
    ((and (sequencep sequence-length-0)
          (integerp sequence-length-1))
     (= (length sequence-length-0) sequence-length-1))
    ((and (sequencep sequence-length-1)
          (integerp sequence-length-0))
     (= (length sequence-length-1) sequence-length-0))))

(define-exported-function string-ends-with (str end)
  "does a string end with another string"
  (let ((str-length (length str)))
      (string= (subseq str (- str-length (length end))
                           str-length)
                end)))

(define-exported-macro do-list-index ((name-member list name-index) &rest body)
 `(dotimes (,name-index (length ,list))
    (let ((,name-member (elt ,list ,name-index)))
      ,@body)))

(define-exported-function alphabetize (strings)
  "alphabetize a list of strings"
  (sort strings #'string<))

(define-exported-macro body-to-string (&rest body)
  "make a body of code a string"
  `(write-to-string (quote ,@body) :pretty nil))

(define-exported-macro with-index (&rest r)
  "with an incrementing index function"
  `(let ((current-index -1))
     (define-exported-function i ()
       (incf current-index))
     ,@r))

(define-exported-function eval-from-string (str)
  "evaluate from a string"
  (eval (read-from-string str)))

(define-exported-function eval-to-string (body)
  "eval a string to a string"
  (write-to-string (eval-from-string body)))

(define-exported-function string=case (string-1 string-2 case-sensitive)
  "strings are equal, case senstive or not"
  (if case-sensitive
    (string= string-1 string-2)
    (string= (string-downcase string-1)
             (string-downcase string-2))))

(define-exported-function string-member (item list &key (case-sensitive t))
  "an item (string) is a member of a list"
  (dolist (elem list)
    (when (string=case item elem case-sensitive)
      (return-from string-member t))))

(define-exported-function split-on (split str &optional (splits '()))
  "split a list on a string"
  (let ((split-pos (search split str)))
    (if split-pos
      (progn 
        (push (subseq str 0 split-pos) splits)
        (split-on split (subseq str (+ split-pos (length split)) (length str)) splits))
      (reverse (push str splits)))))

(define-exported-function ensure-string (atm &key (downcase nil))
  "write-to-string unless it's a string"
  (let ((ensured (cond
                   ((stringp atm)
                    atm)
                   ((characterp atm)
                    (format nil "~a" atm))
                   (t ;; integer, others.. FIXME test for all other types
                    (write-to-string atm)))))
    (if downcase (string-downcase ensured) ensured)))

(define-exported-function list-delimited-by (list delimiter &key (delimiter-prefix nil) (delimiter-suffix nil))
  "delimit a list by a string, return as string"
  ;; handle other types
  (setf delimiter (ensure-string delimiter))
  (let* ((delimited     (if delimiter-prefix delimiter ""))
         (length-list   (length list))
         (length-list-1 (1- length-list)))
    ;; elements
    (dotimes (i length-list)
      (if (= i length-list-1)
        (setf delimited (concatenate 'string delimited (ensure-string (elt list i))))
        (setf delimited (concatenate 'string delimited (ensure-string (elt list i)) delimiter))))
    ;; suffix
    (when delimiter-suffix
      (setf delimited (concatenate 'string delimited delimiter)))
    delimited))

(define-exported-function concatenate-path (&rest paths)
  (list-delimited-by paths "/"))

(define-exported-function intern-concatenate (&rest r)
  "concatenate to a symbol"
  (intern (string-upcase (apply #'concatenate 'string (mapcar #'ensure-string r)))))

(define-exported-function make-keyword (name &key (strip-colon t))
  (cond
    ;; keyword
    ((keywordp name)
     name)
    ;; symbol
    ((symbolp name)
     (make-keyword (write-to-string name)))
    ;; string
    ((stringp name)
     (when (and strip-colon
                (string= (subseq name 0 1) ":"))
       (setf name (subseq name 1 (length name))))
     (values (intern name "KEYWORD")))
    ;; incompatible
    (t
      nil)))

(define-exported-function make-name (name)
  (intern (string-upcase (ensure-string name))))

(define-exported-function chance (rate &optional total) ;; FIXME handle negatives
  "a random chance"
  (if total
    (chance (* 100 (/ rate total))) ;; 1 in 5 chance (chance 1 5)
    (< (random 100) rate)))
  
(define-exported-function random-elt (lst)
  "random element from a list"
  (when lst
    (elt lst (random (length lst)))))
  
(define-exported-function alpha-or-number ()
  "get a random letter or number"
  (if (chance 50)
    (random-elt *alphabet*)
    (random-elt *numbers*)))

(define-exported-function random-string (len)
  "make a random string"
  (map-into (make-string len) #'alpha-or-number))

(define-exported-function delete-nil (lst)
  "deletes nils from a list"
  (delete 'nil lst))

(define-exported-function remove-nil (lst)
  "removes nils from a list"
  (remove 'nil lst))

(define-exported-macro push-end (elem lst)
  "destructively push an element to the end of a list"
  `(if (null ,lst)
     (setf  ,lst (list ,elem))
     (nconc ,lst (list ,elem))))

(define-exported-function string-wrap (ch str)
  "wrap a string in a character"
  (concatenate 'string (list ch) str (list ch)))

(define-exported-function comma-separate (lst) ;; FIXME use list-delimited-by ", "
  "comma seperate a list of strings"
  (let ((lst-length (length lst))
        (rtn ""))
    (dotimes (x lst-length)
      (if (= x 0)
        (setf rtn (elt lst x))
        (setf rtn (concatenate 'string rtn ", " (elt lst x)))))
    rtn))

(define-exported-function symbol-downcase (sym)
  "symbol to lower case string"
  (when (symbolp sym)
    (string-downcase (symbol-name sym))))

(define-exported-function replace-all (str part replacement &key (test #'char=))
  "replace all instances of 'part' with 'replacement'"
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part str
                            :start2 old-pos
                            :test test)
          do (write-string str out
                           :start old-pos
                           :end (or pos (length str)))
          when pos do (write-string replacement out)
          while pos)))

(define-exported-function require-and-use-package (package)
  (require     package)
  (use-package package))

(define-exported-function prompt-read (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(define-exported-function trailing-slash (string)
  (concatenate 'string string "/"))

(define-exported-macro with-verbose (verbose current-function-name format-destination &rest body)
  `(let ((verbose ,verbose)
         (current-function-name ,current-function-name)
         (format-destination ,format-destination))
     (verbose-format-function-name)
     ,@body))

(define-exported-macro verbose-format (level &rest rest)
  `(when verbose 
     (format format-destination "~a~a~%" (if (= ,level 0)
                                             ""
                                             (concatenate 'string
                                               (list-delimited-by
                                                 (make-list ,level :initial-element "  ")
                                                 "")
                                               "- "))
                                         (format nil ,@rest))))
                                         
(define-exported-macro verbose-format-function-name ()  
  `(verbose-format 0 "#'~a" current-function-name))

;; FIXME doesn't validate at all
(define-exported-function emailp (email)
  (and email
       (stringp email)
       (search "@" email)))

(define-exported-function round-percent-to-255 (n)
  (round (float (/ (* n 255) 100))))

(define-exported-function current-directory ()
  (truename "."))

(define-exported-function max-string-length (list)
  (apply #'max (mapcar #'length (ensure-list list))))

(define-exported-function make-string-spaces (n)
  (make-string n :initial-element #\Space))

(define-exported-function file-as-list (path)
  (handler-case 
    (with-open-file (stream path)
      (loop :for     line = (read-line stream nil)
            :while   line
            :collect line))
    ;; file does not exist
    (sb-int:simple-file-error ())))

(define-exported-function file-as-string (path &key (line-breaks nil))
  (let ((return-string nil))
    (dolist (line (file-as-list path))
      (setf return-string (concatenate 'string return-string line (when line-breaks (list #\Newline)))))
    return-string))

(define-exported-function file-extension-to (filename extension)
  (let ((dot-index (search "." (reverse filename))))
    (when dot-index
      (concatenate 'string 
                   (subseq filename 0 (- (length filename) dot-index 1))
                   "."
                   extension))))

(define-exported-function file-extension (filename)
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (let ((dot-index       (search "." (reverse filename)))
        (length-filename (length filename)))
    (when dot-index
      (subseq filename (- length-filename dot-index)
                       length-filename))))

(define-exported-function write-list-to-file (list filename &key (if-exists         :supersede)
                                                                 (if-does-not-exist :create))
  (with-open-file (stream filename
                          :direction         :output
                          :if-exists         if-exists
                          :if-does-not-exist if-does-not-exist)
    (dolist (line list)
      (format stream "~a~%" line))))

(define-exported-function load-when-exists (path)
  (when (probe-file path)
    (load path)))

(define-exported-function format-dashes (&optional (stream t))
  (format stream "----------------------------------------~%"))

(define-exported-function parse-integer-or-nil (string)
  (handler-case (parse-integer string)
    (SB-INT:SIMPLE-PARSE-ERROR
      nil)))

(define-exported-function string-or-empty (string)
  (or string ""))

