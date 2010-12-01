;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               check-pathnames.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test and probe conforming logical pathnames,
;;;;    and their translation to unix physical pathnames.
;;;;
;;;;    We want to check the good working of logical pathnames,
;;;;    and the translation of logical pathnames to physical pathnames,
;;;;    in a semi-standard way on unix systems.
;;;;
;;;;    Namely:
;;;;
;;;;    #P"LOGHOST:DIR;SUBDIR;NAME.TYPE.NEWEST"
;;;;    must be the same as (make-pathname :host "LOGHOST"
;;;;                                       :directory '(:absolute "DIR" "SUBDIR")
;;;;                                       :name "NAME" :type "TYPE" :version :newest
;;;;                                       :case :common)
;;;;
;;;;    and given the translations:
;;;;        
;;;;        (setf (logical-pathname-translations "LOGHOST") nil)
;;;;        (setf (logical-pathname-translations "LOGHOST") 
;;;;              '((#P"LOGHOST:**;*.*" #P"/tmp/**/*.*")
;;;;                (#P"LOGHOST:**;*"   #P"/tmp/**/*")))
;;;;
;;;;    will translate to: #P"/tmp/dir/subdir/name.type" on unix.
;;;;    and similarly on MS-DOS/MS-Windows.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-30 <PJB> Added checks for translate-pathname.
;;;;                     Formated output as reStructured Text
;;;;                     to improve readability.
;;;;
;;;;    2010-11-27 <PJB> Relaxed (wrong) requirements on
;;;;                     (pathname-* logical-pathname :case :local)
;;;;                     These calls are not clearly specified, and
;;;;                     therefore not made anymore.
;;;; 
;;;;    2010-11-17 <PJB> Created.
;;;;BUGS
;;;;
;;;;    reStructured Text uses stars and some other character as markup,
;;;;    and therefore must be escaped.  This is unfortunate since we have
;;;;    quite a number of stars in the output, and escaping them renders
;;;;    the rst output less readable (but the pdf is ok).
;;;;    It would be nice to have an option to output pure text or rst source.
;;;;
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(defpackage "CHECK-PATHNAMES"
  (:use "COMMON-LISP"))
(in-package "CHECK-PATHNAMES")


(setf *print-circle* t
      *print-right-margin* 80
      *print-pretty* t
      *print-readably* nil)


(defun make-circular-list (list)
  (setf (cdr (last list)) list)
  list)

(defun split-string (string separators)
  (loop
     :for start = (position-if-not (lambda (ch) (find ch separators)) string)
     :then (position-if-not (lambda (ch) (find ch separators)) string :start end) 
     :for end = (and start (position-if (lambda (ch) (find ch separators)) string :start start))
     :while start
     :collect (subseq string start end)
     :while end))


(defun title (above below control-string &rest arguments)
  (let ((title (format nil "~?" control-string arguments)))
    (format t "~2%")
    (when above
      (format t "~A~%" (make-string (length title) :initial-element above)))
    (write-line title)
    (format t "~A~2%" (make-string (length title) :initial-element below))))


(defun comment (control-string &rest arguments)
  (let ((lines (split-string (quote-rst-text (format nil "~?" control-string arguments)) #(#\newline))))
    (if (= 1 (length lines))
        (format t "~&.. comment ~A~%" (first lines))
        (format t "~&.. comment~2%~{    ~A~%~}~%" lines))))

(defmacro with-comment (&body body)
  `(comment "~A" (with-output-to-string (*standard-output*) ,@body)))

(defun paragraph (control-string &rest arguments)
  (format t "~%~?~%" control-string arguments))

(defun quote-rst-text (text)
  (with-output-to-string (*standard-output*)
    (loop
       :for ch :across text
       :do (case ch
             ((#\* #\` #\\) (princ "\\") (princ ch))
             (otherwise (princ ch))))))

(defun code-quote (control-string &rest arguments)
  (format t "~2%~{|    ~A~%~}~%"
          (split-string (quote-rst-text (format nil "~?" control-string arguments)) #(#\newline))))

(defmacro with-code-quote (&body body)
  `(code-quote "~A" (with-output-to-string (*standard-output*) ,@body)))



(defmacro show (&body expressions)
  (let ((width (reduce (function max)
                       (mapcar (lambda (expr) (length (format nil "~S" expr)))
                               expressions)
                       :initial-value 0)))
    `(with-code-quote
       ,@(mapcar
          (lambda (expr) 
            `(let ((vals  (multiple-value-list ,expr)))
               (format t ,(format nil "~~~DS = ~~{~~S~~^ ; ~~%~:*~VA   ~~}~~%" width "")
                       (quote ,expr) vals)
               (values-list vals)))
          expressions)
       (terpri))))


(defun justify (control-string &rest arguments)
  (format t "~{~<~%~1,v:;~a~>~^ ~}"
          (mapcan (function list)
                  (make-circular-list (list (or *print-right-margin* 80)))
                  (split-string (format nil "~?" control-string arguments)
                                #(#\space #\newline)))))




(comment ".. comment -*- mode:rst -*-")
(comment "
Output of this script should be formated as a reStructured text,
so that it can be rendered nicely and readably.
")


(title #\# #\# "check-pathnames of ~A (~A)"
       (lisp-implementation-type)
       (lisp-implementation-version))


(title nil #\# "Table of Contents")
(write-line ".. sectnum::")
(write-line ".. contents::")



(title nil #\# "Introduction")

(paragraph "Test and probe conforming logical pathnames, and their
translation to unix physical pathnames.")

(paragraph "We want to check the good working of logical pathnames, and the
translation of logical pathnames to physical pathnames, in a
semi-standard way on unix systems.")

(paragraph "Namely, given the logical hosts and their translations:")

(code-quote "

  (setf (logical-pathname-translations \"LOGICAL\") nil)
  (setf (logical-pathname-translations \"LOGICAL\") 
        '((#P\"LOGICAL:**;*.*\" #P\"/tmp/**/*.*\")
          (#P\"LOGICAL:**;*\"   #P\"/tmp/**/*\")))

  (setf (logical-pathname-translations \"LOG1\") nil)
  (setf (logical-pathname-translations \"LOG1\") 
        '((#P\"LOG1:**;*.*\" #P\"/tmp/log1/**/*.*\"))

  (setf (logical-pathname-translations \"LOG2\") nil)
  (setf (logical-pathname-translations \"LOG2\") 
        '((#P\"LOG2:**;*.*\" #P\"/tmp/log2/**/*.*\"))
")

(paragraph "Then:")
(code-quote "#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\"")
(paragraph "must be the same as:")
(code-quote "
     (make-pathname :host \"LOGICAL\"
                    :directory '(:absolute \"DIR\" \"SUBDIR\")
                    :name \"NAME\" :type \"TYPE\" :version :newest
                    :case :common)")
(paragraph "and must translate to: #P\"/tmp/dir/subdir/name.type\" on unix.")

(paragraph "Merging physical pathnames specified with :case :common is also tested:")
(code-quote "
  (merge-pathnames (make-pathname :directory '(:relative \"DIR\" \"SUBDIR\")
                                  :name \"NAME\" :type \"TYPE\" :version :newest
                                  :case :common :default #1=#P\"/tmp/\")
                    #1# nil)
")
(paragraph "must give #P\"/tmp/dir/subdir/name.type\" on unix.")

(paragraph "(An empty section means that all tests passed successfully).")



(setf (logical-pathname-translations "LOGICAL") nil)

(setf (logical-pathname-translations "LOGICAL") 
      #+unix
      '((#P"LOGICAL:**;*.*" #P"/tmp/**/*.*")
        (#P"LOGICAL:**;*"   #P"/tmp/**/*"))
      #+(or win32 mswindows)
      '((#P"LOGICAL:**;*.*" #P"C:\\TEMP\\**\\*.*")
        (#P"LOGICAL:**;*"   #P"C:\\TEMP\\**\\*"))
      #-(or unix win32 mswindows)
      (error "Please add a pathname translation of the file system of this implementation."))


(setf (logical-pathname-translations "LOG1") '())
(setf (logical-pathname-translations "LOG1")
      #+unix '((#P"LOG1:**;*.*" #P"/tmp/log1/**/*.*"))
      #-unix (error "Please add a pathname translation of the file system of this implementation."))

(setf (logical-pathname-translations "LOG2") '())
(setf (logical-pathname-translations "LOG2")
      #+unix '((#P"LOG2:**;*.*" #P"/tmp/log2/**/*.*"))
      #-unix (error "Please add a pathname translation of the file system of this implementation."))


(defun equiv (a b) (or (and a b) (and (not a) (not b))))
(defun xor   (a b) (or (and a (not b)) (and (not a) b)))
(defun imply (a b) (or (not a) b))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun print-pathname (pathname)
    (format t "~&is a ~A: ~S~%" (type-of pathname) pathname)
    (unless (typep pathname 'logical-pathname)
      (format t "The fields of this pathname with :case :local (default) are:~%")
      (format t "~&~{~{    ~@(~9A~) : ~S~&~}~}"
              (mapcar (lambda (name field) (list name (funcall field pathname)))
                      '(host device directory name type version)
                      '(pathname-host pathname-device pathname-directory
                        pathname-name pathname-type pathname-version))))
    (format t "The fields of this pathname with :case :common are:~%")
    (format t "~&~{~{    ~@(~9A~) : ~S~&~}~}"
            (mapcar (lambda (name field arguments) (list name (apply field pathname arguments)))
                    '(host device directory name type version)
                    '(pathname-host pathname-device pathname-directory
                      pathname-name pathname-type pathname-version)
                    '((:case :common) (:case :common) (:case :common)
                      (:case :common) (:case :common) ())))
    (terpri)
    pathname))


(defmacro check (&key
                 name
                 with-pathnames ; predefined pathnames      (variable form)
                                        ; both unevaluated.
                 bind-pathnames ; new pathnames to be bound (variable expression)
                                        ; expression is evaluated and its result bound to variable.
                 ;; bind-pathnames works like LET*
                 assert         ; expression that must be true, otherwise
                                        ; a message is printed and
                 on-failure)    ; form executed when ASSERT is false.
  (unless assert (error ":ASSERT argument required by CHECK macro."))
  (if (and (null bind-pathnames) (<= 3 (length assert)))
      (let ((vleft  (make-symbol "LEFT  ARGUMENT "))
            (vright (make-symbol "RIGHT ARGUMENT ")))
        `(check :name ,name
                :bind-pathnames ((,vleft  ,(second assert))
                                 (,vright ,(third  assert)))
                :assert ,assert
                :on-failure ,on-failure))
      `(let* ,bind-pathnames
         (unless ,assert
           (title nil #\= "~@[Check ~A~]" ',name)
           (format t "Failed assertion: ")
           (code-quote "~S" ',assert)
           (terpri)
           ,@(mapcar (lambda (binding)
                       (destructuring-bind (pathvar pathform) binding
                         `(with-code-quote
                              (if (pathnamep ,pathvar)
                                  (progn
                                    (format t "~:(~A~) ~A = ~S~%"
                                            (type-of ,pathvar) ',pathvar ,pathform)
                                    (print-pathname ,pathvar))
                                  (progn
                                    (format t "~A = ~S =~%" ',pathvar ,pathform)
                                    (format t "~S~%" ,pathvar))))))
                     with-pathnames)
           ,@(mapcar (lambda (binding)
                       (destructuring-bind (pathvar pathexpr) binding
                         `(with-code-quote
                              (if (pathnamep ,pathvar)
                                  (progn
                                    (format t "~:(~A~) ~A = ~S~%"
                                            (type-of ,pathvar)  ',pathvar ',pathexpr)
                                    (print-pathname ,pathvar))
                                  (progn
                                    (format t "~A = ~S =~%" ',pathvar ',pathexpr)
                                    (format t "~S~%" ,pathvar))))))
                     bind-pathnames)
           ,on-failure))))



(defun customary-case ()
  (let* ((path (make-pathname :host "LOGICAL"
                              :device :unspecific
                              :directory '(:absolute)
                              :name "NAME"
                              :type "TYPE"
                              :version nil ; instead of :newest to avoid a bug in sbcl
                              :case :common))
         (customary-case nil)
         (ppath          (translate-logical-pathname path))
         (namestring     (namestring ppath)))

    
    (cond
      ((search "NAME" namestring) (setf customary-case :upper))
      ((search "name" namestring) (setf customary-case :lower)))

    
    (justify "With ~A (~A) on ~A, the customary case for the file system of the host ~S of the pathname ~S seems to be ~A.~%"
             (lisp-implementation-type) (lisp-implementation-version)
             (or ; the order implements some heuristic...
              #+genera         "Symbolics Genera"
              #+linux          "Linux"
              #+macosx         "MacOSX"
              #+darwin         "Darwin"
              #+bsd            "BSD Unix"
              #+unix           "Unix"
              #+win32          "MS-Windows"
              #+(or apple mac) "Macintosh"
              #+amiga          "Amiga OS"
              #+os/2           "OS/2"
              #+ACORN-RISCOS   "Acorn RISCOS"
              #+posix          "a POSIX system"
              "an undetermined system")
             (pathname-host ppath)
             namestring
             (ecase customary-case
               ((:upper) "upper case")
               ((:lower) "lower case")
               ((nil)    "indeterminate")))
    #+unix
    (progn
      (terpri)
      (if (eql customary-case :lower)
          (justify "Which was expected.")
          (justify "Which is unexpected.  For unix system the customary case should be lower.")))

    (terpri)
    customary-case))


(title nil #\# "Preliminary checks")

(defparameter *customary-case* (customary-case))
(format t "~3%")
(show *features*)
(format t "~3%")


(check :name customary-case
       :assert (member *customary-case* '(:upper :lower))
       :on-failure (justify "~A (~A) doesn't seem to have a definite customary case.
It should be upper or lower case!~%"
                            (lisp-implementation-type) (lisp-implementation-version)))





(defparameter *base-directory*
  #+unix                 #P"/tmp/"
  #+(or win32 mswindows) #P"C:\\TEMP\\"
  #-(or unix win32 mswindows)
  (error "Please give a base-directory pathname for the file system of this implementation."))


#+unix
(defun expected-translation (path)
  (assert (string= (pathname-host path :case :common) "LOGICAL"))
  (let ((directory (pathname-directory path :case :common))
        (name      (pathname-name      path :case :common))
        (type      (pathname-type      path :case :common))
        (direction (case *customary-case*
                     ((:upper) (function string-upcase))
                     ((:lower) (function string-downcase)))))
    (format nil "/tmp/~A" (funcall direction (format nil  "~{~A/~}~A.~A" (rest directory) name type)))))

#+(or win32 mswindows)
(defun expected-translation (path)
  (assert (string= (pathname-host      path :case :common) "LOGICAL"))
  (let ((directory (pathname-directory path :case :common))
        (name      (pathname-name      path :case :common))
        (type      (pathname-type      path :case :common))
        (direction (case *customary-case*
                     ((:upper) (function string-upcase))
                     ((:lower) (function string-downcase)))))
    (format nil "C:\\TEMP:\\~A" (funcall direction (format nil  "~{~A\\~}~A.~A" (rest directory) name type)))))

#-(or unix win32 mswindows)
(error "Please ad an expected-translation function for the file system of this implementation.")


(defun dirlist= (a b)
  (cond
    ((null a)                       (null b))
    ((stringp a)                    (dirlist= `(:absolute ,a) b))
    ((stringp b)                    (dirlist= a `(:absolute ,b)))
    ((symbolp (first a))            (and (eql (first a) (first b)) (dirlist= (rest a) (rest b))))
    ((string= (first a) (first b))  (dirlist= (rest a) (rest b)))
    (t                              nil)))

#+not-used
(defun compare-pathnames (p1 p2 &key (case :local))
  (flet ((compare (name equal field)
           (unless (funcall equal (funcall field p1) (funcall field p2))
             (format t "~&~A DIFFERENT: ~A /= ~A~%"
                     name (funcall field p1) (funcall field p2)))))
    (compare 'host      (function equal)    (lambda (p) (pathname-host      p :case case)))
    (compare 'device    (function equal)    (lambda (p) (pathname-device    p :case case)))
    (compare 'directory (function dirlist=) (lambda (p) (pathname-directory p :case case)))
    (compare 'name      (function equal)    (lambda (p) (pathname-name      p :case case)))
    (compare 'type      (function equal)    (lambda (p) (pathname-type      p :case case)))
    (compare 'version   (function equal)    (function pathname-version))))

(defun pathname-equal (p1 p2 &key (case :local))
  (cond
    ((not (pathnamep p1)) (error "PATHNAME-EQUAL first parameter is not a pathname but the ~S ~S" (type-of p1) p1))
    ((not (pathnamep p2)) (error "PATHNAME-EQUAL second parameter is not a pathname but the ~S ~S" (type-of p2) p2))
    (t (flet ((field-equal (equal field)
                (funcall equal (funcall field p1) (funcall field p2)))
              (device-equal (d1 d2)
                (cond
                  ((and (typep p1 'logical-pathname)
                        (typep p2 'logical-pathname))
                   (and (eql d1 :unspecific)
                        (eql d2 :unspecific)))
                  ((and (stringp d1) (stringp d2))
                   (string= d1 d2))
                  ((and (find d1 '(nil :unspecific))
                        (find d2 '(nil :unspecific))) t)
                  (t
                   (equal d1 d2)))))
         (let ((same (and (field-equal (function equal)        (lambda (p) (pathname-host      p :case case)))
                          (field-equal (function device-equal) (lambda (p) (pathname-device    p :case case)))
                          (field-equal (function dirlist=)     (lambda (p) (pathname-directory p :case case)))
                          (field-equal (function equal)        (lambda (p) (pathname-name      p :case case)))
                          (field-equal (function equal)        (lambda (p) (pathname-type      p :case case)))
                          (field-equal (function equal)        (function pathname-version)))))
           (unless (equiv same (equal p1 p2))
             (paragraph "CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:")
             `(with-code-quote
                  (progn
                    (format t "The first pathname is a ~:(~A~)~%" (type-of p1))
                    (print-pathname p1)))
             `(with-code-quote
                  (progn
                    (format t "The second pathname is a ~:(~A~)~%" (type-of p2))
                    (print-pathname p2)))
             (paragraph "CL:EQUAL says they're ~:[different~;same~]." (equal p1 p2))
             (paragraph "CHECK-PATHNAME:PATHNAME-EQUAL says they're ~:[different~;same~]." same))
           same)))))


#+unix (check :name customary-case-for-unix
              :assert (eql *customary-case* :lower)
              :on-failure (justify "99% of the unix path names are entirely lower case, so the customary case for
an implementation on unix should be lower case.~%"))




(flet ((check-path (path-form
                    path host device directory name type version case
                    expected-printed)

         (justify "We're considering the pathname built with:")
         (terpri)
         (with-code-quote
           (format t "~S" path-form)
           (print-pathname path))

         (destructuring-bind (host directory name type) (cond
                                                          ((eql case :common) (list host directory name type))
                                                          ((eql *customary-case* :lower)
                                                           (list  (string-upcase host)
                                                                  (cons (first directory)
                                                                        (mapcar (function string-upcase) (rest directory)))
                                                                  (string-upcase name)
                                                                  (string-upcase type)))
                                                          (t
                                                           (list  (string-downcase host)
                                                                  (cons (first directory)
                                                                        (mapcar (function string-downcase) (rest directory)))
                                                                  (string-downcase name)
                                                                  (string-downcase type))))

           (when (typep path 'logical-pathname)
             (check :name logical-host-must-be-a-string
                    :with-pathnames ((path path-form))
                    :assert (typep (pathname-host path :case :common) 'string)
                    :on-failure (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

pathname-host pathname &key case => host

host---a valid pathname host. 

valid logical pathname host n. a string that has been defined as the
name of a logical host.  See the function
load-logical-pathname-translations.
"))
             (when (stringp (pathname-host      path :case :common))
               (check :name logical-host-must-be-upper-case
                      :with-pathnames ((path path-form))
                      :assert (string=  (pathname-host      path :case :common) host)
                      :on-failure (write-line "19.2.2.1.2  makes no exception for pathname-host of logical pathnames."))))
           
           (check :name logical-pathname-directory
                  :with-pathnames ((path path-form))
                  :assert (dirlist= (pathname-directory path :case :common) directory))
           (check :name logical-pathname-name
                  :with-pathnames ((path path-form))
                  :assert (string=  (pathname-name      path :case :common) name))
           (check :name logical-pathname-type
                  :with-pathnames ((path path-form))
                  :assert (string=  (pathname-type      path :case :common) type)))

         (if (typep path 'logical-pathname)
             (check :name logical-pathname-device-must-be-unspecific
                    :with-pathnames ((path path-form))
                    :assert (eql      (pathname-device    path :case :common) :unspecific)
                    :on-failure (write-string "
19.3.2.1 Unspecific Components of a Logical Pathname

The device component of a logical pathname is always :unspecific; no
other component of a logical pathname can be :unspecific.
"))
             (check :name physical-pathname-device
                    :with-pathnames ((path path-form))
                    :assert (eql      (pathname-device    path :case :common) device)))


         (check :name pathname-version
                :with-pathnames ((path path-form))
                :assert (equiv  (find (pathname-version   path) '(nil :newest))
                                (find version                   '(nil :newest)))
                :on-failure (show version))


         (check :name printed-pathname-case-insensitive
                :with-pathnames ((path path-form))
                :assert ((lambda (printed expected-printed)
                           (find-if (lambda (expected) (string-equal expected printed)) expected-printed))
                         (prin1-to-string path)
                         expected-printed))
         
         (when (find-if (lambda (expected) (string-equal expected (prin1-to-string path))) expected-printed)
           (check :name printed-pathname-case-sensitive
                  :with-pathnames ((path path-form))
                  :assert ((lambda (printed expected-printed)
                             (find-if (lambda (expected) (string= expected printed)) expected-printed))
                           (prin1-to-string path)
                           expected-printed)
                  :on-failure (write-string "
It would be better if logical pathnames were printed using upper case
letters, mostly because of 19.3.1.1.7, and because:

    22.1.1 Overview of The Lisp Printer

    Reading a printed representation typically produces an object that is
    equal to the originally printed object. 

and

    2.4.8.14 Sharpsign P

    #P reads a following object, which must be a string.

    #P<<expression>> is equivalent to #.(parse-namestring '<<expression>>),
    except that #P is not affected by *read-eval*.

and

    Function PARSE-NAMESTRING

    * If host is nil and thing is a syntactically valid logical pathname
      namestring containing an explicit host, then it is parsed as a
      logical pathname namestring.

and

    19.3.1.1.7 Lowercase Letters in a Logical Pathname Namestring
    When parsing words and wildcard-words, lowercase letters are translated to uppercase.

Notice that means that a logical pathname built with mixed cases (or
lower case), cannot be printed readably with a conforming syntax (but
it doesn't matter, since it's not a conforming logical pathname
anyways).
")))

         (check :name read-printed-pathname/case-common
                :with-pathnames ((path              path-form))
                :bind-pathnames ((printed-read-path (read-from-string (let ((*print-readably* t)) (prin1-to-string path)))))
                :assert (pathname-equal path printed-read-path :case :common)
                :on-failure (write-line "See what I mean?"))
         (unless (typep path 'logical-pathname)
           (check :name read-printed-pathname/case-local
                  :with-pathnames ((path path-form))
                  :bind-pathnames ((printed-read-path (read-from-string (let ((*print-readably* t)) (prin1-to-string path)))))
                  :assert (pathname-equal path printed-read-path :case :local)
                  :on-failure (write-line "See what I mean?")))

         (unless (typep path 'logical-pathname)
           (when (stringp (pathname-host      path :case :local))
             (check :name physical-pathname-host/case-local
                    :with-pathnames ((path path-form))
                    :assert (string= (pathname-host      path :case :local) (pathname-host      path))
                    :on-failure (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

case---one of :local or :common. The default is :local.
")))

           (if (stringp (pathname-device path))
               (check :name pathname-device/case-local/string
                      :with-pathnames ((path path-form))
                      :assert (string=  (pathname-device    path :case :local) (pathname-device    path)))
               (check :name pathname-device/case-local/not-string
                      :with-pathnames ((path path-form))
                      :assert (eql      (pathname-device    path :case :local) (pathname-device    path))))
           (check :name pathname-directory/case-local
                  :with-pathnames ((path path-form))
                  :assert (dirlist= (pathname-directory path :case :local) (pathname-directory path))
                  :on-failure (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

case---one of :local or :common. The default is :local.
"))
           (check :name pathname-name/case-local
                  :with-pathnames ((path path-form))
                  :assert (string=  (pathname-name      path :case :local) (pathname-name      path))
                  :on-failure (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

case---one of :local or :common. The default is :local.
"))
           (check :name pathname-type/case-local
                  :with-pathnames ((path path-form))
                  :assert (string=  (pathname-type      path :case :local) (pathname-type      path))
                  :on-failure (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

case---one of :local or :common. The default is :local.
"))

           (when *customary-case*
             (destructuring-bind (host directory name type) (cond
                                                              ((eql case :local) (list host directory name type))
                                                              ((eql *customary-case* :upper)
                                                               (list  (string-upcase host)
                                                                      (cons (first directory)
                                                                            (mapcar (function string-upcase) (rest directory)))
                                                                      (string-upcase name)
                                                                      (string-upcase type)))
                                                              (t
                                                               (list  (string-downcase host)
                                                                      (cons (first directory)
                                                                            (mapcar (function string-downcase) (rest directory)))
                                                                      (string-downcase name)
                                                                      (string-downcase type))))
               (when (stringp (pathname-host      path :case :local))
                   (check :name pathname-host/case-local/customary-case
                          :with-pathnames ((path path-form))
                          :assert (string=  (pathname-host      path :case :local) host)
                          :on-failure (write-line "19.2.2.1.2  makes no exception for pathname-host.")))
               (check :name pathname-directory/case-local/customary-case
                      :with-pathnames ((path path-form))
                      :assert (dirlist= (pathname-directory path :case :local) directory))
               (check :name pathname-name/case-local/customary-case
                      :with-pathnames ((path path-form))
                      :assert (string=  (pathname-name      path :case :local) name))
               (check :name pathname-type/case-local/customary-case
                      :with-pathnames ((path path-form))
                      :assert (string=  (pathname-type      path :case :local) type)))))
         (terpri)))

  
  (let ((host "LOGICAL")
        (device :unspecific)
        (directory  '(:absolute "DIR" "SUBDIR"))
        (name "NAME")
        (type "TYPE")
        (version :newest)
        (case :common))

    (title nil #\# "Pathname Accessor Checks with :CASE :COMMON")
    
    (check-path `(make-pathname :host ,host
                                :device ,device
                                :directory ,directory
                                :name ,name
                                :type ,type
                                :version ,version
                                :case ,case)
                (make-pathname :host host
                               :device device
                               :directory directory
                               :name name
                               :type type
                               :version version
                               :case case)
                host device directory name type version case
                '("#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE\""
                  "#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\"")))
  
  (case  *customary-case*
    ;; We don't test the implementation dependant cases.
    
    ((:upper)
     (let ((host "LOGICAL")
           (device :unspecific)
           (directory  '(:absolute "DIR" "SUBDIR"))
           (name "NAME")
           (type "TYPE")
           (version :newest)
           (case :local))

       (title nil #\# "Pathname Accessor Checks with :CASE :LOCAL (customary case is UPPER)")
       (check-path `(make-pathname :host ,host
                                   :device ,device
                                   :directory ,directory
                                   :name ,name
                                   :type ,type
                                   :version ,version
                                   :case ,case)
                   (make-pathname :host host
                                  :device device
                                  :directory directory
                                  :name name
                                  :type type
                                  :version version
                                  :case case)
                   host device directory name type version case
                   '("#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE\""
                     "#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\""))))
    
    ((:lower)
     (let ((host "logical")
           (device :unspecific)
           (directory  '(:absolute "dir" "subdir"))
           (name "name")
           (type "type")
           (version :newest)
           (case :local))

       (title nil #\# "Pathname Accessor Checks with :CASE :LOCAL (customary case is lower)")
       (check-path `(make-pathname :host ,host
                                   :device ,device
                                   :directory ,directory
                                   :name ,name
                                   :type ,type
                                   :version ,version
                                   :case ,case)
                   (make-pathname :host host
                                  :device device
                                  :directory directory
                                  :name name
                                  :type type
                                  :version version
                                  :case case)

                   host device directory name type version case
                   ;; (string-upcase host)
                   ;; device
                   ;; (cons (first directory) (mapcar (function string-upcase) (rest directory)))
                   ;; (string-upcase name)
                   ;; (string-upcase type)
                   ;; version
                   ;; case
                   '("#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE\""
                     "#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\""))))))


(title nil #\# "Logical Pathname Construction Checks")

(check :name logical-pathname/upcasing
       :bind-pathnames ((logical-lower (logical-pathname "LOGICAL:dir;subdir;name.type.newest"))
                        (logical-upper (logical-pathname "LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST")))
       :assert (pathname-equal logical-lower logical-upper :case :common)
       :on-failure (write-string "
    19.3.1.1.7 Lowercase Letters in a Logical Pathname Namestring
    When parsing words and wildcard-words, lowercase letters are translated to uppercase.
"))


(check :name sharp-p/upcasing
       :bind-pathnames ((logical-lower (read-from-string "#P\"LOGICAL:dir;subdir;name.type.newest\""))
                        (logical-upper (read-from-string "#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\"")))
       :assert (pathname-equal logical-lower logical-upper :case :common)
       :on-failure (write-string "
    2.4.8.14 Sharpsign P

    #P reads a following object, which must be a string.

    #P<<expression>> is equivalent to #.(parse-namestring '<<expression>>),
    except that #P is not affected by *read-eval*.

and

    Function PARSE-NAMESTRING

    * If host is nil and thing is a syntactically valid logical pathname
      namestring containing an explicit host, then it is parsed as a
      logical pathname namestring.

and

    19.3.1.1.7 Lowercase Letters in a Logical Pathname Namestring
    When parsing words and wildcard-words, lowercase letters are translated to uppercase.
"))




(check :name sharp-p-vs-make-pathname
       :bind-pathnames ((read-pathname (read-from-string "#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\""))
                        (made-pathname (make-pathname :host "LOGICAL"
                                                      :device :unspecific
                                                      :directory '(:absolute "DIR" "SUBDIR")
                                                      :name "NAME"
                                                      :type "TYPE"
                                                      :version :newest
                                                      :case :common)))
       :assert (pathname-equal read-pathname made-pathname  :case :common))


(check :name sharp-p-vs-pathname-match-p
       :bind-pathnames ((pathname (read-from-string "#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\""))
                        (pattern  (read-from-string "#P\"LOGICAL:**;*.*\"")))
       :assert (pathname-match-p pathname pattern)
       :on-failure (progn
                     (show (logical-pathname-translations "LOGICAL"))
                     (write-string "
    Function PATHNAME-MATCH-P
    pathname-match-p pathname wildcard => generalized-boolean

    pathname-match-p returns true if pathname matches wildcard, otherwise
    nil. The matching rules are implementation-defined but should be
    consistent with directory.
    Missing components of wildcard default to :wild.


Therefore a wildcard of #P\"LOGICAL:\\*\\*;\\*.\\*\" should be equivalent to #P\"LOGICAL:\\*\\*;\\*.\\*.\\*\"
and should match  #P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\".
")))



(title nil #\# "Logical Pathname Translation Checks")

(check :name sharp-p-vs-translate-logical-pathname
       :bind-pathnames ((read-pathname (read-from-string "#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\"")))
       :assert (ignore-errors (translate-logical-pathname read-pathname))
       :on-failure (progn
                     (justify (princ-to-string (nth-value 1 (ignore-errors (translate-logical-pathname #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST")))))
                     (terpri)
                     (show (logical-pathname-translations "LOGICAL"))
                     (write-string "
    Function TRANSLATE-LOGICAL-PATHNAME

    Pathname is first coerced to a pathname. If the coerced pathname is a
    physical pathname, it is returned. If the coerced pathname is a
    logical pathname, the first matching translation (according to
    pathname-match-p) of the  logical pathname host is applied, as if by
    calling translate-pathname. If the result is a logical pathname, this
    process is repeated. When the result is finally a physical pathname,
    it is returned. If no translation matches, an error is signaled.

and:

    Function PATHNAME-MATCH-P
    pathname-match-p pathname wildcard => generalized-boolean

    pathname-match-p returns true if pathname matches wildcard, otherwise
    nil. The matching rules are implementation-defined but should be
    consistent with directory.
    Missing components of wildcard default to :wild.


Therefore a wildcard of #P\"LOGICAL:\\*\\*;\\*.\\*\" should be equivalent to
#P\"LOGICAL:\\*\\*;\\*.\\*.\\*\" and should match
#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\".

and:

    Function TRANSLATE-PATHNAME

    The resulting pathname is to-wildcard with each wildcard or missing
    field replaced by a portion of source.

Therefore whether you consider nil or :wild in the to-wildcard, the
:newer in the from-wildcard should match and replace it!
")))


(check :name translate-logical-pathnames/sharp-p-vs-make-pathname
       :bind-pathnames ((read-pathname (read-from-string "#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\""))
                        (pattern       (read-from-string "#P\"LOGICAL:**;*.*\""))
                        (made-pathname (make-pathname :host "LOGICAL"
                                                      :device :unspecific
                                                      :directory '(:absolute "DIR" "SUBDIR")
                                                      :name "NAME"
                                                      :type "TYPE"
                                                      :version :newest
                                                      :case :common)))
       :assert (or (not (and (pathname-match-p read-pathname pattern)
                             (ignore-errors (translate-logical-pathname read-pathname))))
                   (pathname-equal (translate-logical-pathname read-pathname)
                                   (translate-logical-pathname made-pathname)
                                   :case :local)))


#+unix
(title nil #\# "Unix Pathname Construction Checks")

#+unix
(check :name unix/make-pathname/case-common
       :bind-pathnames ((path (make-pathname :name "NAME" :type "TYPE"
                                             :case :common
                                             :defaults #P"/tmp/")))
       :assert (dirlist= '(:absolute "tmp") (pathname-directory path :case :local))
       :on-failure (write-string "MAKE-PATHNAME :CASE parameter does not apply on the :DEFAULTS parameter!
\"19.2.2.1.2 Case in Pathname Components\"
"))

#+unix
(check :name unix/merge-pathnames/make-pathname/case-common/directory
       :bind-pathnames ((path (merge-pathnames
                               (make-pathname :directory '(:relative "DIR" "SUBDIR")
                                              :name "NAME" :type "TYPE"
                                              :case :common
                                              :defaults #P"/tmp/")
                               #P"/tmp/" nil)))
       :assert (dirlist= '(:absolute "tmp" "dir" "subdir") (pathname-directory path :case :local)))

#+unix
(check :name unix/merge-pathnames/make-pathname/case-common/name
       :bind-pathnames ((path (merge-pathnames
                               (make-pathname :directory '(:relative "DIR" "SUBDIR")
                                              :name "NAME" :type "TYPE"
                                              :case :common
                                              :defaults #P"/tmp/")
                               #P"/tmp/" nil)))
       :assert (equal "name" (pathname-name path :case :local)))

#+unix
(check :name unix/merge-pathnames/make-pathname/case-common/type
       :bind-pathnames ((path (merge-pathnames
                               (make-pathname :directory '(:relative "DIR" "SUBDIR")
                                              :name "NAME" :type "TYPE"
                                              :case :common
                                              :defaults #P"/tmp/")
                               #P"/tmp/" nil)))
       :assert (equal "type" (pathname-type path :case :local)))



;; (print (make-pathname :directory '(:relative "SUB1" "SUB2")
;;                       :name "NAME"
;;                       :type "TYPE"
;;                       :case :common
;;                       :defaults *base-directory*))
;; 
;; (print (merge-pathnames (make-pathname :directory '(:relative "SUB1" "SUB2")
;;                                        :name "NAME"
;;                                        :type "TYPE"
;;                                        :case :common
;;                                        :defaults *base-directory*)
;;                         *base-directory*
;;                         nil))
;; 
;; (check (pathname-equal
;;         (merge-pathnames (make-pathname :directory '(:relative "DIR" "SUBDIR")
;;                                         :name "NAME"
;;                                         :type "TYPE"
;;                                         :case :common
;;                                         :defaults *base-directory*)
;;                          *base-directory*
;;                          nil)
;;         (translate-logical-pathname #P"LOGICAL:DIR;SUBDIR;NAME.TYPE")
;;         :case :local))
;; 
;; (check (pathname-equal
;;         (translate-logical-pathname (make-pathname :host "LOGICAL"
;;                                                    :device :unspecific
;;                                                    :directory '(:absolute "DIR" "SUBDIR")
;;                                                    :name "NAME"
;;                                                    :type "TYPE"
;;                                                    :version nil
;;                                                    :case :common))
;;         (merge-pathnames (make-pathname :directory '(:relative "DIR" "SUBDIR")
;;                                         :name "NAME"
;;                                         :type "TYPE"
;;                                         :case :common
;;                                         :defaults *base-directory*)
;;                          *base-directory*
;;                          nil)
;;         :case :local))
;; 
;; 
;; (print-pathname (make-pathname :host "LOGICAL"
;;                                :device :unspecific
;;                                :directory '(:absolute "DIR" "SUBDIR")
;;                                :name "NAME"
;;                                :type "TYPE"
;;                                :version nil
;;                                :case :common))
;; (print-pathname 
;;  (translate-logical-pathname (make-pathname :host "LOGICAL"
;;                                             :device :unspecific
;;                                             :directory '(:absolute "DIR" "SUBDIR")
;;                                             :name "NAME"
;;                                             :type "TYPE"
;;                                             :version nil
;;                                             :case :common)))
;; (print-pathname         (merge-pathnames (make-pathname :directory '(:relative "DIR" "SUBDIR")
;;                                                         :name "NAME"
;;                                                         :type "TYPE"
;;                                                         :case :common
;;                                                         :defaults *base-directory*)
;;                                          *base-directory*
;;                                          nil))





;; Some fun with translate-pathname


(title nil #\# "Checking Translations between logical pathnames")


(defun checked-translate-pathname (path from-pattern to-pattern)
  (handler-case (translate-pathname path from-pattern to-pattern)
    (error (err) (list :error (princ-to-string err)))))



(check :name translate-pathname/logical/self/i0/identity
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG1:**;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:ABC;DEF;NAME.TYP"))
       :assert (pathname-equal translated expected :case :common)
       :on-failure (justify "translate-pathname should work within the same logical host (identity)."))


(check :name translate-pathname/logical/self/a1/type
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG1:**;*.PYT")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:ABC;DEF;NAME.PYT"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))

(check :name translate-pathname/logical/self/a2/name
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG1:**;EMAN.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:ABC;DEF;EMAN.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))

(check :name translate-pathname/logical/self/a3/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG1:UVW;XYZ;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:UVW;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))




(check :name translate-pathname/logical/self/b1/type
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.TYP")
                        (to-pat     #P"LOG1:**;*.PYT")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:ABC;DEF;NAME.PYT"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))

(check :name translate-pathname/logical/self/b2/name
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;NAME.*")
                        (to-pat     #P"LOG1:**;EMAN.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:ABC;DEF;EMAN.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))

(check :name translate-pathname/logical/self/b3/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:ABC;DEF;*.*")
                        (to-pat     #P"LOG1:UVW;XYZ;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:UVW;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))




(check :name translate-pathname/logical/self/c1/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG1:**;SUBDIR;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:ABC;DEF;SUBDIR;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))

(check :name translate-pathname/logical/self/c2/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG1:SUBDIR;**;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:SUBDIR;ABC;DEF;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))

(check :name translate-pathname/logical/self/c3/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG1:UVW;**;XYZ;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:UVW;ABC;DEF;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))

(check :name translate-pathname/logical/self/c4/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:*;*;*.*")
                        (to-pat     #P"LOG1:UVW;*;XYZ;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:UVW;ABC;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))

(check :name translate-pathname/logical/self/c5/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:*;*;*.*")
                        (to-pat     #P"LOG1:RST;*;UVW;*;XYZ;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:RST;ABC;UVW;DEF;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))








(check :name translate-pathname/logical/logical/j0/identity
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG2:**;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:ABC;DEF;NAME.TYP"))
       :assert (pathname-equal translated expected  :case :common)
       :on-failure (justify "translate-pathname should work from logical host to logical host."))


(check :name translate-pathname/logical/logical/e1/type
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG2:**;*.PYT")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:ABC;DEF;NAME.PYT"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))

(check :name translate-pathname/logical/logical/e2/name
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG2:**;EMAN.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:ABC;DEF;EMAN.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))

(check :name translate-pathname/logical/logical/e3/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG2:UVW;XYZ;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:UVW;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))




(check :name translate-pathname/logical/logical/f1/type
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.TYP")
                        (to-pat     #P"LOG2:**;*.PYT")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:ABC;DEF;NAME.PYT"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))

(check :name translate-pathname/logical/logical/f2/name
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;NAME.*")
                        (to-pat     #P"LOG2:**;EMAN.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:ABC;DEF;EMAN.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))

(check :name translate-pathname/logical/logical/f3/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:ABC;DEF;*.*")
                        (to-pat     #P"LOG2:UVW;XYZ;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:UVW;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))




(check :name translate-pathname/logical/logical/g1/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG2:**;SUBDIR;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))

(check :name translate-pathname/logical/logical/g2/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG2:SUBDIR;**;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:SUBDIR;ABC;DEF;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))

(check :name translate-pathname/logical/logical/g3/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"LOG2:UVW;**;XYZ;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:UVW;ABC;DEF;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))

(check :name translate-pathname/logical/logical/g4/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:*;*;*.*")
                        (to-pat     #P"LOG2:UVW;*;XYZ;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:UVW;ABC;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))

(check :name translate-pathname/logical/logical/g5/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:*;*;*.*")
                        (to-pat     #P"LOG2:RST;*;UVW;*;XYZ;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:RST;ABC;UVW;DEF;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))




;; random tests...

(check :name translate-pathname/logical/self/r1/name
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;NAME.*")
                        (to-pat     #P"LOG1:**;EMAN.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:ABC;DEF;EMAN.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical host to logical host."))

(check :name translate-pathname/logical/logical/r2/type
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.TYP")
                        (to-pat     #P"LOG2:**;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:ABC;DEF;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical pathname to logical pathname."))

(check :name translate-pathname/logical/logical/r3/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.TYP")
                        (to-pat     #P"LOG2:**;SUBDIR;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical pathname to logical pathname."))




(check :name translate-pathname/logical/self/r4/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:ABC;DEF;*.*")
                        (to-pat     #P"LOG1:**;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG1:ABC;DEF;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work within the same logical host."))

(check :name translate-pathname/logical/logical/r5/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:ABC;DEF;*.*")
                        (to-pat     #P"LOG2:UVW;XYZ.*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:UVW;XYZ;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical pathname to logical pathname."))

(check :name translate-pathname/logical/logical/r6/directory
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.TYP")
                        (to-pat     #P"LOG2:**;SUBDIR;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :common))
       :on-failure (justify "translate-pathname should work from logical pathname to logical pathname."))




(title nil #\# "Checking Translations from logical pathname to physical pathname")


#+unix
(check :name unix/translate-pathname/logical/physical/1a
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"/tmp/log3/**/*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/abc/def/name.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))

#+unix
(check :name unix/translate-pathname/logical/physical/2a
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"/tmp/log3/**/xyz/*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/abc/def/xyz/name.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))

#+unix
(check :name unix/translate-pathname/logical/physical/3a
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"/tmp/log3/uvw/**/*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/uvw/abc/def/name.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))


#+unix
(check :name unix/translate-pathname/logical/physical/4a
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.*")
                        (to-pat     #P"/tmp/log3/uvw/**/xyz/*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/uvw/abc/def/xyz/name.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))


#+unix
(check :name unix/translate-pathname/logical/physical/5a
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:*;*;*.*")
                        (to-pat     #P"/tmp/log3/uvw/*/xyz/*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/uvw/abc/xyz/name.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))


#+unix
(check :name unix/translate-pathname/logical/physical/6a
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:*;*;*.*")
                        (to-pat     #P"/tmp/log3/rst/*/uvw/*/xyz/*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/rst/abc/uvw/def/xyz/name.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))





#+unix
(check :name unix/translate-pathname/logical/physical/1b
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:ABC;DEF;*.*")
                        (to-pat     #P"/tmp/log3/**/*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/abc/def/name.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))

#+unix
(check :name unix/translate-pathname/logical/physical/2b
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;NAME.*")
                        (to-pat     #P"/tmp/log3/**/*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/abc/def/name.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))

#+unix
(check :name unix/translate-pathname/logical/physical/3b
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.TYP")
                        (to-pat     #P"/tmp/log3/**/*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/abc/def/name.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))


#+unix
(check :name unix/translate-pathname/logical/physical/1c
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:ABC;DEF;*.*")
                        (to-pat     #P"/tmp/log3/uvw/xyz/*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/uvw/xyz/name.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))

#+unix
(check :name unix/translate-pathname/logical/physical/2c
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;NAME.*")
                        (to-pat     #P"/tmp/log3/**/eman.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/abc/def/eman.typ"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))

#+unix
(check :name unix/translate-pathname/logical/physical/3c
       :bind-pathnames ((path       #P"LOG1:ABC;DEF;NAME.TYP")
                        (from-pat   #P"LOG1:**;*.TYP")
                        (to-pat     #P"/tmp/log3/**/*.pyt")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected   #P"/tmp/log3/abc/def/name.pyt"))
       :assert (and (pathnamep translated) (pathname-equal translated expected  :case :local))
       :on-failure (justify "translate-pathname should work from logical pathname to physical pathname, translating to customary case."))



(title nil #\# "Checking Translations from physical pathname to logical pathname")


#+unix
(check :name unix/translate-pathname/physical/logical/1
       :bind-pathnames ((path       #P"/tmp/log3/abc/def/name.typ")
                        (from-pat   #P"/tmp/log3/**/*.*")
                        (to-pat     #P"LOG1:**;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected1  #P"LOG1:ABC;DEF;NAME.TYP")
                        (expected2  #P"LOG1:ABC;DEF;NAME.TYP.NEWEST"))
       :assert (and (pathnamep translated)
                    (or (pathname-equal translated expected1  :case :common)
                        (pathname-equal translated expected2  :case :common)))
       :on-failure (justify "translate-pathname should work from physical pathname to logical pathname, translating customary case."))

#+unix
(check :name unix/translate-pathname/physical/logical/2
       :bind-pathnames ((path       #P"/tmp/log3/abc/def/subdir/name.typ")
                        (from-pat   #P"/tmp/log3/**/subdir/*.*")
                        (to-pat     #P"LOG1:**;*.*")
                        (translated (checked-translate-pathname  path from-pat to-pat))
                        (expected1  #P"LOG1:ABC;DEF;NAME.TYP")
                        (expected2  #P"LOG1:ABC;DEF;NAME.TYP.NEWEST"))
       :assert (and (pathnamep translated)
                    (or (pathname-equal translated expected1  :case :common)
                        (pathname-equal translated expected2  :case :common)))
       :on-failure (justify "translate-pathname should work from physical pathname to logical pathname, translating customary case."))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-testing-script (progn
                   #+abcl    (extensions:quit)
                   #+allegro (excl:exit)
                   #+ccl     (ccl:quit)
                   #+clisp   (ext:quit)
                   #+cmu     (ext:quit)
                   #+ecl     (si:quit)
                   #+gcl     (lisp:quit)
                   #+sbcl    (sb-ext:quit))

;;;; THE END ;;;;
