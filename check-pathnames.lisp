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
;;;;    2010-11-17 <PJB> Created.
;;;;BUGS
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


(format t "check-pathnames of ~A (~A)~%"
        (lisp-implementation-type)
        (lisp-implementation-version))

(write-string "
================================================================================

Test and probe conforming logical pathnames, and their translation to
unix physical pathnames.

We want to check the good working of logical pathnames, and the
translation of logical pathnames to physical pathnames, in a
semi-standard way on unix systems.

Namely, given the logical host and its translations:

  (setf (logical-pathname-translations \"LOGICAL\") nil)
  (setf (logical-pathname-translations \"LOGICAL\") 
        '((#P\"LOGICAL:**;*.*\" #P\"/tmp/**/*.*\")
          (#P\"LOGICAL:**;*\"   #P\"/tmp/**/*\")))

#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\"
must be the same as (make-pathname :host \"LOGICAL\"
                                   :directory '(:absolute \"DIR\" \"SUBDIR\")
                                   :name \"NAME\" :type \"TYPE\" :version :newest
                                   :case :common)
and must translate to: #P\"/tmp/dir/subdir/name.type\" on unix.



Merging physical pathnames specified with :case :common is also tested:

  (merge-pathnames (make-pathname :directory '(:relative \"DIR\" \"SUBDIR\")
                                  :name \"NAME\" :type \"TYPE\" :version :newest
                                  :case :common :default #1=#P\"/tmp/\")
                    #1# nil)

must give #P\"/tmp/dir/subdir/name.type\" on unix.

================================================================================
")



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



(defun equiv (a b) (or (and a b) (and (not a) (not b))))
(defun xor   (a b) (or (and a (not b)) (and (not a) b)))
(defun imply (a b) (or (not a) b))

(defmacro show (&body expressions)
  (let ((width (reduce (function max)
                       (mapcar (lambda (expr) (length (format nil "~S" expr)))
                               expressions)
                       :initial-value 0)))
    `(progn
       ,@(mapcar
          (lambda (expr) 
            `(let ((vals  (multiple-value-list ,expr)))
               (format *trace-output* 
                 ,(format nil "~~~DS = ~~{~~S~~^ ; ~~%~:*~VA   ~~}~~%" width "")
                 (quote ,expr) vals)
               (values-list vals)))
          expressions))))


(defun justify (control-string &rest arguments)
  (flet ((split-string (string separators)
           (loop
              :for start = (position-if-not (lambda (ch) (find ch separators)) string)
              :then (position-if-not (lambda (ch) (find ch separators)) string :start end) 
              :for end = (and start (position-if (lambda (ch) (find ch separators)) string :start start))
              :while start :collect (subseq string start end)))
         (make-circular-list (list)
           (setf (cdr (last list)) list)
           list))
    (format t "~{~<~%~1,v:;~a~>~^ ~}"
            (mapcan (function list)
                    (make-circular-list (list (or *print-right-margin* 80)))
                    (split-string (format nil "~?" control-string arguments)
                                  #(#\space #\newline))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun print-pathname (pathname)
    (format t "~%~A ~S~%" (type-of pathname) pathname)
    (format t "--------------------  :case :local (default)~%")
    (format t "~&~{~{~@(~9A~) : ~S~&~}~}"
            (mapcar (lambda (name field) (list name (funcall field pathname)))
                    '(host device directory name type version)
                    '(pathname-host pathname-device pathname-directory
                      pathname-name pathname-type pathname-version)))
    (format t "--------------------  :case :common~%")
    (format t "~&~{~{~@(~9A~) : ~S~&~}~}"
            (mapcar (lambda (name field arguments) (list name (apply field pathname arguments)))
                    '(host device directory name type version)
                    '(pathname-host pathname-device pathname-directory
                      pathname-name pathname-type pathname-version)
                    '((:case :common) (:case :common) (:case :common)
                      (:case :common) (:case :common) ())))
    (format t "--------------------  ~%")
    pathname))


(defmacro check (expression &body comment-on-false)
  (if (<= 3 (length expression ))
      (destructuring-bind (equal left right &rest arguments) expression
        (let ((vleft  (gensym))
              (vright (gensym)))
          `(let ((,vleft ,left)
                 (,vright ,right))
             (unless (,equal ,vleft ,vright ,@arguments)
               (format t "~&~80,,,'-A~%" "")
               (format t "Failed assertion: ~S~%   with: ~S = ~S~%    and: ~S = ~S~%"
                       '(,equal ,left ,right ,@arguments)
                       ',left ,vleft
                       ',right ,vright)
               (progn ,@comment-on-false)
               (when (pathnamep ,vleft)  (print-pathname ,vleft))
               (when (pathnamep ,vright) (print-pathname ,vright))))))
      `(unless ,expression
        (format t "~&~80,,,'-A~%" "")
        (format t "Failed assertion: ~S~%" ',expression)
        ,@comment-on-false)))




(defun customary-case ()
  (let ((path (make-pathname :host "LOGICAL"
                             :device :unspecific
                             :directory '(:absolute)
                             :name "NAME"
                             :type "TYPE"
                             :version nil ; instead of :newest to avoid bug in sbcl
                             :case :common))
        (customary-case-1 nil)
        (customary-case-2 nil))
    
    (let ((name (pathname-name path :case :local)))
      (cond
        ((string= name "NAME") (setf customary-case-1 :upper))
        ((string= name "name") (setf customary-case-1 :lower))))

    (let ((namestring (namestring (translate-logical-pathname path))))
      (cond
        ((search "NAME" namestring) (setf customary-case-2 :upper))
        ((search "name" namestring) (setf customary-case-2 :lower))))

    (when (and customary-case-1 customary-case-2)
      (check (imply (eql customary-case-1 :upper)
                    (eql customary-case-2 :upper))
             (show customary-case-1 customary-case-2))
      (check (imply (eql customary-case-1 :lower)
                    (eql customary-case-2 :lower))
             (show customary-case-1 customary-case-2)))
    
    (or customary-case-2
        customary-case-1)))


(defparameter *customary-case* (customary-case))


(format t "~3%")
(justify "The customary case for the file system of ~A (~A) seems to be ~A.~2%"
         (lisp-implementation-type) (lisp-implementation-version)
         (ecase *customary-case*
           ((:upper) "upper case")
           ((:lower) "lower case")
           ((nil)    "indeterminate")))
(format t "~3%")
(show *features*)
(format t "~3%")

(check (member *customary-case* '(:upper :lower))
       (justify "~A (~A) doesn't seem to have a definite customary case.
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
  (assert (string= (pathname-host path :case :common) "LOGICAL"))
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
    ((symbolp (first a))            (and (eql (first a) (first b)) (dirlist= (rest a) (rest b))))
    ((string= (first a) (first b))  (dirlist= (rest a) (rest b)))
    (t                              nil)))

(defun compare-pathnames (p1 p2)
  (flet ((compare (name field)
           (unless (equal (funcall field p1) (funcall field p2))
             (format t "~&~A DIFFERENT: ~A /= ~A~%"
                     name (funcall field p1) (funcall field p2)))))
    (compare 'host      (function pathname-host))
    (compare 'device    (function pathname-device))
    (compare 'directory (function pathname-directory))
    (compare 'name      (function pathname-name))
    (compare 'type      (function pathname-type))
    (compare 'version   (function pathname-version))))

(defun pathname-equal (p1 p2 &key (case :local))
  (flet ((field-equal (field)
           (equal (funcall field p1)
                  (funcall field p2))))
    (and (field-equal (lambda (p) (pathname-host      p :case case)))
         (field-equal (lambda (p) (pathname-device    p :case case)))
         (field-equal (lambda (p) (pathname-directory p :case case)))
         (field-equal (lambda (p) (pathname-name      p :case case)))
         (field-equal (lambda (p) (pathname-type      p :case case)))
         (field-equal (function pathname-version)))))


#+unix (check (eql *customary-case* :lower)
             (justify "99% of the unix path names are entirely lower case, so the customary case for
an implementation on unix should be lower case.~%"))



(flet ((check-path (path-form
                    path host device directory name type version case
                    expected-printed)

         (format t "~2%~80,,,'=A~%" "")
         (format t "~S~2%" path-form)
         (print-pathname path)

         (let ((expected-values (cond
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
                                          (string-downcase type))))))

           (when (typep path 'logical-pathname)
             (check (typep (pathname-host path :case :common) 'string)
                    (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION
pathname-host pathname &key case => host
host---a valid pathname host. 

valid logical pathname host n. a string that has been defined as the
name of a logical host.  See the function
load-logical-pathname-translations.
"))
             (check (typep (pathname-host path :case :local) 'string)
                    (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION
pathname-host pathname &key case => host
host---a valid pathname host. 

valid logical pathname host n. a string that has been defined as the
name of a logical host.  See the function
load-logical-pathname-translations.
"))
             (if (stringp (pathname-host      path :case :common))
               (check  (string=  (pathname-host      path :case :common) (pop expected-values))
                       (write-line "19.2.2.1.2  makes no exception for pathname-host of logical pathnames."))
               (pop expected-values)))
           
           (check  (dirlist= (pathname-directory path :case :common) (pop expected-values)))
           (check  (string=  (pathname-name      path :case :common) (pop expected-values)))
           (check  (string=  (pathname-type      path :case :common) (pop expected-values))))

         (if (typep path 'logical-pathname)
             (check  (eql      (pathname-device    path :case :common) :unspecific)
                     (write-string "
19.3.2.1 Unspecific Components of a Logical Pathname

The device component of a logical pathname is always :unspecific; no
other component of a logical pathname can be :unspecific.
"))
             (check  (eql      (pathname-device    path :case :common) device)))


         (check  (equiv    (position (pathname-version   path) '(nil :newest))
                           (position version                   '(nil :newest)))
                 (show version))


         (check ((lambda (printed expected-printed)
                   (find-if (lambda (expected) (string-equal expected printed)) expected-printed))
                 (prin1-to-string path)
                 expected-printed))
         
         (when (find-if (lambda (expected) (string-equal expected (prin1-to-string path))) expected-printed)
           (check ((lambda (printed expected-printed)
                     (find-if (lambda (expected) (string= expected printed)) expected-printed))
                   (prin1-to-string path)
                   expected-printed)
                  (write-string "
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

         
         (check (pathname-equal path
                                (read-from-string (let ((*print-readably* t)) (prin1-to-string path)))
                                :case :local)
                (write-line "See what I mean?"))
         (check (pathname-equal path
                                (read-from-string (let ((*print-readably* t)) (prin1-to-string path)))
                                :case :common)
                (write-line "See what I mean?"))

         (when (stringp (pathname-host      path :case :local))
           (check  (string=  (pathname-host      path :case :local) (pathname-host      path))
                   (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

case---one of :local or :common. The default is :local.
")))

         (if (stringp (pathname-device path))
             (check  (string=  (pathname-device    path :case :local) (pathname-device    path)))
             (check  (eql      (pathname-device    path :case :local) (pathname-device    path))))
         (check  (dirlist= (pathname-directory path :case :local) (pathname-directory path))
                 (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

case---one of :local or :common. The default is :local.
"))
         (check  (string=  (pathname-name      path :case :local) (pathname-name      path))
                 (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

case---one of :local or :common. The default is :local.
"))
         (check  (string=  (pathname-type      path :case :local) (pathname-type      path))
                 (write-string "
Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

case---one of :local or :common. The default is :local.
"))
         

         (when *customary-case*
           (let ((expected-values (cond
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
                                            (string-downcase type))))))
             (if (stringp (pathname-host      path :case :common))
                 (check  (string=  (pathname-host      path :case :local) (pop expected-values))
                         (write-line "19.2.2.1.2  makes no exception for pathname-host of logical pathnames."))
                 (pop expected-values))
             (check  (dirlist= (pathname-directory path :case :local) (pop expected-values)))
             (check  (string=  (pathname-name      path :case :local) (pop expected-values)))
             (check  (string=  (pathname-type      path :case :local) (pop expected-values))))
           
           )
         (terpri)))
  
  (let ((host "LOGICAL")
        (device :unspecific)
        (directory  '(:absolute "DIR" "SUBDIR"))
        (name "NAME")
        (type "TYPE")
        (version :newest)
        (case :common))
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


(check (pathname-equal (logical-pathname "LOGICAL:dir;subdir;name.type.newest")
                       (logical-pathname "LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"))
      (write-string "
    19.3.1.1.7 Lowercase Letters in a Logical Pathname Namestring
    When parsing words and wildcard-words, lowercase letters are translated to uppercase.
"))

(check (pathname-equal #P"LOGICAL:dir;subdir;name.type.newest"
                       #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST")
       (write-string "
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





#|
clisp:

- I agree, I was wrong, 19.3.1.1.7 doesn't apply.

- make-pathname creates a logical pathname when the :host parameter is
a logical host, in clisp, as in any conforming implementation.

- I was also wrong with:
(pathname-name (make-pathname :host "TEST" :name  "FILE" :type "DATA" :case :common))
it should indeed return "file"; this is correctly implemented in clisp.

- but the other form, (pathname-name #P"TEST:FILE.DATA") should return
"file", not "FILE", because by default the :case parameter of
pathname-name is :local, and "19.2.2.1.2.2 Common Case in Pathname
Components" mentions that round trip should be information
preserving (including case for local pathnames).


Actually there are other problems.  In an implementation where the
customary case is lower case, the pathname accessors cannot return the
same case with and without :case :common, but this is what clisp does:


CL-USER> (print-pathname (make-pathname :host "LOGICAL" :device :unspecific :directory '(:absolute "DIR" "SUBDIR") :name  "NAME" :type "DATA" :version nil :case :common))

LOGICAL-PATHNAME #P"LOGICAL:dir;subdir;name.data"
--------------------  :case :local (default)
Host      : "LOGICAL"
Device    : :UNSPECIFIC
Directory : (:ABSOLUTE "dir" "subdir")
Name      : "name"
Type      : "data"
Version   : NIL
--------------------  :case :common
Host      : "LOGICAL"
Device    : :UNSPECIFIC
Directory : (:ABSOLUTE "dir" "subdir")
Name      : "name"
Type      : "data"
Version   : NIL
--------------------  
#P"LOGICAL:dir;subdir;name.data"
CL-USER> (print-pathname #P"LOGICAL:dir;subdir;name.data")

LOGICAL-PATHNAME #P"LOGICAL:DIR;SUBDIR;NAME.DATA"
--------------------  :case :local (default)
Host      : "LOGICAL"
Device    : :UNSPECIFIC
Directory : (:ABSOLUTE "DIR" "SUBDIR")
Name      : "NAME"
Type      : "DATA"
Version   : NIL
--------------------  :case :common
Host      : "LOGICAL"
Device    : :UNSPECIFIC
Directory : (:ABSOLUTE "DIR" "SUBDIR")
Name      : "NAME"
Type      : "DATA"
Version   : NIL
--------------------  
#P"LOGICAL:DIR;SUBDIR;NAME.DATA"
CL-USER> (print-pathname #P"LOGICAL:DIR;SUBDIR;NAME.DATA")

LOGICAL-PATHNAME #P"LOGICAL:DIR;SUBDIR;NAME.DATA"
--------------------  :case :local (default)
Host      : "LOGICAL"
Device    : :UNSPECIFIC
Directory : (:ABSOLUTE "DIR" "SUBDIR")
Name      : "NAME"
Type      : "DATA"
Version   : NIL
--------------------  :case :common
Host      : "LOGICAL"
Device    : :UNSPECIFIC
Directory : (:ABSOLUTE "DIR" "SUBDIR")
Name      : "NAME"
Type      : "DATA"
Version   : NIL
--------------------  
#P"LOGICAL:DIR;SUBDIR;NAME.DATA"



There is also the problem that the result of:
(make-pathname :host "LOGICAL" :device :unspecific :directory '(:absolute "DIR" "SUBDIR") :name  "NAME" :type "DATA" :version nil :case :common)
is printed as #P"LOGICAL:dir;subdir;name.data".

It should be printed using the logical pathname syntax, that is, in
upper case, because:

1- it is a logical pathname,

2- its printed form should be readable as the 'same' logical pathname,
that is, a pathname with the same components,

3- #P gives the string to parse-namestring, without addionnal
argument, therefore :host is nil, and in this case, for the string
to be parsed as a logical pathname, it needs to have the logical
pathname syntax, which takes only upper case letters.

4- when reading back this lowercase form, we don't read the same
pathname as originally returned by the make-pathname form.


|#

(check (pathname-equal
        #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"
        (make-pathname :host "LOGICAL"
                       :device :unspecific
                       :directory '(:absolute "DIR" "SUBDIR")
                       :name "NAME"
                       :type "TYPE"
                       :version :newest
                       :case :common)))


(check (pathname-match-p  #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"
                          #P"LOGICAL:**;*.*")
       (show (logical-pathname-translations "LOGICAL"))
       (write-string "
    Function PATHNAME-MATCH-P
    pathname-match-p pathname wildcard => generalized-boolean

    pathname-match-p returns true if pathname matches wildcard, otherwise
    nil. The matching rules are implementation-defined but should be
    consistent with directory.
    Missing components of wildcard default to :wild.


Therefore a wildcard of #P\"LOGICAL:**;*.*\" should be equivalent to #P\"LOGICAL:**;*.*.*\"
and should match  #P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\".
"))


(check (ignore-errors (translate-logical-pathname #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"))
       (princ (nth-value 1 (ignore-errors (translate-logical-pathname #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"))))
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


Therefore a wildcard of #P\"LOGICAL:**;*.*\" should be equivalent to
#P\"LOGICAL:**;*.*.*\" and should match
#P\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\".

and:

    Function TRANSLATE-PATHNAME

    The resulting pathname is to-wildcard with each wildcard or missing
    field replaced by a portion of source.

Therefore whether you consider nil or :wild in the to-wildcard, the
:newer in the from-wildcard should match and replace it!
"))


(when (and (pathname-match-p  #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST" #P"LOGICAL:**;*.*")
           (ignore-errors (translate-logical-pathname #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST")))
 (check (pathname-equal
         (translate-logical-pathname #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST")
         (translate-logical-pathname (make-pathname :host "LOGICAL"
                                                    :device :unspecific
                                                    :directory '(:absolute "DIR" "SUBDIR")
                                                    :name "NAME"
                                                    :type "TYPE"
                                                    :version :newest
                                                    :case :common)))))



#+unix
(check ((lambda (dirlist path) (equal dirlist (pathname-directory path :case :local)))
        '(:absolute "tmp")
        (make-pathname :name "NAME" :type "TYPE"
                       :case :common
                       :defaults #P"/tmp/"))
       (write-string "MAKE-PATHNAME :CASE parameter does not apply on the :DEFAULTS parameter!
\"19.2.2.1.2 Case in Pathname Components\"
"))

#+unix
(check ((lambda (dirlist path) (equal dirlist (pathname-directory path :case :local)))
        '(:absolute "tmp" "dir" "subdir")
        (merge-pathnames
         (make-pathname :directory '(:relative "DIR" "SUBDIR")
                        :name "NAME" :type "TYPE"
                        :case :common
                        :defaults #P"/tmp/")
         #P"/tmp/" nil)))

#+unix
(check ((lambda (name path) (equal name (pathname-name path :case :local)))
        "name"
        (merge-pathnames
         (make-pathname :directory '(:relative "DIR" "SUBDIR")
                        :name "NAME" :type "TYPE"
                        :case :common
                        :defaults #P"/tmp/")
         #P"/tmp/" nil)))

#+unix
(check ((lambda (type path) (equal type (pathname-type path :case :local)))
        "type"
        (merge-pathnames
         (make-pathname :directory '(:relative "DIR" "SUBDIR")
                        :name "NAME" :type "TYPE"
                        :case :common
                        :defaults #P"/tmp/")
         #P"/tmp/" nil)))



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
;;         (translate-logical-pathname #P"LOGICAL:DIR;SUBDIR;NAME.TYPE")))
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
;;                          nil)))
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

;;;; THE END ;;;;
