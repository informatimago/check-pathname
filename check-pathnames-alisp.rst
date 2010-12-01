.. comment .. comment -\*- mode:rst -\*-
.. comment

    Output of this script should be formated as a reStructured text,
    so that it can be rendered nicely and readably.



########################################################################################################
check-pathnames of International Allegro CL Free Express Edition (8.2 [Linux (x86)] (Sep 11, 2010 7:36))
########################################################################################################



Table of Contents
#################

.. sectnum::
.. contents::


Introduction
############


Test and probe conforming logical pathnames, and their
translation to unix physical pathnames.

We want to check the good working of logical pathnames, and the
translation of logical pathnames to physical pathnames, in a
semi-standard way on unix systems.

Namely, given the logical hosts and their translations:


|      (setf (logical-pathname-translations "LOGICAL") nil)
|      (setf (logical-pathname-translations "LOGICAL") 
|            '((#P"LOGICAL:\*\*;\*.\*" #P"/tmp/\*\*/\*.\*")
|              (#P"LOGICAL:\*\*;\*"   #P"/tmp/\*\*/\*")))
|      (setf (logical-pathname-translations "LOG1") nil)
|      (setf (logical-pathname-translations "LOG1") 
|            '((#P"LOG1:\*\*;\*.\*" #P"/tmp/log1/\*\*/\*.\*"))
|      (setf (logical-pathname-translations "LOG2") nil)
|      (setf (logical-pathname-translations "LOG2") 
|            '((#P"LOG2:\*\*;\*.\*" #P"/tmp/log2/\*\*/\*.\*"))


Then:


|    #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"


must be the same as:


|         (make-pathname :host "LOGICAL"
|                        :directory '(:absolute "DIR" "SUBDIR")
|                        :name "NAME" :type "TYPE" :version :newest
|                        :case :common)


and must translate to: #P"/tmp/dir/subdir/name.type" on unix.

Merging physical pathnames specified with :case :common is also tested:


|      (merge-pathnames (make-pathname :directory '(:relative "DIR" "SUBDIR")
|                                      :name "NAME" :type "TYPE" :version :newest
|                                      :case :common :default #1=#P"/tmp/")
|                        #1# nil)


must give #P"/tmp/dir/subdir/name.type" on unix.

(An empty section means that all tests passed successfully).


Preliminary checks
##################

With International Allegro CL Free Express Edition (8.2 [Linux (x86)] (Sep 11, 
2010 7:36)) on Linux, the customary case for the file system of the host NIL of 
the pathname "/tmp/name.type" seems to be lower case.
Which was expected.





|    \*FEATURES\* = (:ALLEGRO-CL-EXPRESS :ALLEGRO-CL-TRIAL :IPV6 :ACL-SOCKET
|                  :HIPER-SOCKET :PROFILER :COMPILER :USE-STRUCTS-IN-COMPILER :CLOS
|                  :ATOMIC-SUBWORD-SETF :ATOMIC-SETF :DYNLOAD :DLFCN :UNIX :LINUX
|                  :REDHAT9 :LINUX86 :X86 :VERIFY-STACK :VERIFY-CAR-CDR
|                  :LITTLE-ENDIAN :ENCAPSULATING-EFS :RELATIVE-PACKAGE-NAMES
|                  :MODULE-VERSIONS :IEEE :IEEE-FLOATING-POINT :CONFORMING-IEEE
|                  :GNU-SOURCE :ICS :COMMON-LISP :ANSI-CL :DRAFT-ANSI-CL-2 :X3J13
|                  :ALLEGRO :EXCL :FRANZ-INC :ALLEGRO-VERSION>= :ALLEGRO-VERSION=
|                  :NEW-ENVIRONMENTS :GSGC :FLAVORS :MULTIPROCESSING
|                  :USE-THREAD-LIBS :DYNLOAD-ACL-LIBRARY :ALLEGRO-V8.2 :SMP-MACROS
|                  :SSL-SUPPORT)






Pathname Accessor Checks with :CASE :COMMON
###########################################

We're considering the pathname built with:


|    (MAKE-PATHNAME :HOST "LOGICAL"
|      :DEVICE :UNSPECIFIC
|      :DIRECTORY (:ABSOLUTE "DIR" "SUBDIR")
|      :NAME "NAME"
|      :TYPE "TYPE"
|      :VERSION :NEWEST
|      :CASE :COMMON)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



Check LOGICAL-HOST-MUST-BE-UPPER-CASE
=====================================

Failed assertion: 

|    (STRING= (PATHNAME-HOST PATH :CASE :COMMON) HOST)




|    LEFT  ARGUMENT  = (PATHNAME-HOST PATH :CASE :COMMON) =
|    "logical"



|    RIGHT ARGUMENT  = HOST =
|    "LOGICAL"

19.2.2.1.2  makes no exception for pathname-host of logical pathnames.


Check LOGICAL-PATHNAME-DEVICE-MUST-BE-UNSPECIFIC
================================================

Failed assertion: 

|    (EQL (PATHNAME-DEVICE PATH :CASE :COMMON) :UNSPECIFIC)




|    LEFT  ARGUMENT  = (PATHNAME-DEVICE PATH :CASE :COMMON) =
|    NIL



|    RIGHT ARGUMENT  = :UNSPECIFIC =
|    :UNSPECIFIC


19.3.2.1 Unspecific Components of a Logical Pathname

The device component of a logical pathname is always :unspecific; no
other component of a logical pathname can be :unspecific.


Check PATHNAME-VERSION
======================

Failed assertion: 

|    (EQUIV (FIND (PATHNAME-VERSION PATH) '(NIL :NEWEST))
|           (FIND VERSION '(NIL :NEWEST)))




|    LEFT  ARGUMENT  = (FIND (PATHNAME-VERSION PATH) '(NIL :NEWEST)) =
|    NIL



|    RIGHT ARGUMENT  = (FIND VERSION '(NIL :NEWEST)) =
|    :NEWEST



|    VERSION = :NEWEST



Check PRINTED-PATHNAME-CASE-SENSITIVE
=====================================

Failed assertion: 

|    ((LAMBDA (PRINTED EXPECTED-PRINTED)
|       (FIND-IF (LAMBDA (EXPECTED) (STRING= EXPECTED PRINTED)) EXPECTED-PRINTED))
|     (PRIN1-TO-STRING PATH) EXPECTED-PRINTED)




|    LEFT  ARGUMENT  = (PRIN1-TO-STRING PATH) =
|    "#P\\"LOGICAL:dir;subdir;name.type\\""



|    RIGHT ARGUMENT  = EXPECTED-PRINTED =
|    ("#P\\"LOGICAL:DIR;SUBDIR;NAME.TYPE\\""
|     "#P\\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\\"")


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

CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check READ-PRINTED-PATHNAME/CASE-COMMON
=======================================

Failed assertion: 

|    (PATHNAME-EQUAL PATH PRINTED-READ-PATH :CASE :COMMON)




|    Logical-Pathname PATH = (MAKE-PATHNAME :HOST "LOGICAL"
|                              :DEVICE :UNSPECIFIC
|                              :DIRECTORY (:ABSOLUTE "DIR" "SUBDIR")
|                              :NAME "NAME"
|                              :TYPE "TYPE"
|                              :VERSION :NEWEST
|                              :CASE :COMMON)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



|    Logical-Pathname PRINTED-READ-PATH = (READ-FROM-STRING
|                                            (LET ((\*PRINT-READABLY\* T))
|                                              (PRIN1-TO-STRING PATH)))
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL

See what I mean?



Pathname Accessor Checks with :CASE :LOCAL (customary case is lower)
####################################################################

We're considering the pathname built with:


|    (MAKE-PATHNAME :HOST "logical"
|      :DEVICE :UNSPECIFIC
|      :DIRECTORY (:ABSOLUTE "dir" "subdir")
|      :NAME "name"
|      :TYPE "type"
|      :VERSION :NEWEST
|      :CASE :LOCAL)
|    is a LOGICAL-PATHNAME: #P"logical:dir;subdir;name.type"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : NIL
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



Check LOGICAL-PATHNAME-DEVICE-MUST-BE-UNSPECIFIC
================================================

Failed assertion: 

|    (EQL (PATHNAME-DEVICE PATH :CASE :COMMON) :UNSPECIFIC)




|    LEFT  ARGUMENT  = (PATHNAME-DEVICE PATH :CASE :COMMON) =
|    NIL



|    RIGHT ARGUMENT  = :UNSPECIFIC =
|    :UNSPECIFIC


19.3.2.1 Unspecific Components of a Logical Pathname

The device component of a logical pathname is always :unspecific; no
other component of a logical pathname can be :unspecific.


Check PATHNAME-VERSION
======================

Failed assertion: 

|    (EQUIV (FIND (PATHNAME-VERSION PATH) '(NIL :NEWEST))
|           (FIND VERSION '(NIL :NEWEST)))




|    LEFT  ARGUMENT  = (FIND (PATHNAME-VERSION PATH) '(NIL :NEWEST)) =
|    NIL



|    RIGHT ARGUMENT  = (FIND VERSION '(NIL :NEWEST)) =
|    :NEWEST



|    VERSION = :NEWEST



Check PRINTED-PATHNAME-CASE-SENSITIVE
=====================================

Failed assertion: 

|    ((LAMBDA (PRINTED EXPECTED-PRINTED)
|       (FIND-IF (LAMBDA (EXPECTED) (STRING= EXPECTED PRINTED)) EXPECTED-PRINTED))
|     (PRIN1-TO-STRING PATH) EXPECTED-PRINTED)




|    LEFT  ARGUMENT  = (PRIN1-TO-STRING PATH) =
|    "#P\\"logical:dir;subdir;name.type\\""



|    RIGHT ARGUMENT  = EXPECTED-PRINTED =
|    ("#P\\"LOGICAL:DIR;SUBDIR;NAME.TYPE\\""
|     "#P\\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\\"")


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

CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check READ-PRINTED-PATHNAME/CASE-COMMON
=======================================

Failed assertion: 

|    (PATHNAME-EQUAL PATH PRINTED-READ-PATH :CASE :COMMON)




|    Logical-Pathname PATH = (MAKE-PATHNAME :HOST "logical"
|                              :DEVICE :UNSPECIFIC
|                              :DIRECTORY (:ABSOLUTE "dir" "subdir")
|                              :NAME "name"
|                              :TYPE "type"
|                              :VERSION :NEWEST
|                              :CASE :LOCAL)
|    is a LOGICAL-PATHNAME: #P"logical:dir;subdir;name.type"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : NIL
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



|    Logical-Pathname PRINTED-READ-PATH = (READ-FROM-STRING
|                                            (LET ((\*PRINT-READABLY\* T))
|                                              (PRIN1-TO-STRING PATH)))
|    is a LOGICAL-PATHNAME: #P"logical:dir;subdir;name.type"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : NIL
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL

See what I mean?



Logical Pathname Construction Checks
####################################



Check LOGICAL-PATHNAME/UPCASING
===============================

Failed assertion: 

|    (PATHNAME-EQUAL LOGICAL-LOWER LOGICAL-UPPER :CASE :COMMON)




|    Logical-Pathname LOGICAL-LOWER = (LOGICAL-PATHNAME "LOGICAL:dir;subdir;name.type.newest")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



|    Logical-Pathname LOGICAL-UPPER = (LOGICAL-PATHNAME "LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "dir" "subdir")
|        Name      : "name"
|        Type      : "type"
|        Version   : NIL


    19.3.1.1.7 Lowercase Letters in a Logical Pathname Namestring
    When parsing words and wildcard-words, lowercase letters are translated to uppercase.


Check SHARP-P/UPCASING
======================

Failed assertion: 

|    (PATHNAME-EQUAL LOGICAL-LOWER LOGICAL-UPPER :CASE :COMMON)




|    Logical-Pathname LOGICAL-LOWER = (READ-FROM-STRING
|                                        "#P\\"LOGICAL:dir;subdir;name.type.newest\\"")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



|    Logical-Pathname LOGICAL-UPPER = (READ-FROM-STRING
|                                        "#P\\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\\"")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "dir" "subdir")
|        Name      : "name"
|        Type      : "type"
|        Version   : NIL


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


Check SHARP-P-VS-MAKE-PATHNAME
==============================

Failed assertion: 

|    (PATHNAME-EQUAL READ-PATHNAME MADE-PATHNAME :CASE :COMMON)




|    Logical-Pathname READ-PATHNAME = (READ-FROM-STRING
|                                        "#P\\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\\"")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "dir" "subdir")
|        Name      : "name"
|        Type      : "type"
|        Version   : NIL



|    Logical-Pathname MADE-PATHNAME = (MAKE-PATHNAME :HOST "LOGICAL"
|                                       :DEVICE :UNSPECIFIC
|                                       :DIRECTORY '(:ABSOLUTE "DIR" "SUBDIR")
|                                       :NAME "NAME"
|                                       :TYPE "TYPE"
|                                       :VERSION :NEWEST
|                                       :CASE :COMMON)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



Logical Pathname Translation Checks
###################################



Check TRANSLATE-LOGICAL-PATHNAMES/SHARP-P-VS-MAKE-PATHNAME
==========================================================

Failed assertion: 

|    (OR (NOT (AND (PATHNAME-MATCH-P READ-PATHNAME PATTERN)
|                  (IGNORE-ERRORS (TRANSLATE-LOGICAL-PATHNAME READ-PATHNAME))))
|        (PATHNAME-EQUAL (TRANSLATE-LOGICAL-PATHNAME READ-PATHNAME)
|                        (TRANSLATE-LOGICAL-PATHNAME MADE-PATHNAME)
|          :CASE :LOCAL))




|    Logical-Pathname READ-PATHNAME = (READ-FROM-STRING
|                                        "#P\\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\\"")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "dir" "subdir")
|        Name      : "name"
|        Type      : "type"
|        Version   : NIL



|    Logical-Pathname PATTERN = (READ-FROM-STRING "#P\\"LOGICAL:\*\*;\*.\*\\"")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname MADE-PATHNAME = (MAKE-PATHNAME :HOST "LOGICAL"
|                                       :DEVICE :UNSPECIFIC
|                                       :DIRECTORY '(:ABSOLUTE "DIR" "SUBDIR")
|                                       :NAME "NAME"
|                                       :TYPE "TYPE"
|                                       :VERSION :NEWEST
|                                       :CASE :COMMON)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : NIL
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



Unix Pathname Construction Checks
#################################



Check UNIX/MAKE-PATHNAME/CASE-COMMON
====================================

Failed assertion: 

|    (DIRLIST= '(:ABSOLUTE "tmp") (PATHNAME-DIRECTORY PATH :CASE :LOCAL))




|    Pathname PATH = (MAKE-PATHNAME :NAME "NAME"
|                      :TYPE "TYPE"
|                      :CASE :COMMON
|                      :DEFAULTS #P"/tmp/")
|    is a PATHNAME: #P"/TMP/name.type"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP")
|        Name      : "name"
|        Type      : "type"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :UNSPECIFIC

MAKE-PATHNAME :CASE parameter does not apply on the :DEFAULTS parameter!
"19.2.2.1.2 Case in Pathname Components"


Checking Translations between logical pathnames
###############################################


CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/I0/IDENTITY
=================================================

Failed assertion: 

|    (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON)




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host (identity).
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/A1/TYPE
=============================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;\*.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "pyt"
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;NAME.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : NIL

translate-pathname should work within the same logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/A2/NAME
=============================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;EMAN.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;EMAN.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "eman"
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;EMAN.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.

Check TRANSLATE-PATHNAME/LOGICAL/SELF/A3/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:UVW;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;XYZ;ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:UVW;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/B1/TYPE
=============================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;\*.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "pyt"
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;NAME.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : NIL

translate-pathname should work within the same logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/B2/NAME
=============================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;NAME.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;NAME.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "name"
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;EMAN.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;EMAN.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "eman"
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;EMAN.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/B3/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:UVW;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:UVW;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/C1/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;SUBDIR;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;SUBDIR;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS "subdir")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;SUBDIR;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def" "subdir")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;SUBDIR;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;SUBDIR;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def" "subdir")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/C2/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:SUBDIR;\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:SUBDIR;\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "subdir" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:SUBDIR;ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "subdir" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:SUBDIR;ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:SUBDIR;ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "subdir" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/C3/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:UVW;\*\*;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;\*\*;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" :WILD-INFERIORS "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;ABC;DEF;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:UVW;ABC;DEF;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;ABC;DEF;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.

Check TRANSLATE-PATHNAME/LOGICAL/SELF/C4/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:UVW;\*;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;\*;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;ABC;XYZ;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "abc" "xyz" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:UVW;ABC;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;ABC;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "abc" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/C5/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:RST;\*;UVW;\*;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:RST;\*;UVW;\*;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "rst" :WILD "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:RST;ABC;UVW;DEF;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "rst" "abc" "uvw" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:RST;ABC;UVW;DEF;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:RST;ABC;UVW;DEF;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "rst" "abc" "uvw" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/J0/IDENTITY
====================================================

Failed assertion: 

|    (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON)




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/E1/TYPE
================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;\*.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;\*.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "pyt"
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;NAME.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/E2/NAME
================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;EMAN.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;EMAN.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "eman"
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;EMAN.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/E3/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:UVW;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:UVW;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/F1/TYPE
================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;\*.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;\*.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "pyt"
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;NAME.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/F2/NAME
================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;NAME.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;NAME.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "name"
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;EMAN.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;EMAN.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "eman"
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;EMAN.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/F3/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:UVW;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:UVW;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/G1/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;SUBDIR;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;SUBDIR;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS "subdir")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def" "subdir")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def" "subdir")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/G2/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:SUBDIR;\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:SUBDIR;\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "subdir" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:SUBDIR;ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "subdir" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:SUBDIR;ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:SUBDIR;ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "subdir" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/G3/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:UVW;\*\*;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;\*\*;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" :WILD-INFERIORS "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;ABC;DEF;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:UVW;ABC;DEF;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;ABC;DEF;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/G4/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:UVW;\*;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;\*;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;ABC;XYZ;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "abc" "xyz" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:UVW;ABC;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;ABC;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "abc" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/G5/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:RST;\*;UVW;\*;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:RST;\*;UVW;\*;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "rst" :WILD "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:RST;ABC;UVW;DEF;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "rst" "abc" "uvw" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:RST;ABC;UVW;DEF;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:RST;ABC;UVW;DEF;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "rst" "abc" "uvw" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/SELF/R1/NAME
=============================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;NAME.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;NAME.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "name"
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;EMAN.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;EMAN.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "eman"
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;EMAN.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical host to logical host.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/R2/TYPE
================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical pathname to logical pathname.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/R3/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;SUBDIR;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;SUBDIR;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS "subdir")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def" "subdir")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def" "subdir")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical pathname to logical pathname.

Check TRANSLATE-PATHNAME/LOGICAL/SELF/R4/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR "didn't find :wild-inferiors in source and from")



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/R5/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:UVW;XYZ.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw")
|        Name      : "xyz"
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw")
|        Name      : "xyz"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:UVW;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical pathname to logical pathname.
CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're same.

CHECK-PATHNAME:PATHNAME-EQUAL says they're different.


Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/R6/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;SUBDIR;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;SUBDIR;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS "subdir")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def" "subdir")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log2"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def" "subdir")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical pathname to logical pathname.

Checking Translations from logical pathname to physical pathname
################################################################



Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/1A
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/2A
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/xyz/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF" "xyz")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def" "XYZ")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/3A
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/ABC/DEF/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/uvw/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/4A
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/\*\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/\*\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD-INFERIORS "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" :WILD-INFERIORS "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/ABC/DEF/xyz/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "ABC" "DEF" "xyz")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "abc" "def" "XYZ")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/uvw/abc/def/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "ABC" "DEF" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/5A
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" :WILD "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/ABC/xyz/DEF/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "ABC" "xyz" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "abc" "XYZ" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/uvw/abc/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "ABC" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/6A
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/rst/\*/uvw/\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/rst/\*/uvw/\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "rst" :WILD "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "RST" :WILD "UVW" :WILD "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/rst/ABC/uvw/DEF/xyz/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "rst" "ABC" "uvw" "DEF" "xyz")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "RST" "abc" "UVW" "def" "XYZ")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/rst/abc/uvw/def/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/rst/abc/uvw/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "rst" "abc" "uvw" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "RST" "ABC" "UVW" "DEF" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/1B
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/2B
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;NAME.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;NAME.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "name"
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/3B
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "typ"
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/1C
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/xyz/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "xyz")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "XYZ")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/uvw/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/2C
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;NAME.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;NAME.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "name"
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/eman.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/eman.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : "eman"
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : "EMAN"
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/eman.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF")
|        Name      : "eman"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def")
|        Name      : "EMAN"
|        Type      : "typ"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/eman.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/eman.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "EMAN"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/3C
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "typ"
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.pyt"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.pyt"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "pyt"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "PYT"
|        Version   : :UNSPECIFIC



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/NAME.pyt"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "pyt"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def")
|        Name      : "name"
|        Type      : "PYT"
|        Version   : :UNSPECIFIC



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.pyt"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.pyt"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "PYT"
|        Version   : :UNSPECIFIC

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Checking Translations from physical pathname to logical pathname
################################################################



Check UNIX/TRANSLATE-PATHNAME/PHYSICAL/LOGICAL/1
================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED)
|         (OR (PATHNAME-EQUAL TRANSLATED EXPECTED1 :CASE :COMMON)
|             (PATHNAME-EQUAL TRANSLATED EXPECTED2 :CASE :COMMON)))




|    Pathname PATH = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC



|    Pathname FROM-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:abc;def;name.typ"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname EXPECTED1 = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED2 = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from physical pathname to logical pathname, 
translating customary case.

Check UNIX/TRANSLATE-PATHNAME/PHYSICAL/LOGICAL/2
================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED)
|         (OR (PATHNAME-EQUAL TRANSLATED EXPECTED1 :CASE :COMMON)
|             (PATHNAME-EQUAL TRANSLATED EXPECTED2 :CASE :COMMON)))




|    Pathname PATH = #P"/tmp/log3/abc/def/subdir/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/subdir/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def" "subdir")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :UNSPECIFIC



|    Pathname FROM-PAT = #P"/tmp/log3/\*\*/subdir/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/subdir/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS "subdir")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS "SUBDIR")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :UNSPECIFIC



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:abc;def;subdir;name.typ"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "ABC" "DEF" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname EXPECTED1 = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname EXPECTED2 = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : NIL
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from physical pathname to logical pathname, 
translating customary case.; Exiting
