.. comment .. comment -\*- mode:rst -\*-
.. comment

    Output of this script should be formated as a reStructured text,
    so that it can be rendered nicely and readably.



###################################################################################
check-pathnames of CLISP (2.49 (2010-07-07) (built 3499302370) (memory 3499302538))
###################################################################################



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

With CLISP (2.49 (2010-07-07) (built 3499302370) (memory 3499302538)) on Unix, 
the customary case for the file system of the host NIL of the pathname 
"/tmp/name.type" seems to be lower case.
Which was expected.





|    \*FEATURES\* = 
|    (:READLINE :REGEXP :SYSCALLS :I18N :LOOP :COMPILER :CLOS :MOP :CLISP :ANSI-CL
|     :COMMON-LISP :LISP=CL :INTERPRETER :SOCKETS :GENERIC-STREAMS :LOGICAL-PATHNAMES
|     :SCREEN :FFI :GETTEXT :UNICODE :BASE-CHAR=CHARACTER :WORD-SIZE=64 :PC386 :UNIX)






Pathname Accessor Checks with :CASE :COMMON
###########################################

We're considering the pathname built with:


|    (MAKE-PATHNAME :HOST "LOGICAL" :DEVICE :UNSPECIFIC :DIRECTORY
|     (:ABSOLUTE "DIR" "SUBDIR") :NAME "NAME" :TYPE "TYPE" :VERSION :NEWEST :CASE
|     :COMMON)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type.NEWEST"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "dir" "subdir")
|        Name      : "name"
|        Type      : "type"
|        Version   : :NEWEST



Check LOGICAL-PATHNAME-DIRECTORY
================================

Failed assertion: 

|    (DIRLIST= (PATHNAME-DIRECTORY PATH :CASE :COMMON) DIRECTORY)




|    LEFT  ARGUMENT  = (PATHNAME-DIRECTORY PATH :CASE :COMMON) =
|    (:ABSOLUTE "dir" "subdir")



|    RIGHT ARGUMENT  = DIRECTORY =
|    (:ABSOLUTE "DIR" "SUBDIR")



Check LOGICAL-PATHNAME-NAME
===========================

Failed assertion: 

|    (STRING= (PATHNAME-NAME PATH :CASE :COMMON) NAME)




|    LEFT  ARGUMENT  = (PATHNAME-NAME PATH :CASE :COMMON) =
|    "name"



|    RIGHT ARGUMENT  = NAME =
|    "NAME"



Check LOGICAL-PATHNAME-TYPE
===========================

Failed assertion: 

|    (STRING= (PATHNAME-TYPE PATH :CASE :COMMON) TYPE)




|    LEFT  ARGUMENT  = (PATHNAME-TYPE PATH :CASE :COMMON) =
|    "type"



|    RIGHT ARGUMENT  = TYPE =
|    "TYPE"



Check PRINTED-PATHNAME-CASE-SENSITIVE
=====================================

Failed assertion: 

|    ((LAMBDA (PRINTED EXPECTED-PRINTED)
|      (FIND-IF (LAMBDA (EXPECTED) (STRING= EXPECTED PRINTED)) EXPECTED-PRINTED))
|     (PRIN1-TO-STRING PATH) EXPECTED-PRINTED)




|    LEFT  ARGUMENT  = (PRIN1-TO-STRING PATH) =
|    "#P\\"LOGICAL:dir;subdir;name.type.NEWEST\\""



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



Pathname Accessor Checks with :CASE :LOCAL (customary case is lower)
####################################################################

We're considering the pathname built with:


|    (MAKE-PATHNAME :HOST "logical" :DEVICE :UNSPECIFIC :DIRECTORY
|     (:ABSOLUTE "dir" "subdir") :NAME "name" :TYPE "type" :VERSION :NEWEST :CASE
|     :LOCAL)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type.NEWEST"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "dir" "subdir")
|        Name      : "name"
|        Type      : "type"
|        Version   : :NEWEST



Check LOGICAL-PATHNAME-DIRECTORY
================================

Failed assertion: 

|    (DIRLIST= (PATHNAME-DIRECTORY PATH :CASE :COMMON) DIRECTORY)




|    LEFT  ARGUMENT  = (PATHNAME-DIRECTORY PATH :CASE :COMMON) =
|    (:ABSOLUTE "dir" "subdir")



|    RIGHT ARGUMENT  = DIRECTORY =
|    (:ABSOLUTE "DIR" "SUBDIR")



Check LOGICAL-PATHNAME-NAME
===========================

Failed assertion: 

|    (STRING= (PATHNAME-NAME PATH :CASE :COMMON) NAME)




|    LEFT  ARGUMENT  = (PATHNAME-NAME PATH :CASE :COMMON) =
|    "name"



|    RIGHT ARGUMENT  = NAME =
|    "NAME"



Check LOGICAL-PATHNAME-TYPE
===========================

Failed assertion: 

|    (STRING= (PATHNAME-TYPE PATH :CASE :COMMON) TYPE)




|    LEFT  ARGUMENT  = (PATHNAME-TYPE PATH :CASE :COMMON) =
|    "type"



|    RIGHT ARGUMENT  = TYPE =
|    "TYPE"



Check PRINTED-PATHNAME-CASE-SENSITIVE
=====================================

Failed assertion: 

|    ((LAMBDA (PRINTED EXPECTED-PRINTED)
|      (FIND-IF (LAMBDA (EXPECTED) (STRING= EXPECTED PRINTED)) EXPECTED-PRINTED))
|     (PRIN1-TO-STRING PATH) EXPECTED-PRINTED)




|    LEFT  ARGUMENT  = (PRIN1-TO-STRING PATH) =
|    "#P\\"LOGICAL:dir;subdir;name.type.NEWEST\\""



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



Logical Pathname Construction Checks
####################################



Check SHARP-P-VS-MAKE-PATHNAME
==============================

Failed assertion: 

|    (PATHNAME-EQUAL READ-PATHNAME MADE-PATHNAME :CASE :COMMON)




|    Logical-Pathname READ-PATHNAME = (READ-FROM-STRING "#P\\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\\"")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :NEWEST



|    Logical-Pathname MADE-PATHNAME = 
|    (MAKE-PATHNAME :HOST "LOGICAL" :DEVICE :UNSPECIFIC :DIRECTORY
|     '(:ABSOLUTE "DIR" "SUBDIR") :NAME "NAME" :TYPE "TYPE" :VERSION :NEWEST :CASE
|     :COMMON)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type.NEWEST"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "dir" "subdir")
|        Name      : "name"
|        Type      : "type"
|        Version   : :NEWEST



Logical Pathname Translation Checks
###################################



Unix Pathname Construction Checks
#################################



Checking Translations between logical pathnames
###############################################



Check TRANSLATE-PATHNAME/LOGICAL/SELF/A1/TYPE
=============================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;\*.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "PYT"
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces ((:DIRECTORY \\"ABC\\" \\"DEF\\") \\"NAME\\" \\"TYP\\"
|     NIL) do not fit into #P\\"LOG1:\*\*;\*.PYT\\"
|    ")



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;NAME.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "PYT"
|        Version   : NIL

translate-pathname should work within the same logical host.

Check TRANSLATE-PATHNAME/LOGICAL/SELF/A2/NAME
=============================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;EMAN.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;EMAN.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "EMAN"
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces ((:DIRECTORY \\"ABC\\" \\"DEF\\") \\"NAME\\" \\"TYP\\"
|     NIL) do not fit into #P\\"LOG1:\*\*;EMAN.\*\\"
|    ")



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;EMAN.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "EMAN"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work within the same logical host.

Check TRANSLATE-PATHNAME/LOGICAL/SELF/A3/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:UVW;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW" "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces ((:DIRECTORY \\"ABC\\" \\"DEF\\") \\"NAME\\" \\"TYP\\"
|     NIL) do not fit into #P\\"LOG1:UVW;XYZ;\*.\*\\"
|    ")



|    Logical-Pathname EXPECTED = #P"LOG1:UVW;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work within the same logical host.

Check TRANSLATE-PATHNAME/LOGICAL/SELF/C4/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:UVW;\*;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;\*;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW" :WILD "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces (\\"ABC\\" \\"DEF\\" \\"NAME\\" \\"TYP\\" NIL) do not fit into #P\\"LOG1:UVW;\*;XYZ;\*.\*\\"
|    ")



|    Logical-Pathname EXPECTED = #P"LOG1:UVW;ABC;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;ABC;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW" "ABC" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work within the same logical host.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/E1/TYPE
================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;\*.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;\*.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "PYT"
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces ((:DIRECTORY \\"ABC\\" \\"DEF\\") \\"NAME\\" \\"TYP\\"
|     NIL) do not fit into #P\\"LOG2:\*\*;\*.PYT\\"
|    ")



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;NAME.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "PYT"
|        Version   : NIL

translate-pathname should work from logical host to logical host.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/E2/NAME
================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;EMAN.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;EMAN.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "EMAN"
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces ((:DIRECTORY \\"ABC\\" \\"DEF\\") \\"NAME\\" \\"TYP\\"
|     NIL) do not fit into #P\\"LOG2:\*\*;EMAN.\*\\"
|    ")



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;EMAN.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "EMAN"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical host to logical host.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/E3/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:UVW;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW" "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces ((:DIRECTORY \\"ABC\\" \\"DEF\\") \\"NAME\\" \\"TYP\\"
|     NIL) do not fit into #P\\"LOG2:UVW;XYZ;\*.\*\\"
|    ")



|    Logical-Pathname EXPECTED = #P"LOG2:UVW;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical host to logical host.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/G4/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:UVW;\*;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;\*;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW" :WILD "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces (\\"ABC\\" \\"DEF\\" \\"NAME\\" \\"TYP\\" NIL) do not fit into #P\\"LOG2:UVW;\*;XYZ;\*.\*\\"
|    ")



|    Logical-Pathname EXPECTED = #P"LOG2:UVW;ABC;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;ABC;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW" "ABC" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical host to logical host.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/R2/TYPE
================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : NIL
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical pathname to logical pathname.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/R3/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;SUBDIR;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;SUBDIR;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS "SUBDIR")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;SUBDIR;NAME"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF" "SUBDIR")
|        Name      : "NAME"
|        Type      : NIL
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical pathname to logical pathname.

Check TRANSLATE-PATHNAME/LOGICAL/SELF/R4/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces (\\"NAME\\" \\"TYP\\" NIL) do not fit into #P\\"LOG1:\*\*;\*.\*\\"
|    ")



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work within the same logical host.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/R5/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:UVW;XYZ.\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ.\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW")
|        Name      : "XYZ"
|        Type      : :WILD
|        Version   : :WILD



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces (\\"NAME\\" \\"TYP\\" NIL) do not fit into #P\\"LOG2:UVW;XYZ.\*.\*\\"
|    ")



|    Logical-Pathname EXPECTED = #P"LOG2:UVW;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical pathname to logical pathname.

Check TRANSLATE-PATHNAME/LOGICAL/LOGICAL/R6/DIRECTORY
=====================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:\*\*;SUBDIR;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:\*\*;SUBDIR;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS "SUBDIR")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;SUBDIR;NAME"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF" "SUBDIR")
|        Name      : "NAME"
|        Type      : NIL
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:ABC;DEF;SUBDIR;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical pathname to logical pathname.

Checking Translations from logical pathname to physical pathname
################################################################



Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/5A
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" :WILD "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces (\\"abc\\" \\"def\\" \\"name\\" \\"typ\\" NIL) do not fit into #P\\"/tmp/log3/uvw/\*/xyz/\*.\*\\"
|    ")



|    Pathname EXPECTED = #P"/tmp/log3/uvw/abc/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "ABC" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/1B
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "TRANSLATE-PATHNAME: replacement pieces (\\"name\\" \\"typ\\" NIL) do not fit into #P\\"/tmp/log3/\*\*/\*.\*\\"
|    ")



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/2B
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;NAME.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;NAME.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "NAME"
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/abc/def/typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "typ"
|        Type      : NIL
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "TYP"
|        Type      : NIL
|        Version   : NIL



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/3B
=================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :LOCAL))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "TYP"
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/abc/def/name"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : NIL
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : NIL
|        Version   : NIL



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Checking Translations from physical pathname to logical pathname
################################################################

