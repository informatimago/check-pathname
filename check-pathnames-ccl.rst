.. comment .. comment -\*- mode:rst -\*-
.. comment

    Output of this script should be formated as a reStructured text,
    so that it can be rendered nicely and readably.



##############################################################################
check-pathnames of Clozure Common Lisp (Version 1.6-RC1-r14432M  (LinuxX8664))
##############################################################################



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

With Clozure Common Lisp (Version 1.6-RC1-r14432M (LinuxX8664)) on Linux, the 
customary case for the file system of the host :UNSPECIFIC of the pathname 
"/tmp/name.type" seems to be lower case.
Which was expected.





|    \*FEATURES\* = (:PRIMARY-CLASSES :COMMON-LISP :OPENMCL :CCL :CCL-1.2 :CCL-1.3
|                  :CCL-1.4 :CCL-1.5 :CCL-1.6 :CLOZURE :CLOZURE-COMMON-LISP :ANSI-CL
|                  :UNIX :OPENMCL-UNICODE-STRINGS :OPENMCL-NATIVE-THREADS
|                  :OPENMCL-PARTIAL-MOP :MCL-COMMON-MOP-SUBSET :OPENMCL-MOP-2
|                  :OPENMCL-PRIVATE-HASH-TABLES :X86-64 :X86_64 :X86-TARGET
|                  :X86-HOST :X8664-TARGET :X8664-HOST :LINUX-HOST :LINUX-TARGET
|                  :LINUXX86-TARGET :LINUXX8664-TARGET :LINUXX8664-HOST
|                  :64-BIT-TARGET :64-BIT-HOST :LINUX :LITTLE-ENDIAN-TARGET
|                  :LITTLE-ENDIAN-HOST)






Pathname Accessor Checks with :CASE :COMMON
###########################################

We're considering the pathname built with:


|    (MAKE-PATHNAME
|      :HOST
|      "LOGICAL"
|      :DEVICE
|      :UNSPECIFIC
|      :DIRECTORY
|      (:ABSOLUTE "DIR" "SUBDIR")
|      :NAME
|      "NAME"
|      :TYPE
|      "TYPE"
|      :VERSION
|      :NEWEST
|      :CASE
|      :COMMON)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type.newest"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :NEWEST



Check PRINTED-PATHNAME-CASE-SENSITIVE
=====================================

Failed assertion: 

|    ((LAMBDA (PRINTED EXPECTED-PRINTED)
|       (FIND-IF (LAMBDA (EXPECTED) (STRING= EXPECTED PRINTED)) EXPECTED-PRINTED))
|     (PRIN1-TO-STRING PATH) EXPECTED-PRINTED)




|    LEFT  ARGUMENT  = (PRIN1-TO-STRING PATH) =
|    "#P\\"LOGICAL:dir;subdir;name.type.newest\\""



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


|    (MAKE-PATHNAME
|      :HOST
|      "logical"
|      :DEVICE
|      :UNSPECIFIC
|      :DIRECTORY
|      (:ABSOLUTE "dir" "subdir")
|      :NAME
|      "name"
|      :TYPE
|      "type"
|      :VERSION
|      :NEWEST
|      :CASE
|      :LOCAL)
|    is a LOGICAL-PATHNAME: #P"logical:dir;subdir;name.type.newest"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :NEWEST



Check PRINTED-PATHNAME-CASE-SENSITIVE
=====================================

Failed assertion: 

|    ((LAMBDA (PRINTED EXPECTED-PRINTED)
|       (FIND-IF (LAMBDA (EXPECTED) (STRING= EXPECTED PRINTED)) EXPECTED-PRINTED))
|     (PRIN1-TO-STRING PATH) EXPECTED-PRINTED)




|    LEFT  ARGUMENT  = (PRIN1-TO-STRING PATH) =
|    "#P\\"logical:dir;subdir;name.type.newest\\""



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


CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're different.

CHECK-PATHNAME:PATHNAME-EQUAL says they're same.

CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're different.

CHECK-PATHNAME:PATHNAME-EQUAL says they're same.

CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're different.

CHECK-PATHNAME:PATHNAME-EQUAL says they're same.


Logical Pathname Translation Checks
###################################



Check TRANSLATE-LOGICAL-PATHNAMES/SHARP-P-VS-MAKE-PATHNAME
==========================================================

Failed assertion: 

|    (OR (NOT (AND (PATHNAME-MATCH-P READ-PATHNAME PATTERN)
|                  (IGNORE-ERRORS (TRANSLATE-LOGICAL-PATHNAME READ-PATHNAME))))
|        (PATHNAME-EQUAL
|          (TRANSLATE-LOGICAL-PATHNAME READ-PATHNAME)
|          (TRANSLATE-LOGICAL-PATHNAME MADE-PATHNAME)
|          :CASE
|          :LOCAL))




|    Logical-Pathname READ-PATHNAME = (READ-FROM-STRING
|                                       "#P\\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\\"")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.newest"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :NEWEST



|    Logical-Pathname PATTERN = (READ-FROM-STRING "#P\\"LOGICAL:\*\*;\*.\*\\"")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname MADE-PATHNAME = (MAKE-PATHNAME
|                                       :HOST
|                                       "LOGICAL"
|                                       :DEVICE
|                                       :UNSPECIFIC
|                                       :DIRECTORY
|                                       '(:ABSOLUTE "DIR" "SUBDIR")
|                                       :NAME
|                                       "NAME"
|                                       :TYPE
|                                       "TYPE"
|                                       :VERSION
|                                       :NEWEST
|                                       :CASE
|                                       :COMMON)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:dir;subdir;name.type.newest"
|    The fields of this pathname with :case :common are:
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :NEWEST



Unix Pathname Construction Checks
#################################



Checking Translations between logical pathnames
###############################################



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
|        Version   : "\*"



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG2"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW")
|        Name      : "XYZ"
|        Type      : "TYP"
|        Version   : NIL



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

Checking Translations from logical pathname to physical pathname
################################################################



Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/1A
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



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/2A
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



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/xyz/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF" "xyz")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def" "XYZ")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/3A
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



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/ABC/DEF/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/uvw/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/4A
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



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "LOG1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/\*\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/\*\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD-INFERIORS "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" :WILD-INFERIORS "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/ABC/DEF/xyz/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "ABC" "DEF" "xyz")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "abc" "def" "XYZ")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/uvw/abc/def/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "ABC" "DEF" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

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
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" :WILD "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/ABC/xyz/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "ABC" "xyz")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "abc" "XYZ")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/uvw/abc/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "ABC" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/6A
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



|    Pathname TO-PAT = #P"/tmp/log3/rst/\*/uvw/\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/rst/\*/uvw/\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "rst" :WILD "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "RST" :WILD "UVW" :WILD "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/rst/ABC/uvw/DEF/xyz/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "rst" "ABC" "uvw" "DEF" "xyz")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "RST" "abc" "UVW" "def" "XYZ")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/rst/abc/uvw/def/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/rst/abc/uvw/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "rst" "abc" "uvw" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "RST" "ABC" "UVW" "DEF" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST

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
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST

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
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST

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
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/1C
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



|    Pathname TO-PAT = #P"/tmp/log3/uvw/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/xyz/NAME.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "xyz")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "XYZ")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/uvw/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/2C
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



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/eman.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/eman.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : "eman"
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : "EMAN"
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/eman.TYP"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF")
|        Name      : "eman"
|        Type      : "TYP"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def")
|        Name      : "EMAN"
|        Type      : "typ"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/eman.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/eman.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "EMAN"
|        Type      : "TYP"
|        Version   : :NEWEST

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Check UNIX/TRANSLATE-PATHNAME/LOGICAL/PHYSICAL/3C
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



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.pyt"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.pyt"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "pyt"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "PYT"
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/ABC/DEF/NAME.pyt"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "pyt"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "abc" "def")
|        Name      : "name"
|        Type      : "PYT"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.pyt"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.pyt"
|    The fields of this pathname with :case :local (default) are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : :UNSPECIFIC
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "PYT"
|        Version   : :NEWEST

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Checking Translations from physical pathname to logical pathname
################################################################


CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're different.

CHECK-PATHNAME:PATHNAME-EQUAL says they're same.

CL:EQUAL and CHECK-PATHNAME:PATHNAME-EQUAL don't agree on equality of these pathnames:

CL:EQUAL says they're different.

CHECK-PATHNAME:PATHNAME-EQUAL says they're same.
