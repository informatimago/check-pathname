;;; Loading "/home/pjb/src/lisp/check-pathnames/check-pathnames.lisp"
.. comment .. comment -\*- mode:rst -\*-
.. comment

    Output of this script should be formated as a reStructured text,
    so that it can be rendered nicely and readably.



###############################
check-pathnames of ECL (10.7.1)
###############################



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

With ECL (10.7.1) on Linux, the customary case for the file system of the host 
NIL of the pathname "/tmp/name.type" seems to be lower case.
Which was expected.





|    \*FEATURES\* = (:LINUX :FORMATTER :LONG-LONG :UINT64-T :UINT32-T :UINT16-T
|                  :RELATIVE-PACKAGE-NAMES :LONG-FLOAT :UNICODE :CLOS-STREAMS
|                  :CMU-FORMAT :UNIX :ECL-PDE :DLOPEN :CLOS :BOEHM-GC :ANSI-CL
|                  :COMMON-LISP :IEEE-FLOATING-POINT :PREFIXED-API :FFI :X86_64
|                  :COMMON :ECL)






Pathname Accessor Checks with :CASE :COMMON
###########################################

We're considering the pathname built with:


|    (MAKE-PATHNAME :HOST
|                   "LOGICAL"
|                   :DEVICE
|                   :UNSPECIFIC
|                   :DIRECTORY
|                   (:ABSOLUTE "DIR" "SUBDIR")
|                   :NAME
|                   "NAME"
|                   :TYPE
|                   "TYPE"
|                   :VERSION
|                   :NEWEST
|                   :CASE
|                   :COMMON)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "dir" "subdir")
|        Name      : "name"
|        Type      : "type"
|        Version   : :NEWEST



Check LOGICAL-HOST-MUST-BE-UPPER-CASE
=====================================

Failed assertion: 

|    (STRING= (PATHNAME-HOST PATH :CASE :COMMON) HOST)




|    LEFT  ARGUMENT  = (PATHNAME-HOST PATH :CASE :COMMON) =
|    "logical"



|    RIGHT ARGUMENT  = HOST =
|    "LOGICAL"

19.2.2.1.2  makes no exception for pathname-host of logical pathnames.


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




Pathname Accessor Checks with :CASE :LOCAL (customary case is lower)
####################################################################

We're considering the pathname built with:


|    (MAKE-PATHNAME :HOST
|                   "logical"
|                   :DEVICE
|                   :UNSPECIFIC
|                   :DIRECTORY
|                   (:ABSOLUTE "dir" "subdir")
|                   :NAME
|                   "name"
|                   :TYPE
|                   "type"
|                   :VERSION
|                   :NEWEST
|                   :CASE
|                   :LOCAL)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"
|    The fields of this pathname with :case :common are:
|        Host      : "logical"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "dir" "subdir")
|        Name      : "name"
|        Type      : "type"
|        Version   : :NEWEST



Check LOGICAL-HOST-MUST-BE-UPPER-CASE
=====================================

Failed assertion: 

|    (STRING= (PATHNAME-HOST PATH :CASE :COMMON) HOST)




|    LEFT  ARGUMENT  = (PATHNAME-HOST PATH :CASE :COMMON) =
|    "logical"



|    RIGHT ARGUMENT  = HOST =
|    "LOGICAL"

19.2.2.1.2  makes no exception for pathname-host of logical pathnames.


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




Logical Pathname Construction Checks
####################################



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
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;\*.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "pyt"
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "Number of wildcards in #P\\"LOG1:\*\*;\*.\*\\" do not match  #P\\"LOG1:\*\*;\*.PYT\\"")



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;NAME.PYT"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.PYT"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : NIL

translate-pathname should work within the same logical host.

Check TRANSLATE-PATHNAME/LOGICAL/SELF/A2/NAME
=============================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;EMAN.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;EMAN.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "eman"
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "Number of wildcards in #P\\"LOG1:\*\*;\*.\*\\" do not match  #P\\"LOG1:\*\*;EMAN.\*\\"")



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;EMAN.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;EMAN.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
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
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:UVW;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "uvw" "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "Number of wildcards in #P\\"LOG1:\*\*;\*.\*\\" do not match  #P\\"LOG1:UVW;XYZ;\*.\*\\"")



|    Logical-Pathname EXPECTED = #P"LOG1:UVW;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "uvw" "xyz")
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
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:UVW;\*;XYZ;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;\*;XYZ;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR
|     "Number of wildcards in #P\\"LOG1:\*;\*;\*.\*\\" do not match  #P\\"LOG1:UVW;\*;XYZ;\*.\*\\"")



|    Logical-Pathname EXPECTED = #P"LOG1:UVW;ABC;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:UVW;ABC;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : "log1"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "uvw" "abc" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work within the same logical host.