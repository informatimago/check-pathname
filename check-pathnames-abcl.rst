.. comment .. comment -\*- mode:rst -\*-
.. comment

    Output of this script should be formated as a reStructured text,
    so that it can be rendered nicely and readably.



######################################################
check-pathnames of Armed Bear Common Lisp (0.24.0-dev)
######################################################



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

With Armed Bear Common Lisp (0.24.0-dev) on Linux, the customary case for the 
file system of the host NIL of the pathname "/tmp/name.type" seems to be lower 
case.
Which was expected.





|    \*FEATURES\* = (:X86-64 :JAVA-1.6 :ARMEDBEAR :ABCL :COMMON-LISP :ANSI-CL :UNIX
|                  :LINUX :CDR6)






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
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :NEWEST




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
|        Host      : "LOGICAL"
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :NEWEST




Logical Pathname Construction Checks
####################################



Logical Pathname Translation Checks
###################################



Unix Pathname Construction Checks
#################################



Check UNIX/MERGE-PATHNAMES/MAKE-PATHNAME/CASE-COMMON/DIRECTORY
==============================================================

Failed assertion: 

|    (DIRLIST= '(:ABSOLUTE "tmp" "dir" "subdir")
|              (PATHNAME-DIRECTORY PATH :CASE :LOCAL))




|    Pathname PATH = (MERGE-PATHNAMES (MAKE-PATHNAME :DIRECTORY
|                                                    '(:RELATIVE "DIR" "SUBDIR")
|                                                    :NAME
|                                                    "NAME"
|                                                    :TYPE
|                                                    "TYPE"
|                                                    :CASE
|                                                    :COMMON
|                                                    :DEFAULTS
|                                                    #P"/tmp/")
|                                     #P"/tmp/"
|                                     NIL)
|    is a PATHNAME: #P"/tmp/DIR/SUBDIR/NAME.TYPE"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



Check UNIX/MERGE-PATHNAMES/MAKE-PATHNAME/CASE-COMMON/NAME
=========================================================

Failed assertion: 

|    (EQUAL "name" (PATHNAME-NAME PATH :CASE :LOCAL))




|    Pathname PATH = (MERGE-PATHNAMES (MAKE-PATHNAME :DIRECTORY
|                                                    '(:RELATIVE "DIR" "SUBDIR")
|                                                    :NAME
|                                                    "NAME"
|                                                    :TYPE
|                                                    "TYPE"
|                                                    :CASE
|                                                    :COMMON
|                                                    :DEFAULTS
|                                                    #P"/tmp/")
|                                     #P"/tmp/"
|                                     NIL)
|    is a PATHNAME: #P"/tmp/DIR/SUBDIR/NAME.TYPE"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



Check UNIX/MERGE-PATHNAMES/MAKE-PATHNAME/CASE-COMMON/TYPE
=========================================================

Failed assertion: 

|    (EQUAL "type" (PATHNAME-TYPE PATH :CASE :LOCAL))




|    Pathname PATH = (MERGE-PATHNAMES (MAKE-PATHNAME :DIRECTORY
|                                                    '(:RELATIVE "DIR" "SUBDIR")
|                                                    :NAME
|                                                    "NAME"
|                                                    :TYPE
|                                                    "TYPE"
|                                                    :CASE
|                                                    :COMMON
|                                                    :DEFAULTS
|                                                    #P"/tmp/")
|                                     #P"/tmp/"
|                                     NIL)
|    is a PATHNAME: #P"/tmp/DIR/SUBDIR/NAME.TYPE"
|    The fields of this pathname with :case :local (default) are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL
|    The fields of this pathname with :case :common are:
|        Host      : NIL
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : NIL



Checking Translations between logical pathnames
###############################################



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
|    (:ERROR "Unsupported case in TRANSLATE-DIRECTORY-COMPONENTS.")



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
|    (:ERROR "Unsupported case in TRANSLATE-DIRECTORY-COMPONENTS.")



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
|    (:ERROR "Unsupported case in TRANSLATE-DIRECTORY-COMPONENTS.")



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
|    (:ERROR "Unsupported case in TRANSLATE-DIRECTORY-COMPONENTS.")



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
|    (:ERROR "Unsupported case in TRANSLATE-DIRECTORY-COMPONENTS.")



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
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR "Unsupported case in TRANSLATE-DIRECTORY-COMPONENTS.")



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
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "xyz")
|        Name      : "name"
|        Type      : "typ"
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
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT) =
|    (:ERROR "Unsupported case in TRANSLATE-DIRECTORY-COMPONENTS.")



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
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : NIL

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Checking Translations from physical pathname to logical pathname
################################################################

