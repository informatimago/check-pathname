.. comment .. comment -\*- mode:rst -\*-
.. comment

    Output of this script should be formated as a reStructured text,
    so that it can be rendered nicely and readably.



#######################################
check-pathnames of SBCL (1.0.19-gentoo)
#######################################



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

With SBCL (1.0.19-gentoo) on Linux, the customary case for the file system of 
the host #<SB-IMPL::UNIX-HOST {1000224C01}> of the pathname "/tmp/name.type" 
seems to be lower case.
Which was expected.





|    \*FEATURES\* = (:SB-THREAD :ANSI-CL :COMMON-LISP :SBCL :SB-DOC :SB-PACKAGE-LOCKS
|                  :SB-UNICODE :SB-EVAL :SB-SOURCE-LOCATIONS :IEEE-FLOATING-POINT
|                  :X86-64 :UNIX :ELF :LINUX :GENCGC
|                  :STACK-GROWS-DOWNWARD-NOT-UPWARD :C-STACK-IS-CONTROL-STACK
|                  :LINKAGE-TABLE :COMPARE-AND-SWAP-VOPS
|                  :UNWIND-TO-FRAME-AND-CALL-VOP :RAW-INSTANCE-INIT-VOPS
|                  :STACK-ALLOCATABLE-CLOSURES :ALIEN-CALLBACKS :CYCLE-COUNTER
|                  :OS-PROVIDES-DLOPEN :OS-PROVIDES-PUTWC :OS-PROVIDES-SUSECONDS-T)






Pathname Accessor Checks with :CASE :COMMON
###########################################

We're considering the pathname built with:


|    (MAKE-PATHNAME :HOST "LOGICAL" :DEVICE :UNSPECIFIC :DIRECTORY
|                   (:ABSOLUTE "DIR" "SUBDIR") :NAME "NAME" :TYPE "TYPE" :VERSION
|                   :NEWEST :CASE :COMMON)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOGICAL">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :NEWEST



Check LOGICAL-HOST-MUST-BE-A-STRING
===================================

Failed assertion: 

|    (TYPEP (PATHNAME-HOST PATH :CASE :COMMON) 'STRING)




|    LEFT  ARGUMENT  = (PATHNAME-HOST PATH :CASE :COMMON) =
|    #<SB-KERNEL:LOGICAL-HOST "LOGICAL">



|    RIGHT ARGUMENT  = 'STRING =
|    STRING


Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

pathname-host pathname &key case => host

host---a valid pathname host. 

valid logical pathname host n. a string that has been defined as the
name of a logical host.  See the function
load-logical-pathname-translations.



Pathname Accessor Checks with :CASE :LOCAL (customary case is lower)
####################################################################

We're considering the pathname built with:


|    (MAKE-PATHNAME :HOST "logical" :DEVICE :UNSPECIFIC :DIRECTORY
|                   (:ABSOLUTE "dir" "subdir") :NAME "name" :TYPE "type" :VERSION
|                   :NEWEST :CASE :LOCAL)
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOGICAL">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :NEWEST



Check LOGICAL-HOST-MUST-BE-A-STRING
===================================

Failed assertion: 

|    (TYPEP (PATHNAME-HOST PATH :CASE :COMMON) 'STRING)




|    LEFT  ARGUMENT  = (PATHNAME-HOST PATH :CASE :COMMON) =
|    #<SB-KERNEL:LOGICAL-HOST "LOGICAL">



|    RIGHT ARGUMENT  = 'STRING =
|    STRING


Function PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION

pathname-host pathname &key case => host

host---a valid pathname host. 

valid logical pathname host n. a string that has been defined as the
name of a logical host.  See the function
load-logical-pathname-translations.



Logical Pathname Construction Checks
####################################



Logical Pathname Translation Checks
###################################



Check SHARP-P-VS-TRANSLATE-LOGICAL-PATHNAME
===========================================

Failed assertion: 

|    (IGNORE-ERRORS (TRANSLATE-LOGICAL-PATHNAME READ-PATHNAME))




|    Logical-Pathname READ-PATHNAME = (READ-FROM-STRING
|                                      "#P\\"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST\\"")
|    is a LOGICAL-PATHNAME: #P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOGICAL">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "DIR" "SUBDIR")
|        Name      : "NAME"
|        Type      : "TYPE"
|        Version   : :NEWEST

Pathname components from SOURCE and FROM args to TRANSLATE-PATHNAME did not 
match: :NEWEST NIL


|    (LOGICAL-PATHNAME-TRANSLATIONS "LOGICAL") = ((#P"LOGICAL:\*\*;\*.\*"
|                                                  #P"/tmp/\*\*/\*.\*")
|                                                 (#P"LOGICAL:\*\*;\*" #P"/tmp/\*\*/\*"))


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


Therefore a wildcard of #P"LOGICAL:\*\*;\*.\*" should be equivalent to
#P"LOGICAL:\*\*;\*.\*.\*" and should match
#P"LOGICAL:DIR;SUBDIR;NAME.TYPE.NEWEST".

and:

    Function TRANSLATE-PATHNAME

    The resulting pathname is to-wildcard with each wildcard or missing
    field replaced by a portion of source.

Therefore whether you consider nil or :wild in the to-wildcard, the
:newer in the from-wildcard should match and replace it!


Unix Pathname Construction Checks
#################################



Checking Translations between logical pathnames
###############################################



Check TRANSLATE-PATHNAME/LOGICAL/SELF/R4/DIRECTORY
==================================================

Failed assertion: 

|    (AND (PATHNAMEP TRANSLATED) (PATHNAME-EQUAL TRANSLATED EXPECTED :CASE :COMMON))




|    Logical-Pathname PATH = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG1:NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE)
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG1:ABC;DEF;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Logical-Pathname TO-PAT = #P"LOG2:UVW;XYZ.\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ.\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG2">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW")
|        Name      : "XYZ"
|        Type      : :WILD
|        Version   : :WILD



|    Logical-Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG2">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "UVW")
|        Name      : "XYZ"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname EXPECTED = #P"LOG2:UVW;XYZ;NAME.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG2:UVW;XYZ;NAME.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG2">
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/abc/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/uvw/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/\*\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/\*\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD-INFERIORS "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" :WILD-INFERIORS "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "ABC" "DEF" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/uvw/abc/def/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" :WILD "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "ABC" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/uvw/abc/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/abc/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "abc" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*;\*;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*;\*;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD :WILD)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/rst/\*/uvw/\*/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/rst/\*/uvw/\*/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "rst" :WILD "uvw" :WILD "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "RST" :WILD "UVW" :WILD "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/rst/abc/uvw/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "rst" "abc" "uvw" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "RST" "ABC" "UVW" "DEF" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/rst/abc/uvw/def/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/rst/abc/uvw/def/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "rst" "abc" "uvw" "def" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;NAME.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;NAME.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "NAME"
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "TYP"
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:ABC;DEF;\*.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:ABC;DEF;\*.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/uvw/xyz/\*.\*"
|    is a PATHNAME: #P"/tmp/log3/uvw/xyz/\*.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "xyz")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "XYZ")
|        Name      : :WILD
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/uvw/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "UVW" "XYZ")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/uvw/xyz/name.typ"
|    is a PATHNAME: #P"/tmp/log3/uvw/xyz/name.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "uvw" "xyz")
|        Name      : "name"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;NAME.\*"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;NAME.\*"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : "NAME"
|        Type      : :WILD
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/eman.\*"
|    is a PATHNAME: #P"/tmp/log3/\*\*/eman.\*"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : "eman"
|        Type      : :WILD
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : "EMAN"
|        Type      : :WILD
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/abc/def/eman.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "EMAN"
|        Type      : "TYP"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/eman.typ"
|    is a PATHNAME: #P"/tmp/log3/abc/def/eman.typ"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "eman"
|        Type      : "typ"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
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
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "TYP"
|        Version   : NIL



|    Logical-Pathname FROM-PAT = #P"LOG1:\*\*;\*.TYP"
|    is a LOGICAL-PATHNAME: #P"LOG1:\*\*;\*.TYP"
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-KERNEL:LOGICAL-HOST "LOG1">
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "TYP"
|        Version   : NIL



|    Pathname TO-PAT = #P"/tmp/log3/\*\*/\*.pyt"
|    is a PATHNAME: #P"/tmp/log3/\*\*/\*.pyt"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "pyt"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" :WILD-INFERIORS)
|        Name      : :WILD
|        Type      : "PYT"
|        Version   : :NEWEST



|    Pathname TRANSLATED = (CHECKED-TRANSLATE-PATHNAME PATH FROM-PAT TO-PAT)
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.pyt"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : :UNSPECIFIC
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "PYT"
|        Version   : :NEWEST



|    Pathname EXPECTED = #P"/tmp/log3/abc/def/name.pyt"
|    is a PATHNAME: #P"/tmp/log3/abc/def/name.pyt"
|    The fields of this pathname with :case :local (default) are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "tmp" "log3" "abc" "def")
|        Name      : "name"
|        Type      : "pyt"
|        Version   : :NEWEST
|    The fields of this pathname with :case :common are:
|        Host      : #<SB-IMPL::UNIX-HOST {1000224C01}>
|        Device    : NIL
|        Directory : (:ABSOLUTE "TMP" "LOG3" "ABC" "DEF")
|        Name      : "NAME"
|        Type      : "PYT"
|        Version   : :NEWEST

translate-pathname should work from logical pathname to physical pathname, 
translating to customary case.

Checking Translations from physical pathname to logical pathname
################################################################

