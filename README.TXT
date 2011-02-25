================================
 vcsh README

 (C) Copyright 2001-2011 East Coast Toolworks Inc.
 (C) Portions Copyright 1988-1994 Paradigm Associates Inc.

 schaeffer.michael.a@gmail.com
================================



Build instructions
================

1) Ensure the settings in build-settings are accurate for your target
   platform. If there is no build-settings file, create one by copying
   typical-build-settings and making the appropriate edits.

2) Run 'make'. This will recursively build the modules in the project
   in the order they are listed in the module summary below.

3) The final scheme interpreter will be located in scheme-core/vcsh.

4) The 'make tested' target will run a series of unit tests.


Using vcsh
================

Launching vcsh will display the herald message and immediately drop into a
REPL (Read-Eval-Print-Loop):

     ; Welcome to VCSH
     ;    VM Build ID     : Feb 24 2011-SCAN 0.50 (:debug)
     ;    Scheme Build ID : Feb 24 2011 16:34:03 - Scheme 0.50
     ;
     ; (C) Copyright 2001-2009 East Coast Toolworks Inc.
     ; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
     
     user> 

The REPL may be quit by entering ':X'.

     user> :X
     ;;;; end run, rc=0

Scheme expressions may be evaluated by entering them at the prompt:

     user> (+ 2 2)
     
     ; time = 1.10912 ms (0.00000 gc), 12061 cons work
     ; ##0 = 4

When the expression has been evaluated, the REPL will print a line
that describes the duration of the evaluation, the amount of time
spent garbage collecting during the evaluation, and the number of
memory cells that were allocated during the evaluation. Following this
line, the REPL will print out a line for each value returned by the
evaluation (zero or more). These lines take the form '##x = <value>'.
The '##x' is a shortcut expression that can be used to refer to that
value later during the session:

     user> ##0
     
     ; time = 0.76699 ms (0.00000 gc), 10988 cons work
     ; ##0 = 4

The REPL only assigns new shortcut numbers when is sees an object that
is not 'eq?' to an object it's previously seen. If an object is
returned more than once in the REPL, it will be given the same
shortcut number each time it is printed. This can be useful to make
quick identity checks:

     user> (define a '(1 2 3))
     
     ; time = 1.28603 ms (0.00000 gc), 14294 cons work
     ; ##1 = (1 2 3)
     user> (set-car! a 10)
     
     ; time = 1.04499 ms (0.00000 gc), 12102 cons work
     ; ##2 = 10
     user> a
     
     ; time = 0.78011 ms (0.00000 gc), 8321 cons work
     ; ##1 = (10 2 3)   <======== Same shortcut number as above

In addition to absolute shortcut numbers, the REPL also allows for
access to the last three returned values through relative shortcut
numbers. ##-1, ##-2, and ##-3 will return the most recent three
values. 

In addition to standard Scheme expressions, the REPL also provides
abbreviated forms for common interactive tasks. The exit command
(':X') listed above is an example of one of these abbreviated
forms. Abbreviated forms are composed of a keyword symbol followed by
zero or more arguments. A list of available abbreviated forms can be
found be entering an invalid abbreviation: 

     user> :h
     ; Info: Unknown REPL abbreviation: :h
     
     Current REPL Abbreviations:
     ------------------------------------------------
       :a      apropos'
       :A      apropos-any-package'
       :crh    clear-repl-history!
       :dis    disassemble
       :dp     display-packages
       :i      inspect
       :ip     in-package!'
       :ip1    scheme::in-package-for-one-form!'
       :l      load'
       :mx     macroexpand'
       :mx1    macroexpand-1'
       :r      require-package!'
       :rau    scheme::repl-auto-unwatch
       :raua   scheme::repl-auto-unwatch-all
       :raw    scheme::repl-auto-watch'
       :rsg    referred-symbol-grep'
       :std    show-type-delta
       :sts    show-type-stats
       :t      trace
       :tnr    trace/no-returns
       :top    toplevel
       :use    use-package!'
       :ut     untrace
       :uta    untrace-all
       :x      scheme::exit-repl
       :X      exit

Of notable interest is ':a', which searches currently defined functions for
keywords, and prints a list of all that match, along with their documentation
string.

     user> :a apropos
     --------------------------------
     ** apropos - Function lambda: search-for
       Prints a list describing each symbol public to the current package that
       is apropos of all of the search terms in <search-for>.
     
     ** apropos-any-package - Function lambda: search-for
       Prints a list describing each symbol visible in any package that is
       apropos of all of the search terms in <search-for>.
     
     ; time = 43.5350 ms (23.3381 gc), 259631 cons work

Controlling vcsh heap sizes
================

The heap size used by vcsh can be adjusted with two command line
arguments:

  -Xheap-segment-size=<n>(K|M|G)

  -Xmax-heap-segments=<b>

'Xheap-segment-size' controls the size of a heap segment. The VM will
start out with one heap segment and can enlarge the number of heaps up
to the number specfied in 'Xmax-heap-segments'. Here's how these
options could be used to guarantee a heap that doesn't grow beyond 4MB.

     ./vcsh -Xheap-segment-size=4M -Xmax-heap-segments=1

The current heap enlargement strategy will only allocate new heap
segments when all existing segments are full after a GC cycle. In
cases where a heap is mostly full, this can negatively impact
performance by requiring too many GC cycles for the progress being
made in the computation. This can be addressed by manually enlarging
the heap from scheme code. The 'enlarge-heap' function takes a target
number of heap segments and enlarges the heap to that size.

     user> (enlarge-heap 20)
     
     ; time = 171.349 ms (0.00000 gc), 10985 cons work
     ; ##0 = 20


To monitor GC behavior, the 'show-gc' debug flag can be set to cause
the GC to print messages describing what it's doing.

     ./vcsh -Xdebug-flags=show-gc

Running the benchmark suite
================

vcsh has a collection of benchmarks that measure various aspects of
its performance. These have been maintained for a while, and are
written in a way that maximizes their repeatability. The way to run
them is currently this:

1) Change into the benchmarks directory

     ~/ectworks/vcsh$ cd scheme-core/benchmarks/

2) Launch the interpreter, loading both the csv library and the
   benchmark suite.

     ~/ectworks/vcsh/scheme-core/benchmarks$ ../vcsh ../csv.scm bench.scm

3) Switch to the bench package:

     user> :ip bench
     
     ; time = 1.28388 ms (0.00000 gc), 11149 cons work
     ; ##0 = #<package bench>
     bench> 

4) Run the benchmarks by evaluating '(bench)'. The benchmark library
   will display status messages as progress is made.

     bench> (bench)
     
     [1/10] exec-loop-repeat: 1.2.4.8.
     [2/10] fast-queue: 1.2.4.
     ...

5) When the run is complete, results will be displayed in graphical
   form:

     Benchmark results (shorter bar is better, compared to (iivx.local :linux :debug)):
     Benchmark time mode = :net
     exec-loop-repeat         [-------------------+                    ]  1.015 (35.89 ms.)
     fast-queue               [-------------------+                    ]  0.982 (89.14 ms.)
     ...

6) Additional comparisons can be run with the ':cbr' REPL
   shortcut. The latest run can be recorded in the list of 'official'
   benchmark results with the ':pbr' shortcut.

Module summary
================

util/ - This directory contains base header files for standard things
like types, asserts. etc. There are a couple executable targets built
in this directory.

   to-c-source - This reads a binary file and emits C source code that
   will statically initialize a variable to the contents of that file. Later
   phases of the build process use this program to take scheme image files
   and link them into a single executable.

   show-retval - This executes a program and shows the return value.

vm/ - This is the virtual machine. It runs a scheme image, and most of
the interesting low-level code is here. (Including the garbage collector,
evaluator, and I/O code.)

scheme-core/ - This is the scheme source to a scheme image. Starting at
scheme.scm, this module defines all of the components of the environment
that are written in scheme. This includes the reader, printer, compiler,
and most standard functions.

scc0/ - Because vcsh compiles its own scheme image, a binary copy of an
older image needs to be retained to do this compile. This directory contains
that image.  It can be updated from the image in scheme-core/ by running
'make update' in scc0.

javac/ - This is work in progress to port a Java compiler I wrote in
in Common Lisp over to to vcsh.

