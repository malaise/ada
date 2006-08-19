readme file

PragmAda Software Engineering
PragmAda Reusable Components (PragmARCs)

2006 May 01 release

Files use the GNAT default file names for systems with long-file-name support.

The source code for the components themselves are in files named

   pragmarc*.ad?


The remaining files are

readme.txt              This file

license.txt             License for the components (repeated in every source file)

gpl.txt                 The GNU Public License

arc_list.txt            Brief descriptions of the PragmARCs

assertion_handler.adb   An alternative body for PragmARC.Assertion_Handler

compile_all.adb         Building this null program will compile the PragmARCs

design.txt              Some design notes

The files in the Test directory are test and example programs. Of interest may be

calc.adb          A full-screen postfix calculator for ANSI-standard displays

devil.adb         A full-screen solitaire game for ANSI-standard displays

devil2.adb        A solitaire game for non-ANSI displays

strm_sub.adb      A regular-expression substitution filter

xor.adb           A Recursive Error Minimization (REM) neural network to solve the XOR problem


Please e-mail error reports, comments, and suggestions to

   pragmada@mchsi.com


Changes since 2006 Mar 01 release:

Added PragmARC.Genetic_Algorithm and its test program.
