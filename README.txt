Ada notes
=========

This README file describes briefly the main directories of the Ada contrib.

The software compiles with Ada05 (Gnat GPL 2010) and runs on Unix (Linux).

Some programs depend on PCRE (Perl Compatible Regular Expressions) and work
with version 7.8 or above (8.11 is OK).

Programs are rated from 1 (simple) to 3 (very complex), on subjective
criteria.

Gnatmake (optional)
-------------------
The script ada/gnatmake can be set in the PATH (you can copy it or make a link
from $HOME/bin/gnatmake to it).

Similarly, the commands "ada" and "gnatlink" can be in the path and be this
gnatmake (you can copy it or make links from $HOME/bin/ada and
$HOME/bin/gnatlink to it).

Automatic generation by make (see next section) doesn't use them but these
scripts are convenient.

Makefiles project (MANDATORY)
-----------------------------
This project gathers several generic definitions for C and Ada generation.
Several examples of usage can be found in the source directories.

Relocation of the Makefiles directory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the Makefiles installation directory is not $(HOME)/Makefiles, then

- the definition of TEMPLATES in common.mk has to be adapted,

- all makefiles must have their first include directive of common.mk adapted.

Relocation of C or Repository
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the c or the repository directory are not in $(HOME)/ada, then the template
path.mk of Makefiles must be modified.

Recursive make
~~~~~~~~~~~~~~
The content of the makefile for applying make to subdirecotries is:

SUBDIRS := c reposit usr

include $(HOME)/Makefiles/dir.mk

Other directives can be added if something has also to be compiled in current
dir.

C directory (MANDATORY)
-----------------------
Four libraries here:

.Lib C util (cutil)
This lib gathers several low level utility libraries:

- timeval used by other C libraries, for struct timeval operations,

- socket interfaced by Ada Socket package, interfaces socket (tcp, udp,
ipm, message on tcp, afinet or afux) calls,

- sys_calls, interfaced by Ada Sys_Calls and some other Ada packages, for
several operating system calls,

- wait_evt, interfaced by Ada Even_Mng package, waits (with "select") on
several fds, catches signals...

.Lib X Mng (xmng)
This lib encapsulates all needed calls to X11, interfaced by X_Mng Ada
package.

.Lib C Ndbm (cnbdbm)
This lib interfaces with ndbm C library. The corresponding Ndbm package is not
used at present (except in test).

.Lib Posix to PCRE (posic2pcre)
Own binding of PCRE to POSIX API, with extension, interfaced by
Regular_Expressions Ada package.

Reposit directory
-----------------
Many utility packages here.
Some of them are interdependant, the main dependency trees are:

- Graphic (X11) function: X_Mng <- Con_Io <- Afpx and Curve

- Communication, Timers: Socket, Timers and Event_Mng <- Tcp_Util <-
Autobus (and Channels)

See link:reposit/REPOSIT_LIST.html[] for the list of packages and how to use
them.

5 Usr directories
-----------------
Each ot them hosts a standalone project except tests (test programs of
repository packages) and tasks (simple task examples).

They all depend on C and Reposit and are independant from the others (except
Pipe -> Fifos -> Dictio).

The more usefull are Agite, Alook, Als, Asubst, Lsadeps, Mcd, Udp_Spy and
Xml_Checker.

The games are G, Lem, Mmind, Nimmari and Sokoban.

Here is the full list and description:

Account (3)::
     Based on Afpx, this tool makes the management of bank accounts, with
     cheque, credit card (defered) and transfer operations.

Agite (3)::
     GUI for Git, dir oriented.

Als (2)::
     Lists, more or less the "ls" way, files and directories. Allow selection
     by date, type, regexp. Allows merging of all outputs and sorting.

Approx (3)::
     Using Afpx and Curve, the program computes and displays the polynomial
     approximation of a set of points in the plan.

Astub (3)::
     Ada05 stubber (makes compilable empty body from spec).

Asubst (3)::
     Substitutes strings matching regexp by strings in files.

Chess (3)::
     With Con_Io and Tcp_Util, allows 2 players to play chess on one or 2
     computers.  Checks movement validity (already tricky) but does not play
     against human.

C_Magic (1)::
     Small program that computes magic squares (numbers from to 1 in a
     square, with same sum on all rows, cols and diags). Consumes a lot of
     CPU despite many optims :-)

Code (1)::
     Simple programs that codes (crypts) and decodes a text. The crypting algo
     are old and not secure.

Day_Of_Week (1)::
     Simple program that tells everything on today (or on another day).

Dictio (3)::
     Distributed data dictionnary that allows to store data, distribute this
     data base on all the machines. Ensures correct insertion of new machines
     and constant consistency. Recovery of data loss may be optimized
     (resends all).

Enigma (2)::
     Encodes/decodes text like german enigma machine did in ww2.

Fifos (2)::
     Use Dictio to make distributed point to point connections (logical
     adress, automatic connection and re-connection).

Fl (1)::
     Simple computation of fligh log (adding hours and minutes).

Great_Circle (1)::
     Computation of heading and distance between two points on earth (GPS).

Heart (3)::
     DB of heart rate measures, displayed with Afpx.

Hungar (2)::
     Euristic search (hungarian method) of best affection (e.g. of peaple to
     jobs).  Tough algo.

Lem (2)::
     Game: land the LEM on the moon.

Lsadeps (2)::
     Shows the list/tree of Ada units/files dependencies

Mcd (3)::
     Reverse polish computer. At command line level but very powerful.

Misc::
     Many simple programs here. The most interesting are:
- Alook (1) properly formats the words of a (valid) Ada05 source file.
- Dtd_Checker (1) check a DTD file (of a xml).
- Prime (1) search prime numbers.
- Stat (1) counts the number of instruction of (valid) Ada source files.
- Status (1) evaluates if a target file needs to be rebuilt.
- Tcping (1) pings in tcp a host/port.
- Trail_spaces (1) removes tabs and trailing spaces and dos2unix a file.
- Udp_Spy (2) displays udp/ipm packets received on a port.
- Xml_Checker (2) uses Xml parser and generator (from reposit) to check XML.

Mmind (2)::
     Mastermind game.

Navig (2)::
     Navigation computation (wind, speed, heading, route, drift...).

Nimmari (2)::
     Nim and Marienbad games.

Pexec (1)::
     Recursive execution on directories of commands.

Pipe (1)::
     Uses Fifo to relay stdin to another host where another pipe can put it on
     stdout.

Renardeau (2)::
     Computes a target number by combining base numbers.

Simusi (2)::
     Machining simulation. (Tough algo).

Sokoban (2)::
     The famous Sokoban game (push the boxes).

Tasks::
     Several simple programs using tasks.

Tcpchat (3)::
     Accepts connection of a TCP port, expects sentences and executes
     specified actions depending on what it receives.

Tcpipe (2)::
     As a bridge, multiplexes and relays TCP connections.

Tests::
     Several small test programs of the Repository packages.

Xwords (2)::
     A graphical lexicography of common words and nouns (french DB).

Cots directories
----------------

This directory mainly contains a copy of:

- pragmarc and mine from http://pragmada.home.mchsi.com (under GPL)

- LinXtris from http://sourceforge.net/projects/linxtris (under GPL)

