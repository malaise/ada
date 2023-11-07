# Open Location Code Ada API
This is an Ada implementation of the Open Location Code API.

# Install

The package 'olc' and the test program 't_olc' can be compiled with

```
gnatmake t_olc
```

# Using the API

The 'olc' package implements all the required and optional features of Open
Location Code specification, with the following differences:
* The input latitudes and longitudes are restricted to -90 .. 90 and -180 .. 180
respectively, thanks to the strong Ada typing.
* Input codes longer than 16 characters (precision higher than 15) are rejected
as invalid.

# Test program

The t_olc program takes various commands and arguments in the command line and
displays the result produced by the library.

```
Usage: t_olc <command>
  <commnd> ::= <encode> | <decode> | <center> | <status> | <shorten> | <nearest> | <help>
  <encode>  ::= -c <coord> <precision>
  <coord>   ::= <lat> <lon>
  <decode>  ::= -d <code>
  <center>  ::= -C <code>
  <status>  ::= -S <code>
  <shorten> ::= -s <code> <coord>
  <nearest> ::= -n <code> <coord>
  <help>    ::= -h
```

Example:
```
t_olc -c 3 21.5 11
  => 6GM32G22+222
```

# Test script

The Test script without argument runs automatically the tests described in the
'test' subdirectory (Encode, Decode, Status and Short).

With a path as argument it runs the open-location-code legacy
tests in the provided path (encoding.csv decoding.csv validityTests.csv and
shortCodeTests.csv).

