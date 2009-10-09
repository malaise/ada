-- Check if a given num matches a criteria
-- <criteria> ::= [ <specs> ]
-- <specs>   ::= <spec> [ { , <spec> } ]
-- <spec>    ::= <num> | <range>
-- <range>   ::= [ <num> ] - [ <num> ]
-- <num>     ::= Natural
-- Raises Constraint_Error if incorrect criteria
-- Example:  "-5,10-15,7,30-"
generic
  type Integer_Type is range <>;
package Num_Match is

  subtype Natural_Type is Integer_Type range 0 .. Integer_Type'Last;

  function Matches (Num : in Natural_Type; Criteria : in String) return Boolean;

end Num_Match;
