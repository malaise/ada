-- Check if a given num matches a criteria
-- <criteria> ::= [ <specs> ]
-- <specs>   ::= <spec> [ { , <spec> } ]
-- <spec>    ::= <num> | <range>
-- <range>   ::= [ <num> ] - [ <num> ]
-- <num>     ::= Natural
-- No num matches the empty spec "", every num matches the range "-"
-- Raises Invalid_Criteria if incorrect criteria
-- Raises Constraint_Error if Num < 0
-- The order of the successive <spec> is not significant
-- Example:  "-5,10-15,7,30-"
generic
  type Integer_Type is range <>;
package Num_Match is


  Invalid_Criteria : exception;
  function Matches (Num : in Integer_Type; Criteria : in String) return Boolean;

end Num_Match;
