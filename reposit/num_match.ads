-- Check if a given num matches a criteria
-- <criteria> ::= [ <specs> ]
-- <specs>   ::= <spec> [ { , <spec> } ]
-- <spec>    ::= <num> | <range>
-- <range>   ::= [ <num> ] - [ <num> ]
-- <num>     ::= Natural
-- Raises Constraint_Error if incorrect criteria
-- Example:  "-5,10-15,7,30-"
function Num_Match (Num : in Natural; Criteria : in String) return Boolean;

