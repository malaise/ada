-- Check whether a number maches a criteria
-- <criteria> ::= [ <specs> ]
-- <specs>   ::= <spec> [ { , <spec> } ]
-- <spec>    ::= <num> | <range>
-- <range>   ::= [ <num> ] - [ <num> ]
-- <num>     ::= Natural
-- No num matches the empty spec "", every num matches the range "-"
-- The order of the successive <spec> is not significant
-- The match is true if the number matches at least one spec
-- Example:  "-5,10-15,7,30-" covers any number below or equal to -5,
--  7, numbers 10 to 15 included, and any number equal of larger than 30.
with Arbitrary;
package Match is
  generic
    type Integer_Type is range <>;
  package Num_Match is

    -- Check if a given num matches a criteria
    -- Raises Invalid_Criteria if incorrect criteria
    -- Raises Constraint_Error if Num < 0
    function Matches (Num : in Integer_Type; Criteria : in String)
             return Boolean;

    -- Expand a <specs> as a list of nums
    -- Max is the value to be used to expand "i-" (will lead to (i .. Max)
    -- Raises Invalid_Criteria if incorrect criteria
    -- Raises Constraint_Error if Max < 0
    type Integer_Array is array (Positive range <>) of Integer_Type;
    function Expand (Criteria : in String; Max : in Integer_Type)
             return Integer_Array;

  end Num_Match;

  package Arbi_Match is

    -- Check if a given num matches a criteria
    -- Raises Invalid_Criteria if incorrect criteria
    -- Raises Constraint_Error if Num < 0
    function Matches (Num : in Arbitrary.Number; Criteria : in String)
             return Boolean;

    -- Expand a <specs> as a list of nums
    -- Max is the value to be used to expand "i-" (will lead to (i .. Max)
    -- Raises Invalid_Criteria if incorrect criteria
    -- Raises Constraint_Error if Max < 0
    type Arbitrary_Array is array (Positive range <>) of Arbitrary.Number;
    function Expand (Criteria : in String; Max : in Arbitrary.Number)
             return Arbitrary_Array;

  end Arbi_Match;

  Invalid_Criteria : exception;

end Match;

