-- Parser of command line arguments
-- It allows keys to be set by "-f" or by "--flag",
--  by "-o set" or by "--option=set".
-- Single letter keys can be grouped, like "-of".
-- The parsing stops at last argument or when finding "--"

-- The restrictions on arguments are:
-- - an argument containing spaces or minus is forbidden (and detected).
-- - when grouped, single char keys cannot have options. So, if "-o/--option"
--   can have an option, then in case of "-ao toto" it has no option.
-- - any argument starting by "-" must match the input specification.

with Ada.Strings.Unbounded;
with Argument;
package Argument_Parser is

  -- Max length of an argument
  Max_Arg_Len : constant := Argument.Max_Len_Arg;

  -- Usefull renaming for Unbounded Strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  Asu_Nus : constant  Asu_Us := Asu.Null_Unbounded_String;

  -- No key when character key, when string key
  No_Key_Char : constant Character := ' ';
  No_Key_String : constant Asu_Us := Asu_Nus;
  -- A key definition
  type A_Key_Type is record
    Key_Char : Character := No_Key_Char;
    Key_String : Asu_Us := No_Key_String;
    Key_Can_Multiple : Boolean := False;
    Key_Can_Option : Boolean := False;
  end record;
  -- Max number of keys processed
  Max_Keys_Nb : constant := 1024;
  subtype The_Keys_Range is Positive range 1 .. Max_Keys_Nb;
  -- The keys for parsing
  type The_Keys_Type is array (The_Keys_Range range <>) of A_Key_Type;

  -- The result of parsing
  type Parsed_Dscr is tagged private;

  -- Constructor
  -- May raise:
  No_Key : exception;   -- One key has both Char_Key and String_Key unset
  Dup_Key : exception;  -- Two keys have same Char or String key
  function Parse (The_Keys : The_Keys_Type) return Parsed_Dscr;

  -- Was parsing OK
  function Is_Ok (Dscr : Parsed_Dscr) return Boolean;

  -- Error string
  -- Possible returned strings:
  --  "OK."
  --  "Error: Argument <arg> at pos <i> contains space(s)."
  --  "Error: Argument <arg> at pos <i> contains minus."
  --  "Error: Argument <arg> at pos <i> is not valid."
  --  "Error: Argument <arg> at pos <i> is not expected."
  --  "Error: Argument <arg> at pos <i> has not expected key <k>."
  --  "Error: Argument at pos <i> is too long."
  --  "Error: Argument <arg> at pos <i> appears several times."
  --  "Error: Argument <arg> at pos <i> cannot have option."
  function Get_Error (Dscr : Parsed_Dscr) return String;

  -- All the following operations may raise
  Parsing_Error : exception;
  --  if called on a Dscr that is not Parsed_Is_Ok.

  -- Clean memory allocated during parsing
  procedure Reset (Dscr : in out Parsed_Dscr);

  -- Return the position of the last argument related to keys (in case
  --  of char key with option, it is the position of the key).
  function Get_Last_Pos_Of_Keys (Dscr : Parsed_Dscr) return Natural;

  -- Return position of first argument not related to keys (taking into account
  --  the possible option of a char key) and skipping "--" if any
  function Get_First_Pos_After_Keys (Dscr : Parsed_Dscr) return Natural;

  -- The following operations alow retreiving info per key
  -- Index is relative to the array provided as input

  -- Nb of occurences of the key, possibly 0
  function Get_Nb_Occurences (Dscr  : Parsed_Dscr;
                              Index : The_Keys_Range) return Natural;

  -- Option of a key, possibly empty
  -- May raise, if Occurence > Get_Nb_Occurences(Index)
  Invalid_Occurence : exception;
  function Get_Option (Dscr      : Parsed_Dscr;
                       Index     : The_Keys_Range;
                       Occurence : Positive) return String;

  -- Absolute position of an occurence, for use with Argument.Get_parameter.
  -- This function is normally not necessary.
  -- May raise Invalid_Occurence if Occurence > Get_Nb_Occurences(Index)
  function Get_Position (Dscr      : Parsed_Dscr;
                         Index     : The_Keys_Range;
                         Occurence : Positive) return Positive;
private

  type Nat_Array is array (1 .. Max_Keys_Nb) of Natural;
  type Keys_Access is access The_Keys_Type;

  type Parsed_Dscr is tagged record
    Ok : Boolean := False;
    Error : Asu_Us := Asu_Nus;
    The_Keys : Keys_Access;
    Last_Pos_Key : Natural := 0;
    First_Pos_After_Keys : Natural := 0;
    Nb_Occurences : Nat_Array := (others => 0);
    First_Occurence : Nat_Array := (others => 0);
  end record;

end Argument_Parser;

