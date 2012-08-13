-- Parser of command line arguments
-- It allows keys to be set by "-f" or by "--flag",
--  by "-o set" or by "--option=set".
-- Single letter keys can be grouped, like "-of".
-- The parsing stops at last argument or when finding "--"

-- The restrictions on keys (detected and raising exceptions):
-- - a string key shall not contain spaces or unprintable characters,
--    nor start with '-'
-- - a char key shall be a printable character and not '-'
-- - there shall be at least a char or a string definition for any key
-- - any char or a string definition shall be unique

-- The conventions and restriction on arguments are:
-- - any argument starting by '-', except "-" and "--", is considered as a key
--    and thus must match the input specification.
-- - any argument except --key and -key(s) following a key char that accepts
--    options is considered as an option. So, if key 'o' can have an option,
--    then "-o opt" or "-ao opt" lead "opt" to be an option of key o.
-- - an argument "--" leads to stop the parsing of keys. Any following argument
--    is considered as No_Key (even another "--").
-- - an argument "-" is considered as a not a key (either option or No_Key).

with Ada.Finalization;
with As.U;
package Argument_Parser is

  -- No key when character key, when string key
  No_Key_Char : constant Character := ' ';
  No_Key_String : constant As.U.Asu_Us := As.U.Asu_Null;
  -- A key definition
  type A_Key_Type (Key_Can_Option : Boolean := False) is record
    Key_Char : Character := No_Key_Char;
    Key_String : As.U.Asu_Us := No_Key_String;
    Key_Can_Multiple : Boolean := False;
    case Key_Can_Option is
      when True =>
        Required : Boolean := False;
        Option_Name : As.U.Asu_Us := As.U.Asu_Null;
      when False =>
        null;
    end case;
  end record;
  -- Max number of keys processed
  Max_Keys_Nb : constant := 1024;
  subtype The_Keys_Index is Natural range 0 .. Max_Keys_Nb;
  subtype The_Keys_Range is The_Keys_Index range 1 .. Max_Keys_Nb;
  -- The keys for parsing
  type The_Keys_Type is array (The_Keys_Range range <>) of A_Key_Type;

  -- Returns the string image of a key
  -- If only a char C => '-C'
  -- If only a string Str => "--Str"
  -- If both are set => "-C | --Str"
  -- If it can have option, -C <opt> and --Str=<opt>
  --                    or  -C [<opt>] and --Str[=<opt>]
  --   depending on Opt_Required
  -- "opt" is the Option_Name of the key itf not null,
  -- else the Key_String  of the key itf not null,
  -- else "option"
  function Image (Key : A_Key_Type) return String;

  -- The result of parsing
  type Parsed_Dscr is tagged private;

  -- Constructor
  -- May raise exception if incorrect setting in The_Keys:
  No_Key : exception;       -- One key has both Char_Key and String_Key unset
  Dup_Key : exception;      -- Two keys have same Char or String key
  Invalid_Key : exception;  -- One key contains a unprintable char or space
  function Parse (The_Keys : The_Keys_Type) return Parsed_Dscr;

  -- Free the keys, clean memory allocated during parsing
  procedure Reset (Dscr : in out Parsed_Dscr);

  -- Was parsing of arguments OK
  function Is_Ok (Dscr : Parsed_Dscr) return Boolean;

  -- Error string
  -- Possible returned strings:
  --  "OK."
  --  "Argument <arg> at pos <i> contains minus."
  --  "Argument <arg> at pos <i> is not valid."
  --  "Argument <arg> at pos <i> is not expected."
  --  "Argument <arg> at pos <i> has not expected key <k>."
  --  "Argument at pos <i> is too long."
  --  "Argument <arg> at pos <i> appears several times."
  --  "Argument <arg> at pos <i> cannot have option."
  function Get_Error (Dscr : Parsed_Dscr) return String;

  -- All the following operations may raise, if called on a Dscr that is not
  --  Parsed_Is_Ok:
  Parsing_Error : exception;

  -- Return the number of keys parsed
  -- Each occurence of a key plus its option counts for one
  function Get_Number_Keys (Dscr : Parsed_Dscr) return Natural;

  -- Return the position of the last argument related to keys (in case
  --  of char key(s) with an option, it is the position of the key).
  function Get_Last_Pos_Of_Keys (Dscr : Parsed_Dscr) return Natural;

  -- Return position of first argument not related to keys (taking into account
  --  the possible option of a char key) and skipping the first "--" if any
  function Get_First_Pos_After_Keys (Dscr : Parsed_Dscr) return Natural;

  -- Return the number of embedded (neither key nor option) arguments that are
  --  before last key
  function Get_Nb_Embedded_Arguments (Dscr : Parsed_Dscr) return Natural;

  -- The following operations alow retreiving info per key
  -- Index is relative to the array provided as input
  -- 0 means no key (any argument that is neither a key nor the option of a
  --  char key).
  No_Key_Index : constant The_Keys_Index := 0;

  -- All the following operations may raise, if called with Index too high:
  Invalid_Index : exception;

  -- Nb of occurences of the key, possibly 0
  function Get_Nb_Occurences (Dscr  : Parsed_Dscr;
                              Index : The_Keys_Index) return Natural;
  function Is_Set (Dscr  : Parsed_Dscr;
                   Index : The_Keys_Index) return Boolean;

  -- All the following operations may raise, if called with Occurence too high:
  Invalid_Occurence : exception;

  -- Option of a key, possibly empty
  function Get_Option (Dscr      : Parsed_Dscr;
                       Index     : The_Keys_Index;
                       Occurence : Positive := 1) return String;

  -- Kind of a key (char or string)
  function Is_Char (Dscr      : Parsed_Dscr;
                    Index     : The_Keys_Index;
                    Occurence : Positive := 1) return Boolean;

  -- Absolute position of an occurence
  function Get_Position (Dscr      : Parsed_Dscr;
                         Index     : The_Keys_Index;
                         Occurence : Positive := 1) return Positive;
private

  type Keyed_Array is array (The_Keys_Index) of Natural;
  type Keys_Access is access The_Keys_Type;

  type Parsed_Dscr is new Ada.Finalization.Controlled with record
    Ok : Boolean := False;
    Error : As.U.Asu_Us := As.U.Asu_Null;
    The_Keys : Keys_Access;
    Last_Pos_Key : Natural := 0;
    First_Pos_After_Keys : Natural := 0;
    Nb_Embedded : Natural := 0;
    Nb_Occurences : Keyed_Array := (others => 0);
    First_Occurence : Keyed_Array := (others => 0);
  end record;
  overriding procedure Adjust (Dscr : in out Parsed_Dscr);
  overriding procedure Finalize (Dscr : in out Parsed_Dscr);

end Argument_Parser;

