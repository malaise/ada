-- P. MALAISE
with SYSTEM;
package TEXT_HANDLER is

  subtype MAX_LEN_RANGE is INTEGER range 0 .. 32*1024;

  type TEXT (MAX_LEN : MAX_LEN_RANGE) is limited private;

  EMPTY_TEXT : constant TEXT;

  function LENGTH (T : TEXT) return MAX_LEN_RANGE;
  function VALUE  (T : TEXT) return STRING;
  function EMPTY  (T : TEXT) return BOOLEAN;

  procedure EMPTY (T : in out TEXT);

  function TO_TEXT (S : STRING;    MAX_LEN : MAX_LEN_RANGE) return TEXT;
  function TO_TEXT (C : CHARACTER; MAX_LEN : MAX_LEN_RANGE) return TEXT;
  function TO_TEXT (S : STRING)    return TEXT;
  function TO_TEXT (C : CHARACTER) return TEXT;

  function "&" (LEFT : TEXT;      RIGHT : TEXT)      return TEXT;
  function "&" (LEFT : TEXT;      RIGHT : STRING)    return TEXT;
  function "&" (LEFT : STRING;    RIGHT : TEXT)      return TEXT;
  function "&" (LEFT : TEXT;      RIGHT : CHARACTER) return TEXT;
  function "&" (LEFT : CHARACTER; RIGHT : TEXT)      return TEXT;

  function "="  (LEFT : TEXT; RIGHT : TEXT) return BOOLEAN;
  function "<"  (LEFT : TEXT; RIGHT : TEXT) return BOOLEAN;
  function "<=" (LEFT : TEXT; RIGHT : TEXT) return BOOLEAN;
  function ">"  (LEFT : TEXT; RIGHT : TEXT) return BOOLEAN;
  function ">=" (LEFT : TEXT; RIGHT : TEXT) return BOOLEAN;

  procedure SET (TO : in out TEXT; VALUE : in TEXT);
  procedure SET (TO : in out TEXT; VALUE : in STRING);
  procedure SET (TO : in out TEXT; VALUE : in CHARACTER);

  procedure APPEND (TO : in out TEXT; TAIL : in TEXT);
  procedure APPEND (TO : in out TEXT; TAIL : in STRING);
  procedure APPEND (TO : in out TEXT; TAIL : in CHARACTER);

  procedure AMEND (TO : in out TEXT; BY : in TEXT;
   POSITION : in MAX_LEN_RANGE);
  procedure AMEND (TO : in out TEXT; BY : in STRING;
   POSITION : in MAX_LEN_RANGE);
  procedure AMEND (TO : in out TEXT; BY : in CHARACTER;
   POSITION : in MAX_LEN_RANGE);

  function LOCATE (WITHIN : TEXT; FRAGMENT : TEXT; OCCURENCE : POSITIVE := 1)
   return MAX_LEN_RANGE;
  function LOCATE (WITHIN : TEXT; FRAGMENT : STRING; OCCURENCE : POSITIVE := 1)
   return MAX_LEN_RANGE;
  function LOCATE (WITHIN : TEXT; FRAGMENT : CHARACTER; OCCURENCE : POSITIVE := 1)
   return MAX_LEN_RANGE;

  -- Appends ASCII.NULL to FROM.VAL (FROM.MAX_LEN must be long enough).
  -- STRING_ADDRESS is FROM.VAL'ADDRESS and can then be passed to a C function.
  procedure STRING_FOR_C (FROM : in out TEXT; STRING_ADDRESS : out SYSTEM.ADDRESS);

private

  type TEXT (MAX_LEN : MAX_LEN_RANGE) is record
    LEN : MAX_LEN_RANGE := 0;
    VAL : STRING (1..MAX_LEN);
  end record;

  EMPTY_TEXT : constant TEXT := (MAX_LEN => 0, LEN => 0, VAL => "");

end TEXT_HANDLER;
