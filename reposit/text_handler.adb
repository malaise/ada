package body TEXT_HANDLER is

  function LENGTH (T : TEXT) return MAX_LEN_RANGE is
  begin
    return T.LEN;
  end LENGTH;

  function VALUE  (T : TEXT) return STRING is
  begin
    return T.VAL (1 .. LENGTH(T));
  end VALUE;

  function EMPTY  (T : TEXT) return BOOLEAN is
  begin
    return LENGTH (T) = 0;
  end EMPTY;

  procedure EMPTY (T : in out TEXT) is
  begin
    T.LEN := 0;
  end EMPTY;

  function TO_TEXT (S : STRING; MAX_LEN : MAX_LEN_RANGE) return TEXT is
    T : TEXT (MAX_LEN);
  begin
    T.LEN := S'LENGTH;
    T.VAL (1 .. LENGTH(T)) := S;
    return T;
  end TO_TEXT;

  function TO_TEXT (C : CHARACTER; MAX_LEN : MAX_LEN_RANGE) return TEXT
   is
    T : TEXT (MAX_LEN);
  begin
    T.LEN := 1;
    T.VAL (1) := C;
    return T;
  end TO_TEXT;

  function TO_TEXT (S : STRING) return TEXT is
    T : TEXT (S'LENGTH);
  begin
    T.LEN := T.MAX_LEN;
    T.VAL := S;
    return T;
  end TO_TEXT;

  function TO_TEXT (C : CHARACTER) return TEXT is
    T : TEXT (1);
  begin
    T.LEN := 1;
    T.VAL (1) := C;
    return T;
  end TO_TEXT;


  function "&" (LEFT : TEXT; RIGHT : TEXT) return TEXT is
    T : TEXT (LEFT.MAX_LEN + RIGHT.MAX_LEN);
  begin
    T.LEN := LENGTH(LEFT) + LENGTH(RIGHT);
    T.VAL(1 .. LENGTH(T)) := VALUE(LEFT) & VALUE(RIGHT); 
    return T;
  end "&";

  function "&" (LEFT : TEXT; RIGHT : STRING) return TEXT is
    T : TEXT (LEFT.MAX_LEN + RIGHT'LENGTH);
  begin
    T.LEN := LENGTH(LEFT) + RIGHT'LENGTH;
    T.VAL(1 .. LENGTH(T)) := VALUE(LEFT) & RIGHT;
    return T;
  end "&";

  function "&" (LEFT : STRING; RIGHT : TEXT) return TEXT is
    T : TEXT (LEFT'LENGTH + RIGHT.MAX_LEN);
  begin
    T.LEN := LEFT'LENGTH + LENGTH(RIGHT);
    T.VAL(1 .. LENGTH(T)) := LEFT & VALUE(RIGHT);
    return T;
  end "&";

  function "&" (LEFT : TEXT; RIGHT : CHARACTER) return TEXT is
    T : TEXT (LEFT.MAX_LEN + 1);
  begin
    T.LEN := LENGTH(LEFT) + 1;
    T.VAL(1 .. LENGTH(T)) := VALUE(LEFT) & RIGHT;
    return T;
  end "&";

  function "&" (LEFT : CHARACTER; RIGHT : TEXT) return TEXT is
    T : TEXT (1 + RIGHT.MAX_LEN);
  begin
    T.LEN := 1 + LENGTH(RIGHT);
    T.VAL(1 .. LENGTH(T)) := LEFT & VALUE(RIGHT);
    return T;
  end "&";


  function "=" (LEFT : TEXT; RIGHT : TEXT) return BOOLEAN is
  begin
    return VALUE(LEFT) = VALUE(RIGHT);
  end "=";

  function "<" (LEFT : TEXT; RIGHT : TEXT) return BOOLEAN is
  begin
    return VALUE(LEFT) < VALUE(RIGHT);
  end "<";

  function "<=" (LEFT : TEXT; RIGHT : TEXT) return BOOLEAN is
  begin
    return VALUE(LEFT) <= VALUE(RIGHT);
  end "<=";

  function ">" (LEFT : TEXT; RIGHT : TEXT) return BOOLEAN is
  begin
    return VALUE(LEFT) > VALUE(RIGHT);
  end ">";

  function ">=" (LEFT : TEXT; RIGHT : TEXT) return BOOLEAN is
  begin
    return VALUE(LEFT) >= VALUE(RIGHT);
  end ">=";


  procedure SET (TO : in out TEXT; VALUE : in TEXT) is
  begin
    TO.VAL(1..LENGTH(VALUE)) := TEXT_HANDLER.VALUE(VALUE);
    TO.LEN := LENGTH(VALUE);
  end SET;

  procedure SET (TO : in out TEXT; VALUE : in STRING) is
  begin
    TO.VAL(1..VALUE'LENGTH) := VALUE;
    TO.LEN := VALUE'LENGTH;
  end SET;

  procedure SET (TO : in out TEXT; VALUE : in CHARACTER) is
  begin
    TO.VAL(1) := VALUE;
    TO.LEN := 1;
  end SET;


  procedure APPEND (TO : in out TEXT; TAIL : in TEXT) is
  begin
    TO.VAL(LENGTH(TO)+1 .. LENGTH(TO)+LENGTH(TAIL)) := VALUE(TAIL);
    TO.LEN := LENGTH(TO) + LENGTH(TAIL);
  end APPEND;

  procedure APPEND (TO : in out TEXT; TAIL : in STRING) is
  begin
    TO.VAL(LENGTH(TO)+1 .. LENGTH(TO)+TAIL'LENGTH) := TAIL;
    TO.LEN := LENGTH(TO) + TAIL'LENGTH;
  end APPEND;

  procedure APPEND (TO : in out TEXT; TAIL : in CHARACTER) is
  begin
    TO.VAL(LENGTH(TO)+1) := TAIL;
    TO.LEN := LENGTH(TO) + 1;
  end APPEND;


  procedure AMEND (TO : in out TEXT; BY : in TEXT; 
                   POSITION : in MAX_LEN_RANGE) is
  begin
    AMEND (TO, BY.VAL (1 .. BY.LEN), POSITION);
  end AMEND;

  procedure AMEND (TO : in out TEXT; BY : in STRING; 
                   POSITION : in MAX_LEN_RANGE) is
  begin
    if POSITION > TO.LEN then
      raise CONSTRAINT_ERROR;
    end if;
    if POSITION + BY'LENGTH - 1 > TO.MAX_LEN then
      raise CONSTRAINT_ERROR;
    end if;
    if POSITION + BY'LENGTH - 1 > TO.LEN then
      TO.LEN := POSITION + BY'LENGTH - 1;
      TO.VAL (POSITION .. TO.LEN) := BY;
    else
      TO.VAL (POSITION .. POSITION + BY'LENGTH - 1) := BY;
    end if;

  end AMEND;

  procedure AMEND (TO : in out TEXT; BY : in CHARACTER; 
                   POSITION : in MAX_LEN_RANGE) is
    S : constant STRING (1 .. 1) := BY & "";
  begin
    AMEND (TO, S, POSITION);
  end AMEND;


  function LOCATE (WITHIN : TEXT; FRAGMENT : TEXT; OCCURENCE : POSITIVE := 1) 
   return MAX_LEN_RANGE is
    FOUND_OCCURENCE : NATURAL := 0;
  begin
    for I in 1 .. LENGTH(WITHIN) - LENGTH(FRAGMENT) + 1 loop
      if VALUE(WITHIN)(I .. I+LENGTH(FRAGMENT)-1) = VALUE(FRAGMENT) then
        FOUND_OCCURENCE := FOUND_OCCURENCE + 1;
        if FOUND_OCCURENCE = OCCURENCE then
          return I;
        end if;
      end if;
    end loop;
    return 0;
  end LOCATE;


  function LOCATE (WITHIN : TEXT; FRAGMENT : STRING; OCCURENCE : POSITIVE := 1) 
   return MAX_LEN_RANGE is
    FOUND_OCCURENCE : NATURAL := 0;
  begin
    for I in 1 .. LENGTH(WITHIN) - FRAGMENT'LENGTH + 1 loop
      if VALUE(WITHIN)(I .. I+FRAGMENT'LENGTH-1) = FRAGMENT then
        FOUND_OCCURENCE := FOUND_OCCURENCE + 1;
        if FOUND_OCCURENCE = OCCURENCE then
          return I;
        end if;
      end if;
    end loop;
    return 0;
  end LOCATE;

  function LOCATE (WITHIN : TEXT; FRAGMENT : CHARACTER; OCCURENCE : POSITIVE := 1) 
   return MAX_LEN_RANGE is
    FOUND_OCCURENCE : NATURAL := 0;
  begin
    for I in 1 .. LENGTH(WITHIN) loop
      if VALUE(WITHIN)(I) = FRAGMENT then
        FOUND_OCCURENCE := FOUND_OCCURENCE + 1;
        if FOUND_OCCURENCE = OCCURENCE then
          return I;
        end if;
      end if;
    end loop;
    return 0;
  end LOCATE;

  procedure STRING_FOR_C (FROM : in out TEXT; STRING_ADDRESS : out SYSTEM.ADDRESS) is
  begin
    FROM.VAL(LENGTH(FROM) + 1) := ASCII.NUL;
    STRING_ADDRESS := FROM.VAL'ADDRESS;
  end STRING_FOR_C;

end TEXT_HANDLER;
