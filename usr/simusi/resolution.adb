with COMMON, DATA, DISPLAY;
use COMMON, DATA;
package body RESOLUTION is

  -- Current kind of solving
  KIND : COMMON.COTE_KIND;
  -- Current level in kind
  LEVEL : DATA.EFF_COTE_RANGE;

  -- Mattrix of booleans
  type BOOL_MATTRIX is array (DATA.EFF_LINE_RANGE, DATA.EFF_LINE_RANGE)
    of BOOLEAN;
  G : BOOL_MATTRIX;

  -- A logical cote
  type LOG_COTE_REC is record
    NO : DATA.EFF_COTE_RANGE;
    START, STOP : DATA.EFF_LINE_RANGE;
  end record;

  package COTE_SEARCH is
    -- Init LEVEL
    procedure INIT;
    -- May increment LEVEL
    function NEXT_COTE return LOG_COTE_REC;
  end COTE_SEARCH;

  procedure INIT is
    START, STOP : DATA.EFF_LINE_RANGE;
  begin
    G := (others => (others => FALSE));

    -- G(l, c)  has true is exists cote(l, c)
    -- Cote is kind
    for I in DATA.EFF_COTE_RANGE loop
      if KIND = COMMON.MANUFA then
        START := MANUFAS(I).START;
        STOP := MANUFAS(I).STOP;
      else
        START := DESIGNS(I).START;
        STOP := DESIGNS(I).STOP;
      end if;
      G(START, STOP) := TRUE;
      G(STOP, START) := TRUE;
    end loop;
    COTE_SEARCH.INIT;
  end INIT;

  
  package body COTE_SEARCH is
    GP : BOOL_MATTRIX;

    type BOOL_VECTOR is array (EFF_COTE_RANGE) of BOOLEAN;
    COTE_DONE : BOOL_VECTOR;

    subtype COTE_NB_RANGE is NATURAL range 0 .. DATA.EFF_COTE_RANGE'LAST;
    NB_DONE : COTE_NB_RANGE;
    LAST_COTE_NB : COTE_NB_RANGE;

    procedure INIT is
    begin
      LEVEL := 1;
      GP := G;
      COTE_DONE := (others => FALSE);
      NB_DONE := 0;
      LAST_COTE_NB := 0;
    end INIT;

    procedure PRODG is
      R : BOOL_MATTRIX;
    begin
      for I in DATA.EFF_LINE_RANGE loop
        for J in DATA.EFF_LINE_RANGE range 1 .. I loop
          for K in DATA.EFF_LINE_RANGE loop
            R(I, J) := GP(I, K) and then G(K, J);
            exit when R(I,J);
          end loop;
          R(J,I) := R(I,J);
        end loop;
      end loop;
      GP := R;
    end PRODG;

    function NEXT_COTE return LOG_COTE_REC is
      I : COTE_NB_RANGE;
      C : LOG_COTE_REC;
    begin
      
      -- Search from previously returned cote
      I := LAST_COTE_NB;
      loop
        -- Next cote, or first at next level
        if I /= DATA.EFF_COTE_RANGE'LAST then
          I := I + 1;
        elsif LEVEL /= DATA.EFF_COTE_RANGE'LAST then
          -- Incr level and restart from first cote
          I := 1;
          LEVEL := LEVEL + 1;
          PRODG;
        else
          -- Pb
          for J in DATA.EFF_COTE_RANGE loop
            if not COTE_DONE(J) then
              DISPLAY.PUT_NO_WAY (KIND, J);
            end if;
          end loop;
          raise ABORT_ERROR;
        end if;

        if not COTE_DONE(I) then
          -- When manufa, search a design cote in gp
          if KIND = COMMON.MANUFA then
            C.START := DATA.DESIGNS(I).START;
            C.STOP  := DATA.DESIGNS(I).STOP;
          else
            C.START := DATA.MANUFAS(I).START;
            C.STOP  := DATA.MANUFAS(I).STOP;
          end if;
          -- Search if cote complies with level
          if GP(C.START, C.STOP) then
            LAST_COTE_NB := I;
            COTE_DONE(I) := TRUE;
            C.NO := I;
            return C;
          end if;
        end if;
      end loop;
    end NEXT_COTE;

  end COTE_SEARCH;


  -- Search in G the sequence of cotes which make
  -- COTE.
  function SEARCH_WAY (COTE : LOG_COTE_REC)
                      return DISPLAY.WAY_VECTOR is
    WAY : DISPLAY.WAY_VECTOR(1 .. DATA.EFF_LINE_RANGE'LAST);
    subtype WAY_RANGE is NATURAL range 0 .. DATA.EFF_LINE_RANGE'LAST;
    NEXT, PREV : WAY_RANGE;
    INDEX : EFF_LINE_RANGE;

    procedure PUSH is
    begin
      PREV := WAY(INDEX);
      INDEX := INDEX + 1;
      WAY(INDEX) := NEXT;
      NEXT := 0;
    end PUSH;

    procedure POP is
    begin
      NEXT := WAY(INDEX);
      INDEX := INDEX - 1;
      PREV := WAY(INDEX);
    end POP;

  begin
    -- Init at cote.start
    WAY(1) := COTE.START;
    INDEX := 1;
    PREV := COTE.START;
    NEXT := 0;
    loop
      -- Increment NEXT
      if NEXT /= DATA.EFF_LINE_RANGE'LAST then
        NEXT := NEXT + 1;
        -- Search (cur, next) in g
        if NEXT /= PREV and then NEXT /= WAY(INDEX)
        and then G(WAY(INDEX), NEXT) then
          -- Found it
          if INDEX = LEVEL then
            if NEXT = COTE.STOP then
              -- The last one
              PUSH;
              return WAY(1 .. INDEX);
            end if;
          else
            PUSH;
          end if;
        end if;
      else
        -- No more cote for this current
        POP;
      end if;
    end loop;
  end SEARCH_WAY;
  

  procedure SOLVE (KIND : in COMMON.COTE_KIND) is
    CURRENT_COTE : LOG_COTE_REC;
  begin
    RESOLUTION.KIND := KIND;
    INIT;
    for I in EFF_COTE_RANGE loop
      -- Search next cote
      CURRENT_COTE := COTE_SEARCH.NEXT_COTE;
      -- Find way of cote
      declare
        WAY : constant DISPLAY.WAY_VECTOR := SEARCH_WAY(CURRENT_COTE);
      begin
        -- Display way
        DISPLAY.PRINT (KIND, CURRENT_COTE.NO, WAY);
      end;
    end loop;

  end SOLVE;

end RESOLUTION;

