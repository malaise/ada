with MY_IO;
with BIT_OPS;
with DYN_DATA;
package body HASH is

  function MAX_HASH_FUNC (KEY : STRING) return MAX_HASH_RANGE is
    INT : INTEGER := 0;
    BH, BL : INTEGER;
    use BIT_OPS;

  begin
    BH := 0;
    BL := 0;
    for I in KEY'RANGE loop
      BL := BL xor CHARACTER'POS(KEY(I));
      BH := (BH +  CHARACTER'POS(KEY(I))) and 16#007F#;
    end loop;
    -- lowest bit is same in BH and BL, reset in BH
    BH := SHL (BH and 16#7E#, 3);  -- Max  7F = ..1111110... = 127
    BL := BL and 16#007F#;         -- Max  7F = .....1111111 = 127
    INT := BH xor BL;              -- Max 3FF = 001111111111 = 1023
    return MAX_HASH_RANGE (INT);
  end MAX_HASH_FUNC;


  package body HASH_MNG is

    -- Data organization:
    --  An array (0 .. HASH_SIZE) of FIRST_CELL_REC
    --  Each of them pointing possibly to a CELL_REC which itself...
    type CELL_REC;
    type CELL_ACCESS is access CELL_REC;

    type CELL_REC is record
      DATA : DATA_ACESS;
      NEXT : CELL_ACCESS := null;
    end record;


    type FIRST_CELL_REC is record
      FIRST     : CELL_ACCESS := null;
      CURRENT   : CELL_ACCESS := null;
    end record;

    subtype HASH_RANGE is MAX_HASH_RANGE range 0 .. HASH_SIZE;

    -- The entries of the table
    FIRST_ARRAY : array (HASH_RANGE) of FIRST_CELL_REC;

    NOT_FOUND_REC : constant FOUND_REC
                  := (FOUND => FALSE);

    -- To manage the linked cells
    package DYN_HASH is new DYN_DATA (CELL_REC, CELL_ACCESS);


    function HASH_FUNC (KEY : STRING) return HASH_RANGE is
    begin
      return MAX_HASH_FUNC(KEY) rem HASH_SIZE;
    end HASH_FUNC;

    -- To store association KEY <-> INDEX
    procedure STORE (KEY : in STRING; DATA : in DATA_ACESS) is
      I : constant HASH_RANGE := HASH_FUNC(KEY);
      CA, N : CELL_ACCESS;
    begin
      CA := DYN_HASH.ALLOCATE ((DATA => DATA, NEXT  => null));

      -- Append
      if FIRST_ARRAY(I).FIRST = null then
        FIRST_ARRAY(I).FIRST := CA;
      else
        N := FIRST_ARRAY(I).FIRST;
        while N.NEXT /= null loop
          N := N.NEXT;
        end loop;
        N.NEXT := CA;
      end if;

    end STORE;

    -- To remove a stored association KEY <-> INDEX
    procedure REMOVE (KEY : in STRING) is
      I : constant HASH_RANGE := HASH_FUNC(KEY);
      CA : CELL_ACCESS;
      CU : CELL_ACCESS;
    begin
      CU := FIRST_ARRAY(I).CURRENT;
      CA := FIRST_ARRAY(I).FIRST;
      -- Empty or not found
      if CA = null or else CU = null then
        raise NOT_FOUND;
      end if;

      -- Reset current
      FIRST_ARRAY(I).CURRENT := null;

      if CA = CU then
        -- Special case when current is first
        FIRST_ARRAY(I).FIRST := CA.NEXT; 
      else
        -- Find previous of current
        while CA.NEXT /= CU loop
          CA := CA.NEXT;
        end loop;
        CA.NEXT := CU.NEXT;
      end if;

      -- Get rid of current
      DYN_HASH.FREE (CU);

    end REMOVE;

    procedure RESET_FIND (KEY : STRING) is
      I : constant HASH_RANGE := HASH_FUNC(KEY);
    begin
      FIRST_ARRAY(I).CURRENT := null;
    end RESET_FIND;

    -- To get next INDEX matching KEY
    function FIND_NEXT (KEY : STRING) return FOUND_REC is
      I : constant HASH_RANGE := HASH_FUNC(KEY);
      CU : CELL_ACCESS;
    begin
      if FIRST_ARRAY(I).CURRENT = null then
        CU := FIRST_ARRAY(I).FIRST;
      else
        CU := FIRST_ARRAY(I).CURRENT.NEXT;
      end if;

      FIRST_ARRAY(I).CURRENT := CU;

      if CU = null then
        return NOT_FOUND_REC;
      else
        return (FOUND => TRUE, DATA => CU.DATA);
      end if;

    end FIND_NEXT;

    procedure DUMP (KEY : in STRING) is
      I : constant HASH_RANGE := HASH_FUNC(KEY);
      CA : CELL_ACCESS := FIRST_ARRAY(I).FIRST;
    begin
      MY_IO.PUT_LINE ("Hash " & HASH_RANGE'IMAGE(I));
      if CA = null then
        MY_IO.PUT_line (" No data found");
      end if;
      while CA /= null loop
        MY_IO.PUT (" Data found -> ");
        DUMP (CA.DATA);
        if CA = FIRST_ARRAY(I).CURRENT then
          MY_IO.PUT (" <- Current");
        end if;
        MY_IO.NEW_LINE;
        CA := CA.NEXT;
      end loop;
    end DUMP;

    procedure CLEAR_ALL is
      CA, CN : CELL_ACCESS;
    begin
      for I in HASH_RANGE loop
        CA := FIRST_ARRAY(I).FIRST;
        FIRST_ARRAY(I).FIRST := null;
        FIRST_ARRAY(I).CURRENT := null;

        while CA /= null loop
          CN := CA.NEXT;
          DYN_HASH.FREE (CA);
          CA := CN;
        end loop;
      end loop;
    end CLEAR_ALL;

  end HASH_MNG;

end HASH;

