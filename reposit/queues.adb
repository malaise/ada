package body Queues is

  package body Lifo is
    Pile : array (1 .. Size) of Item;

    -- Ptr is the last pushed except if stack is empty
    --  then it is 0.
    Ptr : Len_Range := 0;

    -- Number of Items in Lifo
    function Length return Len_Range is
    begin
      return Ptr;
    end Length;

    -- Push an item
    procedure Push (X : in Item) is
    begin
      if Ptr = Size then
        raise Lifo_Full;
      end if;
      Ptr := Ptr + 1;
      Pile (Ptr) := X;
    end Push;

    -- Pop an item
    procedure Pop (X : out Item) is
    begin
      if Ptr = 0  then
        raise Lifo_Empty;
      end if;
      X := Pile (Ptr);
      Ptr := Ptr - 1;
    end Pop;

    -- Read without popping
    -- 1 gives the first to be popped (ptr)
    procedure Look_First (X : out Item; No : in No_Range := 1) is
    begin
      if Ptr = 0 then
        raise Lifo_Empty;
      end if;
      if No > Ptr then
        raise Lifo_Not;
      else
        X := Pile (Ptr - No + 1);
      end if;
    end Look_First;

    -- Read without popping
    -- 1 gives the last to be popped
    procedure Look_Last (X : out Item; No : in No_Range := 1) is
    begin
      if Ptr = 0 then
        raise Lifo_Empty;
      end if;
      if No > Ptr then
        raise Lifo_Not;
      else
        X := Pile (No);
      end if;
    end Look_Last;

    -- Remove first item and shift
    procedure Discard_Last is
    begin
      if Ptr = 0 then
        raise Lifo_Empty;
      end if;
      for I in 1 .. Ptr-1 loop
        Pile(I) := Pile (I+1);
      end loop;
      Ptr := Ptr - 1;
    end Discard_Last;

  end Lifo;

  package body Fifo is
    File : array (0..Size - 1) of Item;
    Ptr_In  : Natural range 0 .. Size - 1 := 0;
    Ptr_Out : Natural range 0 .. Size - 1 := 0;

    Full : Boolean := False;

    -- Ptr_In  points to the last pushed
    -- Ptr_Out points to the first to pop
    --  fifo full  is raised if and only if ptr_in  = ptr_out and full
    --  fifo empty is raised if and only if ptr_in  = ptr_out and not full

    -- Number of Items in Fifo
    function Length return Len_Range is
    begin
      if Ptr_In > Ptr_Out then
        return Ptr_In - Ptr_Out;
      elsif Ptr_In < Ptr_Out then
        return Ptr_In + Size - Ptr_Out;
      elsif Full then
        return Size;
      else
        return 0;
      end if;
    end Length;

    -- Push an item
    procedure Push (X : in Item) is
    begin
      if Ptr_In = Ptr_Out and then Full then
        raise Fifo_Full;
      end if;
      Ptr_In := (Ptr_In + 1) mod Size;
      File (Ptr_In) := X;
      Full := (Ptr_In = Ptr_Out);
    end Push;

    -- Pop an item
    procedure Pop (X : out Item) is
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Fifo_Empty;
      end if;
      Ptr_Out := (Ptr_Out + 1) mod Size;
      X := File (Ptr_Out);
      Full := False;
    end Pop;

    -- Read without popping
    -- 1 gives the last pushed
    procedure Look_Last (X : out Item; No : in No_Range := 1) is
      Loc : Natural range 0 .. Size - 1;
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Fifo_Empty;
      end if;
      Loc := (Ptr_In - No + 1) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc

      if      (Ptr_Out <  Ptr_In
                and then (Loc <= Ptr_Out or else  Ptr_In < Loc) )
      or else (Ptr_In  <= Ptr_Out
                and then (Loc <= Ptr_Out and then Ptr_In < Loc) ) then
        raise Fifo_Not;
      else
        X := File (Loc);
      end if;
    end Look_Last;

    -- Read without popping
    -- 1 gives the first to be popped
    procedure Look_First (X : out Item; No : in No_Range := 1) is
      Loc : Natural range 0 .. Size - 1;
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Fifo_Empty;
      end if;
      Loc := (Ptr_Out + No) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc

      if      (Ptr_Out <  Ptr_In
                and then (Loc <= Ptr_Out or else  Ptr_In < Loc) )
      or else (Ptr_In  <= Ptr_Out
                and then (Loc <= Ptr_Out and then Ptr_In < Loc) ) then
        raise Fifo_Not;
      else
        X := File (Loc);
      end if;
    end Look_First;

    -- Discard last pushed
    procedure Discard_Last is
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Fifo_Empty;
      end if;
      Ptr_In := (Ptr_In - 1) mod Size;
      Full := False;
    end Discard_Last;

  end Fifo;

  package body Prio is
    File : array (0 .. Size - 1) of Item;
    File_Prio : array (0 .. Size - 1) of Priority;
    subtype Typ_Ptr is Natural range 0 .. Size - 1;
    Ptr_In  : Typ_Ptr := 0;
    Ptr_Out : Typ_Ptr := 0;

    Full : Boolean := False;

    -- Ptr_In  points to the last pushed
    -- Ptr_Out points to the first to pop
    --  fifo full  is raised if and only if ptr_in  = ptr_out and full
    --  fifo empty is raised if and only if ptr_in  = ptr_out and not full

    -- Number of Items in Prio
    function Length return Len_Range is
    begin
      if Ptr_In > Ptr_Out then
        return Ptr_In - Ptr_Out;
      elsif Ptr_In < Ptr_Out then
        return Ptr_In + Size - Ptr_Out;
      elsif Full then
        return Size;
      else
        return 0;
      end if;
    end Length;

    -- Push an item
    procedure Push (X : in Item; P : in Priority := Priority'Last) is
      I, J : Typ_Ptr;
    begin
      if Ptr_In = Ptr_Out and then Full then
        -- Fifo full
        raise Prio_Full;
      elsif Ptr_In = Ptr_Out then
        -- Fifo empty, store X at its place
        Ptr_In := (Ptr_In + 1) mod Size;
        File (Ptr_In) := X;
        File_Prio (Ptr_In) := P;
      else
        -- Create empty slot
        Ptr_In := (Ptr_In + 1) mod Size;
        -- For each slot
        I := Ptr_In;
        loop
          J := (I-1) mod Size;
          -- Compare prios and test limits
          exit when (J = Ptr_Out) or else (File_Prio (J) >= P);
          -- Shift
          File (I) := File (J);
          File_Prio (I) := File_Prio (J);
          I := J;
        end loop;
        -- Store X
        File (I) := X;
        File_Prio (I) := P;
      end if;
      Full := (Ptr_In = Ptr_Out);
    end Push;

    -- Pop item with highest priority
    procedure Pop (X : out Item) is
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Prio_Empty;
      end if;
      Ptr_Out := (Ptr_Out + 1) mod Size;
      X := File (Ptr_Out);
      Full := False;
    end Pop;

    -- Read without popping
    -- 1 gives the last to be popped (lowest prio)
    procedure Look_Last (X : out Item; No : in No_Range := 1) is
      Loc : Natural range 0 .. Size - 1;
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Prio_Empty;
      end if;
      Loc := (Ptr_In - No + 1) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc

      if      (Ptr_Out <  Ptr_In
                and then (Loc <= Ptr_Out or else  Ptr_In < Loc) )
      or else (Ptr_In  <= Ptr_Out
                and then (Loc <= Ptr_Out and then Ptr_In < Loc) ) then
        raise Prio_Not;
      else
        X := File (Loc);
      end if;
    end Look_Last;

    -- Read without popping
    -- 1 gives the first to be popped (highest prio)
    procedure Look_First (X : out Item; No : in No_Range := 1) is
      Loc : Natural range 0 .. Size - 1;
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Prio_Empty;
      end if;
      Loc := (Ptr_Out + No) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc

      if      (Ptr_Out <  Ptr_In
                and then (Loc <= Ptr_Out or else  Ptr_In < Loc) )
      or else (Ptr_In  <= Ptr_Out
                and then (Loc <= Ptr_Out and then Ptr_In < Loc) ) then
        raise Prio_Not;
      else
        X := File (Loc);
      end if;
    end Look_First;

    -- Remove last item to be popped (lowest prio)
    procedure Discard_Last is
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Prio_Empty;
      end if;
      Ptr_In := (Ptr_In - 1) mod Size;
      Full := False;
    end Discard_Last;

  end Prio;

  package body Circ is
    File : array (0..Size - 1) of Item;
    Ptr_In  : Natural range 0 .. Size - 1 := 0;
    Ptr_Out : Natural range 0 .. Size - 1 := 0;

    Full : Boolean := False;

    -- Ptr_In  points to the last pushed
    -- Ptr_Out points to the first to pop
    --  fifo full  is raised if and only if ptr_in  = ptr_out and full
    --  fifo empty is raised if and only if ptr_in  = ptr_out and not full

    -- Number of Items in Circ
    function Length return Len_Range is
    begin
      if Ptr_In > Ptr_Out then
        return Ptr_In - Ptr_Out;
      elsif Ptr_In < Ptr_Out then
        return Ptr_In + Size - Ptr_Out;
      elsif Full then
        return Size;
      else
        return 0;
      end if;
    end Length;

    -- Push an item
    procedure Push (X : in Item) is
    begin
      if Ptr_In = Ptr_Out and then Full then
        -- Fifo full
        Ptr_Out := (Ptr_Out + 1) mod Size;
      end if;
      Ptr_In := (Ptr_In + 1) mod Size;
      File (Ptr_In) := X;
      Full := (Ptr_In = Ptr_Out);
    end Push;

    -- Pop an item
    procedure Pop (X : out Item) is
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Circ_Empty;
      end if;
      Ptr_Out := (Ptr_Out + 1) mod Size;
      X := File (Ptr_Out);
      Full := False;
    end Pop;

    -- Read without popping
    -- 1 gives the last pushed
    procedure Look_Last (X : out Item; No : in No_Range := 1) is
      Loc : Natural range 0 .. Size - 1;
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Circ_Empty;
      end if;
      Loc := (Ptr_In - No + 1) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc

      if      (Ptr_Out <  Ptr_In
                and then (Loc <= Ptr_Out or else  Ptr_In < Loc) )
      or else (Ptr_In  <= Ptr_Out
                and then (Loc <= Ptr_Out and then Ptr_In < Loc) ) then
        raise Circ_Not;
      else
        X := File (Loc);
      end if;
    end Look_Last;

    -- Read without popping
    -- 1 gives the first to be popped
    procedure Look_First (X : out Item; No : in No_Range := 1) is
      Loc : Natural range 0 .. Size - 1;
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Circ_Empty;
      end if;
      Loc := (Ptr_Out + No) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc

      if      (Ptr_Out <  Ptr_In
                and then (Loc <= Ptr_Out or else  Ptr_In < Loc) )
      or else (Ptr_In  <= Ptr_Out
                and then (Loc <= Ptr_Out and then Ptr_In < Loc) ) then
        raise Circ_Not;
      else
        X := File (Loc);
      end if;
    end Look_First;

    -- Discard last pushed
    procedure Discard_Last is
    begin
      if Ptr_Out = Ptr_In and then not Full then
        raise Circ_Empty;
      end if;
      Ptr_In := (Ptr_In - 1) mod Size;
      Full := False;
    end Discard_Last;

  end Circ;

end Queues;

