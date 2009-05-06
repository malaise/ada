package body Queues is

  package body Lifo is

    -- Number of Items in Lifo
    function Length (Queue : Lifo_Type) return Len_Range is
    begin
      return Queue.Ptr;
    end Length;

    -- Push an item
    procedure Push (Queue : in out Lifo_Type; X : in Item) is
    begin
      if Queue.Ptr = Size then
        raise Lifo_Full;
      end if;
      Queue.Ptr := Queue.Ptr + 1;
      Queue.Pile(Queue.Ptr) := X;
    end Push;

    -- Pop an item
    procedure Pop (Queue : in out Lifo_Type; X : out Item) is
    begin
      if Queue.Ptr = 0  then
        raise Lifo_Empty;
      end if;
      X := Queue.Pile(Queue.Ptr);
      Queue.Ptr := Queue.Ptr - 1;
    end Pop;

    -- Read without popping
    -- 1 gives the first to be popped (ptr)
    procedure Look_First (Queue : in out Lifo_Type;
                          X : out Item; No : in No_Range := 1) is
    begin
      if Queue.Ptr = 0 then
        raise Lifo_Empty;
      end if;
      if No > Queue.Ptr then
        raise Lifo_Not;
      else
        X := Queue.Pile(Queue.Ptr - No + 1);
      end if;
    end Look_First;

    -- Read without popping
    -- 1 gives the last to be popped
    procedure Look_Last (Queue : in out Lifo_Type;
                         X : out Item; No : in No_Range := 1) is
    begin
      if Queue.Ptr = 0 then
        raise Lifo_Empty;
      end if;
      if No > Queue.Ptr then
        raise Lifo_Not;
      else
        X := Queue.Pile(No);
      end if;
    end Look_Last;

    -- Remove first item and shift
    procedure Discard_Last (Queue : in out Lifo_Type) is
    begin
      if Queue.Ptr = 0 then
        raise Lifo_Empty;
      end if;
      for I in 1 .. Queue.Ptr - 1 loop
        Queue.Pile(I) := Queue.Pile (I+1);
      end loop;
      Queue.Ptr := Queue.Ptr - 1;
    end Discard_Last;

    -- Clear
    procedure Clear (Queue : in out Lifo_Type) is
    begin
      Queue.Ptr := 0;
    end Clear;

  end Lifo;

  package body Fifo is

    -- Number of Items in Fifo
    function Length (Queue : Fifo_Type) return Len_Range is
    begin
      if Queue.Ptr_In > Queue.Ptr_Out then
        return Queue.Ptr_In - Queue.Ptr_Out;
      elsif Queue.Ptr_In < Queue.Ptr_Out then
        return Queue.Ptr_In + Size - Queue.Ptr_Out;
      elsif Queue.Full then
        return Size;
      else
        return 0;
      end if;
    end Length;

    -- Push an item
    procedure Push (Queue : in out Fifo_Type; X : in Item) is
    begin
      if Queue.Ptr_In = Queue.Ptr_Out and then Queue.Full then
        raise Fifo_Full;
      end if;
      Queue.Ptr_In := (Queue.Ptr_In + 1) mod Size;
      Queue.File(Queue.Ptr_In) := X;
      Queue.Full := (Queue.Ptr_In = Queue.Ptr_Out);
    end Push;

    -- Pop an item
    procedure Pop (Queue : in out Fifo_Type; X : out Item) is
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Fifo_Empty;
      end if;
      Queue.Ptr_Out := (Queue.Ptr_Out + 1) mod Size;
      X := Queue.File(Queue.Ptr_Out);
      Queue.Full := False;
    end Pop;

    -- Read without popping
    -- 1 gives the last pushed
    procedure Look_Last (Queue : in out Fifo_Type;
                         X : out Item; No : in No_Range := 1) is
      Loc : Ptr_Range;
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Fifo_Empty;
      end if;
      Loc := (Queue.Ptr_In - No + 1) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc
      if      (Queue.Ptr_Out <  Queue.Ptr_In
                and then (Loc <= Queue.Ptr_Out or else  Queue.Ptr_In < Loc) )
      or else (Queue.Ptr_In  <= Queue.Ptr_Out
                and then (Loc <= Queue.Ptr_Out and then Queue.Ptr_In < Loc) )
      then
        raise Fifo_Not;
      else
        X := Queue.File(Loc);
      end if;
    end Look_Last;

    -- Read without popping
    -- 1 gives the first to be popped
    procedure Look_First (Queue : in out Fifo_Type;
                          X : out Item; No : in No_Range := 1) is
      Loc : Ptr_Range;
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Fifo_Empty;
      end if;
      Loc := (Queue.Ptr_Out + No) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc
      if      (Queue.Ptr_Out <  Queue.Ptr_In
                and then (Loc <= Queue.Ptr_Out or else  Queue.Ptr_In < Loc) )
      or else (Queue.Ptr_In  <= Queue.Ptr_Out
                and then (Loc <= Queue.Ptr_Out and then Queue.Ptr_In < Loc) )
      then
        raise Fifo_Not;
      else
        X := Queue.File(Loc);
      end if;
    end Look_First;

    -- Discard last pushed
    procedure Discard_Last (Queue : in out Fifo_Type) is
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Fifo_Empty;
      end if;
      Queue.Ptr_In := (Queue.Ptr_In - 1) mod Size;
      Queue.Full := False;
    end Discard_Last;

    -- Clear
    procedure Clear (Queue : in out Fifo_Type) is
    begin
      Queue.Ptr_Out := Queue.Ptr_In;
      Queue.Full := False;
    end Clear;

  end Fifo;

  package body Prio is

    -- Number of Items in Prio
    function Length (Queue : Prio_Type) return Len_Range is
    begin
      if Queue.Ptr_In > Queue.Ptr_Out then
        return Queue.Ptr_In - Queue.Ptr_Out;
      elsif Queue.Ptr_In < Queue.Ptr_Out then
        return Queue.Ptr_In + Size - Queue.Ptr_Out;
      elsif Queue.Full then
        return Size;
      else
        return 0;
      end if;
    end Length;

    -- Push an item
    procedure Push (Queue : in out Prio_Type;
                    X : in Item; P : in Priority := Priority'Last) is
      I, J : Typ_Ptr;
    begin
      if Queue.Ptr_In = Queue.Ptr_Out and then Queue.Full then
        -- Fifo full
        raise Prio_Full;
      elsif Queue.Ptr_In = Queue.Ptr_Out then
        -- Fifo empty, store X at its place
        Queue.Ptr_In := (Queue.Ptr_In + 1) mod Size;
        Queue.File(Queue.Ptr_In) := X;
        Queue.File_Prio(Queue.Ptr_In) := P;
      else
        -- Create empty slot
        Queue.Ptr_In := (Queue.Ptr_In + 1) mod Size;
        -- For each slot
        I := Queue.Ptr_In;
        loop
          J := (I - 1) mod Size;
          -- Compare prios and test limits
          exit when (J = Queue.Ptr_Out) or else (Queue.File_Prio(J) >= P);
          -- Shift
          Queue.File(I) := Queue.File(J);
          Queue.File_Prio(I) := Queue.File_Prio(J);
          I := J;
        end loop;
        -- Store X
        Queue.File(I) := X;
        Queue.File_Prio(I) := P;
      end if;
      Queue.Full := (Queue.Ptr_In = Queue.Ptr_Out);
    end Push;

    -- Pop item with highest priority
    procedure Pop (Queue : in out Prio_Type; X : out Item) is
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Prio_Empty;
      end if;
      Queue.Ptr_Out := (Queue.Ptr_Out + 1) mod Size;
      X := Queue.File(Queue.Ptr_Out);
      Queue.Full := False;
    end Pop;

    -- Read without popping
    -- 1 gives the last to be popped (lowest prio)
    procedure Look_Last (Queue : in out Prio_Type;
                         X : out Item; No : in No_Range := 1) is
      Loc : Typ_Ptr;
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Prio_Empty;
      end if;
      Loc := (Queue.Ptr_In - No + 1) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc
      if      (Queue.Ptr_Out <  Queue.Ptr_In
                and then (Loc <= Queue.Ptr_Out or else  Queue.Ptr_In < Loc) )
      or else (Queue.Ptr_In  <= Queue.Ptr_Out
                and then (Loc <= Queue.Ptr_Out and then Queue.Ptr_In < Loc) )
      then
        raise Prio_Not;
      else
        X := Queue.File(Loc);
      end if;
    end Look_Last;

    -- Read without popping
    -- 1 gives the first to be popped (highest prio)
    procedure Look_First (Queue : in out Prio_Type;
                          X : out Item; No : in No_Range := 1) is
      Loc : Typ_Ptr;
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Prio_Empty;
      end if;
      Loc := (Queue.Ptr_Out + No) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc
      if      (Queue.Ptr_Out <  Queue.Ptr_In
                and then (Loc <= Queue.Ptr_Out or else  Queue.Ptr_In < Loc) )
      or else (Queue.Ptr_In  <= Queue.Ptr_Out
                and then (Loc <= Queue.Ptr_Out and then Queue.Ptr_In < Loc) )
      then
        raise Prio_Not;
      else
        X := Queue.File(Loc);
      end if;
    end Look_First;

    -- Remove last item to be popped (lowest prio)
    procedure Discard_Last (Queue : in out Prio_Type) is
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Prio_Empty;
      end if;
      Queue.Ptr_In := (Queue.Ptr_In - 1) mod Size;
      Queue.Full := False;
    end Discard_Last;

    -- Clear
    procedure Clear (Queue : in out Prio_Type) is
    begin
      Queue.Ptr_Out := Queue.Ptr_In;
      Queue.Full := False;
    end Clear;

  end Prio;

  package body Circ is

    -- Number of Items in Circ
    function Length (Queue : Circ_Type) return Len_Range is
    begin
      if Queue.Ptr_In > Queue.Ptr_Out then
        return Queue.Ptr_In - Queue.Ptr_Out;
      elsif Queue.Ptr_In < Queue.Ptr_Out then
        return Queue.Ptr_In + Size - Queue.Ptr_Out;
      elsif Queue.Full then
        return Size;
      else
        return 0;
      end if;
    end Length;

    -- Push an item
    procedure Push (Queue : in out Circ_Type; X : in Item) is
    begin
      if Queue.Ptr_In = Queue.Ptr_Out and then Queue.Full then
        -- Fifo full
        Queue.Ptr_Out := (Queue.Ptr_Out + 1) mod Size;
      end if;
      Queue.Ptr_In := (Queue.Ptr_In + 1) mod Size;
      Queue.File(Queue.Ptr_In) := X;
      Queue.Full := (Queue.Ptr_In = Queue.Ptr_Out);
    end Push;

    -- Pop an item
    procedure Pop (Queue : in out Circ_Type; X : out Item) is
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Circ_Empty;
      end if;
      Queue.Ptr_Out := (Queue.Ptr_Out + 1) mod Size;
      X := Queue.File(Queue.Ptr_Out);
      Queue.Full := False;
    end Pop;

    -- Read without popping
    -- 1 gives the last pushed
    procedure Look_Last (Queue : in out Circ_Type;
                         X : out Item; No : in No_Range := 1) is
      Loc : Typ_Ptr;
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Circ_Empty;
      end if;
      Loc := (Queue.Ptr_In - No + 1) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc
      if      (Queue.Ptr_Out <  Queue.Ptr_In
                and then (Loc <= Queue.Ptr_Out or else  Queue.Ptr_In < Loc) )
      or else (Queue.Ptr_In  <= Queue.Ptr_Out
                and then (Loc <= Queue.Ptr_Out and then Queue.Ptr_In < Loc) )
      then
        raise Circ_Not;
      else
        X := Queue.File(Loc);
      end if;
    end Look_Last;

    -- Read without popping
    -- 1 gives the first to be popped
    procedure Look_First (Queue : in out Circ_Type;
                          X : out Item; No : in No_Range := 1) is
      Loc : Typ_Ptr;
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Circ_Empty;
      end if;
      Loc := (Queue.Ptr_Out + No) mod Size;

      -- Good if Ptr_Out < Loc <= Ptr_In
      -- or      Loc <= Ptr_In <= Ptr_Out
      -- or             Ptr_In <= Ptr_Out < Loc
      if      (Queue.Ptr_Out <  Queue.Ptr_In
                and then (Loc <= Queue.Ptr_Out or else  Queue.Ptr_In < Loc) )
      or else (Queue.Ptr_In  <= Queue.Ptr_Out
                and then (Loc <= Queue.Ptr_Out and then Queue.Ptr_In < Loc) )
      then
        raise Circ_Not;
      else
        X := Queue.File(Loc);
      end if;
    end Look_First;

    -- Discard last pushed
    procedure Discard_Last (Queue : in out Circ_Type) is
    begin
      if Queue.Ptr_Out = Queue.Ptr_In and then not Queue.Full then
        raise Circ_Empty;
      end if;
      Queue.Ptr_In := (Queue.Ptr_In - 1) mod Size;
      Queue.Full := False;
    end Discard_Last;

    -- Clear
    procedure Clear (Queue : in out Circ_Type) is
    begin
      Queue.Ptr_Out := Queue.Ptr_In;
      Queue.Full := False;
    end Clear;
  end Circ;

end Queues;

