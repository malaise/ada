package body QUEUES is

  package body LIFO is
    PILE : array (1 .. SIZE) of ITEM;
    PTR : NATURAL range 0 .. SIZE := 0;

    -- PTR is the last pushed except if stack is empty 
    --  the it is 0.

    -- push an item
    procedure PUSH (X : in ITEM) is
    begin
      if PTR = SIZE then
        raise LIFO_FULL;
      end if;
      PTR := PTR + 1;
      PILE (PTR) := X;
    end PUSH;

    -- pop an item
    procedure POP (X : out ITEM) is
    begin
      if PTR = 0  then
        raise LIFO_EMPTY;
      end if;
      X := PILE (PTR);
      PTR := PTR - 1;
    end POP;

    -- read without popping
    -- 1 gives the first to be popped (ptr)
    procedure LOOK_FIRST (X : out ITEM; NO : in NO_RANGE := 1) is
    begin
      if PTR = 0 then
        raise LIFO_EMPTY;
      end if;
      if NO > PTR then
        raise LIFO_NOT;
      else
        X := PILE (PTR - NO + 1);
      end if;
    end LOOK_FIRST;

    -- read without popping
    -- 1 gives the last to be popped 
    procedure LOOK_LAST (X : out ITEM; NO : in NO_RANGE := 1) is
    begin
      if PTR = 0 then
        raise LIFO_EMPTY;
      end if;
      if NO > PTR then
        raise LIFO_NOT;
      else
        X := PILE (NO);
      end if;
    end LOOK_LAST;

    -- remove first item and shift
    procedure DISCARD_LAST is
    begin
      if PTR = 0 then
        raise LIFO_EMPTY;
      end if;
      for I in 1 .. PTR-1 loop
        PILE(I) := PILE (I+1);
      end loop;
      PTR := PTR - 1;
    end DISCARD_LAST;

  end LIFO;

  package body FIFO is
    FILE : array (0..SIZE - 1) of ITEM;
    PTR_IN  : NATURAL range 0 .. SIZE - 1 := 0;
    PTR_OUT : NATURAL range 0 .. SIZE - 1 := 0;

    FULL : BOOLEAN := FALSE;

    -- PTR_IN  points to the last pushed
    -- PTR_OUT points to the first to pop
    --  fifo full  is raised if and only if ptr_in  = ptr_out and full
    --  fifo empty is raised if and only if ptr_in  = ptr_out and not full

    -- push an item
    procedure PUSH (X : in ITEM) is
    begin
      if PTR_IN = PTR_OUT and then FULL then
        raise FIFO_FULL;
      end if;
      PTR_IN := (PTR_IN + 1) mod SIZE;
      FILE (PTR_IN) := X;
      FULL := (PTR_IN = PTR_OUT);
    end PUSH;

    -- pop an item
    procedure POP (X : out ITEM) is
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise FIFO_EMPTY;
      end if;
      PTR_OUT := (PTR_OUT + 1) mod SIZE;
      X := FILE (PTR_OUT);
      FULL := FALSE;
    end POP;

    -- read without popping
    -- 1 gives the last pushed
    procedure LOOK_LAST (X : out ITEM; NO : in NO_RANGE := 1) is
      LOC : NATURAL range 0 .. SIZE - 1;
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise FIFO_EMPTY;
      end if;
      LOC := (PTR_IN - NO + 1) mod SIZE;

      -- Good if PTR_OUT < LOC <= PTR_IN
      -- or      LOC <= PTR_IN <= PTR_OUT
      -- or             PTR_IN <= PTR_OUT < LOC

      if      (PTR_OUT <  PTR_IN
                and then (LOC <= PTR_OUT or else  PTR_IN < LOC) )
      or else (PTR_IN  <= PTR_OUT
                and then (LOC <= PTR_OUT and then PTR_IN < LOC) ) then
        raise FIFO_NOT;
      else
        X := FILE (LOC);
      end if;
    end LOOK_LAST;

    -- read without popping
    -- 1 gives the first to be popped
    procedure LOOK_FIRST (X : out ITEM; NO : in NO_RANGE := 1) is
      LOC : NATURAL range 0 .. SIZE - 1;
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise FIFO_EMPTY;
      end if;
      LOC := (PTR_OUT + NO) mod SIZE;

      -- Good if PTR_OUT < LOC <= PTR_IN
      -- or      LOC <= PTR_IN <= PTR_OUT
      -- or             PTR_IN <= PTR_OUT < LOC

      if      (PTR_OUT <  PTR_IN
                and then (LOC <= PTR_OUT or else  PTR_IN < LOC) )
      or else (PTR_IN  <= PTR_OUT
                and then (LOC <= PTR_OUT and then PTR_IN < LOC) ) then
        raise FIFO_NOT;
      else
        X := FILE (LOC);
      end if;
    end LOOK_FIRST;

    -- discard last pushed 
    procedure DISCARD_LAST is
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise FIFO_EMPTY;
      end if;
      PTR_IN := (PTR_IN - 1) mod SIZE;
      FULL := FALSE;
    end DISCARD_LAST;

  end FIFO;

  package body PRIO is
    FILE : array (0 .. SIZE - 1) of ITEM;
    FILE_PRIO : array (0 .. SIZE - 1) of PRIORITY;
    subtype TYP_PTR is NATURAL range 0 .. SIZE - 1;
    PTR_IN  : TYP_PTR := 0;
    PTR_OUT : TYP_PTR := 0;

    FULL : BOOLEAN := FALSE;

    -- PTR_IN  points to the last pushed
    -- PTR_OUT points to the first to pop
    --  fifo full  is raised if and only if ptr_in  = ptr_out and full
    --  fifo empty is raised if and only if ptr_in  = ptr_out and not full

    -- push an item
    procedure PUSH (X : in ITEM; P : in PRIORITY := PRIORITY'LAST) is
      I, J : TYP_PTR;
    begin
      if PTR_IN = PTR_OUT and then FULL then
        -- file pleine
        raise PRIO_FULL;
      elsif PTR_IN = PTR_OUT then
        -- file vide, ranger x en place
        PTR_IN := (PTR_IN + 1) mod SIZE;
        FILE (PTR_IN) := X;
        FILE_PRIO (PTR_IN) := P;
      else
        -- creer une place vide
        PTR_IN := (PTR_IN + 1) mod SIZE;
        -- decrire toutes les places
        I := PTR_IN;
        loop
          J := (I-1) mod SIZE;
          -- comparer les prio et test de borne
          exit when (J = PTR_OUT) or else (FILE_PRIO (J) >= P);
          -- decalage
          FILE (I) := FILE (J);
          FILE_PRIO (I) := FILE_PRIO (J);
          I := J;
        end loop;
        -- rangement de x
        FILE (I) := X;
        FILE_PRIO (I) := P;
      end if;
      FULL := (PTR_IN = PTR_OUT);
    end PUSH;

    -- pop item with highest priority 
    procedure POP (X : out ITEM) is
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise PRIO_EMPTY;
      end if;
      PTR_OUT := (PTR_OUT + 1) mod SIZE;
      X := FILE (PTR_OUT);
      FULL := FALSE;
    end POP;

    -- read without popping
    -- 1 gives the last to be popped (lowest prio)
    procedure LOOK_LAST (X : out ITEM; NO : in NO_RANGE := 1) is
      LOC : NATURAL range 0 .. SIZE - 1;
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise PRIO_EMPTY;
      end if;
      LOC := (PTR_IN - NO + 1) mod SIZE;

      -- Good if PTR_OUT < LOC <= PTR_IN
      -- or      LOC <= PTR_IN <= PTR_OUT
      -- or             PTR_IN <= PTR_OUT < LOC

      if      (PTR_OUT <  PTR_IN
                and then (LOC <= PTR_OUT or else  PTR_IN < LOC) )
      or else (PTR_IN  <= PTR_OUT
                and then (LOC <= PTR_OUT and then PTR_IN < LOC) ) then
        raise PRIO_NOT;
      else
        X := FILE (LOC);
      end if;
    end LOOK_LAST;

    -- read without popping
    -- 1 gives the first to be popped (highest prio)
    procedure LOOK_FIRST (X : out ITEM; NO : in NO_RANGE := 1) is
      LOC : NATURAL range 0 .. SIZE - 1;
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise PRIO_EMPTY;
      end if;
      LOC := (PTR_OUT + NO) mod SIZE;

      -- Good if PTR_OUT < LOC <= PTR_IN
      -- or      LOC <= PTR_IN <= PTR_OUT
      -- or             PTR_IN <= PTR_OUT < LOC

      if      (PTR_OUT <  PTR_IN
                and then (LOC <= PTR_OUT or else  PTR_IN < LOC) )
      or else (PTR_IN  <= PTR_OUT
                and then (LOC <= PTR_OUT and then PTR_IN < LOC) ) then
        raise PRIO_NOT;
      else
        X := FILE (LOC);
      end if;
    end LOOK_FIRST;

    -- remove last item to be popped (lowest prio)
    procedure DISCARD_LAST is
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise PRIO_EMPTY;
      end if;
      PTR_IN := (PTR_IN - 1) mod SIZE;
      FULL := FALSE;
    end DISCARD_LAST;

  end PRIO;

  package body CIRC is
    FILE : array (0..SIZE - 1) of ITEM;
    PTR_IN  : NATURAL range 0 .. SIZE - 1 := 0;
    PTR_OUT : NATURAL range 0 .. SIZE - 1 := 0;

    FULL : BOOLEAN := FALSE;

    -- PTR_IN  points to the last pushed
    -- PTR_OUT points to the first to pop
    --  fifo full  is raised if and only if ptr_in  = ptr_out and full
    --  fifo empty is raised if and only if ptr_in  = ptr_out and not full

    -- push an item
    procedure PUSH (X : in ITEM) is
    begin
      if PTR_IN = PTR_OUT and then FULL then
        -- Fifo full
        PTR_OUT := (PTR_OUT + 1) mod SIZE;
      end if;
      PTR_IN := (PTR_IN + 1) mod SIZE;
      FILE (PTR_IN) := X;
      FULL := (PTR_IN = PTR_OUT);
    end PUSH;

    -- pop an item
    procedure POP (X : out ITEM) is
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise CIRC_EMPTY;
      end if;
      PTR_OUT := (PTR_OUT + 1) mod SIZE;
      X := FILE (PTR_OUT);
      FULL := FALSE;
    end POP;

    -- read without popping
    -- 1 gives the last pushed
    procedure LOOK_LAST (X : out ITEM; NO : in NO_RANGE := 1) is
      LOC : NATURAL range 0 .. SIZE - 1;
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise CIRC_EMPTY;
      end if;
      LOC := (PTR_IN - NO + 1) mod SIZE;

      -- Good if PTR_OUT < LOC <= PTR_IN
      -- or      LOC <= PTR_IN <= PTR_OUT
      -- or             PTR_IN <= PTR_OUT < LOC

      if      (PTR_OUT <  PTR_IN
                and then (LOC <= PTR_OUT or else  PTR_IN < LOC) )
      or else (PTR_IN  <= PTR_OUT
                and then (LOC <= PTR_OUT and then PTR_IN < LOC) ) then
        raise CIRC_NOT;
      else
        X := FILE (LOC);
      end if;
    end LOOK_LAST;

    -- read without popping
    -- 1 gives the first to be popped
    procedure LOOK_FIRST (X : out ITEM; NO : in NO_RANGE := 1) is
      LOC : NATURAL range 0 .. SIZE - 1;
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise CIRC_EMPTY;
      end if;
      LOC := (PTR_OUT + NO) mod SIZE;

      -- Good if PTR_OUT < LOC <= PTR_IN
      -- or      LOC <= PTR_IN <= PTR_OUT
      -- or             PTR_IN <= PTR_OUT < LOC

      if      (PTR_OUT <  PTR_IN
                and then (LOC <= PTR_OUT or else  PTR_IN < LOC) )
      or else (PTR_IN  <= PTR_OUT
                and then (LOC <= PTR_OUT and then PTR_IN < LOC) ) then
        raise CIRC_NOT;
      else
        X := FILE (LOC);
      end if;
    end LOOK_FIRST;

    -- discard last pushed 
    procedure DISCARD_LAST is
    begin
      if PTR_OUT = PTR_IN and then not FULL then
        raise CIRC_EMPTY;
      end if;
      PTR_IN := (PTR_IN - 1) mod SIZE;
      FULL := FALSE;
    end DISCARD_LAST;

  end CIRC;

end QUEUES;

