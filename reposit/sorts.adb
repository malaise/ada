package body SORTS is

  -- Exchange two objects
  procedure EXCHANGE (X, Y : in out TYP_OBJECT) is
    AUXI : TYP_OBJECT;
  begin
    AUXI := X;
    X := Y;
    Y := AUXI;
  end EXCHANGE;

  -----------------
  -- Bubble sort --
  -----------------
  procedure BUBBLE_SORT (SLICE : in out TYP_ARRAY) is
  begin
    if SLICE'LENGTH = 0 then return; end if;
    declare
      FIRST_INDEX : constant TYP_INDEX := SLICE'FIRST;
      LAST_INDEX  : constant TYP_INDEX := SLICE'LAST;
    begin
      for INDEX in FIRST_INDEX .. TYP_INDEX'PRED(LAST_INDEX) loop
        for BUBBLE in TYP_INDEX'SUCC(INDEX) .. LAST_INDEX loop
          if SLICE(BUBBLE) < SLICE(INDEX) then
            EXCHANGE (SLICE(INDEX), SLICE(BUBBLE) );
          end if;
        end loop;
      end loop;
    end;
  exception
    when others =>
      raise SORT_ERROR;
  end BUBBLE_SORT;

  ---------------------------
  -- Heapsort (tournament) --
  ---------------------------
  procedure HEAP_SORT (SLICE : in out TYP_ARRAY) is
  begin
    if SLICE'LENGTH = 0 then return; end if;

    declare
      FIRST_INDEX : constant TYP_INDEX := SLICE'FIRST;
      LAST_INDEX  : constant TYP_INDEX := SLICE'LAST;
      -- number of  elements to sort
      ARRAY_LENGTH : constant INTEGER := SLICE'LENGTH;
      -- heap an its indexes
      subtype TYP_INDEX_HEAP is INTEGER range 1 .. ARRAY_LENGTH;
      HEAP : array (TYP_INDEX_HEAP) of TYP_OBJECT;
      HEAP_INDEX, SON_INDEX, FATHER_INDEX : TYP_INDEX_HEAP;
      FIRST_LEAF, LAST_LEAF : TYP_INDEX_HEAP;

    begin
      -- Construction of heap
      HEAP_INDEX := 1;
      HEAP(HEAP_INDEX) := SLICE(FIRST_INDEX);

      BUILD:
      for ARRAY_INDEX in TYP_INDEX'SUCC(FIRST_INDEX) .. LAST_INDEX loop
        -- insert object at the bottom of the heap
        HEAP_INDEX := HEAP_INDEX + 1;
        HEAP(HEAP_INDEX) := SLICE(ARRAY_INDEX);

        -- we will insert object at its right place in heap
        SON_INDEX := HEAP_INDEX;
        FATHER_INDEX := SON_INDEX / 2;

        SORTING:
        loop
          -- the father must be greater than its son
          if not (HEAP(SON_INDEX) < HEAP(FATHER_INDEX)) then
            EXCHANGE (HEAP(FATHER_INDEX), HEAP(SON_INDEX));
            -- exit if al the heap has been seen
            exit SORTING when FATHER_INDEX = 1;
            -- new indexes for father and son
            SON_INDEX := FATHER_INDEX;
            FATHER_INDEX := SON_INDEX / 2;
          else
            -- exit as soon as no exchange
            exit SORTING;
          end if;
        end loop SORTING;
      end loop BUILD;

      -- destruction of heap by taking the top at each time
      HEAP_INDEX := TYP_INDEX_HEAP'LAST;

      DESTRUCTION:
      for ARRAY_INDEX in reverse
       TYP_INDEX'SUCC(FIRST_INDEX) .. LAST_INDEX loop
        SLICE(ARRAY_INDEX) := HEAP(1);
        -- Put the last heap element at the top
        HEAP(1) := HEAP(HEAP_INDEX);
        -- indexes of leaves: (firat_leaf, heap_index-1)
        FIRST_LEAF := ( (HEAP_INDEX - 1) / 2) + 1;
        LAST_LEAF := HEAP_INDEX - 1;
        -- re-sort the top
        FATHER_INDEX := 1;

        RESORT:
        while FATHER_INDEX < FIRST_LEAF loop
          -- selection of greatest son or son at left (if equal)
          SON_INDEX := 2 * FATHER_INDEX;
          -- if right son exists and if greater
          if SON_INDEX < LAST_LEAF and then
           HEAP(SON_INDEX) < HEAP(SON_INDEX + 1) then
            -- take right son
            SON_INDEX := SON_INDEX + 1;
          end if;
          -- father must be greater that its gretest son
          if HEAP(FATHER_INDEX) < HEAP(SON_INDEX) then
            EXCHANGE (HEAP(FATHER_INDEX), HEAP(SON_INDEX));
            FATHER_INDEX := SON_INDEX;
          else
            -- if no exchange, the heap is OK
            exit RESORT;
          end if;
        end loop RESORT;

        HEAP_INDEX := HEAP_INDEX - 1;
      end loop DESTRUCTION;

      -- Store the top 
      SLICE(FIRST_INDEX) := HEAP(1);
    exception
      when others =>
        raise SORT_ERROR;
    end;
  end HEAP_SORT;

  ---------------
  -- quicksort --
  ---------------
  procedure QUICK_SORT (SLICE : in out TYP_ARRAY) is
  begin
    if SLICE'LENGTH = 0 then return; end if;

    declare

      -- recursive procedure which sorts a slice of the array
      procedure QUICK (LEFT, RIGHT : in TYP_INDEX) is
        -- middle of the slice
        I_FRONTIER : constant TYP_INDEX :=
         TYP_INDEX'VAL( (TYP_INDEX'POS(LEFT)+TYP_INDEX'POS(RIGHT)) /2);
        FRONTIER : constant TYP_OBJECT := SLICE(I_FRONTIER);
        -- indexes in both halfs of the slice
        I_LEFT, I_RIGHT : TYP_INDEX;
      begin
        I_LEFT := LEFT;
        I_RIGHT := RIGHT;
        loop

          -- first element at left of slice and not positioned ok
          --  regarding the frontier
          while SLICE(I_LEFT) < FRONTIER loop
            I_LEFT := TYP_INDEX'SUCC(I_LEFT);
          end loop;
          -- last  element a right of slice and not positioned ok
          --  regarding the frontier
          while FRONTIER < SLICE(I_RIGHT) loop
            I_RIGHT := TYP_INDEX'PRED(I_RIGHT);
          end loop;

          -- exchange and go to next elements if not both in frontier
          if I_LEFT < I_RIGHT then
            EXCHANGE (SLICE(I_LEFT), SLICE(I_RIGHT));
            I_LEFT := TYP_INDEX'SUCC (I_LEFT);
            I_RIGHT := TYP_INDEX'PRED (I_RIGHT);
          elsif I_LEFT = I_RIGHT then
            -- go to next elements if not crossed and not to end
            if I_LEFT /= RIGHT then
              I_LEFT := TYP_INDEX'SUCC (I_LEFT);
            end if;
            if I_RIGHT /= LEFT then
              I_RIGHT := TYP_INDEX'PRED (I_RIGHT);
            end if;
          end if;

          -- leave if crossed now
          exit when I_LEFT > I_RIGHT or else
                   (I_LEFT = RIGHT and then I_RIGHT = LEFT);
        end loop;

        -- sort both new slices
        if LEFT   < I_RIGHT then QUICK(LEFT,   I_RIGHT); end if;
        if I_LEFT < RIGHT   then QUICK(I_LEFT, RIGHT);   end if;
      end QUICK;

    begin
      QUICK (SLICE'FIRST, SLICE'LAST);
    end;
  exception
    when others =>
      raise SORT_ERROR;
  end QUICK_SORT;

end SORTS;
