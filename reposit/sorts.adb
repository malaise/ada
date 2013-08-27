package body Sorts is

  -- Exchange two objects
  procedure Exchange (X, Y : in out Typ_Object) is
    Auxi : Typ_Object;
  begin
    Auxi := X;
    X := Y;
    Y := Auxi;
  end Exchange;

  -----------------
  -- Bubble sort --
  -----------------
  procedure Bubble_Sort (Slice : in out Typ_Array) is
  begin
    if Slice'Length = 0 then return; end if;
    declare
      First_Index : constant Typ_Index := Slice'First;
      Last_Index  : constant Typ_Index := Slice'Last;
    begin
      for Index in First_Index .. Typ_Index'Pred(Last_Index) loop
        for Bubble in Typ_Index'Succ(Index) .. Last_Index loop
          if Slice(Bubble) < Slice(Index) then
            Exchange (Slice(Index), Slice(Bubble) );
          end if;
        end loop;
      end loop;
    end;
  exception
    when others =>
      raise Sort_Error;
  end Bubble_Sort;

  ---------------------------
  -- Heapsort (tournament) --
  ---------------------------
  procedure Heap_Sort (Slice : in out Typ_Array) is
  begin
    if Slice'Length = 0 then return; end if;

    declare
      First_Index : constant Typ_Index := Slice'First;
      Last_Index  : constant Typ_Index := Slice'Last;
      -- Number of  elements to sort
      Array_Length : constant Integer := Slice'Length;
      -- Heap an its indexes
      subtype Typ_Index_Heap is Integer range 1 .. Array_Length;
      Heap : array (Typ_Index_Heap) of Typ_Object;
      Heap_Index, Son_Index, Father_Index : Typ_Index_Heap;
      First_Leaf, Last_Leaf : Typ_Index_Heap;

    begin
      -- Construction of heap
      Heap_Index := 1;
      Heap(Heap_Index) := Slice(First_Index);

      Build:
      for Array_Index in Typ_Index'Succ(First_Index) .. Last_Index loop
        -- Insert object at the bottom of the heap
        Heap_Index := Heap_Index + 1;
        Heap(Heap_Index) := Slice(Array_Index);

        -- We will insert object at its right place in heap
        Son_Index := Heap_Index;
        Father_Index := Son_Index / 2;

        Sorting:
        loop
          -- The father must be greater than its son
          if not (Heap(Son_Index) < Heap(Father_Index)) then
            Exchange (Heap(Father_Index), Heap(Son_Index));
            -- Exit if al the heap has been seen
            exit Sorting when Father_Index = 1;
            -- New indexes for father and son
            Son_Index := Father_Index;
            Father_Index := Son_Index / 2;
          else
            -- Exit as soon as no exchange
            exit Sorting;
          end if;
        end loop Sorting;
      end loop Build;

      -- Destruction of heap by taking the top at each time
      Heap_Index := Typ_Index_Heap'Last;

      Destruction:
      for Array_Index in reverse
       Typ_Index'Succ(First_Index) .. Last_Index loop
        Slice(Array_Index) := Heap(1);
        -- Put the last heap element at the top
        Heap(1) := Heap(Heap_Index);
        -- Indexes of leaves: (firat_leaf, heap_index-1)
        First_Leaf := ( (Heap_Index - 1) / 2) + 1;
        Last_Leaf := Heap_Index - 1;
        -- Re-sort the top
        Father_Index := 1;

        Resort:
        while Father_Index < First_Leaf loop
          -- Selection of greatest son or son at left (if equal)
          Son_Index := 2 * Father_Index;
          -- If right son exists and if greater
          if Son_Index < Last_Leaf and then
           Heap(Son_Index) < Heap(Son_Index + 1) then
            -- Take right son
            Son_Index := Son_Index + 1;
          end if;
          -- Father must be greater that its gretest son
          if Heap(Father_Index) < Heap(Son_Index) then
            Exchange (Heap(Father_Index), Heap(Son_Index));
            Father_Index := Son_Index;
          else
            -- If no exchange, the heap is OK
            exit Resort;
          end if;
        end loop Resort;

        Heap_Index := Heap_Index - 1;
      end loop Destruction;

      -- Store the top
      Slice(First_Index) := Heap(1);
    exception
      when others =>
        raise Sort_Error;
    end;
  end Heap_Sort;

  ---------------
  -- Quicksort --
  ---------------
  procedure Quick_Sort (Slice : in out Typ_Array) is
  begin
    if Slice'Length = 0 then return; end if;

    declare

      -- Recursive procedure which sorts a slice of the array
      procedure Quick (Left, Right : in Typ_Index) is
        -- Middle of the slice
        I_Frontier : constant Typ_Index :=
         Typ_Index'Val( (Typ_Index'Pos(Left)+Typ_Index'Pos(Right)) /2);
        Frontier : constant Typ_Object := Slice(I_Frontier);
        -- Indexes in both halfs of the slice
        I_Left, I_Right : Typ_Index;
      begin
        I_Left := Left;
        I_Right := Right;
        loop

          -- First element at left of slice and not positioned ok
          --  regarding the frontier
          while Slice(I_Left) < Frontier loop
            I_Left := Typ_Index'Succ(I_Left);
          end loop;
          -- Last  element a right of slice and not positioned ok
          --  regarding the frontier
          while Frontier < Slice(I_Right) loop
            I_Right := Typ_Index'Pred(I_Right);
          end loop;

          -- Exchange and go to next elements if not both in frontier
          if I_Left < I_Right then
            Exchange (Slice(I_Left), Slice(I_Right));
            I_Left := Typ_Index'Succ (I_Left);
            I_Right := Typ_Index'Pred (I_Right);
          elsif I_Left = I_Right then
            -- Go to next elements if not crossed and not to end
            if I_Left /= Right then
              I_Left := Typ_Index'Succ (I_Left);
            end if;
            if I_Right /= Left then
              I_Right := Typ_Index'Pred (I_Right);
            end if;
          end if;

          -- Leave if crossed now
          exit when I_Left > I_Right or else
                   (I_Left = Right and then I_Right = Left);
        end loop;

        -- Sort both new slices recursively
        if Left   < I_Right then Quick(Left,   I_Right); end if;
        if I_Left < Right   then Quick(I_Left, Right);   end if;
      end Quick;

    begin
      Quick (Slice'First, Slice'Last);
    end;
  exception
    when others =>
      raise Sort_Error;
  end Quick_Sort;

end Sorts;

